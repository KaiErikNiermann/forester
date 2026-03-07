(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

open Rust_parser_types

type process_capture = {
  stdout : string;
  stderr : string;
  status : Unix.process_status;
}

type process_run_error =
  | Spawn_error of (Unix.error * string * string)
  | Timed_out of float

type stream_capture = { fd : Unix.file_descr; buffer : Buffer.t }

let format_unix_error (error, fn, arg) =
  let arg_suffix =
    if String.trim arg = "" then "" else Printf.sprintf " (%s)" arg
  in
  Printf.sprintf "%s%s: %s" fn arg_suffix (Unix.error_message error)

let nonempty_output text =
  let trimmed = String.trim text in
  if trimmed = "" then None else Some trimmed

let format_process_output ~stdout ~stderr =
  let sections =
    [ ("stderr", stderr); ("stdout", stdout) ]
    |> List.filter_map (fun (label, content) ->
        Option.map (fun text -> (label, text)) (nonempty_output content))
  in
  match sections with
  | [] -> None
  | [ (label, text) ] -> Some (Printf.sprintf "%s:\n%s" label text)
  | _ ->
      Some
        (sections
        |> List.map (fun (label, text) -> Printf.sprintf "%s:\n%s" label text)
        |> String.concat "\n\n")

let rec waitpid_with_timeout pid deadline =
  let remaining = deadline -. Unix.gettimeofday () in
  if remaining <= 0. then Error (Timed_out 0.)
  else
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        Unix.sleepf (min 0.05 remaining);
        waitpid_with_timeout pid deadline
    | _, status -> Ok status

let read_streams_with_timeout ~stdout_ic ~stderr_ic ~timeout_seconds =
  let stdout = Buffer.create 1024 in
  let stderr = Buffer.create 1024 in
  let active_streams =
    ref
      [
        { fd = Unix.descr_of_in_channel stdout_ic; buffer = stdout };
        { fd = Unix.descr_of_in_channel stderr_ic; buffer = stderr };
      ]
  in
  let scratch = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. timeout_seconds in
  let rec loop () =
    match !active_streams with
    | [] -> Ok (Buffer.contents stdout, Buffer.contents stderr, deadline)
    | streams ->
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0. then Error (Timed_out timeout_seconds)
        else
          let ready, _, _ =
            Unix.select
              (List.map (fun stream -> stream.fd) streams)
              [] [] remaining
          in
          if ready = [] then Error (Timed_out timeout_seconds)
          else (
            active_streams :=
              List.filter_map
                (fun stream ->
                  if List.exists (( = ) stream.fd) ready then (
                    match
                      Unix.read stream.fd scratch 0 (Bytes.length scratch)
                    with
                    | 0 -> None
                    | read_count ->
                        Buffer.add_subbytes stream.buffer scratch 0 read_count;
                        Some stream)
                  else Some stream)
                streams;
            loop ())
  in
  loop ()

let run_process_capture ~program ~args ~timeout_seconds =
  try
    let stdout_ic, stdin_oc, stderr_ic =
      Unix.open_process_args_full program args (Unix.environment ())
    in
    let pid = Unix.process_full_pid (stdout_ic, stdin_oc, stderr_ic) in
    Fun.protect ~finally:(fun () ->
        close_in_noerr stdout_ic;
        close_in_noerr stderr_ic;
        close_out_noerr stdin_oc)
    @@ fun () ->
    close_out_noerr stdin_oc;
    match read_streams_with_timeout ~stdout_ic ~stderr_ic ~timeout_seconds with
    | Error error ->
        (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
        (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
        Error error
    | Ok (stdout, stderr, deadline) -> (
        match waitpid_with_timeout pid deadline with
        | Ok status -> Ok { stdout; stderr; status }
        | Error error ->
            (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
            (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ());
            Error
              (match error with
              | Timed_out 0. -> Timed_out timeout_seconds
              | _ -> error))
  with Unix.Unix_error (error, fn, arg) ->
    Error (Spawn_error (error, fn, arg))

let is_available ~program : bool =
  let args = [| program; "--version" |] in
  match run_process_capture ~program ~args ~timeout_seconds:1.0 with
  | Ok { status = Unix.WEXITED 0; _ } -> true
  | Ok _ -> false
  | Error _ -> false

let args_for_mode ~program ~mode input_path =
  match mode with
  | Strict -> [| program; "--json"; input_path |]
  | Recovery -> [| program; "--json"; "--recovery"; input_path |]

let parse_to_json ~program ~timeout_seconds ~mode input =
  let tmp_file = Filename.temp_file "forester_rust_" ".tree" in
  Fun.protect ~finally:(fun () ->
      if Sys.file_exists tmp_file then Sys.remove tmp_file)
  @@ fun () ->
  let oc = open_out_bin tmp_file in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) @@ fun () ->
  output_string oc input;
  close_out oc;
  let args = args_for_mode ~program ~mode tmp_file in
  match run_process_capture ~program ~args ~timeout_seconds with
  | Ok { stdout; status = Unix.WEXITED 0; _ } -> Ok stdout
  | Ok { stdout; stderr; status = Unix.WEXITED code } ->
      let output_suffix =
        match format_process_output ~stdout ~stderr with
        | None -> ""
        | Some output -> "\n" ^ output
      in
      Error
        (Printf.sprintf "Rust parser `%s` exited with code %d.%s" program code
           output_suffix)
  | Ok { stdout; stderr; status = Unix.WSIGNALED sig_num } ->
      let output_suffix =
        match format_process_output ~stdout ~stderr with
        | None -> ""
        | Some output -> "\n" ^ output
      in
      Error
        (Printf.sprintf "Rust parser `%s` was terminated by signal %d.%s"
           program sig_num output_suffix)
  | Ok { stdout; stderr; status = Unix.WSTOPPED sig_num } ->
      let output_suffix =
        match format_process_output ~stdout ~stderr with
        | None -> ""
        | Some output -> "\n" ^ output
      in
      Error
        (Printf.sprintf "Rust parser `%s` was stopped by signal %d.%s" program
           sig_num output_suffix)
  | Error (Timed_out elapsed_seconds) ->
      Error
        (Printf.sprintf "Rust parser `%s` timed out after %.2fs." program
           elapsed_seconds)
  | Error (Spawn_error unix_error) ->
      Error
        (Printf.sprintf "Rust parser `%s` is unavailable: %s" program
           (format_unix_error unix_error))
