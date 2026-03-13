(* SPDX-FileCopyrightText: 2026 The Forester Project Contributors *)
(* SPDX-License-Identifier: GPL-3.0-or-later *)

let is_executable_file path =
  (not (Sys.is_directory path))
  && try
    Unix.access path [Unix.X_OK];
    true
  with
    | Unix.Unix_error _ -> false

let path_entries () =
  match Sys.getenv_opt "PATH" with
  | None -> []
  | Some value ->
    value |> String.split_on_char ':'
    |> List.filter (fun entry -> String.trim entry <> "")

let find_in_path executable =
  path_entries ()
  |> List.find_map (fun directory ->
      let candidate = Filename.concat directory executable in
      if Sys.file_exists candidate && is_executable_file candidate then
        Some candidate
      else None
    )

let resolve_binary ?env_var ?(candidates = []) executable =
  let candidate_paths =
    let env_paths =
      match env_var with
      | None -> []
      | Some name ->
        (
          match Sys.getenv_opt name with
          | Some path when String.trim path <> "" -> [path]
          | _ -> []
        )
    in
    env_paths @ candidates
  in
  match List.find_opt
    (fun path -> Sys.file_exists path && is_executable_file path)
    candidate_paths with
  | Some path -> Some path
  | None -> find_in_path executable

let bool_of_env name =
  match Sys.getenv_opt name with
  | Some "1" | Some "true" | Some "yes" | Some "on" -> true
  | _ -> false

let rec find_repo_root dir =
  let dune_project = Filename.concat dir "dune-project" in
  let opam_file = Filename.concat dir "forester.opam" in
  if Sys.file_exists dune_project && Sys.file_exists opam_file then Some dir
  else
    let parent = Filename.dirname dir in
    if parent = dir then None else find_repo_root parent

let repo_root () =
  match find_repo_root (Sys.getcwd ()) with
  | Some root -> root
  | None ->
    Alcotest.fail "Unable to locate repository root from current directory"

let read_file path = In_channel.with_open_bin path In_channel.input_all

let trim s =
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  let len = String.length s in
  let rec left i =
    if i >= len then len else if is_space s.[i] then left (i + 1) else i
  in
  let rec right i =
    if i < 0 then -1 else if is_space s.[i] then right (i - 1) else i
  in
  let i = left 0 in
  let j = right (len - 1) in
  if j < i then "" else String.sub s i (j - i + 1)

let strip_comment line =
  match String.index_opt line '#' with
  | None -> line
  | Some i -> String.sub line 0 i

let load_manifest_entries manifest_path =
  read_file manifest_path |> String.split_on_char '\n' |> List.map strip_comment
  |> List.map trim
  |> List.filter (fun line -> line <> "")

let run_command_capture command =
  let input_channel = Unix.open_process_in command in
  let output = Buffer.create 1024 in
  (
    try
      while true do
        Buffer.add_string output (input_line input_channel);
        Buffer.add_char output '\n'
      done
    with
      | End_of_file -> ()
  );
  let status = Unix.close_process_in input_channel in
    (status, Buffer.contents output)

let with_temp_file
    ?(prefix = "forester-test-")
    ?(suffix = ".tmp")
    ?mode
    content
    f
  =
  let path = Filename.temp_file prefix suffix in
  let oc = open_out_bin path in
  Fun.protect
    ~finally: (fun () ->
      close_out_noerr oc;
      if Sys.file_exists path then Sys.remove path
    )
    @@ fun () ->
    output_string oc content;
    close_out oc;
    Option.iter (Unix.chmod path) mode;
    f path

let with_temp_script
    ?(prefix = "forester-script-")
    ?(header = "#!/bin/sh\n")
    body
    f
  =
  with_temp_file ~prefix ~suffix: ".sh" ~mode: 0o755 (header ^ body) f
