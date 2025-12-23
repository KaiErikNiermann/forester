(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

let render_document_class fmt document_class document_class_options =
  match document_class_options with
  | [] -> Format.fprintf fmt "\\documentclass{%s}" document_class
  | _ ->
    let opts = String.concat "," document_class_options in
    Format.fprintf fmt "\\documentclass[%s]{%s}" opts document_class

let pp fmt ~document_class ~document_class_options ~preamble ~body =
  let newline () = Format.fprintf fmt "@\n" in

  (* Documentclass with safer, more flexible options *)
  render_document_class fmt document_class document_class_options;
  newline ();

  (* Engine abstraction and base math packages *)
  Format.fprintf fmt {|
  \usepackage{iftex}
  \ifPDFTeX
    \usepackage[T1]{fontenc}
    \usepackage[utf8]{inputenc}
  \else
    \usepackage{fontspec}
  \fi

  \usepackage{amsmath,amssymb,mathtools}
  |};
  newline ();

  (* User-provided preamble (fonts, macros, etc.) *)
  Format.fprintf fmt "%s" preamble;
  newline ();

  (* Body *)
  Format.fprintf fmt "\\begin{document}";
  newline ();
  Format.fprintf fmt "%s" body;
  newline ();
  Format.fprintf fmt "\\end{document}";
  newline ()

let to_string ~document_class ~document_class_options ~preamble ~body =
  Format.asprintf "%a"
    (fun fmt _ -> pp ~document_class ~document_class_options ~preamble ~body fmt)
    ()
