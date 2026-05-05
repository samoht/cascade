(** I/O helpers shared across the cascade CLI commands. *)

open Cascade

let read_file path =
  let ic = open_in path in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let read_stdin () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with End_of_file -> Buffer.contents buf

let parse_css ~filename css =
  match Css.of_string ~filename css with
  | Ok s -> s
  | Error _ -> (
      (* Strict parsing failed; retry with the recovery parser so a single
         broken rule doesn't take the whole file down. The lenient parser shares
         a bug with [@import url(...)] tokenisation, so guard the exception that
         surfaces as [Invalid_argument]. *)
      try
        let { Css.stylesheet; warnings } = Css.parse ~filename css in
        List.iter
          (fun w -> Fmt.epr "warning: %s@." (Css.pp_parse_error w))
          warnings;
        stylesheet
      with Invalid_argument _ ->
        Fmt.epr "warning: %s: parse failed, dropping content@." filename;
        Css.empty)

let read_input path =
  let css = if path = "-" then read_stdin () else read_file path in
  let filename = if path = "-" then "<stdin>" else path in
  parse_css ~filename css

let print_output output =
  print_string output;
  if output <> "" && output.[String.length output - 1] <> '\n' then
    print_newline ()

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")
