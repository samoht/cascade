(** I/O helpers shared across the cascade CLI commands. *)

open Cascade

let read_file path = In_channel.with_open_bin path In_channel.input_all

let read_stdin () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    Buffer.contents buf
  with End_of_file -> Buffer.contents buf

let report_warning w =
  (* Prefix every line of a multi-line diagnostic so a downstream [grep -v
     "warning"] filters the whole entry, not just the first line. *)
  Cascade.Error.to_string w |> String.split_on_char '\n'
  |> List.iter (fun line -> Fmt.epr "warning: %s@." line)

(* [Error.to_string] already carries the filename the parse was stamped with. *)
let die_parse_error e =
  Fmt.epr "Error: %s@." (Cascade.Error.to_string e);
  Stdlib.exit 1

let parse ?(enforce_spec = false) ~filename css =
  match Css.of_string ~filename ~enforce_spec css with
  | Ok { Css.stylesheet; warnings } ->
      List.iter report_warning warnings;
      (stylesheet, warnings)
  | Error e -> die_parse_error e

let parse_css ?enforce_spec ~filename css =
  fst (parse ?enforce_spec ~filename css)

type input = { stylesheet : Css.t; filename : string; recovered : bool }

let read_input ?enforce_spec path =
  let css = if path = "-" then read_stdin () else read_file path in
  let filename = if path = "-" then "<stdin>" else path in
  let stylesheet, warnings = parse ?enforce_spec ~filename css in
  { stylesheet; filename; recovered = warnings <> [] }

(* Recovery that drops every rule leaves nothing to serialise, so [cascade fmt
   src.css > dist.css] writes a 0-byte file. Warnings on stderr are not
   something a build can gate on; the exit status is. An input that had nothing
   to drop (comments only, an empty rule) stays a plain empty result. *)
let check_not_all_dropped input ~written =
  if written = 0 && input.recovered then begin
    Fmt.epr
      "Error: %s: parse dropped every rule; refusing to write an empty \
       stylesheet@."
      input.filename;
    Stdlib.exit 1
  end

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")
