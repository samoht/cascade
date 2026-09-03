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

(* A stylesheet and the name to report it under. [-] is standard input, which
   has no path, so it borrows the name every command already gives it. *)
let read_source path =
  if path = "-" then (read_stdin (), "<stdin>") else (read_file path, path)

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
  | Ok { Css.stylesheet; warnings; _ } ->
      List.iter report_warning warnings;
      (stylesheet, warnings)
  | Error e -> die_parse_error e

let parse_css ?enforce_spec ~filename css =
  fst (parse ?enforce_spec ~filename css)

type input = { stylesheet : Css.t; filename : string; recovered : bool }

let read_input ?enforce_spec path =
  let css, filename = read_source path in
  let stylesheet, warnings = parse ?enforce_spec ~filename css in
  { stylesheet; filename; recovered = warnings <> [] }

(* Recovery that drops every rule leaves nothing to serialise, so [cascade fmt
   src.css > dist.css] writes a 0-byte file. Warnings on stderr are not
   something a build can gate on; the exit status is. An input that had nothing
   to drop (comments only, an empty rule) stays a plain empty result.

   Whether the parse produced anything is a question about the statement list,
   the same one [cascade apply] asks of each [<style>] block. Counting the bytes
   printed answers a different one and gets it wrong both ways: a rule survives
   a declaration it could not read and prints nothing, and a sheet that lost
   nothing still prints nothing once [--minify] drops a redundant [@charset] or
   an [src]-less [@font-face]. Neither is a parse that dropped every rule. *)
let check_not_all_dropped input =
  match input.stylesheet with
  | _ :: _ -> ()
  | [] ->
      if input.recovered then begin
        Fmt.epr
          "Error: %s: parse dropped every rule; refusing to write an empty \
           stylesheet@."
          input.filename;
        Stdlib.exit 1
      end

let split_comma s =
  String.split_on_char ',' s |> List.map String.trim
  |> List.filter (fun s -> s <> "")
