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

(* [Error.to_string] carries the filename the parse was stamped with. *)
let report_warning w =
  Fmt.epr "%s@." (Cascade.Error.to_string ~severity:`Warning w)

let report_error e = Fmt.epr "%s@." (Cascade.Error.to_string ~severity:`Error e)

let die_parse_error e =
  report_error e;
  Stdlib.exit 1

(* A sheet that trips one unsupported construct a hundred times would bury
   everything else under the same message. Warnings with the same message are
   one group, printed once where it first occurs, with a line counting the rest
   and naming where the next few are. *)
let max_positions = 3

let position (w : Cascade.Error.t) =
  match Cascade.Error.position w with
  | Some (line, column) ->
      String.concat ":" [ Int.to_string line; Int.to_string column ]
  | None -> "?"

let report_repeats = function
  | [] -> ()
  | rest ->
      let n = List.length rest in
      let shown = List.filteri (fun i _ -> i < max_positions) rest in
      let more = if n > max_positions then [ "..." ] else [] in
      Fmt.epr "  (%d more like this, at %s)@." n
        (String.concat ", " (List.map position shown @ more))

let group_warnings ws =
  let groups = Hashtbl.create 16 in
  let order =
    List.fold_left
      (fun order w ->
        let key = Cascade.Pp.to_string Cascade.Error.pp_message w in
        match Hashtbl.find_opt groups key with
        | Some rest ->
            Hashtbl.replace groups key (w :: rest);
            order
        | None ->
            Hashtbl.add groups key [];
            (key, w) :: order)
      [] ws
  in
  List.rev_map (fun (key, w) -> (w, List.rev (Hashtbl.find groups key))) order

let report_warnings (verbosity : Cli_log.verbosity) ~filename ws =
  match verbosity with
  | Quiet -> ()
  | Verbose -> List.iter report_warning ws
  | Normal ->
      let groups = group_warnings ws in
      List.iter
        (fun (w, rest) ->
          report_warning w;
          report_repeats rest)
        groups;
      let total = List.length ws and shown = List.length groups in
      if total > shown then
        Fmt.epr "%s: %d warnings, %d shown; -v prints every one, -q none@."
          filename total shown

let parse verbosity ?(enforce_spec = false) ~filename css =
  match Css.of_string ~filename ~enforce_spec css with
  | Ok { Css.stylesheet; warnings; _ } ->
      report_warnings verbosity ~filename warnings;
      (stylesheet, warnings)
  | Error e -> die_parse_error e

let parse_css verbosity ?enforce_spec ~filename css =
  fst (parse verbosity ?enforce_spec ~filename css)

type input = { stylesheet : Css.t; filename : string; recovered : bool }

let read_input verbosity ?enforce_spec path =
  let css, filename = read_source path in
  let stylesheet, warnings = parse verbosity ?enforce_spec ~filename css in
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
