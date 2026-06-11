(* Scan CSS fixtures for [Unknown_property] declarations and report them.

   Every [Unknown_property] occurrence is a CSS property that cascade has not
   typed yet. The typed/typed [Shorthand.vendor_alias_redundant] structural drop
   only fires for typed pairs, so any vendor-prefixed property landing here is
   potentially a missed minification opportunity.

   Output: one line per distinct property name with its occurrence count across
   the fixtures. Vendor-prefixed names whose unprefixed form is typed are tagged
   [DROP_CANDIDATE]; other names are tagged [TYPE_ME]. The process exits 0
   always; treat the report as advisory. *)

open Cascade

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Walk all rule bodies (including nested) and at-rule blocks, calling [f] on
   every declaration. *)
let rec iter_decls_in_statements f stmts =
  List.iter (iter_decls_in_statement f) stmts

and iter_decls_in_statement f = function
  | Stylesheet.Rule r ->
      List.iter f r.declarations;
      iter_decls_in_statements f r.nested
  | Stylesheet.Media (_, body)
  | Stylesheet.Supports (_, body)
  | Stylesheet.Container (_, _, body)
  | Stylesheet.Layer (_, body)
  | Stylesheet.Origin (_, body)
  | Stylesheet.Scope (_, _, body) ->
      iter_decls_in_statements f body
  | Stylesheet.Keyframes (_, frames)
  | Stylesheet.Webkit_keyframes (_, frames)
  | Stylesheet.Moz_keyframes (_, frames) ->
      List.iter
        (fun (frame : Stylesheet.keyframe) -> List.iter f frame.declarations)
        frames
  | _ -> ()

let unknown_name_of_decl = function
  | Declaration.Declaration { property = Properties.Unknown_property name; _ }
    ->
      Some name
  | _ -> None

let vendor_prefixes = [ "-webkit-"; "-moz-"; "-ms-"; "-o-" ]

let strip_vendor_prefix name =
  let rec try_prefixes = function
    | [] -> None
    | p :: rest ->
        let n = String.length name in
        let pn = String.length p in
        if n > pn && String.sub name 0 pn = p then
          Some (String.sub name pn (n - pn))
        else try_prefixes rest
  in
  try_prefixes vendor_prefixes

let modern_is_typed name =
  let cursor = Cursor.of_string name in
  match Properties.read_any_property cursor with
  | Properties.Prop (Properties.Unknown_property _) -> false
  | Properties.Prop _ -> true
  | exception _ -> false

let classify name =
  match strip_vendor_prefix name with
  | Some modern when modern_is_typed modern -> `Drop_candidate
  | _ -> `Type_me

let collect_from_file counts path =
  let css = read_file path in
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      iter_decls_in_statements
        (fun decl ->
          match unknown_name_of_decl decl with
          | None -> ()
          | Some name ->
              let cur = try Hashtbl.find counts name with Not_found -> 0 in
              Hashtbl.replace counts name (cur + 1))
        stylesheet
  | Error _ -> ()

let () =
  let files =
    if Array.length Sys.argv > 1 then
      Array.sub Sys.argv 1 (Array.length Sys.argv - 1) |> Array.to_list
    else (
      prerr_endline "usage: check_unknown_properties FILE.css [FILE.css ...]";
      exit 2)
  in
  let counts : (string, int) Hashtbl.t = Hashtbl.create 256 in
  List.iter (collect_from_file counts) files;
  let entries =
    Hashtbl.fold (fun n c acc -> (n, c) :: acc) counts []
    |> List.sort (fun (_, a) (_, b) -> compare b a)
  in
  let total_drop = ref 0 in
  let total_type = ref 0 in
  List.iter
    (fun (name, count) ->
      let tag, bump =
        match classify name with
        | `Drop_candidate -> ("DROP_CANDIDATE", total_drop)
        | `Type_me -> ("TYPE_ME", total_type)
      in
      bump := !bump + count;
      Fmt.pr "%-15s %5d  %s@." tag count name)
    entries;
  Fmt.pr
    "@.DROP_CANDIDATE total: %d occurrences (add to vendor_alias_redundant)@."
    !total_drop;
  Fmt.pr "TYPE_ME total:        %d occurrences (consider typing)@." !total_type
