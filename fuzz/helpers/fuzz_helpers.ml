(** Shared helpers for fuzz test modules. *)

open Cascade
open Alcobar

let assert_invalid_declaration_contract label input =
  let css = ".x{" ^ input ^ "}" in
  match Css.of_string ~strict:true css with
  | Ok parsed ->
      failf "%s parsed strictly as invalid declaration: %S -> %S" label input
        (Css.to_string ~minify:true parsed.stylesheet)
  | Error _ ->
      let { Css.warnings; stylesheet } =
        match Css.of_string ~strict:false css with
        | Ok parsed -> parsed
        | Error err ->
            failf "%s did not recover leniently: %s" label
              (Cascade.Error.to_string err)
      in
      ignore (Css.to_string ~minify:true stylesheet : string);
      if warnings = [] then
        failf "%s recovered without a lenient warning: %S" label input

let shapes_with_rule_runs ~boundary_shape ss =
  let rec loop acc seen_rule = function
    | [] -> if seen_rule then List.rev ("rules" :: acc) else List.rev acc
    | Css.Stylesheet.Rule _ :: rest -> loop acc true rest
    | other :: rest ->
        let acc = if seen_rule then "rules" :: acc else acc in
        loop (List.rev_append (boundary_shape other) acc) false rest
  in
  loop [] false ss
