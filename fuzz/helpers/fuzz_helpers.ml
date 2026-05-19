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
