open Cascade
open Css_test_helpers

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rule css =
  match rules css with
  | [ r ] -> r
  | rs -> Alcotest.failf "expected one rule, got %d" (List.length rs)

let min_rule r = Pp.to_string ~minify:true Stylesheet.pp_rule r
let min_decl d = Pp.to_string ~minify:true Declaration.pp d

let test_decl_and_rule_sizes_match_minified_output () =
  let r = rule ".a{color:red;width:1px}" in
  let decl_bytes =
    List.fold_left
      (fun acc d -> acc + String.length (min_decl d))
      0 r.declarations
  in
  Alcotest.(check int)
    "declaration bytes" decl_bytes
    (Size.decls Size.default r.declarations);
  Alcotest.(check int)
    "rule bytes"
    (String.length (min_rule r))
    (Size.rule Size.default r)

let test_rules_size_is_sum () =
  let rs = rules ".a{color:red}.b{width:1px}" in
  Alcotest.(check int)
    "rules byte sum"
    (List.fold_left (fun acc r -> acc + Size.rule Size.default r) 0 rs)
    (Size.rules Size.default rs)

(* Every declaration the optimizer scores is one it already built, so measuring
   one should not print it again. The declarations below take each branch of the
   printer: a custom property, [!important], a [page-break-*] value with and
   without a [break-*] alias. *)
let tricky_declarations () =
  let body =
    String.concat ";"
      [
        "--x: a  b";
        "color:red!important";
        "page-break-before:always";
        "page-break-after:avoid";
        "width:calc(100% - 1px)";
      ]
  in
  (Fmt.kstr rule ".a{%s}" body).declarations

let test_declaration_size_is_its_rendering () =
  List.iter
    (fun d ->
      Alcotest.(check int)
        (min_decl d)
        (String.length (min_decl d))
        (Declaration.size d))
    (tricky_declarations ())

let test_measuring_declarations_allocates_nothing () =
  let decls = tricky_declarations () in
  Alcotest.(check (float 0.))
    "words allocated beyond the measurement's own"
    (measure (fun () -> 0))
    (measure (fun () -> Size.decls Size.default decls))

let suite =
  ( "size",
    [
      Alcotest.test_case "decl and rule sizes match minified output" `Quick
        test_decl_and_rule_sizes_match_minified_output;
      Alcotest.test_case "rules size is sum" `Quick test_rules_size_is_sum;
      Alcotest.test_case "declaration size is its rendering" `Quick
        test_declaration_size_is_its_rendering;
      Alcotest.test_case "measuring declarations allocates nothing" `Quick
        test_measuring_declarations_allocates_nothing;
    ] )
