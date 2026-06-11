open Cascade

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
let min_decl d = Pp.to_string ~minify:true Declaration.pp_declaration d

let test_decl_list_separator_math () =
  Alcotest.(check int) "empty list" 0 (Size.decl_list 0 0);
  Alcotest.(check int) "single declaration" 9 (Size.decl_list 9 1);
  Alcotest.(check int) "three declarations" 11 (Size.decl_list 9 3)

let test_rule_from_parts () =
  Alcotest.(check int) "empty declarations" 4 (Size.rule_from_parts 2 0 0);
  Alcotest.(check int) "one declaration" 13 (Size.rule_from_parts 2 9 1);
  Alcotest.(check int) "two declarations" 23 (Size.rule_from_parts 2 18 2)

let test_decl_and_rule_sizes_match_minified_output () =
  let r = rule ".a{color:red;width:1px}" in
  let decl_bytes =
    List.fold_left
      (fun acc d -> acc + String.length (min_decl d))
      0 r.declarations
  in
  Alcotest.(check int)
    "declaration bytes" decl_bytes
    (Size.decls r.declarations);
  Alcotest.(check int) "rule bytes" (String.length (min_rule r)) (Size.rule r)

let test_rules_size_is_sum () =
  let rs = rules ".a{color:red}.b{width:1px}" in
  Alcotest.(check int)
    "rules byte sum"
    (List.fold_left (fun acc r -> acc + Size.rule r) 0 rs)
    (Size.rules rs)

let suite =
  ( "size",
    [
      Alcotest.test_case "decl list separator math" `Quick
        test_decl_list_separator_math;
      Alcotest.test_case "rule_from_parts" `Quick test_rule_from_parts;
      Alcotest.test_case "decl and rule sizes match minified output" `Quick
        test_decl_and_rule_sizes_match_minified_output;
      Alcotest.test_case "rules size is sum" `Quick test_rules_size_is_sum;
    ] )
