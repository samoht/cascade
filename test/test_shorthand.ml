open Cascade

let decl css = Declaration.of_string css

let decls css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> (
      match Css.statements stylesheet with
      | [ Stylesheet.Rule r ] -> r.declarations
      | _ -> Alcotest.fail "expected one rule")
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let decl_strings decls =
  List.map (Pp.to_string ~minify:true Declaration.pp_declaration) decls

let indexed decls = List.mapi (fun i d -> (i, d)) decls
let unindexed decls = List.map snd decls

let test_declaration_covers_reset_boundaries () =
  Alcotest.(check bool)
    "all covers ordinary property" true
    (Shorthand.declaration_covers (decl "all:initial") (decl "color:red"));
  Alcotest.(check bool)
    "all does not cover direction" false
    (Shorthand.declaration_covers (decl "all:initial") (decl "direction:ltr"));
  Alcotest.(check bool)
    "all does not cover custom properties" false
    (Shorthand.declaration_covers (decl "all:initial") (decl "--brand:red"));
  Alcotest.(check bool)
    "border covers side width" true
    (Shorthand.declaration_covers
       (decl "border:1px solid red")
       (decl "border-top-width:2px"))

let test_intentionally_duplicated_properties () =
  Alcotest.(check bool)
    "content duplicates are preserved" true
    (Shorthand.is_intentionally_duplicated (decl "content:\"a\""));
  Alcotest.(check bool)
    "outline duplicates are preserved" true
    (Shorthand.is_intentionally_duplicated (decl "outline:1px solid red"));
  Alcotest.(check bool)
    "color is ordinary" false
    (Shorthand.is_intentionally_duplicated (decl "color:red"))

let test_merge_overflow_longhands () =
  let merged =
    ".a{overflow-x:hidden;color:red;overflow-y:auto}" |> decls |> indexed
    |> Shorthand.merge_overflow_longhands |> unindexed
  in
  Alcotest.(check (list string))
    "x and y become overflow without disturbing middle declarations"
    [ "overflow:hidden auto"; "color:red" ]
    (decl_strings merged);
  let blocked =
    ".a{overflow-x:hidden;overflow:auto;overflow-y:hidden}" |> decls |> indexed
    |> Shorthand.merge_overflow_longhands |> unindexed
  in
  Alcotest.(check (list string))
    "existing overflow blocks composition"
    [ "overflow-x:hidden"; "overflow:auto"; "overflow-y:hidden" ]
    (decl_strings blocked)

let test_compose_shorthands_and_runtime_guard () =
  let ctx = Ctx.fragment in
  let composed =
    ".a{margin-top:1px;margin-right:2px;margin-bottom:1px;margin-left:2px}"
    |> decls |> indexed
    |> Shorthand.compose_shorthands ~ctx
    |> unindexed
  in
  Alcotest.(check (list string))
    "box longhands compose" [ "margin:1px 2px" ] (decl_strings composed);
  let guarded =
    ".a{border-top-width:var(--w);border-right-width:1px;border-bottom-width:1px;border-left-width:1px;border-top-style:solid;border-right-style:solid;border-bottom-style:solid;border-left-style:solid;border-top-color:red;border-right-color:red;border-bottom-color:red;border-left-color:red}"
    |> decls |> indexed
    |> Shorthand.compose_shorthands ~ctx
    |> unindexed
  in
  Alcotest.(check int)
    "runtime substitution prevents typed border shorthand" 12
    (List.length guarded)

let test_deduplicate_keeps_legacy_fallbacks () =
  let result =
    ".a{display:-webkit-box;display:flex;color:red;color:blue}" |> decls
    |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "vendor fallback is kept but ordinary duplicate is dropped"
    [ "display:-webkit-box"; "display:flex"; "color:blue" ]
    (decl_strings result)

let suite =
  ( "shorthand",
    [
      Alcotest.test_case "declaration coverage reset boundaries" `Quick
        test_declaration_covers_reset_boundaries;
      Alcotest.test_case "intentionally duplicated properties" `Quick
        test_intentionally_duplicated_properties;
      Alcotest.test_case "merge overflow longhands" `Quick
        test_merge_overflow_longhands;
      Alcotest.test_case "compose shorthands and runtime guard" `Quick
        test_compose_shorthands_and_runtime_guard;
      Alcotest.test_case "deduplicate keeps legacy fallbacks" `Quick
        test_deduplicate_keeps_legacy_fallbacks;
    ] )
