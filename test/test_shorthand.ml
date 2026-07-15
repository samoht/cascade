open Cascade
open Css_test_helpers

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

let test_drop_redundant_longhand_after_shorthand () =
  (* A background shorthand sets every longhand it covers, so a later longhand
     equal to what it set (its slot value, else the initial) is a no-op. *)
  decl_optimizes_to ~into:"background:red" "background:red;background-size:auto";
  decl_optimizes_to ~into:"background:red"
    "background:red;background-repeat:repeat";
  decl_optimizes_to ~into:"background:red"
    "background:red;background-attachment:scroll";
  (* A non-initial value, or an override of a size the shorthand set, is
     kept. *)
  decl_optimizes_to ~into:"background:red;background-size:cover"
    "background:red;background-size:cover";
  decl_optimizes_to ~into:"background:url(x)50%/cover;background-size:auto"
    "background:url(x) center/cover;background-size:auto";
  (* Importance mismatch is a real change, not a no-op. *)
  decl_optimizes_to ~into:"background:red;background-size:auto!important"
    "background:red;background-size:auto!important"

let test_drop_redundant_flex_longhand () =
  (* [flex] expands to grow/shrink/basis: a later grow/shrink equal to what it
     set is dropped. flex:1 is [1 1 0], auto is [1 1 auto], none is [0 0
     auto]. *)
  decl_optimizes_to ~into:"flex:1" "flex:1;flex-grow:1";
  decl_optimizes_to ~into:"flex:auto" "flex:auto;flex-grow:1";
  decl_optimizes_to ~into:"flex:none" "flex:none;flex-shrink:0";
  decl_optimizes_to ~into:"flex:2 3" "flex:2 3;flex-grow:2";
  (* A differing factor is kept. *)
  decl_optimizes_to ~into:"flex:1;flex-grow:2" "flex:1;flex-grow:2"

let test_drop_redundant_transition_longhand () =
  (* [transition] sets duration/timing/delay/behavior; a later longhand equal to
     its slot (else the initial 0s/ease/normal) is dropped. *)
  decl_optimizes_to ~into:"transition:all 1s"
    "transition:all 1s;transition-delay:0s";
  decl_optimizes_to ~into:"transition:all 1s"
    "transition:all 1s;transition-timing-function:ease";
  decl_optimizes_to ~into:"transition:all 1s 2s"
    "transition:all 1s 2s;transition-delay:2s";
  (* A differing value is kept. *)
  decl_optimizes_to ~into:"transition:all 1s;transition-delay:1s"
    "transition:all 1s;transition-delay:1s"

let test_drop_redundant_border_longhand () =
  (* [border] sets width/style/color; a later longhand equal to an explicit slot
     is dropped. A differing value or a per-side list is kept. *)
  decl_optimizes_to ~into:"border:1px solid red"
    "border:1px solid red;border-color:red";
  decl_optimizes_to ~into:"border:1px solid red"
    "border:1px solid red;border-width:1px";
  decl_optimizes_to ~into:"border:1px solid red"
    "border:1px solid red;border-style:solid";
  decl_optimizes_to ~into:"border:1px solid red;border-color:#00f"
    "border:1px solid red;border-color:blue";
  decl_optimizes_to ~into:"border:1px solid red;border-color:red green"
    "border:1px solid red;border-color:red green"

let test_drop_redundant_font_longhand () =
  (* [font] resets style/weight/stretch/line-height to normal; a later longhand
     equal to that (weight normal folds to 400) is dropped. *)
  decl_optimizes_to ~into:"font:16px sans-serif"
    "font:16px sans-serif;font-weight:400";
  decl_optimizes_to ~into:"font:16px sans-serif"
    "font:16px sans-serif;font-weight:normal";
  decl_optimizes_to ~into:"font:16px sans-serif"
    "font:16px sans-serif;font-style:normal";
  decl_optimizes_to ~into:"font:700 16px x" "font:bold 16px x;font-weight:bold";
  (* A differing value is kept. *)
  decl_optimizes_to ~into:"font:16px sans-serif;font-weight:700"
    "font:16px sans-serif;font-weight:700"

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

let test_stylesheet_scope_prior_longhand_guard () =
  (* A layer shorthand resets every layer field, so synthesizing one from a run
     would silently revert a same-family longhand that sits before the run.
     Closed-world (stylesheet) scope is closed over external CSS only, not over
     this same-block longhand, so composition must refuse here. *)
  let ctx = Ctx.of_scope (Some `Stylesheet) in
  let compose css =
    css |> decls |> indexed
    |> Shorthand.compose_shorthands ~ctx
    |> unindexed |> decl_strings
  in
  Alcotest.(check (list string))
    "a prior background-clip blocks background composition"
    [
      "background-clip:text";
      "color:red";
      "background-color:blue";
      "background-image:url(x.png)";
    ]
    (compose
       ".a{background-clip:text;color:red;background-color:blue;background-image:url(x.png)}");
  Alcotest.(check (list string))
    "a prior mask-clip blocks mask composition"
    [
      "mask-clip:view-box";
      "color:red";
      "mask-image:url(x.png)";
      "mask-repeat:no-repeat";
    ]
    (compose
       ".a{mask-clip:view-box;color:red;mask-image:url(x.png);mask-repeat:no-repeat}")

let test_deduplicate_keeps_legacy_fallbacks () =
  let result =
    ".a{display:-webkit-box;display:flex;color:red;color:blue}" |> decls
    |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "vendor fallback is kept but ordinary duplicate is dropped"
    [ "display:-webkit-box"; "display:flex"; "color:blue" ]
    (decl_strings result);
  (* The prefixed-keyword fallbacks for position / text-align are kept too: old
     Safari only understands -webkit-sticky / -webkit-match-parent. *)
  let result =
    ".a{position:-webkit-sticky;position:sticky;text-align:-webkit-match-parent;text-align:start}"
    |> decls |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "prefixed-keyword position/text-align fallbacks are kept"
    [
      "position:-webkit-sticky";
      "position:sticky";
      "text-align:-webkit-match-parent";
      "text-align:start";
    ]
    (decl_strings result);
  (* The prefixed intrinsic sizing keywords are kept against the unprefixed
     form, on a length-percentage and a min-* property. *)
  let result =
    ".a{width:-webkit-max-content;width:max-content;min-width:-moz-fit-content;min-width:fit-content}"
    |> decls |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "prefixed sizing keyword fallbacks are kept"
    [
      "width:-webkit-max-content";
      "width:max-content";
      "min-width:-moz-fit-content";
      "min-width:fit-content";
    ]
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
      Alcotest.test_case "drop redundant longhand after shorthand" `Quick
        test_drop_redundant_longhand_after_shorthand;
      Alcotest.test_case "drop redundant flex longhand" `Quick
        test_drop_redundant_flex_longhand;
      Alcotest.test_case "drop redundant transition longhand" `Quick
        test_drop_redundant_transition_longhand;
      Alcotest.test_case "drop redundant border longhand" `Quick
        test_drop_redundant_border_longhand;
      Alcotest.test_case "drop redundant font longhand" `Quick
        test_drop_redundant_font_longhand;
      Alcotest.test_case "compose shorthands and runtime guard" `Quick
        test_compose_shorthands_and_runtime_guard;
      Alcotest.test_case "stylesheet scope prior longhand guard" `Quick
        test_stylesheet_scope_prior_longhand_guard;
      Alcotest.test_case "deduplicate keeps legacy fallbacks" `Quick
        test_deduplicate_keeps_legacy_fallbacks;
    ] )
