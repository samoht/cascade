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

let same_decl a b = a == b || a = b

let test_pseudo_and_vendor_detection () =
  Alcotest.(check (option string))
    "pseudo at selector tail" (Some "::before")
    (Option.map Selector.to_string
       (Merge.pseudo (Selector.of_string ".a .b::before")));
  Alcotest.(check bool)
    "vendor pseudo detected" true
    (Merge.vendor (Selector.of_string ".a::-webkit-scrollbar"));
  Alcotest.(check bool)
    "regular selector is not vendor" false
    (Merge.vendor (Selector.of_string ".a::before"))

let test_declarations_equal_fast_and_structural_paths () =
  let r = rule ".a{color:red;width:1px}" in
  Alcotest.(check bool)
    "physical list fast path" true
    (Merge.declarations_equal ~same:same_decl r.declarations r.declarations);
  let r2 = rule ".b{color:red;width:1px}" in
  Alcotest.(check bool)
    "structural path" true
    (Merge.declarations_equal ~same:same_decl r.declarations r2.declarations);
  let r3 = rule ".c{color:red}" in
  Alcotest.(check bool)
    "different length" false
    (Merge.declarations_equal ~same:same_decl r.declarations r3.declarations)

(* [compatible] is reflexive and symmetric but NOT transitive. A plain selector
   sits beside one carrying a newer pseudo-class, and beside an
   [:is(:where(...))] variant, while those two never sit together: a browser
   that does not know [:user-valid] drops the whole rule, where the forgiving
   variant would have survived. Grouping by sorting a run and checking only
   neighbours is unsound for exactly this reason. *)
let test_compatible_is_not_transitive () =
  let newer = Selector.of_string ".x:user-valid" in
  let plain = Selector.of_string ".y" in
  let guarded = Selector.of_string ":is(:where(.group):hover .z)" in
  Alcotest.(check bool) "newer with plain" true (Merge.compatible newer plain);
  Alcotest.(check bool)
    "plain with guarded" true
    (Merge.compatible plain guarded);
  Alcotest.(check bool)
    "newer with guarded" false
    (Merge.compatible newer guarded);
  Alcotest.(check bool)
    "and the other way round" false
    (Merge.compatible guarded newer);
  Alcotest.(check bool) "reflexive on newer" true (Merge.compatible newer newer);
  Alcotest.(check bool)
    "reflexive on guarded" true
    (Merge.compatible guarded guarded)

let suite =
  ( "merge",
    [
      Alcotest.test_case "pseudo and vendor detection" `Quick
        test_pseudo_and_vendor_detection;
      Alcotest.test_case "compatible is not transitive" `Quick
        test_compatible_is_not_transitive;
      Alcotest.test_case "declarations_equal fast and structural paths" `Quick
        test_declarations_equal_fast_and_structural_paths;
    ] )
