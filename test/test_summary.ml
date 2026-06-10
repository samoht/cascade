open Cascade

let rules css =
  match Css.of_string css with
  | Ok { stylesheet; _ } ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        stylesheet
      |> Array.of_list
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let rule css =
  match rules css with
  | [| rule |] -> rule
  | rules -> Alcotest.failf "expected one rule, got %d" (Array.length rules)

let decl css = Declaration.of_string css
let decl_string decl = Declaration.to_string ~minify:true decl
let decl_size decl = String.length (decl_string decl)

let selector_size selector =
  String.length (Selector.to_string ~minify:true selector)

let summary rule =
  Summary.v ~rule_size:(fun _ -> 123) ~decl_size ~selector_size rule

let same a b = a == b || (Declaration.hash a = Declaration.hash b && a = b)

let test_rule_identity_and_first_decl () =
  Summary.reset ();
  let rule = rule ".a{color:red;color:blue;width:1px}" in
  let t = summary rule in
  Alcotest.(check bool) "rule identity" true (Summary.rule t == rule);
  Alcotest.(check int) "decl count" 3 (Summary.decl_count t);
  Alcotest.(check int) "selector size" 2 (Summary.selector_size t);
  let color = decl "color:red" in
  let width = decl "width:1px" in
  Alcotest.(check bool)
    "declares props" true
    (Summary.declares_all t [ Summary.prop color; Summary.prop width ]);
  Alcotest.(check bool) "contains color" true (Summary.contains ~same t color);
  match Summary.decl_for_prop t (Summary.prop color) with
  | Some first ->
      Alcotest.(check string)
        "first declaration" "color:red" (decl_string first)
  | None -> Alcotest.fail "missing color declaration"

let test_ids_intersection () =
  Summary.reset ();
  let a = summary (rule ".a{color:red;width:1px}") in
  let b = summary (rule ".b{color:blue;height:1px}") in
  let c = summary (rule ".c{margin:0}") in
  let common = Summary.ids_inter (Summary.prop_ids a) (Summary.prop_ids b) in
  Alcotest.(check bool) "common is non-empty" false (Summary.ids_empty common);
  Alcotest.(check bool) "a declares common" true (Summary.declares_ids a common);
  Alcotest.(check bool)
    "c disjoint" true
    (Summary.ids_disjoint common (Summary.prop_ids c))

let test_duplicate_property_maps_keep_first_decl_and_size () =
  Summary.reset ();
  let t = summary (rule ".a{color:red;width:1px;color:blue}") in
  let color = Summary.prop (decl "color:green") in
  Alcotest.(check (option string))
    "first declaration for duplicate property" (Some "color:red")
    (Option.map decl_string (Summary.decl_for_prop t color));
  Alcotest.(check (option int))
    "size map follows first duplicate declaration"
    (Some (String.length "color:red"))
    (Summary.decl_size_for_prop t color)

let test_decl_property_ids_preserve_duplicate_source_order () =
  Summary.reset ();
  let t = summary (rule ".a{color:red;width:1px;color:blue}") in
  let prop_ids = Summary.prop_ids t in
  let decl_prop_ids = Summary.decl_prop_ids t in
  Alcotest.(check int) "unique property ids" 2 (Array.length prop_ids);
  Alcotest.(check int) "ids per declaration" 3 (Array.length decl_prop_ids);
  Alcotest.(check bool)
    "duplicate color ids match" true
    (decl_prop_ids.(0) = decl_prop_ids.(2));
  Alcotest.(check bool)
    "width id differs from color id" true
    (decl_prop_ids.(0) <> decl_prop_ids.(1));
  Alcotest.(check bool)
    "color id is in property id set" true
    (Summary.ids_mem decl_prop_ids.(0) prop_ids);
  Alcotest.(check bool)
    "width id is in property id set" true
    (Summary.ids_mem decl_prop_ids.(1) prop_ids);
  Alcotest.(check (array int))
    "self intersection is stable" prop_ids
    (Summary.ids_inter prop_ids prop_ids)

let test_ids_set_operations_with_partial_overlap () =
  Summary.reset ();
  let a = summary (rule ".a{color:red;width:1px;margin:0}") in
  let b = summary (rule ".b{width:2px;height:1px}") in
  let c = summary (rule ".c{padding:1px}") in
  let shared = Summary.ids_inter (Summary.prop_ids a) (Summary.prop_ids b) in
  Alcotest.(check bool)
    "overlapping summaries are not disjoint" false
    (Summary.ids_disjoint (Summary.prop_ids a) (Summary.prop_ids b));
  Alcotest.(check bool)
    "intersection is subset of left" true
    (Summary.ids_subset shared (Summary.prop_ids a));
  Alcotest.(check bool)
    "intersection is subset of right" true
    (Summary.ids_subset shared (Summary.prop_ids b));
  Alcotest.(check bool)
    "unrelated summary is disjoint from intersection" true
    (Summary.ids_disjoint shared (Summary.prop_ids c));
  Alcotest.(check bool)
    "intersection is non-empty" false (Summary.ids_empty shared)

let test_contains_does_not_trust_bloom_hit () =
  Summary.reset ();
  let t = summary (rule ".a{color:red}") in
  let color = decl "color:red" in
  let calls = ref 0 in
  let never_same _ _ =
    incr calls;
    false
  in
  Alcotest.(check bool)
    "bloom hit still requires equality" false
    (Summary.contains ~same:never_same t color);
  Alcotest.(check int) "equality predicate was consulted" 1 !calls;
  Alcotest.(check bool)
    "structural equality confirms containment" true
    (Summary.contains ~same t color)

let test_cached_fields_and_bloom_for_reordered_decls () =
  Summary.reset ();
  let t = summary (rule ".a{color:red;width:1px}") in
  Alcotest.(check int) "rule size is cached from callback" 123 (Summary.size t);
  Alcotest.(check (list int))
    "declaration sizes stay in source order"
    (List.map decl_size (Summary.rule t).declarations)
    (Summary.decl_sizes t);
  Alcotest.(check int)
    "declaration pp size is cached sum"
    (List.fold_left ( + ) 0 (Summary.decl_sizes t))
    (Summary.decl_pp_size t);

  let reversed = summary (rule ".b{width:1px;color:red}") in
  Alcotest.(check bool)
    "same declaration set has same bloom" true
    (Summary.same_bloom t reversed);
  Alcotest.(check bool)
    "same declaration set may share declaration hashes" true
    (Summary.may_share_decl_hash t reversed);
  Alcotest.(check bool)
    "explicit bloom probe matches same declaration set" true
    (Summary.may_share_bloom t (Summary.bloom reversed))

let suite =
  ( "factor_summary",
    [
      Alcotest.test_case "rule identity and first declaration" `Quick
        test_rule_identity_and_first_decl;
      Alcotest.test_case "property id intersection" `Quick test_ids_intersection;
      Alcotest.test_case "duplicate maps keep first declaration" `Quick
        test_duplicate_property_maps_keep_first_decl_and_size;
      Alcotest.test_case "decl property ids preserve source order" `Quick
        test_decl_property_ids_preserve_duplicate_source_order;
      Alcotest.test_case "ids set operations with partial overlap" `Quick
        test_ids_set_operations_with_partial_overlap;
      Alcotest.test_case "contains verifies bloom hits" `Quick
        test_contains_does_not_trust_bloom_hit;
      Alcotest.test_case "cached fields and bloom for reordered decls" `Quick
        test_cached_fields_and_bloom_for_reordered_decls;
    ] )
