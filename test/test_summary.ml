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

let same a b =
  a == b
  || Declaration.hash a = Declaration.hash b
     && Declaration.equal_declaration a b

let custom_rule ?(selector = ".a") decls =
  rule (selector ^ "{" ^ String.concat ";" decls ^ "}")

let test_rule_identity_and_first_decl () =
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

let test_duplicate_property_maps_keep_first_decl_and_size () =
  let t = summary (rule ".a{color:red;width:1px;color:blue}") in
  let color = Summary.prop (decl "color:green") in
  Alcotest.(check (option string))
    "first declaration for duplicate property" (Some "color:red")
    (Option.map decl_string (Summary.decl_for_prop t color));
  Alcotest.(check (option int))
    "size map follows first duplicate declaration"
    (Some (String.length "color:red"))
    (Summary.decl_size_for_prop t color)

let test_contains_does_not_trust_bloom_hit () =
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
    (Summary.may_share_decl_hash t reversed)

let test_large_summary_with_many_duplicate_properties () =
  let unique = 240 in
  let decls =
    List.init (unique * 2) (fun i ->
        let prop = i mod unique in
        Fmt.str "--p%d:%d" prop i)
  in
  let t = summary (custom_rule decls) in
  Alcotest.(check int)
    "all declarations counted" (unique * 2) (Summary.decl_count t);
  let prop = Summary.prop (decl "--p37:37") in
  Alcotest.(check (option string))
    "first duplicate declaration retained" (Some "--p37:37")
    (Option.map decl_string (Summary.decl_for_prop t prop));
  Alcotest.(check bool)
    "declares sampled properties" true
    (Summary.declares_all t
       [ Summary.prop (decl "--p0:0"); Summary.prop (decl "--p239:239") ])

let test_fuzz_summary_declaration_invariants () =
  for seed = 0 to 79 do
    let rng = Random.State.make [| 0x52; seed |] in
    let decl_count = 1 + Random.State.int rng 90 in
    let prop_count = 1 + Random.State.int rng 24 in
    let first_by_prop = Hashtbl.create prop_count in
    let decls =
      List.init decl_count (fun i ->
          let prop = Random.State.int rng prop_count in
          let declaration = Fmt.str "--p%d:%d" prop i in
          if not (Hashtbl.mem first_by_prop prop) then
            Hashtbl.add first_by_prop prop declaration;
          declaration)
    in
    let t = summary (custom_rule ~selector:(Fmt.str ".fuzz%d" seed) decls) in
    Alcotest.(check int) "declaration count" decl_count (Summary.decl_count t);
    Hashtbl.iter
      (fun prop first ->
        let probe = Summary.prop (Fmt.kstr decl "--p%d:0" prop) in
        Alcotest.(check (option string))
          "first declaration for property" (Some first)
          (Option.map decl_string (Summary.decl_for_prop t probe));
        Alcotest.(check (option int))
          "first declaration size for property"
          (Some (String.length first))
          (Summary.decl_size_for_prop t probe);
        Alcotest.(check bool)
          "property declared" true
          (Summary.declares_all t [ probe ]))
      first_by_prop
  done

let suite =
  ( "summary",
    [
      Alcotest.test_case "rule identity and first declaration" `Quick
        test_rule_identity_and_first_decl;
      Alcotest.test_case "duplicate maps keep first declaration" `Quick
        test_duplicate_property_maps_keep_first_decl_and_size;
      Alcotest.test_case "contains verifies bloom hits" `Quick
        test_contains_does_not_trust_bloom_hit;
      Alcotest.test_case "cached fields and bloom for reordered decls" `Quick
        test_cached_fields_and_bloom_for_reordered_decls;
      Alcotest.test_case "large summary with duplicate properties" `Quick
        test_large_summary_with_many_duplicate_properties;
      Alcotest.test_case "fuzz summary declaration invariants" `Quick
        test_fuzz_summary_declaration_invariants;
    ] )
