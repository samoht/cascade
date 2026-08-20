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

let decl css = Declaration.of_string css

let same_decl a b =
  a == b
  || Declaration.hash a = Declaration.hash b
     && Declaration.equal_declaration a b

let safe =
  Factor_safe.v ~same_minified_declaration:same_decl
    ~declaration_covers:Declaration.same_property
    ~contains_vendor_pseudo_element:(fun _ -> false)
    ~rule_factor_boundary:(fun r -> r.merge_key <> None || r.nested <> [])
    ~decl_property:Declaration.property_key

let sum r =
  Factor_safe.summary r
    ~selectors:(lazy (Selector_summary.of_selector r.Stylesheet.selector))

let test_overlap () =
  Alcotest.(check bool)
    "same declaration is harmless" false
    (Factor_safe.overlap safe [ decl "color:red" ] [ decl "color:red" ]);
  Alcotest.(check bool)
    "same property with different value overlaps" true
    (Factor_safe.overlap safe [ decl "color:red" ] [ decl "color:blue" ]);
  Alcotest.(check bool)
    "importance partitions overlap" false
    (Factor_safe.overlap safe
       [ decl "color:red!important" ]
       [ decl "color:blue" ])

let test_blocks_factor () =
  let target = rule ".a{color:red}" in
  let skipped = rule ".a{color:blue}" in
  Alcotest.(check bool)
    "same selector blocks" true
    (Factor_safe.blocks_factor safe
       [ decl "color:red" ]
       (sum target) (sum skipped));
  let target = rule "#x{color:red}" in
  let skipped = rule ".a{color:blue}" in
  Alcotest.(check bool)
    "higher specificity target is safe" false
    (Factor_safe.blocks_factor safe
       [ decl "color:red" ]
       (sum target) (sum skipped))

let test_can_cross () =
  let common = Some [ decl "color:red" ] in
  let plain = rule ".a{color:blue}" in
  Alcotest.(check bool)
    "ordinary rule can be crossed" true
    (Factor_safe.can_cross safe common plain);
  let keyed = { plain with merge_key = Some "k" } in
  Alcotest.(check bool)
    "merge boundary cannot be crossed" false
    (Factor_safe.can_cross safe common keyed);
  let width_nested =
    { plain with nested = [ Stylesheet.Declarations [ decl "width:1px" ] ] }
  in
  Alcotest.(check bool)
    "unrelated nested declaration can be crossed" true
    (Factor_safe.can_cross safe common width_nested);
  let color_nested =
    { plain with nested = [ Stylesheet.Declarations [ decl "color:blue" ] ] }
  in
  Alcotest.(check bool)
    "touching nested declaration stops crossing" false
    (Factor_safe.can_cross safe common color_nested)

let suite =
  ( "factor_safe",
    [
      Alcotest.test_case "overlap" `Quick test_overlap;
      Alcotest.test_case "blocks factoring" `Quick test_blocks_factor;
      Alcotest.test_case "crosses boundaries" `Quick test_can_cross;
    ] )
