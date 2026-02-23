(** CSS optimizer tests - cascade semantics for selector merging *)

open Tw.Css

let test_merge_consecutive_identical () =
  (* Positive: Should merge consecutive rules with identical declarations *)
  let input =
    [
      Stylesheet.Rule
        {
          selector = Selector.class_ "foo";
          declarations = [ Declaration.padding [ Px 10. ] ];
          nested = [];
          merge_key = None;
        };
      Stylesheet.Rule
        {
          selector = Selector.class_ "bar";
          declarations = [ Declaration.padding [ Px 10. ] ];
          nested = [];
          merge_key = None;
        };
    ]
  in
  let optimized = Optimize.stylesheet input in
  let output_str = Stylesheet.to_string ~minify:true optimized in
  (* Should create .foo,.bar{padding:10px} *)
  Alcotest.(check bool)
    "merges consecutive identical rules" true
    (String.contains output_str ',')

let test_no_merge_different_declarations () =
  (* Negative: Should NOT merge rules with different declarations *)
  let input =
    [
      Stylesheet.Rule
        {
          selector = Selector.class_ "foo";
          declarations = [ Declaration.padding [ Px 10. ] ];
          nested = [];
          merge_key = None;
        };
      Stylesheet.Rule
        {
          selector = Selector.class_ "bar";
          declarations = [ Declaration.padding [ Px 20. ] ];
          nested = [];
          merge_key = None;
        };
    ]
  in
  let optimized = Optimize.stylesheet input in
  let output_str = Stylesheet.to_string ~minify:true optimized in
  (* Should keep separate: .foo{padding:10px}.bar{padding:20px} *)
  let module Str = Re.Str in
  let has_foo = Str.string_match (Str.regexp ".*\\.foo{") output_str 0 in
  let has_bar = Str.string_match (Str.regexp ".*\\.bar{") output_str 0 in
  Alcotest.(check bool)
    "keeps rules with different declarations separate" true (has_foo && has_bar)

let test_no_merge_non_consecutive () =
  (* Negative: Should NOT merge non-consecutive rules (preserve cascade) *)
  let input =
    [
      Stylesheet.Rule
        {
          selector = Selector.class_ "foo";
          declarations = [ Declaration.margin [ Px 5. ] ];
          nested = [];
          merge_key = None;
        };
      Stylesheet.Rule
        {
          selector = Selector.class_ "baz";
          declarations = [ Declaration.padding [ Px 10. ] ];
          nested = [];
          merge_key = None;
        };
      Stylesheet.Rule
        {
          selector = Selector.class_ "bar";
          declarations = [ Declaration.margin [ Px 5. ] ];
          nested = [];
          merge_key = None;
        };
    ]
  in
  let optimized = Optimize.stylesheet input in
  let output_str = Stylesheet.to_string ~minify:true optimized in
  (* .foo and .bar have same declarations but aren't consecutive - should stay
     separate *)
  let module Str = Re.Str in
  let foo_separate = Str.string_match (Str.regexp ".*\\.foo{") output_str 0 in
  let bar_separate = Str.string_match (Str.regexp ".*\\.bar{") output_str 0 in
  Alcotest.(check bool)
    "keeps non-consecutive rules separate" true
    (foo_separate && bar_separate)

let test_no_merge_vendor_pseudo () =
  (* Negative: Should NOT merge vendor-specific pseudo-elements *)
  let input =
    [
      Stylesheet.Rule
        {
          selector = Selector.File_selector_button;
          declarations = [ Declaration.margin [ Px 4. ] ];
          nested = [];
          merge_key = None;
        };
      Stylesheet.Rule
        {
          selector = Selector.class_ "foo";
          declarations = [ Declaration.margin [ Px 4. ] ];
          nested = [];
          merge_key = None;
        };
    ]
  in
  let optimized = Optimize.stylesheet input in
  let output_str = Stylesheet.to_string ~minify:true optimized in
  (* Vendor pseudo-elements should not be merged *)
  let module Str = Re.Str in
  let has_file_selector =
    Str.string_match (Str.regexp ".*::file-selector-button{") output_str 0
  in
  let has_foo = Str.string_match (Str.regexp ".*\\.foo{") output_str 0 in
  Alcotest.(check bool)
    "doesn't merge vendor pseudo-elements" true
    (has_file_selector && has_foo)

let test_no_merge_with_nested () =
  (* Negative: Should NOT merge rules with nested statements *)
  let input =
    [
      Stylesheet.Rule
        {
          selector = Selector.class_ "foo";
          declarations = [ Declaration.padding [ Px 10. ] ];
          nested =
            [
              Stylesheet.Rule
                {
                  selector = Selector.Hover;
                  declarations = [ Declaration.padding [ Px 20. ] ];
                  nested = [];
                  merge_key = None;
                };
            ];
          merge_key = None;
        };
      Stylesheet.Rule
        {
          selector = Selector.class_ "bar";
          declarations = [ Declaration.padding [ Px 10. ] ];
          nested = [];
          merge_key = None;
        };
    ]
  in
  let optimized = Optimize.stylesheet input in
  let output_str = Stylesheet.to_string ~minify:true optimized in
  (* Rules with nested statements shouldn't be merged *)
  let module Str = Re.Str in
  let has_foo = Str.string_match (Str.regexp ".*\\.foo{") output_str 0 in
  let has_bar = Str.string_match (Str.regexp ".*\\.bar{") output_str 0 in
  Alcotest.(check bool)
    "doesn't merge rules with nested statements" true (has_foo && has_bar)

let () =
  let open Alcotest in
  run "CSS Optimizer"
    [
      ( "selector_merging",
        [
          test_case "merge consecutive identical" `Quick
            test_merge_consecutive_identical;
          test_case "no merge different declarations" `Quick
            test_no_merge_different_declarations;
          test_case "no merge non-consecutive" `Quick
            test_no_merge_non_consecutive;
          test_case "no merge vendor pseudo" `Quick test_no_merge_vendor_pseudo;
          test_case "no merge with nested" `Quick test_no_merge_with_nested;
        ] );
    ]
