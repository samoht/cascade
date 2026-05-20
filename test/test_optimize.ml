(** Tests for CSS Optimize module *)

open Alcotest
open Cascade
open Css.Optimize
open Css.Declaration
open Css.Values
open Css.Properties

let hex_color s = Hex { hash = true; value = s }
let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Compose optimize + minified to_string the way [to_string ~minify:true] used
   to behave implicitly. *)
let minify s = s |> Css.optimize |> Css.to_string ~minify:true

let statement_of_rule (rule : Css.Stylesheet.rule) =
  Css.rule ~selector:rule.selector ~nested:rule.nested ?merge_key:rule.merge_key
    rule.declarations

let rule_of_statement stmt =
  match Css.as_rule stmt with
  | Some (selector, declarations, nested) ->
      ({ selector; declarations; nested; merge_key = None }
        : Css.Stylesheet.rule)
  | None -> failwith "Expected Rule"

(* Helper to check if a declaration is !important *)
let is_important = Css.Declaration.is_important

(* Helper to extract color hex value from declaration string like "color:red" *)
let color_value_of_decl decl =
  let s = Css.Declaration.string_of_declaration ~minify:true decl in
  (* Extract just the hex value after the colon and before any !important *)
  let after_colon =
    String.split_on_char ':' s |> List.tl |> String.concat ":"
  in
  let before_important = String.split_on_char '!' after_colon |> List.hd in
  String.trim before_important

(* Generic check function for optimize types *)
let _check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Reader.of_string input in
  let v = reader t in
  let s = to_string pp v in
  check string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Reader.of_string s in
  let v2 = reader t2 in
  let s2 = to_string pp v2 in
  check string (Fmt.str "roundtrip %s %s" name input) s s2

(** Test declaration deduplication *)
let test_deduplicate_declarations () =
  (* Test case: later declaration wins *)
  let decls = [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ] in
  let deduped = deduplicate_declarations decls in
  check int "single color property remains" 1 (List.length deduped);

  (* Test case: !important wins over normal *)
  let decls_important =
    [
      v Color (hex_color "ff0000");
      v ~important:true Color (hex_color "00ff00");
      v Color (hex_color "0000ff");
    ]
  in
  let deduped_important = deduplicate_declarations decls_important in
  check int "single color property remains" 1 (List.length deduped_important);
  let result = List.hd deduped_important in
  check bool "!important wins" true (is_important result);
  check string "green color wins" "#0f0" (color_value_of_decl result);

  (* Test case: last !important wins when multiple !important *)
  let decls_multi_important =
    [
      v ~important:true Color (hex_color "ff0000");
      v Color (hex_color "00ff00");
      v ~important:true Color (hex_color "0000ff");
    ]
  in
  let deduped_multi = deduplicate_declarations decls_multi_important in
  check int "single color remains" 1 (List.length deduped_multi);
  let result = List.hd deduped_multi in
  check bool "last !important wins" true (is_important result);
  check string "blue color wins" "#00f" (color_value_of_decl result);

  (* Test case: normal after !important doesn't override *)
  let decls_normal_after =
    [
      v ~important:true Color (hex_color "ff0000");
      v Color (hex_color "00ff00");
      v Color (hex_color "0000ff");
    ]
  in
  let deduped_normal_after = deduplicate_declarations decls_normal_after in
  check int "single color remains" 1 (List.length deduped_normal_after);
  let result = List.hd deduped_normal_after in
  check bool "!important not overridden by normal" true (is_important result);
  check string "red !important wins" "red" (color_value_of_decl result);

  (* Test case: custom properties *)
  let custom_decls =
    [
      custom_property "--color1" "red";
      custom_property "--color1" "blue";
      custom_property "--color2" "green";
    ]
  in
  let deduped_custom = deduplicate_declarations custom_decls in
  check int "two custom properties remain" 2 (List.length deduped_custom)

(** Test buggy property duplication *)
let test_duplicate_buggy_properties () =
  (* Test -webkit-text-decoration:inherit compatibility. Note: Transform is NOT
     duplicated in Tailwind v4 - they don't emit vendor-prefixed transform. *)
  let decls = [ v Webkit_text_decoration Inherit ] in
  let duplicated = duplicate_buggy_properties decls in
  check (list string) "keeps one webkit-text-decoration inherit fallback"
    [ "-webkit-text-decoration:inherit" ]
    (List.map (Css.Declaration.string_of_declaration ~minify:true) duplicated);

  (* -webkit-text-decoration-color is a compatibility property, not a generated
     fallback for the standard property. External minifiers preserve authored
     prefixed/unprefixed pairs and do not synthesize either spelling. *)
  let pp_decls decls =
    List.map (Css.Declaration.string_of_declaration ~minify:true) decls
  in
  let standard_color =
    duplicate_buggy_properties [ v Text_decoration_color (hex_color "0000ff") ]
  in
  check (list string) "standard decoration color is not prefixed"
    [ "text-decoration-color:#00f" ]
    (pp_decls standard_color);
  let prefixed_color =
    duplicate_buggy_properties
      [ v Webkit_text_decoration_color (hex_color "0000ff") ]
  in
  check (list string) "authored webkit decoration color is preserved"
    [ "-webkit-text-decoration-color:#00f" ]
    (pp_decls prefixed_color);
  let pair =
    deduplicate_declarations
      [
        v Webkit_text_decoration_color (hex_color "ff0000");
        v Text_decoration_color (hex_color "0000ff");
      ]
  in
  check (list string) "prefixed and standard decoration colors both remain"
    [ "-webkit-text-decoration-color:red"; "text-decoration-color:#00f" ]
    (pp_decls pair)

(** Test rule optimization *)
let single_rule () =
  let selector = Css.Selector.class_ "test" in
  let decls =
    [
      v Color (hex_color "ff0000");
      v Color (hex_color "0000ff");
      v Background_color (hex_color "ffffff");
    ]
  in
  let rule : Css.Stylesheet.rule =
    { selector; declarations = decls; nested = []; merge_key = None }
  in
  let optimized = Css.Optimize.single_rule rule in

  (* Check that duplicate color declarations are removed *)
  let color_count =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Declaration { property = Color; _ } -> acc + 1
        | _ -> acc)
      0 optimized.declarations
  in
  check int "only one color declaration remains" 1 color_count

(** Test rule merging *)
let test_merge_rules () =
  let selector = Css.Selector.class_ "test" in
  let rule1 : Css.Stylesheet.rule =
    {
      selector;
      declarations = [ v Color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector;
      declarations = [ v Background_color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let merged = merge_rules [ rule1; rule2 ] in
  check int "rules with same selector are merged" 1 (List.length merged);

  let merged_rule = List.hd merged in
  check int "merged rule has both declarations" 2
    (List.length merged_rule.declarations)

(** Test selector grouping *)
let test_group_selectors () =
  let decls = [ v Color (hex_color "ff0000") ] in
  let rule1 : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "a";
      declarations = decls;
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "b";
      declarations = decls;
      nested = [];
      merge_key = None;
    }
  in
  let rule3 : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "c";
      declarations = decls;
      nested = [];
      merge_key = None;
    }
  in

  (* Test combine_identical_rules function *)
  let grouped = combine_identical_rules [ rule1; rule2; rule3 ] in
  check int "rules with same declarations are grouped" 1 (List.length grouped);

  (* Check that selector is a list *)
  let grouped_rule = List.hd grouped in
  check bool "grouped selector is a list" true
    (Css.Selector.is_compound_list grouped_rule.selector)

(** Test selector grouping with complex prose-like selectors *)
let test_group_complex_selectors () =
  (* Mimics prose selectors: .prose :where(a strong), .prose :where(blockquote
     strong) *)
  let decls = [ v Color Inherit ] in

  (* Parse complex selectors from strings to match real-world prose selectors *)
  let sel1_str =
    ".prose :where(a strong):not(:where([class~=not-prose], [class~=not-prose] \
     *))"
  in
  let sel2_str =
    ".prose :where(blockquote strong):not(:where([class~=not-prose], \
     [class~=not-prose] *))"
  in
  let sel3_str =
    ".prose :where(thead th strong):not(:where([class~=not-prose], \
     [class~=not-prose] *))"
  in

  let sel1 = Css.Selector.read (Cursor.of_string sel1_str) in
  let sel2 = Css.Selector.read (Cursor.of_string sel2_str) in
  let sel3 = Css.Selector.read (Cursor.of_string sel3_str) in

  let rule1 : Css.Stylesheet.rule =
    { selector = sel1; declarations = decls; nested = []; merge_key = None }
  in
  let rule2 : Css.Stylesheet.rule =
    { selector = sel2; declarations = decls; nested = []; merge_key = None }
  in
  let rule3 : Css.Stylesheet.rule =
    { selector = sel3; declarations = decls; nested = []; merge_key = None }
  in

  (* Complex selectors with descendant combinators can still be grouped when
     their declarations and cascade context are identical. Tailwind source shape
     compatibility is not a reason to block shortest-safe grouping. *)
  let grouped = combine_identical_rules [ rule1; rule2; rule3 ] in
  check int "complex descendant selectors are grouped" 1 (List.length grouped)

(** Test complete stylesheet optimization *)
let count_rules stmts =
  List.fold_left
    (fun acc stmt ->
      match Css.as_rule stmt with Some _ -> acc + 1 | None -> acc)
    0 stmts

let optimize_all () =
  let selector1 = Css.Selector.class_ "test" in
  let selector2 = Css.Selector.class_ "other" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations =
        [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Background_color (hex_color "ffffff") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule3 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let stylesheet =
    [
      statement_of_rule rule1; statement_of_rule rule2; statement_of_rule rule3;
    ]
  in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Should merge rule1 and rule2 since they have same selector *)
  check bool "optimization reduces rule count" true
    (count_rules optimized < count_rules stylesheet)

(** Test media query optimization *)
let media_queries () =
  let selector = Css.Selector.class_ "test" in
  let rule : Css.Stylesheet.rule =
    {
      selector;
      declarations =
        [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let media_stmt =
    Css.media
      ~condition:(Css.Media.of_string "screen")
      [ statement_of_rule rule ]
  in

  let stylesheet = [ media_stmt ] in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Check that declarations within media queries are also deduplicated *)
  let optimized_rule =
    match List.hd optimized with
    | Css.Stylesheet.Media (_, [ stmt ]) -> rule_of_statement stmt
    | _ -> failwith "Expected Media with Rule"
  in
  let color_count =
    List.fold_left
      (fun acc decl ->
        match decl with
        | Declaration { property = Color; _ } -> acc + 1
        | _ -> acc)
      0 optimized_rule.declarations
  in
  check int "media rule declarations are deduplicated" 1 color_count

(** Test layer optimization *)
let layers () =
  let selector = Css.Selector.class_ "test" in
  let rule : Css.Stylesheet.rule =
    {
      selector;
      declarations =
        [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let layer_stmt =
    Css.Stylesheet.Layer (Some "utilities", [ statement_of_rule rule ])
  in

  let stylesheet = [ layer_stmt ] in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Check that layer rules are optimized *)
  match List.hd optimized with
  | Css.Stylesheet.Layer (_, [ stmt ]) ->
      let optimized_rule = rule_of_statement stmt in
      let color_count =
        List.fold_left
          (fun acc decl ->
            match decl with
            | Declaration { property = Color; _ } -> acc + 1
            | _ -> acc)
          0 optimized_rule.declarations
      in
      check int "layer rule declarations are deduplicated" 1 color_count
  | _ -> fail "Expected Rule in layer"

(** Test consecutive media query merging *)
let test_consecutive_media_merge () =
  (* Two consecutive media queries with same condition should merge *)
  let selector1 = Css.Selector.class_ "a" in
  let selector2 = Css.Selector.class_ "b" in
  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let stylesheet =
    [
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule2 ];
    ]
  in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Should merge into single media query *)
  check int "consecutive media queries are merged" 1 (List.length optimized);

  (* Verify both rules are in the merged media query *)
  match List.hd optimized with
  | Css.Stylesheet.Media (_, rules) ->
      check int "merged media contains both rules" 2 (List.length rules)
  | _ -> fail "Expected Media statement"

(** Test non-consecutive media queries are NOT merged *)
let test_nonconsecutive_media_unmerged () =
  (* Media queries separated by other statements should NOT merge *)
  let selector1 = Css.Selector.class_ "a" in
  let selector2 = Css.Selector.class_ "a" in
  let selector3 = Css.Selector.class_ "c" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "00ff00") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule3 : Css.Stylesheet.rule =
    {
      selector = selector3;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let stylesheet =
    [
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule1 ];
      statement_of_rule rule2;
      (* Separator *)
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule3 ];
    ]
  in

  let optimized = Css.Optimize.stylesheet stylesheet in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in

  Alcotest.(check string)
    "non-consecutive media queries preserve source order"
    "@media(width>=48px){.a{color:red}}.a{color:#0f0}@media(width>=48px){.c{color:#00f}}"
    output

(** Test media queries with different conditions are NOT merged *)
let test_different_conditions_unmerged () =
  let selector1 = Css.Selector.class_ "a" in
  let selector2 = Css.Selector.class_ "b" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let stylesheet =
    [
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(Css.Media.Min_width 64.) [ statement_of_rule rule2 ];
    ]
  in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Should have 2 separate media queries *)
  check int "different media conditions stay separate" 2 (List.length optimized)

(** Test multiple consecutive media queries merge together *)
let test_multiple_consecutive_media_merge () =
  let selector1 = Css.Selector.class_ "a" in
  let selector2 = Css.Selector.class_ "b" in
  let selector3 = Css.Selector.class_ "c" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "00ff00") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule3 : Css.Stylesheet.rule =
    {
      selector = selector3;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let stylesheet =
    [
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule2 ];
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule3 ];
    ]
  in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Should merge all three into single media query *)
  check int "three consecutive media queries merge into one" 1
    (List.length optimized);

  (* Verify all three rules are in the merged media query *)
  match List.hd optimized with
  | Css.Stylesheet.Media (_, rules) ->
      check int "merged media contains all three rules" 3 (List.length rules)
  | _ -> fail "Expected Media statement"

(** Test media query merging inside layers *)
let test_media_merge_in_layers () =
  (* Media queries inside layers should also be merged *)
  let selector1 = Css.Selector.class_ "a" in
  let selector2 = Css.Selector.class_ "b" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
      merge_key = None;
    }
  in

  let layer_content =
    [
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(Css.Media.Min_width 48.) [ statement_of_rule rule2 ];
    ]
  in

  let stylesheet = [ Css.Stylesheet.Layer (Some "utilities", layer_content) ] in

  let optimized = Css.Optimize.stylesheet stylesheet in

  (* Check that media queries inside layer are merged *)
  match List.hd optimized with
  | Css.Stylesheet.Layer (_, layer_stmts) -> (
      check int "media queries inside layer are merged" 1
        (List.length layer_stmts);
      match List.hd layer_stmts with
      | Css.Stylesheet.Media (_, rules) ->
          check int "merged media in layer contains both rules" 2
            (List.length rules)
      | _ -> fail "Expected Media inside layer")
  | _ -> fail "Expected Layer statement"

let test_empty_layers_statement () =
  (* Positive optimization case: empty named @layer blocks establish order but
     contain no declarations, so consecutive empty named blocks can be
     represented by the statement form from CSS Cascade 5. *)
  let stylesheet =
    [
      Css.Stylesheet.Layer (Some "reset", []);
      Css.Stylesheet.Layer (Some "theme", []);
      Css.Stylesheet.Layer
        ( Some "components",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "card")
              [ Css.Declaration.display Flex ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet stylesheet in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "empty named layer blocks canonicalize to layer statement"
    "@layer reset,theme;@layer components{.card{display:flex}}" output

let test_tw_empty_layers_statement () =
  (* Tailwind's layered output commonly leaves empty components/utilities layer
     markers. CSS Cascade allows a statement form for named empty layers, and
     the shortest faithful spelling combines adjacent declarations. *)
  let input =
    [
      Css.Stylesheet.Layer (Some "components", []);
      Css.Stylesheet.Layer (Some "utilities", []);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "empty components/utilities collapse to one layer statement"
    "@layer components,utilities;" output

let test_tw_conditionals_layer () =
  (* Tailwind emits utility rules inside @layer utilities. Cascade owns the
     generic CSS optimization: adjacent identical conditions merge inside that
     layer, but the utility layer remains the boundary. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string
         "@layer utilities{@supports \
          (display:grid){.grid{display:grid}}@supports \
          (display:grid){.gap{gap:1rem}}@container (inline-size > \
          30em){.wide{display:block}}@container (inline-size > \
          30em){.pad{padding:1rem}}}")
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "default minify elides baseline supports inside utility layer"
    "@layer \
     utilities{.grid{display:grid}.gap{gap:1rem}@container(inline-size>30em){.wide{display:block}.pad{padding:1rem}}}"
    output;
  let spec = Css.Optimize.stylesheet ~enforce_spec:true input in
  let spec_output = Css.Stylesheet.to_string ~minify:true spec |> String.trim in
  Alcotest.(check string)
    "enforce-spec keeps adjacent supports merge inside utility layer"
    "@layer \
     utilities{@supports(display:grid){.grid{display:grid}.gap{gap:1rem}}@container(inline-size>30em){.wide{display:block}.pad{padding:1rem}}}"
    spec_output

let test_tw_conditionals_split () =
  (* The optimizer must not collect same-condition blocks across an intervening
     utility rule: Tailwind's sort order can intentionally interleave base and
     variant rules to preserve source-order ties. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string
         "@layer utilities{@media \
          (min-width:48rem){.md\\:flex{display:flex}}.flex{display:flex}@media \
          (min-width:48rem){.md\\:grid{display:grid}}@supports \
          (display:grid){.grid{display:grid}}.block{display:block}@supports \
          (display:grid){.gap{gap:1rem}}}")
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "default minify elides non-adjacent baseline supports"
    "@layer \
     utilities{@media(width>=48rem){.md\\:flex{display:flex}}.flex{display:flex}@media(width>=48rem){.md\\:grid{display:grid}}.grid{display:grid}.block{display:block}.gap{gap:1rem}}"
    output;
  let spec = Css.Optimize.stylesheet ~enforce_spec:true input in
  let spec_output = Css.Stylesheet.to_string ~minify:true spec |> String.trim in
  Alcotest.(check string)
    "enforce-spec keeps non-adjacent supports split inside utility layer"
    "@layer \
     utilities{@media(min-width:48rem){.md\\:flex{display:flex}}.flex{display:flex}@media(min-width:48rem){.md\\:grid{display:grid}}@supports(display:grid){.grid{display:grid}}.block{display:block}@supports(display:grid){.gap{gap:1rem}}}"
    spec_output

let optimize_tests =
  [
    ("deduplicate declarations", `Quick, test_deduplicate_declarations);
    ("duplicate buggy properties", `Quick, test_duplicate_buggy_properties);
    ("optimize single rule", `Quick, single_rule);
    ("merge rules", `Quick, test_merge_rules);
    ("group selectors", `Quick, test_group_selectors);
    ("group complex selectors", `Quick, test_group_complex_selectors);
    ("optimize stylesheet", `Quick, optimize_all);
    ("optimize media queries", `Quick, media_queries);
    ("optimize layers", `Quick, layers);
    ("merge consecutive media queries", `Quick, test_consecutive_media_merge);
    ( "preserve non-consecutive media queries",
      `Quick,
      test_nonconsecutive_media_unmerged );
    ( "different media conditions not merged",
      `Quick,
      test_different_conditions_unmerged );
    ( "multiple consecutive media merge",
      `Quick,
      test_multiple_consecutive_media_merge );
    ("media merge in layers", `Quick, test_media_merge_in_layers);
    ( "positive empty named layers to statement",
      `Quick,
      test_empty_layers_statement );
    ( "tailwind empty components/utilities layers to statement",
      `Quick,
      test_tw_empty_layers_statement );
    ( "tailwind conditionals merge inside layer",
      `Quick,
      test_tw_conditionals_layer );
    ( "tailwind non-adjacent conditionals in layer stay split",
      `Quick,
      test_tw_conditionals_split );
  ]

(** {1 Selector merging tests (cascade semantics)} *)

let optimized_string ?(enforce_spec = false) css =
  css |> Cursor.of_string |> Css.Stylesheet.read
  |> Css.Optimize.stylesheet ~enforce_spec
  |> Css.Stylesheet.to_string ~minify:true
  |> String.trim

let test_merge_consecutive_identical () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.padding [ Px 10. ] ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  Alcotest.(check bool)
    "merges consecutive identical rules" true
    (String.contains output_str ',')

let test_no_merge_different_declarations () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.padding [ Px 20. ] ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  let has_foo = Astring.String.is_infix ~affix:".foo{" output_str in
  let has_bar = Astring.String.is_infix ~affix:".bar{" output_str in
  Alcotest.(check bool)
    "keeps rules with different declarations separate" true (has_foo && has_bar)

let test_merge_non_consecutive_non_conflicting () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.margin [ Px 5. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "baz")
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.margin [ Px 5. ] ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  Alcotest.(check string)
    "merges non-consecutive non-conflicting rules"
    ".bar,.foo{margin:5px}.baz{padding:10px}" output_str

let test_no_merge_vendor_pseudo () =
  let input =
    [
      Css.rule ~selector:Css.Selector.File_selector_button
        [ Css.Declaration.margin [ Px 4. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        [ Css.Declaration.margin [ Px 4. ] ];
    ]
  in
  let optimized = stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  let has_file_selector =
    Astring.String.is_infix ~affix:"::file-selector-button{" output_str
  in
  let has_foo = Astring.String.is_infix ~affix:".foo{" output_str in
  Alcotest.(check bool)
    "doesn't merge vendor pseudo-elements" true
    (has_file_selector && has_foo)

let test_no_merge_with_nested () =
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "foo")
        ~nested:
          [
            Css.rule ~selector:Css.Selector.Hover
              [ Css.Declaration.padding [ Px 20. ] ];
          ]
        [ Css.Declaration.padding [ Px 10. ] ];
      Css.rule
        ~selector:(Css.Selector.class_ "bar")
        [ Css.Declaration.padding [ Px 10. ] ];
    ]
  in
  let optimized = stylesheet input in
  let output_str = Css.Stylesheet.to_string ~minify:true optimized in
  let has_foo = Astring.String.is_infix ~affix:".foo{" output_str in
  let has_bar = Astring.String.is_infix ~affix:".bar{" output_str in
  Alcotest.(check bool)
    "doesn't merge rules with nested statements" true (has_foo && has_bar)

let c3_shorthand_resets () =
  (* CSS Cascade section 3: a shorthand declaration sets all longhands,
     including omitted sub-properties. A previous longhand covered by a later
     shorthand is therefore dead even when the shorthand omits that
     component. *)
  let margin_rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin_left (Px 1.); Css.Declaration.margin [ Px 2. ];
        ];
      nested = [];
      merge_key = None;
    }
  in
  let margin_optimized = Css.Optimize.single_rule margin_rule in
  let margin_output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule margin_optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later margin shorthand resets previous margin-left" ".box{margin:2px}"
    margin_output;

  let background_rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "hero";
      declarations =
        [
          Css.Declaration.background_image (Css.Properties.url "hero.png");
          Css.Declaration.background
            (Css.Properties.background_shorthand ~color:(hex_color "008000") ());
        ];
      nested = [];
      merge_key = None;
    }
  in
  let background_optimized = Css.Optimize.single_rule background_rule in
  let background_output =
    Css.Stylesheet.to_string ~minify:true
      [ statement_of_rule background_optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "background shorthand resets previous background-image"
    ".hero{background:green}" background_output

let c3_open_closed_world_background_synthesis () =
  (* CSS Backgrounds shorthands are resetful: synthesizing [background] resets
     omitted background longhands. In the default open world, a fragment cannot
     assume no earlier author CSS wrote one of those omitted longhands. In a
     closed world, the caller asserts the whole relevant author stylesheet graph
     is available, so the shorter resetful shorthand is allowed. *)
  let optimize ?scope css =
    Css.of_string_exn ~strict:false css
    |> Css.optimize ?scope |> Css.to_string ~minify:true |> String.trim
  in
  let partial_run =
    {|
      .card {
        background-color: red;
        background-image: none;
        background-repeat: repeat;
        background-position: 0% 0%;
        background-attachment: scroll;
      }
    |}
  in
  Alcotest.(check string)
    "open-world partial background run keeps longhands"
    ".card{background-color:red;background-image:none;background-repeat:repeat;background-position:0;background-attachment:scroll}"
    (optimize partial_run);
  Alcotest.(check string)
    "closed-world partial background run may synthesize shorthand"
    ".card{background:red}"
    (optimize ~scope:`Stylesheet partial_run);
  let reset_closed_run =
    {|
      .card {
        background-color: red;
        background-image: none;
        background-repeat: repeat;
        background-position: 0% 0%;
        background-size: auto;
        background-attachment: scroll;
        background-origin: padding-box;
        background-clip: border-box;
      }
    |}
  in
  Alcotest.(check string)
    "open-world reset-closed background run may synthesize shorthand"
    ".card{background:red}"
    (optimize reset_closed_run)

let c3_shorthand_order_edges () =
  (* CSS Cascade section 3 plus source order: a later shorthand resets all
     covered longhands, including a longhand that occurred between two shorthand
     declarations. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin [ Px 1. ];
          Css.Declaration.margin_left (Px 2.);
          Css.Declaration.margin [ Px 3. ];
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later shorthand resets intervening longhand" ".box{margin:3px}" output

let c3_important_shorthand_expands () =
  (* CSS Cascade section 3: declaring a shorthand !important is equivalent to
     declaring all of its longhand sub-properties !important. A later normal
     longhand covered by the shorthand cannot override any sub-property. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "hero";
      declarations =
        [
          Css.Declaration.background_image (Css.Properties.url "before.png");
          Css.Declaration.important
            (Css.Declaration.background
               (Css.Properties.background_shorthand ~color:(hex_color "008000")
                  ()));
          Css.Declaration.background_image (Css.Properties.url "after.png");
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "important background shorthand blocks later normal background-image"
    ".hero{background:green!important}" output

let c61_decl_order_shorthand_boundary () =
  (* CSS Cascade section 6.1: order of appearance is a cascade criterion.
     Removing an earlier duplicate must not move the surviving longhand before
     an intervening shorthand, because shorthands reset their longhands. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin_left (Px 1.);
          Css.Declaration.margin [ Px 2. ];
          Css.Declaration.margin_left (Px 3.);
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later longhand stays after shorthand" ".box{margin:2px 2px 2px 3px}" output

let c61_adjacent_shorthand_order () =
  (* CSS Cascade sections 3 and 6.1: merging adjacent equal-selector rules is
     only semantics-preserving when the declaration sequence stays in source
     order, because a shorthand in the later rule resets earlier longhands. The
     first rule also contains a non-shadowed declaration so this remains a
     cross-rule merge test even when dead earlier declarations are
     eliminated. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.color (hex_color "ff0000");
          Css.Declaration.margin_left (Px 1.);
        ];
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.margin [ Px 2. ]; Css.Declaration.margin_left (Px 3.);
        ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "adjacent same-selector merge keeps shorthand/longhand source order"
    ".box{color:red;margin:2px 2px 2px 3px}" output

let c61_adjacent_later_dedup () =
  (* Positive merge case: adjacent same-selector rules in the same cascade slot
     may merge, and ordinary duplicate declarations inside the merged rule still
     reduce by source order. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.display Flex;
          Css.Declaration.color (hex_color "0000ff");
        ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "adjacent same-selector rules merge and dedupe by source order"
    ".box{display:flex;color:#00f}" output

let c61_no_merge_intervening () =
  (* CSS Cascade section 6.1: if rules tie on origin, importance, layer,
     specificity, and scope proximity, the later declaration wins. Merging equal
     selectors across an intervening rule would move the first rule's
     declaration after that intervening rule. *)
  let input =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule ~selector:(Css.Selector.class_ "b")
        [ Css.Declaration.color (hex_color "00ff00") ];
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.background_color (hex_color "0000ff") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across an intervening rule"
    ".a{color:red}.b{color:#0f0}.a{background-color:#00f}" output

let c61_no_group_nonadjacent () =
  (* CSS Cascade section 6.1: selector grouping changes where a rule appears in
     source order. Non-adjacent equal declaration blocks must not be grouped
     across another same-specificity rule, because elements matching both the
     middle selector and the later selector would observe a different winner. *)
  let input =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule ~selector:(Css.Selector.class_ "b")
        [ Css.Declaration.color (hex_color "0000ff") ];
      Css.rule ~selector:(Css.Selector.class_ "c")
        [ Css.Declaration.color (hex_color "ff0000") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same declarations are not grouped across source-order competitor"
    ".a{color:red}.b{color:#00f}.c{color:red}" output

let aba_forbidden_intersection_dependency () =
  (* A?B?A / CSS-graph soundness: two equal A declarations cannot be grouped
     across an intervening B declaration when B writes the same property and the
     selector intersection is satisfiable. Here an element can match all three
     selectors, so source order decides whether red or blue wins. *)
  Alcotest.(check string)
    "overlapping selectors keep intervening same-property dependency"
    ".a.x{color:red}.b.x{color:#00f}.a.y{color:red}"
    (optimized_string ".a.x{color:red}.b.x{color:blue}.a.y{color:red}")

let aba_allowed_same_selector_dead () =
  (* Exact same selector and same property is not an A?B?A dependency: the final
     A shadows the first A for every matched element. The first declaration may
     be removed even with an unrelated intervening rule. *)
  Alcotest.(check string)
    "same selector dead A may be removed across intervening rule"
    ".b{color:#00f}.a{color:#000}"
    (optimized_string ".a{color:red}.b{color:blue}.a{color:black}")

let aba_allowed_local_refactoring () =
  (* Punt, Visscher, Zaytsev, "The A-B*-A Pattern: Undoing Style in CSS" (ICSME
     2016), section IV defines undoing style as assigning A, then one or more B
     values, then A again. With one selector and one property, the final A
     dominates locally. *)
  Alcotest.(check string)
    "same selector A-B-A collapses to final value" ".x{color:red}"
    (optimized_string ".x{color:red}.x{color:blue}.x{color:red}")

let aba_runtime_shorthand_boundaries () =
  (* Shorthand reasoning is not a pure syntactic rewrite when the shorthand
     value is provided by a runtime substitution. Keep the explicit longhand
     next to var()/env()/attr() shorthands instead of contracting or deleting it
     during optimization. *)
  Alcotest.(check string)
    "var shorthand keeps following longhand"
    ".box{margin:var(--m);margin-left:1px}"
    (optimized_string ".box{margin:var(--m);margin-left:1px}");
  Alcotest.(check string)
    "env shorthand keeps following longhand"
    ".box{margin:env(safe-area-inset-left);margin-left:1px}"
    (optimized_string ".box{margin:env(safe-area-inset-left);margin-left:1px}");
  Alcotest.(check string)
    "attr shorthand keeps following longhand"
    ".box{margin:attr(data-m px);margin-left:1px}"
    (optimized_string ".box{margin:attr(data-m px);margin-left:1px}")

let c61_no_merge_atrule () =
  (* CSS Cascade section 6.1 defines style sheets and imported/nested sheets in
     document order. An at-rule boundary is not a free reordering point for
     surrounding rules, even when the surrounding selectors match. *)
  let input =
    [
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.media ~condition:(Css.Media.Min_width 48.)
        [
          Css.rule ~selector:(Css.Selector.class_ "m")
            [ Css.Declaration.color (hex_color "00ff00") ];
        ];
      Css.rule ~selector:(Css.Selector.class_ "a")
        [ Css.Declaration.background_color (hex_color "0000ff") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across media boundary"
    ".a{color:red}@media(width>=48px){.m{color:#0f0}}.a{background-color:#00f}"
    output

let c61_conditional_competitor_order () =
  (* CSS Cascade section 6.1: after conditional rules are filtered, declarations
     with the same origin, importance, layer, specificity, and scope proximity
     are resolved by order of appearance. These max-width conditions overlap,
     and both write the same property for the same selector, so sorting them by
     media condition would change the winning declaration. *)
  Alcotest.(check string)
    "overlapping media competitors preserve authored order"
    "@media not all and (width>=1024px){.u{display:flex}}@media not all and \
     (width>=640px){.u{display:grid}}"
    (optimized_string
       "@media not all and (min-width:1024px){.u{display:flex}}@media not all \
        and (min-width:640px){.u{display:grid}}");
  Alcotest.(check string)
    "reverse authored order is also preserved"
    "@media not all and (width>=640px){.u{display:grid}}@media not all and \
     (width>=1024px){.u{display:flex}}"
    (optimized_string
       "@media not all and (min-width:640px){.u{display:grid}}@media not all \
        and (min-width:1024px){.u{display:flex}}")

let target_minify_enforce_spec_split () =
  let check_modes name input ~default ~spec =
    Alcotest.(check string) (name ^ " default") default (optimized_string input);
    Alcotest.(check string)
      (name ^ " enforce-spec") spec
      (optimized_string ~enforce_spec:true input)
  in
  check_modes "baseline supports grid"
    "@supports (display: grid) { a { display: grid } }"
    ~default:"a{display:grid}" ~spec:"@supports(display:grid){a{display:grid}}";
  check_modes "baseline supports flex with several rules"
    "@supports (display: flex) { a { display: flex } b { gap: 1rem } }"
    ~default:"a{display:flex}b{gap:1rem}"
    ~spec:"@supports(display:flex){a{display:flex}b{gap:1rem}}";
  check_modes "negated baseline supports"
    "@supports not (display: grid) { a { color: red } } b { color: blue }"
    ~default:"b{color:#00f}"
    ~spec:"@supports not (display:grid){a{color:red}}b{color:#00f}";
  check_modes "unknown supports preserved"
    "@supports (future-layout: masonry-plus) { a { color: red } }"
    ~default:"@supports(future-layout:masonry-plus){a{color:red}}"
    ~spec:"@supports(future-layout:masonry-plus){a{color:red}}";
  check_modes "known supports conjunction"
    "@supports (display: grid) and (display: flex) { a { display: grid } }"
    ~default:"a{display:grid}"
    ~spec:"@supports(display:grid)and (display:flex){a{display:grid}}";
  check_modes "known supports disjunction"
    "@supports (display: grid) or (future-layout: masonry-plus) { a { display: \
     grid } }"
    ~default:"a{display:grid}"
    ~spec:
      "@supports(display:grid)or (future-layout:masonry-plus){a{display:grid}}";
  check_modes "known and unknown supports conjunction"
    "@supports (display: grid) and (future-layout: masonry-plus) { a { \
     display: grid } }"
    ~default:"@supports(future-layout:masonry-plus){a{display:grid}}"
    ~spec:
      "@supports(display:grid)and (future-layout:masonry-plus){a{display:grid}}";
  check_modes "unknown negated supports preserved"
    "@supports not (future-layout: masonry-plus) { a { color: red } }"
    ~default:"@supports not (future-layout:masonry-plus){a{color:red}}"
    ~spec:"@supports not (future-layout:masonry-plus){a{color:red}}";
  check_modes "supports nested in media"
    "@media (min-width: 40em) { @supports (display: grid) { a { display: grid \
     } } }"
    ~default:"@media(width>=40em){a{display:grid}}"
    ~spec:"@media(min-width:40em){@supports(display:grid){a{display:grid}}}";
  check_modes "media min-width grammar"
    "@media (min-width: 700px) { a { color: red } }"
    ~default:"@media(width>=700px){a{color:red}}"
    ~spec:"@media(min-width:700px){a{color:red}}";
  check_modes "media not all min-width grammar"
    "@media not all and (min-width: 700px) { a { color: red } }"
    ~default:"@media not all and (width>=700px){a{color:red}}"
    ~spec:"@media not all and (min-width:700px){a{color:red}}";
  check_modes "media interval grammar"
    "@media (min-width: 768px) and (max-width: 1024px) { a { color: red } }"
    ~default:"@media(768px<=width<=1024px){a{color:red}}"
    ~spec:"@media(min-width:768px)and (max-width:1024px){a{color:red}}";
  check_modes "container min-width grammar"
    "@container sidebar (min-width: 700px) { a { color: red } }"
    ~default:"@container sidebar (width>=700px){a{color:red}}"
    ~spec:"@container sidebar (min-width:700px){a{color:red}}";
  check_modes "import supports baseline"
    "@import url(\"grid.css\") supports(display: grid);"
    ~default:"@import\"grid.css\";"
    ~spec:"@import\"grid.css\"supports(display:grid);";
  check_modes "import layer supports baseline"
    "@import url(\"theme.css\") layer(theme) supports(display: flex);"
    ~default:"@import\"theme.css\"layer(theme);"
    ~spec:"@import\"theme.css\"layer(theme)supports(display:flex);"

let c61_no_layer_media_merge () =
  (* CSS Cascade section 6.4.4.2: a layer statement between matching media
     queries still establishes layer order at that point. Media-query merging
     must not cross it. *)
  let media_rule selector color =
    Css.media ~condition:(Css.Media.Min_width 48.)
      [
        Css.rule
          ~selector:(Css.Selector.class_ selector)
          [ Css.Declaration.color (hex_color color) ];
      ]
  in
  let input =
    [
      media_rule "a" "ff0000";
      Css.Stylesheet.Layer_decl [ "theme" ];
      media_rule "b" "0000ff";
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "matching media queries do not merge across layer statement"
    "@media(width>=48px){.a{color:red}}@layer \
     theme;@media(width>=48px){.b{color:#00f}}"
    output

let c61_all_property_reset_boundary () =
  (* CSS Cascade section 3: the 'all' shorthand resets nearly every property. It
     is not a duplicate of a later ordinary longhand and must remain in source
     order. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "reset";
      declarations =
        [
          Css.Declaration.color (hex_color "ff0000");
          Css.Declaration.v Css.Properties.All Css.Properties.Unset;
          Css.Declaration.display Flex;
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "all shorthand reset remains before later longhand"
    ".reset{all:unset;display:flex}" output

let c61_no_factor_across_all_reset () =
  (* CSS Cascade section 3: [all] is a reset shorthand, so factoring a shared
     longhand out of an adjacent rule can move it before the reset and change
     the computed value. Keep any rule containing [all] out of adjacent
     declaration factoring. *)
  let check_case reset =
    let input =
      Css.Stylesheet.read
        (Fmt.kstr Cursor.of_string ".foo{color:red}.bar{all:%s;color:red}" reset)
    in
    let optimized = Css.Optimize.stylesheet input in
    let output =
      Css.Stylesheet.to_string ~minify:true optimized |> String.trim
    in
    Alcotest.(check string)
      (Fmt.str "all:%s keeps later color after reset" reset)
      (Fmt.str ".foo{color:red}.bar{all:%s;color:red}" reset)
      output
  in
  List.iter check_case [ "unset"; "initial"; "revert-layer" ]

let c61_no_merge_layer () =
  (* CSS Cascade section 6.1: layers are a cascade sorting criterion. Rules in
     different layers must not be merged, even when their selectors match. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( Some "reset",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "btn")
              [ Css.Declaration.display Block ];
          ] );
      Css.Stylesheet.Layer
        ( Some "components",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "btn")
              [ Css.Declaration.display Flex ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across layer boundary"
    "@layer reset{.btn{display:block}}@layer components{.btn{display:flex}}"
    output

let c64_layer_order_boundary () =
  (* CSS Cascade section 6.4.4.2: a statement @layer rule establishes layer
     order at its source position. Rule merging must not move style rules across
     that ordering point, even when their selectors match. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "theme")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.Stylesheet.Layer_decl [ "reset"; "components" ];
      Css.rule
        ~selector:(Css.Selector.class_ "theme")
        [ Css.Declaration.display Flex ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across layer statement boundary"
    ".theme{color:red}@layer reset,components;.theme{display:flex}" output

let c61_unlayered_outside_layer () =
  (* CSS Cascade section 6.1: unlayered declarations are in the implicit final
     layer for normal declarations. Optimizing must not hoist unlayered rules
     into explicit layers or pull layered rules out. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( Some "reset",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "audio")
              [ Css.Declaration.display Block ];
          ] );
      Css.rule
        ~selector:(Css.Selector.class_ "audio")
        [ Css.Declaration.display Flex ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "unlayered rule stays outside explicit layer"
    "@layer reset{.audio{display:block}}.audio{display:flex}" output

let c61_important_layer_order () =
  (* CSS Cascade section 6.1: for important declarations, earlier layers have
     higher priority than later layers. Optimizing must preserve both layer
     membership and layer order. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( Some "base",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "btn")
              [
                Css.Declaration.important
                  (Css.Declaration.color (hex_color "ff0000"));
              ];
          ] );
      Css.Stylesheet.Layer
        ( Some "theme",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "btn")
              [
                Css.Declaration.important
                  (Css.Declaration.color (hex_color "0000ff"));
              ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "important declarations keep layer order"
    "@layer base{.btn{color:red!important}}@layer \
     theme{.btn{color:#00f!important}}"
    output

let c61_style_attr_boundary () =
  (* CSS Cascade section 6.1 gives element-attached declarations a distinct
     cascade slot. The closest AST analogue here is a bare declaration block: it
     must remain a boundary for surrounding selector-mapped rules. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.Stylesheet.Declarations
        [ Css.Declaration.background_color (hex_color "00ff00") ];
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.display Flex ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "bare declarations remain an optimizer boundary"
    ".card{color:red}background-color:#0f0;.card{display:flex}" output

let c61_adjacent_specificity_grouping () =
  (* CSS Cascade section 6.1 compares specificity per selector. Grouping
     adjacent rules with identical declarations must keep each selector intact
     rather than rewriting them into a different selector shape. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "item")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule
        ~selector:
          (Css.Selector.compound
             [ Css.Selector.class_ "item"; Css.Selector.class_ "active" ])
        [ Css.Declaration.color (hex_color "ff0000") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "adjacent grouping keeps selector-specific specificity"
    ".item,.item.active{color:red}" output

let c61_specificity_blocks_grouping () =
  (* CSS Cascade section 6.1: specificity is evaluated before source order, but
     source order still matters among declarations that tie. A grouping pass
     must not move lower-specificity selectors across an overlapping
     higher-specificity rule and change the neighboring tie behavior. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "item")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.rule
        ~selector:
          (Css.Selector.compound
             [ Css.Selector.class_ "item"; Css.Selector.class_ "active" ])
        [ Css.Declaration.color (hex_color "0000ff") ];
      Css.rule
        ~selector:(Css.Selector.class_ "active")
        [ Css.Declaration.color (hex_color "ff0000") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "specificity competitor remains between lower-specificity rules"
    ".item{color:red}.item.active{color:#00f}.active{color:red}" output

let c61_no_merge_scope () =
  (* CSS Cascade level 6 adds scope proximity to the cascade sorting order.
     Scoped and unscoped rules must not be merged across the @scope boundary. *)
  let item_rule decl =
    Css.rule ~selector:(Css.Selector.class_ "item") [ decl ]
  in
  let input =
    [
      item_rule (Css.Declaration.color (hex_color "ff0000"));
      Css.Stylesheet.Scope
        ( Some ".component",
          None,
          [
            Css.rule
              ~selector:(Css.Selector.class_ "scoped")
              [ Css.Declaration.display Block ];
          ] );
      item_rule (Css.Declaration.display Flex);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   before_stmt;
   Css.Stylesheet.Scope (Some ".component", None, [ scoped_stmt ]);
   after_stmt;
  ] ->
      let before = rule_of_statement before_stmt in
      let scoped = rule_of_statement scoped_stmt in
      let after = rule_of_statement after_stmt in
      Alcotest.(check string)
        "rule before scope is unchanged" ".item{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule before ]
        |> String.trim);
      Alcotest.(check string)
        "scoped rule is unchanged" ".scoped{display:block}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule scoped ]
        |> String.trim);
      Alcotest.(check string)
        "rule after scope is unchanged" ".item{display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule after ]
        |> String.trim)
  | _ -> Alcotest.fail "optimizer must preserve rule/scope/rule structure"

let c61_distinct_scopes_preserved () =
  (* CSS Cascade level 6: two @scope rules can produce different proximity for
     the same scoped style rule. Equal nested rules in different scopes must
     stay in their original scope blocks. *)
  let scoped_rule =
    Css.rule
      ~selector:(Css.Selector.class_ "item")
      [ Css.Declaration.color (hex_color "ff0000") ]
  in
  let input =
    [
      Css.Stylesheet.Scope (Some ".outer", None, [ scoped_rule ]);
      Css.Stylesheet.Scope (Some ".inner", None, [ scoped_rule ]);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Scope (Some ".outer", None, [ outer_stmt ]);
   Css.Stylesheet.Scope (Some ".inner", None, [ inner_stmt ]);
  ] ->
      let outer = rule_of_statement outer_stmt in
      let inner = rule_of_statement inner_stmt in
      Alcotest.(check string)
        "outer scoped rule is unchanged" ".item{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule outer ]
        |> String.trim);
      Alcotest.(check string)
        "inner scoped rule is unchanged" ".item{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule inner ]
        |> String.trim)
  | _ -> Alcotest.fail "optimizer must preserve distinct scope blocks"

let c61_distinct_scope_limits_preserved () =
  (* CSS Cascade level 6: the scope limit changes where a scoped rule applies.
     Equal rules with the same root but different limits must not be merged into
     one scope block. *)
  let scoped_rule =
    Css.rule
      ~selector:(Css.Selector.class_ "item")
      [ Css.Declaration.color (hex_color "ff0000") ]
  in
  let input =
    [
      Css.Stylesheet.Scope (Some ".card", Some ".footer", [ scoped_rule ]);
      Css.Stylesheet.Scope (Some ".card", Some ".aside", [ scoped_rule ]);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same scoped rule remains split by distinct scope limits"
    "@scope(.card)to (.footer){.item{color:red}}@scope(.card)to \
     (.aside){.item{color:red}}"
    output

let c61_no_merge_supports () =
  (* CSS Cascade section 6.1 order of appearance applies after filtering.
     Conditional groups are not optimizer reordering points for surrounding
     rules, even when the surrounding selectors match. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.supports
        ~condition:(Css.Supports.property "display" "flex")
        [
          Css.rule
            ~selector:(Css.Selector.class_ "feature")
            [ Css.Declaration.display Flex ];
        ];
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.background_color (hex_color "0000ff") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across supports boundary"
    ".card{color:red}@supports(display:flex){.feature{display:flex}}.card{background-color:#00f}"
    output

let c61_no_merge_container () =
  (* CSS Cascade section 6.1 order of appearance still determines the winner
     among declarations that tie after a container query matches. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.container ~condition:(Css.Container.Min_width_px 48)
        [
          Css.rule
            ~selector:(Css.Selector.class_ "feature")
            [ Css.Declaration.display Flex ];
        ];
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.background_color (hex_color "0000ff") ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across container boundary"
    ".card{color:red}@container(width>=48px){.feature{display:flex}}.card{background-color:#00f}"
    output

let c61_no_merge_starting_style () =
  (* CSS Cascade section 6.1 includes transitions as the highest-precedence
     origin, and @starting-style participates in transition setup. It must stay
     as an ordering boundary for surrounding rules. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "toast")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.Stylesheet.Starting_style
        [
          Css.rule
            ~selector:(Css.Selector.class_ "toast")
            [ Css.Declaration.opacity (Opacity_number 0.) ];
        ];
      Css.rule
        ~selector:(Css.Selector.class_ "toast")
        [ Css.Declaration.display Flex ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across starting-style boundary"
    ".toast{color:red}@starting-style{.toast{opacity:0}}.toast{display:flex}"
    output

let c61_import_substitution_point () =
  (* The optimizer strips this unresolved import placeholder, but it still must
     not merge the surrounding same-selector rules. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.class_ "theme")
        [ Css.Declaration.color (hex_color "ff0000") ];
      Css.Stylesheet.Import
        {
          url = "url(\"base.css\")";
          layer = None;
          supports = None;
          media = None;
        };
      Css.rule
        ~selector:(Css.Selector.class_ "theme")
        [ Css.Declaration.display Flex ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same selector is not merged across import substitution point"
    ".theme{color:red}.theme{display:flex}" output

let c61_no_named_atrule_merge () =
  (* CSS-wide name-defining at-rules and descriptor at-rules are stylesheet
     statements, not rule declarations. Optimizing adjacent rules must preserve
     their source positions instead of treating them as transparent
     separators. *)
  let check_case label css expected =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized = Css.Optimize.stylesheet input in
    let output =
      Css.Stylesheet.to_string ~minify:true optimized |> String.trim
    in
    Alcotest.(check string) label expected output
  in
  check_case "font-face boundary"
    ".theme{color:red}@font-face{font-family:Brand;src:url(brand.woff2)}.theme{display:flex}"
    ".theme{color:red}@font-face{font-family:Brand;src:url(brand.woff2)}.theme{display:flex}";
  check_case "keyframes boundary"
    ".theme{color:red}@keyframes \
     fade{from{opacity:0}to{opacity:1}}.theme{display:flex}"
    ".theme{color:red}@keyframes \
     fade{0%{opacity:0}to{opacity:1}}.theme{display:flex}";
  check_case "property registration boundary"
    ".theme{color:red}@property \
     --gap{syntax:\"<length>\";inherits:false;initial-value:1rem}.theme{display:flex}"
    ".theme{color:red}@property \
     --gap{syntax:\"<length>\";inherits:false;initial-value:1rem}.theme{display:flex}";
  check_case "view-transition boundary"
    ".theme{color:red}@view-transition{navigation:auto}.theme{display:flex}"
    ".theme{color:red}@view-transition{navigation:auto}.theme{display:flex}"

let c61_no_nested_boundary_merge () =
  (* Scope proximity and page context are cascade-visible boundaries. The same
     is true when @scope appears as a nested group rule inside a style rule. *)
  let check_case label css expected =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized = Css.Optimize.stylesheet input in
    let output =
      Css.Stylesheet.to_string ~minify:true optimized |> String.trim
    in
    Alcotest.(check string) label expected output
  in
  check_case "top-level scope boundary"
    ".item{color:red}@scope(.card){.item{display:block}}.item{padding:1rem}"
    ".item{color:red}@scope(.card){.item{display:block}}.item{padding:1rem}";
  check_case "distinct scope roots stay split"
    "@scope(.card){.item{color:red}}@scope(.panel){.item{color:red}}"
    "@scope(.card){.item{color:red}}@scope(.panel){.item{color:red}}";
  check_case "page boundary"
    ".doc{color:red}@page:left{margin-left:2cm}.doc{display:block}"
    ".doc{color:red}@page:left{margin-left:2cm}.doc{display:block}";
  check_case "nested scope boundary"
    ".card{& .title{color:red}@scope(&) to (.boundary){& \
     .title{display:block}}& .title{padding:1rem}}"
    ".card{& .title{color:red}@scope(&)to (.boundary){& \
     .title{display:block}}& .title{padding:1rem}}"

let c61_no_pseudo_group () =
  (* Grouping equal declarations across an overlapping pseudo-class competitor
     can move source-order ties for elements matching both selectors. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string ".btn{color:red}.btn:hover{color:blue}.link{color:red}")
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "equal declarations do not group across pseudo-class competitor"
    ".btn{color:red}.btn:hover{color:#00f}.link{color:red}" output

let c61_no_conditional_cli_merge () =
  (* Surrounding rules must stay split across supports/container/starting-style
     boundaries; each condition filters declarations independently. *)
  let css =
    ".card{color:red}@supports \
     (display:grid){.card{display:grid}}.card{padding:1rem}@container \
     (inline-size > \
     30em){.card{margin:1rem}}.card{border-color:blue}@starting-style{.card{opacity:0}}.card{background-color:white}"
  in
  let input = Css.Stylesheet.read (Cursor.of_string css) in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "rules do not merge across conditional boundaries"
    ".card{color:red}@supports(display:grid){.card{display:grid}}.card{padding:1rem}@container(inline-size>30em){.card{margin:1rem}}.card{border-color:#00f}@starting-style{.card{opacity:0}}.card{background-color:#fff}"
    output

let c61_important_blocks_longhand () =
  (* CSS Cascade sections 3 and 6.1: an important shorthand is equivalent to
     important declarations for all of its longhands, so a later normal longhand
     cannot override it. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "box";
      declarations =
        [
          Css.Declaration.margin_left (Px 1.);
          Css.Declaration.important (Css.Declaration.margin [ Px 2. ]);
          Css.Declaration.margin_left (Px 3.);
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "important shorthand keeps priority over later normal longhand"
    ".box{margin:2px!important}" output

let c63_mixed_importance_edges () =
  (* CSS Cascade section 6.3: importance is applied per declaration. A normal
     shorthand still contributes the longhands not overridden by an important
     longhand, so neither declaration is dead in either source-order
     direction. *)
  let check_rule label expected declarations =
    let rule : Css.Stylesheet.rule =
      {
        selector = Css.Selector.class_ "box";
        declarations;
        nested = [];
        merge_key = None;
      }
    in
    let optimized = Css.Optimize.single_rule rule in
    let output =
      Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
      |> String.trim
    in
    Alcotest.(check string) label expected output
  in
  check_rule "earlier important longhand survives later normal shorthand"
    ".box{margin-left:1px!important;margin:2px}"
    [
      Css.Declaration.important (Css.Declaration.margin_left (Px 1.));
      Css.Declaration.margin [ Px 2. ];
    ];
  check_rule "later important longhand survives earlier normal shorthand"
    ".box{margin:2px;margin-left:1px!important}"
    [
      Css.Declaration.margin [ Px 2. ];
      Css.Declaration.important (Css.Declaration.margin_left (Px 1.));
    ]

let c62_origin_importance_rank () =
  (* CSS Cascade section 6.2 defines origins, and section 6.1 orders them with
     importance. The API rank should represent that cascade order directly. *)
  let rank origin important =
    Css.Stylesheet.origin_importance_rank ~important origin
  in
  Alcotest.(check (list int))
    "origin and importance precedence from highest to lowest"
    [ 9; 8; 7; 6; 5; 4; 3; 2; 1 ]
    [
      rank Transition false;
      rank User_agent true;
      rank User true;
      rank Author true;
      rank Animation false;
      rank Author false;
      rank Author_presentational_hint false;
      rank User false;
      rank User_agent false;
    ]

let c62_no_merge_author_user () =
  (* CSS Cascade section 6.2: author and user stylesheets are distinct cascade
     origins. Equal selectors from different origins must stay separated. *)
  let origin_rule origin color =
    Css.Stylesheet.with_origin origin
      [
        Css.rule
          ~selector:(Css.Selector.class_ "doc")
          [ Css.Declaration.color (hex_color color) ];
      ]
  in
  let input = [ origin_rule User "ff0000"; origin_rule Author "0000ff" ] in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Origin (User, [ user_stmt ]);
   Css.Stylesheet.Origin (Author, [ author_stmt ]);
  ] ->
      let user_rule = rule_of_statement user_stmt in
      let author_rule = rule_of_statement author_stmt in
      Alcotest.(check string)
        "user-origin rule is preserved" ".doc{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule user_rule ]
        |> String.trim);
      Alcotest.(check string)
        "author-origin rule is preserved" ".doc{color:#00f}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule author_rule ]
        |> String.trim)
  | _ -> Alcotest.fail "optimizer must preserve user and author origin blocks"

let c62_no_merge_ua_author () =
  (* CSS Cascade section 6.2: user-agent defaults, user styles, and author
     styles are separate origins with different normal precedence. *)
  let origin_rule origin display =
    Css.Stylesheet.with_origin origin
      [
        Css.rule
          ~selector:(Css.Selector.class_ "panel")
          [ Css.Declaration.display display ];
      ]
  in
  let input =
    [
      origin_rule User_agent Block;
      origin_rule User Flex;
      origin_rule Author Inline_flex;
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Origin (User_agent, [ ua_stmt ]);
   Css.Stylesheet.Origin (User, [ user_stmt ]);
   Css.Stylesheet.Origin (Author, [ author_stmt ]);
  ] ->
      let ua_rule = rule_of_statement ua_stmt in
      let user_rule = rule_of_statement user_stmt in
      let author_rule = rule_of_statement author_stmt in
      Alcotest.(check string)
        "user-agent-origin rule is preserved" ".panel{display:block}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule ua_rule ]
        |> String.trim);
      Alcotest.(check string)
        "user-origin rule is preserved" ".panel{display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule user_rule ]
        |> String.trim);
      Alcotest.(check string)
        "author-origin rule is preserved" ".panel{display:inline-flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule author_rule ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "optimizer must preserve user-agent, user, and author origin blocks"

let c62_animation_transition_origins () =
  (* CSS Cascade section 6.2: animation and transition origins are generated
     virtual origins and must not be folded into author styles. *)
  let origin_rule origin color =
    Css.Stylesheet.with_origin origin
      [
        Css.rule
          ~selector:(Css.Selector.class_ "animated")
          [ Css.Declaration.color (hex_color color) ];
      ]
  in
  let input =
    [
      origin_rule Author "ff0000";
      origin_rule Animation "00ff00";
      origin_rule Transition "0000ff";
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Origin (Author, [ author_stmt ]);
   Css.Stylesheet.Origin (Animation, [ animation_stmt ]);
   Css.Stylesheet.Origin (Transition, [ transition_stmt ]);
  ] ->
      let author_rule = rule_of_statement author_stmt in
      let animation_rule = rule_of_statement animation_stmt in
      let transition_rule = rule_of_statement transition_stmt in
      Alcotest.(check string)
        "author-origin rule is preserved" ".animated{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule author_rule ]
        |> String.trim);
      Alcotest.(check string)
        "animation-origin rule is preserved" ".animated{color:#0f0}"
        (Css.Stylesheet.to_string ~minify:true
           [ statement_of_rule animation_rule ]
        |> String.trim);
      Alcotest.(check string)
        "transition-origin rule is preserved" ".animated{color:#00f}"
        (Css.Stylesheet.to_string ~minify:true
           [ statement_of_rule transition_rule ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "optimizer must preserve author, animation, and transition origins"

let c62_optimize_single_origin () =
  (* CSS Cascade section 6.2 creates an origin boundary, not a ban on safe
     optimization inside one origin. Adjacent same-selector author rules can
     still merge within the author-origin block. *)
  let input =
    [
      Css.Stylesheet.with_origin Author
        [
          Css.rule
            ~selector:(Css.Selector.class_ "doc")
            [ Css.Declaration.color (hex_color "ff0000") ];
          Css.rule
            ~selector:(Css.Selector.class_ "doc")
            [ Css.Declaration.display Flex ];
        ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [ Css.Stylesheet.Origin (Author, [ stmt ]) ] ->
      let author_rule = rule_of_statement stmt in
      Alcotest.(check string)
        "adjacent author-origin rules merge inside the same origin"
        ".doc{color:red;display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule author_rule ]
        |> String.trim)
  | _ -> Alcotest.fail "optimizer should preserve one optimized author origin"

let c62_no_group_across_origins () =
  (* CSS Cascade section 6.2: origins are part of the cascade input. Equal
     declaration blocks from different origins must not be selector-grouped into
     one rule. *)
  let origin_rule origin selector =
    Css.Stylesheet.with_origin origin
      [
        Css.rule
          ~selector:(Css.Selector.class_ selector)
          [ Css.Declaration.color (hex_color "ff0000") ];
      ]
  in
  let input = [ origin_rule User "user"; origin_rule Author "author" ] in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Origin (User, [ user_stmt ]);
   Css.Stylesheet.Origin (Author, [ author_stmt ]);
  ] ->
      let user_rule = rule_of_statement user_stmt in
      let author_rule = rule_of_statement author_stmt in
      Alcotest.(check string)
        "user-origin selector remains local to user origin" ".user{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule user_rule ]
        |> String.trim);
      Alcotest.(check string)
        "author-origin selector remains local to author origin"
        ".author{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule author_rule ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "optimizer must not group identical declarations across origins"

let c62_imports_keep_origin () =
  (* The unresolved import placeholder is stripped during optimization; the
     surrounding rules still remain in their author-origin wrapper. *)
  let before_rule =
    {
      Css.Stylesheet.selector = Css.Selector.class_ "theme";
      declarations = [ Css.Declaration.color (hex_color "ff0000") ];
      nested = [];
      merge_key = None;
    }
  in
  let after_rule =
    {
      Css.Stylesheet.selector = Css.Selector.class_ "theme";
      declarations = [ Css.Declaration.display Flex ];
      nested = [];
      merge_key = None;
    }
  in
  let import =
    Css.Stylesheet.Import
      {
        url = "url(\"theme-base.css\")";
        layer = None;
        supports = None;
        media = None;
      }
  in
  let input =
    [
      Css.Stylesheet.with_origin Author
        [ statement_of_rule before_rule; import; statement_of_rule after_rule ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [ Css.Stylesheet.Origin (Author, [ before_stmt; after_stmt ]) ] ->
      let before = rule_of_statement before_stmt in
      let after_ = rule_of_statement after_stmt in
      Alcotest.(check string)
        "rule before stripped import remains in author origin"
        ".theme{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule before ]
        |> String.trim);
      Alcotest.(check string)
        "rule after stripped import remains in author origin"
        ".theme{display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule after_ ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "optimizer must preserve author-origin wrapper around import neighbors"

let c62_origin_wrapper_api () =
  (* CSS Cascade section 6.2 has no CSS syntax for choosing a stylesheet origin
     inside one CSS file, so the origin wrapper is an API-level annotation. *)
  let stmt =
    Css.with_origin User
      [
        Css.rule
          ~selector:(Css.Selector.class_ "reader")
          [ Css.color (Css.hex "#00ff00") ];
      ]
  in
  (match Css.as_origin stmt with
  | Some (User, [ child ]) -> (
      match Css.as_rule child with
      | Some (selector, declarations, []) ->
          Alcotest.(check string)
            "origin child selector" ".reader"
            (Css.Selector.to_string selector);
          Alcotest.(check int)
            "origin child declaration count" 1 (List.length declarations)
      | _ -> Alcotest.fail "origin child should be a rule")
  | _ -> Alcotest.fail "Css.with_origin should be visible through Css.as_origin");
  Alcotest.(check string)
    "origin annotation has no stylesheet syntax" ".reader{color:#0f0}"
    (Css.to_string ~minify:true (Css.v [ stmt ]))

let c63_important_beats_normal () =
  (* CSS Cascade section 6.3: an important declaration takes precedence over a
     normal declaration, even when the normal declaration appears later. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "alert";
      declarations =
        [
          Css.Declaration.important (Css.Declaration.color (hex_color "ff0000"));
          Css.Declaration.color (hex_color "0000ff");
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "important declaration beats later normal declaration"
    ".alert{color:red!important}" output

let c63_later_important_wins () =
  (* CSS Cascade section 6.3 changes the importance weight, but declarations
     with the same origin and importance still fall through to source order. *)
  let rule : Css.Stylesheet.rule =
    {
      selector = Css.Selector.class_ "alert";
      declarations =
        [
          Css.Declaration.important (Css.Declaration.color (hex_color "ff0000"));
          Css.Declaration.important (Css.Declaration.color (hex_color "0000ff"));
        ];
      nested = [];
      merge_key = None;
    }
  in
  let optimized = Css.Optimize.single_rule rule in
  let output =
    Css.Stylesheet.to_string ~minify:true [ statement_of_rule optimized ]
    |> String.trim
  in
  Alcotest.(check string)
    "later important declaration wins within the same origin and importance"
    ".alert{color:#00f!important}" output

let c63_importance_inverts_origin () =
  (* CSS Cascade section 6.3 balances author and user styles: normal origin
     precedence is author > user > user-agent, while important origin precedence
     is inverted. *)
  let rank origin important =
    Css.Stylesheet.origin_importance_rank ~important origin
  in
  Alcotest.(check bool)
    "normal author beats normal user" true
    (rank Author false > rank User false);
  Alcotest.(check bool)
    "normal user beats normal user-agent" true
    (rank User false > rank User_agent false);
  Alcotest.(check bool)
    "important user beats important author" true
    (rank User true > rank Author true);
  Alcotest.(check bool)
    "important user-agent beats important user" true
    (rank User_agent true > rank User true);
  Alcotest.(check bool)
    "important author beats animation origin" true
    (rank Author true > rank Animation false)

let c63_keyframes_ignore_important () =
  (* CSS Cascade section 6.3: declarations qualified with !important inside
     @keyframes are ignored. *)
  let stylesheet =
    Css.Stylesheet.read
      (Cursor.of_string
         "@keyframes fade{from{opacity:0!important}to{opacity:1}}")
  in
  match stylesheet with
  | [
   Css.Stylesheet.Keyframes
     ( "fade",
       [
         {
           keyframe_selector = Css.Keyframe.Positions [ Css.Keyframe.From ];
           keyframe_declarations = from_decls;
         };
         {
           keyframe_selector = Css.Keyframe.Positions [ Css.Keyframe.To ];
           keyframe_declarations = to_decls;
         };
       ] );
  ] ->
      Alcotest.(check int)
        "important keyframe declaration is ignored" 0 (List.length from_decls);
      Alcotest.(check int)
        "normal keyframe declaration remains" 1 (List.length to_decls)
  | _ -> Alcotest.fail "expected parsed fade keyframes"

let c64_statement_layer_order () =
  (* CSS Cascade section 6.4.4.2: the statement form can declare multiple layer
     names up front, establishing their order independently of where the block
     rules appear later. *)
  let input =
    [
      Css.Stylesheet.Layer_decl [ "default"; "theme"; "components" ];
      Css.Stylesheet.Layer
        ( Some "theme",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "widget")
              [ Css.Declaration.color (hex_color "0000ff") ];
          ] );
      Css.Stylesheet.Layer
        ( Some "default",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "widget")
              [ Css.Declaration.color (hex_color "ff0000") ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "statement layer order remains before later block assignments"
    "@layer default,theme,components;@layer theme{.widget{color:#00f}}@layer \
     default{.widget{color:red}}"
    output

let c64_redundant_layer_prelude () =
  (* CSS Cascade section 6.4.4.2: when a layer statement only repeats the layer
     order introduced immediately by following layer blocks/statements, it is a
     redundant minified spelling. Lightning CSS drops this form too. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string
         "@layer theme,base,components,utilities;@layer \
          theme{:root{--x:1}}@layer base{a{color:red}}@layer \
          components,utilities;")
  in
  let output = minify input in
  Alcotest.(check string)
    "redundant leading layer order is removed"
    "@layer theme{:root{--x:1}}@layer base{a{color:red}}@layer \
     components,utilities;"
    output

let c64_redundant_layer_duplicate () =
  (* A layer statement that names only layers already introduced by earlier
     blocks does not change layer order and can be dropped. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string "@layer theme{a{color:red}}@layer theme;")
  in
  let output = minify input in
  Alcotest.(check string)
    "duplicate layer statement after block is removed"
    "@layer theme{a{color:red}}" output

let c64_layer_prelude_order_boundary () =
  (* The prelude is not redundant if later layer blocks introduce the same names
     in a different order. Removing it would change normal and important layer
     precedence. The trailing [@layer components;] is still redundant because
     the kept prelude already introduces [components]. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string
         "@layer theme,base,components;@layer base{a{color:red}}@layer \
          theme{:root{--x:1}}@layer components;")
  in
  let output = minify input in
  Alcotest.(check string)
    "layer prelude remains when it changes later block order"
    "@layer theme,base,components;@layer base{a{color:red}}@layer \
     theme{:root{--x:1}}"
    output

let c64_layer_prelude_missing_name () =
  (* The prelude is not redundant if it declares a layer that following blocks
     do not introduce. The empty layer still participates in layer order, but
     the later standalone [@layer components;] only repeats a name already
     introduced by the kept prelude. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string
         "@layer theme,base,components;@layer theme{:root{--x:1}}@layer \
          components;")
  in
  let output = minify input in
  Alcotest.(check string)
    "layer prelude remains when it introduces an otherwise empty layer"
    "@layer theme,base,components;@layer theme{:root{--x:1}}" output

let c64_layer_prelude_import_barrier () =
  (* Layer statements before imports are part of the import prelude. Do not
     remove them by looking through an import boundary to a later layer
     block. *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string
         "@layer theme;@import \"theme.css\";@layer theme{a{color:red}}")
  in
  let output = minify input in
  Alcotest.(check string)
    "layer declaration before import is not removed across import boundary"
    "@layer theme;@import\"theme.css\";@layer theme{a{color:red}}" output

let c64_unlayered_final_layer () =
  (* CSS Cascade section 6.4 example: normal unlayered declarations are in the
     implicit final layer and can outrank more-specific explicit-layer rules.
     The optimizer must not move either rule across that layer boundary. *)
  let input =
    [
      Css.rule
        ~selector:(Css.Selector.element "audio")
        [ Css.Declaration.display Flex ];
      Css.Stylesheet.Layer
        ( Some "reset",
          [
            Css.rule
              ~selector:
                (Css.Selector.compound
                   [
                     Css.Selector.element "audio";
                     Css.Selector.attribute "controls" Css.Selector.Presence;
                   ])
              [ Css.Declaration.display Block ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "unlayered normal rule remains outside explicit reset layer"
    "audio{display:flex}@layer reset{audio[controls]{display:block}}" output

let c64_important_layers_reverse () =
  (* CSS Cascade sections 6.1 and 6.4: later layers win for normal declarations,
     but earlier layers win for important declarations. *)
  let input =
    [
      Css.Stylesheet.Layer_decl [ "defaults"; "overrides" ];
      Css.Stylesheet.Layer
        ( Some "defaults",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "notice")
              [
                Css.Declaration.important
                  (Css.Declaration.color (hex_color "ff0000"));
              ];
          ] );
      Css.Stylesheet.Layer
        ( Some "overrides",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "notice")
              [
                Css.Declaration.important
                  (Css.Declaration.color (hex_color "0000ff"));
              ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "important declarations keep earlier and later layers distinct"
    "@layer defaults,overrides;@layer \
     defaults{.notice{color:red!important}}@layer \
     overrides{.notice{color:#00f!important}}"
    output

let c64_anonymous_layers_distinct () =
  (* CSS Cascade section 6.4.2.1: each anonymous @layer block has a unique
     anonymous segment, so two unnamed layers cannot be merged into one
     layer. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( None,
          [
            Css.rule
              ~selector:(Css.Selector.class_ "private")
              [ Css.Declaration.color (hex_color "ff0000") ];
          ] );
      Css.Stylesheet.Layer
        ( None,
          [
            Css.rule
              ~selector:(Css.Selector.class_ "private")
              [ Css.Declaration.display Flex ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Layer (None, [ first_stmt ]);
   Css.Stylesheet.Layer (None, [ second_stmt ]);
  ] ->
      let first = rule_of_statement first_stmt in
      let second = rule_of_statement second_stmt in
      Alcotest.(check string)
        "first anonymous layer remains distinct" ".private{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule first ]
        |> String.trim);
      Alcotest.(check string)
        "second anonymous layer remains distinct" ".private{display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule second ]
        |> String.trim)
  | _ -> Alcotest.fail "anonymous layer blocks must remain separate"

let c64_nested_layer_distinct () =
  (* CSS Cascade section 6.4.2: a nested framework.base layer is distinct from
     the top-level base layer. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( Some "base",
          [
            Css.rule ~selector:(Css.Selector.element "p")
              [ Css.Declaration.max_width (Ch 70.) ];
          ] );
      Css.Stylesheet.Layer
        ( Some "framework",
          [
            Css.Stylesheet.Layer
              ( Some "base",
                [
                  Css.rule ~selector:(Css.Selector.element "p")
                    [ Css.Declaration.margin_block (Em 0.75) ];
                ] );
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Layer (Some "base", [ base_stmt ]);
   Css.Stylesheet.Layer
     ( Some "framework",
       [ Css.Stylesheet.Layer (Some "base", [ framework_base_stmt ]) ] );
  ] ->
      let base_rule = rule_of_statement base_stmt in
      let framework_base_rule = rule_of_statement framework_base_stmt in
      Alcotest.(check string)
        "top-level base layer remains top-level" "p{max-width:70ch}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule base_rule ]
        |> String.trim);
      Alcotest.(check string)
        "nested framework.base layer remains nested" "p{margin-block:.75em}"
        (Css.Stylesheet.to_string ~minify:true
           [ statement_of_rule framework_base_rule ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "nested framework.base layer must remain distinct from top-level base"

let c64_keyframe_name_layers () =
  (* CSS Cascade section 6.4: name-defining at-rules such as @keyframes use
     layer order to resolve collisions, so same-name keyframes in different
     layers must not be deduplicated or hoisted out of their layers. *)
  let frame decl =
    {
      Css.Stylesheet.keyframe_selector =
        Css.Keyframe.Positions [ Css.Keyframe.From ];
      keyframe_declarations = [ decl ];
    }
  in
  let input =
    [
      Css.Stylesheet.Layer_decl [ "framework"; "override" ];
      Css.Stylesheet.Layer
        ( Some "override",
          [
            Css.Stylesheet.Keyframes
              ( "slide-left",
                [ frame (Css.Declaration.opacity (Opacity_number 0.)) ] );
          ] );
      Css.Stylesheet.Layer
        ( Some "framework",
          [
            Css.Stylesheet.Keyframes
              ("slide-left", [ frame (Css.Declaration.margin_left (Pct 0.)) ]);
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "same-name keyframes remain in their declared layers"
    "@layer framework,override;@layer override{@keyframes \
     slide-left{0%{opacity:0}}}@layer framework{@keyframes \
     slide-left{0%{margin-left:0%}}}"
    output

let c64_layer_decls_import_cross () =
  (* CSS Cascade section 6.4.4.2: layer statement rules can be interleaved with
     imports to establish order, but @import and @namespace processing still
     depends on their source positions. Optimizing must not merge layer
     declarations across an import. *)
  let input =
    [
      Css.Stylesheet.Layer_decl [ "default" ];
      Css.Stylesheet.Import
        {
          url = "url(\"theme.css\")";
          layer = Some "theme";
          supports = None;
          media = None;
        };
      Css.Stylesheet.Layer_decl [ "components" ];
      Css.Stylesheet.Layer
        ( Some "default",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "audio")
              [ Css.Declaration.display Block ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "layer declarations remain on their own sides of the import"
    "@layer default;@import\"theme.css\"layer(theme);@layer components;@layer \
     default{.audio{display:block}}"
    output

let c64_repeated_layer_blocks_ordered () =
  (* CSS Cascade section 6.4.2: repeated explicit layer identifiers assign style
     blocks to the same layer. Same-name blocks in one scope may be serialized
     as one layer block at the layer's first occurrence. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( Some "base",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "button")
              [ Css.Declaration.color (hex_color "ff0000") ];
          ] );
      Css.Stylesheet.Layer
        ( Some "theme",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "button")
              [ Css.Declaration.color (hex_color "0000ff") ];
          ] );
      Css.Stylesheet.Layer
        ( Some "base",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "button")
              [ Css.Declaration.display Flex ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "repeated named layer blocks merge by layer name"
    "@layer base{.button{color:red;display:flex}}@layer \
     theme{.button{color:#00f}}"
    output

let c64_child_layer_one_anonymous () =
  (* CSS Cascade section 6.4.2.1: child layers with the same name inside one
     anonymous parent share that anonymous parent segment, so they refer to the
     same nested layer. *)
  let input =
    [
      Css.Stylesheet.Layer
        ( None,
          [
            Css.Stylesheet.Layer
              ( Some "foo",
                [
                  Css.rule
                    ~selector:(Css.Selector.class_ "inside")
                    [ Css.Declaration.color (hex_color "ff0000") ];
                ] );
            Css.Stylesheet.Layer
              ( Some "foo",
                [
                  Css.rule
                    ~selector:(Css.Selector.class_ "inside")
                    [ Css.Declaration.display Flex ];
                ] );
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  (* Per shortest-wins, two adjacent [@layer foo] blocks inside the same
     anonymous parent merge into one - they refer to the same nested layer per
     Cascade L6 §6.4.2.1, so collapsing is spec-allowed. *)
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check bool)
    "anonymous parent preserved" true
    (Astring.String.is_infix ~affix:"@layer{" output
    || Astring.String.is_infix ~affix:"@layer {" output);
  Alcotest.(check bool)
    "single child foo layer with both rules" true
    (Astring.String.is_infix ~affix:"@layer foo{" output
    || Astring.String.is_infix ~affix:"@layer foo {" output);
  Alcotest.(check bool)
    "first declared color preserved" true
    (Astring.String.is_infix ~affix:"color:red" output);
  Alcotest.(check bool)
    "second declared display preserved" true
    (Astring.String.is_infix ~affix:"display:flex" output)

let c64_child_layer_distinct_anonymous () =
  (* CSS Cascade section 6.4.2.1: child layers with the same name inside
     separate anonymous parents are different layers because the anonymous
     parent segments are distinct. *)
  let anon_child color_or_display =
    Css.Stylesheet.Layer
      ( None,
        [
          Css.Stylesheet.Layer
            ( Some "foo",
              [
                Css.rule
                  ~selector:(Css.Selector.class_ "inside")
                  [ color_or_display ];
              ] );
        ] )
  in
  let input =
    [
      anon_child (Css.Declaration.color (hex_color "ff0000"));
      anon_child (Css.Declaration.display Flex);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Layer
     (None, [ Css.Stylesheet.Layer (Some "foo", [ first_stmt ]) ]);
   Css.Stylesheet.Layer
     (None, [ Css.Stylesheet.Layer (Some "foo", [ second_stmt ]) ]);
  ] ->
      let first = rule_of_statement first_stmt in
      let second = rule_of_statement second_stmt in
      Alcotest.(check string)
        "first anonymous parent keeps its foo child" ".inside{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule first ]
        |> String.trim);
      Alcotest.(check string)
        "second anonymous parent keeps its distinct foo child"
        ".inside{display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule second ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "same child layer name in distinct anonymous parents must not collapse"

let c64_conditional_layer_decls_nested () =
  (* CSS Cascade section 6.4.3: layer declarations inside media/supports can be
     conditional and must stay inside their conditional group; moving them out
     would establish a different global layer order. *)
  let input =
    [
      Css.media
        ~condition:(Css.Media.of_string "(min-width:30em)")
        [
          Css.layer ~name:"layout"
            [
              Css.rule
                ~selector:(Css.Selector.class_ "title")
                [ Css.Declaration.font_size (Rem 2.) ];
            ];
        ];
      Css.supports
        ~condition:(Css.Supports.property "display" "grid")
        [
          Css.Stylesheet.Layer_decl [ "grid" ];
          Css.layer ~name:"grid"
            [
              Css.rule
                ~selector:(Css.Selector.class_ "title")
                [ Css.Declaration.display Grid ];
            ];
        ];
      Css.Stylesheet.Layer_decl [ "theme"; "layout" ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "conditional layer declarations remain nested in their conditions"
    "@media(width>=30em){@layer \
     layout{.title{font-size:2rem}}}@supports(display:grid){@layer grid;@layer \
     grid{.title{display:grid}}}@layer theme,layout;"
    output

let c64_empty_layer_before_block () =
  (* CSS Cascade section 6.4.4.2: an empty statement can establish a layer order
     before a later block assigns style rules to that layer. *)
  let input =
    [
      Css.Stylesheet.Layer (Some "reset", []);
      Css.Stylesheet.Layer
        ( Some "components",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "card")
              [ Css.Declaration.display Flex ];
          ] );
      Css.Stylesheet.Layer
        ( Some "reset",
          [
            Css.rule
              ~selector:(Css.Selector.class_ "card")
              [ Css.Declaration.display Block ];
          ] );
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "empty named layer establishes order before later block"
    "@layer reset;@layer components{.card{display:flex}}@layer \
     reset{.card{display:block}}"
    output

let c64_layer_precedence_api () =
  (* CSS Cascade section 6.4.3: normal declarations rank later explicit layers
     above earlier ones, and unlayered normal declarations are in the implicit
     final layer. Important declarations reverse that layer order. *)
  let order = [ "reset"; "framework"; "theme" ] in
  let rank important layer =
    Css.Stylesheet.cascade_layer_precedence_rank ~layer_order:order ~important
      layer
  in
  Alcotest.(check bool)
    "normal framework layer beats normal reset layer" true
    (rank false (Some "framework") > rank false (Some "reset"));
  Alcotest.(check bool)
    "normal unlayered beats normal explicit layers" true
    (rank false None > rank false (Some "theme"));
  Alcotest.(check bool)
    "important reset layer beats important framework layer" true
    (rank true (Some "reset") > rank true (Some "framework"));
  Alcotest.(check bool)
    "important explicit layers beat important unlayered" true
    (rank true (Some "theme") > rank true None);
  let candidate layer important source_order value :
      Css.Stylesheet.cascade_layer_candidate =
    { layer; important; source_order; value }
  in
  let layer_value (c : Css.Stylesheet.cascade_layer_candidate) = c.value in
  let winner =
    Css.Stylesheet.winning_cascade_layer_candidate ~layer_order:order
      [
        candidate (Some "reset") false 0 "reset";
        candidate (Some "theme") false 1 "theme";
        candidate None false 2 "unlayered";
      ]
  in
  Alcotest.(check (option string))
    "normal unlayered candidate wins after explicit layers" (Some "unlayered")
    (Option.map layer_value winner);
  let important_winner =
    Css.Stylesheet.winning_cascade_layer_candidate ~layer_order:order
      [
        candidate None true 0 "unlayered-important";
        candidate (Some "theme") true 1 "theme-important";
        candidate (Some "reset") true 2 "reset-important";
      ]
  in
  Alcotest.(check (option string))
    "important first layer candidate wins after reversal"
    (Some "reset-important")
    (Option.map layer_value important_winner)

let c65_presentational_hint_rank () =
  (* CSS Cascade section 6.5: presentational hints can enter a special-purpose
     author presentational-hint origin between regular user and author
     origins. *)
  let rank origin =
    Css.Stylesheet.origin_importance_rank ~important:false origin
  in
  Alcotest.(check bool)
    "author styles beat author presentational hints" true
    (rank Author > rank Author_presentational_hint);
  Alcotest.(check bool)
    "author presentational hints beat user normal styles" true
    (rank Author_presentational_hint > rank User);
  Alcotest.(check bool)
    "author presentational hints beat user-agent normal styles" true
    (rank Author_presentational_hint > rank User_agent);
  let input =
    [
      Css.Stylesheet.with_origin Author_presentational_hint
        [
          Css.rule
            ~selector:(Css.Selector.class_ "legacy")
            [ Css.Declaration.color (hex_color "ff0000") ];
        ];
      Css.Stylesheet.with_origin Author
        [
          Css.rule
            ~selector:(Css.Selector.class_ "legacy")
            [ Css.Declaration.color (hex_color "0000ff") ];
        ];
    ]
  in
  match Css.Optimize.stylesheet input with
  | [
   Css.Stylesheet.Origin (Author_presentational_hint, [ hint_stmt ]);
   Css.Stylesheet.Origin (Author, [ author_stmt ]);
  ] ->
      let hint_rule = rule_of_statement hint_stmt in
      let author_rule = rule_of_statement author_stmt in
      Alcotest.(check string)
        "presentational hint origin stays distinct" ".legacy{color:red}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule hint_rule ]
        |> String.trim);
      Alcotest.(check string)
        "author origin stays distinct" ".legacy{color:#00f}"
        (Css.Stylesheet.to_string ~minify:true [ statement_of_rule author_rule ]
        |> String.trim)
  | _ ->
      Alcotest.fail
        "presentational hint and author origins must remain separate boundaries"

let c735_revert_layer_candidates () =
  (* CSS Cascade section 7.3.5: revert-layer rolls the cascaded value back as if
     no rules were specified in the current cascade layer for the property. The
     helper models the lower-priority candidate set used after that removal. *)
  let order = [ "base"; "components"; "theme" ] in
  let candidate layer important source_order value :
      Css.Stylesheet.cascade_layer_candidate =
    { layer; important; source_order; value }
  in
  let layer_value (c : Css.Stylesheet.cascade_layer_candidate) = c.value in
  let candidates =
    [
      candidate (Some "base") false 0 "base";
      candidate (Some "components") false 1 "components";
      candidate (Some "theme") false 2 "revert-layer";
      candidate None false 3 "unlayered";
    ]
  in
  let rolled_back =
    Css.Stylesheet.cascade_revert_layer_candidates ~layer_order:order
      ~important:false ~current_layer:(Some "theme") candidates
  in
  Alcotest.(check (list string))
    "normal revert-layer in theme can roll back to lower explicit layers"
    [ "base"; "components" ]
    (List.map layer_value rolled_back);
  let winner =
    Css.Stylesheet.winning_cascade_layer_candidate ~layer_order:order
      rolled_back
  in
  Alcotest.(check (option string))
    "normal revert-layer resolves to highest lower layer" (Some "components")
    (Option.map layer_value winner);
  let important_candidates =
    [
      candidate None true 0 "unlayered-important";
      candidate (Some "theme") true 1 "theme-important";
      candidate (Some "components") true 2 "components-important";
      candidate (Some "base") true 3 "revert-layer";
    ]
  in
  let important_rolled_back =
    Css.Stylesheet.cascade_revert_layer_candidates ~layer_order:order
      ~important:true ~current_layer:(Some "base") important_candidates
  in
  Alcotest.(check (list string))
    "important revert-layer in first layer can roll back to later important \
     layers and unlayered important"
    [ "unlayered-important"; "theme-important"; "components-important" ]
    (List.map layer_value important_rolled_back)

let c734_revert_origin_candidates () =
  (* CSS Cascade section 7.3.4: revert rolls the cascaded value back to the
     previous origin tier. Author and animation origins roll back to the user
     level; user origin rolls back to user-agent; user-agent origin behaves like
     unset because no previous origin exists. Presentational hints are treated
     as part of the author origin for revert. *)
  let candidate origin important source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important; source_order; value }
  in
  let origin_value (c : Css.Stylesheet.cascade_origin_candidate) = c.value in
  let candidates =
    [
      candidate User_agent false 0 "ua";
      candidate User false 1 "user";
      candidate Author_presentational_hint false 2 "hint";
      candidate Author false 3 "author";
      candidate Animation false 4 "animation";
    ]
  in
  let author_rollback =
    Css.Stylesheet.cascade_revert_origin_candidates ~important:false
      ~current_origin:Author candidates
  in
  Alcotest.(check (list string))
    "normal author revert exposes user and user-agent origins" [ "ua"; "user" ]
    (List.map origin_value author_rollback);
  let author_winner =
    Css.Stylesheet.winning_cascade_origin_candidate author_rollback
  in
  Alcotest.(check (option string))
    "normal author revert rolls back to user winner" (Some "user")
    (Option.map origin_value author_winner);
  let hint_rollback =
    Css.Stylesheet.cascade_revert_origin_candidates ~important:false
      ~current_origin:Author_presentational_hint candidates
  in
  Alcotest.(check (list string))
    "presentational hint revert is treated like author revert" [ "ua"; "user" ]
    (List.map origin_value hint_rollback);
  let animation_rollback =
    Css.Stylesheet.cascade_revert_origin_candidates ~important:false
      ~current_origin:Animation candidates
  in
  Alcotest.(check (list string))
    "animation revert is treated like author revert" [ "ua"; "user" ]
    (List.map origin_value animation_rollback);
  let user_rollback =
    Css.Stylesheet.cascade_revert_origin_candidates ~important:false
      ~current_origin:User candidates
  in
  Alcotest.(check (list string))
    "normal user revert exposes user-agent origin" [ "ua" ]
    (List.map origin_value user_rollback);
  let ua_rollback =
    Css.Stylesheet.cascade_revert_origin_candidates ~important:false
      ~current_origin:User_agent candidates
  in
  Alcotest.(check (list string))
    "user-agent revert has no previous origin and behaves like unset" []
    (List.map origin_value ua_rollback);
  let important_candidates =
    [
      candidate User_agent true 0 "ua-important";
      candidate User true 1 "user-important";
      candidate Author true 2 "author-important";
    ]
  in
  let important_author_rollback =
    Css.Stylesheet.cascade_revert_origin_candidates ~important:true
      ~current_origin:Author important_candidates
  in
  Alcotest.(check (list string))
    "important author revert exposes important user-agent and user origins"
    [ "ua-important"; "user-important" ]
    (List.map origin_value important_author_rollback)

let selector_merging_tests =
  [
    ("merge consecutive identical", `Quick, test_merge_consecutive_identical);
    ( "no merge different declarations",
      `Quick,
      test_no_merge_different_declarations );
    ( "merge non-consecutive non-conflicting",
      `Quick,
      test_merge_non_consecutive_non_conflicting );
    ("no merge vendor pseudo", `Quick, test_no_merge_vendor_pseudo);
    ("no merge with nested", `Quick, test_no_merge_with_nested);
    ( "spec cascade 3 shorthand resets omitted longhands",
      `Quick,
      c3_shorthand_resets );
    ( "spec cascade 3 open/closed world background synthesis",
      `Quick,
      c3_open_closed_world_background_synthesis );
    ( "spec cascade 3 shorthand source order corner cases",
      `Quick,
      c3_shorthand_order_edges );
    ( "spec cascade 3 important shorthand expands to longhands",
      `Quick,
      c3_important_shorthand_expands );
    ( "spec cascade 6.1 declaration order shorthand boundary",
      `Quick,
      c61_decl_order_shorthand_boundary );
    ( "spec cascade 6.1 adjacent merge preserves shorthand order",
      `Quick,
      c61_adjacent_shorthand_order );
    ( "spec cascade 6.1 positive adjacent merge with later dedup",
      `Quick,
      c61_adjacent_later_dedup );
    ( "spec cascade 6.1 no merge across intervening rule",
      `Quick,
      c61_no_merge_intervening );
    ( "spec cascade 6.1 no group non-adjacent equal declarations",
      `Quick,
      c61_no_group_nonadjacent );
    ( "A?B?A forbidden selector-intersection dependency",
      `Quick,
      aba_forbidden_intersection_dependency );
    ( "A?B?A allowed same-selector dead A elimination",
      `Quick,
      aba_allowed_same_selector_dead );
    ( "A?B?A runtime shorthand boundaries block contraction",
      `Quick,
      aba_runtime_shorthand_boundaries );
    ("A?B?A allowed local refactoring", `Quick, aba_allowed_local_refactoring);
    ( "spec cascade 6.1 no merge across at-rule boundary",
      `Quick,
      c61_no_merge_atrule );
    ( "spec cascade 6.1 conditional competitors keep source order",
      `Quick,
      c61_conditional_competitor_order );
    ( "target minify and enforce-spec split",
      `Quick,
      target_minify_enforce_spec_split );
    ( "spec cascade 6.1 no media merge across layer statement",
      `Quick,
      c61_no_layer_media_merge );
    ( "spec cascade 6.1 all property reset boundary",
      `Quick,
      c61_all_property_reset_boundary );
    ( "spec cascade 6.1 no factor across all reset",
      `Quick,
      c61_no_factor_across_all_reset );
    ( "spec cascade 6.1 no merge across layer boundary",
      `Quick,
      c61_no_merge_layer );
    ( "spec cascade 6.4 layer statement is ordering boundary",
      `Quick,
      c64_layer_order_boundary );
    ( "spec cascade 6.1 unlayered rule stays outside layer",
      `Quick,
      c61_unlayered_outside_layer );
    ( "spec cascade 6.1 important layer order preserved",
      `Quick,
      c61_important_layer_order );
    ( "spec cascade 6.1 style attribute boundary",
      `Quick,
      c61_style_attr_boundary );
    ( "spec cascade 6.1 adjacent different specificity grouping",
      `Quick,
      c61_adjacent_specificity_grouping );
    ( "spec cascade 6.1 specificity competitor blocks grouping",
      `Quick,
      c61_specificity_blocks_grouping );
    ( "spec cascade 6.1 no merge across scope boundary",
      `Quick,
      c61_no_merge_scope );
    ( "spec cascade 6.1 distinct scopes preserved",
      `Quick,
      c61_distinct_scopes_preserved );
    ( "spec cascade 6.1 distinct scope limits preserved",
      `Quick,
      c61_distinct_scope_limits_preserved );
    ( "spec cascade 6.1 no merge across supports boundary",
      `Quick,
      c61_no_merge_supports );
    ( "spec cascade 6.1 no merge across container boundary",
      `Quick,
      c61_no_merge_container );
    ( "spec cascade 6.1 no merge across starting-style boundary",
      `Quick,
      c61_no_merge_starting_style );
    ( "spec cascade 6.1 import keeps substitution point",
      `Quick,
      c61_import_substitution_point );
    ( "spec cascade 6.1 at-rule boundaries are opaque",
      `Quick,
      c61_no_named_atrule_merge );
    ( "spec cascade 6.1 scope/page/nested boundaries are opaque",
      `Quick,
      c61_no_nested_boundary_merge );
    ( "spec cascade 6.1 no group across pseudo competitor",
      `Quick,
      c61_no_pseudo_group );
    ( "spec cascade 6.1 conditional boundaries are opaque",
      `Quick,
      c61_no_conditional_cli_merge );
    ( "spec cascade 6.1 important shorthand blocks normal longhand",
      `Quick,
      c61_important_blocks_longhand );
    ( "spec cascade 6.3 mixed importance shorthand longhand edges",
      `Quick,
      c63_mixed_importance_edges );
    ( "spec cascade 6.2 origin importance precedence rank",
      `Quick,
      c62_origin_importance_rank );
    ( "spec cascade 6.2 no merge across author user origins",
      `Quick,
      c62_no_merge_author_user );
    ( "spec cascade 6.2 no merge across user-agent author origins",
      `Quick,
      c62_no_merge_ua_author );
    ( "spec cascade 6.2 animation transition origins preserved",
      `Quick,
      c62_animation_transition_origins );
    ( "spec cascade 6.2 optimize within single origin",
      `Quick,
      c62_optimize_single_origin );
    ( "spec cascade 6.2 identical declarations not grouped across origins",
      `Quick,
      c62_no_group_across_origins );
    ( "spec cascade 6.2 imported rules keep importing origin",
      `Quick,
      c62_imports_keep_origin );
    ( "spec cascade 6.2 origin wrapper public api",
      `Quick,
      c62_origin_wrapper_api );
    ( "spec cascade 6.3 important beats later normal",
      `Quick,
      c63_important_beats_normal );
    ( "spec cascade 6.3 later important beats earlier important",
      `Quick,
      c63_later_important_wins );
    ( "spec cascade 6.3 importance inverts origin precedence",
      `Quick,
      c63_importance_inverts_origin );
    ( "spec cascade 6.3 keyframes ignore important declarations",
      `Quick,
      c63_keyframes_ignore_important );
    ( "spec cascade 6.4 statement declares layer order",
      `Quick,
      c64_statement_layer_order );
    ( "spec cascade 6.4 redundant layer prelude removed",
      `Quick,
      c64_redundant_layer_prelude );
    ( "spec cascade 6.4 duplicate layer statement removed",
      `Quick,
      c64_redundant_layer_duplicate );
    ( "spec cascade 6.4 layer prelude order boundary",
      `Quick,
      c64_layer_prelude_order_boundary );
    ( "spec cascade 6.4 layer prelude missing name",
      `Quick,
      c64_layer_prelude_missing_name );
    ( "spec cascade 6.4 layer prelude import barrier",
      `Quick,
      c64_layer_prelude_import_barrier );
    ( "spec cascade 6.4 unlayered normal is implicit final layer",
      `Quick,
      c64_unlayered_final_layer );
    ( "spec cascade 6.4 important layers reverse order",
      `Quick,
      c64_important_layers_reverse );
    ( "spec cascade 6.4 anonymous layers are distinct",
      `Quick,
      c64_anonymous_layers_distinct );
    ( "spec cascade 6.4 nested layer name is distinct from top level",
      `Quick,
      c64_nested_layer_distinct );
    ( "spec cascade 6.4 keyframes name collisions are layered",
      `Quick,
      c64_keyframe_name_layers );
    ( "spec cascade 6.4 layer declarations do not cross imports",
      `Quick,
      c64_layer_decls_import_cross );
    ( "spec cascade 6.4 repeated named layer blocks stay ordered",
      `Quick,
      c64_repeated_layer_blocks_ordered );
    ( "spec cascade 6.4 same child layer in one anonymous parent",
      `Quick,
      c64_child_layer_one_anonymous );
    ( "spec cascade 6.4 same child layer in distinct anonymous parents",
      `Quick,
      c64_child_layer_distinct_anonymous );
    ( "spec cascade 6.4 conditional layer declarations stay nested",
      `Quick,
      c64_conditional_layer_decls_nested );
    ( "spec cascade 6.4 empty named layer before block keeps order",
      `Quick,
      c64_empty_layer_before_block );
    ("spec cascade 6.4 layer precedence api", `Quick, c64_layer_precedence_api);
    ( "spec cascade 6.5 presentational hint origin rank",
      `Quick,
      c65_presentational_hint_rank );
    ( "spec cascade 7.3.5 revert-layer candidate set",
      `Quick,
      c735_revert_layer_candidates );
    ( "spec cascade 7.3.4 revert origin candidate set",
      `Quick,
      c734_revert_origin_candidates );
  ]

let suite = ("optimize", optimize_tests @ selector_merging_tests)
