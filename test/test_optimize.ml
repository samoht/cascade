(** Tests for CSS Optimize module *)

open Alcotest
open Css.Optimize
open Css.Declaration
open Css.Values
open Css.Properties

let hex_color s = Hex { hash = true; value = s }
let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Generic check function for optimize types *)
let _check_value name pp reader ?expected input =
  let expected = Option.value ~default:input expected in
  (* First pass: parse + print equals expected (minified) *)
  let t = Css.Reader.of_string input in
  let v = reader t in
  let s = to_string pp v in
  check string (Fmt.str "%s %s" name input) expected s;
  (* Roundtrip stability: read printed output and ensure idempotent printing *)
  let t2 = Css.Reader.of_string s in
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
  (match List.hd deduped_important with
  | Declaration { important = true; _ } -> ()
  | _ -> fail "Expected !important declaration to win");

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
  (* Test -webkit-transform duplication *)
  let decls = [ v Transform [ Rotate (Deg 45.) ] ] in
  let duplicated = duplicate_buggy_properties decls in
  (* Should add -webkit-transform *)
  check bool "adds webkit prefix" true
    (List.length duplicated > List.length decls)

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
    { selector; declarations = decls; nested = [] }
  in
  let optimized = single_rule rule in

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
    { selector; declarations = [ v Color (hex_color "ff0000") ]; nested = [] }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector;
      declarations = [ v Background_color (hex_color "0000ff") ];
      nested = [];
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
    { selector = Css.Selector.class_ "a"; declarations = decls; nested = [] }
  in
  let rule2 : Css.Stylesheet.rule =
    { selector = Css.Selector.class_ "b"; declarations = decls; nested = [] }
  in
  let rule3 : Css.Stylesheet.rule =
    { selector = Css.Selector.class_ "c"; declarations = decls; nested = [] }
  in

  (* Test combine_identical_rules function *)
  let grouped = combine_identical_rules [ rule1; rule2; rule3 ] in
  check int "rules with same declarations are grouped" 1 (List.length grouped);

  (* Check that selector is a list *)
  let grouped_rule = List.hd grouped in
  check bool "grouped selector is a list" true
    (Css.Selector.is_compound_list grouped_rule.selector)

(** Test complete stylesheet optimization *)
let optimize_all () =
  let selector1 = Css.Selector.class_ "test" in
  let selector2 = Css.Selector.class_ "other" in

  let rule1 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations =
        [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ];
      nested = [];
    }
  in
  let rule2 : Css.Stylesheet.rule =
    {
      selector = selector1;
      declarations = [ v Background_color (hex_color "ffffff") ];
      nested = [];
    }
  in
  let rule3 : Css.Stylesheet.rule =
    {
      selector = selector2;
      declarations = [ v Color (hex_color "0000ff") ];
      nested = [];
    }
  in

  let stylesheet =
    [
      Css.Stylesheet.Rule rule1;
      Css.Stylesheet.Rule rule2;
      Css.Stylesheet.Rule rule3;
    ]
  in

  let optimized = optimize_stylesheet stylesheet in

  (* Should merge rule1 and rule2 since they have same selector *)
  let rule_count stmts =
    List.fold_left
      (fun acc stmt ->
        match stmt with Css.Stylesheet.Rule _ -> acc + 1 | _ -> acc)
      0 stmts
  in
  check bool "optimization reduces rule count" true
    (rule_count optimized < rule_count stylesheet)

(** Test media query optimization *)
let media_queries () =
  let selector = Css.Selector.class_ "test" in
  let rule : Css.Stylesheet.rule =
    {
      selector;
      declarations =
        [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ];
      nested = [];
    }
  in

  let media_stmt =
    Css.Stylesheet.Media ("screen", [ Css.Stylesheet.Rule rule ])
  in

  let stylesheet = [ media_stmt ] in

  let optimized = optimize_stylesheet stylesheet in

  (* Check that declarations within media queries are also deduplicated *)
  let optimized_rule =
    match List.hd optimized with
    | Css.Stylesheet.Media (_, [ Css.Stylesheet.Rule r ]) -> r
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
    }
  in

  let layer_stmt =
    Css.Stylesheet.Layer (Some "utilities", [ Css.Stylesheet.Rule rule ])
  in

  let stylesheet = [ layer_stmt ] in

  let optimized = optimize_stylesheet stylesheet in

  (* Check that layer rules are optimized *)
  match List.hd optimized with
  | Css.Stylesheet.Layer (_, [ Css.Stylesheet.Rule optimized_rule ]) ->
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

let optimize_tests =
  [
    ("deduplicate declarations", `Quick, test_deduplicate_declarations);
    ("duplicate buggy properties", `Quick, test_duplicate_buggy_properties);
    ("optimize single rule", `Quick, single_rule);
    ("merge rules", `Quick, test_merge_rules);
    ("group selectors", `Quick, test_group_selectors);
    ("optimize stylesheet", `Quick, optimize_all);
    ("optimize media queries", `Quick, media_queries);
    ("optimize layers", `Quick, layers);
  ]

let suite = ("optimize", optimize_tests)
