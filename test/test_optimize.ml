(** Optimize tests: rule, media, layer and conditional-group passes, pruning and
    registered properties. *)

open Alcotest
open Cascade
open Css.Optimize
open Css.Declaration
open Css.Properties
open Optimize_helpers

let to_string pp v = Css.Pp.to_string ~minify:true pp v

(* Helper to check if a declaration is !important *)
let is_important = Css.Declaration.is_important

(* Helper to extract color hex value from declaration string like "color:red" *)
let color_value_of_decl decl =
  let s = Css.Declaration.to_string ~minify:true decl in
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
  check string "red !important wins" "#f00" (color_value_of_decl result);

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

(* When deduplicate_declarations has nothing to remove, compose, or drop, its
   result equals the input and must be the very same physical list - the sharing
   finalize_rule relies on to keep an unchanged rule's declarations by identity.
   Two distinct custom properties are a genuine no-op; and one pass over a list
   with a real override reaches a fixed point whose re-deduplication must also
   preserve identity. *)
let test_deduplicate_declarations_physical_identity () =
  let no_op = [ custom_property "--a" "red"; custom_property "--b" "blue" ] in
  let result = deduplicate_declarations no_op in
  check bool "no-op deduplicate is structurally unchanged" true
    (List.equal Declaration.equal_declaration result no_op);
  check bool "no-op deduplicate preserves physical identity" true
    (result == no_op);
  let overriding =
    [ v Color (hex_color "ff0000"); v Color (hex_color "0000ff") ]
  in
  let canon = deduplicate_declarations overriding in
  let again = deduplicate_declarations canon in
  check bool "re-deduplicating a fixed point preserves physical identity" true
    (again == canon)

(** Test buggy property duplication *)
let test_duplicate_buggy_properties () =
  (* Test -webkit-text-decoration:inherit compatibility. Note: Transform is NOT
     duplicated in Tailwind v4 - they don't emit vendor-prefixed transform. *)
  let decls = [ v Webkit_text_decoration Inherit ] in
  let duplicated = duplicate_buggy_properties decls in
  check (list string) "keeps one webkit-text-decoration inherit fallback"
    [ "-webkit-text-decoration:inherit" ]
    (List.map (Css.Declaration.to_string ~minify:true) duplicated);

  (* Synthesizing -webkit-text-decoration-color is the target-driven layer's
     decision, taken against the declared browser versions;
     target_evergreen_compatibility_prefixes covers it. This pass carries no
     targets, so it duplicates only the properties WebKit reads wrongly and
     leaves authored prefixed and unprefixed spellings distinct. *)
  let pp_decls decls =
    List.map (Css.Declaration.to_string ~minify:true) decls
  in
  let standard_color =
    duplicate_buggy_properties [ v Text_decoration_color (hex_color "0000ff") ]
  in
  check (list string) "the target-less pass does not prefix a decoration color"
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
    [ "-webkit-text-decoration-color:#f00"; "text-decoration-color:#00f" ]
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
    Css.Stylesheet.Layer (Some [ "utilities" ], [ statement_of_rule rule ])
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
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule2 ];
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
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule1 ];
      statement_of_rule rule2;
      (* Separator *)
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule3 ];
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
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(media_min_width 64.) [ statement_of_rule rule2 ];
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
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule2 ];
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule3 ];
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
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule1 ];
      Css.media ~condition:(media_min_width 48.) [ statement_of_rule rule2 ];
    ]
  in

  let stylesheet =
    [ Css.Stylesheet.Layer (Some [ "utilities" ], layer_content) ]
  in

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
      Css.Stylesheet.Layer (Some [ "reset" ], []);
      Css.Stylesheet.Layer (Some [ "theme" ], []);
      Css.Stylesheet.Layer
        ( Some [ "components" ],
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
      Css.Stylesheet.Layer (Some [ "components" ], []);
      Css.Stylesheet.Layer (Some [ "utilities" ], []);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "empty components/utilities collapse to one layer statement"
    "@layer components,utilities;" output

(* A rule that writes no declarations of its own but nests rules that do is a
   rule with contents, so the layer holding it is not an empty layer. Collapsing
   it to the statement form deleted every declaration below the brace. *)
let test_layer_keeps_nested_only_rule () =
  let cases =
    [
      ( "a rule nesting two rules",
        "@layer a{.x{.y{color:red}.z{margin:0}}}",
        "@layer a{.x{.y{color:red}.z{margin:0}}}" );
      ( "a selector list nesting a rule",
        "@layer a{.w,.x{.y{color:red}}}",
        "@layer a{.w,.x{.y{color:red}}}" );
      ( "a rule nesting an at-rule",
        "@layer a{.x{@media print{color:red}}}",
        "@layer a{.x{@media print{color:red}}}" );
    ]
  in
  List.iter
    (fun (name, input, expected) ->
      let optimized =
        Css.Optimize.stylesheet (Css.Stylesheet.read (Cursor.of_string input))
      in
      Alcotest.(check string)
        name expected
        (Css.Stylesheet.to_string ~minify:true optimized |> String.trim))
    cases

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
    "adjacent same-condition blocks merge inside the utility layer"
    (* Source order is preserved: generator order is a public contract for
       Tailwind utilities even when disjoint properties would commute. *)
    "@layer \
     utilities{@supports(display:grid){.grid{display:grid}.gap{gap:1rem}}@container(inline-size>30em){.wide{display:block}.pad{padding:1rem}}}"
    output

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
    "default minify keeps non-adjacent supports split"
    "@layer \
     utilities{@media(width>=48rem){.md\\:flex{display:flex}}.flex{display:flex}@media(width>=48rem){.md\\:grid{display:grid}}@supports(display:grid){.grid{display:grid}}.block{display:block}@supports(display:grid){.gap{gap:1rem}}}"
    output;
  let spec = Css.Optimize.stylesheet ~enforce_spec:true input in
  let spec_output = Css.Stylesheet.to_string ~minify:true spec |> String.trim in
  Alcotest.(check string)
    "enforce-spec keeps non-adjacent supports split inside utility layer"
    "@layer \
     utilities{@media(min-width:48rem){.md\\:flex{display:flex}}.flex{display:flex}@media(min-width:48rem){.md\\:grid{display:grid}}@supports(display:grid){.grid{display:grid}}.block{display:block}@supports(display:grid){.gap{gap:1rem}}}"
    spec_output

let test_supports_author_guard_kept_by_default () =
  (* CSS Conditional 5 sec. 2 evaluates an @supports condition in the UA that
     renders the sheet, so the guarded block is the author's enhancement and
     whatever it does not cover is the fallback. Deciding the condition at build
     time deletes that fallback path, which is the construct's only purpose, so
     the default optimize keeps every author guard. *)
  let minify css =
    Css.Optimize.stylesheet (Css.Stylesheet.read (Cursor.of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "a widely-available feature is still a guard"
    "@supports(display:grid){.a{display:grid}}"
    (minify "@supports (display:grid){.a{display:grid}}");
  (* The condition is a feature test, not a colour. Folding the probe to
     [color:red] would ask a question every UA answers yes to. *)
  Alcotest.(check string)
    "a color-mix probe keeps its condition unfolded"
    "@supports(color:color-mix(in lab,red,red)){.a{color:#00f}}"
    (minify "@supports (color:color-mix(in lab, red, red)){.a{color:blue}}");
  (* [not <supports-in-parens>] is itself a <supports-condition>, so the outer
     parens drop; the space before [(] stays, since [not(] lexes as a function
     token. *)
  Alcotest.(check string)
    "a negated guard selects the UAs the fallback is for"
    "@supports not (display:grid){.a{display:flex}}"
    (minify "@supports (not (display:grid)){.a{display:flex}}")

let test_supports_entailed_by_its_context () =
  (* CSS Conditional 3 sec. 6 evaluates a condition as a two-valued boolean over
     its feature tests, so a condition is a propositional formula and classical
     entailment holds over it. Inside @supports (A) the UA has already answered
     A, and one sheet gets the same answer everywhere in it, so an inner
     condition simplifies against the conjunction K of its enclosing conditions
     by propositional logic alone. K and not C unsatisfiable means C is entailed
     and its guard goes; K and C unsatisfiable means C is refuted and its block
     is dead. No support table is consulted, so this holds whatever the UA
     answers. Atom identity is syntactic: two feature tests are the same
     variable when they are structurally equal after parsing. *)
  let minify css =
    Css.Optimize.stylesheet (Css.Stylesheet.read (Cursor.of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  (* The boundary first. A disjunction does not entail its disjuncts: a UA
     answering yes to (B) and no to (A) satisfies the context and still has to
     be asked about (A). *)
  Alcotest.(check string)
    "a disjunctive context entails neither disjunct"
    "@supports(nonsense-a:1px)or \
     (nonsense-b:2px){@supports(nonsense-a:1px){.a{top:0}}}"
    (minify
       "@supports (nonsense-a:1px) or (nonsense-b:2px){@supports \
        (nonsense-a:1px){.a{top:0}}}");
  (* Two feature tests on one property are still two variables: accepting
     display:grid does not imply accepting display:inline-grid. *)
  Alcotest.(check string)
    "two tests on one property are two variables"
    "@supports(display:grid){@supports(display:inline-grid){.a{top:0}}}"
    (minify
       "@supports (display:grid){@supports (display:inline-grid){.a{top:0}}}");
  Alcotest.(check string)
    "a repeated guard asks a question already answered"
    "@supports(nonsense-a:1px){.a{top:0}}"
    (minify "@supports (nonsense-a:1px){@supports (nonsense-a:1px){.a{top:0}}}");
  (* A conjunction entails each conjunct, and operand order is not part of the
     question: (A) and (B) has the truth table of (B) and (A). *)
  Alcotest.(check string)
    "a conjunction entails its conjuncts in either order"
    "@supports(nonsense-a:1px)and (nonsense-b:2px){.a{top:0}}"
    (minify
       "@supports (nonsense-a:1px) and (nonsense-b:2px){@supports \
        (nonsense-b:2px) and (nonsense-a:1px){.a{top:0}}}");
  (* A disjunct entails its disjunction, so (B) is never asked. *)
  Alcotest.(check string)
    "a disjunct entails its disjunction" "@supports(nonsense-a:1px){.a{top:0}}"
    (minify
       "@supports (nonsense-a:1px){@supports (nonsense-a:1px) or \
        (nonsense-b:2px){.a{top:0}}}");
  (* A and not A is false on every UA, so the inner block never applies and the
     enclosing block is left empty. *)
  Alcotest.(check string)
    "a guard negating its context is dead" ""
    (minify
       "@supports (nonsense-a:1px){@supports (not (nonsense-a:1px)){.a{top:0}}}");
  (* Neither entailed nor refuted: what K leaves to ask is (B). *)
  Alcotest.(check string)
    "a narrowing guard keeps only its residual"
    "@supports(nonsense-a:1px){@supports(nonsense-b:2px){.a{top:0}}}"
    (minify
       "@supports (nonsense-a:1px){@supports (nonsense-a:1px) and \
        (nonsense-b:2px){.a{top:0}}}");
  (* Knowing the property changes nothing: the transform reads the condition's
     shape, not a support table. *)
  Alcotest.(check string)
    "a property cascade models behaves the same"
    "@supports(display:grid){.a{top:0}}"
    (minify "@supports (display:grid){@supports (display:grid){.a{top:0}}}")

(* Optimize must preserve physical identity when there is nothing left to do.
   Optimizing once reaches a fixed point [canon]; a second pass changes nothing,
   so it must return the very same value ([==]) rather than a structurally-equal
   copy. This is the unit-level companion to the fuzzer's sharing invariant. *)
let test_optimize_preserves_physical_identity () =
  let fixpoint css =
    match Css.of_string ~strict:false css with
    | Ok p -> Css.optimize p.stylesheet
    | Error _ -> Alcotest.failf "parse failed: %s" css
  in
  let cases =
    [
      ".a{color:red}";
      ".a{color:red}.b{display:block}";
      "@media screen{.a{color:red}}";
      "@layer base{.a{margin:0}}";
      ".a{color:red;background:#fff}";
    ]
  in
  List.iter
    (fun css ->
      let canon = fixpoint css in
      let again = Css.optimize canon in
      Alcotest.(check bool)
        (css ^ ": optimize returns the same value when nothing changes")
        true (again == canon))
    cases

(* Each level presents one already-optimized nested [@media] block beside a
   second block with the same condition. Merging the pair must optimize the new
   seam without recursively walking the first block a second time: doing so at
   every level makes a linear-size stylesheet take quadratic work. *)
let test_nested_media_merge_is_linear () =
  let sheet depth =
    let b = Buffer.create ((depth * 48) + 16) in
    let out = Fmt.with_buffer b in
    for _ = 1 to depth do
      Buffer.add_string b "@media all{"
    done;
    Buffer.add_string b "a{color:red}";
    for i = depth downto 1 do
      Fmt.pf out "}@media all{.x%d{color:red}}" i
    done;
    match Css.of_string ~strict:false (Buffer.contents b) with
    | Ok p -> p.stylesheet
    | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
  in
  let measure stylesheet =
    Css_test_helpers.measure (fun () -> Css.optimize stylesheet)
  in
  let small_words = measure (sheet 256) in
  let large_words = measure (sheet 512) in
  let ratio = large_words /. small_words in
  Alcotest.(check bool)
    (Fmt.str "alloc %.0f -> %.0f (%.2fx for 2x depth)" small_words large_words
       ratio)
    true (ratio < 3.)

let test_prune_unused_custom_props () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok p -> p.stylesheet
    | Error _ -> Alcotest.fail "parse"
  in
  let opt ?(prune = false) css =
    parse css
    |> Css.optimize ~prune_unused_custom_props:prune
    |> Css.to_string ~minify:true
  in
  let dead = ":root{--spacing:.25rem}.top-0{top:0}" in
  Alcotest.(check string)
    "default keeps an unreferenced binding"
    ":root{--spacing:.25rem}.top-0{top:0}" (opt dead);
  Alcotest.(check string)
    "opt-in drops an unreferenced binding" ".top-0{top:0}"
    (opt ~prune:true dead);
  Alcotest.(check string)
    "opt-in keeps a referenced binding"
    ":root{--spacing:.25rem}.gap{gap:var(--spacing)}"
    (opt ~prune:true ":root{--spacing:.25rem}.gap{gap:var(--spacing)}");
  (* A [var()] inside an opaque custom-property value still counts, so the
     binding it depends on is not pruned. *)
  Alcotest.(check bool)
    "opt-in keeps a binding referenced only inside an opaque value" true
    (Astring.String.is_infix ~affix:"--spacing"
       (opt ~prune:true
          ":root{--spacing:.25rem;--shadow:0 0 \
           var(--spacing)}.x{box-shadow:var(--shadow)}"));
  (* A [var()] inside a string literal is data, not a reference (recognised
     structurally, not by text), so a binding referenced only there is
     pruned. *)
  Alcotest.(check string)
    "opt-in: a var() inside a string is not a reference"
    ".a{width:var(--x)}:root{--x:\"var(--y)\"}"
    (opt ~prune:true ".a{width:var(--x)}:root{--x:\"var(--y)\";--y:1px}");
  (* An at-rule that holds declarations outside a nested block references a
     binding like any rule does, so pruning must leave such a sheet alone. *)
  let unchanged name css =
    Alcotest.(check string) name (opt css) (opt ~prune:true css)
  in
  unchanged "a keyframe frame reference keeps its binding"
    ":root{--c:red}@keyframes k{from{color:var(--c)}}";
  unchanged "a @page reference keeps its binding"
    ":root{--m:1cm}@page{margin:var(--m)}";
  unchanged "a page margin box reference keeps its binding"
    ":root{--t:\"x\"}@page{@top-center{content:var(--t)}}";
  unchanged "a @position-try reference keeps its binding"
    ":root{--t:10px}@position-try --p{top:var(--t)}";
  unchanged "a @supports-condition reference keeps its binding"
    ":root{--c:red}@supports-condition --x{color:var(--c)}"

(* [drop_invalid] is spec recovery rather than optimisation: a browser discards
   a declaration whose value it cannot parse wherever that declaration sits, so
   a [@keyframes] frame is no different from a style rule. *)
let test_drop_invalid_reaches_keyframe_frames () =
  let recovered css =
    Css.of_string_exn css |> Css.Optimize.drop_invalid
    |> Css.Stylesheet.to_string ~minify:true
  in
  Alcotest.(check string)
    "a style rule drops the invalid declaration" "a{opacity:0}"
    (recovered "a{width:asin(sin(45deg));opacity:0}");
  Alcotest.(check string)
    "every length-percentage property drops an invalid value" "a{opacity:0}"
    (recovered
       "a{width:asin(sin(45deg));shape-margin:asin(sin(45deg));offset-distance:asin(sin(45deg));opacity:0}");
  Alcotest.(check string)
    "a keyframe frame drops it too" "@keyframes k{0%{opacity:0}}"
    (recovered "@keyframes k{from{width:asin(sin(45deg));opacity:0}}");
  Alcotest.(check string)
    "so does a vendor-prefixed one" "@-webkit-keyframes k{0%{opacity:0}}"
    (recovered "@-webkit-keyframes k{from{width:asin(sin(45deg));opacity:0}}")

(* Under closed-stylesheet scope a [position-try-fallbacks] name with no
   [@position-try] rule can never match, and where the declaration is written
   does not change that, so the prune reaches a keyframe frame as it does a
   style rule. *)
let test_position_try_prune_reaches_keyframe_frames () =
  let pruned css =
    Css.of_string_exn css
    |> Css.Optimize.stylesheet ~scope:`Stylesheet
    |> Css.Stylesheet.to_string ~minify:true
  in
  Alcotest.(check string)
    "a style rule loses the unknown fallback"
    "@position-try --k{top:1px}a{position-try-fallbacks:--k}"
    (pruned "@position-try --k{top:1px}a{position-try-fallbacks:--k,--gone}");
  Alcotest.(check string)
    "a keyframe frame loses it too"
    "@position-try --k{top:1px}@keyframes f{0%{position-try-fallbacks:--k}}"
    (pruned
       "@position-try --k{top:1px}@keyframes \
        f{from{position-try-fallbacks:--k,--gone}}")

(* Every conditional group wraps ordinary rules, so the optimizer's main
   recursion has to descend into all of them: a body it walks past keeps
   whatever the author wrote, and [--minify] silently does nothing there. *)
let test_optimize_descends_into_every_conditional_group () =
  let check name css expected =
    Alcotest.(check string) name expected (minify (Css.of_string_exn css))
  in
  check "@media" "@media print{a{color:#f00}a{color:#f00}}"
    "@media print{a{color:red}}";
  check "@-moz-document"
    "@-moz-document url-prefix(){a{color:#f00}a{color:#f00}}"
    "@-moz-document url-prefix(){a{color:red}}";
  check "@starting-style" "@starting-style{a{color:#f00}a{color:#f00}}"
    "@starting-style{a{color:red}}";
  check "@when and @else"
    "@when \
     media(print){a{color:#f00}a{color:#f00}}@else{b{color:#f00}b{color:#f00}}"
    "@when media(print){a{color:red}}@else{b{color:red}}"

(* CSS Transitions 2 sec. 3.3: a [@starting-style] rule carries no condition, so
   a run of adjacent blocks holds the same starting styles, in the same order,
   as one block over their concatenation - the argument the conditional groups
   beside it make once their conditions are known equal. *)
let test_merge_consecutive_starting_style () =
  let check name css expected =
    Alcotest.(check string) name expected (minify (Css.of_string_exn css))
  in
  check "@media merges an adjacent run"
    "@media print{.a{opacity:0}}@media print{.b{opacity:1}}"
    "@media print{.a{opacity:0}.b{opacity:1}}";
  check "@supports merges an adjacent run"
    "@supports (display:grid){.a{opacity:0}}@supports \
     (display:grid){.b{opacity:1}}"
    "@supports(display:grid){.a{opacity:0}.b{opacity:1}}";
  check "@starting-style merges an adjacent run"
    "@starting-style{.a{opacity:0}}@starting-style{.b{opacity:1}}"
    "@starting-style{.a{opacity:0}.b{opacity:1}}";
  check "three in a row collapse to one"
    "@starting-style{.a{opacity:0}}@starting-style{.b{opacity:1}}@starting-style{.c{opacity:.5}}"
    "@starting-style{.a{opacity:0}.b{opacity:1}.c{opacity:.5}}";
  (* Adjacency-only, like the four passes beside it: reaching a block further
     down means hoisting it over the statements between, which is the analysis
     [merge_distant_media] carries and this pass does not. *)
  check "a rule between two blocks keeps them apart"
    "@starting-style{.a{opacity:0}}.c{opacity:1}@starting-style{.b{opacity:1}}"
    "@starting-style{.a{opacity:0}}.c{opacity:1}@starting-style{.b{opacity:1}}"

(* An [@property] registration is document-global (CSS Properties and Values API
   1 sec. 2), so a declaration of that name is typed wherever it is written.
   Promotion has to reach the at-rules that hold declarations outside a nested
   block, or the same registered value canonicalises one way in a rule and
   another inside one of them. *)
let test_promote_registered_reaches_at_rule_declarations () =
  let registration =
    "@property --c{syntax:\"<color>\";inherits:false;initial-value:red}"
  in
  let check name body expected =
    Alcotest.(check string)
      name (registration ^ expected)
      (minify (Css.of_string_exn ~strict:false (registration ^ body)))
  in
  check "a style rule types the declaration" "a{--c:rgb(255 0 0)}" "a{--c:red}";
  check "@position-try types it too" "@position-try --p{--c:rgb(255 0 0)}"
    "@position-try --p{--c:red}";
  check "@supports-condition types it too"
    "@supports-condition --x{--c:rgb(255 0 0)}"
    "@supports-condition --x{--c:red}"

(* CSS Fonts 4 sec. 2.1.1 spells one [<family-name>] either as a [<string>] or
   as a [<custom-ident>+], but the two only name the same family in a
   font-family position. No registration reaches that position: [<family-name>]
   is not among the syntax component names CSS Properties and Values API 1 (ED)
   sec. 5.1 supports, so [<custom-ident>+] is the generic ident-sequence grammar
   and a quoted string does not match it. The declaration is then invalid at
   computed-value time (ED sec. 2.4) and computes to the registration's initial
   value, which unquoting it would replace with the name itself. *)
let test_promote_registered_keeps_unmatched_string_quoted () =
  let check name registration body expected =
    Alcotest.(check string)
      name (registration ^ expected)
      (minify (Css.of_string_exn ~strict:false (registration ^ body)))
  in
  let idents =
    "@property \
     --a{syntax:\"<custom-ident>+\";inherits:false;initial-value:fallbackname}"
  in
  check "a multi-word string stays quoted" idents ":root{--a:\"Segoe UI\"}"
    ":root{--a:\"Segoe UI\"}";
  check "a single-word string stays quoted" idents ":root{--a:\"Helvetica\"}"
    ":root{--a:\"Helvetica\"}";
  check "the ident sequence the registration accepts stays bare" idents
    ":root{--a:Segoe UI}" ":root{--a:Segoe UI}";
  (* [<string>] is a registrable component of its own (ED sec. 5.1), so under
     this alternation both spellings match and each computes to the arm it
     names: a [content] use reads the quoted one back as text and the bare one
     not at all. *)
  let alternation =
    "@property \
     --b{syntax:\"<string>|<custom-ident>+\";inherits:false;initial-value:fallbackname}"
  in
  check "an alternation keeps the string arm" alternation
    ":root{--b:\"Segoe UI\"}" ":root{--b:\"Segoe UI\"}";
  (* A value that does match its registration is still promoted, so no reading
     of this test is satisfied by switching the pass off. *)
  let color =
    "@property --c{syntax:\"<color>\";inherits:false;initial-value:red}"
  in
  check "a matching value is still typed-promoted" color
    ":root{--c:rgb(255 0 0)}" ":root{--c:red}"

(* CSS Properties and Values API 1 (ED) sec. 2.3 reads an [initial-value]
   against the registration's syntax, and a [<length>] accepts a unitless zero
   for the length it names, so the registration takes the shorter [0] a
   declaration of that type takes. Measured in headless Chromium on 2026-09-14:
   [initial-value: 0] and [initial-value: 0px] register the same [<length>] and
   both compute to [0px], while a bare [1] is refused. A zero [<percentage>] or
   [<time>] keeps its unit, because a bare [0] is not one of those. *)
let test_registered_zero_length_initial_value () =
  let check name input expected =
    Alcotest.(check string)
      name expected
      (minify (Css.of_string_exn ~strict:false input))
  in
  let registration syntax value =
    String.concat ""
      [
        "@property --z{syntax:\"";
        syntax;
        "\";inherits:false;initial-value:";
        value;
        "}";
      ]
  in
  check "a <length> zero drops its unit"
    (registration "<length>" "0px")
    (registration "<length>" "0");
  check "a <length-percentage> zero length drops its unit"
    (registration "<length-percentage>" "0px")
    (registration "<length-percentage>" "0");
  check "a <percentage> zero keeps its %"
    (registration "<percentage>" "0%")
    (registration "<percentage>" "0%");
  check "a <time> zero keeps its unit"
    (registration "<time>" "0s")
    (registration "<time>" "0s");
  check "a non-zero <length> is untouched"
    (registration "<length>" "1px")
    (registration "<length>" "1px")

(* A colour function carrying a var() is a pending-substitution value (CSS
   Variables L1 section 3): its arity and legacy/modern separator style aren't
   known until substitution, so minify+optimize must keep it verbatim - never
   dropped, never re-spelled (a comma-list var like [--bs-*-rgb: 255, 255, 255]
   only stays valid in the legacy comma form). *)
let test_var_color_functions_preserved () =
  let opt css =
    match Css.of_string css with
    | Ok { Css.stylesheet; _ } -> minify stylesheet
    | Error _ -> Alcotest.failf "expected %s to parse, not be dropped" css
  in
  let same css = Alcotest.(check string) css css (opt css) in
  same ".x{color:rgba(var(--rgb),var(--op))}";
  same ".x{background-color:rgba(var(--rgb),var(--op))}";
  same ".x{border-color:rgba(var(--rgb),var(--op))}";
  same ".x{outline-color:rgba(var(--rgb),var(--op))}";
  (* text-decoration-color also takes the WebKit alias at the declared targets,
     which target_evergreen_compatibility_prefixes owns. The pending value is
     what has to survive, and it survives on both spellings. *)
  Alcotest.(check string)
    "a decoration colour keeps its pending value"
    ".x{-webkit-text-decoration-color:rgba(var(--rgb),var(--op));text-decoration-color:rgba(var(--rgb),var(--op))}"
    (opt ".x{text-decoration-color:rgba(var(--rgb),var(--op))}");
  same ".x{caret-color:rgba(var(--rgb),var(--op))}";
  same ".x{fill:rgba(var(--rgb),var(--op))}";
  same ".x{stroke:rgba(var(--rgb),var(--op))}";
  same ".x{color:hsla(var(--hsl),var(--a))}";
  same ".x{color:rgba(var(--rgb),.5)}";
  (* nested in a gradient stop, a shadow colour, a border shorthand *)
  same
    ".x{background:linear-gradient(90deg,rgba(var(--rgb),var(--op)),transparent)}";
  same ".x{box-shadow:0 0 4px rgba(var(--rgb),var(--op))}";
  same ".x{text-shadow:0 1px rgba(var(--rgb),var(--op))}";
  same ".x{border:1px solid rgba(var(--rgb),var(--op))}";
  (* a var() in any channel slot must never be read as 0 and folded to black *)
  same ".x{color:rgb(var(--r) var(--g) var(--b))}";
  same ".x{color:rgb(255 var(--g) 0)}";
  same ".x{color:rgb(var(--rgb)/var(--op))}";
  (* the rgb() wrapper around a whole-channels var must survive: a bare
     [var(--x)] is a different value and renders black *)
  same ".x{color:rgb(var(--rgb))}";
  same ".x{color:rgb(var(--x,1 2 3))}";
  Alcotest.(check string)
    "distinct channel vars canonicalise to the space form, not black"
    ".x{color:rgb(var(--r) var(--g) var(--b))}"
    (opt ".x{color:rgb(var(--r),var(--g),var(--b))}");
  (* a sub-byte fractional channel stays verbatim, not floored to black *)
  same ".x{color:rgb(.5 0 0)}";
  (* guard: fully static colours still fold *)
  Alcotest.(check string)
    "static rgb still folds to hex" ".x{color:#fff}"
    (opt ".x{color:rgb(255,255,255)}");
  Alcotest.(check string)
    "static rgb still folds to a keyword" ".x{color:red}"
    (opt ".x{color:rgb(255 0 0)}");
  Alcotest.(check string)
    "a none channel still folds (none computes to 0)" ".x{color:#000}"
    (opt ".x{color:rgb(none 0 0)}")

(* [0], [0px], [0%] and the [left]/[top] keywords all name the same origin, so
   the optimiser folds every zero transform-origin component to the unitless
   [0]; a non-zero origin is left alone. *)
let test_transform_origin_zero_folds () =
  let opt css =
    match Css.of_string css with
    | Ok { Css.stylesheet; _ } -> minify stylesheet
    | Error _ -> Alcotest.failf "expected %s to parse" css
  in
  let same canonical input =
    Alcotest.(check string) input canonical (opt input)
  in
  same ".x{transform-origin:0}" ".x{transform-origin:0% 50%}";
  same ".x{transform-origin:0}" ".x{transform-origin:0px 50%}";
  same ".x{transform-origin:0}" ".x{transform-origin:left center}";
  same ".x{transform-origin:0 0}" ".x{transform-origin:0% 0px}";
  same ".x{transform-origin:50%}" ".x{transform-origin:50% 50%}";
  same ".x{transform-origin:10px 20px}" ".x{transform-origin:10px 20px}"

let suite =
  ( "optimize",
    [
      ( "a registered zero length drops its unit",
        `Quick,
        test_registered_zero_length_initial_value );
      ( "var() colour functions preserved",
        `Quick,
        test_var_color_functions_preserved );
      ( "transform-origin zero components fold to 0",
        `Quick,
        test_transform_origin_zero_folds );
      ( "optimize preserves physical identity on a fixed point",
        `Quick,
        test_optimize_preserves_physical_identity );
      ( "nested media merges take linear work",
        `Quick,
        test_nested_media_merge_is_linear );
      ("deduplicate declarations", `Quick, test_deduplicate_declarations);
      ( "opt-in prune unused custom properties",
        `Quick,
        test_prune_unused_custom_props );
      ( "drop_invalid reaches keyframe frames",
        `Quick,
        test_drop_invalid_reaches_keyframe_frames );
      ( "position-try prune reaches keyframe frames",
        `Quick,
        test_position_try_prune_reaches_keyframe_frames );
      ( "optimize descends into every conditional group",
        `Quick,
        test_optimize_descends_into_every_conditional_group );
      ( "merge consecutive starting-style",
        `Quick,
        test_merge_consecutive_starting_style );
      ( "promote registered reaches at-rule declarations",
        `Quick,
        test_promote_registered_reaches_at_rule_declarations );
      ( "promote registered keeps an unmatched string quoted",
        `Quick,
        test_promote_registered_keeps_unmatched_string_quoted );
      ( "deduplicate declarations preserves physical identity",
        `Quick,
        test_deduplicate_declarations_physical_identity );
      ("duplicate buggy properties", `Quick, test_duplicate_buggy_properties);
      ("optimize single rule", `Quick, single_rule);
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
      ( "layer keeps a rule that only nests",
        `Quick,
        test_layer_keeps_nested_only_rule );
      ( "tailwind conditionals merge inside layer",
        `Quick,
        test_tw_conditionals_layer );
      ( "tailwind non-adjacent conditionals in layer stay split",
        `Quick,
        test_tw_conditionals_split );
      ( "author supports guards survive the default minify",
        `Quick,
        test_supports_author_guard_kept_by_default );
      ( "a supports condition simplifies against the conditions enclosing it",
        `Quick,
        test_supports_entailed_by_its_context );
    ] )
