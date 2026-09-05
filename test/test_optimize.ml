(** Tests for CSS Optimize module *)

open Alcotest
open Cascade
open Css.Optimize
open Css.Declaration
open Css.Values
open Css.Properties

let hex_color s = Css.Values.hex s
let to_string pp v = Css.Pp.to_string ~minify:true pp v

let media_min_width px : Css.Media.t =
  Css.Media.Cond
    (Css.Media.Feature
       (Css.Media.Plain (Css.Media.Min Css.Media.Width, Css.Media.Length (Px px))))

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

(* Adjacent identical-body rules must merge even on a sheet whose factoring
   preflight predicts too little gain to run the global scheduler: the merge is
   a local always-on rewrite. The filler rules share nothing, so the only
   predicted gain is the one duplicate body, far below the preflight
   threshold. *)
let test_adjacent_merge_survives_preflight_skip () =
  let filler =
    List.init 1200 (fun i ->
        String.concat ""
          [
            ".r"; string_of_int i; "{margin-left:"; string_of_int (i + 1); "px}";
          ])
  in
  let css =
    String.concat ""
      (filler @ [ ".flex-grow{flex-grow:1}"; ".grow{flex-grow:1}" ])
  in
  match Css.of_string css with
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
  | Ok { Css.stylesheet; _ } ->
      let out = minify stylesheet in
      Alcotest.(check bool)
        "adjacent duplicate bodies merged on a preflight-skipped sheet" true
        (Astring.String.is_infix ~affix:".flex-grow,.grow{flex-grow:1}" out)

let test_vendor_prefix_strip () =
  let opt ?(enforce_spec = false) css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~enforce_spec p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* Default targets evergreen browsers: a vendor prefix whose unprefixed twin
     is present with the same value is redundant and dropped. *)
  Alcotest.(check string)
    "box-sizing prefix drops by default" ".a{box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:border-box;box-sizing:border-box}");
  (* --enforce-spec drops the evergreen target: every prefix is kept. *)
  Alcotest.(check string)
    "enforce-spec keeps every prefix"
    ".a{-moz-box-sizing:border-box;box-sizing:border-box}"
    (opt ~enforce_spec:true
       ".a{-moz-box-sizing:border-box;box-sizing:border-box}");
  (* A differing value is not a redundant twin. *)
  Alcotest.(check string)
    "differing value kept"
    ".a{-moz-box-sizing:content-box;box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:content-box;box-sizing:border-box}")

(* The evergreen-target vendor drop is a Baseline question, not a hand-kept pair
   list: a prefixed declaration is dead when its unprefixed twin sits in the
   same rule with the same value and importance AND that unprefixed property is
   Baseline "widely available". [Baseline.greenfield_properties] holds the
   properties that are not, which is exactly the set whose prefix a maintained
   browser may still need. *)
let test_vendor_prefix_baseline_gate () =
  let opt ?targets ?(enforce_spec = false) css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true
          (Css.optimize ?targets ~enforce_spec p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  let chrome_120 = { Css.Optimize.evergreen_targets with chrome = (120, 0) } in
  (* Widely available: every maintained browser reads the unprefixed form, so
     the WebKit copy is dead weight. *)
  Alcotest.(check string)
    "text-decoration-color prefix drops" ".a{text-decoration-color:red}"
    (opt ".a{-webkit-text-decoration-color:red;text-decoration-color:red}");
  Alcotest.(check string)
    "mask-image prefix drops" ".a{mask-image:none}"
    (opt ~targets:chrome_120 ".a{-webkit-mask-image:none;mask-image:none}");
  (* box-sizing is the same rule and already collapses; keep it pinned. *)
  Alcotest.(check string)
    "webkit box-sizing prefix drops" ".a{box-sizing:border-box}"
    (opt ".a{-webkit-box-sizing:border-box;box-sizing:border-box}");
  Alcotest.(check string)
    "moz box-sizing prefix drops" ".a{box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:border-box;box-sizing:border-box}");
  (* Not widely available: [backdrop-filter] and [user-select] are both in
     [Baseline.greenfield_properties], so their prefix is still load-bearing
     (Safari reads only -webkit-backdrop-filter up to 17.6) and stays. *)
  Alcotest.(check string)
    "backdrop-filter prefix kept"
    ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}"
    (opt ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}");
  Alcotest.(check string)
    "user-select prefix kept" ".a{-webkit-user-select:none;user-select:none}"
    (opt ".a{-webkit-user-select:none;user-select:none}");
  (* --enforce-spec drops the evergreen target, so every prefix survives. *)
  Alcotest.(check string)
    "enforce-spec keeps the text-decoration-color prefix"
    ".a{-webkit-text-decoration-color:red;text-decoration-color:red}"
    (opt ~enforce_spec:true
       ".a{-webkit-text-decoration-color:red;text-decoration-color:red}");
  Alcotest.(check string)
    "enforce-spec keeps the mask-image prefix"
    ".a{-webkit-mask-image:none;mask-image:none}"
    (opt ~enforce_spec:true ".a{-webkit-mask-image:none;mask-image:none}");
  Alcotest.(check string)
    "enforce-spec keeps the box-sizing prefix"
    ".a{-webkit-box-sizing:border-box;box-sizing:border-box}"
    (opt ~enforce_spec:true
       ".a{-webkit-box-sizing:border-box;box-sizing:border-box}");
  Alcotest.(check string)
    "enforce-spec keeps the backdrop-filter prefix"
    ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}"
    (opt ~enforce_spec:true
       ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}");
  (* A twin only supersedes at the same value and the same importance, whatever
     the Baseline status of the property. *)
  Alcotest.(check string)
    "differing value keeps the prefix"
    ".a{-webkit-text-decoration-color:red;text-decoration-color:green}"
    (opt ".a{-webkit-text-decoration-color:red;text-decoration-color:green}");
  Alcotest.(check string)
    "differing importance keeps the prefix"
    ".a{-moz-box-sizing:border-box!important;box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:border-box!important;box-sizing:border-box}")

(* A colour-valued property folds its colour whatever its name: CIE Lab
   L=1.90334 a=0.278696 b=-5.48866 is sRGB 3,7,18, so both spellings print the
   same hex. A property left out of the fold reports the two as different
   values, which is what the browser-differential harness then counts as a
   render change. *)
let test_color_property_folds () =
  let opt css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun property ->
      let rule value = String.concat "" [ ".a{"; property; ":"; value; "}" ] in
      Alcotest.(check string)
        (String.concat "" [ property; " folds lab() to hex" ])
        (rule "#030712")
        (opt (rule "lab(1.90334 0.278696 -5.48866)"));
      Alcotest.(check string)
        (String.concat "" [ property; " folds rgb() to hex" ])
        (rule "#030712")
        (opt (rule "rgb(3, 7, 18)")))
    [
      "color";
      "column-rule-color";
      "-webkit-text-fill-color";
      "-webkit-text-stroke-color";
    ]

let test_lossless_declaration_order () =
  let opt ?(lossless = false) css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* Both modes keep authored declaration order. [--lossless] narrows the
     transforms allowed by minification; it does not opt into a CSSOM-visible
     canonical order. *)
  Alcotest.(check string)
    "default keeps source order" ".a{width:1px;color:red}"
    (opt ".a{width:1px;color:red}");
  Alcotest.(check string)
    "lossless keeps authored order" ".a{width:1px;color:red}"
    (opt ~lossless:true ".a{width:1px;color:red}");
  Alcotest.(check string)
    "lossless keeps reverse authored order" ".a{color:red;width:1px}"
    (opt ~lossless:true ".a{color:red;width:1px}");
  (* Overlapping declarations keep their relative order (cascade-significant):
     border-top before border-color decides the top colour. *)
  Alcotest.(check string)
    "overlapping declarations keep order"
    ".a{border-top:1px solid red;border-color:#00f}"
    (opt ~lossless:true ".a{border-top:1px solid red;border-color:blue}")

let test_lossless_keeps_unknown_property_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* A property with no typed spelling still writes cascade slots, so the
     canonical order must not move it past a shorthand that resets it:
     [background] resets the X position the longhand set. *)
  Alcotest.(check string)
    "an unknown longhand stays after its shorthand"
    ".a{background:red;background-position-x:10px}"
    (opt ".a{background:red;background-position-x:10px}");
  (* [grid-row-gap] is the legacy alias of [row-gap]; moving it after [gap]
     would make the row gap 9px instead of 1px. *)
  Alcotest.(check string)
    "a legacy alias stays before the shorthand that resets it"
    ".a{grid-row-gap:9px;gap:1px}"
    (opt ".a{grid-row-gap:9px;gap:1px}");
  (* A typed longhand whose value defeats the typed reader is recovered as an
     unknown property under its own name, and is just as order-dependent. *)
  Alcotest.(check string)
    "a recovered typed longhand stays after its shorthand"
    ".a{margin:0;margin-top:var(--a) var(--b)}"
    (opt ".a{margin:0;margin-top:var(--a) var(--b)}")

(* A typed shorthand and the typed longhands it resets write common cascade
   slots, so the canonical order has to keep them in source order. Each case is
   one family whose two declarations render differently when swapped; the
   expected string is the source order with value normalisation applied. *)
let shorthand_longhand_order_cases =
  [
    ("gap", ".a{row-gap:9px;gap:1px}", ".a{row-gap:9px;gap:1px}");
    ( "animation",
      ".a{animation:x 1s;animation-duration:2s}",
      ".a{animation:x 1s;animation-duration:2s}" );
    ( "animation-range",
      ".a{animation-range:normal;animation-range-start:10%}",
      ".a{animation-range:normal;animation-range-start:10%}" );
    ( "outline",
      ".a{outline:1px solid red;outline-color:blue}",
      ".a{outline:1px solid red;outline-color:#00f}" );
    ( "grid",
      ".a{grid:auto/auto;grid-template-columns:1fr}",
      ".a{grid:auto/auto;grid-template-columns:1fr}" );
    ( "grid-template",
      ".a{grid-template:auto/auto;grid-template-areas:\"a\"}",
      ".a{grid-template:auto/auto;grid-template-areas:\"a\"}" );
    ( "grid-row",
      ".a{grid-row:1/2;grid-row-start:3}",
      ".a{grid-row:1/2;grid-row-start:3}" );
    ( "place-content",
      ".a{place-content:center;align-content:start}",
      ".a{place-content:center;align-content:start}" );
    ( "overflow",
      ".a{overflow:hidden;overflow-x:visible}",
      ".a{overflow:hidden;overflow-x:visible}" );
    ( "border-radius",
      ".a{border-top-left-radius:8px;border-radius:4px}",
      ".a{border-top-left-radius:8px;border-radius:4px}" );
    ( "border resets border-image",
      ".a{border:1px solid red;border-image-source:url(a)}",
      ".a{border:1px solid red;border-image-source:url(a)}" );
    ( "border-image",
      ".a{border-image:none;border-image-repeat:round}",
      ".a{border-image:none;border-image-repeat:round}" );
    ( "columns",
      ".a{columns:2;column-width:10em}",
      ".a{columns:2;column-width:10em}" );
    ( "column-rule",
      ".a{column-rule:1px solid red;column-rule-color:green}",
      ".a{column-rule:1px solid red;column-rule-color:green}" );
    ( "list-style",
      ".a{list-style:none;list-style-type:disc}",
      ".a{list-style:none;list-style-type:disc}" );
    ( "text-decoration",
      ".a{text-decoration:underline;text-decoration-color:red}",
      ".a{text-decoration:underline;text-decoration-color:red}" );
    ( "text-decoration-skip",
      ".a{text-decoration-skip:none;text-decoration-skip-ink:auto}",
      ".a{text-decoration-skip:none;text-decoration-skip-ink:auto}" );
    ( "text-emphasis",
      ".a{text-emphasis:dot;text-emphasis-color:red}",
      ".a{text-emphasis:dot;text-emphasis-color:red}" );
    ( "font resets font-language-override",
      ".a{font:12px a;font-language-override:normal}",
      ".a{font:12px a;font-language-override:normal}" );
    ( "font resets font-palette",
      ".a{font:12px a;font-palette:dark}",
      ".a{font:12px a;font-palette:dark}" );
    ( "font-synthesis",
      ".a{font-synthesis:none;font-synthesis-weight:auto}",
      ".a{font-synthesis:none;font-synthesis-weight:auto}" );
    ( "contain-intrinsic-size",
      ".a{contain-intrinsic-width:20px;contain-intrinsic-size:10px}",
      ".a{contain-intrinsic-width:20px;contain-intrinsic-size:10px}" );
    ( "scroll-margin",
      ".a{scroll-margin:1px;scroll-margin-top:2px}",
      ".a{scroll-margin:1px;scroll-margin-top:2px}" );
    ( "scroll-padding",
      ".a{scroll-padding:1px;scroll-padding-left:2px}",
      ".a{scroll-padding:1px;scroll-padding-left:2px}" );
    ( "overscroll-behavior",
      ".a{overscroll-behavior:auto;overscroll-behavior-x:contain}",
      ".a{overscroll-behavior:auto;overscroll-behavior-x:contain}" );
    ( "container",
      ".a{container:a/size;container-type:normal}",
      ".a{container:a/size;container-type:normal}" );
    (* CSS Scroll Animations 1 sec. 4.2 makes [block] the axis's initial, so the
       shorthand names it by leaving the component out. *)
    ( "scroll-timeline",
      ".a{scroll-timeline:--a block;scroll-timeline-axis:inline}",
      ".a{scroll-timeline:--a;scroll-timeline-axis:inline}" );
    ( "view-timeline",
      ".a{view-timeline:--a block;view-timeline-axis:inline}",
      ".a{view-timeline:--a;view-timeline-axis:inline}" );
    ("caret", ".a{caret:red;caret-shape:bar}", ".a{caret:red;caret-shape:bar}");
    ( "text-box",
      ".a{text-box:trim-both cap alphabetic;text-box-edge:auto}",
      ".a{text-box:trim-both cap alphabetic;text-box-edge:auto}" );
    ( "text-wrap",
      ".a{text-wrap:balance;text-wrap-mode:nowrap}",
      ".a{text-wrap:balance;text-wrap-mode:nowrap}" );
    ( "white-space resets text-wrap-mode",
      ".a{white-space:pre;text-wrap-mode:wrap}",
      ".a{white-space:pre;text-wrap-mode:wrap}" );
    ( "interest-delay",
      ".a{interest-delay:1s;interest-delay-start:2s}",
      ".a{interest-delay:1s;interest-delay-start:2s}" );
    ( "position-try",
      ".a{position-try:--a;position-try-order:most-width}",
      ".a{position-try:--a;position-try-order:most-width}" );
  ]

let test_lossless_keeps_shorthand_longhand_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (opt input))
    shorthand_longhand_order_cases

(* The same relation across rules. Two rules writing one colour tempt the
   optimizer to share a selector list, which puts the later one where the
   earlier one sits; a rule writing the shorthand in between resets that colour,
   so an element matching both would repaint. *)
let test_shorthand_longhand_rule_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  Alcotest.(check string)
    "a colour rule does not cross the shorthand that resets it"
    ".a{column-rule-color:green}.b{column-rule:1px solid \
     red}.c{column-rule-color:green}"
    (opt
       ".a{column-rule-color:green}.b{column-rule:1px solid \
        red}.c{column-rule-color:green}")

(* CSS Logical 1 sec. 2: a flow-relative longhand resolves to a physical side
   the writing mode picks, so a sheet on its own cannot say whether
   [margin-inline-start] and [margin-left] name one slot or two. The canonical
   order therefore has to keep such a pair in source order, in both directions.
   The first three cases are corpus tests duplicates/0015-0017, each of which a
   browser renders differently once the pair is swapped. *)
let logical_physical_order_cases =
  [
    ( "border-top before border-block",
      ".a{border-top:1px solid red;border-block:2px solid red}",
      ".a{border-top:1px solid red;border-block:2px solid red}" );
    ( "border-block before border-top",
      ".a{border-block:2px solid red;border-top:1px solid red}",
      ".a{border-block:2px solid red;border-top:1px solid red}" );
    ( "margin-left before margin-inline-start",
      ".a{margin-left:10px;margin-inline-start:20px}",
      ".a{margin-left:10px;margin-inline-start:20px}" );
    ( "margin-inline-start before margin-left",
      ".a{margin-inline-start:20px;margin-left:10px}",
      ".a{margin-inline-start:20px;margin-left:10px}" );
    ( "padding-top before padding-block-start",
      ".a{padding-top:10px;padding-block-start:20px}",
      ".a{padding-top:10px;padding-block-start:20px}" );
    ( "padding-block-start before padding-top",
      ".a{padding-block-start:20px;padding-top:10px}",
      ".a{padding-block-start:20px;padding-top:10px}" );
    (* The inline axis is the horizontal one in [horizontal-tb] and the vertical
       one in a vertical mode, so a logical border width aliases whichever
       physical side the mode gives it. *)
    ( "border-left-width before border-inline-width",
      ".a{border-left-width:1px;border-inline-width:2px}",
      ".a{border-left-width:1px;border-inline-width:2px}" );
    ( "border-inline-width before border-left-width",
      ".a{border-inline-width:2px;border-left-width:1px}",
      ".a{border-inline-width:2px;border-left-width:1px}" );
    (* A logical side also aliases into the physical shorthand that covers every
       side. *)
    ( "border before border-block",
      ".a{border:1px solid red;border-block:2px solid red}",
      ".a{border:1px solid red;border-block:2px solid red}" );
    ( "top before inset-inline-start",
      ".a{top:1px;inset-inline-start:5px}",
      ".a{top:1px;inset-inline-start:5px}" );
    ( "scroll-margin-top before scroll-margin-block-start",
      ".a{scroll-margin-top:1px;scroll-margin-block-start:2px}",
      ".a{scroll-margin-top:1px;scroll-margin-block-start:2px}" );
    (* CSS Logical 1 sec. 4.4: the sizing family aliases the same way, with no
       physical shorthand covering the two axes. *)
    ( "width before inline-size",
      ".a{width:10px;inline-size:20px}",
      ".a{width:10px;inline-size:20px}" );
    ( "inline-size before width",
      ".a{inline-size:20px;width:10px}",
      ".a{inline-size:20px;width:10px}" );
    ( "height before block-size",
      ".a{height:10px;block-size:20px}",
      ".a{height:10px;block-size:20px}" );
    ( "min-width before min-inline-size",
      ".a{min-width:10px;min-inline-size:20px}",
      ".a{min-width:10px;min-inline-size:20px}" );
    ( "max-width before max-inline-size",
      ".a{max-width:10px;max-inline-size:20px}",
      ".a{max-width:10px;max-inline-size:20px}" );
    (* CSS Logical 1 sec. 4.3: a flow-relative corner resolves to whichever
       physical corner the writing mode and the direction give it. *)
    ( "border-top-left-radius before border-start-start-radius",
      ".a{border-top-left-radius:1px;border-start-start-radius:2px}",
      ".a{border-top-left-radius:1px;border-start-start-radius:2px}" );
    ( "border-start-start-radius before border-top-left-radius",
      ".a{border-start-start-radius:2px;border-top-left-radius:1px}",
      ".a{border-start-start-radius:2px;border-top-left-radius:1px}" );
    ( "contain-intrinsic-width before contain-intrinsic-inline-size",
      ".a{contain-intrinsic-width:1px;contain-intrinsic-inline-size:2px}",
      ".a{contain-intrinsic-width:1px;contain-intrinsic-inline-size:2px}" );
    ( "overscroll-behavior-x before overscroll-behavior-inline",
      ".a{overscroll-behavior-x:none;overscroll-behavior-inline:contain}",
      ".a{overscroll-behavior-x:none;overscroll-behavior-inline:contain}" );
  ]

let test_lossless_keeps_logical_physical_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (opt input))
    logical_physical_order_cases

(* Regrouping - factoring a shared declaration into a selector list, and
   synthesising nesting from a run of adjacent rules - depends on how the input
   happened to order its rules: a rule sitting between two others decides
   whether either applies. A canonical projection therefore turns regrouping
   off, so the same stylesheet written either way maps to one form. *)
(* Eliminating a non-adjacent duplicate declaration has to read the importance
   flag: with an [!important] earlier write in play, the later normal write is
   the dead one, not the winner. Dropping the wrong one changes what [.a]
   computes to, and it is the failure mode a peer minifier ships (Lightning CSS
   1.0.0-alpha.71 emits [.b{color:green}.a{color:#00f}] here, so [.a] resolves
   to blue rather than red). *)
let important_survives_non_adjacent_duplicate () =
  let out src =
    Css.to_string ~minify:true (Css.optimize (Css.of_string_exn src))
  in
  Alcotest.(check string)
    "the !important winner is kept and the dead write dropped"
    ".a{color:red!important}.b{color:green}"
    (out ".a{color:red!important}.b{color:green}.a{color:blue}");
  (* The mirror image: when the later write carries the flag it is the winner,
     so the earlier one is what goes. *)
  Alcotest.(check string)
    "a later !important wins over an earlier normal write"
    ".b{color:green}.a{color:#00f!important}"
    (out ".a{color:red}.b{color:green}.a{color:blue!important}");
  (* Neither carries the flag, so the plain last-wins rule applies. *)
  Alcotest.(check string)
    "without !important the later write wins" ".b{color:green}.a{color:#00f}"
    (out ".a{color:red}.b{color:green}.a{color:blue}");
  (* Both carry it, so last-wins applies among them. *)
  Alcotest.(check string)
    "between two !important writes the later wins"
    ".b{color:green}.a{color:#00f!important}"
    (out ".a{color:red!important}.b{color:green}.a{color:blue!important}")

let regrouping_can_be_disabled () =
  let src =
    ".text-xs{font-size:var(--text-xs);line-height:1}.text-xs\\/4{font-size:var(--text-xs);line-height:4}.text-xs\\/5{font-size:var(--text-xs);line-height:5}.text-xs\\/6{font-size:var(--text-xs);line-height:6}.text-xs\\/7{font-size:var(--text-xs);line-height:7}"
  in
  let sheet = Css.of_string_exn src in
  let out ?regroup () =
    Css.to_string ~minify:true (Css.optimize ?regroup sheet)
  in
  Alcotest.(check bool)
    "regrouping on lifts the shared declaration" true
    (Astring.String.is_infix ~affix:".text-xs,.text-xs\\/4" (out ()));
  Alcotest.(check bool)
    "regrouping off leaves each rule alone" false
    (Astring.String.is_infix ~affix:".text-xs,.text-xs\\/4"
       (out ~regroup:false ()))

(* The other regrouping pass: two adjacent rules sharing a selector prefix
   become one nested rule, and whether they are adjacent is exactly what an
   unrelated rule between them decides. *)
let nesting_synthesis_can_be_disabled () =
  let sheet =
    Css.of_string_exn
      ".prose:where(:not(.not-prose,.not-prose \
       *)){color:red;font-size:1rem}.prose:where(:not(.not-prose,.not-prose \
       *)):where(.dark,.dark *){color:blue;font-size:2rem}"
  in
  let out ?regroup () =
    Css.to_string ~minify:true (Css.optimize ?regroup sheet)
  in
  Alcotest.(check bool)
    "regrouping on nests the second rule" true
    (Astring.String.is_infix ~affix:"&:where(.dark,.dark *)" (out ()));
  Alcotest.(check bool)
    "regrouping off keeps both flat" false
    (Astring.String.is_infix ~affix:"&:where(.dark,.dark *)"
       (out ~regroup:false ()))

let flatten_nesting_is_an_output_invariant () =
  let out css =
    Css.of_string_exn css
    |> Css.optimize ~flatten_nesting:true
    |> Css.to_string ~minify:true
  in
  let expected =
    ".long-component-name{color:red}.long-component-name:hover{color:#00f}"
  in
  Alcotest.(check string)
    "flat input stays flat after nesting synthesis" expected
    (out ".long-component-name{color:red}.long-component-name:hover{color:blue}");
  Alcotest.(check string)
    "authored nesting is flat in the final output" expected
    (out ".long-component-name{color:red;&:hover{color:blue}}")

(* CSS Fragmentation 3 sec. 3.4 makes [page-break-*] the same property as its
   [break-*] alias, so two [page-break-*] declarations in one rule are two
   declarations of one property and the later one shadows the earlier, exactly
   as [break-*] and every other property does. A [var()] on the left does not
   change whose property it is. *)
let page_break_var_shadows_like_its_alias () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (Css.of_string_exn ~strict:false input |> minify))
    [
      (* The modern spelling sets the oracle. *)
      (".a{break-after:var(--x);break-after:page}", ".a{break-after:page}");
      (".a{break-after:page;break-after:var(--x)}", ".a{break-after:var(--x)}");
      (".a{break-inside:var(--x);break-inside:avoid}", ".a{break-inside:avoid}");
      (* The legacy spelling answers the same. *)
      ( ".a{page-break-after:var(--x);page-break-after:always}",
        ".a{break-after:page}" );
      ( ".a{page-break-after:always;page-break-after:var(--x)}",
        ".a{page-break-after:var(--x)}" );
      ( ".a{page-break-before:var(--x);page-break-before:left}",
        ".a{break-before:left}" );
      ( ".a{page-break-inside:var(--x);page-break-inside:avoid}",
        ".a{break-inside:avoid}" );
      ( ".a{page-break-inside:avoid;page-break-inside:var(--x)}",
        ".a{page-break-inside:var(--x)}" );
    ]

(* CSS Syntax 3 sec. 5.4.2 keeps an unrecognised at-rule's body as raw source
   text, so nothing in it is typed and the printer cannot touch it: rewriting
   the body changes the AST, which is the optimizer's job and not the
   serializer's. The optimizer reads the body as a component-value stream and
   writes it back with the separator rules that already serve custom-property
   streams, so token boundaries, strings, escapes and nested blocks survive
   while the layout between them goes.

   [--minify] on its own leaves the body alone, since it may not change what the
   AST holds. *)
let unknown_at_rule_body_is_compacted () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok p -> p.stylesheet
    | Error _ -> Alcotest.failf "parse failed: %s" css
  in
  let optimized css = Css.to_string ~minify:true (Css.optimize (parse css)) in
  let minified css = Css.to_string ~minify:true (parse css) in
  let compacts name css expected =
    Alcotest.(check string) name expected (optimized css);
    Alcotest.(check string)
      (name ^ ": optimizing again changes nothing")
      expected (optimized expected)
  in
  compacts "a nested rule in an opaque body loses its layout"
    "@foo{ .a { color: red } }" "@foo{.a{color:red}}";
  compacts "a string keeps every byte between its quotes"
    "@foo{ a: \"  b  c  \" }" "@foo{a:\"  b  c  \"}";
  compacts "an escape spelling an ident is written as that ident"
    "@foo{ \\41 b }" "@foo{Ab}";
  compacts "an escape the ident needs is kept" "@foo{ a\\ b }" "@foo{a\\ b}";
  compacts "two idents keep the boundary between them" "@foo{ a   b }"
    "@foo{a b}";
  compacts "an ident before a paren keeps the space that stops a function token"
    "@foo{ a ( b  c ) [ d ] }" "@foo{a (b c)[d]}";
  (* [--minify] alone may not rewrite the AST, so the body travels verbatim. *)
  Alcotest.(check string)
    "minify alone leaves the opaque body as authored"
    "@foo{ .a { color: red } }"
    (minified "@foo{ .a { color: red } }")

let optimize_tests =
  [
    ( "unknown at-rule body is compacted",
      `Quick,
      unknown_at_rule_body_is_compacted );
    ( "!important survives non-adjacent duplicate elimination",
      `Quick,
      important_survives_non_adjacent_duplicate );
    ("regrouping can be disabled", `Quick, regrouping_can_be_disabled);
    ( "nesting synthesis can be disabled",
      `Quick,
      nesting_synthesis_can_be_disabled );
    ( "flatten nesting is an output invariant",
      `Quick,
      flatten_nesting_is_an_output_invariant );
    ( "a page-break var shadows like its alias",
      `Quick,
      page_break_var_shadows_like_its_alias );
    ("vendor prefix strip", `Quick, test_vendor_prefix_strip);
    ("vendor prefix baseline gate", `Quick, test_vendor_prefix_baseline_gate);
    ("color property folds", `Quick, test_color_property_folds);
    ("lossless declaration order", `Quick, test_lossless_declaration_order);
    ( "lossless keeps unknown property order",
      `Quick,
      test_lossless_keeps_unknown_property_order );
    ( "lossless keeps shorthand longhand order",
      `Quick,
      test_lossless_keeps_shorthand_longhand_order );
    ("shorthand longhand rule order", `Quick, test_shorthand_longhand_rule_order);
    ( "lossless keeps logical physical order",
      `Quick,
      test_lossless_keeps_logical_physical_order );
    ( "var() colour functions preserved",
      `Quick,
      test_var_color_functions_preserved );
    ( "transform-origin zero components fold to 0",
      `Quick,
      test_transform_origin_zero_folds );
    ( "adjacent merge survives preflight skip",
      `Quick,
      test_adjacent_merge_survives_preflight_skip );
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
  ]

(** {1 Selector merging tests (cascade semantics)} *)

let optimized_string ?scope ?targets ?(enforce_spec = false) css =
  css |> Cursor.of_string |> Css.Stylesheet.read
  |> Css.Optimize.stylesheet ?scope ?targets ~enforce_spec
  |> Css.Stylesheet.to_string ~minify:true ~enforce_spec
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

let test_combine_identical_oklab_none () =
  (* A [none] channel in oklab() is a missing component that matters only for
     color interpolation (CSS Color 4 sec. 4.4): color-mix, gradients, and
     transitions carry it forward. It does not affect rule grouping, which is
     set union (Selectors 4 sec. 4.2) and never interpolates. So two rules with
     byte-identical oklab-none declarations combine into one comma-selector rule
     exactly like any other identical pair - no minifier-specific exception.
     oklab() serialises space-separated, so a comma in the output is the
     combined selector list. *)
  let output =
    optimized_string
      ".a { color: oklab(40% none .1) } .b { color: oklab(40% none .1) }"
  in
  Alcotest.(check bool)
    "identical oklab(none) rules combine into a comma selector" true
    (String.contains output ',')

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

let minify_str css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } -> minify stylesheet |> String.trim
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

(* pp alone, with no optimize pass: serialize the parsed AST minified. Under the
   policy "pp must not change semantics", this is faithful - it may pick the
   shortest spelling of a node but must not transform one node into another. *)
let pp_min css =
  match Css.of_string ~strict:false css with
  | Ok { Css.stylesheet; _ } ->
      Css.to_string ~minify:true stylesheet |> String.trim
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let assert_pp_and_optimize input ~minified ~optimized =
  Alcotest.(check string) (input ^ " [minify]") minified (pp_min input);
  Alcotest.(check string)
    (input ^ " [minify+optimize]")
    optimized (minify_str input)

let test_factor_shared_declarations () =
  (* Two sibling rules sharing a declaration subset factor that subset into a
     combined selector, leaving each rule its unique declarations. Safe here -
     adjacent, distinct selectors, no conflicting property - and shorter, in a
     single pass. *)
  let once =
    minify_str
      ".emoji-icon{display:inline-block;height:20px;width:20px}.emoji-result{display:inline-block;font-size:18px;height:20px}"
  in
  Alcotest.(check string)
    "shared declarations factor into a combined selector"
    ".emoji-icon,.emoji-result{display:inline-block;height:20px}.emoji-icon{width:20px}.emoji-result{font-size:18px}"
    once;
  Alcotest.(check string) "factoring reached in one pass" once (minify_str once)

let test_factor_interval_schedule () =
  (* The whole run shares display:block, but the optimal rewrite is two
     non-overlapping local intervals: .a/.b/.c additionally share color:red,
     while .d/.e/.f additionally share background-color:blue. This pins the
     weighted interval scheduler rather than a single broad greedy factoring. *)
  let once =
    minify_str
      ".a{display:block;color:red;width:1px}.b{display:block;color:red;height:1px}.c{display:block;color:red;padding:1px}.d{display:block;background-color:blue;width:2px}.e{display:block;background-color:blue;height:2px}.f{display:block;background-color:blue;padding:2px}"
  in
  Alcotest.(check string)
    "disjoint local common blocks factor independently"
    ".a,.b,.c{display:block;color:red}.a{width:1px}.b{height:1px}.c{padding:1px}.d,.e,.f{display:block;background-color:#00f}.d{width:2px}.e{height:2px}.f{padding:2px}"
    once;
  Alcotest.(check string)
    "interval factoring is idempotent" once (minify_str once)

let test_factor_interval_keeps_overrides () =
  (* A same-property different-value member can still join the common-property
     group, but its value must be emitted after the shared rule. This catches
     the score/build path for interval leftovers, including the offset-aware
     earlier-overrides check. *)
  Alcotest.(check string)
    "interval factoring preserves later overrides"
    ".a,.b,.c{display:block;color:#00f}.a,.c{color:red}"
    (minify_str
       ".a{display:block;color:red}.b{display:block;color:blue}.a{display:block;color:red}.c{display:block;color:red}")

let test_factor_selector_branch_keeps_later_override () =
  (* Extracting the shared overflow/position declarations must not let the
     earlier selector-list branch overwrite the later .scroll-item a z-index.
     Only that branch is overridden; .scroll-pic-frame keeps z-index:100. *)
  Alcotest.(check string)
    "selector-list branch keeps its later override"
    ".scroll-item \
     a,.scroll-pic-frame,.scroll-pic-wrap{overflow:hidden;position:relative}.scroll-item \
     a,.scroll-pic-frame{z-index:100}.scroll-item{display:inline;float:left;height:164px;overflow:hidden;width:200px}.scroll-item \
     a{border:1px solid#fff;display:block;height:162px;width:198px;z-index:50}"
    (minify_str
       ".scroll-item \
        a,.scroll-pic-frame{overflow:hidden;position:relative;z-index:100}.scroll-pic-wrap{overflow:hidden;position:relative}.scroll-item{display:inline;float:left;height:164px;overflow:hidden;width:200px}.scroll-item \
        a{border:1px solid \
        #fff;display:block;height:162px;width:198px;z-index:50}")

let test_factoring_reaches_fixpoint () =
  (* Factoring one shared subset can expose another: grouping .a/.b on color:red
     leaves .b with padding:0, now groupable with .c. A correct optimizer
     applies every such factoring in a single pass - a second pass that shrinks
     the output means the first stopped early. This pins idempotence rather than
     the exact grouping, since the triangle of shared declarations has several
     equal-length factorings. *)
  let once =
    minify_str
      ".a{color:red;margin:0}.b{color:red;padding:0}.c{margin:0;padding:0}"
  in
  Alcotest.(check string)
    "interacting factorings converge in one pass" once (minify_str once)

let test_large_stylesheet_reaches_local_fixpoint () =
  (* Large sheets deliberately limit the expensive global factoring walk, but
     rule-local normalization must still converge. Composing these longhands
     creates a shorthand whose explicit initial delay only disappears when the
     synthesized declaration itself is normalized. *)
  let padding =
    List.init 129 (fun i -> Fmt.str ".padding-%d{z-index:%d}" i i)
    |> String.concat ""
  in
  let input =
    padding
    ^ ".target{transition-property:opacity;transition-duration:70ms;transition-delay:0ms;transition-timing-function:linear}"
  in
  let once = minify_str input in
  Alcotest.(check string)
    "large-sheet local rewrites converge in one pass" once (minify_str once)

let test_large_stylesheet_normalizes_vendor_aliases () =
  (* Alias comparison must see the normalized typed values. Otherwise the
     prefixed 250ms/500ms values differ structurally from the normalized .25s/
     .5s unprefixed twins until the serialized output is parsed again. *)
  let padding =
    List.init 129 (fun i -> Fmt.str ".vendor-padding-%d{z-index:%d}" i i)
    |> String.concat ""
  in
  let input =
    padding
    ^ ".target{-webkit-transition:opacity 250ms;transition:opacity \
       250ms;-webkit-animation:spin 500ms linear;animation:spin 500ms linear}"
  in
  let once = minify_str input in
  Alcotest.(check string)
    "large-sheet aliases compare normalized values" once (minify_str once)

let test_large_stylesheet_factoring_reaches_fixpoint () =
  (* Large graphs refresh candidates in batches. The refreshed batch can itself
     expose another profitable grouping, which must be enumerated before the
     scheduler returns. Inert custom-property rules select the large-graph path
     without participating in these factoring groups. *)
  let padding =
    List.init 129 (fun i ->
        Fmt.str ".factor-padding-%d{--factor-padding-%d:0}" i i)
    |> String.concat ""
  in
  let input =
    padding
    ^ ".scroll-item{display:inline;float:left;height:164px;overflow:hidden;width:200px}.scroll-item \
       a{border:1px solid \
       #fff;display:block;height:162px;overflow:hidden;position:relative;width:198px;z-index:50}.part-h-m{margin-right:20px;width:360px}.part-j-l,.part-j-m,.part-j-r{display:inline;float:left}.part-n-l,.part-n-m,.part-n-r{display:inline;float:left}.part-n-l{margin-right:20px;width:240px}.mod44-list{display:inline;float:right;margin-right:5px}.mod44-list \
       li{display:inline;float:left;line-height:34px;height:34px;margin-right:5px}.mod-a \
       .tab-nav-a \
       a{border-left:0;float:left;line-height:23px;height:23px;padding:0}.mod-a \
       .tab-nav-a \
       span{border-left:0;float:left;line-height:23px;height:23px;padding:0 \
       2px}"
  in
  let once = minify_str input in
  Alcotest.(check string)
    "large-sheet factoring converges in one scheduler run" once
    (minify_str once)

let test_no_factor_across_conflict () =
  (* CSS Cascade 6.1: the two .x rules conflict on color, so they merge (last
     wins). The later .y carries the first .x's value, but grouping it with that
     .x would reorder it past the conflicting .x{color:blue} and change the
     cascade - so .y stays separate. *)
  Alcotest.(check string)
    "no grouping across a conflicting same-selector override"
    ".x{color:#00f}.y{color:red}"
    (minify_str ".x{color:red}.x{color:blue}.y{color:red}")

let test_zero_box_side_covered_by_shorthand () =
  (* Margin and padding have only the four side longhands, so a zero shorthand
     and same-side zero longhand are equivalent on that side. Border is not:
     border shorthands also carry style and color state. *)
  assert_pp_and_optimize ".x{margin:0;margin-top:0}"
    ~minified:".x{margin:0;margin-top:0}" ~optimized:".x{margin:0}";
  assert_pp_and_optimize ".x{margin-top:0}.x{margin:0}"
    ~minified:".x{margin-top:0}.x{margin:0}" ~optimized:".x{margin:0}";
  assert_pp_and_optimize ".x{padding:0}.x{padding-left:0}"
    ~minified:".x{padding:0}.x{padding-left:0}" ~optimized:".x{padding:0}";
  assert_pp_and_optimize ".x{margin:5px}.x{margin-top:0}"
    ~minified:".x{margin:5px}.x{margin-top:0}" ~optimized:".x{margin:0 5px 5px}";
  assert_pp_and_optimize ".x{border:0}.x{border-top-width:0}"
    ~minified:".x{border:0}.x{border-top-width:0}"
    ~optimized:".x{border:0;border-top-width:0}"

let test_keep_zero_duration_transition () =
  (* transition:color has 0s duration, so nothing animates now - but it still
     sets transition-property:color, which becomes live the moment any duration
     override applies (a :hover rule, inline style, or JS). Minify is open over
     that runtime state, so the declaration is not dead and must not be dropped.
     cubic-bezier(.25,.1,.25,1) is ease (the default timing function), so only
     the timing function drops; lightningcss agrees on transition:color. *)
  Alcotest.(check string)
    "0s transition keeps its property, not dropped" "a{transition:color}"
    (minify_str "a{transition:color}");
  Alcotest.(check string)
    "bezier folds to ease, the default ease then drops" "a{transition:color}"
    (minify_str "a{transition:color cubic-bezier(0.25,0.1,0.25,1)}")

let test_keep_var_border_spaces () =
  (* The spaces between var() references in a border value are significant: they
     are token separators in the substituted value, so [var(--bw) var(--bs)]
     keeps the components apart where [var(--bw)var(--bs)] would glue them into
     a single token on substitution. The minify round trip must preserve
     them. *)
  Alcotest.(check string)
    "var()-valued border keeps significant spaces"
    "a{border:var(--bw) var(--bs) var(--bc)}"
    (minify_str "a{border:var(--bw) var(--bs) var(--bc)}")

let test_custom_value_close_paren_whitespace () =
  (* A [)] closing a non-substitution function or a block is a hard token
     boundary that no neighbour can merge across, so the whitespace after it is
     insignificant: optimize folds it out (and the canonical diff inherits
     that), while pp keeps it because pp is a pure serializer. *)
  Alcotest.(check string)
    "pp keeps whitespace after a function close (held)"
    "a{--x:drop-shadow(0 0 0) drop-shadow(1px 1px)}"
    (pp_min "a{--x:drop-shadow(0 0 0) drop-shadow(1px 1px)}");
  Alcotest.(check string)
    "optimize folds whitespace after a function close (canonical)"
    "a{--x:drop-shadow(0 0 0)drop-shadow(1px 1px)}"
    (minify_str "a{--x:drop-shadow(0 0 0) drop-shadow(1px 1px)}");
  (* A [var()]-carrying calc does not reduce to a leaf, so it stays a function
     and exercises the whitespace-after-[)] fold (a fully-constant angle calc
     instead folds to a dimension - see the custom-property folding tests). *)
  Alcotest.(check string)
    "optimize folds whitespace after calc() before an ident"
    "a{--x:calc(45deg*var(--n))in oklab}"
    (minify_str "a{--x:calc(45deg * var(--n)) in oklab}");
  (* A [var()] substitutes a token stream textually, so dropping the whitespace
     after it could merge the substituted values ([1px2px]); it stays. *)
  Alcotest.(check string)
    "optimize keeps whitespace after var()" "a{--x:var(--a) var(--b)}"
    (minify_str "a{--x:var(--a) var(--b)}")

let test_distant_media_merge () =
  (* Non-adjacent same-condition @media merge: safe when the crossed rule sets a
     different property, so no element's computed value changes. *)
  Alcotest.(check string)
    "merges across a non-conflicting intervening rule"
    "@media(width>=1px){a{background:#0b0;color:red}}a{color:green}"
    (minify_str
       "@media (width>=1px){a{color:red}}a{color:green}@media \
        (width>=1px){a{background:#0b0}}");
  (* A crossed rule that reorders the same property to a different value blocks
     the merge. *)
  Alcotest.(check string)
    "keeps blocks apart on a conflicting reorder"
    "@media(width>=1px){a{color:red}}a{color:green}@media(width>=1px){a{color:#00f}}"
    (minify_str
       "@media (width>=1px){a{color:red}}a{color:green}@media \
        (width>=1px){a{color:blue}}");
  (* A layer statement establishes cascade order; never merge across it. *)
  Alcotest.(check string)
    "no merge across a layer statement"
    "@media(width>=1px){a{color:red}}@layer \
     theme;@media(width>=1px){a{color:#00f}}"
    (minify_str
       "@media (width>=1px){a{color:red}}@layer theme;@media \
        (width>=1px){a{color:blue}}");
  (* A crossed shorthand writes the longhand slot the hoisted block writes, so
     the two do not commute even though no property name is shared: hoisting
     computes blue where the source computes green. *)
  Alcotest.(check string)
    "keeps blocks apart when a crossed shorthand writes the same slot"
    "@media(width>=1px){a{background-color:red}}a{background:#00f}@media(width>=1px){a{background-color:green}}"
    (minify_str
       "@media (width>=1px){a{background-color:red}}a{background:blue}@media \
        (width>=1px){a{background-color:green}}");
  (* CSS Nesting 1 sec. 3.4: a declaration written after a nested rule stays
     behind it, so the run between the two blocks is one more rule with the
     enclosing selector. Hoisting the second block over it would compute blue
     where the source computes green. *)
  Alcotest.(check string)
    "keeps blocks apart across a conflicting nested declarations run"
    ".a{@media(width>=1px){color:red}color:#00f;@media(width>=1px){color:green}}"
    (minify_str
       ".a{@media (width>=1px){color:red}color:blue;@media \
        (width>=1px){color:green}}");
  (* The run sets another property, so nothing an element computes depends on
     the order: the blocks still merge, and the disjoint run rejoins the rule's
     own declarations. *)
  Alcotest.(check string)
    "merges across a nested declarations run that sets another property"
    ".a{outline-color:#00f;@media(width>=1px){color:red;color:green}}"
    (minify_str
       ".a{@media (width>=1px){color:red}outline-color:blue;@media \
        (width>=1px){color:green}}");
  (* Adjacent blocks cross nothing, so the run after them keeps its place and
     the merge stands. *)
  Alcotest.(check string)
    "merges adjacent blocks inside a rule"
    ".a{@media(width>=1px){color:red;color:green}color:#00f}"
    (minify_str
       ".a{@media (width>=1px){color:red}@media \
        (width>=1px){color:green}color:blue}")

(* CSS Conditional Rules 5 sec. 5: a [@container] rule applies to an element
   when the container its name selects matches its query, so two blocks under
   one name and one query hold the same content as one block over their
   concatenation. What a distant merge has to earn is the move: hoisting the
   later block over the statements written between changes the order of
   appearance of its declarations against theirs, and an element computes that
   differently only where a pair of declarations does not commute. *)
let test_distant_container_merge () =
  (* Conditional Rules 5 sec. 5.4: the query container is the nearest ancestor
     the name selects, so [main] and [side] ask about different elements. The
     conditions are the same characters and the blocks still hold apart. *)
  Alcotest.(check string)
    "keeps distinct container names apart"
    "@container main (width>=10px){.a{color:red}}.c{background:#00f}@container \
     side (width>=10px){.b{color:green}}"
    (minify_str
       "@container main \
        (width>=10px){.a{color:red}}.c{background:blue}@container side \
        (width>=10px){.b{color:green}}");
  (* An unnamed query takes the nearest ancestor container whatever its name,
     which is not the nearest one called [main]. *)
  Alcotest.(check string)
    "keeps a named and an unnamed query apart"
    "@container main \
     (width>=10px){.a{color:red}}.c{background:#00f}@container(width>=10px){.b{color:green}}"
    (minify_str
       "@container main \
        (width>=10px){.a{color:red}}.c{background:blue}@container \
        (width>=10px){.b{color:green}}");
  (* The crossed rule writes the colour slot the hoisted block writes on a
     selector it overlaps, so hoisting computes blue where the source computes
     green. *)
  Alcotest.(check string)
    "keeps blocks apart on a conflicting reorder"
    "@container(width>=10px){.a{color:red}}.a{color:green}@container(width>=10px){.a{color:#00f}}"
    (minify_str
       "@container (width>=10px){.a{color:red}}.a{color:green}@container \
        (width>=10px){.a{color:blue}}");
  (* A layer statement establishes cascade order; never merge across it. *)
  Alcotest.(check string)
    "no merge across a layer statement"
    "@container(width>=10px){.a{color:red}}@layer \
     theme;@container(width>=10px){.b{color:#00f}}"
    (minify_str
       "@container (width>=10px){.a{color:red}}@layer theme;@container \
        (width>=10px){.b{color:blue}}");
  (* The crossed rule sets another property on another selector, so no element
     computes anything differently once the block moves before it. *)
  Alcotest.(check string)
    "merges across a non-conflicting intervening rule"
    "@container(width>=10px){.a{color:red}.b{color:green}}.c{background:#00f}"
    (minify_str
       "@container (width>=10px){.a{color:red}}.c{background:blue}@container \
        (width>=10px){.b{color:green}}")

let test_keep_bang_comment_leading () =
  (* A bang comment (/*! ... */) is preserved by minify; it stays where it was
     authored rather than being moved past the following rule. *)
  Alcotest.(check string)
    "leading bang comment stays leading" "/*! important */a{color:red}"
    (minify_str "/*! important */a{color:red}")

(* New policy: pp serializes the AST faithfully and must not change semantics.
   The litmus for where a normalization belongs: if two spellings parse to the
   SAME node it is a pure serialization choice and pp picks the shortest; if
   they parse to DIFFERENT nodes the unification is semantic and belongs in
   optimize, so pp must keep them textually distinct. The pairs below are
   semantically equal but parse to distinct nodes - a math function vs a
   literal, a named vs a hex colour, a color-mix() vs its result. *)
let normalize_pairs =
  [
    ("a{width:calc(1px + 2px)}", "a{width:3px}");
    ("a{width:calc(2 * 5px)}", "a{width:10px}");
    ("a{color:red}", "a{color:#f00}");
    ("a{color:black}", "a{color:#000}");
    ("a{color:rgb(255 255 255)}", "a{color:#fff}");
    ("a{color:rgb(255 0 0)}", "a{color:red}");
    ("a{color:hsl(0 100% 50%)}", "a{color:red}");
    ("a{color:color-mix(in srgb,red,red)}", "a{color:red}");
  ]

(* Same normalization story for gradients, basic shapes, and clip-path: each
   pair is semantically equal (a side keyword vs the matching angle, an
   explicit-defaults shape vs the bare functional form), so optimize must
   collapse it to one canonical node and pp alone must stay a fixed point.
   (Whether pp keeps these textually distinct depends on which forms the parser
   canonicalizes, so that side is asserted only for the colour/calc pairs, where
   the node distinction is certain.) The basic-shape pairs use [clip-path]:
   [shape-outside] holds its value as raw text, so nothing about it is
   normalized. *)
let gradient_shape_pairs =
  [
    ( "a{background:linear-gradient(to top,red,blue)}",
      "a{background:linear-gradient(0deg,red,blue)}" );
    ("a{clip-path:circle(closest-side at center)}", "a{clip-path:circle()}");
    ( "a{clip-path:ellipse(closest-side closest-side at center)}",
      "a{clip-path:ellipse()}" );
  ]

let assert_pp_keeps_distinct pairs =
  List.iter
    (fun (a, b) ->
      if String.equal (pp_min a) (pp_min b) then
        Alcotest.failf
          "pp collapsed distinct nodes %S and %S to %S (normalize in optimize, \
           not pp)"
          a b (pp_min a))
    pairs

let assert_optimize_unifies pairs =
  List.iter
    (fun (a, b) ->
      Alcotest.(check string)
        (Fmt.str "optimize unifies %s / %s" a b)
        (minify_str a) (minify_str b))
    pairs

let assert_pp_idempotent pairs =
  List.iter
    (fun (a, b) ->
      Alcotest.(check string)
        (Fmt.str "pp idempotent on %s" a)
        (pp_min a)
        (pp_min (pp_min a));
      Alcotest.(check string)
        (Fmt.str "pp idempotent on %s" b)
        (pp_min b)
        (pp_min (pp_min b)))
    pairs

let test_pp_keeps_distinct_nodes () =
  (* pp must keep each pair textually distinct: collapsing them is a node
     transform, which is optimize's job, not pp's. *)
  assert_pp_keeps_distinct normalize_pairs

let test_optimize_unifies_equivalent_nodes () =
  (* The flip side: optimize is the AST->AST transform that collapses each pair
     to one canonical node, so the optimized minified output matches. *)
  assert_optimize_unifies normalize_pairs

let test_optimize_unifies_gradients_shapes () =
  assert_optimize_unifies gradient_shape_pairs

let test_optimize_folds_flex_calc () =
  (* The reader holds a flex calc() unfolded and pp serializes it lexically; the
     constant fold to a <number> literal is this optimize+minify transform
     (matching lightningcss). pp alone keeps calc(1 + 2). *)
  Alcotest.(check string)
    "flex-grow constant calc folds under optimize" "a{flex-grow:3}"
    (minify_str "a{flex-grow:calc(1 + 2)}");
  Alcotest.(check string)
    "flex shorthand grow constant calc folds under optimize" "a{flex:3 1 0}"
    (minify_str "a{flex:calc(1 + 2) 1 0}")

let test_pp_picks_shortest_same_node () =
  (* Spellings that parse to the SAME node are a pure serialization choice, so
     pp emits one shortest spelling for both with no optimize pass. *)
  let same a b =
    Alcotest.(check string)
      (Fmt.str "pp canonicalizes same-node %s / %s" a b)
      (pp_min a) (pp_min b)
  in
  same "a{width:0.5px}" "a{width:.5px}";
  same "a{width:10.0px}" "a{width:10px}";
  same "a{color:#FFFFFF}" "a{color:#fff}";
  same "a{color:#AbC}" "a{color:#abc}";
  same "a{color:RED}" "a{color:red}"

let test_pp_minify_is_idempotent () =
  (* pp alone (no optimize) is a fixed point: re-parsing minified output and
     re-printing changes nothing, because pp emits canonical bytes per node. *)
  assert_pp_idempotent normalize_pairs;
  assert_pp_idempotent gradient_shape_pairs

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
    ".hero{background:#008000}" background_output

let c3_stylesheet_scope_background_synthesis () =
  (* CSS Backgrounds shorthands are resetful: synthesizing [background] resets
     omitted background longhands. Under the default [`Fragment] scope, a
     fragment cannot assume no earlier author CSS wrote one of those omitted
     longhands. Under [`Stylesheet] scope, the caller asserts the whole relevant
     author stylesheet graph is available, so the shorter resetful shorthand is
     allowed. This is the CSS-text scope axis, not the [~closed_world] DOM
     assumption. *)
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
    ".card{background-color:red;background-image:none;background-repeat:repeat;background-position:0 \
     0;background-attachment:scroll}"
    (optimize partial_run);
  Alcotest.(check string)
    "stylesheet-scope partial background run may synthesize shorthand"
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

let closed_world_groups_disjoint_selectors () =
  (* [.a] and [.b] share a body, but [.c] sits between them writing a different
     value for the same property. In the open world an element could be [.b.c],
     where grouping [.a,.b] ahead of [.c] would flip its colour, so the default
     refuses. Under closed_world the caller asserts no element matches two of
     these selectors, so the group is safe. *)
  let opt ?closed_world css =
    Css.of_string_exn ~strict:false css
    |> Css.optimize ?closed_world |> Css.to_string ~minify:true
  in
  let input = ".a{color:red}.c{color:blue}.b{color:red}" in
  let default = opt input in
  let closed = opt ~closed_world:true input in
  (* the only comma either output can carry is a grouped selector list *)
  Alcotest.(check bool)
    "open world keeps the selectors separate (a .b.c element would flip)" false
    (String.contains default ',');
  Alcotest.(check bool)
    "closed world groups the disjoint .a and .b" true
    (String.contains closed ',');
  Alcotest.(check bool)
    "closed world is shorter" true
    (String.length closed < String.length default)

let coalesce_adjacent_custom_property_rules () =
  let opt css =
    Css.of_string_exn ~strict:false css
    |> Css.optimize |> Css.to_string ~minify:true
  in
  (* Custom-property rules are barred from the DAG factoring, but adjacent rules
     with an identical custom-property body (Tailwind gradient/mask/ring stacks)
     are unconditionally safe to coalesce into one comma-selector rule. *)
  Alcotest.(check string)
    "adjacent identical custom-property rules coalesce"
    ".a,.b,.c{--tw-gradient-from:#3b82f6}"
    (opt
       ".a{--tw-gradient-from:#3b82f6}.b{--tw-gradient-from:#3b82f6}.c{--tw-gradient-from:#3b82f6}");
  (* A different value between them keeps the run split: merging across the
     conflicting `.b` would flip a `.b.c` element. *)
  Alcotest.(check bool)
    "custom-property run split by a conflicting value stays unmerged" false
    (String.contains (opt ".a{--x:red}.b{--x:blue}.c{--x:red}") ',')

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
    ".hero{background:#008000!important}" output

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

(* A rule's [declarations] is the run written before its first nested statement
   (CSS Nesting 1 sec. 3.4), not its whole body, so absorbing a later
   same-selector rule moves that rule's declarations ahead of body content the
   merge has to read for itself. It reads the whole body, so a rule carrying
   nested children can still absorb a later one: the move is only observable for
   a property that body also sets. *)
let same_selector_merge_past_nested () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "disjoint nested children do not block the merge"
    ".a{color:red;padding:1rem;&:hover{background:#00f}}"
    (canon ".a{color:red;&:hover{background:blue}}.a{padding:1rem}");
  Alcotest.(check string)
    "a nested child setting the same property keeps its place"
    ".a{color:red;&:hover{padding:2rem}}.a{padding:1rem}"
    (canon ".a{color:red;&:hover{padding:2rem}}.a{padding:1rem}");
  (* A nested conditional group sets properties just as a nested rule does:
     hoisting [padding:1rem] ahead of it hands the conditional the win wherever
     it applies, which is a different rendered padding. *)
  Alcotest.(check string)
    "a nested @media setting the same property blocks it"
    ".a{color:red;@media(width>=1px){padding:2rem}}.a{padding:1rem}"
    (canon ".a{color:red;@media (min-width:1px){padding:2rem}}.a{padding:1rem}");
  Alcotest.(check string)
    "a nested @container setting the same property blocks it"
    ".a{color:red;@container(width>=1px){padding:2rem}}.a{padding:1rem}"
    (canon
       ".a{color:red;@container (min-width:1px){padding:2rem}}.a{padding:1rem}");
  (* The merged rule replays every declaration ahead of every nested block, so
     no order of the rules rescues a merge a later declaration would have to
     cross: [color:green] wins over the [@media] wherever it applies, and
     merging hands the win back to the [@media]. *)
  Alcotest.(check string)
    "a later declaration overriding a nested @media blocks the merge"
    ".a{padding:1rem;@media(width>=1px){color:#00f}}.a{color:green}"
    (canon ".a{padding:1rem;@media (min-width:1px){color:blue}}.a{color:green}");
  (* Two nested blocks setting a common property race in the order the merged
     rule replays them, which stays the order they were written in. *)
  Alcotest.(check string)
    "nested blocks setting a common property keep their order"
    ".a{padding:1rem;margin:1rem;@media(width>=1px){color:#00f}@media(width>=2px){color:green}}"
    (canon
       ".a{padding:1rem;@media \
        (min-width:1px){color:blue}}.a{margin:1rem;@media \
        (min-width:2px){color:green}}");
  (* A shorthand and a longhand of its family write a common cascade slot, so a
     nested [margin] and a later [margin-top] compete: hoisting the longhand
     ahead of the nested block hands [margin] the win, a different rendered
     margin-top wherever the query applies. *)
  Alcotest.(check string)
    "a later longhand overriding a nested shorthand blocks the merge"
    ".a{color:red;@media(width>=1px){margin:2rem}}.a{margin-top:1rem}"
    (canon
       ".a{color:red;@media (min-width:1px){margin:2rem}}.a{margin-top:1rem}");
  (* The same slot written the other way round: a later shorthand resets the
     longhand the nested block set. *)
  Alcotest.(check string)
    "a later shorthand overriding a nested longhand blocks the merge"
    ".a{color:red;@media(width>=1px){margin-top:2rem}}.a{margin:1rem}"
    (canon
       ".a{color:red;@media (min-width:1px){margin-top:2rem}}.a{margin:1rem}");
  (* Two properties of different families share no slot, so the merge stands:
     the guard reads footprints, not the presence of a nested block. *)
  Alcotest.(check string)
    "a nested block of a disjoint family still merges"
    ".a{color:red;padding:1rem;@media(width>=1px){margin:2rem}}"
    (canon ".a{color:red;@media (min-width:1px){margin:2rem}}.a{padding:1rem}");
  (* Nested blocks race in the order the merged rule replays them, and shorthand
     against longhand is such a race: swapping these two blocks changes
     margin-top wherever both queries apply. *)
  Alcotest.(check string)
    "nested blocks sharing a slot through a shorthand keep their order"
    ".a{padding:1rem;color:red;@media(width>=1px){margin:2rem}@media(width>=2px){margin-top:3rem}}"
    (canon
       ".a{padding:1rem;@media \
        (min-width:1px){margin:2rem}}.a{color:red;@media \
        (min-width:2px){margin-top:3rem}}")

(* Whether a nested body and a later declaration block share a slot reads the
   same whichever of the two is asked about: a custom property overlaps the same
   name and nothing else, [all] reaches every non-exempt slot, and a property
   the footprint model cannot place reaches whatever it meets. Each of those is
   stated over an ordered pair, so each is checked here from both ends. The
   shorthand-against-longhand pair is above. *)
let nested_merge_reads_the_slot_from_either_side () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "a later write of the nested custom property blocks the merge"
    ".a{color:red;&:hover{--x:2}}.a{--x:1}"
    (canon ".a{color:red;&:hover{--x:2}}.a{--x:1}");
  Alcotest.(check string)
    "another custom property still merges" ".a{--y:1;color:red;&:hover{--x:2}}"
    (canon ".a{color:red;&:hover{--x:2}}.a{--y:1}");
  Alcotest.(check string)
    "a custom property and a longhand share no slot"
    ".a{color:red;padding:1rem;&:hover{--x:2}}"
    (canon ".a{color:red;&:hover{--x:2}}.a{padding:1rem}");
  Alcotest.(check string)
    "nor do they the other way round"
    ".a{--x:1;color:red;&:hover{padding:2rem}}"
    (canon ".a{color:red;&:hover{padding:2rem}}.a{--x:1}");
  Alcotest.(check string)
    "a nested all reset blocks the merge"
    ".a{color:red;&:hover{all:unset}}.a{padding:1rem}"
    (canon ".a{color:red;&:hover{all:unset}}.a{padding:1rem}");
  Alcotest.(check string)
    "a nested unplaceable name blocks the merge"
    ".a{color:red;&:hover{unknown-thing:2}}.a{padding:1rem}"
    (canon ".a{color:red;&:hover{unknown-thing:2}}.a{padding:1rem}");
  Alcotest.(check string)
    "and so does a later one"
    ".a{color:red;&:hover{padding:2rem}}.a{unknown-thing:1}"
    (canon ".a{color:red;&:hover{padding:2rem}}.a{unknown-thing:1}")

(* A run of declarations written after a nested statement (CSS Nesting 1 sec.
   3.4) is a declaration list like any other, with nothing between two writes
   inside it, so the deduplication a rule body gets applies to it. *)
let nested_declaration_run_dedupes () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  (* The run cannot move: [color] is what the nested rule sets too. *)
  Alcotest.(check string)
    "a repeated declaration inside a run collapses" ".a{b{color:red}color:#00f}"
    (canon ".a{& b{color:red}color:blue;color:blue}");
  Alcotest.(check string)
    "a declaration the run itself overrides goes" ".a{b{color:red}color:green}"
    (canon ".a{& b{color:red}color:blue;color:green}");
  (* The crossed shorthand pins every one of the four longhands, so they compose
     where they stand. *)
  Alcotest.(check string)
    "longhands inside a run compose" ".a{b{margin:9px}margin:1px}"
    (canon
       ".a{& \
        b{margin:9px}margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}")

(* A selector list holding a vendor pseudo-element is invalidated as a whole by
   a browser that does not know it, so the other selectors silently lose the
   declarations. The grouping passes refuse to build such a list; one the author
   wrote is split so the risky branches stand alone. *)
let vendor_pseudo_list_is_split () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "the risky branches leave the list"
    ".i::-webkit-search-cancel-button{display:none}.i::-webkit-search-decoration{display:none}.i::-webkit-search-results-button,.reset{display:none}"
    (canon
       ".i::-webkit-search-cancel-button,.i::-webkit-search-decoration,.i::-webkit-search-results-button{display:none}.reset{display:none}");
  Alcotest.(check string)
    "a list of only risky branches is left alone"
    ".i::-webkit-search-cancel-button,.i::-webkit-search-decoration{display:none}"
    (canon
       ".i::-webkit-search-cancel-button,.i::-webkit-search-decoration{display:none}")

let c61_no_merge_intervening () =
  (* CSS Cascade 6.1: [.a] and the intervening [.b] tie on specificity (0,1,0),
     so source order is observable for any element matching both. A
     same-selector merge would put the combined [.a] rule on one side of [.b],
     changing that order, so it is blocked even though the moved property does
     not itself conflict. *)
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
  (* The two [.a] rules merge across the tied [.b]: [.b] writes [color], which
     pins the merged rule at the slot where [.b] still wins for [.a.b];
     [background-color] is uncontested, so joining it is cascade-neutral. The
     merged body uses canonical declaration order. The graph rewrite's
     acyclicity check is what proves this safe. *)
  Alcotest.(check string)
    "same selector merges across a tied intervening rule when safe"
    ".a{background-color:#00f;color:red}.b{color:#0f0}" output

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

let vendor_alias_pins_intervening_rule () =
  (* A?B?A across a vendor alias: the prefixed spelling writes the slot its
     unprefixed twin writes, so for an element matching [.b] and [.c] source
     order decides the winner. Grouping [.a] with [.c] moves the prefixed
     declaration in front of [.b] and flips it. Chrome 146 computes [transform:
     none] for the first sheet below and [matrix(...)] once the pair is grouped;
     Safari 26.5 agrees, and answers the same for [-webkit-hyphens], which
     Chrome does not implement. *)
  Alcotest.(check string)
    "a prefixed transform is not grouped across its unprefixed twin"
    ".a{-webkit-transform:none}.b{transform:rotate(45deg)}.c{-webkit-transform:none}"
    (optimized_string
       ".a{-webkit-transform:none}.b{transform:rotate(45deg)}.c{-webkit-transform:none}");
  Alcotest.(check string)
    "a prefixed hyphens is not grouped across its unprefixed twin"
    ".a{-webkit-hyphens:none}.b{-webkit-hyphens:auto;hyphens:auto}.c{-webkit-hyphens:none}"
    (optimized_string
       ".a{-webkit-hyphens:none}.b{hyphens:auto}.c{-webkit-hyphens:none}");
  Alcotest.(check string)
    "a prefixed mask longhand is not grouped across its unprefixed twin"
    ".a{-webkit-mask-size:auto}.b{-webkit-mask-size:cover;mask-size:cover}.c{-webkit-mask-size:auto}"
    (optimized_string
       ".a{-webkit-mask-size:auto}.b{mask-size:cover}.c{-webkit-mask-size:auto}");
  (* A prefix names one slot, not its whole family, so an unrelated intervening
     declaration still leaves the pair free to group. *)
  Alcotest.(check string)
    "a prefixed transform still groups across an unrelated declaration"
    ".a,.c{-webkit-transform:none}.b{color:red}"
    (optimized_string
       ".a{-webkit-transform:none}.b{color:red}.c{-webkit-transform:none}")

let unplaceable_name_pins_intervening_rule () =
  (* A property name the footprint model cannot place may be a shorthand, a
     legacy alias, or a longhand of a family the model does not carry, so it
     writes whatever slot the declaration it meets writes. Grouping [.a] with
     [.c] past [.b] changes which of the two wins for an element matching [.b]
     and [.c]: Chrome 146 computes [background-position: 10px 0%] for the first
     sheet below and [0% 0%] once the pair is grouped, and [overflow-wrap:
     break-word] against [normal] for the second. *)
  Alcotest.(check string)
    "an unplaceable longhand is not grouped across the shorthand resetting it"
    ".a{background-position-x:10px}.b{background:red}.c{background-position-x:10px}"
    (optimized_string
       ".a{background-position-x:10px}.b{background:red}.c{background-position-x:10px}");
  Alcotest.(check string)
    "an unplaceable legacy alias is not grouped across the property it aliases"
    ".a{word-wrap:break-word}.b{overflow-wrap:normal}.c{word-wrap:break-word}"
    (optimized_string
       ".a{word-wrap:break-word}.b{overflow-wrap:normal}.c{word-wrap:break-word}");
  (* A name the model does place keeps naming its own slot, so a rule holding
     one still groups across an unrelated declaration. *)
  Alcotest.(check string)
    "a placeable name still groups across an unrelated declaration"
    ".a,.c{margin-top:var(--a) var(--b)}.b{color:red}"
    (optimized_string
       ".a{margin-top:var(--a) var(--b)}.b{color:red}.c{margin-top:var(--a) \
        var(--b)}")

let c63_group_across_higher_specificity () =
  (* CSS Cascade 5 sec. 6.3: among the same origin, layer, and importance higher
     specificity wins regardless of source order; only a specificity tie defers
     to source order. [#m] (1,0,0) outranks [.a.x]/[.c.x] (0,2,0), so for any
     element matching all three (class="a c x" id="m") #m's blue wins whatever
     the order. The two equal [.a.x]/[.c.x] rules may therefore group across #m.
     Blocking the group because the class-pairs do not *beat* #m is
     over-conservative: differing specificity already makes the order
     unobservable. Contrast [aba_forbidden_intersection_dependency], where the
     intervening rule ties on specificity and the block is required. *)
  (* Pure cascade-safety reasoning, so it holds in both scopes. *)
  List.iter
    (fun scope ->
      let output =
        optimized_string ?scope ".a.x{color:red}#m{color:blue}.c.x{color:red}"
      in
      Alcotest.(check bool)
        "equal rules group across a higher-specificity intervening rule" true
        (Astring.String.is_infix ~affix:".a.x,.c.x{color:red}" output))
    [ None; Some `Stylesheet ]

let c63_merge_same_selector_across_higher_specificity () =
  (* Same reasoning for merging two equal-selector rules across a gap: [#footer]
     (1,0,0) outranks [body]/[html] (0,0,1), so font-size on a <body id=footer>
     resolves to #footer's 10pt whatever the order. The two body,html rules
     therefore merge across #footer - the case that motivates relaxing the
     intervening-overlap guard from "the merge selector beats the intervening
     one" to "their specificities differ". *)
  List.iter
    (fun scope ->
      let output =
        optimized_string ?scope
          "body,html{font-size:small}#footer{font-size:10pt}body,html{line-height:1.5}"
      in
      Alcotest.(check bool)
        "body,html merges across higher-specificity #footer" true
        (Astring.String.is_infix ~affix:"font-size:small;line-height:1.5" output))
    [ None; Some `Stylesheet ]

let factor_diff_value_descendant_tie_unsafe () =
  (* Collapsing the shared [float:left] of [.a .log] and [.b .hx] into one group
     is unsound: the merged rule G sits at a single source position while the
     intervening [.h .a] (float:right, a 0,2,0 tie with both branches) sits at
     another, and two elements demand opposite placements: E1 = <h><a><a.log> :
     matches [.a .log] (left) and [.h .a] (right). Source order is [.a .log]
     then [.h .a], so right wins, requiring G before [.h .a]. E2 = <b><h><a.hx>
     : matches [.b .hx] (left) and [.h .a] (right). Source order is [.h .a] then
     [.b .hx], so left wins, requiring G after [.h .a]. No single position for G
     satisfies both, so the two [float:left] branches must never share a group
     across [.h .a]. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".a .log{color:red;float:left;width:10px}.h .a{float:right}.b \
       .hx{color:red;float:left;width:20px}"
  in
  let unsafe_float_group =
    String.split_on_char '}' output
    |> List.exists (fun rule ->
        match Astring.String.cut ~sep:"{" rule with
        | Some (selector, decls) ->
            Astring.String.is_infix ~affix:".a .log" selector
            && Astring.String.is_infix ~affix:".b .hx" selector
            && Astring.String.is_infix ~affix:"float:left" decls
        | None -> false)
  in
  Alcotest.(check bool)
    "the two float:left branches never share a group across the float:right tie"
    false unsafe_float_group

let factor_same_value_descendant_tie_groups () =
  (* Here [.h .a] writes the same [float:left]. Reordering identical-value
     declarations is unobservable, so the shared block factors across the
     tie. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".a .log{color:red;float:left;width:10px}.h .a{float:left}.b \
       .hx{color:red;float:left;width:20px}"
  in
  Alcotest.(check bool)
    "same-value descendant-combinator tie allows factoring" true
    (Astring.String.is_infix ~affix:".a .log,.b .hx" output)

let factor_prefers_size_minimal_subset () =
  (* Hoisting a shared declaration is profitable for a member only when its
     selector entry (len+1) is cheaper than the bytes it would otherwise
     duplicate ("position:absolute;" = 18). [.very-long-closebtn-selector] (28)
     is a net loss, so it keeps [position:absolute] inline while the three short
     selectors form the shared group. *)
  let output =
    optimized_string ~scope:`Stylesheet
      "#f{top:0;position:absolute;width:100%}.b{padding:16px;position:absolute;z-index:1}.b-arrow{position:absolute}.very-long-closebtn-selector{height:21px;position:absolute;width:21px}"
  in
  Alcotest.(check bool)
    "short selectors group; long selector keeps position:absolute inline" true
    (Astring.String.is_infix ~affix:"#f,.b,.b-arrow{position:absolute}" output
    && Astring.String.is_infix
         ~affix:
           ".very-long-closebtn-selector{height:21px;position:absolute;width:21px}"
         output)

let factor_steps_over_same_value_nesting_boundary () =
  (* A rule carrying a nested rule is a factor boundary, but the
     [position:absolute] scan steps over [.jfk-bubble-closebtn] (which nests
     [&:focus]) to reach [.jfk-bubble-arrow]. Stepping over it is sound:
     closebtn carries the same [position:absolute] and its nested [&:focus] sets
     only opacity, so reordering position across it is unobservable. The three
     same-value members group while closebtn stays inline with its nested
     rule. *)
  let output =
    optimized_string ~scope:`Stylesheet
      "#footer{position:absolute;width:100%}.jfk-bubble{padding:16px;position:absolute}.jfk-bubble-closebtn{position:absolute;right:2px;&:focus{opacity:.8}}.jfk-bubble-arrow{position:absolute}"
  in
  Alcotest.(check bool)
    "cluster groups across the same-value nesting boundary" true
    (Astring.String.is_infix
       ~affix:"#footer,.jfk-bubble,.jfk-bubble-arrow{position:absolute}" output)

let factor_conflicting_nesting_boundary_unsafe () =
  (* The boundary [.bnd] nests a rule and also sets [position:relative], a 0,1,0
     tie with [.m1]/[.m2]'s [position:absolute]. For an element matching [.m2]
     and [.bnd], source order ([.bnd] before [.m2]) makes absolute win; hoisting
     [.m1] and [.m2] into a group before [.bnd] flips it to relative. So the
     scan steps over a nesting boundary only when it shares the factored value,
     never when it conflicts. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".m1{position:absolute}.bnd{position:relative;&:hover{color:red}}.m2{position:absolute}"
  in
  let unsafe_group =
    String.split_on_char '}' output
    |> List.exists (fun rule ->
        match Astring.String.cut ~sep:"{" rule with
        | Some (selector, decls) ->
            Astring.String.is_infix ~affix:".m1" selector
            && Astring.String.is_infix ~affix:".m2" selector
            && Astring.String.is_infix ~affix:"position:absolute" decls
        | None -> false)
  in
  Alcotest.(check bool)
    "no position:absolute group spans .m1 and .m2 across the conflicting \
     boundary"
    false unsafe_group

let factor_inlines_rather_than_grouping_long_selector () =
  (* outlook-3 gap: when a declaration is already shared in source between a
     long selector and a sibling, keeping it grouped repeats the long selector
     in the group list. Inlining it into the sibling rules costs only one extra
     copy of the declaration per rule, which is cheaper once the selector
     exceeds the declaration. Here [.really-long-block-name .primary] (>>15
     chars) should keep [padding-top:1px] inline alongside its [font-size], not
     share a [padding-top] group with [.really-long-block-name .secondary]. *)
  let output =
    optimized_string ~scope:`Stylesheet
      ".really-long-block-name .primary{font-size:14px}.really-long-block-name \
       .primary,.really-long-block-name .secondary{padding-top:1px}"
  in
  let costly_padding_group =
    String.split_on_char '}' output
    |> List.exists (fun rule ->
        match Astring.String.cut ~sep:"{" rule with
        | Some (selector, decls) ->
            Astring.String.is_infix ~affix:".primary" selector
            && Astring.String.is_infix ~affix:".secondary" selector
            && Astring.String.is_infix ~affix:"padding-top" decls
        | None -> false)
  in
  Alcotest.(check bool)
    "padding-top is inlined, not grouped across the long selector" false
    costly_padding_group

let factor_groups_single_property_past_other_shared_decl () =
  (* The anchor [#footer] shares [width] with [.fll] and [position] with the
     [.jfk-bubble] cluster. A scan that commits to the first shared declaration
     it meets would lock onto [width] at [.fll] and never group [position] with
     the cluster. Seeding the scan per anchor declaration recovers the
     [position] group across the intervening [.fll]. *)
  let output =
    optimized_string ~scope:`Stylesheet
      "#footer{bottom:0;position:absolute;width:100%}.fll{float:right;width:100%}.jfk-bubble{padding:16px;position:absolute}.jfk-bubble-arrow{position:absolute}"
  in
  Alcotest.(check bool)
    "position groups across .fll which shares only width" true
    (Astring.String.is_infix
       ~affix:"#footer,.jfk-bubble,.jfk-bubble-arrow{position:absolute}" output)

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
      Css.media ~condition:(media_min_width 48.)
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
    "@media not (width>=1024px){.u{display:flex}}@media not \
     (width>=640px){.u{display:grid}}"
    (optimized_string
       "@media not all and (min-width:1024px){.u{display:flex}}@media not all \
        and (min-width:640px){.u{display:grid}}");
  Alcotest.(check string)
    "reverse authored order is also preserved"
    "@media not (width>=640px){.u{display:grid}}@media not \
     (width>=1024px){.u{display:flex}}"
    (optimized_string
       "@media not all and (min-width:640px){.u{display:grid}}@media not all \
        and (min-width:1024px){.u{display:flex}}")

(* CSS Syntax 3 sec. 5.4.4/5.4.5: a declaration inside a nested block runs to
   the next [;] or to the block's [}], so a declaration followed by a nested
   at-rule or nested rule owes it a separator; without one the declaration runs
   on into the next sibling and the reader drops the rule. [Css.of_string
   ~strict:true] is the gate: it turns any recovery warning into an error. *)
let nested_block_separator () =
  let check name input expected =
    let output = optimized_string input in
    Alcotest.(check string) name expected output;
    match Css.of_string ~strict:true output with
    | Ok _ -> ()
    | Error e ->
        Alcotest.failf "%s: emitted CSS %S is rejected by the reader: %s" name
          output (Error.to_string e)
  in
  check "adjacent same-condition blocks merge"
    ".a{@supports (color:red){color:red}@supports (color:red){top:0}}"
    ".a{@supports(color:red){color:red;top:0}}";
  check "three blocks in a row"
    ".a{@supports (color:red){color:red}@supports (color:red){top:0}@supports \
     (color:red){left:0}}"
    ".a{@supports(color:red){color:red;top:0;left:0}}";
  check "nested at-rule before a nested rule"
    ".a{@supports (color:red){color:red}.b{top:0}}"
    ".a{@supports(color:red){color:red}.b{top:0}}";
  check "two nested at-rules"
    ".a{@supports (color:red){color:red}@media print{top:0}}"
    ".a{@supports(color:red){color:red}@media print{top:0}}";
  check "declaration then nested at-rule then nested rule"
    ".a{top:0;@supports (color:red){color:red}.b{left:0}}"
    ".a{top:0;@supports(color:red){color:red}.b{left:0}}"

let target_minify_enforce_spec_split () =
  let check_modes name input ~default ~spec =
    Alcotest.(check string) (name ^ " default") default (optimized_string input);
    Alcotest.(check string)
      (name ^ " enforce-spec") spec
      (optimized_string ~enforce_spec:true input)
  in
  check_modes "explicit flex zero basis is not one-value shorthand"
    "a { flex: 1 1 0 }" ~default:"a{flex:1 1 0}" ~spec:"a{flex:1 1 0}";
  check_modes "color-mix keeps the required in oklab in both modes"
    "a { color: color-mix(in oklab, var(--a), var(--b)) }"
    ~default:"a{color:color-mix(in oklab,var(--a),var(--b))}"
    ~spec:"a{color:color-mix(in oklab,var(--a),var(--b))}";
  check_modes "computed oklch lightness keeps its numeric form"
    "a { color: color-mix(in oklch, red 50%, blue 50%) }"
    ~default:"a{color:oklch(.54 .285 326.643)}"
    ~spec:"a{color:oklch(.54 .285 326.643)}";
  (* oklch/oklab chroma takes <number> | <percentage>, exactly interchangeable
     (CSS Color 4: 100% = 0.4 on this axis). Default minify picks the shorter
     spelling per axis - .304 becomes 76%, but .1 stays because 25% is longer.
     --enforce-spec emits the spec-canonical resolved form, which is the number,
     and never introduces a percentage even when it would be shorter. *)
  check_modes "oklch chroma uses the shorter percentage on the default target"
    "a { color: oklch(.5 .304 200) }" ~default:"a{color:oklch(.5 76%200)}"
    ~spec:"a{color:oklch(.5 .304 200)}";
  check_modes
    "oklch chroma keeps the number when the number is already shortest"
    "a { color: oklch(.5 .1 200) }" ~default:"a{color:oklch(.5 .1 200)}"
    ~spec:"a{color:oklch(.5 .1 200)}";
  check_modes "enforce-spec renders oklch chroma as the canonical number"
    "a { color: oklch(.5 76% 200) }" ~default:"a{color:oklch(.5 76%200)}"
    ~spec:"a{color:oklch(.5 .304 200)}";
  check_modes "media min-width grammar"
    "@media (min-width: 700px) { a { color: red } }"
    ~default:"@media(width>=700px){a{color:red}}"
    ~spec:"@media(min-width:700px){a{color:red}}";
  check_modes "media not all min-width grammar"
    "@media not all and (min-width: 700px) { a { color: red } }"
    ~default:"@media not (width>=700px){a{color:red}}"
    ~spec:"@media not all and (min-width:700px){a{color:red}}";
  check_modes "media interval grammar"
    "@media (min-width: 768px) and (max-width: 1024px) { a { color: red } }"
    ~default:"@media(768px<=width<=1024px){a{color:red}}"
    ~spec:"@media(min-width:768px)and (max-width:1024px){a{color:red}}";
  check_modes "container min-width grammar"
    "@container sidebar (min-width: 700px) { a { color: red } }"
    ~default:"@container sidebar (width>=700px){a{color:red}}"
    ~spec:"@container sidebar (min-width:700px){a{color:red}}"

let target_evergreen_compatibility_prefixes () =
  Alcotest.(check string)
    "the declared target adds WebKit declaration fallbacks"
    ".a{-webkit-user-select:none;user-select:none;-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}"
    (optimized_string ".a{user-select:none;backdrop-filter:blur(1px)}");
  Alcotest.(check string)
    "the declared target adds WebKit hyphens and mask longhand fallbacks"
    ".a{-webkit-hyphens:auto;hyphens:auto;-webkit-mask-image:none;mask-image:none;-webkit-mask-position:50%;mask-position:50%;-webkit-mask-size:cover;mask-size:cover;-webkit-mask-repeat:no-repeat;mask-repeat:no-repeat;-webkit-mask-clip:border-box;mask-clip:border-box;-webkit-mask-origin:content-box;mask-origin:content-box}"
    (optimized_string
       ".a{hyphens:auto;mask-image:none;mask-position:center;mask-size:cover;mask-repeat:no-repeat;mask-clip:border-box;mask-origin:content-box}");
  Alcotest.(check string)
    "the declared target adds the WebKit mask shorthand"
    ".a{-webkit-mask:none;mask:none}"
    (optimized_string ".a{mask:none}");
  Alcotest.(check string)
    "the WebKit mask shorthand omits fields with different legacy grammars"
    ".a{-webkit-mask:url(a.svg);mask:url(a.svg)luminance add}"
    (optimized_string ".a{mask:url(a.svg) luminance add}");
  Alcotest.(check string)
    "mask fields without equivalent WebKit grammars are not prefixed"
    ".a{mask-mode:luminance;mask-composite:add}"
    (optimized_string ".a{mask-mode:luminance;mask-composite:add}");
  Alcotest.(check string)
    "the declaration fallback preserves importance"
    ".a{-webkit-user-select:none!important;user-select:none!important}"
    (optimized_string ".a{user-select:none!important}");
  Alcotest.(check string)
    "the declared target prefixes feature tests and their declarations"
    "@supports(-webkit-backdrop-filter:var(--tw))or \
     (backdrop-filter:var(--tw)){.a{-webkit-backdrop-filter:var(--tw);backdrop-filter:var(--tw)}}"
    (optimized_string
       "@supports(backdrop-filter:var(--tw)){.a{backdrop-filter:var(--tw)}}");
  Alcotest.(check string)
    "the declared target prefixes mask feature tests and declarations"
    "@supports(-webkit-mask:none)or \
     (mask:none){.a{-webkit-mask:none;mask:none}}"
    (optimized_string "@supports(mask:none){.a{mask:none}}");
  Alcotest.(check string)
    "an authored fallback is not duplicated"
    ".a{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}"
    (optimized_string
       ".a{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}");
  Alcotest.(check string)
    "an authored mask fallback is not duplicated"
    ".a{-webkit-mask-size:contain;mask-size:cover}"
    (optimized_string ".a{-webkit-mask-size:contain;mask-size:cover}");
  Alcotest.(check string)
    "a synthesized mask shorthand preserves importance"
    ".a{-webkit-mask:none!important;mask:none!important}"
    (optimized_string ".a{mask:none!important}");
  Alcotest.(check string)
    "an unresolved decoration color takes the WebKit spelling too"
    ".prose \
     a{text-decoration:underline;-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}"
    (optimized_string
       ".prose{a{text-decoration:underline;text-decoration-color:var(--c)}}");
  Alcotest.(check string)
    "a decoration color read as a colour keeps the standard property alone"
    ".a{text-decoration-color:#00f}"
    (optimized_string ".a{text-decoration-color:blue}");
  Alcotest.(check string)
    "the decoration color fallback preserves importance"
    ".a{-webkit-text-decoration-color:var(--c)!important;text-decoration-color:var(--c)!important}"
    (optimized_string ".a{text-decoration-color:var(--c)!important}");
  Alcotest.(check string)
    "an authored decoration color fallback is not supplemented"
    ".a{-webkit-text-decoration-color:red;text-decoration-color:var(--c)}"
    (optimized_string
       ".a{-webkit-text-decoration-color:red;text-decoration-color:var(--c)}");
  Alcotest.(check string)
    "the declared target prefixes a decoration color feature test"
    "@supports(-webkit-text-decoration-color:var(--c))or \
     (text-decoration-color:var(--c)){.a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}}"
    (optimized_string
       "@supports(text-decoration-color:var(--c)){.a{text-decoration-color:var(--c)}}");
  Alcotest.(check string)
    "spec-only mode does not synthesize target fallbacks"
    ".a{user-select:none;backdrop-filter:blur(1px);hyphens:auto;mask:none}"
    (optimized_string ~enforce_spec:true
       ".a{user-select:none;backdrop-filter:blur(1px);hyphens:auto;mask:none}");
  let newer_webkit =
    {
      Css.Optimize.evergreen_targets with
      safari = (18, 0);
      ios_safari = (18, 0);
    }
  in
  Alcotest.(check string)
    "a newer WebKit target drops only the obsolete fallback"
    ".a{-webkit-user-select:none;user-select:none;backdrop-filter:blur(1px)}"
    (optimized_string ~targets:newer_webkit
       ".a{user-select:none;backdrop-filter:blur(1px)}");
  let safari_16_6 =
    {
      Css.Optimize.evergreen_targets with
      chrome = (120, 0);
      safari = (16, 6);
      ios_safari = (17, 0);
    }
  in
  Alcotest.(check string)
    "Safari 16.6 still receives the hyphens fallback"
    ".a{-webkit-hyphens:auto;hyphens:auto}"
    (optimized_string ~targets:safari_16_6 ".a{hyphens:auto}");
  let ios_safari_16_7 =
    {
      Css.Optimize.evergreen_targets with
      chrome = (120, 0);
      safari = (17, 0);
      ios_safari = (16, 7);
    }
  in
  Alcotest.(check string)
    "iOS Safari 16.7 still receives the hyphens fallback"
    ".a{-webkit-hyphens:auto;hyphens:auto}"
    (optimized_string ~targets:ios_safari_16_7 ".a{hyphens:auto}");
  let targets_past_hyphens_and_mask_prefixes =
    {
      Css.Optimize.evergreen_targets with
      chrome = (120, 0);
      safari = (17, 0);
      ios_safari = (17, 0);
    }
  in
  Alcotest.(check string)
    "targets past the compatibility boundaries omit hyphens and mask prefixes"
    ".a{hyphens:auto;mask-image:none}"
    (optimized_string ~targets:targets_past_hyphens_and_mask_prefixes
       ".a{hyphens:auto;mask-image:none}");
  let safari_26_1 =
    {
      Css.Optimize.evergreen_targets with
      safari = (26, 1);
      ios_safari = (26, 1);
    }
  in
  Alcotest.(check string)
    "Safari 26.1 still receives the decoration color fallback"
    ".a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}"
    (optimized_string ~targets:safari_26_1 ".a{text-decoration-color:var(--c)}");
  let ios_safari_26_1 =
    {
      Css.Optimize.evergreen_targets with
      safari = (26, 2);
      ios_safari = (26, 1);
    }
  in
  Alcotest.(check string)
    "iOS Safari 26.1 still receives the decoration color fallback"
    ".a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}"
    (optimized_string ~targets:ios_safari_26_1
       ".a{text-decoration-color:var(--c)}");
  let targets_past_decoration_color_prefix =
    {
      Css.Optimize.evergreen_targets with
      safari = (26, 2);
      ios_safari = (26, 2);
    }
  in
  Alcotest.(check string)
    "targets past the decoration color boundary omit its fallback"
    ".a{text-decoration-color:var(--c)}"
    (optimized_string ~targets:targets_past_decoration_color_prefix
       ".a{text-decoration-color:var(--c)}")

let c61_no_layer_media_merge () =
  (* CSS Cascade section 6.4.4.2: a layer statement between matching media
     queries still establishes layer order at that point. Media-query merging
     must not cross it. *)
  let media_rule selector color =
    Css.media ~condition:(media_min_width 48.)
      [
        Css.rule
          ~selector:(Css.Selector.class_ selector)
          [ Css.Declaration.color (hex_color color) ];
      ]
  in
  let input =
    [
      media_rule "a" "ff0000";
      Css.Stylesheet.Layer_decl [ [ "theme" ] ];
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
        ( Some [ "reset" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "btn")
              [ Css.Declaration.display Block ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "components" ],
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
      Css.Stylesheet.Layer_decl [ [ "reset" ]; [ "components" ] ];
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
        ( Some [ "reset" ],
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
        ( Some [ "base" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "btn")
              [
                Css.Declaration.important
                  (Css.Declaration.color (hex_color "ff0000"));
              ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "theme" ],
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

let c61_group_across_higher_specificity_competitor () =
  (* CSS Cascade 6.1: specificity is evaluated before source order. The
     intervening [.item.active] (0,2,0) strictly outranks [.item]/[.active]
     (0,1,0), and every element matching both [.item] and [.active] also matches
     [.item.active] - which wins regardless of order. So grouping the two equal
     red rules across it is unobservable. Contrast the tie cases
     ([c61_no_group_nonadjacent]), where the competitor ties and blocking is
     required. *)
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
    "equal rules group across a strictly-higher-specificity competitor"
    ".active,.item{color:red}.item.active{color:#00f}" output

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
        ( Some (Css.Selector.of_string ".component"),
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
   Css.Stylesheet.Scope (Some start, None, [ scoped_stmt ]);
   after_stmt;
  ]
    when Css.Selector.to_string ~minify:true start = ".component" ->
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
      Css.Stylesheet.Scope
        (Some (Css.Selector.of_string ".outer"), None, [ scoped_rule ]);
      Css.Stylesheet.Scope
        (Some (Css.Selector.of_string ".inner"), None, [ scoped_rule ]);
    ]
  in
  let optimized = Css.Optimize.stylesheet input in
  match optimized with
  | [
   Css.Stylesheet.Scope (Some outer_sel, None, [ outer_stmt ]);
   Css.Stylesheet.Scope (Some inner_sel, None, [ inner_stmt ]);
  ]
    when Css.Selector.to_string ~minify:true outer_sel = ".outer"
         && Css.Selector.to_string ~minify:true inner_sel = ".inner" ->
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
      Css.Stylesheet.Scope
        ( Some (Css.Selector.of_string ".card"),
          Some (Css.Selector.of_string ".footer"),
          [ scoped_rule ] );
      Css.Stylesheet.Scope
        ( Some (Css.Selector.of_string ".card"),
          Some (Css.Selector.of_string ".aside"),
          [ scoped_rule ] );
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
  (* CSS Cascade section 6.1 makes order of appearance a cascade criterion, and
     an [@supports] block decides whether the declarations it holds apply at
     all, so the optimizer reads it as a boundary: the two [.card] rules stay on
     either side of it rather than merging across. *)
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
    "supports boundary blocks the merge"
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
  (* A misplaced @import - one that follows a style rule - is invalid, and every
     browser ignores it (CSS Cascade 5 section 2, CSS 2.1 section 6.3). It is a
     no-op, not a cascade boundary and not a live substitution point: an invalid
     import is never resolved, even under inline-imports. So the optimizer drops
     it, and the now-adjacent same-selector rules merge. Matches browsers and
     csso, and stays idempotent in a single pass. *)
  let stylesheet =
    match
      Css.of_string ~strict:false
        ".theme{color:red}@import url(\"base.css\");.theme{display:flex}"
    with
    | Ok { Css.stylesheet; _ } -> stylesheet
    | Error e -> Alcotest.failf "lenient parse failed: %s" (Error.to_string e)
  in
  let output = minify stylesheet |> String.trim in
  Alcotest.(check string)
    "misplaced @import is a no-op: dropped, surrounding rules merge"
    ".theme{color:red;display:flex}" output

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

let preserve_independent_custom_prop_position () =
  let opt css =
    Css.Stylesheet.read (Cursor.of_string css)
    |> Css.Optimize.stylesheet
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  (* A rule of only globally-unique custom properties is position-independent
     for the cascade, but the optimiser keeps source order for rules it has no
     cascade reason to reorder rather than inventing a canonical position. *)
  Alcotest.(check string)
    "independent :root keeps its source position"
    ".a{width:var(--spacing)}:root{--spacing:.25rem}"
    (opt ".a{width:var(--spacing)}:root{--spacing:.25rem}");
  (* A custom property declared twice is order-significant: the two orderings
     have different effective values, so they must not collapse together. *)
  Alcotest.(check bool)
    "conflicting redefinitions stay order-significant" false
    (String.equal
       (opt ":root{--spacing:1rem}.a{width:1px}:root{--spacing:2rem}")
       (opt ":root{--spacing:2rem}.a{width:1px}:root{--spacing:1rem}"));
  (* @property is an anchor: a use is not reordered before its registration. *)
  Alcotest.(check string)
    "custom-property use stays after its @property"
    "@property --x{syntax:\"*\";inherits:false}.y{--x:1}"
    (opt "@property --x{syntax:\"*\";inherits:false}.y{--x:1}")

let calc_flatten_registered_single_valued () =
  let check label css expected =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized = Css.Optimize.stylesheet input in
    Alcotest.(check string)
      label expected
      (Css.Stylesheet.to_string ~minify:true optimized |> String.trim)
  in
  (* CSS Properties and Values API 1: a single-component [@property] syntax
     substitutes exactly one calc term, so a redundant nested [calc(var)] folds
     to a bare reference. *)
  check "single-valued registration flattens the nested calc"
    "@property \
     --x{syntax:\"<length>\";inherits:false;initial-value:0px}.a{width:calc(calc(var(--x)) \
     * 2)}"
    "@property \
     --x{syntax:\"<length>\";inherits:false;initial-value:0px}.a{width:calc(var(--x)*2)}";
  (* CSS Values 4 sec. 10.10: an unregistered var() could substitute a
     multi-term value, so the grouping must stay. *)
  check "unregistered var keeps the nested calc"
    ".a{width:calc(calc(var(--x)) * 2)}" ".a{width:calc(calc(var(--x))*2)}";
  (* A universal syntax is not a single term, so it is not unwrapped. *)
  check "universal-syntax registration keeps the nested calc"
    "@property --x{syntax:\"*\";inherits:false}.a{width:calc(calc(var(--x)) * \
     2)}"
    "@property --x{syntax:\"*\";inherits:false}.a{width:calc(calc(var(--x))*2)}"

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
    ".card{.title{color:red}@scope(&)to (.boundary){& \
     .title{display:block}}.title{padding:1rem}}"

let c61_nesting_synthesis_source_order () =
  (* CSS Cascade section 6.1: order of appearance is a cascade criterion.
     Synthesizing [B] as a nested rule under an earlier [A] is only a
     serialization change when [B] is adjacent to [A] in the same cascade
     context. Non-adjacent synthesis moves [B] before intervening rules in the
     stylesheet tree, so the optimizer must leave those rules as siblings even
     when [B]'s selector extends [A]. *)
  let run_case ((label, css, expected) : string * string * string) :
      string option =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized = Css.Optimize.stylesheet input in
    let output =
      Css.Stylesheet.to_string ~minify:true optimized |> String.trim
    in
    if String.equal output expected then None
    else
      Fmt.kstr
        (fun s -> Some s)
        "%s\n  input:    %S\n  expected: %S\n  actual:   %S" label css expected
        output
  in
  let mismatches =
    List.filter_map run_case
      [
        ( "adjacent extended selector may synthesize nesting",
          ".card{color:red}.card .title{color:blue}",
          ".card{color:red;.title{color:#00f}}" );
        ( "same-context adjacent synthesis is allowed inside media",
          "@media(min-width:40em){.card{color:red}.card .title{color:blue}}",
          "@media(width>=40em){.card{color:red;.title{color:#00f}}}" );
        (* The intervening [.other] is cascade-neutral, but source-stable
           emission does not move the nested child across it. *)
        ( "non-competing intervening rule preserves source order",
          ".card{color:red}.other{display:block}.card .title{color:blue}",
          ".card{color:red}.other{display:block}.card .title{color:#00f}" );
        ( "intervening cascade competitor blocks nesting synthesis",
          ".card{color:red}.card:hover{color:green}.card .title{color:blue}",
          ".card{color:red}.card:hover{color:green}.card .title{color:#00f}" );
        ( "conditional boundary blocks nesting synthesis",
          ".card{color:red}@media(min-width:40em){.card .title{color:blue}}",
          ".card{color:red}@media(width>=40em){.card .title{color:#00f}}" );
        ( "supports boundary blocks nesting synthesis",
          ".card{color:red}@supports(display:grid){.card{display:grid}}.card \
           .title{color:blue}",
          ".card{color:red}@supports(display:grid){.card{display:grid}}.card \
           .title{color:#00f}" );
      ]
  in
  match mismatches with
  | [] -> ()
  | _ ->
      Alcotest.failf "nesting synthesis source-order oracle mismatches:\n%s"
        (String.concat "\n" mismatches)

let c61_group_across_pseudo_competitor () =
  (* CSS Cascade 6.1: [.btn:hover] (0,2,0) strictly outranks [.btn]/[.link]
     (0,1,0), so on any element matching the group it wins regardless of order.
     The two equal red rules group, and [.btn:hover] stays a separate rule (it
     is not nested under the group, so it never applies to [.link]). *)
  let input =
    Css.Stylesheet.read
      (Cursor.of_string ".btn{color:red}.btn:hover{color:blue}.link{color:red}")
  in
  let optimized = Css.Optimize.stylesheet input in
  let output = Css.Stylesheet.to_string ~minify:true optimized |> String.trim in
  Alcotest.(check string)
    "equal rules group across a higher-specificity pseudo competitor"
    ".btn,.link{color:red}.btn:hover{color:#00f}" output

let c61_no_conditional_cli_merge () =
  (* Each conditional boundary filters its declarations independently, so none
     of them may be crossed by a merge. *)
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
    "every conditional boundary blocks the merge"
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
  (* A misplaced @import (after a style rule) is an invalid no-op the optimizer
     drops; the surrounding same-selector rules then merge, and the merge stays
     inside the author-origin wrapper since both rules share that origin (CSS
     Cascade 5 section 6.2). *)
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
  | [ Css.Stylesheet.Origin (Author, [ merged ]) ] ->
      Alcotest.(check string)
        "import neighbors merge into one rule within the author origin"
        ".theme{color:red;display:flex}"
        (Css.Stylesheet.to_string ~minify:true [ merged ] |> String.trim)
  | _ ->
      Alcotest.fail
        "import neighbors should merge into one rule inside the author origin"

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
    ".alert{color:#f00!important}" output

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
           selector = Css.Keyframe.Positions [ Css.Keyframe.From ];
           declarations = from_decls;
         };
         {
           selector = Css.Keyframe.Positions [ Css.Keyframe.To ];
           declarations = to_decls;
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
      Css.Stylesheet.Layer_decl [ [ "default" ]; [ "theme" ]; [ "components" ] ];
      Css.Stylesheet.Layer
        ( Some [ "theme" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "widget")
              [ Css.Declaration.color (hex_color "0000ff") ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "default" ],
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
        ( Some [ "reset" ],
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
      Css.Stylesheet.Layer_decl [ [ "defaults" ]; [ "overrides" ] ];
      Css.Stylesheet.Layer
        ( Some [ "defaults" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "notice")
              [
                Css.Declaration.important
                  (Css.Declaration.color (hex_color "ff0000"));
              ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "overrides" ],
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
        ( Some [ "base" ],
          [
            Css.rule ~selector:(Css.Selector.element "p")
              [ Css.Declaration.max_width (Ch 70.) ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "framework" ],
          [
            Css.Stylesheet.Layer
              ( Some [ "base" ],
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
   Css.Stylesheet.Layer (Some [ "base" ], [ base_stmt ]);
   Css.Stylesheet.Layer
     ( Some [ "framework" ],
       [ Css.Stylesheet.Layer (Some [ "base" ], [ framework_base_stmt ]) ] );
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
      Css.Stylesheet.selector = Css.Keyframe.Positions [ Css.Keyframe.From ];
      declarations = [ decl ];
    }
  in
  let input =
    [
      Css.Stylesheet.Layer_decl [ [ "framework" ]; [ "override" ] ];
      Css.Stylesheet.Layer
        ( Some [ "override" ],
          [
            Css.Stylesheet.Keyframes
              ( "slide-left",
                [ frame (Css.Declaration.opacity (Opacity_number 0.)) ] );
          ] );
      Css.Stylesheet.Layer
        ( Some [ "framework" ],
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
      Css.Stylesheet.Layer_decl [ [ "default" ] ];
      Css.Stylesheet.Import
        {
          url = "url(\"theme.css\")";
          layer = Some [ "theme" ];
          supports = None;
          media = None;
        };
      Css.Stylesheet.Layer_decl [ [ "components" ] ];
      Css.Stylesheet.Layer
        ( Some [ "default" ],
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
        ( Some [ "base" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "button")
              [ Css.Declaration.color (hex_color "ff0000") ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "theme" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "button")
              [ Css.Declaration.color (hex_color "0000ff") ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "base" ],
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
              ( Some [ "foo" ],
                [
                  Css.rule
                    ~selector:(Css.Selector.class_ "inside")
                    [ Css.Declaration.color (hex_color "ff0000") ];
                ] );
            Css.Stylesheet.Layer
              ( Some [ "foo" ],
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
     Cascade L5 sec. 6.4.2.1, so collapsing is spec-allowed. *)
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
            ( Some [ "foo" ],
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
     (None, [ Css.Stylesheet.Layer (Some [ "foo" ], [ first_stmt ]) ]);
   Css.Stylesheet.Layer
     (None, [ Css.Stylesheet.Layer (Some [ "foo" ], [ second_stmt ]) ]);
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
          Css.layer ~name:[ "layout" ]
            [
              Css.rule
                ~selector:(Css.Selector.class_ "title")
                [ Css.Declaration.font_size (Rem 2.) ];
            ];
        ];
      Css.supports
        ~condition:(Css.Supports.property "display" "grid")
        [
          Css.Stylesheet.Layer_decl [ [ "grid" ] ];
          Css.layer ~name:[ "grid" ]
            [
              Css.rule
                ~selector:(Css.Selector.class_ "title")
                [ Css.Declaration.display Grid ];
            ];
        ];
      Css.Stylesheet.Layer_decl [ [ "theme" ]; [ "layout" ] ];
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
      Css.Stylesheet.Layer (Some [ "reset" ], []);
      Css.Stylesheet.Layer
        ( Some [ "components" ],
          [
            Css.rule
              ~selector:(Css.Selector.class_ "card")
              [ Css.Declaration.display Flex ];
          ] );
      Css.Stylesheet.Layer
        ( Some [ "reset" ],
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

(* CSS Properties & Values API registrations are document-global, but generated
   CSS often treats emitted registration order as part of its textual contract.
   Preserve it instead of sorting into a canonical by-name order. *)
let test_property_source_order () =
  Alcotest.(check string)
    "unique @property run keeps source order"
    {|@property --z{syntax:"*";inherits:false}@property --a{syntax:"*";inherits:false}|}
    (minify_str
       {|@property --z{syntax:"*";inherits:false}@property --a{syntax:"*";inherits:false}|});
  Alcotest.(check string)
    "@property runs keep source order within rule-bounded segments"
    {|@property --m{syntax:"*";inherits:false}@property --a{syntax:"*";inherits:false}.x{color:red}|}
    (minify_str
       {|@property --m{syntax:"*";inherits:false}@property --a{syntax:"*";inherits:false}.x{color:red}|})

let selector_merging_tests =
  [
    ("property source order", `Quick, test_property_source_order);
    ("merge consecutive identical", `Quick, test_merge_consecutive_identical);
    ( "combine identical oklab(none) rules",
      `Quick,
      test_combine_identical_oklab_none );
    ( "no merge different declarations",
      `Quick,
      test_no_merge_different_declarations );
    ( "merge non-consecutive non-conflicting",
      `Quick,
      test_merge_non_consecutive_non_conflicting );
    ("factor shared declarations", `Quick, test_factor_shared_declarations);
    ("factor weighted intervals", `Quick, test_factor_interval_schedule);
    ("factor interval overrides", `Quick, test_factor_interval_keeps_overrides);
    ( "factor selector branch keeps later override",
      `Quick,
      test_factor_selector_branch_keeps_later_override );
    ( "factoring reaches fixpoint in one pass",
      `Quick,
      test_factoring_reaches_fixpoint );
    ( "large stylesheet reaches local fixpoint",
      `Quick,
      test_large_stylesheet_reaches_local_fixpoint );
    ( "large stylesheet normalizes vendor aliases",
      `Quick,
      test_large_stylesheet_normalizes_vendor_aliases );
    ( "large stylesheet factoring reaches fixpoint",
      `Quick,
      test_large_stylesheet_factoring_reaches_fixpoint );
    ("no factor across conflict", `Quick, test_no_factor_across_conflict);
    ( "zero box side covered by shorthand",
      `Quick,
      test_zero_box_side_covered_by_shorthand );
    ( "0s transition keeps its property",
      `Quick,
      test_keep_zero_duration_transition );
    ("var border keeps significant spaces", `Quick, test_keep_var_border_spaces);
    ( "custom value folds whitespace after a hard close-paren",
      `Quick,
      test_custom_value_close_paren_whitespace );
    ("distant @media merge when safe", `Quick, test_distant_media_merge);
    ("distant @container merge when safe", `Quick, test_distant_container_merge);
    ( "leading bang comment stays leading",
      `Quick,
      test_keep_bang_comment_leading );
    ("pp keeps distinct nodes distinct", `Quick, test_pp_keeps_distinct_nodes);
    ( "optimize unifies equivalent nodes",
      `Quick,
      test_optimize_unifies_equivalent_nodes );
    ( "optimize unifies gradient/shape/clip-path nodes",
      `Quick,
      test_optimize_unifies_gradients_shapes );
    ("optimize folds flex calc", `Quick, test_optimize_folds_flex_calc);
    ( "pp picks shortest spelling of same node",
      `Quick,
      test_pp_picks_shortest_same_node );
    ("pp minify is idempotent", `Quick, test_pp_minify_is_idempotent);
    ("no merge vendor pseudo", `Quick, test_no_merge_vendor_pseudo);
    ("no merge with nested", `Quick, test_no_merge_with_nested);
    ( "spec cascade 3 shorthand resets omitted longhands",
      `Quick,
      c3_shorthand_resets );
    ( "spec cascade 3 stylesheet-scope background synthesis",
      `Quick,
      c3_stylesheet_scope_background_synthesis );
    ( "closed-world groups disjoint selectors",
      `Quick,
      closed_world_groups_disjoint_selectors );
    ( "coalesce adjacent custom-property rules",
      `Quick,
      coalesce_adjacent_custom_property_rules );
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
    ( "vendor alias pins an intervening rule",
      `Quick,
      vendor_alias_pins_intervening_rule );
    ( "unplaceable property name pins an intervening rule",
      `Quick,
      unplaceable_name_pins_intervening_rule );
    ( "group equal rules across higher-specificity intervening rule",
      `Quick,
      c63_group_across_higher_specificity );
    ( "merge same selector across higher-specificity intervening rule",
      `Quick,
      c63_merge_same_selector_across_higher_specificity );
    ( "factor across different-value descendant tie is unsafe",
      `Quick,
      factor_diff_value_descendant_tie_unsafe );
    ( "factor allowed across same-value descendant-combinator tie",
      `Quick,
      factor_same_value_descendant_tie_groups );
    ( "factor prefers size-minimal subset",
      `Quick,
      factor_prefers_size_minimal_subset );
    ( "factor steps over a same-value nesting boundary",
      `Quick,
      factor_steps_over_same_value_nesting_boundary );
    ( "factor does not step over a conflicting nesting boundary",
      `Quick,
      factor_conflicting_nesting_boundary_unsafe );
    ( "factor inlines rather than grouping across a long selector (known gap)",
      `Quick,
      factor_inlines_rather_than_grouping_long_selector );
    ( "factor groups a single property past a differently-shared decl",
      `Quick,
      factor_groups_single_property_past_other_shared_decl );
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
    ( "target evergreen compatibility prefixes",
      `Quick,
      target_evergreen_compatibility_prefixes );
    ( "a nested block keeps its declaration separator",
      `Quick,
      nested_block_separator );
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
    ( "spec cascade 6.1 group across higher-specificity competitor",
      `Quick,
      c61_group_across_higher_specificity_competitor );
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
    ( "calc flatten gated on single-valued @property registration",
      `Quick,
      calc_flatten_registered_single_valued );
    ( "preserve independent custom-property rule position",
      `Quick,
      preserve_independent_custom_prop_position );
    ( "spec cascade 6.1 nesting synthesis preserves source order",
      `Quick,
      c61_nesting_synthesis_source_order );
    ( "spec cascade 6.1 group across higher-specificity pseudo competitor",
      `Quick,
      c61_group_across_pseudo_competitor );
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

(* Property-based fuzz tests for [Css.optimize]'s high-level invariants.
   Namespaced so this stays in test_optimize.ml (mapping to lib/optimize.ml)
   rather than a standalone module with no library counterpart. *)
module Fuzz = struct
  open Cascade

  let error_str e = Error.to_string e

  let parse s =
    match Css.of_string ~strict:false s with
    | Ok p -> p.Css.stylesheet
    | Error e -> Alcotest.failf "parse failed: %s\n  for: %s" (error_str e) s

  let minify_str s = parse s |> Css.to_string ~minify:true
  let optimize_str s = parse s |> Css.optimize |> Css.to_string ~minify:true

  (* --- deterministic generator --- *)

  let selectors =
    [| ".a"; ".b"; ".c"; ".a.b"; ".a.c"; ".b.c"; "#x"; "div"; "p"; "div.a" |]

  (* Only independent property names (no shorthand/longhand aliasing), so an
     element's computed style is a clean per-name map - and a name-level cascade
     change is exactly what a reordering bug would cause. *)
  let decls =
    [|
      "color:red";
      "color:green";
      "color:blue";
      "margin:0";
      "margin:1px";
      "display:block";
      "display:flex";
      "padding:0";
      "padding:2px";
      "top:0";
      "top:1px";
      "z-index:1";
      "z-index:2";
    |]

  let pick rng a = a.(Random.State.int rng (Array.length a))

  let gen_rule rng =
    let sel = pick rng selectors in
    let n = 1 + Random.State.int rng 3 in
    let ds = List.init n (fun _ -> pick rng decls) in
    Fmt.str "%s{%s}" sel (String.concat ";" ds)

  let gen_rules rng =
    List.init (2 + Random.State.int rng 6) (fun _ -> gen_rule rng)

  (* --- elements for the cascade resolver --- *)

  type node = { nname : string; nid : string option; nclasses : string list }

  module Node = struct
    type t = node

    let equal = ( = )
    let name t = Some t.nname
    let id t = t.nid
    let classes t = t.nclasses
    let attribute _ _ : string option = None
    let parent _ : node option = None
    let children _ = []
    let text_children _ = []
  end

  module R = Resolve.Make (Node)

  (* Every element distinguishable by the selector alphabet: tag x optional id x
     class subset. Checking all of them makes the cascade-equivalence and
     permutation-neutrality judgements complete for this alphabet. *)
  let all_elements =
    let class_subsets =
      [
        [];
        [ "a" ];
        [ "b" ];
        [ "c" ];
        [ "a"; "b" ];
        [ "a"; "c" ];
        [ "b"; "c" ];
        [ "a"; "b"; "c" ];
      ]
    in
    List.concat_map
      (fun nname ->
        List.concat_map
          (fun nid ->
            List.map (fun nclasses -> { nname; nid; nclasses }) class_subsets)
          [ None; Some "x" ])
      [ "div"; "p"; "span" ]

  (* Canonical form of one declaration's value, so the comparison is semantic,
     not textual: optimize legitimately rewrites a value (e.g. [blue] -> [#00f],
     [0px] -> [0]) without changing the cascade. Memoized - the same
     declarations recur across thousands of iterations. *)
  let canon_memo : (string, string) Hashtbl.t = Hashtbl.create 256

  let canon_decl d =
    let s = Pp.to_string ~minify:true Declaration.pp d in
    match Hashtbl.find_opt canon_memo s with
    | Some c -> c
    | None ->
        let c =
          match Css.of_string ~strict:false ("x{" ^ s ^ "}") with
          | Ok p ->
              let out =
                Css.to_string ~minify:true (Css.optimize p.Css.stylesheet)
              in
              if String.length out >= 3 then
                String.sub out 2 (String.length out - 3)
              else out
          | Error _ -> s
        in
        Hashtbl.replace canon_memo s c;
        c

  let computed sheet elt =
    R.resolve sheet elt |> List.map canon_decl |> List.sort String.compare

  let describe e =
    Fmt.str "%s%s%s" e.nname
      (match e.nid with Some i -> "#" ^ i | None -> "")
      (match e.nclasses with [] -> "" | cs -> "." ^ String.concat "." cs)

  let iters = 1000

  (* Invariant 1 - idempotence: reparsing and re-optimizing the output changes
     nothing. Guards against any pass that only converges on a second run. *)
  let fuzz_idempotent () =
    let rng = Random.State.make [| 0x1d3a |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_rules rng) in
      let once = optimize_str css in
      let twice = optimize_str once in
      if once <> twice then
        Alcotest.failf
          "not a fixed point:\n  in:    %s\n  once:  %s\n  twice: %s" css once
          twice
    done

  (* Invariant 2 - validity and size: the output reparses and is never larger
     than plain (non-optimizing) minification of the same input. *)
  let fuzz_valid_and_no_larger () =
    let rng = Random.State.make [| 0x5a1d |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_rules rng) in
      let opt = optimize_str css in
      (match Css.of_string ~strict:false opt with
      | Ok _ -> ()
      | Error e ->
          Alcotest.failf
            "optimize produced invalid CSS: %s\n  in:  %s\n  out: %s"
            (error_str e) css opt);
      if String.length opt > String.length (minify_str css) then
        Alcotest.failf
          "optimize larger than minify:\n  in:  %s\n  min: %s\n  opt: %s" css
          (minify_str css) opt
    done

  (* Invariant 3 - cascade equivalence (soundness): the computed style of every
     element is identical before and after optimization. *)
  let fuzz_cascade_equivalent () =
    let rng = Random.State.make [| 0xca5c |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_rules rng) in
      let before = parse css and after = parse (optimize_str css) in
      List.iter
        (fun e ->
          let b = computed before e and a = computed after e in
          if b <> a then
            Alcotest.failf
              "optimize changed the cascade for %s:\n\
              \  css:    %s\n\
              \  before: %s\n\
              \  after:  %s"
              (describe e) css (String.concat " " b) (String.concat " " a))
        all_elements
    done

  (* Invariant 4 - neutral-permutation soundness: a permutation that preserves
     every element's computed style (a cascade-neutral reorder) must still
     compute the same styles after optimization. The outputs need not be
     byte-identical: source order is part of the generator-facing textual
     contract. Neutrality is judged over the complete element set, so it is
     never falsely claimed. *)
  let shuffle rng lst =
    let a = Array.of_list lst in
    for i = Array.length a - 1 downto 1 do
      let j = Random.State.int rng (i + 1) in
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    done;
    Array.to_list a

  let fuzz_neutral_permutation_soundness () =
    let rng = Random.State.make [| 0xbeef |] in
    let checked = ref 0 in
    for _ = 1 to iters do
      let rules = gen_rules rng in
      let x = String.concat "" rules
      and x' = String.concat "" (shuffle rng rules) in
      let bx = parse x and bx' = parse x' in
      let neutral =
        List.for_all (fun e -> computed bx e = computed bx' e) all_elements
      in
      if neutral then begin
        incr checked;
        let ax = parse (optimize_str x) and ax' = parse (optimize_str x') in
        List.iter
          (fun e ->
            let sx = computed ax e and sx' = computed ax' e in
            if sx <> sx' then
              Alcotest.failf
                "cascade-neutral permutation diverged after optimization for %s:\n\
                \  x:   %s\n\
                \  x':  %s\n\
                \  x:   %s\n\
                \  x':  %s"
                (describe e) x x' (String.concat " " sx) (String.concat " " sx'))
          all_elements
      end
    done;
    (* a permutation is neutral often enough that this should fire many times;
       guard against the test silently checking nothing. *)
    Alcotest.(check bool)
      (Fmt.str "exercised %d neutral permutations" !checked)
      true (!checked > 0)

  (* --- shorthand cascade-order soundness ---

     The per-name [computed] above is blind to shorthand/longhand overlap: it
     treats [border-top] and [border-color] as different properties, missing
     that both write [border-top-color]. This oracle compares the computed style
     at the *longhand* level, expanding each declaration through a hand-written
     footprint table independent of cascade's own shorthand code. Values are
     chosen not to fold under minify (5px/7px/red/green/solid all stay), so an
     optimised declaration minifies back to its alphabet key. *)
  let sh_alphabet =
    [
      ( "margin:5px",
        [
          ("margin-top", "5px");
          ("margin-right", "5px");
          ("margin-bottom", "5px");
          ("margin-left", "5px");
        ] );
      ( "margin:9px",
        [
          ("margin-top", "9px");
          ("margin-right", "9px");
          ("margin-bottom", "9px");
          ("margin-left", "9px");
        ] );
      ("margin-top:7px", [ ("margin-top", "7px") ]);
      ( "padding:5px",
        [
          ("padding-top", "5px");
          ("padding-right", "5px");
          ("padding-bottom", "5px");
          ("padding-left", "5px");
        ] );
      ( "border-top:3px solid red",
        [
          ("border-top-width", "3px");
          ("border-top-style", "solid");
          ("border-top-color", "red");
        ] );
      ( "border-color:green",
        [
          ("border-top-color", "green");
          ("border-right-color", "green");
          ("border-bottom-color", "green");
          ("border-left-color", "green");
        ] );
      ("border-top-color:red", [ ("border-top-color", "red") ]);
      ("color:red", [ ("color", "red") ]);
    ]

  let sh_decls = Array.of_list (List.map fst sh_alphabet)
  let sh_decl_string d = Pp.to_string ~minify:true Declaration.pp d

  (* Four-side box shorthands: physical longhand names, in top/right/bottom/left
     order. optimize composes longhand runs back into these, so the oracle must
     expand any box shorthand, not just the uniform alphabet forms. *)
  let box_sides =
    [
      ( "margin",
        [ "margin-top"; "margin-right"; "margin-bottom"; "margin-left" ] );
      ( "padding",
        [ "padding-top"; "padding-right"; "padding-bottom"; "padding-left" ] );
      ("inset", [ "top"; "right"; "bottom"; "left" ]);
      ( "border-color",
        [
          "border-top-color";
          "border-right-color";
          "border-bottom-color";
          "border-left-color";
        ] );
      ( "border-width",
        [
          "border-top-width";
          "border-right-width";
          "border-bottom-width";
          "border-left-width";
        ] );
      ( "border-style",
        [
          "border-top-style";
          "border-right-style";
          "border-bottom-style";
          "border-left-style";
        ] );
    ]

  (* Distribute 1..4 box values over top/right/bottom/left per the CSS box
     rule. *)
  let box_values value =
    match String.split_on_char ' ' value |> List.filter (fun s -> s <> "") with
    | [ a ] -> [ a; a; a; a ]
    | [ a; b ] -> [ a; b; a; b ]
    | [ a; b; c ] -> [ a; b; c; b ]
    | a :: b :: c :: d :: _ -> [ a; b; c; d ]
    | [] -> [ value; value; value; value ]

  (* Expand a declaration to its (longhand, value) footprint, independent of
     cascade's own shorthand code. [box_sides] handles the four-side families
     (including composed forms like [margin:7px 9px 9px]); [sh_alphabet] handles
     the non-box shorthands ([border-top]); anything else is its own
     longhand. *)
  let longhands_of s =
    match String.index_opt s ':' with
    | None -> [ (s, "") ]
    | Some i -> (
        let prop = String.sub s 0 i in
        let value = String.sub s (i + 1) (String.length s - i - 1) in
        match List.assoc_opt prop box_sides with
        | Some sides -> List.combine sides (box_values value)
        | None -> (
            match List.assoc_opt s sh_alphabet with
            | Some lhs -> lhs
            | None -> [ (prop, value) ]))

  (* Class-only branches: nested [&.c] output can compose into nested [Compound]
     nodes, so collect class parts recursively inside a compound. Selectors with
     combinators or non-class parts still contribute no branch; the shorthand
     alphabet is class-only. *)
  let rec class_only_compound : Selector.t -> string list option = function
    | Selector.Class c -> Some [ c ]
    | Selector.Compound ps ->
        List.fold_left
          (fun acc part ->
            match (acc, class_only_compound part) with
            | Some acc, Some classes -> Some (acc @ classes)
            | _ -> None)
          (Some []) ps
    | _ -> None

  let rec branch_class_sets (sel : Selector.t) =
    match sel with
    | Selector.Class c -> [ [ c ] ]
    | Selector.Compound _ -> (
        match class_only_compound sel with
        | Some classes -> [ classes ]
        | None -> [])
    | Selector.List ss -> List.concat_map branch_class_sets ss
    | _ -> []

  let class_subset small big = List.for_all (fun x -> List.mem x big) small

  (* applicable specificity = max class count over matching branches, or None *)
  let sh_match sel classes : int option =
    match
      branch_class_sets sel |> List.filter (fun cs -> class_subset cs classes)
    with
    | [] -> None
    | bs -> Some (List.fold_left (fun m cs -> max m (List.length cs)) 0 bs)

  let sh_rules sheet =
    let rec rule ?parent (rule : Stylesheet.rule) =
      let selector =
        match parent with
        | None -> Stylesheet.selector rule
        | Some parent -> Nest.combine parent (Stylesheet.selector rule)
      in
      let direct =
        if Stylesheet.declarations rule = [] then []
        else [ { rule with selector; nested = [] } ]
      in
      direct @ statements ~parent:selector (Stylesheet.nested rule)
    and statements ?parent stmts =
      List.concat_map
        (function Stylesheet.Rule r -> rule ?parent r | _ -> [])
        stmts
    in
    statements (Css.statements sheet)

  (* computed longhand -> value map for one element, in cascade order *)
  let sh_computed sheet classes =
    let matched =
      sh_rules sheet
      |> List.mapi (fun i r -> (i, r))
      |> List.filter_map (fun (i, r) ->
          match sh_match (Stylesheet.selector r) classes with
          | Some spec -> Some (spec, i, Stylesheet.declarations r)
          | None -> None)
      |> List.sort (fun (s1, i1, _) (s2, i2, _) ->
          match compare s1 s2 with 0 -> compare i1 i2 | c -> c)
    in
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (_, _, decls) ->
        List.iter
          (fun d ->
            List.iter
              (fun (lh, v) -> Hashtbl.replace tbl lh v)
              (longhands_of (sh_decl_string d)))
          decls)
      matched;
    Hashtbl.fold (fun k v acc -> (k ^ ":" ^ v) :: acc) tbl []
    |> List.sort String.compare

  let sh_class_subsets =
    [
      [];
      [ "a" ];
      [ "b" ];
      [ "c" ];
      [ "a"; "b" ];
      [ "a"; "c" ];
      [ "b"; "c" ];
      [ "a"; "b"; "c" ];
    ]

  let sh_selectors = [| ".a"; ".b"; ".c"; ".a.b"; ".a.c"; ".b.c"; ".a.b.c" |]

  let gen_sh_rule rng =
    let sel = pick rng sh_selectors in
    let n = 1 + Random.State.int rng 2 in
    let ds = List.init n (fun _ -> pick rng sh_decls) in
    Fmt.str "%s{%s}" sel (String.concat ";" ds)

  let gen_sh_rules rng =
    List.init (2 + Random.State.int rng 6) (fun _ -> gen_sh_rule rng)

  (* The DAG must keep two rules ordered (or refuse to merge) whenever their
     declarations share a longhand, even through different shorthands
     ([border-top] vs [border-color] over [border-top-color]). A false-disjoint
     reorders them and flips a longhand winner; this catches it. *)
  let fuzz_shorthand_cascade_equivalent () =
    let rng = Random.State.make [| 0x5304 |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_sh_rules rng) in
      let before = parse css and after = parse (optimize_str css) in
      List.iter
        (fun classes ->
          let b = sh_computed before classes
          and a = sh_computed after classes in
          if b <> a then
            Alcotest.failf
              "optimize changed the longhand cascade for .%s:\n\
              \  css:    %s\n\
              \  before: %s\n\
              \  after:  %s"
              (String.concat "." classes)
              css (String.concat " " b) (String.concat " " a))
        sh_class_subsets
    done

  (* Two source orders that agree on every element's longhand cascade must
     optimise to cascade-equivalent output: the DAG result cannot depend on a
     shorthand-overlap order it is free to reorder. *)
  let fuzz_shorthand_permutation_sound () =
    let rng = Random.State.make [| 0x5305 |] in
    let checked = ref 0 in
    for _ = 1 to iters do
      let rules = gen_sh_rules rng in
      let x = String.concat "" rules
      and x' = String.concat "" (shuffle rng rules) in
      let bx = parse x and bx' = parse x' in
      let neutral =
        List.for_all
          (fun e -> sh_computed bx e = sh_computed bx' e)
          sh_class_subsets
      in
      if neutral then begin
        incr checked;
        let ax = parse (optimize_str x) and ax' = parse (optimize_str x') in
        List.iter
          (fun e ->
            if sh_computed ax e <> sh_computed ax' e then
              Alcotest.failf
                "shorthand-neutral permutation diverged for .%s:\n\
                \  x:  %s\n\
                \  x': %s"
                (String.concat "." e) x x')
          sh_class_subsets
      end
    done;
    Alcotest.(check bool)
      (Fmt.str "exercised %d shorthand-neutral permutations" !checked)
      true (!checked > 0)

  let cases =
    [
      Alcotest.test_case "optimize is idempotent" `Slow fuzz_idempotent;
      Alcotest.test_case "optimize output is valid and no larger" `Slow
        fuzz_valid_and_no_larger;
      Alcotest.test_case "optimize preserves the cascade" `Slow
        fuzz_cascade_equivalent;
      Alcotest.test_case "same-selector merge past nested children" `Quick
        same_selector_merge_past_nested;
      Alcotest.test_case "nested merge reads the slot from either side" `Quick
        nested_merge_reads_the_slot_from_either_side;
      Alcotest.test_case "a nested declarations run is deduplicated" `Quick
        nested_declaration_run_dedupes;
      Alcotest.test_case "vendor pseudo-element list is split" `Quick
        vendor_pseudo_list_is_split;
      Alcotest.test_case "cascade-neutral permutations stay equivalent" `Slow
        fuzz_neutral_permutation_soundness;
      Alcotest.test_case "shorthand longhand cascade preserved" `Slow
        fuzz_shorthand_cascade_equivalent;
      Alcotest.test_case "shorthand-neutral permutations stay equivalent" `Slow
        fuzz_shorthand_permutation_sound;
    ]
end

let suite = ("optimize", optimize_tests @ selector_merging_tests @ Fuzz.cases)
