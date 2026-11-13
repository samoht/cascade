(** Optimize tests: the at-rule and cascade boundaries a merge must not cross.
*)

open Cascade
open Css.Values
open Css.Properties
open Optimize_helpers

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

let cascade2_import_substitution_point () =
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

let cascade2_import_after_bang_comment () =
  (* CSS Cascade 5 sec. 2 makes an @import invalid only after a valid at-rule or
     style rule, and a comment is neither (CSS Syntax 3 sec. 4.3.2 consumes it
     without producing a token). Chrome keeps the CSSImportRule after a
     preserved license comment (curated case [comment_before_import] of
     test/render/parse_recovery.ml), so the optimizer keeps the import. *)
  let import =
    Css.Stylesheet.Import
      { url = "url(a.css)"; layer = None; supports = None; media = None }
  in
  let rule =
    Css.rule ~selector:(Css.Selector.class_ "a")
      [ Css.Declaration.color (hex_color "ff0000") ]
  in
  let output =
    Css.Optimize.stylesheet
      [ Css.Stylesheet.Bang_comment "! license "; import; rule ]
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "import after a bang comment is kept"
    "/*! license */@import\"a.css\";.a{color:red}" output

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
     --gap{syntax:\"<length>\";inherits:false;initial-value:1px}.theme{display:flex}"
    ".theme{color:red}@property \
     --gap{syntax:\"<length>\";inherits:false;initial-value:1px}.theme{display:flex}";
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
     --x{syntax:\"<length>\";inherits:false;initial-value:0}.a{width:calc(var(--x)*2)}";
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

let suite =
  ( "optimize_boundaries",
    [
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
      ( "spec cascade 2 import keeps substitution point",
        `Quick,
        cascade2_import_substitution_point );
      ( "spec cascade 2 import after bang comment is kept",
        `Quick,
        cascade2_import_after_bang_comment );
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
    ] )
