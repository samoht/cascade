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
        ( Some (Css.Selector.v ".component"),
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
        (Some (Css.Selector.v ".outer"), None, [ scoped_rule ]);
      Css.Stylesheet.Scope
        (Some (Css.Selector.v ".inner"), None, [ scoped_rule ]);
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
        ( Some (Css.Selector.v ".card"),
          Some (Css.Selector.v ".footer"),
          [ scoped_rule ] );
      Css.Stylesheet.Scope
        ( Some (Css.Selector.v ".card"),
          Some (Css.Selector.v ".aside"),
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
      { url = "a.css"; layer = None; supports = None; media = None }
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
  (* A registration types its property only under [--scope=stylesheet], where no
     script or later sheet registers the name again (CSS Properties and Values
     API 1 sec. 2.1). *)
  let check label css expected =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized = Css.Optimize.stylesheet ~scope:`Stylesheet input in
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

(* CSS Values 4 sec. 10.10.1 folds [x * 0] to a zero only over an operand of
   known type. An unregistered [var()] has none: it substitutes tokens, so
   [calc(0 * var(--x))] under [--x:1px + 2px] is [0 * 1px + 2px], and an
   undefined one makes the declaration invalid at computed-value time. A
   [<number>] registration substitutes one number, always valid (CSS Properties
   and Values API 1 sec. 2.4), so the product is a zero of the other operand's
   type. A [<length>] registration is one value too, but of a type the zero has
   to match. *)
let calc_zero_product_registered_number () =
  (* A registration types its property only under [--scope=stylesheet], where no
     script or later sheet registers the name again (CSS Properties and Values
     API 1 sec. 2.1). *)
  let check label css expected =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized = Css.Optimize.stylesheet ~scope:`Stylesheet input in
    Alcotest.(check string)
      label expected
      (Css.Stylesheet.to_string ~minify:true optimized |> String.trim)
  in
  check "a <number> registration folds the zero product"
    "@property \
     --n{syntax:\"<number>\";inherits:false;initial-value:1}.a{width:calc(var(--n) \
     * 0px)}"
    "@property \
     --n{syntax:\"<number>\";inherits:false;initial-value:1}.a{width:0}";
  check "and keeps a number zero a number"
    "@property \
     --n{syntax:\"<number>\";inherits:false;initial-value:1}.a{width:calc(10px \
     * (var(--n) * 0 + 1))}"
    "@property \
     --n{syntax:\"<number>\";inherits:false;initial-value:1}.a{width:10px}";
  check "an unregistered var keeps it" ".a{width:calc(var(--n) * 0px)}"
    ".a{width:calc(var(--n)*0px)}";
  check "a universal registration keeps it"
    "@property --n{syntax:\"*\";inherits:false}.a{width:calc(var(--n) * 0px)}"
    "@property --n{syntax:\"*\";inherits:false}.a{width:calc(var(--n)*0px)}";
  check "a <length> registration keeps it"
    "@property \
     --n{syntax:\"<length>\";inherits:false;initial-value:0px}.a{rotate:calc(var(--n) \
     * 0)}"
    "@property \
     --n{syntax:\"<length>\";inherits:false;initial-value:0}.a{rotate:calc(var(--n)*0)}"

(* A [<length>] registration substitutes one finite length (CSS Properties and
   Values API 1 sec. 2.4; CSS Values 4 sec. 10.12 censors an infinite one), so
   its product with the number [0] is a zero length wherever the calculation
   needs a length. Elsewhere the product is a type error the browser finds only
   at computed-value time, and the zero would be one it finds at parse time:
   Chromium 157 computes [width:calc(10px * (var(--l) * 0 + 1))] to [auto], not
   to the [50px] of an earlier declaration. *)
let calc_zero_product_registered_length () =
  let length =
    "@property --l{syntax:\"<length>\";inherits:false;initial-value:0px}"
  in
  let length' =
    "@property --l{syntax:\"<length>\";inherits:false;initial-value:0}"
  in
  let check label body expected =
    let input =
      Css.Stylesheet.read (Cursor.of_string (String.concat "" [ length; body ]))
    in
    let optimized = Css.Optimize.stylesheet ~scope:`Stylesheet input in
    Alcotest.(check string)
      label
      (String.concat "" [ length'; expected ])
      (Css.Stylesheet.to_string ~minify:true optimized |> String.trim)
  in
  check "a <length> registration folds at a length slot"
    ".a{width:calc(var(--l) * 0)}" ".a{width:0}";
  check "and in a sum of lengths" ".a{width:calc(0 * var(--l) + 10px)}"
    ".a{width:10px}";
  check "and beside a number factor" ".a{width:calc(2 * (var(--l) * 0))}"
    ".a{width:0}";
  check "a number slot keeps it" ".a{width:calc(10px * (var(--l) * 0 + 1))}"
    ".a{width:calc(10px*(var(--l)*0 + 1))}";
  check "a length factor keeps it" ".a{width:calc(var(--l) * 0px)}"
    ".a{width:calc(var(--l)*0px)}"

(* A registration types a custom property only where it is the one every browser
   applies. CSS Properties and Values API 1 sec. 2.1 takes a
   [CSS.registerProperty()] record over any [@property], and the last
   [@property] in document order over the others, so in a fragment a script or a
   later sheet can register the name again: with [@property
   --n{syntax:"*";inherits:false}] and [--n:1px + 2] after it, Chromium 153
   computes the source's [calc(var(--n) * 0px)] to 1px and the folded [0] to
   0px. A registration inside a conditional group rule applies only where the
   condition holds (Chromium 153 and WebKit 26.5 register an [@media print]
   [@property] on paper only). Under [--enforce-spec] a later [@property]
   without [inherits] is a registration too, since the current sec. 3 makes
   every descriptor optional, while Chromium 153 and WebKit 26.5 drop it. *)
let registration_known_only_in_closed_scope () =
  let opt ?scope ?(enforce_spec = false) css =
    match Css.of_string ~strict:false ~enforce_spec css with
    | Ok p ->
        Css.Optimize.stylesheet ?scope ~enforce_spec p.stylesheet
        |> Css.Stylesheet.to_string ~minify:true
        |> String.trim
    | Error _ -> Alcotest.failf "parse failed: %s" css
  in
  let number =
    "@property --n{syntax:\"<number>\";inherits:false;initial-value:1}"
  in
  let length =
    "@property --l{syntax:\"<length>\";inherits:false;initial-value:0px}"
  in
  let length' =
    "@property --l{syntax:\"<length>\";inherits:false;initial-value:0}"
  in
  let cat = String.concat "" in
  let check ?scope ?enforce_spec label css expected =
    Alcotest.(check string) label expected (opt ?scope ?enforce_spec css)
  in
  (* A fragment: nothing registered is known. *)
  check "fragment keeps the zero product"
    (cat [ number; ".a{width:calc(var(--n) * 0px)}" ])
    (cat [ number; ".a{width:calc(var(--n)*0px)}" ]);
  check "fragment keeps a registered value's tokens"
    (cat [ length; ".a{--l:0px}" ])
    (cat [ length'; ".a{--l:0px}" ]);
  check "fragment keeps the nested calc()"
    (cat [ length; ".a{width:calc(calc(var(--l)) * 2)}" ])
    (cat [ length'; ".a{width:calc(calc(var(--l))*2)}" ]);
  check "fragment keeps a var() out of a shorthand"
    (cat
       [
         length;
         ".a{margin-top:var(--l);margin-right:1px;margin-bottom:1px;margin-left:1px}";
       ])
    (cat
       [
         length';
         ".a{margin-top:var(--l);margin-right:1px;margin-bottom:1px;margin-left:1px}";
       ]);
  (* The whole stylesheet: the last registration is the one. *)
  let scope = `Stylesheet in
  check ~scope "stylesheet scope folds the zero product"
    (cat [ number; ".a{width:calc(var(--n) * 0px)}" ])
    (cat [ number; ".a{width:0}" ]);
  check ~scope "stylesheet scope types a registered value"
    (cat [ length; ".a{--l:0px}" ])
    (cat [ length'; ".a{--l:0}" ]);
  check ~scope "stylesheet scope drops the nested calc()"
    (cat [ length; ".a{width:calc(calc(var(--l)) * 2)}" ])
    (cat [ length'; ".a{width:calc(var(--l)*2)}" ]);
  check ~scope "a later registration is the one"
    (cat
       [
         number;
         "@property --n{syntax:\"*\";inherits:false}.a{width:calc(var(--n) * \
          0px)}";
       ])
    (cat
       [
         number;
         "@property \
          --n{syntax:\"*\";inherits:false}.a{width:calc(var(--n)*0px)}";
       ]);
  check ~scope "a conditional registration is not known"
    "@media print{@property \
     --n{syntax:\"<number>\";inherits:false;initial-value:1}}.a{width:calc(var(--n) \
     * 0px)}"
    "@media print{@property \
     --n{syntax:\"<number>\";inherits:false;initial-value:1}}.a{width:calc(var(--n)*0px)}";
  check ~scope ~enforce_spec:true
    "under --enforce-spec a later partial registration stays and counts"
    (cat
       [
         number;
         "@property --n{syntax:\"<color>\"}.a{width:calc(var(--n) * 0px)}";
       ])
    (cat
       [
         number; "@property --n{syntax:\"<color>\"}.a{width:calc(var(--n)*0px)}";
       ])

(* [--scope=stylesheet] rules out author CSS the optimizer does not see, not the
   user-agent sheet nor the sheet's own other rules, and author beats UA (CSS
   Cascade 5 sec. 6.2). A shorthand built from some of its longhands sets the
   others to their initial values, so it is built only where no rule of the
   sheet and no rule or presentational hint of the HTML rendering section (sec.
   15) holds one of them. Chromium 153: [h1{font-size:20px; font-family:serif}]
   as [h1{font:20px serif}] takes an [h1] from weight 700 to 400 and an [em]
   from italic to normal; beside [.y{font-weight:bold}] an element of both
   classes goes from 700 to 400; beside [.y{background-repeat:no-repeat}]
   [background:red] repeats; and a [transition-delay:1s] inside [@media screen]
   is lost to [transition:opacity 1s]. *)
let stylesheet_scope_reset_held () =
  let opt css =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    Css.Optimize.stylesheet ~scope:`Stylesheet input
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  let same label css = Alcotest.(check string) label css (opt css) in
  same "the UA sheet holds an h1's weight"
    "h1{font-size:20px;font-family:serif}";
  same "the UA sheet holds an em's style" "em{font-size:20px;font-family:serif}";
  same "another rule holds the weight"
    ".y{font-weight:700}.x{font-size:20px;font-family:serif}";
  same "another rule holds the repeat"
    ".y{background-repeat:no-repeat}.x{background-color:red;background-image:none}";
  same "a rule in another block holds the delay"
    "@media \
     screen{.y{transition-delay:1s}}.x{transition-property:opacity;transition-duration:1s}";
  (* Two rules of one selector merge into one body. A shorthand built from the
     second rule alone resets the image the first set; Chromium 153 then paints
     none. 1.2.1 printed [.x{background:no-repeat fixed}] and
     [.x{border-image-source:url(b.png);border-image:30 round}]. *)
  Alcotest.(check string)
    "the image the first rule sets survives the second's background"
    ".x{background-image:url(b.png);background-position:0 \
     0;background-size:auto;background-repeat:no-repeat;background-attachment:fixed;background-origin:padding-box;background-clip:border-box}"
    (opt
       ".x{background-image:url(b.png)}.x{background-position:0 \
        0;background-size:auto;background-repeat:no-repeat;background-attachment:fixed;background-origin:padding-box;background-clip:border-box}");
  Alcotest.(check string)
    "the source the first rule sets survives the second's border-image"
    ".x{border-image:url(b.png)30 round}"
    (opt
       ".x{border-image-source:url(b.png)}.x{border-image-slice:30;border-image-repeat:round}");
  (* Where nothing holds the others, the shorthand is still built. *)
  Alcotest.(check string)
    "nothing holds the other background longhands" ".x{background:red}"
    (opt ".x{background-color:red;background-image:none}");
  Alcotest.(check string)
    "nothing holds the other transition longhands" ".x{transition:opacity 1s}"
    (opt ".x{transition-property:opacity;transition-duration:1s}")

let c61_no_nested_boundary_merge () =
  (* Scope proximity and page context are cascade-visible boundaries. The same
     is true when @scope appears as a nested group rule inside a style rule. *)
  let check_case label css expected =
    let input = Css.Stylesheet.read (Cursor.of_string css) in
    let optimized =
      Css.Optimize.stylesheet ~targets:Optimize_helpers.nesting_targets input
    in
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
    let optimized =
      Css.Optimize.stylesheet ~targets:Optimize_helpers.nesting_targets input
    in
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

(* CSS Cascade 5 sec. 7.3.6 (Editor's Draft): [revert-rule] rolls the cascaded
   value back as if the style rule holding it were absent, and every property
   takes it (sec. 7.3). Chrome 153 reads [.a{color:red}.a{color:revert-rule}] as
   red and the same two declarations in one rule as the inherited colour, so the
   rule a declaration sits in is part of its meaning: no rewrite merges two such
   rules, hoists the declaration into its parent, or lets it shadow a write of
   another rule. A custom property rolls back the same way, and a [var()]
   fallback reaches it too. Chrome 145 drops the declaration, so the one before
   it in its rule stays. *)
let c736_revert_rule_keeps_its_rule () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string) input expected (optimized_string input))
    [
      (".a{width:revert-rule}", ".a{width:revert-rule}");
      (".a{animation-name:revert-rule}", ".a{animation-name:revert-rule}");
      (".a{all:revert-rule}", ".a{all:revert-rule}");
      ( ".a{color:red}.a{color:revert-rule}",
        ".a{color:red}.a{color:revert-rule}" );
      (".a{color:red;color:revert-rule}", ".a{color:red;color:revert-rule}");
      ( ".a{color:red;margin:0}.a{color:revert-rule}",
        ".a{color:red;margin:0}.a{color:revert-rule}" );
      ( ".a{color:red}.b{color:blue}.a{color:revert-rule}",
        ".a{color:red}.b{color:#00f}.a{color:revert-rule}" );
      (".a{--x:1}.a{--x:revert-rule}", ".a{--x:1}.a{--x:revert-rule}");
      ( ".a{color:red}.a{color:var(--n,revert-rule)}",
        ".a{color:red}.a{color:var(--n,revert-rule)}" );
      ( ".a{color:red}.a{&{color:revert-rule}}",
        ".a{color:red}.a{color:revert-rule}" );
      ( "@supports (color:revert-rule){.a{color:blue}}",
        "@supports(color:revert-rule){.a{color:#00f}}" );
    ];
  (* A declaration after a nested rule sits in a nested declarations rule of its
     own (CSS Nesting 1 sec. 3.4), so it may be written nested or unnested after
     its parent, but never in the parent's own run. *)
  let output =
    optimized_string ".n{color:red;& .k{margin:0}color:revert-rule}"
  in
  Alcotest.(check bool)
    (Fmt.str "revert-rule stays out of its parent's run (got %s)" output)
    false
    (Astring.String.is_infix ~affix:"color:red;color:revert-rule" output);
  Alcotest.(check bool)
    "revert-rule is kept" true
    (Astring.String.is_infix ~affix:"color:revert-rule" output)

(* CSS Values 4 (ED) sec. 10.2, "Comparison Functions": either bound of
   [clamp()] can be [none], for no clamp from that side. Chromium 151 and WebKit
   read it and Chromium 145 drops it, so the call never folds into a value that
   browser reads, the declaration before it stays, and a guard over it is
   undecided. Chromium 153 computes [clamp(none,90px,80px)] as 80px. *)
let v4102_clamp_none_bound () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string) input expected (optimized_string input))
    [
      (".a{width:clamp(none,50px,80px)}", ".a{width:clamp(none,50px,80px)}");
      (".a{width:clamp(10px,50px,none)}", ".a{width:clamp(10px,50px,none)}");
      (".a{width:clamp(none,50px,none)}", ".a{width:clamp(none,50px,none)}");
      (".a{width:clamp(none,90px,80px)}", ".a{width:clamp(none,90px,80px)}");
      ( ".a{width:100px;width:clamp(none,50px,80px)}",
        ".a{width:100px;width:clamp(none,50px,80px)}" );
      ( ".a{width:calc(clamp(none,1px,2px) + 1px)}",
        ".a{width:calc(clamp(none,1px,2px) + 1px)}" );
      (".a{rotate:clamp(none,5deg,10deg)}", ".a{rotate:clamp(none,5deg,10deg)}");
      ( ".a{animation-range:clamp(1px, 5px, none)}",
        ".a{animation-range:clamp(1px,5px,none)}" );
      ( ".a{animation-range-start:clamp(1px, 5px, none)}",
        ".a{animation-range-start:clamp(1px,5px,none)}" );
      (".a{columns:clamp(1px, 5px, none)}", ".a{columns:clamp(1px,5px,none)}");
      ( "@supports (width:clamp(none,1px,2px)){.a{color:blue}}",
        "@supports(width:clamp(none,1px,2px)){.a{color:#00f}}" );
    ];
  (* Each keeps its call; the shortest spelling is the bare call. *)
  List.iter
    (fun (input, call) ->
      let output = optimized_string input in
      Alcotest.(check bool)
        (Fmt.str "%s keeps %s (got %s)" input call output)
        true
        (Astring.String.is_infix ~affix:call output))
    [
      (".a{border-width:clamp(none,1px,2px)}", "clamp(none,1px,2px)");
      (".a{border:clamp(1px,5px,none) solid}", "clamp(1px,5px,none)");
      (".a{line-height:clamp(none,1.5,2)}", "clamp(none,1.5,2)");
      (".a{line-height:clamp(1px,5px,none)}", "clamp(1px,5px,none)");
      (".a{transition-duration:clamp(none,1s,2s)}", "clamp(none,1s,2s)");
      (".a{z-index:clamp(none,5,10)}", "clamp(none,5,10)");
    ];
  Alcotest.(check bool)
    "a none bound is no fold" false
    (Cascade_diff.Css_compare.equal ~mode:`Canonical
       ".a{width:clamp(none,50px,80px)}" ".a{width:50px}")

(* CSS Values 4 (ED) sec. 10.9, "Type Checking": a math function "can be used
   anywhere a value of that type is allowed", so every numeric math function
   stands in a [<number>] or [<integer>] slot, and sec. 10.3 lets [round()] omit
   its step when the value is a [<number>], the step then being 1. Chromium 153
   computes each source below as its output: [z-index:2], [columns] and
   [column-count] 2, [aspect-ratio:.5], [tab-size:.841471]. Sec. 10.4 has
   [asin()] and its siblings answer an [<angle>], and sec. 10.3 makes the step
   of a [<length>] required, so those stay refused. *)
let v4109_math_in_number_slots () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string) input expected (optimized_string input))
    [
      (".a{z-index:round(1.5)}", ".a{z-index:2}");
      (".a{columns:round(1.5)}", ".a{columns:2}");
      (".a{column-count:round(1.5)}", ".a{column-count:2}");
      (".a{opacity:round(.4)}", ".a{opacity:0}");
      (".a{line-height:round(1.5)}", ".a{line-height:2}");
      (".a{font-weight:round(down,450,100)}", ".a{font-weight:400}");
      (".a{aspect-ratio:cos(60deg)}", ".a{aspect-ratio:.5}");
      (".a{aspect-ratio:exp(0) / 2}", ".a{aspect-ratio:1/2}");
      (".a{tab-size:sin(1)}", ".a{tab-size:.84147098}");
      ( ".a{animation-iteration-count:log(100,10)}",
        ".a{animation-iteration-count:2}" );
      (".a{hyphenate-limit-chars:cos(0)}", ".a{hyphenate-limit-chars:1}");
      (".a{stroke-dashoffset:exp(0)}", ".a{stroke-dashoffset:1}");
      (".a{counter-increment:a sin(1)}", ".a{counter-increment:a 1}");
    ];
  (* Kept, though not folded: the value type keeps the call. *)
  Alcotest.(check bool)
    "border-image-outset keeps exp(1)" true
    (Astring.String.is_infix ~affix:"exp(1)"
       (optimized_string ".a{border-image-outset:exp(1)}"));
  List.iter
    (fun input ->
      match Css.of_string ~strict:true input with
      | Ok _ -> Alcotest.failf "%s should be refused" input
      | Error _ -> ())
    [
      ".a{rotate:round(1.5)}";
      ".a{width:round(1.5px)}";
      ".a{tab-size:asin(0)}";
      ".a{counter-increment:a acos(1)}";
    ]

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
      ( "stylesheet scope resets only what nothing holds",
        `Quick,
        stylesheet_scope_reset_held );
      ( "registration known only in closed scope",
        `Quick,
        registration_known_only_in_closed_scope );
      ( "calc zero product over a registered number",
        `Quick,
        calc_zero_product_registered_number );
      ( "calc zero product over a registered length",
        `Quick,
        calc_zero_product_registered_length );
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
      ( "spec cascade 7.3.6 revert-rule keeps its rule",
        `Quick,
        c736_revert_rule_keeps_its_rule );
      ( "spec values 10.2 clamp takes a none bound",
        `Quick,
        v4102_clamp_none_bound );
      ( "spec values 10.9 math functions in number slots",
        `Quick,
        v4109_math_in_number_slots );
    ] )
