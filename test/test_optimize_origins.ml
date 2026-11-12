(** Optimize tests: origins, importance, layers and revert. *)

open Cascade
open Css.Values
open Css.Properties
open Optimize_helpers

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

let suite =
  ( "optimize_origins",
    [
      ("property source order", `Quick, test_property_source_order);
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
    ] )
