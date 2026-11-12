open Cascade
open Context_helpers

let check_eval_value name ~ctx ~expected input =
  let decl = Css_test_helpers.ok (Css.Declaration.of_string input) in
  Alcotest.(check decl_t)
    name
    (declaration_with_value decl expected)
    (Css.eval_declaration ctx decl)

let declaration_shape decl =
  Css.Declaration.property_name decl
  ^ if Css.Declaration.is_important decl then " !important" else ""

let raw_descriptor_shape (descriptor : Css.Stylesheet.page_descriptor) =
  Css.Declaration.property_name descriptor

let page_selectors_string selectors =
  let pseudo = function
    | Css.Stylesheet.First -> ":first"
    | Left -> ":left"
    | Right -> ":right"
    | Blank -> ":blank"
  in
  let one { Css.Stylesheet.name; pseudos } =
    Option.value ~default:"" name ^ String.concat "" (List.map pseudo pseudos)
  in
  String.concat "," (List.map one selectors)

let rec conditional_shape = function
  | Css.Stylesheet.Media_condition condition ->
      "media(" ^ Css.Pp.to_string ~minify:true Css.Media.pp condition ^ ")"
  | Supports_condition_test condition ->
      "supports("
      ^ Css.Pp.to_string ~minify:true Css.Supports.pp condition
      ^ ")"
  | And (left, right) ->
      "(" ^ conditional_shape left ^ " and " ^ conditional_shape right ^ ")"
  | Or (left, right) ->
      "(" ^ conditional_shape left ^ " or " ^ conditional_shape right ^ ")"

let rec statement_shape stmt =
  let sheet_source stmt = Css.Stylesheet.to_string ~minify:true [ stmt ] in
  let prefixed prefix lines = List.map (fun line -> prefix ^ line) lines in
  let declaration_lines declarations =
    List.map (fun decl -> "decl:" ^ declaration_shape decl) declarations
  in
  let block_lines block =
    block |> List.map statement_shape |> List.concat |> prefixed "  "
  in
  match stmt with
  | Css.Stylesheet.Rule rule ->
      ("rule:" ^ Css.Selector.to_string ~minify:true rule.selector)
      :: declaration_lines rule.declarations
      @ block_lines rule.nested
  | Declarations declarations -> "decls" :: declaration_lines declarations
  | Charset _ -> [ "charset:" ^ sheet_source stmt ]
  | Import _ -> [ "import:" ^ sheet_source stmt ]
  | Namespace _ -> [ "namespace:" ^ sheet_source stmt ]
  | Property _ -> [ "property:" ^ sheet_source stmt ]
  | Layer_decl names ->
      [
        "layer-decl:"
        ^ String.concat "." (List.map Css.Stylesheet.string_of_layer_name names);
      ]
  | Layer (name, block) ->
      ("layer:"
      ^ Option.fold ~none:"" ~some:Css.Stylesheet.string_of_layer_name name)
      :: block_lines block
  | Media (condition, block) ->
      ("media:" ^ Css.Pp.to_string ~minify:true Css.Media.pp condition)
      :: block_lines block
  | Container (name, condition, block) ->
      ("container:"
      ^ Option.value ~default:"" name
      ^ ":"
      ^ Option.fold ~none:"" ~some:Css.Container.to_string condition)
      :: block_lines block
  | Supports (condition, block) ->
      ("supports:" ^ Css.Pp.to_string ~minify:true Css.Supports.pp condition)
      :: block_lines block
  | When (condition, block) ->
      ("when:" ^ conditional_shape condition) :: block_lines block
  | Else (condition, block) ->
      ("else:" ^ Option.fold ~none:"" ~some:conditional_shape condition)
      :: block_lines block
  | Supports_condition (name, declarations) ->
      ("supports-condition:" ^ name) :: declaration_lines declarations
  | Starting_style block -> "starting-style" :: block_lines block
  | Origin (origin, block) ->
      ("origin:"
      ^ string_of_int
          (Css.Stylesheet.origin_importance_rank ~important:false origin))
      :: block_lines block
  | Moz_document (_, block) -> "moz-document" :: block_lines block
  | Scope (start, boundary, block) ->
      let bound =
        Option.fold ~none:"" ~some:(Css.Selector.to_string ~minify:true)
      in
      String.concat ":" [ "scope"; bound start; bound boundary ]
      :: block_lines block
  | Keyframes (name, keyframes) ->
      ("keyframes:" ^ name)
      :: (keyframes
         |> List.map (fun (keyframe : Css.Stylesheet.keyframe) ->
             ("  keyframe:" ^ Css.Keyframe.to_string keyframe.selector)
             :: prefixed "    " (declaration_lines keyframe.declarations))
         |> List.concat)
  | Webkit_keyframes (name, keyframes) ->
      ("webkit-keyframes:" ^ name)
      :: (keyframes
         |> List.map (fun (keyframe : Css.Stylesheet.keyframe) ->
             ("  keyframe:" ^ Css.Keyframe.to_string keyframe.selector)
             :: prefixed "    " (declaration_lines keyframe.declarations))
         |> List.concat)
  | Moz_keyframes (name, keyframes) ->
      ("moz-keyframes:" ^ name)
      :: (keyframes
         |> List.map (fun (keyframe : Css.Stylesheet.keyframe) ->
             ("  keyframe:" ^ Css.Keyframe.to_string keyframe.selector)
             :: prefixed "    " (declaration_lines keyframe.declarations))
         |> List.concat)
  | Font_face descriptors ->
      "font-face" :: List.map (fun _ -> "descriptor") descriptors
  | Page (selector, declarations) ->
      ("page:" ^ page_selectors_string selector)
      :: declaration_lines declarations
  | Page_with_margins (selector, descriptors, margins) ->
      String.concat ":"
        [
          "page-margins";
          page_selectors_string selector;
          String.concat "," (List.map raw_descriptor_shape descriptors);
        ]
      :: List.map
           (fun (margin : Css.Stylesheet.page_margin_rule) ->
             "  margin:" ^ margin.name ^ ":"
             ^ String.concat ","
                 (List.map raw_descriptor_shape margin.descriptors))
           margins
  | Font_palette_values (name, descriptors) ->
      [
        "font-palette-values:" ^ name ^ ":"
        ^ String.concat "," (List.map (fun _ -> "descriptor") descriptors);
      ]
  | View_transition descriptors ->
      [
        "view-transition:"
        ^ String.concat "," (List.map (fun _ -> "descriptor") descriptors);
      ]
  | Position_try (name, declarations) ->
      ("position-try:" ^ name) :: declaration_lines declarations
  | Counter_style (name, _) -> [ "counter-style:" ^ name ]
  | Viewport (prefix, descriptors) ->
      let label =
        match prefix with
        | Standard -> "viewport"
        | Ms_prefixed -> "-ms-viewport"
      in
      [
        label ^ ":"
        ^ String.concat ","
            (List.map
               (fun (d : Css.Stylesheet.viewport_descriptor) -> d.name)
               descriptors);
      ]
  | Unknown_at_rule { name; _ } -> [ "unknown-at-rule:" ^ name ]
  | Font_feature_values (families, _) ->
      [ "font-feature-values:" ^ string_of_int (List.length families) ]
  | Bang_comment _ -> [ "bang-comment" ]

let stylesheet_shape sheet = List.concat_map statement_shape sheet

let check_eval_stylesheet_preserves_structure name ~ctx ?layer_order ?layer
    input =
  let sheet = stylesheet_of_string input in
  let actual = Css.eval_stylesheet ctx ?layer_order ?layer sheet in
  Alcotest.(check (list string))
    name (stylesheet_shape sheet) (stylesheet_shape actual)

let test_computed_value_resolution_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--brand: red");
          Css_test_helpers.ok (Css.Declaration.of_string "--gap: 1rem");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: blue");
          Css_test_helpers.ok (Css.Declaration.of_string "font-size: 10px");
          Css_test_helpers.ok (Css.Declaration.of_string "line-height: 1.5");
          Css_test_helpers.ok
            (Css.Declaration.of_string "border-collapse: collapse");
        ]
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "display: inline");
          Css_test_helpers.ok (Css.Declaration.of_string "width: auto");
          Css_test_helpers.ok (Css.Declaration.of_string "line-height: normal");
          Css_test_helpers.ok
            (Css.Declaration.of_string "border-collapse: separate");
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1024.)
      ~viewport_height:(Css.Values.Px 768.)
      ~container_width:(Css.Values.Px 640.)
      ~container_height:(Css.Values.Px 480.) ()
  in
  check_eval_value "initial keyword uses property initial value" ~ctx
    ~expected:"inline" "display: initial";
  check_eval_value "inherit keyword uses inherited value" ~ctx ~expected:"blue"
    "color: inherit";
  check_eval_value "unset on inherited property uses inherited value" ~ctx
    ~expected:"blue" "color: unset";
  check_eval_value "unset on non-inherited property uses initial value" ~ctx
    ~expected:"auto" "width: unset";
  check_eval_value "line-height inherit uses inherited value" ~ctx
    ~expected:"1.5" "line-height: inherit";
  check_eval_value "line-height unset uses inherited value" ~ctx ~expected:"1.5"
    "line-height: unset";
  check_eval_value "line-height initial uses initial value" ~ctx
    ~expected:"normal" "line-height: initial";
  check_eval_value "border-collapse unset uses inherited value" ~ctx
    ~expected:"collapse" "border-collapse: unset";
  check_eval_value "currentColor uses explicit current color" ~ctx
    ~expected:"red" "border-color: currentColor";
  check_eval_value "custom property var resolves from context" ~ctx
    ~expected:"red" "color: var(--brand)";
  check_eval_value "custom property fallback resolves when missing" ~ctx
    ~expected:"green" "color: var(--missing, green)";
  check_eval_value "custom length var resolves then computes" ~ctx
    ~expected:"16px" "margin-left: var(--gap)";
  check_eval_value "rem resolves against root font size" ~ctx ~expected:"32px"
    "margin-left: 2rem";
  check_eval_value "em resolves against parent font size" ~ctx ~expected:"20px"
    "font-size: 2em";
  check_eval_value "vw resolves against viewport width" ~ctx ~expected:"512px"
    "width: 50vw";
  check_eval_value "vh resolves against viewport height" ~ctx ~expected:"192px"
    "height: 25vh";
  check_eval_value "cqw resolves against container width" ~ctx ~expected:"320px"
    "width: 50cqw";
  check_eval_value "cqh resolves against container height" ~ctx
    ~expected:"120px" "height: 25cqh";
  check_eval_value "relative URL resolves against base URL" ~ctx
    ~expected:"url(https://example.test/img/logo.svg)"
    "background-image: url(../img/logo.svg)";
  check_eval_preserves "missing var without fallback is unresolved" ~ctx
    "color: var(--missing)";
  check_eval_preserves "viewport unit without viewport context is unresolved"
    ~ctx:Css.Context.empty "width: 50vw";
  check_eval_preserves "currentColor without current color is unresolved"
    ~ctx:Css.Context.empty "color: currentColor";
  check_eval_preserves "relative URL without base URL is unresolved"
    ~ctx:Css.Context.empty "background-image: url(../img/logo.svg)"

let computed_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--brand: red");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--accent: var(--brand)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--cycle-a: var(--cycle-b)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--cycle-b: var(--cycle-a)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--cycle-fb: var(--cycle-fb, red)");
          Css_test_helpers.ok (Css.Declaration.of_string "--space: 2em");
        ]
      ~inherited_values:
        [ Css_test_helpers.ok (Css.Declaration.of_string "font-size: 12px") ]
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: canvastext");
          Css_test_helpers.ok (Css.Declaration.of_string "font-size: medium");
          Css_test_helpers.ok (Css.Declaration.of_string "line-height: normal");
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
        ]
      ~base_url:"https://example.test/assets/css/theme.css"
      ~root_font_size:(Css.Values.Px 20.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1200.)
      ~viewport_height:(Css.Values.Px 800.)
      ~container_width:(Css.Values.Px 300.)
      ~container_height:(Css.Values.Px 900.) ()
  in
  check_eval_value "inherit on root falls back to property initial" ~ctx
    ~expected:"canvastext" "color: inherit";
  check_eval_value "nested custom property var resolves transitively" ~ctx
    ~expected:"red" "color: var(--accent)";
  check_eval_value "nested var fallback resolves transitively" ~ctx
    ~expected:"red" "color: var(--missing, var(--accent))";
  check_eval_value "custom length var resolves with parent font context" ~ctx
    ~expected:"24px" "margin-left: var(--space)";
  check_eval_value "font-size percentage resolves against parent font" ~ctx
    ~expected:"18px" "font-size: 150%";
  check_eval_value "absolute inches canonicalize to px" ~ctx ~expected:"96px"
    "margin-left: 1in";
  check_eval_value "absolute points canonicalize to px" ~ctx ~expected:"96px"
    "margin-left: 72pt";
  check_eval_value "viewport vmin uses smaller viewport side" ~ctx
    ~expected:"80px" "width: 10vmin";
  check_eval_value "viewport vmax uses larger viewport side" ~ctx
    ~expected:"120px" "width: 10vmax";
  check_eval_value "container cqmin uses smaller container side" ~ctx
    ~expected:"30px" "width: 10cqmin";
  check_eval_value "container cqmax uses larger container side" ~ctx
    ~expected:"90px" "width: 10cqmax";
  check_eval_value "same-directory URL resolves against base URL" ~ctx
    ~expected:"url(https://example.test/assets/css/panel.css)"
    "background-image: url(panel.css)";
  check_eval_value "root-relative URL preserves origin" ~ctx
    ~expected:"url(https://example.test/icons/logo.svg)"
    "background-image: url(/icons/logo.svg)";
  check_eval_preserves "custom property cycle is unresolved" ~ctx
    "color: var(--cycle-a)";
  (* A cyclic custom property is invalid at computed-value time: its own var()
     fallback does not rescue it, so the consumer's fallback wins. *)
  check_eval_value "cyclic var ignores its own fallback for the consumer's" ~ctx
    ~expected:"blue" "color: var(--cycle-fb, blue)";
  check_eval_preserves "width percentage needs layout context" ~ctx "width: 50%";
  check_eval_preserves "em unit without parent font context is unresolved"
    ~ctx:Css.Context.empty "margin-left: 2em";
  check_eval_preserves "rem unit without root font context is unresolved"
    ~ctx:Css.Context.empty "margin-left: 2rem";
  check_eval_preserves "container unit without container context is unresolved"
    ~ctx:Css.Context.empty "width: 50cqw"

let computed_calc_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--pad-y: 1em");
          Css_test_helpers.ok (Css.Declaration.of_string "--pad-x: 2rem");
          Css_test_helpers.ok (Css.Declaration.of_string "--brand-alpha: 0.5");
        ]
      ~inherited_values:
        [ Css_test_helpers.ok (Css.Declaration.of_string "font-size: 12px") ]
      ~initial_values:
        [ Css_test_helpers.ok (Css.Declaration.of_string "opacity: 1") ]
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~viewport_width:(Css.Values.Px 1000.)
      ~viewport_height:(Css.Values.Px 600.)
      ~container_width:(Css.Values.Px 500.)
      ~container_height:(Css.Values.Px 400.) ()
  in
  check_eval_value "calc resolves compatible absolute and rem units" ~ctx
    ~expected:"18px" "margin-left: calc(1rem + 2px)";
  check_eval_value "calc resolves multiplication with rem" ~ctx ~expected:"32px"
    "margin-left: calc(2 * 1rem)";
  check_eval_value "calc resolves division with em" ~ctx ~expected:"6px"
    "margin-left: calc(1em / 2)";
  check_eval_value "margin shorthand computes each length slot" ~ctx
    ~expected:"12px 32px" "margin: 1em 2rem";
  check_eval_value "padding shorthand computes var-backed slots" ~ctx
    ~expected:"12px 32px" "padding: var(--pad-y) var(--pad-x)";
  check_eval_value "border-width shorthand computes absolute units" ~ctx
    ~expected:"1px 2px 4px 8px" "border-width: 1px 0.125rem 4px 0.5rem";
  check_eval_value "number custom property resolves for opacity" ~ctx
    ~expected:"0.5" "opacity: var(--brand-alpha)";
  check_eval_value "viewport dvw resolves against viewport width" ~ctx
    ~expected:"250px" "width: 25dvw";
  check_eval_value "viewport svh resolves against viewport height" ~ctx
    ~expected:"60px" "height: 10svh";
  check_eval_value "viewport dvh resolves against viewport height" ~ctx
    ~expected:"60px" "height: 10dvh";
  check_eval_value "container cqi resolves against inline size" ~ctx
    ~expected:"125px" "width: 25cqi";
  check_eval_value "container cqb resolves against block size" ~ctx
    ~expected:"100px" "height: 25cqb";
  check_eval "calc partially residualizes known length under unresolved layout"
    ~ctx ~expected:"width: calc(100% - 16px)" "width: calc(100% - 1rem)";
  check_eval_preserves "font metric unit ch needs font metrics" ~ctx
    "width: 4ch";
  check_eval_preserves "font metric unit ex needs font metrics" ~ctx
    "width: 2ex"

let eval_calc_family_contract () =
  (* The reader holds calc unfolded; all-constant reduction is an optimize
     transform, and eval reduces what the supplied context resolves. These
     eval-time tests exercise var-substitution chains and mixed-unit calc that
     needs a viewport / font / container context to fold. *)
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok
            (Css.Declaration.of_string "--lp: calc(50% + 1rem)");
        ]
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~viewport_width:(Css.Values.Px 1000.)
      ~container_width:(Css.Values.Px 400.) ()
  in
  check_eval "eval partially folds length-percentage calc" ~ctx
    ~expected:"width: calc(50% + 16px)" "width: var(--lp)";
  (* The context supplies no viewport height, so [dvh] stays a live leaf while
     the [rem] term folds. *)
  check_eval "eval preserves unknown viewport leaf after folding known units"
    ~ctx ~expected:"height: calc(16px + 10dvh)" "height: calc(1rem + 10dvh)"

let eval_ast_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--brand: red");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--gap: calc(1rem + 2px)");
          Css_test_helpers.ok (Css.Declaration.of_string "--rgb: 10 20 30");
          Css_test_helpers.ok (Css.Declaration.of_string "--alpha: 0.5");
          Css_test_helpers.ok (Css.Declaration.of_string "--shift: 1rem");
          Css_test_helpers.ok (Css.Declaration.of_string "--ring-offset: 2px");
          Css_test_helpers.ok (Css.Declaration.of_string "--ring-color: blue");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: blue");
          Css_test_helpers.ok (Css.Declaration.of_string "font-size: 12px");
        ]
      ~initial_values:
        [ Css_test_helpers.ok (Css.Declaration.of_string "width: auto") ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1000.)
      ~container_width:(Css.Values.Px 400.) ()
  in
  check_eval "eval resolves inherited css-wide keyword" ~ctx
    ~expected:"color: blue" "color: inherit";
  check_eval "eval resolves initial css-wide keyword" ~ctx
    ~expected:"width: auto" "width: initial";
  check_eval "eval resolves color variable" ~ctx ~expected:"color: red"
    "color: var(--brand)";
  check_eval "eval resolves fallback variable" ~ctx ~expected:"color: green"
    "color: var(--missing, green)";
  check_eval "eval resolves currentColor leaf" ~ctx
    ~expected:"border-color: red" "border-color: currentColor";
  check_eval "eval resolves relative URL leaf" ~ctx
    ~expected:"background-image: url(https://example.test/img/logo.svg)"
    "background-image: url(../img/logo.svg)";
  (* The reader holds calc unfolded; the eval contract here exercises mixed-unit
     calc that needs the context to fold (rem -> px via root font size). *)
  check_eval "eval folds known length calc subtree" ~ctx
    ~expected:"margin-left: 18px" "margin-left: calc(1rem + 2px)";
  check_eval "eval preserves unresolved viewport leaf in calc AST" ~ctx
    ~expected:"height: calc(16px + 10vh)" "height: calc(1rem + 10vh)";
  check_eval "eval preserves unresolved percentage leaf in calc AST" ~ctx
    ~expected:"width: calc(100% - 16px)" "width: calc(100% - 1rem)";
  check_eval "eval preserves abstract var leaf in calc AST" ~ctx
    ~expected:"margin-left: calc(16px + var(--future-gap))"
    "margin-left: calc(1rem + var(--future-gap))";
  check_eval "eval resolves vars inside color function AST" ~ctx
    ~expected:"background-color: rgb(10 20 30 / .5)"
    "background-color: rgb(var(--rgb) / var(--alpha))";
  check_eval "eval resolves vars inside transform list AST" ~ctx
    ~expected:"transform: translateX(16px) rotate(45deg)"
    "transform: translateX(var(--shift)) rotate(calc(30deg + 15deg))";
  check_eval "eval resolves vars and calc inside shadow list AST" ~ctx
    ~expected:"box-shadow: 0 0 0 5px blue"
    "box-shadow: 0 0 0 calc(3px + var(--ring-offset)) var(--ring-color)";
  check_eval "eval resolves shorthand slots independently" ~ctx
    ~expected:"padding: 18px 36px" "padding: var(--gap) calc(var(--gap) * 2)"

let eval_spec_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok
            (Css.Declaration.of_string "--gap: calc(1rem + 2px)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--outer: var(--missing, var(--gap))");
          Css_test_helpers.ok
            (Css.Declaration.of_string
               "--nested-fallback: var(--missing, calc(var(--gap) + 1rem))");
          Css_test_helpers.ok (Css.Declaration.of_string "--rgb: 12 34 56");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--alpha: calc(0.2 + 0.3)");
        ]
      ~inherited_values:
        [ Css_test_helpers.ok (Css.Declaration.of_string "color: blue") ]
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
          Css_test_helpers.ok (Css.Declaration.of_string "color: canvastext");
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1000.)
      ~viewport_height:(Css.Values.Px 800.)
      ~container_width:(Css.Values.Px 400.)
      ~container_height:(Css.Values.Px 300.) ()
  in
  check_eval "eval resolves nested var fallback chain" ~ctx
    ~expected:"margin-left: 18px" "margin-left: var(--outer)";
  check_eval "eval resolves fallback containing nested var and calc" ~ctx
    ~expected:"margin-left: 34px" "margin-left: var(--nested-fallback)";
  check_eval "eval applies css-wide fallback after var substitution" ~ctx
    ~expected:"color: blue" "color: var(--missing, inherit)";
  check_eval "eval applies initial fallback after var substitution" ~ctx
    ~expected:"margin-left: 0" "margin-left: var(--missing, initial)";
  check_eval "eval preserves unresolved fallback subtree as AST" ~ctx
    ~expected:"margin-left: calc(18px + var(--user-gap))"
    "margin-left: var(--missing, calc(var(--gap) + var(--user-gap)))";
  check_eval "eval preserves mixed percentage and resolved dimension" ~ctx
    ~expected:"width: calc(50% + 18px)" "width: calc(50% + var(--gap))";
  check_eval "eval resolves percentage where property defines computed basis"
    ~ctx ~expected:"font-size: 18px" "font-size: calc(100% + 0.5em)";
  check_eval "eval canonicalizes absolute units inside calc" ~ctx
    ~expected:"margin-left: 0" "margin-left: calc(1in - 72pt)";
  check_eval "eval resolves min with comparable computed lengths" ~ctx
    ~expected:"width: 20px" "width: min(10cqi, calc(1rem + 4px))";
  check_eval "eval resolves clamp with comparable computed lengths" ~ctx
    ~expected:"font-size: 18px" "font-size: clamp(1rem, calc(1rem + 2px), 2rem)";
  check_eval "eval resolves vars inside slash color function" ~ctx
    ~expected:"background-color: rgb(12 34 56 / .5)"
    "background-color: rgb(var(--rgb) / var(--alpha))";
  check_eval "eval resolves currentColor inside list item" ~ctx
    ~expected:"box-shadow: 0 0 0 1px blue, 0 0 0 2px red"
    "box-shadow: 0 0 0 1px currentColor, 0 0 0 2px red";
  check_eval "eval resolves nested URL inside image function" ~ctx
    ~expected:
      "background-image: image-set(url(https://example.test/img/a.png) 1x)"
    "background-image: image-set(url(../img/a.png) 1x)"

let eval_css_spec_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--brand: red");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--gap: calc(1rem + 2px)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--angle: calc(30deg + 15deg)");
          Css_test_helpers.ok (Css.Declaration.of_string "--alpha: 0.5");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--shadow: 0 0 var(--gap) currentColor");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--image: url(../img/pattern.svg)");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: blue");
          Css_test_helpers.ok (Css.Declaration.of_string "display: block");
          Css_test_helpers.ok (Css.Declaration.of_string "font-size: 10px");
        ]
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "display: inline");
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
          Css_test_helpers.ok (Css.Declaration.of_string "opacity: 1");
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1200.) ()
  in
  check_eval "eval applies inherited css-wide keyword from var fallback" ~ctx
    ~expected:"display: block" "display: var(--missing, inherit)";
  check_eval "eval applies unset fallback using inherited property default" ~ctx
    ~expected:"color: blue" "color: var(--missing, unset)";
  check_eval "eval applies unset fallback using non-inherited initial default"
    ~ctx ~expected:"margin-left: 0" "margin-left: var(--missing, unset)";
  check_eval "eval applies initial fallback after nested var fallback" ~ctx
    ~expected:"opacity: 1" "opacity: var(--missing, var(--other, initial))";
  check_eval "eval residualizes unresolved nested fallback after known rewrite"
    ~ctx ~expected:"margin-left: calc(18px + var(--fluid-gap))"
    "margin-left: var(--missing, calc(var(--gap) + var(--fluid-gap)))";
  check_eval "eval resolves currentColor and length inside shadow custom prop"
    ~ctx ~expected:"box-shadow: 0 0 18px blue" "box-shadow: var(--shadow)";
  check_eval "eval resolves url custom property after substitution" ~ctx
    ~expected:"background-image: url(https://example.test/img/pattern.svg)"
    "background-image: var(--image)";
  check_eval "eval resolves transform longhand angle variable" ~ctx
    ~expected:"rotate: 45deg" "rotate: var(--angle)";
  check_eval "eval resolves opacity alpha inside color function from var" ~ctx
    ~expected:"background-color: rgb(255 0 0 / .5)"
    "background-color: rgb(255 0 0 / var(--alpha))";
  check_eval "eval preserves division by zero calc as residual" ~ctx
    ~expected:"margin-left: calc(16px / 0)" "margin-left: calc(1rem / 0)";
  check_eval "eval preserves sibling-dependent calc as residual" ~ctx
    ~expected:"z-index: calc(sibling-index() + 1)"
    "z-index: calc(sibling-index() + 1)";
  check_eval
    "eval resolves font-relative percentage only where property has basis" ~ctx
    ~expected:"font-size: 15px" "font-size: calc(100% + 0.5em)";
  check_eval "eval leaves layout percentage abstract while reducing known side"
    ~ctx ~expected:"width: calc(50% + 16px)" "width: calc(50% + 1rem)"

let runtime_boundary_contract () =
  let ctx =
    Css.Context.v ~root_font_size:(Css.Values.Px 16.)
      ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Blue) ()
  in
  check_eval_preserves "layout percentage remains a computed percentage" ~ctx
    "width: 50%";
  check_eval "layout calc partially folds lengths but preserves percentage" ~ctx
    ~expected:"width: calc(50% - 16px)" "width: calc(50% - 1rem)";
  check_eval_preserves "grid fr tracks are layout-only used values" ~ctx
    "grid-template-columns: minmax(min-content, 1fr) fit-content(20%)";
  check_eval_preserves "flex content basis is layout-only" ~ctx
    "flex-basis: content";
  check_eval_preserves "aspect-ratio affects layout, not computed eval" ~ctx
    "aspect-ratio: 16 / 9";
  check_eval_preserves "font metric ch unit needs font metrics" ~ctx
    "width: 12ch";
  check_eval_preserves "font metric cap unit needs font metrics" ~ctx
    "height: 2cap";
  check_eval_preserves "color interpolation space is rendering-time behavior"
    ~ctx "color: color-mix(in oklab, red 40%, blue)";
  check_eval_preserves "gamut mapping is rendering-time behavior" ~ctx
    "color: color(display-p3 1 0.5 0)";
  check_eval_preserves "filter raster effects are rendering-time behavior" ~ctx
    "filter: blur(4px) contrast(120%)";
  check_eval_preserves "mask painting is rendering-time behavior" ~ctx
    "mask-image: linear-gradient(black, transparent)";
  check_eval_preserves "scroll timeline sampling is runtime behavior" ~ctx
    "animation-timeline: scroll()";
  check_eval_preserves "view timeline sampling is runtime behavior" ~ctx
    "view-timeline: --reveal block";
  check_eval_stylesheet "font loading stays a stylesheet syntax boundary" ~ctx
    ~expected:
      "@font-face { font-family: Brand; src: url(brand.woff2) \
       format(\"woff2\"); font-display: swap; }"
    "@font-face { font-family: Brand; src: url(brand.woff2) format(\"woff2\"); \
     font-display: swap; }";
  check_eval_stylesheet "keyframe timing stays runtime, values still eval" ~ctx
    ~expected:
      "@keyframes fade { from { opacity: 0; } to { opacity: 1; transform: \
       translateX(16px); } }"
    "@keyframes fade { from { opacity: 0; } to { opacity: 1; transform: \
     translateX(1rem); } }"

let eval_observable_matrix_contract () =
  let full_ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--gap: 1rem");
          Css_test_helpers.ok (Css.Declaration.of_string "--angle: 45deg");
          Css_test_helpers.ok (Css.Declaration.of_string "--duration: 500ms");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: blue");
          Css_test_helpers.ok (Css.Declaration.of_string "font-size: 10px");
        ]
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "display: inline");
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
          Css_test_helpers.ok (Css.Declaration.of_string "color: canvastext");
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 10.)
      ~current_color:(Css.Values.Named Css.Values.Blue)
      ~viewport_width:(Css.Values.Px 1000.)
      ~viewport_height:(Css.Values.Px 500.)
      ~container_width:(Css.Values.Px 400.)
      ~container_height:(Css.Values.Px 300.) ()
  in
  let empty_ctx = Css.Context.empty in
  check_eval "observable custom property resolves present var" ~ctx:full_ctx
    ~expected:"margin-left: 16px" "margin-left: var(--gap)";
  check_eval_preserves "observable missing custom property stays residual"
    ~ctx:empty_ctx "margin-left: var(--missing)";
  check_eval "observable inherited value resolves inherit" ~ctx:full_ctx
    ~expected:"color: blue" "color: inherit";
  check_eval_preserves "observable absent inherited value preserves inherit"
    ~ctx:empty_ctx "color: inherit";
  check_eval "observable initial value resolves initial" ~ctx:full_ctx
    ~expected:"display: inline" "display: initial";
  check_eval_preserves "observable absent initial value preserves initial"
    ~ctx:empty_ctx "display: initial";
  check_eval "observable currentColor resolves present color" ~ctx:full_ctx
    ~expected:"border-color: blue" "border-color: currentColor";
  check_eval_preserves "observable absent currentColor preserves currentColor"
    ~ctx:empty_ctx "border-color: currentColor";
  check_eval "observable root font size resolves rem" ~ctx:full_ctx
    ~expected:"margin-left: 16px" "margin-left: 1rem";
  check_eval_preserves "observable absent root font size preserves rem"
    ~ctx:empty_ctx "margin-left: 1rem";
  check_eval "observable parent font size resolves em" ~ctx:full_ctx
    ~expected:"font-size: 20px" "font-size: 2em";
  check_eval_preserves "observable absent parent font size preserves em"
    ~ctx:empty_ctx "font-size: 2em";
  check_eval "observable viewport resolves vh" ~ctx:full_ctx
    ~expected:"height: 50px" "height: 10vh";
  check_eval_preserves "observable absent viewport preserves vh" ~ctx:empty_ctx
    "height: 10vh";
  check_eval "observable container resolves cqi" ~ctx:full_ctx
    ~expected:"width: 100px" "width: 25cqi";
  check_eval_preserves "observable absent container preserves cqi"
    ~ctx:empty_ctx "width: 25cqi";
  check_eval "observable base URL resolves relative URL" ~ctx:full_ctx
    ~expected:"background-image: url(https://example.test/img/a.png)"
    "background-image: url(../img/a.png)";
  check_eval_preserves "observable absent base URL preserves relative URL"
    ~ctx:empty_ctx "background-image: url(../img/a.png)";
  check_eval_preserves "observable absent cascade chain preserves revert-layer"
    ~ctx:empty_ctx "color: revert-layer";
  check_eval_preserves "observable absent cascade chain preserves revert"
    ~ctx:empty_ctx "color: revert";
  check_eval "observable angle calc folds with typed value" ~ctx:full_ctx
    ~expected:"rotate: 45deg" "rotate: var(--angle)";
  check_eval_preserves "observable absent angle var stays residual"
    ~ctx:empty_ctx "rotate: var(--angle)";
  check_eval "observable duration var folds with typed value" ~ctx:full_ctx
    ~expected:"animation-duration: 0.5s" "animation-duration: var(--duration)";
  check_eval_preserves "observable absent duration var stays residual"
    ~ctx:empty_ctx "animation-duration: var(--duration)"

let eval_stylesheet_spec_edge_contract () =
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "--brand: red");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--gap: calc(1rem + 2px)");
          Css_test_helpers.ok (Css.Declaration.of_string "--alpha: 0.5");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--angle: calc(30deg + 15deg)");
        ]
      ~base_url:"https://example.test/css/app.css"
      ~root_font_size:(Css.Values.Px 16.) ~parent_font_size:(Css.Values.Px 12.)
      ~current_color:(Css.Values.Named Css.Values.Red)
      ~viewport_width:(Css.Values.Px 1000.) ()
  in
  check_eval_stylesheet
    "stylesheet eval walks nested rules, grouping at-rules, and keyframes" ~ctx
    ~expected:
      {|
        @import url(theme.css);
        @font-face { font-family: Brand; src: url(brand.woff2); }
        .card {
          color: red;
          margin-left: 18px;
          background-image: url(https://example.test/img/card.svg);
          & .title { border-color: red; padding: 18px; }
          @media (min-width: 40rem) { width: calc(100% - 16px); }
          @supports (display: grid) { transform: rotate(45deg); }
        }
        @starting-style { .card { opacity: 0.5; } }
        @keyframes fade {
          from { opacity: 0.5; }
          to { transform: translateX(18px); }
        }
        @page { margin: 18px; }
        @position-try --flip { inset-inline-start: 18px; }
      |}
    {|
        @import url(theme.css);
        @font-face { font-family: Brand; src: url(brand.woff2); }
        .card {
          color: var(--brand);
          margin-left: var(--gap);
          background-image: url(../img/card.svg);
          & .title { border-color: currentColor; padding: var(--gap); }
          @media (min-width: 40rem) { width: calc(100% - 1rem); }
          @supports (display: grid) { transform: rotate(var(--angle)); }
        }
        @starting-style { .card { opacity: var(--alpha); } }
        @keyframes fade {
          from { opacity: var(--alpha); }
          to { transform: translateX(var(--gap)); }
        }
        @page { margin: var(--gap); }
        @position-try --flip { inset-inline-start: var(--gap); }
      |};
  check_eval_stylesheet_preserves_structure
    "stylesheet eval preserves non-declaration structure" ~ctx
    {|
      @import url(theme.css);
      @layer theme, components, utilities;
      @layer components {
        .card {
          color: var(--brand);
          & .title { border-color: currentColor; padding: var(--gap); }
          @media (min-width: 40rem) { width: calc(100% - 1rem); }
          @supports (display: grid) { transform: rotate(var(--angle)); }
        }
      }
      @scope (.card) to (.boundary) {
        .title { color: var(--brand); }
      }
      @starting-style { .card { opacity: var(--alpha); } }
      @keyframes fade {
        from { opacity: var(--alpha); }
        to { transform: translateX(var(--gap)); }
      }
      @page { margin: var(--gap); }
      @position-try --flip { inset-inline-start: var(--gap); }
    |}

let eval_laws_contract () =
  let weak_ctx =
    Css.Context.v ~root_font_size:(Css.Values.Px 16.)
      ~parent_font_size:(Css.Values.Px 12.) ()
  in
  let strong_ctx =
    Css.Context.v ~root_font_size:(Css.Values.Px 16.)
      ~parent_font_size:(Css.Values.Px 12.)
      ~viewport_height:(Css.Values.Px 1000.) ()
  in
  check_eval_idempotent "eval is idempotent on reduced calc AST" ~ctx:weak_ctx
    "height: calc(1rem + 10vh)";
  check_eval_context_extension
    "eval with stronger context equals staged weaker-then-stronger eval"
    ~weak_ctx ~strong_ctx ~expected:"height: 116px" "height: calc(1rem + 10vh)";
  check_eval_context_extension
    "eval context extension recurses through nested calc AST" ~weak_ctx
    ~strong_ctx ~expected:"height: 124px" "height: calc((1rem + 8px) + 10vh)"

let runtime_var_not_folded_contract () =
  let open Css.Values in
  (* A [~runtime:true] theme var keeps its var() reference even when its default
     is known, so a script can still override --spacing at runtime. *)
  let _decl, spacing = Css.var ~runtime:true "spacing" Length (Px 4.) in
  let decl = Css.width (Calc (Expr (Var spacing, Mul, Num 4.))) in
  Alcotest.(check decl_t)
    "runtime var stays a var() in calc" decl
    (Css.eval_declaration Css.Context.empty decl);
  (* Without [~runtime] the same calc folds to a constant through the
     default. *)
  let _decl, fixed = Css.var "fixed-spacing" Length (Px 4.) in
  let folding = Css.width (Calc (Expr (Var fixed, Mul, Num 4.))) in
  Alcotest.(check bool)
    "non-runtime var folds" false
    (String.equal
       (Css.Declaration.to_string folding)
       (Css.Declaration.to_string
          (Css.eval_declaration Css.Context.empty folding)));
  (* Multiplicative identities never read the variable's value, so they still
     simplify a runtime var; only value resolution ([* n], n >= 2) is
     skipped. *)
  let eval c =
    Css.Declaration.to_string ~minify:true
      (Css.eval_declaration Css.Context.empty (Css.width (Calc c)))
  in
  Alcotest.(check string)
    "runtime var * 0 folds to 0" "width:0"
    (eval (Expr (Var spacing, Mul, Num 0.)));
  Alcotest.(check string)
    "runtime var * 1 folds to the operand" "width:var(--spacing)"
    (eval (Expr (Var spacing, Mul, Num 1.)));
  Alcotest.(check string)
    "runtime var * 4 keeps the var() reference" "width:calc(var(--spacing)*4)"
    (eval (Expr (Var spacing, Mul, Num 4.)));
  (* The spacing utilities multiply the theme var, so the [* 1] case must reach
     the operand through a property shorthand too: [p-N] for N=1 is [padding:
     var(--spacing)], not [calc(var(--spacing) * 1)]. *)
  Alcotest.(check string)
    "runtime var * 1 in the padding shorthand folds to the operand"
    "padding:var(--spacing)"
    (Css.Declaration.to_string ~minify:true
       (Css.eval_declaration Css.Context.empty
          (Css.padding [ Calc (Expr (Var spacing, Mul, Num 1.)) ])))

(* CSS Variables 1 sec. 3 replaces a [var()] with the custom property's value
   "if the value of the custom property ... is anything but the initial value",
   and puts the fallback in only where it is not. A binding this resolver cannot
   make a value of - an empty token stream, one the property's grammar refuses,
   or a CSS-wide keyword, whose meaning sec. 2.1 leaves to the cascade against
   an element this resolver has none of - is one of the former, so the
   declaration is invalid at computed-value time and the fallback is no answer
   to it. *)
let unreadable_binding_is_not_an_absent_one () =
  let ctx binding =
    Css.Context.v
      ~custom_properties:
        [
          (match Css.Declaration.parse_custom_property "--x" binding with
          | Some d -> d
          | None -> Alcotest.failf "cannot bind --x to %S" binding);
        ]
      ()
  in
  List.iter
    (fun (binding, expected) ->
      check_eval_stylesheet
        (String.concat "" [ "--x: "; binding ])
        ~ctx:(ctx binding) ~expected ".a{width:var(--x,8px)}")
    [
      ("", ".a{width:var(--x,8px)}");
      ("notalength", ".a{width:var(--x,8px)}");
      ("unset", ".a{width:var(--x,8px)}");
      ("inherit", ".a{width:var(--x,8px)}");
      (* A binding this resolver can read still answers. *)
      ("2px", ".a{width:2px}");
    ];
  (* Sec. 3 makes a custom property that references itself invalid at
     computed-value time, which is the guaranteed-invalid value, so there the
     reference's fallback is exactly the answer. *)
  check_eval_stylesheet "--x: var(--x)" ~ctx:(ctx "var(--x)")
    ~expected:".a{width:8px}" ".a{width:var(--x,8px)}"

let suite =
  ( "context_eval",
    [
      Alcotest.test_case "runtime var not folded" `Quick
        runtime_var_not_folded_contract;
      Alcotest.test_case "computed value resolution contract" `Quick
        test_computed_value_resolution_contract;
      Alcotest.test_case "computed value edge contract" `Quick
        computed_edge_contract;
      Alcotest.test_case "computed value calc and shorthand contract" `Quick
        computed_calc_contract;
      Alcotest.test_case "eval calc family contract" `Quick
        eval_calc_family_contract;
      Alcotest.test_case "eval AST contract" `Quick eval_ast_contract;
      Alcotest.test_case "eval spec edge contract" `Quick
        eval_spec_edge_contract;
      Alcotest.test_case "eval CSS spec edge contract" `Quick
        eval_css_spec_edge_contract;
      Alcotest.test_case "runtime boundary contract" `Quick
        runtime_boundary_contract;
      Alcotest.test_case "eval observable matrix contract" `Quick
        eval_observable_matrix_contract;
      Alcotest.test_case "eval stylesheet spec edge contract" `Quick
        eval_stylesheet_spec_edge_contract;
      Alcotest.test_case "eval laws contract" `Quick eval_laws_contract;
      Alcotest.test_case "an unreadable binding is not an absent one" `Quick
        unreadable_binding_is_not_an_absent_one;
    ] )
