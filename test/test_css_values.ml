open Cascade
open Css
open Css_helpers

let public_property_edges () =
  let sheet =
    property ~name:"--gap" Length ~initial_value:(Px 1.) ~inherits:false ()
  in
  match statements sheet with
  | [ stmt ] -> (
      match as_property stmt with
      | Some (Property_info info) -> (
          Alcotest.(check string) "registered name" "--gap" info.name;
          Alcotest.(check bool) "inherits flag" false info.inherits;
          match (info.syntax, info.initial_value) with
          | Css.Variables.Length, Some (Px n) ->
              Alcotest.(check (float 0.0001)) "initial value" 1. n
          | _ -> Alcotest.fail "registered property shape changed")
      | None -> Alcotest.fail "expected @property statement")
  | _ -> Alcotest.fail "expected one @property statement"

let public_theme_edges () =
  let guarded = theme_guarded ~var_name:"brand" (color (hex "#ff0000")) in
  let sheet =
    v
      [
        rule ~selector:(Selector.class_ "card")
          [ guarded; background_color (hex "#ffffff") ];
      ]
  in
  let empty_theme = Css.Pp.String_set.empty in
  let brand_theme = Css.Pp.String_set.add "brand" empty_theme in
  Alcotest.(check string)
    "guarded declaration hidden" ".card{background-color:#fff}"
    (sheet |> Css.resolve_theme ~theme:empty_theme |> to_string ~minify:true);
  Alcotest.(check string)
    "guarded declaration shown" ".card{color:#f00;background-color:#fff}"
    (sheet |> Css.resolve_theme ~theme:brand_theme |> to_string ~minify:true);
  Alcotest.(check string)
    "guarded declaration shown optimize+minify"
    ".card{color:red;background-color:#fff}"
    (sheet
    |> Css.resolve_theme ~theme:brand_theme
    |> Css.optimize |> to_string ~minify:true)

(* A theme guard is a compile-time filter on the declaration it wraps, so the
   keep-set decides its fate wherever that declaration sits. [@keyframes],
   [@page], [@position-try] and [@supports-condition] hold declarations directly
   rather than in a nested block, so a walk that only descends through blocks
   never reaches their guards and prints them as declarations the theme never
   selected. *)
let public_theme_guards_in_declaration_at_rules () =
  let guarded () = theme_guarded ~var_name:"brand" (color (hex "#ff0000")) in
  let resolved stmts =
    v stmts
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
    |> to_string ~minify:true
  in
  let frame : Stylesheet.keyframe =
    {
      selector = Keyframe.Positions [ Keyframe.From ];
      declarations = [ guarded () ];
    }
  in
  let margin : Stylesheet.page_margin_rule =
    { name = "top-left"; descriptors = [ guarded () ] }
  in
  let cases =
    [
      ("@keyframes", [ Stylesheet.keyframes "k" [ frame ] ]);
      ("@-webkit-keyframes", [ Stylesheet.Webkit_keyframes ("k", [ frame ]) ]);
      ("@-moz-keyframes", [ Stylesheet.Moz_keyframes ("k", [ frame ]) ]);
      ("@page", [ Stylesheet.Page ([], [ guarded () ]) ]);
      ("@page margin box", [ Stylesheet.Page_with_margins ([], [], [ margin ]) ]);
      ("@position-try", [ Stylesheet.Position_try ("--pt", [ guarded () ]) ]);
      ( "@supports-condition",
        [ Stylesheet.Supports_condition ("--sc", [ guarded () ]) ] );
    ]
  in
  match
    List.filter_map
      (fun (name, stmts) ->
        let printed = resolved stmts in
        if Astring.String.is_infix ~affix:"color" printed then
          Fmt.kstr Option.some "%s kept the guarded declaration: %S" name
            printed
        else Option.none)
      cases
  with
  | [] -> ()
  | kept ->
      Alcotest.failf "theme guards left unresolved:\n%s"
        (String.concat "\n" kept)

let public_value_combinator_edges () =
  let _spacing_decl, spacing = var "spacing" Length (Rem 0.25) in
  let check_padding_calc ?optimized label expected calc =
    let sheet =
      v [ rule ~selector:(Selector.class_ "p-1") [ padding [ Calc calc ] ] ]
    in
    Alcotest.(check string) label expected (to_string ~minify:true sheet);
    match optimized with
    | None -> ()
    | Some expected ->
        Alcotest.(check string)
          (label ^ " optimize+minify")
          expected (minify sheet)
  in
  let calc_var_sheet =
    v
      [
        rule ~selector:(Selector.class_ "p-1")
          [ padding [ Calc (Var spacing) ] ];
      ]
  in
  Alcotest.(check string)
    "padding calc(var()) preserves runtime boundary"
    ".p-1{padding:calc(var(--spacing))}"
    (to_string ~minify:true calc_var_sheet);
  let calc_lifted_var_x1_sheet =
    v
      [
        rule ~selector:(Selector.class_ "p-1")
          [
            padding
              [ Calc (Calc.mul (Calc.length (Var spacing)) (Calc.float 1.0)) ];
          ];
      ]
  in
  Alcotest.(check string)
    "padding calc lifted var times one keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing)*1)}"
    (to_string ~minify:true calc_lifted_var_x1_sheet);
  check_padding_calc "padding one times lifted var keeps var arithmetic"
    ".p-1{padding:calc(1*var(--spacing))}"
    (Calc.mul (Calc.float 1.0) (Calc.length (Var spacing)));
  check_padding_calc "padding lifted var divided by one keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing)/1)}"
    (Calc.div (Calc.length (Var spacing)) (Calc.float 1.0));
  check_padding_calc "padding lifted var plus zero keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing) + 0px)}"
    (Calc.add (Calc.length (Var spacing)) (Calc.length (Px 0.)));
  check_padding_calc "padding zero plus lifted var keeps var arithmetic"
    ".p-1{padding:calc(0px + var(--spacing))}"
    (Calc.add (Calc.length (Px 0.)) (Calc.length (Var spacing)));
  check_padding_calc "padding lifted var minus zero keeps var arithmetic"
    ".p-1{padding:calc(var(--spacing) - 0px)}"
    (Calc.sub (Calc.length (Var spacing)) (Calc.length (Px 0.)));
  check_padding_calc "padding nested lifted var identities keep var arithmetic"
    ".p-1{padding:calc(var(--spacing)*1 + 0px)}"
    (Calc.add
       (Calc.mul (Calc.length (Var spacing)) (Calc.float 1.0))
       (Calc.length (Px 0.)));
  check_padding_calc ~optimized:".p-1{padding:calc(3px + var(--spacing))}"
    "padding var-free left subtree may fold before var"
    ".p-1{padding:calc(1px + 2px + var(--spacing))}"
    (Calc.add
       (Calc.add (Calc.length (Px 1.)) (Calc.length (Px 2.)))
       (Calc.length (Var spacing)));
  check_padding_calc ~optimized:".p-1{padding:calc(var(--spacing)*6)}"
    "padding var-free right subtree may fold after var"
    ".p-1{padding:calc(var(--spacing)*2*3)}"
    (Calc.mul
       (Calc.length (Var spacing))
       (Calc.mul (Calc.float 2.0) (Calc.float 3.0)));
  check_padding_calc ~optimized:".p-1{padding:calc(50% - var(--spacing))}"
    "padding var-free percentage subtree may fold before var"
    ".p-1{padding:calc(100%/2 - var(--spacing))}"
    (Calc.sub
       (Calc.div (Calc.length (Pct 100.)) (Calc.float 2.0))
       (Calc.length (Var spacing)));
  let bare_var_sheet =
    v [ rule ~selector:(Selector.class_ "p-1") [ padding [ Var spacing ] ] ]
  in
  Alcotest.(check string)
    "padding bare var stays bare" ".p-1{padding:var(--spacing)}"
    (to_string ~minify:true bare_var_sheet);

  (* [Calc.float] is generic ['a calc], so a non-length calc - here a flex_basis
     calc with a numeric multiplier - can be built through the typed API. *)
  let flex_basis_calc_sheet =
    v
      [
        rule
          ~selector:(Selector.class_ "basis-4")
          [ flex_basis (Calc Calc.(var "spacing" * float 4.)) ];
      ]
  in
  Alcotest.(check string)
    "flex-basis calc multiplier builds via generic Calc.float"
    ".basis-4{flex-basis:calc(var(--spacing)*4)}"
    (to_string ~minify:true flex_basis_calc_sheet);
  (* z-index carries a typed calc, so a var-based z-index calc builds through
     the typed API rather than a raw string. *)
  let z_index_calc_sheet =
    v
      [
        rule ~selector:(Selector.class_ "z")
          [ z_index (Calc (Calc.var "layer")) ];
      ]
  in
  Alcotest.(check string)
    "z-index calc builds via typed calc" ".z{z-index:calc(var(--layer))}"
    (to_string ~minify:true z_index_calc_sheet);
  (* order carries a typed calc, so a var-based order calc builds through the
     typed API rather than a raw string. *)
  let order_calc_sheet =
    v [ rule ~selector:(Selector.class_ "o") [ order (Calc (Calc.var "o")) ] ]
  in
  Alcotest.(check string)
    "order calc builds via typed calc" ".o{order:calc(var(--o))}"
    (to_string ~minify:true order_calc_sheet);
  (* grid-line carries a typed calc, so col-start-[calc(...)] and similar
     arbitraries build through the typed API rather than a raw string. *)
  let grid_line_calc_sheet =
    v
      [
        rule ~selector:(Selector.class_ "g")
          [ grid_column_start (Calc (Calc.var "n")) ];
      ]
  in
  Alcotest.(check string)
    "grid-line calc builds via typed calc"
    ".g{grid-column-start:calc(var(--n))}"
    (to_string ~minify:true grid_line_calc_sheet);

  let sheet =
    v
      [
        rule ~selector:(Selector.class_ "card")
          [
            border_radius (radius (Rem 0.375));
            gap (gaps ~column:(Rem 0.75) (Rem 0.5));
            font_family (font_stack [ Ui_sans_serif; System_ui; Sans_serif ]);
            text_shadow
              (text_shadow_value ~blur:(Px 4.) ~color:(hex "#000000") (Px 1.)
                 (Px 2.));
            aspect_ratio (ratio 16. 9.);
            columns (columns_both (Rem 12.) 3);
            counter_reset (counter_set [ counter_item ~value:1 "section" ]);
            mask (mask_layers [ mask_layer ~image:(url "mask.svg") () ]);
            outline
              (outline_shorthand ~width:(Px 2.) ~style:Solid
                 ~color:(hex "#0000ff") ());
          ];
        rule
          ~selector:(Selector.class_ "helpers")
          [
            object_position (position_xy (Px 10.) (Px 20.));
            text_overflow (text_overflow_pair Clip (text_overflow_string "..."));
            content
              (content_list
                 [ content_string "Section "; content_counter "section" ]);
            background_image
              (conic_gradient
                 ~config:
                   (conic_gradient_config ~angle:(Deg 45.)
                      ~position:(position_length (Pct 50.))
                      ())
                 [ color_stop (hex "#ff0000"); color_stop (hex "#0000ff") ]);
            background_size (background_size_pair (Px 20.) (Px 30.));
            object_view_box (object_view_box_inset ~right:(Px 1.) (Px 0.));
            grid_template_columns
              (grid_tracks
                 [ Fr 1.; grid_repeat (Count 2) [ Min_max (Px 0., Fr 1.) ] ]);
            grid_row (grid_line_span 2, grid_line_name "footer");
            transform
              (transform_list
                 [ Translate (Px 1., Some (Px 2.)); Rotate (Deg 45.) ]);
            filter (filter_list [ Blur (Px 4.); Opacity (Pct 50.) ]);
            cursor (cursor_url ~hotspot:(1., 2.) ~fallback:Pointer "cursor.svg");
            contain (contain_list [ Layout; Paint ]);
            border_spacing (border_spacing_values [ Px 1.; Px 2. ]);
            border_inline_color
              (logical_border_colors (hex "#ffffff") (hex "#000000"));
            list_style_type
              (list_style_symbols ~kind:Cyclic
                 [
                   list_style_symbol_string "*"; list_style_symbol_url "dot.svg";
                 ]);
            list_style_image (list_style_image_url "bullet.svg");
            fill
              (svg_paint_url
                 ~fallback:(svg_paint_color (hex "#ff0000"))
                 "#paint");
          ];
      ]
  in
  Alcotest.(check string)
    "simple value helpers"
    ".card{border-radius:.375rem;gap:.5rem \
     .75rem;font-family:ui-sans-serif,system-ui,sans-serif;text-shadow:1px 2px \
     4px #000;aspect-ratio:16/9;columns:12rem 3;counter-reset:section \
     1;mask:url(mask.svg);outline:2px solid #00f}.helpers{object-position:10px \
     20px;text-overflow:clip \"...\";content:\"Section \" \
     counter(section);background-image:conic-gradient(from 45deg at \
     50%,#f00,#00f);background-size:20px 30px;object-view-box:inset(0px \
     1px);grid-template-columns:1fr repeat(2,minmax(0px,1fr));grid-row:span \
     2/footer;transform:translate(1px,2px)rotate(45deg);filter:blur(4px)opacity(50%);cursor:url(cursor.svg) \
     1 2,pointer;contain:layout paint;border-spacing:1px \
     2px;border-inline-color:#fff #000;list-style-type:symbols(cyclic\"*\" \
     url(dot.svg));list-style-image:url(bullet.svg);fill:url(#paint)#f00}"
    (to_string ~minify:true sheet);
  Alcotest.(check string)
    "simple value helpers optimize+minify"
    ".card{border-radius:.375rem;gap:.5rem \
     .75rem;font-family:ui-sans-serif,system-ui,sans-serif;text-shadow:1px 2px \
     4px #000;aspect-ratio:16/9;columns:12rem 3;counter-reset:section \
     1;-webkit-mask:url(mask.svg);mask:url(mask.svg);outline:2px solid \
     #00f}.helpers{object-position:10px 20px;text-overflow:clip \
     \"...\";content:\"Section \" \
     counter(section);background-image:conic-gradient(from 45deg at \
     50%,red,#00f);background-size:20px 30px;object-view-box:inset(0 \
     1px);grid-template-columns:1fr repeat(2,minmax(0,1fr));grid-row:span \
     2/footer;transform:translate(1px,2px)rotate(45deg);filter:blur(4px)opacity(.5);cursor:url(cursor.svg) \
     1 2,pointer;contain:layout paint;border-spacing:1px \
     2px;border-inline-color:#fff #000;list-style-type:symbols(cyclic\"*\" \
     url(dot.svg));list-style-image:url(bullet.svg);fill:url(#paint)red}"
    (minify sheet)

let public_theme_var_rendering_edges () =
  let sans_stack : font_family =
    font_stack [ Ui_sans_serif; System_ui; Sans_serif ]
  in
  let fallback_stack : font_family = font_stack [ Arial; Sans_serif ] in
  let _font_decl, font_sans = var "font-sans" Font_family sans_stack in
  let _font_fb_decl, font_fallback =
    var "font-fallback" Font_family ~fallback:(Fallback fallback_stack)
      sans_stack
  in
  let sheet_for decl =
    v [ rule ~selector:(Selector.class_ "font-sans") [ font_family decl ] ]
  in
  let empty_theme = Css.Pp.String_set.empty in
  let font_theme = Css.Pp.String_set.add "font-sans" empty_theme in
  let fallback_theme = Css.Pp.String_set.add "font-fallback" empty_theme in
  let resolve_font = function
    | "font-sans" -> Some "Arial,sans-serif"
    | "font-fallback" -> Some "Arial,sans-serif"
    | _ -> None
  in
  let render_theme ?theme ?theme_defaults sheet =
    sheet |> Css.resolve_theme ?theme ?theme_defaults |> to_string ~minify:true
  in
  Alcotest.(check string)
    "kept theme var keeps its reference and gains a :root default"
    ":root{--font-sans:Arial,sans-serif}.font-sans{font-family:var(--font-sans)}"
    (render_theme ~theme:font_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_sans)));
  Alcotest.(check string)
    "kept theme var with fallback gains a :root default"
    ":root{--font-fallback:Arial,sans-serif}.font-sans{font-family:var(--font-fallback,Arial,sans-serif)}"
    (render_theme ~theme:fallback_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_fallback)));
  Alcotest.(check string)
    "theme default emitted in :root without an explicit theme"
    ":root{--font-sans:Arial,sans-serif}.font-sans{font-family:var(--font-sans)}"
    (render_theme ~theme_defaults:resolve_font (sheet_for (Var font_sans)));
  Alcotest.(check string)
    "inline stylesheet may use typed default"
    ".font-sans{font-family:ui-sans-serif,system-ui,sans-serif}"
    (sheet_for (Var font_sans) |> Css.inline_vars |> to_string ~minify:true);
  Alcotest.(check string)
    "inline stylesheet may use typed fallback default"
    ".font-sans{font-family:ui-sans-serif,system-ui,sans-serif}"
    (sheet_for (Var font_fallback) |> Css.inline_vars |> to_string ~minify:true);
  Alcotest.(check string)
    "theme_defaults still resolves non-theme vars"
    ".font-sans{font-family:Arial,sans-serif}"
    (render_theme ~theme:empty_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_sans)));
  Alcotest.(check string)
    "theme_defaults still resolves non-theme vars with fallback"
    ".font-sans{font-family:Arial,sans-serif}"
    (render_theme ~theme:empty_theme ~theme_defaults:resolve_font
       (sheet_for (Var font_fallback)));
  (* A runtime channel var ([--tw-duration]) keeps its live [var()] reference,
     while a theme default reachable only through its fallback
     ([--default-transition-duration]) is inlined - the theme-inline transition
     shape. The kept wrapper must survive even though its fallback is a concrete
     duration. *)
  let tw_duration : duration var =
    var_ref ~fallback:(Var_fallback "default-transition-duration") "tw-duration"
  in
  let resolve_duration = function
    | "default-transition-duration" -> Some ".1s"
    | _ -> None
  in
  Alcotest.(check string)
    "kept var keeps its wrapper while the nested theme default inlines"
    ".transition{transition-duration:var(--tw-duration,.1s)}"
    (render_theme ~theme:empty_theme ~theme_defaults:resolve_duration
       (v
          [
            rule
              ~selector:(Selector.class_ "transition")
              [ transition_duration (Var tw_duration) ];
          ]))

(* The value parsers the [list-style] shorthand is built from, exposed so a
   caller can read a single [list-style-type] / [list-style-image]. *)
let list_style_value_parsers () =
  let ok what = function
    | Some _ -> ()
    | None -> Alcotest.failf "%s should parse" what
  in
  ok "square" (Css.parse_list_style_type "square");
  ok "upper-roman" (Css.parse_list_style_type "upper-roman");
  ok "url()" (Css.parse_list_style_image "url(/carrot.png)");
  ok "none" (Css.parse_list_style_image "none");
  (* CSS Lists 3 section 3.4 falls back to decimal if a named style is absent;
     the name is still valid at parse time. *)
  ok "nonsense-style" (Css.parse_list_style_type "nonsense-style")

(* [font-family] takes a stack, and a token defined as one is what a theme feeds
   back in, so a var() among the entries has to survive the read. *)
let font_family_value_parser () =
  let roundtrip s =
    match Css.parse_font_family s with
    | None -> Alcotest.failf "%s should parse" s
    | Some v ->
        Alcotest.(check string)
          s s
          (Pp.to_string Css.Properties.pp_font_family v)
  in
  roundtrip "Georgia, serif";
  roundtrip "ui-sans-serif";
  roundtrip "var(--font-source-sans-pro), system-ui";
  roundtrip "var(--font-ubuntu-mono)"

(* The legacy option-returning value parsers all accept one complete value and
   turn malformed or trailing input into [None], without leaking parser
   exceptions. Keep that shared public contract while their cursor plumbing is
   consolidated. *)
let option_value_parser_contracts () =
  let parsers =
    [
      ( "length",
        (fun s -> Option.is_some (Css.parse_length s)),
        "1px",
        "red",
        "1px red" );
      ( "color",
        (fun s -> Option.is_some (Css.parse_color s)),
        "red",
        "not-a-color",
        "red junk" );
      ( "shadow",
        (fun s -> Option.is_some (Css.parse_shadow s)),
        "0 1px 2px #000",
        "not-a-shadow",
        "0 1px 2px #000 junk" );
      ( "background-image",
        (fun s -> Option.is_some (Css.parse_background_image s)),
        "url(a.png)",
        "red",
        "url(a.png) junk" );
      ( "font-family",
        (fun s -> Option.is_some (Css.parse_font_family s)),
        "Inter, sans-serif",
        ",",
        "Inter, sans-serif !" );
      ( "list-style-type",
        (fun s -> Option.is_some (Css.parse_list_style_type s)),
        "square",
        "default",
        "square junk" );
      ( "list-style-image",
        (fun s -> Option.is_some (Css.parse_list_style_image s)),
        "none",
        "red",
        "none junk" );
    ]
  in
  List.iter
    (fun (name, accepts, valid, malformed, trailing) ->
      Alcotest.(check bool) (name ^ " complete value") true (accepts valid);
      Alcotest.(check bool)
        (name ^ " malformed value")
        false (accepts malformed);
      Alcotest.(check bool) (name ^ " trailing input") false (accepts trailing))
    parsers

let suite =
  ( "css_values",
    [
      Alcotest.test_case "list-style value parsers" `Quick
        list_style_value_parsers;
      Alcotest.test_case "font-family value parser" `Quick
        font_family_value_parser;
      Alcotest.test_case "option value parser contracts" `Quick
        option_value_parser_contracts;
      Alcotest.test_case "public property introspection" `Quick
        public_property_edges;
      Alcotest.test_case "public theme guards" `Quick public_theme_edges;
      Alcotest.test_case "public theme guards in declaration at-rules" `Quick
        public_theme_guards_in_declaration_at_rules;
      Alcotest.test_case "public value combinators" `Quick
        public_value_combinator_edges;
      Alcotest.test_case "public theme var rendering" `Quick
        public_theme_var_rendering_edges;
    ] )
