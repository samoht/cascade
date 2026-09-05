(** Fuzz tests for the CSS Stylesheet module.

    Tests crash safety of stylesheet, rule, and declaration parsing. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let css_ident buf i =
  pick
    [ "card"; "button"; "title"; "panel"; "layout"; "theme"; "accent"; "item" ]
    buf i

let css_selector_text buf i =
  pick
    [
      "." ^ css_ident buf (i + 1);
      "#" ^ css_ident buf (i + 1);
      "button";
      ":root";
      "." ^ css_ident buf (i + 1) ^ " > ." ^ css_ident buf (i + 2);
      "." ^ css_ident buf (i + 1) ^ ":is(:hover,:focus-visible)";
      "." ^ css_ident buf (i + 1) ^ ":has(> img)";
      ".grid > *";
    ]
    buf i

let css_declaration_text buf i =
  pick
    [
      "color:red";
      "color:var(--fg,#000)";
      "background-color:rgb(255 0 0 / .5)";
      "display:grid";
      "margin:1px 2px 3px 4px";
      "padding:clamp(1rem,2vw,2rem)";
      "width:calc(100% - 2rem)";
      "font:italic 700 1rem/1.5 \"Brand\",sans-serif";
      "--fg:oklch(50% .2 30)";
      "--" ^ css_ident buf (i + 1) ^ ":[a,b] (c) { d:e }";
    ]
    buf i

let css_invalid_declaration_text buf i =
  pick
    [
      "color red";
      ":red";
      "color:rgb(255 0)";
      "width:calc(1px + )";
      "margin:1px 2px 3px 4px 5px";
      "animation-duration:-1s";
      "font-family:Brand,";
      "color:red !important !important";
    ]
    buf i

let css_rule_text buf i =
  let selector = css_selector_text buf i in
  let first = css_declaration_text buf (i + 4) in
  let second = css_declaration_text buf (i + 8) in
  selector ^ "{" ^ first ^ ";" ^ second ^ "}"

let css_invalid_rule_text buf i =
  pick
    [
      css_selector_text buf i ^ "{"
      ^ css_invalid_declaration_text buf (i + 4)
      ^ ";color:red}";
      ".bad:not(){color:red}";
      ".bad:nth-child(foo){color:red}";
      ".a, :future-pseudo{color:red}";
      css_selector_text buf i ^ "}";
      "{" ^ css_declaration_text buf (i + 4) ^ "}";
    ]
    buf i

let css_valid_at_rule_text buf i =
  let rule = css_rule_text buf (i + 8) in
  pick
    [
      "@media (width >= 30em){" ^ rule ^ "}";
      "@supports (display:grid){" ^ rule ^ "}";
      "@container card (inline-size > 30em){" ^ rule ^ "}";
      "@layer " ^ css_ident buf (i + 1) ^ "{" ^ rule ^ "}";
      "@scope (.card) to (.footer){" ^ rule ^ "}";
      "@starting-style{" ^ rule ^ "}";
      "@font-face{font-family:Brand;src:url(brand.woff2);unicode-range:U+4??}";
      "@property --"
      ^ css_ident buf (i + 1)
      ^ "{syntax:\"<length>\";inherits:false;initial-value:1px}";
      "@keyframes fade{0%{opacity:0}to{opacity:1}}";
    ]
    buf i

let css_invalid_at_rule_text buf i =
  pick
    [
      "@media screen{@import url(inner.css);}";
      "@media screen and{" ^ css_rule_text buf (i + 4) ^ "}";
      "@supports selector(){" ^ css_rule_text buf (i + 4) ^ "}";
      "@container style(){" ^ css_rule_text buf (i + 4) ^ "}";
      "@scope (){" ^ css_rule_text buf (i + 4) ^ "}";
      "@font-face{src:url(brand.woff2)}";
      "@property --"
      ^ css_ident buf (i + 1)
      ^ "{syntax:\"<angle>\";inherits:false;initial-value:0}";
      "@keyframes none{to{opacity:1}}";
      "@import url(theme.css){.x{color:red}}";
    ]
    buf i

let css_known_valid_snippet_text buf i =
  pick
    [
      "@property --gap { syntax: \"<length>\"; inherits: false; initial-value: \
       1px }";
      "@property --tokens { inherits: true; initial-value: red; syntax: \
       \"<color>\" }";
      "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
       format(\"woff2\"); font-display: swap; unicode-range: U+0025-00FF; }";
      "@page invoice:first { size: A4; margin: 1cm; @top-left { content: \
       \"Invoice\" } }";
      "@keyframes fade { from { opacity: 0 } 50%, 100% { opacity: 1 } }";
      "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
       override-colors: 0 red, 1 color(display-p3 none 0.5 1); }";
      "@view-transition { navigation: auto; }";
      "@position-try --below { top: anchor(bottom); left: anchor(center); }";
      "@container card (inline-size > 30em) { .item { display: grid } }";
      "@container style(--variant: featured) { .item { display: grid } }";
      "@scope (.card) to (.footer) { .title { color: red } }";
      "@starting-style { .dialog { opacity: 0; translate: 0 1rem } }";
      "@charset \"UTF-8\"; @layer reset, theme; @import url(theme.css) \
       layer(theme); @namespace url(http://www.w3.org/1999/xhtml);";
      "@import url(base.css) screen and (width >= 40em); @namespace svg \
       url(http://www.w3.org/2000/svg); .icon { fill: currentColor }";
    ]
    buf i

let css_like_statement buf i =
  pick
    [
      css_rule_text buf i;
      css_known_valid_snippet_text buf i;
      css_valid_at_rule_text buf i;
      css_invalid_rule_text buf i;
      css_invalid_at_rule_text buf i;
      "@layer reset,theme;";
      "@import url(theme.css) layer(theme);";
      "@namespace svg url(http://www.w3.org/2000/svg);";
      "@unknown-rule{" ^ css_rule_text buf (i + 4) ^ "}";
    ]
    buf i

let css_like_stylesheet buf =
  let count = 1 + (byte_at buf 0 mod 4) in
  let rec loop n i acc =
    if n = 0 then String.concat "" (List.rev acc)
    else loop (n - 1) (i + 13) (css_like_statement buf i :: acc)
  in
  loop count 1 []

let segment buf i =
  pick
    [
      "reset";
      "base";
      "framework";
      "theme";
      "layout";
      "components";
      "utilities";
      "overrides";
    ]
    buf i

let layer_name buf i =
  match byte_at buf i mod 4 with
  | 0 -> [ segment buf (i + 1) ]
  | 1 -> [ segment buf (i + 1); segment buf (i + 2) ]
  | 2 -> [ segment buf (i + 1); segment buf (i + 2); segment buf (i + 3) ]
  | _ -> [ "framework"; "theme" ]

let selector buf i =
  Css.Selector.class_ (pick [ "card"; "title"; "button"; "inside" ] buf i)

let declaration buf i =
  match byte_at buf i mod 4 with
  | 0 -> Css.Declaration.display Css.Properties.Block
  | 1 -> Css.Declaration.display Css.Properties.Flex
  | 2 -> Css.Declaration.color (Css.Values.hex "#ff0000")
  | _ -> Css.Declaration.background_color (Css.Values.hex "#0000ff")

let rule buf i = Css.rule ~selector:(selector buf i) [ declaration buf (i + 1) ]

let import_rule ?layer url =
  Css.Stylesheet.Import { url; layer; supports = None; media = None }

let generated_layer_stylesheet buf =
  let primary = layer_name buf 0 in
  let secondary = layer_name buf 4 in
  let nested = layer_name buf 8 in
  [
    Css.Stylesheet.Layer_decl [ primary; secondary ];
    import_rule ~layer:secondary "theme.css";
    import_rule ~layer:[] "anonymous-layer.css";
    import_rule "plain.css";
    Css.Stylesheet.Layer
      ( Some primary,
        [
          rule buf 12;
          Css.Stylesheet.Media
            ( Css.Media.of_string "(min-width:30em)",
              [ Css.Stylesheet.Layer (Some nested, [ rule buf 16 ]) ] );
        ] );
    Css.Stylesheet.Layer
      ( None,
        [
          Css.Stylesheet.Layer (Some [ "foo" ], [ rule buf 20 ]);
          Css.Stylesheet.Layer (Some [ "foo" ], [ rule buf 24 ]);
        ] );
    Css.Stylesheet.Layer
      (None, [ Css.Stylesheet.Layer (Some [ "foo" ], [ rule buf 28 ]) ]);
    rule buf 32;
  ]

let generated_media_condition buf =
  pick
    [
      Css.Media.of_string "(width >= 40em)";
      Css.Media.of_string "(30em <= width < 60em)";
      Css.Media.of_string "(dynamic-range: high)";
      Css.Media.of_string "(prefers-reduced-motion: reduce)";
    ]
    buf 0

let generated_supports_condition buf =
  pick
    [
      Css.Supports.property "display" "grid";
      Css.Supports.func "selector" ":has(img)";
      Css.Supports.func "font-tech" "variations";
      Css.Supports.And
        ( Css.Supports.property "display" "grid",
          Css.Supports.func "selector" ":has(img)" );
    ]
    buf 4

let generated_container_condition buf =
  pick
    [
      Css.Container.of_string "(inline-size > 30em)";
      Css.Container.of_string "(30em <= inline-size < 60em)";
      Css.Container.of_string "style(--variant: featured)";
      Css.Container.of_string "scroll-state(stuck: top)";
    ]
    buf 8

let nested_condition_block buf media supports container =
  [
    Css.Stylesheet.Media
      ( media,
        [
          Css.Stylesheet.Supports
            ( supports,
              [
                Css.container ~name:"card" ~condition:container [ rule buf 12 ];
              ] );
        ] );
    Css.Stylesheet.When
      ( Css.Stylesheet.And
          ( Css.Stylesheet.Media_condition media,
            Css.Stylesheet.Supports_condition_test
              (Css.Supports.property "display" "grid") ),
        [ rule buf 13 ] );
    Css.Stylesheet.Else
      ( Some
          (Css.Stylesheet.Media_condition
             (Css.Media.of_string "(pointer: fine)")),
        [ rule buf 14 ] );
    Css.Stylesheet.Supports_condition
      ("--generated-condition", [ declaration buf 15 ]);
    Css.Stylesheet.Scope
      ( Some (Css.Selector.of_string ".card"),
        Some (Css.Selector.of_string ".boundary"),
        [ Css.Stylesheet.Starting_style [ rule buf 16 ] ] );
  ]

let generated_condition_stylesheet buf =
  let media = generated_media_condition buf in
  let supports = generated_supports_condition buf in
  let container = generated_container_condition buf in
  nested_condition_block buf media supports container

let parse_stylesheet input =
  let r = Cursor.of_string input in
  try Some (Css.Stylesheet.read r) with Cursor.Parse_error _ -> None

let parse_declaration input =
  let r = Cursor.of_string input in
  try
    match Css.Declaration.read_declaration r with
    | None -> None
    | Some decl -> Some (Css.Declaration.to_string ~minify:true decl)
  with Cursor.Parse_error _ | Error.Parse_error _ -> None

let minified_stylesheet ss =
  Css.Stylesheet.to_string ~minify:true ss |> String.trim

let pretty_stylesheet ss =
  Css.Stylesheet.to_string ~minify:false ss |> String.trim

let recovered_css label input =
  match Css.of_string ~strict:false input with
  | Ok parsed -> parsed
  | Error err ->
      failf "%s did not recover leniently: %s" label
        (Cascade.Error.to_string err)

let assert_strict_rejects_lenient_warns label input =
  match Css.of_string ~strict:true input with
  | Ok parsed ->
      failf "%s parsed strictly as invalid CSS: %S -> %S" label input
        (Css.to_string ~minify:true parsed.stylesheet)
  | Error _ ->
      let { Css.stylesheet; warnings; _ } = recovered_css label input in
      ignore (Css.to_string ~minify:true stylesheet : string);
      if warnings = [] then
        failf "%s recovered without a lenient warning: %S" label input

let assert_strict_accepts_cleanly label input =
  match Css.of_string ~strict:true input with
  | Error _ -> failf "%s rejected valid CSS strictly: %S" label input
  | Ok parsed ->
      let strict_output = Css.to_string ~minify:true parsed.stylesheet in
      let { Css.stylesheet; warnings; _ } = recovered_css label input in
      if warnings <> [] then
        failf "%s emitted lenient warnings for valid CSS: %S" label input;
      let lenient_output = Css.to_string ~minify:true stylesheet in
      if strict_output <> lenient_output then
        failf "%s strict/lenient output changed: %S -> %S / %S" label input
          strict_output lenient_output

let assert_invalid_declaration_contract label input =
  assert_strict_rejects_lenient_warns label (".x{" ^ input ^ "}")

let recovered_declaration_counts stylesheet =
  Css.rule_statements stylesheet
  |> List.map (fun statement ->
      match Css.as_rule statement with
      | Some (_, declarations, _) -> List.length declarations
      | None -> fail "recovery returned a non-rule in rule_statements")

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let css_wide_keyword buf i =
  pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf i

let shorthand_property buf i =
  pick [ "margin"; "padding"; "background"; "border"; "font"; "all" ] buf i

let invalid_shorthand_keyword_mix buf i =
  let keyword = css_wide_keyword buf i in
  pick
    [
      ("margin", keyword ^ " 1px");
      ("padding", "1px " ^ keyword);
      ("background", "green " ^ keyword);
      ("border", "1px solid " ^ keyword);
      ("font", "bold " ^ keyword ^ " 12pt Helvetica");
      ("all", keyword ^ " color");
    ]
    buf (i + 1)

let platform_declaration_vector buf i =
  pick
    [
      ("color", "color:light-dark(black,white)");
      ("color", "color:rgb(from rebeccapurple r g b)");
      ( "background-image",
        "background-image:image-set(url(a.png) 1x,url(a@2x.png) 2x)" );
      ("width", "width:stretch");
      ("height", "height:contain");
      ("anchor-name", "anchor-name:--tooltip");
      ("position-anchor", "position-anchor:--tooltip");
      ("grid-template-columns", "grid-template-columns:subgrid");
      ("grid-template-rows", "grid-template-rows:masonry");
      ("shape-outside", "shape-outside:circle(50%)");
      ("overflow-clip-margin", "overflow-clip-margin:1px");
      ("scrollbar-width", "scrollbar-width:thin");
      ("scrollbar-color", "scrollbar-color:red blue");
      ("scrollbar-gutter", "scrollbar-gutter:stable both-edges");
      ("font-palette", "font-palette:--brand");
      ("text-wrap-style", "text-wrap-style:pretty");
      ("writing-mode", "writing-mode:sideways-rl");
      ("animation-timeline", "animation-timeline:scroll()");
      ("transition-behavior", "transition-behavior:allow-discrete");
      ("view-transition-name", "view-transition-name:card");
    ]
    buf i

let invalid_platform_declaration_vector buf i =
  pick
    [
      "color:light-dark(black)";
      "background-image:image-set()";
      "display:block solid";
      "position:sticky absolute";
      "anchor-name:tooltip";
      "position-anchor:tooltip";
      "font-weight:0";
      "line-height:-1";
      "z-index:1px";
      "translate:10";
      "filter:blur(red)";
      "grid-template-areas:\"a\" \"a b\"";
      "shape-margin:-1px";
      "overflow-clip-margin:-1px";
      "scrollbar-width:wide";
      "scrollbar-gutter:stable auto";
      "font-palette:1";
      "text-wrap-style:loud";
      "animation-range:exit nope";
      "view-transition-name:none card";
    ]
    buf i

(* A feature query that every target browser satisfies is always true, so its
   @supports wrapper imposes no condition and the default optimizer may drop it.
   display:grid and display:flex are supported by every maintained evergreen
   browser (flexbox since ~2015, grid since 2017), making those queries
   unconditionally true on the target. This oracle encodes that browser fact
   directly - independent of the code under test - and admits a dropped wrapper
   only for those two conditions. (--enforce-spec keeps the wrapper, but these
   invariants exercise the default optimizer.) *)
let supports_is_baseline_true condition =
  Css.Supports.equal condition (Css.Supports.property "display" "grid")
  || Css.Supports.equal condition (Css.Supports.property "display" "flex")

let rec boundary_shape = function
  | Css.Stylesheet.Rule _ -> [ "rule" ]
  | Declarations _ -> [ "declarations" ]
  | Import { layer; _ } ->
      [
        "import:"
        ^ Option.fold ~none:"<none>" ~some:Css.Stylesheet.string_of_layer_name
            layer;
      ]
  | Namespace _ -> [ "namespace" ]
  | Layer_decl names ->
      (* Naming an already-declared layer adds nothing - layer order follows the
         first occurrence of each name - so the optimizer drops later repeats.
         Compare the order-preserving deduplicated names. *)
      let rec dedup seen = function
        | [] -> []
        | n :: rest when List.mem n seen -> dedup seen rest
        | n :: rest -> n :: dedup (n :: seen) rest
      in
      [
        "layer-decl:"
        ^ String.concat ","
            (List.map Css.Stylesheet.string_of_layer_name (dedup [] names));
      ]
  | Layer (name, block) ->
      let name =
        Option.fold ~none:"<anonymous>"
          ~some:Css.Stylesheet.string_of_layer_name name
      in
      ("layer:" ^ name)
      :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block
      @ [ "/layer" ]
  | Media (_, block) ->
      ("media" :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block)
      @ [ "/media" ]
  | Supports (condition, block) when supports_is_baseline_true condition ->
      (* The wrapper is unconditionally true on the target and the default
         optimizer drops it, so the surviving shape is the block contents with
         no supports markers. *)
      Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block
  | Supports (_, block) ->
      ("supports" :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block)
      @ [ "/supports" ]
  | Container (_, _, block) ->
      ("container" :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block)
      @ [ "/container" ]
  | When (_, block) ->
      ("when" :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block)
      @ [ "/when" ]
  | Else (_, block) ->
      ("else" :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block)
      @ [ "/else" ]
  | Supports_condition (name, _) -> [ "supports-condition:" ^ name ]
  | Scope (_, _, block) ->
      ("scope" :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block)
      @ [ "/scope" ]
  | Starting_style block ->
      "starting-style"
      :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block
      @ [ "/starting-style" ]
  | Origin (origin, block) ->
      let origin =
        match origin with
        | User_agent -> "ua"
        | User -> "user"
        | Author_presentational_hint -> "author-presentational-hint"
        | Author -> "author"
        | Animation -> "animation"
        | Transition -> "transition"
      in
      ("origin:" ^ origin)
      :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block
      @ [ "/origin" ]
  | Moz_document (_, block) ->
      "moz-document"
      :: Fuzz_helpers.shapes_with_rule_runs ~boundary_shape block
      @ [ "/moz-document" ]
  | Charset _ -> [ "charset" ]
  | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ -> [ "keyframes" ]
  | Font_face _ -> [ "font-face" ]
  | Counter_style _ -> [ "counter-style" ]
  | Page _ -> [ "page" ]
  | Page_with_margins _ -> [ "page" ]
  | Font_palette_values _ -> [ "font-palette-values" ]
  | Font_feature_values _ -> [ "font-feature-values" ]
  | View_transition _ -> [ "view-transition" ]
  | Position_try _ -> [ "position-try" ]
  | Viewport _ -> [ "viewport" ]
  | Unknown_at_rule { name; _ } -> [ "unknown-at-rule:" ^ name ]
  | Property _ -> [ "property" ]
  | Bang_comment _ -> [ "bang-comment" ]

(* CSS Cascade 5 section 6.4.1: a layer's contents are the concatenation, in
   source order, of every block naming it, and layer order is fixed by first
   appearance. Two ADJACENT same-name layer blocks may therefore be fused into
   one without changing the cascade (Lightning CSS does exactly this). Normalise
   both sides of the invariant by that fusion so the check tracks real boundary
   corruption rather than a legal layer merge. Anonymous layers each have a
   distinct identity and are never merged - the anonymous-layer-count invariant
   guards that separately. *)
let rec merge_adjacent_layers = function
  | Css.Stylesheet.Layer (Some a, ba)
    :: Css.Stylesheet.Layer (Some b, bb)
    :: rest
    when Css.Stylesheet.equal_layer_name a b ->
      merge_adjacent_layers (Css.Stylesheet.Layer (Some a, ba @ bb) :: rest)
  | stmt :: rest -> normalize_blocks stmt :: merge_adjacent_layers rest
  | [] -> []

and normalize_blocks = function
  | Css.Stylesheet.Layer (name, block) ->
      Css.Stylesheet.Layer (name, merge_adjacent_layers block)
  | Media (c, block) -> Media (c, merge_adjacent_layers block)
  | Supports (c, block) -> Supports (c, merge_adjacent_layers block)
  | Container (n, c, block) -> Container (n, c, merge_adjacent_layers block)
  | When (c, block) -> When (c, merge_adjacent_layers block)
  | Else (c, block) -> Else (c, merge_adjacent_layers block)
  | Scope (a, b, block) -> Scope (a, b, merge_adjacent_layers block)
  | Starting_style block -> Starting_style (merge_adjacent_layers block)
  | Origin (o, block) -> Origin (o, merge_adjacent_layers block)
  | Moz_document (m, block) -> Moz_document (m, merge_adjacent_layers block)
  | other -> other

let boundary_shapes ss =
  Fuzz_helpers.shapes_with_rule_runs ~boundary_shape (merge_adjacent_layers ss)

let anonymous_layer_count ss =
  let rec statement = function
    | Css.Stylesheet.Layer (None, block) -> 1 + block_count block
    | Layer (Some _, block)
    | Media (_, block)
    | Supports (_, block)
    | Container (_, _, block)
    | When (_, block)
    | Else (_, block)
    | Scope (_, _, block)
    | Starting_style block
    | Moz_document (_, block)
    | Origin (_, block) ->
        block_count block
    | Rule _ | Declarations _ | Charset _ | Import _ | Namespace _
    | Layer_decl _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
    | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
    | Font_palette_values _ | Font_feature_values _ | View_transition _
    | Position_try _ | Property _ | Supports_condition _ | Viewport _
    | Unknown_at_rule _ | Bang_comment _ ->
        0
  and block_count block = List.fold_left (fun n s -> n + statement s) 0 block in
  block_count ss

(** [Stylesheet.read] must not crash on arbitrary input. *)
let test_read_stylesheet buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Stylesheet.read r) with Cursor.Parse_error _ -> ()

(* The same property over byte shapes an ASCII alphabet cannot reach. *)
let test_stylesheet_unicode_bytes buf =
  let r = Cursor.of_string (Fuzz_helpers.unicodish buf) in
  try ignore (Css.Stylesheet.read r) with Cursor.Parse_error _ -> ()

(** read_rule -- must not crash. *)
let test_read_rule buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_rule r)
  with Cursor.Parse_error _ | Invalid_argument _ -> ()

(** read_block -- must not crash. *)
let test_read_block buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_block r) with Cursor.Parse_error _ -> ()

(** read (legacy) -- must not crash. *)
let test_read buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Stylesheet.read r) with Cursor.Parse_error _ -> ()

(** read_import_rule -- must not crash. *)
let test_read_import_rule buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Stylesheet.read_import_rule r)
  with Cursor.Parse_error _ -> ()

(** read_declaration -- must not crash. *)
let test_read_declaration buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Declaration.read_declaration r)
  with Cursor.Parse_error _ -> ()

(** read_declarations -- must not crash. *)
let test_read_declarations buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Declaration.read_declarations r)
  with Cursor.Parse_error _ -> ()

(** read_property_name -- must not crash. *)
let test_read_property_name buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Declaration.read_property_name r)
  with Cursor.Parse_error _ -> ()

(** read_property_value -- must not crash. *)
let test_read_property_value buf =
  let r = Cursor.of_string buf in
  try ignore (Css.Declaration.read_property_value r)
  with Cursor.Parse_error _ -> ()

(** Stylesheet roundtrip: parse -> to_string -> parse should not crash. *)
let test_stylesheet_roundtrip buf =
  let r = Cursor.of_string buf in
  match
    try Some (Css.Stylesheet.read r) with Cursor.Parse_error _ -> None
  with
  | None -> ()
  | Some ss -> (
      let s = Css.Stylesheet.to_string ss in
      let r2 = Cursor.of_string s in
      try ignore (Css.Stylesheet.read r2)
      with Cursor.Parse_error _ -> fail "stylesheet roundtrip re-parse failed")

(* Allow one canonicalization pass (numeric trim, [1e3] -> [1000], escape
   canonical form, NUL -> U+FFFD, ...) that only fires on re-parse, then require
   fixed point. Serializer output must always reparse. *)
let assert_stylesheet_idempotent ~label serialize buf =
  let reparse_or_fail step s =
    match parse_stylesheet s with
    | Some ss -> ss
    | None -> failf "%s stylesheet did not reparse at %s: %S" label step s
  in
  match parse_stylesheet buf with
  | None -> ()
  | Some ss ->
      let once = serialize ss in
      let twice = serialize (reparse_or_fail "first reparse" once) in
      let thrice = serialize (reparse_or_fail "second reparse" twice) in
      if twice <> thrice then
        failf
          "stylesheet %s serialization drifted past canonicalization: %S -> %S \
           -> %S"
          label once twice thrice

(** Minified stylesheet serialization should reach a fixed point after at most
    one canonicalization pass on reparsed CSS-shaped input. *)
let test_stylesheet_minified_idempotent buf =
  assert_stylesheet_idempotent ~label:"minified" minified_stylesheet
    (cssish buf)

let test_stylesheet_pretty_idempotent buf =
  assert_stylesheet_idempotent ~label:"pretty" pretty_stylesheet (cssish buf)

(** CSS Cascade section 6.4: stylesheets emitted with layer statements, nested
    layer names, imports, anonymous layers, and conditional layer blocks should
    parse back to the same canonical serialization. *)
let test_generated_layer_stylesheet_roundtrip buf =
  let ss = generated_layer_stylesheet buf in
  let once = minified_stylesheet ss in
  match parse_stylesheet once with
  | None -> failf "generated layer stylesheet did not reparse: %S" once
  | Some reparsed ->
      let twice = minified_stylesheet reparsed in
      if once <> twice then
        failf "generated layer stylesheet serialization changed: %S -> %S" once
          twice

let test_generated_condition_stylesheet_roundtrip buf =
  let ss = generated_condition_stylesheet buf in
  let once = minified_stylesheet ss in
  match parse_stylesheet once with
  | None -> failf "generated conditional stylesheet did not reparse: %S" once
  | Some reparsed ->
      let twice = minified_stylesheet reparsed in
      if once <> twice then
        failf "generated conditional stylesheet serialization changed: %S -> %S"
          once twice

let test_generated_stylesheets_pretty_roundtrip buf =
  List.iter
    (fun ss ->
      let once = pretty_stylesheet ss in
      match parse_stylesheet once with
      | None -> failf "generated pretty stylesheet did not reparse: %S" once
      | Some reparsed ->
          let twice = pretty_stylesheet reparsed in
          if once <> twice then
            failf "generated pretty stylesheet serialization changed: %S -> %S"
              once twice)
    [ generated_layer_stylesheet buf; generated_condition_stylesheet buf ]

(** CSS Cascade section 6.4: optimization must preserve cascade boundaries that
    define layer identity, layer order, import placement, and conditional scope.
*)
let test_layer_boundary_shape_invariant buf =
  let ss = generated_layer_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = boundary_shapes ss in
  let after = boundary_shapes optimized in
  if before <> after then
    failf "layer boundary shape changed: %S -> %S" (String.concat " " before)
      (String.concat " " after)

(** CSS Cascade section 6.4.2.1: anonymous layer declarations have unique
    identities and must not be collapsed away by optimization. *)
let test_anonymous_layer_count_invariant buf =
  let ss = generated_layer_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = anonymous_layer_count ss in
  let after = anonymous_layer_count optimized in
  if before <> after then
    failf "anonymous layer count changed: %d -> %d" before after

let test_condition_boundary_shape_invariant buf =
  let ss = generated_condition_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = boundary_shapes ss in
  let after = boundary_shapes optimized in
  if before <> after then
    failf "conditional boundary shape changed: %S -> %S"
      (String.concat " " before) (String.concat " " after)

(** CSS Cascade section 3: CSS-wide keywords used on shorthands are whole
    declaration values and serialize as the shorthand property plus the keyword.
*)
let test_shorthand_wide_keyword buf =
  let property = shorthand_property buf 0 in
  let keyword = css_wide_keyword buf 1 in
  let input = property ^ ":" ^ keyword in
  match parse_declaration input with
  | None -> failf "CSS-wide shorthand did not parse: %S" input
  | Some serialized ->
      (* pp serializes a CSS-wide keyword verbatim. Resolving [initial] to a
         property's initial value (margin/padding:initial -> 0) needs the
         initial-value table, so it is a normalize fold in optimize, not pp. *)
      if serialized <> property ^ ":" ^ keyword then
        failf "CSS-wide shorthand changed: %S -> %S" input serialized

(** CSS Cascade section 3: CSS-wide keywords cannot be combined with other
    component values in a single declaration, including in shorthands. *)
let test_shorthand_wide_mix buf =
  let property, value = invalid_shorthand_keyword_mix buf 0 in
  let input = property ^ ":" ^ value in
  assert_invalid_declaration_contract "invalid shorthand keyword mix" input

(** CSS Cascade section 3.1: legacy shorthands are parse-time aliases and are
    not chosen when serializing declarations. *)
let test_legacy_alias_stable buf =
  let input, expected =
    pick
      [
        ("page-break-before:always", "break-before:page");
        ("page-break-after:always", "break-after:page");
        ("page-break-inside:avoid", "break-inside:avoid");
      ]
      buf 0
  in
  match parse_declaration input with
  | None -> failf "legacy shorthand alias did not parse: %S" input
  | Some serialized ->
      if starts_with ~prefix:"page-break-" serialized then
        failf "legacy shorthand serialized with old name: %S" serialized;
      if serialized <> expected then
        failf "legacy shorthand alias changed: %S -> %S" input serialized

(** CSS Cascade section 4.1: declared values preserve declaration source order
    before cascade sorting. *)
let test_declared_order_stable buf =
  let first_color =
    if byte_at buf 0 mod 2 = 0 then Css.Values.hex "#ff0000"
    else Css.Values.hex "#00ff00"
  in
  let second_color =
    if byte_at buf 1 mod 2 = 0 then Css.Values.hex "#0000ff"
    else Css.Values.hex "#ffff00"
  in
  let declarations =
    [
      Css.Declaration.color first_color;
      Css.Declaration.margin [ Css.Values.Px 1. ];
      Css.Declaration.important (Css.Declaration.color second_color);
    ]
  in
  let colors = Css.Stylesheet.declared_values ~property:"color" declarations in
  let orders =
    List.map (fun (d : Css.Stylesheet.declared_value) -> d.source_order) colors
  in
  let important =
    List.map (fun (d : Css.Stylesheet.declared_value) -> d.important) colors
  in
  if not (List.equal Int.equal orders [ 0; 2 ]) then
    failf "declared color source order changed: %s"
      (String.concat "," (List.map string_of_int orders));
  if important <> [ false; true ] then
    fail "declared value importance did not follow declarations"

(** CSS Cascade section 4.3: specified-value defaulting for [unset] depends on
    whether the property is inherited. *)
let test_unset_inheritance buf =
  let inherited = Some (pick [ "blue"; "inside"; "4.2px" ] buf 0) in
  let initial = pick [ "black"; "outside"; "medium" ] buf 1 in
  let inherited_property =
    Css.Stylesheet.value ~inherits:true ~initial ~inherited
      ~cascaded:(Some "unset")
  in
  let non_inherited_property =
    Css.Stylesheet.value ~inherits:false ~initial ~inherited
      ~cascaded:(Some "unset")
  in
  if inherited_property.value <> Option.get inherited then
    failf "unset inherited property did not inherit: %S"
      inherited_property.value;
  if non_inherited_property.value <> initial then
    failf "unset non-inherited property did not use initial: %S"
      non_inherited_property.value

(** CSS Cascade sections 4.2 and 6.1: after all higher-priority criteria tie,
    later source order determines the cascaded value. *)
let test_cascade_source_order buf =
  let first = pick [ "red"; "green"; "blue" ] buf 0 in
  let second = pick [ "cyan"; "magenta"; "yellow" ] buf 1 in
  let candidate source_order value : Css.Stylesheet.cascade_candidate =
    {
      origin = Css.Stylesheet.Author;
      layer = None;
      important = false;
      specificity = 10;
      scope_hops = None;
      source_order;
      value;
    }
  in
  match
    Css.Stylesheet.winning_cascade_candidate ~layer_order:[]
      [ candidate 0 first; candidate 1 second ]
  with
  | Some winner when winner.value = second -> ()
  | Some winner ->
      failf "later source-order candidate lost: %S vs %S" second winner.value
  | None -> fail "integrated cascade returned no winner"

(** CSS Cascade section 7.3.5 as used by section 4.3: [revert-layer] falls back
    to the winning lower layer, or defaulting if there is no lower layer. *)
let test_revert_layer_value buf =
  let fallback = pick [ "green"; "blue"; "black" ] buf 0 in
  let candidate layer source_order value :
      Css.Stylesheet.cascade_layer_candidate =
    { layer; important = false; source_order; value }
  in
  let specified =
    Css.Stylesheet.specified_value_after_revert_layer ~inherits:false
      ~initial:"transparent" ~inherited:None ~layer_order:[ "base"; "theme" ]
      [
        candidate (Some "base") 0 fallback;
        candidate (Some "theme") 1 "revert-layer";
      ]
  in
  if specified.value <> fallback then
    failf "revert-layer did not expose lower layer: %S" specified.value

let test_platform_decl_name buf =
  let property, input = platform_declaration_vector buf 0 in
  match parse_declaration input with
  | None -> ()
  | Some serialized ->
      let prefix = property ^ ":" in
      if not (starts_with ~prefix serialized) then
        failf "accepted platform declaration changed property: %S -> %S" input
          serialized

let test_invalid_platform_declaration_rejected buf =
  let input = invalid_platform_declaration_vector buf 0 in
  assert_invalid_declaration_contract "invalid platform declaration" input

let test_import_url_syntax buf =
  let url =
    pick
      [
        "theme.css";
        "../fonts/brand.woff2";
        "#paint";
        "data:image/svg+xml,%3Csvg%3E";
        "//cdn.example/reset.css";
      ]
      buf 0
  in
  let import =
    {
      Css.Stylesheet.url;
      layer = Some (layer_name buf 1);
      supports = Some (Css.Supports.func "selector" ":has(img)");
      media = Some (Css.Media.of_string "(width >= 40em)");
    }
  in
  let ss = [ Css.Stylesheet.Import import ] in
  let once = minified_stylesheet ss in
  match parse_stylesheet once with
  | None -> failf "import/url syntax did not reparse: %S" once
  | Some reparsed ->
      let twice = minified_stylesheet reparsed in
      if once <> twice then
        failf "import/url syntax changed: %S -> %S" once twice

let test_namespace_prefix_separator buf =
  let prefix = pick [ "svg"; "math"; "html"; "foo" ] buf 0 in
  let url =
    pick
      [
        "http://www.w3.org/2000/svg";
        "http://www.w3.org/1998/Math/MathML";
        "http://www.w3.org/1999/xhtml";
      ]
      buf 1
  in
  let input = Fmt.str "@namespace %s url(%s);" prefix url in
  match parse_stylesheet input with
  | None -> failf "valid prefixed namespace did not parse: %S" input
  | Some ss -> (
      let output = minified_stylesheet ss in
      match parse_stylesheet output with
      | Some
          [
            Css.Stylesheet.Namespace
              (Some output_prefix, Css.Stylesheet.Url (u, _));
          ]
        when output_prefix = prefix && u = url ->
          ()
      | Some
          [
            Css.Stylesheet.Namespace
              (Some output_prefix, Css.Stylesheet.Quoted u);
          ]
        when output_prefix = prefix && u = url ->
          ()
      | Some reparsed ->
          failf
            "CSS Namespaces serialization changed namespace structure: %S -> \
             %S -> %S"
            input output
            (String.concat " " (boundary_shapes reparsed))
      | None ->
          failf "CSS Namespaces serialization emitted unparsable CSS: %S -> %S"
            input output)

let test_valid_atrule_descriptor buf =
  let input =
    pick
      [
        "@property --gap { syntax: \"<length>\"; inherits: false; \
         initial-value: 1px }";
        "@property --tokens { inherits: true; initial-value: red; syntax: \
         \"<color>\" }";
        "@property --angle-zero { syntax: \"<angle>\"; inherits: false; \
         initial-value: 0deg }";
        "@font-face { font-family: Brand; src: url(\"brand.woff2\") \
         format(\"woff2\"); font-display: swap; unicode-range: U+0025-00FF; }";
        "@font-face { font-family: Brand; src: local(\"Brand\"), \
         url(\"brand.woff2\") format(\"woff2\") tech(variations); font-weight: \
         100 900; font-style: oblique 10deg 20deg; }";
        "@font-face { font-family: Icons; src: url(icons.woff2); \
         unicode-range: U+4?? }";
        "@counter-style thumbs { system: cyclic; symbols: \"*\" \"x\"; suffix: \
         \" \" }";
        "@page invoice:first { size: A4; margin: 1cm; @top-left { content: \
         \"Invoice\" } }";
        "@page chapter:right { size: letter landscape; marks: crop cross; \
         @bottom-center { content: counter(page) } }";
        "@page :first { margin-left: 2cm }";
        "@keyframes fade { from { opacity: 0 } 50%, 100% { opacity: 1 } }";
        "@keyframes slide { 100% { translate: 10px 0 } 0% { translate: none } }";
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         override-colors: 0 red, 1 color(display-p3 none 0.5 1); }";
        "@view-transition { navigation: auto; }";
        "@position-try --below { top: anchor(bottom); left: anchor(center); }";
        "@container card (inline-size > 30em) { .item { display: grid } }";
        "@container sidebar (inline-size > 30em) { .item { display: grid } }";
        "@container style(--variant: featured) { .item { display: grid } }";
        "@container scroll-state(stuck: top) { .item { display: grid } }";
        "@scope (.card) to (.footer) { .title { color: red } }";
        "@starting-style { .dialog { opacity: 0; translate: 0 1rem } }";
        "@layer { .anonymous { color: red } }";
        "@layer framework { @layer theme { .title { color: red } } }";
        "@media (400px <= width <= 1000px) { .x { color: red } }";
        "@supports ((display: grid) or (display: flex)) and (not (display: \
         subgrid)) { .x { display: grid } }";
      ]
      buf 0
  in
  assert_strict_accepts_cleanly "valid spec at-rule vector" input;
  match parse_stylesheet input with
  | None -> failf "valid spec at-rule vector did not parse: %S" input
  | Some ss ->
      let output = minified_stylesheet ss in
      if output = "" then failf "valid at-rule serialized empty: %S" input

let test_invalid_atrule_descriptor buf =
  let input =
    pick
      [
        "@property --gap { syntax: \"<length>\"; inherits: false }";
        "@property --angle { syntax: \"<angle>\"; inherits: false; \
         initial-value: 0 }";
        "@property --angle-list { syntax: \"<angle>#\"; inherits: false; \
         initial-value: 0 }";
        "@font-face { src: url(\"brand.woff2\"); }";
        "@font-face { font-family: Brand; src: format(\"woff2\"); }";
        "@font-face { font-family: Brand; src: url(font.woff2); unicode-range: \
         U+20-10 }";
        "@font-face { font-family: Brand; src: url(font.woff2); font-display: \
         block swap }";
        "@page :unknown { margin: 1cm }";
        "@page { @top-center { display: 1px } }";
        "@font-palette-values brand { font-family: Brand; base-palette: 1 }";
        "@font-palette-values --brand { override-colors: -1 red }";
        "@counter-style thumbs { system: cyclic }";
        "@view-transition { navigation: always; }";
        "@position-try default { top: 0; }";
        "@position-try --fallback { @media screen { .x { color: red } } }";
        "@namespace svg;";
        "@container style() { .x { color: red } }";
        "@container scroll-state() { .x { color: red } }";
        "@scope () { .x { color: red } }";
        "@scope (.x) to () { .x { color: red } }";
        "@starting-style;";
        "@media screen and { .x { color: red } }";
        "@supports not { .x { color: red } }";
        "@supports (display: grid) and (color: red) or (width: 1px) { .x { \
         color: red } }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid spec at-rule vector" input

let test_atrule_inventory_valid buf =
  let row = pick Cascade_spec_inventory.At_rule_grammar.positive buf 0 in
  match parse_stylesheet row.input with
  | None ->
      failf "shared at-rule inventory valid row rejected: %s/%s %S" row.feature
        row.branch row.input
  | Some ss -> (
      let output = minified_stylesheet ss in
      if output <> row.expected then
        failf "shared at-rule inventory serialization changed: %s/%s %S -> %S"
          row.feature row.branch row.input output;
      match parse_stylesheet output with
      | None ->
          failf "shared at-rule inventory serialization did not reparse: %S"
            output
      | Some reparsed ->
          let twice = minified_stylesheet reparsed in
          if twice <> output then
            failf
              "shared at-rule inventory serialization not idempotent: %S -> %S"
              output twice)

let test_atrule_inventory_invalid buf =
  let row = pick Cascade_spec_inventory.At_rule_grammar.negative buf 0 in
  assert_strict_rejects_lenient_warns
    (Fmt.str "shared at-rule inventory invalid row %s/%s" row.feature row.branch)
    row.input

let test_font_face_descriptor_matrix buf =
  let input =
    pick
      [
        "@font-face { font-family: Brand; src: local(\"Brand\"), \
         url(brand.woff2) format(\"woff2\"); font-weight: 100 900; }";
        "@font-face { font-family: Metrics; src: url(metrics.woff2); \
         size-adjust: 100%; ascent-override: normal; descent-override: 20%; \
         line-gap-override: 0%; }";
        "@font-face { font-family: TallMetrics; src: url(tall.woff2); \
         ascent-override: 120%; descent-override: 125%; line-gap-override: 0%; \
         }";
      ]
      buf 0
  in
  match parse_stylesheet input with
  | None -> failf "font-face descriptor vector rejected: %S" input
  | Some ss ->
      if minified_stylesheet ss = "" then
        fail "font-face vector serialized empty"

let test_invalid_font_face buf =
  let input =
    pick
      [
        "@font-face { font-family: Brand; src: url(brand.woff2); \
         ascent-override: -1%; }";
        "@font-face { font-family: Brand; src: url(brand.woff2); size-adjust: \
         normal; }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid font-face descriptor" input

let test_page_margin_descriptor_matrix buf =
  let input =
    pick
      [
        "@page chapter:right { size: letter landscape; margin: 1in; @right-top \
         { content: counter(page) } }";
        "@page :left { margin-left: 3cm; @left-middle { content: \
         string(chapter) } }";
      ]
      buf 0
  in
  match parse_stylesheet input with
  | None -> failf "page margin descriptor vector rejected: %S" input
  | Some ss ->
      if minified_stylesheet ss = "" then fail "page vector serialized empty"

let test_invalid_page_margin buf =
  let input =
    pick
      [
        "@page :unknown { margin: 1cm }";
        "@page { @top-center { display: 1px } }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid page descriptor" input

let test_palette_descriptor_matrix buf =
  let input =
    pick
      [
        "@font-palette-values --brand { font-family: Brand; base-palette: 1; \
         override-colors: 0 red, 1 color(display-p3 none 0.5 1); }";
        "@font-palette-values --dark { font-family: \"Color Font\", Brand; \
         base-palette: dark; }";
      ]
      buf 0
  in
  match parse_stylesheet input with
  | None -> failf "font-palette-values vector rejected: %S" input
  | Some ss ->
      if minified_stylesheet ss = "" then
        fail "font-palette-values vector serialized empty"

let test_invalid_palette_descriptor buf =
  let input =
    pick
      [
        "@font-palette-values brand { font-family: Brand; base-palette: 1 }";
        "@font-palette-values --brand;";
        "@font-palette-values --brand { font-family: Brand; override-colors: \
         -1 red }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid font-palette-values vector" input

let test_view_transition_descriptor_matrix buf =
  let input =
    pick
      [
        "@view-transition { navigation: auto; }";
        "@view-transition { navigation: none; }";
      ]
      buf 0
  in
  match parse_stylesheet input with
  | None -> failf "view-transition vector rejected: %S" input
  | Some ss ->
      if minified_stylesheet ss = "" then
        fail "view-transition vector serialized empty"

let test_invalid_view_transition buf =
  let input =
    pick
      [
        "@view-transition page { navigation: auto; }";
        "@view-transition;";
        "@view-transition { navigation: always; }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid view-transition vector" input

let test_position_try_descriptor_matrix buf =
  let input =
    pick
      [
        "@position-try --below { top: anchor(bottom); left: anchor(center); \
         width: anchor-size(width); }";
        "@position-try --inline-start { inset-inline-end: anchor(start); \
         margin-inline: 1rem; }";
      ]
      buf 0
  in
  match parse_stylesheet input with
  | None -> failf "position-try vector rejected: %S" input
  | Some ss ->
      if minified_stylesheet ss = "" then
        fail "position-try vector serialized empty"

let test_invalid_position_try buf =
  let input =
    pick
      [
        "@position-try default { top: 0; }";
        "@position-try --fallback;";
        "@position-try --fallback { @media screen { .x { color: red } } }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid position-try vector" input

let test_property_value_context buf =
  let open Css.Values in
  let name = pick [ "--a"; "--registered"; "--empty"; "--cycle" ] buf 0 in
  let specified =
    pick
      [
        "var(--missing,)";
        "var(--a, var(--b, red))";
        "var(--cycle)";
        "10px";
        "{ color: red }";
      ]
      buf 1
  in
  let custom_decl = Css.Declaration.of_string (name ^ ": " ^ specified) in
  let ctx =
    Css.Context.v ~custom_properties:[ custom_decl ]
      ~inherited_values:[ Css.Declaration.of_string "color: currentColor" ]
      ~initial_values:[ Css.Declaration.of_string "display: inline" ]
      ~base_url:"https://example.test/css/app.css" ~root_font_size:(Px 16.)
      ~parent_font_size:(Px 14.) ~current_color:(Named Black)
      ~viewport_width:(Px 1024.) ~viewport_height:(Px 768.)
      ~container_width:(Px 640.) ~container_height:(Px 480.) ()
  in
  if
    not
      (Option.equal Css.Declaration.equal_declaration
         (Css.Context.custom_property name ctx)
         (Some custom_decl))
  then fail "property-value context lost custom property"

let test_recovery_keeps_rules buf =
  let bad_decl =
    pick [ "color:rgb(300)"; "width:calc(1px + )"; "transform:rotate()" ] buf 0
  in
  let css = ".a{color:red}.b{" ^ bad_decl ^ "}.c{display:block}" in
  assert_strict_rejects_lenient_warns "recovery keeps sibling rules" css;
  let { Css.stylesheet; warnings; _ } =
    recovered_css "recovery keeps sibling rules" css
  in
  let counts = recovered_declaration_counts stylesheet in
  if not (List.equal Int.equal counts [ 1; 0; 1 ]) then
    failf "CSS Syntax recovery changed rule/declaration shape: %S" css;
  if warnings = [] then failf "recovery emitted no warning: %S" css

let test_recovery_invalid_rule_boundary buf =
  let invalid_rule =
    pick
      [
        "@unknown-rule{.bad{color:red}}";
        ".bad:not(){color:red}";
        ".bad:nth-child(foo){color:red}";
      ]
      buf 0
  in
  let css = ".ok{color:green}" ^ invalid_rule ^ ".next{color:blue}" in
  assert_strict_rejects_lenient_warns "invalid rule boundary recovery" css;
  let { Css.stylesheet; warnings; _ } =
    recovered_css "invalid rule boundary recovery" css
  in
  let counts = recovered_declaration_counts stylesheet in
  if not (List.equal Int.equal counts [ 1; 1 ]) then
    failf "CSS Syntax invalid-rule recovery did not preserve sibling rules: %S"
      css;
  if warnings = [] then failf "invalid rule emitted no warning: %S" css

let test_recovery_bad_declaration buf =
  let bad_decl =
    pick
      [ "color:rgb(300)"; "background-color:rgb(999)"; "width:calc(1px + )" ]
      buf 0
  in
  let css = ".a{" ^ bad_decl ^ ";color:red}" in
  assert_strict_rejects_lenient_warns "bad declaration recovery" css;
  let { Css.stylesheet; warnings; _ } =
    recovered_css "bad declaration recovery" css
  in
  let counts = recovered_declaration_counts stylesheet in
  if not (List.equal Int.equal counts [ 1 ]) then
    failf "CSS Syntax recovery dropped valid declaration after invalid one: %S"
      css;
  if warnings = [] then failf "bad declaration emitted no warning: %S" css

let test_random_stylesheet_contract buf =
  let input = css_like_stylesheet buf in
  let lenient = recovered_css "random stylesheet contract" input in
  match Css.of_string ~strict:true input with
  | Ok strict_result ->
      if lenient.warnings <> [] then
        failf "strict accepted fuzz input but lenient emitted warnings: %S"
          input;
      let strict_output =
        Css.to_string ~minify:true strict_result.Css.stylesheet
      in
      let lenient_output = Css.to_string ~minify:true lenient.stylesheet in
      if strict_output <> lenient_output then
        failf
          "strict/lenient serialization diverged for fuzz input: %S -> %S / %S"
          input strict_output lenient_output
  | Error _ ->
      ignore (Css.to_string ~minify:true lenient.stylesheet : string);
      if lenient.warnings = [] then
        failf "strict rejected fuzz input but lenient recovered silently: %S"
          input

(* Count [!important] declarations across all top-level statements. *)
let count_important sheet =
  Css.fold
    (fun acc stmt ->
      Css.Stylesheet.statement_declarations stmt
      |> List.filter Css.declaration_is_important
      |> List.length |> ( + ) acc)
    0 sheet

(* [!important] preservation: round-tripping must not lose, gain, or
   accidentally promote/demote the !important flag on any declaration. *)
let test_important_roundtrip buf =
  match Css.of_string ~strict:true buf with
  | Error _ -> ()
  | Ok parsed -> (
      let before = count_important parsed.stylesheet in
      let serialized = Css.to_string ~minify:true parsed.stylesheet in
      match Css.of_string ~strict:true serialized with
      | Error err ->
          failf "!important roundtrip: reparse failed: %S (%s)" serialized
            (Cascade.Error.to_string err)
      | Ok reparsed ->
          let after = count_important reparsed.Css.stylesheet in
          if before <> after then
            failf
              "!important count drifted across roundtrip (%d -> %d): %S -> %S"
              before after buf serialized)

(* Cascade source order: serializing a stylesheet then reparsing it must yield
   the same number of top-level statements in the same order (by minified
   rendering of each). *)
let test_source_order_preserved buf =
  match Css.of_string ~strict:true buf with
  | Error _ -> ()
  | Ok parsed -> (
      (* An empty rule renders to nothing under minify and is eliminated, so it
         carries no cascade position and a roundtrip legitimately makes it
         vanish. Compare only the statements that survive minification; their
         relative order is what source-order preservation means. *)
      let render_each ss =
        List.rev
          (Css.fold
             (fun acc stmt ->
               match
                 Css.to_string ~minify:true (Css.statements [ stmt ] |> Css.v)
               with
               | "" -> acc
               | rendered -> rendered :: acc)
             [] ss)
      in
      let before = render_each parsed.stylesheet in
      let serialized = Css.to_string ~minify:true parsed.stylesheet in
      match Css.of_string ~strict:true serialized with
      | Error _ -> ()
      | Ok reparsed ->
          let after = render_each reparsed.Css.stylesheet in
          if before <> after then
            failf
              "cascade source order changed across roundtrip:\n\
              \  before: %s\n\
              \  after:  %s"
              (String.concat "|" before) (String.concat "|" after))

(* CSS Syntax consumes ordinary comments before parsing. Minified output of any
   input - clean or recovered - must never contain CSS comment delimiters. *)
let test_no_comments_in_output buf =
  let check label output =
    let contains_comment_open =
      let n = String.length output in
      let rec scan i =
        if i + 1 >= n then false
        else if output.[i] = '/' && output.[i + 1] = '*' then true
        else scan (i + 1)
      in
      scan 0
    in
    if contains_comment_open then
      failf "%s output contains comment delimiter: %S" label output
  in
  (match Css.of_string ~strict:true buf with
  | Error _ -> ()
  | Ok parsed ->
      check "strict-minified" (Css.to_string ~minify:true parsed.stylesheet));
  match Css.of_string ~strict:false buf with
  | Error _ -> ()
  | Ok parsed ->
      check "lenient-minified" (Css.to_string ~minify:true parsed.stylesheet);
      check "lenient-pretty" (Css.to_string ~minify:false parsed.stylesheet)

(* Every warning kind but one names something lenient mode repairs: a rejected
   value or selector is dropped, an unterminated node is closed, so the repair
   is done once and the reparse is silent. [Unknown_at_rule] names no defect.
   CSS Syntax 3 sec. 5.4.2 consumes an at-rule whatever its at-keyword, and sec.
   5.5.2 keeps the block it consumes, so an at-keyword cascade has no handler
   for is well-formed CSS carrying no grammar cascade knows - discarding it is
   the user agent's step (CSS 2.1 sec. 4.2), and a transform that took it would
   delete every construct newer than its own vocabulary. There is no repair that
   both keeps the at-rule and clears the notice, because the notice is about the
   name and the name is the data. So it recurs on every reparse by design, and
   the two properties below, which measure repair, read it apart from the kinds
   that do report one. *)
let is_unknown_at_keyword (err : Error.t) =
  match err.Error.kind with Error.Unknown_at_rule _ -> true | _ -> false

(* Count them rather than name them: an at-keyword's spelling is not stable
   across a roundtrip, since a source byte outside ASCII comes back as the hex
   escape for the code point it denotes, exactly as a selector's does ([.\xb5x]
   serialises to [.\b5 x]). Serialisation splitting one at-rule in two, or
   minting one the input never held, still moves the count. *)
let unknown_at_rule_count warnings =
  List.length (List.filter is_unknown_at_keyword warnings)

(* Lenient output is strict-parseable: after lenient recovery, the serialized
   stylesheet must be acceptable to strict mode. This is the "best-minifier"
   guarantee: feed lenient any garbage, get clean spec-compliant CSS out. The
   one thing strict may still reject is an at-keyword it had no handler for on
   the way in, which lenient carried through rather than deleting. Strict stops
   at the first warning, so an unrecognised at-rule sitting ahead of a real
   defect hides it here; [recovery is total] below reads the whole warning list
   and fails on that case. *)
let test_lenient_output_strict_parseable buf =
  match Css.of_string ~strict:false buf with
  | Error _ -> ()
  | Ok parsed -> (
      let serialized = Css.to_string ~minify:true parsed.stylesheet in
      match Css.of_string ~strict:true serialized with
      | Ok _ -> ()
      | Error err
        when is_unknown_at_keyword err
             && unknown_at_rule_count parsed.warnings > 0 ->
          ()
      | Error err ->
          failf
            "lenient output is not strict-parseable: lenient cleaned %S to %S \
             but strict rejected it (%s)"
            buf serialized
            (Cascade.Error.to_string err))

(* Recovery is total: after lenient parse, everything the recovered AST still
   holds is material cascade can read back, so re-parsing its serialization in
   lenient mode reports no repairable defect. A warning of any other kind means
   the parser kept invalid material inside the AST and laundered the error
   through the serializer instead of fixing it. The unrecognised at-rules that
   do recur must be ones the input already carried: minting one is the same bug
   wearing the exempt kind. *)
let test_lenient_recovery_is_total buf =
  match Css.of_string ~strict:false buf with
  | Error _ -> ()
  | Ok parsed -> (
      let serialized = Css.to_string ~minify:true parsed.stylesheet in
      match Css.of_string ~strict:false serialized with
      | Error err ->
          failf
            "lenient recovery is not total: recovered serialization failed to \
             reparse: %S (%s)"
            serialized
            (Cascade.Error.to_string err)
      | Ok reparsed ->
          let defects =
            List.filter
              (fun w -> not (is_unknown_at_keyword w))
              reparsed.Css.warnings
          in
          if defects <> [] then
            failf
              "lenient recovery is not total: recovered AST re-serialized to \
               CSS that still emits warnings: %S -> %S (%s)"
              buf serialized
              (String.concat "; " (List.map Cascade.Error.to_string defects));
          let carried = unknown_at_rule_count parsed.warnings in
          let reported = unknown_at_rule_count reparsed.Css.warnings in
          if reported > carried then
            failf
              "lenient recovery is not total: serialization turned %d \
               unrecognised at-rule(s) into %d: %S -> %S"
              carried reported buf serialized)

(* Strict output reparses strictly: if [Css.of_string ~strict:true] accepted the
   input, the serialized output must also be strict-accepted (no new warnings
   introduced by serialization). Catches "serializer emits CSS the strict parser
   would reject" bugs. *)
let test_strict_output_reparses_strictly buf =
  match Css.of_string ~strict:true buf with
  | Error _ -> ()
  | Ok parsed -> (
      let serialized = Css.to_string ~minify:true parsed.stylesheet in
      match Css.of_string ~strict:true serialized with
      | Ok _ -> ()
      | Error err ->
          failf
            "strict-accepted input serialized to strict-rejected output: %S -> \
             %S (%s)"
            buf serialized
            (Cascade.Error.to_string err))

(* Optimize preserves strict-validity: if strict accepted the input, the
   optimized stylesheet must also be strict-accepted after serialization. *)
let test_optimize_preserves_strict_validity buf =
  match Css.of_string ~strict:true buf with
  | Error _ -> ()
  | Ok parsed -> (
      let optimized = Css.optimize parsed.stylesheet in
      let serialized = Css.to_string ~minify:true optimized in
      match Css.of_string ~strict:true serialized with
      | Ok _ -> ()
      | Error err ->
          failf "optimize broke strict-validity: %S -> %S (%s)" buf serialized
            (Cascade.Error.to_string err))

(* Comments-anywhere robustness: per CSS Syntax 3 SS 4.3.2 comments are
   whitespace-equivalent and can appear between any two tokens. Insert a random
   comment at every whitespace position in a shaped stylesheet and verify that
   lenient parse succeeds (the parser must not crash on comments in any
   position, even if it discards them). *)
let test_comments_anywhere_robust buf =
  let base = css_like_stylesheet buf in
  let marker =
    pick [ "/* x */"; "/*!loud*/"; "/**/"; "/* multi\nline */" ] buf 0
  in
  (* Insert the comment at every ASCII whitespace boundary. *)
  let inject s =
    let b = Buffer.create (String.length s * 2) in
    String.iter
      (fun c ->
        if c = ' ' then Buffer.add_string b marker;
        Buffer.add_char b c)
      s;
    Buffer.contents b
  in
  let mutated = inject base in
  match Css.of_string ~strict:false mutated with
  | Ok _ -> ()
  | Error err ->
      failf "lenient parse must accept comments anywhere; failed on %S: %s"
        mutated
        (Cascade.Error.to_string err)

(* Comments in raw bytes: the lower-level [Stylesheet.read] (used through
   [Cursor.of_string]) must also not crash when the input contains comments at
   pathological positions, even alongside random garbage. *)
let test_comments_random_no_crash buf =
  let marker = "/* fuzz */" in
  let n = String.length buf in
  if n = 0 then ()
  else
    let pos = byte_at buf 0 mod (n + 1) in
    let mutated =
      String.sub buf 0 pos ^ marker ^ String.sub buf pos (n - pos)
    in
    try ignore (Css.of_string ~strict:false mutated : (_, _) result)
    with exn ->
      failf "lenient parse raised on comment-inserted input %S: %s" mutated
        (Printexc.to_string exn)

(* Minify monotonicity: minified output must never be longer than pretty output
   for the same stylesheet. A regression here means the minifier added bytes -
   the textbook minifier bug. *)
let test_minify_monotonicity buf =
  let buf = cssish buf in
  match parse_stylesheet buf with
  | None -> ()
  | Some ss ->
      let m = minified_stylesheet ss in
      let p = pretty_stylesheet ss in
      if String.length m > String.length p then
        failf
          "minify is not monotonic: minified is longer than pretty (%d > %d): \
           %S vs %S"
          (String.length m) (String.length p) m p

(* Universal dual-mode invariant on raw bytes: (A) [of_string ~strict:false _]
   is total - never returns [Error] no matter what bytes you feed it. (B)
   [Error] in strict mode iff non-empty [warnings] in lenient mode. (C) when
   strict succeeds, both modes serialize to the same minified output. *)
let test_dual_mode_invariant_raw buf =
  let lenient =
    match Css.of_string ~strict:false buf with
    | Ok parsed -> parsed
    | Error err ->
        failf "lenient mode is not total: returned Error on raw input %S: %s"
          buf
          (Cascade.Error.to_string err)
  in
  match Css.of_string ~strict:true buf with
  | Ok strict_result ->
      if lenient.warnings <> [] then
        failf
          "dual-mode drift: strict accepted but lenient warned on raw input %S"
          buf;
      let strict_output =
        Css.to_string ~minify:true strict_result.Css.stylesheet
      in
      let lenient_output = Css.to_string ~minify:true lenient.stylesheet in
      if strict_output <> lenient_output then
        failf
          "dual-mode drift: strict/lenient outputs diverged on raw input %S: \
           %S vs %S"
          buf strict_output lenient_output
  | Error _ ->
      if lenient.warnings = [] then
        failf
          "dual-mode drift: strict rejected raw input %S but lenient emitted \
           no warning"
          buf

let test_stylesheet_prelude_order_vectors buf =
  let input =
    pick
      [
        "@charset \"UTF-8\"; @layer reset, theme; @import url(theme.css) \
         layer(theme); @namespace url(http://www.w3.org/1999/xhtml);";
        "@layer reset, theme; @import url(theme.css) layer(theme); .ok { \
         color: red }";
        "@import url(base.css) screen and (width >= 40em); @namespace svg \
         url(http://www.w3.org/2000/svg); .icon { fill: currentColor }";
        "@import url(theme.css) layer() supports((display: grid)) screen and \
         (color); .ok { color: red }";
        "@namespace svg url(http://www.w3.org/2000/svg); *|a { color: red }";
      ]
      buf 0
  in
  assert_strict_accepts_cleanly "valid stylesheet prelude order" input;
  match parse_stylesheet input with
  | None -> failf "valid stylesheet prelude order did not parse: %S" input
  | Some ss ->
      let shapes = boundary_shapes ss in
      if shapes = [] then
        failf "valid stylesheet prelude produced no statements: %S" input

let test_invalid_prelude_order buf =
  let input =
    pick
      [
        ".x { color: red } @charset \"UTF-8\";";
        ".x { color: red } }";
        "@charset url(UTF-8);";
        "@charset \"UTF-8\" .x { color: red }";
        ".x { color: red } @import url(late.css);";
        "@namespace svg url(http://www.w3.org/2000/svg); @import url(late.css);";
        "@import url(theme.css) { .x { color: red } }";
        "@import url(theme.css) layer(theme) layer(base);";
        "@import url(theme.css) screen supports(display: grid);";
        "@media screen { @import url(inner.css); }";
        "@import url(base.css); @layer theme; @import url(late.css);";
        "@import url(base.css); @layer theme { .x { color: red } } @namespace \
         url(http://www.w3.org/1999/xhtml);";
        "@namespace svg;";
        "@layer reset,,base;";
        "@layer initial { .x { color: red } }";
      ]
      buf 0
  in
  assert_strict_rejects_lenient_warns "invalid stylesheet prelude order" input

let parser_cases =
  [
    test_case "stylesheet read crash safety" [ bytes ] test_read_stylesheet;
    test_case "stylesheet read crash safety over non-ascii bytes" [ bytes ]
      test_stylesheet_unicode_bytes;
    test_case "read_rule crash safety" [ bytes ] test_read_rule;
    test_case "read_block crash safety" [ bytes ] test_read_block;
    test_case "read crash safety" [ bytes ] test_read;
    test_case "read_import_rule crash safety" [ bytes ] test_read_import_rule;
    test_case "read_declaration crash safety" [ bytes ] test_read_declaration;
    test_case "read_declarations crash safety" [ bytes ] test_read_declarations;
    test_case "read_property_name crash safety" [ bytes ]
      test_read_property_name;
    test_case "read_property_value crash safety" [ bytes ]
      test_read_property_value;
  ]

let roundtrip_cases =
  [
    test_case "roundtrip" [ bytes ] test_stylesheet_roundtrip;
    test_case "minified serialization idempotent" [ bytes ]
      test_stylesheet_minified_idempotent;
    test_case "pretty serialization idempotent" [ bytes ]
      test_stylesheet_pretty_idempotent;
    test_case "generated layer stylesheet roundtrip" [ bytes ]
      test_generated_layer_stylesheet_roundtrip;
    test_case "generated condition stylesheet roundtrip" [ bytes ]
      test_generated_condition_stylesheet_roundtrip;
    test_case "generated pretty stylesheet roundtrip" [ bytes ]
      test_generated_stylesheets_pretty_roundtrip;
  ]

let invariant_cases =
  [
    test_case "layer boundary shape invariant" [ bytes ]
      test_layer_boundary_shape_invariant;
    test_case "anonymous layer count invariant" [ bytes ]
      test_anonymous_layer_count_invariant;
    test_case "condition boundary shape invariant" [ bytes ]
      test_condition_boundary_shape_invariant;
    test_case "shorthand CSS-wide keyword invariant" [ bytes ]
      test_shorthand_wide_keyword;
    test_case "shorthand CSS-wide keyword mix rejected" [ bytes ]
      test_shorthand_wide_mix;
    test_case "legacy shorthand alias serialization invariant" [ bytes ]
      test_legacy_alias_stable;
    test_case "declared values source order invariant" [ bytes ]
      test_declared_order_stable;
    test_case "specified value unset inheritance invariant" [ bytes ]
      test_unset_inheritance;
    test_case "integrated cascade source order invariant" [ bytes ]
      test_cascade_source_order;
    test_case "revert-layer specified value invariant" [ bytes ]
      test_revert_layer_value;
    test_case "platform declaration property name invariant" [ bytes ]
      test_platform_decl_name;
    test_case "invalid platform declaration rejected" [ bytes ]
      test_invalid_platform_declaration_rejected;
    test_case "import and URL syntax roundtrip" [ bytes ] test_import_url_syntax;
    test_case "namespace prefix separator invariant" [ bytes ]
      test_namespace_prefix_separator;
  ]

let descriptor_cases =
  [
    test_case "valid at-rule descriptor vectors" [ bytes ]
      test_valid_atrule_descriptor;
    test_case "invalid at-rule descriptor vectors rejected" [ bytes ]
      test_invalid_atrule_descriptor;
    test_case "shared at-rule inventory valid vectors" [ bytes ]
      test_atrule_inventory_valid;
    test_case "shared at-rule inventory invalid vectors rejected" [ bytes ]
      test_atrule_inventory_invalid;
    test_case "font-face descriptor matrix" [ bytes ]
      test_font_face_descriptor_matrix;
    test_case "invalid font-face descriptor matrix rejected" [ bytes ]
      test_invalid_font_face;
    test_case "page margin descriptor matrix" [ bytes ]
      test_page_margin_descriptor_matrix;
    test_case "invalid page margin descriptor matrix rejected" [ bytes ]
      test_invalid_page_margin;
    test_case "font-palette-values descriptor matrix" [ bytes ]
      test_palette_descriptor_matrix;
    test_case "invalid font-palette-values descriptor matrix rejected" [ bytes ]
      test_invalid_palette_descriptor;
    test_case "view-transition descriptor matrix" [ bytes ]
      test_view_transition_descriptor_matrix;
    test_case "invalid view-transition descriptor matrix rejected" [ bytes ]
      test_invalid_view_transition;
    test_case "position-try descriptor matrix" [ bytes ]
      test_position_try_descriptor_matrix;
    test_case "invalid position-try descriptor matrix rejected" [ bytes ]
      test_invalid_position_try;
  ]

let recovery_cases =
  [
    test_case "property value context invariant" [ bytes ]
      test_property_value_context;
    test_case "CSS Syntax recovery keeps sibling rules" [ bytes ]
      test_recovery_keeps_rules;
    test_case "CSS Syntax recovery invalid rule boundary" [ bytes ]
      test_recovery_invalid_rule_boundary;
    test_case "CSS Syntax recovery bad declaration then good" [ bytes ]
      test_recovery_bad_declaration;
    test_case "strict and lenient random stylesheet contract" [ bytes ]
      test_random_stylesheet_contract;
    test_case "dual-mode invariant: lenient total, strict iff warnings"
      [ bytes ] test_dual_mode_invariant_raw;
    test_case "minify monotonicity: minified <= pretty" [ bytes ]
      test_minify_monotonicity;
    test_case "strict output reparses strictly" [ bytes ]
      test_strict_output_reparses_strictly;
    test_case "optimize preserves strict-validity" [ bytes ]
      test_optimize_preserves_strict_validity;
    test_case "lenient recovery is total: re-serialize repairs nothing further"
      [ bytes ] test_lenient_recovery_is_total;
    test_case "lenient output is strict-parseable" [ bytes ]
      test_lenient_output_strict_parseable;
    test_case "no CSS comment delimiters survive serialization" [ bytes ]
      test_no_comments_in_output;
    test_case "comments at every whitespace boundary parse leniently" [ bytes ]
      test_comments_anywhere_robust;
    test_case "comment inserted at random byte position never crashes" [ bytes ]
      test_comments_random_no_crash;
    test_case "!important count preserved across roundtrip" [ bytes ]
      test_important_roundtrip;
    test_case "cascade source order preserved across roundtrip" [ bytes ]
      test_source_order_preserved;
    test_case "stylesheet prelude order vectors" [ bytes ]
      test_stylesheet_prelude_order_vectors;
    test_case "invalid stylesheet prelude order vectors rejected" [ bytes ]
      test_invalid_prelude_order;
  ]

let suite_cases =
  parser_cases @ roundtrip_cases @ invariant_cases @ descriptor_cases
  @ recovery_cases

let suite = ("stylesheet", suite_cases)
