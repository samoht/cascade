(** Fuzz tests for explicit CSS transform contexts. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let name prefix buf i = prefix ^ string_of_int (byte_at buf i)
let css_value pp value = Css.Pp.to_string ~minify:true pp value
let px value = Css.Media.Length (Css.Values.Px value)
let ident value = Css.Media.Ident (Css.Media.ident_of_string value)
let feature = Css.Media.feature
let container_feature = Css.Container.feature

let contains_literal haystack needle =
  let re = Re.compile (Re.str needle) in
  Re.execp re haystack

let pp_decl decl = Css.Declaration.string_of_declaration ~minify:true decl
let pp_stylesheet sheet = Css.Stylesheet.to_string ~minify:true sheet

let normalize_decl decl =
  Css.Declaration.of_string
    (Css.Declaration.string_of_declaration ~minify:true decl)

let check_same_decl label expected actual =
  let expected = normalize_decl expected in
  let actual = normalize_decl actual in
  let expected = pp_decl expected in
  let actual = pp_decl actual in
  if expected <> actual then
    failf "%s: expected %S, got %S" label expected actual

let check_eval_shape label input actual =
  let before_name = Css.Declaration.property_name input in
  let after_name = Css.Declaration.property_name actual in
  if before_name <> after_name then
    failf "%s: property changed from %S to %S" label before_name after_name;
  let reparsed = Css.Declaration.of_string (pp_decl actual) in
  check_same_decl (label ^ " roundtrip") actual reparsed

let check_same_stylesheet label expected actual =
  if not (Css.Stylesheet.equal expected actual) then
    failf "%s: expected %S, got %S" label (pp_stylesheet expected)
      (pp_stylesheet actual)

let expect_contains label haystack needle =
  if not (contains_literal haystack needle) then
    failf "%s missing %S in %S" label needle haystack

let number buf i =
  let n = byte_at buf i mod 9 in
  if n = 0 then "0" else string_of_int n

let percentage buf i = string_of_int (byte_at buf i mod 101) ^ "%"

let length buf i =
  let n = 1 + (byte_at buf i mod 16) in
  let unit = pick [ "px"; "rem"; "em"; "vw"; "vh"; "cqw"; "cqh" ] buf (i + 1) in
  string_of_int n ^ unit

let resolvable_length buf i =
  let n = 1 + (byte_at buf i mod 16) in
  let unit = pick [ "px"; "rem"; "em" ] buf (i + 1) in
  string_of_int n ^ unit

let angle buf i =
  let n = 1 + (byte_at buf i mod 359) in
  let unit = pick [ "deg"; "turn"; "grad"; "rad" ] buf (i + 1) in
  string_of_int n ^ unit

let duration buf i =
  let n = 1 + (byte_at buf i mod 9) in
  let unit = pick [ "ms"; "s" ] buf (i + 1) in
  string_of_int n ^ unit

let generated_decl buf =
  let calc_len i =
    "calc(" ^ resolvable_length buf i ^ " + "
    ^ resolvable_length buf (i + 2)
    ^ ")"
  in
  let residual_len i =
    "calc(" ^ resolvable_length buf i ^ " + " ^ length buf (i + 2) ^ ")"
  in
  let decl =
    match byte_at buf 13 mod 30 with
    | 0 -> "margin-left: " ^ calc_len 14
    | 1 -> "height: " ^ residual_len 15
    | 2 ->
        "width: calc(" ^ percentage buf 16 ^ " + " ^ resolvable_length buf 17
        ^ ")"
    | 3 ->
        "padding: var(--gap, " ^ resolvable_length buf 18 ^ ") "
        ^ "calc(var(--missing, " ^ resolvable_length buf 20 ^ ") * "
        ^ number buf 22 ^ ")"
    | 4 -> "color: var(--brand, " ^ pick [ "red"; "green"; "blue" ] buf 23 ^ ")"
    | 5 -> "display: var(--missing, inherit)"
    | 6 -> "color: var(--missing, unset)"
    | 7 -> "background-image: url(../img/" ^ name "asset-" buf 24 ^ ".svg)"
    | 8 ->
        "background-color: rgb(var(--rgb) / var(--alpha, "
        ^ pick [ "0.25"; "0.5"; "75%" ] buf 25
        ^ "))"
    | 9 ->
        "transform: translateX(" ^ calc_len 26 ^ ") rotate(calc(" ^ angle buf 28
        ^ " + " ^ angle buf 30 ^ "))"
    | 10 -> "rotate: calc(" ^ angle buf 31 ^ " + " ^ angle buf 33 ^ ")"
    | 11 ->
        "transition-duration: calc(" ^ duration buf 34 ^ " + " ^ duration buf 36
        ^ ")"
    | 12 -> "scale: calc(" ^ percentage buf 37 ^ " + " ^ percentage buf 38 ^ ")"
    | 13 ->
        "box-shadow: 0 0 0 calc(" ^ resolvable_length buf 39
        ^ " + var(--ring-offset)) var(--ring-color)"
    | 14 -> "margin-left: var(--missing, calc(var(--gap) + var(--future-gap)))"
    | 15 -> "z-index: calc(sibling-index() + " ^ number buf 40 ^ ")"
    | 16 ->
        "font-size: calc(" ^ percentage buf 41 ^ " + "
        ^ resolvable_length buf 42 ^ ")"
    | 17 -> "grid-template-columns: minmax(min-content, 1fr) fit-content(20%)"
    | 18 -> "flex-basis: content"
    | 19 -> "aspect-ratio: 16 / 9"
    | 20 -> "width: " ^ string_of_int (1 + (byte_at buf 44 mod 16)) ^ "ch"
    | 21 ->
        "color: color-mix(in oklab, "
        ^ pick [ "red"; "green"; "blue" ] buf 45
        ^ " 40%, "
        ^ pick [ "white"; "black"; "transparent" ] buf 46
        ^ ")"
    | 22 -> "color: color(display-p3 1 0.5 0)"
    | 23 -> "filter: blur(" ^ resolvable_length buf 47 ^ ") contrast(120%)"
    | 24 -> "mask-image: linear-gradient(black, transparent)"
    | 25 -> "animation-timeline: scroll()"
    | 26 ->
        "animation-duration: calc(" ^ duration buf 48 ^ " + " ^ duration buf 50
        ^ ")"
    | 27 ->
        "animation-delay: var(--delay, calc(" ^ duration buf 52 ^ " * "
        ^ number buf 54 ^ "))"
    | 28 ->
        "transition: opacity var(--duration, " ^ duration buf 55
        ^ ") ease-in var(--delay, " ^ duration buf 56 ^ ")"
    | _ -> "view-timeline: --reveal block"
  in
  Css.Declaration.of_string decl

let fuzz_decl buf =
  if byte_at buf 0 mod 3 = 0 then generated_decl buf
  else
    pick
      [
        "margin-left: calc(1rem + 2px)";
        "height: calc(1rem + 10vh)";
        "width: calc(50% + 1rem)";
        "padding: var(--gap) calc(var(--gap) * 2)";
        "color: var(--brand, green)";
        "border-color: currentColor";
        "background-image: url(../img/logo.svg)";
        "background-color: rgb(var(--rgb) / var(--alpha))";
        "color: color(srgb calc(0.25 + 0.25) 0 0)";
        "display: var(--missing, inherit)";
        "color: var(--missing, unset)";
        "margin-left: var(--missing, calc(var(--gap) + var(--future-gap)))";
        "box-shadow: var(--shadow)";
        "z-index: calc(sibling-index() + 1)";
        "opacity: calc(0.25 * 2)";
        "rotate: calc(30deg + 15deg)";
        "animation-duration: calc(1s * 2)";
        "transition-duration: calc(500ms + 0.5s)";
        "scale: calc(50% + 25%)";
        "filter: opacity(calc(50% + 25%))";
        "transform: translateX(var(--shift)) rotate(45deg)";
        "box-shadow: 0 0 0 calc(3px + var(--ring-offset)) var(--ring-color)";
        "grid-template-columns: minmax(min-content, 1fr) fit-content(20%)";
        "flex-basis: content";
        "aspect-ratio: 16 / 9";
        "width: 12ch";
        "color: color-mix(in oklab, red 40%, blue)";
        "color: color(display-p3 1 0.5 0)";
        "filter: blur(4px) contrast(120%)";
        "mask-image: linear-gradient(black, transparent)";
        "animation-timeline: scroll()";
        "animation-duration: calc(500ms * 2)";
        "animation-delay: calc(250ms * 2)";
        "transition: opacity var(--duration, 1s) ease-in var(--delay, 500ms)";
        "view-timeline: --reveal block";
      ]
      buf 0
    |> Css.Declaration.of_string

let conservative_decl buf =
  pick
    [
      "display: grid";
      "color: red";
      "opacity: 0.5";
      "margin-left: 12px";
      "background-image: url(https://example.test/img/logo.svg)";
      "transform: translateX(10px) rotate(45deg)";
    ]
    buf 1
  |> Css.Declaration.of_string

let computable_decl buf =
  pick
    [
      "margin-left: var(--gap)";
      "padding: calc(var(--gap) * 2)";
      "color: var(--brand)";
      "border-color: currentColor";
      "background-image: url(../img/logo.svg)";
      "background-color: rgb(var(--rgb) / var(--alpha))";
      "opacity: var(--alpha)";
      "rotate: calc(30deg + 15deg)";
      "animation-duration: var(--duration)";
      "transition-delay: var(--delay)";
      "transform: translateX(var(--shift)) rotate(45deg)";
    ]
    buf 2
  |> Css.Declaration.of_string

let contains_residual text =
  List.exists (contains_literal text)
    [
      "var(";
      "currentColor";
      "inherit";
      "initial";
      "unset";
      "revert";
      "revert-layer";
    ]

let eval_ctx ?viewport_height buf =
  let open Css.Values in
  let root = float_of_int (12 + (byte_at buf 2 mod 9)) in
  let parent = float_of_int (10 + (byte_at buf 3 mod 9)) in
  let viewport_width = float_of_int (320 + (byte_at buf 4 mod 1200)) in
  let container_width = float_of_int (160 + (byte_at buf 5 mod 900)) in
  Css.Context.v
    ~custom_properties:
      [
        Css.Declaration.of_string "--brand: red";
        Css.Declaration.of_string "--gap: calc(1rem + 2px)";
        Css.Declaration.of_string "--rgb: 12 34 56";
        Css.Declaration.of_string "--alpha: 0.5";
        Css.Declaration.of_string "--shift: 1rem";
        Css.Declaration.of_string "--ring-offset: 2px";
        Css.Declaration.of_string "--ring-color: blue";
        Css.Declaration.of_string "--shadow: 0 0 var(--gap) currentColor";
        Css.Declaration.of_string "--duration: calc(500ms * 2)";
        Css.Declaration.of_string "--delay: calc(250ms * 2)";
      ]
    ~inherited_values:
      [
        Css.Declaration.of_string "color: blue";
        Css.Declaration.of_string "display: block";
      ]
    ~initial_values:
      [
        Css.Declaration.of_string "display: inline";
        Css.Declaration.of_string "margin-left: 0";
      ]
    ~base_url:"https://example.test/css/app.css" ~root_font_size:(Px root)
    ~parent_font_size:(Px parent) ~current_color:(Named Blue)
    ~viewport_width:(Px viewport_width) ?viewport_height
    ~container_width:(Px container_width) ()

let stylesheet_of_string css =
  try Css.Stylesheet.read_stylesheet (Cursor.of_string css)
  with Cursor.Parse_error err ->
    failf "stylesheet did not parse: %s" (Error.to_string err)

let fuzz_stylesheet buf =
  pick
    [
      ".card{color:var(--brand);margin-left:var(--gap)}";
      ".card{& .title{border-color:currentColor;padding:var(--gap)}}";
      "@media (min-width:40rem){.card{width:calc(100% - 1rem)}}";
      "@supports (display:grid){.card{transform:translateX(var(--gap))}}";
      "@starting-style{.card{opacity:var(--alpha)}}";
      "@keyframes \
       fade{from{opacity:var(--alpha)}to{transform:translateX(var(--gap))}}";
      "@font-face{font-family:Brand;src:url(brand.woff2) \
       format(\"woff2\");font-display:swap}";
      ".grid{grid-template-columns:minmax(min-content,1fr) \
       fit-content(20%);aspect-ratio:16/9}";
      ".media{filter:blur(4px) contrast(120%);animation-timeline:scroll()}";
      "@page{margin:var(--gap)}";
    ]
    buf 12
  |> stylesheet_of_string

let eval_layered_ctx ?viewport_height buf =
  let open Css.Values in
  let root = float_of_int (12 + (byte_at buf 6 mod 9)) in
  Css.Context.v
    ~custom_properties:
      [
        Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
        Css.Declaration.custom_property ~layer:"base" "--base-gap"
          "calc(var(--spacing) * 2)";
        Css.Declaration.custom_property ~layer:"components" "--component-gap"
          "calc(var(--base-gap) + 1rem)";
        Css.Declaration.custom_property ~layer:"utilities" "--utility-gap"
          "calc(var(--component-gap) + 2vh)";
        Css.Declaration.custom_property ~layer:"utilities" "--future-gap"
          "calc(var(--component-gap) + var(--user-gap))";
      ]
    ~root_font_size:(Px root) ?viewport_height ()

let fuzz_layered_decl buf =
  pick
    [
      "margin-left: var(--utility-gap)";
      "margin-left: var(--future-gap)";
      "padding: var(--component-gap)";
    ]
    buf 7
  |> Css.Declaration.of_string

let assert_empty_property_value_context (value : Css.Context.t) =
  let empty_lists =
    [
      value.custom_properties = [];
      value.inherited_values = [];
      value.initial_values = [];
    ]
  in
  let empty_options =
    [
      value.base_url = None;
      value.root_font_size = None;
      value.parent_font_size = None;
      value.current_color = None;
      value.viewport_width = None;
      value.viewport_height = None;
      value.container_width = None;
      value.container_height = None;
    ]
  in
  if not (List.for_all Fun.id (empty_lists @ empty_options)) then
    fail "empty property-value context changed"

let assert_empty_document_context document =
  if Css.Context.has_class "x" document || Css.Context.has_id "x" document then
    fail "empty document context matched a class or id";
  if Css.Context.attribute "x" document <> None then
    fail "empty document context matched an attribute"

let assert_empty_query_context query =
  if Css.Context.media_feature "width" query <> None then
    fail "empty query context matched a media feature";
  if Css.Context.matches_supports query (Css.Supports.property "display" "grid")
  then fail "empty query context matched a support declaration";
  if Css.Context.container_feature "inline-size" query <> None then
    fail "empty query context matched a container feature"

let assert_empty_runtime_contexts () =
  if Css.Context.import_source "theme.css" Css.Context.empty_loader <> None then
    fail "empty loader context matched an import";
  if Css.Context.animates_property "opacity" Css.Context.empty_animation then
    fail "empty animation context matched a property"

let test_empty_contexts _buf =
  assert_empty_property_value_context Css.Context.empty;
  assert_empty_document_context Css.Context.empty_document;
  assert_empty_query_context Css.Context.empty_query;
  assert_empty_runtime_contexts ()

let assert_property_value_lookups ctx custom custom_decl property inherited_decl
    =
  if
    not
      (Option.equal Css.Declaration.equal_declaration
         (Css.Context.custom_property custom ctx)
         (Some custom_decl))
  then fail "custom property lookup changed";
  if
    not
      (Option.equal Css.Declaration.equal_declaration
         (Css.Context.inherited_value property ctx)
         (Some inherited_decl))
  then fail "inherited value lookup changed";
  if Css.Context.custom_property (custom ^ "-missing") ctx <> None then
    fail "custom property lookup stopped being exact";
  if Css.Context.inherited_value (property ^ "-missing") ctx <> None then
    fail "inherited value lookup stopped being exact"

let assert_initial_and_urls (ctx : Css.Context.t) initial_decl =
  if
    not
      (Option.equal Css.Declaration.equal_declaration
         (Css.Context.initial_value "display" ctx)
         (Some initial_decl))
  then fail "initial value lookup changed";
  if ctx.base_url <> Some "https://example.test/a.css" then
    fail "base URL context changed"

let assert_context_dimensions (ctx : Css.Context.t) =
  let open Css.Values in
  if
    (not
       (Option.equal Css.Values.equal_length ctx.root_font_size (Some (Px 16.))))
    || not
         (Option.equal Css.Values.equal_length ctx.parent_font_size
            (Some (Px 14.)))
  then fail "font-size context changed";
  if
    (not
       (Option.equal Css.Values.equal_length ctx.viewport_width
          (Some (Px 1024.))))
    || not
         (Option.equal Css.Values.equal_length ctx.viewport_height
            (Some (Px 768.)))
  then fail "viewport context changed";
  if
    (not
       (Option.equal Css.Values.equal_length ctx.container_width
          (Some (Px 640.))))
    || not
         (Option.equal Css.Values.equal_length ctx.container_height
            (Some (Px 480.)))
  then fail "container dimension context changed"

let test_property_value_context buf =
  let open Css.Values in
  let custom = name "--v" buf 0 in
  let property, value =
    pick
      [
        ("color", css_value pp_color (Named Red));
        ("font-size", css_value pp_length (Rem 1.));
        ("display", "grid");
        ("width", css_value pp_length Auto);
      ]
      buf 1
  in
  let custom_decl = Css.Declaration.of_string (custom ^ ": " ^ value) in
  let inherited_decl = Css.Declaration.of_string (property ^ ": " ^ value) in
  let initial_decl = Css.Declaration.of_string "display: inline" in
  let ctx =
    Css.Context.v ~custom_properties:[ custom_decl ]
      ~inherited_values:[ inherited_decl ] ~initial_values:[ initial_decl ]
      ~base_url:"https://example.test/a.css" ~root_font_size:(Px 16.)
      ~parent_font_size:(Px 14.) ~current_color:(Named Red)
      ~viewport_width:(Px 1024.) ~viewport_height:(Px 768.)
      ~container_width:(Px 640.) ~container_height:(Px 480.) ()
  in
  assert_property_value_lookups ctx custom custom_decl property inherited_decl;
  assert_initial_and_urls ctx initial_decl;
  assert_context_dimensions ctx

let document_context class_name id attr =
  Css.Context.document ~element:"div" ~classes:[ class_name ] ~ids:[ id ]
    ~attributes:[ (attr, Some "x"); ("disabled", None) ]
    ~pseudo_classes:[ "hover" ] ~pseudo_elements:[ "before" ] ()

let assert_document_lookup ctx class_name id attr =
  if not (Css.Context.has_class class_name ctx) then
    fail "document context lost class";
  if not (Css.Context.has_id id ctx) then fail "document context lost id";
  if Css.Context.attribute attr ctx <> Some (Some "x") then
    fail "document context lost attribute value";
  if Css.Context.attribute "disabled" ctx <> Some None then
    fail "document context lost valueless attribute";
  if Css.Context.has_class (class_name ^ "-missing") ctx then
    fail "document context class lookup stopped being exact";
  if Css.Context.has_id (id ^ "-missing") ctx then
    fail "document context id lookup stopped being exact";
  if Css.Context.attribute (attr ^ "-missing") ctx <> None then
    fail "document context attribute lookup stopped being exact"

let assert_document_fields (ctx : Css.Context.document) =
  if ctx.element <> Some "div" then fail "document context lost element";
  if not (List.equal String.equal ctx.pseudo_classes [ "hover" ]) then
    fail "document context lost pseudo-class list";
  if not (List.equal String.equal ctx.pseudo_elements [ "before" ]) then
    fail "document context lost pseudo-element list"

let test_document_context buf =
  let class_name = name "c" buf 0 in
  let id = name "id" buf 1 in
  let attr = name "data-" buf 2 in
  let ctx = document_context class_name id attr in
  assert_document_lookup ctx class_name id attr;
  assert_document_fields ctx

let query_feature_vector_at buf i =
  pick
    [
      ("width", px 1024.); ("height", px 768.); ("dynamic-range", ident "high");
    ]
    buf i

let query_feature_vector buf = query_feature_vector_at buf 0

let query_supports =
  [
    Css.Supports.property "display" "grid";
    Css.Supports.func "selector" ":has(img)";
  ]

let query_context media feature_value inline_size =
  Css.Context.query ~media_type:"screen"
    ~media_features:[ feature media feature_value ]
    ~supports:query_supports ~container_name:"card"
    ~container_features:[ container_feature "inline-size" inline_size ]
    ()

let assert_query_lookup ctx media feature_value inline_size =
  if
    not
      (Option.equal Css.Media.equal_value
         (Css.Context.media_feature media ctx)
         (Some feature_value))
  then fail "query context lost media feature";
  if
    not
      (Option.equal Css.Media.equal_value
         (Css.Context.container_feature "inline-size" ctx)
         (Some inline_size))
  then fail "query context lost container feature";
  if Css.Context.media_feature (media ^ "-missing") ctx <> None then
    fail "query context media lookup stopped being exact";
  if Css.Context.container_feature "block-size" ctx <> None then
    fail "query context container lookup stopped being exact"

let assert_query_supports ctx =
  if
    not
      (Css.Context.matches_supports ctx
         (Css.Supports.property "display" "grid"))
  then fail "query context lost supported declaration";
  if Css.Context.matches_supports ctx (Css.Supports.property "display" "flex")
  then fail "query context support value lookup stopped being exact";
  if
    not
      (Css.Context.matches_supports ctx
         (Css.Supports.property "Display" "grid"))
  then
    fail "query context support property lookup stopped being case-insensitive"

let assert_query_fields (ctx : Css.Context.query) =
  if ctx.media_type <> Some "screen" then fail "query context lost media type";
  if not (List.equal Css.Supports.equal ctx.supports query_supports) then
    fail "query context lost supports list";
  if ctx.container_name <> Some "card" then
    fail "query context lost container name"

let test_query_context buf =
  let media, feature_value = query_feature_vector_at buf 2 in
  let inline_size = px 640. in
  let ctx = query_context media feature_value inline_size in
  assert_query_lookup ctx media feature_value inline_size;
  assert_query_supports ctx;
  assert_query_fields ctx

let test_loader_animation_context buf =
  let url = pick [ "theme.css"; "../base.css"; "#inline" ] buf 0 in
  let source = pick [ ".a{color:red}"; "@layer theme{}" ] buf 1 in
  let loader =
    Css.Context.loader ~base_url:"https://example.test/"
      ~imports:[ (url, source) ]
      ()
  in
  if Css.Context.import_source url loader <> Some source then
    fail "loader context lost import source";
  let property = pick [ "opacity"; "transform"; "color" ] buf 2 in
  let animation =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.5
      ~animated_properties:[ property ] ()
  in
  if not (Css.Context.animates_property property animation) then
    fail "animation context lost property";
  if Css.Context.import_source (url ^ "-missing") loader <> None then
    fail "loader context import lookup stopped being exact";
  if Css.Context.animates_property (property ^ "-missing") animation then
    fail "animation context property lookup stopped being exact";
  if loader.base_url <> Some "https://example.test/" then
    fail "loader context lost base URL";
  if animation.timeline_time <> Some "250ms" || animation.progress <> Some 0.5
  then fail "animation context lost timeline fields"

let test_property_registration_context buf =
  let name = pick [ "--gap"; "--brand"; "--tokens" ] buf 0 in
  let syntax =
    pick
      [
        Css.Variables.Syntax Css.Variables.Length;
        Css.Variables.Syntax Css.Variables.Color;
        Css.Variables.Syntax Css.Variables.Universal;
      ]
      buf 1
  in
  let valid_value, invalid_value =
    match syntax with
    | Css.Variables.Syntax Css.Variables.Length -> ("1rem", "red")
    | Css.Variables.Syntax Css.Variables.Color -> ("red", "1rem")
    | Css.Variables.Syntax Css.Variables.Universal -> ("{ color: red }", "")
    | _ -> ("red", "")
  in
  let registration =
    Css.Context.property_registration name syntax ~inherits:true
      ~initial_value:valid_value
  in
  let registry =
    Css.Context.property_registry ~property_registrations:[ registration ] ()
  in
  if
    not
      (Option.equal Css.Context.equal_property_registration
         (Css.Context.registered_property name registry)
         (Some registration))
  then fail "property registry lost registration";
  let valid_decl = Css.Declaration.of_string (name ^ ": " ^ valid_value) in
  (match
     Css.Context.validate_registered_custom_property registry valid_decl
   with
  | Ok () -> ()
  | Error msg -> failf "registered valid value rejected: %S" msg);
  if invalid_value <> "" then
    let invalid_decl =
      Css.Declaration.of_string (name ^ ": " ^ invalid_value)
    in
    match
      Css.Context.validate_registered_custom_property registry invalid_decl
    with
    | Ok () -> fail "registered invalid value accepted"
    | Error _ -> ()

let assert_value_printer custom =
  let value_ctx =
    Css.Context.v
      ~custom_properties:[ Css.Declaration.of_string (custom ^ ": red") ]
      ~base_url:"https://example.test/print.css"
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  let value_dump = Css.Pp.to_string Css.Context.pp value_ctx in
  expect_contains "property-value context printer" value_dump custom;
  expect_contains "property-value context printer" value_dump
    "https://example.test/print.css"

let assert_document_printer class_name =
  let document_dump =
    Css.Context.document ~element:"section" ~classes:[ class_name ]
      ~attributes:[ ("data-state", Some "open") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_document
  in
  expect_contains "document context printer" document_dump class_name;
  expect_contains "document context printer" document_dump "data-state=open"

let assert_query_printer media feature_value =
  let query_dump =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ feature media feature_value ]
      ~supports:[ Css.Supports.func "selector" ":has(img)" ]
      ()
    |> Css.Pp.to_string Css.Context.pp_query
  in
  expect_contains "query context printer" query_dump media;
  expect_contains "query context printer" query_dump "selector(:has(img))"

let assert_loader_printer () =
  let loader_dump =
    Css.Context.loader ~base_url:"https://example.test/"
      ~imports:[ ("theme.css", ".card{color:red}") ]
      ()
    |> Css.Pp.to_string Css.Context.pp_loader
  in
  expect_contains "loader context printer" loader_dump "theme.css"

let assert_animation_printer property =
  let animation_dump =
    Css.Context.animation ~timeline_time:"250ms" ~progress:0.5
      ~animated_properties:[ property ] ()
    |> Css.Pp.to_string Css.Context.pp_animation
  in
  expect_contains "animation context printer" animation_dump property;
  expect_contains "animation context printer" animation_dump "progress=Some 0.5"

let test_context_printers buf =
  let custom = name "--print" buf 0 in
  let class_name = name "c" buf 1 in
  let media, feature_value = query_feature_vector buf in
  let property = pick [ "opacity"; "transform"; "color" ] buf 3 in
  assert_value_printer custom;
  assert_document_printer class_name;
  assert_query_printer media feature_value;
  assert_loader_printer ();
  assert_animation_printer property

let test_eval_idempotent buf =
  let ctx =
    eval_ctx
      ~viewport_height:
        (Css.Values.Px (float_of_int (480 + (byte_at buf 8 mod 900))))
      buf
  in
  let decl = fuzz_decl buf in
  let once = Css.eval_declaration ctx decl in
  let twice = Css.eval_declaration ctx once in
  check_eval_shape "eval idempotency shape" decl once;
  check_same_decl "eval idempotency" once twice

let test_eval_context_monotonicity buf =
  let weak = eval_ctx buf in
  let strong =
    eval_ctx
      ~viewport_height:
        (Css.Values.Px (float_of_int (480 + (byte_at buf 9 mod 900))))
      buf
  in
  let decl = fuzz_decl buf in
  let direct = Css.eval_declaration strong decl in
  let staged = Css.eval_declaration strong (Css.eval_declaration weak decl) in
  check_eval_shape "eval context monotonicity direct shape" decl direct;
  check_eval_shape "eval context monotonicity staged shape" decl staged;
  check_same_decl "eval context monotonicity" direct staged

let test_eval_conservativity buf =
  let ctx =
    eval_ctx
      ~viewport_height:
        (Css.Values.Px (float_of_int (480 + (byte_at buf 10 mod 900))))
      buf
  in
  let decl = conservative_decl buf in
  let actual = Css.eval_declaration ctx decl in
  check_eval_shape "eval conservativity shape" decl actual;
  check_same_decl "eval conservativity" decl actual

let test_full_context_observables buf =
  let ctx =
    eval_ctx
      ~viewport_height:
        (Css.Values.Px (float_of_int (480 + (byte_at buf 42 mod 900))))
      buf
  in
  let decl = computable_decl buf in
  let actual = Css.eval_declaration ctx decl in
  check_eval_shape "eval full-context conservativity shape" decl actual;
  let rendered = pp_decl actual in
  if contains_residual rendered then
    failf "eval full-context conservativity left residual in %S" rendered

let test_layered_eval_laws buf =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let weak = eval_layered_ctx buf in
  let strong =
    eval_layered_ctx
      ~viewport_height:
        (Css.Values.Px (float_of_int (480 + (byte_at buf 11 mod 900))))
      buf
  in
  let decl = fuzz_layered_decl buf in
  let once = Css.eval_declaration weak ~layer_order ~layer:"utilities" decl in
  let twice = Css.eval_declaration weak ~layer_order ~layer:"utilities" once in
  check_eval_shape "layered eval idempotency shape" decl once;
  check_same_decl "layered eval idempotency" once twice;
  let direct =
    Css.eval_declaration strong ~layer_order ~layer:"utilities" decl
  in
  let staged =
    Css.eval_declaration strong ~layer_order ~layer:"utilities" once
  in
  check_eval_shape "layered eval context monotonicity direct shape" decl direct;
  check_eval_shape "layered eval context monotonicity staged shape" decl staged;
  check_same_decl "layered eval context monotonicity" direct staged

let test_stylesheet_eval_laws buf =
  let weak = eval_ctx buf in
  let strong =
    eval_ctx
      ~viewport_height:
        (Css.Values.Px (float_of_int (480 + (byte_at buf 43 mod 900))))
      buf
  in
  let stylesheet = fuzz_stylesheet buf in
  let once = Css.eval_stylesheet weak stylesheet in
  let twice = Css.eval_stylesheet weak once in
  check_same_stylesheet "stylesheet eval idempotency" once twice;
  let direct = Css.eval_stylesheet strong stylesheet in
  let staged = Css.eval_stylesheet strong once in
  check_same_stylesheet "stylesheet eval context monotonicity" direct staged

let suite =
  ( "context",
    [
      test_case "empty contexts" [ bytes ] test_empty_contexts;
      test_case "property value context" [ bytes ] test_property_value_context;
      test_case "document context" [ bytes ] test_document_context;
      test_case "query context" [ bytes ] test_query_context;
      test_case "loader and animation context" [ bytes ]
        test_loader_animation_context;
      test_case "property registration context" [ bytes ]
        test_property_registration_context;
      test_case "context printers" [ bytes ] test_context_printers;
      test_case "eval idempotency law" [ bytes ] test_eval_idempotent;
      test_case "eval context monotonicity law" [ bytes ]
        test_eval_context_monotonicity;
      test_case "eval conservativity law" [ bytes ] test_eval_conservativity;
      test_case "eval full-context conservativity law" [ bytes ]
        test_full_context_observables;
      test_case "layered eval laws" [ bytes ] test_layered_eval_laws;
      test_case "stylesheet eval laws" [ bytes ] test_stylesheet_eval_laws;
    ] )
