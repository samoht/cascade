(** Fuzz tests for CSS declaration parsing and serialization. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let parse_declaration input =
  let r = Css.Cursor.of_string input in
  try Css.Declaration.read_declaration r with Css.Cursor.Parse_error _ -> None

let serialize decl = Css.Declaration.string_of_declaration ~minify:true decl

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let test_read_declaration_crash_safety buf =
  ignore (parse_declaration (cssish buf))

let test_serialization_idempotent buf =
  match parse_declaration (cssish buf) with
  | None -> ()
  | Some decl -> (
      let once = serialize decl in
      match parse_declaration once with
      | None -> fail (Fmt.str "serialized declaration did not reparse: %S" once)
      | Some reparsed ->
          let twice = serialize reparsed in
          if once <> twice then
            fail
              (Fmt.str "declaration serialization changed: %S -> %S" once twice)
      )

let test_property_name_stable buf =
  match parse_declaration (cssish buf) with
  | None -> ()
  | Some decl -> (
      let property = Css.Declaration.property_name decl in
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None -> fail "serialized declaration did not reparse"
      | Some reparsed ->
          let reparsed_property = Css.Declaration.property_name reparsed in
          if property <> reparsed_property then
            fail
              (Fmt.str "declaration property changed: %S -> %S" property
                 reparsed_property))

let test_important_flag_stable buf =
  match parse_declaration (cssish buf) with
  | None -> ()
  | Some decl -> (
      let important = Css.Declaration.is_important decl in
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None -> fail "serialized declaration did not reparse"
      | Some reparsed ->
          if important <> Css.Declaration.is_important reparsed then
            fail "declaration !important flag changed across serialization")

let test_custom_property_serialization_shape buf =
  let name =
    "--fuzz-"
    ^ string_of_int (if String.length buf = 0 then 0 else Char.code buf.[0])
  in
  let value = cssish buf in
  let decl = Css.Declaration.custom_property name value in
  let serialized = serialize decl in
  if not (starts_with ~prefix:(name ^ ":") serialized) then
    fail (Fmt.str "custom property lost its name: %S" serialized);
  match parse_declaration serialized with
  | None -> ()
  | Some reparsed ->
      if Css.Declaration.property_name reparsed <> name then
        fail "custom property name changed after reparse"

let test_block_declarations_serialize_individually buf =
  let input = "{color:red;" ^ cssish buf ^ ";margin:1px}" in
  let r = Css.Cursor.of_string input in
  try
    let declarations = Css.Declaration.read_block r in
    List.iter
      (fun decl ->
        let serialized = serialize decl in
        match parse_declaration serialized with
        | None ->
            fail
              (Fmt.str "block declaration serialization did not reparse: %S"
                 serialized)
        | Some _ -> ())
      declarations
  with Css.Cursor.Parse_error _ -> ()

let test_url_decl_local buf =
  let url =
    match
      Char.code (if String.length buf = 0 then '\000' else buf.[0]) mod 4
    with
    | 0 -> "../img/logo.svg"
    | 1 -> "#fragment"
    | 2 -> "https://cdn.example/font.woff2"
    | _ -> "data:image/svg+xml,%3Csvg%3E"
  in
  let input = "background-image:url(" ^ url ^ ")" in
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      let serialized = serialize decl in
      if not (starts_with ~prefix:"background-image:url(" serialized) then
        fail (Fmt.str "url declaration changed shape: %S" serialized);
      let ctx =
        {
          Css.Context.empty with
          base_url = Some "https://example.test/app.css";
        }
      in
      if ctx.base_url = None then fail "URL base context was not preserved"

let test_custom_cycle_context buf =
  let name =
    "--cycle-"
    ^ string_of_int (if String.length buf = 0 then 0 else Char.code buf.[0])
  in
  let specified = "var(" ^ name ^ ")" in
  let decl = Css.Declaration.custom_property name specified in
  let serialized = serialize decl in
  if not (starts_with ~prefix:(name ^ ":var(") serialized) then
    fail (Fmt.str "custom property var() shape changed: %S" serialized);
  let ctx = { Css.Context.empty with custom_properties = [ decl ] } in
  if Css.Context.custom_property name ctx <> Some decl then
    fail "custom property context lost var() value"

let feature_decl_vector buf =
  pick
    [
      ("display", "ruby");
      ("contain", "strict");
      ("content-visibility", "hidden");
      ("overflow-block", "scroll");
      ("scroll-snap-align", "start end");
      ("scroll-snap-stop", "always");
      ("columns", "12rem 3");
      ("column-rule", "1px solid currentColor");
      ("break-before", "page");
      ("background-clip", "padding-box");
      ("border-block", "1px solid red");
      ("text-decoration", "underline wavy red 2px");
      ("text-emphasis", "filled dot red");
      ("text-orientation", "mixed");
      ("font-optical-sizing", "auto");
      ("font-variant-caps", "small-caps");
      ("object-view-box", "inset(0 0 10% 0)");
      ("image-rendering", "pixelated");
      ("mask-size", "contain");
      ("backdrop-filter", "blur(4px) saturate(120%)");
      ("will-change", "transform, opacity");
      ("touch-action", "pan-x pinch-zoom");
      ("animation-composition", "add");
      ("scroll-timeline-name", "--scroller");
      ("position-visibility", "anchors-visible");
    ]
    buf 0

let invalid_feature_decl buf =
  pick
    [
      "display:ruby block";
      "contain:strict layout";
      "content-visibility:visible hidden";
      "overflow-block:visible hidden";
      "scroll-snap-align:start center end";
      "scroll-snap-stop:normal always";
      "columns:1 2 3";
      "column-rule:solid solid";
      "break-before:page column";
      "background-size:contain cover";
      "border-inline-color:red blue green";
      "text-decoration:underline none";
      "text-emphasis:filled open";
      "font-optical-sizing:auto none";
      "font-variant-caps:small-caps unicase";
      "object-view-box:inset()";
      "image-rendering:pixelated smooth";
      "mask-size:contain cover";
      "backdrop-filter:blur()";
      "will-change:auto, transform";
      "touch-action:pan-x pan-left";
      "animation-composition:add replace";
      "scroll-timeline-axis:block inline";
      "position-visibility:anchors-visible always";
    ]
    buf 0

let test_feature_decl_table buf =
  let property, value = feature_decl_vector buf in
  let input = property ^ ":" ^ value in
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      let serialized = serialize decl in
      if not (starts_with ~prefix:(property ^ ":") serialized) then
        fail
          (Fmt.str "feature declaration changed property: %S -> %S" input
             serialized)

let test_invalid_features buf =
  let input = invalid_feature_decl buf in
  match parse_declaration input with
  | None -> ()
  | Some decl -> ignore (serialize decl)

let test_css_wide_keyword_vectors buf =
  let property =
    pick [ "color"; "margin"; "padding"; "background"; "border"; "all" ] buf 0
  in
  let keyword =
    pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 1
  in
  let input = property ^ ":" ^ keyword in
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      let serialized = serialize decl in
      if serialized <> input then
        fail
          (Fmt.str "CSS-wide keyword declaration changed: %S -> %S" input
             serialized)

let test_invalid_css_wide_keyword_mixes buf =
  let keyword =
    pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 0
  in
  let input =
    pick
      [
        "color:" ^ keyword ^ " red";
        "margin:1px " ^ keyword;
        "padding:" ^ keyword ^ " 1px";
        "background:red " ^ keyword;
        "border:1px solid " ^ keyword;
        "all:" ^ keyword ^ " color";
      ]
      buf 1
  in
  match parse_declaration input with
  | None -> ()
  | Some decl -> ignore (serialize decl)

let test_custom_property_token_stream_vectors buf =
  let name = "--spec-" ^ string_of_int (byte_at buf 0) in
  let value =
    pick
      [
        "{ color: red }";
        "[a, b, c]";
        "var(--missing,)";
        "1 ! important";
        "url(foo;bar) trailing tokens";
      ]
      buf 1
  in
  let input = name ^ ":" ^ value in
  match parse_declaration input with
  | None ->
      fail (Fmt.str "custom property token stream did not parse: %S" input)
  | Some decl -> (
      if Css.Declaration.property_name decl <> name then
        fail (Fmt.str "custom property name changed: %S" input);
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None ->
          fail
            (Fmt.str "custom property serialization did not reparse: %S"
               serialized)
      | Some reparsed ->
          if Css.Declaration.property_name reparsed <> name then
            fail "custom property name changed after serialization")

let suite =
  ( "declaration",
    [
      test_case "read_declaration crash safety" [ bytes ]
        test_read_declaration_crash_safety;
      test_case "serialization idempotent" [ bytes ]
        test_serialization_idempotent;
      test_case "property name preserved after serialization" [ bytes ]
        test_property_name_stable;
      test_case "important flag preserved after serialization" [ bytes ]
        test_important_flag_stable;
      test_case "custom property serialization shape" [ bytes ]
        test_custom_property_serialization_shape;
      test_case "block declarations serialize individually" [ bytes ]
        test_block_declarations_serialize_individually;
      test_case "url declaration serialization stays local" [ bytes ]
        test_url_decl_local;
      test_case "custom property context invariant" [ bytes ]
        test_custom_cycle_context;
      test_case "feature declaration table" [ bytes ] test_feature_decl_table;
      test_case "invalid feature declarations rejected" [ bytes ]
        test_invalid_features;
      test_case "CSS-wide keyword declaration vectors" [ bytes ]
        test_css_wide_keyword_vectors;
      test_case "invalid CSS-wide keyword mixes rejected" [ bytes ]
        test_invalid_css_wide_keyword_mixes;
      test_case "custom property token stream vectors" [ bytes ]
        test_custom_property_token_stream_vectors;
    ] )
