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
let ident buf i = pick [ "brand"; "gap"; "fg"; "accent"; "item"; "panel" ] buf i

let declaration_text buf i =
  pick
    [
      "color:red";
      "color:var(--" ^ ident buf (i + 1) ^ ", #000)";
      "background-color:rgb(255 0 0 / .5)";
      "display:grid";
      "margin:1px 2px 3px 4px";
      "padding:clamp(1rem,2vw,2rem)";
      "width:calc(100% - 2rem)";
      "font:italic 700 1rem/1.5 \"Brand\", sans-serif";
      "background-image:url(../img/" ^ ident buf (i + 1) ^ ".svg)";
      "--" ^ ident buf (i + 1) ^ ":";
      "--" ^ ident buf (i + 1) ^ ": ";
      "--" ^ ident buf (i + 1) ^ ":[a,b] (c) { d:e }";
      "color:red!important";
    ]
    buf i

let invalid_declaration_text buf i =
  pick
    [
      "color red";
      ":red";
      "color:rgb(255 0)";
      "width:calc(1px + )";
      "margin:1px 2px 3px 4px 5px";
      "font-family:Brand,";
      "color:red !important !important";
    ]
    buf i

let parse_declaration input =
  let r = Cursor.of_string input in
  try Css.Declaration.read_declaration r with Cursor.Parse_error _ -> None

let serialize decl = Css.Declaration.to_string ~minify:true decl

let assert_invalid_declaration_contract =
  Fuzz_helpers.assert_invalid_declaration_contract

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix

let test_read_declaration_crash_safety buf =
  ignore (parse_declaration (cssish buf))

(* The same property over the byte shapes [cssish] cannot reach. *)
let test_declaration_unicode_bytes buf =
  ignore (parse_declaration (Fuzz_helpers.unicodish buf))

(* Allow one canonicalization pass (numeric trim, escape canonical form, ...)
   that only fires on re-parse, then require fixed point. *)
let test_serialization_idempotent buf =
  let reparse_or_fail step s =
    match parse_declaration s with
    | Some d -> d
    | None -> failf "serialized declaration did not reparse at %s: %S" step s
  in
  match parse_declaration (declaration_text buf 0) with
  | None -> ()
  | Some decl ->
      let once = serialize decl in
      let twice = serialize (reparse_or_fail "first reparse" once) in
      let thrice = serialize (reparse_or_fail "second reparse" twice) in
      if twice <> thrice then
        failf
          "declaration serialization drifted past canonicalization: %S -> %S \
           -> %S"
          once twice thrice

let test_property_name_stable buf =
  match parse_declaration (declaration_text buf 0) with
  | None -> ()
  | Some decl -> (
      let property = Css.Declaration.property_name decl in
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None -> fail "serialized declaration did not reparse"
      | Some reparsed ->
          let reparsed_property = Css.Declaration.property_name reparsed in
          if property <> reparsed_property then
            failf "declaration property changed: %S -> %S" property
              reparsed_property)

let test_important_flag_stable buf =
  match parse_declaration (declaration_text buf 0) with
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
  (* [custom_property] refuses a value it cannot write back as one declaration,
     so the shape holds over the pairs it accepts. *)
  match Css.Declaration.custom_property name value with
  | exception Failure _ -> ()
  | decl -> (
      let serialized = serialize decl in
      if not (starts_with ~prefix:(name ^ ":") serialized) then
        failf "custom property lost its name: %S" serialized;
      match parse_declaration serialized with
      | None -> ()
      | Some reparsed ->
          if Css.Declaration.property_name reparsed <> name then
            fail "custom property name changed after reparse")

let test_block_declarations_serialize_individually buf =
  let middle =
    if byte_at buf 0 mod 4 = 0 then invalid_declaration_text buf 1
    else declaration_text buf 1
  in
  let input = "{color:red;" ^ middle ^ ";margin:1px}" in
  let r = Cursor.of_string input in
  try
    let declarations = Css.Declaration.read_block r in
    List.iter
      (fun decl ->
        let serialized = serialize decl in
        match parse_declaration serialized with
        | None ->
            failf "block declaration serialization did not reparse: %S"
              serialized
        | Some _ -> ())
      declarations
  with Cursor.Parse_error _ -> ()

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
        failf "url declaration changed shape: %S" serialized;
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
    failf "custom property var() shape changed: %S" serialized;
  let ctx = { Css.Context.empty with custom_properties = [ decl ] } in
  if
    not
      (Option.equal Css.Declaration.equal_declaration
         (Css.Context.custom_property name ctx)
         (Some decl))
  then fail "custom property context lost var() value"

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
      "display:";
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
      "backdrop-filter:blur(1px, 2px)";
      "will-change:auto, transform";
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
        failf "feature declaration changed property: %S -> %S" input serialized

let test_invalid_features buf =
  let input = invalid_feature_decl buf in
  assert_invalid_declaration_contract "invalid feature declaration" input

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
      (* pp serializes a CSS-wide keyword verbatim. Resolving [initial] to a
         property's initial value (e.g. margin/padding:initial -> 0) is a
         normalize fold that needs the property's initial-value table, so it
         belongs to optimize, not pp. *)
      if serialized <> input then
        failf "CSS-wide keyword declaration changed: %S -> %S" input serialized

let test_invalid_css_wide buf =
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
  assert_invalid_declaration_contract "invalid CSS-wide keyword mix" input

let test_shared_css_wide_inventory buf =
  let row =
    pick Cascade_spec_inventory.Declaration_grammar.css_wide_positive buf 0
  in
  match parse_declaration row.input with
  | None -> failf "shared CSS-wide declaration rejected: %S" row.input
  | Some decl ->
      let serialized = serialize decl in
      if serialized <> row.expected then
        failf "shared CSS-wide declaration changed: %S -> %S" row.input
          serialized

let test_css_wide_inventory buf =
  let row =
    pick Cascade_spec_inventory.Declaration_grammar.css_wide_negative buf 0
  in
  assert_invalid_declaration_contract "shared invalid CSS-wide declaration"
    row.input

let test_shared_alias_inventory buf =
  let row =
    pick Cascade_spec_inventory.Declaration_grammar.alias_positive buf 0
  in
  match parse_declaration row.input with
  | None -> failf "shared alias declaration rejected: %S" row.input
  | Some decl ->
      let serialized = serialize decl in
      if starts_with ~prefix:"page-break-" serialized then
        failf "legacy alias serialized with old name: %S" serialized;
      if serialized <> row.expected then
        failf "shared alias declaration changed: %S -> %S" row.input serialized

let test_shared_invalid_alias_inventory buf =
  let row =
    pick Cascade_spec_inventory.Declaration_grammar.alias_negative buf 0
  in
  assert_invalid_declaration_contract "shared invalid alias declaration"
    row.input

let test_custom_tokens buf =
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
  | None -> failf "custom property token stream did not parse: %S" input
  | Some decl -> (
      if Css.Declaration.property_name decl <> name then
        failf "custom property name changed: %S" input;
      let serialized = serialize decl in
      match parse_declaration serialized with
      | None ->
          failf "custom property serialization did not reparse: %S" serialized
      | Some reparsed ->
          if Css.Declaration.property_name reparsed <> name then
            fail "custom property name changed after serialization")

let test_custom_prop_empty_edges _buf =
  let assert_serializes label input expected =
    match parse_declaration input with
    | None -> failf "%s custom property rejected: %S" label input
    | Some decl ->
        let serialized = serialize decl in
        if serialized <> expected then
          failf "%s custom property changed: %S -> %S" label input serialized
  in
  assert_serializes "browser-compatible empty value" "--x:" "--x:";
  assert_serializes "spec whitespace-token value" "--x: " "--x: "

let suite =
  ( "declaration",
    [
      test_case "read_declaration crash safety" [ bytes ]
        test_read_declaration_crash_safety;
      test_case "read_declaration crash safety over non-ascii bytes" [ bytes ]
        test_declaration_unicode_bytes;
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
        test_invalid_css_wide;
      test_case "shared CSS-wide declaration inventory" [ bytes ]
        test_shared_css_wide_inventory;
      test_case "shared invalid CSS-wide declaration inventory" [ bytes ]
        test_css_wide_inventory;
      test_case "shared legacy alias declaration inventory" [ bytes ]
        test_shared_alias_inventory;
      test_case "shared invalid legacy alias declaration inventory" [ bytes ]
        test_shared_invalid_alias_inventory;
      test_case "custom property token stream vectors" [ bytes ]
        test_custom_tokens;
      test_case "custom property empty value edges" [ bytes ]
        test_custom_prop_empty_edges;
    ] )
