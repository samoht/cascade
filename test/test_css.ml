(** High-level CSS integration tests

    Tests the integration between different CSS modules and end-to-end CSS
    generation using only the public Css module interface.

    Detailed module functionality is tested in dedicated files:
    - test_values.ml - CSS value types
    - test_properties.ml - CSS properties
    - test_selector.ml - CSS selectors
    - test_declaration.ml - CSS declarations
    - test_stylesheet.ml - Stylesheet construction
    - test_optimize.ml - CSS optimization
    - test_variables.ml - CSS variables
    - test_pp.ml - Pretty printing *)

open Cascade
open Css
open Css_helpers

let parse_with_source ?(strict = false) source =
  match Css.of_string ~strict ~preserve_source:true source with
  | Ok ({ source = Some source; _ } as parsed) -> (parsed, source)
  | Ok { source = None; _ } ->
      Alcotest.fail "source-preserving parse returned no source snapshot"
  | Error error ->
      Alcotest.failf "source-preserving parse rejected %S: %s" source
        (Error.to_string error)

let source_fidelity_roundtrip_and_locations () =
  let original = "\xEF\xBB\xBF/* lead */\r\n.a{color:rgb(300)}\r\n/* tail */" in
  let preprocessed = "/* lead */\n.a{color:rgb(300)}\n/* tail */" in
  (match Css.of_string original with
  | Ok { source = None; _ } -> ()
  | Ok { source = Some _; _ } ->
      Alcotest.fail "ordinary parse unexpectedly retained source"
  | Error error -> Alcotest.fail (Error.to_string error));
  let parsed, source = parse_with_source original in
  Alcotest.(check string)
    "exact caller bytes round-trip" original
    (Css.Source.contents source);
  Alcotest.(check string)
    "CSS preprocessing is inspectable" preprocessed
    (Css.Source.preprocessed source);
  (match Css.Source.comments source with
  | [ leading; trailing ] ->
      Alcotest.(check string)
        "leading comment bytes" "/* lead */"
        (Css.Source.slice source leading.loc);
      Alcotest.(check bool) "leading comment terminated" true leading.terminated;
      Alcotest.(check string)
        "leading comment original bytes" "/* lead */"
        (Css.Source.original_slice source leading.loc);
      Alcotest.(check string)
        "leading comment original location" "[3-13]"
        (Loc.to_string (Css.Source.original_loc source leading.loc));
      Alcotest.(check string)
        "trailing comment bytes" "/* tail */"
        (Css.Source.slice source trailing.loc)
  | comments ->
      Alcotest.failf "expected two comments, got %d" (List.length comments));
  (match Css.Source.rules source with
  | [ rule ] ->
      Alcotest.(check string)
        "semantic rule location" "[11-29]" (Loc.to_string rule.loc);
      Alcotest.(check string)
        "rule owns its leading trivia" "[0-29]"
        (Loc.to_string rule.owned_loc);
      let start = Css.Source.position source rule.loc.start_pos in
      Alcotest.(check int) "original byte offset" 15 start.byte;
      Alcotest.(check int) "line" 2 start.line;
      Alcotest.(check int) "column" 1 start.column
  | rules ->
      Alcotest.failf "expected one syntax rule, got %d" (List.length rules));
  Alcotest.(check bool) "typed diagnostic retained" true (parsed.warnings <> []);
  let warning = List.hd parsed.warnings in
  let span = Css.Source.span source (Error.context warning).Loc.Context.loc in
  Alcotest.(check int) "diagnostic source line" 2 span.start.line;
  let _, replaced = parse_with_source "\x00a{}" in
  Alcotest.(check string)
    "NUL is visible in caller bytes" "\x00a{}"
    (Css.Source.contents replaced);
  Alcotest.(check string)
    "NUL is preprocessed to replacement character" "\xEF\xBF\xBDa{}"
    (Css.Source.preprocessed replaced);
  Alcotest.(check string)
    "replacement range maps back to one caller byte" "[0-1]"
    (Loc.to_string
       (Css.Source.original_loc replaced (Loc.v ~start_pos:0 ~end_pos:3)))

let source_fidelity_owns_all_trivia () =
  let input = " /* before a */ .a{}\n/* before b */ .b{} /* after */ " in
  let _, source = parse_with_source input in
  let owned =
    List.map
      (fun (rule : Css.Source.rule) -> Css.Source.slice source rule.owned_loc)
      (Css.Source.rules source)
  in
  let trailing = Css.Source.slice source (Css.Source.trailing_loc source) in
  Alcotest.(check string)
    "rule-owned text plus trailing trivia partitions the source" input
    (String.concat "" (owned @ [ trailing ]));
  let _, unterminated = parse_with_source "a{}/* tail" in
  (match Css.Source.comments unterminated with
  | [ comment ] ->
      Alcotest.(check bool)
        "unterminated comment recorded" false comment.terminated;
      Alcotest.(check string)
        "unterminated comment bytes" "/* tail"
        (Css.Source.slice unterminated comment.loc)
  | comments ->
      Alcotest.failf "expected one unterminated comment, got %d"
        (List.length comments));
  let _, strings =
    parse_with_source "a{content:\"/* not a comment */\"}/* actual */"
  in
  (match Css.Source.comments strings with
  | [ comment ] ->
      Alcotest.(check string)
        "comment-looking string bytes stay inside the string" "/* actual */"
        (Css.Source.slice strings comment.loc)
  | comments ->
      Alcotest.failf "expected only the actual comment, got %d"
        (List.length comments));
  let _, unchanged = parse_with_source "a{}" in
  Alcotest.(check bool)
    "unchanged input buffer is shared" true
    (Css.Source.contents unchanged == Css.Source.preprocessed unchanged)

let source_fidelity_is_an_immutable_snapshot () =
  let nested, nested_source =
    parse_with_source ".a{color:red;& .b{color:blue}}"
  in
  let flattened = Css.flatten_nesting nested.stylesheet in
  Alcotest.(check int)
    "one authored syntax rule" 1
    (List.length (Css.Source.rules nested_source));
  Alcotest.(check int)
    "flattening splits the typed rule" 2 (List.length flattened);
  Alcotest.(check string)
    "split transform leaves source snapshot unchanged"
    ".a{color:red;& .b{color:blue}}"
    (Css.Source.contents nested_source);
  let adjacent, adjacent_source =
    parse_with_source ".a{color:red}.b{color:red}"
  in
  let merged = Css.optimize adjacent.stylesheet in
  Alcotest.(check int)
    "two authored syntax rules" 2
    (List.length (Css.Source.rules adjacent_source));
  Alcotest.(check int) "optimization merges typed rules" 1 (List.length merged);
  Alcotest.(check string)
    "merge transform leaves source snapshot unchanged"
    ".a{color:red}.b{color:red}"
    (Css.Source.contents adjacent_source)

(* Test end-to-end CSS generation *)
let generation () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ color (Css.Values.hex "ff0000"); padding [ Px 10. ] ];
        rule ~selector:card
          [ margin [ Px 5. ]; background_color (Css.Values.hex "ffffff") ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "exact css generation"
    ".btn{color:#f00;padding:10px}.card{margin:5px;background-color:#fff}" css;
  Alcotest.(check string)
    "generation optimize+minify"
    ".btn{color:red;padding:10px}.card{margin:5px;background-color:#fff}"
    (minify stylesheet)

(* Test optimization flag works *)
let optimization_flag () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            color (Css.Values.hex "ff0000");
            color (Css.Values.hex "0000ff");
            (* duplicate - should be optimized *)
          ];
      ]
  in

  let css_optimized = minify stylesheet in
  Alcotest.(check string) "optimized exact" ".btn{color:#00f}" css_optimized

let nonempty_declaration_lists () =
  let rejects_empty name helper =
    Alcotest.check_raises name
      (Invalid_argument (String.concat "" [ name; ": empty list" ]))
      (fun () -> ignore (helper []))
  in
  rejects_empty "padding" padding;
  rejects_empty "margin" margin;
  rejects_empty "padding_inline" padding_inline;
  rejects_empty "padding_block" padding_block;
  rejects_empty "inset" inset;
  rejects_empty "inset_inline" inset_inline;
  rejects_empty "inset_block" inset_block;
  rejects_empty "text_shadows" text_shadows;
  rejects_empty "transitions" transitions;
  rejects_empty "transforms" transforms;
  rejects_empty "box_shadows" box_shadows;
  rejects_empty "background_position" background_position;
  rejects_empty "webkit_mask_position" webkit_mask_position;
  rejects_empty "mask_position" mask_position;
  rejects_empty "scroll_margin" scroll_margin;
  rejects_empty "scroll_margin_inline" scroll_margin_inline;
  rejects_empty "scroll_margin_block" scroll_margin_block;
  rejects_empty "scroll_padding" scroll_padding;
  rejects_empty "scroll_padding_inline" scroll_padding_inline;
  rejects_empty "scroll_padding_block" scroll_padding_block;
  rejects_empty "overscroll_behavior" overscroll_behavior;
  rejects_empty "font_families" font_families

let grid_template_areas_values () =
  let render value =
    Css.Declaration.to_string ~minify:true (grid_template_areas value)
  in
  Alcotest.(check string) "none" "grid-template-areas:none" (render No_areas);
  Alcotest.(check string)
    "areas" "grid-template-areas:\"a\"" (render (Areas "\"a\""));
  Alcotest.(check string)
    "global keyword" "grid-template-areas:initial" (render Initial)

(* Test layers work end-to-end *)
let layers_integration () =
  let utility_rule = rule ~selector:btn [ padding [ Px 10. ] ] in
  let stylesheet = Css.v [ layer ~name:[ "utilities" ] [ utility_rule ] ] in

  let css = Css.to_string ~minify:true stylesheet in

  (* Should contain @layer *)
  Alcotest.(check bool)
    "contains @layer" true
    (Astring.String.is_infix ~affix:"@layer" css);
  Alcotest.(check bool)
    "contains layer name" true
    (Astring.String.is_infix ~affix:"utilities" css)

(* Test media queries work end-to-end *)
let media_integration () =
  let mobile_rule = rule ~selector:btn [ font_size (Rem 0.875) ] in
  let stylesheet =
    Css.v
      [
        media
          ~condition:(Css.Media.of_string "(max-width: 640px)")
          [ mobile_rule ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "media exact" "@media(max-width:640px){.btn{font-size:.875rem}}" css

(* Test minify flag *)
let minify_flag () =
  let stylesheet =
    v [ rule ~selector:btn [ color (Css.Values.hex "ff0000") ] ]
  in

  let css_minified = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string) "minified exact" ".btn{color:#f00}" css_minified;
  Alcotest.(check string)
    "minified optimize+minify" ".btn{color:red}" (minify stylesheet)

let pure_minify_value_fallbacks () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [
            Css.Variables.typed_custom_property "--tw-duration" Time (Ms 200.);
            transition_duration (Ms 200.);
            color
              (Hsl { h = Angle (Deg 120.); s = Pct 50.; l = Pct 50.; a = None });
          ];
      ]
  in
  Alcotest.(check string)
    "constructed value fallbacks"
    ".btn{--tw-duration:.2s;transition-duration:.2s;color:hsl(120 50%50%)}"
    (Css.to_string ~minify:true stylesheet)

(* [to_string ~minify:true] is a pure formatter, so a constructed AST reaches
   the printer without the normalize pass. A keyword whose spec-equivalent value
   the optimiser substitutes must therefore survive pure minification verbatim:
   holding [initial] is always correct, whereas printing the wrong equivalent
   would change layout for a consumer that never calls [Css.optimize]. Every
   test around the initial-value fold otherwise goes through the parser, which
   runs normalization. *)
let constructed_initial_keyword_fold () =
  let sheet keyword =
    v [ rule ~selector:btn [ min_inline_size keyword; min_block_size keyword ] ]
  in
  Alcotest.(check string)
    "pure minify holds the initial keyword"
    ".btn{min-inline-size:initial;min-block-size:initial}"
    (Css.to_string ~minify:true (sheet Initial));
  (* CSS Logical 1 sec. 4 gives each logical minimum-size property and its
     physical counterpart one shared computed value, so [initial] resolves
     through min-width / min-height to CSS Sizing 3 sec. 3.1.2's [auto]. *)
  Alcotest.(check string)
    "optimize folds the keyword to auto"
    ".btn{min-inline-size:auto;min-block-size:auto}"
    (minify (sheet Initial));
  Alcotest.(check string)
    "both phases hold an explicit auto"
    ".btn{min-inline-size:auto;min-block-size:auto}"
    (Css.to_string ~minify:true (sheet Auto))

(* Motion Path 1 (ED) sec. 2.6 puts a [!] on the leading group of the [offset]
   shorthand, so no combination of slots may serialise to an empty value and
   none may serialise to a value that reads back as a different one.
   [Css.to_string ~minify:true] never runs the normalizers, so the printer has
   to hold that on its own for a caller that builds the AST rather than parsing
   it. *)
let constructed_offset_shorthand () =
  let offset value = Css.Declaration.v Css.Properties.Offset value in
  let empty_slots : Css.Properties.offset =
    Shorthand
      {
        target =
          With_path
            {
              position = None;
              path = (None : Css.Properties.offset_path);
              distance = None;
              rotate = None;
            };
        anchor = None;
      }
  in
  let initial_slots : Css.Properties.offset =
    Shorthand
      {
        target = Position_only (Normal : Css.Properties.offset_position);
        anchor = Some (Auto : Css.Properties.offset_anchor);
      }
  in
  let stylesheet =
    v
      [
        rule ~selector:btn [ offset empty_slots ];
        rule ~selector:card [ offset initial_slots ];
      ]
  in
  let printed = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "constructed offset shorthand" ".btn{offset:none}.card{offset:normal/auto}"
    printed;
  (* The printed sheet reads back as the sheet that printed it. *)
  match Css.of_string ~strict:true printed with
  | Ok { stylesheet; _ } ->
      Alcotest.(check string)
        "constructed offset shorthand reparses" printed
        (String.trim (Css.to_string ~minify:true stylesheet))
  | Error e -> Alcotest.failf "%s: %s" printed (Error.to_string e)

(* CSS Syntax 3 (ED) sec. 4 ends a percentage-token at the [%], so
   [oklch(63.7%.237 25.331)] and [oklch(63.7% .237 25.331)] tokenise to one
   stream and [Css.to_string ~minify:true] may spell the boundary either way. It
   may not spell it both ways: minified output is the printer's canonical form,
   so reading it back and printing it again has to give the same bytes.
   [Css.var] binds a typed value where the reader keeps an opaque token stream,
   so both sides of the printer answer the same question here. *)
let constructed_custom_property_reparses () =
  let modes =
    [
      ("default", false, false);
      ("lossless", true, false);
      ("enforce-spec", false, true);
    ]
  in
  let fixed_point label sheet =
    List.iter
      (fun (mode, lossless, enforce_spec) ->
        let printed =
          Css.to_string ~minify:true ~lossless ~enforce_spec sheet
        in
        let again =
          Css.to_string ~minify:true ~lossless ~enforce_spec
            (Css.of_string_exn printed)
        in
        Alcotest.(check string)
          (String.concat "" [ label; " is its own fixed point ("; mode; ")" ])
          printed again)
      modes
  in
  let decl, _ = Css.var "brand" Css.Color (Css.oklch 63.7 0.237 25.331) in
  fixed_point "constructed custom property" (v [ rule ~selector:btn [ decl ] ]);
  fixed_point "parsed custom property"
    (Css.of_string_exn ".btn{--brand:oklch(63.7% .237 25.331)}");
  (* A parsed typed colour is its own fixed point: the typed printer and the
     custom-value serialiser have to agree on the percentage boundary. *)
  fixed_point "typed percentage pair"
    (Css.of_string_exn ".btn{color:hsl(120 50% 50%)}");
  (* [%] closes its token (CSS Syntax 3 (ED) sec. 4), so a constructed colour
     has to spell the boundary after a percentage the way a reparse of its own
     bytes does. *)
  List.iter
    (fun (name, color) ->
      let decl, _ = Css.var name Css.Color color in
      fixed_point
        (String.concat "" [ "constructed "; name ])
        (v [ rule ~selector:btn [ decl ] ]))
    [
      ("hsl", Css.hsl 120. 50. 50.);
      ("hwb", Css.hwb 120. 30. 40.);
      ("hsla", Css.hsla 120. 50. 50. 0.5);
      ("hwba", Css.hwba 120. 30. 40. 0.5);
      ( "rgb-percentage-channels",
        Css.Values.Rgb (Channels { r = Pct 50.; g = Pct 60.; b = Pct 70. }) );
      ("oklch-percentage-chroma", Css.oklch 50. 0.304 120.);
      ( "oklch-none-chroma",
        Css.Values.Oklch
          {
            l = Some (Pct 50.);
            c = Option.None;
            h = Unitless 120.;
            alpha = None;
          } );
      ( "oklab-none-axis",
        Css.Values.Oklab
          { l = Some (Pct 50.); a = Option.None; b = Some 0.12; alpha = None }
      );
      ( "rgb-var-channel",
        Css.Values.Rgb
          (Channels
             { r = Pct 50.; g = Var (Css.Values.var_ref "g"); b = Pct 70. }) );
      (* The other side of the same boundary: a [var()] closes on [)], which the
         custom-value serialiser keeps separated, so that gap stays. *)
      ( "hsl-var-saturation",
        Css.Values.Hsl
          {
            h = Unitless 120.;
            s = Var (Css.Values.var_ref "s");
            l = Pct 50.;
            a = None;
          } );
    ];
  (* A custom property never comes out longer than it went in. [%] closes the
     percentage, so the reader has no boundary to restore. *)
  List.iter
    (fun source ->
      let printed = Css.to_string ~minify:true (Css.of_string_exn source) in
      Alcotest.(check string)
        "minified custom property keeps its spelling" source printed;
      fixed_point "tight custom property" (Css.of_string_exn source))
    [ ":root{--x:10%5px}"; ":root{--x:50%.5}" ];
  (* Pretty output is the upper bound on minified output for the same node. *)
  List.iter
    (fun source ->
      let sheet = Css.of_string_exn source in
      let minified = Css.to_string ~minify:true sheet in
      let pretty = Css.to_string sheet in
      if String.length minified > String.length pretty then
        Alcotest.failf "minified %S is longer than pretty %S" minified pretty)
    [
      ":root{--x:10%5px}";
      ":root{--x:50%.5}";
      ".btn{--brand:oklch(63.7% .237 25.331)}";
      ".btn{color:hsl(120 50% 50%)}";
      ".btn{color:hwb(120 30% 40%)}";
    ]

let explicit_phase_pipeline () =
  let stylesheet =
    v
      [
        rule ~selector:(Selector.class_ "foo") [ color (hex "#0000ff") ];
        rule ~selector:(Selector.class_ "bar") [ color (hex "#0000ff") ];
      ]
  in
  Alcotest.(check string)
    "to_string minifies tokens without optimizing AST shape"
    ".foo{color:#00f}.bar{color:#00f}"
    (Css.to_string ~minify:true stylesheet);
  Alcotest.(check string)
    "optimize is the explicit AST optimization phase" ".bar,.foo{color:#00f}"
    (minify stylesheet);
  let theme = Css.Pp.String_set.(empty |> add "brand") in
  let guarded =
    v
      [
        rule ~selector:(Selector.class_ "card")
          [
            theme_guarded ~var_name:"brand" (color (hex "#ff0000"));
            background_color (hex "#ffffff");
          ];
      ]
  in
  Alcotest.(check string)
    "to_string does not resolve theme guards"
    ".card{color:#f00;background-color:#fff}"
    (Css.to_string ~minify:true guarded);
  Alcotest.(check string)
    "resolve_theme is the explicit theme phase"
    ".card{color:#f00;background-color:#fff}"
    (guarded |> Css.resolve_theme ~theme |> Css.to_string ~minify:true);
  Alcotest.(check string)
    "resolve_theme output can still optimize"
    ".card{color:red;background-color:#fff}"
    (guarded |> Css.resolve_theme ~theme |> minify);
  Alcotest.(check string)
    "resolve_theme can drop inactive theme guards"
    ".card{background-color:#fff}"
    (guarded
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty
    |> Css.to_string ~minify:true);
  let spacing_decl, spacing = var "spacing" Length (Rem 0.25) in
  let var_sheet =
    v
      [
        rule ~selector:(Selector.class_ "p-1")
          [ spacing_decl; padding [ Var spacing ] ];
      ]
  in
  Alcotest.(check string)
    "to_string does not inline vars"
    ".p-1{--spacing:.25rem;padding:var(--spacing)}"
    (Css.to_string ~minify:true var_sheet);
  Alcotest.(check string)
    "inline_vars is the explicit variable substitution phase"
    ".p-1{padding:.25rem}"
    (var_sheet |> Css.inline_vars |> Css.to_string ~minify:true)

(* Test important declarations *)
let important_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ important (color (Css.Values.hex "ff0000")); padding [ Px 10. ] ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "important exact" ".btn{color:#f00!important;padding:10px}" css;
  Alcotest.(check string)
    "important optimize+minify" ".btn{color:red!important;padding:10px}"
    (minify stylesheet)

(* Test custom properties integration *)
let custom_properties_integration () =
  let stylesheet =
    v
      [
        rule ~selector:btn
          [ custom_property "--primary-color" "blue"; color (Named Blue) ];
      ]
  in

  let css = Css.to_string ~minify:true stylesheet in
  Alcotest.(check string)
    "custom properties exact" ".btn{--primary-color:blue;color:blue}" css;
  Alcotest.(check string)
    "custom properties optimize+minify" ".btn{--primary-color:blue;color:#00f}"
    (minify stylesheet)

(* Regression: a custom property name starting with a digit after the -- is a
   valid dashed-ident per CSS Values 4 sec. 4.3. Tailwind emits these for
   arbitrary-value classes like text-[1A202C]. *)
let var_digit_after_dashes () =
  let css = ".x{font-size:var(--1A202C)}" in
  match Css.of_string ~strict:false css with
  | Error err -> Alcotest.fail ("parse failed: " ^ Cascade.Error.to_string err)
  | Ok parsed ->
      let out = Css.to_string ~minify:true parsed.stylesheet in
      Alcotest.(check string) "var(--1A202C) roundtrip" css out

(* CSS Roundtrip Test: Parse generated CSS and compare roundtrip *)
let roundtrip () =
  let read_file path =
    let ic = open_in path in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () -> really_input_string ic (in_channel_length ic))
  in
  let original_css =
    let candidates =
      [ "examples/empty_tailwind.css"; "test/examples/empty_tailwind.css" ]
    in
    match List.find_opt Sys.file_exists candidates with
    | Some path -> read_file path
    | None ->
        Alcotest.fail
          "Could not find empty_tailwind.css (tried examples/ and \
           test/examples/)"
  in

  (* Parse-then-minify must be an idempotent fixed point: a second pass over the
     minified output should produce byte-identical CSS. The input fixture comes
     from Tailwind, whose valid minification choices do not have to match
     cascade's canonical printer byte for byte. *)
  let parse_or_fail label css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        let formatted_error = Cascade.Error.to_string err in
        Alcotest.fail ("Failed to parse " ^ label ^ ": " ^ formatted_error)
  in
  let first_pass =
    Css.to_string ~minify:true (parse_or_fail "input" original_css)
  in
  let second_pass =
    Css.to_string ~minify:true (parse_or_fail "first pass" first_pass)
  in
  if first_pass <> second_pass then
    match Cascade_diff.String_diff.first_diff_pos first_pass second_pass with
    | Some pos ->
        Fmt.epr "CSS roundtrip differs at position %d@." pos;
        Alcotest.fail "CSS roundtrip should be idempotent"
    | None -> Alcotest.fail "CSS roundtrip should be idempotent"
  else
    Alcotest.(check string)
      "CSS roundtrip should be idempotent" first_pass second_pass

let suite =
  ( "css",
    [
      (* Integration tests using public Css interface only *)
      Alcotest.test_case "CSS generation end-to-end" `Quick generation;
      Alcotest.test_case "source fidelity round-trip and locations" `Quick
        source_fidelity_roundtrip_and_locations;
      Alcotest.test_case "source fidelity owns all trivia" `Quick
        source_fidelity_owns_all_trivia;
      Alcotest.test_case "source fidelity snapshot survives transforms" `Quick
        source_fidelity_is_an_immutable_snapshot;
      Alcotest.test_case "optimization flag works" `Quick optimization_flag;
      Alcotest.test_case "non-empty declaration lists" `Quick
        nonempty_declaration_lists;
      Alcotest.test_case "grid-template-areas values" `Quick
        grid_template_areas_values;
      Alcotest.test_case "layers integration" `Quick layers_integration;
      Alcotest.test_case "media queries integration" `Quick media_integration;
      Alcotest.test_case "minify flag" `Quick minify_flag;
      Alcotest.test_case "pure minify value fallbacks" `Quick
        pure_minify_value_fallbacks;
      Alcotest.test_case "constructed offset shorthand" `Quick
        constructed_offset_shorthand;
      Alcotest.test_case "constructed initial keyword fold" `Quick
        constructed_initial_keyword_fold;
      Alcotest.test_case "constructed custom property reparses" `Quick
        constructed_custom_property_reparses;
      Alcotest.test_case "explicit phase pipeline" `Quick
        explicit_phase_pipeline;
      Alcotest.test_case "important declarations" `Quick important_integration;
      Alcotest.test_case "custom properties" `Quick
        custom_properties_integration;
      Alcotest.test_case "var(--1A202C) parses" `Quick var_digit_after_dashes;
      Alcotest.test_case "CSS roundtrip parsing" `Quick roundtrip;
    ] )
