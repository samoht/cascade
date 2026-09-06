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

(* Test helper: compose optimize + minified to_string the way [to_string
   ~minify:true] used to behave implicitly. *)
let minify s = s |> Css.optimize |> Css.to_string ~minify:true

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

(* Helper selectors for tests *)
let btn = Selector.class_ "btn"
let card = Selector.class_ "card"

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

(* Test AST introspection helpers *)
let test_layer_block () =
  let stylesheet =
    v
      [
        layer ~name:[ "theme" ] [ rule ~selector:btn [ color (hex "#ff0000") ] ];
        layer ~name:[ "utilities" ]
          [ rule ~selector:card [ padding [ Px 10. ] ] ];
        rule ~selector:(Selector.class_ "base") [ margin [ Px 5. ] ];
      ]
  in

  (* Test extracting theme layer *)
  let theme_stmts = layer_block [ "theme" ] stylesheet in
  Alcotest.(check bool) "theme layer found" true (Option.is_some theme_stmts);

  let theme_rules = theme_stmts |> Option.get |> rules_of_statements in
  Alcotest.(check int) "theme has one rule" 1 (List.length theme_rules);

  (* Test extracting non-existent layer *)
  let missing = layer_block [ "missing" ] stylesheet in
  Alcotest.(check bool) "missing layer not found" true (Option.is_none missing)

let test_rules_of_statements () =
  let stmts =
    [
      rule ~selector:btn [ color (hex "#ff0000") ];
      media
        ~condition:(Css.Media.of_string "(min-width: 768px)")
        [ rule ~selector:card [ padding [ Px 5. ] ] ];
      rule ~selector:card [ margin [ Px 10. ] ];
    ]
  in

  let rules = rules_of_statements stmts in
  Alcotest.(check int) "extracts 2 rules from statements" 2 (List.length rules);

  let selectors = List.map (fun (sel, _) -> Selector.to_string sel) rules in
  Alcotest.(check bool) "contains btn selector" true (List.mem ".btn" selectors);
  Alcotest.(check bool)
    "contains card selector" true
    (List.mem ".card" selectors)

let test_custom_prop_names () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let decls = [ color_def; margin_def; padding [ Px 10. ] ] in
  let prop_names = custom_prop_names decls in

  Alcotest.(check int) "finds 2 custom properties" 2 (List.length prop_names);
  Alcotest.(check bool)
    "contains primary-color" true
    (List.mem "--primary-color" prop_names);
  Alcotest.(check bool)
    "contains spacing" true
    (List.mem "--spacing" prop_names)

let test_custom_props_of_rules () =
  let color_def, _color_var = var "primary-color" Color (hex "#3b82f6") in
  let margin_def, _margin_var = var "spacing" Length (Px 8.) in

  let rules =
    [
      (btn, [ color_def; padding [ Px 10. ] ]);
      (card, [ margin_def; background_color (hex "#ffffff") ]);
    ]
  in

  let prop_names = custom_props_of_rules rules in

  Alcotest.(check int)
    "finds 2 custom properties total" 2 (List.length prop_names);
  Alcotest.(check bool)
    "contains primary-color" true
    (List.mem "--primary-color" prop_names);
  Alcotest.(check bool)
    "contains spacing" true
    (List.mem "--spacing" prop_names);

  (* Test order preservation *)
  Alcotest.(check string)
    "first property is primary-color" "--primary-color" (List.hd prop_names)

(* Test Css.map - transforms rules in statements *)
let test_map () =
  let sel1 = Selector.class_ "foo" in
  let sel2 = Selector.class_ "bar" in
  let stmts =
    [
      rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ];
      rule ~selector:sel2 [ color (Css.Values.hex "00ff00") ];
    ]
  in

  (* Map that changes all colors to blue *)
  let mapped =
    Css.map
      (fun sel _decls ->
        let new_decls = [ color (Css.Values.hex "0000ff") ] in
        rule ~selector:sel new_decls)
      stmts
  in

  let css = minify (v mapped) in
  Alcotest.(check string) "map changes all rules" ".bar,.foo{color:#00f}" css

(* Test Css.map with nested media queries *)
let test_map_nested () =
  let sel1 = Selector.class_ "foo" in
  let stmts =
    [
      media
        ~condition:(Css.Media.of_string "(min-width:768px)")
        [ rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ] ];
    ]
  in

  (* Map should descend into media query *)
  let mapped =
    Css.map
      (fun sel _decls ->
        let new_decls = [ color (Css.Values.hex "0000ff") ] in
        rule ~selector:sel new_decls)
      stmts
  in

  let css = Css.to_string ~minify:true (v mapped) in
  Alcotest.(check bool)
    "map descends into media" true
    (Astring.String.is_infix ~affix:"color:#00f" css)

(* Every conditional group at-rule holding [body]. [map] and [sort] speak of
   "all rules at all nesting levels", so each of these has to give the same
   answer as [@media]: they all wrap style rules, and which one wraps them is
   not something a caller rewriting or reordering rules asked about. *)
let conditional_groups body =
  [
    ("@media", Stylesheet.Media (Media.of_string "screen", body));
    ("@supports", Stylesheet.Supports (Supports.of_string "(top: 0)", body));
    ("@container", Stylesheet.Container (None, None, body));
    ("@layer", Stylesheet.Layer (Some [ "a" ], body));
    ("@origin", Stylesheet.Origin (Stylesheet.Author, body));
    ("@scope", Stylesheet.Scope (Some (Selector.class_ "card"), None, body));
    ("@starting-style", Stylesheet.Starting_style body);
    ( "@-moz-document",
      Stylesheet.Moz_document
        ([ Stylesheet.Url_prefix (Some "https://example.com/") ], body) );
    ( "@when",
      Stylesheet.When
        (Stylesheet.Media_condition (Media.of_string "screen"), body) );
    ("@else", Stylesheet.Else (None, body));
  ]

let test_spec_map_conditional_boundaries () =
  let recolor sel _decls =
    rule ~selector:sel [ color (Css.Values.hex "0000ff") ]
  in
  let stmts =
    [
      supports
        ~condition:(Css.Supports.func "selector" ":has(img)")
        [
          container ~name:"card"
            ~condition:(Css.Container.of_string "(inline-size > 30em)")
            [
              rule ~selector:(Selector.class_ "title") [ color (hex "#ff0000") ];
            ];
        ];
      layer ~name:[ "components" ]
        [ rule ~selector:(Selector.class_ "inside") [ color (hex "#ff0000") ] ];
    ]
  in
  let mapped = Css.map recolor stmts in
  let css = Css.to_string ~minify:true (v mapped) in
  Alcotest.(check bool)
    "map descends through supports/container" true
    (Astring.String.is_infix ~affix:".title{color:#00f}" css);
  Alcotest.(check bool)
    "map descends through layer" true
    (Astring.String.is_infix ~affix:".inside{color:#00f}" css);
  Alcotest.(check bool)
    "map preserves condition boundaries" true
    (Astring.String.is_infix ~affix:"@supports" css
    && Astring.String.is_infix ~affix:"@container" css
    && Astring.String.is_infix ~affix:"@layer" css);
  let missed =
    List.filter_map
      (fun (label, stmt) ->
        let mapped = Css.map recolor [ stmt ] in
        let css = Css.to_string ~minify:true (v mapped) in
        if Astring.String.is_infix ~affix:"color:#00f" css then None
        else Some label)
      (conditional_groups
         [ rule ~selector:(Selector.class_ "a") [ color (hex "#ff0000") ] ])
  in
  if missed <> [] then
    Alcotest.failf "map did not reach the rules of: %s"
      (String.concat ", " missed)

(* Test Css.sort - sorts rules by custom comparison *)
let test_sort () =
  let sel1 = Selector.class_ "zzz" in
  let sel2 = Selector.class_ "aaa" in
  let sel3 = Selector.class_ "mmm" in
  let stmts =
    [
      rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ];
      rule ~selector:sel2 [ color (Css.Values.hex "00ff00") ];
      rule ~selector:sel3 [ color (Css.Values.hex "0000ff") ];
    ]
  in

  (* Sort alphabetically by selector *)
  let sorted =
    Css.sort
      (fun (sel1, _) (sel2, _) ->
        String.compare (Selector.to_string sel1) (Selector.to_string sel2))
      stmts
  in

  let css = Css.to_string ~minify:true (v sorted) in
  (* Should be .aaa, .mmm, .zzz order *)
  let aaa_pos = String.index css 'a' in
  let mmm_pos = String.index_from css (aaa_pos + 1) 'm' in
  let zzz_pos = String.index_from css (mmm_pos + 1) 'z' in
  Alcotest.(check bool)
    "sort orders rules alphabetically" true
    (aaa_pos < mmm_pos && mmm_pos < zzz_pos)

(* Test Css.sort with nested media queries *)
let test_sort_nested () =
  let sel1 = Selector.class_ "zzz" in
  let sel2 = Selector.class_ "aaa" in
  let stmts =
    [
      media
        ~condition:(Css.Media.of_string "(min-width:768px)")
        [
          rule ~selector:sel1 [ color (Css.Values.hex "ff0000") ];
          rule ~selector:sel2 [ color (Css.Values.hex "00ff00") ];
        ];
    ]
  in

  (* Sort should descend into media query and reorder *)
  let sorted =
    Css.sort
      (fun (sel1, _) (sel2, _) ->
        String.compare (Selector.to_string sel1) (Selector.to_string sel2))
      stmts
  in

  let css = Css.to_string ~minify:true (v sorted) in
  (* Inside media, should be .aaa before .zzz *)
  let media_start = String.index css '@' in
  let aaa_pos = String.index_from css media_start 'a' in
  let zzz_pos = String.index_from css aaa_pos 'z' in
  Alcotest.(check bool) "sort descends into media" true (aaa_pos < zzz_pos)

let test_spec_sort_conditional_boundaries () =
  let cmp (sel1, _) (sel2, _) =
    String.compare (Selector.to_string sel1) (Selector.to_string sel2)
  in
  let stmts =
    [
      supports
        ~condition:(Css.Supports.property "display" "grid")
        [
          container
            ~condition:(Css.Container.of_string "(inline-size > 30em)")
            [
              rule ~selector:(Selector.class_ "zzz") [ color (hex "#ff0000") ];
              rule ~selector:(Selector.class_ "aaa") [ color (hex "#00ff00") ];
            ];
        ];
      layer ~name:[ "base" ]
        [
          rule ~selector:(Selector.class_ "yyy") [ color (hex "#ff0000") ];
          rule ~selector:(Selector.class_ "bbb") [ color (hex "#00ff00") ];
        ];
    ]
  in
  let sorted = Css.sort cmp stmts in
  let css = Css.to_string ~minify:true (v sorted) in
  let aaa = Astring.String.find_sub ~sub:".aaa" css |> Option.get in
  let zzz = Astring.String.find_sub ~sub:".zzz" css |> Option.get in
  let bbb = Astring.String.find_sub ~sub:".bbb" css |> Option.get in
  let yyy = Astring.String.find_sub ~sub:".yyy" css |> Option.get in
  Alcotest.(check bool) "sort descends into container" true (aaa < zzz);
  Alcotest.(check bool) "sort descends into layer" true (bbb < yyy);
  let unsorted =
    List.filter_map
      (fun (label, stmt) ->
        let css = Css.to_string ~minify:true (v (Css.sort cmp [ stmt ])) in
        let at sub = Astring.String.find_sub ~sub css in
        match (at ".aaa", at ".zzz") with
        | Some a, Some z when a < z -> None
        | _ -> Some label)
      (conditional_groups
         [
           rule ~selector:(Selector.class_ "zzz") [ color (hex "#ff0000") ];
           rule ~selector:(Selector.class_ "aaa") [ color (hex "#00ff00") ];
         ])
  in
  if unsorted <> [] then
    Alcotest.failf "sort did not reach the rules of: %s"
      (String.concat ", " unsorted)

(* An [@else] answers the [@when] before it, so a chain is one unit: sorting may
   not put a rule between its links nor swap them. [sort] moves rules ahead of
   the at-rules they sit among and leaves the at-rules in source order, which
   holds the chain together wherever it sits, including in a block [sort] only
   reaches by descending. *)
let test_spec_sort_when_else_chain () =
  let cmp (sel1, _) (sel2, _) =
    String.compare (Selector.to_string sel1) (Selector.to_string sel2)
  in
  let when_link =
    Stylesheet.When
      ( Stylesheet.Media_condition (Media.of_string "screen"),
        [ rule ~selector:(Selector.class_ "w") [ color (hex "#ff0000") ] ] )
  in
  let else_link =
    Stylesheet.Else
      (None, [ rule ~selector:(Selector.class_ "e") [ color (hex "#00ff00") ] ])
  in
  let chain = [ when_link; else_link ] in
  let zzz = rule ~selector:(Selector.class_ "zzz") [ color (hex "#ff0000") ] in
  let aaa = rule ~selector:(Selector.class_ "aaa") [ color (hex "#00ff00") ] in
  let check label stmts =
    let css = Css.to_string ~minify:true (v (Css.sort cmp stmts)) in
    Alcotest.(check bool)
      (label ^ ": @else still answers its @when")
      true
      (Astring.String.is_infix
         ~affix:"@when media(screen){.w{color:#f00}}@else{.e{color:#0f0}}" css)
  in
  check "top level" chain;
  check "rules around the chain" ((zzz :: chain) @ [ aaa ]);
  check "rule between the links" [ when_link; aaa; else_link ];
  check "inside @media" [ Stylesheet.Media (Media.of_string "print", chain) ];
  check "inside @scope"
    [ Stylesheet.Scope (Some (Selector.class_ "card"), None, chain) ];
  check "inside @when"
    [
      Stylesheet.When
        (Stylesheet.Media_condition (Media.of_string "print"), chain);
    ]

let public_fold_edges () =
  let title = Selector.class_ "title" in
  let nested = rule ~selector:title [ color (hex "#0000ff") ] in
  let parent =
    rule ~selector:(Selector.class_ "card")
      ~nested:
        [
          media ~condition:(Css.Media.of_string "(width >= 40em)") [ nested ];
          declarations [ background_color (hex "#ffffff") ];
        ]
      [ padding [ Rem 1. ] ]
  in
  let sheet =
    v
      [
        with_origin Author
          [
            layer ~name:[ "components" ]
              [
                supports
                  ~condition:(Css.Supports.property "display" "grid")
                  [
                    container
                      ~condition:
                        (Css.Container.of_string "(inline-size > 30em)")
                      [ parent ];
                  ];
              ];
          ];
        starting_style
          [
            rule ~selector:(Selector.class_ "entry")
              [ opacity (Opacity_number 0.) ];
          ];
      ]
  in
  let rule_count, decl_blocks, boundary_count =
    fold
      (fun (rules, decls, boundaries) stmt ->
        let rules =
          match as_rule stmt with Some _ -> rules + 1 | None -> rules
        in
        let decls =
          match as_declarations stmt with Some _ -> decls + 1 | None -> decls
        in
        let boundaries =
          boundaries
          +
          match
            ( as_origin stmt,
              as_layer stmt,
              as_supports stmt,
              as_container stmt,
              as_media stmt )
          with
          | Some _, _, _, _, _
          | _, Some _, _, _, _
          | _, _, Some _, _, _
          | _, _, _, Some _, _
          | _, _, _, _, Some _ ->
              1
          | _ -> 0
        in
        (rules, decls, boundaries))
      (0, 0, 0) sheet
  in
  Alcotest.(check int) "fold visits all nested rules" 3 rule_count;
  Alcotest.(check int) "fold visits bare declaration block" 1 decl_blocks;
  Alcotest.(check int) "fold visits cascade boundaries" 5 boundary_count

let public_custom_props_edges () =
  let root =
    rule ~selector:Selector.Root
      [ custom_property "--outside" "0"; color (hex "#111111") ]
  in
  let theme_rule =
    rule ~selector:Selector.Root [ custom_property "--brand" "red" ]
  in
  let nested_theme =
    media
      ~condition:(Css.Media.of_string "(prefers-color-scheme: dark)")
      [
        layer ~name:[ "theme" ]
          [
            rule ~selector:Selector.Root
              [ custom_property "--brand-dark" "#000" ];
          ];
      ]
  in
  let utilities =
    layer ~name:[ "utilities" ]
      [
        rule ~selector:(Selector.class_ "m")
          [ custom_property "--space" "1rem" ];
      ]
  in
  let sheet =
    v [ root; layer ~name:[ "theme" ] [ theme_rule ]; nested_theme; utilities ]
  in
  let all_props = custom_props sheet in
  let theme_props = custom_props ~layer:[ "theme" ] sheet in
  let has name props = List.mem name props in
  Alcotest.(check bool)
    "all props include unlayered" true
    (has "--outside" all_props);
  Alcotest.(check bool)
    "all props include theme" true
    (has "--brand" all_props && has "--brand-dark" all_props);
  Alcotest.(check bool)
    "all props include utilities" true (has "--space" all_props);
  Alcotest.(check bool)
    "theme props include named layer" true
    (has "--brand" theme_props && has "--brand-dark" theme_props);
  Alcotest.(check bool)
    "theme props exclude siblings" false
    (has "--outside" theme_props || has "--space" theme_props)

(* [custom_props] reports a custom property wherever it is declared for an
   element: every conditional group that holds style rules, and a bare nesting
   block, whose declarations apply to the enclosing rule's subject. The other
   declaration sites belong to another cascade origin or to no element at all
   (CSS Cascading 5 sec. 6.1), so a name declared only there is not declared for
   the element that reads it. *)
let public_custom_props_declaration_sites () =
  let decl = custom_property "--x" "1" in
  let styled = rule ~selector:(Selector.class_ "a") [ decl ] in
  let frame : Stylesheet.keyframe =
    { selector = Keyframe.Positions [ Keyframe.From ]; declarations = [ decl ] }
  in
  let margin : Stylesheet.page_margin_rule =
    { name = "top-left"; descriptors = [ decl ] }
  in
  let reported =
    [
      ("@media", Stylesheet.Media (Media.of_string "screen", [ styled ]));
      ( "@supports",
        Stylesheet.Supports (Supports.of_string "(top: 0)", [ styled ]) );
      ("@container", Stylesheet.Container (None, None, [ styled ]));
      ("@layer", Stylesheet.Layer (Some [ "a" ], [ styled ]));
      ("@origin", Stylesheet.Origin (Stylesheet.Author, [ styled ]));
      ( "@scope",
        Stylesheet.Scope (Some (Selector.class_ "card"), None, [ styled ]) );
      ("@starting-style", Stylesheet.Starting_style [ styled ]);
      ( "@-moz-document",
        Stylesheet.Moz_document
          ([ Stylesheet.Url_prefix (Some "https://example.com/") ], [ styled ])
      );
      ( "@when",
        Stylesheet.When
          (Stylesheet.Media_condition (Media.of_string "screen"), [ styled ]) );
      ("@else", Stylesheet.Else (None, [ styled ]));
      ( "nesting block",
        rule ~selector:(Selector.class_ "a")
          ~nested:[ Stylesheet.Declarations [ decl ] ]
          [] );
    ]
  in
  let hidden =
    [
      ("@keyframes", Stylesheet.keyframes "k" [ frame ]);
      ("@page", Stylesheet.Page ([], [ decl ]));
      ("@page margin box", Stylesheet.Page_with_margins ([], [], [ margin ]));
      ("@position-try", Stylesheet.Position_try ("--pt", [ decl ]));
      ("@supports-condition", Stylesheet.Supports_condition ("--sc", [ decl ]));
    ]
  in
  let missing =
    List.filter_map
      (fun (label, stmt) ->
        if custom_props (v [ stmt ]) = [ "--x" ] then None else Some label)
      reported
  in
  if missing <> [] then
    Alcotest.failf "custom property not reported in: %s"
      (String.concat ", " missing);
  let leaked =
    List.filter_map
      (fun (label, stmt) ->
        if custom_props (v [ stmt ]) = [] then None else Some label)
      hidden
  in
  if leaked <> [] then
    Alcotest.failf "custom property reported outside element matching: %s"
      (String.concat ", " leaked);
  (* A layer holds whatever the conditional groups below it hold, so [layer]
     selects a name declared inside one of them and nothing beside it. *)
  let layered =
    v
      [
        Stylesheet.Layer
          (Some [ "a" ], [ Stylesheet.Scope (None, None, [ styled ]) ]);
        rule ~selector:(Selector.class_ "b") [ custom_property "--out" "2" ];
      ]
  in
  Alcotest.(check (list string))
    "layer selects through @scope" [ "--x" ]
    (custom_props ~layer:[ "a" ] layered);
  Alcotest.(check (list string))
    "unlayered sibling still reported" [ "--x"; "--out" ] (custom_props layered)

(* A [@layer] inside a conditional group is ordinary CSS: the group decides
   whether its contents apply, not whether the layer exists, and a layer named
   there is the same layer a sibling block names (css-cascade-5 sec. 6.4). A
   caller asking which layers a sheet declares gets a wrong answer, not a
   conservative one, when such a block is skipped. *)
let public_layers_conditional_groups () =
  let styled = rule ~selector:(Selector.class_ "a") [ color (hex "#111111") ] in
  let block = Stylesheet.Layer (Some [ "inner" ], [ styled ]) in
  let decl = Stylesheet.Layer_decl [ [ "declared" ] ] in
  let groups =
    [
      ("@media", fun b -> Stylesheet.Media (Media.of_string "screen", b));
      ( "@supports",
        fun b -> Stylesheet.Supports (Supports.of_string "(top: 0)", b) );
      ("@container", fun b -> Stylesheet.Container (None, None, b));
      ("@layer", fun b -> Stylesheet.Layer (Some [ "outer" ], b));
      ("@origin", fun b -> Stylesheet.Origin (Stylesheet.Author, b));
      ( "@scope",
        fun b -> Stylesheet.Scope (Some (Selector.class_ "card"), None, b) );
      ("@starting-style", fun b -> Stylesheet.Starting_style b);
      ( "@-moz-document",
        fun b ->
          Stylesheet.Moz_document
            ([ Stylesheet.Url_prefix (Some "https://example.com/") ], b) );
      ( "@when",
        fun b ->
          Stylesheet.When
            (Stylesheet.Media_condition (Media.of_string "screen"), b) );
      ("@else", fun b -> Stylesheet.Else (None, b));
      ("style rule", fun b -> rule ~selector:(Selector.class_ "b") ~nested:b []);
    ]
  in
  let qualify label name =
    if label = "@layer" then [ "outer"; name ] else [ name ]
  in
  let missing =
    List.filter_map
      (fun (label, group) ->
        let sheet = v [ group [ block; decl ] ] in
        let want_block = qualify label "inner" in
        let want_decl = qualify label "declared" in
        let names = layers sheet in
        if
          List.mem want_block names && List.mem want_decl names
          && layer_block want_block sheet <> None
        then None
        else Some label)
      groups
  in
  if missing <> [] then
    Alcotest.failf "layer not reported inside: %s" (String.concat ", " missing);
  (* A sublayer of an anonymous [@layer { }] has no name any caller can ask for,
     so it is not one of the sheet's declared layers. *)
  Alcotest.(check (list (list string)))
    "anonymous layer hides its sublayers" []
    (layers (v [ Stylesheet.Layer (None, [ block ]) ]));
  (* A layer statement declares a name without opening a block, so it must not
     stand in for the block that fills the layer in later. *)
  let forward_declared =
    v
      [
        Stylesheet.Layer_decl [ [ "one" ]; [ "two" ] ];
        Stylesheet.Layer (Some [ "one" ], [ styled ]);
      ]
  in
  Alcotest.(check (list (list string)))
    "statement declares the order" [ [ "one" ]; [ "two" ] ]
    (layers forward_declared);
  let block_text name sheet =
    match layer_block name sheet with
    | None -> "<no block>"
    | Some stmts -> to_string ~minify:true (v stmts)
  in
  Alcotest.(check string)
    "statement does not shadow the block" ".a{color:#111}"
    (block_text [ "one" ] forward_declared);
  Alcotest.(check string)
    "a name only declared opens no block" "<no block>"
    (block_text [ "two" ] forward_declared);
  (* Names come in source order, so a caller reading them reads the order the
     sheet introduces its layers in. *)
  Alcotest.(check (list (list string)))
    "names in source order"
    [ [ "first" ]; [ "second" ]; [ "third" ] ]
    (layers
       (v
          [
            Stylesheet.Layer_decl [ [ "first" ] ];
            Stylesheet.Media
              ( Media.of_string "screen",
                [ Stylesheet.Layer (Some [ "second" ], [ styled ]) ] );
            Stylesheet.Layer (Some [ "third" ], [ styled ]);
          ]))

(* [Stylesheet.layers] and [Css.layers] answer one question, so they answer it
   the same way: two functions in one library disagreeing about what a sheet
   declares makes the answer depend on which one a caller happened to reach
   for. *)
let stylesheet_layers_agree_with_css () =
  let styled = rule ~selector:(Selector.class_ "a") [ color (hex "#111111") ] in
  let sheets =
    [
      ("dotted name", v [ Stylesheet.Layer (Some [ "foo"; "bar" ], [ styled ]) ]);
      ( "layer in a group",
        v
          [
            Stylesheet.Media
              ( Media.of_string "screen",
                [ Stylesheet.Layer (Some [ "inner" ], [ styled ]) ] );
          ] );
      ( "layer in a rule",
        v
          [
            rule ~selector:(Selector.class_ "b")
              ~nested:[ Stylesheet.Layer (Some [ "deep" ], [ styled ]) ]
              [];
          ] );
      ( "statement form",
        v [ Stylesheet.Layer_decl [ [ "one" ]; [ "two"; "three" ] ] ] );
    ]
  in
  List.iter
    (fun (label, sheet) ->
      Alcotest.(check (list (list string)))
        (label ^ ": Stylesheet.layers matches Css.layers")
        (layers sheet)
        (Css.Stylesheet.layers sheet))
    sheets

(* A [@media] or [@container] is the same at-rule whether or not a group sits
   above it, and the rules it holds are the ones below its brace, not the ones
   that happen to be direct children. A walk that stops at the top level reports
   neither, so a caller gets a wrong answer rather than a partial one. *)
let stylesheet_queries_reach_nested () =
  let styled sel =
    rule ~selector:(Selector.class_ sel) [ color (hex "#111111") ]
  in
  let screen = Media.of_string "screen" in
  let wide = Container.of_string "(width > 10px)" in
  let selectors_of rules =
    List.map (fun (r : Stylesheet.rule) -> Selector.to_string r.selector) rules
  in
  (* The at-rule sits under a group. *)
  let grouped wrap =
    v [ Stylesheet.Supports (Supports.of_string "(top: 0)", [ wrap ]) ]
  in
  let media_in_group = grouped (Stylesheet.Media (screen, [ styled "a" ])) in
  Alcotest.(check (list string))
    "@media under @supports is still a media query" [ ".a" ]
    (List.concat_map
       (fun (_, rules) -> selectors_of rules)
       (Css.Stylesheet.media_queries media_in_group));
  let container_in_group =
    grouped (Stylesheet.Container (Some "card", Some wide, [ styled "a" ]))
  in
  Alcotest.(check (list string))
    "@container under @supports is still a container query" [ ".a" ]
    (List.concat_map
       (fun (_, _, rules) -> selectors_of rules)
       (Css.Stylesheet.container_queries container_in_group));
  (* The rules sit under something inside the at-rule. *)
  let deep_body =
    [
      rule ~selector:(Selector.class_ "outer")
        ~nested:[ styled "nested" ]
        [ color (hex "#111111") ];
      Stylesheet.Layer (Some [ "l" ], [ styled "layered" ]);
    ]
  in
  Alcotest.(check (list string))
    "a media query holds every rule below its brace"
    [ ".outer"; ".nested"; ".layered" ]
    (List.concat_map
       (fun (_, rules) -> selectors_of rules)
       (Css.Stylesheet.media_queries
          (v [ Stylesheet.Media (screen, deep_body) ])));
  Alcotest.(check (list string))
    "a container query holds every rule below its brace"
    [ ".outer"; ".nested"; ".layered" ]
    (List.concat_map
       (fun (_, _, rules) -> selectors_of rules)
       (Css.Stylesheet.container_queries
          (v [ Stylesheet.Container (None, Some wide, deep_body) ])));
  (* [Css.media_queries] is the same walk with the rules wrapped back up as
     statements, so it reaches what the one below it reaches. *)
  let statement_selectors =
    List.concat_map
      (fun (_, stmts) ->
        List.filter_map
          (fun stmt ->
            match as_rule stmt with
            | Some (sel, _, _) -> Some (Selector.to_string sel)
            | None -> None)
          stmts)
      (media_queries (grouped (Stylesheet.Media (screen, deep_body))))
  in
  Alcotest.(check (list string))
    "Css.media_queries reaches the same rules"
    [ ".outer"; ".nested"; ".layered" ]
    statement_selectors

(* [vars_of_rules] answers the same question as [vars_of_stylesheet] over the
   statements it is given, so it reports a reference wherever a declaration
   sits: nested in a rule, inside any grouping at-rule, and in an at-rule that
   holds declarations without a block. A [var()] no walk reaches is a variable a
   caller thinks nothing needs. *)
let public_vars_declaration_sites () =
  let decl = Declaration.of_string "color:var(--x)" in
  let styled = rule ~selector:(Selector.class_ "a") [ decl ] in
  let frame : Stylesheet.keyframe =
    { selector = Keyframe.Positions [ Keyframe.From ]; declarations = [ decl ] }
  in
  let margin : Stylesheet.page_margin_rule =
    { name = "top-left"; descriptors = [ decl ] }
  in
  let sites =
    [
      ("@media", Stylesheet.Media (Media.of_string "screen", [ styled ]));
      ( "@supports",
        Stylesheet.Supports (Supports.of_string "(top: 0)", [ styled ]) );
      ("@container", Stylesheet.Container (None, None, [ styled ]));
      ("@layer", Stylesheet.Layer (Some [ "a" ], [ styled ]));
      ("@origin", Stylesheet.Origin (Stylesheet.Author, [ styled ]));
      ( "@scope",
        Stylesheet.Scope (Some (Selector.class_ "card"), None, [ styled ]) );
      ("@starting-style", Stylesheet.Starting_style [ styled ]);
      ( "@-moz-document",
        Stylesheet.Moz_document
          ([ Stylesheet.Url_prefix (Some "https://example.com/") ], [ styled ])
      );
      ( "@when",
        Stylesheet.When
          (Stylesheet.Media_condition (Media.of_string "screen"), [ styled ]) );
      ("@else", Stylesheet.Else (None, [ styled ]));
      ("nested rule", rule ~selector:(Selector.class_ "a") ~nested:[ styled ] []);
      ( "nesting block",
        rule ~selector:(Selector.class_ "a")
          ~nested:[ Stylesheet.Declarations [ decl ] ]
          [] );
      ("@keyframes", Stylesheet.keyframes "k" [ frame ]);
      ("@page", Stylesheet.Page ([], [ decl ]));
      ("@page margin box", Stylesheet.Page_with_margins ([], [], [ margin ]));
      ("@position-try", Stylesheet.Position_try ("--pt", [ decl ]));
      ("@supports-condition", Stylesheet.Supports_condition ("--sc", [ decl ]));
    ]
  in
  let names stmts = List.map any_var_name (vars_of_rules stmts) in
  let missing =
    List.filter_map
      (fun (label, stmt) ->
        if names [ stmt ] = [ "--x" ] then None else Some label)
      sites
  in
  if missing <> [] then
    Alcotest.failf "var() not reported in: %s" (String.concat ", " missing);
  (* One question, one answer: the two entry points differ only in what they are
     handed. *)
  let disagree =
    List.filter_map
      (fun (label, stmt) ->
        if
          names [ stmt ]
          = List.map any_var_name (vars_of_stylesheet (v [ stmt ]))
        then None
        else Some label)
      sites
  in
  if disagree <> [] then
    Alcotest.failf "vars_of_rules and vars_of_stylesheet disagree on: %s"
      (String.concat ", " disagree);
  (* Deduplicated across statements, in source order. *)
  let y = Declaration.of_string "color:var(--y)" in
  Alcotest.(check (list string))
    "deduplicated in source order" [ "--x"; "--y" ]
    (names
       [
         rule ~selector:(Selector.class_ "a") [ decl ];
         Stylesheet.Media (Media.of_string "screen", [ styled ]);
         rule ~selector:(Selector.class_ "b") [ y ];
       ])

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

let public_parse_edges () =
  match of_string ~strict:true ~filename:"spec.css" ".a{color:rgb(300)}" with
  | Ok _ -> Alcotest.fail "strict parser accepted invalid declaration"
  | Error err ->
      let msg = Cascade.Error.to_string err in
      Alcotest.(check bool)
        "parse error carries filename" true
        (Astring.String.is_infix ~affix:"spec.css" msg);
      let parsed =
        match
          of_string ~strict:false ~filename:"spec.css"
            ".a{color:rgb(300)}.b{color:red}.c{color:rgb(301)}"
        with
        | Ok parsed -> parsed
        | Error err ->
            Alcotest.failf "lenient parse rejected recoverable CSS: %s"
              (Cascade.Error.to_string err)
      in
      let rules = rule_statements parsed.stylesheet in
      let declaration_counts =
        List.map
          (fun statement ->
            match as_rule statement with
            | Some (_, declarations, _) -> List.length declarations
            | None -> Alcotest.fail "expected a qualified rule")
          rules
      in
      Alcotest.(check (list int))
        "partial parser discards invalid declarations, not containing rules"
        [ 0; 1; 0 ] declaration_counts;
      Alcotest.(check int)
        "partial parser reports invalid rules" 2
        (List.length parsed.warnings)

(* A newline inside a string produces a <bad-string-token>. In an at-rule
   prelude that token used to serialize to nothing, so [@media <bad-string>]
   reached the media-condition reader as an empty condition: the rule kept its
   body but lost its condition, with no diagnostic. *)
let public_bad_string_prelude_edges () =
  match of_string "@media \"abc\n{ .a { color: red } }" with
  | Error err ->
      Alcotest.failf "lenient parse rejected recoverable CSS: %s"
        (Cascade.Error.to_string err)
  | Ok parsed ->
      Alcotest.(check bool)
        "the lost media condition is reported" true (parsed.warnings <> []);
      Alcotest.(check bool)
        "no unconditional @media is emitted" false
        (Astring.String.is_infix ~affix:"@media{" (minify parsed.stylesheet))

(* CSS Syntax 3 (ED) sec. 4.3.5 ends a <bad-string-token> at the newline, so the
   token carries an opening quote and nothing to close it. Sec. 7.2 keeps one
   out of a <declaration-value>, and CSS Custom Properties 1 sec. 2.1 repeats
   the exclusion for a custom property, so a declaration carrying one is
   invalid. Emitting it anyway hands the next reader a bare quote: the [}] the
   rule closes with becomes string content and everything after it is swallowed.
   Assert the round trip rather than the spelling, which is the property the
   output owes its own reader. *)
let public_bad_string_declaration_edges () =
  let parse label css =
    match of_string ~strict:false css with
    | Ok parsed -> parsed
    | Error err ->
        Alcotest.failf "%s: lenient parse rejected recoverable CSS: %s" label
          (Cascade.Error.to_string err)
  in
  let label_of parts = String.concat "" parts in
  let reparses ~survivor label css =
    let out = Css.to_string ~minify:true (parse label css).stylesheet in
    let reread = parse (label_of [ label; " (re-read)" ]) out in
    Alcotest.(check (list string))
      (label_of [ label; ": minified output re-reads clean" ])
      []
      (List.map Cascade.Error.to_string reread.warnings);
    Alcotest.(check string)
      (label_of [ label; ": minification is a fixed point" ])
      out
      (Css.to_string ~minify:true reread.stylesheet);
    Alcotest.(check bool)
      (label_of [ label; ": keeps "; survivor ])
      true
      (Astring.String.is_infix ~affix:survivor out)
  in
  reparses ~survivor:"b{color:red}" "bad string last in a rule"
    "a{--t:\"abc\n}b{color:red}";
  reparses ~survivor:"a{color:red}" "bad string before a later declaration"
    "a{--t:\"abc\n;color:red}";
  reparses ~survivor:"b{color:red}" "bad string last in a nested rule"
    "a{&{--t:\"abc\n}}b{color:red}";
  reparses ~survivor:"b{color:red}" "bad string last in a conditional group"
    "@media screen{a{--t:\"abc\n}b{color:red}}";
  reparses ~survivor:"color:red" "bad string last in a keyframe"
    "@keyframes k{from{--t:\"abc\n}to{color:red}}";
  reparses ~survivor:"color:red" "bad string last in @page"
    "@page{--t:\"abc\n;color:red}";
  (* Control: a closed string is a <declaration-value> and survives whole. *)
  reparses ~survivor:"--t:\"abc\"" "a closed string round-trips"
    "a{--t:\"abc\";color:red}b{color:red}"

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

(* CSS Syntax 3 sec. 5.4.2 "consume an at-rule" builds an at-rule node for any
   at-keyword, recognised or not; the block it consumes keeps its contents.
   Discarding an unrecognised at-rule is a user-agent cascade step (CSS 2.1 sec.
   4.2 "Rules for handling parsing errors"), not a serialisation step, and a
   transform cannot know which agent reads its output: an agent that later
   implements the name renders the input and a stripped output differently.
   Lightning CSS, esbuild, csso, clean-css and cssnano all keep the at-rule and
   its block. So [to_string] keeps it too, at every block depth, with a body or
   without one. [Optimize.drop_unknown_at_rules] stays available for a caller
   that does want user-agent-equivalent output. *)
(* CSS Values 4 sec. 4.1: "Keywords are identifiers and are interpreted ASCII
   case-insensitively (i.e., [a-z] and [A-Z] are equivalent)." A keyword the
   author capitalised is the same keyword, so it reads to the same node and
   prints as that node prints. *)
let spec_keyword_case_insensitive () =
  let render input =
    match of_string ~strict:false input with
    | Ok parsed -> to_string ~minify:true parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  (* [expect] is the shortest spelling of the one node both inputs name. *)
  let pins name ~upper ~lower ~expect =
    Alcotest.(check string)
      (String.concat "" [ name; " (lower-case control)" ])
      expect (render lower);
    Alcotest.(check string)
      (String.concat "" [ name; " (capitalised keyword)" ])
      expect (render upper)
  in
  (* Where the printed spelling is a canonicalisation of cascade's own, the pin
     is that both spellings reach it, not what it is. *)
  let agrees name ~upper ~lower =
    let out = render lower in
    Alcotest.(check bool)
      (String.concat "" [ name; " (lower-case control parses)" ])
      true (out <> "");
    Alcotest.(check string)
      (String.concat "" [ name; " (capitalised keyword)" ])
      out (render upper)
  in
  (* CSS Grid 2 sec. 8.3: [span] is a keyword of [<grid-line>], so [SPAN 2] is
     the span of 2 tracks and not the line named [SPAN] on track 2. *)
  pins "grid-column span" ~upper:".a{grid-column:SPAN 2}"
    ~lower:".a{grid-column:span 2}" ~expect:".a{grid-column:span 2}";
  (* CSS Animations 1 sec. 4.1 makes [from] equivalent to [0%] and [to] to
     [100%]; the shorter of each pair is the printed form. *)
  pins "keyframe selector" ~upper:"@keyframes f{FROM{opacity:0}TO{opacity:1}}"
    ~lower:"@keyframes f{from{opacity:0}to{opacity:1}}"
    ~expect:"@keyframes f{0%{opacity:0}to{opacity:1}}";
  (* CSS Cascade 5 sec. 2.1 takes a [<url>] or a [<string>]; they name the same
     resource and the string is shorter. *)
  pins "@import url()" ~upper:"@import URL(\"reset.css\");"
    ~lower:"@import url(\"reset.css\");" ~expect:"@import\"reset.css\";";
  (* CSS Backgrounds 3 sec. 6.2: [inset] is a keyword of [<shadow>]. *)
  pins "box-shadow inset" ~upper:".a{box-shadow:INSET 1px 1px red}"
    ~lower:".a{box-shadow:inset 1px 1px red}"
    ~expect:".a{box-shadow:inset 1px 1px red}";
  agrees "nth-child of" ~upper:".a:nth-child(2n OF .item){color:red}"
    ~lower:".a:nth-child(2n of .item){color:red}";
  agrees "@import layer()" ~upper:"@import \"a.css\" LAYER(base);"
    ~lower:"@import \"a.css\" layer(base);";
  agrees "@property descriptors"
    ~upper:"@property --x{SYNTAX:\"<length>\";INHERITS:false;INITIAL-VALUE:0px}"
    ~lower:"@property --x{syntax:\"<length>\";inherits:false;initial-value:0px}";
  agrees "@font-face descriptors"
    ~upper:"@font-face{FONT-FAMILY:Foo;SRC:url(a.woff)}"
    ~lower:"@font-face{font-family:Foo;src:url(a.woff)}";
  agrees "gradient corner keywords"
    ~upper:".a{background:linear-gradient(TO RIGHT,red,blue)}"
    ~lower:".a{background:linear-gradient(to right,red,blue)}";
  agrees "@counter-style system"
    ~upper:"@counter-style c{system:FIXED;symbols:\"a\"}"
    ~lower:"@counter-style c{system:fixed;symbols:\"a\"}";
  (* CSS Color 4 sec. 5 names each colour function; the name is a keyword, so
     the capitalised spelling is the same function. *)
  agrees "rgb() function name" ~upper:".a{color:RGB(1,2,3)}"
    ~lower:".a{color:rgb(1,2,3)}";
  agrees "oklch() function name" ~upper:".a{color:OKLCH(50% .1 30)}"
    ~lower:".a{color:oklch(50% .1 30)}";
  agrees "light-dark() function name" ~upper:".a{color:LIGHT-DARK(red,blue)}"
    ~lower:".a{color:light-dark(red,blue)}";
  (* CSS Color 5 sec. 3: [in] introduces the optional
     [<color-interpolation-method>], so it is a keyword of the grammar. *)
  agrees "color-mix() interpolation method"
    ~upper:".a{color:color-mix(IN srgb,red,blue)}"
    ~lower:".a{color:color-mix(in srgb,red,blue)}";
  (* CSS Values 4 sec. 4.5.1 writes a [<url>] as the [url()] name over a
     [<string>], or as the url-token a URL with nothing to quote spells
     shorter. *)
  pins "background url()" ~upper:".a{background:URL(\"a.png\")}"
    ~lower:".a{background:url(\"a.png\")}" ~expect:".a{background:url(a.png)}";
  (* CSS Paged Media 3 sec. 4.3: a [<pseudo-page>] comes from the closed set
     [first | left | right | blank], so it is a keyword and not a page name. *)
  pins "@page pseudo-page" ~upper:"@page :FIRST{margin:1in}"
    ~lower:"@page :first{margin:1in}" ~expect:"@page:first{margin:1in}";
  (* The legacy shadow-piercing combinator spells one fixed ident between its
     two slashes, so that ident belongs to the combinator's syntax. *)
  pins "/deep/ combinator" ~upper:".a /DEEP/ .b{color:red}"
    ~lower:".a /deep/ .b{color:red}" ~expect:".a/deep/.b{color:red}";
  (* CSS Paged Media 3 sec. 5.2 names the sixteen margin boxes, so each name is
     a keyword of the [@page] body. *)
  pins "@page margin box" ~upper:"@page{@TOP-LEFT{content:\"x\"}}"
    ~lower:"@page{@top-left{content:\"x\"}}"
    ~expect:"@page{@top-left{content:\"x\"}}";
  (* An at-rule name is an identifier of the grammar, so sec. 4.1 reaches it
     too. The name beside it is the author's and sec. 4.2 keeps it. *)
  pins "@keyframes at-rule name" ~upper:"@KEYFRAMES Slide{FROM{opacity:0}}"
    ~lower:"@keyframes Slide{from{opacity:0}}"
    ~expect:"@keyframes Slide{0%{opacity:0}}";
  pins "@media at-rule name" ~upper:"@MEDIA screen{a{color:red}}"
    ~lower:"@media screen{a{color:red}}" ~expect:"@media screen{a{color:red}}";
  pins "@media nested in a style rule" ~upper:".a{@MEDIA screen{color:red}}"
    ~lower:".a{@media screen{color:red}}" ~expect:".a{@media screen{color:red}}";
  agrees "@font-face at-rule name"
    ~upper:"@Font-Face{font-family:F;src:url(a.woff)}"
    ~lower:"@font-face{font-family:F;src:url(a.woff)}";
  (* CSS Custom Properties 1 sec. 3 names the substitution function [var()];
     capitalising it names the same function, even where a typed reader still
     matched the function name as a bare string. *)
  agrees "display var()" ~upper:".a{display:VAR(--x)}"
    ~lower:".a{display:var(--x)}";
  agrees "animation-timeline var()" ~upper:".a{animation-timeline:VAR(--x)}"
    ~lower:".a{animation-timeline:var(--x)}";
  (* scroll-animations-1 sec. 3/4 name [scroll()] and [view()] as the timeline
     functions; sec. 4.1 folds a function name too. *)
  agrees "animation-timeline scroll()"
    ~upper:".a{animation-timeline:SCROLL(root block)}"
    ~lower:".a{animation-timeline:scroll(root block)}";
  agrees "animation-timeline view()" ~upper:".a{animation-timeline:VIEW(block)}"
    ~lower:".a{animation-timeline:view(block)}";
  agrees "transition shorthand var() property"
    ~upper:".a{transition:VAR(--x) 1s ease}"
    ~lower:".a{transition:var(--x) 1s ease}";
  agrees "box-shadow inset var()" ~upper:".a{box-shadow:inset VAR(--x)}"
    ~lower:".a{box-shadow:inset var(--x)}";
  (* Sibling gap: #620 folded [var()]'s name here but left [inset] itself, so a
     capitalised inset before [var()] still misses the typed reader. *)
  agrees "box-shadow INSET var()" ~upper:".a{box-shadow:INSET VAR(--x)}"
    ~lower:".a{box-shadow:inset var(--x)}";
  agrees "container shorthand var()" ~upper:".a{container:VAR(--x)}"
    ~lower:".a{container:var(--x)}";
  agrees "@font-face unicode-range var()"
    ~upper:"@font-face{font-family:F;src:url(a.woff);unicode-range:VAR(--x)}"
    ~lower:"@font-face{font-family:F;src:url(a.woff);unicode-range:var(--x)}";
  agrees "gradient position var()"
    ~upper:".a{background:linear-gradient(VAR(--dir),red,blue)}"
    ~lower:".a{background:linear-gradient(var(--dir),red,blue)}";
  (* [shape-outside] is a [string property]: a valid value is handed back as
     the raw source text, uppercase var() included, so there is no folded
     form to agree on here. Only the empty-var() rejection below is a shared
     path. *)
  (* [from()]/[to()] name the two required stops of [-webkit-gradient()]; the
     sibling [color-stop()] fold below (#620) left these two untouched. *)
  agrees "-webkit-gradient from()/to()"
    ~upper:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,FROM(red),TO(blue))}"
    ~lower:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,from(red),to(blue))}";
  (* WebKit's legacy gradient names [color-stop()] as a helper function of
     [-webkit-gradient()], so sec. 4.1 folds it exactly as any other function
     name. *)
  agrees "-webkit-gradient color-stop()"
    ~upper:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,from(red),COLOR-STOP(50%,green),to(blue))}"
    ~lower:
      ".a{background:-webkit-gradient(linear,left top,left \
       bottom,from(red),color-stop(50%,green),to(blue))}";
  (* An empty [var()] is invalid regardless of case (CSS Custom Properties 1
     sec. 3); the diagnosis must agree, not just the rejection. *)
  let empty_var_diagnostic input =
    match of_string ~strict:true input with
    | Ok _ -> Alcotest.failf "%s: strict parse accepted an empty var()" input
    | Error err -> Cascade.Error.to_string err
  in
  let lower_diag = empty_var_diagnostic ".a{shape-outside:var()}" in
  Alcotest.(check bool)
    (String.concat ""
       [ "lower-case empty var() names the reason, got "; lower_diag ])
    true
    (Astring.String.is_infix ~affix:"empty var()" lower_diag);
  let upper_diag = empty_var_diagnostic ".a{shape-outside:VAR()}" in
  Alcotest.(check bool)
    (String.concat ""
       [ "capitalised empty var() names the reason, got "; upper_diag ])
    true
    (Astring.String.is_infix ~affix:"empty var()" upper_diag)

(* CSS Values 4 sec. 4.2: an author-defined identifier is "fully case-sensitive
   [...] even in the ASCII range (e.g. example and EXAMPLE are two different,
   unrelated user-defined identifiers)". Reading a keyword case-insensitively
   must not reach any of these. *)
let spec_author_ident_case_sensitive () =
  let render input =
    match of_string ~strict:false input with
    | Ok parsed -> to_string ~minify:true parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let keeps name input affix =
    let out = render input in
    Alcotest.(check bool)
      (String.concat "" [ name; ": "; affix; " survives, got "; out ])
      true
      (Astring.String.is_infix ~affix out)
  in
  keeps "custom property name" ".a{--Foo:1px;color:var(--Foo)}" "--Foo:1px";
  keeps "custom property name (var reference)" ".a{--Foo:1px;color:var(--Foo)}"
    "var(--Foo)";
  (* [--Foo] and [--foo] are two unrelated properties, so neither absorbs the
     other. *)
  keeps "custom property names stay distinct" ".a{--Foo:1px;--foo:2px}"
    "--Foo:1px";
  keeps "custom property names stay distinct" ".a{--Foo:1px;--foo:2px}"
    "--foo:2px";
  keeps "@keyframes name" "@keyframes Slide{from{opacity:0}}" "Slide";
  keeps "@layer name" "@layer Base{a{color:red}}" "Base";
  keeps "container name" "@container Card (width>0px){a{color:red}}" "Card";
  keeps "view-transition type" "@view-transition{types:Foo}" "Foo";
  keeps "counter name" ".a{counter-reset:Chapter}" "Chapter";
  keeps "grid line name" ".a{grid-column:Foo}" "Foo";
  (* [span] is excluded from the name, so [SPAN Foo] is a span of the line named
     [Foo] and the name keeps its case. *)
  keeps "grid line name after span" ".a{grid-column:SPAN Foo}" "span Foo";
  keeps "class selector" ".Foo{color:red}" ".Foo";
  keeps "id selector" "#Bar{color:red}" "#Bar";
  keeps "class selectors stay distinct" ".Foo{color:red}.foo{color:red}" ".foo";
  keeps "attribute value" "[data-x=\"Foo\"]{color:red}" "Foo";
  keeps "font family name" ".a{font-family:MyFont,sans-serif}" "MyFont";
  keeps "@property name" "@property --Foo{syntax:\"*\";inherits:false}" "--Foo";
  (* CSS Fonts 4 sec. 12.1: [light] and [dark] are the keywords of
     [base-palette], so any other ident there is the author's own. *)
  keeps "base-palette ident"
    "@font-palette-values --P{font-family:F;base-palette:MyPalette}" "MyPalette";
  (* CSS Paged Media 3 sec. 4.3 puts an optional [<ident-token>] before the
     pseudo-pages; that one is the page's own name. *)
  keeps "@page name" "@page Invoice:FIRST{margin:1in}" "Invoice";
  (* A class beside the folded [/deep/] ident is still a name the author
     chose. *)
  keeps "class next to /deep/" ".a /DEEP/ .DEEP{color:red}" ".DEEP";
  (* CSS Values 4 sec. 4.5.1 carries the [<url>] as a string, not an identifier,
     so nothing in it folds. *)
  keeps "url path" ".a{background:URL(\"A.PNG\")}" "A.PNG";
  (* A [var()] reference inside a folded colour function still names a custom
     property. *)
  keeps "custom property inside color-mix()"
    ".a{--Foo:red;color:color-mix(IN srgb,var(--Foo),blue)}" "var(--Foo)";
  (* [var()]'s function name folds (sec. 4.1); the custom property name in its
     argument does not (sec. 4.2). *)
  keeps "display var() argument name" ".a{display:VAR(--Foo)}" "var(--Foo)";
  keeps "animation-timeline var() argument name"
    ".a{animation-timeline:VAR(--Foo)}" "var(--Foo)";
  (* scroll-animations-1 sec. 3: a bare [<dashed-ident>] here names a
     scroll-timeline/view-timeline, not the [scroll()]/[view()] function;
     folding the function name (above) must not reach this. *)
  keeps "animation-timeline name" ".a{animation-timeline:--Foo}" "--Foo";
  keeps "transition shorthand var() argument name"
    ".a{transition:VAR(--Foo) 1s ease}" "var(--Foo)";
  keeps "box-shadow inset var() argument name" ".a{box-shadow:inset VAR(--Foo)}"
    "var(--Foo)";
  keeps "box-shadow INSET var() argument name" ".a{box-shadow:INSET VAR(--Foo)}"
    "var(--Foo)";
  keeps "container shorthand var() argument name" ".a{container:VAR(--Foo)}"
    "var(--Foo)";
  keeps "@font-face unicode-range var() argument name"
    "@font-face{font-family:F;src:url(a.woff);unicode-range:VAR(--Foo)}"
    "var(--Foo)";
  keeps "gradient position var() argument name"
    ".a{background:linear-gradient(VAR(--Foo),red,blue)}" "var(--Foo)";
  (* CSS Syntax 3 sec. 8.2 recognises [@charset] only as the exact byte
     sequence, so the encoding name is not a keyword to fold. *)
  (match of_string ~strict:true "@charset \"utf-8\";.a{color:red}" with
  | Ok _ -> Alcotest.fail "@charset accepted a lower-case encoding name"
  | Error _ -> ());
  match of_string ~strict:true "@charset \"UTF-8\";.a{color:red}" with
  | Ok _ -> ()
  | Error err ->
      Alcotest.failf "@charset rejected the exact encoding name: %s"
        (Cascade.Error.to_string err)

let unknown_at_rule_reaches_output () =
  let parse input =
    match of_string ~strict:false input with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let keeps name input expected =
    Alcotest.(check string)
      (name ^ " (serialize)") expected
      (to_string ~minify:true (parse input));
    Alcotest.(check string)
      (name ^ " (optimize)") expected
      (parse input |> optimize |> to_string ~minify:true)
  in
  keeps "a statement-form at-rule reaches the output" "@foo bar;\na{color:red}"
    "@foo bar;a{color:red}";
  keeps "a block-form at-rule takes its contents with it"
    "@future x{a{color:red}}\nb{color:red}"
    "@future x{a{color:red}}b{color:red}";
  keeps "an unknown at-rule nested in @media survives"
    "@media screen{@foo bar;d{color:red}}"
    "@media screen{@foo bar;d{color:red}}";
  (* tw hands cascade [--input-css] written in this vocabulary. *)
  keeps "the Tailwind v4 authoring vocabulary survives"
    "@theme{--c:red}\n@utility btn{color:red}\n.x{color:red}"
    "@theme{--c:red}@utility btn{color:red}.x{color:red}";
  (* An at-rule alone in the stylesheet is the whole output, not nothing. *)
  keeps "an at-rule that is the entire stylesheet is the entire output"
    "@foo bar;" "@foo bar;";
  (* Control: a recognised at-rule is unaffected. *)
  keeps "a recognised at-rule is unaffected" "@media screen{a{color:red}}"
    "@media screen{a{color:red}}";
  (* Pretty output re-parses to the same statement list as the minified one. *)
  let input = "@future x{a{color:red}}\nb{color:red}" in
  Alcotest.(check string)
    "pretty output re-parses to the minified output"
    "@future x{a{color:red}}b{color:red}"
    (to_string ~minify:true (parse (to_string (parse input))))

(* Reading an at-rule name as a keyword decides what a miscased name reaches: a
   name cascade has a grammar for is that grammar, and only a name it has none
   for stays the [Unknown_at_rule] above. *)
let spec_at_rule_name_case_insensitive () =
  let parse input =
    match of_string ~strict:false input with
    | Ok parsed -> parsed.stylesheet
    | Error err ->
        Alcotest.failf "lenient parse rejected %S: %s" input
          (Cascade.Error.to_string err)
  in
  let render input = to_string ~minify:true (parse input) in
  let optimized input = to_string ~minify:true (optimize (parse input)) in
  let rejects name input =
    match of_string ~strict:true input with
    | Ok _ -> Alcotest.failf "%s: strict parse accepted %S" name input
    | Error _ -> ()
  in
  (* A name cascade has no grammar for is the keyword of nothing, so CSS Syntax
     3 sec. 5.4.2 consumes it and it reaches the output as it was written. *)
  Alcotest.(check string)
    "an unknown at-rule keeps the case it was written in"
    "@Foo bar;a{color:red}"
    (render "@Foo bar;\na{color:red}");
  Alcotest.(check string)
    "an unknown at-rule nested in a block keeps its case"
    "@layer x{@Bar baz;a{color:red}}"
    (render "@layer x{@Bar baz;a{color:red}}");
  (* CSS Syntax 3 sec. 8.2 matches [@charset] and the quote after it as an exact
     byte sequence, so no other spelling of it is the charset rule. *)
  Alcotest.(check string)
    "@CHARSET is a name, not the charset rule" "@CHARSET \"UTF-8\";a{color:red}"
    (render "@CHARSET \"UTF-8\";a{color:red}");
  (* Read as [@media], the block is the node its lower-case spelling is, so the
     optimizer treats the two as one. *)
  let merged =
    optimized "@media screen{a{color:red}}@media screen{b{color:blue}}"
  in
  Alcotest.(check bool)
    (String.concat ""
       [ "the lower-case control merges into one block, got "; merged ])
    true
    (not (Astring.String.is_infix ~affix:"}@media" merged));
  Alcotest.(check string)
    "@MEDIA merges with the @media beside it" merged
    (optimized "@media screen{a{color:red}}@MEDIA screen{b{color:blue}}");
  (* The grammar comes with its rejections: a prelude [@media] has no reading
     for is rejected under every spelling of the name. *)
  rejects "@media with a malformed prelude" "@media (bad{a{color:red}}";
  rejects "@MEDIA with a malformed prelude" "@MEDIA (bad{a{color:red}}"

(* SVG 2 and CSS Inline 3 give these longhands a value type each, and the facade
   carried [fill], [stroke] and [stroke-width] alone. *)
let svg_longhand_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "fill-rule:evenodd" (Css.fill_rule Evenodd);
  check "clip-rule:nonzero" (Css.clip_rule Nonzero);
  check "fill-opacity:.5" (Css.fill_opacity (Opacity_number 0.5));
  check "stroke-opacity:1" (Css.stroke_opacity (Opacity_number 1.));
  check "stroke-linecap:round" (Css.stroke_linecap Round);
  check "stroke-linejoin:bevel" (Css.stroke_linejoin Bevel);
  check "stroke-miterlimit:4" (Css.stroke_miterlimit (Number 4.));
  check "stroke-dashoffset:2px"
    (Css.stroke_dashoffset (Dash (Length (Length (Px 2.)))));
  check "stroke-dasharray:4 2"
    (Css.stroke_dasharray (Dashes [ Number 4.; Number 2. ]));
  check "paint-order:stroke fill" (Css.paint_order (Order [ Stroke; Fill ]));
  check "vector-effect:non-scaling-stroke"
    (Css.vector_effect (Effects ([ Non_scaling_stroke ], None)));
  check "stop-color:red" (Css.stop_color (Named Red));
  check "stop-opacity:0" (Css.stop_opacity (Opacity_number 0.));
  check "flood-color:red" (Css.flood_color (Named Red));
  check "flood-opacity:1" (Css.flood_opacity (Opacity_number 1.));
  check "lighting-color:red" (Css.lighting_color (Named Red));
  check "dominant-baseline:middle" (Css.dominant_baseline Middle);
  check "alignment-baseline:hanging" (Css.alignment_baseline Hanging);
  check "baseline-shift:super" (Css.baseline_shift Super);
  check "baseline-source:last" (Css.baseline_source Last)

(* CSS Anchor Positioning 1 is modelled end to end and the facade built none of
   it, so a caller could parse an anchored box but not write one. *)
let anchor_positioning_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "anchor-name:--tip" (Css.anchor_name (Names [ "--tip" ]));
  check "position-anchor:--tip" (Css.position_anchor (Anchor "--tip"));
  check "position-anchor:normal" (Css.position_anchor Normal);
  check "position-area:block-start span-inline-end"
    (Css.position_area (Area (Block_start, Some Span_inline_end)));
  check "position-try-fallbacks:--a flip-block"
    (Css.position_try_fallbacks
       (Fallbacks [ Tactics [ Name "--a"; Flip_block ] ]));
  check "position-try-fallbacks:start end"
    (Css.position_try_fallbacks (Fallbacks [ Area (Start, Some End) ]));
  check "position-try-order:most-width" (Css.position_try_order Most_width);
  check "position-try:most-width --a"
    (Css.position_try (Try (Most_width, Fallbacks [ Tactics [ Name "--a" ] ])));
  check "position-visibility:anchors-visible"
    (Css.position_visibility (Conditions [ Anchors_visible ]))

(* View Transitions is advertised in the README and the facade built neither
   property, so a caller could parse a transition name but not write one. *)
let view_transition_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "view-transition-name:card" (Css.view_transition_name (Name "card"));
  check "view-transition-name:none" (Css.view_transition_name None);
  check "view-transition-class:hero wide"
    (Css.view_transition_class (Classes [ "hero"; "wide" ]))

(* CSS Cascading 5 sec. 3.3 gives [all] every longhand but [direction] and
   [unicode-bidi]; the optimiser acts on it and the facade could not write
   one. *)
let all_shorthand_builder () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "all:initial" (Css.all Initial);
  check "all:revert-layer" (Css.all Revert_layer)

(* Motion Path 1 is modelled down to the shorthand and the facade built none of
   it. *)
let motion_path_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "offset-path:none" (Css.offset_path None);
  check "offset-path:path(\"M 0 0 H 1\")" (Css.offset_path (Path "M 0 0 H 1"));
  check "offset-distance:50%" (Css.offset_distance (Pct 50.));
  check "offset-rotate:reverse" (Css.offset_rotate Reverse);
  check "offset-rotate:auto 30deg"
    (Css.offset_rotate (With_angle (Auto, Deg 30.)));
  check "offset-anchor:auto" (Css.offset_anchor Auto);
  check "offset-position:normal" (Css.offset_position Normal);
  check "offset:none"
    (Css.offset
       (Shorthand
          {
            target =
              With_path
                { position = None; path = None; distance = None; rotate = None };
            anchor = None;
          }))

(* CSS Multicol 2 models each longhand of [columns] and the gap decoration
   lines, and the facade wrote only the two shorthands. *)
let multicol_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "column-width:20em" (Css.column_width (Width (Em 20.)));
  check "column-count:3" (Css.column_count (Count 3));
  check "column-height:10em" (Css.column_height (Height (Em 10.)));
  check "column-wrap:wrap" (Css.column_wrap Wrap);
  check "column-rule-width:1px,2px" (Css.column_rule_width [ Px 1.; Px 2. ]);
  check "column-rule-style:dotted,dashed"
    (Css.column_rule_style [ Dotted; Dashed ]);
  check "column-rule-color:red" (Css.column_rule_color [ Named Red ])

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
      Alcotest.test_case "list-style value parsers" `Quick
        list_style_value_parsers;
      Alcotest.test_case "font-family value parser" `Quick
        font_family_value_parser;
      Alcotest.test_case "option value parser contracts" `Quick
        option_value_parser_contracts;
      (* AST introspection helpers *)
      Alcotest.test_case "layer_block extraction" `Quick test_layer_block;
      Alcotest.test_case "rules_of_statements" `Quick test_rules_of_statements;
      Alcotest.test_case "custom_prop_names" `Quick test_custom_prop_names;
      Alcotest.test_case "custom_props_of_rules" `Quick
        test_custom_props_of_rules;
      (* Statement transformation helpers *)
      Alcotest.test_case "map transforms rules" `Quick test_map;
      Alcotest.test_case "map nested in media" `Quick test_map_nested;
      Alcotest.test_case "spec map conditional boundaries" `Quick
        test_spec_map_conditional_boundaries;
      Alcotest.test_case "sort orders rules" `Quick test_sort;
      Alcotest.test_case "sort nested in media" `Quick test_sort_nested;
      Alcotest.test_case "spec sort conditional boundaries" `Quick
        test_spec_sort_conditional_boundaries;
      Alcotest.test_case "spec sort keeps a when/else chain" `Quick
        test_spec_sort_when_else_chain;
      Alcotest.test_case "public fold edge traversal" `Quick public_fold_edges;
      Alcotest.test_case "public custom property scoping" `Quick
        public_custom_props_edges;
      Alcotest.test_case "public custom property declaration sites" `Quick
        public_custom_props_declaration_sites;
      Alcotest.test_case "public layers in conditional groups" `Quick
        public_layers_conditional_groups;
      Alcotest.test_case "stylesheet layers agree with css layers" `Quick
        stylesheet_layers_agree_with_css;
      Alcotest.test_case "stylesheet queries reach nested" `Quick
        stylesheet_queries_reach_nested;
      Alcotest.test_case "public var declaration sites" `Quick
        public_vars_declaration_sites;
      Alcotest.test_case "public property introspection" `Quick
        public_property_edges;
      Alcotest.test_case "public theme guards" `Quick public_theme_edges;
      Alcotest.test_case "public theme guards in declaration at-rules" `Quick
        public_theme_guards_in_declaration_at_rules;
      Alcotest.test_case "public value combinators" `Quick
        public_value_combinator_edges;
      Alcotest.test_case "public theme var rendering" `Quick
        public_theme_var_rendering_edges;
      Alcotest.test_case "public parse recovery edges" `Quick public_parse_edges;
      Alcotest.test_case "public bad-string prelude edges" `Quick
        public_bad_string_prelude_edges;
      Alcotest.test_case "public bad-string declaration edges" `Quick
        public_bad_string_declaration_edges;
      Alcotest.test_case "spec unknown at-rule reaches the output" `Quick
        unknown_at_rule_reaches_output;
      Alcotest.test_case "spec section 4.1 keyword case is insensitive" `Quick
        spec_keyword_case_insensitive;
      Alcotest.test_case "spec section 4.2 author ident case is sensitive"
        `Quick spec_author_ident_case_sensitive;
      Alcotest.test_case "public svg longhand builders" `Quick
        svg_longhand_builders;
      Alcotest.test_case "public anchor positioning builders" `Quick
        anchor_positioning_builders;
      Alcotest.test_case "public view transition builders" `Quick
        view_transition_builders;
      Alcotest.test_case "public all shorthand builder" `Quick
        all_shorthand_builder;
      Alcotest.test_case "public motion path builders" `Quick
        motion_path_builders;
      Alcotest.test_case "public multicol builders" `Quick multicol_builders;
      Alcotest.test_case "spec section 4.1 at-rule name case is insensitive"
        `Quick spec_at_rule_name_case_insensitive;
    ] )
