(** Tests for CSS Variables module - CSS/MDN spec compliance *)

open Cascade
open Css.Declaration
open Css.Values
open Css_test_helpers
open Css.Variables

let check_any_syntax =
  check_value_cursor "any_syntax" read_any_syntax pp_any_syntax

let decl_t : Css.Declaration.declaration Alcotest.testable =
  Alcotest.testable
    (fun fmt d -> Format.pp_print_string fmt (Css.Declaration.to_string d))
    ( = )

(* These tests are for CSS Variables module *)
let test_any_var () =
  (* Test CSS custom property declaration creation using Variables.var *)
  let decl, _var = var "primary-color" Color (Css.Values.hex "ff0000") in

  (* Check declaration is created properly *)
  let name_opt = custom_declaration_name decl in
  (match name_opt with
  | Some name ->
      Alcotest.(check string)
        "variable name has -- prefix" "--primary-color" name
  | None -> Alcotest.fail "Expected custom declaration");

  (* Test var with fallback *)
  let decl2, _var2 = var "theme-color" Color (Css.Values.hex "ff0000") in

  let name_opt2 = custom_declaration_name decl2 in
  (match name_opt2 with
  | Some name ->
      Alcotest.(check string) "theme color variable" "--theme-color" name
  | None -> Alcotest.fail "Expected custom declaration for theme color");

  (* Test negative cases *)
  neg_cursor read_reference "not-a-var()"

(* CSS Variables 1 sec. 2: a custom-property name is any dashed-ident other than
   bare [--]. A third dash starts the ident body; it is not forbidden. *)
let custom_property_name_edges () =
  let cursor = Cursor.of_string "var(---foo)" in
  let name, fallback = read_reference cursor in
  Alcotest.(check string) "third dash belongs to the name" "-foo" name;
  Alcotest.(check (option string)) "no fallback" None fallback;
  neg_cursor read_reference "var(--)"

(* Not a roundtrip test *)
let test_vars_of_calc () =
  (* Test calc without variables *)
  let simple_calc : length calc = Expr (Num 100., Add, Num 8.) in
  let vars = vars_of_calc simple_calc in
  Alcotest.(check int) "no variables in numeric calc" 0 (List.length vars);

  (* Another calc without variables *)
  let mul_calc : length calc = Expr (Num 100., Mul, Num 2.) in
  let no_vars = vars_of_calc mul_calc in
  Alcotest.(check int) "no variables in numeric calc" 0 (List.length no_vars);

  (* Calc with a variable *)
  (* Use CSS Variables.var function to create proper variable *)
  let _gap_decl, gap_var = var "gap" Length (Px 16.) in
  let calc_with_var : length calc = Expr (Var gap_var, Add, Val (Px 10.)) in
  let with_vars = vars_of_calc calc_with_var in
  Alcotest.(check int) "one variable in calc" 1 (List.length with_vars);

  (* Nested calc with multiple variables *)
  let _width_decl, width_var = var "width" Length (Pct 100.) in
  let nested : length calc =
    Expr (Var gap_var, Add, Expr (Var width_var, Div, Num 2.))
  in
  let nested_vars = vars_of_calc nested in
  Alcotest.(check int)
    "two variables in nested calc" 2 (List.length nested_vars);

  (* Complex calc expression *)
  let complex : length calc =
    Expr (Expr (Var gap_var, Mul, Num 2.), Sub, Val (Rem 1.))
  in
  let complex_vars = vars_of_calc complex in
  Alcotest.(check int)
    "one variable in complex calc" 1 (List.length complex_vars)

(* Not a roundtrip test *)
let test_vars_of_property () =
  let _width_decl, width_var = var "container-width" Length (Px 1024.) in

  (* Width property with variable *)
  let width_vars = vars_of_property Width (Length (Var width_var)) in
  Alcotest.(check int) "found width variable" 1 (List.length width_vars);

  (* Width property with calc containing variable *)
  let calc_with_var : length = Calc (Expr (Var width_var, Sub, Num 32.)) in
  let calc_vars = vars_of_property Width (Length calc_with_var) in
  Alcotest.(check int) "found variable in calc" 1 (List.length calc_vars);

  (* Width property without variable *)
  let no_vars = vars_of_property Width (Length (Px 100.)) in
  Alcotest.(check int) "no variables in px value" 0 (List.length no_vars)

let spec_vars_of_property_matrix () =
  let check property_name declaration =
    let decl = Css.Declaration.of_string declaration in
    let vars = vars_of_declarations [ decl ] in
    Alcotest.(check int) property_name 1 (List.length vars)
  in
  List.iter
    (fun (property_name, declaration) -> check property_name declaration)
    [
      ("caption-side", "caption-side: var(--spec-caption-side)");
      ("dominant-baseline", "dominant-baseline: var(--spec-dominant-baseline)");
      ("field-sizing", "field-sizing: var(--spec-field-sizing)");
      ( "grid-template-areas",
        "grid-template-areas: var(--spec-grid-template-areas)" );
      ("hyphens", "hyphens: var(--spec-hyphens)");
      ( "initial-letter-align",
        "initial-letter-align: var(--spec-initial-letter-align)" );
      ( "initial-letter-wrap",
        "initial-letter-wrap: var(--spec-initial-letter-wrap)" );
      ("isolation", "isolation: var(--spec-isolation)");
      ("mask-type", "mask-type: var(--spec-mask-type)");
      ("order", "order: var(--spec-order)");
      ("table-layout", "table-layout: var(--spec-table-layout)");
      ( "text-emphasis-skip",
        "text-emphasis-skip: var(--spec-text-emphasis-skip)" );
      ( "text-emphasis-style",
        "text-emphasis-style: var(--spec-text-emphasis-style)" );
      ("-webkit-hyphens", "-webkit-hyphens: var(--spec-webkit-hyphens)");
      ("-webkit-line-clamp", "-webkit-line-clamp: var(--spec-webkit-line-clamp)");
      ("z-index", "z-index: var(--spec-z-index)");
    ]

(* CSS Custom Properties L1 sec. 3: a var() reference is a reference whatever
   property holds it. A logical property, a shorthand taking a length list and a
   position-valued property each carry the reference exactly as their physical
   or longhand twin does. *)
let spec_vars_of_property_logical_matrix () =
  let check property_name declaration =
    let decl = Css.Declaration.of_string declaration in
    let vars = vars_of_declarations [ decl ] in
    Alcotest.(check int) property_name 1 (List.length vars)
  in
  List.iter
    (fun (property_name, declaration) -> check property_name declaration)
    [
      ("inline-size", "inline-size: var(--spec-inline-size)");
      ("min-inline-size", "min-inline-size: var(--spec-min-inline-size)");
      ("max-inline-size", "max-inline-size: var(--spec-max-inline-size)");
      ("block-size", "block-size: var(--spec-block-size)");
      ("min-block-size", "min-block-size: var(--spec-min-block-size)");
      ("max-block-size", "max-block-size: var(--spec-max-block-size)");
      ("inset", "inset: var(--spec-inset)");
      ("inset-inline", "inset-inline: var(--spec-inset-inline)");
      ( "inset-inline-start",
        "inset-inline-start: var(--spec-inset-inline-start)" );
      ("inset-inline-end", "inset-inline-end: var(--spec-inset-inline-end)");
      ("inset-block", "inset-block: var(--spec-inset-block)");
      ("inset-block-start", "inset-block-start: var(--spec-inset-block-start)");
      ("inset-block-end", "inset-block-end: var(--spec-inset-block-end)");
      ("scroll-margin-inline", "scroll-margin-inline: var(--spec-sm-inline)");
      ( "scroll-margin-inline-start",
        "scroll-margin-inline-start: var(--spec-sm-inline-start)" );
      ( "scroll-margin-inline-end",
        "scroll-margin-inline-end: var(--spec-sm-inline-end)" );
      ("scroll-margin-block", "scroll-margin-block: var(--spec-sm-block)");
      ( "scroll-margin-block-start",
        "scroll-margin-block-start: var(--spec-sm-block-start)" );
      ( "scroll-margin-block-end",
        "scroll-margin-block-end: var(--spec-sm-block-end)" );
      ("scroll-padding-inline", "scroll-padding-inline: var(--spec-sp-inline)");
      ( "scroll-padding-inline-start",
        "scroll-padding-inline-start: var(--spec-sp-inline-start)" );
      ( "scroll-padding-inline-end",
        "scroll-padding-inline-end: var(--spec-sp-inline-end)" );
      ("scroll-padding-block", "scroll-padding-block: var(--spec-sp-block)");
      ( "scroll-padding-block-start",
        "scroll-padding-block-start: var(--spec-sp-block-start)" );
      ( "scroll-padding-block-end",
        "scroll-padding-block-end: var(--spec-sp-block-end)" );
      ("background-position", "background-position: var(--spec-bg-position)");
      ("mask-position", "mask-position: var(--spec-mask-position)");
      ( "-webkit-mask-position",
        "-webkit-mask-position: var(--spec-webkit-mask-position)" );
      ("text-emphasis-color", "text-emphasis-color: var(--spec-te-color)");
      ("text-underline-offset", "text-underline-offset: var(--spec-tu-offset)");
      ("shape-margin", "shape-margin: var(--spec-shape-margin)");
      ("shape-outside", "shape-outside: var(--spec-shape-outside)");
      ("line-height-step", "line-height-step: var(--spec-line-height-step)");
      ("offset-distance", "offset-distance: var(--spec-offset-distance)");
      ("transition-property", "transition-property: var(--spec-transition-prop)");
      ("place-self", "place-self: var(--spec-place-self)");
    ]

(* CSS Custom Properties L1 sec. 3: [resolve_theme] emits a root definition for
   every var() the sheet references and the theme resolves. A logical property
   holding the reference must get the same binding as its physical twin, or the
   emitted sheet carries an undefined var(). *)
let spec_theme_binding_for_logical_property () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.singleton "w" in
  let theme_defaults = function "w" -> Some "10px" | _ -> None in
  let render css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "width emits the theme binding" ":root{--w:10px}.a{width:var(--w)}"
    (render ".a { width: var(--w) }");
  Alcotest.(check string)
    "inline-size emits the theme binding"
    ":root{--w:10px}.a{inline-size:var(--w)}"
    (render ".a { inline-size: var(--w) }")

(* CSS Animations L1 sec. 3: a keyframe block holds ordinary property
   declarations. CSS Custom Properties L1 sec. 3: var() in one is substituted
   against the animated element, so a name referenced only from a keyframe needs
   the same root binding as one referenced from a rule. *)
let spec_theme_binding_inside_keyframes () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme = Css.Pp.String_set.singleton "w" in
  let theme_defaults = function "w" -> Some "10px" | _ -> None in
  let render css =
    parse css
    |> Css.resolve_theme ~theme ~theme_defaults
    |> Css.to_string ~minify:true
  in
  Alcotest.(check string)
    "rule emits the theme binding" ":root{--w:10px}.a{width:var(--w)}"
    (render ".a { width: var(--w) }");
  Alcotest.(check string)
    "keyframe block emits the theme binding"
    ":root{--w:10px}@keyframes k{0%{width:var(--w)}}"
    (render "@keyframes k { from { width: var(--w) } }");
  Alcotest.(check string)
    "keyframes in a layer emits the theme binding"
    ":root{--w:10px}@layer l{@keyframes k{0%{width:var(--w)}}}"
    (render "@layer l { @keyframes k { from { width: var(--w) } } }")

(* CSS Cascading 5 sec. 6.1: a declaration inside [@keyframes] belongs to the
   animation origin and one inside [@position-try] to the position fallback
   origin, so neither applies to an element that merely references the name.
   Only a style rule declares a custom property for ordinary matching, so those
   at-rules must not suppress the root binding: suppressed, the reference is
   left free and the property falls back to its initial value. *)
let spec_theme_binding_past_non_cascading_declaration () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let theme_defaults = function "w" -> Some "10px" | _ -> None in
  let render css =
    parse css
    |> Css.resolve_theme ~theme:Css.Pp.String_set.empty ~theme_defaults
    |> Css.to_string ~minify:true
  in
  let bound = ".a{width:10px}" in
  Alcotest.(check string)
    "no declaration anywhere" bound
    (render ".a { width: var(--w) }");
  Alcotest.(check string)
    "keyframe declaration does not suppress the binding"
    ("@keyframes k{0%{--w:1px}}" ^ bound)
    (render "@keyframes k { from { --w: 1px } } .a { width: var(--w) }");
  Alcotest.(check string)
    "position-try declaration does not suppress the binding"
    ("@position-try --pt{--w:1px}" ^ bound)
    (render "@position-try --pt { --w: 1px } .a { width: var(--w) }");
  (* A style rule does declare the name for ordinary matching, so the reference
     stays live for the elements the cascade may reach. *)
  Alcotest.(check string)
    "rule declaration keeps the reference live" ".b{--w:1px}.a{width:var(--w)}"
    (render ".b { --w: 1px } .a { width: var(--w) }")

(* Not a roundtrip test *)
let test_vars_of_declarations () =
  let custom_color_decl, color_var =
    var "text-color" Color (Css.Values.hex "333333")
  in
  let custom_size_decl, size_var = var "font-size" Length (Rem 1.0) in

  (* Create declarations using the variables *)
  let color_decl = v Color (Var color_var) in
  let size_decl = v Font_size (Length (Var size_var)) in

  let vars =
    vars_of_declarations
      [ custom_color_decl; custom_size_decl; color_decl; size_decl ]
  in

  (* Should find the two variables used in declarations (not definitions) *)
  Alcotest.(check bool) "found variables" true (List.length vars >= 2);

  (* A custom property whose typed value references another var must expose that
     reference, just like a standard property does (regression: returned []). *)
  let _black_decl, black_var =
    var "color-black" Color (Css.Values.hex "000000")
  in
  let standard_ref = vars_of_declarations [ v Color (Var black_var) ] in
  let custom_ref =
    vars_of_declarations [ fst (var "x" Color (Var black_var)) ]
  in
  let has_black = List.exists (fun av -> any_var_name av = "--color-black") in
  Alcotest.(check bool)
    "standard property exposes the ref" true (has_black standard_ref);
  Alcotest.(check bool)
    "custom Typed value exposes the ref" true (has_black custom_ref);
  (* A custom property whose *opaque* token-stream value references another var
     exposes it too: the value never went through a typed parser, so the
     reference is recovered structurally from the component stream. *)
  let tokens_ref =
    vars_of_declarations [ of_string "--gradient-bg:var(--color-black)" ]
  in
  Alcotest.(check bool)
    "custom Tokens value exposes the ref" true (has_black tokens_ref)

(* CSS Images 3 sec. 3.2 gives a radial gradient three independently animatable
   parts - shape, size and centre <position> - and CSS Variables L1 sec. 3 lets
   a custom property hold any one of them. Each type carries [Var], so a caller
   binds one to a custom property, prints the binding, and reads it back through
   a [var()] channel exactly as [Color] or [Length] does. *)
let spec_radial_and_position_kinds () =
  let bind name kind value expected =
    let decl, _ = var name kind value in
    Alcotest.(check string)
      ("--" ^ name ^ " binding")
      expected
      (Css.Declaration.to_string ~minify:true decl)
  in
  bind "radial-shape" Radial_shape Circle "--radial-shape:circle";
  bind "radial-size" Radial_size Farthest_corner "--radial-size:farthest-corner";
  bind "radial-position" Position_value Center "--radial-position:center";
  (* A typed binding whose value is itself a [var()] exposes the reference, so
     the three channels compose the way tw's mask-radial stops need. *)
  let _, shape_src = var "src-shape" Radial_shape Ellipse in
  let _, size_src = var "src-size" Radial_size Closest_side in
  let _, position_src = var "src-position" Position_value Top in
  let reads name decl expected =
    Alcotest.(check (list string))
      name [ expected ]
      (List.map any_var_name (vars_of_declarations [ decl ]))
  in
  reads "radial shape channel"
    (fst (var "radial-shape" Radial_shape (Var shape_src)))
    "--src-shape";
  reads "radial size channel"
    (fst (var "radial-size" Radial_size (Var size_src)))
    "--src-size";
  reads "radial position channel"
    (fst (var "radial-position" Position_value (Var position_src)))
    "--src-position"

(* Not a roundtrip test *)
let test_any_var_name () =
  let _spacing_decl, var_handle = var "spacing" Length (Px 0.) in
  let any_var = V var_handle in

  let name = any_var_name any_var in
  Alcotest.(check string) "variable name with prefix" "--spacing" name

(* Not a roundtrip test *)
let test_extract_custom_declarations () =
  let regular = v Width (Length (Px 100.)) in

  let custom1, _ = var "color1" Color (Css.Values.hex "ff0000") in
  let custom2, _ = var "size1" Length (Px 16.) in

  let decls = [ custom1; regular; custom2 ] in
  let customs = custom_declarations decls in

  Alcotest.(check int) "extracted custom declarations" 2 (List.length customs)

(* Not a roundtrip test *)
let test_custom_declaration_name () =
  let regular = v Height (Length (Px 50.)) in

  let custom, _ = var "my-var" Length (Px 20.) in
  let custom_name = custom_declaration_name custom in
  let regular_name = custom_declaration_name regular in

  Alcotest.(check (option string))
    "custom has name" (Some "--my-var") custom_name;
  Alcotest.(check (option string)) "regular has no name" None regular_name

(* Not a roundtrip test *)
let test_compare_vars_by_name () =
  let _decl1, var1 = var "aaa" Length (Px 0.) in
  let _decl2, var2 = var "bbb" Length (Px 0.) in
  let _decl3, var3 = var "aaa" Length (Px 0.) in
  (* Same name as var1 *)

  let cmp1 = compare_vars_by_name (V var1) (V var2) in
  let cmp2 = compare_vars_by_name (V var1) (V var3) in

  Alcotest.(check bool) "aaa < bbb" true (cmp1 < 0);
  Alcotest.(check int) "aaa = aaa" 0 cmp2

(* Not a roundtrip test *)
let test_custom_property_roundtrip () =
  (* Create a custom property using Variables.var *)
  let custom, _ = var "primary" Color (Css.Values.hex "0080ff") in

  (* Check it follows CSS custom property syntax *)
  match custom_declaration_name custom with
  | Some name ->
      Alcotest.(check bool)
        "has -- prefix" true
        (String.starts_with ~prefix:"--" name);
      Alcotest.(check string) "correct name" "--primary" name
  | None -> Alcotest.fail "Expected custom declaration"

let check_typed_custom name expected syntax value =
  Alcotest.(check string)
    name expected
    (Css.Declaration.to_string ~minify:true
       (typed_custom_property name syntax value))

let spec_typed_custom_property () =
  (* CSS Variables 1 sec. 2 substitutes the declared token stream verbatim, so
     the value has to spell its own type. A unitless 0 is a <number> token
     there, and CSS Values 4 sec. 10.1 rejects a <number> added to a <length>,
     so the zero keeps its unit. *)
  check_typed_custom "--w" "--w:0px" Length (Px 0.);
  (* The multipliers of CSS Values 4 sec. 2.3: [+] repeats space-separated, [#]
     comma-separated. *)
  check_typed_custom "--edges" "--edges:10px 20px" (Plus Length)
    [ Px 10.; Px 20. ];
  check_typed_custom "--stops" "--stops:1px,2px" (Hash Length) [ Px 1.; Px 2. ];
  (* Unquoted, CSS Syntax 3 sec. 4.3.1 reads [a b] as two idents, which is not
     the one <string> the syntax names. *)
  check_typed_custom "--label" "--label:\"a b\"" String "a b";
  (* A disjunction writes the arm it holds, not both. *)
  check_typed_custom "--edge" "--edge:auto"
    (Or (Length, Ident_keyword "auto"))
    (Either.Right ());
  Alcotest.(check (option string))
    "layer travels with the declaration" (Some "theme")
    (Css.Declaration.custom_declaration_layer
       (typed_custom_property ~layer:"theme" "--w" Length (Px 1.)));
  (* CSS Variables 1 sec. 2 names a custom property with a <dashed-ident>, the
     same check Declaration.custom_property makes. *)
  Alcotest.check_raises "undashed name is no custom property"
    (Failure "custom_property: w: not a custom-property name") (fun () ->
      ignore (typed_custom_property "w" Length (Px 1.)))

let spec_typed_custom_property_registration () =
  (* A registration read back at its syntax assigns without a printer per arm,
     and writes the value the registration registers. *)
  let registration =
    Css.property ~name:"--w" Length ~initial_value:(Px 0.) ~inherits:false ()
  in
  let declared =
    match List.map Css.as_property registration with
    | [ Some (Css.Property_info { name; syntax; initial_value = Some v; _ }) ]
      ->
        Css.Declaration.to_string ~minify:true
          (typed_custom_property name syntax v)
    | _ -> Alcotest.fail "expected one @property registration"
  in
  Alcotest.(check string)
    "declares the registered initial value" "--w:0px" declared

let test_any_syntax () =
  (* Test syntax parsing according to CSS @property spec
     https://developer.mozilla.org/en-US/docs/Web/CSS/@property/syntax *)
  (* Syntax values must be quoted strings per CSS spec *)
  check_any_syntax "\"<length>\"";
  check_any_syntax "\"<color>\"";
  check_any_syntax "\"<number>\"";
  check_any_syntax "\"<integer>\"";
  check_any_syntax "\"<percentage>\"";
  check_any_syntax "\"<angle>\"";
  check_any_syntax "\"<time>\"";
  check_any_syntax "\"*\"";
  check_any_syntax ~expected:"\"<length>|<percentage>\""
    "\"<length> | <percentage>\"";

  (* Test invalid syntax values *)
  neg_cursor read_any_syntax "<length>";
  (* Missing quotes *)
  neg_cursor read_any_syntax "length";
  (* No angle brackets or quotes *)
  neg_cursor read_any_syntax "\"<invalid-type>\"";
  (* Invalid type name *)
  neg_cursor read_any_syntax "\"\"";
  (* Empty syntax *)
  neg_cursor read_any_syntax "unquoted"

let spec_property_syntax_edges () =
  check_any_syntax "\"<length>+\"";
  check_any_syntax "\"<color>#\"";
  check_any_syntax "\"<custom-ident>\"";
  check_any_syntax "\"<transform-list>\"";
  check_any_syntax "\"<url>\"";
  check_any_syntax "\"<image>\"";
  check_any_syntax ~expected:"\"<length>|<percentage>|auto\""
    "\"<length> | <percentage> | auto\"";
  check_any_syntax ~expected:"\"<number>|none\"" "\"<number> | none\"";
  neg_cursor read_any_syntax "\"<length>++\"";
  neg_cursor read_any_syntax "\"<color># #\"";
  neg_cursor read_any_syntax "\"<length>|\"";
  neg_cursor read_any_syntax "\"| <length>\"";
  neg_cursor read_any_syntax "\"<length> <color>\"";
  neg_cursor read_any_syntax "\"<length> || <color>\"";
  neg_cursor read_any_syntax "\"<unknown>\"";
  neg_cursor read_any_syntax "\"<length\""

(* CSS Properties and Values API 1 (ED) sec. 5.4.3 "Consume a Syntax Component"
   consumes at most one multiplier: after the component name it consumes a
   single [+] or [#] and returns. Sec. 5.4.2 then accepts only EOF or [|] after
   a component, so a second multiplier fails the whole syntax definition. Sec.
   3.1 makes an unreadable syntax string an invalid descriptor, which leaves
   [@property] without the descriptor it requires and drops the registration -
   the state [<custom-ident>#+], [<custom-ident>++] and [<custom-ident>##]
   already reach. Sec. 5.3 puts a multiplier inside an alternation arm, where
   one per arm stays valid. *)
let spec_property_syntax_multiplier_chain () =
  check_any_syntax "\"<custom-ident>+\"";
  check_any_syntax "\"<custom-ident>#\"";
  neg_cursor read_any_syntax "\"<custom-ident>+#\"";
  neg_cursor read_any_syntax "\"<custom-ident>#+\"";
  neg_cursor read_any_syntax "\"<custom-ident>++\"";
  neg_cursor read_any_syntax "\"<custom-ident>##\"";
  (* One multiplier per alternation arm, on either side of the [|]. *)
  check_any_syntax ~expected:"\"<length>+|auto\"" "\"<length>+ | auto\"";
  check_any_syntax ~expected:"\"auto|<color>#\"" "\"auto | <color>#\"";
  neg_cursor read_any_syntax "\"<length>+# | auto\"";
  neg_cursor read_any_syntax "\"auto | <color>#+\"";
  (* The registration a rejected syntax string costs. An [initial-value] of two
     idents parses only under the [+], so the surviving rule is one that kept
     its multiplier and typed the value at it. *)
  let render css =
    match Css.of_string ~strict:false css with
    | Ok parsed -> Css.to_string ~minify:true parsed.stylesheet
    | Error _ -> Alcotest.failf "failed to parse: %s" css
  in
  let sheet syn =
    String.concat ""
      [
        "@property --p{syntax:\"";
        syn;
        "\";inherits:false;initial-value:a b}.x{color:red}";
      ]
  in
  Alcotest.(check string)
    "one multiplier registers and types the initial value"
    (sheet "<custom-ident>+")
    (render (sheet "<custom-ident>+"));
  Alcotest.(check string)
    "chained multipliers drop the registration" ".x{color:red}"
    (render (sheet "<custom-ident>+#"))

(* CSS Properties and Values API 1 (ED) sec. 5.4.3 gives a component a [+] or
   [#] multiplier, and [read_value] repeats the component reader to read the
   list. A reader that reports success without consuming anything never lets
   that repetition end, so every component reader has to reject an empty stream.
   The universal syntax is the one reader that may match nothing, and sec. 5.4.2
   returns it only for a syntax string that is [*] and nothing else, so it takes
   no multiplier and joins no alternation. *)
let spec_multiplied_component_terminates () =
  let rejects_empty_stream name =
    let quoted = String.concat "" [ "\""; name; "\"" ] in
    match read_any_syntax (Cursor.of_string quoted) with
    | Syntax syntax -> (
        match read_value (Cursor.of_string "") syntax with
        | exception Error.Parse_error _ -> ()
        | _ -> Alcotest.failf "%s read a value from an empty stream" name)
  in
  List.iter rejects_empty_stream
    [
      "<length>";
      "<color>";
      "<number>";
      "<integer>";
      "<percentage>";
      "<length-percentage>";
      "<angle>";
      "<time>";
      "<resolution>";
      "<custom-ident>";
      "<string>";
      "<url>";
      "<image>";
      "<transform-function>";
      "<transform-list>";
      "auto";
    ];
  (* The universal syntax stands alone. *)
  check_any_syntax "\"*\"";
  neg_cursor read_any_syntax "\"*+\"";
  neg_cursor read_any_syntax "\"*#\"";
  neg_cursor read_any_syntax "\"* | <length>\"";
  neg_cursor read_any_syntax "\"<length> | *\"";
  (* The multipliers these readers sit under stay readable. *)
  check_any_syntax "\"<transform-function>+\"";
  check_any_syntax "\"<resolution>+\""

(* Not a roundtrip test *)
let test_syntax () =
  (* Syntax checking is not available in current implementation *)
  ()

(* ignore-test: read_reference is a function, not a type *)
let test_read_var_reference () =
  (* Test parsing CSS var() references - just extracts name and fallback *)
  let check_var_ref input expected_name expected_fallback =
    let r = Cursor.of_string input in
    let name, fallback = read_reference r in
    Alcotest.(check string) "variable name" expected_name name;
    Alcotest.(check (option string)) "fallback" expected_fallback fallback
  in

  (* Basic var() references *)
  check_var_ref "var(--color)" "color" None;
  check_var_ref "var(--primary)" "primary" None;
  check_var_ref "var(--theme-bg)" "theme-bg" None;

  (* Custom property names starting with a digit after -- are valid per the CSS
     Syntax spec (the two dashes are the ident-start). Tailwind arbitrary values
     like text-[1A202C] emit var(--1A202C). *)
  check_var_ref "var(--1A202C)" "1A202C" None;
  check_var_ref "var(--42, 10px)" "42" (Some "10px");

  (* With fallbacks *)
  check_var_ref "var(--color, red)" "color" (Some "red");
  check_var_ref "var(--size, 10px)" "size" (Some "10px");

  (* Test invalid cases *)
  let neg input =
    let r = Cursor.of_string input in
    try
      let _ = read_reference r in
      Alcotest.failf "Expected failure for: %s" input
    with
    | Cursor.Parse_error _ | Reader.Parse_error _ -> ()
    | exn ->
        Alcotest.failf "Unexpected exception for '%s': %s" input
          (Printexc.to_string exn)
  in

  neg "not-a-var";
  neg "var(color)";
  (* Missing -- prefix *)
  neg "var()";
  (* Empty variable name *)
  neg "variable(--color)";
  (* Wrong function name *)
  neg "var(--)";
  (* No name after -- *)
  (* CSS Custom Properties for Cascading Variables 1 sec. 3: content after the
     name without a leading comma is not part of [var()]'s grammar.
     [Cursor.call] did not require [read_reference] to consume its whole
     sub-cursor, so [var(--x 10px)] silently dropped [ 10px] instead of
     invalidating the reference. *)
  neg "var(--x 10px)"

let spec_custom_fallback_edges () =
  let check_var_ref input expected_name expected_fallback =
    let r = Cursor.of_string input in
    let name, fallback = read_reference r in
    Alcotest.(check string) (input ^ " name") expected_name name;
    Alcotest.(check (option string))
      (input ^ " fallback") expected_fallback fallback
  in
  check_var_ref "var(--color,)" "color" (Some "");
  check_var_ref "var(--color, red, blue)" "color" (Some "red, blue");
  check_var_ref "var(--shadow, 0 0 0 var(--fallback, black))" "shadow"
    (Some "0 0 0 var(--fallback, black)");
  check_var_ref "var(--tokens, { color: red; })" "tokens"
    (Some "{ color: red; }");
  check_var_ref "var(--list, [a, b], (c))" "list" (Some "[a, b], (c)");
  check_var_ref "var(--commented, a /*x*/ b)" "commented" (Some "a /*x*/ b");
  check_var_ref "var(--string, \"a,b\")" "string" (Some "\"a,b\"");
  check_var_ref "var(--empty-block, {})" "empty-block" (Some "{}");
  (* CSS Syntax sec. 4.3.5 consumes ')' as part of an unterminated string at
     EOF; it is not a function close token. *)
  check_var_ref "var(--bad-string, \"unterminated)" "bad-string"
    (Some "\"unterminated)");
  let neg input =
    let r = Cursor.of_string input in
    try
      let _ = read_reference r in
      Alcotest.failf "Expected failure for: %s" input
    with
    | Cursor.Parse_error _ | Reader.Parse_error _ -> ()
    | exn ->
        Alcotest.failf "Unexpected exception for '%s': %s" input
          (Printexc.to_string exn)
  in
  neg "var(--color";
  check_var_ref "var(---)" "-" None;
  neg "var(--, red)"

(* CSS Custom Properties 1 sec. 3: [var()] = [var( <custom-property-name> ,
   <declaration-value>? )]. [read_reference_body] reads that argument list from
   a cursor already positioned at it, without the surrounding [var(] and [)] a
   caller would otherwise have to assemble and hand back to a parser - the same
   relationship [Values.read_calc_expr] has to a [calc()] body. *)
let spec_read_reference_body_edges () =
  let check ?(expected = "") input =
    check_value_cursor "var body"
      (read_reference_body read_length)
      (pp_var pp_length) ~minify:false ~expected input
  in
  (* No fallback: just the [<custom-property-name>]. *)
  check "--x" ~expected:"var(--x)";
  (* A fallback that itself parses as the target syntax is kept typed. *)
  check "--x, 10px" ~expected:"var(--x, 10px)";
  (* A trailing comma with nothing after it is an explicit empty fallback,
     distinct from no fallback at all. *)
  check "--x," ~expected:"var(--x,)";
  (* A fallback that is not itself a <length> is preserved as the
     declaration-value tokens it is, not rejected. *)
  check "--x, red" ~expected:"var(--x, red)";
  neg_cursor (read_reference_body read_length) "not-a-var";
  neg_cursor (read_reference_body read_length) "--"

(* The string-returning counterpart of {!read_reference_body}: same [var(
   <custom-property-name> , <declaration-value>? )] argument list, read from a
   cursor already positioned at it, but handing back the name and the fallback
   text rather than a typed handle. A caller that has no value type to pick a
   fallback reader from needs the [<declaration-value>] as the text
   {!Values.syntax_fallback} consumes. *)
let spec_read_reference_body_as_string_edges () =
  let check input expected_name expected_fallback =
    let r = Cursor.of_string input in
    let name, fallback = read_reference_body_as_string r in
    Alcotest.(check string) (input ^ " name") expected_name name;
    Alcotest.(check (option string))
      (input ^ " fallback") expected_fallback fallback
  in
  (* No fallback: just the [<custom-property-name>], [--] stripped as
     {!read_reference} strips it. *)
  check "--x" "x" None;
  check "--x, 10px" "x" (Some "10px");
  (* A trailing comma with nothing after it is an explicit empty fallback,
     distinct from no fallback at all. *)
  check "--x," "x" (Some "");
  (* The fallback is a [<declaration-value>]: commas inside it belong to it. *)
  check "--x, red, blue" "x" (Some "red, blue");
  check "--shadow, 0 0 0 var(--f, black)" "shadow"
    (Some "0 0 0 var(--f, black)");
  (* CSS Custom Properties 1 sec. 3 round-trips author comments in a
     fallback. *)
  check "--commented, a /*x*/ b" "commented" (Some "a /*x*/ b");
  (* A third dash starts the ident body; it is not forbidden. *)
  check "---" "-" None;
  neg_cursor read_reference_body_as_string "not-a-var";
  neg_cursor read_reference_body_as_string "--";
  neg_cursor read_reference_body_as_string "--, red"

let spec_custom_computed_edges () =
  let check_context name specified =
    let decl = Css.Declaration.of_string (name ^ ": " ^ specified) in
    let ctx = { Css.Context.empty with custom_properties = [ decl ] } in
    Alcotest.(check (option decl_t))
      (name ^ " context") (Some decl)
      (Css.Context.custom_property name ctx)
  in
  check_context "--gap" "var(--space, 1rem)";
  check_context "--self" "var(--self)";
  check_context "--a" "var(--b)";
  check_context "--registered" "10px";
  check_context "--invalid-fallback" "var(--missing, 10px)";
  let check_var_ref input expected_name expected_fallback =
    let r = Cursor.of_string input in
    let name, fallback = read_reference r in
    Alcotest.(check string) (input ^ " name") expected_name name;
    Alcotest.(check (option string))
      (input ^ " fallback") expected_fallback fallback
  in
  check_var_ref "var(--self)" "self" None;
  check_var_ref "var(--a, var(--b, var(--c)))" "a" (Some "var(--b, var(--c))");
  check_var_ref "var(--registered, color(display-p3 1 0 0))" "registered"
    (Some "color(display-p3 1 0 0)")

let tests =
  [
    ("any_var", `Quick, test_any_var);
    ("custom property name edges", `Quick, custom_property_name_edges);
    ("any_syntax", `Quick, test_any_syntax);
    ("spec property syntax descriptor edges", `Quick, spec_property_syntax_edges);
    ( "spec property syntax multiplier chain",
      `Quick,
      spec_property_syntax_multiplier_chain );
    ( "spec multiplied component terminates",
      `Quick,
      spec_multiplied_component_terminates );
    ("vars of calc", `Quick, test_vars_of_calc);
    ("vars of property", `Quick, test_vars_of_property);
    ("spec vars of property matrix", `Quick, spec_vars_of_property_matrix);
    ( "spec vars of property logical matrix",
      `Quick,
      spec_vars_of_property_logical_matrix );
    ( "spec theme binding for logical property",
      `Quick,
      spec_theme_binding_for_logical_property );
    ( "spec theme binding inside keyframes",
      `Quick,
      spec_theme_binding_inside_keyframes );
    ( "spec theme binding past non-cascading declaration",
      `Quick,
      spec_theme_binding_past_non_cascading_declaration );
    ("vars of declarations", `Quick, test_vars_of_declarations);
    ("spec radial and position kinds", `Quick, spec_radial_and_position_kinds);
    ("any_var_name", `Quick, test_any_var_name);
    ("extract custom declarations", `Quick, test_extract_custom_declarations);
    ("custom declaration name", `Quick, test_custom_declaration_name);
    ("compare vars by name", `Quick, test_compare_vars_by_name);
    ("custom property roundtrip", `Quick, test_custom_property_roundtrip);
    ("spec typed custom property", `Quick, spec_typed_custom_property);
    ( "spec typed custom property registration",
      `Quick,
      spec_typed_custom_property_registration );
    ("syntax", `Quick, test_syntax);
    ("read_reference", `Quick, test_read_var_reference);
    ("spec custom property fallback edges", `Quick, spec_custom_fallback_edges);
    ("spec read_reference_body edges", `Quick, spec_read_reference_body_edges);
    ( "spec read_reference_body_as_string edges",
      `Quick,
      spec_read_reference_body_as_string_edges );
    ( "spec custom property computed-time edges",
      `Quick,
      spec_custom_computed_edges );
  ]

let suite = ("variables", tests)
