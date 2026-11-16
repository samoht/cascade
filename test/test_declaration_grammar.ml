(** Declaration tests: the property grammar manifest and where a declaration
    value ends. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

type property_grammar_row = Cascade_spec_inventory.Property_grammar.row

let property_grammar_matrix = Cascade_spec_inventory.Property_grammar.rows

let parse_property_decl property value =
  let input = property ^ ":" ^ value in
  try
    let c = Cursor.of_string input in
    match read_declaration c with
    | None -> None
    | Some decl ->
        let serialized = Css.Declaration.to_string ~minify:true decl in
        let c2 = Cursor.of_string serialized in
        Some (input, serialized, decl, read_declaration c2)
  with Cursor.Parse_error _ -> None

let same_property_reparse (row : property_grammar_row) decl reparsed =
  Css.Declaration.property_name reparsed = row.property
  && Css.Declaration.to_string ~minify:true decl
     = Css.Declaration.to_string ~minify:true reparsed

(* A manifest row's failure. Every row is checked, so a row's checks raise this
   rather than ending the test at the first failing row. *)
exception Row_failed of string

let row_fail msg = raise (Row_failed msg)
let row_failf fmt = Fmt.kstr row_fail fmt

let check_property_positive (row : property_grammar_row) value =
  match parse_property_decl row.property value with
  | Some (_input, serialized, decl, Some reparsed)
    when same_property_reparse row decl reparsed ->
      ignore serialized
  | Some (input, serialized, _, _) ->
      row_failf
        "%s positive vector serialized to unparsable or structurally different \
         declaration: %s -> %s"
        row.property input serialized
  | None -> row_failf "%s positive vector rejected: %s" row.property value

let check_property_negative (row : property_grammar_row) value =
  match parse_property_decl row.property value with
  | None -> ()
  | Some (_, _, decl, _) when Css.Declaration.is_invalid decl -> ()
  | Some (input, serialized, _, _) ->
      row_failf "%s negative vector parsed: %s -> %s" row.property input
        serialized

let check_property_css_wide (row : property_grammar_row) keyword =
  match parse_property_decl row.property keyword with
  | Some (_, _, decl, Some reparsed)
    when same_property_reparse row decl reparsed ->
      ()
  | Some (input, serialized, _, _) ->
      row_failf "%s CSS-wide keyword did not structurally reparse: %s -> %s"
        row.property input serialized
  | None -> row_failf "%s CSS-wide keyword rejected: %s" row.property keyword

let check_property_var (row : property_grammar_row) =
  let token_stream_fallbacks =
    [
      "{ color: red; }";
      "[a, b, c]";
      "translateX(10px) rotate(45deg)";
      "1 ! important";
    ]
  in
  List.iter
    (fun fallback ->
      match
        parse_property_decl row.property ("var(--spec-value," ^ fallback ^ ")")
      with
      | Some (_, _, decl, Some reparsed)
        when same_property_reparse row decl reparsed ->
          ()
      | Some (input, serialized, _, _) ->
          row_failf "%s var() value did not structurally reparse: %s -> %s"
            row.property input serialized
      | None ->
          row_failf "%s var() value rejected with fallback: %s" row.property
            fallback)
    (row.positives @ token_stream_fallbacks)

let property_value_has_substitution value =
  let value = String.lowercase_ascii value in
  List.exists
    (fun name -> Astring.String.is_infix ~affix:(name ^ "(") value)
    [ "var"; "env"; "attr" ]

let check_property_trailing_token_rejected (row : property_grammar_row) value =
  (* Arbitrary substitutions defer property-grammar validation, including an
     otherwise-unexpected token beside the function. *)
  if not (property_value_has_substitution value) then
    match parse_property_decl row.property (value ^ " )") with
    | None -> ()
    | Some (input, serialized, _, _) ->
        row_failf
          "%s positive vector accepted an extra trailing token: %s -> %s"
          row.property input serialized

(* CSS Syntax 3 (ED) sec. 5.5.6 stops a declaration's value at the top-level [;]
   and lifts a trailing [!important] out of it into the important flag, so every
   vector the grammar accepts bare it has to accept with either tail behind it,
   and to the same value. *)
let check_property_value_tail (row : property_grammar_row) value =
  let bare = row.property ^ ":" ^ value in
  match parse_property_decl row.property value with
  | None -> row_failf "%s positive vector rejected: %s" row.property value
  | Some (_, _, decl, _) ->
      let expected = Css.Declaration.string_of_value ~minify:true decl in
      List.iter
        (fun (suffix, important) ->
          let input = bare ^ suffix in
          let c = Cursor.of_string input in
          match read_declaration c with
          | None ->
              row_failf "%s dropped the declaration: %s" row.property input
          | exception Cursor.Parse_error _ ->
              row_failf "%s rejected the declaration: %s" row.property input
          | Some tailed ->
              let got = Css.Declaration.string_of_value ~minify:true tailed in
              if not (String.equal got expected) then
                row_failf "%s holds the value of %S: expected %S, got %S" input
                  bare expected got;
              if
                not (Bool.equal important (Css.Declaration.is_important tailed))
              then row_failf "%s important flag: expected %b" input important)
        [ (";", false); (" !important", true); (" !important;", true) ]

let check_property_name (row : property_grammar_row) =
  if String.trim row.property <> row.property then
    row_failf "%S has leading or trailing property-name whitespace" row.property;
  if row.property = "" then row_fail "empty property-name row";
  String.iter
    (function
      | 'a' .. 'z' | '0' .. '9' | '-' -> ()
      | c ->
          row_failf "%s contains non-CSS property-name character %C"
            row.property c)
    row.property

let check_branch_counts (row : property_grammar_row) =
  if row.positives = [] then
    row_failf "%s has no positive grammar vectors" row.property;
  if row.negatives = [] then
    row_failf "%s has no negative grammar vectors" row.property;
  if List.length row.positives < 2 then
    row_failf "%s needs at least two positive branch vectors" row.property;
  if List.length row.negatives < 2 then
    row_failf "%s needs at least two negative branch vectors" row.property

let manifest_normalize_value = String.trim

let check_no_branch_duplicates (row : property_grammar_row) =
  let duplicate_values values =
    let normalized = List.map manifest_normalize_value values in
    List.length normalized
    <> List.length (List.sort_uniq String.compare normalized)
  in
  if duplicate_values row.positives then
    row_failf "%s has duplicate positive grammar vectors" row.property;
  if duplicate_values row.negatives then
    row_failf "%s has duplicate negative grammar vectors" row.property

let check_no_positive_negative_overlap (row : property_grammar_row) =
  let positive_set =
    List.sort_uniq String.compare
      (List.map manifest_normalize_value row.positives)
  in
  List.iter
    (fun negative ->
      let normalized = manifest_normalize_value negative in
      if List.mem normalized positive_set then
        row_failf "%s grammar vector is both positive and negative: %S"
          row.property normalized)
    row.negatives

let check_no_empty_branches (row : property_grammar_row) =
  List.iter
    (fun value ->
      if String.trim value = "" then
        row_failf "%s has an empty grammar vector" row.property)
    (row.positives @ row.negatives)

let check_manifest_row_shape (row : property_grammar_row) =
  check_property_name row;
  check_branch_counts row;
  check_no_branch_duplicates row;
  check_no_positive_negative_overlap row;
  check_no_empty_branches row

let calc_expected_properties =
  [
    "width";
    "height";
    "margin";
    "padding";
    "border-radius";
    "opacity";
    "font-size";
    "line-height";
    "translate";
    "rotate";
    "scale";
    "transition";
    "animation";
    "filter";
    "background-position";
    "object-position";
    "transform-origin";
  ]

let check_manifest_calc_surface (row : property_grammar_row) =
  if List.mem row.property calc_expected_properties then
    let has_calc =
      List.exists
        (fun value ->
          List.exists
            (fun prefix -> String.starts_with ~prefix value)
            [ "calc("; "min("; "max("; "clamp("; "round("; "mod("; "rem(" ]
          || List.exists
               (fun needle -> Re.execp Re.(compile (str needle)) value)
               [ "calc("; "min("; "max("; "clamp("; "round("; "mod("; "rem(" ])
        row.positives
    in
    if not has_calc then
      row_failf
        "%s should include at least one function/calc-family grammar vector"
        row.property

let check_property_row (row : property_grammar_row) =
  check_manifest_row_shape row;
  check_manifest_calc_surface row;
  List.iter (check_property_positive row) row.positives;
  List.iter (check_property_trailing_token_rejected row) row.positives;
  List.iter (check_property_value_tail row) row.positives;
  List.iter (check_property_negative row) row.negatives;
  List.iter
    (check_property_css_wide row)
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  check_property_var row

(* Every row is checked and every failure reported. Alcotest raises on the first
   one, so iterating the matrix directly makes a 2643-vector check report one
   finding standing for however many there are, and the size of the gap is the
   thing this manifest exists to measure. *)
let spec_property_grammar_manifest () =
  let unique_properties =
    List.sort_uniq String.compare
      (List.map
         (fun (row : property_grammar_row) -> row.property)
         property_grammar_matrix)
  in
  if List.length unique_properties <> List.length property_grammar_matrix then
    Alcotest.fail "property grammar manifest has duplicate property rows";
  (* No count here. How much of the reader this manifest covers is derived from
     the reader's own name table by scripts/check_properties.ml, which reports
     it every run; a literal here says only that the file did not shrink, and
     counting rows is what let one row stand in for three grammars. *)
  if unique_properties = [] then
    Alcotest.fail "property grammar manifest is empty";
  let failures =
    List.filter_map
      (fun (row : property_grammar_row) ->
        match check_property_row row with
        | () -> None
        | exception Row_failed msg ->
            Some (String.concat "" [ row.property; ": "; msg ]))
      property_grammar_matrix
  in
  match failures with
  | [] -> ()
  | failures ->
      Alcotest.failf "%d manifest row(s) failed:@.%s" (List.length failures)
        (String.concat "\n" failures)

let parse_declaration_case () =
  (* A known property parses to a typed declaration. *)
  (match parse_declaration "mask-type" "luminance" with
  | Some d ->
      Alcotest.(check string)
        "typed prop" "mask-type"
        (Css.Declaration.property_name d)
  | None -> Alcotest.fail "mask-type:luminance should parse");
  (* A known property's var() refs are visible to vars_of_declarations. *)
  (match parse_declaration "color" "var(--color-black)" with
  | Some d ->
      let names = List.map Css.any_var_name (Css.vars_of_declarations [ d ]) in
      Alcotest.(check bool)
        "typed var ref visible" true
        (List.mem "--color-black" names)
  | None -> Alcotest.fail "color:var(--color-black) should parse");
  (* A custom property parses (and keeps its component stream, not an opaque
     wrapper); it is dispatched as a custom property, not dropped. *)
  (match parse_declaration "--gradient-bg" "var(--color-black)" with
  | Some d ->
      Alcotest.(check (option string))
        "custom name" (Some "--gradient-bg")
        (Css.custom_declaration_name d)
  | None -> Alcotest.fail "--gradient-bg should parse");
  (* A layer attaches to a custom property. *)
  (match parse_declaration ~layer:"theme" "--c" "red" with
  | Some d ->
      Alcotest.(check (option string))
        "layer" (Some "theme")
        (custom_declaration_layer d)
  | None -> Alcotest.fail "--c should parse");
  (* An unparseable value yields None, not a recovered declaration. *)
  Alcotest.(check bool)
    "invalid value is None" true
    (parse_declaration "mask-type" "definitely-not-a-mask-type" = None)

(* CSS Scroll Snap 1 sec. 5.1: the [scroll-margin] longhands are [<length>],
   with no [0,inf] range and no "negative values are invalid" clause - that
   clause belongs to [scroll-padding] alone (sec. 4.2), and the CR changelog
   records the restriction as applying to [scroll-padding] only. Outsets may
   therefore be negative, exactly like [margin]. *)
let scroll_margin_negative () =
  let props =
    [
      "scroll-margin";
      "scroll-margin-top";
      "scroll-margin-right";
      "scroll-margin-bottom";
      "scroll-margin-left";
      "scroll-margin-inline";
      "scroll-margin-inline-start";
      "scroll-margin-inline-end";
      "scroll-margin-block";
      "scroll-margin-block-start";
      "scroll-margin-block-end";
    ]
  in
  List.iter
    (fun prop ->
      (* Minified (no space after the colon) and spaced spellings agree. *)
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-2vh")
        (prop ^ ":-2vh");
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-2vh")
        (prop ^ ": -2vh");
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-1px")
        (prop ^ ": -1px");
      check_declaration ~roundtrip:true ~expected:(prop ^ ":-.5em")
        (prop ^ ": -.5em");
      check_declaration ~roundtrip:true
        ~expected:(prop ^ ":calc(-1px - 2px)")
        (prop ^ ": calc(-1px - 2px)");
      (* Percentages stay invalid: the longhands are [<length>], "Percentages:
         n/a". *)
      neg_cursor read_declaration (prop ^ ": -10%");
      neg_cursor read_declaration (prop ^ ": 10%"))
    props;
  (* Multi-value shorthand forms take a negative in any position. *)
  check_declaration ~roundtrip:true ~expected:"scroll-margin:-1px 2px"
    "scroll-margin:-1px 2px";
  check_declaration ~roundtrip:true ~expected:"scroll-margin:1px -2px"
    "scroll-margin: 1px -2px";
  check_declaration ~roundtrip:true
    ~expected:"scroll-margin:-1px -2px -3px -4px"
    "scroll-margin: -1px -2px -3px -4px";
  check_declaration ~roundtrip:true ~expected:"scroll-margin-block:-1px -2px"
    "scroll-margin-block: -1px -2px";
  check_declaration ~roundtrip:true ~expected:"scroll-margin-inline:-1px -2px"
    "scroll-margin-inline: -1px -2px";
  (* Control: [scroll-padding] is [auto | <length-percentage>] with "Negative
     values are invalid", so its longhands keep rejecting them. *)
  List.iter
    (fun prop ->
      neg_cursor read_declaration (prop ^ ":-2vh");
      neg_cursor read_declaration (prop ^ ": -2vh"))
    [
      "scroll-padding";
      "scroll-padding-top";
      "scroll-padding-inline";
      "scroll-padding-block-end";
    ]

(* The whole-sheet path must accept the same values without recovering: a
   swallowed warning is what hid this from [Css.of_string] callers. *)
let scroll_margin_negative_sheet () =
  List.iter
    (fun css ->
      match Css.of_string ~strict:true css with
      | Ok { stylesheet; _ } ->
          Alcotest.(check string)
            "scroll-margin sheet roundtrip" css
            (String.trim (Css.to_string ~minify:true stylesheet))
      | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e))
    [
      "a{scroll-margin:-2vh}";
      "a{scroll-margin:-1px 2px}";
      "a{scroll-margin-top:-2vh}";
      "a{scroll-margin-block:-1px -2px}";
      "a{scroll-margin-inline-start:-1px}";
    ]

(* A declaration the reader rejects is dropped from the sheet with nothing but a
   warning, so every reader gap needs a whole-sheet pin too. *)
let check_sheet_roundtrip name css =
  match Css.of_string ~strict:true css with
  | Ok { stylesheet; _ } ->
      Alcotest.(check string)
        (name ^ " sheet roundtrip")
        css
        (String.trim (Css.to_string ~minify:true stylesheet))
  | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)

(* CSS Text Decoration 4 sec. 2.6 joins the line, thickness, style and colour
   components with [||], so no single component is mandatory. *)
let text_decoration_thickness_range () =
  (* CSS Text Decoration 4 section 2.4: <length-percentage> has no non-negative
     grammar range. The minimum device-pixel thickness is a rendering rule.
     Unwrapping a negative [calc()] is the optimizer's to make and not the
     printer's: which properties read the bare value back is a fact about the
     property, so the two spellings differ in the held form and meet in the
     optimised one. *)
  List.iter
    (fun (value, printed, optimized) ->
      check_declaration ~roundtrip:true
        ~expected:("text-decoration-thickness:" ^ printed)
        ~optimized:("text-decoration-thickness:" ^ optimized)
        ("text-decoration-thickness:" ^ value))
    [
      ("-1px", "-1px", "-1px");
      ("-10%", "-10%", "-10%");
      ("-.25em", "-.25em", "-.25em");
      ("calc(-1px)", "calc(-1px)", "-1px");
      ("calc(-10%)", "calc(-10%)", "-10%");
      ("calc(2px - 3px)", "calc(2px - 3px)", "-1px");
      ("min(-1px,2px)", "min(-1px,2px)", "-1px");
    ];
  check_declaration ~roundtrip:true "text-decoration:underline -1px";
  none_cursor read_declaration "text-decoration-thickness:-1s"

let text_decoration_optional_line () =
  check_declaration ~roundtrip:true "text-decoration:red";
  check_sheet_roundtrip "text-decoration" "a{text-decoration:red}"

(* CSS Text Decoration 4 (ED) sec. 2.6 sets an omitted shorthand slot to its
   initial value - [solid] (sec. 2.2) and [currentcolor] (sec. 2.3) - so a slot
   written with that value says what leaving it out says. Written alone the slot
   is all there is, and dropping it drains the shorthand: what is left declares
   nothing but the four initials, which is what [none] declares. *)
let text_decoration_drained_shorthand () =
  check_declaration ~expected:"text-decoration:solid"
    ~optimized:"text-decoration:none" "text-decoration: solid";
  check_declaration ~expected:"text-decoration:currentColor"
    ~optimized:"text-decoration:none" "text-decoration: currentcolor";
  check_declaration ~expected:"text-decoration:solid currentColor"
    ~optimized:"text-decoration:none" "text-decoration: solid currentcolor";
  check_sheet_roundtrip "text-decoration solid" "a{text-decoration:solid}";
  (* The drop still runs wherever a slot outlives it. *)
  check_declaration ~expected:"text-decoration:solid red"
    ~optimized:"text-decoration:red" "text-decoration: solid red";
  check_declaration ~expected:"text-decoration:underline solid"
    ~optimized:"text-decoration:underline" "text-decoration: underline solid";
  check_declaration ~expected:"text-decoration:underline currentColor"
    ~optimized:"text-decoration:underline"
    "text-decoration: underline currentcolor";
  check_declaration ~expected:"text-decoration:solid 2px"
    ~optimized:"text-decoration:2px" "text-decoration: solid 2px";
  check_declaration ~expected:"text-decoration:none"
    ~optimized:"text-decoration:none" "text-decoration: none"

(* CSS Text 4 sec. 3 allows each longhand component of [white-space] on its own;
   omitted components take their initial values. *)
let white_space_collapse_only () =
  check_declaration ~roundtrip:true "white-space:collapse";
  check_sheet_roundtrip "white-space" "a{white-space:collapse}"

(* CSS Backgrounds 3 sec. 5.7 sets omitted border-image shorthand slots to their
   initial values, so the repeat component is valid on its own. *)
let border_image_repeat_only () =
  check_declaration ~roundtrip:true "border-image:round";
  check_sheet_roundtrip "border-image" "a{border-image:round}"

(* Scroll-driven Animations 1 sec. 2.3.3 and 3.4.4 make the axis optional in
   both named timeline shorthands. *)
let timeline_name_only () =
  List.iter
    (fun (property, name) ->
      let declaration = String.concat "" [ property; ":"; name ] in
      check_declaration ~roundtrip:true declaration;
      check_sheet_roundtrip property
        (String.concat "" [ "a{"; declaration; "}" ]))
    [ ("scroll-timeline", "--t"); ("view-timeline", "--v") ]

(* Scroll-driven Animations 1 sec. 3.4.4 includes the optional
   view-timeline-inset slot in each view-timeline shorthand item. *)
let view_timeline_inset_slot () =
  check_declaration ~roundtrip:true "view-timeline:--v 10% 20%";
  check_sheet_roundtrip "view-timeline" "a{view-timeline:--v 10% 20%}"

(* CSS Inline 3 sec. 6.1 joins text-box-trim and text-box-edge with [||], so the
   edge is valid without an explicit trim value. *)
let text_box_edge_only () =
  check_declaration ~roundtrip:true "text-box:cap alphabetic";
  check_sheet_roundtrip "text-box" "a{text-box:cap alphabetic}"

(* CSS Inline 3 sec. 6.1 gives [normal] its own text-box shorthand branch. *)
let text_box_normal () =
  check_declaration ~roundtrip:true "text-box:normal";
  check_sheet_roundtrip "text-box" "a{text-box:normal}"

(* CSS Values 4 sec. 8.3 allows four edge-offset components but excludes the
   three-value form. CSS Backgrounds 3 sec. 2.6 retains valid three-value
   <bg-position>s, while CSS Transforms 1 sec. 4 gives transform-origin a
   narrower grammar with an optional Z length after a two-value origin. *)
let edge_offset_position_grammar () =
  List.iter
    (fun (declaration, expected) ->
      check_declaration ~expected ~roundtrip:true declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      let expected_css = String.concat "" [ "a{"; expected; "}" ] in
      match Css.of_string ~strict:true css with
      | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)
      | Ok { stylesheet; _ } ->
          Alcotest.(check string)
            "edge-offset position sheet roundtrip" expected_css
            (String.trim (Css.to_string ~minify:true stylesheet)))
    [
      ("background-position:left top 10px", "background-position:left top 10px");
      ("background-position:left top 10%", "background-position:left top 10%");
      ( "background-position:center top 10px",
        "background-position:center top 10px" );
      ( "background-position:center left 10px",
        "background-position:center left 10px" );
      ("background-position:top 10px left", "background-position:top 10px left");
      ( "perspective-origin:left 10px top 20px",
        "perspective-origin:left 10px top 20px" );
      ("transform-origin:left 10px", "transform-origin:0% 10px");
      ("transform-origin:left top 10px", "transform-origin:left top 10px");
      ("transform-origin:center top 10px", "transform-origin:center top 10px");
    ];
  List.iter
    (fun declaration ->
      none_cursor read_declaration declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      match Css.of_string ~strict:true css with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "strict parsing accepted %s" css)
    [
      "transform-origin:foo 1px bar 2px";
      "transform-origin:left 1px top 2px";
      "transform-origin:right 10% bottom 20%";
      "transform-origin:top 2px left 1px";
      "transform-origin:left 1px top 2px 3px";
      "background-position:left 1px right 2px";
      "object-position:left 1px middle";
      "perspective-origin:top 1px bottom";
      "object-position:left 1px top";
      "perspective-origin:left top 1px";
    ]

(* Motion Path 1 secs. 2.3-2.4 define offset-position as [normal | auto |
   <position>] and offset-anchor as [auto | <position>]. Both use the generic
   <position> grammar, not background's three-value extension. *)
let offset_position_properties () =
  List.iter
    (fun (declaration, expected) ->
      check_declaration ~expected ~roundtrip:true declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      let expected_css = String.concat "" [ "a{"; expected; "}" ] in
      match Css.of_string ~strict:true css with
      | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)
      | Ok { stylesheet; _ } ->
          Alcotest.(check string)
            "offset position property sheet roundtrip" expected_css
            (String.trim (Css.to_string ~minify:true stylesheet)))
    [
      ("offset-anchor:auto", "offset-anchor:auto");
      ("offset-anchor:center", "offset-anchor:center");
      ("offset-anchor:left top", "offset-anchor:left top");
      ("offset-anchor:20% 30%", "offset-anchor:20%30%");
      ("offset-anchor:left 10px top 20px", "offset-anchor:left 10px top 20px");
      ("offset-position:normal", "offset-position:normal");
      ("offset-position:auto", "offset-position:auto");
      ("offset-position:center", "offset-position:center");
      ("offset-position:left top", "offset-position:left top");
      ("offset-position:20% 30%", "offset-position:20%30%");
      ( "offset-position:left 10px top 20px",
        "offset-position:left 10px top 20px" );
    ];
  decl_optimizes ~prop:"offset-anchor" ~into:"50%" "center";
  decl_optimizes ~prop:"offset-anchor" ~into:"0 0" "left top";
  decl_optimizes ~prop:"offset-anchor" ~into:"auto" "initial";
  decl_optimizes ~prop:"offset-position" ~into:"50%" "center";
  decl_optimizes ~prop:"offset-position" ~into:"0 0" "left top";
  decl_optimizes ~prop:"offset-position" ~into:"normal" "initial";
  List.iter
    (fun declaration ->
      none_cursor read_declaration declaration;
      let css = String.concat "" [ "a{"; declaration; "}" ] in
      match Css.of_string ~strict:true css with
      | Error _ -> ()
      | Ok _ -> Alcotest.failf "strict parsing accepted %s" css)
    [
      "offset-anchor:foo bar baz";
      "offset-anchor:normal";
      "offset-anchor:size";
      "offset-anchor:auto center";
      "offset-anchor:left top 10px";
      "offset-position:foo bar baz";
      "offset-position:none";
      "offset-position:normal center";
      "offset-position:left top 10px";
      "object-position:normal";
    ]

(* CSS Syntax 3 (ED) sec. 5.5.6 "consume a declaration" reads the value with
   [<semicolon-token>] as the stop token, then removes a trailing [!]
   [important] pair from that value and sets the declaration's important flag
   instead. A property grammar therefore never sees a [;] or an [!important],
   and neither may change what the value in front of it parses to. *)
let value_tail_forms property value =
  let bare = String.concat "" [ property; ":"; value ] in
  [
    (bare, false);
    (bare ^ ";", false);
    (bare ^ " !important", true);
    (bare ^ "!important", true);
    (bare ^ " !important;", true);
    (bare ^ " !IMPORTANT", true);
  ]

let read_one_declaration what input =
  let c = Cursor.of_string input in
  match read_declaration c with
  | Some decl -> decl
  | None -> Alcotest.failf "%s: %S read no declaration" what input
  | exception Error.Parse_error e ->
      Alcotest.failf "%s: %S was rejected: %s" what input (Error.to_string e)

(* The value a declaration holds, and its important flag, are independent of the
   tail the declaration consumer strips. Compared against the tail-free spelling
   rather than a pinned string, so the invariant is what is asserted. *)
let check_value_end property value =
  let bare = String.concat "" [ property; ":"; value ] in
  let expected =
    Css.Declaration.string_of_value ~minify:true
      (read_one_declaration "value end" bare)
  in
  List.iter
    (fun (input, important) ->
      let decl = read_one_declaration "value end" input in
      Alcotest.(check string)
        (Fmt.str "%S holds the value of %S" input bare)
        expected
        (Css.Declaration.string_of_value ~minify:true decl);
      Alcotest.(check bool)
        (Fmt.str "%S important flag" input)
        important
        (Css.Declaration.is_important decl);
      (* A declaration cascade prints has to read back as itself: the minified
         spelling of an important declaration ends in [!important], which is the
         very tail the reader has to see past. *)
      let printed = Css.Declaration.to_string ~minify:true decl in
      Alcotest.(check string)
        (Fmt.str "%S reparses" printed)
        printed
        (Css.Declaration.to_string ~minify:true
           (read_one_declaration "value end reparse" printed)))
    (value_tail_forms property value)

(* One reader per spelling of the end-of-value question that was open-coded:
   readers that ended on cursor exhaustion alone dropped the declaration over a
   [;] as well as over an [!important], readers that also peeked for a [;]
   dropped it over an [!important] only. Each entry is a property whose grammar
   has an optional trailing component, so the reader has to ask the question at
   all. *)
let value_end_properties =
  [
    ("font-style", "oblique");
    ("caret", "red");
    ("color-scheme", "dark");
    ("rotate", "45deg");
    ("clip-path", "border-box");
    ("animation-range", "normal");
    ("contain-intrinsic-size", "auto 300px");
    ("text-box", "none");
    ("hyphenate-limit-chars", "6");
    ("initial-letter", "2 3");
    ("justify-items", "legacy");
    ("overflow-clip-margin", "1px");
    ("grid-row", "span 2");
    ("border-image-slice", "30%");
    ("size", "A4");
    ("mask-border-slice", "30%");
    ("column-rule", "1px");
    ("text-emphasis", "filled");
    ("scale", "2");
    ("transform-origin", "left");
    (* Controls: readers that already spelled the question with all three of its
       answers, and a value with no optional tail at all. *)
    ("overflow", "hidden");
    ("color", "red");
  ]

let declaration_value_end () =
  List.iter
    (fun (property, value) -> check_value_end property value)
    value_end_properties

(* The pretty spelling of a declaration ends in a [;], so a reader that stops at
   the value's last component and then fails on the [;] cannot read back what
   cascade itself wrote. *)
let declaration_value_end_sheet () =
  List.iter
    (fun (property, value) ->
      List.iter
        (fun (decl, _) ->
          let css = String.concat "" [ "x{"; decl; "}" ] in
          match Css.of_string ~strict:true css with
          | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)
          | Ok { stylesheet; _ } -> (
              let minified =
                String.trim (Css.to_string ~minify:true stylesheet)
              in
              let pretty = Css.to_string ~minify:false stylesheet in
              match Css.of_string ~strict:true pretty with
              | Error e ->
                  Alcotest.failf "reparse of %S: %s" pretty (Error.to_string e)
              | Ok p ->
                  Alcotest.(check string)
                    (Fmt.str "%S survives a pretty round trip" css)
                    minified
                    (String.trim (Css.to_string ~minify:true p.stylesheet))))
        (value_tail_forms property value))
    value_end_properties

(* Seeing past the tail is not licence to accept it: a [!] that does not open
   [!important], a second [!important], a value component after one, and a value
   with more components than the grammar has slots all stay invalid. *)
let declaration_value_end_negatives () =
  List.iter
    (neg_cursor read_declaration)
    [
      "color:red !foo";
      "color:red !important !important";
      "color:red !important blue";
      "font-style:oblique !";
      "text-box:none !foo";
      "rotate:45deg 45deg 45deg 45deg";
      "rotate:45deg 45deg !important";
      "initial-letter:2 3 4";
      "hyphenate-limit-chars:2 3 4 5";
      "font-style:oblique 20deg 30deg 40deg";
      (* CSS Sizing 4 sec. 5.2 spells [contain-intrinsic-size] as [[ auto? [
         none | <length [0,inf]> ] ]{1,2}], so a third slot is one too many. *)
      "contain-intrinsic-size:auto 300px 400px 500px";
      "animation-range:normal normal normal";
    ]

(* CSS Syntax 3 (ED) sec. 7.2 excludes a [<bad-string-token>], a
   [<bad-url-token>] and an unmatched [)], []] or [}] from a
   [<declaration-value>], and a custom property takes [<declaration-value>?] and
   nothing wider. The exclusion reaches inside a block, so ONE such token among
   the components of a block is enough to refuse the value: the question asked
   of a block's components is whether any of them leaves the grammar, not
   whether all of them do. An empty block leaves nothing, so it is a value. *)
let custom_property_block_components () =
  List.iter (neg_cursor read_declaration) [ "--x:(red ])"; "--x:[a}b]" ];
  check_declaration ~expected:"--x:()" "--x: ()";
  check_declaration ~expected:"--x:(red)" "--x: (red)"

(* A cursor over a function's arguments is asking a different question: its
   grammar ends at the closing paren, where neither a [;] nor an [!] can stand,
   so it must still be read to true end of input. Widening it would accept an
   argument list longer than the function takes. *)
let function_argument_end_negatives () =
  List.iter
    (neg_cursor read_declaration)
    [
      "transform:skew(10deg,red)";
      "transform:matrix(1,2,3,4,5,6,7)";
      "transform:translate(1px,2px,3px)";
      "clip-path:inset(1px 2px 3px 4px 5px)";
      "transform:rotate(45deg,45deg)";
      "background:linear-gradient(red,blue) extra";
    ]

(* CSS Shapes 1 sec. 6.1: [shape-outside] is [none | [<basic-shape> ||
   <shape-box>] | <image>]. The reader only looked at the first component and
   only knew [none], [circle()] and [inset()] there, so a reference box, a
   box/shape pair, the other basic shapes and an image were all rejected and the
   declaration dropped. *)
let shape_outside_grammar () =
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [
      "shape-outside:none";
      "shape-outside:inherit";
      "shape-outside:var(--shape)";
      (* <basic-shape> *)
      "shape-outside:circle(50%)";
      "shape-outside:circle()";
      "shape-outside:circle(50% at 20% 30%)";
      "shape-outside:ellipse(closest-side farthest-side at 10px 20px)";
      "shape-outside:inset(10px round 2px)";
      "shape-outside:polygon(0 0,100% 0,100% 100%)";
      "shape-outside:xywh(0 0 100% 100%)";
      (* <shape-box> on its own, and either order in the [||] pair *)
      "shape-outside:margin-box";
      "shape-outside:content-box";
      "shape-outside:circle() border-box";
      "shape-outside:padding-box circle(50%)";
      (* <image> *)
      "shape-outside:url(shape.png)";
      "shape-outside:linear-gradient(red,blue)";
    ];
  (* Controls: an unknown keyword is no part of the grammar, [none] and a box
     are alternatives rather than a [||] pair, the box appears once, and
     [<shape-box>] excludes the three SVG boxes [<geometry-box>] adds. *)
  List.iter
    (neg_cursor read_declaration)
    [
      "shape-outside:not-a-shape";
      "shape-outside:none margin-box";
      "shape-outside:margin-box border-box";
      "shape-outside:circle(50%) fill-box";
    ]

let shape_outside_sheet () =
  List.iter
    (check_sheet_roundtrip "shape-outside")
    [
      "a{shape-outside:none}";
      "a{shape-outside:margin-box}";
      "a{shape-outside:circle(50%) content-box}";
      "a{shape-outside:url(shape.png)}";
    ]

(* Every property that reads a [<line-width>]: the four physical longhands and
   their logical counterparts, the 1-4 value shorthand, the two-value logical
   shorthands, and the width slot of [border] and of [outline]. *)
let line_width_sites value =
  List.map
    (fun property -> String.concat "" [ property; ":"; value ])
    [
      "border-width";
      "border-top-width";
      "border-right-width";
      "border-bottom-width";
      "border-left-width";
      "border-block-start-width";
      "border-block-end-width";
      "border-inline-start-width";
      "border-inline-end-width";
      "border-block-width";
      "border-inline-width";
      "border";
      "outline-width";
      "outline";
    ]

(* A [<length>] site reading the same comparison, for contrast. *)
let length_math_sites value =
  List.map
    (fun property -> String.concat "" [ property; ":"; value ])
    [ "margin"; "margin-top"; "width" ]

(* CSS Values 4 sec. 10.2 gives [min()] / [max()] a comma-separated list of
   [<calc-sum>] and [clamp()] exactly three arguments, and CSS Syntax 3 sec. 8.2
   drops a declaration whose value is invalid. The [<length>] sites answer that
   way already. The [<line-width>] sites read arguments up to the first one they
   could not read and compared those, so [max(1px,red)] answered [1px]: a
   narrower comparison than the one written, and one the author never asked
   for. *)
let line_width_invalid_math_argument () =
  List.iter
    (fun value ->
      List.iter (neg_cursor read_declaration) (line_width_sites value);
      List.iter (neg_cursor read_declaration) (length_math_sites value))
    [
      "max(1px,red)";
      "min(3px,red,1px)";
      "max(1px 2px)";
      "clamp(1px,2px,3px,4px)";
    ];
  (* Controls: two bounds no unit relates stand as written at every site, and
     the 1-4 value shorthand still takes one per side. *)
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    (line_width_sites "max(3dvh,4px)" @ length_math_sites "max(3dvh,4px)");
  check_declaration ~roundtrip:true "border-width:1px max(3dvh,4px)"

(* [skew()] (CSS Transforms 1 sec. 13.2) takes one or two [<angle>], [matrix()]
   (sec. 12.1) exactly six [<number>], [matrix3d()] (CSS Transforms 2 sec. 12.2)
   exactly sixteen, and [repeat()] (CSS Grid 1 sec. 7.2.3) a count and a track
   list; a trailing argument outside that grammar is invalid (CSS Syntax 3 sec.
   8.2), which invalidates the declaration. The same [Cursor.call] gap #617
   closed for the [<line-width>] readers left these four reading only up to the
   first argument they could not read and answering with a truncated function,
   e.g. [transform: skew(10deg, red)] as [skew(10deg)]. *)
let function_argument_validity () =
  List.iter
    (neg_cursor read_declaration)
    [
      "transform:skew(10deg,red)";
      "transform:matrix(1,2,3,4,5,6,red)";
      "transform:matrix3d(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,red)";
      "grid-template-columns:repeat(2,1fr red)";
      "grid-template-columns:repeat(2,1fr,red)";
    ];
  (* Controls: a valid call at every site still stands, including interior
     whitespace around commas and parens. *)
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [
      "transform:skew(10deg)";
      "transform:skew(10deg,20deg)";
      "transform:matrix(1,2,3,4,5,6)";
      "transform:matrix3d(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)";
      "grid-template-columns:repeat(2,1fr)";
    ];
  check_declaration ~expected:"transform:skew(10deg,20deg)"
    "transform:skew( 10deg , 20deg )"

(* CSS list productions are non-empty unless their grammar says otherwise.
   Backgrounds 3 gives both longhands a comma-separated item list, CSS Values 4
   gives [hypot()] one or more calculations, and Grid 2 gives [repeat()] a
   positive explicit count followed by one or more tracks. *)
let non_empty_list_grammar () =
  List.iter
    (neg_cursor read_declaration)
    [
      "background-image:";
      "background-blend-mode:";
      "width:hypot()";
      "grid-template-columns:repeat(2,)";
      "grid-template-columns:repeat(0,1px)";
      "grid-template-columns:repeat(-2,1px)";
    ];
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [
      "background-image:none";
      "background-blend-mode:normal";
      "width:hypot(3px,4px)";
      "grid-template-columns:repeat(2,1px)";
    ];
  (* Grid 2 defines a line-name block with a [*] multiplier, so this is the one
     audited grammar list that deliberately remains empty. *)
  check_declaration ~expected:"grid-template-columns:[]1px"
    "grid-template-columns:[] 1px"

(* CSS Values 4 sec. 5.7.3: a [#] multiplier's comma never trails the last item.
   [min()]/[max()] take a comma-separated [<calc-sum>] list (sec. 10.2), and
   [matrix()]/[matrix3d()] (CSS Transforms 1 sec. 12.1, 2 sec. 12.2) a
   fixed-arity comma list of [<number>]; [list] committed a separator before it
   knew whether another item followed it, so [min(1px,)] read as [min(1px)] and
   [matrix(1,2,3,4,5,6,)] as [matrix(1,2,3,4,5,6)] instead of invalidating the
   declaration (CSS Syntax 3 sec. 8.2). *)
let list_trailing_separator_invalid () =
  List.iter
    (fun value ->
      List.iter (neg_cursor read_declaration) (line_width_sites value);
      List.iter (neg_cursor read_declaration) (length_math_sites value))
    [ "min(1px,)"; "min(1px,2px,)"; "max(1px,2px,)" ];
  List.iter
    (neg_cursor read_declaration)
    [
      "transform:matrix(1,2,3,4,5,6,)";
      "transform:matrix3d(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,)";
    ];
  (* Controls: no trailing separator, with and without interior whitespace, and
     a single-item list, all still parse. *)
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    (line_width_sites "min(1px)"
    @ length_math_sites "min(1px)"
    @ line_width_sites "min(1px,2px)"
    @ length_math_sites "min(1px,2px)");
  check_declaration ~expected:"border-width:min(1px,2px)"
    "border-width:min( 1px , 2px )";
  List.iter
    (fun css -> check_declaration ~roundtrip:true css)
    [ "transform:matrix(1,2,3,4,5,6)" ];
  check_declaration ~expected:"transform:matrix(1,2,3,4,5,6)"
    "transform:matrix( 1 , 2 , 3 , 4 , 5 , 6 )"

(* CSS Backgrounds 3 sec. 5.1: [border-radius] is [<length-percentage
   [0,inf]>{1,4} [ / <length-percentage [0,inf]>{1,4} ]?], so an
   intrinsic-sizing keyword ([auto], [max-content], [stretch], ...) is no part
   of it. CSS Shapes 1 sec. 3.1 spells the rounded-corner suffix of a basic
   shape [round <'border-radius'>], and [<'property'>] (CSS Values 4 sec. 2.2)
   names that same value definition, so the keywords stay out there too. Chrome
   151 and WebKit 26.5 drop every declaration below. *)
let radius_keywords =
  [
    "auto";
    "none";
    "normal";
    "size";
    "max-content";
    "min-content";
    "fit-content";
    "-webkit-max-content";
    "-webkit-min-content";
    "-webkit-fit-content";
    "-moz-max-content";
    "-moz-min-content";
    "-moz-fit-content";
    "contain";
    "stretch";
    "from-font";
  ]

let radius_shorthand_sites radius =
  List.map
    (fun property -> String.concat "" [ property; ":"; radius ])
    [ "border-radius"; "-webkit-border-radius"; "-moz-border-radius" ]

(* Every [round <'border-radius'>] site: the basic shapes behind [clip-path] and
   [shape-outside], and the [rect()] / [xywh()] of [object-view-box]. *)
let radius_nested_sites radius =
  List.map
    (fun (before, after) -> String.concat "" [ before; radius; after ])
    [
      ("clip-path:inset(0 round ", ")");
      ("clip-path:rect(0 1px 1px 0 round ", ")");
      ("clip-path:xywh(0 0 1px 1px round ", ")");
      ("shape-outside:inset(0 round ", ")");
      ("object-view-box:rect(0 1px 1px 0 round ", ")");
      ("object-view-box:xywh(0 0 1px 1px round ", ")");
    ]

let border_radius_keyword_radii () =
  List.iter
    (fun radius ->
      List.iter (neg_cursor read_declaration) (radius_shorthand_sites radius);
      List.iter (neg_cursor read_declaration) (radius_nested_sites radius))
    radius_keywords;
  (* Controls: a length and a percentage are radii everywhere, a negative one is
     a radius nowhere. *)
  List.iter
    (fun radius ->
      List.iter
        (fun css -> check_declaration ~roundtrip:true css)
        (radius_shorthand_sites radius @ radius_nested_sites radius))
    [ "10px"; "10%" ];
  List.iter (neg_cursor read_declaration) (radius_shorthand_sites "-5px");
  List.iter (neg_cursor read_declaration) (radius_nested_sites "-5px")

(* CSS Cascade 5 sec. 6: a CSS-wide keyword is only valid "as the entire
   property value", so it is a whole [border-radius] and never one radius inside
   a basic shape. Chrome 151 and WebKit 26.5 keep the shorthand and drop the
   nested form. *)
let border_radius_css_wide_keywords () =
  List.iter
    (fun radius ->
      List.iter
        (fun css -> check_declaration ~roundtrip:true css)
        (radius_shorthand_sites radius);
      List.iter (neg_cursor read_declaration) (radius_nested_sites radius))
    [ "inherit"; "initial"; "unset"; "revert"; "revert-layer" ]

(* Vendor-prefixed properties with a typed constructor need a reader arm as
   well: without one the dispatch falls through to "unsupported property reader"
   and the declaration is dropped. Each takes the same value type as the
   unprefixed property, so each reads with the unprefixed reader. *)
let vendor_prefixed_shorthands () =
  let values =
    [
      ("animation", "spin 1s");
      ("animation-delay", "1s");
      ("animation-direction", "reverse");
      ("animation-duration", "2s");
      ("animation-fill-mode", "forwards");
      ("animation-iteration-count", "infinite");
      ("animation-name", "spin");
      ("animation-play-state", "paused");
      ("animation-timing-function", "ease-in");
      ("transition", "all .3s");
      ("transition-delay", "1s");
      ("transition-duration", "2s");
      ("transition-property", "opacity");
      ("transition-timing-function", "linear");
      ("border-radius", "4px");
      ("box-shadow", "0 1px 2px #000");
      ("background-size", "cover");
      ("flex-direction", "column");
      ("flex-flow", "row wrap");
      ("flex-wrap", "wrap");
      ("justify-content", "space-between");
      ("align-content", "center");
      ("align-items", "center");
      ("align-self", "flex-end");
    ]
  in
  let prefixed prefix names =
    List.filter_map
      (fun (name, value) ->
        if List.mem name names then Some (prefix ^ name ^ ":" ^ value) else None)
      values
  in
  let cases =
    prefixed "-webkit-"
      [
        "animation-delay";
        "animation-direction";
        "animation-duration";
        "animation-fill-mode";
        "animation-iteration-count";
        "animation-name";
        "animation-play-state";
        "animation-timing-function";
        "transition-delay";
        "transition-duration";
        "transition-property";
        "transition-timing-function";
        "border-radius";
        "box-shadow";
        "background-size";
        "flex-direction";
        "flex-flow";
        "flex-wrap";
        "justify-content";
        "align-content";
        "align-items";
        "align-self";
      ]
    @ prefixed "-moz-"
        [
          "animation";
          "animation-delay";
          "animation-direction";
          "animation-duration";
          "animation-fill-mode";
          "animation-iteration-count";
          "animation-name";
          "animation-play-state";
          "animation-timing-function";
          "transition";
          "transition-delay";
          "transition-duration";
          "transition-property";
          "transition-timing-function";
          "border-radius";
          "box-shadow";
        ]
  in
  List.iter (fun css -> check_declaration ~roundtrip:true css) cases;
  List.iter
    (fun css -> check_sheet_roundtrip "vendor prefix" ("a{" ^ css ^ "}"))
    cases

let suite =
  ( "declaration_grammar",
    [
      test_case "parse_declaration" `Quick parse_declaration_case;
      test_case "vendor-prefixed shorthand readers" `Quick
        vendor_prefixed_shorthands;
      test_case "spec property grammar manifest" `Quick
        spec_property_grammar_manifest;
      test_case "scroll-margin negative lengths" `Quick scroll_margin_negative;
      test_case "scroll-margin negative lengths (sheet)" `Quick
        scroll_margin_negative_sheet;
      test_case "text-decoration optional line" `Quick
        text_decoration_optional_line;
      test_case "text decoration thickness range" `Quick
        text_decoration_thickness_range;
      test_case "text-decoration drained shorthand" `Quick
        text_decoration_drained_shorthand;
      test_case "white-space collapse only" `Quick white_space_collapse_only;
      test_case "border-image repeat only" `Quick border_image_repeat_only;
      test_case "timeline name only" `Quick timeline_name_only;
      test_case "view-timeline inset slot" `Quick view_timeline_inset_slot;
      test_case "text-box edge only" `Quick text_box_edge_only;
      test_case "text-box normal" `Quick text_box_normal;
      test_case "edge-offset position grammar" `Quick
        edge_offset_position_grammar;
      test_case "offset position properties" `Quick offset_position_properties;
      test_case "declaration value end" `Quick declaration_value_end;
      test_case "declaration value end (sheet)" `Quick
        declaration_value_end_sheet;
      test_case "declaration value end negatives" `Quick
        declaration_value_end_negatives;
      test_case "custom property block components" `Quick
        custom_property_block_components;
      test_case "function argument end negatives" `Quick
        function_argument_end_negatives;
      test_case "shape-outside grammar" `Quick shape_outside_grammar;
      test_case "shape-outside grammar (sheet)" `Quick shape_outside_sheet;
      test_case "line-width invalid math argument" `Quick
        line_width_invalid_math_argument;
      test_case "function argument validity" `Quick function_argument_validity;
      test_case "non-empty list grammar" `Quick non_empty_list_grammar;
      test_case "list rejects a trailing separator" `Quick
        list_trailing_separator_invalid;
      test_case "border-radius keyword radii" `Quick border_radius_keyword_radii;
      test_case "border-radius CSS-wide keywords" `Quick
        border_radius_css_wide_keywords;
    ] )
