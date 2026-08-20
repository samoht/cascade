open Cascade
open Css.Media

let feat f : t = Cond (Feature f)
let plain name value : t = feat (Plain (name_of_string name, value))
let print_type : t = Type { prefix = None; type_ = Print; trailing = None }
let not_print : t = Type { prefix = Some Not; type_ = Print; trailing = None }

let not_all_and f : t =
  Type { prefix = Some Not; type_ = All; trailing = Some (Feature f) }

let min_width px = plain "min-width" (Length (Css.Values.Px px))
let max_width px = plain "max-width" (Length (Css.Values.Px px))

let test_string_output () =
  Alcotest.(check string)
    "min-width" "(min-width: 640px)"
    (to_string (min_width 640.));
  Alcotest.(check string)
    "max-width" "(max-width: 768px)"
    (to_string (max_width 768.));
  Alcotest.(check string)
    "prefers-color-scheme" "(prefers-color-scheme: dark)"
    (to_string (plain "prefers-color-scheme" (Ident Dark)));
  Alcotest.(check string)
    "prefers-reduced-motion" "(prefers-reduced-motion: reduce)"
    (to_string (plain "prefers-reduced-motion" (Ident Reduce)));
  Alcotest.(check string) "print" "print" (to_string print_type);
  Alcotest.(check string)
    "media queries 5 dynamic range" "(dynamic-range: high)"
    (to_string (of_string "(dynamic-range: high)"));
  Alcotest.(check string)
    "media queries 5 reduced data" "(prefers-reduced-data: reduce)"
    (to_string (of_string "(prefers-reduced-data: reduce)"));
  Alcotest.(check string)
    "media queries range comparison" "(width >= 40em)"
    (to_string (of_string "(width >= 40em)"));
  Alcotest.(check string)
    "media queries chained range" "(30em <= width < 60em)"
    (to_string (of_string "(30em <= width < 60em)"));
  Alcotest.(check string)
    "media queries hdr" "(video-dynamic-range: high)"
    (to_string (of_string "(video-dynamic-range: high)"));
  Alcotest.(check string)
    "media queries update" "(update: fast)"
    (to_string (of_string "(update: fast)"));
  Alcotest.(check string)
    "media queries scripting" "(scripting: enabled)"
    (to_string (of_string "(scripting: enabled)"));
  Alcotest.(check string)
    "media queries prefers contrast" "(prefers-contrast: more)"
    (to_string (of_string "(prefers-contrast: more)"));
  Alcotest.(check string) "empty media query list" "" (to_string (List []))

let spec_media_l45_vectors () =
  let check_raw name input =
    Alcotest.(check string) name input (to_string (of_string input))
  in
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.row) ->
      check_raw row.branch row.input)
    Cascade_spec_inventory.Query_grammar.media_positive;
  Alcotest.(check string) "negated print" "not print" (to_string not_print);
  Alcotest.(check string)
    "negated min width" "not (min-width: 40px)"
    (to_string
       (Cond (Not (Feature (Plain (Min Width, Length (Css.Values.Px 40.)))))));
  Alcotest.(check string)
    "not min width rem" "not all and (min-width: 30rem)"
    (to_string (not_all_and (Plain (Min Width, Length (Css.Values.Rem 30.)))))

let spec_media_structural_vectors () =
  let open Css.Media in
  let length l = Length l in
  let check name input expected =
    let actual = of_string input in
    Alcotest.(check bool) name true (equal expected actual)
  in
  check "min-width plain feature" "(min-width: 640px)" (min_width 640.);
  check "max-width plain feature" "(max-width: 768px)" (max_width 768.);
  check "reduced motion feature" "(prefers-reduced-motion: reduce)"
    (plain "prefers-reduced-motion" (Ident Reduce));
  check "print media type" "print" print_type;
  check "empty media query list" "" (List []);
  check "negated print media type" "not print" not_print;
  check "negated min-width shorthand" "not all and (min-width: 40px)"
    (not_all_and (Plain (Min Width, length (Css.Values.Px 40.))));
  check "name first range" "(width > 40em)"
    (feat (Range (Width, Gt, length (Css.Values.Em 40.))));
  check "value first range" "(40em < width)"
    (feat (Range_rev (length (Css.Values.Em 40.), Lt, Width)));
  check "interval range" "(30em <= width < 60em)"
    (feat
       (Interval
          (length (Css.Values.Em 30.), Le, Width, Lt, length (Css.Values.Em 60.))));
  check "media type with trailing condition" "screen and (hover: hover)"
    (Type
       {
         prefix = None;
         type_ = Screen;
         trailing = Some (Feature (Plain (Hover, Ident Hover)));
       });
  check "media query list" "screen and (width >= 40em), print"
    (List
       [
         Type
           {
             prefix = None;
             type_ = Screen;
             trailing =
               Some (Feature (Range (Width, Ge, length (Css.Values.Em 40.))));
           };
         print_type;
       ])

let spec_media_error_recovery_vectors () =
  let check_recovers name input =
    Alcotest.(check string) name "not all" (to_string (of_string input))
  in
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.invalid_row) ->
      check_recovers row.branch row.input)
    Cascade_spec_inventory.Query_grammar.media_negative;
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.row) ->
      Alcotest.(check string)
        row.branch row.expected
        (to_string (of_string row.input)))
    Cascade_spec_inventory.Query_grammar.media_recovery

let test_kind () =
  Alcotest.(check bool)
    "min-width is responsive" true
    (match kind (min_width 640.) with Responsive _ -> true | _ -> false);
  Alcotest.(check bool)
    "prefers-color-scheme is appearance" true
    (match kind (plain "prefers-color-scheme" (Ident Dark)) with
    | Preference_appearance -> true
    | _ -> false);
  Alcotest.(check bool)
    "prefers-reduced-motion is accessibility" true
    (match kind (plain "prefers-reduced-motion" (Ident Reduce)) with
    | Preference_accessibility -> true
    | _ -> false)

let test_compare () =
  let cmp = compare (min_width 640.) (min_width 768.) in
  Alcotest.(check bool) "640 < 768" true (cmp < 0)

let test_spec_media_sorting_edges () =
  let sorted =
    List.sort compare
      [
        plain "prefers-color-scheme" (Ident Dark);
        min_width 768.;
        plain "hover" (Ident Hover);
        max_width 1024.;
        plain "prefers-reduced-motion" (Ident Reduce);
        print_type;
        not_all_and (Plain (Min Width, Length (Css.Values.Px 768.)));
        min_width 320.;
      ]
    |> List.map to_string
  in
  Alcotest.(check (list string))
    "media ordering keeps interaction, other, preferences, and responsive \
     buckets stable"
    [
      "(hover: hover)";
      "print";
      "(prefers-reduced-motion: reduce)";
      "not all and (min-width: 768px)";
      "(max-width: 1024px)";
      "(min-width: 320px)";
      "(min-width: 768px)";
      "(prefers-color-scheme: dark)";
    ]
    sorted

let test_sort_key () =
  let qs =
    [
      plain "prefers-color-scheme" (Ident Dark);
      min_width 768.;
      plain "hover" (Ident Hover);
      max_width 1024.;
      plain "prefers-reduced-motion" (Ident Reduce);
      print_type;
      not_all_and (Plain (Min Width, Length (Css.Values.Px 768.)));
      min_width 320.;
    ]
  in
  List.iter
    (fun a ->
      List.iter
        (fun b ->
          Alcotest.(check int)
            "compare_keys (sort_key a) (sort_key b) = compare a b" (compare a b)
            (compare_keys (sort_key a) (sort_key b)))
        qs)
    qs;
  Alcotest.(check (list string))
    "sort_by reproduces List.sort compare"
    (List.sort compare qs |> List.map to_string)
    (sort_by Fun.id qs |> List.map to_string)

let spec_media_context_vectors () =
  let check condition expected =
    Alcotest.(check string)
      "media condition syntax" expected (to_string condition)
  in
  List.iter
    (fun (condition, expected) -> check condition expected)
    [
      (print_type, "print");
      (min_width 640., "(min-width: 640px)");
      (max_width 640., "(max-width: 640px)");
      (of_string "(width >= 40em)", "(width >= 40em)");
      (of_string "(30em <= width < 60em)", "(30em <= width < 60em)");
      ( of_string "(prefers-reduced-data: reduce)",
        "(prefers-reduced-data: reduce)" );
      (of_string "(dynamic-range: high)", "(dynamic-range: high)");
      (not_print, "not print");
    ]

let spec_media_query_vectors () =
  List.iter
    (fun (row : Cascade_spec_inventory.Query_grammar.row) ->
      Alcotest.(check string)
        row.branch row.expected
        (to_string (of_string row.input)))
    Cascade_spec_inventory.Query_grammar.media_positive

(* Media Queries 4 3.1 [<general-enclosed>]: both the [<function-token>] form
   and the [( <any-value> )] form are grammatical. An unrecognised one is
   [unknown], which becomes false where a boolean is expected, so the query
   never matches -- but the rule itself is valid and must survive parsing
   verbatim. Rejecting it as malformed costs the whole rule. *)
let general_enclosed_roundtrip () =
  let check src = Alcotest.(check string) src src (to_string (of_string src)) in
  (* function-token form *)
  check "theme(static)";
  check "foo(bar)";
  check "unknown-fn(1 2 3)";
  check "supports-something(a: b)";
  (* nested parentheses inside the arguments *)
  check "foo(bar(baz))";
  (* the ( <ident> ... ) form already parsed; keep it covered *)
  check "(unknown-feature: 1)";
  check "(unknown-boolean)"

let general_enclosed_in_context () =
  let check src = Alcotest.(check string) src src (to_string (of_string src)) in
  (* negated, listed and combined with a real query *)
  check "not theme(static)";
  check "screen and theme(static)";
  check "theme(static), print";
  check "print, theme(static)";
  (* [<media-and>] takes a [<media-in-parens>], so a bare media type cannot
     follow [and]: that is malformed and becomes [not all], unlike the
     grammatical forms above. *)
  Alcotest.(check string)
    "condition and media type is malformed" "not all"
    (to_string (of_string "theme(static) and screen"))

(* A function token must not be mistaken for a media type: [theme(static)] is a
   condition, where [theme] alone would be a type. *)
let general_enclosed_is_not_a_media_type () =
  Alcotest.(check string)
    "bare ident stays a media type" "theme"
    (to_string (of_string "theme"));
  Alcotest.(check string)
    "function token is a condition" "theme(static)"
    (to_string (of_string "theme(static)"));
  (* real media types and features keep working *)
  Alcotest.(check string) "screen" "screen" (to_string (of_string "screen"));
  Alcotest.(check string)
    "print and feature" "print and (min-width: 30em)"
    (to_string (of_string "print and (min-width: 30em)"))

let component_parser_edges () =
  let check name expected input =
    Alcotest.(check string) name expected (to_string (of_string input))
  in
  check "escaped feature value" "(prefers-color-scheme: dark)"
    "(prefers-color-scheme: d\\61 rk)";
  check "escaped conjunction" "screen and (width >= 40em)"
    "screen \\61 nd (width >= 40em)";
  check "parenthesis in a string" "theme(\") and (\")" "theme(\") and (\")";
  let source =
    "@media screen \\61 nd (prefers-color-scheme:d\\61 rk){a{color:red}}"
  in
  match Css.of_string ~strict:false source with
  | Error error -> Alcotest.fail (Cascade.Error.to_string error)
  | Ok parsed ->
      Alcotest.(check string)
        "stylesheet prelude"
        "@media screen and (prefers-color-scheme:dark){a{color:red}}"
        (Css.to_string ~minify:true parsed.stylesheet)

let suite =
  let open Alcotest in
  ( "media",
    [
      test_case "to_string" `Quick test_string_output;
      test_case "spec media query level 4/5 vectors" `Quick
        spec_media_l45_vectors;
      test_case "spec media query structural vectors" `Quick
        spec_media_structural_vectors;
      test_case "spec media query error recovery vectors" `Quick
        spec_media_error_recovery_vectors;
      test_case "kind" `Quick test_kind;
      test_case "compare" `Quick test_compare;
      test_case "sort_key" `Quick test_sort_key;
      test_case "spec media sorting edges" `Quick test_spec_media_sorting_edges;
      test_case "spec media query context syntax vectors" `Quick
        spec_media_context_vectors;
      test_case "spec media query boolean and range vectors" `Quick
        spec_media_query_vectors;
      test_case "general-enclosed roundtrip" `Quick general_enclosed_roundtrip;
      test_case "general-enclosed in context" `Quick general_enclosed_in_context;
      test_case "general-enclosed is not a media type" `Quick
        general_enclosed_is_not_a_media_type;
      test_case "component parser edges" `Quick component_parser_edges;
    ] )
