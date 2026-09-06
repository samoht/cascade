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

let pp_kind ppf : kind -> unit = function
  | Hover -> Fmt.string ppf "interaction"
  | Responsive (unit_ord, v) ->
      Fmt.pf ppf "lower width bound (%d, %g)" unit_ord v
  | Responsive_max (unit_ord, v) ->
      Fmt.pf ppf "upper width bound (%d, %g)" unit_ord v
  | Preference_accessibility -> Fmt.string ppf "accessibility preference"
  | Preference_appearance -> Fmt.string ppf "appearance preference"
  | Other -> Fmt.string ppf "other"

let kind_testable = Alcotest.testable pp_kind equal_kind

(* Media Queries 4 sec. 2.1 and sec. 3: [all] is the identity media type and
   [not] negates the condition it wraps, so [not all and (X)] and [not (X)]
   match the same viewports and a doubled [not] cancels. The bucket a query
   sorts into follows what it matches, never how it is spelled: [not (min-width:
   640px)] matches viewports narrower than 640px, bounding width from above like
   [(max-width: 640px)] rather than from below. *)
let kind_follows_meaning_not_spelling () =
  let kind_of source = kind (of_string source) in
  let check name expected source =
    Alcotest.check kind_testable name expected (kind_of source)
  in
  let bounds name side source =
    Alcotest.(check bool) name true (side (kind_of source))
  in
  let from_below = function Responsive _ -> true | _ -> false in
  let from_above = function Responsive_max _ -> true | _ -> false in
  (* Guards: the un-negated forms fix which constructor each side of a bound is,
     so the negated cases can be stated against them and a fix that flips every
     width query fails here. *)
  bounds "(min-width: 640px) bounds from below" from_below "(min-width: 640px)";
  bounds "(max-width: 640px) bounds from above" from_above "(max-width: 640px)";
  bounds "(width >= 640px) bounds from below" from_below "(width >= 640px)";
  bounds "(width < 640px) bounds from above" from_above "(width < 640px)";
  check "(hover) bounds no width" Hover "(hover)";
  check "print bounds no width" Other "print";
  let lower_640 = kind_of "(min-width: 640px)" in
  let upper_640 = kind_of "(width < 640px)" in
  (* One query, two spellings. *)
  check "not (min-width: 640px) matches width < 640px" upper_640
    "not (min-width: 640px)";
  check "not all and (min-width: 640px) matches width < 640px" upper_640
    "not all and (min-width: 640px)";
  (* Two negations cancel, leaving the lower bound they wrap. *)
  check "not (not (min-width: 640px)) matches width >= 640px" lower_640
    "not (not (min-width: 640px))";
  check "not all and not (min-width: 640px) matches width >= 640px" lower_640
    "not all and not (min-width: 640px)";
  (* A sort consumes [group_order], which reads the kind, so a query classified
     by spelling is grouped with the queries it is the complement of. *)
  let group source = fst (group_order (kind_of source)) in
  Alcotest.(check int)
    "not (min-width: 640px) groups with (max-width: 640px)"
    (group "(max-width: 640px)")
    (group "not (min-width: 640px)")

(* Media Queries 4 sec. 3: [not] matches the complement, and the complement of a
   width range is not a range. [not ((min-width: 640px) and (max-width:
   1024px))] matches the viewports narrower than 640px together with those wider
   than 1024px, and the [or] form matches nothing at all; neither is a single
   bound. Minification folds the two operands into the Level 4 interval and [not
   all and X] into [not X], so every spelling has to agree. *)
let negated_range_bounds_neither_side () =
  let kind_of source = kind (of_string source) in
  let bounds_neither name source =
    Alcotest.check kind_testable name Other (kind_of source)
  in
  (* Guard: the range on its own is a bound, so [Other] below comes from the
     negation and not from a shape the classifier never read. *)
  Alcotest.check kind_testable "(640px <= width <= 1024px) bounds from below"
    (kind_of "(min-width: 640px)")
    (kind_of "(640px <= width <= 1024px)");
  bounds_neither "not ((min-width: 640px) and (max-width: 1024px))"
    "not ((min-width: 640px) and (max-width: 1024px))";
  bounds_neither "not (640px <= width <= 1024px)"
    "not (640px <= width <= 1024px)";
  bounds_neither "not all and (min-width: 640px) and (max-width: 1024px)"
    "not all and (min-width: 640px) and (max-width: 1024px)";
  bounds_neither "not ((min-width: 640px) or (max-width: 1024px))"
    "not ((min-width: 640px) or (max-width: 1024px))"

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
    (Cascade_spec_inventory.Query_grammar.media_positive
   @ Cascade_spec_inventory.Query_grammar.media_general_enclosed)

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

(* MQ4 sections 3 and 3.1: a failed feature grammar falls back to
   general-enclosed; it must not invalidate an enclosing disjunction. *)
let general_enclosed_feature_fallback () =
  List.iter
    (fun input ->
      Alcotest.(check string) input input (to_string (of_string input)))
    [
      "(orientation: sideways) or (min-width: 0px)";
      "(width >=) or (color)";
      "(future syntax) or (color)";
      "((color) and) or (color)";
      "() or (color)";
    ]

let general_enclosed_in_context () =
  List.iter
    (fun input ->
      Alcotest.(check string)
        "bad token cannot be general-enclosed" "not all"
        (to_string (of_string input)))
    [
      "(future ]) or (color)";
      "future(]) or (color)";
      "future(url(a b)) or (color)";
      "future(\"a\nb\") or (color)";
    ];
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

(* Media Queries 4 sec. 3 [<media-not> = not <media-in-parens>]: the operand of
   [not] is one parenthesised block, and nothing may follow it at that level. A
   negated disjunction therefore keeps the wrapper the author wrote; without it
   the query no longer reads back. *)
let media_not_takes_media_in_parens () =
  let minified ?(enforce_spec = false) source =
    match Css.of_string ~strict:false source with
    | Error error -> Alcotest.fail (Cascade.Error.to_string error)
    | Ok parsed ->
        Css.optimize ~enforce_spec parsed.stylesheet
        |> Css.to_string ~minify:true ~enforce_spec
  in
  let check_modes name input ~default ~spec =
    Alcotest.(check string) (name ^ " default") default (minified input);
    Alcotest.(check string)
      (name ^ " enforce-spec") spec
      (minified ~enforce_spec:true input)
  in
  let negated_or =
    "@media not ((min-width:1px) or (max-width:2px)){a{color:red}}"
  in
  check_modes "negated disjunction" negated_or
    ~default:"@media not ((width>=1px)or (width<=2px)){a{color:red}}"
    ~spec:"@media not ((min-width:1px)or (max-width:2px)){a{color:red}}";
  (* Losing the wrapper costs the whole block: the reader rejects trailing
     content after [not <media-in-parens>]. *)
  Alcotest.(check string)
    "negated disjunction reparses"
    "@media not ((width>=1px)or (width<=2px)){a{color:red}}"
    (minified (minified negated_or));
  (* A single [<media-in-parens>] operand is already wrapped, so no second pair
     of parentheses appears. *)
  check_modes "negated single condition"
    "@media not (min-width:1px){a{color:red}}"
    ~default:"@media not (width>=1px){a{color:red}}"
    ~spec:"@media not (min-width:1px){a{color:red}}"

(* Media Queries 4 sec. 2.3: [all] is the identity media type, so [not all and
   (X)] and [not (X)] are the same query. Default minify already spends Level 3
   compatibility by lowering [min-width] to range syntax, so it takes the
   shorter Level 4 [not] as well; [--enforce-spec] keeps both Level 3
   spellings. *)
let negated_all_is_level4_not () =
  let minified ?(enforce_spec = false) css =
    match Css.of_string css with
    | Error error -> Alcotest.fail (Cascade.Error.to_string error)
    | Ok parsed ->
        Css.optimize ~enforce_spec parsed.stylesheet
        |> Css.to_string ~minify:true ~enforce_spec
        |> String.trim
  in
  let check_modes name input ~default ~spec =
    Alcotest.(check string) (name ^ " default") default (minified input);
    Alcotest.(check string)
      (name ^ " enforce-spec") spec
      (minified ~enforce_spec:true input)
  in
  check_modes "negated feature"
    "@media not all and (min-width:100px){a{color:red}}"
    ~default:"@media not (width>=100px){a{color:red}}"
    ~spec:"@media not all and (min-width:100px){a{color:red}}";
  (* [<media-type> and <media-condition-without-or>] forbids a top-level [or],
     so the inner parentheses are what keeps the moved condition grammatical
     under a bare [not]. *)
  check_modes "negated or condition"
    "@media not all and ((min-width:1px) or (max-width:2px)){a{color:red}}"
    ~default:"@media not ((width>=1px)or (width<=2px)){a{color:red}}"
    ~spec:"@media not all and ((min-width:1px)or (max-width:2px)){a{color:red}}";
  (* Bare [not all] has no condition form and matches nothing: verbatim. *)
  check_modes "bare not all" "@media not all{a{color:red}}"
    ~default:"@media not all{a{color:red}}" ~spec:"@media not all{a{color:red}}"

(* CSS Values 4 sec. 5 writes a zero [<length>] as the unitless number [0], and
   Media Queries 4 sec. 1.3 takes its units from CSS Values, so a length-typed
   media feature accepts a bare zero. Chrome matches [@media (min-width: 0)],
   [(min-height: 0)], [(width >= 0)], [(0 <= width)] and [@container (min-width:
   0)] against a live document; it does not match [(min-resolution: 0)] or
   [(min-width: 1)], where no unitless spelling exists. *)
let spec_media_unitless_zero_length () =
  let check_raw name input expected =
    Alcotest.(check string) name expected (to_string (of_string input))
  in
  check_raw "plain prefixed feature" "(min-width: 0)" "(min-width: 0px)";
  check_raw "plain height feature" "(min-height: 0)" "(min-height: 0px)";
  check_raw "plain equality feature" "(width: 0)" "(width: 0px)";
  check_raw "name-first range" "(width >= 0)" "(width >= 0px)";
  check_raw "value-first range" "(0 <= width)" "(0px <= width)";
  check_raw "interval range" "(0 <= width <= 60em)" "(0px <= width <= 60em)";
  check_raw "fractional zero" "(min-width: 0.0)" "(min-width: 0px)";
  check_raw "negative zero" "(min-width: -0)" "(min-width: 0px)";
  Alcotest.(check bool)
    "unitless zero is the zero length" true
    (equal (of_string "(min-width: 0)") (min_width 0.));
  (* The allowance is [<length>]-only: a [<resolution>] has no unitless zero,
     and a non-zero number is not a length in any feature. *)
  let check_recovers name input =
    Alcotest.(check string) name input (to_string (of_string input))
  in
  check_recovers "resolution has no unitless zero" "(min-resolution: 0)";
  check_recovers "non-zero number is not a length" "(min-width: 1)";
  check_recovers "non-zero number in a range" "(width >= 1)"

(* Media Queries 4 sec. 2.4.4 "Using 'min-' and 'max-' Prefixes On Range
   Features": "Using a 'min-' prefix on a feature name is equivalent to using
   the '>=' operator", and 'max-' is the '<=' operator. Two spellings of one
   query, so [equal] must not read them as two. Sec. 2.4.3 adds the value-first
   spelling and the two interval directions to the same class. *)
let equal_ignores_bound_spelling () =
  let same name a b =
    Alcotest.(check bool) name true (equal (of_string a) (of_string b))
  in
  same "min- prefix is the >= comparison" "(min-width: 10px)" "(width >= 10px)";
  same "max- prefix is the <= comparison" "(max-width: 10px)" "(width <= 10px)";
  same "value-first range" "(10px <= width)" "(width >= 10px)";
  same "value-first strict range" "(10px < width)" "(width > 10px)";
  same "min- prefix against the value-first spelling" "(min-width: 10px)"
    "(10px <= width)";
  same "descending interval" "(20em >= width >= 10em)" "(10em <= width <= 20em)";
  same "all is the identity media type" "all and (min-width: 10px)"
    "(width >= 10px)";
  (* An inclusive bound is not the strict one: normalising the spelling must not
     normalise away the comparison. *)
  let differ name a b =
    Alcotest.(check bool) name false (equal (of_string a) (of_string b))
  in
  differ "inclusive is not strict" "(min-width: 10px)" "(width > 10px)";
  differ "a lower bound is not an upper bound" "(min-width: 10px)"
    "(max-width: 10px)";
  differ "the bound value still counts" "(min-width: 10px)" "(min-width: 11px)"

(* An unknown media type never matches (Media Queries 4 sec. 3.2 error
   handling), so a query whose type is one escaped ident is not the query that
   spells the same characters as a type plus a condition. [equal] therefore
   cannot be an equality on serialised text, even though correct serialisation
   also keeps their spellings apart. *)
let equal_separates_an_escaped_media_type () =
  let escaped = of_string {|screen\ and\ \(min-width\:\ 10px\)|} in
  let real = of_string "screen and (min-width: 10px)" in
  Alcotest.(check bool)
    "the two queries keep distinct serialisations" false
    (String.equal (to_string escaped) (to_string real));
  Alcotest.(check bool)
    "an unknown media type is not a media type plus a condition" false
    (equal escaped real)

(* CSS Syntax 3 sec. 4.3.11 consumes escapes before Media sees an ident, while
   sec. 4.2 requires serialisation to produce a token stream that re-parses to
   the same tokens. This applies to unknown media types, feature names and
   feature values alike. Losing those token boundaries can turn an unknown
   condition into a real one, after which a second minify pass merges blocks
   that the first pass correctly kept apart. *)
let escaped_identifiers_survive_emission () =
  let check_roundtrip name source =
    let parsed = of_string source in
    let reparsed = parsed |> to_string ~minify:true |> of_string in
    Alcotest.(check bool) name true (equal parsed reparsed)
  in
  check_roundtrip "escaped media type" {|screen\ and\ \(min-width\:\ 10px\)|};
  check_roundtrip "escaped feature name" {|(width\ \>\=\ 10px)|};
  check_roundtrip "escaped feature value" {|(future: light\ or\ dark)|};
  let minify source =
    source
    |> Css.of_string_exn ~strict:false
    |> Css.optimize |> Css.to_string ~minify:true
  in
  let source =
    "@media(width\\ \\>\\=\\ \
     10px){.a{color:red}}@media(width>=10px){.b{color:blue}}"
  in
  let once = minify source in
  Alcotest.(check string)
    "a second minify pass preserves the two conditions" once (minify once)

(* An opaque condition carries no structure to reason over, so it equals an
   identical spelling and nothing else. Reflexivity is not optional: [equal] is
   an equality, and the duplicate scan in [Block] reads a query against
   itself. *)
let equal_on_opaque_conditions () =
  let opaque = of_string "theme(static)" in
  Alcotest.(check bool)
    "an opaque condition equals itself" true
    (equal opaque (of_string "theme(static)"));
  Alcotest.(check bool)
    "an opaque condition does not equal a different spelling" false
    (equal opaque (of_string "theme( static )"));
  Alcotest.(check bool)
    "an opaque condition does not equal a different one" false
    (equal opaque (of_string "theme(dynamic)"));
  (* Nothing in an opaque condition is read, so it does not equal the feature
     whose serialisation it happens to share. *)
  Alcotest.(check bool)
    "an opaque condition is not the feature it spells" false
    (equal
       (of_string "(unknown-feature: 1)")
       (feat (General_enclosed "(unknown-feature: 1)")))

(* A normal form is already normal, and it is still the query it came from:
   reparsing what [normalize] printed lands back on the same normal form. *)
let normalize_is_idempotent () =
  List.iter
    (fun input ->
      let once = normalize (of_string input) in
      Alcotest.(check string)
        ("normalize is idempotent: " ^ input)
        (to_string once)
        (to_string (normalize once));
      Alcotest.(check bool)
        ("a normal form reparses to itself: " ^ input)
        true
        (equal once (of_string (to_string once))))
    [
      "(min-width: 10px)";
      "(width >= 10px)";
      "(10px <= width)";
      "(20em >= width >= 10em)";
      "all and (min-width: 10px)";
      "not all and (min-width: 10px)";
      "screen and (max-width: 40em)";
      "(min-width: 10px) and (orientation: landscape)";
      "not (min-width: 10px)";
      "print, screen and (min-width: 10px)";
      "theme(static)";
    ]

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
      test_case "spec media unitless zero length" `Quick
        spec_media_unitless_zero_length;
      test_case "kind" `Quick test_kind;
      test_case "kind follows meaning not spelling" `Quick
        kind_follows_meaning_not_spelling;
      test_case "negated range bounds neither side" `Quick
        negated_range_bounds_neither_side;
      test_case "compare" `Quick test_compare;
      test_case "sort_key" `Quick test_sort_key;
      test_case "spec media sorting edges" `Quick test_spec_media_sorting_edges;
      test_case "spec media query context syntax vectors" `Quick
        spec_media_context_vectors;
      test_case "spec media query boolean and range vectors" `Quick
        spec_media_query_vectors;
      test_case "general-enclosed roundtrip" `Quick general_enclosed_roundtrip;
      test_case "general-enclosed feature fallback" `Quick
        general_enclosed_feature_fallback;
      test_case "general-enclosed in context" `Quick general_enclosed_in_context;
      test_case "general-enclosed is not a media type" `Quick
        general_enclosed_is_not_a_media_type;
      test_case "component parser edges" `Quick component_parser_edges;
      test_case "media-not takes a media-in-parens" `Quick
        media_not_takes_media_in_parens;
      test_case "negated all is level 4 not" `Quick negated_all_is_level4_not;
      test_case "equal ignores bound spelling" `Quick
        equal_ignores_bound_spelling;
      test_case "equal separates an escaped media type" `Quick
        equal_separates_an_escaped_media_type;
      test_case "escaped identifiers survive emission" `Quick
        escaped_identifiers_survive_emission;
      test_case "equal on opaque conditions" `Quick equal_on_opaque_conditions;
      test_case "normalize is idempotent" `Quick normalize_is_idempotent;
    ] )
