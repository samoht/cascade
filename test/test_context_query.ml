open Cascade
open Context_helpers

let check_selector_match name ~ctx ~expected input =
  let selector = Css.Selector.of_string input in
  Alcotest.(check bool)
    name expected
    (Css.Context.matches_selector ctx selector)

let check_media_match name ~ctx ~expected input =
  let query = Css.Media.of_string input in
  Alcotest.(check bool) name expected (Css.Context.matches_media ctx query)

let check_supports_match name ~ctx ~expected input =
  let condition = Css.Supports.of_string input in
  Alcotest.(check bool)
    name expected
    (Css.Context.matches_supports ctx condition)

(* MQ4 sections 2.4.2, 3.1, and 5.1: boolean feature values, unknown
   propagation, and resolution units are independent parts of matching. *)
let media_boolean_values () =
  let ctx =
    Css.Context.query
      ~media_features:
        (List.map Css.Media.of_string
           [
             "(monochrome: 0)";
             "(color-index: 0)";
             "(grid: 0)";
             "(forced-colors: none)";
             "(prefers-reduced-motion: no-preference)";
             "(color: 8)";
           ])
      ()
  in
  List.iter
    (check_media_match "zero or none is false" ~ctx ~expected:false)
    [
      "(monochrome)";
      "(color-index)";
      "(grid)";
      "(forced-colors)";
      "(prefers-reduced-motion)";
    ];
  check_media_match "nonzero is true" ~ctx ~expected:true "(color)"

let media_unknown_logic () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ Css.Media.of_string "(width: 800px)" ]
      ()
  in
  List.iter
    (fun (input, expected) -> check_media_match input ~ctx ~expected input)
    [
      ("not (cascade-unknown: 1)", false);
      ("not (orientation: sideways)", false);
      ("not ((orientation: sideways) or (width < 0px))", false);
      ("not ((orientation: sideways) and (width < 0px))", true);
      ("(orientation: sideways) or (width >= 0px)", true);
      ("not screen and (orientation: sideways)", false);
      ("not print and (orientation: sideways)", true);
      ("", true);
    ]

let media_resolution_units () =
  let ctx =
    Css.Context.query
      ~media_features:[ Css.Media.of_string "(resolution: 1dppx)" ]
      ()
  in
  List.iter
    (check_media_match "equivalent resolution units" ~ctx ~expected:true)
    [
      "(resolution: 96dpi)";
      "(resolution: 1dppx)";
      "(resolution >= 37dpcm)";
      "(resolution < 38dpcm)";
    ];
  check_media_match "larger resolution does not match" ~ctx ~expected:false
    "(resolution: 192dpi)"

let media_inapplicable_feature () =
  let ctx = Css.Context.query ~media_inapplicable:[ Css.Media.Scan ] () in
  check_media_match "recognized inapplicable feature is false, not unknown" ~ctx
    ~expected:true "not (scan: progressive)";
  check_media_match "inapplicable boolean is false" ~ctx ~expected:false
    "(scan)";
  check_media_match "an absent feature remains unknown" ~ctx ~expected:false
    "not (cascade-unknown: 1)"

let check_container_match name ~ctx ?name:container_name ~expected input =
  let condition = Css.Container.of_string input in
  Alcotest.(check bool)
    name expected
    (Css.Context.matches_container ctx ?name:container_name condition)

(* Conditional Rules 5 section 5.4 selects a container supporting every queried
   feature before evaluating the boolean expression. *)
let container_feature_eligibility () =
  let ctx =
    Css.Context.query
      ~container_features:[ Css.Container.of_string "(width: 500px)" ]
      ()
  in
  List.iter
    (check_container_match "no eligible container" ~ctx ~expected:false)
    [
      "(width >= 400px) or (height >= 900px)";
      "(width >= 400px) or (cascade-unknown: 1)";
      "not (cascade-unknown: 1)";
      "not scroll-state(stuck: top)";
    ];
  check_container_match "eligible negation" ~ctx ~expected:true
    "not (width >= 900px)";
  let ctx =
    Css.Context.query
      ~container_features:
        [
          Css.Container.of_string "(width: 500px)";
          Css.Container.of_string "(height: 300px)";
        ]
      ()
  in
  check_container_match "both axes are eligible" ~ctx ~expected:true
    "(width >= 400px) or (height >= 900px)"

let test_document_selector_context_contract () =
  let ctx =
    Css.Context.document ~root:"html"
      ~scope:(Css.Selector.of_string ".card")
      ~element:"button" ~classes:[ "btn"; "primary" ] ~ids:[ "submit" ]
      ~attributes:
        [
          ("type", Some "submit");
          ("disabled", None);
          ("data-state", Some "ready");
        ]
      ~pseudo_classes:[ "focus-visible"; "enabled" ]
      ~pseudo_elements:[ "before" ] ()
  in
  check_selector_match "type selector matches element" ~ctx ~expected:true
    "button";
  check_selector_match "class selector matches class list" ~ctx ~expected:true
    ".primary";
  check_selector_match "id selector matches id list" ~ctx ~expected:true
    "#submit";
  check_selector_match "attribute presence selector matches" ~ctx ~expected:true
    "[disabled]";
  check_selector_match "attribute value selector matches" ~ctx ~expected:true
    "[type=submit]";
  check_selector_match "pseudo-class selector matches explicit state" ~ctx
    ~expected:true ":focus-visible";
  check_selector_match "pseudo-element selector matches explicit context" ~ctx
    ~expected:true "::before";
  check_selector_match "compound selector matches all simple selectors" ~ctx
    ~expected:true "button.btn.primary[type=submit]:focus-visible";
  check_selector_match "wrong element fails" ~ctx ~expected:false "a";
  check_selector_match "missing class fails" ~ctx ~expected:false ".secondary";
  check_selector_match "missing attribute value fails" ~ctx ~expected:false
    "[type=button]";
  check_selector_match "missing pseudo-class fails" ~ctx ~expected:false
    ":hover";
  check_selector_match "missing pseudo-element fails" ~ctx ~expected:false
    "::after"

let test_query_context_evaluation_contract () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:
        [
          feature "width" (px 1024.);
          feature "height" (px 768.);
          feature "prefers-color-scheme" (ident "dark");
          feature "dynamic-range" (ident "high");
        ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "container-type" "inline-size";
          Css.Supports.property "color" "oklch(50% 0.1 20)";
          Css.Supports.func "selector" ":has(img)";
        ]
      ~container_name:"card"
      ~container_features:
        [
          container_feature "inline-size" (px 640.);
          container_feature "block-size" (px 480.);
          Css.Container.style ~value:"dark" "--theme";
          Css.Container.scroll_state "stuck" "top";
        ]
      ()
  in
  check_media_match "media type and min-width match" ~ctx ~expected:true
    "screen and (min-width: 48em)";
  check_media_match "media range comparison matches" ~ctx ~expected:true
    "(width >= 1024px)";
  check_media_match "media feature equality matches" ~ctx ~expected:true
    "(prefers-color-scheme: dark)";
  check_media_match "media not operator negates" ~ctx ~expected:false
    "not screen";
  check_media_match "media range comparison fails" ~ctx ~expected:false
    "(width > 1200px)";
  check_supports_match "supported declaration matches" ~ctx ~expected:true
    "(display: grid)";
  check_supports_match "supported selector function matches" ~ctx ~expected:true
    "selector(:has(img))";
  check_supports_match "supports and combines true branches" ~ctx ~expected:true
    "(display: grid) and (container-type: inline-size)";
  check_supports_match "supports not negates unsupported branch" ~ctx
    ~expected:true "not (display: ruby)";
  check_supports_match "unsupported declaration fails" ~ctx ~expected:false
    "(display: ruby)";
  check_container_match "named container and width match" ~ctx ~name:"card"
    ~expected:true "(inline-size >= 40rem)";
  check_container_match "wrong container name fails" ~ctx ~name:"sidebar"
    ~expected:false "(inline-size >= 40rem)";
  check_container_match "style query matches explicit container state" ~ctx
    ~name:"card" ~expected:true "style(--theme: dark)";
  check_container_match "scroll-state query matches explicit container state"
    ~ctx ~name:"card" ~expected:true "scroll-state(stuck: top)";
  check_container_match "container range comparison fails" ~ctx ~name:"card"
    ~expected:false "(inline-size > 900px)"

let test_document_selector_function_contract () =
  let ctx =
    Css.Context.document ~element:"button" ~classes:[ "btn"; "primary" ]
      ~ids:[ "submit" ]
      ~attributes:[ ("aria-expanded", Some "true"); ("disabled", None) ]
      ~pseudo_classes:[ "enabled"; "focus-visible" ]
      ~pseudo_elements:[ "before" ] ()
  in
  check_selector_match ":is matches one matching branch" ~ctx ~expected:true
    ":is(.link, .btn)";
  check_selector_match ":where matches without changing context semantics" ~ctx
    ~expected:true ":where(button.primary)";
  check_selector_match ":not matches absent selector" ~ctx ~expected:true
    ":not(.secondary)";
  check_selector_match ":not fails when inner selector matches" ~ctx
    ~expected:false ":not(.btn)";
  check_selector_match "compound with :is and attribute matches" ~ctx
    ~expected:true "button:is(.primary)[aria-expanded=true]";
  check_selector_match "compound with :where and pseudo-class matches" ~ctx
    ~expected:true ":where(button.btn):enabled";
  check_selector_match "pseudo-element compound matches explicit element" ~ctx
    ~expected:true "button::before";
  check_selector_match "attribute exact value is case-sensitive by default" ~ctx
    ~expected:false "[aria-expanded=True]"

let test_query_context_boolean_contract () =
  let ctx =
    Css.Context.query ~media_type:"screen"
      ~media_features:
        [
          feature "width" (px 1024.);
          feature "height" (px 768.);
          feature "orientation" (ident "landscape");
          feature "hover" (ident "hover");
          feature "pointer" (ident "fine");
        ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "display" "flex";
          Css.Supports.property "color" "lab(50% 0 0)";
          Css.Supports.func "selector" ":is(.a, .b)";
          Css.Supports.func "font-format" "woff2";
        ]
      ~container_name:"card"
      ~container_features:
        [
          container_feature "inline-size" (px 720.);
          container_feature "block-size" (px 360.);
          Css.Container.style ~value:"compact" "--density";
        ]
      ()
  in
  check_media_match "media and chain matches" ~ctx ~expected:true
    "screen and (width >= 60em) and (orientation: landscape)";
  check_media_match "media comma list matches any branch" ~ctx ~expected:true
    "print, screen and (hover: hover)";
  check_media_match "media comma list fails all branches" ~ctx ~expected:false
    "print, speech";
  check_media_match "media pointer feature matches" ~ctx ~expected:true
    "(pointer: fine)";
  check_supports_match "supports or matches one branch" ~ctx ~expected:true
    "(display: ruby) or (display: grid)";
  check_supports_match "supports and fails when one branch fails" ~ctx
    ~expected:false "(display: grid) and (display: ruby)";
  check_supports_match "supports selector function exact match" ~ctx
    ~expected:true "selector(:is(.a, .b))";
  check_supports_match "unsupported function argument fails" ~ctx
    ~expected:false "selector(:has(img))";
  check_container_match "container block axis range matches" ~ctx ~name:"card"
    ~expected:true "(block-size >= 20rem)";
  check_container_match "container boolean and matches" ~ctx ~name:"card"
    ~expected:true "((inline-size >= 40rem) and style(--density: compact))";
  check_container_match "container boolean and fails one branch" ~ctx
    ~name:"card" ~expected:false
    "((inline-size >= 40rem) and style(--density: spacious))"

let selector_scope_contract () =
  let scoped =
    Css.Context.document ~root:"html"
      ~scope:(Css.Selector.of_string ".card")
      ~element:"section"
      ~classes:[ "card"; "group"; "is-open" ]
      ~ids:[ "billing" ]
      ~attributes:[ ("data-theme", Some "dark"); ("dir", Some "ltr") ]
      ~pseudo_classes:[ "focus-within" ] ()
  in
  check_selector_match ":root matches explicit root context" ~ctx:scoped
    ~expected:true ":root";
  check_selector_match ":scope matches explicit scope element" ~ctx:scoped
    ~expected:true ":scope";
  check_selector_match "scoped class compound matches" ~ctx:scoped
    ~expected:true ":scope.card[data-theme=dark]";
  check_selector_match "scope mismatch fails" ~ctx:scoped ~expected:false
    ":scope.modal";
  check_selector_match "dir attribute selector matches explicit context"
    ~ctx:scoped ~expected:true "[dir=ltr]";
  check_selector_match "group state selector matches explicit class/state"
    ~ctx:scoped ~expected:true ".group:focus-within";
  check_selector_match "missing group state fails" ~ctx:scoped ~expected:false
    ".group:hover";
  let rootless =
    Css.Context.document ~element:"section" ~classes:[ "card" ] ()
  in
  check_selector_match ":root fails without root context" ~ctx:rootless
    ~expected:false ":root";
  check_selector_match ":scope fails without scope context" ~ctx:rootless
    ~expected:false ":scope"

let suite =
  ( "context_query",
    [
      Alcotest.test_case "document selector context contract" `Quick
        test_document_selector_context_contract;
      Alcotest.test_case "query context evaluation contract" `Quick
        test_query_context_evaluation_contract;
      Alcotest.test_case "media boolean values" `Quick media_boolean_values;
      Alcotest.test_case "media unknown logic" `Quick media_unknown_logic;
      Alcotest.test_case "media resolution units" `Quick media_resolution_units;
      Alcotest.test_case "media inapplicable feature" `Quick
        media_inapplicable_feature;
      Alcotest.test_case "container feature eligibility" `Quick
        container_feature_eligibility;
      Alcotest.test_case "document selector function contract" `Quick
        test_document_selector_function_contract;
      Alcotest.test_case "query context boolean contract" `Quick
        test_query_context_boolean_contract;
      Alcotest.test_case "selector scope context contract" `Quick
        selector_scope_contract;
    ] )
