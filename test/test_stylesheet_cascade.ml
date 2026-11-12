(** Stylesheet tests: cascade layers and the value stages of CSS Cascade. *)

open Cascade
open Css.Stylesheet
open Css_test_helpers
open Stylesheet_helpers

(* Not a roundtrip test *)
let test_layer_roundtrip () =
  let test_css ~expected input =
    let r = Cursor.of_string input in
    try
      let stylesheet = Css.Stylesheet.read r in
      let roundtrip =
        String.trim (Css.Stylesheet.to_string ~minify:true stylesheet)
      in
      Alcotest.(check string)
        ("layer roundtrip for " ^ input)
        expected roundtrip
    with Cursor.Parse_error err ->
      Alcotest.fail ("Failed to parse " ^ input ^ ": " ^ Error.to_string err)
  in
  (* Layer statement form should roundtrip as-is *)
  test_css ~expected:"@layer components,utilities;"
    "@layer components,utilities;";
  (* Empty layer blocks should be minified to statement form *)
  test_css ~expected:"@layer components;@layer utilities;"
    "@layer components{}@layer utilities{}"

(* Not a roundtrip test *)
let c64_layer_name_syntax () =
  (* CSS Cascade section 6.4.2: a layer name is a dot-separated list of idents
     with no whitespace around dots; CSS-wide keywords are reserved. *)
  check_stylesheet ~expected:"@layer framework.theme{blockquote{display:block}}"
    "@layer framework.theme { blockquote { display: block } }";
  check_stylesheet ~expected:"@layer framework.base,framework.theme;"
    "@layer framework.base, framework.theme;";
  check_stylesheet
    ~expected:
      "@layer reset.type{strong{font-weight:bold}}@layer \
       reset{[hidden]{display:none}}"
    "@layer reset.type { strong { font-weight: bold } } @layer reset { \
     [hidden] { display: none } }";
  neg_cursor read "@layer framework . theme { blockquote { display: block } }";
  neg_cursor read "@layer initial { blockquote { display: block } }";
  neg_cursor read
    "@layer framework.revert-layer { blockquote { display: block } }"

(* Not a roundtrip test *)
let c64_layer_nesting_examples () =
  (* CSS Cascade sections 6.4.2 and 6.4.3: dotted layer names are shorthand for
     nested layer segments; nested names do not escape their parent layer. *)
  let nested_layers =
    "@layer base { p { max-width: 70ch } } @layer framework { @layer base { p \
     { margin-block: 0.75em } } @layer theme { p { color: #222 } } } @layer \
     framework.theme { blockquote { color: rebeccapurple } }"
  in
  check_stylesheet
    ~expected:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:rebeccapurple}}"
    nested_layers;
  assert_minify_and_optimize nested_layers
    ~minified:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:rebeccapurple}}"
    ~optimized:
      "@layer base{p{max-width:70ch}}@layer framework{@layer \
       base{p{margin-block:.75em}}@layer theme{p{color:#222}}}@layer \
       framework.theme{blockquote{color:#639}}";
  check_stylesheet
    ~expected:
      "@layer reset.type{strong{font-weight:bold}}@layer \
       framework{.title{font-weight:100}@layer \
       theme{h1,h2{color:maroon}}}@layer reset{[hidden]{display:none}}"
    "@layer reset.type { strong { font-weight: bold } } @layer framework { \
     .title { font-weight: 100 } @layer theme { h1, h2 { color: maroon } } } \
     @layer reset { [hidden] { display: none } }"

(* Not a roundtrip test *)
let c64_layer_statement_edges () =
  (* CSS Cascade section 6.4.4.2: statement @layer accepts one or more layer
     names, can appear before imports, and declares names in source order. *)
  check_stylesheet
    ~expected:
      "@layer default,theme,components;@import\"theme.css\"layer(theme);@layer \
       default{audio[controls]{display:block}}"
    "@layer default, theme, components; @import url(theme.css) layer(theme); \
     @layer default { audio[controls] { display: block } }";
  check_stylesheet
    ~expected:
      "@layer default;@import\"theme.css\"layer(theme);@layer \
       components;@layer default{audio[controls]{display:block}}"
    "@layer default; @import url(theme.css) layer(theme); @layer components; \
     @layer default { audio[controls] { display: block } }";
  check_stylesheet ~expected:"@layer framework.base,framework.theme;"
    "@layer framework.base, framework.theme;";
  neg_cursor read "@layer;";
  neg_cursor read "@layer , theme;";
  neg_cursor read "@layer default, { audio { display: block } }";
  neg_cursor read "@layer default, theme { audio { display: block } }"

(* Not a roundtrip test *)
let c64_anonymous_layer_edges () =
  (* CSS Cascade section 6.4.2.1: anonymous layers are valid block @layer rules
     but cannot be referenced by name from outside the block. *)
  check_stylesheet
    ~expected:
      "@layer{.private{color:red}}@layer{.private{display:block}}@layer public;"
    "@layer { .private { color: red } } @layer { .private { display: block } } \
     @layer public;";
  check_stylesheet
    ~expected:
      "@layer{@layer foo{.inside{color:red}}@layer foo{.inside{display:block}}}"
    "@layer { @layer foo { .inside { color: red } } @layer foo { .inside { \
     display: block } } }";
  check_stylesheet
    ~expected:
      "@layer{@layer foo{.inside{color:red}}}@layer{@layer \
       foo{.inside{display:block}}}"
    "@layer { @layer foo { .inside { color: red } } } @layer { @layer foo { \
     .inside { display: block } } }"

(* Not a roundtrip test *)
let c64_import_layer_syntax () =
  (* CSS Cascade section 6.4.1: @import can assign an imported sheet to a named
     layer with layer(<layer-name>) or to an anonymous layer with
     layer/layer(). *)
  check_import_rule ~expected:"@import\"headings.css\"layer(default);"
    "@import url(headings.css) layer(default);";
  check_import_rule ~expected:"@import\"links.css\"layer(default)screen;"
    "@import url(links.css) layer(default) screen;";
  check_import_rule ~expected:"@import\"theme.css\"layer(framework.theme);"
    "@import url(theme.css) layer(framework.theme);";
  check_import_rule ~expected:"@import\"base-forms.css\"layer;"
    "@import url(base-forms.css) layer;";
  check_import_rule ~expected:"@import\"base-links.css\"layer;"
    "@import url(base-links.css) layer();";
  check_import_rule ~expected:"@import\"conditional.css\"layer print;"
    "@import url(conditional.css) layer() print;";
  neg_cursor read_import_rule "@import url(theme.css) layer(initial);";
  neg_cursor read_import_rule "@import url(theme.css) layer(framework . theme);";
  neg_cursor read_import_rule "@import url(theme.css) layer(framework,theme);"

(* Not a roundtrip test *)
let c2_import_conditions () =
  (* CSS Cascade sections 2 and 2.1: @import accepts url/string sources,
     optional layer or layer(<layer-name>), optional supports(), and optional
     media query lists. A declaration inside supports() is equivalent to the
     same declaration wrapped as a supports condition. *)
  check_import_rule ~expected:"@import\"mystyle.css\";"
    "@import url(mystyle.css);";
  check_import_rule ~expected:"@import\"mystyle.css\";"
    "@import \"mystyle.css\";";
  check_import_rule
    ~expected:"@import\"narrow.css\"supports(display:flex)handheld;"
    "@import url(narrow.css) supports(display: flex) handheld;";
  check_import_rule
    ~expected:"@import\"narrow.css\"supports(display:flex)handheld;"
    "@import url(narrow.css) supports((display: flex)) handheld;";
  check_import_rule
    ~expected:
      "@import\"layout.css\"layer(framework.component)supports(display:grid)screen \
       and (min-width:30em);"
    "@import url(layout.css) layer(framework.component) supports(display: \
     grid) screen and (min-width: 30em);";
  check_import_rule ~expected:"@import\"bluish.css\"projection,tv;"
    "@import url(bluish.css) projection, tv;";
  neg_cursor read_import_rule "@import url(theme.css) supports();";
  neg_cursor read_import_rule "@import url(theme.css) supports(display);";
  neg_cursor read_import_rule "@import layer(default) url(theme.css);"

(* Not a roundtrip test *)
let c64_import_namespace_order () =
  (* CSS Cascade sections 2 and 6.4.4.2: empty @layer statements may appear
     before @import, but @layer rules must not be interleaved with consecutive
     @import/@namespace rules. Block @layer rules cannot be interleaved with
     @import rules. *)
  check_stylesheet
    ~expected:
      "@layer reset,theme;@import\"theme.css\"layer(theme);@namespace \
       \"http://www.w3.org/1999/xhtml\";"
    "@charset \"UTF-8\"; @layer reset, theme; @import url(theme.css) \
     layer(theme); @namespace url(http://www.w3.org/1999/xhtml);";
  neg_cursor read
    "@import url(default.css) layer(default); @layer theme; @import \
     url(components.css) layer(components);";
  neg_cursor read
    "@import url(default.css) layer(default); @layer theme { .x { color: red } \
     } @import url(components.css) layer(components);";
  neg_cursor read
    "@import url(default.css) layer(default); @layer theme; @namespace \
     url(http://www.w3.org/1999/xhtml);"

(* Not a roundtrip test *)
let c64_invalid_layer_names () =
  (* CSS Cascade section 6.4.2 reserves CSS-wide keywords in every layer-name
     segment, and the <layer-name> grammar has no empty segments. *)
  List.iter
    (fun keyword ->
      neg_cursor read ("@layer " ^ keyword ^ " { .x { color: red } }");
      neg_cursor read ("@layer framework." ^ keyword ^ " { .x { color: red } }"))
    [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ];
  neg_cursor read "@layer framework..theme { .x { color: red } }";
  neg_cursor read "@layer .framework { .x { color: red } }";
  neg_cursor read "@layer framework. { .x { color: red } }";
  neg_cursor read "@layer framework.theme. { .x { color: red } }";
  neg_cursor read "@layer InHeRiT { .x { color: red } }"

(* Not a roundtrip test *)
let c8_layer_api () =
  (* CSS Cascade section 8: CSSOM exposes the declared layer name on imports and
     layer block rules, and the declared name list on layer statement rules. A
     name is its idents, so [framework.theme] is two of them; nested block rule
     names are the at-rule's own name, not parent-prefixed. *)
  let name = Alcotest.(option (list string)) in
  let import_named =
    {
      url = "theme.css";
      layer = Some [ "framework"; "theme" ];
      supports = None;
      media = None;
    }
  in
  let import_anonymous =
    { url = "private.css"; layer = Some []; supports = None; media = None }
  in
  let import_plain =
    { url = "plain.css"; layer = None; supports = None; media = None }
  in
  Alcotest.check name "named import layerName"
    (Some [ "framework"; "theme" ])
    (Css.Stylesheet.import_layer_name import_named);
  Alcotest.check name "anonymous import layerName is the empty name" (Some [])
    (Css.Stylesheet.import_layer_name import_anonymous);
  Alcotest.check name "unlayered import layerName is null" None
    (Css.Stylesheet.import_layer_name import_plain);
  Alcotest.check name "named layer block API name"
    (Some [ "framework"; "theme" ])
    (Css.Stylesheet.layer_block_name
       (Css.Stylesheet.Layer (Some [ "framework"; "theme" ], [])));
  Alcotest.check name "anonymous layer block API name is the empty name"
    (Some [])
    (Css.Stylesheet.layer_block_name (Css.Stylesheet.Layer (None, [])));
  (match
     Css.Stylesheet.Layer
       (Some [ "outer" ], [ Css.Stylesheet.Layer (Some [ "foo"; "bar" ], []) ])
   with
  | Css.Stylesheet.Layer (_, [ inner ]) ->
      Alcotest.check name "inner layer block API name is not parent-prefixed"
        (Some [ "foo"; "bar" ])
        (Css.Stylesheet.layer_block_name inner)
  | _ -> Alcotest.fail "expected nested layer block");
  Alcotest.(check (option (list (list string))))
    "layer statement API nameList"
    (Some [ [ "reset" ]; [ "framework"; "theme" ]; [ "components" ] ])
    (Css.Stylesheet.layer_statement_name_list
       (Css.Stylesheet.Layer_decl
          [ [ "reset" ]; [ "framework"; "theme" ]; [ "components" ] ]));
  Alcotest.(check (option (list (list string))))
    "non-statement layer has no nameList" None
    (Css.Stylesheet.layer_statement_name_list
       (Css.Stylesheet.Layer (Some [ "reset" ], [])))

(* Not a roundtrip test *)
let c41_declared_values () =
  (* CSS Cascade section 4.1: each property declaration applied to an element
     contributes a declared value for that element/property. The library can
     expose those declaration-level values without resolving selector
     matching. *)
  let declarations =
    [
      Css.Declaration.color (Css.Values.hex "#ff0000");
      Css.Declaration.margin [ Css.Values.Px 1. ];
      Css.Declaration.important
        (Css.Declaration.color (Css.Values.hex "#0000ff"));
      Css.Declaration.custom_property "--accent" "currentColor";
    ]
  in
  let declared = Css.Stylesheet.declared_values declarations in
  let color_declared =
    Css.Stylesheet.declared_values ~property:"color" declarations
  in
  let declared_property (d : Css.Stylesheet.declared_value) = d.property in
  let declared_value (d : Css.Stylesheet.declared_value) = d.value in
  let declared_important (d : Css.Stylesheet.declared_value) = d.important in
  let declared_source_order (d : Css.Stylesheet.declared_value) =
    d.source_order
  in
  Alcotest.(check (list string))
    "declared values preserve every declaration property"
    [ "color"; "margin"; "color"; "--accent" ]
    (List.map declared_property declared);
  Alcotest.(check (list string))
    "declared values expose serialized values"
    [ "#f00"; "1px"; "#00f"; "currentcolor" ]
    (List.map declared_value declared);
  Alcotest.(check (list int))
    "declared values preserve source order" [ 0; 1; 2; 3 ]
    (List.map declared_source_order declared);
  Alcotest.(check (list string))
    "declared value filtering selects one property" [ "#f00"; "#00f" ]
    (List.map declared_value color_declared);
  Alcotest.(check (list bool))
    "declared values preserve importance for cascade sorting" [ false; true ]
    (List.map declared_important color_declared)

(* Not a roundtrip test *)
let c42_cascaded_values () =
  (* CSS Cascade section 4.2: after cascade sorting there is at most one
     cascaded value per property. No matching declaration means no cascaded
     value. *)
  let candidate origin important source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important; source_order; value }
  in
  Alcotest.(check (option string))
    "empty candidate set has no cascaded value" None
    (Css.Stylesheet.cascaded_value []);
  Alcotest.(check (option string))
    "highest origin/importance candidate becomes the cascaded value"
    (Some "author-important")
    (Css.Stylesheet.cascaded_value
       [
         candidate User_agent false 0 "ua-normal";
         candidate User false 1 "user-normal";
         candidate Author false 2 "author-normal";
         candidate Author true 3 "author-important";
       ]);
  Alcotest.(check (option string))
    "source order breaks ties within one origin/importance bucket"
    (Some "later")
    (Css.Stylesheet.cascaded_value
       [
         candidate Author false 10 "earlier"; candidate Author false 11 "later";
       ])

let spec_cascade_origin_importance_order () =
  (* CSS Cascade origin/importance order, including generated animation and
     transition origins. Larger rank wins in the helper API. *)
  let rank origin important =
    Css.Stylesheet.origin_importance_rank ~important origin
  in
  let open Css.Stylesheet in
  Alcotest.(check (list int))
    "normal and important origin ranks"
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    [
      rank User_agent false;
      rank User false;
      rank Author_presentational_hint false;
      rank Author false;
      rank Animation false;
      rank Author true;
      rank User true;
      rank User_agent true;
      rank Transition false;
    ];
  let candidate origin important source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important; source_order; value }
  in
  Alcotest.(check (option string))
    "transition beats important user-agent" (Some "transition")
    (Css.Stylesheet.cascaded_value
       [
         candidate User_agent true 0 "ua-important";
         candidate Transition false 1 "transition";
       ]);
  Alcotest.(check (option string))
    "important user beats important author" (Some "user-important")
    (Css.Stylesheet.cascaded_value
       [
         candidate Author true 0 "author-important";
         candidate User true 1 "user-important";
       ])

(* Not a roundtrip test *)
let c43_specified_values () =
  (* CSS Cascade section 4.3: defaulting guarantees a specified value exists for
     every property. CSS-wide keywords are handled before computed values. *)
  check_specified "normal cascaded value" "block" "cascaded"
    (Css.Stylesheet.value ~inherits:false ~initial:"inline" ~inherited:None
       ~cascaded:(Some "block"));
  check_specified "missing non-inherited property" "auto" "initial-default"
    (Css.Stylesheet.value ~inherits:false ~initial:"auto" ~inherited:None
       ~cascaded:None);
  check_specified "missing inherited property" "blue" "inherited-default"
    (Css.Stylesheet.value ~inherits:true ~initial:"black"
       ~inherited:(Some "blue") ~cascaded:None);
  check_specified "initial keyword" "medium" "initial-keyword"
    (Css.Stylesheet.value ~inherits:true ~initial:"medium"
       ~inherited:(Some "large") ~cascaded:(Some "initial"));
  check_specified "inherit keyword" "4.2px" "inherit-keyword"
    (Css.Stylesheet.value ~inherits:false ~initial:"medium"
       ~inherited:(Some "4.2px") ~cascaded:(Some "inherit"));
  check_specified "inherit keyword on root" "medium" "inherit-keyword"
    (Css.Stylesheet.value ~inherits:false ~initial:"medium" ~inherited:None
       ~cascaded:(Some "inherit"));
  check_specified "unset on inherited property" "inside" "unset-inherited"
    (Css.Stylesheet.value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "unset"));
  check_specified "unset on non-inherited property" "auto" "unset-initial"
    (Css.Stylesheet.value ~inherits:false ~initial:"auto"
       ~inherited:(Some "80px") ~cascaded:(Some "unset"))

(* Not a roundtrip test *)
let c448_value_stage_scope () =
  (* Later value stages need context supplied by a caller or platform; the
     parser itself only models the local ordering of stages. *)
  Alcotest.(check (list bool))
    "declared/cascaded/specified are library-local; later stages need context"
    [ false; false; false; true; true; true ]
    (List.map Css.Stylesheet.value_processing_requires_document_context
       [
         Css.Stylesheet.Declared_value;
         Cascaded_value;
         Specified_value;
         Computed_value;
         Used_value;
         Actual_value;
       ])

(* Not a roundtrip test *)
let c42_integrated_order () =
  (* CSS Cascade section 4.2 consumes the sorted cascade output. This helper
     covers the full ordering criteria available without DOM matching:
     origin/importance, layer, specificity, scope proximity, and source
     order. *)
  let candidate origin important layer specificity scope_hops source_order value
      : Css.Stylesheet.cascade_candidate =
    { origin; important; layer; specificity; scope_hops; source_order; value }
  in
  let winner_value candidates =
    Css.Stylesheet.winning_cascade_candidate
      ~layer_order:[ "reset"; "theme"; "utilities" ]
      candidates
    |> Option.map (fun (c : Css.Stylesheet.cascade_candidate) -> c.value)
  in
  Alcotest.(check (option string))
    "important user origin beats important author origin"
    (Some "user-important")
    (winner_value
       [
         candidate Author true None 1 None 0 "author-important";
         candidate User true None 1 None 1 "user-important";
       ]);
  Alcotest.(check (option string))
    "normal unlayered beats explicit layers" (Some "unlayered")
    (winner_value
       [
         candidate Author false (Some "utilities") 1 None 2 "utilities";
         candidate Author false None 1 None 1 "unlayered";
       ]);
  Alcotest.(check (option string))
    "specificity beats later source order" (Some "id-selector")
    (winner_value
       [
         candidate Author false None 10 None 2 "later-class";
         candidate Author false None 100 None 1 "id-selector";
       ]);
  Alcotest.(check (option string))
    "closer scope beats farther scope" (Some "near-scope")
    (winner_value
       [
         candidate Author false None 10 (Some 4) 1 "far-scope";
         candidate Author false None 10 (Some 1) 0 "near-scope";
       ]);
  Alcotest.(check (option string))
    "source order breaks final ties" (Some "later")
    (winner_value
       [
         candidate Author false None 10 None 1 "earlier";
         candidate Author false None 10 None 2 "later";
       ])

(* Not a roundtrip test *)
let c43_revert_values () =
  (* CSS Cascade sections 4.3 and 7.3.4-7.3.5: [revert] and [revert-layer]
     resolve by rolling back the candidate set, then defaulting if no lower
     candidate remains. *)
  let origin_candidate origin source_order value :
      Css.Stylesheet.cascade_origin_candidate =
    { origin; important = false; source_order; value }
  in
  check_specified "revert rolls back to user origin" "blue" "cascaded"
    (Css.Stylesheet.specified_value_after_revert ~inherits:true ~initial:"black"
       ~inherited:(Some "purple")
       [
         origin_candidate User_agent 0 "black";
         origin_candidate User 1 "blue";
         origin_candidate Author 2 "revert";
       ]);
  check_specified "revert with no previous origin defaults" "black"
    "initial-default"
    (Css.Stylesheet.specified_value_after_revert ~inherits:false
       ~initial:"black" ~inherited:None
       [ origin_candidate User_agent 0 "revert" ]);
  let layer_candidate layer source_order value :
      Css.Stylesheet.cascade_layer_candidate =
    { layer; important = false; source_order; value }
  in
  check_specified "revert-layer rolls back to lower layer" "green" "cascaded"
    (Css.Stylesheet.specified_value_after_revert_layer ~inherits:false
       ~initial:"transparent" ~inherited:None ~layer_order:[ "base"; "theme" ]
       [
         layer_candidate (Some "base") 0 "green";
         layer_candidate (Some "theme") 1 "revert-layer";
       ]);
  check_specified "revert-layer with no lower layer defaults" "transparent"
    "initial-default"
    (Css.Stylesheet.specified_value_after_revert_layer ~inherits:false
       ~initial:"transparent" ~inherited:None ~layer_order:[ "base"; "theme" ]
       [ layer_candidate (Some "base") 0 "revert-layer" ])

(* Not a roundtrip test *)
let c47_examples () =
  (* CSS Cascade section 4.7 examples this library can model from CSS text. *)
  check_specified "border width inherit example" "4.2px" "inherit-keyword"
    (Css.Stylesheet.value ~inherits:false ~initial:"medium"
       ~inherited:(Some "4.2px") ~cascaded:(Some "inherit"));
  check_specified "width missing declaration example" "auto" "initial-default"
    (Css.Stylesheet.value ~inherits:false ~initial:"auto" ~inherited:None
       ~cascaded:None);
  check_specified "list-style-position inherit example" "inside"
    "inherit-keyword"
    (Css.Stylesheet.value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "inherit"));
  check_specified "list-style-position initial example" "outside"
    "initial-keyword"
    (Css.Stylesheet.value ~inherits:true ~initial:"outside"
       ~inherited:(Some "inside") ~cascaded:(Some "initial"))

let suite =
  ( "stylesheet_cascade",
    [
      ("layer roundtrip", `Quick, test_layer_roundtrip);
      ("spec cascade 6.4 layer name syntax", `Quick, c64_layer_name_syntax);
      ( "spec cascade 6.4 layer nesting examples",
        `Quick,
        c64_layer_nesting_examples );
      ( "spec cascade 6.4 layer statement edges",
        `Quick,
        c64_layer_statement_edges );
      ( "spec cascade 6.4 anonymous layer edges",
        `Quick,
        c64_anonymous_layer_edges );
      ("spec cascade 6.4 import layer syntax", `Quick, c64_import_layer_syntax);
      ("spec cascade 2 import conditions", `Quick, c2_import_conditions);
      ( "spec cascade 6.4 import namespace ordering",
        `Quick,
        c64_import_namespace_order );
      ("spec cascade 6.4 invalid layer names", `Quick, c64_invalid_layer_names);
      ("spec cascade 8 layer api", `Quick, c8_layer_api);
      ("spec cascade 4.1 declared values", `Quick, c41_declared_values);
      ("spec cascade 4.2 cascaded values", `Quick, c42_cascaded_values);
      ( "spec cascade origin/importance order",
        `Quick,
        spec_cascade_origin_importance_order );
      ("spec cascade 4.2 integrated cascade order", `Quick, c42_integrated_order);
      ("spec cascade 4.3 specified values", `Quick, c43_specified_values);
      ("spec cascade 4.3 revert specified values", `Quick, c43_revert_values);
      ("spec cascade 4.7 examples", `Quick, c47_examples);
      ( "spec cascade 4.4-4.8 value processing scope",
        `Quick,
        c448_value_stage_scope );
    ] )
