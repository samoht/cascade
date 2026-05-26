(** Fuzz tests for stylesheet optimization invariants. *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let selector buf i =
  Css.Selector.class_ (pick [ "card"; "title"; "button"; "panel" ] buf i)

let declaration buf i =
  match byte_at buf i mod 5 with
  | 0 -> Css.Declaration.display Css.Properties.Block
  | 1 -> Css.Declaration.display Css.Properties.Flex
  | 2 -> Css.Declaration.color (Css.Values.hex "#ff0000")
  | 3 -> Css.Declaration.background_color (Css.Values.hex "#0000ff")
  | _ -> Css.Declaration.margin [ Css.Values.Px (Float.of_int (byte_at buf i)) ]

let rule buf i = Css.rule ~selector:(selector buf i) [ declaration buf (i + 1) ]

let generated_import =
  Css.Stylesheet.Import
    {
      url = "theme.css";
      layer = Some "theme";
      supports = Some (Css.Supports.property "display" "grid");
      media = Some (Css.Media.of_string "(width >= 40em)");
    }

let generated_namespace =
  Css.Stylesheet.Namespace
    ( Some "svg",
      Css.Stylesheet.Url ("http://www.w3.org/2000/svg", Css.Stylesheet.Url_bare)
    )

let generated_media buf =
  Css.Stylesheet.Media
    ( Css.Media.of_string "(width >= 40em)",
      [
        rule buf 4;
        Css.Stylesheet.Supports
          ( Css.Supports.property "display" "grid",
            [
              Css.container ~name:"card"
                ~condition:(Css.Container.of_string "(inline-size > 30em)")
                [ rule buf 8 ];
            ] );
      ] )

let generated_when buf =
  Css.Stylesheet.When
    ( Css.Stylesheet.And
        ( Css.Stylesheet.Media_condition (Css.Media.of_string "(width >= 48em)"),
          Css.Stylesheet.Supports_condition_test
            (Css.Supports.property "display" "grid") ),
      [ rule buf 10 ] )

let generated_else buf =
  Css.Stylesheet.Else
    ( Some
        (Css.Stylesheet.Supports_condition_test
           (Css.Supports.property "display" "flex")),
      [ rule buf 11 ] )

let generated_keyframes =
  Css.Stylesheet.Keyframes
    ( "fade",
      [
        {
          keyframe_selector = Css.Keyframe.Positions [ Css.Keyframe.Percent 0. ];
          keyframe_declarations =
            [ Css.Declaration.opacity (Css.Properties.Opacity_number 0.) ];
        };
        {
          keyframe_selector =
            Css.Keyframe.Positions [ Css.Keyframe.Percent 100. ];
          keyframe_declarations =
            [ Css.Declaration.opacity (Css.Properties.Opacity_number 1.) ];
        };
      ] )

let generated_sheet_prelude =
  [
    Css.Stylesheet.Charset "UTF-8";
    Css.Stylesheet.Layer_decl [ "reset"; "theme"; "components" ];
    generated_import;
    generated_namespace;
    Css.Stylesheet.property ~syntax:Css.Variables.Universal "--fuzz";
  ]

let generated_stylesheet buf =
  generated_sheet_prelude
  @ [
      rule buf 0;
      rule buf 0;
      generated_media buf;
      generated_when buf;
      generated_else buf;
      Css.Stylesheet.Supports_condition
        ("--fuzz-condition", [ declaration buf 12 ]);
      Css.Stylesheet.Layer
        ( Some (pick [ "base"; "theme"; "components" ] buf 12),
          [ rule buf 16; Css.Stylesheet.Layer (None, [ rule buf 20 ]) ] );
      Css.Stylesheet.Scope (Some ".card", Some ".limit", [ rule buf 24 ]);
      Css.Stylesheet.Starting_style [ rule buf 28 ];
      Css.Stylesheet.with_origin Css.Stylesheet.Author
        [ rule buf 32; rule buf 32 ];
      Css.Stylesheet.Page
        (Some ":first", [ Css.Declaration.margin [ Css.Values.Px 10. ] ]);
      generated_keyframes;
    ]

let minified ss = Css.Stylesheet.to_string ~minify:true ss |> String.trim

let parse_stylesheet input =
  let r = Cursor.of_string input in
  try Some (Css.Stylesheet.read_stylesheet r)
  with Cursor.Parse_error _ -> None

let class_name prefix buf i = prefix ^ string_of_int (byte_at buf i mod 10)

let count_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop count i =
    if needle_len = 0 || i + needle_len > haystack_len then count
    else if String.sub haystack i needle_len = needle then
      loop (count + 1) (i + needle_len)
    else loop count (i + 1)
  in
  loop 0 0

let first_index ~needle haystack =
  try Some (Re.Str.search_forward (Re.Str.regexp_string needle) haystack 0)
  with Not_found -> None

(* A feature query that every target browser satisfies is always true, so its
   @supports wrapper imposes no condition and the optimizer may drop it.
   display:grid and display:flex are supported by every maintained evergreen
   browser (flexbox since ~2015, grid since 2017), making those queries
   unconditionally true on the target. This oracle encodes that browser fact
   directly - independent of the code under test - and admits a dropped wrapper
   only for those two conditions. *)
let baseline_true_supports condition =
  condition = Css.Supports.property "display" "grid"
  || condition = Css.Supports.property "display" "flex"

let rec boundary_shape = function
  | Css.Stylesheet.Rule _ -> [ "rule" ]
  | Declarations _ -> [ "declarations" ]
  | Import { layer; _ } -> [ "import:" ^ Option.value ~default:"<none>" layer ]
  | Namespace _ -> [ "namespace" ]
  | Layer_decl names -> [ "layer-decl:" ^ String.concat "," names ]
  | Layer (name, block) ->
      let name = Option.value ~default:"<anonymous>" name in
      (("layer:" ^ name) :: shapes_with_rule_runs block) @ [ "/layer" ]
  | Media (_, block) -> ("media" :: shapes_with_rule_runs block) @ [ "/media" ]
  | Supports (condition, block) when baseline_true_supports condition ->
      shapes_with_rule_runs block
  | Supports (_, block) ->
      ("supports" :: shapes_with_rule_runs block) @ [ "/supports" ]
  | Container (_, _, block) ->
      ("container" :: shapes_with_rule_runs block) @ [ "/container" ]
  | When (_, block) -> ("when" :: shapes_with_rule_runs block) @ [ "/when" ]
  | Else (_, block) -> ("else" :: shapes_with_rule_runs block) @ [ "/else" ]
  | Supports_condition (name, _) -> [ "supports-condition:" ^ name ]
  | Scope (_, _, block) ->
      ("scope" :: shapes_with_rule_runs block) @ [ "/scope" ]
  | Starting_style block ->
      ("starting-style" :: shapes_with_rule_runs block) @ [ "/starting-style" ]
  | Origin (_, block) ->
      ("origin" :: shapes_with_rule_runs block) @ [ "/origin" ]
  | Moz_document (_, block) ->
      ("moz-document" :: shapes_with_rule_runs block) @ [ "/moz-document" ]
  (* CSS Syntax 3 section 8.3: @charset is an encoding declaration byte
     sequence, not an actual at-rule. Parsed occurrences are invalid and may be
     dropped, so it is not a cascade boundary shape invariant. *)
  | Charset _ -> []
  | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ -> [ "keyframes" ]
  | Font_face _ -> [ "font-face" ]
  | Counter_style _ -> [ "counter-style" ]
  | Page _ -> [ "page" ]
  | Page_with_margins _ -> [ "page" ]
  | Font_palette_values _ -> [ "font-palette-values" ]
  | Font_feature_values _ -> [ "font-feature-values" ]
  | View_transition _ -> [ "view-transition" ]
  | Position_try _ -> [ "position-try" ]
  | Viewport _ -> [ "viewport" ]
  | Unknown_at_rule { name; _ } -> [ "unknown-at-rule:" ^ name ]
  | Property _ -> [ "property" ]
  | Bang_comment _ -> [ "bang-comment" ]

(* The optimizer is allowed to merge a contiguous run of [Rule]s into fewer
   rules (e.g. [combine_identical_rules]). Collapse consecutive [Rule] entries
   into a single [rules] token so the boundary check tracks the at-rule skeleton
   without forcing the optimizer to leave every individual rule intact. *)
and shapes_with_rule_runs ss =
  let rec loop acc seen_rule = function
    | [] -> if seen_rule then List.rev ("rules" :: acc) else List.rev acc
    | Css.Stylesheet.Rule _ :: rest -> loop acc true rest
    | other :: rest ->
        let acc = if seen_rule then "rules" :: acc else acc in
        loop (List.rev_append (boundary_shape other) acc) false rest
  in
  loop [] false ss

let boundary_shapes ss = shapes_with_rule_runs ss

let test_optimize_idempotent buf =
  let ss = generated_stylesheet buf in
  let once = Css.Optimize.stylesheet ss in
  let twice = Css.Optimize.stylesheet once in
  let once_s = minified once in
  let twice_s = minified twice in
  if once_s <> twice_s then
    failf "optimization serialization changed on second pass: %S -> %S" once_s
      twice_s

let test_optimized_stylesheet_reparses buf =
  let optimized = Css.Optimize.stylesheet (generated_stylesheet buf) in
  let serialized = minified optimized in
  match parse_stylesheet serialized with
  | Some _ -> ()
  | None -> failf "optimized stylesheet did not reparse: %S" serialized

(* Optimizer monotonicity: minifying after optimize must never be longer than
   minifying without optimize. Any regression here means optimize emitted bytes
   the plain minifier wouldn't - the classical "optimizer made it bigger"
   bug. *)
let test_optimize_minify_monotonicity buf =
  let ss = generated_stylesheet buf in
  let baseline = minified ss in
  let optimized = minified (Css.Optimize.stylesheet ss) in
  if String.length optimized > String.length baseline then
    failf
      "optimize is not monotonic: optimized output is longer than minify alone \
       (%d > %d): %S vs %S"
      (String.length optimized) (String.length baseline) optimized baseline

let test_optimization_preserves_boundary_shape buf =
  let ss = generated_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let before = boundary_shapes ss in
  let after = boundary_shapes optimized in
  if before <> after then
    failf "optimization changed conditional/cascade boundary shape: %S -> %S"
      (String.concat " " before) (String.concat " " after)

let test_optimized_reparse_idempotent buf =
  let optimized = Css.Optimize.stylesheet (generated_stylesheet buf) in
  let serialized = minified optimized in
  match parse_stylesheet serialized with
  | None -> fail "optimized stylesheet did not reparse"
  | Some parsed ->
      let reparsed_optimized = Css.Optimize.stylesheet parsed in
      let serialized2 = minified reparsed_optimized in
      if serialized <> serialized2 then
        failf "optimized reparse serialization changed: %S -> %S" serialized
          serialized2

(* Declarations that the optimizer either groups across rules or folds in place,
   including ones whose fold or canonical order has historically been reached
   only on a second pass (background-position 0%, transition component order,
   transform-origin, var()-spaced border). *)
let fold_prone_decls =
  [|
    "color:red";
    "margin:0";
    "padding:0";
    "display:block";
    "top:0";
    "left:0";
    "background:url(a) 0%/44px";
    "transition:opacity linear .25s";
    "transform-origin:100%";
    "border:var(--w) var(--s) var(--c)";
  |]

(* A cluster of sibling rules drawing overlapping declaration subsets from
   [fold_prone_decls]. Overlap creates factoring opportunities, and grouping one
   subset can expose another, so a single optimize pass must still reach a fixed
   point - the emergent non-idempotence that the uniform generator does not hit
   and that only surfaced at scale in the SatCSS benchmark. *)
let generated_cluster_css buf =
  let len = max 1 (String.length buf) in
  let at i = Char.code buf.[i mod len] in
  let sel i = "c" ^ string_of_int (at i mod 8) in
  let decl i = fold_prone_decls.(at i mod Array.length fold_prone_decls) in
  let rule ri =
    let b = ri * 3 in
    String.concat ""
      [
        "."; sel b; "{"; decl (b + 1); ";"; decl (b + 2); ";"; decl (b + 3); "}";
      ]
  in
  String.concat "" (List.init 6 rule)

let test_minify_cluster_fixpoint buf =
  match parse_stylesheet (generated_cluster_css buf) with
  | None -> ()
  | Some ss -> (
      let once = minified (Css.Optimize.stylesheet ss) in
      match parse_stylesheet once with
      | None -> failf "cluster minified output did not reparse: %S" once
      | Some parsed ->
          let twice = minified (Css.Optimize.stylesheet parsed) in
          if once <> twice then
            failf
              "minify is not a fixed point on an interacting cluster: %S -> %S"
              once twice)

let count_imports ss =
  List.fold_left
    (fun acc -> function Css.Stylesheet.Import _ -> acc + 1 | _ -> acc)
    0 ss

let count_namespaces ss =
  List.fold_left
    (fun acc -> function Css.Stylesheet.Namespace _ -> acc + 1 | _ -> acc)
    0 ss

let test_import_namespace_counts buf =
  let ss = generated_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  if count_imports ss <> count_imports optimized then
    fail "optimization changed top-level import count";
  if count_namespaces ss <> count_namespaces optimized then
    fail "optimization changed top-level namespace count"

let count_kind f ss =
  List.fold_left (fun acc stmt -> if f stmt then acc + 1 else acc) 0 ss

let atrule_count_checks =
  [
    ("property", function Css.Stylesheet.Property _ -> true | _ -> false);
    (* CSS Syntax 3 section 8.3: @charset is not an at-rule whose count is
       stable across grammar-checking/minification. *)
    ( "starting-style",
      function Css.Stylesheet.Starting_style _ -> true | _ -> false );
    ("origin", function Css.Stylesheet.Origin _ -> true | _ -> false);
    ("page", function Css.Stylesheet.Page _ -> true | _ -> false);
    ("keyframes", function Css.Stylesheet.Keyframes _ -> true | _ -> false);
    ("when", function Css.Stylesheet.When _ -> true | _ -> false);
    ("else", function Css.Stylesheet.Else _ -> true | _ -> false);
    ( "supports-condition",
      function Css.Stylesheet.Supports_condition _ -> true | _ -> false );
  ]

let test_atrule_counts_stable buf =
  let ss = generated_stylesheet buf in
  let optimized = Css.Optimize.stylesheet ss in
  let check (label, pred) =
    let before = count_kind pred ss in
    let after = count_kind pred optimized in
    if before <> after then
      failf "optimization changed %s count: %d -> %d" label before after
  in
  List.iter check atrule_count_checks

let media_rule selector color =
  Css.Stylesheet.Media
    ( Css.Media.Cond
        (Css.Media.Feature
           (Css.Media.Plain
              ( Css.Media.Min Css.Media.Width,
                Css.Media.Length (Css.Values.Px 48.) ))),
      [
        Css.rule
          ~selector:(Css.Selector.class_ selector)
          [ Css.Declaration.color (Css.Values.hex color) ];
      ] )

let cascade_merge_vectors =
  [
    [
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [ Css.Declaration.color (Css.Values.hex "#ff0000") ];
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [
          Css.Declaration.display Css.Properties.Flex;
          Css.Declaration.color (Css.Values.hex "#0000ff");
        ];
    ];
    [
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [ Css.Declaration.color (Css.Values.hex "#ff0000") ];
      Css.Stylesheet.Layer_decl [ "reset"; "components" ];
      Css.rule
        ~selector:(Css.Selector.class_ "box")
        [ Css.Declaration.display Css.Properties.Flex ];
    ];
    [
      media_rule "a" "#ff0000";
      Css.Stylesheet.Layer_decl [ "theme" ];
      media_rule "b" "#0000ff";
    ];
  ]

let test_cascade_merge_vectors buf =
  let input = pick cascade_merge_vectors buf 0 in
  let optimized = Css.Optimize.stylesheet input in
  let before = boundary_shapes input in
  let after = boundary_shapes optimized in
  if before <> after then
    failf "merge vector changed cascade boundary shape: %S -> %S"
      (String.concat " " before) (String.concat " " after);
  ignore (minified optimized)

let test_cascade_shorthand_importance_vectors buf =
  let declarations =
    pick
      [
        [
          Css.Declaration.important
            (Css.Declaration.margin_left (Css.Values.Px 1.));
          Css.Declaration.margin [ Css.Values.Px 2. ];
        ];
        [
          Css.Declaration.margin [ Css.Values.Px 2. ];
          Css.Declaration.important
            (Css.Declaration.margin_left (Css.Values.Px 1.));
        ];
      ]
      buf 0
  in
  let rule = Css.rule ~selector:(Css.Selector.class_ "box") declarations in
  let optimized = Css.Optimize.stylesheet [ rule ] |> minified in
  if
    not
      (String.contains optimized '!'
      && Astring.String.is_infix ~affix:"margin:" optimized
      && Astring.String.is_infix ~affix:"margin-left:" optimized)
  then
    failf
      "optimization dropped a required mixed-importance shorthand/longhand: %S"
      optimized

let test_smt_intersection_dependency_vectors buf =
  (* Hague, Lin, Hong, "CSS Minification via Constraint Solving", sections 4 and
     5.4: the CSS graph carries an edge order induced by selector intersection
     and same-property writes. These generated selectors can all match one
     element, so the middle declaration is an ordering dependency and the equal
     declarations around it must not be grouped across it. *)
  let a = class_name "a" buf 0 in
  let b = class_name "b" buf 1 in
  let c = class_name "c" buf 2 in
  let x = class_name "x" buf 3 in
  let input =
    Fmt.str ".%s.%s{color:red}.%s.%s{color:blue}.%s.%s{color:red}" a x b x c x
  in
  match parse_stylesheet input with
  | None -> failf "SMT dependency vector did not parse: %S" input
  | Some ss ->
      let optimized = Css.Optimize.stylesheet ss |> minified in
      let required =
        [
          Fmt.str ".%s.%s{color:red}" a x;
          Fmt.str ".%s.%s{color:#00f}" b x;
          Fmt.str ".%s.%s{color:red}" c x;
        ]
      in
      if
        not
          (List.for_all
             (fun chunk -> Astring.String.is_infix ~affix:chunk optimized)
             required)
      then
        failf "optimization violated selector-intersection edge order: %S -> %S"
          input optimized

let test_biclique_no_new_edges buf =
  (* Section 7's Max-SAT encoding constrains candidate bicliques so a merged
     rule cannot introduce selector/property edges that were absent from the
     original CSS graph. This missing-corner rectangle may factor out the shared
     color edge, but must not give the second selector the first selector's
     background-color edge. *)
  let a = class_name "a" buf 0 in
  let b = class_name "b" buf 1 in
  let input =
    Fmt.str ".%s{color:red;background-color:blue}.%s{color:red}" a b
  in
  match parse_stylesheet input with
  | None -> failf "SMT biclique vector did not parse: %S" input
  | Some ss ->
      let optimized = Css.Optimize.stylesheet ss |> minified in
      let unsafe_ab = Fmt.str ".%s,.%s{color:red;background-color:#00f}" a b in
      let unsafe_ba = Fmt.str ".%s,.%s{color:red;background-color:#00f}" b a in
      if
        Astring.String.is_infix ~affix:unsafe_ab optimized
        || Astring.String.is_infix ~affix:unsafe_ba optimized
      then
        failf
          "optimization introduced a missing selector/property edge: %S -> %S"
          input optimized

let test_smt_property_order_vectors buf =
  (* Section 4 models rule properties as an ordered sequence: selectors commute,
     but declarations inside one rule do not. When the later declaration uses
     runtime-dependent CSS Color 4 syntax, older browsers drop it and the
     earlier same-property declaration is a real cascade fallback - it must
     survive optimization and stay first. Static in-gamut Color 4 colors may
     fold to sRGB under minify, so this vector uses a var() dependency. *)
  let selector = "." ^ class_name "fallback" buf 0 in
  let input =
    Fmt.str "%s{color:rgb(59,130,246);color:rgb(from var(--brand) r g b/.5)}"
      selector
  in
  match parse_stylesheet input with
  | None -> failf "SMT property-order vector did not parse: %S" input
  | Some ss ->
      let optimized = Css.Optimize.stylesheet ss |> minified in
      if count_substring ~needle:"color:" optimized <> 2 then
        failf
          "optimization dropped legacy color fallback before runtime Color 4 \
           spelling: %S -> %S"
          input optimized;
      begin match
        ( first_index ~needle:"color:rgb(from" optimized,
          first_index ~needle:"color:" optimized )
      with
      | Some modern_pos, Some first_color_pos when modern_pos > first_color_pos
        ->
          ()
      | _ ->
          failf
            "optimization reordered runtime Color 4 declaration before its \
             fallback: %S -> %S"
            input optimized
      end

let test_positive_layer_statement_vectors buf =
  let input =
    [
      Css.Stylesheet.Layer (Some (pick [ "reset"; "base" ] buf 0), []);
      Css.Stylesheet.Layer (Some (pick [ "theme"; "components" ] buf 1), []);
      Css.rule
        ~selector:(Css.Selector.class_ "card")
        [ Css.Declaration.display Css.Properties.Flex ];
    ]
  in
  let optimized = Css.Optimize.stylesheet input |> minified in
  if not (Astring.String.is_prefix ~affix:"@layer " optimized) then
    failf "empty named layers did not canonicalize to layer statement: %S"
      optimized;
  match parse_stylesheet optimized with
  | Some _ -> ()
  | None ->
      failf "positive layer statement optimization did not reparse: %S"
        optimized

let test_layer_before_specificity buf =
  (* CSS Cascade 5 section 6.4: layer order is a cascade sorting criterion
     before selector specificity. Generate an earlier layer containing a
     higher-specificity selector and a later layer containing a
     lower-specificity selector; optimization must not merge, reorder, or drop
     either rule by reasoning from selector specificity alone. *)
  let earlier = pick [ "reset"; "base"; "theme" ] buf 0 in
  let later = pick [ "components"; "utilities"; "overrides" ] buf 1 in
  let input =
    Fmt.str "@layer %s,%s;@layer %s{.x.y{color:blue}}@layer %s{.x{color:red}}"
      earlier later earlier later
  in
  match parse_stylesheet input with
  | None -> failf "layer specificity vector did not parse: %S" input
  | Some ss -> (
      let optimized = Css.Optimize.stylesheet ss |> minified in
      let required_prelude = Fmt.str "@layer %s,%s;" earlier later in
      let required_earlier = Fmt.str "@layer %s{.x.y{color:#00f}}" earlier in
      let required_later = Fmt.str "@layer %s{.x{color:red}}" later in
      if
        not
          (Astring.String.is_infix ~affix:required_prelude optimized
          && Astring.String.is_infix ~affix:required_earlier optimized
          && Astring.String.is_infix ~affix:required_later optimized)
      then
        failf "optimization ignored layer order before specificity: %S -> %S"
          input optimized;
      match parse_stylesheet optimized with
      | Some _ -> ()
      | None ->
          failf "optimized layer specificity vector did not reparse: %S"
            optimized)

let test_name_defining_atrules_preserved buf =
  let css =
    pick
      [
        "@font-face{font-family:Brand;src:url(brand.woff2)}.x{color:red}";
        "@keyframes fade{from{opacity:0}to{opacity:1}}.x{color:red}";
        "@property \
         --gap{syntax:\"<length>\";inherits:false;initial-value:1rem}.x{padding:var(--gap)}";
        "@view-transition{navigation:auto}.x{view-transition-name:card}";
      ]
      buf 0
  in
  match parse_stylesheet css with
  | None -> failf "name-defining at-rule vector did not parse: %S" css
  | Some ss -> (
      let optimized = Css.Optimize.stylesheet ss |> minified in
      let required =
        pick
          [ "@font-face"; "@keyframes"; "@property --gap"; "@view-transition" ]
          buf 0
      in
      if not (Astring.String.is_infix ~affix:required optimized) then
        failf "optimization dropped name-defining at-rule %S from %S -> %S"
          required css optimized;
      match parse_stylesheet optimized with
      | Some _ -> ()
      | None ->
          failf "optimized name-defining at-rule did not reparse: %S" optimized)

(* When optimize has nothing left to change it must return its input unchanged -
   the very same physical value, not a structurally-equal reallocation.
   Optimizing once reaches a fixed point [canon]; a second pass is then a
   genuine no-op, so [optimize canon] must be physically equal to [canon]. A
   reallocation is wasted work and a sign the optimizer rebuilds subtrees it
   never touched. *)
let test_optimize_preserves_physical_identity buf =
  let canon = Css.Optimize.stylesheet (generated_stylesheet buf) in
  let again = Css.Optimize.stylesheet canon in
  if again == canon then ()
  else if again = canon then
    fail
      "optimize reallocated an already-optimized stylesheet instead of \
       returning it unchanged (x = optimize x but not x == optimize x)"
  else
    failf
      "optimize is not idempotent, so the sharing invariant cannot apply: %S \
       -> %S"
      (minified canon) (minified again)

let suite =
  ( "optimize",
    [
      test_case "optimize idempotent" [ bytes ] test_optimize_idempotent;
      test_case "optimize preserves physical identity on a fixed point"
        [ bytes ] test_optimize_preserves_physical_identity;
      test_case "optimized stylesheet reparses" [ bytes ]
        test_optimized_stylesheet_reparses;
      test_case "optimize monotonicity: optimized <= minified-alone" [ bytes ]
        test_optimize_minify_monotonicity;
      test_case "optimization preserves boundary shape" [ bytes ]
        test_optimization_preserves_boundary_shape;
      test_case "optimized reparse idempotent" [ bytes ]
        test_optimized_reparse_idempotent;
      test_case "minify fixpoint on interacting clusters" [ bytes ]
        test_minify_cluster_fixpoint;
      test_case "import namespace counts" [ bytes ] test_import_namespace_counts;
      test_case "at-rule counts stable" [ bytes ] test_atrule_counts_stable;
      test_case "cascade positive negative merge vectors" [ bytes ]
        test_cascade_merge_vectors;
      test_case "cascade shorthand importance vectors" [ bytes ]
        test_cascade_shorthand_importance_vectors;
      test_case "SMT selector-intersection dependency vectors" [ bytes ]
        test_smt_intersection_dependency_vectors;
      test_case "SMT biclique no-new-edges vectors" [ bytes ]
        test_biclique_no_new_edges;
      test_case "SMT ordered declaration vectors" [ bytes ]
        test_smt_property_order_vectors;
      test_case "positive layer statement vectors" [ bytes ]
        test_positive_layer_statement_vectors;
      test_case "layer order precedes specificity vectors" [ bytes ]
        test_layer_before_specificity;
      test_case "name-defining at-rules preserved" [ bytes ]
        test_name_defining_atrules_preserved;
    ] )
