open Cascade
open Context_helpers

let check_layered_eval_value name ~ctx ~layer_order ?layer ~expected input =
  let decl = Css_test_helpers.ok (Css.Declaration.of_string input) in
  Alcotest.(check decl_t)
    name
    (declaration_with_value decl expected)
    (Css.eval_declaration ctx ~layer_order ?layer decl)

let check_layered_eval_preserves name ~ctx ~layer_order ?layer input =
  let decl = Css_test_helpers.ok (Css.Declaration.of_string input) in
  Alcotest.(check decl_t)
    name decl
    (Css.eval_declaration ctx ~layer_order ?layer decl)

let specificity_score selector =
  let specificity = Css.Selector.specificity selector in
  (specificity.ids * 1_000_000)
  + (specificity.classes * 1_000)
  + specificity.elements

let declaration_source decl = Css.Declaration.to_string ~minify:true decl

let declaration_value_source decl =
  let source = declaration_source decl in
  let value =
    match String.index_opt source ':' with
    | None -> source
    | Some i -> String.sub source (i + 1) (String.length source - i - 1)
  in
  match String.index_opt value '!' with
  | None -> value
  | Some i -> String.sub value 0 i

let scope_selector_matches (document : Css.Context.document) = function
  | None -> true
  | Some selector ->
      Option.equal Css.Selector.equal document.scope (Some selector)
      || Css.Context.matches_selector document selector

let scope_boundary_allows document start boundary =
  scope_selector_matches document start
  &&
  match boundary with
  | None -> true
  | Some _ -> not (scope_selector_matches document boundary)

let rec conditional_matches query = function
  | Css.Stylesheet.Media_condition condition ->
      Css.Context.matches_media query condition
  | Supports_condition_test condition ->
      Css.Context.matches_supports query condition
  | And (left, right) ->
      conditional_matches query left && conditional_matches query right
  | Or (left, right) ->
      conditional_matches query left || conditional_matches query right

let add_matching_declarations ~source_order ~property ~origin ~layer
    ~specificity ~scope_hops acc declarations =
  List.fold_left
    (fun acc decl ->
      let order = !source_order in
      incr source_order;
      if Css.Declaration.property_name decl <> property then acc
      else
        let candidate : Css.Stylesheet.cascade_candidate =
          {
            origin;
            important = Css.Declaration.is_important decl;
            layer;
            specificity;
            scope_hops;
            source_order = order;
            value = declaration_value_source decl;
          }
        in
        candidate :: acc)
    acc declarations

let collect_declaration_statement ~source_order ~property ~origin ~layer
    ~current_specificity ~scope_hops acc (stmt : Css.Stylesheet.statement) =
  match stmt with
  | Declarations declarations -> (
      match current_specificity with
      | None -> (acc, None)
      | Some specificity ->
          ( add_matching_declarations ~source_order ~property ~origin ~layer
              ~specificity ~scope_hops acc declarations,
            None ))
  | _ -> (acc, None)

let rec collect_matching_block ~source_order ~property ~document ~query ~origin
    ~layer ~current_specificity ~scope_hops acc block =
  let step (acc, chain_matched) stmt =
    collect_matching_statement ~source_order ~property ~document ~query ~origin
      ~layer ~current_specificity ~scope_hops ~chain_matched acc stmt
  in
  fst (List.fold_left step (acc, None) block)

and collect_matching_statement ~source_order ~property ~document ~query ~origin
    ~layer ~current_specificity ~scope_hops ~chain_matched acc stmt =
  match
    collect_rule_like_statement ~source_order ~property ~document ~query ~origin
      ~layer ~current_specificity ~scope_hops acc stmt
  with
  | Some result -> result
  | None -> (
      match
        collect_conditional_statement ~source_order ~property ~document ~query
          ~origin ~layer ~current_specificity ~scope_hops ~chain_matched acc
          stmt
      with
      | Some result -> result
      | None ->
          collect_declaration_statement ~source_order ~property ~origin ~layer
            ~current_specificity ~scope_hops acc stmt)

and collect_rule_like_statement ~source_order ~property ~document ~query ~origin
    ~layer ~current_specificity ~scope_hops acc = function
  | Css.Stylesheet.Rule rule ->
      Some
        (if Css.Context.matches_selector document rule.selector then
           let specificity = specificity_score rule.selector in
           let acc =
             add_matching_declarations ~source_order ~property ~origin ~layer
               ~specificity ~scope_hops acc rule.declarations
           in
           ( collect_matching_block ~source_order ~property ~document ~query
               ~origin ~layer ~current_specificity:(Some specificity)
               ~scope_hops acc rule.nested,
             None )
         else (acc, None))
  | Layer (name, block) ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query ~origin
            ~layer:(Option.map Css.Stylesheet.string_of_layer_name name)
            ~current_specificity ~scope_hops acc block,
          None )
  | Starting_style block ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query
            ~origin ~layer ~current_specificity ~scope_hops acc block,
          None )
  | Origin (origin, block) ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query
            ~origin ~layer ~current_specificity ~scope_hops acc block,
          None )
  | Moz_document (_, block) ->
      Some
        ( collect_matching_block ~source_order ~property ~document ~query
            ~origin ~layer ~current_specificity ~scope_hops acc block,
          None )
  | Scope (start, boundary, block) ->
      Some
        (if scope_boundary_allows document start boundary then
           ( collect_matching_block ~source_order ~property ~document ~query
               ~origin ~layer ~current_specificity ~scope_hops:(Some 0) acc
               block,
             None )
         else (acc, None))
  | _ -> None

and collect_conditional_statement ~source_order ~property ~document ~query
    ~origin ~layer ~current_specificity ~scope_hops ~chain_matched acc =
  let collect_if matched block =
    let acc =
      if matched then
        collect_matching_block ~source_order ~property ~document ~query ~origin
          ~layer ~current_specificity ~scope_hops acc block
      else acc
    in
    (acc, Some matched)
  in
  function
  | Media (condition, block) ->
      Some (collect_if (Css.Context.matches_media query condition) block)
  | Supports (condition, block) ->
      Some (collect_if (Css.Context.matches_supports query condition) block)
  | Container (name, condition, block) ->
      let matches =
        match condition with
        | Some c -> Css.Context.matches_container query ?name c
        | None -> true
      in
      Some (collect_if matches block)
  | When (condition, block) ->
      Some (collect_if (conditional_matches query condition) block)
  | Else (condition, block) ->
      Some
        (match chain_matched with
        | None -> (acc, None)
        | Some previous_matched ->
            let condition_matched =
              match condition with
              | None -> true
              | Some condition -> conditional_matches query condition
            in
            let matched = (not previous_matched) && condition_matched in
            let acc =
              if matched then
                collect_matching_block ~source_order ~property ~document ~query
                  ~origin ~layer ~current_specificity ~scope_hops acc block
              else acc
            in
            (acc, Some (previous_matched || matched)))
  | _ -> None

let cascade_layer_candidate_of (c : Css.Stylesheet.cascade_candidate) :
    Css.Stylesheet.cascade_layer_candidate =
  {
    layer = c.layer;
    important = c.important;
    source_order = c.source_order;
    value = c.value;
  }

let cascade_origin_candidate_of (c : Css.Stylesheet.cascade_candidate) :
    Css.Stylesheet.cascade_origin_candidate =
  {
    origin = c.origin;
    important = c.important;
    source_order = c.source_order;
    value = c.value;
  }

let same_layer_candidate (c : Css.Stylesheet.cascade_candidate)
    (l : Css.Stylesheet.cascade_layer_candidate) =
  l.layer = c.layer && l.important = c.important
  && l.source_order = c.source_order

let same_origin_candidate (c : Css.Stylesheet.cascade_candidate)
    (l : Css.Stylesheet.cascade_origin_candidate) =
  Css.Stylesheet.equal_cascade_origin l.origin c.origin
  && l.important = c.important
  && l.source_order = c.source_order

let lower_layer_candidates ~layer_order
    (candidate : Css.Stylesheet.cascade_candidate) candidates =
  Css.Stylesheet.cascade_revert_layer_candidates ~layer_order
    ~important:candidate.important ~current_layer:candidate.layer
    (List.map cascade_layer_candidate_of candidates)

let lower_origin_candidates (candidate : Css.Stylesheet.cascade_candidate)
    candidates =
  Css.Stylesheet.cascade_revert_origin_candidates ~important:candidate.important
    ~current_origin:candidate.origin
    (List.map cascade_origin_candidate_of candidates)

let rec winning_resolved_candidate ~layer_order candidates =
  Option.bind (Css.Stylesheet.winning_cascade_candidate ~layer_order candidates)
    (fun (candidate : Css.Stylesheet.cascade_candidate) ->
      match candidate.value with
      | "revert-layer" ->
          let lower =
            lower_layer_candidates ~layer_order candidate candidates
          in
          let candidates =
            List.filter
              (fun candidate ->
                List.exists (same_layer_candidate candidate) lower)
              candidates
          in
          winning_resolved_candidate ~layer_order candidates
      | "revert" ->
          let lower = lower_origin_candidates candidate candidates in
          let candidates =
            List.filter
              (fun candidate ->
                List.exists (same_origin_candidate candidate) lower)
              candidates
          in
          winning_resolved_candidate ~layer_order candidates
      | value -> Some (candidate, value))

let resolve_stylesheet_property ?(layer_order = []) ~ctx ~document ~query
    ~property stylesheet =
  let source_order = ref 0 in
  let candidates =
    collect_matching_block ~source_order ~property ~document ~query
      ~origin:Css.Stylesheet.Author ~layer:None ~current_specificity:None
      ~scope_hops:None [] stylesheet
  in
  winning_resolved_candidate ~layer_order candidates
  |> Option.map (fun ((candidate : Css.Stylesheet.cascade_candidate), value) ->
      let decl =
        Css_test_helpers.ok
          (Css.Declaration.of_string (property ^ ": " ^ value))
      in
      match candidate.layer with
      | None -> Css.eval_declaration ctx ~layer_order decl
      | Some layer -> Css.eval_declaration ctx ~layer_order ~layer decl)

let check_resolved_property ?layer_order name ~ctx ~document ~query ~property
    ~expected stylesheet =
  let expected = Css_test_helpers.ok (Css.Declaration.of_string expected) in
  let actual =
    resolve_stylesheet_property ?layer_order ~ctx ~document ~query ~property
      (stylesheet_of_string stylesheet)
  in
  Alcotest.(check (option decl_t)) name (Some expected) actual

let check_ast_resolved_property ?layer_order name ~ctx ~document ~query
    ~property ~expected stylesheet =
  let expected = Css_test_helpers.ok (Css.Declaration.of_string expected) in
  let actual =
    resolve_stylesheet_property ?layer_order ~ctx ~document ~query ~property
      stylesheet
  in
  Alcotest.(check (option decl_t)) name (Some expected) actual

let layered_eval_ast_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let custom_properties =
    [
      Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
      Css.Declaration.custom_property ~layer:"base" "--base-gap"
        "calc(var(--spacing) * 2)";
      Css.Declaration.custom_property ~layer:"components" "--component-gap"
        "calc(var(--base-gap) + 1rem)";
      Css.Declaration.custom_property ~layer:"utilities" "--utility-gap"
        "calc(var(--component-gap) + 2vh)";
      Css.Declaration.custom_property ~layer:"utilities" "--future-gap"
        "calc(var(--component-gap) + var(--user-gap))";
      Css.Declaration.custom_property ~layer:"utilities" "--component-gap"
        "revert-layer";
      Css.Declaration.custom_property ~layer:"utilities" "--color-brand"
        "revert-layer";
      Css.Declaration.custom_property ~layer:"base" "--color-brand"
        "oklch(60% 0.12 250)";
      Css.Declaration.custom_property ~layer:"theme" "--color-brand"
        "oklch(70% 0.15 250)";
    ]
  in
  let weak_ctx =
    Css.Context.v ~custom_properties
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: canvastext");
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok
            (Css.Declaration.of_string "color: var(--color-brand)");
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  let strong_ctx =
    Css.Context.v ~custom_properties
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: canvastext");
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok
            (Css.Declaration.of_string "color: var(--color-brand)");
        ]
      ~root_font_size:(Css.Values.Px 16.) ~viewport_height:(Css.Values.Px 1000.)
      ()
  in
  check_eval "eval resolves layered calc across all cascade layers"
    ~ctx:strong_ctx ~layer_order ~layer:"utilities"
    ~expected:"margin-left: 44px" "margin-left: var(--utility-gap)";
  check_eval "eval preserves unknown viewport leaf after layered var resolution"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    ~expected:"margin-left: calc(24px + 2vh)" "margin-left: var(--utility-gap)";
  check_eval "eval preserves abstract var leaf after known layers resolve"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    ~expected:"margin-left: calc(24px + var(--user-gap))"
    "margin-left: var(--future-gap)";
  check_eval "eval resolves custom property revert-layer before calc eval"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities" ~expected:"padding: 24px"
    "padding: var(--component-gap)";
  check_eval "eval resolves layered color revert through inherited value"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    ~expected:"color: oklch(60% 0.12 250)" "color: revert-layer";
  check_eval "eval resolves theme revert-layer to property initial value"
    ~ctx:weak_ctx ~layer_order ~layer:"theme" ~expected:"margin-left: 0"
    "margin-left: revert-layer";
  check_eval_idempotent "layered eval is idempotent on symbolic AST"
    ~ctx:weak_ctx ~layer_order ~layer:"utilities"
    "margin-left: var(--utility-gap)";
  check_eval_context_extension
    "layered eval context extension reaches the same final AST" ~layer_order
    ~layer:"utilities" ~weak_ctx ~strong_ctx ~expected:"margin-left: 44px"
    "margin-left: var(--utility-gap)"

let layered_vars_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--color-brand-500"
            "oklch(70% 0.15 250)";
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"theme" "--font-sans"
            "Inter, sans-serif";
          Css.Declaration.custom_property ~layer:"base" "--color-brand-500"
            "oklch(60% 0.12 250)";
          Css.Declaration.custom_property ~layer:"components" "--card-padding"
            "calc(var(--spacing) * 6)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-translate-x"
            "calc(var(--spacing) * 4)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-bg-opacity"
            "0.5";
          Css.Declaration.custom_property "--color-brand-500"
            "oklch(40% 0.10 250)";
          Css.Declaration.important
            (Css.Declaration.custom_property ~layer:"utilities"
               "--tw-ring-color" "red");
          Css.Declaration.important
            (Css.Declaration.custom_property ~layer:"theme" "--tw-ring-color"
               "blue");
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value
    "tailwind theme variable is visible to utility declarations" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"oklch(70% 0.15 250)"
    "color: var(--color-brand-500)";
  check_layered_eval_value
    "normal unlayered app variable overrides tailwind layered tokens" ~ctx
    ~layer_order ~expected:"oklch(40% 0.10 250)" "color: var(--color-brand-500)";
  check_layered_eval_value
    "base layer token overrides theme for base-scoped resolution" ~ctx
    ~layer_order ~layer:"base" ~expected:"oklch(60% 0.12 250)"
    "color: var(--color-brand-500)";
  check_layered_eval_value
    "component token resolves through tailwind theme spacing" ~ctx ~layer_order
    ~layer:"components" ~expected:"24px" "padding: var(--card-padding)";
  check_layered_eval_value
    "utility-local --tw variable resolves through theme spacing" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"16px"
    "translate: var(--tw-translate-x)";
  check_layered_eval_value "utility-local opacity variable resolves as a number"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"0.5"
    "opacity: var(--tw-bg-opacity)";
  check_layered_eval_value
    "important tailwind theme layer beats important utility layer" ~ctx
    ~layer_order ~expected:"blue" "outline-color: var(--tw-ring-color)";
  check_layered_eval_preserves "unknown layer is rejected for scoped resolution"
    ~ctx ~layer_order ~layer:"unknown" "color: var(--color-brand-500)"

let test_layered_computed_revert_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~initial_values:
        [
          Css_test_helpers.ok (Css.Declaration.of_string "color: canvastext");
          Css_test_helpers.ok (Css.Declaration.of_string "margin-left: 0");
        ]
      ~inherited_values:
        [
          Css_test_helpers.ok
            (Css.Declaration.of_string "color: var(--color-brand-500)");
        ]
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--color-brand-500"
            "oklch(70% 0.15 250)";
          Css.Declaration.custom_property ~layer:"base" "--color-brand-500"
            "oklch(60% 0.12 250)";
          Css.Declaration.custom_property ~layer:"utilities" "--color-brand-500"
            "revert-layer";
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"components" "--card-padding"
            "calc(var(--spacing) * 6)";
          Css.Declaration.custom_property ~layer:"utilities" "--card-padding"
            "revert-layer";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value "revert-layer utility token rolls back to base token"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"oklch(60% 0.12 250)"
    "color: var(--color-brand-500)";
  check_layered_eval_value
    "revert-layer utility spacing rolls back to component token then theme \
     token"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"24px"
    "padding: var(--card-padding)";
  check_layered_eval_value
    "property revert-layer rolls back to lower cascaded layer" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"oklch(60% 0.12 250)" "color: revert-layer";
  check_layered_eval_value
    "property revert-layer in theme falls back to initial" ~ctx ~layer_order
    ~layer:"theme" ~expected:"0" "margin-left: revert-layer";
  check_layered_eval_preserves "revert-layer cycle is unresolved" ~ctx
    ~layer_order ~layer:"utilities" "margin-left: var(--missing, revert-layer)"

let tw_vars_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--color-red-500"
            "239 68 68";
          Css.Declaration.custom_property ~layer:"theme" "--color-blue-500"
            "59 130 246";
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"theme" "--radius-lg" "0.5rem";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-bg-opacity"
            "0.75";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-border-opacity" "0.5";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-gradient-from" "rgb(var(--color-red-500) / 1)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-gradient-to"
            "rgb(var(--color-blue-500) / 0)";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-gradient-stops"
            "var(--tw-gradient-from), var(--tw-gradient-to)";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-ring-offset-width" "2px";
          Css.Declaration.custom_property ~layer:"utilities"
            "--tw-ring-offset-color" "#fff";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-ring-color"
            "rgb(var(--color-blue-500) / 0.5)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-translate-x"
            "calc(var(--spacing) * 4)";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-rotate"
            "45deg";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-scale-x" "1";
          Css.Declaration.custom_property ~layer:"utilities" "--tw-scale-y" "1";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value
    "tailwind rgb slash alpha resolves theme channel and opacity var" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"rgb(239 68 68/.75)"
    "background-color: rgb(var(--color-red-500) / var(--tw-bg-opacity))";
  check_layered_eval_value "tailwind border opacity var resolves independently"
    ~ctx ~layer_order ~layer:"utilities" ~expected:"rgb(59 130 246/.5)"
    "border-color: rgb(var(--color-blue-500) / var(--tw-border-opacity))";
  check_layered_eval_value
    "tailwind gradient stops resolve chained utility variables" ~ctx
    ~layer_order ~layer:"utilities"
    ~expected:"linear-gradient(to right,rgb(239 68 68/1),rgb(59 130 246/0))"
    "background-image: linear-gradient(to right, var(--tw-gradient-stops))";
  check_layered_eval_value
    "tailwind ring shadow resolves offset and color variables" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"0 0 0 2px #fff,0 0 0 5px rgb(59 130 246/.5)"
    "box-shadow: 0 0 0 var(--tw-ring-offset-width) \
     var(--tw-ring-offset-color), 0 0 0 calc(3px + \
     var(--tw-ring-offset-width)) var(--tw-ring-color)";
  check_layered_eval_value
    "tailwind transform utility variables resolve in order" ~ctx ~layer_order
    ~layer:"utilities"
    ~expected:"translate(16px) rotate(45deg) scaleX(1) scaleY(1)"
    "transform: translate(var(--tw-translate-x)) rotate(var(--tw-rotate)) \
     scaleX(var(--tw-scale-x)) scaleY(var(--tw-scale-y))";
  check_layered_eval_value
    "tailwind radius token resolves through theme variable" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"8px" "border-radius: var(--radius-lg)";
  (* CSS Values 4: once the fallback var() has selected the calc() and --spacing
     has resolved to a rem length, computed-value simplification has enough
     information to reduce the expression to px. *)
  check_layered_eval_value
    "tailwind nested fallback uses theme token when utility var is absent" ~ctx
    ~layer_order ~layer:"utilities" ~expected:"16px"
    "margin-left: var(--tw-space-x, calc(var(--spacing) * 4))";
  check_layered_eval_preserves
    "tailwind utility var without fallback remains unresolved" ~ctx ~layer_order
    ~layer:"utilities" "outline-width: var(--tw-outline-width)"

let tw_layer_order_contract () =
  let layer_order = [ "theme"; "base"; "components"; "utilities" ] in
  let ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css.Declaration.custom_property ~layer:"theme" "--spacing" "0.25rem";
          Css.Declaration.custom_property ~layer:"components" "--spacing"
            "0.5rem";
          Css.Declaration.custom_property ~layer:"utilities" "--spacing" "1rem";
          Css.Declaration.custom_property ~layer:"theme" "--color-brand" "red";
          Css.Declaration.custom_property ~layer:"components" "--color-brand"
            "blue";
          Css.Declaration.custom_property ~layer:"utilities" "--color-brand"
            "green";
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  check_layered_eval_value
    "tailwind utility layer token wins within utility layer" ~ctx ~layer_order
    ~layer:"utilities" ~expected:"16px" "margin-left: var(--spacing)";
  check_layered_eval_value
    "tailwind component layer token wins within component layer" ~ctx
    ~layer_order ~layer:"components" ~expected:"8px"
    "margin-left: var(--spacing)";
  check_layered_eval_value "tailwind theme layer token wins within theme layer"
    ~ctx ~layer_order ~layer:"theme" ~expected:"4px"
    "margin-left: var(--spacing)";
  check_layered_eval_value
    "tailwind unscoped normal lookup uses final layered token" ~ctx ~layer_order
    ~expected:"green" "color: var(--color-brand)";
  check_layered_eval_value
    "tailwind component scoped lookup ignores later utility token" ~ctx
    ~layer_order ~layer:"components" ~expected:"blue"
    "color: var(--color-brand)"

let cascade_rule_resolver_contract () =
  let layer_order = [ "reset"; "theme"; "components"; "utilities" ] in
  let value_ctx =
    Css.Context.v
      ~custom_properties:
        [
          Css_test_helpers.ok
            (Css.Declaration.of_string "--theme-color: oklch(70% 0.15 250)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--app-color: oklch(40% 0.10 250)");
          Css_test_helpers.ok
            (Css.Declaration.of_string "--scoped-color: green");
        ]
      ~root_font_size:(Css.Values.Px 16.) ()
  in
  let query =
    Css.Context.query ~media_type:"screen"
      ~media_features:[ feature "width" (px 1024.) ]
      ~supports:
        [
          Css.Supports.property "display" "grid";
          Css.Supports.property "container-type" "inline-size";
        ]
      ~container_name:"card"
      ~container_features:[ container_feature "inline-size" (px 640.) ]
      ()
  in
  let primary =
    Css.Context.document ~element:"button" ~classes:[ "btn"; "primary" ] ()
  in
  check_resolved_property "unlayered rule beats matching explicit layers"
    ~layer_order ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: oklch(40% 0.10 250)"
    {|
      @layer reset, theme, components, utilities;
      @layer theme { .btn { color: var(--theme-color); } }
      @layer utilities { .btn { color: orange; } }
      @media screen and (width >= 40rem) { .btn { color: blue; } }
      @supports (display: ruby) { .btn.primary { color: red; } }
      @supports (display: grid) { .btn.primary { color: var(--app-color); } }
    |};
  let id_button =
    Css.Context.document ~element:"button" ~classes:[ "btn"; "primary" ]
      ~ids:[ "submit" ] ()
  in
  check_resolved_property
    "specificity chooses id selector after condition filtering" ~layer_order
    ~ctx:value_ctx ~document:id_button ~query ~property:"color"
    ~expected:"color: purple"
    {|
      .btn.primary { color: var(--app-color); }
      #submit { color: purple; }
      @supports (display: ruby) { #submit { color: red; } }
    |};
  check_resolved_property "important layer order reverses normal layer order"
    ~layer_order ~ctx:value_ctx ~document:primary ~query
    ~property:"border-color" ~expected:"border-color: red"
    {|
      @layer reset, theme, components, utilities;
      .btn { border-color: green !important; }
      @layer reset { .btn { border-color: red !important; } }
      @layer utilities { .btn { border-color: blue !important; } }
    |};
  check_resolved_property
    "revert-layer rolls back to the next lower normal layer" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: green"
    {|
      @layer reset, theme, components, utilities;
      @layer theme { .btn { color: red; } }
      @layer components { .btn { color: green; } }
      @layer utilities { .btn { color: revert-layer; } }
    |};
  let scoped =
    Css.Context.document ~scope:(Css.Selector.v ".card") ~element:"h2"
      ~classes:[ "title" ] ()
  in
  check_resolved_property
    "scope proximity beats later source order at equal specificity" ~layer_order
    ~ctx:value_ctx ~document:scoped ~query ~property:"color"
    ~expected:"color: green"
    {|
      @scope (.card) { .title { color: var(--scoped-color); } }
      .title { color: blue; }
    |};
  check_resolved_property
    "scope boundary suppresses declarations outside the active scope"
    ~layer_order ~ctx:value_ctx
    ~document:
      (Css.Context.document ~scope:(Css.Selector.v ".card") ~element:"h2"
         ~classes:[ "title"; "boundary" ] ())
    ~query ~property:"color" ~expected:"color: blue"
    {|
      @scope (.card) to (.boundary) { .title { color: green; } }
      .title { color: blue; }
    |};
  let ast_rule selector declarations =
    Css.rule ~selector:(Css.Selector.v selector)
      (List.map
         (fun d -> Css_test_helpers.ok (Css.Declaration.of_string d))
         declarations)
  in
  let origin_sheet =
    [
      Css.Stylesheet.with_origin Css.Stylesheet.User_agent
        [ ast_rule ".btn" [ "color: black" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.Author
        [ ast_rule ".btn" [ "color: red" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.User
        [ ast_rule ".btn" [ "color: blue !important" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.Transition
        [ ast_rule ".btn" [ "color: yellow" ] ];
    ]
  in
  check_ast_resolved_property
    "origin and animation/transition ranks are applied" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: yellow" origin_sheet;
  let revert_origin_sheet =
    [
      Css.Stylesheet.with_origin Css.Stylesheet.User_agent
        [ ast_rule ".btn" [ "color: black" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.User
        [ ast_rule ".btn" [ "color: blue" ] ];
      Css.Stylesheet.with_origin Css.Stylesheet.Author
        [ ast_rule ".btn" [ "color: revert" ] ];
    ]
  in
  check_ast_resolved_property
    "revert rolls back to the next lower cascade origin" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"color"
    ~expected:"color: blue" revert_origin_sheet;
  check_resolved_property "nested conditional declarations inherit parent rule"
    ~layer_order ~ctx:value_ctx ~document:primary ~query
    ~property:"outline-color" ~expected:"outline-color: oklch(40% 0.10 250)"
    {|
      .btn {
        @media screen and (width >= 40rem) {
          outline-color: var(--app-color);
        }
        @supports (display: ruby) {
          outline-color: red;
        }
      }
    |};
  check_resolved_property
    "source order breaks ties after equal origin layer specificity and scope"
    ~layer_order ~ctx:value_ctx ~document:primary ~query
    ~property:"background-color" ~expected:"background-color: blue"
    {|
      .btn { background-color: red; }
      .btn { background-color: blue; }
    |};
  check_resolved_property
    "normal named layer order follows the declared layer statement" ~layer_order
    ~ctx:value_ctx ~document:primary ~query ~property:"outline-color"
    ~expected:"outline-color: blue"
    {|
      @layer reset, theme, components, utilities;
      @layer utilities { .btn { outline-color: blue; } }
      @layer theme { .btn { outline-color: red; } }
    |}

let suite =
  ( "context_cascade",
    [
      Alcotest.test_case "layered eval AST contract" `Quick
        layered_eval_ast_contract;
      Alcotest.test_case "layered custom property context contract" `Quick
        layered_vars_contract;
      Alcotest.test_case "layered computed revert contract" `Quick
        test_layered_computed_revert_contract;
      Alcotest.test_case "tailwind variable layer contract" `Quick
        tw_vars_contract;
      Alcotest.test_case "tailwind layer order contract" `Quick
        tw_layer_order_contract;
      Alcotest.test_case "cascade rule resolver contract" `Quick
        cascade_rule_resolver_contract;
    ] )
