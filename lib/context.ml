(** Explicit contexts for CSS AST/value transforms. *)

include Context_intf
include Context_core
include Context_pp
open Context_length
open Context_match
open Context_numeric
open Context_css_wide
open Context_paint
open Context_keyword

(** {1 CSS Evaluator Pipeline}

    Browser CSS engines structure rule application as a sequence of stages
    operating on closed inputs:

    + {b Tokenization / parsing} produces structured ASTs (handled by the
      individual module readers).
    + {b Length canonicalisation} ([Length]) maps each unit to absolute pixels
      using the supplied font/viewport/container references.
    + {b Condition evaluation} ([Match_supports], [Match_media],
      [Match_container]) walks the structured query AST against the explicit
      feature/declaration tables in [query].
    + {b Selector matching} ([Match_selector]) decides whether a selector would
      attach to the element described by [document]. The element is not part of
      a tree, so combinators reduce to matching the rightmost compound selector.
    + {b Cascade + computed value} ([eval]) resolves CSS-wide keywords, expands
      [var()], evaluates [calc()], and converts relative lengths against the
      property-value context in the typed AST.
    + {b Loader} ([Import], [Url]) resolves relative URLs and looks up imported
      stylesheets, applying [@import] guards. *)

(** {2 Public API surface (forwards to the internal modules)} *)

let matches_supports = Match_supports.eval
let matches_media = Match_media.eval
let matches_container = Match_container.eval
let matches_selector = Match_selector.eval
let resolve_url = Url.resolve
let load_import = Import.load

(* Every kind resolves the same way - simplify the value in context, then let a
   css-wide keyword in the result take over - so the simplifier and the css-wide
   reader are all that vary. Keeping them in one total table is what makes a new
   kind a compile error rather than a value that falls through unsimplified. *)
let kind_simplifier : type a.
    layer_order:string list ->
    ?layer:string ->
    t ->
    a Properties.property_value_kind ->
    (a -> a) * (a -> css_wide_keyword option) =
 fun ~layer_order ?layer ctx kind ->
  let length_ctx = Length.of_t ctx in
  match kind with
  | Properties.Length ->
      (Length.simplify ~layer_order ?layer ctx length_ctx, css_wide_of_length)
  | Properties.Lengths ->
      ( simplify_lengths_value ~layer_order ?layer ctx length_ctx,
        css_wide_of_length_list )
  | Properties.Length_percentage ->
      ( simplify_length_percentage ~layer_order ?layer ctx length_ctx,
        css_wide_of_length_percentage )
  | Properties.Border_width ->
      ( simplify_border_width ~layer_order ?layer ctx length_ctx,
        css_wide_of_border_width )
  | Properties.Border_widths ->
      ( List.map (simplify_border_width ~layer_order ?layer ctx length_ctx),
        css_wide_of_border_widths )
  | Properties.Font_size ->
      ( simplify_font_size ~layer_order ?layer ctx length_ctx,
        css_wide_of_font_size )
  | Properties.Translate ->
      ( simplify_translate_value ~layer_order ?layer ctx length_ctx,
        css_wide_of_translate_value )
  | Properties.Transform ->
      ( List.map (simplify_transform ~layer_order ?layer ctx length_ctx),
        css_wide_of_transforms )
  | Properties.Filter ->
      (simplify_filter ~layer_order ?layer ctx length_ctx, css_wide_of_filter)
  | Properties.Shadow ->
      (simplify_shadow ~layer_order ?layer ctx length_ctx, css_wide_of_shadow)
  | Properties.Border_radius ->
      ( simplify_border_radius ~layer_order ?layer ctx length_ctx,
        css_wide_of_border_radius )
  | Properties.Opacity ->
      (simplify_opacity ~layer_order ?layer ctx, css_wide_of_opacity)
  | Properties.Rotate ->
      (simplify_rotate_value ~layer_order ?layer ctx, css_wide_of_rotate_value)
  | Properties.Duration ->
      (simplify_duration ~layer_order ?layer ctx, css_wide_of_duration)
  | Properties.Display ->
      (simplify_display ~layer_order ?layer ctx, css_wide_of_display)
  | Properties.Position ->
      (simplify_position ~layer_order ?layer ctx, css_wide_of_position)
  | Properties.Visibility ->
      (simplify_visibility ~layer_order ?layer ctx, css_wide_of_visibility)
  | Properties.Clear ->
      (simplify_clear ~layer_order ?layer ctx, css_wide_of_clear)
  | Properties.Float ->
      (simplify_float_side ~layer_order ?layer ctx, css_wide_of_float_side)
  | Properties.Number_percentage ->
      (simplify_number_percentage ~layer_order ?layer ctx, fun _ -> None)
  | Properties.Scale ->
      (simplify_scale ~layer_order ?layer ctx, css_wide_of_scale)
  | Properties.Animation ->
      let duration = simplify_duration ~layer_order ?layer ctx in
      ( List.map (simplify_animation_item ~layer_order ?layer ctx duration),
        css_wide_of_animations )
  | Properties.Transition ->
      let duration = simplify_duration ~layer_order ?layer ctx in
      ( List.map (simplify_transition_item ~layer_order ?layer ctx duration),
        css_wide_of_transitions )
  | Properties.Color ->
      (simplify_color ~layer_order ?layer ctx, css_wide_of_color)
  | Properties.Colors ->
      (List.map (simplify_color ~layer_order ?layer ctx), css_wide_of_colors)
  | Properties.Animation_name ->
      (simplify_animation_name ~layer_order ?layer ctx, fun _ -> None)
  | Properties.Background ->
      (List.map (simplify_background ~layer_order ?layer ctx), fun _ -> None)
  | Properties.Background_image ->
      ( simplify_background_image ~layer_order ?layer ctx,
        css_wide_of_background_image )
  | Properties.Background_images ->
      ( List.map (simplify_background_image ~layer_order ?layer ctx),
        css_wide_of_background_images )
  | Properties.Font_src ->
      (simplify_font_src ~layer_order ?layer ctx, fun _ -> None)
  | Properties.Font_family ->
      (simplify_font_family ~layer_order ?layer ctx, css_wide_of_font_family)
  | Properties.Stroke_width ->
      ( simplify_stroke_width ~layer_order ?layer ctx length_ctx,
        css_wide_of_stroke_width )

let rec eval_typed ?layer_order ?layer ctx decl =
  let ctx = scope ?layer_order ?layer ctx in
  let layer_order = ctx.layer_order in
  let layer = ctx.layer in
  match decl with
  | Declaration.Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name
        (eval_typed ~layer_order ?layer ctx decl)
  | Declaration.Declaration { property; value; important; _ } -> (
      match css_wide_of_property_value property value with
      | Some keyword ->
          Option.value
            (resolve_css_wide_keyword ~layer_order ctx ~important ~property
               keyword)
            ~default:decl
      | None -> (
          match Properties.property_value_kind property with
          | Some kind ->
              eval_kind ~layer_order ?layer ctx kind property important value
          | None -> decl))

and resolve_typed : type b.
    layer_order:string list ->
    t ->
    (b -> b) ->
    (b -> css_wide_keyword option) ->
    b Properties.property ->
    bool ->
    b ->
    Declaration.declaration =
 fun ~layer_order ctx simplify css_wide_of property important value ->
  let value = simplify value in
  Option.value
    (Option.bind (css_wide_of value) (fun keyword ->
         resolve_css_wide_keyword ~layer_order ctx ~important ~property keyword))
    ~default:(Declaration.v ~important property value)

and eval_kind : type a.
    layer_order:string list ->
    ?layer:string ->
    t ->
    a Properties.property_value_kind ->
    a Properties.property ->
    bool ->
    a ->
    Declaration.declaration =
 fun ~layer_order ?layer ctx kind property important value ->
  let simplify, css_wide_of = kind_simplifier ~layer_order ?layer ctx kind in
  resolve_typed ~layer_order ctx simplify css_wide_of property important value

and resolve_css_wide_keyword : type b.
    layer_order:string list ->
    t ->
    important:bool ->
    property:b Properties.property ->
    css_wide_keyword ->
    Declaration.declaration option =
 fun ~layer_order ctx ~important ~property keyword ->
  let property_name = property_name property in
  let eval_source ctx decl = eval_typed ~layer_order ctx decl in
  let inherit_source () =
    match inherited_value property_name ctx with
    | Some decl -> Some decl
    | None -> initial_value property_name ctx
  in
  let initial_source () = initial_value property_name ctx in
  let unset_source () =
    if Properties.property_is_inherited property then inherit_source ()
    else initial_source ()
  in
  let cascade_source () =
    match cascade_rule_chain ctx ~property_name ~important with
    | None -> None
    | Some chain -> (
        match chain with
        | rule :: _ ->
            let cascade_rules =
              Option.map (remove_cascade_rule rule) ctx.cascade_rules
            in
            let ctx = { ctx with layer = rule.layer; cascade_rules } in
            Some (eval_source ctx rule.declaration)
        | [] -> Option.map (eval_source ctx) (unset_source ()))
  in
  match keyword with
  | Revert_layer ->
      Option.map (declaration_with_importance important) (cascade_source ())
  | _ ->
      let source =
        match keyword with
        | Inherit -> inherit_source ()
        | Initial -> initial_source ()
        | Unset -> unset_source ()
        | Revert -> None
        | Revert_layer -> assert false
      in
      Option.map
        (fun decl ->
          eval_source ctx decl |> declaration_with_importance important)
        source

(* [eval] is a declaration-level abstract interpreter: it rewrites a declaration
   to a more-defined one in the same typed AST, keeping unresolved subtrees as
   residual syntax. The typed walker is the only semantic path. *)
let eval ?layer_order ?layer ctx decl = eval_typed ?layer_order ?layer ctx decl
