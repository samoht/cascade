type css_wide_keyword = Inherit | Initial | Unset | Revert | Revert_layer

(* [pp_property_value] is exhaustive over the sealed property GADT, so this
   generic path cannot silently miss a newly added property. CSS-wide keywords
   are reserved from every property's ordinary grammar, making an exact minified
   rendering unambiguous here. *)
let css_wide_of_property_value property value =
  match
    Pp.to_string ~minify:true Properties.pp_property_value (property, value)
  with
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | _ -> None

let css_wide_of_length (value : Values.length) =
  match value with
  | Values.Inherit -> Some Inherit
  | Values.Initial -> Some Initial
  | Values.Unset -> Some Unset
  | Values.Revert -> Some Revert
  | Values.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_length_list (value : Values.length list) =
  match value with [ length ] -> css_wide_of_length length | _ -> None

let css_wide_of_length_percentage (value : Values.length_percentage) =
  match value with
  | Values.Length length -> css_wide_of_length length
  | _ -> None

let css_wide_of_border_width (value : Properties.border_width) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_border_widths (value : Properties.border_width list) =
  match value with [ width ] -> css_wide_of_border_width width | _ -> None

let css_wide_of_opacity (value : Properties.opacity) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_stroke_width (value : Properties.stroke_width) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_font_size (value : Properties.font_size) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_font_family (value : Properties.font_family) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | _ -> None

let css_wide_of_rotate_value (value : Properties.rotate_value) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_duration (value : Values.duration) =
  match value with
  | Values.Inherit -> Some Inherit
  | Values.Initial -> Some Initial
  | Values.Unset -> Some Unset
  | Values.Revert -> Some Revert
  | Values.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_display (value : Properties.display) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_position (value : Properties.position) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_visibility (value : Properties.visibility) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_clear (value : Properties.clear) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_float_side (value : Properties.float_side) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_scale (value : Properties.scale) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_translate_value (value : Properties.translate_value) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_transform (value : Properties.transform) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_transforms (value : Properties.transform list) =
  match value with
  | [ transform ] -> css_wide_of_transform transform
  | _ -> None

let css_wide_of_filter (value : Properties.filter) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_shadow (value : Properties.shadow) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_border_radius (value : Properties.border_radius) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_color (value : Values.color) =
  match value with
  | Values.Inherit -> Some Inherit
  | Values.Initial -> Some Initial
  | Values.Unset -> Some Unset
  | Values.Revert -> Some Revert
  | Values.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_colors (value : Values.color list) =
  match value with [ color ] -> css_wide_of_color color | _ -> None

let css_wide_of_background_image (value : Properties.background_image) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_background_images (value : Properties.background_image list) =
  match value with [ image ] -> css_wide_of_background_image image | _ -> None

let css_wide_of_animation (value : Properties.animation) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | _ -> None

let css_wide_of_animations (value : Properties.animation list) =
  match value with
  | [ animation ] -> css_wide_of_animation animation
  | _ -> None

let css_wide_of_transition (value : Properties.transition) =
  match value with
  | Properties.Inherit -> Some Inherit
  | Properties.Initial -> Some Initial
  | Properties.Unset -> Some Unset
  | Properties.Revert -> Some Revert
  | Properties.Revert_layer -> Some Revert_layer
  | _ -> None

let css_wide_of_transitions (value : Properties.transition list) =
  match value with
  | [ transition ] -> css_wide_of_transition transition
  | _ -> None

let rec declaration_with_importance important = function
  | Declaration.Declaration { property; value; important = _; _ } ->
      Declaration.v ~important property value
  | Declaration.Theme_guarded { var_name; decl; _ } ->
      Declaration.theme_guarded ~var_name
        (declaration_with_importance important decl)

let property_name (type a) (property : a Properties.property) =
  Pp.to_string ~minify:true Properties.pp_property property
