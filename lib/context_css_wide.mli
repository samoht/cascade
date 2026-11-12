(** CSS-wide keywords ([inherit], [initial], [unset], [revert], [revert-layer]):
    recognising one in a typed value, and the declaration helpers their
    resolution needs. *)

(** The CSS-wide keywords (CSS Cascade 5 sec. 7.3). *)
type css_wide_keyword = Inherit | Initial | Unset | Revert | Revert_layer

val css_wide_of_property_value :
  'a Properties.property -> 'a -> css_wide_keyword option
(** [css_wide_of_property_value p v] is the CSS-wide keyword [v] is, as a value
    of [p]. *)

val css_wide_of_length : Values.length -> css_wide_keyword option
(** [css_wide_of_length v] is the CSS-wide keyword [v] is. *)

val css_wide_of_length_list : Values.length list -> css_wide_keyword option
(** [css_wide_of_length_list v] is the CSS-wide keyword [v] is. *)

val css_wide_of_length_percentage :
  Values.length_percentage -> css_wide_keyword option
(** [css_wide_of_length_percentage v] is the CSS-wide keyword [v] is. *)

val css_wide_of_border_width :
  Properties.border_width -> css_wide_keyword option
(** [css_wide_of_border_width v] is the CSS-wide keyword [v] is. *)

val css_wide_of_border_widths :
  Properties.border_width list -> css_wide_keyword option
(** [css_wide_of_border_widths v] is the CSS-wide keyword [v] is. *)

val css_wide_of_opacity : Properties.opacity -> css_wide_keyword option
(** [css_wide_of_opacity v] is the CSS-wide keyword [v] is. *)

val css_wide_of_stroke_width :
  Properties.stroke_width -> css_wide_keyword option
(** [css_wide_of_stroke_width v] is the CSS-wide keyword [v] is. *)

val css_wide_of_font_size : Properties.font_size -> css_wide_keyword option
(** [css_wide_of_font_size v] is the CSS-wide keyword [v] is. *)

val css_wide_of_font_family : Properties.font_family -> css_wide_keyword option
(** [css_wide_of_font_family v] is the CSS-wide keyword [v] is. *)

val css_wide_of_rotate_value :
  Properties.rotate_value -> css_wide_keyword option
(** [css_wide_of_rotate_value v] is the CSS-wide keyword [v] is. *)

val css_wide_of_duration : Values.duration -> css_wide_keyword option
(** [css_wide_of_duration v] is the CSS-wide keyword [v] is. *)

val css_wide_of_display : Properties.display -> css_wide_keyword option
(** [css_wide_of_display v] is the CSS-wide keyword [v] is. *)

val css_wide_of_position : Properties.position -> css_wide_keyword option
(** [css_wide_of_position v] is the CSS-wide keyword [v] is. *)

val css_wide_of_visibility : Properties.visibility -> css_wide_keyword option
(** [css_wide_of_visibility v] is the CSS-wide keyword [v] is. *)

val css_wide_of_clear : Properties.clear -> css_wide_keyword option
(** [css_wide_of_clear v] is the CSS-wide keyword [v] is. *)

val css_wide_of_float_side : Properties.float_side -> css_wide_keyword option
(** [css_wide_of_float_side v] is the CSS-wide keyword [v] is. *)

val css_wide_of_scale : Properties.scale -> css_wide_keyword option
(** [css_wide_of_scale v] is the CSS-wide keyword [v] is. *)

val css_wide_of_translate_value :
  Properties.translate_value -> css_wide_keyword option
(** [css_wide_of_translate_value v] is the CSS-wide keyword [v] is. *)

val css_wide_of_transforms :
  Properties.transform list -> css_wide_keyword option
(** [css_wide_of_transforms v] is the CSS-wide keyword [v] is. *)

val css_wide_of_filter : Properties.filter -> css_wide_keyword option
(** [css_wide_of_filter v] is the CSS-wide keyword [v] is. *)

val css_wide_of_shadow : Properties.shadow -> css_wide_keyword option
(** [css_wide_of_shadow v] is the CSS-wide keyword [v] is. *)

val css_wide_of_border_radius :
  Properties.border_radius -> css_wide_keyword option
(** [css_wide_of_border_radius v] is the CSS-wide keyword [v] is. *)

val css_wide_of_color : Values.color -> css_wide_keyword option
(** [css_wide_of_color v] is the CSS-wide keyword [v] is. *)

val css_wide_of_colors : Values.color list -> css_wide_keyword option
(** [css_wide_of_colors v] is the CSS-wide keyword [v] is. *)

val css_wide_of_background_image :
  Properties.background_image -> css_wide_keyword option
(** [css_wide_of_background_image v] is the CSS-wide keyword [v] is. *)

val css_wide_of_background_images :
  Properties.background_image list -> css_wide_keyword option
(** [css_wide_of_background_images v] is the CSS-wide keyword [v] is. *)

val css_wide_of_animations :
  Properties.animation list -> css_wide_keyword option
(** [css_wide_of_animations v] is the CSS-wide keyword [v] is. *)

val css_wide_of_transitions :
  Properties.transition list -> css_wide_keyword option
(** [css_wide_of_transitions v] is the CSS-wide keyword [v] is. *)

val declaration_with_importance :
  bool -> Declaration.declaration -> Declaration.declaration
(** [declaration_with_importance important d] is [d] with its [!important] flag
    set to [important], through any theme guard. *)

val property_name : 'a Properties.property -> string
(** [property_name p] is the name of [p] as printed. *)
