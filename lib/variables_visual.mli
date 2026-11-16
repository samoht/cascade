(** Variable extraction from the painting property values: backgrounds,
    gradients, borders and border images, shadows, masks, clip paths, SVG paint
    and stroke, cursor and caret. *)

val vars_of_border_spacing :
  Properties.border_spacing -> Variables_intf.any_var list
(** [vars_of_border_spacing v] is the variables [v] references. *)

val vars_of_border_width :
  Properties.border_width -> Variables_intf.any_var list
(** [vars_of_border_width v] is the variables [v] references. *)

val vars_of_border_width_list :
  Properties.border_width list -> Variables_intf.any_var list
(** [vars_of_border_width_list v] is the variables [v] references. *)

val vars_of_shadow : Properties.shadow -> Variables_intf.any_var list
(** [vars_of_shadow v] is the variables [v] references. *)

val vars_of_blend_mode : Properties.blend_mode -> Variables_intf.any_var list
(** [vars_of_blend_mode v] is the variables [v] references. *)

val vars_of_border_style :
  Properties.border_style -> Variables_intf.any_var list
(** [vars_of_border_style v] is the variables [v] references. *)

val vars_of_filter : Properties.filter -> Variables_intf.any_var list
(** [vars_of_filter v] is the variables [v] references. *)

val vars_of_background : Properties.background -> Variables_intf.any_var list
(** [vars_of_background v] is the variables [v] references. *)

val vars_of_position_value :
  Properties.position_value -> Variables_intf.any_var list
(** [vars_of_position_value v] is the variables [v] references. *)

val vars_of_background_position_axis :
  Properties.background_position_axis -> Variables_intf.any_var list
(** [vars_of_background_position_axis v] is the variables [v] references. *)

val vars_of_gradient_direction :
  Properties.gradient_direction -> Variables_intf.any_var list
(** [vars_of_gradient_direction v] is the variables [v] references. *)

val vars_of_radial_shape :
  Properties.radial_shape -> Variables_intf.any_var list
(** [vars_of_radial_shape v] is the variables [v] references. *)

val vars_of_radial_size : Properties.radial_size -> Variables_intf.any_var list
(** [vars_of_radial_size v] is the variables [v] references. *)

val vars_of_gradient_position :
  Properties.gradient_position -> Variables_intf.any_var list
(** [vars_of_gradient_position v] is the variables [v] references. *)

val vars_of_gradient_stop :
  Properties.gradient_stop -> Variables_intf.any_var list
(** [vars_of_gradient_stop v] is the variables [v] references. *)

val vars_of_background_image :
  Properties.background_image -> Variables_intf.any_var list
(** [vars_of_background_image v] is the variables [v] references. *)

val vars_of_background_size :
  Properties.background_size -> Variables_intf.any_var list
(** [vars_of_background_size v] is the variables [v] references. *)

val vars_of_border_image_repeat :
  Properties.border_image_repeat -> Variables_intf.any_var list
(** [vars_of_border_image_repeat v] is the variables [v] references. *)

val vars_of_border_image_width :
  Properties.border_image_width -> Variables_intf.any_var list
(** [vars_of_border_image_width v] is the variables [v] references. *)

val vars_of_border_image_outset :
  Properties.border_image_outset -> Variables_intf.any_var list
(** [vars_of_border_image_outset v] is the variables [v] references. *)

val vars_of_cursor : Properties.cursor -> Variables_intf.any_var list
(** [vars_of_cursor v] is the variables [v] references. *)

val vars_of_caret_animation :
  Properties.caret_animation -> Variables_intf.any_var list
(** [vars_of_caret_animation v] is the variables [v] references. *)

val vars_of_caret_shape : Properties.caret_shape -> Variables_intf.any_var list
(** [vars_of_caret_shape v] is the variables [v] references. *)

val vars_of_caret : Properties.caret -> Variables_intf.any_var list
(** [vars_of_caret v] is the variables [v] references. *)

val vars_of_outline_style :
  Properties.outline_style -> Variables_intf.any_var list
(** [vars_of_outline_style v] is the variables [v] references. *)

val vars_of_opacity : Properties.opacity -> Variables_intf.any_var list
(** [vars_of_opacity v] is the variables [v] references. *)

val vars_of_shape_image_threshold :
  Properties.shape_image_threshold -> Variables_intf.any_var list
(** [vars_of_shape_image_threshold v] is the variables [v] references. *)

val vars_of_scrollbar_color :
  Properties.scrollbar_color -> Variables_intf.any_var list
(** [vars_of_scrollbar_color v] is the variables [v] references. *)

val vars_of_image_orientation :
  Properties.image_orientation -> Variables_intf.any_var list
(** [vars_of_image_orientation v] is the variables [v] references. *)

val vars_of_image_rendering :
  Properties.image_rendering -> Variables_intf.any_var list
(** [vars_of_image_rendering v] is the variables [v] references. *)

val vars_of_image_resolution :
  Properties.image_resolution -> Variables_intf.any_var list
(** [vars_of_image_resolution v] is the variables [v] references. *)

val vars_of_border_radius :
  Properties.border_radius -> Variables_intf.any_var list
(** [vars_of_border_radius v] is the variables [v] references. *)

val vars_of_clip_path : Properties.clip_path -> Variables_intf.any_var list
(** [vars_of_clip_path v] is the variables [v] references. *)

val vars_of_border : Properties.border -> Variables_intf.any_var list
(** [vars_of_border v] is the variables [v] references. *)

val vars_of_logical_border_color :
  Properties.logical_border_color -> Variables_intf.any_var list
(** [vars_of_logical_border_color v] is the variables [v] references. *)

val vars_of_logical_border_width :
  Properties.logical_border_width -> Variables_intf.any_var list
(** [vars_of_logical_border_width v] is the variables [v] references. *)

val vars_of_logical_border_style :
  Properties.logical_border_style -> Variables_intf.any_var list
(** [vars_of_logical_border_style v] is the variables [v] references. *)

val vars_of_outline : Properties.outline -> Variables_intf.any_var list
(** [vars_of_outline v] is the variables [v] references. *)

val vars_of_background_attachment :
  Properties.background_attachment -> Variables_intf.any_var list
(** [vars_of_background_attachment v] is the variables [v] references. *)

val vars_of_background_box :
  Properties.background_box -> Variables_intf.any_var list
(** [vars_of_background_box v] is the variables [v] references. *)

val vars_of_background_repeat :
  Properties.background_repeat -> Variables_intf.any_var list
(** [vars_of_background_repeat v] is the variables [v] references. *)

val vars_of_border_collapse :
  Properties.border_collapse -> Variables_intf.any_var list
(** [vars_of_border_collapse v] is the variables [v] references. *)

val vars_of_appearance : Properties.appearance -> Variables_intf.any_var list
(** [vars_of_appearance v] is the variables [v] references. *)

val vars_of_webkit_appearance :
  Properties.webkit_appearance -> Variables_intf.any_var list
(** [vars_of_webkit_appearance v] is the variables [v] references. *)

val vars_of_color_scheme :
  Properties.color_scheme -> Variables_intf.any_var list
(** [vars_of_color_scheme v] is the variables [v] references. *)

val vars_of_print_color_adjust :
  Properties.print_color_adjust -> Variables_intf.any_var list
(** [vars_of_print_color_adjust v] is the variables [v] references. *)

val vars_of_box_decoration_break :
  Properties.box_decoration_break -> Variables_intf.any_var list
(** [vars_of_box_decoration_break v] is the variables [v] references. *)

val vars_of_forced_color_adjust :
  Properties.forced_color_adjust -> Variables_intf.any_var list
(** [vars_of_forced_color_adjust v] is the variables [v] references. *)

val vars_of_object_fit : Properties.object_fit -> Variables_intf.any_var list
(** [vars_of_object_fit v] is the variables [v] references. *)

val vars_of_svg_paint : Properties.svg_paint -> Variables_intf.any_var list
(** [vars_of_svg_paint v] is the variables [v] references. *)

val vars_of_clip : Properties.clip -> Variables_intf.any_var list
(** [vars_of_clip v] is the variables [v] references. *)

val vars_of_object_view_box :
  Properties.object_view_box -> Variables_intf.any_var list
(** [vars_of_object_view_box v] is the variables [v] references. *)

val vars_of_mask_box : Properties.mask_box -> Variables_intf.any_var list
(** [vars_of_mask_box v] is the variables [v] references. *)

val vars_of_webkit_mask_box :
  Properties.webkit_mask_box -> Variables_intf.any_var list
(** [vars_of_webkit_mask_box v] is the variables [v] references. *)

val vars_of_webkit_mask_composite :
  Properties.webkit_mask_composite -> Variables_intf.any_var list
(** [vars_of_webkit_mask_composite v] is the variables [v] references. *)

val vars_of_mask_composite :
  Properties.mask_composite -> Variables_intf.any_var list
(** [vars_of_mask_composite v] is the variables [v] references. *)

val vars_of_mask_source_type :
  Properties.webkit_mask_source_type -> Variables_intf.any_var list
(** [vars_of_mask_source_type v] is the variables [v] references. *)

val vars_of_mask_mode : Properties.mask_mode -> Variables_intf.any_var list
(** [vars_of_mask_mode v] is the variables [v] references. *)

val vars_of_mask : Properties.mask -> Variables_intf.any_var list
(** [vars_of_mask v] is the variables [v] references. *)

val vars_of_border_image :
  Properties.border_image -> Variables_intf.any_var list
(** [vars_of_border_image v] is the variables [v] references. *)

val vars_of_fill_rule : Properties.fill_rule -> Variables_intf.any_var list
(** [vars_of_fill_rule v] is the variables [v] references. *)

val vars_of_stroke_linecap :
  Properties.stroke_linecap -> Variables_intf.any_var list
(** [vars_of_stroke_linecap v] is the variables [v] references. *)

val vars_of_stroke_linejoin :
  Properties.stroke_linejoin -> Variables_intf.any_var list
(** [vars_of_stroke_linejoin v] is the variables [v] references. *)

val vars_of_stroke_miterlimit :
  Properties.stroke_miterlimit -> Variables_intf.any_var list
(** [vars_of_stroke_miterlimit v] is the variables [v] references. *)

val vars_of_stroke_width :
  Properties.stroke_width -> Variables_intf.any_var list
(** [vars_of_stroke_width v] is the variables [v] references. *)

val vars_of_stroke_dashoffset :
  Properties.stroke_dashoffset -> Variables_intf.any_var list
(** [vars_of_stroke_dashoffset v] is the variables [v] references. *)

val vars_of_stroke_dasharray :
  Properties.stroke_dasharray -> Variables_intf.any_var list
(** [vars_of_stroke_dasharray v] is the variables [v] references. *)

val vars_of_paint_order : Properties.paint_order -> Variables_intf.any_var list
(** [vars_of_paint_order v] is the variables [v] references. *)

val vars_of_vector_effect :
  Properties.vector_effect -> Variables_intf.any_var list
(** [vars_of_vector_effect v] is the variables [v] references. *)

val vars_of_mask_type : Properties.mask_type -> Variables_intf.any_var list
(** [vars_of_mask_type v] is the variables [v] references. *)
