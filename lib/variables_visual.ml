open Properties
open Variables_intf
open Variables_value

let vars_of_border_spacing (value : Properties.border_spacing) : any_var list =
  match value with
  | Var v -> [ V v ]
  | (Lengths values : Properties.border_spacing) -> vars_of_length_list values

let vars_of_border_width (value : Properties.border_width) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

let vars_of_border_width_list values =
  List.concat_map vars_of_border_width values

let rec vars_of_shadow (value : Properties.shadow) : any_var list =
  match value with
  | Var v -> [ V v ]
  | List shadows -> List.concat_map vars_of_shadow shadows
  | Shadow { color; _ } -> (
      match color with Some c -> vars_of_color c | None -> [])
  | _ -> []

let vars_of_blend_mode (value : Properties.blend_mode) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_style (value : Properties.border_style) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_filter (value : Properties.filter) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background (value : Properties.background) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Vars vars -> List.map (fun v -> V v) vars
  | _ -> []

let vars_of_webkit_gradient_stop = function
  | Properties.Webkit_gradient.From color | Properties.Webkit_gradient.To color
    ->
      vars_of_color color
  | Properties.Webkit_gradient.Color_stop (position, color) ->
      vars_of_percentage position @ vars_of_color color

let vars_of_color_interpolation (value : Properties.color_interpolation) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_value (value : Properties.position_value) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Single l -> vars_of_length l
  | XY (l1, l2) -> vars_of_length l1 @ vars_of_length l2
  | Edge_offset_axis (_, lp, _) -> vars_of_length_percentage lp
  | Axis_edge_offset (_, _, offset) -> vars_of_length_percentage offset
  | Edge_offset_edge_offset (_, lp1, _, lp2) ->
      vars_of_length_percentage lp1 @ vars_of_length_percentage lp2
  | _ -> []

let rec vars_of_background_position_axis
    (value : Properties.background_position_axis) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Offset lp | Edge_offset (_, lp) -> vars_of_length_percentage lp
  (* A layer of the list holds a position of its own, so a reference inside one
     is a reference the caller has to see. *)
  | Layers positions ->
      List.concat_map vars_of_background_position_axis positions
  | _ -> []

let rec vars_of_gradient_direction (value : Properties.gradient_direction) :
    any_var list =
  match value with
  | Angle angle -> vars_of_angle angle
  | With_interpolation (direction, interpolation) ->
      vars_of_gradient_direction direction
      @ vars_of_color_interpolation interpolation
  | Var v -> [ V v ]
  | _ -> []

let vars_of_radial_shape (value : Properties.radial_shape) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_radial_size (value : Properties.radial_size) : any_var list =
  match value with
  | Circle_radius length -> vars_of_length length
  | Ellipse_radii (lp1, lp2) ->
      vars_of_length_percentage lp1 @ vars_of_length_percentage lp2
  | Var v -> [ V v ]
  | _ -> []

let vars_of_radial_gradient_config (value : Properties.radial_gradient_config) :
    any_var list =
  Option.fold ~none:[] ~some:vars_of_radial_shape value.shape
  @ Option.fold ~none:[] ~some:vars_of_radial_size value.size
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position
  @ Option.fold ~none:[] ~some:vars_of_color_interpolation value.interpolation

let vars_of_conic_gradient_config (value : Properties.conic_gradient_config) :
    any_var list =
  Option.fold ~none:[] ~some:vars_of_angle value.angle
  @ Option.fold ~none:[] ~some:vars_of_position_value value.position
  @ Option.fold ~none:[] ~some:vars_of_color_interpolation value.interpolation

let vars_of_gradient_position (value : Properties.gradient_position) :
    any_var list =
  match value with
  | Linear_position direction -> vars_of_gradient_direction direction
  | Radial_position config -> vars_of_radial_gradient_config config
  | Conic_position config -> vars_of_conic_gradient_config config
  | Var v -> [ V v ]

let rec vars_of_gradient_stop (value : Properties.gradient_stop) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Color_percentage (color, first, second) ->
      vars_of_color color
      @ Option.fold ~none:[] ~some:vars_of_length_percentage first
      @ Option.fold ~none:[] ~some:vars_of_length_percentage second
  | Color_length (color, first, second) ->
      vars_of_color color
      @ Option.fold ~none:[] ~some:vars_of_length first
      @ Option.fold ~none:[] ~some:vars_of_length second
  | Length length -> vars_of_length length
  | Channel channel -> vars_of_channel channel
  | Percentage percentage -> vars_of_percentage percentage
  | List stops -> List.concat_map vars_of_gradient_stop stops
  | Position position -> vars_of_gradient_position position
  | Direction direction -> vars_of_gradient_direction direction

let vars_of_background_image (value : Properties.background_image) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Linear_gradient (direction, stops)
  | Repeating_linear_gradient (direction, stops)
  | Webkit_linear_gradient (direction, stops)
  | Webkit_repeating_linear_gradient (direction, stops)
  | Moz_linear_gradient (direction, stops)
  | Moz_repeating_linear_gradient (direction, stops)
  | O_linear_gradient (direction, stops)
  | O_repeating_linear_gradient (direction, stops) ->
      vars_of_gradient_direction direction
      @ List.concat_map vars_of_gradient_stop stops
  | Radial_gradient (_, stops)
  | Repeating_radial_gradient (_, stops)
  | Webkit_radial_gradient (_, stops)
  | Webkit_repeating_radial_gradient (_, stops)
  | Moz_radial_gradient (_, stops)
  | Moz_repeating_radial_gradient (_, stops)
  | O_radial_gradient (_, stops)
  | O_repeating_radial_gradient (_, stops)
  | Conic_gradient (_, stops)
  | Repeating_conic_gradient (_, stops) ->
      List.concat_map vars_of_gradient_stop stops
  | Webkit_gradient (Webkit_gradient.Linear { stops; _ })
  | Webkit_gradient (Webkit_gradient.Radial { stops; _ }) ->
      List.concat_map vars_of_webkit_gradient_stop stops
  | _ -> []

let vars_of_background_size (value : Properties.background_size) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Length l -> vars_of_length l
  | Size (w, h) -> vars_of_length w @ vars_of_length h
  | _ -> []

let vars_of_border_image_repeat (value : Properties.border_image_repeat) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_image_width (value : Properties.border_image_width) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Widths values ->
      List.concat_map
        (fun (item : Properties.border_image_width_item) ->
          match item with
          | Length length -> vars_of_length length
          | Number n -> vars_of_number_value n
          | _ -> [])
        values
  | _ -> []

let vars_of_border_image_outset (value : Properties.border_image_outset) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Outsets values ->
      List.concat_map
        (fun (item : Properties.border_image_outset_item) ->
          match item with
          | Length length -> vars_of_length length
          | Number n -> vars_of_number_value n)
        values
  | _ -> []

let rec vars_of_cursor (value : Properties.cursor) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Url (_, _, fallback) -> vars_of_cursor fallback
  | _ -> []

let vars_of_caret_animation (value : Properties.caret_animation) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caret_shape (value : Properties.caret_shape) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caret (value : Properties.caret) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Caret (color, _, _) -> Option.fold ~none:[] ~some:vars_of_color color
  | _ -> []

let vars_of_outline_style (value : Properties.outline_style) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_opacity (value : Properties.opacity) : any_var list =
  match value with
  | Opacity_number _ -> []
  | Calc calc -> vars_of_calc calc
  | Var v -> [ V v ]
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_shape_image_threshold (value : Properties.shape_image_threshold) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc c -> vars_of_calc c
  | Number _ | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_scrollbar_color (value : Properties.scrollbar_color) =
  match value with
  | Var v -> [ V v ]
  | Colors (thumb, track) -> vars_of_color thumb @ vars_of_color track
  | Auto | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_image_orientation (value : Properties.image_orientation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_image_rendering (value : Properties.image_rendering) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_image_resolution (value : Properties.image_resolution) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_radius (value : Properties.border_radius) =
  let from_list = List.concat_map vars_of_length_percentage in
  match value with
  | Var v -> [ V v ]
  | Radius { horizontal; vertical } ->
      from_list horizontal
      @ Option.value ~default:[] (Option.map from_list vertical)
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_clip_path_extent (value : Properties.clip_path_extent) =
  match value with Extent_length l -> vars_of_length l | _ -> []

let rec vars_of_clip_path (value : Properties.clip_path) =
  match value with
  | Var v -> [ V v ]
  | Clip_path_inset { top; right; bottom; left; rounded } ->
      vars_of_length_percentage top
      @ Option.value ~default:[] (Option.map vars_of_length_percentage right)
      @ Option.value ~default:[] (Option.map vars_of_length_percentage bottom)
      @ Option.value ~default:[] (Option.map vars_of_length_percentage left)
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | Clip_path_circle { radius; _ } ->
      Option.value ~default:[] (Option.map vars_of_clip_path_extent radius)
  | Clip_path_ellipse { rx; ry; _ } ->
      Option.value ~default:[] (Option.map vars_of_clip_path_extent rx)
      @ Option.value ~default:[] (Option.map vars_of_clip_path_extent ry)
  | Clip_path_polygon { points; _ } ->
      List.concat_map (fun (a, b) -> vars_of_length a @ vars_of_length b) points
  | Clip_path_box _ -> []
  | Clip_path_with_box { shape; _ } -> vars_of_clip_path shape
  | Clip_path_xywh { x; y; width; height; rounded }
  | Clip_path_rect
      { top = x; right = y; bottom = width; left = height; rounded } ->
      vars_of_length_percentage x
      @ vars_of_length_percentage y
      @ vars_of_length_percentage width
      @ vars_of_length_percentage height
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | _ -> []

let vars_of_border (value : Properties.border) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { width; style; color } ->
      Option.value ~default:[] (Option.map vars_of_border_width width)
      @ Option.value ~default:[] (Option.map vars_of_border_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let vars_of_logical_border_color (value : Properties.logical_border_color) =
  match value with
  | Var v -> [ V v ]
  | Single color -> vars_of_color color
  | Pair (start_, end_) -> vars_of_color start_ @ vars_of_color end_
  | _ -> []

let vars_of_logical_border_width (value : Properties.logical_border_width) =
  match value with
  | Var v -> [ V v ]
  | Single w -> vars_of_border_width w
  | Pair (start_, end_) ->
      vars_of_border_width start_ @ vars_of_border_width end_
  | _ -> []

let vars_of_logical_border_style (value : Properties.logical_border_style) =
  match value with
  | Var v -> [ V v ]
  | Single s -> vars_of_border_style s
  | Pair (start_, end_) ->
      vars_of_border_style start_ @ vars_of_border_style end_
  | _ -> []

let vars_of_outline (value : Properties.outline) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { width; style; color } ->
      Option.value ~default:[] (Option.map vars_of_border_width width)
      @ Option.value ~default:[] (Option.map vars_of_outline_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let rec vars_of_background_attachment (value : Properties.background_attachment)
    =
  match value with
  | Var v -> [ V v ]
  | Layers layers -> List.concat_map vars_of_background_attachment layers
  | Scroll | Fixed | Local | Initial | Inherit | Unset | Revert | Revert_layer
    ->
      []

let vars_of_background_box (value : Properties.background_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_background_repeat (value : Properties.background_repeat) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_border_collapse (value : Properties.border_collapse) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_appearance (value : Properties.appearance) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_appearance (value : Properties.webkit_appearance) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_color_scheme (value : Properties.color_scheme) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_print_color_adjust (value : Properties.print_color_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_box_decoration_break (value : Properties.box_decoration_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_forced_color_adjust (value : Properties.forced_color_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_object_fit (value : Properties.object_fit) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_svg_paint (value : Properties.svg_paint) =
  match value with
  | Var v -> [ V v ]
  | Color c -> vars_of_color c
  | Url (_, fallback) ->
      Option.value ~default:[] (Option.map vars_of_svg_paint fallback)
  | _ -> []

let vars_of_clip (value : Properties.clip) =
  match value with
  | Var v -> [ V v ]
  | Clip_rect (a, b, c, d) ->
      vars_of_length a @ vars_of_length b @ vars_of_length c @ vars_of_length d
  | _ -> []

let vars_of_object_view_box (value : Properties.object_view_box) =
  match value with
  | Var v -> [ V v ]
  | Inset (a, b, c, d) ->
      vars_of_length a
      @ Option.value ~default:[] (Option.map vars_of_length b)
      @ Option.value ~default:[] (Option.map vars_of_length c)
      @ Option.value ~default:[] (Option.map vars_of_length d)
  | Xywh { x; y; width; height; rounded }
  | Rect { top = x; right = y; bottom = width; left = height; rounded } ->
      vars_of_length_percentage x
      @ vars_of_length_percentage y
      @ vars_of_length_percentage width
      @ vars_of_length_percentage height
      @ Option.value ~default:[] (Option.map vars_of_border_radius rounded)
  | None | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_mask_box (value : Properties.mask_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_mask_box (value : Properties.webkit_mask_box) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_webkit_mask_composite (value : Properties.webkit_mask_composite)
    =
  match value with
  | Var v -> [ V v ]
  | Composites composites ->
      List.concat_map vars_of_webkit_mask_composite composites
  | _ -> []

let vars_of_mask_composite (value : Properties.mask_composite) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_mask_source_type (value : Properties.webkit_mask_source_type) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_mask_mode (value : Properties.mask_mode) =
  match value with
  | Var v -> [ V v ]
  | Modes modes -> List.concat_map vars_of_mask_mode modes
  | Alpha | Luminance | Match_source | Initial | Inherit | Unset | Revert
  | Revert_layer ->
      []

let vars_of_mask_layer (layer : Properties.mask_layer) : any_var list =
  Option.fold ~none:[] ~some:vars_of_background_image layer.image
  @ Option.fold ~none:[] ~some:vars_of_position_value layer.position
  @ Option.fold ~none:[] ~some:vars_of_background_size layer.size
  @ Option.fold ~none:[] ~some:vars_of_background_repeat layer.repeat
  @ Option.fold ~none:[] ~some:vars_of_mask_box layer.origin
  @ Option.fold ~none:[] ~some:vars_of_mask_box layer.clip
  @ Option.fold ~none:[] ~some:vars_of_mask_mode layer.mode
  @ Option.fold ~none:[] ~some:vars_of_mask_composite layer.composite

let vars_of_mask (value : Properties.mask) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Layer layer -> vars_of_mask_layer layer
  | Layers layers -> List.concat_map vars_of_mask_layer layers
  | _ -> []

let vars_of_border_image (value : Properties.border_image) : any_var list =
  Option.fold ~none:[] ~some:vars_of_background_image value.source

let vars_of_fill_rule (value : Properties.fill_rule) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_linecap (value : Properties.stroke_linecap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_linejoin (value : Properties.stroke_linejoin) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_miterlimit (value : Properties.stroke_miterlimit) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_stroke_width (value : Properties.stroke_width) =
  match value with
  | Var v -> [ V v ]
  | Length lp -> vars_of_length_percentage lp
  | _ -> []

let vars_of_dash_length (value : Properties.dash_length) =
  match value with
  | Number n -> vars_of_number_value n
  | Length lp -> vars_of_length_percentage lp

let vars_of_stroke_dashoffset (value : Properties.stroke_dashoffset) =
  match value with
  | Var v -> [ V v ]
  | Dash d -> vars_of_dash_length d
  | _ -> []

let vars_of_stroke_dasharray (value : Properties.stroke_dasharray) =
  match value with
  | Var v -> [ V v ]
  | Dashes ds -> List.concat_map vars_of_dash_length ds
  | _ -> []

let vars_of_paint_order (value : Properties.paint_order) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_vector_effect (value : Properties.vector_effect) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_mask_type (value : Properties.mask_type) =
  match value with Var v -> [ V v ] | _ -> []
