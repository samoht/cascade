open Properties
open Variables_intf
open Variables_value

let vars_of_content_visibility (value : Properties.content_visibility) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_strictness (value : Properties.scroll_snap_strictness) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_axis (value : Properties.scroll_snap_axis) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_snap_type (value : Properties.scroll_snap_type) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Axis axis -> vars_of_scroll_snap_axis axis
  | Axis_with_strictness (axis, strictness) ->
      vars_of_scroll_snap_axis axis @ vars_of_scroll_snap_strictness strictness
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_aspect_ratio (value : Properties.aspect_ratio) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Auto_ratio_calc (width, height) | Ratio_calc (width, height) ->
      vars_of_number_value width @ vars_of_number_value height
  | _ -> []

let vars_of_columns_value (value : Properties.columns_value) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Width l -> vars_of_length l
  | Both (l, _) -> vars_of_length l
  | _ -> []

let vars_of_column_width (value : Properties.column_width) : any_var list =
  match value with Var v -> [ V v ] | Width l -> vars_of_length l | _ -> []

let vars_of_column_height (value : Properties.column_height) : any_var list =
  match value with Var v -> [ V v ] | Height l -> vars_of_length l | _ -> []

let vars_of_column_wrap (value : Properties.column_wrap) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_column_count (value : Properties.column_count) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_column_span (value : Properties.column_span) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_contain (value : Properties.contain) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_interactivity (value : Properties.interactivity) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_interest_delay (value : Properties.interest_delay) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_nav (value : Properties.nav) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_grid_template (value : Properties.grid_template) : any_var list
    =
  match value with
  | Var v -> [ V v ]
  | Fit_content l -> vars_of_length l
  | Min_max (a, b) -> vars_of_grid_template a @ vars_of_grid_template b
  | Repeat (_, ts) -> List.concat_map vars_of_grid_template ts
  | Tracks ts -> List.concat_map vars_of_grid_template ts
  | Split (rows, columns) ->
      vars_of_grid_template rows @ vars_of_grid_template columns
  | Auto_flow_columns (rows, _, auto_columns) ->
      vars_of_grid_template rows
      @ Option.fold ~none:[] ~some:vars_of_grid_template auto_columns
  | Auto_flow_rows (_, auto_rows, columns) ->
      Option.fold ~none:[] ~some:vars_of_grid_template auto_rows
      @ vars_of_grid_template columns
  | Named_tracks ts ->
      List.concat_map (fun (_, t) -> vars_of_grid_template t) ts
  | _ -> []

let vars_of_grid_line (value : Properties.grid_line) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_line_pair (value : Properties.grid_line_pair) : any_var list =
  match value with
  | Var v -> [ V v ]
  | (Lines (start, end_) : Properties.grid_line_pair) ->
      vars_of_grid_line start @ vars_of_grid_line end_

let vars_of_overflow_clip_margin (value : Properties.overflow_clip_margin) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Clip_margin (_, Some length) -> vars_of_length length
  | Clip_margin (_, None) | Initial | Inherit | Unset | Revert | Revert_layer ->
      []

let vars_of_zoom (value : Properties.zoom) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Calc c -> vars_of_calc c
  | Normal | Reset | Num _ | Pct _ | Initial | Inherit | Unset | Revert
  | Revert_layer ->
      []

let vars_of_display (value : Properties.display) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position (value : Properties.position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow (value : Properties.overflow) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_direction (value : Properties.flex_direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_wrap (value : Properties.flex_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_flow (value : Properties.flex_flow) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_factor (value : Properties.flex_factor) =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_align_content (value : Properties.align_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_items (value : Properties.align_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_align_self (value : Properties.align_self) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_content (value : Properties.justify_content) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_items (value : Properties.justify_items) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_justify_self (value : Properties.justify_self) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_place_content (value : Properties.place_content) =
  match value with
  | Var v -> [ V v ]
  | Align_justify (align, justify) ->
      vars_of_align_content align @ vars_of_justify_content justify
  | _ -> []

let vars_of_place_items (value : Properties.place_items) =
  match value with
  | Var v -> [ V v ]
  | Align_justify (align, justify) ->
      vars_of_align_items align @ vars_of_justify_items justify
  | _ -> []

let vars_of_grid_flow_component (value : Properties.grid_auto_flow_component) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_auto_flow (value : Properties.grid_auto_flow) =
  match value with
  | Var v -> [ V v ]
  | Components components ->
      List.concat_map vars_of_grid_flow_component components
  | _ -> []

let vars_of_container_name (value : Properties.container_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_anchor_name (value : Properties.anchor_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_anchor (value : Properties.position_anchor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try_fallbacks (value : Properties.position_try_fallbacks) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try_order (value : Properties.position_try_order) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_try (value : Properties.position_try) =
  match value with
  | Var v -> [ V v ]
  | Try (order, fallbacks) ->
      vars_of_position_try_order order
      @ vars_of_position_try_fallbacks fallbacks
  | _ -> []

let vars_of_position_visibility (value : Properties.position_visibility) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_position_area (value : Properties.position_area) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow_anchor (value : Properties.overflow_anchor) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scrollbar_width (value : Properties.scrollbar_width) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scrollbar_gutter (value : Properties.scrollbar_gutter) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_intrinsic_size_item (value : Properties.contain_intrinsic_size_item)
    =
  match value with
  | Length len | Auto len -> vars_of_length len
  | None | Auto_none -> []

let vars_of_contain_intrinsic_size (value : Properties.contain_intrinsic_size) =
  match value with
  | Var v -> [ V v ]
  | Intrinsic (first, second) ->
      vars_of_intrinsic_size_item first
      @ Option.fold ~none:[] ~some:vars_of_intrinsic_size_item second
  | None | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_contain_intrinsic_longhand
    (value : Properties.contain_intrinsic_longhand) =
  match value with
  | Var v -> [ V v ]
  | Size size -> vars_of_intrinsic_size_item size
  | None | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_margin_trim (value : Properties.margin_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_flex_basis (value : Properties.flex_basis) =
  match value with Var v -> [ V v ] | Calc c -> vars_of_calc c | _ -> []

let vars_of_flex (value : Properties.flex) =
  match value with
  | Var v -> [ V v ]
  | Grow g -> vars_of_flex_factor g
  | Grow_shrink (g, s) -> vars_of_flex_factor g @ vars_of_flex_factor s
  | Full (g, s, b) ->
      vars_of_flex_factor g @ vars_of_flex_factor s @ vars_of_flex_basis b
  | Basis b -> vars_of_flex_basis b
  | _ -> []

let vars_of_visibility (value : Properties.visibility) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_interpolate_size (value : Properties.interpolate_size) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_min_intrinsic_sizing (value : Properties.min_intrinsic_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_box_sizing (value : Properties.box_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overlay (value : Properties.overlay) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_clear (value : Properties.clear) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_float_side (value : Properties.float_side) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_pointer_events (value : Properties.pointer_events) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_resize (value : Properties.resize) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_type (value : Properties.container_type) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_container_shorthand (value : Properties.container_shorthand) =
  match value with
  | Var v -> [ V v ]
  | Shorthand { ctype = Some ctype; _ } -> vars_of_container_type ctype
  | _ -> []

let vars_of_break_value (value : Properties.break_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_break_inside_value (value : Properties.break_inside_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_break_value (value : Properties.page_break_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_break_inside (value : Properties.page_break_inside_value) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size_name (value : Properties.page_size_name) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size_orientation (value : Properties.page_size_orientation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_page_size (value : Properties.page_size) =
  match value with
  | Var v -> [ V v ]
  | Single l -> vars_of_length l
  | Pair (a, b) -> vars_of_length a @ vars_of_length b
  | Named name -> vars_of_page_size_name name
  | Named_oriented (name, orientation) ->
      vars_of_page_size_name name @ vars_of_page_size_orientation orientation
  | Oriented orientation -> vars_of_page_size_orientation orientation
  | _ -> []

let rec vars_of_scroll_snap_align (value : Properties.scroll_snap_align) =
  match value with
  | Var v -> [ V v ]
  | Snap_align_pair (a, b) ->
      vars_of_scroll_snap_align a @ vars_of_scroll_snap_align b
  | _ -> []

let vars_of_scroll_snap_stop (value : Properties.scroll_snap_stop) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overscroll_behavior (value : Properties.overscroll_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_box_orient (value : Properties.webkit_box_orient) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_moz_orient (value : Properties.moz_orient) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_touch_action (value : Properties.touch_action) =
  match value with
  | Var v -> [ V v ]
  | Vars vars -> List.map (fun v -> V v) vars
  | _ -> []

let vars_of_user_select (value : Properties.user_select) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_scroll_behavior (value : Properties.scroll_behavior) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_field_sizing (value : Properties.field_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_template_areas (value : Properties.grid_template_areas) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_isolation (value : Properties.isolation) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_order (value : Properties.order) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_table_layout (value : Properties.table_layout) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_line_clamp (value : Properties.webkit_line_clamp) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_z_index (value : Properties.z_index) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_grid_area (value : Properties.grid_area) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Lines { row_start; column_start; row_end; column_end } ->
      vars_of_grid_line row_start
      @ vars_of_grid_line column_start
      @ vars_of_grid_line row_end
      @ vars_of_grid_line column_end
  | _ -> []
