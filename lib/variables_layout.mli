(** Variable extraction from the box, flow and layout property values: display,
    flex, grid, alignment, multi-column, positioning and anchoring, containment,
    overflow, scroll snap, paging and user interaction. *)

val vars_of_content_visibility :
  Properties.content_visibility -> Variables_intf.any_var list
(** [vars_of_content_visibility v] is the variables [v] references. *)

val vars_of_scroll_snap_strictness :
  Properties.scroll_snap_strictness -> Variables_intf.any_var list
(** [vars_of_scroll_snap_strictness v] is the variables [v] references. *)

val vars_of_scroll_snap_type :
  Properties.scroll_snap_type -> Variables_intf.any_var list
(** [vars_of_scroll_snap_type v] is the variables [v] references. *)

val vars_of_aspect_ratio :
  Properties.aspect_ratio -> Variables_intf.any_var list
(** [vars_of_aspect_ratio v] is the variables [v] references. *)

val vars_of_columns_value :
  Properties.columns_value -> Variables_intf.any_var list
(** [vars_of_columns_value v] is the variables [v] references. *)

val vars_of_column_width :
  Properties.column_width -> Variables_intf.any_var list
(** [vars_of_column_width v] is the variables [v] references. *)

val vars_of_column_height :
  Properties.column_height -> Variables_intf.any_var list
(** [vars_of_column_height v] is the variables [v] references. *)

val vars_of_column_wrap : Properties.column_wrap -> Variables_intf.any_var list
(** [vars_of_column_wrap v] is the variables [v] references. *)

val vars_of_column_count :
  Properties.column_count -> Variables_intf.any_var list
(** [vars_of_column_count v] is the variables [v] references. *)

val vars_of_column_span : Properties.column_span -> Variables_intf.any_var list
(** [vars_of_column_span v] is the variables [v] references. *)

val vars_of_contain : Properties.contain -> Variables_intf.any_var list
(** [vars_of_contain v] is the variables [v] references. *)

val vars_of_interactivity :
  Properties.interactivity -> Variables_intf.any_var list
(** [vars_of_interactivity v] is the variables [v] references. *)

val vars_of_interest_delay :
  Properties.interest_delay -> Variables_intf.any_var list
(** [vars_of_interest_delay v] is the variables [v] references. *)

val vars_of_nav : Properties.nav -> Variables_intf.any_var list
(** [vars_of_nav v] is the variables [v] references. *)

val vars_of_grid_template :
  Properties.grid_template -> Variables_intf.any_var list
(** [vars_of_grid_template v] is the variables [v] references. *)

val vars_of_grid_line : Properties.grid_line -> Variables_intf.any_var list
(** [vars_of_grid_line v] is the variables [v] references. *)

val vars_of_grid_line_pair :
  Properties.grid_line_pair -> Variables_intf.any_var list
(** [vars_of_grid_line_pair v] is the variables [v] references. *)

val vars_of_overflow_clip_margin :
  Properties.overflow_clip_margin -> Variables_intf.any_var list
(** [vars_of_overflow_clip_margin v] is the variables [v] references. *)

val vars_of_zoom : Properties.zoom -> Variables_intf.any_var list
(** [vars_of_zoom v] is the variables [v] references. *)

val vars_of_display : Properties.display -> Variables_intf.any_var list
(** [vars_of_display v] is the variables [v] references. *)

val vars_of_position : Properties.position -> Variables_intf.any_var list
(** [vars_of_position v] is the variables [v] references. *)

val vars_of_overflow : Properties.overflow -> Variables_intf.any_var list
(** [vars_of_overflow v] is the variables [v] references. *)

val vars_of_flex_direction :
  Properties.flex_direction -> Variables_intf.any_var list
(** [vars_of_flex_direction v] is the variables [v] references. *)

val vars_of_flex_wrap : Properties.flex_wrap -> Variables_intf.any_var list
(** [vars_of_flex_wrap v] is the variables [v] references. *)

val vars_of_flex_flow : Properties.flex_flow -> Variables_intf.any_var list
(** [vars_of_flex_flow v] is the variables [v] references. *)

val vars_of_flex_factor : Properties.flex_factor -> Variables_intf.any_var list
(** [vars_of_flex_factor v] is the variables [v] references. *)

val vars_of_align_content :
  Properties.align_content -> Variables_intf.any_var list
(** [vars_of_align_content v] is the variables [v] references. *)

val vars_of_align_items : Properties.align_items -> Variables_intf.any_var list
(** [vars_of_align_items v] is the variables [v] references. *)

val vars_of_align_self : Properties.align_self -> Variables_intf.any_var list
(** [vars_of_align_self v] is the variables [v] references. *)

val vars_of_justify_content :
  Properties.justify_content -> Variables_intf.any_var list
(** [vars_of_justify_content v] is the variables [v] references. *)

val vars_of_justify_items :
  Properties.justify_items -> Variables_intf.any_var list
(** [vars_of_justify_items v] is the variables [v] references. *)

val vars_of_justify_self :
  Properties.justify_self -> Variables_intf.any_var list
(** [vars_of_justify_self v] is the variables [v] references. *)

val vars_of_place_content :
  Properties.place_content -> Variables_intf.any_var list
(** [vars_of_place_content v] is the variables [v] references. *)

val vars_of_place_items : Properties.place_items -> Variables_intf.any_var list
(** [vars_of_place_items v] is the variables [v] references. *)

val vars_of_grid_auto_flow :
  Properties.grid_auto_flow -> Variables_intf.any_var list
(** [vars_of_grid_auto_flow v] is the variables [v] references. *)

val vars_of_container_name :
  Properties.container_name -> Variables_intf.any_var list
(** [vars_of_container_name v] is the variables [v] references. *)

val vars_of_anchor_name : Properties.anchor_name -> Variables_intf.any_var list
(** [vars_of_anchor_name v] is the variables [v] references. *)

val vars_of_position_anchor :
  Properties.position_anchor -> Variables_intf.any_var list
(** [vars_of_position_anchor v] is the variables [v] references. *)

val vars_of_position_try_fallbacks :
  Properties.position_try_fallbacks -> Variables_intf.any_var list
(** [vars_of_position_try_fallbacks v] is the variables [v] references. *)

val vars_of_position_try_order :
  Properties.position_try_order -> Variables_intf.any_var list
(** [vars_of_position_try_order v] is the variables [v] references. *)

val vars_of_position_try :
  Properties.position_try -> Variables_intf.any_var list
(** [vars_of_position_try v] is the variables [v] references. *)

val vars_of_position_visibility :
  Properties.position_visibility -> Variables_intf.any_var list
(** [vars_of_position_visibility v] is the variables [v] references. *)

val vars_of_position_area :
  Properties.position_area -> Variables_intf.any_var list
(** [vars_of_position_area v] is the variables [v] references. *)

val vars_of_overflow_anchor :
  Properties.overflow_anchor -> Variables_intf.any_var list
(** [vars_of_overflow_anchor v] is the variables [v] references. *)

val vars_of_scrollbar_width :
  Properties.scrollbar_width -> Variables_intf.any_var list
(** [vars_of_scrollbar_width v] is the variables [v] references. *)

val vars_of_scrollbar_gutter :
  Properties.scrollbar_gutter -> Variables_intf.any_var list
(** [vars_of_scrollbar_gutter v] is the variables [v] references. *)

val vars_of_contain_intrinsic_size :
  Properties.contain_intrinsic_size -> Variables_intf.any_var list
(** [vars_of_contain_intrinsic_size v] is the variables [v] references. *)

val vars_of_contain_intrinsic_longhand :
  Properties.contain_intrinsic_longhand -> Variables_intf.any_var list
(** [vars_of_contain_intrinsic_longhand v] is the variables [v] references. *)

val vars_of_margin_trim : Properties.margin_trim -> Variables_intf.any_var list
(** [vars_of_margin_trim v] is the variables [v] references. *)

val vars_of_flex_basis : Properties.flex_basis -> Variables_intf.any_var list
(** [vars_of_flex_basis v] is the variables [v] references. *)

val vars_of_flex : Properties.flex -> Variables_intf.any_var list
(** [vars_of_flex v] is the variables [v] references. *)

val vars_of_visibility : Properties.visibility -> Variables_intf.any_var list
(** [vars_of_visibility v] is the variables [v] references. *)

val vars_of_interpolate_size :
  Properties.interpolate_size -> Variables_intf.any_var list
(** [vars_of_interpolate_size v] is the variables [v] references. *)

val vars_of_min_intrinsic_sizing :
  Properties.min_intrinsic_sizing -> Variables_intf.any_var list
(** [vars_of_min_intrinsic_sizing v] is the variables [v] references. *)

val vars_of_box_sizing : Properties.box_sizing -> Variables_intf.any_var list
(** [vars_of_box_sizing v] is the variables [v] references. *)

val vars_of_overlay : Properties.overlay -> Variables_intf.any_var list
(** [vars_of_overlay v] is the variables [v] references. *)

val vars_of_clear : Properties.clear -> Variables_intf.any_var list
(** [vars_of_clear v] is the variables [v] references. *)

val vars_of_float_side : Properties.float_side -> Variables_intf.any_var list
(** [vars_of_float_side v] is the variables [v] references. *)

val vars_of_pointer_events :
  Properties.pointer_events -> Variables_intf.any_var list
(** [vars_of_pointer_events v] is the variables [v] references. *)

val vars_of_resize : Properties.resize -> Variables_intf.any_var list
(** [vars_of_resize v] is the variables [v] references. *)

val vars_of_container_type :
  Properties.container_type -> Variables_intf.any_var list
(** [vars_of_container_type v] is the variables [v] references. *)

val vars_of_container_shorthand :
  Properties.container_shorthand -> Variables_intf.any_var list
(** [vars_of_container_shorthand v] is the variables [v] references. *)

val vars_of_break_value : Properties.break_value -> Variables_intf.any_var list
(** [vars_of_break_value v] is the variables [v] references. *)

val vars_of_break_inside_value :
  Properties.break_inside_value -> Variables_intf.any_var list
(** [vars_of_break_inside_value v] is the variables [v] references. *)

val vars_of_page_break_value :
  Properties.page_break_value -> Variables_intf.any_var list
(** [vars_of_page_break_value v] is the variables [v] references. *)

val vars_of_page_break_inside :
  Properties.page_break_inside_value -> Variables_intf.any_var list
(** [vars_of_page_break_inside v] is the variables [v] references. *)

val vars_of_page_size : Properties.page_size -> Variables_intf.any_var list
(** [vars_of_page_size v] is the variables [v] references. *)

val vars_of_scroll_snap_align :
  Properties.scroll_snap_align -> Variables_intf.any_var list
(** [vars_of_scroll_snap_align v] is the variables [v] references. *)

val vars_of_scroll_snap_stop :
  Properties.scroll_snap_stop -> Variables_intf.any_var list
(** [vars_of_scroll_snap_stop v] is the variables [v] references. *)

val vars_of_overscroll_behavior :
  Properties.overscroll_behavior -> Variables_intf.any_var list
(** [vars_of_overscroll_behavior v] is the variables [v] references. *)

val vars_of_webkit_box_orient :
  Properties.webkit_box_orient -> Variables_intf.any_var list
(** [vars_of_webkit_box_orient v] is the variables [v] references. *)

val vars_of_moz_orient : Properties.moz_orient -> Variables_intf.any_var list
(** [vars_of_moz_orient v] is the variables [v] references. *)

val vars_of_touch_action :
  Properties.touch_action -> Variables_intf.any_var list
(** [vars_of_touch_action v] is the variables [v] references. *)

val vars_of_user_select : Properties.user_select -> Variables_intf.any_var list
(** [vars_of_user_select v] is the variables [v] references. *)

val vars_of_scroll_behavior :
  Properties.scroll_behavior -> Variables_intf.any_var list
(** [vars_of_scroll_behavior v] is the variables [v] references. *)

val vars_of_field_sizing :
  Properties.field_sizing -> Variables_intf.any_var list
(** [vars_of_field_sizing v] is the variables [v] references. *)

val vars_of_grid_template_areas :
  Properties.grid_template_areas -> Variables_intf.any_var list
(** [vars_of_grid_template_areas v] is the variables [v] references. *)

val vars_of_isolation : Properties.isolation -> Variables_intf.any_var list
(** [vars_of_isolation v] is the variables [v] references. *)

val vars_of_order : Properties.order -> Variables_intf.any_var list
(** [vars_of_order v] is the variables [v] references. *)

val vars_of_table_layout :
  Properties.table_layout -> Variables_intf.any_var list
(** [vars_of_table_layout v] is the variables [v] references. *)

val vars_of_webkit_line_clamp :
  Properties.webkit_line_clamp -> Variables_intf.any_var list
(** [vars_of_webkit_line_clamp v] is the variables [v] references. *)

val vars_of_z_index : Properties.z_index -> Variables_intf.any_var list
(** [vars_of_z_index v] is the variables [v] references. *)

val vars_of_grid_area : Properties.grid_area -> Variables_intf.any_var list
(** [vars_of_grid_area v] is the variables [v] references. *)
