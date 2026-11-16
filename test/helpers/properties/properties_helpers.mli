(** Checkers shared by the property test suites. *)

val check_position : ?minify:bool -> ?expected:string -> string -> unit
(** [check_position input] reads [input] with the [position] reader and checks
    how it prints. *)

val check_overflow : ?minify:bool -> ?expected:string -> string -> unit
(** [check_overflow input] reads [input] with the [overflow] reader and checks
    how it prints. *)

val check_visibility : ?minify:bool -> ?expected:string -> string -> unit
(** [check_visibility input] reads [input] with the [visibility] reader and
    checks how it prints. *)

val check_flex_direction : ?minify:bool -> ?expected:string -> string -> unit
(** [check_flex_direction input] reads [input] with the [flex-direction] reader
    and checks how it prints. *)

val check_align_self :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_align_self input] reads [input] with the [align-self] reader and
    checks how it prints. *)

val check_text_align :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_text_align input] reads [input] with the [text-align] reader and
    checks how it prints. *)

val check_text_decoration_style :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_text_decoration_style input] reads [input] with the
    [text-decoration-style] reader and checks how it prints. *)

val check_text_wrap :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_text_wrap input] reads [input] with the [text-wrap] reader and checks
    how it prints. *)

val check_white_space :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_white_space input] reads [input] with the [white-space] reader and
    checks how it prints. *)

val check_word_break :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_word_break input] reads [input] with the [word-break] reader and
    checks how it prints. *)

val check_overflow_wrap :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_overflow_wrap input] reads [input] with the [overflow-wrap] reader
    and checks how it prints. *)

val check_hyphens :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_hyphens input] reads [input] with the [hyphens] reader and checks how
    it prints. *)

val check_user_select :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_user_select input] reads [input] with the [user-select] reader and
    checks how it prints. *)

val check_pointer_events :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_pointer_events input] reads [input] with the [pointer-events] reader
    and checks how it prints. *)

val check_resize :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_resize input] reads [input] with the [resize] reader and checks how
    it prints. *)

val check_box_sizing :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_box_sizing input] reads [input] with the [box-sizing] reader and
    checks how it prints. *)

val check_object_fit :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_object_fit input] reads [input] with the [object-fit] reader and
    checks how it prints. *)

val check_content_visibility :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_content_visibility input] reads [input] with the [content-visibility]
    reader and checks how it prints. *)

val check_container_shorthand : ?expected:string -> string -> unit
(** [check_container_shorthand input] reads [input] as a [container] shorthand
    and checks that it prints minified as [expected] ([input] by default) and
    reads back to the same value. *)

val check_scroll_snap_align :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_scroll_snap_align input] reads [input] with the [scroll-snap-align]
    reader and checks how it prints. *)

val check_scroll_snap_stop :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_scroll_snap_stop input] reads [input] with the [scroll-snap-stop]
    reader and checks how it prints. *)

val check_scroll_snap_type :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_scroll_snap_type input] reads [input] with the [scroll-snap-type]
    reader and checks how it prints. *)

val check_direction :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_direction input] reads [input] with the [direction] reader and checks
    how it prints. *)

val check_writing_mode :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_writing_mode input] reads [input] with the [writing-mode] reader and
    checks how it prints. *)

val check_print_color_adjust :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_print_color_adjust input] reads [input] with the [print-color-adjust]
    reader and checks how it prints. *)

val check_appearance :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_appearance input] reads [input] with the [appearance] reader and
    checks how it prints. *)

val check_clear :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_clear input] reads [input] with the [clear] reader and checks how it
    prints. *)

val check_float_side :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_float_side input] reads [input] with the [float] reader and checks
    how it prints. *)

val check_font_feature_settings :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_font_feature_settings input] reads [input] with the
    [font-feature-settings] reader and checks how it prints. *)

val check_font_variation_settings :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_font_variation_settings input] reads [input] with the
    [font-variation-settings] reader and checks how it prints. *)

val check_timing_function :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_timing_function input] reads [input] with the [timing-function]
    reader and checks how it prints. *)

val check_content :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_content input] reads [input] with the [content] reader and checks how
    it prints. *)

val check_grid_template :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_grid_template input] reads [input] with the [grid-template] reader
    and checks how it prints. *)

val check_grid_line :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_grid_line input] reads [input] with the [grid-line] reader and checks
    how it prints. *)

val check_column_count :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_column_count input] reads [input] with the [column-count] reader and
    checks how it prints. *)

val check_transform :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_transform input] reads [input] with the [transform] reader and checks
    how it prints. *)

val check_transforms :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_transforms input] reads [input] with the [transforms] reader and
    checks how it prints. *)

val check_scroll_snap_axis :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_scroll_snap_axis input] reads [input] with the [scroll_snap_axis]
    reader and checks how it prints. *)

val check_scroll_snap_strictness :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_scroll_snap_strictness input] reads [input] with the
    [scroll_snap_strictness] reader and checks how it prints. *)

val check_clip_path :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_clip_path input] reads [input] with the [clip_path] reader and checks
    how it prints. *)

val check_columns_value :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_columns_value input] reads [input] with the [columns_value] reader
    and checks how it prints. *)

val check_field_sizing :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_field_sizing input] reads [input] with the [field_sizing] reader and
    checks how it prints. *)

val check_mask_box :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_mask_box input] reads [input] with the [mask_box] reader and checks
    how it prints. *)

val check_mask_composite :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_mask_composite input] reads [input] with the [mask_composite] reader
    and checks how it prints. *)

val check_mask_mode :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_mask_mode input] reads [input] with the [mask_mode] reader and checks
    how it prints. *)

val check_webkit_mask_composite :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_webkit_mask_composite input] reads [input] with the
    [webkit_mask_composite] reader and checks how it prints. *)

val check_webkit_mask_source_type :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_webkit_mask_source_type input] reads [input] with the
    [webkit_mask_source_type] reader and checks how it prints. *)

val check_east_asian_feature :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_east_asian_feature input] reads [input] with the [east_asian_feature]
    reader and checks how it prints. *)

val check_grid_line_pair :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_grid_line_pair input] reads [input] with the [grid_line_pair] reader
    and checks how it prints. *)

val check_grid_template_areas :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_grid_template_areas input] reads [input] with the
    [grid_template_areas] reader and checks how it prints. *)

val check_ray_size :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_ray_size input] reads [input] with the [ray_size] reader and checks
    how it prints. *)

val check_tab_size :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_tab_size input] reads [input] with the [tab_size] reader and checks
    how it prints. *)

val check_zoom :
  ?minify:bool -> ?roundtrip:bool -> ?expected:string -> string -> unit
(** [check_zoom input] reads [input] with the [zoom] reader and checks how it
    prints. *)
