(** CSS properties: types and helpers. *)

open Values
include module type of Properties_intf

val pp_property : 'a property Pp.t
(** [pp_property] is the pretty-printer for property names. *)

val property_is_inherited : 'a property -> bool
(** [property_is_inherited property] is whether [property] inherits by default.
    Shorthands return [true] when every longhand they reset inherits. The
    classification is exhaustive over the property GADT, so adding a property
    requires an explicit decision here. *)

val minified_name_carries : 'a property -> 'a -> bool
(** [minified_name_carries property value] is whether the name {!pp_property}
    gives [property] under minify can carry [value]. A [page-break-*] property
    minifies to the CSS Fragmentation 3 sec. 3.4 [break-*] property it aliases,
    and that alias is defined by a value mapping table, so it is a spelling the
    declaration can use only for a value the table names. A [var()] is not one:
    substitution happens at computed-value time, and [always], the only mapping
    that is not the identity, has no [break-*] spelling. Every other property
    names itself the same whatever it carries. *)

val compare_property : 'a property -> 'b property -> int
(** [compare_property a b] is a total order on property identities. It is [0]
    exactly when [a] and [b] are the same property, so an ordered container
    keyed on a property agrees with equality on it. *)

val eq_property : 'a property -> 'b property -> ('a, 'b) Type.eq option
(** [eq_property a b] is [Some Equal] when [a] and [b] are the same property,
    and [None] otherwise. It answers {!compare_property}'s question and carries
    the proof that the two properties value the same type, which the comparison
    on its own cannot express. *)

val pp_property_value : ('a property * 'a) Pp.t
(** [pp_property_value] is the pretty-printer for a property and its typed
    value. *)

val normalize_property_value :
  ?lossless:bool ->
  ?exact_srgb:bool ->
  ?resolve_missing:bool ->
  ?ctx:Values.calc_ctx ->
  'a property ->
  'a ->
  'a
(** [normalize_property_value ?lossless prop value] applies semantic
    (equivalence) canonicalisation to [value] so the optimizer holds a canonical
    AST and the pretty-printer stays a pure serialiser. Identity for properties
    whose folds have not yet migrated out of [pp]. [lossless] disables bounded
    colour and numeric approximation while keeping exact canonicalisation.

    [exact_srgb] and [resolve_missing] are {!Values.normalize_color}'s flags of
    the same names, applied to the properties whose whole value is a colour.
    Both are for the canonical diff projection only. A colour a value nests - a
    gradient stop, a shadow colour - normalises without either, so
    [resolve_missing] never reaches a colour the value interpolates. *)

val normalize_custom_property_value :
  ?lossless:bool ->
  ?ctx:Values.calc_ctx ->
  custom_property_value ->
  custom_property_value
(** [normalize_custom_property_value v] canonicalises the typed value of a
    registered custom property (currently the [<color>] syntax) so a promoted
    custom-property value folds the same way a typed colour property would. *)

val is_invalid_value : 'a property -> 'a -> bool
(** [is_invalid_value prop value] is [true] when [value] contains an [Invalid]
    arm cascade detected at parse time (CSS spec violations preserved verbatim
    for round-trip). [Optimize.drop_invalid], which every serialisation runs,
    removes declarations that satisfy this predicate. The classification is
    exhaustive over the property GADT, so adding a property requires an explicit
    validity decision here. *)

val pp_value : ('a kind * 'a) Pp.t
(** [pp_value] pretty-prints a typed custom property value. *)

val pp_custom_property_value : custom_property_value Pp.t
(** [pp_custom_property_value] pretty-prints a parsed custom-property payload.
*)

val read_custom_property_value :
  ?font_family:bool -> Cursor.t -> custom_property_value
(** [read_custom_property_value t] returns the remaining component values as a
    raw token stream. Typed promotion is deferred to the [@property] registry
    pass. *)

val unquote_font_family_strings : custom_value -> custom_value
(** [unquote_font_family_strings components] rewrites each [<string>] token
    whose content is an identifier sequence as the equivalent
    [<ident> <whitespace> <ident> ...] component sequence, which CSS Fonts 4
    sec. 2.1.1 spells as the same [<font-family-name>], one word or several.
    Only for a stream a generic family has proven to be a font stack. A string
    holding a word that sec. 2.1.1 excludes from [<custom-ident>], and any token
    that isn't a [<string>], pass through unchanged; a name of one word also
    clears [emoji], [fangsong] and [none]. *)

val canonicalize_math_whitespace_components : custom_value -> custom_value
(** [canonicalize_math_whitespace_components components] drops the whitespace of
    [components] that CSS reads as nothing: around the [*] and [/] of CSS Values
    4 (ED) sec. 10.8 arithmetic, and after a function or block whose closing
    bracket already separates it from what follows. The whitespace sec. 10.8
    requires around a math [+] or [-], and the whitespace next to a [var()],
    [env()] or [attr()] whose substitution would otherwise merge with its
    neighbour, both stay. *)

val components_have_generic_family : custom_value -> bool
(** [components_have_generic_family components] is [true] when a bare ident in
    [components] matches a generic font family ([sans-serif], [ui-monospace],
    ...). A generic family is only valid in a font-family list, so its presence
    proves the stream is a font-family value. *)

val try_read_custom_color : custom_value -> custom_property_value option
(** [try_read_custom_color tokens] parses [tokens] as a [<color>], returning a
    typed payload when the stream matches. *)

val try_read_custom_length : custom_value -> custom_property_value option
(** [try_read_custom_length tokens] parses [tokens] as a [<length>]. *)

val try_read_custom_length_percentage :
  custom_value -> custom_property_value option
(** [try_read_custom_length_percentage tokens] parses [tokens] as a
    [<length-percentage>]. *)

val try_read_custom_number : custom_value -> custom_property_value option
(** [try_read_custom_number tokens] parses [tokens] as a [<number>]. *)

val try_read_custom_percentage : custom_value -> custom_property_value option
(** [try_read_custom_percentage tokens] parses [tokens] as a [<percentage>]. *)

val try_read_custom_angle : custom_value -> custom_property_value option
(** [try_read_custom_angle tokens] parses [tokens] as an [<angle>]. *)

val try_read_custom_time : custom_value -> custom_property_value option
(** [try_read_custom_time tokens] parses [tokens] as a [<time>]. *)

val components_of_custom_property_value :
  custom_property_value -> Component.t list
(** [components_of_custom_property_value value] returns the component stream
    represented by a custom-property payload. Typed values are serialized with
    the normal minified typed printer before being parsed back to components. *)

val string_of_kind_value : 'a kind -> 'a -> string
(** [string_of_kind_value kind value] serializes a typed CSS value for a custom
    property initial value. Values that cannot be represented as a concrete
    initial value serialize to ["initial"]. *)

val pp_shadow : shadow Pp.t
(** [pp_shadow] is the pretty-printer for {!val-shadow} values. *)

val read_shadow : Cursor.t -> shadow
(** [read_shadow t] parses a {!val-shadow} value from [t]. *)

val read_timeline_shorthand : Cursor.t -> timeline_shorthand
(** [read_timeline_shorthand t] parses [scroll-timeline]. *)

val pp_timeline_shorthand : timeline_shorthand Pp.t
(** [pp_timeline_shorthand] pretty-prints [scroll-timeline] shorthand values. *)

val read_view_timeline_shorthand : Cursor.t -> view_timeline_shorthand
(** [read_view_timeline_shorthand t] parses [view-timeline]. *)

val pp_view_timeline_shorthand : view_timeline_shorthand Pp.t
(** [pp_view_timeline_shorthand] pretty-prints [view-timeline] shorthand values.
*)

val pp_timeline_axis : timeline_axis Pp.t
(** [pp_timeline_axis] pretty-prints a timeline axis. *)

val read_timeline_axis : Cursor.t -> timeline_axis
(** [read_timeline_axis t] parses a timeline axis. *)

val pp_timeline_name : timeline_name Pp.t
(** [pp_timeline_name] pretty-prints a timeline name list. *)

val read_timeline_name : Cursor.t -> timeline_name
(** [read_timeline_name t] parses [view-timeline-name] and [timeline-scope]. *)

val pp_timeline_inset : timeline_inset Pp.t
(** [pp_timeline_inset] pretty-prints [view-timeline-inset]. *)

val read_timeline_inset : Cursor.t -> timeline_inset
(** [read_timeline_inset t] parses [view-timeline-inset]. *)

val pp_position_try_order : position_try_order Pp.t
(** [pp_position_try_order] pretty-prints [position-try-order]. *)

val pp_position_try_fallback : position_try_fallback Pp.t
(** [pp_position_try_fallback] pretty-prints a [position-try-fallback] item. *)

val pp_position_try_fallbacks : position_try_fallbacks Pp.t
(** [pp_position_try_fallbacks] pretty-prints [position-try-fallbacks]. *)

val read_position_try_fallback : Cursor.t -> position_try_fallback
(** [read_position_try_fallback t] parses a single [position-try-fallback]. *)

val read_position_try_fallbacks : Cursor.t -> position_try_fallbacks
(** [read_position_try_fallbacks t] parses [position-try-fallbacks]. *)

val read_position_try_order : Cursor.t -> position_try_order
(** [read_position_try_order t] parses [position-try-order]. *)

val read_position_try : Cursor.t -> position_try
(** [read_position_try t] parses the [position-try] shorthand. *)

val pp_contain_intrinsic_size_item : contain_intrinsic_size_item Pp.t
(** [pp_contain_intrinsic_size_item] pretty-prints one [contain-intrinsic-size]
    item. *)

val read_contain_intrinsic_size_item : Cursor.t -> contain_intrinsic_size_item
(** [read_contain_intrinsic_size_item t] parses one [contain-intrinsic-size]
    item. *)

val pp_counter_item : counter_item Pp.t
(** [pp_counter_item] pretty-prints one [counter-reset]/[counter-increment]
    item. *)

val read_counter_item : Cursor.t -> counter_item
(** [read_counter_item t] parses one [counter-reset]/[counter-increment] item.
*)

val pp_font_synthesis_feature : font_synthesis_feature Pp.t
(** [pp_font_synthesis_feature] pretty-prints one [font-synthesis] feature. *)

val read_font_synthesis_feature : Cursor.t -> font_synthesis_feature
(** [read_font_synthesis_feature t] parses one [font-synthesis] feature. *)

val pp_line_fit_edge_keyword : line_fit_edge_keyword Pp.t
(** [pp_line_fit_edge_keyword] pretty-prints a [line-fit-edge] keyword. *)

val read_line_fit_edge_keyword : Cursor.t -> line_fit_edge_keyword
(** [read_line_fit_edge_keyword t] parses a [line-fit-edge] keyword. *)

val pp_mask_layer : mask_layer Pp.t
(** [pp_mask_layer] pretty-prints one [mask] layer. *)

val read_mask_layer : Cursor.t -> mask_layer
(** [read_mask_layer t] parses one [mask] layer. *)

val pp_min_intrinsic_sizing_keyword : min_intrinsic_sizing_keyword Pp.t
(** [pp_min_intrinsic_sizing_keyword] pretty-prints a [min-intrinsic-sizing]
    keyword. *)

val read_min_intrinsic_sizing_keyword : Cursor.t -> min_intrinsic_sizing_keyword
(** [read_min_intrinsic_sizing_keyword t] parses a [min-intrinsic-sizing]
    keyword. *)

val pp_offset_rotate_mode : offset_rotate_mode Pp.t
(** [pp_offset_rotate_mode] pretty-prints the [offset-rotate] mode prefix. *)

val read_offset_rotate_mode : Cursor.t -> offset_rotate_mode
(** [read_offset_rotate_mode t] parses the [offset-rotate] mode prefix. *)

val pp_overflow_clip_box : overflow_clip_box Pp.t
(** [pp_overflow_clip_box] pretty-prints [overflow-clip-box]. *)

val read_overflow_clip_box : Cursor.t -> overflow_clip_box
(** [read_overflow_clip_box t] parses [overflow-clip-box]. *)

val pp_position_area_keyword : position_area_keyword Pp.t
(** [pp_position_area_keyword] pretty-prints one [position-area] keyword. *)

val read_position_area_keyword : Cursor.t -> position_area_keyword
(** [read_position_area_keyword t] parses one [position-area] keyword. *)

val pp_ruby_position_keyword : ruby_position_keyword Pp.t
(** [pp_ruby_position_keyword] pretty-prints one [ruby-position] keyword. *)

val read_ruby_position_keyword : Cursor.t -> ruby_position_keyword
(** [read_ruby_position_keyword t] parses one [ruby-position] keyword. *)

val pp_text_box_edge_keyword : text_box_edge_keyword Pp.t
(** [pp_text_box_edge_keyword] pretty-prints one [text-box-edge] keyword. *)

val read_text_box_edge_keyword : Cursor.t -> text_box_edge_keyword
(** [read_text_box_edge_keyword t] parses one [text-box-edge] keyword. *)

val pp_text_emphasis_fill : text_emphasis_fill Pp.t
(** [pp_text_emphasis_fill] pretty-prints the [text-emphasis] fill keyword. *)

val read_text_emphasis_fill : Cursor.t -> text_emphasis_fill
(** [read_text_emphasis_fill t] parses the [text-emphasis] fill keyword. *)

val pp_text_emphasis_line : text_emphasis_line Pp.t
(** [pp_text_emphasis_line] pretty-prints a [text-emphasis] line keyword. *)

val read_text_emphasis_line : Cursor.t -> text_emphasis_line
(** [read_text_emphasis_line t] parses a [text-emphasis] line keyword. *)

val pp_text_emphasis_shape : text_emphasis_shape Pp.t
(** [pp_text_emphasis_shape] pretty-prints a [text-emphasis] shape keyword. *)

val read_text_emphasis_shape : Cursor.t -> text_emphasis_shape
(** [read_text_emphasis_shape t] parses a [text-emphasis] shape keyword. *)

val pp_text_emphasis_side : text_emphasis_side Pp.t
(** [pp_text_emphasis_side] pretty-prints a [text-emphasis] side keyword. *)

val read_text_emphasis_side : Cursor.t -> text_emphasis_side
(** [read_text_emphasis_side t] parses a [text-emphasis] side keyword. *)

val pp_text_underline_position_keyword : text_underline_position_keyword Pp.t
(** [pp_text_underline_position_keyword] pretty-prints a
    [text-underline-position] keyword. *)

val read_text_underline_position_keyword :
  Cursor.t -> text_underline_position_keyword
(** [read_text_underline_position_keyword t] parses a [text-underline-position]
    keyword. *)

val pp_timeline_inset_item : timeline_inset_item Pp.t
(** [pp_timeline_inset_item] pretty-prints one [view-timeline-inset] item. *)

val read_timeline_inset_item : Cursor.t -> timeline_inset_item
(** [read_timeline_inset_item t] parses one [view-timeline-inset] item. *)

val pp_timeline_shorthand_item : timeline_shorthand_item Pp.t
(** [pp_timeline_shorthand_item] pretty-prints one [scroll-timeline] shorthand
    item. *)

val read_timeline_shorthand_item : Cursor.t -> timeline_shorthand_item
(** [read_timeline_shorthand_item t] parses one [scroll-timeline] shorthand
    item. *)

val pp_view_timeline_shorthand_item : view_timeline_shorthand_item Pp.t
(** [pp_view_timeline_shorthand_item] pretty-prints one [view-timeline]
    shorthand item. *)

val read_view_timeline_shorthand_item : Cursor.t -> view_timeline_shorthand_item
(** [read_view_timeline_shorthand_item t] parses one [view-timeline] shorthand
    item. *)

val pp_font_variant_emoji : font_variant_emoji Pp.t
(** [pp_font_variant_emoji] pretty-prints [font-variant-emoji]. *)

val read_font_variant_emoji : Cursor.t -> font_variant_emoji
(** [read_font_variant_emoji t] parses [font-variant-emoji]. *)

val pp_dominant_baseline : dominant_baseline Pp.t
(** [pp_dominant_baseline] pretty-prints [dominant-baseline]. *)

val read_dominant_baseline : Cursor.t -> dominant_baseline
(** [read_dominant_baseline t] parses [dominant-baseline]. *)

val pp_ray_size : ray_size Pp.t
(** [pp_ray_size] pretty-prints the size keyword inside [ray()]. *)

val read_ray_size : Cursor.t -> ray_size
(** [read_ray_size t] parses the size keyword inside [ray()]. *)

val pp_initial_letter_align_keyword : initial_letter_align_keyword Pp.t
(** [pp_initial_letter_align_keyword] pretty-prints one [initial-letter-align]
    keyword. *)

val read_initial_letter_align_keyword : Cursor.t -> initial_letter_align_keyword
(** [read_initial_letter_align_keyword t] parses one [initial-letter-align]
    keyword. *)

val pp_font_size_adjust_metric : font_size_adjust_metric Pp.t
(** [pp_font_size_adjust_metric] pretty-prints the metric keyword of
    [font-size-adjust]. *)

val read_font_size_adjust_metric : Cursor.t -> font_size_adjust_metric
(** [read_font_size_adjust_metric t] parses the metric keyword of
    [font-size-adjust]. *)

val pp_animation_range_name : animation_range_name Pp.t
(** [pp_animation_range_name] pretty-prints a [<timeline-range-name>]. *)

val read_animation_range_name : Cursor.t -> animation_range_name
(** [read_animation_range_name t] parses a [<timeline-range-name>]. *)

val pp_border_image_repeat_keyword : border_image_repeat_keyword Pp.t
(** [pp_border_image_repeat_keyword] pretty-prints one [border-image-repeat]
    keyword. *)

val read_border_image_repeat_keyword : Cursor.t -> border_image_repeat_keyword
(** [read_border_image_repeat_keyword t] parses one [border-image-repeat]
    keyword. *)

val pp_margin_trim_axis : margin_trim_axis Pp.t
(** [pp_margin_trim_axis] pretty-prints a [margin-trim] axis keyword. *)

val read_margin_trim_axis : Cursor.t -> margin_trim_axis
(** [read_margin_trim_axis t] parses a [margin-trim] axis keyword. *)

val pp_margin_trim_edge : margin_trim_edge Pp.t
(** [pp_margin_trim_edge] pretty-prints a [margin-trim] edge keyword. *)

val read_margin_trim_edge : Cursor.t -> margin_trim_edge
(** [read_margin_trim_edge t] parses a [margin-trim] edge keyword. *)

val pp_font_size_adjust : font_size_adjust Pp.t
(** [pp_font_size_adjust] pretty-prints [font-size-adjust]. *)

val read_font_size_adjust : Cursor.t -> font_size_adjust
(** [read_font_size_adjust t] parses [font-size-adjust]. *)

val read_initial_letter : Cursor.t -> initial_letter
(** [read_initial_letter t] parses [initial-letter]. *)

val pp_initial_letter_align : initial_letter_align Pp.t
(** [pp_initial_letter_align] pretty-prints [initial-letter-align]. *)

val read_initial_letter_align : Cursor.t -> initial_letter_align
(** [read_initial_letter_align t] parses [initial-letter-align]. *)

val pp_initial_letter_wrap : initial_letter_wrap Pp.t
(** [pp_initial_letter_wrap] pretty-prints [initial-letter-wrap]. *)

val read_initial_letter_wrap : Cursor.t -> initial_letter_wrap
(** [read_initial_letter_wrap t] parses [initial-letter-wrap]. *)

val pp_margin_trim : margin_trim Pp.t
(** [pp_margin_trim] pretty-prints [margin-trim]. *)

val read_margin_trim : Cursor.t -> margin_trim
(** [read_margin_trim t] parses [margin-trim]. *)

val pp_offset_path : offset_path Pp.t
(** [pp_offset_path] pretty-prints [offset-path]. *)

val read_offset_path : Cursor.t -> offset_path
(** [read_offset_path t] parses [offset-path]. *)

val pp_offset_anchor : offset_anchor Pp.t
(** [pp_offset_anchor] pretty-prints [offset-anchor]. *)

val read_offset_anchor : Cursor.t -> offset_anchor
(** [read_offset_anchor t] parses [offset-anchor]. *)

val pp_offset : offset Pp.t
(** [pp_offset] pretty-prints the [offset] shorthand. *)

val read_offset : Cursor.t -> offset
(** [read_offset t] parses the [offset] shorthand. *)

val pp_offset_target : offset_target Pp.t
(** [pp_offset_target] pretty-prints the required leading group of the [offset]
    shorthand. *)

val read_offset_target : Cursor.t -> offset_target
(** [read_offset_target t] parses the required leading group of the [offset]
    shorthand. *)

val pp_offset_position : offset_position Pp.t
(** [pp_offset_position] pretty-prints [offset-position]. *)

val read_offset_position : Cursor.t -> offset_position
(** [read_offset_position t] parses [offset-position]. *)

val pp_animation_range_item : animation_range_item Pp.t
(** [pp_animation_range_item] pretty-prints one [<single-animation-range>]. *)

val read_animation_range_item : Cursor.t -> animation_range_item
(** [read_animation_range_item t] parses one [<single-animation-range>]. *)

val read_animation_range : Cursor.t -> animation_range
(** [read_animation_range t] parses [animation-range]. *)

val pp_border_image_slice_item : border_image_slice_item Pp.t
(** [pp_border_image_slice_item] pretty-prints one [border-image-slice] item. *)

val read_border_image_slice_item : Cursor.t -> border_image_slice_item
(** [read_border_image_slice_item t] parses one [border-image-slice] item. *)

val pp_border_image_slice : border_image_slice Pp.t
(** [pp_border_image_slice] pretty-prints [border-image-slice]. *)

val read_border_image_slice : Cursor.t -> border_image_slice
(** [read_border_image_slice t] parses [border-image-slice]. *)

val read_border_image_repeat : Cursor.t -> border_image_repeat
(** [read_border_image_repeat t] parses the [border-image-repeat] longhand,
    including the CSS-wide keywords. *)

val read_border_image_width : Cursor.t -> border_image_width
(** [read_border_image_width t] parses the [border-image-width] longhand,
    including the CSS-wide keywords. *)

val read_border_image_outset : Cursor.t -> border_image_outset
(** [read_border_image_outset t] parses the [border-image-outset] longhand,
    including the CSS-wide keywords. *)

val pp_border_image_width_item : border_image_width_item Pp.t
(** [pp_border_image_width_item] pretty-prints one [border-image-width] item. *)

val read_border_image_width_item : Cursor.t -> border_image_width_item
(** [read_border_image_width_item t] parses one [border-image-width] item. *)

val pp_border_image_outset_item : border_image_outset_item Pp.t
(** [pp_border_image_outset_item] pretty-prints one [border-image-outset] item.
*)

val read_border_image_outset_item : Cursor.t -> border_image_outset_item
(** [read_border_image_outset_item t] parses one [border-image-outset] item. *)

val pp_mask_border_mode : mask_border_mode Pp.t
(** [pp_mask_border_mode] pretty-prints a [mask-border] mode keyword. *)

val read_mask_border_mode : Cursor.t -> mask_border_mode
(** [read_mask_border_mode t] parses a [mask-border] mode keyword. *)

val pp_border_image : border_image Pp.t
(** [pp_border_image] pretty-prints the [border-image] shorthand. *)

val read_border_image : Cursor.t -> border_image
(** [read_border_image t] parses the [border-image] shorthand. *)

val read_mask_border : Cursor.t -> border_image
(** [read_mask_border t] parses the [mask-border] shorthand. It shares
    {!read_border_image}'s slots and adds the [<'mask-border-mode'>] one, which
    border-image has no grammar for. *)

val read_grid_template_areas : Cursor.t -> grid_template_areas
(** [read_grid_template_areas t] parses [grid-template-areas]. *)

val read_border_spacing : Cursor.t -> border_spacing
(** [read_border_spacing t] parses [border-spacing]. *)

val pp_position_visibility_condition : position_visibility_condition Pp.t
(** [pp_position_visibility_condition] pretty-prints one [position-visibility]
    condition keyword. *)

val read_position_visibility_condition :
  Cursor.t -> position_visibility_condition
(** [read_position_visibility_condition t] parses one [position-visibility]
    condition keyword. *)

val pp_ray : ray Pp.t
(** [pp_ray] pretty-prints a [ray()] expression. *)

val read_ray : Cursor.t -> ray
(** [read_ray t] parses a [ray()] expression. *)

val pp_property_value_kind : 'a property_value_kind Pp.t
(** [pp_property_value_kind] pretty-prints the kind label of a typed property
    value. *)

val read_property_value_kind : Cursor.t -> 'a property_value_kind
(** [read_property_value_kind t] always raises. The {!val-property_value_kind}
    GADT classifies typed values for dispatch and is not addressable as a
    standalone CSS value; the function exists only so the API surface stays
    symmetric. *)

val pp_position_visibility : position_visibility Pp.t
(** [pp_position_visibility] pretty-prints [position-visibility]. *)

val read_position_visibility : Cursor.t -> position_visibility
(** [read_position_visibility t] parses [position-visibility]. *)

val pp_position_area : position_area Pp.t
(** [pp_position_area] pretty-prints [position-area]. *)

val read_position_area : Cursor.t -> position_area
(** [read_position_area t] parses [position-area]. *)

val pp_offset_rotate : offset_rotate Pp.t
(** [pp_offset_rotate] pretty-prints [offset-rotate]. *)

val read_offset_rotate : Cursor.t -> offset_rotate
(** [read_offset_rotate t] parses [offset-rotate]. *)

val read_page_size : Cursor.t -> page_size
(** [read_page_size t] parses the paged-media [size] descriptor/property. *)

val pp_page_size : page_size Pp.t
(** [pp_page_size] pretty-prints the paged-media [size] descriptor/property. *)

val pp_page_size_name : page_size_name Pp.t
(** [pp_page_size_name] pretty-prints a page size name. *)

val read_page_size_name : Cursor.t -> page_size_name
(** [read_page_size_name t] parses a page size name. *)

val pp_page_size_orientation : page_size_orientation Pp.t
(** [pp_page_size_orientation] pretty-prints a page size orientation. *)

val read_page_size_orientation : Cursor.t -> page_size_orientation
(** [read_page_size_orientation t] parses a page size orientation. *)

(* Background and animation helpers moved from Css *)

val url : string -> background_image
(** [url path] builds a [background_image] URL value. *)

val linear_gradient :
  gradient_direction -> gradient_stop list -> background_image
(** [linear_gradient dir stops] builds a linear-gradient background image. *)

val radial_gradient :
  ?config:radial_gradient_config -> gradient_stop list -> background_image
(** [radial_gradient ?config stops] builds a radial-gradient background image.
*)

val color_stop : Values.color -> gradient_stop
(** [color_stop c] is a gradient stop with just a color. *)

val color_position : Values.color -> Values.length -> gradient_stop
(** [color_position c pos] is a gradient stop with color and position. *)

val animation_shorthand :
  ?name:string ->
  ?duration:Values.duration ->
  ?timing_function:timing_function ->
  ?delay:Values.duration ->
  ?iteration_count:animation_iteration_count ->
  ?direction:animation_direction ->
  ?fill_mode:animation_fill_mode ->
  ?play_state:animation_play_state ->
  ?timeline:animation_timeline ->
  unit ->
  animation
(** [animation_shorthand ?name ?duration ?timing_function ?delay
     ?iteration_count ?direction ?fill_mode ?play_state ?timeline ()] is the
    animation shorthand. *)

val transition_shorthand :
  ?property:transition_property_value ->
  ?duration:Values.duration ->
  ?timing_function:timing_function ->
  ?delay:Values.duration ->
  ?behavior:transition_behavior ->
  unit ->
  transition
(** [transition_shorthand ?property ?duration ?timing_function ?delay ?behavior
     ()] is the transition shorthand. Defaults to property = All. *)

val border_shorthand :
  ?width:border_width ->
  ?style:border_style ->
  ?color:Values.color ->
  unit ->
  border
(** [border_shorthand ?width ?style ?color ()] is the border shorthand. *)

val text_decoration_shorthand :
  ?lines:text_decoration_line list ->
  ?style:text_decoration_style ->
  ?color:Values.color ->
  ?thickness:Values.length ->
  unit ->
  text_decoration
(** [text_decoration_shorthand ?lines ?style ?color ?thickness ()] is the
    text-decoration shorthand. *)

val background_shorthand :
  ?color:Values.color ->
  ?image:background_image ->
  ?position:position_value ->
  ?size:background_size ->
  ?repeat:background_repeat ->
  ?attachment:background_attachment ->
  ?clip:background_box ->
  ?origin:background_box ->
  unit ->
  background
(** [background_shorthand ?color ?image ?position ?size ?repeat ?attachment
     ?clip ?origin ()] is the background shorthand. *)

(** Pretty-printers and readers for property value types. *)

val pp_border_style : border_style Pp.t
(** [pp_border_style] is the pretty-printer for [border_style]. *)

val read_border_style : Cursor.t -> border_style
(** [read_border_style t] is the [border_style] parsed from [t]. *)

val read_border_style_box : Cursor.t -> border_style list
(** [read_border_style_box t] is the [<line-style>{1,4}] box the [border-style]
    shorthand takes (CSS Backgrounds 3 (ED) sec. 3.2). *)

val pp_border_width : border_width Pp.t
(** [pp_border_width] is the pretty-printer for [border_width]. *)

val read_border_width : Cursor.t -> border_width
(** [read_border_width t] is the [border_width] parsed from [t]. *)

val border_width_has_runtime_subst : border_width -> bool
(** [border_width_has_runtime_subst w] is [true] when [w] is a [var()] or
    reaches one through a math function. *)

val pp_border : border Pp.t
(** [pp_border] is the pretty-printer for [border]. *)

val read_border : Cursor.t -> border
(** [read_border t] is the [border] shorthand parsed from [t]. *)

val pp_border_shorthand : border_shorthand Pp.t
(** [pp_border_shorthand] pretty-prints a border shorthand value. *)

val read_border_shorthand : Cursor.t -> border_shorthand
(** [read_border_shorthand t] parses a border shorthand value. *)

val pp_line_height : line_height Pp.t
(** [pp_line_height] is the pretty-printer for [line_height]. *)

val read_line_height : Cursor.t -> line_height
(** [read_line_height t] is the [line_height] parsed from [t]. *)

val pp_font_weight : font_weight Pp.t
(** [pp_font_weight] is the pretty-printer for [font_weight]. *)

val read_font_weight : Cursor.t -> font_weight
(** [read_font_weight t] is the [font_weight] parsed from [t]. *)

val pp_display : display Pp.t
(** [pp_display] is the pretty-printer for [display]. *)

val read_display : Cursor.t -> display
(** [read_display t] is the [display] parsed from [t]. *)

val pp_position : position Pp.t
(** [pp_position] is the pretty-printer for [position]. *)

val read_position : Cursor.t -> position
(** [read_position t] is the [position] parsed from [t]. *)

val pp_css_wide : css_wide Pp.t
(** [pp_css_wide] is the pretty-printer for the CSS-wide keyword set ([initial],
    [inherit], [unset], [revert], [revert-layer]). *)

val read_css_wide : Cursor.t -> css_wide
(** [read_css_wide t] reads a CSS-wide keyword (and only those) from [t]. *)

val is_css_wide_keyword : string -> bool
(** [is_css_wide_keyword s] is [true] when [s] (case-insensitively) is one of
    [initial], [inherit], [unset], [revert], [revert-layer]. *)

val value_has_css_wide_mix : string -> bool
(** [value_has_css_wide_mix value] is [true] when [value] is not itself a lone
    CSS-wide keyword but contains one mixed with other tokens (CSS Cascade 5
    sec. 7.3 forbids this in a multi-value shorthand). *)

val components_have_css_wide_mix : Component.t list -> bool
(** [components_have_css_wide_mix cvs] is the same check as
    {!value_has_css_wide_mix}, but operates on a component list directly so
    callers that already hold one avoid the round-trip through a string buffer.
*)

val pp_visibility : visibility Pp.t
(** [pp_visibility] is the pretty-printer for [visibility]. *)

val read_visibility : Cursor.t -> visibility
(** [read_visibility t] is the [visibility] parsed from [t]. *)

val pp_baseline_source : baseline_source Pp.t
(** [pp_baseline_source] is the pretty-printer for [baseline_source]. *)

val read_baseline_source : Cursor.t -> baseline_source
(** [read_baseline_source t] is the [baseline_source] parsed from [t]. *)

val pp_alignment_baseline : alignment_baseline Pp.t
(** [pp_alignment_baseline] is the pretty-printer for [alignment_baseline]. *)

val read_alignment_baseline : Cursor.t -> alignment_baseline
(** [read_alignment_baseline t] is the [alignment_baseline] parsed from [t]. *)

val pp_baseline_shift : baseline_shift Pp.t
(** [pp_baseline_shift] is the pretty-printer for [baseline_shift]. *)

val read_baseline_shift : Cursor.t -> baseline_shift
(** [read_baseline_shift t] is the [baseline_shift] parsed from [t]. *)

val pp_z_index : z_index Pp.t
(** [pp_z_index] is the pretty-printer for [z_index]. *)

val read_z_index : Cursor.t -> z_index
(** [read_z_index t] is the [z_index] parsed from [t]. *)

val pp_tab_size : tab_size Pp.t
(** [pp_tab_size] is the pretty-printer for [tab_size]. *)

val pp_zoom : zoom Pp.t
(** [pp_zoom] is the pretty-printer for [zoom]. *)

val read_tab_size : Cursor.t -> tab_size
(** [read_tab_size t] is the [tab_size] parsed from [t]. *)

val read_zoom : Cursor.t -> zoom
(** [read_zoom t] is the [zoom] value parsed from [t]. *)

val pp_order : order Pp.t
(** [pp_order] is the pretty-printer for [order]. *)

val read_order : Cursor.t -> order
(** [read_order t] parses an order value (integer or calc expression). *)

val pp_overflow : overflow Pp.t
(** [pp_overflow] is the pretty-printer for [overflow]. *)

val pp_border_spacing : border_spacing Pp.t
(** [pp_border_spacing] is the pretty-printer for [border_spacing]. *)

val read_overflow : Cursor.t -> overflow
(** [read_overflow t] is the [overflow] parsed from [t]. *)

val read_overflow_single : Cursor.t -> overflow
(** [read_overflow_single t] parses a single-axis [overflow] value. *)

val pp_flex_direction : flex_direction Pp.t
(** [pp_flex_direction] is the pretty-printer for [flex_direction]. *)

val read_flex_direction : Cursor.t -> flex_direction
(** [read_flex_direction t] is the [flex_direction] parsed from [t]. *)

val pp_flex_wrap : flex_wrap Pp.t
(** [pp_flex_wrap] is the pretty-printer for [flex_wrap]. *)

val read_flex_wrap : Cursor.t -> flex_wrap
(** [read_flex_wrap t] is the [flex_wrap] parsed from [t]. *)

val pp_flex_flow : flex_flow Pp.t
(** [pp_flex_flow] is the pretty-printer for [flex_flow]. *)

val read_flex_flow : Cursor.t -> flex_flow
(** [read_flex_flow t] is the [flex_flow] parsed from [t]. *)

val pp_flex_factor : flex_factor Pp.t
(** [pp_flex_factor] is the pretty-printer for [flex_factor]. *)

val read_flex_factor : Cursor.t -> flex_factor
(** [read_flex_factor t] is the [flex_factor] parsed from [t]. *)

(* align type removed; use align_content/justify_* instead *)

val pp_align_items : align_items Pp.t
(** [pp_align_items] is the pretty-printer for [align_items]. *)

val read_align_items : Cursor.t -> align_items
(** [read_align_items t] is the [align_items] parsed from [t]. *)

val pp_align_self : align_self Pp.t
(** [pp_align_self] is the pretty-printer for [align_self]. *)

val read_align_self : Cursor.t -> align_self
(** [read_align_self t] is the [align_self] parsed from [t]. *)

val pp_justify_content : justify_content Pp.t
(** [pp_justify_content] is the pretty-printer for [justify_content]. *)

val read_justify_content : Cursor.t -> justify_content
(** [read_justify_content t] is the [justify_content] parsed from [t]. *)

val pp_align_content : align_content Pp.t
(** [pp_align_content] is the pretty-printer for [align_content]. *)

val read_align_content : Cursor.t -> align_content
(** [read_align_content t] is the [align_content] parsed from [t]. *)

val pp_justify_items : justify_items Pp.t
(** [pp_justify_items] is the pretty-printer for [justify_items]. *)

val read_justify_items : Cursor.t -> justify_items
(** [read_justify_items t] is the [justify_items] parsed from [t]. *)

val pp_justify_self : justify_self Pp.t
(** [pp_justify_self] is the pretty-printer for [justify_self]. *)

val read_justify_self : Cursor.t -> justify_self
(** [read_justify_self t] is the [justify_self] parsed from [t]. *)

val pp_flex : flex Pp.t
(** [pp_flex] is the pretty-printer for [flex]. *)

val read_flex : Cursor.t -> flex
(** [read_flex t] is the [flex] parsed from [t]. *)

val pp_column_width : column_width Pp.t
(** [pp_column_width] is the pretty-printer for [column_width]. *)

val pp_column_count : column_count Pp.t
(** [pp_column_count] is the pretty-printer for [column_count]. *)

val pp_position_try : position_try Pp.t
(** [pp_position_try] is the pretty-printer for [position_try]. *)

val pp_border_image_repeat : border_image_repeat Pp.t
(** [pp_border_image_repeat] is the pretty-printer for [border_image_repeat]. *)

val pp_border_image_width : border_image_width Pp.t
(** [pp_border_image_width] is the pretty-printer for [border_image_width]. *)

val pp_border_image_outset : border_image_outset Pp.t
(** [pp_border_image_outset] is the pretty-printer for [border_image_outset]. *)

val pp_font_variant_css21 : font_variant_css21 Pp.t
(** [pp_font_variant_css21] is the pretty-printer for [font_variant_css21]. *)

val read_font_variant_css21 : Cursor.t -> font_variant_css21
(** [read_font_variant_css21 t] is the [font_variant_css21] parsed from [t]. *)

val pp_list_style : list_style Pp.t
(** [pp_list_style] is the pretty-printer for [list_style]. *)

val read_list_style : Cursor.t -> list_style
(** [read_list_style t] is the [list_style] shorthand parsed from [t]. *)

val pp_list_style_shorthand : list_style_shorthand Pp.t
(** [pp_list_style_shorthand] is the pretty-printer for [list_style_shorthand].
*)

val read_list_style_shorthand : Cursor.t -> list_style_shorthand
(** [read_list_style_shorthand t] is the [list_style_shorthand] record parsed
    from [t]. *)

val pp_flex_basis : flex_basis Pp.t
(** [pp_flex_basis] pretty-prints a flex-basis value. *)

val read_flex_basis : Cursor.t -> flex_basis
(** [read_flex_basis t] parses a flex-basis value. *)

val pp_place_content : place_content Pp.t
(** [pp_place_content] is the pretty-printer for [place_content]. *)

val read_place_content : Cursor.t -> place_content
(** [read_place_content t] is the [place_content] parsed from [t]. *)

val pp_place_items : place_items Pp.t
(** [pp_place_items] is the pretty-printer for [place_items]. *)

val read_place_items : Cursor.t -> place_items
(** [read_place_items t] is the [place_items] parsed from [t]. *)

val pp_grid_auto_flow_component : grid_auto_flow_component Pp.t
(** [pp_grid_auto_flow_component] is the pretty-printer for one [grid_auto_flow]
    component. *)

val read_grid_auto_flow_component : Cursor.t -> grid_auto_flow_component
(** [read_grid_auto_flow_component t] is one [grid_auto_flow] component parsed
    from [t]. *)

val pp_grid_auto_flow : grid_auto_flow Pp.t
(** [pp_grid_auto_flow] is the pretty-printer for [grid_auto_flow]. *)

val read_grid_auto_flow : Cursor.t -> grid_auto_flow
(** [read_grid_auto_flow t] is the [grid_auto_flow] parsed from [t]. *)

val pp_grid_flex_math : grid_flex_math Pp.t
(** [pp_grid_flex_math] is the pretty-printer for [grid_flex_math]. *)

val read_grid_flex_math : Cursor.t -> grid_flex_math
(** [read_grid_flex_math t] is the [grid_flex_math] parsed from [t]. *)

val pp_grid_template : grid_template Pp.t
(** [pp_grid_template] is the pretty-printer for [grid_template]. *)

val pp_grid_template_areas : grid_template_areas Pp.t
(** [pp_grid_template_areas] is the pretty-printer for [grid_template_areas]. *)

val read_grid_template : Cursor.t -> grid_template
(** [read_grid_template t] is the [grid_template] parsed from [t]. *)

val read_grid_template_tracks : Cursor.t -> grid_template
(** [read_grid_template_tracks t] is the [grid-template-columns] or
    [grid-template-rows] track list parsed from [t]. *)

val read_grid_auto_tracks : Cursor.t -> grid_template
(** [read_grid_auto_tracks t] is the [grid-auto-columns] / [grid-auto-rows]
    value parsed from [t]. CSS Grid 2 (ED) sec. 7.6 gives those properties
    [<track-size>+], so the value shares the [grid_template] type with
    [grid-template-columns] but takes none of its line-name, [repeat()] or slash
    forms. *)

val read_grid : Cursor.t -> grid_template
(** [read_grid t] is the [grid] shorthand parsed from [t]. *)

val pp_grid_line : grid_line Pp.t
(** [pp_grid_line] is the pretty-printer for [grid_line]. *)

val read_grid_line : Cursor.t -> grid_line
(** [read_grid_line t] is the [grid_line] parsed from [t]. *)

val pp_grid_line_pair : grid_line_pair Pp.t
(** [pp_grid_line_pair] is the pretty-printer for [grid_line_pair]. *)

val read_grid_line_pair : Cursor.t -> grid_line_pair
(** [read_grid_line_pair t] parses a grid column/row shorthand value:
    [<grid-line> [ / <grid-line> ]?]. If no slash is present, the second value
    defaults to [Auto]. *)

val read_grid_area : Cursor.t -> grid_area
(** [read_grid_area t] parses a [grid-area] shorthand value as one to four
    grid-line values, applying the spec's defaulting rules so the returned
    record always has all four longhands populated. *)

val pp_grid_area : grid_area Pp.t
(** [pp_grid_area] pretty-prints {!grid_area}, picking the shortest 1-/2-/3-/
    4-value spelling that the spec's defaulting rules round-trip to the same
    record. *)

val pp_aspect_ratio : aspect_ratio Pp.t
(** [pp_aspect_ratio] is the pretty-printer for [aspect_ratio]. *)

val read_aspect_ratio : Cursor.t -> aspect_ratio
(** [read_aspect_ratio t] is the [aspect_ratio] parsed from [t]. *)

val pp_font_style : font_style Pp.t
(** [pp_font_style] is the pretty-printer for [font_style]. *)

val read_font_style : Cursor.t -> font_style
(** [read_font_style t] is the [font_style] parsed from [t]. *)

val pp_font_size : font_size Pp.t
(** [pp_font_size] is the pretty-printer for [font_size]. *)

val read_font_size : Cursor.t -> font_size
(** [read_font_size t] is the [font_size] parsed from [t]. *)

val pp_text_align : text_align Pp.t
(** [pp_text_align] is the pretty-printer for [text_align]. *)

val read_text_align : Cursor.t -> text_align
(** [read_text_align t] is the [text_align] parsed from [t]. *)

val pp_text_decoration : text_decoration Pp.t
(** [pp_text_decoration] is the pretty-printer for [text_decoration]. *)

val read_text_decoration : Cursor.t -> text_decoration
(** [read_text_decoration t] is the [text_decoration] parsed from [t]. *)

val pp_text_decoration_line : text_decoration_line Pp.t
(** [pp_text_decoration_line] pretty-prints a text-decoration-line value. *)

val read_text_decoration_line : Cursor.t -> text_decoration_line
(** [read_text_decoration_line t] parses a text-decoration-line value. *)

val pp_text_decoration_shorthand : text_decoration_shorthand Pp.t
(** [pp_text_decoration_shorthand] pretty-prints a text-decoration shorthand
    value. *)

val read_text_decoration_shorthand : Cursor.t -> text_decoration_shorthand
(** [read_text_decoration_shorthand t] parses a text-decoration shorthand. *)

val pp_text_decoration_style : text_decoration_style Pp.t
(** [pp_text_decoration_style] is the pretty-printer for
    [text_decoration_style]. *)

val read_text_decoration_style : Cursor.t -> text_decoration_style
(** [read_text_decoration_style t] is the [text_decoration_style] parsed from
    [t]. *)

val pp_text_transform : text_transform Pp.t
(** [pp_text_transform] is the pretty-printer for [text_transform]. *)

val read_text_transform : Cursor.t -> text_transform
(** [read_text_transform t] is the [text_transform] parsed from [t]. *)

val pp_text_transform_case : text_transform_case Pp.t
(** [pp_text_transform_case] pretty-prints a text-transform case keyword. *)

val read_text_transform_case : Cursor.t -> text_transform_case
(** [read_text_transform_case t] parses a text-transform case keyword. *)

val pp_text_overflow : text_overflow Pp.t
(** [pp_text_overflow] is the pretty-printer for [text_overflow]. *)

val read_text_overflow : Cursor.t -> text_overflow
(** [read_text_overflow t] is the [text_overflow] parsed from [t]. *)

val pp_text_wrap : text_wrap Pp.t
(** [pp_text_wrap] is the pretty-printer for [text_wrap]. *)

val read_text_wrap : Cursor.t -> text_wrap
(** [read_text_wrap t] is the [text_wrap] parsed from [t]. *)

val pp_text_wrap_mode : text_wrap_mode Pp.t
(** [pp_text_wrap_mode] is the pretty-printer for [text_wrap_mode]. *)

val read_text_wrap_mode : Cursor.t -> text_wrap_mode
(** [read_text_wrap_mode t] is the [text_wrap_mode] parsed from [t]. *)

val pp_text_wrap_style : text_wrap_style Pp.t
(** [pp_text_wrap_style] is the pretty-printer for [text_wrap_style]. *)

val read_text_wrap_style : Cursor.t -> text_wrap_style
(** [read_text_wrap_style t] is the [text_wrap_style] parsed from [t]. *)

val pp_text_box_trim : text_box_trim Pp.t
(** [pp_text_box_trim] is the pretty-printer for [text_box_trim]. *)

val read_text_box_trim : Cursor.t -> text_box_trim
(** [read_text_box_trim t] is the [text_box_trim] parsed from [t]. *)

val pp_text_underline_position : text_underline_position Pp.t
(** [pp_text_underline_position] pretty-prints [text_underline_position]. *)

val read_text_underline_position : Cursor.t -> text_underline_position
(** [read_text_underline_position t] parses [text-underline-position]. *)

val pp_text_box_edge : text_box_edge Pp.t
(** [pp_text_box_edge] is the pretty-printer for [text_box_edge]. *)

val read_text_box_edge : ?global:bool -> Cursor.t -> text_box_edge
(** [read_text_box_edge t] parses [text-box-edge]. *)

val pp_text_box : text_box Pp.t
(** [pp_text_box] is the pretty-printer for [text_box]. *)

val read_text_box : Cursor.t -> text_box
(** [read_text_box t] parses [text-box]. *)

val pp_inline_sizing : inline_sizing Pp.t
(** [pp_inline_sizing] is the pretty-printer for [inline_sizing]. *)

val read_inline_sizing : Cursor.t -> inline_sizing
(** [read_inline_sizing t] parses [inline-sizing]. *)

val pp_line_fit_edge : line_fit_edge Pp.t
(** [pp_line_fit_edge] is the pretty-printer for [line_fit_edge]. *)

val read_line_fit_edge : Cursor.t -> line_fit_edge
(** [read_line_fit_edge t] parses [line-fit-edge]. *)

val pp_interpolate_size : interpolate_size Pp.t
(** [pp_interpolate_size] is the pretty-printer for [interpolate_size]. *)

val read_interpolate_size : Cursor.t -> interpolate_size
(** [read_interpolate_size t] parses [interpolate-size]. *)

val pp_min_intrinsic_sizing : min_intrinsic_sizing Pp.t
(** [pp_min_intrinsic_sizing] pretty-prints [min_intrinsic_sizing]. *)

val read_min_intrinsic_sizing : Cursor.t -> min_intrinsic_sizing
(** [read_min_intrinsic_sizing t] parses [min-intrinsic-sizing]. *)

val pp_ruby_align : ruby_align Pp.t
(** [pp_ruby_align] is the pretty-printer for [ruby_align]. *)

val read_ruby_align : Cursor.t -> ruby_align
(** [read_ruby_align t] parses [ruby-align]. *)

val pp_ruby_merge : ruby_merge Pp.t
(** [pp_ruby_merge] is the pretty-printer for [ruby_merge]. *)

val read_ruby_merge : Cursor.t -> ruby_merge
(** [read_ruby_merge t] parses [ruby-merge]. *)

val pp_ruby_overhang : ruby_overhang Pp.t
(** [pp_ruby_overhang] is the pretty-printer for [ruby_overhang]. *)

val read_ruby_overhang : Cursor.t -> ruby_overhang
(** [read_ruby_overhang t] parses [ruby-overhang]. *)

val pp_ruby_position : ruby_position Pp.t
(** [pp_ruby_position] is the pretty-printer for [ruby_position]. *)

val read_ruby_position : Cursor.t -> ruby_position
(** [read_ruby_position t] parses [ruby-position]. *)

val pp_glyph_orientation_vertical : glyph_orientation_vertical Pp.t
(** [pp_glyph_orientation_vertical] pretty-prints [glyph_orientation_vertical].
*)

val read_glyph_orientation_vertical : Cursor.t -> glyph_orientation_vertical
(** [read_glyph_orientation_vertical t] parses [glyph-orientation-vertical]. *)

val pp_text_spacing_trim : text_spacing_trim Pp.t
(** [pp_text_spacing_trim] is the pretty-printer for [text_spacing_trim]. *)

val read_text_spacing_trim : Cursor.t -> text_spacing_trim
(** [read_text_spacing_trim t] is the [text_spacing_trim] parsed from [t]. *)

val pp_hyphenate_limit_chars : hyphenate_limit_chars Pp.t
(** [pp_hyphenate_limit_chars] pretty-prints a [hyphenate_limit_chars]. *)

val read_hyphenate_limit_chars : Cursor.t -> hyphenate_limit_chars
(** [read_hyphenate_limit_chars t] parses a [hyphenate_limit_chars]. *)

val pp_initial_letter : initial_letter Pp.t
(** [pp_initial_letter] pretty-prints an [initial_letter]. *)

val pp_text_size_adjust : text_size_adjust Pp.t
(** [pp_text_size_adjust] pretty-prints a text-size-adjust value. *)

val pp_white_space : white_space Pp.t
(** [pp_white_space] is the pretty-printer for [white_space]. *)

val pp_white_space_collapse : white_space_collapse Pp.t
(** [pp_white_space_collapse] is the pretty-printer for [white_space_collapse].
*)

val read_white_space_collapse : Cursor.t -> white_space_collapse
(** [read_white_space_collapse t] is the [white_space_collapse] parsed from [t].
*)

val read_white_space : Cursor.t -> white_space
(** [read_white_space t] is the [white_space] parsed from [t]. *)

val pp_word_break : word_break Pp.t
(** [pp_word_break] is the pretty-printer for [word_break]. *)

val read_word_break : Cursor.t -> word_break
(** [read_word_break t] is the [word_break] parsed from [t]. *)

val pp_overflow_wrap : overflow_wrap Pp.t
(** [pp_overflow_wrap] is the pretty-printer for [overflow_wrap]. *)

val read_overflow_wrap : Cursor.t -> overflow_wrap
(** [read_overflow_wrap t] is the [overflow_wrap] parsed from [t]. *)

val pp_hyphens : hyphens Pp.t
(** [pp_hyphens] is the pretty-printer for [hyphens]. *)

val read_hyphens : Cursor.t -> hyphens
(** [read_hyphens t] is the [hyphens] parsed from [t]. *)

val pp_list_style_type : list_style_type Pp.t
(** [pp_list_style_type] is the pretty-printer for [list_style_type]. *)

val read_list_style_type : Cursor.t -> list_style_type
(** [read_list_style_type t] is the [list_style_type] parsed from [t]. *)

val pp_symbols_type : symbols_type Pp.t
(** [pp_symbols_type] pretty-prints a [symbols()] system type. *)

val read_symbols_type : Cursor.t -> symbols_type
(** [read_symbols_type t] parses a [symbols()] system type. *)

val pp_list_style_symbol : list_style_symbol Pp.t
(** [pp_list_style_symbol] pretty-prints one [symbols()] symbol. *)

val read_list_style_symbol : Cursor.t -> list_style_symbol
(** [read_list_style_symbol t] parses one [symbols()] symbol. *)

val pp_list_style_position : list_style_position Pp.t
(** [pp_list_style_position] is the pretty-printer for [list_style_position]. *)

val read_list_style_position : Cursor.t -> list_style_position
(** [read_list_style_position t] is the [list_style_position] parsed from [t].
*)

val pp_list_style_image : list_style_image Pp.t
(** [pp_list_style_image] is the pretty-printer for [list_style_image]. *)

val read_list_style_image : Cursor.t -> list_style_image
(** [read_list_style_image t] is the [list_style_image] parsed from [t]. *)

val pp_table_layout : table_layout Pp.t
(** [pp_table_layout] is the pretty-printer for [table_layout]. *)

val read_table_layout : Cursor.t -> table_layout
(** [read_table_layout t] is the [table_layout] parsed from [t]. *)

val pp_vertical_align : vertical_align Pp.t
(** [pp_vertical_align] is the pretty-printer for [vertical_align]. *)

val read_vertical_align : Cursor.t -> vertical_align
(** [read_vertical_align t] is the [vertical_align] parsed from [t]. *)

val pp_border_collapse : border_collapse Pp.t
(** [pp_border_collapse] is the pretty-printer for [border_collapse]. *)

val read_border_collapse : Cursor.t -> border_collapse
(** [read_border_collapse t] is the [border_collapse] parsed from [t]. *)

val pp_outline_style : outline_style Pp.t
(** [pp_outline_style] is the pretty-printer for [outline_style]. *)

val read_outline_style : Cursor.t -> outline_style
(** [read_outline_style t] is the [outline_style] parsed from [t]. *)

val pp_outline : outline Pp.t
(** [pp_outline] is the pretty-printer for [outline]. *)

val read_outline : Cursor.t -> outline
(** [read_outline t] is the [outline] parsed from [t]. *)

val pp_font_family : font_family Pp.t
(** [pp_font_family] is the pretty-printer for [font_family]. *)

val pp_font_family_name : font_family Pp.t
(** [pp_font_family_name] is the pretty-printer for a descriptor
    [<font-family-name>]. *)

val read_font_family : Cursor.t -> font_family
(** [read_font_family t] is the [font_family] parsed from [t]. *)

val read_font_family_name : Cursor.t -> font_family
(** [read_font_family_name t] is the descriptor [<font-family-name>] parsed from
    [t]. Unquoted generic-family and CSS-wide keywords are rejected. *)

val is_font_family_name_value : font_family -> bool
(** [is_font_family_name_value family] is true when [family] can fill a
    descriptor [<font-family-name>] slot. *)

val pp_font : font Pp.t
(** [pp_font] is the pretty-printer for the [font] shorthand. *)

val read_font : Cursor.t -> font
(** [read_font t] is the [font] shorthand parsed from [t]. *)

val pp_font_shorthand : font_shorthand Pp.t
(** [pp_font_shorthand] is the pretty-printer for the [font_shorthand] record.
*)

val read_font_shorthand : Cursor.t -> font_shorthand
(** [read_font_shorthand t] is the [font_shorthand] record (the
    [<size>[/<line-height>]? <family>+] body with optional prefixes) parsed from
    [t]. *)

val pp_font_src : Font_face.src Pp.t
(** [pp_font_src] is the pretty-printer for [@font-face] [src] values. *)

val read_font_src : Cursor.t -> Font_face.src
(** [read_font_src t] is the [@font-face] [src] value parsed from [t]. *)

val pp_font_stretch : font_stretch Pp.t
(** [pp_font_stretch] is the pretty-printer for [font_stretch]. *)

val read_font_stretch : Cursor.t -> font_stretch
(** [read_font_stretch t] is the [font_stretch] parsed from [t]. *)

val pp_font_display : font_display Pp.t
(** [pp_font_display] is the pretty-printer for [font_display]. *)

val read_font_display : Cursor.t -> font_display
(** [read_font_display t] is the [font_display] parsed from [t]. *)

val pp_unicode_range : unicode_range Pp.t
(** [pp_unicode_range] is the pretty-printer for [unicode_range]. *)

val read_unicode_range : Cursor.t -> unicode_range
(** [read_unicode_range t] is the [unicode_range] parsed from [t]. *)

val pp_font_variant_numeric_token : font_variant_numeric_token Pp.t
(** [pp_font_variant_numeric_token] is the pretty-printer for
    [font_variant_numeric_token]. *)

val read_font_variant_numeric_token : Cursor.t -> font_variant_numeric_token
(** [read_font_variant_numeric_token t] is the [font_variant_numeric_token]
    parsed from [t]. *)

val pp_font_variant_numeric : font_variant_numeric Pp.t
(** [pp_font_variant_numeric] is the pretty-printer for [font_variant_numeric].
*)

val read_font_variant_numeric : Cursor.t -> font_variant_numeric
(** [read_font_variant_numeric t] is the [font_variant_numeric] parsed from [t].
*)

val pp_font_feature_value : font_feature_value Pp.t
(** [pp_font_feature_value] is the pretty-printer for [font_feature_value]. *)

val read_font_feature_value : Cursor.t -> font_feature_value
(** [read_font_feature_value t] is the [font_feature_value] parsed from [t]. *)

val pp_font_feature_setting : font_feature_setting Pp.t
(** [pp_font_feature_setting] is the pretty-printer for [font_feature_setting].
*)

val read_font_feature_setting : Cursor.t -> font_feature_setting
(** [read_font_feature_setting t] is the [font_feature_setting] parsed from [t].
*)

val pp_font_feature_settings : font_feature_settings Pp.t
(** [pp_font_feature_settings] is the pretty-printer for
    [font_feature_settings]. *)

val read_font_feature_settings : Cursor.t -> font_feature_settings
(** [read_font_feature_settings t] is the [font_feature_settings] parsed from
    [t]. *)

val pp_font_variation_setting : font_variation_setting Pp.t
(** [pp_font_variation_setting] is the pretty-printer for
    [font_variation_setting]. *)

val read_font_variation_setting : Cursor.t -> font_variation_setting
(** [read_font_variation_setting t] is the [font_variation_setting] parsed from
    [t]. *)

val pp_font_variation_settings : font_variation_settings Pp.t
(** [pp_font_variation_settings] is the pretty-printer for
    [font_variation_settings]. *)

val read_font_variation_settings : Cursor.t -> font_variation_settings
(** [read_font_variation_settings t] is the [font_variation_settings] parsed
    from [t]. *)

val pp_transform : transform Pp.t
(** [pp_transform] is the pretty-printer for [transform]. *)

val pp_transforms : transform list Pp.t
(** [pp_transforms] is the pretty-printer for [transform list]. *)

val read_transform : Cursor.t -> transform
(** [read_transform t] is the [transform] parsed from [t]. *)

val read_transforms : Cursor.t -> transform list
(** [read_transforms t] is the [transform list] parsed from [t]. *)

val read_transform_origin : Cursor.t -> transform_origin
(** [read_transform_origin t] is the [transform_origin] parsed from [t]. *)

val pp_transform_box : transform_box Pp.t
(** [pp_transform_box] is the pretty-printer for [transform_box]. *)

val read_transform_box : Cursor.t -> transform_box
(** [read_transform_box t] is the [transform_box] parsed from [t]. *)

val pp_transform_style : transform_style Pp.t
(** [pp_transform_style] is the pretty-printer for [transform_style]. *)

val read_transform_style : Cursor.t -> transform_style
(** [read_transform_style t] is the [transform_style] parsed from [t]. *)

val pp_backface_visibility : backface_visibility Pp.t
(** [pp_backface_visibility] is the pretty-printer for [backface_visibility]. *)

val read_backface_visibility : Cursor.t -> backface_visibility
(** [read_backface_visibility t] is the [backface_visibility] parsed from [t].
*)

val pp_scale : scale Pp.t
(** [pp_scale] is the pretty-printer for [scale]. *)

val read_scale : Cursor.t -> scale
(** [read_scale t] is the [scale] parsed from [t]. *)

val pp_steps_direction : steps_direction Pp.t
(** [pp_steps_direction] is the pretty-printer for [steps_direction]. *)

val pp_timing_function : timing_function Pp.t
(** [pp_timing_function] is the pretty-printer for [timing_function]. *)

val read_steps_direction : Cursor.t -> steps_direction
(** [read_steps_direction t] is the [steps_direction] parsed from [t]. *)

val read_timing_function : Cursor.t -> timing_function
(** [read_timing_function t] is the [timing_function] parsed from [t]. *)

val read_timing_function_list : Cursor.t -> timing_function
(** [read_timing_function_list t] is a comma-separated list of timing functions;
    folds to a single value when there is one entry, [Timing_functions]
    otherwise. *)

val read_duration_list : (Cursor.t -> duration) -> Cursor.t -> duration
(** [read_duration_list read_one t] is a comma-separated list of durations using
    [read_one]; folds to a single value when the list has one entry, [Durations]
    otherwise. *)

val pp_transition_property_value : transition_property_value Pp.t
(** [pp_transition_property_value] is the pretty-printer for
    [transition_property_value]. *)

val read_transition_property_value : Cursor.t -> transition_property_value
(** [read_transition_property_value t] is the [transition_property_value] parsed
    from [t]. *)

val pp_transition_property : transition_property Pp.t
(** [pp_transition_property] is the pretty-printer for [transition_property]. *)

val read_transition_property : Cursor.t -> transition_property
(** [read_transition_property t] is the [transition_property] parsed from [t].
*)

val pp_transition_behavior : transition_behavior Pp.t
(** [pp_transition_behavior] is the pretty-printer for [transition_behavior]. *)

val read_transition_behavior : Cursor.t -> transition_behavior
(** [read_transition_behavior t] is the [transition_behavior] parsed from [t].
*)

val pp_overlay : overlay Pp.t
(** [pp_overlay] is the pretty-printer for [overlay]. *)

val read_overlay : Cursor.t -> overlay
(** [read_overlay t] parses [overlay]. *)

val pp_transition_shorthand : transition_shorthand Pp.t
(** [pp_transition_shorthand] is the pretty-printer for
    {!val-transition_shorthand}. *)

val read_transition_shorthand : Cursor.t -> transition_shorthand
(** [read_transition_shorthand t] is the {!val-transition_shorthand} parsed from
    [t]. *)

val pp_transition : transition Pp.t
(** [pp_transition] is the pretty-printer for [transition]. *)

val read_transition : Cursor.t -> transition
(** [read_transition t] is the [transition] parsed from [t]. *)

val read_transitions : Cursor.t -> transition list
(** [read_transitions t] parses a comma-separated list of [transition]s. *)

val pp_animation_direction : animation_direction Pp.t
(** [pp_animation_direction] is the pretty-printer for [animation_direction]. *)

val read_animation_direction : Cursor.t -> animation_direction
(** [read_animation_direction t] is the [animation_direction] parsed from [t].
*)

val pp_animation_fill_mode : animation_fill_mode Pp.t
(** [pp_animation_fill_mode] is the pretty-printer for [animation_fill_mode]. *)

val read_animation_fill_mode : Cursor.t -> animation_fill_mode
(** [read_animation_fill_mode t] is the [animation_fill_mode] parsed from [t].
*)

val pp_animation_iteration_count : animation_iteration_count Pp.t
(** [pp_animation_iteration_count] is the pretty-printer for
    [animation_iteration_count]. *)

val read_animation_iteration_count : Cursor.t -> animation_iteration_count
(** [read_animation_iteration_count t] is the [animation_iteration_count] parsed
    from [t]. *)

val pp_animation_name : animation_name Pp.t
(** [pp_animation_name] is the pretty-printer for [animation_name]. *)

val read_animation_name : Cursor.t -> animation_name
(** [read_animation_name t] is the [animation_name] parsed from [t]. *)

val pp_animation_play_state : animation_play_state Pp.t
(** [pp_animation_play_state] is the pretty-printer for [animation_play_state].
*)

val read_animation_play_state : Cursor.t -> animation_play_state
(** [read_animation_play_state t] is the [animation_play_state] parsed from [t].
*)

val pp_animation_composition_item : animation_composition_item Pp.t
(** [pp_animation_composition_item] is the pretty-printer for
    [animation_composition_item]. *)

val read_animation_composition_item : Cursor.t -> animation_composition_item
(** [read_animation_composition_item t] is the [animation_composition_item]
    parsed from [t]. *)

val pp_animation_composition : animation_composition Pp.t
(** [pp_animation_composition] is the pretty-printer for
    [animation_composition]. *)

val read_animation_composition : Cursor.t -> animation_composition
(** [read_animation_composition t] is the [animation_composition] parsed from
    [t]. *)

val pp_animation_shorthand : animation_shorthand Pp.t
(** [pp_animation_shorthand] is the pretty-printer for
    {!val-animation_shorthand}. *)

val read_animation_shorthand : Cursor.t -> animation_shorthand
(** [read_animation_shorthand t] is the {!val-animation_shorthand} parsed from
    [t]. *)

val pp_animation : animation Pp.t
(** [pp_animation] is the pretty-printer for [animation]. *)

val read_animation : Cursor.t -> animation
(** [read_animation t] is the [animation] parsed from [t]. *)

val read_animations : Cursor.t -> animation list
(** [read_animations t] parses a comma-separated list of [animation]s. *)

val pp_blend_mode : blend_mode Pp.t
(** [pp_blend_mode] is the pretty-printer for [blend_mode]. *)

val read_blend_mode : Cursor.t -> blend_mode
(** [read_blend_mode t] is the [blend_mode] parsed from [t]. *)

val pp_position_value : position_value Pp.t
(** [pp_position_value] pretty-prints a 2D position. Special case:
    [Center, Center] prints as "center". *)

val pp_transform_origin : transform_origin Pp.t
(** [pp_transform_origin] pretty-prints a transform-origin value. *)

val origin : length -> length -> transform_origin
(** [origin x y] transform-origin helper for 2D positions. *)

val origin3d : length -> length -> length -> transform_origin
(** [origin3d x y z] transform-origin helper for 3D positions. *)

val pp_text_shadow : text_shadow Pp.t
(** [pp_text_shadow] is the pretty-printer for [text_shadow]. *)

val read_text_shadow : Cursor.t -> text_shadow
(** [read_text_shadow t] is the [text_shadow] parsed from [t]. *)

val read_text_shadows : Cursor.t -> text_shadow list
(** [read_text_shadows t] parses a comma-separated list of [text_shadow]s. *)

val pp_translate_value : translate_value Pp.t
(** [pp_translate_value] is the pretty-printer for [translate_value]. *)

val read_translate_value : Cursor.t -> translate_value
(** [read_translate_value t] is the [translate_value] parsed from [t]. *)

val pp_rotate_value : rotate_value Pp.t
(** [pp_rotate_value] is the pretty-printer for [rotate_value]. *)

val read_rotate_value : Cursor.t -> rotate_value
(** [read_rotate_value t] is the [rotate_value] parsed from [t]. *)

val pp_filter_function : filter_function Pp.t
(** [pp_filter_function] prints the name of a filter function with an optional
    argument, without parentheses. *)

val read_filter_function : Cursor.t -> filter_function
(** [read_filter_function t] reads the name of a filter function with an
    optional argument. [drop-shadow] is excluded because its arguments are
    mandatory. *)

val pp_filter : filter Pp.t
(** [pp_filter] is the pretty-printer for [filter]. *)

val read_filter : Cursor.t -> filter
(** [read_filter t] is the [filter] parsed from [t]. *)

val pp_opacity : opacity Pp.t
(** [pp_opacity] is the pretty-printer for [opacity]. *)

val read_opacity : Cursor.t -> opacity
(** [read_opacity t] is the [opacity] parsed from [t]. *)

val pp_shape_image_threshold : shape_image_threshold Pp.t
(** [pp_shape_image_threshold] is the pretty-printer for
    [shape_image_threshold]. *)

val read_shape_image_threshold : Cursor.t -> shape_image_threshold
(** [read_shape_image_threshold t] parses [shape_image_threshold]. *)

val pp_overflow_clip_margin : overflow_clip_margin Pp.t
(** [pp_overflow_clip_margin] is the pretty-printer for [overflow_clip_margin].
*)

val read_overflow_clip_margin : Cursor.t -> overflow_clip_margin
(** [read_overflow_clip_margin t] parses [overflow_clip_margin]. *)

val pp_background_attachment : background_attachment Pp.t
(** [pp_background_attachment] is the pretty-printer for
    [background_attachment]. *)

val read_background_attachment : Cursor.t -> background_attachment
(** [read_background_attachment t] is the [background_attachment] parsed from
    [t]. *)

val pp_background_repeat : background_repeat Pp.t
(** [pp_background_repeat] is the pretty-printer for [background_repeat]. *)

val read_background_repeat : Cursor.t -> background_repeat
(** [read_background_repeat t] is the single-layer [background_repeat] parsed
    from [t] (used by the [background] / [mask] shorthand). *)

val read_background_repeat_list : Cursor.t -> background_repeat
(** [read_background_repeat_list t] parses the standalone [background-repeat] /
    [mask-repeat] longhand: a comma-separated layer list. *)

val pp_background_size : background_size Pp.t
(** [pp_background_size] is the pretty-printer for [background_size]. *)

val read_background_size : Cursor.t -> background_size
(** [read_background_size t] is the single-layer [background_size] parsed from
    [t] (used by the shorthand). *)

val read_background_size_list : Cursor.t -> background_size
(** [read_background_size_list t] parses the standalone size longhand: a
    comma-separated layer list. *)

val pp_gradient_direction : gradient_direction Pp.t
(** [pp_gradient_direction] is the pretty-printer for [gradient_direction]. *)

val read_gradient_direction : Cursor.t -> gradient_direction
(** [read_gradient_direction t] is the [gradient_direction] parsed from [t]. *)

val read_gradient_prelude : Cursor.t -> gradient_direction
(** [read_gradient_prelude t] parses a gradient direction including the optional
    [in <color-interpolation>] tail ([45deg in oklab]), unlike
    {!read_gradient_direction} which stops before the tail. *)

val pp_color_interpolation : color_interpolation Pp.t
(** [pp_color_interpolation] pretty-prints a color interpolation space (e.g.,
    "in oklab"). *)

val read_color_interpolation : Cursor.t -> color_interpolation
(** [read_color_interpolation t] parses a color interpolation space starting
    with the keyword "in" (e.g., "in oklab"). *)

val pp_hue_interpolation_method : hue_interpolation_method Pp.t
(** [pp_hue_interpolation_method] pretty-prints a hue interpolation method. *)

val read_hue_interpolation_method : Cursor.t -> hue_interpolation_method option
(** [read_hue_interpolation_method t] parses a trailing hue interpolation method
    such as ["shorter hue"], returning [None] when no method is present. *)

val pp_radial_shape : radial_shape Pp.t
(** [pp_radial_shape] is the pretty-printer for [radial_shape]. *)

val read_radial_shape : Cursor.t -> radial_shape
(** [read_radial_shape t] is the [radial_shape] parsed from [t]. *)

val pp_radial_size : radial_size Pp.t
(** [pp_radial_size] is the pretty-printer for [radial_size]. *)

val read_radial_size : Cursor.t -> radial_size
(** [read_radial_size t] is the [radial_size] parsed from [t]. *)

val pp_radial_gradient_config : radial_gradient_config Pp.t
(** [pp_radial_gradient_config] is the pretty-printer for
    [radial_gradient_config]. *)

val read_radial_gradient_config : Cursor.t -> radial_gradient_config
(** [read_radial_gradient_config t] is the [radial_gradient_config] parsed from
    [t]. *)

val pp_conic_gradient_config : conic_gradient_config Pp.t
(** [pp_conic_gradient_config] pretty-prints a conic-gradient prefix
    configuration. *)

val read_conic_gradient_config : Cursor.t -> conic_gradient_config
(** [read_conic_gradient_config t] parses a conic-gradient prefix configuration.
*)

val pp_gradient_position : gradient_position Pp.t
(** [pp_gradient_position] pretty-prints a gradient prelude/position value. *)

val read_gradient_position : Cursor.t -> gradient_position
(** [read_gradient_position t] parses a linear, radial, or conic gradient
    prelude/position value. *)

val pp_gradient_stop : gradient_stop Pp.t
(** [pp_gradient_stop] is the pretty-printer for [gradient_stop]. *)

val read_gradient_stop : Cursor.t -> gradient_stop
(** [read_gradient_stop t] is the [gradient_stop] parsed from [t]. *)

val read_gradient_stop_list : Cursor.t -> gradient_stop
(** [read_gradient_stop_list t] parses a comma-separated [gradient_stop] list.
*)

val pp_background_image : background_image Pp.t
(** [pp_background_image] is the pretty-printer for [background_image]. *)

val read_background_image : Cursor.t -> background_image
(** [read_background_image t] is the [background_image] parsed from [t]. *)

val read_background_images : Cursor.t -> background_image list
(** [read_background_images t] parses a comma-separated list of
    [background_image]s. *)

val minify_background_image : background_image -> background_image
(** [minify_background_image img] converts named colors in gradient stops to
    their shortest hex form, matching Lightning CSS behavior. *)

val read_background_box : Cursor.t -> background_box
(** [read_background_box t] parses a single-layer background-clip or
    background-origin value (used by the shorthand). *)

val read_background_box_list : Cursor.t -> background_box
(** [read_background_box_list t] parses the standalone background-clip /
    background-origin longhand: a comma-separated layer list. *)

val pp_background_box : background_box Pp.t
(** [pp_background_box] pretty-prints a background-clip or background-origin
    value. *)

val read_webkit_mask_composite : Cursor.t -> webkit_mask_composite
(** [read_webkit_mask_composite t] parses a webkit-mask-composite value. *)

val pp_webkit_mask_composite : webkit_mask_composite Pp.t
(** [pp_webkit_mask_composite] pretty-prints a webkit-mask-composite value. *)

val read_mask_composite : Cursor.t -> mask_composite
(** [read_mask_composite t] parses a single-layer standard mask-composite value
    (used by the shorthand). *)

val read_mask_composite_list : Cursor.t -> mask_composite
(** [read_mask_composite_list t] parses the standalone mask-composite longhand:
    a comma-separated layer list. *)

val pp_mask_composite : mask_composite Pp.t
(** [pp_mask_composite] pretty-prints a standard mask-composite value. *)

val read_webkit_mask_source_type : Cursor.t -> webkit_mask_source_type
(** [read_webkit_mask_source_type t] parses a webkit-mask-source-type value. *)

val pp_webkit_mask_source_type : webkit_mask_source_type Pp.t
(** [pp_webkit_mask_source_type] pretty-prints a webkit-mask-source-type value.
*)

val read_mask_mode : Cursor.t -> mask_mode
(** [read_mask_mode t] parses a standard mask-mode value. *)

val pp_mask_mode : mask_mode Pp.t
(** [pp_mask_mode] pretty-prints a standard mask-mode value. *)

val read_mask_type : Cursor.t -> mask_type
(** [read_mask_type t] parses a mask-type value (alpha/luminance). *)

val pp_mask_type : mask_type Pp.t
(** [pp_mask_type] pretty-prints a mask-type value. *)

val read_mask_box : Cursor.t -> mask_box
(** [read_mask_box t] parses a single-layer mask-clip or mask-origin value (used
    by the shorthand). *)

val read_mask_box_list : Cursor.t -> mask_box
(** [read_mask_box_list t] parses the standalone mask-clip / mask-origin
    longhand: a comma-separated layer list. *)

val pp_mask_box : mask_box Pp.t
(** [pp_mask_box] pretty-prints a mask-clip or mask-origin value. *)

val read_mask : Cursor.t -> mask
(** [read_mask t] parses a mask shorthand value. *)

val pp_mask : mask Pp.t
(** [pp_mask] pretty-prints a mask shorthand value. *)

val read_background_shorthand : Cursor.t -> background_shorthand
(** [read_background_shorthand t] parses a background shorthand property. *)

val pp_background_shorthand : background_shorthand Pp.t
(** [pp_background_shorthand] pretty-prints a background shorthand value. *)

val read_background : Cursor.t -> background
(** [read_background t] parses a background property. *)

val read_backgrounds : Cursor.t -> background list
(** [read_backgrounds t] parses a comma-separated list of background properties.
*)

val pp_background : background Pp.t
(** [pp_background] pretty-prints a background value. *)

val pp_border_radius : border_radius Pp.t
(** [pp_border_radius] pretty-prints a border-radius shorthand value. *)

val read_border_radius : Cursor.t -> border_radius
(** [read_border_radius t] parses a border-radius shorthand value. *)

val read_gap : Cursor.t -> gap
(** [read_gap t] parses a gap shorthand property (one or two length values). *)

val pp_gap : gap Pp.t
(** [pp_gap] pretty-prints a gap shorthand value. *)

val pp_cursor : cursor Pp.t
(** [pp_cursor] is the pretty-printer for [cursor]. *)

val read_cursor : Cursor.t -> cursor
(** [read_cursor t] is the [cursor] parsed from [t]. *)

val pp_interactivity : interactivity Pp.t
(** [pp_interactivity] is the pretty-printer for [interactivity]. *)

val read_interactivity : Cursor.t -> interactivity
(** [read_interactivity t] is the [interactivity] parsed from [t]. *)

val pp_caret_animation : caret_animation Pp.t
(** [pp_caret_animation] is the pretty-printer for [caret_animation]. *)

val read_caret_animation : Cursor.t -> caret_animation
(** [read_caret_animation t] is the [caret_animation] parsed from [t]. *)

val pp_caret_shape : caret_shape Pp.t
(** [pp_caret_shape] is the pretty-printer for [caret_shape]. *)

val read_caret_shape : Cursor.t -> caret_shape
(** [read_caret_shape t] is the [caret_shape] parsed from [t]. *)

val pp_caret : caret Pp.t
(** [pp_caret] is the pretty-printer for [caret]. *)

val read_caret : Cursor.t -> caret
(** [read_caret t] is the [caret] parsed from [t]. *)

val pp_interest_delay : interest_delay Pp.t
(** [pp_interest_delay] is the pretty-printer for [interest_delay]. *)

val read_interest_delay : ?longhand:bool -> Cursor.t -> interest_delay
(** [read_interest_delay t] is the [interest_delay] parsed from [t]. *)

val pp_nav_scope : nav_scope Pp.t
(** [pp_nav_scope] is the pretty-printer for [nav_scope]. *)

val read_nav_scope : Cursor.t -> nav_scope
(** [read_nav_scope t] is the [nav_scope] parsed from [t]. *)

val pp_nav : nav Pp.t
(** [pp_nav] is the pretty-printer for [nav]. *)

val read_nav : Cursor.t -> nav
(** [read_nav t] is the [nav] parsed from [t]. *)

val pp_user_select : user_select Pp.t
(** [pp_user_select] is the pretty-printer for [user_select]. *)

val read_user_select : Cursor.t -> user_select
(** [read_user_select t] is the [user_select] parsed from [t]. *)

val pp_pointer_events : pointer_events Pp.t
(** [pp_pointer_events] is the pretty-printer for [pointer_events]. *)

val read_pointer_events : Cursor.t -> pointer_events
(** [read_pointer_events t] is the [pointer_events] parsed from [t]. *)

val pp_touch_action : touch_action Pp.t
(** [pp_touch_action] is the pretty-printer for [touch_action]. *)

val read_touch_action : Cursor.t -> touch_action
(** [read_touch_action t] is the [touch_action] parsed from [t]. *)

val pp_resize : resize Pp.t
(** [pp_resize] is the pretty-printer for [resize]. *)

val read_resize : Cursor.t -> resize
(** [read_resize t] is the [resize] parsed from [t]. *)

val pp_box_sizing : box_sizing Pp.t
(** [pp_box_sizing] is the pretty-printer for [box_sizing]. *)

val read_box_sizing : Cursor.t -> box_sizing
(** [read_box_sizing t] is the [box_sizing] parsed from [t]. *)

val pp_field_sizing : field_sizing Pp.t
(** [pp_field_sizing] is the pretty-printer for [field_sizing]. *)

val read_field_sizing : Cursor.t -> field_sizing
(** [read_field_sizing t] is the [field_sizing] parsed from [t]. *)

val pp_caption_side : caption_side Pp.t
(** [pp_caption_side] is the pretty-printer for [caption_side]. *)

val read_caption_side : Cursor.t -> caption_side
(** [read_caption_side t] is the [caption_side] parsed from [t]. *)

val pp_object_fit : object_fit Pp.t
(** [pp_object_fit] is the pretty-printer for [object_fit]. *)

val read_object_fit : Cursor.t -> object_fit
(** [read_object_fit t] is the [object_fit] parsed from [t]. *)

val pp_object_view_box : object_view_box Pp.t
(** [pp_object_view_box] is the pretty-printer for [object_view_box]. *)

val read_object_view_box : Cursor.t -> object_view_box
(** [read_object_view_box t] is the [object_view_box] parsed from [t]. *)

val read_position_value : Cursor.t -> position_value
(** [read_position_value t] is the [position_value] parsed from [t]. *)

val pp_background_position : background_position Pp.t
(** [pp_background_position] is the pretty-printer for [background_position]. *)

val pp_background_position_axis : background_position_axis Pp.t
(** [pp_background_position_axis] is the pretty-printer for one axis of
    [background-position]. *)

val read_background_position_x : Cursor.t -> background_position_axis
(** [read_background_position_x t] is the [background-position-x] value parsed
    from [t]. *)

val read_background_position_y : Cursor.t -> background_position_axis
(** [read_background_position_y t] is the [background-position-y] value parsed
    from [t]. *)

val read_background_position : Cursor.t -> background_position
(** [read_background_position t] is the [background_position] parsed from [t].
*)

val pp_content : content Pp.t
(** [pp_content] is the pretty-printer for [content]. *)

val read_content : Cursor.t -> content
(** [read_content t] is the [content] parsed from [t]. *)

val pp_counter_set : counter_set Pp.t
(** [pp_counter_set] is the pretty-printer for [counter_set]. *)

val read_counter_set : Cursor.t -> counter_set
(** [read_counter_set t] is the [counter_set] parsed from [t]. *)

val pp_content_visibility : content_visibility Pp.t
(** [pp_content_visibility] is the pretty-printer for [content_visibility]. *)

val read_content_visibility : Cursor.t -> content_visibility
(** [read_content_visibility t] is the [content_visibility] parsed from [t]. *)

val pp_quotes : quotes Pp.t
(** [pp_quotes] is the pretty-printer for [quotes]. *)

val read_quotes : Cursor.t -> quotes
(** [read_quotes t] is the [quotes] parsed from [t]. *)

val pp_container_type : container_type Pp.t
(** [pp_container_type] is the pretty-printer for [container_type]. *)

val read_container_type : Cursor.t -> container_type
(** [read_container_type t] is the [container_type] parsed from [t]. *)

val pp_container_name : container_name Pp.t
(** [pp_container_name] is the pretty-printer for [container_name]. *)

val read_container_name : Cursor.t -> container_name
(** [read_container_name t] is the [container_name] parsed from [t]. *)

val pp_anchor_name : anchor_name Pp.t
(** [pp_anchor_name] is the pretty-printer for [anchor_name]. *)

val read_anchor_name : Cursor.t -> anchor_name
(** [read_anchor_name t] is the [anchor_name] parsed from [t]. *)

val pp_position_anchor : position_anchor Pp.t
(** [pp_position_anchor] is the pretty-printer for [position_anchor]. *)

val read_position_anchor : Cursor.t -> position_anchor
(** [read_position_anchor t] is the [position_anchor] parsed from [t]. *)

val pp_overflow_anchor : overflow_anchor Pp.t
(** [pp_overflow_anchor] is the pretty-printer for [overflow_anchor]. *)

val read_overflow_anchor : Cursor.t -> overflow_anchor
(** [read_overflow_anchor t] is the [overflow_anchor] parsed from [t]. *)

val pp_scrollbar_width : scrollbar_width Pp.t
(** [pp_scrollbar_width] is the pretty-printer for [scrollbar_width]. *)

val read_scrollbar_width : Cursor.t -> scrollbar_width
(** [read_scrollbar_width t] is the [scrollbar_width] parsed from [t]. *)

val pp_scrollbar_color : scrollbar_color Pp.t
(** [pp_scrollbar_color] is the pretty-printer for [scrollbar_color]. *)

val read_scrollbar_color : Cursor.t -> scrollbar_color
(** [read_scrollbar_color t] is the [scrollbar_color] parsed from [t]. *)

val pp_scrollbar_gutter : scrollbar_gutter Pp.t
(** [pp_scrollbar_gutter] is the pretty-printer for [scrollbar_gutter]. *)

val read_scrollbar_gutter : Cursor.t -> scrollbar_gutter
(** [read_scrollbar_gutter t] is the [scrollbar_gutter] parsed from [t]. *)

val pp_font_palette : font_palette Pp.t
(** [pp_font_palette] is the pretty-printer for [font_palette]. *)

val read_font_palette : Cursor.t -> font_palette
(** [read_font_palette t] is the [font_palette] parsed from [t]. *)

val pp_font_synthesis : font_synthesis Pp.t
(** [pp_font_synthesis] is the pretty-printer for [font_synthesis]. *)

val read_font_synthesis : Cursor.t -> font_synthesis
(** [read_font_synthesis t] is the [font_synthesis] parsed from [t]. *)

val pp_animation_timeline : animation_timeline Pp.t
(** [pp_animation_timeline] is the pretty-printer for [animation_timeline]. *)

val read_animation_timeline : Cursor.t -> animation_timeline
(** [read_animation_timeline t] is the [animation_timeline] parsed from [t]. *)

val pp_animation_range : animation_range Pp.t
(** [pp_animation_range] is the pretty-printer for [animation_range]. *)

val pp_view_transition_name : view_transition_name Pp.t
(** [pp_view_transition_name] is the pretty-printer for [view_transition_name].
*)

val read_view_transition_name : Cursor.t -> view_transition_name
(** [read_view_transition_name t] is the [view_transition_name] parsed from [t].
*)

val pp_view_transition_class : view_transition_class Pp.t
(** [pp_view_transition_class] is the pretty-printer for
    [view_transition_class]. *)

val read_view_transition_class : Cursor.t -> view_transition_class
(** [read_view_transition_class t] parses [view_transition_class]. *)

val pp_image_orientation : image_orientation Pp.t
(** [pp_image_orientation] is the pretty-printer for [image_orientation]. *)

val read_image_orientation : Cursor.t -> image_orientation
(** [read_image_orientation t] is the [image_orientation] parsed from [t]. *)

val pp_image_rendering : image_rendering Pp.t
(** [pp_image_rendering] is the pretty-printer for [image_rendering]. *)

val read_image_rendering : Cursor.t -> image_rendering
(** [read_image_rendering t] is the [image_rendering] parsed from [t]. *)

val pp_resolution : resolution Pp.t
(** [pp_resolution] is the pretty-printer for [resolution]. *)

val read_resolution : Cursor.t -> resolution
(** [read_resolution t] is the [resolution] parsed from [t]. *)

val pp_image_resolution : image_resolution Pp.t
(** [pp_image_resolution] is the pretty-printer for [image_resolution]. *)

val read_image_resolution : Cursor.t -> image_resolution
(** [read_image_resolution t] is the [image_resolution] parsed from [t]. *)

val pp_contain_intrinsic_size : contain_intrinsic_size Pp.t
(** [pp_contain_intrinsic_size] is the pretty-printer for
    [contain_intrinsic_size]. *)

val read_contain_intrinsic_size : Cursor.t -> contain_intrinsic_size
(** [read_contain_intrinsic_size t] is the [contain_intrinsic_size] parsed from
    [t]. *)

val pp_contain_intrinsic_longhand : contain_intrinsic_longhand Pp.t
(** [pp_contain_intrinsic_longhand] is the pretty-printer for
    [contain_intrinsic_longhand]. *)

val read_contain_intrinsic_longhand : Cursor.t -> contain_intrinsic_longhand
(** [read_contain_intrinsic_longhand t] parses a contain-intrinsic-* longhand
    value. *)

val pp_container_shorthand : container_shorthand Pp.t
(** [pp_container_shorthand] is the pretty-printer for [container_shorthand]. *)

val read_container_shorthand : Cursor.t -> container_shorthand
(** [read_container_shorthand t] is the [container_shorthand] parsed from [t].
*)

val pp_contain : contain Pp.t
(** [pp_contain] is the pretty-printer for [contain]. *)

val read_contain : Cursor.t -> contain
(** [read_contain t] is the [contain] parsed from [t]. *)

val pp_isolation : isolation Pp.t
(** [pp_isolation] is the pretty-printer for [isolation]. *)

val read_isolation : Cursor.t -> isolation
(** [read_isolation t] is the [isolation] parsed from [t]. *)

val pp_break_value : break_value Pp.t
(** [pp_break_value] is the pretty-printer for [break_value]. *)

val read_break_value : Cursor.t -> break_value
(** [read_break_value t] is the break value parsed from [t]. *)

val pp_break_inside_value : break_inside_value Pp.t
(** [pp_break_inside_value] is the pretty-printer for [break_inside_value]. *)

val read_break_inside_value : Cursor.t -> break_inside_value
(** [read_break_inside_value t] is the break-inside value parsed from [t]. *)

val pp_page_break_value : page_break_value Pp.t
(** [pp_page_break_value] is the printer for legacy [page-break-*]. *)

val read_page_break_value : Cursor.t -> page_break_value
(** [read_page_break_value t] is the legacy [page-break-before / -after] value
    parsed from [t]. *)

val pp_page_break_inside_value : page_break_inside_value Pp.t
(** [pp_page_break_inside_value] is the printer for legacy [page-break-inside].
*)

val read_page_break_inside_value : Cursor.t -> page_break_inside_value
(** [read_page_break_inside_value t] is the legacy [page-break-inside] value
    parsed from [t]. *)

val pp_columns_value : columns_value Pp.t
(** [pp_columns_value] is the pretty-printer for [columns_value]. *)

val read_columns_value : Cursor.t -> columns_value
(** [read_columns_value t] is the [columns_value] parsed from [t]. *)

val read_column_width : Cursor.t -> column_width
(** [read_column_width t] is the [column_width] parsed from [t]. *)

val read_column_count : Cursor.t -> column_count
(** [read_column_count t] is the [column_count] parsed from [t]. *)

val pp_logical_border_color : logical_border_color Pp.t
(** [pp_logical_border_color] is the pretty-printer for [logical_border_color].
*)

val read_logical_border_color : Cursor.t -> logical_border_color
(** [read_logical_border_color t] is the [logical_border_color] parsed from [t].
*)

val pp_logical_border_width : logical_border_width Pp.t
(** [pp_logical_border_width] is the pretty-printer for [logical_border_width].
*)

val read_logical_border_width : Cursor.t -> logical_border_width
(** [read_logical_border_width t] is the [logical_border_width] parsed from [t].
*)

val pp_logical_border_style : logical_border_style Pp.t
(** [pp_logical_border_style] is the pretty-printer for [logical_border_style].
*)

val read_logical_border_style : Cursor.t -> logical_border_style
(** [read_logical_border_style t] is the [logical_border_style] parsed from [t].
*)

val pp_column_span : column_span Pp.t
(** [pp_column_span] is the pretty-printer for [column_span]. *)

val read_column_span : Cursor.t -> column_span
(** [read_column_span t] is the [column_span] parsed from [t]. *)

val pp_text_emphasis : text_emphasis Pp.t
(** [pp_text_emphasis] is the pretty-printer for [text_emphasis]. *)

val read_text_emphasis : Cursor.t -> text_emphasis
(** [read_text_emphasis t] is the [text_emphasis] parsed from [t]. *)

val pp_text_emphasis_style : text_emphasis_style Pp.t
(** [pp_text_emphasis_style] is the pretty-printer for [text_emphasis_style]. *)

val read_text_emphasis_style : Cursor.t -> text_emphasis_style
(** [read_text_emphasis_style t] is the [text_emphasis_style] parsed from [t].
*)

val pp_text_decoration_skip : text_decoration_skip Pp.t
(** [pp_text_decoration_skip] is the pretty-printer for [text_decoration_skip].
*)

val read_text_decoration_skip : Cursor.t -> text_decoration_skip
(** [read_text_decoration_skip t] is the [text_decoration_skip] parsed from [t].
*)

val pp_text_decoration_skip_self : text_decoration_skip_self Pp.t
(** [pp_text_decoration_skip_self] is the pretty-printer for
    [text_decoration_skip_self]. *)

val read_text_decoration_skip_self : Cursor.t -> text_decoration_skip_self
(** [read_text_decoration_skip_self t] is the [text_decoration_skip_self] parsed
    from [t]. *)

val pp_text_decoration_skip_box : text_decoration_skip_box Pp.t
(** [pp_text_decoration_skip_box] is the pretty-printer for
    [text_decoration_skip_box]. *)

val read_text_decoration_skip_box : Cursor.t -> text_decoration_skip_box
(** [read_text_decoration_skip_box t] is the [text_decoration_skip_box] parsed
    from [t]. *)

val pp_text_decoration_skip_inset : text_decoration_skip_inset Pp.t
(** [pp_text_decoration_skip_inset] is the pretty-printer for
    [text_decoration_skip_inset]. *)

val read_text_decoration_skip_inset : Cursor.t -> text_decoration_skip_inset
(** [read_text_decoration_skip_inset t] is the [text_decoration_skip_inset]
    parsed from [t]. *)

val pp_text_decoration_skip_space : text_decoration_skip_space Pp.t
(** [pp_text_decoration_skip_space] is the pretty-printer for
    [text_decoration_skip_space]. *)

val read_text_decoration_skip_space : Cursor.t -> text_decoration_skip_space
(** [read_text_decoration_skip_space t] is the [text_decoration_skip_space]
    parsed from [t]. *)

val pp_text_decoration_skip_spaces : text_decoration_skip_spaces Pp.t
(** [pp_text_decoration_skip_spaces] is the pretty-printer for
    [text_decoration_skip_spaces]. *)

val read_text_decoration_skip_spaces : Cursor.t -> text_decoration_skip_spaces
(** [read_text_decoration_skip_spaces t] is the [text_decoration_skip_spaces]
    parsed from [t]. *)

val pp_text_emphasis_position : text_emphasis_position Pp.t
(** [pp_text_emphasis_position] is the pretty-printer for
    [text_emphasis_position]. *)

val read_text_emphasis_position : Cursor.t -> text_emphasis_position
(** [read_text_emphasis_position t] is the [text_emphasis_position] parsed from
    [t]. *)

val pp_text_indent_value : text_indent_value Pp.t
(** [pp_text_indent_value] is the pretty-printer for [text_indent_value]. *)

val read_text_indent_value : Cursor.t -> text_indent_value
(** [read_text_indent_value t] is the [text_indent_value] parsed from [t]. *)

val pp_text_emphasis_skip_keyword : text_emphasis_skip_keyword Pp.t
(** [pp_text_emphasis_skip_keyword] is the pretty-printer for
    [text_emphasis_skip_keyword]. *)

val read_text_emphasis_skip_keyword : Cursor.t -> text_emphasis_skip_keyword
(** [read_text_emphasis_skip_keyword t] is the [text_emphasis_skip_keyword]
    parsed from [t]. *)

val pp_text_emphasis_skip : text_emphasis_skip Pp.t
(** [pp_text_emphasis_skip] is the pretty-printer for [text_emphasis_skip]. *)

val read_text_emphasis_skip : Cursor.t -> text_emphasis_skip
(** [read_text_emphasis_skip t] is the [text_emphasis_skip] parsed from [t]. *)

val pp_text_orientation : text_orientation Pp.t
(** [pp_text_orientation] is the pretty-printer for [text_orientation]. *)

val read_text_orientation : Cursor.t -> text_orientation
(** [read_text_orientation t] is the [text_orientation] parsed from [t]. *)

val pp_line_break : line_break Pp.t
(** [pp_line_break] is the pretty-printer for [line_break]. *)

val read_line_break : Cursor.t -> line_break
(** [read_line_break t] is the [line_break] parsed from [t]. *)

val pp_font_optical_sizing : font_optical_sizing Pp.t
(** [pp_font_optical_sizing] is the pretty-printer for [font_optical_sizing]. *)

val read_font_optical_sizing : Cursor.t -> font_optical_sizing
(** [read_font_optical_sizing t] is the [font_optical_sizing] parsed from [t].
*)

val pp_font_kerning : font_kerning Pp.t
(** [pp_font_kerning] is the pretty-printer for [font_kerning]. *)

val read_font_kerning : Cursor.t -> font_kerning
(** [read_font_kerning t] is the [font_kerning] parsed from [t]. *)

val pp_font_language_override : font_language_override Pp.t
(** [pp_font_language_override] is the pretty-printer for
    [font_language_override]. *)

val read_font_language_override : Cursor.t -> font_language_override
(** [read_font_language_override t] is the [font_language_override] parsed from
    [t]. *)

val pp_font_synthesis_style : font_synthesis_style Pp.t
(** [pp_font_synthesis_style] is the pretty-printer for [font_synthesis_style].
*)

val read_font_synthesis_style : Cursor.t -> font_synthesis_style
(** [read_font_synthesis_style t] is the [font_synthesis_style] parsed from [t].
*)

val pp_font_synthesis_weight : font_synthesis_weight Pp.t
(** [pp_font_synthesis_weight] is the pretty-printer for
    [font_synthesis_weight]. *)

val read_font_synthesis_weight : Cursor.t -> font_synthesis_weight
(** [read_font_synthesis_weight t] is the [font_synthesis_weight] parsed from
    [t]. *)

val pp_font_synthesis_small_caps : font_synthesis_small_caps Pp.t
(** [pp_font_synthesis_small_caps] is the pretty-printer for
    [font_synthesis_small_caps]. *)

val read_font_synthesis_small_caps : Cursor.t -> font_synthesis_small_caps
(** [read_font_synthesis_small_caps t] is the [font_synthesis_small_caps] parsed
    from [t]. *)

val pp_font_synthesis_position : font_synthesis_position Pp.t
(** [pp_font_synthesis_position] is the pretty-printer for
    [font_synthesis_position]. *)

val read_font_synthesis_position : Cursor.t -> font_synthesis_position
(** [read_font_synthesis_position t] is the [font_synthesis_position] parsed
    from [t]. *)

val pp_font_variant_ligature : font_variant_ligature Pp.t
(** [pp_font_variant_ligature] is the pretty-printer for
    [font_variant_ligature]. *)

val read_font_variant_ligature : Cursor.t -> font_variant_ligature
(** [read_font_variant_ligature t] is the [font_variant_ligature] parsed from
    [t]. *)

val pp_font_variant_ligatures : font_variant_ligatures Pp.t
(** [pp_font_variant_ligatures] is the pretty-printer for
    [font_variant_ligatures]. *)

val read_font_variant_ligatures : Cursor.t -> font_variant_ligatures
(** [read_font_variant_ligatures t] is the [font_variant_ligatures] parsed from
    [t]. *)

val pp_font_variant_caps : font_variant_caps Pp.t
(** [pp_font_variant_caps] is the pretty-printer for [font_variant_caps]. *)

val read_font_variant_caps : Cursor.t -> font_variant_caps
(** [read_font_variant_caps t] is the [font_variant_caps] parsed from [t]. *)

val pp_font_variant_position : font_variant_position Pp.t
(** [pp_font_variant_position] is the pretty-printer for
    [font_variant_position]. *)

val read_font_variant_position : Cursor.t -> font_variant_position
(** [read_font_variant_position t] is the [font_variant_position] parsed from
    [t]. *)

val pp_east_asian_feature : east_asian_feature Pp.t
(** [pp_east_asian_feature] is the pretty-printer for [east_asian_feature]. *)

val read_east_asian_feature : Cursor.t -> east_asian_feature
(** [read_east_asian_feature t] is the [east_asian_feature] parsed from [t]. *)

val pp_font_variant_east_asian : font_variant_east_asian Pp.t
(** [pp_font_variant_east_asian] is the pretty-printer for
    [font_variant_east_asian]. *)

val read_font_variant_east_asian : Cursor.t -> font_variant_east_asian
(** [read_font_variant_east_asian t] is the [font_variant_east_asian] parsed
    from [t]. *)

val pp_scroll_behavior : scroll_behavior Pp.t
(** [pp_scroll_behavior] is the pretty-printer for [scroll_behavior]. *)

val read_scroll_behavior : Cursor.t -> scroll_behavior
(** [read_scroll_behavior t] is the [scroll_behavior] parsed from [t]. *)

val pp_scroll_snap_align : scroll_snap_align Pp.t
(** [pp_scroll_snap_align] is the pretty-printer for [scroll_snap_align]. *)

val read_scroll_snap_align : Cursor.t -> scroll_snap_align
(** [read_scroll_snap_align t] is the [scroll_snap_align] parsed from [t]. *)

val pp_scroll_snap_stop : scroll_snap_stop Pp.t
(** [pp_scroll_snap_stop] is the pretty-printer for [scroll_snap_stop]. *)

val read_scroll_snap_stop : Cursor.t -> scroll_snap_stop
(** [read_scroll_snap_stop t] is the [scroll_snap_stop] parsed from [t]. *)

val pp_scroll_snap_strictness : scroll_snap_strictness Pp.t
(** [pp_scroll_snap_strictness] is the pretty-printer for
    [scroll_snap_strictness]. *)

val read_scroll_snap_strictness : Cursor.t -> scroll_snap_strictness
(** [read_scroll_snap_strictness t] is the [scroll_snap_strictness] parsed from
    [t]. *)

val pp_scroll_snap_axis : scroll_snap_axis Pp.t
(** [pp_scroll_snap_axis] is the pretty-printer for [scroll_snap_axis]. *)

val read_scroll_snap_axis : Cursor.t -> scroll_snap_axis
(** [read_scroll_snap_axis t] is the [scroll_snap_axis] parsed from [t]. *)

val pp_scroll_snap_type : scroll_snap_type Pp.t
(** [pp_scroll_snap_type] is the pretty-printer for [scroll_snap_type]. *)

val read_scroll_snap_type : Cursor.t -> scroll_snap_type
(** [read_scroll_snap_type t] is the [scroll_snap_type] parsed from [t]. *)

val pp_overscroll_behavior : overscroll_behavior Pp.t
(** [pp_overscroll_behavior] is the pretty-printer for [overscroll_behavior]. *)

val read_overscroll_behavior : Cursor.t -> overscroll_behavior
(** [read_overscroll_behavior t] is the [overscroll_behavior] parsed from [t].
*)

val pp_svg_paint : svg_paint Pp.t
(** [pp_svg_paint] is the pretty-printer for [svg_paint]. *)

val read_svg_paint : Cursor.t -> svg_paint
(** [read_svg_paint t] is the [svg_paint] parsed from [t]. *)

val pp_direction : direction Pp.t
(** [pp_direction] is the pretty-printer for [direction]. *)

val read_direction : Cursor.t -> direction
(** [read_direction t] is the [direction] parsed from [t]. *)

val pp_fill_rule : fill_rule Pp.t
(** [pp_fill_rule] pretty-prints an SVG [<fill-rule>]. *)

val read_fill_rule : Cursor.t -> fill_rule
(** [read_fill_rule t] is the [fill_rule] parsed from [t]. *)

val pp_stroke_linecap : stroke_linecap Pp.t
(** [pp_stroke_linecap] pretty-prints a [stroke-linecap] keyword. *)

val read_stroke_linecap : Cursor.t -> stroke_linecap
(** [read_stroke_linecap t] is the [stroke_linecap] parsed from [t]. *)

val pp_stroke_linejoin : stroke_linejoin Pp.t
(** [pp_stroke_linejoin] pretty-prints a [stroke-linejoin] keyword. *)

val read_stroke_linejoin : Cursor.t -> stroke_linejoin
(** [read_stroke_linejoin t] is the [stroke_linejoin] parsed from [t]. *)

val pp_stroke_miterlimit : stroke_miterlimit Pp.t
(** [pp_stroke_miterlimit] pretty-prints a [stroke-miterlimit]. *)

val read_stroke_miterlimit : Cursor.t -> stroke_miterlimit
(** [read_stroke_miterlimit t] is the [stroke_miterlimit] parsed from [t]. *)

val pp_vector_effect_keyword : vector_effect_keyword Pp.t
(** [pp_vector_effect_keyword] pretty-prints one [vector-effect] operand. *)

val read_vector_effect_keyword : Cursor.t -> vector_effect_keyword
(** [read_vector_effect_keyword t] is the [vector_effect_keyword] parsed from
    [t]. *)

val pp_vector_effect_space : vector_effect_space Pp.t
(** [pp_vector_effect_space] pretty-prints a [vector-effect] host space. *)

val read_vector_effect_space : Cursor.t -> vector_effect_space
(** [read_vector_effect_space t] is the [vector_effect_space] parsed from [t].
*)

val pp_vector_effect : vector_effect Pp.t
(** [pp_vector_effect] pretty-prints a [vector-effect]. *)

val read_vector_effect : Cursor.t -> vector_effect
(** [read_vector_effect t] is the [vector_effect] parsed from [t]. *)

val pp_paint_order_keyword : paint_order_keyword Pp.t
(** [pp_paint_order_keyword] pretty-prints one [paint-order] operand. *)

val read_paint_order_keyword : Cursor.t -> paint_order_keyword
(** [read_paint_order_keyword t] is the [paint_order_keyword] parsed from [t].
*)

val pp_paint_order : paint_order Pp.t
(** [pp_paint_order] pretty-prints a [paint-order]. *)

val read_paint_order : Cursor.t -> paint_order
(** [read_paint_order t] is the [paint_order] parsed from [t]. *)

val pp_stroke_width : stroke_width Pp.t
(** [pp_stroke_width] pretty-prints a [stroke-width]. *)

val read_stroke_width : Cursor.t -> stroke_width
(** [read_stroke_width t] is the [stroke_width] parsed from [t]. A bare number
    is in user units; anything carrying a unit or a percent sign is a
    [<length-percentage>]. A negative width is rejected. *)

val pp_dash_length : dash_length Pp.t
(** [pp_dash_length] pretty-prints one SVG dash length. *)

val read_dash_length : Cursor.t -> dash_length
(** [read_dash_length t] is the [dash_length] parsed from [t]. A bare number is
    in user units; anything carrying a unit or a percent sign is a
    [<length-percentage>]. *)

val pp_stroke_dashoffset : stroke_dashoffset Pp.t
(** [pp_stroke_dashoffset] pretty-prints a [stroke-dashoffset]. *)

val read_stroke_dashoffset : Cursor.t -> stroke_dashoffset
(** [read_stroke_dashoffset t] is the [stroke_dashoffset] parsed from [t]. *)

val pp_stroke_dasharray : stroke_dasharray Pp.t
(** [pp_stroke_dasharray] pretty-prints a [stroke-dasharray]. *)

val read_stroke_dasharray : Cursor.t -> stroke_dasharray
(** [read_stroke_dasharray t] is the [stroke_dasharray] parsed from [t]. *)

val pp_unicode_bidi : unicode_bidi Pp.t
(** [pp_unicode_bidi] is the pretty-printer for [unicode_bidi]. *)

val read_unicode_bidi : Cursor.t -> unicode_bidi
(** [read_unicode_bidi t] is the [unicode_bidi] parsed from [t]. *)

val pp_writing_mode : writing_mode Pp.t
(** [pp_writing_mode] is the pretty-printer for [writing_mode]. *)

val read_writing_mode : Cursor.t -> writing_mode
(** [read_writing_mode t] is the [writing_mode] parsed from [t]. *)

val pp_text_combine_upright : text_combine_upright Pp.t
(** [pp_text_combine_upright] is the pretty-printer for [text_combine_upright].
*)

val read_text_combine_upright : Cursor.t -> text_combine_upright
(** [read_text_combine_upright t] is the [text_combine_upright] parsed from [t].
*)

val pp_webkit_appearance : webkit_appearance Pp.t
(** [pp_webkit_appearance] is the pretty-printer for [webkit_appearance]. *)

val read_webkit_appearance : Cursor.t -> webkit_appearance
(** [read_webkit_appearance t] is the [webkit_appearance] parsed from [t]. *)

val pp_webkit_font_smoothing : webkit_font_smoothing Pp.t
(** [pp_webkit_font_smoothing] is the pretty-printer for
    [webkit_font_smoothing]. *)

val read_webkit_font_smoothing : Cursor.t -> webkit_font_smoothing
(** [read_webkit_font_smoothing t] is the [webkit_font_smoothing] parsed from
    [t]. *)

val pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t
(** [pp_moz_osx_font_smoothing] is the pretty-printer for
    [moz_osx_font_smoothing]. *)

val read_moz_osx_font_smoothing : Cursor.t -> moz_osx_font_smoothing
(** [read_moz_osx_font_smoothing t] is the [moz_osx_font_smoothing] parsed from
    [t]. *)

val pp_webkit_box_orient : webkit_box_orient Pp.t
(** [pp_webkit_box_orient] is the pretty-printer for [webkit_box_orient]. *)

val read_webkit_box_orient : Cursor.t -> webkit_box_orient
(** [read_webkit_box_orient t] is the [webkit_box_orient] parsed from [t]. *)

val pp_moz_orient : moz_orient Pp.t
(** [pp_moz_orient] is the pretty-printer for [moz_orient]. *)

val read_moz_orient : Cursor.t -> moz_orient
(** [read_moz_orient t] is the [moz_orient] parsed from [t]. *)

val pp_webkit_line_clamp : webkit_line_clamp Pp.t
(** [pp_webkit_line_clamp] is the pretty-printer for [webkit_line_clamp]. *)

val read_webkit_line_clamp : Cursor.t -> webkit_line_clamp
(** [read_webkit_line_clamp t] is the [webkit_line_clamp] parsed from [t]. *)

val read_text_size_adjust : Cursor.t -> text_size_adjust
(** [read_text_size_adjust t] is the text size adjust value parsed from [t]. *)

val pp_forced_color_adjust : forced_color_adjust Pp.t
(** [pp_forced_color_adjust] is the pretty-printer for [forced_color_adjust]. *)

val read_forced_color_adjust : Cursor.t -> forced_color_adjust
(** [read_forced_color_adjust t] is the [forced_color_adjust] parsed from [t].
*)

val pp_appearance : appearance Pp.t
(** [pp_appearance] is the pretty-printer for [appearance]. *)

val read_appearance : Cursor.t -> appearance
(** [read_appearance t] is the [appearance] parsed from [t]. *)

val pp_color_scheme : color_scheme Pp.t
(** [pp_color_scheme] is the pretty-printer for [color_scheme]. *)

val read_color_scheme : Cursor.t -> color_scheme
(** [read_color_scheme t] is the [color_scheme] parsed from [t]. *)

val pp_print_color_adjust : print_color_adjust Pp.t
(** [pp_print_color_adjust] is the pretty-printer for [print_color_adjust]. *)

val read_print_color_adjust : Cursor.t -> print_color_adjust
(** [read_print_color_adjust t] is the [print_color_adjust] parsed from [t]. *)

val pp_box_decoration_break : box_decoration_break Pp.t
(** [pp_box_decoration_break] is the pretty-printer for [box_decoration_break].
*)

val read_box_decoration_break : Cursor.t -> box_decoration_break
(** [read_box_decoration_break t] is the [box_decoration_break] parsed from [t].
*)

val pp_clear : clear Pp.t
(** [pp_clear] is the pretty-printer for [clear]. *)

val read_clear : Cursor.t -> clear
(** [read_clear t] is the [clear] parsed from [t]. *)

val pp_float_side : float_side Pp.t
(** [pp_float_side] is the pretty-printer for [float_side]. *)

val read_float_side : Cursor.t -> float_side
(** [read_float_side t] is the [float_side] parsed from [t]. *)

val pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t
(** [pp_text_decoration_skip_ink] is the pretty-printer for
    [text_decoration_skip_ink]. *)

val read_text_decoration_skip_ink : Cursor.t -> text_decoration_skip_ink
(** [read_text_decoration_skip_ink t] is the [text_decoration_skip_ink] parsed
    from [t]. *)

val pp_will_change : will_change Pp.t
(** [pp_will_change] is the pretty-printer for [will_change]. *)

val read_will_change : Cursor.t -> will_change
(** [read_will_change t] is the [will_change] parsed from [t]. *)

val pp_clip : clip Pp.t
(** [pp_clip] is the pretty-printer for [clip]. *)

val read_clip : Cursor.t -> clip
(** [read_clip t] is the [clip] parsed from [t]. *)

val pp_clip_path : clip_path Pp.t
(** [pp_clip_path] is the pretty-printer for [clip_path]. *)

val read_clip_path : Cursor.t -> clip_path
(** [read_clip_path t] is the [clip_path] parsed from [t]. *)

val pp_clip_geometry_box : clip_geometry_box Pp.t
(** [pp_clip_geometry_box] pretty-prints a clip geometry box. *)

val read_clip_geometry_box : Cursor.t -> clip_geometry_box
(** [read_clip_geometry_box t] parses a clip geometry box. *)

val pp_clip_path_extent : clip_path_extent Pp.t
(** [pp_clip_path_extent] pretty-prints a clip path extent. *)

val read_clip_path_extent : Cursor.t -> clip_path_extent
(** [read_clip_path_extent t] parses a clip path extent. *)

val pp_clip_path_fill_rule : clip_path_fill_rule Pp.t
(** [pp_clip_path_fill_rule] pretty-prints a clip path fill rule. *)

val read_clip_path_fill_rule : Cursor.t -> clip_path_fill_rule
(** [read_clip_path_fill_rule t] parses a clip path fill rule. *)

val pp_perspective_origin : perspective_origin Pp.t
(** [pp_perspective_origin] is the pretty-printer for [perspective_origin]. *)

val read_perspective_origin : Cursor.t -> perspective_origin
(** [read_perspective_origin t] is the [perspective_origin] parsed from [t]. *)

val pp_outline_shorthand : outline_shorthand Pp.t
(** [pp_outline_shorthand] is the pretty-printer for [outline_shorthand]. *)

val read_outline_shorthand : Cursor.t -> outline_shorthand
(** [read_outline_shorthand t] is the [outline_shorthand] parsed from [t]. *)

(** {2 Helper functions for property types} *)

val shadow :
  ?inset:bool ->
  ?inset_var:string ->
  ?inset_var_no_fallback:bool ->
  ?h_offset:length ->
  ?v_offset:length ->
  ?blur:length ->
  ?spread:length ->
  ?color:color ->
  unit ->
  shadow
(** [shadow ?inset ?inset_var ?inset_var_no_fallback ?h_offset ?v_offset ?blur
     ?spread ?color ()] is a shadow value. When [inset_var] is set, outputs
    [var(--<name>,)] (empty fallback) or [var(--<name>)] (when
    [inset_var_no_fallback] is true). Defaults: inset=false, h_offset=0px,
    v_offset=0px, blur=0px, spread=0px, color=Rgb(0,0,0). *)

(** {2 Generic property handling} *)

val pp_any_property : any_property Pp.t
(** [pp_any_property] pretty-prints any CSS property. *)

val read_any_property : Cursor.t -> any_property
(** [read_any_property t] parses any CSS property. *)

val property_value_kind : 'a property -> 'a property_value_kind option
(** [property_value_kind property] returns the optional property-specific
    evaluator for a value shape. Generic context evaluation, including direct
    CSS-wide keywords, does not depend on this specialization. *)

(** {2 Function names} *)

val is_math_function : string -> bool
(** [is_math_function name] is true for a CSS Values 4 sec. 10 math function:
    [calc()], the comparison functions ([min()], [max()], [clamp()]), the
    stepped-value ([round()], [mod()], [rem()]), trigonometric ([sin()] through
    [atan2()]), exponential ([pow()], [sqrt()], [hypot()], [log()], [exp()]) and
    sign-related ([abs()], [sign()]) families. [name] is matched case
    insensitively, as CSS function names are. *)

val is_color_function : string -> bool
(** [is_color_function name] is true for a function whose own syntax fixes it as
    a colour: the CSS Color 4 numeric notations, [color()] and [color-mix()],
    and [light-dark()]. A gradient is not one: its type is fixed by the property
    it lands in, not by the function. *)
