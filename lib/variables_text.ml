open Properties
open Variables_intf
open Variables_value

let vars_of_text_indent_value (value : Properties.text_indent_value) :
    any_var list =
  match value with
  | Var v -> [ V v ]
  | Indent { length; _ } -> vars_of_length_percentage length
  | Inherit | Initial | Unset | Revert | Revert_layer -> []

let vars_of_font_size (value : Properties.font_size) : any_var list =
  vars_of_lvc
    (match value with
    | Length l -> `Length l
    | Var v -> `Var (V v)
    | Calc calc -> `Calc calc
    | _ -> `Other)

let vars_of_line_height (value : Properties.line_height) : any_var list =
  match value with Var v -> [ V v ] | Calc calc -> vars_of_calc calc | _ -> []

(* Complex type extractors *)
let vars_of_font_weight (value : Properties.font_weight) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_family (value : Properties.font_family) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_content (value : Properties.content) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_counter_set (value : Properties.counter_set) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration (value : Properties.text_decoration) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_emphasis_style (value : Properties.text_emphasis_style) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_emphasis (value : Properties.text_emphasis) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Emphasis (style, color) ->
      Option.value ~default:[] (Option.map vars_of_text_emphasis_style style)
      @ Option.value ~default:[] (Option.map vars_of_color color)
  | _ -> []

let vars_of_text_emphasis_position (value : Properties.text_emphasis_position) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_orientation (value : Properties.text_orientation) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_transform (value : Properties.text_transform) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_quotes (value : Properties.quotes) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_feature_settings (value : Properties.font_feature_settings) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variation_settings (value : Properties.font_variation_settings)
    : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_numeric (value : Properties.font_variant_numeric) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_image (value : Properties.list_style_image) :
    any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_type (value : Properties.list_style_type) : any_var list
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_shadow (value : Properties.text_shadow) : any_var list =
  match value with
  | Var v -> [ V v ]
  | Text_shadow { h_offset; v_offset; blur; color; _ } -> (
      vars_of_length h_offset @ vars_of_length v_offset
      @ (match blur with Some l -> vars_of_length l | None -> [])
      @ match color with Some c -> vars_of_color c | None -> [])
  | _ -> []

let vars_of_text_shadow_list (values : Properties.text_shadow list) :
    any_var list =
  List.concat_map vars_of_text_shadow values

let vars_of_vertical_align (value : Properties.vertical_align) : any_var list =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_tab_size (value : Properties.tab_size) : any_var list =
  match value with
  | Number n -> vars_of_number_value n
  | Length len -> vars_of_length len
  | Var v -> [ V v ]
  | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_font_palette (value : Properties.font_palette) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis (value : Properties.font_synthesis) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_style (value : Properties.font_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_align (value : Properties.text_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_baseline_source (value : Properties.baseline_source) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_alignment_baseline (value : Properties.alignment_baseline) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_baseline_shift (value : Properties.baseline_shift) =
  match value with
  | Var v -> [ V v ]
  | Shift value -> vars_of_length_percentage value
  | _ -> []

let vars_of_text_decoration_line (value : Properties.text_decoration_line) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_style (value : Properties.text_decoration_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_decoration_skip (value : Properties.text_decoration_skip) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_self (value : Properties.text_decoration_skip_self)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_box (value : Properties.text_decoration_skip_box) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_inset
    (value : Properties.text_decoration_skip_inset) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_spaces
    (value : Properties.text_decoration_skip_spaces) =
  match value with Var v -> [ V v ] | _ -> []

let rec vars_of_text_overflow (value : Properties.text_overflow) =
  match value with
  | Var v -> [ V v ]
  | Pair (first, second) ->
      vars_of_text_overflow first @ vars_of_text_overflow second
  | _ -> []

let vars_of_text_wrap (value : Properties.text_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_wrap_mode (value : Properties.text_wrap_mode) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_wrap_style (value : Properties.text_wrap_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_glyph_orientation_vertical
    (value : Properties.glyph_orientation_vertical) =
  match value with
  | Var v -> [ V v ]
  | Angle angle -> vars_of_angle angle
  | _ -> []

let vars_of_text_box_trim (value : Properties.text_box_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_underline_position (value : Properties.text_underline_position)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_box_edge (value : Properties.text_box_edge) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_box (value : Properties.text_box) =
  match value with
  | Var v -> [ V v ]
  | Box (trim, edge) ->
      Option.value ~default:[] (Option.map vars_of_text_box_trim trim)
      @ Option.value ~default:[] (Option.map vars_of_text_box_edge edge)
  | Normal | Initial | Inherit | Unset | Revert | Revert_layer -> []

let vars_of_inline_sizing (value : Properties.inline_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_line_fit_edge (value : Properties.line_fit_edge) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_align (value : Properties.ruby_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_merge (value : Properties.ruby_merge) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_overhang (value : Properties.ruby_overhang) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_ruby_position (value : Properties.ruby_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_spacing_trim (value : Properties.text_spacing_trim) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_hyphenate_limit_chars (value : Properties.hyphenate_limit_chars) =
  let slot (item : Properties.hyphenate_limit_chars_item) =
    match item with Auto -> [] | Chars n -> vars_of_number_value n
  in
  match value with
  | Var v -> [ V v ]
  | One a -> slot a
  | Two (a, b) -> slot a @ slot b
  | Three (a, b, c) -> slot a @ slot b @ slot c
  | _ -> []

let vars_of_initial_letter (value : Properties.initial_letter) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_white_space (value : Properties.white_space) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant (value : Properties.font_variant) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_alternates (value : Properties.font_variant_alternates)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_white_space_collapse (value : Properties.white_space_collapse) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_word_break (value : Properties.word_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_overflow_wrap (value : Properties.overflow_wrap) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_line_break (value : Properties.line_break) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_list_style_position (value : Properties.list_style_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_decoration_skip_ink (value : Properties.text_decoration_skip_ink) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_unicode_bidi (value : Properties.unicode_bidi) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_writing_mode (value : Properties.writing_mode) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_combine_upright (value : Properties.text_combine_upright) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_webkit_font_smoothing (value : Properties.webkit_font_smoothing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_osx_font_smoothing (value : Properties.moz_osx_font_smoothing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_stretch (value : Properties.font_stretch) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_optical_sizing (value : Properties.font_optical_sizing) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_kerning (value : Properties.font_kerning) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_language_override (value : Properties.font_language_override) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis_style (value : Properties.font_synthesis_style) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis_weight (value : Properties.font_synthesis_weight) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_synthesis_small_caps (value : Properties.font_synthesis_small_caps)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_synthesis_position (value : Properties.font_synthesis_position)
    =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_ligatures (value : Properties.font_variant_ligatures) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_caps (value : Properties.font_variant_caps) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_position (value : Properties.font_variant_position) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_east_asian (value : Properties.font_variant_east_asian) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_size_adjust (value : Properties.font_size_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font_variant_emoji (value : Properties.font_variant_emoji) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_text_size_adjust (value : Properties.text_size_adjust) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_direction (value : Properties.direction) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_caption_side (value : Properties.caption_side) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_dominant_baseline (value : Properties.dominant_baseline) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_hyphens (value : Properties.hyphens) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_initial_letter_align (value : Properties.initial_letter_align) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_initial_letter_wrap (value : Properties.initial_letter_wrap) =
  match value with
  | Var v -> [ V v ]
  | Length lp -> vars_of_length_percentage lp
  | _ -> []

let vars_of_text_emphasis_skip (value : Properties.text_emphasis_skip) =
  match value with Var v -> [ V v ] | _ -> []

let vars_of_font (value : Properties.font) : any_var list =
  let opt f = function Some x -> f x | None -> [] in
  match value with
  | Var v -> [ V v ]
  | Shorthand { style; variant = _; weight; stretch; size; line_height; family }
    ->
      opt vars_of_font_style style
      @ opt vars_of_font_weight weight
      @ opt vars_of_font_stretch stretch
      @ vars_of_font_size size
      @ opt vars_of_line_height line_height
      @ vars_of_font_family family
  | _ -> []

let vars_of_list_style (value : Properties.list_style) : any_var list =
  let opt f = function Some x -> f x | None -> [] in
  match value with
  | Var v -> [ V v ]
  | Shorthand { type_; position; image } ->
      opt vars_of_list_style_type type_
      @ opt vars_of_list_style_position position
      @ opt vars_of_list_style_image image
  | _ -> []
