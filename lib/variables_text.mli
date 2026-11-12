(** Variable extraction from the text and font property values: fonts, text
    decoration and emphasis, line breaking, ruby, lists and generated content.
*)

val vars_of_text_indent_value :
  Properties.text_indent_value -> Variables_intf.any_var list
(** [vars_of_text_indent_value v] is the variables [v] references. *)

val vars_of_font_size : Properties.font_size -> Variables_intf.any_var list
(** [vars_of_font_size v] is the variables [v] references. *)

val vars_of_line_height : Properties.line_height -> Variables_intf.any_var list
(** [vars_of_line_height v] is the variables [v] references. *)

val vars_of_font_weight : Properties.font_weight -> Variables_intf.any_var list
(** [vars_of_font_weight v] is the variables [v] references. *)

val vars_of_font_family : Properties.font_family -> Variables_intf.any_var list
(** [vars_of_font_family v] is the variables [v] references. *)

val vars_of_content : Properties.content -> Variables_intf.any_var list
(** [vars_of_content v] is the variables [v] references. *)

val vars_of_counter_set : Properties.counter_set -> Variables_intf.any_var list
(** [vars_of_counter_set v] is the variables [v] references. *)

val vars_of_text_decoration :
  Properties.text_decoration -> Variables_intf.any_var list
(** [vars_of_text_decoration v] is the variables [v] references. *)

val vars_of_text_emphasis_style :
  Properties.text_emphasis_style -> Variables_intf.any_var list
(** [vars_of_text_emphasis_style v] is the variables [v] references. *)

val vars_of_text_emphasis :
  Properties.text_emphasis -> Variables_intf.any_var list
(** [vars_of_text_emphasis v] is the variables [v] references. *)

val vars_of_text_emphasis_position :
  Properties.text_emphasis_position -> Variables_intf.any_var list
(** [vars_of_text_emphasis_position v] is the variables [v] references. *)

val vars_of_text_orientation :
  Properties.text_orientation -> Variables_intf.any_var list
(** [vars_of_text_orientation v] is the variables [v] references. *)

val vars_of_text_transform :
  Properties.text_transform -> Variables_intf.any_var list
(** [vars_of_text_transform v] is the variables [v] references. *)

val vars_of_quotes : Properties.quotes -> Variables_intf.any_var list
(** [vars_of_quotes v] is the variables [v] references. *)

val vars_of_font_feature_settings :
  Properties.font_feature_settings -> Variables_intf.any_var list
(** [vars_of_font_feature_settings v] is the variables [v] references. *)

val vars_of_font_variation_settings :
  Properties.font_variation_settings -> Variables_intf.any_var list
(** [vars_of_font_variation_settings v] is the variables [v] references. *)

val vars_of_font_variant_numeric :
  Properties.font_variant_numeric -> Variables_intf.any_var list
(** [vars_of_font_variant_numeric v] is the variables [v] references. *)

val vars_of_list_style_image :
  Properties.list_style_image -> Variables_intf.any_var list
(** [vars_of_list_style_image v] is the variables [v] references. *)

val vars_of_list_style_type :
  Properties.list_style_type -> Variables_intf.any_var list
(** [vars_of_list_style_type v] is the variables [v] references. *)

val vars_of_text_shadow_list :
  Properties.text_shadow list -> Variables_intf.any_var list
(** [vars_of_text_shadow_list v] is the variables [v] references. *)

val vars_of_vertical_align :
  Properties.vertical_align -> Variables_intf.any_var list
(** [vars_of_vertical_align v] is the variables [v] references. *)

val vars_of_tab_size : Properties.tab_size -> Variables_intf.any_var list
(** [vars_of_tab_size v] is the variables [v] references. *)

val vars_of_font_palette :
  Properties.font_palette -> Variables_intf.any_var list
(** [vars_of_font_palette v] is the variables [v] references. *)

val vars_of_font_synthesis :
  Properties.font_synthesis -> Variables_intf.any_var list
(** [vars_of_font_synthesis v] is the variables [v] references. *)

val vars_of_font_style : Properties.font_style -> Variables_intf.any_var list
(** [vars_of_font_style v] is the variables [v] references. *)

val vars_of_text_align : Properties.text_align -> Variables_intf.any_var list
(** [vars_of_text_align v] is the variables [v] references. *)

val vars_of_baseline_source :
  Properties.baseline_source -> Variables_intf.any_var list
(** [vars_of_baseline_source v] is the variables [v] references. *)

val vars_of_alignment_baseline :
  Properties.alignment_baseline -> Variables_intf.any_var list
(** [vars_of_alignment_baseline v] is the variables [v] references. *)

val vars_of_baseline_shift :
  Properties.baseline_shift -> Variables_intf.any_var list
(** [vars_of_baseline_shift v] is the variables [v] references. *)

val vars_of_text_decoration_line :
  Properties.text_decoration_line -> Variables_intf.any_var list
(** [vars_of_text_decoration_line v] is the variables [v] references. *)

val vars_of_text_decoration_style :
  Properties.text_decoration_style -> Variables_intf.any_var list
(** [vars_of_text_decoration_style v] is the variables [v] references. *)

val vars_of_text_decoration_skip :
  Properties.text_decoration_skip -> Variables_intf.any_var list
(** [vars_of_text_decoration_skip v] is the variables [v] references. *)

val vars_of_decoration_skip_self :
  Properties.text_decoration_skip_self -> Variables_intf.any_var list
(** [vars_of_decoration_skip_self v] is the variables [v] references. *)

val vars_of_decoration_skip_box :
  Properties.text_decoration_skip_box -> Variables_intf.any_var list
(** [vars_of_decoration_skip_box v] is the variables [v] references. *)

val vars_of_decoration_skip_inset :
  Properties.text_decoration_skip_inset -> Variables_intf.any_var list
(** [vars_of_decoration_skip_inset v] is the variables [v] references. *)

val vars_of_decoration_skip_spaces :
  Properties.text_decoration_skip_spaces -> Variables_intf.any_var list
(** [vars_of_decoration_skip_spaces v] is the variables [v] references. *)

val vars_of_text_overflow :
  Properties.text_overflow -> Variables_intf.any_var list
(** [vars_of_text_overflow v] is the variables [v] references. *)

val vars_of_text_wrap : Properties.text_wrap -> Variables_intf.any_var list
(** [vars_of_text_wrap v] is the variables [v] references. *)

val vars_of_text_wrap_mode :
  Properties.text_wrap_mode -> Variables_intf.any_var list
(** [vars_of_text_wrap_mode v] is the variables [v] references. *)

val vars_of_text_wrap_style :
  Properties.text_wrap_style -> Variables_intf.any_var list
(** [vars_of_text_wrap_style v] is the variables [v] references. *)

val vars_of_glyph_orientation_vertical :
  Properties.glyph_orientation_vertical -> Variables_intf.any_var list
(** [vars_of_glyph_orientation_vertical v] is the variables [v] references. *)

val vars_of_text_box_trim :
  Properties.text_box_trim -> Variables_intf.any_var list
(** [vars_of_text_box_trim v] is the variables [v] references. *)

val vars_of_text_underline_position :
  Properties.text_underline_position -> Variables_intf.any_var list
(** [vars_of_text_underline_position v] is the variables [v] references. *)

val vars_of_text_box_edge :
  Properties.text_box_edge -> Variables_intf.any_var list
(** [vars_of_text_box_edge v] is the variables [v] references. *)

val vars_of_text_box : Properties.text_box -> Variables_intf.any_var list
(** [vars_of_text_box v] is the variables [v] references. *)

val vars_of_inline_sizing :
  Properties.inline_sizing -> Variables_intf.any_var list
(** [vars_of_inline_sizing v] is the variables [v] references. *)

val vars_of_line_fit_edge :
  Properties.line_fit_edge -> Variables_intf.any_var list
(** [vars_of_line_fit_edge v] is the variables [v] references. *)

val vars_of_ruby_align : Properties.ruby_align -> Variables_intf.any_var list
(** [vars_of_ruby_align v] is the variables [v] references. *)

val vars_of_ruby_merge : Properties.ruby_merge -> Variables_intf.any_var list
(** [vars_of_ruby_merge v] is the variables [v] references. *)

val vars_of_ruby_overhang :
  Properties.ruby_overhang -> Variables_intf.any_var list
(** [vars_of_ruby_overhang v] is the variables [v] references. *)

val vars_of_ruby_position :
  Properties.ruby_position -> Variables_intf.any_var list
(** [vars_of_ruby_position v] is the variables [v] references. *)

val vars_of_text_spacing_trim :
  Properties.text_spacing_trim -> Variables_intf.any_var list
(** [vars_of_text_spacing_trim v] is the variables [v] references. *)

val vars_of_hyphenate_limit_chars :
  Properties.hyphenate_limit_chars -> Variables_intf.any_var list
(** [vars_of_hyphenate_limit_chars v] is the variables [v] references. *)

val vars_of_initial_letter :
  Properties.initial_letter -> Variables_intf.any_var list
(** [vars_of_initial_letter v] is the variables [v] references. *)

val vars_of_white_space : Properties.white_space -> Variables_intf.any_var list
(** [vars_of_white_space v] is the variables [v] references. *)

val vars_of_font_variant :
  Properties.font_variant -> Variables_intf.any_var list
(** [vars_of_font_variant v] is the variables [v] references. *)

val vars_of_font_variant_alternates :
  Properties.font_variant_alternates -> Variables_intf.any_var list
(** [vars_of_font_variant_alternates v] is the variables [v] references. *)

val vars_of_white_space_collapse :
  Properties.white_space_collapse -> Variables_intf.any_var list
(** [vars_of_white_space_collapse v] is the variables [v] references. *)

val vars_of_word_break : Properties.word_break -> Variables_intf.any_var list
(** [vars_of_word_break v] is the variables [v] references. *)

val vars_of_overflow_wrap :
  Properties.overflow_wrap -> Variables_intf.any_var list
(** [vars_of_overflow_wrap v] is the variables [v] references. *)

val vars_of_line_break : Properties.line_break -> Variables_intf.any_var list
(** [vars_of_line_break v] is the variables [v] references. *)

val vars_of_list_style_position :
  Properties.list_style_position -> Variables_intf.any_var list
(** [vars_of_list_style_position v] is the variables [v] references. *)

val vars_of_decoration_skip_ink :
  Properties.text_decoration_skip_ink -> Variables_intf.any_var list
(** [vars_of_decoration_skip_ink v] is the variables [v] references. *)

val vars_of_unicode_bidi :
  Properties.unicode_bidi -> Variables_intf.any_var list
(** [vars_of_unicode_bidi v] is the variables [v] references. *)

val vars_of_writing_mode :
  Properties.writing_mode -> Variables_intf.any_var list
(** [vars_of_writing_mode v] is the variables [v] references. *)

val vars_of_text_combine_upright :
  Properties.text_combine_upright -> Variables_intf.any_var list
(** [vars_of_text_combine_upright v] is the variables [v] references. *)

val vars_of_webkit_font_smoothing :
  Properties.webkit_font_smoothing -> Variables_intf.any_var list
(** [vars_of_webkit_font_smoothing v] is the variables [v] references. *)

val vars_of_osx_font_smoothing :
  Properties.moz_osx_font_smoothing -> Variables_intf.any_var list
(** [vars_of_osx_font_smoothing v] is the variables [v] references. *)

val vars_of_font_stretch :
  Properties.font_stretch -> Variables_intf.any_var list
(** [vars_of_font_stretch v] is the variables [v] references. *)

val vars_of_font_optical_sizing :
  Properties.font_optical_sizing -> Variables_intf.any_var list
(** [vars_of_font_optical_sizing v] is the variables [v] references. *)

val vars_of_font_kerning :
  Properties.font_kerning -> Variables_intf.any_var list
(** [vars_of_font_kerning v] is the variables [v] references. *)

val vars_of_font_language_override :
  Properties.font_language_override -> Variables_intf.any_var list
(** [vars_of_font_language_override v] is the variables [v] references. *)

val vars_of_font_synthesis_style :
  Properties.font_synthesis_style -> Variables_intf.any_var list
(** [vars_of_font_synthesis_style v] is the variables [v] references. *)

val vars_of_font_synthesis_weight :
  Properties.font_synthesis_weight -> Variables_intf.any_var list
(** [vars_of_font_synthesis_weight v] is the variables [v] references. *)

val vars_of_synthesis_small_caps :
  Properties.font_synthesis_small_caps -> Variables_intf.any_var list
(** [vars_of_synthesis_small_caps v] is the variables [v] references. *)

val vars_of_font_synthesis_position :
  Properties.font_synthesis_position -> Variables_intf.any_var list
(** [vars_of_font_synthesis_position v] is the variables [v] references. *)

val vars_of_font_variant_ligatures :
  Properties.font_variant_ligatures -> Variables_intf.any_var list
(** [vars_of_font_variant_ligatures v] is the variables [v] references. *)

val vars_of_font_variant_caps :
  Properties.font_variant_caps -> Variables_intf.any_var list
(** [vars_of_font_variant_caps v] is the variables [v] references. *)

val vars_of_font_variant_position :
  Properties.font_variant_position -> Variables_intf.any_var list
(** [vars_of_font_variant_position v] is the variables [v] references. *)

val vars_of_east_asian :
  Properties.font_variant_east_asian -> Variables_intf.any_var list
(** [vars_of_east_asian v] is the variables [v] references. *)

val vars_of_font_size_adjust :
  Properties.font_size_adjust -> Variables_intf.any_var list
(** [vars_of_font_size_adjust v] is the variables [v] references. *)

val vars_of_font_variant_emoji :
  Properties.font_variant_emoji -> Variables_intf.any_var list
(** [vars_of_font_variant_emoji v] is the variables [v] references. *)

val vars_of_text_size_adjust :
  Properties.text_size_adjust -> Variables_intf.any_var list
(** [vars_of_text_size_adjust v] is the variables [v] references. *)

val vars_of_direction : Properties.direction -> Variables_intf.any_var list
(** [vars_of_direction v] is the variables [v] references. *)

val vars_of_caption_side :
  Properties.caption_side -> Variables_intf.any_var list
(** [vars_of_caption_side v] is the variables [v] references. *)

val vars_of_dominant_baseline :
  Properties.dominant_baseline -> Variables_intf.any_var list
(** [vars_of_dominant_baseline v] is the variables [v] references. *)

val vars_of_hyphens : Properties.hyphens -> Variables_intf.any_var list
(** [vars_of_hyphens v] is the variables [v] references. *)

val vars_of_initial_letter_align :
  Properties.initial_letter_align -> Variables_intf.any_var list
(** [vars_of_initial_letter_align v] is the variables [v] references. *)

val vars_of_initial_letter_wrap :
  Properties.initial_letter_wrap -> Variables_intf.any_var list
(** [vars_of_initial_letter_wrap v] is the variables [v] references. *)

val vars_of_text_emphasis_skip :
  Properties.text_emphasis_skip -> Variables_intf.any_var list
(** [vars_of_text_emphasis_skip v] is the variables [v] references. *)

val vars_of_font : Properties.font -> Variables_intf.any_var list
(** [vars_of_font v] is the variables [v] references. *)

val vars_of_list_style : Properties.list_style -> Variables_intf.any_var list
(** [vars_of_list_style v] is the variables [v] references. *)
