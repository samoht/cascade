open Common
open Values
include Properties_intf
include Prop_common
include Prop_svg
include Prop_multicol
include Prop_writing
include Prop_align
include Prop_flex
include Prop_grid
include Prop_image
include Prop_background
include Prop_filter
include Prop_mask
include Prop_transform
include Prop_ui
include Prop_layout
include Prop_box
include Prop_font
include Prop_text
include Prop_animation

let pp_list_style_symbol_sep ctx first (symbol : list_style_symbol) =
  if !first then first := false
  else
    match symbol with
    | String _ when Pp.minified ctx -> ()
    | _ -> Pp.space ctx ()

let pp_symbols_type ctx (kind : symbols_type) =
  match kind with
  | Cyclic -> Pp.string ctx "cyclic"
  | Numeric -> Pp.string ctx "numeric"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Symbolic -> Pp.string ctx "symbolic"
  | Fixed -> Pp.string ctx "fixed"

let pp_list_style_symbol ctx (symbol : list_style_symbol) =
  match symbol with
  | String symbol -> Pp.quoted_string ctx symbol
  | Url url -> Pp.url ctx url

let pp_list_style_symbols ctx (kind, symbols) =
  let first = ref true in
  let sep symbol = pp_list_style_symbol_sep ctx first symbol in
  let kind =
    match kind with
    | Option.Some Symbolic when Pp.minified ctx -> Option.None
    | kind -> (kind : symbols_type option)
  in
  Option.iter
    (fun kind ->
      sep (String "");
      pp_symbols_type ctx kind)
    kind;
  List.iter
    (fun symbol ->
      sep symbol;
      pp_list_style_symbol ctx symbol)
    symbols

let rec pp_list_style_type : list_style_type Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Disc -> Pp.string ctx "disc"
  | Circle -> Pp.string ctx "circle"
  | Square -> Pp.string ctx "square"
  | Decimal -> Pp.string ctx "decimal"
  | Lower_alpha -> Pp.string ctx "lower-alpha"
  | Upper_alpha -> Pp.string ctx "upper-alpha"
  | Lower_roman -> Pp.string ctx "lower-roman"
  | Upper_roman -> Pp.string ctx "upper-roman"
  | Decimal_leading_zero -> Pp.string ctx "decimal-leading-zero"
  | Arabic_indic -> Pp.string ctx "arabic-indic"
  | Armenian -> Pp.string ctx "armenian"
  | Upper_armenian -> Pp.string ctx "upper-armenian"
  | Lower_armenian -> Pp.string ctx "lower-armenian"
  | Bengali -> Pp.string ctx "bengali"
  | Cambodian -> Pp.string ctx "cambodian"
  | Khmer -> Pp.string ctx "khmer"
  | Cjk_decimal -> Pp.string ctx "cjk-decimal"
  | Devanagari -> Pp.string ctx "devanagari"
  | Georgian -> Pp.string ctx "georgian"
  | Gujarati -> Pp.string ctx "gujarati"
  | Gurmukhi -> Pp.string ctx "gurmukhi"
  | Hebrew -> Pp.string ctx "hebrew"
  | Kannada -> Pp.string ctx "kannada"
  | Lao -> Pp.string ctx "lao"
  | Malayalam -> Pp.string ctx "malayalam"
  | Mongolian -> Pp.string ctx "mongolian"
  | Myanmar -> Pp.string ctx "myanmar"
  | Oriya -> Pp.string ctx "oriya"
  | Persian -> Pp.string ctx "persian"
  | Tamil -> Pp.string ctx "tamil"
  | Telugu -> Pp.string ctx "telugu"
  | Thai -> Pp.string ctx "thai"
  | Tibetan -> Pp.string ctx "tibetan"
  | Lower_latin -> Pp.string ctx "lower-latin"
  | Upper_latin -> Pp.string ctx "upper-latin"
  | Cjk_earthly_branch -> Pp.string ctx "cjk-earthly-branch"
  | Cjk_heavenly_stem -> Pp.string ctx "cjk-heavenly-stem"
  | Lower_greek -> Pp.string ctx "lower-greek"
  | Hiragana -> Pp.string ctx "hiragana"
  | Hiragana_iroha -> Pp.string ctx "hiragana-iroha"
  | Katakana -> Pp.string ctx "katakana"
  | Katakana_iroha -> Pp.string ctx "katakana-iroha"
  | Disclosure_open -> Pp.string ctx "disclosure-open"
  | Disclosure_closed -> Pp.string ctx "disclosure-closed"
  | Cjk_ideographic -> Pp.string ctx "cjk-ideographic"
  | Japanese_informal -> Pp.string ctx "japanese-informal"
  | Japanese_formal -> Pp.string ctx "japanese-formal"
  | Korean_hangul_formal -> Pp.string ctx "korean-hangul-formal"
  | Korean_hanja_informal -> Pp.string ctx "korean-hanja-informal"
  | Korean_hanja_formal -> Pp.string ctx "korean-hanja-formal"
  | Simp_chinese_informal -> Pp.string ctx "simp-chinese-informal"
  | Simp_chinese_formal -> Pp.string ctx "simp-chinese-formal"
  | Trad_chinese_informal -> Pp.string ctx "trad-chinese-informal"
  | Trad_chinese_formal -> Pp.string ctx "trad-chinese-formal"
  | Ethiopic_numeric -> Pp.string ctx "ethiopic-numeric"
  | Name name -> pp_ident ctx name
  | String s -> Pp.quoted_string ctx s
  | Symbols (kind, symbols) ->
      Pp.call "symbols" pp_list_style_symbols ctx (kind, symbols)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style_type ctx v

let rec pp_list_style_position : list_style_position Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_list_style_position ctx v
  | Inside -> Pp.string ctx "inside"
  | Outside -> Pp.string ctx "outside"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_list_style_image : list_style_image Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Image i -> pp_background_image ctx i
  | Var v -> pp_var pp_list_style_image ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_list_style_shorthand : list_style_shorthand Pp.t =
 fun ctx { type_; position; image } ->
  let is_none_type = type_ = Some (None : list_style_type) in
  let is_none_image = image = Some (None : list_style_image) in
  (* CSS Lists 3 (ED) sec. 3.6: "a value of none in the shorthand must be
     applied to whichever of the two properties aren't otherwise set by the
     shorthand", so a lone [none] with no position reads back as the same pair
     of nones the two keywords spell out. *)
  if is_none_type && is_none_image && position = Option.None && Pp.minified ctx
  then Pp.string ctx "none"
  else
    let first = ref true in
    let emit pp = function
      | Option.None -> ()
      | Some v ->
          if !first then first := false else Pp.space ctx ();
          pp ctx v
    in
    let ambiguous_type =
      match type_ with
      | Some (Name name) ->
          let lower = String.lowercase_ascii name in
          lower = "inside" || lower = "outside"
      | _ -> false
    in
    (* A position-shaped counter name must follow an explicit position, even
       when normalisation has removed the default [outside] slot. *)
    if ambiguous_type then begin
      emit pp_list_style_position
        (Some (Option.value ~default:Outside position));
      emit pp_list_style_type type_
    end
    else begin
      emit pp_list_style_type type_;
      emit pp_list_style_position position
    end;
    emit pp_list_style_image image;
    (* Everything was an initial value and got dropped: the shorthand still
       needs one token. Emit the type initial [disc] (the shortest spelling of
       the all-initial value), not the position initial [outside] - a lone
       [outside] would set the position, changing nothing, but [disc] is shorter
       and is the canonical single-value form. *)
    if !first then Pp.string ctx "disc"

(* CSS Lists 3 (ED) sec. 3.6: [list-style] is [<'list-style-position'> ||
   <'list-style-image'> || <'list-style-type'>], so a component left out of the
   shorthand takes its longhand initial - [outside] (sec. 3.5), [none] (sec.
   3.3) and [disc] (sec. 3.4). Writing an initial out names what leaving it out
   names, and leaving it out is the shorter spelling. *)
let normalize_list_style_shorthand (s : list_style_shorthand) :
    list_style_shorthand =
  let type_ =
    drop_default ~is_default:(fun (t : list_style_type) -> t = Disc) s.type_
  in
  let position =
    drop_default
      ~is_default:(fun (p : list_style_position) -> p = Outside)
      s.position
  in
  let image =
    drop_default ~is_default:(fun (i : list_style_image) -> i = None) s.image
  in
  if
    option_is_phys_same type_ s.type_
    && option_is_phys_same position s.position
    && option_is_phys_same image s.image
  then s
  else { type_; position; image }

let normalize_list_style : list_style -> list_style = function
  | Shorthand s as value ->
      let s' = normalize_list_style_shorthand s in
      if s' == s then value else Shorthand s'
  | value -> value

let rec pp_list_style : list_style Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_list_style_shorthand ctx sh
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style ctx v

let pp_property : type a. a property Pp.t =
 fun ctx -> function
  (* CSS Syntax 3 (ED) sec. 4.3.7 lets an escape carry a [;], a [}] or any other
     non-name code point into a custom-property name, so the name is written
     back with the escapes that read it as the same name. Printed raw it ends
     its own declaration or closes the rule around it. *)
  | Custom_property name -> Pp.string ctx (Parser.escape_ident name)
  | Unknown_property name -> Pp.string ctx name
  | All -> Pp.string ctx "all"
  | Background_color -> Pp.string ctx "background-color"
  | Color -> Pp.string ctx "color"
  | Border_color -> Pp.string ctx "border-color"
  | Border_style -> Pp.string ctx "border-style"
  | Border_top_style -> Pp.string ctx "border-top-style"
  | Border_right_style -> Pp.string ctx "border-right-style"
  | Border_bottom_style -> Pp.string ctx "border-bottom-style"
  | Border_left_style -> Pp.string ctx "border-left-style"
  | Border_inline_start_style -> Pp.string ctx "border-inline-start-style"
  | Border_inline_end_style -> Pp.string ctx "border-inline-end-style"
  | Border_block_start_style -> Pp.string ctx "border-block-start-style"
  | Border_block_end_style -> Pp.string ctx "border-block-end-style"
  | Padding -> Pp.string ctx "padding"
  | Padding_left -> Pp.string ctx "padding-left"
  | Padding_right -> Pp.string ctx "padding-right"
  | Padding_bottom -> Pp.string ctx "padding-bottom"
  | Padding_top -> Pp.string ctx "padding-top"
  | Padding_inline -> Pp.string ctx "padding-inline"
  | Padding_inline_start -> Pp.string ctx "padding-inline-start"
  | Padding_inline_end -> Pp.string ctx "padding-inline-end"
  | Padding_block -> Pp.string ctx "padding-block"
  | Padding_block_start -> Pp.string ctx "padding-block-start"
  | Padding_block_end -> Pp.string ctx "padding-block-end"
  | Margin -> Pp.string ctx "margin"
  | Margin_inline_end -> Pp.string ctx "margin-inline-end"
  | Margin_inline_start -> Pp.string ctx "margin-inline-start"
  | Margin_left -> Pp.string ctx "margin-left"
  | Margin_right -> Pp.string ctx "margin-right"
  | Margin_top -> Pp.string ctx "margin-top"
  | Margin_bottom -> Pp.string ctx "margin-bottom"
  | Margin_inline -> Pp.string ctx "margin-inline"
  | Margin_block -> Pp.string ctx "margin-block"
  | Margin_block_start -> Pp.string ctx "margin-block-start"
  | Margin_block_end -> Pp.string ctx "margin-block-end"
  | Gap -> Pp.string ctx "gap"
  | Column_gap -> Pp.string ctx "column-gap"
  | Row_gap -> Pp.string ctx "row-gap"
  | Width -> Pp.string ctx "width"
  | Height -> Pp.string ctx "height"
  | Min_width -> Pp.string ctx "min-width"
  | Min_height -> Pp.string ctx "min-height"
  | Max_width -> Pp.string ctx "max-width"
  | Max_height -> Pp.string ctx "max-height"
  | Inline_size -> Pp.string ctx "inline-size"
  | Min_inline_size -> Pp.string ctx "min-inline-size"
  | Max_inline_size -> Pp.string ctx "max-inline-size"
  | Block_size -> Pp.string ctx "block-size"
  | Min_block_size -> Pp.string ctx "min-block-size"
  | Max_block_size -> Pp.string ctx "max-block-size"
  | Font_size -> Pp.string ctx "font-size"
  | Line_height -> Pp.string ctx "line-height"
  | Font_weight -> Pp.string ctx "font-weight"
  | Font_style -> Pp.string ctx "font-style"
  | Text_align -> Pp.string ctx "text-align"
  | Text_decoration -> Pp.string ctx "text-decoration"
  | Text_decoration_line -> Pp.string ctx "text-decoration-line"
  | Text_decoration_style -> Pp.string ctx "text-decoration-style"
  | Text_decoration_color -> Pp.string ctx "text-decoration-color"
  | Text_decoration_thickness -> Pp.string ctx "text-decoration-thickness"
  | Text_underline_offset -> Pp.string ctx "text-underline-offset"
  | Text_decoration_skip -> Pp.string ctx "text-decoration-skip"
  | Text_decoration_skip_self -> Pp.string ctx "text-decoration-skip-self"
  | Text_decoration_skip_box -> Pp.string ctx "text-decoration-skip-box"
  | Text_decoration_skip_inset -> Pp.string ctx "text-decoration-skip-inset"
  | Text_decoration_skip_spaces -> Pp.string ctx "text-decoration-skip-spaces"
  | Text_emphasis -> Pp.string ctx "text-emphasis"
  | Text_emphasis_style -> Pp.string ctx "text-emphasis-style"
  | Text_emphasis_color -> Pp.string ctx "text-emphasis-color"
  | Text_emphasis_position -> Pp.string ctx "text-emphasis-position"
  | Text_emphasis_skip -> Pp.string ctx "text-emphasis-skip"
  | Text_orientation -> Pp.string ctx "text-orientation"
  | Text_transform -> Pp.string ctx "text-transform"
  | Letter_spacing -> Pp.string ctx "letter-spacing"
  | List_style_type -> Pp.string ctx "list-style-type"
  | List_style_position -> Pp.string ctx "list-style-position"
  | List_style_image -> Pp.string ctx "list-style-image"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Baseline_source -> Pp.string ctx "baseline-source"
  | Alignment_baseline -> Pp.string ctx "alignment-baseline"
  | Baseline_shift -> Pp.string ctx "baseline-shift"
  | Flex_direction -> Pp.string ctx "flex-direction"
  | Flex_wrap -> Pp.string ctx "flex-wrap"
  | Flex_flow -> Pp.string ctx "flex-flow"
  | Flex -> Pp.string ctx "flex"
  | Flex_grow -> Pp.string ctx "flex-grow"
  | Flex_shrink -> Pp.string ctx "flex-shrink"
  | Flex_basis -> Pp.string ctx "flex-basis"
  | Order -> Pp.string ctx "order"
  | Align_items -> Pp.string ctx "align-items"
  | Justify_content -> Pp.string ctx "justify-content"
  | Justify_items -> Pp.string ctx "justify-items"
  | Align_content -> Pp.string ctx "align-content"
  | Align_self -> Pp.string ctx "align-self"
  | Justify_self -> Pp.string ctx "justify-self"
  | Place_content -> Pp.string ctx "place-content"
  | Place_items -> Pp.string ctx "place-items"
  | Place_self -> Pp.string ctx "place-self"
  | Grid_template_columns -> Pp.string ctx "grid-template-columns"
  | Grid_template_rows -> Pp.string ctx "grid-template-rows"
  | Grid_template_areas -> Pp.string ctx "grid-template-areas"
  | Grid_template -> Pp.string ctx "grid-template"
  | Grid -> Pp.string ctx "grid"
  | Grid_area -> Pp.string ctx "grid-area"
  | Grid_auto_flow -> Pp.string ctx "grid-auto-flow"
  | Grid_auto_columns -> Pp.string ctx "grid-auto-columns"
  | Grid_auto_rows -> Pp.string ctx "grid-auto-rows"
  | Grid_column -> Pp.string ctx "grid-column"
  | Grid_row -> Pp.string ctx "grid-row"
  | Grid_column_start -> Pp.string ctx "grid-column-start"
  | Grid_column_end -> Pp.string ctx "grid-column-end"
  | Grid_row_start -> Pp.string ctx "grid-row-start"
  | Grid_row_end -> Pp.string ctx "grid-row-end"
  | Border_width -> Pp.string ctx "border-width"
  | Border_top_width -> Pp.string ctx "border-top-width"
  | Border_right_width -> Pp.string ctx "border-right-width"
  | Border_bottom_width -> Pp.string ctx "border-bottom-width"
  | Border_left_width -> Pp.string ctx "border-left-width"
  | Border_inline_start_width -> Pp.string ctx "border-inline-start-width"
  | Border_inline_end_width -> Pp.string ctx "border-inline-end-width"
  | Border_block_start_width -> Pp.string ctx "border-block-start-width"
  | Border_block_end_width -> Pp.string ctx "border-block-end-width"
  | Border_image -> Pp.string ctx "border-image"
  | Border_radius -> Pp.string ctx "border-radius"
  | Border_top_left_radius -> Pp.string ctx "border-top-left-radius"
  | Border_top_right_radius -> Pp.string ctx "border-top-right-radius"
  | Border_bottom_left_radius -> Pp.string ctx "border-bottom-left-radius"
  | Border_bottom_right_radius -> Pp.string ctx "border-bottom-right-radius"
  | Border_top_color -> Pp.string ctx "border-top-color"
  | Border_right_color -> Pp.string ctx "border-right-color"
  | Border_bottom_color -> Pp.string ctx "border-bottom-color"
  | Border_left_color -> Pp.string ctx "border-left-color"
  | Border_inline_start_color -> Pp.string ctx "border-inline-start-color"
  | Border_inline_end_color -> Pp.string ctx "border-inline-end-color"
  | Border_block_start_color -> Pp.string ctx "border-block-start-color"
  | Border_block_end_color -> Pp.string ctx "border-block-end-color"
  | Border_inline_color -> Pp.string ctx "border-inline-color"
  | Border_block_color -> Pp.string ctx "border-block-color"
  | Border_inline_width -> Pp.string ctx "border-inline-width"
  | Border_block_width -> Pp.string ctx "border-block-width"
  | Border_inline_style -> Pp.string ctx "border-inline-style"
  | Border_block_style -> Pp.string ctx "border-block-style"
  | Border_start_start_radius -> Pp.string ctx "border-start-start-radius"
  | Border_start_end_radius -> Pp.string ctx "border-start-end-radius"
  | Border_end_start_radius -> Pp.string ctx "border-end-start-radius"
  | Border_end_end_radius -> Pp.string ctx "border-end-end-radius"
  | Box_shadow -> Pp.string ctx "box-shadow"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | Stroke_width -> Pp.string ctx "stroke-width"
  | Fill_rule -> Pp.string ctx "fill-rule"
  | Clip_rule -> Pp.string ctx "clip-rule"
  | Stroke_linecap -> Pp.string ctx "stroke-linecap"
  | Stroke_linejoin -> Pp.string ctx "stroke-linejoin"
  | Stroke_miterlimit -> Pp.string ctx "stroke-miterlimit"
  | Stroke_dashoffset -> Pp.string ctx "stroke-dashoffset"
  | Stroke_dasharray -> Pp.string ctx "stroke-dasharray"
  | Paint_order -> Pp.string ctx "paint-order"
  | Vector_effect -> Pp.string ctx "vector-effect"
  | Stop_color -> Pp.string ctx "stop-color"
  | Flood_color -> Pp.string ctx "flood-color"
  | Lighting_color -> Pp.string ctx "lighting-color"
  | Opacity -> Pp.string ctx "opacity"
  | Fill_opacity -> Pp.string ctx "fill-opacity"
  | Stroke_opacity -> Pp.string ctx "stroke-opacity"
  | Stop_opacity -> Pp.string ctx "stop-opacity"
  | Flood_opacity -> Pp.string ctx "flood-opacity"
  | Mix_blend_mode -> Pp.string ctx "mix-blend-mode"
  | Transition -> Pp.string ctx "transition"
  | Transform -> Pp.string ctx "transform"
  | Translate -> Pp.string ctx "translate"
  | Cursor -> Pp.string ctx "cursor"
  | Interactivity -> Pp.string ctx "interactivity"
  | Caret_animation -> Pp.string ctx "caret-animation"
  | Caret_shape -> Pp.string ctx "caret-shape"
  | Caret -> Pp.string ctx "caret"
  | Interest_delay -> Pp.string ctx "interest-delay"
  | Interest_delay_start -> Pp.string ctx "interest-delay-start"
  | Interest_delay_end -> Pp.string ctx "interest-delay-end"
  | Nav_up -> Pp.string ctx "nav-up"
  | Nav_right -> Pp.string ctx "nav-right"
  | Nav_down -> Pp.string ctx "nav-down"
  | Nav_left -> Pp.string ctx "nav-left"
  | Table_layout -> Pp.string ctx "table-layout"
  | Border_collapse -> Pp.string ctx "border-collapse"
  | Border_spacing -> Pp.string ctx "border-spacing"
  | User_select -> Pp.string ctx "user-select"
  | Pointer_events -> Pp.string ctx "pointer-events"
  | Overflow -> Pp.string ctx "overflow"
  | Inset -> Pp.string ctx "inset"
  | Inset_inline -> Pp.string ctx "inset-inline"
  | Inset_inline_start -> Pp.string ctx "inset-inline-start"
  | Inset_inline_end -> Pp.string ctx "inset-inline-end"
  | Inset_block -> Pp.string ctx "inset-block"
  | Inset_block_start -> Pp.string ctx "inset-block-start"
  | Inset_block_end -> Pp.string ctx "inset-block-end"
  | Top -> Pp.string ctx "top"
  | Right -> Pp.string ctx "right"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Z_index -> Pp.string ctx "z-index"
  | Outline -> Pp.string ctx "outline"
  | Outline_style -> Pp.string ctx "outline-style"
  | Outline_width -> Pp.string ctx "outline-width"
  | Outline_color -> Pp.string ctx "outline-color"
  | Outline_offset -> Pp.string ctx "outline-offset"
  | Forced_color_adjust -> Pp.string ctx "forced-color-adjust"
  | Scroll_snap_type -> Pp.string ctx "scroll-snap-type"
  | Clip -> Pp.string ctx "clip"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | White_space -> Pp.string ctx "white-space"
  | White_space_collapse -> Pp.string ctx "white-space-collapse"
  | Border -> Pp.string ctx "border"
  | Background -> Pp.string ctx "background"
  | Tab_size -> Pp.string ctx "tab-size"
  | Zoom -> Pp.string ctx "zoom"
  | Webkit_text_size_adjust -> Pp.string ctx "-webkit-text-size-adjust"
  | Font_feature_settings -> Pp.string ctx "font-feature-settings"
  | Font_variation_settings -> Pp.string ctx "font-variation-settings"
  | Webkit_tap_highlight_color -> Pp.string ctx "-webkit-tap-highlight-color"
  | Webkit_text_fill_color -> Pp.string ctx "-webkit-text-fill-color"
  | Webkit_text_stroke_color -> Pp.string ctx "-webkit-text-stroke-color"
  | Webkit_text_stroke -> Pp.string ctx "-webkit-text-stroke"
  | Webkit_text_stroke_width -> Pp.string ctx "-webkit-text-stroke-width"
  | Webkit_user_select -> Pp.string ctx "-webkit-user-select"
  | Ms_user_select -> Pp.string ctx "-ms-user-select"
  | Moz_user_select -> Pp.string ctx "-moz-user-select"
  | Webkit_text_decoration -> Pp.string ctx "-webkit-text-decoration"
  | Webkit_text_decoration_color ->
      Pp.string ctx "-webkit-text-decoration-color"
  | Text_indent -> Pp.string ctx "text-indent"
  | List_style -> Pp.string ctx "list-style"
  | Font -> Pp.string ctx "font"
  | Source -> Pp.string ctx "src"
  | Webkit_appearance -> Pp.string ctx "-webkit-appearance"
  | Container_type -> Pp.string ctx "container-type"
  | Container_name -> Pp.string ctx "container-name"
  | Container -> Pp.string ctx "container"
  | Anchor_name -> Pp.string ctx "anchor-name"
  | Position_anchor -> Pp.string ctx "position-anchor"
  | Position_try_fallbacks -> Pp.string ctx "position-try-fallbacks"
  | Position_try_order -> Pp.string ctx "position-try-order"
  | Position_try -> Pp.string ctx "position-try"
  | Position_visibility -> Pp.string ctx "position-visibility"
  | Position_area -> Pp.string ctx "position-area"
  | Shape_outside -> Pp.string ctx "shape-outside"
  | Shape_margin -> Pp.string ctx "shape-margin"
  | Shape_image_threshold -> Pp.string ctx "shape-image-threshold"
  | Overflow_clip_margin -> Pp.string ctx "overflow-clip-margin"
  | Overflow_anchor -> Pp.string ctx "overflow-anchor"
  | Scrollbar_width -> Pp.string ctx "scrollbar-width"
  | Scrollbar_color -> Pp.string ctx "scrollbar-color"
  | Scrollbar_gutter -> Pp.string ctx "scrollbar-gutter"
  | Line_height_step -> Pp.string ctx "line-height-step"
  | Font_palette -> Pp.string ctx "font-palette"
  | Font_synthesis -> Pp.string ctx "font-synthesis"
  | Text_wrap_mode -> Pp.string ctx "text-wrap-mode"
  | Text_wrap_style -> Pp.string ctx "text-wrap-style"
  | Text_box_trim -> Pp.string ctx "text-box-trim"
  | Text_underline_position -> Pp.string ctx "text-underline-position"
  | Text_box_edge -> Pp.string ctx "text-box-edge"
  | Text_box -> Pp.string ctx "text-box"
  | Inline_sizing -> Pp.string ctx "inline-sizing"
  | Line_fit_edge -> Pp.string ctx "line-fit-edge"
  | Interpolate_size -> Pp.string ctx "interpolate-size"
  | Min_intrinsic_sizing -> Pp.string ctx "min-intrinsic-sizing"
  | Ruby_align -> Pp.string ctx "ruby-align"
  | Ruby_merge -> Pp.string ctx "ruby-merge"
  | Ruby_overhang -> Pp.string ctx "ruby-overhang"
  | Ruby_position -> Pp.string ctx "ruby-position"
  | Glyph_orientation_vertical -> Pp.string ctx "glyph-orientation-vertical"
  | Animation_timeline -> Pp.string ctx "animation-timeline"
  | Animation_range -> Pp.string ctx "animation-range"
  | Animation_range_start -> Pp.string ctx "animation-range-start"
  | Animation_range_end -> Pp.string ctx "animation-range-end"
  | Scroll_timeline -> Pp.string ctx "scroll-timeline"
  | Scroll_timeline_name -> Pp.string ctx "scroll-timeline-name"
  | Scroll_timeline_axis -> Pp.string ctx "scroll-timeline-axis"
  | View_transition_name -> Pp.string ctx "view-transition-name"
  | View_transition_class -> Pp.string ctx "view-transition-class"
  | Image_orientation -> Pp.string ctx "image-orientation"
  | Image_rendering -> Pp.string ctx "image-rendering"
  | Image_resolution -> Pp.string ctx "image-resolution"
  | Contain_intrinsic_size -> Pp.string ctx "contain-intrinsic-size"
  | Contain_intrinsic_width -> Pp.string ctx "contain-intrinsic-width"
  | Contain_intrinsic_height -> Pp.string ctx "contain-intrinsic-height"
  | Contain_intrinsic_block_size -> Pp.string ctx "contain-intrinsic-block-size"
  | Contain_intrinsic_inline_size ->
      Pp.string ctx "contain-intrinsic-inline-size"
  | Margin_trim -> Pp.string ctx "margin-trim"
  | Offset_path -> Pp.string ctx "offset-path"
  | Offset -> Pp.string ctx "offset"
  | Offset_anchor -> Pp.string ctx "offset-anchor"
  | Offset_position -> Pp.string ctx "offset-position"
  | Offset_distance -> Pp.string ctx "offset-distance"
  | Offset_rotate -> Pp.string ctx "offset-rotate"
  | Font_size_adjust -> Pp.string ctx "font-size-adjust"
  | Font_variant_emoji -> Pp.string ctx "font-variant-emoji"
  | Text_spacing_trim -> Pp.string ctx "text-spacing-trim"
  | Hyphenate_limit_chars -> Pp.string ctx "hyphenate-limit-chars"
  | Initial_letter -> Pp.string ctx "initial-letter"
  | Initial_letter_align -> Pp.string ctx "initial-letter-align"
  | Initial_letter_wrap -> Pp.string ctx "initial-letter-wrap"
  | Dominant_baseline -> Pp.string ctx "dominant-baseline"
  | View_timeline_name -> Pp.string ctx "view-timeline-name"
  | View_timeline_axis -> Pp.string ctx "view-timeline-axis"
  | View_timeline_inset -> Pp.string ctx "view-timeline-inset"
  | View_timeline -> Pp.string ctx "view-timeline"
  | Timeline_scope -> Pp.string ctx "timeline-scope"
  | Perspective -> Pp.string ctx "perspective"
  | Perspective_origin -> Pp.string ctx "perspective-origin"
  | Transform_style -> Pp.string ctx "transform-style"
  | Backface_visibility -> Pp.string ctx "backface-visibility"
  | Object_position -> Pp.string ctx "object-position"
  | Rotate -> Pp.string ctx "rotate"
  | Scale -> Pp.string ctx "scale"
  | Transition_duration -> Pp.string ctx "transition-duration"
  | Transition_timing_function -> Pp.string ctx "transition-timing-function"
  | Transition_delay -> Pp.string ctx "transition-delay"
  | Transition_property -> Pp.string ctx "transition-property"
  | Transition_behavior -> Pp.string ctx "transition-behavior"
  | Overlay -> Pp.string ctx "overlay"
  | Will_change -> Pp.string ctx "will-change"
  | Contain -> Pp.string ctx "contain"
  | Isolation -> Pp.string ctx "isolation"
  | Break_before -> Pp.string ctx "break-before"
  | Break_after -> Pp.string ctx "break-after"
  | Break_inside -> Pp.string ctx "break-inside"
  | Page_break_before ->
      Pp.string ctx
        (if Pp.minified ctx then "break-before" else "page-break-before")
  | Page_break_after ->
      Pp.string ctx
        (if Pp.minified ctx then "break-after" else "page-break-after")
  | Page_break_inside ->
      Pp.string ctx
        (if Pp.minified ctx then "break-inside" else "page-break-inside")
  | Page_size -> Pp.string ctx "size"
  | Columns -> Pp.string ctx "columns"
  | Column_width -> Pp.string ctx "column-width"
  | Column_height -> Pp.string ctx "column-height"
  | Column_wrap -> Pp.string ctx "column-wrap"
  | Column_count -> Pp.string ctx "column-count"
  | Column_rule -> Pp.string ctx "column-rule"
  | Column_rule_width -> Pp.string ctx "column-rule-width"
  | Column_rule_style -> Pp.string ctx "column-rule-style"
  | Column_rule_color -> Pp.string ctx "column-rule-color"
  | Column_span -> Pp.string ctx "column-span"
  | Word_spacing -> Pp.string ctx "word-spacing"
  | Background_attachment -> Pp.string ctx "background-attachment"
  | Border_top -> Pp.string ctx "border-top"
  | Border_right -> Pp.string ctx "border-right"
  | Border_bottom -> Pp.string ctx "border-bottom"
  | Border_left -> Pp.string ctx "border-left"
  | Border_block -> Pp.string ctx "border-block"
  | Border_block_start -> Pp.string ctx "border-block-start"
  | Border_block_end -> Pp.string ctx "border-block-end"
  | Border_inline -> Pp.string ctx "border-inline"
  | Border_inline_start -> Pp.string ctx "border-inline-start"
  | Border_inline_end -> Pp.string ctx "border-inline-end"
  | Transform_origin -> Pp.string ctx "transform-origin"
  | Transform_box -> Pp.string ctx "transform-box"
  | Text_shadow -> Pp.string ctx "text-shadow"
  | Clip_path -> Pp.string ctx "clip-path"
  | Mask -> Pp.string ctx "mask"
  | Mask_border -> Pp.string ctx "mask-border"
  | Content_visibility -> Pp.string ctx "content-visibility"
  | Filter -> Pp.string ctx "filter"
  | Background_image -> Pp.string ctx "background-image"
  | Background_origin -> Pp.string ctx "background-origin"
  | Background_clip -> Pp.string ctx "background-clip"
  | Webkit_background_clip -> Pp.string ctx "-webkit-background-clip"
  | Animation -> Pp.string ctx "animation"
  | Aspect_ratio -> Pp.string ctx "aspect-ratio"
  | Overflow_x -> Pp.string ctx "overflow-x"
  | Overflow_y -> Pp.string ctx "overflow-y"
  | Overflow_block -> Pp.string ctx "overflow-block"
  | Overflow_inline -> Pp.string ctx "overflow-inline"
  | Vertical_align -> Pp.string ctx "vertical-align"
  | Font_family -> Pp.string ctx "font-family"
  | Background_position -> Pp.string ctx "background-position"
  | Background_position_x -> Pp.string ctx "background-position-x"
  | Background_position_y -> Pp.string ctx "background-position-y"
  | Webkit_mask_position_x -> Pp.string ctx "-webkit-mask-position-x"
  | Webkit_mask_position_y -> Pp.string ctx "-webkit-mask-position-y"
  | Background_repeat -> Pp.string ctx "background-repeat"
  | Background_size -> Pp.string ctx "background-size"
  | Webkit_font_smoothing -> Pp.string ctx "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> Pp.string ctx "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> Pp.string ctx "-webkit-line-clamp"
  | Webkit_box_orient -> Pp.string ctx "-webkit-box-orient"
  | Moz_orient -> Pp.string ctx "-moz-orient"
  | Text_overflow -> Pp.string ctx "text-overflow"
  | Text_wrap -> Pp.string ctx "text-wrap"
  | Word_break -> Pp.string ctx "word-break"
  | Overflow_wrap -> Pp.string ctx "overflow-wrap"
  | Line_break -> Pp.string ctx "line-break"
  | Hyphens -> Pp.string ctx "hyphens"
  | Webkit_hyphens -> Pp.string ctx "-webkit-hyphens"
  | Font_stretch -> Pp.string ctx "font-stretch"
  | Font_optical_sizing -> Pp.string ctx "font-optical-sizing"
  | Font_kerning -> Pp.string ctx "font-kerning"
  | Font_language_override -> Pp.string ctx "font-language-override"
  | Font_synthesis_style -> Pp.string ctx "font-synthesis-style"
  | Font_synthesis_weight -> Pp.string ctx "font-synthesis-weight"
  | Font_synthesis_small_caps -> Pp.string ctx "font-synthesis-small-caps"
  | Font_synthesis_position -> Pp.string ctx "font-synthesis-position"
  | Font_variant_ligatures -> Pp.string ctx "font-variant-ligatures"
  | Caps -> Pp.string ctx "font-variant-caps"
  | Numeric -> Pp.string ctx "font-variant-numeric"
  | Font_variant_position -> Pp.string ctx "font-variant-position"
  | Font_variant_alternates -> Pp.string ctx "font-variant-alternates"
  | Font_variant -> Pp.string ctx "font-variant"
  | East_asian -> Pp.string ctx "font-variant-east-asian"
  | Backdrop_filter -> Pp.string ctx "backdrop-filter"
  | Webkit_backdrop_filter -> Pp.string ctx "-webkit-backdrop-filter"
  | Webkit_mask_image -> Pp.string ctx "-webkit-mask-image"
  | Webkit_mask_composite -> Pp.string ctx "-webkit-mask-composite"
  | Webkit_mask_source_type -> Pp.string ctx "-webkit-mask-source-type"
  | Webkit_mask_size -> Pp.string ctx "-webkit-mask-size"
  | Webkit_mask_position -> Pp.string ctx "-webkit-mask-position"
  | Webkit_mask_repeat -> Pp.string ctx "-webkit-mask-repeat"
  | Webkit_mask_clip -> Pp.string ctx "-webkit-mask-clip"
  | Webkit_mask_origin -> Pp.string ctx "-webkit-mask-origin"
  | Border_image_source -> Pp.string ctx "border-image-source"
  | Border_image_slice -> Pp.string ctx "border-image-slice"
  | Border_image_repeat -> Pp.string ctx "border-image-repeat"
  | Border_image_width -> Pp.string ctx "border-image-width"
  | Border_image_outset -> Pp.string ctx "border-image-outset"
  | Mask_image -> Pp.string ctx "mask-image"
  | Mask_composite -> Pp.string ctx "mask-composite"
  | Mask_mode -> Pp.string ctx "mask-mode"
  | Mask_size -> Pp.string ctx "mask-size"
  | Mask_position -> Pp.string ctx "mask-position"
  | Mask_repeat -> Pp.string ctx "mask-repeat"
  | Mask_clip -> Pp.string ctx "mask-clip"
  | Mask_origin -> Pp.string ctx "mask-origin"
  | Mask_type -> Pp.string ctx "mask-type"
  | Scroll_snap_align -> Pp.string ctx "scroll-snap-align"
  | Scroll_snap_stop -> Pp.string ctx "scroll-snap-stop"
  | Scroll_behavior -> Pp.string ctx "scroll-behavior"
  | Box_sizing -> Pp.string ctx "box-sizing"
  | Webkit_box_sizing -> Pp.string ctx "-webkit-box-sizing"
  | Moz_box_sizing -> Pp.string ctx "-moz-box-sizing"
  | Field_sizing -> Pp.string ctx "field-sizing"
  | Caption_side -> Pp.string ctx "caption-side"
  | Resize -> Pp.string ctx "resize"
  | Object_fit -> Pp.string ctx "object-fit"
  | Object_view_box -> Pp.string ctx "object-view-box"
  | Appearance -> Pp.string ctx "appearance"
  | Color_scheme -> Pp.string ctx "color-scheme"
  | Print_color_adjust -> Pp.string ctx "print-color-adjust"
  | Webkit_print_color_adjust -> Pp.string ctx "-webkit-print-color-adjust"
  | Box_decoration_break -> Pp.string ctx "box-decoration-break"
  | Webkit_box_decoration_break -> Pp.string ctx "-webkit-box-decoration-break"
  | Content -> Pp.string ctx "content"
  | Counter_reset -> Pp.string ctx "counter-reset"
  | Counter_increment -> Pp.string ctx "counter-increment"
  | Quotes -> Pp.string ctx "quotes"
  | Text_size_adjust -> Pp.string ctx "text-size-adjust"
  | Touch_action -> Pp.string ctx "touch-action"
  | Direction -> Pp.string ctx "direction"
  | Unicode_bidi -> Pp.string ctx "unicode-bidi"
  | Writing_mode -> Pp.string ctx "writing-mode"
  | Text_combine_upright -> Pp.string ctx "text-combine-upright"
  | Text_decoration_skip_ink -> Pp.string ctx "text-decoration-skip-ink"
  | Animation_name -> Pp.string ctx "animation-name"
  | Animation_duration -> Pp.string ctx "animation-duration"
  | Animation_timing_function -> Pp.string ctx "animation-timing-function"
  | Animation_delay -> Pp.string ctx "animation-delay"
  | Animation_iteration_count -> Pp.string ctx "animation-iteration-count"
  | Animation_direction -> Pp.string ctx "animation-direction"
  | Animation_fill_mode -> Pp.string ctx "animation-fill-mode"
  | Animation_play_state -> Pp.string ctx "animation-play-state"
  | Animation_composition -> Pp.string ctx "animation-composition"
  | Background_blend_mode -> Pp.string ctx "background-blend-mode"
  | Scroll_margin -> Pp.string ctx "scroll-margin"
  | Scroll_margin_top -> Pp.string ctx "scroll-margin-top"
  | Scroll_margin_right -> Pp.string ctx "scroll-margin-right"
  | Scroll_margin_bottom -> Pp.string ctx "scroll-margin-bottom"
  | Scroll_margin_left -> Pp.string ctx "scroll-margin-left"
  | Scroll_margin_inline -> Pp.string ctx "scroll-margin-inline"
  | Scroll_margin_inline_start -> Pp.string ctx "scroll-margin-inline-start"
  | Scroll_margin_inline_end -> Pp.string ctx "scroll-margin-inline-end"
  | Scroll_margin_block -> Pp.string ctx "scroll-margin-block"
  | Scroll_margin_block_start -> Pp.string ctx "scroll-margin-block-start"
  | Scroll_margin_block_end -> Pp.string ctx "scroll-margin-block-end"
  | Scroll_padding -> Pp.string ctx "scroll-padding"
  | Scroll_padding_top -> Pp.string ctx "scroll-padding-top"
  | Scroll_padding_right -> Pp.string ctx "scroll-padding-right"
  | Scroll_padding_bottom -> Pp.string ctx "scroll-padding-bottom"
  | Scroll_padding_left -> Pp.string ctx "scroll-padding-left"
  | Scroll_padding_inline -> Pp.string ctx "scroll-padding-inline"
  | Scroll_padding_inline_start -> Pp.string ctx "scroll-padding-inline-start"
  | Scroll_padding_inline_end -> Pp.string ctx "scroll-padding-inline-end"
  | Scroll_padding_block -> Pp.string ctx "scroll-padding-block"
  | Scroll_padding_block_start -> Pp.string ctx "scroll-padding-block-start"
  | Scroll_padding_block_end -> Pp.string ctx "scroll-padding-block-end"
  | Overscroll_behavior -> Pp.string ctx "overscroll-behavior"
  | Overscroll_behavior_x -> Pp.string ctx "overscroll-behavior-x"
  | Overscroll_behavior_y -> Pp.string ctx "overscroll-behavior-y"
  | Overscroll_behavior_block -> Pp.string ctx "overscroll-behavior-block"
  | Overscroll_behavior_inline -> Pp.string ctx "overscroll-behavior-inline"
  | Accent_color -> Pp.string ctx "accent-color"
  | Caret_color -> Pp.string ctx "caret-color"
  | Webkit_transform -> Pp.string ctx "-webkit-transform"
  | Moz_transform -> Pp.string ctx "-moz-transform"
  | Ms_transform -> Pp.string ctx "-ms-transform"
  | O_transform -> Pp.string ctx "-o-transform"
  | Webkit_transition -> Pp.string ctx "-webkit-transition"
  | Webkit_transition_delay -> Pp.string ctx "-webkit-transition-delay"
  | Webkit_transition_duration -> Pp.string ctx "-webkit-transition-duration"
  | Webkit_transition_property -> Pp.string ctx "-webkit-transition-property"
  | Webkit_transition_timing_function ->
      Pp.string ctx "-webkit-transition-timing-function"
  | Webkit_animation -> Pp.string ctx "-webkit-animation"
  | Webkit_animation_delay -> Pp.string ctx "-webkit-animation-delay"
  | Webkit_animation_duration -> Pp.string ctx "-webkit-animation-duration"
  | Webkit_animation_direction -> Pp.string ctx "-webkit-animation-direction"
  | Webkit_animation_iteration_count ->
      Pp.string ctx "-webkit-animation-iteration-count"
  | Webkit_animation_name -> Pp.string ctx "-webkit-animation-name"
  | Webkit_animation_timing_function ->
      Pp.string ctx "-webkit-animation-timing-function"
  | Webkit_animation_fill_mode -> Pp.string ctx "-webkit-animation-fill-mode"
  | Webkit_animation_play_state -> Pp.string ctx "-webkit-animation-play-state"
  | Webkit_flex_direction -> Pp.string ctx "-webkit-flex-direction"
  | Webkit_flex_wrap -> Pp.string ctx "-webkit-flex-wrap"
  | Webkit_flex_flow -> Pp.string ctx "-webkit-flex-flow"
  | Webkit_justify_content -> Pp.string ctx "-webkit-justify-content"
  | Webkit_align_items -> Pp.string ctx "-webkit-align-items"
  | Webkit_align_content -> Pp.string ctx "-webkit-align-content"
  | Webkit_align_self -> Pp.string ctx "-webkit-align-self"
  | Webkit_border_radius -> Pp.string ctx "-webkit-border-radius"
  | Webkit_box_shadow -> Pp.string ctx "-webkit-box-shadow"
  | Webkit_background_size -> Pp.string ctx "-webkit-background-size"
  | Webkit_filter -> Pp.string ctx "-webkit-filter"
  | Moz_appearance -> Pp.string ctx "-moz-appearance"
  | Moz_animation -> Pp.string ctx "-moz-animation"
  | Moz_animation_delay -> Pp.string ctx "-moz-animation-delay"
  | Moz_animation_duration -> Pp.string ctx "-moz-animation-duration"
  | Moz_animation_direction -> Pp.string ctx "-moz-animation-direction"
  | Moz_animation_iteration_count ->
      Pp.string ctx "-moz-animation-iteration-count"
  | Moz_animation_name -> Pp.string ctx "-moz-animation-name"
  | Moz_animation_timing_function ->
      Pp.string ctx "-moz-animation-timing-function"
  | Moz_animation_fill_mode -> Pp.string ctx "-moz-animation-fill-mode"
  | Moz_animation_play_state -> Pp.string ctx "-moz-animation-play-state"
  | Moz_transition -> Pp.string ctx "-moz-transition"
  | Moz_transition_delay -> Pp.string ctx "-moz-transition-delay"
  | Moz_transition_duration -> Pp.string ctx "-moz-transition-duration"
  | Moz_transition_property -> Pp.string ctx "-moz-transition-property"
  | Moz_transition_timing_function ->
      Pp.string ctx "-moz-transition-timing-function"
  | Moz_border_radius -> Pp.string ctx "-moz-border-radius"
  | Moz_box_shadow -> Pp.string ctx "-moz-box-shadow"
  | Ms_filter -> Pp.string ctx "-ms-filter"
  | O_transition -> Pp.string ctx "-o-transition"

(* Per-property semantic metadata. Keep this exhaustive over the sealed property
   GADT: adding a constructor must decide both its default inheritance and
   whether its typed value can contain a parse-time [Invalid] arm. Names without
   a typed constructor stay in [Unknown_property], where the payload is the only
   identity available. Shorthands count as inherited when every longhand they
   reset inherits. *)
type _ property_class =
  | Inherited : 'a property_class
  | Non_inherited : 'a property_class
  | Checks_length_percentage : length_percentage property_class
  | Checks_rotate : rotate_value property_class
  | Checks_clip_path : clip_path property_class
  | Checks_text_indent : text_indent_value property_class
  | Checks_font_family : font_family property_class

let property_class : type a. a property -> a property_class = function
  | Unknown_property name -> (
      match String.lowercase_ascii name with
      | "empty-cells" | "font-variant" | "font-variant-alternates" | "orphans"
      | "text-align-last" | "text-justify" | "text-rendering" | "widows"
      | "word-wrap" ->
          Inherited
      | _ -> Non_inherited)
  | Width -> Checks_length_percentage
  | Height -> Checks_length_percentage
  | Min_width -> Checks_length_percentage
  | Min_height -> Checks_length_percentage
  | Max_width -> Checks_length_percentage
  | Max_height -> Checks_length_percentage
  | Block_size -> Checks_length_percentage
  | Inline_size -> Checks_length_percentage
  | Min_block_size -> Checks_length_percentage
  | Min_inline_size -> Checks_length_percentage
  | Max_block_size -> Checks_length_percentage
  | Max_inline_size -> Checks_length_percentage
  | Shape_margin -> Checks_length_percentage
  | Offset_distance -> Checks_length_percentage
  | Rotate -> Checks_rotate
  | Clip_path -> Checks_clip_path
  | Text_indent -> Checks_text_indent
  | Font_family -> Checks_font_family
  | Color -> Inherited
  | Font_size -> Inherited
  | Line_height -> Inherited
  | Font_weight -> Inherited
  | Font_style -> Inherited
  | Text_align -> Inherited
  | Text_underline_offset -> Inherited
  | Text_decoration_skip -> Inherited
  | Text_decoration_skip_box -> Inherited
  | Text_decoration_skip_inset -> Inherited
  | Text_decoration_skip_spaces -> Inherited
  | Text_emphasis -> Inherited
  | Text_emphasis_style -> Inherited
  | Text_emphasis_color -> Inherited
  | Text_emphasis_position -> Inherited
  | Text_emphasis_skip -> Inherited
  | Text_orientation -> Inherited
  | Text_transform -> Inherited
  | Letter_spacing -> Inherited
  | List_style_type -> Inherited
  | List_style_position -> Inherited
  | List_style_image -> Inherited
  | Visibility -> Inherited
  | Fill_opacity -> Inherited
  | Stroke_opacity -> Inherited
  | Cursor -> Inherited
  | Interactivity -> Inherited
  | Caret_animation -> Inherited
  | Caret_shape -> Inherited
  | Caret -> Inherited
  | Interest_delay -> Inherited
  | Interest_delay_start -> Inherited
  | Interest_delay_end -> Inherited
  | Border_collapse -> Inherited
  | Border_spacing -> Inherited
  | Pointer_events -> Inherited
  | Forced_color_adjust -> Inherited
  | White_space -> Inherited
  | White_space_collapse -> Inherited
  | Font_variant_alternates -> Inherited
  | Font_variant -> Inherited
  | Tab_size -> Inherited
  | Webkit_text_size_adjust -> Inherited
  | Font_feature_settings -> Inherited
  | Font_variation_settings -> Inherited
  | Webkit_tap_highlight_color -> Inherited
  | Webkit_text_fill_color -> Inherited
  | Webkit_text_stroke_color -> Inherited
  | Webkit_text_stroke -> Inherited
  | Webkit_text_stroke_width -> Inherited
  | List_style -> Inherited
  | Font -> Inherited
  | Scrollbar_color -> Inherited
  | Line_height_step -> Inherited
  | Font_palette -> Inherited
  | Font_synthesis -> Inherited
  | Text_wrap_mode -> Inherited
  | Text_wrap_style -> Inherited
  | Text_underline_position -> Inherited
  | Text_box_edge -> Inherited
  | Inline_sizing -> Inherited
  | Line_fit_edge -> Inherited
  | Interpolate_size -> Inherited
  | Ruby_align -> Inherited
  | Ruby_merge -> Inherited
  | Ruby_overhang -> Inherited
  | Ruby_position -> Inherited
  | Glyph_orientation_vertical -> Inherited
  | Text_combine_upright -> Inherited
  | Image_orientation -> Inherited
  | Image_rendering -> Inherited
  | Image_resolution -> Inherited
  | Font_size_adjust -> Inherited
  | Font_variant_emoji -> Inherited
  | Text_spacing_trim -> Inherited
  | Hyphenate_limit_chars -> Inherited
  | Initial_letter_align -> Inherited
  | Initial_letter_wrap -> Inherited
  | Dominant_baseline -> Inherited
  | Word_spacing -> Inherited
  | Text_shadow -> Inherited
  | Webkit_font_smoothing -> Inherited
  | Moz_osx_font_smoothing -> Inherited
  | Text_wrap -> Inherited
  | Word_break -> Inherited
  | Overflow_wrap -> Inherited
  | Line_break -> Inherited
  | Hyphens -> Inherited
  | Webkit_hyphens -> Inherited
  | Font_stretch -> Inherited
  | Font_optical_sizing -> Inherited
  | Font_kerning -> Inherited
  | Font_language_override -> Inherited
  | Font_synthesis_style -> Inherited
  | Font_synthesis_weight -> Inherited
  | Font_synthesis_small_caps -> Inherited
  | Font_synthesis_position -> Inherited
  | Font_variant_ligatures -> Inherited
  | Caps -> Inherited
  | Numeric -> Inherited
  | Font_variant_position -> Inherited
  | East_asian -> Inherited
  | Caption_side -> Inherited
  | Color_scheme -> Inherited
  | Print_color_adjust -> Inherited
  | Webkit_print_color_adjust -> Inherited
  | Quotes -> Inherited
  | Text_size_adjust -> Inherited
  | Fill -> Inherited
  | Stroke -> Inherited
  | Stroke_width -> Inherited
  | Fill_rule -> Inherited
  | Clip_rule -> Inherited
  | Stroke_linecap -> Inherited
  | Stroke_linejoin -> Inherited
  | Stroke_miterlimit -> Inherited
  | Stroke_dashoffset -> Inherited
  | Stroke_dasharray -> Inherited
  | Paint_order -> Inherited
  | Direction -> Inherited
  | Writing_mode -> Inherited
  | Text_decoration_skip_ink -> Inherited
  | Accent_color -> Inherited
  | Caret_color -> Inherited
  | Custom_property _ | All | Background_color | Border_color | Border_style
  | Border_top_style | Border_right_style | Border_bottom_style
  | Border_left_style | Border_inline_start_style | Border_inline_end_style
  | Border_block_start_style | Border_block_end_style | Padding | Padding_left
  | Padding_right | Padding_bottom | Padding_top | Padding_inline
  | Padding_inline_start | Padding_inline_end | Padding_block
  | Padding_block_start | Padding_block_end | Margin | Margin_inline_end
  | Margin_inline_start | Margin_left | Margin_right | Margin_top
  | Margin_bottom | Margin_inline | Margin_block | Margin_block_start
  | Margin_block_end | Gap | Column_gap | Row_gap | Text_decoration
  | Text_decoration_line | Text_decoration_style | Text_decoration_color
  | Text_decoration_skip_self | Display | Position | Baseline_source
  | Alignment_baseline | Baseline_shift | Flex_direction | Flex_wrap | Flex_flow
  | Flex | Flex_grow | Flex_shrink | Flex_basis | Order | Align_items
  | Justify_content | Justify_items | Justify_self | Align_content | Align_self
  | Place_content | Place_items | Place_self | Grid_template_columns
  | Grid_template_rows | Grid_template_areas | Grid_template | Grid | Grid_area
  | Grid_auto_flow | Grid_auto_columns | Grid_auto_rows | Grid_column | Grid_row
  | Grid_column_start | Grid_column_end | Grid_row_start | Grid_row_end
  | Border_width | Border_top_width | Border_right_width | Border_bottom_width
  | Border_left_width | Border_inline_start_width | Border_inline_end_width
  | Border_block_start_width | Border_block_end_width | Border_inline_width
  | Border_block_width | Border_image | Border_image_source | Border_image_slice
  | Border_image_repeat | Border_image_width | Border_image_outset
  | Border_radius | Border_top_left_radius | Border_top_right_radius
  | Border_bottom_left_radius | Border_bottom_right_radius | Border_top_color
  | Border_right_color | Border_bottom_color | Border_left_color
  | Border_inline_start_color | Border_inline_end_color
  | Border_block_start_color | Border_block_end_color | Border_inline_color
  | Border_block_color | Border_inline_style | Border_block_style
  | Border_start_start_radius | Border_start_end_radius
  | Border_end_start_radius | Border_end_end_radius | Opacity | Stop_opacity
  | Flood_opacity | Mix_blend_mode | Transform | Translate | Nav_up | Nav_right
  | Nav_down | Nav_left | Table_layout | User_select | Overflow | Inset
  | Inset_inline | Inset_inline_start | Inset_inline_end | Inset_block
  | Inset_block_start | Inset_block_end | Top | Right | Bottom | Left | Z_index
  | Outline | Outline_style | Outline_width | Outline_color | Outline_offset
  | Scroll_snap_type | Border | Border_block | Border_block_start
  | Border_block_end | Border_inline | Border_inline_start | Border_inline_end
  | Background | Zoom | Webkit_user_select | Moz_user_select | Ms_user_select
  | Webkit_text_decoration | Webkit_text_decoration_color | Source
  | Webkit_appearance | Webkit_transform | Moz_transform | Ms_transform
  | O_transform | Webkit_transition | Webkit_transition_delay
  | Webkit_transition_duration | Webkit_transition_property
  | Webkit_transition_timing_function | Webkit_animation
  | Webkit_animation_delay | Webkit_animation_duration
  | Webkit_animation_direction | Webkit_animation_iteration_count
  | Webkit_animation_name | Webkit_animation_timing_function
  | Webkit_animation_fill_mode | Webkit_animation_play_state
  | Webkit_flex_direction | Webkit_flex_wrap | Webkit_flex_flow
  | Webkit_justify_content | Webkit_align_items | Webkit_align_content
  | Webkit_align_self | Webkit_border_radius | Webkit_box_sizing
  | Moz_box_sizing | Webkit_box_shadow | Webkit_background_size | Webkit_filter
  | Moz_appearance | Moz_animation | Moz_animation_delay
  | Moz_animation_duration | Moz_animation_direction
  | Moz_animation_iteration_count | Moz_animation_name
  | Moz_animation_timing_function | Moz_animation_fill_mode
  | Moz_animation_play_state | Moz_transition | Moz_transition_delay
  | Moz_transition_duration | Moz_transition_property
  | Moz_transition_timing_function | Moz_border_radius | Moz_box_shadow
  | Ms_filter | O_transition | Container_type | Container_name | Container
  | Anchor_name | Position_anchor | Position_try_fallbacks | Position_try_order
  | Position_try | Position_visibility | Position_area | Shape_outside
  | Shape_image_threshold | Overflow_clip_margin | Overflow_anchor
  | Scrollbar_width | Scrollbar_gutter | Text_box_trim | Text_box
  | Min_intrinsic_sizing | Animation_timeline | Animation_range
  | Animation_range_start | Animation_range_end | Scroll_timeline
  | Scroll_timeline_name | Scroll_timeline_axis | View_transition_name
  | View_transition_class | Contain_intrinsic_size | Contain_intrinsic_width
  | Contain_intrinsic_height | Contain_intrinsic_block_size
  | Contain_intrinsic_inline_size | Margin_trim | Offset_path | Offset_rotate
  | Initial_letter | View_timeline_name | View_timeline_axis
  | View_timeline_inset | View_timeline | Timeline_scope | Perspective
  | Perspective_origin | Transform_style | Backface_visibility | Object_position
  | Transition_duration | Transition_timing_function | Transition_delay
  | Transition_property | Transition_behavior | Overlay | Will_change | Contain
  | Isolation | Break_before | Break_after | Break_inside | Page_break_before
  | Page_break_after | Page_break_inside | Page_size | Columns | Column_width
  | Column_height | Column_wrap | Column_count | Column_rule | Column_rule_color
  | Column_rule_width | Column_rule_style | Column_span | Background_attachment
  | Border_top | Border_right | Border_bottom | Border_left | Transform_origin
  | Transform_box | Mask | Mask_border | Content_visibility | Filter
  | Background_image | Background_origin | Background_clip
  | Webkit_background_clip | Animation | Aspect_ratio | Overflow_x | Overflow_y
  | Overflow_block | Overflow_inline | Vertical_align | Background_position
  | Background_position_x | Background_position_y | Webkit_mask_position_x
  | Webkit_mask_position_y | Background_repeat | Background_size
  | Webkit_line_clamp | Webkit_box_orient | Moz_orient | Text_overflow
  | Backdrop_filter | Webkit_backdrop_filter | Webkit_mask_image
  | Webkit_mask_composite | Webkit_mask_source_type | Webkit_mask_size
  | Webkit_mask_position | Webkit_mask_repeat | Webkit_mask_clip
  | Webkit_mask_origin | Mask_image | Mask_composite | Mask_mode | Mask_size
  | Mask_position | Mask_repeat | Mask_clip | Mask_origin | Mask_type
  | Scroll_snap_align | Scroll_snap_stop | Scroll_behavior | Box_sizing
  | Field_sizing | Resize | Object_fit | Object_view_box | Appearance
  | Box_decoration_break | Webkit_box_decoration_break | Content | Counter_reset
  | Counter_increment | Text_decoration_thickness | Touch_action | Clip | Clear
  | Float | Scale | Transition | Box_shadow | Vector_effect | Stop_color
  | Flood_color | Lighting_color | Unicode_bidi | Animation_name
  | Animation_duration | Animation_timing_function | Animation_delay
  | Animation_iteration_count | Animation_direction | Animation_fill_mode
  | Animation_play_state | Animation_composition | Background_blend_mode
  | Scroll_margin | Scroll_margin_top | Scroll_margin_right
  | Scroll_margin_bottom | Scroll_margin_left | Scroll_margin_inline
  | Scroll_margin_inline_start | Scroll_margin_inline_end | Scroll_margin_block
  | Scroll_margin_block_start | Scroll_margin_block_end | Scroll_padding
  | Scroll_padding_top | Scroll_padding_right | Scroll_padding_bottom
  | Scroll_padding_left | Scroll_padding_inline | Scroll_padding_inline_start
  | Scroll_padding_inline_end | Scroll_padding_block
  | Scroll_padding_block_start | Scroll_padding_block_end | Overscroll_behavior
  | Overscroll_behavior_x | Overscroll_behavior_y | Overscroll_behavior_block
  | Overscroll_behavior_inline | Offset_anchor | Offset_position | Offset ->
      Non_inherited

let property_is_inherited : type a. a property -> bool =
 fun property ->
  match property_class property with
  | Inherited -> true
  | Non_inherited -> false
  | Checks_length_percentage -> false
  | Checks_rotate -> false
  | Checks_clip_path -> false
  | Checks_text_indent -> true
  | Checks_font_family -> true

(* Whether the name [pp_property] gives [property] under minify can carry
   [value]. CSS Fragmentation 3 sec. 3.4 defines the [page-break-*] alias of
   [break-*] by a value mapping table, so the [break-*] name a [page-break-*]
   property minifies to is a spelling the declaration can use only for a value
   the table names: [always] maps to [page], the rest map to themselves, and a
   [var()] maps to nothing here since substitution happens at computed-value
   time. Every other property names itself the same whatever it carries. *)
let minified_name_carries : type a. a property -> a -> bool =
 fun property value ->
  match property with
  | Page_break_before -> Option.is_some (break_of_page_break value)
  | Page_break_after -> Option.is_some (break_of_page_break value)
  | Page_break_inside -> Option.is_some (break_inside_of_page_break value)
  | _ -> true

(* A dense integer per property constructor, in the order [Properties_intf]
   declares them. It exists so an ordered container keyed on a property
   identity compares two integers rather than walking the runtime
   representation through [caml_compare]; ordering an identity never has to
   relate the two payload types a [Declaration] pack hides, so a tag is all it
   takes.

   The match carries no wildcard, so a new constructor fails this build, and
   [scripts/check_properties.ml] pins that no two constructors take the same
   tag - a silent collision would make an ordered container hold one entry for
   two different properties. *)
(* PROPERTY_TAG_START - Used by scripts/check_properties.ml *)
let property_tag : type a. a property -> int = function
  | Custom_property _ -> 0
  | Unknown_property _ -> 1
  | All -> 2
  | Background_color -> 3
  | Color -> 4
  | Border_color -> 5
  | Border_style -> 6
  | Border_top_style -> 7
  | Border_right_style -> 8
  | Border_bottom_style -> 9
  | Border_left_style -> 10
  | Border_inline_start_style -> 11
  | Border_inline_end_style -> 12
  | Border_block_start_style -> 13
  | Border_block_end_style -> 14
  | Padding -> 15
  | Padding_left -> 16
  | Padding_right -> 17
  | Padding_bottom -> 18
  | Padding_top -> 19
  | Padding_inline -> 20
  | Padding_inline_start -> 21
  | Padding_inline_end -> 22
  | Padding_block -> 23
  | Padding_block_start -> 24
  | Padding_block_end -> 25
  | Margin -> 26
  | Margin_inline_end -> 27
  | Margin_inline_start -> 28
  | Margin_left -> 29
  | Margin_right -> 30
  | Margin_top -> 31
  | Margin_bottom -> 32
  | Margin_inline -> 33
  | Margin_block -> 34
  | Margin_block_start -> 35
  | Margin_block_end -> 36
  | Gap -> 37
  | Column_gap -> 38
  | Row_gap -> 39
  | Width -> 40
  | Height -> 41
  | Min_width -> 42
  | Min_height -> 43
  | Max_width -> 44
  | Max_height -> 45
  | Inline_size -> 46
  | Min_inline_size -> 47
  | Max_inline_size -> 48
  | Block_size -> 49
  | Min_block_size -> 50
  | Max_block_size -> 51
  | Font_size -> 52
  | Line_height -> 53
  | Font_weight -> 54
  | Font_style -> 55
  | Text_align -> 56
  | Text_decoration -> 57
  | Text_decoration_line -> 58
  | Text_decoration_style -> 59
  | Text_decoration_color -> 60
  | Text_underline_offset -> 61
  | Text_decoration_skip -> 62
  | Text_decoration_skip_self -> 63
  | Text_decoration_skip_box -> 64
  | Text_decoration_skip_inset -> 65
  | Text_decoration_skip_spaces -> 66
  | Text_emphasis -> 67
  | Text_emphasis_style -> 68
  | Text_emphasis_color -> 69
  | Text_emphasis_position -> 70
  | Text_emphasis_skip -> 71
  | Text_orientation -> 72
  | Text_transform -> 73
  | Letter_spacing -> 74
  | List_style_type -> 75
  | List_style_position -> 76
  | List_style_image -> 77
  | Display -> 78
  | Position -> 79
  | Visibility -> 80
  | Baseline_source -> 81
  | Alignment_baseline -> 82
  | Baseline_shift -> 83
  | Flex_direction -> 84
  | Flex_wrap -> 85
  | Flex_flow -> 86
  | Flex -> 87
  | Flex_grow -> 88
  | Flex_shrink -> 89
  | Flex_basis -> 90
  | Order -> 91
  | Align_items -> 92
  | Justify_content -> 93
  | Justify_items -> 94
  | Justify_self -> 95
  | Align_content -> 96
  | Align_self -> 97
  | Place_content -> 98
  | Place_items -> 99
  | Place_self -> 100
  | Grid_template_columns -> 101
  | Grid_template_rows -> 102
  | Grid_template_areas -> 103
  | Grid_template -> 104
  | Grid -> 105
  | Grid_area -> 106
  | Grid_auto_flow -> 107
  | Grid_auto_columns -> 108
  | Grid_auto_rows -> 109
  | Grid_column -> 110
  | Grid_row -> 111
  | Grid_column_start -> 112
  | Grid_column_end -> 113
  | Grid_row_start -> 114
  | Grid_row_end -> 115
  | Border_width -> 116
  | Border_top_width -> 117
  | Border_right_width -> 118
  | Border_bottom_width -> 119
  | Border_left_width -> 120
  | Border_inline_start_width -> 121
  | Border_inline_end_width -> 122
  | Border_block_start_width -> 123
  | Border_block_end_width -> 124
  | Border_inline_width -> 125
  | Border_block_width -> 126
  | Border_image -> 127
  | Border_image_source -> 128
  | Border_image_slice -> 129
  | Border_image_repeat -> 130
  | Border_image_width -> 131
  | Border_image_outset -> 132
  | Border_radius -> 133
  | Border_top_left_radius -> 134
  | Border_top_right_radius -> 135
  | Border_bottom_left_radius -> 136
  | Border_bottom_right_radius -> 137
  | Border_top_color -> 138
  | Border_right_color -> 139
  | Border_bottom_color -> 140
  | Border_left_color -> 141
  | Border_inline_start_color -> 142
  | Border_inline_end_color -> 143
  | Border_block_start_color -> 144
  | Border_block_end_color -> 145
  | Border_inline_color -> 146
  | Border_block_color -> 147
  | Border_inline_style -> 148
  | Border_block_style -> 149
  | Border_start_start_radius -> 150
  | Border_start_end_radius -> 151
  | Border_end_start_radius -> 152
  | Border_end_end_radius -> 153
  | Opacity -> 154
  | Fill_opacity -> 155
  | Stroke_opacity -> 156
  | Stop_opacity -> 157
  | Flood_opacity -> 158
  | Mix_blend_mode -> 159
  | Transform -> 160
  | Translate -> 161
  | Cursor -> 162
  | Interactivity -> 163
  | Caret_animation -> 164
  | Caret_shape -> 165
  | Caret -> 166
  | Interest_delay -> 167
  | Interest_delay_start -> 168
  | Interest_delay_end -> 169
  | Nav_up -> 170
  | Nav_right -> 171
  | Nav_down -> 172
  | Nav_left -> 173
  | Table_layout -> 174
  | Border_collapse -> 175
  | Border_spacing -> 176
  | User_select -> 177
  | Pointer_events -> 178
  | Overflow -> 179
  | Inset -> 180
  | Inset_inline -> 181
  | Inset_inline_start -> 182
  | Inset_inline_end -> 183
  | Inset_block -> 184
  | Inset_block_start -> 185
  | Inset_block_end -> 186
  | Top -> 187
  | Right -> 188
  | Bottom -> 189
  | Left -> 190
  | Z_index -> 191
  | Outline -> 192
  | Outline_style -> 193
  | Outline_width -> 194
  | Outline_color -> 195
  | Outline_offset -> 196
  | Forced_color_adjust -> 197
  | Scroll_snap_type -> 198
  | White_space -> 199
  | White_space_collapse -> 541
  | Font_variant_alternates -> 548
  | Font_variant -> 549
  | Border -> 200
  | Border_block -> 201
  | Border_block_start -> 202
  | Border_block_end -> 203
  | Border_inline -> 204
  | Border_inline_start -> 205
  | Border_inline_end -> 206
  | Background -> 207
  | Tab_size -> 208
  | Zoom -> 209
  | Webkit_text_size_adjust -> 210
  | Font_feature_settings -> 211
  | Font_variation_settings -> 212
  | Webkit_tap_highlight_color -> 213
  | Webkit_user_select -> 214
  | Moz_user_select -> 215
  | Ms_user_select -> 216
  | Webkit_text_decoration -> 217
  | Webkit_text_decoration_color -> 218
  | Webkit_text_fill_color -> 219
  | Webkit_text_stroke_color -> 220
  | Webkit_text_stroke -> 542
  | Webkit_text_stroke_width -> 543
  | Text_indent -> 221
  | List_style -> 222
  | Font -> 223
  | Source -> 224
  | Webkit_appearance -> 225
  | Webkit_transform -> 226
  | Moz_transform -> 227
  | Ms_transform -> 228
  | O_transform -> 229
  | Webkit_transition -> 230
  | Webkit_transition_delay -> 231
  | Webkit_transition_duration -> 232
  | Webkit_transition_property -> 233
  | Webkit_transition_timing_function -> 234
  | Webkit_animation -> 235
  | Webkit_animation_delay -> 236
  | Webkit_animation_duration -> 237
  | Webkit_animation_direction -> 238
  | Webkit_animation_iteration_count -> 239
  | Webkit_animation_name -> 240
  | Webkit_animation_timing_function -> 241
  | Webkit_animation_fill_mode -> 242
  | Webkit_animation_play_state -> 243
  | Webkit_flex_direction -> 244
  | Webkit_flex_wrap -> 245
  | Webkit_flex_flow -> 246
  | Webkit_justify_content -> 247
  | Webkit_align_items -> 248
  | Webkit_align_content -> 249
  | Webkit_align_self -> 250
  | Webkit_border_radius -> 251
  | Webkit_box_sizing -> 252
  | Moz_box_sizing -> 253
  | Webkit_box_shadow -> 254
  | Webkit_background_size -> 255
  | Webkit_filter -> 256
  | Moz_appearance -> 257
  | Moz_animation -> 258
  | Moz_animation_delay -> 259
  | Moz_animation_duration -> 260
  | Moz_animation_direction -> 261
  | Moz_animation_iteration_count -> 262
  | Moz_animation_name -> 263
  | Moz_animation_timing_function -> 264
  | Moz_animation_fill_mode -> 265
  | Moz_animation_play_state -> 266
  | Moz_transition -> 267
  | Moz_transition_delay -> 268
  | Moz_transition_duration -> 269
  | Moz_transition_property -> 270
  | Moz_transition_timing_function -> 271
  | Moz_border_radius -> 272
  | Moz_box_shadow -> 273
  | Ms_filter -> 274
  | O_transition -> 275
  | Container_type -> 276
  | Container_name -> 277
  | Container -> 278
  | Anchor_name -> 279
  | Position_anchor -> 280
  | Position_try_fallbacks -> 281
  | Position_try_order -> 282
  | Position_try -> 283
  | Position_visibility -> 284
  | Position_area -> 285
  | Shape_outside -> 286
  | Shape_margin -> 287
  | Shape_image_threshold -> 288
  | Overflow_clip_margin -> 289
  | Overflow_anchor -> 290
  | Scrollbar_width -> 291
  | Scrollbar_color -> 292
  | Scrollbar_gutter -> 293
  | Line_height_step -> 294
  | Font_palette -> 295
  | Font_synthesis -> 296
  | Text_wrap_mode -> 297
  | Text_wrap_style -> 298
  | Text_box_trim -> 299
  | Text_underline_position -> 300
  | Text_box_edge -> 301
  | Text_box -> 302
  | Inline_sizing -> 303
  | Line_fit_edge -> 304
  | Interpolate_size -> 305
  | Min_intrinsic_sizing -> 306
  | Ruby_align -> 307
  | Ruby_merge -> 308
  | Ruby_overhang -> 309
  | Ruby_position -> 310
  | Glyph_orientation_vertical -> 311
  | Text_combine_upright -> 312
  | Animation_timeline -> 313
  | Animation_range -> 314
  | Animation_range_start -> 315
  | Animation_range_end -> 316
  | Scroll_timeline -> 317
  | Scroll_timeline_name -> 318
  | Scroll_timeline_axis -> 319
  | View_transition_name -> 320
  | View_transition_class -> 321
  | Image_orientation -> 322
  | Image_rendering -> 323
  | Image_resolution -> 324
  | Contain_intrinsic_size -> 325
  | Contain_intrinsic_width -> 326
  | Contain_intrinsic_height -> 327
  | Contain_intrinsic_block_size -> 328
  | Contain_intrinsic_inline_size -> 329
  | Margin_trim -> 330
  | Offset_path -> 331
  | Offset_distance -> 332
  | Offset_rotate -> 333
  | Font_size_adjust -> 334
  | Font_variant_emoji -> 335
  | Text_spacing_trim -> 336
  | Hyphenate_limit_chars -> 337
  | Initial_letter -> 338
  | Initial_letter_align -> 339
  | Initial_letter_wrap -> 340
  | Dominant_baseline -> 341
  | View_timeline_name -> 342
  | View_timeline_axis -> 343
  | View_timeline_inset -> 344
  | View_timeline -> 345
  | Timeline_scope -> 346
  | Perspective -> 347
  | Perspective_origin -> 348
  | Transform_style -> 349
  | Backface_visibility -> 350
  | Object_position -> 351
  | Rotate -> 352
  | Transition_duration -> 353
  | Transition_timing_function -> 354
  | Transition_delay -> 355
  | Transition_property -> 356
  | Transition_behavior -> 357
  | Overlay -> 358
  | Will_change -> 359
  | Contain -> 360
  | Isolation -> 361
  | Break_before -> 362
  | Break_after -> 363
  | Break_inside -> 364
  | Page_break_before -> 365
  | Page_break_after -> 366
  | Page_break_inside -> 367
  | Page_size -> 368
  | Columns -> 369
  | Column_width -> 370
  | Column_height -> 544
  | Column_wrap -> 545
  | Column_count -> 371
  | Column_rule -> 372
  | Column_rule_color -> 373
  | Column_rule_width -> 539
  | Column_rule_style -> 540
  | Column_span -> 374
  | Word_spacing -> 375
  | Background_attachment -> 376
  | Border_top -> 377
  | Border_right -> 378
  | Border_bottom -> 379
  | Border_left -> 380
  | Transform_origin -> 381
  | Transform_box -> 382
  | Text_shadow -> 383
  | Clip_path -> 384
  | Mask -> 385
  | Mask_border -> 386
  | Content_visibility -> 387
  | Filter -> 388
  | Background_image -> 389
  | Background_origin -> 390
  | Background_clip -> 391
  | Webkit_background_clip -> 392
  | Animation -> 393
  | Aspect_ratio -> 394
  | Overflow_x -> 395
  | Overflow_y -> 396
  | Overflow_block -> 397
  | Overflow_inline -> 398
  | Vertical_align -> 399
  | Font_family -> 400
  | Background_position -> 401
  | Background_position_x -> 537
  | Background_position_y -> 538
  | Webkit_mask_position_x -> 546
  | Webkit_mask_position_y -> 547
  | Background_repeat -> 402
  | Background_size -> 403
  | Webkit_font_smoothing -> 404
  | Moz_osx_font_smoothing -> 405
  | Webkit_line_clamp -> 406
  | Webkit_box_orient -> 407
  | Moz_orient -> 408
  | Text_overflow -> 409
  | Text_wrap -> 410
  | Word_break -> 411
  | Overflow_wrap -> 412
  | Line_break -> 413
  | Hyphens -> 414
  | Webkit_hyphens -> 415
  | Font_stretch -> 416
  | Font_optical_sizing -> 417
  | Font_kerning -> 418
  | Font_language_override -> 419
  | Font_synthesis_style -> 420
  | Font_synthesis_weight -> 421
  | Font_synthesis_small_caps -> 422
  | Font_synthesis_position -> 423
  | Font_variant_ligatures -> 424
  | Caps -> 425
  | Numeric -> 426
  | Font_variant_position -> 427
  | East_asian -> 428
  | Backdrop_filter -> 429
  | Webkit_backdrop_filter -> 430
  | Webkit_mask_image -> 431
  | Webkit_mask_composite -> 432
  | Webkit_mask_source_type -> 433
  | Webkit_mask_size -> 434
  | Webkit_mask_position -> 435
  | Webkit_mask_repeat -> 436
  | Webkit_mask_clip -> 437
  | Webkit_mask_origin -> 438
  | Mask_image -> 439
  | Mask_composite -> 440
  | Mask_mode -> 441
  | Mask_size -> 442
  | Mask_position -> 443
  | Mask_repeat -> 444
  | Mask_clip -> 445
  | Mask_origin -> 446
  | Mask_type -> 447
  | Scroll_snap_align -> 448
  | Scroll_snap_stop -> 449
  | Scroll_behavior -> 450
  | Box_sizing -> 451
  | Field_sizing -> 452
  | Caption_side -> 453
  | Resize -> 454
  | Object_fit -> 455
  | Object_view_box -> 456
  | Appearance -> 457
  | Color_scheme -> 458
  | Print_color_adjust -> 459
  | Webkit_print_color_adjust -> 460
  | Box_decoration_break -> 461
  | Webkit_box_decoration_break -> 462
  | Content -> 463
  | Counter_reset -> 464
  | Counter_increment -> 465
  | Quotes -> 466
  | Text_decoration_thickness -> 467
  | Text_size_adjust -> 468
  | Touch_action -> 469
  | Clip -> 470
  | Clear -> 471
  | Float -> 472
  | Scale -> 473
  | Transition -> 474
  | Box_shadow -> 475
  | Fill -> 476
  | Stroke -> 477
  | Stroke_width -> 478
  | Fill_rule -> 479
  | Clip_rule -> 480
  | Stroke_linecap -> 481
  | Stroke_linejoin -> 482
  | Stroke_miterlimit -> 483
  | Stroke_dashoffset -> 484
  | Stroke_dasharray -> 485
  | Paint_order -> 486
  | Vector_effect -> 487
  | Stop_color -> 488
  | Flood_color -> 489
  | Lighting_color -> 490
  | Direction -> 491
  | Unicode_bidi -> 492
  | Writing_mode -> 493
  | Text_decoration_skip_ink -> 494
  | Animation_name -> 495
  | Animation_duration -> 496
  | Animation_timing_function -> 497
  | Animation_delay -> 498
  | Animation_iteration_count -> 499
  | Animation_direction -> 500
  | Animation_fill_mode -> 501
  | Animation_play_state -> 502
  | Animation_composition -> 503
  | Background_blend_mode -> 504
  | Scroll_margin -> 505
  | Scroll_margin_top -> 506
  | Scroll_margin_right -> 507
  | Scroll_margin_bottom -> 508
  | Scroll_margin_left -> 509
  | Scroll_margin_inline -> 510
  | Scroll_margin_inline_start -> 511
  | Scroll_margin_inline_end -> 512
  | Scroll_margin_block -> 513
  | Scroll_margin_block_start -> 514
  | Scroll_margin_block_end -> 515
  | Scroll_padding -> 516
  | Scroll_padding_top -> 517
  | Scroll_padding_right -> 518
  | Scroll_padding_bottom -> 519
  | Scroll_padding_left -> 520
  | Scroll_padding_inline -> 521
  | Scroll_padding_inline_start -> 522
  | Scroll_padding_inline_end -> 523
  | Scroll_padding_block -> 524
  | Scroll_padding_block_start -> 525
  | Scroll_padding_block_end -> 526
  | Overscroll_behavior -> 527
  | Overscroll_behavior_x -> 528
  | Overscroll_behavior_y -> 529
  | Overscroll_behavior_block -> 530
  | Overscroll_behavior_inline -> 531
  | Accent_color -> 532
  | Caret_color -> 533
  | Offset_anchor -> 534
  | Offset_position -> 535
  | Offset -> 536
(* PROPERTY_TAG_END *)

(* Two property identities order by tag, and the two payload-carrying
   constructors by name within their own tag, so [0] means the same property
   exactly as [Declaration.equal_prop_key] does. *)
let compare_property : type a b. a property -> b property -> int =
 fun a b ->
  match (a, b) with
  | Custom_property x, Custom_property y -> String.compare x y
  | Unknown_property x, Unknown_property y -> String.compare x y
  | _ -> Int.compare (property_tag a) (property_tag b)

(* A property identity carries the type its value has, so two identities that
   agree prove their value types equal. [compare_property] answers the same
   question without the proof, and the two must agree: [Some Equal] exactly
   where the comparison is [0].

   [scripts/check_properties.ml] pins that the table below names every
   constructor. The trailing wildcard is what the type checker cannot see past:
   without the pin a property added later would silently read back as [None]. *)
(* PROPERTY_EQ_START - Used by scripts/check_properties.ml *)
let eq_property : type a b. a property -> b property -> (a, b) Type.eq option =
 fun a b ->
  match (a, b) with
  | Custom_property x, Custom_property y ->
      if String.equal x y then Some Equal else None
  | Unknown_property x, Unknown_property y ->
      if String.equal x y then Some Equal else None
  | All, All -> Some Equal
  | Background_color, Background_color -> Some Equal
  | Color, Color -> Some Equal
  | Border_color, Border_color -> Some Equal
  | Border_style, Border_style -> Some Equal
  | Border_top_style, Border_top_style -> Some Equal
  | Border_right_style, Border_right_style -> Some Equal
  | Border_bottom_style, Border_bottom_style -> Some Equal
  | Border_left_style, Border_left_style -> Some Equal
  | Border_inline_start_style, Border_inline_start_style -> Some Equal
  | Border_inline_end_style, Border_inline_end_style -> Some Equal
  | Border_block_start_style, Border_block_start_style -> Some Equal
  | Border_block_end_style, Border_block_end_style -> Some Equal
  | Padding, Padding -> Some Equal
  | Padding_left, Padding_left -> Some Equal
  | Padding_right, Padding_right -> Some Equal
  | Padding_bottom, Padding_bottom -> Some Equal
  | Padding_top, Padding_top -> Some Equal
  | Padding_inline, Padding_inline -> Some Equal
  | Padding_inline_start, Padding_inline_start -> Some Equal
  | Padding_inline_end, Padding_inline_end -> Some Equal
  | Padding_block, Padding_block -> Some Equal
  | Padding_block_start, Padding_block_start -> Some Equal
  | Padding_block_end, Padding_block_end -> Some Equal
  | Margin, Margin -> Some Equal
  | Margin_inline_end, Margin_inline_end -> Some Equal
  | Margin_inline_start, Margin_inline_start -> Some Equal
  | Margin_left, Margin_left -> Some Equal
  | Margin_right, Margin_right -> Some Equal
  | Margin_top, Margin_top -> Some Equal
  | Margin_bottom, Margin_bottom -> Some Equal
  | Margin_inline, Margin_inline -> Some Equal
  | Margin_block, Margin_block -> Some Equal
  | Margin_block_start, Margin_block_start -> Some Equal
  | Margin_block_end, Margin_block_end -> Some Equal
  | Gap, Gap -> Some Equal
  | Column_gap, Column_gap -> Some Equal
  | Row_gap, Row_gap -> Some Equal
  | Width, Width -> Some Equal
  | Height, Height -> Some Equal
  | Min_width, Min_width -> Some Equal
  | Min_height, Min_height -> Some Equal
  | Max_width, Max_width -> Some Equal
  | Max_height, Max_height -> Some Equal
  | Inline_size, Inline_size -> Some Equal
  | Min_inline_size, Min_inline_size -> Some Equal
  | Max_inline_size, Max_inline_size -> Some Equal
  | Block_size, Block_size -> Some Equal
  | Min_block_size, Min_block_size -> Some Equal
  | Max_block_size, Max_block_size -> Some Equal
  | Font_size, Font_size -> Some Equal
  | Line_height, Line_height -> Some Equal
  | Font_weight, Font_weight -> Some Equal
  | Font_style, Font_style -> Some Equal
  | Text_align, Text_align -> Some Equal
  | Text_decoration, Text_decoration -> Some Equal
  | Text_decoration_line, Text_decoration_line -> Some Equal
  | Text_decoration_style, Text_decoration_style -> Some Equal
  | Text_decoration_color, Text_decoration_color -> Some Equal
  | Text_underline_offset, Text_underline_offset -> Some Equal
  | Text_decoration_skip, Text_decoration_skip -> Some Equal
  | Text_decoration_skip_self, Text_decoration_skip_self -> Some Equal
  | Text_decoration_skip_box, Text_decoration_skip_box -> Some Equal
  | Text_decoration_skip_inset, Text_decoration_skip_inset -> Some Equal
  | Text_decoration_skip_spaces, Text_decoration_skip_spaces -> Some Equal
  | Text_emphasis, Text_emphasis -> Some Equal
  | Text_emphasis_style, Text_emphasis_style -> Some Equal
  | Text_emphasis_color, Text_emphasis_color -> Some Equal
  | Text_emphasis_position, Text_emphasis_position -> Some Equal
  | Text_emphasis_skip, Text_emphasis_skip -> Some Equal
  | Text_orientation, Text_orientation -> Some Equal
  | Text_transform, Text_transform -> Some Equal
  | Letter_spacing, Letter_spacing -> Some Equal
  | List_style_type, List_style_type -> Some Equal
  | List_style_position, List_style_position -> Some Equal
  | List_style_image, List_style_image -> Some Equal
  | Display, Display -> Some Equal
  | Position, Position -> Some Equal
  | Visibility, Visibility -> Some Equal
  | Baseline_source, Baseline_source -> Some Equal
  | Alignment_baseline, Alignment_baseline -> Some Equal
  | Baseline_shift, Baseline_shift -> Some Equal
  | Flex_direction, Flex_direction -> Some Equal
  | Flex_wrap, Flex_wrap -> Some Equal
  | Flex_flow, Flex_flow -> Some Equal
  | Flex, Flex -> Some Equal
  | Flex_grow, Flex_grow -> Some Equal
  | Flex_shrink, Flex_shrink -> Some Equal
  | Flex_basis, Flex_basis -> Some Equal
  | Order, Order -> Some Equal
  | Align_items, Align_items -> Some Equal
  | Justify_content, Justify_content -> Some Equal
  | Justify_items, Justify_items -> Some Equal
  | Justify_self, Justify_self -> Some Equal
  | Align_content, Align_content -> Some Equal
  | Align_self, Align_self -> Some Equal
  | Place_content, Place_content -> Some Equal
  | Place_items, Place_items -> Some Equal
  | Place_self, Place_self -> Some Equal
  | Grid_template_columns, Grid_template_columns -> Some Equal
  | Grid_template_rows, Grid_template_rows -> Some Equal
  | Grid_template_areas, Grid_template_areas -> Some Equal
  | Grid_template, Grid_template -> Some Equal
  | Grid, Grid -> Some Equal
  | Grid_area, Grid_area -> Some Equal
  | Grid_auto_flow, Grid_auto_flow -> Some Equal
  | Grid_auto_columns, Grid_auto_columns -> Some Equal
  | Grid_auto_rows, Grid_auto_rows -> Some Equal
  | Grid_column, Grid_column -> Some Equal
  | Grid_row, Grid_row -> Some Equal
  | Grid_column_start, Grid_column_start -> Some Equal
  | Grid_column_end, Grid_column_end -> Some Equal
  | Grid_row_start, Grid_row_start -> Some Equal
  | Grid_row_end, Grid_row_end -> Some Equal
  | Border_width, Border_width -> Some Equal
  | Border_top_width, Border_top_width -> Some Equal
  | Border_right_width, Border_right_width -> Some Equal
  | Border_bottom_width, Border_bottom_width -> Some Equal
  | Border_left_width, Border_left_width -> Some Equal
  | Border_inline_start_width, Border_inline_start_width -> Some Equal
  | Border_inline_end_width, Border_inline_end_width -> Some Equal
  | Border_block_start_width, Border_block_start_width -> Some Equal
  | Border_block_end_width, Border_block_end_width -> Some Equal
  | Border_inline_width, Border_inline_width -> Some Equal
  | Border_block_width, Border_block_width -> Some Equal
  | Border_image, Border_image -> Some Equal
  | Border_image_source, Border_image_source -> Some Equal
  | Border_image_slice, Border_image_slice -> Some Equal
  | Border_image_repeat, Border_image_repeat -> Some Equal
  | Border_image_width, Border_image_width -> Some Equal
  | Border_image_outset, Border_image_outset -> Some Equal
  | Border_radius, Border_radius -> Some Equal
  | Border_top_left_radius, Border_top_left_radius -> Some Equal
  | Border_top_right_radius, Border_top_right_radius -> Some Equal
  | Border_bottom_left_radius, Border_bottom_left_radius -> Some Equal
  | Border_bottom_right_radius, Border_bottom_right_radius -> Some Equal
  | Border_top_color, Border_top_color -> Some Equal
  | Border_right_color, Border_right_color -> Some Equal
  | Border_bottom_color, Border_bottom_color -> Some Equal
  | Border_left_color, Border_left_color -> Some Equal
  | Border_inline_start_color, Border_inline_start_color -> Some Equal
  | Border_inline_end_color, Border_inline_end_color -> Some Equal
  | Border_block_start_color, Border_block_start_color -> Some Equal
  | Border_block_end_color, Border_block_end_color -> Some Equal
  | Border_inline_color, Border_inline_color -> Some Equal
  | Border_block_color, Border_block_color -> Some Equal
  | Border_inline_style, Border_inline_style -> Some Equal
  | Border_block_style, Border_block_style -> Some Equal
  | Border_start_start_radius, Border_start_start_radius -> Some Equal
  | Border_start_end_radius, Border_start_end_radius -> Some Equal
  | Border_end_start_radius, Border_end_start_radius -> Some Equal
  | Border_end_end_radius, Border_end_end_radius -> Some Equal
  | Opacity, Opacity -> Some Equal
  | Fill_opacity, Fill_opacity -> Some Equal
  | Stroke_opacity, Stroke_opacity -> Some Equal
  | Stop_opacity, Stop_opacity -> Some Equal
  | Flood_opacity, Flood_opacity -> Some Equal
  | Mix_blend_mode, Mix_blend_mode -> Some Equal
  | Transform, Transform -> Some Equal
  | Translate, Translate -> Some Equal
  | Cursor, Cursor -> Some Equal
  | Interactivity, Interactivity -> Some Equal
  | Caret_animation, Caret_animation -> Some Equal
  | Caret_shape, Caret_shape -> Some Equal
  | Caret, Caret -> Some Equal
  | Interest_delay, Interest_delay -> Some Equal
  | Interest_delay_start, Interest_delay_start -> Some Equal
  | Interest_delay_end, Interest_delay_end -> Some Equal
  | Nav_up, Nav_up -> Some Equal
  | Nav_right, Nav_right -> Some Equal
  | Nav_down, Nav_down -> Some Equal
  | Nav_left, Nav_left -> Some Equal
  | Table_layout, Table_layout -> Some Equal
  | Border_collapse, Border_collapse -> Some Equal
  | Border_spacing, Border_spacing -> Some Equal
  | User_select, User_select -> Some Equal
  | Pointer_events, Pointer_events -> Some Equal
  | Overflow, Overflow -> Some Equal
  | Inset, Inset -> Some Equal
  | Inset_inline, Inset_inline -> Some Equal
  | Inset_inline_start, Inset_inline_start -> Some Equal
  | Inset_inline_end, Inset_inline_end -> Some Equal
  | Inset_block, Inset_block -> Some Equal
  | Inset_block_start, Inset_block_start -> Some Equal
  | Inset_block_end, Inset_block_end -> Some Equal
  | Top, Top -> Some Equal
  | Right, Right -> Some Equal
  | Bottom, Bottom -> Some Equal
  | Left, Left -> Some Equal
  | Z_index, Z_index -> Some Equal
  | Outline, Outline -> Some Equal
  | Outline_style, Outline_style -> Some Equal
  | Outline_width, Outline_width -> Some Equal
  | Outline_color, Outline_color -> Some Equal
  | Outline_offset, Outline_offset -> Some Equal
  | Forced_color_adjust, Forced_color_adjust -> Some Equal
  | Scroll_snap_type, Scroll_snap_type -> Some Equal
  | White_space, White_space -> Some Equal
  | White_space_collapse, White_space_collapse -> Some Equal
  | Font_variant_alternates, Font_variant_alternates -> Some Equal
  | Font_variant, Font_variant -> Some Equal
  | Border, Border -> Some Equal
  | Border_block, Border_block -> Some Equal
  | Border_block_start, Border_block_start -> Some Equal
  | Border_block_end, Border_block_end -> Some Equal
  | Border_inline, Border_inline -> Some Equal
  | Border_inline_start, Border_inline_start -> Some Equal
  | Border_inline_end, Border_inline_end -> Some Equal
  | Background, Background -> Some Equal
  | Tab_size, Tab_size -> Some Equal
  | Zoom, Zoom -> Some Equal
  | Webkit_text_size_adjust, Webkit_text_size_adjust -> Some Equal
  | Font_feature_settings, Font_feature_settings -> Some Equal
  | Font_variation_settings, Font_variation_settings -> Some Equal
  | Webkit_tap_highlight_color, Webkit_tap_highlight_color -> Some Equal
  | Webkit_user_select, Webkit_user_select -> Some Equal
  | Moz_user_select, Moz_user_select -> Some Equal
  | Ms_user_select, Ms_user_select -> Some Equal
  | Webkit_text_decoration, Webkit_text_decoration -> Some Equal
  | Webkit_text_decoration_color, Webkit_text_decoration_color -> Some Equal
  | Webkit_text_fill_color, Webkit_text_fill_color -> Some Equal
  | Webkit_text_stroke_color, Webkit_text_stroke_color -> Some Equal
  | Webkit_text_stroke, Webkit_text_stroke -> Some Equal
  | Webkit_text_stroke_width, Webkit_text_stroke_width -> Some Equal
  | Text_indent, Text_indent -> Some Equal
  | List_style, List_style -> Some Equal
  | Font, Font -> Some Equal
  | Source, Source -> Some Equal
  | Webkit_appearance, Webkit_appearance -> Some Equal
  | Webkit_transform, Webkit_transform -> Some Equal
  | Moz_transform, Moz_transform -> Some Equal
  | Ms_transform, Ms_transform -> Some Equal
  | O_transform, O_transform -> Some Equal
  | Webkit_transition, Webkit_transition -> Some Equal
  | Webkit_transition_delay, Webkit_transition_delay -> Some Equal
  | Webkit_transition_duration, Webkit_transition_duration -> Some Equal
  | Webkit_transition_property, Webkit_transition_property -> Some Equal
  | Webkit_transition_timing_function, Webkit_transition_timing_function ->
      Some Equal
  | Webkit_animation, Webkit_animation -> Some Equal
  | Webkit_animation_delay, Webkit_animation_delay -> Some Equal
  | Webkit_animation_duration, Webkit_animation_duration -> Some Equal
  | Webkit_animation_direction, Webkit_animation_direction -> Some Equal
  | Webkit_animation_iteration_count, Webkit_animation_iteration_count ->
      Some Equal
  | Webkit_animation_name, Webkit_animation_name -> Some Equal
  | Webkit_animation_timing_function, Webkit_animation_timing_function ->
      Some Equal
  | Webkit_animation_fill_mode, Webkit_animation_fill_mode -> Some Equal
  | Webkit_animation_play_state, Webkit_animation_play_state -> Some Equal
  | Webkit_flex_direction, Webkit_flex_direction -> Some Equal
  | Webkit_flex_wrap, Webkit_flex_wrap -> Some Equal
  | Webkit_flex_flow, Webkit_flex_flow -> Some Equal
  | Webkit_justify_content, Webkit_justify_content -> Some Equal
  | Webkit_align_items, Webkit_align_items -> Some Equal
  | Webkit_align_content, Webkit_align_content -> Some Equal
  | Webkit_align_self, Webkit_align_self -> Some Equal
  | Webkit_border_radius, Webkit_border_radius -> Some Equal
  | Webkit_box_sizing, Webkit_box_sizing -> Some Equal
  | Moz_box_sizing, Moz_box_sizing -> Some Equal
  | Webkit_box_shadow, Webkit_box_shadow -> Some Equal
  | Webkit_background_size, Webkit_background_size -> Some Equal
  | Webkit_filter, Webkit_filter -> Some Equal
  | Moz_appearance, Moz_appearance -> Some Equal
  | Moz_animation, Moz_animation -> Some Equal
  | Moz_animation_delay, Moz_animation_delay -> Some Equal
  | Moz_animation_duration, Moz_animation_duration -> Some Equal
  | Moz_animation_direction, Moz_animation_direction -> Some Equal
  | Moz_animation_iteration_count, Moz_animation_iteration_count -> Some Equal
  | Moz_animation_name, Moz_animation_name -> Some Equal
  | Moz_animation_timing_function, Moz_animation_timing_function -> Some Equal
  | Moz_animation_fill_mode, Moz_animation_fill_mode -> Some Equal
  | Moz_animation_play_state, Moz_animation_play_state -> Some Equal
  | Moz_transition, Moz_transition -> Some Equal
  | Moz_transition_delay, Moz_transition_delay -> Some Equal
  | Moz_transition_duration, Moz_transition_duration -> Some Equal
  | Moz_transition_property, Moz_transition_property -> Some Equal
  | Moz_transition_timing_function, Moz_transition_timing_function -> Some Equal
  | Moz_border_radius, Moz_border_radius -> Some Equal
  | Moz_box_shadow, Moz_box_shadow -> Some Equal
  | Ms_filter, Ms_filter -> Some Equal
  | O_transition, O_transition -> Some Equal
  | Container_type, Container_type -> Some Equal
  | Container_name, Container_name -> Some Equal
  | Container, Container -> Some Equal
  | Anchor_name, Anchor_name -> Some Equal
  | Position_anchor, Position_anchor -> Some Equal
  | Position_try_fallbacks, Position_try_fallbacks -> Some Equal
  | Position_try_order, Position_try_order -> Some Equal
  | Position_try, Position_try -> Some Equal
  | Position_visibility, Position_visibility -> Some Equal
  | Position_area, Position_area -> Some Equal
  | Shape_outside, Shape_outside -> Some Equal
  | Shape_margin, Shape_margin -> Some Equal
  | Shape_image_threshold, Shape_image_threshold -> Some Equal
  | Overflow_clip_margin, Overflow_clip_margin -> Some Equal
  | Overflow_anchor, Overflow_anchor -> Some Equal
  | Scrollbar_width, Scrollbar_width -> Some Equal
  | Scrollbar_color, Scrollbar_color -> Some Equal
  | Scrollbar_gutter, Scrollbar_gutter -> Some Equal
  | Line_height_step, Line_height_step -> Some Equal
  | Font_palette, Font_palette -> Some Equal
  | Font_synthesis, Font_synthesis -> Some Equal
  | Text_wrap_mode, Text_wrap_mode -> Some Equal
  | Text_wrap_style, Text_wrap_style -> Some Equal
  | Text_box_trim, Text_box_trim -> Some Equal
  | Text_underline_position, Text_underline_position -> Some Equal
  | Text_box_edge, Text_box_edge -> Some Equal
  | Text_box, Text_box -> Some Equal
  | Inline_sizing, Inline_sizing -> Some Equal
  | Line_fit_edge, Line_fit_edge -> Some Equal
  | Interpolate_size, Interpolate_size -> Some Equal
  | Min_intrinsic_sizing, Min_intrinsic_sizing -> Some Equal
  | Ruby_align, Ruby_align -> Some Equal
  | Ruby_merge, Ruby_merge -> Some Equal
  | Ruby_overhang, Ruby_overhang -> Some Equal
  | Ruby_position, Ruby_position -> Some Equal
  | Glyph_orientation_vertical, Glyph_orientation_vertical -> Some Equal
  | Text_combine_upright, Text_combine_upright -> Some Equal
  | Animation_timeline, Animation_timeline -> Some Equal
  | Animation_range, Animation_range -> Some Equal
  | Animation_range_start, Animation_range_start -> Some Equal
  | Animation_range_end, Animation_range_end -> Some Equal
  | Scroll_timeline, Scroll_timeline -> Some Equal
  | Scroll_timeline_name, Scroll_timeline_name -> Some Equal
  | Scroll_timeline_axis, Scroll_timeline_axis -> Some Equal
  | View_transition_name, View_transition_name -> Some Equal
  | View_transition_class, View_transition_class -> Some Equal
  | Image_orientation, Image_orientation -> Some Equal
  | Image_rendering, Image_rendering -> Some Equal
  | Image_resolution, Image_resolution -> Some Equal
  | Contain_intrinsic_size, Contain_intrinsic_size -> Some Equal
  | Contain_intrinsic_width, Contain_intrinsic_width -> Some Equal
  | Contain_intrinsic_height, Contain_intrinsic_height -> Some Equal
  | Contain_intrinsic_block_size, Contain_intrinsic_block_size -> Some Equal
  | Contain_intrinsic_inline_size, Contain_intrinsic_inline_size -> Some Equal
  | Margin_trim, Margin_trim -> Some Equal
  | Offset_path, Offset_path -> Some Equal
  | Offset, Offset -> Some Equal
  | Offset_anchor, Offset_anchor -> Some Equal
  | Offset_position, Offset_position -> Some Equal
  | Offset_distance, Offset_distance -> Some Equal
  | Offset_rotate, Offset_rotate -> Some Equal
  | Font_size_adjust, Font_size_adjust -> Some Equal
  | Font_variant_emoji, Font_variant_emoji -> Some Equal
  | Text_spacing_trim, Text_spacing_trim -> Some Equal
  | Hyphenate_limit_chars, Hyphenate_limit_chars -> Some Equal
  | Initial_letter, Initial_letter -> Some Equal
  | Initial_letter_align, Initial_letter_align -> Some Equal
  | Initial_letter_wrap, Initial_letter_wrap -> Some Equal
  | Dominant_baseline, Dominant_baseline -> Some Equal
  | View_timeline_name, View_timeline_name -> Some Equal
  | View_timeline_axis, View_timeline_axis -> Some Equal
  | View_timeline_inset, View_timeline_inset -> Some Equal
  | View_timeline, View_timeline -> Some Equal
  | Timeline_scope, Timeline_scope -> Some Equal
  | Perspective, Perspective -> Some Equal
  | Perspective_origin, Perspective_origin -> Some Equal
  | Transform_style, Transform_style -> Some Equal
  | Backface_visibility, Backface_visibility -> Some Equal
  | Object_position, Object_position -> Some Equal
  | Rotate, Rotate -> Some Equal
  | Transition_duration, Transition_duration -> Some Equal
  | Transition_timing_function, Transition_timing_function -> Some Equal
  | Transition_delay, Transition_delay -> Some Equal
  | Transition_property, Transition_property -> Some Equal
  | Transition_behavior, Transition_behavior -> Some Equal
  | Overlay, Overlay -> Some Equal
  | Will_change, Will_change -> Some Equal
  | Contain, Contain -> Some Equal
  | Isolation, Isolation -> Some Equal
  | Break_before, Break_before -> Some Equal
  | Break_after, Break_after -> Some Equal
  | Break_inside, Break_inside -> Some Equal
  | Page_break_before, Page_break_before -> Some Equal
  | Page_break_after, Page_break_after -> Some Equal
  | Page_break_inside, Page_break_inside -> Some Equal
  | Page_size, Page_size -> Some Equal
  | Columns, Columns -> Some Equal
  | Column_width, Column_width -> Some Equal
  | Column_height, Column_height -> Some Equal
  | Column_wrap, Column_wrap -> Some Equal
  | Column_count, Column_count -> Some Equal
  | Column_rule, Column_rule -> Some Equal
  | Column_rule_color, Column_rule_color -> Some Equal
  | Column_rule_width, Column_rule_width -> Some Equal
  | Column_rule_style, Column_rule_style -> Some Equal
  | Column_span, Column_span -> Some Equal
  | Word_spacing, Word_spacing -> Some Equal
  | Background_attachment, Background_attachment -> Some Equal
  | Border_top, Border_top -> Some Equal
  | Border_right, Border_right -> Some Equal
  | Border_bottom, Border_bottom -> Some Equal
  | Border_left, Border_left -> Some Equal
  | Transform_origin, Transform_origin -> Some Equal
  | Transform_box, Transform_box -> Some Equal
  | Text_shadow, Text_shadow -> Some Equal
  | Clip_path, Clip_path -> Some Equal
  | Mask, Mask -> Some Equal
  | Mask_border, Mask_border -> Some Equal
  | Content_visibility, Content_visibility -> Some Equal
  | Filter, Filter -> Some Equal
  | Background_image, Background_image -> Some Equal
  | Background_origin, Background_origin -> Some Equal
  | Background_clip, Background_clip -> Some Equal
  | Webkit_background_clip, Webkit_background_clip -> Some Equal
  | Animation, Animation -> Some Equal
  | Aspect_ratio, Aspect_ratio -> Some Equal
  | Overflow_x, Overflow_x -> Some Equal
  | Overflow_y, Overflow_y -> Some Equal
  | Overflow_block, Overflow_block -> Some Equal
  | Overflow_inline, Overflow_inline -> Some Equal
  | Vertical_align, Vertical_align -> Some Equal
  | Font_family, Font_family -> Some Equal
  | Background_position, Background_position -> Some Equal
  | Background_position_x, Background_position_x -> Some Equal
  | Background_position_y, Background_position_y -> Some Equal
  | Webkit_mask_position_x, Webkit_mask_position_x -> Some Equal
  | Webkit_mask_position_y, Webkit_mask_position_y -> Some Equal
  | Background_repeat, Background_repeat -> Some Equal
  | Background_size, Background_size -> Some Equal
  | Webkit_font_smoothing, Webkit_font_smoothing -> Some Equal
  | Moz_osx_font_smoothing, Moz_osx_font_smoothing -> Some Equal
  | Webkit_line_clamp, Webkit_line_clamp -> Some Equal
  | Webkit_box_orient, Webkit_box_orient -> Some Equal
  | Moz_orient, Moz_orient -> Some Equal
  | Text_overflow, Text_overflow -> Some Equal
  | Text_wrap, Text_wrap -> Some Equal
  | Word_break, Word_break -> Some Equal
  | Overflow_wrap, Overflow_wrap -> Some Equal
  | Line_break, Line_break -> Some Equal
  | Hyphens, Hyphens -> Some Equal
  | Webkit_hyphens, Webkit_hyphens -> Some Equal
  | Font_stretch, Font_stretch -> Some Equal
  | Font_optical_sizing, Font_optical_sizing -> Some Equal
  | Font_kerning, Font_kerning -> Some Equal
  | Font_language_override, Font_language_override -> Some Equal
  | Font_synthesis_style, Font_synthesis_style -> Some Equal
  | Font_synthesis_weight, Font_synthesis_weight -> Some Equal
  | Font_synthesis_small_caps, Font_synthesis_small_caps -> Some Equal
  | Font_synthesis_position, Font_synthesis_position -> Some Equal
  | Font_variant_ligatures, Font_variant_ligatures -> Some Equal
  | Caps, Caps -> Some Equal
  | Numeric, Numeric -> Some Equal
  | Font_variant_position, Font_variant_position -> Some Equal
  | East_asian, East_asian -> Some Equal
  | Backdrop_filter, Backdrop_filter -> Some Equal
  | Webkit_backdrop_filter, Webkit_backdrop_filter -> Some Equal
  | Webkit_mask_image, Webkit_mask_image -> Some Equal
  | Webkit_mask_composite, Webkit_mask_composite -> Some Equal
  | Webkit_mask_source_type, Webkit_mask_source_type -> Some Equal
  | Webkit_mask_size, Webkit_mask_size -> Some Equal
  | Webkit_mask_position, Webkit_mask_position -> Some Equal
  | Webkit_mask_repeat, Webkit_mask_repeat -> Some Equal
  | Webkit_mask_clip, Webkit_mask_clip -> Some Equal
  | Webkit_mask_origin, Webkit_mask_origin -> Some Equal
  | Mask_image, Mask_image -> Some Equal
  | Mask_composite, Mask_composite -> Some Equal
  | Mask_mode, Mask_mode -> Some Equal
  | Mask_size, Mask_size -> Some Equal
  | Mask_position, Mask_position -> Some Equal
  | Mask_repeat, Mask_repeat -> Some Equal
  | Mask_clip, Mask_clip -> Some Equal
  | Mask_origin, Mask_origin -> Some Equal
  | Mask_type, Mask_type -> Some Equal
  | Scroll_snap_align, Scroll_snap_align -> Some Equal
  | Scroll_snap_stop, Scroll_snap_stop -> Some Equal
  | Scroll_behavior, Scroll_behavior -> Some Equal
  | Box_sizing, Box_sizing -> Some Equal
  | Field_sizing, Field_sizing -> Some Equal
  | Caption_side, Caption_side -> Some Equal
  | Resize, Resize -> Some Equal
  | Object_fit, Object_fit -> Some Equal
  | Object_view_box, Object_view_box -> Some Equal
  | Appearance, Appearance -> Some Equal
  | Color_scheme, Color_scheme -> Some Equal
  | Print_color_adjust, Print_color_adjust -> Some Equal
  | Webkit_print_color_adjust, Webkit_print_color_adjust -> Some Equal
  | Box_decoration_break, Box_decoration_break -> Some Equal
  | Webkit_box_decoration_break, Webkit_box_decoration_break -> Some Equal
  | Content, Content -> Some Equal
  | Counter_reset, Counter_reset -> Some Equal
  | Counter_increment, Counter_increment -> Some Equal
  | Quotes, Quotes -> Some Equal
  | Text_decoration_thickness, Text_decoration_thickness -> Some Equal
  | Text_size_adjust, Text_size_adjust -> Some Equal
  | Touch_action, Touch_action -> Some Equal
  | Clip, Clip -> Some Equal
  | Clear, Clear -> Some Equal
  | Float, Float -> Some Equal
  | Scale, Scale -> Some Equal
  | Transition, Transition -> Some Equal
  | Box_shadow, Box_shadow -> Some Equal
  | Fill, Fill -> Some Equal
  | Stroke, Stroke -> Some Equal
  | Stroke_width, Stroke_width -> Some Equal
  | Fill_rule, Fill_rule -> Some Equal
  | Clip_rule, Clip_rule -> Some Equal
  | Stroke_linecap, Stroke_linecap -> Some Equal
  | Stroke_linejoin, Stroke_linejoin -> Some Equal
  | Stroke_miterlimit, Stroke_miterlimit -> Some Equal
  | Stroke_dashoffset, Stroke_dashoffset -> Some Equal
  | Stroke_dasharray, Stroke_dasharray -> Some Equal
  | Paint_order, Paint_order -> Some Equal
  | Vector_effect, Vector_effect -> Some Equal
  | Stop_color, Stop_color -> Some Equal
  | Flood_color, Flood_color -> Some Equal
  | Lighting_color, Lighting_color -> Some Equal
  | Direction, Direction -> Some Equal
  | Unicode_bidi, Unicode_bidi -> Some Equal
  | Writing_mode, Writing_mode -> Some Equal
  | Text_decoration_skip_ink, Text_decoration_skip_ink -> Some Equal
  | Animation_name, Animation_name -> Some Equal
  | Animation_duration, Animation_duration -> Some Equal
  | Animation_timing_function, Animation_timing_function -> Some Equal
  | Animation_delay, Animation_delay -> Some Equal
  | Animation_iteration_count, Animation_iteration_count -> Some Equal
  | Animation_direction, Animation_direction -> Some Equal
  | Animation_fill_mode, Animation_fill_mode -> Some Equal
  | Animation_play_state, Animation_play_state -> Some Equal
  | Animation_composition, Animation_composition -> Some Equal
  | Background_blend_mode, Background_blend_mode -> Some Equal
  | Scroll_margin, Scroll_margin -> Some Equal
  | Scroll_margin_top, Scroll_margin_top -> Some Equal
  | Scroll_margin_right, Scroll_margin_right -> Some Equal
  | Scroll_margin_bottom, Scroll_margin_bottom -> Some Equal
  | Scroll_margin_left, Scroll_margin_left -> Some Equal
  | Scroll_margin_inline, Scroll_margin_inline -> Some Equal
  | Scroll_margin_inline_start, Scroll_margin_inline_start -> Some Equal
  | Scroll_margin_inline_end, Scroll_margin_inline_end -> Some Equal
  | Scroll_margin_block, Scroll_margin_block -> Some Equal
  | Scroll_margin_block_start, Scroll_margin_block_start -> Some Equal
  | Scroll_margin_block_end, Scroll_margin_block_end -> Some Equal
  | Scroll_padding, Scroll_padding -> Some Equal
  | Scroll_padding_top, Scroll_padding_top -> Some Equal
  | Scroll_padding_right, Scroll_padding_right -> Some Equal
  | Scroll_padding_bottom, Scroll_padding_bottom -> Some Equal
  | Scroll_padding_left, Scroll_padding_left -> Some Equal
  | Scroll_padding_inline, Scroll_padding_inline -> Some Equal
  | Scroll_padding_inline_start, Scroll_padding_inline_start -> Some Equal
  | Scroll_padding_inline_end, Scroll_padding_inline_end -> Some Equal
  | Scroll_padding_block, Scroll_padding_block -> Some Equal
  | Scroll_padding_block_start, Scroll_padding_block_start -> Some Equal
  | Scroll_padding_block_end, Scroll_padding_block_end -> Some Equal
  | Overscroll_behavior, Overscroll_behavior -> Some Equal
  | Overscroll_behavior_x, Overscroll_behavior_x -> Some Equal
  | Overscroll_behavior_y, Overscroll_behavior_y -> Some Equal
  | Overscroll_behavior_block, Overscroll_behavior_block -> Some Equal
  | Overscroll_behavior_inline, Overscroll_behavior_inline -> Some Equal
  | Accent_color, Accent_color -> Some Equal
  | Caret_color, Caret_color -> Some Equal
  | _ -> None
(* PROPERTY_EQ_END *)

let rec pp_content : content Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Quoted { value; quote; repr } -> (
      if Pp.minified ctx then Pp.quoted_string ctx value
      else
        match repr with
        | Some repr -> Pp.string ctx repr
        | None ->
            Pp.char ctx quote;
            Pp.string ctx value;
            Pp.char ctx quote)
  | Image image -> pp_background_image ctx image
  | Open_quote -> Pp.string ctx "open-quote"
  | Close_quote -> Pp.string ctx "close-quote"
  | Attr attr -> Pp.call "attr" (Values.pp_attr_call pp_content) ctx attr
  | Counter name -> Pp.call "counter" pp_ident ctx name
  | String_ref name -> Pp.call "string" pp_ident ctx name
  | Counters (name, separator) ->
      Pp.string ctx "counters(";
      pp_ident ctx name;
      Pp.char ctx ',';
      Pp.space ctx ();
      Pp.quoted_string ctx separator;
      Pp.char ctx ')'
  | Content_list items -> Pp.list ~sep:Pp.space pp_content ctx items
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_content ctx v

let pp_counter_item ctx { name; value } =
  pp_ident ctx name;
  match value with
  | None -> ()
  | Some n ->
      Pp.space ctx ();
      Pp.int ctx n

let rec pp_counter_set : counter_set Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Counters items -> Pp.list ~sep:Pp.space pp_counter_item ctx items
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_counter_set ctx v

let rec pp_quotes : quotes Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pairs pairs ->
      List.iter
        (fun (open_q, close_q) ->
          Pp.char ctx '"';
          Pp.string ctx open_q;
          Pp.char ctx '"';
          Pp.char ctx '"';
          Pp.string ctx close_q;
          Pp.char ctx '"')
        pairs
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_quotes ctx v

(* Helpers for timing-function pretty printing *)

(* CSS Grid template - flattened type with direct constructors *)

let read_symbols_type t : symbols_type =
  Cursor.enum "symbols type"
    [
      ("cyclic", (Cyclic : symbols_type));
      ("numeric", Numeric);
      ("alphabetic", Alphabetic);
      ("symbolic", Symbolic);
      ("fixed", Fixed);
    ]
    t

let read_list_style_symbol t : list_style_symbol =
  Cursor.one_of
    [
      (fun t -> (Url (Cursor.url t) : list_style_symbol));
      (fun t -> String (Cursor.string t));
    ]
    t

let list_style_type_keywords : (string * list_style_type) list =
  [
    ("none", (None : list_style_type));
    ("disc", Disc);
    ("circle", Circle);
    ("square", Square);
    ("decimal", Decimal);
    ("lower-alpha", Lower_alpha);
    ("upper-alpha", Upper_alpha);
    ("lower-roman", Lower_roman);
    ("upper-roman", Upper_roman);
    ("decimal-leading-zero", (Decimal_leading_zero : list_style_type));
    ("arabic-indic", (Arabic_indic : list_style_type));
    ("armenian", (Armenian : list_style_type));
    ("upper-armenian", (Upper_armenian : list_style_type));
    ("lower-armenian", (Lower_armenian : list_style_type));
    ("bengali", (Bengali : list_style_type));
    ("cambodian", (Cambodian : list_style_type));
    ("khmer", (Khmer : list_style_type));
    ("cjk-decimal", (Cjk_decimal : list_style_type));
    ("devanagari", (Devanagari : list_style_type));
    ("georgian", (Georgian : list_style_type));
    ("gujarati", (Gujarati : list_style_type));
    ("gurmukhi", (Gurmukhi : list_style_type));
    ("hebrew", (Hebrew : list_style_type));
    ("kannada", (Kannada : list_style_type));
    ("lao", (Lao : list_style_type));
    ("malayalam", (Malayalam : list_style_type));
    ("mongolian", (Mongolian : list_style_type));
    ("myanmar", (Myanmar : list_style_type));
    ("oriya", (Oriya : list_style_type));
    ("persian", (Persian : list_style_type));
    ("tamil", (Tamil : list_style_type));
    ("telugu", (Telugu : list_style_type));
    ("thai", (Thai : list_style_type));
    ("tibetan", (Tibetan : list_style_type));
    ("lower-latin", (Lower_latin : list_style_type));
    ("upper-latin", (Upper_latin : list_style_type));
    ("cjk-earthly-branch", (Cjk_earthly_branch : list_style_type));
    ("cjk-heavenly-stem", (Cjk_heavenly_stem : list_style_type));
    ("lower-greek", (Lower_greek : list_style_type));
    ("hiragana", (Hiragana : list_style_type));
    ("hiragana-iroha", (Hiragana_iroha : list_style_type));
    ("katakana", (Katakana : list_style_type));
    ("katakana-iroha", (Katakana_iroha : list_style_type));
    ("disclosure-open", (Disclosure_open : list_style_type));
    ("disclosure-closed", (Disclosure_closed : list_style_type));
    ("cjk-ideographic", (Cjk_ideographic : list_style_type));
    ("japanese-informal", (Japanese_informal : list_style_type));
    ("japanese-formal", (Japanese_formal : list_style_type));
    ("korean-hangul-formal", (Korean_hangul_formal : list_style_type));
    ("korean-hanja-informal", (Korean_hanja_informal : list_style_type));
    ("korean-hanja-formal", (Korean_hanja_formal : list_style_type));
    ("simp-chinese-informal", (Simp_chinese_informal : list_style_type));
    ("simp-chinese-formal", (Simp_chinese_formal : list_style_type));
    ("trad-chinese-informal", (Trad_chinese_informal : list_style_type));
    ("trad-chinese-formal", (Trad_chinese_formal : list_style_type));
    ("ethiopic-numeric", (Ethiopic_numeric : list_style_type));
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let rec read_list_style_type t : list_style_type =
  let read_var t : list_style_type = Var (read_var read_list_style_type t) in
  let read_symbols_body t : list_style_type =
    let kind = Cursor.option read_symbols_type t in
    Cursor.ws t;
    let symbols =
      Cursor.list ~sep:Cursor.ws ~at_least:1 read_list_style_symbol t
    in
    Symbols (kind, symbols)
  in
  Cursor.enum_or_var "list-style-type" list_style_type_keywords ~var:read_var
    ~default:
      (Cursor.one_of
         [
           (fun t -> Cursor.call "symbols" t read_symbols_body);
           (fun t -> (String (Cursor.string t) : list_style_type));
           (fun t ->
             let name = Cursor.ident t in
             if String.lowercase_ascii name = "default" then
               Cursor.err_invalid t "reserved counter-style name";
             (Name name : list_style_type));
         ])
    t

let rec read_list_style_position t : list_style_position =
  Cursor.enum_or_var "list-style-position"
    [
      ("inside", (Inside : list_style_position));
      ("outside", Outside);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_list_style_position t))
    t

(* CSS Lists 3 sec. 3.5 gives [list-style-image] an [<image>], so every image
   the [background-image] vocabulary reads goes here: the comma list is what
   only that property takes. *)
let rec read_list_style_image t : list_style_image =
  let read_var t : list_style_image = Var (read_var read_list_style_image t) in
  Cursor.one_of
    [
      (fun t ->
        Cursor.enum_or_calls "list-style-image"
          [
            ("none", (None : list_style_image));
            ("inherit", Inherit);
            ("initial", Initial);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:[ ("var", read_var) ]
          t);
      (fun t -> (Image (read_bg_image t) : list_style_image));
    ]
    t

(* Parse the [list-style] shorthand into a typed [list_style_shorthand] record.
   Each slot is recognised by the longhand reader; a single bare [none]
   populates both [type_] and [image] per CSS Lists 3 sec. 3.6. *)
let try_list_style_slot r read_fn (slot : 'a option ref) =
  if !slot <> Option.None then false
  else
    let pos = Cursor.save r in
    match read_fn r with
    | v ->
        slot := Some v;
        true
    | exception Cursor.Parse_error _ ->
        Cursor.restore r pos;
        false

let read_list_style_shorthand r : list_style_shorthand =
  let type_ : list_style_type option ref = ref Option.None in
  let position : list_style_position option ref = ref Option.None in
  let image : list_style_image option ref = ref Option.None in
  let saw_none = ref false in
  let try_one () =
    try_list_style_slot r read_list_style_position position
    || try_list_style_slot r read_list_style_image image
    || try_list_style_slot r read_list_style_type type_
  in
  let rec consume () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else
      let saved = Cursor.save r in
      let kw = Cursor.peek_ident r in
      if kw = Some "none" then begin
        let _ = Cursor.ident r in
        saw_none := true;
        consume ()
      end
      else if try_one () then consume ()
      else Cursor.restore r saved
  in
  consume ();
  Cursor.ws r;
  if not (Cursor.is_done r) then
    Cursor.err_invalid r "invalid list-style shorthand";
  if !saw_none then begin
    if !type_ = Option.None then type_ := Some (None : list_style_type);
    if !image = Option.None then image := Some (None : list_style_image)
  end;
  if
    !type_ = Option.None && !position = Option.None && !image = Option.None
    && not !saw_none
  then Cursor.err_invalid r "invalid list-style shorthand";
  { type_ = !type_; position = !position; image = !image }

let rec read_list_style t : list_style =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Inherit
  | "initial" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Initial
  | "unset" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Unset
  | "revert" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Revert
  | "revert-layer" ->
      ignore (Cursor.consume_to_decl_end ~trim:true t);
      Revert_layer
  | _ ->
      let is_valid_var () =
        let r = Cursor.of_string raw in
        match
          Values.read_var (fun r -> Cursor.consume_to_decl_end ~trim:true r) r
        with
        | (_ : string var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_valid_var () then (
        let r = Cursor.of_string raw in
        let var = Values.read_var (fun r -> read_list_style r) r in
        ignore (Cursor.consume_to_decl_end ~trim:true t);
        Var var)
      else
        let body =
          try read_list_style_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid t "invalid list-style shorthand"
        in
        ignore (Cursor.consume_to_decl_end ~trim:true t);
        Shorthand body

let read_content_string t =
  match Cursor.string_repr_with_quote_opt t with
  | Some (value, quote, repr) -> Quoted { value; quote; repr }
  | None -> Cursor.err_expected t "string"

let read_content_counter t =
  Cursor.call "counter" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Counter name)

let read_content_string_ref t =
  Cursor.call "string" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      String_ref name)

let read_content_counters t =
  Cursor.call "counters" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident inner in
      Cursor.ws inner;
      Cursor.comma inner;
      Cursor.ws inner;
      let separator = Cursor.string inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Counters (name, separator) : content))

let rec read_content_attr t =
  Cursor.call "attr" t (fun inner ->
      Cursor.ws inner;
      let name = Cursor.ident ~keep_case:true inner in
      let type_ : Values.attr_type option =
        Cursor.ws inner;
        if Cursor.is_done inner || Cursor.peek_comma inner then Option.None
        else Option.Some (Values.read_attr_type inner)
      in
      Cursor.ws inner;
      let fallback : content Values.attr_fallback =
        if Cursor.comma_opt inner then (
          Cursor.ws inner;
          if Cursor.is_done inner then Empty_fallback
          else Attr_fallback (read_content inner))
        else No_fallback
      in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Attr { name; type_; fallback })

and read_content_single t =
  let read_var t : content = Var (read_var read_content t) in
  Cursor.enum_or_calls "content"
    [
      ("none", (None : content));
      ("normal", Normal);
      ("open-quote", Open_quote);
      ("close-quote", Close_quote);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", read_var);
        ("attr", read_content_attr);
        ("counter", read_content_counter);
        ("string", read_content_string_ref);
        ("counters", read_content_counters);
      ]
    ~default:
      (Cursor.one_of
         [ read_content_string; (fun t -> (Image (read_bg_image t) : content)) ])
    t

and read_content t : content =
  let items = Cursor.list ~sep:Cursor.ws ~at_least:1 read_content_single t in
  match items with
  | [ item ] -> item
  | _ ->
      if
        List.exists
          (fun (item : content) ->
            match item with None | Normal -> true | _ -> false)
          items
      then Cursor.err_invalid t "none/normal cannot be combined in content";
      Content_list items

let counter_name_reserved =
  [ "none"; "inherit"; "initial"; "unset"; "revert"; "revert-layer" ]

let read_counter_name t =
  let name = Cursor.ident t in
  if List.mem name counter_name_reserved then
    Cursor.err_invalid t ("reserved counter name: " ^ name);
  name

let read_counter_item t =
  let name = read_counter_name t in
  Cursor.ws t;
  let value = Cursor.integer_opt t in
  { name; value }

let rec read_counter_set t : counter_set =
  Cursor.enum_or_var "counter"
    [
      ("none", (None : counter_set));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_counter_set t))
    ~default:(fun t ->
      let items = Cursor.list ~sep:Cursor.ws ~at_least:1 read_counter_item t in
      Counters items)
    t

let rec read_quotes t : quotes =
  let read_var' t : quotes = Var (read_var read_quotes t) in
  (* Read pairs of strings for quotes property *)
  let read_pairs t =
    let rec read_quotes_pairs acc =
      Cursor.ws t;
      match Cursor.string_opt t with
      | Some open_q ->
          Cursor.ws t;
          let close_q = Cursor.string t in
          read_quotes_pairs ((open_q, close_q) :: acc)
      | None -> List.rev acc
    in
    Pairs (read_quotes_pairs [])
  in
  Cursor.enum_or_calls "quotes"
    [
      ("auto", (Auto : quotes));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert-layer", Revert_layer);
      ("revert", Revert);
    ]
    ~calls:[ ("var", read_var') ]
    ~default:read_pairs t

let read_any_property t =
  (* CSS property names are case-insensitive per CSS Syntax 3 (ED) sec. 8.1. *)
  let prop_name = String.lowercase_ascii_preserve (Cursor.ident t) in
  (* PROPERTY_MATCHING_START - Used by scripts/check_properties.ml *)
  match prop_name with
  | "all" -> Prop All
  | "width" -> Prop Width
  | "height" -> Prop Height
  | "min-width" -> Prop Min_width
  | "min-height" -> Prop Min_height
  | "max-width" -> Prop Max_width
  | "max-height" -> Prop Max_height
  | "inline-size" -> Prop Inline_size
  | "min-inline-size" -> Prop Min_inline_size
  | "max-inline-size" -> Prop Max_inline_size
  | "block-size" -> Prop Block_size
  | "min-block-size" -> Prop Min_block_size
  | "max-block-size" -> Prop Max_block_size
  | "color" -> Prop Color
  | "background-color" -> Prop Background_color
  | "background" -> Prop Background (* Shorthand property *)
  | "background-image" -> Prop Background_image
  | "border-color" -> Prop Border_color
  | "border-top-color" -> Prop Border_top_color
  | "border-right-color" -> Prop Border_right_color
  | "border-bottom-color" -> Prop Border_bottom_color
  | "border-left-color" -> Prop Border_left_color
  | "border-inline-color" -> Prop Border_inline_color
  | "border-block-color" -> Prop Border_block_color
  | "border-inline-width" -> Prop Border_inline_width
  | "border-block-width" -> Prop Border_block_width
  | "border-style" -> Prop Border_style
  | "border-top-style" -> Prop Border_top_style
  | "border-right-style" -> Prop Border_right_style
  | "border-bottom-style" -> Prop Border_bottom_style
  | "border-left-style" -> Prop Border_left_style
  | "border-inline-start-style" -> Prop Border_inline_start_style
  | "border-inline-end-style" -> Prop Border_inline_end_style
  | "border-block-start-style" -> Prop Border_block_start_style
  | "border-block-end-style" -> Prop Border_block_end_style
  | "border-width" -> Prop Border_width
  | "border-top-width" -> Prop Border_top_width
  | "border-right-width" -> Prop Border_right_width
  | "border-bottom-width" -> Prop Border_bottom_width
  | "border-left-width" -> Prop Border_left_width
  | "border-image" -> Prop Border_image
  | "border-radius" -> Prop Border_radius
  | "border-top-left-radius" -> Prop Border_top_left_radius
  | "border-top-right-radius" -> Prop Border_top_right_radius
  | "border-bottom-left-radius" -> Prop Border_bottom_left_radius
  | "border-bottom-right-radius" -> Prop Border_bottom_right_radius
  | "outline-color" -> Prop Outline_color
  | "text-decoration-color" -> Prop Text_decoration_color
  | "display" -> Prop Display
  | "position" -> Prop Position
  | "visibility" -> Prop Visibility
  | "baseline-source" -> Prop Baseline_source
  | "alignment-baseline" -> Prop Alignment_baseline
  | "baseline-shift" -> Prop Baseline_shift
  | "overflow" -> Prop Overflow
  | "overflow-x" -> Prop Overflow_x
  | "overflow-y" -> Prop Overflow_y
  | "overflow-block" -> Prop Overflow_block
  | "overflow-inline" -> Prop Overflow_inline
  | "margin" -> Prop Margin
  | "margin-left" -> Prop Margin_left
  | "margin-right" -> Prop Margin_right
  | "margin-top" -> Prop Margin_top
  | "margin-bottom" -> Prop Margin_bottom
  | "margin-inline" -> Prop Margin_inline
  | "margin-inline-start" -> Prop Margin_inline_start
  | "margin-inline-end" -> Prop Margin_inline_end
  | "margin-block" -> Prop Margin_block
  | "margin-block-start" -> Prop Margin_block_start
  | "margin-block-end" -> Prop Margin_block_end
  | "padding" -> Prop Padding
  | "padding-left" -> Prop Padding_left
  | "padding-right" -> Prop Padding_right
  | "padding-top" -> Prop Padding_top
  | "padding-bottom" -> Prop Padding_bottom
  | "padding-inline" -> Prop Padding_inline
  | "padding-inline-start" -> Prop Padding_inline_start
  | "padding-inline-end" -> Prop Padding_inline_end
  | "padding-block" -> Prop Padding_block
  | "padding-block-start" -> Prop Padding_block_start
  | "padding-block-end" -> Prop Padding_block_end
  | "font-size" -> Prop Font_size
  | "font-weight" -> Prop Font_weight
  | "font-style" -> Prop Font_style
  | "font-family" -> Prop Font_family
  | "font-feature-settings" -> Prop Font_feature_settings
  | "font-variation-settings" -> Prop Font_variation_settings
  | "src" -> Prop Source
  | "text-align" -> Prop Text_align
  | "text-decoration" -> Prop Text_decoration
  | "text-decoration-line" -> Prop Text_decoration_line
  | "text-decoration-skip" -> Prop Text_decoration_skip
  | "text-decoration-skip-self" -> Prop Text_decoration_skip_self
  | "text-decoration-skip-box" -> Prop Text_decoration_skip_box
  | "text-decoration-skip-inset" -> Prop Text_decoration_skip_inset
  | "text-decoration-skip-spaces" -> Prop Text_decoration_skip_spaces
  | "text-emphasis" -> Prop Text_emphasis
  | "text-emphasis-style" -> Prop Text_emphasis_style
  | "text-emphasis-color" -> Prop Text_emphasis_color
  | "text-emphasis-position" -> Prop Text_emphasis_position
  | "text-emphasis-skip" -> Prop Text_emphasis_skip
  | "text-orientation" -> Prop Text_orientation
  | "text-transform" -> Prop Text_transform
  | "text-indent" -> Prop Text_indent
  | "letter-spacing" -> Prop Letter_spacing
  | "flex" -> Prop Flex
  | "flex-direction" -> Prop Flex_direction
  | "flex-wrap" -> Prop Flex_wrap
  | "flex-flow" -> Prop Flex_flow
  | "align-items" -> Prop Align_items
  | "justify-content" -> Prop Justify_content
  | "opacity" -> Prop Opacity
  (* SVG 2 sec. 13.4.3, 13.5.2 and 14.2.4.2 / Filter Effects 1 sec. 9.13.2: each
     is an <alpha-value>, the same number-or-percentage as [opacity]. *)
  | "fill-opacity" -> Prop Fill_opacity
  | "stroke-opacity" -> Prop Stroke_opacity
  | "stop-opacity" -> Prop Stop_opacity
  | "flood-opacity" -> Prop Flood_opacity
  | "animation-name" -> Prop Animation_name
  | "transform" -> Prop Transform
  | "transform-origin" -> Prop Transform_origin
  | "transform-box" -> Prop Transform_box
  | "translate" -> Prop Translate
  | "box-sizing" -> Prop Box_sizing
  | "field-sizing" -> Prop Field_sizing
  | "caption-side" -> Prop Caption_side
  | "grid-template-columns" -> Prop Grid_template_columns
  | "grid-template-rows" -> Prop Grid_template_rows
  | "box-shadow" -> Prop Box_shadow
  | "content" -> Prop Content
  | "counter-reset" -> Prop Counter_reset
  | "counter-increment" -> Prop Counter_increment
  | "accent-color" -> Prop Accent_color
  | "caret-color" -> Prop Caret_color
  (* Common properties that were missing *)
  | "border" -> Prop Border
  | "resize" -> Prop Resize
  | "user-select" -> Prop User_select
  | "pointer-events" -> Prop Pointer_events
  | "cursor" -> Prop Cursor
  | "interactivity" -> Prop Interactivity
  | "caret-animation" -> Prop Caret_animation
  | "caret-shape" -> Prop Caret_shape
  | "caret" -> Prop Caret
  | "interest-delay" -> Prop Interest_delay
  | "interest-delay-start" -> Prop Interest_delay_start
  | "interest-delay-end" -> Prop Interest_delay_end
  | "nav-up" -> Prop Nav_up
  | "nav-right" -> Prop Nav_right
  | "nav-down" -> Prop Nav_down
  | "nav-left" -> Prop Nav_left
  | "appearance" -> Prop Appearance
  | "color-scheme" -> Prop Color_scheme
  | "print-color-adjust" -> Prop Print_color_adjust
  | "-webkit-print-color-adjust" -> Prop Webkit_print_color_adjust
  | "box-decoration-break" -> Prop Box_decoration_break
  | "-webkit-box-decoration-break" -> Prop Webkit_box_decoration_break
  | "filter" -> Prop Filter
  | "transition" -> Prop Transition
  | "animation" -> Prop Animation
  | "transition-behavior" -> Prop Transition_behavior
  | "overlay" -> Prop Overlay
  | "text-shadow" -> Prop Text_shadow
  | "font" -> Prop Font
  | "outline" -> Prop Outline
  | "z-index" -> Prop Z_index
  | "zoom" -> Prop Zoom
  | "inset" -> Prop Inset
  | "inset-inline" -> Prop Inset_inline
  | "inset-inline-start" -> Prop Inset_inline_start
  | "inset-inline-end" -> Prop Inset_inline_end
  | "inset-block" -> Prop Inset_block
  | "inset-block-start" -> Prop Inset_block_start
  | "inset-block-end" -> Prop Inset_block_end
  | "top" -> Prop Top
  | "right" -> Prop Right
  | "bottom" -> Prop Bottom
  | "left" -> Prop Left
  | "border-top" -> Prop Border_top
  | "border-right" -> Prop Border_right
  | "border-bottom" -> Prop Border_bottom
  | "border-left" -> Prop Border_left
  | "border-collapse" -> Prop Border_collapse
  | "tab-size" -> Prop Tab_size
  | "line-height" -> Prop Line_height
  | "list-style" -> Prop List_style
  | "vertical-align" -> Prop Vertical_align
  (* Missing properties to add *)
  | "align-content" -> Prop Align_content
  | "align-self" -> Prop Align_self
  | "animation-delay" -> Prop Animation_delay
  | "animation-direction" -> Prop Animation_direction
  | "animation-duration" -> Prop Animation_duration
  | "animation-fill-mode" -> Prop Animation_fill_mode
  | "animation-iteration-count" -> Prop Animation_iteration_count
  | "animation-play-state" -> Prop Animation_play_state
  | "animation-composition" -> Prop Animation_composition
  | "animation-timing-function" -> Prop Animation_timing_function
  | "aspect-ratio" -> Prop Aspect_ratio
  | "backdrop-filter" -> Prop Backdrop_filter
  | "-webkit-backdrop-filter" -> Prop Webkit_backdrop_filter
  | "-webkit-mask-image" -> Prop Webkit_mask_image
  | "-webkit-mask-composite" -> Prop Webkit_mask_composite
  | "-webkit-mask-source-type" -> Prop Webkit_mask_source_type
  | "-webkit-mask-size" -> Prop Webkit_mask_size
  | "-webkit-mask-position" -> Prop Webkit_mask_position
  | "-webkit-mask-repeat" -> Prop Webkit_mask_repeat
  | "-webkit-mask-clip" -> Prop Webkit_mask_clip
  | "-webkit-mask-origin" -> Prop Webkit_mask_origin
  | "border-image-source" -> Prop Border_image_source
  | "border-image-slice" -> Prop Border_image_slice
  | "border-image-repeat" -> Prop Border_image_repeat
  | "border-image-width" -> Prop Border_image_width
  | "border-image-outset" -> Prop Border_image_outset
  | "mask-image" -> Prop Mask_image
  | "mask-composite" -> Prop Mask_composite
  | "mask-mode" -> Prop Mask_mode
  | "mask-size" -> Prop Mask_size
  | "mask-position" -> Prop Mask_position
  | "mask-repeat" -> Prop Mask_repeat
  | "mask-border" -> Prop Mask_border
  | "mask-clip" -> Prop Mask_clip
  | "mask-origin" -> Prop Mask_origin
  | "mask-type" -> Prop Mask_type
  | "backface-visibility" -> Prop Backface_visibility
  | "background-attachment" -> Prop Background_attachment
  | "background-blend-mode" -> Prop Background_blend_mode
  | "background-origin" -> Prop Background_origin
  | "background-clip" -> Prop Background_clip
  | "-webkit-background-clip" -> Prop Webkit_background_clip
  | "background-position" -> Prop Background_position
  | "background-position-x" -> Prop Background_position_x
  | "background-position-y" -> Prop Background_position_y
  | "-webkit-mask-position-x" -> Prop Webkit_mask_position_x
  | "-webkit-mask-position-y" -> Prop Webkit_mask_position_y
  | "background-repeat" -> Prop Background_repeat
  | "background-size" -> Prop Background_size
  | "border-block" -> Prop Border_block
  | "border-block-start" -> Prop Border_block_start
  | "border-block-end" -> Prop Border_block_end
  | "border-inline" -> Prop Border_inline
  | "border-inline-start" -> Prop Border_inline_start
  | "border-inline-end" -> Prop Border_inline_end
  | "border-end-end-radius" -> Prop Border_end_end_radius
  | "border-end-start-radius" -> Prop Border_end_start_radius
  | "border-inline-end-color" -> Prop Border_inline_end_color
  | "border-block-start-color" -> Prop Border_block_start_color
  | "border-block-end-color" -> Prop Border_block_end_color
  | "border-block-end-width" -> Prop Border_block_end_width
  | "border-block-start-width" -> Prop Border_block_start_width
  | "border-block-style" -> Prop Border_block_style
  | "border-inline-end-width" -> Prop Border_inline_end_width
  | "border-inline-start-color" -> Prop Border_inline_start_color
  | "border-inline-start-width" -> Prop Border_inline_start_width
  | "border-inline-style" -> Prop Border_inline_style
  | "border-spacing" -> Prop Border_spacing
  | "border-start-end-radius" -> Prop Border_start_end_radius
  | "border-start-start-radius" -> Prop Border_start_start_radius
  | "break-before" -> Prop Break_before
  | "break-after" -> Prop Break_after
  | "break-inside" -> Prop Break_inside
  | "size" -> Prop Page_size
  (* CSS Fragmentation 3 sec. 3.4 page-break-* aliases. Keep them as typed
     legacy properties so pretty output preserves the authored property name;
     minified output still serializes through the shorter modern break-*
     spelling. *)
  | "page-break-before" -> Prop Page_break_before
  | "page-break-after" -> Prop Page_break_after
  | "page-break-inside" -> Prop Page_break_inside
  | "columns" -> Prop Columns
  | "column-width" -> Prop Column_width
  | "column-height" -> Prop Column_height
  | "column-wrap" -> Prop Column_wrap
  | "column-count" -> Prop Column_count
  | "column-rule" -> Prop Column_rule
  | "column-rule-color" -> Prop Column_rule_color
  | "column-rule-width" -> Prop Column_rule_width
  | "column-rule-style" -> Prop Column_rule_style
  | "column-span" -> Prop Column_span
  | "clear" -> Prop Clear
  | "clip" -> Prop Clip
  | "clip-path" -> Prop Clip_path
  | "column-gap" -> Prop Column_gap
  | "contain" -> Prop Contain
  | "container-name" -> Prop Container_name
  | "container-type" -> Prop Container_type
  | "container" -> Prop Container
  | "anchor-name" -> Prop Anchor_name
  | "position-anchor" -> Prop Position_anchor
  | "position-try-fallbacks" -> Prop Position_try_fallbacks
  | "position-try-order" -> Prop Position_try_order
  | "position-try" -> Prop Position_try
  | "position-visibility" -> Prop Position_visibility
  | "position-area" -> Prop Position_area
  | "shape-outside" -> Prop Shape_outside
  | "shape-margin" -> Prop Shape_margin
  | "shape-image-threshold" -> Prop Shape_image_threshold
  | "overflow-clip-margin" -> Prop Overflow_clip_margin
  | "overflow-anchor" -> Prop Overflow_anchor
  | "scrollbar-width" -> Prop Scrollbar_width
  | "scrollbar-color" -> Prop Scrollbar_color
  | "scrollbar-gutter" -> Prop Scrollbar_gutter
  | "line-height-step" -> Prop Line_height_step
  | "font-palette" -> Prop Font_palette
  | "font-synthesis" -> Prop Font_synthesis
  | "text-wrap-mode" -> Prop Text_wrap_mode
  | "text-wrap-style" -> Prop Text_wrap_style
  | "text-box-trim" -> Prop Text_box_trim
  | "text-underline-position" -> Prop Text_underline_position
  | "text-box-edge" -> Prop Text_box_edge
  | "text-box" -> Prop Text_box
  | "inline-sizing" -> Prop Inline_sizing
  | "line-fit-edge" -> Prop Line_fit_edge
  | "interpolate-size" -> Prop Interpolate_size
  | "min-intrinsic-sizing" -> Prop Min_intrinsic_sizing
  | "ruby-align" -> Prop Ruby_align
  | "ruby-merge" -> Prop Ruby_merge
  | "ruby-overhang" -> Prop Ruby_overhang
  | "ruby-position" -> Prop Ruby_position
  | "glyph-orientation-vertical" -> Prop Glyph_orientation_vertical
  | "animation-timeline" -> Prop Animation_timeline
  | "animation-range" -> Prop Animation_range
  | "animation-range-start" -> Prop Animation_range_start
  | "animation-range-end" -> Prop Animation_range_end
  | "scroll-timeline" -> Prop Scroll_timeline
  | "scroll-timeline-name" -> Prop Scroll_timeline_name
  | "scroll-timeline-axis" -> Prop Scroll_timeline_axis
  | "view-transition-name" -> Prop View_transition_name
  | "view-transition-class" -> Prop View_transition_class
  | "image-orientation" -> Prop Image_orientation
  | "image-rendering" -> Prop Image_rendering
  | "image-resolution" -> Prop Image_resolution
  | "contain-intrinsic-size" -> Prop Contain_intrinsic_size
  | "contain-intrinsic-width" -> Prop Contain_intrinsic_width
  | "contain-intrinsic-height" -> Prop Contain_intrinsic_height
  | "contain-intrinsic-block-size" -> Prop Contain_intrinsic_block_size
  | "contain-intrinsic-inline-size" -> Prop Contain_intrinsic_inline_size
  | "margin-trim" -> Prop Margin_trim
  | "offset-path" -> Prop Offset_path
  | "offset" -> Prop Offset
  | "offset-anchor" -> Prop Offset_anchor
  | "offset-position" -> Prop Offset_position
  | "offset-distance" -> Prop Offset_distance
  | "offset-rotate" -> Prop Offset_rotate
  | "font-size-adjust" -> Prop Font_size_adjust
  | "font-variant-emoji" -> Prop Font_variant_emoji
  | "text-spacing-trim" -> Prop Text_spacing_trim
  | "hyphenate-limit-chars" -> Prop Hyphenate_limit_chars
  | "initial-letter" -> Prop Initial_letter
  | "initial-letter-align" -> Prop Initial_letter_align
  | "initial-letter-wrap" -> Prop Initial_letter_wrap
  | "dominant-baseline" -> Prop Dominant_baseline
  | "view-timeline-name" -> Prop View_timeline_name
  | "view-timeline-axis" -> Prop View_timeline_axis
  | "view-timeline-inset" -> Prop View_timeline_inset
  | "view-timeline" -> Prop View_timeline
  | "timeline-scope" -> Prop Timeline_scope
  | "content-visibility" -> Prop Content_visibility
  | "direction" -> Prop Direction
  | "fill" -> Prop Fill
  (* SVG 2 sec. 13.4.2 and CSS Masking 1 sec. 6.2: both take the same
     <fill-rule>. *)
  | "fill-rule" -> Prop Fill_rule
  | "clip-rule" -> Prop Clip_rule
  | "stroke-linecap" -> Prop Stroke_linecap
  | "stroke-linejoin" -> Prop Stroke_linejoin
  | "stroke-miterlimit" -> Prop Stroke_miterlimit
  | "stroke-dashoffset" -> Prop Stroke_dashoffset
  | "stroke-dasharray" -> Prop Stroke_dasharray
  | "paint-order" -> Prop Paint_order
  | "vector-effect" -> Prop Vector_effect
  (* SVG 2 sec. 14.2.4.2 / Filter Effects 1 sec. 9.13.1 and 11.5: each is a
     plain <color>, so they minify like any other colour-valued property. *)
  | "stop-color" -> Prop Stop_color
  | "flood-color" -> Prop Flood_color
  | "lighting-color" -> Prop Lighting_color
  | "flex-basis" -> Prop Flex_basis
  | "flex-grow" -> Prop Flex_grow
  | "flex-shrink" -> Prop Flex_shrink
  | "float" -> Prop Float
  | "font-stretch" -> Prop Font_stretch
  | "font-optical-sizing" -> Prop Font_optical_sizing
  | "font-kerning" -> Prop Font_kerning
  | "font-language-override" -> Prop Font_language_override
  | "font-synthesis-style" -> Prop Font_synthesis_style
  | "font-synthesis-weight" -> Prop Font_synthesis_weight
  | "font-synthesis-small-caps" -> Prop Font_synthesis_small_caps
  | "font-synthesis-position" -> Prop Font_synthesis_position
  | "font-variant-ligatures" -> Prop Font_variant_ligatures
  | "font-variant-caps" -> Prop Caps
  | "font-variant-numeric" -> Prop Numeric
  | "font-variant-position" -> Prop Font_variant_position
  | "font-variant-east-asian" -> Prop East_asian
  | "forced-color-adjust" -> Prop Forced_color_adjust
  | "gap" -> Prop Gap
  | "grid-area" -> Prop Grid_area
  | "grid-auto-columns" -> Prop Grid_auto_columns
  | "grid-auto-flow" -> Prop Grid_auto_flow
  | "grid-auto-rows" -> Prop Grid_auto_rows
  | "grid-column" -> Prop Grid_column
  | "grid-column-end" -> Prop Grid_column_end
  | "grid-column-start" -> Prop Grid_column_start
  | "grid-row" -> Prop Grid_row
  | "grid-row-end" -> Prop Grid_row_end
  | "grid-row-start" -> Prop Grid_row_start
  | "grid" -> Prop Grid
  | "grid-template" -> Prop Grid_template
  | "grid-template-areas" -> Prop Grid_template_areas
  | "hyphens" -> Prop Hyphens
  | "isolation" -> Prop Isolation
  | "justify-items" -> Prop Justify_items
  | "justify-self" -> Prop Justify_self
  | "list-style-image" -> Prop List_style_image
  | "list-style-position" -> Prop List_style_position
  | "list-style-type" -> Prop List_style_type
  | "mask" -> Prop Mask
  | "mix-blend-mode" -> Prop Mix_blend_mode
  | "object-fit" -> Prop Object_fit
  | "object-view-box" -> Prop Object_view_box
  | "object-position" -> Prop Object_position
  | "order" -> Prop Order
  | "outline-offset" -> Prop Outline_offset
  | "outline-style" -> Prop Outline_style
  | "outline-width" -> Prop Outline_width
  | "overflow-wrap" -> Prop Overflow_wrap
  | "overscroll-behavior" -> Prop Overscroll_behavior
  | "overscroll-behavior-x" -> Prop Overscroll_behavior_x
  | "overscroll-behavior-y" -> Prop Overscroll_behavior_y
  | "overscroll-behavior-block" -> Prop Overscroll_behavior_block
  | "overscroll-behavior-inline" -> Prop Overscroll_behavior_inline
  | "perspective" -> Prop Perspective
  | "perspective-origin" -> Prop Perspective_origin
  | "place-content" -> Prop Place_content
  | "place-items" -> Prop Place_items
  | "place-self" -> Prop Place_self
  | "quotes" -> Prop Quotes
  | "rotate" -> Prop Rotate
  | "row-gap" -> Prop Row_gap
  | "scale" -> Prop Scale
  | "scroll-behavior" -> Prop Scroll_behavior
  | "scroll-margin" -> Prop Scroll_margin
  | "scroll-margin-bottom" -> Prop Scroll_margin_bottom
  | "scroll-margin-left" -> Prop Scroll_margin_left
  | "scroll-margin-right" -> Prop Scroll_margin_right
  | "scroll-margin-top" -> Prop Scroll_margin_top
  | "scroll-margin-inline" -> Prop Scroll_margin_inline
  | "scroll-margin-inline-start" -> Prop Scroll_margin_inline_start
  | "scroll-margin-inline-end" -> Prop Scroll_margin_inline_end
  | "scroll-margin-block" -> Prop Scroll_margin_block
  | "scroll-margin-block-start" -> Prop Scroll_margin_block_start
  | "scroll-margin-block-end" -> Prop Scroll_margin_block_end
  | "scroll-padding" -> Prop Scroll_padding
  | "scroll-padding-bottom" -> Prop Scroll_padding_bottom
  | "scroll-padding-left" -> Prop Scroll_padding_left
  | "scroll-padding-right" -> Prop Scroll_padding_right
  | "scroll-padding-top" -> Prop Scroll_padding_top
  | "scroll-padding-inline" -> Prop Scroll_padding_inline
  | "scroll-padding-inline-start" -> Prop Scroll_padding_inline_start
  | "scroll-padding-inline-end" -> Prop Scroll_padding_inline_end
  | "scroll-padding-block" -> Prop Scroll_padding_block
  | "scroll-padding-block-start" -> Prop Scroll_padding_block_start
  | "scroll-padding-block-end" -> Prop Scroll_padding_block_end
  | "scroll-snap-align" -> Prop Scroll_snap_align
  | "scroll-snap-stop" -> Prop Scroll_snap_stop
  | "scroll-snap-type" -> Prop Scroll_snap_type
  | "stroke" -> Prop Stroke
  | "stroke-width" -> Prop Stroke_width
  | "table-layout" -> Prop Table_layout
  | "text-decoration-skip-ink" -> Prop Text_decoration_skip_ink
  | "text-decoration-style" -> Prop Text_decoration_style
  | "text-decoration-thickness" -> Prop Text_decoration_thickness
  | "text-overflow" -> Prop Text_overflow
  | "text-size-adjust" -> Prop Text_size_adjust
  | "text-underline-offset" -> Prop Text_underline_offset
  | "text-wrap" -> Prop Text_wrap
  | "line-break" -> Prop Line_break
  | "touch-action" -> Prop Touch_action
  | "transform-style" -> Prop Transform_style
  | "transition-delay" -> Prop Transition_delay
  | "transition-duration" -> Prop Transition_duration
  | "transition-property" -> Prop Transition_property
  | "transition-timing-function" -> Prop Transition_timing_function
  | "unicode-bidi" -> Prop Unicode_bidi
  | "white-space" -> Prop White_space
  | "white-space-collapse" -> Prop White_space_collapse
  | "font-variant-alternates" -> Prop Font_variant_alternates
  | "font-variant" -> Prop Font_variant
  | "will-change" -> Prop Will_change
  | "word-break" -> Prop Word_break
  | "word-spacing" -> Prop Word_spacing
  | "writing-mode" -> Prop Writing_mode
  | "text-combine-upright" -> Prop Text_combine_upright
  (* Vendor prefixed properties *)
  | "-webkit-transform" -> Prop Webkit_transform
  | "-moz-transform" -> Prop Moz_transform
  | "-ms-transform" -> Prop Ms_transform
  | "-o-transform" -> Prop O_transform
  | "-webkit-transition" -> Prop Webkit_transition
  | "-webkit-transition-delay" -> Prop Webkit_transition_delay
  | "-webkit-transition-duration" -> Prop Webkit_transition_duration
  | "-webkit-transition-property" -> Prop Webkit_transition_property
  | "-webkit-transition-timing-function" ->
      Prop Webkit_transition_timing_function
  | "-webkit-animation" -> Prop Webkit_animation
  | "-webkit-animation-delay" -> Prop Webkit_animation_delay
  | "-webkit-animation-duration" -> Prop Webkit_animation_duration
  | "-webkit-animation-direction" -> Prop Webkit_animation_direction
  | "-webkit-animation-iteration-count" -> Prop Webkit_animation_iteration_count
  | "-webkit-animation-name" -> Prop Webkit_animation_name
  | "-webkit-animation-timing-function" -> Prop Webkit_animation_timing_function
  | "-webkit-animation-fill-mode" -> Prop Webkit_animation_fill_mode
  | "-webkit-animation-play-state" -> Prop Webkit_animation_play_state
  | "-webkit-flex-direction" -> Prop Webkit_flex_direction
  | "-webkit-flex-wrap" -> Prop Webkit_flex_wrap
  | "-webkit-flex-flow" -> Prop Webkit_flex_flow
  | "-webkit-justify-content" -> Prop Webkit_justify_content
  | "-webkit-align-items" -> Prop Webkit_align_items
  | "-webkit-align-content" -> Prop Webkit_align_content
  | "-webkit-align-self" -> Prop Webkit_align_self
  | "-webkit-border-radius" -> Prop Webkit_border_radius
  | "-webkit-box-sizing" -> Prop Webkit_box_sizing
  | "-moz-box-sizing" -> Prop Moz_box_sizing
  | "-webkit-box-shadow" -> Prop Webkit_box_shadow
  | "-webkit-background-size" -> Prop Webkit_background_size
  | "-webkit-filter" -> Prop Webkit_filter
  | "-moz-animation" -> Prop Moz_animation
  | "-moz-animation-delay" -> Prop Moz_animation_delay
  | "-moz-animation-duration" -> Prop Moz_animation_duration
  | "-moz-animation-direction" -> Prop Moz_animation_direction
  | "-moz-animation-iteration-count" -> Prop Moz_animation_iteration_count
  | "-moz-animation-name" -> Prop Moz_animation_name
  | "-moz-animation-timing-function" -> Prop Moz_animation_timing_function
  | "-moz-animation-fill-mode" -> Prop Moz_animation_fill_mode
  | "-moz-animation-play-state" -> Prop Moz_animation_play_state
  | "-moz-transition" -> Prop Moz_transition
  | "-moz-transition-delay" -> Prop Moz_transition_delay
  | "-moz-transition-duration" -> Prop Moz_transition_duration
  | "-moz-transition-property" -> Prop Moz_transition_property
  | "-moz-transition-timing-function" -> Prop Moz_transition_timing_function
  | "-moz-border-radius" -> Prop Moz_border_radius
  | "-moz-box-shadow" -> Prop Moz_box_shadow
  | "-webkit-text-size-adjust" -> Prop Webkit_text_size_adjust
  | "-webkit-tap-highlight-color" -> Prop Webkit_tap_highlight_color
  | "-webkit-text-fill-color" -> Prop Webkit_text_fill_color
  | "-webkit-text-stroke-color" -> Prop Webkit_text_stroke_color
  | "-webkit-text-stroke" -> Prop Webkit_text_stroke
  | "-webkit-text-stroke-width" -> Prop Webkit_text_stroke_width
  | "-webkit-user-select" -> Prop Webkit_user_select
  | "-ms-user-select" -> Prop Ms_user_select
  | "-moz-user-select" -> Prop Moz_user_select
  | "-webkit-text-decoration" -> Prop Webkit_text_decoration
  | "-webkit-text-decoration-color" -> Prop Webkit_text_decoration_color
  | "-webkit-appearance" -> Prop Webkit_appearance
  | "-webkit-font-smoothing" -> Prop Webkit_font_smoothing
  | "-webkit-line-clamp" -> Prop Webkit_line_clamp
  | "-webkit-box-orient" -> Prop Webkit_box_orient
  | "-webkit-hyphens" -> Prop Webkit_hyphens
  | "-moz-appearance" -> Prop Moz_appearance
  | "-moz-orient" -> Prop Moz_orient
  | "-moz-osx-font-smoothing" -> Prop Moz_osx_font_smoothing
  | "-ms-filter" -> Prop Ms_filter
  | "-o-transition" -> Prop O_transition
  (* PROPERTY_MATCHING_END - Used by scripts/check_properties.ml *)
  (* Custom properties [--*] always pass through as [Unknown_property] (their
     value is opaque); other unrecognized names fail here. The lenient
     declaration recovery in [Declaration.read_regular_property_declaration]
     catches and falls back to [read_unknown_property_declaration]. *)
  | _ when Custom_property_name.has_prefix prop_name ->
      Prop (Unknown_property prop_name)
  | _ -> Cursor.err_invalid t ("unknown property: " ^ prop_name)

(* Helper functions for property types *)

let pp_any_property ctx (Prop p) = pp_property ctx p

let read_custom_value_as kind read components =
  match
    let cursor = Cursor.of_components components in
    let parsed = read cursor in
    Cursor.ws cursor;
    Cursor.expect_eof cursor;
    Some (Typed { kind; value = parsed })
  with
  | result -> result
  | exception Cursor.Parse_error _ -> None

(* CSS Custom Properties for Cascading Variables 1 sec. 2: an unregistered
   custom property is an opaque token stream that [var()] later substitutes
   wholesale into whichever consumer site invokes it. Canonical typed rewrites
   like [rgb(0 0 0) -> #000] assume a consumer type that only [@property --foo {
   syntax: "<color>"; ... }] can promise, so the parser leaves the tokens alone
   here. *)
let read_custom_property_value ?font_family:_ cursor =
  Tokens (Cursor.remaining cursor)

(* A registered [<color>] custom property carries a typed colour once promoted,
   so canonicalise it the same way a real colour property would. *)
let is_color_function name =
  match String.lowercase_ascii name with
  | "rgb" | "rgba" | "hsl" | "hsla" | "hwb" | "lab" | "lch" | "oklab" | "oklch"
  | "color" | "color-mix" | "light-dark" ->
      true
  | _ -> false

(* A construct whose type is fixed by its own syntax - a complete colour
   function ([oklab(...)], [color-mix(...)], ...) or a hex colour ([#abc]) - is
   unconditionally a colour in every [var()] substitution site, so folding it to
   its canonical spelling inside an opaque custom-property token stream
   preserves every rendered result while collapsing two spellings of the same
   colour. The only observable change is the exact token string a script reads
   back via [getPropertyValue]; that readback is the optimizer's domain (it
   already folds insignificant math whitespace here), so the canonical diff
   inherits this fold rather than shimming it. Bare keywords are left untouched
   - they may be a [<custom-ident>] in a non-colour context, whereas a hex token
   never can. *)
(* [c] is one component whose syntax fixes it as a colour; fold it to the
   shortest non-keyword spelling, falling back to [fallback ()] when it does not
   actually parse as a complete colour. *)
let fold_custom_color ~lossless (c : Component.t) ~fallback =
  let text = Parser.string_of_components [ c ] in
  let cur = Cursor.of_string text in
  match
    try Some (Values.read_color cur) with Cursor.Parse_error _ -> None
  with
  | Some col when Cursor.is_done cur -> (
      let canon =
        Pp.to_string ~minify:true Values.pp_color
          (Values.nonkeyword_color (Values.normalize_color ~lossless col))
      in
      match read_custom_property_value (Cursor.of_string canon) with
      | Tokens cs -> cs
      | Typed _ -> [ c ])
  | _ -> fallback ()

let is_math_function = Parser.is_math_function

(* A math function is unconditionally a math expression whose type is fixed by
   its operands' units, so when it reduces to a single constant it has that
   value in every [var()] substitution site - fold it inside an opaque
   custom-property stream like a complete colour. [<number>] and the
   unit-unambiguous dimensions ([<angle>] / [<time>]) qualify; [<percentage>] is
   ambiguous (length vs number percentage) so it stays verbatim, as does a
   function that still references a [var()] (it does not reduce to a leaf). *)
let fold_custom_calc (c : Component.t) ~fallback =
  let text = Parser.string_of_components [ c ] in
  (* Parse the whole token as one typed value and fold only when it reduces to a
     single concrete leaf (not a [calc()] that still carries a [var()]). *)
  let try_typed : type a.
      (Cursor.t -> a) ->
      (a -> a) ->
      a Pp.t ->
      (a -> bool) ->
      Component.t list option =
   fun reader normalize pp reduced ->
    let cur = Cursor.of_string text in
    match try Some (reader cur) with Cursor.Parse_error _ -> None with
    | Some v when Cursor.is_done cur ->
        let folded = normalize v in
        if reduced folded then
          match
            read_custom_property_value
              (Cursor.of_string (Pp.to_string ~minify:true pp folded))
          with
          | Tokens cs -> Some cs
          | Typed _ -> None
        else None
    | _ -> None
  in
  let number_reduced = function (Num _ : number) -> true | _ -> false in
  let angle_reduced = function
    | (Deg _ | Rad _ | Turn _ | Grad _ : angle) -> true
    | _ -> false
  in
  let time_reduced = function (S _ | Ms _ : duration) -> true | _ -> false in
  match
    List.find_map Fun.id
      [
        try_typed read_number
          (fun n -> normalize_number n)
          pp_number number_reduced;
        try_typed read_angle_unit_required
          (fun a -> normalize_angle a)
          pp_angle angle_reduced;
        try_typed read_duration
          (fun d -> normalize_duration d)
          pp_duration time_reduced;
      ]
  with
  | Some cs -> cs
  | None -> fallback ()

(* Filter Effects 1 sec. 6.1 gives [hue-rotate()] the argument [[ <angle> |
   <zero> ]?] and 0 when omitted, so a zero argument is redundant. [hue-rotate]
   names a filter function and nothing else, so this holds wherever the stream
   is substituted, which is the same argument that lets a colour function fold
   here. *)
let hue_rotate_zero_argument (func : Component.func) =
  String.lowercase_ascii func.name = "hue-rotate"
  && func.arguments <> []
  &&
  let cur = Cursor.of_string (Parser.string_of_components func.arguments) in
  match read_angle_unit_required cur with
  | exception Cursor.Parse_error _ -> false
  | angle ->
      Cursor.is_done cur
      && Values.angle_degrees_opt (normalize_angle angle) = Some 0.

let drop_function_arguments wrapped =
  Component.Func
    { wrapped with node = { wrapped.Component.node with arguments = [] } }

let rec normalize_shadow_colors ~lossless (shadow : shadow) =
  let normalize_body (body : shadow_body) : shadow_body =
    {
      body with
      color = option_map_preserve (Values.normalize_color ~lossless) body.color;
    }
  in
  match shadow with
  | Shadow body -> preserve_if_equal shadow (Shadow (normalize_body body))
  | Inset (Body body) ->
      preserve_if_equal shadow (Inset (Body (normalize_body body)) : shadow)
  | Inset (Toggle ({ body; _ } as toggle)) ->
      preserve_if_equal shadow
        (Inset (Toggle { toggle with body = normalize_body body }) : shadow)
  | List shadows ->
      preserve_if_equal shadow
        (List (map_preserve (normalize_shadow_colors ~lossless) shadows))
  | other -> other

(* A complete shadow token stream is self-typing: its length sequence and
   optional final <color> are parsed together, so a bare colour keyword cannot
   be mistaken for a custom-ident at another substitution site. Canonicalise
   only that typed colour while preserving the authored shadow shape; applying
   the full shadow optimiser here would also drop explicit default lengths from
   an otherwise opaque custom-property token stream. *)
let canonicalize_custom_shadow_components ~lossless components =
  let t = Cursor.of_string (Parser.string_of_components components) in
  match try Some (read_shadow t) with Cursor.Parse_error _ -> None with
  | Some shadow when Cursor.is_done t -> (
      let shadow = normalize_shadow_colors ~lossless shadow in
      let canonical = Pp.to_string ~minify:true pp_shadow shadow in
      match read_custom_property_value (Cursor.of_string canonical) with
      | Tokens components -> components
      | Typed _ -> components)
  | _ -> components

let rec canonicalize_custom_colors_components ~lossless comps =
  let fold_color c ~fallback = fold_custom_color ~lossless c ~fallback in
  List.concat_map
    (fun (c : Component.t) ->
      match c with
      | Component.Func wrapped
        when is_color_function wrapped.Component.node.name ->
          fold_color c ~fallback:(fun () ->
              let func = wrapped.Component.node in
              let args =
                canonicalize_custom_colors_components ~lossless func.arguments
              in
              [
                Component.Func
                  { wrapped with node = { func with arguments = args } };
              ])
      | Component.Preserved { kind = Token.Hash _; _ } ->
          fold_color c ~fallback:(fun () -> [ c ])
      | Component.Func wrapped when is_math_function wrapped.Component.node.name
        ->
          fold_custom_calc c ~fallback:(fun () ->
              let func = wrapped.Component.node in
              let args =
                canonicalize_custom_colors_components ~lossless func.arguments
              in
              [
                Component.Func
                  { wrapped with node = { func with arguments = args } };
              ])
      | Component.Func wrapped
        when hue_rotate_zero_argument wrapped.Component.node ->
          [ drop_function_arguments wrapped ]
      | Component.Func wrapped ->
          let func = wrapped.Component.node in
          let args =
            canonicalize_custom_colors_components ~lossless func.arguments
          in
          [
            Component.Func
              { wrapped with node = { func with arguments = args } };
          ]
      | Component.Block wrapped ->
          let block = wrapped.Component.node in
          let value =
            canonicalize_custom_colors_components ~lossless block.value
          in
          [ Component.Block { wrapped with node = { block with value } } ]
      | Component.Preserved _ -> [ c ])
    comps

(* Typed re-readers exposed for the registry pass that consumes [@property]
   declarations. Each reader takes a token stream and tries to parse it as the
   matching typed kind, returning [None] when the stream doesn't match. The
   unregistered path stays opaque; the registry pass is what flips a value to
   [Typed]. *)
let try_read_custom_color components =
  read_custom_value_as Color read_color components

let try_read_custom_length components =
  read_custom_value_as Length (read_length ~with_keywords:false) components

let try_read_custom_length_percentage components =
  read_custom_value_as Length_percentage
    (read_length_percentage ~with_keywords:false)
    components

let try_read_custom_number components =
  read_custom_value_as Number read_number components

let try_read_custom_percentage components =
  read_custom_value_as Percentage read_percentage components

let try_read_custom_angle components =
  read_custom_value_as Angle read_angle_unit_required components

let try_read_custom_time components =
  read_custom_value_as Duration read_duration components

let pp_number_value ctx (value : number) =
  match value with
  | Calc c when Pp.minified ctx -> (
      (* [eval_numeric_calc] is Cascade's own arithmetic, so its result takes
         the six significant figures Cascade commits to in serialised output. An
         authored coefficient is the author's digits and keeps every one of
         them, so it falls through to [pp_number]. *)
      match eval_numeric_calc c with
      | Some f -> Pp.float ctx (Pp.round_sig 6 f)
      | None -> pp_number ctx (Calc (eval_calc c)))
  | _ -> pp_number ctx value

let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp (pp_length ~always:true)
  | Color -> pp pp_specified_color
  | Rgb ->
      let rec pp_rgb_type : rgb Pp.t =
       fun ctx rgb ->
        match rgb with
        | Channels { r; g; b } ->
            pp_channel ctx r;
            Pp.space ctx ();
            pp_channel ctx g;
            Pp.space ctx ();
            pp_channel ctx b
        | Var v -> pp_var pp_rgb_type ctx v
      in
      pp pp_rgb_type
  | Number -> pp pp_number_value
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | Percentage -> pp pp_percentage
  | Length_percentage -> pp (pp_length_percentage ~always:true)
  | Number_percentage -> pp pp_number_percentage
  | Opacity -> pp pp_opacity
  | Value ->
      let rendered =
        if Pp.minified ctx then
          Parser.to_string_custom_minified
            ~fold_ident:Values.fold_custom_value_ident value
        else Parser.string_of_components value
      in
      Pp.string ctx rendered
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Outline_style -> pp pp_outline_style
  | Border -> pp pp_border
  | Font_weight -> pp pp_font_weight
  | Font_size -> pp pp_font_size
  | Line_height -> pp pp_line_height
  | Font_family -> pp pp_font_family
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_numeric_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Rotate -> pp pp_rotate_value
  | Scale -> pp pp_scale
  | Content -> pp pp_content
  | Gradient_stop -> pp pp_gradient_stop
  | Gradient_direction -> pp pp_gradient_direction
  | Gradient_position -> pp pp_gradient_position
  | Radial_shape -> pp pp_radial_shape
  | Radial_size -> pp pp_radial_size
  | Position_value -> pp pp_position_value
  | Animation -> pp pp_animation
  | Timing_function -> pp pp_timing_function
  | Transform -> pp pp_transform
  | Touch_action -> pp pp_touch_action
  | Transition_property_value -> pp pp_transition_property_value
  | Background_image -> pp pp_background_image
  | Z_index -> pp pp_z_index
  | Filter -> pp pp_filter
  | Font_src -> pp pp_font_src

let string_of_channel : channel -> string = function
  | Int i -> string_of_int i
  | Num f -> Pp.string_of_float f
  | Pct p -> Pp.string_of_float p ^ "%"
  | Var _ -> "0"
  | None -> "none"

let string_of_kind_value : type a. a kind -> a -> string =
 fun kind value ->
  match kind with
  | Length -> Pp.to_string (pp_length ~always:false) value
  | Color -> Pp.to_string pp_color value
  | Angle -> Pp.to_string pp_angle value
  | Duration -> Pp.to_string pp_duration value
  | Float -> Pp.string_of_float value
  | Percentage -> (
      match value with Pct f -> Pp.string_of_float f | _ -> "initial")
  | Number_percentage -> Values.string_of_number_percentage value
  | Number -> Pp.to_string pp_number value
  | Int -> string_of_int value
  | Value -> Parser.string_of_components value
  | Content -> (
      match value with
      | String "" -> "\"\""
      | String s -> "\"" ^ s ^ "\""
      | Quoted { value; quote; repr = _ } ->
          String.make 1 quote ^ value ^ String.make 1 quote
      | None -> "none"
      | Normal -> "normal"
      | Open_quote -> "open-quote"
      | Close_quote -> "close-quote"
      | Image _ | Attr _ | Counter _ | Counters _ | String_ref _
      | Content_list _ | Inherit | Initial | Unset | Revert | Revert_layer
      | Var _ ->
          "initial")
  | Font_weight -> Pp.to_string pp_font_weight value
  | Shadow -> "0 0 #0000"
  | Border_style -> Pp.to_string pp_border_style value
  | Outline_style -> Pp.to_string pp_outline_style value
  | Scroll_snap_strictness -> Pp.to_string pp_scroll_snap_strictness value
  | Rgb -> (
      match value with
      | Channels { r; g; b } ->
          string_of_channel r ^ " " ^ string_of_channel g ^ " "
          ^ string_of_channel b
      | Var _ -> "initial")
  | Animation -> Pp.to_string pp_animation value
  | Gradient_direction -> Pp.to_string pp_gradient_direction value
  | Gradient_position -> Pp.to_string pp_gradient_position value
  | _ -> "initial"

let pp_custom_property_value ctx = function
  | Typed { kind; value } -> pp_value ctx (kind, value)
  | Tokens value -> pp_value ctx (Value, value)

let components_of_custom_property_value = function
  | Tokens components -> components
  | Typed { kind; value } ->
      Cursor.remaining
        (Cursor.of_string (Pp.to_string ~minify:true pp_value (kind, value)))

let pp_custom_property ctx (Custom_value { value; _ }) =
  pp_custom_property_value ctx value

(* CSS Values 4 sec. 4.1.1: the [initial] keyword resolves to the property's
   spec-defined initial value at computed time. Under [--minify] swap the
   keyword for that value when its serialization is shorter (or the same length
   but a more canonical spelling that cleancss / csso emit). *)
(* Detect the [<css-wide-keyword>] keyword sequences that the box-shorthand
   expander leaves behind: [margin: initial] is read as the singleton
   [[Initial]], then [try_merge_box_shorthand] fans it out to
   [[Initial; Initial; Initial; Initial]] before we reach the printer. *)
let box_is_all_initial : length list -> bool = function
  | [ Initial ] | [ Initial; Initial; Initial; Initial ] -> true
  | _ -> false

let canonical_initial_for_minify : type a. a property -> a -> a =
 fun prop value ->
  match (prop, value) with
  (* CSS2 sec. 9.9.1 gives [z-index] the initial value [auto]. *)
  | Z_index, Initial -> Auto
  (* Motion Path 1 secs. 2.3-2.4: the initial values are [normal] and [auto]. *)
  | Offset_anchor, Initial -> Auto
  | Offset_position, Initial -> Normal
  (* Sec. 2.6 resets all five longhands, so [initial] and a bare [none] path
     leave the same computed values behind. *)
  | Offset, Initial ->
      Shorthand
        {
          target =
            With_path
              {
                position = None;
                path = (None : offset_path);
                distance = None;
                rotate = None;
              };
          anchor = None;
        }
  (* CSS Color 4 sec. 3.3 gives [opacity] the initial value [1]. *)
  | Opacity, Initial -> Opacity_number 1.
  (* CSS Box 4 secs. 3.1 and 4.1 give the margin and padding longhands the
     initial value [0]. *)
  | Margin, vs when box_is_all_initial vs -> [ Px 0. ]
  | Padding, vs when box_is_all_initial vs -> [ Px 0. ]
  | Margin_top, Initial -> Px 0.
  | Margin_right, Initial -> Px 0.
  | Margin_bottom, Initial -> Px 0.
  | Margin_left, Initial -> Px 0.
  | Padding_top, Initial -> Px 0.
  | Padding_right, Initial -> Px 0.
  | Padding_bottom, Initial -> Px 0.
  | Padding_left, Initial -> Px 0.
  (* CSS Sizing 3 secs. 3.1.1-3.1.2 gives [width] / [height] and their physical
     minimum-size properties the initial value [auto]. This is level-sensitive:
     CSS2 sec. 10.4 instead gives [min-width] / [min-height] the initial value
     [0]. *)
  | Width, Length Initial -> Length Auto
  | Height, Length Initial -> Length Auto
  | Min_width, Length Initial -> Length Auto
  | Min_height, Length Initial -> Length Auto
  (* The logical twins fold to [auto] too. Two stale tables say [0] and neither
     governs: CSS2 sec. 10.4 predates CSS Sizing 3, and CSS Logical 1 sec. 4.1's
     own [Initial: 0] line copies that superseded CSS2 value while its [Value:
     <'min-width'>] line and sec. 4 ("paired properties share a computed value")
     both bind these to min-width. [auto] is the automatic minimum size, not
     zero, so folding to [0] would let a flex item shrink below its content. *)
  | Min_inline_size, Length Initial -> Length Auto
  | Min_block_size, Length Initial -> Length Auto
  (* Keep this fallback exhaustive: a new property must make this match fail to
     compile until its initial-value fold has been considered. *)
  | Custom_property _, value -> value
  | Unknown_property _, value -> value
  | All, value -> value
  | ( ( Background_color | Color | Text_decoration_color | Text_emphasis_color
      | Border_top_color | Border_right_color | Border_bottom_color
      | Border_left_color | Border_inline_start_color | Border_inline_end_color
      | Border_block_start_color | Border_block_end_color | Outline_color
      | Webkit_tap_highlight_color | Webkit_text_decoration_color
      | Webkit_text_fill_color | Webkit_text_stroke_color | Column_rule_color
      | Webkit_text_stroke | Stop_color | Flood_color | Lighting_color
      | Accent_color | Caret_color ),
      value ) ->
      value
  | Border_color, value -> value
  | ( ( Border_style | Border_top_style | Border_right_style
      | Border_bottom_style | Border_left_style | Border_inline_start_style
      | Border_inline_end_style | Border_block_start_style
      | Border_block_end_style | Border_inline_style | Border_block_style
      | Column_rule_style ),
      value ) ->
      value
  | ( ( Padding | Padding_inline | Padding_block | Margin | Margin_inline
      | Margin_block | Inset | Inset_inline | Inset_inline_start
      | Inset_inline_end | Inset_block | Inset_block_start | Inset_block_end
      | Top | Right | Bottom | Left | Scroll_margin | Scroll_margin_inline
      | Scroll_margin_block | Scroll_padding | Scroll_padding_inline
      | Scroll_padding_block ),
      value ) ->
      value
  | ( ( Padding_left | Padding_right | Padding_bottom | Padding_top
      | Padding_inline_start | Padding_inline_end | Padding_block_start
      | Padding_block_end | Margin_inline_end | Margin_inline_start
      | Margin_left | Margin_right | Margin_top | Margin_bottom
      | Margin_block_start | Margin_block_end | Column_gap | Row_gap
      | Text_underline_offset | Letter_spacing | Border_top_left_radius
      | Border_top_right_radius | Border_bottom_left_radius
      | Border_bottom_right_radius | Border_start_start_radius
      | Border_start_end_radius | Border_end_start_radius
      | Border_end_end_radius | Outline_offset | Line_height_step | Perspective
      | Word_spacing | Text_decoration_thickness | Scroll_margin_top
      | Scroll_margin_right | Scroll_margin_bottom | Scroll_margin_left
      | Scroll_margin_inline_start | Scroll_margin_inline_end
      | Scroll_margin_block_start | Scroll_margin_block_end | Scroll_padding_top
      | Scroll_padding_right | Scroll_padding_bottom | Scroll_padding_left
      | Scroll_padding_inline_start | Scroll_padding_inline_end
      | Scroll_padding_block_start | Scroll_padding_block_end ),
      value ) ->
      value
  | Gap, value -> value
  | ( ( Width | Height | Min_width | Min_height | Max_width | Max_height
      | Inline_size | Min_inline_size | Max_inline_size | Block_size
      | Min_block_size | Max_block_size | Shape_margin | Offset_distance ),
      value ) ->
      value
  | Font_size, value -> value
  | Line_height, value -> value
  | Font_weight, value -> value
  | Font_style, value -> value
  | Text_align, value -> value
  | (Text_decoration | Webkit_text_decoration), value -> value
  | Text_decoration_line, value -> value
  | Text_decoration_style, value -> value
  | Text_decoration_skip, value -> value
  | Text_decoration_skip_self, value -> value
  | Text_decoration_skip_box, value -> value
  | Text_decoration_skip_inset, value -> value
  | Text_decoration_skip_spaces, value -> value
  | Text_emphasis, value -> value
  | Text_emphasis_style, value -> value
  | Text_emphasis_position, value -> value
  | Text_emphasis_skip, value -> value
  | Text_orientation, value -> value
  | Text_transform, value -> value
  | List_style_type, value -> value
  | List_style_position, value -> value
  | List_style_image, value -> value
  | Display, value -> value
  | Position, value -> value
  | Visibility, value -> value
  | Baseline_source, value -> value
  | Alignment_baseline, value -> value
  | Baseline_shift, value -> value
  | (Flex_direction | Webkit_flex_direction), value -> value
  | (Flex_wrap | Webkit_flex_wrap), value -> value
  | (Flex_flow | Webkit_flex_flow), value -> value
  | Flex, value -> value
  | (Flex_grow | Flex_shrink), value -> value
  | Flex_basis, value -> value
  | Order, value -> value
  | (Align_items | Webkit_align_items), value -> value
  | (Justify_content | Webkit_justify_content), value -> value
  | Justify_items, value -> value
  | Justify_self, value -> value
  | (Align_content | Webkit_align_content), value -> value
  | (Align_self | Webkit_align_self), value -> value
  | Place_content, value -> value
  | Place_items, value -> value
  | Place_self, value -> value
  | ( ( Grid_template_columns | Grid_template_rows | Grid_template | Grid
      | Grid_auto_columns | Grid_auto_rows ),
      value ) ->
      value
  | Grid_template_areas, value -> value
  | Grid_area, value -> value
  | Grid_auto_flow, value -> value
  | (Grid_column | Grid_row), value -> value
  | (Grid_column_start | Grid_column_end | Grid_row_start | Grid_row_end), value
    ->
      value
  | Border_width, value -> value
  | ( ( Border_top_width | Border_right_width | Border_bottom_width
      | Border_left_width | Border_inline_start_width | Border_inline_end_width
      | Border_block_start_width | Border_block_end_width | Outline_width
      | Column_rule_width | Webkit_text_stroke_width ),
      value ) ->
      value
  | (Border_inline_width | Border_block_width), value -> value
  | (Border_image | Mask_border), value -> value
  | (Border_image_source | Webkit_mask_image | Mask_image), value -> value
  | Border_image_slice, value -> value
  | Border_image_repeat, value -> value
  | Border_image_width, value -> value
  | Border_image_outset, value -> value
  | (Border_radius | Webkit_border_radius | Moz_border_radius), value -> value
  | (Border_inline_color | Border_block_color), value -> value
  | ( (Opacity | Fill_opacity | Stroke_opacity | Stop_opacity | Flood_opacity),
      value ) ->
      value
  | Mix_blend_mode, value -> value
  | ( (Transform | Webkit_transform | Moz_transform | Ms_transform | O_transform),
      value ) ->
      value
  | Translate, value -> value
  | Cursor, value -> value
  | Interactivity, value -> value
  | Caret_animation, value -> value
  | Caret_shape, value -> value
  | Caret, value -> value
  | (Interest_delay | Interest_delay_start | Interest_delay_end), value -> value
  | (Nav_up | Nav_right | Nav_down | Nav_left), value -> value
  | Table_layout, value -> value
  | Border_collapse, value -> value
  | Border_spacing, value -> value
  | (User_select | Webkit_user_select | Moz_user_select | Ms_user_select), value
    ->
      value
  | Pointer_events, value -> value
  | ( (Overflow | Overflow_x | Overflow_y | Overflow_block | Overflow_inline),
      value ) ->
      value
  | Z_index, value -> value
  | Outline, value -> value
  | Outline_style, value -> value
  | Forced_color_adjust, value -> value
  | Scroll_snap_type, value -> value
  | White_space, value -> value
  | White_space_collapse, value -> value
  | Font_variant_alternates, value -> value
  | Font_variant, value -> value
  | ( ( Border | Border_block | Border_block_start | Border_block_end
      | Border_inline | Border_inline_start | Border_inline_end | Column_rule
      | Border_top | Border_right | Border_bottom | Border_left ),
      value ) ->
      value
  | Background, value -> value
  | Tab_size, value -> value
  | Zoom, value -> value
  | (Webkit_text_size_adjust | Text_size_adjust), value -> value
  | Font_feature_settings, value -> value
  | Font_variation_settings, value -> value
  | Text_indent, value -> value
  | List_style, value -> value
  | Font, value -> value
  | Source, value -> value
  | Webkit_appearance, value -> value
  | (Webkit_transition | Moz_transition | O_transition | Transition), value ->
      value
  | ( ( Webkit_transition_delay | Webkit_transition_duration
      | Webkit_animation_delay | Webkit_animation_duration | Moz_animation_delay
      | Moz_animation_duration | Moz_transition_delay | Moz_transition_duration
      | Transition_duration | Transition_delay | Animation_duration
      | Animation_delay ),
      value ) ->
      value
  | ( ( Webkit_transition_property | Moz_transition_property
      | Transition_property ),
      value ) ->
      value
  | ( ( Webkit_transition_timing_function | Webkit_animation_timing_function
      | Moz_animation_timing_function | Moz_transition_timing_function
      | Transition_timing_function | Animation_timing_function ),
      value ) ->
      value
  | (Webkit_animation | Moz_animation | Animation), value -> value
  | ( ( Webkit_animation_direction | Moz_animation_direction
      | Animation_direction ),
      value ) ->
      value
  | ( ( Webkit_animation_iteration_count | Moz_animation_iteration_count
      | Animation_iteration_count ),
      value ) ->
      value
  | (Webkit_animation_name | Moz_animation_name | Animation_name), value ->
      value
  | ( ( Webkit_animation_fill_mode | Moz_animation_fill_mode
      | Animation_fill_mode ),
      value ) ->
      value
  | ( ( Webkit_animation_play_state | Moz_animation_play_state
      | Animation_play_state ),
      value ) ->
      value
  | (Webkit_box_sizing | Moz_box_sizing | Box_sizing), value -> value
  | (Webkit_box_shadow | Moz_box_shadow | Box_shadow), value -> value
  | ( (Webkit_background_size | Background_size | Webkit_mask_size | Mask_size),
      value ) ->
      value
  | ( ( Webkit_filter | Ms_filter | Filter | Backdrop_filter
      | Webkit_backdrop_filter ),
      value ) ->
      value
  | (Moz_appearance | Appearance), value -> value
  | Container_type, value -> value
  | Container_name, value -> value
  | Container, value -> value
  | Anchor_name, value -> value
  | Position_anchor, value -> value
  | Position_try_fallbacks, value -> value
  | Position_try_order, value -> value
  | Position_try, value -> value
  | Position_visibility, value -> value
  | Position_area, value -> value
  | Shape_outside, value -> value
  | Shape_image_threshold, value -> value
  | Overflow_clip_margin, value -> value
  | Overflow_anchor, value -> value
  | Scrollbar_width, value -> value
  | Scrollbar_color, value -> value
  | Scrollbar_gutter, value -> value
  | Font_palette, value -> value
  | Font_synthesis, value -> value
  | Text_wrap_mode, value -> value
  | Text_wrap_style, value -> value
  | Text_box_trim, value -> value
  | Text_underline_position, value -> value
  | Text_box_edge, value -> value
  | Text_box, value -> value
  | Inline_sizing, value -> value
  | Line_fit_edge, value -> value
  | Interpolate_size, value -> value
  | Min_intrinsic_sizing, value -> value
  | Ruby_align, value -> value
  | Ruby_merge, value -> value
  | Ruby_overhang, value -> value
  | Ruby_position, value -> value
  | Glyph_orientation_vertical, value -> value
  | Text_combine_upright, value -> value
  | Animation_timeline, value -> value
  | Animation_range, value -> value
  | (Animation_range_start | Animation_range_end), value -> value
  | Scroll_timeline, value -> value
  | View_timeline, value -> value
  | (Scroll_timeline_name | View_timeline_name | Timeline_scope), value -> value
  | (Scroll_timeline_axis | View_timeline_axis), value -> value
  | View_transition_name, value -> value
  | View_transition_class, value -> value
  | Image_orientation, value -> value
  | Image_rendering, value -> value
  | Image_resolution, value -> value
  | Contain_intrinsic_size, value -> value
  | ( ( Contain_intrinsic_width | Contain_intrinsic_height
      | Contain_intrinsic_block_size | Contain_intrinsic_inline_size ),
      value ) ->
      value
  | Margin_trim, value -> value
  | Offset_path, value -> value
  | Offset, value -> value
  | Offset_anchor, value -> value
  | Offset_position, value -> value
  | Offset_rotate, value -> value
  | Font_size_adjust, value -> value
  | Font_variant_emoji, value -> value
  | Text_spacing_trim, value -> value
  | Hyphenate_limit_chars, value -> value
  | Initial_letter, value -> value
  | Initial_letter_align, value -> value
  | Initial_letter_wrap, value -> value
  | Dominant_baseline, value -> value
  | View_timeline_inset, value -> value
  | Perspective_origin, value -> value
  | Transform_style, value -> value
  | Backface_visibility, value -> value
  | Object_position, value -> value
  | Rotate, value -> value
  | Transition_behavior, value -> value
  | Overlay, value -> value
  | Will_change, value -> value
  | Contain, value -> value
  | Isolation, value -> value
  | (Break_before | Break_after), value -> value
  | Break_inside, value -> value
  | (Page_break_before | Page_break_after), value -> value
  | Page_break_inside, value -> value
  | Page_size, value -> value
  | Columns, value -> value
  | Column_width, value -> value
  | Column_height, value -> value
  | Column_wrap, value -> value
  | Column_count, value -> value
  | Column_span, value -> value
  | Background_attachment, value -> value
  | Transform_origin, value -> value
  | Transform_box, value -> value
  | Text_shadow, value -> value
  | Clip_path, value -> value
  | Mask, value -> value
  | Content_visibility, value -> value
  | Background_image, value -> value
  | (Background_origin | Background_clip | Webkit_background_clip), value ->
      value
  | Aspect_ratio, value -> value
  | Vertical_align, value -> value
  | Font_family, value -> value
  | (Background_position | Webkit_mask_position | Mask_position), value -> value
  | (Background_position_x | Background_position_y), value -> value
  | (Webkit_mask_position_x | Webkit_mask_position_y), value -> value
  | (Background_repeat | Webkit_mask_repeat | Mask_repeat), value -> value
  | Webkit_font_smoothing, value -> value
  | Moz_osx_font_smoothing, value -> value
  | Webkit_line_clamp, value -> value
  | Webkit_box_orient, value -> value
  | Moz_orient, value -> value
  | Text_overflow, value -> value
  | Text_wrap, value -> value
  | Word_break, value -> value
  | Overflow_wrap, value -> value
  | Line_break, value -> value
  | (Hyphens | Webkit_hyphens), value -> value
  | Font_stretch, value -> value
  | Font_optical_sizing, value -> value
  | Font_kerning, value -> value
  | Font_language_override, value -> value
  | Font_synthesis_style, value -> value
  | Font_synthesis_weight, value -> value
  | Font_synthesis_small_caps, value -> value
  | Font_synthesis_position, value -> value
  | Font_variant_ligatures, value -> value
  | Caps, value -> value
  | Numeric, value -> value
  | Font_variant_position, value -> value
  | East_asian, value -> value
  | Webkit_mask_composite, value -> value
  | Webkit_mask_source_type, value -> value
  | (Webkit_mask_clip | Webkit_mask_origin | Mask_clip | Mask_origin), value ->
      value
  | Mask_composite, value -> value
  | Mask_mode, value -> value
  | Mask_type, value -> value
  | Scroll_snap_align, value -> value
  | Scroll_snap_stop, value -> value
  | Scroll_behavior, value -> value
  | Field_sizing, value -> value
  | Caption_side, value -> value
  | Resize, value -> value
  | Object_fit, value -> value
  | Object_view_box, value -> value
  | Color_scheme, value -> value
  | (Print_color_adjust | Webkit_print_color_adjust), value -> value
  | (Box_decoration_break | Webkit_box_decoration_break), value -> value
  | Content, value -> value
  | (Counter_reset | Counter_increment), value -> value
  | Quotes, value -> value
  | Touch_action, value -> value
  | Clip, value -> value
  | Clear, value -> value
  | Float, value -> value
  | Scale, value -> value
  | (Fill | Stroke), value -> value
  | Stroke_width, value -> value
  | (Fill_rule | Clip_rule), value -> value
  | Stroke_linecap, value -> value
  | Stroke_linejoin, value -> value
  | Stroke_miterlimit, value -> value
  | Stroke_dashoffset, value -> value
  | Stroke_dasharray, value -> value
  | Paint_order, value -> value
  | Vector_effect, value -> value
  | Direction, value -> value
  | Unicode_bidi, value -> value
  | Writing_mode, value -> value
  | Text_decoration_skip_ink, value -> value
  | Animation_composition, value -> value
  | Background_blend_mode, value -> value
  | Overscroll_behavior, value -> value
  | ( ( Overscroll_behavior_x | Overscroll_behavior_y
      | Overscroll_behavior_block | Overscroll_behavior_inline ),
      value ) ->
      value

(* CSS Values 4 (ED) sec. 10.8 ("Syntax"): inside a math function whitespace is
   *required* around binary [+] and [-] (sign-token disambiguation - stripping
   it changes [100% - var(--a)] to [100%-var(--a)], where [-var] is one
   ident-like function token) but *optional* around [*], [/], [(], [)], [,].
   Strip the optional whitespace from math-function arguments so two
   custom-property token streams that differ only there have the same canonical
   AST. Typed math is already minified by [pp_calc]; this matters for opaque
   [Tokens _] custom-property values where cascade preserves the author's
   whitespace verbatim by design. Nested non-math functions ([var()] etc.) get a
   recursive component walk but no whitespace stripping; nested math functions
   get their own. *)
let strip_math_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_pm =
          match acc with
          | [] -> false
          | p :: _ -> Parser.is_plus_or_minus_delim p
        in
        let next_pm =
          match rest with
          | [] -> false
          | n :: _ -> Parser.is_plus_or_minus_delim n
        in
        if prev_pm || next_pm then aux (ws :: acc) rest else aux acc rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

let is_mul_or_div_delim = function
  | Component.Preserved { kind = Token.Delim ("*" | "/"); _ } -> true
  | _ -> false

(* Outside a math function only the whitespace adjacent to a [*] or [/] delim is
   insignificant (CSS Values 4 sec. 10.8): [16 / 9] and [16/9] re-tokenise
   identically wherever the stream is substituted. Every other separator stays
   (a whitespace token between two values is part of the stream). *)
let strip_mul_div_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_md =
          match acc with [] -> false | p :: _ -> is_mul_or_div_delim p
        in
        let next_md =
          match rest with [] -> false | n :: _ -> is_mul_or_div_delim n
        in
        if prev_md || next_md then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* CSS Values 4 sec. 2.5: [var()] / [env()] / [attr()] substitute a token stream
   textually, so the whitespace next to one is significant - dropping it lets
   the substituted values merge ([var(--a) var(--b)] could become [1px2px]).
   Every other function and every block closes with a hard token boundary ([)],
   []], [}]) that no neighbour can merge across. *)
let is_substitution_func_name name =
  match String.lowercase_ascii name with
  | "var" | "env" | "attr" -> true
  | _ -> false

(* Whitespace immediately after a function or block that closes with a hard
   token boundary is insignificant: [drop-shadow(a) drop-shadow(b)] and
   [calc(45deg*-1) in oklab] re-tokenise identically without it, wherever the
   stream is substituted. Whitespace after a substitution function stays, and so
   does the whitespace before a math operator, which sec. 10.8 requires however
   hard the boundary on its left: [calc(min(1px,2px) - 3px)] keeps both spaces
   or the browser drops the declaration. *)
let strip_after_close_paren ~in_math comps =
  let closes_hard = function
    | Component.Func wrapped ->
        not (is_substitution_func_name wrapped.Component.node.name)
    | Component.Block _ -> true
    | _ -> false
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_hard =
          match acc with [] -> false | p :: _ -> closes_hard p
        in
        let next_pm =
          match rest with
          | [] -> false
          | n :: _ -> in_math && Parser.is_plus_or_minus_delim n
        in
        if prev_hard && not next_pm then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* [in_math] tracks whether the current component list is inside a math
   function's grammar. It enters at the args of a [calc()] / [min()] / ... call,
   propagates through grouping paren [Block]s since those are math operands, and
   turns off in square or curly blocks and nested non-math functions like
   [var()], which have their own grammars. *)
let rec canonicalize_math_whitespace ~in_math comps =
  let comps' =
    List.map
      (fun c ->
        match c with
        | Component.Func wrapped ->
            let func = wrapped.Component.node in
            let nested_in_math = is_math_function func.name in
            let args =
              canonicalize_math_whitespace ~in_math:nested_in_math
                func.arguments
            in
            Component.Func
              { wrapped with node = { func with arguments = args } }
        | Component.Block wrapped ->
            let block = wrapped.Component.node in
            let nested_in_math =
              match block.opening with
              | Token.Paren -> in_math
              | Token.Square | Token.Curly -> false
            in
            let value =
              canonicalize_math_whitespace ~in_math:nested_in_math block.value
            in
            Component.Block { wrapped with node = { block with value } }
        | Component.Preserved _ -> c)
      comps
  in
  let comps' =
    if in_math then strip_math_whitespace comps'
    else strip_mul_div_whitespace comps'
  in
  strip_after_close_paren ~in_math comps'

let canonicalize_math_whitespace_components comps =
  canonicalize_math_whitespace ~in_math:false comps

let normalize_property_value : type a.
    ?lossless:bool ->
    ?exact_srgb:bool ->
    ?resolve_missing:bool ->
    ?ctx:Values.calc_ctx ->
    a property ->
    a ->
    a =
 fun ?(lossless = false) ?(exact_srgb = false) ?(resolve_missing = false)
     ?(ctx = Values.default_calc_ctx) property value ->
  let normalize_color =
    Values.normalize_color ~lossless ~exact_srgb ~resolve_missing
  in
  (* [initial] -> shortest spec-equivalent (e.g. min-width:initial -> auto) is a
     semantic rewrite, so it belongs here, not in pp. *)
  let value = canonical_initial_for_minify property value in
  match property with
  | Transform -> map_preserve normalize_transform value
  | Webkit_transform -> map_preserve normalize_transform value
  | Webkit_border_radius -> normalize_border_radius value
  | Moz_border_radius -> normalize_border_radius value
  | Webkit_box_shadow -> normalize_shadow ~lossless value
  | Moz_box_shadow -> normalize_shadow ~lossless value
  | Rotate -> normalize_rotate value
  | Scale -> normalize_scale value
  | Translate -> normalize_translate_value value
  | Transform_origin -> normalize_transform_origin value
  | Offset_path -> normalize_offset_path value
  | Offset -> normalize_offset ~ctx value
  | Offset_anchor -> normalize_offset_anchor value
  | Offset_position -> normalize_offset_position value
  | Offset_rotate -> normalize_offset_rotate value
  | Font_style -> normalize_font_style value
  | Width -> Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Height -> Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_width ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_height ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_inline_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Min_block_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_width ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_height ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Inline_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_inline_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Block_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Max_block_size ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Shape_margin ->
      Values.normalize_length_percentage ~non_negative:true ~ctx value
  | Offset_distance -> Values.normalize_length_percentage ~ctx value
  | Border_radius -> normalize_border_radius value
  | Background_image ->
      map_preserve (normalize_background_image ~lossless) value
  | Mask_image -> normalize_background_image ~lossless value
  | Webkit_mask_image -> normalize_background_image ~lossless value
  | Border_image_source -> normalize_background_image ~lossless value
  | Background -> map_preserve (normalize_background ~lossless) value
  | Background_repeat -> normalize_background_repeat value
  | Mask_border -> normalize_mask_border value
  | Border_spacing -> normalize_border_spacing value
  | Mask -> normalize_mask ~lossless value
  | Clip_path -> normalize_clip_path value
  | Object_view_box -> normalize_object_view_box value
  | Object_position -> normalize_position_value value
  | Perspective_origin -> normalize_position_value value
  | Background_position -> map_preserve normalize_position_value value
  | Background_position_x -> normalize_background_position_axis value
  | Background_position_y -> normalize_background_position_axis value
  | Webkit_mask_position_x -> normalize_background_position_axis value
  | Webkit_mask_position_y -> normalize_background_position_axis value
  | Mask_position -> map_preserve normalize_position_value value
  | Webkit_mask_position -> map_preserve normalize_position_value value
  | Text_indent -> normalize_text_indent value
  | Animation_range -> normalize_animation_range value
  | Animation_iteration_count -> normalize_animation_iteration_count ~ctx value
  | Webkit_animation_iteration_count ->
      normalize_animation_iteration_count ~ctx value
  | Moz_animation_iteration_count ->
      normalize_animation_iteration_count ~ctx value
  | Scroll_timeline -> normalize_timeline_shorthand value
  | View_timeline -> normalize_view_timeline_shorthand value
  | View_timeline_inset -> normalize_timeline_inset value
  | Baseline_shift -> normalize_baseline_shift value
  | Background_color -> normalize_color value
  | Color -> normalize_color value
  | Border_color ->
      normalize_box_shorthand ~is_substitution:is_color_substitution
        normalize_color value
  | Border_style ->
      normalize_box_shorthand ~is_substitution:is_border_style_substitution
        Fun.id value
  | Border_top_color -> normalize_color value
  | Border_right_color -> normalize_color value
  | Border_bottom_color -> normalize_color value
  | Border_left_color -> normalize_color value
  | Border_inline_start_color -> normalize_color value
  | Border_inline_end_color -> normalize_color value
  | Border_block_start_color -> normalize_color value
  | Border_block_end_color -> normalize_color value
  | Border_inline_color -> normalize_logical_border_color ~lossless value
  | Border_block_color -> normalize_logical_border_color ~lossless value
  | Text_decoration_color -> normalize_color value
  | Webkit_text_decoration_color -> normalize_color value
  | Webkit_text_fill_color -> normalize_color value
  | Webkit_text_stroke_color -> normalize_color value
  | Webkit_text_stroke_width -> normalize_border_width value
  | Column_rule_color -> List.map normalize_color value
  | Column_rule_width -> List.map normalize_border_width value
  | Webkit_tap_highlight_color -> normalize_color value
  | Text_emphasis_color -> normalize_color value
  | Outline_color -> normalize_color value
  | Accent_color -> normalize_color value
  | Caret_color -> normalize_color value
  | Stop_color -> normalize_color value
  | Flood_color -> normalize_color value
  | Lighting_color -> normalize_color value
  | Border -> normalize_border ~lossless value
  | Border_block -> normalize_border ~lossless value
  | Border_block_start -> normalize_border ~lossless value
  | Border_block_end -> normalize_border ~lossless value
  | Border_inline -> normalize_border ~lossless value
  | Border_inline_start -> normalize_border ~lossless value
  | Border_inline_end -> normalize_border ~lossless value
  | Border_top -> normalize_border ~lossless value
  | Border_right -> normalize_border ~lossless value
  | Border_bottom -> normalize_border ~lossless value
  | Border_left -> normalize_border ~lossless value
  | Column_rule -> normalize_border ~lossless value
  | Outline -> normalize_outline ~lossless value
  | Box_shadow -> normalize_shadow ~lossless value
  | Text_shadow -> map_preserve (normalize_text_shadow ~lossless) value
  | Text_decoration -> normalize_text_decoration ~lossless value
  | Webkit_text_decoration -> normalize_text_decoration ~lossless value
  | Text_emphasis -> normalize_text_emphasis ~lossless value
  | Flex_flow -> normalize_flex_flow value
  | Caret -> normalize_caret ~lossless value
  | Interest_delay -> normalize_interest_delay value
  | Interest_delay_start -> normalize_interest_delay value
  | Interest_delay_end -> normalize_interest_delay value
  | Fill -> normalize_svg_paint ~lossless value
  | Stroke -> normalize_svg_paint ~lossless value
  | Scrollbar_color -> normalize_scrollbar_color ~lossless value
  | Filter -> normalize_filter ~lossless value
  | Webkit_filter -> normalize_filter ~lossless value
  | Ms_filter -> normalize_filter ~lossless value
  | Backdrop_filter -> normalize_filter ~lossless value
  | Webkit_backdrop_filter -> normalize_filter ~lossless value
  | Flex_grow -> normalize_flex_factor value
  | Stroke_miterlimit -> normalize_stroke_miterlimit value
  | Stroke_dashoffset -> normalize_stroke_dashoffset ~ctx value
  | Stroke_dasharray -> normalize_stroke_dasharray ~ctx value
  | Paint_order -> normalize_paint_order value
  | Vector_effect -> normalize_vector_effect value
  | Flex_shrink -> normalize_flex_factor value
  | Flex_basis -> normalize_flex_basis value
  | Flex -> normalize_flex value
  | Grid_template_columns -> normalize_grid_template value
  | Grid_template_rows -> normalize_grid_template value
  | Grid_template -> normalize_grid_template value
  | Grid -> normalize_grid_template value
  | Grid_auto_columns -> normalize_grid_template value
  | Grid_auto_rows -> normalize_grid_template value
  | Grid_auto_flow -> normalize_grid_auto_flow value
  | Aspect_ratio -> normalize_aspect_ratio value
  | Gap -> normalize_gap value
  | Font_size -> normalize_font_size value
  | Font_weight -> normalize_font_weight value
  | Font_family -> normalize_font_family value
  | Font_stretch -> normalize_font_stretch value
  | Font -> normalize_font value
  | Display -> normalize_display value
  | Overflow -> normalize_overflow value
  | Transition -> map_preserve normalize_transition value
  | Webkit_transition -> map_preserve normalize_transition value
  | Moz_transition -> map_preserve normalize_transition value
  | O_transition -> map_preserve normalize_transition value
  | List_style -> normalize_list_style value
  | Transition_timing_function -> normalize_timing_function value
  | Animation -> map_preserve normalize_animation value
  | Webkit_animation -> map_preserve normalize_animation value
  | Moz_animation -> map_preserve normalize_animation value
  | Animation_timing_function -> normalize_timing_function value
  | Padding_left -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_right -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_bottom -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_top -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_inline_start ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Padding_inline_end -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_block_start -> Values.normalize_length ~non_negative:true ~ctx value
  | Padding_block_end -> Values.normalize_length ~non_negative:true ~ctx value
  | Margin_inline_end -> Values.normalize_length ~ctx value
  | Margin_inline_start -> Values.normalize_length ~ctx value
  | Margin_left -> Values.normalize_length ~ctx value
  | Margin_right -> Values.normalize_length ~ctx value
  | Margin_top -> Values.normalize_length ~ctx value
  | Margin_bottom -> Values.normalize_length ~ctx value
  | Margin_block_start -> Values.normalize_length ~ctx value
  | Margin_block_end -> Values.normalize_length ~ctx value
  | Column_gap -> Values.normalize_length ~non_negative:true ~ctx value
  | Row_gap -> Values.normalize_length ~non_negative:true ~ctx value
  | Text_underline_offset -> Values.normalize_length ~ctx value
  | Letter_spacing -> Values.normalize_length ~ctx value
  | Border_top_left_radius -> normalize_length_box ~non_negative:true ~ctx value
  | Border_top_right_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_bottom_left_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_bottom_right_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_start_start_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_start_end_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_end_start_radius ->
      normalize_length_box ~non_negative:true ~ctx value
  | Border_end_end_radius -> normalize_length_box ~non_negative:true ~ctx value
  | Outline_width -> normalize_border_width value
  | Outline_offset -> Values.normalize_length ~ctx value
  | Line_height_step -> Values.normalize_length ~non_negative:true ~ctx value
  | Perspective -> Values.normalize_length ~non_negative:true ~ctx value
  | Word_spacing -> Values.normalize_length ~ctx value
  | Text_decoration_thickness -> Values.normalize_length ~ctx value
  | Stroke_width -> normalize_stroke_width ~ctx value
  | Scroll_margin_top -> Values.normalize_length ~ctx value
  | Scroll_margin_right -> Values.normalize_length ~ctx value
  | Scroll_margin_bottom -> Values.normalize_length ~ctx value
  | Scroll_margin_left -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_start -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_end -> Values.normalize_length ~ctx value
  | Scroll_margin_block_start -> Values.normalize_length ~ctx value
  | Scroll_margin_block_end -> Values.normalize_length ~ctx value
  | Scroll_padding_top -> Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_right ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_bottom ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_left -> Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_inline_start ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_inline_end ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_block_start ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Scroll_padding_block_end ->
      Values.normalize_length ~non_negative:true ~ctx value
  | Padding -> normalize_length_box ~non_negative:true ~ctx value
  | Padding_inline -> normalize_length_box ~non_negative:true ~ctx value
  | Padding_block -> normalize_length_box ~non_negative:true ~ctx value
  | Margin -> normalize_length_box ~ctx value
  | Margin_inline -> normalize_length_box ~ctx value
  | Margin_block -> normalize_length_box ~ctx value
  | Inset -> normalize_length_box ~ctx value
  | Inset_inline -> normalize_length_box ~ctx value
  | Inset_inline_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline_end -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block -> normalize_length_box ~ctx value
  | Inset_block_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block_end -> map_preserve (Values.normalize_length ~ctx) value
  | Top -> map_preserve (Values.normalize_length ~ctx) value
  | Right -> map_preserve (Values.normalize_length ~ctx) value
  | Bottom -> map_preserve (Values.normalize_length ~ctx) value
  | Left -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin -> normalize_length_box ~ctx value
  | Scroll_margin_inline -> normalize_length_box ~ctx value
  | Scroll_margin_block -> normalize_length_box ~ctx value
  | Scroll_padding -> normalize_length_box ~ctx value
  | Scroll_padding_inline -> normalize_length_box ~ctx value
  | Scroll_padding_block -> normalize_length_box ~ctx value
  | Custom_property _ -> (
      match value with
      | Custom_value ({ value = Tokens components; _ } as r) ->
          let components' =
            components
            |> canonicalize_custom_shadow_components ~lossless
            |> canonicalize_custom_colors_components ~lossless
            |> canonicalize_math_whitespace_components
          in
          if components' == components then value
          else Custom_value { r with value = Tokens components' }
      | Custom_value _ -> value)
  | Opacity -> normalize_opacity value
  | Fill_opacity -> normalize_opacity value
  | Stroke_opacity -> normalize_opacity value
  | Stop_opacity -> normalize_opacity value
  | Flood_opacity -> normalize_opacity value
  | Line_height -> normalize_line_height ~lossless value
  | Vertical_align -> normalize_vertical_align value
  | Border_image -> normalize_border_image value
  | Columns -> normalize_columns_value value
  | Border_width ->
      normalize_box_shorthand ~is_substitution:is_border_width_substitution
        normalize_border_width value
  | Border_top_width -> normalize_border_width value
  | Border_right_width -> normalize_border_width value
  | Border_bottom_width -> normalize_border_width value
  | Border_left_width -> normalize_border_width value
  | Border_inline_start_width -> normalize_border_width value
  | Border_inline_end_width -> normalize_border_width value
  | Border_block_start_width -> normalize_border_width value
  | Border_block_end_width -> normalize_border_width value
  | Border_inline_width -> normalize_logical_border_width value
  | Border_block_width -> normalize_logical_border_width value
  | Border_inline_style -> normalize_logical_border_style value
  | Border_block_style -> normalize_logical_border_style value
  | Transition_duration -> Values.normalize_duration ~ctx value
  | Transition_delay -> Values.normalize_duration ~ctx value
  | Animation_duration -> Values.normalize_duration ~ctx value
  | Animation_delay -> Values.normalize_duration ~ctx value
  | Webkit_transition_duration -> Values.normalize_duration ~ctx value
  | Webkit_transition_delay -> Values.normalize_duration ~ctx value
  | Webkit_animation_duration -> Values.normalize_duration ~ctx value
  | Webkit_animation_delay -> Values.normalize_duration ~ctx value
  | Moz_transition_duration -> Values.normalize_duration ~ctx value
  | Moz_transition_delay -> Values.normalize_duration ~ctx value
  | Moz_animation_duration -> Values.normalize_duration ~ctx value
  | Moz_animation_delay -> Values.normalize_duration ~ctx value
  | _ -> value

let normalize_custom_property_value ?(lossless = false)
    ?(ctx = Values.default_calc_ctx) :
    custom_property_value -> custom_property_value = function
  | Typed { kind = Length; value } ->
      Typed { kind = Length; value = Values.normalize_length ~ctx value }
  | Typed { kind = Color; value } ->
      Typed { kind = Color; value = Values.normalize_color ~lossless value }
  | Typed { kind = Number; value } ->
      Typed { kind = Number; value = Values.normalize_number ~ctx value }
  | Typed { kind = Percentage; value } ->
      Typed
        { kind = Percentage; value = Values.normalize_percentage ~ctx value }
  | Typed { kind = Length_percentage; value } ->
      Typed
        {
          kind = Length_percentage;
          value = Values.normalize_length_percentage ~ctx value;
        }
  | Typed { kind = Angle; value } ->
      Typed { kind = Angle; value = Values.normalize_angle ~ctx value }
  | Typed { kind = Duration; value } ->
      Typed { kind = Duration; value = Values.normalize_duration ~ctx value }
  | Typed { kind = Gradient_direction; value } ->
      Typed
        {
          kind = Gradient_direction;
          value = normalize_gradient_direction value;
        }
  | Tokens components ->
      Tokens
        (canonicalize_math_whitespace_components
           (canonicalize_custom_colors_components ~lossless
              (canonicalize_custom_shadow_components ~lossless components)))
  | Typed _ as other -> other

let pp_property_value : type a. (a property * a) Pp.t =
 fun ctx (prop, value) ->
  let pp pp_a = pp_a ctx value in
  match prop with
  | Custom_property _ -> pp pp_custom_property
  | Unknown_property _ ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified_numbers value
        else Parser.string_of_components value
      in
      Pp.string ctx rendered
  | All -> pp pp_css_wide
  | Background_color -> pp pp_color
  | Color -> pp pp_color
  | Border_color -> pp (Pp.list ~sep:Pp.token_sp pp_color)
  | Border_style -> pp (pp_box_shorthand pp_border_style)
  | Border_top_style -> pp pp_border_style
  | Border_right_style -> pp pp_border_style
  | Border_bottom_style -> pp pp_border_style
  | Border_left_style -> pp pp_border_style
  | Border_inline_start_style -> pp pp_border_style
  | Border_inline_end_style -> pp pp_border_style
  | Border_block_start_style -> pp pp_border_style
  | Border_block_end_style -> pp pp_border_style
  | Padding -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Padding_left -> pp pp_length
  | Padding_right -> pp pp_length
  | Padding_bottom -> pp pp_length
  | Padding_top -> pp pp_length
  | Padding_inline -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Padding_inline_start -> pp pp_length
  | Padding_inline_end -> pp pp_length
  | Padding_block -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Padding_block_start -> pp pp_length
  | Padding_block_end -> pp pp_length
  | Margin -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Margin_inline_end -> pp pp_length
  | Margin_inline_start -> pp pp_length
  | Margin_left -> pp pp_length
  | Margin_right -> pp pp_length
  | Margin_top -> pp pp_length
  | Margin_bottom -> pp pp_length
  | Margin_inline -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Margin_block -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Margin_block_start -> pp pp_length
  | Margin_block_end -> pp pp_length
  | Gap -> pp pp_gap
  | Column_gap -> pp pp_length
  | Row_gap -> pp pp_length
  | Width -> pp pp_length_percentage
  | Height -> pp pp_length_percentage
  | Min_width -> pp pp_length_percentage
  | Min_height -> pp pp_length_percentage
  | Max_width -> pp pp_length_percentage
  | Max_height -> pp pp_length_percentage
  | Inline_size -> pp pp_length_percentage
  | Min_inline_size -> pp pp_length_percentage
  | Max_inline_size -> pp pp_length_percentage
  | Block_size -> pp pp_length_percentage
  | Min_block_size -> pp pp_length_percentage
  | Max_block_size -> pp pp_length_percentage
  | Font_size -> pp pp_font_size
  | Line_height -> pp pp_line_height
  | Font_weight -> pp pp_font_weight
  | Display -> pp pp_display
  | Position -> pp pp_position
  | Visibility -> pp pp_visibility
  | Baseline_source -> pp pp_baseline_source
  | Alignment_baseline -> pp pp_alignment_baseline
  | Baseline_shift -> pp pp_baseline_shift
  | Align_items -> pp pp_align_items
  | Justify_content -> pp pp_justify_content
  | Justify_items -> pp pp_justify_items
  | Align_self -> pp pp_align_self
  | Border_collapse -> pp pp_border_collapse
  | Table_layout -> pp pp_table_layout
  | Grid_auto_flow -> pp pp_grid_auto_flow
  | Opacity -> pp pp_opacity
  | Fill_opacity -> pp pp_opacity
  | Stroke_opacity -> pp pp_opacity
  | Stop_opacity -> pp pp_opacity
  | Flood_opacity -> pp pp_opacity
  | Mix_blend_mode -> pp pp_blend_mode
  | Z_index -> pp pp_z_index
  | Tab_size -> pp pp_tab_size
  | Zoom -> pp pp_zoom
  | Webkit_line_clamp -> pp pp_webkit_line_clamp
  | Webkit_box_orient -> pp pp_webkit_box_orient
  | Inset -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Inset_inline -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Inset_inline_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_inline_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block -> pp (Pp.list ~sep:Pp.token_sp pp_length)
  | Inset_block_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Top -> pp (Pp.list ~sep:Pp.space pp_length)
  | Right -> pp (Pp.list ~sep:Pp.space pp_length)
  | Bottom -> pp (Pp.list ~sep:Pp.space pp_length)
  | Left -> pp (Pp.list ~sep:Pp.space pp_length)
  | Border_width -> pp (Pp.list ~sep:Pp.space pp_border_width)
  | Border_top_width -> pp pp_border_width
  | Border_right_width -> pp pp_border_width
  | Border_bottom_width -> pp pp_border_width
  | Border_left_width -> pp pp_border_width
  | Border_inline_start_width -> pp pp_border_width
  | Border_inline_end_width -> pp pp_border_width
  | Border_block_start_width -> pp pp_border_width
  | Border_block_end_width -> pp pp_border_width
  | Border_image -> pp pp_border_image
  | Border_radius -> pp pp_border_radius
  | Border_top_left_radius -> pp (pp_box_shorthand pp_length)
  | Border_top_right_radius -> pp (pp_box_shorthand pp_length)
  | Border_bottom_left_radius -> pp (pp_box_shorthand pp_length)
  | Border_bottom_right_radius -> pp (pp_box_shorthand pp_length)
  | Border_top_color -> pp pp_color
  | Border_right_color -> pp pp_color
  | Border_bottom_color -> pp pp_color
  | Border_left_color -> pp pp_color
  | Border_inline_start_color -> pp pp_color
  | Border_inline_end_color -> pp pp_color
  | Border_block_start_color -> pp pp_color
  | Border_block_end_color -> pp pp_color
  | Border_inline_color -> pp pp_logical_border_color
  | Border_block_color -> pp pp_logical_border_color
  | Border_inline_width -> pp pp_logical_border_width
  | Border_block_width -> pp pp_logical_border_width
  | Border_inline_style -> pp pp_logical_border_style
  | Border_block_style -> pp pp_logical_border_style
  | Border_start_start_radius -> pp (pp_box_shorthand pp_length)
  | Border_start_end_radius -> pp (pp_box_shorthand pp_length)
  | Border_end_start_radius -> pp (pp_box_shorthand pp_length)
  | Border_end_end_radius -> pp (pp_box_shorthand pp_length)
  | Text_decoration_color -> pp pp_color
  | Webkit_text_decoration_color -> pp pp_color
  | Webkit_tap_highlight_color -> pp pp_color
  | Webkit_text_fill_color -> pp pp_color
  | Webkit_text_stroke_color -> pp pp_color
  | Webkit_text_stroke -> pp pp_webkit_text_stroke
  | Webkit_text_stroke_width -> pp pp_border_width
  | Column_rule_color -> pp (Pp.list ~sep:Pp.comma pp_color)
  | Column_rule_width -> pp (Pp.list ~sep:Pp.comma pp_border_width)
  | Column_rule_style -> pp (Pp.list ~sep:Pp.comma pp_border_style)
  | Text_indent -> pp pp_text_indent_value
  | Border_spacing -> pp pp_border_spacing
  | Outline_offset -> pp pp_length
  | Perspective -> pp pp_length
  | Transform -> pp pp_transforms
  | Translate -> pp pp_translate_value
  | Isolation -> pp pp_isolation
  | Break_before -> pp pp_break_value
  | Break_after -> pp pp_break_value
  | Break_inside -> pp pp_break_inside_value
  | Page_break_before -> (
      match if Pp.minified ctx then break_of_page_break value else None with
      | Some aliased -> pp_break_value ctx aliased
      | None -> pp_page_break_value ctx value)
  | Page_break_after -> (
      match if Pp.minified ctx then break_of_page_break value else None with
      | Some aliased -> pp_break_value ctx aliased
      | None -> pp_page_break_value ctx value)
  | Page_break_inside -> (
      match
        if Pp.minified ctx then break_inside_of_page_break value else None
      with
      | Some aliased -> pp_break_inside_value ctx aliased
      | None -> pp_page_break_inside_value ctx value)
  | Page_size -> pp pp_page_size
  | Columns -> pp pp_columns_value
  | Column_width -> pp pp_column_width
  | Column_height -> pp pp_column_height
  | Column_wrap -> pp pp_column_wrap
  | Column_count -> pp pp_column_count
  | Column_rule -> pp pp_border
  | Column_span -> pp pp_column_span
  | Transform_style -> pp pp_transform_style
  | Backface_visibility -> pp pp_backface_visibility
  | Scroll_snap_align -> pp pp_scroll_snap_align
  | Scroll_snap_stop -> pp pp_scroll_snap_stop
  | Scroll_behavior -> pp pp_scroll_behavior
  | Box_sizing -> pp pp_box_sizing
  | Webkit_box_sizing -> pp pp_box_sizing
  | Moz_box_sizing -> pp pp_box_sizing
  | Field_sizing -> pp pp_field_sizing
  | Caption_side -> pp pp_caption_side
  | Resize -> pp pp_resize
  | Object_fit -> pp pp_object_fit
  | Object_view_box -> pp pp_object_view_box
  | Appearance -> pp pp_appearance
  | Color_scheme -> pp pp_color_scheme
  | Print_color_adjust -> pp pp_print_color_adjust
  | Webkit_print_color_adjust -> pp pp_print_color_adjust
  | Box_decoration_break -> pp pp_box_decoration_break
  | Webkit_box_decoration_break -> pp pp_box_decoration_break
  | Flex_grow -> pp pp_flex_factor
  | Flex_shrink -> pp pp_flex_factor
  | Order -> pp pp_order
  | Flex_direction -> pp pp_flex_direction
  | Flex_wrap -> pp pp_flex_wrap
  | Flex_flow -> pp pp_flex_flow
  | Font_style -> pp pp_font_style
  | Text_align -> pp pp_text_align
  | Text_decoration -> pp pp_text_decoration
  | Text_decoration_line -> pp (Pp.list ~sep:Pp.space pp_text_decoration_line)
  | Text_decoration_style -> pp pp_text_decoration_style
  | Text_decoration_skip -> pp pp_text_decoration_skip
  | Text_decoration_skip_self -> pp pp_text_decoration_skip_self
  | Text_decoration_skip_box -> pp pp_text_decoration_skip_box
  | Text_decoration_skip_inset -> pp pp_text_decoration_skip_inset
  | Text_decoration_skip_spaces -> pp pp_text_decoration_skip_spaces
  | Text_emphasis -> pp pp_text_emphasis
  | Text_emphasis_style -> pp pp_text_emphasis_style
  | Text_emphasis_color -> pp pp_color
  | Text_emphasis_position -> pp pp_text_emphasis_position
  | Text_emphasis_skip -> pp pp_text_emphasis_skip
  | Text_orientation -> pp pp_text_orientation
  | Text_transform -> pp pp_text_transform
  | List_style_type -> pp pp_list_style_type
  | List_style_position -> pp pp_list_style_position
  | List_style_image -> pp pp_list_style_image
  | Overflow -> pp pp_overflow
  | Overflow_x -> pp pp_overflow
  | Overflow_y -> pp pp_overflow
  | Overflow_block -> pp pp_overflow
  | Overflow_inline -> pp pp_overflow
  | Vertical_align -> pp pp_vertical_align
  | Text_overflow -> pp pp_text_overflow
  | Text_wrap -> pp pp_text_wrap
  | Word_break -> pp pp_word_break
  | Overflow_wrap -> pp pp_overflow_wrap
  | Line_break -> pp pp_line_break
  | Hyphens -> pp pp_hyphens
  | Webkit_hyphens -> pp pp_hyphens
  | Font_stretch -> pp pp_font_stretch
  | Font_optical_sizing -> pp pp_font_optical_sizing
  | Font_kerning -> pp pp_font_kerning
  | Font_language_override -> pp pp_font_language_override
  | Font_synthesis_style -> pp pp_font_synthesis_style
  | Font_synthesis_weight -> pp pp_font_synthesis_weight
  | Font_synthesis_small_caps -> pp pp_font_synthesis_small_caps
  | Font_synthesis_position -> pp pp_font_synthesis_position
  | Font_variant_ligatures -> pp pp_font_variant_ligatures
  | Caps -> pp pp_font_variant_caps
  | Numeric -> pp pp_font_variant_numeric
  | Font_variant_position -> pp pp_font_variant_position
  | East_asian -> pp pp_font_variant_east_asian
  | Webkit_font_smoothing -> pp pp_webkit_font_smoothing
  | Scroll_snap_type -> pp pp_scroll_snap_type
  | Container_type -> pp pp_container_type
  | Container -> pp pp_container_shorthand
  | White_space -> pp pp_white_space
  | White_space_collapse -> pp pp_white_space_collapse
  | Font_variant_alternates -> pp pp_font_variant_alternates
  | Font_variant -> pp pp_font_variant
  | Grid_template_columns -> pp pp_grid_template
  | Grid_template_rows -> pp pp_grid_template
  | Grid_template_areas -> pp pp_grid_template_areas
  | Grid_template -> pp pp_grid_template
  | Grid -> pp pp_grid_template
  | Grid_area -> pp pp_grid_area
  | Grid_auto_columns -> pp pp_grid_template
  | Grid_auto_rows -> pp pp_grid_template
  | Flex -> pp pp_flex
  | Flex_basis -> pp pp_flex_basis
  | Align_content -> pp pp_align_content
  | Justify_self -> pp pp_justify_self
  | Place_content -> pp pp_place_content
  | Place_items -> pp pp_place_items
  | Place_self ->
      pp (fun ctx (a, j) ->
          pp_align_self ctx a;
          (* Tailwind's minifier quirk: outputs single value for most cases, but
             expands stretch to two values *)
          let needs_second_value =
            match (a, j) with
            | Stretch, Stretch -> false
            | Auto, Auto -> false
            | Normal, Normal -> false
            | Baseline, Baseline -> false
            | First_baseline, First_baseline -> false
            | Last_baseline, Last_baseline -> false
            | Center, Center -> false
            | Start, Start -> false
            | End, End -> false
            | Self_start, Self_start -> false
            | Self_end, Self_end -> false
            | Flex_start, Flex_start -> false
            | Flex_end, Flex_end -> false
            | Safe_center, Safe_center -> false
            | Safe_start, Safe_start -> false
            | Safe_end, Safe_end -> false
            | Safe_flex_start, Safe_flex_start -> false
            | Safe_flex_end, Safe_flex_end -> false
            | Unsafe_center, Unsafe_center -> false
            | Unsafe_start, Unsafe_start -> false
            | Unsafe_end, Unsafe_end -> false
            | Inherit, Inherit -> false
            | Initial, Initial -> false
            | Unset, Unset -> false
            | Revert, Revert -> false
            | Revert_layer, Revert_layer -> false
            | _ -> true (* Different values always need both *)
          in
          if needs_second_value then (
            Pp.space ctx ();
            pp_justify_self ctx j))
  | Grid_column -> pp pp_grid_line_pair
  | Grid_row -> pp pp_grid_line_pair
  | Grid_column_start -> pp pp_grid_line
  | Grid_column_end -> pp pp_grid_line
  | Grid_row_start -> pp pp_grid_line
  | Grid_row_end -> pp pp_grid_line
  | Text_underline_offset -> pp pp_length
  | Background_position -> pp pp_background_position
  | Background_position_x -> pp pp_background_position_axis
  | Background_position_y -> pp pp_background_position_axis
  | Webkit_mask_position_x -> pp pp_background_position_axis
  | Webkit_mask_position_y -> pp pp_background_position_axis
  | Background_repeat -> pp pp_background_repeat
  | Background_size -> pp pp_background_size
  | Moz_osx_font_smoothing -> pp pp_moz_osx_font_smoothing
  | Backdrop_filter -> pp pp_filter
  | Webkit_backdrop_filter -> pp pp_filter
  | Webkit_mask_image -> pp pp_background_image
  | Webkit_mask_composite -> pp pp_webkit_mask_composite
  | Webkit_mask_source_type -> pp pp_webkit_mask_source_type
  | Webkit_mask_size -> pp pp_background_size
  | Webkit_mask_position -> pp pp_background_position
  | Webkit_mask_repeat -> pp pp_background_repeat
  | Webkit_mask_clip -> pp pp_mask_box
  | Webkit_mask_origin -> pp pp_mask_box
  | Border_image_source -> pp pp_background_image
  | Border_image_slice -> pp pp_border_image_slice
  | Border_image_repeat -> pp pp_border_image_repeat
  | Border_image_width -> pp pp_border_image_width
  | Border_image_outset -> pp pp_border_image_outset
  | Mask_image -> pp pp_background_image
  | Mask_composite -> pp pp_mask_composite
  | Mask_mode -> pp pp_mask_mode
  | Mask_size -> pp pp_background_size
  | Mask_position -> pp pp_background_position
  | Mask_repeat -> pp pp_background_repeat
  | Mask_clip -> pp pp_mask_box
  | Mask_origin -> pp pp_mask_box
  | Mask_type -> pp pp_mask_type
  | Mask -> pp pp_mask
  | Container_name -> pp pp_container_name
  | Anchor_name -> pp pp_anchor_name
  | Position_anchor -> pp pp_position_anchor
  | Position_try_fallbacks -> pp pp_position_try_fallbacks
  | Position_try_order -> pp pp_position_try_order
  | Position_try -> pp pp_position_try
  | Position_visibility -> pp pp_position_visibility
  | Position_area -> pp pp_position_area
  | Shape_outside -> pp Pp.string
  | Shape_margin -> pp (pp_length_percentage ~always:true)
  | Shape_image_threshold -> pp pp_shape_image_threshold
  | Overflow_clip_margin -> pp pp_overflow_clip_margin
  | Overflow_anchor -> pp pp_overflow_anchor
  | Scrollbar_width -> pp pp_scrollbar_width
  | Scrollbar_color -> pp pp_scrollbar_color
  | Scrollbar_gutter -> pp pp_scrollbar_gutter
  | Line_height_step -> pp (pp_length ~always:true)
  | Font_palette -> pp pp_font_palette
  | Font_synthesis -> pp pp_font_synthesis
  | Text_wrap_mode -> pp pp_text_wrap_mode
  | Text_wrap_style -> pp pp_text_wrap_style
  | Text_box_trim -> pp pp_text_box_trim
  | Text_underline_position -> pp pp_text_underline_position
  | Text_box_edge -> pp pp_text_box_edge
  | Text_box -> pp pp_text_box
  | Inline_sizing -> pp pp_inline_sizing
  | Line_fit_edge -> pp pp_line_fit_edge
  | Interpolate_size -> pp pp_interpolate_size
  | Min_intrinsic_sizing -> pp pp_min_intrinsic_sizing
  | Ruby_align -> pp pp_ruby_align
  | Ruby_merge -> pp pp_ruby_merge
  | Ruby_overhang -> pp pp_ruby_overhang
  | Ruby_position -> pp pp_ruby_position
  | Glyph_orientation_vertical -> pp pp_glyph_orientation_vertical
  | Animation_timeline -> pp pp_animation_timeline
  | Animation_range -> pp pp_animation_range
  | Animation_range_start -> pp pp_animation_range_item
  | Animation_range_end -> pp pp_animation_range_item
  | Scroll_timeline -> pp pp_timeline_shorthand
  | Scroll_timeline_name -> pp pp_timeline_name
  | Scroll_timeline_axis -> pp pp_timeline_axis
  | View_transition_name -> pp pp_view_transition_name
  | View_transition_class -> pp pp_view_transition_class
  | Image_orientation -> pp pp_image_orientation
  | Image_rendering -> pp pp_image_rendering
  | Image_resolution -> pp pp_image_resolution
  | Contain_intrinsic_size -> pp pp_contain_intrinsic_size
  | Contain_intrinsic_width -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_height -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_block_size -> pp pp_contain_intrinsic_longhand
  | Contain_intrinsic_inline_size -> pp pp_contain_intrinsic_longhand
  | Margin_trim -> pp pp_margin_trim
  | Offset_path -> pp pp_offset_path
  | Offset -> pp pp_offset
  | Offset_anchor -> pp pp_offset_anchor
  | Offset_position -> pp pp_offset_position
  | Offset_distance -> pp (pp_length_percentage ~always:true)
  | Offset_rotate -> pp pp_offset_rotate
  | Font_size_adjust -> pp pp_font_size_adjust
  | Font_variant_emoji -> pp pp_font_variant_emoji
  | Text_spacing_trim -> pp pp_text_spacing_trim
  | Hyphenate_limit_chars -> pp pp_hyphenate_limit_chars
  | Initial_letter -> pp pp_initial_letter
  | Initial_letter_align -> pp pp_initial_letter_align
  | Initial_letter_wrap -> pp pp_initial_letter_wrap
  | Dominant_baseline -> pp pp_dominant_baseline
  | View_timeline_name -> pp pp_timeline_name
  | View_timeline_axis -> pp pp_timeline_axis
  | View_timeline_inset -> pp pp_timeline_inset
  | View_timeline -> pp pp_view_timeline_shorthand
  | Timeline_scope -> pp pp_timeline_name
  | Perspective_origin -> pp pp_perspective_origin
  | Object_position -> pp pp_position_value
  | Rotate -> pp pp_rotate_value
  | Transition_duration -> pp pp_duration
  | Transition_timing_function -> pp pp_timing_function
  | Transition_delay -> pp pp_duration
  | Transition_property -> pp pp_transition_property
  | Transition_behavior -> pp pp_transition_behavior
  | Overlay -> pp pp_overlay
  | Will_change -> pp pp_will_change
  | Contain -> pp pp_contain
  | Word_spacing -> pp pp_length
  | Background_attachment -> pp pp_background_attachment
  | Border_top -> pp pp_border
  | Border_right -> pp pp_border
  | Border_bottom -> pp pp_border
  | Border_left -> pp pp_border
  | Transform_origin -> pp pp_transform_origin
  | Transform_box -> pp pp_transform_box
  | Text_shadow -> pp (Pp.list ~sep:Pp.comma pp_text_shadow)
  | Clip_path -> pp pp_clip_path
  | Mask_border -> pp pp_border_image
  | Content_visibility -> pp pp_content_visibility
  | Filter -> pp pp_filter
  | Background_image -> pp (Pp.list ~sep:Pp.comma pp_background_image)
  | Background_origin -> pp pp_background_box
  | Background_clip -> pp pp_background_box
  | Webkit_background_clip -> pp pp_background_box
  | Animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Aspect_ratio -> pp pp_aspect_ratio
  | Content -> pp pp_content
  | Counter_reset -> pp pp_counter_set
  | Counter_increment -> pp pp_counter_set
  | Quotes -> pp pp_quotes
  | Box_shadow -> pp pp_shadow
  | Fill -> pp pp_svg_paint
  | Stroke -> pp pp_svg_paint
  | Stroke_width -> pp pp_stroke_width
  | Transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Scale -> pp pp_scale
  | Outline -> pp pp_outline
  | Outline_style -> pp pp_outline_style
  | Outline_width -> pp pp_border_width
  | Outline_color -> pp pp_color
  | Forced_color_adjust -> pp pp_forced_color_adjust
  | Clip -> pp pp_clip
  | Clear -> pp pp_clear
  | Float -> pp pp_float_side
  | Border -> pp pp_border
  | Border_block -> pp pp_border
  | Border_block_start -> pp pp_border
  | Border_block_end -> pp pp_border
  | Border_inline -> pp pp_border
  | Border_inline_start -> pp pp_border
  | Border_inline_end -> pp pp_border
  | Background -> pp (Pp.list ~sep:Pp.comma pp_background)
  | Text_decoration_thickness -> pp pp_length
  | Text_size_adjust -> pp pp_text_size_adjust
  | Touch_action -> pp pp_touch_action
  | Direction -> pp pp_direction
  | Fill_rule -> pp pp_fill_rule
  | Clip_rule -> pp pp_fill_rule
  | Stroke_linecap -> pp pp_stroke_linecap
  | Stroke_linejoin -> pp pp_stroke_linejoin
  | Stroke_miterlimit -> pp pp_stroke_miterlimit
  | Stroke_dashoffset -> pp pp_stroke_dashoffset
  | Stroke_dasharray -> pp pp_stroke_dasharray
  | Paint_order -> pp pp_paint_order
  | Vector_effect -> pp pp_vector_effect
  | Unicode_bidi -> pp pp_unicode_bidi
  | Writing_mode -> pp pp_writing_mode
  | Text_combine_upright -> pp pp_text_combine_upright
  | Text_decoration_skip_ink -> pp pp_text_decoration_skip_ink
  | Animation_name -> pp pp_animation_name
  | Animation_duration -> pp pp_duration
  | Animation_timing_function -> pp pp_timing_function
  | Animation_delay -> pp pp_duration
  | Animation_iteration_count -> pp pp_animation_iteration_count
  | Animation_direction -> pp pp_animation_direction
  | Animation_fill_mode -> pp pp_animation_fill_mode
  | Animation_play_state -> pp pp_animation_play_state
  | Animation_composition -> pp pp_animation_composition
  | Background_blend_mode -> pp (Pp.list ~sep:Pp.comma pp_blend_mode)
  | Scroll_margin -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_top -> pp pp_length
  | Scroll_margin_right -> pp pp_length
  | Scroll_margin_bottom -> pp pp_length
  | Scroll_margin_left -> pp pp_length
  | Scroll_margin_inline -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_inline_start -> pp pp_length
  | Scroll_margin_inline_end -> pp pp_length
  | Scroll_margin_block -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_margin_block_start -> pp pp_length
  | Scroll_margin_block_end -> pp pp_length
  | Scroll_padding -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_top -> pp pp_length
  | Scroll_padding_right -> pp pp_length
  | Scroll_padding_bottom -> pp pp_length
  | Scroll_padding_left -> pp pp_length
  | Scroll_padding_inline -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_inline_start -> pp pp_length
  | Scroll_padding_inline_end -> pp pp_length
  | Scroll_padding_block -> pp (Pp.list ~sep:Pp.space (pp_length ~always:true))
  | Scroll_padding_block_start -> pp pp_length
  | Scroll_padding_block_end -> pp pp_length
  | Overscroll_behavior -> pp (Pp.list ~sep:Pp.space pp_overscroll_behavior)
  | Overscroll_behavior_x -> pp pp_overscroll_behavior
  | Overscroll_behavior_y -> pp pp_overscroll_behavior
  | Overscroll_behavior_block -> pp pp_overscroll_behavior
  | Overscroll_behavior_inline -> pp pp_overscroll_behavior
  | Accent_color -> pp pp_color
  | Caret_color -> pp pp_color
  | Stop_color -> pp pp_color
  | Flood_color -> pp pp_color
  | Lighting_color -> pp pp_color
  | List_style -> pp pp_list_style
  | Font -> pp pp_font
  | Source -> pp pp_font_src
  | Webkit_appearance -> pp pp_webkit_appearance
  | Letter_spacing -> pp pp_length
  | Cursor -> pp pp_cursor
  | Interactivity -> pp pp_interactivity
  | Caret_animation -> pp pp_caret_animation
  | Caret_shape -> pp pp_caret_shape
  | Caret -> pp pp_caret
  | Interest_delay -> pp pp_interest_delay
  | Interest_delay_start -> pp pp_interest_delay
  | Interest_delay_end -> pp pp_interest_delay
  | Nav_up -> pp pp_nav
  | Nav_right -> pp pp_nav
  | Nav_down -> pp pp_nav
  | Nav_left -> pp pp_nav
  | Pointer_events -> pp pp_pointer_events
  | User_select -> pp pp_user_select
  | Webkit_user_select -> pp pp_user_select
  | Ms_user_select -> pp pp_user_select
  | Moz_user_select -> pp pp_user_select
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Webkit_text_decoration -> pp pp_text_decoration
  | Webkit_text_size_adjust -> pp pp_text_size_adjust
  | Webkit_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Moz_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Ms_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | O_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Webkit_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Webkit_transition_delay -> pp pp_duration
  | Webkit_transition_duration -> pp pp_duration
  | Webkit_transition_property -> pp pp_transition_property
  | Webkit_transition_timing_function -> pp pp_timing_function
  | Webkit_animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Webkit_animation_delay -> pp pp_duration
  | Webkit_animation_duration -> pp pp_duration
  | Webkit_animation_direction -> pp pp_animation_direction
  | Webkit_animation_iteration_count -> pp pp_animation_iteration_count
  | Webkit_animation_name -> pp pp_animation_name
  | Webkit_animation_timing_function -> pp pp_timing_function
  | Webkit_animation_fill_mode -> pp pp_animation_fill_mode
  | Webkit_animation_play_state -> pp pp_animation_play_state
  | Webkit_flex_direction -> pp pp_flex_direction
  | Webkit_flex_wrap -> pp pp_flex_wrap
  | Webkit_flex_flow -> pp pp_flex_flow
  | Webkit_justify_content -> pp pp_justify_content
  | Webkit_align_items -> pp pp_align_items
  | Webkit_align_content -> pp pp_align_content
  | Webkit_align_self -> pp pp_align_self
  | Webkit_border_radius -> pp pp_border_radius
  | Webkit_box_shadow -> pp pp_shadow
  | Webkit_background_size -> pp pp_background_size
  | Webkit_filter -> pp pp_filter
  | Moz_appearance -> pp pp_appearance
  | Moz_animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Moz_animation_delay -> pp pp_duration
  | Moz_animation_duration -> pp pp_duration
  | Moz_animation_direction -> pp pp_animation_direction
  | Moz_animation_iteration_count -> pp pp_animation_iteration_count
  | Moz_animation_name -> pp pp_animation_name
  | Moz_animation_timing_function -> pp pp_timing_function
  | Moz_animation_fill_mode -> pp pp_animation_fill_mode
  | Moz_animation_play_state -> pp pp_animation_play_state
  | Moz_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Moz_transition_delay -> pp pp_duration
  | Moz_transition_duration -> pp pp_duration
  | Moz_transition_property -> pp pp_transition_property
  | Moz_transition_timing_function -> pp pp_timing_function
  | Moz_border_radius -> pp pp_border_radius
  | Moz_box_shadow -> pp pp_shadow
  | Moz_orient -> pp pp_moz_orient
  | Ms_filter -> pp pp_filter
  | O_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Font_family -> pp pp_font_family

(* Cascade detected the value is spec-invalid (an [Invalid] arm in one of the
   typed value types). [Optimize.drop_invalid], which every serialisation runs,
   uses this to discard the declaration. *)
let invalid_angle : angle -> bool = function Invalid _ -> true | _ -> false

let invalid_length_percentage : length_percentage -> bool = function
  | Invalid _ -> true
  | _ -> false

let invalid_rotate_value : rotate_value -> bool = function
  | Angle a | X a | Y a | Z a | Axis (_, _, _, a) -> invalid_angle a
  | _ -> false

let invalid_clip_path : clip_path -> bool = function
  | Invalid _ -> true
  | _ -> false

let invalid_text_indent_value : text_indent_value -> bool = function
  | Indent { length; _ } -> invalid_length_percentage length
  | _ -> false

let is_invalid_value : type a. a property -> a -> bool =
 fun property value ->
  match property_class property with
  | Inherited -> false
  | Non_inherited -> false
  | Checks_length_percentage -> invalid_length_percentage value
  | Checks_rotate -> invalid_rotate_value value
  | Checks_clip_path -> invalid_clip_path value
  | Checks_text_indent -> invalid_text_indent_value value
  | Checks_font_family -> ( match value with Invalid _ -> true | _ -> false)

let property_value_kind : type a. a property -> a property_value_kind option =
  function
  | Padding_left -> Some Length
  | Padding_right -> Some Length
  | Padding_bottom -> Some Length
  | Padding_top -> Some Length
  | Padding_inline -> Some Lengths
  | Padding_inline_start -> Some Length
  | Padding_inline_end -> Some Length
  | Padding_block -> Some Lengths
  | Padding_block_start -> Some Length
  | Padding_block_end -> Some Length
  | Margin_inline_end -> Some Length
  | Margin_inline_start -> Some Length
  | Margin_left -> Some Length
  | Margin_right -> Some Length
  | Margin_top -> Some Length
  | Margin_bottom -> Some Length
  | Margin_block_start -> Some Length
  | Margin_block_end -> Some Length
  | Column_gap -> Some Length
  | Row_gap -> Some Length
  | Text_underline_offset -> Some Length
  | Letter_spacing -> Some Length
  | Border_top_left_radius -> Some Lengths
  | Border_top_right_radius -> Some Lengths
  | Border_bottom_left_radius -> Some Lengths
  | Border_bottom_right_radius -> Some Lengths
  | Border_start_start_radius -> Some Lengths
  | Border_start_end_radius -> Some Lengths
  | Border_end_start_radius -> Some Lengths
  | Border_end_end_radius -> Some Lengths
  | Outline_width -> Some Border_width
  | Border_top_width -> Some Border_width
  | Border_right_width -> Some Border_width
  | Border_bottom_width -> Some Border_width
  | Border_left_width -> Some Border_width
  | Border_inline_start_width -> Some Border_width
  | Border_inline_end_width -> Some Border_width
  | Border_block_start_width -> Some Border_width
  | Border_block_end_width -> Some Border_width
  | Outline_offset -> Some Length
  | Text_indent -> None
  | Line_height_step -> Some Length
  | Perspective -> Some Length
  | Text_decoration_thickness -> Some Length
  | Stroke_width -> Some Stroke_width
  | Scroll_margin_top -> Some Length
  | Scroll_margin_right -> Some Length
  | Scroll_margin_bottom -> Some Length
  | Scroll_margin_left -> Some Length
  | Scroll_margin_inline_start -> Some Length
  | Scroll_margin_inline_end -> Some Length
  | Scroll_margin_block_start -> Some Length
  | Scroll_margin_block_end -> Some Length
  | Scroll_padding_top -> Some Length
  | Scroll_padding_right -> Some Length
  | Scroll_padding_bottom -> Some Length
  | Scroll_padding_left -> Some Length
  | Scroll_padding_inline_start -> Some Length
  | Scroll_padding_inline_end -> Some Length
  | Scroll_padding_block_start -> Some Length
  | Scroll_padding_block_end -> Some Length
  | Padding -> Some Lengths
  | Margin -> Some Lengths
  | Margin_inline -> Some Lengths
  | Margin_block -> Some Lengths
  | Inset -> Some Lengths
  | Inset_inline -> Some Lengths
  | Inset_inline_start -> Some Lengths
  | Inset_inline_end -> Some Lengths
  | Inset_block -> Some Lengths
  | Inset_block_start -> Some Lengths
  | Inset_block_end -> Some Lengths
  | Top -> Some Lengths
  | Right -> Some Lengths
  | Bottom -> Some Lengths
  | Left -> Some Lengths
  | Border_width -> Some Border_widths
  | Scroll_margin -> Some Lengths
  | Scroll_margin_inline -> Some Lengths
  | Scroll_margin_block -> Some Lengths
  | Scroll_padding -> Some Lengths
  | Scroll_padding_inline -> Some Lengths
  | Scroll_padding_block -> Some Lengths
  | Width -> Some Length_percentage
  | Height -> Some Length_percentage
  | Min_width -> Some Length_percentage
  | Min_height -> Some Length_percentage
  | Max_width -> Some Length_percentage
  | Max_height -> Some Length_percentage
  | Inline_size -> Some Length_percentage
  | Min_inline_size -> Some Length_percentage
  | Max_inline_size -> Some Length_percentage
  | Block_size -> Some Length_percentage
  | Min_block_size -> Some Length_percentage
  | Max_block_size -> Some Length_percentage
  | Shape_margin -> Some Length_percentage
  | Font_size -> Some Font_size
  | Opacity -> Some Opacity
  | Fill_opacity -> Some Opacity
  | Stroke_opacity -> Some Opacity
  | Stop_opacity -> Some Opacity
  | Flood_opacity -> Some Opacity
  | Rotate -> Some Rotate
  | Animation_duration -> Some Duration
  | Animation_delay -> Some Duration
  | Webkit_animation_duration -> Some Duration
  | Webkit_animation_delay -> Some Duration
  | Moz_animation_duration -> Some Duration
  | Moz_animation_delay -> Some Duration
  | Transition_duration -> Some Duration
  | Transition_delay -> Some Duration
  | Webkit_transition_duration -> Some Duration
  | Webkit_transition_delay -> Some Duration
  | Moz_transition_duration -> Some Duration
  | Moz_transition_delay -> Some Duration
  | Display -> Some Display
  | Position -> Some Position
  | Visibility -> Some Visibility
  | Clear -> Some Clear
  | Float -> Some Float
  | Scale -> Some Scale
  | Translate -> Some Translate
  | Transform -> Some Transform
  | Webkit_transform -> Some Transform
  | Animation -> Some (Animation : animation list property_value_kind)
  | Webkit_animation -> Some (Animation : animation list property_value_kind)
  | Moz_animation -> Some (Animation : animation list property_value_kind)
  | Transition -> Some (Transition : transition list property_value_kind)
  | Webkit_transition -> Some (Transition : transition list property_value_kind)
  | Moz_transition -> Some (Transition : transition list property_value_kind)
  | O_transition -> Some (Transition : transition list property_value_kind)
  | Filter -> Some Filter
  | Backdrop_filter -> Some Filter
  | Webkit_backdrop_filter -> Some Filter
  | Webkit_filter -> Some Filter
  | Ms_filter -> Some Filter
  | Box_shadow -> Some Shadow
  | Webkit_box_shadow -> Some Shadow
  | Moz_box_shadow -> Some Shadow
  | Border_radius -> Some Border_radius
  | Webkit_border_radius -> Some Border_radius
  | Moz_border_radius -> Some Border_radius
  | Offset_distance -> Some Length_percentage
  | Background_color -> Some Color
  | Animation_name -> Some Animation_name
  | Webkit_animation_name -> Some Animation_name
  | Moz_animation_name -> Some Animation_name
  | Color -> Some Color
  | Border_color -> Some Colors
  | Text_decoration_color -> Some Color
  | Border_top_color -> Some Color
  | Border_right_color -> Some Color
  | Border_bottom_color -> Some Color
  | Border_left_color -> Some Color
  | Outline_color -> Some Color
  | Webkit_tap_highlight_color -> Some Color
  | Webkit_text_decoration_color -> Some Color
  | Webkit_text_fill_color -> Some Color
  | Webkit_text_stroke_color -> Some Color
  | Webkit_text_stroke_width -> Some Border_width
  | Column_rule_color -> Some Colors
  | Column_rule_width -> Some Border_widths
  | Accent_color -> Some Color
  | Caret_color -> Some Color
  | Stop_color -> Some Color
  | Flood_color -> Some Color
  | Lighting_color -> Some Color
  | Background_image -> Some Background_images
  | Background -> Some Background
  | Webkit_mask_image -> Some Background_image
  | Border_image_source -> Some Background_image
  | Mask_image -> Some Background_image
  | Source -> Some Font_src
  | Font_family -> Some Font_family
  | _ -> None

(* ===== Readers moved here from Declaration so the API consistency script can
   surface them in [properties.mli]. ===== *)

let pp_property_value_kind : type a. a property_value_kind Pp.t =
 fun ctx -> function
  | Length -> Pp.string ctx "length"
  | Lengths -> Pp.string ctx "lengths"
  | Length_percentage -> Pp.string ctx "length-percentage"
  | Border_width -> Pp.string ctx "border-width"
  | Border_widths -> Pp.string ctx "border-widths"
  | Opacity -> Pp.string ctx "opacity"
  | Rotate -> Pp.string ctx "rotate"
  | Duration -> Pp.string ctx "duration"
  | Number_percentage -> Pp.string ctx "number-percentage"
  | Font_size -> Pp.string ctx "font-size"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Clear -> Pp.string ctx "clear"
  | Float -> Pp.string ctx "float"
  | Scale -> Pp.string ctx "scale"
  | Translate -> Pp.string ctx "translate"
  | Transform -> Pp.string ctx "transform"
  | Animation -> Pp.string ctx "animation"
  | Transition -> Pp.string ctx "transition"
  | Filter -> Pp.string ctx "filter"
  | Shadow -> Pp.string ctx "shadow"
  | Border_radius -> Pp.string ctx "border-radius"
  | Color -> Pp.string ctx "color"
  | Colors -> Pp.string ctx "colors"
  | Animation_name -> Pp.string ctx "animation-name"
  | Background -> Pp.string ctx "background"
  | Background_image -> Pp.string ctx "background-image"
  | Background_images -> Pp.string ctx "background-images"
  | Font_src -> Pp.string ctx "font-src"
  | Font_family -> Pp.string ctx "font-family"
  | Stroke_width -> Pp.string ctx "stroke-width"

let read_property_value_kind (type a) (_ : Cursor.t) : a property_value_kind =
  invalid_arg
    "Properties.read_property_value_kind: property_value_kind is a phantom \
     GADT and cannot be parsed standalone"
