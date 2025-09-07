(** CSS declaration types and parser. *)

include Declaration_intf
open Properties

(* Extract metadata from a declaration *)
let declaration_meta : declaration -> Values.meta option = function
  | Custom_declaration { meta; _ } -> meta
  | Declaration _ -> None
  | Important_declaration _ -> None

(* Helper to mark a declaration as important - needs special handling for
   GADT *)
let important = function
  | Declaration (prop, value) -> Important_declaration (prop, value)
  | Custom_declaration { name; kind; value; layer; meta } ->
      Custom_declaration { name; kind; value; layer; meta }
  (* Custom properties remain as-is; we don't attach !important here *)
  | Important_declaration (prop, value) -> Important_declaration (prop, value)
(* Already important *)

(* Helper for raw custom properties - primarily for internal use *)
let custom_property ?layer name value =
  (* Validate that this is a proper CSS variable name *)
  if not (String.length name > 2 && String.sub name 0 2 = "--") then
    failwith
      (String.concat ""
         [
           "custom_property: ";
           name;
           " is not a valid CSS variable name (must start with --)";
         ]);
  Custom_declaration { name; kind = String; value; layer; meta = None }

(* Access the layer associated with a custom declaration, if any *)
let custom_declaration_layer = function
  | Custom_declaration { layer; _ } -> layer
  | Declaration _ -> None
  | Important_declaration _ -> None

(* Check if a property name is a custom property (starts with --) *)
let is_custom_property name =
  String.length name > 2 && String.sub name 0 2 = "--"

(* Parser functions *)

(** Parse a property name *)
let read_property_name t =
  Reader.ws t;
  let name = Reader.while_ t (fun c -> c <> ':' && c <> ';' && c <> '}') in
  String.trim name

(** Parse property value until semicolon, closing brace, or !important *)
let read_property_value t =
  let rec parse_value acc depth =
    Reader.peek t |> function
    | None -> String.concat "" (List.rev acc)
    | Some ';' when depth = 0 -> String.concat "" (List.rev acc)
    | Some '}' when depth = 0 -> String.concat "" (List.rev acc)
    | Some '!' when depth = 0 -> (
        (* Check if this is !important *)
        Reader.save t;
        Reader.expect t '!';
        Reader.ws t;
        try
          Reader.expect_string t "important";
          Reader.restore t;
          (* Rewind to the '!' *)
          String.concat "" (List.rev acc)
          (* Stop parsing, leave !important for caller *)
        with Reader.Parse_error _ ->
          Reader.restore t;
          parse_value (String.make 1 (Reader.char t) :: acc) depth)
    | Some (('(' | '[' | '{') as c) ->
        Reader.expect t c;
        parse_value (String.make 1 c :: acc) (depth + 1)
    | Some ((')' | ']' | '}') as c) when depth > 0 ->
        Reader.expect t c;
        parse_value (String.make 1 c :: acc) (depth - 1)
    | Some (('"' | '\'') as q) ->
        (* Parse quoted string keeping original escape sequences *)
        Reader.expect t q;
        let rec parse_quoted acc =
          match Reader.peek t with
          | None -> String.concat "" (List.rev acc)
          | Some '\\' ->
              let esc = Reader.char t in
              let next = Reader.char t in
              parse_quoted (String.make 1 next :: String.make 1 esc :: acc)
          | Some c when c = q ->
              Reader.expect t q;
              String.concat "" (List.rev acc)
          | Some c ->
              Reader.expect t c;
              parse_quoted (String.make 1 c :: acc)
        in
        let content = parse_quoted [] in
        parse_value ((String.make 1 q ^ content ^ String.make 1 q) :: acc) depth
    | _ -> parse_value (String.make 1 (Reader.char t) :: acc) depth
  in
  String.trim (parse_value [] 0)

(** Check for and consume !important *)
let read_importance t =
  Reader.ws t;
  match Reader.peek t with
  | Some '!' -> (
      Reader.save t;
      try
        Reader.expect t '!';
        Reader.ws t;
        Reader.expect_string t "important";
        true
      with Reader.Parse_error _ ->
        Reader.restore t;
        false)
  | _ -> false

(** Parse a single declaration. *)
let read_declaration t : (string * string * bool) option =
  Reader.ws t;
  match Reader.peek t with
  | Some '}' | None -> None
  | _ ->
      let name = read_property_name t in
      Reader.ws t;
      Reader.expect t ':';
      Reader.ws t;
      let value = read_property_value t in
      Reader.ws t;
      let is_important = read_importance t in
      Reader.ws t;
      (* Skip optional semicolon *)
      ignore
        (Reader.peek t = Some ';'
        &&
        (Reader.expect t ';';
         true));
      Some (name, value, is_important)

(** Parse all declarations in a block (without braces). *)
let read_declarations t =
  let rec loop acc =
    match read_declaration t with
    | None -> List.rev acc
    | Some decl -> loop (decl :: acc)
  in
  loop []

(** Parse declaration block including braces. *)
let read_block t =
  Reader.ws t;
  Reader.expect t '{';
  Reader.ws t;
  let decls = read_declarations t in
  Reader.ws t;
  Reader.expect t '}';
  decls

(* Pretty printer for declarations *)
let pp_declaration : declaration Pp.t =
 fun ctx -> function
  | Declaration (prop, value) ->
      Properties.pp_property ctx prop;
      Pp.string ctx ": ";
      Properties.pp_property_value ctx (prop, value)
  | Custom_declaration { name; kind; value; _ } -> (
      Pp.string ctx name;
      Pp.string ctx ": ";
      (* Pretty-print custom property value by kind *)
      match kind with
      | String -> Pp.string ctx value
      | Length -> Values.pp_length ctx value
      | Color -> Values.pp_color ctx value
      | Int -> Pp.int ctx value
      | Float -> Pp.float ctx value
      | Duration -> Values.pp_duration ctx value
      | Angle -> Values.pp_angle ctx value
      | Shadow -> Properties.pp_shadow ctx value
      | Box_shadow -> Properties.pp_box_shadow ctx value
      | Content -> Properties.pp_content ctx value
      | _ -> Pp.string ctx "")
  | Important_declaration (prop, value) ->
      Properties.pp_property ctx prop;
      Pp.string ctx ": ";
      Properties.pp_property_value ctx (prop, value);
      Pp.string ctx " !important"

(* Pretty printer for values based on their kind *)
let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp Values.pp_length
  | Color -> pp Values.pp_color
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | String -> pp Pp.string
  | Shadow -> pp Properties.pp_shadow
  | Duration -> pp Values.pp_duration
  | Aspect_ratio -> pp Properties.pp_aspect_ratio
  | Border_style -> pp Properties.pp_border_style
  | Font_weight -> pp Properties.pp_font_weight
  | Font_family -> pp (Pp.list ~sep:Pp.comma Properties.pp_font_family)
  | Font_feature_settings -> pp Properties.pp_font_feature_settings
  | Font_variation_settings -> pp Properties.pp_font_variation_settings
  | Font_variant_numeric -> pp Properties.pp_font_variant_numeric
  | Font_variant_numeric_token -> pp Properties.pp_font_variant_numeric_token
  | Blend_mode -> pp Properties.pp_blend_mode
  | Scroll_snap_strictness -> pp Properties.pp_scroll_snap_strictness
  | Angle -> pp Values.pp_angle
  | Box_shadow -> pp Properties.pp_box_shadow
  | Content -> pp Properties.pp_content

(* Single-to-list property helpers *)
let background_image value = Declaration (Background_image, [ value ])
let text_shadow value = Declaration (Text_shadow, [ value ])
let transition value = Declaration (Transition, [ value ])
let transitions values = Declaration (Transition, values)
let animation value = Declaration (Animation, [ value ])
let box_shadow value = Declaration (Box_shadow, [ value ])
let box_shadow_list values = Declaration (Box_shadow, values)

(* Special helpers *)
let z_index_auto = Declaration (Z_index, Auto)

(* Font variant helpers *)
let font_variant_numeric_tokens tokens = Tokens tokens

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

(* Property constructors with typed values *)
let background_color c = Declaration (Background_color, c)
let color c = Declaration (Color, c)
let border_color c = Declaration (Border_color, c)
let border_style bs = Declaration (Border_style, bs)
let border_top_style bs = Declaration (Border_top_style, bs)
let border_right_style bs = Declaration (Border_right_style, bs)
let border_bottom_style bs = Declaration (Border_bottom_style, bs)
let border_left_style bs = Declaration (Border_left_style, bs)
let text_decoration td = Declaration (Text_decoration, td)
let font_style fs = Declaration (Font_style, fs)
let list_style_type lst = Declaration (List_style_type, lst)
let list_style_position v = Declaration (List_style_position, v)
let list_style_image v = Declaration (List_style_image, v)
let padding len = Declaration (Padding, len)
let padding_left len = Declaration (Padding_left, len)
let padding_right len = Declaration (Padding_right, len)
let padding_bottom len = Declaration (Padding_bottom, len)
let padding_top len = Declaration (Padding_top, len)

(* Remove deprecated string-based versions *)
let margin len = Declaration (Margin, len)
let margin_left len = Declaration (Margin_left, len)
let margin_right len = Declaration (Margin_right, len)
let margin_top len = Declaration (Margin_top, len)
let margin_bottom len = Declaration (Margin_bottom, len)

(* Remove deprecated string-based versions *)
let gap len = Declaration (Gap, len)
let column_gap len = Declaration (Column_gap, len)
let row_gap len = Declaration (Row_gap, len)

(* Grid functions *)
let grid_template_areas template = Declaration (Grid_template_areas, template)
let grid_template template = Declaration (Grid_template, template)
let grid_auto_columns size = Declaration (Grid_auto_columns, size)
let grid_auto_rows size = Declaration (Grid_auto_rows, size)
let grid_row_start value = Declaration (Grid_row_start, value)
let grid_row_end value = Declaration (Grid_row_end, value)
let grid_column_start value = Declaration (Grid_column_start, value)
let grid_column_end value = Declaration (Grid_column_end, value)

let grid_row (start, end_) =
  let pp ctx () =
    Properties.pp_grid_line ctx start;
    Pp.string ctx " / ";
    Properties.pp_grid_line ctx end_
  in
  Declaration (Grid_row, Pp.to_string pp ())

let grid_column (start, end_) =
  let pp ctx () =
    Properties.pp_grid_line ctx start;
    Pp.string ctx " / ";
    Properties.pp_grid_line ctx end_
  in
  Declaration (Grid_column, Pp.to_string pp ())

let grid_area value = Declaration (Grid_area, value)
let width len = Declaration (Width, len)
let height len = Declaration (Height, len)

(* Remove deprecated string-based versions *)
let min_width len = Declaration (Min_width, len)
let min_height len = Declaration (Min_height, len)
let max_width len = Declaration (Max_width, len)
let max_height len = Declaration (Max_height, len)
let font_size len = Declaration (Font_size, len)
let line_height len = Declaration (Line_height, len)
let font_weight w = Declaration (Font_weight, w)
let text_align a = Declaration (Text_align, a)
let text_decoration_style value = Declaration (Text_decoration_style, value)
let text_underline_offset value = Declaration (Text_underline_offset, value)
let text_transform value = Declaration (Text_transform, value)
let letter_spacing len = Declaration (Letter_spacing, len)
let white_space value = Declaration (White_space, value)
let display d = Declaration (Display, d)
let position p = Declaration (Position, p)
let visibility v = Declaration (Visibility, v)
let top len = Declaration (Top, len)
let right len = Declaration (Right, len)
let bottom len = Declaration (Bottom, len)
let left len = Declaration (Left, len)
let opacity value = Declaration (Opacity, value)

(* Remove deprecated string-based versions *)
let flex_direction d = Declaration (Flex_direction, d)
let flex value = Declaration (Flex, value)
let flex_grow value = Declaration (Flex_grow, value)
let flex_shrink value = Declaration (Flex_shrink, value)
let flex_basis value = Declaration (Flex_basis, value)
let flex_wrap value = Declaration (Flex_wrap, value)
let order value = Declaration (Order, value)
let align_items a = Declaration (Align_items, a)
let align_content a = Declaration (Align_content, a)
let align_self a = Declaration (Align_self, a)
let justify_content a = Declaration (Justify_content, a)
let justify_items a = Declaration (Justify_items, a)
let justify_self a = Declaration (Justify_self, a)
let place_content value = Declaration (Place_content, value)
let place_items value = Declaration (Place_items, value)
let place_self value = Declaration (Place_self, value)
let border_width len = Declaration (Border_width, len)
let border_radius len = Declaration (Border_radius, len)
let fill value = Declaration (Fill, value)
let stroke value = Declaration (Stroke, value)
let stroke_width value = Declaration (Stroke_width, value)
let outline_style v = Declaration (Outline_style, v)
let outline_width len = Declaration (Outline_width, len)
let outline_color c = Declaration (Outline_color, c)
let forced_color_adjust v = Declaration (Forced_color_adjust, v)
let table_layout value = Declaration (Table_layout, value)
let border_spacing len = Declaration (Border_spacing, len)
let overflow o = Declaration (Overflow, o)
let object_fit value = Declaration (Object_fit, value)
let clip value = Declaration (Clip, value)
let clear value = Declaration (Clear, value)
let float value = Declaration (Float, value)
let touch_action value = Declaration (Touch_action, value)
let direction value = Declaration (Direction, value)
let unicode_bidi value = Declaration (Unicode_bidi, value)
let writing_mode value = Declaration (Writing_mode, value)

let text_decoration_skip_ink value =
  Declaration (Text_decoration_skip_ink, value)

let animation_name value = Declaration (Animation_name, value)
let animation_duration value = Declaration (Animation_duration, value)

let animation_timing_function value =
  Declaration (Animation_timing_function, value)

let animation_delay value = Declaration (Animation_delay, value)

let animation_iteration_count value =
  Declaration (Animation_iteration_count, value)

let animation_direction value = Declaration (Animation_direction, value)
let animation_fill_mode value = Declaration (Animation_fill_mode, value)
let animation_play_state value = Declaration (Animation_play_state, value)
let background_blend_mode value = Declaration (Background_blend_mode, [ value ])
let scroll_margin value = Declaration (Scroll_margin, value)
let scroll_margin_top value = Declaration (Scroll_margin_top, value)
let scroll_margin_right value = Declaration (Scroll_margin_right, value)
let scroll_margin_bottom value = Declaration (Scroll_margin_bottom, value)
let scroll_margin_left value = Declaration (Scroll_margin_left, value)
let scroll_padding value = Declaration (Scroll_padding, value)
let scroll_padding_top value = Declaration (Scroll_padding_top, value)
let scroll_padding_right value = Declaration (Scroll_padding_right, value)
let scroll_padding_bottom value = Declaration (Scroll_padding_bottom, value)
let scroll_padding_left value = Declaration (Scroll_padding_left, value)
let overscroll_behavior value = Declaration (Overscroll_behavior, value)
let overscroll_behavior_x value = Declaration (Overscroll_behavior_x, value)
let overscroll_behavior_y value = Declaration (Overscroll_behavior_y, value)
let accent_color value = Declaration (Accent_color, value)
let caret_color value = Declaration (Caret_color, value)
let text_decoration_color value = Declaration (Text_decoration_color, value)

let text_decoration_thickness value =
  Declaration (Text_decoration_thickness, value)

let text_size_adjust value = Declaration (Text_size_adjust, value)
let aspect_ratio v = Declaration (Aspect_ratio, v)
let filter value = Declaration (Filter, value)
let word_spacing value = Declaration (Word_spacing, value)
let quotes value = Declaration (Quotes, value)
let border value = Declaration (Border, value)
let tab_size value = Declaration (Tab_size, value)
let webkit_text_size_adjust value = Declaration (Webkit_text_size_adjust, value)
let font_feature_settings value = Declaration (Font_feature_settings, value)
let font_variation_settings value = Declaration (Font_variation_settings, value)

let webkit_tap_highlight_color value =
  Declaration (Webkit_tap_highlight_color, value)

let webkit_text_decoration value = Declaration (Webkit_text_decoration, value)

let webkit_text_decoration_color value =
  Declaration (Webkit_text_decoration_color, value)

let text_indent len = Declaration (Text_indent, len)
let border_collapse value = Declaration (Border_collapse, value)
let list_style value = Declaration (List_style, value)
let font value = Declaration (Font, value)
let webkit_appearance value = Declaration (Webkit_appearance, value)
let transform_style value = Declaration (Transform_style, value)
let backface_visibility value = Declaration (Backface_visibility, value)
let object_position value = Declaration (Object_position, value)
let transition_duration value = Declaration (Transition_duration, value)

let transition_timing_function value =
  Declaration (Transition_timing_function, value)

let transition_delay value = Declaration (Transition_delay, value)

(* Additional declaration constructors to match the interface *)
let mix_blend_mode value = Declaration (Mix_blend_mode, value)
let grid_template_columns value = Declaration (Grid_template_columns, value)
let grid_template_rows value = Declaration (Grid_template_rows, value)
let grid_auto_flow value = Declaration (Grid_auto_flow, value)
let pointer_events value = Declaration (Pointer_events, value)
let z_index value = Declaration (Z_index, value)
let appearance value = Declaration (Appearance, value)
let overflow_x value = Declaration (Overflow_x, value)
let overflow_y value = Declaration (Overflow_y, value)
let resize value = Declaration (Resize, value)
let vertical_align value = Declaration (Vertical_align, value)
let box_sizing value = Declaration (Box_sizing, value)
let font_family value = Declaration (Font_family, value)
let background_attachment value = Declaration (Background_attachment, value)
let border_top value = Declaration (Border_top, value)
let border_right value = Declaration (Border_right, value)
let border_bottom value = Declaration (Border_bottom, value)
let border_left value = Declaration (Border_left, value)
let transform_origin value = Declaration (Transform_origin, value)
let clip_path value = Declaration (Clip_path, value)
let mask value = Declaration (Mask, value)
let content_visibility value = Declaration (Content_visibility, value)
let moz_osx_font_smoothing value = Declaration (Moz_osx_font_smoothing, value)
let webkit_line_clamp value = Declaration (Webkit_line_clamp, value)
let webkit_box_orient value = Declaration (Webkit_box_orient, value)
let text_overflow value = Declaration (Text_overflow, value)
let text_wrap value = Declaration (Text_wrap, value)
let word_break value = Declaration (Word_break, value)
let overflow_wrap value = Declaration (Overflow_wrap, value)
let hyphens value = Declaration (Hyphens, value)
let webkit_hyphens value = Declaration (Webkit_hyphens, value)
let font_stretch value = Declaration (Font_stretch, value)
let font_variant_numeric value = Declaration (Font_variant_numeric, value)
let backdrop_filter value = Declaration (Backdrop_filter, value)
let background_position value = Declaration (Background_position, value)
let background_repeat value = Declaration (Background_repeat, value)
let background_size value = Declaration (Background_size, value)
let content value = Declaration (Content, value)
let border_left_width value = Declaration (Border_left_width, value)

let border_inline_start_width value =
  Declaration (Border_inline_start_width, value)

let border_inline_end_width value = Declaration (Border_inline_end_width, value)
let border_bottom_width value = Declaration (Border_bottom_width, value)
let border_top_width value = Declaration (Border_top_width, value)
let border_right_width value = Declaration (Border_right_width, value)
let border_top_color value = Declaration (Border_top_color, value)
let border_right_color value = Declaration (Border_right_color, value)
let border_bottom_color value = Declaration (Border_bottom_color, value)
let border_left_color value = Declaration (Border_left_color, value)

let border_inline_start_color value =
  Declaration (Border_inline_start_color, value)

let border_inline_end_color value = Declaration (Border_inline_end_color, value)
let webkit_font_smoothing value = Declaration (Webkit_font_smoothing, value)
let cursor value = Declaration (Cursor, value)
let user_select value = Declaration (User_select, value)
let container_type value = Declaration (Container_type, value)
let container_name value = Declaration (Container_name, value)
let transform value = Declaration (Transform, value)
let rotate value = Declaration (Rotate, value)
let scale (value : Properties_intf.scale) = Declaration (Scale, value)
let perspective value = Declaration (Perspective, value)
let perspective_origin value = Declaration (Perspective_origin, value)
let padding_inline value = Declaration (Padding_inline, value)
let padding_inline_start value = Declaration (Padding_inline_start, value)
let padding_inline_end value = Declaration (Padding_inline_end, value)
let padding_block value = Declaration (Padding_block, value)
let margin_inline value = Declaration (Margin_inline, value)
let margin_block value = Declaration (Margin_block, value)
let margin_inline_end value = Declaration (Margin_inline_end, value)
let will_change value = Declaration (Will_change, value)
let contain value = Declaration (Contain, value)
let isolation value = Declaration (Isolation, value)
let outline value = Declaration (Outline, value)
let outline_offset len = Declaration (Outline_offset, len)
let scroll_snap_type value = Declaration (Scroll_snap_type, value)
let scroll_snap_align value = Declaration (Scroll_snap_align, value)
let scroll_snap_stop value = Declaration (Scroll_snap_stop, value)
let scroll_behavior value = Declaration (Scroll_behavior, value)
