(** CSS declaration types and parser. *)

include Declaration_intf
open Properties

(* Re-export pp_property from Properties module *)
let pp_property = Properties.pp_property

(* Extract metadata from a declaration *)
let declaration_meta : declaration -> Values.meta option = function
  | Custom_declaration { meta; _ } -> meta
  | Declaration _ -> None

(* Smart constructor for declarations *)
let declaration ?(important = false) property value =
  Declaration { property; value; important }

(* Smart constructor for custom declarations *)
let custom_declaration ?(important = false) ?layer ?meta name kind value =
  Custom_declaration { name; kind; value; layer; meta; important }

(* Helper to mark a declaration as important *)
let important = function
  | Declaration { property; value; _ } ->
      Declaration { property; value; important = true }
  | Custom_declaration d -> Custom_declaration { d with important = true }

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
  custom_declaration ?layer name String value

(* Access the layer associated with a custom declaration, if any *)
let custom_declaration_layer = function
  | Custom_declaration { layer; _ } -> layer
  | Declaration _ -> None

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
  let trimmed = String.trim (parse_value [] 0) in
  (* CSS spec: empty property values are invalid *)
  if String.length trimmed = 0 then
    Reader.err_invalid t "property value (cannot be empty)";
  trimmed

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

(** Parse a single declaration with full type checking. *)
let read_declaration t : declaration option =
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

      (* Convert to typed declaration *)
      let decl =
        if is_custom_property name then custom_property name value
        else
          (* Parse standard properties using proper type-driven approach *)
          try
            (* First, identify the property type from the name *)
            let prop_reader = Reader.of_string name in
            let (Properties.Prop prop_type) =
              Properties.read_property prop_reader
            in

            (* Now parse the value based on the property type *)
            let value_reader = Reader.of_string value in
            let declaration =
              match prop_type with
              | Properties.Color ->
                  let color = Values.read_color value_reader in
                  declaration Properties.Color color
              | Properties.Background_color ->
                  let color = Values.read_color value_reader in
                  declaration Properties.Background_color color
              | Properties.Display ->
                  let display = Properties.read_display value_reader in
                  declaration Properties.Display display
              | Properties.Position ->
                  let position = Properties.read_position value_reader in
                  declaration Properties.Position position
              | Properties.Width ->
                  let width = Values.read_length value_reader in
                  declaration Properties.Width width
              | Properties.Height ->
                  let height = Values.read_length value_reader in
                  declaration Properties.Height height
              | Properties.Padding ->
                  let padding = Values.read_length value_reader in
                  declaration Properties.Padding padding
              | Properties.Margin ->
                  let margin = Values.read_length value_reader in
                  declaration Properties.Margin margin
              | _ ->
                  (* For properties we haven't implemented readers for yet *)
                  Reader.err_invalid t
                    ("property '" ^ name ^ "' (not yet implemented)")
            in
            declaration
          with
          | Reader.Parse_error _ as e -> raise e (* Re-raise parse errors *)
          | _ ->
              (* If parsing fails for other reasons, it's an invalid property *)
              Reader.err_invalid t ("property '" ^ name ^ "' (unknown property)")
      in
      Some (if is_important then important decl else decl)

(** Type-driven helper functions for working with declarations *)

(** Check if a declaration is marked as important *)
let is_important = function
  | Declaration { important; _ } -> important
  | Custom_declaration { important; _ } -> important

(** Get the property name as a string from a declaration *)
let property_name decl =
  let ctx =
    { Pp.minify = true; indent = 0; buf = Buffer.create 16; inline = false }
  in
  match decl with
  | Declaration { property; _ } ->
      Properties.pp_property ctx property;
      Buffer.contents ctx.buf
  | Custom_declaration { name; _ } -> name

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

(** Get the value as a string from a declaration *)
let string_of_value ?(minify = true) decl =
  let ctx = { Pp.minify; indent = 0; buf = Buffer.create 16; inline = false } in
  match decl with
  | Declaration { property; value; _ } ->
      Properties.pp_property_value ctx (property, value);
      Buffer.contents ctx.buf
  | Custom_declaration { kind; value; _ } ->
      pp_value ctx (kind, value);
      Buffer.contents ctx.buf

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
  | Declaration { property; value; _ } ->
      Properties.pp_property ctx property;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      Properties.pp_property_value ctx (property, value)
  | Custom_declaration { name; kind; value; _ } -> (
      Pp.string ctx name;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
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

(* Single-to-list property helpers *)
let background_image value = declaration Background_image [ value ]
let text_shadow value = declaration Text_shadow [ value ]
let transition value = declaration Transition [ value ]
let transitions values = declaration Transition values
let animation value = declaration Animation [ value ]
let box_shadow value = declaration Box_shadow [ value ]
let box_shadow_list values = declaration Box_shadow values

(* Special helpers *)
let z_index_auto = declaration Z_index Auto

(* Font variant helpers *)
let font_variant_numeric_tokens tokens = Tokens tokens

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

(* Property constructors with typed values *)
let background_color c = declaration Background_color c
let color c = declaration Color c
let border_color c = declaration Border_color c
let border_style bs = declaration Border_style bs
let border_top_style bs = declaration Border_top_style bs
let border_right_style bs = declaration Border_right_style bs
let border_bottom_style bs = declaration Border_bottom_style bs
let border_left_style bs = declaration Border_left_style bs
let text_decoration td = declaration Text_decoration td
let font_style fs = declaration Font_style fs
let list_style_type lst = declaration List_style_type lst
let list_style_position v = declaration List_style_position v
let list_style_image v = declaration List_style_image v
let padding len = declaration Padding len
let padding_left len = declaration Padding_left len
let padding_right len = declaration Padding_right len
let padding_bottom len = declaration Padding_bottom len
let padding_top len = declaration Padding_top len

(* Remove deprecated string-based versions *)
let margin len = declaration Margin len
let margin_left len = declaration Margin_left len
let margin_right len = declaration Margin_right len
let margin_top len = declaration Margin_top len
let margin_bottom len = declaration Margin_bottom len

(* Remove deprecated string-based versions *)
let gap len = declaration Gap len
let column_gap len = declaration Column_gap len
let row_gap len = declaration Row_gap len

(* Grid functions *)
let grid_template_areas template = declaration Grid_template_areas template
let grid_template template = declaration Grid_template template
let grid_auto_columns size = declaration Grid_auto_columns size
let grid_auto_rows size = declaration Grid_auto_rows size
let grid_row_start value = declaration Grid_row_start value
let grid_row_end value = declaration Grid_row_end value
let grid_column_start value = declaration Grid_column_start value
let grid_column_end value = declaration Grid_column_end value

let grid_row (start, end_) =
  let pp ctx () =
    Properties.pp_grid_line ctx start;
    Pp.string ctx " / ";
    Properties.pp_grid_line ctx end_
  in
  declaration Grid_row (Pp.to_string pp ())

let grid_column (start, end_) =
  let pp ctx () =
    Properties.pp_grid_line ctx start;
    Pp.string ctx " / ";
    Properties.pp_grid_line ctx end_
  in
  declaration Grid_column (Pp.to_string pp ())

let grid_area value = declaration Grid_area value
let width len = declaration Width len
let height len = declaration Height len

(* Remove deprecated string-based versions *)
let min_width len = declaration Min_width len
let min_height len = declaration Min_height len
let max_width len = declaration Max_width len
let max_height len = declaration Max_height len
let font_size len = declaration Font_size len
let line_height len = declaration Line_height len
let font_weight w = declaration Font_weight w
let text_align a = declaration Text_align a
let text_decoration_style value = declaration Text_decoration_style value
let text_underline_offset value = declaration Text_underline_offset value
let text_transform value = declaration Text_transform value
let letter_spacing len = declaration Letter_spacing len
let white_space value = declaration White_space value
let display d = declaration Display d
let position p = declaration Position p
let visibility v = declaration Visibility v
let top len = declaration Top len
let right len = declaration Right len
let bottom len = declaration Bottom len
let left len = declaration Left len
let opacity value = declaration Opacity value

(* Remove deprecated string-based versions *)
let flex_direction d = declaration Flex_direction d
let flex value = declaration Flex value
let flex_grow value = declaration Flex_grow value
let flex_shrink value = declaration Flex_shrink value
let flex_basis value = declaration Flex_basis value
let flex_wrap value = declaration Flex_wrap value
let order value = declaration Order value
let align_items a = declaration Align_items a
let align_content a = declaration Align_content a
let align_self a = declaration Align_self a
let justify_content a = declaration Justify_content a
let justify_items a = declaration Justify_items a
let justify_self a = declaration Justify_self a
let place_content value = declaration Place_content value
let place_items value = declaration Place_items value
let place_self value = declaration Place_self value
let border_width len = declaration Border_width len
let border_radius len = declaration Border_radius len
let fill value = declaration Fill value
let stroke value = declaration Stroke value
let stroke_width value = declaration Stroke_width value
let outline_style v = declaration Outline_style v
let outline_width len = declaration Outline_width len
let outline_color c = declaration Outline_color c
let forced_color_adjust v = declaration Forced_color_adjust v
let table_layout value = declaration Table_layout value
let border_spacing len = declaration Border_spacing len
let overflow o = declaration Overflow o
let object_fit value = declaration Object_fit value
let clip value = declaration Clip value
let clear value = declaration Clear value
let float value = declaration Float value
let touch_action value = declaration Touch_action value
let direction value = declaration Direction value
let unicode_bidi value = declaration Unicode_bidi value
let writing_mode value = declaration Writing_mode value
let text_decoration_skip_ink value = declaration Text_decoration_skip_ink value
let animation_name value = declaration Animation_name value
let animation_duration value = declaration Animation_duration value

let animation_timing_function value =
  declaration Animation_timing_function value

let animation_delay value = declaration Animation_delay value

let animation_iteration_count value =
  declaration Animation_iteration_count value

let animation_direction value = declaration Animation_direction value
let animation_fill_mode value = declaration Animation_fill_mode value
let animation_play_state value = declaration Animation_play_state value
let background_blend_mode value = declaration Background_blend_mode [ value ]
let scroll_margin value = declaration Scroll_margin value
let scroll_margin_top value = declaration Scroll_margin_top value
let scroll_margin_right value = declaration Scroll_margin_right value
let scroll_margin_bottom value = declaration Scroll_margin_bottom value
let scroll_margin_left value = declaration Scroll_margin_left value
let scroll_padding value = declaration Scroll_padding value
let scroll_padding_top value = declaration Scroll_padding_top value
let scroll_padding_right value = declaration Scroll_padding_right value
let scroll_padding_bottom value = declaration Scroll_padding_bottom value
let scroll_padding_left value = declaration Scroll_padding_left value
let overscroll_behavior value = declaration Overscroll_behavior value
let overscroll_behavior_x value = declaration Overscroll_behavior_x value
let overscroll_behavior_y value = declaration Overscroll_behavior_y value
let accent_color value = declaration Accent_color value
let caret_color value = declaration Caret_color value
let text_decoration_color value = declaration Text_decoration_color value

let text_decoration_thickness value =
  declaration Text_decoration_thickness value

let text_size_adjust value = declaration Text_size_adjust value
let aspect_ratio v = declaration Aspect_ratio v
let filter value = declaration Filter value
let word_spacing value = declaration Word_spacing value
let quotes value = declaration Quotes value
let border value = declaration Border value
let tab_size value = declaration Tab_size value
let webkit_text_size_adjust value = declaration Webkit_text_size_adjust value
let font_feature_settings value = declaration Font_feature_settings value
let font_variation_settings value = declaration Font_variation_settings value

let webkit_tap_highlight_color value =
  declaration Webkit_tap_highlight_color value

let webkit_text_decoration value = declaration Webkit_text_decoration value

let webkit_text_decoration_color value =
  declaration Webkit_text_decoration_color value

let text_indent len = declaration Text_indent len
let border_collapse value = declaration Border_collapse value
let list_style value = declaration List_style value
let font value = declaration Font value
let webkit_appearance value = declaration Webkit_appearance value
let transform_style value = declaration Transform_style value
let backface_visibility value = declaration Backface_visibility value
let object_position value = declaration Object_position value
let transition_duration value = declaration Transition_duration value

let transition_timing_function value =
  declaration Transition_timing_function value

let transition_delay value = declaration Transition_delay value

(* Additional declaration constructors to match the interface *)
let mix_blend_mode value = declaration Mix_blend_mode value
let grid_template_columns value = declaration Grid_template_columns value
let grid_template_rows value = declaration Grid_template_rows value
let grid_auto_flow value = declaration Grid_auto_flow value
let pointer_events value = declaration Pointer_events value
let z_index value = declaration Z_index value
let appearance value = declaration Appearance value
let overflow_x value = declaration Overflow_x value
let overflow_y value = declaration Overflow_y value
let resize value = declaration Resize value
let vertical_align value = declaration Vertical_align value
let box_sizing value = declaration Box_sizing value
let font_family value = declaration Font_family value
let background_attachment value = declaration Background_attachment value
let border_top value = declaration Border_top value
let border_right value = declaration Border_right value
let border_bottom value = declaration Border_bottom value
let border_left value = declaration Border_left value
let transform_origin value = declaration Transform_origin value
let clip_path value = declaration Clip_path value
let mask value = declaration Mask value
let content_visibility value = declaration Content_visibility value
let moz_osx_font_smoothing value = declaration Moz_osx_font_smoothing value
let webkit_line_clamp value = declaration Webkit_line_clamp value
let webkit_box_orient value = declaration Webkit_box_orient value
let text_overflow value = declaration Text_overflow value
let text_wrap value = declaration Text_wrap value
let word_break value = declaration Word_break value
let overflow_wrap value = declaration Overflow_wrap value
let hyphens value = declaration Hyphens value
let webkit_hyphens value = declaration Webkit_hyphens value
let font_stretch value = declaration Font_stretch value
let font_variant_numeric value = declaration Font_variant_numeric value
let backdrop_filter value = declaration Backdrop_filter value
let background_position value = declaration Background_position value
let background_repeat value = declaration Background_repeat value
let background_size value = declaration Background_size value
let content value = declaration Content value
let border_left_width value = declaration Border_left_width value

let border_inline_start_width value =
  declaration Border_inline_start_width value

let border_inline_end_width value = declaration Border_inline_end_width value
let border_bottom_width value = declaration Border_bottom_width value
let border_top_width value = declaration Border_top_width value
let border_right_width value = declaration Border_right_width value
let border_top_color value = declaration Border_top_color value
let border_right_color value = declaration Border_right_color value
let border_bottom_color value = declaration Border_bottom_color value
let border_left_color value = declaration Border_left_color value

let border_inline_start_color value =
  declaration Border_inline_start_color value

let border_inline_end_color value = declaration Border_inline_end_color value
let webkit_font_smoothing value = declaration Webkit_font_smoothing value
let cursor value = declaration Cursor value
let user_select value = declaration User_select value
let container_type value = declaration Container_type value
let container_name value = declaration Container_name value
let transform value = declaration Transform value
let rotate value = declaration Rotate value
let scale (value : Properties_intf.scale) = declaration Scale value
let perspective value = declaration Perspective value
let perspective_origin value = declaration Perspective_origin value
let padding_inline value = declaration Padding_inline value
let padding_inline_start value = declaration Padding_inline_start value
let padding_inline_end value = declaration Padding_inline_end value
let padding_block value = declaration Padding_block value
let margin_inline value = declaration Margin_inline value
let margin_block value = declaration Margin_block value
let margin_inline_end value = declaration Margin_inline_end value
let will_change value = declaration Will_change value
let contain value = declaration Contain value
let isolation value = declaration Isolation value
let outline value = declaration Outline value
let outline_offset len = declaration Outline_offset len
let scroll_snap_type value = declaration Scroll_snap_type value
let scroll_snap_align value = declaration Scroll_snap_align value
let scroll_snap_stop value = declaration Scroll_snap_stop value
let scroll_behavior value = declaration Scroll_behavior value

(** Parse a typed declaration from property name and value strings *)
let read_typed_declaration name value is_important =
  if is_custom_property name then Some (custom_property name value)
  else
    (* Parse standard properties using type-driven approach *)
    try
      (* First, identify the property type from the name *)
      let prop_reader = Reader.of_string name in
      let (Properties.Prop prop_type) = Properties.read_property prop_reader in

      (* Now parse the value based on the property type *)
      let value_reader = Reader.of_string value in
      let declaration =
        match prop_type with
        (* Color properties *)
        | Properties.Color ->
            let color = Values.read_color value_reader in
            declaration Properties.Color color
        | Properties.Background_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Background_color color
        | Properties.Border_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Border_color color
        | Properties.Border_top_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Border_top_color color
        | Properties.Border_right_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Border_right_color color
        | Properties.Border_bottom_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Border_bottom_color color
        | Properties.Border_left_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Border_left_color color
        | Properties.Outline_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Outline_color color
        | Properties.Text_decoration_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Text_decoration_color color
        | Properties.Accent_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Accent_color color
        | Properties.Caret_color ->
            let color = Values.read_color value_reader in
            declaration Properties.Caret_color color
        (* Display and layout *)
        | Properties.Display ->
            let display = Properties.read_display value_reader in
            declaration Properties.Display display
        | Properties.Position ->
            let position = Properties.read_position value_reader in
            declaration Properties.Position position
        | Properties.Visibility ->
            let visibility = Properties.read_visibility value_reader in
            declaration Properties.Visibility visibility
        | Properties.Overflow ->
            let overflow = Properties.read_overflow value_reader in
            declaration Properties.Overflow overflow
        | Properties.Overflow_x ->
            let overflow = Properties.read_overflow value_reader in
            declaration Properties.Overflow_x overflow
        | Properties.Overflow_y ->
            let overflow = Properties.read_overflow value_reader in
            declaration Properties.Overflow_y overflow
        (* Length properties *)
        | Properties.Width ->
            let width = Values.read_length value_reader in
            declaration Properties.Width width
        | Properties.Height ->
            let height = Values.read_length value_reader in
            declaration Properties.Height height
        | Properties.Min_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Min_width width
        | Properties.Min_height ->
            let height = Values.read_length value_reader in
            declaration Properties.Min_height height
        | Properties.Max_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Max_width width
        | Properties.Max_height ->
            let height = Values.read_length value_reader in
            declaration Properties.Max_height height
        (* Padding *)
        | Properties.Padding ->
            let padding = Values.read_length value_reader in
            declaration Properties.Padding padding
        | Properties.Padding_left ->
            let padding = Values.read_length value_reader in
            declaration Properties.Padding_left padding
        | Properties.Padding_right ->
            let padding = Values.read_length value_reader in
            declaration Properties.Padding_right padding
        | Properties.Padding_top ->
            let padding = Values.read_length value_reader in
            declaration Properties.Padding_top padding
        | Properties.Padding_bottom ->
            let padding = Values.read_length value_reader in
            declaration Properties.Padding_bottom padding
        (* Margin *)
        | Properties.Margin ->
            let margin = Values.read_length value_reader in
            declaration Properties.Margin margin
        | Properties.Margin_left ->
            let margin = Values.read_length value_reader in
            declaration Properties.Margin_left margin
        | Properties.Margin_right ->
            let margin = Values.read_length value_reader in
            declaration Properties.Margin_right margin
        | Properties.Margin_top ->
            let margin = Values.read_length value_reader in
            declaration Properties.Margin_top margin
        | Properties.Margin_bottom ->
            let margin = Values.read_length value_reader in
            declaration Properties.Margin_bottom margin
        (* Border styles *)
        | Properties.Border_style ->
            let style = Properties.read_border_style value_reader in
            declaration Properties.Border_style style
        | Properties.Border_top_style ->
            let style = Properties.read_border_style value_reader in
            declaration Properties.Border_top_style style
        | Properties.Border_right_style ->
            let style = Properties.read_border_style value_reader in
            declaration Properties.Border_right_style style
        | Properties.Border_bottom_style ->
            let style = Properties.read_border_style value_reader in
            declaration Properties.Border_bottom_style style
        | Properties.Border_left_style ->
            let style = Properties.read_border_style value_reader in
            declaration Properties.Border_left_style style
        (* Border widths *)
        | Properties.Border_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Border_width width
        | Properties.Border_top_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Border_top_width width
        | Properties.Border_right_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Border_right_width width
        | Properties.Border_bottom_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Border_bottom_width width
        | Properties.Border_left_width ->
            let width = Values.read_length value_reader in
            declaration Properties.Border_left_width width
        (* Typography *)
        | Properties.Font_size ->
            let size = Values.read_length value_reader in
            declaration Properties.Font_size size
        | Properties.Font_weight ->
            let weight = Properties.read_font_weight value_reader in
            declaration Properties.Font_weight weight
        | Properties.Font_style ->
            let style = Properties.read_font_style value_reader in
            declaration Properties.Font_style style
        | Properties.Line_height ->
            let height = Properties.read_line_height value_reader in
            declaration Properties.Line_height height
        | Properties.Letter_spacing ->
            let spacing = Values.read_length value_reader in
            declaration Properties.Letter_spacing spacing
        | Properties.Word_spacing ->
            let spacing = Values.read_length value_reader in
            declaration Properties.Word_spacing spacing
        | Properties.Text_align ->
            let align = Properties.read_text_align value_reader in
            declaration Properties.Text_align align
        | Properties.Text_transform ->
            let transform = Properties.read_text_transform value_reader in
            declaration Properties.Text_transform transform
        | Properties.White_space ->
            let ws = Properties.read_white_space value_reader in
            declaration Properties.White_space ws
        (* Flexbox *)
        | Properties.Flex_direction ->
            let dir = Properties.read_flex_direction value_reader in
            declaration Properties.Flex_direction dir
        | Properties.Flex_wrap ->
            let wrap = Properties.read_flex_wrap value_reader in
            declaration Properties.Flex_wrap wrap
        | Properties.Align_items ->
            let align = Properties.read_align_items value_reader in
            declaration Properties.Align_items align
        | Properties.Justify_content ->
            let justify = Properties.read_justify_content value_reader in
            declaration Properties.Justify_content justify
        (* Z-index *)
        | Properties.Z_index ->
            let z = Properties.read_z_index value_reader in
            declaration Properties.Z_index z
        (* Opacity *)
        | Properties.Opacity ->
            let opacity = Reader.number value_reader in
            declaration Properties.Opacity opacity
        | _ ->
            (* For properties we haven't implemented readers for yet *)
            raise Not_found
      in
      Some (if is_important then important declaration else declaration)
    with _ ->
      (* If parsing fails, return None *)
      None
