(** CSS declaration types and parser. *)

include Declaration_intf
open Properties
open Values

(* Re-export pp_property from Properties module *)
let pp_property = pp_property

(* Extract metadata from a declaration *)
let declaration_meta : declaration -> meta option = function
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

(** Parse property value with validation for missing semicolons *)
let read_property_value t =
  Reader.with_context t "property-value" @@ fun () ->
  (* Read value token by token, detecting property-like patterns early *)
  let buf = Buffer.create 64 in
  let rec parse_tokens depth in_quote quote_char =
    match Reader.peek t with
    | None -> Buffer.contents buf
    | Some c when in_quote ->
        Buffer.add_char buf c;
        Reader.skip t;
        if c = quote_char then parse_tokens depth false '\000'
        else if c = '\\' then (
          match Reader.peek t with
          | None -> Buffer.contents buf
          | Some next_c ->
              Buffer.add_char buf next_c;
              Reader.skip t;
              parse_tokens depth in_quote quote_char)
        else parse_tokens depth in_quote quote_char
    | Some (('"' | '\'') as q) ->
        Buffer.add_char buf q;
        Reader.skip t;
        parse_tokens depth true q
    | Some (('(' | '[' | '{') as c) ->
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens (depth + 1) in_quote quote_char
    | Some ((')' | ']' | '}') as c) when depth > 0 ->
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens (depth - 1) in_quote quote_char
    | Some c when depth = 0 && (c = ';' || c = '}' || c = '!') ->
        Buffer.contents buf
    | Some c when depth = 0 && Reader.is_ident_start c ->
        (* Check if this could be start of a new property declaration *)
        let potential_prop =
          Reader.while_ t (fun ch ->
              Reader.is_ident_start ch || (ch >= '0' && ch <= '9') || ch = '-')
        in
        (* Save the next character before consuming whitespace *)
        let next_char_before_ws = Reader.peek t in
        Reader.ws t;
        if Reader.peek t = Some ':' then
          (* This looks like "property:" pattern - missing semicolon *)
          Reader.err_invalid t "missing semicolon before property declaration"
        else (
          (* Not a property, add to buffer and continue *)
          Buffer.add_string buf potential_prop;
          (* Add a space if we consumed whitespace *)
          let next_char_after_ws = Reader.peek t in
          if
            next_char_before_ws <> next_char_after_ws
            && next_char_after_ws <> None
          then Buffer.add_char buf ' ';
          parse_tokens depth in_quote quote_char)
    | Some c ->
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens depth in_quote quote_char
  in
  let value = parse_tokens 0 false '\000' in
  let trimmed = String.trim value in
  if String.length trimmed = 0 then
    Reader.err_invalid t "property value (cannot be empty)";
  trimmed

(** Check for and consume !important *)
let read_importance t =
  Reader.ws t;
  match Reader.peek t with
  | Some '!' ->
      Reader.expect '!' t;
      (* After !, we can have optional whitespace/comments before "important" *)
      Reader.ws t;
      (* Try to read an identifier after the ! *)
      if
        Reader.peek t
        |> Option.map Reader.is_ident_start
        |> Option.value ~default:false
      then
        let ident = Reader.ident t in
        if ident = "important" then true
        else Reader.err_invalid t ("invalid !important declaration: !" ^ ident)
      else
        (* No identifier after ! - dangling bang *)
        Reader.err_invalid t "dangling ! without important"
  | _ -> false

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
      pp_property ctx property;
      Buffer.contents ctx.buf
  | Custom_declaration { name; _ } -> name

(* Pretty printer for values based on their kind *)
let pp_value : type a. (a kind * a) Pp.t =
 fun ctx (kind, value) ->
  let pp pp_a = pp_a ctx value in
  match kind with
  | Length -> pp pp_length
  | Color -> pp pp_color
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | String -> pp Pp.string
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Border -> pp pp_border
  | Font_weight -> pp pp_font_weight
  | Font_family -> pp (Pp.list ~sep:Pp.comma pp_font_family)
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Font_variant_numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_numeric_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Box_shadow -> pp pp_box_shadow
  | Content -> pp pp_content

let string_of_value ?(minify = true) decl =
  let ctx = { Pp.minify; indent = 0; buf = Buffer.create 16; inline = false } in
  match decl with
  | Declaration { property; value; _ } ->
      pp_property_value ctx (property, value);
      Buffer.contents ctx.buf
  | Custom_declaration { kind; value; _ } ->
      pp_value ctx (kind, value);
      Buffer.contents ctx.buf

(* Helper to read a trimmed string *)
let read_string t = Reader.string ~trim:true t

(* Parse value directly based on property type *)
let read_value (type a) (prop_type : a property) t : declaration =
  let prop_name =
    let buf = Buffer.create 32 in
    let ctx = { Pp.minify = true; indent = 0; buf; inline = false } in
    pp_property ctx prop_type;
    Buffer.contents buf
  in
  Reader.with_context t prop_name @@ fun () ->
  match prop_type with
  | Color -> declaration Color (read_color t)
  | Background_color -> declaration Background_color (read_color t)
  | Border_color -> declaration Border_color (read_color t)
  | Outline_color -> declaration Outline_color (read_color t)
  | Border_top_color -> declaration Border_top_color (read_color t)
  | Border_right_color -> declaration Border_right_color (read_color t)
  | Border_bottom_color -> declaration Border_bottom_color (read_color t)
  | Border_left_color -> declaration Border_left_color (read_color t)
  (* Length properties *)
  | Width -> declaration Width (read_length t)
  | Height -> declaration Height (read_length t)
  | Min_width -> declaration Min_width (read_length t)
  | Min_height -> declaration Min_height (read_length t)
  | Max_width -> declaration Max_width (read_length t)
  | Max_height -> declaration Max_height (read_length t)
  | Font_size -> declaration Font_size (read_length t)
  | Border_radius -> declaration Border_radius (read_length t)
  | Gap -> declaration Gap (read_length t)
  | Column_gap -> declaration Column_gap (read_length t)
  | Row_gap -> declaration Row_gap (read_length t)
  (* Display and layout *)
  | Display -> declaration Display (read_display t)
  | Position -> declaration Position (read_position t)
  | Visibility -> declaration Visibility (read_visibility t)
  | Overflow -> declaration Overflow (read_overflow t)
  | Overflow_x -> declaration Overflow_x (read_overflow t)
  | Overflow_y -> declaration Overflow_y (read_overflow t)
  (* Padding/Margin *)
  | Padding -> declaration Padding (read_non_negative_length t)
  | Margin -> declaration Margin (read_margin_shorthand t)
  (* Border styles *)
  | Border_style -> declaration Border_style (read_border_style t)
  | Border_width -> declaration Border_width (read_border_width t)
  | Border_top_width -> declaration Border_top_width (read_border_width t)
  | Border_right_width -> declaration Border_right_width (read_border_width t)
  | Border_bottom_width -> declaration Border_bottom_width (read_border_width t)
  | Border_left_width -> declaration Border_left_width (read_border_width t)
  (* Typography *)
  | Line_height -> declaration Line_height (read_line_height t)
  | Font_weight -> declaration Font_weight (read_font_weight t)
  | Font_style -> declaration Font_style (read_font_style t)
  | Font_family ->
      (* Font-family accepts a comma-separated list *)
      declaration Font_family (Reader.list ~sep:Reader.comma read_font_family t)
  | Text_align -> declaration Text_align (read_text_align t)
  | Text_transform -> declaration Text_transform (read_text_transform t)
  | White_space -> declaration White_space (read_white_space t)
  | Text_decoration -> declaration Text_decoration (read_text_decoration t)
  | Transform_origin -> declaration Transform_origin (read_transform_origin t)
  (* Flexbox *)
  | Flex_direction -> declaration Flex_direction (read_flex_direction t)
  | Flex_wrap -> declaration Flex_wrap (read_flex_wrap t)
  | Flex -> declaration Flex (read_flex t)
  | Flex_grow -> declaration Flex_grow (Reader.number t)
  | Flex_shrink -> declaration Flex_shrink (Reader.number t)
  | Flex_basis -> declaration Flex_basis (read_length t)
  | Align_items -> declaration Align_items (read_align_items t)
  | Justify_content -> declaration Justify_content (read_justify_content t)
  (* Transform property *)
  | Transform ->
      let transforms, error_opt = Reader.many read_transform t in
      if List.length transforms = 0 then
        match error_opt with
        | Some msg -> Reader.err_invalid t ("transform: " ^ msg)
        | None -> Reader.err_invalid t "transform value"
      else declaration Transform transforms
  (* Webkit Transform *)
  | Webkit_transform ->
      let transforms, error_opt = Reader.many read_transform t in
      if List.length transforms = 0 then
        match error_opt with
        | Some msg -> Reader.err_invalid t ("webkit-transform: " ^ msg)
        | None -> Reader.err_invalid t "webkit-transform value"
      else declaration Webkit_transform transforms
  (* Webkit Transition *)
  | Webkit_transition -> declaration Webkit_transition (read_transitions t)
  (* Webkit Filter *)
  | Webkit_filter -> declaration Webkit_filter (read_filter t)
  (* Moz Appearance *)
  | Moz_appearance -> declaration Moz_appearance (read_appearance t)
  (* Ms Filter *)
  | Ms_filter -> declaration Ms_filter (read_filter t)
  (* O Transition *)
  | O_transition -> declaration O_transition (read_transitions t)
  (* Filter *)
  | Filter -> declaration Filter (read_filter t)
  (* Appearance *)
  | Appearance -> declaration Appearance (read_appearance t)
  (* Background *)
  | Background_image ->
      let images = read_background_images t in
      declaration Background_image images
  | Background -> declaration Background (read_background t)
  | Border -> declaration Border (read_border t)
  (* Grid properties *)
  | Grid_template_columns ->
      declaration Grid_template_columns (read_grid_template t)
  | Grid_template_rows -> declaration Grid_template_rows (read_grid_template t)
  | Grid_row_start -> declaration Grid_row_start (read_grid_line t)
  | Grid_row_end -> declaration Grid_row_end (read_grid_line t)
  | Grid_column_start -> declaration Grid_column_start (read_grid_line t)
  | Grid_column_end -> declaration Grid_column_end (read_grid_line t)
  | Grid_auto_flow -> declaration Grid_auto_flow (read_grid_auto_flow t)
  | Grid_template_areas -> declaration Grid_template_areas (read_string t)
  (* Shadows *)
  | Box_shadow -> declaration Box_shadow (read_box_shadows t)
  | Text_shadow -> declaration Text_shadow (read_text_shadows t)
  (* Content *)
  | Content -> declaration Content (read_content t)
  (* Other properties *)
  | Z_index -> declaration Z_index (read_z_index t)
  | Opacity ->
      let n = Reader.number t in
      declaration Opacity n
  | Cursor -> declaration Cursor (read_cursor t)
  | Box_sizing -> declaration Box_sizing (read_box_sizing t)
  | User_select -> declaration User_select (read_user_select t)
  | Pointer_events -> declaration Pointer_events (read_pointer_events t)
  | Resize -> declaration Resize (read_resize t)
  | Transition -> declaration Transition (read_transitions t)
  | Animation -> declaration Animation (read_animations t)
  (* Border style properties *)
  | Border_top_style -> declaration Border_top_style (read_border_style t)
  | Border_right_style -> declaration Border_right_style (read_border_style t)
  | Border_bottom_style -> declaration Border_bottom_style (read_border_style t)
  | Border_left_style -> declaration Border_left_style (read_border_style t)
  (* Additional margin/padding properties *)
  | Padding_left -> declaration Padding_left (read_non_negative_length t)
  | Padding_right -> declaration Padding_right (read_non_negative_length t)
  | Padding_top -> declaration Padding_top (read_non_negative_length t)
  | Padding_bottom -> declaration Padding_bottom (read_non_negative_length t)
  | Padding_inline -> declaration Padding_inline (read_non_negative_length t)
  | Padding_inline_start ->
      declaration Padding_inline_start (read_non_negative_length t)
  | Padding_inline_end ->
      declaration Padding_inline_end (read_non_negative_length t)
  | Padding_block -> declaration Padding_block (read_non_negative_length t)
  | Margin_left -> declaration Margin_left (read_margin_shorthand t)
  | Margin_right -> declaration Margin_right (read_margin_shorthand t)
  | Margin_top -> declaration Margin_top (read_margin_shorthand t)
  | Margin_bottom -> declaration Margin_bottom (read_margin_shorthand t)
  | Margin_inline -> declaration Margin_inline (read_margin_shorthand t)
  | Margin_inline_end -> declaration Margin_inline_end (read_margin_shorthand t)
  | Margin_block -> declaration Margin_block (read_margin_shorthand t)
  (* Additional color properties *)
  | Text_decoration_color -> declaration Text_decoration_color (read_color t)
  (* Text decoration style *)
  | Text_decoration_style ->
      declaration Text_decoration_style (read_text_decoration_style t)
  | Text_underline_offset -> declaration Text_underline_offset (read_string t)
  | Letter_spacing -> declaration Letter_spacing (read_length t)
  (* List properties *)
  | List_style_type -> declaration List_style_type (read_list_style_type t)
  | List_style_position ->
      declaration List_style_position (read_list_style_position t)
  | List_style_image -> declaration List_style_image (read_list_style_image t)
  (* Flexbox order *)
  | Order -> declaration Order (int_of_float (Reader.number t))
  (* Justify properties *)
  | Justify_items -> declaration Justify_items (read_justify t)
  | Justify_self -> declaration Justify_self (read_justify t)
  (* Align content *)
  | Align_content -> declaration Align_content (read_align t)
  | Align_self -> declaration Align_self (read_align_self t)
  (* Place properties *)
  | Place_content -> declaration Place_content (read_place_content t)
  | Place_items -> declaration Place_items (read_place_items t)
  | Place_self -> declaration Place_self (read_align_self t)
  (* Additional grid properties *)
  | Grid_template -> declaration Grid_template (read_grid_template t)
  | Grid_area -> declaration Grid_area (read_string t)
  | Grid_auto_columns -> declaration Grid_auto_columns (read_grid_template t)
  | Grid_auto_rows -> declaration Grid_auto_rows (read_grid_template t)
  | Grid_column -> declaration Grid_column (read_string t)
  | Grid_row -> declaration Grid_row (read_string t)
  (* Border inline/block properties *)
  | Border_inline_start_width ->
      declaration Border_inline_start_width (read_border_width t)
  | Border_inline_end_width ->
      declaration Border_inline_end_width (read_border_width t)
  | Border_inline_start_color ->
      declaration Border_inline_start_color (read_color t)
  | Border_inline_end_color ->
      declaration Border_inline_end_color (read_color t)
  (* Position properties *)
  | Top -> declaration Top (read_length t)
  | Right -> declaration Right (read_length t)
  | Bottom -> declaration Bottom (read_length t)
  | Left -> declaration Left (read_length t)
  (* Outline properties *)
  | Outline -> declaration Outline (read_string t)
  | Outline_style -> declaration Outline_style (read_outline_style t)
  | Outline_width -> declaration Outline_width (read_length t)
  | Outline_offset -> declaration Outline_offset (read_length t)
  (* Forced color adjust *)
  | Forced_color_adjust ->
      declaration Forced_color_adjust (read_forced_color_adjust t)
  (* Scroll snap *)
  | Scroll_snap_type -> declaration Scroll_snap_type (read_scroll_snap_type t)
  (* Tab size *)
  | Tab_size -> declaration Tab_size (int_of_float (Reader.number t))
  (* Webkit properties *)
  | Webkit_text_size_adjust ->
      declaration Webkit_text_size_adjust (read_text_size_adjust t)
  | Webkit_tap_highlight_color ->
      declaration Webkit_tap_highlight_color (read_color t)
  | Webkit_text_decoration ->
      declaration Webkit_text_decoration (read_text_decoration t)
  | Webkit_text_decoration_color ->
      declaration Webkit_text_decoration_color (read_color t)
  | Webkit_appearance ->
      declaration Webkit_appearance (read_webkit_appearance t)
  | Webkit_font_smoothing ->
      declaration Webkit_font_smoothing (read_webkit_font_smoothing t)
  | Webkit_line_clamp ->
      declaration Webkit_line_clamp (int_of_float (Reader.number t))
  | Webkit_box_orient ->
      declaration Webkit_box_orient (read_webkit_box_orient t)
  | Webkit_hyphens -> declaration Webkit_hyphens (read_hyphens t)
  (* Font properties *)
  | Font_feature_settings ->
      declaration Font_feature_settings (read_font_feature_settings t)
  | Font_variation_settings ->
      declaration Font_variation_settings (read_font_variation_settings t)
  | Font_stretch -> declaration Font_stretch (read_font_stretch t)
  | Font_variant_numeric ->
      declaration Font_variant_numeric (read_font_variant_numeric t)
  | Font -> declaration Font (read_string t)
  (* Text properties *)
  | Text_indent -> declaration Text_indent (read_length t)
  | Text_overflow -> declaration Text_overflow (read_text_overflow t)
  | Text_wrap -> declaration Text_wrap (read_text_wrap t)
  | Text_decoration_thickness ->
      declaration Text_decoration_thickness (read_length t)
  | Text_size_adjust -> declaration Text_size_adjust (read_string t)
  | Text_decoration_skip_ink ->
      declaration Text_decoration_skip_ink (read_text_decoration_skip_ink t)
  (* Word/text breaking *)
  | Word_break -> declaration Word_break (read_word_break t)
  | Overflow_wrap -> declaration Overflow_wrap (read_overflow_wrap t)
  | Hyphens -> declaration Hyphens (read_hyphens t)
  | Word_spacing -> declaration Word_spacing (read_length t)
  (* List style *)
  | List_style -> declaration List_style (read_string t)
  (* Container properties *)
  | Container_type -> declaration Container_type (read_container_type t)
  | Container_name -> declaration Container_name (read_string t)
  (* Transform properties *)
  | Perspective -> declaration Perspective (read_length t)
  | Perspective_origin -> declaration Perspective_origin (read_string t)
  | Transform_style -> declaration Transform_style (read_transform_style t)
  | Backface_visibility ->
      declaration Backface_visibility (read_backface_visibility t)
  | Rotate -> declaration Rotate (read_angle t)
  | Scale -> declaration Scale (read_scale t)
  (* Object properties *)
  | Object_position -> declaration Object_position (read_position_2d t)
  | Object_fit -> declaration Object_fit (read_object_fit t)
  (* Transition properties *)
  | Transition_duration -> declaration Transition_duration (read_duration t)
  | Transition_timing_function ->
      declaration Transition_timing_function (read_timing_function t)
  | Transition_delay -> declaration Transition_delay (read_duration t)
  (* Will change *)
  | Will_change -> declaration Will_change (read_string t)
  (* Contain and isolation *)
  | Contain -> declaration Contain (read_contain t)
  | Isolation -> declaration Isolation (read_isolation t)
  (* Background properties *)
  | Background_attachment ->
      declaration Background_attachment (read_background_attachment t)
  | Background_position ->
      declaration Background_position
        (Reader.list ~sep:Reader.comma read_position_2d t)
  | Background_repeat ->
      declaration Background_repeat (read_background_repeat t)
  | Background_size -> declaration Background_size (read_background_size t)
  | Background_blend_mode ->
      declaration Background_blend_mode
        (Reader.list ~sep:Reader.comma read_blend_mode t)
  (* Border shorthands *)
  | Border_top -> declaration Border_top (read_string t)
  | Border_right -> declaration Border_right (read_string t)
  | Border_bottom -> declaration Border_bottom (read_string t)
  | Border_left -> declaration Border_left (read_string t)
  | Border_spacing -> declaration Border_spacing (read_length t)
  | Border_collapse -> declaration Border_collapse (read_border_collapse t)
  (* Clip and mask *)
  | Clip_path -> declaration Clip_path (read_string t)
  | Mask -> declaration Mask (read_string t)
  | Clip -> declaration Clip (read_string t)
  (* Content visibility *)
  | Content_visibility ->
      declaration Content_visibility (read_content_visibility t)
  (* Aspect ratio *)
  | Aspect_ratio -> declaration Aspect_ratio (read_aspect_ratio t)
  (* Vertical align *)
  | Vertical_align -> declaration Vertical_align (read_vertical_align t)
  (* Moz properties *)
  | Moz_osx_font_smoothing ->
      declaration Moz_osx_font_smoothing (read_moz_osx_font_smoothing t)
  (* Backdrop filter *)
  | Backdrop_filter -> declaration Backdrop_filter (read_filter t)
  (* Scroll properties *)
  | Scroll_snap_align ->
      declaration Scroll_snap_align (read_scroll_snap_align t)
  | Scroll_snap_stop -> declaration Scroll_snap_stop (read_scroll_snap_stop t)
  | Scroll_behavior -> declaration Scroll_behavior (read_scroll_behavior t)
  | Scroll_margin -> declaration Scroll_margin (read_length t)
  | Scroll_margin_top -> declaration Scroll_margin_top (read_length t)
  | Scroll_margin_right -> declaration Scroll_margin_right (read_length t)
  | Scroll_margin_bottom -> declaration Scroll_margin_bottom (read_length t)
  | Scroll_margin_left -> declaration Scroll_margin_left (read_length t)
  | Scroll_padding -> declaration Scroll_padding (read_length t)
  | Scroll_padding_top -> declaration Scroll_padding_top (read_length t)
  | Scroll_padding_right -> declaration Scroll_padding_right (read_length t)
  | Scroll_padding_bottom -> declaration Scroll_padding_bottom (read_length t)
  | Scroll_padding_left -> declaration Scroll_padding_left (read_length t)
  | Overscroll_behavior ->
      declaration Overscroll_behavior (read_overscroll_behavior t)
  | Overscroll_behavior_x ->
      declaration Overscroll_behavior_x (read_overscroll_behavior t)
  | Overscroll_behavior_y ->
      declaration Overscroll_behavior_y (read_overscroll_behavior t)
  (* Quotes *)
  | Quotes -> declaration Quotes (read_string t)
  (* Touch action *)
  | Touch_action -> declaration Touch_action (read_touch_action t)
  (* Clear and float *)
  | Clear -> declaration Clear (read_clear t)
  | Float -> declaration Float (read_float_side t)
  (* SVG properties *)
  | Fill -> declaration Fill (read_svg_paint t)
  | Stroke -> declaration Stroke (read_svg_paint t)
  | Stroke_width -> declaration Stroke_width (read_length t)
  (* Direction and writing *)
  | Direction -> declaration Direction (read_direction t)
  | Unicode_bidi -> declaration Unicode_bidi (read_unicode_bidi t)
  | Writing_mode -> declaration Writing_mode (read_writing_mode t)
  (* Animation properties *)
  | Animation_name -> declaration Animation_name (read_string t)
  | Animation_duration -> declaration Animation_duration (read_duration t)
  | Animation_timing_function ->
      declaration Animation_timing_function (read_timing_function t)
  | Animation_delay -> declaration Animation_delay (read_duration t)
  | Animation_iteration_count ->
      declaration Animation_iteration_count (read_animation_iteration_count t)
  | Animation_direction ->
      declaration Animation_direction (read_animation_direction t)
  | Animation_fill_mode ->
      declaration Animation_fill_mode (read_animation_fill_mode t)
  | Animation_play_state ->
      declaration Animation_play_state (read_animation_play_state t)
  (* Color properties *)
  | Accent_color -> declaration Accent_color (read_color t)
  | Caret_color -> declaration Caret_color (read_color t)
  (* Mix blend mode *)
  | Mix_blend_mode -> declaration Mix_blend_mode (read_blend_mode t)
  (* Table layout *)
  | Table_layout -> declaration Table_layout (read_table_layout t)

(** Parse a single declaration directly from stream - no string roundtrips *)
let read_declaration t : declaration option =
  Reader.ws t;
  match Reader.peek t with
  | Some '}' | None -> None
  | _ -> (
      Reader.with_context t "read_declaration" @@ fun () ->
      try
        (* Check if this is a custom property (starts with --) *)
        if Reader.looking_at t "--" then (
          let name = read_property_name t in
          Reader.ws t;
          Reader.expect ':' t;
          Reader.ws t;
          let value_str = read_property_value t in
          let is_important = read_importance t in
          let decl = custom_property name value_str in
          Some (if is_important then important decl else decl))
        else
          let (Prop prop_type) = read_property t in
          Reader.ws t;
          Reader.expect ':' t;
          Reader.ws t;

          let decl = read_value prop_type t in
          let is_important = read_importance t in
          Reader.ws t;
          (match Reader.peek t with
          | Some '!' -> Reader.err_invalid t "duplicate !important"
          | _ -> ());
          Some (if is_important then important decl else decl)
      with
      | Failure msg ->
          (* Handle property parsing errors *)
          Reader.err_invalid t msg
      | Invalid_argument msg ->
          (* Normalize invalid_arg into a structured parse error with context *)
          Reader.err_invalid t msg)

let read_declarations t =
  Reader.with_context t "declarations" @@ fun () ->
  let rec loop acc =
    Reader.ws t;
    match Reader.peek t with
    | Some '}' | None -> List.rev acc
    | _ -> (
        match read_declaration t with
        | None -> List.rev acc
        | Some decl -> (
            let acc = decl :: acc in
            (* After reading a declaration, check for proper separation *)
            Reader.ws t;
            match Reader.peek t with
            | Some '}' -> List.rev acc (* End of block, no semicolon needed *)
            | Some ';' ->
                Reader.expect ';' t;
                loop acc
            | None -> List.rev acc (* End of input *)
            | _ ->
                (* Check if we have more tokens that look like a new
                   declaration *)
                if
                  Reader.is_ident_start
                    (Option.value (Reader.peek t) ~default:' ')
                then Reader.err t "missing semicolon between declarations"
                else
                  (* Some other character - let the next iteration handle it *)
                  List.rev acc))
  in
  loop []

let read_block t =
  Reader.ws t;
  Reader.expect '{' t;
  Reader.ws t;
  let decls = read_declarations t in
  Reader.ws t;
  Reader.expect '}' t;
  decls

(* Pretty printer for declarations *)
let pp_declaration : declaration Pp.t =
 fun ctx -> function
  | Declaration { property; value; important } ->
      pp_property ctx property;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      pp_property_value ctx (property, value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")
  | Custom_declaration { name; kind; value; important; _ } ->
      Pp.string ctx name;
      Pp.string ctx ":";
      Pp.space_if_pretty ctx ();
      (* Pretty-print custom property value by kind *)
      (match kind with
      | String -> Pp.string ctx value
      | Length -> pp_length ctx value
      | Color -> pp_color ctx value
      | Int -> Pp.int ctx value
      | Float -> Pp.float ctx value
      | Duration -> pp_duration ctx value
      | Angle -> pp_angle ctx value
      | Shadow -> pp_shadow ctx value
      | Box_shadow -> pp_box_shadow ctx value
      | Content -> pp_content ctx value
      | Font_family -> Pp.list ~sep:Pp.comma pp_font_family ctx value
      | Font_weight -> pp_font_weight ctx value
      | Font_feature_settings -> pp_font_feature_settings ctx value
      | Font_variation_settings -> pp_font_variation_settings ctx value
      | Font_variant_numeric -> pp_font_variant_numeric ctx value
      | Font_variant_numeric_token -> pp_font_variant_numeric_token ctx value
      | Border_style -> pp_border_style ctx value
      | Border -> pp_border ctx value
      | Blend_mode -> pp_blend_mode ctx value
      | Scroll_snap_strictness -> pp_scroll_snap_strictness ctx value
      | Aspect_ratio -> pp_aspect_ratio ctx value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")

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
    pp_grid_line ctx start;
    Pp.string ctx " / ";
    pp_grid_line ctx end_
  in
  declaration Grid_row (Pp.to_string pp ())

let grid_column (start, end_) =
  let pp ctx () =
    pp_grid_line ctx start;
    Pp.string ctx " / ";
    pp_grid_line ctx end_
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

let border ?width ?style ?color () =
  let border_value : border =
    match (width, style, color) with
    | None, None, None -> None
    | _ -> Border { width; style; color }
  in
  declaration Border border_value

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
