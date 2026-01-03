(** CSS declaration types and parser. *)

include Declaration_intf
open Properties
open Values

(* Re-export pp_property from Properties module *)
let pp_property = pp_property

(* Extract metadata from a declaration *)
let meta_of_declaration : declaration -> meta option = function
  | Custom_declaration { meta; _ } -> meta
  | Declaration _ -> None

(* Smart constructor for declarations *)
let v ?(important = false) property value =
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
  let rec handle_escape buf t depth in_quote quote_char =
    match Reader.peek t with
    | None -> Buffer.contents buf
    | Some next_c ->
        Buffer.add_char buf next_c;
        Reader.skip t;
        parse_tokens buf t depth in_quote quote_char
  and parse_tokens buf t depth in_quote quote_char =
    match Reader.peek t with
    | None -> Buffer.contents buf
    | Some c when in_quote ->
        Buffer.add_char buf c;
        Reader.skip t;
        if c = quote_char then parse_tokens buf t depth false '\000'
        else if c = '\\' then handle_escape buf t depth in_quote quote_char
        else parse_tokens buf t depth in_quote quote_char
    | Some (('"' | '\'') as q) ->
        Buffer.add_char buf q;
        Reader.skip t;
        parse_tokens buf t depth true q
    | Some (('(' | '[' | '{') as c) ->
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens buf t (depth + 1) in_quote quote_char
    | Some ((')' | ']' | '}') as c) when depth > 0 ->
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens buf t (depth - 1) in_quote quote_char
    | Some c when depth = 0 && (c = ';' || c = '}' || c = '!') ->
        Buffer.contents buf
    | Some c when depth = 0 && Reader.is_ident_start c ->
        (* Continue parsing as normal character - don't check for property
           declarations within values *)
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens buf t depth in_quote quote_char
    | Some c ->
        Buffer.add_char buf c;
        Reader.skip t;
        parse_tokens buf t depth in_quote quote_char
  in
  let value = parse_tokens buf t 0 false '\000' in
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
  | Length -> pp (pp_length ~always:true)
  | Color -> pp pp_color
  | Rgb ->
      let rec pp_rgb_type : rgb Pp.t =
       fun ctx rgb ->
        match rgb with
        | Channels { r; g; b } ->
            (* Just output the RGB values without wrapper *)
            pp_channel ctx r;
            Pp.space ctx ();
            pp_channel ctx g;
            Pp.space ctx ();
            pp_channel ctx b
        | Var v -> pp_var pp_rgb_type ctx v
      in
      pp pp_rgb_type
  | Int -> pp Pp.int
  | Float -> pp Pp.float
  | Percentage -> pp pp_percentage
  | Number_percentage -> pp pp_number_percentage
  | String -> pp Pp.string
  | Shadow -> pp pp_shadow
  | Duration -> pp pp_duration
  | Aspect_ratio -> pp pp_aspect_ratio
  | Border_style -> pp pp_border_style
  | Outline_style -> pp pp_outline_style
  | Border -> pp pp_border
  | Font_weight -> pp pp_font_weight
  | Line_height -> pp pp_line_height
  | Font_family -> pp pp_font_family
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Font_variant_numeric -> pp pp_font_variant_numeric
  | Font_variant_numeric_token -> pp pp_font_variant_numeric_token
  | Blend_mode -> pp pp_blend_mode
  | Scroll_snap_strictness -> pp pp_scroll_snap_strictness
  | Angle -> pp pp_angle
  | Box_shadow -> pp pp_shadow
  | Content -> pp pp_content
  | Gradient_stop -> pp pp_gradient_stop
  | Gradient_direction -> pp pp_gradient_direction
  | Animation -> pp pp_animation
  | Timing_function -> pp pp_timing_function
  | Transform -> pp pp_transform

let string_of_value ?(minify = true) ?(inline = false) decl =
  let ctx = { Pp.minify; indent = 0; buf = Buffer.create 16; inline } in
  match decl with
  | Declaration { property; value; _ } ->
      pp_property_value ctx (property, value);
      Buffer.contents ctx.buf
  | Custom_declaration { kind; value; _ } ->
      pp_value ctx (kind, value);
      Buffer.contents ctx.buf

(* Helper to read a trimmed string *)
let read_string t = Reader.string ~trim:true t

(* Helper to validate no extra tokens remain *)
let validate_no_extra_tokens t =
  Reader.ws t;
  match Reader.peek t with
  | Some '!' | Some ';' | Some '}' | None -> ()
  | Some _ ->
      let remaining = Reader.css_value ~stops:[ ';'; '}'; '!' ] t in
      let trimmed = String.trim remaining in
      if trimmed <> "" then
        Reader.err_invalid t
          ("unexpected tokens after property value: " ^ trimmed)

(* Custom parser for grid-template-areas: reads multiple quoted strings *)
let read_grid_template_areas t =
  let rec read_strings acc =
    Reader.ws t;
    match Reader.peek t with
    | Some (';' | '}' | '!') | None -> String.concat " " (List.rev acc)
    | _ ->
        let s = Reader.string t in
        let quoted_s = "\"" ^ s ^ "\"" in
        read_strings (quoted_s :: acc)
  in
  read_strings []

(* Custom parser for grid-template-columns/rows: handles both single values and
   lists *)
let read_grid_template_list t =
  let first_value = read_grid_template t in
  Reader.ws t;
  (* Try to read more values - if none, it's a single value *)
  let remaining_values =
    Reader.list ~sep:(fun t -> Reader.ws t) ~at_least:0 read_grid_template t
  in
  if remaining_values = [] then
    (* Single value (e.g., "none", "repeat(3, 1fr)", "1fr") *)
    first_value
  else
    (* Multiple values (e.g., "100px 200px", "1fr 2fr") *)
    let all_values = first_value :: remaining_values in
    Tracks all_values

(* Helper to read animation-name: none | <custom-ident> *)
let read_animation_name t =
  if Reader.looking_at t "none" then (
    Reader.expect_string "none" t;
    Reader.ws t;
    "none")
  else Reader.ident t

(* Helper to read opacity: accepts either <number> (0-1) or <percentage>
   (0%-100%) Both formats are valid per CSS spec. Tailwind v4 outputs
   percentages. *)
let read_opacity t =
  Reader.ws t;
  let n = Reader.number t in
  (* Check if followed by %, indicating percentage *)
  match Reader.peek t with
  | Some '%' ->
      Reader.expect '%' t;
      n /. 100.0 (* Convert 100% -> 1.0, 0% -> 0.0 *)
  | _ -> n (* Already in 0-1 range *)

(* Helper to read raw property value - for properties that accept any text *)
let read_raw_value t =
  (* Read characters until we hit a semicolon, closing brace, or !important *)
  let buffer = Buffer.create 64 in
  let rec loop () =
    match Reader.peek t with
    | Some ';' | Some '}' | None -> Buffer.contents buffer |> String.trim
    | Some '!' ->
        (* Look ahead to see if this is !important *)
        Buffer.add_char buffer '!';
        Reader.expect '!' t;
        if Reader.looking_at t "important" then
          (* This is !important, stop reading the value *)
          String.trim
            (String.sub (Buffer.contents buffer) 0 (Buffer.length buffer - 1))
        else loop ()
    | Some c ->
        Reader.expect c t;
        Buffer.add_char buffer c;
        loop ()
  in
  loop ()

(* Stub reader for translate_value - we primarily generate CSS, not parse it *)
let read_translate_value t : Properties_intf.translate_value =
  (* For now, just consume the value and return a None placeholder *)
  ignore (read_raw_value t);
  None

let read_transform_value t = v Transform (read_transforms t)
let read_webkit_transform_value t = v Webkit_transform (read_transforms t)

let read_place_self_value t =
  let a = read_align_self t in
  Reader.ws t;
  let j = Reader.option read_justify_self t in
  let pair =
    match j with None -> (a, (Center : justify_self)) | Some jj -> (a, jj)
  in
  v Place_self pair

let read_background_blend_mode_value t =
  v Background_blend_mode (Reader.list ~sep:Reader.comma read_blend_mode t)

let prop_name (type a) (prop_type : a property) =
  let buf = Buffer.create 32 in
  let ctx = { Pp.minify = true; indent = 0; buf; inline = false } in
  pp_property ctx prop_type;
  Buffer.contents buf

let read_value (type a) (prop : a property) t : declaration =
  Reader.with_context t (prop_name prop) @@ fun () ->
  match prop with
  | Color -> v Color (read_color t)
  | Background_color -> v Background_color (read_color t)
  | Border_color -> v Border_color (read_color t)
  | Outline_color -> v Outline_color (read_color t)
  | Border_top_color -> v Border_top_color (read_color t)
  | Border_right_color -> v Border_right_color (read_color t)
  | Border_bottom_color -> v Border_bottom_color (read_color t)
  | Border_left_color -> v Border_left_color (read_color t)
  (* Length/percentage properties *)
  | Width -> v Width (read_length_percentage t)
  | Height -> v Height (read_length_percentage t)
  | Min_width -> v Min_width (read_length_percentage t)
  | Min_height -> v Min_height (read_length_percentage t)
  | Max_width -> v Max_width (read_length_percentage t)
  | Max_height -> v Max_height (read_length_percentage t)
  | Font_size -> v Font_size (read_length_percentage t)
  | Border_radius -> v Border_radius (read_length t)
  | Border_top_left_radius -> v Border_top_left_radius (read_length t)
  | Border_top_right_radius -> v Border_top_right_radius (read_length t)
  | Border_bottom_left_radius -> v Border_bottom_left_radius (read_length t)
  | Border_bottom_right_radius -> v Border_bottom_right_radius (read_length t)
  | Gap -> v Gap (Properties.read_gap t)
  | Column_gap -> v Column_gap (read_length t)
  | Row_gap -> v Row_gap (read_length t)
  (* Display and layout *)
  | Display -> v Display (read_display t)
  | Position -> v Position (read_position t)
  | Visibility -> v Visibility (read_visibility t)
  | Overflow -> v Overflow (read_overflow t)
  | Overflow_x -> v Overflow_x (read_overflow t)
  | Overflow_y -> v Overflow_y (read_overflow t)
  (* Padding/Margin *)
  | Padding -> v Padding (read_padding_shorthand t)
  | Margin -> v Margin (read_margin_shorthand t)
  (* Border styles *)
  | Border_style -> v Border_style (read_border_style t)
  | Border_width -> v Border_width (read_border_width t)
  | Border_top_width -> v Border_top_width (read_border_width t)
  | Border_right_width -> v Border_right_width (read_border_width t)
  | Border_bottom_width -> v Border_bottom_width (read_border_width t)
  | Border_left_width -> v Border_left_width (read_border_width t)
  (* Typography *)
  | Line_height -> v Line_height (read_line_height t)
  | Font_weight -> v Font_weight (read_font_weight t)
  | Font_style -> v Font_style (read_font_style t)
  | Font_family -> v Font_family (read_font_family t)
  | Font -> v Font (read_raw_value t)
  | Text_align -> v Text_align (read_text_align t)
  | Text_transform -> v Text_transform (read_text_transform t)
  | White_space -> v White_space (read_white_space t)
  | Text_decoration -> v Text_decoration (read_text_decoration t)
  | Transform_origin -> v Transform_origin (read_transform_origin t)
  (* Flexbox *)
  | Flex_direction -> v Flex_direction (read_flex_direction t)
  | Flex_wrap -> v Flex_wrap (read_flex_wrap t)
  | Flex -> v Flex (read_flex t)
  | Flex_grow -> v Flex_grow (Reader.number t)
  | Flex_shrink -> v Flex_shrink (Reader.number t)
  | Flex_basis -> v Flex_basis (read_length t)
  | Align_items -> v Align_items (read_align_items t)
  | Justify_content -> v Justify_content (read_justify_content t)
  (* Transform property *)
  | Transform -> read_transform_value t
  | Translate -> v Translate (read_translate_value t)
  (* Webkit Transform *)
  | Webkit_transform -> read_webkit_transform_value t
  (* Webkit Transition *)
  | Webkit_transition -> v Webkit_transition (read_transitions t)
  (* Webkit Filter *)
  | Webkit_filter -> v Webkit_filter (read_filter t)
  (* Moz Appearance *)
  | Moz_appearance -> v Moz_appearance (read_appearance t)
  (* Ms Filter *)
  | Ms_filter -> v Ms_filter (read_filter t)
  (* O Transition *)
  | O_transition -> v O_transition (read_transitions t)
  (* Filter *)
  | Filter -> v Filter (read_filter t)
  (* Appearance *)
  | Appearance -> v Appearance (read_appearance t)
  (* Background *)
  | Background_image ->
      let images = read_background_images t in
      v Background_image images
  | Background -> v Background (read_backgrounds t)
  | Border -> v Border (read_border t)
  (* Grid properties *)
  | Grid_template_columns -> v Grid_template_columns (read_grid_template_list t)
  | Grid_template_rows -> v Grid_template_rows (read_grid_template_list t)
  | Grid_row_start -> v Grid_row_start (read_grid_line t)
  | Grid_row_end -> v Grid_row_end (read_grid_line t)
  | Grid_column_start -> v Grid_column_start (read_grid_line t)
  | Grid_column_end -> v Grid_column_end (read_grid_line t)
  | Grid_auto_flow -> v Grid_auto_flow (read_grid_auto_flow t)
  | Grid_template_areas -> v Grid_template_areas (read_grid_template_areas t)
  (* Shadows *)
  | Box_shadow -> v Box_shadow (read_shadow t)
  | Text_shadow -> v Text_shadow (read_text_shadows t)
  (* Content *)
  | Content -> v Content (read_content t)
  (* Other properties *)
  | Z_index -> v Z_index (Properties.read_z_index t)
  | Opacity -> v Opacity (read_opacity t)
  | Cursor -> v Cursor (read_cursor t)
  | Box_sizing -> v Box_sizing (read_box_sizing t)
  | User_select -> v User_select (read_user_select t)
  | Webkit_user_select -> v Webkit_user_select (read_user_select t)
  | Pointer_events -> v Pointer_events (read_pointer_events t)
  | Resize -> v Resize (read_resize t)
  | Transition -> v Transition (read_transitions t)
  | Animation -> v Animation (read_animations t)
  (* Border style properties *)
  | Border_top_style -> v Border_top_style (read_border_style t)
  | Border_right_style -> v Border_right_style (read_border_style t)
  | Border_bottom_style -> v Border_bottom_style (read_border_style t)
  | Border_left_style -> v Border_left_style (read_border_style t)
  (* Additional margin/padding properties *)
  | Padding_left -> v Padding_left (read_non_negative_length t)
  | Padding_right -> v Padding_right (read_non_negative_length t)
  | Padding_top -> v Padding_top (read_non_negative_length t)
  | Padding_bottom -> v Padding_bottom (read_non_negative_length t)
  | Padding_inline -> v Padding_inline (read_non_negative_length t)
  | Padding_inline_start -> v Padding_inline_start (read_non_negative_length t)
  | Padding_inline_end -> v Padding_inline_end (read_non_negative_length t)
  | Padding_block -> v Padding_block (read_non_negative_length t)
  | Margin_left -> v Margin_left (read_length t)
  | Margin_right -> v Margin_right (read_length t)
  | Margin_top -> v Margin_top (read_length t)
  | Margin_bottom -> v Margin_bottom (read_length t)
  | Margin_inline -> v Margin_inline (read_length t)
  | Margin_inline_start -> v Margin_inline_start (read_length t)
  | Margin_inline_end -> v Margin_inline_end (read_length t)
  | Margin_block -> v Margin_block (read_length t)
  | Margin_block_start -> v Margin_block_start (read_length t)
  | Margin_block_end -> v Margin_block_end (read_length t)
  (* Additional color properties *)
  | Text_decoration_color -> v Text_decoration_color (read_color t)
  (* Text decoration line and style *)
  | Text_decoration_line -> v Text_decoration_line (read_text_decoration_line t)
  | Text_decoration_style ->
      v Text_decoration_style (read_text_decoration_style t)
  | Text_underline_offset -> v Text_underline_offset (read_length t)
  | Letter_spacing -> v Letter_spacing (read_length t)
  (* List properties *)
  | List_style_type -> v List_style_type (read_list_style_type t)
  | List_style_position -> v List_style_position (read_list_style_position t)
  | List_style_image -> v List_style_image (read_list_style_image t)
  | List_style -> v List_style (read_raw_value t)
  (* Flexbox order *)
  | Order -> v Order (int_of_float (Reader.number t))
  (* Justify properties *)
  | Justify_items -> v Justify_items (read_justify_items t)
  | Justify_self -> v Justify_self (read_justify_self t)
  (* Align content *)
  | Align_content -> v Align_content (read_align_content t)
  | Align_self -> v Align_self (read_align_self t)
  (* Place properties *)
  | Place_content -> v Place_content (read_place_content t)
  | Place_items -> v Place_items (read_place_items t)
  | Place_self -> read_place_self_value t
  (* Additional grid properties *)
  | Grid_template -> v Grid_template (read_grid_template t)
  | Grid_area -> v Grid_area (read_raw_value t)
  | Grid_auto_columns -> v Grid_auto_columns (read_grid_template t)
  | Grid_auto_rows -> v Grid_auto_rows (read_grid_template t)
  | Grid_column -> v Grid_column (read_raw_value t)
  | Grid_row -> v Grid_row (read_raw_value t)
  (* Border inline/block properties *)
  | Border_inline_start_width ->
      v Border_inline_start_width (read_border_width t)
  | Border_inline_end_width -> v Border_inline_end_width (read_border_width t)
  | Border_inline_start_color -> v Border_inline_start_color (read_color t)
  | Border_inline_end_color -> v Border_inline_end_color (read_color t)
  (* Position properties *)
  | Inset -> v Inset (read_length t)
  | Top -> v Top (read_length t)
  | Right -> v Right (read_length t)
  | Bottom -> v Bottom (read_length t)
  | Left -> v Left (read_length t)
  (* Outline properties *)
  | Outline -> v Outline (read_outline t)
  | Outline_style -> v Outline_style (read_outline_style t)
  | Outline_width -> v Outline_width (read_length t)
  | Outline_offset -> v Outline_offset (read_length t)
  (* Forced color adjust *)
  | Forced_color_adjust -> v Forced_color_adjust (read_forced_color_adjust t)
  (* Scroll snap *)
  | Scroll_snap_type -> v Scroll_snap_type (read_scroll_snap_type t)
  (* Tab size *)
  | Tab_size -> v Tab_size (int_of_float (Reader.number t))
  (* Webkit properties *)
  | Webkit_text_size_adjust ->
      v Webkit_text_size_adjust (read_text_size_adjust t)
  | Webkit_tap_highlight_color -> v Webkit_tap_highlight_color (read_color t)
  | Webkit_text_decoration -> v Webkit_text_decoration (read_text_decoration t)
  | Webkit_text_decoration_color ->
      v Webkit_text_decoration_color (read_color t)
  | Webkit_appearance -> v Webkit_appearance (read_webkit_appearance t)
  | Webkit_font_smoothing ->
      v Webkit_font_smoothing (read_webkit_font_smoothing t)
  | Webkit_line_clamp -> v Webkit_line_clamp (int_of_float (Reader.number t))
  | Webkit_box_orient -> v Webkit_box_orient (read_webkit_box_orient t)
  | Webkit_hyphens -> v Webkit_hyphens (read_hyphens t)
  (* Font properties *)
  | Font_feature_settings ->
      v Font_feature_settings (read_font_feature_settings t)
  | Font_variation_settings ->
      v Font_variation_settings (read_font_variation_settings t)
  | Font_stretch -> v Font_stretch (read_font_stretch t)
  | Font_variant_numeric -> v Font_variant_numeric (read_font_variant_numeric t)
  (* Text properties *)
  | Text_indent -> v Text_indent (read_length t)
  | Text_overflow -> v Text_overflow (read_text_overflow t)
  | Text_wrap -> v Text_wrap (read_text_wrap t)
  | Text_decoration_thickness -> v Text_decoration_thickness (read_length t)
  | Text_size_adjust -> v Text_size_adjust (read_text_size_adjust t)
  | Text_decoration_skip_ink ->
      v Text_decoration_skip_ink (read_text_decoration_skip_ink t)
  (* Word/text breaking *)
  | Word_break -> v Word_break (read_word_break t)
  | Overflow_wrap -> v Overflow_wrap (read_overflow_wrap t)
  | Hyphens -> v Hyphens (read_hyphens t)
  | Word_spacing -> v Word_spacing (read_length t)
  (* Container properties *)
  | Container_type -> v Container_type (read_container_type t)
  | Container_name -> v Container_name (read_raw_value t)
  | Container -> v Container (read_container_shorthand t)
  (* Transform properties *)
  | Perspective -> v Perspective (read_length t)
  | Perspective_origin -> v Perspective_origin (read_perspective_origin t)
  | Transform_style -> v Transform_style (read_transform_style t)
  | Backface_visibility -> v Backface_visibility (read_backface_visibility t)
  | Rotate -> v Rotate (read_angle t)
  | Scale -> v Scale (read_scale t)
  (* Object properties *)
  | Object_position -> v Object_position (read_position_value t)
  | Object_fit -> v Object_fit (read_object_fit t)
  (* Transition properties *)
  | Transition_duration -> v Transition_duration (read_duration t)
  | Transition_timing_function ->
      v Transition_timing_function (read_timing_function t)
  | Transition_delay -> v Transition_delay (read_time t)
  | Transition_property -> v Transition_property (read_transition_property t)
  | Transition_behavior ->
      v Transition_behavior (Properties.read_transition_behavior t)
  (* Will change *)
  | Will_change -> v Will_change (read_will_change t)
  (* Contain and isolation *)
  | Contain -> v Contain (read_contain t)
  | Isolation -> v Isolation (read_isolation t)
  (* Background properties *)
  | Background_attachment ->
      v Background_attachment (read_background_attachment t)
  | Background_origin -> v Background_origin (read_background_box t)
  | Background_position -> v Background_position (read_background_position t)
  | Background_repeat -> v Background_repeat (read_background_repeat t)
  | Background_size -> v Background_size (read_background_size t)
  | Background_blend_mode -> read_background_blend_mode_value t
  (* Border shorthands *)
  | Border_top -> v Border_top (read_string t)
  | Border_right -> v Border_right (read_string t)
  | Border_bottom -> v Border_bottom (read_string t)
  | Border_left -> v Border_left (read_string t)
  | Border_spacing -> v Border_spacing (read_length t)
  | Border_collapse -> v Border_collapse (read_border_collapse t)
  (* Clip and mask *)
  | Clip_path -> v Clip_path (read_clip_path t)
  | Mask -> v Mask (read_string t)
  | Clip -> v Clip (read_clip t)
  (* Content visibility *)
  | Content_visibility -> v Content_visibility (read_content_visibility t)
  (* Aspect ratio *)
  | Aspect_ratio -> v Aspect_ratio (read_aspect_ratio t)
  (* Vertical align *)
  | Vertical_align -> v Vertical_align (read_vertical_align t)
  (* Moz properties *)
  | Moz_osx_font_smoothing ->
      v Moz_osx_font_smoothing (read_moz_osx_font_smoothing t)
  (* Backdrop filter *)
  | Backdrop_filter -> v Backdrop_filter (read_filter t)
  | Webkit_backdrop_filter -> v Webkit_backdrop_filter (read_filter t)
  (* Scroll properties *)
  | Scroll_snap_align -> v Scroll_snap_align (read_scroll_snap_align t)
  | Scroll_snap_stop -> v Scroll_snap_stop (read_scroll_snap_stop t)
  | Scroll_behavior -> v Scroll_behavior (read_scroll_behavior t)
  | Scroll_margin -> v Scroll_margin (read_length t)
  | Scroll_margin_top -> v Scroll_margin_top (read_length t)
  | Scroll_margin_right -> v Scroll_margin_right (read_length t)
  | Scroll_margin_bottom -> v Scroll_margin_bottom (read_length t)
  | Scroll_margin_left -> v Scroll_margin_left (read_length t)
  | Scroll_padding -> v Scroll_padding (read_length t)
  | Scroll_padding_top -> v Scroll_padding_top (read_length t)
  | Scroll_padding_right -> v Scroll_padding_right (read_length t)
  | Scroll_padding_bottom -> v Scroll_padding_bottom (read_length t)
  | Scroll_padding_left -> v Scroll_padding_left (read_length t)
  | Overscroll_behavior -> v Overscroll_behavior (read_overscroll_behavior t)
  | Overscroll_behavior_x ->
      v Overscroll_behavior_x (read_overscroll_behavior t)
  | Overscroll_behavior_y ->
      v Overscroll_behavior_y (read_overscroll_behavior t)
  (* Quotes *)
  | Quotes -> v Quotes (read_quotes t)
  (* Touch action *)
  | Touch_action -> v Touch_action (read_touch_action t)
  (* Clear and float *)
  | Clear -> v Clear (read_clear t)
  | Float -> v Float (read_float_side t)
  (* SVG properties *)
  | Fill -> v Fill (read_svg_paint t)
  | Stroke -> v Stroke (read_svg_paint t)
  | Stroke_width -> v Stroke_width (read_length t)
  (* Direction and writing *)
  | Direction -> v Direction (read_direction t)
  | Unicode_bidi -> v Unicode_bidi (read_unicode_bidi t)
  | Writing_mode -> v Writing_mode (read_writing_mode t)
  (* Animation properties *)
  | Animation_name -> v Animation_name (read_animation_name t)
  | Animation_duration -> v Animation_duration (read_duration t)
  | Animation_timing_function ->
      v Animation_timing_function (read_timing_function t)
  | Animation_delay -> v Animation_delay (read_time t)
  | Animation_iteration_count ->
      v Animation_iteration_count (read_animation_iteration_count t)
  | Animation_direction -> v Animation_direction (read_animation_direction t)
  | Animation_fill_mode -> v Animation_fill_mode (read_animation_fill_mode t)
  | Animation_play_state -> v Animation_play_state (read_animation_play_state t)
  (* Color properties *)
  | Accent_color -> v Accent_color (read_color t)
  | Caret_color -> v Caret_color (read_color t)
  (* Mix blend mode *)
  | Mix_blend_mode -> v Mix_blend_mode (read_blend_mode t)
  (* Table layout *)
  | Table_layout -> v Table_layout (read_table_layout t)
  (* Print color adjust *)
  | Print_color_adjust -> v Print_color_adjust (read_print_color_adjust t)

(** Parse a custom property (--name: value) *)
let read_custom_property_declaration t : declaration =
  let name = read_property_name t in
  Reader.ws t;
  Reader.expect ':' t;
  Reader.ws t;
  let value_str = read_property_value t in
  let is_important = read_importance t in
  (* custom_property may raise Failure for invalid names like "--" *)
  try
    let decl = custom_property name value_str in
    if is_important then important decl else decl
  with Failure msg -> Reader.err_invalid t msg

(** Parse a regular property (name: value) *)
let read_regular_property_declaration t : declaration =
  let (Prop prop_type) = read_any_property t in
  Reader.ws t;
  Reader.expect ':' t;
  Reader.ws t;
  let decl = read_value prop_type t in
  validate_no_extra_tokens t;
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  (match Reader.peek t with
  | Some '!' -> Reader.err_invalid t "duplicate !important"
  | _ -> ());
  if is_important then important decl else decl

(** Parse a single declaration directly from stream - no string roundtrips *)
let read_declaration t : declaration option =
  let read_one () =
    Reader.with_context t "read_declaration" @@ fun () ->
    (* Check if this is a custom property (starts with --) *)
    if Reader.looking_at t "--" then read_custom_property_declaration t
    else read_regular_property_declaration t
  in
  Reader.ws t;
  match Reader.peek t with
  | Some '}' -> None (* End of block - no more declarations *)
  | None -> None (* EOF is acceptable at top-level parsing *)
  | Some c ->
      (* Check if content looks like a selector rather than a declaration.
         Selector-like characters (., #, [, *, :, &) indicate nested rules. *)
      if c = '.' || c = '#' || c = '[' || c = '*' || c = ':' || c = '&' then
        None
      else Some (read_one ())

let read_declarations t =
  Reader.with_context t "declarations" @@ fun () ->
  let rec check_separator acc =
    Reader.ws t;
    match Reader.peek t with
    | Some '}' -> List.rev acc (* End of block, no semicolon needed *)
    | Some ';' ->
        Reader.expect ';' t;
        loop acc
    | None -> List.rev acc (* End of input *)
    | _ ->
        (* Check if we have more tokens that look like a new v *)
        if Reader.is_ident_start (Option.value (Reader.peek t) ~default:' ')
        then Reader.err t "missing semicolon between declarations"
        else
          (* Some other character - let the next iteration handle it *)
          List.rev acc
  and loop acc =
    Reader.ws t;
    match Reader.peek t with
    | Some '}' | None -> List.rev acc
    | _ -> (
        match read_declaration t with
        | None -> List.rev acc
        | Some decl ->
            let acc = decl :: acc in
            (* After reading a declaration, check for proper separation *)
            check_separator acc)
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
      pp_value ctx (kind, value);
      if important then
        Pp.string ctx (if ctx.minify then "!important" else " !important")

(* Convert a declaration to its string representation *)
let string_of_declaration ?(minify = false) decl =
  let buf = Buffer.create 32 in
  let ctx = { Pp.minify; indent = 0; buf; inline = false } in
  pp_declaration ctx decl;
  Buffer.contents buf

(* Single-to-list property helpers *)
let background_image value = v Background_image [ value ]
let text_shadow value = v Text_shadow [ value ]
let transition value = v Transition [ value ]
let transitions values = v Transition values
let animation value = v Animation [ value ]
let box_shadow value = v Box_shadow value

let box_shadows = function
  | [] -> failwith "empty box_shadows"
  | values -> v Box_shadow (List values)

(* Special helpers *)
let z_index_auto = v Z_index Auto

(* Font variant helpers *)
let font_variant_numeric_tokens tokens = Tokens tokens

let font_variant_numeric_composed ?ordinal ?slashed_zero ?numeric_figure
    ?numeric_spacing ?numeric_fraction () =
  Composed
    { ordinal; slashed_zero; numeric_figure; numeric_spacing; numeric_fraction }

(* Property constructors with typed values *)
let background bg = v Background [ bg ]
let background_color c = v Background_color c
let color c = v Color c
let border_color c = v Border_color c
let border_style bs = v Border_style bs
let border_top_style bs = v Border_top_style bs
let border_right_style bs = v Border_right_style bs
let border_bottom_style bs = v Border_bottom_style bs
let border_left_style bs = v Border_left_style bs
let text_decoration td = v Text_decoration td
let font_style fs = v Font_style fs
let list_style_type lst = v List_style_type lst
let list_style_position ls = v List_style_position ls
let list_style_image is = v List_style_image is
let padding (values : length list) = v Padding values
let padding_left len = v Padding_left len
let padding_right len = v Padding_right len
let padding_bottom len = v Padding_bottom len
let padding_top len = v Padding_top len
let margin (values : length list) = v Margin values
let margin_left len = v Margin_left len
let margin_right len = v Margin_right len
let margin_top len = v Margin_top len
let margin_bottom len = v Margin_bottom len

(* Remove deprecated string-based versions *)
let gap len = v Gap len
let column_gap len = v Column_gap len
let row_gap len = v Row_gap len

(* Grid functions *)
let grid_template_areas template = v Grid_template_areas template
let grid_template template = v Grid_template template
let grid_auto_columns size = v Grid_auto_columns size
let grid_auto_rows size = v Grid_auto_rows size
let grid_row_start value = v Grid_row_start value
let grid_row_end value = v Grid_row_end value
let grid_column_start value = v Grid_column_start value
let grid_column_end value = v Grid_column_end value

let grid_row (start, end_) =
  let pp ctx () =
    pp_grid_line ctx start;
    Pp.char ctx '/';
    pp_grid_line ctx end_
  in
  v Grid_row (Pp.to_string ~minify:true pp ())

let grid_column (start, end_) =
  let pp ctx () =
    pp_grid_line ctx start;
    Pp.char ctx '/';
    pp_grid_line ctx end_
  in
  v Grid_column (Pp.to_string ~minify:true pp ())

let grid_area value = v Grid_area value
let width len = v Width (Length len)
let height len = v Height (Length len)

(* Remove deprecated string-based versions *)
let min_width len = v Min_width (Length len)
let min_height len = v Min_height (Length len)
let max_width len = v Max_width (Length len)
let max_height len = v Max_height (Length len)
let font_size len = v Font_size (Length len)
let line_height len = v Line_height len
let font_weight w = v Font_weight w
let text_align a = v Text_align a
let text_decoration_style value = v Text_decoration_style value
let text_decoration_line value = v Text_decoration_line value
let text_underline_offset value = v Text_underline_offset value
let text_transform value = v Text_transform value
let letter_spacing len = v Letter_spacing len
let white_space value = v White_space value
let display d = v Display d
let position p = v Position p
let visibility p = v Visibility p
let inset len = v Inset len
let top len = v Top len
let right len = v Right len
let bottom len = v Bottom len
let left len = v Left len
let opacity value = v Opacity value

(* Remove deprecated string-based versions *)
let flex_direction d = v Flex_direction d
let flex value = v Flex value
let flex_grow value = v Flex_grow value
let flex_shrink value = v Flex_shrink value
let flex_basis value = v Flex_basis value
let flex_wrap value = v Flex_wrap value
let order value = v Order value
let align_items a = v Align_items a
let align_content a = v Align_content a
let align_self a = v Align_self a
let justify_content a = v Justify_content a
let justify_items a = v Justify_items a
let justify_self a = v Justify_self a
let place_content value = v Place_content value
let place_items value = v Place_items value
let place_self value = v Place_self value
let border_width len = v Border_width len
let border_radius len = v Border_radius len
let border_top_left_radius len = v Border_top_left_radius len
let border_top_right_radius len = v Border_top_right_radius len
let border_bottom_left_radius len = v Border_bottom_left_radius len
let border_bottom_right_radius len = v Border_bottom_right_radius len
let fill value = v Fill value
let stroke value = v Stroke value
let stroke_width value = v Stroke_width value
let outline_style o = v Outline_style o
let outline_width len = v Outline_width len
let outline_color c = v Outline_color c
let forced_color_adjust c = v Forced_color_adjust c
let table_layout value = v Table_layout value
let border_spacing len = v Border_spacing len
let overflow o = v Overflow o
let object_fit value = v Object_fit value
let clip value = v Clip value
let clear value = v Clear value
let float value = v Float value
let touch_action value = v Touch_action value
let direction value = v Direction value
let unicode_bidi value = v Unicode_bidi value
let writing_mode value = v Writing_mode value
let text_decoration_skip_ink value = v Text_decoration_skip_ink value
let animation_name value = v Animation_name value
let animation_duration value = v Animation_duration value
let animation_timing_function value = v Animation_timing_function value
let animation_delay value = v Animation_delay value
let animation_iteration_count value = v Animation_iteration_count value
let animation_direction value = v Animation_direction value
let animation_fill_mode value = v Animation_fill_mode value
let animation_play_state value = v Animation_play_state value
let background_blend_mode value = v Background_blend_mode [ value ]
let scroll_margin value = v Scroll_margin value
let scroll_margin_top value = v Scroll_margin_top value
let scroll_margin_right value = v Scroll_margin_right value
let scroll_margin_bottom value = v Scroll_margin_bottom value
let scroll_margin_left value = v Scroll_margin_left value
let scroll_padding value = v Scroll_padding value
let scroll_padding_top value = v Scroll_padding_top value
let scroll_padding_right value = v Scroll_padding_right value
let scroll_padding_bottom value = v Scroll_padding_bottom value
let scroll_padding_left value = v Scroll_padding_left value
let overscroll_behavior value = v Overscroll_behavior value
let overscroll_behavior_x value = v Overscroll_behavior_x value
let overscroll_behavior_y value = v Overscroll_behavior_y value
let accent_color value = v Accent_color value
let caret_color value = v Caret_color value
let text_decoration_color value = v Text_decoration_color value
let text_decoration_thickness value = v Text_decoration_thickness value
let text_size_adjust value = v Text_size_adjust value
let aspect_ratio a = v Aspect_ratio a
let filter value = v Filter value
let word_spacing value = v Word_spacing value
let quotes value = v Quotes value

let border ?width ?style ?color () =
  let border_value : border =
    match (width, style, color) with
    | None, None, None -> None
    | _ -> Shorthand { width; style; color }
  in
  v Border border_value

let tab_size value = v Tab_size value
let webkit_text_size_adjust value = v Webkit_text_size_adjust value
let font_feature_settings value = v Font_feature_settings value
let font_variation_settings value = v Font_variation_settings value
let webkit_tap_highlight_color value = v Webkit_tap_highlight_color value
let webkit_text_decoration value = v Webkit_text_decoration value
let webkit_text_decoration_color value = v Webkit_text_decoration_color value
let text_indent len = v Text_indent len
let border_collapse value = v Border_collapse value
let list_style value = v List_style value
let font value = v Font value
let webkit_appearance value = v Webkit_appearance value
let transform_style value = v Transform_style value
let backface_visibility value = v Backface_visibility value
let object_position value = v Object_position value
let transition_duration value = v Transition_duration value
let transition_timing_function value = v Transition_timing_function value
let transition_delay value = v Transition_delay value
let transition_behavior value = v Transition_behavior value
let transition_property value = v Transition_property value

(* Additional v constructors to match the interface *)
let mix_blend_mode value = v Mix_blend_mode value
let grid_template_columns value = v Grid_template_columns value
let grid_template_rows value = v Grid_template_rows value
let grid_auto_flow value = v Grid_auto_flow value
let pointer_events value = v Pointer_events value
let z_index value = v Z_index value
let appearance value = v Appearance value
let overflow_x value = v Overflow_x value
let overflow_y value = v Overflow_y value
let resize value = v Resize value
let vertical_align value = v Vertical_align value
let box_sizing value = v Box_sizing value
let font_family value = v Font_family value
let print_color_adjust value = v Print_color_adjust value
let background_origin value = v Background_origin value

let font_families = function
  | [] -> failwith "empty font_families"
  | fonts -> v Font_family (List fonts)

let background_attachment value = v Background_attachment value
let border_top value = v Border_top value
let border_right value = v Border_right value
let border_bottom value = v Border_bottom value
let border_left value = v Border_left value
let transform_origin value = v Transform_origin value
let clip_path value = v Clip_path value
let mask value = v Mask value
let content_visibility value = v Content_visibility value
let moz_osx_font_smoothing value = v Moz_osx_font_smoothing value
let webkit_line_clamp value = v Webkit_line_clamp value
let webkit_box_orient value = v Webkit_box_orient value
let text_overflow value = v Text_overflow value
let text_wrap value = v Text_wrap value
let word_break value = v Word_break value
let overflow_wrap value = v Overflow_wrap value
let hyphens value = v Hyphens value
let webkit_hyphens value = v Webkit_hyphens value
let font_stretch value = v Font_stretch value
let font_variant_numeric value = v Font_variant_numeric value
let backdrop_filter value = v Backdrop_filter value
let background_position value = v Background_position value
let background_repeat value = v Background_repeat value
let background_size value = v Background_size value
let content value = v Content value
let border_left_width value = v Border_left_width value
let border_inline_start_width value = v Border_inline_start_width value
let border_inline_end_width value = v Border_inline_end_width value
let border_bottom_width value = v Border_bottom_width value
let border_top_width value = v Border_top_width value
let border_right_width value = v Border_right_width value
let border_top_color value = v Border_top_color value
let border_right_color value = v Border_right_color value
let border_bottom_color value = v Border_bottom_color value
let border_left_color value = v Border_left_color value
let border_inline_start_color value = v Border_inline_start_color value
let border_inline_end_color value = v Border_inline_end_color value
let webkit_font_smoothing value = v Webkit_font_smoothing value
let cursor value = v Cursor value
let user_select value = v User_select value
let webkit_user_select value = v Webkit_user_select value
let container_type value = v Container_type value
let container_name value = v Container_name value
let transform value = v Transform [ value ]
let transforms value = v Transform value
let rotate value = v Rotate value
let scale (value : Properties_intf.scale) = v Scale value
let translate (value : Properties_intf.translate_value) = v Translate value
let perspective value = v Perspective value
let perspective_origin value = v Perspective_origin value
let padding_inline value = v Padding_inline value
let padding_inline_start value = v Padding_inline_start value
let padding_inline_end value = v Padding_inline_end value
let padding_block value = v Padding_block value
let margin_inline value = v Margin_inline value
let margin_inline_start value = v Margin_inline_start value
let margin_inline_end value = v Margin_inline_end value
let margin_block value = v Margin_block value
let margin_block_start value = v Margin_block_start value
let margin_block_end value = v Margin_block_end value
let will_change value = v Will_change value
let contain value = v Contain value
let isolation value = v Isolation value
let outline value = v Outline value
let outline_offset len = v Outline_offset len
let scroll_snap_type value = v Scroll_snap_type value
let scroll_snap_align value = v Scroll_snap_align value
let scroll_snap_stop value = v Scroll_snap_stop value
let scroll_behavior value = v Scroll_behavior value

(* Alignment constructor helpers (declarations) *)
