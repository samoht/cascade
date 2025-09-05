(** CSS Property value parsing using Reader API *)

open Reader

(** Error helpers *)
let err_invalid_value prop_name value =
  raise (Parse_error ("invalid " ^ prop_name ^ " value: " ^ value))

let err_invalid_function func_type name =
  raise (Parse_error ("invalid " ^ func_type ^ " function: " ^ name))

(** Read display property value *)
let read_display t : Css.display =
  let v = ident t in
  match String.lowercase_ascii v with
  | "none" -> None
  | "block" -> Block
  | "inline" -> Inline
  | "inline-block" -> Inline_block
  | "flex" -> Flex
  | "inline-flex" -> Inline_flex
  | "grid" -> Grid
  | "inline-grid" -> Inline_grid
  | "flow-root" -> Flow_root
  | "table" -> Table
  | "table-row" -> Table_row
  | "table-cell" -> Table_cell
  | "table-caption" -> Table_caption
  | "table-column" -> Table_column
  | "table-column-group" -> Table_column_group
  | "table-footer-group" -> Table_footer_group
  | "table-header-group" -> Table_header_group
  | "table-row-group" -> Table_row_group
  | "inline-table" -> Inline_table
  | "list-item" -> List_item
  | "contents" -> Contents
  | "-webkit-box" -> Webkit_box
  | _ -> err_invalid_value "display" v

(** Read position property value *)
let read_position t : Css.position =
  let v = ident t in
  match String.lowercase_ascii v with
  | "static" -> Static
  | "relative" -> Relative
  | "absolute" -> Absolute
  | "fixed" -> Fixed
  | "sticky" -> Sticky
  | _ -> err_invalid_value "position" v

(** Read flex-direction property value *)
let read_flex_direction t : Css.flex_direction =
  let v = ident t in
  match String.lowercase_ascii v with
  | "row" -> Row
  | "row-reverse" -> Row_reverse
  | "column" -> Column
  | "column-reverse" -> Column_reverse
  | _ -> err_invalid_value "flex-direction" v

(** Read align-items property value *)
let read_align_items t : Css.align_items =
  let v = ident t in
  match String.lowercase_ascii v with
  | "normal" -> Normal
  | "stretch" -> Stretch
  | "center" -> Center
  | "start" -> Start
  | "end" -> End
  | "self-start" -> Self_start
  | "self-end" -> Self_end
  | "flex-start" -> Flex_start
  | "flex-end" -> Flex_end
  | "baseline" -> Baseline
  | "first" ->
      ws t;
      if looking_at t "baseline" then (
        skip_n t 8;
        (* skip "baseline" *)
        First_baseline)
      else err_invalid_value "align-items" (v ^ " " ^ peek_string t 10)
  | "last" ->
      ws t;
      if looking_at t "baseline" then (
        skip_n t 8;
        (* skip "baseline" *)
        Last_baseline)
      else err_invalid_value "align-items" (v ^ " " ^ peek_string t 10)
  | "safe" ->
      ws t;
      let next = ident t in
      if String.lowercase_ascii next = "center" then Safe_center
      else err_invalid_value "align-items" (v ^ " " ^ next)
  | "unsafe" ->
      ws t;
      let next = ident t in
      if String.lowercase_ascii next = "center" then Unsafe_center
      else err_invalid_value "align-items" (v ^ " " ^ next)
  | "inherit" -> Inherit_align
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | _ -> err_invalid_value "align-items" v

(** Read justify-content property value *)
let read_justify_content t : Css.justify_content =
  let v = ident t in
  match String.lowercase_ascii v with
  | "normal" -> Normal
  | "flex-start" -> Flex_start
  | "flex-end" -> Flex_end
  | "center" -> Center
  | "space-between" -> Space_between
  | "space-around" -> Space_around
  | "space-evenly" -> Space_evenly
  | "stretch" -> Stretch
  | "start" -> Start
  | "end" -> End
  | "left" -> Left
  | "right" -> Right
  | _ -> err_invalid_value "justify-content" v

(** Read font-weight property value *)
let read_font_weight t : Css.font_weight =
  match try_parse number t with
  | Some n -> Weight (int_of_float n)
  | None -> (
      let v = ident t in
      match String.lowercase_ascii v with
      | "normal" -> Normal
      | "bold" -> Bold
      | "bolder" -> Bolder
      | "lighter" -> Lighter
      | "inherit" -> Inherit
      | _ -> err_invalid_value "font-weight" v)

(** Read font-style property value *)
let read_font_style t : Css.font_style =
  let v = ident t in
  match String.lowercase_ascii v with
  | "normal" -> Normal
  | "italic" -> Italic
  | "oblique" -> Oblique
  | "inherit" -> Inherit
  | _ -> err_invalid_value "font-style" v

(** Read text-align property value *)
let read_text_align t : Css.text_align =
  let v = ident t in
  match String.lowercase_ascii v with
  | "left" -> Left
  | "right" -> Right
  | "center" -> Center
  | "justify" -> Justify
  | "start" -> Start
  | "end" -> End
  | "inherit" -> Inherit
  | _ -> err_invalid_value "text-align" v

(** Read text-decoration property value *)
let read_text_decoration t : Css.text_decoration =
  let v = ident t in
  match String.lowercase_ascii v with
  | "none" -> None
  | "underline" -> Underline
  | "overline" -> Overline
  | "line-through" -> Line_through
  | "inherit" -> Inherit
  | _ -> err_invalid_value "text-decoration" v

(** Read text-transform property value *)
let read_text_transform t : Css.text_transform =
  let v = ident t in
  match String.lowercase_ascii v with
  | "none" -> None
  | "capitalize" -> Capitalize
  | "uppercase" -> Uppercase
  | "lowercase" -> Lowercase
  | "full-width" -> Full_width
  | _ -> err_invalid_value "text-transform" v

(** Read overflow property value *)
let read_overflow t : Css.overflow =
  let v = ident t in
  match String.lowercase_ascii v with
  | "visible" -> Visible
  | "hidden" -> Hidden
  | "scroll" -> Scroll
  | "auto" -> Auto
  | "clip" -> Clip
  | _ -> err_invalid_value "overflow" v

(** Read border-style property value *)
let read_border_style t : Css.border_style =
  let v = ident t in
  match String.lowercase_ascii v with
  | "none" -> None
  | "hidden" -> Hidden
  | "dotted" -> Dotted
  | "dashed" -> Dashed
  | "solid" -> Solid
  | "double" -> Double
  | "groove" -> Groove
  | "ridge" -> Ridge
  | "inset" -> Inset
  | "outset" -> Outset
  | _ -> err_invalid_value "border-style" v

(** Read cursor property value *)
let read_cursor t : Css.cursor =
  let v = ident t in
  match String.lowercase_ascii v with
  | "auto" -> Auto
  | "default" -> Default
  | "none" -> None
  | "pointer" -> Pointer
  | "progress" -> Progress
  | "wait" -> Wait
  | "text" -> Text
  | "move" -> Move
  | "help" -> Help
  | "not-allowed" -> Not_allowed
  | "context-menu" -> Context_menu
  | "cell" -> Cell
  | "crosshair" -> Crosshair
  | "vertical-text" -> Vertical_text
  | "alias" -> Alias
  | "copy" -> Copy
  | "no-drop" -> No_drop
  | "grab" -> Grab
  | "grabbing" -> Grabbing
  | "all-scroll" -> All_scroll
  | "col-resize" -> Col_resize
  | "row-resize" -> Row_resize
  | "n-resize" -> N_resize
  | "e-resize" -> E_resize
  | "s-resize" -> S_resize
  | "w-resize" -> W_resize
  | "ne-resize" -> Ne_resize
  | "nw-resize" -> Nw_resize
  | "se-resize" -> Se_resize
  | "sw-resize" -> Sw_resize
  | "ew-resize" -> Ew_resize
  | "ns-resize" -> Ns_resize
  | "nesw-resize" -> Nesw_resize
  | "nwse-resize" -> Nwse_resize
  | "zoom-in" -> Zoom_in
  | "zoom-out" -> Zoom_out
  | _ -> err_invalid_value "cursor" v

(** Read box-shadow property value *)
let read_box_shadow t : Css.box_shadow =
  (* Simple version: inset? offset-x offset-y blur spread color *)
  let inset =
    if looking_at t "inset" then (
      let _ = ident t in
      ws t;
      true)
    else false
  in
  let h_offset = Values.read_length t in
  ws t;
  let v_offset = Values.read_length t in
  ws t;
  let blur =
    match try_parse Values.read_length t with
    | Some l ->
        ws t;
        l
    | None -> Zero
  in
  let spread =
    match try_parse Values.read_length t with
    | Some l ->
        ws t;
        l
    | None -> Zero
  in
  let color =
    match try_parse Values.read_color t with Some c -> c | None -> Current
  in
  Shadow (Shadow { inset; h_offset; v_offset; blur; spread; color })

(** Read transform function *)
let read_transform t : Css.transform =
  let name = ident t in
  expect t '(';
  ws t;
  let result =
    match String.lowercase_ascii name with
    | "translatex" ->
        let x = Values.read_length t in
        Css.Translate_x x
    | "translatey" ->
        let y = Values.read_length t in
        Css.Translate_y y
    | "translate" ->
        let x = Values.read_length t in
        ws t;
        if peek t = Some ',' then (
          skip t;
          ws t);
        let y =
          match try_parse Values.read_length t with
          | Some l -> Some l
          | None -> None
        in
        Css.Translate (x, y)
    | "scale" ->
        let x = number t in
        ws t;
        let y =
          if peek t = Some ',' then (
            skip t;
            ws t;
            let n = number t in
            Some (Css.Num n : Css.transform_scale))
          else None
        in
        Css.Scale (Css.Num x, y)
    | "scalex" ->
        let x = number t in
        Css.Scale_x (Css.Num x)
    | "scaley" ->
        let y = number t in
        Css.Scale_y (Css.Num y)
    | "rotate" ->
        let angle = Values.read_angle t in
        Css.Rotate angle
    | "skewx" ->
        let angle = Values.read_angle t in
        Css.Skew_x angle
    | "skewy" ->
        let angle = Values.read_angle t in
        Css.Skew_y angle
    | _ -> err_invalid_function "transform" name
  in
  ws t;
  expect t ')';
  result

(** Read transform list *)
let read_transform_list t : Css.transform list =
  let rec loop acc =
    ws t;
    match try_parse read_transform t with
    | None -> List.rev acc
    | Some tf -> loop (tf :: acc)
  in
  loop []

(** Parse a property value based on property name *)
let parse_by_name prop_name value_str : Css.declaration option =
  let t = Reader.of_string value_str in
  try
    match String.lowercase_ascii prop_name with
    (* Display & Positioning *)
    | "display" -> Some (Css.display (read_display t))
    | "position" -> Some (Css.position (read_position t))
    (* Flexbox *)
    | "flex-direction" -> Some (Css.flex_direction (read_flex_direction t))
    | "align-items" -> Some (Css.align_items (read_align_items t))
    | "justify-content" -> Some (Css.justify_content (read_justify_content t))
    (* Typography *)
    | "font-weight" -> Some (Css.font_weight (read_font_weight t))
    | "font-style" -> Some (Css.font_style (read_font_style t))
    | "text-align" -> Some (Css.text_align (read_text_align t))
    | "text-decoration" -> Some (Css.text_decoration (read_text_decoration t))
    | "text-transform" -> Some (Css.text_transform (read_text_transform t))
    (* Box Model *)
    | "width" | "height" | "min-width" | "min-height" | "max-width"
    | "max-height" ->
        let length = Values.read_length t in
        Some
          (match prop_name with
          | "width" -> Css.width length
          | "height" -> Css.height length
          | "min-width" -> Css.min_width length
          | "min-height" -> Css.min_height length
          | "max-width" -> Css.max_width length
          | "max-height" -> Css.max_height length
          | _ -> failwith "unreachable")
    | "padding" | "padding-top" | "padding-right" | "padding-bottom"
    | "padding-left" ->
        let length = Values.read_length t in
        Some
          (match prop_name with
          | "padding-top" -> Css.padding_top length
          | "padding-right" -> Css.padding_right length
          | "padding-bottom" -> Css.padding_bottom length
          | "padding-left" -> Css.padding_left length
          | "padding" -> Css.padding length
          | _ -> failwith "unreachable")
    | "margin" | "margin-top" | "margin-right" | "margin-bottom" | "margin-left"
      ->
        let length = Values.read_length t in
        Some
          (match prop_name with
          | "margin-top" -> Css.margin_top length
          | "margin-right" -> Css.margin_right length
          | "margin-bottom" -> Css.margin_bottom length
          | "margin-left" -> Css.margin_left length
          | "margin" -> Css.margin length
          | _ -> failwith "unreachable")
    (* Colors & Backgrounds *)
    | "color" -> Some (Css.color (Values.read_color t))
    | "background-color" -> Some (Css.background_color (Values.read_color t))
    (* Borders *)
    | "border-style" -> Some (Css.border_style (read_border_style t))
    | "border-width" -> Some (Css.border_width (Values.read_length t))
    | "border-color" -> Some (Css.border_color (Values.read_color t))
    (* Visual Effects *)
    | "opacity" ->
        let n = number t in
        Some (Css.opacity n)
    | "overflow" | "overflow-x" | "overflow-y" ->
        let overflow = read_overflow t in
        Some
          (match prop_name with
          | "overflow" -> Css.overflow overflow
          | "overflow-x" -> Css.overflow_x overflow
          | "overflow-y" -> Css.overflow_y overflow
          | _ -> failwith "unreachable")
    (* User Interaction *)
    | "cursor" -> Some (Css.cursor (read_cursor t))
    (* Transforms *)
    | "transform" ->
        let transforms = read_transform_list t in
        Some (Css.transform transforms)
    (* Box Shadow *)
    | "box-shadow" -> Some (Css.box_shadow (read_box_shadow t))
    (* Custom Properties *)
    | prop when Custom_property.is_custom_property prop ->
        let value = Custom_property.read_custom_property_value t in
        Some (Css.custom_property prop value)
    (* Unknown property - return as custom for now *)
    | _ -> None
  with
  | Parse_error _ -> None
  | _ -> None
