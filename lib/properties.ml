open Values
include Properties_intf

let err_invalid_value ?got t prop_name value =
  Reader.err ?got t ("invalid " ^ prop_name ^ " value: " ^ value)

(* Parse a var(...) body and build a typed Var value *)

(* Generic length parsing helpers *)
let read_line_height_length t : line_height =
  let n, unit = Reader.number_with_unit t in
  match unit with
  | "px" -> Px n
  | "rem" -> Rem n
  | "em" -> Em n
  | "%" -> Pct n
  | "" -> Num n (* unitless number *)
  | _ -> Reader.err t ("unsupported line-height unit: " ^ unit)

let read_vertical_align_length t : vertical_align =
  let n, unit = Reader.number_with_unit t in
  match unit with
  | "px" -> Px n
  | "rem" -> Rem n
  | "em" -> Em n
  | "%" -> Pct (n /. 100.)
  | _ ->
      Reader.err t
        ("expected px, rem, em, or % for vertical-align, got: " ^ unit)

let read_background_size_length t : background_size =
  let n, unit = Reader.number_with_unit t in
  match unit with
  | "px" -> Px n
  | "rem" -> Rem n
  | "em" -> Em n
  | "%" -> Pct (n /. 100.)
  | "vw" -> Vw n
  | "vh" -> Vh n
  | _ -> Reader.err t ("unsupported background-size unit: " ^ unit)

let read_position_component_length t : position_component =
  let n, unit = Reader.number_with_unit t in
  match unit with
  | "px" -> Px n
  | "rem" -> Rem n
  | "em" -> Em n
  | "%" -> Pct n
  | _ -> Reader.err t ("unsupported position unit: " ^ unit)

let read_display t : display =
  Reader.enum "display"
    [
      ("none", (None : display));
      ("block", Block);
      ("inline", Inline);
      ("inline-block", Inline_block);
      ("flex", Flex);
      ("inline-flex", Inline_flex);
      ("grid", Grid);
      ("inline-grid", Inline_grid);
      ("flow-root", Flow_root);
      ("table", Table);
      ("table-row", Table_row);
      ("table-cell", Table_cell);
      ("table-caption", Table_caption);
      ("table-column", Table_column);
      ("table-column-group", Table_column_group);
      ("table-footer-group", Table_footer_group);
      ("table-header-group", Table_header_group);
      ("table-row-group", Table_row_group);
      ("inline-table", Inline_table);
      ("list-item", List_item);
      ("contents", Contents);
      ("-webkit-box", Webkit_box);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
    ]
    t

let read_position t : position =
  Reader.enum "position"
    [
      ("static", (Static : position));
      ("relative", Relative);
      ("absolute", Absolute);
      ("fixed", Fixed);
      ("sticky", Sticky);
    ]
    t

let read_flex_direction t : flex_direction =
  Reader.enum "flex-direction"
    [
      ("row", (Row : flex_direction));
      ("row-reverse", Row_reverse);
      ("column", Column);
      ("column-reverse", Column_reverse);
    ]
    t

let read_align_items t : align_items =
  Reader.enum "align-items"
    [
      ("normal", (Normal : align_items));
      ("stretch", Stretch);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("baseline", Baseline);
    ]
    t

let read_justify_content t : justify_content =
  Reader.enum "justify-content"
    [
      ("left", (Left : justify_content));
      ("right", Right);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("space-evenly", Space_evenly);
    ]
    t

let rec read_font_weight t : font_weight =
  let read_var t : font_weight = Var (read_var read_font_weight t) in
  Reader.ws t;
  Reader.enum_or_calls "font-weight"
    [
      ("normal", Normal);
      ("bold", Bold);
      ("bolder", Bolder);
      ("lighter", Lighter);
      ("inherit", Inherit);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let n = Reader.number t in
      let weight = int_of_float n in
      if weight >= 1 && weight <= 1000 then Weight weight
      else err_invalid_value t "font-weight" (string_of_int weight))
    t

let read_font_style t : font_style =
  Reader.enum "font-style"
    [
      ("normal", (Normal : font_style));
      ("italic", (Italic : font_style));
      ("oblique", (Oblique : font_style));
      ("inherit", (Inherit : font_style));
    ]
    t

let read_text_align t : text_align =
  Reader.enum "text-align"
    [
      ("left", (Left : text_align));
      ("right", Right);
      ("center", Center);
      ("justify", Justify);
      ("start", Start);
      ("end", End);
      ("inherit", Inherit);
    ]
    t

let read_text_decoration_line t : text_decoration_line =
  Reader.enum "text-decoration-line"
    [
      ("underline", (Underline : text_decoration_line));
      ("overline", Overline);
      ("line-through", Line_through);
    ]
    t

let read_text_decoration_style t : text_decoration_style =
  Reader.enum "text-decoration-style"
    [
      ("solid", (Solid : text_decoration_style));
      ("double", Double);
      ("dotted", Dotted);
      ("dashed", Dashed);
      ("wavy", Wavy);
      ("inherit", Inherit);
    ]
    t

let rec read_text_decoration t : text_decoration =
  let update (acc : text_decoration_shorthand) = function
    | `Line l -> { acc with lines = l :: acc.lines }
    | `Style s -> if acc.style = None then { acc with style = Some s } else acc
    | `Color c -> if acc.color = None then { acc with color = Some c } else acc
    | `Thickness th ->
        if acc.thickness = None then { acc with thickness = Some th } else acc
  in
  let read_line_comp t = `Line (read_text_decoration_line t) in
  let read_style_comp t = `Style (read_text_decoration_style t) in
  let read_color_comp t = `Color (read_color t) in
  let read_thickness_comp t = `Thickness (read_length t) in
  let read_component t =
    Reader.one_of
      [ read_line_comp; read_style_comp; read_color_comp; read_thickness_comp ]
      t
  in
  let read_var t : text_decoration = Var (read_var read_text_decoration t) in
  Reader.enum_or_calls "text-decoration"
    [ ("inherit", (Inherit : text_decoration)); ("none", None) ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let init = { lines = []; style = None; color = None; thickness = None } in
      let acc, _ = Reader.fold_many read_component ~init ~f:update t in
      Shorthand { acc with lines = List.rev acc.lines })
    t

let rec read_text_transform t : text_transform =
  let read_var t : text_transform = Var (read_var read_text_transform t) in
  Reader.enum_or_calls "text-transform"
    [
      ("none", (None : text_transform));
      ("uppercase", Uppercase);
      ("lowercase", Lowercase);
      ("capitalize", Capitalize);
      ("full-width", Full_width);
      ("inherit", Inherit);
    ]
    ~calls:[ ("var", read_var) ]
    t

let read_overflow t : overflow =
  Reader.enum "overflow"
    [
      ("visible", (Visible : overflow));
      ("hidden", Hidden);
      ("scroll", Scroll);
      ("auto", Auto);
      ("clip", Clip);
    ]
    t

let read_cursor t : cursor =
  Reader.enum "cursor"
    [
      ("auto", (Auto : cursor));
      ("default", Default);
      ("none", None);
      ("context-menu", Context_menu);
      ("help", Help);
      ("pointer", Pointer);
      ("progress", Progress);
      ("wait", Wait);
      ("cell", Cell);
      ("crosshair", Crosshair);
      ("text", Text);
      ("vertical-text", Vertical_text);
      ("alias", Alias);
      ("copy", Copy);
      ("move", Move);
      ("no-drop", No_drop);
      ("not-allowed", Not_allowed);
      ("grab", Grab);
      ("grabbing", Grabbing);
      ("all-scroll", All_scroll);
      ("col-resize", Col_resize);
      ("row-resize", Row_resize);
      ("n-resize", N_resize);
      ("e-resize", E_resize);
      ("s-resize", S_resize);
      ("w-resize", W_resize);
      ("ne-resize", Ne_resize);
      ("nw-resize", Nw_resize);
      ("se-resize", Se_resize);
      ("sw-resize", Sw_resize);
      ("ew-resize", Ew_resize);
      ("ns-resize", Ns_resize);
      ("nesw-resize", Nesw_resize);
      ("nwse-resize", Nwse_resize);
      ("zoom-in", Zoom_in);
      ("zoom-out", Zoom_out);
    ]
    t

let read_box_shadow_component t =
  Reader.one_of
    [
      (fun t ->
        Reader.expect_string "inset" t;
        `Inset);
      (fun t -> `Color (read_color t));
      (fun t -> `Length (read_length t));
    ]
    t

let read_box_shadow_lengths lengths =
  match lengths with
  | h_offset :: v_offset :: rest ->
      let blur, spread =
        match rest with
        | b :: s :: _ -> (Some b, Some s)
        | b :: [] -> (Some b, None)
        | [] -> (None, None)
      in
      Some (h_offset, v_offset, blur, spread)
  | _ -> None

(* Helper type for accumulating box shadow components *)
type box_shadow_accumulator = {
  inset : bool;
  lengths : length list;
  color : color option;
}

let fold_box_shadow_components components =
  List.fold_left
    (fun (acc : box_shadow_accumulator) comp ->
      match comp with
      | `Inset -> { acc with inset = true }
      | `Color c ->
          if acc.color = None then { acc with color = Some c } else acc
      | `Length l -> { acc with lengths = l :: acc.lengths })
    { inset = false; lengths = []; color = None }
    components

let read_box_shadow_custom t =
  let components, _ = Reader.many read_box_shadow_component t in
  let parts = fold_box_shadow_components components in
  let lengths = List.rev parts.lengths in
  match read_box_shadow_lengths lengths with
  | Some (h_offset, v_offset, blur, spread) ->
      Shadow
        {
          inset = parts.inset;
          h_offset;
          v_offset;
          blur;
          spread;
          color = parts.color;
        }
  | None -> err_invalid_value t "box-shadow" "at least two lengths are required"

let rec read_box_shadow t : box_shadow =
  let read_var t : box_shadow = Var (read_var read_box_shadow t) in
  Reader.ws t;
  Reader.enum_or_calls "box-shadow"
    [ ("none", None); ("inherit", Inherit) ]
    ~calls:[ ("var", read_var) ]
    ~default:read_box_shadow_custom t

let read_box_shadows t : box_shadow list =
  Reader.list ~sep:Reader.comma ~at_least:1 read_box_shadow t

(* Named helpers for transform function bodies to keep the call table tidy *)
let read_translate_x t =
  Reader.call "translatex" t (fun t -> Translate_x (read_length t))

let read_translate_y t =
  Reader.call "translatey" t (fun t -> Translate_y (read_length t))

let read_translate_z t =
  Reader.call "translatez" t (fun t -> Translate_z (read_length t))

let read_translate3d t =
  Reader.call "translate3d" t (fun t ->
      let x, y, z =
        Reader.(triple ~sep:comma read_length read_length read_length) t
      in
      Translate_3d (x, y, z))

let read_translate t =
  Reader.call "translate" t (fun t ->
      let x = read_length t in
      let y =
        Reader.option
          (fun t ->
            Reader.comma t;
            read_length t)
          t
      in
      Translate (x, y))

let read_rotate_x t = Reader.call "rotatex" t (fun t -> Rotate_x (read_angle t))
let read_rotate_y t = Reader.call "rotatey" t (fun t -> Rotate_y (read_angle t))
let read_rotate_z t = Reader.call "rotatez" t (fun t -> Rotate_z (read_angle t))

let read_rotate t : transform =
  Reader.call "rotate" t (fun t -> (Rotate (read_angle t) : transform))

let read_scale_x t = Reader.call "scalex" t (fun t -> Scale_x (Reader.number t))
let read_scale_y t = Reader.call "scaley" t (fun t -> Scale_y (Reader.number t))
let read_scale_z t = Reader.call "scalez" t (fun t -> Scale_z (Reader.number t))

let read_rotate3d t =
  Reader.call "rotate3d" t (fun t ->
      let x, y, z = Reader.(triple ~sep:comma number number number) t in
      Reader.comma t;
      let angle = read_angle t in
      Rotate_3d (x, y, z, angle))

let read_scale3d t =
  Reader.call "scale3d" t (fun t ->
      let x, y, z = Reader.(triple ~sep:comma number number number) t in
      Scale_3d (x, y, z))

let read_scale t : transform =
  Reader.call "scale" t (fun t ->
      let x = Reader.number t in
      let y =
        Reader.option
          (fun t ->
            Reader.comma t;
            Reader.number t)
          t
      in
      (Scale (x, y) : transform))

let read_skew_x t = Reader.call "skewx" t (fun t -> Skew_x (read_angle t))
let read_skew_y t = Reader.call "skewy" t (fun t -> Skew_y (read_angle t))

let read_skew t =
  Reader.call "skew" t (fun t ->
      let x = read_angle t in
      let y =
        Reader.option
          (fun t ->
            Reader.comma t;
            read_angle t)
          t
      in
      Skew (x, y))

let read_matrix t =
  Reader.call "matrix" t (fun t ->
      match Reader.list ~sep:Reader.comma Reader.number t with
      | [ a; b; c; d; e; f ] -> Matrix (a, b, c, d, e, f)
      | _ -> err_invalid_value t "matrix" "expected 6 arguments")

let read_matrix3d t =
  Reader.call "matrix3d" t (fun t ->
      match Reader.list ~sep:Reader.comma Reader.number t with
      | [
       m11;
       m12;
       m13;
       m14;
       m21;
       m22;
       m23;
       m24;
       m31;
       m32;
       m33;
       m34;
       m41;
       m42;
       m43;
       m44;
      ] ->
          Matrix_3d
            ( m11,
              m12,
              m13,
              m14,
              m21,
              m22,
              m23,
              m24,
              m31,
              m32,
              m33,
              m34,
              m41,
              m42,
              m43,
              m44 )
      | _ -> err_invalid_value t "matrix3d" "expected 16 arguments")

let transform_parsers =
  [
    ("translatex", read_translate_x);
    ("translatey", read_translate_y);
    ("translatez", read_translate_z);
    ("translate3d", read_translate3d);
    ("translate", read_translate);
    ("rotatex", read_rotate_x);
    ("rotatey", read_rotate_y);
    ("rotatez", read_rotate_z);
    ("rotate3d", read_rotate3d);
    ("rotate", read_rotate);
    ("scalex", read_scale_x);
    ("scaley", read_scale_y);
    ("scalez", read_scale_z);
    ("scale3d", read_scale3d);
    ("scale", read_scale);
    ("skewx", read_skew_x);
    ("skewy", read_skew_y);
    ("skew", read_skew);
    ("matrix", read_matrix);
    ("matrix3d", read_matrix3d);
  ]

let read_transform t : transform =
  Reader.enum_or_calls "transform"
    [ ("none", (None : transform)) ]
    ~calls:transform_parsers t

let pp_opt_space pp ctx = function
  | Some v ->
      Pp.space ctx ();
      pp ctx v
  | None -> ()

let pp_keyword s ctx = Pp.string ctx s

(* Read only the body of a url(...) call when used inside enum_calls. The
   surrounding function name and parentheses are handled by Reader. *)
let read_url_arg t =
  Reader.ws t;
  match Reader.peek t with
  | Some ('"' | '\'') -> Reader.string ~trim:true t
  | _ -> String.trim (Reader.until t ')')

let pp_shadow_parts ctx ~inset h v blur spread color =
  if inset then (
    Pp.string ctx "inset";
    Pp.space ctx ());
  pp_length ctx h;
  Pp.space ctx ();
  pp_length ctx v;
  pp_opt_space pp_length ctx blur;
  pp_opt_space pp_length ctx spread;
  pp_opt_space pp_color ctx color

let rec pp_shadow : shadow Pp.t =
 fun ctx -> function
  | Simple (h, v, blur, spread, color) ->
      pp_shadow_parts ctx ~inset:false h v blur spread color
  | Inset (h, v, blur, spread, color) ->
      pp_shadow_parts ctx ~inset:true h v blur spread color
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_shadow ctx v

let pp_gradient_direction : gradient_direction Pp.t =
 fun ctx -> function
  | To_top -> Pp.string ctx "to top"
  | To_top_right -> Pp.string ctx "to top right"
  | To_right -> Pp.string ctx "to right"
  | To_bottom_right -> Pp.string ctx "to bottom right"
  | To_bottom -> Pp.string ctx "to bottom"
  | To_bottom_left -> Pp.string ctx "to bottom left"
  | To_left -> Pp.string ctx "to left"
  | To_top_left -> Pp.string ctx "to top left"
  | Angle a -> pp_angle ctx a

let pp_gradient_stop : gradient_stop Pp.t =
 fun ctx -> function
  | Color_stop c -> pp_color ctx c
  | Color_position (c, len) ->
      pp_color ctx c;
      Pp.space ctx ();
      pp_length ctx len
  | Var v -> pp_var pp_color ctx v
  | Computed_stops s -> Pp.string ctx s

let rec pp_filter : filter Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Blur l -> Pp.call "blur" pp_length ctx l
  | Brightness n -> Pp.call "brightness" pp_number ctx n
  | Contrast n -> Pp.call "contrast" pp_number ctx n
  | Drop_shadow s -> Pp.call "drop-shadow" pp_shadow ctx s
  | Grayscale n -> Pp.call "grayscale" pp_number ctx n
  | Hue_rotate a -> Pp.call "hue-rotate" pp_angle ctx a
  | Invert n -> Pp.call "invert" pp_number ctx n
  | Opacity n -> Pp.call "opacity" pp_number ctx n
  | Saturate n -> Pp.call "saturate" pp_number ctx n
  | Sepia n -> Pp.call "sepia" pp_number ctx n
  | Url url -> Pp.url ctx url
  | List filters -> Pp.list ~sep:Pp.space pp_filter ctx filters
  | Var v -> pp_var pp_filter ctx v

let pp_background_image : background_image Pp.t =
 fun ctx -> function
  | Url url -> Pp.url ctx url
  | Linear_gradient (dir, stops) ->
      Pp.call "linear-gradient"
        (fun ctx (dir, stops) ->
          (* Only print direction if it's not the default "to bottom" *)
          let print_direction =
            match dir with To_bottom -> false | _ -> true
          in
          if print_direction then (
            pp_gradient_direction ctx dir;
            match stops with [] -> () | _ -> Pp.comma ctx ());
          match stops with
          | [] -> ()
          | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
        ctx (dir, stops)
  | Radial_gradient stops ->
      Pp.call "radial-gradient"
        (fun ctx stops ->
          match stops with
          | [] -> ()
          | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
        ctx stops
  | None -> Pp.string ctx "none"

let rec pp_font_family : font_family Pp.t =
 fun ctx -> function
  (* Generic CSS font families *)
  | Sans_serif -> Pp.string ctx "sans-serif"
  | Serif -> Pp.string ctx "serif"
  | Monospace -> Pp.string ctx "monospace"
  | Cursive -> Pp.string ctx "cursive"
  | Fantasy -> Pp.string ctx "fantasy"
  | System_ui -> Pp.string ctx "system-ui"
  | Ui_sans_serif -> Pp.string ctx "ui-sans-serif"
  | Ui_serif -> Pp.string ctx "ui-serif"
  | Ui_monospace -> Pp.string ctx "ui-monospace"
  | Ui_rounded -> Pp.string ctx "ui-rounded"
  | Emoji -> Pp.string ctx "emoji"
  | Math -> Pp.string ctx "math"
  | Fangsong -> Pp.string ctx "fangsong"
  (* Popular web fonts *)
  | Inter -> Pp.string ctx "Inter"
  | Roboto -> Pp.string ctx "Roboto"
  | Open_sans -> Pp.string ctx "\"Open Sans\""
  | Lato -> Pp.string ctx "Lato"
  | Montserrat -> Pp.string ctx "Montserrat"
  | Poppins -> Pp.string ctx "Poppins"
  | Source_sans_pro -> Pp.string ctx "\"Source Sans Pro\""
  | Raleway -> Pp.string ctx "Raleway"
  | Oswald -> Pp.string ctx "Oswald"
  | Noto_sans -> Pp.string ctx "\"Noto Sans\""
  | Ubuntu -> Pp.string ctx "Ubuntu"
  | Playfair_display -> Pp.string ctx "\"Playfair Display\""
  | Merriweather -> Pp.string ctx "Merriweather"
  | Lora -> Pp.string ctx "Lora"
  | PT_sans -> Pp.string ctx "\"PT Sans\""
  | PT_serif -> Pp.string ctx "\"PT Serif\""
  | Nunito -> Pp.string ctx "Nunito"
  | Nunito_sans -> Pp.string ctx "\"Nunito Sans\""
  | Work_sans -> Pp.string ctx "\"Work Sans\""
  | Rubik -> Pp.string ctx "Rubik"
  | Fira_sans -> Pp.string ctx "\"Fira Sans\""
  | Fira_code -> Pp.string ctx "\"Fira Code\""
  | JetBrains_mono -> Pp.string ctx "\"JetBrains Mono\""
  | IBM_plex_sans -> Pp.string ctx "\"IBM Plex Sans\""
  | IBM_plex_serif -> Pp.string ctx "\"IBM Plex Serif\""
  | IBM_plex_mono -> Pp.string ctx "\"IBM Plex Mono\""
  | Source_code_pro -> Pp.string ctx "\"Source Code Pro\""
  | Space_mono -> Pp.string ctx "\"Space Mono\""
  | DM_sans -> Pp.string ctx "\"DM Sans\""
  | DM_serif_display -> Pp.string ctx "\"DM Serif Display\""
  | Bebas_neue -> Pp.string ctx "\"Bebas Neue\""
  | Barlow -> Pp.string ctx "Barlow"
  | Mulish -> Pp.string ctx "Mulish"
  | Josefin_sans -> Pp.string ctx "\"Josefin Sans\""
  (* Platform-specific fonts *)
  | Helvetica -> Pp.string ctx "Helvetica"
  | Helvetica_neue -> Pp.string ctx "\"Helvetica Neue\""
  | Arial -> Pp.string ctx "Arial"
  | Verdana -> Pp.string ctx "Verdana"
  | Tahoma -> Pp.string ctx "Tahoma"
  | Trebuchet_ms -> Pp.string ctx "\"Trebuchet MS\""
  | Times_new_roman -> Pp.string ctx "\"Times New Roman\""
  | Times -> Pp.string ctx "Times"
  | Georgia -> Pp.string ctx "Georgia"
  | Cambria -> Pp.string ctx "Cambria"
  | Garamond -> Pp.string ctx "Garamond"
  | Courier_new -> Pp.string ctx "\"Courier New\""
  | Courier -> Pp.string ctx "Courier"
  | Lucida_console -> Pp.string ctx "\"Lucida Console\""
  | SF_pro -> Pp.string ctx "\"SF Pro\""
  | SF_pro_display -> Pp.string ctx "\"SF Pro Display\""
  | SF_pro_text -> Pp.string ctx "\"SF Pro Text\""
  | SF_mono -> Pp.string ctx "\"SF Mono\""
  | NY -> Pp.string ctx "\"New York\""
  | Segoe_ui -> Pp.string ctx "\"Segoe UI\""
  | Segoe_ui_emoji -> Pp.string ctx "\"Segoe UI Emoji\""
  | Segoe_ui_symbol -> Pp.string ctx "\"Segoe UI Symbol\""
  | Apple_color_emoji -> Pp.string ctx "\"Apple Color Emoji\""
  | Noto_color_emoji -> Pp.string ctx "\"Noto Color Emoji\""
  | Android_emoji -> Pp.string ctx "\"Android Emoji\""
  | Twemoji_mozilla -> Pp.string ctx "\"Twemoji Mozilla\""
  (* Developer fonts *)
  | Menlo -> Pp.string ctx "Menlo"
  | Monaco -> Pp.string ctx "Monaco"
  | Consolas -> Pp.string ctx "Consolas"
  | Liberation_mono -> Pp.string ctx "\"Liberation Mono\""
  | SFMono_regular -> Pp.string ctx "SFMono-Regular"
  | Cascadia_code -> Pp.string ctx "\"Cascadia Code\""
  | Cascadia_mono -> Pp.string ctx "\"Cascadia Mono\""
  | Victor_mono -> Pp.string ctx "\"Victor Mono\""
  | Inconsolata -> Pp.string ctx "Inconsolata"
  | Hack -> Pp.string ctx "Hack"
  (* CSS keywords *)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Name s ->
      (* Font names with spaces must be quoted per CSS spec *)
      if String.contains s ' ' then Pp.quoted_string ctx s else Pp.string ctx s
  | Var v -> pp_var (Pp.list ~sep:Pp.comma pp_font_family) ctx v

let rec pp_border_style : border_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Hidden -> Pp.string ctx "hidden"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Var v -> pp_var pp_border_style ctx v

let rec pp_border_width : border_width Pp.t =
 fun ctx -> function
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Ch f -> Pp.unit ctx f "ch"
  | Vh f -> Pp.unit ctx f "vh"
  | Vw f -> Pp.unit ctx f "vw"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Pct f -> Pp.unit ctx f "%"
  | Zero -> Pp.char ctx '0'
  | Auto -> Pp.string ctx "auto"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | Fit_content -> Pp.string ctx "fit-content"
  | From_font -> Pp.string ctx "from-font"
  | Calc cv -> pp_calc pp_border_width ctx cv
  | Var v -> pp_var pp_border_width ctx v
  | Inherit -> Pp.string ctx "inherit"

let pp_border : border Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | None -> Pp.string ctx "none"
  | Border { width; style; color } ->
      let first = ref true in
      let add_space () = if !first then first := false else Pp.space ctx () in
      Option.iter
        (fun w ->
          add_space ();
          pp_border_width ctx w)
        width;
      Option.iter
        (fun s ->
          add_space ();
          pp_border_style ctx s)
        style;
      Option.iter
        (fun c ->
          add_space ();
          pp_color ctx c)
        color

let pp_clip : string Pp.t = Pp.string

let pp_display : display Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inline_block -> Pp.string ctx "inline-block"
  | Flex -> Pp.string ctx "flex"
  | Inline_flex -> Pp.string ctx "inline-flex"
  | Grid -> Pp.string ctx "grid"
  | Inline_grid -> Pp.string ctx "inline-grid"
  | Flow_root -> Pp.string ctx "flow-root"
  | Table -> Pp.string ctx "table"
  | Table_row -> Pp.string ctx "table-row"
  | Table_cell -> Pp.string ctx "table-cell"
  | Table_caption -> Pp.string ctx "table-caption"
  | Table_column -> Pp.string ctx "table-column"
  | Table_column_group -> Pp.string ctx "table-column-group"
  | Table_footer_group -> Pp.string ctx "table-footer-group"
  | Table_header_group -> Pp.string ctx "table-header-group"
  | Table_row_group -> Pp.string ctx "table-row-group"
  | Inline_table -> Pp.string ctx "inline-table"
  | List_item -> Pp.string ctx "list-item"
  | Contents -> Pp.string ctx "contents"
  | Webkit_box -> Pp.string ctx "-webkit-box"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"

let pp_position : position Pp.t =
 fun ctx -> function
  | Static -> Pp.string ctx "static"
  | Relative -> Pp.string ctx "relative"
  | Absolute -> Pp.string ctx "absolute"
  | Fixed -> Pp.string ctx "fixed"
  | Sticky -> Pp.string ctx "sticky"

let pp_visibility : visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Collapse -> Pp.string ctx "collapse"

let pp_z_index : z_index Pp.t =
 fun ctx -> function Auto -> Pp.string ctx "auto" | Index i -> Pp.int ctx i

let pp_overflow : overflow Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Scroll -> Pp.string ctx "scroll"
  | Auto -> Pp.string ctx "auto"
  | Clip -> Pp.string ctx "clip"

let pp_flex_direction : flex_direction Pp.t =
 fun ctx -> function
  | Row -> Pp.string ctx "row"
  | Row_reverse -> Pp.string ctx "row-reverse"
  | Column -> Pp.string ctx "column"
  | Column_reverse -> Pp.string ctx "column-reverse"

let pp_flex_wrap : flex_wrap Pp.t =
 fun ctx -> function
  | Nowrap -> Pp.string ctx "nowrap"
  | Wrap -> Pp.string ctx "wrap"
  | Wrap_reverse -> Pp.string ctx "wrap-reverse"

let rec pp_align : align Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Var v -> pp_var pp_align ctx v

let pp_align_items : align_items Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Safe_center -> Pp.string ctx "safe center"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"

let pp_align_self : align_self Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Center -> Pp.string ctx "center"
  | Baseline -> Pp.string ctx "baseline"
  | Stretch -> Pp.string ctx "stretch"

let pp_justify_content : justify_content Pp.t =
 fun ctx -> function
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Center -> Pp.string ctx "center"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let pp_justify : justify Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Baseline -> Pp.string ctx "baseline"
  | Inherit -> Pp.string ctx "inherit"

let pp_font_style : font_style Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Italic -> Pp.string ctx "italic"
  | Oblique -> Pp.string ctx "oblique"
  | Inherit -> Pp.string ctx "inherit"

let pp_text_align : text_align Pp.t =
 fun ctx -> function
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Center -> Pp.string ctx "center"
  | Justify -> Pp.string ctx "justify"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Inherit -> Pp.string ctx "inherit"

let pp_text_decoration_line : text_decoration_line Pp.t =
 fun ctx -> function
  | Underline -> Pp.string ctx "underline"
  | Overline -> Pp.string ctx "overline"
  | Line_through -> Pp.string ctx "line-through"

let pp_text_decoration_style : text_decoration_style Pp.t =
 fun ctx -> function
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Wavy -> Pp.string ctx "wavy"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_text_decoration : text_decoration Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Shorthand { lines; style; color; thickness } -> (
      let first = ref true in
      let space_if_needed () =
        if !first then first := false else Pp.space ctx ()
      in
      (match lines with
      | [] -> ()
      | ls ->
          space_if_needed ();
          Pp.list ~sep:Pp.space pp_text_decoration_line ctx ls);
      (match style with
      | None -> ()
      | Some s ->
          space_if_needed ();
          pp_text_decoration_style ctx s);
      (match color with
      | None -> ()
      | Some c ->
          space_if_needed ();
          pp_color ctx c);
      match thickness with
      | None -> ()
      | Some l ->
          space_if_needed ();
          pp_length ctx l)
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_text_decoration ctx v

let rec pp_text_transform : text_transform Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Capitalize -> Pp.string ctx "capitalize"
  | Uppercase -> Pp.string ctx "uppercase"
  | Lowercase -> Pp.string ctx "lowercase"
  | Full_width -> Pp.string ctx "full-width"
  | Full_size_kana -> Pp.string ctx "full-size-kana"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_text_transform ctx v

let pp_text_overflow : text_overflow Pp.t =
 fun ctx -> function
  | Clip -> Pp.string ctx "clip"
  | Ellipsis -> Pp.string ctx "ellipsis"
  | String s -> Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"

let pp_text_wrap : text_wrap Pp.t =
 fun ctx -> function
  | Wrap -> Pp.string ctx "wrap"
  | No_wrap -> Pp.string ctx "nowrap"
  | Balance -> Pp.string ctx "balance"
  | Pretty -> Pp.string ctx "pretty"
  | Inherit -> Pp.string ctx "inherit"

let pp_white_space : white_space Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Nowrap -> Pp.string ctx "nowrap"
  | Pre -> Pp.string ctx "pre"
  | Pre_wrap -> Pp.string ctx "pre-wrap"
  | Pre_line -> Pp.string ctx "pre-line"
  | Break_spaces -> Pp.string ctx "break-spaces"
  | Inherit -> Pp.string ctx "inherit"

let pp_word_break : word_break Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Break_all -> Pp.string ctx "break-all"
  | Keep_all -> Pp.string ctx "keep-all"
  | Break_word -> Pp.string ctx "break-word"
  | Inherit -> Pp.string ctx "inherit"

let pp_overflow_wrap : overflow_wrap Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Break_word -> Pp.string ctx "break-word"
  | Anywhere -> Pp.string ctx "anywhere"
  | Inherit -> Pp.string ctx "inherit"

let pp_hyphens : hyphens Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Manual -> Pp.string ctx "manual"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"

let pp_list_style_type : list_style_type Pp.t =
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

let pp_list_style_position : list_style_position Pp.t =
 fun ctx -> function
  | Inside -> Pp.string ctx "inside"
  | Outside -> Pp.string ctx "outside"
  | Inherit -> Pp.string ctx "inherit"

let pp_list_style_image : list_style_image Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Url u -> Pp.url ctx u
  | Inherit -> Pp.string ctx "inherit"

let pp_table_layout : table_layout Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Fixed -> Pp.string ctx "fixed"
  | Inherit -> Pp.string ctx "inherit"

let pp_vertical_align : vertical_align Pp.t =
 fun ctx -> function
  | Baseline -> Pp.string ctx "baseline"
  | Top -> Pp.string ctx "top"
  | Middle -> Pp.string ctx "middle"
  | Bottom -> Pp.string ctx "bottom"
  | Text_top -> Pp.string ctx "text-top"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.unit ctx p "%"
  | Inherit -> Pp.string ctx "inherit"

let pp_grid_auto_flow : grid_auto_flow Pp.t =
 fun ctx -> function
  | Row -> Pp.string ctx "row"
  | Column -> Pp.string ctx "column"
  | Dense -> Pp.string ctx "dense"
  | Row_dense -> Pp.string ctx "row dense"
  | Column_dense -> Pp.string ctx "column dense"

let pp_grid_line : grid_line Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Num n -> Pp.int ctx n
  | Name s -> Pp.string ctx s
  | Span n ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.int ctx n

let pp_aspect_ratio : aspect_ratio Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Ratio (a, b) ->
      Pp.float ctx a;
      Pp.char ctx '/';
      Pp.float ctx b

let pp_property : type a. a property Pp.t =
 fun ctx -> function
  | Background_color -> Pp.string ctx "background-color"
  | Color -> Pp.string ctx "color"
  | Border_color -> Pp.string ctx "border-color"
  | Border_style -> Pp.string ctx "border-style"
  | Border_top_style -> Pp.string ctx "border-top-style"
  | Border_right_style -> Pp.string ctx "border-right-style"
  | Border_bottom_style -> Pp.string ctx "border-bottom-style"
  | Border_left_style -> Pp.string ctx "border-left-style"
  | Padding -> Pp.string ctx "padding"
  | Padding_left -> Pp.string ctx "padding-left"
  | Padding_right -> Pp.string ctx "padding-right"
  | Padding_bottom -> Pp.string ctx "padding-bottom"
  | Padding_top -> Pp.string ctx "padding-top"
  | Padding_inline -> Pp.string ctx "padding-inline"
  | Padding_inline_start -> Pp.string ctx "padding-inline-start"
  | Padding_inline_end -> Pp.string ctx "padding-inline-end"
  | Padding_block -> Pp.string ctx "padding-block"
  | Margin -> Pp.string ctx "margin"
  | Margin_inline_end -> Pp.string ctx "margin-inline-end"
  | Margin_left -> Pp.string ctx "margin-left"
  | Margin_right -> Pp.string ctx "margin-right"
  | Margin_top -> Pp.string ctx "margin-top"
  | Margin_bottom -> Pp.string ctx "margin-bottom"
  | Margin_inline -> Pp.string ctx "margin-inline"
  | Margin_block -> Pp.string ctx "margin-block"
  | Gap -> Pp.string ctx "gap"
  | Column_gap -> Pp.string ctx "column-gap"
  | Row_gap -> Pp.string ctx "row-gap"
  | Width -> Pp.string ctx "width"
  | Height -> Pp.string ctx "height"
  | Min_width -> Pp.string ctx "min-width"
  | Min_height -> Pp.string ctx "min-height"
  | Max_width -> Pp.string ctx "max-width"
  | Max_height -> Pp.string ctx "max-height"
  | Font_size -> Pp.string ctx "font-size"
  | Line_height -> Pp.string ctx "line-height"
  | Font_weight -> Pp.string ctx "font-weight"
  | Font_style -> Pp.string ctx "font-style"
  | Text_align -> Pp.string ctx "text-align"
  | Text_decoration -> Pp.string ctx "text-decoration"
  | Text_decoration_style -> Pp.string ctx "text-decoration-style"
  | Text_decoration_color -> Pp.string ctx "text-decoration-color"
  | Text_decoration_thickness -> Pp.string ctx "text-decoration-thickness"
  | Text_underline_offset -> Pp.string ctx "text-underline-offset"
  | Text_transform -> Pp.string ctx "text-transform"
  | Letter_spacing -> Pp.string ctx "letter-spacing"
  | List_style_type -> Pp.string ctx "list-style-type"
  | List_style_position -> Pp.string ctx "list-style-position"
  | List_style_image -> Pp.string ctx "list-style-image"
  | Display -> Pp.string ctx "display"
  | Position -> Pp.string ctx "position"
  | Visibility -> Pp.string ctx "visibility"
  | Flex_direction -> Pp.string ctx "flex-direction"
  | Flex_wrap -> Pp.string ctx "flex-wrap"
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
  | Border_radius -> Pp.string ctx "border-radius"
  | Border_top_color -> Pp.string ctx "border-top-color"
  | Border_right_color -> Pp.string ctx "border-right-color"
  | Border_bottom_color -> Pp.string ctx "border-bottom-color"
  | Border_left_color -> Pp.string ctx "border-left-color"
  | Border_inline_start_color -> Pp.string ctx "border-inline-start-color"
  | Border_inline_end_color -> Pp.string ctx "border-inline-end-color"
  | Box_shadow -> Pp.string ctx "box-shadow"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | Stroke_width -> Pp.string ctx "stroke-width"
  | Opacity -> Pp.string ctx "opacity"
  | Mix_blend_mode -> Pp.string ctx "mix-blend-mode"
  | Transition -> Pp.string ctx "transition"
  | Transform -> Pp.string ctx "transform"
  | Scale -> Pp.string ctx "scale"
  | Cursor -> Pp.string ctx "cursor"
  | Table_layout -> Pp.string ctx "table-layout"
  | Border_collapse -> Pp.string ctx "border-collapse"
  | Border_spacing -> Pp.string ctx "border-spacing"
  | User_select -> Pp.string ctx "user-select"
  | Pointer_events -> Pp.string ctx "pointer-events"
  | Overflow -> Pp.string ctx "overflow"
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
  | Border -> Pp.string ctx "border"
  | Background -> Pp.string ctx "background"
  | Tab_size -> Pp.string ctx "tab-size"
  | Webkit_text_size_adjust -> Pp.string ctx "-webkit-text-size-adjust"
  | Font_feature_settings -> Pp.string ctx "font-feature-settings"
  | Font_variation_settings -> Pp.string ctx "font-variation-settings"
  | Webkit_tap_highlight_color -> Pp.string ctx "-webkit-tap-highlight-color"
  | Webkit_text_decoration -> Pp.string ctx "-webkit-text-decoration"
  | Webkit_text_decoration_color ->
      Pp.string ctx "-webkit-text-decoration-color"
  | Text_indent -> Pp.string ctx "text-indent"
  | List_style -> Pp.string ctx "list-style"
  | Font -> Pp.string ctx "font"
  | Webkit_appearance -> Pp.string ctx "-webkit-appearance"
  | Container_type -> Pp.string ctx "container-type"
  | Container_name -> Pp.string ctx "container-name"
  | Perspective -> Pp.string ctx "perspective"
  | Perspective_origin -> Pp.string ctx "perspective-origin"
  | Transform_style -> Pp.string ctx "transform-style"
  | Backface_visibility -> Pp.string ctx "backface-visibility"
  | Object_position -> Pp.string ctx "object-position"
  | Rotate -> Pp.string ctx "rotate"
  | Transition_duration -> Pp.string ctx "transition-duration"
  | Transition_timing_function -> Pp.string ctx "transition-timing-function"
  | Transition_delay -> Pp.string ctx "transition-delay"
  | Will_change -> Pp.string ctx "will-change"
  | Contain -> Pp.string ctx "contain"
  | Isolation -> Pp.string ctx "isolation"
  | Word_spacing -> Pp.string ctx "word-spacing"
  | Background_attachment -> Pp.string ctx "background-attachment"
  | Border_top -> Pp.string ctx "border-top"
  | Border_right -> Pp.string ctx "border-right"
  | Border_bottom -> Pp.string ctx "border-bottom"
  | Border_left -> Pp.string ctx "border-left"
  | Transform_origin -> Pp.string ctx "transform-origin"
  | Text_shadow -> Pp.string ctx "text-shadow"
  | Clip_path -> Pp.string ctx "clip-path"
  | Mask -> Pp.string ctx "mask"
  | Content_visibility -> Pp.string ctx "content-visibility"
  | Filter -> Pp.string ctx "filter"
  | Background_image -> Pp.string ctx "background-image"
  | Animation -> Pp.string ctx "animation"
  | Aspect_ratio -> Pp.string ctx "aspect-ratio"
  | Overflow_x -> Pp.string ctx "overflow-x"
  | Overflow_y -> Pp.string ctx "overflow-y"
  | Vertical_align -> Pp.string ctx "vertical-align"
  | Font_family -> Pp.string ctx "font-family"
  | Background_position -> Pp.string ctx "background-position"
  | Background_repeat -> Pp.string ctx "background-repeat"
  | Background_size -> Pp.string ctx "background-size"
  | Webkit_font_smoothing -> Pp.string ctx "-webkit-font-smoothing"
  | Moz_osx_font_smoothing -> Pp.string ctx "-moz-osx-font-smoothing"
  | Webkit_line_clamp -> Pp.string ctx "-webkit-line-clamp"
  | Webkit_box_orient -> Pp.string ctx "-webkit-box-orient"
  | Text_overflow -> Pp.string ctx "text-overflow"
  | Text_wrap -> Pp.string ctx "text-wrap"
  | Word_break -> Pp.string ctx "word-break"
  | Overflow_wrap -> Pp.string ctx "overflow-wrap"
  | Hyphens -> Pp.string ctx "hyphens"
  | Webkit_hyphens -> Pp.string ctx "-webkit-hyphens"
  | Font_stretch -> Pp.string ctx "font-stretch"
  | Font_variant_numeric -> Pp.string ctx "font-variant-numeric"
  | Backdrop_filter -> Pp.string ctx "backdrop-filter"
  | Scroll_snap_align -> Pp.string ctx "scroll-snap-align"
  | Scroll_snap_stop -> Pp.string ctx "scroll-snap-stop"
  | Scroll_behavior -> Pp.string ctx "scroll-behavior"
  | Box_sizing -> Pp.string ctx "box-sizing"
  | Resize -> Pp.string ctx "resize"
  | Object_fit -> Pp.string ctx "object-fit"
  | Appearance -> Pp.string ctx "appearance"
  | Content -> Pp.string ctx "content"
  | Quotes -> Pp.string ctx "quotes"
  | Text_size_adjust -> Pp.string ctx "text-size-adjust"
  | Touch_action -> Pp.string ctx "touch-action"
  | Direction -> Pp.string ctx "direction"
  | Unicode_bidi -> Pp.string ctx "unicode-bidi"
  | Writing_mode -> Pp.string ctx "writing-mode"
  | Text_decoration_skip_ink -> Pp.string ctx "text-decoration-skip-ink"
  | Animation_name -> Pp.string ctx "animation-name"
  | Animation_duration -> Pp.string ctx "animation-duration"
  | Animation_timing_function -> Pp.string ctx "animation-timing-function"
  | Animation_delay -> Pp.string ctx "animation-delay"
  | Animation_iteration_count -> Pp.string ctx "animation-iteration-count"
  | Animation_direction -> Pp.string ctx "animation-direction"
  | Animation_fill_mode -> Pp.string ctx "animation-fill-mode"
  | Animation_play_state -> Pp.string ctx "animation-play-state"
  | Background_blend_mode -> Pp.string ctx "background-blend-mode"
  | Scroll_margin -> Pp.string ctx "scroll-margin"
  | Scroll_margin_top -> Pp.string ctx "scroll-margin-top"
  | Scroll_margin_right -> Pp.string ctx "scroll-margin-right"
  | Scroll_margin_bottom -> Pp.string ctx "scroll-margin-bottom"
  | Scroll_margin_left -> Pp.string ctx "scroll-margin-left"
  | Scroll_padding -> Pp.string ctx "scroll-padding"
  | Scroll_padding_top -> Pp.string ctx "scroll-padding-top"
  | Scroll_padding_right -> Pp.string ctx "scroll-padding-right"
  | Scroll_padding_bottom -> Pp.string ctx "scroll-padding-bottom"
  | Scroll_padding_left -> Pp.string ctx "scroll-padding-left"
  | Overscroll_behavior -> Pp.string ctx "overscroll-behavior"
  | Overscroll_behavior_x -> Pp.string ctx "overscroll-behavior-x"
  | Overscroll_behavior_y -> Pp.string ctx "overscroll-behavior-y"
  | Accent_color -> Pp.string ctx "accent-color"
  | Caret_color -> Pp.string ctx "caret-color"
  | Webkit_transform -> Pp.string ctx "-webkit-transform"
  | Webkit_transition -> Pp.string ctx "-webkit-transition"
  | Webkit_filter -> Pp.string ctx "-webkit-filter"
  | Moz_appearance -> Pp.string ctx "-moz-appearance"
  | Ms_filter -> Pp.string ctx "-ms-filter"
  | O_transition -> Pp.string ctx "-o-transition"

let rec pp_font_feature_settings : font_feature_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Feature_list s -> Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_feature_settings ctx v

let rec pp_font_variation_settings : font_variation_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Axis_list s -> Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_variation_settings ctx v

let pp_rotate_3d : (float * float * float * angle) Pp.t =
 fun ctx (x, y, z, a) ->
  Pp.string ctx "rotate3d(";
  Pp.float ctx x;
  Pp.comma ctx ();
  Pp.float ctx y;
  Pp.comma ctx ();
  Pp.float ctx z;
  Pp.comma ctx ();
  pp_angle ctx a;
  Pp.char ctx ')'

let pp_translate_3d : (length * length * length) Pp.t =
 fun ctx (x, y, z) ->
  Pp.string ctx "translate3d(";
  pp_length ctx x;
  Pp.comma ctx ();
  pp_length ctx y;
  Pp.comma ctx ();
  pp_length ctx z;
  Pp.char ctx ')'

let pp_matrix_3d : _ Pp.t =
 fun ctx (a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4) ->
  let values =
    [ a1; a2; a3; a4; b1; b2; b3; b4; c1; c2; c3; c4; d1; d2; d3; d4 ]
  in
  Pp.string ctx "matrix3d(";
  Pp.list ~sep:Pp.comma Pp.float ctx values;
  Pp.char ctx ')'

let rec pp_transform : transform Pp.t =
 fun ctx -> function
  | None -> pp_keyword "none" ctx
  | Translate (x, None) -> Pp.call "translate" pp_length ctx x
  | Translate (x, Some y) -> Pp.call_2 "translate" pp_length pp_length ctx (x, y)
  | Translate_x x -> Pp.call "translateX" pp_length ctx x
  | Translate_y y -> Pp.call "translateY" pp_length ctx y
  | Translate_z z -> Pp.call "translateZ" pp_length ctx z
  | Translate_3d (x, y, z) -> pp_translate_3d ctx (x, y, z)
  | Rotate a -> Pp.call "rotate" pp_angle ctx a
  | Rotate_x a -> Pp.call "rotateX" pp_angle ctx a
  | Rotate_y a -> Pp.call "rotateY" pp_angle ctx a
  | Rotate_z a -> Pp.call "rotateZ" pp_angle ctx a
  | Rotate_3d (x, y, z, a) -> pp_rotate_3d ctx (x, y, z, a)
  | Scale (x, None) -> Pp.call "scale" Pp.float ctx x
  | Scale (x, Some y) -> Pp.call_2 "scale" Pp.float Pp.float ctx (x, y)
  | Scale_x f -> Pp.call "scaleX" Pp.float ctx f
  | Scale_y f -> Pp.call "scaleY" Pp.float ctx f
  | Scale_z f -> Pp.call "scaleZ" Pp.float ctx f
  | Scale_3d (x, y, z) ->
      Pp.call_3 "scale3d" Pp.float Pp.float Pp.float ctx (x, y, z)
  | Skew (x, None) -> Pp.call "skew" pp_angle ctx x
  | Skew (x, Some y) -> Pp.call_2 "skew" pp_angle pp_angle ctx (x, y)
  | Skew_x x -> Pp.call "skewX" pp_angle ctx x
  | Skew_y y -> Pp.call "skewY" pp_angle ctx y
  | Matrix (a, b, c, d, e, f) ->
      Pp.call_list "matrix" Pp.float ctx [ a; b; c; d; e; f ]
  | Matrix_3d m -> pp_matrix_3d ctx m
  | Perspective p -> Pp.call "perspective" pp_length ctx p
  | Inherit -> pp_keyword "inherit" ctx
  | Var v -> pp_var (Pp.list ~sep:Pp.space pp_transform) ctx v

let pp_transform_style : transform_style Pp.t =
 fun ctx -> function
  | Flat -> Pp.string ctx "flat"
  | Preserve_3d -> Pp.string ctx "preserve-3d"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_blend_mode : blend_mode Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Multiply -> Pp.string ctx "multiply"
  | Screen -> Pp.string ctx "screen"
  | Overlay -> Pp.string ctx "overlay"
  | Darken -> Pp.string ctx "darken"
  | Lighten -> Pp.string ctx "lighten"
  | Color_dodge -> Pp.string ctx "color-dodge"
  | Color_burn -> Pp.string ctx "color-burn"
  | Hard_light -> Pp.string ctx "hard-light"
  | Soft_light -> Pp.string ctx "soft-light"
  | Difference -> Pp.string ctx "difference"
  | Exclusion -> Pp.string ctx "exclusion"
  | Hue -> Pp.string ctx "hue"
  | Saturation -> Pp.string ctx "saturation"
  | Color -> Pp.string ctx "color"
  | Luminosity -> Pp.string ctx "luminosity"
  | Var v -> pp_var pp_blend_mode ctx v

let rec pp_box_shadow : box_shadow Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Shadow { inset; h_offset; v_offset; blur; spread; color } -> (
      (* Special case: box-shadow: 0 0 is equivalent to none when minifying *)
      match (ctx.minify, inset, h_offset, v_offset, blur, spread, color) with
      | true, false, Zero, Zero, None, None, None -> Pp.string ctx "none"
      | _ -> (
          if inset then (
            Pp.string ctx "inset";
            Pp.space ctx ());
          pp_length ctx h_offset;
          Pp.space ctx ();
          pp_length ctx v_offset;
          (match blur with
          | Some b ->
              Pp.space ctx ();
              pp_length ctx b
          | None -> ());
          (match spread with
          | Some s ->
              Pp.space ctx ();
              pp_length ctx s
          | None -> ());
          match color with
          | Some c ->
              Pp.space ctx ();
              pp_color ctx c
          | None -> ()))
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_box_shadow ctx v

let pp_text_shadow : text_shadow Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Text_shadow { h_offset; v_offset; blur; color } -> (
      pp_length ctx h_offset;
      Pp.space ctx ();
      pp_length ctx v_offset;
      (match blur with
      | Some b ->
          Pp.space ctx ();
          pp_length ctx b
      | None -> ());
      match color with
      | Some c ->
          Pp.space ctx ();
          pp_color ctx c
      | None -> ())

let pp_background_attachment : background_attachment Pp.t =
 fun ctx -> function
  | Fixed -> Pp.string ctx "fixed"
  | Local -> Pp.string ctx "local"
  | Scroll -> Pp.string ctx "scroll"
  | Inherit -> Pp.string ctx "inherit"

let pp_background_repeat : background_repeat Pp.t =
 fun ctx -> function
  | Repeat -> Pp.string ctx "repeat"
  | Repeat_x -> Pp.string ctx "repeat-x"
  | Repeat_y -> Pp.string ctx "repeat-y"
  | No_repeat -> Pp.string ctx "no-repeat"
  | Space -> Pp.string ctx "space"
  | Round -> Pp.string ctx "round"
  | Inherit -> Pp.string ctx "inherit"

let pp_background_box : background_box Pp.t =
 fun ctx -> function
  | Border_box -> Pp.string ctx "border-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Content_box -> Pp.string ctx "content-box"
  | Text -> Pp.string ctx "text"
  | Inherit -> Pp.string ctx "inherit"

let pp_position_component : position_component Pp.t =
 fun ctx -> function
  | Left -> Pp.string ctx "left"
  | Center -> Pp.string ctx "center"
  | Right -> Pp.string ctx "right"
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.unit ctx p "%"
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"

let pp_position_2d : position_2d Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Center -> Pp.string ctx "center"
  | XY (a, b) ->
      pp_position_component ctx a;
      Pp.space ctx ();
      pp_position_component ctx b

let pp_background_size : background_size Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Cover -> Pp.string ctx "cover"
  | Contain -> Pp.string ctx "contain"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.unit ctx p "%"
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"
  | Size (w, h) ->
      pp_length ctx w;
      Pp.char ctx ' ';
      pp_length ctx h
  | Inherit -> Pp.string ctx "inherit"

let pp_background : background Pp.t =
 fun ctx bg ->
  let first = ref true in
  let maybe_space () = if !first then first := false else Pp.space ctx () in
  (* Add image if present *)
  (match bg.image with
  | Some img ->
      maybe_space ();
      pp_background_image ctx img
  | None -> ());
  (* Add position if present *)
  (match bg.position with
  | Some pos ->
      maybe_space ();
      pp_position_2d ctx pos
  | None -> ());
  (* Add size if present (must follow position with /) *)
  (match bg.size with
  | Some size when bg.position <> None ->
      Pp.string ctx "/";
      pp_background_size ctx size
  | Some size ->
      maybe_space ();
      pp_background_size ctx size
  | None -> ());
  (* Add repeat if present *)
  (match bg.repeat with
  | Some rep ->
      maybe_space ();
      pp_background_repeat ctx rep
  | None -> ());
  (* Add attachment if present *)
  (match bg.attachment with
  | Some att ->
      maybe_space ();
      pp_background_attachment ctx att
  | None -> ());
  (* Add origin if present *)
  (match bg.origin with
  | Some orig ->
      maybe_space ();
      pp_background_box ctx orig
  | None -> ());
  (* Add clip if present *)
  (match bg.clip with
  | Some cl ->
      maybe_space ();
      pp_background_box ctx cl
  | None -> ());
  (* Add color last *)
  (match bg.color with
  | Some c ->
      maybe_space ();
      pp_color ctx c
  | None -> ());
  (* If nothing was set, output 'none' *)
  if !first then Pp.string ctx "none"

let pp_gap : gap Pp.t =
 fun ctx gap ->
  match (gap.row_gap, gap.column_gap) with
  | Some row, Some col when row = col ->
      (* Single value when both gaps are equal *)
      pp_length ctx row
  | Some row, Some col ->
      (* Two values when different *)
      pp_length ctx row;
      Pp.space ctx ();
      pp_length ctx col
  | Some row, None | None, Some row ->
      (* Single value *)
      pp_length ctx row
  | None, None ->
      (* Fallback - shouldn't happen with proper parsing *)
      Pp.string ctx "0"

let pp_transform_origin : transform_origin Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Center -> Pp.string ctx "center"
  | XY (a, b) ->
      pp_position_component ctx a;
      Pp.space ctx ();
      pp_position_component ctx b
  | XYZ (a, b, z) ->
      pp_position_component ctx a;
      Pp.space ctx ();
      pp_position_component ctx b;
      Pp.space ctx ();
      pp_length ctx z

(* Helpers for typed positions (shorter than direct constructors) *)
let pos_left : position_2d = XY (Left, Center)
let pos_right : position_2d = XY (Right, Center)
let pos_top : position_2d = XY (Center, Top)
let pos_bottom : position_2d = XY (Center, Bottom)

(* Helpers for transform-origin *)
let origin (a : position_component) (b : position_component) : transform_origin
    =
  XY (a, b)

let origin3d (a : position_component) (b : position_component) (z : length) :
    transform_origin =
  XYZ (a, b, z)

let pp_animation : animation Pp.t =
 fun ctx anim ->
  let opt pp f =
    Pp.option
      (fun ctx v ->
        Pp.char ctx ' ';
        pp ctx v)
      ctx f
  in
  let pp_iter_count ctx = function
    | Infinite -> Pp.string ctx "infinite"
    | Num n -> Pp.float ctx n
  in
  let pp_timing ctx = function
    | Linear -> Pp.string ctx "linear"
    | Ease -> Pp.string ctx "ease"
    | Ease_in -> Pp.string ctx "ease-in"
    | Ease_out -> Pp.string ctx "ease-out"
    | Ease_in_out -> Pp.string ctx "ease-in-out"
    | Step_start -> Pp.string ctx "step-start"
    | Step_end -> Pp.string ctx "step-end"
    | Cubic_bezier (x1, y1, x2, y2) ->
        Pp.string ctx "cubic-bezier(";
        Pp.float ctx x1;
        Pp.char ctx ',';
        Pp.float ctx y1;
        Pp.char ctx ',';
        Pp.float ctx x2;
        Pp.char ctx ',';
        Pp.float ctx y2;
        Pp.char ctx ')'
    | Steps (steps, direction) ->
        Pp.string ctx "steps(";
        Pp.int ctx steps;
        (match direction with
        | Some `Start -> Pp.string ctx ",start"
        | Some `End -> Pp.string ctx ",end"
        | Some `Jump_start -> Pp.string ctx ",jump-start"
        | Some `Jump_end -> Pp.string ctx ",jump-end"
        | Some `Jump_none -> Pp.string ctx ",jump-none"
        | Some `Jump_both -> Pp.string ctx ",jump-both"
        | None -> ());
        Pp.char ctx ')'
  in
  match anim.name with
  | None -> Pp.string ctx "none"
  | Some name ->
      Pp.string ctx name;
      opt pp_duration anim.duration;
      opt pp_timing anim.timing_function;
      opt pp_iter_count anim.iteration_count;
      opt pp_duration anim.delay

let pp_animation_direction : animation_direction Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Reverse -> Pp.string ctx "reverse"
  | Alternate -> Pp.string ctx "alternate"
  | Alternate_reverse -> Pp.string ctx "alternate-reverse"

let pp_animation_fill_mode : animation_fill_mode Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Forwards -> Pp.string ctx "forwards"
  | Backwards -> Pp.string ctx "backwards"
  | Both -> Pp.string ctx "both"

let pp_animation_iteration_count : animation_iteration_count Pp.t =
 fun ctx -> function
  | Infinite -> Pp.string ctx "infinite"
  | Num n -> Pp.float ctx n

let pp_animation_play_state : animation_play_state Pp.t =
 fun ctx -> function
  | Running -> Pp.string ctx "running"
  | Paused -> Pp.string ctx "paused"

let pp_appearance : appearance Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Inherit -> Pp.string ctx "inherit"

let pp_backface_visibility : backface_visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Inherit -> Pp.string ctx "inherit"

let pp_border_collapse : border_collapse Pp.t =
 fun ctx -> function
  | Collapse -> Pp.string ctx "collapse"
  | Separate -> Pp.string ctx "separate"
  | Inherit -> Pp.string ctx "inherit"

let pp_box_sizing : box_sizing Pp.t =
 fun ctx -> function
  | Border_box -> Pp.string ctx "border-box"
  | Content_box -> Pp.string ctx "content-box"
  | Inherit -> Pp.string ctx "inherit"

let pp_clear : clear Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Both -> Pp.string ctx "both"

let rec pp_contain : contain Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Strict -> Pp.string ctx "strict"
  | Content -> Pp.string ctx "content"
  | Size -> Pp.string ctx "size"
  | Layout -> Pp.string ctx "layout"
  | Style -> Pp.string ctx "style"
  | Paint -> Pp.string ctx "paint"
  | List items -> Pp.list ~sep:Pp.space pp_contain ctx items

let pp_container_type : container_type Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Size -> Pp.string ctx "size"
  | Inline_size -> Pp.string ctx "inline-size"

let rec pp_content : content Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Open_quote -> Pp.string ctx "open-quote"
  | Close_quote -> Pp.string ctx "close-quote"
  | Var v -> pp_var pp_content ctx v

let rec pp_content_visibility : content_visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_content_visibility ctx v

let pp_cursor : cursor Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Default -> Pp.string ctx "default"
  | Pointer -> Pp.string ctx "pointer"
  | Wait -> Pp.string ctx "wait"
  | Text -> Pp.string ctx "text"
  | Move -> Pp.string ctx "move"
  | Help -> Pp.string ctx "help"
  | Not_allowed -> Pp.string ctx "not-allowed"
  | None -> Pp.string ctx "none"
  | Context_menu -> Pp.string ctx "context-menu"
  | Progress -> Pp.string ctx "progress"
  | Cell -> Pp.string ctx "cell"
  | Crosshair -> Pp.string ctx "crosshair"
  | Vertical_text -> Pp.string ctx "vertical-text"
  | Alias -> Pp.string ctx "alias"
  | Copy -> Pp.string ctx "copy"
  | No_drop -> Pp.string ctx "no-drop"
  | Grab -> Pp.string ctx "grab"
  | Grabbing -> Pp.string ctx "grabbing"
  | All_scroll -> Pp.string ctx "all-scroll"
  | Col_resize -> Pp.string ctx "col-resize"
  | Row_resize -> Pp.string ctx "row-resize"
  | N_resize -> Pp.string ctx "n-resize"
  | E_resize -> Pp.string ctx "e-resize"
  | S_resize -> Pp.string ctx "s-resize"
  | W_resize -> Pp.string ctx "w-resize"
  | Ne_resize -> Pp.string ctx "ne-resize"
  | Nw_resize -> Pp.string ctx "nw-resize"
  | Se_resize -> Pp.string ctx "se-resize"
  | Sw_resize -> Pp.string ctx "sw-resize"
  | Ew_resize -> Pp.string ctx "ew-resize"
  | Ns_resize -> Pp.string ctx "ns-resize"
  | Nesw_resize -> Pp.string ctx "nesw-resize"
  | Nwse_resize -> Pp.string ctx "nwse-resize"
  | Zoom_in -> Pp.string ctx "zoom-in"
  | Zoom_out -> Pp.string ctx "zoom-out"
  | Url (url, coords) -> (
      Pp.url ctx url;
      match coords with
      | Some (x, y) ->
          Pp.char ctx ' ';
          Pp.float ctx x;
          Pp.char ctx ' ';
          Pp.float ctx y
      | None -> ())
  | Inherit -> Pp.string ctx "inherit"

let pp_direction : direction Pp.t =
 fun ctx -> function
  | Ltr -> Pp.string ctx "ltr"
  | Rtl -> Pp.string ctx "rtl"
  | Inherit -> Pp.string ctx "inherit"

let pp_isolation : isolation Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Isolate -> Pp.string ctx "isolate"
  | Inherit -> Pp.string ctx "inherit"

let pp_scroll_snap_align : scroll_snap_align Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"

let pp_scroll_snap_stop : scroll_snap_stop Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Always -> Pp.string ctx "always"
  | Inherit -> Pp.string ctx "inherit"

let pp_scroll_behavior : scroll_behavior Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | Inherit -> Pp.string ctx "inherit"

let pp_resize : resize Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Both -> Pp.string ctx "both"
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inherit -> Pp.string ctx "inherit"

let pp_object_fit : object_fit Pp.t =
 fun ctx -> function
  | Fill -> Pp.string ctx "fill"
  | Contain -> Pp.string ctx "contain"
  | Cover -> Pp.string ctx "cover"
  | None -> Pp.string ctx "none"
  | Scale_down -> Pp.string ctx "scale-down"
  | Inherit -> Pp.string ctx "inherit"

let pp_font_stretch : font_stretch Pp.t =
 fun ctx -> function
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Ultra_condensed -> Pp.string ctx "ultra-condensed"
  | Extra_condensed -> Pp.string ctx "extra-condensed"
  | Condensed -> Pp.string ctx "condensed"
  | Semi_condensed -> Pp.string ctx "semi-condensed"
  | Normal -> Pp.string ctx "normal"
  | Semi_expanded -> Pp.string ctx "semi-expanded"
  | Expanded -> Pp.string ctx "expanded"
  | Extra_expanded -> Pp.string ctx "extra-expanded"
  | Ultra_expanded -> Pp.string ctx "ultra-expanded"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_font_variant_numeric_token : font_variant_numeric_token Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Lining_nums -> Pp.string ctx "lining-nums"
  | Oldstyle_nums -> Pp.string ctx "oldstyle-nums"
  | Proportional_nums -> Pp.string ctx "proportional-nums"
  | Tabular_nums -> Pp.string ctx "tabular-nums"
  | Diagonal_fractions -> Pp.string ctx "diagonal-fractions"
  | Stacked_fractions -> Pp.string ctx "stacked-fractions"
  | Ordinal -> Pp.string ctx "ordinal"
  | Slashed_zero -> Pp.string ctx "slashed-zero"
  | Var v -> pp_var pp_font_variant_numeric_token ctx v

and pp_font_variant_numeric : font_variant_numeric Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Tokens tokens ->
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens
  | Var v -> pp_var pp_font_variant_numeric ctx v
  | Composed
      {
        ordinal;
        slashed_zero;
        numeric_figure;
        numeric_spacing;
        numeric_fraction;
      } ->
      let tokens =
        [
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        ]
        |> List.filter_map (fun x -> x)
      in
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens

let pp_text_size_adjust : text_size_adjust Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Pct n -> Pp.unit ctx n "%"
  | Inherit -> Pp.string ctx "inherit"

let pp_webkit_font_smoothing : webkit_font_smoothing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Antialiased -> Pp.string ctx "antialiased"
  | Subpixel_antialiased -> Pp.string ctx "subpixel-antialiased"
  | Inherit -> Pp.string ctx "inherit"

let pp_scroll_snap_axis : scroll_snap_axis Pp.t =
 fun ctx -> function
  | X -> Pp.string ctx "x"
  | Y -> Pp.string ctx "y"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Both -> Pp.string ctx "both"

let rec pp_scroll_snap_strictness : scroll_snap_strictness Pp.t =
 fun ctx -> function
  | Proximity -> Pp.string ctx "proximity"
  | Mandatory -> Pp.string ctx "mandatory"
  | Var v -> pp_var pp_scroll_snap_strictness ctx v

let pp_scroll_snap_type : scroll_snap_type Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Axis (axis, strictness) -> (
      pp_scroll_snap_axis ctx axis;
      match strictness with
      | None -> ()
      | Some s ->
          Pp.space ctx ();
          pp_scroll_snap_strictness ctx s)
  | Inherit -> Pp.string ctx "inherit"

let pp_grid_track_breadth : grid_track_breadth Pp.t =
 fun ctx -> function
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct f -> Pp.unit ctx f "%"
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Zero -> Pp.char ctx '0'
  | Fr f -> Pp.unit ctx f "fr"
  | Auto -> Pp.string ctx "auto"
  | Min_content -> Pp.string ctx "min-content"
  | Max_content -> Pp.string ctx "max-content"

let rec pp_grid_track_size : grid_track_size Pp.t =
 fun ctx -> function
  | Track_size breadth -> pp_grid_track_breadth ctx breadth
  | Min_max (min, max) ->
      Pp.call_2 "minmax" pp_grid_track_breadth pp_grid_track_breadth ctx
        (min, max)
  | Fit_content l -> Pp.call "fit-content" pp_length ctx l
  | Repeat (count, sizes) ->
      Pp.call "repeat"
        (fun ctx (count, sizes) ->
          Pp.int ctx count;
          Pp.comma ctx ();
          Pp.space ctx ();
          Pp.list ~sep:Pp.space pp_grid_track_size ctx sizes)
        ctx (count, sizes)

let pp_grid_template : grid_template Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Tracks sizes -> Pp.list ~sep:Pp.space pp_grid_track_size ctx sizes
  | Named_tracks tracks ->
      let pp_named_track ctx (name, size) =
        (match name with
        | Some n ->
            Pp.char ctx '[';
            Pp.string ctx n;
            Pp.string ctx "] "
        | None -> ());
        pp_grid_track_size ctx size
      in
      Pp.list ~sep:Pp.space pp_named_track ctx tracks
  | Subgrid -> Pp.string ctx "subgrid"
  | Masonry -> Pp.string ctx "masonry"

let rec pp_flex_basis : flex_basis Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Content -> Pp.string ctx "content"
  | Px f -> Pp.unit ctx f "px"
  | Cm f -> Pp.unit ctx f "cm"
  | Mm f -> Pp.unit ctx f "mm"
  | Q f -> Pp.unit ctx f "q"
  | In f -> Pp.unit ctx f "in"
  | Pt f -> Pp.unit ctx f "pt"
  | Pc f -> Pp.unit ctx f "pc"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Ex f -> Pp.unit ctx f "ex"
  | Cap f -> Pp.unit ctx f "cap"
  | Ic f -> Pp.unit ctx f "ic"
  | Rlh f -> Pp.unit ctx f "rlh"
  | Pct f -> Pp.unit ctx f "%"
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Vi f -> Pp.unit ctx f "vi"
  | Vb f -> Pp.unit ctx f "vb"
  | Dvh f -> Pp.unit ctx f "dvh"
  | Dvw f -> Pp.unit ctx f "dvw"
  | Dvmin f -> Pp.unit ctx f "dvmin"
  | Dvmax f -> Pp.unit ctx f "dvmax"
  | Lvh f -> Pp.unit ctx f "lvh"
  | Lvw f -> Pp.unit ctx f "lvw"
  | Lvmin f -> Pp.unit ctx f "lvmin"
  | Lvmax f -> Pp.unit ctx f "lvmax"
  | Svh f -> Pp.unit ctx f "svh"
  | Svw f -> Pp.unit ctx f "svw"
  | Svmin f -> Pp.unit ctx f "svmin"
  | Svmax f -> Pp.unit ctx f "svmax"
  | Ch f -> Pp.unit ctx f "ch"
  | Lh f -> Pp.unit ctx f "lh"
  | Num f -> Pp.float ctx f
  | Zero -> Pp.char ctx '0'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Fit_content -> Pp.string ctx "fit-content"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | From_font -> Pp.string ctx "from-font"
  | Var v -> pp_var pp_flex_basis ctx v
  | Calc cv -> pp_calc pp_flex_basis ctx cv

let pp_flex : flex Pp.t =
 fun ctx -> function
  | Initial -> Pp.string ctx "initial"
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Grow f -> Pp.float ctx f
  | Basis fb -> pp_flex_basis ctx fb
  | Grow_shrink (grow, shrink) ->
      Pp.float ctx grow;
      Pp.space ctx ();
      Pp.float ctx shrink
  | Full (grow, shrink, basis) ->
      Pp.float ctx grow;
      Pp.space ctx ();
      Pp.float ctx shrink;
      Pp.space ctx ();
      pp_flex_basis ctx basis

let pp_place_content : place_content Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Align_justify (a, j) ->
      pp_align ctx a;
      Pp.space ctx ();
      pp_justify ctx j

let pp_place_items : place_items Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Align_justify (a, j) ->
      pp_align_items ctx a;
      Pp.space ctx ();
      pp_justify ctx j

let pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Grayscale -> Pp.string ctx "grayscale"
  | Inherit -> Pp.string ctx "inherit"

(* Helpers for timing-function pretty printing *)
let pp_steps_args :
    (int
    * [ `Jump_start | `Jump_end | `Jump_none | `Jump_both | `Start | `End ]
      option)
    Pp.t =
 fun ctx (n, kind) ->
  Pp.int ctx n;
  match kind with
  | Some `Jump_start ->
      Pp.comma ctx ();
      Pp.string ctx "jump-start"
  | Some `Jump_end ->
      Pp.comma ctx ();
      Pp.string ctx "jump-end"
  | Some `Jump_none ->
      Pp.comma ctx ();
      Pp.string ctx "jump-none"
  | Some `Jump_both ->
      Pp.comma ctx ();
      Pp.string ctx "jump-both"
  | Some `Start ->
      Pp.comma ctx ();
      Pp.string ctx "start"
  | Some `End ->
      Pp.comma ctx ();
      Pp.string ctx "end"
  | None -> ()

let pp_steps = Pp.call "steps" pp_steps_args

let pp_cubic_bezier_args : (float * float * float * float) Pp.t =
 fun ctx (a, b, c, d) -> Pp.list ~sep:Pp.comma Pp.float ctx [ a; b; c; d ]

let pp_cubic_bezier = Pp.call "cubic-bezier" pp_cubic_bezier_args

let pp_timing_function : timing_function Pp.t =
 fun ctx -> function
  | Ease -> Pp.string ctx "ease"
  | Linear -> Pp.string ctx "linear"
  | Ease_in -> Pp.string ctx "ease-in"
  | Ease_out -> Pp.string ctx "ease-out"
  | Ease_in_out -> Pp.string ctx "ease-in-out"
  | Step_start -> Pp.string ctx "step-start"
  | Step_end -> Pp.string ctx "step-end"
  | Steps (n, jump_term_opt) -> pp_steps ctx (n, jump_term_opt)
  | Cubic_bezier (x1, y1, x2, y2) -> pp_cubic_bezier ctx (x1, y1, x2, y2)

let pp_svg_paint : svg_paint Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Current_color -> Pp.string ctx "currentcolor"
  | Color c -> pp_color ctx c

let pp_transition_property : transition_property Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Property s -> Pp.string ctx s

let pp_transition : transition Pp.t =
 fun ctx { property; duration; timing_function; delay } ->
  pp_transition_property ctx property;
  (match duration with
  | Some d ->
      Pp.space ctx ();
      pp_duration ctx d
  | None -> ());
  (match timing_function with
  | Some tf ->
      Pp.space ctx ();
      pp_timing_function ctx tf
  | None -> ());
  match delay with
  | Some d ->
      Pp.space ctx ();
      pp_duration ctx d
  | None -> ()

let rec pp_scale : scale Pp.t =
 fun ctx -> function
  | X f -> Pp.float ctx f
  | XY (x, y) ->
      Pp.float ctx x;
      Pp.space ctx ();
      Pp.float ctx y
  | XYZ (x, y, z) ->
      Pp.float ctx x;
      Pp.space ctx ();
      Pp.float ctx y;
      Pp.space ctx ();
      Pp.float ctx z
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_scale ctx v

let pp_outline_style : outline_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Solid -> Pp.string ctx "solid"
  | Dashed -> Pp.string ctx "dashed"
  | Dotted -> Pp.string ctx "dotted"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Auto -> Pp.string ctx "auto"

let pp_forced_color_adjust : forced_color_adjust Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"

let pp_float_side : float_side Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Inherit -> Pp.string ctx "inherit"

let pp_touch_action : touch_action Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pan_x -> Pp.string ctx "pan-x"
  | Pan_y -> Pp.string ctx "pan-y"
  | Manipulation -> Pp.string ctx "manipulation"
  | Inherit -> Pp.string ctx "inherit"

let pp_unicode_bidi : unicode_bidi Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Embed -> Pp.string ctx "embed"
  | Isolate -> Pp.string ctx "isolate"
  | Bidi_override -> Pp.string ctx "bidi-override"
  | Isolate_override -> Pp.string ctx "isolate-override"
  | Plaintext -> Pp.string ctx "plaintext"
  | Inherit -> Pp.string ctx "inherit"

let pp_writing_mode : writing_mode Pp.t =
 fun ctx -> function
  | Horizontal_tb -> Pp.string ctx "horizontal-tb"
  | Vertical_rl -> Pp.string ctx "vertical-rl"
  | Vertical_lr -> Pp.string ctx "vertical-lr"
  | Inherit -> Pp.string ctx "inherit"

let pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"

let pp_overscroll_behavior : overscroll_behavior Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Contain -> Pp.string ctx "contain"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"

let pp_webkit_appearance : webkit_appearance Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Listbox -> Pp.string ctx "listbox"
  | Checkbox -> Pp.string ctx "checkbox"
  | Radio -> Pp.string ctx "radio"
  | Push_button -> Pp.string ctx "push-button"
  | Square_button -> Pp.string ctx "square-button"
  | Inherit -> Pp.string ctx "inherit"

let pp_pointer_events : pointer_events Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Visible_painted -> Pp.string ctx "visiblepainted"
  | Visible_fill -> Pp.string ctx "visiblefill"
  | Visible_stroke -> Pp.string ctx "visiblestroke"
  | Visible -> Pp.string ctx "visible"
  | Painted -> Pp.string ctx "painted"
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"

let pp_user_select : user_select Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Text -> Pp.string ctx "text"
  | All -> Pp.string ctx "all"
  | Contain -> Pp.string ctx "contain"

let rec pp_line_height : line_height Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.unit ctx p "%"
  | Num n -> Pp.float ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_line_height ctx v
  | Calc c -> pp_calc pp_line_height ctx c

let rec pp_font_weight : font_weight Pp.t =
 fun ctx -> function
  | Weight n -> Pp.int ctx n
  | Normal -> Pp.string ctx "normal"
  | Bold -> Pp.string ctx "bold"
  | Bolder -> Pp.string ctx "bolder"
  | Lighter -> Pp.string ctx "lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_font_weight ctx v

let pp_webkit_box_orient : webkit_box_orient Pp.t =
 fun ctx -> function
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Inherit -> Pp.string ctx "inherit"

let pp_property_value : type a. (a property * a) Pp.t =
 fun ctx (prop, value) ->
  let pp pp_a = pp_a ctx value in
  match prop with
  | Background_color -> pp pp_color
  | Color -> pp pp_color
  | Border_color -> pp pp_color
  | Border_style -> pp pp_border_style
  | Border_top_style -> pp pp_border_style
  | Border_right_style -> pp pp_border_style
  | Border_bottom_style -> pp pp_border_style
  | Border_left_style -> pp pp_border_style
  | Padding -> pp pp_length
  | Padding_left -> pp pp_length
  | Padding_right -> pp pp_length
  | Padding_bottom -> pp pp_length
  | Padding_top -> pp pp_length
  | Padding_inline -> pp pp_length
  | Padding_inline_start -> pp pp_length
  | Padding_inline_end -> pp pp_length
  | Padding_block -> pp pp_length
  | Margin -> pp pp_length
  | Margin_inline_end -> pp pp_length
  | Margin_left -> pp pp_length
  | Margin_right -> pp pp_length
  | Margin_top -> pp pp_length
  | Margin_bottom -> pp pp_length
  | Margin_inline -> pp pp_length
  | Margin_block -> pp pp_length
  | Gap -> pp pp_gap
  | Column_gap -> pp pp_length
  | Row_gap -> pp pp_length
  | Width -> pp pp_length
  | Height -> pp pp_length
  | Min_width -> pp pp_length
  | Min_height -> pp pp_length
  | Max_width -> pp pp_length
  | Max_height -> pp pp_length
  | Font_size -> pp pp_length
  | Line_height -> pp pp_line_height
  | Font_weight -> pp pp_font_weight
  | Display -> pp pp_display
  | Position -> pp pp_position
  | Visibility -> pp pp_visibility
  | Align_items -> pp pp_align_items
  | Justify_content -> pp pp_justify_content
  | Justify_items -> pp pp_justify
  | Align_self -> pp pp_align_self
  | Border_collapse -> pp pp_border_collapse
  | Table_layout -> pp pp_table_layout
  | Grid_auto_flow -> pp pp_grid_auto_flow
  | Opacity -> pp Pp.float
  | Mix_blend_mode -> pp pp_blend_mode
  | Z_index -> pp pp_z_index
  | Tab_size -> pp Pp.int
  | Webkit_line_clamp -> pp Pp.int
  | Webkit_box_orient -> pp pp_webkit_box_orient
  | Top -> pp pp_length
  | Right -> pp pp_length
  | Bottom -> pp pp_length
  | Left -> pp pp_length
  | Border_width -> pp pp_border_width
  | Border_top_width -> pp pp_border_width
  | Border_right_width -> pp pp_border_width
  | Border_bottom_width -> pp pp_border_width
  | Border_left_width -> pp pp_border_width
  | Border_inline_start_width -> pp pp_border_width
  | Border_inline_end_width -> pp pp_border_width
  | Border_radius -> pp pp_length
  | Border_top_color -> pp pp_color
  | Border_right_color -> pp pp_color
  | Border_bottom_color -> pp pp_color
  | Border_left_color -> pp pp_color
  | Border_inline_start_color -> pp pp_color
  | Border_inline_end_color -> pp pp_color
  | Text_decoration_color -> pp pp_color
  | Webkit_text_decoration_color -> pp pp_color
  | Webkit_tap_highlight_color -> pp pp_color
  | Text_indent -> pp pp_length
  | Border_spacing -> pp pp_length
  | Outline_offset -> pp pp_length
  | Perspective -> pp pp_length
  | Transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Isolation -> pp pp_isolation
  | Transform_style -> pp pp_transform_style
  | Backface_visibility -> pp pp_backface_visibility
  | Scroll_snap_align -> pp pp_scroll_snap_align
  | Scroll_snap_stop -> pp pp_scroll_snap_stop
  | Scroll_behavior -> pp pp_scroll_behavior
  | Box_sizing -> pp pp_box_sizing
  | Resize -> pp pp_resize
  | Object_fit -> pp pp_object_fit
  | Appearance -> pp pp_appearance
  | Flex_grow -> pp Pp.float
  | Flex_shrink -> pp Pp.float
  | Order -> pp Pp.int
  | Flex_direction -> pp pp_flex_direction
  | Flex_wrap -> pp pp_flex_wrap
  | Font_style -> pp pp_font_style
  | Text_align -> pp pp_text_align
  | Text_decoration -> pp pp_text_decoration
  | Text_decoration_style -> pp pp_text_decoration_style
  | Text_transform -> pp pp_text_transform
  | List_style_type -> pp pp_list_style_type
  | List_style_position -> pp pp_list_style_position
  | List_style_image -> pp pp_list_style_image
  | Overflow -> pp pp_overflow
  | Overflow_x -> pp pp_overflow
  | Overflow_y -> pp pp_overflow
  | Vertical_align -> pp pp_vertical_align
  | Text_overflow -> pp pp_text_overflow
  | Text_wrap -> pp pp_text_wrap
  | Word_break -> pp pp_word_break
  | Overflow_wrap -> pp pp_overflow_wrap
  | Hyphens -> pp pp_hyphens
  | Webkit_hyphens -> pp pp_hyphens
  | Font_stretch -> pp pp_font_stretch
  | Font_variant_numeric -> pp pp_font_variant_numeric
  | Webkit_font_smoothing -> pp pp_webkit_font_smoothing
  | Scroll_snap_type -> pp pp_scroll_snap_type
  | Container_type -> pp pp_container_type
  | White_space -> pp pp_white_space
  | Grid_template_columns -> pp pp_grid_template
  | Grid_template_rows -> pp pp_grid_template
  | Grid_template_areas -> pp Pp.string
  | Grid_template -> pp pp_grid_template
  | Grid_area -> pp Pp.string
  | Grid_auto_columns -> pp pp_grid_template
  | Grid_auto_rows -> pp pp_grid_template
  | Flex -> pp pp_flex
  | Flex_basis -> pp pp_length
  | Align_content -> pp pp_align
  | Justify_self -> pp pp_justify
  | Place_content -> pp pp_place_content
  | Place_items -> pp pp_place_items
  | Place_self -> pp pp_align_self
  | Grid_column -> pp Pp.string
  | Grid_row -> pp Pp.string
  | Grid_column_start -> pp pp_grid_line
  | Grid_column_end -> pp pp_grid_line
  | Grid_row_start -> pp pp_grid_line
  | Grid_row_end -> pp pp_grid_line
  | Text_underline_offset -> pp Pp.string
  | Background_position -> pp (Pp.list ~sep:Pp.comma pp_position_2d)
  | Background_repeat -> pp pp_background_repeat
  | Background_size -> pp pp_background_size
  | Moz_osx_font_smoothing -> pp pp_moz_osx_font_smoothing
  | Backdrop_filter -> pp pp_filter
  | Container_name -> pp Pp.string
  | Perspective_origin -> pp Pp.string
  | Object_position -> pp pp_position_2d
  | Rotate -> pp pp_angle
  | Transition_duration -> pp pp_duration
  | Transition_timing_function -> pp pp_timing_function
  | Transition_delay -> pp pp_duration
  | Will_change -> pp Pp.string
  | Contain -> pp pp_contain
  | Word_spacing -> pp pp_length
  | Background_attachment -> pp pp_background_attachment
  | Border_top -> pp Pp.string
  | Border_right -> pp Pp.string
  | Border_bottom -> pp Pp.string
  | Border_left -> pp Pp.string
  | Transform_origin -> pp pp_transform_origin
  | Text_shadow -> pp (Pp.list ~sep:Pp.comma pp_text_shadow)
  | Clip_path -> pp Pp.string
  | Mask -> pp Pp.string
  | Content_visibility -> pp pp_content_visibility
  | Filter -> pp pp_filter
  | Background_image -> pp (Pp.list ~sep:Pp.comma pp_background_image)
  | Animation -> pp (Pp.list ~sep:Pp.comma pp_animation)
  | Aspect_ratio -> pp pp_aspect_ratio
  | Content -> pp pp_content
  | Quotes -> pp Pp.string
  | Box_shadow -> pp (Pp.list ~sep:Pp.comma pp_box_shadow)
  | Fill -> pp pp_svg_paint
  | Stroke -> pp pp_svg_paint
  | Stroke_width -> pp pp_length
  | Transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Scale -> pp pp_scale
  | Outline -> pp Pp.string
  | Outline_style -> pp pp_outline_style
  | Outline_width -> pp pp_length
  | Outline_color -> pp pp_color
  | Forced_color_adjust -> pp pp_forced_color_adjust
  | Clip -> pp pp_clip
  | Clear -> pp pp_clear
  | Float -> pp pp_float_side
  | Border -> pp pp_border
  | Background -> pp (Pp.list ~sep:Pp.comma pp_background)
  | Text_decoration_thickness -> pp pp_length
  | Text_size_adjust -> pp Pp.string
  | Touch_action -> pp pp_touch_action
  | Direction -> pp pp_direction
  | Unicode_bidi -> pp pp_unicode_bidi
  | Writing_mode -> pp pp_writing_mode
  | Text_decoration_skip_ink -> pp pp_text_decoration_skip_ink
  | Animation_name -> pp Pp.string
  | Animation_duration -> pp pp_duration
  | Animation_timing_function -> pp pp_timing_function
  | Animation_delay -> pp pp_duration
  | Animation_iteration_count -> pp pp_animation_iteration_count
  | Animation_direction -> pp pp_animation_direction
  | Animation_fill_mode -> pp pp_animation_fill_mode
  | Animation_play_state -> pp pp_animation_play_state
  | Background_blend_mode -> pp (Pp.list ~sep:Pp.comma pp_blend_mode)
  | Scroll_margin -> pp pp_length
  | Scroll_margin_top -> pp pp_length
  | Scroll_margin_right -> pp pp_length
  | Scroll_margin_bottom -> pp pp_length
  | Scroll_margin_left -> pp pp_length
  | Scroll_padding -> pp pp_length
  | Scroll_padding_top -> pp pp_length
  | Scroll_padding_right -> pp pp_length
  | Scroll_padding_bottom -> pp pp_length
  | Scroll_padding_left -> pp pp_length
  | Overscroll_behavior -> pp pp_overscroll_behavior
  | Overscroll_behavior_x -> pp pp_overscroll_behavior
  | Overscroll_behavior_y -> pp pp_overscroll_behavior
  | Accent_color -> pp pp_color
  | Caret_color -> pp pp_color
  | List_style -> pp Pp.string
  | Font -> pp Pp.string
  | Webkit_appearance -> pp pp_webkit_appearance
  | Letter_spacing -> pp pp_length
  | Cursor -> pp pp_cursor
  | Pointer_events -> pp pp_pointer_events
  | User_select -> pp pp_user_select
  | Font_feature_settings -> pp pp_font_feature_settings
  | Font_variation_settings -> pp pp_font_variation_settings
  | Webkit_text_decoration -> pp pp_text_decoration
  | Webkit_text_size_adjust -> pp pp_text_size_adjust
  | Webkit_transform -> pp (Pp.list ~sep:Pp.space pp_transform)
  | Webkit_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Webkit_filter -> pp pp_filter
  | Moz_appearance -> pp pp_appearance
  | Ms_filter -> pp pp_filter
  | O_transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Font_family -> pp (Pp.list ~sep:Pp.comma pp_font_family)

let rec read_border_style t : border_style =
  let read_var t : border_style = Var (read_var read_border_style t) in
  Reader.enum_or_calls "border-style"
    [
      ("none", (None : border_style));
      ("solid", Solid);
      ("dashed", Dashed);
      ("dotted", Dotted);
      ("double", Double);
      ("groove", Groove);
      ("ridge", Ridge);
      ("inset", Inset);
      ("outset", Outset);
      ("hidden", Hidden);
    ]
    ~calls:[ ("var", read_var) ]
    t

let rec read_border_width t : border_width =
  let read_var t : border_width = Var (read_var read_border_width t) in
  let read_length_as_border_width t =
    let length = read_length t in
    match length with
    | Zero -> (Zero : border_width)
    | Px n -> Px n
    | Rem n -> Rem n
    | Em n -> Em n
    | Ch n -> Ch n
    | Vh n -> Vh n
    | Vw n -> Vw n
    | Vmin n -> Vmin n
    | Vmax n -> Vmax n
    | Pct n -> Pct n
    | _ -> err_invalid_value t "border-width" "unsupported length type"
  in

  Reader.enum_or_calls "border-width"
    [
      ("thin", (Thin : border_width));
      ("medium", Medium);
      ("thick", Thick);
      ("auto", Auto);
      ("max-content", Max_content);
      ("min-content", Min_content);
      ("fit-content", Fit_content);
      ("from-font", From_font);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_length_as_border_width t

let read_border_component t =
  Reader.one_of
    [
      (fun t -> `Width (read_border_width t));
      (fun t -> `Style (read_border_style t));
      (fun t -> `Color (read_color t));
    ]
    t

let read_border t : border =
  let apply acc = function
    | `Width w -> if acc.width = None then { acc with width = Some w } else acc
    | `Style s -> if acc.style = None then { acc with style = Some s } else acc
    | `Color c -> if acc.color = None then { acc with color = Some c } else acc
  in
  Reader.enum "border"
    [ ("inherit", (Inherit : border)); ("initial", Initial); ("none", None) ]
    ~default:(fun t : border ->
      let init : border_shorthand =
        { width = None; style = None; color = None }
      in
      let acc, _ = Reader.fold_many read_border_component ~init ~f:apply t in
      (Border acc : border))
    t

let read_visibility t : visibility =
  Reader.enum "visibility"
    [
      ("visible", (Visible : visibility));
      ("hidden", Hidden);
      ("collapse", Collapse);
    ]
    t

let read_z_index t : z_index =
  Reader.enum "z-index"
    [ ("auto", (Auto : z_index)) ]
    ~default:(fun t ->
      let n = Reader.number t in
      Index (int_of_float n))
    t

let read_flex_wrap t : flex_wrap =
  Reader.enum "flex-wrap"
    [
      ("nowrap", (Nowrap : flex_wrap));
      ("wrap", Wrap);
      ("wrap-reverse", Wrap_reverse);
    ]
    t

let rec read_align t : align =
  let read_var t : align = Var (read_var read_align t) in
  Reader.enum_or_calls "align"
    [
      ("normal", (Normal : align));
      ("start", Start);
      ("end", End);
      ("center", Center);
      ("stretch", Stretch);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("space-evenly", Space_evenly);
    ]
    ~calls:[ ("var", read_var) ]
    t

let read_align_self t : align_self =
  Reader.enum "align-self"
    [
      ("auto", (Auto : align_self));
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("center", Center);
      ("baseline", Baseline);
      ("stretch", Stretch);
    ]
    t

let read_justify t : justify =
  Reader.enum "justify"
    [
      ("auto", (Auto : justify));
      ("normal", Normal);
      ("stretch", Stretch);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("self-start", Self_start);
      ("self-end", Self_end);
      ("left", Left);
      ("right", Right);
      ("baseline", Baseline);
      ("inherit", Inherit);
    ]
    t

let read_flex_basis t : flex_basis =
  (* Read flex-basis: auto | content | <length> *)
  Reader.enum "flex-basis"
    [ ("auto", (Auto : flex_basis)); ("content", Content) ]
    ~default:(fun t ->
      (* Parse as length and convert to flex_basis *)
      match read_length t with
      | Px n -> (Px n : flex_basis)
      | Rem n -> Rem n
      | Em n -> Em n
      | Ex n -> Ex n
      | Pct n -> Pct n
      | Cm n -> Cm n
      | Mm n -> Mm n
      | Q n -> Q n
      | In n -> In n
      | Pt n -> Pt n
      | Pc n -> Pc n
      | Cap n -> Cap n
      | Ic n -> Ic n
      | Rlh n -> Rlh n
      | Vw n -> Vw n
      | Vh n -> Vh n
      | Vmin n -> Vmin n
      | Vmax n -> Vmax n
      | Zero -> Px 0.0 (* Convert Zero to Px 0 *)
      | _ -> Reader.err_invalid t "unsupported flex-basis value")
    t

(* Helper functions for flex parsing *)
let read_flex_basis_only t = Basis (read_flex_basis t)

let read_flex_grow_shrink_basis t =
  (* Parse grow [shrink] [basis] *)
  let grow = Reader.number t in

  (* Check if there's a unit immediately after the first number (no whitespace) *)
  (* If so, this is actually a flex-basis value with units, not flex-grow *)
  match Reader.peek t with
  | Some '%' ->
      (* This number has a unit, it's not a flex-grow value *)
      Reader.err t "not a flex-grow value"
  | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ->
      (* This number has a unit, it's not a flex-grow value *)
      Reader.err t "not a flex-grow value"
  | _ -> (
      (* It's a unitless number, continue parsing as flex-grow *)
      (* Optional shrink (defaults to 1) *)
      let shrink =
        Reader.option
          (fun t ->
            Reader.ws t;
            Reader.number t)
          t
      in

      (* Optional basis (defaults to 0%) *)
      let basis =
        Reader.option
          (fun t ->
            Reader.ws t;
            read_flex_basis t)
          t
      in

      match (shrink, basis) with
      | None, None -> Grow grow
      | Some s, None -> Grow_shrink (grow, s)
      | _, Some b -> Full (grow, Option.value shrink ~default:1.0, b))

let read_flex t : flex =
  Reader.enum "flex"
    [
      ("initial", (Initial : flex));
      ("auto", Auto);
      ("none", (None : flex));
      ("content", Basis Content);
    ]
    ~default:
      (Reader.one_of [ read_flex_grow_shrink_basis; read_flex_basis_only ])
    t

let read_place_content t : place_content =
  let read_pair t =
    let a, j = Reader.pair read_align read_justify t in
    (Align_justify (a, j) : place_content)
  in
  let read_single t =
    Reader.enum "place-content"
      [
        ("normal", (Normal : place_content));
        ("start", Start);
        ("end", End);
        ("center", Center);
        ("stretch", Stretch);
        ("space-between", Space_between);
        ("space-around", Space_around);
        ("space-evenly", Space_evenly);
      ]
      t
  in
  Reader.one_of [ read_pair; read_single ] t

let read_place_items t : place_items =
  (* Pair path: align-items then justify (whitespace-separated) *)
  let read_pair t =
    let a, j = Reader.pair read_align_items read_justify t in
    Align_justify (a, j)
  in
  (* Single keyword path *)
  let read_single t =
    Reader.enum "place-items"
      [
        ("normal", (Normal : place_items));
        ("start", Start);
        ("end", End);
        ("center", Center);
        ("stretch", Stretch);
      ]
      t
  in
  Reader.one_of [ read_pair; read_single ] t

let read_grid_auto_flow t : grid_auto_flow =
  let v = Reader.ident t in
  Reader.ws t;
  let second = Reader.option Reader.ident t in
  match (v, second) with
  | "row", Some "dense" -> Row_dense
  | "row", _ -> Row
  | "column", Some "dense" -> Column_dense
  | "column", _ -> Column
  | "dense", Some "row" -> Row_dense
  | "dense", Some "column" -> Column_dense
  | "dense", _ -> Dense
  | _ -> err_invalid_value t "grid-auto-flow" v

(* CSS Grid track breadth - supports <length-percentage> | <flex> | min-content
   | max-content | auto *)
let read_grid_track_breadth t : grid_track_breadth =
  Reader.one_of
    [
      (* Keywords first *)
      (fun t ->
        Reader.enum "track-breadth"
          [
            ("auto", (Auto : grid_track_breadth));
            ("min-content", (Min_content : grid_track_breadth));
            ("max-content", (Max_content : grid_track_breadth));
          ]
          t);
      (* Fr unit *)
      (fun t ->
        let n, unit = Reader.number_with_unit t in
        if unit = "fr" then Fr n
        else Reader.err_invalid t ("expected 'fr' unit, got '" ^ unit ^ "'"));
      (* Length units *)
      (fun t ->
        let n, unit = Reader.number_with_unit t in
        match unit with
        | "" ->
            if n = 0.0 then Zero
            else Reader.err_invalid t "unitless non-zero values not allowed"
        | "px" -> Px n
        | "rem" -> Rem n
        | "em" -> Em n
        | "%" -> Pct n
        | "vw" -> Vw n
        | "vh" -> Vh n
        | "vmin" -> Vmin n
        | "vmax" -> Vmax n
        | _ -> failwith ("unsupported grid track breadth unit: " ^ unit));
    ]
    t

let rec read_grid_track_size t : grid_track_size =
  let read_fit_content t =
    Reader.call "fit-content" t (fun t -> Fit_content (read_length t))
  in
  let read_minmax t =
    Reader.call "minmax" t (fun t ->
        (* Per CSS spec: min cannot be flex, max can be flex *)
        let read_min_breadth t =
          Reader.one_of
            [
              (* Keywords *)
              (fun t ->
                Reader.enum "min-breadth"
                  [
                    ("auto", (Auto : grid_track_breadth));
                    ("min-content", (Min_content : grid_track_breadth));
                    ("max-content", (Max_content : grid_track_breadth));
                  ]
                  t);
              (* Length only for min *)
              (fun t ->
                let n, unit = Reader.number_with_unit t in
                match unit with
                | "" ->
                    if n = 0.0 then Zero
                    else
                      Reader.err_invalid t
                        "unitless non-zero values not allowed"
                | "px" -> Px n
                | "rem" -> Rem n
                | "em" -> Em n
                | "%" -> Pct n
                | "vw" -> Vw n
                | "vh" -> Vh n
                | "vmin" -> Vmin n
                | "vmax" -> Vmax n
                | _ -> failwith ("unsupported min breadth unit: " ^ unit));
            ]
            t
        in
        let a, b =
          Reader.pair ~sep:Reader.comma read_min_breadth read_grid_track_breadth
            t
        in
        Min_max (a, b))
  in
  let read_repeat t =
    Reader.call "repeat" t (fun t ->
        let count, sizes =
          Reader.pair ~sep:Reader.comma Reader.int
            (Reader.list read_grid_track_size)
            t
        in
        (Repeat (count, sizes) : grid_track_size))
  in
  let read_default t = Track_size (read_grid_track_breadth t) in
  Reader.one_of [ read_fit_content; read_minmax; read_repeat; read_default ] t

let read_grid_template t : grid_template =
  Reader.enum "grid-template"
    [
      ("none", (None : grid_template));
      ("subgrid", Subgrid);
      ("masonry", Masonry);
    ]
    ~default:(fun t -> Tracks (Reader.list read_grid_track_size t))
    t

let read_grid_line t : grid_line =
  let read_span t =
    Reader.expect_string "span" t;
    Reader.ws t;
    Span (Reader.int t)
  in
  let read_number t : grid_line = Num (Reader.int t) in
  let read_name t : grid_line = Name (Reader.ident t) in
  Reader.enum "grid-line"
    [ ("auto", (Auto : grid_line)) ]
    ~default:(fun t -> Reader.one_of [ read_span; read_number; read_name ] t)
    t

let read_aspect_ratio t : aspect_ratio =
  let read_ratio t =
    let w = Reader.number t in
    Reader.expect '/' t;
    let h = Reader.number t in
    Ratio (w, h)
  in
  Reader.enum "aspect-ratio"
    [ ("auto", (Auto : aspect_ratio)); ("inherit", Inherit) ]
    ~default:read_ratio t

let read_text_overflow t : text_overflow =
  let read_string_overflow t : text_overflow = String (Reader.string t) in
  Reader.one_of
    [
      read_string_overflow;
      (fun t ->
        Reader.enum "text-overflow"
          [
            ("clip", (Clip : text_overflow));
            ("ellipsis", Ellipsis);
            ("inherit", Inherit);
          ]
          t);
    ]
    t

let read_text_wrap t : text_wrap =
  Reader.enum "text-wrap"
    [
      ("wrap", (Wrap : text_wrap));
      ("nowrap", No_wrap);
      ("balance", Balance);
      ("pretty", Pretty);
      ("inherit", Inherit);
    ]
    t

let read_white_space t : white_space =
  Reader.enum "white-space"
    [
      ("normal", (Normal : white_space));
      ("nowrap", Nowrap);
      ("pre", Pre);
      ("pre-wrap", Pre_wrap);
      ("pre-line", Pre_line);
      ("break-spaces", Break_spaces);
      ("inherit", Inherit);
    ]
    t

let read_word_break t : word_break =
  Reader.enum "word-break"
    [
      ("normal", (Normal : word_break));
      ("break-all", Break_all);
      ("keep-all", Keep_all);
      ("break-word", Break_word);
      ("inherit", Inherit);
    ]
    t

let read_overflow_wrap t : overflow_wrap =
  Reader.enum "overflow-wrap"
    [
      ("normal", (Normal : overflow_wrap));
      ("break-word", Break_word);
      ("anywhere", Anywhere);
      ("inherit", Inherit);
    ]
    t

let read_hyphens t : hyphens =
  Reader.enum "hyphens"
    [
      ("none", (None : hyphens));
      ("manual", Manual);
      ("auto", Auto);
      ("inherit", Inherit);
    ]
    t

let rec read_line_height t : line_height =
  let read_var t : line_height = Var (read_var read_line_height t) in
  let read_calc t : line_height = Calc (read_calc read_line_height t) in
  Reader.enum_or_calls "line-height"
    [ ("normal", Normal); ("inherit", Inherit) ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:read_line_height_length t

let read_list_style_type t : list_style_type =
  Reader.enum "list-style-type"
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
    ]
    t

let read_list_style_position t : list_style_position =
  Reader.enum "list-style-position"
    [
      ("inside", (Inside : list_style_position));
      ("outside", Outside);
      ("inherit", Inherit);
    ]
    t

let read_list_style_image t : list_style_image =
  let read_url t =
    Reader.call "url" t (fun t -> (Url (read_url_arg t) : list_style_image))
  in
  Reader.enum_or_calls "list-style-image"
    [ ("none", (None : list_style_image)); ("inherit", Inherit) ]
    ~calls:[ ("url", read_url) ]
    t

let read_table_layout t : table_layout =
  Reader.enum "table-layout"
    [ ("auto", (Auto : table_layout)); ("fixed", Fixed); ("inherit", Inherit) ]
    t

let read_border_collapse t : border_collapse =
  Reader.enum "border-collapse"
    [
      ("collapse", (Collapse : border_collapse));
      ("separate", Separate);
      ("inherit", Inherit);
    ]
    t

let read_user_select t : user_select =
  Reader.enum "user-select"
    [
      ("none", (None : user_select));
      ("auto", Auto);
      ("text", Text);
      ("all", All);
      ("contain", Contain);
    ]
    t

let read_pointer_events t : pointer_events =
  Reader.enum "pointer-events"
    [
      ("auto", (Auto : pointer_events));
      ("none", None);
      ("visiblepainted", Visible_painted);
      ("visiblefill", Visible_fill);
      ("visiblestroke", Visible_stroke);
      ("visible", Visible);
      ("painted", Painted);
      ("fill", Fill);
      ("stroke", Stroke);
      ("all", All);
      ("inherit", Inherit);
    ]
    t

let read_touch_action t : touch_action =
  Reader.enum "touch-action"
    [
      ("auto", (Auto : touch_action));
      ("none", None);
      ("pan-x", Pan_x);
      ("pan-y", Pan_y);
      ("manipulation", Manipulation);
      ("inherit", Inherit);
    ]
    t

let read_resize t : resize =
  Reader.enum "resize"
    [
      ("none", (None : resize));
      ("both", Both);
      ("horizontal", Horizontal);
      ("vertical", Vertical);
      ("block", Block);
      ("inline", Inline);
      ("inherit", Inherit);
    ]
    t

let read_box_sizing t : box_sizing =
  Reader.enum "box-sizing"
    [
      ("border-box", (Border_box : box_sizing));
      ("content-box", Content_box);
      ("inherit", Inherit);
    ]
    t

let read_object_fit t : object_fit =
  Reader.enum "object-fit"
    [
      ("fill", (Fill : object_fit));
      ("contain", Contain);
      ("cover", Cover);
      ("none", None);
      ("scale-down", Scale_down);
      ("inherit", Inherit);
    ]
    t

let rec read_content t : content =
  let read_var t : content = Var (read_var read_content t) in
  let read_string t = String (Reader.string t) in
  Reader.enum_or_calls "content"
    [
      ("none", (None : content));
      ("normal", Normal);
      ("open-quote", Open_quote);
      ("close-quote", Close_quote);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_string t

let rec read_content_visibility t : content_visibility =
  let read_var t : content_visibility =
    Var (read_var read_content_visibility t)
  in
  Reader.enum_or_calls "content-visibility"
    [
      ("visible", (Visible : content_visibility));
      ("auto", Auto);
      ("hidden", Hidden);
      ("inherit", Inherit);
    ]
    ~calls:[ ("var", read_var) ]
    t

let read_container_type t : container_type =
  Reader.enum "container-type"
    [
      ("normal", (Normal : container_type));
      ("inline-size", Inline_size);
      ("size", Size);
    ]
    t

let read_contain t : contain =
  Reader.enum "contain"
    [
      ("none", (None : contain));
      ("strict", Strict);
      ("content", Content);
      ("size", Size);
      ("layout", Layout);
      ("style", Style);
      ("paint", Paint);
    ]
    t

let read_isolation t : isolation =
  Reader.enum "isolation"
    [ ("auto", (Auto : isolation)); ("isolate", Isolate); ("inherit", Inherit) ]
    t

let read_scroll_behavior t : scroll_behavior =
  Reader.enum "scroll-behavior"
    [
      ("auto", (Auto : scroll_behavior));
      ("smooth", Smooth);
      ("inherit", Inherit);
    ]
    t

let read_scroll_snap_align t : scroll_snap_align =
  Reader.enum "scroll-snap-align"
    [
      ("none", (None : scroll_snap_align));
      ("start", Start);
      ("end", End);
      ("center", Center);
    ]
    t

let read_scroll_snap_stop t : scroll_snap_stop =
  Reader.enum "scroll-snap-stop"
    [
      ("normal", (Normal : scroll_snap_stop));
      ("always", Always);
      ("inherit", Inherit);
    ]
    t

let read_scroll_snap_axis t : scroll_snap_axis =
  Reader.enum "scroll-snap-axis"
    [
      ("x", (X : scroll_snap_axis));
      ("y", Y);
      ("block", Block);
      ("inline", Inline);
      ("both", Both);
    ]
    t

let rec read_scroll_snap_strictness t : scroll_snap_strictness =
  let read_var t = Var (read_var read_scroll_snap_strictness t) in
  Reader.enum_or_calls "scroll-snap-strictness"
    [
      ("proximity", (Proximity : scroll_snap_strictness));
      ("mandatory", Mandatory);
    ]
    ~calls:[ ("var", read_var) ]
    t

let read_scroll_snap_type t : scroll_snap_type =
  let v = Reader.ident t in
  match v with
  | "none" -> None
  | "inherit" -> Inherit
  | _ ->
      let axis =
        match v with
        | "x" -> X
        | "y" -> Y
        | "block" -> Block
        | "inline" -> Inline
        | "both" -> Both
        | _ ->
            raise
              (Invalid_argument
                 (Printf.sprintf "Invalid scroll-snap-axis: %s" v))
      in
      Reader.ws t;
      let strictness =
        Reader.option
          (fun t ->
            Reader.enum "scroll-snap-strictness"
              [
                ("proximity", (Proximity : scroll_snap_strictness));
                ("mandatory", Mandatory);
              ]
              t)
          t
      in
      Axis (axis, strictness)

let read_overscroll_behavior t : overscroll_behavior =
  Reader.enum "overscroll-behavior"
    [
      ("auto", (Auto : overscroll_behavior));
      ("contain", Contain);
      ("none", None);
      ("inherit", Inherit);
    ]
    t

let read_svg_paint t : svg_paint =
  Reader.enum "svg-paint"
    [ ("none", (None : svg_paint)); ("currentcolor", Current_color) ]
    ~default:(fun t -> (Color (read_color t) : svg_paint))
    t

let read_direction t : direction =
  Reader.enum "direction"
    [ ("ltr", (Ltr : direction)); ("rtl", Rtl); ("inherit", Inherit) ]
    t

let read_unicode_bidi t : unicode_bidi =
  Reader.enum "unicode-bidi"
    [
      ("normal", (Normal : unicode_bidi));
      ("embed", Embed);
      ("isolate", Isolate);
      ("bidi-override", Bidi_override);
      ("isolate-override", Isolate_override);
      ("plaintext", Plaintext);
      ("inherit", Inherit);
    ]
    t

let read_writing_mode t : writing_mode =
  Reader.enum "writing-mode"
    [
      ("horizontal-tb", (Horizontal_tb : writing_mode));
      ("vertical-rl", Vertical_rl);
      ("vertical-lr", Vertical_lr);
      ("inherit", Inherit);
    ]
    t

let read_webkit_appearance t : webkit_appearance =
  Reader.enum "webkit-appearance"
    [
      ("none", (None : webkit_appearance));
      ("auto", Auto);
      ("button", Button);
      ("textfield", Textfield);
      ("menulist", Menulist);
      ("listbox", Listbox);
      ("checkbox", Checkbox);
      ("radio", Radio);
      ("push-button", Push_button);
      ("square-button", Square_button);
      ("inherit", Inherit);
    ]
    t

let read_text_size_adjust t : text_size_adjust =
  Reader.ws t;
  match Reader.peek t with
  | Some c when Reader.is_digit c || c = '-' || c = '.' ->
      (* Percentage value *)
      let n = Reader.number t in
      Reader.expect '%' t;
      Pct n
  | _ ->
      (* Keyword *)
      Reader.enum "text-size-adjust"
        [
          ("none", (None : text_size_adjust));
          ("auto", Auto);
          ("inherit", Inherit);
        ]
        t

let read_webkit_font_smoothing t : webkit_font_smoothing =
  Reader.enum "webkit-font-smoothing"
    [
      ("auto", (Auto : webkit_font_smoothing));
      ("none", None);
      ("antialiased", Antialiased);
      ("subpixel-antialiased", Subpixel_antialiased);
      ("inherit", Inherit);
    ]
    t

let read_moz_osx_font_smoothing t : moz_osx_font_smoothing =
  Reader.enum "moz-osx-font-smoothing"
    [
      ("auto", (Auto : moz_osx_font_smoothing));
      ("grayscale", Grayscale);
      ("inherit", Inherit);
    ]
    t

let read_webkit_box_orient t : webkit_box_orient =
  Reader.enum "webkit-box-orient"
    [
      ("horizontal", (Horizontal : webkit_box_orient));
      ("vertical", Vertical);
      ("inherit", Inherit);
    ]
    t

let read_forced_color_adjust t : forced_color_adjust =
  Reader.enum "forced-color-adjust"
    [
      ("auto", (Auto : forced_color_adjust));
      ("none", None);
      ("inherit", Inherit);
    ]
    t

let read_appearance t : appearance =
  Reader.enum "appearance"
    [
      ("none", (None : appearance));
      ("auto", Auto);
      ("button", Button);
      ("textfield", Textfield);
      ("menulist", Menulist);
      ("inherit", Inherit);
    ]
    t

let read_clear t : clear =
  Reader.enum "clear"
    [
      ("none", (None : clear)); ("left", Left); ("right", Right); ("both", Both);
    ]
    t

let read_float_side t : float_side =
  Reader.enum "float-side"
    [
      ("none", (None : float_side));
      ("left", Left);
      ("right", Right);
      ("inline-start", Inline_start);
      ("inline-end", Inline_end);
      ("inherit", Inherit);
    ]
    t

let read_text_decoration_skip_ink t : text_decoration_skip_ink =
  Reader.enum "text-decoration-skip-ink"
    [
      ("auto", (Auto : text_decoration_skip_ink));
      ("none", None);
      ("all", All);
      ("inherit", Inherit);
    ]
    t

(* TODO: Fix vertical_align function let read_vertical_align t : vertical_align
   = *)
let read_vertical_align t : vertical_align =
  Reader.enum "vertical-align"
    [
      ("baseline", (Baseline : vertical_align));
      ("top", Top);
      ("middle", Middle);
      ("bottom", Bottom);
      ("text-top", Text_top);
      ("text-bottom", Text_bottom);
      ("sub", Sub);
      ("super", Super);
      ("inherit", Inherit);
    ]
    ~default:read_vertical_align_length t

let read_outline_style t : outline_style =
  Reader.enum "outline-style"
    [
      ("none", (None : outline_style));
      ("solid", Solid);
      ("dashed", Dashed);
      ("dotted", Dotted);
      ("double", Double);
      ("groove", Groove);
      ("ridge", Ridge);
      ("inset", Inset);
      ("outset", Outset);
      ("auto", Auto);
    ]
    t

let rec read_font_family t : font_family =
  let read_var t : font_family =
    let v = read_var (Reader.list ~sep:Reader.comma read_font_family) t in
    Var v
  in
  Reader.enum_or_calls "font-family"
    [
      (* Generic CSS font families *)
      ("sans-serif", Sans_serif);
      ("serif", Serif);
      ("monospace", Monospace);
      ("cursive", Cursive);
      ("fantasy", Fantasy);
      ("system-ui", System_ui);
      ("ui-sans-serif", Ui_sans_serif);
      ("ui-serif", Ui_serif);
      ("ui-monospace", Ui_monospace);
      ("ui-rounded", Ui_rounded);
      ("emoji", Emoji);
      ("math", Math);
      ("fangsong", Fangsong);
      (* CSS keywords *)
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      (* Popular web fonts *)
      ("inter", Inter);
      ("roboto", Roboto);
      ("open-sans", Open_sans);
      ("lato", Lato);
      ("montserrat", Montserrat);
      ("poppins", Poppins);
      ("source-sans-pro", Source_sans_pro);
      ("raleway", Raleway);
      ("oswald", Oswald);
      ("noto-sans", Noto_sans);
      ("ubuntu", Ubuntu);
      ("playfair-display", Playfair_display);
      ("merriweather", Merriweather);
      ("lora", Lora);
      ("pt-sans", PT_sans);
      ("pt-serif", PT_serif);
      ("nunito", Nunito);
      ("nunito-sans", Nunito_sans);
      ("work-sans", Work_sans);
      ("rubik", Rubik);
      ("fira-sans", Fira_sans);
      ("fira-code", Fira_code);
      ("jetbrains-mono", JetBrains_mono);
      ("ibm-plex-sans", IBM_plex_sans);
      ("ibm-plex-serif", IBM_plex_serif);
      ("ibm-plex-mono", IBM_plex_mono);
      ("source-code-pro", Source_code_pro);
      ("space-mono", Space_mono);
      ("dm-sans", DM_sans);
      ("dm-serif-display", DM_serif_display);
      ("bebas-neue", Bebas_neue);
      ("barlow", Barlow);
      ("mulish", Mulish);
      ("josefin-sans", Josefin_sans);
      (* Platform-specific fonts *)
      ("helvetica", Helvetica);
      ("helvetica-neue", Helvetica_neue);
      ("arial", Arial);
      ("verdana", Verdana);
      ("tahoma", Tahoma);
      ("trebuchet-ms", Trebuchet_ms);
      ("times-new-roman", Times_new_roman);
      ("times", Times);
      ("georgia", Georgia);
      ("cambria", Cambria);
      ("garamond", Garamond);
      ("courier-new", Courier_new);
      ("courier", Courier);
      ("lucida-console", Lucida_console);
      ("sf-pro", SF_pro);
      ("sf-pro-display", SF_pro_display);
      ("sf-pro-text", SF_pro_text);
      ("sf-mono", SF_mono);
      ("ny", NY);
      ("segoe-ui", Segoe_ui);
      ("segoe-ui-emoji", Segoe_ui_emoji);
      ("segoe-ui-symbol", Segoe_ui_symbol);
      ("apple-color-emoji", Apple_color_emoji);
      ("noto-color-emoji", Noto_color_emoji);
      ("android-emoji", Android_emoji);
      ("twemoji-mozilla", Twemoji_mozilla);
      (* Developer fonts *)
      ("menlo", Menlo);
      ("monaco", Monaco);
      ("consolas", Consolas);
      ("liberation-mono", Liberation_mono);
      ("sfmono-regular", SFMono_regular);
      ("cascadia-code", Cascadia_code);
      ("cascadia-mono", Cascadia_mono);
      ("victor-mono", Victor_mono);
      ("inconsolata", Inconsolata);
      ("hack", Hack);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      (* Handle quoted strings and arbitrary identifiers *)
      Reader.ws t;
      match Reader.peek t with
      | Some ('"' | '\'') ->
          let name = Reader.string t in
          Name name
      | _ ->
          let name = Reader.ident t in
          Name name)
    t

let read_font_stretch t : font_stretch =
  let read_percentage t : font_stretch =
    let n = Reader.number t in
    Reader.expect '%' t;
    Pct (n /. 100.)
  in
  Reader.enum "font-stretch"
    [
      ("ultra-condensed", Ultra_condensed);
      ("extra-condensed", Extra_condensed);
      ("condensed", Condensed);
      ("semi-condensed", Semi_condensed);
      ("normal", Normal);
      ("semi-expanded", Semi_expanded);
      ("expanded", Expanded);
      ("extra-expanded", Extra_expanded);
      ("ultra-expanded", Ultra_expanded);
      ("inherit", Inherit);
    ]
    ~default:read_percentage t

let rec read_font_variant_numeric_token t : font_variant_numeric_token =
  let read_var t : font_variant_numeric_token =
    Var (read_var read_font_variant_numeric_token t)
  in
  Reader.enum_or_calls "font-variant-numeric-token"
    [
      ("normal", (Normal : font_variant_numeric_token));
      ("lining-nums", Lining_nums);
      ("oldstyle-nums", Oldstyle_nums);
      ("proportional-nums", Proportional_nums);
      ("tabular-nums", Tabular_nums);
      ("diagonal-fractions", Diagonal_fractions);
      ("stacked-fractions", Stacked_fractions);
      ("ordinal", Ordinal);
      ("slashed-zero", Slashed_zero);
    ]
    ~calls:[ ("var", read_var) ]
    t

let rec read_font_variant_numeric t : font_variant_numeric =
  let read_var t : font_variant_numeric =
    Var (read_var read_font_variant_numeric t)
  in
  Reader.enum_or_calls "font-variant-numeric"
    [ ("normal", (Normal : font_variant_numeric)) ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let tokens, _ = Reader.many read_font_variant_numeric_token t in
      if tokens = [] then err_invalid_value t "font-variant-numeric" "<empty>"
      else Tokens tokens)
    t

let rec read_font_feature_settings t : font_feature_settings =
  let read_var t : font_feature_settings =
    Var (read_var read_font_feature_settings t)
  in
  let read_string t : font_feature_settings = String (Reader.string t) in
  let read_feature_list t = Feature_list (Reader.ident t) in
  Reader.enum_or_calls "font-feature-settings"
    [ ("normal", (Normal : font_feature_settings)); ("inherit", Inherit) ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t -> Reader.one_of [ read_string; read_feature_list ] t)
    t

let rec read_font_variation_settings t : font_variation_settings =
  let read_var t : font_variation_settings =
    Var (read_var read_font_variation_settings t)
  in
  let read_string t : font_variation_settings = String (Reader.string t) in
  let read_axis_list t = Axis_list (Reader.ident t) in
  Reader.enum_or_calls "font-variation-settings"
    [ ("normal", (Normal : font_variation_settings)); ("inherit", Inherit) ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t -> Reader.one_of [ read_string; read_axis_list ] t)
    t

let read_transform_style t : transform_style =
  Reader.enum "transform-style"
    [
      ("flat", (Flat : transform_style));
      ("preserve-3d", Preserve_3d);
      ("inherit", Inherit);
    ]
    t

let read_backface_visibility t : backface_visibility =
  Reader.enum "backface-visibility"
    [
      ("visible", (Visible : backface_visibility));
      ("hidden", Hidden);
      ("inherit", Inherit);
    ]
    t

let rec read_scale t : scale =
  let read_var t : scale = Var (read_var read_scale t) in
  let read_number t : scale = X (Reader.number t) in
  Reader.enum_or_calls "scale"
    [ ("none", (None : scale)) ]
    ~calls:[ ("var", read_var) ]
    ~default:read_number t

let read_timing_function t : timing_function =
  let read_steps t : timing_function =
    Reader.call "steps" t (fun t ->
        let n = int_of_float (Reader.number t) in
        let kind =
          Reader.option
            (fun t ->
              Reader.comma t;
              match Reader.ident t with
              | "jump-start" -> `Jump_start
              | "jump-end" -> `Jump_end
              | "jump-none" -> `Jump_none
              | "jump-both" -> `Jump_both
              | "start" -> `Start
              | "end" -> `End
              | s -> err_invalid_value t "steps kind" s)
            t
        in
        Steps (n, kind))
  in
  let read_cubic_bezier t : timing_function =
    Reader.call "cubic-bezier" t (fun t ->
        let a = Reader.number t in
        Reader.comma t;
        Reader.ws t;
        let b = Reader.number t in
        Reader.comma t;
        Reader.ws t;
        let c = Reader.number t in
        Reader.comma t;
        Reader.ws t;
        let d = Reader.number t in
        Cubic_bezier (a, b, c, d))
  in
  Reader.enum_or_calls "timing-function"
    [
      ("ease", (Ease : timing_function));
      ("linear", Linear);
      ("ease-in", Ease_in);
      ("ease-out", Ease_out);
      ("ease-in-out", Ease_in_out);
      ("step-start", Step_start);
      ("step-end", Step_end);
    ]
    ~calls:[ ("steps", read_steps); ("cubic-bezier", read_cubic_bezier) ]
    t

let read_transition_property t : transition_property =
  Reader.enum "transition-property"
    [
      ("all", (All : transition_property));
      ("none", (None : transition_property));
    ]
    ~default:(fun t -> Property (Reader.ident t))
    t

let read_transition t : transition =
  (* Parse transition shorthand: property duration timing-function delay *)
  let property = read_transition_property t in

  (* Optional duration *)
  let duration =
    Reader.option
      (fun t ->
        Reader.ws t;
        read_duration t)
      t
  in

  (* Optional timing function (only if we have duration) *)
  let timing_function =
    match duration with
    | Some _ ->
        Reader.option
          (fun t ->
            Reader.ws t;
            read_timing_function t)
          t
    | None -> None
  in

  (* Optional delay (only if we have duration) *)
  let delay =
    match (duration, timing_function) with
    | Some _, _ ->
        Reader.option
          (fun t ->
            Reader.ws t;
            read_duration t)
          t
    | _ -> None
  in

  { property; duration; timing_function; delay }

let read_transitions t : transition list =
  Reader.list ~at_least:1 ~sep:Reader.comma read_transition t

let read_animation_direction t : animation_direction =
  Reader.enum "animation-direction"
    [
      ("normal", (Normal : animation_direction));
      ("reverse", Reverse);
      ("alternate", Alternate);
      ("alternate-reverse", Alternate_reverse);
    ]
    t

let read_animation_fill_mode t : animation_fill_mode =
  Reader.enum "animation-fill-mode"
    [
      ("none", (None : animation_fill_mode));
      ("forwards", Forwards);
      ("backwards", Backwards);
      ("both", Both);
    ]
    t

let read_animation_iteration_count t : animation_iteration_count =
  Reader.enum "animation-iteration-count"
    [ ("infinite", Infinite) ]
    ~default:(fun t -> Num (Reader.number t))
    t

let read_animation_play_state t : animation_play_state =
  Reader.enum "animation-play-state"
    [ ("running", (Running : animation_play_state)); ("paused", Paused) ]
    t

let read_animation_component t =
  let read_duration_component t = `Duration (read_duration t) in
  let read_timing_component t = `Timing_function (read_timing_function t) in
  let read_iteration_component t =
    `Iteration_count (read_animation_iteration_count t)
  in
  let read_direction_component t = `Direction (read_animation_direction t) in
  let read_fill_component t = `Fill_mode (read_animation_fill_mode t) in
  let read_play_component t = `Play_state (read_animation_play_state t) in
  let read_name_component t =
    let v = Reader.ident t in
    if v = "none" then `Name (None : string option)
    else `Name (Some v : string option)
  in
  Reader.one_of
    [
      (* Duration - parse durations before other components *)
      read_duration_component;
      (* Timing function *)
      read_timing_component;
      (* Iteration count *)
      read_iteration_component;
      (* Direction *)
      read_direction_component;
      (* Fill mode *)
      read_fill_component;
      (* Play state *)
      read_play_component;
      (* Animation name (including "none") - parse this LAST since it accepts
         any identifier *)
      read_name_component;
    ]
    t

let read_animation t : animation =
  let apply acc = function
    | `Name name ->
        (* Only set name if we don't already have one *)
        if acc.name = None then { acc with name } else acc
    | `Duration d ->
        (* First duration is duration, second is delay *)
        if acc.duration = None then { acc with duration = Some d }
        else if acc.delay = None then { acc with delay = Some d }
        else acc
    | `Timing_function tf -> { acc with timing_function = Some tf }
    | `Iteration_count ic -> { acc with iteration_count = Some ic }
    | `Direction dir -> { acc with direction = Some dir }
    | `Fill_mode fm -> { acc with fill_mode = Some fm }
    | `Play_state ps -> { acc with play_state = Some ps }
  in

  let init =
    {
      name = None;
      duration = None;
      timing_function = None;
      delay = None;
      iteration_count = None;
      direction = None;
      fill_mode = None;
      play_state = None;
    }
  in

  let acc, _ = Reader.fold_many read_animation_component ~init ~f:apply t in
  (* CSS animation shorthand requires at least one component *)
  if acc = init then
    Reader.err t "animation shorthand requires at least one component"
  else acc

let read_animations t : animation list =
  Reader.list ~at_least:1 ~sep:Reader.comma read_animation t

let rec read_blend_mode t : blend_mode =
  let read_var t : blend_mode = Var (read_var read_blend_mode t) in
  Reader.enum_or_calls "blend-mode"
    [
      ("normal", (Normal : blend_mode));
      ("multiply", Multiply);
      ("screen", Screen);
      ("overlay", Overlay);
      ("darken", Darken);
      ("lighten", Lighten);
      ("color-dodge", Color_dodge);
      ("color-burn", Color_burn);
      ("hard-light", Hard_light);
      ("soft-light", Soft_light);
      ("difference", Difference);
      ("exclusion", Exclusion);
      ("hue", Hue);
      ("saturation", Saturation);
      ("color", Color);
      ("luminosity", Luminosity);
    ]
    ~calls:[ ("var", read_var) ]
    t

let read_blend_modes t : blend_mode list =
  Reader.list ~sep:Reader.comma read_blend_mode t

let read_text_shadow t : text_shadow =
  let read_component t =
    Reader.one_of
      [ (fun t -> `Color (read_color t)); (fun t -> `Length (read_length t)) ]
      t
  in
  Reader.enum "text-shadow"
    [ ("none", None); ("inherit", Inherit) ]
    t
    ~default:(fun t ->
      let parts, _ = Reader.many read_component t in
      let lengths =
        List.filter_map (function `Length l -> Some l | _ -> None) parts
      in
      let color =
        List.find_map (function `Color c -> Some c | _ -> None) parts
      in
      match lengths with
      | h :: v :: rest ->
          let blur = match rest with b :: _ -> Some b | _ -> None in
          (Text_shadow { h_offset = h; v_offset = v; blur; color }
            : text_shadow)
      | _ -> err_invalid_value t "text-shadow" "expected at least two lengths")

let read_text_shadows t : text_shadow list =
  Reader.list ~sep:Reader.comma ~at_least:1 read_text_shadow t

let read_blur t : filter = Reader.call "blur" t (fun t -> Blur (read_length t))

let read_brightness t : filter =
  Reader.call "brightness" t (fun t -> Brightness (read_number t))

let read_contrast t : filter =
  Reader.call "contrast" t (fun t -> Contrast (read_number t))

let read_grayscale t : filter =
  Reader.call "grayscale" t (fun t : filter -> Grayscale (read_number t))

let read_hue_rotate t : filter =
  Reader.call "hue-rotate" t (fun t -> Hue_rotate (read_angle t))

let read_invert t : filter =
  Reader.call "invert" t (fun t -> Invert (read_number t))

let read_opacity t : filter =
  Reader.call "opacity" t (fun t : filter -> Opacity (read_number t))

let read_saturate t : filter =
  Reader.call "saturate" t (fun t -> Saturate (read_number t))

let read_sepia t : filter =
  Reader.call "sepia" t (fun t -> Sepia (read_number t))

let read_drop_shadow t : filter =
  Reader.call "drop-shadow" t (fun t ->
      (* Allow var() to produce a shadow via fallback parser *)
      if Reader.looking_at t "var" then
        let read_shadow t =
          let components, _ = Reader.many read_box_shadow_component t in
          let parts = fold_box_shadow_components components in
          let lengths = List.rev parts.lengths in
          match read_box_shadow_lengths lengths with
          | Some (h, v, blur, spread) ->
              if parts.inset then Inset (h, v, blur, spread, parts.color)
              else Simple (h, v, blur, spread, parts.color)
          | None ->
              err_invalid_value t "drop-shadow"
                "at least two lengths are required"
        in
        let v = read_var read_shadow t in
        Drop_shadow (Var v)
      else
        let components, _ = Reader.many read_box_shadow_component t in
        let parts = fold_box_shadow_components components in
        let lengths = List.rev parts.lengths in
        match read_box_shadow_lengths lengths with
        | Some (h, v, blur, spread) ->
            let s =
              if parts.inset then Inset (h, v, blur, spread, parts.color)
              else Simple (h, v, blur, spread, parts.color)
            in
            Drop_shadow s
        | None ->
            err_invalid_value t "drop-shadow"
              "at least two lengths are required")

let rec read_filter_item t : filter =
  let read_var t : filter =
    let v = read_var read_filter t in
    Var v
  in
  let read_url t = (Url (read_url_arg t) : filter) in
  Reader.enum_or_calls "filter"
    [ ("none", (None : filter)) ]
    ~calls:
      [
        ("blur", read_blur);
        ("brightness", read_brightness);
        ("contrast", read_contrast);
        ("grayscale", read_grayscale);
        ("hue-rotate", read_hue_rotate);
        ("invert", read_invert);
        ("opacity", read_opacity);
        ("saturate", read_saturate);
        ("sepia", read_sepia);
        ("drop-shadow", read_drop_shadow);
        ("url", read_url);
        ("var", read_var);
      ]
    t

and read_filter t : filter =
  let read_filter_list t =
    let filters, _ = Reader.many read_filter_item t in
    match filters with
    | [] -> err_invalid_value t "filter" "expected filter function(s)"
    | [ f ] -> f
    | fs -> List fs
  in
  Reader.enum "filter" [ ("none", (None : filter)) ] ~default:read_filter_list t

(* Background-related readers *)
let read_background_attachment t : background_attachment =
  Reader.enum "background-attachment"
    [
      ("scroll", (Scroll : background_attachment));
      ("fixed", Fixed);
      ("local", Local);
      ("inherit", Inherit);
    ]
    t

let read_background_repeat t : background_repeat =
  Reader.enum "background-repeat"
    [
      ("repeat", (Repeat : background_repeat));
      ("repeat-x", Repeat_x);
      ("repeat-y", Repeat_y);
      ("no-repeat", No_repeat);
      ("space", Space);
      ("round", Round);
      ("inherit", Inherit);
    ]
    t

let read_background_size t : background_size =
  let read_pair t : background_size =
    let a, b = Reader.pair read_length read_length t in
    Size (a, b)
  in
  let read_pct t : background_size =
    let n = Reader.number t in
    Reader.expect '%' t;
    Pct (n /. 100.)
  in
  let read_length_value t : background_size = read_background_size_length t in
  Reader.enum "background-size"
    [
      ("auto", (Auto : background_size));
      ("cover", Cover);
      ("contain", Contain);
      ("inherit", Inherit);
    ]
    ~default:(fun t ->
      Reader.one_of [ read_pair; read_length_value; read_pct ] t)
    t

let read_gradient_direction t : gradient_direction =
  let read_to_direction t =
    Reader.expect_string "to" t;
    Reader.ws t;
    let read_dir_token t =
      Reader.enum "direction"
        [
          ("top", `Top); ("bottom", `Bottom); ("left", `Left); ("right", `Right);
        ]
        t
    in
    let directions, _ = Reader.many read_dir_token t in
    match directions with
    | [ `Top ] -> To_top
    | [ `Bottom ] -> To_bottom
    | [ `Left ] -> To_left
    | [ `Right ] -> To_right
    | [ `Top; `Left ] | [ `Left; `Top ] -> To_top_left
    | [ `Top; `Right ] | [ `Right; `Top ] -> To_top_right
    | [ `Bottom; `Left ] | [ `Left; `Bottom ] -> To_bottom_left
    | [ `Bottom; `Right ] | [ `Right; `Bottom ] -> To_bottom_right
    | _ ->
        err_invalid_value t "gradient-direction" "invalid direction combination"
  in
  Reader.one_of [ read_to_direction; (fun t -> Angle (read_angle t)) ] t

let read_gradient_stop t : gradient_stop =
  (* Simple implementation - just read color for now *)
  let color = read_color t in
  Color_stop color

let read_background_image t : background_image =
  let read_linear_body t =
    Reader.ws t;
    let direction =
      match Reader.option read_gradient_direction t with
      | Some d ->
          ignore (Reader.comma_opt t);
          d
      | None -> To_bottom
    in
    let stops =
      Reader.list ~at_least:2 ~sep:Reader.comma read_gradient_stop t
    in
    Linear_gradient (direction, stops)
  in
  let read_radial_body t =
    Reader.ws t;
    let stops =
      Reader.list ~at_least:2 ~sep:Reader.comma read_gradient_stop t
    in
    Radial_gradient stops
  in
  Reader.enum_or_calls "background-image"
    [ ("none", (None : background_image)) ]
    ~calls:
      [
        ("url", fun t -> Url (Reader.url t));
        ( "linear-gradient",
          fun t -> Reader.call "linear-gradient" t read_linear_body );
        ( "radial-gradient",
          fun t -> Reader.call "radial-gradient" t read_radial_body );
      ]
    t

let read_background_images t : background_image list =
  Reader.list ~sep:Reader.comma read_background_image t

let read_property t =
  let prop_name = Reader.ident t in
  (* PROPERTY_MATCHING_START - Used by scripts/check_properties.ml *)
  match prop_name with
  | "width" -> Prop Width
  | "height" -> Prop Height
  | "min-width" -> Prop Min_width
  | "min-height" -> Prop Min_height
  | "max-width" -> Prop Max_width
  | "max-height" -> Prop Max_height
  | "color" -> Prop Color
  | "background-color" -> Prop Background_color
  | "background" -> Prop Background (* Shorthand property *)
  | "background-image" -> Prop Background_image
  | "border-color" -> Prop Border_color
  | "border-top-color" -> Prop Border_top_color
  | "border-right-color" -> Prop Border_right_color
  | "border-bottom-color" -> Prop Border_bottom_color
  | "border-left-color" -> Prop Border_left_color
  | "border-style" -> Prop Border_style
  | "border-top-style" -> Prop Border_top_style
  | "border-right-style" -> Prop Border_right_style
  | "border-bottom-style" -> Prop Border_bottom_style
  | "border-left-style" -> Prop Border_left_style
  | "border-width" -> Prop Border_width
  | "border-top-width" -> Prop Border_top_width
  | "border-right-width" -> Prop Border_right_width
  | "border-bottom-width" -> Prop Border_bottom_width
  | "border-left-width" -> Prop Border_left_width
  | "border-radius" -> Prop Border_radius
  | "outline-color" -> Prop Outline_color
  | "text-decoration-color" -> Prop Text_decoration_color
  | "display" -> Prop Display
  | "position" -> Prop Position
  | "visibility" -> Prop Visibility
  | "overflow" -> Prop Overflow
  | "overflow-x" -> Prop Overflow_x
  | "overflow-y" -> Prop Overflow_y
  | "margin" -> Prop Margin
  | "margin-left" -> Prop Margin_left
  | "margin-right" -> Prop Margin_right
  | "margin-top" -> Prop Margin_top
  | "margin-bottom" -> Prop Margin_bottom
  | "margin-inline" -> Prop Margin_inline
  | "margin-inline-end" -> Prop Margin_inline_end
  | "margin-block" -> Prop Margin_block
  | "padding" -> Prop Padding
  | "padding-left" -> Prop Padding_left
  | "padding-right" -> Prop Padding_right
  | "padding-top" -> Prop Padding_top
  | "padding-bottom" -> Prop Padding_bottom
  | "padding-inline" -> Prop Padding_inline
  | "padding-inline-start" -> Prop Padding_inline_start
  | "padding-inline-end" -> Prop Padding_inline_end
  | "padding-block" -> Prop Padding_block
  | "font-size" -> Prop Font_size
  | "font-weight" -> Prop Font_weight
  | "font-style" -> Prop Font_style
  | "font-family" -> Prop Font_family
  | "font-feature-settings" -> Prop Font_feature_settings
  | "font-variation-settings" -> Prop Font_variation_settings
  | "text-align" -> Prop Text_align
  | "text-decoration" -> Prop Text_decoration
  | "text-transform" -> Prop Text_transform
  | "text-indent" -> Prop Text_indent
  | "letter-spacing" -> Prop Letter_spacing
  | "flex" -> Prop Flex
  | "flex-direction" -> Prop Flex_direction
  | "flex-wrap" -> Prop Flex_wrap
  | "align-items" -> Prop Align_items
  | "justify-content" -> Prop Justify_content
  | "opacity" -> Prop Opacity
  | "animation-name" -> Prop Animation_name
  | "transform" -> Prop Transform
  | "transform-origin" -> Prop Transform_origin
  | "box-sizing" -> Prop Box_sizing
  | "grid-template-columns" -> Prop Grid_template_columns
  | "grid-template-rows" -> Prop Grid_template_rows
  | "box-shadow" -> Prop Box_shadow
  | "content" -> Prop Content
  | "accent-color" -> Prop Accent_color
  | "caret-color" -> Prop Caret_color
  (* Common properties that were missing *)
  | "border" -> Prop Border
  | "resize" -> Prop Resize
  | "user-select" -> Prop User_select
  | "pointer-events" -> Prop Pointer_events
  | "cursor" -> Prop Cursor
  | "appearance" -> Prop Appearance
  | "filter" -> Prop Filter
  | "transition" -> Prop Transition
  | "animation" -> Prop Animation
  | "text-shadow" -> Prop Text_shadow
  | "font" -> Prop Font
  | "outline" -> Prop Outline
  | "z-index" -> Prop Z_index
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
  | "animation-timing-function" -> Prop Animation_timing_function
  | "aspect-ratio" -> Prop Aspect_ratio
  | "backdrop-filter" -> Prop Backdrop_filter
  | "backface-visibility" -> Prop Backface_visibility
  | "background-attachment" -> Prop Background_attachment
  | "background-blend-mode" -> Prop Background_blend_mode
  | "background-position" -> Prop Background_position
  | "background-repeat" -> Prop Background_repeat
  | "background-size" -> Prop Background_size
  | "border-inline-end-color" -> Prop Border_inline_end_color
  | "border-inline-end-width" -> Prop Border_inline_end_width
  | "border-inline-start-color" -> Prop Border_inline_start_color
  | "border-inline-start-width" -> Prop Border_inline_start_width
  | "border-spacing" -> Prop Border_spacing
  | "clear" -> Prop Clear
  | "clip" -> Prop Clip
  | "clip-path" -> Prop Clip_path
  | "column-gap" -> Prop Column_gap
  | "contain" -> Prop Contain
  | "container-name" -> Prop Container_name
  | "container-type" -> Prop Container_type
  | "content-visibility" -> Prop Content_visibility
  | "direction" -> Prop Direction
  | "fill" -> Prop Fill
  | "flex-basis" -> Prop Flex_basis
  | "flex-grow" -> Prop Flex_grow
  | "flex-shrink" -> Prop Flex_shrink
  | "float" -> Prop Float
  | "font-stretch" -> Prop Font_stretch
  | "font-variant-numeric" -> Prop Font_variant_numeric
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
  | "object-position" -> Prop Object_position
  | "order" -> Prop Order
  | "outline-offset" -> Prop Outline_offset
  | "outline-style" -> Prop Outline_style
  | "outline-width" -> Prop Outline_width
  | "overflow-wrap" -> Prop Overflow_wrap
  | "overscroll-behavior" -> Prop Overscroll_behavior
  | "overscroll-behavior-x" -> Prop Overscroll_behavior_x
  | "overscroll-behavior-y" -> Prop Overscroll_behavior_y
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
  | "scroll-padding" -> Prop Scroll_padding
  | "scroll-padding-bottom" -> Prop Scroll_padding_bottom
  | "scroll-padding-left" -> Prop Scroll_padding_left
  | "scroll-padding-right" -> Prop Scroll_padding_right
  | "scroll-padding-top" -> Prop Scroll_padding_top
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
  | "touch-action" -> Prop Touch_action
  | "transform-style" -> Prop Transform_style
  | "transition-delay" -> Prop Transition_delay
  | "transition-duration" -> Prop Transition_duration
  | "transition-timing-function" -> Prop Transition_timing_function
  | "unicode-bidi" -> Prop Unicode_bidi
  | "white-space" -> Prop White_space
  | "will-change" -> Prop Will_change
  | "word-break" -> Prop Word_break
  | "word-spacing" -> Prop Word_spacing
  | "writing-mode" -> Prop Writing_mode
  (* Vendor prefixed properties *)
  | "-webkit-transform" -> Prop Webkit_transform
  | "-webkit-transition" -> Prop Webkit_transition
  | "-webkit-filter" -> Prop Webkit_filter
  | "-webkit-text-size-adjust" -> Prop Webkit_text_size_adjust
  | "-webkit-tap-highlight-color" -> Prop Webkit_tap_highlight_color
  | "-webkit-text-decoration" -> Prop Webkit_text_decoration
  | "-webkit-text-decoration-color" -> Prop Webkit_text_decoration_color
  | "-webkit-appearance" -> Prop Webkit_appearance
  | "-webkit-font-smoothing" -> Prop Webkit_font_smoothing
  | "-webkit-line-clamp" -> Prop Webkit_line_clamp
  | "-webkit-box-orient" -> Prop Webkit_box_orient
  | "-webkit-hyphens" -> Prop Webkit_hyphens
  | "-moz-appearance" -> Prop Moz_appearance
  | "-moz-osx-font-smoothing" -> Prop Moz_osx_font_smoothing
  | "-ms-filter" -> Prop Ms_filter
  | "-o-transition" -> Prop O_transition
  (* PROPERTY_MATCHING_END - Used by scripts/check_properties.ml *)
  | _ -> Reader.err_invalid t ("read_property: unknown property " ^ prop_name)

(* Helper functions for property types *)
let shadow ?(inset = false) ?(h_offset : length option)
    ?(v_offset : length option) ?(blur : length option)
    ?(spread : length option) ?(color : color option) () : shadow =
  let default_color = Rgb { r = Int 0; g = Int 0; b = Int 0 } in
  if inset then
    Inset
      ( Option.value h_offset ~default:(Px 0.),
        Option.value v_offset ~default:(Px 0.),
        Some (Option.value blur ~default:(Px 0.)),
        Some (Option.value spread ~default:(Px 0.)),
        Some (Option.value color ~default:default_color) )
  else
    Simple
      ( Option.value h_offset ~default:(Px 0.),
        Option.value v_offset ~default:(Px 0.),
        Some (Option.value blur ~default:(Px 0.)),
        Some (Option.value spread ~default:(Px 0.)),
        Some (Option.value color ~default:default_color) )

let inset_ring_shadow ?(h_offset : length option) ?(v_offset : length option)
    ?(blur : length option) ?(spread : length option) ?(color : color option) ()
    : shadow =
  let h_offset = Option.value h_offset ~default:(Zero : length) in
  let v_offset = Option.value v_offset ~default:(Zero : length) in
  Inset (h_offset, v_offset, blur, spread, color)

let box_shadows shadows =
  match shadows with
  | [] -> failwith "box_shadows requires at least one shadow"
  | [ s ] -> (
      match s with
      | Simple (h, v, b, s, c) ->
          Shadow
            {
              inset = false;
              h_offset = h;
              v_offset = v;
              blur = b;
              spread = s;
              color = c;
            }
      | Inset (h, v, b, s, c) ->
          Shadow
            {
              inset = true;
              h_offset = h;
              v_offset = v;
              blur = b;
              spread = s;
              color = c;
            }
      | _ -> failwith "Invalid shadow type for box_shadows")
  | _ -> failwith "Multiple shadows not yet supported"

(* make_animation removed - unused *)

(* Background and animation helpers (moved from Css) *)
let url path : background_image = Url path
let linear_gradient dir stops = Linear_gradient (dir, stops)
let radial_gradient stops = Radial_gradient stops
let color_stop c = Color_stop c
let color_position c pos = Color_position (c, pos)

let make_animation ?name ?duration ?timing_function ?delay ?iteration_count
    ?direction ?fill_mode ?play_state () =
  {
    name;
    duration;
    timing_function;
    delay;
    iteration_count;
    direction;
    fill_mode;
    play_state;
  }

(* Background constructor with optional arguments *)
let background ?color ?image ?position ?size ?repeat ?attachment ?clip ?origin
    () =
  { color; image; position; size; repeat; attachment; clip; origin }

(* Parser for background_box values *)
let read_background_box t : background_box =
  Reader.enum "background-box"
    [
      ("border-box", (Border_box : background_box));
      ("padding-box", Padding_box);
      ("content-box", Content_box);
      ("text", Text);
      ("inherit", Inherit);
    ]
    t

let read_position_component t : position_component =
  let read_pct t : position_component =
    let n = Reader.number t in
    Reader.expect '%' t;
    Pct n
  in
  let read_length_value t : position_component =
    read_position_component_length t
  in
  Reader.enum "position-component"
    [
      ("left", (Left : position_component));
      ("center", Center);
      ("right", Right);
      ("top", Top);
      ("bottom", Bottom);
    ]
    ~default:(fun t -> Reader.one_of [ read_length_value; read_pct ] t)
    t

let read_position_2d t : position_2d =
  Reader.enum "position-2d"
    [ ("center", Center); ("inherit", Inherit) ]
    ~default:(fun t : position_2d ->
      let first = read_position_component t in
      Reader.ws t;
      match Reader.option read_position_component t with
      | Some second -> XY (first, second)
      | None -> XY (first, first))
    t

let read_transform_origin t : transform_origin =
  let read_component t =
    Reader.one_of
      [
        (fun t -> `Pos (read_position_component t));
        (fun t -> `Len (read_length t));
      ]
      t
  in
  Reader.enum "transform-origin"
    [ ("inherit", Inherit); ("center", Center) ]
    ~default:(fun t ->
      let components, _ = Reader.many read_component t in
      match components with
      | [ `Pos a ] -> (
          match a with Top | Bottom -> XY (Center, a) | _ -> XY (a, Center))
      | [ `Pos a; `Pos b ] -> XY (a, b)
      | [ `Pos a; `Pos b; `Len z ] -> XYZ (a, b, z)
      | [ `Pos a; `Len z ] -> XYZ (a, Center, z)
      | _ ->
          err_invalid_value t "transform-origin" "invalid component combination")
    t

(* Full CSS background shorthand parser *)
let read_background t : background =
  let read_image_item t =
    let img = read_background_image t in
    fun (bg : background) ->
      if bg.image = None then { bg with image = Some img } else bg
  in
  let read_position_size_item t =
    let pos = read_position_2d t in
    Reader.ws t;
    let size_opt =
      if Reader.slash_opt t then Some (read_background_size t) else None
    in
    fun (bg : background) ->
      if bg.position = None then
        let bg' = { bg with position = Some pos } in
        match size_opt with
        | Some s when bg'.size = None -> { bg' with size = Some s }
        | _ -> bg'
      else bg
  in
  let read_repeat_item t =
    let rep = read_background_repeat t in
    fun (bg : background) ->
      if bg.repeat = None then { bg with repeat = Some rep } else bg
  in
  let read_attachment_item t =
    let att = read_background_attachment t in
    fun (bg : background) ->
      if bg.attachment = None then { bg with attachment = Some att } else bg
  in
  let read_box_item t =
    let box = read_background_box t in
    fun (bg : background) ->
      if bg.origin = None then { bg with origin = Some box }
      else if bg.clip = None then { bg with clip = Some box }
      else bg
  in
  let read_color_item t =
    let col = read_color t in
    fun (bg : background) ->
      if bg.color = None then { bg with color = Some col } else bg
  in
  let read_item t =
    Reader.one_of
      [
        read_image_item;
        read_position_size_item;
        read_repeat_item;
        read_attachment_item;
        read_box_item;
        read_color_item;
      ]
      t
  in
  Reader.ws t;
  let init = background () in
  let apply acc upd = upd acc in
  let acc, _ = Reader.fold_many read_item ~init ~f:apply t in
  acc

let read_backgrounds t : background list =
  Reader.list ~sep:Reader.comma read_background t

(* Gap shorthand parser *)
let read_gap t : gap =
  let first_length = read_length t in
  Reader.ws t;
  let second_length = Reader.option read_length t in
  match second_length with
  | Some col_gap -> { row_gap = Some first_length; column_gap = Some col_gap }
  | None -> { row_gap = Some first_length; column_gap = Some first_length }
