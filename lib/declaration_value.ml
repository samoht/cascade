open Declaration_intf
open Common
open Properties
open Values
open Declaration_core

(* Helper to validate no extra tokens remain *)
let validate_no_extra_tokens t =
  Cursor.ws t;
  match Cursor.peek_head_shape t with
  | `Eof | `Semicolon | `Bang -> ()
  | _ ->
      (* The trailing tokens are consumed before they are judged, so the span is
         theirs rather than that of the terminator they stop at. *)
      let loc = Cursor.decl_value_loc t in
      let trimmed = Cursor.consume_to_decl_end ~trim:true t in
      if trimmed <> "" then
        Cursor.err_invalid ~loc t
          ("unexpected tokens after property value: " ^ trimmed)

(* CSS Position 3 sec. 3.1 gives every inset property [auto |
   <length-percentage>] and sec. 3.2 builds [inset] from it, so no sizing
   function reaches any of them. That is the same production CSS Box 4 sec. 3.1
   gives a margin, so they share its component reader. *)
let read_inset_length t = Values.read_margin_length ~global:true t

let read_length_box t =
  let values = Cursor.list ~at_least:1 ~at_most:4 read_inset_length t in
  if values = [] then Cursor.err_expected t "length value";
  values

let read_inset_longhand t = [ read_inset_length t ]
let read_inset_axis t = Cursor.list ~at_least:1 ~at_most:2 read_inset_length t

let read_border_width_box t =
  Cursor.list ~at_least:1 ~at_most:4 read_border_width t

let read_text_decoration_lines t =
  let lines = Cursor.list ~at_least:1 read_text_decoration_line t in
  let is_none (line : text_decoration_line) =
    match line with None -> true | _ -> false
  in
  let is_error_kind (line : text_decoration_line) =
    match line with Spelling_error | Grammar_error -> true | _ -> false
  in
  if List.exists is_none lines && List.length lines > 1 then
    Cursor.err_invalid t "none cannot be combined with text-decoration lines";
  (* CSS Text Decoration 4 section 2.1: [<text-decoration-line>] is [none |
     [underline || overline || line-through || blink] | spelling-error |
     grammar-error]. [spelling-error] and [grammar-error] are alternatives to
     the [||] group, not members of it, so they cannot be combined with any
     other line. *)
  if List.exists is_error_kind lines && List.length lines > 1 then
    Cursor.err_invalid t
      "spelling-error and grammar-error cannot be combined with other \
       text-decoration lines";
  let rec duplicates = function
    | [] -> false
    | x :: xs -> List.mem x xs || duplicates xs
  in
  if duplicates lines then Cursor.err_invalid t "duplicate text-decoration-line";
  lines

(* CSS Shapes 1 sec. 5: [<shape-box>] is [<visual-box> | margin-box], so the
   three SVG boxes [<geometry-box>] adds are not valid here. *)
let check_shape_box t (box : clip_geometry_box) =
  match box with
  | Margin_box | Border_box | Padding_box | Content_box -> ()
  | Fill_box | Stroke_box | View_box ->
      Cursor.err_invalid t "shape-outside takes a <shape-box>"

(* [read_clip_path] reads [<basic-shape> || <geometry-box>], the same double-bar
   pair shape-outside uses, along with [none], [url()] and the CSS-wide
   keywords. Shapes 1 sec. 6.1 differs on two points, checked here: the box is a
   [<shape-box>], and everything outside [<basic-shape>] is an alternative to
   the pair rather than a member of it. *)
let check_shape_outside_shape t =
  let is_basic_shape (shape : clip_path) =
    match shape with
    | Clip_path_inset _ | Clip_path_circle _ | Clip_path_ellipse _
    | Clip_path_polygon _ | Clip_path_path _ | Clip_path_shape _
    | Clip_path_xywh _ | Clip_path_rect _ ->
        true
    (* A shape the [clip_path] reader kept verbatim as spec-invalid: the raw
       text survives here too rather than costing the whole declaration. *)
    | Invalid _ -> true
    | _ -> false
  in
  match read_clip_path t with
  | Clip_path_box box -> check_shape_box t box
  | Clip_path_with_box { shape; box; _ } when is_basic_shape shape ->
      check_shape_box t box
  | Clip_path_with_box _ ->
      Cursor.err_invalid t "shape-outside pairs a <shape-box> with a shape"
  | _ -> ()

(* CSS Shapes 1 sec. 6.1: [shape-outside] is [none | [<basic-shape> ||
   <shape-box>] | <image>]. The value is the raw source text ([Shape_outside :
   string property]): typing it would mean a sum of the [clip_path] shapes and
   the whole [background_image] type for [<image>], so the reader validates the
   grammar and hands the text back verbatim. *)
let read_shape_outside t =
  let raw = Cursor.lookahead (Cursor.consume_to_decl_end ~trim:true) t in
  let read_shape_image t =
    ignore (read_background_image t : background_image)
  in
  (match Cursor.peek t with
  | Some
      (Component.Func
         { node = { name; terminated = true; arguments = []; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      Cursor.err_invalid t "empty var()"
  | Some (Component.Func { node = { name; terminated = true; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      let _ : string var =
        Values.read_var
          (fun inner -> Cursor.consume_remaining_as_string ~trim:true inner)
          t
      in
      ()
  | _ ->
      Cursor.one_of
        [
          check_shape_outside_shape;
          read_shape_image;
          (fun t -> Cursor.err_invalid t ("invalid shape-outside: " ^ raw));
        ]
        t);
  Cursor.expect_eof t;
  raw

(* Some properties (shape-margin, scroll-padding, padding, etc.) require a
   non-negative length-percentage. Detect a leading [-] number/percentage and
   reject before delegating to the typed reader. *)
let read_non_negative_length_percentage t =
  Values.read_length_percentage ~allow_negative:false ~with_keywords:false t

let read_nn_lp_or_global t =
  Cursor.enum "non-negative length-percentage"
    [
      ("inherit", (Length Inherit : length_percentage));
      ("initial", Length Initial);
      ("unset", Length Unset);
      ("revert", Length Revert);
      ("revert-layer", Length Revert_layer);
    ]
    ~default:read_non_negative_length_percentage t

(* CSS Motion Path 1 sec. 2.2 gives [offset-distance] a plain
   [<length-percentage>], with no range restriction on it. *)
let read_lp_or_global t =
  Cursor.enum "length-percentage"
    [
      ("inherit", (Length Inherit : length_percentage));
      ("initial", Length Initial);
      ("unset", Length Unset);
      ("revert", Length Revert);
      ("revert-layer", Length Revert_layer);
    ]
    ~default:(Values.read_length_percentage ~with_keywords:false)
    t

(* CSS Box 4 sec. 3.1 gives a margin longhand [<length-percentage> | auto], and
   CSS Cascade 5 sec. 7.3 gives every property the CSS-wide keywords, which a
   longhand takes as its whole value where a shorthand component cannot. *)
let read_margin_length_or_global t = Values.read_margin_length ~global:true t

let read_nn_length_or_global ?(length_only = false) t =
  Cursor.enum "non-negative length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:
      (read_length ~allow_negative:false ~with_keywords:false ~length_only)
    t

(* CSS Backgrounds 3 (ED) sec. 4.1: each [border-*-radius] corner longhand is
   [<length-percentage [0,inf]>{1,2}], the horizontal radius then the vertical
   one; a single value sets both. *)
let read_corner_radius t =
  Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_nn_length_or_global t

let read_gap_longhand t =
  Cursor.enum "gap"
    [ ("normal", (Normal : length)) ]
    ~default:read_nn_length_or_global t

(* CSS Animations 2 sec. 4.1: [animation-duration] is [ auto | <time [0s,inf]>
   ]#. The transition durations and both delays keep the time grammar alone. *)
let read_animation_duration t =
  Cursor.enum "animation-duration"
    [ ("auto", (Auto : duration)) ]
    ~default:read_duration t

(* CSS Text 3 section 7: [letter-spacing] is [normal | <length>], [word-spacing]
   is [normal | <length-percentage>]; both accept negative values. *)
let read_normal_or_length name t =
  Cursor.enum name
    [
      ("normal", (Normal : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(Values.read_length ~with_keywords:false)
    t

(* CSS Transforms 2 sec. 3: [perspective] is [none | <length [0,inf]>]; the
   keyword is its initial value, so it has to read. *)
let read_perspective_value t =
  Cursor.enum "perspective"
    [
      ("none", (None : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    (* CSS Transforms 2 (ED) sec. 3: [perspective] is [none | <length [0,inf]>],
       so it takes no percentage; Chrome 146 refuses one. *)
    ~default:(read_non_negative_length ~with_keywords:false ~length_only:true)
    t

(* CSS Text Decoration 4 sec. 2.8: [text-underline-offset] is [auto |
   <length-percentage>], and unlike the lengths above it may be negative. *)
let read_underline_offset t =
  Cursor.enum "text-underline-offset"
    [
      ("auto", (Auto : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(Values.read_length ~with_keywords:false)
    t

let read_letter_spacing t = read_normal_or_length "letter-spacing" t
let read_word_spacing t = read_normal_or_length "word-spacing" t

(* CSS Logical Properties 1 3.5: [padding-block] / [padding-inline] are 2-value
   shorthands. A bare CSS-wide keyword counts as a single value (not a list
   element). *)
let read_padding_logical_shorthand t =
  let read_global_singleton t =
    let kw : length =
      Cursor.enum "padding logical"
        [
          ("inherit", (Inherit : length));
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        t
    in
    [ kw ]
  in
  let read_lengths t =
    Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
      (Values.read_length ~allow_negative:false ~with_keywords:false)
      t
  in
  Cursor.one_of [ read_global_singleton; read_lengths ] t

(* CSS Scroll Snap 1 sec. 5.1: the [scroll-margin] longhands are [<length>] - an
   unrestricted range, so an outset may be negative just as a margin may. Only
   [scroll-padding] (sec. 4.2) says "Negative values are invalid". "Percentages:
   n/a" rules a percentage out, nested in math as much as written on its own. *)
let read_scroll_margin_length t =
  Cursor.enum "scroll-margin length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(fun t -> read_length ~with_keywords:false ~length_only:true t)
    t

let read_scroll_padding_length t =
  Cursor.enum "scroll-padding length"
    [
      ("auto", (Auto : length));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:(read_non_negative_length ~with_keywords:false)
    t

let rec read_length_or_css_wide t =
  Cursor.enum_or_calls "length"
    [
      ("inherit", (Inherit : length));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_length_or_css_wide t)) ]
    ~default:(read_length ~with_keywords:false ~length_only:true)
    t

let rec read_text_decoration_thickness t =
  Cursor.enum_or_calls "text-decoration-thickness"
    [
      ("auto", (Auto : length));
      ("from-font", From_font);
      ("hairline", Hairline);
      ("thin", Thin);
      ("medium", Medium);
      ("thick", Thick);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (read_var read_text_decoration_thickness t));
        ( "calc",
          fun t ->
            Calc
              (read_calc ~result_type:`Value
                 (read_length ~with_keywords:false)
                 t) );
      ]
    ~default:(read_length ~with_keywords:false)
    t

(* Delegate to the proper reader in Properties *)
let read_translate_value t : Properties_intf.translate_value =
  Properties.read_translate_value t

let read_transform_value t = v Transform (read_transforms t)
let read_webkit_transform_value t = v Webkit_transform (read_transforms t)

let rec justify_self_of_align_self (a : align_self) : justify_self =
  match a with
  | Auto -> Auto
  | Normal -> Normal
  | Stretch -> Stretch
  | Baseline -> Baseline
  | First_baseline -> First_baseline
  | Last_baseline -> Last_baseline
  | Center -> Center
  | Start -> Start
  | End -> End
  | Self_start -> Self_start
  | Self_end -> Self_end
  | Flex_start -> Flex_start
  | Flex_end -> Flex_end
  | Safe_center -> Safe_center
  | Safe_start -> Safe_start
  | Safe_end -> Safe_end
  | Safe_flex_start -> Safe_flex_start
  | Safe_flex_end -> Safe_flex_end
  | Unsafe_center -> Unsafe_center
  | Unsafe_start -> Unsafe_start
  | Unsafe_end -> Unsafe_end
  | Unsafe_self_start -> Unsafe_self_start
  | Unsafe_self_end -> Unsafe_self_end
  | Unsafe_flex_start -> Unsafe_flex_start
  | Unsafe_flex_end -> Unsafe_flex_end
  | Inherit -> Inherit
  | Initial -> Initial
  | Unset -> Unset
  | Revert -> Revert
  | Revert_layer -> Revert_layer
  | Var v -> Var (justify_var_of_align_var v)

and justify_var_of_align_var (v : align_self var) : justify_self var =
  let fallback : justify_self fallback =
    match v.fallback with
    | None -> None
    | Empty -> Empty
    | Empty2 -> Empty2
    | Var_fallback name -> Var_fallback name
    | Syntax_fallback value -> Syntax_fallback value
    | Fallback value -> Fallback (justify_self_of_align_self value)
  in
  {
    name = v.name;
    fallback;
    default = Option.map justify_self_of_align_self v.default;
    layer = v.layer;
    meta = v.meta;
    runtime = v.runtime;
  }

let read_place_self_value t =
  let a = read_align_self t in
  Cursor.ws t;
  let j = Cursor.option read_justify_self t in
  (* Per CSS spec, when only one value is given, both values are set to it. *)
  let pair =
    match j with None -> (a, justify_self_of_align_self a) | Some jj -> (a, jj)
  in
  v Place_self pair

let read_background_blend_mode_value t =
  v Background_blend_mode (Cursor.list ~sep:Cursor.comma read_blend_mode t)

(* The spelling a property parses back from, and the one a diagnostic names: a
   minified [page-break-*] is the CSS Fragmentation 3 sec. 3.4 [break-*] alias
   of a different property, which reports the failure against a property the
   declaration never wrote. *)
let canonical_prop_name (type a) (prop : a property) =
  Pp.to_string ~minify:false pp_property prop

type value_reader = {
  read_value_opt : 'a. 'a property -> Cursor.t -> declaration option;
}

let read_color_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Color -> Some (v Color (read_color t))
  | Background_color -> Some (v Background_color (read_color t))
  | Border_color ->
      (* CSS Backgrounds 3 sec. 3.1: [border-color] is a 1-4 value box shorthand
         (top / right / bottom / left). *)
      Some (v Border_color (Cursor.list ~at_least:1 ~at_most:4 read_color t))
  | Outline_color -> Some (v Outline_color (Prop_common.read_auto_color t))
  | Border_top_color -> Some (v Border_top_color (read_color t))
  | Border_right_color -> Some (v Border_right_color (read_color t))
  | Border_bottom_color -> Some (v Border_bottom_color (read_color t))
  | Border_left_color -> Some (v Border_left_color (read_color t))
  | Text_decoration_color -> Some (v Text_decoration_color (read_color t))
  | Text_emphasis_color -> Some (v Text_emphasis_color (read_color t))
  | Accent_color -> Some (v Accent_color (Prop_common.read_auto_color t))
  | Caret_color -> Some (v Caret_color (Prop_common.read_auto_color t))
  | Stop_color -> Some (v Stop_color (read_color t))
  | Flood_color -> Some (v Flood_color (read_color t))
  | Lighting_color -> Some (v Lighting_color (read_color t))
  | Webkit_tap_highlight_color ->
      Some (v Webkit_tap_highlight_color (read_color t))
  | Webkit_text_decoration_color ->
      Some (v Webkit_text_decoration_color (read_color t))
  | _ -> None

(* CSS Sizing 3 sec. 5: the box sizes are the properties the intrinsic sizes
   belong to, so this reader asks for them. *)
let read_box_size ?(maximum = false) t =
  match read_length_percentage ~allow_negative:false ~sizing:true t with
  | Length (Normal | From_font | Size) ->
      Cursor.err_invalid t "expected a box size"
  | Length Auto when maximum ->
      Cursor.err_invalid t "maximum sizes do not accept auto"
  | Length None when not maximum ->
      Cursor.err_invalid t "only maximum sizes accept none"
  | size -> size

let read_sizing_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Width -> Some (v Width (read_box_size t))
  | Height -> Some (v Height (read_box_size t))
  | Min_width -> Some (v Min_width (read_box_size t))
  | Min_height -> Some (v Min_height (read_box_size t))
  | Max_width -> Some (v Max_width (read_box_size ~maximum:true t))
  | Max_height -> Some (v Max_height (read_box_size ~maximum:true t))
  | Inline_size -> Some (v Inline_size (read_box_size t))
  | Min_inline_size -> Some (v Min_inline_size (read_box_size t))
  | Max_inline_size -> Some (v Max_inline_size (read_box_size ~maximum:true t))
  | Block_size -> Some (v Block_size (read_box_size t))
  | Min_block_size -> Some (v Min_block_size (read_box_size t))
  | Max_block_size -> Some (v Max_block_size (read_box_size ~maximum:true t))
  | Font_size -> Some (v Font_size (Properties.read_font_size t))
  | Perspective -> Some (v Perspective (read_perspective_value t))
  | Offset_distance -> Some (v Offset_distance (read_lp_or_global t))
  | Shape_margin -> Some (v Shape_margin (read_nn_lp_or_global t))
  | Line_height_step ->
      Some (v Line_height_step (read_nn_length_or_global ~length_only:true t))
  | _ -> None

let read_radius_gap_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | Border_radius -> Some (v Border_radius (read_border_radius t))
  | Border_top_left_radius ->
      Some (v Border_top_left_radius (read_corner_radius t))
  | Border_top_right_radius ->
      Some (v Border_top_right_radius (read_corner_radius t))
  | Border_bottom_left_radius ->
      Some (v Border_bottom_left_radius (read_corner_radius t))
  | Border_bottom_right_radius ->
      Some (v Border_bottom_right_radius (read_corner_radius t))
  | Border_start_start_radius ->
      Some (v Border_start_start_radius (read_corner_radius t))
  | Border_start_end_radius ->
      Some (v Border_start_end_radius (read_corner_radius t))
  | Border_end_start_radius ->
      Some (v Border_end_start_radius (read_corner_radius t))
  | Border_end_end_radius ->
      Some (v Border_end_end_radius (read_corner_radius t))
  | Gap -> Some (v Gap (Properties.read_gap t))
  | Column_gap -> Some (v Column_gap (read_gap_longhand t))
  | Row_gap -> Some (v Row_gap (read_gap_longhand t))
  | _ -> None

let read_layout_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Display -> Some (v Display (read_display t))
  | Position -> Some (v Position (read_position t))
  | Visibility -> Some (v Visibility (read_visibility t))
  | Baseline_source -> Some (v Baseline_source (read_baseline_source t))
  | Alignment_baseline ->
      Some (v Alignment_baseline (read_alignment_baseline t))
  | Baseline_shift -> Some (v Baseline_shift (read_baseline_shift t))
  | Overflow -> Some (v Overflow (read_overflow t))
  | Overflow_x -> Some (v Overflow_x (read_overflow_single t))
  | Overflow_y -> Some (v Overflow_y (read_overflow_single t))
  | Overflow_block -> Some (v Overflow_block (read_overflow_single t))
  | Overflow_inline -> Some (v Overflow_inline (read_overflow_single t))
  | Content_visibility ->
      Some (v Content_visibility (read_content_visibility t))
  | Aspect_ratio -> Some (v Aspect_ratio (read_aspect_ratio t))
  | Vertical_align -> Some (v Vertical_align (read_vertical_align t))
  | _ -> None

let read_box_edge_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Padding -> Some (v Padding (read_padding_shorthand t))
  | Margin -> Some (v Margin (read_margin_shorthand t))
  | Border_style -> Some (v Border_style (read_border_style_box t))
  | Border_width -> Some (v Border_width (read_border_width_box t))
  | Border_top_width -> Some (v Border_top_width (read_border_width t))
  | Border_right_width -> Some (v Border_right_width (read_border_width t))
  | Border_bottom_width -> Some (v Border_bottom_width (read_border_width t))
  | Border_left_width -> Some (v Border_left_width (read_border_width t))
  | Border_top_style -> Some (v Border_top_style (read_border_style t))
  | Border_right_style -> Some (v Border_right_style (read_border_style t))
  | Border_bottom_style -> Some (v Border_bottom_style (read_border_style t))
  | Border_left_style -> Some (v Border_left_style (read_border_style t))
  | Border_inline_start_width ->
      Some (v Border_inline_start_width (read_border_width t))
  | Border_inline_end_width ->
      Some (v Border_inline_end_width (read_border_width t))
  | Border_block_start_width ->
      Some (v Border_block_start_width (read_border_width t))
  | Border_block_end_width ->
      Some (v Border_block_end_width (read_border_width t))
  | Border_inline_start_color ->
      Some (v Border_inline_start_color (read_color t))
  | Border_inline_end_color -> Some (v Border_inline_end_color (read_color t))
  | Border_block_start_color -> Some (v Border_block_start_color (read_color t))
  | Border_block_end_color -> Some (v Border_block_end_color (read_color t))
  | Border_inline_color ->
      Some (v Border_inline_color (read_logical_border_color t))
  | Border_block_color ->
      Some (v Border_block_color (read_logical_border_color t))
  | Border_inline_width ->
      Some (v Border_inline_width (read_logical_border_width t))
  | Border_block_width ->
      Some (v Border_block_width (read_logical_border_width t))
  | Border_inline_style ->
      Some (v Border_inline_style (read_logical_border_style t))
  | Border_block_style ->
      Some (v Border_block_style (read_logical_border_style t))
  | Border_inline_start_style ->
      Some (v Border_inline_start_style (read_border_style t))
  | Border_inline_end_style ->
      Some (v Border_inline_end_style (read_border_style t))
  | Border_block_start_style ->
      Some (v Border_block_start_style (read_border_style t))
  | Border_block_end_style ->
      Some (v Border_block_end_style (read_border_style t))
  | _ -> None

let read_type_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Line_height -> Some (v Line_height (read_line_height t))
  | Font_weight -> Some (v Font_weight (read_font_weight t))
  | Font_style -> Some (v Font_style (read_font_style t))
  | Font_family -> Some (v Font_family (read_font_family t))
  | Font -> Some (v Font (Properties.read_font t))
  | Source -> Some (v Source (Properties.read_font_src t))
  | Text_align -> Some (v Text_align (read_text_align t))
  | Text_transform -> Some (v Text_transform (read_text_transform t))
  | White_space -> Some (v White_space (read_white_space t))
  | White_space_collapse ->
      Some (v White_space_collapse (read_white_space_collapse t))
  | Font_variant_alternates ->
      Some (v Font_variant_alternates (read_font_variant_alternates t))
  | Font_variant -> Some (v Font_variant (read_font_variant t))
  | Text_decoration -> Some (v Text_decoration (read_text_decoration t))
  | Text_decoration_line ->
      Some (v Text_decoration_line (read_text_decoration_lines t))
  | Text_decoration_style ->
      Some (v Text_decoration_style (read_text_decoration_style t))
  | Text_underline_offset ->
      Some (v Text_underline_offset (read_underline_offset t))
  | Text_emphasis -> Some (v Text_emphasis (read_text_emphasis t))
  | Text_emphasis_style ->
      Some (v Text_emphasis_style (read_text_emphasis_style t))
  | Text_emphasis_position ->
      Some (v Text_emphasis_position (read_text_emphasis_position t))
  | Text_emphasis_skip ->
      Some (v Text_emphasis_skip (read_text_emphasis_skip t))
  | Text_orientation -> Some (v Text_orientation (read_text_orientation t))
  | Letter_spacing -> Some (v Letter_spacing (read_letter_spacing t))
  | Transform_origin -> Some (v Transform_origin (read_transform_origin t))
  | Transform_box -> Some (v Transform_box (read_transform_box t))
  | _ -> None

let read_flex_effect_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Flex_direction -> Some (v Flex_direction (read_flex_direction t))
  | Flex_wrap -> Some (v Flex_wrap (read_flex_wrap t))
  | Flex_flow -> Some (v Flex_flow (read_flex_flow t))
  | Flex -> Some (v Flex (read_flex t))
  | Flex_grow -> Some (v Flex_grow (Properties.read_flex_factor t))
  | Flex_shrink -> Some (v Flex_shrink (Properties.read_flex_factor t))
  | Flex_basis -> Some (v Flex_basis (read_flex_basis t))
  | Align_items -> Some (v Align_items (read_align_items t))
  | Justify_content -> Some (v Justify_content (read_justify_content t))
  | Transform -> Some (read_transform_value t)
  | Translate -> Some (v Translate (read_translate_value t))
  | Webkit_transform -> Some (read_webkit_transform_value t)
  | Moz_transform -> Some (v Moz_transform (read_transforms t))
  | Ms_transform -> Some (v Ms_transform (read_transforms t))
  | O_transform -> Some (v O_transform (read_transforms t))
  | Webkit_transition -> Some (v Webkit_transition (read_transitions t))
  | Webkit_animation -> Some (v Webkit_animation (read_animations t))
  | Webkit_filter -> Some (v Webkit_filter (read_filter t))
  | Moz_appearance -> Some (v Moz_appearance (read_appearance t))
  | Ms_filter -> Some (v Ms_filter (read_filter t))
  | O_transition -> Some (v O_transition (read_transitions t))
  | Filter -> Some (v Filter (read_filter t))
  | Appearance -> Some (v Appearance (read_appearance t))
  | Color_scheme -> Some (v Color_scheme (read_color_scheme t))
  | _ -> None

let read_vendor_alias_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Webkit_transition_delay ->
      Some (v Webkit_transition_delay (read_duration_list read_time t))
  | Webkit_transition_duration ->
      Some (v Webkit_transition_duration (read_duration_list read_duration t))
  | Webkit_transition_property ->
      Some (v Webkit_transition_property (read_transition_property t))
  | Webkit_transition_timing_function ->
      Some (v Webkit_transition_timing_function (read_timing_function_list t))
  | Webkit_animation_delay ->
      Some (v Webkit_animation_delay (read_duration_list read_time t))
  | Webkit_animation_duration ->
      Some
        (v Webkit_animation_duration
           (read_duration_list read_animation_duration t))
  | Webkit_animation_direction ->
      Some (v Webkit_animation_direction (read_animation_direction t))
  | Webkit_animation_iteration_count ->
      Some
        (v Webkit_animation_iteration_count (read_animation_iteration_count t))
  | Webkit_animation_name ->
      Some (v Webkit_animation_name (read_animation_name t))
  | Webkit_animation_timing_function ->
      Some (v Webkit_animation_timing_function (read_timing_function_list t))
  | Webkit_animation_fill_mode ->
      Some (v Webkit_animation_fill_mode (read_animation_fill_mode t))
  | Webkit_animation_play_state ->
      Some (v Webkit_animation_play_state (read_animation_play_state t))
  | Webkit_flex_direction ->
      Some (v Webkit_flex_direction (read_flex_direction t))
  | Webkit_flex_wrap -> Some (v Webkit_flex_wrap (read_flex_wrap t))
  | Webkit_flex_flow -> Some (v Webkit_flex_flow (read_flex_flow t))
  | Webkit_justify_content ->
      Some (v Webkit_justify_content (read_justify_content t))
  | Webkit_align_content -> Some (v Webkit_align_content (read_align_content t))
  | Webkit_align_items -> Some (v Webkit_align_items (read_align_items t))
  | Webkit_align_self -> Some (v Webkit_align_self (read_align_self t))
  | Webkit_border_radius -> Some (v Webkit_border_radius (read_border_radius t))
  | Webkit_box_shadow -> Some (v Webkit_box_shadow (read_shadow t))
  | Webkit_background_size ->
      Some (v Webkit_background_size (read_background_size_list t))
  | Moz_animation -> Some (v Moz_animation (read_animations t))
  | Moz_animation_delay ->
      Some (v Moz_animation_delay (read_duration_list read_time t))
  | Moz_animation_duration ->
      Some
        (v Moz_animation_duration
           (read_duration_list read_animation_duration t))
  | Moz_animation_direction ->
      Some (v Moz_animation_direction (read_animation_direction t))
  | Moz_animation_iteration_count ->
      Some (v Moz_animation_iteration_count (read_animation_iteration_count t))
  | Moz_animation_name -> Some (v Moz_animation_name (read_animation_name t))
  | Moz_animation_timing_function ->
      Some (v Moz_animation_timing_function (read_timing_function_list t))
  | Moz_animation_fill_mode ->
      Some (v Moz_animation_fill_mode (read_animation_fill_mode t))
  | Moz_animation_play_state ->
      Some (v Moz_animation_play_state (read_animation_play_state t))
  | Moz_transition -> Some (v Moz_transition (read_transitions t))
  | Moz_transition_delay ->
      Some (v Moz_transition_delay (read_duration_list read_time t))
  | Moz_transition_duration ->
      Some (v Moz_transition_duration (read_duration_list read_duration t))
  | Moz_transition_property ->
      Some (v Moz_transition_property (read_transition_property t))
  | Moz_transition_timing_function ->
      Some (v Moz_transition_timing_function (read_timing_function_list t))
  | Moz_border_radius -> Some (v Moz_border_radius (read_border_radius t))
  | Moz_box_shadow -> Some (v Moz_box_shadow (read_shadow t))
  | _ -> None

let read_background_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | Background_image ->
      let images = read_background_images t in
      Some (v Background_image images)
  | Background -> Some (v Background (read_backgrounds t))
  | Border -> Some (v Border (read_border t))
  | Background_attachment ->
      Some (v Background_attachment (read_background_attachment t))
  | Background_origin -> Some (v Background_origin (read_background_box_list t))
  | Background_clip -> Some (v Background_clip (read_background_box_list t))
  | Webkit_background_clip ->
      Some (v Webkit_background_clip (read_background_box_list t))
  | Background_position ->
      Some (v Background_position (read_background_position t))
  | Background_position_x ->
      Some (v Background_position_x (read_background_position_x t))
  | Background_position_y ->
      Some (v Background_position_y (read_background_position_y t))
  | Webkit_mask_position_x ->
      Some (v Webkit_mask_position_x (read_background_position_x t))
  | Webkit_mask_position_y ->
      Some (v Webkit_mask_position_y (read_background_position_y t))
  | Background_repeat ->
      Some (v Background_repeat (read_background_repeat_list t))
  | Background_size -> Some (v Background_size (read_background_size_list t))
  | Background_blend_mode -> Some (read_background_blend_mode_value t)
  | _ -> None

(* CSS Grid 2 sec. 7.4 spells [grid-template] as [none], a
   [<'grid-template-rows'> / <'grid-template-columns'>] pair, or a track list
   built from strings. A bare track list is a [grid-template-rows] value and no
   [grid-template] one, and sec. 7.8 gives [grid] the same forms plus the two
   [auto-flow] ones. *)
let names_both_axes : Properties.grid_template -> bool = function
  | None | Inherit | Initial | Unset | Revert | Revert_layer | Var _ | Split _
  | Auto_flow_columns _ | Auto_flow_rows _ | Named_tracks _ | Template _ ->
      true
  | Px _ | Rem _ | Em _ | Pct _ | Vw _ | Vh _ | Vmin _ | Vmax _ | Zero
  | Length _ | Fr _ | Flex_math _ | Auto | Min_content | Max_content | Min_max _
  | Fit_content _ | Repeat _ | Tracks _ | Line_names _ | Subgrid | Masonry ->
      false

let read_grid_template_shorthand ~name t =
  let value =
    if String.equal name "grid" then read_grid t else read_grid_template t
  in
  if not (names_both_axes value) then
    Cursor.err_invalid t (name ^ " names one axis only");
  value

let read_grid_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Grid_template_columns ->
      Some (v Grid_template_columns (read_grid_template_tracks t))
  | Grid_template_rows ->
      Some (v Grid_template_rows (read_grid_template_tracks t))
  | Grid_row_start -> Some (v Grid_row_start (read_grid_line t))
  | Grid_row_end -> Some (v Grid_row_end (read_grid_line t))
  | Grid_column_start -> Some (v Grid_column_start (read_grid_line t))
  | Grid_column_end -> Some (v Grid_column_end (read_grid_line t))
  | Grid_auto_flow -> Some (v Grid_auto_flow (read_grid_auto_flow t))
  | Grid_template_areas ->
      Some (v Grid_template_areas (read_grid_template_areas t))
  | Grid_template ->
      Some
        (v Grid_template (read_grid_template_shorthand ~name:"grid-template" t))
  | Grid -> Some (v Grid (read_grid_template_shorthand ~name:"grid" t))
  | Grid_area -> Some (v Grid_area (read_grid_area t))
  | Grid_auto_columns -> Some (v Grid_auto_columns (read_grid_auto_tracks t))
  | Grid_auto_rows -> Some (v Grid_auto_rows (read_grid_auto_tracks t))
  | Grid_column -> Some (v Grid_column (read_grid_line_pair t))
  | Grid_row -> Some (v Grid_row (read_grid_line_pair t))
  | _ -> None

let read_shadow_content_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Box_shadow -> Some (v Box_shadow (read_shadow t))
  | Text_shadow -> Some (v Text_shadow (read_text_shadows t))
  | Content -> Some (v Content (read_content t))
  | Counter_reset -> Some (v Counter_reset (read_counter_set t))
  | Counter_increment -> Some (v Counter_increment (read_counter_set t))
  | Z_index -> Some (v Z_index (Properties.read_z_index t))
  | Opacity -> Some (v Opacity (Properties.read_opacity t))
  | Fill_opacity -> Some (v Fill_opacity (Properties.read_opacity t))
  | Stroke_opacity -> Some (v Stroke_opacity (Properties.read_opacity t))
  | Stop_opacity -> Some (v Stop_opacity (Properties.read_opacity t))
  | Flood_opacity -> Some (v Flood_opacity (Properties.read_opacity t))
  | Cursor -> Some (v Cursor (read_cursor t))
  | Interactivity -> Some (v Interactivity (read_interactivity t))
  | Caret_animation -> Some (v Caret_animation (read_caret_animation t))
  | Caret_shape -> Some (v Caret_shape (read_caret_shape t))
  | Caret -> Some (v Caret (read_caret t))
  | Interest_delay -> Some (v Interest_delay (read_interest_delay t))
  | Interest_delay_start ->
      Some (v Interest_delay_start (read_interest_delay ~longhand:true t))
  | Interest_delay_end ->
      Some (v Interest_delay_end (read_interest_delay ~longhand:true t))
  | Nav_up -> Some (v Nav_up (read_nav t))
  | Nav_right -> Some (v Nav_right (read_nav t))
  | Nav_down -> Some (v Nav_down (read_nav t))
  | Nav_left -> Some (v Nav_left (read_nav t))
  | Box_sizing -> Some (v Box_sizing (read_box_sizing t))
  | Webkit_box_sizing -> Some (v Webkit_box_sizing (read_box_sizing t))
  | Moz_box_sizing -> Some (v Moz_box_sizing (read_box_sizing t))
  | Field_sizing -> Some (v Field_sizing (read_field_sizing t))
  | Caption_side -> Some (v Caption_side (read_caption_side t))
  | _ -> None

let read_spacing_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Padding_left -> Some (v Padding_left (read_nn_length_or_global t))
  | Padding_right -> Some (v Padding_right (read_nn_length_or_global t))
  | Padding_top -> Some (v Padding_top (read_nn_length_or_global t))
  | Padding_bottom -> Some (v Padding_bottom (read_nn_length_or_global t))
  | Padding_inline -> Some (v Padding_inline (read_padding_logical_shorthand t))
  | Padding_inline_start ->
      Some (v Padding_inline_start (read_nn_length_or_global t))
  | Padding_inline_end ->
      Some (v Padding_inline_end (read_nn_length_or_global t))
  | Padding_block -> Some (v Padding_block (read_padding_logical_shorthand t))
  | Padding_block_start ->
      Some (v Padding_block_start (read_nn_length_or_global t))
  | Padding_block_end -> Some (v Padding_block_end (read_nn_length_or_global t))
  (* CSS Box 4 sec. 3.1 gives every margin longhand [<length-percentage> |
     auto], and CSS Logical 1 sec. 4.2 builds the flow-relative pair from that
     same production, so a sizing function reaches none of them. The shorthand
     already read its components this way. *)
  | Margin_left -> Some (v Margin_left (read_margin_length_or_global t))
  | Margin_right -> Some (v Margin_right (read_margin_length_or_global t))
  | Margin_top -> Some (v Margin_top (read_margin_length_or_global t))
  | Margin_bottom -> Some (v Margin_bottom (read_margin_length_or_global t))
  | Margin_inline ->
      Some
        (v Margin_inline
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_margin_length_or_global t))
  | Margin_inline_start ->
      Some (v Margin_inline_start (read_margin_length_or_global t))
  | Margin_inline_end ->
      Some (v Margin_inline_end (read_margin_length_or_global t))
  | Margin_block ->
      Some
        (v Margin_block
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_margin_length_or_global t))
  | Margin_block_start ->
      Some (v Margin_block_start (read_margin_length_or_global t))
  | Margin_block_end ->
      Some (v Margin_block_end (read_margin_length_or_global t))
  | _ -> None

let read_list_align_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | List_style_type -> Some (v List_style_type (read_list_style_type t))
  | List_style_position ->
      Some (v List_style_position (read_list_style_position t))
  | List_style_image -> Some (v List_style_image (read_list_style_image t))
  | List_style -> Some (v List_style (Properties.read_list_style t))
  | Order -> Some (v Order (Properties.read_order t))
  | Justify_items -> Some (v Justify_items (read_justify_items t))
  | Justify_self -> Some (v Justify_self (read_justify_self t))
  | Align_content -> Some (v Align_content (read_align_content t))
  | Align_self -> Some (v Align_self (read_align_self t))
  | Place_content -> Some (v Place_content (read_place_content t))
  | Place_items -> Some (v Place_items (read_place_items t))
  | Place_self -> Some (read_place_self_value t)
  | _ -> None

let read_position_value_prop : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Inset -> Some (v Inset (read_length_box t))
  | Inset_inline -> Some (v Inset_inline (read_inset_axis t))
  | Inset_inline_start -> Some (v Inset_inline_start (read_inset_longhand t))
  | Inset_inline_end -> Some (v Inset_inline_end (read_inset_longhand t))
  | Inset_block -> Some (v Inset_block (read_inset_axis t))
  | Inset_block_start -> Some (v Inset_block_start (read_inset_longhand t))
  | Inset_block_end -> Some (v Inset_block_end (read_inset_longhand t))
  | Top -> Some (v Top (read_inset_longhand t))
  | Right -> Some (v Right (read_inset_longhand t))
  | Bottom -> Some (v Bottom (read_inset_longhand t))
  | Left -> Some (v Left (read_inset_longhand t))
  | Outline -> Some (v Outline (read_outline t))
  | Outline_style -> Some (v Outline_style (read_outline_style t))
  | Outline_width -> Some (v Outline_width (read_border_width t))
  | Outline_offset -> Some (v Outline_offset (read_length_or_css_wide t))
  | Forced_color_adjust ->
      Some (v Forced_color_adjust (read_forced_color_adjust t))
  | Scroll_snap_type -> Some (v Scroll_snap_type (read_scroll_snap_type t))
  | Tab_size -> Some (v Tab_size (read_tab_size t))
  | Zoom -> Some (v Zoom (read_zoom t))
  | _ -> None

let read_vendor_font_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Webkit_text_size_adjust ->
      Some (v Webkit_text_size_adjust (read_text_size_adjust t))
  | Webkit_text_decoration ->
      Some (v Webkit_text_decoration (read_text_decoration t))
  | Webkit_appearance -> Some (v Webkit_appearance (read_webkit_appearance t))
  | Webkit_font_smoothing ->
      Some (v Webkit_font_smoothing (read_webkit_font_smoothing t))
  | Webkit_line_clamp -> Some (v Webkit_line_clamp (read_webkit_line_clamp t))
  | Webkit_box_orient -> Some (v Webkit_box_orient (read_webkit_box_orient t))
  | Moz_orient -> Some (v Moz_orient (read_moz_orient t))
  | Webkit_hyphens -> Some (v Webkit_hyphens (read_hyphens t))
  | Font_feature_settings ->
      Some (v Font_feature_settings (read_font_feature_settings t))
  | Font_variation_settings ->
      Some (v Font_variation_settings (read_font_variation_settings t))
  | Font_stretch -> Some (v Font_stretch (read_font_stretch t))
  | Font_optical_sizing ->
      Some (v Font_optical_sizing (read_font_optical_sizing t))
  | Font_kerning -> Some (v Font_kerning (read_font_kerning t))
  | Font_language_override ->
      Some (v Font_language_override (read_font_language_override t))
  | Font_synthesis_style ->
      Some (v Font_synthesis_style (read_font_synthesis_style t))
  | Font_synthesis_weight ->
      Some (v Font_synthesis_weight (read_font_synthesis_weight t))
  | Font_synthesis_small_caps ->
      Some (v Font_synthesis_small_caps (read_font_synthesis_small_caps t))
  | Font_synthesis_position ->
      Some (v Font_synthesis_position (read_font_synthesis_position t))
  | Font_variant_ligatures ->
      Some (v Font_variant_ligatures (read_font_variant_ligatures t))
  | Caps -> Some (v Caps (read_font_variant_caps t))
  | Numeric -> Some (v Numeric (read_font_variant_numeric t))
  | Font_variant_position ->
      Some (v Font_variant_position (read_font_variant_position t))
  | East_asian -> Some (v East_asian (read_font_variant_east_asian t))
  | _ -> None

let read_text_flow_value : type a. a property -> Cursor.t -> declaration option
    =
 fun prop t ->
  match prop with
  | Text_indent -> Some (v Text_indent (read_text_indent_value t))
  | Text_overflow -> Some (v Text_overflow (read_text_overflow t))
  | Text_wrap -> Some (v Text_wrap (read_text_wrap t))
  | Text_decoration_thickness ->
      Some (v Text_decoration_thickness (read_text_decoration_thickness t))
  | Text_size_adjust -> Some (v Text_size_adjust (read_text_size_adjust t))
  | Text_decoration_skip_ink ->
      Some (v Text_decoration_skip_ink (read_text_decoration_skip_ink t))
  | Text_decoration_skip ->
      Some (v Text_decoration_skip (read_text_decoration_skip t))
  | Text_decoration_skip_self ->
      Some (v Text_decoration_skip_self (read_text_decoration_skip_self t))
  | Text_decoration_skip_box ->
      Some (v Text_decoration_skip_box (read_text_decoration_skip_box t))
  | Text_decoration_skip_inset ->
      Some (v Text_decoration_skip_inset (read_text_decoration_skip_inset t))
  | Text_decoration_skip_spaces ->
      Some (v Text_decoration_skip_spaces (read_text_decoration_skip_spaces t))
  | Word_break -> Some (v Word_break (read_word_break t))
  | Overflow_wrap -> Some (v Overflow_wrap (read_overflow_wrap t))
  | Line_break -> Some (v Line_break (read_line_break t))
  | Hyphens -> Some (v Hyphens (read_hyphens t))
  | Word_spacing -> Some (v Word_spacing (read_word_spacing t))
  | _ -> None

let read_modern_layout_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Container_type -> Some (v Container_type (read_container_type t))
  | Container_name -> Some (v Container_name (read_container_name t))
  | Container -> Some (v Container (read_container_shorthand t))
  | Anchor_name -> Some (v Anchor_name (read_anchor_name t))
  | Position_anchor -> Some (v Position_anchor (read_position_anchor t))
  | Position_try_fallbacks ->
      Some (v Position_try_fallbacks (Properties.read_position_try_fallbacks t))
  | Position_try_order ->
      Some (v Position_try_order (read_position_try_order t))
  | Position_try -> Some (v Position_try (read_position_try t))
  | Position_visibility ->
      Some (v Position_visibility (read_position_visibility t))
  | Position_area -> Some (v Position_area (read_position_area t))
  | Shape_outside -> Some (v Shape_outside (read_shape_outside t))
  | Shape_image_threshold ->
      Some (v Shape_image_threshold (read_shape_image_threshold t))
  | Overflow_clip_margin ->
      Some (v Overflow_clip_margin (read_overflow_clip_margin t))
  | Overflow_anchor -> Some (v Overflow_anchor (read_overflow_anchor t))
  | Scrollbar_width -> Some (v Scrollbar_width (read_scrollbar_width t))
  | Scrollbar_color -> Some (v Scrollbar_color (read_scrollbar_color t))
  | Scrollbar_gutter -> Some (v Scrollbar_gutter (read_scrollbar_gutter t))
  | Font_palette -> Some (v Font_palette (read_font_palette t))
  | Font_synthesis -> Some (v Font_synthesis (read_font_synthesis t))
  | _ -> None

let read_inline_text_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Text_wrap_mode -> Some (v Text_wrap_mode (read_text_wrap_mode t))
  | Text_wrap_style -> Some (v Text_wrap_style (read_text_wrap_style t))
  | Text_box_trim -> Some (v Text_box_trim (read_text_box_trim t))
  | Text_underline_position ->
      Some (v Text_underline_position (read_text_underline_position t))
  | Text_box_edge -> Some (v Text_box_edge (read_text_box_edge t))
  | Text_box -> Some (v Text_box (read_text_box t))
  | Inline_sizing -> Some (v Inline_sizing (read_inline_sizing t))
  | Line_fit_edge -> Some (v Line_fit_edge (read_line_fit_edge t))
  | Interpolate_size -> Some (v Interpolate_size (read_interpolate_size t))
  | Min_intrinsic_sizing ->
      Some (v Min_intrinsic_sizing (read_min_intrinsic_sizing t))
  | Ruby_align -> Some (v Ruby_align (read_ruby_align t))
  | Ruby_merge -> Some (v Ruby_merge (read_ruby_merge t))
  | Ruby_overhang -> Some (v Ruby_overhang (read_ruby_overhang t))
  | Ruby_position -> Some (v Ruby_position (read_ruby_position t))
  | Glyph_orientation_vertical ->
      Some (v Glyph_orientation_vertical (read_glyph_orientation_vertical t))
  | _ -> None

let read_timeline_image_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Animation_timeline ->
      Some (v Animation_timeline (read_animation_timeline t))
  | Animation_range -> Some (v Animation_range (read_animation_range t))
  | Animation_range_start ->
      Some (v Animation_range_start (read_animation_range_item t))
  | Animation_range_end ->
      Some (v Animation_range_end (read_animation_range_item t))
  | Scroll_timeline -> Some (v Scroll_timeline (read_timeline_shorthand t))
  | Scroll_timeline_name -> Some (v Scroll_timeline_name (read_timeline_name t))
  | Scroll_timeline_axis -> Some (v Scroll_timeline_axis (read_timeline_axis t))
  | View_transition_name ->
      Some (v View_transition_name (read_view_transition_name t))
  | View_transition_class ->
      Some (v View_transition_class (read_view_transition_class t))
  | Image_orientation -> Some (v Image_orientation (read_image_orientation t))
  | Image_rendering -> Some (v Image_rendering (read_image_rendering t))
  | Image_resolution -> Some (v Image_resolution (read_image_resolution t))
  | Contain_intrinsic_size ->
      Some (v Contain_intrinsic_size (read_contain_intrinsic_size t))
  | Contain_intrinsic_width ->
      Some (v Contain_intrinsic_width (read_contain_intrinsic_longhand t))
  | Contain_intrinsic_height ->
      Some (v Contain_intrinsic_height (read_contain_intrinsic_longhand t))
  | Contain_intrinsic_block_size ->
      Some (v Contain_intrinsic_block_size (read_contain_intrinsic_longhand t))
  | Contain_intrinsic_inline_size ->
      Some (v Contain_intrinsic_inline_size (read_contain_intrinsic_longhand t))
  | _ -> None

let read_motion_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Margin_trim -> Some (v Margin_trim (read_margin_trim t))
  | Offset_path -> Some (v Offset_path (read_offset_path t))
  | Offset -> Some (v Offset (read_offset t))
  | Offset_anchor -> Some (v Offset_anchor (read_offset_anchor t))
  | Offset_position -> Some (v Offset_position (read_offset_position t))
  | Offset_rotate -> Some (v Offset_rotate (read_offset_rotate t))
  | Font_size_adjust -> Some (v Font_size_adjust (read_font_size_adjust t))
  | Font_variant_emoji ->
      Some (v Font_variant_emoji (read_font_variant_emoji t))
  | Text_spacing_trim -> Some (v Text_spacing_trim (read_text_spacing_trim t))
  | Hyphenate_limit_chars ->
      Some (v Hyphenate_limit_chars (read_hyphenate_limit_chars t))
  | Initial_letter -> Some (v Initial_letter (read_initial_letter t))
  | Initial_letter_align ->
      Some (v Initial_letter_align (read_initial_letter_align t))
  | Initial_letter_wrap ->
      Some (v Initial_letter_wrap (read_initial_letter_wrap t))
  | Dominant_baseline -> Some (v Dominant_baseline (read_dominant_baseline t))
  | View_timeline_name -> Some (v View_timeline_name (read_timeline_name t))
  | View_timeline_axis -> Some (v View_timeline_axis (read_timeline_axis t))
  | View_timeline_inset -> Some (v View_timeline_inset (read_timeline_inset t))
  | View_timeline -> Some (v View_timeline (read_view_timeline_shorthand t))
  | Timeline_scope -> Some (v Timeline_scope (read_timeline_scope t))
  | Perspective_origin ->
      Some (v Perspective_origin (read_perspective_origin t))
  | Transform_style -> Some (v Transform_style (read_transform_style t))
  | Backface_visibility ->
      Some (v Backface_visibility (read_backface_visibility t))
  | Rotate -> Some (v Rotate (read_rotate_value t))
  | Scale -> Some (v Scale (read_scale t))
  | _ -> None

let read_object_transition_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Object_position -> Some (v Object_position (read_position_value t))
  | Object_fit -> Some (v Object_fit (read_object_fit t))
  | Object_view_box -> Some (v Object_view_box (read_object_view_box t))
  | Transition_duration ->
      Some (v Transition_duration (read_duration_list read_duration t))
  | Transition_timing_function ->
      Some (v Transition_timing_function (read_timing_function_list t))
  | Transition_delay ->
      Some (v Transition_delay (read_duration_list read_time t))
  | Transition_property ->
      Some (v Transition_property (read_transition_property t))
  | Transition_behavior ->
      Some (v Transition_behavior (Properties.read_transition_behavior_list t))
  | Overlay -> Some (v Overlay (read_overlay t))
  | Will_change -> Some (v Will_change (read_will_change t))
  | Contain -> Some (v Contain (read_contain t))
  | Isolation -> Some (v Isolation (read_isolation t))
  | Break_before -> Some (v Break_before (read_break_value t))
  | Break_after -> Some (v Break_after (read_break_value t))
  | Break_inside -> Some (v Break_inside (read_break_inside_value t))
  | Page_break_before -> Some (v Page_break_before (read_page_break_value t))
  | Page_break_after -> Some (v Page_break_after (read_page_break_value t))
  | Page_break_inside ->
      Some (v Page_break_inside (read_page_break_inside_value t))
  | Page_size -> Some (v Page_size (read_page_size t))
  | Columns -> Some (v Columns (read_columns_value t))
  | Column_width -> Some (v Column_width (read_column_width t))
  | Column_height -> Some (v Column_height (read_column_height t))
  | Column_wrap -> Some (v Column_wrap (read_column_wrap t))
  | Column_count -> Some (v Column_count (read_column_count t))
  (* CSS Gaps 1 sec. 4.4 spells [column-rule] as a [<gap-rule>#], one entry per
     rule line, the same list its longhands carry. *)
  | Column_rule ->
      Some
        (v Column_rule
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_border t))
  (* CSS Gaps 1 sec. 4 gives each gap decoration longhand a comma-separated
     list, one entry per rule line. *)
  | Column_rule_color ->
      Some
        (v Column_rule_color
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_color t))
  | Column_rule_width ->
      Some
        (v Column_rule_width
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_border_width t))
  | Column_rule_style ->
      Some
        (v Column_rule_style
           (Cursor.list ~sep:Cursor.comma ~at_least:1 read_border_style t))
  | Column_span -> Some (v Column_span (read_column_span t))
  | _ -> None

let read_border_clip_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Border_top -> Some (v Border_top (read_border t))
  | Border_right -> Some (v Border_right (read_border t))
  | Border_bottom -> Some (v Border_bottom (read_border t))
  | Border_left -> Some (v Border_left (read_border t))
  | Border_block -> Some (v Border_block (read_border t))
  | Border_block_start -> Some (v Border_block_start (read_border t))
  | Border_block_end -> Some (v Border_block_end (read_border t))
  | Border_inline -> Some (v Border_inline (read_border t))
  | Border_inline_start -> Some (v Border_inline_start (read_border t))
  | Border_inline_end -> Some (v Border_inline_end (read_border t))
  | Border_spacing -> Some (v Border_spacing (read_border_spacing t))
  | Border_image -> Some (v Border_image (read_border_image t))
  | Border_collapse -> Some (v Border_collapse (read_border_collapse t))
  | Clip_path -> Some (v Clip_path (read_clip_path t))
  | Mask -> Some (v Mask (read_mask t))
  | Clip -> Some (v Clip (read_clip t))
  | Moz_osx_font_smoothing ->
      Some (v Moz_osx_font_smoothing (read_moz_osx_font_smoothing t))
  | Backdrop_filter -> Some (v Backdrop_filter (read_filter t))
  | Webkit_backdrop_filter -> Some (v Webkit_backdrop_filter (read_filter t))
  | _ -> None

let read_scroll_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Scroll_snap_align -> Some (v Scroll_snap_align (read_scroll_snap_align t))
  | Scroll_snap_stop -> Some (v Scroll_snap_stop (read_scroll_snap_stop t))
  | Scroll_behavior -> Some (v Scroll_behavior (read_scroll_behavior t))
  | Scroll_margin ->
      Some
        (v Scroll_margin
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
              read_scroll_margin_length t))
  | Scroll_margin_top ->
      Some (v Scroll_margin_top (read_scroll_margin_length t))
  | Scroll_margin_right ->
      Some (v Scroll_margin_right (read_scroll_margin_length t))
  | Scroll_margin_bottom ->
      Some (v Scroll_margin_bottom (read_scroll_margin_length t))
  | Scroll_margin_left ->
      Some (v Scroll_margin_left (read_scroll_margin_length t))
  | Scroll_margin_inline ->
      Some
        (v Scroll_margin_inline
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_margin_length t))
  | Scroll_margin_inline_start ->
      Some (v Scroll_margin_inline_start (read_scroll_margin_length t))
  | Scroll_margin_inline_end ->
      Some (v Scroll_margin_inline_end (read_scroll_margin_length t))
  | Scroll_margin_block ->
      Some
        (v Scroll_margin_block
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_margin_length t))
  | Scroll_margin_block_start ->
      Some (v Scroll_margin_block_start (read_scroll_margin_length t))
  | Scroll_margin_block_end ->
      Some (v Scroll_margin_block_end (read_scroll_margin_length t))
  | Scroll_padding ->
      Some
        (v Scroll_padding
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
              read_scroll_padding_length t))
  | Scroll_padding_top ->
      Some (v Scroll_padding_top (read_scroll_padding_length t))
  | Scroll_padding_right ->
      Some (v Scroll_padding_right (read_scroll_padding_length t))
  | Scroll_padding_bottom ->
      Some (v Scroll_padding_bottom (read_scroll_padding_length t))
  | Scroll_padding_left ->
      Some (v Scroll_padding_left (read_scroll_padding_length t))
  | Scroll_padding_inline ->
      Some
        (v Scroll_padding_inline
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_padding_length t))
  | Scroll_padding_inline_start ->
      Some (v Scroll_padding_inline_start (read_scroll_padding_length t))
  | Scroll_padding_inline_end ->
      Some (v Scroll_padding_inline_end (read_scroll_padding_length t))
  | Scroll_padding_block ->
      Some
        (v Scroll_padding_block
           (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
              read_scroll_padding_length t))
  | Scroll_padding_block_start ->
      Some (v Scroll_padding_block_start (read_scroll_padding_length t))
  | Scroll_padding_block_end ->
      Some (v Scroll_padding_block_end (read_scroll_padding_length t))
  | Overscroll_behavior ->
      let xs =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_overscroll_behavior t
      in
      Some (v Overscroll_behavior xs)
  | Overscroll_behavior_x ->
      Some (v Overscroll_behavior_x (read_overscroll_behavior t))
  | Overscroll_behavior_y ->
      Some (v Overscroll_behavior_y (read_overscroll_behavior t))
  | Overscroll_behavior_block ->
      Some (v Overscroll_behavior_block (read_overscroll_behavior t))
  | Overscroll_behavior_inline ->
      Some (v Overscroll_behavior_inline (read_overscroll_behavior t))
  | _ -> None

let read_interaction_value : type a.
    a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | User_select -> Some (v User_select (read_user_select t))
  | Webkit_user_select -> Some (v Webkit_user_select (read_user_select t))
  | Ms_user_select -> Some (v Ms_user_select (read_user_select t))
  | Webkit_text_fill_color -> Some (v Webkit_text_fill_color (read_color t))
  | Webkit_text_stroke_color -> Some (v Webkit_text_stroke_color (read_color t))
  | Webkit_text_stroke_width ->
      Some (v Webkit_text_stroke_width (read_border_width t))
  | Webkit_text_stroke ->
      Some (v Webkit_text_stroke (read_webkit_text_stroke t))
  | Moz_user_select -> Some (v Moz_user_select (read_user_select t))
  | Pointer_events -> Some (v Pointer_events (read_pointer_events t))
  | Resize -> Some (v Resize (read_resize t))
  | Transition -> Some (v Transition (read_transitions t))
  | Animation -> Some (v Animation (read_animations t))
  | Quotes -> Some (v Quotes (read_quotes t))
  | Touch_action -> Some (v Touch_action (read_touch_action t))
  | Clear -> Some (v Clear (read_clear t))
  | Float -> Some (v Float (read_float_side t))
  | Fill -> Some (v Fill (read_svg_paint t))
  | Stroke -> Some (v Stroke (read_svg_paint t))
  | Direction -> Some (v Direction (read_direction t))
  | Fill_rule -> Some (v Fill_rule (Properties.read_fill_rule t))
  | Clip_rule -> Some (v Clip_rule (Properties.read_fill_rule t))
  | Stroke_linecap -> Some (v Stroke_linecap (Properties.read_stroke_linecap t))
  | Stroke_linejoin ->
      Some (v Stroke_linejoin (Properties.read_stroke_linejoin t))
  | Stroke_miterlimit ->
      Some (v Stroke_miterlimit (Properties.read_stroke_miterlimit t))
  | Stroke_width -> Some (v Stroke_width (Properties.read_stroke_width t))
  | Stroke_dashoffset ->
      Some (v Stroke_dashoffset (Properties.read_stroke_dashoffset t))
  | Stroke_dasharray ->
      Some (v Stroke_dasharray (Properties.read_stroke_dasharray t))
  | Paint_order -> Some (v Paint_order (Properties.read_paint_order t))
  | Vector_effect -> Some (v Vector_effect (Properties.read_vector_effect t))
  | Unicode_bidi -> Some (v Unicode_bidi (read_unicode_bidi t))
  | Writing_mode -> Some (v Writing_mode (read_writing_mode t))
  | Text_combine_upright ->
      Some (v Text_combine_upright (read_text_combine_upright t))
  | Animation_name -> Some (v Animation_name (read_animation_name t))
  | Animation_duration ->
      Some (v Animation_duration (read_duration_list read_animation_duration t))
  | Animation_timing_function ->
      Some (v Animation_timing_function (read_timing_function_list t))
  | Animation_delay -> Some (v Animation_delay (read_duration_list read_time t))
  | Animation_iteration_count ->
      Some (v Animation_iteration_count (read_animation_iteration_count t))
  | Animation_direction ->
      Some (v Animation_direction (read_animation_direction t))
  | Animation_fill_mode ->
      Some (v Animation_fill_mode (read_animation_fill_mode t))
  | Animation_play_state ->
      Some (v Animation_play_state (read_animation_play_state t))
  | Animation_composition ->
      Some (v Animation_composition (read_animation_composition t))
  | Mix_blend_mode -> Some (v Mix_blend_mode (read_blend_mode t))
  | Table_layout -> Some (v Table_layout (read_table_layout t))
  | Print_color_adjust ->
      Some (v Print_color_adjust (read_print_color_adjust t))
  | Webkit_print_color_adjust ->
      Some (v Webkit_print_color_adjust (read_print_color_adjust t))
  | Box_decoration_break ->
      Some (v Box_decoration_break (read_box_decoration_break t))
  | Webkit_box_decoration_break ->
      Some (v Webkit_box_decoration_break (read_box_decoration_break t))
  | _ -> None

let read_mask_value : type a. a property -> Cursor.t -> declaration option =
 fun prop t ->
  match prop with
  | Webkit_mask_image -> Some (v Webkit_mask_image (read_background_image t))
  | Webkit_mask_composite ->
      Some (v Webkit_mask_composite (read_webkit_mask_composite t))
  | Webkit_mask_source_type ->
      Some (v Webkit_mask_source_type (read_webkit_mask_source_type t))
  | Webkit_mask_size -> Some (v Webkit_mask_size (read_background_size_list t))
  | Webkit_mask_position ->
      Some (v Webkit_mask_position (read_background_position t))
  | Webkit_mask_repeat ->
      Some (v Webkit_mask_repeat (read_background_repeat_list t))
  | Webkit_mask_clip ->
      Some (v Webkit_mask_clip (read_webkit_mask_box_list ~clip:true t))
  | Webkit_mask_origin ->
      Some (v Webkit_mask_origin (read_webkit_mask_box_list t))
  | Border_image_source ->
      Some (v Border_image_source (read_border_image_source t))
  | Border_image_slice ->
      Some (v Border_image_slice (read_border_image_slice t))
  | Border_image_repeat ->
      Some (v Border_image_repeat (read_border_image_repeat t))
  | Border_image_width ->
      Some (v Border_image_width (read_border_image_width t))
  | Border_image_outset ->
      Some (v Border_image_outset (read_border_image_outset t))
  | Mask_image -> Some (v Mask_image (read_background_image t))
  | Mask_composite -> Some (v Mask_composite (read_mask_composite_list t))
  | Mask_mode -> Some (v Mask_mode (read_mask_mode t))
  | Mask_border -> Some (v Mask_border (read_mask_border t))
  | Mask_size -> Some (v Mask_size (read_background_size_list t))
  | Mask_position -> Some (v Mask_position (read_background_position t))
  | Mask_repeat -> Some (v Mask_repeat (read_background_repeat_list t))
  | Mask_clip -> Some (v Mask_clip (read_mask_box_list ~clip:true t))
  | Mask_origin -> Some (v Mask_origin (read_mask_box_list t))
  | Mask_type -> Some (v Mask_type (read_mask_type t))
  | All -> Some (v All (Properties.read_css_wide t))
  | _ -> None

let read_value_readers =
  [
    { read_value_opt = read_color_value };
    { read_value_opt = read_sizing_value };
    { read_value_opt = read_radius_gap_value };
    { read_value_opt = read_layout_value };
    { read_value_opt = read_box_edge_value };
    { read_value_opt = read_type_value };
    { read_value_opt = read_flex_effect_value };
    { read_value_opt = read_vendor_alias_value };
    { read_value_opt = read_background_value };
    { read_value_opt = read_grid_value };
    { read_value_opt = read_shadow_content_value };
    { read_value_opt = read_spacing_value };
    { read_value_opt = read_list_align_value };
    { read_value_opt = read_position_value_prop };
    { read_value_opt = read_vendor_font_value };
    { read_value_opt = read_text_flow_value };
    { read_value_opt = read_modern_layout_value };
    { read_value_opt = read_inline_text_value };
    { read_value_opt = read_timeline_image_value };
    { read_value_opt = read_motion_value };
    { read_value_opt = read_object_transition_value };
    { read_value_opt = read_border_clip_value };
    { read_value_opt = read_scroll_value };
    { read_value_opt = read_interaction_value };
    { read_value_opt = read_mask_value };
  ]

let rec read_value_from : type a.
    value_reader list -> a property -> Cursor.t -> declaration =
 fun readers prop t ->
  match readers with
  | [] ->
      Cursor.err_invalid t
        ("unsupported property reader: " ^ canonical_prop_name prop)
  | reader :: rest -> (
      match reader.read_value_opt prop t with
      | Some decl -> decl
      | None -> read_value_from rest prop t)

let read_value (type a) (prop : a property) t : declaration =
  Cursor.with_context t (canonical_prop_name prop) @@ fun () ->
  match prop with
  | Custom_property name ->
      Cursor.err_invalid t
        ("custom property read through regular property: " ^ name)
  | Unknown_property name ->
      Cursor.err_invalid t
        ("unknown property read through typed parser: " ^ name)
  | _ -> read_value_from read_value_readers prop t
