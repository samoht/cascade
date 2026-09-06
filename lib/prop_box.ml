(* css-display-4, css-box-4, css-sizing-4, css-overflow-5, css-position-3 and
   css-shapes-1: display, position, the overflow family (including
   overflow-clip-margin and its clip box), intrinsic sizing, aspect-ratio,
   object-fit / object-view-box, margin-trim, opacity, shape-image-threshold,
   z-index, order, zoom, float / clear, isolation, table-layout and
   caption-side.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_background
open Prop_mask

(* CSS Display 3 sec. 2.1 [display-outside]: pre-existing aliases inside the
   single-value vocabulary that compose with a [display-inside] in the two-value
   form. The composite [<outside> <inside>] is a [Multi]. *)
(* CSS Display 3 (ED) sec. 2: [<display-outside> = block | inline | run-in].
   [list-item] belongs to [<display-listitem>], which spells it out separately,
   so it is not one of these. *)
let display_outside_idents : (string * display) list =
  [ ("block", Block); ("inline", Inline); ("run-in", Run_in) ]

let display_inside_idents : (string * display) list =
  [
    ("flow", Block);
    ("flow-root", Flow_root);
    ("table", Table);
    ("flex", Flex);
    ("grid", Grid);
    ("ruby", Ruby);
  ]

let list_item_inside_idents : (string * display) list =
  [ ("flow", Block); ("flow-root", Flow_root) ]

let read_display_legacy t : display =
  Cursor.enum "display"
    [
      ("none", (None : display));
      ("block", Block);
      ("inline", Inline);
      ("inline-block", Inline_block);
      ("flex", Flex);
      ("inline-flex", Inline_flex);
      ("grid", Grid);
      ("inline-grid", Inline_grid);
      (* CSS Grid 3 (ED) sec. 2.2 adds these two to [display] and to neither
         [<display-inside>] nor [<display-outside>], so they are whole values
         and pair with nothing. *)
      ("grid-lanes", Grid_lanes);
      ("inline-grid-lanes", Inline_grid_lanes);
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
      ("run-in", Run_in);
      ("ruby", Ruby);
      ("ruby-base", Ruby_base);
      ("ruby-text", Ruby_text);
      ("ruby-base-container", Ruby_base_container);
      ("ruby-text-container", Ruby_text_container);
      ("math", Math);
      ("-webkit-flex", Webkit_flex);
      ("-webkit-inline-flex", Webkit_inline_flex);
      ("-ms-flexbox", Ms_flexbox);
      ("-webkit-box", Webkit_box);
      ("-moz-box", Moz_box);
      ("-moz-inline-box", Moz_inline_box);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    t

let read_display_two_value t : display =
  (* CSS Display 3 sec. 2.1 two-value form [<display-outside> <display-inside>].
     Both keywords must come from their respective vocabularies; otherwise
     reject so the caller can fall back to the legacy single-value form. *)
  let outside = Cursor.enum "display-outside" display_outside_idents t in
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some s when List.mem_assoc s display_inside_idents ->
      let inside = Cursor.enum "display-inside" display_inside_idents t in
      Multi (outside, inside)
  | _ -> Cursor.err_expected t "<display-inside>"

let read_display_list_item t : display =
  let outside = ref Option.None in
  let inside = ref Option.None in
  let list_item = ref false in
  let consume_slot () =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "list-item" when not !list_item ->
        ignore (Cursor.ident t : string);
        list_item := true;
        true
    | Some s ->
        (* The three components are combined with [&&], so each keyword fills
           whichever slot it belongs to wherever it appears; testing the outside
           slot alone would stop the loop at a leading inside keyword. *)
        let fill slot idents =
          match (!slot, List.assoc_opt s idents) with
          | Option.None, Some value ->
              ignore (Cursor.ident t : string);
              slot := Option.Some value;
              true
          | _ -> false
        in
        fill outside display_outside_idents
        || fill inside list_item_inside_idents
    | Option.None -> false
  in
  while consume_slot () do
    ()
  done;
  if not !list_item then Cursor.err_expected t "list-item";
  if Option.is_none !outside && Option.is_none !inside then
    Cursor.err_expected t "list-item with outside or inside display";
  let outside = Option.value !outside ~default:Block in
  let inside = Option.value !inside ~default:Block in
  Multi (Multi (outside, inside), List_item)

let rec read_display t : display =
  let read_var t : display = Var (read_var read_display t) in
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      read_var t
  | _ -> (
      match Cursor.option read_display_list_item t with
      | Some d -> d
      | None -> (
          match Cursor.option read_display_two_value t with
          | Some d -> d
          | None -> read_display_legacy t))

let rec read_position t : position =
  Cursor.enum_or_var "position"
    [
      ("static", (Static : position));
      ("relative", Relative);
      ("absolute", Absolute);
      ("fixed", Fixed);
      ("sticky", Sticky);
      ("-webkit-sticky", Webkit_sticky);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_position t))
    t

let rec read_overflow_single (t : Cursor.t) : overflow =
  Cursor.enum_or_var "overflow"
    [
      ("visible", (Visible : overflow));
      ("hidden", Hidden);
      ("scroll", Scroll);
      ("auto", Auto);
      ("clip", Clip);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_overflow_single t))
    t

let read_overflow t : overflow =
  let first = read_overflow_single t in
  Cursor.ws t;
  if Cursor.is_done t then first
  else
    let second = read_overflow_single t in
    Cursor.ws t;
    Cursor.expect_eof t;
    Overflow_pair (first, second)

(* [object-view-box] has its own [Inset] / [Xywh] / [Rect] variants distinct
   from [clip-path]'s. Same principle as the clip-path normalisers: the slot
   values are plain [<length>] / [<length-percentage>] positions, not [calc()]
   operands, so the zero-unit drop ([0px] -> [0]) applies (CSS Values L4 sec.
   6.1.1). *)
let normalize_object_view_box (value : object_view_box) : object_view_box =
  match value with
  | Inset (top, right, bottom, left) ->
      let nl = Values.normalize_length ~strip:true in
      let top' = nl top in
      let right' = option_map_preserve nl right in
      let bottom' = option_map_preserve nl bottom in
      let left' = option_map_preserve nl left in
      if top' == top && right' == right && bottom' == bottom && left' == left
      then value
      else Inset (top', right', bottom', left')
  | Xywh r ->
      let lp = Values.normalize_length_percentage ~strip:true in
      let x = lp r.x in
      let y = lp r.y in
      let width = lp r.width in
      let height = lp r.height in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        x == r.x && y == r.y && width == r.width && height == r.height
        && rounded == r.rounded
      then value
      else Xywh { x; y; width; height; rounded }
  | Rect r ->
      let lp = Values.normalize_length_percentage ~strip:true in
      let top = lp r.top in
      let right = lp r.right in
      let bottom = lp r.bottom in
      let left = lp r.left in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        top == r.top && right == r.right && bottom == r.bottom && left == r.left
        && rounded == r.rounded
      then value
      else Rect { top; right; bottom; left; rounded }
  | other -> other

let aspect_ratio_of_numbers ~auto (a : number) (b : number) : aspect_ratio =
  match (auto, a, b) with
  | false, Num a, Num b -> Ratio (a, b)
  | true, Num a, Num b -> Auto_ratio (a, b)
  | false, a, b -> Ratio_calc (a, b)
  | true, a, b -> Auto_ratio_calc (a, b)

let normalize_aspect_ratio : aspect_ratio -> aspect_ratio =
 fun value ->
  match value with
  | Auto_ratio_calc (a, b) ->
      aspect_ratio_of_numbers ~auto:true
        (Values.normalize_number a)
        (Values.normalize_number b)
  | Ratio_calc (a, b) ->
      aspect_ratio_of_numbers ~auto:false
        (Values.normalize_number a)
        (Values.normalize_number b)
  | other -> other

let rec pp_display : display Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inline_block -> Pp.string ctx "inline-block"
  | Flex -> Pp.string ctx "flex"
  | Inline_flex -> Pp.string ctx "inline-flex"
  | Grid -> Pp.string ctx "grid"
  | Inline_grid -> Pp.string ctx "inline-grid"
  | Grid_lanes -> Pp.string ctx "grid-lanes"
  | Inline_grid_lanes -> Pp.string ctx "inline-grid-lanes"
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
  | Run_in -> Pp.string ctx "run-in"
  | Ruby -> Pp.string ctx "ruby"
  | Ruby_base -> Pp.string ctx "ruby-base"
  | Ruby_text -> Pp.string ctx "ruby-text"
  | Ruby_base_container -> Pp.string ctx "ruby-base-container"
  | Ruby_text_container -> Pp.string ctx "ruby-text-container"
  | Math -> Pp.string ctx "math"
  | Webkit_flex -> Pp.string ctx "-webkit-flex"
  | Webkit_inline_flex -> Pp.string ctx "-webkit-inline-flex"
  | Ms_flexbox -> Pp.string ctx "-ms-flexbox"
  | Webkit_box -> Pp.string ctx "-webkit-box"
  | Moz_box -> Pp.string ctx "-moz-box"
  | Moz_inline_box -> Pp.string ctx "-moz-inline-box"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_display ctx v
  (* CSS Display 3 (ED) sec. 2.3: a [list-item] written with no inside display
     type takes [flow], so leaving the [flow] out names the same value. *)
  | Multi (Multi (outside, Block), List_item) when Pp.minified ctx ->
      pp_display ctx outside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  (* sec. 2.3 defaults the unwritten outside to [block] the same way, so with an
     inside written the [block] can go. One of the two has to stay: with both
     left out the value is the bare [list-item] keyword, which is a different
     node for the optimizer to fold to. *)
  | Multi (Multi (Block, inside), List_item) when Pp.minified ctx ->
      pp_display_inside ctx inside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  | Multi (Multi (outside, inside), List_item) ->
      pp_display ctx outside;
      Pp.space ctx ();
      pp_display_inside ctx inside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  | Multi (outside, inside) ->
      pp_display ctx outside;
      Pp.space ctx ();
      pp_display_inside ctx inside

and pp_display_inside ctx = function
  | Block -> Pp.string ctx "flow"
  | Flow_root -> Pp.string ctx "flow-root"
  | display -> pp_display ctx display

(* CSS Display 3 (ED) sec. 2.1: a [<display-outside>] written without an inside
   defaults the inner type to [flow]; sec. 2.2: an inside written alone defaults
   the outer type to [block], "except for ruby, which defaults to inline"; sec.
   2.3: a [list-item] with neither slot filled is [block flow list-item]; sec.
   2.6 names the four precomposed inline-level keywords. Each pair below is one
   value under two spellings, and the single keyword is the shorter one. That
   ruby exception is why [block ruby] has no entry: the bare [ruby] keyword
   names [inline ruby], so the two are different values. *)
let normalize_display : display -> display = function
  | Multi (Multi (Block, Block), List_item) -> List_item
  | Multi (Block, Block) -> Block
  | Multi (Inline, Block) -> Inline
  | Multi (Run_in, Block) -> Run_in
  | Multi (Block, Flow_root) -> Flow_root
  | Multi (Inline, Flow_root) -> Inline_block
  | Multi (Block, Flex) -> Flex
  | Multi (Inline, Flex) -> Inline_flex
  | Multi (Block, Grid) -> Grid
  | Multi (Inline, Grid) -> Inline_grid
  | Multi (Block, Table) -> Table
  | Multi (Inline, Table) -> Inline_table
  | Multi (Inline, Ruby) -> Ruby
  | value -> value

let rec pp_position : position Pp.t =
 fun ctx -> function
  | Static -> Pp.string ctx "static"
  | Relative -> Pp.string ctx "relative"
  | Absolute -> Pp.string ctx "absolute"
  | Fixed -> Pp.string ctx "fixed"
  | Sticky -> Pp.string ctx "sticky"
  | Webkit_sticky -> Pp.string ctx "-webkit-sticky"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_position ctx v

let rec pp_visibility : visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Collapse -> Pp.string ctx "collapse"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_visibility ctx v

let rec pp_z_index : z_index Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Index i -> Pp.int ctx i
  | Calc c -> pp_calc pp_z_index ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_z_index ctx v

let rec pp_zoom : zoom Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Reset -> Pp.string ctx "reset"
  | Num n -> Pp.float ctx n
  | Pct p -> Pp.pct ctx p
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_zoom ctx v

(* Opacity as a float (0.0-1.0). Tailwind's minifier writes percentages as
   decimals (50% -> .5), so minified output emits decimals to match. *)
let rec pp_opacity : opacity Pp.t =
 fun ctx -> function
  | Opacity_number f -> Pp.float ctx f
  | Calc c -> pp_calc pp_opacity ctx c
  | Abs v -> Pp.call "abs" pp_opacity ctx v
  | Sign v -> Pp.call "sign" pp_opacity ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_opacity ctx v

(* Inside an [opacity] [calc()], a raw [<number>] / [<percentage>] is modelled
   at the calc level as the [Num x] node rather than [Val (Opacity_number x)].
   The [_dim_only] reader excludes the unitless alternative so
   [read_calc_factor] falls through to its own [Num] path, matching the
   [<number-percentage>] convention. *)
let rec read_opacity_dim_only t : opacity =
  Cursor.ws t;
  Cursor.one_of
    [
      (* A [<percentage>] operand is the number it denotes (50% = 0.5), matching
         how a bare opacity percentage parses. A raw [<number>] is excluded so
         [read_calc] falls through to its own [Num] path. *)
      (fun t -> (Opacity_number (Cursor.pct t /. 100.) : opacity));
      (fun t ->
        Cursor.enum_or_calls "opacity"
          [
            ("inherit", (Inherit : opacity));
            ("initial", Initial);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:[ ("var", fun t -> Var (read_var read_opacity_dim_only t)) ]
          ~default:(fun t ->
            Cursor.err_expected t "opacity (var/calc inside calc)")
          t);
    ]
    t

let rec read_opacity t : opacity =
  let read_var t : opacity = Var (read_var read_opacity t) in
  let read_numeric_math t : opacity =
    Opacity_number (Values.read_numeric_expression t)
  in
  let read_number_or_percentage t =
    let n, unit = Cursor.number_with_unit t in
    let value =
      match unit with
      | Some "%" -> n /. 100.
      | Some unit -> Cursor.err_invalid t ("opacity unit: " ^ unit)
      | None -> n
    in
    Opacity_number value
  in
  Cursor.enum_or_calls "opacity"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", read_var);
        ( "calc",
          fun t ->
            Calc
              (Values.read_calc ~result_type:`Number_or_value
                 read_opacity_dim_only t) );
        ("min", read_numeric_math);
        ("max", read_numeric_math);
        ("clamp", read_numeric_math);
        ( "abs",
          fun t ->
            Cursor.call "abs" t (fun inner ->
                (Abs (read_opacity inner) : opacity)) );
        ( "sign",
          fun t ->
            Cursor.call "sign" t (fun inner ->
                (Sign (read_opacity inner) : opacity)) );
      ]
    ~default:read_number_or_percentage t

let rec pp_shape_image_threshold : shape_image_threshold Pp.t =
 fun ctx -> function
  | Number n -> Pp.float ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_shape_image_threshold ctx v

let rec read_shape_image_threshold t : shape_image_threshold =
  let read_var t : shape_image_threshold =
    Var (read_var read_shape_image_threshold t)
  in
  (* CSS Shapes 1 sec. 6.2 takes an [<opacity-value>], which CSS Color 4 spells
     [<number> | <percentage>], and computes it "clamped to the range [0,1]":
     the clamp is at computed-value time, so a value outside it still reads. *)
  let read_number t : shape_image_threshold =
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> Number (n /. 100.)
    | Some unit -> Cursor.err_invalid t ("shape-image-threshold unit: " ^ unit)
    | None -> Number n
  in
  Cursor.enum_or_calls "shape-image-threshold"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_number t

let rec pp_overflow : overflow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overflow ctx v
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Scroll -> Pp.string ctx "scroll"
  | Auto -> Pp.string ctx "auto"
  | Clip -> Pp.string ctx "clip"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Overflow_pair (x, y) ->
      pp_overflow ctx x;
      Pp.space ctx ();
      pp_overflow ctx y

(* CSS Overflow 3 (ED) sec. 3.1: [overflow] is [<'overflow-block'>{1,2}] and
   "sets the specified values of overflow-x and overflow-y in that order. If the
   second value is omitted, it is copied from the first." Repeating an axis
   therefore says what the single value says. *)
let normalize_overflow : overflow -> overflow = function
  | Overflow_pair (x, y) when equal_overflow x y -> x
  | value -> value

let pp_overflow_clip_box : overflow_clip_box Pp.t =
 fun ctx -> function
  | Content_box -> Pp.string ctx "content-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Border_box -> Pp.string ctx "border-box"

let rec pp_overflow_clip_margin : overflow_clip_margin Pp.t =
 fun ctx -> function
  | Clip_margin (Some box, Some length) ->
      pp_overflow_clip_box ctx box;
      Pp.space ctx ();
      pp_length ~always:true ctx length
  | Clip_margin (Some box, None) -> pp_overflow_clip_box ctx box
  | Clip_margin (None, Some length) -> pp_length ~always:true ctx length
  | Clip_margin (None, None) -> ()
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_overflow_clip_margin ctx v

let read_overflow_clip_box t : overflow_clip_box =
  Cursor.enum "overflow-clip-margin box"
    [
      ("content-box", (Content_box : overflow_clip_box));
      ("padding-box", Padding_box);
      ("border-box", Border_box);
    ]
    t

let read_overflow_clip_box_item (box : overflow_clip_box option ref) t =
  Cursor.ws t;
  let snap = Cursor.save t in
  match !box with
  | None -> (
      match read_overflow_clip_box t with
      | value ->
          box := Some value;
          true
      | exception Error.Parse_error _ ->
          Cursor.restore t snap;
          false)
  | Some _ -> false

(* CSS Overflow 4 sec. 3.2 spells [overflow-clip-margin] as [<visual-box> ||
   <length>], with no range on the length, so a negative one pulls the clip edge
   inside the box. *)
let read_overflow_clip_length_item (length : length option ref) t =
  Cursor.ws t;
  match !length with
  | None ->
      length :=
        Some
          (read_length ~allow_negative:true ~with_keywords:false
             ~length_only:true t);
      true
  | Some _ -> false

let rec read_overflow_clip_margin_items box length consumed t =
  Cursor.ws t;
  if Cursor.is_done t then consumed
  else
    let snap = Cursor.save t in
    if
      read_overflow_clip_box_item box t
      || read_overflow_clip_length_item length t
    then read_overflow_clip_margin_items box length true t
    else (
      Cursor.restore t snap;
      consumed)

let rec read_overflow_clip_margin t : overflow_clip_margin =
  let read_var t : overflow_clip_margin =
    Var (read_var read_overflow_clip_margin t)
  in
  let read_value t =
    let box : overflow_clip_box option ref =
      ref (None : overflow_clip_box option)
    in
    let length : length option ref = ref (None : length option) in
    if not (read_overflow_clip_margin_items box length false t) then
      Cursor.err_expected t "overflow-clip-margin";
    Clip_margin (!box, !length)
  in
  Cursor.enum_or_calls "overflow-clip-margin"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_value t

let rec pp_inline_sizing : inline_sizing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_inline_sizing ctx v
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_interpolate_size : interpolate_size Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_interpolate_size ctx v
  | Numeric_only -> Pp.string ctx "numeric-only"
  | Allow_keywords -> Pp.string ctx "allow-keywords"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_min_intrinsic_sizing_keyword : min_intrinsic_sizing_keyword Pp.t =
 fun ctx -> function
  | Legacy -> Pp.string ctx "legacy"
  | Zero_if_scroll -> Pp.string ctx "zero-if-scroll"
  | Zero_if_extrinsic -> Pp.string ctx "zero-if-extrinsic"

let rec pp_min_intrinsic_sizing : min_intrinsic_sizing Pp.t =
 fun ctx -> function
  | Sizing keywords ->
      Pp.list ~sep:Pp.space pp_min_intrinsic_sizing_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_min_intrinsic_sizing ctx v

let rec pp_table_layout : table_layout Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_table_layout ctx v
  | Auto -> Pp.string ctx "auto"
  | Fixed -> Pp.string ctx "fixed"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_aspect_ratio_number ctx value =
  match (Pp.minified ctx, eval_number_value value) with
  | true, Some value -> Pp.float ctx value
  | _ -> pp_number ctx value

let pp_aspect_ratio_pair ctx a b =
  let b_value = eval_number_value b in
  if Pp.minified ctx && b_value = Some 1. then pp_aspect_ratio_number ctx a
  else (
    pp_aspect_ratio_number ctx a;
    if b_value <> Some 1. then (
      Pp.op_char ctx '/';
      pp_aspect_ratio_number ctx b))

let rec pp_aspect_ratio : aspect_ratio Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Auto_ratio (a, b) ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      pp_aspect_ratio ctx (Ratio (a, b))
  | Auto_ratio_calc (a, b) ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      pp_aspect_ratio_pair ctx a b
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_aspect_ratio ctx v
  | Ratio_calc (a, b) -> pp_aspect_ratio_pair ctx a b
  | Ratio (a, b) ->
      if b = 1.0 then
        (* Single number case - don't show "/1" *)
        Pp.float ctx a
      else (
        Pp.float ctx a;
        Pp.op_char ctx '/';
        Pp.float ctx b)

let rec pp_box_sizing : box_sizing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_box_sizing ctx v
  | Border_box -> Pp.string ctx "border-box"
  | Content_box -> Pp.string ctx "content-box"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_caption_side : caption_side Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_caption_side ctx v
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_clear : clear Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_clear ctx v
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Both -> Pp.string ctx "both"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_overflow_anchor : overflow_anchor Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overflow_anchor ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_object_fit : object_fit Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_object_fit ctx v
  | Fill -> Pp.string ctx "fill"
  | Contain -> Pp.string ctx "contain"
  | Cover -> Pp.string ctx "cover"
  | None -> Pp.string ctx "none"
  | Scale_down -> Pp.string ctx "scale-down"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_object_view_box : object_view_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_object_view_box ctx v
  | None -> Pp.string ctx "none"
  | Inset (top, right, bottom, left) ->
      Pp.string ctx "inset(";
      pp_length ctx top;
      pp_optional_inset_sides pp_length ctx right bottom left;
      Pp.char ctx ')'
  | Xywh { x; y; width; height; rounded } ->
      Pp.string ctx "xywh(";
      pp_clip_path_inset_quad ctx x y width height;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Rect { top; right; bottom; left; rounded } ->
      Pp.string ctx "rect(";
      pp_clip_path_inset_quad ctx top right bottom left;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_float_side : float_side Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_float_side ctx v
  | None -> Pp.string ctx "none"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_visibility t : visibility =
  Cursor.enum_or_var "visibility"
    [
      ("visible", (Visible : visibility));
      ("hidden", Hidden);
      ("collapse", Collapse);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_visibility t))
    t

let rec read_z_index t : z_index =
  let read_calc_z t : z_index =
    match read_integer_calc "z-index" t with
    | `Int n -> Index n
    | `Calc expr -> Calc expr
  in
  let read_var_z t : z_index = Var (read_var read_z_index t) in
  Cursor.enum_or_calls "z-index"
    [
      ("auto", (Auto : z_index));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("calc", read_calc_z); ("var", read_var_z) ]
    ~default:(fun t -> (Index (Cursor.int t) : z_index))
    t

let read_aspect_ratio_number t =
  (* CSS Sizing 4 sec. 5: an [<aspect-ratio>] component may be a [calc()] that
     resolves to a number. Keep the number AST for normal-output fidelity; the
     printer folds constant expressions for minified output. The production is
     [<ratio>], whose CSS Values 4 sec. 6.5 numbers carry a [0,inf] range. *)
  let n = read_number t in
  match n with
  | Num v when v < 0. -> Cursor.err_invalid t "aspect-ratio cannot be negative"
  | _ -> n

let rec read_aspect_ratio (t : Cursor.t) : aspect_ratio =
  let read_var_ar t : aspect_ratio =
    (Var (read_var read_aspect_ratio t) : aspect_ratio)
  in
  let read_ratio t =
    let w = read_aspect_ratio_number t in
    Cursor.ws t;
    if Cursor.peek_delim t = Some '/' then (
      Cursor.expect '/' t;
      Cursor.ws t;
      let h = read_aspect_ratio_number t in
      (w, h))
    else (w, Num 1.0)
  in
  let read_number_or_ratio t : aspect_ratio =
    let w, h = read_ratio t in
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "auto" ->
        Cursor.skip t;
        aspect_ratio_of_numbers ~auto:true w h
    | _ -> aspect_ratio_of_numbers ~auto:false w h
  in
  let read_auto t : aspect_ratio =
    match Cursor.peek_ident t with
    | Some "auto" -> (
        Cursor.skip t;
        (* [auto] may stand alone or be followed by a [<ratio>]. Only treat a
           following number as a ratio so a trailing separator / whitespace
           (e.g. [aspect-ratio: auto;]) resolves to plain [Auto]. *)
        match Cursor.option read_ratio t with
        | Some (w, h) -> aspect_ratio_of_numbers ~auto:true w h
        | None -> Auto)
    | _ -> Cursor.err_expected t "auto"
  in
  Cursor.enum_or_var "aspect-ratio"
    [
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:read_var_ar
    ~default:(Cursor.one_of [ read_auto; read_number_or_ratio ])
    t

let rec read_inline_sizing t : inline_sizing =
  Cursor.enum_or_var "inline-sizing"
    [
      ("normal", (Normal : inline_sizing));
      ("stretch", Stretch);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_inline_sizing t) : inline_sizing))
    t

let rec read_interpolate_size t : interpolate_size =
  Cursor.enum_or_var "interpolate-size"
    [
      ("numeric-only", (Numeric_only : interpolate_size));
      ("allow-keywords", Allow_keywords);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_interpolate_size t) : interpolate_size))
    t

let read_min_intrinsic_sizing_keyword t : min_intrinsic_sizing_keyword =
  Cursor.enum "min-intrinsic-sizing"
    [
      ("legacy", (Legacy : min_intrinsic_sizing_keyword));
      ("zero-if-scroll", Zero_if_scroll);
      ("zero-if-extrinsic", Zero_if_extrinsic);
    ]
    t

let rec read_min_intrinsic_sizing t : min_intrinsic_sizing =
  Cursor.enum_or_var "min-intrinsic-sizing"
    [
      ("inherit", (Inherit : min_intrinsic_sizing));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_min_intrinsic_sizing t) : min_intrinsic_sizing))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_min_intrinsic_sizing_keyword t
      in
      let duplicate keyword =
        List.length
          (List.filter (equal_min_intrinsic_sizing_keyword keyword) keywords)
        > 1
      in
      if List.exists duplicate keywords then
        Cursor.err_invalid t "min-intrinsic-sizing";
      Sizing keywords)
    t

let rec read_table_layout t : table_layout =
  Cursor.enum_or_var "table-layout"
    [
      ("auto", (Auto : table_layout));
      ("fixed", Fixed);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_table_layout t) : table_layout))
    t

let rec read_box_sizing (t : Cursor.t) : box_sizing =
  Cursor.enum_or_var "box-sizing"
    [
      ("border-box", (Border_box : box_sizing));
      ("content-box", Content_box);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_box_sizing t) : box_sizing))
    t

let rec read_caption_side t : caption_side =
  Cursor.enum_or_var "caption-side"
    [
      ("top", (Top : caption_side));
      ("bottom", Bottom);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caption_side t))
    t

let rec read_object_fit t : object_fit =
  Cursor.enum_or_var "object-fit"
    [
      ("fill", (Fill : object_fit));
      ("contain", Contain);
      ("cover", Cover);
      ("none", None);
      ("scale-down", Scale_down);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_object_fit t))
    t

let rec read_overflow_anchor (t : Cursor.t) : overflow_anchor =
  let keywords : (string * overflow_anchor) list =
    [
      ("auto", Auto);
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "overflow-anchor" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_overflow_anchor t) : overflow_anchor))
     t
    : overflow_anchor)

let rec read_clear (t : Cursor.t) : clear =
  Cursor.enum_or_var "clear"
    [
      ("none", (None : clear));
      ("left", Left);
      ("right", Right);
      ("both", Both);
      ("inline-start", Inline_start);
      ("inline-end", Inline_end);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_clear t))
    t

let rec read_float_side (t : Cursor.t) : float_side =
  Cursor.enum_or_var "float-side"
    [
      ("none", (None : float_side));
      ("left", Left);
      ("right", Right);
      ("inline-start", Inline_start);
      ("inline-end", Inline_end);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_float_side t))
    t

let rec read_zoom (t : Cursor.t) : zoom =
  (* CSS Viewport 1 sec. 3 spells [zoom] as [normal | reset | <number [0,inf]> |
     <percentage [0,inf]>], so a negative zoom is no zoom. *)
  let non_negative n =
    if n < 0. then Cursor.err_invalid t "zoom cannot be negative" else n
  in
  let read_value t =
    match Cursor.percentage_opt t with
    | Some p -> (Pct (non_negative p) : zoom)
    | None -> (
        match Cursor.number_opt t with
        | Some n -> Num (non_negative n)
        | None ->
            Cursor.err_invalid t "expected a number or percentage for zoom")
  in
  Cursor.enum_or_var "zoom"
    [
      ("normal", (Normal : zoom));
      ("reset", Reset);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_zoom t))
    ~default:read_value t

let read_object_view_box_inset t =
  Cursor.call "inset" t (fun t ->
      Cursor.ws t;
      let top = read_length t in
      Cursor.ws t;
      let read_opt () : length option =
        if Cursor.is_done t then None else Some (read_length t)
      in
      let right = read_opt () in
      Cursor.ws t;
      let bottom = if Option.is_some right then read_opt () else None in
      Cursor.ws t;
      let left = if Option.is_some bottom then read_opt () else None in
      (Inset (top, right, bottom, left) : object_view_box))

let read_object_view_box_xywh t =
  Cursor.call "xywh" t (fun inner ->
      Cursor.ws inner;
      let x, y, width, height = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      (Xywh { x; y; width; height; rounded } : object_view_box))

let read_object_view_box_rect t =
  Cursor.call "rect" t (fun inner ->
      Cursor.ws inner;
      let top, right, bottom, left = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      (Rect { top; right; bottom; left; rounded } : object_view_box))

let rec read_object_view_box t : object_view_box =
  Cursor.enum_or_calls "object-view-box"
    [
      ("none", (None : object_view_box));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("inset", read_object_view_box_inset);
        ("xywh", read_object_view_box_xywh);
        ("rect", read_object_view_box_rect);
        ("var", fun t -> Var (Values.read_var read_object_view_box t));
      ]
    t

(* Fold the value-independent parts of these own-typed [calc()]s ([calc(var(--x)
   * 1)] -> [calc(var(--x))], [calc(10 / 2)] -> [5]), keeping any [var()]. The
   printer is a pure serialiser, so these AST-level folds replace the numeric /
   identity reduction the printer did under minify. *)
(* A [<percentage>] operand parsed inside an opacity [calc()] is the number it
   denotes ([Val (Opacity_number f)] = [f]); drop the [Val] wrapper so the
   generic numeric fold combines it like any other number. *)
let rec flatten_opacity_pct (c : opacity Values.calc) : opacity Values.calc =
  match c with
  | Values.Val (Opacity_number f) -> Values.Num f
  | Values.Nested inner -> Values.Nested (flatten_opacity_pct inner)
  | Values.Parens inner -> Values.Parens (flatten_opacity_pct inner)
  | Values.Expr (l, op, r) ->
      Values.Expr (flatten_opacity_pct l, op, flatten_opacity_pct r)
  | _ -> c

let normalize_opacity (o : opacity) : opacity =
  match o with
  | Calc c -> (
      match Values.eval_calc (flatten_opacity_pct c) with
      | Values.Num f -> Opacity_number f
      | Values.Val v -> v
      | folded -> Calc folded)
  | _ -> o
