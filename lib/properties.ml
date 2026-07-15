open Common
open Values
open Syntax
include Properties_intf

let err_invalid_value ?got t prop_name value =
  Cursor.err ?got t ("invalid " ^ prop_name ^ " value: " ^ value)

(* Generic length parsing helpers *)
let read_line_height_length t : line_height =
  let n, repr, unit = Cursor.number_repr_with_unit t in
  let n, repr = normalize_signed_zero n repr in
  if n < 0. then Cursor.err_invalid t "line-height cannot be negative"
  else
    let authored () : line_height = Number { value = n; unit; repr } in
    match unit with
    | Some "px" -> if Pp.string_of_float n = repr then Px n else authored ()
    | Some "rem" -> if Pp.string_of_float n = repr then Rem n else authored ()
    | Some "em" -> if Pp.string_of_float n = repr then Em n else authored ()
    | Some "%" -> if Pp.string_of_float n = repr then Pct n else authored ()
    | None -> if Pp.string_of_float n = repr then Num n else authored ()
    | Some _ -> authored ()

let rec numeric_line_height_calc_leaves : line_height calc -> line_height calc =
  function
  | Val (Num n) | Val (Number { value = n; unit = None; _ }) -> Num n
  | Nested inner -> Nested (numeric_line_height_calc_leaves inner)
  | Parens inner -> Parens (numeric_line_height_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_line_height_calc_leaves left,
          op,
          numeric_line_height_calc_leaves right )
  | leaf -> leaf

let read_vertical_align_length t : vertical_align =
  let n, unit = Cursor.number_with_unit t in
  match unit with
  | Some "px" -> Px n
  | Some "rem" -> Rem n
  | Some "em" -> Em n
  | Some "%" -> Pct n
  (* A unitless [0] is the valid zero <length> (CSS Values 4 §6.1); any other
     unitless number is not a length and is rejected. *)
  | None when n = 0. -> Zero
  | None -> Cursor.err_invalid t "vertical-align requires a unit"
  | Some u -> Cursor.err_invalid t ("invalid vertical-align unit: " ^ u)

(* CSS Display 3 §2.1 [display-outside]: pre-existing aliases inside the
   single-value vocabulary that compose with a [display-inside] in the two-value
   form. The composite [<outside> <inside>] is a [Multi]. *)
let display_outside_idents : (string * display) list =
  [
    ("block", Block);
    ("inline", Inline);
    ("run-in", Run_in);
    ("list-item", List_item);
  ]

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
  (* CSS Display 3 §2.1 two-value form [<display-outside> <display-inside>].
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
    | Some s when Option.is_none !outside -> (
        match List.assoc_opt s display_outside_idents with
        | Some value ->
            ignore (Cursor.ident t : string);
            outside := Option.Some value;
            true
        | Option.None -> false)
    | Some s when Option.is_none !inside -> (
        match List.assoc_opt s list_item_inside_idents with
        | Some value ->
            ignore (Cursor.ident t : string);
            inside := Option.Some value;
            true
        | Option.None -> false)
    | _ -> false
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
  | Some (Component.Func { node = { name = "var"; _ }; _ }) -> read_var t
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

let rec read_css_wide t : css_wide =
  Cursor.enum_or_var "css-wide keyword"
    [
      ("initial", (Initial : css_wide));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_css_wide t))
    t

let rec pp_css_wide : css_wide Pp.t =
 fun ctx -> function
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_css_wide ctx v

let css_wide_keywords =
  [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let is_css_wide_keyword value =
  List.mem (String.lowercase_ascii value) css_wide_keywords

(* CSS Cascade 5 sec. 7.3: a CSS-wide keyword must stand alone, so it is invalid
   mixed with other tokens (e.g. [font: initial 16px serif]). True when [value]
   is not itself a lone CSS-wide keyword yet contains one as an identifier. *)
let value_has_css_wide_mix value =
  let trimmed = String.trim value in
  (not (is_css_wide_keyword trimmed))
  &&
  let components = Cursor.remaining (Cursor.of_string trimmed) in
  List.exists
    (function
      | Component.Preserved { kind = Token.Ident ident; _ } ->
          is_css_wide_keyword ident
      | _ -> false)
    components

(* Components-form equivalent: skip the round-trip through a string buffer used
   by [value_has_css_wide_mix] when callers already hold the component list. *)
let components_have_css_wide_mix components =
  let non_ws =
    List.filter
      (function
        | Component.Preserved { kind = Token.Whitespace; _ } -> false
        | _ -> true)
      components
  in
  let lone_css_wide =
    match non_ws with
    | [ Component.Preserved { kind = Token.Ident ident; _ } ] ->
        is_css_wide_keyword ident
    | _ -> false
  in
  (not lone_css_wide)
  && List.exists
       (function
         | Component.Preserved { kind = Token.Ident ident; _ } ->
             is_css_wide_keyword ident
         | _ -> false)
       non_ws

let rec read_flex_direction t : flex_direction =
  Cursor.enum_or_var "flex-direction"
    [
      ("row", (Row : flex_direction));
      ("row-reverse", Row_reverse);
      ("column", Column);
      ("column-reverse", Column_reverse);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_flex_direction t))
    t

(* Helper to parse flattened baseline tokens shared across align/justify
   readers *)
let read_flat_baseline ~what ~baseline ~first ~last t =
  Cursor.enum what
    [ ("baseline", baseline) ]
    ~default:(fun t ->
      let tok = Cursor.ident t in
      match tok with
      | "first" ->
          Cursor.ws t;
          Cursor.expect_string "baseline" t;
          first
      | "last" ->
          Cursor.ws t;
          Cursor.expect_string "baseline" t;
          last
      | s -> err_invalid_value t what s)
    t

module Align_items = struct
  let read_flat_baseline t : align_items =
    read_flat_baseline ~what:"align-items"
      ~baseline:(Baseline : align_items)
      ~first:First_baseline ~last:Last_baseline t

  let read_safe t : align_items =
    Cursor.expect_string "safe" t;
    Cursor.ws t;
    Cursor.enum "align-items safe"
      [
        ("center", (Safe_center : align_items));
        ("start", Safe_start);
        ("end", Safe_end);
        ("self-start", Self_start);
        ("self-end", Self_end);
        ("flex-start", Safe_flex_start);
        ("flex-end", Safe_flex_end);
      ]
      t

  let read_unsafe t : align_items =
    Cursor.expect_string "unsafe" t;
    Cursor.ws t;
    Cursor.enum "align-items unsafe"
      [
        ("center", (Unsafe_center : align_items));
        ("start", Unsafe_start);
        ("end", Unsafe_end);
        ("self-start", Unsafe_self_start);
        ("self-end", Unsafe_self_end);
        ("flex-start", Unsafe_flex_start);
        ("flex-end", Unsafe_flex_end);
      ]
      t
end

let rec read_align_items t : align_items =
  Cursor.enum_or_var "align-items"
    [
      ("normal", Normal);
      ("stretch", Stretch);
      ("anchor-center", Anchor_center);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("self-start", Self_start);
      ("self-end", Self_end);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("inherit", (Inherit : align_items));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_align_items t) : align_items))
    ~default:
      (Cursor.one_of
         [
           Align_items.read_flat_baseline;
           Align_items.read_safe;
           Align_items.read_unsafe;
         ])
    t

module Align_content = struct
  let read_flat_baseline t : align_content =
    read_flat_baseline ~what:"align-content"
      ~baseline:(Baseline : align_content)
      ~first:First_baseline ~last:Last_baseline t

  let read_safe t : align_content =
    Cursor.expect_string "safe" t;
    Cursor.ws t;
    Cursor.enum "align-content safe"
      [
        ("center", (Safe_center : align_content));
        ("start", Safe_start);
        ("end", Safe_end);
        ("flex-start", Safe_flex_start);
        ("flex-end", Safe_flex_end);
        ("left", Safe_left);
        ("right", Safe_right);
      ]
      t

  let read_unsafe t : align_content =
    Cursor.expect_string "unsafe" t;
    Cursor.ws t;
    Cursor.enum "align-content unsafe"
      [
        ("center", (Unsafe_center : align_content));
        ("start", Unsafe_start);
        ("end", Unsafe_end);
        ("flex-start", Unsafe_flex_start);
        ("flex-end", Unsafe_flex_end);
        ("left", Unsafe_left);
        ("right", Unsafe_right);
      ]
      t
end

let rec read_align_content t : align_content =
  Cursor.enum_or_var "align-content"
    [
      ("normal", (Normal : align_content));
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("left", Left);
      ("right", Right);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("space-evenly", Space_evenly);
      ("stretch", Stretch);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_align_content t) : align_content))
    ~default:
      (Cursor.one_of
         [
           Align_content.read_flat_baseline;
           Align_content.read_safe;
           Align_content.read_unsafe;
         ])
    t

module Justify_content = struct
  let read_safe t : justify_content =
    Cursor.expect_string "safe" t;
    Cursor.ws t;
    Cursor.enum "justify-content safe"
      [
        ("center", (Safe_center : justify_content));
        ("start", Safe_start);
        ("end", Safe_end);
        ("flex-start", Safe_flex_start);
        ("flex-end", Safe_flex_end);
        ("left", Left);
        ("right", Right);
      ]
      t

  let read_unsafe t : justify_content =
    Cursor.expect_string "unsafe" t;
    Cursor.ws t;
    Cursor.enum "justify-content unsafe"
      [
        ("center", (Unsafe_center : justify_content));
        ("start", Unsafe_start);
        ("end", Unsafe_end);
        ("flex-start", Unsafe_flex_start);
        ("flex-end", Unsafe_flex_end);
        ("left", Unsafe_left);
        ("right", Unsafe_right);
      ]
      t
end

let rec read_justify_content t : justify_content =
  Cursor.enum_or_var "justify-content"
    [
      ("normal", (Normal : justify_content));
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("left", Left);
      ("right", Right);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("space-evenly", Space_evenly);
      ("stretch", Stretch);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_justify_content t) : justify_content))
    ~default:
      (Cursor.one_of [ Justify_content.read_safe; Justify_content.read_unsafe ])
    t

module Align_self = struct
  let read_flat_baseline t : align_self =
    read_flat_baseline ~what:"align-self"
      ~baseline:(Baseline : align_self)
      ~first:First_baseline ~last:Last_baseline t

  let read_safe t : align_self =
    Cursor.expect_string "safe" t;
    Cursor.ws t;
    Cursor.enum "align-self safe"
      [
        ("center", (Safe_center : align_self));
        ("start", Safe_start);
        ("end", Safe_end);
        ("self-start", Self_start);
        ("self-end", Self_end);
        ("flex-start", Safe_flex_start);
        ("flex-end", Safe_flex_end);
      ]
      t

  let read_unsafe t : align_self =
    Cursor.expect_string "unsafe" t;
    Cursor.ws t;
    Cursor.enum "align-self unsafe"
      [
        ("center", (Unsafe_center : align_self));
        ("start", Unsafe_start);
        ("end", Unsafe_end);
        ("self-start", Unsafe_self_start);
        ("self-end", Unsafe_self_end);
        ("flex-start", Unsafe_flex_start);
        ("flex-end", Unsafe_flex_end);
      ]
      t
end

let rec read_align_self t : align_self =
  Cursor.enum_or_var "align-self"
    [
      ("auto", Auto);
      ("normal", Normal);
      ("stretch", Stretch);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("self-start", Self_start);
      ("self-end", Self_end);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("inherit", (Inherit : align_self));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_align_self t) : align_self))
    ~default:
      (Cursor.one_of
         [
           Align_self.read_flat_baseline;
           Align_self.read_safe;
           Align_self.read_unsafe;
         ])
    t

module Justify_items = struct
  let read_flat_baseline t : justify_items =
    read_flat_baseline ~what:"justify-items"
      ~baseline:(Baseline : justify_items)
      ~first:First_baseline ~last:Last_baseline t

  let read_safe t : justify_items =
    Cursor.expect_string "safe" t;
    Cursor.ws t;
    Cursor.enum "justify-items safe"
      [
        ("center", (Safe_center : justify_items));
        ("start", Safe_start);
        ("end", Safe_end);
        ("self-start", Safe_self_start);
        ("self-end", Safe_self_end);
        ("flex-start", Safe_flex_start);
        ("flex-end", Safe_flex_end);
        ("left", Safe_left);
        ("right", Safe_right);
      ]
      t

  let read_unsafe t : justify_items =
    Cursor.expect_string "unsafe" t;
    Cursor.ws t;
    Cursor.enum "justify-items unsafe"
      [
        ("center", (Unsafe_center : justify_items));
        ("start", Unsafe_start);
        ("end", Unsafe_end);
        ("self-start", Unsafe_self_start);
        ("self-end", Unsafe_self_end);
        ("flex-start", Unsafe_flex_start);
        ("flex-end", Unsafe_flex_end);
        ("left", Unsafe_left);
        ("right", Unsafe_right);
      ]
      t
end

let rec read_justify_items t : justify_items =
  let read_legacy t =
    Cursor.expect_string "legacy" t;
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "center" ->
        let _ = Cursor.ident t in
        Legacy_center
    | Some "left" ->
        let _ = Cursor.ident t in
        Legacy_left
    | Some "right" ->
        let _ = Cursor.ident t in
        Legacy_right
    | None -> Legacy
    | Some _ when Cursor.peek_semicolon t -> Legacy
    | Some s -> err_invalid_value t "justify-items legacy" s
  in
  Cursor.enum_or_var "justify-items"
    [
      ("normal", (Normal : justify_items));
      ("stretch", Stretch);
      ("anchor-center", Anchor_center);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("self-start", Self_start);
      ("self-end", Self_end);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("left", Left);
      ("right", Right);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_justify_items t) : justify_items))
    ~default:
      (Cursor.one_of
         [
           read_legacy;
           Justify_items.read_flat_baseline;
           Justify_items.read_safe;
           Justify_items.read_unsafe;
         ])
    t

module Justify_self = struct
  let read_flat_baseline t : justify_self =
    read_flat_baseline ~what:"justify-self"
      ~baseline:(Baseline : justify_self)
      ~first:First_baseline ~last:Last_baseline t

  let read_unsafe t : justify_self =
    Cursor.expect_string "unsafe" t;
    Cursor.ws t;
    Cursor.enum "justify-self unsafe"
      [
        ("center", (Unsafe_center : justify_self));
        ("start", Unsafe_start);
        ("end", Unsafe_end);
        ("self-start", Unsafe_self_start);
        ("self-end", Unsafe_self_end);
        ("flex-start", Unsafe_flex_start);
        ("flex-end", Unsafe_flex_end);
        ("left", Unsafe_left);
        ("right", Unsafe_right);
      ]
      t

  let read_safe t : justify_self =
    Cursor.expect_string "safe" t;
    Cursor.ws t;
    Cursor.enum "justify-self safe"
      [
        ("center", (Safe_center : justify_self));
        ("start", Safe_start);
        ("end", Safe_end);
        ("self-start", Safe_self_start);
        ("self-end", Safe_self_end);
        ("flex-start", Safe_flex_start);
        ("flex-end", Safe_flex_end);
        ("left", Safe_left);
        ("right", Safe_right);
      ]
      t
end

let rec read_justify_self t : justify_self =
  Cursor.enum_or_var "justify-self"
    [
      ("auto", Auto);
      ("inherit", (Inherit : justify_self));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("normal", Normal);
      ("stretch", Stretch);
      ("anchor-center", Anchor_center);
      ("center", Center);
      ("start", Start);
      ("end", End);
      ("self-start", Self_start);
      ("self-end", Self_end);
      ("flex-start", Flex_start);
      ("flex-end", Flex_end);
      ("left", Left);
      ("right", Right);
    ]
    ~var:(fun t -> (Var (Values.read_var read_justify_self t) : justify_self))
    ~default:
      (Cursor.one_of
         [
           Justify_self.read_flat_baseline;
           Justify_self.read_unsafe;
           Justify_self.read_safe;
         ])
    t

let rec read_font_weight t : font_weight =
  let read_var t : font_weight = Var (read_var read_font_weight t) in
  Cursor.ws t;
  Cursor.enum_or_calls "font-weight"
    [
      ("normal", Normal);
      ("bold", Bold);
      ("bolder", Bolder);
      ("lighter", Lighter);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let n = Cursor.number t in
      let weight = int_of_float n in
      if weight >= 1 && weight <= 1000 then (Weight weight : font_weight)
      else err_invalid_value t "font-weight" (string_of_int weight))
    t

let rec read_font_style t : font_style =
  Cursor.enum_or_var "font-style"
    [
      ("normal", (Normal : font_style));
      ("italic", (Italic : font_style));
      ("inherit", (Inherit : font_style));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_style t))
    ~default:(fun t ->
      Cursor.expect_string "oblique" t;
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_semicolon t then Oblique
      else
        let first = read_angle t in
        Cursor.ws t;
        if Cursor.is_done t || Cursor.peek_semicolon t then Oblique_angle first
        else
          (* CSS Fonts 4 §11.2 wants the first oblique angle <= the second.
             Browsers keep a descending range ([oblique 20deg 10deg]), so accept
             it but warn, leaving [Css.of_string ~strict] free to reject it. *)
          let second = read_angle t in
          (match (angle_degrees_opt first, angle_degrees_opt second) with
          | Some a, Some b when a > b ->
              Cursor.push_warning t
                (Error.bad_value (Cursor.position t) ~property:"font-style"
                   ~reason:
                     "oblique angle range must run from the smaller angle to \
                      the larger (CSS Fonts 4 §11.2)")
          | _ -> ());
          Oblique_range (first, second))
    t

let rec read_font_size t : font_size =
  let read_var t : font_size = Var (read_var read_font_size t) in
  let read_calc t : font_size = Calc (read_calc read_font_size t) in
  let read_length t : font_size =
    let len = read_non_negative_length ~with_keywords:false t in
    Length len
  in
  let read_pct t : font_size =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "negative font-size percentage";
    Cursor.expect '%' t;
    Pct n
  in
  Cursor.enum_or_calls "font-size"
    [
      ("xx-small", (Xx_small : font_size));
      ("x-small", X_small);
      ("small", Small);
      ("medium", Medium);
      ("large", Large);
      ("x-large", X_large);
      ("xx-large", Xx_large);
      ("xxx-large", Xxx_large);
      ("larger", Larger);
      ("smaller", Smaller);
      ("math", Math);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:(fun t ->
      (* Try percentage first, then length *)
      Cursor.one_of [ read_pct; read_length ] t)
    t

let rec read_text_align t : text_align =
  Cursor.enum_or_var "text-align"
    [
      ("left", (Left : text_align));
      ("right", Right);
      ("center", Center);
      ("justify", Justify);
      ("start", Start);
      ("end", End);
      ("match-parent", Match_parent);
      ("-webkit-match-parent", Webkit_match_parent);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_align t))
    t

let rec read_text_decoration_line t : text_decoration_line =
  Cursor.enum_or_var "text-decoration-line"
    [
      ("none", (None : text_decoration_line));
      ("underline", Underline);
      ("overline", Overline);
      ("line-through", Line_through);
      ("blink", Blink);
      ("spelling-error", Spelling_error);
      ("grammar-error", Grammar_error);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_line t))
    t

let rec read_text_decoration_style t : text_decoration_style =
  Cursor.enum_or_var "text-decoration-style"
    [
      ("solid", (Solid : text_decoration_style));
      ("double", Double);
      ("dotted", Dotted);
      ("dashed", Dashed);
      ("wavy", Wavy);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_style t))
    t

module Text_decoration = struct
  type component =
    | Line of text_decoration_line
    | Style of text_decoration_style
    | Color of color
    | Thickness of length

  type components = {
    lines : text_decoration_line list;
    style : text_decoration_style option;
    color : color option;
    thickness : length option;
  }

  let empty = { lines = []; style = None; color = None; thickness = None }

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Line (read_text_decoration_line t));
        (fun t -> Style (read_text_decoration_style t));
        (fun t -> Color (read_color t));
        (fun t -> Thickness (read_length t));
      ]
      t

  let merge t acc = function
    | Line l ->
        (* Check for duplicate lines - per CSS spec, || combinator means each
           component at most once *)
        if l = (None : text_decoration_line) && acc.lines <> [] then
          Cursor.err t "text-decoration-line none cannot be mixed"
        else if
          l <> (None : text_decoration_line)
          && List.mem (None : text_decoration_line) acc.lines
        then Cursor.err t "text-decoration-line none cannot be mixed"
        else if List.mem l acc.lines then
          Cursor.err t
            ("duplicate text-decoration-line: "
            ^
            match l with
            | None -> "none"
            | Underline -> "underline"
            | Overline -> "overline"
            | Line_through -> "line-through"
            | Blink -> "blink"
            | Spelling_error -> "spelling-error"
            | Grammar_error -> "grammar-error"
            | Inherit -> "inherit"
            | Initial -> "initial"
            | Unset -> "unset"
            | Revert -> "revert"
            | Revert_layer -> "revert-layer"
            | Var _ -> "var(...)")
        else { acc with lines = acc.lines @ [ l ] }
    | Style s when acc.style = None -> { acc with style = Some s }
    | Color c when acc.color = None -> { acc with color = Some c }
    | Thickness th when acc.thickness = None -> { acc with thickness = Some th }
    | Style _ -> Cursor.err t "duplicate text-decoration-style"
    | Color _ -> Cursor.err t "duplicate text-decoration-color"
    | Thickness _ -> Cursor.err t "duplicate text-decoration-thickness"

  let to_shorthand (components : components) : text_decoration_shorthand =
    {
      lines = components.lines;
      style = components.style;
      color = components.color;
      thickness = components.thickness;
    }
end

let read_text_decoration_shorthand t : text_decoration_shorthand =
  let acc, _ =
    Cursor.fold_many Text_decoration.read_component ~init:Text_decoration.empty
      ~f:(Text_decoration.merge t) t
  in
  Text_decoration.to_shorthand acc

let rec read_text_decoration t : text_decoration =
  let read_var t : text_decoration = Var (read_var read_text_decoration t) in
  Cursor.enum_or_calls "text-decoration"
    [
      ("inherit", (Inherit : text_decoration));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("none", None);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let shorthand = read_text_decoration_shorthand t in
      (* For the main text-decoration property, require at least one line
         decoration *)
      if shorthand.lines = [] then
        Cursor.err t
          "text-decoration requires at least one line decoration (underline, \
           overline, or line-through)"
      else (Shorthand shorthand : text_decoration))
    t

let rec read_text_decoration_skip t : text_decoration_skip =
  Cursor.enum_or_var "text-decoration-skip"
    [
      ("none", (None : text_decoration_skip));
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip t))
    t

let rec read_text_decoration_skip_self t : text_decoration_skip_self =
  Cursor.enum_or_var "text-decoration-skip-self"
    [
      ("none", (None : text_decoration_skip_self));
      ("objects", Objects);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_self t))
    t

let rec read_text_decoration_skip_box t : text_decoration_skip_box =
  Cursor.enum_or_var "text-decoration-skip-box"
    [
      ("all", (All : text_decoration_skip_box));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_box t))
    t

let rec read_text_decoration_skip_inset t : text_decoration_skip_inset =
  Cursor.enum_or_var "text-decoration-skip-inset"
    [
      ("none", (None : text_decoration_skip_inset));
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_inset t))
    t

let read_text_decoration_skip_space t : text_decoration_skip_space =
  Cursor.enum "text-decoration-skip-spaces"
    [
      ("all", (All : text_decoration_skip_space)); ("start", Start); ("end", End);
    ]
    t

let rec read_text_decoration_skip_spaces t : text_decoration_skip_spaces =
  Cursor.enum_or_var "text-decoration-skip-spaces"
    [
      ("inherit", (Inherit : text_decoration_skip_spaces));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_decoration_skip_spaces t))
    ~default:(fun t ->
      let spaces =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_text_decoration_skip_space t
      in
      match spaces with
      | [ All ] | [ Start ] | [ End ] | [ Start; End ] | [ End; Start ] ->
          (Spaces spaces : text_decoration_skip_spaces)
      | _ -> Cursor.err_invalid t "text-decoration-skip-spaces")
    t

let rec read_text_indent_value t : text_indent_value =
  Cursor.enum_or_var "text-indent"
    [
      ("inherit", (Inherit : text_indent_value));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_indent_value t))
    ~default:(fun t ->
      let length = ref Option.None in
      let hanging = ref false in
      let each_line = ref false in
      let at_end t =
        Cursor.is_done t || Cursor.peek_semicolon t
        || Cursor.peek_delim t = Some '!'
      in
      while not (at_end t) do
        Cursor.ws t;
        match Cursor.peek_ident t with
        | Some "hanging" when not !hanging ->
            Cursor.skip t;
            hanging := true
        | Some "each-line" when not !each_line ->
            Cursor.skip t;
            each_line := true
        | _ when Option.is_none !length ->
            length := Some (read_length_percentage ~with_keywords:false t)
        | _ -> Cursor.err_invalid t "text-indent"
      done;
      match !length with
      | Some length ->
          Indent { length; hanging = !hanging; each_line = !each_line }
      | None -> Cursor.err_invalid t "text-indent")
    t

let pp_text_emphasis_fill ctx = function
  | Filled -> Pp.string ctx "filled"
  | Open -> Pp.string ctx "open"

let pp_text_emphasis_shape ctx = function
  | Dot -> Pp.string ctx "dot"
  | Circle -> Pp.string ctx "circle"
  | Double_circle -> Pp.string ctx "double-circle"
  | Triangle -> Pp.string ctx "triangle"
  | Sesame -> Pp.string ctx "sesame"

let rec pp_text_emphasis_style : text_emphasis_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Mark (fill, shape) ->
      let first = ref true in
      let space_if_needed () =
        if !first then first := false else Pp.space ctx ()
      in
      Option.iter
        (fun fill ->
          space_if_needed ();
          pp_text_emphasis_fill ctx fill)
        fill;
      Option.iter
        (fun shape ->
          space_if_needed ();
          pp_text_emphasis_shape ctx shape)
        shape
  | String s -> Pp.quoted_string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis_style ctx v

let rec pp_text_emphasis : text_emphasis Pp.t =
 fun ctx -> function
  | Emphasis (style, color) ->
      let first = ref true in
      let space_if_needed () =
        if !first then first := false else Pp.space ctx ()
      in
      Option.iter
        (fun style ->
          space_if_needed ();
          pp_text_emphasis_style ctx style)
        style;
      Option.iter
        (fun color ->
          space_if_needed ();
          pp_color ctx color)
        color
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis ctx v

let read_text_emphasis_fill t =
  Cursor.enum "text-emphasis fill"
    [ ("filled", (Filled : text_emphasis_fill)); ("open", Open) ]
    t

let read_text_emphasis_shape t =
  Cursor.enum "text-emphasis shape"
    [
      ("dot", (Dot : text_emphasis_shape));
      ("circle", Circle);
      ("double-circle", Double_circle);
      ("triangle", Triangle);
      ("sesame", Sesame);
    ]
    t

module Text_emphasis_style = struct
  type part = Fill of text_emphasis_fill | Shape of text_emphasis_shape

  let read_part t =
    Cursor.one_of
      [
        (fun t -> Fill (read_text_emphasis_fill t));
        (fun t -> Shape (read_text_emphasis_shape t));
      ]
      t

  let merge t (fill_opt, shape_opt) = function
    | Fill fill when Option.is_none fill_opt -> (Some fill, shape_opt)
    | Shape shape when Option.is_none shape_opt -> (fill_opt, Some shape)
    | Fill _ -> Cursor.err t "duplicate text-emphasis fill"
    | Shape _ -> Cursor.err t "duplicate text-emphasis shape"

  let read_mark t =
    let first = read_part t in
    let second = Cursor.option read_part t in
    let fill, shape =
      List.fold_left (merge t) (None, None) (first :: Option.to_list second)
    in
    (Mark (fill, shape) : text_emphasis_style)
end

let rec read_text_emphasis_style t : text_emphasis_style =
  Cursor.enum_or_calls "text-emphasis-style"
    [
      ("none", (None : text_emphasis_style));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_text_emphasis_style t)) ]
    ~default:(fun t ->
      Cursor.one_of
        [
          (fun t -> (String (Cursor.string t) : text_emphasis_style));
          Text_emphasis_style.read_mark;
        ]
        t)
    t

module Text_emphasis = struct
  type component = Style of text_emphasis_style | Color of color

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Style (read_text_emphasis_style t));
        (fun t -> Color (read_color t));
      ]
      t

  let merge t (style_opt, color_opt) = function
    | Style style when Option.is_none style_opt -> (Some style, color_opt)
    | Color color when Option.is_none color_opt -> (style_opt, Some color)
    | Style _ -> Cursor.err t "duplicate text-emphasis-style"
    | Color _ -> Cursor.err t "duplicate text-emphasis-color"
end

let rec read_text_emphasis t : text_emphasis =
  Cursor.enum_or_calls "text-emphasis"
    [
      ("inherit", (Inherit : text_emphasis));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_text_emphasis t)) ]
    ~default:(fun t ->
      let style, color =
        Cursor.fold_many Text_emphasis.read_component
          ~init:((None : text_emphasis_style option), (None : color option))
          ~f:(Text_emphasis.merge t) t
        |> fst
      in
      match (style, color) with
      | None, None -> Cursor.err_expected t "text-emphasis value"
      | _ -> Emphasis (style, color))
    t

let read_text_emphasis_skip_keyword t : text_emphasis_skip_keyword =
  Cursor.enum "text-emphasis-skip"
    [
      ("spaces", (Spaces : text_emphasis_skip_keyword));
      ("punctuation", Punctuation);
      ("symbols", Symbols);
      ("narrow", Narrow);
    ]
    t

let rec read_text_emphasis_skip t : text_emphasis_skip =
  Cursor.enum_or_var "text-emphasis-skip"
    [
      ("inherit", (Inherit : text_emphasis_skip));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_emphasis_skip t))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 read_text_emphasis_skip_keyword t
      in
      let duplicate =
        List.exists
          (fun keyword ->
            List.length (List.filter (( = ) keyword) keywords) > 1)
          keywords
      in
      if duplicate then Cursor.err_invalid t "text-emphasis-skip"
      else (Skip keywords : text_emphasis_skip))
    t

let pp_text_emphasis_line : text_emphasis_line Pp.t =
 fun ctx -> function
  | Over -> Pp.string ctx "over"
  | Under -> Pp.string ctx "under"

let pp_text_emphasis_side : text_emphasis_side Pp.t =
 fun ctx -> function
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let pp_text_emphasis_skip_keyword : text_emphasis_skip_keyword Pp.t =
 fun ctx -> function
  | Spaces -> Pp.string ctx "spaces"
  | Punctuation -> Pp.string ctx "punctuation"
  | Symbols -> Pp.string ctx "symbols"
  | Narrow -> Pp.string ctx "narrow"

let rec pp_text_emphasis_skip : text_emphasis_skip Pp.t =
 fun ctx -> function
  | Skip keywords ->
      Pp.list ~sep:Pp.space pp_text_emphasis_skip_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis_skip ctx v

let rec pp_text_emphasis_position : text_emphasis_position Pp.t =
 fun ctx -> function
  | Position (line, side) ->
      pp_text_emphasis_line ctx line;
      Option.iter
        (fun side ->
          Pp.space ctx ();
          pp_text_emphasis_side ctx side)
        side
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_emphasis_position ctx v

let read_text_emphasis_line t =
  Cursor.enum "text-emphasis-position line"
    [ ("over", (Over : text_emphasis_line)); ("under", Under) ]
    t

let read_text_emphasis_side t =
  Cursor.enum "text-emphasis-position side"
    [ ("left", (Left : text_emphasis_side)); ("right", Right) ]
    t

module Text_emphasis_position = struct
  type component = Line of text_emphasis_line | Side of text_emphasis_side

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Line (read_text_emphasis_line t));
        (fun t -> Side (read_text_emphasis_side t));
      ]
      t

  let merge t (line_opt, side_opt) = function
    | Line line when Option.is_none line_opt -> (Some line, side_opt)
    | Side side when Option.is_none side_opt -> (line_opt, Some side)
    | Line _ -> Cursor.err t "duplicate text-emphasis-position line"
    | Side _ -> Cursor.err t "duplicate text-emphasis-position side"
end

let rec read_text_emphasis_position t : text_emphasis_position =
  Cursor.enum_or_calls "text-emphasis-position"
    [
      ("inherit", (Inherit : text_emphasis_position));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_text_emphasis_position t)) ]
    ~default:(fun t ->
      let line, side =
        Cursor.fold_many Text_emphasis_position.read_component
          ~init:
            ( (None : text_emphasis_line option),
              (None : text_emphasis_side option) )
          ~f:(Text_emphasis_position.merge t)
          t
        |> fst
      in
      match (line, side) with
      | Some line, side -> (Position (line, side) : text_emphasis_position)
      | _ -> Cursor.err_expected t "text-emphasis-position value")
    t

let pp_text_underline_position_keyword : text_underline_position_keyword Pp.t =
 fun ctx -> function
  | Under -> Pp.string ctx "under"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"

let rec pp_text_underline_position : text_underline_position Pp.t =
 fun ctx (value : text_underline_position) ->
  match value with
  | Auto -> Pp.string ctx "auto"
  | From_font -> Pp.string ctx "from-font"
  | Position keywords ->
      Pp.list ~sep:Pp.space pp_text_underline_position_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_underline_position ctx v

let read_text_underline_position_keyword t : text_underline_position_keyword =
  Cursor.enum "text-underline-position"
    [
      ("under", (Under : text_underline_position_keyword));
      ("left", Left);
      ("right", Right);
    ]
    t

let rec read_text_underline_position t : text_underline_position =
  Cursor.enum_or_var "text-underline-position"
    [
      ("auto", (Auto : text_underline_position));
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_text_underline_position t)
        : text_underline_position))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_text_underline_position_keyword t
      in
      let valid =
        match keywords with
        | [ Under ] -> true
        | [ Under; (Left | Right) ] | [ (Left | Right); Under ] -> true
        | _ -> false
      in
      if not valid then Cursor.err_invalid t "text-underline-position";
      (Position keywords : text_underline_position))
    t

let rec pp_text_orientation : text_orientation Pp.t =
 fun ctx -> function
  | Mixed -> Pp.string ctx "mixed"
  | Upright -> Pp.string ctx "upright"
  | Sideways -> Pp.string ctx "sideways"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_orientation ctx v

let rec pp_glyph_orientation_vertical : glyph_orientation_vertical Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Angle angle -> pp_angle ctx angle
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_glyph_orientation_vertical ctx v

let rec read_text_orientation t : text_orientation =
  Cursor.enum_or_var "text-orientation"
    [
      ("mixed", (Mixed : text_orientation));
      ("upright", Upright);
      ("sideways", Sideways);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_orientation t))
    t

let rec read_glyph_orientation_vertical t : glyph_orientation_vertical =
  Cursor.enum_or_var "glyph-orientation-vertical"
    [
      ("auto", (Auto : glyph_orientation_vertical));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (read_var read_glyph_orientation_vertical t)
        : glyph_orientation_vertical))
    ~default:(fun t ->
      let angle =
        Cursor.one_of
          [ read_angle; (fun t -> (Deg (Cursor.number t) : angle)) ]
          t
      in
      match angle with
      | Deg value when Float.equal value 0. || Float.equal value 90. ->
          (Angle angle : glyph_orientation_vertical)
      | _ -> Cursor.err_invalid t "glyph-orientation-vertical")
    t

let rec read_text_transform t : text_transform =
  Cursor.enum_or_var "text-transform"
    [
      ("none", (None : text_transform));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_transform t))
    ~default:(fun t ->
      let case : text_transform_case option ref = ref Option.None in
      let full_width = ref false in
      let full_size_kana = ref false in
      let consumed = ref true in
      while !consumed do
        consumed := false;
        Cursor.ws t;
        match Cursor.peek_ident t with
        | Some "uppercase" when Option.is_none !case ->
            Cursor.skip t;
            case := Option.Some Uppercase;
            consumed := true
        | Some "lowercase" when Option.is_none !case ->
            Cursor.skip t;
            case := Option.Some Lowercase;
            consumed := true
        | Some "capitalize" when Option.is_none !case ->
            Cursor.skip t;
            case := Option.Some Capitalize;
            consumed := true
        | Some "full-width" when not !full_width ->
            Cursor.skip t;
            full_width := true;
            consumed := true
        | Some "full-size-kana" when not !full_size_kana ->
            Cursor.skip t;
            full_size_kana := true;
            consumed := true
        | _ -> ()
      done;
      match (!case, !full_width, !full_size_kana) with
      | None, false, false -> Cursor.err_expected t "text-transform"
      | Some c, false, false -> Case c
      | case, full_width, full_size_kana ->
          Combo { case; full_width; full_size_kana })
    t

let read_text_transform_case t =
  Cursor.enum "text-transform case"
    [
      ("capitalize", (Capitalize : text_transform_case));
      ("uppercase", Uppercase);
      ("lowercase", Lowercase);
    ]
    t

let rec pp_line_break : line_break Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Loose -> Pp.string ctx "loose"
  | Normal -> Pp.string ctx "normal"
  | Strict -> Pp.string ctx "strict"
  | Anywhere -> Pp.string ctx "anywhere"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_line_break ctx v

let rec read_line_break t : line_break =
  Cursor.enum_or_var "line-break"
    [
      ("auto", (Auto : line_break));
      ("loose", Loose);
      ("normal", Normal);
      ("strict", Strict);
      ("anywhere", Anywhere);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_line_break t))
    t

let rec pp_font_optical_sizing : font_optical_sizing Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_optical_sizing ctx v

let rec read_font_optical_sizing t : font_optical_sizing =
  Cursor.enum_or_var "font-optical-sizing"
    [
      ("auto", (Auto : font_optical_sizing));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_optical_sizing t))
    t

let rec pp_font_kerning : font_kerning Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_kerning ctx v

let rec read_font_kerning t : font_kerning =
  Cursor.enum_or_var "font-kerning"
    [
      ("auto", (Auto : font_kerning));
      ("normal", Normal);
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_kerning t))
    t

let rec pp_font_language_override : font_language_override Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | String s -> Pp.quoted_string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_language_override ctx v

let rec read_font_language_override t : font_language_override =
  Cursor.enum_or_calls "font-language-override"
    [
      ("normal", (Normal : font_language_override));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_font_language_override t)) ]
    ~default:(fun t -> (String (Cursor.string t) : font_language_override))
    t

let rec pp_font_synthesis_style : font_synthesis_style Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Oblique_only -> Pp.string ctx "oblique-only"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_style ctx v

let rec read_font_synthesis_style t : font_synthesis_style =
  Cursor.enum_or_var "font-synthesis-style"
    [
      ("auto", (Auto : font_synthesis_style));
      ("none", None);
      ("oblique-only", Oblique_only);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_style t))
    t

let rec pp_font_synthesis_weight : font_synthesis_weight Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_weight ctx v

let rec read_font_synthesis_weight t : font_synthesis_weight =
  Cursor.enum_or_var "font-synthesis-weight"
    [
      ("auto", (Auto : font_synthesis_weight));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_weight t))
    t

let rec pp_font_synthesis_small_caps : font_synthesis_small_caps Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_small_caps ctx v

let rec read_font_synthesis_small_caps t : font_synthesis_small_caps =
  Cursor.enum_or_var "font-synthesis-small-caps"
    [
      ("auto", (Auto : font_synthesis_small_caps));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_small_caps t))
    t

let rec pp_font_synthesis_position : font_synthesis_position Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_synthesis_position ctx v

let rec read_font_synthesis_position t : font_synthesis_position =
  Cursor.enum_or_var "font-synthesis-position"
    [
      ("auto", (Auto : font_synthesis_position));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_synthesis_position t))
    t

let pp_font_variant_ligature ctx = function
  | Common_ligatures -> Pp.string ctx "common-ligatures"
  | No_common_ligatures -> Pp.string ctx "no-common-ligatures"
  | Discretionary_ligatures -> Pp.string ctx "discretionary-ligatures"
  | No_discretionary_ligatures -> Pp.string ctx "no-discretionary-ligatures"
  | Historical_ligatures -> Pp.string ctx "historical-ligatures"
  | No_historical_ligatures -> Pp.string ctx "no-historical-ligatures"
  | Contextual -> Pp.string ctx "contextual"
  | No_contextual -> Pp.string ctx "no-contextual"

let rec pp_font_variant_ligatures : font_variant_ligatures Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | None -> Pp.string ctx "none"
  | Ligatures ligatures ->
      Pp.list ~sep:Pp.space pp_font_variant_ligature ctx ligatures
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_ligatures ctx v

let read_font_variant_ligature t =
  Cursor.enum "font-variant-ligature"
    [
      ("common-ligatures", Common_ligatures);
      ("no-common-ligatures", No_common_ligatures);
      ("discretionary-ligatures", Discretionary_ligatures);
      ("no-discretionary-ligatures", No_discretionary_ligatures);
      ("historical-ligatures", Historical_ligatures);
      ("no-historical-ligatures", No_historical_ligatures);
      ("contextual", Contextual);
      ("no-contextual", No_contextual);
    ]
    t

let font_variant_ligature_slot = function
  | Common_ligatures | No_common_ligatures -> `Common
  | Discretionary_ligatures | No_discretionary_ligatures -> `Discretionary
  | Historical_ligatures | No_historical_ligatures -> `Historical
  | Contextual | No_contextual -> `Contextual

let has_duplicate_ligature_slot ligatures =
  List.exists
    (fun ligature ->
      let slot = font_variant_ligature_slot ligature in
      List.length
        (List.filter
           (fun other -> font_variant_ligature_slot other = slot)
           ligatures)
      > 1)
    ligatures

let rec read_font_variant_ligatures t : font_variant_ligatures =
  Cursor.enum_or_var "font-variant-ligatures"
    [
      ("normal", (Normal : font_variant_ligatures));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_ligatures t))
    ~default:(fun t ->
      match Cursor.many read_font_variant_ligature t with
      | [], _ -> Cursor.err_invalid t "font-variant-ligatures"
      | ligatures, _ ->
          if has_duplicate_ligature_slot ligatures then
            Cursor.err_invalid t "font-variant-ligatures";
          Ligatures ligatures)
    t

let rec pp_font_variant_caps : font_variant_caps Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"
  | All_small_caps -> Pp.string ctx "all-small-caps"
  | Petite_caps -> Pp.string ctx "petite-caps"
  | All_petite_caps -> Pp.string ctx "all-petite-caps"
  | Unicase -> Pp.string ctx "unicase"
  | Titling_caps -> Pp.string ctx "titling-caps"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_caps ctx v

let rec read_font_variant_caps t : font_variant_caps =
  Cursor.enum_or_var "font-variant-caps"
    [
      ("normal", (Normal : font_variant_caps));
      ("small-caps", Small_caps);
      ("all-small-caps", All_small_caps);
      ("petite-caps", Petite_caps);
      ("all-petite-caps", All_petite_caps);
      ("unicase", Unicase);
      ("titling-caps", Titling_caps);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_caps t))
    t

let rec pp_font_variant_position : font_variant_position Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_position ctx v

let rec read_font_variant_position t : font_variant_position =
  Cursor.enum_or_var "font-variant-position"
    [
      ("normal", (Normal : font_variant_position));
      ("sub", Sub);
      ("super", Super);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_position t))
    t

let pp_east_asian_feature ctx = function
  | Jis78 -> Pp.string ctx "jis78"
  | Jis83 -> Pp.string ctx "jis83"
  | Jis90 -> Pp.string ctx "jis90"
  | Jis04 -> Pp.string ctx "jis04"
  | Simplified -> Pp.string ctx "simplified"
  | Traditional -> Pp.string ctx "traditional"
  | Full_width -> Pp.string ctx "full-width"
  | Proportional_width -> Pp.string ctx "proportional-width"
  | Ruby -> Pp.string ctx "ruby"

let rec pp_font_variant_east_asian : font_variant_east_asian Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Features features ->
      Pp.list ~sep:Pp.space pp_east_asian_feature ctx features
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_east_asian ctx v

let read_east_asian_feature t =
  Cursor.enum "font-variant-east-asian-feature"
    [
      ("jis78", Jis78);
      ("jis83", Jis83);
      ("jis90", Jis90);
      ("jis04", Jis04);
      ("simplified", Simplified);
      ("traditional", Traditional);
      ("full-width", Full_width);
      ("proportional-width", Proportional_width);
      ("ruby", Ruby);
    ]
    t

let rec read_font_variant_east_asian t : font_variant_east_asian =
  let invalid_feature_set features =
    let variant_count = ref 0 in
    let width_count = ref 0 in
    let seen = ref [] in
    List.exists
      (fun feature ->
        let duplicate = List.mem feature !seen in
        seen := feature :: !seen;
        (match feature with
        | Jis78 | Jis83 | Jis90 | Jis04 | Simplified | Traditional ->
            incr variant_count
        | Full_width | Proportional_width -> incr width_count
        | Ruby -> ());
        duplicate || !variant_count > 1 || !width_count > 1)
      features
  in
  Cursor.enum_or_var "font-variant-east-asian"
    [
      ("normal", (Normal : font_variant_east_asian));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_east_asian t))
    ~default:(fun t ->
      match Cursor.many read_east_asian_feature t with
      | [], _ -> Cursor.err_invalid t "font-variant-east-asian"
      | features, _ when invalid_feature_set features ->
          Cursor.err_invalid t "font-variant-east-asian"
      | features, _ -> (Features features : font_variant_east_asian))
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
  if
    Cursor.is_done t || Cursor.peek_semicolon t
    || Cursor.peek_delim t = Some '!'
  then first
  else
    let second = read_overflow_single t in
    Cursor.ws t;
    if
      (not (Cursor.is_done t))
      && not (Cursor.peek_semicolon t || Cursor.peek_delim t = Some '!')
    then Cursor.expect_eof t;
    Overflow_pair (first, second)

module Cursor_prop = struct
  let read_keyword (t : Cursor.t) : cursor =
    Cursor.enum "cursor"
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
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      t

  let read_url_with_hotspot (t : Cursor.t) : string * (float * float) option =
    Cursor.ws t;
    let url =
      match Cursor.string_opt t with
      | Some s -> String.trim s
      | None -> Cursor.consume_remaining_as_string ~trim:true t
    in
    Cursor.ws t;
    let hotspot =
      if not (Cursor.is_done t) then (
        let x = Cursor.number t in
        Cursor.ws t;
        let y = Cursor.number t in
        Some (x, y))
      else None
    in
    (url, hotspot)

  let read_optional_hotspot (t : Cursor.t) : (float * float) option =
    Cursor.option
      (fun t ->
        let x = Cursor.number t in
        Cursor.ws t;
        let y = Cursor.number t in
        Cursor.ws t;
        if x < 0. || y < 0. then
          Cursor.err t "cursor hotspot coordinates cannot be negative"
        else (x, y))
      t

  let or_else a b = match a with Some _ -> a | None -> b

  let rec read_url_cursor (t : Cursor.t) : cursor =
    let (url, hotspot) : string * (float * float) option =
      (* Bare [url(foo.cur)] is a [Token.Url]; quoted [url("foo.cur")] is a
         [Func "url"] — handle both. *)
      match Cursor.url_opt t with
      | Some url -> (url, None)
      | None -> Cursor.call "url" t read_url_with_hotspot
    in
    Cursor.ws t;
    let hotspot = or_else (read_optional_hotspot t) hotspot in
    if not (Cursor.comma_opt t) then
      err_invalid_value t "cursor" "url without fallback keyword"
    else
      let fallback = read t in
      (Url (url, hotspot, fallback) : cursor)

  and read_var (t : Cursor.t) : cursor = Var (Values.read_var read t)

  and read (t : Cursor.t) : cursor =
    Cursor.ws t;
    Cursor.one_of [ read_url_cursor; read_var; read_keyword ] t
end

let read_cursor t : cursor = Cursor_prop.read t

let rec read_interactivity t : interactivity =
  Cursor.enum_or_var "interactivity"
    [
      ("auto", (Auto : interactivity));
      ("inert", Inert);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_interactivity t))
    t

let rec read_caret_animation t : caret_animation =
  Cursor.enum_or_var "caret-animation"
    [
      ("auto", (Auto : caret_animation));
      ("manual", Manual);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caret_animation t))
    t

let rec read_caret_shape t : caret_shape =
  Cursor.enum_or_var "caret-shape"
    [
      ("auto", (Auto : caret_shape));
      ("bar", Bar);
      ("block", Block);
      ("underscore", Underscore);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caret_shape t))
    t

let read_caret_animation_component t : caret_animation =
  Cursor.enum "caret-animation component"
    [ ("manual", (Manual : caret_animation)) ]
    t

let read_caret_shape_component t : caret_shape =
  Cursor.enum "caret-shape component"
    [
      ("auto", (Auto : caret_shape));
      ("bar", Bar);
      ("block", Block);
      ("underscore", Underscore);
    ]
    t

let read_caret_shorthand t : caret =
  let rec loop color animation shape count =
    if Cursor.is_done t || Cursor.peek_semicolon t then
      if count = 0 then Cursor.err_expected t "caret"
      else (Caret (color, animation, shape) : caret)
    else
      let try_each =
        let attempts =
          List.filter_map Fun.id
            [
              (if Option.is_none animation then
                 Some (fun t -> `Animation (read_caret_animation_component t))
               else None);
              (if Option.is_none color then
                 Some (fun t -> `Color (read_color t))
               else None);
              (if Option.is_none shape then
                 Some (fun t -> `Shape (read_caret_shape_component t))
               else None);
            ]
        in
        Cursor.one_of attempts t
      in
      match try_each with
      | `Color value -> loop (Some value) animation shape (count + 1)
      | `Animation value -> loop color (Some value) shape (count + 1)
      | `Shape value -> loop color animation (Some value) (count + 1)
  in
  loop None None None 0

let rec read_caret t : caret =
  Cursor.enum_or_var "caret"
    [
      ("inherit", (Inherit : caret));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_caret t))
    ~default:read_caret_shorthand t

let read_non_negative_duration t =
  match Values.read_duration_preserve_ms t with
  | Ms f when f < 0. -> Cursor.err_invalid t "negative duration"
  | S f when f < 0. -> Cursor.err_invalid t "negative duration"
  | duration -> duration

let rec read_interest_delay ?(longhand = false) t : interest_delay =
  Cursor.enum_or_var "interest-delay"
    [
      ("normal", (Normal : interest_delay));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var (read_interest_delay ~longhand) t))
    ~default:(fun t ->
      let at_most = if longhand then 1 else 2 in
      Durations
        (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most
           read_non_negative_duration t))
    t

let read_nav_scope t : nav_scope =
  Cursor.one_of
    [
      (fun t ->
        match Cursor.ident t with
        | "current" -> Current
        | "root" -> Root
        | ident -> Cursor.err_invalid t ("nav scope: " ^ ident));
      (fun t ->
        let name = Cursor.string t in
        if String.equal name "_self" then Cursor.err_invalid t "nav target"
        else Named name);
    ]
    t

let rec read_nav t : nav =
  Cursor.enum_or_var "nav"
    [
      ("auto", (Auto : nav));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_nav t))
    ~default:(fun t ->
      match Cursor.hash_opt t with
      | None -> Cursor.err_expected t "nav target"
      | Some target ->
          Cursor.ws t;
          let scope = Cursor.option read_nav_scope t in
          Target (target, scope))
    t

module Shadow = struct
  let read_lengths lengths =
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

  let read t =
    (* Drive [inset? && <length>{2,4} && <color>?] positionally so a trailing
       [var()] in the length run isn't greedily folded into the colour slot. *)
    let inset = ref false in
    let color : color option ref = ref (None : color option) in
    let try_inset () =
      if !inset then false
      else
        match
          Cursor.option
            (fun t ->
              Cursor.expect_string "inset" t;
              true)
            t
        with
        | Some _ ->
            inset := true;
            Cursor.ws t;
            true
        | None -> false
    in
    let try_color () =
      match !color with
      | Some _ -> false
      | None -> (
          match Cursor.option (fun t -> read_color t) t with
          | Some c ->
              color := Some c;
              Cursor.ws t;
              true
          | None -> false)
    in
    let _ : bool = try_inset () in
    let _ : bool = try_color () in
    let _ : bool = try_inset () in
    let lengths_rev = ref [] in
    let rec read_lengths_loop n =
      if n >= 4 then ()
      else
        match Cursor.option (fun t -> read_length t) t with
        | Some l ->
            lengths_rev := l :: !lengths_rev;
            Cursor.ws t;
            let _ : bool = try_inset () in
            read_lengths_loop (n + 1)
        | None -> ()
    in
    read_lengths_loop 0;
    let _ : bool = try_inset () in
    let _ : bool = try_color () in
    let _ : bool = try_inset () in
    let lengths = List.rev !lengths_rev in
    match read_lengths lengths with
    | Some (h_offset, v_offset, blur, spread) ->
        if
          blur = None && spread = None && !color = None && h_offset = Zero
          && v_offset = Zero
        then err_invalid_value t "shadow" "blur, spread, or color is required";
        let body = { h_offset; v_offset; blur; spread; color = !color } in
        (if !inset then Inset (Body body) else Shadow body : shadow)
    | None -> err_invalid_value t "shadow" "at least two lengths are required"
end

let rec read_shadow_single t : shadow =
  let read_var_shadow t : shadow = Var (read_var read_shadow_single t) in
  Cursor.ws t;
  (* inset var(--x): Shadow.read needs concrete offsets, so handle it here. *)
  let snap = Cursor.save t in
  let inset_var : shadow option =
    match Cursor.ident_opt t with
    | Some "inset" -> (
        Cursor.ws t;
        match Cursor.peek t with
        | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
            Some (Inset (Var (read_var read_shadow_single t)))
        | _ -> Option.None)
    | _ -> Option.None
  in
  match inset_var with
  | Some shadow -> shadow
  | Option.None ->
      Cursor.restore t snap;
      Cursor.enum_or_calls "shadow"
        [
          ("none", None);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~calls:[ ("var", read_var_shadow) ]
        ~default:Shadow.read t

let read_shadow t : shadow =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_shadow_single t with
  | [ x ] -> x
  | l -> List l

module Transform = struct
  let read_translate_x t =
    Cursor.call "translatex" t (fun t -> Translate_x (read_length t))

  let read_translate_y t =
    Cursor.call "translatey" t (fun t -> Translate_y (read_length t))

  let read_translate_z t =
    Cursor.call "translatez" t (fun t -> Translate_z (read_length t))

  let read_translate3d t =
    Cursor.call "translate3d" t (fun t ->
        let x, y, z =
          Cursor.(triple ~sep:comma read_length read_length read_length) t
        in
        Translate_3d (x, y, z))

  let read_translate t =
    Cursor.call "translate" t (fun t ->
        let x = read_length t in
        let y =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_length t)
            t
        in
        (Translate (x, y) : transform))

  let read_rotate_x t =
    Cursor.call "rotatex" t (fun t -> Rotate_x (read_angle t))

  let read_rotate_y t =
    Cursor.call "rotatey" t (fun t -> Rotate_y (read_angle t))

  let read_rotate_z t =
    Cursor.call "rotatez" t (fun t -> Rotate_z (read_angle t))

  let read_rotate t : transform =
    Cursor.call "rotate" t (fun t ->
        Cursor.one_of
          [
            (fun t ->
              let x = Cursor.number t in
              Cursor.ws t;
              let y = Cursor.number t in
              Cursor.ws t;
              let z = Cursor.number t in
              Cursor.ws t;
              let angle = read_angle t in
              (Rotate_axis (x, y, z, angle) : transform));
            (fun t -> (Rotate (read_angle t) : transform));
          ]
          t)

  let read_scale_x t =
    Cursor.call "scalex" t (fun t -> Scale_x (Values.read_number_percentage t))

  let read_scale_y t =
    Cursor.call "scaley" t (fun t -> Scale_y (Values.read_number_percentage t))

  let read_scale_z t =
    Cursor.call "scalez" t (fun t -> Scale_z (Values.read_number_percentage t))

  let read_rotate3d t =
    Cursor.call "rotate3d" t (fun t ->
        let x, y, z = Cursor.(triple ~sep:comma number number number) t in
        Cursor.comma t;
        let angle = read_angle t in
        Rotate_3d (x, y, z, angle))

  let read_scale3d t =
    Cursor.call "scale3d" t (fun t ->
        let x, y, z =
          Cursor.(
            triple ~sep:comma Values.read_number_percentage
              Values.read_number_percentage Values.read_number_percentage)
            t
        in
        Scale_3d (x, y, z))

  let read_scale t : transform =
    Cursor.call "scale" t (fun t ->
        let x = Values.read_number_percentage t in
        let y =
          Cursor.option
            (fun t ->
              let has_comma = Cursor.comma_opt t in
              Cursor.ws t;
              (has_comma, Values.read_number_percentage t))
            t
        in
        match y with
        | None ->
            Cursor.ws t;
            Cursor.expect_eof t;
            (Scale (x, None) : transform)
        | Some (true, y) ->
            Cursor.ws t;
            Cursor.expect_eof t;
            Scale (x, Some y)
        | Some (false, y) ->
            Cursor.ws t;
            Cursor.expect_eof t;
            Scale_space (x, y))

  let read_skew_x t = Cursor.call "skewx" t (fun t -> Skew_x (read_angle t))
  let read_skew_y t = Cursor.call "skewy" t (fun t -> Skew_y (read_angle t))

  let read_skew t =
    Cursor.call "skew" t (fun t ->
        let x = read_angle t in
        let y =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_angle t)
            t
        in
        Skew (x, y))

  let read_matrix t =
    Cursor.call "matrix" t (fun t ->
        match Cursor.list ~sep:Cursor.comma Cursor.number t with
        | [ a; b; c; d; e; f ] -> Matrix (a, b, c, d, e, f)
        | _ -> err_invalid_value t "matrix" "expected 6 arguments")

  let read_matrix3d t =
    Cursor.call "matrix3d" t (fun t ->
        match Cursor.list ~sep:Cursor.comma Cursor.number t with
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

  let read_perspective t : transform =
    Cursor.call "perspective" t (fun inner ->
        Cursor.ws inner;
        let len = read_length inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        (Perspective len : transform))

  let parsers =
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
      ("perspective", read_perspective);
    ]
end

let is_transform_none (value : transform) =
  match value with None -> true | _ -> false

let rec read_transform t : transform =
  (* Add var support to the parsers list *)
  let read_var_transform t : transform =
    Var (Values.read_var read_transform_value t)
  in
  Cursor.enum_or_calls "transform"
    [
      ("none", (None : transform));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:(("var", read_var_transform) :: Transform.parsers)
    t

and read_transform_value t : transform =
  match read_transforms t with [ x ] -> x | xs -> (List xs : transform)

and read_transforms t : transform list =
  let transforms, error_opt = Cursor.many read_transform t in
  if List.length transforms = 0 then
    match error_opt with
    | Some msg -> Cursor.err_invalid t ("transform: " ^ msg)
    | None -> Cursor.err_invalid t "transform value"
  else if List.length transforms > 1 && List.exists is_transform_none transforms
  then Cursor.err_invalid t "transform none cannot be combined"
  else transforms

let pp_opt_space pp ctx = function
  | Some v ->
      Pp.space ctx ();
      pp ctx v
  | None -> ()

let pp_keyword s ctx = Pp.string ctx s

(* Read only the body of a url(...) call when used inside enum_calls. The
   surrounding function name and parentheses are handled by Cursor. *)
let is_zero_length : length -> bool = function
  | Zero
  | Px 0.
  | Rem 0.
  | Em 0.
  | Ex 0.
  | Cap 0.
  | Ic 0.
  | Ric 0.
  | Rlh 0.
  | Cm 0.
  | Mm 0.
  | Q 0.
  | In 0.
  | Pt 0.
  | Pc 0.
  | Pct 0.
  | Vw 0.
  | Vh 0.
  | Vmin 0.
  | Vmax 0.
  | Vi 0.
  | Vb 0.
  | Dvh 0.
  | Dvw 0.
  | Dvmin 0.
  | Dvmax 0.
  | Lvh 0.
  | Lvw 0.
  | Lvmin 0.
  | Lvmax 0.
  | Svh 0.
  | Svw 0.
  | Svmin 0.
  | Svmax 0.
  | Cqw 0.
  | Cqh 0.
  | Cqi 0.
  | Cqb 0.
  | Cqmin 0.
  | Cqmax 0.
  | Dimension { value = 0.; _ } ->
      true
  | _ -> false

let pp_color_after_length ctx color =
  Pp.space ctx ();
  pp_color ctx color

(* Faithful: a default-zero spread is dropped in [normalize_shadow], not here.
   [Some Zero] re-parses differently from [None] ([0 1px 3px 0] vs [0 1px 3px]),
   so collapsing it is a node-changing fold that belongs in the AST normalize
   pass, leaving pp a pure serialiser. *)
let pp_shadow_spread _ctx (spread : length option) : length option = spread

let pp_shadow_body ctx { h_offset; v_offset; blur; spread; color } =
  pp_length ctx h_offset;
  Pp.space ctx ();
  pp_length ctx v_offset;
  let spread = pp_shadow_spread ctx spread in
  pp_opt_space pp_length ctx blur;
  pp_opt_space pp_length ctx spread;
  match color with Some c -> pp_color_after_length ctx c | None -> ()

(* The [var(--name,)] prefix stands in for the optional [inset] keyword (a var
   resolving to [inset] must read [inset 0 ...], never [inset0 ...]), so the
   separator after it is load-bearing. *)
let pp_inset_toggle ctx ~name ~no_fallback =
  Pp.string ctx "var(--";
  Pp.string ctx name;
  if no_fallback then Pp.string ctx ")"
  else (
    (* Empty fallback: var(--name, ) in pretty, var(--name,) in minified. *)
    Pp.char ctx ',';
    Pp.space_if_pretty ctx ();
    Pp.space_if_pretty ctx ();
    Pp.string ctx ")")

let rec pp_shadow : shadow Pp.t =
 fun ctx -> function
  | Shadow body -> pp_shadow_body ctx body
  | Inset (Body body) ->
      Pp.string ctx "inset";
      Pp.space ctx ();
      pp_shadow_body ctx body
  | Inset (Var v) ->
      Pp.string ctx "inset";
      Pp.space ctx ();
      pp_var pp_shadow ctx v
  | Inset (Toggle { name; no_fallback; body }) ->
      pp_inset_toggle ctx ~name ~no_fallback;
      Pp.space ctx ();
      pp_shadow_body ctx body
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_shadow ctx v
  | List shadows -> Pp.list ~sep:Pp.comma pp_shadow ctx shadows

(* pp_box_shadow removed - use pp_shadow with List constructor *)

let pp_hue_interpolation_method ctx = function
  | Shorter -> Pp.string ctx "shorter hue"
  | Longer -> Pp.string ctx "longer hue"
  | Increasing -> Pp.string ctx "increasing hue"
  | Decreasing -> Pp.string ctx "decreasing hue"

let pp_polar_with_hue ctx space (hue : hue_interpolation_method option) =
  Pp.string ctx "in ";
  Pp.string ctx space;
  match hue with
  | None -> ()
  | Some hue ->
      Pp.space ctx ();
      pp_hue_interpolation_method ctx hue

let rec pp_color_interpolation : color_interpolation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_color_interpolation ctx v
  | In_oklab -> Pp.string ctx "in oklab"
  | In_oklch hue -> pp_polar_with_hue ctx "oklch" hue
  | In_srgb -> Pp.string ctx "in srgb"
  | In_hsl hue -> pp_polar_with_hue ctx "hsl" hue
  | In_lab -> Pp.string ctx "in lab"
  | In_lch hue -> pp_polar_with_hue ctx "lch" hue

(* CSS Color 5 section 12: after a polar color space (lch / oklch / hsl / hwb),
   the [<color-interpolation-method>] may carry a trailing
   [<hue-interpolation-method>] followed by [hue]. *)
let read_hue_interpolation_method t =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some (("shorter" | "longer" | "increasing" | "decreasing") as kw) -> (
      Cursor.ws t;
      match Cursor.ident_opt t with
      | Some "hue" ->
          Some
            (match kw with
            | "shorter" -> Shorter
            | "longer" -> Longer
            | "increasing" -> Increasing
            | "decreasing" -> Decreasing
            | _ -> assert false)
      | _ ->
          Cursor.restore t snap;
          None)
  | _ ->
      Cursor.restore t snap;
      None

let read_color_interpolation (t : Cursor.t) : color_interpolation =
  Cursor.with_context t "color-interpolation" (fun () ->
      Cursor.expect_string "in" t;
      (* At the component-value level, [in oklab] lexes as two separate idents;
         [inoklab] lexes as a single ident and would fail [expect_string "in"]
         above, so no extra whitespace check is needed here. *)
      let space = Cursor.ident t in
      let hue () =
        Cursor.ws t;
        read_hue_interpolation_method t
      in
      match space with
      | "oklab" -> In_oklab
      | "oklch" -> In_oklch (hue ())
      | "srgb" -> In_srgb
      | "hsl" -> In_hsl (hue ())
      | "lab" -> In_lab
      | "lch" -> In_lch (hue ())
      | _ -> Cursor.err_invalid t "color-interpolation")

let rec pp_gradient_direction : gradient_direction Pp.t =
 fun ctx -> function
  | Default_direction -> Pp.string ctx "to bottom"
  | To_top -> Pp.string ctx "to top"
  | To_top_right -> Pp.string ctx "to top right"
  | To_right -> Pp.string ctx "to right"
  | To_bottom_right -> Pp.string ctx "to bottom right"
  | To_bottom -> Pp.string ctx "to bottom"
  | To_bottom_left -> Pp.string ctx "to bottom left"
  | To_left -> Pp.string ctx "to left"
  | To_top_left -> Pp.string ctx "to top left"
  | Angle a -> pp_angle ctx a
  | With_interpolation (Default_direction, interp) ->
      (* default direction is omitted: [in <interp>], not [to bottom in
         <interp>] *)
      pp_color_interpolation ctx interp
  | With_interpolation (dir, interp) ->
      pp_gradient_direction ctx dir;
      (* After calc() or var(), the closing ) is a delimiter token so no space
         is needed before "in oklab" in minified mode. After keywords like "to
         right", a space is always needed to separate tokens. *)
      (match dir with
      | Angle (Calc _) | Var _ -> Pp.sp ctx ()
      | _ -> Pp.space ctx ());
      pp_color_interpolation ctx interp
  | Var v -> pp_var pp_gradient_direction ctx v

let rec pp_webkit_gradient_direction : gradient_direction Pp.t =
 fun ctx -> function
  | Default_direction -> Pp.string ctx "bottom"
  | To_top -> Pp.string ctx "top"
  | To_top_right -> Pp.string ctx "top right"
  | To_right -> Pp.string ctx "right"
  | To_bottom_right -> Pp.string ctx "bottom right"
  | To_bottom -> Pp.string ctx "bottom"
  | To_bottom_left -> Pp.string ctx "bottom left"
  | To_left -> Pp.string ctx "left"
  | To_top_left -> Pp.string ctx "top left"
  | Angle a -> pp_angle ctx a
  | With_interpolation (dir, interp) ->
      pp_webkit_gradient_direction ctx dir;
      Pp.space ctx ();
      pp_color_interpolation ctx interp
  | Var v -> pp_var pp_webkit_gradient_direction ctx v

let rec pp_radial_shape : radial_shape Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_radial_shape ctx v
  | Circle -> Pp.string ctx "circle"
  | Ellipse -> Pp.string ctx "ellipse"

let rec pp_radial_size : radial_size Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_radial_size ctx v
  | Closest_side -> Pp.string ctx "closest-side"
  | Farthest_side -> Pp.string ctx "farthest-side"
  | Closest_corner -> Pp.string ctx "closest-corner"
  | Farthest_corner -> Pp.string ctx "farthest-corner"
  | Circle_radius l -> pp_length ctx l
  | Ellipse_radii (a, b) ->
      pp_length_percentage ~always:true ctx a;
      Pp.space ctx ();
      pp_length_percentage ~always:true ctx b

(* CSS Images 4 sec. 3.5.3 color-stop fixup: an omitted position on the first
   stop defaults to [0%], on the last stop to [100%]. The reverse holds for
   minify: drop an explicit [Pct 0.] from the first stop and [Pct 100.] from the
   last when there's no second-position carrier; the resolved color stop list
   stays identical. *)
let canonicalise_gradient_stops stops =
  let strip_pos stop =
    match stop with
    | Color_percentage (c, _, _) -> Color_percentage (c, None, None)
    | _ -> stop
  in
  let trailing_default stop expected =
    match stop with
    | Color_percentage (_, Some (Pct p), None) when p = expected -> true
    | _ -> false
  in
  let rec strip_last = function
    | [] -> []
    | [ last ] when trailing_default last 100. -> [ strip_pos last ]
    | x :: rest -> x :: strip_last rest
  in
  match stops with
  | [] -> []
  | first :: rest when trailing_default first 0. ->
      strip_pos first :: strip_last rest
  | _ -> strip_last stops

let rec pp_filter : filter Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Blur l -> Pp.call "blur" pp_length ctx l
  | Brightness n ->
      Pp.call "brightness" (pp_number_percentage ~always:true) ctx n
  | Contrast n -> Pp.call "contrast" (pp_number_percentage ~always:true) ctx n
  | Drop_shadow s -> Pp.call "drop-shadow" pp_shadow ctx s
  | Grayscale n -> Pp.call "grayscale" (pp_number_percentage ~always:true) ctx n
  | Hue_rotate (Deg 0.) when Pp.minified ctx -> Pp.string ctx "hue-rotate()"
  | Hue_rotate a -> Pp.call "hue-rotate" pp_angle ctx a
  | Invert n -> Pp.call "invert" (pp_number_percentage ~always:true) ctx n
  | Opacity n -> Pp.call "opacity" (pp_number_percentage ~always:true) ctx n
  | Saturate n -> Pp.call "saturate" (pp_number_percentage ~always:true) ctx n
  | Sepia n -> Pp.call "sepia" (pp_number_percentage ~always:true) ctx n
  | Url url -> Pp.url ctx url
  | List filters ->
      let sep = if Pp.minified ctx then Pp.nop else Pp.space in
      Pp.list ~sep pp_filter ctx filters
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_filter ctx v

let pp_position_offset ctx (lp : Values.length_percentage) =
  pp_length_percentage ~always:true ctx lp

let rec pp_position_value : position_value Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Center -> Pp.string ctx "center"
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Left_top -> Pp.string ctx "left top"
  | Left_center -> Pp.string ctx "left center"
  | Left_bottom -> Pp.string ctx "left bottom"
  | Right_top -> Pp.string ctx "right top"
  | Right_center -> Pp.string ctx "right center"
  | Right_bottom -> Pp.string ctx "right bottom"
  | Center_top -> Pp.string ctx "center top"
  | Center_bottom -> Pp.string ctx "center bottom"
  | Top_left -> Pp.string ctx "top left"
  | Top_right -> Pp.string ctx "top right"
  | Bottom_left -> Pp.string ctx "bottom left"
  | Bottom_right -> Pp.string ctx "bottom right"
  | XY (a, b) ->
      pp_length ctx a;
      Pp.space ctx ();
      pp_length ctx b
  | Single l -> pp_length ctx l
  | Edge_offset_axis (edge, offset, axis) ->
      Pp.string ctx edge;
      Pp.space ctx ();
      pp_position_offset ctx offset;
      Pp.space ctx ();
      Pp.string ctx axis
  | Axis_edge_offset (axis, edge, offset) ->
      Pp.string ctx axis;
      Pp.space ctx ();
      Pp.string ctx edge;
      Pp.space ctx ();
      pp_length ctx offset
  | Edge_offset_edge_offset (edge1, offset1, edge2, offset2) ->
      Pp.string ctx edge1;
      Pp.space ctx ();
      pp_position_offset ctx offset1;
      Pp.space ctx ();
      Pp.string ctx edge2;
      Pp.space ctx ();
      pp_position_offset ctx offset2
  | Var v -> pp_var pp_position_value ctx v

(* CSS Images 3 sec. 3.2.1: an omitted shape defaults to [ellipse], an omitted
   size to [farthest-corner], and an omitted position to [center]. Under minify,
   drop a component that equals its default - the resolved gradient stays
   identical. *)
let radial_shape_is_default : radial_shape -> bool = function
  | Ellipse -> true
  | _ -> false

let radial_size_is_default : radial_size -> bool = function
  | Farthest_corner -> true
  | _ -> false

let position_value_is_center : position_value -> bool = function
  | Center -> true
  | XY (Pct 50., Pct 50.) -> true
  | Single (Pct 50.) -> true
  | _ -> false

let radial_shape_kept (s : radial_shape option) : radial_shape option =
  match s with Some s when radial_shape_is_default s -> None | s -> s

let radial_size_kept (s : radial_size option) : radial_size option =
  match s with Some s when radial_size_is_default s -> None | s -> s

let radial_position_kept (p : position_value option) : position_value option =
  match p with Some p when position_value_is_center p -> None | p -> p

let pp_radial_gradient_config : radial_gradient_config Pp.t =
 fun ctx config ->
  let shape = config.shape in
  let size = config.size in
  let position = config.position in
  let has_output = ref false in
  let emit_space_if_needed () =
    if !has_output then Pp.space ctx () else has_output := true
  in
  Option.iter
    (fun i ->
      pp_color_interpolation ctx i;
      has_output := true)
    config.interpolation;
  Option.iter
    (fun s ->
      emit_space_if_needed ();
      pp_radial_shape ctx s)
    shape;
  Option.iter
    (fun s ->
      emit_space_if_needed ();
      pp_radial_size ctx s)
    size;
  Option.iter
    (fun p ->
      emit_space_if_needed ();
      Pp.string ctx "at";
      Pp.space ctx ();
      pp_position_value ctx p)
    position

let pp_conic_gradient_config : conic_gradient_config Pp.t =
 fun ctx config ->
  let has_output = ref false in
  (match config.interpolation with
  | Some i ->
      pp_color_interpolation ctx i;
      has_output := true
  | None -> ());
  (match config.angle with
  | Some a ->
      if !has_output then Pp.space ctx ();
      Pp.string ctx "from";
      Pp.space ctx ();
      pp_angle ctx a;
      has_output := true
  | None -> ());
  match config.position with
  | Some p ->
      if !has_output then Pp.space ctx ();
      Pp.string ctx "at";
      Pp.space ctx ();
      pp_position_value ctx p
  | None -> ()

let rec pp_gradient_position : gradient_position Pp.t =
 fun ctx -> function
  | Linear_position dir -> pp_gradient_direction ctx dir
  | Radial_position config -> pp_radial_gradient_config ctx config
  | Conic_position config -> pp_conic_gradient_config ctx config
  | Var v -> pp_var pp_gradient_position ctx v

let rec pp_gradient_stop : gradient_stop Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_gradient_stop ctx v
  | Color_percentage (c, pos1_opt, pos2_opt) -> (
      let rendered_color =
        Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
          pp_color c
      in
      Pp.string ctx rendered_color;
      match pos1_opt with
      | None -> ()
      | Some pos1 -> (
          (* CSS Syntax 3 §4: ident- and hash-typed colours absorb the following
             digit/hex into the same token ([red0%] -> ident [red0] + [%]), so
             the separator is mandatory there; and when the stop lives in a
             custom-property token stream, the whitespace token between a
             function-shaped colour and its position is part of the value a
             var() substitution receives, so it is never elided either. *)
          Pp.space ctx ();
          pp_length_percentage ~always:true ctx pos1;
          match pos2_opt with
          | None -> ()
          | Some pos2 ->
              Pp.space ctx ();
              pp_length_percentage ~always:true ctx pos2))
  | Color_length (c, len1_opt, len2_opt) -> (
      pp_color ctx c;
      match len1_opt with
      | None -> ()
      | Some len1 -> (
          Pp.space ctx ();
          pp_length ctx len1;
          match len2_opt with
          | None -> ()
          | Some len2 ->
              Pp.space ctx ();
              pp_length ctx len2))
  | Length len -> pp_length ctx len
  | Channel channel -> pp_channel ctx channel
  | Percentage pct -> pp_percentage ctx pct
  | List stops ->
      Pp.list ~sep:Pp.comma pp_gradient_stop ctx
        (if Pp.minified ctx then canonicalise_gradient_stops stops else stops)
  | Position pos -> pp_gradient_position ctx pos
  | Direction dir -> pp_gradient_direction ctx dir

let pp_webkit_gradient_point ctx = function
  | Webkit_gradient.Left_top ->
      Pp.string ctx (if Pp.minified ctx then "0 0" else "left top")
  | Webkit_gradient.Left_bottom ->
      Pp.string ctx (if Pp.minified ctx then "0 100%" else "left bottom")
  | Webkit_gradient.Center ->
      Pp.string ctx (if Pp.minified ctx then "50% 50%" else "center center")
  | Webkit_gradient.Position position -> pp_position_value ctx position

let pp_webkit_gradient_position ctx (pct : percentage) =
  match (Pp.minified ctx, pct) with
  | true, Pct value -> Pp.float ctx (value /. 100.)
  | _ -> pp_percentage ctx pct

let pp_webkit_gradient_stop ctx = function
  | Webkit_gradient.From color -> Pp.call "from" pp_color ctx color
  | Webkit_gradient.To color -> Pp.call "to" pp_color ctx color
  | Webkit_gradient.Color_stop (Pct 0., color)
  | Webkit_gradient.Color_stop (Num 0., color) ->
      Pp.call "from" pp_color ctx color
  | Webkit_gradient.Color_stop (Pct 100., color)
  | Webkit_gradient.Color_stop (Num 1., color) ->
      Pp.call "to" pp_color ctx color
  | Webkit_gradient.Color_stop (position, color) ->
      Pp.call "color-stop"
        (fun ctx (position, color) ->
          pp_webkit_gradient_position ctx position;
          Pp.comma ctx ();
          pp_color ctx color)
        ctx (position, color)

let pp_webkit_gradient : Webkit_gradient.t Pp.t =
 fun ctx -> function
  | Webkit_gradient.Linear { start; finish; stops } ->
      Pp.call "-webkit-gradient"
        (fun ctx (start, finish, stops) ->
          Pp.string ctx "linear";
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx start;
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx finish;
          Pp.comma ctx ();
          Pp.list ~sep:Pp.comma pp_webkit_gradient_stop ctx stops)
        ctx (start, finish, stops)
  | Webkit_gradient.Radial
      { inner_center; inner_radius; outer_center; outer_radius; stops } ->
      Pp.call "-webkit-gradient"
        (fun ctx (inner_center, inner_radius, outer_center, outer_radius, stops)
           ->
          Pp.string ctx "radial";
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx inner_center;
          Pp.comma ctx ();
          Pp.float ctx inner_radius;
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx outer_center;
          Pp.comma ctx ();
          Pp.float ctx outer_radius;
          Pp.comma ctx ();
          Pp.list ~sep:Pp.comma pp_webkit_gradient_stop ctx stops)
        ctx
        (inner_center, inner_radius, outer_center, outer_radius, stops)

let pp_quoted_url quote ctx url =
  Pp.string ctx "url(";
  Pp.quoted_string ~quote ctx url;
  Pp.char ctx ')'

let conic_gradient_has_config (config : conic_gradient_config) =
  config.angle <> None || config.position <> None
  || config.interpolation <> None

let pp_conic_gradient_named name ctx (config, stops) =
  Pp.call name
    (fun ctx (config, stops) ->
      if conic_gradient_has_config config then (
        pp_conic_gradient_config ctx config;
        match stops with [] -> () | _ -> Pp.comma ctx ());
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (config, stops)

let pp_linear_gradient_named name ctx (dir, stops) =
  Pp.call name
    (fun ctx (dir, stops) ->
      (* CSS Images 4 §5.1: the default linear-gradient direction is [to
         bottom], equivalent to [180deg]; both spellings can be elided. *)
      let is_default_direction = function
        | Default_direction -> true
        | _ -> false
      in
      let head : [ `Skip | `Direction | `Interp_only of color_interpolation ] =
        match dir with
        | dir when is_default_direction dir -> `Skip
        | With_interpolation (inner, interp) when is_default_direction inner ->
            `Interp_only interp
        | _ -> `Direction
      in
      (match head with
      | `Skip -> ()
      | `Direction -> (
          pp_gradient_direction ctx dir;
          match stops with [] -> () | _ -> Pp.comma ctx ())
      | `Interp_only interp -> (
          pp_color_interpolation ctx interp;
          match stops with [] -> () | _ -> Pp.comma ctx ()));
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (dir, stops)

let pp_webkit_linear_gradient_named name ctx (dir, stops) =
  Pp.call name
    (fun ctx (dir, stops) ->
      let print_direction =
        match dir with Default_direction -> false | _ -> true
      in
      if print_direction then (
        pp_webkit_gradient_direction ctx dir;
        match stops with [] -> () | _ -> Pp.comma ctx ());
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (dir, stops)

let pp_radial_gradient_named name ctx (config, stops) =
  Pp.call name
    (fun ctx (config, stops) ->
      let has_config =
        config.shape <> None || config.size <> None || config.position <> None
        || config.interpolation <> None
      in
      if has_config then (
        pp_radial_gradient_config ctx config;
        match stops with [] -> () | _ -> Pp.comma ctx ());
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (config, stops)

let pp_image_set_option ctx { source; resolution; mime_type } =
  (match source with Url u -> Pp.url ctx u | String s -> Pp.quoted ctx s);
  Option.iter
    (fun mime ->
      Pp.space ctx ();
      Pp.string ctx "type(";
      Pp.quoted ctx mime;
      Pp.char ctx ')')
    mime_type;
  Option.iter
    (fun res ->
      Pp.sp ctx ();
      Pp.string ctx res)
    resolution

let rec pp_background_image : background_image Pp.t =
 fun ctx (image : background_image) ->
  match image with
  | Url url -> Pp.url ctx url
  | Quoted (url, quote) ->
      if Pp.minified ctx then Pp.url ctx url else pp_quoted_url quote ctx url
  | Linear_gradient (dir, stops) ->
      pp_linear_gradient_named "linear-gradient" ctx (dir, stops)
  | Linear_gradient_var var_ref ->
      Pp.call "linear-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_linear_gradient (dir, stops) ->
      pp_linear_gradient_named "repeating-linear-gradient" ctx (dir, stops)
  | Radial_gradient (config, stops) ->
      pp_radial_gradient_named "radial-gradient" ctx (config, stops)
  | Radial_gradient_var var_ref ->
      Pp.call "radial-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "repeating-radial-gradient" ctx (config, stops)
  | Conic_gradient (config, stops) ->
      pp_conic_gradient_named "conic-gradient" ctx (config, stops)
  | Conic_gradient_var var_ref ->
      Pp.call "conic-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_conic_gradient (config, stops) ->
      pp_conic_gradient_named "repeating-conic-gradient" ctx (config, stops)
  | Webkit_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-webkit-linear-gradient" ctx (dir, stops)
  | Webkit_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-webkit-repeating-linear-gradient" ctx
        (dir, stops)
  | Webkit_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-webkit-radial-gradient" ctx (config, stops)
  | Webkit_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-webkit-repeating-radial-gradient" ctx
        (config, stops)
  | Moz_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-moz-linear-gradient" ctx (dir, stops)
  | Moz_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-moz-repeating-linear-gradient" ctx
        (dir, stops)
  | Moz_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-moz-radial-gradient" ctx (config, stops)
  | Moz_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-moz-repeating-radial-gradient" ctx
        (config, stops)
  | O_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-o-linear-gradient" ctx (dir, stops)
  | O_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-o-repeating-linear-gradient" ctx
        (dir, stops)
  | O_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-o-radial-gradient" ctx (config, stops)
  | O_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-o-repeating-radial-gradient" ctx (config, stops)
  | Image_set options ->
      Pp.call "image-set"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_image_set_option ctx os)
        ctx options
  | Webkit_image_set options ->
      Pp.call "-webkit-image-set"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_image_set_option ctx os)
        ctx options
  | Cross_fade options ->
      Pp.call "cross-fade"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_cross_fade_option ctx os)
        ctx options
  | Webkit_gradient gradient -> pp_webkit_gradient ctx gradient
  | Var v -> pp_var pp_background_image ctx v
  | List images -> Pp.list ~sep:Pp.comma pp_background_image ctx images
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

and pp_cross_fade_option ctx { image; percent } =
  pp_background_image ctx image;
  Option.iter
    (fun pct ->
      Pp.space ctx ();
      Values.pp_percentage ~always:true ctx pct)
    percent

let is_font_family_ident_word s =
  let len = String.length s in
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
    | _ -> false
  in
  len > 0
  && (is_alpha s.[0] || s.[0] = '_' || s.[0] = '-')
  && (not (len >= 2 && s.[0] = '-' && is_digit s.[1]))
  && String.for_all is_ident_char s

let pp_font_family_name ctx s =
  (* A multi-word named family unquotes under minify (shorter, valid), but the
     CSSOM-canonical serialization quotes it, so enforce_spec keeps the
     quotes. *)
  if Pp.minified ctx && not ctx.Pp.enforce_spec then Pp.string ctx s
  else (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')

let can_unquote_font_family_name s =
  match String.split_on_char ' ' s with
  | _ :: _ :: _ as words ->
      (* CSS Fonts 4 §4.1: a [<family-name>] formed of two or more
         [<custom-ident>]s is unambiguous - none of its words can be picked up
         as a property-level CSS-wide keyword once the parser has committed to a
         multi-token value. So [inherit test] / [revert serif] etc. round-trip
         unquoted, just like [Times New Roman]. *)
      List.for_all is_font_family_ident_word words
  | _ -> false

(* Walk a component stream and rewrite each [<string>] token whose content is a
   multi-word identifier sequence (the [can_unquote_font_family_name] guard) as
   an explicit [<ident>] sequence. Used by the [@property]-registered custom
   property promotion path when the registered syntax accepts [<custom-ident>+]
   - the two forms ([custom-ident>+] vs [<string>]) are spec-equivalent there
   (CSS Fonts 4 sec. 15.3), so the rewrite produces a single canonical AST. The
   guard's "two or more words" rule avoids the CSS-wide-keyword trap (a quoted
   ["inherit"] never collapses to the bare keyword). *)
let unquote_font_family_strings components =
  let changed = ref false in
  let words_of s =
    String.split_on_char ' ' s |> List.filter (fun w -> w <> "")
  in
  let rec interleave loc = function
    | [] -> []
    | [ w ] -> [ Component.Preserved (Token.v ~kind:(Token.Ident w) ~loc) ]
    | w :: rest ->
        Component.Preserved (Token.v ~kind:(Token.Ident w) ~loc)
        :: Component.Preserved (Token.v ~kind:Token.Whitespace ~loc)
        :: interleave loc rest
  in
  let result =
    List.concat_map
      (fun c ->
        match c with
        | Component.Preserved { kind = Token.String { value; _ }; loc }
          when can_unquote_font_family_name value ->
            changed := true;
            interleave loc (words_of value)
        | _ -> [ c ])
      components
  in
  if !changed then result else components

let is_generic_family : font_family -> bool = function
  | Sans_serif | Serif | Monospace | Cursive | Fantasy | System_ui
  | Ui_sans_serif | Ui_serif | Ui_monospace | Ui_rounded | Emoji | Math
  | Fangsong ->
      true
  | _ -> false

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
  (* Platform-specific fonts. Multi-word names emit unquoted under minify (CSS
     Fonts 4 sec. 4.1: a [<family-name>] of two or more [<custom-ident>] words
     parses without quotes and is the shorter spelling). Pretty mode keeps the
     quoted form for readability. *)
  | Helvetica -> Pp.string ctx "Helvetica"
  | Helvetica_neue -> pp_font_family_name ctx "Helvetica Neue"
  | Arial -> Pp.string ctx "Arial"
  | Verdana -> Pp.string ctx "Verdana"
  | Tahoma -> Pp.string ctx "Tahoma"
  | Trebuchet_ms -> pp_font_family_name ctx "Trebuchet MS"
  | Times_new_roman -> pp_font_family_name ctx "Times New Roman"
  | Times -> Pp.string ctx "Times"
  | Georgia -> Pp.string ctx "Georgia"
  | Cambria -> Pp.string ctx "Cambria"
  | Garamond -> Pp.string ctx "Garamond"
  | Courier_new -> pp_font_family_name ctx "Courier New"
  | Courier -> Pp.string ctx "Courier"
  | Lucida_console -> pp_font_family_name ctx "Lucida Console"
  | SF_pro -> pp_font_family_name ctx "SF Pro"
  | SF_pro_display -> pp_font_family_name ctx "SF Pro Display"
  | SF_pro_text -> pp_font_family_name ctx "SF Pro Text"
  | SF_mono -> pp_font_family_name ctx "SF Mono"
  | NY -> pp_font_family_name ctx "New York"
  | Segoe_ui -> pp_font_family_name ctx "Segoe UI"
  | Segoe_ui_emoji -> pp_font_family_name ctx "Segoe UI Emoji"
  | Segoe_ui_symbol -> pp_font_family_name ctx "Segoe UI Symbol"
  | Apple_color_emoji -> pp_font_family_name ctx "Apple Color Emoji"
  | Noto_color_emoji -> pp_font_family_name ctx "Noto Color Emoji"
  | Android_emoji -> pp_font_family_name ctx "Android Emoji"
  | Twemoji_mozilla -> pp_font_family_name ctx "Twemoji Mozilla"
  (* Developer fonts *)
  | Menlo -> Pp.string ctx "Menlo"
  | Monaco -> Pp.string ctx "Monaco"
  | Consolas -> Pp.string ctx "Consolas"
  | Liberation_mono -> pp_font_family_name ctx "Liberation Mono"
  | SFMono_regular -> Pp.string ctx "SFMono-Regular"
  | Cascadia_code -> pp_font_family_name ctx "Cascadia Code"
  | Cascadia_mono -> pp_font_family_name ctx "Cascadia Mono"
  | Victor_mono -> pp_font_family_name ctx "Victor Mono"
  | Inconsolata -> Pp.string ctx "Inconsolata"
  | Hack -> Pp.string ctx "Hack"
  (* CSS keywords *)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Name s ->
      let safe_ident_char = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
        | _ -> false
      in
      (* A single-word [Name] that matches a generic family / CSS-wide keyword
         must stay quoted - dropping the quotes turns the [<family-name>] into
         the generic keyword (different semantics in [@font-face] and in
         [font-family] cascade). *)
      let collides_with_keyword =
        List.mem (String.lowercase_ascii s)
          [
            "serif";
            "sans-serif";
            "monospace";
            "cursive";
            "fantasy";
            "system-ui";
            "ui-serif";
            "ui-sans-serif";
            "ui-monospace";
            "ui-rounded";
            "emoji";
            "math";
            "fangsong";
            "inherit";
            "initial";
            "unset";
            "revert";
            "revert-layer";
            "none";
            "default";
          ]
      in
      if
        Pp.minified ctx && (not ctx.Pp.enforce_spec)
        && can_unquote_font_family_name s
      then Pp.string ctx s
      else if
        s = ""
        || (not (String.for_all safe_ident_char s))
        || collides_with_keyword
      then Pp.quoted_string ctx s
      else Pp.string ctx s
  | Var v -> pp_var pp_font_family ctx v
  | List fonts ->
      let level_chars =
        match ctx.Pp.indent with Some w -> w * ctx.Pp.level | None -> 0
      in
      (* CSS Fonts 4 sec. 4.1: [font-family] is a fallback list, so a duplicate
         entry never wins under cascade resolution - drop it under minify (the
         first occurrence keeps the source position). A bare generic keyword
         (notably [monospace]) takes the UA generic-font size, so the
         [monospace, monospace] idiom opts back into the normal size; a dedup
         must not collapse a list to a single generic, which would shrink the
         text. *)
      let fonts =
        if Pp.minified ctx then
          let seen = Hashtbl.create 8 in
          let deduped =
            List.filter
              (fun f ->
                let key = Pp.to_string ~minify:true pp_font_family f in
                if Hashtbl.mem seen key then false
                else (
                  Hashtbl.add seen key ();
                  true))
              fonts
          in
          match deduped with
          | [ single ] when is_generic_family single && List.length fonts > 1 ->
              fonts
          | _ -> deduped
        else fonts
      in
      Pp.list_wrap ~threshold:90 ~sep:Pp.comma ~wrap_indent:(level_chars + 2)
        pp_font_family ctx fonts
  | Invalid tokens ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified tokens
        else Parser.string_of_components tokens
      in
      Pp.string ctx rendered

(* pp_font_families is no longer needed since Fonts is now part of
   font_family *)

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
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_style ctx v

(* CSS Box 4 7.1: a 1-to-4 value box shorthand ([margin], [padding],
   [border-radius] sides, [background-position]) collapses when sides repeat. [a
   a a a] -> [a]; [a b a b] -> [a b]; [a b c b] -> [a b c]. *)
let collapse_box_shorthand vs =
  match vs with
  | [ a; b; c; d ] when a = b && b = c && c = d -> [ a ]
  | [ a; b; c; d ] when a = c && b = d -> [ a; b ]
  | [ a; b; c; d ] when b = d -> [ a; b; c ]
  | [ a; b; c ] when a = b && b = c -> [ a ]
  | [ a; b; c ] when a = c -> [ a; b ]
  | [ a; b ] when a = b -> [ a ]
  | _ -> vs

let pp_box_shorthand pp ctx vs =
  let vs = if Pp.minified ctx then collapse_box_shorthand vs else vs in
  Pp.list ~sep:Pp.space pp ctx vs

let rec pp_border_radius : border_radius Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border_radius ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Radius { horizontal; vertical } -> (
      pp_box_shorthand (pp_length_percentage ~always:true) ctx horizontal;
      match vertical with
      | None -> ()
      | Some vs ->
          Pp.sp ctx ();
          Pp.char ctx '/';
          Pp.sp ctx ();
          pp_box_shorthand (pp_length_percentage ~always:true) ctx vs)

(* Canonicalise a colour to its shortest spelling. The normalize pass only ever
   visits real declarations, never an [@supports] feature-test condition (those
   live in the unwalked [Supports] condition), so the static colour-space fold
   is never suppressed here. *)
let normalize_color ?(lossless = false) =
  Values.normalize_color ~lossless ~in_feature_query:false

let preserve_if_equal before after = if after == before then before else after

let map_preserve f xs =
  let rec loop changed acc = function
    | [] -> if changed then List.rev acc else xs
    | x :: rest ->
        let y = f x in
        loop (changed || not (y == x)) (y :: acc) rest
  in
  loop false [] xs

let option_map_preserve f opt =
  match opt with
  | Option.None -> opt
  | Option.Some x ->
      let y = f x in
      if y == x then opt else Option.Some y

let option_is_phys_same a b =
  match (a, b) with
  | Option.None, Option.None -> true
  | Option.Some a, Option.Some b -> a == b
  | _ -> false

let normalize_border_radius ?(strip = true) : border_radius -> border_radius =
 fun value ->
  match value with
  | Radius { horizontal; vertical } ->
      preserve_if_equal value
        (Radius
           {
             horizontal =
               map_preserve
                 (Values.normalize_length_percentage ~strip)
                 horizontal;
             vertical =
               option_map_preserve
                 (map_preserve (Values.normalize_length_percentage ~strip))
                 vertical;
           })
  | other -> other

let normalize_radial_size : radial_size -> radial_size =
 fun value ->
  match value with
  | Ellipse_radii (a, b) ->
      preserve_if_equal value
        (Ellipse_radii
           ( Values.normalize_length_percentage ~strip:false a,
             Values.normalize_length_percentage ~strip:false b ))
  | Circle_radius r ->
      preserve_if_equal value
        (Circle_radius (Values.normalize_length ~strip:false r))
  | other -> other

let rec normalize_gradient_direction : gradient_direction -> gradient_direction
    =
 fun value ->
  let normalize_angle_direction angle =
    match Values.angle_degrees_opt angle with
    | Some deg when Float.rem (deg -. 180.) 360. = 0. -> Default_direction
    | _ -> Angle (Values.normalize_angle angle)
  in
  match value with
  | Default_direction -> value
  | To_top -> Angle (Values.Deg 0.)
  | To_right -> Angle (Values.Deg 90.)
  | To_bottom -> Default_direction
  | To_left -> Angle (Values.Deg 270.)
  | Angle a -> preserve_if_equal value (normalize_angle_direction a)
  | With_interpolation (dir, interp) ->
      let dir' =
        match dir with
        | Default_direction -> dir
        | _ -> normalize_gradient_direction dir
      in
      preserve_if_equal value (With_interpolation (dir', interp))
  | (To_top_right | To_bottom_right | To_bottom_left | To_top_left | Var _) as
    other ->
      other

let rec normalize_gradient_stop ?(lossless = false) :
    gradient_stop -> gradient_stop =
 fun value ->
  match value with
  | Color_percentage (c, p1, p2) ->
      preserve_if_equal value
        (Color_percentage
           ( normalize_color ~lossless c,
             option_map_preserve
               (Values.normalize_length_percentage ~strip:false)
               p1,
             option_map_preserve
               (Values.normalize_length_percentage ~strip:false)
               p2 ))
  | Color_length (c, l1, l2) ->
      preserve_if_equal value
        (Color_length
           ( normalize_color ~lossless c,
             option_map_preserve (Values.normalize_length ~strip:false) l1,
             option_map_preserve (Values.normalize_length ~strip:false) l2 ))
  | Length l ->
      preserve_if_equal value (Length (Values.normalize_length ~strip:false l))
  | Direction dir ->
      preserve_if_equal value (Direction (normalize_gradient_direction dir))
  | List stops ->
      preserve_if_equal value
        (List (map_preserve (normalize_gradient_stop ~lossless) stops))
  | other -> other

let normalize_webkit_gradient_stop :
    ?lossless:bool -> Webkit_gradient.stop -> Webkit_gradient.stop =
 fun ?(lossless = false) value ->
  match value with
  | From c -> preserve_if_equal value (From (normalize_color ~lossless c))
  | To c -> preserve_if_equal value (To (normalize_color ~lossless c))
  | Color_stop (p, c) ->
      preserve_if_equal value (Color_stop (p, normalize_color ~lossless c))

let normalize_webkit_gradient ?(lossless = false) :
    Webkit_gradient.t -> Webkit_gradient.t =
 fun value ->
  match value with
  | Linear r ->
      let stops =
        map_preserve (normalize_webkit_gradient_stop ~lossless) r.stops
      in
      if stops == r.stops then value else Linear { r with stops }
  | Radial r ->
      let stops =
        map_preserve (normalize_webkit_gradient_stop ~lossless) r.stops
      in
      if stops == r.stops then value else Radial { r with stops }

(* [<position>] spelling canonicalization. CSS Values 4 sec. 6.1: the edge
   keywords are percentage synonyms ([left] = [top] = [0%], [center] = [50%],
   [right] = [bottom] = [100%]), a percentage offset of [0%] and the length [0]
   resolve to the same point, and a single value defaults the vertical component
   to [center]. Fold every statically-known position to the node the parser
   produces for its shortest spelling; the printer stays faithful to the node,
   so a normalized AST and the reparse of its output agree (the structural-hash
   invariant of [Declaration.v]). *)
let position_zero (l : Values.length) : Values.length =
  match l with Pct 0. -> Zero | l -> l

let position_offset_zero (lp : Values.length_percentage) :
    Values.length_percentage =
  match lp with Pct 0. | Length (Pct 0.) -> Length Zero | lp -> lp

(* The horizontal and vertical components of a statically-known position; [None]
   when a component is dynamic ([var()], [calc()], [env()]) or offset from a
   non-zero edge ([right]/[bottom] offsets need the box size). *)
let position_xy : position_value -> (Values.length * Values.length) option =
  let pct n : Values.length = Pct n in
  let static_offset : Values.length_percentage -> Values.length option =
    function
    | Length l -> Some l
    | Pct n -> Some (pct n)
    | Env _ | Var _ | Calc _ | Invalid _ -> None
  in
  function
  | Center -> Some (pct 50., pct 50.)
  | Left -> Some (pct 0., pct 50.)
  | Right -> Some (pct 100., pct 50.)
  | Top -> Some (pct 50., pct 0.)
  | Bottom -> Some (pct 50., pct 100.)
  | Left_top | Top_left -> Some (pct 0., pct 0.)
  | Left_center -> Some (pct 0., pct 50.)
  | Left_bottom | Bottom_left -> Some (pct 0., pct 100.)
  | Right_top | Top_right -> Some (pct 100., pct 0.)
  | Right_center -> Some (pct 100., pct 50.)
  | Right_bottom | Bottom_right -> Some (pct 100., pct 100.)
  | Center_top -> Some (pct 50., pct 0.)
  | Center_bottom -> Some (pct 50., pct 100.)
  | XY (x, y) -> Some (x, y)
  | Single x -> Some (x, pct 50.)
  | Edge_offset_axis ("left", off, "center") ->
      Option.map (fun x -> (x, pct 50.)) (static_offset off)
  | Edge_offset_axis ("left", off, "top") ->
      Option.map (fun x -> (x, pct 0.)) (static_offset off)
  | Axis_edge_offset ("center", "top", off) -> Some (pct 50., off)
  | Edge_offset_edge_offset ("left", x, "top", y) -> (
      match (static_offset x, static_offset y) with
      | Some x, Some y -> Some (x, y)
      | _ -> None)
  | _ -> None

(* The parser's node for the shortest spelling of a static (x, y) position: [x]
   alone when [y] is centred, the [top] / [bottom] keywords when they beat [50%
   0] / [50% 100%], a plain pair otherwise. *)
let position_of_xy (x : Values.length) (y : Values.length) : position_value =
  match (position_zero x, position_zero y) with
  | x, Pct 50. -> Single x
  | Pct 50., Zero -> Top
  | Pct 50., Pct 100. -> Bottom
  | x, y -> XY (x, y)

let normalize_position_value ?(strip = true) : position_value -> position_value
    =
 fun value ->
  let length l = position_zero (Values.normalize_length ~strip l) in
  let offset lp =
    position_offset_zero (Values.normalize_length_percentage ~strip lp)
  in
  match position_xy value with
  | Some (x, y) ->
      preserve_if_equal value (position_of_xy (length x) (length y))
  | None -> (
      match value with
      | Edge_offset_axis (e, lp, a) ->
          preserve_if_equal value (Edge_offset_axis (e, offset lp, a))
      | Axis_edge_offset (a, e, l) ->
          preserve_if_equal value (Axis_edge_offset (a, e, length l))
      | Edge_offset_edge_offset (e1, lp1, e2, lp2) ->
          preserve_if_equal value
            (Edge_offset_edge_offset (e1, offset lp1, e2, offset lp2))
      | other -> other)

let normalize_radial_config (c : radial_gradient_config) =
  (* CSS Images 4 section 3.1: inside a radial-gradient() prelude the default
     shape (ellipse), size (farthest-corner) and position (center) are implied,
     so the canonical form drops them. pp stays faithful to the AST; this
     elision is an optimize-side rewrite. *)
  let shape = radial_shape_kept c.shape in
  let size =
    option_map_preserve normalize_radial_size (radial_size_kept c.size)
  in
  let position =
    option_map_preserve
      (normalize_position_value ~strip:false)
      (radial_position_kept c.position)
  in
  if shape == c.shape && size == c.size && position == c.position then c
  else { shape; size; position; interpolation = c.interpolation }

let normalize_conic_config (c : conic_gradient_config) =
  let angle = option_map_preserve Values.normalize_angle c.angle in
  let position =
    option_map_preserve (normalize_position_value ~strip:false) c.position
  in
  if angle == c.angle && position == c.position then c
  else { c with angle; position }

let rec normalize_background_image ?(lossless = false) :
    background_image -> background_image =
 fun value ->
  let stops = map_preserve (normalize_gradient_stop ~lossless) in
  match value with
  | Linear_gradient (d, s) ->
      preserve_if_equal value
        (Linear_gradient (normalize_gradient_direction d, stops s))
  | Radial_gradient (c, s) ->
      preserve_if_equal value
        (Radial_gradient (normalize_radial_config c, stops s))
  | Conic_gradient (c, s) ->
      preserve_if_equal value
        (Conic_gradient (normalize_conic_config c, stops s))
  | Repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | Repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Repeating_radial_gradient (normalize_radial_config c, stops s))
  | Repeating_conic_gradient (c, s) ->
      preserve_if_equal value
        (Repeating_conic_gradient (normalize_conic_config c, stops s))
  | Webkit_linear_gradient (d, s) ->
      preserve_if_equal value
        (Webkit_linear_gradient (normalize_gradient_direction d, stops s))
  | Webkit_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Webkit_repeating_linear_gradient
           (normalize_gradient_direction d, stops s))
  | Webkit_radial_gradient (c, s) ->
      preserve_if_equal value
        (Webkit_radial_gradient (normalize_radial_config c, stops s))
  | Webkit_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Webkit_repeating_radial_gradient (normalize_radial_config c, stops s))
  | Moz_linear_gradient (d, s) ->
      preserve_if_equal value
        (Moz_linear_gradient (normalize_gradient_direction d, stops s))
  | Moz_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Moz_repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | Moz_radial_gradient (c, s) ->
      preserve_if_equal value
        (Moz_radial_gradient (normalize_radial_config c, stops s))
  | Moz_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Moz_repeating_radial_gradient (normalize_radial_config c, stops s))
  | O_linear_gradient (d, s) ->
      preserve_if_equal value
        (O_linear_gradient (normalize_gradient_direction d, stops s))
  | O_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (O_repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | O_radial_gradient (c, s) ->
      preserve_if_equal value
        (O_radial_gradient (normalize_radial_config c, stops s))
  | O_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (O_repeating_radial_gradient (normalize_radial_config c, stops s))
  | Webkit_gradient g ->
      preserve_if_equal value
        (Webkit_gradient (normalize_webkit_gradient ~lossless g))
  | Cross_fade opts ->
      let normalize_opt (o : cross_fade_option) =
        let image = normalize_background_image ~lossless o.image in
        if image == o.image then o else { o with image }
      in
      preserve_if_equal value (Cross_fade (map_preserve normalize_opt opts))
  | List imgs ->
      preserve_if_equal value
        (List (map_preserve (normalize_background_image ~lossless) imgs))
  | other -> other

let normalize_clip_path_extent v =
  match v with
  | Extent_length l ->
      let l' = Values.normalize_length ~strip:false l in
      if l' == l then v else Extent_length l'
  | other -> other

let drop_default_clip_path_position (opt : position_value option) =
  match opt with
  | Option.Some p when position_value_is_center p -> Option.None
  | _ -> opt

let drop_default_clip_path_extent (opt : clip_path_extent option) =
  match opt with Option.Some Closest_side -> Option.None | _ -> opt

let normalize_clip_path_inset value =
  match value with
  | Clip_path_inset r ->
      (* The four inset slots and the rounded clause are plain
         [<length-percentage>] / [<border-radius>] positions, not [calc()]
         operands, so a zero length can drop its unit per CSS Values L4 sec.
         6.1.1. *)
      let lp = Values.normalize_length_percentage ~strip:true in
      let top = lp r.top in
      let right = option_map_preserve lp r.right in
      let bottom = option_map_preserve lp r.bottom in
      let left = option_map_preserve lp r.left in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        top == r.top && right == r.right && bottom == r.bottom && left == r.left
        && rounded == r.rounded
      then value
      else Clip_path_inset { top; right; bottom; left; rounded }
  | _ -> value

let normalize_clip_path_xywh value =
  match value with
  | Clip_path_xywh r ->
      (* xywh slots are plain [<length-percentage>] / [<border-radius>], not
         calc operands - zero lengths can drop the unit. *)
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
      else Clip_path_xywh { x; y; width; height; rounded }
  | _ -> value

let normalize_clip_path_rect value =
  match value with
  | Clip_path_rect r ->
      (* rect() slots are plain [<length-percentage>] / [<border-radius>], not
         calc operands - zero lengths can drop the unit. *)
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
      else Clip_path_rect { top; right; bottom; left; rounded }
  | _ -> value

let normalize_clip_path_circle value =
  match value with
  | Clip_path_circle r ->
      let radius =
        option_map_preserve normalize_clip_path_extent r.radius
        |> drop_default_clip_path_extent
      in
      let position =
        option_map_preserve (normalize_position_value ~strip:false) r.position
        |> drop_default_clip_path_position
      in
      if
        option_is_phys_same radius r.radius
        && option_is_phys_same position r.position
      then value
      else Clip_path_circle { radius; position }
  | _ -> value

let normalize_clip_path_ellipse value =
  match value with
  | Clip_path_ellipse r ->
      let rx =
        option_map_preserve normalize_clip_path_extent r.rx
        |> drop_default_clip_path_extent
      in
      let ry =
        option_map_preserve normalize_clip_path_extent r.ry
        |> drop_default_clip_path_extent
      in
      let position =
        option_map_preserve (normalize_position_value ~strip:false) r.position
        |> drop_default_clip_path_position
      in
      if
        option_is_phys_same rx r.rx
        && option_is_phys_same ry r.ry
        && option_is_phys_same position r.position
      then value
      else Clip_path_ellipse { rx; ry; position }
  | _ -> value

let normalize_clip_path_polygon value =
  match value with
  | Clip_path_polygon r ->
      (* polygon vertex coordinates are plain [<length-percentage>], not calc
         operands - zero lengths can drop the unit. *)
      let len = Values.normalize_length ~strip:true in
      let normalize_point (x, y) =
        let x' = len x in
        let y' = len y in
        if x' == x && y' == y then (x, y) else (x', y')
      in
      let points = map_preserve normalize_point r.points in
      if points == r.points then value else Clip_path_polygon { r with points }
  | _ -> value

let rec normalize_clip_path : clip_path -> clip_path =
 fun value ->
  match value with
  | Clip_path_inset _ -> normalize_clip_path_inset value
  | Clip_path_xywh _ -> normalize_clip_path_xywh value
  | Clip_path_rect _ -> normalize_clip_path_rect value
  | Clip_path_circle _ -> normalize_clip_path_circle value
  | Clip_path_ellipse _ -> normalize_clip_path_ellipse value
  | Clip_path_polygon _ -> normalize_clip_path_polygon value
  | Clip_path_with_box r ->
      let shape = normalize_clip_path r.shape in
      if shape == r.shape then value else Clip_path_with_box { r with shape }
  | other -> other

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

let normalize_background_shorthand ?(lossless = false)
    (b : background_shorthand) =
  let color = option_map_preserve (normalize_color ~lossless) b.color in
  let image =
    option_map_preserve (normalize_background_image ~lossless) b.image
  in
  let position = option_map_preserve normalize_position_value b.position in
  if color == b.color && image == b.image && position == b.position then b
  else { b with color; image; position }

let normalize_background ?(lossless = false) : background -> background =
 fun value ->
  match value with
  | Shorthand s ->
      let s' = normalize_background_shorthand ~lossless s in
      if s' == s then value else Shorthand s'
  | other -> other

let normalize_mask_layer ?(lossless = false) (l : mask_layer) =
  let image =
    option_map_preserve (normalize_background_image ~lossless) l.image
  in
  let position = option_map_preserve normalize_position_value l.position in
  if image == l.image && position == l.position then l
  else { l with image; position }

let normalize_mask ?(lossless = false) : mask -> mask =
 fun value ->
  match value with
  | Layer l ->
      let l' = normalize_mask_layer ~lossless l in
      if l' == l then value else Layer l'
  | Layers ls ->
      let ls' = map_preserve (normalize_mask_layer ~lossless) ls in
      if ls' == ls then value else Layers ls'
  | other -> other

let normalize_text_indent : text_indent_value -> text_indent_value =
 fun value ->
  match value with
  | Indent r ->
      let length = Values.normalize_length_percentage r.length in
      if length == r.length then value else Indent { r with length }
  | other -> other

let normalize_animation_range_item :
    animation_range_item -> animation_range_item =
 fun value ->
  match value with
  | Offset lp ->
      preserve_if_equal value (Offset (Values.normalize_length_percentage lp))
  | Named (n, lp) ->
      preserve_if_equal value
        (Named (n, option_map_preserve Values.normalize_length_percentage lp))
  | other -> other

let normalize_animation_range : animation_range -> animation_range =
 fun value ->
  match value with
  | Range (a, b) ->
      preserve_if_equal value
        (Range
           ( normalize_animation_range_item a,
             option_map_preserve normalize_animation_range_item b ))
  | other -> other

let normalize_timeline_inset_item : timeline_inset_item -> timeline_inset_item =
 fun value ->
  match value with
  | Length lp ->
      preserve_if_equal value (Length (Values.normalize_length_percentage lp))
  | other -> other

let normalize_timeline_inset : timeline_inset -> timeline_inset =
 fun value ->
  match value with
  | Inset (a, b) ->
      preserve_if_equal value
        (Inset
           ( normalize_timeline_inset_item a,
             option_map_preserve normalize_timeline_inset_item b ))
  | other -> other

let normalize_baseline_shift : baseline_shift -> baseline_shift =
 fun value ->
  match value with
  | Shift lp ->
      preserve_if_equal value (Shift (Values.normalize_length_percentage lp))
  | other -> other

let normalize_gap : gap -> gap =
 fun value ->
  match value with
  | Lengths { row_gap; column_gap } ->
      preserve_if_equal value
        (Lengths
           {
             row_gap = option_map_preserve Values.normalize_length row_gap;
             column_gap = option_map_preserve Values.normalize_length column_gap;
           })
  | other -> other

let rec numeric_flex_factor_calc_leaves : flex_factor calc -> flex_factor calc =
  function
  | Val (Number n) -> Num n
  | Nested inner -> Nested (numeric_flex_factor_calc_leaves inner)
  | Parens inner -> Parens (numeric_flex_factor_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_flex_factor_calc_leaves left,
          op,
          numeric_flex_factor_calc_leaves right )
  | other -> other

let rec normalize_flex_factor (value : flex_factor) : flex_factor =
  match value with
  | Calc c -> (
      match eval_calc (numeric_flex_factor_calc_leaves c) with
      | Num f -> Number f
      | Val v -> normalize_flex_factor v
      | folded -> if folded == c then value else Calc folded)
  | _ -> value

let rec normalize_flex_basis (value : flex_basis) : flex_basis =
  match value with
  | Px 0.
  | Cm 0.
  | Mm 0.
  | Q 0.
  | In 0.
  | Pt 0.
  | Pc 0.
  | Rem 0.
  | Em 0.
  | Ex 0.
  | Cap 0.
  | Ic 0.
  | Ric 0.
  | Rlh 0.
  | Vw 0.
  | Vh 0.
  | Vmin 0.
  | Vmax 0.
  | Vi 0.
  | Vb 0.
  | Dvh 0.
  | Dvw 0.
  | Dvmin 0.
  | Dvmax 0.
  | Lvh 0.
  | Lvw 0.
  | Lvmin 0.
  | Lvmax 0.
  | Svh 0.
  | Svw 0.
  | Svmin 0.
  | Svmax 0.
  | Ch 0.
  | Lh 0. ->
      Zero
  | Calc c -> (
      match eval_calc c with
      | Val v -> normalize_flex_basis v
      | folded -> if folded == c then value else Calc folded)
  | _ -> value

let normalize_flex (value : flex) : flex =
  match value with
  | Grow f ->
      let f' = normalize_flex_factor f in
      if f' == f then value else Grow f'
  | Basis b ->
      let b' = normalize_flex_basis b in
      if b' == b then value else Basis b'
  | Grow_shrink (g, s) ->
      let g' = normalize_flex_factor g in
      let s' = normalize_flex_factor s in
      if g' == g && s' == s then value else Grow_shrink (g', s')
  | Full (g, s, b) ->
      let g' = normalize_flex_factor g in
      let s' = normalize_flex_factor s in
      let b' = normalize_flex_basis b in
      if g' == g && s' == s && b' == b then value else Full (g', s', b')
  | _ -> value

let normalize_aspect_ratio : aspect_ratio -> aspect_ratio =
 fun value ->
  match value with
  | Auto_ratio_calc (a, b) ->
      preserve_if_equal value
        (Auto_ratio_calc (Values.normalize_number a, Values.normalize_number b))
  | Ratio_calc (a, b) ->
      preserve_if_equal value
        (Ratio_calc (Values.normalize_number a, Values.normalize_number b))
  | other -> other

let normalize_border ?(lossless = false) : border -> border =
 fun value ->
  match value with
  | Shorthand s ->
      let color = option_map_preserve (normalize_color ~lossless) s.color in
      if color == s.color then value else Shorthand { s with color }
  | other -> other

let normalize_outline ?(lossless = false) : outline -> outline =
 fun value ->
  match value with
  | Shorthand s ->
      let width = option_map_preserve Values.normalize_length s.width in
      let color = option_map_preserve (normalize_color ~lossless) s.color in
      if width == s.width && color == s.color then value
      else Shorthand { s with width; color }
  | other -> other

let normalize_logical_border_color ?(lossless = false) :
    logical_border_color -> logical_border_color =
 fun value ->
  match value with
  | Single c -> preserve_if_equal value (Single (normalize_color ~lossless c))
  | Pair (a, b) ->
      preserve_if_equal value
        (Pair (normalize_color ~lossless a, normalize_color ~lossless b))
  | other -> other

let normalize_text_decoration ?(lossless = false) :
    text_decoration -> text_decoration =
 fun value ->
  match value with
  | Shorthand s ->
      let color = option_map_preserve (normalize_color ~lossless) s.color in
      if color == s.color then value else Shorthand { s with color }
  | other -> other

let normalize_text_emphasis ?(lossless = false) : text_emphasis -> text_emphasis
    =
 fun value ->
  match value with
  | Emphasis (style, color) ->
      preserve_if_equal value
        (Emphasis (style, option_map_preserve (normalize_color ~lossless) color))
  | other -> other

let rec normalize_shadow ?(lossless = false) : shadow -> shadow =
 fun value ->
  let normalize_body (s : shadow_body) : shadow_body =
    let blur = option_map_preserve Values.normalize_length s.blur in
    let spread = option_map_preserve Values.normalize_length s.spread in
    let color = option_map_preserve (normalize_color ~lossless) s.color in
    (* Drop a trailing optional length equal to its [0] default, contiguously
       from the end. [spread] is always the last token, so a zero spread drops
       freely; a zero blur drops only when no spread follows - otherwise it is
       positional and dropping it would re-bind the spread as the blur (e.g. [0
       1px 0 5px] must keep the [0] blur). *)
    let spread : length option =
      match spread with Some sp when is_zero_length sp -> None | _ -> spread
    in
    (* A [var()] colour could resolve to a length, so dropping a zero blur
       before it lets the shortened form re-bind the colour as the blur: [0 3px
       0 var(--c)] is blur [0] + colour, but [0 3px var(--c)] parses [var(--c)]
       as the blur. Keep the explicit [0] there; a concrete colour is
       unambiguous and the [0] still drops. *)
    let colour_may_be_length =
      match color with Some (Var _) -> true | _ -> false
    in
    let blur : length option =
      match blur with
      | Some b
        when is_zero_length b && Option.is_none spread
             && not colour_may_be_length ->
          None
      | _ -> blur
    in
    {
      h_offset = Values.normalize_length s.h_offset;
      v_offset = Values.normalize_length s.v_offset;
      blur;
      spread;
      color;
    }
  in
  match value with
  | Shadow s -> preserve_if_equal value (Shadow (normalize_body s))
  | Inset (Body s) ->
      preserve_if_equal value (Inset (Body (normalize_body s)) : shadow)
  | Inset (Toggle { name; no_fallback; body }) ->
      preserve_if_equal value
        (Inset (Toggle { name; no_fallback; body = normalize_body body })
          : shadow)
  | List shadows ->
      preserve_if_equal value
        (List (map_preserve (normalize_shadow ~lossless) shadows))
  | other -> other

let normalize_text_shadow ?(lossless = false) : text_shadow -> text_shadow =
 fun value ->
  match value with
  | Text_shadow s ->
      preserve_if_equal value
        (Text_shadow
           {
             h_offset = Values.normalize_length s.h_offset;
             v_offset = Values.normalize_length s.v_offset;
             blur = option_map_preserve Values.normalize_length s.blur;
             color = option_map_preserve (normalize_color ~lossless) s.color;
           })
  | other -> other

let rec normalize_filter ?(lossless = false) : filter -> filter =
 fun value ->
  let np = Values.normalize_number_percentage in
  match value with
  | Drop_shadow s ->
      preserve_if_equal value (Drop_shadow (normalize_shadow ~lossless s))
  | Hue_rotate a ->
      preserve_if_equal value (Hue_rotate (Values.normalize_angle a))
  | Brightness x -> preserve_if_equal value (Brightness (np x))
  | Contrast x -> preserve_if_equal value (Contrast (np x))
  | Grayscale x -> preserve_if_equal value (Grayscale (np x))
  | Invert x -> preserve_if_equal value (Invert (np x))
  | Opacity x -> preserve_if_equal value (Opacity (np x))
  | Saturate x -> preserve_if_equal value (Saturate (np x))
  | Sepia x -> preserve_if_equal value (Sepia (np x))
  | List filters ->
      preserve_if_equal value
        (List (map_preserve (normalize_filter ~lossless) filters))
  | other -> other

let normalize_rotate : rotate_value -> rotate_value =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Angle a -> preserve_if_equal value (Angle (na a))
    | X a -> preserve_if_equal value (X (na a))
    | Y a -> preserve_if_equal value (Y (na a))
    | Z a -> preserve_if_equal value (Z (na a))
    | Axis (x, y, z, a) -> preserve_if_equal value (Axis (x, y, z, na a))
    | other -> other

let normalize_translate_value : translate_value -> translate_value =
  let nl = Values.normalize_length in
  fun value ->
    match value with
    | X x -> preserve_if_equal value (X (nl x))
    | XY (x, y) -> preserve_if_equal value (XY (nl x, nl y))
    | XYZ (x, y, z) -> preserve_if_equal value (XYZ (nl x, nl y, nl z))
    | other -> other

(* The CSS [scale] property: pick the shorter [<number-percentage>] spelling so
   pp serialises the canonical node. The X/XY/XYZ variants are distinct from the
   transform [scale()] family - those live in [normalize_transform_leaves]. *)
let normalize_scale : scale -> scale =
 fun value ->
  let np = Values.normalize_number_percentage in
  match value with
  | X x -> preserve_if_equal value (X (np x))
  | XY (x, y) -> preserve_if_equal value (XY (np x, np y))
  | XYZ (x, y, z) -> preserve_if_equal value (XYZ (np x, np y, np z))
  | other -> other

(* [transform-origin] had no normalize pass, so [0px 50%] and [0% 50%] (and
   [left center]) stayed in distinct spellings of the same origin. [0], [0px]
   and [0%] all place the origin at the same edge, so fold every zero component
   to the unitless [0] - the canonical form the [left]/[top] keywords already
   minify to. *)
let normalize_transform_origin : transform_origin -> transform_origin =
  let z l =
    match (l : Values.length) with
    | Pct 0. -> (Zero : Values.length)
    | _ -> Values.normalize_length l
  in
  fun value ->
    match value with
    | X a -> preserve_if_equal value (X (z a))
    | XY (a, b) -> preserve_if_equal value (XY (z a, z b))
    | XYZ (a, b, c) -> preserve_if_equal value (XYZ (z a, z b, z c))
    | Position p ->
        preserve_if_equal value (Position (normalize_position_value p))
    | Position_z (p, c) ->
        preserve_if_equal value (Position_z (normalize_position_value p, z c))
    | other -> other

let normalize_ray_size : ray_size -> ray_size =
 fun value ->
  match value with
  | Radial size -> preserve_if_equal value (Radial (normalize_radial_size size))
  | Sides -> Sides

let normalize_offset_path : offset_path -> offset_path =
 fun value ->
  match value with
  | Ray ray ->
      preserve_if_equal value
        (Ray
           {
             ray with
             angle = Values.normalize_angle ray.angle;
             size = option_map_preserve normalize_ray_size ray.size;
             position =
               option_map_preserve
                 (normalize_position_value ~strip:false)
                 ray.position;
           })
  | other -> other

let normalize_offset_rotate : offset_rotate -> offset_rotate =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Angle a -> preserve_if_equal value (Angle (na a))
    | With_angle (mode, a) -> preserve_if_equal value (With_angle (mode, na a))
    | other -> other

let normalize_font_style : font_style -> font_style =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Oblique_angle a -> preserve_if_equal value (Oblique_angle (na a))
    | Oblique_range (a, b) ->
        preserve_if_equal value (Oblique_range (na a, na b))
    | other -> other

let normalize_caret ?(lossless = false) : caret -> caret =
 fun value ->
  match value with
  | Caret (color, anim, shape) ->
      preserve_if_equal value
        (Caret
           (option_map_preserve (normalize_color ~lossless) color, anim, shape))
  | other -> other

let normalize_scrollbar_color ?(lossless = false) :
    scrollbar_color -> scrollbar_color =
 fun value ->
  match value with
  | Colors (a, b) ->
      preserve_if_equal value
        (Colors (normalize_color ~lossless a, normalize_color ~lossless b))
  | other -> other

let rec normalize_svg_paint ?(lossless = false) : svg_paint -> svg_paint =
 fun value ->
  match value with
  | Color c -> preserve_if_equal value (Color (normalize_color ~lossless c))
  | Url (u, fallback) ->
      preserve_if_equal value
        (Url (u, option_map_preserve (normalize_svg_paint ~lossless) fallback))
  | other -> other

let length_of_border_width : border_width -> length option = function
  | Px n -> Some (Px n)
  | Cm n -> Some (Cm n)
  | Mm n -> Some (Mm n)
  | Q n -> Some (Q n)
  | In n -> Some (In n)
  | Pt n -> Some (Pt n)
  | Pc n -> Some (Pc n)
  | Rem n -> Some (Rem n)
  | Em n -> Some (Em n)
  | Ex n -> Some (Ex n)
  | Cap n -> Some (Cap n)
  | Ic n -> Some (Ic n)
  | Ric n -> Some (Ric n)
  | Rlh n -> Some (Rlh n)
  | Ch n -> Some (Ch n)
  | Lh n -> Some (Lh n)
  | Vh n -> Some (Vh n)
  | Vw n -> Some (Vw n)
  | Vmin n -> Some (Vmin n)
  | Vmax n -> Some (Vmax n)
  | Pct n -> Some (Pct n)
  | Zero -> Some Zero
  | _ -> None

let length_of_border_width_calc calc =
  let rec aux : border_width calc -> length calc option = function
    | Val width ->
        Option.map (fun length -> Val length) (length_of_border_width width)
    | Num n -> Some (Num n)
    | Math_const c -> Some (Math_const c)
    | Math_fn fn -> Some (Math_fn fn)
    | Var _ | Sibling_index | Sibling_count -> None
    | Nested inner -> Option.map (fun inner -> Nested inner) (aux inner)
    | Parens inner -> Option.map (fun inner -> Parens inner) (aux inner)
    | Expr (left, op, right) -> (
        match (aux left, aux right) with
        | Some left, Some right -> Some (Expr (left, op, right))
        | _ -> None)
  in
  aux calc

let length_linear_term : length -> (string * float * (float -> length)) option =
  function
  | Zero -> Some ("px", 0., fun _ -> Zero)
  | Px n -> Some ("px", n, fun n -> if n = 0. then Zero else Px n)
  | Cm n -> Some ("cm", n, fun n -> Cm n)
  | Mm n -> Some ("mm", n, fun n -> Mm n)
  | Q n -> Some ("q", n, fun n -> Q n)
  | In n -> Some ("in", n, fun n -> In n)
  | Pt n -> Some ("pt", n, fun n -> Pt n)
  | Pc n -> Some ("pc", n, fun n -> Pc n)
  | Rem n -> Some ("rem", n, fun n -> Rem n)
  | Em n -> Some ("em", n, fun n -> Em n)
  | Ex n -> Some ("ex", n, fun n -> Ex n)
  | Cap n -> Some ("cap", n, fun n -> Cap n)
  | Ic n -> Some ("ic", n, fun n -> Ic n)
  | Ric n -> Some ("ric", n, fun n -> Ric n)
  | Rlh n -> Some ("rlh", n, fun n -> Rlh n)
  | Ch n -> Some ("ch", n, fun n -> Ch n)
  | Lh n -> Some ("lh", n, fun n -> Lh n)
  | Pct n -> Some ("%", n, fun n -> Pct n)
  | Vw n -> Some ("vw", n, fun n -> Vw n)
  | Vh n -> Some ("vh", n, fun n -> Vh n)
  | Vmin n -> Some ("vmin", n, fun n -> Vmin n)
  | Vmax n -> Some ("vmax", n, fun n -> Vmax n)
  | _ -> None

let add_border_width_term
    (table : (string, int * float * (float -> length)) Hashtbl.t) pos
    (key, n, make) =
  match Hashtbl.find_opt table key with
  | None -> Hashtbl.add table key (pos, n, make)
  | Some (old_pos, old_n, old_make) ->
      Hashtbl.replace table key (old_pos, old_n +. n, old_make)

let collect_border_width_terms
    (terms : (string * float * (float -> length)) list) =
  let table = Hashtbl.create 4 in
  List.iteri (add_border_width_term table) terms;
  Hashtbl.to_seq_values table
  |> List.of_seq
  |> List.filter (fun (_, n, _) -> n <> 0.)
  |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)

let append_border_width_term (acc : length calc)
    ((_, n, make) : int * float * (float -> length)) : length calc =
  if n < 0. then Expr (acc, Sub, Val (make (-.n)))
  else Expr (acc, Add, Val (make n))

let rebuild_border_width_length_terms
    (terms : (string * float * (float -> length)) list) : length calc =
  match collect_border_width_terms terms with
  | [] -> Val Zero
  | [ (_, n, make) ] -> Val (make n)
  | (_, n, make) :: rest ->
      List.fold_left append_border_width_term (Val (make n)) rest

let simplify_border_width_length_calc calc =
  let scale factor terms =
    List.map (fun (key, n, make) -> (key, factor *. n, make)) terms
  in
  let rec terms = function
    | Val length ->
        Option.map (fun term -> [ term ]) (length_linear_term length)
    | Num 0. -> Some []
    | Num _ | Math_const _ | Var _ | Sibling_index | Sibling_count | Math_fn _
      ->
        None
    | Nested inner | Parens inner -> terms inner
    | Expr (left, Add, right) -> (
        match (terms left, terms right) with
        | Some left, Some right -> Some (left @ right)
        | _ -> None)
    | Expr (left, Sub, right) -> (
        match (terms left, terms right) with
        | Some left, Some right -> Some (left @ scale (-1.) right)
        | _ -> None)
    | Expr (left, Mul, Num n) -> Option.map (scale n) (terms left)
    | Expr (Num n, Mul, right) -> Option.map (scale n) (terms right)
    | Expr (left, Div, Num n) when n <> 0. ->
        Option.map (scale (1. /. n)) (terms left)
    | Expr _ -> None
  in
  match terms calc with
  | None -> calc
  | Some terms -> rebuild_border_width_length_terms terms

let simplified_border_width_length calc =
  Option.map simplify_border_width_length_calc
    (length_of_border_width_calc calc)

let pp_length_calc_op = pp_calc_op

let pp_length_calc_contents ctx calc =
  let precedence (op : calc_op) =
    match op with Add | Sub -> 1 | Mul | Div -> 2
  in
  let rec pp_inner ~parent_prec ~right_of_noncommut ctx = function
    | Val length -> pp_length ctx length
    | Num n -> Pp.float ctx n
    | (Var _ | Sibling_index | Sibling_count | Math_const _ | Math_fn _) as calc
      ->
        pp_calc pp_length ctx calc
    | Nested inner ->
        Pp.call "calc"
          (pp_inner ~parent_prec:0 ~right_of_noncommut:false)
          ctx inner
    | Parens inner ->
        Pp.char ctx '(';
        pp_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner;
        Pp.char ctx ')'
    | Expr (left, op, right) ->
        let op_prec = precedence op in
        let needs_parens =
          op_prec < parent_prec || (right_of_noncommut && op_prec <= parent_prec)
        in
        if needs_parens then Pp.char ctx '(';
        pp_inner ~parent_prec:op_prec ~right_of_noncommut:false ctx left;
        pp_length_calc_op ctx op;
        let is_noncommut = match op with Sub | Div -> true | _ -> false in
        pp_inner ~parent_prec:op_prec ~right_of_noncommut:is_noncommut ctx right;
        if needs_parens then Pp.char ctx ')'
  in
  pp_inner ~parent_prec:0 ~right_of_noncommut:false ctx calc

let border_width_length_measure : length -> [ `Abs | `Unit of string ] * float =
  function
  | Px n -> (`Abs, n)
  | In n -> (`Abs, n *. 96.)
  | Cm n -> (`Abs, n *. 96. /. 2.54)
  | Mm n -> (`Abs, n *. 96. /. 25.4)
  | Q n -> (`Abs, n *. 96. /. 101.6)
  | Pt n -> (`Abs, n *. 96. /. 72.)
  | Pc n -> (`Abs, n *. 16.)
  | Rem n -> (`Unit "rem", n)
  | Em n -> (`Unit "em", n)
  | Ex n -> (`Unit "ex", n)
  | Cap n -> (`Unit "cap", n)
  | Ic n -> (`Unit "ic", n)
  | Ric n -> (`Unit "ric", n)
  | Rlh n -> (`Unit "rlh", n)
  | Ch n -> (`Unit "ch", n)
  | Lh n -> (`Unit "lh", n)
  | Vw n -> (`Unit "vw", n)
  | Vh n -> (`Unit "vh", n)
  | Vmin n -> (`Unit "vmin", n)
  | Vmax n -> (`Unit "vmax", n)
  | Pct n -> (`Unit "%", n)
  | Zero -> (`Abs, 0.)
  | _ -> (`Unit "", nan)

let comparable_border_width_length length :
    ([ `Abs | `Unit of string ] * float) option =
  match border_width_length_measure length with
  | `Unit "", _ -> None
  | key, n -> Some (key, n)

let border_width_calc_length = function Val length -> Some length | _ -> None

let record_border_width_group kind groups pos length =
  match comparable_border_width_length length with
  | None -> ()
  | Some (key, n) ->
      let keep =
        match Hashtbl.find_opt groups key with
        | None -> (pos, length, n)
        | Some (old_pos, old_length, old_n) ->
            let better =
              match kind with `Min -> n < old_n | `Max -> n > old_n
            in
            if better then (old_pos, length, n) else (old_pos, old_length, old_n)
      in
      Hashtbl.replace groups key keep

let reduce_border_width_minmax kind args : length calc list option =
  let simplified = List.map simplified_border_width_length args in
  if List.exists Option.is_none simplified then None
  else
    let vals = List.map Option.get simplified in
    match List.map border_width_calc_length vals with
    | lengths when List.exists Option.is_none lengths -> None
    | lengths -> (
        let lengths = List.map Option.get lengths in
        let groups = Hashtbl.create 4 in
        List.iteri (record_border_width_group kind groups) lengths;
        let reduced =
          Hashtbl.to_seq_values groups
          |> List.of_seq
          |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
          |> List.map (fun (_, length, _) -> Val length)
        in
        match reduced with [] -> None | _ -> Some reduced)

let reduce_border_width_clamp lower value upper =
  match
    ( simplified_border_width_length lower,
      simplified_border_width_length value,
      simplified_border_width_length upper )
  with
  | Some (Val lower), Some (Val value), Some (Val upper) -> (
      match
        ( comparable_border_width_length lower,
          comparable_border_width_length value,
          comparable_border_width_length upper )
      with
      | ( Some (lower_key, lower_n),
          Some (value_key, value_n),
          Some (upper_key, upper_n) )
        when lower_key = value_key && value_key = upper_key ->
          Some
            (`Length
               (if value_n < lower_n then lower
                else if value_n > upper_n then upper
                else value))
      | Some _, Some (value_key, value_n), Some (upper_key, upper_n)
        when value_key = upper_key && value_n <= upper_n ->
          Some (`Max [ Val lower; Val value ])
      | _ -> None)
  | _ -> None

let rec pp_border_width : border_width Pp.t =
 fun ctx -> function
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
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
  | Ric f -> Pp.unit ctx f "ric"
  | Rlh f -> Pp.unit ctx f "rlh"
  | Ch f -> Pp.unit ctx f "ch"
  | Lh f -> Pp.unit ctx f "lh"
  | Vh f -> Pp.unit ctx f "vh"
  | Vw f -> Pp.unit ctx f "vw"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Pct f -> Pp.pct ctx f
  | Zero -> Pp.char ctx '0'
  | Auto -> Pp.string ctx "auto"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | Fit_content -> Pp.string ctx "fit-content"
  | From_font -> Pp.string ctx "from-font"
  | Calc cv -> (
      match (Pp.minified ctx, length_of_border_width_calc cv) with
      | true, Some cv -> pp_length ctx (Values.normalize_length (Calc cv))
      | _ -> pp_calc pp_border_width ctx cv)
  | Min args -> pp_border_width_minmax "min" `Min ctx args
  | Max args -> pp_border_width_minmax "max" `Max ctx args
  | Clamp (lower, value, upper) -> pp_border_width_clamp ctx lower value upper
  | Var v -> pp_var pp_border_width ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

and pp_border_width_calc_contents ctx calc =
  match simplified_border_width_length calc with
  | Some (Val length) -> pp_length ctx length
  | Some calc -> pp_length_calc_contents ctx calc
  | None -> pp_calc pp_border_width ctx calc

and pp_border_width_minmax name kind ctx args =
  match reduce_border_width_minmax kind args with
  | Some [ Val length ] -> pp_length ctx length
  | Some args ->
      Pp.call name (Pp.list ~sep:Pp.comma pp_length_calc_contents) ctx args
  | None ->
      Pp.call name
        (Pp.list ~sep:Pp.comma pp_border_width_calc_contents)
        ctx args

and pp_border_width_clamp ctx lower value upper =
  match reduce_border_width_clamp lower value upper with
  | Some (`Length length) -> pp_length ctx length
  | Some (`Max args) ->
      Pp.call "max" (Pp.list ~sep:Pp.comma pp_length_calc_contents) ctx args
  | None ->
      Pp.call "clamp"
        (fun ctx (lower, value, upper) ->
          pp_border_width_calc_contents ctx lower;
          Pp.comma ctx ();
          pp_border_width_calc_contents ctx value;
          Pp.comma ctx ();
          pp_border_width_calc_contents ctx upper)
        ctx (lower, value, upper)

let pp_border_shorthand : border_shorthand Pp.t =
 fun ctx { width; style; color } ->
  let first = ref true in
  let add_space () = if !first then first := false else Pp.space ctx () in
  let color =
    match color with
    | Some Current when Pp.minified ctx -> (None : color option)
    | color -> color
  in
  let style =
    match (style, width, color) with
    | Some (None : border_style), None, Some _ when Pp.minified ctx ->
        (None : border_style option)
    | style, _, _ -> style
  in
  (* CSS Backgrounds 3 §4.4: [<border-width>] defaults to [medium]. When the
     user spelled it explicitly and another slot is non-default, the keyword is
     redundant - drop it. *)
  let width : border_width option =
    match (width, style, color) with
    | (Some Medium, Some _, _ | Some Medium, _, Some _) when Pp.minified ctx ->
        None
    | width, _, _ -> width
  in
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
      let rendered =
        Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
          pp_color c
      in
      (* CSS Syntax: a [#hex] hash token is unambiguous after an ident, so
         minified output drops the separating space. *)
      let leads_with_delim =
        Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '#'
      in
      if not !first then if not leads_with_delim then Pp.space ctx ();
      first := false;
      Pp.string ctx rendered)
    color;
  if !first then Pp.string ctx "none"

let rec pp_border : border Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_border_shorthand ctx shorthand

let rec pp_logical_border_color : logical_border_color Pp.t =
 fun ctx -> function
  | Single color -> pp_color ctx color
  | Pair (start_, end_) ->
      pp_color ctx start_;
      Pp.space ctx ();
      pp_color ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_color ctx v

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
  | Multi (Multi (Block, Block), List_item) when Pp.minified ctx ->
      Pp.string ctx "list-item"
  | Multi (Multi (outside, Block), List_item) when Pp.minified ctx ->
      pp_display ctx outside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  | Multi (Multi (outside, inside), List_item) ->
      pp_display ctx outside;
      Pp.space ctx ();
      pp_display_inside ctx inside;
      Pp.space ctx ();
      Pp.string ctx "list-item"
  (* CSS Display 3 sec. 2: [<display-outside> <display-inside>] with
     [<display-inside>] = [flow] (encoded as the inner [Block] arm here)
     collapses to just the outside keyword - [block flow] -> [block], [inline
     flow] -> [inline]. *)
  | Multi (Block, Block) when Pp.minified ctx -> Pp.string ctx "block"
  | Multi (Inline, Block) when Pp.minified ctx -> Pp.string ctx "inline"
  | Multi (Run_in, Block) when Pp.minified ctx -> Pp.string ctx "run-in"
  | Multi (Block, Flow_root) when Pp.minified ctx -> Pp.string ctx "flow-root"
  | Multi (Inline, Flow_root) when Pp.minified ctx ->
      (* CSS Display 3 sec. 2.6: [inline flow-root] is the two-value equivalent
         of the legacy [inline-block] keyword. *)
      Pp.string ctx "inline-block"
  | Multi (Block, Flex) when Pp.minified ctx -> Pp.string ctx "flex"
  | Multi (Inline, Flex) when Pp.minified ctx -> Pp.string ctx "inline-flex"
  | Multi (Block, Grid) when Pp.minified ctx -> Pp.string ctx "grid"
  | Multi (Inline, Grid) when Pp.minified ctx -> Pp.string ctx "inline-grid"
  | Multi (Block, Table) when Pp.minified ctx -> Pp.string ctx "table"
  | Multi (Inline, Table) when Pp.minified ctx -> Pp.string ctx "inline-table"
  | Multi (Block, Ruby) when Pp.minified ctx -> Pp.string ctx "ruby"
  | Multi (outside, inside) ->
      pp_display ctx outside;
      Pp.space ctx ();
      pp_display_inside ctx inside

and pp_display_inside ctx = function
  | Block -> Pp.string ctx "flow"
  | Flow_root -> Pp.string ctx "flow-root"
  | display -> pp_display ctx display

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

let rec pp_baseline_source : baseline_source Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | First -> Pp.string ctx "first"
  | Last -> Pp.string ctx "last"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_baseline_source ctx v

let rec pp_alignment_baseline : alignment_baseline Pp.t =
 fun ctx -> function
  | Baseline -> Pp.string ctx "baseline"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Middle -> Pp.string ctx "middle"
  | Central -> Pp.string ctx "central"
  | Text_top -> Pp.string ctx "text-top"
  | Ideographic -> Pp.string ctx "ideographic"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Hanging -> Pp.string ctx "hanging"
  | Mathematical -> Pp.string ctx "mathematical"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_alignment_baseline ctx v

let rec pp_baseline_shift : baseline_shift Pp.t =
 fun ctx -> function
  | Shift value -> pp_length_percentage ~always:true ctx value
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Top -> Pp.string ctx "top"
  | Center -> Pp.string ctx "center"
  | Bottom -> Pp.string ctx "bottom"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_baseline_shift ctx v

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

let rec pp_tab_size : tab_size Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Length len -> pp_length ~always:true ctx len
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_tab_size ctx v

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

let rec pp_order : order Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Calc c -> pp_calc pp_order ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_order ctx v

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
        ("calc", fun t -> Calc (Values.read_calc read_opacity_dim_only t));
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
  let read_number t : shape_image_threshold =
    let value = Cursor.number t in
    if value < 0. || value > 1. then
      Cursor.err_invalid t "shape-image-threshold must be between 0 and 1";
    Number value
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
  | Overflow_pair (x, y) when Pp.minified ctx && x = y -> pp_overflow ctx x
  | Overflow_pair (x, y) ->
      pp_overflow ctx x;
      Pp.space ctx ();
      pp_overflow ctx y

let rec pp_border_spacing : border_spacing Pp.t =
 fun ctx -> function
  | Lengths [ a; b ] when a = b -> pp_length ctx a
  | (Lengths lengths : border_spacing) ->
      Pp.list ~sep:Pp.space pp_length ctx lengths
  | Var v -> pp_var pp_border_spacing ctx v

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

let read_overflow_clip_length_item (length : length option ref) t =
  Cursor.ws t;
  match !length with
  | None ->
      length := Some (read_length ~allow_negative:false ~with_keywords:false t);
      true
  | Some _ -> false

let rec read_overflow_clip_margin_items box length consumed t =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_semicolon t then consumed
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

let rec pp_flex_direction : flex_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_direction ctx v
  | Row -> Pp.string ctx "row"
  | Row_reverse -> Pp.string ctx "row-reverse"
  | Column -> Pp.string ctx "column"
  | Column_reverse -> Pp.string ctx "column-reverse"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_flex_wrap : flex_wrap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_wrap ctx v
  | Nowrap -> Pp.string ctx "nowrap"
  | Wrap -> Pp.string ctx "wrap"
  | Wrap_reverse -> Pp.string ctx "wrap-reverse"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_flex_flow : flex_flow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_flow ctx v
  | Flow (direction, wrap) -> (
      match (direction, wrap) with
      | Some direction, Some wrap ->
          pp_flex_direction ctx direction;
          Pp.space ctx ();
          pp_flex_wrap ctx wrap
      | Some direction, None -> pp_flex_direction ctx direction
      | None, Some wrap -> pp_flex_wrap ctx wrap
      | None, None -> Pp.string ctx "row")
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_flex_factor : flex_factor Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_factor ctx v
  | Number value -> Pp.float ctx value
  | Calc c -> pp_calc pp_flex_factor ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_align_items : align_items Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_align_items ctx v
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_flex_start -> Pp.string ctx "safe flex-start"
  | Safe_flex_end -> Pp.string ctx "safe flex-end"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_self_start -> Pp.string ctx "unsafe self-start"
  | Unsafe_self_end -> Pp.string ctx "unsafe self-end"
  | Unsafe_flex_start -> Pp.string ctx "unsafe flex-start"
  | Unsafe_flex_end -> Pp.string ctx "unsafe flex-end"
  | Anchor_center -> Pp.string ctx "anchor-center"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_align_self : align_self Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_align_self ctx v
  | Auto -> Pp.string ctx "auto"
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_flex_start -> Pp.string ctx "safe flex-start"
  | Safe_flex_end -> Pp.string ctx "safe flex-end"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_self_start -> Pp.string ctx "unsafe self-start"
  | Unsafe_self_end -> Pp.string ctx "unsafe self-end"
  | Unsafe_flex_start -> Pp.string ctx "unsafe flex-start"
  | Unsafe_flex_end -> Pp.string ctx "unsafe flex-end"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_justify_content : justify_content Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_justify_content ctx v
  | Normal -> Pp.string ctx "normal"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_flex_start -> Pp.string ctx "safe flex-start"
  | Safe_flex_end -> Pp.string ctx "safe flex-end"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_flex_start -> Pp.string ctx "unsafe flex-start"
  | Unsafe_flex_end -> Pp.string ctx "unsafe flex-end"
  | Unsafe_left -> Pp.string ctx "unsafe left"
  | Unsafe_right -> Pp.string ctx "unsafe right"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Stretch -> Pp.string ctx "stretch"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_justify_items : justify_items Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_justify_items ctx v
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_self_start -> Pp.string ctx "safe self-start"
  | Safe_self_end -> Pp.string ctx "safe self-end"
  | Safe_flex_start -> Pp.string ctx "safe flex-start"
  | Safe_flex_end -> Pp.string ctx "safe flex-end"
  | Safe_left -> Pp.string ctx "safe left"
  | Safe_right -> Pp.string ctx "safe right"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_self_start -> Pp.string ctx "unsafe self-start"
  | Unsafe_self_end -> Pp.string ctx "unsafe self-end"
  | Unsafe_flex_start -> Pp.string ctx "unsafe flex-start"
  | Unsafe_flex_end -> Pp.string ctx "unsafe flex-end"
  | Unsafe_left -> Pp.string ctx "unsafe left"
  | Unsafe_right -> Pp.string ctx "unsafe right"
  | Anchor_center -> Pp.string ctx "anchor-center"
  | Legacy -> Pp.string ctx "legacy"
  | Legacy_center -> Pp.string ctx "legacy center"
  | Legacy_left -> Pp.string ctx "legacy left"
  | Legacy_right -> Pp.string ctx "legacy right"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_justify_self : justify_self Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_justify_self ctx v
  | Auto -> Pp.string ctx "auto"
  | Normal -> Pp.string ctx "normal"
  | Stretch -> Pp.string ctx "stretch"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_self_start -> Pp.string ctx "safe self-start"
  | Safe_self_end -> Pp.string ctx "safe self-end"
  | Safe_flex_start -> Pp.string ctx "safe flex-start"
  | Safe_flex_end -> Pp.string ctx "safe flex-end"
  | Safe_left -> Pp.string ctx "safe left"
  | Safe_right -> Pp.string ctx "safe right"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_self_start -> Pp.string ctx "unsafe self-start"
  | Unsafe_self_end -> Pp.string ctx "unsafe self-end"
  | Unsafe_flex_start -> Pp.string ctx "unsafe flex-start"
  | Unsafe_flex_end -> Pp.string ctx "unsafe flex-end"
  | Unsafe_left -> Pp.string ctx "unsafe left"
  | Unsafe_right -> Pp.string ctx "unsafe right"
  | Anchor_center -> Pp.string ctx "anchor-center"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_style : font_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_style ctx v
  | Normal -> Pp.string ctx "normal"
  | Italic -> Pp.string ctx "italic"
  | Oblique -> Pp.string ctx "oblique"
  | Oblique_angle angle ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx angle
  | Oblique_range (first, second) ->
      Pp.string ctx "oblique";
      Pp.space ctx ();
      pp_angle ctx first;
      Pp.space ctx ();
      pp_angle ctx second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_align : text_align Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_align ctx v
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Center -> Pp.string ctx "center"
  | Justify -> Pp.string ctx "justify"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Match_parent -> Pp.string ctx "match-parent"
  | Webkit_match_parent -> Pp.string ctx "-webkit-match-parent"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_indent_value : text_indent_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_indent_value ctx v
  | Indent { length; hanging; each_line } ->
      pp_length_percentage ctx length;
      if hanging then (
        Pp.space ctx ();
        Pp.string ctx "hanging");
      if each_line then (
        Pp.space ctx ();
        Pp.string ctx "each-line")
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_decoration_line : text_decoration_line Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_decoration_line ctx v
  | None -> Pp.string ctx "none"
  | Underline -> Pp.string ctx "underline"
  | Overline -> Pp.string ctx "overline"
  | Line_through -> Pp.string ctx "line-through"
  | Blink -> Pp.string ctx "blink"
  | Spelling_error -> Pp.string ctx "spelling-error"
  | Grammar_error -> Pp.string ctx "grammar-error"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_decoration_style : text_decoration_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_decoration_style ctx v
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Wavy -> Pp.string ctx "wavy"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_text_decoration_shorthand : text_decoration_shorthand Pp.t =
 fun ctx { lines; style; color; thickness } ->
  let first = ref true in
  let space_if_needed () = if !first then first := false else Pp.space ctx () in
  (* CSS Text Decoration 4 sec. 2: under minify, drop components that equal the
     longhand default ([style: solid], [color: currentcolor]); the shorthand is
     interpreted with the dropped fields restored to default. *)
  let drop_default = Pp.minified ctx in
  let style : text_decoration_style option =
    if drop_default then match style with Some Solid -> None | s -> s
    else style
  in
  let color : Values.color option =
    if drop_default then match color with Some Values.Current -> None | c -> c
    else color
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
      pp_length ctx l

let rec pp_text_decoration : text_decoration Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_text_decoration_shorthand ctx shorthand
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration ctx v

let rec pp_text_decoration_skip : text_decoration_skip Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip ctx v

let rec pp_text_decoration_skip_self : text_decoration_skip_self Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Objects -> Pp.string ctx "objects"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_self ctx v

let rec pp_text_decoration_skip_box : text_decoration_skip_box Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_box ctx v

let rec pp_text_decoration_skip_inset : text_decoration_skip_inset Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_inset ctx v

let pp_text_decoration_skip_space : text_decoration_skip_space Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"

let rec pp_text_decoration_skip_spaces : text_decoration_skip_spaces Pp.t =
 fun ctx -> function
  | Spaces spaces ->
      Pp.list ~sep:Pp.space pp_text_decoration_skip_space ctx spaces
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_decoration_skip_spaces ctx v

let pp_text_transform_case ctx = function
  | (Capitalize : text_transform_case) -> Pp.string ctx "capitalize"
  | Uppercase -> Pp.string ctx "uppercase"
  | Lowercase -> Pp.string ctx "lowercase"

let rec pp_text_transform : text_transform Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Case c -> pp_text_transform_case ctx c
  | Combo { case; full_width; full_size_kana } ->
      let first = ref true in
      let space () = if !first then first := false else Pp.space ctx () in
      Option.iter
        (fun c ->
          space ();
          pp_text_transform_case ctx c)
        case;
      if full_width then (
        space ();
        Pp.string ctx "full-width");
      if full_size_kana then (
        space ();
        Pp.string ctx "full-size-kana")
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_transform ctx v

let rec pp_text_overflow : text_overflow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_overflow ctx v
  | Clip -> Pp.string ctx "clip"
  | Ellipsis -> Pp.string ctx "ellipsis"
  | String s -> Pp.quoted_string ctx s
  | Pair (first, second) ->
      pp_text_overflow ctx first;
      Pp.space ctx ();
      pp_text_overflow ctx second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_wrap : text_wrap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_wrap ctx v
  | Wrap -> Pp.string ctx "wrap"
  | No_wrap -> Pp.string ctx "nowrap"
  | Balance -> Pp.string ctx "balance"
  | Pretty -> Pp.string ctx "pretty"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_wrap_mode : text_wrap_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_wrap_mode ctx v
  | Wrap -> Pp.string ctx "wrap"
  | No_wrap -> Pp.string ctx "nowrap"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_wrap_style : text_wrap_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_wrap_style ctx v
  | Auto -> Pp.string ctx "auto"
  | Balance -> Pp.string ctx "balance"
  | Pretty -> Pp.string ctx "pretty"
  | Stable -> Pp.string ctx "stable"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_box_trim : text_box_trim Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_box_trim ctx v
  | None -> Pp.string ctx "none"
  | Trim_start -> Pp.string ctx "trim-start"
  | Trim_end -> Pp.string ctx "trim-end"
  | Trim_both -> Pp.string ctx "trim-both"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_text_box_edge_keyword : text_box_edge_keyword Pp.t =
 fun ctx -> function
  | Text -> Pp.string ctx "text"
  | Cap -> Pp.string ctx "cap"
  | Ex -> Pp.string ctx "ex"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Ideographic_ink -> Pp.string ctx "ideographic-ink"

let rec pp_text_box_edge : text_box_edge Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_box_edge ctx v
  | Auto -> Pp.string ctx "auto"
  | Edge (first, second) ->
      pp_text_box_edge_keyword ctx first;
      Option.iter
        (fun keyword ->
          Pp.space ctx ();
          pp_text_box_edge_keyword ctx keyword)
        second
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_box : text_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_box ctx v
  | Box (trim, edge) ->
      pp_text_box_trim ctx trim;
      Option.iter
        (fun edge ->
          Pp.space ctx ();
          pp_text_box_edge ctx edge)
        edge
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

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

let pp_line_fit_edge_keyword : line_fit_edge_keyword Pp.t =
 fun ctx -> function
  | Leading -> Pp.string ctx "leading"
  | Text -> Pp.string ctx "text"
  | Cap -> Pp.string ctx "cap"
  | Ex -> Pp.string ctx "ex"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Ideographic_ink -> Pp.string ctx "ideographic-ink"

let rec pp_line_fit_edge : line_fit_edge Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_line_fit_edge ctx v
  | Edge (first, second) ->
      pp_line_fit_edge_keyword ctx first;
      Option.iter
        (fun keyword ->
          Pp.space ctx ();
          pp_line_fit_edge_keyword ctx keyword)
        second
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

let rec pp_text_spacing_trim : text_spacing_trim Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_spacing_trim ctx v
  | Normal -> Pp.string ctx "normal"
  | Space_all -> Pp.string ctx "space-all"
  | Trim_start -> Pp.string ctx "trim-start"
  | Space_first -> Pp.string ctx "space-first"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_hyphenate_limit_chars : hyphenate_limit_chars Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_hyphenate_limit_chars ctx v
  | Auto -> Pp.string ctx "auto"
  | One a -> Pp.int ctx a
  | Two (a, b) ->
      Pp.int ctx a;
      Pp.space ctx ();
      Pp.int ctx b
  | Three (a, b, c) ->
      Pp.int ctx a;
      Pp.space ctx ();
      Pp.int ctx b;
      Pp.space ctx ();
      Pp.int ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_initial_letter : initial_letter Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_initial_letter ctx v
  | Normal -> Pp.string ctx "normal"
  | Drop -> Pp.string ctx "drop"
  | Raise -> Pp.string ctx "raise"
  | Size size -> Pp.float ctx size
  | Size_sink (size, sink) ->
      Pp.float ctx size;
      Pp.space ctx ();
      Pp.int ctx sink
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_initial_letter_align_keyword : initial_letter_align_keyword Pp.t =
 fun ctx -> function
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Hanging -> Pp.string ctx "hanging"
  | Leading -> Pp.string ctx "leading"
  | Border_box -> Pp.string ctx "border-box"

let rec pp_initial_letter_align : initial_letter_align Pp.t =
 fun ctx -> function
  | Align keywords ->
      Pp.list ~sep:Pp.space pp_initial_letter_align_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_initial_letter_align ctx v

let rec pp_initial_letter_wrap : initial_letter_wrap Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | First -> Pp.string ctx "first"
  | All -> Pp.string ctx "all"
  | Grid -> Pp.string ctx "grid"
  | Length value -> pp_length_percentage ~always:true ctx value
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_initial_letter_wrap ctx v

let rec pp_ruby_merge : ruby_merge Pp.t =
 fun ctx -> function
  | Separate -> Pp.string ctx "separate"
  | Merge -> Pp.string ctx "merge"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_merge ctx v

let rec pp_ruby_align : ruby_align Pp.t =
 fun ctx -> function
  | Start -> Pp.string ctx "start"
  | Center -> Pp.string ctx "center"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_align ctx v

let rec pp_ruby_overhang : ruby_overhang Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_overhang ctx v

let pp_ruby_position_keyword : ruby_position_keyword Pp.t =
 fun ctx -> function
  | Alternate -> Pp.string ctx "alternate"
  | Over -> Pp.string ctx "over"
  | Under -> Pp.string ctx "under"
  | Inter_character -> Pp.string ctx "inter-character"

let rec pp_ruby_position : ruby_position Pp.t =
 fun ctx (value : ruby_position) ->
  match value with
  | Position keywords ->
      Pp.list ~sep:Pp.space pp_ruby_position_keyword ctx keywords
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_ruby_position ctx v

let rec pp_dominant_baseline : dominant_baseline Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Mathematical -> Pp.string ctx "mathematical"
  | Central -> Pp.string ctx "central"
  | Middle -> Pp.string ctx "middle"
  | Text_top -> Pp.string ctx "text-top"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_dominant_baseline ctx v

let rec pp_white_space : white_space Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_white_space ctx v
  | Normal -> Pp.string ctx "normal"
  | Nowrap -> Pp.string ctx "nowrap"
  | Pre -> Pp.string ctx "pre"
  | Pre_wrap -> Pp.string ctx "pre-wrap"
  | Pre_line -> Pp.string ctx "pre-line"
  | Break_spaces -> Pp.string ctx "break-spaces"
  | Preserve_nowrap -> Pp.string ctx "preserve nowrap"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_word_break : word_break Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_word_break ctx v
  | Normal -> Pp.string ctx "normal"
  | Break_all -> Pp.string ctx "break-all"
  | Keep_all -> Pp.string ctx "keep-all"
  | Break_word -> Pp.string ctx "break-word"
  | Auto_phrase -> Pp.string ctx "auto-phrase"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_overflow_wrap : overflow_wrap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overflow_wrap ctx v
  | Normal -> Pp.string ctx "normal"
  | Break_word -> Pp.string ctx "break-word"
  | Anywhere -> Pp.string ctx "anywhere"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_hyphens : hyphens Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_hyphens ctx v
  | None -> Pp.string ctx "none"
  | Manual -> Pp.string ctx "manual"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

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
  | Url u -> Pp.url ctx u
  | Var v -> pp_var pp_list_style_image ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* CSS Lists 3 sec. 4.1: under minify, drop components equal to their longhand
   initial ([type_: Disc], [position: Outside], [image: None]). When both
   [type_] and [image] are [None] and [position] is omitted, emit the single
   [none] keyword. If every component is defaulted, leave [outside] so the value
   isn't empty. *)
let drop_default_if ~drop ~is_default v =
  match v with Some x when drop && is_default x -> Option.None | _ -> v

let pp_list_style_shorthand : list_style_shorthand Pp.t =
 fun ctx { type_; position; image } ->
  let drop = Pp.minified ctx in
  let type_ =
    drop_default_if ~drop
      ~is_default:(fun (t : list_style_type) -> t = Disc)
      type_
  in
  let position =
    drop_default_if ~drop
      ~is_default:(fun (p : list_style_position) -> p = Outside)
      position
  in
  let image =
    drop_default_if ~drop
      ~is_default:(fun (i : list_style_image) -> i = None)
      image
  in
  let is_none_type = type_ = Some (None : list_style_type) in
  let is_none_image = image = Some (None : list_style_image) in
  if is_none_type && is_none_image && position = Option.None && drop then
    Pp.string ctx "none"
  else
    let first = ref true in
    let emit pp = function
      | Option.None -> ()
      | Some v ->
          if !first then first := false else Pp.space ctx ();
          pp ctx v
    in
    emit pp_list_style_type type_;
    emit pp_list_style_position position;
    emit pp_list_style_image image;
    (* Everything was an initial value and got dropped: the shorthand still
       needs one token. Emit the type initial [disc] (the shortest spelling of
       the all-initial value), not the position initial [outside] - a lone
       [outside] would set the position, changing nothing, but [disc] is shorter
       and is the canonical single-value form. *)
    if !first then Pp.string ctx "disc"

let rec pp_list_style : list_style Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_list_style_shorthand ctx sh
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_list_style ctx v

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

let rec pp_vertical_align : vertical_align Pp.t =
 fun ctx -> function
  | Baseline -> Pp.string ctx "baseline"
  | Top -> Pp.string ctx "top"
  | Middle -> Pp.string ctx "middle"
  | Bottom -> Pp.string ctx "bottom"
  | Text_top -> Pp.string ctx "text-top"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Zero -> Pp.string ctx "0"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.pct ctx p
  | Calc c -> pp_calc pp_vertical_align ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_vertical_align ctx v

let rec pp_grid_auto_flow : grid_auto_flow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_grid_auto_flow ctx v
  | Row -> Pp.string ctx "row"
  | Column -> Pp.string ctx "column"
  | Dense -> Pp.string ctx "dense"
  | Row_dense -> Pp.string ctx "row dense"
  | Column_dense -> Pp.string ctx "column dense"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_grid_line : grid_line Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Num n -> Pp.int ctx n
  | Name s -> Pp.string ctx s
  | Num_name (n, name) ->
      Pp.int ctx n;
      Pp.char ctx ' ';
      Pp.string ctx name
  | Span n ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.int ctx n
  | Span_name name ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.string ctx name
  | Span_num_name (1, name) when Pp.minified ctx ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.string ctx name
  | Span_num_name (n, name) ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.int ctx n;
      Pp.char ctx ' ';
      Pp.string ctx name
  | Calc c -> pp_calc pp_grid_line ctx c
  | Var v -> pp_var pp_grid_line ctx v

let rec pp_grid_line_pair : grid_line_pair Pp.t =
 fun ctx -> function
  | (Lines (start, end_) : grid_line_pair) -> (
      pp_grid_line ctx start;
      match end_ with
      | Auto -> ()
      | _ ->
          Pp.sp ctx ();
          Pp.slash ctx ();
          Pp.sp ctx ();
          pp_grid_line ctx end_)
  | Var v -> pp_var pp_grid_line_pair ctx v

(* Inverse of [grid_area_default_from]: drop trailing slots that match the value
   the spec would have implied from the preceding slot. Yields the shortest
   equivalent 1-/2-/3-/4-value spelling. *)
let grid_area_default_from (line : grid_line) : grid_line =
  match line with Name _ -> line | _ -> Auto

let grid_area_compact ~(row_start : grid_line) ~(column_start : grid_line)
    ~(row_end : grid_line) ~(column_end : grid_line) =
  let drop_column_end = column_end = grid_area_default_from column_start in
  let drop_row_end =
    drop_column_end && row_end = grid_area_default_from row_start
  in
  let drop_column_start =
    drop_row_end && column_start = grid_area_default_from row_start
  in
  match (drop_column_start, drop_row_end, drop_column_end) with
  | true, true, true -> [ row_start ]
  | _, true, true -> [ row_start; column_start ]
  | _, _, true -> [ row_start; column_start; row_end ]
  | _ -> [ row_start; column_start; row_end; column_end ]

let rec pp_grid_area : grid_area Pp.t =
 fun ctx -> function
  | Lines { row_start; column_start; row_end; column_end } ->
      let lines =
        grid_area_compact ~row_start ~column_start ~row_end ~column_end
      in
      let sep ctx () =
        Pp.sp ctx ();
        Pp.slash ctx ();
        Pp.sp ctx ()
      in
      Pp.list ~sep pp_grid_line ctx lines
  | Var v -> pp_var pp_grid_area ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec eval_number_value : number -> float option = function
  | Num f -> Some f
  | Var _ -> None
  | Calc c -> eval_number_calc c
  | Round (strategy, value, step) -> (
      match (eval_number_value value, eval_number_value step) with
      | Some value, Some step when step <> 0. ->
          let quotient = value /. step in
          let rounded =
            match strategy with
            | "up" -> Float.ceil quotient
            | "down" -> Float.floor quotient
            | "to-zero" -> Float.trunc quotient
            | _ -> Float.round quotient
          in
          Some (rounded *. step)
      | _ -> None)
  | Mod (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b when b <> 0. -> Some (a -. (Float.floor (a /. b) *. b))
      | _ -> None)
  | Rem (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b when b <> 0. -> Some (Float.rem a b)
      | _ -> None)
  | Hypot (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b -> Some (Float.sqrt ((a *. a) +. (b *. b)))
      | _ -> None)
  | Pow (a, b) -> (
      match (eval_number_value a, eval_number_value b) with
      | Some a, Some b -> Some (a ** b)
      | _ -> None)
  | Sqrt v -> Option.map Float.sqrt (eval_number_value v)
  | Abs v -> Option.map Float.abs (eval_number_value v)
  | Sign v ->
      Option.map
        (fun x -> if x > 0. then 1. else if x < 0. then -1. else 0.)
        (eval_number_value v)
  | Sin _ -> None

and eval_number_calc : number calc -> float option = function
  | Num f -> Some f
  | Math_const c ->
      Some
        (match c with
        | Pi -> Float.pi
        | E -> Float.exp 1.
        | Infinity -> Float.infinity
        | Neg_infinity -> Float.neg_infinity
        | Nan -> Float.nan)
  | Math_fn fn -> Values.eval_math_fn fn
  | Val v -> eval_number_value v
  | Var _ | Sibling_index | Sibling_count -> None
  | Nested inner | Parens inner -> eval_number_calc inner
  | Expr (left, op, right) -> (
      match (eval_number_calc left, eval_number_calc right) with
      | Some left, Some right -> (
          match op with
          | Add -> Some (left +. right)
          | Sub -> Some (left -. right)
          | Mul -> Some (left *. right)
          | Div when right <> 0. -> Some (left /. right)
          | Div -> None)
      | _ -> None)

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

let rec pp_will_change : will_change Pp.t =
 fun ctx -> function
  | Will_change_auto -> Pp.string ctx "auto"
  | Scroll_position -> Pp.string ctx "scroll-position"
  | Contents -> Pp.string ctx "contents"
  | Transform -> Pp.string ctx "transform"
  | Opacity -> Pp.string ctx "opacity"
  | Properties props -> Pp.list ~sep:Pp.comma Pp.string ctx props
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_will_change ctx v

let pp_perspective_origin : perspective_origin Pp.t = pp_position_value

let rec pp_clip : clip Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_clip ctx v
  | Clip_auto -> Pp.string ctx "auto"
  | Clip_rect (top, right, bottom, left) ->
      Pp.string ctx "rect(";
      pp_length ctx top;
      Pp.char ctx ',';
      pp_length ctx right;
      Pp.char ctx ',';
      pp_length ctx bottom;
      Pp.char ctx ',';
      pp_length ctx left;
      Pp.char ctx ')'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_clip_geometry_box ctx = function
  | (Margin_box : clip_geometry_box) -> Pp.string ctx "margin-box"
  | Border_box -> Pp.string ctx "border-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Content_box -> Pp.string ctx "content-box"
  | Fill_box -> Pp.string ctx "fill-box"
  | Stroke_box -> Pp.string ctx "stroke-box"
  | View_box -> Pp.string ctx "view-box"

let pp_clip_path_extent ctx = function
  | (Extent_length l : clip_path_extent) -> pp_length ctx l
  | Closest_side -> Pp.string ctx "closest-side"
  | Farthest_side -> Pp.string ctx "farthest-side"

let pp_clip_path_fill_rule ctx = function
  | (Nonzero : clip_path_fill_rule) -> Pp.string ctx "nonzero"
  | Evenodd -> Pp.string ctx "evenodd"

let pp_clip_path_circle_args ctx radius position =
  let printed = ref false in
  Option.iter
    (fun r ->
      pp_clip_path_extent ctx r;
      printed := true)
    radius;
  Option.iter
    (fun p ->
      if !printed then Pp.space ctx ();
      Pp.string ctx "at ";
      pp_position_value ctx p;
      printed := true)
    position;
  ignore !printed

let pp_clip_path_ellipse_args ctx rx ry position =
  let printed = ref false in
  (match (rx, ry) with
  | Some rx, Some ry ->
      pp_clip_path_extent ctx rx;
      Pp.space ctx ();
      pp_clip_path_extent ctx ry;
      printed := true
  | Some rx, None ->
      pp_clip_path_extent ctx rx;
      printed := true
  | None, Some ry ->
      pp_clip_path_extent ctx ry;
      printed := true
  | None, None -> ());
  Option.iter
    (fun p ->
      if !printed then Pp.space ctx ();
      Pp.string ctx "at ";
      pp_position_value ctx p;
      printed := true)
    position;
  ignore !printed

let pp_clip_path_inset_quad ctx a b c d =
  pp_length_percentage ~always:true ctx a;
  Pp.space ctx ();
  pp_length_percentage ~always:true ctx b;
  Pp.space ctx ();
  pp_length_percentage ~always:true ctx c;
  Pp.space ctx ();
  pp_length_percentage ~always:true ctx d

let pp_clip_path_round ctx (rounded : border_radius option) =
  match rounded with
  | None -> ()
  | Some r ->
      Pp.space ctx ();
      Pp.string ctx "round";
      Pp.space ctx ();
      pp_border_radius ctx r

let minified_clip_path_fill_rule ctx (fill_rule : clip_path_fill_rule option) :
    clip_path_fill_rule option =
  match fill_rule with
  | Some Nonzero when Pp.minified ctx -> None
  | other -> other

let pp_clip_path_polygon_rule ctx fill_rule points =
  Option.iter
    (fun rule ->
      pp_clip_path_fill_rule ctx rule;
      if points <> [] then (
        Pp.char ctx ',';
        if not (Pp.minified ctx) then Pp.space ctx ()))
    (minified_clip_path_fill_rule ctx fill_rule)

let pp_clip_path_polygon_sep spaced ctx () =
  Pp.char ctx ',';
  if spaced && not (Pp.minified ctx) then Pp.space ctx ()

let pp_clip_path_axis_pair ctx (x, y) =
  pp_length ctx x;
  if not (Pp.minified ctx && match x with Pct _ -> true | _ -> false) then
    Pp.space ctx ();
  pp_length ctx y

let pp_clip_path_polygon_body ctx fill_rule points spaced =
  pp_clip_path_polygon_rule ctx fill_rule points;
  Pp.list
    ~sep:(pp_clip_path_polygon_sep spaced)
    pp_clip_path_axis_pair ctx points

let pp_optional_inset_sides : type a.
    a Pp.t -> Pp.ctx -> a option -> a option -> a option -> unit =
 fun pp ctx right bottom left ->
  Option.iter
    (fun r ->
      Pp.space ctx ();
      pp ctx r;
      Option.iter
        (fun b ->
          Pp.space ctx ();
          pp ctx b;
          Option.iter
            (fun l ->
              Pp.space ctx ();
              pp ctx l)
            left)
        bottom)
    right

let rec pp_clip_path : clip_path Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_clip_path ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Invalid tokens ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified tokens
        else Parser.string_of_components tokens
      in
      Pp.string ctx rendered
  | Clip_path_none -> Pp.string ctx "none"
  | Clip_path_url url ->
      Pp.string ctx "url(";
      Pp.string ctx url;
      Pp.char ctx ')'
  | Clip_path_inset { top; right; bottom; left; rounded } ->
      Pp.string ctx "inset(";
      pp_length_percentage ctx top;
      pp_optional_inset_sides pp_length_percentage ctx right bottom left;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Clip_path_circle { radius; position } ->
      Pp.string ctx "circle(";
      pp_clip_path_circle_args ctx radius position;
      Pp.char ctx ')'
  | Clip_path_ellipse { rx; ry; position } ->
      Pp.string ctx "ellipse(";
      pp_clip_path_ellipse_args ctx rx ry position;
      Pp.char ctx ')'
  | Clip_path_polygon { fill_rule; points; spaced } ->
      Pp.string ctx "polygon(";
      pp_clip_path_polygon_body ctx fill_rule points spaced;
      Pp.char ctx ')'
  | Clip_path_path d ->
      Pp.string ctx "path(\"";
      Pp.string ctx d;
      Pp.string ctx "\")"
  | Clip_path_shape body ->
      Pp.string ctx "shape(";
      Pp.string ctx body;
      Pp.char ctx ')'
  | Clip_path_box box -> pp_clip_geometry_box ctx box
  | Clip_path_with_box { shape; box; box_first } ->
      if box_first then (
        pp_clip_geometry_box ctx box;
        Pp.space ctx ();
        pp_clip_path ctx shape)
      else (
        pp_clip_path ctx shape;
        Pp.space ctx ();
        pp_clip_geometry_box ctx box)
  | Clip_path_xywh { x; y; width; height; rounded } ->
      Pp.string ctx "xywh(";
      pp_clip_path_inset_quad ctx x y width height;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Clip_path_rect { top; right; bottom; left; rounded } ->
      Pp.string ctx "rect(";
      pp_clip_path_inset_quad ctx top right bottom left;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'

let pp_property : type a. a property Pp.t =
 fun ctx -> function
  | Custom_property name -> Pp.string ctx name
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
  | Border_inline_color -> Pp.string ctx "border-inline-color"
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
  | Opacity -> Pp.string ctx "opacity"
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
  | Border -> Pp.string ctx "border"
  | Background -> Pp.string ctx "background"
  | Tab_size -> Pp.string ctx "tab-size"
  | Zoom -> Pp.string ctx "zoom"
  | Webkit_text_size_adjust -> Pp.string ctx "-webkit-text-size-adjust"
  | Font_feature_settings -> Pp.string ctx "font-feature-settings"
  | Font_variation_settings -> Pp.string ctx "font-variation-settings"
  | Webkit_tap_highlight_color -> Pp.string ctx "-webkit-tap-highlight-color"
  | Webkit_text_fill_color -> Pp.string ctx "-webkit-text-fill-color"
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
  | Column_count -> Pp.string ctx "column-count"
  | Column_rule -> Pp.string ctx "column-rule"
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

let rec pp_font_feature_settings : font_feature_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Feature_list s ->
      (* Feature list contains quoted tags already in the stored string *)
      Pp.string ctx s
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | String s -> Pp.quoted_string ctx s
  | Var v -> pp_var pp_font_feature_settings ctx v

(* Collapse the optional whitespace after each axis-separator comma in a
   [font-variation-settings] list. Commas inside a quoted axis tag (a 4-char tag
   may contain U+2C) are left untouched. *)
let minify_axis_list s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let in_quote = ref false in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if !in_quote then (
      Buffer.add_char buf c;
      if c = '"' then in_quote := false;
      incr i)
    else if c = '"' then (
      Buffer.add_char buf c;
      in_quote := true;
      incr i)
    else if c = ',' then (
      Buffer.add_char buf ',';
      incr i;
      while !i < n && s.[!i] = ' ' do
        incr i
      done)
    else (
      Buffer.add_char buf c;
      incr i)
  done;
  Buffer.contents buf

let rec pp_font_variation_settings : font_variation_settings Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Axis_list s ->
      Pp.string ctx (if Pp.minified ctx then minify_axis_list s else s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
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

(* Tailwind concatenates var() calls without spaces, but uses spaces between
   normal transform functions. This helper determines if a transform is a Var *)
let is_transform_var : transform -> bool = function Var _ -> true | _ -> false

(* CSS Transforms 1 sec. 11: collapse a transform function to a shorter
   equivalent when an axis is zero / unity / matches another. Spec-equivalent in
   every case - they all map to the same matrix. *)
let canonicalise_transform : transform -> transform = function
  | Translate_3d (x, y, z)
    when Values.length_is_zero y && Values.length_is_zero z ->
      Translate (x, None)
  | Translate_3d (x, y, z)
    when Values.length_is_zero x && Values.length_is_zero z ->
      Translate_y y
  | Translate_3d (x, y, z)
    when Values.length_is_zero x && Values.length_is_zero y ->
      Translate_z z
  | Translate (x, Some y) when Values.length_is_zero y -> Translate (x, None)
  | Translate (x, Some y) when Values.length_is_zero x -> Translate_y y
  | Translate_x x -> Translate (x, None)
  | Scale (x, Some (Num 1.)) -> Scale_x x
  | Scale (Num 1., Some y) -> Scale_y y
  | Scale (x, Some y) when x = y -> Scale (x, None)
  | Scale_3d (x, y, Num 1.) -> Scale (x, Some y)
  | Rotate_z a -> Rotate a
  | Rotate_3d (1., 0., 0., a) -> Rotate_x a
  | Rotate_3d (0., 1., 0., a) -> Rotate_y a
  | Rotate_3d (0., 0., 1., a) -> Rotate a
  | other -> other

(* Drive [canonicalise_transform] to a fixed point and recurse into a transform
   list: one rewrite can expose another (e.g. [scale3d(a,a,1)] -> [scale(a,a)]
   -> [scale(a)]), so a single application is not idempotent. This is the
   AST-level normaliser; [pp_transform] stays a pure serialiser of its
   result. *)
(* Fold the [<length>] operands of the translate / perspective functions; the
   angle and number-percentage operands still fold through their own printers.
   Running this before [canonicalise_transform] lets the zero-checks see a folded
   [calc()]. *)
let normalize_transform_leaves : transform -> transform =
  (* The translate / perspective operands are inside a function, so they keep a
     zero's unit ([translate(0px)] stays). The rotate / skew operands are
     angles, converted to the shortest unit. The scale operands are
     [<number-percentage>]: pick the shorter spelling so pp does not have to
     fold the [Pct]/[Num] node distinction. *)
  let nl = Values.normalize_length ~strip:false in
  let na = Values.normalize_angle in
  let np = Values.normalize_number_percentage in
  function
  | Translate (x, y) -> Translate (nl x, Option.map nl y)
  | Translate_x x -> Translate_x (nl x)
  | Translate_y y -> Translate_y (nl y)
  | Translate_z z -> Translate_z (nl z)
  | Translate_3d (x, y, z) -> Translate_3d (nl x, nl y, nl z)
  | Perspective l -> Perspective (nl l)
  | Rotate a -> Rotate (na a)
  | Rotate_x a -> Rotate_x (na a)
  | Rotate_y a -> Rotate_y (na a)
  | Rotate_z a -> Rotate_z (na a)
  | Rotate_3d (x, y, z, a) -> Rotate_3d (x, y, z, na a)
  | Rotate_axis (x, y, z, a) -> Rotate_axis (x, y, z, na a)
  | Skew (a, b) -> Skew (na a, Option.map na b)
  | Skew_x a -> Skew_x (na a)
  | Skew_y a -> Skew_y (na a)
  | Scale (x, y) -> Scale (np x, Option.map np y)
  | Scale_space (x, y) -> Scale_space (np x, np y)
  | Scale_x x -> Scale_x (np x)
  | Scale_y y -> Scale_y (np y)
  | Scale_z z -> Scale_z (np z)
  | Scale_3d (x, y, z) -> Scale_3d (np x, np y, np z)
  | other -> other

let rec normalize_transform (t : transform) : transform =
  match t with
  | List ts -> List (List.map normalize_transform ts)
  | _ ->
      let t = normalize_transform_leaves t in
      let t' = canonicalise_transform t in
      if t' == t then t else normalize_transform t'

let rec pp_transform : transform Pp.t =
 fun ctx t ->
  (* Pure serialiser: the [canonicalise_transform] fold lives in
     [normalize_transform], so minify makes no semantic choice here. *)
  pp_transform_raw ctx t

and pp_transform_raw : transform Pp.t =
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
  | Rotate_axis (x, y, z, a) ->
      Pp.string ctx "rotate(";
      Pp.float ctx x;
      Pp.space ctx ();
      Pp.float ctx y;
      Pp.space ctx ();
      Pp.float ctx z;
      Pp.space ctx ();
      pp_angle ctx a;
      Pp.char ctx ')'
  | Scale (x, None) -> Pp.call "scale" pp_number_percentage ctx x
  | Scale (x, Some y) ->
      Pp.call_2 "scale" pp_number_percentage pp_number_percentage ctx (x, y)
  | Scale_space (x, y) ->
      Pp.string ctx "scale(";
      pp_number_percentage ctx x;
      Pp.space ctx ();
      pp_number_percentage ctx y;
      Pp.char ctx ')'
  | Scale_x v -> Pp.call "scaleX" pp_number_percentage ctx v
  | Scale_y v -> Pp.call "scaleY" pp_number_percentage ctx v
  | Scale_z v -> Pp.call "scaleZ" pp_number_percentage ctx v
  | Scale_3d (x, y, z) ->
      Pp.call_3 "scale3d" pp_number_percentage pp_number_percentage
        pp_number_percentage ctx (x, y, z)
  | Skew (x, None) -> Pp.call "skew" pp_angle ctx x
  | Skew (x, Some y) -> Pp.call_2 "skew" pp_angle pp_angle ctx (x, y)
  | Skew_x x -> Pp.call "skewX" pp_angle ctx x
  | Skew_y y -> Pp.call "skewY" pp_angle ctx y
  | Matrix (a, b, c, d, e, f) ->
      Pp.call_list "matrix" Pp.float ctx [ a; b; c; d; e; f ]
  | Matrix_3d m -> pp_matrix_3d ctx m
  | Perspective p -> Pp.call "perspective" pp_length ctx p
  | Inherit -> pp_keyword "inherit" ctx
  | Initial -> pp_keyword "initial" ctx
  | Unset -> pp_keyword "unset" ctx
  | Revert -> pp_keyword "revert" ctx
  | Revert_layer -> pp_keyword "revert-layer" ctx
  | Var v -> pp_var pp_transform ctx v
  | List transforms -> pp_transforms ctx transforms

and pp_transforms : transform list Pp.t =
 fun ctx transforms ->
  let rec loop prev = function
    | [] -> ()
    | x :: rest ->
        (* CSS Transforms 1: chained transform functions are separated by
           whitespace in the source. The closing [)] and following [(] serve as
           token boundaries, so under minify the space is redundant. [Var]
           adjacencies always need a space because var fallback parsing is
           whitespace-sensitive. *)
        if is_transform_var prev && is_transform_var x then Pp.space ctx ()
        else Pp.sp ctx ();
        pp_transform ctx x;
        loop x rest
  in
  match transforms with
  | [] -> ()
  | [ x ] -> pp_transform ctx x
  | h :: t ->
      pp_transform ctx h;
      loop h t

let rec pp_transform_style : transform_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_transform_style ctx v
  | Flat -> Pp.string ctx "flat"
  | Preserve_3d -> Pp.string ctx "preserve-3d"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

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
  | Plus_darker -> Pp.string ctx "plus-darker"
  | Plus_lighter -> Pp.string ctx "plus-lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_blend_mode ctx v

let rec pp_text_shadow : text_shadow Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_shadow ctx v
  | Text_shadow { h_offset; v_offset; blur; color } -> (
      pp_length ctx h_offset;
      Pp.space ctx ();
      pp_length ctx v_offset;
      (match blur with
      | Some b when not (Pp.minified ctx && is_zero_length b) ->
          Pp.space ctx ();
          pp_length ctx b
      | Some _ | None -> ());
      match color with Some c -> pp_color_after_length ctx c | None -> ())

let rec pp_background_attachment : background_attachment Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_background_attachment ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_attachment ctx layers
  | Fixed -> Pp.string ctx "fixed"
  | Local -> Pp.string ctx "local"
  | Scroll -> Pp.string ctx "scroll"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_background_repeat : background_repeat Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_background_repeat ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_repeat ctx layers
  | Repeat -> Pp.string ctx "repeat"
  | Space -> Pp.string ctx "space"
  | Round -> Pp.string ctx "round"
  | No_repeat -> Pp.string ctx "no-repeat"
  | Repeat_x -> Pp.string ctx "repeat-x"
  | Repeat_y -> Pp.string ctx "repeat-y"
  (* CSS Backgrounds 3 §3.6.1: the two-value forms collapse when both axes match
     ([X X] -> [X]) or when they alias a single-keyword shorthand ([repeat
     no-repeat] -> [repeat-x], [no-repeat repeat] -> [repeat-y]). *)
  | Repeat_repeat when Pp.minified ctx -> Pp.string ctx "repeat"
  | Space_space when Pp.minified ctx -> Pp.string ctx "space"
  | Round_round when Pp.minified ctx -> Pp.string ctx "round"
  | No_repeat_no_repeat when Pp.minified ctx -> Pp.string ctx "no-repeat"
  | Repeat_no_repeat when Pp.minified ctx -> Pp.string ctx "repeat-x"
  | No_repeat_repeat when Pp.minified ctx -> Pp.string ctx "repeat-y"
  | Repeat_repeat -> Pp.string ctx "repeat repeat"
  | Repeat_space -> Pp.string ctx "repeat space"
  | Repeat_round -> Pp.string ctx "repeat round"
  | Repeat_no_repeat -> Pp.string ctx "repeat no-repeat"
  | Space_repeat -> Pp.string ctx "space repeat"
  | Space_space -> Pp.string ctx "space space"
  | Space_round -> Pp.string ctx "space round"
  | Space_no_repeat -> Pp.string ctx "space no-repeat"
  | Round_repeat -> Pp.string ctx "round repeat"
  | Round_space -> Pp.string ctx "round space"
  | Round_round -> Pp.string ctx "round round"
  | Round_no_repeat -> Pp.string ctx "round no-repeat"
  | No_repeat_repeat -> Pp.string ctx "no-repeat repeat"
  | No_repeat_space -> Pp.string ctx "no-repeat space"
  | No_repeat_round -> Pp.string ctx "no-repeat round"
  | No_repeat_no_repeat -> Pp.string ctx "no-repeat no-repeat"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_background_box : background_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_background_box ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_box ctx layers
  | Border_box -> Pp.string ctx "border-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Content_box -> Pp.string ctx "content-box"
  | Text -> Pp.string ctx "text"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_mask_composite : webkit_mask_composite Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_mask_composite ctx v
  | Composites composites ->
      Pp.list ~sep:Pp.comma pp_webkit_mask_composite ctx composites
  | Source_over -> Pp.string ctx "source-over"
  | Xor -> Pp.string ctx "xor"
  | Source_in -> Pp.string ctx "source-in"
  | Source_out -> Pp.string ctx "source-out"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_composite : mask_composite Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_composite ctx v
  | Composites composites ->
      Pp.list ~sep:Pp.comma pp_mask_composite ctx composites
  | Add -> Pp.string ctx "add"
  | Subtract -> Pp.string ctx "subtract"
  | Intersect -> Pp.string ctx "intersect"
  | Exclude -> Pp.string ctx "exclude"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_mask_source_type : webkit_mask_source_type Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_mask_source_type ctx v
  | Alpha -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_mode : mask_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_mode ctx v
  | Alpha -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"
  | Match_source -> Pp.string ctx "match-source"
  | Modes modes -> Pp.list ~sep:Pp.comma pp_mask_mode ctx modes
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_type : mask_type Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_type ctx v
  | Alpha -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_box : mask_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_box ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_mask_box ctx layers
  | Border_box -> Pp.string ctx "border-box"
  | Content_box -> Pp.string ctx "content-box"
  | Fill_box -> Pp.string ctx "fill-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Stroke_box -> Pp.string ctx "stroke-box"
  | View_box -> Pp.string ctx "view-box"
  | No_clip -> Pp.string ctx "no-clip"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_background_size : background_size Pp.t =
 fun ctx -> function
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_size ctx layers
  | Auto -> Pp.string ctx "auto"
  | Cover -> Pp.string ctx "cover"
  | Contain -> Pp.string ctx "contain"
  | Length l -> pp_length ctx l
  | Size (w, h) ->
      pp_length ctx w;
      Pp.char ctx ' ';
      pp_length ctx h
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_background_size ctx v

let pp_background_position : background_position Pp.t =
 fun ctx positions -> pp_box_shorthand pp_position_value ctx positions

let pp_bg_prop maybe_space pp_func ctx = function
  | Some value ->
      maybe_space ();
      pp_func ctx value
  | None -> ()

let pp_bg_size_with_position maybe_space (bg : background_shorthand) ctx =
  match bg.size with
  | Some size when bg.position <> None ->
      Pp.string ctx "/";
      pp_background_size ctx size
  | Some size ->
      (* A [<bg-size>] is only reachable after [<position> /], so emit the
         initial position [0 0] (= [0% 0%]) when none was given, otherwise the
         shorthand fails to reparse. *)
      maybe_space ();
      Pp.string ctx "0 0/";
      pp_background_size ctx size
  | None -> ()

let pp_mask_layer : mask_layer Pp.t =
 fun ctx layer ->
  let first = ref true in
  (* CSS Syntax 3 §5.4.6: a token ending with [)], [\]] or [}] is
     self-delimiting, so under minify we can drop the inter-slot space after
     [url(...)] / [<image>]. *)
  let last_is_self_delim () =
    match Pp.last_char ctx with Some (')' | ']' | '}') -> true | _ -> false
  in
  let maybe_space () =
    if !first then first := false
    else if Pp.minified ctx && last_is_self_delim () then ()
    else Pp.space ctx ()
  in
  pp_bg_prop maybe_space pp_background_image ctx layer.image;
  (match (layer.position, layer.size) with
  | Some position, Some size ->
      maybe_space ();
      pp_position_value ctx position;
      Pp.string ctx "/";
      pp_background_size ctx size
  | Some position, None ->
      maybe_space ();
      pp_position_value ctx position
  | None, Some size ->
      (* A [<bg-size>] is only reachable after [<position> /]; emit the initial
         position [0 0] so the layer round-trips. *)
      maybe_space ();
      Pp.string ctx "0 0/";
      pp_background_size ctx size
  | None, None -> ());
  pp_bg_prop maybe_space pp_background_repeat ctx layer.repeat;
  pp_bg_prop maybe_space pp_mask_box ctx layer.origin;
  pp_bg_prop maybe_space pp_mask_box ctx layer.clip;
  pp_bg_prop maybe_space pp_mask_mode ctx layer.mode;
  pp_bg_prop maybe_space pp_mask_composite ctx layer.composite

let rec pp_mask : mask Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Layer layer -> pp_mask_layer ctx layer
  | Layers layers -> Pp.list ~sep:Pp.comma pp_mask_layer ctx layers
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_mask ctx v

let pp_border_image_slice_item ctx (value : border_image_slice_item) =
  match value with Number n -> Pp.float ctx n | Pct n -> Pp.pct ctx n

let pp_border_image_slice ctx { offsets; fill } =
  Pp.list ~sep:Pp.space pp_border_image_slice_item ctx offsets;
  if fill then (
    Pp.space ctx ();
    Pp.string ctx "fill")

let pp_border_image_width_item ctx (value : border_image_width_item) =
  match value with
  | Number n -> Pp.float ctx n
  | Pct n -> Pp.pct ctx n
  | Length len -> pp_length ctx len
  | Auto -> Pp.string ctx "auto"

let pp_border_image_outset_item ctx (value : border_image_outset_item) =
  match value with
  | Number n -> Pp.float ctx n
  | Length len -> pp_length ctx len

let pp_border_image_repeat_keyword ctx (value : border_image_repeat_keyword) =
  match value with
  | Stretch -> Pp.string ctx "stretch"
  | Repeat -> Pp.string ctx "repeat"
  | Round -> Pp.string ctx "round"
  | Space -> Pp.string ctx "space"

let rec pp_border_image_repeat ctx : border_image_repeat -> unit = function
  | Repeats l -> Pp.list ~sep:Pp.space pp_border_image_repeat_keyword ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_repeat ctx v

let rec pp_border_image_width ctx : border_image_width -> unit = function
  | Widths l -> Pp.list ~sep:Pp.space pp_border_image_width_item ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_width ctx v

let rec pp_border_image_outset ctx : border_image_outset -> unit = function
  | Outsets l -> Pp.list ~sep:Pp.space pp_border_image_outset_item ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_outset ctx v

let pp_mask_border_mode ctx = function
  | (Alpha : mask_border_mode) -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"

let pp_border_image : border_image Pp.t =
 fun ctx { source; slice; width; outset; repeat; mode } ->
  let first = ref true in
  (* CSS Syntax 3 §5.4.6: tokens ending with [)] are self-delimiting, so the
     inter-slot space after [url(...)] / [<image>] can be elided under
     minify. *)
  let last_is_self_delim () =
    match Pp.last_char ctx with Some (')' | ']' | '}') -> true | _ -> false
  in
  let maybe_space () =
    if !first then first := false
    else if Pp.minified ctx && last_is_self_delim () then ()
    else Pp.space ctx ()
  in
  pp_bg_prop maybe_space pp_background_image ctx source;
  pp_bg_prop maybe_space pp_border_image_slice ctx slice;
  (match width with
  | None -> ()
  | Some width ->
      Pp.char ctx '/';
      Pp.list ~sep:Pp.space pp_border_image_width_item ctx width);
  (match outset with
  | None -> ()
  | Some outset ->
      Pp.char ctx '/';
      Pp.list ~sep:Pp.space pp_border_image_outset_item ctx outset);
  pp_bg_prop maybe_space
    (Pp.list ~sep:Pp.space pp_border_image_repeat_keyword)
    ctx repeat;
  (* CSS Masking 1 §6: [alpha] is the default mode, so drop it under minify;
     [luminance] always prints. *)
  let mode =
    match mode with
    | Some Alpha when Pp.minified ctx -> (None : mask_border_mode option)
    | _ -> mode
  in
  pp_bg_prop maybe_space pp_mask_border_mode ctx mode

(* CSS Backgrounds 3 2.1: the [background] shorthand's default position is [0%
   0%]; when no [<bg-size>] is also set the position is redundant and elided
   under minify. *)
let position_is_default_origin (p : position_value) =
  match p with
  | XY (x, y) -> is_zero_length x && is_zero_length y
  | Single x -> is_zero_length x
  | Left_top | Top_left -> true
  | _ -> false

(* CSS Backgrounds 3 sec. 3.10: under minify, drop a longhand whose value equals
   its [background] shorthand initial. The resolved cascade is unchanged because
   the omitted slot inherits that same initial. *)
let drop_initial_when_minified : 'a. Pp.ctx -> 'a -> 'a option -> 'a option =
 fun ctx initial opt ->
  match opt with
  | Some x when Pp.minified ctx && x = initial -> (None : _ option)
  | _ -> opt

let drop_default_position ctx (opt : position_value option) :
    position_value option =
  match opt with
  | Some pos when Pp.minified ctx && position_is_default_origin pos ->
      (None : _ option)
  | _ -> opt

let pp_background_shorthand : background_shorthand Pp.t =
 fun ctx bg ->
  let first = ref true in
  let maybe_space () = if !first then first := false else Pp.token_sp ctx () in

  let size = drop_initial_when_minified ctx (Auto : background_size) bg.size in
  let position =
    if size = None then drop_default_position ctx bg.position else bg.position
  in
  let image =
    drop_initial_when_minified ctx (None : background_image) bg.image
  in
  let color = drop_initial_when_minified ctx (Transparent : color) bg.color in
  let repeat =
    drop_initial_when_minified ctx (Repeat : background_repeat) bg.repeat
  in
  let attachment =
    drop_initial_when_minified ctx
      (Scroll : background_attachment)
      bg.attachment
  in
  let origin =
    drop_initial_when_minified ctx (Padding_box : background_box) bg.origin
  in
  let clip =
    drop_initial_when_minified ctx (Border_box : background_box) bg.clip
  in

  pp_bg_prop maybe_space pp_background_image ctx image;
  pp_bg_prop maybe_space pp_position_value ctx position;
  pp_bg_size_with_position maybe_space { bg with position; size } ctx;
  pp_bg_prop maybe_space pp_background_repeat ctx repeat;
  pp_bg_prop maybe_space pp_background_attachment ctx attachment;
  pp_bg_prop maybe_space pp_background_box ctx origin;
  pp_bg_prop maybe_space pp_background_box ctx clip;
  pp_bg_prop maybe_space pp_color ctx color;

  if !first then Pp.string ctx (if Pp.minified ctx then "0 0" else "none")

let rec pp_gap : gap Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_gap ctx v
  | Lengths { row_gap; column_gap } -> (
      match (row_gap, column_gap) with
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
          Pp.string ctx "0")

let pp_transform_origin_pair ctx a b =
  pp_length ctx a;
  Pp.space ctx ();
  pp_length ctx b

let pp_minified_transform_origin ctx value =
  let pct n = (Pct n : length) in
  let zero = (Zero : length) in
  match value with
  | Center | Center_center -> Some (fun () -> pp_length ctx (pct 50.))
  | Left | Left_center -> Some (fun () -> pp_length ctx zero)
  | Right | Right_center -> Some (fun () -> pp_length ctx (pct 100.))
  | Left_top | Top_left ->
      Some (fun () -> pp_transform_origin_pair ctx zero zero)
  | Right_top | Top_right ->
      Some (fun () -> pp_transform_origin_pair ctx (pct 100.) zero)
  | Left_bottom | Bottom_left ->
      Some (fun () -> pp_transform_origin_pair ctx zero (pct 100.))
  | Right_bottom | Bottom_right ->
      Some (fun () -> pp_transform_origin_pair ctx (pct 100.) (pct 100.))
  | Center_top -> Some (fun () -> Pp.string ctx "top")
  | Center_bottom -> Some (fun () -> Pp.string ctx "bottom")
  | _ -> None

let pp_transform_origin_keywords ctx = function
  | Center -> Pp.string ctx "center"
  | Center_center -> Pp.string ctx "center center"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Left_top -> Pp.string ctx "left top"
  | Left_center -> Pp.string ctx "left center"
  | Left_bottom -> Pp.string ctx "left bottom"
  | Right_top -> Pp.string ctx "right top"
  | Right_center -> Pp.string ctx "right center"
  | Right_bottom -> Pp.string ctx "right bottom"
  | Center_top -> Pp.string ctx "center top"
  | Center_bottom -> Pp.string ctx "center bottom"
  | Top_left -> Pp.string ctx "top left"
  | Top_right -> Pp.string ctx "top right"
  | Bottom_left -> Pp.string ctx "bottom left"
  | Bottom_right -> Pp.string ctx "bottom right"
  | _ -> ()

let rec pp_transform_origin : transform_origin Pp.t =
 fun ctx value ->
  match
    if Pp.minified ctx then pp_minified_transform_origin ctx value else None
  with
  | Some pp -> pp ()
  | None -> (
      match value with
      | Inherit -> Pp.string ctx "inherit"
      | Initial -> Pp.string ctx "initial"
      | Unset -> Pp.string ctx "unset"
      | Revert -> Pp.string ctx "revert"
      | Revert_layer -> Pp.string ctx "revert-layer"
      | Center | Center_center | Left | Right | Top | Bottom | Left_top
      | Left_center | Left_bottom | Right_top | Right_center | Right_bottom
      | Center_top | Center_bottom | Top_left | Top_right | Bottom_left
      | Bottom_right ->
          pp_transform_origin_keywords ctx value
      | Position position -> pp_position_value ctx position
      | X a -> pp_length ctx a
      | XY (a, Pct 50.) when Pp.minified ctx -> pp_length ctx a
      | XY (a, b) ->
          pp_length ctx a;
          Pp.space ctx ();
          pp_length ctx b
      | XYZ (a, b, z) ->
          pp_length ctx a;
          Pp.space ctx ();
          pp_length ctx b;
          Pp.space ctx ();
          pp_length ctx z
      | Position_z (position, z) ->
          pp_position_value ctx position;
          Pp.space ctx ();
          pp_length ctx z
      | Var v -> pp_var pp_transform_origin ctx v)

let rec pp_transform_box : transform_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_transform_box ctx v
  | Content_box -> Pp.string ctx "content-box"
  | Border_box -> Pp.string ctx "border-box"
  | Fill_box -> Pp.string ctx "fill-box"
  | Stroke_box -> Pp.string ctx "stroke-box"
  | View_box -> Pp.string ctx "view-box"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_background : background Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | None ->
      (* CSS Backgrounds 3 §3.10: [background: none] and [background: 0 0] are
         computed-value-equivalent (both clear the image and set position to [0
         0]). Pick the shorter form under minify. *)
      Pp.string ctx (if Pp.minified ctx then "0 0" else "none")
  | Var v -> pp_var pp_background ctx v
  | Vars vars -> Pp.list ~sep:Pp.space (pp_var pp_background) ctx vars
  | Shorthand s -> pp_background_shorthand ctx s

(* Helpers for transform-origin *)
let origin (a : length) (b : length) : transform_origin = XY (a, b)

let origin3d (a : length) (b : length) (z : length) : transform_origin =
  XYZ (a, b, z)

let rec pp_animation_direction : animation_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_direction ctx v
  | Directions directions ->
      Pp.list ~sep:Pp.comma pp_animation_direction ctx directions
  | Normal -> Pp.string ctx "normal"
  | Reverse -> Pp.string ctx "reverse"
  | Alternate -> Pp.string ctx "alternate"
  | Alternate_reverse -> Pp.string ctx "alternate-reverse"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_animation_fill_mode : animation_fill_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_fill_mode ctx v
  | Fill_modes modes -> Pp.list ~sep:Pp.comma pp_animation_fill_mode ctx modes
  | None -> Pp.string ctx "none"
  | Forwards -> Pp.string ctx "forwards"
  | Backwards -> Pp.string ctx "backwards"
  | Both -> Pp.string ctx "both"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_animation_iteration_count : animation_iteration_count Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_iteration_count ctx v
  | Counts counts ->
      Pp.list ~sep:Pp.comma pp_animation_iteration_count ctx counts
  | Infinite -> Pp.string ctx "infinite"
  | Num n -> Pp.float ctx n
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_animation_name : animation_name Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Name name -> Pp.string ctx name
  | Ambiguous name -> Pp.string ctx name
  | Quoted name ->
      (* CSS Animations 1 §3.3: [<keyframes-name>] excludes [none], the CSS-wide
         keywords, and [default]. A source [animation-name: "none"] therefore
         can't refer to a real [@keyframes none] - it's invalid input that
         browsers tolerate. Minified output drops the quotes so the value
         collapses to the equivalent (and shorter) keyword form. *)
      if Pp.minified ctx then Pp.string ctx name else Pp.quoted_string ctx name
  | Names names -> Pp.list ~sep:Pp.comma pp_animation_name ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_animation_name ctx v

let rec pp_animation_play_state : animation_play_state Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_play_state ctx v
  | States states -> Pp.list ~sep:Pp.comma pp_animation_play_state ctx states
  | Running -> Pp.string ctx "running"
  | Paused -> Pp.string ctx "paused"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_animation_composition_item ctx = function
  | Replace -> Pp.string ctx "replace"
  | Add -> Pp.string ctx "add"
  | Accumulate -> Pp.string ctx "accumulate"

let rec pp_animation_composition : animation_composition Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_composition ctx v
  | Compositions items ->
      Pp.list ~sep:Pp.comma pp_animation_composition_item ctx items
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_steps_direction : steps_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_steps_direction ctx v
  | Jump_start -> Pp.string ctx "jump-start"
  | Jump_end -> Pp.string ctx "jump-end"
  | Jump_none -> Pp.string ctx "jump-none"
  | Jump_both -> Pp.string ctx "jump-both"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"

let rec pp_appearance : appearance Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_appearance ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Button -> Pp.string ctx "button"
  | Textfield -> Pp.string ctx "textfield"
  | Menulist -> Pp.string ctx "menulist"
  | Base_select -> Pp.string ctx "base-select"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_color_scheme : color_scheme Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_color_scheme ctx v
  | Normal -> Pp.string ctx "normal"
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Light_dark ->
      Pp.string ctx "light";
      Pp.space ctx ();
      Pp.string ctx "dark"
  (* Color Adjust 1 SS 2.1: [only] is unordered relative to the scheme keywords;
     the canonical serialization (CSSOM, browsers) puts the scheme first and
     [only] last. *)
  | Only_light ->
      Pp.string ctx "light";
      Pp.space ctx ();
      Pp.string ctx "only"
  | Only_dark ->
      Pp.string ctx "dark";
      Pp.space ctx ();
      Pp.string ctx "only"
  | Only_light_dark ->
      Pp.string ctx "light";
      Pp.space ctx ();
      Pp.string ctx "dark";
      Pp.space ctx ();
      Pp.string ctx "only"
  | Custom names -> Pp.list ~sep:Pp.space Pp.string ctx names
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_print_color_adjust : print_color_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_print_color_adjust ctx v
  | Economy -> Pp.string ctx "economy"
  | Exact -> Pp.string ctx "exact"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_box_decoration_break : box_decoration_break Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_box_decoration_break ctx v
  | Clone -> Pp.string ctx "clone"
  | Slice -> Pp.string ctx "slice"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_backface_visibility : backface_visibility Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_backface_visibility ctx v
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_border_collapse : border_collapse Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border_collapse ctx v
  | Collapse -> Pp.string ctx "collapse"
  | Separate -> Pp.string ctx "separate"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

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

let rec pp_field_sizing : field_sizing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_field_sizing ctx v
  | Content -> Pp.string ctx "content"
  | Fixed -> Pp.string ctx "fixed"
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

let rec pp_contain : contain Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Strict -> Pp.string ctx "strict"
  | Content -> Pp.string ctx "content"
  | Size -> Pp.string ctx "size"
  | Layout -> Pp.string ctx "layout"
  | Style -> Pp.string ctx "style"
  | Paint -> Pp.string ctx "paint"
  | Inline_size -> Pp.string ctx "inline-size"
  | List items -> Pp.list ~sep:Pp.space pp_contain ctx items
  | Var v -> pp_var pp_contain ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_container_type : container_type Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_container_type ctx v
  | Normal -> Pp.string ctx "normal"
  | Size -> Pp.string ctx "size"
  | Inline_size -> Pp.string ctx "inline-size"
  | Scroll_state -> Pp.string ctx "scroll-state"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_container_name : container_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_container_name ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.space Pp.string ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_anchor_name : anchor_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_anchor_name ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.comma Pp.string ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_position_anchor : position_anchor Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_anchor ctx v
  | Auto -> Pp.string ctx "auto"
  | Anchor name -> Pp.string ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_position_try_fallback : position_try_fallback Pp.t =
 fun ctx -> function
  | Flip_block -> Pp.string ctx "flip-block"
  | Flip_inline -> Pp.string ctx "flip-inline"
  | Flip_start -> Pp.string ctx "flip-start"
  | Name name -> Pp.string ctx name

let rec pp_position_try_fallbacks : position_try_fallbacks Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_try_fallbacks ctx v
  | None -> Pp.string ctx "none"
  | Fallbacks fallbacks ->
      Pp.list ~sep:Pp.comma pp_position_try_fallback ctx fallbacks
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_position_try_order : position_try_order Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_try_order ctx v
  | Normal -> Pp.string ctx "normal"
  | Most_width -> Pp.string ctx "most-width"
  | Most_height -> Pp.string ctx "most-height"
  | Most_block_size -> Pp.string ctx "most-block-size"
  | Most_inline_size -> Pp.string ctx "most-inline-size"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_position_try : position_try Pp.t =
 fun ctx -> function
  | Try (Normal, fallbacks) -> pp_position_try_fallbacks ctx fallbacks
  | Try (order, None) -> pp_position_try_order ctx order
  | Try (order, fallbacks) ->
      pp_position_try_order ctx order;
      Pp.space ctx ();
      pp_position_try_fallbacks ctx fallbacks
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_position_try ctx v

let pp_position_visibility_condition : position_visibility_condition Pp.t =
 fun ctx -> function
  | Anchors_visible -> Pp.string ctx "anchors-visible"
  | No_overflow -> Pp.string ctx "no-overflow"

let rec pp_position_visibility : position_visibility Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_visibility ctx v
  | Always -> Pp.string ctx "always"
  | Conditions conditions ->
      Pp.list ~sep:Pp.space pp_position_visibility_condition ctx conditions
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_position_area_keyword : position_area_keyword Pp.t =
 fun ctx -> function
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Center -> Pp.string ctx "center"
  | Span_top -> Pp.string ctx "span-top"
  | Span_bottom -> Pp.string ctx "span-bottom"
  | Span_left -> Pp.string ctx "span-left"
  | Span_right -> Pp.string ctx "span-right"
  | X_start -> Pp.string ctx "x-start"
  | X_end -> Pp.string ctx "x-end"
  | Y_start -> Pp.string ctx "y-start"
  | Y_end -> Pp.string ctx "y-end"
  | Span_x_start -> Pp.string ctx "span-x-start"
  | Span_x_end -> Pp.string ctx "span-x-end"
  | Span_y_start -> Pp.string ctx "span-y-start"
  | Span_y_end -> Pp.string ctx "span-y-end"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Block_start -> Pp.string ctx "block-start"
  | Block_end -> Pp.string ctx "block-end"
  | Span_inline_start -> Pp.string ctx "span-inline-start"
  | Span_inline_end -> Pp.string ctx "span-inline-end"
  | Span_block_start -> Pp.string ctx "span-block-start"
  | Span_block_end -> Pp.string ctx "span-block-end"
  | Span_all -> Pp.string ctx "span-all"

let rec pp_position_area : position_area Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_area ctx v
  | None -> Pp.string ctx "none"
  (* CSS Anchor Positioning 1 §6: the second axis defaults to [center], so [X
     center] minifies to [X]; both axes equal also collapse. *)
  | Area (first, Some second)
    when Pp.minified ctx && (first = second || second = Center) ->
      pp_position_area_keyword ctx first
  | Area (first, second) ->
      pp_position_area_keyword ctx first;
      Option.iter
        (fun keyword ->
          Pp.space ctx ();
          pp_position_area_keyword ctx keyword)
        second
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

let rec pp_scrollbar_width : scrollbar_width Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scrollbar_width ctx v
  | Auto -> Pp.string ctx "auto"
  | Thin -> Pp.string ctx "thin"
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scrollbar_color : scrollbar_color Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scrollbar_color ctx v
  | Auto -> Pp.string ctx "auto"
  | Colors (thumb, track) ->
      pp_color ctx thumb;
      Pp.space ctx ();
      pp_color ctx track
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scrollbar_gutter : scrollbar_gutter Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scrollbar_gutter ctx v
  | Auto -> Pp.string ctx "auto"
  | Stable -> Pp.string ctx "stable"
  | Stable_both_edges -> Pp.string ctx "stable both-edges"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_font_palette : font_palette Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_palette ctx v
  | Normal -> Pp.string ctx "normal"
  | Light -> Pp.string ctx "light"
  | Dark -> Pp.string ctx "dark"
  | Palette name -> Pp.string ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_synthesis_feature : font_synthesis_feature Pp.t =
 fun ctx -> function
  | Weight -> Pp.string ctx "weight"
  | Style -> Pp.string ctx "style"
  | Small_caps -> Pp.string ctx "small-caps"
  | Position -> Pp.string ctx "position"

let rec pp_font_synthesis : font_synthesis Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_synthesis ctx v
  | None -> Pp.string ctx "none"
  | Features features ->
      Pp.list ~sep:Pp.space pp_font_synthesis_feature ctx features
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let split_ws_words s =
  s |> String.split_on_char ' '
  |> List.filter (fun word -> String.length word > 0)

let canonical_scroll_timeline_args args =
  let words = split_ws_words args in
  let axes = [ "block"; "inline" ] in
  let scrollers = [ "nearest"; "root"; "self" ] in
  if
    List.for_all
      (fun word -> List.mem word axes || List.mem word scrollers)
      words
    && List.length words <= 2
  then
    let axis =
      Option.value
        (List.find_opt (fun word -> List.mem word axes) words)
        ~default:"block"
    in
    let scroller =
      Option.value
        (List.find_opt (fun word -> List.mem word scrollers) words)
        ~default:"nearest"
    in
    match (scroller, axis) with
    | "nearest", "block" -> ""
    | "nearest", axis -> axis
    | scroller, "block" -> scroller
    | scroller, axis -> scroller ^ " " ^ axis
  else args

let canonical_view_timeline_args args =
  match split_ws_words args with
  | [] -> ""
  | words ->
      let axis, insets =
        match words with
        | (("block" | "inline") as axis) :: rest -> (axis, rest)
        | words -> ("block", words)
      in
      let insets =
        match insets with
        | [] | [ "auto" ] | [ "auto"; "auto" ] -> []
        | [ first; second ] when first = second -> [ first ]
        | insets -> insets
      in
      String.concat " " ((if axis = "block" then [] else [ axis ]) @ insets)

let rec pp_animation_timeline : animation_timeline Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_timeline ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Name name -> Pp.string ctx name
  | Scroll args ->
      Pp.string ctx "scroll(";
      Pp.string ctx (canonical_scroll_timeline_args args);
      Pp.char ctx ')'
  | View args ->
      Pp.string ctx "view(";
      Pp.string ctx (canonical_view_timeline_args args);
      Pp.char ctx ')'
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_animation_range_name : animation_range_name Pp.t =
 fun ctx -> function
  | Cover -> Pp.string ctx "cover"
  | Contain -> Pp.string ctx "contain"
  | Entry -> Pp.string ctx "entry"
  | Exit -> Pp.string ctx "exit"
  | Entry_crossing -> Pp.string ctx "entry-crossing"
  | Exit_crossing -> Pp.string ctx "exit-crossing"

let rec pp_animation_range_item : animation_range_item Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Offset lp -> pp_length_percentage ~always:true ctx lp
  | Named (name, None) -> pp_animation_range_name ctx name
  | Named (name, Some lp) ->
      pp_animation_range_name ctx name;
      Pp.space ctx ();
      pp_length_percentage ~always:true ctx lp
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_animation_range_item ctx v

let animation_range_boundary_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
  | _ -> false

let animation_range_needs_space ctx =
  if not (Pp.minified ctx) then true
  else
    match Pp.last_char ctx with
    | None -> true
    | Some c -> animation_range_boundary_char c

let rec pp_animation_range : animation_range Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_animation_range ctx v
  | Range (first, None) -> pp_animation_range_item ctx first
  | Range (first, Some Normal) -> pp_animation_range_item ctx first
  | Range
      ( Named (start_name, Some start_offset),
        Some (Named (end_name, Some end_offset)) )
    when start_name = end_name
         && start_offset = (Pct 0. : length_percentage)
         && end_offset = (Pct 100. : length_percentage) ->
      pp_animation_range_item ctx (Named (start_name, Some start_offset))
  | Range (first, Some second) ->
      pp_animation_range_item ctx first;
      if animation_range_needs_space ctx then Pp.space ctx ();
      pp_animation_range_item ctx second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_view_transition_name : view_transition_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_view_transition_name ctx v
  | None -> Pp.string ctx "none"
  | Match_element -> Pp.string ctx "match-element"
  | Name name -> Pp.string ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_view_transition_class : view_transition_class Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_view_transition_class ctx v
  | None -> Pp.string ctx "none"
  | Classes classes -> Pp.list ~sep:Pp.space Pp.string ctx classes
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_image_orientation : image_orientation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_orientation ctx v
  | None -> Pp.string ctx "none"
  | From_image -> Pp.string ctx "from-image"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_image_rendering : image_rendering Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_rendering ctx v
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | High_quality -> Pp.string ctx "high-quality"
  | Crisp_edges -> Pp.string ctx "crisp-edges"
  | Pixelated -> Pp.string ctx "pixelated"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_resolution ctx = function
  | Dpi f ->
      Pp.float ctx f;
      Pp.string ctx "dpi"
  | Dpcm f ->
      Pp.float ctx f;
      Pp.string ctx "dpcm"
  | Dppx f ->
      Pp.float ctx f;
      Pp.string ctx "dppx"
  | X f ->
      Pp.float ctx f;
      Pp.char ctx 'x'

let rec pp_image_resolution : image_resolution Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_resolution ctx v
  | Resolution r -> pp_resolution ctx r
  | From_image -> Pp.string ctx "from-image"
  | From_image_resolution r ->
      Pp.string ctx "from-image";
      Pp.space ctx ();
      pp_resolution ctx r
  | Snap r ->
      pp_resolution ctx r;
      Pp.space ctx ();
      Pp.string ctx "snap"
  | From_image_snap -> Pp.string ctx "from-image snap"
  | From_image_snap_resolution r ->
      Pp.string ctx "from-image";
      Pp.space ctx ();
      pp_resolution ctx r;
      Pp.space ctx ();
      Pp.string ctx "snap"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_contain_intrinsic_size_item : contain_intrinsic_size_item Pp.t =
 fun ctx -> function
  | Length len -> pp_length ~always:true ctx len
  | Auto len ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      pp_length ~always:true ctx len

let rec pp_contain_intrinsic_size : contain_intrinsic_size Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_contain_intrinsic_size ctx v
  | None -> Pp.string ctx "none"
  | Intrinsic (first, None) -> pp_contain_intrinsic_size_item ctx first
  | Intrinsic (first, Some second) ->
      pp_contain_intrinsic_size_item ctx first;
      Pp.space ctx ();
      pp_contain_intrinsic_size_item ctx second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_contain_intrinsic_longhand : contain_intrinsic_longhand Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_contain_intrinsic_longhand ctx v
  | None -> Pp.string ctx "none"
  | Size size -> pp_contain_intrinsic_size_item ctx size
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_margin_trim_edge : margin_trim_edge Pp.t =
 fun ctx -> function
  | Block_start -> Pp.string ctx "block-start"
  | Inline_start -> Pp.string ctx "inline-start"
  | Block_end -> Pp.string ctx "block-end"
  | Inline_end -> Pp.string ctx "inline-end"

let pp_margin_trim_axis : margin_trim_axis Pp.t =
 fun ctx -> function
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"

let rec pp_margin_trim : margin_trim Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_margin_trim ctx v
  | None -> Pp.string ctx "none"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Axes axes -> Pp.list ~sep:Pp.space pp_margin_trim_axis ctx axes
  | Edges edges -> Pp.list ~sep:Pp.space pp_margin_trim_edge ctx edges
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_ray_size : ray_size Pp.t =
 fun ctx -> function
  | Radial Closest_side -> Pp.string ctx "closest-side"
  | Radial Closest_corner -> Pp.string ctx "closest-corner"
  | Radial Farthest_side -> Pp.string ctx "farthest-side"
  | Radial Farthest_corner -> Pp.string ctx "farthest-corner"
  | Radial _ -> invalid_arg "pp_ray_size: invalid radial size for ray()"
  | Sides -> Pp.string ctx "sides"

let pp_ray : ray Pp.t =
 fun ctx ({ angle; size; contain; position } : ray) ->
  pp_angle ctx angle;
  Option.iter
    (fun size ->
      Pp.space ctx ();
      pp_ray_size ctx size)
    size;
  if contain then (
    Pp.space ctx ();
    Pp.string ctx "contain");
  Option.iter
    (fun position ->
      Pp.space ctx ();
      Pp.string ctx "at";
      Pp.space ctx ();
      pp_position_value ctx position)
    position

let rec pp_offset_path : offset_path Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_offset_path ctx v
  | None -> Pp.string ctx "none"
  | Url url -> Pp.url ctx url
  | Path path -> Pp.call "path" Pp.quoted_string ctx path
  | Ray ray -> Pp.call "ray" pp_ray ctx ray
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_offset_rotate_mode : offset_rotate_mode Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Reverse -> Pp.string ctx "reverse"

let rec pp_offset_rotate : offset_rotate Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Reverse -> Pp.string ctx "reverse"
  | Angle angle -> pp_angle ctx angle
  | With_angle (mode, angle) ->
      pp_offset_rotate_mode ctx mode;
      Pp.space ctx ();
      pp_angle ctx angle
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_offset_rotate ctx v

let read_offset_rotate_mode t : offset_rotate_mode =
  Cursor.enum "offset-rotate mode"
    [ ("auto", (Auto : offset_rotate_mode)); ("reverse", Reverse) ]
    t

let rec read_offset_rotate t : offset_rotate =
  let read_var t : offset_rotate = Var (read_var read_offset_rotate t) in
  let read_mode_first t : offset_rotate =
    let mode = read_offset_rotate_mode t in
    Cursor.ws t;
    match Cursor.option read_angle t with
    | Some angle -> With_angle (mode, angle)
    | None -> (
        match mode with Auto -> (Auto : offset_rotate) | Reverse -> Reverse)
  in
  let read_angle_first t : offset_rotate =
    let angle = read_angle t in
    Cursor.ws t;
    match Cursor.option read_offset_rotate_mode t with
    | Some mode -> With_angle (mode, angle)
    | None -> Angle angle
  in
  Cursor.enum_or_calls "offset-rotate"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t -> Cursor.one_of [ read_mode_first; read_angle_first ] t)
    t

let rec pp_container_shorthand : container_shorthand Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_container_shorthand ctx v
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Shorthand { name; ctype } -> (
      match (name, ctype) with
      | None, None -> () (* Should not happen, but emit nothing *)
      | Some n, None -> Pp.string ctx n
      | None, Some t -> pp_container_type ctx t
      | Some n, Some t ->
          Pp.string ctx n;
          Pp.sp ctx ();
          Pp.char ctx '/';
          Pp.sp ctx ();
          pp_container_type ctx t)

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
  | Open_quote -> Pp.string ctx "open-quote"
  | Close_quote -> Pp.string ctx "close-quote"
  | Attr attr -> Pp.call "attr" (Values.pp_attr_call pp_content) ctx attr
  | Counter name -> Pp.call "counter" Pp.string ctx name
  | String_ref name -> Pp.call "string" Pp.string ctx name
  | Counters (name, separator) ->
      Pp.string ctx "counters(";
      Pp.string ctx name;
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
  Pp.string ctx name;
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

let rec pp_content_visibility : content_visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Auto -> Pp.string ctx "auto"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_content_visibility ctx v

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

let rec pp_cursor : cursor Pp.t =
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
  | Url (url, coords, fallback) ->
      Pp.url ctx url;
      (match coords with
      | Some (x, y) ->
          Pp.char ctx ' ';
          Pp.float ctx x;
          Pp.char ctx ' ';
          Pp.float ctx y
      | None -> ());
      Pp.comma ctx ();
      pp_cursor ctx fallback
  | Var v -> pp_var pp_cursor ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_interactivity : interactivity Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Inert -> Pp.string ctx "inert"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_interactivity ctx v

let rec pp_caret_animation : caret_animation Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Manual -> Pp.string ctx "manual"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_caret_animation ctx v

let rec pp_caret_shape : caret_shape Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Bar -> Pp.string ctx "bar"
  | Block -> Pp.string ctx "block"
  | Underscore -> Pp.string ctx "underscore"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_caret_shape ctx v

let rec pp_caret : caret Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Caret (color, animation, shape) ->
      let items =
        ( ( [] |> fun items ->
            match color with
            | None -> items
            | Some value -> (fun ctx -> pp_color ctx value) :: items )
        |> fun items ->
          match animation with
          | None -> items
          | Some value -> (fun ctx -> pp_caret_animation ctx value) :: items )
        |> fun items ->
        match shape with
        | None -> items
        | Some value -> (fun ctx -> pp_caret_shape ctx value) :: items
      in
      Pp.list ~sep:Pp.space (fun ctx pp -> pp ctx) ctx (List.rev items)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_caret ctx v

let rec pp_interest_delay : interest_delay Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Durations durations ->
      Pp.list ~sep:Pp.space pp_duration_preserve_ms ctx durations
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_interest_delay ctx v

let pp_nav_scope ctx = function
  | Current -> Pp.string ctx "current"
  | Root -> Pp.string ctx "root"
  | Named name -> Pp.quoted_string ctx name

let rec pp_nav : nav Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Target (target, scope) ->
      Pp.char ctx '#';
      Pp.string ctx target;
      Option.iter
        (fun scope ->
          Pp.space ctx ();
          pp_nav_scope ctx scope)
        scope
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_nav ctx v

let rec pp_direction : direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_direction ctx v
  | Ltr -> Pp.string ctx "ltr"
  | Rtl -> Pp.string ctx "rtl"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_isolation : isolation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_isolation ctx v
  | Auto -> Pp.string ctx "auto"
  | Isolate -> Pp.string ctx "isolate"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_break_value : break_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_break_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Avoid -> Pp.string ctx "avoid"
  | All -> Pp.string ctx "all"
  | Avoid_page -> Pp.string ctx "avoid-page"
  | Page -> Pp.string ctx "page"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Recto -> Pp.string ctx "recto"
  | Verso -> Pp.string ctx "verso"
  | Avoid_column -> Pp.string ctx "avoid-column"
  | Column -> Pp.string ctx "column"
  | Avoid_region -> Pp.string ctx "avoid-region"
  | Region -> Pp.string ctx "region"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_break_inside_value : break_inside_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_break_inside_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Avoid -> Pp.string ctx "avoid"
  | Avoid_page -> Pp.string ctx "avoid-page"
  | Avoid_column -> Pp.string ctx "avoid-column"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_page_break_value : page_break_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_break_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Always -> Pp.string ctx "always"
  | Avoid -> Pp.string ctx "avoid"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_page_break_inside_value : page_break_inside_value Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_break_inside_value ctx v
  | Auto -> Pp.string ctx "auto"
  | Avoid -> Pp.string ctx "avoid"
  | Inherit -> Pp.string ctx "inherit"

let break_of_page_break (value : page_break_value) : break_value =
  match value with
  | Auto -> Auto
  | Always -> Page
  | Avoid -> Avoid
  | Left -> Left
  | Right -> Right
  | Inherit -> Inherit
  | Var _ -> invalid_arg "page-break value var cannot be converted"

let break_inside_of_page_break (value : page_break_inside_value) :
    break_inside_value =
  match value with
  | Auto -> Auto
  | Avoid -> Avoid
  | Inherit -> Inherit
  | Var _ -> invalid_arg "page-break-inside value var cannot be converted"

let rec pp_columns_value : columns_value Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Count n -> Pp.int ctx n
  | Width len -> pp_length ctx len
  | Both (len, n) ->
      pp_length ctx len;
      Pp.space ctx ();
      Pp.int ctx n
  | Auto_count n ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      Pp.int ctx n
  | Var v -> pp_var pp_columns_value ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_width : column_width Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Width len -> pp_length ctx len
  | Var v -> pp_var pp_column_width ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_count : column_count Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Count n -> Pp.int ctx n
  | Var v -> pp_var pp_column_count ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_column_span : column_span Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_column_span ctx v

let rec pp_scroll_snap_align : scroll_snap_align Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scroll_snap_align ctx v
  | None -> Pp.string ctx "none"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Snap_align_pair (block, inline) ->
      pp_scroll_snap_align ctx block;
      Pp.space ctx ();
      pp_scroll_snap_align ctx inline

let rec pp_timeline_axis : timeline_axis Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_axis ctx v
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | X -> Pp.string ctx "x"
  | Y -> Pp.string ctx "y"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_timeline_name : timeline_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_name ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.comma Pp.string ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_timeline_shorthand_item : timeline_shorthand_item Pp.t =
 fun ctx { name; axis } ->
  Pp.string ctx name;
  Pp.space ctx ();
  pp_timeline_axis ctx axis

let rec pp_timeline_shorthand : timeline_shorthand Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Timelines items ->
      Pp.list ~sep:Pp.comma pp_timeline_shorthand_item ctx items
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_timeline_shorthand ctx v

let pp_timeline_inset_item : timeline_inset_item Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Length lp -> pp_length_percentage ~always:true ctx lp

let rec pp_timeline_inset : timeline_inset Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_inset ctx v
  | Inset (first, second) ->
      pp_timeline_inset_item ctx first;
      Option.iter
        (fun item ->
          Pp.space ctx ();
          pp_timeline_inset_item ctx item)
        second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_page_size_name : page_size_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_size_name ctx v
  | A5 -> Pp.string ctx "A5"
  | A4 -> Pp.string ctx "A4"
  | A3 -> Pp.string ctx "A3"
  | B5 -> Pp.string ctx "B5"
  | B4 -> Pp.string ctx "B4"
  | Jis_b5 -> Pp.string ctx "JIS-B5"
  | Jis_b4 -> Pp.string ctx "JIS-B4"
  | Letter -> Pp.string ctx "letter"
  | Legal -> Pp.string ctx "legal"
  | Ledger -> Pp.string ctx "ledger"

let rec pp_page_size_orientation : page_size_orientation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_page_size_orientation ctx v
  | Portrait -> Pp.string ctx "portrait"
  | Landscape -> Pp.string ctx "landscape"

let rec pp_page_size : page_size Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Single len -> pp_length ctx len
  | Pair (width, height) ->
      pp_length ctx width;
      Pp.space ctx ();
      pp_length ctx height
  | Named name -> pp_page_size_name ctx name
  | Named_oriented (name, orientation) ->
      pp_page_size_name ctx name;
      Pp.space ctx ();
      pp_page_size_orientation ctx orientation
  | Oriented orientation -> pp_page_size_orientation ctx orientation
  | Inherit -> Pp.string ctx "inherit"
  | Var v -> pp_var pp_page_size ctx v

let rec pp_scroll_snap_stop : scroll_snap_stop Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scroll_snap_stop ctx v
  | Normal -> Pp.string ctx "normal"
  | Always -> Pp.string ctx "always"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scroll_behavior : scroll_behavior Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scroll_behavior ctx v
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_resize : resize Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_resize ctx v
  | None -> Pp.string ctx "none"
  | Both -> Pp.string ctx "both"
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
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

let rec pp_font_stretch : font_stretch Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_stretch ctx v
  | Pct f -> Pp.pct ctx f
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
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_font_size_adjust_metric : font_size_adjust_metric Pp.t =
 fun ctx -> function
  | Ex_height -> Pp.string ctx "ex-height"
  | Cap_height -> Pp.string ctx "cap-height"
  | Ch_width -> Pp.string ctx "ch-width"
  | Ic_width -> Pp.string ctx "ic-width"
  | Ic_height -> Pp.string ctx "ic-height"

let rec pp_font_size_adjust : font_size_adjust Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Number f -> Pp.float ctx f
  | From_font -> Pp.string ctx "from-font"
  | Metric_number (metric, f) ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.float ctx f
  | Metric_from_font metric ->
      pp_font_size_adjust_metric ctx metric;
      Pp.space ctx ();
      Pp.string ctx "from-font"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_size_adjust ctx v

let rec pp_font_variant_emoji : font_variant_emoji Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Text -> Pp.string ctx "text"
  | Emoji -> Pp.string ctx "emoji"
  | Unicode -> Pp.string ctx "unicode"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_variant_emoji ctx v

let rec pp_font_display : font_display Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_display ctx v
  | Auto -> Pp.string ctx "auto"
  | Block -> Pp.string ctx "block"
  | Swap -> Pp.string ctx "swap"
  | Fallback -> Pp.string ctx "fallback"
  | Optional -> Pp.string ctx "optional"

let hex_string n =
  let rec loop n acc =
    let digit n =
      if n < 10 then Char.chr (Char.code '0' + n)
      else Char.chr (Char.code 'A' + n - 10)
    in
    if n = 0 && acc = [] then "0"
    else if n = 0 then String.of_seq (List.to_seq acc)
    else loop (n / 16) (digit (n mod 16) :: acc)
  in
  loop n []

let padded_hex width n =
  let hex = hex_string n in
  if String.length hex >= width then hex
  else String.make (width - String.length hex) '0' ^ hex

let pp_unicode_range_range ctx start end_ =
  Pp.string ctx "U+";
  Pp.hex ctx start;
  Pp.char ctx '-';
  Pp.hex ctx end_

let unicode_range_wildcard start end_ : string option =
  let wildcard_for q : string option =
    let size = 1 lsl (4 * q) in
    if start mod size <> 0 || end_ <> start + size - 1 then None
    else
      let prefix = start / size in
      let prefix = if prefix = 0 then "" else hex_string prefix in
      let wildcard = "U+" ^ prefix ^ String.make q '?' in
      let range = "U+" ^ hex_string start ^ "-" ^ hex_string end_ in
      if String.length wildcard < String.length range then
        (Some wildcard : string option)
      else (None : string option)
  in
  let rec loop q =
    if q > 6 then (None : string option)
    else
      match wildcard_for q with
      | (Some _ : string option) as wildcard -> wildcard
      | None -> loop (q + 1)
  in
  loop 1

let rec pp_unicode_range : unicode_range Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_unicode_range ctx v
  | Single hex ->
      Pp.string ctx "U+";
      Pp.hex ctx hex
  | Range (start, end_) -> (
      let pp_range () = pp_unicode_range_range ctx start end_ in
      if not (Pp.minified ctx) then pp_range ()
      else
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_range ())
  | Padded_single (value, width) ->
      if Pp.minified ctx then pp_unicode_range ctx (Single value)
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex width value))
  | Padded_range { start; end_; start_width; end_width } ->
      if Pp.minified ctx then pp_unicode_range ctx (Range (start, end_))
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex start_width start);
        Pp.char ctx '-';
        Pp.string ctx (padded_hex end_width end_))
  | Wildcard { prefix; prefix_width; wildcards } ->
      let start = prefix lsl (4 * wildcards) in
      let end_ = start + (1 lsl (4 * wildcards)) - 1 in
      if Pp.minified ctx then
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_unicode_range_range ctx start end_
      else (
        Pp.string ctx "U+";
        if prefix_width > 0 then Pp.string ctx (padded_hex prefix_width prefix);
        Pp.string ctx (String.make wildcards '?'))

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

let rec pp_font_variant_numeric : font_variant_numeric Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
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
      (* Print all 5 variables, including None values The Empty fallback in vars
         will produce var(--name,) *)
      let tokens =
        List.filter_map Fun.id
          [
            ordinal;
            slashed_zero;
            numeric_figure;
            numeric_spacing;
            numeric_fraction;
          ]
      in
      Pp.list ~sep:Pp.space pp_font_variant_numeric_token ctx tokens

let rec pp_text_size_adjust : text_size_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_size_adjust ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Pct n -> Pp.pct ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_font_smoothing : webkit_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Antialiased -> Pp.string ctx "antialiased"
  | Subpixel_antialiased -> Pp.string ctx "subpixel-antialiased"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scroll_snap_strictness : scroll_snap_strictness Pp.t =
 fun ctx -> function
  | Proximity -> Pp.string ctx "proximity"
  | Mandatory -> Pp.string ctx "mandatory"
  | Var v -> pp_var pp_scroll_snap_strictness ctx v

let rec pp_scroll_snap_axis : scroll_snap_axis Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | X -> Pp.string ctx "x"
  | Y -> Pp.string ctx "y"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Both -> Pp.string ctx "both"
  | Var v -> pp_var pp_scroll_snap_axis ctx v

let rec pp_scroll_snap_type : scroll_snap_type Pp.t =
 fun ctx -> function
  | Axis axis -> pp_scroll_snap_axis ctx axis
  | Axis_with_strictness (axis, strictness) -> (
      pp_scroll_snap_axis ctx axis;
      match axis with
      | None | Var _ -> () (* "none" and vars don't take strictness *)
      | _ ->
          Pp.space ctx ();
          pp_scroll_snap_strictness ctx strictness)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_scroll_snap_type ctx v

let rec pp_repeat_count ctx (count : repeat_count) =
  match count with
  | Count n -> Pp.int ctx n
  | Auto_fill -> Pp.string ctx "auto-fill"
  | Auto_fit -> Pp.string ctx "auto-fit"
  | Var v -> pp_var pp_repeat_count ctx v

let pp_grid_auto_flow_shorthand ctx = function
  | Row | Column -> Pp.string ctx "auto-flow"
  | Row_dense | Column_dense ->
      Pp.string ctx "auto-flow";
      Pp.space ctx ();
      Pp.string ctx "dense"
  | Dense ->
      Pp.string ctx "auto-flow";
      Pp.space ctx ();
      Pp.string ctx "dense"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_grid_auto_flow ctx v

let rec pp_grid_template : grid_template Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct f -> Pp.pct ctx f
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Zero -> Pp.char ctx '0'
  | Length l -> pp_length ctx l
  (* CSS Grid 2 sec. 7.2: [<flex>] is [<number>fr]; the unit-drop rule is for
     [<length>] only. [0fr] is a zero flex factor, distinct from a [0]
     [<length>] in [grid-template]'s union grammar. *)
  | Fr f ->
      Pp.float ctx f;
      Pp.string ctx "fr"
  | Auto -> Pp.string ctx "auto"
  | Min_content -> Pp.string ctx "min-content"
  | Max_content -> Pp.string ctx "max-content"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Min_max (min, max) ->
      Pp.call_2 "minmax" pp_grid_template pp_grid_template ctx (min, max)
  | Fit_content l -> Pp.call "fit-content" pp_length ctx l
  | Repeat (count, sizes) ->
      Pp.call "repeat"
        (fun ctx (count, sizes) ->
          pp_repeat_count ctx count;
          Pp.comma ctx ();
          pp_grid_track_list ctx sizes)
        ctx (count, sizes)
  | Tracks sizes -> pp_grid_track_list ctx sizes
  | Split (rows, columns) ->
      pp_grid_template ctx rows;
      Pp.slash ctx ();
      pp_grid_template ctx columns
  | Auto_flow_columns (rows, flow, auto_columns) ->
      pp_grid_template ctx rows;
      Pp.slash ctx ();
      pp_grid_auto_flow_shorthand ctx flow;
      Option.iter
        (fun columns ->
          Pp.space ctx ();
          pp_grid_template ctx columns)
        auto_columns
  | Auto_flow_rows (Row, None, columns) ->
      Pp.string ctx "none";
      Pp.slash ctx ();
      pp_grid_template ctx columns
  | Auto_flow_rows (flow, auto_rows, columns) ->
      pp_grid_auto_flow_shorthand ctx flow;
      Option.iter
        (fun rows ->
          Pp.space ctx ();
          pp_grid_template ctx rows)
        auto_rows;
      Pp.slash ctx ();
      pp_grid_template ctx columns
  | Named_tracks tracks ->
      let pp_named_track ctx (name, size) =
        (match name with
        | Some n ->
            Pp.char ctx '[';
            Pp.string ctx n;
            Pp.string ctx "] "
        | None -> ());
        pp_grid_template ctx size
      in
      Pp.list ~sep:Pp.space pp_named_track ctx tracks
  | Line_names names ->
      Pp.char ctx '[';
      Pp.list ~sep:Pp.space Pp.string ctx names;
      Pp.char ctx ']'
  | Template raw ->
      (* The complex grid-template syntax (with [<grid-template-areas>] string
         tracks) is stored as a canonical component-value slice because the
         typed grammar can't easily express it yet. *)
      Pp.string ctx raw
  | Subgrid -> Pp.string ctx "subgrid"
  | Masonry -> Pp.string ctx "masonry"
  | Var v -> pp_var pp_grid_template ctx v

and pp_grid_track_list ctx tracks =
  (* Track-list items are separated by whitespace, but [[...]] blocks have
     self-delimiting brackets, so [[col-start]minmax(...)] tokenises the same as
     [[col-start] minmax(...)]. Drop the inter-item space whenever the previous
     output ends with [\]] or [\)] (line-name block / function call) or the next
     item is a line-name block - matching the lightning / csso minified
     spelling. *)
  let buf_last_char ctx : char option = Pp.last_char ctx in
  let starts_with_bracket = function Line_names _ -> true | _ -> false in
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp_grid_template ctx x
    | x :: y :: rest ->
        pp_grid_template ctx x;
        let needs_space =
          if not (Pp.minified ctx) then true
          else
            match buf_last_char ctx with
            | Some (']' | ')') -> false
            | _ -> not (starts_with_bracket y)
        in
        if needs_space then Pp.space ctx ();
        loop (y :: rest)
  in
  loop tracks

let rec normalize_grid_template (value : grid_template) : grid_template =
  match value with
  | Px 0. | Rem 0. | Em 0. | Vw 0. | Vh 0. | Vmin 0. | Vmax 0. -> Zero
  | Min_max (min, max) ->
      let min' = normalize_grid_template min in
      let max' = normalize_grid_template max in
      if min' == min && max' == max then value else Min_max (min', max')
  | Fit_content length ->
      let length' = Values.normalize_length length in
      if length' == length then value else Fit_content length'
  | Repeat (count, sizes) ->
      let sizes' = map_preserve normalize_grid_template sizes in
      if sizes' == sizes then value else Repeat (count, sizes')
  | Tracks sizes ->
      let sizes' = map_preserve normalize_grid_template sizes in
      if sizes' == sizes then value else Tracks sizes'
  | Split (rows, columns) ->
      let rows' = normalize_grid_template rows in
      let columns' = normalize_grid_template columns in
      if rows' == rows && columns' == columns then value
      else Split (rows', columns')
  | Auto_flow_columns (rows, flow, columns) ->
      let rows' = normalize_grid_template rows in
      let columns' = option_map_preserve normalize_grid_template columns in
      if rows' == rows && columns' == columns then value
      else Auto_flow_columns (rows', flow, columns')
  | Auto_flow_rows (flow, rows, columns) ->
      let rows' = option_map_preserve normalize_grid_template rows in
      let columns' = normalize_grid_template columns in
      if rows' == rows && columns' == columns then value
      else Auto_flow_rows (flow, rows', columns')
  | Named_tracks tracks ->
      let tracks' =
        map_preserve
          (fun ((name, track) as item) ->
            let track' = normalize_grid_template track in
            if track' == track then item else (name, track'))
          tracks
      in
      if tracks' == tracks then value else Named_tracks tracks'
  | _ -> value

let grid_area_row_ws = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let grid_template_area_row_cells row =
  let row_len = String.length row in
  let rec skip_ws i =
    if i < row_len && grid_area_row_ws row.[i] then skip_ws (i + 1) else i
  in
  let rec take_cell start i =
    if i < row_len && not (grid_area_row_ws row.[i]) then take_cell start (i + 1)
    else (String.sub row start (i - start), i)
  in
  let rec loop acc i =
    let start = skip_ws i in
    if start >= row_len then List.rev acc
    else
      let cell, next = take_cell start start in
      loop (cell :: acc) next
  in
  loop [] 0

(* CSS Grid Layout 2 §7.3: a "null cell token" is one or more sequential
   periods, all denoting the same single empty cell. Collapse multi-dot
   spellings ([....] / [..]) to the canonical single [.]. *)
let normalize_grid_template_area_cell c =
  let n = String.length c in
  let rec all_dots i = i >= n || (c.[i] = '.' && all_dots (i + 1)) in
  if n > 1 && all_dots 0 then "." else c

let minify_grid_template_area_row row =
  String.concat " "
    (List.map normalize_grid_template_area_cell
       (grid_template_area_row_cells row))

let minify_grid_template_areas_string value =
  let len = String.length value in
  let buf = Buffer.create len in
  let rec take_quoted quote start i =
    if i >= len then (String.sub value start (i - start), i)
    else if value.[i] = quote then (String.sub value start (i - start), i + 1)
    else take_quoted quote start (i + 1)
  in
  let rec loop i =
    if i < len then
      match value.[i] with
      | ('"' | '\'') as quote ->
          let row, next = take_quoted quote (i + 1) (i + 1) in
          Buffer.add_char buf '"';
          Buffer.add_string buf (minify_grid_template_area_row row);
          Buffer.add_char buf '"';
          loop next
      | ' ' | '\t' | '\n' | '\r' | '\012' -> loop (i + 1)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents buf

let rec pp_grid_template_areas : grid_template_areas Pp.t =
 fun ctx -> function
  | No_areas -> Pp.string ctx "none"
  | Areas value ->
      if Pp.minified ctx then
        Pp.string ctx (minify_grid_template_areas_string value)
      else Pp.string ctx value
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_grid_template_areas ctx v

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
  | Ric f -> Pp.unit ctx f "ric"
  | Rlh f -> Pp.unit ctx f "rlh"
  | Pct f -> Pp.pct ctx f
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
  | Fit_content_arg length ->
      Pp.string ctx "fit-content(";
      pp_length ctx length;
      Pp.char ctx ')'
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | From_font -> Pp.string ctx "from-font"
  (* Math functions mirror [length]; reuse its printer. *)
  | Clamp (a, b, c) -> pp_length ctx (Clamp (a, b, c))
  | Min xs -> pp_length ctx (Min xs)
  | Max xs -> pp_length ctx (Max xs)
  | Round (s, a, b) -> pp_length ctx (Round (s, a, b))
  | Mod (a, b) -> pp_length ctx (Mod (a, b))
  | Rem_fn (a, b) -> pp_length ctx (Rem_fn (a, b))
  | Hypot xs -> pp_length ctx (Hypot xs)
  | Abs a -> pp_length ctx (Abs a)
  | Var v -> pp_var pp_flex_basis ctx v
  | Calc cv -> pp_calc pp_flex_basis ctx cv

let rec pp_flex : flex Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex ctx v
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Grow f -> pp_flex_factor ctx f
  | Basis fb -> pp_flex_basis ctx fb
  | Grow_shrink (grow, shrink) ->
      pp_flex_factor ctx grow;
      Pp.space ctx ();
      pp_flex_factor ctx shrink
  | Full (grow, shrink, basis) ->
      pp_flex_factor ctx grow;
      (* CSS Flexbox 1 sec. 7.1.1: the one-number [flex: g] form expands to [g 1
         0%], so only a [0%] basis (with the default [1] shrink) is the
         droppable shorthand default. A length [0] / [0px] basis is a different
         computed value and is kept. An omitted flex-shrink is [1]. *)
      let basis_is_default = match basis with Pct 0.0 -> true | _ -> false in
      let shrink_is_default =
        match shrink with Number 1.0 -> true | _ -> false
      in
      if basis_is_default then (
        if not shrink_is_default then (
          Pp.space ctx ();
          pp_flex_factor ctx shrink))
      else (
        Pp.space ctx ();
        (* A basis that serialises as a bare number ([0], [0px] -> [0], or a
           unitless basis) would reparse as the shrink factor, so keep the
           shrink to disambiguate. *)
        let basis_bare_number =
          match basis with Num _ | Zero | Px 0.0 -> true | _ -> false
        in
        if (not shrink_is_default) || basis_bare_number then (
          pp_flex_factor ctx shrink;
          Pp.space ctx ());
        pp_flex_basis ctx basis)

let rec pp_font_size : font_size Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_font_size ctx v
  | Calc c -> (
      let rec to_length_calc : font_size calc -> length calc option = function
        | Val (Length l) -> Some (Val l)
        | Val (Pct n) -> Some (Val (Pct n : length))
        | Num n -> Some (Num n)
        | Math_const c -> Some (Math_const c)
        | Math_fn fn -> Some (Math_fn fn)
        | Var _ | Sibling_index | Sibling_count | Val _ -> None
        | Nested inner -> Option.map (fun c -> Nested c) (to_length_calc inner)
        | Parens inner -> Option.map (fun c -> Parens c) (to_length_calc inner)
        | Expr (left, op, right) -> (
            match (to_length_calc left, to_length_calc right) with
            | Some left, Some right -> Some (Expr (left, op, right))
            | _ -> None)
      in
      match (Pp.minified ctx, to_length_calc c) with
      | true, Some c -> pp_length ctx (Calc c)
      | _ -> pp_calc pp_font_size ctx c)
  | Xx_small -> Pp.string ctx "xx-small"
  | X_small -> Pp.string ctx "x-small"
  | Small -> Pp.string ctx "small"
  | Medium -> Pp.string ctx "medium"
  | Large -> Pp.string ctx "large"
  | X_large -> Pp.string ctx "x-large"
  | Xx_large -> Pp.string ctx "xx-large"
  | Xxx_large -> Pp.string ctx "xxx-large"
  | Larger -> Pp.string ctx "larger"
  | Smaller -> Pp.string ctx "smaller"
  | Math -> Pp.string ctx "math"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_align_content : align_content Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_align_content ctx v
  | Normal -> Pp.string ctx "normal"
  | Baseline -> Pp.string ctx "baseline"
  | First_baseline -> Pp.string ctx "first baseline"
  | Last_baseline -> Pp.string ctx "last baseline"
  | Center -> Pp.string ctx "center"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Flex_start -> Pp.string ctx "flex-start"
  | Flex_end -> Pp.string ctx "flex-end"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_flex_start -> Pp.string ctx "safe flex-start"
  | Safe_flex_end -> Pp.string ctx "safe flex-end"
  | Safe_left -> Pp.string ctx "safe left"
  | Safe_right -> Pp.string ctx "safe right"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_flex_start -> Pp.string ctx "unsafe flex-start"
  | Unsafe_flex_end -> Pp.string ctx "unsafe flex-end"
  | Unsafe_left -> Pp.string ctx "unsafe left"
  | Unsafe_right -> Pp.string ctx "unsafe right"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Stretch -> Pp.string ctx "stretch"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_place_content : place_content Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_place_content ctx v
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_stretch -> Pp.string ctx "safe stretch"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_stretch -> Pp.string ctx "unsafe stretch"
  | Align_justify (a, j) ->
      let a_s = Pp.to_string ~minify:(Pp.minified ctx) pp_align_content a in
      let j_s = Pp.to_string ~minify:(Pp.minified ctx) pp_justify_content j in
      if Pp.minified ctx && a_s = j_s then Pp.string ctx a_s
      else (
        Pp.string ctx a_s;
        Pp.space ctx ();
        Pp.string ctx j_s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_place_items : place_items Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_place_items ctx v
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Baseline -> Pp.string ctx "baseline"
  | Start_safe -> Pp.string ctx "safe start"
  | End_safe -> Pp.string ctx "safe end"
  | Center_safe -> Pp.string ctx "safe center"
  | Stretch_stretch when Pp.minified ctx -> Pp.string ctx "stretch"
  | Stretch_stretch -> Pp.string ctx "stretch stretch"
  | Align_justify (a, j) ->
      let a_s = Pp.to_string ~minify:(Pp.minified ctx) pp_align_items a in
      let j_s = Pp.to_string ~minify:(Pp.minified ctx) pp_justify_items j in
      (* CSS Align 3 §6.1: when align and justify render to the same token, the
         single-value spelling is canonical. *)
      if Pp.minified ctx && a_s = j_s then Pp.string ctx a_s
      else (
        Pp.string ctx a_s;
        Pp.space ctx ();
        Pp.string ctx j_s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_moz_osx_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | Grayscale -> Pp.string ctx "grayscale"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* Helpers for timing-function pretty printing *)

let pp_timing_float ctx f =
  Pp.string ctx (Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f)

let pp_cubic_bezier_args : (float * float * float * float) Pp.t =
 fun ctx (a, b, c, d) ->
  Pp.list ~sep:Pp.comma pp_timing_float ctx [ a; b; c; d ]

let pp_cubic_bezier = Pp.call "cubic-bezier" pp_cubic_bezier_args

let rec pp_timing_function : timing_function Pp.t =
 fun ctx -> function
  | Ease -> Pp.string ctx "ease"
  | Linear -> Pp.string ctx "linear"
  | Ease_in -> Pp.string ctx "ease-in"
  | Ease_out -> Pp.string ctx "ease-out"
  | Ease_in_out -> Pp.string ctx "ease-in-out"
  | Step_start -> Pp.string ctx "step-start"
  | Step_end -> Pp.string ctx "step-end"
  | Steps (1, Some (Jump_start | Start)) when Pp.minified ctx ->
      (* CSS Easing 1 §2: [steps(1, jump-start)] = [steps(1, start)] = the
         [step-start] alias; the [end] / [jump-end] equivalents fold to
         [step-end]. *)
      Pp.string ctx "step-start"
  | Steps (1, Some (Jump_end | End)) when Pp.minified ctx ->
      Pp.string ctx "step-end"
  | Steps (n, jump_term_opt) ->
      Pp.string ctx "steps(";
      Pp.int ctx n;
      (match jump_term_opt with
      | Some d ->
          Pp.char ctx ',';
          Pp.sp ctx ();
          pp_steps_direction ctx d
      | None -> ());
      Pp.char ctx ')'
  | Cubic_bezier (0.25, 0.1, 0.25, 1.0) when Pp.minified ctx ->
      (* CSS Easing 1 2: the named cubic-bezier aliases canonicalise to the
         keyword form, which is shorter than the four-argument call. *)
      Pp.string ctx "ease"
  | Cubic_bezier (0.42, 0.0, 1.0, 1.0) when Pp.minified ctx ->
      Pp.string ctx "ease-in"
  | Cubic_bezier (0.0, 0.0, 0.58, 1.0) when Pp.minified ctx ->
      Pp.string ctx "ease-out"
  | Cubic_bezier (0.42, 0.0, 0.58, 1.0) when Pp.minified ctx ->
      Pp.string ctx "ease-in-out"
  | Cubic_bezier (0.0, 0.0, 1.0, 1.0) when Pp.minified ctx ->
      (* CSS Easing 1 sec. 3: [cubic-bezier(0, 0, 1, 1)] is the identity curve,
         equivalent to the [linear] keyword. *)
      Pp.string ctx "linear"
  | Cubic_bezier (x1, y1, x2, y2) -> pp_cubic_bezier ctx (x1, y1, x2, y2)
  | Timing_functions timings ->
      Pp.list ~sep:Pp.comma pp_timing_function ctx timings
  | Linear_function body ->
      Pp.string ctx "linear(";
      Pp.string ctx body;
      Pp.char ctx ')'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_timing_function ctx v

let rec pp_svg_paint : svg_paint Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_svg_paint ctx v
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Current_color -> Pp.string ctx "currentcolor"
  | Color c -> pp_color ctx c
  | Context_fill -> Pp.string ctx "context-fill"
  | Context_stroke -> Pp.string ctx "context-stroke"
  | Url (u, fallback) -> (
      Pp.url ctx u;
      match fallback with
      | None -> ()
      | Some fb ->
          (* CSS Syntax 3 §5.4.6: a [url(...)] token closes with [)], so the
             whitespace before a fallback keyword/colour can be elided under
             minify. *)
          Pp.sp ctx ();
          pp_svg_paint ctx fb)

let rec pp_transition_property_value : transition_property_value Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Property s -> Pp.string ctx s
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_transition_property_value ctx v

let pp_transition_property : transition_property Pp.t =
 fun ctx -> Pp.list ~sep:Pp.comma pp_transition_property_value ctx

let rec pp_transition_behavior : transition_behavior Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_transition_behavior ctx v
  | Normal -> Pp.string ctx "normal"
  | Allow_discrete -> Pp.string ctx "allow-discrete"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let transition_timing_is_default ctx = function
  | Ease when Pp.minified ctx -> true
  | Cubic_bezier (0.25, 0.1, 0.25, 1.0) when Pp.minified ctx -> true
  | _ -> false

let rec pp_overlay : overlay Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overlay ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_transition_shorthand : transition_shorthand Pp.t =
 fun ctx { property; duration; timing_function; delay; behavior } ->
  pp_transition_property_value ctx property;
  (* Only output non-default values: defaults are 0s, ease, 0s *)
  (match duration with
  | Some (S 0.) | Some (Ms 0.) | None -> ()
  | Some d ->
      Pp.space ctx ();
      pp_duration ctx d);
  (match timing_function with
  | None -> ()
  | Some tf when transition_timing_is_default ctx tf -> ()
  | Some tf ->
      Pp.space ctx ();
      pp_timing_function ctx tf);
  (match delay with
  | Some (S 0.) | Some (Ms 0.) | None -> ()
  | Some d ->
      Pp.space ctx ();
      pp_duration ctx d);
  match behavior with
  | None | Some Normal -> ()
  | Some b ->
      Pp.space ctx ();
      pp_transition_behavior ctx b

let rec pp_transition : transition Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_transition ctx v
  | Shorthand s -> pp_transition_shorthand ctx s

(* CSS Syntax 3 §4: two adjacent [<number-percentage>] tokens need a separator
   unless the boundary is unambiguous - the previous ends with [%] (the unit
   terminates the token), or the next starts with [-]/[+] (a sign starts a new
   number). Render values to strings first since [pp_number_percentage] picks
   between [<number>] and [<percentage>] spelling and the spacing depends on the
   choice. *)
let render_number_percentages ctx vs =
  List.map (Pp.to_string ~minify:(Pp.minified ctx) pp_number_percentage) vs

let pp_number_percentage_separated ctx vs =
  let needs_sep prev next =
    (not (Pp.minified ctx))
    ||
    let last = prev.[String.length prev - 1] in
    (* Tokens that end with [%] / [)] (function-shaped values like [var(...)],
       [calc(...)]) close cleanly and need no separator; numbers and
       neg-prefixed digits stay separated. *)
    last <> '%' && last <> ')' && (String.length next = 0 || next.[0] <> '-')
  in
  let rec loop = function
    | [] -> ()
    | [ s ] -> Pp.string ctx s
    | s :: (next :: _ as rest) ->
        Pp.string ctx s;
        if needs_sep s next then Pp.space ctx ();
        loop rest
  in
  loop vs

let rec pp_scale : scale Pp.t =
 fun ctx -> function
  | X n -> pp_number_percentage ctx n
  | XY (x, y) ->
      pp_number_percentage_separated ctx
        (render_number_percentages ctx [ x; y ])
  | XYZ (x, y, z) ->
      pp_number_percentage_separated ctx
        (render_number_percentages ctx [ x; y; z ])
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_scale ctx v

let rec pp_translate_value : translate_value Pp.t =
 fun ctx -> function
  | X len -> pp_length ctx len
  | XY (Var x, Var y) ->
      (* In minified mode, Tailwind omits spaces between var() values *)
      pp_length ctx (Var x);
      Pp.space_if_pretty ctx ();
      pp_length ctx (Var y)
  | XY (x, y) ->
      pp_length ctx x;
      Pp.space ctx ();
      pp_length ctx y
  | XYZ (Var x, Var y, Var z) ->
      (* In minified mode, Tailwind omits spaces between var() values *)
      pp_length ctx (Var x);
      Pp.space_if_pretty ctx ();
      pp_length ctx (Var y);
      Pp.space_if_pretty ctx ();
      pp_length ctx (Var z)
  | XYZ (x, y, z) ->
      pp_length ctx x;
      Pp.space ctx ();
      pp_length ctx y;
      Pp.space ctx ();
      pp_length ctx z
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_translate_value ctx v

let rec read_translate_value t : translate_value =
  (* Per CSS Transforms 2 §3.5: [<length-percentage> <length-percentage>?
     <length>?]. Same two [var()] shapes as [read_scale]:

     - [translate: var(--t)] is a whole-value [var()] — produce [Var _]. -
     [translate: var(--x) var(--y)] is per-slot — produce [XY (_, _)]. *)
  let read_lengths_from t (x : length) : translate_value =
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some y -> (
        Cursor.ws t;
        match Cursor.option read_length t with
        | Some z -> XYZ (x, y, z)
        | None -> XY (x, y))
    | None -> X x
  in
  let read_lengths t : translate_value =
    let x = read_length t in
    read_lengths_from t x
  in
  let read_var_or_components t : translate_value =
    let snap = Cursor.save t in
    let whole_var : translate_value = Var (read_var read_translate_value t) in
    Cursor.ws t;
    if Cursor.is_done t then whole_var
    else
      let () = Cursor.restore t snap in
      let x = read_length t in
      read_lengths_from t x
  in
  Cursor.enum_or_calls "translate"
    [
      ("none", (None : translate_value));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var_or_components) ]
    ~default:read_lengths t

(* CSS Transforms 2 sec. 3.3: the standalone [rotate] / individual transform
   properties accept [<angle>] but reject the bare [0] from the CSS Values
   [<zero>] token shortcut (browsers don't apply the parse-time fallback). Emit
   the unit even on zero so [rotate: 0deg] survives minification. *)
let pp_required_unit_angle ctx = function
  | (Deg f | Rad f | Turn f | Grad f) as _a when f = 0. ->
      (* Round-trip stable: input [0deg] / [0rad] / etc. keeps its unit. *)
      let suffix =
        match _a with
        | Deg _ -> "deg"
        | Rad _ -> "rad"
        | Turn _ -> "turn"
        | Grad _ -> "grad"
        | _ -> "deg"
      in
      Pp.char ctx '0';
      Pp.string ctx suffix
  | a -> pp_angle ctx a

let rec pp_rotate_value : rotate_value Pp.t =
 fun ctx -> function
  | Angle a -> pp_required_unit_angle ctx a
  | X a ->
      Pp.string ctx "x";
      Pp.space ctx ();
      pp_required_unit_angle ctx a
  | Y a ->
      Pp.string ctx "y";
      Pp.space ctx ();
      pp_required_unit_angle ctx a
  | Z a ->
      Pp.string ctx "z";
      Pp.space ctx ();
      pp_required_unit_angle ctx a
  | Axis (1., 0., 0., a) when Pp.minified ctx -> pp_rotate_value ctx (X a)
  | Axis (0., 1., 0., a) when Pp.minified ctx -> pp_rotate_value ctx (Y a)
  | Axis (0., 0., 1., a) when Pp.minified ctx -> pp_rotate_value ctx (Z a)
  | Axis (x, y, z, a) ->
      (* CSS Transforms 2 sec. 3.3 [rotate] [<angle> <number>{3}] is shorter
         under minify when the angle leads (csso convention) and the second /
         third numbers drop the separator if they start with a sign. *)
      let pp_sep ctx (next : float) =
        if Pp.minified ctx && next < 0. then () else Pp.space ctx ()
      in
      pp_required_unit_angle ctx a;
      pp_sep ctx x;
      Pp.float ctx x;
      pp_sep ctx y;
      Pp.float ctx y;
      pp_sep ctx z;
      Pp.float ctx z
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_rotate_value ctx v

let read_rotate_axis prefix make t =
  Cursor.expect_string prefix t;
  Cursor.ws t;
  make (read_angle t)

let read_rotate_x t : rotate_value =
  read_rotate_axis "x" (fun a -> (X a : rotate_value)) t

let read_rotate_y t : rotate_value =
  read_rotate_axis "y" (fun a -> (Y a : rotate_value)) t

let read_rotate_z t : rotate_value =
  read_rotate_axis "z" (fun a -> (Z a : rotate_value)) t

(* Read custom axis: x y z angle. *)
let read_rotate_axis_angle t : rotate_value =
  let first = Cursor.number t in
  Cursor.ws t;
  let second = Cursor.number t in
  Cursor.ws t;
  let third = Cursor.number t in
  Cursor.ws t;
  let angle = read_angle t in
  Axis (first, second, third, angle)

let read_rotate_angle_axis_tail angle t =
  match Cursor.peek_ident t with
  | Some "x" ->
      Cursor.skip t;
      (X angle : rotate_value)
  | Some "y" ->
      Cursor.skip t;
      (Y angle : rotate_value)
  | Some "z" ->
      Cursor.skip t;
      (Z angle : rotate_value)
  | _ ->
      let first = Cursor.number t in
      Cursor.ws t;
      let second = Cursor.number t in
      Cursor.ws t;
      let third = Cursor.number t in
      (Axis (first, second, third, angle) : rotate_value)

(* CSS Transforms 2 §3.3 [rotate] also accepts angle then axis: [<angle> x|y|z]
   or [<angle> <number>{3}]. Try angle-first after the plain forms; consume the
   angle, then look for a trailing axis. *)
let read_rotate_angle_then_axis t : rotate_value =
  let angle = read_angle t in
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_semicolon t then Angle angle
  else read_rotate_angle_axis_tail angle t

let rec read_rotate_value t : rotate_value =
  Cursor.enum_or_calls "rotate"
    [
      ("none", (None : rotate_value));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> (Var (read_var read_rotate_value t) : rotate_value));
        ("calc", fun t -> Angle (Calc (read_calc read_angle t)));
      ]
    ~default:(fun t ->
      Cursor.one_of
        [
          read_rotate_x;
          read_rotate_y;
          read_rotate_z;
          read_rotate_axis_angle;
          read_rotate_angle_then_axis;
        ]
        t)
    t

let rec pp_outline_style : outline_style Pp.t =
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
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_outline_style ctx v

let pp_outline_shorthand : outline_shorthand Pp.t =
 fun ctx { width; style; color } ->
  let first = ref true in
  let add_space () = if !first then first := false else Pp.space ctx () in
  Option.iter
    (fun w ->
      add_space ();
      pp_length ctx w)
    width;
  Option.iter
    (fun s ->
      add_space ();
      pp_outline_style ctx s)
    style;
  Option.iter
    (fun c ->
      add_space ();
      pp_color ctx c)
    color

let rec pp_outline : outline Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_outline ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_outline_shorthand ctx shorthand

let rec pp_forced_color_adjust : forced_color_adjust Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_forced_color_adjust ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Preserve_parent_color -> Pp.string ctx "preserve-parent-color"
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

let rec pp_touch_action : touch_action Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_touch_action ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Pan_x -> Pp.string ctx "pan-x"
  | Pan_y -> Pp.string ctx "pan-y"
  | Pan_left -> Pp.string ctx "pan-left"
  | Pan_right -> Pp.string ctx "pan-right"
  | Pan_up -> Pp.string ctx "pan-up"
  | Pan_down -> Pp.string ctx "pan-down"
  | Pinch_zoom -> Pp.string ctx "pinch-zoom"
  | Manipulation -> Pp.string ctx "manipulation"
  | Actions actions -> Pp.list ~sep:Pp.space pp_touch_action ctx actions
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Vars vars -> Pp.list ~sep:Pp.space (pp_var pp_touch_action) ctx vars

let rec pp_unicode_bidi : unicode_bidi Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_unicode_bidi ctx v
  | Normal -> Pp.string ctx "normal"
  | Embed -> Pp.string ctx "embed"
  | Isolate -> Pp.string ctx "isolate"
  | Bidi_override -> Pp.string ctx "bidi-override"
  | Isolate_override -> Pp.string ctx "isolate-override"
  | Plaintext -> Pp.string ctx "plaintext"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_writing_mode : writing_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_writing_mode ctx v
  | Horizontal_tb -> Pp.string ctx "horizontal-tb"
  | Vertical_rl -> Pp.string ctx "vertical-rl"
  | Vertical_lr -> Pp.string ctx "vertical-lr"
  | Sideways_lr -> Pp.string ctx "sideways-lr"
  | Sideways_rl -> Pp.string ctx "sideways-rl"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_combine_upright : text_combine_upright Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_combine_upright ctx v
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Digits None -> Pp.string ctx "digits"
  | Digits (Some n) ->
      Pp.string ctx "digits";
      Pp.space ctx ();
      Pp.int ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_decoration_skip_ink : text_decoration_skip_ink Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_decoration_skip_ink ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_overscroll_behavior : overscroll_behavior Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overscroll_behavior ctx v
  | Auto -> Pp.string ctx "auto"
  | Contain -> Pp.string ctx "contain"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_appearance : webkit_appearance Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_appearance ctx v
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
  | Apple_pay_button -> Pp.string ctx "-apple-pay-button"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_pointer_events : pointer_events Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_pointer_events ctx v
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
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_user_select : user_select Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_user_select ctx v
  | None -> Pp.string ctx "none"
  | Auto -> Pp.string ctx "auto"
  | Text -> Pp.string ctx "text"
  | All -> Pp.string ctx "all"
  | Contain -> Pp.string ctx "contain"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_line_height : line_height Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct p -> Pp.pct ctx p
  | Num n -> Pp.float ctx n
  | Number { value; unit; repr } -> (
      match (ctx.minify, unit) with
      | false, None -> Pp.string ctx repr
      | false, Some unit ->
          Pp.string ctx repr;
          Pp.string ctx unit
      | true, None -> Pp.float ctx value
      | true, Some "%" -> Pp.pct ctx value
      | true, Some unit -> Pp.unit ctx value unit)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_line_height ctx v
  | Calc c -> pp_calc pp_line_height ctx c

let rec pp_font_weight : font_weight Pp.t =
 fun ctx -> function
  | Weight n -> Pp.int ctx n
  | Normal when Pp.minified ctx ->
      (* CSS Fonts 4 5.1.2: [normal] is spec-equivalent to [400]. *)
      Pp.string ctx "400"
  | Normal -> Pp.string ctx "normal"
  | Bold when Pp.minified ctx ->
      (* CSS Fonts 4 5.1.2: [bold] is spec-equivalent to [700]. *)
      Pp.string ctx "700"
  | Bold -> Pp.string ctx "bold"
  | Bolder -> Pp.string ctx "bolder"
  | Lighter -> Pp.string ctx "lighter"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font_weight ctx v

(* CSS Fonts 4 sec. 2.7: under minify, drop [<style>? <weight>? <stretch>?]
   components that equal their longhand initial value, and drop [line-height]
   when it's [normal]. The shorthand body itself is always [<size>
   [/<line-height>]? <family>+]; size and family are required. *)
let pp_font_variant_css21 ctx = function
  | (Normal : font_variant_css21) -> Pp.string ctx "normal"
  | Small_caps -> Pp.string ctx "small-caps"

let read_font_variant_css21 t : font_variant_css21 =
  Cursor.enum "font-variant-css21"
    [ ("normal", (Normal : font_variant_css21)); ("small-caps", Small_caps) ]
    t

let drop_font_default ctx (type a) ~(is_default : a -> bool) (opt : a option) :
    a option =
  if Pp.minified ctx then
    match opt with Some v when is_default v -> None | _ -> opt
  else opt

let drop_font_shorthand_defaults ctx style variant weight stretch line_height =
  let style =
    drop_font_default ctx style ~is_default:(function
      | (Normal : font_style) -> true
      | _ -> false)
  in
  let variant =
    drop_font_default ctx variant ~is_default:(function
      | (Normal : font_variant_css21) -> true
      | _ -> false)
  in
  let weight =
    drop_font_default ctx weight ~is_default:(function
      | (Normal : font_weight) | Weight 400 -> true
      | _ -> false)
  in
  let stretch =
    drop_font_default ctx stretch ~is_default:(function
      | (Normal : font_stretch) -> true
      | _ -> false)
  in
  let line_height =
    drop_font_default ctx line_height ~is_default:(function
      | (Normal : line_height) -> true
      | _ -> false)
  in
  (style, variant, weight, stretch, line_height)

let pp_font_prefix ctx style variant weight stretch =
  let first = ref true in
  let emit pp opt =
    Option.iter
      (fun v ->
        if not !first then Pp.space ctx ();
        first := false;
        pp ctx v)
      opt
  in
  emit pp_font_style style;
  emit pp_font_variant_css21 variant;
  emit pp_font_weight weight;
  emit pp_font_stretch stretch;
  !first

let pp_font_shorthand : font_shorthand Pp.t =
 fun ctx { style; variant; weight; stretch; size; line_height; family } ->
  let style, variant, weight, stretch, line_height =
    drop_font_shorthand_defaults ctx style variant weight stretch line_height
  in
  if not (pp_font_prefix ctx style variant weight stretch) then Pp.space ctx ();
  pp_font_size ctx size;
  Option.iter
    (fun lh ->
      Pp.char ctx '/';
      pp_line_height ctx lh)
    line_height;
  Pp.space ctx ();
  pp_font_family ctx family

let rec pp_font : font Pp.t =
 fun ctx -> function
  | Shorthand sh -> pp_font_shorthand ctx sh
  | Caption -> Pp.string ctx "caption"
  | Icon -> Pp.string ctx "icon"
  | Menu -> Pp.string ctx "menu"
  | Message_box -> Pp.string ctx "message-box"
  | Small_caption -> Pp.string ctx "small-caption"
  | Status_bar -> Pp.string ctx "status-bar"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_font ctx v

let rec pp_webkit_box_orient : webkit_box_orient Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_box_orient ctx v
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Inline_axis -> Pp.string ctx "inline-axis"
  | Block_axis -> Pp.string ctx "block-axis"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_moz_orient : moz_orient Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_moz_orient ctx v
  | Inline -> Pp.string ctx "inline"
  | Block -> Pp.string ctx "block"
  | Horizontal -> Pp.string ctx "horizontal"
  | Vertical -> Pp.string ctx "vertical"
  | Inherit -> Pp.string ctx "inherit"

let rec pp_webkit_line_clamp : webkit_line_clamp Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Lines n -> Pp.int ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_webkit_line_clamp ctx v

let rec read_border_style t : border_style =
  Cursor.enum_or_var "border-style"
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
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_border_style t))
    t

(* Helper: ensure border-width values are non-negative per CSS spec *)
let ensure_non_negative_border_width t value =
  if value < 0.0 then
    err_invalid_value t "border-width" "negative values not allowed"
  else value

(* Helper: convert length to border_width, ensuring non-negative values *)
let length_to_border_width t (length : length) : border_width =
  let non_neg = ensure_non_negative_border_width t in
  let typed_dimension value unit : border_width =
    let value = non_neg value in
    match String.lowercase_ascii unit with
    | "%" -> Pct value
    | "px" -> Px value
    | "cm" -> Cm value
    | "mm" -> Mm value
    | "q" -> Q value
    | "in" -> In value
    | "pt" -> Pt value
    | "pc" -> Pc value
    | "rem" -> Rem value
    | "em" -> Em value
    | "ex" -> Ex value
    | "cap" -> Cap value
    | "ic" -> Ic value
    | "ric" -> Ric value
    | "rlh" -> Rlh value
    | "ch" -> Ch value
    | "lh" -> Lh value
    | "vh" -> Vh value
    | "vw" -> Vw value
    | "vmin" -> Vmin value
    | "vmax" -> Vmax value
    | _ -> err_invalid_value t "border-width" "unsupported length type"
  in
  match length with
  | Zero -> Zero
  | Px n -> Px (non_neg n)
  | Cm n -> Cm (non_neg n)
  | Mm n -> Mm (non_neg n)
  | Q n -> Q (non_neg n)
  | In n -> In (non_neg n)
  | Pt n -> Pt (non_neg n)
  | Pc n -> Pc (non_neg n)
  | Rem n -> Rem (non_neg n)
  | Em n -> Em (non_neg n)
  | Ex n -> Ex (non_neg n)
  | Cap n -> Cap (non_neg n)
  | Ic n -> Ic (non_neg n)
  | Ric n -> Ric (non_neg n)
  | Rlh n -> Rlh (non_neg n)
  | Ch n -> Ch (non_neg n)
  | Lh n -> Lh (non_neg n)
  | Vh n -> Vh (non_neg n)
  | Vw n -> Vw (non_neg n)
  | Vmin n -> Vmin (non_neg n)
  | Vmax n -> Vmax (non_neg n)
  | Pct n -> Pct (non_neg n)
  | Dimension { value; unit; _ } -> typed_dimension value unit
  | _ -> err_invalid_value t "border-width" "unsupported length type"

let rec read_border_width t : border_width =
  let read_var t : border_width = Var (read_var read_border_width t) in
  let read_calc t : border_width = Calc (read_calc read_border_width t) in
  let read_math_arg t = read_calc_expr read_border_width t in
  let read_min t : border_width =
    Min
      (Cursor.call "min" t
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg))
  in
  let read_max t : border_width =
    Max
      (Cursor.call "max" t
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg))
  in
  let read_clamp t : border_width =
    match
      Cursor.call "clamp" t
        (Cursor.list ~sep:Cursor.comma ~at_least:3 ~at_most:3 read_math_arg)
    with
    | [ lower; value; upper ] -> Clamp (lower, value, upper)
    | _ -> Cursor.err_invalid t "invalid clamp"
  in
  let read_length_as_border_width t =
    let length = read_length ~with_keywords:false t in
    length_to_border_width t length
  in

  Cursor.enum_or_calls "border-width"
    [
      ("thin", (Thin : border_width));
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
        ("var", read_var);
        ("calc", read_calc);
        ("min", read_min);
        ("max", read_max);
        ("clamp", read_clamp);
      ]
    ~default:read_length_as_border_width t

module Border = struct
  type component =
    | Width of border_width
    | Style of border_style
    | Color of color

  type components = {
    width : border_width option;
    style : border_style option;
    color : color option;
  }

  let empty = { width = None; style = None; color = None }

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Color (read_color t));
        (fun t -> Width (read_border_width t));
        (fun t -> Style (read_border_style t));
      ]
      t

  let merge t acc = function
    | Width w when acc.width = None -> { acc with width = Some w }
    | Style s when acc.style = None -> { acc with style = Some s }
    | Color c when acc.color = None -> { acc with color = Some c }
    | Width _ -> Cursor.err_invalid t "duplicate border width"
    | Style _ -> Cursor.err_invalid t "duplicate border style"
    | Color _ -> Cursor.err_invalid t "duplicate border color"

  let to_shorthand (components : components) : border_shorthand =
    {
      width = components.width;
      style = components.style;
      color = components.color;
    }
end

let read_border_shorthand t : border_shorthand =
  (* A [var()] in the border shorthand is type-ambiguous (it could substitute a
     width, style, or colour), so it cannot be assigned by matching a typed
     reader - assign it to the next unfilled slot in width/style/colour order
     and read its fallback with that slot's reader. Concrete components still
     bind by type. *)
  let acc = ref Border.empty in
  let consumed = ref true in
  while !consumed do
    Cursor.ws t;
    consumed :=
      if Cursor.is_done t || Cursor.peek_comma t then false
      else if Cursor.looking_at_func "var" t then (
        let a = !acc in
        acc :=
          if a.width = Option.None then
            { a with width = Some (Var (read_var read_border_width t)) }
          else if a.style = Option.None then
            { a with style = Some (Var (read_var read_border_style t)) }
          else if a.color = Option.None then
            { a with color = Some (Var (read_var read_color t)) }
          else Cursor.err_invalid t "too many border components";
        true)
      else
        match Cursor.option Border.read_component t with
        | Some c ->
            acc := Border.merge t !acc c;
            true
        | Option.None -> false
  done;
  Border.to_shorthand !acc

let border_keyword = function
  | "inherit" -> Some (Inherit : border)
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | "none" -> Some None
  | _ -> None

let read_border_shorthand_from t snap : border =
  Cursor.restore t snap;
  Shorthand (read_border_shorthand t)

let read_border_keyword_or_shorthand t : border =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match border_keyword (String.lowercase_ascii ident) with
      | Some value ->
          Cursor.ws t;
          if Cursor.is_done t || Cursor.peek_comma t then value
          else read_border_shorthand_from t snap
      | None -> read_border_shorthand_from t snap)
  | None -> read_border_shorthand_from t snap

let rec read_border (t : Cursor.t) : border =
  if Cursor.looking_at_func "var" t then (
    (* A lone [var()] is the whole value; a [var()] followed by more components
       is a shorthand whose first component happens to be a var. *)
    let snap = Cursor.save t in
    let v = Values.read_var read_border t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_comma t then (Var v : border)
    else (
      Cursor.restore t snap;
      read_border_keyword_or_shorthand t))
  else read_border_keyword_or_shorthand t

let rec read_logical_border_color t : logical_border_color =
  let read_colors t =
    match Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_color t with
    | [ color ] -> (Single color : logical_border_color)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two colors"
  in
  Cursor.enum_or_var "logical border color"
    [
      ("inherit", (Inherit : logical_border_color));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_color t))
    ~default:read_colors t

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

let rec read_baseline_source t : baseline_source =
  Cursor.enum_or_var "baseline-source"
    [
      ("auto", (Auto : baseline_source));
      ("first", First);
      ("last", Last);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_baseline_source t))
    t

let rec read_alignment_baseline t : alignment_baseline =
  Cursor.enum_or_var "alignment-baseline"
    [
      ("baseline", (Baseline : alignment_baseline));
      ("text-bottom", Text_bottom);
      ("middle", Middle);
      ("central", Central);
      ("text-top", Text_top);
      ("ideographic", Ideographic);
      ("alphabetic", Alphabetic);
      ("hanging", Hanging);
      ("mathematical", Mathematical);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_alignment_baseline t))
    t

let rec read_baseline_shift t : baseline_shift =
  Cursor.enum_or_var "baseline-shift"
    [
      ("sub", (Sub : baseline_shift));
      ("super", Super);
      ("top", Top);
      ("center", Center);
      ("bottom", Bottom);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_baseline_shift t))
    ~default:(fun t ->
      Shift (Values.read_length_percentage ~with_keywords:false t))
    t

let rec read_z_index t : z_index =
  let read_calc_z t =
    (* read_calc handles the calc(...) wrapper itself *)
    let expr =
      read_calc (fun _ -> Cursor.err t "unexpected value in z-index calc") t
    in
    match eval_numeric_calc expr with
    | Some f when Float.is_integer f -> Index (int_of_float f)
    | Some _ -> Cursor.err_invalid t "z-index calc must evaluate to integer"
    | None -> Calc expr
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
    ~default:(fun t ->
      let n = Cursor.number t in
      if Float.is_integer n then Index (int_of_float n)
      else Cursor.err_invalid t "z-index must be integer")
    t

let rec read_order t : order =
  let read_calc_order t =
    (* read_calc handles the calc(...) wrapper itself *)
    let expr =
      read_calc (fun _ -> Cursor.err t "unexpected value in order calc") t
    in
    match eval_numeric_calc expr with
    | Some f when Float.is_integer f -> (Int (int_of_float f) : order)
    | Some _ -> Cursor.err_invalid t "order calc must evaluate to integer"
    | None -> (Calc expr : order)
  in
  let read_var t : order = Var (read_var read_order t) in
  Cursor.enum_or_calls "order"
    [
      ("inherit", (Inherit : order));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("calc", read_calc_order); ("var", read_var) ]
    ~default:(fun t ->
      let n = Cursor.number t in
      if Float.is_integer n then (Int (int_of_float n) : order)
      else Cursor.err_invalid t "order must be integer")
    t

let rec read_flex_wrap t : flex_wrap =
  Cursor.enum_or_var "flex-wrap"
    [
      ("nowrap", (Nowrap : flex_wrap));
      ("wrap", Wrap);
      ("wrap-reverse", Wrap_reverse);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_flex_wrap t))
    t

let read_flex_flow_direction t : flex_direction =
  Cursor.enum "flex-flow direction"
    [
      ("row", (Row : flex_direction));
      ("row-reverse", Row_reverse);
      ("column", Column);
      ("column-reverse", Column_reverse);
    ]
    t

let read_flex_flow_wrap t : flex_wrap =
  Cursor.enum "flex-flow wrap"
    [
      ("nowrap", (Nowrap : flex_wrap));
      ("wrap", Wrap);
      ("wrap-reverse", Wrap_reverse);
    ]
    t

let read_flex_flow_part (direction : flex_direction option ref)
    (wrap : flex_wrap option ref) t =
  match (!direction, Cursor.option read_flex_flow_direction t) with
  | None, Some value ->
      direction := Some value;
      true
  | Some _, Some _ -> Cursor.err_invalid t "duplicate flex direction"
  | _, None -> (
      match (!wrap, Cursor.option read_flex_flow_wrap t) with
      | None, Some value ->
          wrap := Some value;
          true
      | Some _, Some _ -> Cursor.err_invalid t "duplicate flex wrap"
      | _, None -> Cursor.err_expected t "flex-flow")

let rec read_flex_flow t : flex_flow =
  Cursor.enum_or_var "flex-flow"
    [
      ("inherit", (Inherit : flex_flow));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_flex_flow t))
    ~default:(fun t ->
      let direction : flex_direction option ref =
        ref (None : flex_direction option)
      in
      let wrap : flex_wrap option ref = ref (None : flex_wrap option) in
      let seen = ref false in
      (* CSS Flexbox 1 sec. 6.3: [flex-flow] is at most two values
         ([flex-direction] || [flex-wrap]). Stop once both slots are filled, and
         break on the first iteration where neither matches instead of letting
         [read_flex_flow_part] raise on the trailing [;] / [!important]. *)
      let rec loop () =
        Cursor.ws t;
        if Cursor.is_done t then ()
        else if Option.is_some !direction && Option.is_some !wrap then ()
        else
          let before = Cursor.save t in
          match read_flex_flow_part direction wrap t with
          | true ->
              seen := true;
              loop ()
          | false -> Cursor.restore t before
          | exception Cursor.Parse_error _ -> Cursor.restore t before
      in
      loop ();
      if not !seen then Cursor.err_expected t "flex-flow";
      Flow (!direction, !wrap))
    t

let read_non_negative_flex_number t =
  let value =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Number_tok _; _ }) -> (
        let n, unit = Cursor.number_with_unit t in
        match unit with
        | None -> n
        | Some u -> Cursor.err_invalid t ("flex factor unit: " ^ u))
    | _ -> (
        match (Values.read_number t : Values.number) with
        | Values.Num value -> value
        | _ -> Cursor.err_invalid t "flex number must resolve to a number")
  in
  if value < 0. then Cursor.err_invalid t "negative number not allowed";
  value

let rec read_flex_factor t : flex_factor =
  let read_number t =
    (Number (read_non_negative_flex_number t) : flex_factor)
  in
  Cursor.enum_or_calls "flex factor"
    [
      ("inherit", (Inherit : flex_factor));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (Values.read_var read_flex_factor t));
        ("calc", fun t -> Calc (read_calc read_flex_factor t));
      ]
    ~default:read_number t

let flex_basis_of_length t (length : length) : flex_basis =
  match length with
  | Px n -> Px n
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
  | Ric n -> Ric n
  | Rlh n -> Rlh n
  | Vw n -> Vw n
  | Vh n -> Vh n
  | Vmin n -> Vmin n
  | Vmax n -> Vmax n
  | Vi n -> Vi n
  | Vb n -> Vb n
  | Dvh n -> Dvh n
  | Dvw n -> Dvw n
  | Dvmin n -> Dvmin n
  | Dvmax n -> Dvmax n
  | Lvh n -> Lvh n
  | Lvw n -> Lvw n
  | Lvmin n -> Lvmin n
  | Lvmax n -> Lvmax n
  | Svh n -> Svh n
  | Svw n -> Svw n
  | Svmin n -> Svmin n
  | Svmax n -> Svmax n
  | Ch n -> Ch n
  | Lh n -> Lh n
  | Zero -> Zero
  | Fit_content -> Fit_content
  | Fit_content_arg arg -> (Fit_content_arg arg : flex_basis)
  | Max_content -> Max_content
  | Min_content -> Min_content
  | Inherit -> Inherit
  | Initial -> Initial
  | Unset -> Unset
  | Revert -> Revert
  | Revert_layer -> Revert_layer
  (* [read_length_unit] wraps a known-unit zero in [Dimension { unit; repr }] to
     preserve the authored spelling. Map back to the typed [flex_basis] form so
     [flex-basis: 0px] / [flex-basis: 0%] type-check. CSS Flexbox 1 sec. 7.2:
     [flex-basis] accepts [<'width'>] which accepts [<length-percentage>]. *)
  | Dimension { value = 0.; unit = "%"; _ } -> Pct 0.
  | Dimension { value = 0.; unit = "px"; _ } -> Px 0.
  | Dimension { value = 0.; unit = "cm"; _ } -> Cm 0.
  | Dimension { value = 0.; unit = "mm"; _ } -> Mm 0.
  | Dimension { value = 0.; unit = "q"; _ } -> Q 0.
  | Dimension { value = 0.; unit = "in"; _ } -> In 0.
  | Dimension { value = 0.; unit = "pt"; _ } -> Pt 0.
  | Dimension { value = 0.; unit = "pc"; _ } -> Pc 0.
  | Dimension { value = 0.; unit = "rem"; _ } -> Rem 0.
  | Dimension { value = 0.; unit = "em"; _ } -> Em 0.
  | Dimension { value = 0.; unit = "ex"; _ } -> Ex 0.
  | Dimension { value = 0.; unit = "cap"; _ } -> Cap 0.
  | Dimension { value = 0.; unit = "ic"; _ } -> Ic 0.
  | Dimension { value = 0.; unit = "ric"; _ } -> Ric 0.
  | Dimension { value = 0.; unit = "rlh"; _ } -> Rlh 0.
  | Dimension { value = 0.; unit = "vw"; _ } -> Vw 0.
  | Dimension { value = 0.; unit = "vh"; _ } -> Vh 0.
  | Dimension { value = 0.; unit = "vmin"; _ } -> Vmin 0.
  | Dimension { value = 0.; unit = "vmax"; _ } -> Vmax 0.
  | Dimension { value = 0.; unit = "vi"; _ } -> Vi 0.
  | Dimension { value = 0.; unit = "vb"; _ } -> Vb 0.
  | Dimension { value = 0.; unit = "dvh"; _ } -> Dvh 0.
  | Dimension { value = 0.; unit = "dvw"; _ } -> Dvw 0.
  | Dimension { value = 0.; unit = "dvmin"; _ } -> Dvmin 0.
  | Dimension { value = 0.; unit = "dvmax"; _ } -> Dvmax 0.
  | Dimension { value = 0.; unit = "lvh"; _ } -> Lvh 0.
  | Dimension { value = 0.; unit = "lvw"; _ } -> Lvw 0.
  | Dimension { value = 0.; unit = "lvmin"; _ } -> Lvmin 0.
  | Dimension { value = 0.; unit = "lvmax"; _ } -> Lvmax 0.
  | Dimension { value = 0.; unit = "svh"; _ } -> Svh 0.
  | Dimension { value = 0.; unit = "svw"; _ } -> Svw 0.
  | Dimension { value = 0.; unit = "svmin"; _ } -> Svmin 0.
  | Dimension { value = 0.; unit = "svmax"; _ } -> Svmax 0.
  | Dimension { value = 0.; unit = "ch"; _ } -> Ch 0.
  | Dimension { value = 0.; unit = "lh"; _ } -> Lh 0.
  (* Math functions over <length-percentage> carry across unchanged:
     [flex_basis] mirrors [length]'s constructors. [Sign] (a <number>) and
     [Minmax] (grid only) are not valid here, so they stay rejected. *)
  | Clamp (a, b, c) -> Clamp (a, b, c)
  | Min xs -> Min xs
  | Max xs -> Max xs
  | Round (s, a, b) -> Round (s, a, b)
  | Mod (a, b) -> Mod (a, b)
  | Rem_fn (a, b) -> Rem_fn (a, b)
  | Hypot xs -> Hypot xs
  | Abs a -> Abs a
  | _ -> Cursor.err_invalid t "unsupported flex-basis value"

let rec read_flex_basis t : flex_basis =
  Cursor.enum_or_calls "flex-basis"
    [
      ("auto", (Auto : flex_basis));
      ("content", Content);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (read_var read_flex_basis t));
        ("calc", fun t -> Calc (read_calc read_flex_basis t));
      ]
    ~default:(fun t ->
      let pos = Cursor.save t in
      match Cursor.option Cursor.number_with_unit t with
      | Some (0.0, None) -> (Zero : flex_basis)
      | Some _ ->
          Cursor.restore t pos;
          read_length ~allow_negative:false t |> flex_basis_of_length t
      | None -> read_length ~allow_negative:false t |> flex_basis_of_length t)
    t

module Flex = struct
  (* Helper functions for flex parsing *)
  let read_basis_only t = Basis (read_flex_basis t)

  let read_factor t : flex_factor =
    (* A flex factor is a [<number>]: a [var()], a [calc()] (held unfolded; the
       optimize+minify pass folds a constant calc to a literal), or a literal
       number. *)
    if Cursor.looking_at_func "var" t then Var (read_var read_flex_factor t)
    else if Cursor.looking_at_func "calc" t then
      Calc (read_calc read_flex_factor t)
    else Number (read_non_negative_flex_number t)

  let read_grow_shrink_basis t =
    (* Parse grow [shrink] [basis]; a [50%] / [10px] is a basis, not a
       factor. *)
    let grow = read_factor t in
    let shrink =
      Cursor.option
        (fun t ->
          Cursor.ws t;
          read_factor t)
        t
    in
    (* Optional basis (defaults to 0%) *)
    let basis =
      Cursor.option
        (fun t ->
          Cursor.ws t;
          read_flex_basis t)
        t
    in
    match (shrink, basis) with
    | None, None -> Grow grow
    | Some s, None -> Grow_shrink (grow, s)
    | _, Some b -> Full (grow, Option.value shrink ~default:(Number 1.0), b)
end

let rec read_flex t : flex =
  Cursor.enum_or_var "flex"
    [
      ("initial", (Initial : flex));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("auto", Auto);
      ("none", (None : flex));
      ("content", Basis Content);
    ]
    ~var:(fun t ->
      (* A lone var() is the whole value; a var() followed by more components is
         a grow/shrink/basis sequence whose first factor happens to be a var. *)
      let snap = Cursor.save t in
      let v = read_var read_flex t in
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_comma t then (Var v : flex)
      else (
        Cursor.restore t snap;
        Cursor.one_of [ Flex.read_grow_shrink_basis; Flex.read_basis_only ] t))
    ~default:
      (Cursor.one_of [ Flex.read_grow_shrink_basis; Flex.read_basis_only ])
    t

let read_place_align_content t =
  match read_align_content t with
  | Left | Right | Safe_left | Safe_right | Unsafe_left | Unsafe_right ->
      Cursor.err_invalid t "place-content align value cannot be left or right"
  | value -> value

let read_place_content_pair t =
  let a, j = Cursor.pair read_place_align_content read_justify_content t in
  (Align_justify (a, j) : place_content)

let read_place_content_safe t =
  Cursor.expect_string "safe" t;
  Cursor.ws t;
  Cursor.enum "place-content safe"
    [
      ("center", (Safe_center : place_content));
      ("start", Safe_start);
      ("end", Safe_end);
      ("stretch", Safe_stretch);
    ]
    t

let read_place_content_unsafe t =
  Cursor.expect_string "unsafe" t;
  Cursor.ws t;
  Cursor.enum "place-content unsafe"
    [
      ("center", (Unsafe_center : place_content));
      ("start", Unsafe_start);
      ("end", Unsafe_end);
      ("stretch", Unsafe_stretch);
    ]
    t

let read_place_content_single t =
  Cursor.enum "place-content"
    [
      ("normal", (Normal : place_content));
      ("start", Start);
      ("end", End);
      ("center", Center);
      ("stretch", Stretch);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("space-evenly", Space_evenly);
      ("inherit", Inherit);
    ]
    t

let rec read_place_content t : place_content =
  Cursor.enum_or_var "place-content"
    [
      ("inherit", (Inherit : place_content));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_place_content t))
    ~default:
      (Cursor.one_of
         [
           read_place_content_pair;
           read_place_content_safe;
           read_place_content_unsafe;
           read_place_content_single;
         ])
    t

let read_place_items_safe t =
  Cursor.expect_string "safe" t;
  Cursor.ws t;
  match Cursor.ident t with
  | "start" -> Start_safe
  | "end" -> End_safe
  | "center" -> Center_safe
  | kw -> Cursor.err_invalid t ("place-items safe " ^ kw)

let read_place_items_stretch t =
  Cursor.expect_string "stretch" t;
  Cursor.ws t;
  if
    Cursor.option (fun t -> Cursor.expect_string "stretch" t) t
    |> Option.is_some
  then Stretch_stretch
  else Stretch

let place_items_align : place_items -> align_items option = function
  | Normal -> Some (Normal : align_items)
  | Start -> Some Start
  | End -> Some End
  | Center -> Some Center
  | Baseline -> Some Baseline
  | Stretch -> Some Stretch
  | _ -> None

let read_place_items_first t =
  Cursor.enum "place-items"
    [
      ("normal", (Normal : place_items));
      ("start", Start);
      ("end", End);
      ("center", Center);
      ("baseline", Baseline);
      ("inherit", Inherit);
    ]
    t

let read_place_items_default t =
  if Cursor.looking_at t "safe" then read_place_items_safe t
  else if Cursor.looking_at t "stretch" then read_place_items_stretch t
  else
    let first = read_place_items_first t in
    Cursor.ws t;
    match Cursor.option read_justify_items t with
    | None -> first
    | Some justify -> (
        match place_items_align first with
        | Some align -> Align_justify (align, justify)
        | None -> Cursor.err_invalid t "place-items two-value")

let rec read_place_items t : place_items =
  Cursor.ws t;
  Cursor.enum_or_var "place-items"
    [
      ("inherit", (Inherit : place_items));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_place_items t))
    ~default:read_place_items_default t

let rec read_grid_auto_flow t : grid_auto_flow =
  Cursor.enum_or_var "grid-auto-flow"
    [
      ("inherit", (Inherit : grid_auto_flow));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_grid_auto_flow t))
    ~default:(fun t ->
      let v = Cursor.ident t in
      Cursor.ws t;
      let second = Cursor.option Cursor.ident t in
      match (v, second) with
      | "row", Some "dense" -> Row_dense
      | "row", None -> Row
      | "column", Some "dense" -> Column_dense
      | "column", None -> Column
      | "dense", Some "row" -> Row_dense
      | "dense", Some "column" -> Column_dense
      | "dense", None -> Dense
      | _, Some _ ->
          err_invalid_value t "grid-auto-flow"
            (v ^ " " ^ Option.value second ~default:"")
      | _ -> err_invalid_value t "grid-auto-flow" v)
    t

(* CSS Grid template - flattened type with direct constructors *)

let grid_line_at_end t =
  Cursor.is_done t || Cursor.peek_semicolon t
  ||
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Delim "/"; _ }) -> true
  | _ -> false

let read_grid_line_name t =
  let name = Cursor.ident t in
  if name = "span" then Cursor.err_invalid t "duplicate span grid line"
  else name

let read_grid_span t =
  let span_word = Cursor.ident t in
  if span_word <> "span" then
    Cursor.err t ("Expected 'span' but got " ^ span_word);
  Cursor.ws t;
  let first_int : int option = Cursor.option Cursor.int t in
  Cursor.ws t;
  let first_name : string option =
    if Option.is_none first_int && not (grid_line_at_end t) then
      Cursor.option read_grid_line_name t
    else None
  in
  Cursor.ws t;
  let second : [ `Name of string | `Num of int ] option =
    if grid_line_at_end t then None
    else
      match first_int with
      | Some _ ->
          Option.map
            (fun name -> `Name name)
            (Cursor.option read_grid_line_name t)
      | None -> Option.map (fun n -> `Num n) (Cursor.option Cursor.int t)
  in
  match (first_int, first_name, second) with
  | Some n, None, Some (`Name name) -> Span_num_name (n, name)
  | Some n, None, None -> Span n
  | None, Some name, Some (`Num n) -> Span_num_name (n, name)
  | None, Some name, None -> Span_name name
  | _ -> Cursor.err_invalid t "invalid span grid line"

let read_grid_line_number t : grid_line =
  let n = Cursor.int t in
  Cursor.ws t;
  let name : string option =
    if grid_line_at_end t then None else Cursor.option read_grid_line_name t
  in
  match name with Some name -> Num_name (n, name) | None -> Num n

let read_grid_line_name_value t : grid_line =
  let name = read_grid_line_name t in
  Cursor.ws t;
  let n : int option =
    if grid_line_at_end t then None else Cursor.option Cursor.int t
  in
  match n with Some n -> Num_name (n, name) | None -> Name name

let read_grid_line_calc t : grid_line =
  (* read_calc handles the calc(...) wrapper itself *)
  let expr =
    read_calc (fun _ -> Cursor.err t "unexpected value in grid-line calc") t
  in
  match eval_numeric_calc expr with
  | Some f when Float.is_integer f -> Num (int_of_float f)
  | Some _ -> Cursor.err_invalid t "grid-line calc must evaluate to integer"
  | None -> Calc expr

let rec read_grid_line t : grid_line =
  Cursor.enum_or_calls "grid-line"
    [ ("auto", (Auto : grid_line)) ]
    ~calls:
      [
        ("calc", read_grid_line_calc);
        ("var", fun t -> (Var (Values.read_var read_grid_line t) : grid_line));
      ]
    ~default:(fun t ->
      Cursor.one_of
        [ read_grid_line_number; read_grid_span; read_grid_line_name_value ]
        t)
    t

let read_grid_line_pair t : grid_line_pair =
  let read_pair t =
    let start = read_grid_line t in
    if Cursor.slash_opt t then
      let end_ = read_grid_line t in
      (Lines (start, end_) : grid_line_pair)
    else (Lines (start, Auto) : grid_line_pair)
  in
  if Cursor.looking_at_func "var" t then
    (Var (Values.read_var read_pair t) : grid_line_pair)
  else read_pair t

(* CSS Grid 2 §8.4: an omitted slot inherits from the corresponding row/column
   start when that's a [<custom-ident>], else defaults to [auto]. The forward
   helper is [grid_area_default_from] (defined above with the printer); both
   directions share it. *)
let read_grid_area t : grid_area =
  let first = read_grid_line t in
  let rest =
    Cursor.many
      (fun t ->
        Cursor.slash t;
        read_grid_line t)
      t
    |> fst
  in
  let row_start, column_start, row_end, column_end =
    match rest with
    | [] ->
        let other = grid_area_default_from first in
        (first, other, other, other)
    | [ v2 ] ->
        let row_end = grid_area_default_from first in
        let column_end = grid_area_default_from v2 in
        (first, v2, row_end, column_end)
    | [ v2; v3 ] ->
        let column_end = grid_area_default_from v2 in
        (first, v2, v3, column_end)
    | [ v2; v3; v4 ] -> (first, v2, v3, v4)
    | _ -> err_invalid_value t "grid-area" "too many grid lines"
  in
  Lines { row_start; column_start; row_end; column_end }

module Grid_template = struct
  let read_length_as_grid t : grid_template =
    (* [~with_keywords:false]: track keywords (auto / min-content / ...) are a
       separate [one_of] alternative, so this reader handles only real lengths -
       the unit-specific cases below, plus a general [Length] carrier for a
       [calc()], a [var()] in a [calc()], or a less common unit. *)
    match read_length ~with_keywords:false t with
    | Px n -> (Px n : grid_template)
    | Rem n -> Rem n
    | Em n -> Em n
    | Vw n -> Vw n
    | Vh n -> Vh n
    | Vmin n -> Vmin n
    | Vmax n -> Vmax n
    | Pct n -> Pct n
    | Zero -> Zero
    | other -> Length other

  let read_fr t : grid_template =
    (* [1fr] lexes as a single [Dimension] with unit "fr". *)
    match Cursor.dimension_opt t with
    | Some (n, "fr") -> Fr n
    | _ -> Cursor.err_expected t "<fr>"

  let read_track_breadth t : grid_template =
    (* Accept a single breadth: length, fr, or keywords *)
    Cursor.one_of
      [
        read_fr;
        read_length_as_grid;
        (fun t ->
          Cursor.enum "grid-breadth"
            [
              ("auto", (Auto : grid_template));
              ("min-content", (Min_content : grid_template));
              ("max-content", (Max_content : grid_template));
              ("inherit", (Inherit : grid_template));
            ]
            t);
      ]
      t

  let read_minmax t : grid_template =
    Cursor.call "minmax" t @@ fun inner ->
    Cursor.ws inner;
    let minv = read_track_breadth inner in
    Cursor.ws inner;
    Cursor.comma inner;
    Cursor.ws inner;
    let maxv = read_track_breadth inner in
    Min_max (minv, maxv)

  let read_fit_content t : grid_template =
    Cursor.call "fit-content" t @@ fun inner ->
    Cursor.ws inner;
    Fit_content (read_length inner)

  let rec read_repeat_count t : repeat_count =
    if Cursor.looking_at t "var(" then Var (Values.read_var read_repeat_count t)
    else
      match Cursor.option Cursor.int t with
      | Some n -> Count n
      | None -> (
          match Cursor.ident t with
          | "auto-fill" -> Auto_fill
          | "auto-fit" -> Auto_fit
          | ident -> Cursor.err_invalid t ("repeat count: " ^ ident))

  let read_line_names t : grid_template =
    Cursor.brackets
      (fun inner ->
        Cursor.ws inner;
        let names =
          Cursor.list
            ~sep:(fun i -> Cursor.ws i)
            (fun i -> Cursor.ident i)
            inner
        in
        Line_names names)
      t

  let rec read_single_track t =
    if Cursor.peek_block t = Some Token.Square then read_line_names t
    else
      Cursor.enum_or_calls "grid-template"
        [
          ("none", (None : grid_template));
          ("auto", Auto);
          ("min-content", Min_content);
          ("max-content", Max_content);
          ("subgrid", Subgrid);
          ("masonry", Masonry);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~calls:
          [
            ("minmax", read_minmax);
            ("fit-content", read_fit_content);
            ( "repeat",
              fun t ->
                Cursor.call "repeat" t @@ fun inner ->
                Cursor.ws inner;
                let count = read_repeat_count inner in
                Cursor.ws inner;
                Cursor.comma inner;
                Cursor.ws inner;
                let tracks =
                  Cursor.list
                    ~sep:(fun i -> Cursor.ws i)
                    read_single_track inner
                in
                (Repeat (count, tracks) : grid_template) );
          ]
        ~default:(fun t -> Cursor.one_of [ read_length_as_grid; read_fr ] t)
        t
end

let grid_template_needs_raw_template cvs =
  let rec has_string = function
    | [] -> false
    | Component.Preserved { kind = Token.String _; _ } :: _ -> true
    | Component.Block { node = { value; _ }; _ } :: rest ->
        has_string value || has_string rest
    | Component.Func { node = { arguments; _ }; _ } :: rest ->
        has_string arguments || has_string rest
    | _ :: rest -> has_string rest
  in
  has_string cvs

let grid_template_top_level_slashes cvs =
  List.fold_left
    (fun count -> function
      | Component.Preserved { kind = Token.Delim "/"; _ } -> count + 1
      | _ -> count)
    0 cvs

let grid_template_components_well_formed cvs =
  let rec well_formed = function
    | [] -> true
    | Component.Preserved
        { kind = Token.Close _ | Token.Bad_string | Token.Bad_url; _ }
      :: _ ->
        false
    | Component.Block { node = { value; closed; _ }; _ } :: rest ->
        closed && well_formed value && well_formed rest
    | Component.Func { node = { arguments; terminated; _ }; _ } :: rest ->
        terminated && well_formed arguments && well_formed rest
    | _ :: rest -> well_formed rest
  in
  well_formed cvs

let read_grid_template_tracks t =
  let tracks =
    Cursor.list ~sep:(fun t -> Cursor.ws t) Grid_template.read_single_track t
  in
  match tracks with
  | [] -> Cursor.err t "Expected at least one grid track"
  | [ single ] -> single
  | multiple
    when List.exists
           (fun (track : grid_template) ->
             match track with None | Subgrid | Masonry -> true | _ -> false)
           multiple ->
      Cursor.err_invalid t "grid-template standalone keyword in track list"
  | multiple -> Tracks multiple

let rec read_grid_template t : grid_template =
  if Cursor.looking_at_func "var" t then
    (Var (Values.read_var read_grid_template t) : grid_template)
  else if grid_template_needs_raw_template (Cursor.remaining t) then (
    let cvs = Cursor.remaining t in
    if grid_template_top_level_slashes cvs > 1 then
      Cursor.err_invalid t "grid-template duplicate slash form";
    if not (grid_template_components_well_formed cvs) then
      Cursor.err_invalid t "grid-template malformed raw template";
    let raw = Cursor.consume_to_decl_end ~trim:true t in
    Template
      (Parser.to_string_minified (Cursor.remaining (Cursor.of_string raw))))
  else
    let rows = read_grid_template_tracks t in
    Cursor.ws t;
    if Cursor.slash_opt t then (
      Cursor.ws t;
      let columns = read_grid_template_tracks t in
      match (rows, columns) with
      | None, _ | _, None ->
          err_invalid_value t "grid-template" "none in slash form"
      | _ -> Split (rows, columns))
    else rows

let read_grid_auto_flow_clause side t =
  let rec loop seen_auto_flow seen_dense =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "auto-flow" when not seen_auto_flow ->
        let _ = Cursor.ident t in
        loop true seen_dense
    | Some "dense" when not seen_dense ->
        let _ = Cursor.ident t in
        loop seen_auto_flow true
    | _ -> (
        if not seen_auto_flow then Cursor.err_expected t "auto-flow";
        match (side, seen_dense) with
        | `Rows, false -> (Row : grid_auto_flow)
        | `Rows, true -> Row_dense
        | `Columns, false -> Column
        | `Columns, true -> Column_dense)
  in
  loop false false

let read_grid_auto_flow_tracks t : grid_template option =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Delim "/"; _ }) | None -> None
  | Some _ -> Cursor.option read_grid_template_tracks t

let grid_starts_auto_flow t =
  match Cursor.peek_ident t with
  | Some ("auto-flow" | "dense") -> true
  | _ -> false

let read_grid_split t (rows : grid_template) (columns : grid_template) :
    grid_template =
  match (rows, columns) with
  | None, _ | _, None -> err_invalid_value t "grid" "none in slash form"
  | _ -> Split (rows, columns)

let read_grid_auto_flow_rows t =
  let flow = read_grid_auto_flow_clause `Rows t in
  let auto_rows = read_grid_auto_flow_tracks t in
  Cursor.ws t;
  Cursor.slash t;
  Cursor.ws t;
  let columns = read_grid_template_tracks t in
  Auto_flow_rows (flow, auto_rows, columns)

let read_grid_auto_flow_columns t rows =
  let flow = read_grid_auto_flow_clause `Columns t in
  let auto_columns = read_grid_auto_flow_tracks t in
  Auto_flow_columns (rows, flow, auto_columns)

let read_grid_slash_rhs t rows =
  Cursor.ws t;
  if grid_starts_auto_flow t then read_grid_auto_flow_columns t rows
  else read_grid_split t rows (read_grid_template_tracks t)

let read_grid_template_or_split t =
  let rows = read_grid_template_tracks t in
  Cursor.ws t;
  if Cursor.slash_opt t then read_grid_slash_rhs t rows else rows

let rec read_grid t : grid_template =
  if Cursor.looking_at_func "var" t then
    (Var (Values.read_var read_grid t) : grid_template)
  else if grid_template_needs_raw_template (Cursor.remaining t) then
    (* CSS Grid 1 §10.1 [<'grid-template'>] form of [grid]: when the input
       contains a [<string>] token, the value is the [<line-names>? <string>
       <track-size>? <line-names>?]+ form, which [read_grid_template] already
       handles. *)
    read_grid_template t
  else if grid_starts_auto_flow t then read_grid_auto_flow_rows t
  else read_grid_template_or_split t

let read_aspect_ratio_number t =
  (* CSS Sizing 4 5: an [<aspect-ratio>] component may be a [calc()] that
     resolves to a number. Keep the number AST for normal-output fidelity; the
     printer folds constant expressions for minified output. *)
  read_number t

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
        Auto_ratio_calc (w, h)
    | _ -> Ratio_calc (w, h)
  in
  let read_auto t : aspect_ratio =
    match Cursor.peek_ident t with
    | Some "auto" -> (
        Cursor.skip t;
        (* [auto] may stand alone or be followed by a [<ratio>]. Only treat a
           following number as a ratio so a trailing separator / whitespace
           (e.g. [aspect-ratio: auto;]) resolves to plain [Auto]. *)
        match Cursor.option read_ratio t with
        | Some (w, h) -> Auto_ratio_calc (w, h)
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

let rec read_text_overflow t : text_overflow =
  let read_var t : text_overflow = Var (read_var read_text_overflow t) in
  let read_css_wide_or_var t =
    Cursor.enum_or_calls "text-overflow"
      [
        ("inherit", (Inherit : text_overflow));
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~calls:[ ("var", read_var) ]
      t
  in
  let read_single t =
    let read_string_overflow t : text_overflow = String (Cursor.string t) in
    Cursor.one_of
      [
        read_string_overflow;
        (fun t ->
          Cursor.enum "text-overflow"
            [ ("clip", (Clip : text_overflow)); ("ellipsis", Ellipsis) ]
            t);
      ]
      t
  in
  Cursor.one_of
    [
      read_css_wide_or_var;
      (fun t ->
        let first = read_single t in
        Cursor.ws t;
        (* CSS Text 4 sec. 9.1 two-value form. The declaration cursor extends
           past the value (the [;] / [!important] / [}] terminator is the
           caller's concern), so [Cursor.is_done] is the wrong gate here: try to
           read a second value, restore on failure. *)
        match
          try Some (Cursor.lookahead (fun t -> Some (read_single t)) t)
          with Cursor.Parse_error _ -> None
        with
        | Some (Some _) ->
            let second = read_single t in
            Cursor.ws t;
            Pair (first, second)
        | _ -> first);
    ]
    t

let rec read_text_wrap t : text_wrap =
  Cursor.enum_or_var "text-wrap"
    [
      ("wrap", (Wrap : text_wrap));
      ("nowrap", No_wrap);
      ("balance", Balance);
      ("pretty", Pretty);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_wrap t))
    t

let rec read_text_wrap_mode t : text_wrap_mode =
  Cursor.enum_or_var "text-wrap-mode"
    [
      ("wrap", (Wrap : text_wrap_mode));
      ("nowrap", No_wrap);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_wrap_mode t))
    t

let rec read_text_wrap_style t : text_wrap_style =
  Cursor.enum_or_var "text-wrap-style"
    [
      ("auto", (Auto : text_wrap_style));
      ("balance", Balance);
      ("pretty", Pretty);
      ("stable", Stable);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_wrap_style t))
    t

let rec read_text_box_trim t : text_box_trim =
  Cursor.enum_or_var "text-box-trim"
    [
      ("none", (None : text_box_trim));
      ("trim-start", Trim_start);
      ("trim-end", Trim_end);
      ("trim-both", Trim_both);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_box_trim t))
    t

let read_text_box_edge_keyword t : text_box_edge_keyword =
  Cursor.enum "text-box-edge"
    [
      ("text", (Text : text_box_edge_keyword));
      ("cap", Cap);
      ("ex", Ex);
      ("alphabetic", Alphabetic);
      ("ideographic", Ideographic);
      ("ideographic-ink", Ideographic_ink);
    ]
    t

let read_text_box_edge_value t : text_box_edge =
  let keywords =
    Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_text_box_edge_keyword
      t
  in
  match keywords with
  | [ Text ] | [ Ideographic_ink ] -> Edge (List.hd keywords, None)
  | [ ((Cap | Ex) as first); ((Alphabetic | Text) as second) ]
  | [ (Text as first); ((Alphabetic | Ideographic) as second) ] ->
      Edge (first, Some second)
  | _ -> Cursor.err_invalid t "text-box-edge"

let rec read_text_box_edge ?(global = true) t : text_box_edge =
  if not global then read_text_box_edge_value t
  else
    Cursor.enum_or_var "text-box-edge"
      [
        ("auto", (Auto : text_box_edge));
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~var:(fun t ->
        (Var (Values.read_var read_text_box_edge t) : text_box_edge))
      ~default:read_text_box_edge_value t

let rec read_text_box t : text_box =
  Cursor.enum_or_var "text-box"
    [
      ("initial", (Initial : text_box));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_text_box t) : text_box))
    ~default:(fun t ->
      let trim = read_text_box_trim t in
      Cursor.ws t;
      let edge : text_box_edge option =
        if Cursor.is_done t then None
        else Some (read_text_box_edge ~global:false t)
      in
      (Box (trim, edge) : text_box))
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

let read_line_fit_edge_keyword t : line_fit_edge_keyword =
  Cursor.enum "line-fit-edge"
    [
      ("leading", (Leading : line_fit_edge_keyword));
      ("text", Text);
      ("cap", Cap);
      ("ex", Ex);
      ("alphabetic", Alphabetic);
      ("ideographic", Ideographic);
      ("ideographic-ink", Ideographic_ink);
    ]
    t

let rec read_line_fit_edge t : line_fit_edge =
  Cursor.enum_or_var "line-fit-edge"
    [
      ("inherit", (Inherit : line_fit_edge));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_line_fit_edge t) : line_fit_edge))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_line_fit_edge_keyword t
      in
      match keywords with
      | [ Leading ] | [ Text ] | [ Ideographic_ink ] ->
          (Edge (List.hd keywords, None) : line_fit_edge)
      | [ ((Cap | Ex) as first); ((Alphabetic | Text) as second) ]
      | [ (Text as first); (Alphabetic as second) ] ->
          Edge (first, Some second)
      | _ -> Cursor.err_invalid t "line-fit-edge")
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
        List.length (List.filter (( = ) keyword) keywords) > 1
      in
      if List.exists duplicate keywords then
        Cursor.err_invalid t "min-intrinsic-sizing";
      Sizing keywords)
    t

let rec read_ruby_merge t : ruby_merge =
  Cursor.enum_or_var "ruby-merge"
    [
      ("separate", (Separate : ruby_merge));
      ("merge", Merge);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_merge t) : ruby_merge))
    t

let rec read_ruby_align t : ruby_align =
  Cursor.enum_or_var "ruby-align"
    [
      ("start", (Start : ruby_align));
      ("center", Center);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_align t) : ruby_align))
    t

let rec read_ruby_overhang t : ruby_overhang =
  Cursor.enum_or_var "ruby-overhang"
    [
      ("auto", (Auto : ruby_overhang));
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_overhang t) : ruby_overhang))
    t

let read_ruby_position_keyword t : ruby_position_keyword =
  Cursor.enum "ruby-position"
    [
      ("alternate", (Alternate : ruby_position_keyword));
      ("over", Over);
      ("under", Under);
      ("inter-character", Inter_character);
    ]
    t

let rec read_ruby_position t : ruby_position =
  Cursor.enum_or_var "ruby-position"
    [
      ("inherit", (Inherit : ruby_position));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_ruby_position t) : ruby_position))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_ruby_position_keyword t
      in
      let duplicate keyword =
        List.length (List.filter (( = ) keyword) keywords) > 1
      in
      let valid =
        match keywords with
        | [ _ ] -> true
        | [ Alternate; (Over | Under) ] | [ (Over | Under); Alternate ] -> true
        | _ -> false
      in
      if (not valid) || List.exists duplicate keywords then
        Cursor.err_invalid t "ruby-position";
      (Position keywords : ruby_position))
    t

let rec read_text_spacing_trim t : text_spacing_trim =
  Cursor.enum_or_var "text-spacing-trim"
    [
      ("normal", (Normal : text_spacing_trim));
      ("space-all", Space_all);
      ("trim-start", Trim_start);
      ("space-first", Space_first);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_spacing_trim t))
    t

let rec read_hyphenate_limit_chars t : hyphenate_limit_chars =
  let read_counts t =
    let counts =
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:3 Cursor.int t
    in
    let check_count n =
      if n < 1 then Cursor.err_invalid t "hyphenate-limit-chars must be >= 1"
    in
    List.iter check_count counts;
    Cursor.ws t;
    Cursor.expect_eof t;
    match counts with
    | [ a ] -> (One a : hyphenate_limit_chars)
    | [ a; b ] -> Two (a, b)
    | [ a; b; c ] -> Three (a, b, c)
    | _ -> Cursor.err_invalid t "expected one to three integers"
  in
  Cursor.enum_or_calls "hyphenate-limit-chars"
    [
      ("auto", (Auto : hyphenate_limit_chars));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_hyphenate_limit_chars t)) ]
    ~default:read_counts t

let rec read_white_space t : white_space =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "preserve" ->
      ignore (Cursor.ident t : string);
      Cursor.ws t;
      Cursor.expect_string "nowrap" t;
      Preserve_nowrap
  | _ ->
      Cursor.enum_or_var "white-space"
        [
          ("normal", (Normal : white_space));
          ("nowrap", Nowrap);
          ("pre", Pre);
          ("pre-wrap", Pre_wrap);
          ("pre-line", Pre_line);
          ("break-spaces", Break_spaces);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (read_var read_white_space t))
        t

let rec read_word_break t : word_break =
  Cursor.enum_or_var "word-break"
    [
      ("normal", (Normal : word_break));
      ("break-all", Break_all);
      ("keep-all", Keep_all);
      ("break-word", Break_word);
      ("auto-phrase", Auto_phrase);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_word_break t))
    t

let rec read_overflow_wrap t : overflow_wrap =
  Cursor.enum_or_var "overflow-wrap"
    [
      ("normal", (Normal : overflow_wrap));
      ("break-word", Break_word);
      ("anywhere", Anywhere);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_overflow_wrap t))
    t

let rec read_hyphens t : hyphens =
  Cursor.enum_or_var "hyphens"
    [
      ("none", (None : hyphens));
      ("manual", Manual);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_hyphens t))
    t

let rec read_line_height t : line_height =
  let read_var t : line_height = Var (read_var read_line_height t) in
  let read_calc t : line_height =
    Calc (read_calc read_line_height t |> numeric_line_height_calc_leaves)
  in
  Cursor.enum_or_calls "line-height"
    [
      ("normal", Normal);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:read_line_height_length t

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

let rec read_list_style_image t : list_style_image =
  let read_url t = (Url (Cursor.url t) : list_style_image) in
  let read_var t : list_style_image = Var (read_var read_list_style_image t) in
  Cursor.one_of
    [
      read_url;
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
    ]
    t

(* Parse the [list-style] shorthand into a typed [list_style_shorthand] record.
   Each slot is recognised by the longhand reader; a single bare [none]
   populates both [type_] and [image] per CSS Lists 3 sec. 4.1. *)
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

let rec read_border_collapse t : border_collapse =
  Cursor.enum_or_var "border-collapse"
    [
      ("collapse", (Collapse : border_collapse));
      ("separate", Separate);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_border_collapse t) : border_collapse))
    t

let rec read_user_select t : user_select =
  Cursor.enum_or_var "user-select"
    [
      ("none", (None : user_select));
      ("auto", Auto);
      ("text", Text);
      ("all", All);
      ("contain", Contain);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_user_select t) : user_select))
    t

let rec read_pointer_events t : pointer_events =
  Cursor.enum_or_var "pointer-events"
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
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_pointer_events t))
    t

let touch_action_is_var t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      String.equal (String.lowercase_ascii name) "var"
  | _ -> false

let touch_action_starts_keyword t =
  match Cursor.peek_ident t with
  | Some
      ( "auto" | "none" | "manipulation" | "inherit" | "initial" | "unset"
      | "revert" | "revert-layer" ) ->
      true
  | _ -> false

let read_touch_action_keyword t =
  Cursor.enum "touch-action"
    [
      ("auto", (Auto : touch_action));
      ("none", None);
      ("manipulation", Manipulation);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    t

let read_touch_action_gesture t =
  Cursor.enum "touch-action gesture"
    [
      ("pan-x", (Pan_x : touch_action));
      ("pan-y", Pan_y);
      ("pan-left", Pan_left);
      ("pan-right", Pan_right);
      ("pan-up", Pan_up);
      ("pan-down", Pan_down);
      ("pinch-zoom", Pinch_zoom);
    ]
    t

let check_touch_action_seen t has_horizontal has_vertical has_pinch = function
  | Pan_x | Pan_left | Pan_right ->
      if !has_horizontal then
        Cursor.err t "duplicate horizontal touch-action gesture";
      has_horizontal := true
  | Pan_y | Pan_up | Pan_down ->
      if !has_vertical then
        Cursor.err t "duplicate vertical touch-action gesture";
      has_vertical := true
  | Pinch_zoom ->
      if !has_pinch then Cursor.err t "duplicate pinch-zoom touch-action";
      has_pinch := true
  | _ -> Cursor.err t "invalid touch-action gesture"

let validate_touch_actions t actions =
  let has_horizontal = ref false in
  let has_vertical = ref false in
  let has_pinch = ref false in
  List.iter
    (check_touch_action_seen t has_horizontal has_vertical has_pinch)
    actions;
  match actions with [ action ] -> action | _ -> Actions actions

let rec read_touch_action_var t : touch_action var =
  Values.read_var read_touch_action t

and read_touch_action t : touch_action =
  let rec read_vars acc =
    Cursor.ws t;
    if touch_action_is_var t then read_vars (read_touch_action_var t :: acc)
    else List.rev acc
  in
  if touch_action_is_var t then
    let first = read_touch_action_var t in
    Vars (first :: read_vars [])
  else if touch_action_starts_keyword t then read_touch_action_keyword t
  else
    validate_touch_actions t
      (Cursor.list ~at_least:1 read_touch_action_gesture t)

let rec read_resize t : resize =
  Cursor.enum_or_var "resize"
    [
      ("none", (None : resize));
      ("both", Both);
      ("horizontal", Horizontal);
      ("vertical", Vertical);
      ("block", Block);
      ("inline", Inline);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_resize t))
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

let rec read_field_sizing t : field_sizing =
  Cursor.enum_or_var "field-sizing"
    [
      ("content", (Content : field_sizing));
      ("fixed", Fixed);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_field_sizing t))
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
    ~default:read_content_string t

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

let rec read_content_visibility t : content_visibility =
  let read_var t : content_visibility =
    Var (read_var read_content_visibility t)
  in
  Cursor.enum_or_calls "content-visibility"
    [
      ("visible", (Visible : content_visibility));
      ("auto", Auto);
      ("hidden", Hidden);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
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

let rec read_container_type (t : Cursor.t) : container_type =
  Cursor.enum_or_var "container-type"
    [
      ("normal", (Normal : container_type));
      ("inline-size", Inline_size);
      ("size", Size);
      ("scroll-state", Scroll_state);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_container_type t) : container_type))
    t

let container_name_reserved =
  [ "none"; "default"; "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let read_container_custom_ident t =
  let ident = Cursor.ident ~keep_case:true t in
  if List.mem (String.lowercase_ascii ident) container_name_reserved then
    Cursor.err_invalid t ("reserved container-name ident: " ^ ident)
  else ident

let rec read_container_name (t : Cursor.t) : container_name =
  let keywords : (string * container_name) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "container-name" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_container_name t) : container_name))
     ~default:(fun t ->
       (Names
          (Cursor.list ~sep:Cursor.ws ~at_least:1 read_container_custom_ident t)
         : container_name))
     t
    : container_name)

let read_dashed_ident t =
  let ident = Cursor.ident ~keep_case:true t in
  if String.length ident >= 2 && ident.[0] = '-' && ident.[1] = '-' then ident
  else Cursor.err_invalid t ("expected dashed ident, got: " ^ ident)

let rec read_anchor_name (t : Cursor.t) : anchor_name =
  let keywords : (string * anchor_name) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "anchor-name" keywords
     ~var:(fun t -> (Var (Values.read_var read_anchor_name t) : anchor_name))
     ~default:(fun t ->
       (Names (Cursor.list ~sep:Cursor.comma ~at_least:1 read_dashed_ident t)
         : anchor_name))
     t
    : anchor_name)

let rec read_position_anchor (t : Cursor.t) : position_anchor =
  let keywords : (string * position_anchor) list =
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "position-anchor" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_position_anchor t) : position_anchor))
     ~default:(fun t -> (Anchor (read_dashed_ident t) : position_anchor))
     t
    : position_anchor)

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

let rec read_scrollbar_width (t : Cursor.t) : scrollbar_width =
  let keywords : (string * scrollbar_width) list =
    [
      ("auto", Auto);
      ("thin", Thin);
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "scrollbar-width" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_scrollbar_width t) : scrollbar_width))
     t
    : scrollbar_width)

let rec read_scrollbar_color (t : Cursor.t) : scrollbar_color =
  let keywords : (string * scrollbar_color) list =
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "scrollbar-color" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_scrollbar_color t) : scrollbar_color))
     ~default:(fun t ->
       let thumb = Values.read_color t in
       Cursor.ws t;
       let track = Values.read_color t in
       (Colors (thumb, track) : scrollbar_color))
     t
    : scrollbar_color)

let rec read_scrollbar_gutter (t : Cursor.t) : scrollbar_gutter =
  let keywords : (string * scrollbar_gutter) list =
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "scrollbar-gutter" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_scrollbar_gutter t) : scrollbar_gutter))
     ~default:(fun t ->
       Cursor.expect_string "stable" t;
       Cursor.ws t;
       match Cursor.peek_ident t with
       | Some "both-edges" ->
           let _ = Cursor.ident t in
           Stable_both_edges
       | Some s ->
           Cursor.err_invalid t
             (String.concat "" [ "unexpected scrollbar-gutter modifier: "; s ])
       | None -> Stable)
     t
    : scrollbar_gutter)

let rec read_font_palette (t : Cursor.t) : font_palette =
  let keywords : (string * font_palette) list =
    [
      ("normal", Normal);
      ("light", Light);
      ("dark", Dark);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "font-palette" keywords
     ~var:(fun t -> (Var (Values.read_var read_font_palette t) : font_palette))
     ~default:(fun t -> (Palette (read_dashed_ident t) : font_palette))
     t
    : font_palette)

let read_font_synthesis_feature t : font_synthesis_feature =
  Cursor.enum "font-synthesis feature"
    [
      ("weight", Weight);
      ("style", Style);
      ("small-caps", Small_caps);
      ("position", Position);
    ]
    t

let rec read_font_synthesis (t : Cursor.t) : font_synthesis =
  let keywords : (string * font_synthesis) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_features t =
    let features =
      Cursor.list ~sep:Cursor.ws ~at_least:1 read_font_synthesis_feature t
    in
    let rec duplicates = function
      | [] -> false
      | x :: xs -> List.mem x xs || duplicates xs
    in
    if duplicates features then
      Cursor.err_invalid t "duplicate font-synthesis feature";
    (Features features : font_synthesis)
  in
  (Cursor.enum_or_var "font-synthesis" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_font_synthesis t) : font_synthesis))
     ~default:read_features t
    : font_synthesis)

let rec read_animation_timeline (t : Cursor.t) : animation_timeline =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_animation_timeline t) : animation_timeline)
  | Some (Component.Func fn) when not fn.node.terminated ->
      Cursor.err_invalid t
        (String.concat "" [ "unterminated function "; fn.node.name; "(...)" ])
  | Some (Component.Func fn)
    when fn.node.name = "scroll" || fn.node.name = "view" ->
      let _ = Cursor.next t in
      let args = Parser.string_of_components fn.node.arguments in
      if fn.node.name = "scroll" then Scroll args else View args
  | _ ->
      let keywords : (string * animation_timeline) list =
        [
          ("none", None);
          ("auto", Auto);
          ("initial", Initial);
          ("inherit", Inherit);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
      in
      (Cursor.enum "animation-timeline" keywords
         ~default:(fun t -> (Name (read_dashed_ident t) : animation_timeline))
         t
        : animation_timeline)

let rec read_view_transition_name (t : Cursor.t) : view_transition_name =
  let keywords : (string * view_transition_name) list =
    [
      ("none", None);
      ("match-element", Match_element);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_name t =
    let name = Cursor.ident ~keep_case:true t in
    if String.lowercase_ascii name = "auto" then
      Cursor.err_invalid t "invalid view-transition-name: auto";
    (Name name : view_transition_name)
  in
  (Cursor.enum_or_var "view-transition-name" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_view_transition_name t)
         : view_transition_name))
     ~default:read_name t
    : view_transition_name)

let view_transition_class_reserved =
  [ "none"; "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let read_view_transition_class_ident t =
  let ident = Cursor.ident ~keep_case:true t in
  if List.mem (String.lowercase_ascii ident) view_transition_class_reserved then
    Cursor.err_invalid t ("reserved view-transition-class ident: " ^ ident)
  else ident

let rec read_view_transition_class t : view_transition_class =
  let keywords : (string * view_transition_class) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "view-transition-class" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_view_transition_class t)
         : view_transition_class))
     ~default:(fun t ->
       (Classes
          (Cursor.list ~sep:Cursor.ws ~at_least:1
             read_view_transition_class_ident t)
         : view_transition_class))
     t
    : view_transition_class)

let rec read_image_orientation (t : Cursor.t) : image_orientation =
  let keywords : (string * image_orientation) list =
    [
      ("none", None);
      ("from-image", From_image);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "image-orientation" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_image_orientation t) : image_orientation))
     t
    : image_orientation)

let rec read_image_rendering (t : Cursor.t) : image_rendering =
  let keywords : (string * image_rendering) list =
    [
      ("auto", Auto);
      ("smooth", Smooth);
      ("high-quality", High_quality);
      ("crisp-edges", Crisp_edges);
      ("pixelated", Pixelated);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "image-rendering" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_image_rendering t) : image_rendering))
     t
    : image_rendering)

let read_resolution t : resolution =
  match Cursor.dimension_opt t with
  | None -> Cursor.err_expected t "<resolution>"
  | Some (value, unit_) -> (
      if value < 0. then Cursor.err_invalid t "negative resolution";
      match String.lowercase_ascii unit_ with
      | "dpi" -> Dpi value
      | "dpcm" -> Dpcm value
      | "dppx" -> Dppx value
      | "x" -> (X value : resolution)
      | _ -> Cursor.err_expected t "<resolution>")

let image_resolution_of_parts (t : Cursor.t)
    ~(parts : [ `From_image | `Snap ] list) (resolution : resolution option) :
    image_resolution =
  let from_image = List.mem `From_image parts in
  let snap = List.mem `Snap parts in
  match (from_image, snap, resolution) with
  | false, false, None -> Cursor.err_invalid t "image-resolution"
  | false, false, Some r -> Resolution r
  | true, false, None -> From_image
  | true, false, Some r -> From_image_resolution r
  | false, true, None -> Cursor.err_invalid t "image-resolution"
  | false, true, Some r -> Snap r
  | true, true, None -> From_image_snap
  | true, true, Some r -> From_image_snap_resolution r

let image_resolution_has_dimension t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Dimension _; _ }) -> true
  | _ -> false

let read_image_resolution_from_image t parts resolution =
  if List.mem `From_image parts then Cursor.err_invalid t "duplicate from-image";
  if parts <> [] || Option.is_some resolution then
    Cursor.err_invalid t "from-image must precede image resolution";
  ignore (Cursor.ident t : string);
  (`Continue (`From_image :: parts, resolution)
    : [ `Continue of [ `From_image | `Snap ] list * resolution option
      | `Done of image_resolution ])

let read_image_resolution_snap t parts resolution =
  if List.mem `Snap parts then Cursor.err_invalid t "duplicate snap";
  if (not (List.mem `From_image parts)) && Option.is_none resolution then
    Cursor.err_invalid t "snap must follow image resolution";
  ignore (Cursor.ident t : string);
  (`Continue (`Snap :: parts, resolution)
    : [ `Continue of [ `From_image | `Snap ] list * resolution option
      | `Done of image_resolution ])

let read_image_resolution_dimension t (parts : [ `From_image | `Snap ] list)
    (resolution : resolution option) =
  match resolution with
  | None ->
      if List.mem `Snap parts then
        Cursor.err_invalid t "image resolution must precede snap";
      `Continue (parts, Some (read_resolution t))
  | Some _ -> Cursor.err_invalid t "duplicate resolution"

let read_image_resolution_step t parts resolution =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "from-image" -> read_image_resolution_from_image t parts resolution
  | Some "snap" -> read_image_resolution_snap t parts resolution
  | _ when image_resolution_has_dimension t ->
      read_image_resolution_dimension t parts resolution
  | _ -> `Done (image_resolution_of_parts t ~parts resolution)

let read_image_resolution_value t =
  let rec loop parts resolution =
    match read_image_resolution_step t parts resolution with
    | `Continue (parts, resolution) -> loop parts resolution
    | `Done result -> result
  in
  loop [] (None : resolution option)

let rec read_image_resolution (t : Cursor.t) : image_resolution =
  Cursor.enum_or_var "image-resolution"
    [
      ("initial", (Initial : image_resolution));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_image_resolution t))
    ~default:read_image_resolution_value t

let read_contain_intrinsic_size_item t : contain_intrinsic_size_item =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "auto" ->
      let _ = Cursor.ident t in
      Cursor.ws t;
      (Auto (Values.read_length ~allow_negative:false ~with_keywords:false t)
        : contain_intrinsic_size_item)
  | _ ->
      Length (Values.read_length ~allow_negative:false ~with_keywords:false t)

let rec read_contain_intrinsic_size (t : Cursor.t) : contain_intrinsic_size =
  let keywords : (string * contain_intrinsic_size) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_intrinsic t =
    let first = read_contain_intrinsic_size_item t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_semicolon t then Intrinsic (first, None)
    else
      let second = read_contain_intrinsic_size_item t in
      Intrinsic (first, Some second)
  in
  (Cursor.enum_or_var "contain-intrinsic-size" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_contain_intrinsic_size t)
         : contain_intrinsic_size))
     ~default:read_intrinsic t
    : contain_intrinsic_size)

let rec read_contain_intrinsic_longhand (t : Cursor.t) :
    contain_intrinsic_longhand =
  let keywords : (string * contain_intrinsic_longhand) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "contain-intrinsic longhand" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_contain_intrinsic_longhand t)
         : contain_intrinsic_longhand))
     ~default:(fun t ->
       (Size (read_contain_intrinsic_size_item t) : contain_intrinsic_longhand))
     t
    : contain_intrinsic_longhand)

let rec read_container_shorthand (t : Cursor.t) : container_shorthand =
  (* Syntax: container: [<custom-ident>] [ / <container-type> ]? *)
  let is_container_type_keyword ident =
    match String.lowercase_ascii ident with
    | "normal" | "inline-size" | "size" | "scroll-state" -> true
    | _ -> false
  in
  let validate_name_before_slash ident =
    if is_container_type_keyword ident then
      Cursor.err_invalid t
        ("container shorthand type keyword used as name: " ^ ident);
    if List.mem (String.lowercase_ascii ident) container_name_reserved then
      Cursor.err_invalid t ("reserved container-name ident: " ^ ident)
  in
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_container_shorthand t) : container_shorthand)
  | _ -> (
      let first = Cursor.ident t in
      Cursor.ws t;
      match Cursor.peek_delim t with
      | Some '/' ->
          (* We have: name / type *)
          validate_name_before_slash first;
          Cursor.expect '/' t;
          Cursor.ws t;
          let ctype = read_container_type t in
          Shorthand { name = Some first; ctype = Some ctype }
      | _ -> (
          (* Just a name, or just a type? Check if it's a valid
             container-type *)
          match first with
          | "normal" -> Shorthand { name = None; ctype = Some Normal }
          | "inline-size" -> Shorthand { name = None; ctype = Some Inline_size }
          | "size" -> Shorthand { name = None; ctype = Some Size }
          | "scroll-state" ->
              Shorthand { name = None; ctype = Some Scroll_state }
          | "initial" -> Initial
          | "inherit" -> Inherit
          | "unset" -> Unset
          | "revert" -> Revert
          | "revert-layer" -> Revert_layer
          | _ -> Shorthand { name = Some first; ctype = None }))

let rec read_contain t : contain =
  let read_contain_value t : contain =
    if Cursor.looking_at t "var(" then Var (read_var read_contain t)
    else
      Cursor.enum "contain-value"
        [
          ("size", Size);
          ("inline-size", Inline_size);
          ("layout", Layout);
          ("style", Style);
          ("paint", Paint);
        ]
        t
  in
  let read_contain_list t =
    let values = Cursor.list ~sep:Cursor.ws read_contain_value t in
    (* Per CSS Contain Module: each contain value may appear at most once.
       Compare on the constructor shape (Var omitted from the duplicate check
       since two equal var refs are not authored ambiguity). *)
    let rec has_duplicate = function
      | [] | [ _ ] -> false
      | x :: rest -> List.exists (fun y -> y = x) rest || has_duplicate rest
    in
    match values with
    | [] -> err_invalid_value t "contain" "expected contain value(s)"
    | _ when has_duplicate values ->
        err_invalid_value t "contain" "duplicate contain value"
    | [ v ] -> v
    | vs -> List vs
  in
  Cursor.enum "contain"
    [
      ("none", (None : contain));
      ("strict", Strict);
      ("content", Content);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:read_contain_list t

let rec read_isolation t : isolation =
  Cursor.enum_or_var "isolation"
    [
      ("auto", (Auto : isolation));
      ("isolate", Isolate);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_isolation t))
    t

let rec read_break_value t : break_value =
  Cursor.enum_or_var "break"
    [
      ("auto", (Auto : break_value));
      ("avoid", Avoid);
      ("all", All);
      (* CSS Fragmentation 3 §6: legacy [page-break-*: always] maps to [break-*:
         page]. The reader accepts the legacy spelling so the page-break alias
         dispatch (which routes to [Break_before/after]) can keep using this
         reader. *)
      ("always", Page);
      ("avoid-page", Avoid_page);
      ("page", Page);
      ("left", Left);
      ("right", Right);
      ("recto", Recto);
      ("verso", Verso);
      ("avoid-column", Avoid_column);
      ("column", Column);
      ("avoid-region", Avoid_region);
      ("region", Region);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_break_value t))
    t

let rec read_break_inside_value t : break_inside_value =
  Cursor.enum_or_var "break-inside"
    [
      ("auto", (Auto : break_inside_value));
      ("avoid", Avoid);
      ("avoid-page", Avoid_page);
      ("avoid-column", Avoid_column);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_break_inside_value t))
    t

let read_page_break_value t : page_break_value =
  Cursor.enum "page-break"
    [
      ("auto", (Auto : page_break_value));
      ("always", Always);
      ("avoid", Avoid);
      ("left", Left);
      ("right", Right);
      ("inherit", Inherit);
    ]
    t

let read_page_break_inside_value t : page_break_inside_value =
  Cursor.enum "page-break-inside"
    [
      ("auto", (Auto : page_break_inside_value));
      ("avoid", Avoid);
      ("inherit", Inherit);
    ]
    t

let read_columns_count t =
  let n = Cursor.int t in
  if n <= 0 then Cursor.err_invalid t "column count must be positive";
  n

let read_columns_component t =
  Cursor.one_of
    [
      (fun t ->
        Cursor.expect_string "auto" t;
        `Auto);
      (fun t -> `Count (read_columns_count t));
      (fun t -> `Width (read_length t));
    ]
    t

let combine_columns_components t a b : columns_value =
  match (a, b) with
  | `Auto, `Auto -> (Auto : columns_value)
  | `Auto, `Count n | `Count n, `Auto -> Auto_count n
  | `Auto, `Width w | `Width w, `Auto -> Width w
  | `Width w, `Count n | `Count n, `Width w -> Both (w, n)
  | `Count _, `Count _ -> Cursor.err_invalid t "duplicate column-count"
  | `Width _, `Width _ -> Cursor.err_invalid t "duplicate column-width"

let columns_value_of_component :
    [< `Auto | `Count of int | `Width of Values.length ] -> columns_value =
  function
  | `Auto -> Auto
  | `Count n -> (Count n : columns_value)
  | `Width w -> Width w

let read_columns_components t : columns_value =
  let first = read_columns_component t in
  Cursor.ws t;
  match Cursor.option read_columns_component t with
  | Some second -> combine_columns_components t first second
  | None -> columns_value_of_component first

let rec read_columns_value t : columns_value =
  (* CSS Multicol 2 sec. 6.1: [<'column-width'> || <'column-count'>], where
     column-width is [auto | <length>] and column-count is [auto | <integer>].
     Read up to two space-separated components in any order, then assign the
     length to the width slot and the integer to the count slot. An explicit
     [auto] keeps the width unset; [columns: auto 3] therefore differs from the
     bare [columns: 3] only in spelling, captured by [Auto_count]. *)
  Cursor.enum_or_var "columns"
    [
      ("inherit", (Inherit : columns_value));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_columns_value t))
    ~default:read_columns_components t

let rec read_column_width t : column_width =
  Cursor.enum_or_var "column-width"
    [
      ("auto", (Auto : column_width));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_width t))
    ~default:(fun t -> (Width (read_length t) : column_width))
    t

let rec read_column_count t : column_count =
  Cursor.enum_or_var "column-count"
    [
      ("auto", (Auto : column_count));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_count t))
    ~default:(fun t ->
      let n = Cursor.int t in
      if n <= 0 then Cursor.err_invalid t "column count must be positive";
      Count n)
    t

let rec read_column_span t : column_span =
  Cursor.enum_or_var "column-span"
    [
      ("none", (None : column_span));
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_column_span t))
    t

let rec read_scroll_behavior (t : Cursor.t) : scroll_behavior =
  Cursor.enum_or_var "scroll-behavior"
    [
      ("auto", (Auto : scroll_behavior));
      ("smooth", Smooth);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_scroll_behavior t) : scroll_behavior))
    t

let rec read_scroll_snap_align (t : Cursor.t) : scroll_snap_align =
  Cursor.ws t;
  let read_single t =
    Cursor.enum_or_var "scroll-snap-align"
      [
        ("none", (None : scroll_snap_align));
        ("start", Start);
        ("end", End);
        ("center", Center);
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~var:(fun t ->
        (Var (Values.read_var read_scroll_snap_align t) : scroll_snap_align))
      t
  in
  let first = read_single t in
  (* The optional second keyword is probed with backtracking so the reader stops
     cleanly at whatever follows the value (the declaration's [;], a block
     close, ...). *)
  match Cursor.option read_single t with
  | Option.None -> first
  | Option.Some second ->
      (match first with
      | Inherit | Initial | Unset | Revert | Revert_layer ->
          Cursor.err_invalid t "scroll-snap-align CSS-wide value in pair"
      | _ -> ());
      Snap_align_pair (first, second)

let rec read_timeline_axis t : timeline_axis =
  Cursor.enum_or_var "timeline-axis"
    [
      ("block", (Block : timeline_axis));
      ("inline", (Inline : timeline_axis));
      ("x", (X : timeline_axis));
      ("y", (Y : timeline_axis));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_timeline_axis t))
    t

let rec read_timeline_name t : timeline_name =
  Cursor.enum_or_var "timeline-name"
    [
      ("none", (None : timeline_name));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_timeline_name t) : timeline_name))
    ~default:(fun t ->
      (Names (Cursor.list ~sep:Cursor.comma ~at_least:1 read_dashed_ident t)
        : timeline_name))
    t

let read_timeline_shorthand_item t : timeline_shorthand_item =
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  if not (String.starts_with ~prefix:"--" name) then
    Cursor.err_invalid t "timeline name";
  Cursor.ws t;
  let axis = read_timeline_axis t in
  { name; axis }

let rec read_timeline_shorthand t : timeline_shorthand =
  Cursor.enum_or_var "timeline"
    [
      ("none", (None : timeline_shorthand));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_timeline_shorthand t))
    ~default:(fun t ->
      Timelines
        (Cursor.list ~sep:Cursor.comma ~at_least:1 read_timeline_shorthand_item
           t))
    t

let read_timeline_inset_item t : timeline_inset_item =
  Cursor.enum "timeline-inset item"
    [ ("auto", (Auto : timeline_inset_item)) ]
    ~default:(fun t ->
      (Length
         (read_length_percentage ~allow_negative:false ~with_keywords:false t)
        : timeline_inset_item))
    t

let rec read_timeline_inset t : timeline_inset =
  Cursor.enum_or_var "timeline-inset"
    [
      ("initial", (Initial : timeline_inset));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_timeline_inset t) : timeline_inset))
    ~default:(fun t ->
      match Cursor.list ~at_least:1 ~at_most:2 read_timeline_inset_item t with
      | [ first ] -> (Inset (first, None) : timeline_inset)
      | [ first; second ] -> (Inset (first, Some second) : timeline_inset)
      | _ -> Cursor.err_expected t "timeline-inset")
    t

let read_position_try_fallback t =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some (("flip-block" | "flip-inline" | "flip-start") as keyword) -> (
      let _ = Cursor.ident t in
      match keyword with
      | "flip-block" -> (Flip_block : position_try_fallback)
      | "flip-inline" -> Flip_inline
      | "flip-start" -> Flip_start
      | _ -> assert false)
  | _ -> Name (read_dashed_ident t)

let rec read_position_try_fallbacks t : position_try_fallbacks =
  let keywords : (string * position_try_fallbacks) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "position-try-fallbacks" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_position_try_fallbacks t)
         : position_try_fallbacks))
     ~default:(fun t ->
       (Fallbacks
          (Cursor.list ~sep:Cursor.comma ~at_least:1 read_position_try_fallback
             t)
         : position_try_fallbacks))
     t
    : position_try_fallbacks)

let rec read_position_try_order t : position_try_order =
  Cursor.enum_or_var "position-try-order"
    [
      ("normal", (Normal : position_try_order));
      ("most-width", Most_width);
      ("most-height", Most_height);
      ("most-block-size", Most_block_size);
      ("most-inline-size", Most_inline_size);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_position_try_order t) : position_try_order))
    t

let rec read_position_try t : position_try =
  Cursor.enum_or_var "position-try"
    [
      ("initial", (Initial : position_try));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_position_try t))
    ~default:(fun t ->
      (* [<order> || <fallbacks>]: an optional leading order keyword (which
         never collides with a fallback dashed-ident or try-tactic), then the
         fallbacks. An order alone leaves fallbacks at its initial [none]. *)
      let order : position_try_order =
        match Cursor.peek_ident t with
        | Some
            ( "normal" | "most-width" | "most-height" | "most-block-size"
            | "most-inline-size" ) ->
            let o = read_position_try_order t in
            Cursor.ws t;
            o
        | _ -> Normal
      in
      let fallbacks =
        match Cursor.option read_position_try_fallbacks t with
        | Some f -> f
        | None -> (None : position_try_fallbacks)
      in
      Try (order, fallbacks))
    t

let rec read_position_visibility t : position_visibility =
  Cursor.enum_or_var "position-visibility"
    [
      ("always", (Always : position_visibility));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_position_visibility t) : position_visibility))
    ~default:(fun t ->
      let read_condition t =
        Cursor.enum "position-visibility condition"
          [
            ( "anchors-visible",
              (Anchors_visible : position_visibility_condition) );
            ("no-overflow", No_overflow);
          ]
          t
      in
      let conditions =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_condition t
      in
      let duplicate condition =
        List.length (List.filter (( = ) condition) conditions) > 1
      in
      if List.exists duplicate conditions then
        Cursor.err_invalid t "position-visibility";
      (Conditions conditions : position_visibility))
    t

let read_position_area_keyword t : position_area_keyword =
  Cursor.enum "position-area keyword"
    [
      ("top", (Top : position_area_keyword));
      ("bottom", Bottom);
      ("left", Left);
      ("right", Right);
      ("center", Center);
      ("span-top", Span_top);
      ("span-bottom", Span_bottom);
      ("span-left", Span_left);
      ("span-right", Span_right);
      ("x-start", X_start);
      ("x-end", X_end);
      ("y-start", Y_start);
      ("y-end", Y_end);
      ("span-x-start", Span_x_start);
      ("span-x-end", Span_x_end);
      ("span-y-start", Span_y_start);
      ("span-y-end", Span_y_end);
      ("inline-start", Inline_start);
      ("inline-end", Inline_end);
      ("block-start", Block_start);
      ("block-end", Block_end);
      ("span-inline-start", Span_inline_start);
      ("span-inline-end", Span_inline_end);
      ("span-block-start", Span_block_start);
      ("span-block-end", Span_block_end);
      ("span-all", Span_all);
    ]
    t

type position_area_axis = Horizontal | Vertical | Either

let position_area_axis (keyword : position_area_keyword) =
  match keyword with
  | Left | Right | Span_left | Span_right | X_start | X_end | Span_x_start
  | Span_x_end | Inline_start | Inline_end | Span_inline_start | Span_inline_end
    ->
      Horizontal
  | Top | Bottom | Span_top | Span_bottom | Y_start | Y_end | Span_y_start
  | Span_y_end | Block_start | Block_end | Span_block_start | Span_block_end ->
      Vertical
  | Center | Span_all -> Either

let compatible_position_area_keywords first second =
  match (position_area_axis first, position_area_axis second) with
  | Horizontal, Horizontal | Vertical, Vertical -> false
  | _ -> true

let rec read_position_area t : position_area =
  Cursor.enum_or_var "position-area"
    [
      ("none", (None : position_area));
      ("initial", (Initial : position_area));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_position_area t) : position_area))
    ~default:(fun t ->
      match Cursor.list ~at_least:1 ~at_most:2 read_position_area_keyword t with
      | [ first ] -> (Area (first, None) : position_area)
      | [ first; second ] ->
          if not (compatible_position_area_keywords first second) then
            Cursor.err_invalid t
              "position-area keywords must cover different axes";
          (Area (first, Some second) : position_area)
      | _ -> Cursor.err_expected t "position-area")
    t

let read_page_size_name t : page_size_name =
  Cursor.enum "page-size name"
    [
      ("a5", A5);
      ("a4", A4);
      ("a3", A3);
      ("b5", B5);
      ("b4", B4);
      ("jis-b5", Jis_b5);
      ("jis-b4", Jis_b4);
      ("letter", Letter);
      ("legal", Legal);
      ("ledger", Ledger);
    ]
    t

let read_page_size_orientation t : page_size_orientation =
  Cursor.enum "page-size orientation"
    [ ("portrait", Portrait); ("landscape", Landscape) ]
    t

let rec read_page_size t : page_size =
  let read_var_ps t : page_size = Var (read_var read_page_size t) in
  let at_end t = Cursor.is_done t || Cursor.peek_semicolon t in
  let expect_value_end t = if not (at_end t) then Cursor.expect_eof t in
  let read_named t =
    let name = read_page_size_name t in
    Cursor.ws t;
    if at_end t then Named name
    else
      let orientation = read_page_size_orientation t in
      Cursor.ws t;
      expect_value_end t;
      Named_oriented (name, orientation)
  in
  let read_oriented t =
    let orientation = read_page_size_orientation t in
    Cursor.ws t;
    expect_value_end t;
    Oriented orientation
  in
  let read_lengths t =
    let first = read_length t in
    Cursor.ws t;
    if at_end t then Single first
    else
      let second = read_length t in
      Cursor.ws t;
      expect_value_end t;
      Pair (first, second)
  in
  Cursor.enum_or_calls "page-size"
    [ ("auto", Auto); ("inherit", Inherit) ]
    ~calls:[ ("var", read_var_ps) ]
    ~default:(Cursor.one_of [ read_named; read_oriented; read_lengths ])
    t

let rec read_scroll_snap_stop (t : Cursor.t) : scroll_snap_stop =
  Cursor.ws t;
  Cursor.enum_or_var "scroll-snap-stop"
    [
      ("normal", (Normal : scroll_snap_stop));
      ("always", Always);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_scroll_snap_stop t) : scroll_snap_stop))
    t

let rec read_scroll_snap_strictness t : scroll_snap_strictness =
  Cursor.enum_or_calls "scroll-snap-strictness"
    [
      ("proximity", (Proximity : scroll_snap_strictness));
      ("mandatory", Mandatory);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_scroll_snap_strictness t)) ]
    t

let rec read_scroll_snap_axis t : scroll_snap_axis =
  Cursor.enum_or_calls "scroll-snap axis"
    [
      ("none", (None : scroll_snap_axis));
      ("x", X);
      ("y", Y);
      ("block", Block);
      ("inline", Inline);
      ("both", Both);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_scroll_snap_axis t)) ]
    t

let rec read_scroll_snap_type t : scroll_snap_type =
  let read_axis_with_optional_strictness t =
    let axis = read_scroll_snap_axis t in
    Cursor.ws t;
    match axis with
    | None | Var _ ->
        (* "none" and vars don't take strictness *)
        Axis axis
    | _ -> (
        (* Try to read strictness *)
        match Cursor.option read_scroll_snap_strictness t with
        | Some strictness -> Axis_with_strictness (axis, strictness)
        | None -> Axis axis)
  in
  Cursor.enum_or_var "scroll-snap-type"
    [
      ("inherit", (Inherit : scroll_snap_type));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (read_var read_scroll_snap_type t) : scroll_snap_type))
    ~default:read_axis_with_optional_strictness t

let rec read_overscroll_behavior t : overscroll_behavior =
  Cursor.enum_or_var "overscroll-behavior"
    [
      ("auto", (Auto : overscroll_behavior));
      ("contain", Contain);
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_overscroll_behavior t))
    t

let read_svg_paint t : svg_paint =
  let read_url_with_fallback t =
    let u = Cursor.url t in
    (* Empty URLs are invalid in SVG paint context *)
    if u = "" then Cursor.err t "svg-paint url() must have a non-empty URL";
    Cursor.ws t;
    let fb =
      Cursor.option
        (fun t ->
          Cursor.enum "svg-paint-fallback"
            [ ("none", (None : svg_paint)); ("currentcolor", Current_color) ]
            ~default:(fun t -> (Color (read_color t) : svg_paint))
            t)
        t
    in
    Url (u, fb)
  in
  (* Bare [url(#grad)] is a single [Token.Url] component; handle before the
     function/ident dispatch. *)
  Cursor.one_of
    [
      read_url_with_fallback;
      (fun t ->
        Cursor.enum_or_calls "svg-paint"
          [
            ("none", (None : svg_paint));
            ("inherit", Inherit);
            ("currentcolor", Current_color);
            ("context-fill", Context_fill);
            ("context-stroke", Context_stroke);
          ]
          ~default:(fun t -> (Color (read_color t) : svg_paint))
          t);
    ]
    t

let rec read_direction t : direction =
  Cursor.enum_or_var "direction"
    [
      ("ltr", (Ltr : direction));
      ("rtl", Rtl);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_direction t))
    t

let rec read_unicode_bidi t : unicode_bidi =
  Cursor.enum_or_var "unicode-bidi"
    [
      ("normal", (Normal : unicode_bidi));
      ("embed", Embed);
      ("isolate", Isolate);
      ("bidi-override", Bidi_override);
      ("isolate-override", Isolate_override);
      ("plaintext", Plaintext);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_unicode_bidi t))
    t

let rec read_writing_mode t : writing_mode =
  Cursor.enum_or_var "writing-mode"
    [
      ("horizontal-tb", (Horizontal_tb : writing_mode));
      ("vertical-rl", Vertical_rl);
      ("vertical-lr", Vertical_lr);
      ("sideways-lr", Sideways_lr);
      ("sideways-rl", Sideways_rl);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_writing_mode t))
    t

let read_text_combine_digits t =
  let n = Cursor.int t in
  if n < 2 || n > 4 then
    Cursor.err_invalid t "text-combine-upright digits must be 2, 3, or 4";
  n

let rec read_text_combine_upright t : text_combine_upright =
  Cursor.enum_or_var "text-combine-upright"
    [
      ("none", (None : text_combine_upright));
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_combine_upright t))
    ~default:(fun t ->
      match Cursor.peek_ident t with
      | Some "digits" ->
          let _ = Cursor.ident t in
          Cursor.ws t;
          Digits (Cursor.option read_text_combine_digits t)
      | _ -> Cursor.err_expected t "text-combine-upright")
    t

let rec read_webkit_appearance t : webkit_appearance =
  Cursor.enum_or_var "webkit-appearance"
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
      ("-apple-pay-button", Apple_pay_button);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_webkit_appearance t) : webkit_appearance))
    t

let rec read_text_size_adjust t : text_size_adjust =
  Cursor.ws t;
  match Cursor.percentage_opt t with
  | Some n ->
      if n < 0.0 then
        Cursor.err t "text-size-adjust percentages cannot be negative"
      else Pct n
  | _ ->
      (* Keyword *)
      Cursor.enum_or_var "text-size-adjust"
        [
          ("none", (None : text_size_adjust));
          ("auto", Auto);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (Values.read_var read_text_size_adjust t))
        t

let rec read_webkit_font_smoothing t : webkit_font_smoothing =
  Cursor.enum_or_var "webkit-font-smoothing"
    [
      ("auto", (Auto : webkit_font_smoothing));
      ("none", None);
      ("antialiased", Antialiased);
      ("subpixel-antialiased", Subpixel_antialiased);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_font_smoothing t))
    t

let rec read_moz_osx_font_smoothing t : moz_osx_font_smoothing =
  Cursor.enum_or_var "moz-osx-font-smoothing"
    [
      ("auto", (Auto : moz_osx_font_smoothing));
      ("grayscale", Grayscale);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_moz_osx_font_smoothing t))
    t

let rec read_webkit_box_orient t : webkit_box_orient =
  Cursor.enum_or_var "webkit-box-orient"
    [
      ("horizontal", (Horizontal : webkit_box_orient));
      ("vertical", Vertical);
      ("inline-axis", Inline_axis);
      ("block-axis", Block_axis);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_box_orient t))
    t

let read_moz_orient t : moz_orient =
  Cursor.enum "moz-orient"
    [
      ("inline", (Inline : moz_orient));
      ("block", Block);
      ("horizontal", Horizontal);
      ("vertical", Vertical);
      ("inherit", Inherit);
    ]
    t

let rec read_webkit_line_clamp t : webkit_line_clamp =
  let read_var t : webkit_line_clamp =
    Var (read_var read_webkit_line_clamp t)
  in
  Cursor.enum_or_calls "-webkit-line-clamp"
    [
      ("none", (None : webkit_line_clamp));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let n = Cursor.int t in
      if n <= 0 then Cursor.err_invalid t "-webkit-line-clamp must be positive";
      Lines n)
    t

let rec read_forced_color_adjust t : forced_color_adjust =
  Cursor.enum_or_var "forced-color-adjust"
    [
      ("auto", (Auto : forced_color_adjust));
      ("none", None);
      ("preserve-parent-color", Preserve_parent_color);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_forced_color_adjust t) : forced_color_adjust))
    t

let rec read_appearance t : appearance =
  Cursor.enum_or_var "appearance"
    [
      ("none", (None : appearance));
      ("auto", Auto);
      ("button", Button);
      ("textfield", Textfield);
      ("menulist", Menulist);
      ("base-select", Base_select);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_appearance t) : appearance))
    t

let color_scheme_of_idents t names : color_scheme =
  match names with
  | [ "normal" ] -> Normal
  | [ "light" ] -> Light
  | [ "dark" ] -> Dark
  | [ "light"; "dark" ] | [ "dark"; "light" ] -> Light_dark
  | [ "only"; "light" ] | [ "light"; "only" ] -> Only_light
  | [ "only"; "dark" ] | [ "dark"; "only" ] -> Only_dark
  | [ "only"; "light"; "dark" ]
  | [ "only"; "dark"; "light" ]
  | [ "light"; "dark"; "only" ]
  | [ "dark"; "light"; "only" ] ->
      Only_light_dark
  | [ "inherit" ] -> Inherit
  | [ "initial" ] -> Initial
  | [ "unset" ] -> Unset
  | [ "revert" ] -> Revert
  | [ "revert-layer" ] -> Revert_layer
  | [] -> Cursor.err t "empty color-scheme"
  | _ ->
      (* CSS Color Adjust 1 section 2.1: [color-scheme] is [normal | [light |
         dark | <custom-ident>]+ && only?]. [normal] is mutually exclusive with
         the list form; [only] is a modifier that must accompany a non-empty
         list; CSS-wide keywords can only stand alone. *)
      let has_normal = List.mem "normal" names in
      let has_css_wide =
        List.exists
          (fun n ->
            List.mem (String.lowercase_ascii n)
              [ "inherit"; "initial"; "unset"; "revert"; "revert-layer" ])
          names
      in
      if has_normal then
        Cursor.err_invalid t
          "color-scheme: [normal] cannot be mixed with other keywords";
      if has_css_wide then
        Cursor.err_invalid t
          "color-scheme: CSS-wide keyword cannot be mixed with other keywords";
      let non_only_names =
        List.filter (fun n -> String.lowercase_ascii n <> "only") names
      in
      if non_only_names = [] then
        Cursor.err_invalid t
          "color-scheme: [only] must be combined with a color scheme";
      Custom names

let rec read_color_scheme t : color_scheme =
  let rec read_idents acc =
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_semicolon t then List.rev acc
    else read_idents (Cursor.ident t :: acc)
  in
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.equal (String.lowercase_ascii name) "var" ->
      Var (Values.read_var read_color_scheme t)
  | _ -> color_scheme_of_idents t (read_idents [])

let rec read_print_color_adjust t : print_color_adjust =
  Cursor.enum_or_var "print-color-adjust"
    [
      ("economy", (Economy : print_color_adjust));
      ("exact", Exact);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_print_color_adjust t))
    t

let rec read_box_decoration_break t : box_decoration_break =
  Cursor.enum_or_var "box-decoration-break"
    [
      ("clone", (Clone : box_decoration_break));
      ("slice", Slice);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_box_decoration_break t))
    t

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

let rec read_tab_size (t : Cursor.t) : tab_size =
  let read_value t =
    match Cursor.integer_opt t with
    | Some i ->
        if i < 0 then Cursor.err_invalid t "negative tab-size integer";
        (Int i : tab_size)
    | None ->
        Length (Values.read_length ~allow_negative:false ~with_keywords:false t)
  in
  Cursor.enum_or_var "tab-size"
    [
      ("initial", (Initial : tab_size));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_tab_size t))
    ~default:read_value t

let rec read_zoom (t : Cursor.t) : zoom =
  let read_value t =
    match Cursor.percentage_opt t with
    | Some p -> (Pct p : zoom)
    | None -> (
        match Cursor.number_opt t with
        | Some n -> Num n
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

let rec read_text_decoration_skip_ink t : text_decoration_skip_ink =
  Cursor.enum_or_var "text-decoration-skip-ink"
    [
      ("auto", (Auto : text_decoration_skip_ink));
      ("none", None);
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_text_decoration_skip_ink t))
    t

let rec read_vertical_align t : vertical_align =
  let read_var t : vertical_align = Var (read_var read_vertical_align t) in
  let read_calc t : vertical_align = Calc (read_calc read_vertical_align t) in
  Cursor.enum_or_calls "vertical-align"
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
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var); ("calc", read_calc) ]
    ~default:read_vertical_align_length t

let rec read_outline_style t : outline_style =
  let read_var t : outline_style = Var (read_var read_outline_style t) in
  Cursor.enum_or_calls "outline-style"
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
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    t

let outline_style_keywords =
  [
    "none";
    "solid";
    "dashed";
    "dotted";
    "double";
    "groove";
    "ridge";
    "inset";
    "outset";
    "auto";
  ]

let outline_starts_length t =
  Option.is_some
    (Cursor.lookahead
       (Cursor.option (fun t ->
            ignore (Cursor.number_with_unit t : float * string option)))
       t)

let outline_starts_style t =
  List.exists (fun kw -> Cursor.looking_at t kw) outline_style_keywords

let outline_at_end t =
  Cursor.is_done t || Cursor.peek_semicolon t || Cursor.peek_delim t = Some '!'

let read_outline_part ~width ~style ~color t =
  if Cursor.looking_at_func "var" t then
    (* A var() is type-ambiguous; assign it to the next unfilled
       width/style/color slot, reading its fallback with that slot's reader. *)
    if Option.is_none !width then
      width := Some (Var (read_var read_length t) : length)
    else if Option.is_none !style then
      style := Some (Var (read_var read_outline_style t) : outline_style)
    else if Option.is_none !color then color := Some (read_color t)
    else Cursor.err_expected t "outline"
  else if Option.is_none !style && outline_starts_style t then
    style := Some (read_outline_style t)
  else if Option.is_none !width && outline_starts_length t then
    width := Some (read_length t)
  else if Option.is_none !color then color := Some (read_color t)
  else Cursor.err_expected t "outline"

let read_outline_parts ~width ~style ~color t =
  let rec loop () =
    Cursor.ws t;
    if not (outline_at_end t) then (
      read_outline_part ~width ~style ~color t;
      loop ())
  in
  loop ()

let read_outline_shorthand_value t : outline =
  let width = ref Option.None in
  let style = ref Option.None in
  let color = ref Option.None in
  read_outline_parts ~width ~style ~color t;
  match (!width, !style, !color) with
  | Option.None, Some (None : outline_style), Option.None -> None
  | _ -> Shorthand { width = !width; style = !style; color = !color }

let rec read_outline t : outline =
  Cursor.enum_or_var "outline"
    [
      ("inherit", (Inherit : outline));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (* A lone var() is the whole value; a var() followed by more components is
         a shorthand whose first slot happens to be a var. *)
      let snap = Cursor.save t in
      let v = Values.read_var read_outline t in
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_comma t then (Var v : outline)
      else (
        Cursor.restore t snap;
        read_outline_shorthand_value t))
    ~default:read_outline_shorthand_value t

let read_outline_shorthand t : outline_shorthand =
  match read_outline t with
  | Shorthand s -> s
  | Inherit | Initial | Unset | Revert | Revert_layer | None ->
      { width = Option.None; style = Option.None; color = Option.None }
  | Var _ -> Cursor.err_invalid t "outline var shorthand"

let font_family_generic_css =
  [
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
  ]

let font_family_css_keywords : (string * font_family) list =
  [
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let font_family_popular_web =
  [
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
  ]

let font_family_platform =
  [
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
  ]

let font_family_developer =
  [
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

let font_family_all_enums : (string * font_family) list =
  font_family_generic_css @ font_family_css_keywords @ font_family_popular_web
  @ font_family_platform @ font_family_developer

let font_family_lookup_key name =
  name |> String.lowercase_ascii |> String.map (function ' ' -> '-' | c -> c)

(* The unquoted single-word lookup matches generic family keywords; a quoted
   name is always a [<custom-ident>] by spec, so it preserves the user's intent
   even when its text matches a generic keyword ([font-family: "serif"] is a
   custom family named "serif", not the [serif] generic). Multi-word quoted
   names still match the platform-name table since those entries are not
   keywords. *)
let font_family_of_quoted_name name =
  match List.assoc_opt (font_family_lookup_key name) font_family_all_enums with
  | Some family when String.contains name ' ' -> family
  | _ -> Name name

let rec read_font_family_single t : font_family =
  let read_var t : font_family = Var (read_var read_font_family t) in
  (* CSS Fonts 4 sec. 2.1 / CSS Cascade 5 sec. 7.3: the CSS-wide keywords and
     the reserved [default] are excluded from [<custom-ident>], so none may
     appear as any word of an unquoted family name. *)
  let is_reserved_word word =
    List.mem
      (String.lowercase_ascii word)
      [ "inherit"; "initial"; "unset"; "revert"; "revert-layer"; "default" ]
  in
  (* Read unquoted multi-word font names, e.g., "arial rounded" *)
  let rec read_unquoted_name_words acc =
    let word = Cursor.ident ~keep_case:true t in
    if is_reserved_word word then
      Cursor.err_invalid t
        "font-family: reserved word cannot appear in an unquoted family name";
    let acc = word :: acc in
    Cursor.ws t;
    if Option.is_some (Cursor.peek_ident t) then read_unquoted_name_words acc
    else String.concat " " (List.rev acc)
  in
  let read_single_word t : font_family =
    (* For single-word names, try enum match first *)
    (Cursor.enum_or_calls "font-family" font_family_all_enums
       ~calls:[ ("var", read_var) ]
       ~default:(fun t ->
         let name = Cursor.ident ~keep_case:true t in
         (* CSS Fonts 4 sec. 2.1: [default] is reserved and is not a valid
            unquoted [<custom-ident>] family name; it must be quoted. *)
         if String.lowercase_ascii name = "default" then
           Cursor.err_invalid t
             "font-family: 'default' is reserved and must be quoted"
         else (Name name : font_family))
       t
      : font_family)
  in
  Cursor.ws t;
  match Cursor.string_opt t with
  | Some name -> font_family_of_quoted_name name
  | None when Cursor.looking_at_func "var" t -> read_var t
  | None when Option.is_some (Cursor.peek_ident t) ->
      (* Peek ahead to see if this is multi-word or single-word *)
      let is_multi_word =
        Cursor.lookahead
          (fun t ->
            let _ = Cursor.ident t in
            Cursor.ws t;
            Option.is_some (Cursor.peek_ident t))
          t
      in
      if is_multi_word then
        (* Multi-word unquoted name; [read_unquoted_name_words] rejects any
           reserved word in the sequence. *)
        Name (read_unquoted_name_words [])
      else
        (* Single word - try enum match *)
        read_single_word t
  | None -> Cursor.err t "expected font-family value"

and read_font_family t : font_family =
  (* CSS Cascade 5 §7.3: a CSS-wide keyword ([inherit] / [initial] / [unset] /
     [revert] / [revert-layer]) must stand alone; mixed inside a
     [<custom-ident>#] list it makes the whole declaration invalid. *)
  let rec loop acc =
    Cursor.ws t;
    if Cursor.comma_opt t then (
      Cursor.ws t;
      loop (read_font_family_single t :: acc))
    else List.rev acc
  in
  let first = read_font_family_single t in
  let items = loop [ first ] in
  let is_css_wide = function
    | (Inherit : font_family) | Initial | Unset | Revert | Revert_layer -> true
    | _ -> false
  in
  match items with
  | [ x ] -> x
  | _ when List.exists is_css_wide items ->
      (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword must be the sole value; in a
         [<family-name>#] list it makes the whole declaration invalid. *)
      Cursor.err_invalid t
        "font-family: a CSS-wide keyword cannot appear in a family list"
  | l -> List l

let read_shorthand_line_height_typed r : line_height =
  let before = Cursor.save r in
  match read_line_height r with
  | lh -> lh
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      Cursor.err_invalid r "invalid line-height in font shorthand"

let generic_font_family_keywords =
  [
    "sans-serif";
    "serif";
    "monospace";
    "cursive";
    "fantasy";
    "system-ui";
    "ui-sans-serif";
    "ui-serif";
    "ui-monospace";
    "ui-rounded";
    "emoji";
    "math";
    "fangsong";
  ]

(* A bare ident matching a generic family ([sans-serif], [ui-monospace], ...) is
   only valid inside a font-family list, so its presence proves the whole token
   stream is a font-family value (the same "the type is obviously correct"
   reasoning that folds a colour function in an opaque stream). *)
let components_have_generic_family components =
  List.exists
    (function
      | Component.Preserved { kind = Token.Ident name; _ } ->
          List.mem (String.lowercase_ascii name) generic_font_family_keywords
      | _ -> false)
    components

let long_generic_family_start r =
  let is_ws = function
    | Component.Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let is_comma = function
    | Component.Preserved { kind = Token.Comma; _ } -> true
    | _ -> false
  in
  let rec drop_while p = function
    | x :: rest when p x -> drop_while p rest
    | l -> l
  in
  let item =
    Cursor.remaining r |> drop_while is_ws |> List.to_seq
    |> Seq.take_while (fun cv -> not (is_comma cv))
    |> List.of_seq
    |> List.filter (fun cv -> not (is_ws cv))
  in
  match item with
  | Component.Preserved { kind = Token.Ident first; _ } :: _ :: _ ->
      List.mem (String.lowercase_ascii first) generic_font_family_keywords
  | _ -> false

let font_shorthand_prefix_ident = function
  | Some
      ( "italic" | "oblique" | "normal" | "small-caps" | "bold" | "bolder"
      | "lighter" | "condensed" | "expanded" ) ->
      true
  | _ -> false

(* Keyword -> which prefix slot it fills in the [font] shorthand. [Normal] is
   the absence keyword (style / variant / weight / stretch each have their own
   [normal]); we accept it and move on. *)
type font_prefix_slot =
  | Style of font_style
  | Variant of font_variant_css21
  | Weight of font_weight
  | Stretch of font_stretch
  | No_op

let font_prefix_slot_of = function
  | "italic" -> Style Italic
  | "oblique" -> Style Oblique
  | "small-caps" -> Variant Small_caps
  | "bold" -> Weight Bold
  | "bolder" -> Weight Bolder
  | "lighter" -> Weight Lighter
  | "condensed" -> Stretch Condensed
  | "expanded" -> Stretch Expanded
  | "normal" | _ -> No_op

let assign_font_prefix_slot ~(style : font_style option ref)
    ~(variant : font_variant_css21 option ref)
    ~(weight : font_weight option ref) ~(stretch : font_stretch option ref) =
  function
  | Style s -> if !style = None then style := Some s
  | Variant v -> if !variant = None then variant := Some v
  | Weight w -> if !weight = None then weight := Some w
  | Stretch st -> if !stretch = None then stretch := Some st
  | No_op -> ()

let try_numeric_font_weight r (weight : font_weight option ref) =
  let before = Cursor.save r in
  match read_font_weight r with
  | w when !weight = None ->
      weight := Some w;
      true
  | _ ->
      Cursor.restore r before;
      false
  | exception Cursor.Parse_error _ ->
      Cursor.restore r before;
      false

let read_optional_line_height r =
  Cursor.ws r;
  match Cursor.peek_delim r with
  | Some '/' ->
      Cursor.skip r;
      Cursor.ws r;
      Some (read_shorthand_line_height_typed r)
  | _ -> None

(* Parse the [font] shorthand body: a keyword prefix loop fills style / variant
   / weight / stretch (a [normal] binds no slot), then the required [size
   [/<line-height>]? <family>+] tail. *)
let read_font_shorthand r : font_shorthand =
  let style : font_style option ref = ref Option.None in
  let variant : font_variant_css21 option ref = ref Option.None in
  let weight : font_weight option ref = ref Option.None in
  let stretch : font_stretch option ref = ref Option.None in
  let assign = assign_font_prefix_slot ~style ~variant ~weight ~stretch in
  let rec consume_prefix () =
    Cursor.ws r;
    if Cursor.is_done r then ()
    else if font_shorthand_prefix_ident (Cursor.peek_ident r) then (
      assign (font_prefix_slot_of (Cursor.ident r));
      consume_prefix ())
    else if try_numeric_font_weight r weight then consume_prefix ()
  in
  consume_prefix ();
  Cursor.ws r;
  let size = read_font_size r in
  let line_height = read_optional_line_height r in
  if long_generic_family_start r then
    Cursor.err_invalid r "generic font family must be a standalone family item";
  let family = read_font_family r in
  Cursor.ws r;
  Cursor.expect_eof r;
  {
    style = !style;
    variant = !variant;
    weight = !weight;
    stretch = !stretch;
    size;
    line_height;
    family;
  }

let rec read_font t : font =
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  let lower = String.lowercase_ascii (String.trim raw) in
  match lower with
  | "inherit" -> Inherit
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | "caption" -> Caption
  | "icon" -> Icon
  | "menu" -> Menu
  | "message-box" -> Message_box
  | "small-caption" -> Small_caption
  | "status-bar" -> Status_bar
  | _ ->
      let is_valid_var () =
        let r = Cursor.of_string raw in
        match Values.read_var (fun r -> read_font r) r with
        | (_ : font var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_valid_var () then
        let r = Cursor.of_string raw in
        Var (Values.read_var (fun r -> read_font r) r)
      else if value_has_css_wide_mix raw then
        Cursor.err_invalid t "CSS-wide keyword mixed with other values"
      else
        let body =
          try read_font_shorthand (Cursor.of_string raw)
          with Cursor.Parse_error _ ->
            Cursor.err_invalid t "invalid font shorthand"
        in
        Shorthand body

let rec read_font_stretch t : font_stretch =
  let read_percentage t : font_stretch =
    let n = Cursor.pct t in
    (* CSS Fonts 4 §6.1.2: font-stretch percentage is non-negative. *)
    if n < 0. then err_invalid_value t "font-stretch" (string_of_float n);
    Pct n
  in
  Cursor.enum_or_var "font-stretch"
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
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_stretch t))
    ~default:read_percentage t

let read_font_display t : font_display =
  Cursor.enum "font-display"
    [
      ("auto", (Auto : font_display));
      ("block", Block);
      ("swap", Swap);
      ("fallback", Fallback);
      ("optional", Optional);
    ]
    t

let read_unicode_single start_value width =
  if width > String.length (hex_string start_value) then
    (Padded_single (start_value, width) : unicode_range)
  else Single start_value

let read_unicode_range_pair start_value end_value start_width end_width =
  if
    start_width > String.length (hex_string start_value)
    || end_width > String.length (hex_string end_value)
  then
    Padded_range
      { start = start_value; end_ = end_value; start_width; end_width }
  else Range (start_value, end_value)

let read_unicode_wildcard start_value prefix_width wildcards =
  let prefix = start_value lsr (4 * wildcards) in
  Wildcard { prefix; prefix_width; wildcards }

let read_unicode_token_form start_value end_value = function
  | Token.Single { width } -> read_unicode_single start_value width
  | Token.Range { start_width; end_width } ->
      read_unicode_range_pair start_value end_value start_width end_width
  | Token.Wildcard { prefix_width; wildcards } ->
      read_unicode_wildcard start_value prefix_width wildcards

let read_unicode_token t start_value end_value form =
  Cursor.skip t;
  if start_value > end_value then
    Cursor.err_invalid t "unicode range: start > end";
  if end_value > 0x10FFFF then
    Cursor.err_invalid t "unicode range: code point out of range";
  read_unicode_token_form start_value end_value form

let rec read_unicode_range t : unicode_range =
  (* The lexer emits a single [Unicode_range] token for [U+...] forms (CSS
     Syntax section 4.3.14); we just translate it to the [unicode_range] ADT. *)
  Cursor.with_context t "unicode-range" @@ fun () ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_unicode_range t) : unicode_range)
  | Some
      (Component.Preserved
         { kind = Token.Unicode_range { start_value; end_value; form }; _ }) ->
      read_unicode_token t start_value end_value form
  | _ -> Cursor.err_expected t "unicode-range"

let rec read_font_variant_numeric_token t : font_variant_numeric_token =
  let read_var t : font_variant_numeric_token =
    Var (read_var read_font_variant_numeric_token t)
  in
  Cursor.enum_or_var "font-variant-numeric-token"
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
    ~var:read_var t

let font_variant_numeric_token_family : font_variant_numeric_token -> _ =
  function
  | Lining_nums | Oldstyle_nums -> `Numeric_figure
  | Proportional_nums | Tabular_nums -> `Numeric_spacing
  | Diagonal_fractions | Stacked_fractions -> `Numeric_fraction
  | Ordinal -> `Ordinal
  | Slashed_zero -> `Slashed_zero
  | Normal | Var _ -> `Other

let numeric_token_is_normal : font_variant_numeric_token -> bool = function
  | Normal -> true
  | _ -> false

let reject_duplicate_numeric_families t
    (tokens : font_variant_numeric_token list) =
  let seen = Hashtbl.create 5 in
  List.iter
    (fun token ->
      match font_variant_numeric_token_family token with
      | `Other -> ()
      | family ->
          if Hashtbl.mem seen family then
            err_invalid_value t "font-variant-numeric" "duplicate token";
          Hashtbl.add seen family ())
    tokens

let read_font_variant_numeric_tokens t : font_variant_numeric =
  let tokens, _ = Cursor.many read_font_variant_numeric_token t in
  match tokens with
  | [] -> err_invalid_value t "font-variant-numeric" "<empty>"
  | tokens ->
      (* CSS Fonts 4 section 6.6: [normal] resets all sub-properties and must
         stand alone; it can't be mixed with other numeric tokens. *)
      if List.exists numeric_token_is_normal tokens && List.length tokens > 1
      then
        err_invalid_value t "font-variant-numeric"
          "[normal] cannot be mixed with other tokens";
      reject_duplicate_numeric_families t tokens;
      Tokens tokens

let rec read_font_variant_numeric t : font_variant_numeric =
  Cursor.enum_or_var "font-variant-numeric"
    [
      ("normal", (Normal : font_variant_numeric));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_font_variant_numeric t))
    ~default:read_font_variant_numeric_tokens t

let rec read_font_feature_settings t : font_feature_settings =
  let read_var t : font_feature_settings =
    Var (read_var read_font_feature_settings t)
  in
  let read_feature t =
    let tag_content = Cursor.string t in
    if String.length tag_content <> 4 then
      Cursor.err t "font-feature-settings tag must be exactly 4 characters";
    let tag = "\"" ^ tag_content ^ "\"" in
    Cursor.ws t;
    match Cursor.option Cursor.number t with
    | Some n ->
        if n <> 0.0 && n <> 1.0 then
          Cursor.err t "font-feature-settings value must be 0 or 1";
        tag ^ " " ^ string_of_int (int_of_float n)
    | None -> (
        match Cursor.option Cursor.ident t with
        | Some "on" -> tag ^ " on"
        | Some "off" -> tag ^ " off"
        | Some _ -> Cursor.err t "font-feature-settings value must be on/off"
        | None -> tag)
  in
  let read_feature_list t =
    let items = Cursor.list ~sep:Cursor.comma ~at_least:1 read_feature t in
    Feature_list (String.concat ", " items)
  in
  Cursor.enum_or_calls "font-feature-settings"
    [
      ("normal", (Normal : font_feature_settings));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_feature_list t

let rec read_font_variation_settings t : font_variation_settings =
  let read_var t : font_variation_settings =
    Var (read_var read_font_variation_settings t)
  in
  let read_axis t =
    let tag_content = Cursor.string t in
    if String.length tag_content <> 4 then
      Cursor.err t
        "font-variation-settings axis tag must be exactly 4 characters";
    String.iter
      (fun c ->
        let code = Char.code c in
        if code < 0x20 || code > 0x7E then
          Cursor.err t
            "font-variation-settings axis tag must contain only ASCII \
             characters (U+20 - U+7E)")
      tag_content;
    let tag = "\"" ^ tag_content ^ "\"" in
    Cursor.ws t;
    let value = Cursor.number t in
    tag ^ " " ^ string_of_int (int_of_float value)
  in
  let read_axis_list t =
    let items = Cursor.list ~sep:Cursor.comma ~at_least:1 read_axis t in
    Axis_list (String.concat ", " items)
  in
  Cursor.enum_or_calls "font-variation-settings"
    [
      ("normal", (Normal : font_variation_settings));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_axis_list t

let rec read_transform_style t : transform_style =
  Cursor.enum_or_var "transform-style"
    [
      ("flat", (Flat : transform_style));
      ("preserve-3d", Preserve_3d);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_transform_style t))
    t

let rec read_backface_visibility t : backface_visibility =
  Cursor.enum_or_var "backface-visibility"
    [
      ("visible", (Visible : backface_visibility));
      ("hidden", Hidden);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_backface_visibility t))
    t

let rec read_scale t : scale =
  (* CSS Transforms 2 §3.6: [<number-percentage>{1,3}]. Two [var()] shapes to
     keep distinct: [scale: var(--s)] is a whole-value var (produce [Var _]);
     [scale: var(--x) var(--y)] is per-slot (produce [XY _]). They are
     indistinguishable until a second component appears, so peel the first
     [var()] as a whole-value [Var]; if another follows, the saved snapshot
     re-reads it as a per-slot number-percentage. *)
  let read_numbers_from t (x : number_percentage) : scale =
    match Cursor.option Values.read_number_percentage t with
    | None -> X x
    | Some y -> (
        match Cursor.option Values.read_number_percentage t with
        | None -> XY (x, y)
        | Some z -> XYZ (x, y, z))
  in
  let read_numbers t : scale =
    let x = Values.read_number_percentage t in
    read_numbers_from t x
  in
  let read_var_or_components t : scale =
    let snap = Cursor.save t in
    let whole_var : scale = Var (read_var read_scale t) in
    Cursor.ws t;
    if Cursor.is_done t then whole_var
    else
      (* Another value follows the [var()]: this was per-slot syntax, not a
         whole-value var. Rewind and re-read the first [var()] as a number-
         percentage so it ends up inside [XY] / [XYZ]. *)
      let () = Cursor.restore t snap in
      let x = Values.read_number_percentage t in
      read_numbers_from t x
  in
  Cursor.enum_or_calls "scale"
    [
      ("none", (None : scale));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var_or_components) ]
    ~default:read_numbers t

let read_steps_direction t : steps_direction =
  Cursor.enum "steps direction"
    [
      ("jump-start", Jump_start);
      ("jump-end", Jump_end);
      ("jump-none", Jump_none);
      ("jump-both", Jump_both);
      ("start", Start);
      ("end", End);
    ]
    t

module Timing_function = struct
  let css_wide =
    [
      ("inherit", (Inherit : timing_function));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]

  let read_linear_function t : timing_function =
    Cursor.call "linear" t (fun t ->
        let body = Cursor.consume_remaining_as_string ~trim:true t in
        if body = "" then Cursor.err t "linear() requires at least one stop";
        Linear_function body)

  let read_steps t : timing_function =
    Cursor.call "steps" t (fun t ->
        let n = int_of_float (Cursor.number t) in
        if n <= 0 then Cursor.err t "steps() requires a positive step count";
        let kind =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_steps_direction t)
            t
        in
        Steps (n, kind))

  let read_cubic_bezier t : timing_function =
    Cursor.call "cubic-bezier" t (fun t ->
        let a = Cursor.number t in
        Cursor.comma t;
        Cursor.ws t;
        let b = Cursor.number t in
        Cursor.comma t;
        Cursor.ws t;
        let c = Cursor.number t in
        Cursor.comma t;
        Cursor.ws t;
        let d = Cursor.number t in
        Cubic_bezier (a, b, c, d))

  let rec read t : timing_function =
    let read_var_timing t : timing_function = Var (Values.read_var read t) in
    Cursor.enum_or_calls "timing-function"
      ([
         ("ease", (Ease : timing_function));
         ("linear", Linear);
         ("ease-in", Ease_in);
         ("ease-out", Ease_out);
         ("ease-in-out", Ease_in_out);
         ("step-start", Step_start);
         ("step-end", Step_end);
       ]
      @ css_wide)
      ~calls:
        [
          ("linear", read_linear_function);
          ("steps", read_steps);
          ("cubic-bezier", read_cubic_bezier);
          ("var", read_var_timing);
        ]
      t
end

let read_timing_function t : timing_function = Timing_function.read t

let read_timing_function_list t =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_timing_function t with
  | [ value ] -> value
  | values -> Timing_functions values

let read_duration_list (read_one : Cursor.t -> Values.duration) t :
    Values.duration =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_one t with
  | [ value ] -> value
  | values -> Durations values

let rec read_transition_property_value t : transition_property_value =
  let read_var t : transition_property_value =
    Var (read_var read_transition_property_value t)
  in
  let read_property_ident t =
    let name = Cursor.ident ~keep_case:true t in
    (* CSS Transitions 1 section 2.1: [transition-property] is a
       [<custom-ident>], so it excludes keywords reserved for other slots of the
       [transition] shorthand. [normal] and [allow-discrete] are the
       [transition-behavior] enum and would silently absorb a duplicate (e.g.
       [transition: normal normal]) into the property slot. *)
    if List.mem (String.lowercase_ascii name) [ "normal"; "allow-discrete" ]
    then
      Cursor.err_invalid t
        ("transition-property cannot be reserved keyword: " ^ name)
    else Property name
  in
  Cursor.enum_or_calls "transition-property-value"
    [
      ("all", (All : transition_property_value));
      ("none", (None : transition_property_value));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:read_property_ident t

let read_transition_property t : transition_property =
  (* Parse comma-separated list of transition properties *)
  let rec loop acc =
    let v = read_transition_property_value t in
    Cursor.ws t;
    if Cursor.comma_opt t then loop (v :: acc) else List.rev (v :: acc)
  in
  let values = loop [] in
  let singleton_only : transition_property_value -> bool = function
    | All | None | Initial | Inherit | Unset | Revert | Revert_layer -> true
    | Property _ | Var _ -> false
  in
  if List.length values > 1 && List.exists singleton_only values then
    Cursor.err_invalid t "transition-property singleton value in list";
  values

let rec read_transition_behavior t : transition_behavior =
  Cursor.enum_or_var "transition-behavior"
    [
      ("normal", (Normal : transition_behavior));
      ("allow-discrete", (Allow_discrete : transition_behavior));
      ("inherit", (Inherit : transition_behavior));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_transition_behavior t))
    t

let rec read_overlay t : overlay =
  Cursor.enum_or_var "overlay"
    [
      ("auto", (Auto : overlay));
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_overlay t) : overlay))
    t

type transition_parts = {
  mutable property : transition_property_value option;
  mutable times : duration list;
  mutable timing : timing_function option;
  mutable behavior : transition_behavior option;
}

let read_transition_part t read set =
  let snap = Cursor.save t in
  try
    set (read t);
    true
  with Cursor.Parse_error _ ->
    Cursor.restore t snap;
    false

let transition_property_start t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident _; _ })
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      true
  | _ -> false

let read_transition_property_part parts t =
  if Option.is_some parts.property || not (transition_property_start t) then
    false
  else
    read_transition_part t read_transition_property_value (fun v ->
        parts.property <- Option.Some v)

let read_transition_var_property_part parts t =
  if Option.is_some parts.property || not (Cursor.looking_at_func "var" t) then
    false
  else read_transition_property_part parts t

let read_transition_timing_part parts t =
  if Option.is_some parts.timing then false
  else
    read_transition_part t read_timing_function (fun v ->
        parts.timing <- Option.Some v)

let read_transition_behavior_part parts t =
  if Option.is_some parts.behavior then false
  else
    read_transition_part t read_transition_behavior (fun v ->
        parts.behavior <- Option.Some v)

let read_transition_time_part parts t =
  if List.length parts.times >= 2 then false
  else
    read_transition_part t read_duration (fun v ->
        parts.times <- v :: parts.times)

let transition_duration_delay parts =
  match List.rev parts.times with
  | [] -> (Option.None, Option.None)
  | [ d ] -> (Option.Some d, Option.None)
  | d :: l :: _ -> (Option.Some d, Option.Some l)

let read_transition_next_part parts t =
  read_transition_var_property_part parts t
  || read_transition_time_part parts t
  || read_transition_timing_part parts t
  || read_transition_behavior_part parts t
  || read_transition_property_part parts t

let transition_property_or_all = function
  | Option.Some property -> property
  | Option.None -> (All : transition_property_value)

let read_transition_shorthand t : transition_shorthand =
  let parts =
    {
      property = Option.None;
      times = [];
      timing = Option.None;
      behavior = Option.None;
    }
  in
  let consumed = ref true in
  while !consumed do
    Cursor.ws t;
    consumed := read_transition_next_part parts t
  done;
  let property = transition_property_or_all parts.property in
  let duration, delay = transition_duration_delay parts in
  {
    property;
    duration;
    timing_function = parts.timing;
    delay;
    behavior = parts.behavior;
  }

let rec read_transition t : transition =
  Cursor.enum "transition"
    [
      ("inherit", (Inherit : transition));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("none", None);
    ]
    ~default:(fun t : transition ->
      if Cursor.looking_at_func "var" t then (
        let snap = Cursor.save t in
        let value = (Var (read_var read_transition t) : transition) in
        Cursor.ws t;
        if Cursor.is_done t then value
        else (
          Cursor.restore t snap;
          Shorthand (read_transition_shorthand t)))
      else Shorthand (read_transition_shorthand t))
    t

let read_transitions t : transition list =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_transition t

let rec read_animation_direction t : animation_direction =
  let read_one t =
    Cursor.enum "animation-direction-item"
      [
        ("normal", (Normal : animation_direction));
        ("reverse", Reverse);
        ("alternate", Alternate);
        ("alternate-reverse", Alternate_reverse);
      ]
      t
  in
  Cursor.enum_or_var "animation-direction"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_direction t))
    ~default:(fun t ->
      match Cursor.list ~sep:Cursor.comma ~at_least:1 read_one t with
      | [ value ] -> value
      | values -> Directions values)
    t

let rec read_animation_fill_mode t : animation_fill_mode =
  let read_one t =
    Cursor.enum "animation-fill-mode-item"
      [
        ("none", (None : animation_fill_mode));
        ("forwards", Forwards);
        ("backwards", Backwards);
        ("both", Both);
      ]
      t
  in
  Cursor.enum_or_var "animation-fill-mode"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_fill_mode t))
    ~default:(fun t ->
      match Cursor.list ~sep:Cursor.comma ~at_least:1 read_one t with
      | [ value ] -> value
      | values -> Fill_modes values)
    t

let read_animation_count_number t =
  let n, unit = Cursor.number_with_unit t in
  match unit with
  | Some u ->
      Cursor.err_invalid t
        ("animation-iteration-count must be unitless, got: " ^ u)
  | None ->
      if n < 0. then
        Cursor.err_invalid t "animation-iteration-count cannot be negative";
      Num n

let read_animation_count_item t =
  Cursor.enum "animation-iteration-count-item"
    [ ("infinite", (Infinite : animation_iteration_count)) ]
    ~default:read_animation_count_number t

let read_animation_counts t =
  match
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_animation_count_item t
  with
  | [ count ] -> count
  | counts -> Counts counts

let rec read_animation_iteration_count t : animation_iteration_count =
  Cursor.enum_or_var "animation-iteration-count"
    [
      ("initial", (Initial : animation_iteration_count));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_iteration_count t))
    ~default:read_animation_counts t

type animation_reserved_string_name =
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Default

let animation_reserved_string_name = function
  | "none" -> Some (None : animation_reserved_string_name)
  | "initial" -> Some Initial
  | "inherit" -> Some Inherit
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | "default" -> Some Default
  | _ -> None

type animation_shorthand_kind =
  | Timing
  | Iteration
  | Direction
  | Fill
  | Play
  | Timeline

let animation_shorthand_kind = function
  | "ease" | "linear" | "ease-in" | "ease-out" | "ease-in-out" | "step-start"
  | "step-end" ->
      Some Timing
  | "infinite" -> Some Iteration
  | "normal" | "reverse" | "alternate" | "alternate-reverse" -> Some Direction
  | "none" | "forwards" | "backwards" | "both" -> Some Fill
  | "running" | "paused" -> Some Play
  | "auto" -> Some Timeline
  | _ -> None

let animation_quoted_or_name s =
  match animation_reserved_string_name (String.lowercase_ascii s) with
  | Some _ -> (Quoted s : animation_name)
  | None -> (
      match animation_shorthand_kind (String.lowercase_ascii s) with
      | Some _ -> Ambiguous s
      | None -> Name s)

let rec read_animation_name t : animation_name =
  let read_item t =
    Cursor.enum "animation-name-item"
      [ ("none", (None : animation_name)) ]
      ~default:(fun t ->
        match Cursor.string_opt t with
        | Some s -> (animation_quoted_or_name s : animation_name)
        | None -> Name (Cursor.ident t))
      t
  in
  Cursor.enum_or_var "animation-name"
    [
      ("initial", (Initial : animation_name));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_name t))
    ~default:(fun t ->
      let names = Cursor.list ~sep:Cursor.comma ~at_least:1 read_item t in
      match names with [ name ] -> name | names -> Names names)
    t

let rec read_animation_play_state t : animation_play_state =
  let read_state t =
    Cursor.enum "animation-play-state-item"
      [ ("running", (Running : animation_play_state)); ("paused", Paused) ]
      t
  in
  Cursor.enum_or_var "animation-play-state"
    [
      ("initial", (Initial : animation_play_state));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_play_state t))
    ~default:(fun t ->
      let states = Cursor.list ~sep:Cursor.comma ~at_least:1 read_state t in
      match states with [ state ] -> state | states -> States states)
    t

let read_animation_composition_item t =
  Cursor.enum "animation-composition-item"
    [ ("replace", Replace); ("add", Add); ("accumulate", Accumulate) ]
    t

let rec read_animation_composition t : animation_composition =
  Cursor.enum_or_var "animation-composition"
    [
      ("initial", (Initial : animation_composition));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_animation_composition t))
    ~default:(fun t ->
      Compositions
        (Cursor.list ~sep:Cursor.comma ~at_least:1
           read_animation_composition_item t))
    t

module Animation = struct
  type component =
    | Name of animation_name option
    | Duration of duration
    | Timing_function of timing_function
    | Iteration_count of animation_iteration_count
    | Direction of animation_direction
    | Fill_mode of animation_fill_mode
    | Play_state of animation_play_state
    | Timeline of animation_timeline

  let timing_name : timing_function -> string option = function
    | Ease -> Some "ease"
    | Linear -> Some "linear"
    | Ease_in -> Some "ease-in"
    | Ease_out -> Some "ease-out"
    | Ease_in_out -> Some "ease-in-out"
    | Step_start -> Some "step-start"
    | Step_end -> Some "step-end"
    | _ -> None

  let direction_name : animation_direction -> string option = function
    | Normal -> Some "normal"
    | Reverse -> Some "reverse"
    | Alternate -> Some "alternate"
    | Alternate_reverse -> Some "alternate-reverse"
    | _ -> None

  let fill_name : animation_fill_mode -> animation_name option = function
    | None -> Some (None : animation_name)
    | Forwards -> Some (Ambiguous "forwards")
    | Backwards -> Some (Ambiguous "backwards")
    | Both -> Some (Ambiguous "both")
    | _ -> None

  let play_name : animation_play_state -> string option = function
    | Running -> Some "running"
    | Paused -> Some "paused"
    | _ -> None

  let name : animation_timeline -> string option = function
    | Auto -> Some "auto"
    | _ -> None

  let set_name name_seen (acc : animation_shorthand) (name : animation_name) =
    name_seen := true;
    { acc with name = Some name }

  let set_string_name t label name_seen acc = function
    | Some name when not !name_seen -> set_name name_seen acc (Ambiguous name)
    | _ -> Cursor.err t ("duplicate " ^ label)

  let set_animation_name t label name_seen acc = function
    | Some name when not !name_seen -> set_name name_seen acc name
    | _ -> Cursor.err t ("duplicate " ^ label)

  type read_state = {
    duration_count : int ref;
    name_seen : bool ref;
    timing_seen : bool ref;
    iteration_seen : bool ref;
    direction_seen : bool ref;
    fill_seen : bool ref;
    play_seen : bool ref;
    timeline_seen : bool ref;
    component_seen : bool ref;
  }

  let read_state () =
    {
      duration_count = ref 0;
      name_seen = ref false;
      timing_seen = ref false;
      iteration_seen = ref false;
      direction_seen = ref false;
      fill_seen = ref false;
      play_seen = ref false;
      timeline_seen = ref false;
      component_seen = ref false;
    }

  let default_shorthand =
    {
      name = None;
      (* CSS default: none *)
      duration = Some (S 0.0);
      (* CSS default: 0s *)
      timing_function = Some Ease;
      (* CSS default: ease *)
      delay = Some (S 0.0);
      (* CSS default: 0s *)
      iteration_count = Some (Num 1.0);
      (* CSS default: 1 *)
      direction = Some Normal;
      (* CSS default: normal *)
      fill_mode = Some None;
      (* CSS default: none *)
      play_state = Some Running;
      (* CSS default: running *)
      timeline = Some Auto;
      (* CSS default: auto *)
    }

  let apply_name t state (acc : animation_shorthand)
      (name : animation_name option) =
    if !(state.name_seen) then Cursor.err t "duplicate animation-name";
    state.name_seen := true;
    { acc with name }

  let apply_duration t state acc d =
    (* CSS spec: First time value is duration, second is delay *)
    incr state.duration_count;
    if !(state.duration_count) > 2 then
      Cursor.err t "animation shorthand cannot have more than two time values";
    if !(state.duration_count) = 1 then { acc with duration = Some d }
    else { acc with delay = Some d }

  let apply_timing t state acc tf =
    if !(state.timing_seen) then
      set_string_name t "animation-timing-function" state.name_seen acc
        (timing_name tf)
    else (
      state.timing_seen := true;
      { acc with timing_function = Some tf })

  let apply_iteration t state acc ic =
    if !(state.iteration_seen) then
      (* CSS Animations 1 section 8.5: [<single-animation-iteration-count>] is
         [infinite | <number>]; [infinite] is a reserved keyword that cannot be
         a [<custom-ident>] animation name. Reject the duplicate rather than
         coercing it into the name slot. *)
      Cursor.err t "duplicate animation-iteration-count";
    state.iteration_seen := true;
    { acc with iteration_count = Some ic }

  let apply_direction t state acc dir =
    if !(state.direction_seen) then
      set_string_name t "animation-direction" state.name_seen acc
        (direction_name dir)
    else (
      state.direction_seen := true;
      { acc with direction = Some dir })

  let apply_fill t state acc fm =
    if !(state.fill_seen) then
      set_animation_name t "animation-fill-mode" state.name_seen acc
        (fill_name fm)
    else (
      state.fill_seen := true;
      { acc with fill_mode = Some fm })

  let apply_play t state acc ps =
    if !(state.play_seen) then
      set_string_name t "animation-play-state" state.name_seen acc
        (play_name ps)
    else (
      state.play_seen := true;
      { acc with play_state = Some ps })

  let apply_timeline t state acc tl =
    if !(state.timeline_seen) then
      set_string_name t "animation-timeline" state.name_seen acc (name tl)
    else (
      state.timeline_seen := true;
      { acc with timeline = Some tl })

  let apply_component t state (acc : animation_shorthand) component =
    state.component_seen := true;
    match component with
    | Name name -> apply_name t state acc name
    | Duration d -> apply_duration t state acc d
    | Timing_function tf -> apply_timing t state acc tf
    | Iteration_count ic -> apply_iteration t state acc ic
    | Direction dir -> apply_direction t state acc dir
    | Fill_mode fm -> apply_fill t state acc fm
    | Play_state ps -> apply_play t state acc ps
    | Timeline tl -> apply_timeline t state acc tl

  let read_component t =
    let read_duration t = Duration (read_duration t) in
    let read_timing t = Timing_function (read_timing_function t) in
    let read_iteration t = Iteration_count (read_animation_count_item t) in
    let read_direction t =
      match Option.map String.lowercase_ascii (Cursor.ident_opt t) with
      | Some "normal" -> Direction (Normal : animation_direction)
      | Some "reverse" -> Direction Reverse
      | Some "alternate" -> Direction Alternate
      | Some "alternate-reverse" -> Direction Alternate_reverse
      | Some s -> Cursor.err t ("unknown animation-direction-item: " ^ s)
      | None -> Cursor.err_expected t "animation-direction-item"
    in
    let read_fill t =
      match Option.map String.lowercase_ascii (Cursor.ident_opt t) with
      | Some "none" -> Fill_mode (None : animation_fill_mode)
      | Some "forwards" -> Fill_mode Forwards
      | Some "backwards" -> Fill_mode Backwards
      | Some "both" -> Fill_mode Both
      | Some s -> Cursor.err t ("unknown animation-fill-mode-item: " ^ s)
      | None -> Cursor.err_expected t "animation-fill-mode-item"
    in
    let read_play t =
      match Option.map String.lowercase_ascii (Cursor.ident_opt t) with
      | Some "running" -> Play_state (Running : animation_play_state)
      | Some "paused" -> Play_state Paused
      | Some s -> Cursor.err t ("unknown animation-play-state-item: " ^ s)
      | None -> Cursor.err_expected t "animation-play-state-item"
    in
    let read_timeline t = Timeline (read_animation_timeline t) in
    let read_var_name t =
      Name (Some (Var (Values.read_var read_animation_name t)))
    in
    let read_string_name t =
      match Cursor.string_opt t with
      | Some s -> Name (Some (animation_quoted_or_name s))
      | None -> Cursor.err t "expected animation-name string"
    in
    let read_name t =
      let v = Cursor.ident t in
      if Option.is_some (animation_shorthand_kind (String.lowercase_ascii v))
      then
        (* This identifier is for another property, not animation-name *)
        Cursor.err t
          ("'" ^ v ^ "' is a reserved keyword for animation properties")
      else Name (Some (Name v))
    in
    Cursor.one_of
      [
        read_duration;
        read_timing;
        read_iteration;
        read_direction;
        read_fill;
        read_play;
        read_timeline;
        read_var_name;
        read_string_name;
        (* Animation name - parse this LAST since it accepts any non-reserved
           identifier *)
        read_name;
      ]
      t

  let read_shorthand t =
    let state = read_state () in
    let acc, _ =
      Cursor.fold_many read_component ~init:default_shorthand
        ~f:(apply_component t state) t
    in
    (* CSS spec: All components are optional *)
    if not !(state.component_seen) then
      Cursor.err t "animation shorthand requires at least one component"
    else acc

  let is_zero_duration = function S 0. | Ms 0. -> true | _ -> false
  let pp_iter_count = pp_animation_iteration_count

  (* Check if a timing function prints with a trailing ')'. Some function values
     canonicalise to keyword aliases in minified output. *)
  let rec ends_with_paren ctx = function
    | Cubic_bezier (0.25, 0.1, 0.25, 1.0)
    | Cubic_bezier (0.42, 0.0, 1.0, 1.0)
    | Cubic_bezier (0.0, 0.0, 0.58, 1.0)
    | Cubic_bezier (0.42, 0.0, 0.58, 1.0)
      when Pp.minified ctx ->
        false
    | (Steps (1, Some (Jump_start | Start)) | Steps (1, Some (Jump_end | End)))
      when Pp.minified ctx ->
        (* These fold to the [step-start] / [step-end] keywords in
           [pp_timing_function], so the rendered output is keyword-shaped, no
           closing paren. *)
        false
    | Cubic_bezier _ | Steps _ | Linear_function _ | Var _ -> true
    | Timing_functions [] -> false
    | Timing_functions values -> ends_with_paren ctx (List.hd (List.rev values))
    | Inherit | Initial | Unset | Revert | Revert_layer -> false
    | Linear | Ease | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
      ->
        false

  let pp_timing = pp_timing_function

  let is_duration : duration option -> bool = function
    | Some d when not (is_zero_duration d) -> true
    | _ -> false

  let is_default_timing ctx = function
    | Ease -> true
    | Cubic_bezier (0.25, 0.1, 0.25, 1.0) when Pp.minified ctx -> true
    | _ -> false

  let is_timing ctx : timing_function option -> bool = function
    | Some Ease | None -> false
    | Some tf -> not (is_default_timing ctx tf)

  let is_iteration : animation_iteration_count option -> bool = function
    | Some (Num 1.) | None -> false
    | Some _ -> true

  let is_direction : animation_direction option -> bool = function
    | Some Normal | None -> false
    | Some _ -> true

  let is_fill_mode : animation_fill_mode option -> bool = function
    | Some None | None -> false
    | Some _ -> true

  let is_play_state : animation_play_state option -> bool = function
    | Some Running | None -> false
    | Some _ -> true

  let is_timeline : animation_timeline option -> bool = function
    | Some Auto | None -> false
    | Some _ -> true

  let has_non_defaults ctx (anim : animation_shorthand) =
    is_duration anim.duration
    || is_timing ctx anim.timing_function
    || is_duration anim.delay
    || is_iteration anim.iteration_count
    || is_direction anim.direction
    || is_fill_mode anim.fill_mode
    || is_play_state anim.play_state
    || is_timeline anim.timeline

  let ambiguous_name_kind (anim : animation_shorthand) =
    match anim.name with
    | Some (Ambiguous name) ->
        animation_shorthand_kind (String.lowercase_ascii name)
    | _ -> None

  (* If the caller will quote an ambiguous animation name, the placeholder slot
     used for unquoted disambiguation is no longer needed. *)
  let effective_ambiguous_kind ~quote_name anim =
    if quote_name then Option.None else ambiguous_name_kind anim

  (* When the colliding shorthand slot already carries an explicit non-default
     value, the bare-ident form of the ambiguous name is unambiguous on
     re-parse: the slot fills first, and the second occurrence falls through to
     the keyframes-name. In that case the printer can skip the quoting trick
     entirely. *)
  let bare_ambiguous_safe ctx (anim : animation_shorthand) =
    match ambiguous_name_kind anim with
    | None -> false
    | Some Timing -> (
        match anim.timing_function with
        | Some tf -> not (is_default_timing ctx tf)
        | None -> false)
    | Some Iteration -> is_iteration anim.iteration_count
    | Some Direction -> is_direction anim.direction
    | Some Fill -> is_fill_mode anim.fill_mode
    | Some Play -> is_play_state anim.play_state
    | Some Timeline -> is_timeline anim.timeline

  let duration (anim : animation_shorthand) : duration option =
    match (anim.duration, anim.delay) with
    | Some d, Some delay when is_zero_duration d && is_duration (Some delay) ->
        Some d
    | _ -> (
        match anim.duration with
        | Some d when not (is_zero_duration d) -> Some d
        | _ -> None)

  let timing ?(quote_name = false) ctx (anim : animation_shorthand) :
      timing_function option =
    match (anim.timing_function, effective_ambiguous_kind ~quote_name anim) with
    | (Some Ease | None), Some Timing -> Some Ease
    | Some tf, _ when is_default_timing ctx tf -> None
    | None, _ -> None
    | Some t, _ -> Some t

  let delay (anim : animation_shorthand) : duration option =
    match anim.delay with
    | Some d when not (is_zero_duration d) -> Some d
    | _ -> None

  let iteration ?(quote_name = false) (anim : animation_shorthand) :
      animation_iteration_count option =
    match (anim.iteration_count, effective_ambiguous_kind ~quote_name anim) with
    | (Some (Num 1.) | None), Some Iteration -> Some (Num 1.)
    | Some (Num 1.), _ | None, _ -> None
    | Some c, _ -> Some c

  let direction ?(quote_name = false) (anim : animation_shorthand) :
      animation_direction option =
    match (anim.direction, effective_ambiguous_kind ~quote_name anim) with
    | (Some Normal | None), Some Direction -> Some Normal
    | Some Normal, _ | None, _ -> None
    | Some d, _ -> Some d

  let fill_mode ?(quote_name = false) (anim : animation_shorthand) :
      animation_fill_mode option =
    match (anim.fill_mode, effective_ambiguous_kind ~quote_name anim) with
    | (Some None | None), Some Fill -> Some None
    | Some None, _ | None, _ -> None
    | Some m, _ -> Some m

  let play_state ?(quote_name = false) (anim : animation_shorthand) :
      animation_play_state option =
    match (anim.play_state, effective_ambiguous_kind ~quote_name anim) with
    | (Some Running | None), Some Play -> Some Running
    | Some Running, _ | None, _ -> None
    | Some s, _ -> Some s

  let timeline ?(quote_name = false) (anim : animation_shorthand) :
      animation_timeline option =
    match (anim.timeline, effective_ambiguous_kind ~quote_name anim) with
    | (Some Auto | None), Some Timeline -> Some Auto
    | Some Auto, _ | None, _ -> None
    | Some tl, _ -> Some tl
end

let read_animation_shorthand t : animation_shorthand =
  Animation.read_shorthand t

type animation_pp_state = {
  first : bool ref;
  prev_ends_with_paren : bool ref;
  prev_ends_with_quote : bool ref;
}

let animation_pp_state () =
  {
    first = ref true;
    prev_ends_with_paren = ref false;
    prev_ends_with_quote = ref false;
  }

let pp_animation_space_before state ?(ends_with_paren = false)
    ?(ends_with_quote = false) ?(starts_with_quote = false) pp ctx x =
  if !(state.first) then state.first := false
  else if
    Pp.minified ctx
    && (!(state.prev_ends_with_paren)
       || !(state.prev_ends_with_quote)
       || starts_with_quote)
  then
    (* Minified output drops the inter-token space when one side already carries
       a self-delimiting boundary - a closing function paren, a closing string
       quote, or an upcoming opening string quote. *)
    ()
  else Pp.char ctx ' ';
  state.prev_ends_with_paren := ends_with_paren;
  state.prev_ends_with_quote := ends_with_quote;
  pp ctx x

let animation_name_is_default_none ctx = function
  | Some (None : animation_name) -> true
  | Some (Quoted s) when Pp.minified ctx && String.lowercase_ascii s = "none" ->
      true
  | _ -> false

let animation_quote_ambiguous_name ctx anim =
  Pp.minified ctx
  && Animation.ambiguous_name_kind anim <> None
  && not (Animation.bare_ambiguous_safe ctx anim)

let pp_animation_initial_none ctx (anim : animation_shorthand)
    ~name_is_default_none ~has_any_non_default =
  match (anim.name, name_is_default_none, has_any_non_default) with
  | _, true, true -> ()
  | Option.None, _, false -> Pp.string ctx "none"
  | Option.None, _, true -> ()
  | Option.Some _, _, _ -> ()

let pp_animation_name_slot ctx state ~quote_ambiguous_name
    (anim : animation_shorthand) =
  if not (animation_name_is_default_none ctx anim.name) then
    Option.iter
      (fun (name : animation_name) ->
        match name with
        | Ambiguous s when quote_ambiguous_name ->
            pp_animation_space_before state ~starts_with_quote:true
              ~ends_with_quote:true Pp.quoted_string ctx s
        | Quoted s ->
            pp_animation_space_before state ~starts_with_quote:true
              ~ends_with_quote:true Pp.quoted_string ctx s
        | _ -> pp_animation_space_before state pp_animation_name ctx name)
      anim.name

let pp_animation_timing_slot ctx state ~quote_ambiguous_name anim =
  match Animation.timing ~quote_name:quote_ambiguous_name ctx anim with
  | Some tf ->
      let ends = Animation.ends_with_paren ctx tf in
      pp_animation_space_before state ~ends_with_paren:ends Animation.pp_timing
        ctx tf
  | None -> ()

let pp_animation_shorthand : animation_shorthand Pp.t =
 fun ctx anim ->
  let state = animation_pp_state () in
  let has_any_non_default = Animation.has_non_defaults ctx anim in
  let name_is_default_none = animation_name_is_default_none ctx anim.name in
  let quote_ambiguous_name = animation_quote_ambiguous_name ctx anim in
  pp_animation_initial_none ctx anim ~name_is_default_none ~has_any_non_default;
  (* Cascade canonical order puts the animation [name] first: it is the only
     ident-shaped component that survives the rest defaulting away, so leading
     with it makes "single-token" outputs ([animation:slide]) read naturally and
     matches the common minifier convention. *)
  pp_animation_name_slot ctx state ~quote_ambiguous_name anim;
  Pp.option
    (pp_animation_space_before state pp_duration)
    ctx (Animation.duration anim);
  pp_animation_timing_slot ctx state ~quote_ambiguous_name anim;
  Pp.option
    (pp_animation_space_before state pp_duration)
    ctx (Animation.delay anim);
  Pp.option
    (pp_animation_space_before state Animation.pp_iter_count)
    ctx
    (Animation.iteration ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_direction)
    ctx
    (Animation.direction ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_fill_mode)
    ctx
    (Animation.fill_mode ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_play_state)
    ctx
    (Animation.play_state ~quote_name:quote_ambiguous_name anim);
  Pp.option
    (pp_animation_space_before state pp_animation_timeline)
    ctx
    (Animation.timeline ~quote_name:quote_ambiguous_name anim)

let rec pp_animation : animation Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_animation ctx v
  | Shorthand s -> pp_animation_shorthand ctx s

let animation_global_value ident : animation option =
  match String.lowercase_ascii ident with
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "none" -> Some None
  | _ -> None

let animation_value_boundary t =
  Cursor.ws t;
  Cursor.is_done t || Cursor.peek_comma t

let read_animation_shorthand_from t snap : animation =
  Cursor.restore t snap;
  Shorthand (read_animation_shorthand t)

let read_animation_global_or_shorthand t =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match animation_global_value ident with
      | Some value when animation_value_boundary t -> value
      | _ -> read_animation_shorthand_from t snap)
  | None -> read_animation_shorthand_from t snap

let read_animation_var_or_shorthand read_self t =
  let snap = Cursor.save t in
  let value = (Var (read_var read_self t) : animation) in
  if animation_value_boundary t then value
  else read_animation_shorthand_from t snap

let rec read_animation t : animation =
  if Cursor.looking_at_func "var" t then
    read_animation_var_or_shorthand read_animation t
  else read_animation_global_or_shorthand t

let read_animations t : animation list =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_animation t

let rec read_blend_mode t : blend_mode =
  let read_var t : blend_mode = Var (read_var read_blend_mode t) in
  Cursor.enum_or_calls "blend-mode"
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
      ("plus-darker", Plus_darker);
      ("plus-lighter", Plus_lighter);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    t

module Text_shadow = struct
  type component = Color of color | Length of length

  let read_component t : component =
    Cursor.one_of
      [ (fun t -> Color (read_color t)); (fun t -> Length (read_length t)) ]
      t

  let fold_components components =
    let lengths =
      List.filter_map (function Length l -> Some l | _ -> None) components
    in
    let color =
      List.find_map (function Color c -> Some c | _ -> None) components
    in
    (lengths, color)
end

let rec read_text_shadow t : text_shadow =
  let read_var t : text_shadow = Var (read_var read_text_shadow t) in
  Cursor.enum_or_calls "text-shadow"
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t ->
      let components, _ = Cursor.many Text_shadow.read_component t in
      let lengths, color = Text_shadow.fold_components components in
      match lengths with
      | h :: v :: rest ->
          let blur = match rest with b :: _ -> Some b | _ -> None in
          (Text_shadow { h_offset = h; v_offset = v; blur; color }
            : text_shadow)
      | _ -> err_invalid_value t "text-shadow" "expected at least two lengths")
    t

let read_text_shadows t : text_shadow list =
  Cursor.list ~sep:Cursor.comma ~at_least:1 read_text_shadow t

let read_blur t : filter = Cursor.call "blur" t (fun t -> Blur (read_length t))

module Filter = struct
  let read_brightness t : filter =
    Cursor.call "brightness" t (fun t ->
        Brightness (Values.read_number_percentage t))

  let read_contrast t : filter =
    Cursor.call "contrast" t (fun t ->
        Contrast (Values.read_number_percentage t))

  let read_grayscale t : filter =
    Cursor.call "grayscale" t (fun t : filter ->
        Grayscale (Values.read_number_percentage t))

  let read_hue_rotate t : filter =
    Cursor.call "hue-rotate" t (fun t ->
        if Cursor.is_done t then Hue_rotate (Deg 0.)
        else Hue_rotate (read_angle t))

  let read_invert t : filter =
    Cursor.call "invert" t (fun t -> Invert (Values.read_number_percentage t))

  let read_opacity t : filter =
    Cursor.call "opacity" t (fun t : filter ->
        Opacity (Values.read_number_percentage t))

  let read_saturate t : filter =
    Cursor.call "saturate" t (fun t ->
        Saturate (Values.read_number_percentage t))

  let read_sepia t : filter =
    Cursor.call "sepia" t (fun t -> Sepia (Values.read_number_percentage t))

  let read_drop_shadow t : filter =
    Cursor.call "drop-shadow" t (fun t ->
        let read_var t : filter = Drop_shadow (Var (read_var Shadow.read t)) in
        let read_shadow t : filter = Drop_shadow (Shadow.read t) in
        Cursor.one_of [ read_var; read_shadow ] t)
end

let rec read_filter_item t : filter =
  let read_var t : filter = Var (read_var read_filter t) in
  (* [<filter-value-list>] mixes filter functions with a bare [<url>] reference
     to an SVG filter. [url(#id)] tokenises as a url-token, not a [url(]
     function, so it is read via [Cursor.url] (which handles both that and the
     quoted [url("#id")] form) and backtracks to the function dispatch. *)
  let read_url t = (Url (Cursor.url t) : filter) in
  Cursor.one_of
    [
      read_url;
      (fun t ->
        Cursor.enum_or_calls "filter"
          [ ("none", (None : filter)) ]
          ~calls:
            [
              ("blur", read_blur);
              ("brightness", Filter.read_brightness);
              ("contrast", Filter.read_contrast);
              ("grayscale", Filter.read_grayscale);
              ("hue-rotate", Filter.read_hue_rotate);
              ("invert", Filter.read_invert);
              ("opacity", Filter.read_opacity);
              ("saturate", Filter.read_saturate);
              ("sepia", Filter.read_sepia);
              ("drop-shadow", Filter.read_drop_shadow);
              ("var", read_var);
            ]
          t);
    ]
    t

and read_filter t : filter =
  let read_filter_list t =
    let filters, _ = Cursor.many read_filter_item t in
    match filters with
    | [] -> err_invalid_value t "filter" "expected filter function(s)"
    | [ f ] -> f
    | fs
      when List.exists
             (fun (value : filter) ->
               match value with None -> true | _ -> false)
             fs ->
        err_invalid_value t "filter" "none cannot be combined"
    | fs -> List fs
  in
  Cursor.enum "filter"
    [
      ("none", (None : filter));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:read_filter_list t

(* Background-related readers *)
let rec read_background_attachment t : background_attachment =
  let read_layer t =
    Cursor.enum "background-attachment layer"
      [
        ("scroll", (Scroll : background_attachment));
        ("fixed", Fixed);
        ("local", Local);
      ]
      t
  in
  let read_layers t =
    match Cursor.list ~sep:Cursor.comma ~at_least:1 read_layer t with
    | [ layer ] -> layer
    | layers -> Layers layers
  in
  Cursor.enum_or_var "background-attachment"
    [
      ("initial", (Initial : background_attachment));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_background_attachment t))
    ~default:read_layers t

let rec read_background_repeat t : background_repeat =
  let read_style t =
    Cursor.enum "background-repeat"
      [
        ("repeat", (Repeat : background_repeat));
        ("space", Space);
        ("round", Round);
        ("no-repeat", No_repeat);
      ]
      t
  in
  let pair (a : background_repeat) (b : background_repeat) =
    match (a, b) with
    | Repeat, Repeat -> Repeat_repeat
    | Repeat, Space -> Repeat_space
    | Repeat, Round -> Repeat_round
    | Repeat, No_repeat -> Repeat_no_repeat
    | Space, Repeat -> Space_repeat
    | Space, Space -> Space_space
    | Space, Round -> Space_round
    | Space, No_repeat -> Space_no_repeat
    | Round, Repeat -> Round_repeat
    | Round, Space -> Round_space
    | Round, Round -> Round_round
    | Round, No_repeat -> Round_no_repeat
    | No_repeat, Repeat -> No_repeat_repeat
    | No_repeat, Space -> No_repeat_space
    | No_repeat, Round -> No_repeat_round
    | No_repeat, No_repeat -> No_repeat_no_repeat
    | _ -> a
  in
  let read_repeats t =
    let first = read_style t in
    Cursor.ws t;
    match Cursor.option read_style t with
    | None -> first
    | Some second -> pair first second
  in
  Cursor.enum_or_var "background-repeat"
    [
      ("repeat-x", Repeat_x);
      ("repeat-y", Repeat_y);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_background_repeat t))
    ~default:read_repeats t

(* The standalone [background-repeat] / [mask-repeat] longhand is a
   comma-separated layer list (CSS Backgrounds 3 §3.6); the [background] /
   [mask] shorthand reuses the single-value [read_background_repeat] so it does
   not eat the layer comma. Same split for the box / size / composite readers
   below. *)
let read_background_repeat_list t : background_repeat =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_background_repeat t with
  | [ one ] -> one
  | many -> Layers many

let rec read_background_size t : background_size =
  let read_pair t : background_size =
    let a, b =
      Cursor.pair
        (read_length ~allow_negative:false)
        (read_length ~allow_negative:false)
        t
    in
    Size (a, b)
  in
  let read_single t : background_size =
    Length (read_length ~allow_negative:false t)
  in
  let read_var_call t : background_size =
    (Var (read_var read_background_size t) : background_size)
  in
  Cursor.enum_or_var "background-size"
    [
      ("auto", (Auto : background_size));
      ("cover", Cover);
      ("contain", Contain);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:read_var_call
    ~default:(fun t -> Cursor.one_of [ read_pair; read_single ] t)
    t

let read_background_size_list t : background_size =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_background_size t with
  | [ one ] -> one
  | many -> Layers many

module Gradient_direction = struct
  type keyword = Top | Bottom | Left | Right

  let read_keyword t : keyword =
    Cursor.enum "direction"
      [ ("top", Top); ("bottom", Bottom); ("left", Left); ("right", Right) ]
      t

  let merge_keywords t (keywords : keyword list) =
    match keywords with
    | [ Top ] -> To_top
    | [ Bottom ] -> To_bottom
    | [ Left ] -> To_left
    | [ Right ] -> To_right
    | [ Top; Left ] | [ Left; Top ] -> To_top_left
    | [ Top; Right ] | [ Right; Top ] -> To_top_right
    | [ Bottom; Left ] | [ Left; Bottom ] -> To_bottom_left
    | [ Bottom; Right ] | [ Right; Bottom ] -> To_bottom_right
    | _ ->
        err_invalid_value t "gradient-direction" "invalid direction combination"

  let read_to_direction t =
    Cursor.expect_string "to" t;
    Cursor.ws t;
    let directions, _ = Cursor.many read_keyword t in
    merge_keywords t directions

  let read_legacy t =
    let directions, _ = Cursor.many read_keyword t in
    match directions with
    | [] -> Cursor.err_expected t "legacy gradient direction"
    | _ -> merge_keywords t directions

  let read_angle t = (Angle (Values.read_angle t) : gradient_direction)

  let read t : gradient_direction =
    Cursor.one_of [ read_to_direction; read_angle ] t
end

let read_gradient_direction t : gradient_direction = Gradient_direction.read t

module Gradient_stop = struct
  (* Parse specific combinations *)
  let read_color_lp_lp t =
    let color, lp1, lp2 =
      Cursor.triple ~sep:Cursor.ws read_color read_length_percentage
        read_length_percentage t
    in
    Color_percentage (color, Some lp1, Some lp2)

  let read_color_lp t =
    let color, lp =
      Cursor.pair ~sep:Cursor.ws read_color read_length_percentage t
    in
    Color_percentage (color, Some lp, None)

  let read_color_len_len t =
    let color, len1, len2 =
      Cursor.triple ~sep:Cursor.ws read_color read_length read_length t
    in
    Color_length (color, Some len1, Some len2)

  let read_color_len t =
    let color = read_color t in
    Cursor.ws t;
    let len = read_length t in
    Color_length (color, Some len, None)

  let read_color_only t =
    let color = read_color t in
    Color_percentage (color, None, None)

  let read_pct t : gradient_stop = Percentage (read_percentage t)
  let read_len t : gradient_stop = Length (read_length t)
  let read_channel t : gradient_stop = Channel (Values.read_channel t)
end

let rec read_gradient_stop_single t : gradient_stop =
  let read_var t : gradient_stop = Var (read_var read_gradient_stop_list t) in
  Cursor.ws t;
  (* Try from most specific to most general, letting individual parsers handle
     their own variables *)
  Cursor.one_of
    [
      (* 3 elements: color + two length-percentages/lengths (most specific) *)
      Gradient_stop.read_color_lp_lp;
      Gradient_stop.read_color_len_len;
      (* 2 elements: color + length-percentage/length *)
      Gradient_stop.read_color_lp;
      Gradient_stop.read_color_len;
      (* 1 element: single values *)
      Gradient_stop.read_color_only;
      Gradient_stop.read_pct;
      Gradient_stop.read_len;
      Gradient_stop.read_channel;
      (* Full gradient_stop variables (e.g., var(--tw-gradient-stops)) - last
         resort *)
      read_var;
    ]
    t

and read_gradient_stop_list t : gradient_stop =
  (* Parse a list of gradient stops - used only for var fallbacks *)
  match
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_gradient_stop_single t
  with
  | [ x ] -> x
  | l -> List l

let read_gradient_stop t : gradient_stop =
  (* Only parse single gradient stops - lists are created by var fallback
     parsing *)
  read_gradient_stop_single t

let read_gradient_stops t =
  match
    Cursor.option
      (Cursor.list ~at_least:1 ~sep:Cursor.comma read_gradient_stop)
      t
  with
  | Some stops -> stops
  | None -> []

module Position_value = struct
  type keyword =
    | Center
    | Left
    | Right
    | Top
    | Bottom
    | Inherit
    | Initial
    | Unset
    | Revert
    | Revert_layer

  let read_xy (t : Cursor.t) : position_value =
    let x = read_length t in
    (* Reject global keywords - they should be parsed by read_1_value *)
    (match x with
    | Inherit | Initial | Unset | Revert | Revert_layer ->
        Cursor.err_invalid t "global keywords must be used alone"
    | _ -> ());
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some y -> XY (x, y)
    | None -> Single x

  let read_keyword t : keyword =
    Cursor.enum "position-keyword"
      [
        ("center", Center);
        ("left", Left);
        ("right", Right);
        ("top", Top);
        ("bottom", Bottom);
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      t

  (* Read single keyword value *)
  let read_1_value t : position_value =
    let kw = read_keyword t in
    match kw with
    | Center -> Center
    | Inherit -> Inherit
    | Initial -> Initial
    | Unset -> Unset
    | Revert -> Revert
    | Revert_layer -> Revert_layer
    | Left -> Left
    | Right -> Right
    | Top -> Top
    | Bottom -> Bottom

  (* Read two keyword values *)
  let read_2_value t : position_value =
    let first = read_keyword t in
    (* Global keywords cannot be combined with other keywords *)
    (match first with
    | Inherit | Initial | Unset | Revert | Revert_layer ->
        Cursor.err_invalid t "global keywords cannot be combined"
    | _ -> ());
    Cursor.ws t;
    let second = read_keyword t in
    (* Preserve the source-level order of the two keywords; the AST has both a
       [horizontal_then_vertical] and [vertical_then_horizontal] variant per
       pair, and the printer emits them in their source spelling. *)
    match (first, second) with
    | Left, Top -> Left_top
    | Top, Left -> Top_left
    | Left, Center -> Left_center
    | Center, Left -> Left_center
    | Left, Bottom -> Left_bottom
    | Bottom, Left -> Bottom_left
    | Right, Top -> Right_top
    | Top, Right -> Top_right
    | Right, Center -> Right_center
    | Center, Right -> Right_center
    | Right, Bottom -> Right_bottom
    | Bottom, Right -> Bottom_right
    | Center, Top -> Center_top
    | Top, Center -> Center_top
    | Center, Bottom -> Center_bottom
    | Bottom, Center -> Center_bottom
    | Center, Center -> Center
    | _ -> Cursor.err_invalid t "invalid position keyword combination"

  (* Read 3-value syntax: keyword offset keyword *)
  let read_3_value t : position_value =
    let edge1 = Cursor.ident t in
    Cursor.ws t;
    let offset = read_length_percentage t in
    Cursor.ws t;
    let axis = Cursor.ident t in
    Edge_offset_axis (edge1, offset, axis)

  let read_axis_edge_offset t : position_value =
    let axis = Cursor.ident t in
    Cursor.ws t;
    let edge = Cursor.ident t in
    Cursor.ws t;
    let offset = read_length t in
    match (axis, edge) with
    | "center", ("top" | "bottom") -> Axis_edge_offset (axis, edge, offset)
    | _ -> Cursor.err_invalid t "invalid position axis edge offset"

  let read_horizontal_keyword_length t : position_value =
    let keyword = Cursor.ident t in
    Cursor.ws t;
    let y = read_length t in
    match keyword with
    | "left" -> XY ((Pct 0. : length), y)
    | "right" -> XY ((Pct 100. : length), y)
    | "center" -> XY ((Pct 50. : length), y)
    | _ -> Cursor.err_invalid t "invalid horizontal position keyword length"

  let read_length_keyword t : position_value =
    let offset = read_length t in
    Cursor.ws t;
    match Cursor.ident t with
    | "center" -> Single offset
    | _ -> Cursor.err_invalid t "invalid position length keyword"

  (* Read 4-value syntax: keyword offset keyword offset *)
  let read_4_value t : position_value =
    let edge1 = Cursor.ident t in
    Cursor.ws t;
    let offset1 = read_length_percentage t in
    Cursor.ws t;
    let edge2 = Cursor.ident t in
    Cursor.ws t;
    let offset2 = read_length_percentage t in
    Edge_offset_edge_offset (edge1, offset1, edge2, offset2)
end

let rec read_position_value t : position_value =
  let read_var t : position_value = Var (read_var read_position_value t) in
  Cursor.one_of
    [
      Position_value.read_4_value;
      Position_value.read_axis_edge_offset;
      Position_value.read_3_value;
      Position_value.read_horizontal_keyword_length;
      Position_value.read_length_keyword;
      Position_value.read_xy;
      Position_value.read_2_value;
      Position_value.read_1_value;
      read_var;
    ]
    t

module Radial_config = struct
  let read_shape t : radial_shape =
    Cursor.enum "radial-shape" [ ("circle", Circle); ("ellipse", Ellipse) ] t

  let read_size_keyword t : radial_size =
    Cursor.enum "radial-size"
      [
        ("closest-side", (Closest_side : radial_size));
        ("farthest-side", Farthest_side);
        ("closest-corner", Closest_corner);
        ("farthest-corner", Farthest_corner);
      ]
      t

  let read_explicit_size t : radial_size =
    let first = read_length_percentage t in
    Cursor.ws t;
    match Cursor.option read_length_percentage t with
    | Some second -> Ellipse_radii (first, second)
    | None -> (
        match first with
        | Length l -> Circle_radius l
        | _ ->
            Cursor.err_invalid t
              "circle radius must be a length, not a percentage")

  let read_at_position t : position_value =
    Cursor.expect_string "at" t;
    Cursor.ws t;
    read_position_value t

  let read t : radial_gradient_config =
    (* CSS Images 4 section 6.2: the [radial-gradient] prelude is
       [<ending-shape> || <size> || at <position> ||
       <color-interpolation-method>]? - the [||] combinator allows any order.
       Loop trying each missing slot until no progress is made. *)
    let shape : radial_shape option ref = ref Option.None in
    let size : radial_size option ref = ref Option.None in
    let position : position_value option ref = ref Option.None in
    let interpolation : color_interpolation option ref = ref Option.None in
    let try_slot : 'a. 'a option ref -> (Cursor.t -> 'a) -> bool =
     fun slot reader ->
      if !slot <> Option.None then false
      else
        match Cursor.option reader t with
        | Option.Some v ->
            slot := Option.Some v;
            true
        | Option.None -> false
    in
    let rec loop () =
      Cursor.ws t;
      if
        try_slot shape read_shape
        || try_slot size (fun t ->
            Cursor.one_of [ read_size_keyword; read_explicit_size ] t)
        || try_slot position read_at_position
        || try_slot interpolation read_color_interpolation
      then loop ()
    in
    loop ();
    {
      shape = !shape;
      size = !size;
      position = !position;
      interpolation = !interpolation;
    }
end

let read_radial_shape t : radial_shape = Radial_config.read_shape t

let read_radial_size t : radial_size =
  Cursor.one_of
    [ Radial_config.read_size_keyword; Radial_config.read_explicit_size ]
    t

let read_radial_gradient_config t : radial_gradient_config =
  Radial_config.read t

let read_conic_gradient_config t : conic_gradient_config =
  (* CSS Images 4 section 3.5.1: the [conic-gradient] prelude is [[from
     <angle>]? || [at <position>]? || <color-interpolation-method>]? - the [||]
     combinator allows any order. Loop trying each missing slot until no
     progress is made. *)
  let angle : angle option ref = ref Option.None in
  let position : position_value option ref = ref Option.None in
  let interpolation : color_interpolation option ref = ref Option.None in
  let read_from t =
    match Cursor.peek_ident t with
    | Some "from" ->
        let _ = Cursor.ident t in
        Cursor.ws t;
        Values.read_angle t
    | _ -> Cursor.err_expected t "from"
  in
  let read_at t =
    match Cursor.peek_ident t with
    | Some "at" ->
        let _ = Cursor.ident t in
        Cursor.ws t;
        read_position_value t
    | _ -> Cursor.err_expected t "at"
  in
  let try_slot : 'a. 'a option ref -> (Cursor.t -> 'a) -> bool =
   fun slot reader ->
    if !slot <> Option.None then false
    else
      match Cursor.option reader t with
      | Option.Some v ->
          slot := Option.Some v;
          true
      | Option.None -> false
  in
  let rec loop () =
    Cursor.ws t;
    if
      try_slot angle read_from || try_slot position read_at
      || try_slot interpolation read_color_interpolation
    then loop ()
  in
  loop ();
  if
    !angle = Option.None && !position = Option.None
    && !interpolation = Option.None
  then Cursor.err_invalid t "conic-gradient config";
  { angle = !angle; position = !position; interpolation = !interpolation }

(* CSS Images 4 §6.1 [linear-gradient] prelude: [ <angle> | to <side-or-corner>
   ]? || <color-interpolation-method> A bare interpolation, a bare direction, or
   both in either order are all valid. Returns [None] when the prelude consumed
   no tokens. *)
let read_linear_prelude_opt t : gradient_direction option =
  let direction : gradient_direction option ref = ref Option.None in
  let interpolation : color_interpolation option ref = ref Option.None in
  let try_direction () =
    if !direction <> Option.None then false
    else
      match Cursor.option read_gradient_direction t with
      | Some d ->
          direction := Option.Some d;
          true
      | Option.None -> false
  in
  let try_interpolation () =
    if !interpolation <> Option.None then false
    else
      match Cursor.option read_color_interpolation t with
      | Some i ->
          interpolation := Option.Some i;
          true
      | Option.None -> false
  in
  let rec loop () =
    Cursor.ws t;
    if try_direction () || try_interpolation () then loop ()
  in
  loop ();
  match (!direction, !interpolation) with
  | Some d, Some i -> Option.Some (With_interpolation (d, i))
  | Some d, None -> Option.Some d
  | None, Some i -> Option.Some (With_interpolation (Default_direction, i))
  | None, None -> Option.None

(* Direction plus the optional [in <interpolation>] tail ([45deg in oklab]). *)
let read_gradient_prelude t : gradient_direction =
  match read_linear_prelude_opt t with
  | Some d -> d
  | None -> Cursor.err_expected t "gradient direction"

let rec read_gradient_position t : gradient_position =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name = "var"; _ }; _ }) ->
      (Var (Values.read_var read_gradient_position t) : gradient_position)
  | _ -> (
      match Cursor.peek_ident t with
      | Some "from" -> Conic_position (read_conic_gradient_config t)
      | Some
          ( "circle" | "ellipse" | "closest-side" | "closest-corner"
          | "farthest-side" | "farthest-corner" | "at" ) ->
          Radial_position (read_radial_gradient_config t)
      | _ ->
          Cursor.one_of
            [
              (fun t ->
                match read_linear_prelude_opt t with
                | Option.Some direction ->
                    (Linear_position direction : gradient_position)
                | Option.None -> Cursor.err_expected t "gradient position");
              (fun t -> Radial_position (read_radial_gradient_config t));
            ]
            t)

let read_linear_gradient_body_stops t =
  (* After the prelude the comma separating it from the colour-stop list is
     required if and only if the prelude consumed any tokens. *)
  let prelude = read_linear_prelude_opt t in
  let prelude_consumed = prelude <> Option.None in
  let direction =
    match prelude with Option.Some d -> d | Option.None -> Default_direction
  in
  if prelude_consumed then (
    Cursor.ws t;
    if not (Cursor.comma_opt t) then
      Cursor.err_expected t "',' after linear-gradient direction");
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in linear-gradient()";
  Linear_gradient (direction, stops)

let read_linear_gradient_body t =
  Cursor.ws t;
  (* CSS Variables 1 §3: a single [var()] can stand in for the entire body of
     [linear-gradient(...)], since the variable's value may itself contain
     commas and stops. Check this case first so the var() does not get
     mis-parsed as an [Angle (Var _)] direction by the prelude reader. *)
  let var_only =
    Cursor.lookahead
      (fun t ->
        let v = Cursor.option (Values.read_var read_gradient_stop) t in
        Cursor.ws t;
        if Cursor.is_done t then v else None)
      t
  in
  match var_only with
  | Some _ ->
      let v = Values.read_var read_gradient_stop t in
      Cursor.ws t;
      Cursor.expect_eof t;
      Linear_gradient_var v
  | None -> read_linear_gradient_body_stops t

let read_webkit_linear_gradient_body t =
  Cursor.ws t;
  let direction =
    Cursor.option
      (fun t ->
        Cursor.one_of
          [ Gradient_direction.read_legacy; Gradient_direction.read_angle ]
          t)
      t
  in
  if direction <> None then (
    Cursor.ws t;
    if not (Cursor.comma_opt t) then
      Cursor.err_expected t "',' after -webkit-linear-gradient direction");
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in -webkit-linear-gradient()";
  Webkit_linear_gradient (Option.value ~default:To_bottom direction, stops)

let read_radial_gradient_body t =
  Cursor.ws t;
  let config =
    match
      Cursor.option
        (fun t ->
          let cfg = Radial_config.read t in
          if
            cfg.shape = None && cfg.size = None && cfg.position = None
            && cfg.interpolation = None
          then Cursor.err_invalid t "no radial config";
          Cursor.ws t;
          ignore (Cursor.comma_opt t);
          cfg)
        t
    with
    | Some cfg -> cfg
    | None ->
        { shape = None; size = None; position = None; interpolation = None }
  in
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in radial-gradient()";
  Radial_gradient (config, stops)

let read_conic_gradient_body t =
  (* [conic-gradient([from <angle>]? [at <position>]? ,? <color-stop-list>)] *)
  let config = Cursor.option read_conic_gradient_config t in
  Cursor.ws t;
  let has_config = config <> None in
  if has_config && not (Cursor.peek_comma t || Cursor.is_done t) then
    Cursor.err_expected t "',' or end of conic-gradient prefix";
  if Cursor.peek_comma t then Cursor.skip t;
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in conic-gradient()";
  let config =
    match config with
    | Some config -> config
    | None ->
        ({ angle = None; position = None; interpolation = None }
          : conic_gradient_config)
  in
  Conic_gradient (config, stops)

let read_bg_url_arg inner =
  Cursor.ws inner;
  match Cursor.string_with_quote_opt inner with
  | Some (url, quote) ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Quoted (url, quote) : background_image)
  | None ->
      let url = Cursor.consume_remaining_as_string ~trim:true inner in
      if url = "" then Cursor.err_expected inner "url argument" else Url url

let read_bg_url_call t terminated =
  if not terminated then Cursor.err_expected t "terminated url";
  Cursor.call "url" t read_bg_url_arg

let read_bg_url t : background_image =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Url _; _ }) -> Url (Cursor.url t)
  | Some (Component.Func { node = { name = "url"; terminated; _ }; _ }) ->
      read_bg_url_call t terminated
  | _ -> Cursor.err_expected t "url"

let read_webkit_gradient_point t =
  match read_position_value t with
  | Left_top | Top_left -> Webkit_gradient.Left_top
  | Left_bottom | Bottom_left -> Webkit_gradient.Left_bottom
  | Center -> Webkit_gradient.Center
  | position -> Webkit_gradient.Position position

let read_webkit_gradient_stop t =
  let color_arg name inner =
    Cursor.ws inner;
    let color = read_color inner in
    Cursor.ws inner;
    Cursor.expect_eof inner;
    match name with
    | "from" -> Webkit_gradient.From color
    | "to" -> Webkit_gradient.To color
    | _ -> assert false
  in
  match Cursor.peek t with
  | Some (Component.Func { node = { name = ("from" | "to") as name; _ }; _ }) ->
      Cursor.call name t (color_arg name)
  | Some (Component.Func { node = { name = "color-stop"; _ }; _ }) ->
      Cursor.call "color-stop" t (fun inner ->
          Cursor.ws inner;
          let position = read_percentage inner in
          Cursor.ws inner;
          Cursor.comma inner;
          Cursor.ws inner;
          let color = read_color inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Webkit_gradient.Color_stop (position, color))
  | _ -> Cursor.err_expected t "-webkit-gradient color stop"

let read_webkit_gradient_radius t =
  match Cursor.number_opt t with
  | Some radius -> radius
  | None -> Cursor.err_expected t "-webkit-gradient radius"

let read_webkit_gradient_body t =
  Cursor.ws t;
  let kind = Cursor.ident t |> String.lowercase_ascii in
  Cursor.ws t;
  Cursor.comma t;
  match kind with
  | "linear" ->
      Cursor.ws t;
      let start = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let finish = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let stops =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_webkit_gradient_stop t
      in
      Cursor.ws t;
      Cursor.expect_eof t;
      Webkit_gradient (Webkit_gradient.Linear { start; finish; stops })
  | "radial" ->
      Cursor.ws t;
      let inner_center = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let inner_radius = read_webkit_gradient_radius t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let outer_center = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let outer_radius = read_webkit_gradient_radius t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let stops =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_webkit_gradient_stop t
      in
      Cursor.ws t;
      Cursor.expect_eof t;
      Webkit_gradient
        (Webkit_gradient.Radial
           { inner_center; inner_radius; outer_center; outer_radius; stops })
  | _ -> Cursor.err_invalid t ("-webkit-gradient kind: " ^ kind)

let read_image_set_option t : image_set_option =
  let source : image_set_source =
    Cursor.one_of
      [
        (fun t -> (Url (Cursor.url t) : image_set_source));
        (fun t -> String (Cursor.string t));
      ]
      t
  in
  (* [<resolution> || type(<string>)] in any order; both optional. Recurse,
     accepting whichever modifier hasn't been seen yet, until neither
     matches. *)
  let read_resolution_opt t : string option =
    match Cursor.dimension_opt t with
    | None -> None
    | Some (value, unit_) ->
        let buf = Buffer.create 8 in
        let pp_ctx = Pp.ctx ~minify:true buf in
        Pp.float pp_ctx value;
        Buffer.add_string buf unit_;
        Some (Buffer.contents buf)
  in
  let rec loop (mime : string option) (resolution : string option) =
    Cursor.ws t;
    match (mime, Cursor.function_call "type" Cursor.string t) with
    | None, Some s -> loop (Some s) resolution
    | _ -> (
        match (resolution, read_resolution_opt t) with
        | None, Some r -> loop mime (Some r)
        | _ -> (mime, resolution))
  in
  let mime, resolution = loop None None in
  if Option.is_none mime && Option.is_none resolution then
    Cursor.err_invalid t
      "image-set option requires a <resolution> or type(<string>)";
  { source; resolution; mime_type = mime }

let read_image_set_body t : background_image =
  Image_set (Cursor.list ~sep:Cursor.comma ~at_least:1 read_image_set_option t)

let read_repeating_linear_gradient t =
  match Cursor.call "repeating-linear-gradient" t read_linear_gradient_body with
  | Linear_gradient (d, stops) -> Repeating_linear_gradient (d, stops)
  | other -> other

let read_repeating_radial_gradient t =
  match Cursor.call "repeating-radial-gradient" t read_radial_gradient_body with
  | Radial_gradient (c, stops) -> Repeating_radial_gradient (c, stops)
  | other -> other

let read_repeating_conic_gradient t =
  match Cursor.call "repeating-conic-gradient" t read_conic_gradient_body with
  | Conic_gradient (c, stops) -> Repeating_conic_gradient (c, stops)
  | other -> other

let read_webkit_repeating_linear_gradient t =
  match
    Cursor.call "-webkit-repeating-linear-gradient" t
      read_webkit_linear_gradient_body
  with
  | Webkit_linear_gradient (d, stops) ->
      Webkit_repeating_linear_gradient (d, stops)
  | other -> other

let webkit_radial_gradient_of_radial = function
  | Radial_gradient (c, stops) -> Webkit_radial_gradient (c, stops)
  | other -> other

let webkit_repeat_radial_of_radial = function
  | Radial_gradient (c, stops) -> Webkit_repeating_radial_gradient (c, stops)
  | other -> other

let moz_linear_gradient_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> Moz_linear_gradient (d, stops)
  | other -> other

let moz_repeat_linear_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> Moz_repeating_linear_gradient (d, stops)
  | other -> other

let moz_radial_gradient_of_radial = function
  | Radial_gradient (c, stops) -> Moz_radial_gradient (c, stops)
  | other -> other

let moz_repeat_radial_of_radial = function
  | Radial_gradient (c, stops) -> Moz_repeating_radial_gradient (c, stops)
  | other -> other

let o_linear_gradient_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> O_linear_gradient (d, stops)
  | other -> other

let o_repeat_linear_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> O_repeating_linear_gradient (d, stops)
  | other -> other

let o_radial_gradient_of_radial = function
  | Radial_gradient (c, stops) -> O_radial_gradient (c, stops)
  | other -> other

let o_repeat_radial_of_radial = function
  | Radial_gradient (c, stops) -> O_repeating_radial_gradient (c, stops)
  | other -> other

let webkit_image_set_of_set = function
  | Image_set options -> Webkit_image_set options
  | other -> other

let read_bg_image_standard_calls =
  [
    ( "linear-gradient",
      fun t -> Cursor.call "linear-gradient" t read_linear_gradient_body );
    ( "radial-gradient",
      fun t -> Cursor.call "radial-gradient" t read_radial_gradient_body );
    ( "conic-gradient",
      fun t -> Cursor.call "conic-gradient" t read_conic_gradient_body );
    ("repeating-linear-gradient", read_repeating_linear_gradient);
    ("repeating-radial-gradient", read_repeating_radial_gradient);
    ("repeating-conic-gradient", read_repeating_conic_gradient);
  ]

let webkit_bg_image_calls =
  [
    ( "-webkit-linear-gradient",
      fun t ->
        Cursor.call "-webkit-linear-gradient" t read_webkit_linear_gradient_body
    );
    ("-webkit-repeating-linear-gradient", read_webkit_repeating_linear_gradient);
    ( "-webkit-radial-gradient",
      fun t ->
        Cursor.call "-webkit-radial-gradient" t read_radial_gradient_body
        |> webkit_radial_gradient_of_radial );
    ( "-webkit-repeating-radial-gradient",
      fun t ->
        Cursor.call "-webkit-repeating-radial-gradient" t
          read_radial_gradient_body
        |> webkit_repeat_radial_of_radial );
  ]

let legacy_bg_image_calls =
  [
    ( "-moz-linear-gradient",
      fun t ->
        Cursor.call "-moz-linear-gradient" t read_webkit_linear_gradient_body
        |> moz_linear_gradient_of_webkit );
    ( "-moz-repeating-linear-gradient",
      fun t ->
        Cursor.call "-moz-repeating-linear-gradient" t
          read_webkit_linear_gradient_body
        |> moz_repeat_linear_of_webkit );
    ( "-moz-radial-gradient",
      fun t ->
        Cursor.call "-moz-radial-gradient" t read_radial_gradient_body
        |> moz_radial_gradient_of_radial );
    ( "-moz-repeating-radial-gradient",
      fun t ->
        Cursor.call "-moz-repeating-radial-gradient" t read_radial_gradient_body
        |> moz_repeat_radial_of_radial );
    ( "-o-linear-gradient",
      fun t ->
        Cursor.call "-o-linear-gradient" t read_webkit_linear_gradient_body
        |> o_linear_gradient_of_webkit );
    ( "-o-repeating-linear-gradient",
      fun t ->
        Cursor.call "-o-repeating-linear-gradient" t
          read_webkit_linear_gradient_body
        |> o_repeat_linear_of_webkit );
    ( "-o-radial-gradient",
      fun t ->
        Cursor.call "-o-radial-gradient" t read_radial_gradient_body
        |> o_radial_gradient_of_radial );
    ( "-o-repeating-radial-gradient",
      fun t ->
        Cursor.call "-o-repeating-radial-gradient" t read_radial_gradient_body
        |> o_repeat_radial_of_radial );
  ]

let read_bg_image_vendor_calls = webkit_bg_image_calls @ legacy_bg_image_calls

let rec read_bg_image t : background_image =
  (* Bare [url(foo)] is a single [Token.Url] component, not a [Func]; handle it
     explicitly before dispatching on the function/ident shape. *)
  Cursor.one_of
    [
      read_bg_url;
      (fun t ->
        Cursor.enum_or_calls "background-image"
          [
            ("none", (None : background_image));
            ("initial", Initial);
            ("inherit", Inherit);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:(read_bg_image_calls ()) t);
    ]
    t

and read_bg_image_misc_calls () =
  [
    ("image-set", fun t -> Cursor.call "image-set" t read_image_set_body);
    ( "-webkit-image-set",
      fun t ->
        Cursor.call "-webkit-image-set" t read_image_set_body
        |> webkit_image_set_of_set );
    ("cross-fade", fun t -> Cursor.call "cross-fade" t read_cross_fade_body);
    ( "-webkit-gradient",
      fun t -> Cursor.call "-webkit-gradient" t read_webkit_gradient_body );
    ("var", fun t -> Var (Values.read_var read_bg_image t));
  ]

and read_bg_image_calls () =
  read_bg_image_standard_calls @ read_bg_image_vendor_calls
  @ read_bg_image_misc_calls ()

and read_cross_fade_body t : background_image =
  (* Parse a non-empty comma-separated list where every comma must be followed
     by another option. Cursor.list is permissive about trailing separators
     (rollback on failure leaves the comma silently consumed), so the spec's
     [<cf-mixing-image>#] grammar is enforced manually. *)
  let first = read_cross_fade_option t in
  let rec loop acc =
    Cursor.ws t;
    if Cursor.peek_comma t then (
      Cursor.skip t;
      loop (read_cross_fade_option t :: acc))
    else List.rev acc
  in
  let opts = loop [ first ] in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err_expected t "end of cross-fade() arguments";
  Cross_fade opts

and read_cross_fade_option t : cross_fade_option =
  Cursor.ws t;
  (* [<cf-mixing-image> = <percentage>? && <image>]. The two parts are
     order-independent in the spec; try percentage first, then image, and pick
     up a trailing percentage if the image came first. *)
  let pct =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
        Some (Values.read_percentage t)
    | _ -> None
  in
  Cursor.ws t;
  let image = read_bg_image t in
  Cursor.ws t;
  let pct =
    match pct with
    | Some _ -> pct
    | None -> (
        match Cursor.peek t with
        | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
            Some (Values.read_percentage t)
        | _ -> None)
  in
  { image; percent = pct }

let read_background_image t : background_image =
  let first = read_bg_image t in
  Cursor.ws t;
  if Cursor.comma_opt t then
    let rest = Cursor.list ~sep:Cursor.comma ~at_least:1 read_bg_image t in
    List (first :: rest)
  else first

let read_background_images t : background_image list =
  Cursor.list ~sep:Cursor.comma read_background_image t

let minify_gradient_stop : gradient_stop -> gradient_stop = function
  | Color_percentage (c, p1, p2) -> Color_percentage (minify_color c, p1, p2)
  | s -> s

let minify_webkit_gradient_stop = function
  | Webkit_gradient.From color -> Webkit_gradient.From (minify_color color)
  | Webkit_gradient.To color -> Webkit_gradient.To (minify_color color)
  | Webkit_gradient.Color_stop (position, color) ->
      Webkit_gradient.Color_stop (position, minify_color color)

let minify_conic_gradient_config config = config

let minify_background_image : background_image -> background_image = function
  | Linear_gradient (dir, stops) ->
      Linear_gradient (dir, List.map minify_gradient_stop stops)
  | Repeating_linear_gradient (dir, stops) ->
      Repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Webkit_linear_gradient (dir, stops) ->
      Webkit_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Webkit_repeating_linear_gradient (dir, stops) ->
      Webkit_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Moz_linear_gradient (dir, stops) ->
      Moz_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Moz_repeating_linear_gradient (dir, stops) ->
      Moz_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | O_linear_gradient (dir, stops) ->
      O_linear_gradient (dir, List.map minify_gradient_stop stops)
  | O_repeating_linear_gradient (dir, stops) ->
      O_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Radial_gradient (config, stops) ->
      Radial_gradient (config, List.map minify_gradient_stop stops)
  | Repeating_radial_gradient (config, stops) ->
      Repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | Webkit_radial_gradient (config, stops) ->
      Webkit_radial_gradient (config, List.map minify_gradient_stop stops)
  | Webkit_repeating_radial_gradient (config, stops) ->
      Webkit_repeating_radial_gradient
        (config, List.map minify_gradient_stop stops)
  | Moz_radial_gradient (config, stops) ->
      Moz_radial_gradient (config, List.map minify_gradient_stop stops)
  | Moz_repeating_radial_gradient (config, stops) ->
      Moz_repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | O_radial_gradient (config, stops) ->
      O_radial_gradient (config, List.map minify_gradient_stop stops)
  | O_repeating_radial_gradient (config, stops) ->
      O_repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | Conic_gradient (config, stops) ->
      Conic_gradient
        ( minify_conic_gradient_config config,
          List.map minify_gradient_stop stops )
  | Repeating_conic_gradient (config, stops) ->
      Repeating_conic_gradient
        ( minify_conic_gradient_config config,
          List.map minify_gradient_stop stops )
  | Webkit_gradient (Webkit_gradient.Linear ({ stops; _ } as gradient)) ->
      Webkit_gradient
        (Webkit_gradient.Linear
           { gradient with stops = List.map minify_webkit_gradient_stop stops })
  | Webkit_gradient (Webkit_gradient.Radial ({ stops; _ } as gradient)) ->
      Webkit_gradient
        (Webkit_gradient.Radial
           { gradient with stops = List.map minify_webkit_gradient_stop stops })
  | img -> img

let read_any_property t =
  (* CSS property names are case-insensitive per Syntax §3.3. *)
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
  (* CSS Fragmentation 3 §6 page-break-* aliases. Keep them as typed legacy
     properties so pretty output preserves the authored property name; minified
     output still serializes through the shorter modern break-* spelling. *)
  | "page-break-before" -> Prop Page_break_before
  | "page-break-after" -> Prop Page_break_after
  | "page-break-inside" -> Prop Page_break_inside
  | "columns" -> Prop Columns
  | "column-width" -> Prop Column_width
  | "column-count" -> Prop Column_count
  | "column-rule" -> Prop Column_rule
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
  | _ when String.length prop_name >= 2 && String.sub prop_name 0 2 = "--" ->
      Prop (Unknown_property prop_name)
  | _ -> Cursor.err_invalid t ("unknown property: " ^ prop_name)

(* Helper functions for property types *)

(* RGB color helpers *)
let rgb_black : color = Rgb (Channels { r = Int 0; g = Int 0; b = Int 0 })

let shadow ?(inset = false) ?(inset_var : string option)
    ?(inset_var_no_fallback = false) ?(h_offset : length option)
    ?(v_offset : length option) ?(blur : length option)
    ?(spread : length option) ?(color : color option) () : shadow =
  let default_color = rgb_black in
  let body =
    {
      h_offset = Option.value h_offset ~default:(Px 0.);
      v_offset = Option.value v_offset ~default:(Px 0.);
      blur;
      spread;
      color = Some (Option.value color ~default:default_color);
    }
  in
  match inset_var with
  | Some name ->
      Inset (Toggle { name; no_fallback = inset_var_no_fallback; body })
  | None -> if inset then Inset (Body body) else Shadow body

let inset_ring_shadow ?(h_offset : length option) ?(v_offset : length option)
    ?(blur : length option) ?(spread : length option) ?(color : color option) ()
    : shadow =
  let h_offset = Option.value h_offset ~default:(Zero : length) in
  let v_offset = Option.value v_offset ~default:(Zero : length) in
  (Inset (Body { h_offset; v_offset; blur; spread; color }) : shadow)

let url path : background_image = Url path
let linear_gradient dir stops = Linear_gradient (dir, stops)

let radial_gradient
    ?(config =
      { shape = None; size = None; position = None; interpolation = None })
    stops =
  Radial_gradient (config, stops)

let color_stop c = (Color_percentage (c, None, None) : gradient_stop)
let color_position c pos = (Color_length (c, Some pos, None) : gradient_stop)

let animation_shorthand ?name ?duration ?timing_function ?delay ?iteration_count
    ?direction ?fill_mode ?play_state ?timeline () : animation =
  Shorthand
    {
      name = Option.map (fun name -> (Name name : animation_name)) name;
      duration;
      timing_function;
      delay;
      iteration_count;
      direction;
      fill_mode;
      play_state;
      timeline;
    }

let transition_shorthand ?(property = (All : transition_property_value))
    ?duration ?timing_function ?delay ?behavior () : transition =
  Shorthand { property; duration; timing_function; delay; behavior }

let border_shorthand ?width ?style ?color () : border =
  Shorthand { width; style; color }

let text_decoration_shorthand ?lines ?style ?color ?thickness () :
    text_decoration =
  Shorthand { lines = Option.value ~default:[] lines; style; color; thickness }

let background_shorthand ?color ?image ?position ?size ?repeat ?attachment ?clip
    ?origin () : background =
  Shorthand { color; image; position; size; repeat; attachment; clip; origin }

(* Parser for background_box values *)
let rec read_background_box t : background_box =
  Cursor.enum_or_var "background-box"
    [
      ("border-box", (Border_box : background_box));
      ("padding-box", Padding_box);
      ("content-box", Content_box);
      ("text", Text);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_background_box t))
    t

let read_background_box_list t : background_box =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_background_box t with
  | [ one ] -> one
  | many -> Layers many

(* Parser for webkit_mask_composite values *)
let rec read_webkit_mask_composite t : webkit_mask_composite =
  let read_item t =
    Cursor.enum "webkit-mask-composite-item"
      [
        ("source-over", (Source_over : webkit_mask_composite));
        ("xor", Xor);
        ("source-in", Source_in);
        ("source-out", Source_out);
      ]
      t
  in
  Cursor.enum_or_var "webkit-mask-composite"
    [
      ("inherit", (Inherit : webkit_mask_composite));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_mask_composite t))
    ~default:(fun t ->
      let composites = Cursor.list ~sep:Cursor.comma ~at_least:1 read_item t in
      match composites with
      | [ composite ] -> composite
      | composites -> Composites composites)
    t

(* Parser for mask_composite values (standard, not webkit) *)
let rec read_mask_composite t : mask_composite =
  Cursor.enum_or_var "mask-composite"
    [
      ("add", (Add : mask_composite));
      ("subtract", Subtract);
      ("intersect", Intersect);
      ("exclude", Exclude);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask_composite t))
    t

let read_mask_composite_list t : mask_composite =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_mask_composite t with
  | [ one ] -> one
  | many -> Composites many

(* Parser for webkit_mask_source_type values *)
let rec read_webkit_mask_source_type t : webkit_mask_source_type =
  Cursor.enum_or_var "webkit-mask-source-type"
    [
      ("alpha", (Alpha : webkit_mask_source_type));
      ("luminance", Luminance);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_mask_source_type t))
    t

(* Parser for mask_mode values (standard) *)
let rec read_mask_mode t : mask_mode =
  let read_single t =
    Cursor.enum "mask-mode"
      [
        ("alpha", (Alpha : mask_mode));
        ("luminance", Luminance);
        ("match-source", Match_source);
      ]
      t
  in
  let read_modes t =
    let modes = Cursor.list ~sep:Cursor.comma ~at_least:1 read_single t in
    match modes with [ mode ] -> mode | _ -> Modes modes
  in
  Cursor.enum_or_var "mask-mode"
    [
      ("initial", (Initial : mask_mode));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_mask_mode t) : mask_mode))
    ~default:read_modes t

(* Parser for mask_type values *)
let rec read_mask_type t : mask_type =
  Cursor.enum_or_var "mask-type"
    [
      ("alpha", (Alpha : mask_type));
      ("luminance", Luminance);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask_type t))
    t

(* Parser for mask_box values (mask-clip and mask-origin) *)
let rec read_mask_box t : mask_box =
  Cursor.enum_or_var "mask-box"
    [
      ("border-box", (Border_box : mask_box));
      ("content-box", Content_box);
      ("fill-box", Fill_box);
      ("padding-box", Padding_box);
      ("stroke-box", Stroke_box);
      ("view-box", View_box);
      ("no-clip", No_clip);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask_box t))
    t

let read_mask_box_list t : mask_box =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_mask_box t with
  | [ one ] -> one
  | many -> Layers many

module Mask_shorthand = struct
  let init =
    {
      image = None;
      position = None;
      size = None;
      repeat = None;
      origin = None;
      clip = None;
      mode = None;
      composite = None;
    }

  let read_image_item t =
    let image = read_background_image t in
    fun (mask : mask_layer) ->
      if mask.image = None then { mask with image = Some image } else mask

  let read_position_size_item t =
    let position = read_position_value t in
    Cursor.ws t;
    let size =
      if Cursor.slash_opt t then Some (read_background_size t) else None
    in
    fun (mask : mask_layer) ->
      if mask.position <> None then mask
      else
        let mask = { mask with position = Some position } in
        match size with
        | Some size when mask.size = None -> { mask with size = Some size }
        | _ -> mask

  let read_repeat_item t =
    let repeat = read_background_repeat t in
    fun (mask : mask_layer) ->
      if mask.repeat = None then { mask with repeat = Some repeat } else mask

  let read_box_item t =
    let box = read_mask_box t in
    fun (mask : mask_layer) ->
      if mask.origin = None then { mask with origin = Some box }
      else if mask.clip = None then { mask with clip = Some box }
      else mask

  let read_mode_item t =
    let mode = read_mask_mode t in
    fun (mask : mask_layer) ->
      if mask.mode = None then { mask with mode = Some mode } else mask

  let read_composite_item t =
    let composite = read_mask_composite t in
    fun (mask : mask_layer) ->
      if mask.composite = None then { mask with composite = Some composite }
      else mask

  let read_item t =
    Cursor.one_of
      [
        read_image_item;
        read_position_size_item;
        read_repeat_item;
        read_box_item;
        read_mode_item;
        read_composite_item;
      ]
      t
end

let read_mask_layer t : mask_layer =
  let apply acc upd =
    let next = upd acc in
    if next = acc then Cursor.err t "Duplicate property in mask shorthand"
    else next
  in
  let layer, _ =
    Cursor.fold_many Mask_shorthand.read_item ~init:Mask_shorthand.init ~f:apply
      t
  in
  if layer = Mask_shorthand.init then Cursor.err_expected t "mask value";
  layer

let rec read_mask t : mask =
  Cursor.enum_or_var "mask"
    [
      ("none", (None : mask));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask t))
    ~default:(fun t ->
      let layers =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_mask_layer t
      in
      match layers with [ layer ] -> Layer layer | layers -> Layers layers)
    t

let read_background_position t : background_position =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_position_value t

module Transform_origin = struct
  type keyword = Center | Left | Right | Top | Bottom

  let read_position t : transform_origin =
    let position =
      Cursor.one_of
        [ Position_value.read_4_value; Position_value.read_3_value ]
        t
    in
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some z -> Position_z (position, z)
    | None -> Position position

  let read_xyz (t : Cursor.t) : transform_origin =
    let x = read_length t in
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some y -> (
        Cursor.ws t;
        match Cursor.option read_length t with
        | Some z -> XYZ (x, y, z)
        | None -> XY (x, y))
    (* CSS Transforms 1 sec. 3: a single <length-percentage> sets the X origin
       and defaults Y to [center] ([50%]); it is not duplicated into Y. *)
    | None -> X x

  let read_keyword t : keyword =
    Cursor.enum "transform-origin-keyword"
      [
        ("center", Center);
        ("left", Left);
        ("right", Right);
        ("top", Top);
        ("bottom", Bottom);
      ]
      t

  let merge_keywords t (keywords : keyword list) : transform_origin =
    match keywords with
    | [ Center ] -> Center
    | [ Left ] -> Left
    | [ Right ] -> Right
    | [ Top ] -> Top
    | [ Bottom ] -> Bottom
    (* Two keyword combinations - order matters for output *)
    | [ Left; Top ] -> Left_top
    | [ Top; Left ] -> Top_left
    | [ Left; Center ] -> Left_center
    | [ Left; Bottom ] -> Left_bottom
    | [ Bottom; Left ] -> Bottom_left
    | [ Right; Top ] -> Right_top
    | [ Top; Right ] -> Top_right
    | [ Right; Center ] -> Right_center
    | [ Right; Bottom ] -> Right_bottom
    | [ Bottom; Right ] -> Bottom_right
    | [ Center; Top ] -> Center_top
    | [ Top; Center ] ->
        Center_top (* center can be horizontal, top is vertical *)
    | [ Center; Bottom ] -> Center_bottom
    | [ Bottom; Center ] ->
        Center_bottom (* center can be horizontal, bottom is vertical *)
    | [ Center; Center ] -> Center
    | _ -> err_invalid_value t "transform-origin" "invalid keyword combination"

  let read_keywords t =
    let keywords = Cursor.list ~at_least:1 ~at_most:2 read_keyword t in
    merge_keywords t keywords

  let read_center_center t =
    let first = Cursor.ident t in
    let second = Cursor.ident t in
    if first = "center" && second = "center" && Cursor.is_done t then
      Center_center
    else Cursor.err_invalid t "transform-origin center center"
end

let rec read_transform_origin (t : Cursor.t) : transform_origin =
  Cursor.enum_or_var "transform-origin"
    [
      ("initial", (Initial : transform_origin));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_transform_origin t))
    ~default:(fun t ->
      Cursor.one_of
        [
          Transform_origin.read_center_center;
          Transform_origin.read_position;
          Transform_origin.read_keywords;
          Transform_origin.read_xyz;
        ]
        t)
    t

let rec read_transform_box (t : Cursor.t) : transform_box =
  Cursor.enum_or_var "transform-box"
    [
      ("content-box", (Content_box : transform_box));
      ("border-box", Border_box);
      ("fill-box", Fill_box);
      ("stroke-box", Stroke_box);
      ("view-box", View_box);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_transform_box t))
    t

module Background_shorthand = struct
  let read_image_item t =
    (* A single image per layer: commas in the [background] shorthand separate
       layers, not images (that comma-list is the [background-image]
       longhand). *)
    let img = read_bg_image t in
    fun (bg : background_shorthand) ->
      if bg.image = None then { bg with image = Some img } else bg

  let read_position_size_item t =
    let pos = read_position_value t in
    Cursor.ws t;
    let size_opt =
      if Cursor.slash_opt t then Some (read_background_size t) else None
    in
    fun (bg : background_shorthand) ->
      if bg.position <> None then bg
      else
        let bg' = { bg with position = Some pos } in
        match size_opt with
        | Some s when bg'.size = None -> { bg' with size = Some s }
        | _ -> bg'

  let read_repeat_item t =
    let rep = read_background_repeat t in
    fun (bg : background_shorthand) ->
      if bg.repeat = None then { bg with repeat = Some rep } else bg

  let read_attachment_item t =
    let att = read_background_attachment t in
    fun (bg : background_shorthand) ->
      if bg.attachment = None then { bg with attachment = Some att } else bg

  let read_box_item t =
    let box = read_background_box t in
    fun (bg : background_shorthand) ->
      if bg.origin = None then { bg with origin = Some box }
      else if bg.clip = None then { bg with clip = Some box }
      else bg

  let read_color_item t =
    let col = read_color t in
    fun (bg : background_shorthand) ->
      if bg.color = None then { bg with color = Some col } else bg

  let read_item t =
    Cursor.one_of
      [
        read_image_item;
        read_position_size_item;
        read_repeat_item;
        read_attachment_item;
        read_box_item;
        read_color_item;
      ]
      t
end

let read_background_shorthand t : background_shorthand =
  Cursor.ws t;
  let init =
    {
      color = None;
      image = None;
      position = None;
      size = None;
      repeat = None;
      attachment = None;
      clip = None;
      origin = None;
    }
  in
  let apply acc upd =
    let new_acc = upd acc in
    (* Check if the update actually changed anything *)
    if new_acc = acc then
      (* Nothing changed, meaning we tried to set a duplicate property *)
      Cursor.err t "Duplicate property in background shorthand"
    else new_acc
  in
  let acc, _ =
    Cursor.fold_many Background_shorthand.read_item ~init ~f:apply t
  in
  if acc = init then Cursor.err_expected t "background value";
  acc

let read_background_vars read_self t =
  let rec loop acc =
    Cursor.ws t;
    if Cursor.looking_at_func "var" t then loop (read_var read_self t :: acc)
    else List.rev acc
  in
  loop []

let read_background_var_call read_self t : background =
  let first = read_var read_self t in
  match read_background_vars read_self t with
  | [] -> Var first
  | rest -> Vars (first :: rest)

let read_background_var_sequence read_self t : background =
  let snap = Cursor.save t in
  match read_background_vars read_self t with
  | _ :: _ :: _ as vars -> Vars vars
  | _ ->
      Cursor.restore t snap;
      Cursor.err_expected t "background var() sequence"

let background_keyword_value ident : background option =
  match String.lowercase_ascii ident with
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "none" -> Some None
  | _ -> None

let background_value_boundary t =
  Cursor.ws t;
  Cursor.is_done t || Cursor.peek_comma t

let read_background_shorthand_from t snap : background =
  Cursor.restore t snap;
  Shorthand (read_background_shorthand t)

let read_background_keyword_or_shorthand t : background =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match background_keyword_value ident with
      | Some value when background_value_boundary t -> value
      | _ -> read_background_shorthand_from t snap)
  | None -> read_background_shorthand_from t snap

let read_background_default read_self t =
  Cursor.one_of
    [
      read_background_var_sequence read_self;
      read_background_keyword_or_shorthand;
    ]
    t

let rec read_background t : background =
  Cursor.enum_or_calls "background"
    [ ("inherit", Inherit); ("initial", Initial); ("unset", Unset) ]
    ~calls:[ ("var", read_background_var_call read_background) ]
    ~default:(read_background_default read_background)
    t

let read_backgrounds t : background list =
  Cursor.list ~sep:Cursor.comma ~at_least:1 read_background t

(* Gap shorthand parser *)
let rec read_gap t : gap =
  let read_non_negative_length t =
    let len = read_length t in
    match len with
    | Px v
    | Rem v
    | Em v
    | Ch v
    | Ex v
    | Vw v
    | Vh v
    | Vmin v
    | Vmax v
    | Pt v
    | Pc v
    | In v
    | Cm v
    | Mm v
    | Q v
      when v < 0.0 ->
        Cursor.err t "gap values cannot be negative"
    | Auto | Inherit | Initial | Unset | Revert | Revert_layer | Fit_content ->
        Cursor.err t "gap values must be explicit lengths, not keywords"
    | _ -> len
  in
  Cursor.enum_or_var "gap"
    [
      ("inherit", (Inherit : gap));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_gap t))
    ~default:(fun t ->
      let first_length = read_non_negative_length t in
      Cursor.ws t;
      let second_length = Cursor.option read_non_negative_length t in
      match second_length with
      | Some col_gap ->
          (Lengths { row_gap = Some first_length; column_gap = Some col_gap }
            : gap)
      | None ->
          (Lengths
             { row_gap = Some first_length; column_gap = Some first_length }
            : gap))
    t

(* Reader for will-change property *)
let rec read_will_change t : will_change =
  let read_ident t =
    let ident = Cursor.ident ~keep_case:true t in
    if ident = "auto" then
      Cursor.err_invalid t "auto cannot be used in a will-change list";
    if ident = "will-change" then
      Cursor.err_invalid t "will-change cannot reference itself";
    ident
  in
  Cursor.enum_or_var "will-change"
    [
      ("auto", Will_change_auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_will_change t))
    ~default:(fun t ->
      match Cursor.list ~sep:Cursor.comma ~at_least:1 read_ident t with
      | [ "scroll-position" ] -> Scroll_position
      | [ "contents" ] -> Contents
      | [ "transform" ] -> Transform
      | [ "opacity" ] -> Opacity
      | props -> Properties props)
    t

let read_perspective_origin : Cursor.t -> perspective_origin =
  read_position_value

(* Reader for clip property (deprecated) *)
let rec read_clip t : clip =
  Cursor.ws t;
  Cursor.enum_or_var "clip"
    [
      ("auto", Clip_auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_clip t) : clip))
    ~default:(fun t ->
      Cursor.call "rect" t @@ fun inner ->
      Cursor.ws inner;
      let top = read_length inner in
      Cursor.ws inner;
      ignore (Cursor.comma_opt inner : bool);
      Cursor.ws inner;
      let right = read_length inner in
      Cursor.ws inner;
      ignore (Cursor.comma_opt inner : bool);
      Cursor.ws inner;
      let bottom = read_length inner in
      Cursor.ws inner;
      ignore (Cursor.comma_opt inner : bool);
      Cursor.ws inner;
      let left = read_length inner in
      Clip_rect (top, right, bottom, left))
    t

(* Inline border-radius parser for the [round <border-radius>] suffix shared by
   clip-path basic shapes. Cannot reuse [Declaration.read_border_radius] because
   Declaration depends on Properties; the same grammar is small enough to inline
   here. *)
let read_border_radius_inline_radii t =
  let rec loop acc count =
    if count >= 4 then List.rev acc
    else
      match Cursor.option (read_length_percentage ~allow_negative:false) t with
      | None -> List.rev acc
      | Some lp -> loop (lp :: acc) (count + 1)
  in
  match loop [] 0 with
  | [] -> Cursor.err_expected t "<length-percentage>"
  | radii -> radii

let read_border_radius_inline_vertical t =
  match Cursor.peek_delim t with
  | Some '/' ->
      Cursor.skip t;
      Cursor.ws t;
      Some (read_border_radius_inline_radii t)
  | _ -> None

let read_border_radius_inline t : border_radius =
  Cursor.ws t;
  let horizontal = read_border_radius_inline_radii t in
  Cursor.ws t;
  let vertical = read_border_radius_inline_vertical t in
  Radius { horizontal; vertical }

let read_clip_path_round t : border_radius option =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "round" ->
      let _ = Cursor.ident t in
      Cursor.ws t;
      Some (read_border_radius_inline t)
  | _ -> None

let read_clip_path_inset_side t : length_percentage option =
  Cursor.ws t;
  match (Cursor.is_done t, Cursor.peek_ident t) with
  | true, _ | _, Some "round" -> None
  | false, _ -> Some (read_length_percentage t)

let read_clip_path_inset t =
  Cursor.call "inset" t (fun t ->
      Cursor.ws t;
      let top = read_length_percentage t in
      let right = read_clip_path_inset_side t in
      let bottom = Option.bind right (fun _ -> read_clip_path_inset_side t) in
      let left = Option.bind bottom (fun _ -> read_clip_path_inset_side t) in
      let rounded = read_clip_path_round t in
      Cursor.ws t;
      Cursor.expect_eof t;
      Clip_path_inset { top; right; bottom; left; rounded })

let read_clip_path_extent inner : clip_path_extent =
  match Cursor.peek_ident inner with
  | Some "closest-side" ->
      Cursor.skip inner;
      Closest_side
  | Some "farthest-side" ->
      Cursor.skip inner;
      Farthest_side
  | _ -> Extent_length (read_length inner)

let read_clip_path_fill_rule t =
  Cursor.enum "clip-path fill-rule"
    [ ("nonzero", (Nonzero : clip_path_fill_rule)); ("evenodd", Evenodd) ]
    t

let read_clip_path_position_clause inner =
  Cursor.ws inner;
  match Cursor.peek_ident inner with
  | Some "at" ->
      Cursor.skip inner;
      Cursor.ws inner;
      Some (read_position_value inner)
  | _ -> None

(* Wrap a typed [<basic-shape>] reader so it falls back to [Invalid
   [<original-call>]] when the typed reduction refuses an admittedly-invalid
   input that upstream tools preserve verbatim. *)
let with_basic_shape_fallback t f : clip_path =
  match Cursor.try_typed_call f t with
  | Ok value -> value
  | Error comp -> (Invalid [ comp ] : clip_path)

let read_clip_path_circle t : clip_path =
  with_basic_shape_fallback t (fun t ->
      Cursor.call "circle" t @@ fun inner ->
      Cursor.ws inner;
      let radius : clip_path_extent option =
        if Cursor.is_done inner || Cursor.peek_ident inner = Some "at" then None
        else Some (read_clip_path_extent inner)
      in
      let position = read_clip_path_position_clause inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Clip_path_circle { radius; position })

let read_clip_path_ellipse t : clip_path =
  with_basic_shape_fallback t (fun t ->
      Cursor.call "ellipse" t @@ fun inner ->
      Cursor.ws inner;
      let rx : clip_path_extent option =
        if Cursor.is_done inner || Cursor.peek_ident inner = Some "at" then None
        else Some (read_clip_path_extent inner)
      in
      Cursor.ws inner;
      let ry : clip_path_extent option =
        if
          Option.is_none rx || Cursor.is_done inner
          || Cursor.peek_ident inner = Some "at"
        then None
        else Some (read_clip_path_extent inner)
      in
      let position = read_clip_path_position_clause inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Clip_path_ellipse { rx; ry; position })

let read_polygon_fill_rule inner =
  match Cursor.peek_ident inner with
  | Some "nonzero" ->
      Cursor.skip inner;
      Cursor.ws inner;
      Cursor.comma inner;
      Some Nonzero
  | Some "evenodd" ->
      Cursor.skip inner;
      Cursor.ws inner;
      Cursor.comma inner;
      Some Evenodd
  | _ -> None

let read_polygon_point inner =
  let x = read_length inner in
  Cursor.ws inner;
  let y = read_length inner in
  (x, y)

let polygon_has_ws_after_comma inner =
  match Cursor.peek_raw inner with
  | Some (Component.Preserved { kind = Token.Whitespace; _ }) -> true
  | _ -> false

let read_polygon_points inner =
  let rec loop acc spaced =
    Cursor.ws inner;
    if Cursor.comma_opt inner then (
      let spaced = spaced || polygon_has_ws_after_comma inner in
      Cursor.ws inner;
      loop (read_polygon_point inner :: acc) spaced)
    else (List.rev acc, spaced)
  in
  loop [ read_polygon_point inner ] false

let read_clip_path_polygon t : clip_path =
  Cursor.call "polygon" t @@ fun inner ->
  Cursor.ws inner;
  let fill_rule = read_polygon_fill_rule inner in
  Cursor.ws inner;
  let points, spaced = read_polygon_points inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Clip_path_polygon { fill_rule; points; spaced }

let read_clip_path_path t =
  Cursor.call "path" t @@ fun inner ->
  Cursor.ws inner;
  let data = Cursor.string inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Clip_path_path data

(* Read 4 length-percentages and an optional [round <border-radius>] suffix
   shared by xywh() and rect(). *)
let read_clip_path_inset_quad t =
  let a = read_length_percentage t in
  Cursor.ws t;
  let b = read_length_percentage t in
  Cursor.ws t;
  let c = read_length_percentage t in
  Cursor.ws t;
  let d = read_length_percentage t in
  (a, b, c, d)

let rec read_border_radius (t : Cursor.t) : border_radius =
  Cursor.enum_or_var "border-radius"
    [
      ("inherit", (Inherit : border_radius));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_border_radius t) : border_radius))
    ~default:read_border_radius_inline t

let read_clip_path_xywh t =
  Cursor.call "xywh" t (fun inner ->
      Cursor.ws inner;
      let x, y, width, height = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      Clip_path_xywh { x; y; width; height; rounded })

let read_clip_path_rect t =
  Cursor.call "rect" t (fun inner ->
      Cursor.ws inner;
      let top, right, bottom, left = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      Clip_path_rect { top; right; bottom; left; rounded })

let read_clip_path_shape t =
  Cursor.call "shape" t (fun inner ->
      Clip_path_shape (Cursor.consume_remaining_as_string ~trim:true inner))

let read_clip_geometry_box_opt t : clip_geometry_box option =
  match Cursor.peek_ident t with
  | Some "margin-box" ->
      Cursor.skip t;
      Some Margin_box
  | Some "border-box" ->
      Cursor.skip t;
      Some Border_box
  | Some "padding-box" ->
      Cursor.skip t;
      Some Padding_box
  | Some "content-box" ->
      Cursor.skip t;
      Some Content_box
  | Some "fill-box" ->
      Cursor.skip t;
      Some Fill_box
  | Some "stroke-box" ->
      Cursor.skip t;
      Some Stroke_box
  | Some "view-box" ->
      Cursor.skip t;
      Some View_box
  | _ -> None

let read_clip_geometry_box t =
  match read_clip_geometry_box_opt t with
  | Some box -> box
  | None -> Cursor.err_expected t "clip geometry box"

let rec read_clip_path (t : Cursor.t) : clip_path =
  Cursor.ws t;
  let read_basic_shape t : clip_path =
    Cursor.one_of
      [
        (fun t -> Clip_path_url (Cursor.url t));
        (fun t ->
          Cursor.enum_or_calls "clip-path"
            [
              ("none", Clip_path_none);
              ("inherit", Inherit);
              ("initial", Initial);
              ("unset", Unset);
              ("revert", Revert);
              ("revert-layer", Revert_layer);
            ]
            ~calls:
              [
                ("inset", read_clip_path_inset);
                ("circle", read_clip_path_circle);
                ("ellipse", read_clip_path_ellipse);
                ("polygon", read_clip_path_polygon);
                ("path", read_clip_path_path);
                ("xywh", read_clip_path_xywh);
                ("rect", read_clip_path_rect);
                ("shape", read_clip_path_shape);
                ( "var",
                  fun t -> (Var (Values.read_var read_clip_path t) : clip_path)
                );
              ]
            t);
      ]
      t
  in
  (* CSS Masking 1 §3.6 [<basic-shape> || <geometry-box>]: a shape and a
     reference box may appear in either order, or just a box on its own. *)
  let at_end t = Cursor.is_done t || Cursor.peek_semicolon t in
  match read_clip_geometry_box_opt t with
  | Some box ->
      Cursor.ws t;
      if at_end t then Clip_path_box box
      else
        let shape = read_basic_shape t in
        Clip_path_with_box { shape; box; box_first = true }
  | None -> (
      let shape = read_basic_shape t in
      Cursor.ws t;
      if at_end t then shape
      else
        match read_clip_geometry_box_opt t with
        | Some box -> Clip_path_with_box { shape; box; box_first = false }
        | None -> shape)

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

let pp_any_property ctx (Prop p) = pp_property ctx p

let pp_font_url ctx s =
  Pp.string ctx "url(";
  if url_needs_quotes s then (
    Pp.char ctx '"';
    Pp.string ctx s;
    Pp.char ctx '"')
  else Pp.string ctx s;
  Pp.char ctx ')'

let pp_quoted_font_url ctx quote s =
  Pp.string ctx "url(";
  Pp.char ctx quote;
  Pp.string ctx s;
  Pp.char ctx quote;
  Pp.char ctx ')'

let pp_font_src_modifiers ctx (format : string option) (tech : string option) =
  (match format with
  | None -> ()
  | Some value ->
      Pp.space ctx ();
      Pp.string ctx "format(";
      Pp.string ctx value;
      Pp.char ctx ')');
  match tech with
  | None -> ()
  | Some value ->
      Pp.space ctx ();
      Pp.string ctx "tech(";
      Pp.string ctx value;
      Pp.char ctx ')'

let rec pp_font_src_entry ctx : Font_face.src_entry -> unit = function
  | Local name ->
      Pp.string ctx "local(";
      Pp.char ctx '"';
      Pp.string ctx name;
      Pp.char ctx '"';
      Pp.char ctx ')'
  | Url { url; format; tech } ->
      pp_font_url ctx url;
      pp_font_src_modifiers ctx format tech
  | Quoted_url { url; quote; format; tech } ->
      pp_quoted_font_url ctx quote url;
      pp_font_src_modifiers ctx format tech
  | Var var -> pp_var pp_font_src ctx var

and pp_font_src ctx entries =
  let first = ref true in
  List.iter
    (fun entry ->
      if !first then first := false
      else (
        Pp.char ctx ',';
        Pp.space_if_pretty ctx ());
      pp_font_src_entry ctx entry)
    entries

let read_font_src = Font_face.read_src

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
  let text = Parser.to_string_custom [ c ] in
  let cur = Cursor.of_string text in
  match
    try Some (Values.read_color cur) with Cursor.Parse_error _ -> None
  with
  | Some col when Cursor.is_done cur -> (
      let canon =
        Pp.to_string ~minify:true Values.pp_color
          (Values.nonkeyword_color
             (Values.normalize_color ~lossless ~in_feature_query:false col))
      in
      match read_custom_property_value (Cursor.of_string canon) with
      | Tokens cs -> cs
      | Typed _ -> [ c ])
  | _ -> fallback ()

(* A math function ([calc()], [min()], [clamp()], ...) is unconditionally a math
   expression whose type is fixed by its operands' units, so when it reduces to
   a single constant it has that value in every [var()] substitution site - fold
   it inside an opaque custom-property stream like a complete colour. [<number>]
   and the unit-unambiguous dimensions ([<angle>] / [<time>]) qualify;
   [<percentage>] is ambiguous (length vs number percentage) so it stays
   verbatim, as does a function that still references a [var()] (it does not
   reduce to a leaf). *)
let is_math_function name =
  match String.lowercase_ascii name with
  | "calc" | "min" | "max" | "clamp" | "round" | "mod" | "rem" | "abs" | "sign"
  | "hypot" | "pow" | "sqrt" | "exp" | "log" ->
      true
  | _ -> false

let fold_custom_calc (c : Component.t) ~fallback =
  let text = Parser.to_string_custom [ c ] in
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
  let pp_rounded f = Pp.float ctx (Pp.round_sig 6 f) in
  match value with
  | Num f when Pp.minified ctx -> pp_rounded f
  | Calc c when Pp.minified ctx -> (
      match eval_numeric_calc c with
      | Some f -> pp_rounded f
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
        else Parser.to_string_custom value
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
  | Box_shadow -> pp pp_shadow
  | Content -> pp pp_content
  | Gradient_stop -> pp pp_gradient_stop
  | Gradient_direction -> pp pp_gradient_direction
  | Gradient_position -> pp pp_gradient_position
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
  | Value -> Parser.to_string_custom value
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
      | Attr _ | Counter _ | Counters _ | String_ref _ | Content_list _
      | Inherit | Initial | Unset | Revert | Revert_layer | Var _ ->
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

(* CSS Sizing 3 sec. 3.1: [min-width] / [min-height] / [min-inline-size] /
   [min-block-size] have [auto] as their initial value (not the generic [0]), so
   under minify [initial] rewrites to the shorter [auto]. *)
let pp_length_min_max ctx (v : length_percentage) =
  let v : length_percentage =
    match v with Length Initial when Pp.minified ctx -> Length Auto | _ -> v
  in
  pp_length_percentage ctx v

(* CSS Values 4 sec. 6.5: the [initial] keyword resolves to the property's
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
  | Z_index, Initial -> Auto
  | Opacity, Initial -> Opacity_number 1.
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
  | Width, Length Initial -> Length Auto
  | Height, Length Initial -> Length Auto
  | Min_width, Length Initial -> Length Auto
  | Min_height, Length Initial -> Length Auto
  | _ -> value

(* CSS Values L4 sec. 10.10 ("Mathematical Expressions"): inside a math function
   ([calc], [min], [max], [clamp], [round], [mod], [rem], the trig family,
   [pow]/[sqrt]/[hypot]/[log]/[exp], [abs]/[sign]) whitespace is *required*
   around binary [+] and [-] (sign-token disambiguation - stripping it changes
   [100% - var(--a)] to [100%-var(--a)], where [-var] is one ident-like function
   token) but *optional* around [*], [/], [(], [)], [,]. Strip the optional
   whitespace from math-function arguments so two custom-property token streams
   that differ only there have the same canonical AST. Typed math is already
   minified by [pp_calc]; this matters for opaque [Tokens _] custom-property
   values where cascade preserves the author's whitespace verbatim by design.
   Nested non-math functions ([var()] etc.) get a recursive component walk but
   no whitespace stripping; nested math functions get their own. *)
let math_function_names =
  [
    "calc";
    "min";
    "max";
    "clamp";
    "round";
    "mod";
    "rem";
    "sin";
    "cos";
    "tan";
    "asin";
    "acos";
    "atan";
    "atan2";
    "pow";
    "sqrt";
    "hypot";
    "log";
    "exp";
    "abs";
    "sign";
  ]

let is_math_function_name name =
  List.mem (String.lowercase_ascii name) math_function_names

let is_plus_or_minus_delim = function
  | Component.Preserved { kind = Token.Delim "+"; _ }
  | Component.Preserved { kind = Token.Delim "-"; _ } ->
      true
  | _ -> false

let strip_math_whitespace comps =
  let rec aux acc = function
    | [] -> List.rev acc
    | (Component.Preserved { kind = Token.Whitespace; _ } as ws) :: rest ->
        let prev_pm =
          match acc with [] -> false | p :: _ -> is_plus_or_minus_delim p
        in
        let next_pm =
          match rest with [] -> false | n :: _ -> is_plus_or_minus_delim n
        in
        if prev_pm || next_pm then aux (ws :: acc) rest else aux acc rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

let is_mul_or_div_delim = function
  | Component.Preserved { kind = Token.Delim ("*" | "/"); _ } -> true
  | _ -> false

(* Outside a math function only the whitespace adjacent to a [*] or [/] delim is
   insignificant (CSS Values 4 sec. 10.1): [16 / 9] and [16/9] re-tokenise
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
   stream is substituted. Whitespace after a substitution function stays. *)
let strip_after_close_paren comps =
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
        if prev_hard then aux acc rest else aux (ws :: acc) rest
    | other :: rest -> aux (other :: acc) rest
  in
  aux [] comps

(* [in_math] tracks whether the current component list is inside a math
   function's grammar. It enters at the args of a [calc()] / [min()] / ... call,
   propagates through grouping parens ([Block]s) since those are math operands,
   and turns off when entering a nested non-math function like [var()] which has
   its own grammar. *)
let rec canonicalize_math_whitespace_components ?(in_math = false) comps =
  let comps' =
    List.map
      (fun c ->
        match c with
        | Component.Func wrapped ->
            let func = wrapped.Component.node in
            let nested_in_math = is_math_function_name func.name in
            let args =
              canonicalize_math_whitespace_components ~in_math:nested_in_math
                func.arguments
            in
            Component.Func
              { wrapped with node = { func with arguments = args } }
        | Component.Block wrapped ->
            let block = wrapped.Component.node in
            let value =
              canonicalize_math_whitespace_components ~in_math block.value
            in
            Component.Block { wrapped with node = { block with value } }
        | Component.Preserved _ -> c)
      comps
  in
  let comps' =
    if in_math then strip_math_whitespace comps'
    else strip_mul_div_whitespace comps'
  in
  strip_after_close_paren comps'

(* AST-level value normaliser: applies semantic (equivalence) canonicalisation
   so the optimizer holds a canonical AST and [pp] stays a pure serialiser. Add
   property cases here as their folds migrate out of [pp]; everything else is
   identity. *)
let normalize_font_size (fs : font_size) : font_size =
  match fs with
  | Length l -> preserve_if_equal fs (Length (Values.normalize_length l))
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ -> fs

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

let normalize_line_height (lh : line_height) : line_height =
  match lh with
  | Calc c -> (
      match Values.eval_calc c with
      | Values.Num f -> Num f
      | Values.Val v -> v
      | folded -> Calc folded)
  | _ -> lh

let normalize_vertical_align (va : vertical_align) : vertical_align =
  match va with
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ -> va

(* A zero-valued border-width unit ([0px], [0em]) is the bare length [0]
   ([border-width: 0] = [border-width: 0px]); collapse it to [Zero] like every
   other zero length ([width: 0px] -> [0]), which [<length>]-valued properties
   already do via [normalize_length]. *)
let border_width_is_zero (bw : border_width) =
  match length_of_border_width bw with
  | Some l -> (
      match Values.normalize_length l with Values.Zero -> true | _ -> false)
  | None -> false

let normalize_border_width (bw : border_width) : border_width =
  match bw with
  | Calc c -> (
      match Values.eval_calc c with Values.Val v -> v | folded -> Calc folded)
  | _ when border_width_is_zero bw -> Zero
  | _ -> bw

let normalize_property_value : type a.
    ?lossless:bool -> ?ctx:Values.calc_ctx -> a property -> a -> a =
 fun ?(lossless = false) ?(ctx = Values.default_calc_ctx) property value ->
  let normalize_color =
    Values.normalize_color ~lossless ~in_feature_query:false
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
  | Offset_rotate -> normalize_offset_rotate value
  | Font_style -> normalize_font_style value
  | Width -> Values.normalize_length_percentage ~ctx value
  | Height -> Values.normalize_length_percentage ~ctx value
  | Min_width -> Values.normalize_length_percentage ~ctx value
  | Min_height -> Values.normalize_length_percentage ~ctx value
  | Min_inline_size -> Values.normalize_length_percentage ~ctx value
  | Min_block_size -> Values.normalize_length_percentage ~ctx value
  | Max_width -> Values.normalize_length_percentage ~ctx value
  | Max_height -> Values.normalize_length_percentage ~ctx value
  | Inline_size -> Values.normalize_length_percentage ~ctx value
  | Max_inline_size -> Values.normalize_length_percentage ~ctx value
  | Block_size -> Values.normalize_length_percentage ~ctx value
  | Max_block_size -> Values.normalize_length_percentage ~ctx value
  | Shape_margin -> Values.normalize_length_percentage ~ctx value
  | Offset_distance -> Values.normalize_length_percentage ~ctx value
  | Border_radius -> normalize_border_radius value
  | Background_image ->
      map_preserve (normalize_background_image ~lossless) value
  | Mask_image -> normalize_background_image ~lossless value
  | Webkit_mask_image -> normalize_background_image ~lossless value
  | Border_image_source -> normalize_background_image ~lossless value
  | Background -> map_preserve (normalize_background ~lossless) value
  | Mask -> normalize_mask ~lossless value
  | Clip_path -> normalize_clip_path value
  | Object_view_box -> normalize_object_view_box value
  | Object_position -> normalize_position_value value
  | Perspective_origin -> normalize_position_value value
  | Background_position -> map_preserve normalize_position_value value
  | Mask_position -> map_preserve normalize_position_value value
  | Webkit_mask_position -> map_preserve normalize_position_value value
  | Text_indent -> normalize_text_indent value
  | Animation_range -> normalize_animation_range value
  | View_timeline_inset -> normalize_timeline_inset value
  | Baseline_shift -> normalize_baseline_shift value
  | Background_color -> normalize_color value
  | Color -> normalize_color value
  | Border_color -> map_preserve normalize_color value
  | Border_top_color -> normalize_color value
  | Border_right_color -> normalize_color value
  | Border_bottom_color -> normalize_color value
  | Border_left_color -> normalize_color value
  | Border_inline_start_color -> normalize_color value
  | Border_inline_end_color -> normalize_color value
  | Border_inline_color -> normalize_logical_border_color ~lossless value
  | Text_decoration_color -> normalize_color value
  | Webkit_text_decoration_color -> normalize_color value
  | Webkit_tap_highlight_color -> normalize_color value
  | Text_emphasis_color -> normalize_color value
  | Outline_color -> normalize_color value
  | Accent_color -> normalize_color value
  | Caret_color -> normalize_color value
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
  | Caret -> normalize_caret ~lossless value
  | Fill -> normalize_svg_paint ~lossless value
  | Stroke -> normalize_svg_paint ~lossless value
  | Scrollbar_color -> normalize_scrollbar_color ~lossless value
  | Filter -> normalize_filter ~lossless value
  | Webkit_filter -> normalize_filter ~lossless value
  | Ms_filter -> normalize_filter ~lossless value
  | Backdrop_filter -> normalize_filter ~lossless value
  | Webkit_backdrop_filter -> normalize_filter ~lossless value
  | Flex_grow -> normalize_flex_factor value
  | Flex_shrink -> normalize_flex_factor value
  | Flex_basis -> normalize_flex_basis value
  | Flex -> normalize_flex value
  | Grid_template_columns -> normalize_grid_template value
  | Grid_template_rows -> normalize_grid_template value
  | Grid_template -> normalize_grid_template value
  | Grid -> normalize_grid_template value
  | Grid_auto_columns -> normalize_grid_template value
  | Grid_auto_rows -> normalize_grid_template value
  | Aspect_ratio -> normalize_aspect_ratio value
  | Gap -> normalize_gap value
  | Font_size -> normalize_font_size value
  | Padding_left -> Values.normalize_length ~ctx value
  | Padding_right -> Values.normalize_length ~ctx value
  | Padding_bottom -> Values.normalize_length ~ctx value
  | Padding_top -> Values.normalize_length ~ctx value
  | Padding_inline_start -> Values.normalize_length ~ctx value
  | Padding_inline_end -> Values.normalize_length ~ctx value
  | Padding_block_start -> Values.normalize_length ~ctx value
  | Padding_block_end -> Values.normalize_length ~ctx value
  | Margin_inline_end -> Values.normalize_length ~ctx value
  | Margin_inline_start -> Values.normalize_length ~ctx value
  | Margin_left -> Values.normalize_length ~ctx value
  | Margin_right -> Values.normalize_length ~ctx value
  | Margin_top -> Values.normalize_length ~ctx value
  | Margin_bottom -> Values.normalize_length ~ctx value
  | Margin_block_start -> Values.normalize_length ~ctx value
  | Margin_block_end -> Values.normalize_length ~ctx value
  | Column_gap -> Values.normalize_length ~ctx value
  | Row_gap -> Values.normalize_length ~ctx value
  | Text_underline_offset -> Values.normalize_length ~ctx value
  | Letter_spacing -> Values.normalize_length ~ctx value
  | Border_top_left_radius -> Values.normalize_length ~ctx value
  | Border_top_right_radius -> Values.normalize_length ~ctx value
  | Border_bottom_left_radius -> Values.normalize_length ~ctx value
  | Border_bottom_right_radius -> Values.normalize_length ~ctx value
  | Border_start_start_radius -> Values.normalize_length ~ctx value
  | Border_start_end_radius -> Values.normalize_length ~ctx value
  | Border_end_start_radius -> Values.normalize_length ~ctx value
  | Border_end_end_radius -> Values.normalize_length ~ctx value
  | Outline_width -> Values.normalize_length ~ctx value
  | Outline_offset -> Values.normalize_length ~ctx value
  | Line_height_step -> Values.normalize_length ~ctx value
  | Perspective -> Values.normalize_length ~ctx value
  | Word_spacing -> Values.normalize_length ~ctx value
  | Text_decoration_thickness -> Values.normalize_length ~ctx value
  | Stroke_width -> Values.normalize_length ~ctx value
  | Scroll_margin_top -> Values.normalize_length ~ctx value
  | Scroll_margin_right -> Values.normalize_length ~ctx value
  | Scroll_margin_bottom -> Values.normalize_length ~ctx value
  | Scroll_margin_left -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_start -> Values.normalize_length ~ctx value
  | Scroll_margin_inline_end -> Values.normalize_length ~ctx value
  | Scroll_margin_block_start -> Values.normalize_length ~ctx value
  | Scroll_margin_block_end -> Values.normalize_length ~ctx value
  | Scroll_padding_top -> Values.normalize_length ~ctx value
  | Scroll_padding_right -> Values.normalize_length ~ctx value
  | Scroll_padding_bottom -> Values.normalize_length ~ctx value
  | Scroll_padding_left -> Values.normalize_length ~ctx value
  | Scroll_padding_inline_start -> Values.normalize_length ~ctx value
  | Scroll_padding_inline_end -> Values.normalize_length ~ctx value
  | Scroll_padding_block_start -> Values.normalize_length ~ctx value
  | Scroll_padding_block_end -> Values.normalize_length ~ctx value
  | Padding -> map_preserve (Values.normalize_length ~ctx) value
  | Padding_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Padding_block -> map_preserve (Values.normalize_length ~ctx) value
  | Margin -> map_preserve (Values.normalize_length ~ctx) value
  | Margin_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Margin_block -> map_preserve (Values.normalize_length ~ctx) value
  | Inset -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_inline_end -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block_start -> map_preserve (Values.normalize_length ~ctx) value
  | Inset_block_end -> map_preserve (Values.normalize_length ~ctx) value
  | Top -> map_preserve (Values.normalize_length ~ctx) value
  | Right -> map_preserve (Values.normalize_length ~ctx) value
  | Bottom -> map_preserve (Values.normalize_length ~ctx) value
  | Left -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_margin_block -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_padding -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_padding_inline -> map_preserve (Values.normalize_length ~ctx) value
  | Scroll_padding_block -> map_preserve (Values.normalize_length ~ctx) value
  | Custom_property _ -> (
      match value with
      | Custom_value ({ value = Tokens components; _ } as r) ->
          let components' =
            components
            |> canonicalize_custom_colors_components ~lossless
            |> canonicalize_math_whitespace_components
          in
          if components' == components then value
          else Custom_value { r with value = Tokens components' }
      | Custom_value _ -> value)
  | Opacity -> normalize_opacity value
  | Line_height -> normalize_line_height value
  | Vertical_align -> normalize_vertical_align value
  | Border_width -> map_preserve normalize_border_width value
  | Border_top_width -> normalize_border_width value
  | Border_right_width -> normalize_border_width value
  | Border_bottom_width -> normalize_border_width value
  | Border_left_width -> normalize_border_width value
  | Border_inline_start_width -> normalize_border_width value
  | Border_inline_end_width -> normalize_border_width value
  | Border_block_start_width -> normalize_border_width value
  | Border_block_end_width -> normalize_border_width value
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
      Typed
        {
          kind = Color;
          value = Values.normalize_color ~lossless ~in_feature_query:false value;
        }
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
           (canonicalize_custom_colors_components ~lossless components))
  | Typed _ as other -> other

let pp_property_value : type a. (a property * a) Pp.t =
 fun ctx (prop, value) ->
  let pp pp_a = pp_a ctx value in
  match prop with
  | Custom_property _ -> pp pp_custom_property
  | Unknown_property _ ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified value
        else Parser.string_of_components value
      in
      Pp.string ctx rendered
  | All -> pp pp_css_wide
  | Background_color -> pp pp_color
  | Color -> pp pp_color
  | Border_color -> pp (pp_box_shorthand pp_color)
  | Border_style -> pp pp_border_style
  | Border_top_style -> pp pp_border_style
  | Border_right_style -> pp pp_border_style
  | Border_bottom_style -> pp pp_border_style
  | Border_left_style -> pp pp_border_style
  | Padding -> pp (pp_box_shorthand pp_length)
  | Padding_left -> pp pp_length
  | Padding_right -> pp pp_length
  | Padding_bottom -> pp pp_length
  | Padding_top -> pp pp_length
  | Padding_inline -> pp (pp_box_shorthand pp_length)
  | Padding_inline_start -> pp pp_length
  | Padding_inline_end -> pp pp_length
  | Padding_block -> pp (pp_box_shorthand pp_length)
  | Padding_block_start -> pp pp_length
  | Padding_block_end -> pp pp_length
  | Margin -> pp (pp_box_shorthand pp_length)
  | Margin_inline_end -> pp pp_length
  | Margin_inline_start -> pp pp_length
  | Margin_left -> pp pp_length
  | Margin_right -> pp pp_length
  | Margin_top -> pp pp_length
  | Margin_bottom -> pp pp_length
  | Margin_inline -> pp (Pp.list ~sep:Pp.space pp_length)
  | Margin_block -> pp (Pp.list ~sep:Pp.space pp_length)
  | Margin_block_start -> pp pp_length
  | Margin_block_end -> pp pp_length
  | Gap -> pp pp_gap
  | Column_gap -> pp pp_length
  | Row_gap -> pp pp_length
  | Width -> pp pp_length_percentage
  | Height -> pp pp_length_percentage
  | Min_width -> pp pp_length_min_max
  | Min_height -> pp pp_length_min_max
  | Max_width -> pp pp_length_percentage
  | Max_height -> pp pp_length_percentage
  | Inline_size -> pp pp_length_percentage
  | Min_inline_size -> pp pp_length_min_max
  | Max_inline_size -> pp pp_length_percentage
  | Block_size -> pp pp_length_percentage
  | Min_block_size -> pp pp_length_min_max
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
  | Mix_blend_mode -> pp pp_blend_mode
  | Z_index -> pp pp_z_index
  | Tab_size -> pp pp_tab_size
  | Zoom -> pp pp_zoom
  | Webkit_line_clamp -> pp pp_webkit_line_clamp
  | Webkit_box_orient -> pp pp_webkit_box_orient
  | Inset -> pp (pp_box_shorthand pp_length)
  | Inset_inline -> pp (pp_box_shorthand pp_length)
  | Inset_inline_start -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_inline_end -> pp (Pp.list ~sep:Pp.space pp_length)
  | Inset_block -> pp (pp_box_shorthand pp_length)
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
  | Border_top_left_radius -> pp pp_length
  | Border_top_right_radius -> pp pp_length
  | Border_bottom_left_radius -> pp pp_length
  | Border_bottom_right_radius -> pp pp_length
  | Border_top_color -> pp pp_color
  | Border_right_color -> pp pp_color
  | Border_bottom_color -> pp pp_color
  | Border_left_color -> pp pp_color
  | Border_inline_start_color -> pp pp_color
  | Border_inline_end_color -> pp pp_color
  | Border_inline_color -> pp pp_logical_border_color
  | Border_inline_style -> pp pp_border_style
  | Border_block_style -> pp pp_border_style
  | Border_start_start_radius -> pp pp_length
  | Border_start_end_radius -> pp pp_length
  | Border_end_start_radius -> pp pp_length
  | Border_end_end_radius -> pp pp_length
  | Text_decoration_color -> pp pp_color
  | Webkit_text_decoration_color -> pp pp_color
  | Webkit_tap_highlight_color -> pp pp_color
  | Webkit_text_fill_color -> pp pp_color
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
  | Page_break_before ->
      if Pp.minified ctx then pp_break_value ctx (break_of_page_break value)
      else pp_page_break_value ctx value
  | Page_break_after ->
      if Pp.minified ctx then pp_break_value ctx (break_of_page_break value)
      else pp_page_break_value ctx value
  | Page_break_inside ->
      if Pp.minified ctx then
        pp_break_inside_value ctx (break_inside_of_page_break value)
      else pp_page_break_inside_value ctx value
  | Page_size -> pp pp_page_size
  | Columns -> pp pp_columns_value
  | Column_width -> pp pp_column_width
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
  | View_timeline -> pp pp_timeline_shorthand
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
  | Stroke_width -> pp pp_length
  | Transition -> pp (Pp.list ~sep:Pp.comma pp_transition)
  | Scale -> pp pp_scale
  | Outline -> pp pp_outline
  | Outline_style -> pp pp_outline_style
  | Outline_width -> pp pp_length
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
   typed value types). The minify-time [Optimize.drop_invalid] pass uses this to
   discard the declaration. *)
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
 fun prop value ->
  match prop with
  | Rotate -> invalid_rotate_value value
  | Width -> invalid_length_percentage value
  | Height -> invalid_length_percentage value
  | Min_width -> invalid_length_percentage value
  | Min_height -> invalid_length_percentage value
  | Max_width -> invalid_length_percentage value
  | Max_height -> invalid_length_percentage value
  | Block_size -> invalid_length_percentage value
  | Inline_size -> invalid_length_percentage value
  | Min_block_size -> invalid_length_percentage value
  | Min_inline_size -> invalid_length_percentage value
  | Max_block_size -> invalid_length_percentage value
  | Max_inline_size -> invalid_length_percentage value
  | Clip_path -> invalid_clip_path value
  | Text_indent -> invalid_text_indent_value value
  | Font_family -> ( match value with Invalid _ -> true | _ -> false)
  | _ -> false

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
  | Border_top_left_radius -> Some Length
  | Border_top_right_radius -> Some Length
  | Border_bottom_left_radius -> Some Length
  | Border_bottom_right_radius -> Some Length
  | Border_start_start_radius -> Some Length
  | Border_start_end_radius -> Some Length
  | Border_end_start_radius -> Some Length
  | Border_end_end_radius -> Some Length
  | Outline_width -> Some Length
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
  | Stroke_width -> Some Length
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
  | Accent_color -> Some Color
  | Caret_color -> Some Color
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

let rec read_font_variant_emoji t : font_variant_emoji =
  Cursor.enum_or_var "font-variant-emoji"
    [
      ("normal", (Normal : font_variant_emoji));
      ("text", Text);
      ("emoji", Emoji);
      ("unicode", Unicode);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_font_variant_emoji t))
    t

let rec read_dominant_baseline t : dominant_baseline =
  Cursor.enum_or_var "dominant-baseline"
    [
      ("auto", (Auto : dominant_baseline));
      ("alphabetic", Alphabetic);
      ("ideographic", Ideographic);
      ("mathematical", Mathematical);
      ("central", Central);
      ("middle", Middle);
      ("text-top", Text_top);
      ("text-bottom", Text_bottom);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_dominant_baseline t))
    t

let read_ray_size t : ray_size =
  Cursor.enum "ray size"
    [
      ("closest-side", (Radial Closest_side : ray_size));
      ("closest-corner", Radial Closest_corner);
      ("farthest-side", Radial Farthest_side);
      ("farthest-corner", Radial Farthest_corner);
      ("sides", Sides);
    ]
    t

let read_initial_letter_align_keyword t : initial_letter_align_keyword =
  Cursor.enum "initial-letter-align"
    [
      ("alphabetic", (Alphabetic : initial_letter_align_keyword));
      ("ideographic", Ideographic);
      ("hanging", Hanging);
      ("leading", Leading);
      ("border-box", Border_box);
    ]
    t

let read_font_size_adjust_metric t : font_size_adjust_metric =
  Cursor.enum "font-size-adjust metric"
    [
      ("ex-height", (Ex_height : font_size_adjust_metric));
      ("cap-height", Cap_height);
      ("ch-width", Ch_width);
      ("ic-width", Ic_width);
      ("ic-height", Ic_height);
    ]
    t

let read_animation_range_name t : animation_range_name =
  Cursor.enum "animation-range name"
    [
      ("cover", Cover);
      ("contain", Contain);
      ("entry", Entry);
      ("exit", Exit);
      ("entry-crossing", Entry_crossing);
      ("exit-crossing", Exit_crossing);
    ]
    t

let read_border_image_repeat_keyword t : border_image_repeat_keyword =
  Cursor.enum "border-image-repeat"
    [
      ("stretch", (Stretch : border_image_repeat_keyword));
      ("repeat", Repeat);
      ("round", Round);
      ("space", Space);
    ]
    t

let read_margin_trim_axis t : margin_trim_axis =
  Cursor.enum "margin-trim axis"
    [ ("block", (Block : margin_trim_axis)); ("inline", Inline) ]
    t

let read_margin_trim_edge t : margin_trim_edge =
  Cursor.enum "margin-trim edge"
    [
      ("block-start", (Block_start : margin_trim_edge));
      ("inline-start", Inline_start);
      ("block-end", Block_end);
      ("inline-end", Inline_end);
    ]
    t

let rec read_font_size_adjust t : font_size_adjust =
  let read_non_negative_number t =
    let n = Cursor.number t in
    if n < 0. then Cursor.err_invalid t "font-size-adjust must be non-negative";
    n
  in
  let read_metric_value t =
    let metric = read_font_size_adjust_metric t in
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "from-font" ->
        let _ = Cursor.ident t in
        Metric_from_font metric
    | _ -> Metric_number (metric, read_non_negative_number t)
  in
  Cursor.enum_or_var "font-size-adjust"
    [
      ("none", (None : font_size_adjust));
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_font_size_adjust t))
    ~default:(fun t ->
      match Cursor.peek_ident t with
      | Some _ -> read_metric_value t
      | None -> Number (read_non_negative_number t))
    t

let rec read_initial_letter t : initial_letter =
  let read_number t =
    let size = Cursor.number t in
    if size < 1. then Cursor.err_invalid t "initial-letter size must be >= 1";
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_semicolon t then
      (Size size : initial_letter)
    else
      let sink = Cursor.int t in
      if sink < 1 then Cursor.err_invalid t "initial-letter sink must be >= 1";
      Cursor.ws t;
      Cursor.expect_eof t;
      Size_sink (size, sink)
  in
  Cursor.enum_or_calls "initial-letter"
    [
      ("normal", (Normal : initial_letter));
      ("drop", Drop);
      ("raise", Raise);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_initial_letter t)) ]
    ~default:read_number t

let rec read_initial_letter_align t : initial_letter_align =
  Cursor.enum_or_var "initial-letter-align"
    [
      ("inherit", (Inherit : initial_letter_align));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_initial_letter_align t))
    ~default:(fun t ->
      let keywords =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2
          read_initial_letter_align_keyword t
      in
      let duplicate =
        List.exists
          (fun keyword ->
            List.length (List.filter (( = ) keyword) keywords) > 1)
          keywords
      in
      let valid_pair =
        match keywords with
        | [ _ ] -> true
        | [ first; second ] ->
            (first = Border_box && second <> Border_box)
            || (first <> Border_box && second = Border_box)
        | _ -> false
      in
      if duplicate || not valid_pair then
        Cursor.err_invalid t "initial-letter-align"
      else (Align keywords : initial_letter_align))
    t

let rec read_initial_letter_wrap t : initial_letter_wrap =
  Cursor.enum_or_var "initial-letter-wrap"
    [
      ("none", (None : initial_letter_wrap));
      ("first", First);
      ("all", All);
      ("grid", Grid);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_initial_letter_wrap t))
    ~default:(fun t ->
      (Length (Values.read_length_percentage ~with_keywords:false t)
        : initial_letter_wrap))
    t

let margin_trim_axis_of_ident t = function
  | "block" -> (Block : margin_trim_axis)
  | "inline" -> Inline
  | s -> Cursor.err_invalid t ("invalid margin-trim axis: " ^ s)

let read_margin_trim_axes t : margin_trim option =
  let axes = [ "block"; "inline" ] in
  let rec loop acc =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some s
      when List.mem s axes && not (List.mem (margin_trim_axis_of_ident t s) acc)
      ->
        let _ = Cursor.ident t in
        loop (margin_trim_axis_of_ident t s :: acc)
    | Some s when List.mem s axes ->
        Cursor.err_invalid t
          (String.concat "" [ "duplicate margin-trim axis: "; s ])
    | _ -> List.rev acc
  in
  match loop [] with
  | [] -> None
  | [ Block ] -> Some (Block : margin_trim)
  | [ Inline ] -> Some (Inline : margin_trim)
  | axes -> Some (Axes axes : margin_trim)

let margin_trim_edge_of_ident t = function
  | "block-start" -> (Block_start : margin_trim_edge)
  | "inline-start" -> Inline_start
  | "block-end" -> Block_end
  | "inline-end" -> Inline_end
  | s -> Cursor.err_invalid t ("invalid margin-trim edge: " ^ s)

let read_margin_trim_edges t =
  let edges = [ "block-start"; "inline-start"; "block-end"; "inline-end" ] in
  let rec loop acc =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some s
      when List.mem s edges
           && not (List.mem (margin_trim_edge_of_ident t s) acc) ->
        let _ = Cursor.ident t in
        loop (margin_trim_edge_of_ident t s :: acc)
    | Some s when List.mem s edges ->
        Cursor.err_invalid t
          (String.concat "" [ "duplicate margin-trim edge: "; s ])
    | _ -> List.rev acc
  in
  let chosen = loop [] in
  if chosen = [] then Cursor.err_expected t "margin-trim value"
  else (Edges chosen : margin_trim)

let margin_trim_keywords : (string * margin_trim) list =
  [
    ("none", None);
    ("initial", Initial);
    ("inherit", Inherit);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let rec read_margin_trim t : margin_trim =
  (Cursor.enum_or_var "margin-trim" margin_trim_keywords
     ~var:(fun t -> (Var (Values.read_var read_margin_trim t) : margin_trim))
     ~default:(fun t ->
       match read_margin_trim_axes t with
       | Some value -> value
       | None -> read_margin_trim_edges t)
     t
    : margin_trim)

let read_offset_path_path t =
  Cursor.call "path" t @@ fun inner ->
  Cursor.ws inner;
  match Cursor.string_opt inner with
  | Some path when path <> "" ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Path path : offset_path)
  | Some _ -> Cursor.err_invalid inner "empty offset path"
  | None -> Cursor.err_expected inner "path string"

let read_offset_path_ray_contain inner =
  match Cursor.peek_ident inner with
  | Some "contain" ->
      let _ = Cursor.ident inner in
      true
  | _ -> false

let read_offset_path_ray_position inner =
  match Cursor.peek_ident inner with
  | Some "at" ->
      let _ = Cursor.ident inner in
      Cursor.ws inner;
      Some (read_position_value inner)
  | _ -> None

let read_offset_path_ray t =
  Cursor.call "ray" t @@ fun inner ->
  Cursor.ws inner;
  let angle = Values.read_angle inner in
  Cursor.ws inner;
  let size = Cursor.option read_ray_size inner in
  Cursor.ws inner;
  let contain = read_offset_path_ray_contain inner in
  Cursor.ws inner;
  let position = read_offset_path_ray_position inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  (Ray { angle; size; contain; position } : offset_path)

let read_offset_path_url_function t =
  Cursor.call "url" t @@ fun inner ->
  Cursor.ws inner;
  match Cursor.string_opt inner with
  | Some url when url <> "" ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Url url : offset_path)
  | Some _ -> Cursor.err_invalid inner "empty offset path url"
  | None -> Cursor.err_expected inner "url string"

let read_offset_path_url_token t =
  match Cursor.url_opt t with
  | Some url when url <> "" -> (Url url : offset_path)
  | Some _ -> Cursor.err_invalid t "empty offset path url"
  | None -> Cursor.err_expected t "offset-path"

let rec read_offset_path t : offset_path =
  Cursor.enum_or_calls "offset-path"
    [
      ("none", (None : offset_path));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("path", read_offset_path_path);
        ("ray", read_offset_path_ray);
        ("url", read_offset_path_url_function);
        ( "var",
          fun t -> (Var (Values.read_var read_offset_path t) : offset_path) );
      ]
    ~default:read_offset_path_url_token t

let animation_range_names =
  [ "cover"; "contain"; "entry"; "exit"; "entry-crossing"; "exit-crossing" ]

let read_animation_range_offset t : length_percentage option =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_semicolon t then
    (None : length_percentage option)
  else
    match Cursor.peek_ident t with
    | Some "normal" -> (None : length_percentage option)
    | Some next when List.mem next animation_range_names ->
        (None : length_percentage option)
    | _ -> (Some (Values.read_length_percentage t) : length_percentage option)

let rec read_animation_range_item t : animation_range_item =
  let keywords : (string * animation_range_item) list =
    [
      ("normal", (Normal : animation_range_item));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_item t =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some name when List.mem name animation_range_names ->
        let name : animation_range_name = read_animation_range_name t in
        let lp = read_animation_range_offset t in
        (Named (name, lp) : animation_range_item)
    | _ ->
        let lp = Values.read_length_percentage t in
        Offset lp
  in
  Cursor.enum_or_var "animation-range-item" keywords
    ~var:(fun t ->
      (Var (Values.read_var read_animation_range_item t) : animation_range_item))
    ~default:read_item t

let rec read_animation_range t : animation_range =
  let keywords : (string * animation_range) list =
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_range t =
    let read_single t =
      Cursor.ws t;
      match Cursor.peek_ident t with
      | Some "normal" ->
          let _ = Cursor.ident t in
          (Normal : animation_range_item)
      | Some name when List.mem name animation_range_names ->
          let name : animation_range_name = read_animation_range_name t in
          let lp = read_animation_range_offset t in
          (Named (name, lp) : animation_range_item)
      | _ ->
          let lp = Values.read_length_percentage t in
          Offset lp
    in
    let first = read_single t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_semicolon t then Range (first, None)
    else
      let second = read_single t in
      Range (first, Some second)
  in
  (Cursor.enum_or_var "animation-range" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_animation_range t) : animation_range))
     ~default:read_range t
    : animation_range)

let is_grid_area_ws = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let grid_area_row_cells row =
  let len = String.length row in
  let rec skip_ws i =
    if i < len && is_grid_area_ws row.[i] then skip_ws (i + 1) else i
  in
  let rec take_cell start i =
    if i < len && not (is_grid_area_ws row.[i]) then take_cell start (i + 1)
    else (String.sub row start (i - start), i)
  in
  let rec loop acc i =
    let start = skip_ws i in
    if start >= len then List.rev acc
    else
      let cell, next = take_cell start start in
      loop (cell :: acc) next
  in
  loop [] 0

let grid_area_null_cell cell =
  let len = String.length cell in
  len > 0
  &&
  let rec loop i = i = len || (cell.[i] = '.' && loop (i + 1)) in
  loop 0

(* CSS Grid Layout 2 section 7.3: each row string is a sequence of [.] (null
   cell) tokens or [<custom-ident>] cell names. A [<custom-ident>] starts with a
   letter, [_], or [-]-followed-by-letter, and continues with letters / digits /
   [_] / [-]. *)
let grid_area_ident_cell cell =
  let len = String.length cell in
  let is_start c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'
  in
  let is_continue c = is_start c || (c >= '0' && c <= '9') in
  len > 0
  && is_start cell.[0]
  &&
  let rec loop i = i = len || (is_continue cell.[i] && loop (i + 1)) in
  loop 1

let validate_grid_area_cell t cell =
  if not (grid_area_null_cell cell || grid_area_ident_cell cell) then
    Cursor.err_invalid t ("invalid grid-template-areas cell: " ^ cell)

let validate_grid_area_width t (expected : int option) cells =
  match expected with
  | None -> Some (List.length cells)
  | Some width when List.length cells = width -> expected
  | Some _ -> Cursor.err_invalid t "grid-template-areas rows differ in width"

let grid_area_positions rows =
  rows
  |> List.mapi (fun row cells ->
      cells
      |> List.mapi (fun col cell -> (cell, row, col))
      |> List.filter (fun (cell, _, _) -> not (grid_area_null_cell cell)))
  |> List.flatten

let grid_area_names positions =
  positions
  |> List.fold_left
       (fun names (cell, _, _) ->
         if List.mem cell names then names else cell :: names)
       []

let validate_grid_area_rectangles t rows =
  let positions = grid_area_positions rows in
  let cell_at row col = List.nth (List.nth rows row) col in
  let validate_name name =
    let coords =
      positions
      |> List.filter_map (fun (cell, row, col) ->
          if cell = name then Some (row, col) else None)
    in
    let rows = List.map fst coords in
    let cols = List.map snd coords in
    let min_row = List.fold_left min max_int rows in
    let max_row = List.fold_left max min_int rows in
    let min_col = List.fold_left min max_int cols in
    let max_col = List.fold_left max min_int cols in
    for row = min_row to max_row do
      for col = min_col to max_col do
        if cell_at row col <> name then
          Cursor.err_invalid t
            "grid-template-areas named area is not rectangular"
      done
    done
  in
  List.iter validate_name (grid_area_names positions)

let rec read_border_spacing t : border_spacing =
  let read_numeric_length t =
    let l = read_length ~allow_negative:false t in
    match l with
    | Auto | Size | None | Normal | Fit_content | Content | Contain
    | Max_content | Min_content | From_font | Hairline | Thin | Medium | Thick
    | Stretch ->
        Cursor.err_invalid t "border-spacing requires a <length>"
    | _ -> l
  in
  Cursor.enum_or_var "border-spacing" []
    ~var:(fun t -> Var (Values.read_var read_border_spacing t))
    ~default:(fun t ->
      (Lengths
         (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_numeric_length
            t)
        : border_spacing))
    t

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

let read_property_value_kind (type a) (_ : Cursor.t) : a property_value_kind =
  invalid_arg
    "Properties.read_property_value_kind: property_value_kind is a phantom \
     GADT and cannot be parsed standalone"

let read_position_visibility_condition t : position_visibility_condition =
  Cursor.enum "position-visibility"
    [
      ("anchors-visible", Anchors_visible);
      ("no-overflow", (No_overflow : position_visibility_condition));
    ]
    t

let read_ray t : ray =
  Cursor.call "ray" t @@ fun inner ->
  Cursor.ws inner;
  let angle = Values.read_angle inner in
  Cursor.ws inner;
  let size = Cursor.option read_ray_size inner in
  Cursor.ws inner;
  let contain =
    match Cursor.peek_ident inner with
    | Some "contain" ->
        let _ = Cursor.ident inner in
        true
    | _ -> false
  in
  Cursor.ws inner;
  let position =
    match Cursor.peek_ident inner with
    | Some "at" ->
        let _ = Cursor.ident inner in
        Cursor.ws inner;
        Some (read_position_value inner)
    | _ -> None
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  { angle; size; contain; position }

let read_grid_template_areas_row t width rows rendered =
  Cursor.ws t;
  match Cursor.string_opt t with
  | None -> `Stop
  | Some s ->
      let cells = grid_area_row_cells s in
      if cells = [] then Cursor.err_invalid t "empty grid-template-areas row";
      List.iter (validate_grid_area_cell t) cells;
      let width = validate_grid_area_width t width cells in
      `Continue (width, cells :: rows, ("\"" ^ s ^ "\"") :: rendered)

let read_grid_template_areas_rows t =
  let rec loop width rows rendered =
    match read_grid_template_areas_row t width rows rendered with
    | `Stop ->
        let rows = List.rev rows in
        if rows = [] then Cursor.err_expected t "grid-template-areas row";
        validate_grid_area_rectangles t rows;
        (Areas (String.concat " " (List.rev rendered)) : grid_template_areas)
    | `Continue (width, rows, rendered) -> loop width rows rendered
  in
  loop (None : int option) [] []

let rec read_grid_template_areas t : grid_template_areas =
  Cursor.enum_or_var "grid-template-areas"
    [
      ("none", (No_areas : grid_template_areas));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_grid_template_areas t) : grid_template_areas))
    ~default:read_grid_template_areas_rows t

let border_image_at_end t = Cursor.is_done t || Cursor.peek_semicolon t

let read_border_image_slice_item t : border_image_slice_item =
  match Cursor.percentage_opt t with
  | Some n when n >= 0. -> Pct n
  | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
  | None -> (
      match Cursor.number_opt t with
      | Some n when n >= 0. -> Number n
      | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
      | None -> Cursor.err_expected t "border-image slice")

let read_border_image_slice_value t values has_fill =
  match Cursor.option read_border_image_slice_item t with
  | Some value ->
      if List.length values >= 4 then
        Cursor.err_invalid t "too many border-image slice values";
      `Continue (value :: values, has_fill)
  | None -> `Stop

let read_border_image_slice_step t values has_fill =
  Cursor.ws t;
  if border_image_at_end t || Cursor.peek_delim t = Some '/' then `Stop
  else
    match Cursor.peek_ident t with
    | Some "fill" ->
        if has_fill then
          Cursor.err_invalid t "duplicate border-image fill keyword";
        let _ = Cursor.ident t in
        `Continue (values, true)
    | _ -> read_border_image_slice_value t values has_fill

let read_border_image_slice t : border_image_slice =
  let rec loop values has_fill =
    match read_border_image_slice_step t values has_fill with
    | `Stop -> (values, has_fill)
    | `Continue (values, has_fill) -> loop values has_fill
  in
  let values, has_fill = loop [] false in
  match (List.rev values, has_fill) with
  | [], true -> Cursor.err_invalid t "border-image fill requires slice values"
  | [], false -> Cursor.err_expected t "border-image slice"
  | offsets, fill -> { offsets; fill }

let read_border_image_width_item t : border_image_width_item =
  match Cursor.peek_ident t with
  | Some "auto" ->
      ignore (Cursor.ident t : string);
      Auto
  | _ -> (
      match Cursor.percentage_opt t with
      | Some n when n >= 0. -> Pct n
      | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
      | None -> (
          match Cursor.number_opt t with
          | Some n when n >= 0. -> Number n
          | Some _ ->
              Cursor.err_invalid t "border-image value cannot be negative"
          | None ->
              let len = read_length ~allow_negative:false t in
              Length len))

let read_border_image_outset_item t : border_image_outset_item =
  match Cursor.number_opt t with
  | Some n when n >= 0. -> Number n
  | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
  | None ->
      let len = read_length ~allow_negative:false t in
      Length len

let read_border_image_box_step ~what read_item t acc =
  Cursor.ws t;
  if border_image_at_end t || Cursor.peek_delim t = Some '/' then `Stop
  else
    match Cursor.option read_item t with
    | Some value ->
        if List.length acc >= 4 then
          Cursor.err_invalid t ("too many border-image " ^ what ^ " values");
        `Continue (value :: acc)
    | None -> `Stop

let read_border_image_box_values ~what read_item t =
  let rec loop acc =
    match read_border_image_box_step ~what read_item t acc with
    | `Stop -> List.rev acc
    | `Continue acc -> loop acc
  in
  match loop [] with
  | [] -> Cursor.err_expected t ("border-image " ^ what)
  | values -> values

let read_border_image_repeat_keywords t =
  let first = read_border_image_repeat_keyword t in
  Cursor.ws t;
  match Cursor.option read_border_image_repeat_keyword t with
  | None -> [ first ]
  | Some second -> [ first; second ]

let rec read_border_image_repeat t : border_image_repeat =
  Cursor.enum_or_var "border-image-repeat"
    [
      ("inherit", (Inherit : border_image_repeat));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_repeat t))
    ~default:(fun t -> Repeats (read_border_image_repeat_keywords t))
    t

let rec read_border_image_width t : border_image_width =
  Cursor.enum_or_var "border-image-width"
    [
      ("inherit", (Inherit : border_image_width));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_width t))
    ~default:(fun t ->
      Widths
        (read_border_image_box_values ~what:"width" read_border_image_width_item
           t))
    t

let rec read_border_image_outset t : border_image_outset =
  Cursor.enum_or_var "border-image-outset"
    [
      ("inherit", (Inherit : border_image_outset));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_outset t))
    ~default:(fun t ->
      Outsets
        (read_border_image_box_values ~what:"outset"
           read_border_image_outset_item t))
    t

let read_mask_border_mode t =
  Cursor.enum "mask-border-mode"
    [ ("alpha", (Alpha : mask_border_mode)); ("luminance", Luminance) ]
    t

let read_border_image t : border_image =
  let source = Cursor.option read_background_image t in
  Cursor.ws t;
  (* CSS Masking 1 §6 [mask-border-mode] is in [&&] juxtaposition with the other
     slots, so the keyword may appear after [<source>] (before the slice) or
     after [<repeat>]. Try the early slot first; combine with the trailing slot
     below. *)
  let mode_early = Cursor.option read_mask_border_mode t in
  Cursor.ws t;
  let slice = Cursor.option read_border_image_slice t in
  let width, outset =
    Cursor.ws t;
    if Cursor.slash_opt t then (
      let width =
        Some
          (read_border_image_box_values ~what:"width"
             read_border_image_width_item t)
      in
      Cursor.ws t;
      if Cursor.slash_opt t then
        ( width,
          Some
            (read_border_image_box_values ~what:"outset"
               read_border_image_outset_item t) )
      else (width, None))
    else (None, None)
  in
  Cursor.ws t;
  let repeat = Cursor.option read_border_image_repeat_keywords t in
  Cursor.ws t;
  let mode_late : mask_border_mode option =
    if Option.is_some mode_early then (None : mask_border_mode option)
    else Cursor.option read_mask_border_mode t
  in
  let mode = match mode_early with Some _ -> mode_early | None -> mode_late in
  (match (source, slice) with
  | None, None -> Cursor.err_expected t "border-image source or slice"
  | _ -> ());
  { source; slice; width; outset; repeat; mode }
