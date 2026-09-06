(* css-align-3: box alignment. align-content/items/self,
   justify-content/items/self and the gap shorthand, with the shared safe/unsafe
   and flattened-baseline sub-readers.

   place-content/place-items/place-self live in [Prop_grid], which composes the
   printers and readers below.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Values
open Properties_intf
open Prop_common

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
        ("left", Safe_left);
        ("right", Safe_right);
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
      | Some row, Some col when Values.equal_length row col ->
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
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Stretch -> Pp.string ctx "stretch"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* Gap shorthand parser *)
(* CSS Box Alignment 3 sec. 8.3: each half is a [<'row-gap'>], which is
   [normal | <length-percentage [0,inf]>]. The keyword is read here rather than
   taken from the length grammar, which does not carry it. *)
let read_gap_half t =
  let len =
    Cursor.enum "gap" [ ("normal", (Normal : length)) ] ~default:read_length t
  in
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

let rec read_gap t : gap =
  Cursor.enum_or_whole_value_var "gap"
    [
      ("inherit", (Inherit : gap));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_gap t))
    ~default:(fun t ->
      let first_length = read_gap_half t in
      Cursor.ws t;
      let second_length = Cursor.option read_gap_half t in
      match second_length with
      | Some col_gap ->
          (Lengths { row_gap = Some first_length; column_gap = Some col_gap }
            : gap)
      | None ->
          (Lengths
             { row_gap = Some first_length; column_gap = Some first_length }
            : gap))
    t
