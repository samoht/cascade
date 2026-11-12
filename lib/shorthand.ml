(** Declaration deduplication and shorthand composition. *)

open Shorthand_common
open Shorthand_box
open Shorthand_pair
open Shorthand_held
open Shorthand_dedup
open Shorthand_border
open Shorthand_text
open Shorthand_layout
open Shorthand_timing
open Shorthand_layer

let is_intentionally_duplicated = Shorthand_coverage.is_intentionally_duplicated
let declaration_covers = Shorthand_coverage.declaration_covers
let declarations_overlap = Shorthand_coverage.declarations_overlap

type overlap_key = Shorthand_coverage.overlap_key

let overlap_key_equal = Shorthand_coverage.overlap_key_equal
let overlap_key_hash = Shorthand_coverage.overlap_key_hash
let overlap_key_compare = Shorthand_coverage.overlap_key_compare
let broad_overlap_key = Shorthand_coverage.broad_overlap_key
let overlap_keys_intersect = Shorthand_coverage.overlap_keys_intersect
let declaration_overlap_keys = Shorthand_coverage.declaration_overlap_keys

let declarations_overlap_with_keys =
  Shorthand_coverage.declarations_overlap_with_keys

let declaration_is_broad = Shorthand_coverage.declaration_is_broad
let custom_property_name = Shorthand_coverage.custom_property_name
let same_property = Shorthand_dedup.same_property
let same_value = Shorthand_dedup.same_value
let declarations_commute = Shorthand_dedup.declarations_commute

let drop_redundant_decoration_color_aliases =
  Shorthand_dedup.drop_redundant_decoration_color_aliases

let is_all_declaration = Shorthand_dedup.is_all_declaration
let transitioned_in_rule = Shorthand_timing.transitioned_in_rule
let merge_box_shorthand_longhands = Shorthand_box.merge_box_shorthand_longhands
let merge_overflow_longhands = Shorthand_box.merge_overflow_longhands

type held = Shorthand_held.held

let held_none = Shorthand_held.held_none
let held_add = Shorthand_held.held_add
let duplicate_buggy_properties decls = decls

(* Run every index-based composer against the same Rule_index so we pay one
   [build] + [to_list] per rule for the whole group. *)
let compose_index_group_a ~ctx kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_box_via_index ~ctx idx;
  compose_pair_via_index ~ctx idx;
  compose_outline_via_index idx;
  compose_view_timeline_via_index idx;
  compose_font_synthesis_via_index idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* Second index group runs after the font-reset reorder. Font + list-style +
   flex + text-decoration + border all share the same index. *)
let compose_index_group_b ~ctx ~held kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_font_via_index ~ctx idx;
  compose_list_style_via_index idx;
  compose_flex_via_index idx;
  compose_text_decoration_via_index ~held idx;
  compose_offset_via_index idx;
  compose_columns_via_index idx;
  compose_font_variant_via_index idx;
  compose_grid_via_index idx;
  compose_grid_template_via_index idx;
  compose_border_via_index
    ~bi_hazard:(fun i ~important ->
      bi_reset_hazard idx i ~held ~missing:border_image_bit ~important)
    idx;
  compose_line_via_index idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* Third index group runs at the very end: mask + transition + animation share
   one index. *)
let compose_index_group_c ~ctx ~held kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_mask_via_index ~ctx idx;
  compose_transition_via_index ~ctx ~held idx;
  compose_animation_via_index ~ctx ~held idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

let compose_border_whole_step ~held ~ctx kept =
  let idx = Rule_index.build (List.map snd kept) in
  compose_border_whole_via_index
    ~bi_hazard:(fun i ~important ->
      bi_reset_hazard idx i ~held ~missing:border_image_bit ~important)
    ~ctx idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

let compose_shorthands ?(held = held_none) ~ctx kept =
  kept |> compose_index_group_a ~ctx |> reorder_font_resets_before_font
  |> compose_index_group_b ~ctx ~held
  |> reorder_border_image_before_border
  |> compose_border_whole_step ~held ~ctx
  |> drop_bimg_shadowed_by_border
  |> compose_border_image_shorthand ~ctx
  |> compose_background_shorthand ~ctx
  |> reorder_mask_border_before_mask
  |> compose_index_group_c ~ctx ~held
  |> fun kept ->
  merge_box_shorthand_longhands kept kept |> merge_overflow_longhands

let deduplicate_declarations_with ?(held = held_none) ~ctx ?(merge_box = true)
    props =
  let held = held_outside held props in
  let props = drop_longhands_after_covering_shorthand props in
  let indexed_props = List.mapi (fun i decl -> (i, decl)) props in
  let kept =
    List.rev
      (List.fold_left
         (deduplicate_step ?targets:(Ctx.judge ctx))
         [] indexed_props)
  in
  let kept =
    let kept = if merge_box then compose_shorthands ~held ~ctx kept else kept in
    let kept = drop_vendor_aliases ~ctx kept in
    List.map (fun (_, decl) -> decl) kept
  in
  (* Composition writes shorthands the input did not carry, so the longhands one
     of them now restates are only visible on a second pass. *)
  let kept = drop_longhands_after_covering_shorthand kept in
  let result = duplicate_buggy_properties kept in
  (* Each pipeline step keeps untouched declarations, so [preserve_list]
     restores the input list when every declaration is physically unchanged -
     callers detect a no-op by identity (the factoring fixpoint relies on
     it). *)
  preserve_list props result

let deduplicate_declarations ?scope props =
  deduplicate_declarations_with ~ctx:(Ctx.of_scope scope) props
