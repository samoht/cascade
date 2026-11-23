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
let overlap_keys_broad = Shorthand_coverage.overlap_keys_broad
let overlap_keys_intersect = Shorthand_coverage.overlap_keys_intersect
let declaration_overlap_keys = Shorthand_coverage.declaration_overlap_keys
let declaration_slot_keys = Shorthand_coverage.declaration_slot_keys
let footprint_family = Shorthand_coverage.footprint_family

let declarations_overlap_with_keys =
  Shorthand_coverage.declarations_overlap_with_keys

let declarations_conflict_with_keys =
  Shorthand_dedup.declarations_conflict_with_keys

let declaration_is_broad = Shorthand_coverage.declaration_is_broad
let custom_property_name = Shorthand_coverage.custom_property_name
let same_value = Shorthand_dedup.same_value
let declarations_commute = Shorthand_dedup.declarations_commute
let implied_longhands = Shorthand_dedup.implied_longhands

let drop_redundant_decoration_color_aliases =
  Shorthand_dedup.drop_redundant_decoration_color_aliases

let as_aliased_property = Shorthand_dedup.as_aliased_property
let read_by_no_target = Shorthand_dedup.read_by_no_target
let is_all_declaration = Shorthand_dedup.is_all_declaration
let merge_box_shorthand_longhands = Shorthand_box.merge_box_shorthand_longhands
let merge_overflow_longhands = Shorthand_box.merge_overflow_longhands

type held = Shorthand_held.held

let held_none = Shorthand_held.held_none
let held_add = Shorthand_held.held_add

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

(* CSS Backgrounds 3 sec. 4.4: [border] resets [border-image], which no run of
   its other longhands writes. In [`Fragment] scope a sheet cascade does not see
   may hold it, so the reset is a hazard there unless the rule writes every
   [border-image] slot again after the run; under [`Stylesheet]
   {!Shorthand_common.resets_held} asks the sheet. *)
let border_image_hazard ~ctx ~held idx i ~important =
  (match scope ctx with
    | `Stylesheet -> false
    | `Fragment ->
        not (bi_rewritten_after idx i ~missing:border_image_bit ~important))
  || bi_reset_hazard idx i ~held ~missing:border_image_bit ~important

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
  compose_border_via_index ~ctx
    ~bi_hazard:(border_image_hazard ~ctx ~held idx)
    idx;
  compose_line_via_index idx;
  compose_border_sides_via_index ~ctx
    ~bi_hazard:(border_image_hazard ~ctx ~held idx)
    idx;
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
    ~bi_hazard:(border_image_hazard ~ctx ~held idx)
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

let rec composition_family = function
  | Declaration.Declaration { property; _ } ->
      Properties.composition_family property
  | Theme_guarded { decl; _ } -> composition_family decl

(* Whether two declarations of [kept] are of one composition family, [seen]
   holding the families of those before. *)
let rec shares_family seen = function
  | [] -> false
  | (_, decl) :: rest -> (
      match composition_family decl with
      | None -> shares_family seen rest
      | Some family ->
          let bit = 1 lsl Properties.family_rank family in
          seen land bit <> 0 || shares_family (seen lor bit) rest)

(* What follows the reduction of [kept]: composition, when [merge_box], then the
   vendor aliases and the longhands the shorthands now restate. *)
let compose_reduced ~held ~ctx ~merge_box props kept =
  let kept =
    (* Composition reads a family's declarations together, so a body with at
       most one declaration per family composes to itself in either scope, as
       one declaration on its own does; the tests of that ask every value the
       grammar inventory models, and every pair of families. *)
    (* A shorthand composition writes can override an earlier declaration of
       the same property whole, which only the deduplication sees; what that
       drops can let another composition through. Each round drops a
       declaration, so this ends. *)
    let rec compose kept =
      match kept with
      | _ :: _ :: _ when merge_box && shares_family 0 kept ->
          let composed = compose_shorthands ~held ~ctx kept in
          if List.equal (fun (_, a) (_, b) -> a == b) composed kept then kept
          else
            let deduplicated =
              List.rev
                (List.fold_left
                   (deduplicate_step ~floors:(Ctx.floors ctx)
                      ~judged:(Option.is_some (Ctx.judge ctx)))
                   [] composed)
            in
            if List.length deduplicated < List.length composed then
              compose deduplicated
            else composed
      | _ -> kept
    in
    let kept = compose kept in
    let kept = drop_vendor_aliases ~ctx kept in
    List.map (fun (_, decl) -> decl) kept
  in
  (* Composition writes shorthands the input did not carry, so the longhands one
     of them now restates are only visible on a second pass. *)
  let kept =
    drop_longhands_after_covering_shorthand ~enforce_spec:(Ctx.enforce_spec ctx)
      kept
  in
  (* Each pipeline step keeps untouched declarations, so [preserve_list]
     restores the input list when every declaration is physically unchanged -
     callers detect a no-op by identity (the factoring fixpoint relies on
     it). *)
  preserve_list props kept

let deduplicate_declarations_with ?(held = held_none) ~ctx ?(merge_box = true)
    props =
  let held = held_outside held props in
  let props =
    drop_longhands_after_covering_shorthand ~enforce_spec:(Ctx.enforce_spec ctx)
      props
  in
  let indexed_props = List.mapi (fun i decl -> (i, decl)) props in
  let kept =
    List.rev
      (List.fold_left
         (deduplicate_step ~floors:(Ctx.floors ctx)
            ~judged:(Option.is_some (Ctx.judge ctx)))
         [] indexed_props)
  in
  compose_reduced ~held ~ctx ~merge_box props kept

let compose_deduplicated ?(held = held_none) ~ctx props =
  let held = held_outside held props in
  compose_reduced ~held ~ctx ~merge_box:true props
    (List.mapi (fun i decl -> (i, decl)) props)

let deduplicate_declarations ?scope props =
  deduplicate_declarations_with ~ctx:(Ctx.of_scope scope) props
