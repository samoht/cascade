open Declaration
open Common
open Shorthand_common

(* CSS Backgrounds 3 sec. 2.10: [background] is the shorthand for the eight
   per-layer longhands. Cascade composes when a contiguous run of bg-* longhands
   covers a single layer: every longhand carries a single-layer value, no entry
   uses a CSS-wide keyword or [var()], and all share the same importance. *)
let background_image_singleton :
    Properties.background_image list -> Properties.background_image option =
  function
  | [ img ] -> (
      match img with
      | Initial -> Some (None : Properties.background_image)
      | Inherit | Unset | Revert | Revert_layer | Var _ | List _ -> None
      | _ -> Some img)
  | _ -> None

let background_position_singleton :
    Properties.background_position -> Properties.position_value option =
  function
  | [ pos ] -> Some pos
  | _ -> None

(* CSS Cascade 5 sec. 7.3: [initial] is the property's initial value, which is
   exactly what the shorthand writes to a slot left out, so a longhand spelled
   that way fills its slot. [inherit] and [unset] name the parent's value
   instead and cannot. The initials are CSS Backgrounds 3 sec. 3.1 to 3.10. *)
let bg_color_part : declaration -> Values.color option = function
  | Declaration { property = Background_color; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Transparent : Values.color)
      | v -> Some v)
  | _ -> None

let bg_image_part : declaration -> Properties.background_image option = function
  | Declaration { property = Background_image; value; _ } ->
      background_image_singleton value
  | _ -> None

let bg_repeat_part : declaration -> Properties.background_repeat option =
  function
  | Declaration { property = Background_repeat; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Repeat : Properties.background_repeat)
      | v -> Some v)
  | _ -> None

let bg_position_part : declaration -> Properties.position_value option =
  function
  | Declaration { property = Background_position; value; _ } ->
      background_position_singleton value
  | _ -> None

let bg_size_part : declaration -> Properties.background_size option = function
  | Declaration { property = Background_size; value; _ } -> (
      match value with
      | Inherit | Unset | Revert | Revert_layer | Var _ -> None
      | Initial -> Some (Auto : Properties.background_size)
      | v -> Some v)
  | _ -> None

let bg_attachment_part : declaration -> Properties.background_attachment option
    = function
  | Declaration { property = Background_attachment; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Scroll : Properties.background_attachment)
      | v -> Some v)
  | _ -> None

let bg_origin_part : declaration -> Properties.background_box option = function
  | Declaration { property = Background_origin; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Padding_box : Properties.background_box)
      | v -> Some v)
  | _ -> None

let bg_clip_part : declaration -> Properties.background_box option = function
  | Declaration { property = Background_clip; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Border_box : Properties.background_box)
      | v -> Some v)
  | _ -> None

type bg_part =
  Properties.background_shorthand -> Properties.background_shorthand

type bg_updater = declaration -> bg_part option

let lift_part :
    'a.
    (declaration -> 'a option) ->
    (Properties.background_shorthand -> 'a -> Properties.background_shorthand) ->
    bg_updater =
 fun extract set d ->
  match extract d with Some v -> Some (fun s -> set s v) | None -> None

let bg_updaters : bg_updater list =
  [
    lift_part bg_color_part (fun s v -> { s with color = Some v });
    lift_part bg_image_part (fun s v -> { s with image = Some v });
    lift_part bg_repeat_part (fun s v -> { s with repeat = Some v });
    lift_part bg_position_part (fun s v -> { s with position = Some v });
    lift_part bg_size_part (fun s v -> { s with size = Some v });
    lift_part bg_attachment_part (fun s v -> { s with attachment = Some v });
    lift_part bg_origin_part (fun s v -> { s with origin = Some v });
    lift_part bg_clip_part (fun s v -> { s with clip = Some v });
  ]

let background_part_of (d : declaration) =
  List.find_map (fun f -> f d) bg_updaters

let empty_bg_shorthand : Properties.background_shorthand =
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

(* Default open-world policy: the synthesized [background] shorthand resets
   every absent longhand to its initial, which would shadow a prior cascade
   write the optimizer cannot see (earlier <link>, earlier <style>, bundler
   concatenation, layer outside the file). Cascade composes only when the local
   run is reset-closed -- every reset field has a declaration in the run, so the
   shorthand cannot disturb prior writes. *)
let background_run_is_reset_closed (layer : Properties.background_shorthand) =
  Option.is_some layer.color && Option.is_some layer.image
  && Option.is_some layer.position
  && Option.is_some layer.size
  && Option.is_some layer.repeat
  && Option.is_some layer.attachment
  && Option.is_some layer.origin
  && Option.is_some layer.clip

(* A layer shorthand resets every layer field, so synthesizing it from a run
   silently reverts any same-family longhand sitting before the run (separated
   from it by an unrelated declaration). Even [`Stylesheet] scope, which is
   closed over external CSS only, must not shadow that earlier same-block
   longhand, so composition refuses when one exists. *)
let has_prior_family_longhand part_of idx i =
  let rec aux j =
    if j >= i then false
    else if Rule_index.is_absorbed idx j then aux (j + 1)
    else
      match part_of (Rule_index.decl_at idx j) with
      | Some _ -> true
      | None -> aux (j + 1)
  in
  aux 0

let try_compose_background_at ~ctx idx i =
  let parts, len = take_run_at idx ~part_of:background_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else
      let layer =
        List.fold_left (fun acc (_, f) -> f acc) empty_bg_shorthand parts
      in
      (* [`Fragment] requires the run to cover every reset field; in
         [`Stylesheet] the caller asserts no prior author CSS exists that the
         shorthand could shadow. *)
      let permit =
        match scope ctx with
        | `Stylesheet ->
            not (has_prior_family_longhand background_part_of idx i)
        | `Fragment -> background_run_is_reset_closed layer
      in
      if not permit then None
      else
        let shorthand =
          Declaration.v
            ~important:(is_important (List.hd raw_decls))
            Background
            [ (Shorthand layer : Properties.background) ]
        in
        Some (shorthand, len)

let compose_background_via_index ~ctx idx =
  compose_run_via_index idx ~try_compose:(try_compose_background_at ~ctx)

let compose_background_shorthand ~ctx decls =
  let idx = Rule_index.build (List.map snd decls) in
  compose_background_via_index ~ctx idx;
  List.mapi (fun i d -> (i, d)) (Rule_index.to_list idx)

(* CSS Masking 1 sec. 7.9: [mask] is the layer shorthand for [mask-image] /
   [mask-position] / [mask-size] / [mask-repeat] / [mask-origin] / [mask-clip] /
   [mask-mode] / [mask-composite] (analogous to [background]). Compose a
   contiguous run that carries a [mask-image]; like [border], [mask] resets
   [mask-border] to its initial, so only compose while no [mask-border] precedes
   (the reorder / dead-drop cases are handled separately). Closed-world
   ([`Stylesheet]) only, since the shorthand resets the layer fields the run
   leaves unset. *)
(* The optimizer writes a [-webkit-] twin beside each mask longhand for old
   Safari, so a run reaching the composer carries both spellings of a slot.
   They name one cascade slot and carry one value, so either fills it and the
   run stays contiguous. *)
let mask_image_part : declaration -> Properties.background_image option =
  function
  | Declaration { property = Mask_image; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (None : Properties.background_image)
      | v -> Some v)
  | Declaration { property = Webkit_mask_image; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (None : Properties.background_image)
      | v -> Some v)
  | _ -> None

let mask_repeat_part : declaration -> Properties.background_repeat option =
  function
  | Declaration { property = Mask_repeat; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Repeat : Properties.background_repeat)
      | v -> Some v)
  | Declaration { property = Webkit_mask_repeat; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Repeat : Properties.background_repeat)
      | v -> Some v)
  | _ -> None

let mask_size_part : declaration -> Properties.background_size option = function
  | Declaration { property = Mask_size; value; _ } -> (
      match value with
      | Inherit | Unset | Revert | Revert_layer | Var _ -> None
      | Initial -> Some (Auto : Properties.background_size)
      | v -> Some v)
  | Declaration { property = Webkit_mask_size; value; _ } -> (
      match value with
      | Inherit | Unset | Revert | Revert_layer | Var _ -> None
      | Initial -> Some (Auto : Properties.background_size)
      | v -> Some v)
  | _ -> None

(* The two spellings share a cascade slot, and the CSSOM reports the prefixed
   one when it expands [mask], so either fills the position slot. *)
let mask_position_part : declaration -> Properties.position_value option =
  function
  | Declaration { property = Mask_position; value; _ } ->
      background_position_singleton value
  | Declaration { property = Webkit_mask_position; value; _ } ->
      background_position_singleton value
  | _ -> None

(* The prefixed pair's own vocabulary meets the unprefixed [<coord-box>] on the
   three CSS box names alone, so a bare legacy name or [text] keeps its longhand
   rather than contracting into a slot that cannot spell it. *)
let mask_box_of_webkit :
    Properties.webkit_mask_box -> Properties.mask_box option = function
  | Border_box -> Some Border_box
  | Content_box -> Some Content_box
  | Padding_box -> Some Padding_box
  | Border | Content | Padding | Text | Layers _ | Inherit | Initial | Unset
  | Revert | Revert_layer | Var _ ->
      None

let mask_origin_part : declaration -> Properties.mask_box option = function
  | Declaration { property = Mask_origin; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Border_box : Properties.mask_box)
      | v -> Some v)
  | Declaration { property = Webkit_mask_origin; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Border_box : Properties.mask_box)
      | v -> mask_box_of_webkit v)
  | _ -> None

let mask_clip_part : declaration -> Properties.mask_box option = function
  | Declaration { property = Mask_clip; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Border_box : Properties.mask_box)
      | v -> Some v)
  | Declaration { property = Webkit_mask_clip; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Border_box : Properties.mask_box)
      | v -> mask_box_of_webkit v)
  | _ -> None

let mask_mode_part : declaration -> Properties.mask_mode option = function
  | Declaration { property = Mask_mode; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Match_source : Properties.mask_mode)
      | v -> Some v)
  | _ -> None

let mask_composite_part : declaration -> Properties.mask_composite option =
  function
  | Declaration { property = Mask_composite; value; _ } -> (
      match value with
      | Inherit | Unset -> None
      | Initial -> Some (Add : Properties.mask_composite)
      | v -> Some v)
  | _ -> None

type mask_part = Properties.mask_layer -> Properties.mask_layer
type mask_updater = declaration -> mask_part option

let lift_mask :
    'a.
    (declaration -> 'a option) ->
    (Properties.mask_layer -> 'a -> Properties.mask_layer) ->
    mask_updater =
 fun extract set d ->
  match extract d with Some v -> Some (fun s -> set s v) | None -> None

let mask_updaters : mask_updater list =
  [
    lift_mask mask_image_part (fun s v -> { s with image = Some v });
    lift_mask mask_repeat_part (fun s v -> { s with repeat = Some v });
    lift_mask mask_size_part (fun s v -> { s with size = Some v });
    lift_mask mask_position_part (fun s v -> { s with position = Some v });
    lift_mask mask_origin_part (fun s v -> { s with origin = Some v });
    lift_mask mask_clip_part (fun s v -> { s with clip = Some v });
    lift_mask mask_mode_part (fun s v -> { s with mode = Some v });
    lift_mask mask_composite_part (fun s v -> { s with composite = Some v });
  ]

let mask_part_of (d : declaration) = List.find_map (fun f -> f d) mask_updaters

let empty_mask_layer : Properties.mask_layer =
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

(* Same reading as [background]: the layer shorthand resets every slot, so a run
   naming all eight is the same declaration whatever else the sheet holds. A
   shorter run leaves the rest to the reset, which only the whole-sheet scope
   can judge. *)
let mask_run_is_reset_closed (layer : Properties.mask_layer) =
  Option.is_some layer.image
  && Option.is_some layer.position
  && Option.is_some layer.size
  && Option.is_some layer.repeat
  && Option.is_some layer.origin
  && Option.is_some layer.clip && Option.is_some layer.mode
  && Option.is_some layer.composite

let try_compose_mask_at ~ctx idx i =
  let parts, len = take_run_at idx ~part_of:mask_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else
      let layer =
        List.fold_left (fun acc (_, f) -> f acc) empty_mask_layer parts
      in
      let permit =
        match scope ctx with
        | `Stylesheet -> not (has_prior_family_longhand mask_part_of idx i)
        | `Fragment -> mask_run_is_reset_closed layer
      in
      if not permit then None
      else if layer.image = None then None
      else
        let shorthand =
          Declaration.v
            ~important:(is_important (List.hd raw_decls))
            Mask
            (Layer layer : Properties.mask)
        in
        Some (shorthand, len)

let is_mask_border_decl d =
  match snd d with
  | Declaration { property = Mask_border; _ } -> true
  | _ -> false

let is_mask_image_decl d =
  match snd d with
  | Declaration { property = Mask_image; _ } -> true
  | _ -> false

let is_mask_layer_longhand d =
  match snd d with
  | Declaration { property = Mask_image; _ }
  | Declaration { property = Mask_repeat; _ }
  | Declaration { property = Mask_size; _ }
  | Declaration { property = Mask_position; _ }
  | Declaration { property = Mask_origin; _ }
  | Declaration { property = Mask_clip; _ }
  | Declaration { property = Mask_mode; _ }
  | Declaration { property = Mask_composite; _ } ->
      true
  | _ -> false

(* The [mask] shorthand resets [mask-border] (CSS Masking 1 sec. 7.9), so a
   [mask-border] that precedes a run of mask layer longhands can move after them
   without changing any property's cascade: the synthesised [mask] resets
   [mask-border], but the now-trailing [mask-border] overrides that reset back.
   Only swap when the following run carries a [mask-image] (what the composer
   needs), so the move happens exactly where it enables composition. Mirror of
   [reorder_border_image_before_border]. *)
let reorder_mask_border_before_mask decls =
  let rec span pred acc = function
    | d :: rest when pred d -> span pred (d :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  let rec go acc = function
    | [] -> List.rev acc
    | d :: _ as l when is_mask_border_decl d ->
        let border_block, rest1 = span is_mask_border_decl [] l in
        let long_block, rest2 = span is_mask_layer_longhand [] rest1 in
        if List.exists is_mask_image_decl long_block then
          go
            (List.rev_append border_block (List.rev_append long_block acc))
            rest2
        else go (List.rev_append border_block acc) rest1
    | d :: rest -> go (d :: acc) rest
  in
  go [] decls

let is_mask_border_decl_raw d =
  match d with Declaration { property = Mask_border; _ } -> true | _ -> false

let compose_mask_via_index ~ctx idx =
  let n = Rule_index.length idx in
  let i = ref 0 in
  let seen_mask_border = ref false in
  while !i < n do
    if Rule_index.is_absorbed idx !i then incr i
    else if !seen_mask_border then (
      let d = Rule_index.decl_at idx !i in
      if is_mask_border_decl_raw d then ();
      incr i)
    else
      let step_over () =
        let d = Rule_index.decl_at idx !i in
        if is_mask_border_decl_raw d then seen_mask_border := true;
        incr i
      in
      match try_compose_mask_at ~ctx idx !i with
      | Some (shorthand, k) ->
          let absorbed = List.init k (fun j -> !i + j) in
          if Rule_index.absorb idx ~at:!i ~absorbed ~shorthand then i := !i + k
          else step_over ()
      | None -> step_over ()
  done
