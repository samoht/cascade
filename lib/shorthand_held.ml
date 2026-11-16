open Declaration
open Common
open Shorthand_coverage

type tr_slot = Property | Duration | Timing | Delay | Behavior

let tr_slots = [ Property; Duration; Timing; Delay; Behavior ]

let tr_slot_bit : tr_slot -> int = function
  | Property -> 1
  | Duration -> 2
  | Timing -> 4
  | Delay -> 8
  | Behavior -> 16

let tr_bits slots = List.fold_left (fun acc s -> acc lor tr_slot_bit s) 0 slots
let all_tr_bits = tr_bits tr_slots

let tr_zero_duration : Values.duration -> bool = function
  | S 0. | Ms 0. -> true
  | _ -> false

let tr_layer_holds_slot (s : Properties.transition_shorthand) : tr_slot -> bool
    = function
  | Property -> ( match s.property with All -> false | _ -> true)
  | Duration -> (
      match s.duration with None -> false | Some d -> not (tr_zero_duration d))
  | Timing -> (
      match s.timing_function with None | Some Ease -> false | Some _ -> true)
  | Delay -> (
      match s.delay with None -> false | Some d -> not (tr_zero_duration d))
  | Behavior -> (
      match s.behavior with None | Some Normal -> false | Some _ -> true)

(* Slots [d] leaves holding something other than the slot initial: [all] for the
   property, [0s] for the two times, [ease] for the easing, [normal] for the
   behaviour. A slot written back to its initial is not at risk from a shorthand
   that resets it to the same thing. *)
let tr_overwritten_slots d =
  let bit slot cond = if cond then tr_slot_bit slot else 0 in
  match unwrap_theme_guard d with
  | Declaration { property = Transition_property; value; _ } ->
      bit Property (match value with [ All ] -> false | _ -> true)
  | Declaration { property = Transition_duration; value; _ } ->
      bit Duration (not (tr_zero_duration value))
  | Declaration { property = Transition_timing_function; value; _ } ->
      bit Timing (match value with Ease -> false | _ -> true)
  | Declaration { property = Transition_delay; value; _ } ->
      bit Delay (not (tr_zero_duration value))
  | Declaration { property = Transition_behavior; value; _ } ->
      bit Behavior (match value with Normal -> false | _ -> true)
  | Declaration { property = Transition; value = [ Shorthand s ]; _ } ->
      tr_bits (List.filter (tr_layer_holds_slot s) tr_slots)
  | Declaration { property = Transition; _ } -> all_tr_bits
  (* CSS Cascade 5 sec. 3.2: [all] writes every longhand. No transition longhand
     inherits, so [initial] and [unset] both leave the initials. *)
  | Declaration { property = All; value = Initial | Unset; _ } -> 0
  | Declaration { property = All; _ } -> all_tr_bits
  | _ -> 0

(* CSS Animations 2 sec. 4.11: the slots [animation] resets. The bits sit above
   the transition ones so a single [held] set can carry both families. *)
type an_slot =
  | Name
  | Duration
  | Timing
  | Delay
  | Iteration
  | Direction
  | Fill
  | Play
  | Timeline
  | Range_start
  | Range_end

let an_slots =
  [
    Name;
    Duration;
    Timing;
    Delay;
    Iteration;
    Direction;
    Fill;
    Play;
    Timeline;
    Range_start;
    Range_end;
  ]

let an_slot_bit : an_slot -> int = function
  | Name -> 32
  | Duration -> 64
  | Timing -> 128
  | Delay -> 256
  | Iteration -> 512
  | Direction -> 1024
  | Fill -> 2048
  | Play -> 4096
  | Timeline -> 8192
  | Range_start -> 65536
  | Range_end -> 131072

let an_bits slots = List.fold_left (fun acc s -> acc lor an_slot_bit s) 0 slots
let all_an_bits = an_bits an_slots

(* [text-decoration] resets the thickness slot, the one longhand of the family a
   contraction can leave out, so it needs one bit of its own beside the
   transition and animation sets. *)
let td_thickness_bit = 16384

(* CSS Backgrounds 3 sec. 3.4: [border] resets the whole [border-image] family,
   and each longhand of it gets a bit rather than the family sharing one: a rule
   holding the slice says nothing about a neighbour holding the source, and one
   bit makes the first cancel the second in [held_outside]. *)
let bi_source_bit = 1 lsl 18
let bi_slice_bit = 1 lsl 19
let bi_width_bit = 1 lsl 20
let bi_outset_bit = 1 lsl 21
let bi_repeat_bit = 1 lsl 22

let border_image_bit =
  bi_source_bit lor bi_slice_bit lor bi_width_bit lor bi_outset_bit
  lor bi_repeat_bit

let bi_overwritten_slots d =
  match unwrap_theme_guard d with
  | Declaration { property = Border_image_source; _ } -> bi_source_bit
  | Declaration { property = Border_image_slice; _ } -> bi_slice_bit
  | Declaration { property = Border_image_width; _ } -> bi_width_bit
  | Declaration { property = Border_image_outset; _ } -> bi_outset_bit
  | Declaration { property = Border_image_repeat; _ } -> bi_repeat_bit
  | Declaration { property = Border_image; _ }
  | Declaration { property = Border; _ } ->
      border_image_bit
  | Declaration { property = All; value = Initial | Unset; _ } -> 0
  | Declaration { property = All; _ } -> border_image_bit
  | _ -> 0

let td_overwritten_slots d =
  match unwrap_theme_guard d with
  | Declaration { property = Text_decoration_thickness; _ } -> td_thickness_bit
  | Declaration { property = Text_decoration; _ } -> td_thickness_bit
  | Declaration { property = All; value = Initial | Unset; _ } -> 0
  | Declaration { property = All; _ } -> td_thickness_bit
  | _ -> 0

(* Which slots a declaration leaves holding something. Cruder than the
   transition answer, which compares against each slot initial: this one says
   "set at all". It is only ever consulted for slots MISSING from the run being
   composed, so the extra caution costs a contraction, never a cascade. *)
let an_overwritten_slots d =
  match unwrap_theme_guard d with
  | Declaration { property = Animation_name; _ } -> an_slot_bit Name
  | Declaration { property = Animation_duration; _ } -> an_slot_bit Duration
  | Declaration { property = Animation_timing_function; _ } ->
      an_slot_bit Timing
  | Declaration { property = Animation_delay; _ } -> an_slot_bit Delay
  | Declaration { property = Animation_iteration_count; _ } ->
      an_slot_bit Iteration
  | Declaration { property = Animation_direction; _ } -> an_slot_bit Direction
  | Declaration { property = Animation_fill_mode; _ } -> an_slot_bit Fill
  | Declaration { property = Animation_play_state; _ } -> an_slot_bit Play
  | Declaration { property = Animation_timeline; _ } -> an_slot_bit Timeline
  | Declaration { property = Animation_range_start; _ } ->
      an_slot_bit Range_start
  | Declaration { property = Animation_range_end; _ } -> an_slot_bit Range_end
  | Declaration { property = Animation_range; _ } ->
      an_slot_bit Range_start lor an_slot_bit Range_end
  | Declaration { property = Animation; _ } -> all_an_bits
  (* CSS Cascade 5 sec. 3.2: [all] writes every longhand, and no animation
     longhand inherits, so [initial] and [unset] both leave the initials. *)
  | Declaration { property = All; value = Initial | Unset; _ } -> 0
  | Declaration { property = All; _ } -> all_an_bits
  | _ -> 0

(* Slots a set of declarations leaves holding something other than the slot
   initial, split by importance. Composition reads one rule, so a holder in
   another rule of the same run reaches the scan below only through this. *)
type held = { normal_slots : int; important_slots : int }

let held_none = { normal_slots = 0; important_slots = 0 }
let held_nothing h = Int.equal (h.normal_slots lor h.important_slots) 0

let held_add held decls =
  List.fold_left
    (fun held d ->
      let slots =
        tr_overwritten_slots d lor an_overwritten_slots d
        lor td_overwritten_slots d lor bi_overwritten_slots d
      in
      if Int.equal slots 0 then held
      else if is_important d then
        { held with important_slots = held.important_slots lor slots }
      else { held with normal_slots = held.normal_slots lor slots })
    held decls

(* What the neighbours hold. The rule being composed is judged by the scan
   below, which reads position as well as value, so a slot it holds itself is
   dropped here rather than answered twice and less precisely. *)
let held_outside held decls =
  if held_nothing held then held
  else
    let own = held_add held_none decls in
    {
      normal_slots = held.normal_slots land lnot own.normal_slots;
      important_slots = held.important_slots land lnot own.important_slots;
    }

(* Whether something that reaches the same element holds a slot the run leaves
   out. The composed shorthand resets every such slot, so composing would drop
   that value. An important declaration outranks a non-important shorthand
   whatever the order, so it is only at risk when the run is important too. *)
let reset_hazard ~slots_of idx i ~held ~missing ~important =
  (not (Int.equal missing 0))
  &&
  let outside =
    if important then held.normal_slots lor held.important_slots
    else held.normal_slots
  in
  (not (Int.equal (missing land outside) 0))
  ||
  let rec scan j =
    j >= 0
    &&
    let d = Rule_index.decl_at idx j in
    (important || not (is_important d))
    && not (Int.equal (slots_of d land missing) 0)
    || scan (j - 1)
  in
  scan (i - 1)

let tr_written_slots d =
  match unwrap_theme_guard d with
  | Declaration { property = Transition_property; _ } -> tr_slot_bit Property
  | Declaration { property = Transition_duration; _ } -> tr_slot_bit Duration
  | Declaration { property = Transition_timing_function; _ } ->
      tr_slot_bit Timing
  | Declaration { property = Transition_delay; _ } -> tr_slot_bit Delay
  | Declaration { property = Transition_behavior; _ } -> tr_slot_bit Behavior
  | Declaration { property = Transition; _ } | Declaration { property = All; _ }
    ->
      all_tr_bits
  | _ -> 0

let an_written_slots d =
  match unwrap_theme_guard d with
  | Declaration { property = Animation_range; _ } ->
      an_slot_bit Range_start lor an_slot_bit Range_end
  | Declaration { property = All; _ } -> all_an_bits
  | d -> an_overwritten_slots d

let tr_reset_hazard = reset_hazard ~slots_of:tr_overwritten_slots
let td_reset_hazard = reset_hazard ~slots_of:td_overwritten_slots
let bi_reset_hazard = reset_hazard ~slots_of:bi_overwritten_slots
