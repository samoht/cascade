open Declaration
open Common
open Shorthand_common
open Shorthand_coverage
open Shorthand_held

(* CSS Scroll Animations 1 sec. 5.2: [view-timeline] is [<name> <axis>?
   <inset>?] over its three longhands, with [block] the axis's initial (sec.
   5.1) and [auto] the inset's (sec. 5.3), so a modifier written out at its
   initial names what leaving it out names. The shorthand pairs one name with
   its own modifiers, so a name list has no spelling in it. *)
type view_timeline_part =
  | Name of Properties.timeline_name
  | Axis of Properties.timeline_axis
  | Inset of Properties.timeline_inset

let view_timeline_part_of : declaration -> view_timeline_part option = function
  | Declaration { property = View_timeline_name; value; _ } -> Some (Name value)
  | Declaration { property = View_timeline_axis; value; _ } -> Some (Axis value)
  | Declaration { property = View_timeline_inset; value; _ } ->
      Some (Inset value)
  | _ -> None

let view_timeline_slot = function Name _ -> 0 | Axis _ -> 1 | Inset _ -> 2

let view_timeline_item parts : Properties.view_timeline_shorthand_item option =
  let pick f = List.find_map f parts in
  let name = pick (function Name n -> Some n | _ -> Option.None) in
  let axis =
    pick (function
      | Axis (Block : Properties.timeline_axis) | Axis Initial -> Option.None
      | Axis a -> Some a
      | _ -> Option.None)
  in
  let inset =
    pick (function
      | Inset (Inset (Auto, Option.None) : Properties.timeline_inset)
      | Inset Initial ->
          Option.None
      | Inset i -> Some i
      | _ -> Option.None)
  in
  match (name : Properties.timeline_name option) with
  | Some (Names [ n ]) -> Some { name = n; axis; inset }
  | _ -> Option.None

let view_timeline_of_run raw =
  let parts = List.filter_map view_timeline_part_of raw in
  let slots = List.sort_uniq compare (List.map view_timeline_slot parts) in
  if List.length slots <> 3 then None else view_timeline_item parts

let try_compose_view_timeline_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = [ i; i + 1; i + 2 ] in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if (not (same_importance raw)) || List.exists has_runtime_substitution raw
      then None
      else
        Option.map
          (fun item ->
            Declaration.v
              ~important:(is_important (List.hd raw))
              View_timeline
              (Timelines [ item ] : Properties.view_timeline_shorthand))
          (view_timeline_of_run raw)

let compose_view_timeline_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_view_timeline_at

(* The property a [transition-property] entry names, against the property a
   declaration writes. A shorthand there transitions every longhand it covers,
   so [transition-property: background] reaches [background-color]. A custom
   property is named by its own name, which CSS Variables 1 sec. 2 makes
   case-sensitive; the typed table reads it as an unknown property. A name
   outside that table names no property this can be asked about. *)
let named_property_covers : type a. string -> a Properties.property -> bool =
 fun name target ->
  match target with
  | Properties.Custom_property custom -> String.equal name custom
  | _ -> (
      match property_of_name name with
      | Some (Prop named) -> (
          match Properties.eq_property named target with
          | Some Equal -> true
          | None -> covers_longhand named target)
      | None -> false)

(* CSS Transitions 1 sec. 2.1: [none] transitions nothing, [all] transitions
   everything, and [all] is the initial. A CSS-wide keyword resolves to that
   initial or to a list from outside the sheet, and a [var()] is unread, so both
   read as [all]. *)
let transition_entry_covers : type a.
    Properties.transition_property_value -> a Properties.property -> bool =
 fun entry target ->
  match entry with
  | None -> false
  | Property name -> named_property_covers name target
  | All | Initial | Inherit | Unset | Revert | Revert_layer | Var _ -> true

let transition_layer_covers : type a.
    Properties.transition -> a Properties.property -> bool =
 fun layer target ->
  match layer with
  | None -> false
  | Shorthand s -> transition_entry_covers s.property target
  | Inherit | Initial | Unset | Revert | Revert_layer | Var _ -> true

(* [all] is in because its only values are CSS-wide keywords, and [inherit]
   there brings the parent's transition down onto this element. *)
let declaration_transitions : type a.
    declaration -> a Properties.property -> bool =
 fun decl target ->
  let layers = List.exists (fun l -> transition_layer_covers l target) in
  let entries = List.exists (fun e -> transition_entry_covers e target) in
  match unwrap_theme_guard decl with
  | Declaration { property = Transition; value; _ } -> layers value
  | Declaration { property = Webkit_transition; value; _ } -> layers value
  | Declaration { property = Moz_transition; value; _ } -> layers value
  | Declaration { property = O_transition; value; _ } -> layers value
  | Declaration { property = Transition_property; value; _ } -> entries value
  | Declaration { property = Webkit_transition_property; value; _ } ->
      entries value
  | Declaration { property = Moz_transition_property; value; _ } ->
      entries value
  | Declaration { property = All; _ } -> true
  | _ -> false

let transitioned_in_rule decls decl =
  match unwrap_theme_guard decl with
  | Declaration { property; _ } ->
      List.exists (fun d -> declaration_transitions d property) decls
  | _ -> false

(* CSS Transitions 2 sec. 2.6: [transition] composes from
   [transition-{property,duration,timing-function,delay,behavior}]. Compose when
   a contiguous run covers a single layer (each longhand carries a one-entry
   list), the importance matches, no CSS-wide keyword leaks in, and
   [transition-property] is present (it has no default, so the shorthand needs
   it).

   The shorthand writes all five slots, so a run leaving one out resets that
   slot to its initial. That is a no-op unless something earlier in the rule
   already put another value there, which [tr_reset_hazard] rules out. *)

let transition_property_singleton :
    Properties.transition_property ->
    Properties.transition_property_value option = function
  | [ p ] -> ( match p with Inherit | Initial | Unset -> None | _ -> Some p)
  | _ -> None

let duration_singleton : Values.duration -> Values.duration option = function
  | Durations _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | d -> Some d

let timing_singleton :
    Properties.timing_function -> Properties.timing_function option = function
  | Timing_functions _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | t -> Some t

let tr_property_part :
    declaration -> Properties.transition_property_value option = function
  | Declaration { property = Transition_property; value; _ } ->
      transition_property_singleton value
  | _ -> None

let tr_duration_part : declaration -> Values.duration option = function
  | Declaration { property = Transition_duration; value; _ } ->
      duration_singleton value
  | _ -> None

let tr_timing_part : declaration -> Properties.timing_function option = function
  | Declaration { property = Transition_timing_function; value; _ } ->
      timing_singleton value
  | _ -> None

let tr_delay_part : declaration -> Values.duration option = function
  | Declaration { property = Transition_delay; value; _ } ->
      duration_singleton value
  | _ -> None

(* [transition-behavior] is a [<single-transition>] component, so a run can
   carry it into the shorthand. A CSS-wide keyword has no component spelling,
   and a [var()] there reads back into the property slot, which the reader tries
   first. A [#] list of behaviours spans several transitions and fills no one
   slot. *)
let behavior_singleton :
    Properties.transition_behavior -> Properties.transition_behavior option =
  function
  | Inherit | Initial | Unset | Revert | Revert_layer | Var _ | Behaviors _ ->
      None
  | (Normal | Allow_discrete) as b -> Some b

let tr_behavior_part : declaration -> Properties.transition_behavior option =
  function
  | Declaration { property = Transition_behavior; value; _ } ->
      behavior_singleton value
  | _ -> None

type tr_part =
  Properties.transition_shorthand -> Properties.transition_shorthand

type tr_updater = declaration -> (tr_slot * tr_part) option

let lift_tr_part :
    'a.
    tr_slot ->
    (declaration -> 'a option) ->
    (Properties.transition_shorthand -> 'a -> Properties.transition_shorthand) ->
    tr_updater =
 fun slot extract set d ->
  match extract d with Some v -> Some (slot, fun s -> set s v) | None -> None

let tr_updaters : tr_updater list =
  [
    lift_tr_part Property tr_property_part (fun s v -> { s with property = v });
    lift_tr_part Duration tr_duration_part (fun s v ->
        { s with duration = Some v });
    lift_tr_part Timing tr_timing_part (fun s v ->
        { s with timing_function = Some v });
    lift_tr_part Delay tr_delay_part (fun s v -> { s with delay = Some v });
    lift_tr_part Behavior tr_behavior_part (fun s v ->
        { s with behavior = Some v });
  ]

let transition_part_of (d : declaration) =
  List.find_map (fun f -> f d) tr_updaters

let empty_tr_shorthand : Properties.transition_shorthand =
  {
    property = (All : Properties.transition_property_value);
    duration = None;
    timing_function = None;
    delay = None;
    behavior = None;
  }

let has_transition_property_decl raw_decls =
  List.exists
    (fun d ->
      match d with
      | Declaration { property = Transition_property; _ } -> true
      | _ -> false)
    raw_decls

let try_compose_transition_at ~allow_partial ~held idx i =
  let parts, len = take_run_at idx ~part_of:transition_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else if not (has_transition_property_decl raw_decls) then None
    else
      let important = is_important (List.hd raw_decls) in
      let written =
        List.fold_left
          (fun acc (_, (slot, _)) -> acc lor tr_slot_bit slot)
          0 parts
      in
      let missing = all_tr_bits land lnot written in
      let unanswered =
        missing
        land lnot
               (slots_written_after ~slots_of:tr_written_slots idx
                  ~from:(i + len))
      in
      if
        (unanswered <> 0 && not allow_partial)
        || tr_reset_hazard idx i ~held ~missing ~important
      then None
      else
        let layer =
          List.fold_left (fun acc (_, (_, f)) -> f acc) empty_tr_shorthand parts
        in
        let shorthand =
          Declaration.v ~important Transition
            [ (Shorthand layer : Properties.transition) ]
        in
        Some (shorthand, len)

let compose_transition_via_index ~ctx ~held idx =
  let allow_partial = scope ctx = `Stylesheet in
  compose_run_via_index idx
    ~try_compose:(try_compose_transition_at ~allow_partial ~held)

(* CSS Animations 1 sec. 3.1: [animation] composes from the per-layer animation
   longhands. Compose when a contiguous run sticks to a single layer (no
   multi-value list constructor leaks in), no CSS-wide keyword appears, and the
   importance matches. *)
let animation_name_singleton :
    Properties.animation_name -> Properties.animation_name option = function
  | Names _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | n -> Some n

let animation_direction_singleton :
    Properties.animation_direction -> Properties.animation_direction option =
  function
  | Directions _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | d -> Some d

let animation_fill_mode_singleton :
    Properties.animation_fill_mode -> Properties.animation_fill_mode option =
  function
  | Fill_modes _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | f -> Some f

let animation_iteration_singleton :
    Properties.animation_iteration_count ->
    Properties.animation_iteration_count option = function
  | Counts _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | c -> Some c

let animation_play_state_singleton :
    Properties.animation_play_state -> Properties.animation_play_state option =
  function
  | States _ -> None
  | Inherit | Initial | Unset | Revert | Revert_layer -> None
  | p -> Some p

let an_name_part : declaration -> Properties.animation_name option = function
  | Declaration { property = Animation_name; value; _ } ->
      animation_name_singleton value
  | _ -> None

let an_duration_part : declaration -> Values.duration option = function
  | Declaration { property = Animation_duration; value; _ } ->
      duration_singleton value
  | _ -> None

let an_timing_part : declaration -> Properties.timing_function option = function
  | Declaration { property = Animation_timing_function; value; _ } ->
      timing_singleton value
  | _ -> None

let an_delay_part : declaration -> Values.duration option = function
  | Declaration { property = Animation_delay; value; _ } ->
      duration_singleton value
  | _ -> None

let an_iteration_part :
    declaration -> Properties.animation_iteration_count option = function
  | Declaration { property = Animation_iteration_count; value; _ } ->
      animation_iteration_singleton value
  | _ -> None

let an_direction_part : declaration -> Properties.animation_direction option =
  function
  | Declaration { property = Animation_direction; value; _ } ->
      animation_direction_singleton value
  | _ -> None

let an_fill_mode_part : declaration -> Properties.animation_fill_mode option =
  function
  | Declaration { property = Animation_fill_mode; value; _ } ->
      animation_fill_mode_singleton value
  | _ -> None

let an_play_state_part : declaration -> Properties.animation_play_state option =
  function
  | Declaration { property = Animation_play_state; value; _ } ->
      animation_play_state_singleton value
  | _ -> None

type an_part = Properties.animation_shorthand -> Properties.animation_shorthand
type an_updater = declaration -> (an_slot * an_part) option

let lift_an_part :
    'a.
    an_slot ->
    (declaration -> 'a option) ->
    (Properties.animation_shorthand -> 'a -> Properties.animation_shorthand) ->
    an_updater =
 fun slot extract set d ->
  match extract d with Some v -> Some (slot, fun s -> set s v) | None -> None

let an_updaters : an_updater list =
  [
    lift_an_part Name an_name_part (fun s v -> { s with name = Some v });
    lift_an_part Duration an_duration_part (fun s v ->
        { s with duration = Some v });
    lift_an_part Timing an_timing_part (fun s v ->
        { s with timing_function = Some v });
    lift_an_part Delay an_delay_part (fun s v -> { s with delay = Some v });
    lift_an_part Iteration an_iteration_part (fun s v ->
        { s with iteration_count = Some v });
    lift_an_part Direction an_direction_part (fun s v ->
        { s with direction = Some v });
    lift_an_part Fill an_fill_mode_part (fun s v ->
        { s with fill_mode = Some v });
    lift_an_part Play an_play_state_part (fun s v ->
        { s with play_state = Some v });
  ]

let animation_part_of (d : declaration) =
  List.find_map (fun f -> f d) an_updaters

let empty_an_shorthand : Properties.animation_shorthand =
  {
    name = None;
    duration = None;
    timing_function = None;
    delay = None;
    iteration_count = None;
    direction = None;
    fill_mode = None;
    play_state = None;
    timeline = None;
  }

(* The same reset hazard [tr_reset_hazard] answers for transition: a slot the
   run does not write is reset to its initial by the shorthand, so composing is
   only safe when nothing else in the cascade holds that slot. *)
let an_reset_hazard = reset_hazard ~slots_of:an_overwritten_slots

let try_compose_animation_at ~allow_partial ~held idx i =
  let parts, len = take_run_at idx ~part_of:animation_part_of i in
  if List.length parts < 2 then None
  else
    let raw_decls = List.map fst parts in
    if not (same_importance raw_decls) then None
    else
      let important = is_important (List.hd raw_decls) in
      let written =
        List.fold_left
          (fun acc (_, (slot, _)) -> acc lor an_slot_bit slot)
          0 parts
      in
      let missing = all_an_bits land lnot written in
      (* A slot the shorthand resets that neither the run nor a later
         declaration writes is one the surrounding CSS may hold, so contracting
         needs the whole graph in view. *)
      let unanswered =
        missing
        land lnot
               (slots_written_after ~slots_of:an_written_slots idx
                  ~from:(i + len))
      in
      if
        (unanswered <> 0 && not allow_partial)
        || an_reset_hazard idx i ~held ~missing ~important
      then None
      else
        let layer =
          List.fold_left (fun acc (_, (_, f)) -> f acc) empty_an_shorthand parts
        in
        let shorthand =
          Declaration.v ~important Animation
            [ (Shorthand layer : Properties.animation) ]
        in
        Some (shorthand, len)

let compose_animation_via_index ~ctx ~held idx =
  let allow_partial = scope ctx = `Stylesheet in
  compose_run_via_index idx
    ~try_compose:(try_compose_animation_at ~allow_partial ~held)
