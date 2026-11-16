(* CSS Animations 2: the animation shorthand and its animation-* longhands. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_animation_timeline
open Prop_animation_transition

(* Sec. 3.4 gives the count a [0,inf] [<number>], so a static math function on
   it folds like any other, and one folding below the range keeps its wrapper
   rather than becoming a literal the reader refuses. *)
let rec normalize_animation_iteration_count ~ctx :
    animation_iteration_count -> animation_iteration_count =
 fun value ->
  match value with
  | Count n ->
      preserve_if_equal value
        (Count (Values.normalize_number ~ctx ~non_negative:true n))
  | Counts counts ->
      preserve_if_equal value
        (Counts (map_preserve (normalize_animation_iteration_count ~ctx) counts))
  | other -> other

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
  | Count n -> Values.pp_number ctx n
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* The names a [<custom-ident>] may not spell, so the string arm is the only one
   that carries them and the quotes are part of the value. CSS Animations 1 sec.
   3 adds [none] to what CSS Values 4 sec. 4.2 reserves. *)
let keyframes_name_needs_quotes name =
  Cursor.is_reserved_custom_ident ~reserved:[ "none" ] name

let rec pp_animation_name : animation_name Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Name name -> pp_ident ctx name
  | Ambiguous name -> pp_ident ctx name
  | Quoted name ->
      (* CSS Animations 1 sec. 3: a [<keyframes-name>] is a [<custom-ident>] or
         a [<string>], and only the ident arm excludes [none], [default] and the
         CSS-wide keywords. Unquoting one of those names writes a different
         declaration, and in a list it writes no declaration at all. *)
      if Pp.minified ctx && not (keyframes_name_needs_quotes name) then
        Pp.string ctx name
      else Pp.quoted_string ctx name
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

(* Sec. 3.4 gives the count a [<number [0,inf]>]. A [calc()] holds no value to
   compare, so only a literal is turned away here. Each count is one component
   of a comma list, and the whole-value number reader refuses anything after the
   number it read, so the component is handed to it on its own. *)
let read_animation_count_number t : animation_iteration_count =
  match Cursor.peek t with
  | Some (Component.Func _ as component) ->
      let _ = Cursor.next t in
      Count (Values.read_number (Cursor.of_components [ component ]))
  | _ ->
      let n, unit = Cursor.number_with_unit t in
      (match unit with
      | Some u ->
          Cursor.err_invalid t
            ("animation-iteration-count must be unitless, got: " ^ u)
      | None ->
          if n < 0. then
            Cursor.err_invalid t "animation-iteration-count cannot be negative");
      Count (Num n)

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
        | None -> Name (Cursor.custom_ident "keyframes name" t))
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
      duration = Some Auto;
      (* CSS Animations 2 sec. 4.1: auto, not 0s *)
      timing_function = Some Ease;
      (* CSS default: ease *)
      delay = Some (S 0.0);
      (* CSS default: 0s *)
      iteration_count = Some (Count (Num 1.0));
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

  let apply_iteration t state acc ic keyword =
    if !(state.iteration_seen) then
      set_string_name t "animation-iteration-count" state.name_seen acc keyword
    else (
      state.iteration_seen := true;
      { acc with iteration_count = Some ic })

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

  let apply_component t state (acc : animation_shorthand) (component, keyword) =
    state.component_seen := true;
    let had_name = !(state.name_seen) in
    let acc =
      match component with
      | Name name -> apply_name t state acc name
      | Duration d -> apply_duration t state acc d
      | Timing_function tf -> apply_timing t state acc tf
      | Iteration_count ic -> apply_iteration t state acc ic keyword
      | Direction dir -> apply_direction t state acc dir
      | Fill_mode fm -> apply_fill t state acc fm
      | Play_state ps -> apply_play t state acc ps
      | Timeline tl -> apply_timeline t state acc tl
    in
    match (had_name, keyword, acc.name) with
    | false, Some name, Some (Ambiguous _) ->
        (* Property keywords are case-insensitive; keyframe names are not. *)
        { acc with name = Some (Ambiguous name) }
    | _ -> acc

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
      let v = Cursor.custom_ident "keyframes name" t in
      if Option.is_some (animation_shorthand_kind (String.lowercase_ascii v))
      then
        (* This identifier is for another property, not animation-name *)
        Cursor.err t
          ("'" ^ v ^ "' is a reserved keyword for animation properties")
      else Name (Some (Name v))
    in
    let keyword = Cursor.peek_ident t in
    let component =
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
    in
    (component, keyword)

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

  (* Check if a timing function prints with a trailing ')'. *)
  let rec ends_with_paren = function
    | Cubic_bezier _ | Steps _ | Linear_function _ | Var _ -> true
    | Timing_functions [] -> false
    | Timing_functions values -> ends_with_paren (List.hd (List.rev values))
    | Inherit | Initial | Unset | Revert | Revert_layer -> false
    | Linear | Ease | Ease_in | Ease_out | Ease_in_out | Step_start | Step_end
      ->
        false

  let pp_timing = pp_timing_function

  let is_duration : duration option -> bool = function
    | Some d when not (is_zero_duration d) -> true
    | _ -> false

  (* CSS Animations 2 sec. 4.1 makes [auto] the initial duration, so an explicit
     [0s] is a value of its own and has to print. The delay's initial is still
     [0s], which is what [is_duration] answers for. *)
  let is_animation_duration : duration option -> bool = function
    | Option.None | Some Auto -> false
    | Some _ -> true

  let is_default_timing = function Ease -> true | _ -> false

  let is_timing : timing_function option -> bool = function
    | Some Ease | None -> false
    | Some tf -> not (is_default_timing tf)

  (* [calc(1)] is the initial count written the long way, and CSS Values 4 sec.
     10.12 gives it the same computed value, so the slot answers for it as it
     already answers for the bare [1] the shorthand drops. Reading it as a value
     of its own left [animation: calc(1)] emitting [animation: 1], which is
     every slot at its initial and so reads back as [animation: none]. *)
  let is_default_count : animation_iteration_count -> bool = function
    | Count (Num 1.) | Count (Calc (Num 1.)) -> true
    | _ -> false

  let is_iteration : animation_iteration_count option -> bool = function
    | Some c -> not (is_default_count c)
    | None -> false

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

  let has_non_defaults (anim : animation_shorthand) =
    is_animation_duration anim.duration
    || is_timing anim.timing_function
    || is_duration anim.delay
    || is_iteration anim.iteration_count
    || is_direction anim.direction
    || is_fill_mode anim.fill_mode
    || is_play_state anim.play_state
    || is_timeline anim.timeline

  let ambiguous_name_kind (anim : animation_shorthand) =
    match anim.name with
    | Some (Ambiguous name | Name name) ->
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
  let bare_ambiguous_safe (anim : animation_shorthand) =
    match ambiguous_name_kind anim with
    | None -> false
    | Some Timing -> (
        match anim.timing_function with
        | Some tf -> not (is_default_timing tf)
        | None -> false)
    | Some Iteration -> is_iteration anim.iteration_count
    | Some Direction -> is_direction anim.direction
    | Some Fill -> is_fill_mode anim.fill_mode
    | Some Play -> is_play_state anim.play_state
    | Some Timeline -> is_timeline anim.timeline

  let duration (anim : animation_shorthand) : duration option =
    if is_animation_duration anim.duration then anim.duration else Option.None

  let timing ?(quote_name = false) (anim : animation_shorthand) :
      timing_function option =
    match (anim.timing_function, effective_ambiguous_kind ~quote_name anim) with
    | (Some Ease | None), Some Timing -> Some Ease
    | Some tf, _ when is_default_timing tf -> None
    | None, _ -> None
    | Some t, _ -> Some t

  let delay (anim : animation_shorthand) : duration option =
    match anim.delay with
    | Some d when not (is_zero_duration d) -> Some d
    | _ -> None

  let iteration ?(quote_name = false) (anim : animation_shorthand) :
      animation_iteration_count option =
    match (anim.iteration_count, effective_ambiguous_kind ~quote_name anim) with
    | Some c, Some Iteration when is_default_count c -> Some (Count (Num 1.))
    | None, Some Iteration -> Some (Count (Num 1.))
    | Some c, _ when is_default_count c -> None
    | None, _ -> None
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
  && not (Animation.bare_ambiguous_safe anim)

(* Every other slot holding its initial and the name absent or holding [none]
   leaves the slot printers with nothing to write. What the value declares then
   is the eight initials and nothing else, which is what [animation: none]
   declares (CSS Animations 1 (ED) sec. 4.9); the empty string is not a value
   any parser reads back. A name that is not the initial writes itself. *)
let pp_animation_initial_none ctx (anim : animation_shorthand)
    ~name_is_default_none ~has_any_non_default =
  match (anim.name, name_is_default_none, has_any_non_default) with
  | _, _, true -> ()
  | Option.None, _, false -> Pp.string ctx "none"
  | Option.Some _, true, false -> Pp.string ctx "none"
  | Option.Some _, false, false -> ()

let pp_animation_name_slot ctx state ~quote_ambiguous_name
    (anim : animation_shorthand) =
  if not (animation_name_is_default_none ctx anim.name) then
    Option.iter
      (fun (name : animation_name) ->
        match name with
        | (Ambiguous s | Name s) when quote_ambiguous_name ->
            pp_animation_space_before state ~starts_with_quote:true
              ~ends_with_quote:true Pp.quoted_string ctx s
        | Quoted s ->
            pp_animation_space_before state ~starts_with_quote:true
              ~ends_with_quote:true Pp.quoted_string ctx s
        | _ -> pp_animation_space_before state pp_animation_name ctx name)
      anim.name

let pp_animation_timing_slot ctx state ~quote_ambiguous_name anim =
  match Animation.timing ~quote_name:quote_ambiguous_name anim with
  | Some tf ->
      let ends = Animation.ends_with_paren tf in
      pp_animation_space_before state ~ends_with_paren:ends Animation.pp_timing
        ctx tf
  | None -> ()

let pp_animation_shorthand : animation_shorthand Pp.t =
 fun ctx anim ->
  let state = animation_pp_state () in
  let has_any_non_default = Animation.has_non_defaults anim in
  let name_is_default_none = animation_name_is_default_none ctx anim.name in
  let quote_ambiguous_name = animation_quote_ambiguous_name ctx anim in
  let ambiguous_name_last =
    Animation.ambiguous_name_kind anim <> None && not quote_ambiguous_name
  in
  pp_animation_initial_none ctx anim ~name_is_default_none ~has_any_non_default;
  (* Cascade canonical order puts the animation [name] first: it is the only
     ident-shaped component that survives the rest defaulting away, so leading
     with it makes "single-token" outputs ([animation:slide]) read naturally and
     matches the common minifier convention. *)
  (* An unquoted keyword name must follow the slot it could otherwise fill. *)
  if not ambiguous_name_last then
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
    (Animation.timeline ~quote_name:quote_ambiguous_name anim);
  if ambiguous_name_last then
    pp_animation_name_slot ctx state ~quote_ambiguous_name anim

(* The animation reader fills every slot with its longhand initial, so each slot
   answers here the question its own longhand answers: the easing's keyword and
   curve spellings are one node, and the count carries the [0,inf] range that
   keeps a call wrapped. A slot left out folds only on a second pass over the
   output, which is a pass too late for output that is input. *)
let normalize_animation_shorthand ~ctx (a : animation_shorthand) :
    animation_shorthand =
  let duration = option_map_preserve (Values.normalize_duration ~ctx) a.duration
  and delay = option_map_preserve (Values.normalize_duration ~ctx) a.delay
  and timing_function =
    option_map_preserve normalize_timing_function a.timing_function
  and iteration_count =
    option_map_preserve
      (normalize_animation_iteration_count ~ctx)
      a.iteration_count
  in
  if
    option_is_phys_same duration a.duration
    && option_is_phys_same delay a.delay
    && option_is_phys_same timing_function a.timing_function
    && option_is_phys_same iteration_count a.iteration_count
  then a
  else { a with duration; timing_function; delay; iteration_count }

(* [pp_animation_initial_none] writes [none] for a shorthand holding nothing but
   initials, on the ground CSS Animations 1 (ED) sec. 4.9 gives: what such a
   value declares is the eight initials, which is what [animation:none]
   declares. Fold onto the constructor the printer names, or the two reach one
   minified text through two nodes, which anything keyed on the node reads as
   two values -- the pass dropping a reset-only longhand the shorthand already
   covers included, so one declaration optimised two ways.

   Only a name that is the keyword [none] folds. A quoted ["none"] names a
   keyframes rule rather than the absent name, and the printer equates the two
   under [--minify] alone, so folding it here would change what the unminified
   round-trip says. *)

let animation_is_all_initial (a : animation_shorthand) =
  (not (Animation.has_non_defaults a))
  &&
  match a.name with
  | Option.None | Option.Some (None : animation_name) -> true
  | Option.Some _ -> false

let normalize_animation ~ctx : animation -> animation = function
  | Shorthand a as value ->
      let a' = normalize_animation_shorthand ~ctx a in
      if animation_is_all_initial a' then (None : animation)
      else if a' == a then value
      else Shorthand a'
  | value -> value

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
