(* CSS Easing 1 and CSS Transitions 2: easing functions, the transition
   shorthand and its longhands, and overlay. *)

open Common
open Values
open Properties_intf
open Prop_common

let rec pp_steps_direction : steps_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_steps_direction ctx v
  | Jump_start -> Pp.string ctx "jump-start"
  | Jump_end -> Pp.string ctx "jump-end"
  | Jump_none -> Pp.string ctx "jump-none"
  | Jump_both -> Pp.string ctx "jump-both"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"

let pp_timing_float ctx f =
  if Float.is_nan f then Pp.nan_value ctx ""
  else Pp.string ctx (Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f)

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

(* CSS Easing 1 (ED) sec. 2.2 defines [ease] as [cubic-bezier(0.25, 0.1, 0.25,
   1)], [ease-in] as [cubic-bezier(0.42, 0, 1, 1)], [ease-out] as
   [cubic-bezier(0, 0, 0.58, 1)] and [ease-in-out] as [cubic-bezier(0.42, 0,
   0.58, 1)]; [cubic-bezier(0, 0, 1, 1)] is the identity curve the [linear]
   keyword names. sec. 2.3 computes [step-start] to [steps(1, start)] and
   [step-end] to [steps(1, end)], reads [start] as [jump-start] and [end] as
   [jump-end], and assumes [end] when the step position is left out. Each pair
   is one easing under two spellings, and the keyword is the shorter one. *)
let rec normalize_timing_function : timing_function -> timing_function =
 fun value ->
  match value with
  | Cubic_bezier (0.25, 0.1, 0.25, 1.0) -> Ease
  | Cubic_bezier (0.42, 0.0, 1.0, 1.0) -> Ease_in
  | Cubic_bezier (0.0, 0.0, 0.58, 1.0) -> Ease_out
  | Cubic_bezier (0.42, 0.0, 0.58, 1.0) -> Ease_in_out
  | Cubic_bezier (0.0, 0.0, 1.0, 1.0) -> Linear
  | Steps (1, (Option.None | Some (Jump_end | End))) -> Step_end
  | Steps (1, Some (Jump_start | Start)) -> Step_start
  | Timing_functions timings ->
      let normalized = map_preserve normalize_timing_function timings in
      if normalized == timings then value else Timing_functions normalized
  | _ -> value

let rec pp_transition_property_value : transition_property_value Pp.t =
 fun ctx -> function
  | All -> Pp.string ctx "all"
  | None -> Pp.string ctx "none"
  | Property s -> pp_ident ctx s
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
  | Behaviors l -> Pp.list ~sep:Pp.comma pp_transition_behavior ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

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

let transition_duration_is_zero : duration -> bool = function
  | S 0. | Ms 0. -> true
  | _ -> false

let pp_transition_shorthand : transition_shorthand Pp.t =
 fun ctx { property; duration; timing_function; delay; behavior } ->
  let slot pp v =
    Pp.space ctx ();
    pp ctx v
  in
  pp_transition_property_value ctx property;
  (* CSS Transitions 1 (ED) sec. 2.5: "the first value that can be parsed as a
     time is assigned to the transition-duration, and the second value that can
     be parsed as a time is assigned to transition-delay". The delay is only
     reachable behind a duration, so a value carrying a delay and no duration
     writes the duration initial [0s] out to hold the slot. *)
  (match (duration, delay) with
  | Option.Some d, _ -> slot pp_duration d
  | Option.None, Option.Some _ -> slot Pp.string "0s"
  | Option.None, Option.None -> ());
  Option.iter (slot pp_timing_function) timing_function;
  Option.iter (slot pp_duration) delay;
  Option.iter (slot pp_transition_behavior) behavior

(* CSS Transitions 1 (ED) sec. 2.5 assembles a [<single-transition>] out of
   components that each fall back to their longhand initial when left out: [0s]
   for the duration (sec. 2.2), [ease] for the easing (sec. 2.3), [0s] for the
   delay (sec. 2.4), and [normal] for the Transitions 2 behaviour. Writing an
   initial out says what leaving it out says, and the shorter spelling is the
   canonical node. A zero duration goes with the rest: where the grammar still
   needs the slot to reach a delay, the printer writes it back. *)
let normalize_transition_shorthand (s : transition_shorthand) :
    transition_shorthand =
  let drop_zero d =
    match d with
    | Option.Some d when transition_duration_is_zero d -> Option.None
    | d -> d
  in
  let duration =
    drop_zero (option_map_preserve Values.normalize_duration s.duration)
  in
  let delay =
    drop_zero (option_map_preserve Values.normalize_duration s.delay)
  in
  let timing_function =
    match option_map_preserve normalize_timing_function s.timing_function with
    | Option.Some Ease -> Option.None
    | tf -> tf
  in
  let behavior =
    match s.behavior with Option.Some Normal -> Option.None | b -> b
  in
  if
    option_is_phys_same duration s.duration
    && option_is_phys_same delay s.delay
    && option_is_phys_same timing_function s.timing_function
    && option_is_phys_same behavior s.behavior
  then s
  else { s with duration; timing_function; delay; behavior }

let normalize_transition : transition -> transition = function
  | Shorthand s as value ->
      let s' = normalize_transition_shorthand s in
      if s' == s then value else Shorthand s'
  | value -> value

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
        let n = Cursor.int t in
        let kind =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_steps_direction t)
            t
        in
        (match kind with
        | Some Jump_none when n < 2 ->
            Cursor.err t "steps() with jump-none requires at least two steps"
        | _ when n < 1 -> Cursor.err t "steps() requires a positive step count"
        | _ -> ());
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
  match Cursor.list ~sep:Cursor.comma ~at_least:0 read_one t with
  | [] -> Cursor.err_expected t "time value"
  | [ value ] -> value
  | values -> Durations values

let rec read_transition_property_value t : transition_property_value =
  let read_var t : transition_property_value =
    Var (read_var read_transition_property_value t)
  in
  let read_property_ident t =
    let name = Cursor.ident ~keep_case:true t in
    (* CSS Transitions 1 sec. 2.1 gives [transition-property] a
       [<custom-ident>], and CSS Values 4 sec. 3.2 excludes [default] and the
       CSS-wide keywords from that production and nothing else. What the
       [transition] shorthand can tell apart in a slot is the shorthand reader's
       question, not this one's. *)
    if String.equal (String.lowercase_ascii name) "default" then
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
  (* CSS Transitions 1 sec. 2.1: the exclusion clause names none, inherit and
     initial as what a list of more than one may not hold. [all] is an ordinary
     <single-transition-property> and combines with the rest. *)
  let singleton_only : transition_property_value -> bool = function
    | None | Initial | Inherit | Unset | Revert | Revert_layer -> true
    | All | Property _ | Var _ -> false
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

(* CSS Transitions 2 sec. 2 spells the property [<transition-behavior-value>#],
   one behaviour per transition. The single reader stays the [<single-
   transition>] slot of the [transition] shorthand. *)
let read_transition_behavior_list t : transition_behavior =
  match
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_transition_behavior t
  with
  | [ one ] -> one
  | many -> Behaviors many

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
  | Some (Component.Preserved { kind = Token.Ident _; _ }) -> true
  | Some (Component.Func { node = { name; _ }; _ }) ->
      String.lowercase_ascii_preserve name = "var"
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

(* CSS Transitions 1 (ED) sec. 2.5 assigns the first <time> of a
   <single-transition> to transition-duration and the second to
   transition-delay. Only the duration is [0s,inf]: sec. 2.4 lets a delay be
   negative, starting the transition partway through, so the second slot reads
   the wider grammar. *)
let read_transition_time_part parts t =
  match parts.times with
  | [] -> read_transition_part t read_duration (fun v -> parts.times <- [ v ])
  | [ _ ] ->
      read_transition_part t read_time (fun v ->
          parts.times <- v :: parts.times)
  | _ :: _ :: _ -> false

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
  let snap = Cursor.save t in
  let value =
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
  in
  (* CSS Transitions 1 sec. 3: [none] is a [<single-transition-property>], so it
     is the whole item only when the item ends there. Anything else after it
     belongs to the same [<single-transition>]. *)
  match value with
  | None ->
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_comma t then value
      else (
        Cursor.restore t snap;
        (Shorthand (read_transition_shorthand t) : transition))
  | _ -> value

let read_transitions t : transition list =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_transition t

let transition_shorthand ?(property = (All : transition_property_value))
    ?duration ?timing_function ?delay ?behavior () : transition =
  Shorthand { property; duration; timing_function; delay; behavior }
