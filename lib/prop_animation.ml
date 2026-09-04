(* css-animations-2, css-transitions-2, scroll-animations-1 and
   view-transitions-2: the animation and transition shorthands and their
   longhands, easing functions, scroll and view timelines, animation-range,
   view-transition-name / -class and overlay.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common

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
  | Name name -> pp_ident ctx name
  | Ambiguous name -> pp_ident ctx name
  | Quoted name ->
      (* CSS Animations 1 sec. 3: [<keyframes-name>] excludes [none], the
         CSS-wide keywords, and [default]. A source [animation-name: "none"]
         therefore can't refer to a real [@keyframes none] - it's invalid input
         that browsers tolerate. Minified output drops the quotes so the value
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

let split_ws_words s =
  s |> String.split_on_char ' '
  |> List.filter (fun word -> String.length word > 0)

let canonical_scroll_timeline_args args =
  let words = List.map String.lowercase_ascii_preserve (split_ws_words args) in
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
  | Name name -> pp_ident ctx name
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
    when equal_animation_range_name start_name end_name
         && Values.equal_length_percentage start_offset
              (Pct 0. : length_percentage)
         && Values.equal_length_percentage end_offset
              (Pct 100. : length_percentage) ->
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
  | Name name -> pp_ident ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_view_transition_class : view_transition_class Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_view_transition_class ctx v
  | None -> Pp.string ctx "none"
  | Classes classes -> Pp.list ~sep:Pp.space pp_ident ctx classes
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

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
  | Names names -> Pp.list ~sep:Pp.comma pp_ident ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_timeline_shorthand_item : timeline_shorthand_item Pp.t =
 fun ctx { name; axis } ->
  pp_ident ctx name;
  match axis with
  | None -> ()
  | Some axis ->
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

let pp_view_timeline_shorthand_item : view_timeline_shorthand_item Pp.t =
 fun ctx { name; axis; inset } ->
  pp_ident ctx name;
  Option.iter
    (fun axis ->
      Pp.space ctx ();
      pp_timeline_axis ctx axis)
    axis;
  Option.iter
    (fun inset ->
      Pp.space ctx ();
      pp_timeline_inset ctx inset)
    inset

let rec pp_view_timeline_shorthand : view_timeline_shorthand Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Timelines items ->
      Pp.list ~sep:Pp.comma pp_view_timeline_shorthand_item ctx items
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_view_timeline_shorthand ctx v

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

let rec read_animation_timeline (t : Cursor.t) : animation_timeline =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      (Var (Values.read_var read_animation_timeline t) : animation_timeline)
  | Some (Component.Func fn) when not fn.node.terminated ->
      Cursor.err_invalid t
        (String.concat "" [ "unterminated function "; fn.node.name; "(...)" ])
  | Some (Component.Func fn)
    when String.lowercase_ascii_preserve fn.node.name = "scroll" ->
      let _ = Cursor.next t in
      Scroll (Parser.string_of_components fn.node.arguments)
  | Some (Component.Func fn)
    when String.lowercase_ascii_preserve fn.node.name = "view" ->
      let _ = Cursor.next t in
      View (Parser.string_of_components fn.node.arguments)
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
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid t "timeline name";
  Cursor.ws t;
  let axis = Cursor.option read_timeline_axis t in
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
      (Timelines
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_timeline_shorthand_item
            t)
        : timeline_shorthand))
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

let read_view_timeline_shorthand_item t : view_timeline_shorthand_item =
  (* Scroll-driven Animations 1 sec. 3.4.4: the name is followed by
     [<'view-timeline-axis'> || <'view-timeline-inset'>]?, so try each missing
     slot until neither consumes input. *)
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid t "timeline name";
  let axis = ref Option.None in
  let inset = ref Option.None in
  let try_axis () =
    if !axis <> Option.None then false
    else
      match Cursor.option read_timeline_axis t with
      | Some value ->
          axis := Some value;
          true
      | None -> false
  in
  let try_inset () =
    if !inset <> Option.None then false
    else
      match Cursor.option read_timeline_inset t with
      | Some value ->
          inset := Some value;
          true
      | None -> false
  in
  let rec loop () =
    Cursor.ws t;
    if try_axis () || try_inset () then loop ()
  in
  loop ();
  { name; axis = !axis; inset = !inset }

let rec read_view_timeline_shorthand t : view_timeline_shorthand =
  Cursor.enum_or_var "view-timeline"
    [
      ("none", (None : view_timeline_shorthand));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_view_timeline_shorthand t))
    ~default:(fun t ->
      Timelines
        (Cursor.list ~sep:Cursor.comma ~at_least:1
           read_view_timeline_shorthand_item t))
    t

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
      let v = Cursor.ident t in
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

  let is_default_timing = function Ease -> true | _ -> false

  let is_timing : timing_function option -> bool = function
    | Some Ease | None -> false
    | Some tf -> not (is_default_timing tf)

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

  let has_non_defaults (anim : animation_shorthand) =
    is_duration anim.duration
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
    match (anim.duration, anim.delay) with
    | Some d, Some delay when is_zero_duration d && is_duration (Some delay) ->
        Some d
    | _ -> (
        match anim.duration with
        | Some d when not (is_zero_duration d) -> Some d
        | _ -> None)

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

(* The animation reader fills every slot with its longhand initial, so only the
   easing needs canonicalising here: its keyword and curve spellings are the
   same node question [normalize_timing_function] answers. *)
let normalize_animation_shorthand (a : animation_shorthand) :
    animation_shorthand =
  let duration = option_map_preserve Values.normalize_duration a.duration in
  let delay = option_map_preserve Values.normalize_duration a.delay in
  let timing_function =
    option_map_preserve normalize_timing_function a.timing_function
  in
  if
    option_is_phys_same duration a.duration
    && option_is_phys_same delay a.delay
    && option_is_phys_same timing_function a.timing_function
  then a
  else { a with duration; timing_function; delay }

let normalize_animation : animation -> animation = function
  | Shorthand a as value ->
      let a' = normalize_animation_shorthand a in
      if a' == a then value else Shorthand a'
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

let transition_shorthand ?(property = (All : transition_property_value))
    ?duration ?timing_function ?delay ?behavior () : transition =
  Shorthand { property; duration; timing_function; delay; behavior }

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

let is_animation_range_name name =
  List.mem (String.lowercase_ascii_preserve name) Keyframe.timeline_range_names

let read_animation_range_offset t : length_percentage option =
  Cursor.ws t;
  if Cursor.is_done t then (None : length_percentage option)
  else
    match Option.map String.lowercase_ascii_preserve (Cursor.peek_ident t) with
    | Some "normal" -> (None : length_percentage option)
    | Some next when List.mem next Keyframe.timeline_range_names ->
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
    | Some name when is_animation_range_name name ->
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
      match
        Option.map String.lowercase_ascii_preserve (Cursor.peek_ident t)
      with
      | Some "normal" ->
          let _ = Cursor.ident t in
          (Normal : animation_range_item)
      | Some name when List.mem name Keyframe.timeline_range_names ->
          let name : animation_range_name = read_animation_range_name t in
          let lp = read_animation_range_offset t in
          (Named (name, lp) : animation_range_item)
      | _ ->
          let lp = Values.read_length_percentage t in
          Offset lp
    in
    let first = read_single t in
    Cursor.ws t;
    if Cursor.is_done t then Range (first, None)
    else
      let second = read_single t in
      Range (first, Some second)
  in
  (Cursor.enum_or_var "animation-range" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_animation_range t) : animation_range))
     ~default:read_range t
    : animation_range)
