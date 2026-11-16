(* CSS Scroll Animations 1 and View Transitions 2: animation-timeline,
   animation-range, the scroll and view timelines, timeline-scope and
   view-transition-name / -class. *)

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

let rec normalize_timeline_inset : timeline_inset -> timeline_inset =
 fun value ->
  match value with
  | Inset (a, b) ->
      preserve_if_equal value
        (Inset
           ( normalize_timeline_inset_item a,
             option_map_preserve normalize_timeline_inset_item b ))
  | Insets insets ->
      let insets' = map_preserve normalize_timeline_inset insets in
      if insets' == insets then value else Insets insets'
  | other -> other

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
  | Timelines timelines ->
      Pp.list ~sep:Pp.comma pp_animation_timeline ctx timelines
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
  | Items items -> Pp.list ~sep:Pp.comma pp_animation_range_item ctx items
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
  | Ranges ranges -> Pp.list ~sep:Pp.comma pp_animation_range ctx ranges
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
  | Axes axes -> Pp.list ~sep:Pp.comma pp_timeline_axis ctx axes
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_timeline_ident : timeline_ident Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Name name -> pp_ident ctx name

let rec pp_timeline_scope : timeline_scope Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_scope ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.comma pp_ident ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_timeline_name : timeline_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_timeline_name ctx v
  | Names names -> Pp.list ~sep:Pp.comma pp_timeline_ident ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_timeline_shorthand_item : timeline_shorthand_item Pp.t =
 fun ctx { name; axis } ->
  pp_timeline_ident ctx name;
  match axis with
  | None -> ()
  | Some axis ->
      Pp.space ctx ();
      pp_timeline_axis ctx axis

(* CSS Scroll Animations 1 sec. 4.3: [scroll-timeline] is [<name> <axis>?], and
   an axis left out takes [scroll-timeline-axis]'s initial (sec. 4.2), which is
   [block]. Writing it out names what leaving it out names, so the shorter
   spelling wins. *)
let normalize_timeline_shorthand : timeline_shorthand -> timeline_shorthand =
 fun value ->
  let drop_initial_axis (item : timeline_shorthand_item) =
    match item.axis with
    | Some (Block : timeline_axis) | Some Initial ->
        { item with axis = Option.None }
    | _ -> item
  in
  match value with
  | Timelines items -> Timelines (map_preserve drop_initial_axis items)
  | other -> other

(* Same reading for [view-timeline] (CSS Scroll Animations 1 sec. 5.2), which
   carries an inset slot as well; [auto] is its initial (sec. 5.3). *)
let normalize_view_timeline_shorthand :
    view_timeline_shorthand -> view_timeline_shorthand =
 fun value ->
  let drop_initials (item : view_timeline_shorthand_item) =
    let axis =
      match item.axis with
      | Some (Block : timeline_axis) | Some Initial -> Option.None
      | axis -> axis
    in
    let inset =
      match item.inset with
      | Some (Inset (Auto, Option.None) : timeline_inset) | Some Initial ->
          Option.None
      | inset -> inset
    in
    if axis == item.axis && inset == item.inset then item
    else { item with axis; inset }
  in
  match value with
  | Timelines items -> Timelines (map_preserve drop_initials items)
  | other -> other

let rec pp_timeline_shorthand : timeline_shorthand Pp.t =
 fun ctx -> function
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
  | Insets insets -> Pp.list ~sep:Pp.comma pp_timeline_inset ctx insets
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_view_timeline_shorthand_item : view_timeline_shorthand_item Pp.t =
 fun ctx { name; axis; inset } ->
  pp_timeline_ident ctx name;
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
  | Timelines items ->
      Pp.list ~sep:Pp.comma pp_view_timeline_shorthand_item ctx items
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_view_timeline_shorthand ctx v

(* CSS Animations 2 sec. 5 spells the property [<single-animation-timeline>#],
   one entry per animation. A [var()] and the CSS-wide keywords stand for the
   whole value, which is why they are read inside the entry rather than beside
   the list: an entry that is one of them is the only entry. *)
let rec read_animation_timeline (t : Cursor.t) : animation_timeline =
  match
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_animation_timeline_one t
  with
  | [ timeline ] -> timeline
  | timelines -> Timelines timelines

and read_animation_timeline_one (t : Cursor.t) : animation_timeline =
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
    (Name (Cursor.custom_ident ~reserved:[ "auto" ] "view-transition-name" t)
      : view_transition_name)
  in
  (Cursor.enum_or_var "view-transition-name" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_view_transition_name t)
         : view_transition_name))
     ~default:read_name t
    : view_transition_name)

let read_view_transition_class_ident t =
  Cursor.custom_ident ~reserved:[ "none" ] "view-transition-class ident" t

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

(* Secs. 4.2 and 5.2 spell the axis [[ block | inline | x | y ]#], one entry per
   timeline. A CSS-wide keyword and a [var()] stand for the whole value, so they
   are not list entries and a shorthand slot takes neither. *)
let read_timeline_axis_keyword t : timeline_axis =
  Cursor.enum "timeline-axis"
    [
      ("block", (Block : timeline_axis)); ("inline", Inline); ("x", X); ("y", Y);
    ]
    t

let rec read_timeline_axis t : timeline_axis =
  Cursor.enum_or_var "timeline-axis"
    [
      ("initial", (Initial : timeline_axis));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_timeline_axis t))
    ~default:(fun t ->
      match
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_timeline_axis_keyword t
      with
      | [ axis ] -> axis
      | axes -> Axes axes)
    t

(* Secs. 4.1 and 5.1 spell the name [[ none | <dashed-ident> ]#], so [none]
   names one timeline among others rather than the whole value. *)
let read_timeline_ident t : timeline_ident =
  Cursor.enum "timeline-name"
    [ ("none", (None : timeline_ident)) ]
    ~default:(fun t -> (Name (read_dashed_ident t) : timeline_ident))
    t

let rec read_timeline_name t : timeline_name =
  Cursor.enum_or_var "timeline-name"
    [
      ("initial", (Initial : timeline_name));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_timeline_name t) : timeline_name))
    ~default:(fun t ->
      (Names (Cursor.list ~sep:Cursor.comma ~at_least:1 read_timeline_ident t)
        : timeline_name))
    t

(* Sec. 6 keeps [none] out of the list here: [timeline-scope] is [none |
   <dashed-ident>#], so the keyword stands for the whole value. *)
let rec read_timeline_scope t : timeline_scope =
  Cursor.enum_or_var "timeline-scope"
    [
      ("none", (None : timeline_scope));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_timeline_scope t) : timeline_scope))
    ~default:(fun t ->
      (Names (Cursor.list ~sep:Cursor.comma ~at_least:1 read_dashed_ident t)
        : timeline_scope))
    t

let read_timeline_shorthand_item t : timeline_shorthand_item =
  Cursor.ws t;
  let name = read_timeline_ident t in
  Cursor.ws t;
  let axis = Cursor.option read_timeline_axis_keyword t in
  { name; axis }

let rec read_timeline_shorthand t : timeline_shorthand =
  Cursor.enum_or_var "timeline"
    [
      ("initial", (Initial : timeline_shorthand));
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
      (* Scroll Animations 1 sec. 3.4.3 gives each item [auto] or a plain
         [<length-percentage>], with no range restriction on it. *)
      (Length (read_length_percentage ~with_keywords:false t)
        : timeline_inset_item))
    t

(* Sec. 5.3 spells the inset [[ [ auto | <length-percentage> ]{1,2} ]#]: one or
   two items per timeline, and one entry per timeline. *)
let read_timeline_inset_pair t : timeline_inset =
  match Cursor.list ~at_least:1 ~at_most:2 read_timeline_inset_item t with
  | [ first ] -> (Inset (first, None) : timeline_inset)
  | [ first; second ] -> Inset (first, Some second)
  | _ -> Cursor.err_expected t "timeline-inset"

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
      match
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_timeline_inset_pair t
      with
      | [ inset ] -> inset
      | insets -> Insets insets)
    t

let read_view_timeline_shorthand_item t : view_timeline_shorthand_item =
  (* Scroll-driven Animations 1 sec. 3.4.4: the name is followed by
     [<'view-timeline-axis'> || <'view-timeline-inset'>]?, so try each missing
     slot until neither consumes input. *)
  Cursor.ws t;
  let name = read_timeline_ident t in
  let axis = ref Option.None in
  let inset = ref Option.None in
  let try_axis () =
    if !axis <> Option.None then false
    else
      match Cursor.option read_timeline_axis_keyword t with
      | Some value ->
          axis := Some value;
          true
      | None -> false
  in
  let try_inset () =
    if !inset <> Option.None then false
    else
      match Cursor.option read_timeline_inset_pair t with
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
      ("initial", (Initial : view_timeline_shorthand));
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

(* Scroll Animations 1 sec. 5.1 spells each item [normal | <length-percentage> |
   <timeline-range-name> <length-percentage>?], so the intrinsic-sizing keywords
   a bare length would accept are out. *)
let read_range_length_percentage t =
  Values.read_length_percentage ~with_keywords:false t

let read_animation_range_offset t : length_percentage option =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_comma t then
    (None : length_percentage option)
  else
    match Cursor.peek_keyword t with
    | Some "normal" -> (None : length_percentage option)
    | Some next when List.mem next Keyframe.timeline_range_names ->
        (None : length_percentage option)
    | _ -> (Some (read_range_length_percentage t) : length_percentage option)

(* Secs. 3.1 and 3.2 spell each end [[ normal | <length-percentage> |
   <timeline-range-name> <length-percentage>? ]#], one entry per animation, so
   [normal] names one end among others rather than the whole value. *)
let read_animation_range_one t : animation_range_item =
  Cursor.ws t;
  match Cursor.peek_keyword t with
  | Some "normal" ->
      let _ = Cursor.ident t in
      (Normal : animation_range_item)
  | Some name when is_animation_range_name name ->
      let name : animation_range_name = read_animation_range_name t in
      let lp = read_animation_range_offset t in
      Named (name, lp)
  | _ -> Offset (read_range_length_percentage t)

let rec read_animation_range_item t : animation_range_item =
  let keywords : (string * animation_range_item) list =
    [
      ("initial", (Initial : animation_range_item));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  Cursor.enum_or_var "animation-range-item" keywords
    ~var:(fun t ->
      (Var (Values.read_var read_animation_range_item t) : animation_range_item))
    ~default:(fun t ->
      match
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_animation_range_one t
      with
      | [ item ] -> item
      | items -> Items items)
    t

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
  (* Sec. 3.3 spells the shorthand [[ <start> <end>? ]#], so a comma ends one
     entry the way the end of the value does. *)
  let read_range t =
    let first = read_animation_range_one t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_comma t then Range (first, None)
    else
      let second = read_animation_range_one t in
      Range (first, Some second)
  in
  (Cursor.enum_or_var "animation-range" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_animation_range t) : animation_range))
     ~default:(fun t ->
       match Cursor.list ~sep:Cursor.comma ~at_least:1 read_range t with
       | [ range ] -> range
       | ranges -> Ranges ranges)
     t
    : animation_range)
