(** Property tests: transforms, transitions, animations, motion paths and
    timelines. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_transition_shorthand =
  check_value_cursor "transition_shorthand" read_transition_shorthand
    pp_transition_shorthand

let check_animation_shorthand =
  check_value_cursor "animation_shorthand" read_animation_shorthand
    pp_animation_shorthand

let check_backface_visibility =
  check_value_cursor "backface-visibility" read_backface_visibility
    pp_backface_visibility

let check_scale = check_value_cursor "scale" read_scale pp_scale

let check_steps_direction =
  check_value_cursor "steps-direction" read_steps_direction pp_steps_direction

let check_transition_property_value =
  check_value_cursor "transition-property-value" read_transition_property_value
    pp_transition_property_value

let check_transition_property =
  check_value_cursor "transition-property" read_transition_property
    pp_transition_property

let check_transition_behavior =
  check_value_cursor "transition-behavior" read_transition_behavior
    pp_transition_behavior

let check_transition =
  check_value_cursor "transition" read_transition pp_transition

let check_animation_direction =
  check_value_cursor "animation-direction" read_animation_direction
    pp_animation_direction

let check_animation_fill_mode =
  check_value_cursor "animation-fill-mode" read_animation_fill_mode
    pp_animation_fill_mode

let check_animation_iteration_count =
  check_value_cursor "animation-iteration-count" read_animation_iteration_count
    pp_animation_iteration_count

let check_animation_play_state =
  check_value_cursor "animation-play-state" read_animation_play_state
    pp_animation_play_state

let check_animation = check_value_cursor "animation" read_animation pp_animation

let check_translate_value =
  check_value_cursor "translate_value" read_translate_value pp_translate_value

let check_transform_style =
  check_value_cursor "transform_style" read_transform_style pp_transform_style

let check_transform_origin =
  check_value_cursor "transform_origin" read_transform_origin
    pp_transform_origin

let check_will_change =
  check_value_cursor "will_change" read_will_change pp_will_change

let check_perspective_origin =
  check_value_cursor "perspective_origin" read_perspective_origin
    pp_perspective_origin

let check_timeline_axis =
  check_value_cursor "axis" read_timeline_axis pp_timeline_axis

let check_timeline_shorthand =
  check_value_cursor "timeline_shorthand" read_timeline_shorthand
    pp_timeline_shorthand

let check_view_timeline_shorthand =
  check_value_cursor "view_timeline_shorthand" read_view_timeline_shorthand
    pp_view_timeline_shorthand

let check_rotate_value =
  check_value_cursor "rotate_value" read_rotate_value pp_rotate_value

let check_transform_box =
  check_value_cursor "transform_box" read_transform_box pp_transform_box

let check_offset =
  check_value_cursor "offset" read_offset pp_offset ~roundtrip:true

let check_offset_target =
  check_value_cursor "offset_target" read_offset_target pp_offset_target
    ~roundtrip:true

let check_offset_anchor =
  check_value_cursor "offset_anchor" read_offset_anchor pp_offset_anchor

let check_offset_position =
  check_value_cursor "offset_position" read_offset_position pp_offset_position

let check_view_timeline_shorthand_item =
  check_value_cursor "view_timeline_shorthand_item"
    read_view_timeline_shorthand_item pp_view_timeline_shorthand_item

let test_transform () =
  check_transform "none";
  check_transform "translateX(10px)";
  check_transform "translateX(-50%)";
  check_transform "translateY(2em)";
  check_transform "translateZ(100px)";
  check_transform "translate(10px)";
  check_transform "translate(10px, 20px)" ~expected:"translate(10px,20px)";
  check_transform "translate3d(10px, 20px, 30px)"
    ~expected:"translate3d(10px,20px,30px)";
  check_transform "rotate(45deg)";
  check_transform "rotate(0.5turn)" ~expected:"rotate(.5turn)";
  check_transform "rotate(3.14rad)";
  check_transform "rotateX(45deg)";
  check_transform "rotateY(90deg)";
  (* pp holds the authored transform function and only minifies whitespace;
     folding rotateZ/rotate3d to the shorter equivalent function is an optimize
     transform, not a pp serialization. *)
  check_transform "rotateZ(180deg)";
  check_transform "rotate3d(1, 0, 0, 45deg)" ~expected:"rotate3d(1,0,0,45deg)";
  check_transform "rotate3d(0, 1, 0, 90deg)" ~expected:"rotate3d(0,1,0,90deg)";
  check_transform "rotate3d(1, 1, 1, 60deg)" ~expected:"rotate3d(1,1,1,60deg)";
  check_transform "scale(2)";
  check_transform "scale(0.5)" ~expected:"scale(.5)";
  check_transform "scale(2, 3)" ~expected:"scale(2,3)";
  check_transform "scaleX(2)";
  check_transform "scaleY(0.5)" ~expected:"scaleY(.5)";
  check_transform "scaleZ(1.5)";
  check_transform "scale3d(2, 3, 4)" ~expected:"scale3d(2,3,4)";
  check_transform "skew(30deg)";
  check_transform "skew(30deg, 45deg)" ~expected:"skew(30deg,45deg)";
  check_transform "skew( 10deg , 20deg )" ~expected:"skew(10deg,20deg)";
  check_transform "skewX(45deg)";
  check_transform "skewY(30deg)";
  check_transform "matrix(1, 0, 0, 1, 0, 0)" ~expected:"matrix(1,0,0,1,0,0)";
  check_transform "matrix(0.866, 0.5, -0.5, 0.866, 0, 0)"
    ~expected:"matrix(.866,.5,-.5,.866,0,0)";
  check_transform "matrix3d(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)"
    ~expected:"matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1)";
  (* var() tests - single transform functions can be variables *)
  check_transform "var(--my-transform)";
  check_transform "var(--my-transform,none)";
  check_transform "var(--tw-rotate-x,)";
  check_transform "var(--tw-scale,scale(1))";
  neg_cursor read_transform "invalidfunc()";
  neg_cursor read_transform "translate3d(10px,20px)";
  neg_cursor read_transform "scale3d(1,2)";
  neg_cursor read_transform "matrix(1,2,3,4,5)";
  neg_cursor read_transform "matrix3d(1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0)";
  (* CSS Transforms 1 sec. 13.2 gives [skew()] one or two [<angle>], and
     [matrix()]/[matrix3d()] take exactly six/sixteen [<number>] (sec. 12.1, CSS
     Transforms 2 sec. 12.2); a trailing argument outside that grammar is
     invalid (CSS Syntax 3 sec. 8.2). [Cursor.call] did not require these
     readers to consume their whole sub-cursor, so each read only its prefix and
     answered with a truncated function, e.g. [skew(10deg,red)] as
     [skew(10deg)]. *)
  neg_cursor read_transform "skew(10deg,red)";
  neg_cursor read_transform "matrix(1,2,3,4,5,6,red)";
  neg_cursor read_transform "matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,red)";
  (* A seventeenth, well-formed number is the same over-count as [red] above,
     not a different bug: the exact-arity match already rejects it, with or
     without the [expect_eof] fix. *)
  neg_cursor read_transform "matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,2)";
  (* The same gap in [Cursor.call] itself (not just the readers #617/#627
     already patched): CSS Transforms 1 sec. 7.1 gives each one-argument
     function exactly one [<length-percentage>] / [<angle>] / [<number>], CSS
     Transforms 2 sec. 12.1-12.2 gives the 3d/axis forms a fixed comma count,
     and none of these readers checked that its sub-cursor was exhausted, so
     e.g. [translateX(10px red)] read only [10px] and answered
     [translateX(10px)] instead of invalidating the declaration. *)
  List.iter
    (neg_cursor read_transform)
    [
      "translateX(10px red)";
      "translateY(10px red)";
      "translateZ(10px red)";
      "translate3d(10px,20px,30px,red)";
      "translate(10px red)";
      "translate(10px,20px,red)";
      "rotateX(45deg red)";
      "rotateY(45deg red)";
      "rotateZ(45deg red)";
      "rotate(45deg red)";
      "rotate3d(1,0,0,45deg,red)";
      "scaleX(2 red)";
      "scaleY(2 red)";
      "scaleZ(2 red)";
      "scale3d(2,3,4,red)";
      "skewX(10deg red)";
      "skewY(10deg red)";
    ];
  (* Controls: whitespace directly inside the parens still stands. *)
  check_transform "translateX( 10px )" ~expected:"translateX(10px)";
  check_transform "rotate( 45deg )" ~expected:"rotate(45deg)";
  check_transform "scaleX( 2 )" ~expected:"scaleX(2)"

let test_transforms () =
  (* Adjacent [var()] references in a transform list keep a single separating
     space. Both Lightning CSS and cssnano preserve the space here because a
     [var()] expansion at computed-value time may require whitespace to delimit
     transform functions; concrete function chains can drop the space, [var()]
     chains cannot. *)
  check_transforms ~expected:"var(--tw-rotate-x,) var(--tw-rotate-y,)"
    "var(--tw-rotate-x,) var(--tw-rotate-y,)";
  check_transforms
    ~expected:"var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,)"
    "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)";
  check_transforms
    ~expected:
      "var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,) \
       var(--tw-skew-x,) var(--tw-skew-y,)"
    "var(--tw-rotate-x,)var(--tw-rotate-y,)var(--tw-rotate-z,)var(--tw-skew-x,)var(--tw-skew-y,)";
  (* Per CSS Transforms 1 section 8 the printer drops whitespace between
     back-to-back transform functions under minify. *)
  check_transforms ~expected:"translateX(10px)var(--my-rotate)"
    "translateX(10px) var(--my-rotate)";
  check_transforms ~expected:"var(--translate)scale(2)"
    "var(--translate) scale(2)";
  check_transforms ~expected:"rotate(45deg)scale(1.5)"
    "rotate(45deg) scale(1.5)";
  check_transforms "none";
  (* Test transforms variable reference (whole list as var) *)
  check_transforms "var(--my-transforms)";
  check_transforms "var(--my-transforms,none)";
  neg_cursor read_transforms "invalidfunc()";
  neg_cursor read_transforms "translate3d(10px,20px) scale(2)"

let test_transition_shorthand () =
  check_transition_shorthand "all";
  check_transition_shorthand "opacity 1s";
  check_transition_shorthand "opacity 1s ease-in";
  check_transition_shorthand ~expected:"opacity 1s ease-in .5s"
    "opacity 1s ease-in 0.5s";
  check_transition_shorthand "width 2s";
  check_transition_shorthand ~expected:"all .3s linear" "all 0.3s linear";
  neg_cursor ~allow_partial:true read_transition_shorthand "2invalid";
  neg_cursor ~allow_partial:true read_transition_shorthand "-1s";
  (* negative duration. [transition:opacity] (property only, 0s duration) is
     valid - the shorthand components are all optional - so it is not rejected. *)
  (* too many durations *)
  neg_cursor ~allow_partial:true read_transition_shorthand "1s 2s 3s 4s"

let test_animation_shorthand () =
  (* Valid CSS per spec - all components optional *)
  (* duration only, name defaults to none *)
  check_animation_shorthand "1s";
  (* name only, duration defaults to 0s *)
  check_animation_shorthand "slide";
  check_animation_shorthand ~expected:"slide 1s" "1s slide";
  check_animation_shorthand ~expected:"slide 1s" "slide 1s";
  check_animation_shorthand ~expected:"slide 1s ease-in" "slide 1s ease-in";
  check_animation_shorthand ~expected:"slide 1s ease-in .5s infinite"
    "slide 1s ease-in 0.5s infinite";
  check_animation_shorthand ~expected:"slide 1s infinite" "slide 1s infinite";
  check_animation_shorthand ~expected:"slide 1s reverse" "slide 1s reverse";
  check_animation_shorthand ~expected:"slide 1s forwards" "slide 1s forwards";
  check_animation_shorthand ~expected:"slide 1s paused" "slide 1s paused";

  (* Invalid cases *)
  (* invalid time unit *)
  neg_cursor ~allow_partial:true read_animation_shorthand "2invalid";
  (* negative duration *)
  neg_cursor ~allow_partial:true read_animation_shorthand "slide -1s";
  (* too many time values *)
  neg_cursor read_animation_shorthand "slide 1s 2s 3s 4s 5s"

let test_transform_style () =
  check_transform_style "flat";
  check_transform_style "preserve-3d";
  check_transform_style "inherit";
  neg_cursor read_transform_style "invalid-style"

let test_backface_visibility () =
  check_backface_visibility "visible";
  check_backface_visibility "hidden";
  check_backface_visibility "inherit";
  neg_cursor read_backface_visibility "invalid-visibility"

let test_scale () =
  check_scale "none";
  check_scale "1";
  check_scale ~expected:".5" "0.5";
  check_scale ~expected:"1.5 2" "1.5 2.0";
  check_scale ~expected:".8 .8 1.2" "0.8 0.8 1.2";
  (* var() tests - when all values are vars, they're concatenated without
     spaces *)
  check_scale "var(--my-scale)";
  check_scale "var(--my-scale,1)";
  check_scale "var(--my-scale-x) var(--my-scale-y)"
    ~expected:"var(--my-scale-x)var(--my-scale-y)";
  check_scale "var(--tw-scale-x)var(--tw-scale-y)";
  check_scale "var(--tw-scale-x,1)var(--tw-scale-y,1)";
  check_scale "var(--x,) var(--y,)" ~expected:"var(--x,)var(--y,)";
  check_scale "var(--x,1) var(--y,1) var(--z,1)"
    ~expected:"var(--x,1)var(--y,1)var(--z,1)";
  neg_cursor read_scale "2scale"

let test_steps_direction () =
  check_steps_direction "jump-start";
  check_steps_direction "jump-end";
  check_steps_direction "jump-none";
  check_steps_direction "jump-both";
  check_steps_direction "start";
  check_steps_direction "end";
  neg_cursor read_steps_direction "invalid-direction";
  neg_cursor read_steps_direction "jump";
  neg_cursor read_steps_direction "middle"

let test_timing_function () =
  check_timing_function "ease";
  check_timing_function "linear";
  check_timing_function "ease-in";
  check_timing_function "ease-out";
  check_timing_function "ease-in-out";
  check_timing_function "step-start";
  check_timing_function "step-end";
  check_timing_function ~expected:"cubic-bezier(.1,.7,1,.1)"
    "cubic-bezier(0.1, 0.7, 1.0, 0.1)";
  neg_cursor read_timing_function "cubic-bezier()";
  (* CSS Easing 1 sec. 4.2/4.3 give [cubic-bezier()] exactly four [<number>] and
     [steps()] a count and an optional [<step-position>]; a trailing argument is
     invalid (CSS Syntax 3 sec. 8.2). [Cursor.call] did not require either
     reader to consume its whole sub-cursor, so e.g. [steps(4, red)] read only
     the count and answered [steps(4)] instead of invalidating the
     declaration. *)
  check_timing_function "steps(4)";
  check_timing_function "steps(4, jump-start)" ~expected:"steps(4,jump-start)";
  check_timing_function "steps( 4 , jump-start )"
    ~expected:"steps(4,jump-start)";
  check_timing_function "steps(2, jump-none)" ~expected:"steps(2,jump-none)";
  neg_cursor read_timing_function "steps(1.9)";
  neg_cursor read_timing_function "steps(1, jump-none)";
  neg_cursor read_timing_function "steps(4, jump-start, red)";
  neg_cursor read_timing_function "steps(4 red)";
  neg_cursor read_timing_function "cubic-bezier(0.1,0.7,1,0.1,0.5)"

let test_transition_property_value () =
  check_transition_property_value "all";
  check_transition_property_value "none";
  check_transition_property_value "opacity";
  check_transition_property_value "transform";
  (* Arbitrary identifier should be accepted (inert if non-animatable) *)
  check_transition_property_value "invalid-transition";
  neg_cursor read_transition_property_value ""

let test_transition_property () =
  check_transition_property "all";
  check_transition_property "none";
  check_transition_property "opacity";
  check_transition_property "transform";
  (* Arbitrary identifier should be accepted (inert if non-animatable) *)
  check_transition_property "invalid-transition";
  neg_cursor read_transition_property ""

let test_transition_behavior () =
  check_transition_behavior "normal";
  check_transition_behavior "allow-discrete";
  check_transition_behavior "inherit";
  neg_cursor read_transition_behavior "invalid";
  neg_cursor read_transition_behavior ""

let test_transition () =
  check_transition "inherit";
  check_transition "initial";
  check_transition "none";
  neg_cursor ~allow_partial:true read_transition "2invalid"

let test_animation_direction () =
  check_animation_direction "normal";
  check_animation_direction "reverse";
  check_animation_direction "alternate";
  check_animation_direction "alternate-reverse";
  neg_cursor read_animation_direction "invalid-direction"

let test_animation_fill_mode () =
  check_animation_fill_mode "none";
  check_animation_fill_mode "forwards";
  check_animation_fill_mode "backwards";
  check_animation_fill_mode "both";
  neg_cursor read_animation_fill_mode "invalid-fill"

let test_animation_iteration_count () =
  check_animation_iteration_count "1";
  check_animation_iteration_count "infinite";
  check_animation_iteration_count "2.5";
  (* CSS Values 4 sec. 10.1: a math function stands where a [<number>] does. *)
  check_animation_iteration_count "calc(1 + 2)";
  check_animation_iteration_count "1,calc(2*2)";
  neg_cursor read_animation_iteration_count "invalid-count"

let test_animation_play_state () =
  check_animation_play_state "running";
  check_animation_play_state "paused";
  neg_cursor read_animation_play_state "invalid-state"

let test_animation () =
  check_animation "inherit";
  check_animation "initial";
  check_animation "none";
  check_animation ~expected:"slide-in 1s" "slide-in 1s";
  check_animation ~expected:"my-animation 2s ease-in 1s infinite alternate"
    "my-animation 2s ease-in 1s infinite alternate";
  (* Test invalid animation shorthand according to CSS spec *)
  neg_cursor read_animation "1s 2s 3s";
  (* More than 2 time values *)
  neg_cursor read_animation "-2s";
  (* Negative duration is invalid *)
  neg_cursor ~allow_partial:true read_animation
    "2s -1" (* Negative iteration count is invalid *)

let test_offset_anchor () =
  check_offset_anchor "auto";
  check_offset_anchor "center";
  check_offset_anchor "left top";
  check_offset_anchor "left 10px top 20px";
  check_offset_anchor "var(--anchor,left top)";
  check_offset_anchor "inherit";
  neg_cursor read_offset_anchor "normal";
  neg_cursor read_offset_anchor "size"

let test_offset_position () =
  check_offset_position "normal";
  check_offset_position "auto";
  check_offset_position "center";
  check_offset_position "20% 30%" ~expected:"20%30%";
  check_offset_position "left 10px top 20px";
  check_offset_position "var(--position,center)";
  check_offset_position "inherit";
  neg_cursor read_offset_position "none";
  neg_cursor read_offset_position "size"

(* Motion Path 1 (ED) sec. 2.6 gives the [offset] shorthand

   [ <'offset-position'>? [ <'offset-path'> [ <'offset-distance'> ||
   <'offset-rotate'> ]? ]? ]! [ / <'offset-anchor'> ]?

   and says "Omitted values are set to their initial values". The [!] makes the
   leading group required, so a declaration writes at least a position or a
   path; [offset-distance] and [offset-rotate] live inside the path branch and
   are unreachable without one. *)
let offset_sheet css : Css.parse =
  match Css.of_string ~strict:false css with
  | Ok parse -> parse
  | Error e -> Alcotest.failf "%s: %s" css (Error.to_string e)

let offset_minified css =
  String.trim (Css.to_string ~minify:true (offset_sheet css).stylesheet)

(* A value the grammar rejects is dropped from the sheet and reported, not
   carried through as opaque text. *)
let offset_rejects value =
  let css = String.concat "" [ ".x{offset:"; value; "}" ] in
  Alcotest.(check string) (css ^ " is dropped") "" (offset_minified css);
  Alcotest.(check bool)
    (css ^ " warns") true
    (match (offset_sheet css).warnings with [] -> false | _ :: _ -> true)

(* [css] survives parse / print / parse unchanged. *)
let offset_roundtrips css =
  let once = offset_minified css in
  Alcotest.(check string) (css ^ " roundtrip") css once;
  Alcotest.(check string)
    (css ^ " roundtrip is stable")
    once (offset_minified once)

let test_offset () =
  (* Each slot on its own: the position branch takes [normal | auto |
     <position>] (sec. 2.3), the path branch [none | <offset-path>] (sec.
     2.1). *)
  check_offset "normal";
  check_offset "auto";
  check_offset "left top";
  check_offset "100px";
  check_offset "none";
  check_offset "path(\"M 0 0 H 1\")";
  (* The path branch carries the distance and the rotation behind it, in the
     grammar's order whichever order the [||] pair was written in. *)
  check_offset "none 50px";
  check_offset "none reverse";
  check_offset ~expected:"none 50px reverse 30deg" "none reverse 30deg 50px";
  (* The anchor sits behind the slash (sec. 2.4). *)
  check_offset "none/50%";
  check_offset "100px/50%";
  (* Every slot at once. *)
  check_offset
    ~expected:"left bottom ray(0rad closest-corner)10px auto 30deg/right bottom"
    "left bottom ray(0rad closest-corner) 10px auto 30deg / right bottom";
  (* A CSS-wide keyword and a whole-value [var()] stand for the shorthand. *)
  check_offset "inherit";
  check_offset "var(--motion,none)";
  neg_cursor read_offset "total nonsense here";
  neg_cursor read_offset "path(\"m 0 0 h 100\") auto reverse"

let test_offset_target () =
  (* The [!] group on its own: a position, a path, or a position then a path. *)
  check_offset_target "normal";
  check_offset_target "left top";
  check_offset_target "none";
  check_offset_target "normal none";
  check_offset_target "100px none";
  check_offset_target "none 50px reverse 30deg";
  (* The group must produce a value, and the rotation needs a path in front of
     it. *)
  neg_cursor read_offset_target "";
  neg_cursor read_offset_target "30deg";
  neg_cursor read_offset_target "reverse"

let offset_canonical_slots () =
  (* Each slot canonicalises the way its longhand does. *)
  decl_optimizes ~prop:"offset" ~held:"none/center" ~into:"none/50%"
    "none / center";
  decl_optimizes ~prop:"offset" ~held:"none/left top" ~into:"none/0 0"
    "none / left top";
  decl_optimizes ~prop:"offset" ~held:"left top none" ~into:"0 0"
    "left top none";
  (* A slot holding its longhand's initial says what leaving it out says, so it
     drops: [normal] (sec. 2.3), [0] (sec. 2.2), [auto] (secs. 2.5, 2.4). *)
  decl_optimizes ~prop:"offset" ~held:"normal none reverse" ~into:"none reverse"
    "normal none reverse";
  decl_optimizes ~prop:"offset" ~held:"none 0" ~into:"none" "none 0";
  decl_optimizes ~prop:"offset" ~held:"path(\"M 0 0 H 1\")auto"
    ~into:"path(\"M 0 0 H 1\")" "path('M 0 0 H 1') auto";
  decl_optimizes ~prop:"offset" ~held:"none/auto" ~into:"none" "none / auto";
  (* [offset: initial] resets the same five longhands a bare [none] path leaves
     at their initials. *)
  decl_optimizes ~prop:"offset" ~held:"initial" ~into:"none" "initial";
  (* The path is the group's initial too, but dropping it is behaviour
     preserving only when no distance and no rotation are left behind it: both
     are reachable only through a path, and a bare [50%] re-reads as the
     position. *)
  decl_optimizes ~prop:"offset" ~held:"50%none" ~into:"50%" "50% none";
  decl_optimizes ~prop:"offset" ~held:"none 50%" ~into:"none 50%" "none 50%";
  (* Dropping every slot would leave an empty value, which is not a declaration.
     The path [none] is the shortest spelling of the whole group, so it is what
     stays behind. *)
  decl_optimizes ~prop:"offset" ~held:"normal" ~into:"none" "normal";
  decl_optimizes ~prop:"offset" ~held:"normal none/auto" ~into:"none"
    "normal none / auto"

let offset_invalid_values () =
  offset_rejects "total nonsense here";
  (* The [!] multiplier: the leading group must produce a value. *)
  offset_rejects "/ center";
  (* The anchor is required once the slash is written. *)
  offset_rejects "none /";
  (* The distance and the rotation need a path in front of them. *)
  offset_rejects "30deg";
  offset_rejects "auto 30deg 90px";
  (* The path may not follow the distance or the rotation. *)
  offset_rejects "100px 0deg path(\"m 0 0 h 100\")";
  (* Neither half of the [||] pair may be filled twice. *)
  offset_rejects "path(\"m 0 0 h 100\") 100px 200px";
  offset_rejects "path(\"m 0 0 h 100\") auto reverse";
  offset_rejects "path(\"m 0 0 h 100\") reverse 100px 30deg";
  offset_rejects "path(\"m 0 0 h 100\") 200% auto 100px";
  (* Nothing follows the group but the anchor. *)
  offset_rejects "path(\"m 0 0 h 100\") bottom";
  (* The anchor is one [<position>]: at most four components. *)
  offset_rejects "none/10px 20px 30deg";
  (* A CSS-wide keyword stands alone (CSS Cascade 5 sec. 7.3). *)
  offset_rejects "initial none";
  offset_rejects "none/inherit"

let offset_slots_roundtrip () =
  offset_roundtrips ".x{offset:auto}";
  offset_roundtrips ".x{offset:none}";
  offset_roundtrips ".x{offset:100px}";
  offset_roundtrips ".x{offset:path(\"M 0 0 H 1\")}";
  offset_roundtrips ".x{offset:none 50px}";
  offset_roundtrips ".x{offset:none reverse}";
  offset_roundtrips ".x{offset:none 50px reverse 30deg}";
  offset_roundtrips ".x{offset:none/50%}";
  offset_roundtrips ".x{offset:100px/50%}";
  offset_roundtrips ".x{offset:0 0 path(\"M 0 0 H 1\")10px reverse 45deg/50%}";
  (* A [var()] the shorthand cannot assign to one slot keeps its authored
     text. *)
  offset_roundtrips ".x{offset:var(--a) var(--b)}"

(* Sec. 2.1 gives [offset-path] a [<basic-shape> || <coord-box>] branch beside
   [ray()], [url()] and [path()], so every shape function [clip-path] takes is a
   motion path too, alone or with a reference box. The shorthand's leading group
   holds the same branch. *)
let offset_path_basic_shapes () =
  offset_roundtrips ".x{offset-path:circle()}";
  offset_roundtrips ".x{offset-path:circle(50%)}";
  offset_roundtrips ".x{offset-path:ellipse(10px 20px at 0 0)}";
  offset_roundtrips ".x{offset-path:inset(10px)}";
  offset_roundtrips ".x{offset-path:xywh(0 0 100% 100%)}";
  offset_roundtrips ".x{offset-path:polygon(0 0,10px 0,0 10px)}";
  offset_roundtrips ".x{offset-path:content-box}";
  offset_roundtrips ".x{offset-path:circle(50%) content-box}";
  offset_roundtrips ".x{offset:content-box}";
  offset_roundtrips ".x{offset:circle(50%)10px}"

(* The five longhands keep the behaviour secs. 2.1 to 2.5 give them once the
   shorthand that resets them is modelled. *)
let offset_longhand_guards () =
  offset_roundtrips ".x{offset-path:none}";
  offset_roundtrips ".x{offset-path:path(\"M 0 0 H 1\")}";
  offset_roundtrips ".x{offset-distance:50%}";
  offset_roundtrips ".x{offset-rotate:auto}";
  offset_roundtrips ".x{offset-rotate:reverse 30deg}";
  offset_roundtrips ".x{offset-position:normal}";
  offset_roundtrips ".x{offset-anchor:auto}";
  decl_optimizes ~prop:"offset-anchor" ~held:"center" ~into:"50%" "center";
  decl_optimizes ~prop:"offset-position" ~held:"left top" ~into:"0 0" "left top";
  decl_optimizes ~prop:"offset-distance" ~held:"0px" ~into:"0" "0px"

(* Two spellings that print alike are one declaration, so the rules carrying
   them factor into a single selector list. *)
let offset_spellings_factor () =
  let css = ".a{offset:none/auto}.b{offset:none 0}.c{offset:normal}" in
  Alcotest.(check string)
    "offset spellings coalesce" ".a,.b,.c{offset:none}"
    (String.trim
       (Css.to_string ~minify:true (Css.optimize (offset_sheet css).stylesheet)))

let test_translate_value () =
  check_translate_value "none";
  check_translate_value "10px";
  check_translate_value "10px 20px";
  check_translate_value "10px 20px 30px";
  check_translate_value "50% 100%";
  check_translate_value "var(--my-translate)";
  check_translate_value ~expected:"var(--x)var(--y)" "var(--x) var(--y)";
  (* CSS Transforms 2 sec. 5 names lengths in every slot, so the
     intrinsic-sizing keywords a bare length would accept are out. *)
  neg_cursor read_translate_value "auto";
  neg_cursor read_translate_value "invalid-translate"

let test_transform_origin () =
  (* Per CSS Transforms 1 sec. 4 the keyword [center] is shorthand for [50%] and
     matched-pair shorthand collapses to a single value during optimization. The
     value printer preserves the parsed node. *)
  check_transform_origin "center";
  check_transform_origin "left top";
  (* A single value sets the X origin; Y defaults to center (50%), so it must
     not be duplicated onto the Y axis: [100%] means [100% 50%], not [100%
     100%], and [0] means [0 50%], not [0 0]. Only [50%] coincides with center.
     CSS Transforms 1 section 4; lightningcss and csso keep the single value. *)
  check_transform_origin "100%";
  check_transform_origin "0";
  check_transform_origin "50% 25%";
  check_transform_origin "50% 50% 10px";
  check_transform_origin ~expected:"0% 10px" "left 10px";
  check_transform_origin "left top 10px";
  check_transform_origin "center top 10px";
  check_transform_origin "inherit";
  (* CSS Transforms 1 sec. 4 names lengths and edge keywords, so the
     intrinsic-sizing ones are out. *)
  neg_cursor read_transform_origin "auto";
  neg_cursor read_transform_origin "invalid-origin"

let test_timeline_axis () =
  check_timeline_axis "block";
  check_timeline_axis "inline";
  check_timeline_axis "x";
  check_timeline_axis "y";
  (* Scroll-driven Animations 1 secs. 4.2 and 5.2 spell the axis [[ block |
     inline | x | y ]#], one entry per timeline. *)
  check_timeline_axis "block,inline";
  check_timeline_axis "x,y,block";
  neg_cursor read_timeline_axis "z";
  neg_cursor read_timeline_axis "auto";
  neg_cursor ~allow_partial:true read_timeline_axis "block inline"

let test_timeline_shorthand () =
  check_timeline_shorthand "--main block";
  check_timeline_shorthand "--scroll inline";
  check_timeline_shorthand "--x x";
  check_timeline_shorthand "--main";
  neg_cursor read_timeline_shorthand "main block";
  neg_cursor ~allow_partial:true read_timeline_shorthand "--main z"

let test_view_timeline_shorthand () =
  check_view_timeline_shorthand "--main";
  check_view_timeline_shorthand "--main block";
  check_view_timeline_shorthand "--main 10%";
  check_view_timeline_shorthand "--main x 10% 20%";
  check_view_timeline_shorthand ~expected:"--main x 10%" "--main 10% x";
  check_view_timeline_shorthand "--main 10%,--alt y auto";
  neg_cursor read_view_timeline_shorthand "main 10%";
  neg_cursor ~allow_partial:true read_view_timeline_shorthand "--main x y";
  neg_cursor ~allow_partial:true read_view_timeline_shorthand
    "--main 10% 20% 30%"

let test_view_timeline_shorthand_item () =
  check_view_timeline_shorthand_item "--main";
  check_view_timeline_shorthand_item "--main inline";
  check_view_timeline_shorthand_item "--main auto 10%";
  check_view_timeline_shorthand_item ~expected:"--main x 10%" "--main 10% x";
  neg_cursor read_view_timeline_shorthand_item "main 10%";
  neg_cursor ~allow_partial:true read_view_timeline_shorthand_item "--main x y"

let test_rotate_value () =
  check_rotate_value "45deg";
  check_rotate_value "none";
  check_rotate_value "x 45deg";
  check_rotate_value "y 90deg";
  neg_cursor read_rotate_value "invalid-rotate"

let test_transform_box () =
  check_transform_box "content-box";
  check_transform_box "border-box";
  check_transform_box "fill-box";
  check_transform_box "stroke-box";
  check_transform_box "view-box";
  check_transform_box "inherit";
  neg_cursor read_transform_box "invalid-transform-box"

let test_will_change () =
  check_will_change "auto";
  check_will_change "scroll-position";
  check_will_change "contents";
  check_will_change "transform";
  check_will_change "opacity";
  neg_cursor read_will_change "123invalid"

let test_perspective_origin () =
  check_perspective_origin "center";
  check_perspective_origin "top";
  check_perspective_origin "bottom";
  check_perspective_origin "left";
  check_perspective_origin "right";
  check_perspective_origin "50px 100px";
  neg_cursor read_perspective_origin ""

(* CSS Motion Path 1 (ED) sec. 2.1.1 gives [<ray-size> = <radial-extent> |
   sides], and [<radial-extent>] (css-images-3 sec. 3.2.1) is the four extent
   keywords: a length or a pair of radii is a [<radial-size>] branch that
   [ray()] has no syntax for. Every value the type admits prints, and the
   printed text parses back to it. *)
let test_ray_size () =
  let sizes : ray_size list =
    [ Closest_side; Closest_corner; Farthest_side; Farthest_corner; Sides ]
  in
  List.iter
    (fun size ->
      check_ray_size (Css.Pp.to_string ~minify:true pp_ray_size size))
    sizes;
  neg_cursor read_ray_size "10px"

let suite =
  ( "properties_animation",
    [
      test_case "transform" `Quick test_transform;
      test_case "transforms" `Quick test_transforms;
      test_case "will_change" `Quick test_will_change;
      test_case "perspective_origin" `Quick test_perspective_origin;
      test_case "ray_size" `Quick test_ray_size;
      test_case "transition_shorthand" `Quick test_transition_shorthand;
      test_case "animation_shorthand" `Quick test_animation_shorthand;
      test_case "transform_style" `Quick test_transform_style;
      test_case "backface_visibility" `Quick test_backface_visibility;
      test_case "scale" `Quick test_scale;
      test_case "steps_direction" `Quick test_steps_direction;
      test_case "timing_function" `Quick test_timing_function;
      test_case "transition_property_value" `Quick
        test_transition_property_value;
      test_case "transition_property" `Quick test_transition_property;
      test_case "transition_behavior" `Quick test_transition_behavior;
      test_case "transition" `Quick test_transition;
      test_case "animation_direction" `Quick test_animation_direction;
      test_case "animation_fill_mode" `Quick test_animation_fill_mode;
      test_case "animation_iteration_count" `Quick
        test_animation_iteration_count;
      test_case "animation_play_state" `Quick test_animation_play_state;
      test_case "animation" `Quick test_animation;
      test_case "offset_anchor" `Quick test_offset_anchor;
      test_case "offset_position" `Quick test_offset_position;
      test_case "offset" `Quick test_offset;
      test_case "offset_target" `Quick test_offset_target;
      test_case "offset canonical slots" `Quick offset_canonical_slots;
      test_case "offset invalid values" `Quick offset_invalid_values;
      test_case "offset slots roundtrip" `Quick offset_slots_roundtrip;
      test_case "offset longhand guards" `Quick offset_longhand_guards;
      test_case "offset path basic shapes" `Quick offset_path_basic_shapes;
      test_case "offset spellings factor" `Quick offset_spellings_factor;
      test_case "translate_value" `Quick test_translate_value;
      test_case "transform_origin" `Quick test_transform_origin;
      test_case "axis" `Quick test_timeline_axis;
      test_case "timeline_shorthand" `Quick test_timeline_shorthand;
      test_case "view_timeline_shorthand" `Quick test_view_timeline_shorthand;
      test_case "view_timeline_shorthand_item" `Quick
        test_view_timeline_shorthand_item;
      test_case "rotate_value" `Quick test_rotate_value;
      test_case "transform_box" `Quick test_transform_box;
    ] )
