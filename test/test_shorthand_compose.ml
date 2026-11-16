open Cascade
open Css_test_helpers

(* [decl_optimizes_to] wraps its input in one rule; these cases turn on what a
   neighbouring rule holds, so they optimize a whole sheet. *)
let sheet_optimizes_to ?scope ~into input =
  match Css.of_string input with
  | Ok { stylesheet; _ } ->
      Alcotest.(check string)
        (String.concat "" [ input; " minify+optimize" ])
        into
        (String.trim
           (Css.to_string ~minify:true (Css.optimize ?scope stylesheet)))
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

(* CSS Transitions 2 sec. 2.6: [transition] resets [transition-behavior] along
   with the four Transitions 1 longhands. A run that leaves a slot unwritten
   contracts only when nothing earlier in the rule set that slot to something
   other than its initial, otherwise the shorthand silently resets it. *)
let test_transition_contraction_covers_reset_longhands () =
  (* The behaviour is a component of [<single-transition>], so a run carrying it
     contracts and keeps it, whichever side of the run it sits on. Both [ease]
     and [0s] are initials and drop out. *)
  decl_optimizes_to ~into:"transition:color 1s allow-discrete"
    "transition-behavior:allow-discrete;transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s";
  (* A run leaving a slot out contracts into a shorthand that resets it, and
     under the fragment scope the optimizer assumes by default the surrounding
     CSS may hold that slot. So the two runs below keep their longhands, and
     take the shorthand only once the caller says the input is the whole
     graph. *)
  decl_optimizes_to
    ~into:
      "transition-property:color;transition-duration:1s;transition-behavior:allow-discrete"
    "transition-property:color;transition-duration:1s;transition-behavior:allow-discrete";
  decl_optimizes_to ~scope:`Stylesheet
    ~into:"transition:color 1s allow-discrete"
    "transition-property:color;transition-duration:1s;transition-behavior:allow-discrete";
  decl_optimizes_to
    ~into:
      "transition-property:color;transition-behavior:allow-discrete;transition-duration:1s"
    "transition-property:color;transition-behavior:allow-discrete;transition-duration:1s";
  decl_optimizes_to ~scope:`Stylesheet
    ~into:"transition:color 1s allow-discrete"
    "transition-property:color;transition-behavior:allow-discrete;transition-duration:1s";
  (* A run covering only the Transitions 1 longhands leaves the behaviour to the
     reset, which is the same partial contraction. *)
  decl_optimizes_to
    ~into:
      "transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s"
    "transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s";
  decl_optimizes_to ~scope:`Stylesheet ~into:"transition:color 1s"
    "transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s";
  (* [inherit] is not a shorthand component, so the run cannot carry it and the
     contraction would drop it. *)
  decl_optimizes_to
    ~into:
      "transition-behavior:inherit;transition-property:color;transition-duration:1s"
    "transition-behavior:inherit;transition-property:color;transition-duration:1s";
  (* Same rule for the delay: an unrelated declaration splits it off the run, so
     the run no longer covers the slot the shorthand resets. *)
  decl_optimizes_to
    ~into:
      "transition-delay:5s;color:red;transition-property:color;transition-duration:1s"
    "transition-delay:5s;color:red;transition-property:color;transition-duration:1s";
  (* An earlier shorthand holds the slots its own run left out. Contracting the
     later run resets the delay it set, in either scope. *)
  decl_optimizes_to
    ~into:
      "transition:color 1s \
       5s;transition-property:opacity;transition-duration:2s"
    "transition:color 1s ease \
     5s;transition-property:opacity;transition-duration:2s";
  decl_optimizes_to ~scope:`Stylesheet
    ~into:
      "transition:color 1s \
       5s;transition-property:opacity;transition-duration:2s"
    "transition:color 1s ease \
     5s;transition-property:opacity;transition-duration:2s";
  (* An earlier shorthand whose unwritten slots are already initials shadows
     nothing, so the later run contracts once the graph is in view. *)
  decl_optimizes_to ~scope:`Stylesheet ~into:"transition:opacity 2s"
    "transition:color 1s;transition-property:opacity;transition-duration:2s";
  (* An important longhand outranks the composed shorthand whatever the order,
     so it is not a hazard. *)
  decl_optimizes_to ~scope:`Stylesheet
    ~into:"transition-delay:5s!important;transition:color 1s"
    "transition-delay:5s!important;transition-property:color;transition-duration:1s"

(* The slot a contraction resets belongs to the element, not to the rule: any
   declaration that can reach the same element and holds that slot is at risk,
   whichever rule it sits in. Composition reads one rule, so it cannot see the
   holders next door. *)
(* CSS Scroll Snap 1 sec. 6.1 and 6.2 give the scroll-margin and scroll-padding
   logical axes the same [<length>{1,2}] shape the margin and padding axes have,
   so the pair composes the same way. Without it the canonical comparator cannot
   equate the shorthand with its own longhands. *)
let test_scroll_axis_pair_composes () =
  sheet_optimizes_to ~into:".x{scroll-margin-block:1px 2px}"
    ".x{scroll-margin-block-start:1px;scroll-margin-block-end:2px}";
  sheet_optimizes_to ~into:".x{scroll-margin-inline:1px}"
    ".x{scroll-margin-inline-start:1px;scroll-margin-inline-end:1px}";
  sheet_optimizes_to ~into:".x{scroll-padding-block:1px 2px}"
    ".x{scroll-padding-block-start:1px;scroll-padding-block-end:2px}";
  sheet_optimizes_to ~into:".x{scroll-padding-inline:3em}"
    ".x{scroll-padding-inline-start:3em;scroll-padding-inline-end:3em}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{scroll-margin-block-start:1px;scroll-margin-block-end:2px!important}"
    ".x{scroll-margin-block-start:1px;scroll-margin-block-end:2px!important}"

(* CSS Logical 1 sec. 4.3 and 4.4: [border-block-*] and [border-inline-*] take
   one or two values of the side longhand's own type, so the start and end
   longhands compose into the axis shorthand the way the length axes do. Without
   it the canonical comparator cannot equate the shorthand with its
   longhands. *)
let test_border_axis_pair_composes () =
  sheet_optimizes_to ~into:".x{border-inline-width:2px 1px}"
    ".x{border-inline-start-width:2px;border-inline-end-width:1px}";
  sheet_optimizes_to ~into:".x{border-block-width:2px}"
    ".x{border-block-start-width:2px;border-block-end-width:2px}";
  sheet_optimizes_to ~into:".x{border-inline-style:solid dashed}"
    ".x{border-inline-start-style:solid;border-inline-end-style:dashed}";
  sheet_optimizes_to ~into:".x{border-block-style:double}"
    ".x{border-block-start-style:double;border-block-end-style:double}";
  sheet_optimizes_to ~into:".x{border-inline-color:red currentColor}"
    ".x{border-inline-start-color:red;border-inline-end-color:currentcolor}";
  sheet_optimizes_to ~into:".x{border-block-color:#0f0}"
    ".x{border-block-start-color:#0f0;border-block-end-color:#0f0}";
  (* A CSS-wide keyword is the whole value or nothing, so a mixed pair stays two
     declarations. *)
  sheet_optimizes_to
    ~into:".x{border-inline-start-width:inherit;border-inline-end-width:thin}"
    ".x{border-inline-start-width:inherit;border-inline-end-width:thin}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{border-block-start-color:red;border-block-end-color:#00f!important}"
    ".x{border-block-start-color:red;border-block-end-color:blue!important}"

(* CSS Backgrounds 3 sec. 3.5 and CSS Logical 1 sec. 4.5: each border side is
   [<line-width> || <line-style> || <line-color>] over its own three longhands
   and resets nothing else, so a contiguous run of the three contracts the way
   [outline] already does. *)
let test_line_shorthand_composes () =
  sheet_optimizes_to ~into:".x{border-top:1px solid red}"
    ".x{border-top-width:1px;border-top-style:solid;border-top-color:red}";
  (* The three are independent, so any order names the same declaration. *)
  sheet_optimizes_to ~into:".x{border-left:1px solid red}"
    ".x{border-left-color:red;border-left-style:solid;border-left-width:1px}";
  sheet_optimizes_to ~into:".x{border-block-start:1px solid red}"
    ".x{border-block-start-width:1px;border-block-start-style:solid;border-block-start-color:red}";
  sheet_optimizes_to ~into:".x{border-inline-end:2px dotted#0f0}"
    ".x{border-inline-end-width:2px;border-inline-end-style:dotted;border-inline-end-color:#0f0}";
  (* The shorthand tells width from style from colour by type, so a substituted
     token sequence could land in another slot. *)
  sheet_optimizes_to
    ~into:
      ".x{border-right-width:var(--w);border-right-style:solid;border-right-color:red}"
    ".x{border-right-width:var(--w);border-right-style:solid;border-right-color:red}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{border-bottom-width:1px;border-bottom-style:solid;border-bottom-color:red!important}"
    ".x{border-bottom-width:1px;border-bottom-style:solid;border-bottom-color:red!important}"

(* CSS Cascade 5 sec. 7.3: [initial] is the property's initial value, which is
   what the shorthand assigns to a slot left out, so a longhand written that way
   names the same declaration as an omitted component. *)
let test_line_shorthand_initial_slot () =
  sheet_optimizes_to ~into:".x{border-block-start:thin dashed}"
    ".x{border-block-start-width:thin;border-block-start-style:dashed;border-block-start-color:initial}";
  sheet_optimizes_to ~into:".x{border-top:dashed red}"
    ".x{border-top-width:initial;border-top-style:dashed;border-top-color:red}";
  (* Every slot at its initial is what [none] names. *)
  sheet_optimizes_to ~into:".x{border-left:none}"
    ".x{border-left-width:initial;border-left-style:initial;border-left-color:initial}"

(* CSS Logical 1 sec. 4.6: [border-block] and [border-inline] set the width,
   style and colour of both sides of their axis, which is what the three axis
   shorthands set between them, so a single-valued run of the three contracts
   the way [border-width] / [border-style] / [border-color] contract into
   [border]. *)
let test_logical_border_whole_composes () =
  sheet_optimizes_to ~into:".x{border-block:1px solid red}"
    ".x{border-block-width:1px;border-block-style:solid;border-block-color:red}";
  sheet_optimizes_to ~into:".x{border-inline:1px solid red}"
    ".x{border-inline-start-width:1px;border-inline-end-width:1px;border-inline-start-style:solid;border-inline-end-style:solid;border-inline-start-color:red;border-inline-end-color:red}";
  (* An axis naming two different sides has no slot in the shorthand. *)
  sheet_optimizes_to
    ~into:
      ".x{border-block-width:1px \
       2px;border-block-style:solid;border-block-color:red}"
    ".x{border-block-width:1px \
     2px;border-block-style:solid;border-block-color:red}"

(* CSS Overscroll 1 sec. 2.1 and CSS Sizing 4 sec. 5.1 assign their two
   longhands the way [overflow] does: the first value is the x axis, the second
   the y axis, and one value names both. *)
let test_xy_pair_composes () =
  sheet_optimizes_to ~into:".x{overscroll-behavior:contain}"
    ".x{overscroll-behavior-x:contain;overscroll-behavior-y:contain}";
  sheet_optimizes_to ~into:".x{overscroll-behavior:auto none}"
    ".x{overscroll-behavior-x:auto;overscroll-behavior-y:none}";
  sheet_optimizes_to ~into:".x{contain-intrinsic-size:100px}"
    ".x{contain-intrinsic-width:100px;contain-intrinsic-height:100px}";
  sheet_optimizes_to ~into:".x{contain-intrinsic-size:100px 200px}"
    ".x{contain-intrinsic-width:100px;contain-intrinsic-height:200px}";
  sheet_optimizes_to ~into:".x{contain-intrinsic-size:auto 300px}"
    ".x{contain-intrinsic-width:auto 300px;contain-intrinsic-height:auto 300px}";
  (* [contain-intrinsic-size] has no spelling for one axis at [none] beside a
     sized other, so that pair stays two declarations. *)
  sheet_optimizes_to
    ~into:".x{contain-intrinsic-width:none;contain-intrinsic-height:100px}"
    ".x{contain-intrinsic-width:none;contain-intrinsic-height:100px}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:".x{overscroll-behavior-x:auto;overscroll-behavior-y:none!important}"
    ".x{overscroll-behavior-x:auto;overscroll-behavior-y:none!important}"

(* CSS Grid 2 sec. 8.3 and 8.4: [grid-row] and [grid-column] are [<grid-line> [/
   <grid-line>]?] over their start and end longhands, and [grid-area] is the
   same over all four. The printer then picks the shortest spelling of the lines
   the shorthand names. *)
let test_grid_placement_composes () =
  sheet_optimizes_to ~into:".x{grid-row:1/3}"
    ".x{grid-row-start:1;grid-row-end:3}";
  sheet_optimizes_to ~into:".x{grid-column:1/3}"
    ".x{grid-column-start:1;grid-column-end:3}";
  sheet_optimizes_to ~into:".x{grid-row:span 2}"
    ".x{grid-row-start:span 2;grid-row-end:auto}";
  sheet_optimizes_to ~into:".x{grid-area:1/2/3/4}"
    ".x{grid-row-start:1;grid-column-start:2;grid-row-end:3;grid-column-end:4}";
  sheet_optimizes_to ~into:".x{grid-area:a}"
    ".x{grid-row-start:a;grid-column-start:a;grid-row-end:a;grid-column-end:a}";
  (* A substituted line can be a whole [<start> / <end>], so it stays put. *)
  sheet_optimizes_to ~into:".x{grid-row-start:var(--l);grid-row-end:3}"
    ".x{grid-row-start:var(--l);grid-row-end:3}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to ~into:".x{grid-row-start:1;grid-row-end:3!important}"
    ".x{grid-row-start:1;grid-row-end:3!important}"

(* CSS Flexbox 1 sec. 5.1 and CSS Text Decoration 4 sec. 3.4: [flex-flow] and
   [text-emphasis] each take two longhands, one per component. A longhand at its
   initial names what leaving the component out names, so it drops - unless both
   do and the value would then say nothing. *)
let test_duo_keyword_composes () =
  (* [row] is the direction's initial, so the composed value names it by leaving
     the component out. *)
  sheet_optimizes_to ~into:".x{flex-flow:wrap}"
    ".x{flex-direction:row;flex-wrap:wrap}";
  sheet_optimizes_to ~into:".x{flex-flow:column}"
    ".x{flex-direction:column;flex-wrap:nowrap}";
  sheet_optimizes_to ~into:".x{flex-flow:wrap-reverse}"
    ".x{flex-direction:row;flex-wrap:wrap-reverse}";
  sheet_optimizes_to ~into:".x{flex-flow:row}"
    ".x{flex-direction:row;flex-wrap:nowrap}";
  sheet_optimizes_to ~into:".x{text-emphasis:filled red}"
    ".x{text-emphasis-style:filled;text-emphasis-color:red}";
  sheet_optimizes_to ~into:".x{text-emphasis:none}"
    ".x{text-emphasis-style:none;text-emphasis-color:initial}";
  (* A substituted component can be the whole value, so it stays put. *)
  sheet_optimizes_to ~into:".x{flex-direction:var(--d);flex-wrap:wrap}"
    ".x{flex-direction:var(--d);flex-wrap:wrap}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to ~into:".x{flex-direction:row;flex-wrap:wrap!important}"
    ".x{flex-direction:row;flex-wrap:wrap!important}"

(* CSS Animations 2 sec. 6.3 and CSS Scroll Animations 1 sec. 4.3:
   [animation-range] is [<start> <end>?] and [scroll-timeline] is [<name>
   <axis>?], each over its own two longhands. *)
let test_timeline_range_composes () =
  sheet_optimizes_to ~into:".x{animation-range:normal}"
    ".x{animation-range-start:normal;animation-range-end:normal}";
  sheet_optimizes_to ~into:".x{animation-range:entry 10%exit 90%}"
    ".x{animation-range-start:entry 10%;animation-range-end:exit 90%}";
  (* [block] is the axis's initial, so the composed value names it by leaving
     the component out. *)
  sheet_optimizes_to ~into:".x{scroll-timeline:--t}"
    ".x{scroll-timeline-name:--t;scroll-timeline-axis:block}";
  sheet_optimizes_to ~into:".x{scroll-timeline:none}"
    ".x{scroll-timeline-name:none;scroll-timeline-axis:block}";
  (* Sec. 4.3 spells the shorthand [<name> <axis>?], and the name half is [none
     | <dashed-ident>], so [none] pairs with an axis of its own. Chrome 153
     computes [scroll-timeline: none inline] to the same two longhands and
     serialises them back into it. *)
  sheet_optimizes_to ~into:".x{scroll-timeline:none inline}"
    ".x{scroll-timeline-name:none;scroll-timeline-axis:inline}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{animation-range-start:normal;animation-range-end:normal!important}"
    ".x{animation-range-start:normal;animation-range-end:normal!important}"

(* CSS Contain 3 sec. 4.3 and CSS Scroll Animations 1 sec. 5.2: [container] is
   [<name> [/ <type>]?] and [view-timeline] is [<name> <axis>? <inset>?], each
   over its own name longhand and its modifiers. A modifier at its initial names
   what leaving it out names. *)
let test_named_shorthand_composes () =
  sheet_optimizes_to ~into:".x{container:card/inline-size}"
    ".x{container-name:card;container-type:inline-size}";
  sheet_optimizes_to ~into:".x{container:none}"
    ".x{container-name:none;container-type:normal}";
  sheet_optimizes_to ~into:".x{container:card}"
    ".x{container-name:card;container-type:normal}";
  sheet_optimizes_to ~into:".x{view-timeline:--v}"
    ".x{view-timeline-name:--v;view-timeline-axis:block;view-timeline-inset:auto}";
  sheet_optimizes_to ~into:".x{view-timeline:--v inline}"
    ".x{view-timeline-name:--v;view-timeline-axis:inline;view-timeline-inset:auto}";
  sheet_optimizes_to ~into:".x{view-timeline:none}"
    ".x{view-timeline-name:none;view-timeline-axis:block;view-timeline-inset:auto}";
  (* An unnamed timeline carries no modifier, so a non-initial one beside it has
     no shorthand spelling. *)
  sheet_optimizes_to
    ~into:".x{view-timeline-name:none;view-timeline-axis:inline}"
    ".x{view-timeline-name:none;view-timeline-axis:inline}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:".x{container-name:card;container-type:inline-size!important}"
    ".x{container-name:card;container-type:inline-size!important}"

(* CSS Backgrounds 4 sec. 3.3: [background-position] is [<x> <y>] over the two
   axis longhands, the first value the x axis. A bare offset is measured from
   the start edge, so [left 10px] and [10px] name the same position. *)
let test_background_position_composes () =
  sheet_optimizes_to ~into:".x{background-position:50%10px}"
    ".x{background-position-x:50%;background-position-y:10px}";
  (* Sec. 2.6 reads a lone value as the x axis and centres the y, so the
     one-value spelling names the same position and is shorter. *)
  sheet_optimizes_to ~into:".x{background-position:10px}"
    ".x{background-position-x:10px;background-position-y:center}";
  sheet_optimizes_to ~into:".x{background-position:0}"
    ".x{background-position-x:left;background-position-y:center}";
  sheet_optimizes_to ~into:".x{background-position:right 5%bottom 2px}"
    ".x{background-position-x:right 5%;background-position-y:bottom 2px}";
  (* A substituted axis can be the whole value, so it stays put. *)
  sheet_optimizes_to
    ~into:".x{background-position-x:var(--x);background-position-y:10px}"
    ".x{background-position-x:var(--x);background-position-y:10px}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:".x{background-position-x:10px;background-position-y:20px!important}"
    ".x{background-position-x:10px;background-position-y:20px!important}"

(* CSS Multicol 1 sec. 4.3: [column-rule] is [<line-width> || <line-style> ||
   <line-color>] over its own three longhands, the shape the border sides take,
   and sec. 4.1 to 4.2 give the same initials. *)
let test_column_rule_composes () =
  sheet_optimizes_to ~into:".x{column-rule:1px solid red}"
    ".x{column-rule-width:1px;column-rule-style:solid;column-rule-color:red}";
  sheet_optimizes_to ~into:".x{column-rule:none}"
    ".x{column-rule-width:medium;column-rule-style:none;column-rule-color:currentcolor}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{column-rule-width:1px;column-rule-style:solid;column-rule-color:red!important}"
    ".x{column-rule-width:1px;column-rule-style:solid;column-rule-color:red!important}"

(* CSS Text 4 sec. 3: [white-space] is a shorthand of [white-space-collapse] and
   [text-wrap-mode], and one keyword names each of the four pairs its table
   lists. *)
let test_white_space_composes () =
  sheet_optimizes_to ~into:".x{white-space:pre-wrap}"
    ".x{white-space-collapse:preserve;text-wrap-mode:wrap}";
  sheet_optimizes_to ~into:".x{white-space:normal}"
    ".x{white-space-collapse:collapse;text-wrap-mode:wrap}";
  sheet_optimizes_to ~into:".x{white-space:pre}"
    ".x{white-space-collapse:preserve;text-wrap-mode:nowrap}";
  sheet_optimizes_to ~into:".x{white-space:nowrap}"
    ".x{white-space-collapse:collapse;text-wrap-mode:nowrap}";
  sheet_optimizes_to ~into:".x{white-space:pre-line}"
    ".x{white-space-collapse:preserve-breaks;text-wrap-mode:wrap}";
  (* A pair outside that table has no one-keyword spelling. *)
  sheet_optimizes_to
    ~into:".x{white-space-collapse:preserve-spaces;text-wrap-mode:wrap}"
    ".x{white-space-collapse:preserve-spaces;text-wrap-mode:wrap}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:".x{white-space-collapse:preserve;text-wrap-mode:wrap!important}"
    ".x{white-space-collapse:preserve;text-wrap-mode:wrap!important}"

(* CSS Text 4 sec. 5.1: [text-wrap] is [<'text-wrap-mode'> ||
   <'text-wrap-style'>] over its two longhands, and a longhand at its initial
   names what leaving the component out names. *)
let test_text_wrap_composes () =
  sheet_optimizes_to ~into:".x{text-wrap:balance}"
    ".x{text-wrap-mode:wrap;text-wrap-style:balance}";
  sheet_optimizes_to ~into:".x{text-wrap:wrap}"
    ".x{text-wrap-mode:wrap;text-wrap-style:auto}";
  sheet_optimizes_to ~into:".x{text-wrap:nowrap}"
    ".x{text-wrap-mode:nowrap;text-wrap-style:auto}";
  sheet_optimizes_to ~into:".x{text-wrap:pretty}"
    ".x{text-wrap-mode:wrap;text-wrap-style:pretty}";
  (* Neither component at its initial needs both written out. *)
  sheet_optimizes_to ~into:".x{text-wrap:nowrap balance}"
    ".x{text-wrap-mode:nowrap;text-wrap-style:balance}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:".x{text-wrap-mode:wrap;text-wrap-style:balance!important}"
    ".x{text-wrap-mode:wrap;text-wrap-style:balance!important}"

(* CSS Fonts 4 sec. 2.8.5: [font-synthesis] names the synthesis longhands set to
   [auto]; naming none of them is [none]. It resets the position longhand too,
   so a rule that writes one keeps its longhands. *)
let test_font_synthesis_composes () =
  sheet_optimizes_to ~into:".x{font-synthesis:none}"
    ".x{font-synthesis-weight:none;font-synthesis-style:none;font-synthesis-small-caps:none}";
  sheet_optimizes_to ~into:".x{font-synthesis:weight style}"
    ".x{font-synthesis-weight:auto;font-synthesis-style:auto;font-synthesis-small-caps:none}";
  sheet_optimizes_to ~into:".x{font-synthesis:weight style small-caps}"
    ".x{font-synthesis-weight:auto;font-synthesis-style:auto;font-synthesis-small-caps:auto}";
  (* The shorthand resets the position longhand, so a rule writing one is left
     alone. *)
  sheet_optimizes_to
    ~into:
      ".x{font-synthesis-position:none;font-synthesis-weight:auto;font-synthesis-style:auto;font-synthesis-small-caps:none}"
    ".x{font-synthesis-position:none;font-synthesis-weight:auto;font-synthesis-style:auto;font-synthesis-small-caps:none}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{font-synthesis-weight:none;font-synthesis-style:none;font-synthesis-small-caps:none!important}"
    ".x{font-synthesis-weight:none;font-synthesis-style:none;font-synthesis-small-caps:none!important}"

(* [-webkit-text-stroke] is [<line-width> || <color>] over its two longhands,
   the shape the border sides take, so the pair contracts the same way. *)
let test_webkit_text_stroke_composes () =
  sheet_optimizes_to ~into:".x{-webkit-text-stroke:1px red}"
    ".x{-webkit-text-stroke-width:1px;-webkit-text-stroke-color:red}";
  sheet_optimizes_to ~into:".x{-webkit-text-stroke:0 currentColor}"
    ".x{-webkit-text-stroke-width:0;-webkit-text-stroke-color:currentcolor}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{-webkit-text-stroke-width:1px;-webkit-text-stroke-color:red!important}"
    ".x{-webkit-text-stroke-width:1px;-webkit-text-stroke-color:red!important}"

(* CSS Text Decoration 4 sec. 2.5: [text-decoration] is [<line> || <style> ||
   <color> || <thickness>], and sec. 2.2 makes [auto] the thickness's initial,
   so a run that also writes the thickness contracts and an [auto] one leaves
   the component out. *)
let test_text_decoration_thickness_composes () =
  sheet_optimizes_to ~into:".x{text-decoration:underline dotted red}"
    ".x{text-decoration-line:underline;text-decoration-thickness:auto;text-decoration-style:dotted;text-decoration-color:red}";
  sheet_optimizes_to ~into:".x{text-decoration:overline 2px}"
    ".x{text-decoration-line:overline;text-decoration-thickness:2px;text-decoration-style:solid;text-decoration-color:currentcolor}";
  (* The shorthand resets the thickness, so a run leaving it out is not the same
     declaration and stays expanded. *)
  sheet_optimizes_to
    ~into:
      ".x{text-decoration-line:underline;text-decoration-style:solid;text-decoration-color:red}"
    ".x{text-decoration-line:underline;text-decoration-style:solid;text-decoration-color:red}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{text-decoration-line:underline;text-decoration-thickness:2px;text-decoration-style:solid;text-decoration-color:red!important}"
    ".x{text-decoration-line:underline;text-decoration-thickness:2px;text-decoration-style:solid;text-decoration-color:red!important}";
  (* CSS Cascade 5 (ED) sec. 7.3 gives a longhand written [initial] the
     property's initial value, which is the component the shorthand leaves out,
     so it contracts the same way the written-out initials do. Chrome 146
     expands [text-decoration: none] to these four. *)
  sheet_optimizes_to ~into:".x{text-decoration:none}"
    ".x{text-decoration-line:none;text-decoration-thickness:initial;text-decoration-style:initial;text-decoration-color:initial}";
  sheet_optimizes_to ~into:".x{text-decoration:overline 2px}"
    ".x{text-decoration-line:overline;text-decoration-thickness:2px;text-decoration-style:initial;text-decoration-color:initial}";
  sheet_optimizes_to ~into:".x{text-decoration:none red 2px}"
    ".x{text-decoration-line:initial;text-decoration-thickness:2px;text-decoration-style:solid;text-decoration-color:red}";
  (* The other CSS-wide keywords name no value of their own, so a run carrying
     one stays expanded. *)
  sheet_optimizes_to
    ~into:
      ".x{text-decoration-line:overline;text-decoration-thickness:2px;text-decoration-style:inherit;text-decoration-color:red}"
    ".x{text-decoration-line:overline;text-decoration-thickness:2px;text-decoration-style:inherit;text-decoration-color:red}"

(* CSS Fonts 4 (ED) sec. 5.3 splits the [font] sub-properties into the seven the
   shorthand writes and the thirteen it only resets. A run carrying one of the
   thirteen at its initial says what the shorthand says, so the whole family
   written out contracts; a run leaving one of the twenty to the reset asks for
   the whole graph, the way the animation and transition runs do. Chrome 146
   expands the two shorthands below to the runs beside them. *)
let test_font_composes_the_whole_run () =
  sheet_optimizes_to ~into:".x{font:italic 700 12px/1.5 serif}"
    ".x{font-style:italic;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-alternates:normal;font-size-adjust:none;font-language-override:normal;font-kerning:auto;font-optical-sizing:auto;font-feature-settings:normal;font-variation-settings:normal;font-variant-position:normal;font-variant-emoji:normal;font-weight:bold;font-stretch:normal;font-size:12px;line-height:1.5;font-family:serif}";
  sheet_optimizes_to ~into:".x{font:12px sans-serif}"
    ".x{font-style:normal;font-variant-caps:normal;font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-alternates:normal;font-size-adjust:none;font-language-override:normal;font-kerning:auto;font-optical-sizing:auto;font-feature-settings:normal;font-variation-settings:normal;font-variant-position:normal;font-variant-emoji:normal;font-weight:normal;font-stretch:normal;font-size:12px;line-height:normal;font-family:sans-serif}";
  (* A run naming only the components leaves the other thirteen to the reset. *)
  sheet_optimizes_to
    ~into:
      ".x{font-style:italic;font-weight:700;font-size:12px;line-height:1.5;font-family:serif}"
    ".x{font-style:italic;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:italic 700 12px/1.5 serif}"
    ".x{font-style:italic;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif}";
  (* One of the thirteen away from its initial is a value the shorthand cannot
     carry. It moves past the run, where it overrides the reset and says what it
     said in front of it. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:italic 700 12px/1.5 serif;font-kerning:none}"
    ".x{font-kerning:none;font-style:italic;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif}";
  (* sec. 5.3 lets the shorthand write only the CSS 2.1 caps subset, so
     [small-caps] is a component and [unicase] is one of the thirteen. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:italic small-caps 700 12px/1.5 serif}"
    ".x{font-style:italic;font-variant-caps:small-caps;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:italic 700 12px/1.5 serif;font-variant-caps:unicase}"
    ".x{font-variant-caps:unicase;font-style:italic;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif}";
  (* sec. 5.3 writes the width slot as [<font-width-css3>], the nine keywords
     and no percentage, so a width the longhand normalised to its percentage
     takes its keyword back and one no keyword names stays a longhand. Chrome
     146 refuses `font: 125% 12px serif` and reads `font: expanded 12px
     serif`. *)
  sheet_optimizes_to ~scope:`Stylesheet ~into:".x{font:expanded 12px serif}"
    ".x{font-family:serif;font-size:12px;font-stretch:expanded}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:12px serif;font-stretch:110%}"
    ".x{font-family:serif;font-size:12px;font-stretch:110%}"

(* CSS Cascade 5 sec. 2.1 makes [!important] a property of one declaration, and
   sec. 8 gives an important declaration its own origin in the cascade, so a
   shorthand carries the flag for every longhand it writes or for none. A run
   whose importance is not uniform therefore contracts only over the part that
   agrees, and the odd declaration stays a longhand ahead of it, where it says
   what it said. *)
let test_font_importance_must_be_uniform () =
  let run ?(style = "italic") ?(family = "serif") () =
    String.concat ""
      [
        ".x{font-style:";
        style;
        ";font-weight:bold;font-size:12px;line-height:1.5;font-family:";
        family;
        "}";
      ]
  in
  (* Uniform, both ways: the shorthand carries the flag or does not. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:italic 700 12px/1.5 serif}" (run ());
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font:italic 700 12px/1.5 serif!important}"
    ".x{font-style:italic!important;font-weight:bold!important;font-size:12px!important;line-height:1.5!important;font-family:serif!important}";
  (* Mixed: the important slot cannot ride a shorthand the others need
     unimportant, so it stays a longhand and the rest contract behind it. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".x{font-style:italic!important;font:700 12px/1.5 serif}"
    ".x{font-style:italic!important;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".x{font-style:italic;font-weight:700;font-size:12px;line-height:1.5;font-family:serif!important}"
    ".x{font-style:italic;font-weight:bold;font-size:12px;line-height:1.5;font-family:serif!important}"

(* CSS Fonts 4 (ED) sec. 6.10: [font-variant] writes its seven longhands, and a
   longhand at [normal] is the component the shorthand leaves out. Chrome 146
   expands each shorthand below to the run beside it. *)
let test_font_variant_composes () =
  sheet_optimizes_to ~into:".x{font-variant:small-caps}"
    ".x{font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:small-caps;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}";
  sheet_optimizes_to ~into:".x{font-variant:normal}"
    ".x{font-variant-ligatures:normal;font-variant-caps:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}";
  sheet_optimizes_to ~into:".x{font-variant:common-ligatures}"
    ".x{font-variant-ligatures:common-ligatures;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:normal;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}";
  sheet_optimizes_to ~into:".x{font-variant:none}"
    ".x{font-variant-ligatures:none;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:normal;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}";
  (* [none] is the whole value, so it has no spelling beside another
     component. *)
  sheet_optimizes_to
    ~into:
      ".x{font-variant-ligatures:none;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:small-caps;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}"
    ".x{font-variant-ligatures:none;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:small-caps;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}";
  (* A CSS-wide keyword is no component, and a run missing one of the seven is
     not the same declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:inherit;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}"
    ".x{font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:inherit;font-variant-alternates:normal;font-variant-position:normal;font-variant-emoji:normal}";
  sheet_optimizes_to
    ~into:
      ".x{font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:small-caps;font-variant-position:normal;font-variant-emoji:normal}"
    ".x{font-variant-ligatures:normal;font-variant-numeric:normal;font-variant-east-asian:normal;font-variant-caps:small-caps;font-variant-position:normal;font-variant-emoji:normal}"

(* CSS Grid 2 (ED) sec. 7.8: [grid] writes the three template longhands and the
   three auto ones, and only one of its two auto-flow axes can be spelled, so
   the other axis and its track size have to be at the initials the shorthand
   leaves them. Chrome 146 computes the same six values for each pair below. *)
let test_grid_composes () =
  sheet_optimizes_to ~into:".x{grid:none/1fr 1fr}"
    ".x{grid-template-columns:1fr \
     1fr;grid-template-rows:none;grid-template-areas:none;grid-auto-flow:row;grid-auto-columns:auto;grid-auto-rows:auto}";
  sheet_optimizes_to ~into:".x{grid:none}"
    ".x{grid-template-rows:none;grid-template-columns:none;grid-template-areas:none;grid-auto-flow:row;grid-auto-columns:auto;grid-auto-rows:auto}";
  sheet_optimizes_to ~into:".x{grid:1fr/auto-flow 2em}"
    ".x{grid-template-columns:none;grid-template-rows:1fr;grid-template-areas:none;grid-auto-flow:column;grid-auto-columns:2em;grid-auto-rows:auto}";
  sheet_optimizes_to ~into:".x{grid:1fr/2fr}"
    ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:none;grid-auto-flow:row;grid-auto-columns:auto;grid-auto-rows:auto}";
  sheet_optimizes_to ~into:".x{grid:auto-flow dense 2em/1fr}"
    ".x{grid-template-rows:none;grid-template-columns:1fr;grid-template-areas:none;grid-auto-flow:row \
     dense;grid-auto-columns:auto;grid-auto-rows:2em}";
  (* Where [grid] has no spelling, the three template longhands still contract
     on their own: the row axis flows and the column tracks are sized, an area
     sits beside a flowing axis, and the importance is mixed. *)
  sheet_optimizes_to
    ~into:
      ".x{grid-template:none/1fr;grid-auto-flow:row;grid-auto-columns:2em;grid-auto-rows:3em}"
    ".x{grid-template-rows:none;grid-template-columns:1fr;grid-template-areas:none;grid-auto-flow:row;grid-auto-columns:2em;grid-auto-rows:3em}";
  sheet_optimizes_to
    ~into:
      ".x{grid-template:\"a\" \
       1fr/1fr;grid-auto-flow:column;grid-auto-columns:auto;grid-auto-rows:auto}"
    ".x{grid-template-rows:1fr;grid-template-columns:1fr;grid-template-areas:\"a\";grid-auto-flow:column;grid-auto-columns:auto;grid-auto-rows:auto}";
  sheet_optimizes_to
    ~into:
      ".x{grid-template:1fr/2fr;grid-auto-flow:row;grid-auto-columns:auto;grid-auto-rows:auto!important}"
    ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:none;grid-auto-flow:row;grid-auto-columns:auto;grid-auto-rows:auto!important}"

(* CSS Grid 2 (ED) sec. 7.4: [grid-template] is [none], a [<rows> / <columns>]
   pair, or the areas form writing each row's string beside that row's size, and
   it resets all three longhands. Chrome 146 expands each shorthand below to the
   run beside it. *)
let test_grid_template_composes () =
  sheet_optimizes_to ~into:".x{grid-template:1fr/2fr}"
    ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:none}";
  sheet_optimizes_to ~into:".x{grid-template:none}"
    ".x{grid-template-rows:none;grid-template-columns:none;grid-template-areas:none}";
  sheet_optimizes_to ~into:".x{grid-template:\"a b\" 1fr/1fr 1fr}"
    ".x{grid-template-rows:1fr;grid-template-columns:1fr \
     1fr;grid-template-areas:\"a b\"}";
  sheet_optimizes_to ~into:".x{grid-template:\"a\" 1fr \"b\" 2fr/1fr}"
    ".x{grid-template-rows:1fr \
     2fr;grid-template-columns:1fr;grid-template-areas:\"a\" \"b\"}";
  (* The areas form writes one size per row, so a row count the tracks do not
     match has no shorthand spelling. *)
  sheet_optimizes_to
    ~into:
      ".x{grid-template-rows:1fr;grid-template-columns:1fr;grid-template-areas:\"a\"\"b\"}"
    ".x{grid-template-rows:1fr;grid-template-columns:1fr;grid-template-areas:\"a\" \
     \"b\"}";
  (* A CSS-wide keyword is the whole declaration value and no shorthand
     component, and a run missing one of the three is not the same
     declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:inherit}"
    ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:inherit}";
  sheet_optimizes_to
    ~into:".x{grid-template-rows:1fr;grid-template-columns:2fr}"
    ".x{grid-template-rows:1fr;grid-template-columns:2fr}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:none!important}"
    ".x{grid-template-rows:1fr;grid-template-columns:2fr;grid-template-areas:none!important}"

(* CSS Backgrounds 3 (ED) sec. 4.1: an elliptical corner names a horizontal
   radius and a vertical one, and [border-radius] writes the four horizontals,
   then [/], then the four verticals. *)
let test_border_radius_ellipse_composes () =
  sheet_optimizes_to ~into:".x{border-radius:1px 2px 3px 4px/5px}"
    ".x{border-top-left-radius:1px 5px;border-top-right-radius:2px \
     5px;border-bottom-right-radius:3px 5px;border-bottom-left-radius:4px 5px}";
  sheet_optimizes_to ~into:".x{border-radius:10%/20%}"
    ".x{border-top-left-radius:10% 20%;border-top-right-radius:10% \
     20%;border-bottom-right-radius:10% 20%;border-bottom-left-radius:10% 20%}";
  sheet_optimizes_to ~into:".x{border-radius:1px 2px/3px 4px}"
    ".x{border-top-left-radius:1px 3px;border-top-right-radius:2px \
     4px;border-bottom-right-radius:1px 3px;border-bottom-left-radius:2px 4px}";
  (* Mixing a round corner with an elliptical one has no box spelling. *)
  sheet_optimizes_to
    ~into:
      ".x{border-top-left-radius:1px \
       5px;border-top-right-radius:2px;border-bottom-right-radius:3px \
       5px;border-bottom-left-radius:4px 5px}"
    ".x{border-top-left-radius:1px \
     5px;border-top-right-radius:2px;border-bottom-right-radius:3px \
     5px;border-bottom-left-radius:4px 5px}"

(* CSS Flexbox 1 sec. 7.1.1 gives [none] and [auto] as the one-word names of [0
   0 auto] and [1 1 auto], so a run composing to either takes the shorter
   spelling. *)
let test_flex_keyword_forms () =
  sheet_optimizes_to ~into:".x{flex:none}"
    ".x{flex-grow:0;flex-shrink:0;flex-basis:auto}";
  sheet_optimizes_to ~into:".x{flex:auto}"
    ".x{flex-grow:1;flex-shrink:1;flex-basis:auto}";
  (* [initial] is [0 1 auto], which has no one-word name of its own. *)
  sheet_optimizes_to ~into:".x{flex:0 auto}"
    ".x{flex-grow:0;flex-shrink:1;flex-basis:auto}"

(* CSS Backgrounds 3 sec. 6.1: [border-image] resets all five of its longhands,
   so a run naming every one of them is the same declaration whatever else the
   sheet holds. A shorter run leaves the rest to the reset, which only the
   whole-sheet scope can judge. *)
let test_border_image_full_run_composes () =
  (* Every component but the source and slice is at its initial, so the
     shorthand names them by leaving them out. *)
  sheet_optimizes_to ~into:".x{border-image:url(a.png)30}"
    ".x{border-image-source:url(a.png);border-image-slice:30;border-image-width:1;border-image-outset:0;border-image-repeat:stretch}";
  sheet_optimizes_to ~into:".x{border-image:none}"
    ".x{border-image-source:none;border-image-slice:100%;border-image-width:1;border-image-outset:0;border-image-repeat:stretch}";
  (* A run missing a longhand would have the shorthand reset it. *)
  sheet_optimizes_to
    ~into:
      ".x{border-image-source:url(a.png);border-image-slice:30;border-image-width:1;border-image-outset:0}"
    ".x{border-image-source:url(a.png);border-image-slice:30;border-image-width:1;border-image-outset:0}"

(* Motion Path 1 sec. 2.6: [offset] resets all five of its longhands, so the run
   naming the same declaration is all five, and a component at its initial is
   what leaving it out names. *)
let test_offset_composes () =
  sheet_optimizes_to ~into:".x{offset:path(\"M 0 0 L 10 10\")10px}"
    ".x{offset-position:normal;offset-path:path(\"M 0 0 L 10 \
     10\");offset-distance:10px;offset-rotate:auto;offset-anchor:auto}";
  sheet_optimizes_to ~into:".x{offset:none}"
    ".x{offset-position:normal;offset-path:none;offset-distance:0;offset-rotate:auto;offset-anchor:auto}";
  (* A run missing a longhand would have the shorthand reset it. *)
  sheet_optimizes_to
    ~into:
      ".x{offset-position:normal;offset-path:none;offset-distance:0;offset-rotate:auto}"
    ".x{offset-position:normal;offset-path:none;offset-distance:0;offset-rotate:auto}"

(* CSS Multicol 2 sec. 4.5: [columns] sets the width, the count and the height,
   and Chrome 146 has it reset [column-wrap] too, so the run naming the same
   declaration is all four and the shorthand only stands for a height at its
   [auto] initial. *)
let test_columns_composes () =
  sheet_optimizes_to ~into:".x{columns:10em 2}"
    ".x{column-width:10em;column-count:2;column-height:auto;column-wrap:auto}";
  sheet_optimizes_to ~into:".x{columns:auto}"
    ".x{column-width:auto;column-count:auto;column-height:auto;column-wrap:auto}";
  (* [auto] is the width's initial, so the count alone names the same pair. *)
  sheet_optimizes_to ~into:".x{columns:3}"
    ".x{column-width:auto;column-count:3;column-height:auto;column-wrap:auto}";
  (* The shorthand has no spelling for a height of its own, so that run keeps
     its longhands. *)
  sheet_optimizes_to
    ~into:
      ".x{column-width:10em;column-count:2;column-height:5em;column-wrap:auto}"
    ".x{column-width:10em;column-count:2;column-height:5em;column-wrap:auto}"

(* CSS Cascade 5 sec. 7.3: [initial] is the property's initial value, which is
   what [background] writes to a slot left out, so a run whose untouched slots
   are spelled that way is reset-closed and contracts. *)
let test_background_initial_slots () =
  sheet_optimizes_to ~into:".x{background:url(a.png)50%/cover no-repeat}"
    ".x{background-image:url(a.png);background-position:50%;background-size:cover;background-repeat:no-repeat;background-attachment:initial;background-origin:initial;background-clip:initial;background-color:initial}";
  (* Every slot at its initial is what [background: none] names, which the
     printer spells with the position it keeps. *)
  sheet_optimizes_to ~into:".x{background:0 0}"
    ".x{background-image:none;background-position-x:initial;background-position-y:initial;background-size:initial;background-repeat:initial;background-attachment:initial;background-origin:initial;background-clip:initial;background-color:initial}";
  (* [inherit] names the parent's value, not the initial, so it cannot fill a
     slot the shorthand resets. *)
  sheet_optimizes_to
    ~into:
      ".x{background-image:url(a.png);background-position:50%;background-size:cover;background-repeat:no-repeat;background-attachment:inherit;background-origin:initial;background-clip:initial;background-color:initial}"
    ".x{background-image:url(a.png);background-position:50%;background-size:cover;background-repeat:no-repeat;background-attachment:inherit;background-origin:initial;background-clip:initial;background-color:initial}"

(* The CSSOM reports [-webkit-mask-position-x] and [-webkit-mask-position-y]
   when it expands [mask], and the two share a cascade slot with
   [mask-position], so the pair contracts into it. *)
let test_webkit_mask_position_axes () =
  sheet_optimizes_to ~into:".x{-webkit-mask-position:50%10px}"
    ".x{-webkit-mask-position-x:50%;-webkit-mask-position-y:10px}";
  sheet_optimizes_to ~into:".x{-webkit-mask-position:0 0}"
    ".x{-webkit-mask-position-x:initial;-webkit-mask-position-y:initial}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{-webkit-mask-position-x:50%;-webkit-mask-position-y:10px!important}"
    ".x{-webkit-mask-position-x:50%;-webkit-mask-position-y:10px!important}"

(* CSS Masking 1 sec. 7.9: [mask] resets all eight layer longhands, so a run
   naming every one of them is the same declaration whatever else the sheet
   holds, and a component at its initial is what leaving it out names. *)
let test_mask_full_run_composes () =
  sheet_optimizes_to
    ~into:
      ".x{-webkit-mask:url(a.png)50%/cover no-repeat;mask:url(a.png)50%/cover \
       no-repeat}"
    ".x{mask-image:url(a.png);mask-position:50%;mask-size:cover;mask-repeat:no-repeat;mask-origin:border-box;mask-clip:border-box;mask-composite:add;mask-mode:match-source}";
  (* A run missing a longhand would have the shorthand reset it. *)
  sheet_optimizes_to
    ~into:
      ".x{-webkit-mask-image:url(a.png);mask-image:url(a.png);-webkit-mask-size:cover;mask-size:cover}"
    ".x{mask-image:url(a.png);mask-size:cover}"

(* CSS Backgrounds 3 sec. 3.4: [border] resets the whole [border-image] family,
   so contracting a width/style/colour run over a rule that holds one of those
   longhands would drop it. *)
let test_border_contraction_covers_border_image () =
  sheet_optimizes_to
    ~into:
      ".x{border-image-source:url(b.png);color:red;border-width:4px;border-style:solid;border-color:red}"
    ".x{border-image-source:url(b.png);color:red;border-width:4px;border-style:solid;border-color:red}";
  (* Nothing holds the family, so the run contracts. *)
  sheet_optimizes_to ~into:".x{color:red;border:4px solid red}"
    ".x{color:red;border-width:4px;border-style:solid;border-color:red}"

(* CSS Animations 2 sec. 4.11: [animation] resets every longhand it names,
   [animation-name] included, so contracting a run that has no name beside a
   name written earlier says [none] and animates nothing. The transition family
   already reasons this way; animation had no coverage arms at all. *)
let test_animation_contraction_covers_other_rules () =
  (* [animation] has more longhands than [<single-animation>] can carry, so
     every run here leaves the shorthand resetting something and each case asks
     for the whole graph; the control at the end pins the fragment scope. *)
  (* Split across two rules with the same selector: the shortest spelling that
     keeps the name groups the two and carries it into the shorthand. *)
  sheet_optimizes_to ~scope:`Stylesheet ~into:".a{animation:spin 2s linear}"
    ".a{animation-name:spin}.a{animation-duration:2s;animation-timing-function:linear}";
  (* Same rule: the name is reset by a contraction that does not carry it, so
     the run stays expanded. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{animation-name:spin;color:red;animation-duration:2s}"
    ".a{animation-name:spin;color:red;animation-duration:2s}";
  (* An important name outranks the non-important shorthand whatever the order,
     so the run contracts. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{animation-name:spin!important;color:red;animation:2s linear}"
    ".a{animation-name:spin!important;color:red;animation-duration:2s;animation-timing-function:linear}";
  (* A non-important name is reset by the contraction, so the run stays
     expanded. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".a{animation-name:spin;color:red;animation-duration:2s;animation-timing-function:linear}"
    ".a{animation-name:spin;color:red;animation-duration:2s;animation-timing-function:linear}";
  (* The control: the first input under the fragment scope. *)
  sheet_optimizes_to
    ~into:
      ".a{animation-duration:2s;animation-timing-function:linear;animation-name:spin}"
    ".a{animation-name:spin}.a{animation-duration:2s;animation-timing-function:linear}"

(* Scroll-driven Animations 1 (ED) appendix A.3 makes the two animation-range
   longhands reset-only sub-properties of [animation], which Chrome 146 agrees
   with: [animation-range-start: 20%] beside the shorthand computes to [normal].
   So a range the rule holds blocks the contraction, a range written after the
   shorthand is the reset spelled out, and one written before it is dead. *)
let test_animation_resets_the_range () =
  sheet_optimizes_to
    ~into:
      ".a{animation-range-start:20%;animation-name:x;animation-duration:1s;animation-timing-function:linear}"
    ".a{animation-range-start:20%;animation-name:x;animation-duration:1s;animation-timing-function:linear}";
  decl_optimizes_to ~into:"animation:x 1s" "animation-range:20%;animation:x 1s";
  decl_optimizes_to ~into:"animation:x 1s"
    "animation:x 1s;animation-range:normal";
  decl_optimizes_to ~into:"animation:x 1s"
    "animation:x 1s;animation-range-start:normal;animation-range-end:normal";
  decl_optimizes_to ~into:"animation:x 1s"
    "animation:x 1s;animation-timeline:auto";
  (* A range or a timeline the shorthand does not write is a real override. *)
  decl_optimizes_to ~into:"animation:x 1s;animation-range:20%"
    "animation:x 1s;animation-range:20%";
  decl_optimizes_to ~into:"animation:x 1s;animation-timeline:view()"
    "animation:x 1s;animation-timeline:view()";
  decl_optimizes_to ~into:"animation:x 1s;animation-range:normal!important"
    "animation:x 1s;animation-range:normal!important"

let test_transition_contraction_covers_other_rules () =
  (* Every case here contracts a run that leaves a slot to the shorthand's
     reset, so each one asks for the whole graph. Under the fragment scope the
     run stays expanded whatever the neighbours say, which the two controls at
     the end pin. *)
  (* Same rule, both sides of the guard. A non-important holder is reset by the
     contraction, so the run stays expanded; an important one outranks the
     non-important shorthand whatever the order, so the run contracts. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".a{transition-behavior:allow-discrete;color:red;transition-property:color;transition-duration:1s}"
    ".a{transition-behavior:allow-discrete;color:red;transition-property:color;transition-duration:1s}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".a{transition-behavior:allow-discrete!important;color:red;transition:color \
       1s}"
    ".a{transition-behavior:allow-discrete!important;color:red;transition-property:color;transition-duration:1s}";
  (* Split across two rules with the same selector, the element sees exactly the
     cascade above. The behaviour has to survive, and the shortest spelling that
     keeps it groups the two rules and carries it into the shorthand. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{transition:color 1s allow-discrete}"
    ".a{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{transition:color 1s;transition-behavior:allow-discrete!important}"
    ".a{transition-behavior:allow-discrete!important}.a{transition-property:color;transition-duration:1s}";
  (* Both important: the shorthand no longer loses to the holder, so dropping
     the behaviour would change what the element animates. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{transition:color 1s allow-discrete!important}"
    ".a{transition-behavior:allow-discrete!important}.a{transition-property:color!important;transition-duration:1s!important}";
  (* An unrelated rule between them changes nothing: the holder is still in the
     cascade the second rule lands on. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{transition:color 1s allow-discrete}.b{color:red}"
    ".a{transition-behavior:allow-discrete}.b{color:red}.a{transition-property:color;transition-duration:1s}";
  (* A rule between them that writes the same family keeps the two apart, so
     there is no grouping to carry the behaviour into and the run stays
     expanded. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".a{transition-behavior:allow-discrete}.b{transition:opacity \
       2s}.a{transition-property:color;transition-duration:1s}"
    ".a{transition-behavior:allow-discrete}.b{transition:opacity \
     2s}.a{transition-property:color;transition-duration:1s}";
  (* The holder needs no relation to the run's selector: an element carrying
     both classes reads one cascade. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".b{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}"
    ".b{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}";
  (* The other side of the guard, so it stays a hazard test and not a blanket
     refusal: a neighbour holding the slot at its initial resets to the same
     thing, and an important neighbour outranks the shorthand. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".b{transition-behavior:normal}.a{transition:color 1s}"
    ".b{transition-behavior:normal}.a{transition-property:color;transition-duration:1s}";
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:
      ".b{transition-behavior:allow-discrete!important}.a{transition:color 1s}"
    ".b{transition-behavior:allow-discrete!important}.a{transition-property:color;transition-duration:1s}";
  (* A slot the rule itself rewrites after the run is not at risk from a
     neighbour holding it: the rewrite lands after the reset. *)
  sheet_optimizes_to ~scope:`Stylesheet
    ~into:".a{transition:color 1s;color:red;transition-delay:5s}"
    ".a{transition-property:color;transition-duration:1s;color:red;transition-delay:5s}";
  (* The controls: the same two inputs under the fragment scope the optimizer
     assumes by default. *)
  sheet_optimizes_to
    ~into:
      ".a{transition-behavior:allow-discrete;transition-property:color;transition-duration:1s}"
    ".a{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}";
  sheet_optimizes_to
    ~into:
      ".b{transition-behavior:normal}.a{transition-property:color;transition-duration:1s}"
    ".b{transition-behavior:normal}.a{transition-property:color;transition-duration:1s}"

(* CSS Backgrounds 3 (ED) sec. 3.4: [border] resets the whole border-image
   family, so a neighbour holding one of those longhands is at risk from a
   contraction that does not carry it. The family shares no single slot: a rule
   holding the slice says nothing about the neighbour holding the source, and
   answering for the two together let the source through. Chrome 146 computes
   the image on both sides of each pair below. *)
let test_border_image_held_across_rules () =
  (* The run holds the slice and the repeat itself; the source next door is the
     one the contraction would reset, so the shorthand goes in front of the
     whole family and the reset is overridden. *)
  sheet_optimizes_to
    ~into:
      ".x{border:4px \
       solid#0000;border-image-source:url(b.png);border-image-slice:30;border-image-repeat:round}"
    ".x{border-image-source:url(b.png)}.x{border-image-slice:30;border-image-repeat:round;border-width:4px;border-style:solid;border-color:transparent}";
  (* The same run with nothing of its own to hold. *)
  sheet_optimizes_to
    ~into:".x{border:4px solid#0000;border-image-source:url(b.png)}"
    ".x{border-image-source:url(b.png)}.x{border-width:4px;border-style:solid;border-color:transparent}";
  (* A neighbour that cannot be reordered into place keeps the run expanded. *)
  sheet_optimizes_to
    ~into:
      ".y{border-image-source:url(b.png)}.x{border-width:4px;border-style:solid;border-color:#0000}"
    ".y{border-image-source:url(b.png)}.x{border-width:4px;border-style:solid;border-color:transparent}"

let suite =
  ( "shorthand_compose",
    [
      Alcotest.test_case "transition contraction covers reset longhands" `Quick
        test_transition_contraction_covers_reset_longhands;
      Alcotest.test_case "transition contraction covers other rules" `Quick
        test_transition_contraction_covers_other_rules;
      Alcotest.test_case "animation contraction covers other rules" `Quick
        test_animation_contraction_covers_other_rules;
      Alcotest.test_case "animation resets the range" `Quick
        test_animation_resets_the_range;
      Alcotest.test_case "scroll axis pair composes" `Quick
        test_scroll_axis_pair_composes;
      Alcotest.test_case "border axis pair composes" `Quick
        test_border_axis_pair_composes;
      Alcotest.test_case "line shorthand composes" `Quick
        test_line_shorthand_composes;
      Alcotest.test_case "line shorthand initial slot" `Quick
        test_line_shorthand_initial_slot;
      Alcotest.test_case "logical border whole composes" `Quick
        test_logical_border_whole_composes;
      Alcotest.test_case "xy pair composes" `Quick test_xy_pair_composes;
      Alcotest.test_case "grid placement composes" `Quick
        test_grid_placement_composes;
      Alcotest.test_case "duo keyword composes" `Quick test_duo_keyword_composes;
      Alcotest.test_case "timeline and range compose" `Quick
        test_timeline_range_composes;
      Alcotest.test_case "named shorthand composes" `Quick
        test_named_shorthand_composes;
      Alcotest.test_case "background position composes" `Quick
        test_background_position_composes;
      Alcotest.test_case "column rule composes" `Quick test_column_rule_composes;
      Alcotest.test_case "white space composes" `Quick test_white_space_composes;
      Alcotest.test_case "text wrap composes" `Quick test_text_wrap_composes;
      Alcotest.test_case "font synthesis composes" `Quick
        test_font_synthesis_composes;
      Alcotest.test_case "webkit text stroke composes" `Quick
        test_webkit_text_stroke_composes;
      Alcotest.test_case "font composes the whole run" `Quick
        test_font_composes_the_whole_run;
      Alcotest.test_case "font importance must be uniform" `Quick
        test_font_importance_must_be_uniform;
      Alcotest.test_case "font-variant composes" `Quick
        test_font_variant_composes;
      Alcotest.test_case "grid composes" `Quick test_grid_composes;
      Alcotest.test_case "grid-template composes" `Quick
        test_grid_template_composes;
      Alcotest.test_case "text decoration thickness composes" `Quick
        test_text_decoration_thickness_composes;
      Alcotest.test_case "border radius ellipse composes" `Quick
        test_border_radius_ellipse_composes;
      Alcotest.test_case "flex keyword forms" `Quick test_flex_keyword_forms;
      Alcotest.test_case "border image full run composes" `Quick
        test_border_image_full_run_composes;
      Alcotest.test_case "offset composes" `Quick test_offset_composes;
      Alcotest.test_case "columns composes" `Quick test_columns_composes;
      Alcotest.test_case "background initial slots" `Quick
        test_background_initial_slots;
      Alcotest.test_case "webkit mask position axes" `Quick
        test_webkit_mask_position_axes;
      Alcotest.test_case "mask full run composes" `Quick
        test_mask_full_run_composes;
      Alcotest.test_case "border contraction covers border-image" `Quick
        test_border_contraction_covers_border_image;
      Alcotest.test_case "border-image held across rules" `Quick
        test_border_image_held_across_rules;
    ] )
