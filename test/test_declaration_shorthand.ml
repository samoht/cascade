(** Declaration tests: shorthand slots, line widths and shorthand resets. *)

open Alcotest
open Cascade
open Css.Declaration
open Css_test_helpers
open Declaration_helpers

let check_visible_var name css expected =
  let declaration = ok (Css.Declaration.of_string css) in
  let names =
    Css.vars_of_declarations [ declaration ] |> List.map Css.any_var_name
  in
  Alcotest.(check (list string)) name [ expected ] names

(* CSS Basic User Interface 4 (ED) sec. 3.2 gives outline-width the value
   [<line-width>] and says it "accepts the same values as border-width [...]
   with the same meaning"; CSS Backgrounds 3 (ED) sec. 3.3 defines
   [<line-width>] as [thin | medium | thick | <length>]. The width slot of the
   outline shorthand (sec. 3.1) is [<'outline-width'>], so the keywords and the
   math functions read there too. *)
let outline_line_width () =
  check_declaration ~expected:"outline-width:thin" "outline-width: thin";
  check_declaration ~expected:"outline-width:medium" "outline-width: medium";
  check_declaration ~expected:"outline-width:thick" "outline-width: thick";
  check_declaration ~expected:"outline:thin solid red" "outline: thin solid red";
  (* CSS UI 4 (ED) sec. 3.1: the outline shorthand sets all three longhands, and
     CSS Cascade 5 (ED) sec. 3 assigns an omitted sub-property its initial
     value, which sec. 3.2 gives as [medium]. Spelling [medium] beside another
     slot says what leaving the slot out already says, so the optimizer drops it
     and pp prints the node it was handed. A lone [medium] leaves the shorthand
     declaring nothing but initial values, which is what [outline: none]
     declares. *)
  check_declaration ~expected:"outline:medium solid red"
    ~optimized:"outline:solid red" "outline: medium solid red";
  check_declaration ~expected:"outline:medium dashed"
    ~optimized:"outline:dashed" "outline: medium dashed";
  check_declaration ~expected:"outline:medium red" ~optimized:"outline:red"
    "outline: medium red";
  (* sec. 3.1: a lone [auto], and an [auto] beside a width but no explicit style
     or colour, both set outline-style and outline-color to [auto], so the two
     spellings are the same declaration here too. *)
  check_declaration ~expected:"outline:medium auto" ~optimized:"outline:auto"
    "outline: medium auto";
  check_declaration ~expected:"outline:medium" ~optimized:"outline:none"
    "outline: medium";
  check_declaration ~expected:"outline:thick solid red"
    "outline: thick solid red";
  (* CSS Values 4 sec. 10.2: a comparison over same-unit constants denotes one
     length, so the optimizer reduces it to that length, as at every other
     [<line-width>]; pp holds the call it was written as. *)
  check_declaration ~expected:"outline-width:calc(1px + 1px)"
    ~optimized:"outline-width:2px" "outline-width: calc(1px + 1px)";
  check_declaration ~expected:"outline-width:min(3px)"
    ~optimized:"outline-width:3px" "outline-width: min(3px)";
  check_declaration ~expected:"outline-width:clamp(1px,2px,3px)"
    ~optimized:"outline-width:2px" "outline-width: clamp(1px,2px,3px)";
  check_declaration ~expected:"outline:calc(1px + 1px) solid red"
    ~optimized:"outline:2px solid red" "outline: calc(1px + 1px) solid red";
  check_declaration ~expected:"outline:min(3px) solid red"
    ~optimized:"outline:3px solid red" "outline: min(3px) solid red";
  check_declaration ~expected:"outline:clamp(1px,2px,3px) solid red"
    ~optimized:"outline:2px solid red" "outline: clamp(1px,2px,3px) solid red";
  (* No unit relates dvh to px, so the comparison stands as written. *)
  check_declaration ~roundtrip:true "outline-width:max(3dvh,4px)";
  (* A [<line-width>] is non-negative. *)
  neg_cursor read_declaration "outline-width:-1px";
  (* Controls: a plain length still reads, and the other shorthand slots are
     untouched. *)
  check_declaration ~expected:"outline-width:0px" ~optimized:"outline-width:0"
    "outline-width: 0px";
  check_declaration ~expected:"outline-width:2px" "outline-width: 2px";
  check_declaration ~expected:"outline:0px solid red"
    ~optimized:"outline:0 solid red" "outline: 0px solid red";
  check_declaration ~expected:"outline:2px solid red" "outline: 2px solid red";
  check_declaration ~expected:"outline:none" "outline: none";
  check_declaration ~expected:"outline:auto" "outline: auto";
  check_declaration ~expected:"outline:red" "outline: red";
  check_declaration ~expected:"outline:solid red" "outline: solid red"

(* CSS Backgrounds 3 (ED) sec. 3.3 gives border-width the value
   [<line-width>{1,4}] and defines [<line-width>] as [<length [0,inf]> | thin |
   medium | thick]; the border shorthands (sec. 3.4) read [<line-width> ||
   <line-style> || <color>], so their width slot is that same production.
   css-logical-1 sec. 4.4 repeats it for the logical shorthands, and CSS
   Multi-column 1 (ED) sec. 4.5 gives column-rule [<'column-rule-width'> ||
   <'column-rule-style'> || <'column-rule-color'>], whose width is a
   [<line-width>] (sec. 4.4). Every fold the longhand performs is therefore due
   in the shorthand width slot. *)
let border_line_width () =
  (* CSS Values 4 sec. 10.2: a sum or comparison over same-unit constants
     denotes one length, so the optimizer reduces it to that length; pp holds
     the call it was written as. *)
  check_declaration ~expected:"border-width:calc(1px + 1px)"
    ~optimized:"border-width:2px" "border-width: calc(1px + 1px)";
  check_declaration ~expected:"border:calc(1px + 1px) solid red"
    ~optimized:"border:2px solid red" "border: calc(1px + 1px) solid red";
  check_declaration ~expected:"border:min(3px) solid red"
    ~optimized:"border:3px solid red" "border: min(3px) solid red";
  check_declaration ~expected:"border:clamp(1px,2px,3px) solid red"
    ~optimized:"border:2px solid red" "border: clamp(1px,2px,3px) solid red";
  check_declaration ~expected:"border-top:calc(1px + 1px) solid red"
    ~optimized:"border-top:2px solid red"
    "border-top: calc(1px + 1px) solid red";
  check_declaration ~expected:"border-inline-start:min(3px) dotted red"
    ~optimized:"border-inline-start:3px dotted red"
    "border-inline-start: min(3px) dotted red";
  check_declaration ~expected:"column-rule:clamp(1px,2px,3px) solid red"
    ~optimized:"column-rule:2px solid red"
    "column-rule: clamp(1px,2px,3px) solid red";
  (* No unit relates dvh to px, so the comparison stands as written. *)
  check_declaration ~expected:"border:max(3dvh,4px) solid red"
    ~optimized:"border:max(3dvh,4px) solid red"
    "border: max(3dvh,4px) solid red";
  (* A zero length is the same length whatever unit carries it, so [0] is its
     shortest spelling, in the shorthand slot as in the longhand. *)
  check_declaration ~expected:"border-width:0px" ~optimized:"border-width:0"
    "border-width: 0px";
  check_declaration ~expected:"border:0px" ~optimized:"border:0" "border: 0px";
  check_declaration ~expected:"border:0px solid red"
    ~optimized:"border:0 solid red" "border: 0px solid red";
  check_declaration ~expected:"border-top:0em dashed blue"
    ~optimized:"border-top:0 dashed#00f" "border-top: 0em dashed blue";
  check_declaration ~expected:"column-rule:0rem solid red"
    ~optimized:"column-rule:0 solid red" "column-rule: 0rem solid red";
  (* [thin] and [thick] name a UA-chosen thickness no length spells, so no fold
     reaches them. *)
  check_declaration ~expected:"border:thin solid red"
    ~optimized:"border:thin solid red" "border: thin solid red";
  check_declaration ~expected:"border:thick solid red"
    ~optimized:"border:thick solid red" "border: thick solid red";
  (* [medium] is the initial [<line-width>], and sec. 3.4 sets an omitted
     shorthand slot to its initial value, so an explicit [medium] beside another
     slot is what omitting it already means: the optimizer drops it, pp holds
     the node. A lone [medium] leaves the shorthand declaring nothing but
     initial values, which is what [border: none] declares. *)
  check_declaration ~expected:"border:medium solid red"
    ~optimized:"border:solid red" "border: medium solid red";
  check_declaration ~expected:"border-top:medium dashed blue"
    ~optimized:"border-top:dashed#00f" "border-top: medium dashed blue";
  (* CSS Values 4 sec. 10.12 checks the [0,inf] range of a <line-width> and of
     line-height on the value the math function resolves to, not on each
     operand, so a negative inside calc() reads and a literal one does not. *)
  check_declaration ~expected:"border-right-width:calc(-1px)"
    "border-right-width: calc(-1px)";
  check_declaration ~expected:"outline-width:calc(-1px)"
    "outline-width: calc(-1px)";
  check_declaration ~expected:"border-right:calc(-1px) solid red"
    "border-right: calc(-1px) solid red";
  check_declaration ~expected:"line-height:calc(-10%)" "line-height: calc(-10%)";
  neg_cursor read_declaration "border-right-width: -1px";
  neg_cursor read_declaration "outline-width: -1px";
  neg_cursor read_declaration "line-height: -10%";
  check_declaration ~expected:"column-rule:medium solid red"
    ~optimized:"column-rule:solid red" "column-rule: medium solid red";
  (* CSS Gaps 1 sec. 4 gives each gap decoration longhand a comma-separated
     list, one entry per rule line. *)
  check_declaration ~expected:"column-rule-style:dotted,dashed"
    "column-rule-style: dotted, dashed";
  check_declaration ~expected:"column-rule-width:1px,2px"
    "column-rule-width: 1px, 2px";
  check_declaration ~expected:"column-rule-color:red,blue"
    ~optimized:"column-rule-color:red,#00f" "column-rule-color: red, blue";
  (* CSS Animations 2 sec. 5 spells [animation-timeline] as
     [<single-animation-timeline>#], one entry per animation. Chrome 153 takes
     each of these. *)
  check_declaration ~expected:"animation-timeline:none,auto"
    "animation-timeline: none, auto";
  check_declaration ~expected:"animation-timeline:--a,none"
    "animation-timeline: --a, none";
  check_declaration ~expected:"animation-timeline:scroll(),view()"
    "animation-timeline: scroll(), view()";
  (* Sec. 4.4 writes the shorthand [<gap-rule>#] over the same list. *)
  check_declaration ~expected:"column-rule:0,0" "column-rule: 0, 0";
  check_declaration ~expected:"column-rule:1px solid red,2px dashed blue"
    ~optimized:"column-rule:1px solid red,2px dashed#00f"
    "column-rule: 1px solid red, 2px dashed blue";
  neg_cursor read_declaration "column-rule-style: dotted dashed";
  check_declaration ~expected:"border-inline-start:medium dotted red"
    ~optimized:"border-inline-start:dotted red"
    "border-inline-start: medium dotted red";
  check_declaration ~expected:"border:medium red" ~optimized:"border:red"
    "border: medium red";
  check_declaration ~expected:"border:medium" ~optimized:"border:none"
    "border: medium";
  (* Controls: a plain length stands, and a shorthand that fills no width slot
     is untouched. *)
  check_declaration ~expected:"border:1px solid red"
    ~optimized:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"border:solid red" ~optimized:"border:solid red"
    "border: solid red";
  check_declaration ~expected:"border:red" ~optimized:"border:red" "border: red"

(* CSS Backgrounds 3 (ED) sec. 3.2 gives the border-style properties the initial
   value [none], and sec. 3.4 sets an omitted shorthand slot to its initial
   value, so an explicit [none] beside another slot says what leaving the slot
   out already says: the optimizer drops it, pp holds the node it was handed.
   Drained of every slot the shorthand declares nothing but initial values,
   which is what the [none] keyword declares, so that is the node it folds to -
   and the node [border: none] parses to directly, which is how the two
   spellings reach factoring as one. CSS UI 4 (ED) sec. 3.3 gives outline-style
   the same initial value and sec. 3.1 the same shorthand rule. *)
let border_line_style () =
  check_declaration ~expected:"border:medium none" ~optimized:"border:none"
    "border: medium none";
  check_declaration ~expected:"outline:medium none" ~optimized:"outline:none"
    "outline: medium none";
  check_declaration ~expected:"border:none red" ~optimized:"border:red"
    "border: none red";
  check_declaration ~expected:"outline:none red" ~optimized:"outline:red"
    "outline: none red";
  (* The keyword itself is already that node, and must keep printing, not empty
     out. *)
  check_declaration ~expected:"border:none" ~optimized:"border:none"
    "border: none";
  check_declaration ~expected:"outline:none" ~optimized:"outline:none"
    "outline: none";
  (* The other shorthands reading the same production. *)
  check_declaration ~expected:"border-top:medium none"
    ~optimized:"border-top:none" "border-top: medium none";
  check_declaration ~expected:"border-inline:medium none"
    ~optimized:"border-inline:none" "border-inline: medium none";
  check_declaration ~expected:"border-block-start:medium none"
    ~optimized:"border-block-start:none" "border-block-start: medium none";
  check_declaration ~expected:"column-rule:medium none"
    ~optimized:"column-rule:none" "column-rule: medium none";
  (* Controls: a style that is not the initial one stands, a zero width is not
     the initial width, and [auto] is outline-style's own value, not [none]. *)
  check_declaration ~expected:"border:1px solid red"
    ~optimized:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"border:solid red" ~optimized:"border:solid red"
    "border: solid red";
  check_declaration ~expected:"border:0 none" ~optimized:"border:0"
    "border: 0 none";
  check_declaration ~expected:"border:0" ~optimized:"border:0" "border: 0";
  check_declaration ~expected:"outline:0" ~optimized:"outline:0" "outline: 0";
  check_declaration ~expected:"outline:auto" ~optimized:"outline:auto"
    "outline: auto"

(* CSS Backgrounds 3 (ED) sec. 2.10 has the [background] shorthand reset every
   longhand it covers and then set the ones written, so a slot holding its own
   initial value says what leaving it out says: sec. 2.3 makes that [none] for
   the image, sec. 2.4 [repeat] for the repeat, sec. 2.5 [scroll] for the
   attachment, sec. 2.9 [auto] for the size and sec. 2.2 [transparent] for the
   colour. The fold is the optimizer's; pp holds the node it was handed. *)
let background_initial_slots () =
  check_declaration ~expected:"background:none red" ~optimized:"background:red"
    "background: none red";
  check_declaration ~expected:"background:repeat red"
    ~optimized:"background:red" "background: red repeat";
  check_declaration ~expected:"background:scroll red"
    ~optimized:"background:red" "background: red scroll";
  check_declaration ~expected:"background:url(a.png)transparent"
    ~optimized:"background:url(a.png)" "background: url(a.png) transparent";
  (* Controls: a slot that is not at its initial value stands. *)
  check_declaration ~expected:"background:no-repeat red"
    ~optimized:"background:no-repeat red" "background: red no-repeat";
  check_declaration ~expected:"background:fixed red"
    ~optimized:"background:fixed red" "background: red fixed";
  check_declaration ~expected:"background:url(a.png)red"
    ~optimized:"background:url(a.png)red" "background: url(a.png) red"

(* CSS Backgrounds 3 (ED) sec. 2.1 orders a layer's slots, not the author's
   words, so pp writes the image before the colour whichever way round they were
   read. [background: red var(--x)] therefore prints [var(--x)red], and that
   emission is only readable as the layer it came from if a leading [var()] with
   more behind it fills the image slot rather than standing for the whole
   value. *)
let background_leading_var_slot () =
  check_declaration ~expected:"background:var(--x)red"
    "background: red var(--x)";
  check_declaration ~expected:"background:var(--x)red"
    "background: var(--x) red";
  check_declaration ~expected:"background:var(--x)no-repeat"
    "background: no-repeat var(--x)";
  check_declaration ~expected:"background:var(--x)no-repeat"
    "background: var(--x) no-repeat";
  (* Controls: a [var()] that reaches the value boundary is still the whole
     value, and a run of them is still the run. *)
  check_declaration ~expected:"background:var(--x)" "background: var(--x)";
  check_declaration ~expected:"background:var(--a) var(--b)"
    "background: var(--a) var(--b)"

(* CSS Backgrounds 3 (ED) sec. 2.6 gives background-position the initial value
   [0% 0%], which [0 0] and [left top] both name, so the slot drops with the
   rest. It also reads a lone value as "the second value is assumed to be
   center", so [0] names [0 50%] and stays: no initial position is spelled that
   way. sec. 2.10 writes the size after the position and a [/], so the position
   can only leave once the size has. *)
let background_position_slot () =
  check_declaration ~expected:"background:0 0 red" ~optimized:"background:red"
    "background: red 0 0";
  check_declaration ~expected:"background:left top red"
    ~optimized:"background:red" "background: red left top";
  check_declaration ~expected:"background:0 0/auto red"
    ~optimized:"background:red" "background: red 0 0 / auto";
  (* A single value is horizontal, with [center] vertically: not the initial
     position, so it stays whatever the layer carries. *)
  check_declaration ~expected:"background:0 red" ~optimized:"background:0 red"
    "background: red 0";
  check_declaration ~expected:"background:left red"
    ~optimized:"background:0 red" "background: red left";
  check_declaration ~expected:"background:url(a.png)0"
    ~optimized:"background:url(a.png)0" "background: url(a.png) 0";
  check_declaration ~expected:"background:url(a.png)left"
    ~optimized:"background:url(a.png)0" "background: url(a.png) left";
  (* A size keeps its position, which has to be written for the [/] to attach
     to. *)
  check_declaration ~expected:"background:0 0/cover red"
    ~optimized:"background:0 0/cover red" "background: red 0 0 / cover"

(* CSS Backgrounds 3 (ED) sec. 2.10 reads one [<box>] as setting both
   background-origin and background-clip and two as setting origin then clip, so
   the pair drops together or not at all: sec. 2.8 makes [padding-box] the
   initial origin and sec. 2.7 [border-box] the initial clip. Dropping just the
   one at its initial value would leave a single [<box>] that reassigns the
   other. *)
let background_box_slots () =
  check_declaration ~expected:"background:padding-box border-box red"
    ~optimized:"background:red" "background: red padding-box border-box";
  check_declaration ~expected:"background:border-box border-box red"
    ~optimized:"background:border-box red"
    "background: red border-box border-box";
  check_declaration ~expected:"background:content-box content-box red"
    ~optimized:"background:content-box red"
    "background: red content-box content-box";
  (* A single [<box>] is one slot in the node, and pp holds the node: it is the
     re-reading of that single keyword that sets both longhands. *)
  check_declaration ~expected:"background:border-box red"
    ~optimized:"background:border-box red" "background: red border-box";
  (* Both are written out where a single [<box>] would name a different pair. *)
  check_declaration ~expected:"background:padding-box content-box red"
    ~optimized:"background:padding-box content-box red"
    "background: red padding-box content-box";
  check_declaration ~expected:"background:content-box border-box red"
    ~optimized:"background:content-box border-box red"
    "background: red content-box border-box"

(* CSS Masking 1 (ED) sec. 8.2 gives mask-border-mode the initial value [alpha],
   and sec. 8.7 sets an omitted shorthand slot to its initial value, so an
   explicit [alpha] says what leaving the slot out says. [luminance] is the
   other mode and stays. *)
let mask_border_mode_slot () =
  check_declaration ~expected:"mask-border:url(a.png)alpha"
    ~optimized:"mask-border:url(a.png)" "mask-border: url(a.png) alpha";
  check_declaration ~expected:"mask-border:url(a.png)luminance"
    ~optimized:"mask-border:url(a.png)luminance"
    "mask-border: url(a.png) luminance";
  check_declaration ~expected:"border-image:url(a.png)"
    ~optimized:"border-image:url(a.png)" "border-image: url(a.png)"

(* CSS Masking 1 (ED) sec. 8.7 writes mask-border as [<'mask-border-source'> ||
   <'mask-border-slice'> [ / <'mask-border-width'>? [ / <'mask-border-outset'>
   ]? ]? || <'mask-border-repeat'> || <'mask-border-mode'>], and CSS Values 4
   (ED) sec. 2.2 has [||] ask for one or more of its options, so the mode on its
   own is a whole value. sec. 8.2 gives mask-border-mode the initial value
   [alpha], so [alpha] drains the shorthand and what is left is the six
   initials, which is what [none] declares (sec. 8.1). CSS Backgrounds 3 (ED)
   sec. 5.7 writes border-image without a mode slot, so neither keyword is a
   border-image value wherever it is written. *)
let mask_border_mode_only () =
  check_declaration ~roundtrip:true ~expected:"mask-border:alpha"
    ~optimized:"mask-border:none" "mask-border: alpha";
  check_declaration ~roundtrip:true ~expected:"mask-border:luminance"
    ~optimized:"mask-border:luminance" "mask-border: luminance";
  check_declaration ~expected:"mask-border:none" ~optimized:"mask-border:none"
    "mask-border: none";
  neg_cursor read_declaration "border-image: alpha";
  neg_cursor read_declaration "border-image: luminance";
  neg_cursor read_declaration "border-image: url(a.png) alpha";
  neg_cursor read_declaration "border-image: url(a.png) luminance"

(* CSS Box 4 (ED) sec. 3.2 assigns the values of a one-to-four value box
   shorthand to the four sides: one value goes to all four, two to top-bottom
   then left-right, three to top, left-right, bottom. A repeat that those rules
   already supply is the longer spelling of the same declaration, so the
   optimizer drops it and pp prints the list it was handed. sec. 4.2 says the
   same for padding, css-logical-1 sec. 4.3 assigns inset's values "as for
   margin" and sec. 4.4 padding-block/padding-inline's, and CSS Backgrounds 3
   (ED) sec. 3.1 and sec. 4.1 give border-color and border-radius the same
   four-value form. *)
let box_shorthand_repeats () =
  check_declaration ~expected:"inset:0 0 0 0" ~optimized:"inset:0"
    "inset: 0 0 0 0";
  check_declaration ~expected:"margin:1px 1px 1px 1px" ~optimized:"margin:1px"
    "margin: 1px 1px 1px 1px";
  check_declaration ~expected:"padding:1px 2px 1px 2px"
    ~optimized:"padding:1px 2px" "padding: 1px 2px 1px 2px";
  check_declaration ~expected:"margin:1px 2px 3px 2px"
    ~optimized:"margin:1px 2px 3px" "margin: 1px 2px 3px 2px";
  check_declaration ~expected:"margin:1px 1px 2px"
    ~optimized:"margin:1px 1px 2px" "margin: 1px 1px 2px";
  check_declaration ~expected:"border-color:red red red red"
    ~optimized:"border-color:red" "border-color: red red red red";
  check_declaration ~expected:"inset-block:0 0" ~optimized:"inset-block:0"
    "inset-block: 0 0";
  check_declaration ~expected:"padding-inline:1px 1px"
    ~optimized:"padding-inline:1px" "padding-inline: 1px 1px";
  check_declaration ~expected:"border-radius:1px 1px 1px 1px"
    ~optimized:"border-radius:1px" "border-radius: 1px 1px 1px 1px";
  (* CSS Backgrounds 3 (ED) sec. 4.1 puts the vertical radii after a [/], so
     each group collapses on its own and neither reaches across the slash. With
     no slash the values set both axes equally, so a vertical group equal to the
     horizontal one says what omitting it says. *)
  check_declaration
    ~expected:"border-radius:5px 5px 5px 5px/10px 10px 10px 10px"
    ~optimized:"border-radius:5px/10px"
    "border-radius: 5px 5px 5px 5px / 10px 10px 10px 10px";
  check_declaration ~expected:"border-radius:5px 10px 5px 10px/1px 2px 1px 2px"
    ~optimized:"border-radius:5px 10px/1px 2px"
    "border-radius: 5px 10px 5px 10px / 1px 2px 1px 2px";
  check_declaration ~expected:"border-radius:5px/5px"
    ~optimized:"border-radius:5px" "border-radius: 5px / 5px";
  check_declaration ~expected:"border-radius:5px 10px/5px 10px"
    ~optimized:"border-radius:5px 10px" "border-radius: 5px 10px / 5px 10px";
  (* Control: the two axes differ, so both groups are written. *)
  check_declaration ~expected:"border-radius:5px/10px"
    ~optimized:"border-radius:5px/10px" "border-radius: 5px / 10px";
  (* The per-side folds run first, so sides that only agree once normalised
     still collapse. *)
  check_declaration ~expected:"margin:0px 0 0px 0" ~optimized:"margin:0"
    "margin: 0px 0 0px 0";
  (* Controls: a list the rules cannot rebuild stays as written. *)
  check_declaration ~expected:"margin:1px 2px" ~optimized:"margin:1px 2px"
    "margin: 1px 2px";
  check_declaration ~expected:"margin:1px 2px 3px 4px"
    ~optimized:"margin:1px 2px 3px 4px" "margin: 1px 2px 3px 4px";
  check_declaration ~expected:"padding:1px 2px 1px 3px"
    ~optimized:"padding:1px 2px 1px 3px" "padding: 1px 2px 1px 3px"

let substitution_shorthand_cardinality () =
  let concat = String.concat "" in
  let check_preserved property ~held value =
    let input = concat [ property; ": "; value ] in
    let expected = concat [ property; ":"; held ] in
    check_declaration ~expected ~optimized:expected input
  in
  (* CSS Values 5 arbitrary substitution happens before the property's grammar
     is checked. A top-level var() can therefore contribute several box
     components: dropping one authored slot can turn an invalid computed value
     into a valid one or change the side assignment. *)
  List.iter
    (fun (property, repeated, separator) ->
      check_preserved property
        ~held:
          (concat
             [ "var(--v)"; separator; repeated; " "; repeated; " "; repeated ])
        (concat [ "var(--v) "; repeated; " "; repeated; " "; repeated ]))
    [
      ("margin", "1px", "");
      ("padding", "1px", "");
      ("inset", "1px", "");
      ("scroll-margin", "1px", " ");
      ("scroll-padding", "1px", " ");
      ("border-color", "red", "");
    ];
  List.iter
    (fun (property, separator) ->
      check_preserved property
        ~held:(concat [ "var(--v)"; separator; "var(--v)" ])
        "var(--v) var(--v)")
    [
      ("margin-inline", "");
      ("margin-block", "");
      ("padding-inline", "");
      ("padding-block", "");
      ("inset-inline", "");
      ("inset-block", "");
      ("scroll-margin-inline", " ");
      ("scroll-margin-block", " ");
      ("scroll-padding-inline", " ");
      ("scroll-padding-block", " ");
    ];
  (* env() and attr() are substitution functions too; keep the guard attached to
     the AST shape rather than special-casing var() spellings. *)
  check_preserved "margin" ~held:"env(safe-area-inset-top)1px 1px 1px"
    "env(safe-area-inset-top) 1px 1px 1px";
  check_preserved "margin" ~held:"attr(data-m px)1px 1px 1px"
    "attr(data-m px) 1px 1px 1px";
  check_preserved "border-color"
    ~held:"attr(data-color type(<color>))red red red"
    "attr(data-color type(<color>)) red red red";
  check_preserved "border-radius" ~held:"var(--r)1px 1px 1px"
    "var(--r) 1px 1px 1px";
  check_preserved "border-radius" ~held:"var(--r)/var(--r)" "var(--r)/var(--r)"

(* A [var()] standing beside other components is one operand of the property's
   own grammar rather than a substitution of the whole value: CSS Cascade 5 sec.
   6 keeps the whole-value reading for the CSS-wide keywords, which are valid
   only as the entire value, and a lone [var()] takes it the same way.
   Committing to the whole-value reading on sight of a leading [var()] left the
   components after it unread, so the typed parse failed and the declaration was
   carried as an opaque token stream, costing the components beside the
   reference every fold they have on their own. *)
let component_var_keeps_typed_value () =
  check_declaration ~expected:"border-radius:var(--x)0px"
    ~optimized:"border-radius:var(--x)0" "border-radius: var(--x) 0px";
  (* A top-level var() can expand to several radii, so changing the authored
     component count is not safe before substitution. *)
  check_declaration ~expected:"border-radius:var(--x)1px 1px 1px"
    ~optimized:"border-radius:var(--x)1px 1px 1px"
    "border-radius: var(--x) 1px 1px 1px";
  check_declaration ~expected:"gap:var(--g) 0px" ~optimized:"gap:var(--g) 0"
    "gap: var(--g) 0px";
  check_declaration ~expected:"transform-origin:var(--t) 0px"
    ~optimized:"transform-origin:var(--t) 0" "transform-origin: var(--t) 0px";
  check_declaration ~expected:"border-spacing:var(--s) 0px"
    ~optimized:"border-spacing:var(--s) 0" "border-spacing: var(--s) 0px";
  (* Controls: a lone [var()] is the whole value, a CSS-wide keyword after one
     is still the invalid mix that keeps the declaration opaque, and a reader
     whose grammar takes one component per slot keeps reading it that way. *)
  check_declaration ~expected:"border-radius:var(--x)" "border-radius: var(--x)";
  check_declaration ~expected:"border-radius:var(--x) inherit"
    "border-radius: var(--x) inherit";
  (* CSS Values 4 sec. 4.1 makes a keyword ASCII case-insensitive, so a typed
     read answers with the keyword where an opaque stream keeps the case the
     author wrote. *)
  check_specified_value "place-content reads its components"
    "place-content: var(--p) CENTER" "var(--p) center";
  check_visible_var "place-content component var stays visible"
    "place-content:var(--p) center" "--p";
  (* A var() can substitute more than one component. Keeping two equal
     references is therefore cardinality-sensitive: if [--p] is [center end],
     the two-reference spelling is invalid after substitution while a folded
     single reference is valid. *)
  check_declaration ~expected:"place-content:var(--p) var(--p)"
    ~optimized:"place-content:var(--p) var(--p)"
    "place-content: var(--p) var(--p)";
  check_specified_value "place-items reads a leading component var"
    "place-items: var(--pi) CENTER" "var(--pi) center";
  check_specified_value "place-items keeps its existing trailing component var"
    "place-items: CENTER var(--pi)" "center var(--pi)";
  check_visible_var "place-items component var stays visible"
    "place-items:var(--pi) center" "--pi";
  check_declaration ~expected:"place-items:var(--pi) var(--pi)"
    ~optimized:"place-items:var(--pi) var(--pi)"
    "place-items: var(--pi) var(--pi)";
  check_specified_value "grid-auto-flow keywords are case-insensitive"
    "grid-auto-flow: ROW DENSE" "row dense";
  check_specified_value "grid-auto-flow reads a leading component var"
    "grid-auto-flow: var(--flow) DENSE" "var(--flow) dense";
  check_specified_value "grid-auto-flow reads a trailing component var"
    "grid-auto-flow: ROW var(--flow)" "row var(--flow)";
  check_visible_var "grid-auto-flow component var stays visible"
    "grid-auto-flow:row var(--flow)" "--flow";
  check_specified_value "border-image-width reads a leading component var"
    "border-image-width: var(--width) AUTO" "var(--width) auto";
  check_specified_value "border-image-width reads a trailing component var"
    "border-image-width: AUTO var(--width)" "auto var(--width)";
  check_visible_var "border-image-width component var stays visible"
    "border-image-width:var(--width) auto" "--width";
  check_specified_value "border-image-outset reads a leading component var"
    "border-image-outset: var(--outset) 1PX" "var(--outset) 1px";
  check_specified_value "border-image-outset reads a trailing component var"
    "border-image-outset: 1PX var(--outset)" "1px var(--outset)";
  check_visible_var "border-image-outset component var stays visible"
    "border-image-outset:var(--outset) 1px" "--outset";
  check_visible_var "stroke-dasharray math var stays visible"
    "stroke-dasharray:calc(1 + var(--d)) 2" "--d";
  check_visible_var "stroke-dashoffset length var stays visible"
    "stroke-dashoffset:calc(1px + var(--o))" "--o";
  check_visible_var "animation-iteration-count math var stays visible"
    "animation-iteration-count:calc(1 + var(--n))" "--n";
  check_visible_var "hyphenate-limit-chars slot var stays visible"
    "hyphenate-limit-chars:var(--n) 3" "--n";
  check_specified_value "overflow slots are unchanged"
    "overflow: var(--o) HIDDEN" "var(--o) hidden"

(* CSS Tables 3 (ED) writes [border-spacing] as one or two non-negative lengths
   and reads a single one as "both the horizontal and vertical spacing", so a
   pair of equal lengths is the longer spelling of that one value. The per-side
   fold runs first, so a pair that only agrees once normalised collapses too. *)
let border_spacing_pair () =
  check_declaration ~expected:"border-spacing:1px 1px"
    ~optimized:"border-spacing:1px" "border-spacing: 1px 1px";
  check_declaration ~expected:"border-spacing:0px 0"
    ~optimized:"border-spacing:0" "border-spacing: 0px 0";
  check_declaration ~expected:"border-spacing:0px" ~optimized:"border-spacing:0"
    "border-spacing: 0px";
  (* Controls: two different lengths name two different spacings. *)
  check_declaration ~expected:"border-spacing:1px 2px"
    ~optimized:"border-spacing:1px 2px" "border-spacing: 1px 2px";
  check_declaration ~expected:"border-spacing:1px"
    ~optimized:"border-spacing:1px" "border-spacing: 1px"

(* CSS Backgrounds 3 (ED) sec. 2.4 gives every single [<repeat-style>] keyword
   the pair it computes to: [repeat] is [repeat repeat], [space] is [space
   space], [round] is [round round], [no-repeat] is [no-repeat no-repeat],
   [repeat-x] is [repeat no-repeat] and [repeat-y] is [no-repeat repeat]. Each
   pair therefore has a one-keyword spelling of the same value, and the
   optimizer picks it; pp prints the pair it was handed. *)
let background_repeat_axes () =
  check_declaration ~expected:"background-repeat:repeat repeat"
    ~optimized:"background-repeat:repeat" "background-repeat: repeat repeat";
  check_declaration ~expected:"background-repeat:space space"
    ~optimized:"background-repeat:space" "background-repeat: space space";
  check_declaration ~expected:"background-repeat:round round"
    ~optimized:"background-repeat:round" "background-repeat: round round";
  check_declaration ~expected:"background-repeat:no-repeat no-repeat"
    ~optimized:"background-repeat:no-repeat"
    "background-repeat: no-repeat no-repeat";
  check_declaration ~expected:"background-repeat:repeat no-repeat"
    ~optimized:"background-repeat:repeat-x"
    "background-repeat: repeat no-repeat";
  check_declaration ~expected:"background-repeat:no-repeat repeat"
    ~optimized:"background-repeat:repeat-y"
    "background-repeat: no-repeat repeat";
  (* Each layer of the comma-separated list folds on its own. *)
  check_declaration ~expected:"background-repeat:repeat repeat,space space"
    ~optimized:"background-repeat:repeat,space"
    "background-repeat: repeat repeat, space space";
  (* The shorthand's repeat slot reads the same production, so the fold reaches
     it and leaves the slot at its initial value (sec. 2.4). *)
  check_declaration ~expected:"background:repeat repeat red"
    ~optimized:"background:red" "background: red repeat repeat";
  (* Controls: a pair with two different axes has no one-keyword spelling, and
     the one-keyword forms are already the node they fold to. *)
  check_declaration ~expected:"background-repeat:repeat space"
    ~optimized:"background-repeat:repeat space"
    "background-repeat: repeat space";
  check_declaration ~expected:"background-repeat:space no-repeat"
    ~optimized:"background-repeat:space no-repeat"
    "background-repeat: space no-repeat";
  check_declaration ~expected:"background-repeat:repeat-x"
    ~optimized:"background-repeat:repeat-x" "background-repeat: repeat-x";
  check_declaration ~expected:"background-repeat:repeat"
    ~optimized:"background-repeat:repeat" "background-repeat: repeat"

(* CSS Masking 1 (ED) sec. 8.7 sets an omitted [mask] slot to its initial value,
   so a layer that fills no slot declares what [mask: none] declares. Every
   spelling of it meets on [none], and none of them prints as the empty
   value. *)
let mask_drained_layer () =
  check_declaration ~expected:"mask:none"
    ~optimized:"-webkit-mask:none;mask:none" "mask: none";
  check_declaration ~expected:"mask:0 0"
    ~optimized:"-webkit-mask:none;mask:none" "mask: 0 0";
  check_declaration ~expected:"mask:match-source"
    ~optimized:"-webkit-mask:none;mask:none" "mask: match-source";
  check_declaration ~expected:"mask:url(a.png),none"
    ~optimized:"-webkit-mask:url(a.png),none;mask:url(a.png),none"
    "mask: url(a.png), none"

(* CSS Masking 1 (ED) sec. 8.7 spells [mask] as [<mask-layer>#] and sec. 8.1
   puts [none] in the [<mask-reference>] of one layer, so it names the whole
   value only where no layer follows it and no slot of its own layer does. *)
let mask_none_is_a_layer_reference () =
  check_declaration ~expected:"mask:none,none"
    ~optimized:"-webkit-mask:none,none;mask:none,none" "mask: none, none";
  check_declaration ~expected:"mask:none luminance" "mask: none luminance";
  (* Controls: a lone [none] is still the property keyword, and a layer list
     that never spells [none] reads as before. *)
  check_declaration ~expected:"mask:none" "mask: none";
  check_declaration ~expected:"mask:url(a.png),url(b.png)"
    "mask: url(a.png), url(b.png)"

(* CSS Values 4 (ED) sec. 10.3 keeps a [calc()] valid where its range is
   exceeded and clamps at used-value time, so a property whose range starts at
   zero reads [calc(-10px)] and drops [-10px]. Chrome 146 computes the first as
   [0px] and drops the second, so the call stays on and folding it away would
   write a declaration browsers throw out. A property that takes a negative
   folds as before. *)
let negative_calc_keeps_the_call () =
  List.iter
    (fun prop ->
      check_declaration ~expected:(prop ^ ":calc(-10px)")
        ~optimized:(prop ^ ":calc(-10px)") (prop ^ ": calc(-10px)"))
    [
      "width";
      "height";
      "min-width";
      "max-width";
      "inline-size";
      "padding-left";
      "padding-inline-start";
      "column-gap";
      "row-gap";
      "perspective";
      "scroll-padding-top";
      "border-top-left-radius";
    ];
  List.iter
    (fun prop ->
      check_declaration ~expected:(prop ^ ":calc(-10px)")
        ~optimized:(prop ^ ":-10px") (prop ^ ": calc(-10px)"))
    [ "margin-left"; "top"; "text-decoration-thickness"; "outline-offset" ];
  (* A fold that stays in range is unaffected. *)
  check_declaration ~expected:"width:calc(1px + 2px)" ~optimized:"width:3px"
    "width: calc(1px + 2px)";
  (* The record this was found on: the sign is known but the result is not a
     length any of these properties reads as a literal. *)
  check_declaration ~expected:"width:calc(10px*sign(-1vw))"
    ~optimized:"width:calc(10px*sign(-1vw))" "width: calc(10px * sign(-1vw))";
  (* Sec. 10.7.3 rounds to the nearest multiple of the step and sends a tie
     toward positive infinity, which is the rule sec. 10.12 gives an <integer>
     slot and #1163 gave the grid line index. The other three strategies name
     their own direction, so only [nearest] has a tie to break. Chrome 153
     agrees on every row. *)
  List.iter
    (fun (value, folded) ->
      check_declaration
        ~expected:(String.concat "" [ "margin-left:"; value ])
        ~optimized:(String.concat "" [ "margin-left:"; folded ])
        (String.concat "" [ "margin-left: "; value ]))
    [
      ("round(-3px,2px)", "-2px");
      ("round(3px,2px)", "4px");
      ("round(-5px,2px)", "-4px");
      ("round(5px,2px)", "6px");
      (* A zero length drops its unit at the top level, which is the zero strip
         the optimizer already does everywhere. *)
      ("round(-1px,2px)", "0");
      ("round(-3.5px,1px)", "-3px");
      ("round(3.5px,1px)", "4px");
      ("round(up,-3px,2px)", "-2px");
      ("round(down,-3px,2px)", "-4px");
      ("round(to-zero,-3px,2px)", "-2px");
    ];
  (* CSS Values 4 sec. 10.6 makes [sign()] answer -1, 0 or +1, so the answer
     turns on whether the argument is zero. A relative unit's reference can be:
     a zero font-size, a zero viewport, a zero container, a percentage of zero.
     So the sign of the coefficient is not the sign of the value and the call
     waits for the reference, where an absolute unit has no reference to wait
     for. Chrome 153 keeps every row of the first list and folds every row of
     the second, at a property that takes a negative length so the fold is the
     only thing under test. *)
  List.iter
    (fun unit ->
      let value = String.concat "" [ "calc(10px * sign(-1"; unit; "))" ] in
      let held =
        String.concat "" [ "margin-left:calc(10px*sign(-1"; unit; "))" ]
      in
      check_declaration ~expected:held ~optimized:held
        (String.concat "" [ "margin-left: "; value ]))
    [ "em"; "rem"; "ex"; "ch"; "vw"; "vh"; "vmin"; "dvw"; "cqw"; "%" ];
  List.iter
    (fun unit ->
      let value = String.concat "" [ "calc(10px * sign(-1"; unit; "))" ] in
      check_declaration
        ~expected:
          (String.concat "" [ "margin-left:calc(10px*sign(-1"; unit; "))" ])
        ~optimized:"margin-left:-10px"
        (String.concat "" [ "margin-left: "; value ]))
    [ "px"; "cm"; "pt"; "s"; "deg" ]

(* CSS Backgrounds 3 (ED) sec. 2.10 resets every longhand the shorthand covers,
   so a layer that fills no slot declares what [background: none] declares. [0
   0] is the shortest spelling of that layer, so it is the node the spellings
   meet on. *)
let background_drained_layer () =
  check_declaration ~expected:"background:none" ~optimized:"background:0 0"
    "background: none";
  check_declaration ~expected:"background:0 0" ~optimized:"background:0 0"
    "background: 0 0";
  check_declaration ~expected:"background:left top" ~optimized:"background:0 0"
    "background: left top";
  check_declaration ~expected:"background:transparent"
    ~optimized:"background:0 0" "background: transparent"

(* CSS Backgrounds 3 (ED) sec. 3.1 gives the border-color properties the initial
   value [currentColor], and sec. 3.4 sets an omitted shorthand slot to its
   initial value, so an explicit [currentColor] beside another slot says what
   leaving the colour out already says. Drained of every slot the shorthand
   declares nothing but initial values, which is what [none] declares. CSS
   Multi-column 1 (ED) sec. 4.2 and css-logical-1 sec. 4.5.3 give column-rule
   and the logical borders the same initial colour, so they take the same fold.

   CSS UI 4 (ED) sec. 3.4 does not: outline-color's initial value is [auto], a
   UA-chosen colour that [currentColor] does not name, so the outline colour
   slot holds whatever it was written with. *)
let border_line_color () =
  check_declaration ~expected:"border:solid currentColor"
    ~optimized:"border:solid" "border: solid currentcolor";
  check_declaration ~expected:"border:1px solid currentColor"
    ~optimized:"border:1px solid" "border: 1px solid currentColor";
  check_declaration ~expected:"border:currentColor" ~optimized:"border:none"
    "border: currentcolor";
  check_declaration ~expected:"border-top:solid currentColor"
    ~optimized:"border-top:solid" "border-top: solid currentcolor";
  check_declaration ~expected:"border-inline:solid currentColor"
    ~optimized:"border-inline:solid" "border-inline: solid currentcolor";
  check_declaration ~expected:"border-block-start:solid currentColor"
    ~optimized:"border-block-start:solid"
    "border-block-start: solid currentcolor";
  check_declaration ~expected:"column-rule:solid currentColor"
    ~optimized:"column-rule:solid" "column-rule: solid currentcolor";
  (* outline-color's initial value is [auto], so nothing drops here. *)
  check_declaration ~expected:"outline:solid currentColor"
    ~optimized:"outline:solid currentColor" "outline: solid currentcolor";
  check_declaration ~expected:"outline:currentColor"
    ~optimized:"outline:currentColor" "outline: currentcolor";
  (* Controls: a colour that is not the initial one stands. *)
  check_declaration ~expected:"border:solid red" ~optimized:"border:solid red"
    "border: solid red";
  check_declaration ~expected:"column-rule:solid red"
    ~optimized:"column-rule:solid red" "column-rule: solid red"

(* CSS Syntax 3 (ED) sec. 5.5.6 keeps a parsed declaration only if it "is valid
   in the current context", which for a non-custom property means its value
   matches the property's grammar. CSS Backgrounds 3 (ED) sec. 3.4 writes the
   border shorthands as [<line-width> || <line-style> || <color>], css-logical-1
   sec. 4.5.4 and CSS Multi-column 1 (ED) sec. 4.5 route the logical shorthands
   and column-rule through the same production, and CSS UI 4 (ED) sec. 3.1
   writes outline the same way. CSS Values 4 (ED) sec. 2.2 has [||] require one
   or more of its options to occur, so nothing at all matches none of these
   grammars: the declaration is dropped, not filled in with a value nobody
   wrote. *)
let empty_shorthand_value () =
  List.iter
    (fun prop -> neg_cursor read_declaration (prop ^ ": "))
    [
      "border";
      "border-top";
      "border-right";
      "border-bottom";
      "border-left";
      "border-block";
      "border-block-start";
      "border-block-end";
      "border-inline";
      "border-inline-start";
      "border-inline-end";
      "column-rule";
      "outline";
    ];
  (* Whitespace is discarded before the value is read, so a run of it is still
     an empty value. *)
  neg_cursor read_declaration "border:";
  neg_cursor read_declaration "outline:   ";
  (* Controls: every slot is optional, so any one of them is a whole value. *)
  check_declaration ~expected:"border:none" ~optimized:"border:none"
    "border: none";
  check_declaration ~expected:"border:1px solid red"
    ~optimized:"border:1px solid red" "border: 1px solid red";
  check_declaration ~expected:"outline:none" ~optimized:"outline:none"
    "outline: none";
  check_declaration ~expected:"outline:auto" ~optimized:"outline:auto"
    "outline: auto";
  check_declaration ~expected:"column-rule:1px solid red"
    ~optimized:"column-rule:1px solid red" "column-rule: 1px solid red"

(* The record behind each shorthand is public, so a caller can hand the printer
   a value with no slot filled. That value declares nothing but the initial
   longhands, which is what the [none] keyword declares (CSS Backgrounds 3 (ED)
   sec. 3.4 and sec. 5.7, CSS UI 4 (ED) sec. 3.1, CSS Text Decoration 4 (ED)
   sec. 2.6, CSS Masking 1 (ED) sec. 8.7), and [none] is the spelling that says
   so. An empty string is not a spelling of anything: no parser accepts it.
   [Css.to_string] does not normalize, so the printer is the only thing standing
   between such a record and the output. *)
let drained_mask_layer : Css.Properties.mask_layer =
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

let all_initial_shorthand_prints_none () =
  let pp v = Css.Pp.to_string ~minify:true Css.Declaration.pp v in
  let border : Css.border =
    Shorthand { width = None; style = None; color = None }
  in
  let outline : Css.outline =
    Shorthand { width = None; style = None; color = None }
  in
  let text_decoration : Css.text_decoration =
    Shorthand { lines = []; style = None; color = None; thickness = None }
  in
  let border_image : Css.Properties.border_image =
    {
      source = None;
      slice = None;
      width = None;
      outset = None;
      repeat = None;
      mode = None;
    }
  in
  Alcotest.(check string)
    "drained border shorthand" "border:none"
    (pp (Css.Declaration.v Border border));
  Alcotest.(check string)
    "drained outline shorthand" "outline:none"
    (pp (Css.Declaration.v Outline outline));
  Alcotest.(check string)
    "drained text-decoration shorthand" "text-decoration:none"
    (pp (Css.Declaration.v Text_decoration text_decoration));
  Alcotest.(check string)
    "outline_shorthand with no slot" "outline:none"
    (pp (Css.Declaration.outline (Css.outline_shorthand ())));
  Alcotest.(check string)
    "border_shorthand with no slot" "border:none"
    (pp (Css.Declaration.v Border (Css.border_shorthand ())));
  Alcotest.(check string)
    "text_decoration_shorthand with no slot" "text-decoration:none"
    (pp (Css.Declaration.v Text_decoration (Css.text_decoration_shorthand ())));
  Alcotest.(check string)
    "drained mask-border shorthand" "mask-border:none"
    (pp (Css.Declaration.v Mask_border border_image));
  Alcotest.(check string)
    "drained border-image shorthand" "border-image:none"
    (pp (Css.Declaration.v Border_image border_image));
  Alcotest.(check string)
    "drained mask layer" "mask:none"
    (pp (Css.Declaration.v Mask (Layer drained_mask_layer)));
  Alcotest.(check string)
    "drained mask layer beside another" "mask:none,url(a.png)"
    (pp
       (Css.Declaration.v Mask
          (Layers
             [
               drained_mask_layer;
               { drained_mask_layer with image = Some (Url "a.png") };
             ])))

let logical_border_shorthands () =
  (* css-logical-1 sec. 4.4.1: border-block-start, border-block-end,
     border-inline-start and border-inline-end take <'border-top-width'> ||
     <'border-top-style'> || <'border-top-color'>. sec. 4.4.2: border-block and
     border-inline take <'border-block-start'>. Chrome 151 accepts every vector
     below and reports the same serialisation. *)
  check_declaration ~expected:"border-inline:1px solid red"
    "border-inline: 1px solid red";
  check_declaration ~expected:"border-inline-start:2px dashed red"
    "border-inline-start: 2px dashed red";
  check_declaration ~expected:"border-inline-end:3px dotted red"
    "border-inline-end: 3px dotted red";
  check_declaration ~expected:"border-block-start:4px double red"
    "border-block-start: 4px double red";
  check_declaration ~expected:"border-block-end:5px groove red"
    "border-block-end: 5px groove red";
  (* The || combinator makes each component optional and order-free; the
     serialisation is width, style, colour. *)
  check_declaration ~expected:"border-inline:solid" "border-inline: solid";
  check_declaration ~expected:"border-inline:1px" "border-inline: 1px";
  check_declaration ~expected:"border-block-start:medium"
    "border-block-start: medium";
  check_declaration ~expected:"border-inline:1px solid red"
    "border-inline: red 1px solid";
  (* A second width is not in the grammar; Chrome drops the declaration. *)
  List.iter
    (fun input -> neg_cursor read_declaration input)
    [
      "border-inline: 1px solid red 2px";
      "border-inline-start: 1px solid red 2px";
      "border-block-end: 1px solid red 2px";
    ]

let suite =
  ( "declaration_shorthand",
    [
      test_case "outline line-width" `Quick outline_line_width;
      test_case "border line-width" `Quick border_line_width;
      test_case "border line-style" `Quick border_line_style;
      test_case "background initial slots" `Quick background_initial_slots;
      test_case "background leading var slot" `Quick background_leading_var_slot;
      test_case "background position slot" `Quick background_position_slot;
      test_case "background box slots" `Quick background_box_slots;
      test_case "mask-border mode slot" `Quick mask_border_mode_slot;
      test_case "mask-border mode only" `Quick mask_border_mode_only;
      test_case "box shorthand repeats" `Quick box_shorthand_repeats;
      test_case "substitution shorthand cardinality" `Quick
        substitution_shorthand_cardinality;
      test_case "component var keeps typed value" `Quick
        component_var_keeps_typed_value;
      test_case "border-spacing pair" `Quick border_spacing_pair;
      test_case "background repeat axes" `Quick background_repeat_axes;
      test_case "negative calc keeps the call" `Quick
        negative_calc_keeps_the_call;
      test_case "mask drained layer" `Quick mask_drained_layer;
      test_case "mask none is a layer reference" `Quick
        mask_none_is_a_layer_reference;
      test_case "background drained layer" `Quick background_drained_layer;
      test_case "border line-color" `Quick border_line_color;
      test_case "empty shorthand value" `Quick empty_shorthand_value;
      test_case "all-initial shorthand prints none" `Quick
        all_initial_shorthand_prints_none;
      test_case "logical border shorthands" `Quick logical_border_shorthands;
    ] )
