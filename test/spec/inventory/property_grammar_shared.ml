open Property_grammar_row

let rows =
  rows_for
    [
      "background-color";
      "border-top-color";
      "border-right-color";
      "border-bottom-color";
      "border-left-color";
      "border-inline-start-color";
      "border-inline-end-color";
      "border-block-start-color";
      "border-block-end-color";
      "text-decoration-color";
      "-webkit-text-decoration-color";
      "-webkit-tap-highlight-color";
      "-webkit-text-fill-color";
      "-webkit-text-stroke-color";
      "column-rule-color";
      "stop-color";
      "flood-color";
      "lighting-color";
    ]
    [ "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
    [ "1px"; "red blue" ]
  (* SVG 2 sec. 13.2: <paint> = none | <color> | <url> [none | <color>]? |
     context-fill | context-stroke, none of which a <color> property takes. *)
  @ rows_for [ "fill"; "stroke" ]
      [
        "none";
        "red";
        "currentColor";
        "url(#p)";
        "url(#p) red";
        "context-fill";
        "context-stroke";
      ]
      [ "1px"; "auto"; "red blue" ]
    (* CSS UI 4 sec. 5.2.1: auto | <color> [ auto | <color> ]?, with auto the
       initial value, so a third component is the first invalid one. *)
  @ rows_for [ "caret-color" ]
      [ "auto"; "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
      [ "1px"; "red blue green" ]
    (* CSS UI 4 sec. 7.1: auto | <color>, with auto the initial value. *)
  @ rows_for [ "accent-color" ]
      [ "auto"; "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
      [ "1px"; "red blue" ]
    (* CSS UI 4 sec. 3.4: auto | <'border-top-color'>, with auto the initial
       value. *)
  @ rows_for [ "outline-color" ]
      [ "auto"; "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
      [ "1px"; "red blue" ]
  @ [
      {
        property = "border-color";
        positives = [ "red"; "red blue"; "red blue green"; "currentColor" ];
        negatives = [ "1px"; "red blue green black white" ];
      };
    ]
    (* CSS Backgrounds 3 (ED) sec. 3.2 gives [border-style] the value
       [<line-style>{1,4}], the box over the four side styles that sec. 3.1
       gives [border-color]. *)
  @ rows_for [ "border-style" ]
      [
        "none";
        "solid";
        "hidden";
        "solid dashed";
        "solid dashed dotted";
        "solid dashed dotted double";
        "none solid dashed double";
      ]
      [ "foo"; "solid dashed dotted double hidden" ]
    (* CSS Logical 1 (ED) sec. 4.5.2 gives both flow-relative shorthands the
       value [<'border-top-style'>{1,2}], the start edge then the end edge. *)
  @ rows_for
      [ "border-inline-style"; "border-block-style" ]
      [ "none"; "solid"; "hidden"; "solid dashed" ]
      [ "foo"; "solid dashed dotted" ]
  @ rows_for
      [
        "border-top-style";
        "border-right-style";
        "border-bottom-style";
        "border-left-style";
        "border-inline-start-style";
        "border-inline-end-style";
        "border-block-start-style";
        "border-block-end-style";
      ]
      [ "none"; "solid"; "dashed"; "hidden" ]
      [ "solid dashed"; "foo" ]
    (* CSS Box 4: the padding longhands are one <length-percentage [0,inf]>. *)
  @ rows_for
      [
        "padding-left";
        "padding-right";
        "padding-bottom";
        "padding-top";
        "padding-inline-start";
        "padding-inline-end";
        "padding-block-start";
        "padding-block-end";
      ]
      [ "0"; "1px"; "10%"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px"; "-1px" ]
    (* CSS Backgrounds 3 (ED) sec. 4.1 gives every corner longhand the value
       [<length-percentage [0,inf]>{1,2}]: the horizontal radius then the
       vertical one, a single value setting both. *)
  @ rows_for
      [
        "border-top-left-radius";
        "border-top-right-radius";
        "border-bottom-left-radius";
        "border-bottom-right-radius";
        "border-start-start-radius";
        "border-start-end-radius";
        "border-end-start-radius";
        "border-end-end-radius";
      ]
      [ "0"; "1px"; "10%"; "1px 2px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px 3px"; "-1px" ]
    (* SVG 2 sec. 13.5: stroke-width takes a bare <number> as well. *)
  @ rows_for [ "stroke-width" ]
      [ "0"; "1px"; "10%"; "2"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
    (* CSS UI 4 sec. 3.2: <line-width>, with medium the initial value. *)
  @ rows_for [ "outline-width" ]
      [ "thin"; "medium"; "thick"; "0"; "1px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px"; "-1px" ]
    (* CSS UI 4 sec. 3.3: <length>, which may be negative and takes no
       percentage. *)
  @ rows_for [ "outline-offset" ]
      [ "0"; "1px"; "-1px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px"; "10%" ]
    (* CSS Text 4 sec. 8.1 and 8.2: normal | <length-percentage>, with normal
       the initial value. *)
  @ rows_for
      [ "letter-spacing"; "word-spacing" ]
      [ "normal"; "0"; "1px"; "-1px"; "10%"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
    (* CSS Text 4 sec. 9.1: [ <length-percentage> ] && hanging? && each-line?,
       so the length is required and the two keywords are not. *)
  @ rows_for [ "text-indent" ]
      [ "0"; "1px"; "10%"; "1em hanging"; "2em each-line"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "hanging" ]
    (* CSS Overflow 4 sec. 3.2: <visual-box> || <length>. *)
  @ rows_for [ "overflow-clip-margin" ]
      [ "0"; "1px"; "content-box"; "padding-box 2px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
  @ rows_for
      [ "offset-distance"; "shape-margin" ]
      [ "0"; "1px"; "10%"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
    (* CSS Inline 3: line-height-step is a <length>, so a percentage is not
       one. *)
  @ rows_for [ "line-height-step" ]
      [ "0"; "1px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px"; "10%" ]
  (* CSS Transforms 2 sec. 3: [perspective] takes [none], its initial value. CSS
     Text Decoration 4 sec. 5: [text-underline-offset] takes [auto] and may be
     negative. *)
  @ [
      {
        property = "perspective";
        positives = [ "0"; "1px"; "none"; "calc(1rem + 2px)" ];
        negatives = [ "auto"; "red"; "1px 2px"; "-1px" ];
      };
      {
        property = "text-underline-offset";
        positives = [ "0"; "1px"; "auto"; "-2px"; "10%" ];
        negatives = [ "red"; "1px 2px" ];
      };
    ]
  @ rows_for
      [ "margin-left"; "margin-right"; "margin-top"; "margin-bottom" ]
      [ "0"; "1px"; "10%"; "auto"; "anchor-size(width)" ]
      [ "red"; "1px 2px" ]
    (* CSS Logical 1 sec. 4.4: <'padding-top'>{1,2}. *)
  @ rows_for
      [ "padding-inline"; "padding-block" ]
      [ "0"; "1rem"; "10%"; "1px 2px" ]
      [ "auto"; "1px 2px 3px"; "red" ]
    (* CSS Gaps 1 sec. 2.1: normal | <length-percentage [0,inf]> | <line-width>,
       one value, with normal the initial one. *)
  @ rows_for
      [ "column-gap"; "row-gap" ]
      [ "normal"; "0"; "1rem"; "10%"; "calc(1rem + 2px)" ]
      [ "auto"; "1px 2px"; "red" ]
  @ rows_for
      [ "margin-inline"; "margin-block" ]
      [ "0"; "1px"; "1px 1px"; "1px 2px"; "auto" ]
      [ "red"; "1px 2px 3px" ]
  @ rows_for
      [
        "margin-inline-start";
        "margin-inline-end";
        "margin-block-start";
        "margin-block-end";
      ]
      [ "0"; "1px"; "10%"; "auto" ]
      [ "red"; "1px 2px" ]
    (* CSS Scroll Snap 1 sec. 5.1: the scroll-margin longhands are [<length>]
       with no [0,inf] range, so negatives are valid; percentages are not
       ("Percentages: n/a"). Contrast scroll-padding just below. *)
  @ rows_for [ "scroll-margin" ]
      [ "0"; "1px"; "-1px"; "1px 2px"; "-1px -2px"; "1px 2px 3px 4px" ]
      [ "auto"; "10%"; "-10%"; "red"; "1px 2px 3px 4px 5px" ]
  @ rows_for
      [ "scroll-margin-inline"; "scroll-margin-block" ]
      [ "0"; "1px"; "-1px"; "1px 2px"; "-1px -2px" ]
      [ "auto"; "10%"; "-10%"; "red"; "1px 2px 3px" ]
  @ rows_for
      [
        "scroll-margin-top";
        "scroll-margin-right";
        "scroll-margin-bottom";
        "scroll-margin-left";
        "scroll-margin-inline-start";
        "scroll-margin-inline-end";
        "scroll-margin-block-start";
        "scroll-margin-block-end";
      ]
      [ "0"; "1px"; "-1px" ]
      [ "auto"; "10%"; "-10%"; "red"; "1px 2px" ]
  @ rows_for [ "scroll-padding" ]
      [ "auto"; "0"; "1px"; "10%"; "1px 2px"; "1px 2px 3px 4px" ]
      [ "red"; "1px 2px 3px 4px 5px"; "auto auto auto auto auto"; "-1px" ]
  @ rows_for
      [ "scroll-padding-inline"; "scroll-padding-block" ]
      [ "auto"; "0"; "1px"; "10%"; "1px 2px" ]
      [ "red"; "1px 2px 3px"; "-1px" ]
  @ rows_for
      [
        "scroll-padding-top";
        "scroll-padding-right";
        "scroll-padding-bottom";
        "scroll-padding-left";
        "scroll-padding-inline-start";
        "scroll-padding-inline-end";
        "scroll-padding-block-start";
        "scroll-padding-block-end";
      ]
      [ "auto"; "0"; "1px"; "10%" ]
      [ "red"; "1px 2px"; "-1px" ]
    (* CSS Sizing 3 sec. 3.1.1 and 3.1.2: auto | <box-size>, so none is not one
       of them. *)
  @ rows_for
      [
        "height";
        "min-width";
        "min-height";
        "inline-size";
        "min-inline-size";
        "block-size";
        "min-block-size";
        "flex-basis";
      ]
      [ "auto"; "10%"; "min-content"; "fit-content(20rem)"; "calc(1rem + 2px)" ]
      [ "red"; "none"; "fit-content()"; "1px 2px" ]
    (* CSS Sizing 3 sec. 3.1.3: none | <box-size>, with none the initial value
       and no auto arm at all. *)
  @ rows_for
      [ "max-width"; "max-height"; "max-inline-size"; "max-block-size" ]
      [ "none"; "10%"; "min-content"; "fit-content(20rem)"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "fit-content()"; "1px 2px" ]
  @ rows_for
      [
        "contain-intrinsic-width";
        "contain-intrinsic-height";
        "contain-intrinsic-inline-size";
        "contain-intrinsic-block-size";
      ]
      [ "none"; "100px"; "auto 300px" ]
      [ "auto"; "1px 2px"; "red" ]
    (* CSS Backgrounds 3 sec. 3.3: <line-width>{1,4} for the box, one value for
       each longhand. *)
  @ rows_for [ "border-width" ]
      [ "thin"; "medium"; "thick"; "1px"; "1px 2px"; "thin medium thick 1px" ]
      [ "auto"; "thin medium thick 1px 2px"; "red" ]
  @ rows_for
      [
        "border-top-width";
        "border-right-width";
        "border-bottom-width";
        "border-left-width";
        "border-inline-start-width";
        "border-inline-end-width";
        "border-block-start-width";
        "border-block-end-width";
      ]
      [ "thin"; "medium"; "thick"; "1px" ]
      [ "auto"; "1px 2px"; "red" ]
  @ [
      {
        property = "border-block";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-inline-color";
        positives = [ "red"; "red blue"; "currentColor" ];
        negatives = [ "red blue green"; "1px" ];
      };
      {
        property = "border-block-color";
        positives = [ "red"; "red blue"; "currentColor" ];
        negatives = [ "red blue green"; "1px" ];
      };
      {
        property = "border-inline-width";
        positives = [ "1px"; "1px 2px"; "thin"; "medium thick" ];
        negatives = [ "1px 2px 3px"; "red" ];
      };
      {
        property = "border-block-width";
        positives = [ "1px"; "1px 2px"; "thin"; "medium thick" ];
        negatives = [ "1px 2px 3px"; "red" ];
      };
    ]
  @ rows_for [ "inset" ]
      [ "auto"; "1px"; "10%"; "1px 2px 3px 4px" ]
      [ "red"; "1px 2px 3px 4px 5px" ]
  @ rows_for
      [ "inset-inline"; "inset-block" ]
      [ "auto"; "1px"; "10%"; "1px 2px" ]
      [ "red"; "1px 2px 3px"; "1px 2px 3px 4px" ]
  @ rows_for
      [
        "top";
        "right";
        "bottom";
        "left";
        "inset-inline-start";
        "inset-inline-end";
        "inset-block-start";
        "inset-block-end";
      ]
      [ "auto"; "1px"; "10%" ]
      [ "red"; "1px 2px"; "1px 2px 3px 4px" ]
  @ rows_for
      [ "overflow-x"; "overflow-y" ]
      [ "visible"; "hidden"; "clip"; "auto"; "scroll" ]
      [ "none"; "visible hidden" ]
  @ rows_for
      [
        "backdrop-filter";
        "-webkit-backdrop-filter";
        "-webkit-filter";
        "-ms-filter";
      ]
      (* Filter Effects 1 sec. 6.1: blur( <length>? ). *)
      ([ "none"; "blur(5px)"; "contrast(120%) brightness(.8)" ]
      @ omitted_filters @ blur_lengths)
      ([ "blur(red)"; "none blur(1px)" ] @ invalid_blur_lengths)
    (* CSS Transitions 1 sec. 2.2: <time [0s,inf]>#. *)
  @ rows_for [ "transition-duration" ]
      [ "0s"; ".2s"; "120ms"; "0s, 1s" ]
      [ "1px"; "1s 2s"; "-1s" ]
    (* CSS Transitions 1 sec. 2.4: <time>#, with no lower bound. *)
  @ rows_for [ "transition-delay" ]
      [ "0s"; ".2s"; "120ms"; "-1s"; "0s, 1s" ]
      [ "1px"; "1s 2s" ]
    (* CSS Animations 2 sec. 4.1: [ auto | <time [0s,inf]> ]#, with auto the
       initial value. *)
  @ rows_for [ "animation-duration" ]
      [ "auto"; "0s"; ".2s"; "120ms"; "1s, 2s" ]
      [ "1px"; "1s 2s"; "-1s" ]
    (* CSS Animations 2 sec. 4.8: a <time> list with no lower bound. *)
  @ rows_for [ "animation-delay" ]
      [ "0s"; ".2s"; "120ms"; "-1s"; "1s, 2s" ]
      [ "1px"; "1s 2s" ]
  @ rows_for
      [ "transition-timing-function"; "animation-timing-function" ]
      [ "ease"; "linear"; "steps(4, jump-end)"; "cubic-bezier(.1,.2,.3,.4)" ]
      [ "steps()"; "cubic-bezier(1, 2)" ]
  @ rows_for
      [ "background-origin"; "background-clip"; "-webkit-background-clip" ]
      [ "border-box"; "padding-box"; "content-box" ]
      [ "margin-box"; "border-box padding-box content-box content-box" ]
  @ rows_for
      [ "background-repeat"; "mask-repeat"; "-webkit-mask-repeat" ]
      [
        "repeat"; "no-repeat"; "repeat-x"; "space round"; "no-repeat no-repeat";
      ]
      [ "repeat no-repeat space"; "foo" ]
    (* CSS Backgrounds 3 sec. 2.6: <bg-position>#, one position per layer. *)
  @ rows_for
      [ "background-position"; "mask-position"; "-webkit-mask-position" ]
      [
        "center";
        "left 10px top 20px";
        "10% 20%";
        "center, left top";
        "calc(10% + 1px) 20%";
      ]
      [ "left top center"; "foo" ]
    (* CSS Images 3 sec. 4.6: a single <position>, with no list. *)
  @ rows_for [ "object-position" ]
      [ "center"; "left 10px top 20px"; "10% 20%"; "calc(10% + 1px) 20%" ]
      [ "left top center"; "foo"; "center, center" ]
  @ rows_for
      [ "mask-size"; "-webkit-mask-size" ]
      [ "auto"; "cover"; "contain"; "10px 20%" ]
      [ "cover contain"; "-1px" ]
  @ rows_for
      [ "mask-image"; "-webkit-mask-image" ]
      [ "none"; "url(mask.png)"; "linear-gradient(red, blue)" ]
      [ "linear-gradient()"; "image-set()" ]
  @ [
      {
        property = "list-style-type";
        positives = [ "disc"; "square"; "decimal"; "\"-\"" ];
        negatives = [ "disc square"; "url(marker.png)"; "default" ];
      };
      {
        property = "list-style-position";
        positives = [ "inside"; "outside" ];
        negatives = [ "inside outside"; "center" ];
      };
      {
        property = "list-style-image";
        positives = [ "none"; "url(marker.png)" ];
        negatives = [ "square"; "url(marker.png) none" ];
      };
    ]
