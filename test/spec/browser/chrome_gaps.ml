(* What a headless Chrome cannot arbitrate, and the vectors where it and the
   specifications disagree.

   Shared by the harnesses in this directory. Both ask the same browser the same
   question, so a browser that catches up, or a specification that moves, is
   recorded once and both runs see it. Every entry carries the spec text that
   justifies it: without one an entry is a place for a mistake to hide.

   The two lists are the two directions. [spec_ahead] is grammar a specification
   defines and Chrome has not implemented, so Chrome rejecting it says nothing
   about the value. [lenient] is a value Chrome accepts that no specification
   grants, so Chrome accepting it says nothing either. *)

(* Chrome answers nothing about these, so the spec is their only oracle. Most
   are grammar it has not implemented; a few are names it has retired or never
   had, and [src] is a descriptor rather than a property, so asking about it as
   one gets a rejection that says nothing.

   Each harness checks the list against its own population in both directions: a
   name that becomes implemented, and a name that stops being, are both reported
   rather than skipped in silence. *)
let unimplemented =
  [
    "caret";
    "nav-up";
    "nav-down";
    "nav-left";
    "nav-right";
    "initial-letter-align";
    "initial-letter-wrap";
    "inline-sizing";
    "line-fit-edge";
    "line-height-step";
    "margin-trim";
    "mask-border";
    "min-intrinsic-sizing";
    "ruby-merge";
    "text-decoration-skip";
    "text-decoration-skip-box";
    "text-decoration-skip-inset";
    "text-decoration-skip-self";
    "text-decoration-skip-spaces";
    "text-emphasis-skip";
    "glyph-orientation-vertical";
    "image-resolution";
    "font-synthesis-position";
    "-moz-appearance";
    "-moz-osx-font-smoothing";
    "-ms-filter";
    "-o-transition";
    "-o-transform";
    (* Chrome has never carried Gecko's prefixed names, and dropped the
       Microsoft ones with the Trident engine. *)
    "-moz-animation";
    "-moz-animation-delay";
    "-moz-animation-direction";
    "-moz-animation-duration";
    "-moz-animation-fill-mode";
    "-moz-animation-iteration-count";
    "-moz-animation-name";
    "-moz-animation-play-state";
    "-moz-animation-timing-function";
    "-moz-border-radius";
    "-moz-box-shadow";
    "-moz-box-sizing";
    "-moz-orient";
    "-moz-transform";
    "-moz-transition";
    "-moz-transition-delay";
    "-moz-transition-duration";
    "-moz-transition-property";
    "-moz-transition-timing-function";
    "-moz-user-select";
    "-ms-transform";
    "-ms-user-select";
    (* A descriptor of @font-face, not a property: setProperty and CSS.supports
       both take a property name, so neither can be asked about it. *)
    "src";
    "-webkit-backdrop-filter";
    "-webkit-hyphens";
    "-webkit-mask-source-type";
    "-webkit-text-decoration";
    "-webkit-text-decoration-color";
  ]

(* One vector the browser and the manifest disagree about, and the spec text
   that decides it. Every entry has to be used: an entry that excuses nothing is
   reported, so a browser that catches up, or a row that drops the value, takes
   its excuse with it. *)
type excuse = { properties : string list; value : string; why : string }

let sizing =
  [
    "width";
    "height";
    "min-width";
    "min-height";
    "max-width";
    "max-height";
    "inline-size";
    "min-inline-size";
    "max-inline-size";
    "block-size";
    "min-block-size";
    "max-block-size";
    "flex-basis";
  ]

(* Positives Chrome rejects. Each is grammar a specification defines and Chrome
   has not implemented, so the manifest is ahead of the browser rather than
   wrong. The citation is the whole justification: without it an entry is a
   place for a mistaken row to hide. *)
let spec_ahead : excuse list =
  [
    {
      properties = sizing;
      value = "fit-content(20rem)";
      why =
        "CSS Sizing 4 sec. 3.2 adds fit-content() to <box-size>, which every \
         sizing property takes; Chrome has only the bare fit-content keyword";
    };
    {
      properties = [ "background"; "background-image" ];
      value = "cross-fade(url(a.png) 40%, url(b.png))";
      why =
        "CSS Images 4 sec. 2.6: cross-fade() = cross-fade( <cf-image># ); \
         Chrome ships only -webkit-cross-fade()";
    };
    {
      properties = [ "text-decoration-thickness" ];
      value = "hairline";
      why =
        "CSS Text Decoration 4 sec. 2.4 takes <line-width>, and CSS Borders 4 \
         sec. 2.3 defines <line-width> = <length [0,inf]> | hairline | thin | \
         medium | thick";
    };
    {
      properties = [ "text-decoration-thickness" ];
      value = "thin";
      why = "CSS Borders 4 sec. 2.3: thin is a <line-width>";
    };
    {
      properties = [ "text-decoration-thickness" ];
      value = "thick";
      why = "CSS Borders 4 sec. 2.3: thick is a <line-width>";
    };
    {
      properties = [ "overflow-clip-margin" ];
      value = "0";
      why =
        "CSS Overflow 4 sec. 3.2: <visual-box> || <length>, and a unitless \
         zero is a <length>; Chrome takes only a dimension";
    };
    {
      properties = [ "overflow-clip-margin" ];
      value = "calc(1rem + 2px)";
      why =
        "CSS Values 4 sec. 10.1 admits a math function wherever a <length> is \
         accepted, which CSS Overflow 4 sec. 3.2 is; Chrome takes only a \
         dimension";
    };
    {
      properties = [ "text-align" ];
      value = "match-parent";
      why =
        "CSS Text 4 sec. 7.1 lists match-parent; Chrome ships only \
         -webkit-match-parent";
    };
    {
      properties = [ "text-transform" ];
      value = "full-width";
      why =
        "CSS Text 4 sec. 2.1: none | [ capitalize | uppercase | lowercase ] || \
         full-width || full-size-kana | math-auto";
    };
    {
      properties = [ "text-overflow" ];
      value = "\"...\"";
      why =
        "CSS Overflow 4 sec. 4.1: [ clip | ellipsis | <string> | fade | \
         <fade()> ]{1,2}";
    };
    {
      properties = [ "text-overflow" ];
      value = "clip ellipsis";
      why = "CSS Overflow 4 sec. 4.1: the production repeats {1,2}";
    };
    {
      properties = [ "text-combine-upright" ];
      value = "digits";
      why =
        "CSS Writing Modes 4 sec. 9.1: none | all | [ digits <integer [2,4]>? \
         ]; Chrome has only none and all";
    };
    {
      properties = [ "text-combine-upright" ];
      value = "digits 2";
      why = "CSS Writing Modes 4 sec. 9.1: the integer ranges over [2,4]";
    };
    {
      properties = [ "text-combine-upright" ];
      value = "digits 4";
      why = "CSS Writing Modes 4 sec. 9.1: the integer ranges over [2,4]";
    };
    {
      properties = [ "alignment-baseline" ];
      value = "text-bottom";
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and \
         <baseline-metric> begins text-bottom | alphabetic | ideographic; \
         Chrome implements the SVG 1.1 keyword set";
    };
    {
      properties = [ "baseline-shift" ];
      value = "top";
      why =
        "CSS Inline 3 sec. 4.2.3: <length-percentage> | sub | super | top | \
         center | bottom";
    };
    {
      properties = [ "baseline-shift" ];
      value = "center";
      why = "CSS Inline 3 sec. 4.2.3 lists center";
    };
    {
      properties = [ "baseline-shift" ];
      value = "bottom";
      why = "CSS Inline 3 sec. 4.2.3 lists bottom";
    };
    {
      properties = [ "grid-template-rows" ];
      value = "masonry";
      why =
        "the CSS Grid 3 Working Draft of 2024 added masonry to \
         grid-template-rows, and Firefox ships it; the current draft has \
         replaced it with display: grid-lanes, so this row is the one entry \
         here that wants a decision rather than a browser";
    };
    {
      properties = [ "outline-color" ];
      value = "auto";
      why =
        "CSS UI 4 sec. 3.4: auto | <'border-top-color'>, and auto is the \
         initial value; Chrome computes that initial value without accepting \
         the keyword";
    };
    {
      properties = [ "user-select"; "-webkit-user-select" ];
      value = "contain";
      why =
        "CSS UI 4 sec. 6.1: auto | text | none | contain | all; \
         -webkit-user-select is the browser's legacy name for the same \
         property";
    };
    {
      properties = [ "font-synthesis" ];
      value = "style small-caps position";
      why =
        "CSS Fonts 4 sec. 2.8.5: none | [ weight || style || small-caps || \
         position ]; Chrome has no font-synthesis-position";
    };
    {
      properties = [ "font-synthesis-style" ];
      value = "oblique-only";
      why = "CSS Fonts 4 sec. 2.8.2: auto | none | oblique-only";
    };
    {
      properties = [ "ruby-position" ];
      value = "alternate";
      why =
        "CSS Ruby 1 sec. 4.1: [ alternate || [ over | under ] ] | \
         inter-character; Chrome has only over and under";
    };
    {
      properties = [ "ruby-position" ];
      value = "alternate over";
      why = "CSS Ruby 1 sec. 4.1: alternate combines with over under ||";
    };
    {
      properties = [ "ruby-position" ];
      value = "inter-character";
      why = "CSS Ruby 1 sec. 4.1 lists inter-character";
    };
    {
      properties = [ "image-rendering" ];
      value = "smooth";
      why =
        "CSS Images 3 sec. 5.2: auto | smooth | high-quality | pixelated | \
         crisp-edges";
    };
    {
      properties = [ "-webkit-mask-clip" ];
      value = "no-clip";
      why =
        "CSS Masking 1 sec. 7.5: [ <coord-box> | no-clip ]#; Chrome takes \
         no-clip on mask-clip but not on its own -webkit- alias of it";
    };
    {
      properties = [ "stroke-linejoin" ];
      value = "miter-clip";
      why =
        "SVG Strokes sec. 2.6: miter | miter-clip | round | bevel | arcs; \
         Chrome has miter, round and bevel";
    };
    {
      properties = [ "stroke-linejoin" ];
      value = "arcs";
      why = "SVG Strokes sec. 2.6 lists arcs";
    };
    {
      properties = [ "vector-effect" ];
      value = "non-scaling-size";
      why =
        "SVG 2 sec. 8.13: none | [ non-scaling-stroke | non-scaling-size | \
         non-rotation | fixed-position ]+ [ viewport | screen ]?; Chrome has \
         only non-scaling-stroke";
    };
    {
      properties = [ "vector-effect" ];
      value = "non-rotation";
      why = "SVG 2 sec. 8.13 lists non-rotation";
    };
    {
      properties = [ "vector-effect" ];
      value = "fixed-position";
      why = "SVG 2 sec. 8.13 lists fixed-position";
    };
    {
      properties = [ "vector-effect" ];
      value = "non-scaling-stroke screen";
      why = "SVG 2 sec. 8.13: the effect list is followed by viewport | screen";
    };
    {
      properties = [ "vector-effect" ];
      value = "non-scaling-stroke fixed-position";
      why = "SVG 2 sec. 8.13: the effects themselves repeat with +";
    };
  ]

(* Negatives Chrome accepts. Each is a value no specification grants, kept
   invalid on purpose. *)
let lenient : excuse list =
  [
    {
      properties = [ "resize" ];
      value = "auto";
      why =
        "CSS UI 4 sec. 4.1: none | both | horizontal | vertical | block | \
         inline. Chrome accepts auto, no specification defines it";
    };
    {
      properties = [ "text-orientation" ];
      value = "sideways-right";
      why =
        "CSS Writing Modes 4 sec. 5.1: mixed | upright | sideways. \
         sideways-right is a compatibility alias browsers may keep, not \
         grammar";
    };
    {
      properties = [ "alignment-baseline" ];
      value = "auto";
      why =
        "CSS Inline 3 sec. 4.2.2: baseline | <baseline-metric>, and no arm is \
         auto. Chrome accepts it from the SVG 1.1 grammar";
    };
  ]

let unimplemented_property name = List.exists (String.equal name) unimplemented

(* The entry covering [property]: [value], when one exists. *)
let find table ~property ~value =
  let covers (e : excuse) =
    String.equal e.value value
    && List.exists (String.equal property) e.properties
  in
  List.find_opt covers table
