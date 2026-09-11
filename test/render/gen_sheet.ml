(* Seeded stylesheets for the render differential.

   The sheet is built as CSS text and parsed back rather than assembled from
   typed constructors. The pool has to reach the whole property inventory, and a
   table keyed on a property name costs one entry per property where a typed
   builder costs one per value type. The browser, not this file, decides which
   values are valid, so a hand-written value table is an input here; it could
   never be an expectation.

   [Property_names.all] is generated from the reader's own dispatch table, so a
   property added to lib/ enters the sweep without a second edit. A name the
   table below does not value falls back to the CSS-wide keywords, which every
   property accepts, so no property is silently unreachable. *)

open Cascade

(* A linear congruential generator: the sweep needs the same sheet for the same
   seed on every machine, which Random does not promise across versions. *)
type rng = { mutable state : int }

let rng seed = { state = seed * 2654435761 land max_int }

let next r =
  r.state <- ((r.state * 2862933555777941757) + 3037000493) land max_int;
  r.state

let int r n = if n <= 1 then 0 else next r mod n
let pick r a = a.(int r (Array.length a))
let pick_list r l = List.nth l (int r (List.length l))
let chance r n = int r n = 0

let take n l =
  let rec loop n acc = function
    | x :: tl when n > 0 -> loop (n - 1) (x :: acc) tl
    | _ -> List.rev acc
  in
  loop n [] l

let rec drop n l =
  if n <= 0 then l else match l with [] -> [] | _ :: tl -> drop (n - 1) tl

(* ===== Value pools ===== *)

let colours = [| "red"; "#0f0"; "rgb(1 2 3)"; "currentcolor"; "#12345680" |]
let lengths = [| "0"; "1px"; "2px"; "3em"; "0.25rem" |]
let len_pct = [| "0"; "4px"; "10%"; "2em" |]
let auto_len = [| "auto"; "0"; "5px"; "10%" |]
let widths = [| "0"; "1px"; "thin"; "medium"; "thick" |]
let times = [| "0s"; "1s"; "250ms" |]

let easings =
  [|
    "ease";
    "linear";
    "ease-in-out";
    "steps(2, end)";
    "cubic-bezier(0.1, 0.7, 1, 0.1)";
  |]

let line_styles =
  [|
    "none";
    "solid";
    "dashed";
    "dotted";
    "double";
    "groove";
    "ridge";
    "inset";
    "outset";
  |]

let images = [| "none"; "url(rd.png)"; "linear-gradient(red, blue)" |]
let alphas = [| "1"; "0.5"; "0"; "40%" |]
let boxes = [| "border-box"; "padding-box"; "content-box" |]
let positions = [| "center"; "0 0"; "50% 50%"; "left 10px top 20%" |]
let sizes = [| "auto"; "cover"; "contain"; "100px 50%" |]
let repeats = [| "repeat"; "no-repeat"; "repeat-x"; "round space" |]

let self_align =
  [| "center"; "start"; "end"; "stretch"; "flex-start"; "baseline" |]

let content_align =
  [| "center"; "start"; "end"; "stretch"; "space-between"; "space-around" |]

let tracks = [| "none"; "1fr 1fr"; "repeat(3, minmax(0, 1fr))"; "100px auto" |]
let grid_lines = [| "auto"; "1"; "span 2"; "3" |]
let idents = [| "none"; "--rd-t"; "--rd-v" |]
let wide = [| "initial"; "inherit"; "unset"; "revert" |]

(* Keyed on a property name. The heuristics below cover the regular suffixes; an
   entry here is for a name they would get wrong, or for a family the sweep
   leans on. *)
let table : (string list * string array) list =
  [
    (* box *)
    ( [ "width"; "height"; "inline-size"; "block-size" ],
      [| "auto"; "100px"; "50%"; "max-content"; "fit-content" |] );
    ( [ "min-width"; "min-height"; "min-inline-size"; "min-block-size" ],
      [| "0"; "10px"; "auto"; "min-content" |] );
    ( [ "max-width"; "max-height"; "max-inline-size"; "max-block-size" ],
      [| "none"; "200px"; "80%" |] );
    ( [
        "margin";
        "margin-inline";
        "margin-block";
        "inset";
        "inset-inline";
        "inset-block";
      ],
      [| "0"; "4px"; "auto"; "1em 2em"; "0 auto" |] );
    ( [
        "margin-top";
        "margin-right";
        "margin-bottom";
        "margin-left";
        "margin-inline-start";
        "margin-inline-end";
        "margin-block-start";
        "margin-block-end";
        "top";
        "right";
        "bottom";
        "left";
        "inset-inline-start";
        "inset-inline-end";
        "inset-block-start";
        "inset-block-end";
      ],
      [| "auto"; "0"; "3px"; "10%"; "-2px" |] );
    ( [ "padding"; "padding-inline"; "padding-block" ],
      [| "0"; "4px"; "1em 2em"; "5%" |] );
    ( [
        "padding-top";
        "padding-right";
        "padding-bottom";
        "padding-left";
        "padding-inline-start";
        "padding-inline-end";
        "padding-block-start";
        "padding-block-end";
      ],
      len_pct );
    ( [
        "border-radius";
        "border-top-left-radius";
        "border-top-right-radius";
        "border-bottom-left-radius";
        "border-bottom-right-radius";
        "border-start-start-radius";
        "border-start-end-radius";
        "border-end-start-radius";
        "border-end-end-radius";
      ],
      [| "0"; "4px"; "50%"; "1px 2px" |] );
    ([ "box-sizing" ], [| "content-box"; "border-box" |]);
    ([ "aspect-ratio" ], [| "auto"; "1"; "16 / 9"; "auto 1 / 1" |]);
    ([ "z-index" ], [| "auto"; "0"; "3"; "-1" |]);
    ([ "zoom" ], [| "1"; "normal"; "150%"; "2" |]);
    (* display and flow *)
    ( [ "display" ],
      [|
        "block";
        "inline";
        "flex";
        "grid";
        "inline-block";
        "flow-root";
        "inline-flex";
      |] );
    ([ "position" ], [| "static"; "relative"; "absolute"; "sticky" |]);
    ([ "visibility" ], [| "visible"; "hidden"; "collapse" |]);
    ( [
        "overflow";
        "overflow-x";
        "overflow-y";
        "overflow-block";
        "overflow-inline";
      ],
      [| "visible"; "hidden"; "auto"; "scroll"; "clip" |] );
    ([ "float" ], [| "none"; "left"; "right"; "inline-start" |]);
    ([ "clear" ], [| "none"; "left"; "right"; "both" |]);
    ([ "isolation" ], [| "auto"; "isolate" |]);
    ([ "caption-side" ], [| "top"; "bottom" |]);
    ([ "table-layout" ], [| "auto"; "fixed" |]);
    ([ "border-collapse" ], [| "separate"; "collapse" |]);
    ([ "border-spacing" ], [| "0"; "2px"; "2px 4px" |]);
    ([ "vertical-align" ], [| "baseline"; "middle"; "top"; "10px"; "50%" |]);
    ([ "order" ], [| "0"; "1"; "-1" |]);
    (* colour-ish exceptions the suffix rule would get wrong *)
    ([ "scrollbar-color" ], [| "auto"; "red green" |]);
    ([ "accent-color"; "caret-color" ], [| "auto"; "red"; "#0f0" |]);
    ([ "fill"; "stroke" ], [| "red"; "none"; "currentcolor"; "#0f0" |]);
    ([ "color-scheme" ], [| "normal"; "light"; "dark"; "light dark" |]);
    ([ "forced-color-adjust" ], [| "auto"; "none"; "preserve-parent-color" |]);
    ([ "print-color-adjust" ], [| "economy"; "exact" |]);
    (* border *)
    ( [
        "border";
        "border-top";
        "border-right";
        "border-bottom";
        "border-left";
        "border-block";
        "border-inline";
        "border-block-start";
        "border-block-end";
        "border-inline-start";
        "border-inline-end";
        "outline";
        "column-rule";
      ],
      [| "1px solid red"; "medium none currentcolor"; "2px dashed"; "none" |] );
    ( [ "border-style"; "border-block-style"; "border-inline-style" ],
      [| "none"; "solid"; "dashed dotted"; "double" |] );
    ( [
        "border-top-style";
        "border-right-style";
        "border-bottom-style";
        "border-left-style";
        "border-block-start-style";
        "border-block-end-style";
        "border-inline-start-style";
        "border-inline-end-style";
        "outline-style";
      ],
      line_styles );
    ( [ "border-width"; "border-block-width"; "border-inline-width" ],
      [| "0"; "1px"; "thin medium"; "thick" |] );
    ([ "border-color" ], [| "red"; "#0f0"; "red blue"; "currentcolor" |]);
    ( [ "border-block-color"; "border-inline-color" ],
      [| "red"; "#0f0"; "red blue" |] );
    ( [ "border-image"; "mask-border" ],
      [| "none"; "url(rd.png) 30 / 1 / 0 stretch"; "url(rd.png) 10 fill" |] );
    ( [ "border-image-slice"; "mask-border-slice" ],
      [| "100%"; "30"; "10 fill" |] );
    ( [ "border-image-width"; "mask-border-width" ],
      [| "1"; "2px"; "auto"; "10%" |] );
    ([ "border-image-outset"; "mask-border-outset" ], [| "0"; "2px"; "1" |]);
    ( [ "border-image-repeat"; "mask-border-repeat" ],
      [| "stretch"; "repeat"; "round"; "space" |] );
    ([ "mask-border-mode" ], [| "alpha"; "luminance" |]);
    ([ "outline-offset" ], [| "0"; "2px"; "-1px" |]);
    (* background and mask *)
    ( [ "background" ],
      [|
        "red";
        "none";
        "url(rd.png) no-repeat center / cover";
        "#0f0 url(rd.png) repeat-x fixed border-box content-box";
      |] );
    ([ "background-position"; "mask-position"; "object-position" ], positions);
    ( [ "background-position-x"; "background-position-y" ],
      [| "center"; "0"; "50%"; "10px" |] );
    ([ "background-size"; "mask-size" ], sizes);
    ([ "background-repeat"; "mask-repeat" ], repeats);
    ([ "background-attachment" ], [| "scroll"; "fixed"; "local" |]);
    ([ "background-origin"; "mask-origin" ], boxes);
    ( [ "background-clip" ],
      [| "border-box"; "padding-box"; "content-box"; "text" |] );
    ( [ "mask-clip" ],
      [| "border-box"; "padding-box"; "content-box"; "no-clip" |] );
    ( [ "background-blend-mode"; "mix-blend-mode" ],
      [| "normal"; "multiply"; "screen"; "overlay" |] );
    ( [ "mask" ],
      [| "none"; "url(rd.png)"; "url(rd.png) center / cover no-repeat" |] );
    ([ "mask-mode" ], [| "alpha"; "luminance"; "match-source" |]);
    ([ "mask-composite" ], [| "add"; "subtract"; "intersect"; "exclude" |]);
    ([ "mask-type" ], [| "luminance"; "alpha" |]);
    ([ "-webkit-mask-source-type" ], [| "auto"; "alpha"; "luminance" |]);
    ([ "-webkit-mask-composite" ], [| "source-over"; "xor" |]);
    ( [ "-webkit-mask-clip" ],
      [| "border-box"; "padding-box"; "content-box"; "text" |] );
    ( [ "-webkit-background-clip" ],
      [| "border-box"; "padding-box"; "content-box"; "text" |] );
    (* font *)
    ( [ "font" ],
      [|
        "italic bold 12px/1.5 serif";
        "12px sans-serif";
        "caption";
        "small-caps 700 1em/1.2 monospace";
      |] );
    ([ "font-size" ], [| "12px"; "1.2em"; "120%"; "medium"; "larger" |]);
    ([ "font-weight" ], [| "400"; "bold"; "normal"; "700"; "lighter" |]);
    ([ "font-style" ], [| "normal"; "italic"; "oblique"; "oblique 20deg" |]);
    ( [ "font-family" ],
      [| "serif"; "sans-serif"; "monospace"; {|"Helvetica", sans-serif|} |] );
    ([ "font-stretch" ], [| "normal"; "condensed"; "75%"; "expanded" |]);
    ( [ "font-variant" ],
      [| "normal"; "small-caps"; "common-ligatures"; "tabular-nums" |] );
    ( [ "font-variant-caps" ],
      [| "normal"; "small-caps"; "all-small-caps"; "titling-caps" |] );
    ( [ "font-variant-ligatures" ],
      [| "normal"; "none"; "no-common-ligatures"; "discretionary-ligatures" |]
    );
    ( [ "font-variant-numeric" ],
      [| "normal"; "lining-nums"; "tabular-nums"; "slashed-zero" |] );
    ([ "font-variant-position" ], [| "normal"; "sub"; "super" |]);
    ( [ "font-variant-east-asian" ],
      [| "normal"; "jis78"; "proportional-width" |] );
    ([ "font-variant-emoji" ], [| "normal"; "text"; "emoji" |]);
    ([ "font-variant-alternates" ], [| "normal"; "historical-forms" |]);
    ([ "font-feature-settings" ], [| "normal"; {|"liga" 1|}; {|"smcp"|} |]);
    ([ "font-variation-settings" ], [| "normal"; {|"wght" 700|} |]);
    ([ "font-kerning" ], [| "auto"; "normal"; "none" |]);
    ([ "font-optical-sizing" ], [| "auto"; "none" |]);
    ([ "font-language-override" ], [| "normal"; {|"ENG"|} |]);
    ([ "font-size-adjust" ], [| "none"; "0.5"; "ex-height 0.5" |]);
    ([ "font-synthesis" ], [| "none"; "weight style"; "weight"; "small-caps" |]);
    ( [
        "font-synthesis-weight";
        "font-synthesis-style";
        "font-synthesis-small-caps";
        "font-synthesis-position";
      ],
      [| "auto"; "none" |] );
    ([ "font-palette" ], [| "normal"; "light"; "dark" |]);
    ([ "line-height" ], [| "normal"; "1.5"; "20px"; "150%" |]);
    ([ "line-height-step" ], [| "0"; "10px" |]);
    (* text *)
    ( [ "text-align" ],
      [| "left"; "center"; "right"; "justify"; "start"; "end" |] );
    ([ "text-transform" ], [| "none"; "uppercase"; "lowercase"; "capitalize" |]);
    ([ "text-indent" ], [| "0"; "2em"; "10%"; "1em hanging" |]);
    ([ "letter-spacing"; "word-spacing" ], [| "normal"; "0"; "1px"; "0.1em" |]);
    ( [ "text-decoration"; "-webkit-text-decoration" ],
      [|
        "underline";
        "underline dotted red";
        "none";
        "overline 2px";
        "line-through wavy";
      |] );
    ( [ "text-decoration-line" ],
      [| "none"; "underline"; "overline"; "underline line-through" |] );
    ( [ "text-decoration-style" ],
      [| "solid"; "double"; "dotted"; "dashed"; "wavy" |] );
    ([ "text-decoration-thickness" ], [| "auto"; "from-font"; "2px"; "5%" |]);
    ([ "text-underline-offset" ], [| "auto"; "2px"; "10%" |]);
    ([ "text-underline-position" ], [| "auto"; "under"; "from-font"; "left" |]);
    ([ "text-decoration-skip-ink" ], [| "auto"; "none"; "all" |]);
    ([ "text-decoration-skip" ], [| "none"; "objects"; "spaces" |]);
    ( [ "text-decoration-skip-self"; "text-decoration-skip-box" ],
      [| "none"; "all" |] );
    ([ "text-decoration-skip-inset" ], [| "none"; "auto" |]);
    ([ "text-decoration-skip-spaces" ], [| "none"; "all"; "start end" |]);
    ( [ "text-shadow"; "box-shadow"; "-webkit-box-shadow"; "-moz-box-shadow" ],
      [| "none"; "1px 1px red"; "0 0 2px #0f0" |] );
    ( [ "white-space" ],
      [| "normal"; "pre"; "nowrap"; "pre-wrap"; "pre-line"; "break-spaces" |] );
    ([ "text-wrap" ], [| "wrap"; "nowrap"; "balance"; "pretty"; "stable" |]);
    ([ "text-wrap-mode" ], [| "wrap"; "nowrap" |]);
    ([ "text-wrap-style" ], [| "auto"; "balance"; "pretty"; "stable" |]);
    ([ "white-space-collapse" ], [| "collapse"; "preserve"; "preserve-breaks" |]);
    ([ "word-break" ], [| "normal"; "break-all"; "keep-all"; "break-word" |]);
    ([ "overflow-wrap" ], [| "normal"; "break-word"; "anywhere" |]);
    ([ "line-break" ], [| "auto"; "loose"; "normal"; "strict"; "anywhere" |]);
    ([ "hyphens"; "-webkit-hyphens" ], [| "none"; "manual"; "auto" |]);
    ([ "hyphenate-limit-chars" ], [| "auto"; "5"; "6 3 3" |]);
    ([ "tab-size" ], [| "8"; "4"; "2em" |]);
    ([ "text-overflow" ], [| "clip"; "ellipsis" |]);
    ([ "direction" ], [| "ltr"; "rtl" |]);
    ( [ "unicode-bidi" ],
      [| "normal"; "embed"; "isolate"; "bidi-override"; "plaintext" |] );
    ( [ "writing-mode" ],
      [| "horizontal-tb"; "vertical-rl"; "vertical-lr"; "sideways-rl" |] );
    ([ "text-orientation" ], [| "mixed"; "upright"; "sideways" |]);
    ([ "text-combine-upright" ], [| "none"; "all" |]);
    ([ "text-emphasis" ], [| "none"; "filled red"; "dot" |]);
    ([ "text-emphasis-style" ], [| "none"; "filled"; "open dot"; {|"x"|} |]);
    ([ "text-emphasis-position" ], [| "over right"; "under left" |]);
    ([ "text-emphasis-skip" ], [| "spaces"; "punctuation"; "none" |]);
    ([ "text-spacing-trim" ], [| "normal"; "space-all"; "trim-start" |]);
    ( [ "text-size-adjust"; "-webkit-text-size-adjust" ],
      [| "auto"; "none"; "120%" |] );
    ([ "text-box" ], [| "normal"; "trim-both cap alphabetic" |]);
    ([ "text-box-trim" ], [| "none"; "trim-start"; "trim-both" |]);
    ([ "text-box-edge" ], [| "auto"; "cap alphabetic"; "text" |]);
    ([ "line-fit-edge" ], [| "leading"; "text"; "cap alphabetic" |]);
    ([ "inline-sizing" ], [| "normal"; "stretch" |]);
    ([ "interpolate-size" ], [| "numeric-only"; "allow-keywords" |]);
    ( [ "min-intrinsic-sizing" ],
      [| "legacy"; "zero-if-scroll"; "zero-if-extrinsic" |] );
    ([ "initial-letter" ], [| "normal"; "2"; "2 1" |]);
    ([ "initial-letter-align" ], [| "auto"; "alphabetic"; "hanging" |]);
    ([ "initial-letter-wrap" ], [| "none"; "first"; "all" |]);
    ([ "ruby-align" ], [| "start"; "center"; "space-between"; "space-around" |]);
    ([ "ruby-merge" ], [| "separate"; "collapse"; "auto" |]);
    ([ "ruby-overhang" ], [| "auto"; "none" |]);
    ([ "ruby-position" ], [| "over"; "under" |]);
    ([ "quotes" ], [| "none"; "auto"; {|"<" ">"|} |]);
    ([ "content" ], [| "normal"; "none"; {|"x"|}; {|"a" "b"|} |]);
    ([ "counter-reset" ], [| "none"; "rd 0"; "rd" |]);
    ([ "counter-increment" ], [| "none"; "rd 1" |]);
    (* flex and grid *)
    ([ "flex" ], [| "1"; "none"; "auto"; "2 3 10px"; "0 1 auto" |]);
    ([ "flex-grow"; "flex-shrink" ], [| "0"; "1"; "2" |]);
    ([ "flex-basis" ], [| "auto"; "0%"; "100px"; "content" |]);
    ( [ "flex-direction"; "-webkit-flex-direction" ],
      [| "row"; "column"; "row-reverse"; "column-reverse" |] );
    ( [ "flex-wrap"; "-webkit-flex-wrap" ],
      [| "nowrap"; "wrap"; "wrap-reverse" |] );
    ( [ "flex-flow"; "-webkit-flex-flow" ],
      [| "row wrap"; "column"; "wrap-reverse" |] );
    ( [ "align-items"; "align-self"; "justify-items"; "justify-self" ],
      self_align );
    ([ "align-content"; "justify-content" ], content_align);
    ( [ "place-items"; "place-self" ],
      [| "center"; "start end"; "stretch"; "baseline" |] );
    ( [ "place-content" ],
      [| "center"; "start end"; "stretch"; "space-between center" |] );
    ([ "gap" ], [| "0"; "4px"; "1em"; "4px 8px" |]);
    ([ "row-gap" ], [| "0"; "4px"; "1em"; "2%" |]);
    ([ "column-gap" ], [| "normal"; "0"; "4px"; "1em" |]);
    ([ "grid-template-columns"; "grid-template-rows" ], tracks);
    ([ "grid-template-areas" ], [| "none"; {|"a b" "c d"|} |]);
    ([ "grid-auto-flow" ], [| "row"; "column"; "row dense"; "dense" |]);
    ( [ "grid-auto-rows"; "grid-auto-columns" ],
      [| "auto"; "1fr"; "minmax(10px, auto)" |] );
    ([ "grid" ], [| "none"; "auto-flow / 1fr 1fr"; "1fr / auto-flow 2em" |]);
    ([ "grid-template" ], [| "none"; {|"a" 1fr / 1fr|}; "1fr / 2fr" |]);
    ([ "grid-area" ], [| "auto"; "1 / 2 / 3 / 4"; "span 2" |]);
    ([ "grid-row"; "grid-column" ], [| "auto"; "1 / 3"; "span 2" |]);
    ( [
        "grid-row-start"; "grid-row-end"; "grid-column-start"; "grid-column-end";
      ],
      grid_lines );
    (* transition and animation *)
    ( [ "transition"; "-webkit-transition"; "-moz-transition"; "-o-transition" ],
      [|
        "color 1s";
        "all 2s ease-in 1s";
        "none";
        "opacity 0.3s linear allow-discrete";
      |] );
    ( [
        "transition-property";
        "-webkit-transition-property";
        "-moz-transition-property";
      ],
      [| "none"; "all"; "color"; "color, opacity" |] );
    ([ "transition-behavior" ], [| "normal"; "allow-discrete" |]);
    ( [ "animation"; "-webkit-animation"; "-moz-animation" ],
      [| "none"; "rd-spin 2s linear"; "1s 2s infinite alternate none" |] );
    ( [ "animation-name"; "-webkit-animation-name"; "-moz-animation-name" ],
      [| "none"; "rd-spin"; "rd-fade" |] );
    ( [
        "animation-iteration-count";
        "-webkit-animation-iteration-count";
        "-moz-animation-iteration-count";
      ],
      [| "1"; "2"; "infinite"; "0.5" |] );
    ( [
        "animation-direction";
        "-webkit-animation-direction";
        "-moz-animation-direction";
      ],
      [| "normal"; "reverse"; "alternate"; "alternate-reverse" |] );
    ( [
        "animation-fill-mode";
        "-webkit-animation-fill-mode";
        "-moz-animation-fill-mode";
      ],
      [| "none"; "forwards"; "backwards"; "both" |] );
    ( [
        "animation-play-state";
        "-webkit-animation-play-state";
        "-moz-animation-play-state";
      ],
      [| "running"; "paused" |] );
    ([ "animation-composition" ], [| "replace"; "add"; "accumulate" |]);
    ([ "animation-timeline" ], [| "auto"; "none"; "--rd-t" |]);
    ([ "animation-range" ], [| "normal"; "entry 10% exit 90%" |]);
    ( [ "animation-range-start"; "animation-range-end" ],
      [| "normal"; "entry 10%"; "50%" |] );
    (* transform and filter *)
    ( [
        "transform";
        "-webkit-transform";
        "-moz-transform";
        "-ms-transform";
        "-o-transform";
      ],
      [|
        "none";
        "translateX(10px)";
        "scale(2) rotate(45deg)";
        "matrix(1, 0, 0, 1, 5, 5)";
      |] );
    ([ "transform-origin" ], [| "center"; "0 0"; "50% 50% 10px"; "left top" |]);
    ( [ "transform-box" ],
      [| "content-box"; "border-box"; "fill-box"; "view-box"; "stroke-box" |] );
    ([ "transform-style" ], [| "flat"; "preserve-3d" |]);
    ([ "translate" ], [| "none"; "10px"; "10px 20px"; "1px 2px 3px" |]);
    ([ "rotate" ], [| "none"; "45deg"; "x 30deg"; "1 1 1 90deg" |]);
    ([ "scale" ], [| "none"; "2"; "1 2"; "1 2 3" |]);
    ([ "perspective" ], [| "none"; "500px" |]);
    ([ "perspective-origin" ], [| "center"; "0 0"; "50% 50%" |]);
    ([ "backface-visibility" ], [| "visible"; "hidden" |]);
    ( [
        "filter"; "backdrop-filter"; "-webkit-filter"; "-webkit-backdrop-filter";
      ],
      [|
        "none";
        "blur(2px)";
        "grayscale(50%) blur(1px)";
        "drop-shadow(1px 1px red)";
      |] );
    ([ "-ms-filter" ], [| "none" |]);
    (* columns *)
    ([ "columns" ], [| "auto"; "2"; "10em"; "2 10em" |]);
    ([ "column-width" ], [| "auto"; "10em"; "100px" |]);
    ([ "column-count" ], [| "auto"; "2"; "3" |]);
    ([ "column-rule-width" ], widths);
    ([ "column-rule-style" ], line_styles);
    ([ "column-span" ], [| "none"; "all" |]);
    (* lists *)
    ([ "list-style" ], [| "square inside"; "none"; "disc outside none" |]);
    ([ "list-style-type" ], [| "disc"; "decimal"; "none"; "square"; {|"-"|} |]);
    ([ "list-style-position" ], [| "inside"; "outside" |]);
    (* offset and anchor positioning *)
    ([ "offset" ], [| "none"; {|path("M 0 0 L 10 10") 10px auto|}; "50% 50%" |]);
    ( [ "offset-path" ],
      [| "none"; {|path("M 0 0 L 10 10")|}; "ray(45deg closest-side)" |] );
    ([ "offset-distance" ], [| "0"; "10px"; "50%" |]);
    ([ "offset-rotate" ], [| "auto"; "0deg"; "reverse"; "auto 30deg" |]);
    ([ "offset-anchor" ], [| "auto"; "center"; "0 0" |]);
    ([ "offset-position" ], [| "normal"; "auto"; "50% 50%" |]);
    ([ "anchor-name" ], [| "none"; "--rd-anchor" |]);
    ([ "position-anchor" ], [| "auto"; "--rd-anchor" |]);
    ([ "position-area" ], [| "none"; "top left"; "block-start" |]);
    ([ "position-try" ], [| "none"; "most-width --rd-pt" |]);
    ([ "position-try-fallbacks" ], [| "none"; "--rd-pt"; "flip-block" |]);
    ([ "position-try-order" ], [| "normal"; "most-width"; "most-height" |]);
    ([ "position-visibility" ], [| "always"; "anchors-visible"; "no-overflow" |]);
    (* scrolling *)
    ([ "scroll-behavior" ], [| "auto"; "smooth" |]);
    ( [ "scroll-margin"; "scroll-margin-inline"; "scroll-margin-block" ],
      [| "0"; "4px"; "1em 2em" |] );
    ( [ "scroll-padding"; "scroll-padding-inline"; "scroll-padding-block" ],
      [| "auto"; "0"; "4px"; "1em 2em" |] );
    ([ "scroll-snap-type" ], [| "none"; "x mandatory"; "both proximity"; "y" |]);
    ([ "scroll-snap-align" ], [| "none"; "start"; "center"; "end start" |]);
    ([ "scroll-snap-stop" ], [| "normal"; "always" |]);
    ( [
        "overscroll-behavior";
        "overscroll-behavior-x";
        "overscroll-behavior-y";
        "overscroll-behavior-block";
        "overscroll-behavior-inline";
      ],
      [| "auto"; "contain"; "none" |] );
    ([ "scrollbar-width" ], [| "auto"; "thin"; "none" |]);
    ([ "scrollbar-gutter" ], [| "auto"; "stable"; "stable both-edges" |]);
    ([ "overflow-anchor" ], [| "auto"; "none" |]);
    ([ "overflow-clip-margin" ], [| "0px"; "5px"; "content-box 2px" |]);
    ([ "scroll-timeline" ], [| "none"; "--rd-t block" |]);
    ([ "view-timeline" ], [| "none"; "--rd-v block" |]);
    ([ "scroll-timeline-name"; "view-timeline-name"; "timeline-scope" ], idents);
    ( [ "scroll-timeline-axis"; "view-timeline-axis" ],
      [| "block"; "inline"; "x"; "y" |] );
    ([ "view-timeline-inset" ], [| "auto"; "0"; "10px 20px" |]);
    ([ "view-transition-name" ], [| "none"; "rd-vt" |]);
    ([ "view-transition-class" ], [| "none"; "rd-vc" |]);
    (* containment *)
    ([ "contain" ], [| "none"; "strict"; "content"; "layout paint" |]);
    ([ "container" ], [| "none"; "rd-card / inline-size" |]);
    ([ "container-name" ], [| "none"; "rd-card" |]);
    ([ "container-type" ], [| "normal"; "inline-size"; "size" |]);
    ([ "content-visibility" ], [| "visible"; "auto"; "hidden" |]);
    ( [ "contain-intrinsic-size" ],
      [| "none"; "100px"; "100px 200px"; "auto 300px" |] );
    ( [
        "contain-intrinsic-width";
        "contain-intrinsic-height";
        "contain-intrinsic-block-size";
        "contain-intrinsic-inline-size";
      ],
      [| "none"; "100px"; "auto 200px" |] );
    (* fragmentation *)
    ([ "break-before"; "break-after" ], [| "auto"; "avoid"; "page"; "column" |]);
    ([ "break-inside" ], [| "auto"; "avoid"; "avoid-page"; "avoid-column" |]);
    ( [ "page-break-before"; "page-break-after" ],
      [| "auto"; "always"; "avoid"; "left"; "right" |] );
    ([ "page-break-inside" ], [| "auto"; "avoid" |]);
    ( [ "box-decoration-break"; "-webkit-box-decoration-break" ],
      [| "slice"; "clone" |] );
    ([ "size" ], [| "auto"; "A4"; "landscape" |]);
    ([ "src" ], [| "url(rd.woff2)" |]);
    ([ "margin-trim" ], [| "none"; "block"; "inline"; "block-start" |]);
    (* SVG and images *)
    ([ "fill-rule"; "clip-rule" ], [| "nonzero"; "evenodd" |]);
    ([ "stroke-width" ], [| "1"; "2px"; "50%" |]);
    ([ "stroke-linecap" ], [| "butt"; "round"; "square" |]);
    ([ "stroke-linejoin" ], [| "miter"; "round"; "bevel" |]);
    ([ "stroke-miterlimit" ], [| "4"; "1"; "10" |]);
    ([ "stroke-dasharray" ], [| "none"; "4 2"; "5%" |]);
    ([ "paint-order" ], [| "normal"; "stroke"; "markers fill" |]);
    ([ "vector-effect" ], [| "none"; "non-scaling-stroke" |]);
    ([ "dominant-baseline" ], [| "auto"; "middle"; "hanging" |]);
    ([ "alignment-baseline" ], [| "baseline"; "middle"; "central" |]);
    ([ "baseline-shift" ], [| "0"; "sub"; "super"; "10%" |]);
    ([ "baseline-source" ], [| "auto"; "first"; "last" |]);
    ([ "glyph-orientation-vertical" ], [| "auto"; "0deg"; "90deg" |]);
    ([ "shape-outside" ], [| "none"; "circle(50%)"; "margin-box" |]);
    ([ "shape-image-threshold" ], [| "0"; "0.5"; "1" |]);
    ([ "clip-path" ], [| "none"; "circle(50%)"; "inset(10px)"; "border-box" |]);
    ([ "clip" ], [| "auto"; "rect(0px, 10px, 10px, 0px)" |]);
    ([ "image-rendering" ], [| "auto"; "pixelated"; "crisp-edges" |]);
    ([ "image-orientation" ], [| "none"; "from-image" |]);
    ([ "image-resolution" ], [| "1dppx"; "from-image"; "snap" |]);
    ([ "object-fit" ], [| "fill"; "contain"; "cover"; "none"; "scale-down" |]);
    ([ "object-view-box" ], [| "none"; "inset(10%)" |]);
    (* user interface *)
    ( [ "cursor" ],
      [| "auto"; "pointer"; "default"; "grab"; "url(rd.cur), pointer" |] );
    ([ "resize" ], [| "none"; "both"; "horizontal"; "vertical"; "block" |]);
    ( [
        "user-select";
        "-webkit-user-select";
        "-ms-user-select";
        "-moz-user-select";
      ],
      [| "auto"; "none"; "text"; "all" |] );
    ([ "pointer-events" ], [| "auto"; "none"; "visiblePainted" |]);
    ( [ "appearance"; "-webkit-appearance"; "-moz-appearance" ],
      [| "none"; "auto"; "textfield" |] );
    ([ "caret" ], [| "auto"; "red auto"; "red bar" |]);
    ([ "caret-shape" ], [| "auto"; "bar"; "block"; "underscore" |]);
    ([ "caret-animation" ], [| "auto"; "manual" |]);
    ([ "touch-action" ], [| "auto"; "none"; "pan-x"; "manipulation" |]);
    ([ "will-change" ], [| "auto"; "transform"; "opacity, transform" |]);
    ([ "field-sizing" ], [| "fixed"; "content" |]);
    ([ "interactivity" ], [| "auto"; "inert" |]);
    ([ "overlay" ], [| "none"; "auto" |]);
    ([ "nav-up"; "nav-right"; "nav-down"; "nav-left" ], [| "auto"; "#lead" |]);
    ( [ "-webkit-font-smoothing" ],
      [| "auto"; "antialiased"; "subpixel-antialiased" |] );
    ([ "-moz-osx-font-smoothing" ], [| "auto"; "grayscale" |]);
    ([ "-moz-orient" ], [| "inline"; "block"; "horizontal"; "vertical" |]);
    ([ "-webkit-line-clamp" ], [| "none"; "2" |]);
    ([ "-webkit-box-orient" ], [| "horizontal"; "vertical" |]);
    (* the reset shorthand: rare on purpose, it shadows every other write *)
    ([ "all" ], [| "initial"; "revert" |]);
  ]

let pools : (string, string array) Hashtbl.t =
  let t = Hashtbl.create 1024 in
  List.iter
    (fun (names, pool) -> List.iter (fun n -> Hashtbl.replace t n pool) names)
    table;
  t

let vendor_prefixes = [ "-webkit-"; "-moz-"; "-ms-"; "-o-" ]

let unprefixed name =
  let rec loop = function
    | [] -> None
    | p :: tl ->
        if String.starts_with ~prefix:p name then
          Some
            (String.sub name (String.length p)
               (String.length name - String.length p))
        else loop tl
  in
  loop vendor_prefixes

let ends_with suffix name = String.ends_with ~suffix name

(* The regular suffixes. A guess the browser rejects costs a dropped
   declaration, never a wrong verdict, so the fallback stays cheap. *)
let by_suffix name =
  if ends_with "-color" name then Some colours
  else if ends_with "-image" name then Some images
  else if ends_with "-opacity" name then Some alphas
  else if ends_with "-duration" name || ends_with "-delay" name then Some times
  else if ends_with "-timing-function" name then Some easings
  else if ends_with "-style" name then Some line_styles
  else if ends_with "-width" name then Some widths
  else if
    ends_with "-radius" name || ends_with "-offset" name
    || ends_with "-thickness" name
    || ends_with "-spacing" name || ends_with "-indent" name
    || ends_with "-outset" name || ends_with "-margin" name
    || ends_with "-padding" name || ends_with "-gap" name
    || ends_with "-distance" name
    || ends_with "-dashoffset" name
  then Some lengths
  else if ends_with "-size" name then Some auto_len
  else if ends_with "-position" name then Some positions
  else if ends_with "-name" name then Some idents
  else None

let pool name =
  match Hashtbl.find_opt pools name with
  | Some p -> p
  | None -> (
      let base = match unprefixed name with Some b -> b | None -> name in
      match Hashtbl.find_opt pools base with
      | Some p -> p
      | None -> ( match by_suffix base with Some p -> p | None -> wide))

(* Every property accepts a CSS-wide keyword, so a name the table misses is
   still written, and the sweep still sees the slot. *)
let value r name = if chance r 12 then pick r wide else pick r (pool name)
let decl r name = String.concat "" [ name; ": "; value r name ]

let importance r d =
  if chance r 4 then String.concat "" [ d; " !important" ] else d

(* ===== Shorthand families =====

   A shorthand and the slots it resets. The high-yield shape is a longhand run
   that leaves one of these slots unwritten: contracting the run into the
   shorthand resets the slot, and a pass that does it anyway silently drops
   whatever wrote it. Slots the library does not model are listed too - the
   browser reads them either way, and an unmodelled slot is exactly the one a
   contraction forgets. *)
type family = { shorthand : string; slots : string list }

let families =
  [
    {
      shorthand = "transition";
      slots =
        [
          "transition-property";
          "transition-duration";
          "transition-timing-function";
          "transition-delay";
          "transition-behavior";
        ];
    };
    {
      shorthand = "animation";
      slots =
        [
          "animation-name";
          "animation-duration";
          "animation-timing-function";
          "animation-delay";
          "animation-iteration-count";
          "animation-direction";
          "animation-fill-mode";
          "animation-play-state";
          "animation-composition";
          "animation-timeline";
          "animation-range-start";
          "animation-range-end";
        ];
    };
    {
      shorthand = "background";
      slots =
        [
          "background-image";
          "background-position-x";
          "background-position-y";
          "background-size";
          "background-repeat";
          "background-attachment";
          "background-origin";
          "background-clip";
          "background-color";
        ];
    };
    {
      shorthand = "mask";
      slots =
        [
          "mask-image";
          "mask-mode";
          "mask-repeat";
          "mask-position";
          "mask-clip";
          "mask-origin";
          "mask-size";
          "mask-composite";
        ];
    };
    {
      shorthand = "border";
      slots = [ "border-width"; "border-style"; "border-color"; "border-image" ];
    };
    {
      shorthand = "border-image";
      slots =
        [
          "border-image-source";
          "border-image-slice";
          "border-image-width";
          "border-image-outset";
          "border-image-repeat";
        ];
    };
    {
      shorthand = "font";
      slots =
        [
          "font-style";
          "font-variant";
          "font-weight";
          "font-stretch";
          "font-size";
          "line-height";
          "font-family";
          "font-size-adjust";
          "font-kerning";
          "font-optical-sizing";
          "font-feature-settings";
          "font-variation-settings";
          "font-language-override";
          "font-variant-caps";
          "font-variant-ligatures";
          "font-variant-numeric";
          "font-variant-position";
          "font-variant-east-asian";
        ];
    };
    {
      shorthand = "font-variant";
      slots =
        [
          "font-variant-ligatures";
          "font-variant-caps";
          "font-variant-numeric";
          "font-variant-position";
          "font-variant-east-asian";
          "font-variant-emoji";
          "font-variant-alternates";
        ];
    };
    {
      shorthand = "font-synthesis";
      slots =
        [
          "font-synthesis-weight";
          "font-synthesis-style";
          "font-synthesis-small-caps";
          "font-synthesis-position";
        ];
    };
    {
      shorthand = "grid";
      slots =
        [
          "grid-template-rows";
          "grid-template-columns";
          "grid-template-areas";
          "grid-auto-rows";
          "grid-auto-columns";
          "grid-auto-flow";
        ];
    };
    {
      shorthand = "grid-template";
      slots =
        [ "grid-template-rows"; "grid-template-columns"; "grid-template-areas" ];
    };
    {
      shorthand = "grid-area";
      slots =
        [
          "grid-row-start";
          "grid-column-start";
          "grid-row-end";
          "grid-column-end";
        ];
    };
    {
      shorthand = "offset";
      slots =
        [
          "offset-position";
          "offset-path";
          "offset-distance";
          "offset-rotate";
          "offset-anchor";
        ];
    };
    { shorthand = "columns"; slots = [ "column-width"; "column-count" ] };
    {
      shorthand = "column-rule";
      slots = [ "column-rule-width"; "column-rule-style"; "column-rule-color" ];
    };
    {
      shorthand = "text-decoration";
      slots =
        [
          "text-decoration-line";
          "text-decoration-style";
          "text-decoration-color";
          "text-decoration-thickness";
        ];
    };
    {
      shorthand = "text-emphasis";
      slots =
        [
          "text-emphasis-style"; "text-emphasis-color"; "text-emphasis-position";
        ];
    };
    { shorthand = "flex"; slots = [ "flex-grow"; "flex-shrink"; "flex-basis" ] };
    { shorthand = "flex-flow"; slots = [ "flex-direction"; "flex-wrap" ] };
    {
      shorthand = "list-style";
      slots = [ "list-style-position"; "list-style-image"; "list-style-type" ];
    };
    {
      shorthand = "outline";
      slots =
        [ "outline-width"; "outline-style"; "outline-color"; "outline-offset" ];
    };
    {
      shorthand = "margin";
      slots = [ "margin-top"; "margin-right"; "margin-bottom"; "margin-left" ];
    };
    {
      shorthand = "padding";
      slots =
        [ "padding-top"; "padding-right"; "padding-bottom"; "padding-left" ];
    };
    { shorthand = "inset"; slots = [ "top"; "right"; "bottom"; "left" ] };
    {
      shorthand = "margin-inline";
      slots = [ "margin-inline-start"; "margin-inline-end" ];
    };
    {
      shorthand = "margin-block";
      slots = [ "margin-block-start"; "margin-block-end" ];
    };
    {
      shorthand = "padding-inline";
      slots = [ "padding-inline-start"; "padding-inline-end" ];
    };
    {
      shorthand = "padding-block";
      slots = [ "padding-block-start"; "padding-block-end" ];
    };
    {
      shorthand = "inset-inline";
      slots = [ "inset-inline-start"; "inset-inline-end" ];
    };
    {
      shorthand = "inset-block";
      slots = [ "inset-block-start"; "inset-block-end" ];
    };
    {
      shorthand = "border-width";
      slots =
        [
          "border-top-width";
          "border-right-width";
          "border-bottom-width";
          "border-left-width";
        ];
    };
    {
      shorthand = "border-style";
      slots =
        [
          "border-top-style";
          "border-right-style";
          "border-bottom-style";
          "border-left-style";
        ];
    };
    {
      shorthand = "border-color";
      slots =
        [
          "border-top-color";
          "border-right-color";
          "border-bottom-color";
          "border-left-color";
        ];
    };
    {
      shorthand = "border-radius";
      slots =
        [
          "border-top-left-radius";
          "border-top-right-radius";
          "border-bottom-right-radius";
          "border-bottom-left-radius";
        ];
    };
    {
      shorthand = "border-block";
      slots =
        [
          "border-block-start-width";
          "border-block-start-style";
          "border-block-start-color";
          "border-block-end-width";
          "border-block-end-style";
          "border-block-end-color";
        ];
    };
    {
      shorthand = "border-inline";
      slots =
        [
          "border-inline-start-width";
          "border-inline-start-style";
          "border-inline-start-color";
          "border-inline-end-width";
          "border-inline-end-style";
          "border-inline-end-color";
        ];
    };
    {
      shorthand = "border-top";
      slots = [ "border-top-width"; "border-top-style"; "border-top-color" ];
    };
    {
      shorthand = "border-left";
      slots = [ "border-left-width"; "border-left-style"; "border-left-color" ];
    };
    { shorthand = "gap"; slots = [ "row-gap"; "column-gap" ] };
    { shorthand = "overflow"; slots = [ "overflow-x"; "overflow-y" ] };
    {
      shorthand = "overscroll-behavior";
      slots = [ "overscroll-behavior-x"; "overscroll-behavior-y" ];
    };
    {
      shorthand = "place-content";
      slots = [ "align-content"; "justify-content" ];
    };
    { shorthand = "place-items"; slots = [ "align-items"; "justify-items" ] };
    { shorthand = "place-self"; slots = [ "align-self"; "justify-self" ] };
    {
      shorthand = "scroll-margin";
      slots =
        [
          "scroll-margin-top";
          "scroll-margin-right";
          "scroll-margin-bottom";
          "scroll-margin-left";
        ];
    };
    {
      shorthand = "scroll-padding";
      slots =
        [
          "scroll-padding-top";
          "scroll-padding-right";
          "scroll-padding-bottom";
          "scroll-padding-left";
        ];
    };
    { shorthand = "container"; slots = [ "container-name"; "container-type" ] };
    {
      shorthand = "caret";
      slots = [ "caret-color"; "caret-shape"; "caret-animation" ];
    };
    {
      shorthand = "scroll-timeline";
      slots = [ "scroll-timeline-name"; "scroll-timeline-axis" ];
    };
    {
      shorthand = "view-timeline";
      slots =
        [ "view-timeline-name"; "view-timeline-axis"; "view-timeline-inset" ];
    };
    {
      shorthand = "animation-range";
      slots = [ "animation-range-start"; "animation-range-end" ];
    };
    { shorthand = "text-wrap"; slots = [ "text-wrap-mode"; "text-wrap-style" ] };
    {
      shorthand = "white-space";
      slots = [ "white-space-collapse"; "text-wrap-mode" ];
    };
    { shorthand = "text-box"; slots = [ "text-box-trim"; "text-box-edge" ] };
    {
      shorthand = "position-try";
      slots = [ "position-try-order"; "position-try-fallbacks" ];
    };
    {
      shorthand = "contain-intrinsic-size";
      slots = [ "contain-intrinsic-width"; "contain-intrinsic-height" ];
    };
    {
      shorthand = "mask-border";
      slots =
        [
          "mask-border-source";
          "mask-border-slice";
          "mask-border-width";
          "mask-border-outset";
          "mask-border-repeat";
          "mask-border-mode";
        ];
    };
  ]

let families_array = Array.of_list families

(* ===== Selectors ===== *)

let selectors =
  [|
    ".a";
    ".b";
    ".card";
    "div";
    "p";
    "li";
    "span";
    "#lead";
    "li:first-child";
    "li:last-child";
    ".a:nth-child(2)";
    {|[data-k="v"]|};
    "p:not(.b)";
    ".card > p";
    "ul li + li";
    ".card p";
  |]

(* Pairs that share elements without being equal, so the browser has to resolve
   a cascade the optimizer cannot settle by merging two identical preludes. *)
let overlaps =
  [|
    (".a", ".a.b");
    ("div", "div.a");
    (".card p", "p");
    (":is(.a, .b)", ".a");
    ("li", "li:first-child");
    ("[data-k]", {|[data-k="v"]|});
    (".card", ".card.b");
    ("p", ".card > p");
  |]

let nest_suffixes =
  [| "& p"; "&.b"; "& > span"; "& li:first-child"; ".card &" |]

(* ===== Statements ===== *)

type node =
  | Rule of { selector : string; decls : string list; nested : node list }
  | At of { prelude : string; body : node list }

let rule ?(nested = []) selector decls = Rule { selector; decls; nested }

let rec emit buf node =
  match node with
  | Rule { selector; decls; nested } -> (
      match (decls, nested) with
      | [], [] -> ()
      | _ ->
          Buffer.add_string buf selector;
          Buffer.add_string buf " {\n";
          List.iter
            (fun d ->
              Buffer.add_string buf "  ";
              Buffer.add_string buf d;
              Buffer.add_string buf ";\n")
            decls;
          List.iter (emit buf) nested;
          Buffer.add_string buf "}\n")
  | At { prelude; body } -> (
      match body with
      | [] -> ()
      | _ ->
          Buffer.add_string buf prelude;
          Buffer.add_string buf " {\n";
          List.iter (emit buf) body;
          Buffer.add_string buf "}\n")

(* ===== Shapes ===== *)

let commons =
  [|
    "color";
    "background-color";
    "width";
    "opacity";
    "display";
    "font-weight";
    "z-index";
  |]

let single r = decl r (pick r commons)

(* A contiguous wrapped window of [l], and its complement: the window is the
   longhand run a shorthand could swallow, the complement the slots it would
   reset without anything having written them in the run. *)
let window r l =
  let n = List.length l in
  let keep = 1 + int r n in
  let off = int r n in
  let inside i = (i - off + n) mod n < keep in
  ( List.filteri (fun i _ -> inside i) l,
    List.filteri (fun i _ -> not (inside i)) l )

(* A longhand run cut across a rule boundary, with the slots it leaves unwritten
   set in a rule of their own. Contracting the run into its shorthand resets
   those slots, so the cut point and the side the gap declarations sit on are
   both swept. *)
let split_run r =
  let f = pick r families_array in
  let run, gaps = window r f.slots in
  let sel = pick r selectors in
  let cut = int r (List.length run + 1) in
  let head = List.map (fun s -> importance r (decl r s)) (take cut run) in
  let tail = List.map (fun s -> importance r (decl r s)) (drop cut run) in
  let gap_rule =
    match gaps with [] -> [] | _ -> [ rule sel (List.map (decl r) gaps) ]
  in
  let between =
    if chance r 2 then [ rule (pick r selectors) [ single r ] ] else []
  in
  let run_rules =
    List.filter_map
      (fun ds -> match ds with [] -> None | _ -> Some (rule sel ds))
      [ head; tail ]
  in
  let run_rules =
    match run_rules with
    | a :: b :: tl -> (a :: between) @ (b :: tl)
    | _ -> run_rules
  in
  if chance r 2 then gap_rule @ run_rules else run_rules @ gap_rule

(* A shorthand beside one of its own longhands, in either order, in one rule or
   in two rules sharing the selector. *)
let shorthand_beside_longhand r =
  let f = pick r families_array in
  let sel = pick r selectors in
  let sh = importance r (decl r f.shorthand) in
  let lo = importance r (decl r (pick_list r f.slots)) in
  let a, b = if chance r 2 then (sh, lo) else (lo, sh) in
  if chance r 2 then [ rule sel [ a; b ] ]
  else if chance r 2 then [ rule sel [ a ]; rule sel [ b ] ]
  else [ rule sel [ a ]; rule (pick r selectors) [ single r ]; rule sel [ b ] ]

(* The same selector twice, adjacent or with an unrelated rule between. *)
let repeated r =
  let f = pick r families_array in
  let sel = pick r selectors in
  let first = rule sel [ importance r (decl r f.shorthand) ] in
  let second = rule sel (List.map (decl r) (take (1 + int r 3) f.slots)) in
  if chance r 2 then [ first; second ]
  else [ first; rule (pick r selectors) [ single r ]; second ]

let overlapping r =
  let a, b = pick r overlaps in
  let f = pick r families_array in
  [
    rule a [ importance r (decl r f.shorthand) ];
    rule b
      (List.map (fun s -> importance r (decl r s)) (take (1 + int r 2) f.slots));
  ]

let nested r =
  let f = pick r families_array in
  let sel = pick r selectors in
  let inner = pick r nest_suffixes in
  [
    rule sel
      [ decl r f.shorthand ]
      ~nested:[ rule inner (List.map (decl r) (take (1 + int r 3) f.slots)) ];
  ]

(* The property inventory, sliced by seed: consecutive seeds walk the whole
   list, so a property the table values badly still shows up in the sweep. *)
let inventory = Array.of_list Property_names.all
let slice_size = 17

let long_tail r seed =
  let n = Array.length inventory in
  let start = seed * slice_size mod n in
  let names = List.init slice_size (fun i -> inventory.((start + i) mod n)) in
  let sel = pick r selectors in
  let decls = List.map (fun name -> importance r (decl r name)) names in
  let cut = 1 + int r (List.length decls - 1) in
  [ rule sel (take cut decls); rule sel (drop cut decls) ]

let shape r seed =
  match int r 10 with
  | 0 -> repeated r
  | 1 -> shorthand_beside_longhand r
  | 2 | 3 | 4 -> split_run r
  | 5 -> overlapping r
  | 6 -> nested r
  | 7 -> long_tail r seed
  | _ ->
      [ rule (pick r selectors) (List.init (1 + int r 3) (fun _ -> single r)) ]

(* A wrapper decides whether two rules may merge at all, so the sweep puts one
   around a whole shape and, half the time, around one of its rules alone. *)
let wrapper r =
  pick r
    [|
      "@media (min-width: 400px)";
      "@media (max-width: 2000px)";
      "@supports (display: grid)";
      "@supports not (display: rd-nonesuch)";
      "@layer rd-base";
    |]

let decorate r nodes =
  match int r 6 with
  | 0 -> [ At { prelude = wrapper r; body = nodes } ]
  | 1 -> (
      match nodes with
      | first :: tl -> At { prelude = wrapper r; body = [ first ] } :: tl
      | [] -> nodes)
  | 2 -> (
      match List.rev nodes with
      | last :: tl ->
          List.rev (At { prelude = wrapper r; body = [ last ] } :: tl)
      | [] -> nodes)
  | _ -> nodes

let source ~seed =
  let r = rng seed in
  let n = 3 + int r 4 in
  let shapes =
    List.concat_map (fun _ -> decorate r (shape r seed)) (List.init n Fun.id)
  in
  let nodes = shapes @ long_tail r seed in
  let buf = Buffer.create 4096 in
  List.iter (emit buf) nodes;
  Buffer.contents buf

let stylesheet ~seed =
  match Css.of_string ~strict:false (source ~seed) with
  | Ok { stylesheet; _ } -> stylesheet
  | Error e ->
      failwith
        (String.concat ""
           [
             "gen_sheet: seed ";
             string_of_int seed;
             " does not parse: ";
             Error.to_string e;
           ])
