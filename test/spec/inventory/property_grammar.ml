type row = {
  property : string;
  positives : string list;
  negatives : string list;
}

let matrix =
  [
    {
      property = "display";
      positives =
        [ "block"; "inline"; "inline flow-root"; "list-item flow-root" ];
      negatives = [ "block inline flex"; "unknown-display" ];
    };
    {
      property = "position";
      positives = [ "static"; "relative"; "absolute"; "fixed"; "sticky" ];
      negatives = [ "sticky absolute"; "center" ];
    };
    {
      property = "float";
      positives = [ "left"; "right"; "none"; "inline-start"; "inline-end" ];
      negatives = [ "center"; "left right" ];
    };
    {
      property = "overflow";
      positives = [ "visible"; "hidden"; "clip"; "auto"; "clip auto" ];
      negatives = [ "none"; "visible hidden scroll" ];
    };
    {
      property = "contain";
      positives = [ "none"; "layout paint"; "strict"; "content" ];
      negatives = [ "layout layout"; "strict layout" ];
    };
    {
      property = "container-type";
      positives = [ "normal"; "size"; "inline-size" ];
      negatives = [ "inline-size size"; "block-size" ];
    };
    {
      property = "container";
      positives = [ "card / inline-size"; "inline-size"; "normal" ];
      negatives = [ "/ inline-size"; "card / inline-size / size" ];
    };
    {
      property = "scroll-snap-type";
      positives = [ "none"; "x mandatory"; "block proximity"; "both mandatory" ];
      negatives = [ "mandatory x"; "x y mandatory" ];
    };
    {
      property = "scroll-snap-align";
      positives = [ "none"; "start"; "start end"; "center" ];
      negatives = [ "start center end"; "foo" ];
    };
    {
      property = "scroll-snap-stop";
      positives = [ "normal"; "always" ];
      negatives = [ "normal always"; "sometimes" ];
    };
    {
      property = "box-sizing";
      positives = [ "content-box"; "border-box" ];
      negatives = [ "padding-box"; "border-box content-box" ];
    };
    {
      property = "aspect-ratio";
      positives = [ "auto"; "16/9"; "auto 1/1" ];
      negatives = [ "16 /"; "auto auto" ];
    };
    {
      property = "width";
      positives = [ "auto"; "min-content"; "fit-content(20rem)"; "stretch" ];
      negatives = [ "red"; "fit-content()" ];
    };
    {
      property = "margin";
      positives = [ "0"; "1px 2px 3px 4px"; "auto"; "anchor-size(width)" ];
      negatives = [ "red"; "1px 2px 3px 4px 5px" ];
    };
    {
      property = "padding";
      positives = [ "0"; "1px 2px"; "max(1rem, 2vw)" ];
      negatives = [ "auto"; "1px 2px 3px 4px 5px" ];
    };
    {
      property = "border";
      positives = [ "1px solid red"; "solid"; "0"; "thin currentColor" ];
      negatives = [ "1px 2px"; "solid solid"; "red blue" ];
    };
    {
      property = "border-radius";
      positives = [ "10px"; "10px 20px / 30px 40px" ];
      negatives = [ "10px /"; "10px 20px 30px 40px 50px" ];
    };
    {
      property = "background";
      positives = [ "red"; "url(a.png) no-repeat center / cover"; "none" ];
      negatives = [ "red blue"; "url(" ];
    };
    {
      property = "background-image";
      positives = [ "none"; "url(a.png)"; "linear-gradient(red, blue)" ];
      negatives = [ "linear-gradient()"; "image-set()" ];
    };
    {
      property = "background-size";
      positives = [ "auto"; "cover"; "contain"; "10px 20%" ];
      negatives = [ "cover contain"; "-1px" ];
    };
    {
      property = "clip-path";
      positives =
        [ "none"; "inset(10px)"; "circle(50%)"; "xywh(0 0 100% 100%)" ];
      negatives = [ "circle()"; "inset()" ];
    };
    {
      property = "shape-outside";
      positives = [ "none"; "circle(50%)"; "inset(10px)" ];
      negatives = [ "circle()"; "invalid-shape" ];
    };
    {
      property = "color";
      positives =
        [ "red"; "color(display-p3 1 0 0)"; "light-dark(black, white)" ];
      negatives = [ "not-a-color"; "color(display-p3 1 0)" ];
    };
    {
      property = "opacity";
      positives = [ "0"; ".5"; "1"; "50%" ];
      negatives = [ "red"; "1 2" ];
    };
    {
      property = "filter";
      positives =
        [ "none"; "blur(5px) contrast(120%)"; "drop-shadow(0 0 2px black)" ];
      negatives = [ "blur()"; "drop-shadow()" ];
    };
    {
      property = "font";
      positives = [ "italic small-caps bold 16px/1.5 serif"; "16px serif" ];
      negatives = [ "bold serif"; "16px" ];
    };
    {
      property = "font-family";
      positives = [ "Arial, sans-serif"; "\"A B\", serif"; "system-ui" ];
      negatives = [ "Arial,,serif"; "," ];
    };
    {
      property = "font-weight";
      positives = [ "normal"; "bold"; "400"; "650"; "1000"; "lighter" ];
      negatives = [ "1001"; "0"; "bold 400" ];
    };
    {
      property = "font-feature-settings";
      positives = [ "normal"; "\"kern\" 1"; "\"liga\" off" ];
      negatives = [ "\"kern\" maybe"; "1" ];
    };
    {
      property = "text-decoration";
      positives = [ "underline"; "underline wavy red 2px" ];
      negatives = [ "underline none"; "wavy solid" ];
    };
    {
      property = "white-space";
      positives = [ "normal"; "pre"; "preserve nowrap" ];
      negatives = [ "pre normal"; "wrap nowrap preserve" ];
    };
    {
      property = "word-break";
      positives = [ "normal"; "break-all"; "keep-all"; "break-word" ];
      negatives = [ "break"; "normal keep-all" ];
    };
    {
      property = "writing-mode";
      positives = [ "horizontal-tb"; "vertical-rl"; "sideways-rl" ];
      negatives = [ "vertical"; "vertical-rl horizontal-tb" ];
    };
    {
      property = "transform";
      positives = [ "none"; "translateX(10px) rotate(45deg) scale(1.2)" ];
      negatives = [ "translate()"; "none rotate(1deg)" ];
    };
    {
      property = "translate";
      positives = [ "none"; "10px"; "10px 20px"; "10px 20px 30px" ];
      negatives = [ "10px 20px 30px 40px"; "red" ];
    };
    {
      property = "rotate";
      positives = [ "none"; "45deg"; "1 0 0 45deg" ];
      negatives = [ "1 0 45deg"; "45px" ];
    };
    {
      property = "scale";
      positives = [ "none"; "1.2"; "1.2 2"; "1 2 3" ];
      negatives = [ "1 2 3 4"; "red" ];
    };
    {
      property = "transition";
      positives = [ "opacity 1s ease-in .2s"; "all .2s linear .1s" ];
      negatives = [ "1s 2s 3s"; "ease opacity ease" ];
    };
    {
      property = "transition-behavior";
      positives = [ "normal"; "allow-discrete" ];
      negatives = [ "normal allow-discrete"; "discrete" ];
    };
    {
      property = "animation";
      positives = [ "fade 1s linear 2 alternate both running"; "none" ];
      negatives = [ "1s 2s 3s"; "infinite infinite" ];
    };
    {
      property = "grid-auto-flow";
      positives = [ "row"; "column"; "row dense"; "dense" ];
      negatives = [ "row column"; "dense dense" ];
    };
    {
      property = "gap";
      positives = [ "0"; "1rem"; "1rem 2rem" ];
      negatives = [ "1rem 2rem 3rem"; "red" ];
    };
    {
      property = "flex";
      positives = [ "none"; "auto"; "1"; "1 1 0" ];
      negatives = [ "1 1 1 1"; "row wrap" ];
    };
    {
      property = "place-content";
      positives = [ "center"; "center space-between"; "start stretch" ];
      negatives = [ "center center center"; "left right" ];
    };
    {
      property = "place-items";
      positives = [ "start stretch"; "center"; "normal" ];
      negatives = [ "start center end"; "left right" ];
    };
    {
      property = "list-style";
      positives = [ "square inside"; "none"; "url(marker.png) outside" ];
      negatives = [ "inside outside"; "square disc" ];
    };
    {
      property = "content";
      positives =
        [ "normal"; "\"hello\""; "open-quote attr(title) close-quote" ];
      negatives = [ "attr()"; "open-quote close-quote none" ];
    };
  ]

let rows_for properties positives negatives =
  List.map (fun property -> { property; positives; negatives }) properties

let matrix =
  matrix
  @ rows_for
      [
        "background-color";
        "border-color";
        "border-top-color";
        "border-right-color";
        "border-bottom-color";
        "border-left-color";
        "border-inline-start-color";
        "border-inline-end-color";
        "text-decoration-color";
        "-webkit-text-decoration-color";
        "-webkit-tap-highlight-color";
        "outline-color";
        "fill";
        "stroke";
        "accent-color";
        "caret-color";
      ]
      [ "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
      [ "1px"; "red blue" ]
  @ rows_for
      [
        "border-style";
        "border-top-style";
        "border-right-style";
        "border-bottom-style";
        "border-left-style";
        "border-inline-style";
        "border-block-style";
      ]
      [ "none"; "solid"; "dashed"; "hidden" ]
      [ "solid dashed"; "foo" ]
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
        "border-top-left-radius";
        "border-top-right-radius";
        "border-bottom-left-radius";
        "border-bottom-right-radius";
        "border-start-start-radius";
        "border-start-end-radius";
        "border-end-start-radius";
        "border-end-end-radius";
        "stroke-width";
        "outline-width";
        "outline-offset";
        "text-decoration-thickness";
        "text-underline-offset";
        "letter-spacing";
        "text-indent";
        "word-spacing";
        "line-height-step";
        "overflow-clip-margin";
        "offset-distance";
        "perspective";
        "shape-margin";
      ]
      [ "0"; "1px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
  @ rows_for
      [ "margin-left"; "margin-right"; "margin-top"; "margin-bottom" ]
      [ "0"; "1px"; "10%"; "auto"; "anchor-size(width)" ]
      [ "red"; "1px 2px" ]
  @ rows_for
      [ "padding-inline"; "padding-block"; "column-gap"; "row-gap" ]
      [ "0"; "1rem"; "10%" ]
      [ "auto"; "1px 2px 3px"; "red" ]
  @ rows_for
      [ "margin-inline"; "margin-block"; "scroll-margin-block" ]
      [ "0"; "1px"; "1px 2px"; "auto" ]
      [ "red"; "1px 2px 3px" ]
  @ rows_for
      [
        "margin-inline-start";
        "margin-inline-end";
        "margin-block-start";
        "margin-block-end";
        "scroll-margin";
        "scroll-margin-top";
        "scroll-margin-right";
        "scroll-margin-bottom";
        "scroll-margin-left";
        "scroll-margin-inline";
        "scroll-margin-inline-start";
        "scroll-margin-inline-end";
        "scroll-margin-block-start";
        "scroll-margin-block-end";
        "scroll-padding";
        "scroll-padding-top";
        "scroll-padding-right";
        "scroll-padding-bottom";
        "scroll-padding-left";
        "scroll-padding-inline";
        "scroll-padding-inline-start";
        "scroll-padding-inline-end";
        "scroll-padding-block";
        "scroll-padding-block-start";
        "scroll-padding-block-end";
      ]
      [ "0"; "1px"; "10%" ]
      [ "red"; "1px 2px 3px 4px 5px" ]
  @ rows_for
      [
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
      [ "auto"; "10%"; "min-content"; "fit-content(20rem)" ]
      [ "red"; "fit-content()"; "1px 2px" ]
  @ rows_for
      [
        "border-width";
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
      [ "auto"; "thin medium thick 1px 2px"; "red" ]
  @ rows_for
      [
        "inset";
        "inset-inline";
        "inset-block";
        "top";
        "right";
        "bottom";
        "left";
        "inset-inline-start";
        "inset-inline-end";
        "inset-block-start";
        "inset-block-end";
      ]
      [ "auto"; "1px"; "10%"; "1px 2px 3px 4px" ]
      [ "red"; "1px 2px 3px 4px 5px" ]
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
      [ "none"; "blur(5px)"; "contrast(120%) brightness(.8)" ]
      [ "blur()"; "none blur(1px)" ]
  @ rows_for
      [
        "transition-duration";
        "transition-delay";
        "animation-duration";
        "animation-delay";
      ]
      [ "0s"; ".2s"; "120ms" ] [ "1px"; "1s 2s" ]
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
      [ "repeat"; "no-repeat"; "repeat-x"; "space round" ]
      [ "repeat no-repeat space"; "foo" ]
  @ rows_for
      [
        "background-position";
        "mask-position";
        "-webkit-mask-position";
        "object-position";
      ]
      [ "center"; "left 10px top 20px"; "10% 20%" ]
      [ "left top center"; "foo" ]
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
        negatives = [ "disc square"; "url(marker.png)" ];
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
  @ [
      {
        property = "font-size";
        positives = [ "medium"; "larger"; "12px"; "clamp(1rem, 2vw, 2rem)" ];
        negatives = [ "red"; "12px 14px" ];
      };
      {
        property = "line-height";
        positives = [ "normal"; "1.5"; "12px"; "120%" ];
        negatives = [ "red"; "1 2" ];
      };
      {
        property = "font-style";
        positives = [ "normal"; "italic"; "oblique"; "oblique 20deg" ];
        negatives = [ "italic normal"; "oblique 20px" ];
      };
      {
        property = "font-stretch";
        positives = [ "normal"; "condensed"; "expanded"; "75%" ];
        negatives = [ "75px"; "normal condensed" ];
      };
      {
        property = "font-variation-settings";
        positives = [ "normal"; "\"wght\" 650"; "\"wdth\" 75, \"wght\" 650" ];
        negatives = [ "\"wght\""; "wght 650" ];
      };
      {
        property = "font-variant-numeric";
        positives = [ "normal"; "tabular-nums"; "lining-nums slashed-zero" ];
        negatives = [ "normal tabular-nums"; "tabular-nums tabular-nums" ];
      };
      {
        property = "font-size-adjust";
        positives = [ "none"; "0.5"; "ex-height 0.5" ];
        negatives = [ "auto"; "ex-height" ];
      };
      {
        property = "font-variant-emoji";
        positives = [ "normal"; "text"; "emoji"; "unicode" ];
        negatives = [ "text emoji"; "auto" ];
      };
      {
        property = "text-align";
        positives = [ "start"; "end"; "center"; "match-parent" ];
        negatives = [ "top"; "left right" ];
      };
      {
        property = "text-decoration-line";
        positives = [ "none"; "underline"; "underline overline line-through" ];
        negatives = [ "none underline"; "underline underline" ];
      };
      {
        property = "text-decoration-style";
        positives = [ "solid"; "double"; "dotted"; "wavy" ];
        negatives = [ "solid wavy"; "none" ];
      };
      {
        property = "text-transform";
        positives = [ "none"; "capitalize"; "uppercase"; "full-width" ];
        negatives = [ "uppercase lowercase"; "auto" ];
      };
      {
        property = "text-overflow";
        positives = [ "clip"; "ellipsis"; "\"...\""; "clip ellipsis" ];
        negatives = [ "clip ellipsis clip"; "auto" ];
      };
      {
        property = "text-wrap";
        positives = [ "wrap"; "nowrap"; "balance"; "pretty" ];
        negatives = [ "wrap nowrap"; "auto" ];
      };
      {
        property = "text-wrap-style";
        positives = [ "auto"; "balance"; "pretty"; "stable" ];
        negatives = [ "balance pretty"; "wrap" ];
      };
      {
        property = "text-box-trim";
        positives = [ "none"; "trim-both"; "trim-start"; "trim-end" ];
        negatives = [ "trim-start trim-end"; "auto" ];
      };
      {
        property = "text-spacing-trim";
        positives = [ "normal"; "space-all"; "trim-start"; "space-first" ];
        negatives = [ "normal trim-start"; "auto" ];
      };
      {
        property = "hyphenate-limit-chars";
        positives = [ "auto"; "6"; "6 3"; "6 3 2" ];
        negatives = [ "1 2 3 4"; "red" ];
      };
      {
        property = "initial-letter";
        positives = [ "normal"; "2"; "2 3" ];
        negatives = [ "2 3 4"; "auto" ];
      };
      {
        property = "visibility";
        positives = [ "visible"; "hidden"; "collapse" ];
        negatives = [ "none"; "visible hidden" ];
      };
      {
        property = "flex-direction";
        positives = [ "row"; "row-reverse"; "column"; "column-reverse" ];
        negatives = [ "row column"; "horizontal" ];
      };
      {
        property = "flex-wrap";
        positives = [ "nowrap"; "wrap"; "wrap-reverse" ];
        negatives = [ "wrap nowrap"; "reverse" ];
      };
      {
        property = "flex-grow";
        positives = [ "0"; "1"; "2.5" ];
        negatives = [ "-1"; "1 2" ];
      };
      {
        property = "flex-shrink";
        positives = [ "0"; "1"; "2.5" ];
        negatives = [ "-1"; "1 2" ];
      };
      {
        property = "order";
        positives = [ "0"; "-1"; "calc(1 + 2)" ];
        negatives = [ "1.5"; "red" ];
      };
      {
        property = "align-items";
        positives = [ "normal"; "stretch"; "first baseline"; "safe center" ];
        negatives = [ "left"; "safe unsafe center" ];
      };
      {
        property = "align-self";
        positives = [ "auto"; "normal"; "stretch"; "unsafe flex-end" ];
        negatives = [ "left"; "auto center" ];
      };
      {
        property = "align-content";
        positives = [ "normal"; "space-between"; "safe center"; "baseline" ];
        negatives = [ "auto"; "space-between stretch center" ];
      };
      {
        property = "justify-content";
        positives = [ "normal"; "space-evenly"; "unsafe right"; "stretch" ];
        negatives = [ "auto"; "left right" ];
      };
      {
        property = "justify-items";
        positives = [ "normal"; "stretch"; "legacy left"; "safe center" ];
        negatives = [ "space-between"; "legacy safe left" ];
      };
      {
        property = "justify-self";
        positives = [ "auto"; "normal"; "stretch"; "safe end" ];
        negatives = [ "space-between"; "auto end" ];
      };
      {
        property = "place-self";
        positives = [ "auto"; "center"; "start end" ];
        negatives = [ "start center end"; "left right" ];
      };
      {
        property = "grid-template-columns";
        positives = [ "none"; "subgrid"; "repeat(3, 1fr)"; "minmax(0, 1fr)" ];
        negatives = [ "repeat()"; "subgrid none" ];
      };
      {
        property = "grid-template-rows";
        positives = [ "none"; "subgrid"; "masonry"; "100px 1fr" ];
        negatives = [ "masonry subgrid"; "repeat()" ];
      };
      {
        property = "grid-template-areas";
        positives = [ "none"; "\"a a\" \"b c\"" ];
        negatives = [ "\"a\" \"a a\""; "a b" ];
      };
      {
        property = "grid-template";
        positives = [ "none"; "\"a a\" 1fr / 1fr 1fr"; "subgrid / subgrid" ];
        negatives = [ "/"; "none / 1fr" ];
      };
      {
        property = "grid-area";
        positives = [ "auto"; "header"; "1 / 2 / span 3 / 4" ];
        negatives = [ "1 / 2 / 3 / 4 / 5"; "/" ];
      };
      {
        property = "grid-auto-columns";
        positives = [ "auto"; "minmax(0, 1fr)"; "100px" ];
        negatives = [ "subgrid"; "repeat()" ];
      };
      {
        property = "grid-auto-rows";
        positives = [ "auto"; "minmax(0, 1fr)"; "100px" ];
        negatives = [ "subgrid"; "repeat()" ];
      };
      {
        property = "grid-column";
        positives = [ "auto"; "1 / span 2"; "header-start / header-end" ];
        negatives = [ "1 / 2 / 3"; "/" ];
      };
      {
        property = "grid-row";
        positives = [ "auto"; "1 / span 2"; "row-start / row-end" ];
        negatives = [ "1 / 2 / 3"; "/" ];
      };
      {
        property = "grid-column-start";
        positives = [ "auto"; "1"; "span 2"; "header-start" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "grid-column-end";
        positives = [ "auto"; "1"; "span 2"; "header-end" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "grid-row-start";
        positives = [ "auto"; "1"; "span 2"; "row-start" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "grid-row-end";
        positives = [ "auto"; "1"; "span 2"; "row-end" ];
        negatives = [ "span"; "1 2" ];
      };
      {
        property = "box-shadow";
        positives = [ "none"; "0 1px 2px rgb(0 0 0 / .2)"; "inset 0 0 1px red" ];
        negatives = [ "0 0"; "inset inset 0 0 1px" ];
      };
      {
        property = "mix-blend-mode";
        positives = [ "normal"; "multiply"; "screen"; "plus-lighter" ];
        negatives = [ "normal multiply"; "foo" ];
      };
      {
        property = "cursor";
        positives = [ "auto"; "pointer"; "url(cursor.png), pointer" ];
        negatives = [ "url(cursor.png)"; "pointer auto" ];
      };
      {
        property = "table-layout";
        positives = [ "auto"; "fixed" ];
        negatives = [ "fixed auto"; "block" ];
      };
      {
        property = "border-collapse";
        positives = [ "collapse"; "separate" ];
        negatives = [ "collapse separate"; "none" ];
      };
      {
        property = "border-spacing";
        positives = [ "0"; "1px 2px" ];
        negatives = [ "1px 2px 3px"; "auto" ];
      };
      {
        property = "user-select";
        positives = [ "auto"; "text"; "none"; "all" ];
        negatives = [ "text none"; "contain" ];
      };
      {
        property = "-webkit-user-select";
        positives = [ "auto"; "text"; "none"; "all" ];
        negatives = [ "text none"; "contain" ];
      };
      {
        property = "pointer-events";
        positives = [ "auto"; "none"; "visiblePainted"; "all" ];
        negatives = [ "auto none"; "visible-painted" ];
      };
      {
        property = "z-index";
        positives = [ "auto"; "0"; "-1"; "calc(1 + 2)" ];
        negatives = [ "1.5"; "0 1" ];
      };
      {
        property = "outline";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "outline-style";
        positives = [ "none"; "auto"; "solid"; "dotted" ];
        negatives = [ "solid dotted"; "foo" ];
      };
      {
        property = "forced-color-adjust";
        positives = [ "auto"; "none"; "preserve-parent-color" ];
        negatives = [ "auto none"; "preserve" ];
      };
      {
        property = "clip";
        positives = [ "auto"; "rect(0, 10px, 10px, 0)" ];
        negatives = [ "rect(0, 1px)"; "inset(1px)" ];
      };
      {
        property = "clear";
        positives = [ "none"; "left"; "right"; "both"; "inline-start" ];
        negatives = [ "left right"; "center" ];
      };
      {
        property = "tab-size";
        positives = [ "4"; "8"; "2ch" ];
        negatives = [ "-1"; "4 8" ];
      };
      {
        property = "-webkit-text-size-adjust";
        positives = [ "auto"; "none"; "100%" ];
        negatives = [ "auto none"; "1px" ];
      };
      {
        property = "text-size-adjust";
        positives = [ "auto"; "none"; "100%" ];
        negatives = [ "auto none"; "1px" ];
      };
      {
        property = "-webkit-text-decoration";
        positives = [ "underline"; "underline wavy red 2px" ];
        negatives = [ "underline none"; "wavy solid" ];
      };
      {
        property = "-webkit-appearance";
        positives = [ "none"; "auto"; "button"; "textfield" ];
        negatives = [ "none auto"; "foo" ];
      };
      {
        property = "-moz-appearance";
        positives = [ "none"; "auto"; "button"; "textfield" ];
        negatives = [ "none auto"; "foo" ];
      };
      {
        property = "appearance";
        positives = [ "none"; "auto"; "base-select"; "textfield" ];
        negatives = [ "none auto"; "foo" ];
      };
      {
        property = "container-name";
        positives = [ "none"; "card"; "card layout" ];
        negatives = [ "default"; "card / layout" ];
      };
      {
        property = "anchor-name";
        positives = [ "none"; "--anchor"; "--a, --b" ];
        negatives = [ "anchor"; "--a --b" ];
      };
      {
        property = "position-anchor";
        positives = [ "auto"; "--anchor" ];
        negatives = [ "anchor"; "--a --b" ];
      };
      {
        property = "position-try-fallbacks";
        positives = [ "none"; "--fallback"; "flip-block, --fallback" ];
        negatives = [ "flip-block --fallback"; "," ];
      };
      {
        property = "overflow-anchor";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "hidden" ];
      };
      {
        property = "scrollbar-width";
        positives = [ "auto"; "thin"; "none" ];
        negatives = [ "thin auto"; "1px" ];
      };
      {
        property = "scrollbar-color";
        positives = [ "auto"; "red blue" ];
        negatives = [ "red"; "red blue green" ];
      };
      {
        property = "scrollbar-gutter";
        positives = [ "auto"; "stable"; "stable both-edges" ];
        negatives = [ "both-edges"; "stable stable" ];
      };
      {
        property = "font-palette";
        positives = [ "normal"; "light"; "dark"; "--brand" ];
        negatives = [ "normal light"; "brand" ];
      };
      {
        property = "font-synthesis";
        positives = [ "none"; "weight"; "style small-caps position" ];
        negatives = [ "none weight"; "weight weight" ];
      };
      {
        property = "animation-timeline";
        positives = [ "auto"; "none"; "scroll()"; "--timeline" ];
        negatives = [ "auto none"; "scroll(" ];
      };
      {
        property = "animation-range";
        positives = [ "normal"; "entry 10% exit 90%"; "cover 0% 100%" ];
        negatives = [ "entry exit cover"; "10% 20% 30%" ];
      };
      {
        property = "view-transition-name";
        positives = [ "none"; "card"; "match-element" ];
        negatives = [ "card card"; "auto" ];
      };
      {
        property = "image-orientation";
        positives = [ "none"; "from-image" ];
        negatives = [ "90deg"; "from-image none" ];
      };
      {
        property = "contain-intrinsic-size";
        positives = [ "none"; "auto 300px"; "100px 200px" ];
        negatives = [ "auto"; "1px 2px 3px" ];
      };
      {
        property = "margin-trim";
        positives = [ "none"; "block"; "inline"; "block-start block-end" ];
        negatives = [ "none block"; "block block" ];
      };
      {
        property = "mask-mode";
        positives = [ "match-source"; "alpha"; "luminance"; "alpha, luminance" ];
        negatives = [ "match-source alpha luminance"; "foo" ];
      };
      {
        property = "offset-path";
        positives =
          [ "none"; "path(\"M 0 0 L 1 1\")"; "ray(45deg closest-side)" ];
        negatives = [ "path()"; "ray()" ];
      };
      {
        property = "view-timeline-name";
        positives = [ "none"; "--timeline"; "--a, --b" ];
        negatives = [ "timeline"; "--a --b" ];
      };
      {
        property = "view-timeline-axis";
        positives = [ "block"; "inline"; "x"; "y" ];
        negatives = [ "block inline"; "z" ];
      };
      {
        property = "timeline-scope";
        positives = [ "none"; "--timeline"; "--a, --b" ];
        negatives = [ "timeline"; "--a --b" ];
      };
      {
        property = "perspective-origin";
        positives = [ "center"; "left top"; "10px 20%" ];
        negatives = [ "left right"; "top bottom" ];
      };
      {
        property = "transform-style";
        positives = [ "flat"; "preserve-3d" ];
        negatives = [ "flat preserve-3d"; "none" ];
      };
      {
        property = "backface-visibility";
        positives = [ "visible"; "hidden" ];
        negatives = [ "visible hidden"; "none" ];
      };
      {
        property = "transition-property";
        positives = [ "none"; "all"; "opacity, transform" ];
        negatives = [ "none, opacity"; "all, opacity" ];
      };
      {
        property = "will-change";
        positives =
          [ "auto"; "scroll-position"; "contents"; "opacity, transform" ];
        negatives = [ "auto, opacity"; "will-change" ];
      };
      {
        property = "isolation";
        positives = [ "auto"; "isolate" ];
        negatives = [ "auto isolate"; "none" ];
      };
      {
        property = "break-before";
        positives = [ "auto"; "avoid"; "page"; "recto" ];
        negatives = [ "avoid page"; "none" ];
      };
      {
        property = "break-after";
        positives = [ "auto"; "avoid"; "page"; "verso" ];
        negatives = [ "avoid page"; "none" ];
      };
      {
        property = "break-inside";
        positives = [ "auto"; "avoid"; "avoid-page"; "avoid-column" ];
        negatives = [ "avoid page"; "none" ];
      };
      {
        property = "columns";
        positives = [ "auto"; "12em"; "3"; "12em 3" ];
        negatives = [ "3 4"; "red" ];
      };
      {
        property = "background-attachment";
        positives = [ "scroll"; "fixed"; "local"; "scroll, fixed" ];
        negatives = [ "scroll fixed"; "none" ];
      };
      {
        property = "border-top";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-right";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-bottom";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "border-left";
        positives = [ "1px solid red"; "solid"; "0" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "transform-origin";
        positives = [ "center"; "left top"; "left 10px top 20px"; "10px 20px" ];
        negatives = [ "left right"; "top bottom" ];
      };
      {
        property = "transform-box";
        positives = [ "content-box"; "border-box"; "fill-box"; "view-box" ];
        negatives = [ "margin-box"; "content-box border-box" ];
      };
      {
        property = "text-shadow";
        positives = [ "none"; "1px 1px black"; "0 1px 2px red, 0 0 1px blue" ];
        negatives = [ "1px"; "red blue" ];
      };
      {
        property = "mask";
        positives = [ "none"; "url(mask.png) no-repeat center / contain" ];
        negatives = [ "url("; "red blue" ];
      };
      {
        property = "content-visibility";
        positives = [ "visible"; "auto"; "hidden" ];
        negatives = [ "visible hidden"; "none" ];
      };
      {
        property = "animation-name";
        positives = [ "none"; "fade"; "fade, slide" ];
        negatives = [ "initial fade"; "," ];
      };
      {
        property = "animation-iteration-count";
        positives = [ "infinite"; "1"; "2.5"; "1, infinite" ];
        negatives = [ "-1"; "infinite infinite" ];
      };
      {
        property = "animation-direction";
        positives = [ "normal"; "reverse"; "alternate"; "alternate-reverse" ];
        negatives = [ "normal reverse"; "forwards" ];
      };
      {
        property = "animation-fill-mode";
        positives = [ "none"; "forwards"; "backwards"; "both" ];
        negatives = [ "none forwards"; "running" ];
      };
      {
        property = "animation-play-state";
        positives = [ "running"; "paused"; "running, paused" ];
        negatives = [ "running paused"; "none" ];
      };
      {
        property = "background-blend-mode";
        positives = [ "normal"; "multiply"; "screen, overlay" ];
        negatives = [ "normal multiply"; "foo" ];
      };
      {
        property = "vertical-align";
        positives = [ "baseline"; "sub"; "text-top"; "10%" ];
        negatives = [ "baseline sub"; "red" ];
      };
      {
        property = "-webkit-font-smoothing";
        positives = [ "auto"; "none"; "antialiased"; "subpixel-antialiased" ];
        negatives = [ "auto none"; "smooth" ];
      };
      {
        property = "-moz-osx-font-smoothing";
        positives = [ "auto"; "grayscale" ];
        negatives = [ "auto grayscale"; "antialiased" ];
      };
      {
        property = "-webkit-line-clamp";
        positives = [ "none"; "3" ];
        negatives = [ "0"; "3 4" ];
      };
      {
        property = "-webkit-box-orient";
        positives = [ "horizontal"; "vertical"; "inline-axis"; "block-axis" ];
        negatives = [ "horizontal vertical"; "row" ];
      };
      {
        property = "overflow-wrap";
        positives = [ "normal"; "break-word"; "anywhere" ];
        negatives = [ "normal anywhere"; "break-all" ];
      };
      {
        property = "hyphens";
        positives = [ "none"; "manual"; "auto" ];
        negatives = [ "manual auto"; "normal" ];
      };
      {
        property = "-webkit-hyphens";
        positives = [ "none"; "manual"; "auto" ];
        negatives = [ "manual auto"; "normal" ];
      };
      {
        property = "-webkit-mask-composite";
        positives = [ "source-over"; "xor"; "source-in, source-out" ];
        negatives = [ "add"; "source-over xor" ];
      };
      {
        property = "-webkit-mask-source-type";
        positives = [ "auto"; "alpha"; "luminance" ];
        negatives = [ "alpha luminance"; "match-source" ];
      };
      {
        property = "-webkit-mask-clip";
        positives = [ "border-box"; "padding-box"; "content-box"; "no-clip" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "-webkit-mask-origin";
        positives = [ "border-box"; "padding-box"; "content-box" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "mask-composite";
        positives = [ "add"; "subtract"; "intersect"; "exclude" ];
        negatives = [ "source-over"; "add subtract" ];
      };
      {
        property = "mask-clip";
        positives = [ "border-box"; "padding-box"; "content-box"; "no-clip" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "mask-origin";
        positives = [ "border-box"; "padding-box"; "content-box" ];
        negatives =
          [ "margin-box"; "border-box padding-box content-box content-box" ];
      };
      {
        property = "mask-type";
        positives = [ "alpha"; "luminance" ];
        negatives = [ "match-source"; "alpha luminance" ];
      };
      {
        property = "scroll-behavior";
        positives = [ "auto"; "smooth" ];
        negatives = [ "auto smooth"; "none" ];
      };
      {
        property = "field-sizing";
        positives = [ "fixed"; "content" ];
        negatives = [ "auto"; "fixed content" ];
      };
      {
        property = "caption-side";
        positives = [ "top"; "bottom" ];
        negatives = [ "left"; "top bottom" ];
      };
      {
        property = "resize";
        positives = [ "none"; "both"; "horizontal"; "block" ];
        negatives = [ "horizontal vertical"; "auto" ];
      };
      {
        property = "object-fit";
        positives = [ "fill"; "contain"; "cover"; "scale-down" ];
        negatives = [ "contain cover"; "auto" ];
      };
      {
        property = "color-scheme";
        positives = [ "normal"; "light"; "dark"; "only light" ];
        negatives = [ "normal light"; "only" ];
      };
      {
        property = "print-color-adjust";
        positives = [ "economy"; "exact" ];
        negatives = [ "economy exact"; "auto" ];
      };
      {
        property = "box-decoration-break";
        positives = [ "slice"; "clone" ];
        negatives = [ "slice clone"; "auto" ];
      };
      {
        property = "-webkit-box-decoration-break";
        positives = [ "slice"; "clone" ];
        negatives = [ "slice clone"; "auto" ];
      };
      {
        property = "quotes";
        positives = [ "auto"; "none"; "\"<\" \">\" \"'\" \"'\"" ];
        negatives = [ "\"<\""; "auto none" ];
      };
      {
        property = "touch-action";
        positives = [ "auto"; "none"; "pan-x pinch-zoom"; "manipulation" ];
        negatives = [ "auto none"; "pan-x pan-left" ];
      };
      {
        property = "direction";
        positives = [ "ltr"; "rtl" ];
        negatives = [ "ltr rtl"; "auto" ];
      };
      {
        property = "unicode-bidi";
        positives = [ "normal"; "embed"; "isolate"; "plaintext" ];
        negatives = [ "normal isolate"; "auto" ];
      };
      {
        property = "text-decoration-skip-ink";
        positives = [ "auto"; "none"; "all" ];
        negatives = [ "auto none"; "skip" ];
      };
      {
        property = "overscroll-behavior";
        positives = [ "auto"; "contain"; "none"; "contain none" ];
        negatives = [ "contain auto none"; "hidden" ];
      };
      {
        property = "overscroll-behavior-x";
        positives = [ "auto"; "contain"; "none" ];
        negatives = [ "contain none"; "hidden" ];
      };
      {
        property = "overscroll-behavior-y";
        positives = [ "auto"; "contain"; "none" ];
        negatives = [ "contain none"; "hidden" ];
      };
      {
        property = "-webkit-transform";
        positives = [ "none"; "translateX(10px) rotate(45deg)" ];
        negatives = [ "translate()"; "none rotate(1deg)" ];
      };
      {
        property = "-webkit-transition";
        positives = [ "opacity 1s ease-in .2s"; "all .2s linear .1s" ];
        negatives = [ "1s 2s 3s"; "ease opacity ease" ];
      };
      {
        property = "-o-transition";
        positives = [ "opacity 1s ease-in .2s"; "all .2s linear .1s" ];
        negatives = [ "1s 2s 3s"; "ease opacity ease" ];
      };
    ]

let rows = matrix

let property_names =
  rows |> List.map (fun row -> row.property) |> List.sort_uniq String.compare
