type row = {
  property : string;
  positives : string list;
  negatives : string list;
}

let matrix =
  [
    {
      property = "all";
      positives = [ "initial"; "revert-layer" ];
      negatives = [ "auto"; "none" ];
    };
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
      positives =
        [ "visible"; "hidden"; "clip"; "auto"; "clip auto"; "clip visible" ];
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
      positives = [ "none"; "start"; "start end"; "center"; "none start" ];
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
      positives =
        [
          "auto";
          "min-content";
          "fit-content(20rem)";
          "stretch";
          "calc(1rem + 2px)";
          "attr(data-w px, calc(10px + 0px))";
        ];
      negatives = [ "red"; "fit-content()" ];
    };
    {
      property = "margin";
      positives =
        [
          "0";
          "1px 2px 3px 4px";
          "auto";
          "anchor-size(width)";
          "calc(1rem + 2px)";
        ];
      negatives = [ "red"; "1px 2px 3px 4px 5px" ];
    };
    {
      property = "padding";
      positives = [ "0"; "1px 2px"; "max(1rem, 2vw)" ];
      negatives = [ "auto"; "-1px"; "1px 2px 3px 4px 5px" ];
    };
    {
      property = "border";
      positives = [ "1px solid red"; "solid"; "0"; "thin currentColor" ];
      negatives = [ "1px 2px"; "solid solid"; "red blue" ];
    };
    {
      property = "border-radius";
      positives = [ "10px"; "10px 20px / 30px 40px"; "calc(1rem + 2px)" ];
      negatives = [ "-1px"; "10px /"; "10px 20px 30px 40px 50px" ];
    };
    {
      property = "background";
      positives =
        [
          "red";
          "url(a.png) no-repeat center / cover";
          "none";
          "conic-gradient(from 45deg, red, blue)";
          "cross-fade(url(a.png) 40%, url(b.png))";
        ];
      negatives = [ "red blue"; "red red" ];
    };
    {
      property = "background-image";
      positives =
        [
          "none";
          "url(a.png)";
          "linear-gradient(red, blue)";
          "linear-gradient(in oklab, red, blue)";
          "linear-gradient(to right in oklab, red, blue)";
          "linear-gradient(in oklab to right, red, blue)";
          "radial-gradient(in oklab, red, blue)";
          "radial-gradient(in oklab circle at center, red, blue)";
          "radial-gradient(circle at center in oklab, red, blue)";
          "conic-gradient(in hsl longer hue, red, blue)";
          "conic-gradient(in hsl longer hue from 45deg at center, red, blue)";
          "conic-gradient(from 45deg at center in hsl longer hue, red, blue)";
          "image-set(url(a.avif) type(\"image/avif\") 1x, url(a.png) \
           type(\"image/png\") 1x)";
          "cross-fade(url(a.png) 40%, url(b.png))";
        ];
      negatives =
        [ "linear-gradient()"; "image-set()"; "cross-fade(url(a.png), )" ];
    };
    {
      property = "background-size";
      positives = [ "auto"; "cover"; "contain"; "10px 20%" ];
      negatives = [ "cover contain"; "-1px" ];
    };
    {
      property = "clip-path";
      positives =
        [
          "none";
          "inset(10px)";
          "circle(50%)";
          "circle()";
          "xywh(0 0 100% 100%)";
        ];
      (* Shapes 1: [circle()]'s args are both optional; [inset()] needs 1-4
         args; [polygon()] needs >= 1 vertex. *)
      negatives = [ "inset()"; "polygon()" ];
    };
    {
      property = "shape-outside";
      positives = [ "none"; "circle(50%)"; "circle()"; "inset(10px)" ];
      negatives = [ "invalid-shape"; "polygon()" ];
    };
    {
      property = "color";
      positives =
        [
          "red";
          "color(display-p3 1 0 0)";
          "light-dark(black, white)";
          "hwb(90 10% 20%)";
          "lab(50% 10 20 / .5)";
          "lch(50% 20 30)";
          "oklab(50% 0.1 0.2)";
          "oklch(50% 0.1 20 / .5)";
          "color-mix(in lch longer hue, red 30%, blue)";
          "color-mix(var(--a), var(--b))";
          "attr(data-color type(<color>), var(--fallback-color, red))";
        ];
      negatives =
        [
          "not-a-color";
          "color(display-p3 1 0)";
          "hwb(0 0%)";
          "lab(50% 10)";
          "oklch(50% .1 20 /)";
          "color-mix(in bogus, red, blue)";
        ];
    };
    {
      property = "opacity";
      (* [calc(.5)] is the calc-family vector; it canonicalises to [.5] under
         shortest-wins. *)
      positives = [ "0"; ".5"; "1"; "50%"; "calc(.5)" ];
      negatives = [ "red"; "1 2" ];
    };
    {
      property = "filter";
      positives =
        [
          "none";
          "blur(5px) contrast(120%)";
          "drop-shadow(0 0 2px black)";
          "opacity(calc(50% + 25%))";
        ];
      negatives = [ "blur()"; "drop-shadow()" ];
    };
    {
      property = "font";
      positives = [ "italic small-caps bold 16px/1.5 serif"; "16px serif" ];
      negatives = [ "bold serif"; "16px" ];
    };
    {
      property = "font-family";
      positives =
        [
          "Arial, sans-serif";
          "\"A B\", serif";
          "\"default\"";
          "\"revert\", serif";
          "system-ui";
        ];
      negatives =
        [
          "Arial,,serif";
          ",";
          "default";
          "system-ui default";
          "revert-layer, serif";
          "system-ui revert-layer, serif";
        ];
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
      property = "text-decoration-thickness";
      positives =
        [ "auto"; "from-font"; "0"; "1px"; "10%"; "hairline"; "thin"; "thick" ];
      negatives = [ "red"; "1px 2px"; "-1px"; "none"; "min-content"; "stretch" ];
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
      positives =
        [ "none"; "10px"; "10px 20px"; "10px 20px 30px"; "calc(1rem + 2px)" ];
      negatives = [ "10px 20px 30px 40px"; "red" ];
    };
    {
      property = "rotate";
      positives = [ "none"; "45deg"; "1 0 0 45deg"; "calc(30deg + 15deg)" ];
      negatives = [ "1 0 45deg"; "45px" ];
    };
    {
      property = "scale";
      positives = [ "none"; "1.2"; "1.2 2"; "1 2 3"; "calc(50% + 25%)" ];
      negatives = [ "1 2 3 4"; "red" ];
    };
    {
      property = "transition";
      positives =
        [
          "opacity 1s ease-in .2s";
          "all .2s linear .1s";
          "opacity calc(500ms + .5s)";
        ];
      negatives = [ "1s 2s 3s"; "ease opacity ease" ];
    };
    {
      property = "transition-behavior";
      positives = [ "normal"; "allow-discrete" ];
      negatives = [ "normal allow-discrete"; "discrete" ];
    };
    {
      property = "animation";
      positives =
        [
          "fade 1s linear 2 alternate both running"; "none"; "fade calc(1s * 2)";
        ];
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
      negatives = [ "-1px"; "1rem 2rem 3rem"; "red" ];
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
      property = "counter-reset";
      positives = [ "none"; "section"; "section 2"; "section 2 page -1" ];
      negatives = [ "none section"; "section 1.5"; "section none" ];
    };
    {
      property = "counter-increment";
      positives = [ "none"; "section"; "section 2"; "section 2 page -1" ];
      negatives = [ "none section"; "section 1.5"; "section none" ];
    };
    {
      property = "content";
      positives =
        [
          "normal";
          "\"hello\"";
          "open-quote attr(title) close-quote";
          "attr(data-label)";
          "attr(data-label string, \"x y\")";
          "attr(data-label string, var(--label, \"x y\"))";
          "counter(section)";
          "counters(section, \".\")";
        ];
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
        "outline-color";
        "fill";
        "stroke";
        "accent-color";
        "caret-color";
        "stop-color";
        "flood-color";
        "lighting-color";
      ]
      [ "red"; "currentColor"; "rgb(0 0 0 / 50%)" ]
      [ "1px"; "red blue" ]
  @ [
      {
        property = "border-color";
        positives = [ "red"; "red blue"; "red blue green"; "currentColor" ];
        negatives = [ "1px"; "red blue green black white" ];
      };
    ]
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
  (* CSS Backgrounds 3 (ED) sec. 3.2 gives [border-style] the value
     [<line-style>{1,4}], the box over the four side styles that sec. 3.1 gives
     [border-color]. *)
  @ [
      {
        property = "border-style";
        positives =
          [
            "none";
            "solid";
            "solid dashed";
            "solid dashed dotted";
            "solid dashed dotted double";
          ];
        negatives = [ "foo"; "solid dashed dotted double hidden" ];
      };
    ]
  (* CSS Logical 1 (ED) sec. 4.5.2 gives both flow-relative shorthands the value
     [<'border-top-style'>{1,2}], the start edge then the end edge. *)
  @ rows_for
      [ "border-inline-style"; "border-block-style" ]
      [ "none"; "solid"; "solid dashed" ]
      [ "foo"; "solid dashed dotted" ]
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
        "stroke-width";
        "outline-width";
        "outline-offset";
        "letter-spacing";
        "text-indent";
        "word-spacing";
        "line-height-step";
        "overflow-clip-margin";
        "offset-distance";
        "shape-margin";
      ]
      [ "0"; "1px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px" ]
  (* CSS Backgrounds 3 (ED) sec. 4.1 gives every corner longhand the value
     [<length-percentage [0,inf]>{1,2}]: the horizontal radius then the vertical
     one, a single value setting both. *)
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
      [ "0"; "1px"; "1px 2px"; "calc(1rem + 2px)" ]
      [ "auto"; "red"; "1px 2px 3px" ]
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
  @ rows_for
      [ "padding-inline"; "padding-block"; "column-gap"; "row-gap" ]
      [ "0"; "1rem"; "10%" ]
      [ "auto"; "1px 2px 3px"; "red" ]
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
      [ "auto"; "10%"; "min-content"; "fit-content(20rem)"; "calc(1rem + 2px)" ]
      [ "red"; "fit-content()"; "1px 2px" ]
  @ rows_for
      [
        "contain-intrinsic-width";
        "contain-intrinsic-height";
        "contain-intrinsic-inline-size";
        "contain-intrinsic-block-size";
      ]
      [ "none"; "100px"; "auto 300px" ]
      [ "auto"; "1px 2px"; "red" ]
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
      [
        "repeat"; "no-repeat"; "repeat-x"; "space round"; "no-repeat no-repeat";
      ]
      [ "repeat no-repeat space"; "foo" ]
  @ rows_for
      [
        "background-position";
        "mask-position";
        "-webkit-mask-position";
        "object-position";
      ]
      [ "center"; "left 10px top 20px"; "10% 20%"; "calc(10% + 1px) 20%" ]
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
        negatives = [ "red"; "-1px"; "12px 14px" ];
      };
      {
        property = "line-height";
        positives = [ "normal"; "1.5"; "12px"; "120%"; "calc(1em + 2px)" ];
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
        positives =
          [
            "normal";
            "tabular-nums";
            "lining-nums slashed-zero";
            "oldstyle-nums tabular-nums stacked-fractions ordinal slashed-zero";
          ];
        negatives =
          [
            "normal tabular-nums";
            "tabular-nums tabular-nums";
            "lining-nums oldstyle-nums";
            "proportional-nums tabular-nums";
            "diagonal-fractions stacked-fractions";
          ];
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
        negatives =
          [
            "none underline"; "underline underline"; "spelling-error underline";
          ];
      };
      {
        property = "text-decoration-style";
        positives = [ "solid"; "double"; "dotted"; "wavy" ];
        negatives = [ "solid wavy"; "none" ];
      };
      {
        property = "text-underline-position";
        positives = [ "auto"; "from-font"; "under left"; "right under" ];
        negatives = [ "auto under"; "left right" ];
      };
      {
        property = "text-decoration-skip";
        positives = [ "none"; "auto" ];
        negatives = [ "none auto"; "objects" ];
      };
      {
        property = "text-decoration-skip-self";
        positives = [ "none"; "objects" ];
        negatives = [ "auto"; "none objects" ];
      };
      {
        property = "text-decoration-skip-box";
        positives = [ "all"; "none" ];
        negatives = [ "auto"; "all none" ];
      };
      {
        property = "text-decoration-skip-inset";
        positives = [ "none"; "auto" ];
        negatives = [ "1px"; "none auto" ];
      };
      {
        property = "text-decoration-skip-spaces";
        positives = [ "all"; "start"; "end"; "start end" ];
        negatives = [ "none"; "start start" ];
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
        property = "text-emphasis";
        positives = [ "none"; "filled dot red"; "\"*\" blue"; "sesame" ];
        negatives = [ "filled open"; "dot circle"; "red blue" ];
      };
      {
        property = "text-emphasis-style";
        positives = [ "none"; "filled dot"; "open sesame"; "\"*\"" ];
        negatives = [ "filled open"; "dot circle" ];
      };
      {
        property = "text-emphasis-color";
        positives = [ "currentColor"; "red"; "rgb(0 0 0 / 50%)" ];
        negatives = [ "1px"; "red blue" ];
      };
      {
        property = "text-emphasis-position";
        positives = [ "over right"; "under left"; "over" ];
        negatives = [ "over under"; "left right" ];
      };
      {
        property = "text-emphasis-skip";
        positives =
          [ "spaces"; "punctuation symbols"; "spaces punctuation narrow" ];
        negatives = [ "spaces spaces"; "none" ];
      };
      {
        property = "text-orientation";
        positives = [ "mixed"; "upright"; "sideways" ];
        negatives = [ "mixed upright"; "sideways-right" ];
      };
      {
        property = "text-combine-upright";
        positives = [ "none"; "all"; "digits"; "digits 2"; "digits 4" ];
        negatives = [ "digits 1"; "digits 5"; "all digits"; "digits 2 3" ];
      };
      {
        property = "glyph-orientation-vertical";
        positives = [ "auto"; "0deg"; "90deg"; "0"; "90" ];
        negatives = [ "45deg"; "180"; "auto 90deg" ];
      };
      {
        property = "line-break";
        positives = [ "auto"; "loose"; "normal"; "strict"; "anywhere" ];
        negatives = [ "anywhere strict"; "break-word" ];
      };
      {
        property = "text-box";
        positives = [ "none"; "trim-both cap alphabetic"; "trim-start text" ];
        negatives = [ "cap trim-both"; "trim-start cap alphabetic text" ];
      };
      {
        property = "text-box-edge";
        positives = [ "auto"; "text"; "cap alphabetic"; "ex text" ];
        negatives = [ "cap"; "alphabetic cap"; "cap ex" ];
      };
      {
        property = "line-fit-edge";
        positives = [ "leading"; "text"; "cap alphabetic"; "ideographic-ink" ];
        negatives = [ "leading text"; "cap"; "alphabetic cap" ];
      };
      {
        property = "inline-sizing";
        positives = [ "normal"; "stretch" ];
        negatives = [ "normal stretch"; "auto" ];
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
        property = "initial-letter-align";
        positives =
          [ "alphabetic"; "ideographic"; "border-box"; "border-box hanging" ];
        negatives = [ "alphabetic ideographic"; "border-box border-box" ];
      };
      {
        property = "initial-letter-wrap";
        positives = [ "none"; "first"; "all"; "grid"; "10%" ];
        negatives = [ "none first"; "auto"; "1px 2px" ];
      };
      {
        property = "dominant-baseline";
        positives = [ "auto"; "alphabetic"; "ideographic"; "mathematical" ];
        negatives = [ "alphabetic ideographic"; "baseline" ];
      };
      {
        property = "baseline-source";
        positives = [ "auto"; "first"; "last" ];
        negatives = [ "first last"; "baseline" ];
      };
      {
        property = "alignment-baseline";
        positives = [ "baseline"; "text-bottom"; "middle"; "central" ];
        negatives = [ "baseline middle"; "auto" ];
      };
      {
        property = "baseline-shift";
        positives = [ "0"; "10%"; "sub"; "super"; "top"; "center"; "bottom" ];
        negatives = [ "sub super"; "auto" ];
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
        positives = [ "none"; "\"a a\" \"b c\""; "\"nav  main\" \".    foot\"" ];
        negatives =
          [ "\"a\" \"a a\""; "\"a .\" \". a\""; "\"nav/main\""; "a b" ];
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
        property = "caret";
        positives = [ "auto"; "red"; "manual"; "block"; "red manual block" ];
        negatives = [ "manual manual"; "block underscore"; "red blue" ];
      };
      {
        property = "caret-animation";
        positives = [ "auto"; "manual" ];
        negatives = [ "manual auto"; "blink" ];
      };
      {
        property = "caret-shape";
        positives = [ "auto"; "bar"; "block"; "underscore" ];
        negatives = [ "bar block"; "line" ];
      };
      {
        property = "interactivity";
        positives = [ "auto"; "inert" ];
        negatives = [ "none"; "auto inert" ];
      };
      {
        property = "interest-delay";
        positives = [ "normal"; "200ms"; "200ms 1s" ];
        negatives = [ "auto"; "1s 2s 3s"; "-1s" ];
      };
      {
        property = "interest-delay-start";
        positives = [ "normal"; "200ms"; "1s" ];
        negatives = [ "auto"; "1s 2s"; "-1s" ];
      };
      {
        property = "interest-delay-end";
        positives = [ "normal"; "200ms"; "1s" ];
        negatives = [ "auto"; "1s 2s"; "-1s" ];
      };
      {
        property = "nav-up";
        positives =
          [
            "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"";
          ];
        negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
      };
      {
        property = "nav-right";
        positives =
          [
            "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"";
          ];
        negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
      };
      {
        property = "nav-down";
        positives =
          [
            "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"";
          ];
        negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
      };
      {
        property = "nav-left";
        positives =
          [
            "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"";
          ];
        negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
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
        positives = [ "auto"; "text"; "none"; "contain"; "all" ];
        negatives = [ "text none"; "contain all" ];
      };
      {
        property = "-webkit-user-select";
        positives = [ "auto"; "text"; "none"; "contain"; "all" ];
        negatives = [ "text none"; "contain all" ];
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
        negatives = [ "default"; "none card"; "card / layout" ];
      };
      {
        property = "anchor-name";
        positives = [ "none"; "--anchor"; "--a, --b" ];
        negatives = [ "anchor"; "none --a"; "--a --b" ];
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
        property = "position-area";
        positives = [ "none"; "top span-left"; "center" ];
        negatives = [ "top bottom"; "foo" ];
      };
      {
        property = "position-try-order";
        positives = [ "normal"; "most-width"; "most-height" ];
        negatives = [ "most-width normal"; "largest" ];
      };
      {
        property = "position-visibility";
        positives =
          [ "always"; "anchors-visible"; "anchors-visible no-overflow" ];
        negatives = [ "always anchors-visible"; "visible" ];
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
        negatives = [ "red"; "auto red"; "red blue green" ];
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
        property = "font-optical-sizing";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "normal" ];
      };
      {
        property = "font-kerning";
        positives = [ "auto"; "normal"; "none" ];
        negatives = [ "normal none"; "kern" ];
      };
      {
        property = "font-language-override";
        positives = [ "normal"; "\"TRK\"" ];
        negatives = [ "none"; "TRK"; "\"TRK\" \"ENG\"" ];
      };
      {
        property = "font-synthesis-style";
        positives = [ "auto"; "none"; "oblique-only" ];
        negatives = [ "auto none"; "oblique" ];
      };
      {
        property = "font-synthesis-weight";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "normal" ];
      };
      {
        property = "font-synthesis-small-caps";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "normal" ];
      };
      {
        property = "font-synthesis-position";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "normal" ];
      };
      {
        property = "font-variant-ligatures";
        positives =
          [
            "normal";
            "none";
            "common-ligatures";
            "no-contextual common-ligatures";
            "common-ligatures no-discretionary-ligatures historical-ligatures \
             contextual";
          ];
        negatives =
          [
            "normal common-ligatures";
            "none common-ligatures";
            "common-ligatures no-common-ligatures";
            "common-ligatures common-ligatures";
          ];
      };
      {
        property = "font-variant-caps";
        positives =
          [
            "normal";
            "small-caps";
            "all-small-caps";
            "petite-caps";
            "all-petite-caps";
            "unicase";
            "titling-caps";
          ];
        negatives = [ "small-caps unicase"; "normal small-caps" ];
      };
      {
        property = "font-variant-position";
        positives = [ "normal"; "sub"; "super" ];
        negatives = [ "sub super"; "normal sub" ];
      };
      {
        property = "font-variant-east-asian";
        positives =
          [
            "normal";
            "jis78";
            "jis04 full-width";
            "traditional proportional-width ruby";
          ];
        negatives =
          [
            "normal ruby";
            "jis78 jis83";
            "full-width proportional-width";
            "ruby ruby";
          ];
      };
      {
        property = "ruby-position";
        positives =
          [ "alternate"; "over"; "under"; "alternate over"; "inter-character" ];
        negatives = [ "over under"; "alternate inter-character" ];
      };
      {
        property = "ruby-align";
        positives = [ "start"; "center"; "space-between"; "space-around" ];
        negatives = [ "start center"; "auto" ];
      };
      {
        property = "ruby-merge";
        positives = [ "separate"; "merge"; "auto" ];
        negatives = [ "merge separate"; "none" ];
      };
      {
        property = "ruby-overhang";
        positives = [ "auto"; "none" ];
        negatives = [ "auto none"; "over" ];
      };
      {
        property = "column-rule";
        positives = [ "thin solid currentColor"; "solid"; "1px dotted red" ];
        negatives = [ "solid solid"; "red blue" ];
      };
      {
        property = "column-span";
        positives = [ "none"; "all" ];
        negatives = [ "none all"; "2" ];
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
        property = "animation-range-start";
        positives = [ "normal"; "20%"; "entry 10%" ];
        negatives = [ "entry exit"; "10% 20%" ];
      };
      {
        property = "animation-range-end";
        positives = [ "normal"; "90%"; "exit 90%" ];
        negatives = [ "entry exit"; "10% 20%" ];
      };
      {
        property = "animation-composition";
        positives = [ "replace"; "add"; "accumulate, replace" ];
        negatives = [ "add replace"; "compose" ];
      };
      {
        property = "view-transition-name";
        positives = [ "none"; "card"; "match-element" ];
        negatives = [ "card card"; "auto" ];
      };
      {
        property = "view-transition-class";
        positives = [ "none"; "card"; "card primary" ];
        negatives = [ "none card"; "card, primary" ];
      };
      {
        property = "image-orientation";
        positives = [ "none"; "from-image" ];
        negatives = [ "90deg"; "from-image none" ];
      };
      {
        property = "image-rendering";
        positives = [ "auto"; "smooth"; "pixelated"; "crisp-edges" ];
        negatives = [ "pixelated smooth"; "best-quality" ];
      };
      {
        property = "image-resolution";
        positives = [ "1dppx"; "from-image"; "from-image 2dppx" ];
        negatives = [ "from-image from-image"; "-1dppx" ];
      };
      {
        property = "contain-intrinsic-size";
        positives = [ "none"; "auto 300px"; "100px 200px" ];
        negatives = [ "auto"; "1px 2px 3px" ];
      };
      {
        property = "min-intrinsic-sizing";
        positives =
          [
            "legacy";
            "zero-if-scroll";
            "zero-if-extrinsic";
            "zero-if-scroll zero-if-extrinsic";
          ];
        negatives = [ "none"; "zero-if-scroll zero-if-scroll" ];
      };
      {
        property = "interpolate-size";
        positives = [ "numeric-only"; "allow-keywords" ];
        negatives = [ "auto"; "numeric-only allow-keywords" ];
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
        property = "offset-rotate";
        positives = [ "auto"; "reverse"; "reverse 45deg" ];
        negatives = [ "reverse reverse"; "45px" ];
      };
      {
        property = "scroll-timeline";
        positives = [ "none"; "--scroller block"; "--x x, --y y" ];
        negatives = [ "block --scroller"; "--a --b" ];
      };
      {
        property = "scroll-timeline-name";
        positives = [ "none"; "--scroller"; "--x, --y" ];
        negatives = [ "scroller"; "--x --y" ];
      };
      {
        property = "scroll-timeline-axis";
        positives = [ "block"; "inline"; "x"; "y" ];
        negatives = [ "z"; "block inline" ];
      };
      {
        property = "view-timeline";
        positives = [ "none"; "--reveal inline"; "--x x, --y y" ];
        negatives = [ "inline --reveal"; "--a --b" ];
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
        property = "view-timeline-inset";
        positives = [ "auto"; "auto 10%"; "20px 10%" ];
        negatives = [ "auto auto auto"; "red" ];
      };
      {
        property = "timeline-scope";
        positives = [ "none"; "--timeline"; "--a, --b" ];
        negatives = [ "timeline"; "--a --b" ];
      };
      {
        property = "perspective-origin";
        positives = [ "center"; "left top"; "left 10px top 20px"; "10px 20%" ];
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
        positives =
          [ "center"; "left top"; "10px 20px"; "calc(10% + 1px) 20px" ];
        negatives = [ "left right"; "top bottom"; "left 10px top 20px" ];
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
        negatives = [ "red red"; "red blue" ];
      };
      {
        property = "mask-border";
        positives =
          [ "none"; "url(mask.svg) 30 fill"; "url(mask.svg) 30 / 10px" ];
        negatives = [ "fill fill"; "fill fill fill" ];
      };
      {
        property = "border-image";
        positives =
          [
            "none";
            "linear-gradient(red, blue) 30";
            "linear-gradient(red, blue) 30 fill / 10px / 1 stretch";
          ];
        negatives = [ "none none"; "linear-gradient(red, blue) fill fill" ];
      };
      {
        property = "object-view-box";
        positives = [ "none"; "inset(0 0 10% 0)"; "xywh(0 0 100% 100%)" ];
        negatives = [ "inset()"; "rect(0, 1px)" ];
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
        property = "overlay";
        positives = [ "none"; "auto" ];
        negatives = [ "auto none"; "hidden" ];
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
        negatives = [ "normal light"; "light normal"; "only" ];
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
        property = "fill-rule";
        positives = [ "nonzero"; "evenodd" ];
        negatives = [ "nonzero evenodd"; "even-odd" ];
      };
      {
        property = "clip-rule";
        positives = [ "nonzero"; "evenodd" ];
        negatives = [ "nonzero evenodd"; "even-odd" ];
      };
      {
        property = "stroke-linecap";
        positives = [ "butt"; "round"; "square" ];
        negatives = [ "butt round"; "flat" ];
      };
      {
        property = "stroke-linejoin";
        positives = [ "miter"; "miter-clip"; "round"; "bevel"; "arcs" ];
        negatives = [ "miter bevel"; "mitre" ];
      };
      {
        property = "stroke-miterlimit";
        positives = [ "0"; ".5"; "1"; "4"; "10.5"; "calc(2 * 3)" ];
        negatives = [ "-1"; "4px"; "4 4" ];
      };
      {
        property = "stroke-dashoffset";
        positives = [ "0"; "4"; "4px"; "10%"; "-2px" ];
        negatives = [ "none"; "4 2"; "red" ];
      };
      {
        property = "stroke-dasharray";
        positives = [ "none"; "4"; "4 2"; "4, 2"; "4px 2px"; "10% 5%" ];
        negatives = [ "red"; "4 red" ];
      };
      {
        property = "vector-effect";
        positives =
          [
            "none";
            "non-scaling-stroke";
            "non-scaling-size";
            "non-rotation";
            "fixed-position";
            "non-scaling-stroke screen";
            "non-scaling-stroke fixed-position";
          ];
        negatives = [ "bogus"; "screen"; "none non-scaling-stroke" ];
      };
      {
        property = "paint-order";
        positives =
          [
            "normal";
            "fill";
            "stroke";
            "markers";
            "stroke fill";
            "markers stroke fill";
          ];
        negatives = [ "fill fill"; "normal fill"; "bogus"; "fill bogus" ];
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
        property = "overscroll-behavior-inline";
        positives = [ "auto"; "contain"; "none" ];
        negatives = [ "contain none"; "hidden" ];
      };
      {
        property = "overscroll-behavior-block";
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
