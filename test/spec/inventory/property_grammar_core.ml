open Property_grammar_row

let rows =
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
        ]
        @ omitted_filters @ blur_lengths;
      negatives = [ "drop-shadow()"; "blur(red)" ] @ invalid_blur_lengths;
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
          (* Chrome reads a reserved word inside a longer unquoted name, which
             sec. 2.1.1 excludes; a generic family in one (system-ui above) it
             refuses as the section says. Pinned here so the shape excusing the
             first is checked against the fixed population. *)
          "none default";
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
      (* CSS Text Decoration 4 sec. 2.4: auto | from-font | <length-percentage>
         | <line-width>. The <length-percentage> arm carries no [0,inf] range,
         so a negative thickness is grammar; <line-width> is the CSS Borders 4
         sec. 2.3 keyword set. *)
      property = "text-decoration-thickness";
      positives =
        [
          "auto";
          "from-font";
          "0";
          "1px";
          "10%";
          "-1px";
          "-10%";
          "calc(-1px)";
          "calc(-10%)";
          "calc(2px - 3px)";
          "hairline";
          "thin";
          "thick";
        ];
      negatives = [ "red"; "1px 2px"; "none"; "min-content"; "stretch" ];
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
      (* CSS Transitions 1 (ED) sec. 2.5 spells the shorthand
         [<single-transition>#] over [<single-transition> = [ none |
         <single-transition-property> ] || <time> || <easing-function> ||
         <time>], so [none] is one entry of the list wherever it stands and not
         a whole-value keyword. *)
      property = "transition";
      positives =
        [
          "opacity 1s ease-in .2s";
          "all .2s linear .1s";
          "opacity calc(500ms + .5s)";
          "opacity 1s, none";
        ];
      negatives = [ "1s 2s 3s"; "ease opacity ease" ];
    };
    {
      property = "transition-behavior";
      positives = [ "normal"; "allow-discrete" ];
      negatives = [ "normal allow-discrete"; "discrete" ];
    };
    {
      (* CSS Animations 1 sec. 3: <keyframes-name> = <custom-ident> | <string>,
         and the only name the section excludes on top of what <custom-ident>
         already does is none, so [infinite] names a keyframe set after an
         iteration count. CSS Values 4 sec. 4.2 keeps the reserved [default] out
         of the ident arm, leaving it the string. *)
      property = "animation";
      positives =
        [
          "fade 1s linear 2 alternate both running";
          "none";
          "fade calc(1s * 2)";
          "infinite infinite";
          "--x x";
        ];
      negatives = [ "1s 2s 3s"; "2 3"; "default 1s"; "1s default" ];
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
      (* CSS Lists 3 sec. 3.6: <'list-style-position'> || <'list-style-image'>
         || <'list-style-type'>, and CSS Counter Styles 3 sec. 3 makes
         <counter-style-name> a <custom-ident> excluding none, so [outside]
         names a counter style after the position keyword. *)
      property = "list-style";
      positives =
        [ "square inside"; "none"; "inside outside"; "url(marker.png) outside" ];
      negatives = [ "square disc"; "inside outside outside" ];
    };
    {
      (* CSS Lists 3 sec. 2.1 spells a counter name [<custom-ident>], so CSS
         Values 4 sec. 4.2 keeps the reserved [default] out of it in every ASCII
         case permutation. *)
      property = "counter-reset";
      positives = [ "none"; "section"; "section 2"; "section 2 page -1" ];
      negatives =
        [
          "none section";
          "section 1.5";
          "section none";
          "default";
          "DEFAULT";
          "section default";
        ];
    };
    {
      property = "counter-increment";
      positives = [ "none"; "section"; "section 2"; "section 2 page -1" ];
      negatives = [ "none section"; "section 1.5"; "section none"; "default" ];
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
