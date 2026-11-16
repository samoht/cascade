open Property_grammar_row

let rows =
  [
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
      (* CSS Syntax 3 sec. 2.2 closes an open function at EOF, so [scroll(] is
         the valid [scroll()] and cannot be a negative. It is not a positive
         either: the positive checks append a token after the value, and an
         unclosed function swallows it. *)
      property = "animation-timeline";
      positives = [ "auto"; "none"; "scroll()"; "--timeline" ];
      negatives = [ "auto none"; "scroll() none" ];
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
      negatives = [ "card card"; "auto"; "default"; "DEFAULT" ];
    };
    {
      property = "view-transition-class";
      positives = [ "none"; "card"; "card primary" ];
      negatives = [ "none card"; "card, primary"; "default"; "card default" ];
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
      positives = [ "none"; "path(\"M 0 0 L 1 1\")"; "ray(45deg closest-side)" ];
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
      (* CSS Transitions 1 sec. 2.1: none | <single-transition-property>#, where
         <single-transition-property> = all | <custom-ident>. The exclusion
         clause names none, inherit and initial, not all. *)
      property = "transition-property";
      positives = [ "none"; "all"; "opacity, transform"; "all, opacity" ];
      negatives = [ "none, opacity"; "opacity, none" ];
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
      positives = [ "center"; "left top"; "10px 20px"; "calc(10% + 1px) 20px" ];
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
      positives = [ "none"; "url(mask.svg) 30 fill"; "url(mask.svg) 30 / 10px" ];
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
      negatives = [ "initial fade"; ","; "default"; "DEFAULT"; "fade, default" ];
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
      (* Compositing 2 sec. 3.4.3 spells this <'mix-blend-mode'>#, and sec.
         3.4.1 gives mix-blend-mode plus-lighter, so the value is granted here
         too; Chrome takes it on mix-blend-mode alone. Pinned so the excuse for
         that answers to the fixed population. *)
      positives = [ "normal"; "multiply"; "screen, overlay"; "plus-lighter" ];
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
      positives =
        [
          "source-over";
          "source-in";
          "source-out";
          "source-atop";
          "destination-over";
          "destination-in";
          "destination-out";
          "destination-atop";
          "xor";
          "plus-lighter";
          "clear";
          "copy";
          "source-in, source-out";
        ];
      negatives = [ "add"; "plus-darker"; "source-over xor" ];
    };
    {
      property = "-webkit-mask-source-type";
      positives = [ "auto"; "alpha"; "luminance" ];
      negatives = [ "alpha luminance"; "match-source" ];
    };
    {
      property = "-webkit-mask-clip";
      positives =
        [
          "border";
          "border-box";
          "content";
          "content-box";
          "padding";
          "padding-box";
          "text";
        ];
      negatives =
        [
          "no-clip";
          "fill-box";
          "margin-box";
          "border-box padding-box content-box content-box";
        ];
    };
    {
      property = "-webkit-mask-origin";
      positives =
        [
          "border";
          "border-box";
          "content";
          "content-box";
          "padding";
          "padding-box";
        ];
      negatives =
        [
          "text";
          "no-clip";
          "fill-box";
          "margin-box";
          "border-box padding-box content-box content-box";
        ];
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
        [
          "text";
          "content";
          "margin-box";
          "border-box padding-box content-box content-box";
        ];
    };
    {
      property = "mask-origin";
      positives = [ "border-box"; "padding-box"; "content-box" ];
      negatives =
        [
          "no-clip";
          "content";
          "margin-box";
          "border-box padding-box content-box content-box";
        ];
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
      (* CSS Color Adjust 1 sec. 2.2 spells the list item [light | dark |
         <custom-ident>], so CSS Values 4 sec. 4.2 keeps the reserved [default]
         out of it in every ASCII case permutation. *)
      property = "color-scheme";
      positives = [ "normal"; "light"; "dark"; "only light" ];
      negatives =
        [
          "normal light";
          "light normal";
          "only";
          "default";
          "DEFAULT";
          "light default";
        ];
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
