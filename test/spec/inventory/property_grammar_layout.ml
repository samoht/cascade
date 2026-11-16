open Property_grammar_row

let rows =
  [
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
      negatives = [ "\"a\" \"a a\""; "\"a .\" \". a\""; "\"nav/main\""; "a b" ];
    };
    {
      (* CSS Grid 2 sec. 7.4: [ <'grid-template-rows'> / <'grid-template-
         columns'> ], and sec. 7.2 gives grid-template-rows a none arm. *)
      property = "grid-template";
      positives =
        [ "none"; "none / 1fr"; "\"a a\" 1fr / 1fr 1fr"; "subgrid / subgrid" ];
      negatives = [ "/"; "1fr / 1fr / 1fr" ];
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
      (* CSS Backgrounds 3 sec. 6.1: <shadow> = <color>? && [ <length>{2} ... ]
         && inset?, so two lengths alone are a whole shadow and one is not. *)
      property = "box-shadow";
      positives =
        [ "none"; "0 0"; "0 1px 2px rgb(0 0 0 / .2)"; "inset 0 0 1px red" ];
      negatives = [ "0"; "inset inset 0 0 1px" ];
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
        [ "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"" ];
      negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
    };
    {
      property = "nav-right";
      positives =
        [ "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"" ];
      negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
    };
    {
      property = "nav-down";
      positives =
        [ "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"" ];
      negatives = [ "current"; "#next current root"; "#next \"_self\"" ];
    };
    {
      property = "nav-left";
      positives =
        [ "auto"; "#next"; "#next current"; "#next root"; "#next \"sidebar\"" ];
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
      (* CSS Anchor Positioning 1 sec. 6.1: none | [ [ <dashed-ident> ||
         <try-tactic> ] | <position-area> ]#, and || is order-free. *)
      property = "position-try-fallbacks";
      positives =
        [
          "none";
          "--fallback";
          "flip-block --fallback";
          "flip-block, --fallback";
        ];
      negatives = [ ","; "--a --b" ];
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
      positives = [ "always"; "anchors-visible"; "anchors-visible no-overflow" ];
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
  ]
