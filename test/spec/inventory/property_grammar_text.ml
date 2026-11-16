open Property_grammar_row

let rows =
  [
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
        [ "none underline"; "underline underline"; "spelling-error underline" ];
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
      (* CSS Text 4 sec. 5.5: <'text-wrap-mode'> || <'text-wrap-style'>, and
         sec. 5.4 gives text-wrap-style an auto arm. *)
      property = "text-wrap";
      positives = [ "wrap"; "nowrap"; "balance"; "pretty"; "auto" ];
      negatives = [ "wrap nowrap"; "wrap wrap" ];
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
      positives =
        [ "auto"; "6"; "6 3"; "6 3 2"; "auto 3"; "3 auto auto"; "calc(1 + 2)" ];
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
  ]
