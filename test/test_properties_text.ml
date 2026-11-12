(** Property tests: text, white space, writing modes and lists. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_text_overflow =
  check_value_cursor "text-overflow" read_text_overflow pp_text_overflow

let check_text_decoration_shorthand =
  check_value_cursor "text_decoration_shorthand" read_text_decoration_shorthand
    pp_text_decoration_shorthand

let check_text_decoration_line =
  check_value_cursor "text_decoration_line" read_text_decoration_line
    pp_text_decoration_line

let check_text_size_adjust =
  check_value_cursor "text_size_adjust" read_text_size_adjust
    pp_text_size_adjust

let check_line_height =
  check_value_cursor "line-height" read_line_height pp_line_height

let check_list_style_type =
  check_value_cursor "list-style-type" read_list_style_type pp_list_style_type

let check_list_style_position =
  check_value_cursor "list-style-position" read_list_style_position
    pp_list_style_position

let check_list_style_image =
  check_value_cursor "list-style-image" read_list_style_image
    pp_list_style_image

let check_unicode_bidi =
  check_value_cursor "unicode-bidi" read_unicode_bidi pp_unicode_bidi

let check_text_decoration_skip_ink =
  check_value_cursor "text-decoration-skip-ink" read_text_decoration_skip_ink
    pp_text_decoration_skip_ink

let check_vertical_align =
  check_value_cursor "vertical-align" read_vertical_align pp_vertical_align

let check_list_style =
  check_value_cursor "list-style" read_list_style pp_list_style

let check_list_style_shorthand =
  check_value_cursor "list-style-shorthand" read_list_style_shorthand
    pp_list_style_shorthand

let check_text_decoration =
  check_value_cursor "text_decoration" read_text_decoration pp_text_decoration

let check_text_transform =
  check_value_cursor "text_transform" read_text_transform pp_text_transform

let check_text_indent_value =
  check_value_cursor "text_indent_value" read_text_indent_value
    pp_text_indent_value

let check_text_transform_case =
  check_value_cursor "text_transform_case" read_text_transform_case
    pp_text_transform_case

let check_symbols_type =
  check_value_cursor "symbols_type" read_symbols_type pp_symbols_type

let check_list_style_symbol =
  check_value_cursor "list_style_symbol" read_list_style_symbol
    pp_list_style_symbol

let check_quotes = check_value_cursor "quotes" read_quotes pp_quotes

let check_caption_side =
  check_value_cursor "caption_side" read_caption_side pp_caption_side

let check_webkit_line_clamp =
  check_value_cursor "webkit_line_clamp" read_webkit_line_clamp
    pp_webkit_line_clamp

let test_text_align () =
  check_text_align "left";
  check_text_align "right";
  check_text_align "center";
  check_text_align "justify";
  check_text_align "start";
  check_text_align "end";
  check_text_align "match-parent";
  check_text_align "-webkit-match-parent";
  check_text_align "inherit";
  neg_cursor read_text_align "invalid-align";
  (* vertical align, not text align *)
  neg_cursor read_text_align "middle";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_text_align "left right";
  (* wrong form *)
  neg_cursor read_text_align "justified"

let test_text_decoration_style () =
  check_text_decoration_style "solid";
  check_text_decoration_style "double";
  check_text_decoration_style "dotted";
  check_text_decoration_style "dashed";
  check_text_decoration_style "wavy";
  check_text_decoration_style "inherit";
  neg_cursor read_text_decoration_style "invalid-style";
  (* multiple styles *)
  neg_cursor ~allow_partial:true read_text_decoration_style "solid dotted";
  (* typo *)
  neg_cursor read_text_decoration_style "wavey";
  neg_cursor read_text_decoration_style "underline"

let test_text_overflow () =
  check_text_overflow "clip";
  check_text_overflow "ellipsis";
  check_text_overflow "clip ellipsis";
  check_text_overflow "inherit";
  neg_cursor read_text_overflow "invalid-overflow";
  neg_cursor read_text_overflow "hidden";
  (* literal ellipsis not valid *)
  neg_cursor read_text_overflow "...";
  (* CSS Overflow 4 text-overflow accepts at most two markers *)
  neg_cursor ~allow_partial:true read_text_overflow "clip ellipsis clip"

let test_text_wrap () =
  check_text_wrap "wrap";
  check_text_wrap "nowrap";
  check_text_wrap "auto";
  check_text_wrap "balance";
  check_text_wrap "stable";
  check_text_wrap "pretty";
  check_text_wrap "nowrap balance";
  check_text_wrap ~expected:"nowrap balance" "balance nowrap";
  check_text_wrap "inherit";
  neg_cursor read_text_wrap "invalid-wrap";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_text_wrap "wrap nowrap";
  neg_cursor ~allow_partial:true read_text_wrap "balance pretty";
  (* wrong form *)
  neg_cursor read_text_wrap "no-wrap";
  neg_cursor read_text_wrap "balanced"

let test_white_space () =
  check_white_space "normal";
  check_white_space "nowrap";
  check_white_space "pre";
  check_white_space "pre-wrap";
  check_white_space "pre-line";
  check_white_space "break-spaces";
  check_white_space "collapse";
  check_white_space "inherit";
  neg_cursor read_white_space "invalid-space";
  (* hyphenated form incorrect *)
  neg_cursor read_white_space "no-wrap";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_white_space "normal nowrap";
  (* incomplete *)
  neg_cursor read_white_space "preserve"

let test_word_break () =
  check_word_break "normal";
  check_word_break "break-all";
  check_word_break "keep-all";
  (* Intentional legacy: word-break: break-word is non-standard, kept for
     compatibility; modern alternative is overflow-wrap:anywhere *)
  check_word_break "break-word";
  check_word_break "inherit";
  neg_cursor read_word_break "invalid-break";
  (* incomplete *)
  neg_cursor read_word_break "break";
  (* different property *)
  neg_cursor read_word_break "word-wrap";
  (* contradictory *)
  neg_cursor ~allow_partial:true read_word_break "normal break-all"

let test_overflow_wrap () =
  check_overflow_wrap "normal";
  check_overflow_wrap "break-word";
  check_overflow_wrap "anywhere";
  check_overflow_wrap "inherit";
  neg_cursor read_overflow_wrap "invalid-wrap";
  neg_cursor ~allow_partial:true read_overflow_wrap "normal break-word";
  (* contradictory *)
  neg_cursor read_overflow_wrap "breakword";
  (* missing hyphen *)
  (* not a valid value *)
  neg_cursor read_overflow_wrap "everywhere"

let test_hyphens () =
  check_hyphens "none";
  check_hyphens "manual";
  check_hyphens "auto";
  check_hyphens "inherit";
  neg_cursor read_hyphens "invalid-hyphens";
  neg_cursor read_hyphens "true";
  (* boolean not valid *)
  neg_cursor ~allow_partial:true read_hyphens "auto manual";
  (* contradictory *)
  (* wrong form *)
  neg_cursor read_hyphens "hyphenate"

let test_line_height () =
  (* Only test values supported by the simplified reader *)
  check_line_height "normal";
  check_line_height "inherit";
  check_line_height "1.5";
  check_line_height "120%";
  decl_optimizes ~prop:"line-height" ~held:"calc(28/18)" ~into:"1.55556"
    "calc(28 / 18)";
  decl_lossless ~prop:"line-height" ~into:"calc(28/18)" "calc(28 / 18)";
  decl_optimizes ~prop:"line-height" ~held:"calc(10 + 8)" ~into:"18"
    "calc(10 + 8)";
  decl_optimizes ~prop:"line-height" ~held:"calc(7*4)" ~into:"28" "calc(7 * 4)";
  decl_optimizes ~prop:"line-height" ~held:"calc((28/18))" ~into:"1.55556"
    "calc((28 / 18))";
  decl_optimizes ~prop:"line-height" ~held:"calc(28/var(--x))"
    ~into:"calc(28/var(--x))" "calc(28 / var(--x))";
  neg_cursor read_line_height "invalid";
  neg_cursor read_line_height "-1.5";
  (* CSS Inline 3 sec. 5.1 spells the property [normal | <number [0,inf]> |
     <length-percentage [0,inf]>], so every length unit reads and nothing else
     does. *)
  check_line_height "2ch";
  check_line_height "3cqw";
  (* CSS Values 4 sec. 10.2 gives a comparison its arguments' own type, so it
     reads at either half of sec. 5.1's [<number> | <length-percentage>]. A
     comparison over numbers folds to the coefficient it answers, one over a
     length and a percentage resolves at used-value time and keeps its call, the
     way it does at [width]. Chrome 153 reads every row. *)
  check_line_height ~expected:"min(120%,1px)" "min(120%, 1px)";
  check_line_height ~expected:"clamp(0px,120%,100px)" "clamp(0px, 120%, 100px)";
  check_line_height ~expected:"clamp(1px,2em,3%)" "clamp(1px, 2em, 3%)";
  check_line_height ~expected:"max(1px,2px)" "max(1px, 2px)";
  decl_optimizes ~prop:"line-height" ~into:"1" "min(1, 2)";
  neg_cursor read_line_height "min(1px, 2)";
  neg_cursor read_line_height "0s";
  neg_cursor read_line_height "45deg";
  neg_cursor read_line_height "10zz";
  (* negative line-height *)
  (* multiple values *)
  neg_cursor ~allow_partial:true read_line_height "normal 1.5"

let test_text_transform () =
  check_text_transform "uppercase";
  check_text_transform "lowercase";
  check_text_transform "capitalize";
  check_text_transform "none";
  neg_cursor read_text_transform "invalid-transform";
  neg_cursor ~allow_partial:true read_text_transform "uppercase lowercase";
  (* contradictory *)
  neg_cursor read_text_transform "upper-case";
  (* wrong form *)
  (* not a valid value *)
  neg_cursor read_text_transform "title-case"

let test_text_decoration_line () =
  check_text_decoration_line "underline";
  check_text_decoration_line "overline";
  check_text_decoration_line "line-through";
  neg_cursor read_text_decoration_line "invalid-line";
  neg_cursor read_text_decoration_line "strikethrough";
  (* wrong name *)
  neg_cursor ~allow_partial:true read_text_decoration_line
    "underline overline underline";
  (* duplicate *)
  (* that's a style, not a line *)
  neg_cursor read_text_decoration_line "wavy"

let test_text_decoration () =
  check_text_decoration "underline";
  check_text_decoration "line-through";
  check_text_decoration ~expected:"none" "none";
  neg_cursor ~allow_partial:true read_text_decoration "invalid-decoration";
  (* duplicate - per CSS spec, || combinator means each component at most
     once *)
  neg_cursor read_text_decoration "underline line-through underline";
  (* A style or colour is valid without a line: every component is optional. *)
  check_text_decoration "solid";
  check_text_decoration "red"

let test_text_decoration_shorthand () =
  (* Test individual parts. The printer holds every component it parsed;
     dropping one that equals its longhand initial is a node change the
     optimizer makes (pinned in test_declaration). *)
  check_text_decoration_shorthand "underline";
  check_text_decoration_shorthand "underline solid";
  check_text_decoration_shorthand ~expected:"underline solid red"
    "underline solid red";
  check_text_decoration_shorthand ~expected:"underline solid red 2px"
    "underline solid red 2px";
  (* Test multiple lines *)
  check_text_decoration_shorthand ~expected:"underline overline"
    "underline overline";
  check_text_decoration_shorthand ~expected:"underline overline dashed"
    "underline overline dashed";
  (* Test order independence *)
  check_text_decoration_shorthand ~expected:"underline solid red"
    "red solid underline";
  check_text_decoration_shorthand ~expected:"underline wavy blue 3px"
    "3px wavy blue underline";
  decl_optimizes ~prop:"text-decoration" ~held:"underline wavy blue 3px"
    ~into:"underline wavy #00f 3px" "3px wavy blue underline";
  neg_cursor ~allow_partial:true read_text_decoration_shorthand
    "invalid-decoration";
  neg_cursor read_text_decoration_shorthand "underline underline";
  (* duplicate line *)
  neg_cursor read_text_decoration_shorthand "solid solid";
  (* duplicate style *)
  (* multiple colors *)
  neg_cursor ~allow_partial:true read_text_decoration_shorthand "red blue"

let test_list_style_type () =
  check_list_style_type "none";
  check_list_style_type "disc";
  check_list_style_type "circle";
  check_list_style_type "square";
  check_list_style_type "decimal";
  check_list_style_type "lower-alpha";
  check_list_style_type "upper-alpha";
  check_list_style_type "lower-roman";
  check_list_style_type "upper-roman";
  (* Predefined counter styles (CSS Counter Styles 3 sec. 6). *)
  check_list_style_type "decimal-leading-zero";
  check_list_style_type "lower-greek";
  check_list_style_type "lower-latin";
  check_list_style_type "upper-latin";
  check_list_style_type "armenian";
  check_list_style_type "georgian";
  check_list_style_type "cjk-decimal";
  check_list_style_type "hebrew";
  check_list_style_type "katakana";
  check_list_style_type "ethiopic-numeric";
  check_list_style_type "disclosure-open";
  (* CSS Lists 3 section 3.4 accepts custom counter-style names, but the
     <custom-ident> grammar reserves [default]. *)
  neg_cursor read_list_style_type "default";
  (* CSS Lists 3 sec. 6.2: [symbols() = symbols( <symbols-type>? [ <string> |
     <image> ]+ )]. [Cursor.call] did not require the reader to consume its
     whole sub-cursor, so a trailing token the [<symbols-type>? [<string> |
     <image>]+] grammar cannot place was silently dropped instead of
     invalidating the declaration. *)
  check_list_style_type "symbols(cyclic \"a\" \"b\")"
    ~expected:"symbols(cyclic\"a\"\"b\")";
  check_list_style_type "symbols( cyclic \"a\" \"b\" )"
    ~expected:"symbols(cyclic\"a\"\"b\")";
  neg_cursor read_list_style_type "symbols(\"a\" 5)"

let test_list_style_position () =
  check_list_style_position "inside";
  check_list_style_position "outside";
  check_list_style_position "inherit";
  neg_cursor read_list_style_position "middle"

let test_list_style_image () =
  check_list_style_image "none";
  check_list_style_image "inherit";
  check_list_style_image "url(https://example.com/x.png)";
  (* CSS Lists 3 sec. 3.5 gives the property an [<image>], so every image the
     vocabulary reads goes here and only the comma list does not. *)
  check_list_style_image "linear-gradient(red,blue)";
  check_list_style_image "conic-gradient(in hsl longer hue,red,blue)";
  check_list_style_image "image-set(url(a.png)1x)";
  check_list_style_image "cross-fade(url(a.png) 40%,url(b.png))";
  neg_cursor read_list_style_image "invalid-url"

let test_vertical_align () =
  check_vertical_align "baseline";
  check_vertical_align "top";
  check_vertical_align "middle";
  check_vertical_align "bottom";
  check_vertical_align "text-top";
  check_vertical_align "text-bottom";
  check_vertical_align "sub";
  check_vertical_align "super";
  check_vertical_align "inherit";
  (* vertical-align also takes <length-percentage> (CSS2 section 10.8), so a
     length, var(), and a held (non-reducible) calc() are all valid. *)
  check_vertical_align "10px";
  check_vertical_align "var(--v)";
  check_vertical_align "calc(50% + 10px)";
  (* A unitless 0 is the valid zero <length> and stays unitless; any other
     unitless number is not a length. *)
  check_vertical_align "0";
  (* Every length unit is a [<length-percentage>], so the reader takes no list
     of its own: [ch], [vh] and [cap] read like [px], and a math function over
     them does too. Chrome 153 takes all four. *)
  check_vertical_align "2ch";
  check_vertical_align "2vh";
  check_vertical_align "2cap";
  check_vertical_align "max(1rem,2vw)";
  neg_cursor read_vertical_align "5";
  (* The grammar has no keyword branch, so the intrinsic sizes a bare length
     reader would take are out. *)
  neg_cursor read_vertical_align "min-content";
  neg_cursor read_vertical_align "invalid-align"

let test_content () =
  check_content "\"text\"";
  check_content ~roundtrip:true "\"nav  main\"";
  check_content "none";
  check_content "normal";
  check_content "open-quote";
  check_content "close-quote";
  check_content ~expected:"attr(data-label)" "attr(data-label)";
  check_content ~expected:"attr(data-label string,\"x y\")"
    "attr(data-label string, \"x y\")";
  check_content ~expected:"attr(data-label string,var(--label,\"x y\"))"
    "attr(data-label string, var(--label, \"x y\"))";
  check_content ~minify:false ~expected:"attr(data-label string, \"x y\")"
    "attr(data-label string, \"x y\")";
  check_content ~minify:false
    ~expected:"attr(data-label string, var(--label, \"x y\"))"
    "attr(data-label string, var(--label, \"x y\"))";
  neg_cursor read_content "invalid-content"

let test_direction () =
  check_direction "ltr";
  check_direction "rtl";
  check_direction "inherit";
  neg_cursor read_direction "invalid-direction"

let test_unicode_bidi () =
  check_unicode_bidi "normal";
  check_unicode_bidi "embed";
  check_unicode_bidi "isolate";
  check_unicode_bidi "bidi-override";
  check_unicode_bidi "isolate-override";
  check_unicode_bidi "plaintext";
  check_unicode_bidi "inherit";
  neg_cursor read_unicode_bidi "invalid-bidi"

let test_writing_mode () =
  check_writing_mode "horizontal-tb";
  check_writing_mode "vertical-rl";
  check_writing_mode "vertical-lr";
  check_writing_mode "inherit";
  neg_cursor read_writing_mode "invalid-mode"

let test_text_size_adjust () =
  check_text_size_adjust "none";
  check_text_size_adjust "auto";
  check_text_size_adjust "100%";
  check_text_size_adjust "inherit";
  neg_cursor read_text_size_adjust "invalid";
  neg_cursor read_text_size_adjust "-50%"

let test_text_decoration_skip_ink () =
  check_text_decoration_skip_ink "auto";
  check_text_decoration_skip_ink "none";
  check_text_decoration_skip_ink "all";
  check_text_decoration_skip_ink "inherit";
  neg_cursor read_text_decoration_skip_ink "invalid-skip"

let test_list_style () =
  (* All-initial values collapse to the canonical single token [disc], never the
     position initial [outside] (which would change which longhand is set). *)
  (* The printer holds every component it parsed; dropping one that equals its
     longhand initial is a node change the optimizer makes (pinned in
     test_declaration). *)
  check_list_style "disc";
  check_list_style "outside";
  check_list_style "disc outside";
  check_list_style "disc outside none";
  check_list_style "square inside";
  check_list_style "square outside";
  check_list_style "inside";
  (* CSS Lists 3 (ED) sec. 3.6 sends a lone [none] to both the type and the
     image, so the single keyword is the same node the pair spells out. *)
  check_list_style "none";
  check_list_style "url(a.png)";
  check_list_style "square outside url(a.png)";
  check_list_style "inherit";
  check_list_style "initial";
  neg_cursor read_list_style "12px"

let test_list_style_shorthand () =
  check_list_style_shorthand "disc";
  check_list_style_shorthand "disc outside";
  check_list_style_shorthand "square inside";
  check_list_style_shorthand "square";
  check_list_style_shorthand "none";
  neg_cursor read_list_style_shorthand "12px"

let test_text_indent_value () =
  check_text_indent_value "1em";
  check_text_indent_value ~expected:"1em hanging" "hanging 1em";
  check_text_indent_value ~expected:"2em hanging each-line"
    "each-line 2em hanging";
  check_text_indent_value "inherit";
  check_text_indent_value "var(--indent,1em)";
  (* the <length-percentage> part takes a held (non-reducible) calc(). *)
  check_text_indent_value "calc(50% + 10px)";
  neg_cursor read_text_indent_value "hanging";
  neg_cursor read_text_indent_value "1em hanging hanging";
  neg_cursor read_text_indent_value "1em each-line each-line"

let test_text_transform_case () =
  check_text_transform_case "capitalize";
  check_text_transform_case "uppercase";
  check_text_transform_case "lowercase";
  neg_cursor read_text_transform_case "full-width"

let test_symbols_type () =
  check_symbols_type "cyclic";
  check_symbols_type "numeric";
  check_symbols_type "alphabetic";
  check_symbols_type "symbolic";
  check_symbols_type "fixed";
  neg_cursor read_symbols_type "disc"

let test_list_style_symbol () =
  check_list_style_symbol "\"*\"";
  check_list_style_symbol ~expected:"url(foo.png)" "url(\"foo.png\")";
  neg_cursor read_list_style_symbol "disc"

let test_caption_side () =
  check_caption_side "top";
  check_caption_side "bottom";
  check_caption_side "inherit";
  neg_cursor read_caption_side "invalid-caption"

let test_webkit_line_clamp () =
  check_webkit_line_clamp "2";
  check_webkit_line_clamp "1";
  check_webkit_line_clamp "unset";
  neg_cursor read_webkit_line_clamp "invalid-clamp"

let test_quotes () =
  check_quotes "auto";
  check_quotes "none";
  neg_cursor ~allow_partial:true read_quotes "invalid-quotes-value"

let suite =
  ( "properties_text",
    [
      test_case "text-align" `Quick test_text_align;
      test_case "text-decoration-style" `Quick test_text_decoration_style;
      test_case "text-overflow" `Quick test_text_overflow;
      test_case "text-wrap" `Quick test_text_wrap;
      test_case "white-space" `Quick test_white_space;
      test_case "word-break" `Quick test_word_break;
      test_case "overflow-wrap" `Quick test_overflow_wrap;
      test_case "hyphens" `Quick test_hyphens;
      test_case "line-height" `Quick test_line_height;
      test_case "list-style-type" `Quick test_list_style_type;
      test_case "list-style-position" `Quick test_list_style_position;
      test_case "list-style-image" `Quick test_list_style_image;
      test_case "symbols type" `Quick test_symbols_type;
      test_case "list style symbol" `Quick test_list_style_symbol;
      test_case "list-style" `Quick test_list_style;
      test_case "list-style-shorthand" `Quick test_list_style_shorthand;
      test_case "content" `Quick test_content;
      test_case "quotes" `Quick test_quotes;
      test_case "text_transform" `Quick test_text_transform;
      test_case "text_indent_value" `Quick test_text_indent_value;
      test_case "text_transform_case" `Quick test_text_transform_case;
      test_case "text_decoration_line" `Quick test_text_decoration_line;
      test_case "text_decoration" `Quick test_text_decoration;
      (* New test cases *)
      test_case "text_decoration_shorthand" `Quick
        test_text_decoration_shorthand;
      test_case "text_size_adjust" `Quick test_text_size_adjust;
      test_case "list_style_type" `Quick test_list_style_type;
      test_case "list_style_position" `Quick test_list_style_position;
      test_case "list_style_image" `Quick test_list_style_image;
      test_case "vertical_align" `Quick test_vertical_align;
      test_case "content" `Quick test_content;
      test_case "direction" `Quick test_direction;
      test_case "unicode_bidi" `Quick test_unicode_bidi;
      test_case "writing_mode" `Quick test_writing_mode;
      test_case "text_decoration_skip_ink" `Quick test_text_decoration_skip_ink;
      test_case "caption_side" `Quick test_caption_side;
      test_case "webkit_line_clamp" `Quick test_webkit_line_clamp;
    ] )
