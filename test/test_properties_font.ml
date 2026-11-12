(** Property tests: font properties. *)

open Alcotest
open Cascade
open Css.Properties
open Css_test_helpers
open Properties_helpers

let check_font_style =
  check_value_cursor "font-style" read_font_style pp_font_style

let check_font_display =
  check_value_cursor "font-display" read_font_display pp_font_display

(* CSS Syntax 3 sec. 5.5.11 is the one caller that asks the tokenizer for
   unicode ranges, so these vectors read as that descriptor's value does. *)
let check_unicode_range =
  check_value_cursor ~unicode_ranges:true "unicode-range" read_unicode_range
    pp_unicode_range

let check_webkit_font_smoothing =
  check_value_cursor "-webkit-font-smoothing" read_webkit_font_smoothing
    pp_webkit_font_smoothing

let check_moz_osx_font_smoothing =
  check_value_cursor "-moz-osx-font-smoothing" read_moz_osx_font_smoothing
    pp_moz_osx_font_smoothing

let check_font_family =
  check_value_cursor "font-family" read_font_family pp_font_family

let check_font_stretch =
  check_value_cursor "font-stretch" read_font_stretch pp_font_stretch

let check_font_variant_numeric =
  check_value_cursor "font-variant-numeric" read_font_variant_numeric
    pp_font_variant_numeric

let check_font_feature_value =
  check_value_cursor "font-feature-value" read_font_feature_value
    pp_font_feature_value

let check_font_feature_setting =
  check_value_cursor "font-feature-setting" read_font_feature_setting
    pp_font_feature_setting

let check_font_variation_setting =
  check_value_cursor "font-variation-setting" read_font_variation_setting
    pp_font_variation_setting

let check_font_variant_css21 =
  check_value_cursor "font-variant-css21" read_font_variant_css21
    pp_font_variant_css21

let check_font = check_value_cursor "font" read_font pp_font

let check_font_shorthand =
  check_value_cursor "font-shorthand" read_font_shorthand pp_font_shorthand

let check_font_weight =
  check_value_cursor "font_weight" read_font_weight pp_font_weight

let check_font_variant_numeric_token =
  check_value_cursor "font_variant_numeric_token"
    read_font_variant_numeric_token pp_font_variant_numeric_token

let check_font_size = check_value_cursor "font_size" read_font_size pp_font_size

let test_font_style () =
  check_font_style "normal";
  check_font_style "italic";
  check_font_style "oblique";
  check_font_style "oblique 45deg";
  check_font_style "inherit";
  neg_cursor read_font_style "invalid";
  neg_cursor read_font_style "italics";
  (* common typo *)
  neg_cursor ~allow_partial:true read_font_style "normal italic"

let test_font_display () =
  check_font_display "auto";
  check_font_display "block";
  check_font_display "swap";
  check_font_display "fallback";
  check_font_display "optional";
  neg_cursor read_font_display "invalid";
  neg_cursor read_font_display "inline";
  neg_cursor ~allow_partial:true read_font_display "auto block"

let test_unicode_range () =
  (* Single code points per CSS spec *)
  check_unicode_range ~expected:"U+0" "U+0000";
  check_unicode_range ~expected:"U+26" "U+26";
  (* ampersand example from MDN *)
  check_unicode_range ~expected:"U+FF" "U+00FF";
  (* case insensitive *)
  check_unicode_range ~expected:"U+ABCD" "U+abcd";
  (* Code point ranges per CSS spec *)
  check_unicode_range ~expected:"U+??" "U+0000-00FF";
  check_unicode_range ~expected:"U+25-FF" "U+0025-00FF";
  (* MDN example format *)
  check_unicode_range ~expected:"U+20-7F" "U+0020-007F";
  (* ASCII printable range *)
  check_unicode_range ~expected:"U+A0-A0FF" "U+A0-A0FF";
  neg_cursor read_unicode_range "invalid";
  neg_cursor read_unicode_range "U+";
  neg_cursor read_unicode_range "U+GGGG";
  neg_cursor ~allow_partial:true read_unicode_range "U+1234-";
  neg_cursor ~allow_partial:true read_unicode_range "U+1234-GGGG";
  neg_cursor read_unicode_range "1234";
  neg_cursor read_unicode_range "+1234";
  neg_cursor read_unicode_range "U1234";
  neg_cursor read_unicode_range "U+12345-1234"

let test_font_variant_numeric_token () =
  check_font_variant_numeric_token "normal";
  check_font_variant_numeric_token "ordinal";
  check_font_variant_numeric_token "slashed-zero";
  neg_cursor read_font_variant_numeric_token "invalid-token";
  neg_cursor ~allow_partial:true read_font_variant_numeric_token
    "normal ordinal";
  (* contradictory *)
  neg_cursor read_font_variant_numeric_token "slashed-zeros";
  (* wrong form *)
  (* not a valid value *)
  neg_cursor read_font_variant_numeric_token "diagonal-zero"

let test_font_weight () =
  (* CSS Fonts 4 (ED) sec. 2.2 makes [normal] and [bold] the numbers 400 and
     700, but swapping one for the other is a node change, so pp prints the
     keyword it was handed and the optimizer folds (test_declaration pins the
     fold). *)
  check_font_weight "normal";
  check_font_weight "bold";
  check_font_weight "700";
  check_font_weight "650.5";
  check_font_weight "1000";
  check_font_weight "lighter";
  neg_cursor read_font_weight "invalid-weight";
  neg_cursor read_font_weight "0.5";
  neg_cursor read_font_weight "1001";
  (* out of range *)
  neg_cursor ~allow_partial:true read_font_weight "normal bold";
  (* multiple values *)
  (* not a valid keyword *)
  neg_cursor read_font_weight "extra-bold"

let test_font_family () =
  check_font_family "sans-serif";
  check_font_family "serif";
  check_font_family "monospace";
  check_font_family "cursive";
  check_font_family "fantasy";
  check_font_family "system-ui";
  (* Per CSS spec, arbitrary font family names are valid (both quoted and
     unquoted identifiers) *)
  check_font_family "invalid-font";
  check_font_family ~expected:"Times New Roman" "\"Times New Roman\"";
  check_font_family "Arial";
  (* CSS Fonts 4 sec. 5.1: a [<family-name>] is matched with Default Caseless
     Matching, a caseless string comparison that folds neither a hyphen to a
     space nor one name to another. [open-sans] names a different family from
     [Open Sans] and [ny] a different family from [New York], so the author's
     spelling survives verbatim - re-reading the output must be a fixpoint. *)
  check_font_family ~roundtrip:true "inter";
  check_font_family ~roundtrip:true "open-sans";
  check_font_family ~roundtrip:true "ny";
  check_font_family ~roundtrip:true "sf-pro";
  check_font_family ~roundtrip:true "jetbrains-mono";
  check_font_family ~roundtrip:true "SFMONO-REGULAR";
  (* A [<string>] family name carries no case either; unquoting a single-word
     name keeps its spelling, so the bare form re-reads to the same family. *)
  check_font_family ~roundtrip:true ~expected:"inter" "\"inter\"";
  check_font_family ~roundtrip:true ~expected:"OPEN SANS" "\"OPEN SANS\"";
  check_font_family ~roundtrip:true ~expected:"Open Sans" "\"Open Sans\"";
  (* CSS Syntax 3 sec. 4.3.9 gates when a name may be spelled as an ident at
     all: the first code point must be a name-start code point, or a [-]
     followed by a name-start code point or by a second [-]. A name that fails
     that test has no unquoted spelling - CSS Fonts 4 sec. 2.1.1 leaves only the
     [<string>] arm of [<family-name>] - and emitting it bare makes the browser
     drop the whole declaration. *)
  check_font_family ~roundtrip:true ~expected:"\"2Brand\"" "\"2Brand\"";
  check_font_family ~roundtrip:true ~expected:"\"9\"" "\"9\"";
  check_font_family ~roundtrip:true ~expected:"\"-2x\"" "\"-2x\"";
  check_font_family ~roundtrip:true ~expected:"\"-\"" "\"-\"";
  (* The guard is not a minify choice: the unminified printer picks the same
     spelling. *)
  check_font_family ~minify:false ~roundtrip:true ~expected:"\"2Brand\""
    "\"2Brand\"";
  check_font_family ~minify:false ~roundtrip:true ~expected:"\"-2x\"" "\"-2x\"";
  (* The other side of sec. 4.3.9: a second [-] starts an ident sequence, and
     sec. 4.2 counts [_] as a name-start code point, so these do unquote. *)
  check_font_family ~roundtrip:true ~expected:"--brand" "\"--brand\"";
  check_font_family ~roundtrip:true ~expected:"--" "\"--\"";
  check_font_family ~roundtrip:true ~expected:"-x" "\"-x\"";
  check_font_family ~roundtrip:true ~expected:"_brand" "\"_brand\"";
  check_font_family ~roundtrip:true ~expected:"_" "\"_\"";
  (* Sec. 4.2: [.] is not a name code point, and sec. 4.3.7 reads [\] as the
     start of an escape, so [a\b] would name the family [ab]. Both stay
     quoted. *)
  check_font_family ~roundtrip:true ~expected:"\"Foo.Bar\"" "\"Foo.Bar\"";
  check_font_family ~roundtrip:true ~expected:"\"a\\\\b\"" "\"a\\\\b\"";
  (* Every word of a [<custom-ident>+] sequence is an ident in its own right, so
     the same start rule gates each of them. *)
  check_font_family ~roundtrip:true ~expected:"\"Brand -\"" "\"Brand -\"";
  check_font_family ~roundtrip:true ~expected:"\"3 Rounded\"" "\"3 Rounded\"";
  (* CSS Fonts 4 sec. 2.1 spells [font-family] as [[ <family-name> |
     <generic-family> ]#], so the bare keyword is the generic and only the
     quoted form names a family; CSS Values 4 sec. 4.2 excludes the CSS-wide
     keywords and the reserved [default] from [<custom-ident>]. Unquoting any of
     these changes the meaning rather than the spelling. *)
  check_font_family ~roundtrip:true ~expected:"\"serif\"" "\"serif\"";
  check_font_family ~roundtrip:true ~expected:"\"inherit\"" "\"inherit\"";
  check_font_family ~roundtrip:true ~expected:"\"default\"" "\"default\"";
  (* CSS Fonts 4 sec. 2.1.1 states the exclusion per identifier, not per name:
     "any identifier which could be misinterpreted as a pre-defined keyword in
     the font-family value definition, or the CSS-wide keywords, is not
     allowed". A word of a [<custom-ident>+] sequence is such an identifier, so
     the rule reaches the first, a middle and the last word alike, and a name
     carrying one has no unquoted spelling at all. *)
  check_font_family ~roundtrip:true ~expected:"\"inherit test\""
    "\"inherit test\"";
  check_font_family ~roundtrip:true ~expected:"\"test inherit\""
    "\"test inherit\"";
  check_font_family ~roundtrip:true ~expected:"\"a initial b\""
    "\"a initial b\"";
  check_font_family ~roundtrip:true ~expected:"\"unset Sans\"" "\"unset Sans\"";
  check_font_family ~roundtrip:true ~expected:"\"Brand revert\""
    "\"Brand revert\"";
  check_font_family ~roundtrip:true ~expected:"\"revert-layer x\""
    "\"revert-layer x\"";
  check_font_family ~roundtrip:true ~expected:"\"a revert-layer b\""
    "\"a revert-layer b\"";
  (* CSS Values 4 sec. 4.2 reserves [default] alongside the CSS-wide keywords
     and excludes it from [<custom-ident>] in every position. *)
  check_font_family ~roundtrip:true ~expected:"\"default x\"" "\"default x\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo default Bar\""
    "\"Foo default Bar\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo default\""
    "\"Foo default\"";
  (* Sec. 4.2 excludes the keywords "in all ASCII case permutations". *)
  check_font_family ~roundtrip:true ~expected:"\"INHERIT test\""
    "\"INHERIT test\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo Default\""
    "\"Foo Default\"";
  (* The pre-defined keywords of the [font-family] value definition are the
     [<generic-font-family>] names, and sec. 2.1.1 adds that a UA "must not
     consider these keywords as matching the <font-family-name> type", so a
     sequence word may not be one either. *)
  check_font_family ~roundtrip:true ~expected:"\"Foo serif\"" "\"Foo serif\"";
  check_font_family ~roundtrip:true ~expected:"\"serif Foo\"" "\"serif Foo\"";
  check_font_family ~roundtrip:true ~expected:"\"a sans-serif b\""
    "\"a sans-serif b\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo monospace\""
    "\"Foo monospace\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo cursive\""
    "\"Foo cursive\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo fantasy\""
    "\"Foo fantasy\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo system-ui\""
    "\"Foo system-ui\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo math\"" "\"Foo math\"";
  check_font_family ~roundtrip:true ~expected:"\"Cambria Math\""
    "\"Cambria Math\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-serif\""
    "\"Foo ui-serif\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-sans-serif\""
    "\"Foo ui-sans-serif\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-monospace\""
    "\"Foo ui-monospace\"";
  check_font_family ~roundtrip:true ~expected:"\"Foo ui-rounded\""
    "\"Foo ui-rounded\"";
  (* Sec. 2.1.2 keeps the script-specific generics functional, so bare
     [fangsong] and [emoji] are not pre-defined keywords of the value definition
     and are ordinary [<custom-ident>]s inside a sequence. *)
  check_font_family ~roundtrip:true ~expected:"Foo emoji" "\"Foo emoji\"";
  check_font_family ~roundtrip:true ~expected:"Foo fangsong" "\"Foo fangsong\"";
  check_font_family ~roundtrip:true ~expected:"Noto Color Emoji"
    "\"Noto Color Emoji\"";
  (* The exclusion is on the whole identifier, so a word that merely contains a
     keyword is an ordinary [<custom-ident>] and still unquotes. *)
  check_font_family ~roundtrip:true ~expected:"inherited test"
    "\"inherited test\"";
  check_font_family ~roundtrip:true ~expected:"Foo serifs" "\"Foo serifs\"";
  check_font_family ~roundtrip:true ~expected:"Foo sans" "\"Foo sans\"";
  check_font_family ~roundtrip:true ~expected:"Times New Roman"
    "\"Times New Roman\"";
  let invalid = read_font_family (Cursor.of_string "Arial, inherit") in
  (match invalid with
  | Invalid tokens ->
      Alcotest.(check string)
        "invalid family list source is preserved" "Arial, inherit"
        (Parser.string_of_components tokens)
  | _ -> Alcotest.fail "expected an Invalid font-family node");
  (* Test actual invalid cases *)
  neg_cursor read_font_family "123invalid";
  (* identifier can't start with number *)
  neg_cursor read_font_family "";
  (* Default minify unquotes a multi-word [<family-name>] (CSS Fonts 4 sec.
     2.1.1: an ident sequence is a valid family name and the unquoted form is
     shorter). [enforce_spec] keeps the quotes, matching the CSSOM-canonical
     serialization, so a font stack stored verbatim in a custom property keeps
     its authored quoting. *)
  let stack =
    read_font_family (Cursor.of_string "\"Segoe UI Symbol\",monospace")
  in
  Alcotest.(check string)
    "default minify unquotes a multi-word family name"
    "Segoe UI Symbol,monospace"
    (Css.Pp.to_string ~minify:true pp_font_family stack);
  Alcotest.(check string)
    "enforce_spec keeps a multi-word family name quoted"
    "\"Segoe UI Symbol\",monospace"
    (Css.Pp.to_string ~minify:true ~enforce_spec:true pp_font_family stack)

let test_font_stretch () =
  check_font_stretch "50%";
  check_font_stretch "inherit";
  neg_cursor read_font_stretch "invalid-stretch";
  (* CSS Fonts 4 (ED) sec. 2.3 makes each keyword a percentage, but swapping one
     for the other is a node change, so pp prints the keyword it was handed and
     the optimizer folds (test_declaration pins the fold). *)
  check_font_stretch "normal";
  check_font_stretch "ultra-condensed";
  check_font_stretch "ultra-expanded"

let test_font_variant_numeric () =
  check_font_variant_numeric "normal";
  check_font_variant_numeric "lining-nums";
  check_font_variant_numeric "tabular-nums";
  neg_cursor read_font_variant_numeric "invalid-variant"

let test_font_feature_value () =
  check_font_feature_value "on";
  check_font_feature_value "off";
  check_font_feature_value "2";
  neg_cursor read_font_feature_value "-1";
  neg_cursor read_font_feature_value "1.5"

let test_font_feature_setting () =
  check_font_feature_setting "\"kern\"";
  check_font_feature_setting "\"swsh\" 2";
  check_font_feature_setting ~expected:"\"a\\\"bc\" on" "\"a\\22 bc\" on";
  neg_cursor read_font_feature_setting "\"bad\""

let test_font_feature_settings () =
  check_font_feature_settings "normal";
  check_font_feature_settings "inherit";
  check_font_feature_settings "\"kern\"";
  check_font_feature_settings "\"liga\" 0";
  (* CSS Fonts 4 sec. 6.12 permits every non-negative integer as a feature
     selection index and requires a four-character printable ASCII tag. The tag
     is a CSS string, so its decoded quote must be escaped again by pp. *)
  check_font_feature_settings "\"swsh\" 2";
  check_font_feature_settings ~expected:"\"a\\\"bc\" 3" "\"a\\22 bc\" 3";
  let structured =
    Feature_list
      [
        { tag = "a\"bc"; value = Some (Index 2) };
        { tag = "kern"; value = Some On };
      ]
  in
  check string "structured font feature settings" "\"a\\\"bc\" 2,\"kern\" on"
    (Css.Pp.to_string ~minify:true pp_font_feature_settings structured);
  neg_cursor read_font_feature_settings "\"\\e9 ab\" 1";
  neg_cursor read_font_feature_settings "\"kern\" 1.5";
  neg_cursor read_font_feature_settings "invalid-feature"

let test_font_variation_settings () =
  check_font_variation_settings "normal";
  check_font_variation_settings "inherit";
  check_font_variation_settings "\"wght\" 400";
  (* CSS Fonts 4 sec. 8.2 pairs a printable four-character tag with a number,
     not an integer. Re-serialize the decoded tag through the CSS printer. *)
  check_font_variation_settings "\"wght\" 123.5";
  check_font_variation_settings ~expected:"\"a\\\"bc\" 123.5"
    "\"a\\22 bc\" 123.5";
  let structured =
    Axis_list
      [ { tag = "a\"bc"; value = 123.5 }; { tag = "wght"; value = 650.25 } ]
  in
  check string "structured font variation settings"
    "\"a\\\"bc\" 123.5,\"wght\" 650.25"
    (Css.Pp.to_string ~minify:true pp_font_variation_settings structured);
  neg_cursor read_font_variation_settings "invalid-variation"

let test_font_variation_setting () =
  check_font_variation_setting "\"wght\" 123.5";
  check_font_variation_setting ~expected:"\"a\\\"bc\" 123.5"
    "\"a\\22 bc\" 123.5";
  neg_cursor read_font_variation_setting "\"bad\" 1"

let test_webkit_font_smoothing () =
  check_webkit_font_smoothing "auto";
  check_webkit_font_smoothing "antialiased";
  check_webkit_font_smoothing "subpixel-antialiased";
  check_webkit_font_smoothing "inherit";
  neg_cursor read_webkit_font_smoothing "invalid-smoothing"

let test_moz_osx_font_smoothing () =
  check_moz_osx_font_smoothing "auto";
  check_moz_osx_font_smoothing "grayscale";
  check_moz_osx_font_smoothing "inherit";
  neg_cursor read_moz_osx_font_smoothing "invalid-smoothing"

let test_font_variant_css21 () =
  check_font_variant_css21 "normal";
  check_font_variant_css21 "small-caps";
  neg_cursor read_font_variant_css21 "oblique"

let test_font () =
  check_font "16px serif";
  check_font "italic 700 16px/1.5 serif";
  check_font "italic bold 16px/1.5 serif";
  check_font "small-caps 12px monospace";
  check_font "inherit";
  check_font "caption";
  neg_cursor read_font "16px";
  (* CSS Cascade 5 sec. 7.3: a CSS-wide keyword cannot be mixed with values. *)
  neg_cursor read_font "initial 16px serif"

let test_font_shorthand () =
  check_font_shorthand "16px serif";
  check_font_shorthand "italic 700 16px/1.5 serif";
  neg_cursor read_font_shorthand "16px"

let test_east_asian_feature () =
  check_east_asian_feature "jis78";
  check_east_asian_feature "traditional";
  check_east_asian_feature "ruby";
  neg_cursor read_east_asian_feature "small-caps"

let test_font_size () =
  check_font_size "16px";
  check_font_size "50%";
  let parsed = read_font_size (Cursor.of_string "50%") in
  (match parsed with
  | Pct n -> Alcotest.(check (float 0.)) "public percentage node" 50. n
  | _ -> Alcotest.fail "font-size percentage did not use Pct");
  check_font_size "small";
  check_font_size "medium";
  check_font_size "large";
  check_font_size "inherit";
  neg_cursor read_font_size "invalid-font-size";
  (* CSS Fonts 4 sec. 2.5 takes an absolute or relative size, a
     [<length-percentage>] or [math]; CSS Sizing 4 sec. 3.2's [fit-content()] is
     a [<box-size>] and no length. *)
  neg_cursor read_font_size "fit-content(20rem)"

let suite =
  ( "properties_font",
    [
      test_case "font-style" `Quick test_font_style;
      test_case "font-display" `Quick test_font_display;
      test_case "unicode-range" `Quick test_unicode_range;
      test_case "font variant east asian feature" `Quick test_east_asian_feature;
      test_case "font-variant-css21" `Quick test_font_variant_css21;
      test_case "font" `Quick test_font;
      test_case "font-shorthand" `Quick test_font_shorthand;
      test_case "font_family" `Quick test_font_family;
      test_case "font_weight" `Quick test_font_weight;
      test_case "font_variant_numeric_token" `Quick
        test_font_variant_numeric_token;
      test_case "font_stretch" `Quick test_font_stretch;
      test_case "font_variant_numeric" `Quick test_font_variant_numeric;
      test_case "font_feature_value" `Quick test_font_feature_value;
      test_case "font_feature_setting" `Quick test_font_feature_setting;
      test_case "font_feature_settings" `Quick test_font_feature_settings;
      test_case "font_variation_setting" `Quick test_font_variation_setting;
      test_case "font_variation_settings" `Quick test_font_variation_settings;
      test_case "webkit_font_smoothing" `Quick test_webkit_font_smoothing;
      test_case "moz_osx_font_smoothing" `Quick test_moz_osx_font_smoothing;
      test_case "font_size" `Quick test_font_size;
    ] )
