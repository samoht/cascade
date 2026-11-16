(** Optimize tests: vendor prefixes and targets, lossless mode and declaration
    order. *)

open Cascade
open Css.Optimize
open Optimize_helpers

(* Adjacent identical-body rules must merge even on a sheet whose factoring
   preflight predicts too little gain to run the global scheduler: the merge is
   a local always-on rewrite. The filler rules share nothing, so the only
   predicted gain is the one duplicate body, far below the preflight
   threshold. *)
let test_adjacent_merge_survives_preflight_skip () =
  let filler =
    List.init 1200 (fun i ->
        String.concat ""
          [
            ".r"; string_of_int i; "{margin-left:"; string_of_int (i + 1); "px}";
          ])
  in
  let css =
    String.concat ""
      (filler @ [ ".flex-grow{flex-grow:1}"; ".grow{flex-grow:1}" ])
  in
  match Css.of_string css with
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)
  | Ok { Css.stylesheet; _ } ->
      let out = minify stylesheet in
      Alcotest.(check bool)
        "adjacent duplicate bodies merged on a preflight-skipped sheet" true
        (Astring.String.is_infix ~affix:".flex-grow,.grow{flex-grow:1}" out)

let test_vendor_prefix_strip () =
  let opt ?(enforce_spec = false) css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~enforce_spec p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* Default targets evergreen browsers: a vendor prefix whose unprefixed twin
     is present with the same value is redundant and dropped. *)
  Alcotest.(check string)
    "box-sizing prefix drops by default" ".a{box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:border-box;box-sizing:border-box}");
  (* --enforce-spec drops the evergreen target: every prefix is kept. *)
  Alcotest.(check string)
    "enforce-spec keeps every prefix"
    ".a{-moz-box-sizing:border-box;box-sizing:border-box}"
    (opt ~enforce_spec:true
       ".a{-moz-box-sizing:border-box;box-sizing:border-box}");
  (* A differing value is not a redundant twin. *)
  Alcotest.(check string)
    "differing value kept"
    ".a{-moz-box-sizing:content-box;box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:content-box;box-sizing:border-box}")

(* The evergreen-target vendor drop is a Baseline question, not a hand-kept pair
   list: a prefixed declaration is dead when its unprefixed twin sits in the
   same rule with the same value and importance AND that unprefixed property is
   Baseline "widely available". [Baseline.greenfield_properties] holds the
   properties that are not, which is exactly the set whose prefix a maintained
   browser may still need. *)
let test_vendor_prefix_baseline_gate () =
  let opt ?targets ?(enforce_spec = false) ?(scope = `Fragment) css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true
          (Css.optimize ?targets ~scope ~enforce_spec p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  let chrome_120 = { Css.Optimize.evergreen_targets with chrome = (120, 0) } in
  (* Widely available: every maintained browser reads the unprefixed form, so
     the WebKit copy is dead weight. *)
  Alcotest.(check string)
    "text-decoration-color prefix drops" ".a{text-decoration-color:red}"
    (opt ".a{-webkit-text-decoration-color:red;text-decoration-color:red}");
  Alcotest.(check string)
    "mask-image prefix drops" ".a{mask-image:none}"
    (opt ~targets:chrome_120 ".a{-webkit-mask-image:none;mask-image:none}");
  (* box-sizing is the same rule and already collapses; keep it pinned. *)
  Alcotest.(check string)
    "webkit box-sizing prefix drops" ".a{box-sizing:border-box}"
    (opt ".a{-webkit-box-sizing:border-box;box-sizing:border-box}");
  Alcotest.(check string)
    "moz box-sizing prefix drops" ".a{box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:border-box;box-sizing:border-box}");
  (* Not widely available: [backdrop-filter] and [user-select] are both in
     [Baseline.greenfield_properties], so their prefix is still load-bearing
     (Safari reads only -webkit-backdrop-filter up to 17.6) and stays. *)
  Alcotest.(check string)
    "backdrop-filter prefix kept"
    ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}"
    (opt ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}");
  Alcotest.(check string)
    "user-select prefix kept" ".a{-webkit-user-select:none;user-select:none}"
    (opt ".a{-webkit-user-select:none;user-select:none}");
  (* --enforce-spec drops the evergreen target, so every prefix survives. *)
  Alcotest.(check string)
    "enforce-spec keeps the text-decoration-color prefix"
    ".a{-webkit-text-decoration-color:red;text-decoration-color:red}"
    (opt ~enforce_spec:true
       ".a{-webkit-text-decoration-color:red;text-decoration-color:red}");
  Alcotest.(check string)
    "enforce-spec keeps the mask-image prefix"
    ".a{-webkit-mask-image:none;mask-image:none}"
    (opt ~enforce_spec:true ".a{-webkit-mask-image:none;mask-image:none}");
  (* The pair is the same pair however it got here: cascade synthesising the
     prefix beside a lone [mask-image] and an author writing both must settle on
     one emission, or the second pass over the first one's output moves. The
     twin test asks whether the two spell the same value, which a [var()]
     fallback the slot cannot type answers the same way wherever it was
     written. *)
  let sheet css = opt ~scope:`Stylesheet css in
  Alcotest.(check string)
    "an authored mask-image prefix pair settles where a synthesised one does"
    (sheet ".a{mask-image:var(--x,10px)}")
    (sheet ".a{-webkit-mask-image:var(--x,10px);mask-image:var(--x,10px)}");
  Alcotest.(check string)
    "the same pair over a fallback the slot can type"
    (sheet ".a{mask-image:var(--x,url(a.png))}")
    (sheet
       ".a{-webkit-mask-image:var(--x,url(a.png));mask-image:var(--x,url(a.png))}");
  Alcotest.(check string)
    "enforce-spec keeps the box-sizing prefix"
    ".a{-webkit-box-sizing:border-box;box-sizing:border-box}"
    (opt ~enforce_spec:true
       ".a{-webkit-box-sizing:border-box;box-sizing:border-box}");
  Alcotest.(check string)
    "enforce-spec keeps the backdrop-filter prefix"
    ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}"
    (opt ~enforce_spec:true
       ".a{-webkit-backdrop-filter:blur(2px);backdrop-filter:blur(2px)}");
  (* A twin only supersedes at the same value and the same importance, whatever
     the Baseline status of the property. *)
  Alcotest.(check string)
    "differing value keeps the prefix"
    ".a{-webkit-text-decoration-color:red;text-decoration-color:green}"
    (opt ".a{-webkit-text-decoration-color:red;text-decoration-color:green}");
  Alcotest.(check string)
    "differing importance keeps the prefix"
    ".a{-moz-box-sizing:border-box!important;box-sizing:border-box}"
    (opt ".a{-moz-box-sizing:border-box!important;box-sizing:border-box}")

(* A colour-valued property folds its colour whatever its name: CIE Lab
   L=1.90334 a=0.278696 b=-5.48866 is sRGB 3,7,18, so both spellings print the
   same hex. A property left out of the fold reports the two as different
   values, which is what the browser-differential harness then counts as a
   render change. *)
let test_color_property_folds () =
  let opt css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun property ->
      let rule value = String.concat "" [ ".a{"; property; ":"; value; "}" ] in
      Alcotest.(check string)
        (String.concat "" [ property; " folds lab() to hex" ])
        (rule "#030712")
        (opt (rule "lab(1.90334 0.278696 -5.48866)"));
      Alcotest.(check string)
        (String.concat "" [ property; " folds rgb() to hex" ])
        (rule "#030712")
        (opt (rule "rgb(3, 7, 18)")))
    [
      "color";
      "column-rule-color";
      "-webkit-text-fill-color";
      "-webkit-text-stroke-color";
    ]

let test_lossless_declaration_order () =
  let opt ?(lossless = false) css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* Both modes keep authored declaration order. [--lossless] narrows the
     transforms allowed by minification; it does not opt into a CSSOM-visible
     canonical order. *)
  Alcotest.(check string)
    "default keeps source order" ".a{width:1px;color:red}"
    (opt ".a{width:1px;color:red}");
  Alcotest.(check string)
    "lossless keeps authored order" ".a{width:1px;color:red}"
    (opt ~lossless:true ".a{width:1px;color:red}");
  Alcotest.(check string)
    "lossless keeps reverse authored order" ".a{color:red;width:1px}"
    (opt ~lossless:true ".a{color:red;width:1px}");
  (* Overlapping declarations keep their relative order (cascade-significant):
     border-top before border-color decides the top colour. *)
  Alcotest.(check string)
    "overlapping declarations keep order"
    ".a{border-top:1px solid red;border-color:#00f}"
    (opt ~lossless:true ".a{border-top:1px solid red;border-color:blue}")

(* CSS Color 4 sec. 5 writes the alpha of the hex forms as a byte, so a hex
   spelling exists only where the alpha IS one. [--lossless] therefore folds an
   alpha that lands on a whole byte and leaves the rest functional: .6 is 153
   and folds, .5 is 127.5 and does not. Rounding it would emit a different
   colour under the mode whose whole promise is that it does not. *)
let test_lossless_alpha_folds_only_on_a_whole_byte () =
  let opt css =
    match Css.of_string css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string) input expected (opt input))
    [
      (* Whole bytes: 153, 51, 204, 255. *)
      (".a{color:rgb(0 0 0/.6)}", ".a{color:#0009}");
      (".a{color:rgb(0 0 0/.2)}", ".a{color:#0003}");
      (".a{color:rgb(0 0 0/.8)}", ".a{color:#000c}");
      (".a{color:rgb(0 0 0/1)}", ".a{color:#000}");
      (* 127.5 is not a byte, so the functional spelling stays. *)
      (".a{color:rgb(0 0 0/.5)}", ".a{color:rgb(0 0 0/.5)}");
      (".a{color:rgb(0 0 0/.3)}", ".a{color:rgb(0 0 0/.3)}");
      (* A percentage alpha is the same question asked of a second arm, and 100%
         of 255 is a byte where 33% of it is 84.15. The percentage itself prints
         as the shorter <number> CSS Color 4 sec. 4.1 makes equal to it. *)
      (".a{color:rgb(0 0 0/100%)}", ".a{color:#000}");
      (".a{color:rgb(0 0 0/20%)}", ".a{color:#0003}");
      (".a{color:rgb(0 0 0/33%)}", ".a{color:rgb(0 0 0/.33)}");
      (".a{color:rgb(0 0 0/1%)}", ".a{color:rgb(0 0 0/.01)}");
    ]

let test_lossless_keeps_unknown_property_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* A property with no typed spelling still writes cascade slots, so the
     canonical order must not move it past a shorthand that resets it:
     [background] resets the X position the longhand set. *)
  Alcotest.(check string)
    "an unknown longhand stays after its shorthand"
    ".a{background:red;background-position-x:10px}"
    (opt ".a{background:red;background-position-x:10px}");
  (* [grid-row-gap] is the legacy alias of [row-gap]; moving it after [gap]
     would make the row gap 9px instead of 1px. *)
  Alcotest.(check string)
    "a legacy alias stays before the shorthand that resets it"
    ".a{grid-row-gap:9px;gap:1px}"
    (opt ".a{grid-row-gap:9px;gap:1px}");
  (* A typed longhand whose value defeats the typed reader is recovered as an
     unknown property under its own name, and is just as order-dependent. *)
  Alcotest.(check string)
    "a recovered typed longhand stays after its shorthand"
    ".a{margin:0;margin-top:var(--a) var(--b)}"
    (opt ".a{margin:0;margin-top:var(--a) var(--b)}")

(* A typed shorthand and the typed longhands it resets write common cascade
   slots, so the canonical order has to keep them in source order. Each case is
   one family whose two declarations render differently when swapped; the
   expected string is the source order with value normalisation applied. *)
let shorthand_longhand_order_cases =
  [
    ("gap", ".a{row-gap:9px;gap:1px}", ".a{row-gap:9px;gap:1px}");
    ( "animation",
      ".a{animation:x 1s;animation-duration:2s}",
      ".a{animation:x 1s;animation-duration:2s}" );
    ( "animation-range",
      ".a{animation-range:normal;animation-range-start:10%}",
      ".a{animation-range:normal;animation-range-start:10%}" );
    ( "outline",
      ".a{outline:1px solid red;outline-color:blue}",
      ".a{outline:1px solid red;outline-color:#00f}" );
    ( "grid",
      ".a{grid:auto/auto;grid-template-columns:1fr}",
      ".a{grid:auto/auto;grid-template-columns:1fr}" );
    ( "grid-template",
      ".a{grid-template:auto/auto;grid-template-areas:\"a\"}",
      ".a{grid-template:auto/auto;grid-template-areas:\"a\"}" );
    ( "grid-row",
      ".a{grid-row:1/2;grid-row-start:3}",
      ".a{grid-row:1/2;grid-row-start:3}" );
    ( "place-content",
      ".a{place-content:center;align-content:start}",
      ".a{place-content:center;align-content:start}" );
    ( "overflow",
      ".a{overflow:hidden;overflow-x:visible}",
      ".a{overflow:hidden;overflow-x:visible}" );
    ( "border-radius",
      ".a{border-top-left-radius:8px;border-radius:4px}",
      ".a{border-top-left-radius:8px;border-radius:4px}" );
    ( "border resets border-image",
      ".a{border:1px solid red;border-image-source:url(a)}",
      ".a{border:1px solid red;border-image-source:url(a)}" );
    ( "border-image",
      ".a{border-image:none;border-image-repeat:round}",
      ".a{border-image:none;border-image-repeat:round}" );
    ( "columns",
      ".a{columns:2;column-width:10em}",
      ".a{columns:2;column-width:10em}" );
    ( "column-rule",
      ".a{column-rule:1px solid red;column-rule-color:green}",
      ".a{column-rule:1px solid red;column-rule-color:green}" );
    ( "list-style",
      ".a{list-style:none;list-style-type:disc}",
      ".a{list-style:none;list-style-type:disc}" );
    ( "text-decoration",
      ".a{text-decoration:underline;text-decoration-color:red}",
      ".a{text-decoration:underline;text-decoration-color:red}" );
    ( "text-decoration-skip",
      ".a{text-decoration-skip:none;text-decoration-skip-ink:auto}",
      ".a{text-decoration-skip:none;text-decoration-skip-ink:auto}" );
    ( "text-emphasis",
      ".a{text-emphasis:dot;text-emphasis-color:red}",
      ".a{text-emphasis:dot;text-emphasis-color:red}" );
    ( "font resets font-language-override",
      ".a{font:12px a;font-language-override:normal}",
      ".a{font:12px a;font-language-override:normal}" );
    ( "font resets font-palette",
      ".a{font:12px a;font-palette:dark}",
      ".a{font:12px a;font-palette:dark}" );
    ( "font-synthesis",
      ".a{font-synthesis:none;font-synthesis-weight:auto}",
      ".a{font-synthesis:none;font-synthesis-weight:auto}" );
    ( "contain-intrinsic-size",
      ".a{contain-intrinsic-width:20px;contain-intrinsic-size:10px}",
      ".a{contain-intrinsic-width:20px;contain-intrinsic-size:10px}" );
    ( "scroll-margin",
      ".a{scroll-margin:1px;scroll-margin-top:2px}",
      ".a{scroll-margin:1px;scroll-margin-top:2px}" );
    ( "scroll-padding",
      ".a{scroll-padding:1px;scroll-padding-left:2px}",
      ".a{scroll-padding:1px;scroll-padding-left:2px}" );
    ( "overscroll-behavior",
      ".a{overscroll-behavior:auto;overscroll-behavior-x:contain}",
      ".a{overscroll-behavior:auto;overscroll-behavior-x:contain}" );
    (* CSS Multicol 2 sec. 4.5: the shorthand resets the height and the wrap, so
       synthesising it over a rule that writes either would drop it. *)
    ( "columns",
      ".a{column-height:5em;column-width:10em;column-count:2}",
      ".a{column-height:5em;column-width:10em;column-count:2}" );
    ( "container",
      ".a{container:a/size;container-type:normal}",
      ".a{container:a/size;container-type:normal}" );
    (* CSS Scroll Animations 1 sec. 4.2 makes [block] the axis's initial, so the
       shorthand names it by leaving the component out. *)
    ( "scroll-timeline",
      ".a{scroll-timeline:--a block;scroll-timeline-axis:inline}",
      ".a{scroll-timeline:--a;scroll-timeline-axis:inline}" );
    ( "view-timeline",
      ".a{view-timeline:--a block;view-timeline-axis:inline}",
      ".a{view-timeline:--a;view-timeline-axis:inline}" );
    ("caret", ".a{caret:red;caret-shape:bar}", ".a{caret:red;caret-shape:bar}");
    ( "text-box",
      ".a{text-box:trim-both cap alphabetic;text-box-edge:auto}",
      ".a{text-box:trim-both cap alphabetic;text-box-edge:auto}" );
    ( "text-wrap",
      ".a{text-wrap:balance;text-wrap-mode:nowrap}",
      ".a{text-wrap:balance;text-wrap-mode:nowrap}" );
    ( "white-space resets text-wrap-mode",
      ".a{white-space:pre;text-wrap-mode:wrap}",
      ".a{white-space:pre;text-wrap-mode:wrap}" );
    ( "interest-delay",
      ".a{interest-delay:1s;interest-delay-start:2s}",
      ".a{interest-delay:1s;interest-delay-start:2s}" );
    ( "position-try",
      ".a{position-try:--a;position-try-order:most-width}",
      ".a{position-try:--a;position-try-order:most-width}" );
  ]

let test_lossless_keeps_shorthand_longhand_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (opt input))
    shorthand_longhand_order_cases

(* The same relation across rules. Two rules writing one colour tempt the
   optimizer to share a selector list, which puts the later one where the
   earlier one sits; a rule writing the shorthand in between resets that colour,
   so an element matching both would repaint. *)
let test_shorthand_longhand_rule_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  Alcotest.(check string)
    "a colour rule does not cross the shorthand that resets it"
    ".a{column-rule-color:green}.b{column-rule:1px solid \
     red}.c{column-rule-color:green}"
    (opt
       ".a{column-rule-color:green}.b{column-rule:1px solid \
        red}.c{column-rule-color:green}")

(* CSS Logical 1 sec. 2: a flow-relative longhand resolves to a physical side
   the writing mode picks, so a sheet on its own cannot say whether
   [margin-inline-start] and [margin-left] name one slot or two. The canonical
   order therefore has to keep such a pair in source order, in both directions.
   The first three cases are corpus tests duplicates/0015-0017, each of which a
   browser renders differently once the pair is swapped. *)
let logical_physical_order_cases =
  [
    ( "border-top before border-block",
      ".a{border-top:1px solid red;border-block:2px solid red}",
      ".a{border-top:1px solid red;border-block:2px solid red}" );
    ( "border-block before border-top",
      ".a{border-block:2px solid red;border-top:1px solid red}",
      ".a{border-block:2px solid red;border-top:1px solid red}" );
    ( "margin-left before margin-inline-start",
      ".a{margin-left:10px;margin-inline-start:20px}",
      ".a{margin-left:10px;margin-inline-start:20px}" );
    ( "margin-inline-start before margin-left",
      ".a{margin-inline-start:20px;margin-left:10px}",
      ".a{margin-inline-start:20px;margin-left:10px}" );
    ( "padding-top before padding-block-start",
      ".a{padding-top:10px;padding-block-start:20px}",
      ".a{padding-top:10px;padding-block-start:20px}" );
    ( "padding-block-start before padding-top",
      ".a{padding-block-start:20px;padding-top:10px}",
      ".a{padding-block-start:20px;padding-top:10px}" );
    (* The inline axis is the horizontal one in [horizontal-tb] and the vertical
       one in a vertical mode, so a logical border width aliases whichever
       physical side the mode gives it. *)
    ( "border-left-width before border-inline-width",
      ".a{border-left-width:1px;border-inline-width:2px}",
      ".a{border-left-width:1px;border-inline-width:2px}" );
    ( "border-inline-width before border-left-width",
      ".a{border-inline-width:2px;border-left-width:1px}",
      ".a{border-inline-width:2px;border-left-width:1px}" );
    (* A logical side also aliases into the physical shorthand that covers every
       side. *)
    ( "border before border-block",
      ".a{border:1px solid red;border-block:2px solid red}",
      ".a{border:1px solid red;border-block:2px solid red}" );
    ( "top before inset-inline-start",
      ".a{top:1px;inset-inline-start:5px}",
      ".a{top:1px;inset-inline-start:5px}" );
    ( "scroll-margin-top before scroll-margin-block-start",
      ".a{scroll-margin-top:1px;scroll-margin-block-start:2px}",
      ".a{scroll-margin-top:1px;scroll-margin-block-start:2px}" );
    (* CSS Logical 1 sec. 4.4: the sizing family aliases the same way, with no
       physical shorthand covering the two axes. *)
    ( "width before inline-size",
      ".a{width:10px;inline-size:20px}",
      ".a{width:10px;inline-size:20px}" );
    ( "inline-size before width",
      ".a{inline-size:20px;width:10px}",
      ".a{inline-size:20px;width:10px}" );
    ( "height before block-size",
      ".a{height:10px;block-size:20px}",
      ".a{height:10px;block-size:20px}" );
    ( "min-width before min-inline-size",
      ".a{min-width:10px;min-inline-size:20px}",
      ".a{min-width:10px;min-inline-size:20px}" );
    ( "max-width before max-inline-size",
      ".a{max-width:10px;max-inline-size:20px}",
      ".a{max-width:10px;max-inline-size:20px}" );
    (* CSS Logical 1 sec. 4.3: a flow-relative corner resolves to whichever
       physical corner the writing mode and the direction give it. *)
    ( "border-top-left-radius before border-start-start-radius",
      ".a{border-top-left-radius:1px;border-start-start-radius:2px}",
      ".a{border-top-left-radius:1px;border-start-start-radius:2px}" );
    ( "border-start-start-radius before border-top-left-radius",
      ".a{border-start-start-radius:2px;border-top-left-radius:1px}",
      ".a{border-start-start-radius:2px;border-top-left-radius:1px}" );
    ( "contain-intrinsic-width before contain-intrinsic-inline-size",
      ".a{contain-intrinsic-width:1px;contain-intrinsic-inline-size:2px}",
      ".a{contain-intrinsic-width:1px;contain-intrinsic-inline-size:2px}" );
    ( "overscroll-behavior-x before overscroll-behavior-inline",
      ".a{overscroll-behavior-x:none;overscroll-behavior-inline:contain}",
      ".a{overscroll-behavior-x:none;overscroll-behavior-inline:contain}" );
  ]

let test_lossless_keeps_logical_physical_order () =
  let opt css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize ~lossless:true p.stylesheet)
        |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  List.iter
    (fun (name, input, expected) ->
      Alcotest.(check string) name expected (opt input))
    logical_physical_order_cases

(* Regrouping - factoring a shared declaration into a selector list, and
   synthesising nesting from a run of adjacent rules - depends on how the input
   happened to order its rules: a rule sitting between two others decides
   whether either applies. A canonical projection therefore turns regrouping
   off, so the same stylesheet written either way maps to one form. *)
(* Eliminating a non-adjacent duplicate declaration has to read the importance
   flag: with an [!important] earlier write in play, the later normal write is
   the dead one, not the winner. Dropping the wrong one changes what [.a]
   computes to, and it is the failure mode a peer minifier ships (Lightning CSS
   1.0.0-alpha.71 emits [.b{color:green}.a{color:#00f}] here, so [.a] resolves
   to blue rather than red). *)
let important_survives_non_adjacent_duplicate () =
  let out src =
    Css.to_string ~minify:true (Css.optimize (Css.stylesheet src))
  in
  Alcotest.(check string)
    "the !important winner is kept and the dead write dropped"
    ".a{color:red!important}.b{color:green}"
    (out ".a{color:red!important}.b{color:green}.a{color:blue}");
  (* The mirror image: when the later write carries the flag it is the winner,
     so the earlier one is what goes. *)
  Alcotest.(check string)
    "a later !important wins over an earlier normal write"
    ".b{color:green}.a{color:#00f!important}"
    (out ".a{color:red}.b{color:green}.a{color:blue!important}");
  (* Neither carries the flag, so the plain last-wins rule applies. *)
  Alcotest.(check string)
    "without !important the later write wins" ".b{color:green}.a{color:#00f}"
    (out ".a{color:red}.b{color:green}.a{color:blue}");
  (* Both carry it, so last-wins applies among them. *)
  Alcotest.(check string)
    "between two !important writes the later wins"
    ".b{color:green}.a{color:#00f!important}"
    (out ".a{color:red!important}.b{color:green}.a{color:blue!important}")

let regrouping_can_be_disabled () =
  let src =
    ".text-xs{font-size:var(--text-xs);line-height:1}.text-xs\\/4{font-size:var(--text-xs);line-height:4}.text-xs\\/5{font-size:var(--text-xs);line-height:5}.text-xs\\/6{font-size:var(--text-xs);line-height:6}.text-xs\\/7{font-size:var(--text-xs);line-height:7}"
  in
  let sheet = Css.stylesheet src in
  let out ?regroup () =
    Css.to_string ~minify:true (Css.optimize ?regroup sheet)
  in
  Alcotest.(check bool)
    "regrouping on lifts the shared declaration" true
    (Astring.String.is_infix ~affix:".text-xs,.text-xs\\/4" (out ()));
  Alcotest.(check bool)
    "regrouping off leaves each rule alone" false
    (Astring.String.is_infix ~affix:".text-xs,.text-xs\\/4"
       (out ~regroup:false ()))

(* A guard every judged target satisfies holds wherever its enclosing block
   does, so its rules apply exactly where they sit: the rules before it, inside
   it and after it are one run, and the adjacency the guard hid collapses. *)
let held_guard_joins_the_run () =
  let out src =
    Css.to_string ~minify:true
      (Css.optimize ~judge:Css.Optimize.evergreen_targets (Css.stylesheet src))
  in
  let guard = "@supports (color:color-mix(in lab,red,red))" in
  Alcotest.(check string)
    "the rules on both sides of the guard fold with the one inside"
    ".a{background:#00f;color:red;margin:0}"
    (out (".a{color:red}" ^ guard ^ "{.a{background:blue}}.a{margin:0}"));
  Alcotest.(check string)
    "one guard after another joins the same run"
    ".a{background:#00f;color:red;margin:0}.b{color:green;padding:0}"
    (out
       (".a{color:red}" ^ guard ^ "{.a{background:blue}}" ^ guard
      ^ "{.a{margin:0}}.b{color:green}" ^ guard ^ "{.b{padding:0}}"));
  Alcotest.(check string)
    "a guard the context leaves open keeps its block"
    ".a{color:red}@supports(display:grud){.a{background:#00f}}"
    (out ".a{color:red}@supports (display:grud){.a{background:blue}}")

(* The other regrouping pass: two adjacent rules sharing a selector prefix
   become one nested rule, and whether they are adjacent is exactly what an
   unrelated rule between them decides. *)
let nesting_synthesis_can_be_disabled () =
  let sheet =
    Css.stylesheet
      ".prose:where(:not(.not-prose,.not-prose \
       *)){color:red;font-size:1rem}.prose:where(:not(.not-prose,.not-prose \
       *)):where(.dark,.dark *){color:blue;font-size:2rem}"
  in
  let out ?regroup () =
    Css.to_string ~minify:true (Css.optimize ?regroup sheet)
  in
  Alcotest.(check bool)
    "regrouping on nests the second rule" true
    (Astring.String.is_infix ~affix:"&:where(.dark,.dark *)" (out ()));
  Alcotest.(check bool)
    "regrouping off keeps both flat" false
    (Astring.String.is_infix ~affix:"&:where(.dark,.dark *)"
       (out ~regroup:false ()))

let flatten_nesting_is_an_output_invariant () =
  let out css =
    Css.stylesheet css
    |> Css.optimize ~flatten_nesting:true
    |> Css.to_string ~minify:true
  in
  let expected =
    ".long-component-name{color:red}.long-component-name:hover{color:#00f}"
  in
  Alcotest.(check string)
    "flat input stays flat after nesting synthesis" expected
    (out ".long-component-name{color:red}.long-component-name:hover{color:blue}");
  Alcotest.(check string)
    "authored nesting is flat in the final output" expected
    (out ".long-component-name{color:red;&:hover{color:blue}}")

(* CSS Fragmentation 3 sec. 3.4 makes [page-break-*] the same property as its
   [break-*] alias, so two [page-break-*] declarations in one rule are two
   declarations of one property and the later one shadows the earlier, exactly
   as [break-*] and every other property does. A [var()] on the left does not
   change whose property it is. *)
let page_break_var_shadows_like_its_alias () =
  List.iter
    (fun (input, expected) ->
      Alcotest.(check string)
        input expected
        (Css.stylesheet ~strict:false input |> minify))
    [
      (* The modern spelling sets the oracle. *)
      (".a{break-after:var(--x);break-after:page}", ".a{break-after:page}");
      (".a{break-after:page;break-after:var(--x)}", ".a{break-after:var(--x)}");
      (".a{break-inside:var(--x);break-inside:avoid}", ".a{break-inside:avoid}");
      (* The legacy spelling answers the same. *)
      ( ".a{page-break-after:var(--x);page-break-after:always}",
        ".a{break-after:page}" );
      ( ".a{page-break-after:always;page-break-after:var(--x)}",
        ".a{page-break-after:var(--x)}" );
      ( ".a{page-break-before:var(--x);page-break-before:left}",
        ".a{break-before:left}" );
      ( ".a{page-break-inside:var(--x);page-break-inside:avoid}",
        ".a{break-inside:avoid}" );
      ( ".a{page-break-inside:avoid;page-break-inside:var(--x)}",
        ".a{page-break-inside:var(--x)}" );
    ]

(* CSS Syntax 3 sec. 5.4.2 keeps an unrecognised at-rule's body as raw source
   text, so nothing in it is typed and the printer cannot touch it: rewriting
   the body changes the AST, which is the optimizer's job and not the
   serializer's. The optimizer reads the body as a component-value stream and
   writes it back with the separator rules that already serve custom-property
   streams, so token boundaries, strings, escapes and nested blocks survive
   while the layout between them goes.

   [--minify] on its own leaves the body alone, since it may not change what the
   AST holds. *)
let unknown_at_rule_body_is_compacted () =
  let parse css =
    match Css.of_string ~strict:false css with
    | Ok p -> p.stylesheet
    | Error _ -> Alcotest.failf "parse failed: %s" css
  in
  let optimized css = Css.to_string ~minify:true (Css.optimize (parse css)) in
  let minified css = Css.to_string ~minify:true (parse css) in
  let compacts name css expected =
    Alcotest.(check string) name expected (optimized css);
    Alcotest.(check string)
      (name ^ ": optimizing again changes nothing")
      expected (optimized expected)
  in
  compacts "a nested rule in an opaque body loses its layout"
    "@foo{ .a { color: red } }" "@foo{.a{color:red}}";
  compacts "a string keeps every byte between its quotes"
    "@foo{ a: \"  b  c  \" }" "@foo{a:\"  b  c  \"}";
  compacts "an escape spelling an ident is written as that ident"
    "@foo{ \\41 b }" "@foo{Ab}";
  compacts "an escape the ident needs is kept" "@foo{ a\\ b }" "@foo{a\\ b}";
  compacts "two idents keep the boundary between them" "@foo{ a   b }"
    "@foo{a b}";
  compacts "an ident before a paren keeps the space that stops a function token"
    "@foo{ a ( b  c ) [ d ] }" "@foo{a (b c)[d]}";
  (* [--minify] alone may not rewrite the AST, so the body travels verbatim. *)
  Alcotest.(check string)
    "minify alone leaves the opaque body as authored"
    "@foo{ .a { color: red } }"
    (minified "@foo{ .a { color: red } }")

(* A shorthand resets every longhand of its family, so one written before it is
   dead and minify drops it. The two families here are the ones whose reset
   table entry no other test reaches: their longhands survive a mutation of the
   table and the suite stays green. *)
(* CSS Backgrounds 3 (ED) sec. 3.4: "The border shorthand also resets
   border-image to its initial value." Contracting the border longhands beside a
   border-image longhand would reset the rest of that family, which the
   longhands the rule wrote did not. Each border-image longhand carries its own
   hazard bit for this: sharing one makes a rule holding the slice answer for a
   neighbour holding the source. *)
let test_border_keeps_longhands_beside_a_border_image () =
  let minify_str css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* A NEIGHBOUR holding one is the hazard, and each longhand needs its OWN bit
     for it: with the source and slice sharing one, the rule holding the slice
     answers for the rule holding the source and both contract. *)
  Alcotest.(check string)
    "the border longhands stay where the family is spread over four rules"
    "c,f{border-width:1px;border-style:solid;border-color:red}a,f{border-image-slice:2}b,c{border-image-source:url(x.png)}"
    (minify_str
       "a{border-image-slice:2}c{border-image-source:url(x.png);border-width:1px;border-style:solid;border-color:red}b{border-image-source:url(x.png)}f{border-image-slice:2;border-width:1px;border-style:solid;border-color:red}");
  (* With no border-image longhand anywhere there is nothing to reset, so the
     contraction is the shorter spelling of the same cascade. *)
  Alcotest.(check string)
    "and contracts with none of them present" "a{border:1px solid red}"
    (minify_str "a{border-width:1px;border-style:solid;border-color:red}")

let test_shorthand_drops_the_longhand_it_resets () =
  let minify_str css =
    match Css.of_string ~strict:false css with
    | Ok p ->
        Css.to_string ~minify:true (Css.optimize p.stylesheet) |> String.trim
    | Error _ -> Alcotest.fail "parse"
  in
  (* CSS Transitions 1 (ED) sec. 2.5: the shorthand sets all four longhands, so
     [transition: 1s] gives transition-property its initial [all] and the
     earlier declaration cannot be seen. *)
  Alcotest.(check string)
    "transition resets the property longhand before it" "a{transition:all 1s}"
    (minify_str "a{transition-property:opacity;transition:1s}");
  Alcotest.(check string)
    "and keeps one written after it"
    "a{transition:all 1s;transition-property:opacity}"
    (minify_str "a{transition:1s;transition-property:opacity}");
  (* CSS Fonts 4 (ED) sec. 6.10: font-variant resets its seven longhands, of
     which font-variant-ligatures is one. *)
  Alcotest.(check string)
    "font-variant resets the ligatures longhand before it"
    "a{font-variant:normal}"
    (minify_str "a{font-variant-ligatures:none;font-variant:normal}");
  Alcotest.(check string)
    "and keeps one written after it"
    "a{font-variant:normal;font-variant-ligatures:none}"
    (minify_str "a{font-variant:normal;font-variant-ligatures:none}")

(* CSS Syntax 3 sec. 5.4.4/5.4.5: a declaration inside a nested block runs to
   the next [;] or to the block's [}], so a declaration followed by a nested
   at-rule or nested rule owes it a separator; without one the declaration runs
   on into the next sibling and the reader drops the rule. [Css.of_string
   ~strict:true] is the gate: it turns any recovery warning into an error. *)
let nested_block_separator () =
  let check name input expected =
    let output = optimized_string input in
    Alcotest.(check string) name expected output;
    match Css.of_string ~strict:true output with
    | Ok _ -> ()
    | Error e ->
        Alcotest.failf "%s: emitted CSS %S is rejected by the reader: %s" name
          output (Error.to_string e)
  in
  check "adjacent same-condition blocks merge"
    ".a{@supports (color:red){color:red}@supports (color:red){top:0}}"
    ".a{@supports(color:red){color:red;top:0}}";
  check "three blocks in a row"
    ".a{@supports (color:red){color:red}@supports (color:red){top:0}@supports \
     (color:red){left:0}}"
    ".a{@supports(color:red){color:red;top:0;left:0}}";
  check "nested at-rule before a nested rule"
    ".a{@supports (color:red){color:red}.b{top:0}}"
    ".a{@supports(color:red){color:red}.b{top:0}}";
  check "two nested at-rules"
    ".a{@supports (color:red){color:red}@media print{top:0}}"
    ".a{@supports(color:red){color:red}@media print{top:0}}";
  check "declaration then nested at-rule then nested rule"
    ".a{top:0;@supports (color:red){color:red}.b{left:0}}"
    ".a{top:0;@supports(color:red){color:red}.b{left:0}}"

let target_minify_enforce_spec_split () =
  let check_modes name input ~default ~spec =
    Alcotest.(check string) (name ^ " default") default (optimized_string input);
    Alcotest.(check string)
      (name ^ " enforce-spec") spec
      (optimized_string ~enforce_spec:true input)
  in
  check_modes "explicit flex zero basis is not one-value shorthand"
    "a { flex: 1 1 0 }" ~default:"a{flex:1 1 0}" ~spec:"a{flex:1 1 0}";
  check_modes "color-mix keeps the required in oklab in both modes"
    "a { color: color-mix(in oklab, var(--a), var(--b)) }"
    ~default:"a{color:color-mix(in oklab,var(--a),var(--b))}"
    ~spec:"a{color:color-mix(in oklab,var(--a),var(--b))}";
  check_modes "computed oklch lightness keeps its numeric form"
    "a { color: color-mix(in oklch, red 50%, blue 50%) }"
    ~default:"a{color:oklch(.54 .285 326.643)}"
    ~spec:"a{color:oklch(.54 .285 326.643)}";
  (* oklch/oklab chroma takes <number> | <percentage>, exactly interchangeable
     (CSS Color 4: 100% = 0.4 on this axis). Default minify picks the shorter
     spelling per axis - .304 becomes 76%, but .1 stays because 25% is longer.
     --enforce-spec emits the spec-canonical resolved form, which is the number,
     and never introduces a percentage even when it would be shorter. *)
  check_modes "oklch chroma uses the shorter percentage on the default target"
    "a { color: oklch(.5 .304 200) }" ~default:"a{color:oklch(.5 76%200)}"
    ~spec:"a{color:oklch(.5 .304 200)}";
  check_modes
    "oklch chroma keeps the number when the number is already shortest"
    "a { color: oklch(.5 .1 200) }" ~default:"a{color:oklch(.5 .1 200)}"
    ~spec:"a{color:oklch(.5 .1 200)}";
  check_modes "enforce-spec renders oklch chroma as the canonical number"
    "a { color: oklch(.5 76% 200) }" ~default:"a{color:oklch(.5 76%200)}"
    ~spec:"a{color:oklch(.5 .304 200)}";
  check_modes "media min-width grammar"
    "@media (min-width: 700px) { a { color: red } }"
    ~default:"@media(width>=700px){a{color:red}}"
    ~spec:"@media(min-width:700px){a{color:red}}";
  check_modes "media not all min-width grammar"
    "@media not all and (min-width: 700px) { a { color: red } }"
    ~default:"@media(width<700px){a{color:red}}"
    ~spec:"@media not all and (min-width:700px){a{color:red}}";
  check_modes "media interval grammar"
    "@media (min-width: 768px) and (max-width: 1024px) { a { color: red } }"
    ~default:"@media(768px<=width<=1024px){a{color:red}}"
    ~spec:"@media(min-width:768px)and (max-width:1024px){a{color:red}}";
  check_modes "container min-width grammar"
    "@container sidebar (min-width: 700px) { a { color: red } }"
    ~default:"@container sidebar (width>=700px){a{color:red}}"
    ~spec:"@container sidebar (min-width:700px){a{color:red}}"

let target_evergreen_compatibility_prefixes () =
  Alcotest.(check string)
    "the declared target adds WebKit declaration fallbacks"
    ".a{-webkit-user-select:none;user-select:none;-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}"
    (optimized_string ".a{user-select:none;backdrop-filter:blur(1px)}");
  Alcotest.(check string)
    "the declared target adds WebKit hyphens and mask longhand fallbacks"
    ".a{-webkit-hyphens:auto;hyphens:auto;-webkit-mask-image:none;mask-image:none;-webkit-mask-position:50%;mask-position:50%;-webkit-mask-size:cover;mask-size:cover;-webkit-mask-repeat:no-repeat;mask-repeat:no-repeat;-webkit-mask-clip:border-box;mask-clip:border-box;-webkit-mask-origin:content-box;mask-origin:content-box}"
    (optimized_string
       ".a{hyphens:auto;mask-image:none;mask-position:center;mask-size:cover;mask-repeat:no-repeat;mask-clip:border-box;mask-origin:content-box}");
  Alcotest.(check string)
    "the declared target adds the WebKit mask shorthand"
    ".a{-webkit-mask:none;mask:none}"
    (optimized_string ".a{mask:none}");
  Alcotest.(check string)
    "the WebKit mask shorthand omits fields with different legacy grammars"
    (* [add] is the composite's initial (CSS Masking 1 sec. 7.8), which is what
       leaving the component out names, so the shorthand drops it. *)
    ".a{-webkit-mask:url(a.svg);mask:url(a.svg)luminance}"
    (optimized_string ".a{mask:url(a.svg) luminance add}");
  Alcotest.(check string)
    "mask-mode and mask-composite take WebKit's own vocabulary"
    ".a{-webkit-mask-source-type:luminance;mask-mode:luminance;-webkit-mask-composite:source-in;mask-composite:intersect}"
    (optimized_string ".a{mask-mode:luminance;mask-composite:intersect}");
  Alcotest.(check string)
    "a mask layer list or substitution has no WebKit spelling"
    ".a{mask-mode:alpha,luminance;mask-composite:var(--c)}"
    (optimized_string ".a{mask-mode:alpha,luminance;mask-composite:var(--c)}");
  Alcotest.(check string)
    "the declaration fallback preserves importance"
    ".a{-webkit-user-select:none!important;user-select:none!important}"
    (optimized_string ".a{user-select:none!important}");
  Alcotest.(check string)
    "the declared target prefixes feature tests and their declarations"
    "@supports(-webkit-backdrop-filter:var(--tw))or \
     (backdrop-filter:var(--tw)){.a{-webkit-backdrop-filter:var(--tw);backdrop-filter:var(--tw)}}"
    (optimized_string
       "@supports(backdrop-filter:var(--tw)){.a{backdrop-filter:var(--tw)}}");
  Alcotest.(check string)
    "the declared target prefixes mask feature tests and declarations"
    "@supports(-webkit-mask:none)or \
     (mask:none){.a{-webkit-mask:none;mask:none}}"
    (optimized_string "@supports(mask:none){.a{mask:none}}");
  Alcotest.(check string)
    "an authored fallback is not duplicated"
    ".a{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}"
    (optimized_string
       ".a{-webkit-backdrop-filter:blur(1px);backdrop-filter:blur(1px)}");
  Alcotest.(check string)
    "an authored mask fallback is not duplicated"
    ".a{-webkit-mask-size:contain;mask-size:cover}"
    (optimized_string ".a{-webkit-mask-size:contain;mask-size:cover}");
  Alcotest.(check string)
    "a synthesized mask shorthand preserves importance"
    ".a{-webkit-mask:none!important;mask:none!important}"
    (optimized_string ".a{mask:none!important}");
  Alcotest.(check string)
    "an unresolved decoration color takes the WebKit spelling too"
    ".prose \
     a{text-decoration:underline;-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}"
    (optimized_string
       ".prose{a{text-decoration:underline;text-decoration-color:var(--c)}}");
  Alcotest.(check string)
    "a decoration color read as a colour keeps the standard property alone"
    ".a{text-decoration-color:#00f}"
    (optimized_string ".a{text-decoration-color:blue}");
  Alcotest.(check string)
    "the decoration color fallback preserves importance"
    ".a{-webkit-text-decoration-color:var(--c)!important;text-decoration-color:var(--c)!important}"
    (optimized_string ".a{text-decoration-color:var(--c)!important}");
  Alcotest.(check string)
    "an authored decoration color fallback is not supplemented"
    ".a{-webkit-text-decoration-color:red;text-decoration-color:var(--c)}"
    (optimized_string
       ".a{-webkit-text-decoration-color:red;text-decoration-color:var(--c)}");
  Alcotest.(check string)
    "the declared target prefixes a decoration color feature test"
    "@supports(-webkit-text-decoration-color:var(--c))or \
     (text-decoration-color:var(--c)){.a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}}"
    (optimized_string
       "@supports(text-decoration-color:var(--c)){.a{text-decoration-color:var(--c)}}");
  Alcotest.(check string)
    "spec-only mode does not synthesize target fallbacks"
    ".a{user-select:none;backdrop-filter:blur(1px);hyphens:auto;mask:none}"
    (optimized_string ~enforce_spec:true
       ".a{user-select:none;backdrop-filter:blur(1px);hyphens:auto;mask:none}");
  let newer_webkit =
    {
      Css.Optimize.evergreen_targets with
      safari = (18, 0);
      ios_safari = (18, 0);
    }
  in
  Alcotest.(check string)
    "a newer WebKit target drops only the obsolete fallback"
    ".a{-webkit-user-select:none;user-select:none;backdrop-filter:blur(1px)}"
    (optimized_string ~targets:newer_webkit
       ".a{user-select:none;backdrop-filter:blur(1px)}");
  let safari_16_6 =
    {
      Css.Optimize.evergreen_targets with
      chrome = (120, 0);
      safari = (16, 6);
      ios_safari = (17, 0);
    }
  in
  Alcotest.(check string)
    "Safari 16.6 still receives the hyphens fallback"
    ".a{-webkit-hyphens:auto;hyphens:auto}"
    (optimized_string ~targets:safari_16_6 ".a{hyphens:auto}");
  let ios_safari_16_7 =
    {
      Css.Optimize.evergreen_targets with
      chrome = (120, 0);
      safari = (17, 0);
      ios_safari = (16, 7);
    }
  in
  Alcotest.(check string)
    "iOS Safari 16.7 still receives the hyphens fallback"
    ".a{-webkit-hyphens:auto;hyphens:auto}"
    (optimized_string ~targets:ios_safari_16_7 ".a{hyphens:auto}");
  let targets_past_hyphens_and_mask_prefixes =
    {
      Css.Optimize.evergreen_targets with
      chrome = (120, 0);
      safari = (17, 0);
      ios_safari = (17, 0);
    }
  in
  Alcotest.(check string)
    "targets past the compatibility boundaries omit hyphens and mask prefixes"
    ".a{hyphens:auto;mask-image:none}"
    (optimized_string ~targets:targets_past_hyphens_and_mask_prefixes
       ".a{hyphens:auto;mask-image:none}");
  let safari_26_1 =
    {
      Css.Optimize.evergreen_targets with
      safari = (26, 1);
      ios_safari = (26, 1);
    }
  in
  Alcotest.(check string)
    "Safari 26.1 still receives the decoration color fallback"
    ".a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}"
    (optimized_string ~targets:safari_26_1 ".a{text-decoration-color:var(--c)}");
  let ios_safari_26_1 =
    {
      Css.Optimize.evergreen_targets with
      safari = (26, 2);
      ios_safari = (26, 1);
    }
  in
  Alcotest.(check string)
    "iOS Safari 26.1 still receives the decoration color fallback"
    ".a{-webkit-text-decoration-color:var(--c);text-decoration-color:var(--c)}"
    (optimized_string ~targets:ios_safari_26_1
       ".a{text-decoration-color:var(--c)}");
  let targets_past_decoration_color_prefix =
    {
      Css.Optimize.evergreen_targets with
      safari = (26, 2);
      ios_safari = (26, 2);
    }
  in
  Alcotest.(check string)
    "targets past the decoration color boundary omit its fallback"
    ".a{text-decoration-color:var(--c)}"
    (optimized_string ~targets:targets_past_decoration_color_prefix
       ".a{text-decoration-color:var(--c)}")

let suite =
  ( "optimize_declarations",
    [
      ( "unknown at-rule body is compacted",
        `Quick,
        unknown_at_rule_body_is_compacted );
      ( "!important survives non-adjacent duplicate elimination",
        `Quick,
        important_survives_non_adjacent_duplicate );
      ("regrouping can be disabled", `Quick, regrouping_can_be_disabled);
      ("held guard joins the run", `Quick, held_guard_joins_the_run);
      ( "nesting synthesis can be disabled",
        `Quick,
        nesting_synthesis_can_be_disabled );
      ( "flatten nesting is an output invariant",
        `Quick,
        flatten_nesting_is_an_output_invariant );
      ( "a page-break var shadows like its alias",
        `Quick,
        page_break_var_shadows_like_its_alias );
      ("vendor prefix strip", `Quick, test_vendor_prefix_strip);
      ("vendor prefix baseline gate", `Quick, test_vendor_prefix_baseline_gate);
      ("color property folds", `Quick, test_color_property_folds);
      ("lossless declaration order", `Quick, test_lossless_declaration_order);
      ( "lossless alpha folds only on a whole byte",
        `Quick,
        test_lossless_alpha_folds_only_on_a_whole_byte );
      ( "lossless keeps unknown property order",
        `Quick,
        test_lossless_keeps_unknown_property_order );
      ( "shorthand drops the longhand it resets",
        `Quick,
        test_shorthand_drops_the_longhand_it_resets );
      ( "border keeps longhands beside a border-image",
        `Quick,
        test_border_keeps_longhands_beside_a_border_image );
      ( "lossless keeps shorthand longhand order",
        `Quick,
        test_lossless_keeps_shorthand_longhand_order );
      ( "shorthand longhand rule order",
        `Quick,
        test_shorthand_longhand_rule_order );
      ( "lossless keeps logical physical order",
        `Quick,
        test_lossless_keeps_logical_physical_order );
      ( "adjacent merge survives preflight skip",
        `Quick,
        test_adjacent_merge_survives_preflight_skip );
      ( "target minify and enforce-spec split",
        `Quick,
        target_minify_enforce_spec_split );
      ( "target evergreen compatibility prefixes",
        `Quick,
        target_evergreen_compatibility_prefixes );
      ( "a nested block keeps its declaration separator",
        `Quick,
        nested_block_separator );
    ] )
