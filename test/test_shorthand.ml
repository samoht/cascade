open Cascade
open Css_test_helpers

let decl css = Declaration.of_string css

let decls css =
  match Css.of_string css with
  | Ok { stylesheet; _ } -> (
      match Css.statements stylesheet with
      | [ Stylesheet.Rule r ] -> r.declarations
      | _ -> Alcotest.fail "expected one rule")
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

let decl_strings decls =
  List.map (Pp.to_string ~minify:true Declaration.pp_declaration) decls

let indexed decls = List.mapi (fun i d -> (i, d)) decls
let unindexed decls = List.map snd decls

let test_declaration_covers_reset_boundaries () =
  Alcotest.(check bool)
    "all covers ordinary property" true
    (Shorthand.declaration_covers (decl "all:initial") (decl "color:red"));
  Alcotest.(check bool)
    "all does not cover direction" false
    (Shorthand.declaration_covers (decl "all:initial") (decl "direction:ltr"));
  Alcotest.(check bool)
    "all does not cover custom properties" false
    (Shorthand.declaration_covers (decl "all:initial") (decl "--brand:red"));
  Alcotest.(check bool)
    "border covers side width" true
    (Shorthand.declaration_covers
       (decl "border:1px solid red")
       (decl "border-top-width:2px"))

let test_unknown_property_overlap () =
  (* A property with no typed spelling still writes cascade slots. Judging it
     disjoint from everything lets the canonical declaration order move it past
     a shorthand that resets it, which changes what the rule renders. *)
  Alcotest.(check bool)
    "an unknown longhand overlaps the shorthand that resets it" true
    (Shorthand.declarations_overlap (decl "background:red")
       (decl "background-position-x:10px"));
  Alcotest.(check bool)
    "the relation is symmetric" true
    (Shorthand.declarations_overlap
       (decl "background-position-x:10px")
       (decl "background:red"));
  (* [grid-row-gap] is the legacy alias of [row-gap], which [gap] resets. *)
  Alcotest.(check bool)
    "an unknown legacy alias overlaps the shorthand it aliases into" true
    (Shorthand.declarations_overlap (decl "grid-row-gap:9px") (decl "gap:1px"));
  (* Two names with no typed spelling can still be a shorthand/longhand pair:
     [grid-gap] is the legacy alias of [gap] and resets [grid-row-gap]. *)
  Alcotest.(check bool)
    "two unknown names may be a shorthand and its longhand" true
    (Shorthand.declarations_overlap (decl "grid-row-gap:9px")
       (decl "grid-gap:1px"));
  (* A typed longhand whose value defeats the typed reader is recovered under
     its own name, so the name is one the footprints know. *)
  Alcotest.(check bool)
    "a recovered typed longhand overlaps its shorthand" true
    (Shorthand.declarations_overlap (decl "margin:0")
       (decl "margin-top:var(--a) var(--b)"));
  (* A known longhand name outside the shorthand's footprint stays disjoint. *)
  Alcotest.(check bool)
    "a recovered longhand of another family is disjoint" false
    (Shorthand.declarations_overlap (decl "padding:0")
       (decl "margin-top:var(--a) var(--b)"));
  (* CSS Cascade 5 sec. 7.2: [all] resets unknown properties. *)
  Alcotest.(check bool)
    "all overlaps an unknown property" true
    (Shorthand.declarations_overlap (decl "all:initial")
       (decl "grid-row-gap:9px"));
  (* Custom properties are their own cascade slots. *)
  Alcotest.(check bool)
    "an unknown property and a custom property are disjoint" false
    (Shorthand.declarations_overlap (decl "grid-row-gap:9px")
       (decl "--brand:red"))

let test_vendor_alias_overlap () =
  (* A vendor-prefixed spelling is an alias of the unprefixed property, so the
     prefixed shorthand resets the unprefixed longhands. *)
  Alcotest.(check bool)
    "a prefixed animation shorthand overlaps an unprefixed longhand" true
    (Shorthand.declarations_overlap
       (decl "-webkit-animation:x 1s")
       (decl "animation-duration:2s"));
  Alcotest.(check bool)
    "a prefixed transition shorthand overlaps an unprefixed longhand" true
    (Shorthand.declarations_overlap
       (decl "-webkit-transition:all 1s")
       (decl "transition-duration:2s"));
  Alcotest.(check bool)
    "a prefixed text-decoration overlaps its unprefixed longhand" true
    (Shorthand.declarations_overlap
       (decl "-webkit-text-decoration:underline")
       (decl "text-decoration-color:red"))

let test_shorthand_reset_boundaries () =
  (* CSS UI 4 sec. 6.4: [outline] resets width, style and colour, and leaves
     [outline-offset] alone. *)
  Alcotest.(check bool)
    "outline overlaps its colour longhand" true
    (Shorthand.declarations_overlap
       (decl "outline:1px solid red")
       (decl "outline-color:blue"));
  Alcotest.(check bool)
    "outline and outline-offset are disjoint" false
    (Shorthand.declarations_overlap
       (decl "outline:1px solid red")
       (decl "outline-offset:2px"));
  (* CSS Text Decoration 4 sec. 3.4: [text-emphasis] resets style and colour,
     not position. *)
  Alcotest.(check bool)
    "text-emphasis and its position longhand are disjoint" false
    (Shorthand.declarations_overlap (decl "text-emphasis:dot")
       (decl "text-emphasis-position:over"));
  (* CSS Grid 1 sec. 7.4: [grid] resets the template and auto tracks, not the
     placement longhands. *)
  Alcotest.(check bool)
    "grid overlaps a template longhand" true
    (Shorthand.declarations_overlap (decl "grid:auto/auto")
       (decl "grid-template-columns:1fr"));
  Alcotest.(check bool)
    "grid and a placement longhand are disjoint" false
    (Shorthand.declarations_overlap (decl "grid:auto/auto")
       (decl "grid-row-start:2"));
  (* The legacy gap alias belongs to the gap family, not the grid placement one,
     even though the names share a prefix. *)
  Alcotest.(check bool)
    "a legacy gap alias and a grid row shorthand are disjoint" false
    (Shorthand.declarations_overlap (decl "grid-row-gap:9px")
       (decl "grid-row:1/2"))

let test_logical_physical_overlap () =
  (* CSS Logical 1 sec. 2: a flow-relative longhand resolves to a physical side
     the writing mode picks, so [margin-inline-start] is [margin-left] in one
     mode and [margin-right] in another. A sheet does not say which, so the pair
     may write a common slot and their order has to stand. *)
  Alcotest.(check bool)
    "an inline-start margin overlaps the left margin" true
    (Shorthand.declarations_overlap
       (decl "margin-inline-start:20px")
       (decl "margin-left:10px"));
  Alcotest.(check bool)
    "the relation is symmetric" true
    (Shorthand.declarations_overlap (decl "margin-left:10px")
       (decl "margin-inline-start:20px"));
  Alcotest.(check bool)
    "the same longhand overlaps the right margin too" true
    (Shorthand.declarations_overlap
       (decl "margin-inline-start:20px")
       (decl "margin-right:10px"));
  Alcotest.(check bool)
    "a logical longhand overlaps the physical shorthand" true
    (Shorthand.declarations_overlap (decl "margin:0")
       (decl "margin-inline-start:20px"));
  Alcotest.(check bool)
    "a block-start padding overlaps the top padding" true
    (Shorthand.declarations_overlap
       (decl "padding-block-start:20px")
       (decl "padding-top:10px"));
  Alcotest.(check bool)
    "an inline-start inset overlaps the top offset" true
    (Shorthand.declarations_overlap
       (decl "inset-inline-start:5px")
       (decl "top:1px"));
  Alcotest.(check bool)
    "a logical border side overlaps a physical one" true
    (Shorthand.declarations_overlap
       (decl "border-block:2px solid red")
       (decl "border-top:1px solid red"));
  Alcotest.(check bool)
    "a logical border side overlaps the border shorthand" true
    (Shorthand.declarations_overlap
       (decl "border-block:2px solid red")
       (decl "border:1px solid red"));
  Alcotest.(check bool)
    "a logical border width overlaps a physical border width" true
    (Shorthand.declarations_overlap
       (decl "border-inline-width:2px")
       (decl "border-left-width:1px"));
  Alcotest.(check bool)
    "a logical scroll margin overlaps a physical one" true
    (Shorthand.declarations_overlap
       (decl "scroll-margin-block-start:2px")
       (decl "scroll-margin-top:1px"));
  (* A recovered longhand keeps the aliasing: its value defeated the typed
     reader, not the cascade slot it writes. *)
  Alcotest.(check bool)
    "a recovered logical longhand overlaps its physical side" true
    (Shorthand.declarations_overlap
       (decl "margin-inline-start:var(--a) var(--b)")
       (decl "margin-left:10px"));
  (* The block and inline axes are perpendicular in every writing mode, so two
     logical longhands of one family never resolve to a common side. The model
     says they may: it gives each logical slot the family's whole physical set,
     which is the price of leaving the physical sides one slot each. Overlap is
     the safe answer either way - it keeps the pair in source order. *)
  Alcotest.(check bool)
    "two perpendicular logical longhands are conservatively overlapping" true
    (Shorthand.declarations_overlap
       (decl "margin-inline-start:20px")
       (decl "margin-block-start:10px"));
  (* Two physical sides are two slots whatever the writing mode. *)
  Alcotest.(check bool)
    "two physical sides stay disjoint" false
    (Shorthand.declarations_overlap (decl "margin-top:1px")
       (decl "margin-bottom:2px"));
  (* Aliasing is within a family: a margin never resolves to a padding. *)
  Alcotest.(check bool)
    "a logical margin and a physical padding are disjoint" false
    (Shorthand.declarations_overlap
       (decl "margin-inline-start:20px")
       (decl "padding-left:10px"));
  Alcotest.(check bool)
    "a logical border width and a border colour are disjoint" false
    (Shorthand.declarations_overlap
       (decl "border-inline-width:2px")
       (decl "border-left-color:red"))

let test_logical_axis_overlap () =
  (* CSS Logical 1 sec. 4.4: [inline-size] is [width] in a horizontal writing
     mode and [height] in a vertical one, so it may write either slot. The
     sizing family has no physical shorthand, which does not change the
     aliasing. *)
  Alcotest.(check bool)
    "an inline size overlaps the physical width" true
    (Shorthand.declarations_overlap (decl "inline-size:20px")
       (decl "width:10px"));
  Alcotest.(check bool)
    "the relation is symmetric" true
    (Shorthand.declarations_overlap (decl "width:10px")
       (decl "inline-size:20px"));
  Alcotest.(check bool)
    "the same logical size overlaps the physical height too" true
    (Shorthand.declarations_overlap (decl "inline-size:20px")
       (decl "height:10px"));
  Alcotest.(check bool)
    "a block size overlaps the physical height" true
    (Shorthand.declarations_overlap (decl "block-size:20px")
       (decl "height:10px"));
  Alcotest.(check bool)
    "a logical minimum overlaps the physical minimum" true
    (Shorthand.declarations_overlap
       (decl "min-inline-size:20px")
       (decl "min-width:10px"));
  Alcotest.(check bool)
    "a logical maximum overlaps the physical maximum" true
    (Shorthand.declarations_overlap
       (decl "max-block-size:20px")
       (decl "max-height:10px"));
  (* A recovered longhand keeps the aliasing: its value defeated the typed
     reader, not the cascade slot it writes. *)
  Alcotest.(check bool)
    "a recovered logical size overlaps its physical property" true
    (Shorthand.declarations_overlap
       (decl "inline-size:var(--a) var(--b)")
       (decl "width:10px"));
  (* CSS Sizing 4 sec. 5.2 gives [contain-intrinsic-size] the same two axes. *)
  Alcotest.(check bool)
    "a logical intrinsic size overlaps the physical one" true
    (Shorthand.declarations_overlap
       (decl "contain-intrinsic-inline-size:20px")
       (decl "contain-intrinsic-width:10px"));
  (* CSS Overscroll Behavior 1 sec. 3: the [-inline] and [-block] longhands are
     the flow-relative spellings of [-x] and [-y]. *)
  Alcotest.(check bool)
    "a logical overscroll behaviour overlaps the physical one" true
    (Shorthand.declarations_overlap
       (decl "overscroll-behavior-inline:contain")
       (decl "overscroll-behavior-x:none"));
  (* Aliasing is within a family: a size never resolves to a minimum, and the
     two physical axes are two slots whatever the writing mode. *)
  Alcotest.(check bool)
    "a logical size and a physical minimum are disjoint" false
    (Shorthand.declarations_overlap (decl "inline-size:20px")
       (decl "min-width:10px"));
  Alcotest.(check bool)
    "the two physical sizes stay disjoint" false
    (Shorthand.declarations_overlap (decl "width:10px") (decl "height:20px"));
  Alcotest.(check bool)
    "a logical size and an intrinsic size are disjoint" false
    (Shorthand.declarations_overlap (decl "inline-size:20px")
       (decl "contain-intrinsic-width:10px"))

let test_logical_corner_overlap () =
  (* CSS Logical 1 sec. 4.3: a flow-relative corner resolves to one of the four
     physical corners, which one depending on the writing mode and the
     direction. *)
  Alcotest.(check bool)
    "a start-start corner overlaps the top-left corner" true
    (Shorthand.declarations_overlap
       (decl "border-start-start-radius:2px")
       (decl "border-top-left-radius:1px"));
  Alcotest.(check bool)
    "the relation is symmetric" true
    (Shorthand.declarations_overlap
       (decl "border-top-left-radius:1px")
       (decl "border-start-start-radius:2px"));
  Alcotest.(check bool)
    "the same corner overlaps the opposite physical corner too" true
    (Shorthand.declarations_overlap
       (decl "border-start-start-radius:2px")
       (decl "border-bottom-right-radius:1px"));
  Alcotest.(check bool)
    "an end-end corner overlaps the top-right corner" true
    (Shorthand.declarations_overlap
       (decl "border-end-end-radius:2px")
       (decl "border-top-right-radius:1px"));
  Alcotest.(check bool)
    "a logical corner overlaps the radius shorthand" true
    (Shorthand.declarations_overlap (decl "border-radius:4px")
       (decl "border-start-end-radius:2px"));
  (* A corner radius is not a border width, style or colour. *)
  Alcotest.(check bool)
    "a logical corner and a physical border width are disjoint" false
    (Shorthand.declarations_overlap
       (decl "border-end-start-radius:2px")
       (decl "border-left-width:1px"))

let test_intentionally_duplicated_properties () =
  Alcotest.(check bool)
    "content duplicates are preserved" true
    (Shorthand.is_intentionally_duplicated (decl "content:\"a\""));
  Alcotest.(check bool)
    "outline duplicates are preserved" true
    (Shorthand.is_intentionally_duplicated (decl "outline:1px solid red"));
  Alcotest.(check bool)
    "color is ordinary" false
    (Shorthand.is_intentionally_duplicated (decl "color:red"))

let test_merge_overflow_longhands () =
  let merged =
    ".a{overflow-x:hidden;color:red;overflow-y:auto}" |> decls |> indexed
    |> Shorthand.merge_overflow_longhands |> unindexed
  in
  Alcotest.(check (list string))
    "x and y become overflow without disturbing middle declarations"
    [ "overflow:hidden auto"; "color:red" ]
    (decl_strings merged);
  let blocked =
    ".a{overflow-x:hidden;overflow:auto;overflow-y:hidden}" |> decls |> indexed
    |> Shorthand.merge_overflow_longhands |> unindexed
  in
  Alcotest.(check (list string))
    "existing overflow blocks composition"
    [ "overflow-x:hidden"; "overflow:auto"; "overflow-y:hidden" ]
    (decl_strings blocked)

let test_drop_redundant_longhand_after_shorthand () =
  (* A background shorthand sets every longhand it covers, so a later longhand
     equal to what it set (its slot value, else the initial) is a no-op. *)
  decl_optimizes_to ~into:"background:red" "background:red;background-size:auto";
  decl_optimizes_to ~into:"background:red"
    "background:red;background-repeat:repeat";
  decl_optimizes_to ~into:"background:red"
    "background:red;background-attachment:scroll";
  (* A non-initial value, or an override of a size the shorthand set, is
     kept. *)
  decl_optimizes_to ~into:"background:red;background-size:cover"
    "background:red;background-size:cover";
  decl_optimizes_to ~into:"background:url(x)50%/cover;background-size:auto"
    "background:url(x) center/cover;background-size:auto";
  (* Importance mismatch is a real change, not a no-op. *)
  decl_optimizes_to ~into:"background:red;background-size:auto!important"
    "background:red;background-size:auto!important"

let test_drop_redundant_flex_longhand () =
  (* [flex] expands to grow/shrink/basis: a later grow/shrink equal to what it
     set is dropped. flex:1 is [1 1 0], auto is [1 1 auto], none is [0 0
     auto]. *)
  decl_optimizes_to ~into:"flex:1" "flex:1;flex-grow:1";
  decl_optimizes_to ~into:"flex:auto" "flex:auto;flex-grow:1";
  decl_optimizes_to ~into:"flex:none" "flex:none;flex-shrink:0";
  decl_optimizes_to ~into:"flex:2 3" "flex:2 3;flex-grow:2";
  (* A differing factor is kept. *)
  decl_optimizes_to ~into:"flex:1;flex-grow:2" "flex:1;flex-grow:2"

let test_drop_redundant_transition_longhand () =
  (* [transition] sets duration/timing/delay/behavior; a later longhand equal to
     its slot (else the initial 0s/ease/normal) is dropped. *)
  decl_optimizes_to ~into:"transition:all 1s"
    "transition:all 1s;transition-delay:0s";
  decl_optimizes_to ~into:"transition:all 1s"
    "transition:all 1s;transition-timing-function:ease";
  decl_optimizes_to ~into:"transition:all 1s 2s"
    "transition:all 1s 2s;transition-delay:2s";
  (* A differing value is kept. *)
  decl_optimizes_to ~into:"transition:all 1s;transition-delay:1s"
    "transition:all 1s;transition-delay:1s"

let test_drop_redundant_border_longhand () =
  (* [border] sets width/style/color; a later longhand equal to an explicit slot
     is dropped. A differing value or a per-side list is kept. *)
  decl_optimizes_to ~into:"border:1px solid red"
    "border:1px solid red;border-color:red";
  decl_optimizes_to ~into:"border:1px solid red"
    "border:1px solid red;border-width:1px";
  decl_optimizes_to ~into:"border:1px solid red"
    "border:1px solid red;border-style:solid";
  decl_optimizes_to ~into:"border:1px solid red;border-color:#00f"
    "border:1px solid red;border-color:blue";
  decl_optimizes_to ~into:"border:1px solid red;border-color:red green"
    "border:1px solid red;border-color:red green"

let test_drop_redundant_font_longhand () =
  (* [font] resets style/weight/stretch/line-height to normal; a later longhand
     equal to that (weight normal folds to 400) is dropped. *)
  decl_optimizes_to ~into:"font:16px sans-serif"
    "font:16px sans-serif;font-weight:400";
  decl_optimizes_to ~into:"font:16px sans-serif"
    "font:16px sans-serif;font-weight:normal";
  decl_optimizes_to ~into:"font:16px sans-serif"
    "font:16px sans-serif;font-style:normal";
  decl_optimizes_to ~into:"font:700 16px x" "font:bold 16px x;font-weight:bold";
  (* A differing value is kept. *)
  decl_optimizes_to ~into:"font:16px sans-serif;font-weight:700"
    "font:16px sans-serif;font-weight:700"

let test_compose_shorthands_and_runtime_guard () =
  let ctx = Ctx.fragment in
  let composed =
    ".a{margin-top:1px;margin-right:2px;margin-bottom:1px;margin-left:2px}"
    |> decls |> indexed
    |> Shorthand.compose_shorthands ~ctx
    |> unindexed
  in
  Alcotest.(check (list string))
    "box longhands compose" [ "margin:1px 2px" ] (decl_strings composed);
  let guarded =
    ".a{border-top-width:var(--w);border-right-width:1px;border-bottom-width:1px;border-left-width:1px;border-top-style:solid;border-right-style:solid;border-bottom-style:solid;border-left-style:solid;border-top-color:red;border-right-color:red;border-bottom-color:red;border-left-color:red}"
    |> decls |> indexed
    |> Shorthand.compose_shorthands ~ctx
    |> unindexed
  in
  Alcotest.(check int)
    "runtime substitution prevents typed border shorthand" 12
    (List.length guarded)

let test_stylesheet_scope_prior_longhand_guard () =
  (* A layer shorthand resets every layer field, so synthesizing one from a run
     would silently revert a same-family longhand that sits before the run.
     Closed-world (stylesheet) scope is closed over external CSS only, not over
     this same-block longhand, so composition must refuse here. *)
  let ctx = Ctx.of_scope (Some `Stylesheet) in
  let compose css =
    css |> decls |> indexed
    |> Shorthand.compose_shorthands ~ctx
    |> unindexed |> decl_strings
  in
  Alcotest.(check (list string))
    "a prior background-clip blocks background composition"
    [
      "background-clip:text";
      "color:red";
      "background-color:blue";
      "background-image:url(x.png)";
    ]
    (compose
       ".a{background-clip:text;color:red;background-color:blue;background-image:url(x.png)}");
  Alcotest.(check (list string))
    "a prior mask-clip blocks mask composition"
    [
      "mask-clip:view-box";
      "color:red";
      "mask-image:url(x.png)";
      "mask-repeat:no-repeat";
    ]
    (compose
       ".a{mask-clip:view-box;color:red;mask-image:url(x.png);mask-repeat:no-repeat}")

let test_deduplicate_keeps_legacy_fallbacks () =
  let result =
    ".a{display:-webkit-box;display:flex;color:red;color:blue}" |> decls
    |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "vendor fallback is kept but ordinary duplicate is dropped"
    [ "display:-webkit-box"; "display:flex"; "color:blue" ]
    (decl_strings result);
  (* The prefixed-keyword fallbacks for position / text-align are kept too: old
     Safari only understands -webkit-sticky / -webkit-match-parent. *)
  let result =
    ".a{position:-webkit-sticky;position:sticky;text-align:-webkit-match-parent;text-align:start}"
    |> decls |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "prefixed-keyword position/text-align fallbacks are kept"
    [
      "position:-webkit-sticky";
      "position:sticky";
      "text-align:-webkit-match-parent";
      "text-align:start";
    ]
    (decl_strings result);
  (* The prefixed intrinsic sizing keywords are kept against the unprefixed
     form, on a length-percentage and a min-* property. *)
  let result =
    ".a{width:-webkit-max-content;width:max-content;min-width:-moz-fit-content;min-width:fit-content}"
    |> decls |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "prefixed sizing keyword fallbacks are kept"
    [
      "width:-webkit-max-content";
      "width:max-content";
      "min-width:-moz-fit-content";
      "min-width:fit-content";
    ]
    (decl_strings result)

let test_same_value_ignores_importance () =
  Alcotest.(check bool)
    "importance alone does not change the value" true
    (Shorthand.same_value
       (decl "display:-webkit-box")
       (decl "display:-webkit-box!important"));
  Alcotest.(check bool)
    "the same holds under a theme guard" true
    (Shorthand.same_value
       (Declaration.theme_guarded ~var_name:"--x" (decl "display:-webkit-box"))
       (Declaration.theme_guarded ~var_name:"--x"
          (decl "display:-webkit-box!important")));
  (* Equal values are not a vendor fallback, so the earlier declaration is
     dropped even though the later one adds [!important]. *)
  let result =
    ".a{display:-webkit-box;display:-webkit-box!important}" |> decls
    |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "a vendor value repeated with !important collapses"
    [ "display:-webkit-box!important" ]
    (decl_strings result)

let suite =
  ( "shorthand",
    [
      Alcotest.test_case "declaration coverage reset boundaries" `Quick
        test_declaration_covers_reset_boundaries;
      Alcotest.test_case "unknown property overlap" `Quick
        test_unknown_property_overlap;
      Alcotest.test_case "vendor alias overlap" `Quick test_vendor_alias_overlap;
      Alcotest.test_case "shorthand reset boundaries" `Quick
        test_shorthand_reset_boundaries;
      Alcotest.test_case "logical physical overlap" `Quick
        test_logical_physical_overlap;
      Alcotest.test_case "logical axis overlap" `Quick test_logical_axis_overlap;
      Alcotest.test_case "logical corner overlap" `Quick
        test_logical_corner_overlap;
      Alcotest.test_case "intentionally duplicated properties" `Quick
        test_intentionally_duplicated_properties;
      Alcotest.test_case "merge overflow longhands" `Quick
        test_merge_overflow_longhands;
      Alcotest.test_case "drop redundant longhand after shorthand" `Quick
        test_drop_redundant_longhand_after_shorthand;
      Alcotest.test_case "drop redundant flex longhand" `Quick
        test_drop_redundant_flex_longhand;
      Alcotest.test_case "drop redundant transition longhand" `Quick
        test_drop_redundant_transition_longhand;
      Alcotest.test_case "drop redundant border longhand" `Quick
        test_drop_redundant_border_longhand;
      Alcotest.test_case "drop redundant font longhand" `Quick
        test_drop_redundant_font_longhand;
      Alcotest.test_case "compose shorthands and runtime guard" `Quick
        test_compose_shorthands_and_runtime_guard;
      Alcotest.test_case "stylesheet scope prior longhand guard" `Quick
        test_stylesheet_scope_prior_longhand_guard;
      Alcotest.test_case "deduplicate keeps legacy fallbacks" `Quick
        test_deduplicate_keeps_legacy_fallbacks;
      Alcotest.test_case "same value ignores importance" `Quick
        test_same_value_ignores_importance;
    ] )
