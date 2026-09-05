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
  List.map (Pp.to_string ~minify:true Declaration.pp) decls

let indexed decls = List.mapi (fun i d -> (i, d)) decls
let unindexed decls = List.map snd decls

(* [decl_optimizes_to] wraps its input in one rule; these cases turn on what a
   neighbouring rule holds, so they optimize a whole sheet. *)
let sheet_optimizes_to ~into input =
  match Css.of_string input with
  | Ok { stylesheet; _ } ->
      Alcotest.(check string)
        (String.concat "" [ input; " minify+optimize" ])
        into
        (String.trim (Css.to_string ~minify:true (Css.optimize stylesheet)))
  | Error e -> Alcotest.failf "parse failed: %s" (Error.to_string e)

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
  (* CSS Cascade 5 sec. 3.2: [all] resets unknown properties. *)
  Alcotest.(check bool)
    "all overlaps an unknown property" true
    (Shorthand.declarations_overlap (decl "all:initial")
       (decl "grid-row-gap:9px"));
  (* Custom properties are their own cascade slots. *)
  Alcotest.(check bool)
    "an unknown property and a custom property are disjoint" false
    (Shorthand.declarations_overlap (decl "grid-row-gap:9px")
       (decl "--brand:red"))

let test_unknown_property_fallbacks () =
  let dedup css =
    css |> decls |> Shorthand.deduplicate_declarations |> decl_strings
  in
  Alcotest.(check (list string))
    "different values may target different browser grammars"
    [ "future-property:first"; "future-property:second" ]
    (dedup "a{future-property:first;future-property:second}");
  Alcotest.(check (list string))
    "an exact duplicate is redundant"
    [ "future-property:first" ]
    (dedup "a{future-property:first;future-property:first}")

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
       (decl "text-decoration-color:red"));
  (* The whole alias family, one row per unprefixed twin the model types. A
     prefix an engine does not implement makes the declaration invalid there,
     which no ordering can be wrong about, so the relation follows the engine
     that does implement it: Blink and WebKit for [-webkit-], Gecko for [-moz-],
     Trident for [-ms-] and Presto for [-o-]. *)
  List.iter
    (fun (vendor, twin) ->
      Alcotest.(check bool)
        (String.concat " " [ vendor; "overlaps"; twin ])
        true
        (Shorthand.declarations_overlap (decl vendor) (decl twin)))
    [
      ("-webkit-transform:none", "transform:rotate(45deg)");
      ("-moz-transform:none", "transform:rotate(45deg)");
      ("-ms-transform:none", "transform:rotate(45deg)");
      ("-o-transform:none", "transform:rotate(45deg)");
      ("-webkit-appearance:none", "appearance:auto");
      ("-moz-appearance:none", "appearance:auto");
      ("-webkit-box-shadow:none", "box-shadow:0 0 1px red");
      ("-moz-box-shadow:none", "box-shadow:0 0 1px red");
      ("-webkit-box-sizing:content-box", "box-sizing:border-box");
      ("-moz-box-sizing:content-box", "box-sizing:border-box");
      ("-webkit-user-select:none", "user-select:text");
      ("-moz-user-select:none", "user-select:text");
      ("-ms-user-select:none", "user-select:text");
      ("-webkit-filter:none", "filter:blur(1px)");
      ("-ms-filter:none", "filter:blur(1px)");
      ("-webkit-backdrop-filter:none", "backdrop-filter:blur(1px)");
      ("-webkit-background-clip:border-box", "background-clip:content-box");
      ("-webkit-background-size:auto", "background-size:cover");
      ("-webkit-box-decoration-break:slice", "box-decoration-break:clone");
      ("-webkit-hyphens:none", "hyphens:auto");
      ("-webkit-mask-clip:border-box", "mask-clip:content-box");
      ("-webkit-mask-composite:xor", "mask-composite:subtract");
      ("-webkit-mask-image:none", "mask-image:linear-gradient(red,blue)");
      ("-webkit-mask-origin:border-box", "mask-origin:content-box");
      ("-webkit-mask-position:0 0", "mask-position:10px 20px");
      ("-webkit-mask-repeat:repeat", "mask-repeat:no-repeat");
      ("-webkit-mask-size:auto", "mask-size:cover");
      ("-webkit-print-color-adjust:economy", "print-color-adjust:exact");
      ("-webkit-text-size-adjust:none", "text-size-adjust:200%");
    ];
  (* Aliasing a prefix to its twin joins those two slots and no others: a
     prefixed longhand still misses the rest of its own family. *)
  Alcotest.(check bool)
    "a prefixed mask image and an unprefixed mask size are disjoint" false
    (Shorthand.declarations_overlap
       (decl "-webkit-mask-image:none")
       (decl "mask-size:cover"));
  Alcotest.(check bool)
    "a prefixed transform and an unrelated property are disjoint" false
    (Shorthand.declarations_overlap
       (decl "-webkit-transform:none")
       (decl "color:red"))

let test_shorthand_reset_boundaries () =
  (* CSS UI 4 sec. 3.1: [outline] resets width, style and colour, and leaves
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
  (* CSS Grid 1 sec. 7.8: [grid] resets the template and auto tracks, not the
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
  (* The style sides carry the same aliasing as the width and colour ones:
     [border-block-end-style] resolves to whichever physical style slot the
     writing mode picks, so [border] and [border-style] both reset it. *)
  Alcotest.(check bool)
    "a logical border style side overlaps a physical style side" true
    (Shorthand.declarations_overlap
       (decl "border-block-end-style:dashed")
       (decl "border-bottom-style:solid"));
  Alcotest.(check bool)
    "a logical border style side overlaps the border shorthand" true
    (Shorthand.declarations_overlap
       (decl "border-block-end-style:dashed")
       (decl "border:1px solid red"));
  Alcotest.(check bool)
    "an inline border style side overlaps the border style shorthand" true
    (Shorthand.declarations_overlap
       (decl "border-inline-start-style:dashed")
       (decl "border-style:solid"));
  (* Width and style are separate slots whatever the writing mode is. *)
  Alcotest.(check bool)
    "a logical border style side and a physical width are disjoint" false
    (Shorthand.declarations_overlap
       (decl "border-block-end-style:dashed")
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
  (* CSS Logical 1 sec. 4.1: [inline-size] is [width] in a horizontal writing
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

(* CSS Transitions 2 sec. 2.6: [transition] resets [transition-behavior] along
   with the four Transitions 1 longhands. A run that leaves a slot unwritten
   contracts only when nothing earlier in the rule set that slot to something
   other than its initial, otherwise the shorthand silently resets it. *)
let test_transition_contraction_covers_reset_longhands () =
  (* The behaviour is a component of [<single-transition>], so a run carrying it
     contracts and keeps it, whichever side of the run it sits on. Both [ease]
     and [0s] are initials and drop out. *)
  decl_optimizes_to ~into:"transition:color 1s allow-discrete"
    "transition-behavior:allow-discrete;transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s";
  decl_optimizes_to ~into:"transition:color 1s allow-discrete"
    "transition-property:color;transition-duration:1s;transition-behavior:allow-discrete";
  decl_optimizes_to ~into:"transition:color 1s allow-discrete"
    "transition-property:color;transition-behavior:allow-discrete;transition-duration:1s";
  (* A run covering only the Transitions 1 longhands still contracts: nothing in
     the rule writes the behaviour, so resetting it to [normal] is a no-op. *)
  decl_optimizes_to ~into:"transition:color 1s"
    "transition-property:color;transition-duration:1s;transition-timing-function:ease;transition-delay:0s";
  (* [inherit] is not a shorthand component, so the run cannot carry it and the
     contraction would drop it. *)
  decl_optimizes_to
    ~into:
      "transition-behavior:inherit;transition-property:color;transition-duration:1s"
    "transition-behavior:inherit;transition-property:color;transition-duration:1s";
  (* Same rule for the delay: an unrelated declaration splits it off the run, so
     the run no longer covers the slot the shorthand resets. *)
  decl_optimizes_to
    ~into:
      "transition-delay:5s;color:red;transition-property:color;transition-duration:1s"
    "transition-delay:5s;color:red;transition-property:color;transition-duration:1s";
  (* An earlier shorthand holds the slots its own run left out. Contracting the
     later run resets the delay it set. *)
  decl_optimizes_to
    ~into:
      "transition:color 1s \
       5s;transition-property:opacity;transition-duration:2s"
    "transition:color 1s ease \
     5s;transition-property:opacity;transition-duration:2s";
  (* An earlier shorthand whose unwritten slots are already initials shadows
     nothing, so the later run contracts. *)
  decl_optimizes_to ~into:"transition:opacity 2s"
    "transition:color 1s;transition-property:opacity;transition-duration:2s";
  (* An important longhand outranks the composed shorthand whatever the order,
     so it is not a hazard. *)
  decl_optimizes_to ~into:"transition-delay:5s!important;transition:color 1s"
    "transition-delay:5s!important;transition-property:color;transition-duration:1s"

(* The slot a contraction resets belongs to the element, not to the rule: any
   declaration that can reach the same element and holds that slot is at risk,
   whichever rule it sits in. Composition reads one rule, so it cannot see the
   holders next door. *)
(* CSS Scroll Snap 1 sec. 6.1 and 6.2 give the scroll-margin and scroll-padding
   logical axes the same [<length>{1,2}] shape the margin and padding axes have,
   so the pair composes the same way. Without it the canonical comparator cannot
   equate the shorthand with its own longhands. *)
let test_scroll_axis_pair_composes () =
  sheet_optimizes_to ~into:".x{scroll-margin-block:1px 2px}"
    ".x{scroll-margin-block-start:1px;scroll-margin-block-end:2px}";
  sheet_optimizes_to ~into:".x{scroll-margin-inline:1px}"
    ".x{scroll-margin-inline-start:1px;scroll-margin-inline-end:1px}";
  sheet_optimizes_to ~into:".x{scroll-padding-block:1px 2px}"
    ".x{scroll-padding-block-start:1px;scroll-padding-block-end:2px}";
  sheet_optimizes_to ~into:".x{scroll-padding-inline:3em}"
    ".x{scroll-padding-inline-start:3em;scroll-padding-inline-end:3em}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{scroll-margin-block-start:1px;scroll-margin-block-end:2px!important}"
    ".x{scroll-margin-block-start:1px;scroll-margin-block-end:2px!important}"

(* CSS Logical 1 sec. 4.3 and 4.4: [border-block-*] and [border-inline-*] take
   one or two values of the side longhand's own type, so the start and end
   longhands compose into the axis shorthand the way the length axes do. Without
   it the canonical comparator cannot equate the shorthand with its
   longhands. *)
let test_border_axis_pair_composes () =
  sheet_optimizes_to ~into:".x{border-inline-width:2px 1px}"
    ".x{border-inline-start-width:2px;border-inline-end-width:1px}";
  sheet_optimizes_to ~into:".x{border-block-width:2px}"
    ".x{border-block-start-width:2px;border-block-end-width:2px}";
  sheet_optimizes_to ~into:".x{border-inline-style:solid dashed}"
    ".x{border-inline-start-style:solid;border-inline-end-style:dashed}";
  sheet_optimizes_to ~into:".x{border-block-style:double}"
    ".x{border-block-start-style:double;border-block-end-style:double}";
  sheet_optimizes_to ~into:".x{border-inline-color:red currentColor}"
    ".x{border-inline-start-color:red;border-inline-end-color:currentcolor}";
  sheet_optimizes_to ~into:".x{border-block-color:#0f0}"
    ".x{border-block-start-color:#0f0;border-block-end-color:#0f0}";
  (* A CSS-wide keyword is the whole value or nothing, so a mixed pair stays two
     declarations. *)
  sheet_optimizes_to
    ~into:".x{border-inline-start-width:inherit;border-inline-end-width:thin}"
    ".x{border-inline-start-width:inherit;border-inline-end-width:thin}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{border-block-start-color:red;border-block-end-color:#00f!important}"
    ".x{border-block-start-color:red;border-block-end-color:blue!important}"

(* CSS Backgrounds 3 sec. 3.5 and CSS Logical 1 sec. 4.5: each border side is
   [<line-width> || <line-style> || <line-color>] over its own three longhands
   and resets nothing else, so a contiguous run of the three contracts the way
   [outline] already does. *)
let test_line_shorthand_composes () =
  sheet_optimizes_to ~into:".x{border-top:1px solid red}"
    ".x{border-top-width:1px;border-top-style:solid;border-top-color:red}";
  (* The three are independent, so any order names the same declaration. *)
  sheet_optimizes_to ~into:".x{border-left:1px solid red}"
    ".x{border-left-color:red;border-left-style:solid;border-left-width:1px}";
  sheet_optimizes_to ~into:".x{border-block-start:1px solid red}"
    ".x{border-block-start-width:1px;border-block-start-style:solid;border-block-start-color:red}";
  sheet_optimizes_to ~into:".x{border-inline-end:2px dotted#0f0}"
    ".x{border-inline-end-width:2px;border-inline-end-style:dotted;border-inline-end-color:#0f0}";
  (* The shorthand tells width from style from colour by type, so a substituted
     token sequence could land in another slot. *)
  sheet_optimizes_to
    ~into:
      ".x{border-right-width:var(--w);border-right-style:solid;border-right-color:red}"
    ".x{border-right-width:var(--w);border-right-style:solid;border-right-color:red}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{border-bottom-width:1px;border-bottom-style:solid;border-bottom-color:red!important}"
    ".x{border-bottom-width:1px;border-bottom-style:solid;border-bottom-color:red!important}"

(* CSS Cascade 5 sec. 7.3: [initial] is the property's initial value, which is
   what the shorthand assigns to a slot left out, so a longhand written that way
   names the same declaration as an omitted component. *)
let test_line_shorthand_initial_slot () =
  sheet_optimizes_to ~into:".x{border-block-start:thin dashed}"
    ".x{border-block-start-width:thin;border-block-start-style:dashed;border-block-start-color:initial}";
  sheet_optimizes_to ~into:".x{border-top:dashed red}"
    ".x{border-top-width:initial;border-top-style:dashed;border-top-color:red}";
  (* Every slot at its initial is what [none] names. *)
  sheet_optimizes_to ~into:".x{border-left:none}"
    ".x{border-left-width:initial;border-left-style:initial;border-left-color:initial}"

(* CSS Logical 1 sec. 4.6: [border-block] and [border-inline] set the width,
   style and colour of both sides of their axis, which is what the three axis
   shorthands set between them, so a single-valued run of the three contracts
   the way [border-width] / [border-style] / [border-color] contract into
   [border]. *)
let test_logical_border_whole_composes () =
  sheet_optimizes_to ~into:".x{border-block:1px solid red}"
    ".x{border-block-width:1px;border-block-style:solid;border-block-color:red}";
  sheet_optimizes_to ~into:".x{border-inline:1px solid red}"
    ".x{border-inline-start-width:1px;border-inline-end-width:1px;border-inline-start-style:solid;border-inline-end-style:solid;border-inline-start-color:red;border-inline-end-color:red}";
  (* An axis naming two different sides has no slot in the shorthand. *)
  sheet_optimizes_to
    ~into:
      ".x{border-block-width:1px \
       2px;border-block-style:solid;border-block-color:red}"
    ".x{border-block-width:1px \
     2px;border-block-style:solid;border-block-color:red}"

(* CSS Overscroll 1 sec. 2.1 and CSS Sizing 4 sec. 5.1 assign their two
   longhands the way [overflow] does: the first value is the x axis, the second
   the y axis, and one value names both. *)
let test_xy_pair_composes () =
  sheet_optimizes_to ~into:".x{overscroll-behavior:contain}"
    ".x{overscroll-behavior-x:contain;overscroll-behavior-y:contain}";
  sheet_optimizes_to ~into:".x{overscroll-behavior:auto none}"
    ".x{overscroll-behavior-x:auto;overscroll-behavior-y:none}";
  sheet_optimizes_to ~into:".x{contain-intrinsic-size:100px}"
    ".x{contain-intrinsic-width:100px;contain-intrinsic-height:100px}";
  sheet_optimizes_to ~into:".x{contain-intrinsic-size:100px 200px}"
    ".x{contain-intrinsic-width:100px;contain-intrinsic-height:200px}";
  sheet_optimizes_to ~into:".x{contain-intrinsic-size:auto 300px}"
    ".x{contain-intrinsic-width:auto 300px;contain-intrinsic-height:auto 300px}";
  (* [contain-intrinsic-size] has no spelling for one axis at [none] beside a
     sized other, so that pair stays two declarations. *)
  sheet_optimizes_to
    ~into:".x{contain-intrinsic-width:none;contain-intrinsic-height:100px}"
    ".x{contain-intrinsic-width:none;contain-intrinsic-height:100px}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:".x{overscroll-behavior-x:auto;overscroll-behavior-y:none!important}"
    ".x{overscroll-behavior-x:auto;overscroll-behavior-y:none!important}"

(* CSS Grid 2 sec. 8.3 and 8.4: [grid-row] and [grid-column] are [<grid-line> [/
   <grid-line>]?] over their start and end longhands, and [grid-area] is the
   same over all four. The printer then picks the shortest spelling of the lines
   the shorthand names. *)
let test_grid_placement_composes () =
  sheet_optimizes_to ~into:".x{grid-row:1/3}"
    ".x{grid-row-start:1;grid-row-end:3}";
  sheet_optimizes_to ~into:".x{grid-column:1/3}"
    ".x{grid-column-start:1;grid-column-end:3}";
  sheet_optimizes_to ~into:".x{grid-row:span 2}"
    ".x{grid-row-start:span 2;grid-row-end:auto}";
  sheet_optimizes_to ~into:".x{grid-area:1/2/3/4}"
    ".x{grid-row-start:1;grid-column-start:2;grid-row-end:3;grid-column-end:4}";
  sheet_optimizes_to ~into:".x{grid-area:a}"
    ".x{grid-row-start:a;grid-column-start:a;grid-row-end:a;grid-column-end:a}";
  (* A substituted line can be a whole [<start> / <end>], so it stays put. *)
  sheet_optimizes_to ~into:".x{grid-row-start:var(--l);grid-row-end:3}"
    ".x{grid-row-start:var(--l);grid-row-end:3}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to ~into:".x{grid-row-start:1;grid-row-end:3!important}"
    ".x{grid-row-start:1;grid-row-end:3!important}"

(* CSS Flexbox 1 sec. 5.1 and CSS Text Decoration 4 sec. 3.4: [flex-flow] and
   [text-emphasis] each take two longhands, one per component. A longhand at its
   initial names what leaving the component out names, so it drops - unless both
   do and the value would then say nothing. *)
let test_duo_keyword_composes () =
  (* [row] is the direction's initial, so the composed value names it by leaving
     the component out. *)
  sheet_optimizes_to ~into:".x{flex-flow:wrap}"
    ".x{flex-direction:row;flex-wrap:wrap}";
  sheet_optimizes_to ~into:".x{flex-flow:column}"
    ".x{flex-direction:column;flex-wrap:nowrap}";
  sheet_optimizes_to ~into:".x{flex-flow:wrap-reverse}"
    ".x{flex-direction:row;flex-wrap:wrap-reverse}";
  sheet_optimizes_to ~into:".x{flex-flow:row}"
    ".x{flex-direction:row;flex-wrap:nowrap}";
  sheet_optimizes_to ~into:".x{text-emphasis:filled red}"
    ".x{text-emphasis-style:filled;text-emphasis-color:red}";
  sheet_optimizes_to ~into:".x{text-emphasis:none}"
    ".x{text-emphasis-style:none;text-emphasis-color:initial}";
  (* A substituted component can be the whole value, so it stays put. *)
  sheet_optimizes_to ~into:".x{flex-direction:var(--d);flex-wrap:wrap}"
    ".x{flex-direction:var(--d);flex-wrap:wrap}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to ~into:".x{flex-direction:row;flex-wrap:wrap!important}"
    ".x{flex-direction:row;flex-wrap:wrap!important}"

(* CSS Animations 2 sec. 6.3 and CSS Scroll Animations 1 sec. 4.3:
   [animation-range] is [<start> <end>?] and [scroll-timeline] is [<name>
   <axis>?], each over its own two longhands. *)
let test_timeline_range_composes () =
  sheet_optimizes_to ~into:".x{animation-range:normal}"
    ".x{animation-range-start:normal;animation-range-end:normal}";
  sheet_optimizes_to ~into:".x{animation-range:entry 10%exit 90%}"
    ".x{animation-range-start:entry 10%;animation-range-end:exit 90%}";
  (* [block] is the axis's initial, so the composed value names it by leaving
     the component out. *)
  sheet_optimizes_to ~into:".x{scroll-timeline:--t}"
    ".x{scroll-timeline-name:--t;scroll-timeline-axis:block}";
  sheet_optimizes_to ~into:".x{scroll-timeline:none}"
    ".x{scroll-timeline-name:none;scroll-timeline-axis:block}";
  (* [scroll-timeline: none] sets the axis to [block], so a named axis beside an
     unnamed timeline has no shorthand spelling. *)
  sheet_optimizes_to
    ~into:".x{scroll-timeline-name:none;scroll-timeline-axis:inline}"
    ".x{scroll-timeline-name:none;scroll-timeline-axis:inline}";
  (* Mixed importance is not one declaration. *)
  sheet_optimizes_to
    ~into:
      ".x{animation-range-start:normal;animation-range-end:normal!important}"
    ".x{animation-range-start:normal;animation-range-end:normal!important}"

(* CSS Animations 2 sec. 4.11: [animation] resets every longhand it names,
   [animation-name] included, so contracting a run that has no name beside a
   name written earlier says [none] and animates nothing. The transition family
   already reasons this way; animation had no coverage arms at all. *)
let test_animation_contraction_covers_other_rules () =
  (* Split across two rules with the same selector: the shortest spelling that
     keeps the name groups the two and carries it into the shorthand. *)
  sheet_optimizes_to ~into:".a{animation:spin 2s linear}"
    ".a{animation-name:spin}.a{animation-duration:2s;animation-timing-function:linear}";
  (* Same rule: the name is reset by a contraction that does not carry it, so
     the run stays expanded. *)
  sheet_optimizes_to
    ~into:".a{animation-name:spin;color:red;animation-duration:2s}"
    ".a{animation-name:spin;color:red;animation-duration:2s}";
  (* An important name outranks the non-important shorthand whatever the order,
     so the run contracts. *)
  sheet_optimizes_to
    ~into:".a{animation-name:spin!important;color:red;animation:2s linear}"
    ".a{animation-name:spin!important;color:red;animation-duration:2s;animation-timing-function:linear}";
  (* A non-important name is reset by the contraction, so the run stays
     expanded. *)
  sheet_optimizes_to
    ~into:
      ".a{animation-name:spin;color:red;animation-duration:2s;animation-timing-function:linear}"
    ".a{animation-name:spin;color:red;animation-duration:2s;animation-timing-function:linear}"

let test_transition_contraction_covers_other_rules () =
  (* Same rule, both sides of the guard. A non-important holder is reset by the
     contraction, so the run stays expanded; an important one outranks the
     non-important shorthand whatever the order, so the run contracts. *)
  sheet_optimizes_to
    ~into:
      ".a{transition-behavior:allow-discrete;color:red;transition-property:color;transition-duration:1s}"
    ".a{transition-behavior:allow-discrete;color:red;transition-property:color;transition-duration:1s}";
  sheet_optimizes_to
    ~into:
      ".a{transition-behavior:allow-discrete!important;color:red;transition:color \
       1s}"
    ".a{transition-behavior:allow-discrete!important;color:red;transition-property:color;transition-duration:1s}";
  (* Split across two rules with the same selector, the element sees exactly the
     cascade above. The behaviour has to survive, and the shortest spelling that
     keeps it groups the two rules and carries it into the shorthand. *)
  sheet_optimizes_to ~into:".a{transition:color 1s allow-discrete}"
    ".a{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}";
  sheet_optimizes_to
    ~into:".a{transition:color 1s;transition-behavior:allow-discrete!important}"
    ".a{transition-behavior:allow-discrete!important}.a{transition-property:color;transition-duration:1s}";
  (* Both important: the shorthand no longer loses to the holder, so dropping
     the behaviour would change what the element animates. *)
  sheet_optimizes_to ~into:".a{transition:color 1s allow-discrete!important}"
    ".a{transition-behavior:allow-discrete!important}.a{transition-property:color!important;transition-duration:1s!important}";
  (* An unrelated rule between them changes nothing: the holder is still in the
     cascade the second rule lands on. *)
  sheet_optimizes_to ~into:".a{transition:color 1s allow-discrete}.b{color:red}"
    ".a{transition-behavior:allow-discrete}.b{color:red}.a{transition-property:color;transition-duration:1s}";
  (* A rule between them that writes the same family keeps the two apart, so
     there is no grouping to carry the behaviour into and the run stays
     expanded. *)
  sheet_optimizes_to
    ~into:
      ".a{transition-behavior:allow-discrete}.b{transition:opacity \
       2s}.a{transition-property:color;transition-duration:1s}"
    ".a{transition-behavior:allow-discrete}.b{transition:opacity \
     2s}.a{transition-property:color;transition-duration:1s}";
  (* The holder needs no relation to the run's selector: an element carrying
     both classes reads one cascade. *)
  sheet_optimizes_to
    ~into:
      ".b{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}"
    ".b{transition-behavior:allow-discrete}.a{transition-property:color;transition-duration:1s}";
  (* The other side of the guard, so it stays a hazard test and not a blanket
     refusal: a neighbour holding the slot at its initial resets to the same
     thing, and an important neighbour outranks the shorthand. *)
  sheet_optimizes_to
    ~into:".b{transition-behavior:normal}.a{transition:color 1s}"
    ".b{transition-behavior:normal}.a{transition-property:color;transition-duration:1s}";
  sheet_optimizes_to
    ~into:
      ".b{transition-behavior:allow-discrete!important}.a{transition:color 1s}"
    ".b{transition-behavior:allow-discrete!important}.a{transition-property:color;transition-duration:1s}";
  (* A slot the rule itself rewrites after the run is not at risk from a
     neighbour holding it: the rewrite lands after the reset. *)
  sheet_optimizes_to
    ~into:".a{transition:color 1s;color:red;transition-delay:5s}"
    ".a{transition-property:color;transition-duration:1s;color:red;transition-delay:5s}"

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
  (* The guard is on the width family: the substituted tokens could re-assign
     across width, style and color in [border]. The four styles and the four
     colors carry no substitution and each set exactly their own four longhands,
     so those two boxes still compose. *)
  Alcotest.(check (list string))
    "runtime substitution prevents typed border shorthand"
    [
      "border-top-width:var(--w)";
      "border-right-width:1px";
      "border-bottom-width:1px";
      "border-left-width:1px";
      "border-style:solid";
      "border-color:red";
    ]
    (decl_strings guarded)

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

let test_deduplicate_keeps_shorthand_vendor_image_fallbacks () =
  (* A vendor-prefixed gradient inside a shorthand is a real fallback for the
     unprefixed gradient, just like the equivalent longhand. *)
  let result =
    ".a{background:-webkit-linear-gradient(top,#111,#222);background:linear-gradient(#333,#444);border-image:-webkit-linear-gradient(top,#111,#222);border-image:linear-gradient(#333,#444);mask:-webkit-linear-gradient(top,#111,#222);mask:linear-gradient(#333,#444)}"
    |> decls |> Shorthand.deduplicate_declarations
  in
  Alcotest.(check (list string))
    "vendor-prefixed gradient fallbacks inside shorthands are kept"
    [
      "background:-webkit-linear-gradient(top,#111,#222)";
      "background:linear-gradient(#333,#444)";
      "border-image:-webkit-linear-gradient(top,#111,#222)";
      "border-image:linear-gradient(#333,#444)";
      "mask:-webkit-linear-gradient(top,#111,#222)";
      "mask:linear-gradient(#333,#444)";
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

(* CSS Fragmentation 3 sec. 3.4 aliases [page-break-before/after/inside] to
   [break-before/after/inside] through a value mapping: [always] maps to [page]
   and every other value to itself. The pair is one property writing one cascade
   slot, so either spelling shadows the other and only the last one written
   survives. *)
let test_page_break_alias_shadowing () =
  Alcotest.(check bool)
    "the legacy spelling covers the modern one" true
    (Shorthand.declaration_covers
       (decl "page-break-before:always")
       (decl "break-before:page"));
  Alcotest.(check bool)
    "the modern spelling covers the legacy one" true
    (Shorthand.declaration_covers (decl "break-before:page")
       (decl "page-break-before:always"));
  Alcotest.(check bool)
    "the pair writes one slot" true
    (Shorthand.declarations_overlap
       (decl "page-break-inside:avoid")
       (decl "break-inside:avoid"));
  let dedup css =
    decl_strings (Shorthand.deduplicate_declarations (decls css))
  in
  Alcotest.(check (list string))
    "[always] maps to [page], so the pair is one declaration"
    [ "break-before:page" ]
    (dedup ".a{page-break-before:always;break-before:page}");
  Alcotest.(check (list string))
    "[avoid] maps to itself, so the pair is one declaration"
    [ "break-inside:avoid" ]
    (dedup ".a{page-break-inside:avoid;break-inside:avoid}");
  Alcotest.(check (list string))
    "[always] and [avoid] are two values of one property, and the last wins"
    [ "break-before:avoid" ]
    (dedup ".a{page-break-before:always;break-before:avoid}");
  Alcotest.(check (list string))
    "the legacy spelling wins when it comes last" [ "break-after:page" ]
    (dedup ".a{break-after:avoid;page-break-after:always}")

(* CSS Backgrounds 3 sec. 3.1 to 3.3: [border-color], [border-style] and
   [border-width] set exactly their four side longhands and reset nothing else.
   CSS Scroll Snap 1 sec. 4.2 and 5.1 say the same of [scroll-padding] and
   [scroll-margin]. Each shorthand and its four longhands compute the same
   values on every element, so a canonical diff equates them. *)
let test_box_family_shorthand_equivalence () =
  let same name a b =
    Alcotest.(check bool)
      name true
      (Cascade_diff.Css_compare.equal ~mode:`Canonical a b)
  in
  same "border-width equals its four side longhands" ".a{border-width:1px}"
    ".a{border-top-width:1px;border-right-width:1px;border-bottom-width:1px;border-left-width:1px}";
  same "border-style equals its four side longhands" ".a{border-style:solid}"
    ".a{border-top-style:solid;border-right-style:solid;border-bottom-style:solid;border-left-style:solid}";
  same "border-color equals its four side longhands" ".a{border-color:red}"
    ".a{border-top-color:red;border-right-color:red;border-bottom-color:red;border-left-color:red}";
  same "scroll-margin equals its four side longhands" ".a{scroll-margin:1px}"
    ".a{scroll-margin-top:1px;scroll-margin-right:1px;scroll-margin-bottom:1px;scroll-margin-left:1px}";
  same "scroll-padding equals its four side longhands" ".a{scroll-padding:1px}"
    ".a{scroll-padding-top:1px;scroll-padding-right:1px;scroll-padding-bottom:1px;scroll-padding-left:1px}";
  (* Per-side values reach the shorthand in top-right-bottom-left order. *)
  same "a two-value border-width equals its four side longhands"
    ".a{border-width:1px 2px}"
    ".a{border-top-width:1px;border-right-width:2px;border-bottom-width:1px;border-left-width:2px}"

(* A shorthand that writes more slots than the longhands beside it is a
   different declaration, and the report keeps saying so. *)
let test_box_family_non_equivalence_still_reports () =
  let differs name a b =
    Alcotest.(check bool)
      name false
      (Cascade_diff.Css_compare.equal ~mode:`Canonical a b)
  in
  (* [background] resets every background longhand to its initial. *)
  differs "background is not background-color" ".a{background:red}"
    ".a{background-color:red}";
  (* [border] resets [border-image] on top of the width it names. *)
  differs "border is not border-width" ".a{border:1px solid red}"
    ".a{border-width:1px}";
  differs "an unset side is not the shorthand" ".a{border-width:1px}"
    ".a{border-top-width:1px;border-right-width:1px;border-bottom-width:1px}";
  differs "a differing side is not the shorthand" ".a{border-width:1px}"
    ".a{border-top-width:1px;border-right-width:1px;border-bottom-width:1px;border-left-width:2px}";
  differs "a differing side style is not the shorthand" ".a{border-style:solid}"
    ".a{border-top-style:solid;border-right-style:solid;border-bottom-style:solid;border-left-style:dashed}";
  differs "an unset scroll-margin side is not the shorthand"
    ".a{scroll-margin:1px}"
    ".a{scroll-margin-top:1px;scroll-margin-right:1px;scroll-margin-bottom:1px}"

(* CSS Cascade 5 sec. 7.3: a CSS-wide keyword is the entire value of a
   declaration or nothing at all. Pasting one into a composed shorthand makes a
   declaration the reader rejects and a browser drops, so a run holding one has
   to stay as its longhands. *)
let unfoldable_css_wide = [ "inherit"; "unset"; "revert"; "revert-layer" ]

(* The emission is checked twice: for the expected text, and for reading back. A
   declaration the reader rejects is one the browser drops, so an emission that
   fails to re-read is a rendering change, not a shorter spelling. *)
let minifies_to name ~into css =
  let out =
    match Css.of_string css with
    | Error e -> Alcotest.failf "%s: parse failed: %s" name (Error.to_string e)
    | Ok { stylesheet; _ } ->
        String.trim (Css.to_string ~minify:true (Css.optimize stylesheet))
  in
  Alcotest.(check string) name into out;
  match Css.of_string ~strict:true out with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "%s: emission is not readable CSS: %s" name
        (Error.to_string e)

(* Every family whose composer takes a longhand value as a shorthand component,
   as the run that composes into it with the last value left open. *)
let css_wide_component_runs =
  [
    ( "padding",
      "padding-top:1px;padding-right:2px;padding-bottom:3px;padding-left:" );
    ("margin", "margin-top:1px;margin-right:2px;margin-bottom:3px;margin-left:");
    ("inset", "top:1px;right:2px;bottom:3px;left:");
    ( "border-color",
      "border-top-color:red;border-right-color:#0f0;border-bottom-color:#00f;border-left-color:"
    );
    ( "border-width",
      "border-top-width:1px;border-right-width:2px;border-bottom-width:3px;border-left-width:"
    );
    ( "border-radius",
      "border-top-left-radius:1px;border-top-right-radius:2px;border-bottom-right-radius:3px;border-bottom-left-radius:"
    );
    ( "scroll-margin",
      "scroll-margin-top:1px;scroll-margin-right:2px;scroll-margin-bottom:3px;scroll-margin-left:"
    );
    ( "scroll-padding",
      "scroll-padding-top:1px;scroll-padding-right:2px;scroll-padding-bottom:3px;scroll-padding-left:"
    );
    ("gap", "row-gap:1px;column-gap:");
    ("margin-inline", "margin-inline-start:1px;margin-inline-end:");
    ("margin-block", "margin-block-start:1px;margin-block-end:");
    ("padding-inline", "padding-inline-start:1px;padding-inline-end:");
    ("padding-block", "padding-block-start:1px;padding-block-end:");
    ("inset-inline", "inset-inline-start:1px;inset-inline-end:");
    ("inset-block", "inset-block-start:1px;inset-block-end:");
    ("place-content", "align-content:center;justify-content:");
    ("place-items", "align-items:center;justify-items:");
    ("place-self", "align-self:center;justify-self:");
    ("overflow", "overflow-x:hidden;overflow-y:");
    ("outline", "outline-width:1px;outline-style:solid;outline-color:");
    ( "list-style",
      "list-style-type:square;list-style-position:inside;list-style-image:" );
    ("flex", "flex-grow:1;flex-shrink:1;flex-basis:");
    ( "text-decoration",
      "text-decoration-line:underline;text-decoration-style:solid;text-decoration-color:"
    );
    ("border", "border-width:1px;border-style:solid;border-color:");
  ]

let test_css_wide_keyword_is_not_a_shorthand_component () =
  List.iter
    (fun (family, run) ->
      List.iter
        (fun keyword ->
          let css = String.concat "" [ ".a{"; run; keyword; "}" ] in
          minifies_to (String.concat " " [ family; keyword ]) ~into:css css)
        unfoldable_css_wide)
    css_wide_component_runs

(* The keyword reaches the composer from another rule just as well: rules with a
   matching selector merge before the declarations are composed. *)
let test_css_wide_keyword_from_another_rule () =
  List.iter
    (fun keyword ->
      minifies_to
        (String.concat " " [ "margin absorbs a later side"; keyword ])
        ~into:(String.concat "" [ ".a{margin:"; keyword; ";margin-top:1px}" ])
        (String.concat "" [ ".a{margin:"; keyword; "}.a{margin-top:1px}" ]);
      minifies_to
        (String.concat " " [ "padding absorbs a later side"; keyword ])
        ~into:(String.concat "" [ ".a{padding:"; keyword; ";padding-top:1px}" ])
        (String.concat "" [ ".a{padding:"; keyword; "}.a{padding-top:1px}" ]);
      minifies_to
        (String.concat " " [ "overflow across two rules"; keyword ])
        ~into:
          (String.concat ""
             [ ".a{overflow-x:hidden;overflow-y:"; keyword; "}" ])
        (String.concat ""
           [ ".a{overflow-x:hidden}.a{overflow-y:"; keyword; "}" ]);
      (* Four sides are four disjoint cascade slots, so the merged rule is free
         to order them however it likes; what it may not do is contract them. *)
      minifies_to
        (String.concat " " [ "four box sides across four rules"; keyword ])
        ~into:
          (String.concat ""
             [
               ".a{margin-bottom:3px;margin-left:";
               keyword;
               ";margin-right:2px;margin-top:1px}";
             ])
        (String.concat ""
           [
             ".a{margin-top:1px}.a{margin-right:2px}.a{margin-bottom:3px}.a{margin-left:";
             keyword;
             "}";
           ]))
    unfoldable_css_wide

let test_css_wide_keyword_with_importance () =
  (* Four important sides: the same-importance box composer. *)
  minifies_to "every side important"
    ~into:
      ".a{margin-top:1px!important;margin-right:2px!important;margin-bottom:3px!important;margin-left:inherit!important}"
    ".a{margin-top:1px!important;margin-right:2px!important;margin-bottom:3px!important;margin-left:inherit!important}";
  (* One important side: the mixed-importance split, which emits a shorthand and
     re-states the important longhand after it. *)
  minifies_to "an important side beside the keyword"
    ~into:
      ".a{margin-top:1px!important;margin-right:2px;margin-bottom:3px;margin-left:inherit}"
    ".a{margin-top:1px!important;margin-right:2px;margin-bottom:3px;margin-left:inherit}";
  minifies_to "the keyword is the important side"
    ~into:
      ".a{margin-top:1px;margin-right:2px;margin-bottom:3px;margin-left:inherit!important}"
    ".a{margin-top:1px;margin-right:2px;margin-bottom:3px;margin-left:inherit!important}";
  minifies_to "an important pair"
    ~into:".a{row-gap:1px!important;column-gap:unset!important}"
    ".a{row-gap:1px!important;column-gap:unset!important}";
  minifies_to "an important run"
    ~into:
      ".a{outline-width:1px!important;outline-style:solid!important;outline-color:revert!important}"
    ".a{outline-width:1px!important;outline-style:solid!important;outline-color:revert!important}";
  minifies_to "an important shorthand absorbing an important side"
    ~into:".a{margin:inherit!important;margin-top:1px!important}"
    ".a{margin:inherit!important}.a{margin-top:1px!important}"

(* A run whose sides are all the same CSS-wide keyword collapses to a lone
   keyword, which is a whole declaration value and so stays legal. *)
let test_uniform_css_wide_sides_still_contract () =
  List.iter
    (fun keyword ->
      minifies_to
        (String.concat " " [ "four identical box sides"; keyword ])
        ~into:(String.concat "" [ ".a{margin:"; keyword; "}" ])
        (String.concat ""
           [
             ".a{margin-top:";
             keyword;
             ";margin-right:";
             keyword;
             ";margin-bottom:";
             keyword;
             ";margin-left:";
             keyword;
             "}";
           ]);
      minifies_to
        (String.concat " " [ "two identical gap axes"; keyword ])
        ~into:(String.concat "" [ ".a{gap:"; keyword; "}" ])
        (String.concat ""
           [ ".a{row-gap:"; keyword; ";column-gap:"; keyword; "}" ]))
    unfoldable_css_wide

(* [initial] names a value with a concrete spelling, so the box families fold it
   to that spelling before composition and the run still contracts. *)
let test_initial_still_folds_in_box_families () =
  minifies_to "a lone initial side" ~into:".a{margin-left:0}"
    ".a{margin-left:initial}";
  minifies_to "initial as the fourth margin side"
    ~into:".a{margin:1px 2px 3px 0}"
    ".a{margin-top:1px;margin-right:2px;margin-bottom:3px;margin-left:initial}";
  minifies_to "initial as the fourth padding side"
    ~into:".a{padding:1px 2px 3px 0}"
    ".a{padding-top:1px;padding-right:2px;padding-bottom:3px;padding-left:initial}";
  minifies_to "four initial sides" ~into:".a{margin:0}"
    ".a{margin-top:initial;margin-right:initial;margin-bottom:initial;margin-left:initial}";
  minifies_to "an initial shorthand absorbing a later side"
    ~into:".a{margin:1px 0 0}" ".a{margin:initial}.a{margin-top:1px}";
  (* [scroll-margin] and [scroll-padding] keep [initial] as a keyword, so there
     it is a whole declaration value like the other four and the run stays
     expanded. Folding it to [0] would let the run contract again. *)
  minifies_to "initial in a scroll-margin run"
    ~into:
      ".a{scroll-margin-top:initial;scroll-margin-right:1px;scroll-margin-bottom:2px;scroll-margin-left:3px}"
    ".a{scroll-margin-top:initial;scroll-margin-right:1px;scroll-margin-bottom:2px;scroll-margin-left:3px}";
  minifies_to "initial in a scroll-padding run"
    ~into:
      ".a{scroll-padding-top:initial;scroll-padding-right:1px;scroll-padding-bottom:2px;scroll-padding-left:3px}"
    ".a{scroll-padding-top:initial;scroll-padding-right:1px;scroll-padding-bottom:2px;scroll-padding-left:3px}"

let suite =
  ( "shorthand",
    [
      Alcotest.test_case "declaration coverage reset boundaries" `Quick
        test_declaration_covers_reset_boundaries;
      Alcotest.test_case "unknown property overlap" `Quick
        test_unknown_property_overlap;
      Alcotest.test_case "unknown property fallbacks" `Quick
        test_unknown_property_fallbacks;
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
      Alcotest.test_case "transition contraction covers reset longhands" `Quick
        test_transition_contraction_covers_reset_longhands;
      Alcotest.test_case "transition contraction covers other rules" `Quick
        test_transition_contraction_covers_other_rules;
      Alcotest.test_case "animation contraction covers other rules" `Quick
        test_animation_contraction_covers_other_rules;
      Alcotest.test_case "scroll axis pair composes" `Quick
        test_scroll_axis_pair_composes;
      Alcotest.test_case "border axis pair composes" `Quick
        test_border_axis_pair_composes;
      Alcotest.test_case "line shorthand composes" `Quick
        test_line_shorthand_composes;
      Alcotest.test_case "line shorthand initial slot" `Quick
        test_line_shorthand_initial_slot;
      Alcotest.test_case "logical border whole composes" `Quick
        test_logical_border_whole_composes;
      Alcotest.test_case "xy pair composes" `Quick test_xy_pair_composes;
      Alcotest.test_case "grid placement composes" `Quick
        test_grid_placement_composes;
      Alcotest.test_case "duo keyword composes" `Quick test_duo_keyword_composes;
      Alcotest.test_case "timeline and range compose" `Quick
        test_timeline_range_composes;
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
      Alcotest.test_case "deduplicate keeps shorthand vendor image fallbacks"
        `Quick test_deduplicate_keeps_shorthand_vendor_image_fallbacks;
      Alcotest.test_case "same value ignores importance" `Quick
        test_same_value_ignores_importance;
      Alcotest.test_case "page-break alias shadowing" `Quick
        test_page_break_alias_shadowing;
      Alcotest.test_case "box family shorthand equivalence" `Quick
        test_box_family_shorthand_equivalence;
      Alcotest.test_case "box family non-equivalence still reports" `Quick
        test_box_family_non_equivalence_still_reports;
      Alcotest.test_case "css-wide keyword is not a shorthand component" `Quick
        test_css_wide_keyword_is_not_a_shorthand_component;
      Alcotest.test_case "css-wide keyword from another rule" `Quick
        test_css_wide_keyword_from_another_rule;
      Alcotest.test_case "css-wide keyword with importance" `Quick
        test_css_wide_keyword_with_importance;
      Alcotest.test_case "uniform css-wide sides still contract" `Quick
        test_uniform_css_wide_sides_still_contract;
      Alcotest.test_case "initial still folds in box families" `Quick
        test_initial_still_folds_in_box_families;
    ] )
