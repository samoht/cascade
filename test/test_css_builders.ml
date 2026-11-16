open Cascade
open Css

(* SVG 2 and CSS Inline 3 give these longhands a value type each, and the facade
   carried [fill], [stroke] and [stroke-width] alone. *)
let svg_longhand_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "fill-rule:evenodd" (Css.fill_rule Evenodd);
  check "clip-rule:nonzero" (Css.clip_rule Nonzero);
  check "fill-opacity:.5" (Css.fill_opacity (Opacity_number 0.5));
  check "stroke-opacity:1" (Css.stroke_opacity (Opacity_number 1.));
  check "stroke-linecap:round" (Css.stroke_linecap Round);
  check "stroke-linejoin:bevel" (Css.stroke_linejoin Bevel);
  check "stroke-miterlimit:4" (Css.stroke_miterlimit (Number 4.));
  check "stroke-dashoffset:2px"
    (Css.stroke_dashoffset (Dash (Length (Length (Px 2.)))));
  check "stroke-dasharray:4 2"
    (Css.stroke_dasharray (Dashes [ Number (Num 4.); Number (Num 2.) ]));
  check "paint-order:stroke fill" (Css.paint_order (Order [ Stroke; Fill ]));
  check "vector-effect:non-scaling-stroke"
    (Css.vector_effect (Effects ([ Non_scaling_stroke ], None)));
  check "stop-color:red" (Css.stop_color (Named Red));
  check "stop-opacity:0" (Css.stop_opacity (Opacity_number 0.));
  check "flood-color:red" (Css.flood_color (Named Red));
  check "flood-opacity:1" (Css.flood_opacity (Opacity_number 1.));
  check "lighting-color:red" (Css.lighting_color (Named Red));
  check "dominant-baseline:middle" (Css.dominant_baseline Middle);
  check "alignment-baseline:hanging" (Css.alignment_baseline Hanging);
  check "baseline-shift:super" (Css.baseline_shift Super);
  check "baseline-source:last" (Css.baseline_source Last)

(* CSS Anchor Positioning 1 is modelled end to end and the facade built none of
   it, so a caller could parse an anchored box but not write one. *)
let anchor_positioning_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "anchor-name:--tip" (Css.anchor_name (Names [ "--tip" ]));
  check "position-anchor:--tip" (Css.position_anchor (Anchor "--tip"));
  check "position-anchor:normal" (Css.position_anchor Normal);
  check "position-area:block-start span-inline-end"
    (Css.position_area (Area (Block_start, Some Span_inline_end)));
  check "position-try-fallbacks:--a flip-block"
    (Css.position_try_fallbacks
       (Fallbacks [ Tactics [ Name "--a"; Flip_block ] ]));
  check "position-try-fallbacks:start end"
    (Css.position_try_fallbacks (Fallbacks [ Area (Start, Some End) ]));
  check "position-try-order:most-width" (Css.position_try_order Most_width);
  check "position-try:most-width --a"
    (Css.position_try (Try (Most_width, Fallbacks [ Tactics [ Name "--a" ] ])));
  check "position-visibility:anchors-visible"
    (Css.position_visibility (Conditions [ Anchors_visible ]))

(* View Transitions is advertised in the README and the facade built neither
   property, so a caller could parse a transition name but not write one. *)
let view_transition_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "view-transition-name:card" (Css.view_transition_name (Name "card"));
  check "view-transition-name:none" (Css.view_transition_name None);
  check "view-transition-class:hero wide"
    (Css.view_transition_class (Classes [ "hero"; "wide" ]))

(* CSS Cascading 5 sec. 3.3 gives [all] every longhand but [direction] and
   [unicode-bidi]; the optimiser acts on it and the facade could not write
   one. *)
let all_shorthand_builder () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "all:initial" (Css.all Initial);
  check "all:revert-layer" (Css.all Revert_layer)

(* Motion Path 1 is modelled down to the shorthand and the facade built none of
   it. *)
let motion_path_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "offset-path:none" (Css.offset_path None);
  check "offset-path:path(\"M 0 0 H 1\")" (Css.offset_path (Path "M 0 0 H 1"));
  check "offset-distance:50%" (Css.offset_distance (Pct 50.));
  check "offset-rotate:reverse" (Css.offset_rotate Reverse);
  check "offset-rotate:auto 30deg"
    (Css.offset_rotate (With_angle (Auto, Deg 30.)));
  check "offset-anchor:auto" (Css.offset_anchor Auto);
  check "offset-position:normal" (Css.offset_position Normal);
  check "offset:none"
    (Css.offset
       (Shorthand
          {
            target =
              With_path
                { position = None; path = None; distance = None; rotate = None };
            anchor = None;
          }))

(* CSS Multicol 2 models each longhand of [columns] and the gap decoration
   lines, and the facade wrote only the two shorthands. *)
let multicol_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "column-width:20em" (Css.column_width (Width (Em 20.)));
  check "column-count:3" (Css.column_count (Count 3));
  check "column-height:10em" (Css.column_height (Height (Em 10.)));
  check "column-wrap:wrap" (Css.column_wrap Wrap);
  check "column-rule-width:1px,2px" (Css.column_rule_width [ Px 1.; Px 2. ]);
  check "column-rule-style:dotted,dashed"
    (Css.column_rule_style [ Dotted; Dashed ]);
  check "column-rule-color:red" (Css.column_rule_color [ Named Red ])

(* The border-image longhands and the two shorthands built on them were
   reachable only by parsing. *)
let border_image_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "border-image-source:url(edge.png)"
    (Css.border_image_source (Url "edge.png"));
  check "border-image-slice:30% fill"
    (Css.border_image_slice (Slices { offsets = [ Pct 30. ]; fill = true }));
  check "border-image-width:1 2"
    (Css.border_image_width (Widths [ Number (Num 1.); Number (Num 2.) ]));
  check "border-image-outset:2px"
    (Css.border_image_outset (Outsets [ Length (Px 2.) ]));
  check "border-image-repeat:round space"
    (Css.border_image_repeat (Repeats [ Round; Space ]));
  check "border-image:url(edge.png)30%"
    (Css.border_image
       {
         source = Some (Url "edge.png");
         slice = Some { offsets = [ Pct 30. ]; fill = false };
         width = None;
         outset = None;
         repeat = None;
         mode = None;
       });
  check "mask-border:url(edge.png)luminance"
    (Css.mask_border
       {
         source = Some (Url "edge.png");
         slice = None;
         width = None;
         outset = None;
         repeat = None;
         mode = Some Luminance;
       })

(* CSS Sizing 4 sec. 5 models the shorthand and its four axis longhands, and the
   facade wrote none of them. *)
let contain_intrinsic_size_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "contain-intrinsic-size:auto 300px"
    (Css.contain_intrinsic_size (Intrinsic (Auto (Px 300.), None)));
  check "contain-intrinsic-size:100px 200px"
    (Css.contain_intrinsic_size
       (Intrinsic (Length (Px 100.), Some (Length (Px 200.)))));
  check "contain-intrinsic-width:none" (Css.contain_intrinsic_width None);
  check "contain-intrinsic-height:10px"
    (Css.contain_intrinsic_height (Size (Length (Px 10.))));
  check "contain-intrinsic-block-size:10px"
    (Css.contain_intrinsic_block_size (Size (Length (Px 10.))));
  check "contain-intrinsic-inline-size:10px"
    (Css.contain_intrinsic_inline_size (Size (Length (Px 10.))))

(* Scroll-driven Animations 1 is modelled down to both shorthands, and the
   facade wrote none of the twelve properties. *)
let scroll_driven_animation_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "animation-timeline:--reveal" (Css.animation_timeline (Name "--reveal"));
  check "animation-timeline:scroll(root)"
    (Css.animation_timeline (Scroll "root"));
  check "animation-range:entry"
    (Css.animation_range (Range (Named (Entry, None), None)));
  check "animation-range-start:normal" (Css.animation_range_start Normal);
  check "animation-range-end:cover 100%"
    (Css.animation_range_end (Named (Cover, Some (Pct 100.))));
  check "scroll-timeline:--a block"
    (Css.scroll_timeline
       (Timelines [ { name = Name "--a"; axis = Some Block } ]));
  check "scroll-timeline-name:--a"
    (Css.scroll_timeline_name (Names [ Name "--a" ]));
  check "scroll-timeline-axis:inline" (Css.scroll_timeline_axis Inline);
  check "view-timeline:--a"
    (Css.view_timeline
       (Timelines [ { name = Name "--a"; axis = None; inset = None } ]));
  check "view-timeline-name:--a" (Css.view_timeline_name (Names [ Name "--a" ]));
  check "view-timeline-axis:y" (Css.view_timeline_axis Y);
  check "view-timeline-inset:auto"
    (Css.view_timeline_inset (Inset (Auto, None)));
  check "timeline-scope:--a" (Css.timeline_scope (Names [ "--a" ]))

(* The vendor-specific section documented sixty-odd prefixed longhands and
   exposed no builder for any of them. *)
let vendor_prefixed_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "-webkit-transform:none" (Css.webkit_transform [ None ]);
  check "-moz-transform:none" (Css.moz_transform [ None ]);
  check "-ms-transform:none" (Css.ms_transform [ None ]);
  check "-o-transform:none" (Css.o_transform [ None ]);
  check "-webkit-animation-duration:1s" (Css.webkit_animation_duration (S 1.));
  check "-moz-animation-name:spin" (Css.moz_animation_name (Name "spin"));
  check "-webkit-flex-wrap:wrap" (Css.webkit_flex_wrap Wrap);
  check "-webkit-align-items:center" (Css.webkit_align_items Center);
  check "-webkit-box-sizing:border-box" (Css.webkit_box_sizing Border_box);
  check "-moz-box-sizing:border-box" (Css.moz_box_sizing Border_box);
  check "-webkit-text-fill-color:red" (Css.webkit_text_fill_color (Named Red));
  check "-moz-appearance:none" (Css.moz_appearance None);
  check "-moz-user-select:none" (Css.moz_user_select None);
  check "-ms-user-select:none" (Css.ms_user_select None)

(* The five decoration-skip properties and text-emphasis-skip were built by
   Declaration and unreachable through the facade. *)
let decoration_skip_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "text-decoration-skip:none" (Css.text_decoration_skip None);
  check "text-decoration-skip-self:objects"
    (Css.text_decoration_skip_self Objects);
  check "text-decoration-skip-box:all" (Css.text_decoration_skip_box All);
  check "text-decoration-skip-inset:auto" (Css.text_decoration_skip_inset Auto);
  check "text-decoration-skip-spaces:start end"
    (Css.text_decoration_skip_spaces (Spaces [ Start; End ]));
  check "text-emphasis-skip:spaces punctuation"
    (Css.text_emphasis_skip (Skip [ Spaces; Punctuation ]))

(* CSS Logical 1 sec. 4.2 gives the flow-relative borders the same shorthand
   shape the physical ones have, and only [border-block] reached the facade. *)
let logical_border_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "border-block-start:none" (Css.border_block_start None);
  check "border-block-end:none" (Css.border_block_end None);
  check "border-inline:none" (Css.border_inline None);
  check "border-inline-start:none" (Css.border_inline_start None);
  check "border-inline-end:none" (Css.border_inline_end None)

(* Sixteen typography longhands the AST models and the facade could not
   write. *)
let typography_longhand_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "white-space-collapse:preserve" (Css.white_space_collapse Preserve);
  check "line-height-step:4px" (Css.line_height_step (Px 4.));
  check "font-palette:dark" (Css.font_palette Dark);
  check "font-synthesis:weight style"
    (Css.font_synthesis (Features [ Weight; Style ]));
  check "font-size-adjust:from-font" (Css.font_size_adjust From_font);
  check "font-variant-emoji:emoji" (Css.font_variant_emoji Emoji);
  check "font-variant-alternates:historical-forms"
    (Css.font_variant_alternates (Alternates [ Historical_forms ]));
  check "font-variant:none" (Css.font_variant None);
  check "text-wrap-style:balance" (Css.text_wrap_style Balance);
  check "text-box-trim:trim-both" (Css.text_box_trim Trim_both);
  check "text-box:auto" (Css.text_box (Box (None, Some Auto)));
  check "text-spacing-trim:trim-start" (Css.text_spacing_trim Trim_start);
  check "initial-letter-align:hanging"
    (Css.initial_letter_align (Align [ Hanging ]));
  check "initial-letter-wrap:first" (Css.initial_letter_wrap First)

(* The last of the modelled properties the facade could not write: shapes,
   overflow, images, margins, the top layer and the two position axes. *)
let remaining_longhand_builders () =
  let check expected declaration =
    Alcotest.(check string)
      expected expected
      (Css.Declaration.to_string ~minify:true declaration)
  in
  check "shape-image-threshold:.5" (Css.shape_image_threshold (Number 0.5));
  check "shape-margin:1px" (Css.shape_margin (Length (Px 1.)));
  check "shape-outside:circle(50%)" (Css.shape_outside "circle(50%)");
  check "overflow-clip-margin:content-box 2px"
    (Css.overflow_clip_margin (Clip_margin (Some Content_box, Some (Px 2.))));
  check "overflow-anchor:none" (Css.overflow_anchor None);
  check "overflow-block:clip" (Css.overflow_block Clip);
  check "overflow-inline:auto" (Css.overflow_inline Auto);
  check "overscroll-behavior-block:contain"
    (Css.overscroll_behavior_block Contain);
  check "overscroll-behavior-inline:none" (Css.overscroll_behavior_inline None);
  check "image-orientation:from-image" (Css.image_orientation From_image);
  check "image-rendering:pixelated" (Css.image_rendering Pixelated);
  check "image-resolution:from-image" (Css.image_resolution From_image);
  check "margin-trim:block" (Css.margin_trim Block);
  check "overlay:auto" (Css.overlay Auto);
  check "animation-composition:add"
    (Css.animation_composition (Compositions [ Add ]));
  check "background-position-x:center" (Css.background_position_x Center);
  check "background-position-y:bottom 10px"
    (Css.background_position_y (Edge_offset (Bottom, Length (Px 10.))));
  check "grid:none" (Css.grid None);
  check "-webkit-text-stroke:1px red"
    (Css.webkit_text_stroke { width = Some (Px 1.); color = Some (Named Red) });
  check "size:A4" (Css.page_size (Named A4))

let suite =
  ( "css_builders",
    [
      Alcotest.test_case "public svg longhand builders" `Quick
        svg_longhand_builders;
      Alcotest.test_case "public anchor positioning builders" `Quick
        anchor_positioning_builders;
      Alcotest.test_case "public view transition builders" `Quick
        view_transition_builders;
      Alcotest.test_case "public all shorthand builder" `Quick
        all_shorthand_builder;
      Alcotest.test_case "public motion path builders" `Quick
        motion_path_builders;
      Alcotest.test_case "public multicol builders" `Quick multicol_builders;
      Alcotest.test_case "public border image builders" `Quick
        border_image_builders;
      Alcotest.test_case "public contain intrinsic size builders" `Quick
        contain_intrinsic_size_builders;
      Alcotest.test_case "public scroll driven animation builders" `Quick
        scroll_driven_animation_builders;
      Alcotest.test_case "public vendor prefixed builders" `Quick
        vendor_prefixed_builders;
      Alcotest.test_case "public decoration skip builders" `Quick
        decoration_skip_builders;
      Alcotest.test_case "public logical border builders" `Quick
        logical_border_builders;
      Alcotest.test_case "public typography longhand builders" `Quick
        typography_longhand_builders;
      Alcotest.test_case "public remaining longhand builders" `Quick
        remaining_longhand_builders;
    ] )
