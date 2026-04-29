(** Fuzz tests for individual CSS property value parsers. *)

open Cascade
open Alcobar

let cssish buf =
  let alphabet =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \
     -_#@.%(),;:![]{}\\\"'/*"
  in
  let n = String.length alphabet in
  String.map (fun c -> alphabet.[Char.code c mod n]) buf

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)

let property_inventory : Cascade_spec_inventory.Property_grammar.row list =
  Cascade_spec_inventory.Property_grammar.rows

let inventory_properties =
  Cascade_spec_inventory.Property_grammar.property_names

let check_reader reader printer input =
  let r = Css.Cursor.of_string input in
  match try Some (reader r) with Css.Cursor.Parse_error _ -> None with
  | None -> ()
  | Some value -> (
      let once = Css.Pp.to_string ~minify:true printer value in
      let r2 = Css.Cursor.of_string once in
      match try Some (reader r2) with Css.Cursor.Parse_error _ -> None with
      | None -> ()
      | Some reparsed ->
          let twice = Css.Pp.to_string ~minify:true printer reparsed in
          ignore twice)

let reject_reader reader _property input =
  let r = Css.Cursor.of_string input in
  match try Some (reader r) with Css.Cursor.Parse_error _ -> None with
  | None -> ()
  | Some _ -> ()

let generated_property_vector buf =
  pick
    [
      ( "display",
        fun input ->
          check_reader Css.Properties.read_display Css.Properties.pp_display
            input );
      ( "position",
        fun input ->
          check_reader Css.Properties.read_position Css.Properties.pp_position
            input );
      ( "overflow",
        fun input ->
          check_reader Css.Properties.read_overflow Css.Properties.pp_overflow
            input );
      ( "border",
        fun input ->
          check_reader Css.Properties.read_border Css.Properties.pp_border input
      );
      ( "font-family",
        fun input ->
          check_reader Css.Properties.read_font_family
            Css.Properties.pp_font_family input );
      ( "font-weight",
        fun input ->
          check_reader Css.Properties.read_font_weight
            Css.Properties.pp_font_weight input );
      ( "font-feature-settings",
        fun input ->
          check_reader Css.Properties.read_font_feature_settings
            Css.Properties.pp_font_feature_settings input );
      ( "transform",
        fun input ->
          check_reader Css.Properties.read_transform Css.Properties.pp_transform
            input );
      ( "transforms",
        fun input ->
          check_reader Css.Properties.read_transforms
            Css.Properties.pp_transforms input );
      ( "timing-function",
        fun input ->
          check_reader Css.Properties.read_timing_function
            Css.Properties.pp_timing_function input );
      ( "transition",
        fun input ->
          check_reader Css.Properties.read_transition
            Css.Properties.pp_transition input );
      ( "animation",
        fun input ->
          check_reader Css.Properties.read_animation Css.Properties.pp_animation
            input );
      ( "background-image",
        fun input ->
          check_reader Css.Properties.read_background_image
            Css.Properties.pp_background_image input );
      ( "background",
        fun input ->
          check_reader Css.Properties.read_background
            Css.Properties.pp_background input );
      ( "content",
        fun input ->
          check_reader Css.Properties.read_content Css.Properties.pp_content
            input );
      ( "container",
        fun input ->
          check_reader Css.Properties.read_container_shorthand
            Css.Properties.pp_container_shorthand input );
      ( "scroll-snap-type",
        fun input ->
          check_reader Css.Properties.read_scroll_snap_type
            Css.Properties.pp_scroll_snap_type input );
      ( "clip-path",
        fun input ->
          check_reader Css.Properties.read_clip_path Css.Properties.pp_clip_path
            input );
    ]
    buf 0

let valid_value property buf =
  match property with
  | "display" -> pick [ "block"; "inline flex"; "grid"; "contents" ] buf 1
  | "position" -> pick [ "static"; "relative"; "absolute"; "sticky" ] buf 1
  | "overflow" -> pick [ "visible"; "hidden"; "clip"; "auto" ] buf 1
  | "border" ->
      pick [ "1px solid red"; "solid"; "0"; "thin currentColor" ] buf 1
  | "font-family" ->
      pick [ "Arial, sans-serif"; "\"A B\", serif"; "system-ui" ] buf 1
  | "font-weight" -> pick [ "400"; "700"; "1000"; "bold"; "lighter" ] buf 1
  | "font-feature-settings" ->
      pick [ "normal"; "\"kern\" 1"; "\"liga\" off" ] buf 1
  | "transform" ->
      pick [ "rotate(45deg)"; "translateX(10px)"; "scale(1.2)" ] buf 1
  | "transforms" -> pick [ "rotate(45deg) scale(1.2)"; "none" ] buf 1
  | "timing-function" ->
      pick [ "ease"; "steps(4, jump-end)"; "cubic-bezier(.1,.2,.3,.4)" ] buf 1
  | "transition" -> pick [ "opacity 1s ease"; "all .2s linear .1s" ] buf 1
  | "animation" ->
      pick [ "spin 1s linear infinite"; "none"; "fade .2s ease" ] buf 1
  | "background-image" ->
      pick [ "none"; "url(a.png)"; "linear-gradient(red, blue)" ] buf 1
  | "background" ->
      pick [ "red"; "url(a.png) no-repeat center/cover"; "none" ] buf 1
  | "content" -> pick [ "normal"; "\"hello\""; "attr(title)" ] buf 1
  | "container" -> pick [ "card / inline-size"; "inline-size"; "normal" ] buf 1
  | "scroll-snap-type" ->
      pick [ "x mandatory"; "block proximity"; "none" ] buf 1
  | "clip-path" -> pick [ "none"; "inset(10px)"; "circle(50%)" ] buf 1
  | _ -> cssish buf

let invalid_value property buf =
  match property with
  | "display" -> pick [ "block inline flex"; "nope" ] buf 2
  | "position" -> pick [ "sticky absolute"; "center" ] buf 2
  | "overflow" -> pick [ "visible hidden"; "none" ] buf 2
  | "border" -> pick [ "1px 2px"; "solid solid"; "red blue" ] buf 2
  | "font-family" -> pick [ "Arial,,serif"; "," ] buf 2
  | "font-weight" -> pick [ "1001"; "0"; "bold 400" ] buf 2
  | "font-feature-settings" -> pick [ "\"kern\" maybe"; "1" ] buf 2
  | "transform" -> pick [ "rotate()"; "scale()" ] buf 2
  | "transforms" -> pick [ "none rotate(1deg)"; "translate()" ] buf 2
  | "timing-function" -> pick [ "steps()"; "cubic-bezier(1,2)" ] buf 2
  | "transition" -> pick [ "1s 2s 3s"; "ease opacity ease" ] buf 2
  | "animation" -> pick [ "1s 2s 3s"; "infinite infinite" ] buf 2
  | "background-image" -> pick [ "linear-gradient()"; "image-set()" ] buf 2
  | "background" -> pick [ "red blue"; "url(" ] buf 2
  | "content" -> pick [ "attr()"; "open-quote close-quote none" ] buf 2
  | "container" -> pick [ "/ inline-size"; "card / inline-size / size" ] buf 2
  | "scroll-snap-type" -> pick [ "mandatory x"; "x y mandatory" ] buf 2
  | "clip-path" -> pick [ "circle()"; "inset()" ] buf 2
  | _ -> cssish buf

let test_property_reader_crash_safety buf =
  let _, run = generated_property_vector buf in
  run (cssish buf)

let test_generated_valid_property_idempotent buf =
  let property, run = generated_property_vector buf in
  run (valid_value property buf)

let test_invalid_property_stable buf =
  let property, run = generated_property_vector buf in
  run (invalid_value property buf)

let test_css_wide_keywords_parse buf =
  let property, run = generated_property_vector buf in
  let keyword =
    pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 3
  in
  match property with
  | "font-family" | "font-feature-settings" -> ()
  | _ -> run keyword

let test_css_wide_mixes_stable buf =
  let property, run = generated_property_vector buf in
  let keyword =
    pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 3
  in
  let value =
    match property with
    | "display" -> keyword ^ " block"
    | "position" -> keyword ^ " static"
    | "overflow" -> keyword ^ " hidden"
    | "border" -> keyword ^ " solid"
    | "font-weight" -> keyword ^ " bold"
    | "transform" | "transforms" -> keyword ^ " rotate(1deg)"
    | "transition" -> keyword ^ " opacity 1s"
    | "animation" -> keyword ^ " fade 1s"
    | "background-image" -> keyword ^ " url(a.png)"
    | "background" -> keyword ^ " red"
    | "content" -> keyword ^ " none"
    | "container" -> keyword ^ " / inline-size"
    | "scroll-snap-type" -> keyword ^ " mandatory"
    | "clip-path" -> keyword ^ " inset(1px)"
    | _ -> keyword ^ " " ^ valid_value property buf
  in
  run value

type property_grammar_vector = {
  property : string;
  positives : string list;
  negatives : string list;
  accept : string -> unit;
  reject : string -> unit;
}

let vector property reader printer positives negatives =
  {
    property;
    positives;
    negatives;
    accept = check_reader reader printer;
    reject = reject_reader reader property;
  }

let property_grammar_vectors =
  [
    vector "display" Css.Properties.read_display Css.Properties.pp_display
      [ "block"; "inline"; "inline flow-root"; "list-item flow-root" ]
      [ "block inline flex"; "unknown-display" ];
    vector "position" Css.Properties.read_position Css.Properties.pp_position
      [ "static"; "relative"; "absolute"; "fixed"; "sticky" ]
      [ "sticky absolute"; "center" ];
    vector "overflow" Css.Properties.read_overflow Css.Properties.pp_overflow
      [ "visible"; "hidden"; "clip"; "auto" ]
      [ "none"; "visible hidden scroll" ];
    vector "border" Css.Properties.read_border Css.Properties.pp_border
      [ "1px solid red"; "solid"; "0"; "thin currentColor" ]
      [ "1px 2px"; "solid solid"; "red blue" ];
    vector "font-family" Css.Properties.read_font_family
      Css.Properties.pp_font_family
      [ "Arial, sans-serif"; "\"A B\", serif"; "system-ui" ]
      [ "Arial,,serif"; "," ];
    vector "font-weight" Css.Properties.read_font_weight
      Css.Properties.pp_font_weight
      [ "normal"; "bold"; "400"; "650"; "1000"; "lighter" ]
      [ "1001"; "0"; "bold 400" ];
    vector "font-feature-settings" Css.Properties.read_font_feature_settings
      Css.Properties.pp_font_feature_settings
      [ "normal"; "\"kern\" 1"; "\"liga\" off" ]
      [ "\"kern\" maybe"; "1" ];
    vector "transform" Css.Properties.read_transform Css.Properties.pp_transform
      [ "translateX(10px)"; "rotate(45deg)"; "scale(1.2)" ]
      [ "translate()"; "scale()" ];
    vector "transforms" Css.Properties.read_transforms
      Css.Properties.pp_transforms
      [ "none"; "translateX(10px) rotate(45deg)"; "scale(1.2)" ]
      [ "none rotate(1deg)"; "translate()" ];
    vector "timing-function" Css.Properties.read_timing_function
      Css.Properties.pp_timing_function
      [ "ease"; "steps(4, jump-end)"; "cubic-bezier(.1,.2,.3,.4)" ]
      [ "steps()"; "cubic-bezier(1,2)" ];
    vector "transition" Css.Properties.read_transition
      Css.Properties.pp_transition
      [ "opacity 1s ease"; "all .2s linear .1s" ]
      [ "1s 2s 3s"; "ease opacity ease" ];
    vector "animation" Css.Properties.read_animation Css.Properties.pp_animation
      [ "spin 1s linear infinite"; "none"; "fade .2s ease" ]
      [ "1s 2s 3s"; "infinite infinite" ];
    vector "background-image" Css.Properties.read_background_image
      Css.Properties.pp_background_image
      [ "none"; "url(a.png)"; "linear-gradient(red, blue)" ]
      [ "linear-gradient()"; "image-set()" ];
    vector "background" Css.Properties.read_background
      Css.Properties.pp_background
      [ "red"; "url(a.png) no-repeat center/cover"; "none" ]
      [ "red blue"; "url(" ];
    vector "content" Css.Properties.read_content Css.Properties.pp_content
      [ "normal"; "\"hello\""; "attr(title)" ]
      [ "attr()"; "open-quote close-quote none" ];
    vector "container" Css.Properties.read_container_shorthand
      Css.Properties.pp_container_shorthand
      [ "card / inline-size"; "inline-size"; "normal" ]
      [ "/ inline-size"; "card / inline-size / size" ];
    vector "scroll-snap-type" Css.Properties.read_scroll_snap_type
      Css.Properties.pp_scroll_snap_type
      [ "x mandatory"; "block proximity"; "none" ]
      [ "mandatory x"; "x y mandatory" ];
    vector "clip-path" Css.Properties.read_clip_path Css.Properties.pp_clip_path
      [ "none"; "inset(10px)"; "circle(50%)" ]
      [ "circle()"; "inset()" ];
    vector "touch-action" Css.Properties.read_touch_action
      Css.Properties.pp_touch_action
      [ "auto"; "none"; "pan-x pinch-zoom" ]
      [ "pan-x pan-left"; "auto none" ];
    vector "contain" Css.Properties.read_contain Css.Properties.pp_contain
      [ "none"; "layout paint"; "strict"; "content" ]
      [ "layout layout"; "strict layout" ];
    vector "box-sizing" Css.Properties.read_box_sizing
      Css.Properties.pp_box_sizing
      [ "content-box"; "border-box" ]
      [ "padding-box"; "border-box content-box" ];
    vector "scroll-snap-align" Css.Properties.read_scroll_snap_align
      Css.Properties.pp_scroll_snap_align
      [ "none"; "start"; "start end"; "center" ]
      [ "start center end"; "foo" ];
    vector "scroll-snap-stop" Css.Properties.read_scroll_snap_stop
      Css.Properties.pp_scroll_snap_stop [ "normal"; "always" ]
      [ "normal always"; "sometimes" ];
    vector "background-repeat" Css.Properties.read_background_repeat
      Css.Properties.pp_background_repeat
      [ "repeat"; "no-repeat"; "repeat-x"; "space round" ]
      [ "repeat no-repeat space"; "foo" ];
    vector "background-size" Css.Properties.read_background_size
      Css.Properties.pp_background_size
      [ "auto"; "cover"; "contain"; "10px 20%" ]
      [ "cover contain"; "-1px" ];
    vector "background-position" Css.Properties.read_background_position
      Css.Properties.pp_background_position
      [ "center"; "left 10px top 20px"; "10% 20%" ]
      [ "left top center"; "foo" ];
    vector "mask-composite" Css.Properties.read_mask_composite
      Css.Properties.pp_mask_composite
      [ "add"; "subtract"; "intersect"; "exclude" ]
      [ "source-over"; "add subtract" ];
    vector "mask-mode" Css.Properties.read_mask_mode Css.Properties.pp_mask_mode
      [ "match-source"; "alpha"; "luminance" ]
      [ "match-source alpha"; "foo" ];
    vector "mask-type" Css.Properties.read_mask_type Css.Properties.pp_mask_type
      [ "alpha"; "luminance" ]
      [ "match-source"; "alpha luminance" ];
    vector "mask-box" Css.Properties.read_mask_box Css.Properties.pp_mask_box
      [ "border-box"; "padding-box"; "content-box"; "no-clip" ]
      [ "margin-box"; "border-box padding-box content-box content-box" ];
    vector "resize" Css.Properties.read_resize Css.Properties.pp_resize
      [ "none"; "both"; "horizontal"; "block" ]
      [ "horizontal vertical"; "auto" ];
    vector "object-fit" Css.Properties.read_object_fit
      Css.Properties.pp_object_fit
      [ "fill"; "contain"; "cover"; "scale-down" ]
      [ "contain cover"; "auto" ];
    vector "appearance" Css.Properties.read_appearance
      Css.Properties.pp_appearance
      [ "none"; "auto"; "textfield" ]
      [ "none auto"; "foo" ];
    vector "color-scheme" Css.Properties.read_color_scheme
      Css.Properties.pp_color_scheme
      [ "normal"; "light"; "dark"; "only light" ]
      [ "normal light"; "only" ];
    vector "text-overflow" Css.Properties.read_text_overflow
      Css.Properties.pp_text_overflow
      [ "clip"; "ellipsis"; "\"...\""; "clip ellipsis" ]
      [ "clip ellipsis clip"; "auto" ];
    vector "text-wrap" Css.Properties.read_text_wrap Css.Properties.pp_text_wrap
      [ "wrap"; "nowrap"; "balance"; "pretty" ]
      [ "wrap nowrap"; "auto" ];
    vector "overflow-wrap" Css.Properties.read_overflow_wrap
      Css.Properties.pp_overflow_wrap
      [ "normal"; "break-word"; "anywhere" ]
      [ "normal anywhere"; "break-all" ];
    vector "hyphens" Css.Properties.read_hyphens Css.Properties.pp_hyphens
      [ "none"; "manual"; "auto" ]
      [ "manual auto"; "normal" ];
    vector "animation-iteration-count"
      Css.Properties.read_animation_iteration_count
      Css.Properties.pp_animation_iteration_count [ "infinite"; "1"; "2.5" ]
      [ "-1"; "infinite infinite" ];
    vector "animation-direction" Css.Properties.read_animation_direction
      Css.Properties.pp_animation_direction
      [ "normal"; "reverse"; "alternate"; "alternate-reverse" ]
      [ "normal reverse"; "forwards" ];
    vector "animation-fill-mode" Css.Properties.read_animation_fill_mode
      Css.Properties.pp_animation_fill_mode
      [ "none"; "forwards"; "backwards"; "both" ]
      [ "none forwards"; "running" ];
    vector "animation-play-state" Css.Properties.read_animation_play_state
      Css.Properties.pp_animation_play_state [ "running"; "paused" ]
      [ "running paused"; "none" ];
    vector "overscroll-behavior" Css.Properties.read_overscroll_behavior
      Css.Properties.pp_overscroll_behavior
      [ "auto"; "contain"; "none" ]
      [ "contain none"; "hidden" ];
    vector "direction" Css.Properties.read_direction Css.Properties.pp_direction
      [ "ltr"; "rtl" ] [ "ltr rtl"; "auto" ];
    vector "unicode-bidi" Css.Properties.read_unicode_bidi
      Css.Properties.pp_unicode_bidi
      [ "normal"; "embed"; "isolate"; "plaintext" ]
      [ "normal isolate"; "auto" ];
    vector "writing-mode" Css.Properties.read_writing_mode
      Css.Properties.pp_writing_mode
      [ "horizontal-tb"; "vertical-rl"; "sideways-rl" ]
      [ "vertical"; "vertical-rl horizontal-tb" ];
    vector "caption-side" Css.Properties.read_caption_side
      Css.Properties.pp_caption_side [ "top"; "bottom" ]
      [ "left"; "top bottom" ];
    vector "field-sizing" Css.Properties.read_field_sizing
      Css.Properties.pp_field_sizing [ "fixed"; "content" ]
      [ "auto"; "fixed content" ];
    vector "text-decoration-line" Css.Properties.read_text_decoration_line
      Css.Properties.pp_text_decoration_line
      [ "none"; "underline"; "underline overline line-through" ]
      [ "none underline"; "underline underline" ];
    vector "text-decoration-style" Css.Properties.read_text_decoration_style
      Css.Properties.pp_text_decoration_style
      [ "solid"; "double"; "dotted"; "wavy" ]
      [ "solid wavy"; "none" ];
    vector "font-style" Css.Properties.read_font_style
      Css.Properties.pp_font_style
      [ "normal"; "italic"; "oblique"; "oblique 20deg" ]
      [ "italic normal"; "oblique 20px" ];
    vector "table-layout" Css.Properties.read_table_layout
      Css.Properties.pp_table_layout [ "auto"; "fixed" ]
      [ "fixed auto"; "block" ];
    vector "border-collapse" Css.Properties.read_border_collapse
      Css.Properties.pp_border_collapse [ "collapse"; "separate" ]
      [ "collapse separate"; "none" ];
  ]

let test_property_grammar_manifest_valid buf =
  let row = pick property_grammar_vectors buf 0 in
  row.accept (pick row.positives buf 1)

let test_property_grammar_manifest_invalid buf =
  let row = pick property_grammar_vectors buf 0 in
  row.reject (pick row.negatives buf 1)

let test_property_grammar_manifest_has_both_kinds _buf =
  let names = List.map (fun row -> row.property) property_grammar_vectors in
  let unique_names = List.sort_uniq String.compare names in
  if List.length names <> List.length unique_names then
    fail "property grammar fuzz manifest has duplicate property rows";
  List.iter
    (fun row ->
      if row.positives = [] then
        fail (Fmt.str "%s fuzz row has no positive vectors" row.property);
      if row.negatives = [] then
        fail (Fmt.str "%s fuzz row has no negative vectors" row.property))
    property_grammar_vectors

let test_inventory_css_wide_generation buf =
  if List.length inventory_properties < 346 then
    fail
      (Fmt.str "shared property grammar inventory drifted: only %d rows"
         (List.length inventory_properties));
  let property = pick inventory_properties buf 0 in
  let keyword =
    pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 1
  in
  let input = property ^ ":" ^ keyword in
  let c = Css.Cursor.of_string input in
  match Css.Declaration.read_declaration c with
  | None -> ()
  | Some decl -> (
      let serialized =
        Css.Declaration.string_of_declaration ~minify:true decl
      in
      let c2 = Css.Cursor.of_string serialized in
      match Css.Declaration.read_declaration c2 with
      | Some reparsed when decl = reparsed -> ()
      | _ ->
          fail
            (Fmt.str
               "deterministic manifest CSS-wide declaration did not \
                structurally roundtrip: %S -> %S"
               input serialized))

let parse_declaration input =
  let c = Css.Cursor.of_string input in
  try Css.Declaration.read_declaration c with Css.Cursor.Parse_error _ -> None

let assert_decl_roundtrip label input =
  match parse_declaration input with
  | None -> fail (Fmt.str "%s declaration rejected: %S" label input)
  | Some decl -> (
      let serialized =
        Css.Declaration.string_of_declaration ~minify:true decl
      in
      match parse_declaration serialized with
      | Some reparsed when decl = reparsed -> ()
      | _ ->
          fail
            (Fmt.str "%s declaration did not structurally roundtrip: %S -> %S"
               label input serialized))

let assert_decl_reject label input =
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      fail
        (Fmt.str "%s invalid declaration parsed: %S -> %S" label input
           (Css.Declaration.string_of_declaration ~minify:true decl))

let has_literal literal value = Re.execp Re.(compile (str literal)) value

let invalid_property_mutation
    (row : Cascade_spec_inventory.Property_grammar.row) value buf =
  match byte_at buf 4 mod 6 with
  | 0 -> pick row.negatives buf 5
  | 1 -> "initial " ^ value
  | 2 -> "inherit " ^ value
  | 3 -> "var()"
  | 4 -> if has_literal ")" value then value ^ " )" else value ^ " " ^ value
  | _ -> value ^ " " ^ pick row.negatives buf 6

let test_inventory_positive_values buf =
  let rows = property_inventory in
  if List.length rows < 346 then
    fail
      (Fmt.str "shared property grammar inventory drifted: %d rows"
         (List.length rows));
  let row = pick rows buf 0 in
  let value = pick row.positives buf 1 in
  let input = row.property ^ ":" ^ value in
  match parse_declaration input with
  | None ->
      fail
        (Fmt.str "deterministic manifest positive declaration rejected: %S"
           input)
  | Some decl -> (
      let serialized =
        Css.Declaration.string_of_declaration ~minify:true decl
      in
      match parse_declaration serialized with
      | Some reparsed when decl = reparsed -> ()
      | _ ->
          fail
            (Fmt.str
               "deterministic manifest positive declaration did not \
                structurally roundtrip: %S -> %S"
               input serialized))

let test_inventory_negative_values buf =
  let rows = property_inventory in
  if List.length rows < 346 then
    fail
      (Fmt.str "shared property grammar inventory drifted: %d rows"
         (List.length rows));
  let row = pick rows buf 0 in
  let value = pick row.negatives buf 1 in
  let input = row.property ^ ":" ^ value in
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      fail
        (Fmt.str "deterministic manifest negative declaration parsed: %S -> %S"
           input
           (Css.Declaration.string_of_declaration ~minify:true decl))

let test_inventory_var_values buf =
  let rows =
    List.filter
      (fun (row : Cascade_spec_inventory.Property_grammar.row) ->
        row.property <> "all")
      property_inventory
  in
  if List.length rows < 345 then
    fail
      (Fmt.str "shared property grammar inventory drifted: %d rows"
         (List.length rows));
  let row = pick rows buf 0 in
  let fallback = pick row.positives buf 1 in
  let input = row.property ^ ":var(--spec-value," ^ fallback ^ ")" in
  match parse_declaration input with
  | None ->
      fail
        (Fmt.str "deterministic manifest var() declaration rejected: %S" input)
  | Some decl -> (
      let serialized =
        Css.Declaration.string_of_declaration ~minify:true decl
      in
      match parse_declaration serialized with
      | Some reparsed when decl = reparsed -> ()
      | _ ->
          fail
            (Fmt.str
               "deterministic manifest var() declaration did not structurally \
                roundtrip: %S -> %S"
               input serialized))

let test_inventory_valid_generation buf =
  let row = pick property_inventory buf 0 in
  let value =
    match byte_at buf 2 mod 4 with
    | 0 -> pick row.positives buf 1
    | 1 ->
        pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 1
    | 2 when row.property <> "all" ->
        let fallback = pick row.positives buf 1 in
        "var(--spec-value," ^ fallback ^ ")"
    | _ -> pick row.positives buf 1
  in
  assert_decl_roundtrip "shared-inventory generated valid"
    (row.property ^ ":" ^ value)

let test_inventory_invalid_mutation buf =
  let row = pick property_inventory buf 0 in
  let value = pick row.positives buf 1 in
  let invalid = invalid_property_mutation row value buf in
  assert_decl_reject "shared-inventory generated invalid"
    (row.property ^ ":" ^ invalid)

let test_property_value_branch_depth_positive buf =
  let input =
    pick
      [
        "background:url(bg.png) no-repeat left 10px top 20%/cover border-box \
         content-box";
        "background-image:image-set(url(a.avif) type(\"image/avif\") \
         1x,url(a.png) type(\"image/png\") 1x)";
        "border-image:linear-gradient(red,blue) 30 fill/10px/1 stretch";
        "clip-path:xywh(0 0 100% 100% round 10px)";
        "shape-outside:inset(10px round 2px)";
        "width:clamp(10px,5vw,100px)";
        "width:round(nearest,10px,3px)";
        "width:calc-size(auto,size + 1rem)";
        "opacity:sign(var(--delta))";
        "color:rgb(from var(--c) r g b/50%)";
        "font:italic small-caps 650 condensed 16px/1.5 \"Brand\",serif";
        "grid-template:\"head head\" auto \"nav main\" 1fr/12rem 1fr";
        "animation:fade 1s linear .2s 2 alternate both running";
        "transition:opacity 1s ease-in .2s allow-discrete";
        "scroll-timeline:--scroller block";
        "view-timeline:--reveal inline";
        "container:card/inline-size";
        "position-try-fallbacks:--below,flip-block,--above";
      ]
      buf 2
  in
  match parse_declaration input with
  | None ->
      fail
        (Fmt.str "property branch-depth positive declaration rejected: %S" input)
  | Some decl -> (
      let serialized =
        Css.Declaration.string_of_declaration ~minify:true decl
      in
      match parse_declaration serialized with
      | Some reparsed when decl = reparsed -> ()
      | _ ->
          fail
            (Fmt.str
               "property branch-depth declaration did not structurally \
                roundtrip: %S -> %S"
               input serialized))

let test_property_value_branch_depth_negative buf =
  let input =
    pick
      [
        "background:red blue";
        "background-image:image-set(url(a.png))";
        "border-image:linear-gradient(red,blue) fill fill";
        "clip-path:xywh(0 0)";
        "shape-outside:circle()";
        "width:clamp(10px,20px)";
        "width:round(10px)";
        "width:calc-size()";
        "opacity:sign()";
        "color:rgb(from red r g)";
        "font:bold serif";
        "grid-template:none/1fr";
        "animation:1s 2s 3s";
        "transition:allow-discrete allow-discrete";
        "scroll-timeline:block --scroller";
        "view-timeline:inline --reveal";
        "container:card/inline-size/size";
        "position-try-fallbacks:flip-block --below";
      ]
      buf 3
  in
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      fail
        (Fmt.str "property branch-depth negative declaration parsed: %S -> %S"
           input
           (Css.Declaration.string_of_declaration ~minify:true decl))

let suite =
  ( "properties",
    [
      test_case "property reader crash safety" [ bytes ]
        test_property_reader_crash_safety;
      test_case "generated valid property idempotent" [ bytes ]
        test_generated_valid_property_idempotent;
      test_case "generated invalid property rejected or stable" [ bytes ]
        test_invalid_property_stable;
      test_case "css-wide keywords parse" [ bytes ] test_css_wide_keywords_parse;
      test_case "css-wide mixes stable" [ bytes ] test_css_wide_mixes_stable;
      test_case "property grammar manifest valid vectors" [ bytes ]
        test_property_grammar_manifest_valid;
      test_case "property grammar manifest invalid vectors" [ bytes ]
        test_property_grammar_manifest_invalid;
      test_case "property grammar manifest row shape" [ bytes ]
        test_property_grammar_manifest_has_both_kinds;
      test_case "shared inventory CSS-wide generated vectors" [ bytes ]
        test_inventory_css_wide_generation;
      test_case "shared inventory positive value vectors" [ bytes ]
        test_inventory_positive_values;
      test_case "shared inventory negative value vectors" [ bytes ]
        test_inventory_negative_values;
      test_case "shared inventory var() value vectors" [ bytes ]
        test_inventory_var_values;
      test_case "shared inventory generated valid declarations" [ bytes ]
        test_inventory_valid_generation;
      test_case "shared inventory invalid declaration mutations" [ bytes ]
        test_inventory_invalid_mutation;
      test_case "property value branch-depth positive vectors" [ bytes ]
        test_property_value_branch_depth_positive;
      test_case "property value branch-depth negative vectors" [ bytes ]
        test_property_value_branch_depth_negative;
    ] )
