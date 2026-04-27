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

let check_reader reader printer input =
  let r = Css.Cursor.of_string input in
  match try Some (reader r) with Css.Cursor.Parse_error _ -> None with
  | None -> ()
  | Some value -> (
      let once = Css.Pp.to_string ~minify:true printer value in
      let r2 = Css.Cursor.of_string once in
      match try Some (reader r2) with Css.Cursor.Parse_error _ -> None with
      | None -> fail (Fmt.str "property serialization did not reparse: %S" once)
      | Some reparsed ->
          let twice = Css.Pp.to_string ~minify:true printer reparsed in
          if once <> twice then
            fail (Fmt.str "property serialization changed: %S -> %S" once twice)
      )

let reject_reader reader property input =
  let r = Css.Cursor.of_string input in
  match try Some (reader r) with Css.Cursor.Parse_error _ -> None with
  | None -> ()
  | Some _ ->
      fail (Fmt.str "%s invalid grammar vector parsed: %S" property input)

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
  | "font-weight" -> pick [ "400"; "700"; "bold"; "lighter" ] buf 1
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
  | "font-weight" -> pick [ "1000"; "bold 400" ] buf 2
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
      [ "normal"; "bold"; "400"; "650"; "lighter" ]
      [ "1000"; "bold 400" ];
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
  ]

let test_property_grammar_manifest_valid buf =
  let row = pick property_grammar_vectors buf 0 in
  row.accept (pick row.positives buf 1)

let test_property_grammar_manifest_invalid buf =
  let row = pick property_grammar_vectors buf 0 in
  row.reject (pick row.negatives buf 1)

let test_property_grammar_manifest_has_both_kinds _buf =
  List.iter
    (fun row ->
      if row.positives = [] then
        fail (Fmt.str "%s fuzz row has no positive vectors" row.property);
      if row.negatives = [] then
        fail (Fmt.str "%s fuzz row has no negative vectors" row.property))
    property_grammar_vectors

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
    ] )
