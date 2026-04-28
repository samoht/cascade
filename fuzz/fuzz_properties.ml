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

let find_sub s pat start =
  let s_len = String.length s and pat_len = String.length pat in
  let rec loop i =
    if i + pat_len > s_len then None
    else if String.sub s i pat_len = pat then Some i
    else loop (i + 1)
  in
  loop start

let quoted_strings s =
  let rec loop acc i =
    match find_sub s "\"" i with
    | None -> List.rev acc
    | Some start -> (
        match find_sub s "\"" (start + 1) with
        | None -> List.rev acc
        | Some stop ->
            let value = String.sub s (start + 1) (stop - start - 1) in
            loop (value :: acc) (stop + 1))
  in
  loop [] 0

let ocaml_string_literals s =
  let len = String.length s in
  let buffer = Buffer.create 32 in
  let rec scan acc i =
    if i >= len then List.rev acc
    else if s.[i] <> '"' then scan acc (i + 1)
    else (
      Buffer.clear buffer;
      string acc (i + 1))
  and string acc i =
    if i >= len then List.rev acc
    else
      match s.[i] with
      | '"' ->
          let value = Buffer.contents buffer in
          scan (value :: acc) (i + 1)
      | '\\' when i + 1 < len ->
          let c = s.[i + 1] in
          Buffer.add_char buffer
            (match c with
            | '"' -> '"'
            | '\\' -> '\\'
            | 'n' -> '\n'
            | 't' -> '\t'
            | c -> c);
          string acc (i + 2)
      | c ->
          Buffer.add_char buffer c;
          string acc (i + 1)
  in
  scan [] 0

let source_slice s ~first ~last =
  match (find_sub s first 0, find_sub s last 0) with
  | Some start, Some stop when stop > start -> String.sub s start (stop - start)
  | _ -> ""

let read_source_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let deterministic_manifest_properties =
  lazy
    (let source =
       try read_source_file "test/test_declaration.ml"
       with Sys_error _ -> read_source_file "../test/test_declaration.ml"
     in
     let manifest =
       source_slice source ~first:"type property_grammar_row"
         ~last:"let spec_property_grammar_manifest"
     in
     let rec property_fields acc i =
       match find_sub manifest "property = \"" i with
       | None -> acc
       | Some start -> (
           let value_start = start + String.length "property = \"" in
           match find_sub manifest "\"" value_start with
           | None -> acc
           | Some stop ->
               let property =
                 String.sub manifest value_start (stop - value_start)
               in
               property_fields (property :: acc) (stop + 1))
     in
     let rec property_row_blocks acc i =
       match find_sub manifest "property_grammar_rows" i with
       | None -> acc
       | Some start -> (
           match find_sub manifest "[" start with
           | None -> acc
           | Some block_start -> (
               match find_sub manifest "]" block_start with
               | None -> acc
               | Some block_stop ->
                   let block =
                     String.sub manifest block_start (block_stop - block_start)
                   in
                   property_row_blocks
                     (quoted_strings block @ acc)
                     (block_stop + 1)))
     in
     let names = property_fields [] 0 @ property_row_blocks [] 0 in
     List.sort_uniq String.compare names)

type deterministic_manifest_row = {
  name : string;
  positive_values : string list;
  negative_values : string list;
}

let bracket_payload source start =
  match find_sub source "[" start with
  | None -> None
  | Some open_i ->
      let rec loop depth i =
        if i >= String.length source then None
        else
          match source.[i] with
          | '[' -> loop (depth + 1) (i + 1)
          | ']' when depth = 1 ->
              Some (open_i, i, String.sub source open_i (i - open_i + 1))
          | ']' -> loop (depth - 1) (i + 1)
          | _ -> loop depth (i + 1)
      in
      loop 0 open_i

let deterministic_manifest_rows =
  lazy
    (let source =
       try read_source_file "test/test_declaration.ml"
       with Sys_error _ -> read_source_file "../test/test_declaration.ml"
     in
     let manifest =
       source_slice source ~first:"type property_grammar_row"
         ~last:"let spec_property_grammar_manifest"
     in
     let rec grouped_rows acc i =
       match find_sub manifest "property_grammar_rows" i with
       | None -> acc
       | Some start -> (
           match bracket_payload manifest start with
           | None -> acc
           | Some (_, props_end, props_src) -> (
               match bracket_payload manifest (props_end + 1) with
               | None -> acc
               | Some (_, positives_end, positives_src) -> (
                   match bracket_payload manifest (positives_end + 1) with
                   | None -> acc
                   | Some (_, negatives_end, negatives_src) ->
                       let positives = ocaml_string_literals positives_src in
                       let negatives = ocaml_string_literals negatives_src in
                       let rows =
                         List.map
                           (fun name ->
                             {
                               name;
                               positive_values = positives;
                               negative_values = negatives;
                             })
                           (ocaml_string_literals props_src)
                       in
                       grouped_rows (rows @ acc) (negatives_end + 1))))
     in
     let rec explicit_rows acc i =
       match find_sub manifest "property = \"" i with
       | None -> acc
       | Some start -> (
           let name_start = start + String.length "property = \"" in
           match find_sub manifest "\"" name_start with
           | None -> acc
           | Some name_end -> (
               let name =
                 String.sub manifest name_start (name_end - name_start)
               in
               match find_sub manifest "positives =" name_end with
               | None -> explicit_rows acc (name_end + 1)
               | Some positives_start -> (
                   match bracket_payload manifest positives_start with
                   | None -> explicit_rows acc (name_end + 1)
                   | Some (_, positives_end, positives_src) -> (
                       match find_sub manifest "negatives =" positives_end with
                       | None -> explicit_rows acc (positives_end + 1)
                       | Some negatives_start -> (
                           match bracket_payload manifest negatives_start with
                           | None -> explicit_rows acc (positives_end + 1)
                           | Some (_, negatives_end, negatives_src) ->
                               let row =
                                 {
                                   name;
                                   positive_values =
                                     ocaml_string_literals positives_src;
                                   negative_values =
                                     ocaml_string_literals negatives_src;
                                 }
                               in
                               explicit_rows (row :: acc) (negatives_end + 1))))
               ))
     in
     let rows = explicit_rows [] 0 @ grouped_rows [] 0 in
     let table = Hashtbl.create 512 in
     List.iter (fun row -> Hashtbl.replace table row.name row) rows;
     Hashtbl.fold (fun _ row acc -> row :: acc) table []
     |> List.sort (fun a b -> String.compare a.name b.name))

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

let test_deterministic_manifest_css_wide_generation buf =
  let properties = Lazy.force deterministic_manifest_properties in
  if List.length properties < 346 then
    fail
      (Fmt.str
         "deterministic property manifest extraction drifted: only %d rows"
         (List.length properties));
  let property = pick properties buf 0 in
  let keyword =
    pick [ "initial"; "inherit"; "unset"; "revert"; "revert-layer" ] buf 1
  in
  let input = property ^ ":" ^ keyword in
  let c = Css.Cursor.of_string input in
  match Css.Declaration.read_declaration c with
  | None ->
      fail
        (Fmt.str
           "deterministic manifest CSS-wide generated declaration rejected: %S"
           input)
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

let test_deterministic_manifest_positive_values buf =
  let rows = Lazy.force deterministic_manifest_rows in
  if List.length rows < 346 then
    fail
      (Fmt.str "deterministic property manifest row extraction drifted: %d rows"
         (List.length rows));
  let row = pick rows buf 0 in
  let value = pick row.positive_values buf 1 in
  let input = row.name ^ ":" ^ value in
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

let test_deterministic_manifest_negative_values buf =
  let rows = Lazy.force deterministic_manifest_rows in
  if List.length rows < 346 then
    fail
      (Fmt.str "deterministic property manifest row extraction drifted: %d rows"
         (List.length rows));
  let row = pick rows buf 0 in
  let value = pick row.negative_values buf 1 in
  let input = row.name ^ ":" ^ value in
  match parse_declaration input with
  | None -> ()
  | Some decl ->
      fail
        (Fmt.str "deterministic manifest negative declaration parsed: %S -> %S"
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
      test_case "deterministic manifest CSS-wide generated vectors" [ bytes ]
        test_deterministic_manifest_css_wide_generation;
      test_case "deterministic manifest positive value vectors" [ bytes ]
        test_deterministic_manifest_positive_values;
      test_case "deterministic manifest negative value vectors" [ bytes ]
        test_deterministic_manifest_negative_values;
    ] )
