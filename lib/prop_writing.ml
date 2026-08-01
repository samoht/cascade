(* CSS Writing Modes 4: [writing-mode], [direction], [unicode-bidi],
   [text-orientation], [glyph-orientation-vertical], [text-combine-upright] and
   the baseline properties ([baseline-source], [alignment-baseline],
   [dominant-baseline], [baseline-shift]).

   The [baseline] keyword of the box-alignment properties is a different value
   space and stays with the alignment family.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Values
open Properties_intf
open Prop_common

let rec pp_text_orientation : text_orientation Pp.t =
 fun ctx -> function
  | Mixed -> Pp.string ctx "mixed"
  | Upright -> Pp.string ctx "upright"
  | Sideways -> Pp.string ctx "sideways"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_text_orientation ctx v

let rec pp_glyph_orientation_vertical : glyph_orientation_vertical Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Angle angle -> pp_angle ctx angle
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_glyph_orientation_vertical ctx v

let rec read_text_orientation t : text_orientation =
  Cursor.enum_or_var "text-orientation"
    [
      ("mixed", (Mixed : text_orientation));
      ("upright", Upright);
      ("sideways", Sideways);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_orientation t))
    t

let rec read_glyph_orientation_vertical t : glyph_orientation_vertical =
  Cursor.enum_or_var "glyph-orientation-vertical"
    [
      ("auto", (Auto : glyph_orientation_vertical));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (read_var read_glyph_orientation_vertical t)
        : glyph_orientation_vertical))
    ~default:(fun t ->
      let angle =
        Cursor.one_of
          [ read_angle; (fun t -> (Deg (Cursor.number t) : angle)) ]
          t
      in
      match angle with
      | Deg value when Float.equal value 0. || Float.equal value 90. ->
          (Angle angle : glyph_orientation_vertical)
      | _ -> Cursor.err_invalid t "glyph-orientation-vertical")
    t

let normalize_baseline_shift : baseline_shift -> baseline_shift =
 fun value ->
  match value with
  | Shift lp ->
      preserve_if_equal value (Shift (Values.normalize_length_percentage lp))
  | other -> other

let rec pp_baseline_source : baseline_source Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | First -> Pp.string ctx "first"
  | Last -> Pp.string ctx "last"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_baseline_source ctx v

let rec pp_alignment_baseline : alignment_baseline Pp.t =
 fun ctx -> function
  | Baseline -> Pp.string ctx "baseline"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Middle -> Pp.string ctx "middle"
  | Central -> Pp.string ctx "central"
  | Text_top -> Pp.string ctx "text-top"
  | Ideographic -> Pp.string ctx "ideographic"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Hanging -> Pp.string ctx "hanging"
  | Mathematical -> Pp.string ctx "mathematical"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_alignment_baseline ctx v

let rec pp_baseline_shift : baseline_shift Pp.t =
 fun ctx -> function
  | Shift value -> pp_length_percentage ~always:true ctx value
  | Sub -> Pp.string ctx "sub"
  | Super -> Pp.string ctx "super"
  | Top -> Pp.string ctx "top"
  | Center -> Pp.string ctx "center"
  | Bottom -> Pp.string ctx "bottom"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> Values.pp_var pp_baseline_shift ctx v

let rec pp_dominant_baseline : dominant_baseline Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Alphabetic -> Pp.string ctx "alphabetic"
  | Ideographic -> Pp.string ctx "ideographic"
  | Mathematical -> Pp.string ctx "mathematical"
  | Central -> Pp.string ctx "central"
  | Middle -> Pp.string ctx "middle"
  | Text_top -> Pp.string ctx "text-top"
  | Text_bottom -> Pp.string ctx "text-bottom"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_dominant_baseline ctx v

let rec pp_direction : direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_direction ctx v
  | Ltr -> Pp.string ctx "ltr"
  | Rtl -> Pp.string ctx "rtl"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_unicode_bidi : unicode_bidi Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_unicode_bidi ctx v
  | Normal -> Pp.string ctx "normal"
  | Embed -> Pp.string ctx "embed"
  | Isolate -> Pp.string ctx "isolate"
  | Bidi_override -> Pp.string ctx "bidi-override"
  | Isolate_override -> Pp.string ctx "isolate-override"
  | Plaintext -> Pp.string ctx "plaintext"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_writing_mode : writing_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_writing_mode ctx v
  | Horizontal_tb -> Pp.string ctx "horizontal-tb"
  | Vertical_rl -> Pp.string ctx "vertical-rl"
  | Vertical_lr -> Pp.string ctx "vertical-lr"
  | Sideways_lr -> Pp.string ctx "sideways-lr"
  | Sideways_rl -> Pp.string ctx "sideways-rl"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_text_combine_upright : text_combine_upright Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_text_combine_upright ctx v
  | None -> Pp.string ctx "none"
  | All -> Pp.string ctx "all"
  | Digits None -> Pp.string ctx "digits"
  | Digits (Some n) ->
      Pp.string ctx "digits";
      Pp.space ctx ();
      Pp.int ctx n
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_baseline_source t : baseline_source =
  Cursor.enum_or_var "baseline-source"
    [
      ("auto", (Auto : baseline_source));
      ("first", First);
      ("last", Last);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_baseline_source t))
    t

let rec read_alignment_baseline t : alignment_baseline =
  Cursor.enum_or_var "alignment-baseline"
    [
      ("baseline", (Baseline : alignment_baseline));
      ("text-bottom", Text_bottom);
      ("middle", Middle);
      ("central", Central);
      ("text-top", Text_top);
      ("ideographic", Ideographic);
      ("alphabetic", Alphabetic);
      ("hanging", Hanging);
      ("mathematical", Mathematical);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_alignment_baseline t))
    t

let rec read_baseline_shift t : baseline_shift =
  Cursor.enum_or_var "baseline-shift"
    [
      ("sub", (Sub : baseline_shift));
      ("super", Super);
      ("top", Top);
      ("center", Center);
      ("bottom", Bottom);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_baseline_shift t))
    ~default:(fun t ->
      Shift (Values.read_length_percentage ~with_keywords:false t))
    t

let rec read_direction t : direction =
  Cursor.enum_or_var "direction"
    [
      ("ltr", (Ltr : direction));
      ("rtl", Rtl);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_direction t))
    t

let rec read_unicode_bidi t : unicode_bidi =
  Cursor.enum_or_var "unicode-bidi"
    [
      ("normal", (Normal : unicode_bidi));
      ("embed", Embed);
      ("isolate", Isolate);
      ("bidi-override", Bidi_override);
      ("isolate-override", Isolate_override);
      ("plaintext", Plaintext);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_unicode_bidi t))
    t

let rec read_writing_mode t : writing_mode =
  Cursor.enum_or_var "writing-mode"
    [
      ("horizontal-tb", (Horizontal_tb : writing_mode));
      ("vertical-rl", Vertical_rl);
      ("vertical-lr", Vertical_lr);
      ("sideways-lr", Sideways_lr);
      ("sideways-rl", Sideways_rl);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_writing_mode t))
    t

let read_text_combine_digits t =
  let n = Cursor.int t in
  if n < 2 || n > 4 then
    Cursor.err_invalid t "text-combine-upright digits must be 2, 3, or 4";
  n

let rec read_text_combine_upright t : text_combine_upright =
  Cursor.enum_or_var "text-combine-upright"
    [
      ("none", (None : text_combine_upright));
      ("all", All);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_text_combine_upright t))
    ~default:(fun t ->
      match Cursor.peek_ident t with
      | Some "digits" ->
          let _ = Cursor.ident t in
          Cursor.ws t;
          Digits (Cursor.option read_text_combine_digits t)
      | _ -> Cursor.err_expected t "text-combine-upright")
    t

let rec read_dominant_baseline t : dominant_baseline =
  Cursor.enum_or_var "dominant-baseline"
    [
      ("auto", (Auto : dominant_baseline));
      ("alphabetic", Alphabetic);
      ("ideographic", Ideographic);
      ("mathematical", Mathematical);
      ("central", Central);
      ("middle", Middle);
      ("text-top", Text_top);
      ("text-bottom", Text_bottom);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_dominant_baseline t))
    t
