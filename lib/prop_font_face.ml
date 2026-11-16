open Common
open Values
open Properties_intf
open Prop_common

let rec pp_font_display : font_display Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_font_display ctx v
  | Auto -> Pp.string ctx "auto"
  | Block -> Pp.string ctx "block"
  | Swap -> Pp.string ctx "swap"
  | Fallback -> Pp.string ctx "fallback"
  | Optional -> Pp.string ctx "optional"

let pp_unicode_range_range ctx start end_ =
  Pp.string ctx "U+";
  Pp.hex ctx start;
  Pp.char ctx '-';
  Pp.hex ctx end_

let unicode_range_wildcard start end_ : string option =
  let wildcard_for q : string option =
    let size = 1 lsl (4 * q) in
    if start mod size <> 0 || end_ <> start + size - 1 then None
    else
      let prefix = start / size in
      let prefix = if prefix = 0 then "" else hex_string prefix in
      let wildcard = "U+" ^ prefix ^ String.make q '?' in
      let range = "U+" ^ hex_string start ^ "-" ^ hex_string end_ in
      if String.length wildcard < String.length range then
        (Some wildcard : string option)
      else (None : string option)
  in
  let rec loop q =
    if q > 6 then (None : string option)
    else
      match wildcard_for q with
      | (Some _ : string option) as wildcard -> wildcard
      | None -> loop (q + 1)
  in
  loop 1

let rec pp_unicode_range : unicode_range Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_unicode_range ctx v
  | Single hex ->
      Pp.string ctx "U+";
      Pp.hex ctx hex
  | Range (start, end_) -> (
      let pp_range () = pp_unicode_range_range ctx start end_ in
      if not (Pp.minified ctx) then pp_range ()
      else
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_range ())
  | Padded_single (value, width) ->
      if Pp.minified ctx then pp_unicode_range ctx (Single value)
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex width value))
  | Padded_range { start; end_; start_width; end_width } ->
      if Pp.minified ctx then pp_unicode_range ctx (Range (start, end_))
      else (
        Pp.string ctx "U+";
        Pp.string ctx (padded_hex start_width start);
        Pp.char ctx '-';
        Pp.string ctx (padded_hex end_width end_))
  | Wildcard { prefix; prefix_width; wildcards } ->
      let start = prefix lsl (4 * wildcards) in
      let end_ = start + (1 lsl (4 * wildcards)) - 1 in
      if Pp.minified ctx then
        match unicode_range_wildcard start end_ with
        | Some wildcard -> Pp.string ctx wildcard
        | None -> pp_unicode_range_range ctx start end_
      else (
        Pp.string ctx "U+";
        if prefix_width > 0 then Pp.string ctx (padded_hex prefix_width prefix);
        Pp.string ctx (String.make wildcards '?'))

let rec pp_webkit_font_smoothing : webkit_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Antialiased -> Pp.string ctx "antialiased"
  | Subpixel_antialiased -> Pp.string ctx "subpixel-antialiased"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_moz_osx_font_smoothing : moz_osx_font_smoothing Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_moz_osx_font_smoothing ctx v
  | Auto -> Pp.string ctx "auto"
  | Grayscale -> Pp.string ctx "grayscale"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_webkit_font_smoothing t : webkit_font_smoothing =
  Cursor.enum_or_var "webkit-font-smoothing"
    [
      ("auto", (Auto : webkit_font_smoothing));
      ("none", None);
      ("antialiased", Antialiased);
      ("subpixel-antialiased", Subpixel_antialiased);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_font_smoothing t))
    t

let rec read_moz_osx_font_smoothing t : moz_osx_font_smoothing =
  Cursor.enum_or_var "moz-osx-font-smoothing"
    [
      ("auto", (Auto : moz_osx_font_smoothing));
      ("grayscale", Grayscale);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_moz_osx_font_smoothing t))
    t

let rec read_font_display t : font_display =
  Cursor.enum_or_var "font-display"
    [
      ("auto", (Auto : font_display));
      ("block", Block);
      ("swap", Swap);
      ("fallback", Fallback);
      ("optional", Optional);
    ]
    ~var:(fun t -> Var (read_var read_font_display t))
    t

let read_unicode_single start_value width =
  if width > String.length (hex_string start_value) then
    (Padded_single (start_value, width) : unicode_range)
  else Single start_value

let read_unicode_range_pair start_value end_value start_width end_width =
  if
    start_width > String.length (hex_string start_value)
    || end_width > String.length (hex_string end_value)
  then
    Padded_range
      { start = start_value; end_ = end_value; start_width; end_width }
  else Range (start_value, end_value)

let read_unicode_wildcard start_value prefix_width wildcards =
  let prefix = start_value lsr (4 * wildcards) in
  Wildcard { prefix; prefix_width; wildcards }

let read_unicode_token_form start_value end_value = function
  | Token.Single { width } -> read_unicode_single start_value width
  | Token.Range { start_width; end_width } ->
      read_unicode_range_pair start_value end_value start_width end_width
  | Token.Wildcard { prefix_width; wildcards } ->
      read_unicode_wildcard start_value prefix_width wildcards

let read_unicode_token t start_value end_value form =
  Cursor.skip t;
  if start_value > end_value then
    Cursor.err_invalid t "unicode range: start > end";
  if end_value > 0x10FFFF then
    Cursor.err_invalid t "unicode range: code point out of range";
  read_unicode_token_form start_value end_value form

let rec read_unicode_range t : unicode_range =
  (* The lexer emits a single [Unicode_range] token for [U+...] forms (CSS
     Syntax 3 (ED) sec. 4.3.14); we just translate it to the [unicode_range]
     ADT. *)
  Cursor.with_context t "unicode-range" @@ fun () ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      (Var (Values.read_var read_unicode_range t) : unicode_range)
  | Some
      (Component.Preserved
         { kind = Token.Unicode_range { start_value; end_value; form }; _ }) ->
      read_unicode_token t start_value end_value form
  | _ -> Cursor.err_expected t "unicode-range"

(* [src] has one value type, one reader and one printer. The [@font-face]
   descriptor and every [src:] declaration route through [Font_face.pp], so the
   two spellings cannot drift apart. *)
let pp_font_src : Font_face.src Pp.t = Font_face.pp
let read_font_src = Font_face.read_src
