(** Colour readers, with the static folds of relative colours. *)

open Values_intf
open Values_color_name
open Values_var
open Values_color_fold
open Values_read_calc

(** Read a percentage value as float (number followed by %) Used for color
    components where 0-100% clamping is required *)
let read_percentage_float t : float =
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    0.)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" | None -> max 0. (min 100. n)
    | Some unit -> Cursor.err_invalid t ("percentage unit: " ^ unit)

(** Read an alpha value *)
let rec read_alpha t : alpha =
  Cursor.ws t;
  let read_var_alpha t : alpha = Var (read_var read_alpha t) in
  let read_calc_alpha t : alpha =
    Calc (read_calc ~result_type:`Number_or_value read_alpha t)
  in
  let read_none t : alpha =
    Cursor.expect_string "none" t;
    None
  in
  let read_pct t : alpha =
    (* Alpha percentages are clamped to 0-100 per CSS spec *)
    Pct (Cursor.pct ~clamp:true t)
  in
  let read_num t : alpha =
    let n, _repr, unit = Cursor.number_repr_with_unit t in
    (match unit with
    | None -> ()
    | Some unit -> Cursor.err_invalid t ("alpha unit: " ^ unit));
    (* Clamp numeric alpha to 0-1 range per CSS spec *)
    Num (max 0. (min 1. n))
  in
  Cursor.one_of
    [ read_var_alpha; read_calc_alpha; read_none; read_pct; read_num ]
    t

(** Read optional alpha component *)
let read_optional_alpha t : alpha =
  Cursor.ws t;
  if Cursor.peek_delim t = Some '/' then (
    Cursor.slash t;
    read_alpha t)
  else None

(** Read a channel value (RGB) *)
let rec read_channel t : channel =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "var(" then Var (read_var read_channel t)
  else if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    (None : channel))
  else if
    match Cursor.peek t with
    | Some (Component.Func { node = { name; _ }; _ })
      when String.lowercase_ascii name <> "var" ->
        true
    | _ -> false
  then Int (int_of_float (max 0. (min 255. (read_num_expr t))))
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        (* Clamp percentage to 0-100 per CSS spec *)
        Pct (max 0. (min 100. n))
    | None ->
        (* CSS Color 4 sec. 5.1 rounds a channel that cannot be kept at full
           precision "towards +infinity". Sec. 16.5's worked example reads that
           as nearest-with-ties-up, not as a ceiling: 0.964 serialises as 0.96
           and 0.787 as 0.79. [Float.round] is exactly that for a non-negative
           channel, so [rgb(127.6 ...)] is [128] and [rgb(127.2 ...)] is [127],
           matching Chrome. Decimals in (0, 1) stay as [Num] so alpha helpers
           can still see them as fractional. *)
        if n <= 1.0 && n <> floor n then Num n
        else Int (int_of_float (max 0. (min 255. (Float.round n))))
    | Some _ -> Cursor.err_invalid t "channel value"

let rec read_rgb_var t : rgb var =
  Cursor.ws t;
  read_var read_rgb t

and read_rgb t : rgb =
  Cursor.ws t;
  (* Try to parse as three channels first (any could be a variable) *)
  Cursor.one_of
    [
      (fun t ->
        let r, g, b = Cursor.triple read_channel read_channel read_channel t in
        Channels { r; g; b });
      (* Fall back to a single var representing all channels *)
      (fun t -> Var (read_rgb_var t));
    ]
    t

let read_rgb_space_separated t : color =
  (* The cursor wraps the [rgb(...)] [Func] arguments, so there is no closing
     [)] to consume -- it's the block boundary. *)
  Cursor.ws t;
  if Cursor.looking_at t "var(" then (
    let snap = Cursor.save t in
    let rgb_var = read_rgb_var t in
    Cursor.ws t;
    if Cursor.is_done t then Rgb (Var rgb_var)
    else if Cursor.peek_delim t = Some '/' then (
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      match alpha with
      | None -> Cursor.err t "expected alpha value after '/'"
      | _ -> Rgba { rgb = Var rgb_var; a = alpha })
    else (
      Cursor.restore t snap;
      let r = read_channel t in
      Cursor.ws t;
      let g = read_channel t in
      Cursor.ws t;
      let b = read_channel t in
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      if not (Cursor.is_done t) then
        Cursor.err t "unexpected tokens after rgb()";
      match alpha with
      | None -> Rgb (Channels { r; g; b })
      | Num _ | Pct _ | Var _ | Calc _ ->
          Rgba { rgb = Channels { r; g; b }; a = alpha }))
  else
    let r = read_channel t in
    Cursor.ws t;
    let g = read_channel t in
    Cursor.ws t;
    let b = read_channel t in
    let alpha = read_optional_alpha t in
    Cursor.ws t;
    if not (Cursor.is_done t) then Cursor.err t "unexpected tokens after rgb()";
    match alpha with
    | None -> Rgb (Channels { r; g; b })
    | Num _ | Pct _ | Var _ | Calc _ ->
        Rgba { rgb = Channels { r; g; b }; a = alpha }

let read_rgb_comma_separated t : color =
  let r, g, b =
    Cursor.triple ~sep:Cursor.comma read_channel read_channel read_channel t
  in
  (* CSS4 allows mixing percentages and numbers in RGB functions. This is a
     change from CSS3 which required all values to be the same type. Since we
     target CSS4 (supported by all major browsers), we allow mixing. *)
  let alpha = if Cursor.comma_opt t then read_alpha t else None in
  Cursor.ws t;
  Cursor.expect_eof t;
  match alpha with
  | None -> Rgb (Channels { r; g; b })
  | a -> Rgba { rgb = Channels { r; g; b }; a }

(** Read color space identifier *)
let read_color_space t : color_space =
  let space_ident = Cursor.ident t in
  match Common.String.lowercase_ascii_preserve space_ident with
  | "srgb" -> Srgb
  | "srgb-linear" -> Srgb_linear
  | "display-p3" -> Display_p3
  | "a98-rgb" -> A98_rgb
  | "prophoto-rgb" -> Prophoto_rgb
  | "rec2020" -> Rec2020
  | "lab" -> Lab
  | "oklab" -> Oklab
  | "xyz" -> Xyz
  | "xyz-d50" -> Xyz_d50
  | "xyz-d65" -> Xyz_d65
  | "lch" -> Lch
  | "oklch" -> Oklch
  | "hsl" -> Hsl
  | "hwb" -> Hwb
  | _ -> Cursor.err_invalid t ("color space: " ^ space_ident)

let rec read_color_component t : component =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Component_none)
  else if Cursor.looking_at t "var(" then Var (read_var read_color_component t)
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Number_or_value read_color_component t)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> Pct n
    | Some u -> Cursor.err_invalid t ("unit: " ^ u)
    | None -> Num n

(** Read color components until the alpha separator ['/'] or end of input. *)
let rec read_color_components space t acc =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then List.rev acc
  else
    let component_count = List.length acc in
    let component = read_color_component t in
    (match ((space : color_space), component_count, component) with
    | (Lab | Oklab | Lch | Oklch), 0, (Pct _ | Var _ | Calc _ | Component_none)
      ->
        ()
    | (Lab | Oklab | Lch | Oklch), 0, _ ->
        Cursor.err_invalid t "L component must be percentage"
    | _ -> ());
    read_color_components space t (component :: acc)

(** Read hex color digits. The tokenizer represents [#deadbeef] as a single
    [Hash] token with the digits as value. Kept for parity with callers outside
    this module. *)
let _read_hex_color t =
  let hex =
    match Cursor.hash_opt t with
    | Some s -> s
    | None -> Cursor.err_invalid t "expected hex color"
  in
  let len = String.length hex in
  if len = 0 then Cursor.err_invalid t "empty hex color"
  else if len = 3 || len = 4 || len = 6 || len = 8 then hex
  else Cursor.err_invalid t ("invalid hex color length: " ^ string_of_int len)

(** Normalize hue value to 0-360 range *)
let normalize_hue (degrees : float) : float =
  let normalized = mod_float degrees 360.0 in
  if normalized < 0.0 then normalized +. 360.0 else normalized

(** Read a hue value (preserves unitless vs explicit angle) *)
let rec read_hue t : hue =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Hue_none)
  else if Cursor.looking_at t "var(" then Var (read_var read_hue t)
  else
    let n, unit_raw = Cursor.number_with_unit t in
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "" ->
        Unitless (normalize_hue n) (* Unitless number, defaults to degrees *)
    | "deg" -> Angle (Deg (normalize_hue n))
    | "rad" -> Angle (Rad n)
    | "turn" -> Angle (Turn n)
    | "grad" -> Angle (Grad n)
    | _ -> Cursor.err_invalid t ("hue unit: " ^ unit)

let read_separated_values t p1 p2 =
  let v1 = p1 t in
  Cursor.ws t;
  ignore (Cursor.comma_opt t : bool);
  Cursor.ws t;
  (* Need whitespace after comma *)
  let v2 = p2 t in
  (v1, v2)

let read_hsl t : color =
  Cursor.ws t;
  let h = match read_hue t with Hue_none -> Unitless 0. | h -> h in
  Cursor.ws t;
  (* Handle comma or space separator after hue *)
  let comma_separated = Cursor.comma_opt t in
  Cursor.ws t;
  let s = read_percentage_float t in
  Cursor.ws t;
  if comma_separated then Cursor.comma t
  else if Cursor.comma_opt t then
    Cursor.err_invalid t "mixed comma and space separated hsl() syntax";
  Cursor.ws t;
  let l = read_percentage_float t in
  let a =
    Cursor.ws t;
    if comma_separated && Cursor.comma_opt t then read_alpha t
    else if (not comma_separated) && Cursor.comma_opt t then
      Cursor.err_invalid t "mixed comma and space separated hsl() syntax"
    else if Cursor.peek_delim t = Some '/' then read_optional_alpha t
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Hsl { h; s = Pct s; l = Pct l; a }

let read_hwb t : color =
  Cursor.ws t;
  let h = match read_hue t with Hue_none -> Unitless 0. | h -> h in
  let w, b =
    read_separated_values t read_percentage_float read_percentage_float
  in
  let a =
    Cursor.ws t;
    if Cursor.comma_opt t then read_alpha t
    else if Cursor.peek_delim t = Some '/' then read_optional_alpha t
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Hwb { h; w = Pct w; b = Pct b; a }

let read_ok_lightness t : percentage option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Option.None)
  else
    let n, unit = Cursor.number_with_unit t in
    (* CSS Color 4 sec. 9.3/9.4: an out-of-range lightness clamps rather than
       invalidating the colour, matching lab/lch. 0% to 100% names the same 0 to
       1 a bare number does, so both spellings clamp alike. *)
    match unit with
    | Some "%" -> Some (Pct (max 0. (min 100. n)) : percentage)
    | None -> Some (Num (max 0. (min 1. n)) : percentage)
    | Some u -> Cursor.err_invalid t ("oklch() L unit: " ^ u)

let read_number_or_none ?pct_scale t : float option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    None)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        Some
          (n
          *.
          match pct_scale with
          | Some scale -> scale
          | None -> Cursor.err_invalid t "unexpected percentage")
    | None -> Some n
    | Some unit -> Cursor.err_invalid t ("invalid unit: " ^ unit)

let read_oklch t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec. *)
  let l = read_ok_lightness t in
  Cursor.ws t;
  let c = read_number_or_none ~pct_scale:0.004 t in
  Cursor.ws t;
  let h = read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklch { l; c; h; alpha }

let read_oklab t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l = read_ok_lightness t in
  let a = read_number_or_none ~pct_scale:0.004 t in
  let b = read_number_or_none ~pct_scale:0.004 t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklab { l; a; b; alpha }

let read_lab t : color =
  Cursor.ws t;
  (* CSS Color 4 sec. 9.3: lab() L accepts [<percentage>] or [<number>]; keep
     them distinct ([Pct] vs [Num]) so the printer can drive different precision
     (1 decimal for [<number>] over [0, 100], 4 decimals for [<percentage>]
     which scales by 100). *)
  let l : percentage option =
    if Cursor.looking_at t "none" then (
      Cursor.expect_string "none" t;
      Option.None)
    else
      let n, unit = Cursor.number_with_unit t in
      Some
        (match unit with
         | Some "%" -> Pct (max 0. (min 100. n))
         | None -> Num (max 0. (min 100. n))
         | Some u -> Cursor.err_invalid t ("lab L unit: " ^ u)
          : percentage)
  in
  let a = read_number_or_none ~pct_scale:1.25 t in
  let b = read_number_or_none ~pct_scale:1.25 t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lab { l; a; b; alpha }

let read_lch t : color =
  Cursor.ws t;
  let read_l_axis t : percentage option =
    if Cursor.looking_at t "none" then (
      Cursor.expect_string "none" t;
      Option.None)
    else
      (* CSS Color 4 sec. 9.3 lch(): the L channel is [<percentage> | <number>],
         distinguish so [lch(54.321 ...)] doesn't round-trip as [lch(54.321%
         ...)] - the [Pct] / [Num] variants drive different printers (the
         [%]-typed one scales the precision by 100 and emits [%]). *)
      let n, unit = Cursor.number_with_unit t in
      Some
        (match unit with
         | Some "%" -> Pct (max 0. (min 100. n))
         | None -> Num (max 0. (min 100. n))
         | Some u -> Cursor.err_invalid t ("lch L unit: " ^ u)
          : percentage)
  in
  let read_c = read_number_or_none ~pct_scale:1.5 in
  let l, c, h = Cursor.triple ~sep:Cursor.ws read_l_axis read_c read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lch { l; c; h; alpha }

let read_color_function t : color =
  Cursor.ws t;
  let space = read_color_space t in
  Cursor.ws t;
  let components = read_color_components space t [] in
  if List.length components <> 3 then
    Cursor.err_invalid t "color() requires three components";
  let alpha = read_optional_alpha t in
  Color { space; components; alpha }

(* A bare [<percentage>] leaf inside a color-mix weight [calc()]. Unlike the
   top-level weight it is not range-checked: CSS Values 4 sec. 10 clamps the
   math function's result, not its individual operands. [read_calc] handles the
   [var()] and nested-[calc()] factors itself, so the leaf only sees a token. *)
let read_color_mix_calc_pct t : percentage = Pct (Cursor.pct t)

(** Forward declaration for percentage reader used in color-mix *)
let rec read_percentage_in_color_mix t : percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Var (read_var read_percentage_in_color_mix t)
  else if Cursor.looking_at_calc t then
    (* CSS Color 5 sec. 3: the weight may be a math function. We can't
       bound-check a [calc()] statically (it may carry a [var()]), so we keep it
       verbatim and let substitution-time clamping apply. *)
    Calc (read_calc ~result_type:`Value read_color_mix_calc_pct t)
  else
    let n = Cursor.pct t in
    if n < 0. || n > 100. then
      Cursor.err_invalid t "color-mix percentage must be between 0% and 100%";
    (Pct n : percentage)

let read_optional_percentage t : percentage option =
  (* CSS Color 5 sec. 3 (https://drafts.csswg.org/css-color-5/#color-mix): the
     [color-mix()] weight grammar is [<percentage [0,100]>?] - strictly a
     percentage token, no [<number>] alternative. We don't accept a bare decimal
     here even though some minifiers (cssnano) ship the [0% -> 0] shortcut as a
     non-spec optimization that browsers tolerate. *)
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Some (Var (read_var read_percentage_in_color_mix t))
  else if Cursor.looking_at_calc t then
    Some (Calc (read_calc ~result_type:`Value read_color_mix_calc_pct t))
  else
    match Cursor.percentage_opt t with
    | Some n ->
        if n < 0. || n > 100. then
          Cursor.err_invalid t
            "color-mix percentage must be between 0% and 100%";
        Cursor.ws t;
        Some (Pct n : percentage)
    | None -> None

let hue_interpolation_start = function
  | "shorter" | "longer" | "increasing" | "decreasing" | "specified" -> true
  | _ -> false

let read_hue_interpolation_direction t : hue_interpolation =
  Cursor.enum "hue-interpolation"
    [
      ("shorter", Shorter);
      ("longer", Longer);
      ("increasing", Increasing);
      ("decreasing", Decreasing);
      ("specified", Specified);
    ]
    t

let read_full_hue_interpolation t : hue_interpolation =
  let hue = read_hue_interpolation_direction t in
  Cursor.ws t;
  Cursor.expect_string "hue" t;
  Cursor.ws t;
  hue

let add_pending_space buf last_was_space =
  if last_was_space && Buffer.length buf > 0 then Buffer.add_char buf ' '

let normalize_relative_color_tail tail =
  let tail = String.trim tail in
  let len = String.length tail in
  let buf = Buffer.create len in
  let rec skip_spaces i =
    if i < len && tail.[i] = ' ' then skip_spaces (i + 1) else i
  in
  let rec loop i last_was_space =
    if i >= len then ()
    else
      match tail.[i] with
      | ' ' | '\n' | '\t' | '\r' | '\012' -> loop (i + 1) true
      (* A pending space is dropped rather than trimmed: [add_pending_space]
         writes one only just before a non-space character, so the buffer never
         ends in one. *)
      | '/' ->
          Buffer.add_char buf '/';
          loop (skip_spaces (i + 1)) false
      | c ->
          add_pending_space buf last_was_space;
          if
            Buffer.length buf > 0
            && relative_color_splits_tokens
                 (Buffer.nth buf (Buffer.length buf - 1))
                 tail i
          then Buffer.add_char buf ' ';
          Buffer.add_char buf c;
          loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents buf

(* CSS Color 5 sec. 4.1 gives each channel one component value, so the channels
   are the components before the alpha slash that are not whitespace. Splitting
   on whitespace instead would miscount the spellings that need no separator:
   [20%g] is a percentage and an ident, [calc(...)10] a function and a
   number. *)
let relative_color_channel_count cvs =
  let is_ws = function
    | Component.Preserved { Token.kind = Whitespace _; _ } -> true
    | _ -> false
  in
  let is_alpha_sep = function
    | Component.Preserved { Token.kind = Delim "/"; _ } -> true
    | _ -> false
  in
  let rec loop count = function
    | [] -> count
    | cv :: _ when is_alpha_sep cv -> count
    | cv :: rest when is_ws cv -> loop count rest
    | _ :: rest -> loop (count + 1) rest
  in
  loop 0 cvs

let relative_color_has_empty_alpha cvs =
  let is_ws = function
    | Component.Preserved { Token.kind = Whitespace _; _ } -> true
    | _ -> false
  in
  let rec only_ws = function
    | [] -> true
    | cv :: rest when is_ws cv -> only_ws rest
    | _ -> false
  in
  let rec loop = function
    | [] -> false
    | Component.Preserved { Token.kind = Delim "/"; _ } :: rest -> only_ws rest
    | _ :: rest -> loop rest
  in
  loop cvs

(* The origin may be a [color()] node, which the byte converter has no arm for.
   The linear route covers every space whose matrices are wired up, so a
   relative colour folds whether its origin was written as a hex or as the
   [color()] the same conversion reaches. *)
(* The conversion error the fold takes from the space a colour is written in:
   none for an sRGB spelling, the wide-gamut bound for a [color()] matrix, the
   Lab bound for the Lab family, and nothing for an authored sRGB value. *)
let conversion_error_of_color (c : color) =
  match c with
  | Color { space = Srgb | Srgb_linear; _ } -> 0.
  | Color { space = Lab | Oklab | Lch | Oklch; _ } ->
      Color_space.lab_conversion_error
  | Color _ -> Color_space.conversion_error
  | Oklab _ | Oklch _ | Lab _ | Lch _ -> Color_space.lab_conversion_error
  | _ -> 0.

let relative_origin_srgb_bytes origin : (int * int * int * int) option =
  match static_color_to_srgb_bytes origin with
  | Some _ as found -> found
  | Option.None -> (
      match static_color_to_linear_srgb origin with
      | Some (linear, alpha_f) -> (
          match
            Color_space.srgb_bytes_of_linear
              ~error:(conversion_error_of_color origin)
              linear
          with
          | Some (r, g, b) ->
              let clamp01 v = Float.max 0.0 (Float.min 1.0 v) in
              Some
                (r, g, b, Float.to_int (Float.round (clamp01 alpha_f *. 255.0)))
          | Option.None -> Option.None)
      | Option.None -> Option.None)

(* The call names no alpha of its own, so the origin's carries through. *)
let folded_srgb_color origin (alpha : alpha) : color option =
  match relative_origin_srgb_bytes origin with
  | None -> Option.None
  | Some (r, g, b, origin_a_byte) ->
      let a : alpha =
        match alpha with
        | None when origin_a_byte = 255 -> None
        | None -> Num (Float.of_int origin_a_byte /. 255.)
        | a -> a
      in
      Option.Some
        (Rgba { rgb = Channels { r = Int r; g = Int g; b = Int b }; a })

(* CSS Color 5 secs. 4.3 to 4.9 and 5.1 name the channel keywords each relative
   colour function binds; [alpha] is bound by all of them. *)
let relative_channel_keywords name =
  let channels =
    match name with
    | "rgb" -> [ "r"; "g"; "b" ]
    | "hsl" -> [ "h"; "s"; "l" ]
    | "hwb" -> [ "h"; "w"; "b" ]
    | "lab" | "oklab" -> [ "l"; "a"; "b" ]
    | "lch" | "oklch" -> [ "l"; "c"; "h" ]
    | "color" -> [ "r"; "g"; "b"; "x"; "y"; "z" ]
    | _ -> []
  in
  "alpha" :: channels

(* Sec. 4.1 substitutes each keyword with the origin's channel as a [<number>],
   so a channel expression is typed like any other math: [calc(w + 10%)] adds a
   number to a percentage and [calc(h + 90deg)] a number to an angle, neither of
   which a browser takes, while [calc(w * 1%)] multiplies and is fine.
   Substituting a literal number is what lets the ordinary calc inference answer
   that, rather than a second type system written for this one place. *)
let rec substitute_channel_numbers keywords (c : Component.t) : Component.t =
  match c with
  | Component.Preserved ({ kind = Token.Ident id; _ } as tok)
    when List.mem (String.lowercase_ascii id) keywords ->
      Component.Preserved
        {
          tok with
          kind =
            Token.Number_tok { value = 1.; repr = "1"; number_flag = Integer };
        }
  | Component.Func ({ node; _ } as n) ->
      Component.Func
        {
          n with
          node =
            {
              node with
              Component.arguments =
                List.map (substitute_channel_numbers keywords) node.arguments;
            };
        }
  | Component.Block ({ node; _ } as n) ->
      Component.Block
        {
          n with
          node =
            {
              node with
              Component.value =
                List.map (substitute_channel_numbers keywords) node.value;
            };
        }
  | other -> other

(* Any dimension or percentage is the contextual value the inference weighs
   against a bare number; which dimension it is does not change the answer. *)
let read_relative_channel_unit t : unit =
  match Cursor.peek t with
  | Some
      (Component.Preserved { kind = Token.Percentage _ | Token.Dimension _; _ })
    ->
      Cursor.skip t
  | _ -> Cursor.err t "relative colour channel"

let check_relative_channel_math name components =
  let keywords = relative_channel_keywords name in
  if keywords <> [ "alpha" ] then
    List.iter
      (fun component ->
        match substitute_channel_numbers keywords component with
        | Component.Func { node = { name = fn; _ }; _ } as substituted
          when String.lowercase_ascii fn = "calc" ->
            ignore
              (read_calc ~result_type:`Number_or_value
                 read_relative_channel_unit
                 (Cursor.of_components [ substituted ])
                : unit calc)
        | _ -> ())
      components

let try_fold_color_function_static origin t : color option =
  Cursor.ws t;
  let read_keyword kw =
    match Cursor.peek_ident t with
    | Some id when String.lowercase_ascii id = kw ->
        ignore (Cursor.ident t);
        Cursor.ws t;
        true
    | _ -> false
  in
  if not (read_keyword "srgb") then Option.None
  else if not (read_keyword "r" && read_keyword "g" && read_keyword "b") then
    Option.None
  else
    (* The fold is best effort, so an alpha it cannot read is not an error: sec.
       4.1 puts the origin's [alpha] keyword, and math over it, in that slot,
       and the caller keeps the call as written when this answers [None]. *)
    match Cursor.option read_optional_alpha t with
    | Option.None -> Option.None
    | Option.Some alpha ->
        Cursor.ws t;
        if Cursor.is_done t then folded_srgb_color origin alpha else Option.None

let try_fold_relative_color_static name origin t : color option =
  match name with
  | "color" -> try_fold_color_function_static origin t
  | _ -> Option.None

let rec read_color_mix t : color =
  Cursor.ws t;
  (* CSS Color 5 sec. 3: an omitted [<color-interpolation-method>] defaults to
     [in oklab], so the [in <space> [<hue> hue]?] prefix (and its trailing
     comma) is optional. *)
  let has_method = Cursor.looking_at_ident "in" t in
  let in_space, hue =
    if has_method then (
      Cursor.expect_string "in" t;
      Cursor.ws t;
      let space = read_color_space t in
      Cursor.ws t;
      let hue =
        match Cursor.peek_ident t with
        | Some name when hue_interpolation_start (String.lowercase_ascii name)
          ->
            read_full_hue_interpolation t
        | _ -> Default
      in
      (Some space, hue))
    else ((None : color_space option), Default)
  in

  Cursor.ws t;
  if has_method then (
    Cursor.comma t;
    Cursor.ws t);

  (* Parse first color and optional percentage *)
  let color1, percent1 = read_color_mix_component t in

  Cursor.comma t;
  Cursor.ws t;

  (* Parse second color and optional percentage *)
  let color2, percent2 = read_color_mix_component t in

  Cursor.ws t;
  Mix { in_space; hue; color1; percent1; color2; percent2 }

and read_color_mix_component t =
  (* CSS Color 5 sec. 3: a component is [<color> && <percentage>?] - the two may
     appear in either order. Prefer the [<color> <percentage>?] reading so an
     ambiguous leading [var()] is taken as the colour ([var(--c) var(--p)] keeps
     its source order rather than being re-emitted percentage-first). Fall back
     to [<percentage> <color>] only when the colour-first reading leaves the
     component unconsumed - i.e. the leading token was really the percentage and
     a concrete colour follows ([var(--p) red], [30% red]). *)
  let color_first t =
    let component = read_color_mix_suffix_percentage t in
    Cursor.ws t;
    if Cursor.peek_comma t || Cursor.is_done t then component
    else Cursor.err_expected t "color-mix component end"
  in
  match Cursor.option color_first t with
  | Some component -> component
  | None -> read_color_mix_prefix_percentage t

and read_color_mix_prefix_percentage t =
  match read_optional_percentage t with
  | None -> Cursor.err_expected t "color-mix percentage"
  | Some percent ->
      let color = read_color t in
      Cursor.ws t;
      if Option.is_some (read_optional_percentage t) then
        Cursor.err_invalid t "color-mix component cannot have two percentages";
      (color, Some percent)

and read_color_mix_suffix_percentage t =
  let color = read_color t in
  Cursor.ws t;
  match read_optional_percentage t with
  | None -> (color, None)
  | Some percent -> (color, Some percent)

and read_relative_rgb t : color =
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  let tail_components = Cursor.remaining t in
  check_relative_channel_math "rgb" tail_components;
  if relative_color_has_empty_alpha tail_components then
    Cursor.err_expected t "relative rgb alpha";
  let tail =
    Cursor.consume_remaining_as_string ~trim:true t
    |> normalize_relative_color_tail
  in
  if tail = "" then Cursor.err_expected t "relative rgb channels";
  if relative_color_channel_count tail_components <> 3 then
    Cursor.err_expected t "relative rgb channels";
  Relative_rgb (origin, tail)

and read_contrast_color t : color =
  Cursor.ws t;
  let color = read_color t in
  Cursor.ws t;
  Contrast_color color

and read_light_dark t : color =
  Cursor.ws t;
  let light = read_color t in
  Cursor.ws t;
  Cursor.comma t;
  Cursor.ws t;
  let dark = read_color t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Light_dark (light, dark)

and read_color_attr t : color =
  Cursor.ws t;
  let name = Cursor.ident t in
  Cursor.ws t;
  Cursor.call "type" t (fun inner ->
      Cursor.ws inner;
      Cursor.expect '<' inner;
      Cursor.expect_string "color" inner;
      Cursor.expect '>' inner;
      Cursor.ws inner;
      Cursor.expect_eof inner);
  Cursor.ws t;
  let fallback =
    if Cursor.comma_opt t then (
      Cursor.ws t;
      Some (read_color t))
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Attribute (name, fallback)

and read_relative_color name t : color =
  (* CSS Color 5 sec. 4.2: any colour function may take [from <origin> <c1> <c2>
     <c3> [/ <alpha>]?]. The origin has a fixed <color> type, so retain the
     parsed node while keeping the not-yet-modelled channel expression
     opaque. *)
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  check_relative_channel_math name (Cursor.remaining t);
  let snap = Cursor.save t in
  match try_fold_relative_color_static name origin t with
  | Some folded -> folded
  | None -> (
      Cursor.restore t snap;
      let tail =
        Cursor.consume_remaining_as_string ~trim:true t
        |> normalize_relative_color_tail
      in
      if tail = "" then Cursor.err_expected t (name ^ " channels");
      match fold_relative_color_pass_through name origin tail with
      | Some color -> color
      | None -> Relative_color (name, origin, tail))

(* CSS Color 5 sec. 5.1: [color(from <origin> srgb r g b [/ <alpha>]?)] is a
   self-substitution of the origin's sRGB channels. Static folding requires the
   origin to be reducible to sRGB bytes and the three channel slots to be the
   bare [r] [g] [b] keywords. Wider Color 5 substitutions (other spaces,
   calc()-on-keyword arithmetic, swapped channels) need real per-space
   conversion machinery; absent that we fall through and keep the original
   [color(from ...)] string. *)
and with_relative_fallback name fallback t =
  Cursor.ws t;
  if Cursor.looking_at t "from" then read_relative_color name t else fallback t

and color_parsers =
  [
    ( "rgb",
      fun t ->
        Cursor.ws t;
        if Cursor.looking_at t "from" then read_relative_rgb t
        else
          Cursor.one_of [ read_rgb_space_separated; read_rgb_comma_separated ] t
    );
    ( "rgba",
      fun t ->
        Cursor.ws t;
        Cursor.one_of [ read_rgb_space_separated; read_rgb_comma_separated ] t
    );
    ("hsl", fun t -> with_relative_fallback "hsl" read_hsl t);
    ("hsla", fun t -> with_relative_fallback "hsl" read_hsl t);
    ("hwb", fun t -> with_relative_fallback "hwb" read_hwb t);
    ("oklch", fun t -> with_relative_fallback "oklch" read_oklch t);
    ("lab", fun t -> with_relative_fallback "lab" read_lab t);
    ("oklab", fun t -> with_relative_fallback "oklab" read_oklab t);
    ("lch", fun t -> with_relative_fallback "lch" read_lch t);
    ("color", fun t -> with_relative_fallback "color" read_color_function t);
    ("contrast-color", read_contrast_color);
    ("light-dark", read_light_dark);
    ("attr", read_color_attr);
    ("color-mix", read_color_mix);
  ]

and read_color t : color =
  Cursor.ws t;
  (* The two branches below consume the token before judging it, so they mark
     [loc] rather than whatever the cursor has moved on to. *)
  let loc = Cursor.position t in
  let color =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Hash { value; _ }; _ }) -> (
        Cursor.skip t;
        let len = String.length value in
        let is_hex c =
          ('0' <= c && c <= '9')
          || ('a' <= c && c <= 'f')
          || ('A' <= c && c <= 'F')
        in
        if not (len = 3 || len = 4 || len = 6 || len = 8) then
          Cursor.err_invalid ~loc t ("hex color length: " ^ string_of_int len)
        else if not (String.for_all is_hex value) then
          Cursor.err_invalid ~loc t ("hex color digits: " ^ value)
        else
          match rgba_of_hex value with
          | Some (r, g, b, a) -> Authored_hex { value; r; g; b; a }
          | None -> Cursor.err_invalid ~loc t ("hex color digits: " ^ value))
    | Some (Component.Func ({ node = { name; _ }; _ } as fn)) -> (
        (* CSS Values 4 sec. 4.1: a function name is a keyword, so it names the
           same function in whatever case it was written. *)
        let fn_name = Common.String.lowercase_ascii_preserve name in
        match List.assoc_opt fn_name color_parsers with
        | Some parser ->
            Cursor.skip t;
            parser (Cursor.func_sub fn t)
        | None when fn_name = "var" -> Var (read_var read_color t)
        | None -> Cursor.err t ("unknown color function: " ^ name))
    | Some (Component.Preserved { kind = Token.Ident ident; _ }) -> (
        Cursor.skip t;
        (* CSS color keywords are case-insensitive. *)
        match read_color_keyword_of_string (String.lowercase_ascii ident) with
        | Some Auto -> Cursor.err_invalid ~loc t "auto is not a colour"
        | Some color -> color
        | None -> Cursor.err ~loc t ("unknown color: " ^ ident))
    | _ -> Cursor.err t "color"
  in
  Cursor.ws t;
  (match Cursor.peek_delim t with
  | Some '/' -> Cursor.err_invalid t "unexpected color alpha separator"
  | _ -> ());
  color

let rec color_has_specified_hue = function
  | Mix { hue = Specified; _ } -> true
  | Mix { color1; color2; _ } ->
      color_has_specified_hue color1 || color_has_specified_hue color2
  | Relative_rgb (origin, _) -> color_has_specified_hue origin
  | Relative_color (_, origin, _) -> color_has_specified_hue origin
  | Contrast_color color -> color_has_specified_hue color
  | Light_dark (a, b) -> color_has_specified_hue a || color_has_specified_hue b
  | Attribute (_, Some color) -> color_has_specified_hue color
  | _ -> false

(* [color_exists p c] is [true] when [p] holds for [c] or for any color nested
   inside it (color-mix operands, light-dark arms, contrast-color, attr()
   fallback). Those composites are the only colors that carry sub-colors. *)
let color_exists p =
  let rec go c =
    p c
    ||
    match c with
    | Mix { color1; color2; _ } -> go color1 || go color2
    | Relative_rgb (origin, _) -> go origin
    | Relative_color (_, origin, _) -> go origin
    | Light_dark (a, b) -> go a || go b
    | Contrast_color c -> go c
    | Attribute (_, Some c) -> go c
    | _ -> false
  in
  go

let color_is_color_4 =
  color_exists (function
    | Lab _ | Lch _ | Oklab _ | Oklch _ | Hwb _ | Color _ | Relative_rgb _
    | Relative_color _ ->
        true
    | _ -> false)

(* The web-features key each Color 4 / 5 construct answers to, so a target
   contract can say whether a browser parses the colour. [light-dark()] and
   [contrast-color()] have keys of their own; [none] as a channel has not. *)
let color_feature_key : color -> string option = function
  | Lab _ -> Some "css.types.color.lab"
  | Lch _ -> Some "css.types.color.lch"
  | Oklab _ -> Some "css.types.color.oklab"
  | Oklch _ -> Some "css.types.color.oklch"
  | Hwb _ -> Some "css.types.color.hwb"
  | Color _ -> Some "css.types.color.color"
  | Mix _ -> Some "css.types.color.color-mix"
  | Relative_rgb _ -> Some "css.types.color.rgb.relative_syntax"
  | Relative_color (name, _, _) ->
      Some (String.concat "" [ "css.types.color."; name; ".relative_syntax" ])
  | Light_dark _ -> Some "css.types.color.light-dark"
  | Contrast_color _ -> Some "css.types.color.contrast-color"
  | _ -> None

let color_uses_feature_where lacks =
  color_exists (fun c ->
      match color_feature_key c with Some key -> lacks key | None -> false)

let read_system_color t : system_color =
  Cursor.ws t;
  let keyword = Cursor.ident t in
  match String.lowercase_ascii keyword with
  | "accentcolor" -> Accent_color
  | "accentcolortext" -> Accent_color_text
  | "activetext" -> Active_text
  | "buttonborder" -> Button_border
  | "buttonface" -> Button_face
  | "buttontext" -> Button_text
  | "canvas" -> Canvas
  | "canvastext" -> Canvas_text
  | "field" -> Field
  | "fieldtext" -> Field_text
  | "graytext" -> Gray_text
  | "highlight" -> Highlight
  | "highlighttext" -> Highlight_text
  | "linktext" -> Link_text
  | "mark" -> Mark
  | "marktext" -> Mark_text
  | "selecteditem" -> Selected_item
  | "selecteditemtext" -> Selected_item_text
  | "visitedtext" -> Visited_text
  | "-webkit-focus-ring-color" -> Webkit_focus_ring_color
  | _ -> Cursor.err_invalid t ("system color: " ^ keyword)

(** Read color_name value *)
let read_color_name t : color_name =
  Cursor.ws t;
  let s = Cursor.ident t in
  match color_name_of_string (String.lowercase_ascii s) with
  | Some name -> name
  | None -> Cursor.err_invalid t ("color name: " ^ s)

(** Read hue_interpolation *)
let read_hue_interpolation t : hue_interpolation =
  Cursor.ws t;
  let hue =
    Cursor.enum "hue-interpolation"
      [
        ("shorter", Shorter);
        ("longer", Longer);
        ("increasing", Increasing);
        ("decreasing", Decreasing);
        ("specified", Specified);
        ("default", Default);
      ]
      t
  in
  (match hue with
  | Default -> ()
  | _ ->
      Cursor.ws t;
      ignore (Cursor.option (Cursor.expect_string "hue") t));
  hue
