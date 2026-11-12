(** Static colour evaluation: hex decoding, sRGB and linear-sRGB conversion,
    [color-mix()] interpolation and gamut mapping. *)

open Values_intf
open Values_color_name

let pp_color_name : color_name Pp.t =
 fun ctx name -> Pp.string ctx (fst (color_name_hex name))

(* Relative-colour channel and alpha expressions remain an opaque tail because
   they are [<calc>]-derived. To match the typed-color path's alpha
   canonicalisation under [~minify:true], rewrite a trailing ["/<n>%"] alpha
   suffix to its decimal equivalent. *)
let minify_relative_color_alpha body =
  let len = String.length body in
  if len = 0 || body.[len - 1] <> '%' then body
  else
    match String.rindex_opt body '/' with
    | None -> body
    | Some slash -> (
        let pct = String.sub body (slash + 1) (len - slash - 2) in
        match Float.of_string_opt pct with
        | Some f ->
            String.concat ""
              [
                String.sub body 0 (slash + 1);
                Pp.string_of_float ~drop_leading_zero:true (f /. 100.);
              ]
        | None -> body)

let body_is_digit body i =
  let len = String.length body in
  i < len && body.[i] >= '0' && body.[i] <= '9'

let body_is_number_boundary body i =
  if i = 0 then true
  else
    match body.[i - 1] with
    | ' ' | '/' | ',' | '(' | '+' | '*' -> true
    | _ -> false

let rec body_starts_number body i =
  let len = String.length body in
  body_is_digit body i
  || (i < len && body.[i] = '.' && body_is_digit body (i + 1))
  || i + 1 < len
     && (body.[i] = '+' || body.[i] = '-')
     && body_starts_number body (i + 1)

let rec body_scan_digits body j =
  if body_is_digit body j then body_scan_digits body (j + 1) else j

let body_scan_fraction body after_int =
  let len = String.length body in
  if after_int < len && body.[after_int] = '.' then
    body_scan_digits body (after_int + 1)
  else after_int

let body_scan_exponent body after_frac =
  let len = String.length body in
  let is_e c = c = 'e' || c = 'E' in
  if after_frac < len && is_e body.[after_frac] then
    let signed = after_frac + 1 in
    let signed =
      if signed < len && (body.[signed] = '+' || body.[signed] = '-') then
        signed + 1
      else signed
    in
    let after_digits = body_scan_digits body signed in
    if after_digits = signed then after_frac else after_digits
  else after_frac

let body_scan_number body start =
  let after_exp =
    body_scan_exponent body
      (body_scan_fraction body (body_scan_digits body start))
  in
  let lone_dot =
    after_exp = start + 1 && start < String.length body && body.[start] = '.'
  in
  if after_exp = start || lone_dot then Option.None
  else
    let raw = String.sub body start (after_exp - start) in
    match Float.of_string_opt raw with
    | Option.Some f ->
        Option.Some (Pp.string_of_float ~drop_leading_zero:true f, after_exp)
    | Option.None -> Option.None

(* Reserialise any number literal in [body] through [Pp.string_of_float] so
   forms like [0.2] collapse to [.2]. The body is a normalised token sequence
   with single-space separators and tight [/] joins, so number boundaries are
   one of: start-of-string, space, [/], [(], [,], or another operator delim. *)
let minify_relative_color_numbers body =
  let len = String.length body in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if body_is_number_boundary body i && body_starts_number body i then (
      match body_scan_number body i with
      | Option.Some (rendered, next) ->
          Buffer.add_string buf rendered;
          loop next
      | Option.None ->
          Buffer.add_char buf body.[i];
          loop (i + 1))
    else (
      Buffer.add_char buf body.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let relative_color_space_elidable body i =
  let len = String.length body in
  if i <= 0 || i + 1 >= len || body.[i] <> ' ' then false
  else
    match (body.[i - 1], body.[i + 1]) with
    | ')', next -> body_starts_number body (i + 1) || next = '.'
    | '%', next ->
        body_starts_number body (i + 1)
        || (next >= 'A' && next <= 'Z')
        || (next >= 'a' && next <= 'z')
    | _ -> false

(* The reading [relative_color_space_elidable] owes. Where the printer drops a
   separator the reader has to put one back, or one channel list written the two
   ways it may be written is two tails that print alike: CSS Syntax 3 (ED) sec.
   4.3.3 ends a percentage at its [%] and sec. 4.3.5 ends a block at its [)], so
   [20%g] carries the channels [20%] and [g] exactly as [20% g] does. The two
   answers have to name the same positions or a tail stops round-tripping. *)
let relative_color_splits_tokens prev body i =
  match prev with
  | '%' ->
      body_starts_number body i
      || (body.[i] >= 'A' && body.[i] <= 'Z')
      || (body.[i] >= 'a' && body.[i] <= 'z')
  | ')' -> body_starts_number body i || body.[i] = '.'
  | _ -> false

let minify_relative_color_spaces body =
  let len = String.length body in
  let buf = Buffer.create len in
  let rec loop i =
    if i >= len then ()
    else if relative_color_space_elidable body i then loop (i + 1)
    else (
      Buffer.add_char buf body.[i];
      loop (i + 1))
  in
  loop 0;
  Buffer.contents buf

let zero_percentage : percentage option = Some (Pct 0.)

let zero_float_of_none (v : float option) : float option =
  match v with None -> Some 0. | Some f -> Some f

let zero_hue_of_none = function Hue_none -> Unitless 0. | h -> h

let fold_relative_color_pass_through name origin tail =
  match (name, origin, tail) with
  | "lab", Lab { l; a; b; alpha }, ("l a b" | "l a b/alpha") ->
      Some
        (Lab
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             a = zero_float_of_none a;
             b = zero_float_of_none b;
             alpha;
           })
  | "oklab", Oklab { l; a; b; alpha }, ("l a b" | "l a b/alpha") ->
      Some
        (Oklab
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             a = zero_float_of_none a;
             b = zero_float_of_none b;
             alpha;
           })
  | "lch", Lch { l; c; h; alpha }, ("l c h" | "l c h/alpha") ->
      Some
        (Lch
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             c = zero_float_of_none c;
             h = zero_hue_of_none h;
             alpha;
           })
  | "oklch", Oklch { l; c; h; alpha }, ("l c h" | "l c h/alpha") ->
      Some
        (Oklch
           {
             l = (match l with None -> zero_percentage | Some _ -> l);
             c = zero_float_of_none c;
             h = zero_hue_of_none h;
             alpha;
           })
  | _ -> None

(* Hue value of an [Hsl] colour as a float in degrees, when the input is a plain
   numeric hue (number / [deg] angle). Other forms ([rad]/[turn]/
   [grad]/[var]/[calc]/[none]) return [None] so the printer keeps the authored
   representation. *)
let deg_of_hue = function
  | Unitless f -> Some f
  | Angle (Deg f) -> Some f
  | Angle (Turn f) -> Some (f *. 360.)
  | Angle (Grad f) -> Some (f *. 0.9)
  | Angle (Rad f) -> Some (f *. 180. /. Float.pi)
  | _ -> None

let float_of_percentage = function
  | (Pct f : percentage) -> Some f
  | (Num f : percentage) -> Some (f *. 100.)
  | _ -> None

let alpha_is_full = function
  | (None : alpha) -> true
  | Num 1.0 -> true
  | Pct 100.0 -> true
  | _ -> false

(* Byte value [0..255] for an alpha component, when the alpha is a static number
   or percentage. Returns [None] for symbolic forms ([Var] / [Calc]) that can't
   fold to a fixed byte. *)
let alpha_value_byte = function
  | (None : alpha) -> Some 255
  | Num f when f >= 0. && f <= 1. ->
      Some (Float.to_int (Float.round (f *. 255.)))
  | Pct f when f >= 0. && f <= 100. ->
      Some (Float.to_int (Float.round (f *. 255. /. 100.)))
  | _ -> Option.None

let exact_alpha_value_byte = function
  | (None : alpha) -> Some 255
  | Num f when f >= 0. && f <= 1. ->
      let byte = f *. 255. in
      if Float.is_integer byte then Some (Float.to_int byte) else None
  | Pct f when f >= 0. && f <= 100. ->
      let byte = f *. 255. /. 100. in
      if Float.is_integer byte then Some (Float.to_int byte) else None
  | _ -> None

(* CSS Color 4 3: the hue is interpreted modulo 360 degrees. *)
let normalize_hue f =
  let m = Float.rem f 360. in
  if m < 0. then m +. 360. else m

(* CSS Color 4 4.2.4: convert HSL components to sRGB byte triples. [hue] is in
   degrees [0..360), [saturation] and [lightness] are percentages [0..100].
   Returns [(r, g, b)] each in [0..255]. *)
let hsl_to_rgb_float ~hue ~saturation ~lightness =
  let s = saturation /. 100. in
  let l = lightness /. 100. in
  let a = s *. Float.min l (1. -. l) in
  let f n =
    let k = Float.rem (n +. (hue /. 30.)) 12. in
    let k = if k < 0. then k +. 12. else k in
    let t = Float.max (Float.min (Float.min (k -. 3.) (9. -. k)) 1.) (-1.) in
    l -. (a *. t)
  in
  (f 0., f 8., f 4.)

let hsl_to_rgb_bytes ~hue ~saturation ~lightness =
  let r, g, b = hsl_to_rgb_float ~hue ~saturation ~lightness in
  let to_byte v = Float.to_int (Float.round ((v *. 255.) +. 1e-9)) in
  (to_byte r, to_byte g, to_byte b)

(* CSS Color 4 4.2.5: convert HWB components to sRGB byte triples. [hue] in
   degrees, [whiteness]/[blackness] as percentages. *)
let hwb_to_rgb_bytes ~hue ~whiteness ~blackness =
  let w = whiteness /. 100. in
  let bl = blackness /. 100. in
  if w +. bl >= 1. then
    let g = w /. (w +. bl) in
    let v = Float.to_int (Float.round (g *. 255.)) in
    (v, v, v)
  else if w = 0. && bl = 0. then
    hsl_to_rgb_bytes ~hue ~saturation:100. ~lightness:50.
  else
    let r, g, b = hsl_to_rgb_float ~hue ~saturation:100. ~lightness:50. in
    let scale c =
      let v = (c *. (1. -. w -. bl)) +. w in
      Float.to_int (Float.round (v *. 255.))
    in
    (scale r, scale g, scale b)

(* Map a fully-saturated, mid-lightness HSL hue onto its CSS named colour. Only
   the six primary/secondary hues are addressed, since they are the only ones
   whose name is shorter than the equivalent [#hex] form. *)

(* Parse a single hex digit. *)
let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> Option.None

(* Parse a [Hex.value] string ([RGB], [RGBA], [RRGGBB], [RRGGBBAA]) into (r, g,
   b, a) bytes. Returns [None] for malformed lengths. *)
let rgba_of_hex s =
  let pair a b =
    Option.bind (hex_digit a) (fun ah ->
        Option.map (fun bh -> (ah lsl 4) lor bh) (hex_digit b))
  in
  let single c = Option.map (fun h -> (h lsl 4) lor h) (hex_digit c) in
  match String.length s with
  | 3 -> (
      match (single s.[0], single s.[1], single s.[2]) with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | 4 -> (
      match (single s.[0], single s.[1], single s.[2], single s.[3]) with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | 6 -> (
      match (pair s.[0] s.[1], pair s.[2] s.[3], pair s.[4] s.[5]) with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | 8 -> (
      match
        (pair s.[0] s.[1], pair s.[2] s.[3], pair s.[4] s.[5], pair s.[6] s.[7])
      with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | _ -> Option.None

(* The numeric value of a channel when it is a plain [Int] or integer-valued
   [Num]/[Pct]. Returns [None] for [Var]/[Calc] inputs which the printer cannot
   canonicalise. *)
let channel_byte_value (c : channel) =
  match c with
  | Int i when i >= 0 && i <= 255 -> Some i
  | Num f when Float.is_integer f && f >= 0. && f <= 255. ->
      Some (Float.to_int f)
  | Pct f when f >= 0. && f <= 100. ->
      Some (Float.to_int (Float.round (f *. 255. /. 100.)))
  | _ -> None

(* Classify a channel for static sRGB folding. [Some (Some byte)] is numeric;
   [Some Option.None] is the [none] keyword (foldable, no byte); the outer
   [Option.None] is a [Var] which makes the whole colour unfoldable. Conflating
   the two [None]s would fold [rgb(var(--r) var(--g) var(--b))] to black. *)
let channel_srgb_slot (c : channel) : int option option =
  match c with
  | None -> Some Option.None
  | Var _ -> Option.None
  | _ -> Option.map (fun b -> Some b) (channel_byte_value c)

let exact_channel_byte_value (c : channel) =
  match c with
  | Int i when i >= 0 && i <= 255 -> Some i
  | Num f when Float.is_integer f && f >= 0. && f <= 255. ->
      Some (Float.to_int f)
  | Pct f when f >= 0. && f <= 100. ->
      let byte = f *. 255. /. 100. in
      if Float.is_integer byte then Some (Float.to_int byte) else None
  | _ -> None

let hex_of_byte i =
  let hex_digits = "0123456789abcdef" in
  let s = Bytes.create 2 in
  Bytes.set s 0 hex_digits.[(i lsr 4) land 0xF];
  Bytes.set s 1 hex_digits.[i land 0xF];
  Bytes.to_string s

(* Decode a hex spelling ([#rgb] / [#rrggbb] / [#rgba] / [#rrggbbaa], with or
   without the leading [#]) to its sRGB byte components. Every equivalent
   spelling decodes to the same node. Malformed input raises: there is no colour
   it denotes, and a caller that guessed one would emit a plausible wrong colour
   rather than a failure. Use [hex_opt] to decide. *)
let strip_hash s =
  if String.length s > 0 && s.[0] = '#' then String.sub s 1 (String.length s - 1)
  else s

let hex_opt s : color option =
  match rgba_of_hex (strip_hash s) with
  | Some (r, g, b, a) -> Some (Hex { r; g; b; a })
  | None -> Option.None

let hex s : color =
  match hex_opt s with
  | Some c -> c
  | Option.None ->
      invalid_arg (String.concat "" [ "Values.hex: not a hex colour: "; s ])

(* CSS Color 4 sec. 4.4 [none] sentinel: per-channel folding of a static colour
   to sRGB. Each channel is [Some byte] when the colour resolves statically and
   the channel is a numeric value, [Option.None] when the channel is the [none]
   keyword. The outer [Option.None] means the whole colour can't be folded (e.g.
   contains [Var] / [Calc] / unsupported space). *)
let static_color_to_srgb_channels :
    color -> (int option * int option * int option * int option) option =
  function
  | Hex { r; g; b; a } | Authored_hex { r; g; b; a; _ } ->
      Some (Some r, Some g, Some b, Some a)
  | Rgb (Channels { r; g; b }) -> (
      match (channel_srgb_slot r, channel_srgb_slot g, channel_srgb_slot b) with
      | Some r, Some g, Some b -> Some (r, g, b, Some 255)
      | _ -> Option.None)
  | Rgba { rgb = Channels { r; g; b }; a } -> (
      match (channel_srgb_slot r, channel_srgb_slot g, channel_srgb_slot b) with
      | Some r, Some g, Some b -> Some (r, g, b, alpha_value_byte a)
      | _ -> Option.None)
  | Hsl { h; s; l; a } -> (
      match (deg_of_hue h, float_of_percentage s, float_of_percentage l) with
      | Some hue, Some saturation, Some lightness ->
          let hue = normalize_hue hue in
          let r, g, b = hsl_to_rgb_bytes ~hue ~saturation ~lightness in
          Some (Some r, Some g, Some b, alpha_value_byte a)
      | _ -> Option.None)
  | Hwb { h; w; b; a } -> (
      match (deg_of_hue h, float_of_percentage w, float_of_percentage b) with
      | Some hue, Some whiteness, Some blackness ->
          let hue = normalize_hue hue in
          let r, g, blue = hwb_to_rgb_bytes ~hue ~whiteness ~blackness in
          Some (Some r, Some g, Some blue, alpha_value_byte a)
      | _ -> Option.None)
  | Named name -> (
      let _, hex = color_name_hex name in
      if hex = "" then Option.None
      else
        match rgba_of_hex hex with
        | Some (r, g, b, a) -> Some (Some r, Some g, Some b, Some a)
        | None -> None)
  | Transparent -> Some (Some 0, Some 0, Some 0, Some 0)
  | _ -> Option.None

(* Reduce any all-static colour to its sRGB byte channels and alpha. Returns
   [None] for colours that contain a [Var] / [Calc] / [Current] / [none]
   component or that can't be folded statically. *)
let static_color_to_srgb_bytes c : (int * int * int * int) option =
  match static_color_to_srgb_channels c with
  | Some (r, g, b, Some a) ->
      let component = function Some v -> v | None -> 0 in
      Some (component r, component g, component b, a)
  | _ -> Option.None

(* A bare colour keyword ([red], [transparent]) is also a valid [<custom-ident>]
   in a non-colour context, so it must not be introduced when folding a colour
   inside an opaque custom-property token stream: re-spell a named/transparent
   colour as its hex form, which is a colour or nothing in every context. A
   hex/function/currentcolor colour already carries no keyword spelling. *)
let nonkeyword_color (c : color) : color =
  match c with
  | Named _ | Transparent -> (
      match static_color_to_srgb_bytes c with
      | Some (r, g, b, a) -> Hex { r; g; b; a }
      | None -> c)
  | _ -> c

let exact_rgb_to_srgb_bytes c : (int * int * int * int) option =
  match c with
  | Rgb (Channels { r; g; b }) -> (
      match
        ( exact_channel_byte_value r,
          exact_channel_byte_value g,
          exact_channel_byte_value b )
      with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> None)
  | Rgba { rgb = Channels { r; g; b }; a } -> (
      match
        ( exact_channel_byte_value r,
          exact_channel_byte_value g,
          exact_channel_byte_value b,
          exact_alpha_value_byte a )
      with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> None)
  | Transparent -> Some (0, 0, 0, 0)
  | _ -> None

(* CSS Color 4 sec. 10.2: [color(srgb r g b)] is [rgb()] with its channels
   scaled to [0..1], so the two functions spell one colour exactly when every
   channel lands on a whole byte. No other [color()] space converts to sRGB
   without a transfer function or a matrix, so nothing else here is exact; a
   channel outside the gamut, a [none] and a [var()] are all left alone. *)
let exact_srgb_function_channels c : (int * int * int * alpha) option =
  let byte : component -> int option = function
    | Num f when f >= 0. && f <= 1. ->
        let b = f *. 255. in
        if Float.is_integer b then Some (Float.to_int b) else None
    | Pct f when f >= 0. && f <= 100. ->
        let b = f *. 255. /. 100. in
        if Float.is_integer b then Some (Float.to_int b) else None
    | _ -> None
  in
  match c with
  | Color { space = Srgb; components = [ r; g; b ]; alpha } -> (
      match (byte r, byte g, byte b) with
      | Some r, Some g, Some b -> Some (r, g, b, alpha)
      | _ -> None)
  | _ -> None

(* CSS Color 5 5: combine two [color-mix] percentages into the final
   per-component weights and an [alpha] multiplier. [p1] / [p2] are in [0..100];
   missing values are normalised by the caller. *)
let mix_weights p1 p2 : (float * float * float) option =
  let sum = p1 +. p2 in
  if sum <= 0. then None
  else
    let w1 = p1 /. sum in
    let w2 = p2 /. sum in
    let alpha_mult = if sum >= 100. then 1. else sum /. 100. in
    Some (w1, w2, alpha_mult)

(* Linearly interpolate two byte channels to a byte. *)
let lerp_byte b1 b2 w1 w2 =
  Float.to_int
    (Float.round ((Float.of_int b1 *. w1) +. (Float.of_int b2 *. w2)))

(* Mix two static colours in sRGB per CSS Color 5 sec. 3. [None] if either
   operand can't fold statically ([Var] / [Calc]) or the weights reduce to zero.
   CSS Color 4 sec. 4.4 [none] sentinel: a [none] channel inherits the other
   operand's channel rather than averaging in a zero; both [none] yields [none]
   (returned as zero, since the caller routes through [Hex] and [#000000] is the
   shortest fully-[none] spelling). *)
let mix_srgb_bytes c1 c2 ~p1 ~p2 =
  match
    (static_color_to_srgb_channels c1, static_color_to_srgb_channels c2)
  with
  | Some (r1, g1, b1, a1), Some (r2, g2, b2, a2) -> (
      match mix_weights p1 p2 with
      | None -> Option.None
      | Some (w1, w2, alpha_mult) ->
          (* CSS Color 4 sec. 13.3: colours with alpha interpolate
             premultiplied, then un-premultiply by the result alpha - otherwise
             a transparent operand ([#0000]) bleeds its zero channels in and
             darkens the result. A [none] (or absent) alpha contributes as fully
             opaque to its own channels. *)
          let alpha_f = function
            | Some a -> Float.of_int a /. 255.
            | None -> 1.
          in
          let af1 = alpha_f a1 and af2 = alpha_f a2 in
          (* Un-premultiply by the interpolated alpha only; the [alpha_mult]
             (<100%-sum) scaling applies to the final alpha, not the
             channels. *)
          let interp_alpha = (af1 *. w1) +. (af2 *. w2) in
          let mix_channel x y =
            match (x, y) with
            | Some bx, Some by ->
                let pre =
                  (Float.of_int bx *. af1 *. w1)
                  +. (Float.of_int by *. af2 *. w2)
                in
                let v =
                  if interp_alpha <= 0. then 0. else pre /. interp_alpha
                in
                Some (Float.to_int (Float.round v))
            | Some b, None | None, Some b -> Some b
            | None, None -> None
          in
          let r = mix_channel r1 r2 in
          let g = mix_channel g1 g2 in
          let b = mix_channel b1 b2 in
          let a =
            match (a1, a2) with
            | Some av1, Some av2 ->
                let alpha_pre = lerp_byte av1 av2 w1 w2 in
                Some
                  (Float.to_int
                     (Float.round (Float.of_int alpha_pre *. alpha_mult)))
            | Some av, None | None, Some av ->
                Some
                  (Float.to_int (Float.round (Float.of_int av *. alpha_mult)))
            | None, None -> None
          in
          let unwrap = function Some v -> v | None -> 0 in
          Some (unwrap r, unwrap g, unwrap b, unwrap a))
  | _ -> Option.None

let static_alpha_value = function
  | (None : alpha) -> Some 1.
  | Num f when f >= 0. && f <= 1. -> Some f
  | Pct f when f >= 0. && f <= 100. -> Some (f /. 100.)
  | _ -> Option.None

let alpha_of_mixed_value f = if f >= 1. then (None : alpha) else Num f

let mix_optional_float ~w1 ~w2 v1 v2 =
  match (v1, v2) with
  | Some f1, Some f2 -> Some ((f1 *. w1) +. (f2 *. w2))
  | Some f, None | None, Some f -> Some f
  | None, None -> Option.None

(* Mix the lightness channel of a lab-family colour. [l_num_max] is the L value
   that 100% denotes: 100. for lab/lch (a bare number is already on the 0-100
   scale) and 1. for oklab/oklch (a bare number is on 0-1). The result is stored
   as a bare number on that scale, matching the [mix_in_*_space] fallback. A
   [none] L channel carries the other operand's value over verbatim. *)
let mix_lightness ~l_num_max ~w1 ~w2 l1 l2 =
  let to_float = function
    | (Num f : percentage) -> Some f
    | Pct f -> Some (f /. 100. *. l_num_max)
    | _ -> None
  in
  match (l1, l2) with
  | Some p1, Some p2 -> (
      match (to_float p1, to_float p2) with
      | Some f1, Some f2 ->
          Some (Some (Num ((f1 *. w1) +. (f2 *. w2)) : percentage))
      | _ -> Option.None)
  | Some p, None | None, Some p -> Some (Some p)
  | None, None -> Some None

let mix_alpha ~w1 ~w2 ~alpha_mult a1 a2 =
  match (static_alpha_value a1, static_alpha_value a2) with
  | Some f1, Some f2 ->
      Some (alpha_of_mixed_value (((f1 *. w1) +. (f2 *. w2)) *. alpha_mult))
  | _ -> Option.None

(* Mix two hues along the shorter arc (the default interpolation method, the
   only one the typed lab-family fold handles; specified methods bail out to
   [keep]). A [none] hue carries the other operand's hue over verbatim. *)
let mix_hue_shorter ~w2 h1 h2 =
  match (h1, h2) with
  | Hue_none, Hue_none -> Some Hue_none
  | Hue_none, h | h, Hue_none -> Some h
  | h1, h2 -> (
      match (deg_of_hue h1, deg_of_hue h2) with
      | Some f1, Some f2 ->
          Some
            (Unitless (Color_space.interpolate_hue Color_space.Shorter f1 f2 w2))
      | _ -> Option.None)

let mix_lab_like_result ~l_num_max ~p1 ~p2 build l1 a1 b1 alpha1 l2 a2 b2 alpha2
    =
  match mix_weights p1 p2 with
  | None -> Option.None
  | Some (w1, w2, alpha_mult) -> (
      match
        ( mix_lightness ~l_num_max ~w1 ~w2 l1 l2,
          Some (mix_optional_float ~w1 ~w2 a1 a2),
          Some (mix_optional_float ~w1 ~w2 b1 b2),
          mix_alpha ~w1 ~w2 ~alpha_mult alpha1 alpha2 )
      with
      | Some l, Some a, Some b, Some alpha -> Some (build l a b alpha)
      | _ -> Option.None)

let mix_lch_like_result ~l_num_max ~p1 ~p2 build l1 c1 h1 alpha1 l2 c2 h2 alpha2
    =
  match mix_weights p1 p2 with
  | None -> Option.None
  | Some (w1, w2, alpha_mult) -> (
      match
        ( mix_lightness ~l_num_max ~w1 ~w2 l1 l2,
          Some (mix_optional_float ~w1 ~w2 c1 c2),
          mix_hue_shorter ~w2 h1 h2,
          mix_alpha ~w1 ~w2 ~alpha_mult alpha1 alpha2 )
      with
      | Some l, Some c, Some h, Some alpha -> Some (build l c h alpha)
      | _ -> Option.None)

(* The typed lab-family mix interpolates coordinates directly, which is only
   correct when both operands are fully opaque (premultiplying by 1 is a no-op).
   A partially transparent operand needs the premultiplied [mix_in_*_space]
   path, so a non-opaque operand falls through to [None] here and the caller
   routes it there. *)
let alpha_opaque : alpha -> bool = function
  | None -> true
  | Num f -> f >= 1.0
  | Pct f -> f >= 100.0
  | Var _ | Calc _ -> false

let mix_lab_family in_space color1 color2 ~p1 ~p2 =
  match (in_space, color1, color2) with
  | ( Some (Lab : color_space),
      Lab { l = l1; a = a1; b = b1; alpha = alpha1 },
      Lab { l = l2; a = a2; b = b2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lab_like_result ~l_num_max:100. ~p1 ~p2
        (fun l a b alpha -> Lab { l; a; b; alpha })
        l1 a1 b1 alpha1 l2 a2 b2 alpha2
  | ( Some (Oklab : color_space),
      Oklab { l = l1; a = a1; b = b1; alpha = alpha1 },
      Oklab { l = l2; a = a2; b = b2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lab_like_result ~l_num_max:1. ~p1 ~p2
        (fun l a b alpha -> Oklab { l; a; b; alpha })
        l1 a1 b1 alpha1 l2 a2 b2 alpha2
  | ( Some (Lch : color_space),
      Lch { l = l1; c = c1; h = h1; alpha = alpha1 },
      Lch { l = l2; c = c2; h = h2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lch_like_result ~l_num_max:100. ~p1 ~p2
        (fun l c h alpha -> Lch { l; c; h; alpha })
        l1 c1 h1 alpha1 l2 c2 h2 alpha2
  | ( Some (Oklch : color_space),
      Oklch { l = l1; c = c1; h = h1; alpha = alpha1 },
      Oklch { l = l2; c = c2; h = h2; alpha = alpha2 } )
    when alpha_opaque alpha1 && alpha_opaque alpha2 ->
      mix_lch_like_result ~l_num_max:1. ~p1 ~p2
        (fun l c h alpha -> Oklch { l; c; h; alpha })
        l1 c1 h1 alpha1 l2 c2 h2 alpha2
  | _ -> None

(* Convert any static colour to floating-point linear sRGB plus an alpha value
   in [0, 1]. Routes through [static_color_to_srgb_bytes] for the common case
   and through [Color_space] for spaces whose conversion matrices we now have
   wired up. Returns [None] for colours that contain a [Var] / [Calc] /
   [Current] component or for [none] sentinels (those need the per-channel
   carry-over path in [static_color_to_srgb_channels]). *)
let static_color_to_linear_srgb (c : color) :
    ((float * float * float) * float) option =
  let f255 b = Float.of_int b /. 255.0 in
  match c with
  | Color { space; components; alpha } when List.length components = 3 -> (
      let component_value : component -> float option = function
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | Component_none -> Some 0.0
        | _ -> None
      in
      match List.map component_value components with
      | [ Some c1; Some c2; Some c3 ] -> (
          let alpha_f =
            match alpha with
            | (None : alpha) -> Some 1.0
            | Num f -> Some f
            | Pct f -> Some (f /. 100.0)
            | _ -> None
          in
          match alpha_f with
          | None -> None
          | Some alpha_f -> (
              match space with
              | Srgb ->
                  let lin = Color_space.linear_rgb_of_rgb (c1, c2, c3) in
                  Some (lin, alpha_f)
              | Srgb_linear -> Some ((c1, c2, c3), alpha_f)
              | Display_p3 ->
                  let lin = Color_space.linear_rgb_of_rgb (c1, c2, c3) in
                  let xyz = Color_space.xyz65_of_linear_p3 lin in
                  Some (Color_space.linear_srgb_of_xyz65 xyz, alpha_f)
              | A98_rgb ->
                  let lin =
                    ( Color_space.linear_of_a98_rgb c1,
                      Color_space.linear_of_a98_rgb c2,
                      Color_space.linear_of_a98_rgb c3 )
                  in
                  let xyz = Color_space.xyz65_of_linear_a98 lin in
                  Some (Color_space.linear_srgb_of_xyz65 xyz, alpha_f)
              | Prophoto_rgb ->
                  let lin =
                    ( Color_space.linear_of_prophoto_rgb c1,
                      Color_space.linear_of_prophoto_rgb c2,
                      Color_space.linear_of_prophoto_rgb c3 )
                  in
                  let xyz_d50 = Color_space.xyz50_of_linear_prophoto lin in
                  let xyz_d65 = Color_space.d65_of_xyz50 xyz_d50 in
                  Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
              | Rec2020 ->
                  let lin =
                    ( Color_space.linear_of_rec2020 c1,
                      Color_space.linear_of_rec2020 c2,
                      Color_space.linear_of_rec2020 c3 )
                  in
                  let xyz = Color_space.xyz65_of_linear_rec2020 lin in
                  Some (Color_space.linear_srgb_of_xyz65 xyz, alpha_f)
              | Xyz | Xyz_d65 ->
                  Some (Color_space.linear_srgb_of_xyz65 (c1, c2, c3), alpha_f)
              | Xyz_d50 ->
                  let xyz_d65 = Color_space.d65_of_xyz50 (c1, c2, c3) in
                  Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
              | _ -> None))
      | _ -> None)
  | Oklab { l = Some l; a = Some a; b = Some b; alpha } -> (
      let l =
        match l with
        | (Num f : percentage) -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, alpha_f) with
      | Some l, Some alpha_f ->
          Some (Color_space.linear_srgb_of_oklab (l, a, b), alpha_f)
      | _ -> None)
  | Oklch { l = Some l; c = Some c; h; alpha } -> (
      let l =
        match l with
        | (Num f : percentage) -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      let h = deg_of_hue h in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, h, alpha_f) with
      | Some l, Some h, Some alpha_f ->
          let lab = Color_space.oklab_of_oklch (l, c, h) in
          Some (Color_space.linear_srgb_of_oklab lab, alpha_f)
      | _ -> None)
  | Lab { l = Some l; a = Some a; b = Some b; alpha } -> (
      let l =
        match l with
        | (Pct f : percentage) -> Some f
        | Num f -> Some f
        | _ -> None
      in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, alpha_f) with
      | Some l, Some alpha_f ->
          let xyz_d50 = Color_space.xyz50_of_lab (l, a, b) in
          let xyz_d65 = Color_space.d65_of_xyz50 xyz_d50 in
          Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
      | _ -> None)
  | Lch { l = Some l; c = Some c; h; alpha } -> (
      let l =
        match l with
        | (Pct f : percentage) -> Some f
        | Num f -> Some f
        | _ -> None
      in
      let h = deg_of_hue h in
      let alpha_f =
        match alpha with
        | (None : alpha) -> Some 1.0
        | Num f -> Some f
        | Pct f -> Some (f /. 100.0)
        | _ -> None
      in
      match (l, h, alpha_f) with
      | Some l, Some h, Some alpha_f ->
          let lab = Color_space.lab_of_lch (l, c, h) in
          let xyz_d50 = Color_space.xyz50_of_lab lab in
          let xyz_d65 = Color_space.d65_of_xyz50 xyz_d50 in
          Some (Color_space.linear_srgb_of_xyz65 xyz_d65, alpha_f)
      | _ -> None)
  | _ -> (
      match static_color_to_srgb_bytes c with
      | Some (r, g, b, a) ->
          let lin = Color_space.linear_rgb_of_rgb (f255 r, f255 g, f255 b) in
          Some (lin, f255 a)
      | None -> None)

(* CSS Color 4 sec. 14.2: the sRGB colour a display renders for a static colour,
   gamut mapping the ones sRGB cannot hold. This always answers, so it serves a
   caller that has to write an sRGB colour; the optimizer must not use it, as
   the mapped colour is not the authored one. A colour that is not static is
   returned unchanged. *)
let gamut_map_color (c : color) : color =
  match static_color_to_linear_srgb c with
  | None -> c
  | Some (linear, alpha_f) ->
      let lch =
        Color_space.oklch_of_oklab (Color_space.oklab_of_linear_srgb linear)
      in
      let r, g, b = Color_space.gamut_mapped_srgb_of_oklch lch in
      let byte v = Float.to_int (Float.round (v *. 255.)) in
      let clamp01 v = Float.max 0. (Float.min 1. v) in
      Hex { r = byte r; g = byte g; b = byte b; a = byte (clamp01 alpha_f) }

(* CSS Color 4 sec. 13.4 [hue interpolation method] mapping into the simpler
   [Color_space.hue_interpolation] enum. [Default] and [Specified] both collapse
   to [Shorter] for the static fold; sec. 13.4 makes [shorter hue] the default
   and [specified hue] is a no-op for [color-mix] inputs that aren't already in
   a polar space. *)
let color_space_hue (h : hue_interpolation) : Color_space.hue_interpolation =
  match h with
  | Shorter | Default | Specified -> Color_space.Shorter
  | Longer -> Color_space.Longer
  | Increasing -> Color_space.Increasing
  | Decreasing -> Color_space.Decreasing

let alpha_of_unit_float a : alpha =
  if a >= 1.0 -. 1e-6 then None else if a <= 1e-6 then Num 0.0 else Num a

(* Premultiplied interpolation (CSS Color 4 sec. 13.3): each coordinate is
   multiplied by its operand alpha before mixing and the sum divided by the
   interpolated alpha, so a [transparent] operand contributes only alpha and not
   its zero coordinates. The [alpha_mult] (<100%-sum) scaling lands on the final
   alpha, never the coordinates - matching [mix_srgb_bytes]. *)
let premult_mix3 ~w1 ~w2 ~a1 ~a2 (x1, y1, z1) (x2, y2, z2) =
  let interp_alpha = (a1 *. w1) +. (a2 *. w2) in
  let axis c1 c2 =
    let pre = (c1 *. a1 *. w1) +. (c2 *. a2 *. w2) in
    if interp_alpha <= 0. then 0. else pre /. interp_alpha
  in
  ((axis x1 x2, axis y1 y2, axis z1 z2), interp_alpha)

(* Polar variant: lightness and chroma premultiply like rectangular axes, but
   the hue angle never does. A zero-chroma operand ([transparent], grey, white)
   has a powerless hue (CSS Color 4 sec. 4.4.1): it carries the other operand's
   hue over instead of dragging the mix toward an undefined angle. *)
let premult_mix_polar ~w1 ~w2 ~a1 ~a2 ~hue (l1, c1, h1) (l2, c2, h2) =
  let interp_alpha = (a1 *. w1) +. (a2 *. w2) in
  let axis v1 v2 =
    let pre = (v1 *. a1 *. w1) +. (v2 *. a2 *. w2) in
    if interp_alpha <= 0. then 0. else pre /. interp_alpha
  in
  let powerless c = c <= 1e-6 in
  let h =
    match (powerless c1, powerless c2) with
    | true, true -> h1
    | true, false -> h2
    | false, true -> h1
    | false, false -> Color_space.interpolate_hue (color_space_hue hue) h1 h2 w2
  in
  ((axis l1 l2, axis c1 c2, h), interp_alpha)

let mix_in_oklab_space c1 c2 ~p1 ~p2 : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let (l, a, b), interp_alpha =
            premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2
              (Color_space.oklab_of_linear_srgb lrgb1)
              (Color_space.oklab_of_linear_srgb lrgb2)
          in
          Some
            (Oklab
               {
                 l = Some (Num l : percentage);
                 a = Some a;
                 b = Some b;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let mix_in_oklch_space c1 c2 ~p1 ~p2 hue : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let (l, c, h), interp_alpha =
            premult_mix_polar ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 ~hue
              (Color_space.oklch_of_oklab
                 (Color_space.oklab_of_linear_srgb lrgb1))
              (Color_space.oklch_of_oklab
                 (Color_space.oklab_of_linear_srgb lrgb2))
          in
          Some
            (Oklch
               {
                 l = Some (Num l : percentage);
                 c = Some c;
                 h = Unitless h;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let mix_in_lab_space c1 c2 ~p1 ~p2 : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let to_lab lrgb =
            lrgb |> Color_space.xyz65_of_linear_srgb |> Color_space.d50_of_xyz65
            |> Color_space.lab_of_xyz50
          in
          let (l, a, b), interp_alpha =
            premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 (to_lab lrgb1)
              (to_lab lrgb2)
          in
          Some
            (Lab
               {
                 l = Some (Num l : percentage);
                 a = Some a;
                 b = Some b;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let mix_in_lch_space c1 c2 ~p1 ~p2 hue : color option =
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match mix_weights p1 p2 with
      | None -> None
      | Some (w1, w2, alpha_mult) ->
          let to_lch lrgb =
            lrgb |> Color_space.xyz65_of_linear_srgb |> Color_space.d50_of_xyz65
            |> Color_space.lab_of_xyz50 |> Color_space.lch_of_lab
          in
          let (l, c, h), interp_alpha =
            premult_mix_polar ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 ~hue (to_lch lrgb1)
              (to_lch lrgb2)
          in
          Some
            (Lch
               {
                 l = Some (Num l : percentage);
                 c = Some c;
                 h = Unitless h;
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               }))
  | _ -> None

let map3 f (x, y, z) = (f x, f y, f z)

(* Inverse of the [Color] arm of [static_color_to_linear_srgb]: linear sRGB back
   into the coordinates of a predefined [color()] space, by the CSS Color 4 sec.
   19 reference conversions. [None] for the spaces [color()] cannot spell (the
   lab and polar families), which have their own mixers. *)
let coords_of_linear_srgb (space : color_space) lrgb :
    (float * float * float) option =
  match space with
  | Srgb -> Some (Color_space.rgb_of_linear_rgb lrgb)
  | Srgb_linear -> Some lrgb
  | Display_p3 ->
      Some
        (map3 Color_space.display_p3_of_linear
           (Color_space.linear_p3_of_xyz65
              (Color_space.xyz65_of_linear_srgb lrgb)))
  | A98_rgb ->
      Some
        (map3 Color_space.a98_rgb_of_linear
           (Color_space.linear_a98_of_xyz65
              (Color_space.xyz65_of_linear_srgb lrgb)))
  | Prophoto_rgb ->
      Some
        (map3 Color_space.prophoto_rgb_of_linear
           (Color_space.linear_prophoto_of_xyz50
              (Color_space.d50_of_xyz65 (Color_space.xyz65_of_linear_srgb lrgb))))
  | Rec2020 ->
      Some
        (map3 Color_space.rec2020_of_linear
           (Color_space.linear_rec2020_of_xyz65
              (Color_space.xyz65_of_linear_srgb lrgb)))
  | Xyz | Xyz_d65 -> Some (Color_space.xyz65_of_linear_srgb lrgb)
  | Xyz_d50 ->
      Some (Color_space.d50_of_xyz65 (Color_space.xyz65_of_linear_srgb lrgb))
  | Lab | Oklab | Lch | Oklch | Hsl | Hwb -> None

(* CSS Color 5 sec. 3: mixing in one of the rectangular [color()] spaces. The
   operands are carried into that space's own coordinates, mixed premultiplied
   there (CSS Color 4 sec. 13.3), and the result named in the same space. CSS
   Color 4 sec. 10.1 leaves [color()] coordinates unclamped, so a mix that ends
   outside the space's gamut is still a value: gamut mapping it (sec. 14.2) is
   what a display does, not what the colour is.

   The coordinates come back through a conversion round trip, which leaves float
   noise many orders of magnitude below a channel step. Rounding to six decimals
   clears it; [color_channel_decimals] never prints more than six for a colour
   channel, so no digit that would have been shown is lost. *)
let mix_in_rectangular_space (space : color_space) c1 c2 ~p1 ~p2 : color option
    =
  let round6 v = Float.round (v *. 1e6) /. 1e6 in
  match (static_color_to_linear_srgb c1, static_color_to_linear_srgb c2) with
  | Some (lrgb1, alpha1), Some (lrgb2, alpha2) -> (
      match
        ( coords_of_linear_srgb space lrgb1,
          coords_of_linear_srgb space lrgb2,
          mix_weights p1 p2 )
      with
      | Some x1, Some x2, Some (w1, w2, alpha_mult) ->
          let (c1, c2, c3), interp_alpha =
            premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 x1 x2
          in
          Some
            (Color
               {
                 space;
                 components =
                   [ Num (round6 c1); Num (round6 c2); Num (round6 c3) ];
                 alpha = alpha_of_unit_float (interp_alpha *. alpha_mult);
               })
      | _ -> None)
  | _ -> None

(* CSS Color 5 sec. 3 percentage normalisation: an omitted [percentN] takes the
   complement of the other side ([100% - percentM], clamped to 0); both omitted
   defaults to [50% / 50%]. *)
let color_mix_percentages (percent1 : percentage option)
    (percent2 : percentage option) =
  let f1 = Option.bind percent1 float_of_percentage in
  let f2 = Option.bind percent2 float_of_percentage in
  match (percent1, percent2) with
  | Option.None, Option.None -> Some (50., 50.)
  | Some _, Option.None ->
      Option.map (fun p1 -> (p1, Float.max 0. (100. -. p1))) f1
  | Option.None, Some _ ->
      Option.map (fun p2 -> (Float.max 0. (100. -. p2), p2)) f2
  | Some _, Some _ -> (
      match (f1, f2) with Some p1, Some p2 -> Some (p1, p2) | _ -> None)

(* [value] is the six or eight hex digits [hex_string_of_bytes] builds, never
   the shortened form: a four-digit input would be its own output. *)

(** Minify a color value by converting named colors to hex when shorter,
    matching Lightning CSS behavior. *)
let shorten_hex value =
  let len = String.length value in
  (* #RRGGBB -> #RGB when R=R, G=G, B=B *)
  if
    len = 6
    && value.[0] = value.[1]
    && value.[2] = value.[3]
    && value.[4] = value.[5]
  then (
    let s = Bytes.create 3 in
    Bytes.set s 0 value.[0];
    Bytes.set s 1 value.[2];
    Bytes.set s 2 value.[4];
    Bytes.to_string s (* #RRGGBBAA -> #RGBA when R=R, G=G, B=B, A=A *))
  else if
    len = 8
    && value.[0] = value.[1]
    && value.[2] = value.[3]
    && value.[4] = value.[5]
    && value.[6] = value.[7]
  then (
    if
      (* Further shorten #RGBA -> #RGB when A=f (fully opaque) *)
      value.[6] = 'f' || value.[6] = 'F'
    then (
      let s = Bytes.create 3 in
      Bytes.set s 0 value.[0];
      Bytes.set s 1 value.[2];
      Bytes.set s 2 value.[4];
      Bytes.to_string s)
    else
      let s = Bytes.create 4 in
      Bytes.set s 0 value.[0];
      Bytes.set s 1 value.[2];
      Bytes.set s 2 value.[4];
      Bytes.set s 3 value.[6];
      Bytes.to_string s)
  else if
    (* #RRGGBBFF -> #RRGGBB when fully opaque *)
    len = 8
    && (value.[6] = 'f' || value.[6] = 'F')
    && (value.[7] = 'f' || value.[7] = 'F')
  then String.sub value 0 6
  else value

(* Shortest hex spelling (no [#]) of decoded sRGB byte components: the opaque
   alpha is dropped and [#rrggbb] / [#rrggbbaa] shorten to [#rgb] / [#rgba] when
   each byte is a doubled nibble. *)
let hex_string_of_bytes r g b a =
  let rgb = String.concat "" [ hex_of_byte r; hex_of_byte g; hex_of_byte b ] in
  if a = 255 then shorten_hex rgb
  else shorten_hex (String.concat "" [ rgb; hex_of_byte a ])

(* Only the functional notations carry CSS Color 4 sec. 4.1's [<alpha-value>]
   slot, so a hex, a named colour and [transparent] are re-spelled as the
   [rgb()] over the sRGB bytes sec. 6.1 and sec. 6.3 give them. A colour whose
   channels are known only at used-value time has nothing to write into. *)
let rec with_alpha (c : color) (a : alpha) : color =
  let srgb r g b =
    Rgba { rgb = Channels { r = Int r; g = Int g; b = Int b }; a }
  in
  match c with
  | Hex { r; g; b; _ } | Authored_hex { r; g; b; _ } -> srgb r g b
  | Transparent -> srgb 0 0 0
  | Named n -> (
      match rgba_of_hex (snd (color_name_hex n)) with
      | Some (r, g, b, _) -> srgb r g b
      | Option.None -> c)
  | Rgb rgb | Rgba { rgb; _ } -> Rgba { rgb; a }
  | Hsl { h; s; l; _ } -> Hsl { h; s; l; a }
  | Hwb { h; w; b; _ } -> Hwb { h; w; b; a }
  | Color { space; components; _ } -> Color { space; components; alpha = a }
  | Lab { l; a = axis_a; b; _ } -> Lab { l; a = axis_a; b; alpha = a }
  | Oklab { l; a = axis_a; b; _ } -> Oklab { l; a = axis_a; b; alpha = a }
  | Lch { l; c = chroma; h; _ } -> Lch { l; c = chroma; h; alpha = a }
  | Oklch { l; c = chroma; h; _ } -> Oklch { l; c = chroma; h; alpha = a }
  (* [light-dark()] resolves to one of its two arguments, so both take it. *)
  | Light_dark (light, dark) ->
      Light_dark (with_alpha light a, with_alpha dark a)
  | Relative_rgb _ | Relative_color _ | Contrast_color _ | Attribute _
  | System _ | Var _ | Current | Mix _ | Auto | Inherit | Initial | Unset
  | Revert | Revert_layer ->
      c

let minify_color : color -> color = function
  | Named n -> (
      let name, hex = color_name_hex n in
      if hex = "" || color_name_is_shortest ~name ~hex then Named n
      else
        match rgba_of_hex hex with
        | Some (r, g, b, a) -> Hex { r; g; b; a }
        | None -> Named n)
  | c -> c
