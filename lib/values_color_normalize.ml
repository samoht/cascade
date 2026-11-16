(** Colour canonicalisation: the value-changing folds of the optimize pass. *)

open Values_intf
open Values_color_name
open Values_calc
open Values_color_fold
open Values_numeric
open Values_color_pp
open Values_read_color

(* Shortest spelling of a hex value as a [color]: shorten, then take the named
   form when the table has one, which it holds only for the colours whose name
   is the shorter spelling (the [pp_hex_color] choice, as an AST rewrite
   producing a [color] instead of printing). *)
let canonical_color_of_hex r g b a : color =
  match color_name_of_hex (hex_string_of_bytes r g b a) with
  | Some name -> Named name
  | None -> Hex { r; g; b; a }

(* Canonicalise a colour's alpha for a colour the static fold leaves alone (e.g.
   a [var()] channel). CSS Color 4 sec. 4.2: a fully-opaque [/ 1] / [/ 100%] is
   redundant and drops; an alpha [<percentage>] is the [<number>] divided by 100
   and serialises as the number. *)
let alpha_scale ?(exact = true) op (a : alpha) n : alpha option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match a with
    | Num v -> scaled (fun x -> (Num x : alpha)) v
    | Pct v -> scaled (fun x -> (Pct x : alpha)) v
    | _ -> Option.none

let alpha_combine op (a : alpha) (b : alpha) : alpha option =
  let f = combine_leaf op in
  match (a, b) with
  | Num x, Num y -> Option.map (fun r -> (Num r : alpha)) (f x y)
  | Pct x, Pct y -> Option.map (fun r -> (Pct r : alpha)) (f x y)
  | _ -> Option.none

let alpha_is_zero : alpha -> bool = function
  | Num f | Pct f -> f = 0.
  | _ -> false

let alpha_calc_math_fn (fn : math_fn) : alpha calc =
  typed_math_fn
    (fun unit v ->
      if String.equal unit "%" then Option.some (Pct v : alpha) else Option.none)
    fn

let eval_alpha_calc ?(ctx = default_calc_ctx) (c : alpha calc) : alpha calc =
  eval_typed_calc ~math_fn:alpha_calc_math_fn
    ~scale:(fun ~exact op v n -> alpha_scale ~exact op v n)
      (* An alpha coefficient prints in full through [Pp.float] / [Pp.pct]. *)
    ~combine:alpha_combine ~round:Fun.id ~is_zero:alpha_is_zero
    ~zero:(Val (Num 0.) : alpha calc)
    ~ctx c

let rec normalize_alpha (a : alpha) : alpha =
  match a with
  | Num 1.0 | Pct 100.0 -> None
  | Pct f -> Num (f /. 100.)
  | Calc c -> (
      match eval_alpha_calc c with
      | Num f -> normalize_alpha (Num f)
      | Val v -> normalize_alpha v
      | folded -> Calc folded)
  | other -> other

let drop_full_alpha (c : color) : color =
  match c with
  | Rgba { rgb; a } -> Rgba { rgb; a = normalize_alpha a }
  | Hsl r -> Hsl { r with a = normalize_alpha r.a }
  | Hwb r -> Hwb { r with a = normalize_alpha r.a }
  | Color r -> Color { r with alpha = normalize_alpha r.alpha }
  | Oklab r -> Oklab { r with alpha = normalize_alpha r.alpha }
  | Oklch r -> Oklch { r with alpha = normalize_alpha r.alpha }
  | Lab r -> Lab { r with alpha = normalize_alpha r.alpha }
  | Lch r -> Lch { r with alpha = normalize_alpha r.alpha }
  | _ -> c

(* CSS Color 4 secs. 16.3-16.4 makes [<percentage>] and [<number>] equivalent
   for oklch/oklab/lch/lab lightness ([num = pct *. pct_scale]: ok* L 100% = 1,
   lch/lab L 100% = 100). Canonicalise to the shorter spelling here, in the AST
   normalize pass, so [pp_color_lightness] serialises the node faithfully rather
   than swapping percentage for number at print time. *)
(* Round a lab/oklab/lch/oklch coefficient to its canonical decimal budget. The
   printer serialises faithfully, so this AST fold owns the value-changing
   precision reduction: [axis_max_decimals] picks the shortest form for the
   canonical (optimize) output, while a consumer that skips [normalize_color]
   (e.g. [to_string ~minify:true] on a typed colour) keeps the authored
   coefficient. [lossless] suppresses the reduction. *)
let round_color_axis ~lossless ~decimals f =
  if lossless then f
  else
    let f = Pp.round_sig 6 f in
    let m = 10. ** float_of_int decimals in
    Float.round (f *. m) /. m

(* Round an oklch/lch hue to 3 decimals for the canonical (optimize) form and
   collapse every concrete angle unit to canonical bare degrees. A [var()] /
   [calc()] / [none] hue is left opaque. *)
let round_hue ~lossless (h : hue) : hue =
  let r f = round_color_axis ~lossless ~decimals:3 f in
  match h with
  | Unitless f -> Unitless (r f)
  | Angle (Deg f) -> Unitless (r f)
  | Angle a -> (
      match deg_of_hue (Angle a) with
      | Some f -> Unitless (r (normalize_hue f))
      | None -> h)
  | Hue_none | Var _ -> h

let canonical_hsl_hue ~lossless (h : hue) : hue =
  match h with
  | Angle (Deg f) -> Unitless f
  | Angle a -> (
      match deg_of_hue (Angle a) with
      | Some f ->
          Unitless (round_color_axis ~lossless ~decimals:3 (normalize_hue f))
      | None -> h)
  | Unitless _ | Hue_none | Var _ -> h

let canonical_hsl_hue_in_color ~lossless = function
  | Hsl r -> Hsl { r with h = canonical_hsl_hue ~lossless r.h }
  | Hwb r -> Hwb { r with h = canonical_hsl_hue ~lossless r.h }
  | color -> color

let canonical_color_lightness ~lossless ~pct_scale ~axis_max_decimals
    (l : percentage option) : percentage option =
  let fmt ~max_decimals f =
    let max_decimals = if lossless then 8 else max_decimals in
    Pp.string_of_float ~drop_leading_zero:true ~max_decimals
      (if lossless then f else Pp.round_sig 6 f)
  in
  let num_len f = String.length (fmt ~max_decimals:axis_max_decimals f) in
  let pct_len f =
    String.length (fmt ~max_decimals:(axis_max_decimals + 2) f) + 1
  in
  let num f : percentage =
    Num (round_color_axis ~lossless ~decimals:axis_max_decimals f)
  in
  let pct f : percentage =
    Pct (round_color_axis ~lossless ~decimals:(axis_max_decimals + 2) f)
  in
  match l with
  | Some (Pct f) ->
      (* Preserve the node's representation on an equal-length spelling. A
         trailing [%] can terminate the token before an unsigned following axis,
         so an authored [oklab(25% 20 50)] stays [oklab(25%20 50)]. *)
      if num_len (f *. pct_scale) < pct_len f then Some (num (f *. pct_scale))
      else Some (pct f)
  | Some (Num f) ->
      (* A computed lightness is numeric. Keeping [Num] on a tie avoids turning
         a colour-mix result such as [.54] into [54%]. *)
      if pct_len (f /. pct_scale) < num_len f then Some (pct (f /. pct_scale))
      else Some (num f)
  | other -> other

let normalize_static_modern_color ~exact_srgb ~lossless c =
  let hex_of_bytes r g b (a : alpha) =
    match alpha_value_byte a with
    | Some ab -> canonical_color_of_hex r g b ab
    | Option.None -> c
  in
  if lossless then
    match
      if exact_srgb then exact_srgb_function_channels c else Option.None
    with
    | Some (r, g, b, alpha) -> (
        match exact_alpha_value_byte alpha with
        | Some a -> canonical_color_of_hex r g b a
        | Option.None ->
            (* The channels are exact but the alpha is not a whole byte, so
               [rgb()] is as far as the fold goes: it is the same spelling the
               lossless path leaves an authored [rgb()] in. *)
            drop_full_alpha
              (Rgba
                 {
                   rgb = Channels { r = Int r; g = Int g; b = Int b };
                   a = alpha;
                 }))
    | Option.None -> drop_full_alpha c
  else
    match static_color_to_linear_srgb c with
    | Some (linear, alpha_f) -> (
        match
          Color_space.srgb_bytes_of_linear
            ~error:(conversion_error_of_color c)
            linear
        with
        | Some (r, g, b) ->
            let clamp01 v = Float.max 0.0 (Float.min 1.0 v) in
            let alpha_byte =
              Float.to_int (Float.round (clamp01 alpha_f *. 255.0))
            in
            let a : alpha =
              if alpha_byte = 255 then None
              else if alpha_byte = 0 then Num 0.0
              else Num (Float.of_int alpha_byte /. 255.0)
            in
            hex_of_bytes r g b a
        | None -> drop_full_alpha c)
    | None -> drop_full_alpha c

(* [canonical_color_of_hex] answers a name or the same bytes back, so a [Hex]
   that reaches its own canonical form is shared rather than rebuilt. *)
let normalize_hex_color c r g b a =
  match canonical_color_of_hex r g b a with Hex _ -> c | color -> color

let normalize_named_color c orig_name =
  (* Pick the shortest spelling: a named colour collapses to hex only when the
     SHORTENED hex is shorter than the name. [canonical_color_name] first folds
     aliases (grey -> gray) so the choice is made on the canonical spelling. *)
  let name = canonical_color_name orig_name in
  let name_str, hex = color_name_hex name in
  match rgba_of_hex hex with
  | Some (r, g, b, a)
    when String.length (hex_string_of_bytes r g b a) + 1
         <= String.length name_str ->
      canonical_color_of_hex r g b a
  | _ -> if name == orig_name then c else Named name

let normalize_srgb_mix color1 color2 ~p1 ~p2 =
  let srgb_operand color =
    match static_color_to_srgb_channels color with
    | Some (Some r, Some g, Some b, Some a) ->
        let unit byte = Float.of_int byte /. 255. in
        Some ((unit r, unit g, unit b), unit a)
    | Some _ -> None
    | None ->
        Option.map
          (fun (linear, alpha) -> (Color_space.rgb_of_linear_rgb linear, alpha))
          (static_color_to_linear_srgb color)
  in
  match (srgb_operand color1, srgb_operand color2, mix_weights p1 p2) with
  | Some (rgb1, alpha1), Some (rgb2, alpha2), Some (w1, w2, alpha_mult) -> (
      let (r, g, b), interp_alpha =
        premult_mix3 ~w1 ~w2 ~a1:alpha1 ~a2:alpha2 rgb1 rgb2
      in
      let alpha = interp_alpha *. alpha_mult in
      let linear = Color_space.linear_rgb_of_rgb (r, g, b) in
      (* An operand written in sRGB terms mixes by exact arithmetic every engine
         performs alike; an operand converted from another space carries that
         conversion's error into the result. *)
      let mix_error =
        if
          static_color_to_srgb_channels color1 <> None
          && static_color_to_srgb_channels color2 <> None
        then 0.
        else Color_space.conversion_error
      in
      match Color_space.srgb_bytes_of_linear ~error:mix_error linear with
      | Some (r, g, b) ->
          let clamp01 v = Float.max 0. (Float.min 1. v) in
          let a = Float.to_int (Float.round (clamp01 alpha *. 255.)) in
          Some (canonical_color_of_hex r g b a)
      | None ->
          (* CSS Color 4 sec. 10.1 leaves out-of-gamut sRGB coordinates
             unclamped, so retain them in [color()] notation. *)
          let round6 v = Float.round (v *. 1e6) /. 1e6 in
          Some
            (Color
               {
                 space = Srgb;
                 components = [ Num (round6 r); Num (round6 g); Num (round6 b) ];
                 alpha = alpha_of_unit_float alpha;
               }))
  | _ -> (
      (* The floating-point converter declines sRGB [none] channels. Keep the
         channel carry-over behaviour in the byte mixer for that case. *)
      match mix_srgb_bytes color1 color2 ~p1 ~p2 with
      | Some (r, g, b, a) -> Some (canonical_color_of_hex r g b a)
      | Option.None -> None)

let normalize_lab_family_mix effective color1 color2 ~p1 ~p2 =
  match mix_lab_family effective color1 color2 ~p1 ~p2 with
  | Some color -> Some color
  | None -> (
      match effective with
      | Some (Lab : color_space) -> mix_in_lab_space color1 color2 ~p1 ~p2
      | Some Oklab -> mix_in_oklab_space color1 color2 ~p1 ~p2
      | Some Lch -> mix_in_lch_space color1 color2 ~p1 ~p2 Default
      | Some Oklch -> mix_in_oklch_space color1 color2 ~p1 ~p2 Default
      | _ -> None)

(* Round the surviving coefficients of a lab-family colour to its canonical
   decimal budget. Applied only after the sRGB fold has run, so a colour that
   already collapsed to hex / named keeps its full-precision projection and only
   one that stayed in its own space (out of gamut) gets rounded for display. *)
let round_lab_family_axes ~lossless (c : color) : color =
  let r3 = Option.map (round_color_axis ~lossless ~decimals:3) in
  let r1 = Option.map (round_color_axis ~lossless ~decimals:1) in
  let ok_l =
    canonical_color_lightness ~lossless ~pct_scale:0.01 ~axis_max_decimals:3
  in
  let lab_l =
    canonical_color_lightness ~lossless ~pct_scale:1.0 ~axis_max_decimals:1
  in
  match c with
  | Oklch r ->
      Oklch { r with l = ok_l r.l; c = r3 r.c; h = round_hue ~lossless r.h }
  | Oklab r -> Oklab { r with l = ok_l r.l; a = r3 r.a; b = r3 r.b }
  | Lch r ->
      Lch { r with l = lab_l r.l; c = r1 r.c; h = round_hue ~lossless r.h }
  | Lab r -> Lab { r with l = lab_l r.l; a = r1 r.a; b = r1 r.b }
  | other -> other

(* What a browser paints for a colour, as the minifier needs it: the fold
   verdict of the linear-sRGB colour it converts to. *)
let srgb_verdict_of_color (c : color) : Color_space.srgb_fold =
  match static_color_to_linear_srgb c with
  | Some (linear, _) ->
      Color_space.srgb_fold_of_linear
        ~error:(conversion_error_of_color c)
        linear
  | None -> Color_space.Out_of_gamut

(* Reduce the coefficients of a colour that did not fold, but only as far as the
   fold verdict stays the same. An in-gamut colour whose byte is ambiguous must
   keep the precision that fixes which side of the half-integer it is on, and an
   out-of-gamut colour must keep the precision the engine gamut maps, so a
   shorter spelling that changes either is not emitted. Where the canonical
   rounding is safe it is taken, so the minified output stays as short as it was
   and repeats: the rounded value yields the same verdict. *)
let verdict_preserving (rounded : color) (exact : color) : color =
  if
    Color_space.equal_srgb_fold
      (srgb_verdict_of_color rounded)
      (srgb_verdict_of_color exact)
  then rounded
  else exact

(* The [color()] components at their canonical precision, in the AST, so the
   emitted text and the fold decision agree and minify is a fixed point. The
   printer serialises the node faithfully. *)
let round_modern_color_axes ~lossless (c : color) : color =
  let round_component decimals (component : component) : component =
    match component with
    | Num f -> Num (round_color_axis ~lossless ~decimals f)
    | Pct f -> Pct (round_color_axis ~lossless ~decimals (f /. 100.) *. 100.)
    | other -> other
  in
  match c with
  | Color r ->
      let decimals =
        color_channel_decimals r.space (color_numeric_channels r.components)
      in
      Color
        { r with components = List.map (round_component decimals) r.components }
  | other -> other

(* CSS Color 4 sec. 4.4: "a missing component behaves as a zero value, in the
   appropriate unit for that component", and the spec names rendering the colour
   and converting it to another space among the purposes that holds for. The
   sRGB family already resolves its own [none] channels that way on the path
   through [static_color_to_srgb_bytes]; the Lab family reaches no fold at all
   while a channel is missing, so a converted achromatic OKLab and the hex it
   stands for never meet. This is that half, written as its own step so the
   folds below read one shape of colour.

   Sec. 13.3 is why it is not unconditional: interpolation gives a missing
   component the other colour's analogous component instead of a zero, so the
   two spellings are two different ramps there. *)
let zero_missing_components (c : color) : color =
  let lightness (l : percentage option) : percentage option =
    match l with Option.None -> Some (Pct 0.) | l -> l
  in
  let axis (a : float option) =
    match a with Option.None -> Some 0. | a -> a
  in
  let hue (h : hue) : hue = match h with Hue_none -> Unitless 0. | h -> h in
  match c with
  | Lab r -> Lab { r with l = lightness r.l; a = axis r.a; b = axis r.b }
  | Oklab r -> Oklab { r with l = lightness r.l; a = axis r.a; b = axis r.b }
  | Lch r -> Lch { r with l = lightness r.l; c = axis r.c; h = hue r.h }
  | Oklch r -> Oklch { r with l = lightness r.l; c = axis r.c; h = hue r.h }
  | other -> other

(* CSS Color 5 sec. 4.1: a relative colour whose channel slots are the
   function's own keywords, in order, is the origin converted to that space,
   with the alpha the call names; an omitted or [alpha] slot carries the
   origin's. The conversion is the one the browser makes, so it is a value fold
   like the oklab-to-hex one and runs where that one does, outside [lossless]. A
   tail that rewrites a channel is another colour and stays. *)
let relative_pass_through_alpha ~origin_alpha tail_alpha : alpha option =
  match String.trim tail_alpha with
  | "" | "alpha" -> Some origin_alpha
  | text -> (
      let len = String.length text in
      if len > 1 && text.[len - 1] = '%' then
        match float_of_string_opt (String.sub text 0 (len - 1)) with
        | Some f -> Some (Pct f : alpha)
        | None -> None
      else
        match float_of_string_opt text with
        | Some f -> Some (Num f : alpha)
        | None -> None)

let fold_relative_color_static_origin name origin tail : color option =
  let channels, tail_alpha =
    match String.index_opt tail '/' with
    | Some i ->
        ( String.trim (String.sub tail 0 i),
          String.sub tail (i + 1) (String.length tail - i - 1) )
    | None -> (String.trim tail, "")
  in
  let keywords =
    match name with
    | "lab" | "oklab" -> Some "l a b"
    | "lch" | "oklch" -> Some "l c h"
    | _ -> None
  in
  if keywords <> Some channels then None
  else
    match static_color_to_linear_srgb origin with
    | None -> None
    | Some (linear, origin_alpha_f) -> (
        let origin_alpha : alpha =
          if origin_alpha_f >= 1. then None else Num origin_alpha_f
        in
        match relative_pass_through_alpha ~origin_alpha tail_alpha with
        | None -> None
        | Some alpha -> (
            let lab_d50 () =
              Color_space.lab_of_xyz50
                (Color_space.d50_of_xyz65
                   (Color_space.xyz65_of_linear_srgb linear))
            in
            match name with
            | "oklab" ->
                let l, a, b = Color_space.oklab_of_linear_srgb linear in
                Some
                  (Oklab
                     {
                       l = Some (Pct (l *. 100.));
                       a = Some a;
                       b = Some b;
                       alpha;
                     })
            | "oklch" ->
                let l, c, h =
                  Color_space.oklch_of_oklab
                    (Color_space.oklab_of_linear_srgb linear)
                in
                Some
                  (Oklch
                     {
                       l = Some (Pct (l *. 100.));
                       c = Some c;
                       h = Unitless h;
                       alpha;
                     })
            | "lab" ->
                let l, a, b = lab_d50 () in
                Some (Lab { l = Some (Pct l); a = Some a; b = Some b; alpha })
            | "lch" ->
                let l, c, h = Color_space.lch_of_lab (lab_d50 ()) in
                Some
                  (Lch { l = Some (Pct l); c = Some c; h = Unitless h; alpha })
            | _ -> None))

(* AST-level color canonicalisation: the value-changing colour folds live here,
   producing a canonical [color] so [pp_color] stays a pure serialiser. The sRGB
   fold runs on the authored coefficients first; [round_lab_family_axes] then
   rounds only what survives in its own colour space. *)
let rec normalize_color ?(lossless = false) ?(exact_srgb = false)
    ?(resolve_missing = false) (c : color) : color =
  let c = if resolve_missing then zero_missing_components c else c in
  let normalize_color ?(lossless = lossless) =
    normalize_color ~lossless ~exact_srgb
  in
  let hex_of_byte_quad r g b ab = canonical_color_of_hex r g b ab in
  let static_fold () =
    let folded = normalize_static_modern_color ~exact_srgb ~lossless c in
    verdict_preserving (round_lab_family_axes ~lossless folded) folded
  in
  match c with
  | Oklab { l = Some l; a = None; b = None; alpha }
    when pct_is_zero l && alpha_is_zero alpha ->
      Transparent
  | Oklch { l = Some _; c = Some _; _ } -> static_fold ()
  | Oklab { l = Some _; a = Some _; b = Some _; _ } -> static_fold ()
  | Lch { l = Some _; c = Some _; _ } -> static_fold ()
  | Lab { l = Some _; a = Some _; b = Some _; _ } -> static_fold ()
  | Color _ ->
      let folded = normalize_static_modern_color ~exact_srgb ~lossless c in
      verdict_preserving (round_modern_color_axes ~lossless folded) folded
  | Hex { r; g; b; a } -> normalize_hex_color c r g b a
  (* The authored spelling is a pretty-printing detail the printer reads, not
     part of the colour, so the fold drops it: two spellings of one colour have
     to reach the same node or nothing downstream can see them as one value. *)
  | Authored_hex { r; g; b; a; _ } -> canonical_color_of_hex r g b a
  | Named orig_name -> normalize_named_color c orig_name
  | Rgb _ | Rgba _ | Hsl _ | Hwb _ | Transparent -> (
      let bytes =
        if lossless then exact_rgb_to_srgb_bytes c
        else static_color_to_srgb_bytes c
      in
      match bytes with
      | Some (r, g, b, a) -> hex_of_byte_quad r g b a
      | Option.None -> canonical_hsl_hue_in_color ~lossless (drop_full_alpha c))
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      normalize_mix_color ~lossless ~in_space ~hue ~color1 ~percent1 ~color2
        ~percent2
  | Light_dark (l, d) ->
      Light_dark (normalize_color ~lossless l, normalize_color ~lossless d)
  | Contrast_color inner -> Contrast_color (normalize_color ~lossless inner)
  | Relative_rgb (origin, tail) ->
      Relative_rgb (normalize_color ~lossless origin, tail)
  | Relative_color (name, origin, tail) -> (
      let origin = normalize_color ~lossless origin in
      let folded =
        if lossless then Option.None
        else fold_relative_color_static_origin name origin tail
      in
      match folded with
      | Option.Some color -> normalize_color ~lossless color
      | Option.None -> (
          match light_dark_relative ~lossless name origin tail with
          | Option.Some color -> color
          | Option.None -> Relative_color (name, origin, tail)))
  | Var v ->
      (* A typed [var()] fallback / default is a colour, so canonicalise it the
         same way it would be if it stood alone. The opaque [Syntax_fallback] /
         [Var_fallback] forms are token streams, not typed colours, and stay
         untouched. *)
      let fallback =
        match v.fallback with
        | Fallback c -> Fallback (normalize_color ~lossless c)
        | (Empty | Empty2 | None | Syntax_fallback _ | Var_fallback _) as other
          ->
            other
      in
      Var
        {
          v with
          fallback;
          default = Option.map (normalize_color ~lossless) v.default;
        }
  | _ -> c

and normalize_mix_color ~lossless ~in_space ~hue ~color1 ~percent1 ~color2
    ~percent2 =
  let keep () =
    (* CSS Color 5 sec. 3 / CSS Color 4 sec. 13.4: [shorter] is the default hue
       and drops, and percentages that restate the [100% - other] default drop
       too. The interpolation method itself is required syntax, so a written [in
       oklab] stays - dropping it yields [color-mix(<color>, <color>)], which
       every browser rejects. *)
    let hue = match hue with Shorter -> Default | h -> h in
    let pct (p : percentage option) =
      match p with Some (Pct f) -> Some f | _ -> None
    in
    let percent1, percent2 =
      match (pct percent1, pct percent2) with
      | Some a, Some b when Float.abs (a +. b -. 100.) < 0.0001 ->
          let none : percentage option = None in
          if Float.abs (a -. 50.) < 0.0001 then (none, none)
          else (percent1, none)
      | _ -> (percent1, percent2)
    in
    Mix
      {
        in_space;
        hue;
        color1 = normalize_color ~lossless color1;
        percent1;
        color2 = normalize_color ~lossless color2;
        percent2;
      }
  in
  let folded : color option =
    if lossless then None
    else
      match (in_space, hue, color_mix_percentages percent1 percent2) with
      | Some Srgb, Default, Some (p1, p2) ->
          normalize_srgb_mix color1 color2 ~p1 ~p2
      | ( Some
            (( Srgb_linear | Display_p3 | A98_rgb | Prophoto_rgb | Rec2020 | Xyz
             | Xyz_d50 | Xyz_d65 ) as space),
          Default,
          Some (p1, p2) ) ->
          mix_in_rectangular_space space color1 color2 ~p1 ~p2
      | ((Some (Lab | Oklab | Lch | Oklch) | None) as sp), Default, Some (p1, p2)
        ->
          (* CSS Color 5 sec. 3: [in oklab] is the default interpolation space,
             so an absent space folds the same as [Some Oklab]. *)
          let effective =
            match sp with Some _ -> sp | None -> Some (Oklab : color_space)
          in
          normalize_lab_family_mix effective color1 color2 ~p1 ~p2
      | _ -> None
  in
  match folded with
  | Some color -> normalize_color ~lossless color
  | None -> (
      (* A mix over a [light-dark()] operand is the [light-dark()] of the mix
         over each branch. *)
      match
        light_dark_mix ~lossless ~in_space ~hue ~color1 ~percent1 ~color2
          ~percent2
      with
      | Some color -> color
      | None -> keep ())

(* CSS Color 5 sec. 6: [light-dark()] computes to one of its two colours by the
   element's used colour scheme, so a mix over a [light-dark()] argument is the
   [light-dark()] of the mix over each branch, and the two arguments distribute
   at once. Exact only where each branch folds to a colour: a mix a branch keeps
   stays as written, so the two spellings of it still report. A value fold like
   the mix itself, so [lossless] holds it off. *)
and light_dark_mix ~lossless ~in_space ~hue ~color1 ~percent1 ~color2 ~percent2
    =
  let branches c =
    match c with Light_dark (l, d) -> Some (l, d) | _ -> None
  in
  let c1 = normalize_color ~lossless color1 in
  let c2 = normalize_color ~lossless color2 in
  match (branches c1, branches c2) with
  | _ when lossless -> None
  | None, None -> None
  | b1, b2 -> (
      let l1, d1 = Option.value b1 ~default:(c1, c1) in
      let l2, d2 = Option.value b2 ~default:(c2, c2) in
      let mix color1 color2 =
        normalize_mix_color ~lossless ~in_space ~hue ~color1 ~percent1 ~color2
          ~percent2
      in
      match (mix l1 l2, mix d1 d2) with
      | Mix _, _ | _, Mix _ -> None
      | light, dark -> Some (Light_dark (light, dark)))

(* The same reading for a relative colour: [oklab(from light-dark(a, b) ...)] is
   the [light-dark()] of the call over each branch, kept where a branch keeps
   its call. *)
and light_dark_relative ~lossless name origin tail =
  match origin with
  | Light_dark (l, d) when not lossless -> (
      let over origin =
        normalize_color ~lossless (Relative_color (name, origin, tail))
      in
      match (over l, over d) with
      | Relative_color _, _ | _, Relative_color _ -> None
      | light, dark -> Some (Light_dark (light, dark)))
  | _ -> None
