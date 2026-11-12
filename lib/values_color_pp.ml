(** Colour printers. *)

open Values_intf
open Values_var
open Values_calc
open Values_length
open Values_color_fold
open Values_numeric

(* CSS Color 4 sec. 15.5 normalises system colour keywords to lowercase
   ASCII. *)
let pp_system_color : system_color Pp.t =
 fun ctx -> function
  | Accent_color -> Pp.string ctx "accentcolor"
  | Accent_color_text -> Pp.string ctx "accentcolortext"
  | Active_text -> Pp.string ctx "activetext"
  | Button_border -> Pp.string ctx "buttonborder"
  | Button_face -> Pp.string ctx "buttonface"
  | Button_text -> Pp.string ctx "buttontext"
  | Canvas -> Pp.string ctx "canvas"
  | Canvas_text -> Pp.string ctx "canvastext"
  | Field -> Pp.string ctx "field"
  | Field_text -> Pp.string ctx "fieldtext"
  | Gray_text -> Pp.string ctx "graytext"
  | Highlight -> Pp.string ctx "highlight"
  | Highlight_text -> Pp.string ctx "highlighttext"
  | Link_text -> Pp.string ctx "linktext"
  | Mark -> Pp.string ctx "mark"
  | Mark_text -> Pp.string ctx "marktext"
  | Selected_item -> Pp.string ctx "selecteditem"
  | Selected_item_text -> Pp.string ctx "selecteditemtext"
  | Visited_text -> Pp.string ctx "visitedtext"
  | Webkit_focus_ring_color -> Pp.string ctx "-webkit-focus-ring-color"

let rec pp_channel : channel Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_channel ctx v
  | None -> Pp.string ctx "none"

(* A bare or [deg] hue prints in full precision. Minified output can omit an
   explicit [deg] exactly; converting another angle unit or reducing precision
   remains a node-changing fold owned by the AST normalize pass
   ([round_hue]). *)
let pp_hue_float ctx f =
  Pp.string ctx (Pp.string_of_float ~drop_leading_zero:true f)

let rec pp_hue : hue Pp.t =
 fun ctx -> function
  | Unitless f -> pp_hue_float ctx f
  | Angle (Deg f) when Pp.minified ctx -> pp_hue_float ctx f
  | Angle a -> pp_angle ctx a
  | Var v -> pp_var pp_hue ctx v
  | Hue_none -> Pp.string ctx "none"

let rec pp_alpha : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f ->
      (* CSSOM serialisation (CSSOM 1 sec. 6.7.2) drops a leading zero on
         fractional numbers: emit [.25] not [0.25] in both modes. Under minify,
         round to 3 decimals (alpha precision is 1/255 ~ 0.004 in sRGB); alpha
         is a colour channel, so [lossless] opts out of that fold like the other
         channels. *)
      let max_decimals =
        if Pp.minified ctx && not ctx.Pp.lossless then 3 else 8
      in
      Pp.float_n max_decimals ctx f
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_alpha ctx v
  | Calc c -> pp_calc pp_alpha ctx c

(* Helper to print optional alpha with the correct leading separator *)
let pp_opt_alpha ctx = function
  | None -> ()
  | (Num _ | Pct _ | Var _ | Calc _) as a ->
      Pp.op_char ctx '/';
      pp_alpha ctx a

(* Convert the three channels of a [color(<space> ...)] value to Oklab so a
   candidate rounding can be measured against the color-difference budget.
   Returns [None] for spaces that are not [color()] predefined-RGB / XYZ
   spaces. *)
let color_func_to_oklab (space : color_space) (c1, c2, c3) :
    Color_space.lab option =
  let to_oklab = Color_space.oklab_of_linear_srgb in
  let from_xyz_d65 xyz = to_oklab (Color_space.linear_srgb_of_xyz65 xyz) in
  match space with
  | Srgb ->
      Some
        (to_oklab
           ( Color_space.linear_of_srgb c1,
             Color_space.linear_of_srgb c2,
             Color_space.linear_of_srgb c3 ))
  | Srgb_linear -> Some (to_oklab (c1, c2, c3))
  | Display_p3 ->
      Some
        (from_xyz_d65
           (Color_space.xyz65_of_linear_p3
              ( Color_space.linear_of_display_p3 c1,
                Color_space.linear_of_display_p3 c2,
                Color_space.linear_of_display_p3 c3 )))
  | A98_rgb ->
      Some
        (from_xyz_d65
           (Color_space.xyz65_of_linear_a98
              ( Color_space.linear_of_a98_rgb c1,
                Color_space.linear_of_a98_rgb c2,
                Color_space.linear_of_a98_rgb c3 )))
  | Prophoto_rgb ->
      Some
        (from_xyz_d65
           (Color_space.d65_of_xyz50
              (Color_space.xyz50_of_linear_prophoto
                 ( Color_space.linear_of_prophoto_rgb c1,
                   Color_space.linear_of_prophoto_rgb c2,
                   Color_space.linear_of_prophoto_rgb c3 ))))
  | Rec2020 ->
      Some
        (from_xyz_d65
           (Color_space.xyz65_of_linear_rec2020
              ( Color_space.linear_of_rec2020 c1,
                Color_space.linear_of_rec2020 c2,
                Color_space.linear_of_rec2020 c3 )))
  | Xyz | Xyz_d65 -> Some (from_xyz_d65 (c1, c2, c3))
  | Xyz_d50 -> Some (from_xyz_d65 (Color_space.d65_of_xyz50 (c1, c2, c3)))
  | Lab | Oklab | Lch | Oklch | Hsl | Hwb -> None

(* The three numeric channels of a [color()] value, with percentages normalised
   to [0, 1]; [None] when any channel is non-numeric (var/calc/none). *)
let color_numeric_channels (components : component list) =
  match components with
  | [ a; b; c ] -> (
      let value (component : component) =
        match component with
        | Num f -> Some f
        | Pct f -> Some (f /. 100.)
        | _ -> None
      in
      match (value a, value b, value c) with
      | Some a, Some b, Some c -> Some (a, b, c)
      | _ -> None)
  | _ -> None

(* Fewest decimals whose round-trip stays within the documented 0.002 Oklab
   budget (README). Channels in gamma-encoded RGB spaces reach it at 3 decimals;
   linear-light / XYZ spaces, where a fixed channel step carries more Oklab
   distance, need 4. Falls back to 4 when the budget cannot be checked. *)
let color_channel_decimals (space : color_space)
    (channels : (float * float * float) option) =
  match channels with
  | None -> 4
  | Some (c1, c2, c3) -> (
      match color_func_to_oklab space (c1, c2, c3) with
      | None -> 4
      | Some orig ->
          let round_dec n x =
            let m = 10. ** float_of_int n in
            Float.round (x *. m) /. m
          in
          let within n =
            match
              color_func_to_oklab space
                (round_dec n c1, round_dec n c2, round_dec n c3)
            with
            | Some rounded -> Color_space.oklab_distance orig rounded <= 0.002
            | None -> false
          in
          let rec find n =
            if n >= 6 then 6 else if within n then n else find (n + 1)
          in
          find 3)

let pp_component_float ~decimals ctx f =
  let decimals = if ctx.Pp.lossless then 8 else decimals in
  if Pp.minified ctx then
    Pp.string ctx
      (Pp.string_of_float ~drop_leading_zero:true ~max_decimals:decimals f)
  else Pp.float ctx f

let rec pp_component_prec ~decimals : component Pp.t =
 fun ctx -> function
  | Num f -> pp_component_float ~decimals ctx f
  | Pct f when Pp.minified ctx -> pp_component_float ~decimals ctx (f /. 100.)
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Angle h -> pp_hue ctx h
  | Var v -> pp_var (pp_component_prec ~decimals) ctx v
  | Calc c -> pp_calc (pp_component_prec ~decimals) ctx c
  | Component_none -> Pp.string ctx "none"

(* Context-free single component: without the sibling channels the Oklab budget
   cannot be checked, so use the budget-safe 4-decimal fallback. *)
let pp_component : component Pp.t = pp_component_prec ~decimals:4

let pp_hue_interpolation : hue_interpolation Pp.t =
 fun ctx -> function
  | Shorter -> Pp.string ctx "shorter"
  | Longer -> Pp.string ctx "longer"
  | Increasing -> Pp.string ctx "increasing"
  | Decreasing -> Pp.string ctx "decreasing"
  | Specified -> Pp.string ctx "specified"
  | Default -> ()

let static_component_can_touch_negative (component : component) =
  match component with
  | Num _ | Pct _ -> true
  | Angle _ | Var _ | Calc _ | Component_none -> false

let static_component_starts_negative (component : component) =
  match component with
  | Num f | Pct f -> f < 0.
  | Angle _ | Var _ | Calc _ | Component_none -> false

let color_component_needs_space ctx prev component =
  not
    (Pp.minified ctx
    && static_component_can_touch_negative prev
    && static_component_starts_negative component)

let rec pp_color_component_tail ~decimals ctx prev = function
  | [] -> ()
  | component :: rest ->
      if color_component_needs_space ctx prev component then Pp.space ctx ();
      pp_component_prec ~decimals ctx component;
      pp_color_component_tail ~decimals ctx component rest

let pp_color_components ~decimals : component list Pp.t =
 fun ctx -> function
  | [] -> ()
  | first :: rest ->
      pp_component_prec ~decimals ctx first;
      pp_color_component_tail ~decimals ctx first rest

(* Helpers to pretty print CSS color functions using Pp.call *)
let pp_rgb_args : (channel * channel * channel * alpha) Pp.t =
 fun ctx (r, g, b, alpha) ->
  Pp.list ~sep:Pp.pct_sp pp_channel ctx [ r; g; b ];
  pp_opt_alpha ctx alpha

let pp_rgb_func = Pp.call "rgb" pp_rgb_args

let rec pp_rgb : rgb Pp.t =
 fun ctx -> function
  | Channels { r; g; b } -> Pp.list ~sep:Pp.pct_sp pp_channel ctx [ r; g; b ]
  | Var v -> pp_var pp_rgb ctx v

(** Lab-like float string. CSSOM serialisation (CSSOM 1 sec. 6.7.2) drops a
    leading zero on fractional numbers; the coefficient prints in full so the
    value round-trips, with precision reduction left to [normalize_color]. *)
let string_of_lab_float f =
  (* The printer serialises a lab/oklab coefficient faithfully in both pretty
     and minified output: reducing its precision changes the colour (and can
     cross an sRGB 8-bit boundary), so it is a lossy, node-changing fold that
     belongs in [optimize] (the oklab -> hex collapse via [round_color_axis]),
     not in [pp]. This mirrors [pp_component_float] for the [color()]
     function. *)
  Pp.string_of_float ~drop_leading_zero:true f

let pp_lab_float ctx f = Pp.string ctx (string_of_lab_float f)

let string_of_scaled_color_axis ~pct_scale ctx f =
  let n = string_of_lab_float f in
  (* The number -> percentage axis swap (e.g. oklch chroma [.304] -> [76%]) is a
     shortest-spelling minify win, but [%] on these axes is an evergreen-target
     fact; [--enforce-spec] keeps the spec-canonical number serialisation. *)
  if ctx.Pp.minify && (not ctx.Pp.lossless) && not ctx.Pp.enforce_spec then
    (* Derive the percentage form from what the number string [n] re-parses to,
       not the raw float [f]: an [f] rounding to [n] ([-0.00798] -> "-.008")
       could keep the number for a long raw percentage ("-1.995%") while the
       re-parsed [-0.008] has a short one ("-2%"), flipping on the next pass
       (non-idempotent). *)
    let n_value = try float_of_string n with Failure _ -> f in
    let pct = string_of_lab_float (n_value /. pct_scale) ^ "%" in
    if String.length pct < String.length n then pct else n
  else n

let space_after_color_percentage ctx (l : percentage option) ~next =
  (* The space between the L channel and the next colour component elides when
     the L spelling closes its token cleanly: a [%] ends its percentage token
     whatever follows, and a bare-number end ([4] in [.654]) needs a sign-token
     [+] / [-] to start the next number. Functions and [None] / unknown
     left-hand values stay conservative. *)
  let starts_signed s = String.length s > 0 && (s.[0] = '+' || s.[0] = '-') in
  let elidable =
    Pp.minified ctx
    && (Pp.last_char ctx = Some '%'
       ||
       match (l, next) with
       | Some (Num _), Some s -> starts_signed s
       | _ -> false)
  in
  if not elidable then Pp.space ctx ()

let starts_unsigned_number s =
  String.length s > 0
  && match s.[0] with '0' .. '9' | '.' -> true | _ -> false

(* Pure serialiser: the percentage<->number choice for L is made in the AST
   normalize pass ([canonical_color_lightness]), so this faithfully prints
   whichever node it is given ([Pct] -> [%], [Num] -> bare number). The
   precision is fixed in the AST normalize pass, so this prints in full. *)
let pp_color_lightness ctx (l : percentage option) : percentage option =
  (match l with
  | Some (Pct f) ->
      pp_lab_float ctx f;
      Pp.char ctx '%'
  | Some (Num f) -> pp_lab_float ctx f
  | Some l -> pp_percentage ctx l
  | None -> Pp.string ctx "none");
  l

let pp_pct_chroma_hue_alpha ~chroma_pct_scale :
    (percentage option * float option * hue * alpha) Pp.t =
 fun ctx (l, c, h, alpha) ->
  let printed_l = pp_color_lightness ctx l in
  (match c with
  | Some c ->
      let c = string_of_scaled_color_axis ~pct_scale:chroma_pct_scale ctx c in
      space_after_color_percentage ctx printed_l ~next:(Some c);
      Pp.string ctx c
  | None ->
      space_after_color_percentage ctx printed_l ~next:(Some "none");
      Pp.string ctx "none");
  Pp.pct_sp ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha

let pp_oklch = Pp.call "oklch" (pp_pct_chroma_hue_alpha ~chroma_pct_scale:0.004)

let pp_hue_pct_pct_alpha : (hue * percentage * percentage * alpha) Pp.t =
 fun ctx (h, s, l, a) ->
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx s;
  Pp.pct_sp ctx ();
  pp_percentage ctx l;
  pp_opt_alpha ctx a

let pp_hsl = Pp.call "hsl" pp_hue_pct_pct_alpha
let pp_hwb = Pp.call "hwb" pp_hue_pct_pct_alpha

(** Print a float always dropping leading zeros (for lab-like color axes). Under
    minify the float rounds to 3 decimals - CSS Color 4 alpha precision is 1/255
    ~ 0.004 so 3 decimals is more than display-accurate. *)
let pp_float_drop_zero ctx f =
  let max_decimals = if Pp.minified ctx && not ctx.Pp.lossless then 3 else 8 in
  Pp.float_n max_decimals ctx f

let pp_alpha_drop_zero : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> pp_float_drop_zero ctx f
  | Pct f -> pp_float_drop_zero ctx (f /. 100.)
  | Var v -> pp_var pp_alpha ctx v
  | Calc c -> pp_calc pp_alpha ctx c

let string_of_lab_axis ~pct_scale ctx f =
  string_of_scaled_color_axis ~pct_scale ctx f

let pp_lab_like_args ~axis_pct_scale :
    (percentage option * float option * float option * alpha) Pp.t =
 fun ctx (l, a, b, alpha) ->
  let printed_l = pp_color_lightness ctx l in
  let string_of_axis = function
    | Some f -> string_of_lab_axis ~pct_scale:axis_pct_scale ctx f
    | None -> "none"
  in
  let a = string_of_axis a in
  let b = string_of_axis b in
  space_after_color_percentage ctx printed_l ~next:(Some a);
  Pp.string ctx a;
  (* CSS Color 4 lab / oklab tokens parse the same with or without whitespace
     between an unsigned-number [a] and a sign-prefixed [b] ([.285] then [-.149]
     is two number tokens either way), so under cascade's README shortest-valid
     policy the gap elides when the boundary is unambiguous. *)
  if
    not
      (ctx.Pp.minify && starts_unsigned_number a
      && String.length b > 0
      && (b.[0] = '-' || b.[0] = '+'))
  then Pp.pct_sp ctx ();
  Pp.string ctx b;
  match alpha with
  | None -> ()
  | a when ctx.Pp.minify && alpha_is_full a -> ()
  | a ->
      Pp.op_char ctx '/';
      pp_alpha_drop_zero ctx a

let pp_lab = Pp.call "lab" (pp_lab_like_args ~axis_pct_scale:1.25)
let pp_oklab = Pp.call "oklab" (pp_lab_like_args ~axis_pct_scale:0.004)
let pp_lch = Pp.call "lch" (pp_pct_chroma_hue_alpha ~chroma_pct_scale:1.5)

let pp_color_space : color_space Pp.t =
 fun ctx -> function
  | Srgb -> Pp.string ctx "srgb"
  | Srgb_linear -> Pp.string ctx "srgb-linear"
  | Display_p3 -> Pp.string ctx "display-p3"
  | A98_rgb -> Pp.string ctx "a98-rgb"
  | Prophoto_rgb -> Pp.string ctx "prophoto-rgb"
  | Rec2020 -> Pp.string ctx "rec2020"
  | Lab -> Pp.string ctx "lab"
  | Oklab -> Pp.string ctx "oklab"
  | Xyz -> Pp.string ctx "xyz"
  | Xyz_d50 -> Pp.string ctx "xyz-d50"
  | Xyz_d65 -> Pp.string ctx (if Pp.minified ctx then "xyz" else "xyz-d65")
  | Lch -> Pp.string ctx "lch"
  | Oklch -> Pp.string ctx "oklch"
  | Hsl -> Pp.string ctx "hsl"
  | Hwb -> Pp.string ctx "hwb"

let pp_color' ctx space components alpha =
  (* The AST carries the canonical precision ([round_modern_color_axes]); the
     printer serialises it faithfully rather than rounding again, so a refused
     fold keeps the digits that make its verdict repeat. *)
  let decimals = 8 in
  Pp.call "color"
    (fun ctx (space, components, alpha) ->
      pp_color_space ctx space;
      (match components with
      | [] -> ()
      | _ ->
          Pp.space ctx ();
          pp_color_components ~decimals ctx components);
      pp_opt_alpha ctx alpha)
    ctx (space, components, alpha)

let pp_hex_color ctx r g b a =
  (* Pure serialiser: pick the shortest spelling of the decoded components.
     Minify collapses [#rrggbb] -> [#rgb] and drops an opaque alpha; pretty
     keeps the full byte form. The cross-node folds (hex -> named) are AST
     rewrites done by [normalize_color]. *)
  Pp.char ctx '#';
  if Pp.minified ctx then Pp.string ctx (hex_string_of_bytes r g b a)
  else (
    Pp.string ctx (hex_of_byte r);
    Pp.string ctx (hex_of_byte g);
    Pp.string ctx (hex_of_byte b);
    if a <> 255 then Pp.string ctx (hex_of_byte a))

let rec pp_rgb_as_color : rgb Pp.t =
 fun ctx -> function
  | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
  | Var v -> pp_var pp_rgb_as_color ctx v

let canonical_color_name = function
  | Grey -> Gray
  | Dark_grey -> Dark_gray
  | Light_grey -> Light_gray
  | Slate_grey -> Slate_gray
  | Dark_slate_grey -> Dark_slate_gray
  | Light_slate_grey -> Light_slate_gray
  | Dim_grey -> Dim_gray
  | other -> other

let pp_transparent_color ctx = Pp.string ctx "transparent"

let color_of_default_string value =
  let value = String.trim value in
  let len = String.length value in
  let is_hex c =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
  in
  if len > 1 && value.[0] = '#' then
    let hex = String.sub value 1 (len - 1) in
    let hex_len = String.length hex in
    if
      (hex_len = 3 || hex_len = 4 || hex_len = 6 || hex_len = 8)
      && String.for_all is_hex hex
    then
      match rgba_of_hex hex with
      | Some (r, g, b, a) -> Option.Some (Hex { r; g; b; a })
      | None -> Option.None
    else Option.None
  else if
    len > 5 && String.starts_with ~prefix:"rgb(" value && value.[len - 1] = ')'
  then
    let body = String.sub value 4 (len - 5) in
    let body = String.map (function ',' -> ' ' | c -> c) body in
    let parts =
      body |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
    in
    match List.map int_of_string_opt parts with
    | [ Option.Some r; Option.Some g; Option.Some b ]
      when List.for_all (fun n -> 0 <= n && n <= 255) [ r; g; b ] ->
        Option.Some (Rgb (Channels { r = Int r; g = Int g; b = Int b }))
    | _ -> Option.None
  else Option.None

let color_of_var_resolution ctx (v : color var) =
  value_of_var_resolution ctx color_of_default_string v

let rec pp_color_in_mix : color Pp.t =
 fun ctx -> function
  | Current -> Pp.string ctx "currentcolor" (* lowercase in color-mix *)
  | c -> pp_color ctx c

and pp_color_mix ctx in_space hue color1 percent1 color2 percent2 =
  (* Pure serialiser: the interpolation method, hue strategy, and percentages
     print exactly as held. Dropping the default [in oklab] header, the default
     [shorter] hue, and the redundant percentages are AST rewrites done by
     [normalize_color]. *)
  Pp.call "color-mix"
    (fun ctx (in_space, hue, color1, percent1, color2, percent2) ->
      (match in_space with
      | Some space ->
          Pp.string ctx "in ";
          pp_color_space ctx space;
          (match hue with
          | Default -> ()
          | _ ->
              Pp.space ctx ();
              pp_hue_interpolation ctx hue;
              Pp.string ctx " hue");
          Pp.comma ctx ()
      | None -> ());
      pp_color_in_mix ctx color1;
      (match percent1 with
      | Some p ->
          Pp.space ctx ();
          pp_percentage ctx p
      | None -> ());
      Pp.comma ctx ();
      pp_color_in_mix ctx color2;
      match percent2 with
      | Some p ->
          Pp.space ctx ();
          pp_percentage ctx p
      | None -> ())
    ctx
    (in_space, hue, color1, percent1, color2, percent2)

and pp_typed_relative_color_call ctx name origin tail =
  let tail =
    if Pp.minified ctx then
      tail |> minify_relative_color_alpha |> minify_relative_color_numbers
      |> minify_relative_color_spaces
    else tail
  in
  Pp.call name
    (fun ctx (origin, tail) ->
      Pp.string ctx "from";
      Pp.space ctx ();
      pp_color ctx origin;
      Pp.space ctx ();
      Pp.string ctx tail)
    ctx (origin, tail)

and pp_rgb_color ctx = function
  | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
  (* keep the [rgb()] wrapper: a bare [var(--x)] assumes [--x] is a whole
     colour, whereas [rgb(var(--x))] takes [--x] as the channel list - dropping
     the wrapper changes the value (and renders black instead of the colour).
     The var prints with [pp_rgb] so a fallback stays bare channels, not a
     nested [rgb()]. *)
  | Var v -> Pp.call "rgb" pp_rgb ctx (Var v)

and pp_rgba_color ctx rgb a =
  match rgb with
  | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, a)
  | Var v ->
      Pp.call "rgb"
        (fun ctx (v, a) ->
          pp_rgb_as_color ctx (Var v);
          pp_opt_alpha ctx a)
        ctx (v, a)

and pp_hsl_color ctx h s l a = pp_hsl ctx (h, s, l, a)
and pp_hwb_color ctx h w b a = pp_hwb ctx (h, w, b, a)

and pp_light_dark_color ctx light dark =
  Pp.call "light-dark"
    (fun ctx (light, dark) ->
      pp_color ctx light;
      Pp.comma ctx ();
      pp_color ctx dark)
    ctx (light, dark)

and pp_color_attr ctx name fallback =
  Pp.string ctx "attr(";
  Pp.string ctx name;
  Pp.space ctx ();
  Pp.string ctx "type(<color>)";
  Option.iter
    (fun fallback ->
      Pp.comma ctx ();
      pp_color ctx fallback)
    fallback;
  Pp.char ctx ')'

and pp_color_var (ctx : Pp.ctx) (v : color var) =
  match v with
  | { fallback = Syntax_fallback value; default = Option.None; _ }
    when ctx.inline ->
      let rendered = string_of_syntax_fallback ctx value in
      Pp.string ctx (first_top_level_comma_segment rendered)
  | _ when Pp.minified ctx -> (
      match color_of_var_resolution ctx v with
      | Option.Some color -> pp_color ctx color
      | Option.None -> pp_var pp_color ctx v)
  | _ -> pp_var pp_color ctx v

and pp_color : color Pp.t = fun ctx color -> pp_color_default ctx color

and pp_color_default : color Pp.t =
 fun ctx -> function
  | Hex { r; g; b; a } -> pp_hex_color ctx r g b a
  | Authored_hex { value; r; g; b; a } ->
      if Pp.minified ctx then pp_hex_color ctx r g b a
      else (
        Pp.char ctx '#';
        Pp.string ctx value)
  | Rgb rgb -> pp_rgb_color ctx rgb
  | Rgba { rgb; a } -> pp_rgba_color ctx rgb a
  | Hsl { h; s; l; a } -> pp_hsl_color ctx h s l a
  | Hwb { h; w; b; a } -> pp_hwb_color ctx h w b a
  | Color { space; components; alpha } -> pp_color' ctx space components alpha
  | Relative_rgb (origin, tail) ->
      pp_typed_relative_color_call ctx "rgb" origin tail
  | Relative_color (name, origin, tail) ->
      pp_typed_relative_color_call ctx name origin tail
  | Contrast_color color -> Pp.call "contrast-color" pp_color ctx color
  | Light_dark (light, dark) -> pp_light_dark_color ctx light dark
  | Attribute (name, fallback) -> pp_color_attr ctx name fallback
  | Lab { l; a; b; alpha } -> pp_lab ctx (l, a, b, alpha)
  | Oklch { l; c; h; alpha } -> pp_oklch ctx (l, c, h, alpha)
  | Oklab { l; a; b; alpha } -> pp_oklab ctx (l, a, b, alpha)
  | Lch { l; c; h; alpha } -> pp_lch ctx (l, c, h, alpha)
  | Named name -> pp_color_name ctx name
  | System sc -> pp_system_color ctx sc
  | Var v -> pp_color_var ctx v
  | Current ->
      Pp.string ctx (if ctx.in_function then "currentcolor" else "currentColor")
  | Transparent -> pp_transparent_color ctx
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space hue color1 percent1 color2 percent2

let pp_specified_color = pp_color_default
