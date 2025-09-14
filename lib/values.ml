(** CSS Values & Units parsing using Reader API *)

include Values_intf

let var_ref ?fallback ?default ?layer ?meta name =
  let fallback : _ fallback =
    match fallback with None -> None | Some x -> x
  in
  { name; fallback; default; layer; meta }

let var_ref_empty ?default ?layer ?meta name =
  { name; fallback = Empty; default; layer; meta }

(** Color constructors *)
let hex s =
  let len = String.length s in
  if len > 0 && s.[0] = '#' then
    Hex { hash = true; value = String.sub s 1 (len - 1) }
  else Hex { hash = false; value = s }

let rgb r g b = Rgb { r = Int r; g = Int g; b = Int b }
let rgba r g b a = Rgba { r = Int r; g = Int g; b = Int b; a = Num a }
let hsl h s l = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = None }
let hsla h s l a = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = Num a }
let hwb h w b = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = None }
let hwba h w b a = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = Num a }
let oklch l c h = Oklch { l = Pct l; c; h = Unitless h; alpha = None }
let oklcha l c h a = Oklch { l = Pct l; c; h = Unitless h; alpha = Num a }
let oklab l a b = Oklab { l = Pct l; a; b; alpha = None }
let oklaba l a b alpha = Oklab { l = Pct l; a; b; alpha = Num alpha }
let lch l c h = Lch { l = Pct l; c; h = Unitless h; alpha = None }
let lcha l c h a = Lch { l = Pct l; c; h = Unitless h; alpha = Num a }
let color_name n = Named n
let current_color = Current
let transparent = Transparent

let color_mix ?in_space ?(hue = Default) ?percent1 ?percent2 color1 color2 =
  let percent1 : percentage option =
    match percent1 with Some p -> Some (Pct (float_of_int p)) | None -> None
  in
  let percent2 : percentage option =
    match percent2 with Some p -> Some (Pct (float_of_int p)) | None -> None
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 }

(** Pretty-printing functions *)

let pp_op ctx = function
  | Add ->
      (* CSS spec requires spaces around + and - in calc() *)
      Pp.space ctx ();
      Pp.char ctx '+';
      Pp.space ctx ()
  | Sub ->
      (* CSS spec requires spaces around + and - in calc() *)
      Pp.space ctx ();
      Pp.char ctx '-';
      Pp.space ctx ()
  | Mul ->
      Pp.op_char ctx '*' (* CSS spec: spaces optional, omit when minified *)
  | Div ->
      Pp.op_char ctx '/' (* CSS spec: spaces optional, omit when minified *)

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  (* When inlining is enabled, output the default value if available *)
  if ctx.inline && v.default <> Option.None then
    match v.default with
    | Some value -> pp_value ctx value
    | Option.None -> assert false (* unreachable due to condition above *)
  else (
    (* Standard var() reference output *)
    Pp.string ctx "var(--";
    Pp.string ctx v.name;
    match v.fallback with
    | None -> Pp.char ctx ')'
    | Empty ->
        Pp.char ctx ',';
        Pp.char ctx ')'
    | Fallback value ->
        Pp.char ctx ',';
        Pp.space_if_pretty ctx ();
        pp_value ctx value;
        Pp.char ctx ')')

(* Function call formatting now provided by Pp.call and Pp.call_list *)

let pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  Pp.call "calc"
    (fun ctx calc ->
      let precedence = function Add | Sub -> 1 | Mul | Div -> 2 in
      let rec pp_calc_inner ~parent_prec ctx = function
        | Val v -> pp_value ctx v
        | Var v -> pp_var pp_value ctx v
        | Num n -> Pp.float ctx n
        | Expr (left, op, right) ->
            let op_prec = precedence op in
            let needs_parens = op_prec < parent_prec in
            if needs_parens then Pp.char ctx '(';
            pp_calc_inner ~parent_prec:op_prec ctx left;
            pp_op ctx op;
            pp_calc_inner ~parent_prec:op_prec ctx right;
            if needs_parens then Pp.char ctx ')'
      in
      pp_calc_inner ~parent_prec:0 ctx calc)
    ctx calc

(* Small helpers *)
let pp_unit ?(always = true) ctx f suffix =
  if f = 0. && not always then Pp.char ctx '0'
  else (
    Pp.float ctx f;
    Pp.string ctx suffix)

let rec pp_length : length Pp.t =
 fun ctx v ->
  let pp_unit = pp_unit ~always:false ctx in
  match v with
  | Zero -> Pp.char ctx '0'
  | Px f -> pp_unit f "px"
  | Cm f -> pp_unit f "cm"
  | Mm f -> pp_unit f "mm"
  | Q f -> pp_unit f "q"
  | In f -> pp_unit f "in"
  | Pt f -> pp_unit f "pt"
  | Pc f -> pp_unit f "pc"
  | Rem f -> pp_unit f "rem"
  | Em f -> pp_unit f "em"
  | Ex f -> pp_unit f "ex"
  | Cap f -> pp_unit f "cap"
  | Ic f -> pp_unit f "ic"
  | Rlh f -> pp_unit f "rlh"
  | Pct f -> pp_unit f "%"
  | Vw f -> pp_unit f "vw"
  | Vh f -> pp_unit f "vh"
  | Vmin f -> pp_unit f "vmin"
  | Vmax f -> pp_unit f "vmax"
  | Vi f -> pp_unit f "vi"
  | Vb f -> pp_unit f "vb"
  | Dvh f -> pp_unit f "dvh"
  | Dvw f -> pp_unit f "dvw"
  | Dvmin f -> pp_unit f "dvmin"
  | Dvmax f -> pp_unit f "dvmax"
  | Lvh f -> pp_unit f "lvh"
  | Lvw f -> pp_unit f "lvw"
  | Lvmin f -> pp_unit f "lvmin"
  | Lvmax f -> pp_unit f "lvmax"
  | Svh f -> pp_unit f "svh"
  | Svw f -> pp_unit f "svw"
  | Svmin f -> pp_unit f "svmin"
  | Svmax f -> pp_unit f "svmax"
  | Ch f -> pp_unit f "ch"
  | Lh f -> pp_unit f "lh"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Fit_content -> Pp.string ctx "fit-content"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | From_font -> Pp.string ctx "from-font"
  | Var v -> pp_var pp_length ctx v
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Content -> Pp.string ctx "content"
  | Calc cv -> (
      (* Optimize calc(infinity * dimension) to large value for minification *)
      match cv with
      | Expr (Num f, Mul, Val _) when ctx.minify && f = infinity ->
          Pp.string ctx "3.40282e38px"
      | Expr (Val _, Mul, Num f) when ctx.minify && f = infinity ->
          Pp.string ctx "3.40282e38px"
      | _ -> pp_calc pp_length ctx cv)

let pp_color_name : color_name Pp.t =
 fun ctx -> function
  | Red -> Pp.string ctx "red"
  | Blue -> Pp.string ctx "blue"
  | Green -> Pp.string ctx "green"
  | White -> Pp.string ctx "white"
  | Black -> Pp.string ctx "black"
  | Yellow -> Pp.string ctx "yellow"
  | Cyan -> Pp.string ctx "cyan"
  | Magenta -> Pp.string ctx "magenta"
  | Gray -> Pp.string ctx "gray"
  | Grey -> Pp.string ctx "grey"
  | Orange -> Pp.string ctx "orange"
  | Purple -> Pp.string ctx "purple"
  | Pink -> Pp.string ctx "pink"
  | Silver -> Pp.string ctx "silver"
  | Maroon -> Pp.string ctx "maroon"
  | Fuchsia -> Pp.string ctx "fuchsia"
  | Lime -> Pp.string ctx "lime"
  | Olive -> Pp.string ctx "olive"
  | Navy -> Pp.string ctx "navy"
  | Teal -> Pp.string ctx "teal"
  | Aqua -> Pp.string ctx "aqua"
  | Alice_blue -> Pp.string ctx "aliceblue"
  | Antique_white -> Pp.string ctx "antiquewhite"
  | Aquamarine -> Pp.string ctx "aquamarine"
  | Azure -> Pp.string ctx "azure"
  | Beige -> Pp.string ctx "beige"
  | Bisque -> Pp.string ctx "bisque"
  | Blanched_almond -> Pp.string ctx "blanchedalmond"
  | Blue_violet -> Pp.string ctx "blueviolet"
  | Brown -> Pp.string ctx "brown"
  | Burlywood -> Pp.string ctx "burlywood"
  | Cadet_blue -> Pp.string ctx "cadetblue"
  | Chartreuse -> Pp.string ctx "chartreuse"
  | Chocolate -> Pp.string ctx "chocolate"
  | Coral -> Pp.string ctx "coral"
  | Cornflower_blue -> Pp.string ctx "cornflowerblue"
  | Cornsilk -> Pp.string ctx "cornsilk"
  | Crimson -> Pp.string ctx "crimson"
  | Dark_blue -> Pp.string ctx "darkblue"
  | Dark_cyan -> Pp.string ctx "darkcyan"
  | Dark_goldenrod -> Pp.string ctx "darkgoldenrod"
  | Dark_gray -> Pp.string ctx "darkgray"
  | Dark_green -> Pp.string ctx "darkgreen"
  | Dark_grey -> Pp.string ctx "darkgrey"
  | Dark_khaki -> Pp.string ctx "darkkhaki"
  | Dark_magenta -> Pp.string ctx "darkmagenta"
  | Dark_olive_green -> Pp.string ctx "darkolivegreen"
  | Dark_orange -> Pp.string ctx "darkorange"
  | Dark_orchid -> Pp.string ctx "darkorchid"
  | Dark_red -> Pp.string ctx "darkred"
  | Dark_salmon -> Pp.string ctx "darksalmon"
  | Dark_sea_green -> Pp.string ctx "darkseagreen"
  | Dark_slate_blue -> Pp.string ctx "darkslateblue"
  | Dark_slate_gray -> Pp.string ctx "darkslategray"
  | Dark_slate_grey -> Pp.string ctx "darkslategrey"
  | Dark_turquoise -> Pp.string ctx "darkturquoise"
  | Dark_violet -> Pp.string ctx "darkviolet"
  | Deep_pink -> Pp.string ctx "deeppink"
  | Deep_sky_blue -> Pp.string ctx "deepskyblue"
  | Dim_gray -> Pp.string ctx "dimgray"
  | Dim_grey -> Pp.string ctx "dimgrey"
  | Dodger_blue -> Pp.string ctx "dodgerblue"
  | Firebrick -> Pp.string ctx "firebrick"
  | Floral_white -> Pp.string ctx "floralwhite"
  | Forest_green -> Pp.string ctx "forestgreen"
  | Gainsboro -> Pp.string ctx "gainsboro"
  | Ghost_white -> Pp.string ctx "ghostwhite"
  | Gold -> Pp.string ctx "gold"
  | Goldenrod -> Pp.string ctx "goldenrod"
  | Green_yellow -> Pp.string ctx "greenyellow"
  | Honeydew -> Pp.string ctx "honeydew"
  | Hot_pink -> Pp.string ctx "hotpink"
  | Indian_red -> Pp.string ctx "indianred"
  | Indigo -> Pp.string ctx "indigo"
  | Ivory -> Pp.string ctx "ivory"
  | Khaki -> Pp.string ctx "khaki"
  | Lavender -> Pp.string ctx "lavender"
  | Lavender_blush -> Pp.string ctx "lavenderblush"
  | Lawn_green -> Pp.string ctx "lawngreen"
  | Lemon_chiffon -> Pp.string ctx "lemonchiffon"
  | Light_blue -> Pp.string ctx "lightblue"
  | Light_coral -> Pp.string ctx "lightcoral"
  | Light_cyan -> Pp.string ctx "lightcyan"
  | Light_goldenrod_yellow -> Pp.string ctx "lightgoldenrodyellow"
  | Light_gray -> Pp.string ctx "lightgray"
  | Light_green -> Pp.string ctx "lightgreen"
  | Light_grey -> Pp.string ctx "lightgrey"
  | Light_pink -> Pp.string ctx "lightpink"
  | Light_salmon -> Pp.string ctx "lightsalmon"
  | Light_sea_green -> Pp.string ctx "lightseagreen"
  | Light_sky_blue -> Pp.string ctx "lightskyblue"
  | Light_slate_gray -> Pp.string ctx "lightslategray"
  | Light_slate_grey -> Pp.string ctx "lightslategrey"
  | Light_steel_blue -> Pp.string ctx "lightsteelblue"
  | Light_yellow -> Pp.string ctx "lightyellow"
  | Lime_green -> Pp.string ctx "limegreen"
  | Linen -> Pp.string ctx "linen"
  | Medium_aquamarine -> Pp.string ctx "mediumaquamarine"
  | Medium_blue -> Pp.string ctx "mediumblue"
  | Medium_orchid -> Pp.string ctx "mediumorchid"
  | Medium_purple -> Pp.string ctx "mediumpurple"
  | Medium_sea_green -> Pp.string ctx "mediumseagreen"
  | Medium_slate_blue -> Pp.string ctx "mediumslateblue"
  | Medium_spring_green -> Pp.string ctx "mediumspringgreen"
  | Medium_turquoise -> Pp.string ctx "mediumturquoise"
  | Medium_violet_red -> Pp.string ctx "mediumvioletred"
  | Midnight_blue -> Pp.string ctx "midnightblue"
  | Mint_cream -> Pp.string ctx "mintcream"
  | Misty_rose -> Pp.string ctx "mistyrose"
  | Moccasin -> Pp.string ctx "moccasin"
  | Navajo_white -> Pp.string ctx "navajowhite"
  | Old_lace -> Pp.string ctx "oldlace"
  | Olive_drab -> Pp.string ctx "olivedrab"
  | Orange_red -> Pp.string ctx "orangered"
  | Orchid -> Pp.string ctx "orchid"
  | Pale_goldenrod -> Pp.string ctx "palegoldenrod"
  | Pale_green -> Pp.string ctx "palegreen"
  | Pale_turquoise -> Pp.string ctx "paleturquoise"
  | Pale_violet_red -> Pp.string ctx "palevioletred"
  | Papaya_whip -> Pp.string ctx "papayawhip"
  | Peach_puff -> Pp.string ctx "peachpuff"
  | Peru -> Pp.string ctx "peru"
  | Plum -> Pp.string ctx "plum"
  | Powder_blue -> Pp.string ctx "powderblue"
  | Rebecca_purple -> Pp.string ctx "rebeccapurple"
  | Rosy_brown -> Pp.string ctx "rosybrown"
  | Royal_blue -> Pp.string ctx "royalblue"
  | Saddle_brown -> Pp.string ctx "saddlebrown"
  | Salmon -> Pp.string ctx "salmon"
  | Sandy_brown -> Pp.string ctx "sandybrown"
  | Sea_green -> Pp.string ctx "seagreen"
  | Sea_shell -> Pp.string ctx "seashell"
  | Sienna -> Pp.string ctx "sienna"
  | Sky_blue -> Pp.string ctx "skyblue"
  | Slate_blue -> Pp.string ctx "slateblue"
  | Slate_gray -> Pp.string ctx "slategray"
  | Slate_grey -> Pp.string ctx "slategrey"
  | Snow -> Pp.string ctx "snow"
  | Spring_green -> Pp.string ctx "springgreen"
  | Steel_blue -> Pp.string ctx "steelblue"
  | Tan -> Pp.string ctx "tan"
  | Thistle -> Pp.string ctx "thistle"
  | Tomato -> Pp.string ctx "tomato"
  | Turquoise -> Pp.string ctx "turquoise"
  | Violet -> Pp.string ctx "violet"
  | Wheat -> Pp.string ctx "wheat"
  | White_smoke -> Pp.string ctx "whitesmoke"
  | Yellow_green -> Pp.string ctx "yellowgreen"

let rec pp_channel : channel Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_channel ctx v

let rec pp_angle : angle Pp.t =
 fun ctx -> function
  | Deg f -> pp_unit ctx f "deg"
  | Rad f -> pp_unit ctx f "rad"
  | Turn f -> pp_unit ctx f "turn"
  | Grad f -> pp_unit ctx f "grad"
  | Var v -> pp_var pp_angle ctx v

let rec pp_hue : hue Pp.t =
 fun ctx -> function
  | Unitless f -> Pp.float ctx f
  | Angle (Deg f) when ctx.minify ->
      (* During minification, omit 'deg' since it's the default unit *)
      Pp.float ctx f
  | Angle a -> pp_angle ctx a
  | Var v -> pp_var pp_hue ctx v

and pp_alpha : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> Pp.float ctx f
  | Pct f ->
      if ctx.minify then
        (* During minification, convert percentage to decimal [0,1] *)
        Pp.float ctx (f /. 100.0)
      else (
        Pp.float ctx f;
        Pp.char ctx '%')
  | Var v -> pp_var pp_alpha ctx v

(* Helper to print optional alpha with the correct leading separator *)
let pp_opt_alpha ctx = function
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.op_char ctx '/';
      pp_alpha ctx a

(** Pretty printer for percentage types *)
let rec pp_percentage : percentage Pp.t =
 fun ctx -> function
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_percentage ctx v
  | Calc c -> pp_calc pp_percentage ctx c

and pp_length_percentage : length_percentage Pp.t =
 fun ctx -> function
  | Length l -> pp_length ctx l
  | Percentage p -> pp_percentage ctx p
  | Var v -> pp_var pp_length_percentage ctx v
  | Calc c -> pp_calc pp_length_percentage ctx c

and pp_component : component Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Angle h -> pp_hue ctx h
  | Var v -> pp_var pp_component ctx v
  | Calc c -> pp_calc pp_component ctx c

and pp_hue_interpolation : hue_interpolation Pp.t =
 fun ctx -> function
  | Shorter -> Pp.string ctx "shorter"
  | Longer -> Pp.string ctx "longer"
  | Increasing -> Pp.string ctx "increasing"
  | Decreasing -> Pp.string ctx "decreasing"
  | Default -> ()

(* Helpers to pretty print CSS color functions using Pp.call *)
let pp_rgb_args : (channel * channel * channel * alpha) Pp.t =
 fun ctx (r, g, b, alpha) ->
  Pp.list ~sep:Pp.space pp_channel ctx [ r; g; b ];
  pp_opt_alpha ctx alpha

let pp_rgb = Pp.call "rgb" pp_rgb_args

let pp_oklch_args : (percentage * float * hue * alpha) Pp.t =
 fun ctx (l, c, h, alpha) ->
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha

let pp_oklch = Pp.call "oklch" pp_oklch_args

let pp_hsl_args : (hue * percentage * percentage * alpha) Pp.t =
 fun ctx (h, s, l, a) ->
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx s;
  Pp.space ctx ();
  pp_percentage ctx l;
  pp_opt_alpha ctx a

let pp_hsl = Pp.call "hsl" pp_hsl_args

let pp_hwb_args : (hue * percentage * percentage * alpha) Pp.t =
 fun ctx (h, w, b, a) ->
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx w;
  Pp.space ctx ();
  pp_percentage ctx b;
  pp_opt_alpha ctx a

let pp_hwb = Pp.call "hwb" pp_hwb_args

let pp_oklab_args : (percentage * float * float * alpha) Pp.t =
 fun ctx (l, a, b, alpha) ->
  (* Oklab L must always be output as percentage per CSS spec *)
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx a;
  Pp.space ctx ();
  Pp.float ctx b;
  pp_opt_alpha ctx alpha

let pp_oklab = Pp.call "oklab" pp_oklab_args

let pp_lch_args : (percentage * float * hue * alpha) Pp.t =
 fun ctx (l, c, h, alpha) ->
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha

let pp_lch = Pp.call "lch" pp_lch_args

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
  | Xyz_d65 -> Pp.string ctx "xyz-d65"
  | Lch -> Pp.string ctx "lch"
  | Oklch -> Pp.string ctx "oklch"
  | Hsl -> Pp.string ctx "hsl"
  | Hwb -> Pp.string ctx "hwb"

let rec pp_color_in_mix : color Pp.t =
 fun ctx -> function
  | Current -> Pp.string ctx "currentcolor" (* lowercase in color-mix *)
  | c -> pp_color ctx c

and pp_color_mix ctx in_space hue color1 percent1 color2 percent2 =
  Pp.call "color-mix"
    (fun ctx (in_space, hue, color1, percent1, color2, percent2) ->
      (match in_space with
      | Some space ->
          Pp.string ctx "in ";
          pp_color_space ctx space
      | None -> Pp.string ctx "in oklab");
      (match hue with
      | Default -> ()
      | _ ->
          Pp.space ctx ();
          pp_hue_interpolation ctx hue;
          Pp.string ctx " hue");
      Pp.comma ctx ();
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

and pp_color' ctx space components alpha =
  Pp.call "color"
    (fun ctx (space, components, alpha) ->
      pp_color_space ctx space;
      (match components with
      | [] -> ()
      | _ ->
          Pp.space ctx ();
          Pp.list ~sep:Pp.space pp_component ctx components);
      pp_opt_alpha ctx alpha)
    ctx (space, components, alpha)

and pp_color : color Pp.t =
 fun ctx -> function
  | Hex { hash = _; value } ->
      Pp.char ctx '#';
      Pp.string ctx value
  | Rgb { r; g; b } -> pp_rgb ctx (r, g, b, None)
  | Rgba { r; g; b; a } -> pp_rgb ctx (r, g, b, a)
  | Hsl { h; s; l; a } -> pp_hsl ctx (h, s, l, a)
  | Hwb { h; w; b; a } -> pp_hwb ctx (h, w, b, a)
  | Color { space; components; alpha } -> pp_color' ctx space components alpha
  | Oklch { l; c; h; alpha } -> pp_oklch ctx (l, c, h, alpha)
  | Oklab { l; a; b; alpha } -> pp_oklab ctx (l, a, b, alpha)
  | Lch { l; c; h; alpha } -> pp_lch ctx (l, c, h, alpha)
  | Named name -> pp_color_name ctx name
  | Var v -> pp_var pp_color ctx v
  | Current -> Pp.string ctx "currentColor"
  | Transparent -> Pp.string ctx "transparent"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space hue color1 percent1 color2 percent2

let rec pp_duration : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_unit ctx f "ms"
  | S f -> pp_unit ctx f "s"
  | Var v -> pp_var pp_duration ctx v

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Float f -> Pp.float ctx f
  | Int i -> Pp.int ctx i
  | Pct p ->
      Pp.float ctx p;
      Pp.char ctx '%'
  | Var v -> pp_var pp_number ctx v

(* Print a raw float as a percentage value *)

(* Calc module for building calc() expressions *)
module Calc = struct
  let add left right = Expr (left, Add, right)
  let sub left right = Expr (left, Sub, right)
  let mul left right = Expr (left, Mul, right)
  let div left right = Expr (left, Div, right)

  (* Operators *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  (* Value constructors *)
  let length len = Val len

  let var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc =
   fun ?default ?fallback name -> Var (var_ref ?default ?fallback name)

  let float f : length calc = Num f
  let infinity : length calc = Num infinity
  let px n = Val (Px n)
  let rem f = Val (Rem f)
  let em f = Val (Em f)
  let pct f : length calc = Val (Pct f)
end

(** var() parser after "var" ident has been consumed *)
let read_var_after_ident : type a. (Reader.t -> a) -> Reader.t -> a var =
 fun read_value t ->
  Reader.expect '(' t;
  Reader.ws t;
  let var_name =
    if Reader.looking_at t "--" then (
      Reader.expect_string "--" t;
      Reader.ident ~keep_case:true t)
    else Reader.ident ~keep_case:true t
  in
  Reader.ws t;
  let fallback : _ fallback =
    if Reader.peek t = Some ',' then (
      Reader.comma t;
      Reader.ws t;
      (* Check if we have an empty fallback (nothing before ')') *)
      if Reader.peek t = Some ')' then Empty
      else
        (* For fallback, we need to capture everything until the closing paren,
           respecting nested parens and quotes. Then parse that as the value. *)
        let fallback_str = Reader.css_value ~stops:[ ')' ] t in
        let fallback_reader = Reader.of_string fallback_str in
        Fallback (read_value fallback_reader))
    else None
  in
  Reader.ws t;
  Reader.expect ')' t;
  var_ref ~fallback var_name

(** Generic var() parser that returns a var reference. This works when called
    from enum_calls/enum_or_calls context where "var" has been consumed and
    we're inside parens *)
let read_var : type a. (Reader.t -> a) -> Reader.t -> a var =
 fun read_value t ->
  Reader.ws t;
  Reader.expect_string "var" t;
  Reader.expect '(' t;
  Reader.ws t;
  let var_name =
    if Reader.looking_at t "--" then (
      Reader.expect_string "--" t;
      Reader.ident ~keep_case:true t)
    else Reader.ident ~keep_case:true t
  in
  Reader.ws t;
  let fallback : _ fallback =
    if Reader.peek t = Some ',' then (
      Reader.comma t;
      Reader.ws t;
      (* Check if we have an empty fallback (nothing before ')') *)
      if Reader.peek t = Some ')' then Empty
      else
        (* For fallback, we need to capture everything until the closing paren,
           respecting nested parens and quotes. Then parse that as the value. *)
        let fallback_str = Reader.css_value ~stops:[ ')' ] t in
        let fallback_reader = Reader.of_string fallback_str in
        Fallback (read_value fallback_reader))
    else None
  in
  Reader.ws t;
  Reader.expect ')' t;
  var_ref ~fallback var_name

let read_length_unit t =
  let n = Reader.number t in
  let unit_raw = Reader.while_ t (fun c -> Reader.is_alpha c || c = '%') in
  let unit = String.lowercase_ascii unit_raw in
  match unit with
  | "" when n = 0.0 -> Zero
  | "" -> Reader.err t "length values must have units (except for zero)"
  | _ when n = 0.0 -> Zero
  | "px" -> Px n
  | "cm" -> Cm n
  | "mm" -> Mm n
  | "q" -> Q n
  | "in" -> In n
  | "pt" -> Pt n
  | "pc" -> Pc n
  | "em" -> Em n
  | "rem" -> Rem n
  | "ex" -> Ex n
  | "cap" -> Cap n
  | "ic" -> Ic n
  | "rlh" -> Rlh n
  | "vh" -> Vh n
  | "vw" -> Vw n
  | "vmin" -> Vmin n
  | "vmax" -> Vmax n
  | "vi" -> Vi n
  | "vb" -> Vb n
  | "dvh" -> Dvh n
  | "dvw" -> Dvw n
  | "dvmin" -> Dvmin n
  | "dvmax" -> Dvmax n
  | "lvh" -> Lvh n
  | "lvw" -> Lvw n
  | "lvmin" -> Lvmin n
  | "lvmax" -> Lvmax n
  | "svh" -> Svh n
  | "svw" -> Svw n
  | "svmin" -> Svmin n
  | "svmax" -> Svmax n
  | "ch" -> Ch n
  | "lh" -> Lh n
  | "%" -> Pct n
  | _ -> Reader.err_invalid t ("length unit: " ^ unit)

let read_length_keyword =
  Reader.enum "length"
    [
      ("auto", Auto);
      ("max-content", Max_content);
      ("min-content", Min_content);
      ("fit-content", Fit_content);
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]

let rec read_calc_expr : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.ws t;
  let left = read_calc_term read_a t in
  Reader.ws t;
  match Reader.peek t with
  | Some '+' ->
      Reader.skip t;
      Expr (left, Add, read_calc_expr read_a t)
  | Some '-' ->
      Reader.skip t;
      Expr (left, Sub, read_calc_expr read_a t)
  | _ -> left

and read_calc_term : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.ws t;
  let left = read_calc_factor read_a t in
  Reader.ws t;
  match Reader.peek t with
  | Some '*' ->
      Reader.skip t;
      let right = read_calc_term read_a t in
      (* Validate multiplication: can't multiply two raw dimensions (but
         expressions are OK) *)
      let is_dimension : type a. a calc -> bool = function
        | Val _ -> true
        | _ -> false
      in
      if is_dimension left && is_dimension right then
        Reader.err t "invalid calc: cannot multiply two dimensions";
      Expr (left, Mul, right)
  | Some '/' ->
      Reader.skip t;
      let right = read_calc_term read_a t in
      (* Validate division: right operand must be a number (not a dimension) *)
      let is_not_number : type a. a calc -> bool = function
        | Val _ -> true (* definitely not a number *)
        | Num _ -> false (* is a number *)
        | _ -> false (* expressions could evaluate to numbers *)
      in
      if is_not_number right then
        Reader.err t "invalid calc: division requires a number on the right";
      Expr (left, Div, right)
  | _ -> left

and read_calc_parenthesized : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.skip t;
  let expr = read_calc_expr read_a t in
  Reader.ws t;
  Reader.expect ')' t;
  expr

and read_calc_zero : type a. Reader.t -> a calc =
 fun t ->
  let n = Reader.number t in
  if n <> 0. then Reader.err t "expected zero"
  else
    match Reader.peek t with
    | Some c when Reader.is_alpha c || c = '%' -> Reader.err t "zero with unit"
    | _ -> Num 0.

and read_calc_factor : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.ws t;
  match Reader.peek t with
  | Some '(' -> read_calc_parenthesized read_a t
  | _ when Reader.looking_at t "var(" -> Var (read_var read_a t)
  | _ ->
      let read_val t = Val (read_a t) in
      let read_num t : a calc = Num (Reader.number t) in
      Reader.one_of [ read_calc_zero; read_val; read_num ] t

and read_calc : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.ws t;
  if Reader.looking_at t "calc(" then (
    Reader.expect_string "calc(" t;
    let expr = read_calc_expr read_a t in
    Reader.expect ')' t;
    expr)
  else if Reader.looking_at t "var(" then (
    Reader.expect_string "var(" t;
    Reader.ws t;
    let var_name =
      if Reader.looking_at t "--" then (
        Reader.expect_string "--" t;
        Reader.ident ~keep_case:true t (* var_name should be without -- *))
      else Reader.ident ~keep_case:true t
    in
    Reader.ws t;
    let fallback =
      if Reader.peek t = Some ',' then (
        Reader.comma t;
        (* Parse the fallback length value *)
        Some (Fallback (read_a t)))
      else None
    in
    Reader.expect ')' t;
    (* Create a length var with fallback *)
    let v = var_ref ?fallback var_name in
    Var v)
  else
    (* Not a calc() or var(), so this is not a valid calc expression *)
    Reader.err t "calc() or var()"

let rec read_length t : length =
  Reader.ws t;
  let read_var_length t : length = Var (read_var read_length t) in
  let read_calc_length t : length = Calc (read_calc read_length t) in
  Reader.one_of
    [ read_var_length; read_calc_length; read_length_unit; read_length_keyword ]
    t

(** Read a non-negative length value (for padding properties) *)
let read_non_negative_length t : length =
  let length = read_length t in
  (* Check if the length represents a negative value *)
  match length with
  | Px n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Rem n when n < 0. ->
      Reader.err_invalid t "padding values cannot be negative"
  | Em n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Vw n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Vh n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Cm n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Mm n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | In n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Pt n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | Pc n when n < 0. -> Reader.err_invalid t "padding values cannot be negative"
  | _ -> length

(** Read a percentage value as float (number followed by %) *)
let read_percentage_float t : float =
  Reader.ws t;
  let n = Reader.number t in
  Reader.expect '%' t;
  n

(** Read an alpha value *)
let rec read_alpha t : alpha =
  Reader.ws t;
  (* Check for var() first *)
  if Reader.looking_at t "var(" then Var (read_var read_alpha t)
  else
    (* Check if it's a percentage by looking ahead after the number *)
    let n = Reader.number t in
    if Reader.peek t = Some '%' then (
      Reader.expect '%' t;
      Pct n)
    else Num n
(* Fall back to number *)

(** Read optional alpha component *)
and read_optional_alpha t : alpha =
  Reader.ws t;
  if Reader.peek t = Some '/' then (
    Reader.slash t;
    read_alpha t)
  else None

(** Read a channel value (RGB) *)
let rec read_channel t : channel =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_channel t)
  else
    let n = Reader.number t in
    let unit = Reader.while_ t (fun c -> c = '%') in
    match unit with
    | "%" ->
        (* Clamp percentage to 0-100 per CSS spec *)
        Pct (max 0. (min 100. n))
    | "" ->
        (* Clamp integer RGB values to 0-255 per CSS spec *)
        Int (int_of_float (max 0. (min 255. n)))
    | _ -> Reader.err_invalid t "channel value"

let read_rgb_space_separated t : color =
  let r, g, b =
    Reader.triple ~sep:Reader.ws read_channel read_channel read_channel t
  in
  (* CSS4 allows mixing percentages and numbers in RGB functions. This is a
     change from CSS3 which required all values to be the same type. Since we
     target CSS4 (supported by all major browsers), we allow mixing. *)
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect ')' t;
  match alpha with
  | None -> Rgb { r; g; b }
  | Num _ | Pct _ | Var _ -> Rgba { r; g; b; a = alpha }

let read_rgb_comma_separated t : color =
  let r, g, b =
    Reader.triple ~sep:Reader.comma read_channel read_channel read_channel t
  in
  (* CSS4 allows mixing percentages and numbers in RGB functions. This is a
     change from CSS3 which required all values to be the same type. Since we
     target CSS4 (supported by all major browsers), we allow mixing. *)
  let alpha =
    if Reader.peek t = Some ',' then (
      Reader.comma t;
      read_alpha t)
    else None
  in
  Reader.ws t;
  Reader.expect ')' t;
  match alpha with None -> Rgb { r; g; b } | a -> Rgba { r; g; b; a }

(** Read color space identifier *)
let read_color_space t : color_space =
  let space_ident = Reader.ident t in
  match space_ident with
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
  | _ -> Reader.err_invalid t ("color space: " ^ space_ident)

(** Read color components until ')' or '/' *)
let rec read_color_components space t acc =
  Reader.ws t;
  match Reader.peek t with
  | Some ')' | Some '/' -> List.rev acc
  | Some _ ->
      (* Check if this component should be a percentage based on color space and
         position *)
      let component_count = List.length acc in
      let component =
        match space with
        | (Lab | Oklab | Lch | Oklch) when component_count = 0 ->
            (* L component must be percentage for these spaces in color()
               syntax *)
            let n = Reader.number t in
            Reader.expect '%' t;
            (Pct n : component)
        | _ ->
            (* Check if it's a percentage by looking ahead after the number *)
            let n = Reader.number t in
            if Reader.peek t = Some '%' then (
              Reader.expect '%' t;
              Pct (n /. 100.))
            else Num n
      in
      read_color_components space t (component :: acc)
  | None -> Reader.err_invalid t "color()"

(** Read hex color digits *)
let read_hex_color t =
  let hex =
    Reader.while_ t (fun c ->
        (c >= '0' && c <= '9')
        || (c >= 'a' && c <= 'f')
        || (c >= 'A' && c <= 'F'))
  in
  let len = String.length hex in
  if len = 0 then Reader.err_invalid t "empty hex color"
  else if len = 3 || len = 4 || len = 6 || len = 8 then hex
  else Reader.err_invalid t ("invalid hex color length: " ^ string_of_int len)

(** Read an angle value *)
let rec read_angle t : angle =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_angle t)
  else
    let n = Reader.number t in
    let unit =
      let u = Reader.while_ t Reader.is_alpha in
      String.lowercase_ascii u
    in
    match unit with
    | "deg" -> Deg n
    | "rad" -> Rad n
    | "turn" -> Turn n
    | "grad" -> Grad n
    | "" ->
        Reader.err_invalid t
          "angle values must have units (deg, rad, turn, or grad)"
    | _ -> Reader.err_invalid t ("invalid angle unit: " ^ unit)

(** Read a hue value (preserves unitless vs explicit angle) *)
let rec read_hue t : hue =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_hue t)
  else
    let n = Reader.number t in
    let unit =
      let u = Reader.while_ t Reader.is_alpha in
      String.lowercase_ascii u
    in
    match unit with
    | "" -> Unitless n (* Unitless number, defaults to degrees *)
    | "deg" -> Angle (Deg n)
    | "rad" -> Angle (Rad n)
    | "turn" -> Angle (Turn n)
    | "grad" -> Angle (Grad n)
    | _ -> Reader.err_invalid t ("hue unit: " ^ unit)

let read_separated_values t p1 p2 =
  let v1 = p1 t in
  Reader.ws t;
  let separator = Reader.peek t in
  if separator = Some ',' then Reader.comma t;
  Reader.ws t;
  (* Need whitespace after comma *)
  let v2 = p2 t in
  (v1, v2)

let read_hsl t : color =
  Reader.ws t;
  let h = read_hue t in
  Reader.ws t;
  (* Handle comma or space separator after hue *)
  if Reader.peek t = Some ',' then Reader.comma t;
  Reader.ws t;
  let s = read_percentage_float t in
  Reader.ws t;
  if Reader.peek t = Some ',' then Reader.comma t;
  Reader.ws t;
  let l = read_percentage_float t in
  let a =
    Reader.ws t;
    let next_char = Reader.peek t in
    if next_char = Some ',' then (
      Reader.comma t;
      read_alpha t)
    else if next_char = Some '/' then read_optional_alpha t
    else None
  in
  Reader.ws t;
  Reader.expect ')' t;
  Hsl { h; s = Pct s; l = Pct l; a }

let read_hwb t : color =
  Reader.ws t;
  let h = read_hue t in
  let w, b =
    read_separated_values t read_percentage_float read_percentage_float
  in
  let a =
    Reader.ws t;
    let next_char = Reader.peek t in
    if next_char = Some ',' then (
      Reader.comma t;
      read_alpha t)
    else if next_char = Some '/' then read_optional_alpha t
    else None
  in
  Reader.ws t;
  Reader.expect ')' t;
  Hwb { h; w = Pct w; b = Pct b; a }

let read_oklch t : color =
  Reader.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l =
    let n = Reader.number t in
    if Reader.peek t = Some '%' then (
      Reader.expect '%' t;
      n (* Already a percentage value *))
    else if n >= 0. && n <= 1. then n *. 100. (* Convert 0-1 to percentage *)
    else
      Reader.err_invalid t
        ("oklch() L value must be 0-1 or 0%-100%, got " ^ string_of_float n)
  in
  Reader.ws t;
  let c = Reader.number t in
  Reader.ws t;
  let h = Reader.number t in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect ')' t;
  Oklch { l = Pct l; c; h = Unitless h; alpha }

let read_oklab t : color =
  Reader.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l =
    let n = Reader.number t in
    if Reader.peek t = Some '%' then (
      Reader.expect '%' t;
      n (* Already a percentage value *))
    else if n >= 0. && n <= 1. then n *. 100. (* Convert 0-1 to percentage *)
    else
      Reader.err_invalid t
        ("oklab() L value must be 0-1 or 0%-100%, got " ^ string_of_float n)
  in
  Reader.ws t;
  let a = Reader.number t in
  Reader.ws t;
  let b = Reader.number t in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect ')' t;
  Oklab { l = Pct l; a; b; alpha }

let read_lch t : color =
  Reader.ws t;
  let l, c, h =
    Reader.triple ~sep:Reader.ws read_percentage_float Reader.number
      Reader.number t
  in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect ')' t;
  Lch { l = Pct l; c; h = Unitless h; alpha }

let read_color_function t : color =
  Reader.ws t;
  let space = read_color_space t in
  Reader.ws t;
  let components = read_color_components space t [] in
  let alpha = read_optional_alpha t in
  Reader.expect ')' t;
  Color { space; components; alpha }

(** Parse color space for color-mix - moved before color_parsers *)
let read_color_space t : color_space =
  let ident = Reader.ident t in
  match ident with
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
  | _ -> Reader.err_invalid t ("color space: " ^ ident)

(** Parse color-mix() function - forward declaration needed *)
let rec read_color_mix t : color =
  Reader.ws t;

  (* Parse "in <color-space> [<hue-interpolation-method>]" if present *)
  let in_space, hue =
    (* Check if next word is "in" without consuming it *)
    if Reader.looking_at t "in " || Reader.looking_at t "in," then (
      Reader.expect_string "in" t;
      Reader.ws t;
      let space = read_color_space t in
      Reader.ws t;

      (* For cylindrical color spaces, check for hue interpolation *)
      let hue = Default in
      (* Simplified for now *)
      (Some space, hue))
    else (None, Default)
  in

  Reader.ws t;
  Reader.expect ',' t;
  Reader.ws t;

  (* Parse first color and optional percentage *)
  let color1 = read_color t in
  Reader.ws t;

  (* Parse optional percentage for first color - immediately after color *)
  let percent1 : percentage option =
    try
      let n = Reader.number t in
      Reader.expect '%' t;
      Reader.ws t;
      Some (Pct n)
      (* Don't divide by 100 - keep the raw percentage value *)
    with Reader.Parse_error _ | End_of_file | Failure _ -> None
  in

  Reader.expect ',' t;
  Reader.ws t;

  (* Parse second color and optional percentage *)
  let color2 = read_color t in
  Reader.ws t;

  (* Parse optional percentage for second color - immediately after color *)
  let percent2 : percentage option =
    try
      let n = Reader.number t in
      Reader.expect '%' t;
      Reader.ws t;
      Some (Pct n)
      (* Don't divide by 100 - keep the raw percentage value *)
    with Reader.Parse_error _ | End_of_file | Failure _ -> None
  in

  Reader.ws t;
  Reader.expect ')' t;

  Mix { in_space; hue; color1; percent1; color2; percent2 }

and color_parsers =
  [
    ( "rgb",
      fun t ->
        Reader.expect '(' t;
        Reader.ws t;
        Reader.one_of [ read_rgb_space_separated; read_rgb_comma_separated ] t
    );
    ( "rgba",
      fun t ->
        Reader.expect '(' t;
        Reader.ws t;
        Reader.one_of [ read_rgb_space_separated; read_rgb_comma_separated ] t
    );
    ( "hsl",
      fun t ->
        Reader.expect '(' t;
        read_hsl t );
    ( "hsla",
      fun t ->
        Reader.expect '(' t;
        read_hsl t );
    ( "hwb",
      fun t ->
        Reader.expect '(' t;
        read_hwb t );
    ( "oklch",
      fun t ->
        Reader.expect '(' t;
        read_oklch t );
    ( "oklab",
      fun t ->
        Reader.expect '(' t;
        read_oklab t );
    ( "lch",
      fun t ->
        Reader.expect '(' t;
        read_lch t );
    ( "color",
      fun t ->
        Reader.expect '(' t;
        read_color_function t );
    ( "color-mix",
      fun t ->
        Reader.expect '(' t;
        read_color_mix t );
  ]

and read_color t : color =
  Reader.ws t;
  if Reader.peek t = Some '#' then (
    Reader.expect '#' t;
    let hex = read_hex_color t in
    Hex { hash = true; value = hex })
  else
    let ident = Reader.ident t in
    match List.assoc_opt ident color_parsers with
    | Some parser -> parser t
    | None -> (
        match ident with
        | "var" -> Var (read_var_after_ident read_color t)
        | _ -> read_color_keyword (Reader.of_string ident))

and read_color_keyword t : color =
  let keyword = Reader.ident t in
  match keyword with
  | "transparent" -> Transparent
  | "currentcolor" -> Current
  | "inherit" -> Inherit
  | "red" -> Named Red
  | "green" -> Named Green
  | "blue" -> Named Blue
  | "white" -> Named White
  | "black" -> Named Black
  | "gray" -> Named Gray
  | "grey" -> Named Grey
  | "silver" -> Named Silver
  | "maroon" -> Named Maroon
  | "yellow" -> Named Yellow
  | "olive" -> Named Olive
  | "lime" -> Named Lime
  | "aqua" -> Named Aqua
  | "cyan" -> Named Cyan
  | "teal" -> Named Teal
  | "navy" -> Named Navy
  | "fuchsia" -> Named Fuchsia
  | "magenta" -> Named Magenta
  | "purple" -> Named Purple
  | "orange" -> Named Orange
  | "pink" -> Named Pink
  | "aliceblue" -> Named Alice_blue
  | "antiquewhite" -> Named Antique_white
  | "aquamarine" -> Named Aquamarine
  | "azure" -> Named Azure
  | "beige" -> Named Beige
  | "bisque" -> Named Bisque
  | "blanchedalmond" -> Named Blanched_almond
  | "blueviolet" -> Named Blue_violet
  | "brown" -> Named Brown
  | "burlywood" -> Named Burlywood
  | "cadetblue" -> Named Cadet_blue
  | "chartreuse" -> Named Chartreuse
  | "chocolate" -> Named Chocolate
  | "coral" -> Named Coral
  | "cornflowerblue" -> Named Cornflower_blue
  | "cornsilk" -> Named Cornsilk
  | "crimson" -> Named Crimson
  | "darkblue" -> Named Dark_blue
  | "darkcyan" -> Named Dark_cyan
  | "darkgoldenrod" -> Named Dark_goldenrod
  | "darkgray" -> Named Dark_gray
  | "darkgreen" -> Named Dark_green
  | "darkgrey" -> Named Dark_grey
  | "darkkhaki" -> Named Dark_khaki
  | "darkmagenta" -> Named Dark_magenta
  | "darkolivegreen" -> Named Dark_olive_green
  | "darkorange" -> Named Dark_orange
  | "darkorchid" -> Named Dark_orchid
  | "darkred" -> Named Dark_red
  | "darksalmon" -> Named Dark_salmon
  | "darkseagreen" -> Named Dark_sea_green
  | "darkslateblue" -> Named Dark_slate_blue
  | "darkslategray" -> Named Dark_slate_gray
  | "darkslategrey" -> Named Dark_slate_grey
  | "darkturquoise" -> Named Dark_turquoise
  | "darkviolet" -> Named Dark_violet
  | "deeppink" -> Named Deep_pink
  | "deepskyblue" -> Named Deep_sky_blue
  | "dimgray" -> Named Dim_gray
  | "dimgrey" -> Named Dim_grey
  | "dodgerblue" -> Named Dodger_blue
  | "firebrick" -> Named Firebrick
  | "floralwhite" -> Named Floral_white
  | "forestgreen" -> Named Forest_green
  | "gainsboro" -> Named Gainsboro
  | "ghostwhite" -> Named Ghost_white
  | "gold" -> Named Gold
  | "goldenrod" -> Named Goldenrod
  | "greenyellow" -> Named Green_yellow
  | "honeydew" -> Named Honeydew
  | "hotpink" -> Named Hot_pink
  | "indianred" -> Named Indian_red
  | "indigo" -> Named Indigo
  | "ivory" -> Named Ivory
  | "khaki" -> Named Khaki
  | "lavender" -> Named Lavender
  | "lavenderblush" -> Named Lavender_blush
  | "lawngreen" -> Named Lawn_green
  | "lemonchiffon" -> Named Lemon_chiffon
  | "lightblue" -> Named Light_blue
  | "lightcoral" -> Named Light_coral
  | "lightcyan" -> Named Light_cyan
  | "lightgoldenrodyellow" -> Named Light_goldenrod_yellow
  | "lightgray" -> Named Light_gray
  | "lightgreen" -> Named Light_green
  | "lightgrey" -> Named Light_grey
  | "lightpink" -> Named Light_pink
  | "lightsalmon" -> Named Light_salmon
  | "lightseagreen" -> Named Light_sea_green
  | "lightskyblue" -> Named Light_sky_blue
  | "lightslategray" -> Named Light_slate_gray
  | "lightslategrey" -> Named Light_slate_grey
  | "lightsteelblue" -> Named Light_steel_blue
  | "lightyellow" -> Named Light_yellow
  | "limegreen" -> Named Lime_green
  | "linen" -> Named Linen
  | "mediumaquamarine" -> Named Medium_aquamarine
  | "mediumblue" -> Named Medium_blue
  | "mediumorchid" -> Named Medium_orchid
  | "mediumpurple" -> Named Medium_purple
  | "mediumseagreen" -> Named Medium_sea_green
  | "mediumslateblue" -> Named Medium_slate_blue
  | "mediumspringgreen" -> Named Medium_spring_green
  | "mediumturquoise" -> Named Medium_turquoise
  | "mediumvioletred" -> Named Medium_violet_red
  | "midnightblue" -> Named Midnight_blue
  | "mintcream" -> Named Mint_cream
  | "mistyrose" -> Named Misty_rose
  | "moccasin" -> Named Moccasin
  | "navajowhite" -> Named Navajo_white
  | "oldlace" -> Named Old_lace
  | "olivedrab" -> Named Olive_drab
  | "orangered" -> Named Orange_red
  | "orchid" -> Named Orchid
  | "palegoldenrod" -> Named Pale_goldenrod
  | "palegreen" -> Named Pale_green
  | "paleturquoise" -> Named Pale_turquoise
  | "palevioletred" -> Named Pale_violet_red
  | "papayawhip" -> Named Papaya_whip
  | "peachpuff" -> Named Peach_puff
  | "peru" -> Named Peru
  | "plum" -> Named Plum
  | "powderblue" -> Named Powder_blue
  | "rebeccapurple" -> Named Rebecca_purple
  | "rosybrown" -> Named Rosy_brown
  | "royalblue" -> Named Royal_blue
  | "saddlebrown" -> Named Saddle_brown
  | "salmon" -> Named Salmon
  | "sandybrown" -> Named Sandy_brown
  | "seagreen" -> Named Sea_green
  | "seashell" -> Named Sea_shell
  | "sienna" -> Named Sienna
  | "skyblue" -> Named Sky_blue
  | "slateblue" -> Named Slate_blue
  | "slategray" -> Named Slate_gray
  | "slategrey" -> Named Slate_grey
  | "snow" -> Named Snow
  | "springgreen" -> Named Spring_green
  | "steelblue" -> Named Steel_blue
  | "tan" -> Named Tan
  | "thistle" -> Named Thistle
  | "tomato" -> Named Tomato
  | "turquoise" -> Named Turquoise
  | "violet" -> Named Violet
  | "wheat" -> Named Wheat
  | "whitesmoke" -> Named White_smoke
  | "yellowgreen" -> Named Yellow_green
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | _ -> Reader.err_invalid t ("color: " ^ keyword)

(** Read a duration value *)
let rec read_duration t : duration =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_duration t)
  else
    let n = Reader.number t in
    let unit =
      let u = Reader.while_ t Reader.is_alpha in
      String.lowercase_ascii u
    in
    match unit with
    | "s" -> S n
    | "ms" -> Ms n
    | _ -> Reader.err_invalid t ("duration unit: " ^ unit)

(** Read a dimension (number with unit) - returns value and unit separately *)
let read_dimension t : float * string =
  Reader.ws t;
  let n = Reader.number t in
  let unit = Reader.while_ t (fun c -> (c >= 'a' && c <= 'z') || c = '%') in
  (n, unit)

(** Read a number value *)
let rec read_number t : number =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_number t)
  else
    let n = Reader.number t in
    if n = float_of_int (int_of_float n) then Int (int_of_float n) else Float n

(** Read a percentage type with var() and calc() support *)
let rec read_percentage t : percentage =
  Reader.ws t;
  if Reader.looking_at t "var(" then Var (read_var read_percentage t)
  else if Reader.looking_at t "calc(" then Calc (read_calc read_percentage t)
  else
    let n = Reader.number t in
    Reader.expect '%' t;
    Pct n

(** Read length_percentage value *)
let rec read_length_percentage t : length_percentage =
  Reader.ws t;
  if Reader.looking_at t "var(" then Var (read_var read_length_percentage t)
  else if Reader.looking_at t "calc(" then
    Calc (read_calc read_length_percentage t)
  else
    (* Try to read as percentage first *)
    match Reader.option read_percentage t with
    | Some p -> Percentage p
    | None -> (
        (* Otherwise try to read as length *)
        match read_length t with
        | l -> Length l)

(** Read color_name value *)
let read_color_name t : color_name =
  Reader.ws t;
  let s = Reader.ident t in
  match String.lowercase_ascii s with
  | "red" -> Red
  | "blue" -> Blue
  | "green" -> Green
  | "white" -> White
  | "black" -> Black
  | "yellow" -> Yellow
  | "cyan" -> Cyan
  | "magenta" -> Magenta
  | "gray" | "grey" -> Gray
  | "orange" -> Orange
  | "purple" -> Purple
  | "pink" -> Pink
  | "silver" -> Silver
  | "maroon" -> Maroon
  | "fuchsia" -> Fuchsia
  | "lime" -> Lime
  | "olive" -> Olive
  | "navy" -> Navy
  | "teal" -> Teal
  | "aqua" -> Aqua
  | _ -> Reader.err_invalid t ("color name: " ^ s)

(** Pretty print and read meta values *)
let pp_meta : meta Pp.t =
 fun _ctx _v ->
  (* Meta is an abstract type - cannot pattern match on it *)
  failwith "pp_meta: not implemented for abstract meta type"

let read_meta _t : meta =
  (* Meta is an abstract type - cannot construct values of it *)
  failwith "read_meta: not implemented for abstract meta type"

(** Read hue_interpolation *)
let read_hue_interpolation t : hue_interpolation =
  Reader.ws t;
  Reader.enum "hue-interpolation"
    [
      ("shorter", Shorter);
      ("longer", Longer);
      ("increasing", Increasing);
      ("decreasing", Decreasing);
      ("default", Default);
    ]
    t

(** Pretty print calc_op *)
let pp_calc_op : calc_op Pp.t =
 fun ctx op ->
  match op with
  | Add -> Pp.string ctx " + "
  | Sub -> Pp.string ctx " - "
  | Mul -> Pp.string ctx " * "
  | Div -> Pp.string ctx " / "

(** Read calc_op *)
let read_calc_op t : calc_op =
  Reader.ws t;
  match Reader.peek t with
  | Some '+' ->
      Reader.skip t;
      Add
  | Some '-' ->
      Reader.skip t;
      Sub
  | Some '*' ->
      Reader.skip t;
      Mul
  | Some '/' ->
      Reader.skip t;
      Div
  | _ -> Reader.err_invalid t "calc operator"

(** Read component value *)
let rec read_component t : component =
  Reader.ws t;
  if Reader.looking_at t "var(" then Var (read_var read_component t)
  else if Reader.looking_at t "calc(" then Calc (read_calc read_component t)
  else
    let n = Reader.number t in
    match Reader.peek t with
    | Some '%' ->
        Reader.skip t;
        Pct n
    | _ -> Num n

(* Var helper functions *)
let var_name v = v.name
let var_layer v = v.layer

(** Read padding shorthand property (1-4 values) *)
let read_padding_shorthand t : length list =
  (* CSS padding accepts 1-4 space-separated non-negative values *)
  let rec read_values acc count =
    if count >= 4 then List.rev acc
    else
      match Reader.option read_non_negative_length t with
      | Some v -> read_values (v :: acc) (count + 1)
      | None -> List.rev acc
  in
  let values = read_values [] 0 in
  if values = [] then Reader.err_invalid t "padding requires at least one value"
  else values

(** Read margin shorthand property (1-4 values) Source:
    https://www.w3.org/TR/CSS21/box.html#margin-properties CSS margin accepts
    1-4 space-separated values *)
let read_margin_shorthand t : length list =
  (* CSS margin accepts 1-4 space-separated values *)
  let rec read_values acc count =
    if count >= 4 then List.rev acc
    else
      match Reader.option read_length t with
      | Some v -> read_values (v :: acc) (count + 1)
      | None -> List.rev acc
  in
  let values = read_values [] 0 in
  if values = [] then Reader.err_invalid t "margin requires at least one value"
  else values
