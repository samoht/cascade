(** CSS Values & Units parsing using Reader API *)

open Reader

type meta = ..

type 'a var = {
  name : string;
  fallback : 'a option;
  default : 'a option;
  layer : string option;
  meta : meta option;
}

let var_ref ?fallback ?default ?layer ?meta name =
  { name; fallback; default; layer; meta }

type calc_op = Add | Sub | Mult | Div

type 'a calc =
  | Var of 'a var
  | Val of 'a
  | Num of float
  | Expr of 'a calc * calc_op * 'a calc

type length =
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Ch of float
  | Lh of float
  | Num of float
  | Auto
  | Zero
  | Inherit
  | Fit_content
  | Max_content
  | Min_content
  | From_font
  | Var of length var
  | Calc of length calc

type color_space =
  | Srgb
  | Srgb_linear
  | Display_p3
  | A98_rgb
  | Prophoto_rgb
  | Rec2020
  | Lab
  | Oklab
  | Xyz
  | Xyz_d50
  | Xyz_d65
  | Lch
  | Oklch
  | Hsl
  | Hwb

type color_name =
  | Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  | Cyan
  | Magenta
  | Gray
  | Grey
  | Orange
  | Purple
  | Pink
  | Silver
  | Maroon
  | Fuchsia
  | Lime
  | Olive
  | Navy
  | Teal
  | Aqua
  | Alice_blue
  | Antique_white
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Blanched_almond
  | Blue_violet
  | Brown
  | Burlywood
  | Cadet_blue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflower_blue
  | Cornsilk
  | Crimson
  | Dark_blue
  | Dark_cyan
  | Dark_goldenrod
  | Dark_gray
  | Dark_green
  | Dark_grey
  | Dark_khaki
  | Dark_magenta
  | Dark_olive_green
  | Dark_orange
  | Dark_orchid
  | Dark_red
  | Dark_salmon
  | Dark_sea_green
  | Dark_slate_blue
  | Dark_slate_gray
  | Dark_slate_grey
  | Dark_turquoise
  | Dark_violet
  | Deep_pink
  | Deep_sky_blue
  | Dim_gray
  | Dim_grey
  | Dodger_blue
  | Firebrick
  | Floral_white
  | Forest_green
  | Gainsboro
  | Ghost_white
  | Gold
  | Goldenrod
  | Green_yellow
  | Honeydew
  | Hot_pink
  | Indian_red
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavender_blush
  | Lawn_green
  | Lemon_chiffon
  | Light_blue
  | Light_coral
  | Light_cyan
  | Light_goldenrod_yellow
  | Light_gray
  | Light_green
  | Light_grey
  | Light_pink
  | Light_salmon
  | Light_sea_green
  | Light_sky_blue
  | Light_slate_gray
  | Light_slate_grey
  | Light_steel_blue
  | Light_yellow
  | Lime_green
  | Linen
  | Medium_aquamarine
  | Medium_blue
  | Medium_orchid
  | Medium_purple
  | Medium_sea_green
  | Medium_slate_blue
  | Medium_spring_green
  | Medium_turquoise
  | Medium_violet_red
  | Midnight_blue
  | Mint_cream
  | Misty_rose
  | Moccasin
  | Navajo_white
  | Old_lace
  | Olive_drab
  | Orange_red
  | Orchid
  | Pale_goldenrod
  | Pale_green
  | Pale_turquoise
  | Pale_violet_red
  | Papaya_whip
  | Peach_puff
  | Peru
  | Plum
  | Powder_blue
  | Rebecca_purple
  | Rosy_brown
  | Royal_blue
  | Saddle_brown
  | Salmon
  | Sandy_brown
  | Sea_green
  | Sea_shell
  | Sienna
  | Sky_blue
  | Slate_blue
  | Slate_gray
  | Slate_grey
  | Snow
  | Spring_green
  | Steel_blue
  | Tan
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White_smoke
  | Yellow_green

type channel =
  | Int of int (* 0–255, legacy/comma syntax *)
  | Num of float (* 0–255, modern/space syntax *)
  | Pct of float (* 0%–100% *)
  | Var of channel var

type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var

type hue =
  | Unitless of float (* Unitless number, defaults to degrees *)
  | Angle of angle (* Explicit angle unit *)
  | Var of hue var

type alpha =
  | None
  | Num of float (* Number value (0-1) *)
  | Pct of float (* Percentage value (0%-100%) *)
  | Var of alpha var

(** CSS color component values *)
type component =
  | Number of float
  | Pct of float
  | Angle of hue (* for color(lch ...) / color(lab ...) syntaxes *)
  | Var of component var
  | Calc of component calc

(** CSS percentage values *)
type percentage =
  | Pct of float (* 0%–100% as a % token *)
  | Var of percentage var
  | Calc of percentage calc (* calc(...) that resolves to a % *)

(** CSS hue interpolation options *)
type hue_interpolation = Shorter | Longer | Increasing | Decreasing | Default

type color =
  | Hex of { hash : bool; value : string }
  | Rgb of { r : channel; g : channel; b : channel }
  | Rgba of { r : channel; g : channel; b : channel; a : alpha }
  | Hsl of { h : hue; s : percentage; l : percentage; a : alpha }
  | Hwb of { h : hue; w : percentage; b : percentage; a : alpha }
  | Color of { space : color_space; components : component list; alpha : alpha }
  | Oklch of { l : percentage; c : float; h : hue; alpha : alpha }
  | Oklab of { l : percentage; a : float; b : float; alpha : alpha }
  | Lch of { l : percentage; c : float; h : hue; alpha : alpha }
  | Named of color_name
  | Var of color var
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space option; (* None => default per spec *)
      hue : hue_interpolation;
      color1 : color;
      percent1 : percentage option;
      color2 : color;
      percent2 : percentage option;
    }

type duration = Ms of float | S of float | Var of duration var
type number = Float of float | Int of int | Pct of float | Var of number var

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
  | Add -> Pp.string ctx " + "
  | Sub -> Pp.string ctx " - "
  | Mult -> Pp.string ctx " * "
  | Div -> Pp.string ctx " / "

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  (* When inlining is enabled, output the default value if available *)
  if ctx.inline && v.default <> None then
    match v.default with
    | Some value -> pp_value ctx value
    | None -> assert false (* unreachable due to condition above *)
  else (
    (* Standard var() reference output *)
    Pp.string ctx "var(--";
    Pp.string ctx v.name;
    match v.fallback with
    | None -> Pp.char ctx ')'
    | Some value ->
        Pp.string ctx ", ";
        pp_value ctx value;
        Pp.char ctx ')')

(** Helper to format function calls: name(args) *)
let pp_fun name pp_args ctx args =
  Pp.string ctx name;
  Pp.char ctx '(';
  pp_args ctx args;
  Pp.char ctx ')'

(** Helper to format function calls with comma-separated list: name(arg1, arg2,
    ...) *)
let pp_fun' name pp_item ctx items =
  pp_fun name (Pp.list ~sep:Pp.comma pp_item) ctx items

let pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  pp_fun "calc"
    (fun ctx calc ->
      match calc with
      | Val v -> pp_value ctx v
      | Var v -> pp_var pp_value ctx v
      | Num n -> Pp.float ctx n
      | Expr (left, op, right) ->
          let rec pp_calc_inner ctx = function
            | Val v -> pp_value ctx v
            | Var v -> pp_var pp_value ctx v
            | Num n -> Pp.float ctx n
            | Expr (left, op, right) ->
                pp_calc_inner ctx left;
                pp_op ctx op;
                pp_calc_inner ctx right
          in
          pp_calc_inner ctx (Expr (left, op, right)))
    ctx calc

let rec pp_length : length Pp.t =
 fun ctx -> function
  | Px n when n = 0. -> Pp.char ctx '0'
  | Cm f when f = 0. -> Pp.char ctx '0'
  | Mm f when f = 0. -> Pp.char ctx '0'
  | Q f when f = 0. -> Pp.char ctx '0'
  | In f when f = 0. -> Pp.char ctx '0'
  | Pt f when f = 0. -> Pp.char ctx '0'
  | Pc f when f = 0. -> Pp.char ctx '0'
  | Rem f when f = 0. -> Pp.char ctx '0'
  | Em f when f = 0. -> Pp.char ctx '0'
  | Ex f when f = 0. -> Pp.char ctx '0'
  | Cap f when f = 0. -> Pp.char ctx '0'
  | Ic f when f = 0. -> Pp.char ctx '0'
  | Rlh f when f = 0. -> Pp.char ctx '0'
  | Pct f when f = 0. -> Pp.char ctx '0'
  | Vw f when f = 0. -> Pp.char ctx '0'
  | Vh f when f = 0. -> Pp.char ctx '0'
  | Vmin f when f = 0. -> Pp.char ctx '0'
  | Vmax f when f = 0. -> Pp.char ctx '0'
  | Vi f when f = 0. -> Pp.char ctx '0'
  | Vb f when f = 0. -> Pp.char ctx '0'
  | Dvh f when f = 0. -> Pp.char ctx '0'
  | Dvw f when f = 0. -> Pp.char ctx '0'
  | Dvmin f when f = 0. -> Pp.char ctx '0'
  | Dvmax f when f = 0. -> Pp.char ctx '0'
  | Lvh f when f = 0. -> Pp.char ctx '0'
  | Lvw f when f = 0. -> Pp.char ctx '0'
  | Lvmin f when f = 0. -> Pp.char ctx '0'
  | Lvmax f when f = 0. -> Pp.char ctx '0'
  | Svh f when f = 0. -> Pp.char ctx '0'
  | Svw f when f = 0. -> Pp.char ctx '0'
  | Svmin f when f = 0. -> Pp.char ctx '0'
  | Svmax f when f = 0. -> Pp.char ctx '0'
  | Ch f when f = 0. -> Pp.char ctx '0'
  | Lh f when f = 0. -> Pp.char ctx '0'
  | Zero -> Pp.char ctx '0'
  | Px n ->
      Pp.float ctx n;
      Pp.string ctx "px"
  | Cm f ->
      Pp.float ctx f;
      Pp.string ctx "cm"
  | Mm f ->
      Pp.float ctx f;
      Pp.string ctx "mm"
  | Q f ->
      Pp.float ctx f;
      Pp.string ctx "q"
  | In f ->
      Pp.float ctx f;
      Pp.string ctx "in"
  | Pt f ->
      Pp.float ctx f;
      Pp.string ctx "pt"
  | Pc f ->
      Pp.float ctx f;
      Pp.string ctx "pc"
  | Rem f ->
      Pp.float ctx f;
      Pp.string ctx "rem"
  | Em f ->
      Pp.float ctx f;
      Pp.string ctx "em"
  | Ex f ->
      Pp.float ctx f;
      Pp.string ctx "ex"
  | Cap f ->
      Pp.float ctx f;
      Pp.string ctx "cap"
  | Ic f ->
      Pp.float ctx f;
      Pp.string ctx "ic"
  | Rlh f ->
      Pp.float ctx f;
      Pp.string ctx "rlh"
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Vw f ->
      Pp.float ctx f;
      Pp.string ctx "vw"
  | Vh f ->
      Pp.float ctx f;
      Pp.string ctx "vh"
  | Vmin f ->
      Pp.float ctx f;
      Pp.string ctx "vmin"
  | Vmax f ->
      Pp.float ctx f;
      Pp.string ctx "vmax"
  | Vi f ->
      Pp.float ctx f;
      Pp.string ctx "vi"
  | Vb f ->
      Pp.float ctx f;
      Pp.string ctx "vb"
  | Dvh f ->
      Pp.float ctx f;
      Pp.string ctx "dvh"
  | Dvw f ->
      Pp.float ctx f;
      Pp.string ctx "dvw"
  | Dvmin f ->
      Pp.float ctx f;
      Pp.string ctx "dvmin"
  | Dvmax f ->
      Pp.float ctx f;
      Pp.string ctx "dvmax"
  | Lvh f ->
      Pp.float ctx f;
      Pp.string ctx "lvh"
  | Lvw f ->
      Pp.float ctx f;
      Pp.string ctx "lvw"
  | Lvmin f ->
      Pp.float ctx f;
      Pp.string ctx "lvmin"
  | Lvmax f ->
      Pp.float ctx f;
      Pp.string ctx "lvmax"
  | Svh f ->
      Pp.float ctx f;
      Pp.string ctx "svh"
  | Svw f ->
      Pp.float ctx f;
      Pp.string ctx "svw"
  | Svmin f ->
      Pp.float ctx f;
      Pp.string ctx "svmin"
  | Svmax f ->
      Pp.float ctx f;
      Pp.string ctx "svmax"
  | Ch f ->
      Pp.float ctx f;
      Pp.string ctx "ch"
  | Lh f ->
      Pp.float ctx f;
      Pp.string ctx "lh"
  | Num f -> Pp.float ctx f
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Fit_content -> Pp.string ctx "fit-content"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | From_font -> Pp.string ctx "from-font"
  | Var v -> pp_var pp_length ctx v
  | Calc cv -> (
      (* Optimize calc(infinity * 1px) to 3.40282e38px for minification *)
      match cv with
      | Expr (Val (Num f), Mult, Val (Px 1.)) when f = infinity ->
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
  | Deg f ->
      Pp.float ctx f;
      Pp.string ctx "deg"
  | Rad f ->
      Pp.float ctx f;
      Pp.string ctx "rad"
  | Turn f ->
      Pp.float ctx f;
      Pp.string ctx "turn"
  | Grad f ->
      Pp.float ctx f;
      Pp.string ctx "grad"
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

and pp_percentage : percentage Pp.t =
 fun ctx -> function
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_percentage ctx v
  | Calc c -> pp_calc pp_percentage ctx c

and pp_component : component Pp.t =
 fun ctx -> function
  | Number f -> Pp.float ctx f
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

(* RGB helper function *)
let pp_rgb ctx r g b alpha =
  Pp.string ctx "rgb(";
  pp_channel ctx r;
  Pp.space ctx ();
  pp_channel ctx g;
  Pp.space ctx ();
  pp_channel ctx b;
  (match alpha with
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.string ctx " / ";
      pp_alpha ctx a);
  Pp.char ctx ')'

(* OKLCH helper function *)
let pp_oklch ctx l c h alpha =
  Pp.string ctx "oklch(";
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  (match alpha with
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.string ctx " / ";
      pp_alpha ctx a);
  Pp.char ctx ')'

let pp_hsl ctx h s l a =
  Pp.string ctx "hsl(";
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx s;
  Pp.space ctx ();
  pp_percentage ctx l;
  (match a with
  | None -> ()
  | (Num _ | Pct _ | Var _) as alpha ->
      Pp.string ctx " / ";
      pp_alpha ctx alpha);
  Pp.char ctx ')'

let pp_hwb ctx h w b a =
  Pp.string ctx "hwb(";
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx w;
  Pp.space ctx ();
  pp_percentage ctx b;
  (match a with
  | None -> ()
  | (Num _ | Pct _ | Var _) as alpha ->
      Pp.string ctx " / ";
      pp_alpha ctx alpha);
  Pp.char ctx ')'

let pp_oklab ctx l a b alpha =
  Pp.string ctx "oklab(";
  (* Oklab L must always be output as percentage per CSS spec *)
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx a;
  Pp.space ctx ();
  Pp.float ctx b;
  (match alpha with
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.string ctx " / ";
      pp_alpha ctx a);
  Pp.char ctx ')'

let pp_lch ctx l c h alpha =
  Pp.string ctx "lch(";
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  (match alpha with
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.string ctx " / ";
      pp_alpha ctx a);
  Pp.char ctx ')'

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

(* Color-mix helper function *)
and pp_color_mix ctx in_space hue color1 percent1 color2 percent2 =
  Pp.string ctx "color-mix(";
  (match in_space with
  | Some space ->
      Pp.string ctx "in ";
      pp_color_space ctx space
  | None -> Pp.string ctx "in oklab");
  (* default per spec *)
  (match hue with
  | Default -> ()
  | _ ->
      Pp.space ctx ();
      pp_hue_interpolation ctx hue;
      Pp.string ctx " hue");
  Pp.string ctx ", ";
  pp_color_in_mix ctx color1;
  (match percent1 with
  | Some p ->
      Pp.space ctx ();
      pp_percentage ctx p
  | None -> ());
  Pp.string ctx ", ";
  pp_color_in_mix ctx color2;
  (match percent2 with
  | Some p ->
      Pp.space ctx ();
      pp_percentage ctx p
  | None -> ());
  Pp.char ctx ')'

and pp_color' ctx space components alpha =
  Pp.string ctx "color(";
  pp_color_space ctx space;
  (match components with
  | [] -> ()
  | _ ->
      Pp.string ctx " ";
      Pp.list ~sep:Pp.space pp_component ctx components);
  (match alpha with
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.string ctx " / ";
      pp_alpha ctx a);
  Pp.char ctx ')'

(* Convert to Pp-based color formatter *)
and pp_color : color Pp.t =
 fun ctx -> function
  | Hex { hash = _; value } ->
      Pp.char ctx '#';
      Pp.string ctx value
  | Rgb { r; g; b } -> pp_rgb ctx r g b None
  | Rgba { r; g; b; a } -> pp_rgb ctx r g b a
  | Hsl { h; s; l; a } -> pp_hsl ctx h s l a
  | Hwb { h; w; b; a } -> pp_hwb ctx h w b a
  | Color { space; components; alpha } -> pp_color' ctx space components alpha
  | Oklch { l; c; h; alpha } -> pp_oklch ctx l c h alpha
  | Oklab { l; a; b; alpha } -> pp_oklab ctx l a b alpha
  | Lch { l; c; h; alpha } -> pp_lch ctx l c h alpha
  | Named name -> pp_color_name ctx name
  | Var v -> pp_var pp_color ctx v
  | Current -> Pp.string ctx "currentcolor"
  | Transparent -> Pp.string ctx "transparent"
  | Inherit -> Pp.string ctx "inherit"
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space hue color1 percent1 color2 percent2

let rec pp_duration : duration Pp.t =
 fun ctx -> function
  | Ms f ->
      Pp.float ctx f;
      Pp.string ctx "ms"
  | S f ->
      Pp.float ctx f;
      Pp.char ctx 's'
  | Var v -> pp_var pp_duration ctx v

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Float f -> Pp.float ctx f
  | Int i -> Pp.int ctx i
  | Pct p ->
      Pp.float ctx p;
      Pp.char ctx '%'
  | Var v -> pp_var pp_number ctx v

let pp_percentage : float Pp.t =
 fun ctx p ->
  Pp.float ctx p;
  Pp.char ctx '%'

(* Calc module for building calc() expressions *)
module Calc = struct
  let add left right = Expr (left, Add, right)
  let sub left right = Expr (left, Sub, right)
  let mul left right = Expr (left, Mult, right)
  let div left right = Expr (left, Div, right)

  (* Operators *)
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div

  (* Value constructors *)
  let length len = Val len

  let var : ?default:'a -> ?fallback:'a -> string -> 'a calc =
   fun ?default ?fallback name -> Var (var_ref ?default ?fallback name)

  let float f : length calc = Val (Num f)
  let infinity : length calc = Val (Num infinity)
  let px n = Val (Px n)
  let rem f = Val (Rem f)
  let em f = Val (Em f)
  let pct f : length calc = Val (Pct f)
end

(** Error helpers *)
let err_invalid t what = raise (Parse_error ("invalid " ^ what, t))

(** var() parser after "var" ident has been consumed *)
let read_var_after_ident : type a. (Reader.t -> a) -> Reader.t -> a var =
 fun read_value t ->
  expect t '(';
  ws t;
  let var_name =
    if looking_at t "--" then (
      expect_string t "--";
      ident t)
    else ident t
  in
  ws t;
  let fallback =
    if peek t = Some ',' then (
      skip t;
      ws t;
      Some (read_value t))
    else None
  in
  ws t;
  expect t ')';
  var_ref ?fallback var_name

(** Generic var() parser that returns a var reference *)
let read_var : type a. (Reader.t -> a) -> Reader.t -> a var =
 fun read_value t ->
  expect_string t "var";
  read_var_after_ident read_value t

(** Read a CSS length value *)
let rec read_length t : length =
  ws t;
  (* Try to parse number first *)
  let num_opt = try_parse number t in
  match num_opt with
  | None -> (
      (* Try keyword values *)
      let keyword = ident t in
      match String.lowercase_ascii keyword with
      | "auto" -> Auto
      | "max-content" -> Max_content
      | "min-content" -> Min_content
      | "fit-content" -> Fit_content
      | "from-font" -> From_font
      | "inherit" -> Inherit
      | "var" -> Var (read_var_after_ident read_length t)
      | _ -> err_invalid t ("length keyword: " ^ keyword))
  | Some n -> (
      (* Check for unit *)
      let unit = while_ t (fun c -> (c >= 'a' && c <= 'z') || c = '%') in
      match unit with
      | "" when n = 0.0 -> Zero (* Zero only when 0 without unit *)
      | "" -> Num n
      | _ when n = 0.0 -> Zero (* 0 with any unit becomes Zero *)
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
      | _ -> err_invalid t ("length unit: " ^ unit))

(** Read a percentage value *)
let read_percentage t : float =
  ws t;
  let n = number t in
  expect t '%';
  n

(** Read an alpha value *)
let rec read_alpha t : alpha =
  ws t;
  (* Check for var() first *)
  if looking_at t "var(" then Var (read_var read_alpha t)
  else
    (* Try percentage first *)
    match try_parse read_percentage t with
    | Some pct -> Pct pct
    | None -> Num (number t)
(* Fall back to number *)

(** Read optional alpha component *)
and read_optional_alpha t : alpha =
  ws t;
  if peek t = Some '/' then (
    skip t;
    ws t;
    read_alpha t)
  else None

(** Read a channel value (RGB) *)
let rec read_channel t : channel =
  ws t;
  (* Check for var() *)
  if looking_at t "var(" then Var (read_var read_channel t)
  else
    let n = number t in
    let unit = while_ t (fun c -> c = '%') in
    match unit with
    | "%" -> Pct n
    | "" -> Int (int_of_float n)
    | _ -> err_invalid t "channel value"

(** Read space-separated RGB values (modern syntax) *)
let read_rgb_space_separated t : color =
  (* Try percentage format first with proper backtracking *)
  match
    try_parse
      (fun t ->
        let r_pct = read_channel t in
        ws t;
        let g_pct = read_channel t in
        ws t;
        let b_pct = read_channel t in
        let alpha = read_optional_alpha t in
        ws t;
        expect t ')';
        (r_pct, g_pct, b_pct, alpha))
      t
  with
  | Some (r, g, b, alpha) -> (
      (* Use channels directly from the parsed result *)
      match alpha with
      | None -> Rgb { r; g; b }
      | Num _ | Pct _ | Var _ -> Rgba { r; g; b; a = alpha })
  | None ->
      (* This should not happen with the current logic *)
      err_invalid t "RGB values"

(** Read comma-separated RGB values (legacy syntax) *)
let read_rgb_comma_separated t : color =
  (* Allow mixed channel formats - each channel can be int or percentage *)
  let r = read_channel t in
  ws t;
  expect t ',';
  ws t;
  let g = read_channel t in
  ws t;
  expect t ',';
  ws t;
  let b = read_channel t in
  ws t;
  (* For legacy comma syntax, alpha uses comma instead of slash *)
  let alpha =
    if peek t = Some ',' then (
      skip t;
      ws t;
      Some (read_alpha t))
    else None
  in
  ws t;
  expect t ')';
  match alpha with None -> Rgb { r; g; b } | Some a -> Rgba { r; g; b; a }

(** Read color space identifier *)
let read_color_space t : color_space =
  let space_ident = ident t |> String.lowercase_ascii in
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
  | _ -> err_invalid t ("color space: " ^ space_ident)

(** Read color components until ')' or '/' *)
let rec read_color_components space t acc =
  ws t;
  match peek t with
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
            let n = number t in
            expect t '%';
            (Pct n : component)
        | _ -> (
            (* Try percentage first, then plain number *)
            match try_parse read_percentage t with
            | Some pct -> Pct pct
            | None -> Number (number t))
      in
      read_color_components space t (component :: acc)
  | None -> err_invalid t "color()"

(** Read a CSS color value - single entry point for all color parsing *)
let rec read_color t : color =
  ws t;
  match peek t with
  | Some '#' ->
      (* Hex color: #fff or #ffffff *)
      expect t '#';
      let hex = hex_color t in
      Hex { hash = true; value = hex }
  | _ ->
      if
        (* Check for color functions *)
        looking_at t "rgb(" || looking_at t "rgba("
      then (
        (* Parse rgb() or rgba() function - both legacy and modern syntax *)
        let _ = ident t in
        (* consume "rgb" or "rgba" *)
        expect t '(';
        ws t;

        (* Try space-separated first, then comma-separated *)
        match try_parse read_rgb_space_separated t with
        | Some result -> result
        | None -> read_rgb_comma_separated t)
      else if looking_at t "hsl(" then (
        expect_string t "hsl(";
        ws t;
        let hue = read_hue t in
        ws t;
        let s = read_percentage t in
        ws t;
        let l = read_percentage t in
        let a = read_optional_alpha t in
        ws t;
        expect t ')';
        Hsl { h = hue; s = Pct s; l = Pct l; a })
      else if looking_at t "hwb(" then (
        expect_string t "hwb(";
        ws t;
        let hue = read_hue t in
        ws t;
        let w = read_percentage t in
        ws t;
        let b = read_percentage t in
        let a = read_optional_alpha t in
        ws t;
        expect t ')';
        Hwb { h = hue; w = Pct w; b = Pct b; a })
      else if looking_at t "oklch(" then (
        expect_string t "oklch(";
        ws t;
        (* Oklch L must be a percentage per CSS spec *)
        let l = read_percentage t in
        ws t;
        let c = number t in
        ws t;
        let h = number t in
        let alpha = read_optional_alpha t in
        ws t;
        expect t ')';
        Oklch { l = Pct l; c; h = Unitless h; alpha })
      else if looking_at t "oklab(" then (
        expect_string t "oklab(";
        ws t;
        (* Oklab L must be a percentage per CSS spec *)
        let l = read_percentage t in
        ws t;
        let a = number t in
        ws t;
        let b = number t in
        let alpha = read_optional_alpha t in
        ws t;
        expect t ')';
        Oklab { l = Pct l; a; b; alpha })
      else if looking_at t "lch(" then (
        expect_string t "lch(";
        ws t;
        (* Lch L must be a percentage per CSS spec *)
        let l = read_percentage t in
        ws t;
        let c = number t in
        ws t;
        let h = number t in
        let alpha = read_optional_alpha t in
        ws t;
        expect t ')';
        Lch { l = Pct l; c; h = Unitless h; alpha })
      else if looking_at t "color(" then (
        expect_string t "color(";
        ws t;
        let space = read_color_space t in
        ws t;
        let components = read_color_components space t [] in
        let alpha = read_optional_alpha t in
        expect t ')';
        Color { space; components; alpha })
      else if looking_at t "var(" then
        (* CSS variable *)
        Var (read_var read_color t)
      else
        (* Color keyword or error *)
        read_color_keyword t

and read_color_keyword t : color =
  let keyword = ident t in
  let lower = String.lowercase_ascii keyword in
  match lower with
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
  | _ -> err_invalid t ("color: " ^ keyword)

(** Read an angle value *)
and read_angle t : angle =
  ws t;
  (* Check for var() *)
  if looking_at t "var(" then Var (read_var read_angle t)
  else
    let n = number t in
    let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
    match unit with
    | "deg" -> Deg n
    | "rad" -> Rad n
    | "turn" -> Turn n
    | "grad" -> Grad n
    | _ -> err_invalid t ("angle unit: " ^ unit)

(** Read a hue value (preserves unitless vs explicit angle) *)
and read_hue t : hue =
  ws t;
  (* Check for var() *)
  if looking_at t "var(" then Var (read_var read_hue t)
  else
    let n = number t in
    let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
    match unit with
    | "" -> Unitless n (* Unitless number, defaults to degrees *)
    | "deg" -> Angle (Deg n)
    | "rad" -> Angle (Rad n)
    | "turn" -> Angle (Turn n)
    | "grad" -> Angle (Grad n)
    | _ -> err_invalid t ("hue unit: " ^ unit)

(** Read a duration value *)
let rec read_duration t : duration =
  ws t;
  (* Check for var() *)
  if looking_at t "var(" then Var (read_var read_duration t)
  else
    let n = number t in
    let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
    match unit with
    | "s" -> S n
    | "ms" -> Ms n
    | _ -> err_invalid t ("duration unit: " ^ unit)

(** Read a number value *)
let rec read_number t : number =
  ws t;
  (* Check for var() *)
  if looking_at t "var(" then Var (read_var read_number t)
  else
    let n = number t in
    if n = float_of_int (int_of_float n) then Int (int_of_float n) else Float n

let rec read_calc_expr : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  ws t;
  let left = read_calc_term read_a t in
  ws t;
  match peek t with
  | Some '+' ->
      skip t;
      Expr (left, Add, read_calc_expr read_a t)
  | Some '-' ->
      skip t;
      Expr (left, Sub, read_calc_expr read_a t)
  | _ -> left

and read_calc_term : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  ws t;
  let left = read_calc_factor read_a t in
  ws t;
  match peek t with
  | Some '*' ->
      skip t;
      Expr (left, Mult, read_calc_term read_a t)
  | Some '/' ->
      skip t;
      Expr (left, Div, read_calc_term read_a t)
  | _ -> left

and read_calc_factor : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  ws t;
  if peek t = Some '(' then (
    skip t;
    let expr = read_calc_expr read_a t in
    ws t;
    expect t ')';
    expr)
  else read_calc read_a t

and read_calc : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  ws t;
  if looking_at t "calc(" then (
    expect_string t "calc(";
    (* skip "calc(" *)
    let expr = read_calc_expr read_a t in
    expect t ')';
    expr)
  else if looking_at t "var(" then (
    expect_string t "var(";
    (* skip "var(" *)
    ws t;
    let var_name =
      if looking_at t "--" then (
        expect_string t "--";
        ident t (* var_name should be without -- *))
      else ident t
    in
    ws t;
    let fallback =
      if peek t = Some ',' then (
        skip t;
        ws t;
        (* Parse the fallback length value *)
        Some (read_a t))
      else None
    in
    expect t ')';
    (* Create a length var with fallback *)
    let v = var_ref ?fallback var_name in
    Var v)
  else
    (* Try to parse with specific unit first, fall back to raw number *)
    let _pos = save t in
    try Val (read_a t)
    with Parse_error _ ->
      (* If parsing failed due to unit error, try parsing as raw number *)
      restore t;
      Num (number t)
