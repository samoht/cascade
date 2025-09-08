(** CSS Values & Units parsing using Reader API *)

include Values_intf

let var_ref ?fallback ?default ?layer ?meta name =
  { name; fallback; default; layer; meta }

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

(* Function call formatting now provided by Pp.call and Pp.call_list *)

let pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  Pp.call "calc"
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
      Pp.string ctx "/";
      pp_alpha ctx a

(** Pretty printer for percentage types *)
let rec pp_percentage : percentage Pp.t =
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

let pp_rgb ctx r g b alpha =
  Pp.string ctx "rgb(";
  pp_channel ctx r;
  Pp.space ctx ();
  pp_channel ctx g;
  Pp.space ctx ();
  pp_channel ctx b;
  pp_opt_alpha ctx alpha;
  Pp.char ctx ')'

let pp_oklch ctx l c h alpha =
  Pp.string ctx "oklch(";
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha;
  Pp.char ctx ')'

let pp_hsl ctx h s l a =
  Pp.string ctx "hsl(";
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx s;
  Pp.space ctx ();
  pp_percentage ctx l;
  pp_opt_alpha ctx a;
  Pp.char ctx ')'

let pp_hwb ctx h w b a =
  Pp.string ctx "hwb(";
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx w;
  Pp.space ctx ();
  pp_percentage ctx b;
  pp_opt_alpha ctx a;
  Pp.char ctx ')'

let pp_oklab ctx l a b alpha =
  Pp.string ctx "oklab(";
  (* Oklab L must always be output as percentage per CSS spec *)
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx a;
  Pp.space ctx ();
  Pp.float ctx b;
  pp_opt_alpha ctx alpha;
  Pp.char ctx ')'

let pp_lch ctx l c h alpha =
  Pp.string ctx "lch(";
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha;
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

and pp_color_mix ctx in_space hue color1 percent1 color2 percent2 =
  Pp.string ctx "color-mix(";
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
  pp_opt_alpha ctx alpha;
  Pp.char ctx ')'

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

(** var() parser after "var" ident has been consumed *)
let read_var_after_ident : type a. (Reader.t -> a) -> Reader.t -> a var =
 fun read_value t ->
  Reader.expect t '(';
  Reader.ws t;
  let var_name =
    if Reader.looking_at t "--" then (
      Reader.expect_string t "--";
      Reader.ident t)
    else Reader.ident t
  in
  Reader.ws t;
  let fallback =
    if Reader.peek t = Some ',' then (
      Reader.comma t;
      Some (read_value t))
    else None
  in
  Reader.ws t;
  Reader.expect t ')';
  var_ref ?fallback var_name

(** Generic var() parser that returns a var reference *)
let read_var : type a. (Reader.t -> a) -> Reader.t -> a var =
 fun read_value t ->
  Reader.expect_string t "var";
  read_var_after_ident read_value t

(** Read a CSS length value *)
let rec read_length t : length =
  Reader.ws t;
  (* Try to parse number first *)
  let num_opt = Reader.try_parse Reader.number t in
  match num_opt with
  | None -> (
      (* Try keyword values *)
      let keyword = Reader.ident t in
      match String.lowercase_ascii keyword with
      | "auto" -> Auto
      | "max-content" -> Max_content
      | "min-content" -> Min_content
      | "fit-content" -> Fit_content
      | "from-font" -> From_font
      | "inherit" -> Inherit
      | "var" -> Var (read_var_after_ident read_length t)
      | _ -> Reader.err_invalid t ("length keyword: " ^ keyword))
  | Some n -> (
      (* Check for unit *)
      let unit = Reader.while_ t (fun c -> (c >= 'a' && c <= 'z') || c = '%') in
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
      | _ -> Reader.err_invalid t ("length unit: " ^ unit))

(** Read a percentage value as float (number followed by %) *)
let read_percentage_float t : float =
  Reader.ws t;
  let n = Reader.number t in
  Reader.expect t '%';
  n

(** Read an alpha value *)
let rec read_alpha t : alpha =
  Reader.ws t;
  (* Check for var() first *)
  if Reader.looking_at t "var(" then Var (read_var read_alpha t)
  else
    (* Try percentage first *)
    match Reader.try_parse read_percentage_float t with
    | Some pct -> Pct pct
    | None -> Num (Reader.number t)
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
    | "%" -> Pct n
    | "" -> Int (int_of_float n)
    | _ -> Reader.err_invalid t "channel value"

let read_rgb_space_separated t : color =
  let r, g, b =
    Reader.triple ~sep:Reader.ws read_channel read_channel read_channel t
  in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect t ')';
  match alpha with
  | None -> Rgb { r; g; b }
  | Num _ | Pct _ | Var _ -> Rgba { r; g; b; a = alpha }

let read_rgb_comma_separated t : color =
  let r, g, b =
    Reader.triple ~sep:Reader.comma read_channel read_channel read_channel t
  in
  let alpha =
    if Reader.peek t = Some ',' then (
      Reader.comma t;
      read_alpha t)
    else None
  in
  Reader.ws t;
  Reader.expect t ')';
  match alpha with None -> Rgb { r; g; b } | a -> Rgba { r; g; b; a }

(** Read color space identifier *)
let read_color_space t : color_space =
  let space_ident = Reader.ident t |> String.lowercase_ascii in
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
            Reader.expect t '%';
            (Pct n : component)
        | _ -> (
            (* Try percentage first, then plain number *)
            match Reader.try_parse read_percentage_float t with
            | Some pct -> Pct pct
            | None -> Number (Reader.number t))
      in
      read_color_components space t (component :: acc)
  | None -> Reader.err_invalid t "color()"

(** Read hex color digits *)
let read_hex_color t =
  Reader.while_ t (fun c ->
      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

(** Read an angle value *)
let rec read_angle t : angle =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_angle t)
  else
    let n = Reader.number t in
    let unit = Reader.while_ t (fun c -> c >= 'a' && c <= 'z') in
    match unit with
    | "deg" -> Deg n
    | "rad" -> Rad n
    | "turn" -> Turn n
    | "grad" -> Grad n
    | _ -> Reader.err_invalid t ("angle unit: " ^ unit)

(** Read a hue value (preserves unitless vs explicit angle) *)
let rec read_hue t : hue =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_hue t)
  else
    let n = Reader.number t in
    let unit = Reader.while_ t (fun c -> c >= 'a' && c <= 'z') in
    match unit with
    | "" -> Unitless n (* Unitless number, defaults to degrees *)
    | "deg" -> Angle (Deg n)
    | "rad" -> Angle (Rad n)
    | "turn" -> Angle (Turn n)
    | "grad" -> Angle (Grad n)
    | _ -> Reader.err_invalid t ("hue unit: " ^ unit)

let read_hsl t : color =
  Reader.ws t;
  let hue = read_hue t in
  Reader.ws t;
  if Reader.peek t = Some ',' then (
    Reader.comma t;
    let s, l =
      Reader.pair ~sep:Reader.comma read_percentage_float read_percentage_float
        t
    in
    Reader.ws t;
    let a =
      if Reader.peek t = Some ',' then (
        Reader.comma t;
        read_alpha t)
      else None
    in
    Reader.ws t;
    Reader.expect t ')';
    Hsl { h = hue; s = Pct s; l = Pct l; a })
  else
    let s, l =
      Reader.pair ~sep:Reader.ws read_percentage_float read_percentage_float t
    in
    let a = read_optional_alpha t in
    Reader.ws t;
    Reader.expect t ')';
    Hsl { h = hue; s = Pct s; l = Pct l; a }

let read_hwb t : color =
  Reader.ws t;
  let hue = read_hue t in
  Reader.ws t;
  if Reader.peek t = Some ',' then (
    Reader.comma t;
    let w, b =
      Reader.pair ~sep:Reader.comma read_percentage_float read_percentage_float
        t
    in
    Reader.ws t;
    let a =
      if Reader.peek t = Some ',' then (
        Reader.comma t;
        read_alpha t)
      else None
    in
    Reader.ws t;
    Reader.expect t ')';
    Hwb { h = hue; w = Pct w; b = Pct b; a })
  else
    let w, b =
      Reader.pair ~sep:Reader.ws read_percentage_float read_percentage_float t
    in
    let a = read_optional_alpha t in
    Reader.ws t;
    Reader.expect t ')';
    Hwb { h = hue; w = Pct w; b = Pct b; a }

let read_oklch t : color =
  Reader.ws t;
  let l, c, h =
    Reader.triple ~sep:Reader.ws read_percentage_float Reader.number
      Reader.number t
  in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect t ')';
  Oklch { l = Pct l; c; h = Unitless h; alpha }

let read_oklab t : color =
  Reader.ws t;
  let l, a, b =
    Reader.triple ~sep:Reader.ws read_percentage_float Reader.number
      Reader.number t
  in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect t ')';
  Oklab { l = Pct l; a; b; alpha }

let read_lch t : color =
  Reader.ws t;
  let l, c, h =
    Reader.triple ~sep:Reader.ws read_percentage_float Reader.number
      Reader.number t
  in
  let alpha = read_optional_alpha t in
  Reader.ws t;
  Reader.expect t ')';
  Lch { l = Pct l; c; h = Unitless h; alpha }

let read_color_function t : color =
  Reader.ws t;
  let space = read_color_space t in
  Reader.ws t;
  let components = read_color_components space t [] in
  let alpha = read_optional_alpha t in
  Reader.expect t ')';
  Color { space; components; alpha }

let rec read_color t : color =
  Reader.ws t;
  match Reader.peek t with
  | Some '#' ->
      (* Hex color: #fff or #ffffff *)
      Reader.expect t '#';
      let hex = read_hex_color t in
      Hex { hash = true; value = hex }
  | _ ->
      if Reader.looking_at t "rgb(" || Reader.looking_at t "rgba(" then (
        let _ = Reader.ident t in
        (* consume "rgb" or "rgba" *)
        Reader.expect t '(';
        Reader.ws t;

        match Reader.try_parse read_rgb_space_separated t with
        | Some result -> result
        | None -> read_rgb_comma_separated t)
      else if Reader.looking_at t "hsl(" || Reader.looking_at t "hsla(" then (
        Reader.expect_string t
          (if Reader.looking_at t "hsla(" then "hsla(" else "hsl(");
        read_hsl t)
      else if Reader.looking_at t "hwb(" then (
        Reader.expect_string t "hwb(";
        read_hwb t)
      else if Reader.looking_at t "oklch(" then (
        Reader.expect_string t "oklch(";
        read_oklch t)
      else if Reader.looking_at t "oklab(" then (
        Reader.expect_string t "oklab(";
        read_oklab t)
      else if Reader.looking_at t "lch(" then (
        Reader.expect_string t "lch(";
        read_lch t)
      else if Reader.looking_at t "color(" then (
        Reader.expect_string t "color(";
        read_color_function t)
      else if Reader.looking_at t "var(" then
        (* CSS variable *)
        Var (read_var read_color t)
      else read_color_keyword t

and read_color_keyword t : color =
  let keyword = Reader.ident t in
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
  | _ -> Reader.err_invalid t ("color: " ^ keyword)

(** Read a duration value *)
let rec read_duration t : duration =
  Reader.ws t;
  (* Check for var() *)
  if Reader.looking_at t "var(" then Var (read_var read_duration t)
  else
    let n = Reader.number t in
    let unit = Reader.while_ t (fun c -> c >= 'a' && c <= 'z') in
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
      Expr (left, Mult, read_calc_term read_a t)
  | Some '/' ->
      Reader.skip t;
      Expr (left, Div, read_calc_term read_a t)
  | _ -> left

and read_calc_factor : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.ws t;
  if Reader.peek t = Some '(' then (
    Reader.skip t;
    let expr = read_calc_expr read_a t in
    Reader.ws t;
    Reader.expect t ')';
    expr)
  else read_calc read_a t

and read_calc : type a. (Reader.t -> a) -> Reader.t -> a calc =
 fun read_a t ->
  Reader.ws t;
  if Reader.looking_at t "calc(" then (
    Reader.expect_string t "calc(";
    let expr = read_calc_expr read_a t in
    Reader.expect t ')';
    expr)
  else if Reader.looking_at t "var(" then (
    Reader.expect_string t "var(";
    Reader.ws t;
    let var_name =
      if Reader.looking_at t "--" then (
        Reader.expect_string t "--";
        Reader.ident t (* var_name should be without -- *))
      else Reader.ident t
    in
    Reader.ws t;
    let fallback =
      if Reader.peek t = Some ',' then (
        Reader.comma t;
        (* Parse the fallback length value *)
        Some (read_a t))
      else None
    in
    Reader.expect t ')';
    (* Create a length var with fallback *)
    let v = var_ref ?fallback var_name in
    Var v)
  else
    (* Try to parse with specific unit first, fall back to raw number *)
    let _pos = Reader.save t in
    try Val (read_a t)
    with Reader.Parse_error _ ->
      (* If parsing failed due to unit error, try parsing as raw number *)
      Reader.restore t;
      Num (Reader.number t)

(** Read a percentage type with var() and calc() support *)
let rec read_percentage t : percentage =
  Reader.ws t;
  if Reader.looking_at t "var(" then Var (read_var read_percentage t)
  else if Reader.looking_at t "calc(" then Calc (read_calc read_percentage t)
  else
    let n = Reader.number t in
    Reader.expect t '%';
    Pct n

(* Var helper functions *)
let var_name v = v.name
let var_layer v = v.layer
