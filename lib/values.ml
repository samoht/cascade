(** CSS Values & Units parsing using Reader API *)

include Values_intf

let var_ref ?fallback ?default ?layer ?meta name =
  let fallback : _ fallback =
    match fallback with None -> None | Some x -> x
  in
  { name; fallback; default; layer; meta }

(** Color constructors *)
let hex s =
  let len = String.length s in
  if len > 0 && s.[0] = '#' then
    Hex { hash = true; value = String.sub s 1 (len - 1) }
  else Hex { hash = false; value = s }

let rgb ?alpha r g b =
  match alpha with
  | None -> Rgb (Channels { r = Int r; g = Int g; b = Int b })
  | Some a ->
      Rgba { rgb = Channels { r = Int r; g = Int g; b = Int b }; a = Num a }

let hsl h s l = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = None }
let hsla h s l a = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = Num a }
let hwb h w b = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = None }
let hwba h w b a = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = Num a }
let oklch l c h = Oklch { l = Pct l; c; h = Unitless h; alpha = None }
let oklcha l c h a = Oklch { l = Pct l; c; h = Unitless h; alpha = Num a }
let oklab l a b = Oklab { l = Pct l; a = Some a; b = Some b; alpha = None }

let oklaba l a b alpha =
  Oklab { l = Pct l; a = Some a; b = Some b; alpha = Num alpha }

let oklaba_none_zeros l a b alpha =
  let a = if a = 0.0 then Stdlib.Option.None else Stdlib.Option.Some a in
  let b = if b = 0.0 then Stdlib.Option.None else Stdlib.Option.Some b in
  Oklab { l = Pct l; a; b; alpha = Num alpha }

let lch l c h = Lch { l = Pct l; c; h = Unitless h; alpha = None }
let lcha l c h a = Lch { l = Pct l; c; h = Unitless h; alpha = Num a }
let color_name n = Named n
let current_color = Current
let transparent = Transparent

let color_mix ?in_space ?(hue = Default) ?percent1 ?percent2 color1 color2 =
  let percent1 : percentage option =
    match percent1 with Some p -> Some (Pct p) | None -> None
  in
  let percent2 : percentage option =
    match percent2 with Some p -> Some (Pct p) | None -> None
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 }

let color_mix_var_percent ?in_space ?(hue = Default) ~var_name color1 color2 =
  let percent1 : percentage option =
    Some
      (Var
         {
           name = var_name;
           fallback = None;
           default = None;
           layer = None;
           meta = None;
         })
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 = None }

let color_mix_var_pct_fallback ?in_space ?(hue = Default) ~var_name ~fallback
    color1 color2 =
  let percent1 : percentage option =
    Some
      (Var
         {
           name = var_name;
           fallback;
           default = None;
           layer = None;
           meta = None;
         })
  in
  Mix { in_space; hue; color1; percent1; color2; percent2 = None }

(** Comparison functions *)

(** Pretty-printing functions *)

let is_theme_var v = v.layer = Some "theme"
let in_theme = Pp.in_theme

let pp_var_fallback ctx fallback_name =
  if in_theme ctx fallback_name then (
    Pp.string ctx "var(--";
    Pp.string ctx fallback_name;
    Pp.string ctx "))")
  else
    match ctx.theme_defaults fallback_name with
    | Some resolved ->
        Pp.string ctx resolved;
        Pp.char ctx ')'
    | Option.None ->
        Pp.string ctx "var(--";
        Pp.string ctx fallback_name;
        Pp.string ctx "))"

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  let emit_var_ref () =
    Pp.string ctx "var(--";
    Pp.string ctx v.name;
    Pp.char ctx ')'
  in
  if ctx.inline then
    match v.default with
    | Some value -> pp_value ctx value
    | Option.None -> (
        match v.fallback with
        | Fallback value -> pp_value ctx value
        | Var_fallback fallback_name -> (
            match ctx.theme_defaults fallback_name with
            | Some resolved -> Pp.string ctx resolved
            | Option.None -> emit_var_ref ())
        | None | Empty | Empty2 -> emit_var_ref ())
  else
    match v.fallback with
    | None -> (
        if (not (is_theme_var v)) || in_theme ctx v.name then emit_var_ref ()
        else
          match v.default with
          | Some value -> pp_value ctx value
          | Option.None -> emit_var_ref ())
    | Empty ->
        Pp.string ctx "var(--";
        Pp.string ctx v.name;
        Pp.char ctx ',';
        Pp.char ctx ')'
    | Empty2 ->
        Pp.string ctx "var(--";
        Pp.string ctx v.name;
        Pp.string ctx ",  )"
    | Fallback value ->
        Pp.string ctx "var(--";
        Pp.string ctx v.name;
        Pp.comma ctx ();
        pp_value { ctx with in_function = true } value;
        Pp.char ctx ')'
    | Var_fallback fallback_name ->
        Pp.string ctx "var(--";
        Pp.string ctx v.name;
        Pp.comma ctx ();
        pp_var_fallback ctx fallback_name

(* Function call formatting now provided by Pp.call and Pp.call_list *)

(** Pretty print calc_op *)
let pp_calc_op : calc_op Pp.t =
 fun ctx op ->
  match op with
  | Add -> Pp.string ctx " + "
  | Sub -> Pp.string ctx " - "
  | Mul ->
      Pp.space_if_pretty ctx ();
      Pp.string ctx "*";
      Pp.space_if_pretty ctx ()
  | Div ->
      Pp.space_if_pretty ctx ();
      Pp.string ctx "/";
      Pp.space_if_pretty ctx ()

let pp_calc_contents : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  let precedence = function Add | Sub -> 1 | Mul | Div -> 2 in
  (* Print a calc expression, tracking parent precedence and whether we're on
     the right side of a non-commutative operator (Sub/Div) *)
  let rec pp_calc_inner ~parent_prec ~right_of_noncommut ctx = function
    | Val v -> pp_value ctx v
    | Var v -> pp_var pp_value ctx v
    | Num n -> Pp.float ctx n
    | Sibling_index -> Pp.string ctx "sibling-index()"
    | Sibling_count -> Pp.string ctx "sibling-count()"
    | Nested inner ->
        (* Preserve nested calc() exactly as written to match Tailwind's output
           format, e.g. calc(4px * calc(1 - var(--tw-reverse))) *)
        Pp.call "calc"
          (fun ctx inner ->
            pp_calc_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner)
          ctx inner
    | Parens inner ->
        (* Parenthesized expression - render as (inner) *)
        Pp.char ctx '(';
        pp_calc_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner;
        Pp.char ctx ')'
    | Expr (left, op, right) ->
        let op_prec = precedence op in
        (* Need parens if: - Our precedence is lower than parent (standard) - OR
           we're on the right of a non-commutative op (Sub/Div) and our op has
           same or lower precedence *)
        let needs_parens =
          op_prec < parent_prec || (right_of_noncommut && op_prec <= parent_prec)
        in
        if needs_parens then Pp.char ctx '(';
        pp_calc_inner ~parent_prec:op_prec ~right_of_noncommut:false ctx left;
        pp_calc_op ctx op;
        (* Right side of Sub/Div needs special handling *)
        let is_noncommut = match op with Sub | Div -> true | _ -> false in
        pp_calc_inner ~parent_prec:op_prec ~right_of_noncommut:is_noncommut ctx
          right;
        if needs_parens then Pp.char ctx ')'
  in
  pp_calc_inner ~parent_prec:0 ~right_of_noncommut:false ctx calc

let pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc -> Pp.call "calc" (pp_calc_contents pp_value) ctx calc

(* Small helpers *)
let pp_unit ?(always = true) ctx f suffix =
  if f = 0. && not always then Pp.char ctx '0'
  else (
    (* Always drop leading zeros for CSS unit values (e.g., .25rem not 0.25rem)
       to match Tailwind's output format *)
    Pp.string ctx (Pp.float_to_string ~drop_leading_zero:true f);
    Pp.string ctx suffix)

(** Try to evaluate a calc expression containing only numbers to a float.
    Returns None if the expression contains variables or non-numeric values. *)
let rec eval_numeric_calc : type a. a calc -> float option = function
  | Num f -> Some f
  | Sibling_index -> None
  | Sibling_count -> None
  | Val _ -> None (* Can't evaluate typed values *)
  | Var _ -> None (* Can't evaluate variables *)
  | Nested inner -> eval_numeric_calc inner
  | Parens inner -> eval_numeric_calc inner
  | Expr (left, op, right) -> (
      match (eval_numeric_calc left, eval_numeric_calc right) with
      | Some l, Some r -> (
          match op with
          | Add -> Some (l +. r)
          | Sub -> Some (l -. r)
          | Mul -> Some (l *. r)
          | Div when r <> 0.0 -> Some (l /. r)
          | Div -> None)
      | _ -> None)

(* Count the comma-separated argument groups in [s], ignoring commas inside
   nested function calls / brackets. Used by math-function readers to validate
   arity (clamp wants 3, minmax 2, min/max >= 1). *)
let top_level_arg_count s =
  let depth = ref 0 in
  let groups = ref 1 in
  let saw_any = ref false in
  String.iter
    (fun c ->
      match c with
      | '(' | '[' | '{' ->
          incr depth;
          saw_any := true
      | ')' | ']' | '}' -> decr depth
      | ',' when !depth = 0 -> incr groups
      | ' ' | '\t' | '\n' | '\r' -> ()
      | _ -> saw_any := true)
    s;
  if !saw_any then !groups else 0

(* Top-level commas in math-function args round-trip differently in pretty vs
   minified mode: minified strips space after comma, pretty inserts ", ". Walk
   the raw arg string with a paren-depth counter so commas inside nested calls
   are left untouched. *)
let pp_math_call ctx name args =
  Pp.string ctx name;
  Pp.char ctx '(';
  let buf = ctx.Pp.buf in
  let depth = ref 0 in
  let after_comma = ref false in
  let minify = ctx.Pp.minify in
  String.iter
    (fun c ->
      match c with
      | '(' ->
          after_comma := false;
          incr depth;
          Buffer.add_char buf c
      | ')' ->
          after_comma := false;
          decr depth;
          Buffer.add_char buf c
      | ',' when !depth = 0 ->
          after_comma := true;
          Buffer.add_char buf ',';
          if not minify then Buffer.add_char buf ' '
      | ' ' when !after_comma -> ()
      | _ ->
          after_comma := false;
          Buffer.add_char buf c)
    args;
  Pp.char ctx ')'

let normalize_math_args args =
  let buf = Buffer.create (String.length args) in
  let depth = ref 0 in
  let after_comma = ref false in
  String.iter
    (fun c ->
      match c with
      | '(' ->
          after_comma := false;
          incr depth;
          Buffer.add_char buf c
      | ')' ->
          after_comma := false;
          decr depth;
          Buffer.add_char buf c
      | ',' when !depth = 0 ->
          after_comma := true;
          Buffer.add_char buf ','
      | ' ' when !after_comma -> ()
      | _ ->
          after_comma := false;
          Buffer.add_char buf c)
    args;
  Buffer.contents buf

let rec pp_length ?(always = false) : length Pp.t =
 fun ctx v ->
  let pp_unit_fn = pp_unit ~always ctx in
  match v with
  | Zero -> Pp.char ctx '0'
  | Px f -> pp_unit_fn f "px"
  | Cm f -> pp_unit_fn f "cm"
  | Mm f -> pp_unit_fn f "mm"
  | Q f -> pp_unit_fn f "q"
  | In f -> pp_unit_fn f "in"
  | Pt f -> pp_unit_fn f "pt"
  | Pc f -> pp_unit_fn f "pc"
  | Rem f -> pp_unit_fn f "rem"
  | Em f -> pp_unit_fn f "em"
  | Ex f -> pp_unit_fn f "ex"
  | Cap f -> pp_unit_fn f "cap"
  | Ic f -> pp_unit_fn f "ic"
  | Rlh f -> pp_unit_fn f "rlh"
  | Pct f -> pp_unit_fn f "%"
  | Vw f -> pp_unit_fn f "vw"
  | Vh f -> pp_unit_fn f "vh"
  | Vmin f -> pp_unit_fn f "vmin"
  | Vmax f -> pp_unit_fn f "vmax"
  | Vi f -> pp_unit_fn f "vi"
  | Vb f -> pp_unit_fn f "vb"
  | Dvh f -> pp_unit_fn f "dvh"
  | Dvw f -> pp_unit_fn f "dvw"
  | Dvmin f -> pp_unit_fn f "dvmin"
  | Dvmax f -> pp_unit_fn f "dvmax"
  | Lvh f -> pp_unit_fn f "lvh"
  | Lvw f -> pp_unit_fn f "lvw"
  | Lvmin f -> pp_unit_fn f "lvmin"
  | Lvmax f -> pp_unit_fn f "lvmax"
  | Svh f -> pp_unit_fn f "svh"
  | Svw f -> pp_unit_fn f "svw"
  | Svmin f -> pp_unit_fn f "svmin"
  | Svmax f -> pp_unit_fn f "svmax"
  | Cqw f -> pp_unit_fn f "cqw"
  | Cqh f -> pp_unit_fn f "cqh"
  | Cqi f -> pp_unit_fn f "cqi"
  | Cqb f -> pp_unit_fn f "cqb"
  | Cqmin f -> pp_unit_fn f "cqmin"
  | Cqmax f -> pp_unit_fn f "cqmax"
  | Ch f -> pp_unit_fn f "ch"
  | Lh f -> pp_unit_fn f "lh"
  | Size -> Pp.string ctx "size"
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Fit_content -> Pp.string ctx "fit-content"
  | Fit_content_arg arg ->
      Pp.string ctx "fit-content(";
      pp_length ~always ctx arg;
      Pp.char ctx ')'
  | Contain -> Pp.string ctx "contain"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | From_font -> Pp.string ctx "from-font"
  | Stretch -> Pp.string ctx "stretch"
  | Clamp s -> pp_math_call ctx "clamp" s
  | Min s -> pp_math_call ctx "min" s
  | Max s -> pp_math_call ctx "max" s
  | Minmax s -> pp_math_call ctx "minmax" s
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          Pp.string ctx strategy;
          Pp.comma ctx ();
          pp_length ~always ctx value;
          Pp.comma ctx ();
          pp_length ~always ctx step)
        ctx (strategy, value, step)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_length ~always ctx a;
          Pp.comma ctx ();
          pp_length ~always ctx b)
        ctx (a, b)
  | Rem_fn (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_length ~always ctx a;
          Pp.comma ctx ();
          pp_length ~always ctx b)
        ctx (a, b)
  | Hypot (a, b) ->
      Pp.call "hypot"
        (fun ctx (a, b) ->
          pp_length ~always ctx a;
          Pp.comma ctx ();
          pp_length ~always ctx b)
        ctx (a, b)
  | Abs v -> Pp.call "abs" (pp_length ~always) ctx v
  | Sign v -> Pp.call "sign" (pp_length ~always) ctx v
  | Calc_size (basis, calc) ->
      Pp.call "calc-size"
        (fun ctx (basis, calc) ->
          pp_length ~always ctx basis;
          Pp.comma ctx ();
          pp_calc_contents (pp_length ~always) ctx calc)
        ctx (basis, calc)
  | Anchor_size size ->
      Pp.call "anchor-size" (fun ctx size -> Pp.string ctx size) ctx size
  | Anchor (name, side, fallback) ->
      Pp.call "anchor"
        (fun ctx (name, side, fallback) ->
          Option.iter
            (fun name ->
              Pp.string ctx name;
              Pp.space ctx ())
            name;
          Pp.string ctx side;
          match fallback with
          | Option.None -> ()
          | Option.Some fallback ->
              Pp.comma ctx ();
              pp_length ~always ctx fallback)
        ctx (name, side, fallback)
  | Var v -> pp_var (pp_length ~always) ctx v
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Content -> Pp.string ctx "content"
  | Calc cv -> (
      (* Optimize calc(infinity * dimension) to large value - Tailwind always
         outputs this optimized form regardless of minification *)
      match cv with
      | Expr (Num f, Mul, Val _) when f = infinity ->
          Pp.string ctx "3.40282e38px"
      | Expr (Val _, Mul, Num f) when f = infinity ->
          Pp.string ctx "3.40282e38px"
      | _ -> pp_calc (pp_length ~always) ctx cv)

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

(** Convert a named color to its hex equivalent (name, hex_value). Returns the
    shortest representation matching Lightning CSS behavior. *)
let color_name_hex : color_name -> string * string = function
  | Red -> ("red", "f00")
  | Blue -> ("blue", "00f")
  | Green -> ("green", "008000")
  | White -> ("white", "fff")
  | Black -> ("black", "000")
  | Yellow -> ("yellow", "ff0")
  | Cyan -> ("cyan", "0ff")
  | Magenta -> ("magenta", "f0f")
  | Gray -> ("gray", "808080")
  | Grey -> ("grey", "808080")
  | Orange -> ("orange", "ffa500")
  | Purple -> ("purple", "800080")
  | Pink -> ("pink", "ffc0cb")
  | Silver -> ("silver", "c0c0c0")
  | Maroon -> ("maroon", "800000")
  | Fuchsia -> ("fuchsia", "f0f")
  | Lime -> ("lime", "0f0")
  | Olive -> ("olive", "808000")
  | Navy -> ("navy", "000080")
  | Teal -> ("teal", "008080")
  | Aqua -> ("aqua", "0ff")
  | _ ->
      (* Extended named colors are always longer than 4 chars, so hex form
         (#rrggbb = 7 chars) is never shorter. Keep name. *)
      ("extended", "")

(** Minify a color value by converting named colors to hex when shorter,
    matching Lightning CSS behavior. *)
let shorten_hex value =
  let len = String.length value in
  (* #RRGGBB → #RGB when R=R, G=G, B=B *)
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
    Bytes.to_string s (* #RRGGBBAA → #RGBA when R=R, G=G, B=B, A=A *))
  else if
    len = 8
    && value.[0] = value.[1]
    && value.[2] = value.[3]
    && value.[4] = value.[5]
    && value.[6] = value.[7]
  then (
    if
      (* Further shorten #RGBA → #RGB when A=f (fully opaque) *)
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
    (* #RRGGBBFF → #RRGGBB when fully opaque *)
    len = 8
    && (value.[6] = 'f' || value.[6] = 'F')
    && (value.[7] = 'f' || value.[7] = 'F')
  then String.sub value 0 6
  else if
    (* #RGBA → #RGB when A=f (fully opaque) *)
    len = 4 && (value.[3] = 'f' || value.[3] = 'F')
  then String.sub value 0 3
  else value

let minify_color : color -> color = function
  | Named n ->
      let name, hex = color_name_hex n in
      let hex_len =
        String.length hex + 1
        (* # prefix *)
      in
      if hex <> "" && hex_len <= String.length name then
        Hex { hash = true; value = shorten_hex hex }
      else Named n
  | Hex h -> Hex { h with value = shorten_hex h.value }
  | c -> c

let pp_system_color : system_color Pp.t =
 fun ctx -> function
  | Accent_color -> Pp.string ctx "AccentColor"
  | Accent_color_text -> Pp.string ctx "AccentColorText"
  | Active_text -> Pp.string ctx "ActiveText"
  | Button_border -> Pp.string ctx "ButtonBorder"
  | Button_face -> Pp.string ctx "ButtonFace"
  | Button_text -> Pp.string ctx "buttontext"
  | Canvas -> Pp.string ctx "Canvas"
  | Canvas_text -> Pp.string ctx "CanvasText"
  | Field -> Pp.string ctx "Field"
  | Field_text -> Pp.string ctx "FieldText"
  | Gray_text -> Pp.string ctx "GrayText"
  | Highlight -> Pp.string ctx "Highlight"
  | Highlight_text -> Pp.string ctx "HighlightText"
  | Link_text -> Pp.string ctx "LinkText"
  | Mark -> Pp.string ctx "Mark"
  | Mark_text -> Pp.string ctx "MarkText"
  | Selected_item -> Pp.string ctx "SelectedItem"
  | Selected_item_text -> Pp.string ctx "SelectedItemText"
  | Visited_text -> Pp.string ctx "VisitedText"
  | Webkit_focus_ring_color -> Pp.string ctx "-webkit-focus-ring-color"

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
  | Calc c -> pp_calc pp_angle ctx c
  | Var v -> pp_var pp_angle ctx v

let rec pp_hue : hue Pp.t =
 fun ctx -> function
  | Unitless f -> Pp.float ctx f
  | Angle (Deg f) when ctx.minify ->
      (* During minification, omit 'deg' since it's the default unit *)
      Pp.float ctx f
  | Angle a -> pp_angle ctx a
  | Var v -> pp_var pp_hue ctx v
  | Hue_none -> Pp.string ctx "none"

and pp_alpha : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> Pp.float ctx f
  | Pct f ->
      (* Keep percentage with % sign. CSS spec allows both decimal and
         percentage for alpha values. Tailwind doesn't optimize alpha
         percentages to decimals, so we follow the same approach for
         consistency. *)
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_alpha ctx v

(* Helper to print optional alpha with the correct leading separator *)
let pp_opt_alpha ctx = function
  | None -> ()
  | (Num _ | Pct _ | Var _) as a ->
      Pp.op_char ctx '/';
      pp_alpha ctx a

(** Pretty printer for percentage types *)
let rec pp_percentage ?(always = false) : percentage Pp.t =
 fun ctx -> function
  | Pct f -> Pp.pct ~always ctx f
  | Num f -> Pp.float_compact ctx f
  | Var v -> pp_var (pp_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_percentage ~always) ctx c

and pp_length_percentage ?(always = false) : length_percentage Pp.t =
 fun ctx -> function
  | Length l -> pp_length ~always ctx l
  | Pct f -> Pp.pct ~always ctx f
  | Var v -> pp_var (pp_length_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_length_percentage ~always) ctx c

and pp_number_percentage ?(always = false) : number_percentage Pp.t =
 fun ctx -> function
  | Num f -> Pp.float_compact ctx f
  | Pct f -> Pp.pct ~always ctx f
  | Var v -> pp_var (pp_number_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_number_percentage ~always) ctx c

and pp_component : component Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Angle h -> pp_hue ctx h
  | Var v -> pp_var pp_component ctx v
  | Calc c -> pp_calc pp_component ctx c
  | Component_none -> Pp.string ctx "none"

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

let pp_rgb_func = Pp.call "rgb" pp_rgb_args

let rec pp_rgb : rgb Pp.t =
 fun ctx -> function
  | Channels { r; g; b } -> Pp.list ~sep:Pp.space pp_channel ctx [ r; g; b ]
  | Var v -> pp_var pp_rgb ctx v

let pp_pct_num_hue_alpha : (percentage * float * hue * alpha) Pp.t =
 fun ctx (l, c, h, alpha) ->
  pp_percentage ctx l;
  Pp.space ctx ();
  Pp.float ctx c;
  Pp.space ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha

let pp_oklch = Pp.call "oklch" pp_pct_num_hue_alpha

let pp_hue_pct_pct_alpha : (hue * percentage * percentage * alpha) Pp.t =
 fun ctx (h, s, l, a) ->
  pp_hue ctx h;
  Pp.space ctx ();
  pp_percentage ctx s;
  Pp.space ctx ();
  pp_percentage ctx l;
  pp_opt_alpha ctx a

let pp_hsl = Pp.call "hsl" pp_hue_pct_pct_alpha
let pp_hwb = Pp.call "hwb" pp_hue_pct_pct_alpha

(** Print a float always dropping leading zeros (for oklab a/b values) *)
let pp_float_drop_zero ctx f =
  Buffer.add_string ctx.Pp.buf (Pp.float_to_string ~drop_leading_zero:true f)

let pp_alpha_drop_zero : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> pp_float_drop_zero ctx f
  | Pct f ->
      pp_float_drop_zero ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_alpha ctx v

(** Oklab-specific float printer with precision control. Non-minified: fixed
    decimal places (matching upstream Tailwind test expectations). Minified: 6
    significant digits (matching Tailwind's minified output). *)
let pp_oklab_float ~max_decimals ctx f =
  if ctx.Pp.minify then pp_float_drop_zero ctx (Pp.round_sig 6 f)
  else
    Buffer.add_string ctx.Pp.buf
      (Pp.float_to_string ~drop_leading_zero:true ~max_decimals f)

let pp_oklab_ab ctx = function
  | Some f -> pp_oklab_float ~max_decimals:3 ctx f
  | None -> Pp.string ctx "none"

let pp_oklab_args : (percentage * float option * float option * alpha) Pp.t =
 fun ctx (l, a, b, alpha) ->
  (* Oklab L: percentage with controlled precision *)
  (match l with
  | Pct f ->
      pp_oklab_float ~max_decimals:4 ctx f;
      Pp.char ctx '%'
  | _ -> pp_percentage ctx l);
  Pp.space ctx ();
  pp_oklab_ab ctx a;
  Pp.space ctx ();
  pp_oklab_ab ctx b;
  match alpha with
  | None -> ()
  | a ->
      Pp.op_char ctx '/';
      pp_alpha_drop_zero ctx a

let pp_lab = Pp.call "lab" pp_oklab_args
let pp_oklab = Pp.call "oklab" pp_oklab_args
let pp_lch = Pp.call "lch" pp_pct_num_hue_alpha

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
  | Rgb rgb -> (
      match rgb with
      | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
      | Var v ->
          (* Print as a var that expands to a color *)
          let rec pp_rgb_as_color : rgb Pp.t =
           fun ctx rgb ->
            match rgb with
            | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
            | Var v -> pp_var pp_rgb_as_color ctx v
          in
          pp_rgb_as_color ctx (Var v))
  | Rgba { rgb; a } -> (
      match rgb with
      | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, a)
      | Var v ->
          (* Output as rgb(var(--color)/alpha) *)
          let rec pp_rgb_var : rgb Pp.t =
           fun ctx rgb ->
            match rgb with
            | Channels { r; g; b } -> pp_rgb_func ctx (r, g, b, None)
            | Var v -> pp_var pp_rgb_var ctx v
          in
          Pp.call "rgb"
            (fun ctx (v, a) ->
              pp_rgb_var ctx (Var v);
              pp_opt_alpha ctx a)
            ctx (v, a))
  | Hsl { h; s; l; a } -> pp_hsl ctx (h, s, l, a)
  | Hwb { h; w; b; a } -> pp_hwb ctx (h, w, b, a)
  | Color { space; components; alpha } -> pp_color' ctx space components alpha
  | Relative_rgb body ->
      Pp.call "rgb" (fun ctx body -> Pp.string ctx body) ctx body
  | Contrast_color color -> Pp.call "contrast-color" pp_color ctx color
  | Light_dark (light, dark) ->
      Pp.call "light-dark"
        (fun ctx (light, dark) ->
          pp_color ctx light;
          Pp.comma ctx ();
          pp_color ctx dark)
        ctx (light, dark)
  | Lab { l; a; b; alpha } -> pp_lab ctx (l, a, b, alpha)
  | Oklch { l; c; h; alpha } -> pp_oklch ctx (l, c, h, alpha)
  | Oklab { l; a; b; alpha } -> pp_oklab ctx (l, a, b, alpha)
  | Lch { l; c; h; alpha } -> pp_lch ctx (l, c, h, alpha)
  | Named name -> pp_color_name ctx name
  | System sc -> pp_system_color ctx sc
  | Var v -> pp_var pp_color ctx v
  | Current ->
      Pp.string ctx (if ctx.in_function then "currentcolor" else "currentColor")
  | Transparent -> Pp.string ctx "transparent"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space hue color1 percent1 color2 percent2

(* CSS Values 4 §6.3: [ms] and [s] are interchangeable; pick the shorter
   spelling when minifying. The "s" suffix is one character shorter than "ms",
   so a millisecond value collapses to seconds when its second-form digits are
   no longer than the millisecond-form digits. *)
let pp_duration_unit ?(shorten_ms = true) ctx f suffix =
  if f = 0. then Pp.string ctx "0"
  else if (not shorten_ms) || (not ctx.Pp.minify) || suffix <> "ms" then
    pp_unit ctx f suffix
  else
    let in_seconds = f /. 1000. in
    let ms_str = Pp.float_to_string ~drop_leading_zero:true f in
    let s_str = Pp.float_to_string ~drop_leading_zero:true in_seconds in
    if String.length s_str <= String.length ms_str then (
      Pp.string ctx s_str;
      Pp.string ctx "s")
    else (
      Pp.string ctx ms_str;
      Pp.string ctx "ms")

let rec pp_duration : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Var v -> pp_var pp_duration ctx v
  | Calc c -> pp_calc pp_duration_in_calc ctx c

and pp_duration_in_calc : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ~shorten_ms:false ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Var v -> pp_var pp_duration ctx v
  | Calc c -> pp_calc pp_duration_in_calc ctx c

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Var v -> pp_var pp_number ctx v
  | Calc c -> pp_calc pp_number ctx c
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          Pp.string ctx strategy;
          Pp.comma ctx ();
          pp_number ctx value;
          Pp.comma ctx ();
          pp_number ctx step)
        ctx (strategy, value, step)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Hypot (a, b) ->
      Pp.call "hypot"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Pow (a, b) ->
      Pp.call "pow"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Sqrt v -> Pp.call "sqrt" pp_number ctx v
  | Abs v -> Pp.call "abs" pp_number ctx v
  | Sign v -> Pp.call "sign" pp_number ctx v
  | Sin a -> Pp.call "sin" pp_angle ctx a

let pp_transition_behavior : transition_behavior Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Allow_discrete -> Pp.string ctx "allow-discrete"

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

  (* Wrap an expression in an explicit nested calc() *)
  let nested inner = Nested inner

  (* Wrap an expression in parentheses only *)
  let parens inner = Parens inner
end

(** Read the body of a var(name[, fallback]) call, given a cursor over the
    function arguments. *)
let read_var_body : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  (* Strip the leading [--] from the dashed-ident per css-variables-1. *)
  let var_name =
    if String.length name >= 2 && name.[0] = '-' && name.[1] = '-' then
      String.sub name 2 (String.length name - 2)
    else name
  in
  Cursor.ws t;
  let fallback : _ fallback =
    if not (Cursor.comma_opt t) then None
    else (
      Cursor.ws t;
      if Cursor.is_done t then Empty
      else
        match Cursor.try_parse_full_err read_value t with
        | Ok fb -> Fallback fb
        | Error msg -> Cursor.err_invalid t ("var fallback: " ^ msg))
  in
  var_ref ~fallback var_name

(** Generic [var(...)] parser. Consumes the [var(...)] [Func] and applies
    [read_value] to the fallback, if any. *)
let read_var : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.call "var" t (fun inner -> read_var_body read_value inner)

(** [read_var] consumes the [var(...)] [Func]; this alias matches the pre-port
    entry point and is kept so call sites stay source-compatible. *)
let read_var_after_ident = read_var

let read_length_unit ?(allow_negative = true) t =
  let n, unit_raw = Cursor.number_with_unit t in
  if (not allow_negative) && n < 0.0 then Cursor.err_invalid t "negative";
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  match unit with
  | "" when n = 0.0 -> Zero
  | "" -> Cursor.err t "length values must have units (except for zero)"
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
  | "cqw" -> Cqw n
  | "cqh" -> Cqh n
  | "cqi" -> Cqi n
  | "cqb" -> Cqb n
  | "cqmin" -> Cqmin n
  | "cqmax" -> Cqmax n
  | "ch" -> Ch n
  | "lh" -> Lh n
  | "%" -> Pct n
  | _ -> Cursor.err_invalid t ("length unit: " ^ unit)

let read_length_keyword t : length =
  Cursor.enum "length"
    [
      ("auto", (Auto : length));
      ("none", None);
      ("size", Size);
      ("max-content", Max_content);
      ("min-content", Min_content);
      ("fit-content", Fit_content);
      ("contain", Contain);
      ("stretch", Stretch);
      ("from-font", From_font);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    t

let rec read_calc_expr : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  let left = read_calc_term read_a t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '+' ->
      (* Use atomic to ensure we either parse the full addition or nothing *)
      Cursor.atomic t (fun () ->
          Cursor.skip t;
          Expr (left, Add, read_calc_expr read_a t))
  | Some '-' ->
      (* Use atomic to ensure we either parse the full subtraction or nothing *)
      Cursor.atomic t (fun () ->
          Cursor.skip t;
          Expr (left, Sub, read_calc_expr read_a t))
  | _ -> left

and read_calc_term : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  let rec loop left =
    Cursor.ws t;
    match Cursor.peek_delim t with
    | Some '*' ->
        (* Use atomic to ensure we either parse the full multiplication or
           nothing *)
        Cursor.atomic t (fun () ->
            Cursor.skip t;
            Cursor.ws t;
            let right = read_calc_factor read_a t in
            (* Validate multiplication: can't multiply two raw dimensions (but
               expressions are OK) *)
            let is_dimension : type a. a calc -> bool = function
              | Val _ -> true
              | _ -> false
            in
            (* Allow number × dimension or dimension × number, but not dimension
               × dimension *)
            if is_dimension left && is_dimension right then
              Cursor.err t "invalid calc: cannot multiply two dimensions";
            loop (Expr (left, Mul, right)))
    | Some '/' ->
        (* Use atomic to ensure we either parse the full division or nothing *)
        Cursor.atomic t (fun () ->
            Cursor.skip t;
            Cursor.ws t;
            let right = read_calc_factor read_a t in
            (* Validate division: right operand must be a number (not a
               dimension) *)
            let is_not_number : type a. a calc -> bool = function
              | Val _ -> true (* definitely not a number *)
              | Num _ -> false (* is a number *)
              | _ -> false (* expressions could evaluate to numbers *)
            in
            if is_not_number right then
              Cursor.err t
                "invalid calc: division requires a number on the right";
            loop (Expr (left, Div, right)))
    | _ -> left
  in
  let left = read_calc_factor read_a t in
  loop left

and read_calc_parenthesized : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Parens (Cursor.parens (fun inner -> read_calc_expr read_a inner) t)

and read_calc_zero : type a. Cursor.t -> a calc =
 fun t ->
  (* A zero in calc is a plain Number_tok with value 0 (not a Dimension). *)
  let snap = Cursor.save t in
  match Cursor.number_opt t with
  | Some 0. -> Num 0.
  | _ ->
      Cursor.restore t snap;
      Cursor.err t "expected zero"

and read_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren -> read_calc_parenthesized read_a t
  | _ ->
      (* Handle nested calc() expressions - preserve as Nested node *)
      if Cursor.looking_at_func "calc" t then
        Nested (Cursor.call "calc" t (fun inner -> read_calc_expr read_a inner))
      else if Cursor.looking_at_func "var" t then Var (read_var read_a t)
      else if Cursor.looking_at_func "sibling-index" t then
        Cursor.call "sibling-index" t (fun inner ->
            Cursor.expect_eof inner;
            Sibling_index)
      else if Cursor.looking_at_func "sibling-count" t then
        Cursor.call "sibling-count" t (fun inner ->
            Cursor.expect_eof inner;
            Sibling_count)
      else
        let read_val t = Val (read_a t) in
        let read_num t = (Num (Cursor.number t) : a calc) in
        Cursor.one_of [ read_calc_zero; read_num; read_val ] t

and read_calc : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  if Cursor.looking_at_func "calc" t then
    Cursor.call "calc" t (fun inner -> read_calc_expr read_a inner)
  else if Cursor.looking_at_func "var" t then Var (read_var read_a t)
  else Cursor.err t "calc() or var()"

let rec read_length ?(allow_negative = true) ?(with_keywords = true) t : length
    =
  Cursor.ws t;
  let read_var_length t : length =
    let result : length =
      Var (read_var (read_length ~allow_negative ~with_keywords) t)
    in
    result
  in
  let read_calc_length t : length =
    Calc (read_calc (read_length ~allow_negative ~with_keywords) t)
  in
  let read_function_length t : length =
    (* [clamp(...)], [min(...)], [max(...)], [minmax(...)] arrive as a single
       [Func] component; consume the whole call and serialise the arguments. *)
    match
      Cursor.any_function_call
        (fun name inner ->
          match String.lowercase_ascii name with
          | "clamp" ->
              let s =
                Cursor.consume_remaining_to_string inner |> normalize_math_args
              in
              let groups = top_level_arg_count s in
              if groups <> 3 then
                Cursor.err_invalid inner
                  "clamp() requires three comma-separated arguments"
              else Clamp s
          | "minmax" ->
              let s =
                Cursor.consume_remaining_to_string inner |> normalize_math_args
              in
              if top_level_arg_count s <> 2 then
                Cursor.err_invalid inner
                  "minmax() requires two comma-separated arguments"
              else Minmax s
          | "min" ->
              let s =
                Cursor.consume_remaining_to_string inner |> normalize_math_args
              in
              if top_level_arg_count s < 1 then
                Cursor.err_invalid inner "min() requires at least one argument"
              else Min s
          | "max" ->
              let s =
                Cursor.consume_remaining_to_string inner |> normalize_math_args
              in
              if top_level_arg_count s < 1 then
                Cursor.err_invalid inner "max() requires at least one argument"
              else Max s
          | "fit-content" ->
              Cursor.ws inner;
              let arg = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Fit_content_arg arg
          | "round" ->
              let strategy = Cursor.ident inner in
              Cursor.ws inner;
              Cursor.comma inner;
              let value = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.comma inner;
              let step = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Round (strategy, value, step)
          | "mod" ->
              let a = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.comma inner;
              let b = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Mod (a, b)
          | "rem" ->
              let a = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.comma inner;
              let b = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Rem_fn (a, b)
          | "hypot" ->
              let a = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.comma inner;
              let b = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Hypot (a, b)
          | "abs" ->
              let value = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Abs value
          | "sign" ->
              let value = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Sign value
          | "calc-size" ->
              let basis = read_length ~allow_negative ~with_keywords inner in
              Cursor.ws inner;
              Cursor.comma inner;
              let calc =
                read_calc_expr
                  (read_length ~allow_negative ~with_keywords)
                  inner
              in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Calc_size (basis, calc)
          | "anchor-size" ->
              let size = Cursor.consume_remaining_to_string ~trim:true inner in
              if size = "" then Cursor.err_expected inner "anchor-size argument";
              Anchor_size size
          | "anchor" ->
              let first = Cursor.ident inner in
              Cursor.ws inner;
              let name, side =
                if String.starts_with ~prefix:"--" first then
                  let side = Cursor.ident inner in
                  (Some first, side)
                else (None, first)
              in
              Cursor.ws inner;
              let fallback =
                if Cursor.comma_opt inner then (
                  Cursor.ws inner;
                  Some (read_length ~allow_negative ~with_keywords inner))
                else None
              in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Anchor (name, side, fallback)
          | _ -> Cursor.err t ("unknown function " ^ name))
        t
    with
    | Some length -> length
    | None -> Cursor.err_expected t "function call"
  in
  let parsers =
    [
      read_var_length;
      read_calc_length;
      read_function_length;
      read_length_unit ~allow_negative;
    ]
  in
  let parsers =
    if with_keywords then read_length_keyword :: parsers else parsers
  in
  Cursor.one_of parsers t

(** Read a non-negative length value (for padding properties) *)
let read_non_negative_length ?(with_keywords = true) t : length =
  read_length ~allow_negative:false ~with_keywords t

(** Read a percentage value as float (number followed by %) Used for color
    components where 0-100% clamping is required *)
let read_percentage_float t : float = Cursor.pct ~clamp:true t

(** Read an alpha value *)
let rec read_alpha t : alpha =
  Cursor.ws t;
  let read_var_alpha t : alpha = Var (read_var read_alpha t) in
  let read_pct t : alpha =
    (* Alpha percentages are clamped to 0-100 per CSS spec *)
    Pct (Cursor.pct ~clamp:true t)
  in
  let read_num t : alpha =
    (* Fall back to reading as numeric alpha *)
    let n = Cursor.number t in
    (* Clamp numeric alpha to 0-1 range per CSS spec *)
    Num (max 0. (min 1. n))
  in
  Cursor.one_of [ read_var_alpha; read_pct; read_num ] t

(** Read optional alpha component *)
and read_optional_alpha t : alpha =
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
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        (* Clamp percentage to 0-100 per CSS spec *)
        Pct (max 0. (min 100. n))
    | None ->
        (* For unitless numbers: - If it's a decimal between 0 and 1, treat as
           Num (for alpha values) - Otherwise treat as Int (RGB 0-255 values) *)
        if n <= 1.0 && n <> floor n then Num n
        else Int (int_of_float (max 0. (min 255. n)))
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

and read_rgb_space_separated t : color =
  (* The cursor wraps the [rgb(...)] [Func] arguments, so there is no closing
     [)] to consume — it's the block boundary. *)
  Cursor.ws t;
  if Cursor.looking_at t "var(" then (
    let rgb_var = read_rgb_var t in
    Cursor.ws t;
    if Cursor.peek_delim t = Some '/' then (
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      match alpha with
      | None -> Cursor.err t "expected alpha value after '/'"
      | _ -> Rgba { rgb = Var rgb_var; a = alpha })
    else Rgb (Var rgb_var))
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
    | Num _ | Pct _ | Var _ -> Rgba { rgb = Channels { r; g; b }; a = alpha }

and read_rgb_comma_separated t : color =
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
  | _ -> Cursor.err_invalid t ("color space: " ^ space_ident)

(** Read color components until the alpha separator ['/'] or end of input. *)
let rec read_color_components space t acc =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then List.rev acc
  else if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    read_color_components space t (Component_none :: acc))
  else
    let component_count = List.length acc in
    let n, unit = Cursor.number_with_unit t in
    let component : component =
      match ((space : color_space), component_count, unit) with
      | (Lab | Oklab | Lch | Oklch), 0, Some "%" -> Pct n
      | (Lab | Oklab | Lch | Oklch), 0, _ ->
          Cursor.err_invalid t "L component must be percentage"
      | _, _, Some "%" -> Pct (n /. 100.)
      | _, _, None -> Num n
      | _, _, Some u -> Cursor.err_invalid t ("unit: " ^ u)
    in
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

(** Read an angle value *)
let rec read_angle t : angle =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "var(" then Var (read_var read_angle t)
  else if Cursor.looking_at t "calc(" then Calc (read_calc read_angle t)
  else
    let n, unit_raw = Cursor.number_with_unit t in
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "deg" -> Deg n
    | "rad" -> Rad n
    | "turn" -> Turn n
    | "grad" -> Grad n
    | "" ->
        Cursor.err_invalid t
          "angle values must have units (deg, rad, turn, or grad)"
    | _ -> Cursor.err_invalid t ("invalid angle unit: " ^ unit)

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
  let h = read_hue t in
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
  let h = read_hue t in
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

let read_oklch t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l =
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> n (* Already a percentage value *)
    | None when n >= 0. && n <= 1. -> n *. 100. (* Convert 0-1 *)
    | _ ->
        Cursor.err_invalid t
          ("oklch() L value must be 0-1 or 0%-100%, got " ^ string_of_float n)
  in
  Cursor.ws t;
  let c = Cursor.number t in
  Cursor.ws t;
  let h = read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklch { l = Pct l; c; h; alpha }

let read_number_or_none t : float option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    None)
  else Some (Cursor.number t)

let read_oklab t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l =
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> n (* Already a percentage value *)
    | None when n >= 0. && n <= 1. -> n *. 100. (* Convert 0-1 *)
    | _ ->
        Cursor.err_invalid t
          ("oklab() L value must be 0-1 or 0%-100%, got " ^ string_of_float n)
  in
  let a = read_number_or_none t in
  let b = read_number_or_none t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklab { l = Pct l; a; b; alpha }

let read_lab t : color =
  Cursor.ws t;
  let l = read_percentage_float t in
  let a = read_number_or_none t in
  let b = read_number_or_none t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lab { l = Pct l; a; b; alpha }

let read_lch t : color =
  Cursor.ws t;
  let l, c, h =
    Cursor.triple ~sep:Cursor.ws read_percentage_float Cursor.number read_hue t
  in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lch { l = Pct l; c; h; alpha }

let read_color_function t : color =
  Cursor.ws t;
  let space = read_color_space t in
  Cursor.ws t;
  let components = read_color_components space t [] in
  if List.length components <> 3 then
    Cursor.err_invalid t "color() requires three components";
  let alpha = read_optional_alpha t in
  Color { space; components; alpha }

(** Forward declaration for percentage reader used in color-mix *)
let rec read_percentage_in_color_mix t : percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Var (read_var read_percentage_in_color_mix t)
  else
    let n = Cursor.number t in
    Cursor.expect '%' t;
    if n < 0. || n > 100. then
      Cursor.err_invalid t "color-mix percentage must be between 0% and 100%";
    (Pct n : percentage)

let read_optional_percentage t : percentage option =
  (* Parse optional percentage immediately after a value. In color-mix(), this
     can be a numeric percentage, a decimal (0-1), or var(). *)
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    (* var() percentage like var(--bg-opacity) *)
    Some (Var (read_var read_percentage_in_color_mix t))
  else
    (* [50%] is a single [Percentage] token; a plain decimal is a [Number]. *)
    match Cursor.percentage_opt t with
    | Some n ->
        if n < 0. || n > 100. then
          Cursor.err_invalid t
            "color-mix percentage must be between 0% and 100%";
        Cursor.ws t;
        Some (Pct n : percentage)
    | None ->
        Cursor.option
          (fun t ->
            let n = Cursor.number t in
            if n < 0. || n > 1. then
              Cursor.err_invalid t
                "color-mix percentage must be between 0 and 1";
            Cursor.ws t;
            (* Convert decimal to percentage: .5 -> 50% stored as Pct 50.0 *)
            (Pct (n *. 100.0) : percentage))
          t

let rec read_color_mix t : color =
  Cursor.ws t;

  (* Parse "in <color-space> [<hue-interpolation-method>]" if present *)
  let in_space, hue =
    if Cursor.peek_ident t = Some "in" then (
      Cursor.expect_string "in" t;
      Cursor.ws t;
      let space = read_color_space t in
      Cursor.ws t;
      (* For cylindrical color spaces, check for hue interpolation *)
      let hue =
        match Cursor.peek_ident t with
        | Some ("shorter" | "longer" | "increasing" | "decreasing") ->
            let hue =
              Cursor.enum "hue-interpolation"
                [
                  ("shorter", Shorter);
                  ("longer", Longer);
                  ("increasing", Increasing);
                  ("decreasing", Decreasing);
                ]
                t
            in
            Cursor.ws t;
            Cursor.expect_string "hue" t;
            Cursor.ws t;
            hue
        | _ -> Default
      in
      (Some space, hue))
    else (None, Default)
  in

  Cursor.ws t;
  Cursor.comma t;
  Cursor.ws t;

  (* Parse first color and optional percentage *)
  let color1 = read_color t in
  Cursor.ws t;
  let percent1 = read_optional_percentage t in

  Cursor.comma t;
  Cursor.ws t;

  (* Parse second color and optional percentage *)
  let color2 = read_color t in
  Cursor.ws t;
  let percent2 = read_optional_percentage t in

  Cursor.ws t;
  Mix { in_space; hue; color1; percent1; color2; percent2 }

and normalize_relative_color_tail tail =
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
      | '/' ->
          let blen = Buffer.length buf in
          if blen > 0 && Buffer.nth buf (blen - 1) = ' ' then
            Buffer.truncate buf (blen - 1);
          Buffer.add_char buf '/';
          loop (skip_spaces (i + 1)) false
      | c ->
          if last_was_space && Buffer.length buf > 0 then
            Buffer.add_char buf ' ';
          Buffer.add_char buf c;
          loop (i + 1) false
  in
  loop 0 false;
  Buffer.contents buf

and relative_color_channel_count tail =
  let channel_part =
    match String.index_opt tail '/' with
    | Some i -> String.sub tail 0 i
    | None -> tail
  in
  channel_part |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> List.length

and read_relative_rgb t : color =
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  let tail =
    Cursor.consume_remaining_to_string ~trim:true t
    |> normalize_relative_color_tail
  in
  if tail = "" then Cursor.err_expected t "relative rgb channels";
  if relative_color_channel_count tail <> 3 then
    Cursor.err_expected t "relative rgb channels";
  let origin = Pp.to_string ~minify:true pp_color origin in
  Relative_rgb ("from " ^ origin ^ " " ^ tail)

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
    ("hsl", read_hsl);
    ("hsla", read_hsl);
    ("hwb", read_hwb);
    ("oklch", read_oklch);
    ("lab", read_lab);
    ("oklab", read_oklab);
    ("lch", read_lch);
    ("color", read_color_function);
    ("contrast-color", read_contrast_color);
    ("light-dark", read_light_dark);
    ("color-mix", read_color_mix);
  ]

and read_color t : color =
  Cursor.ws t;
  let color =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Hash { value; _ }; _ }) ->
        Cursor.skip t;
        let len = String.length value in
        let is_hex c =
          ('0' <= c && c <= '9')
          || ('a' <= c && c <= 'f')
          || ('A' <= c && c <= 'F')
        in
        if not (len = 3 || len = 4 || len = 6 || len = 8) then
          Cursor.err_invalid t ("hex color length: " ^ string_of_int len)
        else if not (String.for_all is_hex value) then
          Cursor.err_invalid t ("hex color digits: " ^ value)
        else Hex { hash = true; value }
    | Some (Component.Func ({ node = { name; _ }; _ } as fn)) -> (
        match List.assoc_opt name color_parsers with
        | Some parser ->
            Cursor.skip t;
            parser (Cursor.func_sub fn t)
        | None when name = "var" -> Var (read_var read_color t)
        | None -> Cursor.err t ("unknown color function: " ^ name))
    | Some (Component.Preserved { kind = Token.Ident ident; _ }) -> (
        Cursor.skip t;
        (* CSS color keywords are case-insensitive. *)
        match read_color_keyword_from_string (String.lowercase_ascii ident) with
        | Some color -> color
        | None -> Cursor.err t ("unknown color: " ^ ident))
    | _ -> Cursor.err t "color"
  in
  Cursor.ws t;
  (match Cursor.peek_delim t with
  | Some '/' -> Cursor.err_invalid t "unexpected color alpha separator"
  | _ -> ());
  color

and read_color_keyword_from_string keyword : color option =
  match keyword with
  | "transparent" -> Some Transparent
  | "currentcolor" -> Some Current
  | "auto" -> Some Auto
  | "inherit" -> Some Inherit
  | "red" -> Some (Named Red)
  | "green" -> Some (Named Green)
  | "blue" -> Some (Named Blue)
  | "white" -> Some (Named White)
  | "black" -> Some (Named Black)
  | "gray" -> Some (Named Gray)
  | "grey" -> Some (Named Grey)
  | "silver" -> Some (Named Silver)
  | "maroon" -> Some (Named Maroon)
  | "yellow" -> Some (Named Yellow)
  | "olive" -> Some (Named Olive)
  | "lime" -> Some (Named Lime)
  | "aqua" -> Some (Named Aqua)
  | "cyan" -> Some (Named Cyan)
  | "teal" -> Some (Named Teal)
  | "navy" -> Some (Named Navy)
  | "fuchsia" -> Some (Named Fuchsia)
  | "magenta" -> Some (Named Magenta)
  | "purple" -> Some (Named Purple)
  | "orange" -> Some (Named Orange)
  | "pink" -> Some (Named Pink)
  | "aliceblue" -> Some (Named Alice_blue)
  | "antiquewhite" -> Some (Named Antique_white)
  | "aquamarine" -> Some (Named Aquamarine)
  | "azure" -> Some (Named Azure)
  | "beige" -> Some (Named Beige)
  | "bisque" -> Some (Named Bisque)
  | "blanchedalmond" -> Some (Named Blanched_almond)
  | "blueviolet" -> Some (Named Blue_violet)
  | "brown" -> Some (Named Brown)
  | "burlywood" -> Some (Named Burlywood)
  | "cadetblue" -> Some (Named Cadet_blue)
  | "chartreuse" -> Some (Named Chartreuse)
  | "chocolate" -> Some (Named Chocolate)
  | "coral" -> Some (Named Coral)
  | "cornflowerblue" -> Some (Named Cornflower_blue)
  | "cornsilk" -> Some (Named Cornsilk)
  | "crimson" -> Some (Named Crimson)
  | "darkblue" -> Some (Named Dark_blue)
  | "darkcyan" -> Some (Named Dark_cyan)
  | "darkgoldenrod" -> Some (Named Dark_goldenrod)
  | "darkgray" -> Some (Named Dark_gray)
  | "darkgreen" -> Some (Named Dark_green)
  | "darkgrey" -> Some (Named Dark_grey)
  | "darkkhaki" -> Some (Named Dark_khaki)
  | "darkmagenta" -> Some (Named Dark_magenta)
  | "darkolivegreen" -> Some (Named Dark_olive_green)
  | "darkorange" -> Some (Named Dark_orange)
  | "darkorchid" -> Some (Named Dark_orchid)
  | "darkred" -> Some (Named Dark_red)
  | "darksalmon" -> Some (Named Dark_salmon)
  | "darkseagreen" -> Some (Named Dark_sea_green)
  | "darkslateblue" -> Some (Named Dark_slate_blue)
  | "darkslategray" -> Some (Named Dark_slate_gray)
  | "darkslategrey" -> Some (Named Dark_slate_grey)
  | "darkturquoise" -> Some (Named Dark_turquoise)
  | "darkviolet" -> Some (Named Dark_violet)
  | "deeppink" -> Some (Named Deep_pink)
  | "deepskyblue" -> Some (Named Deep_sky_blue)
  | "dimgray" -> Some (Named Dim_gray)
  | "dimgrey" -> Some (Named Dim_grey)
  | "dodgerblue" -> Some (Named Dodger_blue)
  | "firebrick" -> Some (Named Firebrick)
  | "floralwhite" -> Some (Named Floral_white)
  | "forestgreen" -> Some (Named Forest_green)
  | "gainsboro" -> Some (Named Gainsboro)
  | "ghostwhite" -> Some (Named Ghost_white)
  | "gold" -> Some (Named Gold)
  | "goldenrod" -> Some (Named Goldenrod)
  | "greenyellow" -> Some (Named Green_yellow)
  | "honeydew" -> Some (Named Honeydew)
  | "hotpink" -> Some (Named Hot_pink)
  | "indianred" -> Some (Named Indian_red)
  | "indigo" -> Some (Named Indigo)
  | "ivory" -> Some (Named Ivory)
  | "khaki" -> Some (Named Khaki)
  | "lavender" -> Some (Named Lavender)
  | "lavenderblush" -> Some (Named Lavender_blush)
  | "lawngreen" -> Some (Named Lawn_green)
  | "lemonchiffon" -> Some (Named Lemon_chiffon)
  | "lightblue" -> Some (Named Light_blue)
  | "lightcoral" -> Some (Named Light_coral)
  | "lightcyan" -> Some (Named Light_cyan)
  | "lightgoldenrodyellow" -> Some (Named Light_goldenrod_yellow)
  | "lightgray" -> Some (Named Light_gray)
  | "lightgreen" -> Some (Named Light_green)
  | "lightgrey" -> Some (Named Light_grey)
  | "lightpink" -> Some (Named Light_pink)
  | "lightsalmon" -> Some (Named Light_salmon)
  | "lightseagreen" -> Some (Named Light_sea_green)
  | "lightskyblue" -> Some (Named Light_sky_blue)
  | "lightslategray" -> Some (Named Light_slate_gray)
  | "lightslategrey" -> Some (Named Light_slate_grey)
  | "lightsteelblue" -> Some (Named Light_steel_blue)
  | "lightyellow" -> Some (Named Light_yellow)
  | "limegreen" -> Some (Named Lime_green)
  | "linen" -> Some (Named Linen)
  | "mediumaquamarine" -> Some (Named Medium_aquamarine)
  | "mediumblue" -> Some (Named Medium_blue)
  | "mediumorchid" -> Some (Named Medium_orchid)
  | "mediumpurple" -> Some (Named Medium_purple)
  | "mediumseagreen" -> Some (Named Medium_sea_green)
  | "mediumslateblue" -> Some (Named Medium_slate_blue)
  | "mediumspringgreen" -> Some (Named Medium_spring_green)
  | "mediumturquoise" -> Some (Named Medium_turquoise)
  | "mediumvioletred" -> Some (Named Medium_violet_red)
  | "midnightblue" -> Some (Named Midnight_blue)
  | "mintcream" -> Some (Named Mint_cream)
  | "mistyrose" -> Some (Named Misty_rose)
  | "moccasin" -> Some (Named Moccasin)
  | "navajowhite" -> Some (Named Navajo_white)
  | "oldlace" -> Some (Named Old_lace)
  | "olivedrab" -> Some (Named Olive_drab)
  | "orangered" -> Some (Named Orange_red)
  | "orchid" -> Some (Named Orchid)
  | "palegoldenrod" -> Some (Named Pale_goldenrod)
  | "palegreen" -> Some (Named Pale_green)
  | "paleturquoise" -> Some (Named Pale_turquoise)
  | "palevioletred" -> Some (Named Pale_violet_red)
  | "papayawhip" -> Some (Named Papaya_whip)
  | "peachpuff" -> Some (Named Peach_puff)
  | "peru" -> Some (Named Peru)
  | "plum" -> Some (Named Plum)
  | "powderblue" -> Some (Named Powder_blue)
  | "rebeccapurple" -> Some (Named Rebecca_purple)
  | "rosybrown" -> Some (Named Rosy_brown)
  | "royalblue" -> Some (Named Royal_blue)
  | "saddlebrown" -> Some (Named Saddle_brown)
  | "salmon" -> Some (Named Salmon)
  | "sandybrown" -> Some (Named Sandy_brown)
  | "seagreen" -> Some (Named Sea_green)
  | "seashell" -> Some (Named Sea_shell)
  | "sienna" -> Some (Named Sienna)
  | "skyblue" -> Some (Named Sky_blue)
  | "slateblue" -> Some (Named Slate_blue)
  | "slategray" -> Some (Named Slate_gray)
  | "slategrey" -> Some (Named Slate_grey)
  | "snow" -> Some (Named Snow)
  | "springgreen" -> Some (Named Spring_green)
  | "steelblue" -> Some (Named Steel_blue)
  | "tan" -> Some (Named Tan)
  | "thistle" -> Some (Named Thistle)
  | "tomato" -> Some (Named Tomato)
  | "turquoise" -> Some (Named Turquoise)
  | "violet" -> Some (Named Violet)
  | "wheat" -> Some (Named Wheat)
  | "whitesmoke" -> Some (Named White_smoke)
  | "yellowgreen" -> Some (Named Yellow_green)
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  (* CSS system colors - case-insensitive matching *)
  | _ -> read_system_color_from_string keyword

and read_system_color_from_string keyword : color option =
  (* System colors are case-insensitive per CSS spec *)
  match String.lowercase_ascii keyword with
  | "accentcolor" -> Some (System Accent_color)
  | "accentcolortext" -> Some (System Accent_color_text)
  | "activetext" -> Some (System Active_text)
  | "buttonborder" -> Some (System Button_border)
  | "buttonface" -> Some (System Button_face)
  | "buttontext" -> Some (System Button_text)
  | "canvas" -> Some (System Canvas)
  | "canvastext" -> Some (System Canvas_text)
  | "field" -> Some (System Field)
  | "fieldtext" -> Some (System Field_text)
  | "graytext" -> Some (System Gray_text)
  | "highlight" -> Some (System Highlight)
  | "highlighttext" -> Some (System Highlight_text)
  | "linktext" -> Some (System Link_text)
  | "mark" -> Some (System Mark)
  | "marktext" -> Some (System Mark_text)
  | "selecteditem" -> Some (System Selected_item)
  | "selecteditemtext" -> Some (System Selected_item_text)
  | "visitedtext" -> Some (System Visited_text)
  (* WebKit-specific system colors *)
  | "-webkit-focus-ring-color" -> Some (System Webkit_focus_ring_color)
  | _ -> None

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

(** Read a duration value *)
let rec read_duration t : duration =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "var(" then Var (read_var read_duration t)
  else if Cursor.looking_at t "calc(" then Calc (read_calc read_duration t)
  else
    let n, unit_raw = Cursor.number_with_unit t in
    if n < 0.0 then Cursor.err_invalid t "negative durations are not allowed"
    else
      let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
      match unit with
      | "s" -> S n
      | "ms" -> Ms n
      | _ -> Cursor.err_invalid t ("duration unit: " ^ unit)

(** Read a time value that can be negative (for animation-delay,
    transition-delay) *)
let rec read_time t : duration =
  Cursor.ws t;
  (* Check for var() *)
  if Cursor.looking_at t "var(" then Var (read_var read_time t)
  else
    let n, unit_raw = Cursor.number_with_unit t in
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "s" -> S n
    | "ms" -> Ms n
    | _ -> Cursor.err_invalid t ("time unit: " ^ unit)

(** Read a number value *)
let rec read_number t : number =
  Cursor.ws t;
  let number =
    (* Check for var() *)
    if Cursor.looking_at t "var(" then Var (read_var read_number t)
    else if Cursor.looking_at t "calc(" then Calc (read_calc read_number t)
    else if Cursor.looking_at_func "round" t then
      Cursor.call "round" t (fun inner ->
          let strategy = Cursor.ident inner in
          Cursor.ws inner;
          Cursor.comma inner;
          let value = read_number inner in
          Cursor.ws inner;
          Cursor.comma inner;
          let step = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Round (strategy, value, step))
    else if Cursor.looking_at_func "mod" t then
      Cursor.call "mod" t (fun inner ->
          let a = read_number inner in
          Cursor.ws inner;
          Cursor.comma inner;
          let b = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Mod (a, b))
    else if Cursor.looking_at_func "hypot" t then
      Cursor.call "hypot" t (fun inner ->
          let a = read_number inner in
          Cursor.ws inner;
          Cursor.comma inner;
          let b = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Hypot (a, b))
    else if Cursor.looking_at_func "pow" t then
      Cursor.call "pow" t (fun inner ->
          let a = read_number inner in
          Cursor.ws inner;
          Cursor.comma inner;
          let b = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Pow (a, b))
    else if Cursor.looking_at_func "sqrt" t then
      Cursor.call "sqrt" t (fun inner ->
          let value = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Sqrt value)
    else if Cursor.looking_at_func "abs" t then
      Cursor.call "abs" t (fun inner ->
          let value = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Abs value)
    else if Cursor.looking_at_func "sign" t then
      Cursor.call "sign" t (fun inner ->
          let value = read_number inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Sign value)
    else if Cursor.looking_at_func "sin" t then
      Cursor.call "sin" t (fun inner ->
          let value = read_angle inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Sin value)
    else Num (Cursor.number t)
  in
  Cursor.ws t;
  (match Cursor.peek t with
  | Some (Component.Func _)
  | Some (Component.Preserved { kind = Token.Number_tok _; _ }) ->
      Cursor.err_invalid t "unexpected tokens after number"
  | _ -> ());
  number

(** Read transition_behavior value *)
let read_transition_behavior t : transition_behavior =
  Cursor.enum "transition-behavior"
    [ ("normal", Normal); ("allow-discrete", Allow_discrete) ]
    t

(** Read a percentage type with var() and calc() support *)
let rec read_percentage t : percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_percentage t)
  else if Cursor.looking_at t "calc(" then Calc (read_calc read_percentage t)
  else Pct (Cursor.pct t)

(** Read length_percentage value *)
let rec read_length_percentage t : length_percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_length_percentage t)
  else if Cursor.looking_at t "calc(" then
    Calc (read_calc read_length_percentage t)
  else
    (* Try to read as percentage or length *)
    let read_pct t : length_percentage = Pct (Cursor.pct t) in
    let read_length_as_lp t : length_percentage = Length (read_length t) in
    Cursor.one_of [ read_pct; read_length_as_lp ] t

(** Read number_percentage value *)
let rec read_number_percentage t : number_percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_number_percentage t)
  else if Cursor.looking_at t "calc(" then
    Calc (read_calc read_number_percentage t)
  else
    (* Try to read as percentage or number *)
    Cursor.one_of
      [ (fun t -> Pct (Cursor.pct t)); (fun t -> Num (Cursor.number t)) ]
      t

(** Read color_name value *)
let read_color_name t : color_name =
  Cursor.ws t;
  let s = Cursor.ident t in
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
  | "rebeccapurple" -> Rebecca_purple
  | _ -> Cursor.err_invalid t ("color name: " ^ s)

(** Read hue_interpolation *)
let read_hue_interpolation t : hue_interpolation =
  Cursor.ws t;
  Cursor.enum "hue-interpolation"
    [
      ("shorter", Shorter);
      ("longer", Longer);
      ("increasing", Increasing);
      ("decreasing", Decreasing);
      ("default", Default);
    ]
    t

(** Read calc_op *)
let read_calc_op t : calc_op =
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '+' ->
      Cursor.skip t;
      Add
  | Some '-' ->
      Cursor.skip t;
      Sub
  | Some '*' ->
      Cursor.skip t;
      Mul
  | Some '/' ->
      Cursor.skip t;
      Div
  | _ -> Cursor.err_invalid t "calc operator"

(** Read component value *)
let rec read_component t : component =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_component t)
  else if Cursor.looking_at t "calc(" then Calc (read_calc read_component t)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        (* Clamp percentage to 0-100 range per CSS spec *)
        Pct (max 0. (min 100. n))
    | _ ->
        (* Clamp numeric component to 0-255 range for RGB values per CSS spec *)
        Num (max 0. (min 255. n))

(* Var helper functions *)
let var_name v = v.name
let var_layer v = v.layer
let var_meta v = v.meta

let with_fallback v fallback_value =
  { v with fallback = Fallback fallback_value }

(** Read padding shorthand property (1-4 values) *)
let read_padding_shorthand t : length list =
  (* CSS padding accepts 1-4 space-separated non-negative values *)
  (* CSS-wide keywords must be the only value when present *)
  Cursor.enum "padding"
    [
      ("inherit", [ Inherit ]);
      ("initial", [ Initial ]);
      ("unset", [ Unset ]);
      ("revert", [ Revert ]);
      ("revert-layer", [ Revert_layer ]);
    ]
    ~default:(fun t ->
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
        (read_non_negative_length ~with_keywords:false)
        t)
    t

(** Read margin shorthand property (1-4 values) Source:
    https://www.w3.org/TR/CSS21/box.html#margin-properties CSS margin accepts
    1-4 space-separated values *)
let read_margin_shorthand t : length list =
  (* CSS margin accepts 1-4 space-separated values *)
  (* CSS-wide keywords must be the only value when present *)
  Cursor.enum "margin"
    [
      ("auto", [ Auto ]);
      ("inherit", [ Inherit ]);
      ("initial", [ Initial ]);
      ("unset", [ Unset ]);
      ("revert", [ Revert ]);
      ("revert-layer", [ Revert_layer ]);
    ]
    ~default:(fun t ->
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4
        (read_length ~with_keywords:false)
        t)
    t
