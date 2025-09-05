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
type 'a calc = Var of 'a var | Val of 'a | Expr of 'a calc * calc_op * 'a calc

type length =
  | Px of int
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
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

type color =
  | Hex of { hash : bool; value : string }
  | Rgb of { r : int; g : int; b : int }
  | Rgba of { r : int; g : int; b : int; a : float }
  | Oklch of { l : float; c : float; h : float }
  | Named of color_name
  | Var of color var
  | Current
  | Transparent
  | Inherit
  | Mix of {
      in_space : color_space;
      color1 : color;
      percent1 : int option;
      color2 : color;
      percent2 : int option;
    }

type angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Var of angle var

type duration = Ms of int | S of float | Var of duration var
type number = Float of float | Int of int | Pct of float | Var of number var

(** Color constructors *)
let hex s =
  let len = String.length s in
  if len > 0 && s.[0] = '#' then
    Hex { hash = true; value = String.sub s 1 (len - 1) }
  else Hex { hash = false; value = s }

let rgb r g b = Rgb { r; g; b }
let rgba r g b a = Rgba { r; g; b; a }
let oklch l c h = Oklch { l; c; h }
let color_name n = Named n
let current_color = Current
let transparent = Transparent

let color_mix ?(in_space = Srgb) ?percent1 ?percent2 color1 color2 =
  Mix { in_space; color1; percent1; color2; percent2 }

(** Pretty-printing functions *)

let pp_op ctx = function
  | Add -> Pp.string ctx " + "
  | Sub -> Pp.string ctx " - "
  | Mult -> Pp.string ctx " * "
  | Div -> Pp.string ctx " / "

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  (* TODO: Add proper CSS variable inlining phase *)
  if ctx.inline && v.default <> None then
    (* When inlining is enabled, output the default value if available *)
    match v.default with
    | Some value -> pp_value ctx value
    | None -> assert false (* unreachable due to condition above *)
  else
    (* Standard var() reference output *)
    Pp.string ctx "var(--";
  Pp.string ctx v.name;
  match v.fallback with
  | None -> Pp.char ctx ')'
  | Some value ->
      Pp.string ctx ", ";
      pp_value ctx value;
      Pp.char ctx ')'

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

let rec pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  match calc with
  | Val v -> pp_value ctx v
  | Var v -> pp_var pp_value ctx v
  | Expr (left, op, right) ->
      pp_calc pp_value ctx left;
      pp_op ctx op;
      pp_calc pp_value ctx right

let rec pp_length : length Pp.t =
 fun ctx -> function
  | Px n when n = 0 -> Pp.char ctx '0'
  | Rem f when f = 0. -> Pp.char ctx '0'
  | Em f when f = 0. -> Pp.char ctx '0'
  | Pct f when f = 0. -> Pp.char ctx '0'
  | Vw f when f = 0. -> Pp.char ctx '0'
  | Vh f when f = 0. -> Pp.char ctx '0'
  | Ch f when f = 0. -> Pp.char ctx '0'
  | Lh f when f = 0. -> Pp.char ctx '0'
  | Zero -> Pp.char ctx '0'
  | Px n ->
      Pp.int ctx n;
      Pp.string ctx "px"
  | Rem f ->
      Pp.float ctx f;
      Pp.string ctx "rem"
  | Em f ->
      Pp.float ctx f;
      Pp.string ctx "em"
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Vw f ->
      Pp.float ctx f;
      Pp.string ctx "vw"
  | Vh f ->
      Pp.float ctx f;
      Pp.string ctx "vh"
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
      | Expr (Val (Num f), Mult, Val (Px 1)) when f = infinity ->
          Pp.string ctx "3.40282e38px"
      | _ -> pp_fun "calc" (pp_calc pp_length) ctx cv)

and pp_color_name : color_name Pp.t =
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

(* RGB helper function *)
and pp_rgb ctx r g b alpha =
  Pp.string ctx "rgb(";
  Pp.int ctx r;
  Pp.space ctx ();
  Pp.int ctx g;
  Pp.space ctx ();
  Pp.int ctx b;
  (match alpha with
  | Some a ->
      Pp.string ctx " / ";
      Pp.float ctx a
  | None -> ());
  Pp.char ctx ')'

(* OKLCH helper function *)
and pp_oklch ctx l c h =
  Pp.string ctx "oklch(";
  Pp.float_n 1 ctx l;
  Pp.string ctx "% ";
  Pp.float_n 3 ctx c;
  Pp.space ctx ();
  Pp.float_n 3 ctx h;
  Pp.char ctx ')'

and pp_color_space : color_space Pp.t =
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
and pp_color_mix ctx in_space color1 percent1 color2 percent2 =
  Pp.string ctx "color-mix(in ";
  pp_color_space ctx in_space;
  Pp.string ctx ", ";
  pp_color_in_mix ctx color1;
  (match percent1 with
  | Some p ->
      Pp.space ctx ();
      Pp.int ctx p;
      Pp.char ctx '%'
  | None -> ());
  Pp.string ctx ", ";
  pp_color_in_mix ctx color2;
  (match percent2 with
  | Some p ->
      Pp.space ctx ();
      Pp.int ctx p;
      Pp.char ctx '%'
  | None -> ());
  Pp.char ctx ')'

(* Convert to Pp-based color formatter *)
and pp_color : color Pp.t =
 fun ctx -> function
  | Hex { hash = _; value } ->
      Pp.char ctx '#';
      Pp.string ctx value
  | Rgb { r; g; b } -> pp_rgb ctx r g b None
  | Rgba { r; g; b; a } -> pp_rgb ctx r g b (Some a)
  | Oklch { l; c; h } -> pp_oklch ctx l c h
  | Named name -> pp_color_name ctx name
  | Var v -> pp_var pp_color ctx v
  | Current -> Pp.string ctx "currentcolor"
  | Transparent -> Pp.string ctx "transparent"
  | Inherit -> Pp.string ctx "inherit"
  | Mix { in_space; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space color1 percent1 color2 percent2

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

let rec pp_duration : duration Pp.t =
 fun ctx -> function
  | Ms n ->
      Pp.int ctx n;
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

(** Read a CSS length value *)
let read_length t : length =
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
      | "inherit" -> Inherit
      | _ -> err_invalid t ("length keyword: " ^ keyword))
  | Some n when n = 0.0 -> Zero
  | Some n -> (
      (* Check for unit *)
      let unit = while_ t (fun c -> (c >= 'a' && c <= 'z') || c = '%') in
      match unit with
      | "" -> Num n
      | "px" -> Px (int_of_float n)
      | "em" -> Em n
      | "rem" -> Rem n
      | "vh" -> Vh n
      | "vw" -> Vw n
      | "ch" -> Ch n
      | "%" -> Pct n
      | _ -> err_invalid t ("length unit: " ^ unit))

(** Read a CSS color value *)
let parse_hex_color t : color =
  skip t;
  (* skip the # *)
  let hex = hex_color t in
  Hex { hash = true; value = hex }

let rec parse_var_in_color t : color =
  expect t '(';
  ws t;
  let var_name =
    if looking_at t "--" then (
      skip_n t 2;
      ident t (* var_name should be without -- *))
    else ident t
  in
  ws t;
  let fallback =
    if peek t = Some ',' then (
      skip t;
      ws t;
      (* Parse the fallback color value *)
      Some (read_color_value t))
    else None
  in
  expect t ')';
  (* Create a color var with fallback *)
  let v = var_ref ?fallback var_name in
  Var v

and read_color_value t : color =
  (* Parse a color value that could be a keyword, hex, or rgb function *)
  ws t;
  match peek t with
  | Some '#' -> parse_hex_color t
  | _ -> (
      (* Try rgb function first *)
      match rgb_function t with
      | Some (r, g, b, alpha) -> (
          match alpha with
          | None -> Rgb { r; g; b }
          | Some a -> Rgba { r; g; b; a })
      | None -> (
          (* Not a function, try keyword *)
          let keyword = ident t in
          match String.lowercase_ascii keyword with
          | "transparent" -> Transparent
          | "currentcolor" -> Current
          | "inherit" -> Inherit
          | "red" -> Named Red
          | "green" -> Named Green
          | "blue" -> Named Blue
          | "white" -> Named White
          | "black" -> Named Black
          | "gray" | "grey" -> Named Gray
          | "silver" -> Named Silver
          | "maroon" -> Named Maroon
          | "yellow" -> Named Yellow
          | "olive" -> Named Olive
          | "lime" -> Named Lime
          | "aqua" | "cyan" -> Named Cyan
          | "teal" -> Named Teal
          | "navy" -> Named Navy
          | "fuchsia" | "magenta" -> Named Fuchsia
          | "purple" -> Named Purple
          | "orange" -> Named Orange
          | "pink" -> Named Pink
          | _ -> err_invalid t ("color: " ^ keyword)))

and parse_color_keyword t : color =
  let keyword = ident t in
  match String.lowercase_ascii keyword with
  | "transparent" -> Transparent
  | "currentcolor" -> Current
  | "inherit" -> Inherit
  | "red" -> Named Red
  | "green" -> Named Green
  | "blue" -> Named Blue
  | "white" -> Named White
  | "black" -> Named Black
  | "gray" | "grey" -> Named Gray
  | "silver" -> Named Silver
  | "maroon" -> Named Maroon
  | "yellow" -> Named Yellow
  | "olive" -> Named Olive
  | "lime" -> Named Lime
  | "aqua" | "cyan" -> Named Cyan
  | "teal" -> Named Teal
  | "navy" -> Named Navy
  | "fuchsia" | "magenta" -> Named Fuchsia
  | "purple" -> Named Purple
  | "orange" -> Named Orange
  | "pink" -> Named Pink
  | _ -> err_invalid t ("color: " ^ keyword)

let parse_color_keyword_or_var t : color =
  let keyword = ident t in
  match String.lowercase_ascii keyword with
  | "var" -> parse_var_in_color t
  | _ ->
      (* Put back the keyword and parse it as a color *)
      let t' = Reader.of_string keyword in
      parse_color_keyword t'

let read_color t : color =
  ws t;
  match peek t with
  | Some '#' -> parse_hex_color t
  | _ -> (
      match rgb_function t with
      | Some (r, g, b, alpha) -> (
          match alpha with
          | None -> Rgb { r; g; b }
          | Some a -> Rgba { r; g; b; a })
      | None -> parse_color_keyword_or_var t)

(** Read an angle value *)
let read_angle t : angle =
  ws t;
  let n = number t in
  let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
  match unit with
  | "deg" -> Deg n
  | "rad" -> Rad n
  | "turn" -> Turn n
  | "grad" -> Grad n
  | "" -> Deg n (* Default to degrees *)
  | _ -> err_invalid t ("angle unit: " ^ unit)

(** Read a duration value *)
let read_duration t : duration =
  ws t;
  let n = number t in
  let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
  match unit with
  | "s" -> S n
  | "ms" -> Ms (int_of_float n)
  | "" -> Ms (int_of_float n) (* Default to milliseconds *)
  | _ -> err_invalid t ("duration unit: " ^ unit)

(** Read a number value *)
let read_number t : number =
  ws t;
  let n = number t in
  if n = float_of_int (int_of_float n) then Int (int_of_float n) else Float n

(** Read a percentage value *)
let read_percentage t : float =
  ws t;
  let n = number t in
  expect t '%';
  n

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
    skip_n t 5;
    (* skip "calc(" *)
    let expr = read_calc_expr read_a t in
    expect t ')';
    expr)
  else if looking_at t "var(" then (
    skip_n t 4;
    (* skip "var(" *)
    ws t;
    let var_name =
      if looking_at t "--" then (
        skip_n t 2;
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
    (* Try to parse a value directly *)
    Val (read_a t)
