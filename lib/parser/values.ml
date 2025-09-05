(** CSS Values & Units parsing using Reader API *)

open Reader
module C = Css

(** Error helpers *)
let err_invalid what = raise (Parse_error ("invalid " ^ what))

let err_expected what = raise (Parse_error ("expected " ^ what))

(** Read a CSS length value *)
let read_length t : Css.length =
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
      | _ -> err_invalid ("length keyword: " ^ keyword))
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
      | _ -> err_invalid ("length unit: " ^ unit))

(** Read a CSS color value *)
let parse_hex_color t : Css.color =
  skip t;
  (* skip the # *)
  let hex = hex_color t in
  Hex { hash = true; value = hex }

let rec parse_var_in_color t : Css.color =
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
  let v = Css.var_ref ?fallback var_name in
  Css.Var v

and read_color_value t : Css.color =
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
          | _ -> err_invalid ("color: " ^ keyword)))

and parse_color_keyword t : Css.color =
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
  | _ -> err_invalid ("color: " ^ keyword)

let parse_color_keyword_or_var t : Css.color =
  let keyword = ident t in
  match String.lowercase_ascii keyword with
  | "var" -> parse_var_in_color t
  | _ ->
      (* Put back the keyword and parse it as a color *)
      let t' = Reader.of_string keyword in
      parse_color_keyword t'

let read_color t : Css.color =
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
let read_angle t : Css.angle =
  ws t;
  let n = number t in
  let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
  match unit with
  | "deg" -> Deg n
  | "rad" -> Rad n
  | "turn" -> Turn n
  | "grad" -> Grad n
  | "" -> Deg n (* Default to degrees *)
  | _ -> err_invalid ("angle unit: " ^ unit)

(** Read a duration value *)
let read_duration t : Css.duration =
  ws t;
  let n = number t in
  let unit = while_ t (fun c -> c >= 'a' && c <= 'z') in
  match unit with
  | "s" -> S n
  | "ms" -> Ms (int_of_float n)
  | "" -> Ms (int_of_float n) (* Default to milliseconds *)
  | _ -> err_invalid ("duration unit: " ^ unit)

(** Read a number value *)
let read_number t : Css.number =
  ws t;
  let n = number t in
  if n = float_of_int (int_of_float n) then Int (int_of_float n) else Float n

(** Read a percentage value *)
let read_percentage t : float =
  ws t;
  let n = number t in
  expect t '%';
  n

(** Helper to read a length value for fallback parsing *)
let rec read_length_value t : Css.length =
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
      | _ -> err_invalid ("length keyword: " ^ keyword))
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
      | _ -> err_invalid ("length unit: " ^ unit))

(** Read calc() expression *)
and read_calc t : Css.length Css.calc =
  ws t;
  if looking_at t "calc(" then (
    skip_n t 5;
    (* skip "calc(" *)
    let expr = read_calc_expr t in
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
        Some (read_length_value t))
      else None
    in
    expect t ')';
    (* Create a length var with fallback *)
    let v = Css.var_ref ?fallback var_name in
    Var v)
  else
    (* Try to parse a value *)
    match try_parse number t with
    | Some n -> (
        let unit = while_ t (fun c -> (c >= 'a' && c <= 'z') || c = '%') in
        match unit with
        | "" -> Val (Num n)
        | "px" -> Val (Px (int_of_float n))
        | "em" -> Val (Em n)
        | "rem" -> Val (Rem n)
        | "%" -> Val (Pct n)
        | "vh" -> Val (Vh n)
        | "vw" -> Val (Vw n)
        | _ -> err_invalid ("calc unit: " ^ unit))
    | None -> err_expected "calc expression"

and read_calc_expr t : Css.length Css.calc =
  ws t;
  let left = read_calc_term t in
  ws t;
  match peek t with
  | Some '+' ->
      skip t;
      Expr (left, Add, read_calc_expr t)
  | Some '-' ->
      skip t;
      Expr (left, Sub, read_calc_expr t)
  | _ -> left

and read_calc_term t : Css.length Css.calc =
  ws t;
  let left = read_calc_factor t in
  ws t;
  match peek t with
  | Some '*' ->
      skip t;
      Expr (left, Mult, read_calc_term t)
  | Some '/' ->
      skip t;
      Expr (left, Div, read_calc_term t)
  | _ -> left

and read_calc_factor t : Css.length Css.calc =
  ws t;
  if peek t = Some '(' then (
    skip t;
    let expr = read_calc_expr t in
    ws t;
    expect t ')';
    expr)
  else read_calc t
