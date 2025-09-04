(* CSS parser supporting: - Basic rulesets: selector { declarations } - Media
   queries: @media condition { rules } - CSS variables (custom properties) -
   Comments and whitespace handling - String literals and balanced delimiters in
   values *)

module Reader = Reader
module Declaration = Declaration
module Selector = Selector
module Rule = Rule

(* ======================================================================== Core
   Parser State and Primitives
   ======================================================================== *)

type state = { s : string; mutable i : int; len : int }

let init s = { s; i = 0; len = String.length s }
let[@inline] eof st = st.i >= st.len
let[@inline] peek st = if eof st then None else Some st.s.[st.i]

(* Error helpers *)
let err_not_implement msg = failwith msg

let err_unknown_value type_name value =
  failwith ("Unknown value '" ^ value ^ "' for " ^ type_name)

let[@inline] next st =
  if eof st then None
  else
    let c = st.s.[st.i] in
    st.i <- st.i + 1;
    Some c

let[@inline] at st j = if j < 0 || j >= st.len then None else Some st.s.[j]
let is_ws = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let skip_comment st =
  (* Skip comment content until */ is found *)
  let rec loop () =
    match next st with
    | None -> ()
    | Some '*' -> (
        match peek st with
        | Some '/' -> ignore (next st)
        (* consume / *)
        | _ -> loop ())
    | Some _ -> loop ()
  in
  loop ()

let rec skip_ws st =
  (* Skip whitespace and comments *)
  match peek st with
  | Some c when is_ws c ->
      ignore (next st);
      skip_ws st
  | Some '/' -> (
      match at st (st.i + 1) with
      | Some '*' ->
          ignore (next st);
          (* consume / *)
          ignore (next st);
          (* consume * *)
          skip_comment st;
          skip_ws st
      | _ -> ())
  | _ -> ()

let read_until_char st ~stop =
  (* Read until a stop char (not consuming it), ignoring comments *)
  let b = Buffer.create 64 in
  let rec loop () =
    match peek st with
    | None -> ()
    | Some '/' -> (
        match at st (st.i + 1) with
        | Some '*' ->
            ignore (next st);
            ignore (next st);
            (* skip comment *)
            let rec skipc () =
              match next st with
              | None -> ()
              | Some '*' -> (
                  match peek st with
                  | Some '/' -> ignore (next st)
                  | _ -> skipc ())
              | Some _ -> skipc ()
            in
            skipc ();
            loop ()
        | _ ->
            let _ = next st in
            if stop '/' then ()
            else (
              Buffer.add_char b '/';
              loop ()))
    | Some c ->
        if stop c then ()
        else (
          ignore (next st);
          Buffer.add_char b c;
          loop ())
  in
  loop ();
  Buffer.contents b

let trim s =
  let open Astring in
  String.trim s

(* Helper functions for string manipulation *)
let remove_quotes s =
  let len = String.length s in
  if
    len > 2
    && ((s.[0] = '\'' && s.[len - 1] = '\'')
       || (s.[0] = '"' && s.[len - 1] = '"'))
  then String.sub s 1 (len - 2)
  else s

let function_params fname s =
  (* Extract parameters from function(params) *)
  let prefix = fname ^ "(" in
  if String.starts_with ~prefix s then
    let params_str =
      String.sub s (String.length prefix)
        (String.length s - String.length prefix - 1)
    in
    String.split_on_char ',' params_str |> List.map String.trim
  else []

let has_affix ~affix s = Astring.String.is_infix ~affix s

(* ========================================================================
   State Machine Parsers
   ======================================================================== *)

(* Parse space-separated values that might contain var() or calc() functions *)
(* Uses a state machine to properly handle nested parentheses *)
let space_separated_values_of_string value =
  let len = String.length value in
  let b = Buffer.create 32 in
  let acc = ref [] in
  let add_token () =
    let t = Buffer.contents b |> String.trim in
    if t <> "" then acc := t :: !acc;
    Buffer.clear b
  in
  let rec read_quoted i quote =
    if i >= len then i
    else if value.[i] = quote then (
      Buffer.add_char b quote;
      i + 1)
    else if value.[i] = '\\' && i + 1 < len then (
      Buffer.add_char b '\\';
      Buffer.add_char b value.[i + 1];
      read_quoted (i + 2) quote)
    else (
      Buffer.add_char b value.[i];
      read_quoted (i + 1) quote)
  in
  let rec loop i depth =
    if i >= len then (
      add_token ();
      List.rev !acc)
    else
      match value.[i] with
      | (' ' | '\t' | '\n' | '\r') when depth = 0 ->
          add_token ();
          loop (i + 1) depth
      | ('(' | '[') as c ->
          Buffer.add_char b c;
          loop (i + 1) (depth + 1)
      | (')' | ']') as c when depth > 0 ->
          Buffer.add_char b c;
          loop (i + 1) (depth - 1)
      | '"' ->
          Buffer.add_char b '"';
          let i' = read_quoted (i + 1) '"' in
          loop i' depth
      | '\'' ->
          Buffer.add_char b '\'';
          let i' = read_quoted (i + 1) '\'' in
          loop i' depth
      | c ->
          Buffer.add_char b c;
          loop (i + 1) depth
  in
  loop 0 0

(* Use dedicated Selector module to parse selector strings. *)
let selector_of_string s =
  let r = Reader.of_string s in
  Selector.one_opt r

let read_selector st : Css.Selector.t option =
  skip_ws st;
  let sel = read_until_char st ~stop:(fun c -> c = '{') in
  let has_open = match peek st with Some '{' -> true | _ -> false in
  let trimmed_sel = trim sel in
  if trimmed_sel = "" then None
  else if String.length trimmed_sel > 0 && trimmed_sel.[0] = '@' then None
  else
    match selector_of_string trimmed_sel with
    | Some s ->
        if has_open then ignore (next st);
        Some s
    | None -> None

type value_state = { depth : int; in_s : bool; in_d : bool; stop : bool }

let handle_escape st b =
  match next st with
  | None -> ()
  | Some c2 ->
      Buffer.add_char b '\\';
      Buffer.add_char b c2

let read_value_step st b c (stt : value_state) : value_state =
  match (stt.in_s, stt.in_d, c) with
  | true, _, '"' ->
      Buffer.add_char b c;
      { stt with in_s = false }
  | _, true, '\'' ->
      Buffer.add_char b c;
      { stt with in_d = false }
  | true, _, '\\' ->
      handle_escape st b;
      stt
  | _, true, '\\' ->
      handle_escape st b;
      stt
  | true, _, _ ->
      Buffer.add_char b c;
      stt
  | _, true, _ ->
      Buffer.add_char b c;
      stt
  | false, false, '"' ->
      Buffer.add_char b c;
      { stt with in_s = true }
  | false, false, '\'' ->
      Buffer.add_char b c;
      { stt with in_d = true }
  | false, false, '(' ->
      Buffer.add_char b c;
      { stt with depth = stt.depth + 1 }
  | false, false, '[' ->
      Buffer.add_char b c;
      { stt with depth = stt.depth + 1 }
  | false, false, ')' when stt.depth > 0 ->
      Buffer.add_char b c;
      { stt with depth = stt.depth - 1 }
  | false, false, ']' when stt.depth > 0 ->
      Buffer.add_char b c;
      { stt with depth = stt.depth - 1 }
  | false, false, ';' when stt.depth = 0 -> { stt with stop = true }
  | false, false, '}' when stt.depth = 0 ->
      st.i <- st.i - 1;
      { stt with stop = true }
  | false, false, '/' -> (
      match peek st with
      | Some '*' ->
          ignore (next st);
          skip_comment st;
          Buffer.add_char b ' ';
          stt
      | _ ->
          Buffer.add_char b c;
          stt)
  | _ ->
      Buffer.add_char b c;
      stt

let rec read_value_into_buffer st b ~depth ~in_s ~in_d =
  match next st with
  | None -> ()
  | Some c ->
      let stt = read_value_step st b c { depth; in_s; in_d; stop = false } in
      if stt.stop then ()
      else
        read_value_into_buffer st b ~depth:stt.depth ~in_s:stt.in_s
          ~in_d:stt.in_d

let read_value st =
  let b = Buffer.create 64 in
  read_value_into_buffer st b ~depth:0 ~in_s:false ~in_d:false;
  trim (Buffer.contents b)

(* ======================================================================== CSS
   Value Type Parsers
   ======================================================================== *)

(* Parse var() to extract variable name and optional fallback *)
let var_of_string str =
  let s = String.trim str in
  if not (String.starts_with ~prefix:"var(" s && String.ends_with ~suffix:")" s)
  then None
  else
    (* Extract content between var( and ) *)
    let content = String.sub s 4 (String.length s - 5) in
    (* State machine to parse: var(--name) or var(--name, fallback) *)
    let len = String.length content in
    let rec find_comma i depth in_str quote_char =
      if i >= len then None
      else
        match content.[i] with
        | ',' when depth = 0 && not in_str -> Some i
        | '(' when not in_str ->
            find_comma (i + 1) (depth + 1) in_str quote_char
        | ')' when (not in_str) && depth > 0 ->
            find_comma (i + 1) (depth - 1) in_str quote_char
        | ('"' | '\'') as q when not in_str -> find_comma (i + 1) depth true q
        | c when in_str && c = quote_char -> find_comma (i + 1) depth false ' '
        | '\\' when in_str && i + 1 < len ->
            find_comma (i + 2) depth in_str quote_char
        | _ -> find_comma (i + 1) depth in_str quote_char
    in
    match find_comma 0 0 false ' ' with
    | None ->
        (* No fallback, just var(--name) *)
        let var_name = String.trim content in
        (* Remove -- prefix if present *)
        let var_name =
          if String.starts_with ~prefix:"--" var_name then
            String.sub var_name 2 (String.length var_name - 2)
          else var_name
        in
        Some (var_name, None)
    | Some comma_pos ->
        (* Has fallback: var(--name, fallback) *)
        let var_name = String.sub content 0 comma_pos |> String.trim in
        (* Remove -- prefix if present *)
        let var_name =
          if String.starts_with ~prefix:"--" var_name then
            String.sub var_name 2 (String.length var_name - 2)
          else var_name
        in
        let fallback =
          String.sub content (comma_pos + 1) (len - comma_pos - 1)
          |> String.trim
        in
        Some (var_name, Some fallback)

(* ========================================================================
   Length Parser
   ======================================================================== *)

let length_keyword_of_string : string -> Css.length option = function
  | "auto" -> Some Css.Auto
  | "inherit" -> Some Css.Inherit
  | "fit-content" -> Some Css.Fit_content
  | "max-content" -> Some Css.Max_content
  | "min-content" -> Some Css.Min_content
  | "from-font" -> Some Css.From_font
  | "0" | "0px" -> Some Css.Zero
  | _ -> None

let length_unit_of_string v =
  let open String in
  let take n suf =
    let num = sub v 0 (length v - n) |> Stdlib.float_of_string in
    match suf with
    | "px" -> Css.Px (int_of_float num)
    | "rem" -> Css.Rem num
    | "em" -> Css.Em num
    | "%" -> Css.Pct num
    | "vw" -> Css.Vw num
    | "vh" -> Css.Vh num
    | "ch" -> Css.Ch num
    | "lh" -> Css.Lh num
    | _ -> Css.Num num
  in
  if String.ends_with ~suffix:"px" v then Some (take 2 "px")
  else if String.ends_with ~suffix:"rem" v then Some (take 3 "rem")
  else if String.ends_with ~suffix:"em" v then Some (take 2 "em")
  else if String.ends_with ~suffix:"vw" v then Some (take 2 "vw")
  else if String.ends_with ~suffix:"vh" v then Some (take 2 "vh")
  else if String.ends_with ~suffix:"ch" v then Some (take 2 "ch")
  else if String.ends_with ~suffix:"lh" v then Some (take 2 "lh")
  else if String.ends_with ~suffix:"%" v then Some (take 1 "%")
  else None

let rec length_of_string value : Css.length =
  let v = String.trim value in
  match length_keyword_of_string v with
  | Some k -> k
  | None -> (
      if String.starts_with ~prefix:"var(" v then
        match var_of_string v with
        | None -> failwith "length_of_string"
        | Some (var_name, None) -> Css.Var (Css.var_ref var_name)
        | Some (var_name, Some fb) ->
            let fb_len = length_of_string fb in
            Css.Var (Css.var_ref ~fallback:fb_len var_name)
      else if String.starts_with ~prefix:"calc(" v then Css.Zero
      else
        match length_unit_of_string v with
        | Some u -> u
        | None -> (
            try Css.Num (Stdlib.float_of_string v)
            with Failure _ ->
              err_not_implement ("length: invalid value '" ^ v ^ "'")))

(* ========================================================================
   Color Parser
   ======================================================================== *)

let color_name_of_string v =
  match String.lowercase_ascii v with
  | "red" -> Css.Red
  | "blue" -> Css.Blue
  | "green" -> Css.Green
  | "white" -> Css.White
  | "black" -> Css.Black
  | "yellow" -> Css.Yellow
  | "cyan" -> Css.Cyan
  | "magenta" -> Css.Magenta
  | "gray" -> Css.Gray
  | "grey" -> Css.Grey
  | "orange" -> Css.Orange
  | "purple" -> Css.Purple
  | "pink" -> Css.Pink
  | "silver" -> Css.Silver
  | "maroon" -> Css.Maroon
  | "fuchsia" -> Css.Fuchsia
  | "lime" -> Css.Lime
  | "olive" -> Css.Olive
  | "navy" -> Css.Navy
  | "teal" -> Css.Teal
  | "aqua" -> Css.Aqua
  | _ -> failwith ("Unknown color: " ^ v)

let color_rgb_of_string v =
  let open String in
  let inside =
    let start = if starts_with ~prefix:"rgba(" v then 5 else 4 in
    sub v start (length v - start - 1)
  in
  let parts = inside |> split_on_char ',' |> List.map String.trim in
  let clamp x min_v max_v = max min_v (min max_v x) in
  let parse_chan s =
    if Astring.String.is_suffix ~affix:"%" s then
      let pct = sub s 0 (length s - 1) |> Stdlib.float_of_string in
      int_of_float (clamp (pct *. 2.55) 0. 255.)
    else Stdlib.int_of_string s |> clamp 0 255
  in
  let parse_alpha s =
    if Astring.String.is_suffix ~affix:"%" s then
      let pct = sub s 0 (length s - 1) |> Stdlib.float_of_string in
      clamp (pct /. 100.) 0. 1.
    else Stdlib.float_of_string s |> clamp 0. 1.
  in
  match parts with
  | [ r; g; b ] ->
      Some (Css.Rgb { r = parse_chan r; g = parse_chan g; b = parse_chan b })
  | [ r; g; b; a ] ->
      Some
        (Css.Rgba
           {
             r = parse_chan r;
             g = parse_chan g;
             b = parse_chan b;
             a = parse_alpha a;
           })
  | _ -> None

let rec color_of_string value : Css.color =
  let v = String.trim value in
  let vl = String.lowercase_ascii v in
  if vl = "transparent" then Css.Transparent
  else if vl = "currentcolor" || vl = "current" then Css.Current
  else if vl = "inherit" then Css.Inherit
  else if String.starts_with ~prefix:"var(" v then
    match var_of_string v with
    | None -> Css.Hex { hash = false; value = "000000" }
    | Some (var_name, None) -> Css.Var (Css.var_ref var_name)
    | Some (var_name, Some fb) ->
        let fb_color = color_of_string fb in
        Css.Var (Css.var_ref ~fallback:fb_color var_name)
  else if String.starts_with ~prefix:"#" v then
    let hex = String.sub v 1 (String.length v - 1) in
    Css.Hex { hash = true; value = hex }
  else if
    String.starts_with ~prefix:"rgb(" v || String.starts_with ~prefix:"rgba(" v
  then
    match color_rgb_of_string v with
    | Some c -> c
    | None -> err_not_implement "color: invalid rgb/rgba() arguments"
  else Css.Named (color_name_of_string v)

(* ======================================================================== Enum
   Type Parsers
   ======================================================================== *)

(* Parse text-decoration shorthand we support *)
let text_decoration_of_string (value : string) : Css.text_decoration =
  match String.trim value with
  | "none" -> Css.None
  | "underline" -> Css.Underline
  | "overline" -> Css.Overline
  | "line-through" -> Css.Line_through
  | "inherit" -> Css.Inherit
  | "underline dotted" -> Css.Underline_dotted
  | _ -> Css.Underline

(* Parse text-decoration-style *)
let text_decoration_style_of_string (value : string) : Css.text_decoration_style
    =
  match String.lowercase_ascii (String.trim value) with
  | "solid" -> Css.Solid
  | "double" -> Css.Double
  | "dotted" -> Css.Dotted
  | "dashed" -> Css.Dashed
  | "wavy" -> Css.Wavy
  | "inherit" -> Css.Inherit
  | _ -> Css.Solid

(* Parse text-transform *)
let text_transform_of_string (value : string) : Css.text_transform =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "uppercase" -> Css.Uppercase
  | "lowercase" -> Css.Lowercase
  | "capitalize" -> Css.Capitalize
  | "full-width" -> Css.Full_width
  | "full-size-kana" -> Css.Full_size_kana
  | "inherit" -> Css.Inherit
  | _ -> Css.None

(* Parse white-space *)
let white_space_of_string (value : string) : Css.white_space =
  match String.lowercase_ascii (String.trim value) with
  | "normal" -> Css.Normal
  | "nowrap" -> Css.Nowrap
  | "pre" -> Css.Pre
  | "pre-wrap" -> Css.Pre_wrap
  | "pre-line" -> Css.Pre_line
  | "break-spaces" -> Css.Break_spaces
  | "inherit" -> Css.Inherit
  | _ -> Css.Normal

(* Parse table-layout *)
let table_layout_of_string (value : string) : Css.table_layout =
  match String.lowercase_ascii (String.trim value) with
  | "auto" -> Css.Auto
  | "fixed" -> Css.Fixed
  | "inherit" -> Css.Inherit
  | _ -> Css.Auto

(* Parse appearance *)
let appearance_of_string (value : string) : Css.appearance =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "auto" -> Css.Auto
  | "button" -> Css.Button
  | "textfield" -> Css.Textfield
  | "menulist" -> Css.Menulist
  | "inherit" -> Css.Inherit
  | _ -> Css.Auto

(* Parse overflow/overflow-x/overflow-y *)
let _overflow_of_string (value : string) : Css.overflow =
  match String.lowercase_ascii (String.trim value) with
  | "visible" -> Css.Visible
  | "hidden" -> Css.Hidden
  | "scroll" -> Css.Scroll
  | "auto" -> Css.Auto
  | "clip" -> Css.Clip
  | _ -> Css.Auto

(* Parse clear *)
let clear_of_string (value : string) : Css.clear =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "left" -> Css.Left
  | "right" -> Css.Right
  | "both" -> Css.Both
  | _ -> Css.None

(* Parse float side *)
let float_side_of_string (value : string) : Css.float_side =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "left" -> Css.Left
  | "right" -> Css.Right
  | _ -> Css.None

(* Parse outline-style *)
let outline_style_of_string (value : string) : Css.outline_style =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "auto" -> Css.Auto
  | "dotted" -> Css.Dotted
  | "dashed" -> Css.Dashed
  | "solid" -> Css.Solid
  | "double" -> Css.Double
  | "groove" -> Css.Groove
  | "ridge" -> Css.Ridge
  | "inset" -> Css.Inset
  | "outset" -> Css.Outset
  | "inherit" -> Css.Inherit
  | _ -> Css.Solid

(* Parse font-style *)
let font_style_of_string (value : string) : Css.font_style =
  match String.lowercase_ascii (String.trim value) with
  | "normal" -> Css.Normal
  | "italic" -> Css.Italic
  | "oblique" -> Css.Oblique
  | "inherit" -> Css.Inherit
  | _ -> Css.Normal

(* (removed) Simple font-feature-settings helper; replaced by the recursive
   [parse_font_feature_settings] implementation below. *)

(* Extract function name and parse argument from a CSS function call *)
let try_function_parser (k : string -> string -> 'a option) (str : string) :
    'a option =
  match String.index_opt str '(' with
  | None -> None
  | Some open_idx -> (
      match String.index_opt str ')' with
      | None -> None
      | Some close_idx when close_idx > open_idx ->
          let name = String.sub str 0 open_idx in
          let arg_str =
            String.sub str (open_idx + 1) (close_idx - open_idx - 1)
          in
          k (String.trim name) (String.trim arg_str)
      | _ -> None)

(* Read float from string, fail on error *)
let float_of_raw_string (str : string) : float =
  try float_of_string str with Failure _ -> failwith ("Invalid float: " ^ str)

(* Read number with type-driven approach *)
let number_of_string (value : string) : Css.number =
  let v = String.trim value in
  if String.ends_with v ~suffix:"%" then
    let num = String.sub v 0 (String.length v - 1) in
    Css.Pct (float_of_raw_string num)
  else
    (* Try int first, then float *)
    try
      let int_val = int_of_string v in
      Css.Int int_val
    with Failure _ -> Css.Float (float_of_raw_string v)

(* Read length from string *)
(* read_length is just an alias for length_of_string *)

(* Read angle *)
let angle_of_string (value : string) : Css.angle =
  let r = Reader.of_string (String.trim value) in
  let num, unit = Reader.angle r in
  match String.lowercase_ascii unit with
  | "deg" -> Css.Deg num
  | "rad" -> Css.Rad num
  | "grad" -> Css.Grad num
  | "turn" -> Css.Turn num
  | "" -> Css.Deg num (* Default to degrees if no unit *)
  | _ -> failwith ("Invalid angle unit: " ^ unit)

(* Read duration *)
let duration_of_string (value : string) : Css.duration =
  let r = Reader.of_string (String.trim value) in
  let num, unit = Reader.duration r in
  match String.lowercase_ascii unit with
  | "s" -> Css.S num
  | "ms" -> Css.Ms (int_of_float num)
  | _ -> failwith ("Invalid duration unit: " ^ unit)

(* Read timing function *)
let timing_function_of_string (value : string) : Css.timing_function =
  match String.lowercase_ascii (String.trim value) with
  | "ease" -> Css.Ease
  | "linear" -> Css.Linear
  | "ease-in" -> Css.Ease_in
  | "ease-out" -> Css.Ease_out
  | "ease-in-out" -> Css.Ease_in_out
  | "step-start" -> Css.Step_start
  | "step-end" -> Css.Step_end
  | v when String.starts_with v ~prefix:"steps(" ->
      (* Parse steps(n, start|end) *)
      Css.Ease
      (* Simplified for now *)
  | v when String.starts_with v ~prefix:"cubic-bezier(" ->
      (* Parse cubic-bezier(x1, y1, x2, y2) *)
      Css.Ease
      (* Simplified for now *)
  | _ -> Css.Ease

(* Parse a single filter function like "blur(5px)" or "brightness(0.5)" *)
let filter_function_of_string (str : string) : Css.filter option =
  try_function_parser
    (fun name arg ->
      match name with
      | "blur" -> Some (Css.Blur (length_of_string arg))
      | "brightness" -> Some (Css.Brightness (number_of_string arg))
      | "contrast" -> Some (Css.Contrast (number_of_string arg))
      | "grayscale" -> Some (Css.Grayscale (number_of_string arg))
      | "invert" -> Some (Css.Invert (number_of_string arg))
      | "saturate" -> Some (Css.Saturate (number_of_string arg))
      | "sepia" -> Some (Css.Sepia (number_of_string arg))
      | "hue-rotate" -> Some (Css.Hue_rotate (angle_of_string arg))
      | _ -> None)
    str

(* Parse multiple filter functions from a string *)
let filter_list_of_string (value : string) : Css.filter list =
  (* Split by spaces, but need to handle functions with spaces in args *)
  let rec parse_filters (str : string) (acc : Css.filter list) : Css.filter list
      =
    let str = String.trim str in
    if str = "" then List.rev acc
    else
      (* Find the next function *)
      match String.index_opt str '(' with
      | None -> List.rev acc (* No more functions *)
      | Some open_idx -> (
          match String.index_opt str ')' with
          | None -> List.rev acc (* Malformed *)
          | Some close_idx ->
              if close_idx > open_idx then
                let func_str = String.sub str 0 (close_idx + 1) in
                let rest =
                  if close_idx + 1 < String.length str then
                    String.sub str (close_idx + 1)
                      (String.length str - close_idx - 1)
                  else ""
                in
                match filter_function_of_string func_str with
                | Some filter -> parse_filters rest (filter :: acc)
                | None -> parse_filters rest acc
              else List.rev acc (* Malformed - close_idx <= open_idx *))
  in
  parse_filters value []

(* Additional type-driven parsers for CSS values *)

(* Parse visibility *)
let _visibility_of_string (value : string) : Css.visibility =
  match String.lowercase_ascii (String.trim value) with
  | "visible" -> Css.Visible
  | "hidden" -> Css.Hidden
  | "collapse" -> Css.Collapse
  | _ -> failwith ("Invalid visibility: " ^ value)

(* Parse overflow *)
let overflow_of_string (value : string) : Css.overflow =
  match String.lowercase_ascii (String.trim value) with
  | "visible" -> Css.Visible
  | "hidden" -> Css.Hidden
  | "scroll" -> Css.Scroll
  | "auto" -> Css.Auto
  | "clip" -> Css.Clip
  | _ -> failwith ("Invalid overflow: " ^ value)

(* Parse text_align *)
let _text_align_of_string (value : string) : Css.text_align =
  match String.lowercase_ascii (String.trim value) with
  | "left" -> Css.Left
  | "right" -> Css.Right
  | "center" -> Css.Center
  | "justify" -> Css.Justify
  | "start" -> Css.Start
  | "end" -> Css.End
  | "inherit" -> Css.Inherit
  | _ -> failwith ("Invalid text-align: " ^ value)

(* Parse flex_direction *)
let _flex_direction_of_string (value : string) : Css.flex_direction =
  match String.lowercase_ascii (String.trim value) with
  | "row" -> Css.Row
  | "row-reverse" -> Css.Row_reverse
  | "column" -> Css.Column
  | "column-reverse" -> Css.Column_reverse
  | _ -> failwith ("Invalid flex-direction: " ^ value)

(* Parse align_items *)
let _align_items_of_string (value : string) : Css.align_items =
  match String.lowercase_ascii (String.trim value) with
  | "flex-start" -> Css.Flex_start
  | "flex-end" -> Css.Flex_end
  | "center" -> Css.Center
  | "baseline" -> Css.Baseline
  | "stretch" -> Css.Stretch
  | _ -> failwith ("Invalid align-items: " ^ value)

(* Parse animation - handles shorthand like "name duration timing-function delay
   iteration-count direction fill-mode" *)
let rec animation_parse_parts parts anim =
  match parts with
  | [] -> anim
  | part :: rest ->
      let part_lower = String.lowercase_ascii part in
      if part_lower = "infinite" then
        animation_parse_parts rest
          { anim with Css.iteration_count = Some Css.Infinite }
      else if part_lower = "alternate" then
        animation_parse_parts rest
          { anim with Css.direction = Some Css.Alternate }
      else if part_lower = "alternate-reverse" then
        animation_parse_parts rest
          { anim with Css.direction = Some Css.Alternate_reverse }
      else if part_lower = "reverse" then
        animation_parse_parts rest
          { anim with Css.direction = Some Css.Reverse }
      else if part_lower = "normal" then
        animation_parse_parts rest { anim with Css.direction = Some Css.Normal }
      else if part_lower = "forwards" then
        animation_parse_parts rest
          { anim with Css.fill_mode = Some Css.Forwards }
      else if part_lower = "backwards" then
        animation_parse_parts rest
          { anim with Css.fill_mode = Some Css.Backwards }
      else if part_lower = "both" then
        animation_parse_parts rest { anim with Css.fill_mode = Some Css.Both }
      else if part_lower = "none" then
        animation_parse_parts rest { anim with Css.fill_mode = Some Css.None }
      else if part_lower = "running" then
        animation_parse_parts rest
          { anim with Css.play_state = Some Css.Running }
      else if part_lower = "paused" then
        animation_parse_parts rest
          { anim with Css.play_state = Some Css.Paused }
      else if
        String.ends_with part ~suffix:"s" || String.ends_with part ~suffix:"ms"
      then
        let dur = duration_of_string part in
        if anim.Css.duration = None then
          animation_parse_parts rest { anim with Css.duration = Some dur }
        else animation_parse_parts rest { anim with Css.delay = Some dur }
      else if
        List.mem part_lower
          [
            "ease";
            "linear";
            "ease-in";
            "ease-out";
            "ease-in-out";
            "step-start";
            "step-end";
          ]
      then
        animation_parse_parts rest
          {
            anim with
            Css.timing_function = Some (timing_function_of_string part);
          }
      else if
        String.for_all (fun c -> Char.code c >= 48 && Char.code c <= 57) part
      then
        let count = try int_of_string part with Failure _ -> 1 in
        animation_parse_parts rest
          { anim with Css.iteration_count = Some (Css.Count count) }
      else animation_parse_parts rest { anim with Css.name = Some part }

let animation_of_string (value : string) : Css.animation =
  let parts =
    String.split_on_char ' ' (String.trim value)
    |> List.filter (fun s -> s <> "")
  in
  animation_parse_parts parts
    {
      Css.name = None;
      Css.duration = None;
      Css.timing_function = None;
      Css.delay = None;
      Css.iteration_count = None;
      Css.direction = None;
      Css.fill_mode = None;
      Css.play_state = None;
    }

(* Parse filter - handles functions like blur(5px), brightness(0.5), etc. *)
let filter_of_string (value : string) : Css.filter =
  let v = String.trim value in
  match String.lowercase_ascii v with
  | "none" -> Css.None
  | _ -> (
      match filter_list_of_string v with
      | [] -> Css.None
      | [ f ] -> f
      | fs -> Css.List fs)

(* Parse box-shadow - handles offsets, blur, spread, color, and inset *)
let box_shadow_of_string (value : string) : Css.box_shadow =
  let v = String.trim value in
  match String.lowercase_ascii v with
  | "none" -> Css.None
  | _ ->
      (* Format: [inset] h-offset v-offset [blur] [spread] [color] *)
      (* For now, just return None for complex shadows - proper parsing needed *)
      Css.None

(* Parse background-size *)
let background_size_of_string (value : string) : Css.background_size =
  match String.lowercase_ascii (String.trim value) with
  | "auto" -> Css.Auto
  | "cover" -> Css.Cover
  | "contain" -> Css.Contain
  | "inherit" -> Css.Inherit
  | v -> (
      if
        (* Try to parse as percentage or length *)
        String.ends_with v ~suffix:"%"
      then
        try
          let pct = float_of_string (String.sub v 0 (String.length v - 1)) in
          Css.Percentage pct
        with Failure _ -> Css.Auto
      else
        (* Could be single length or two lengths for Size *)
        try Css.Length (length_of_string v) with Failure _ -> Css.Auto)

(* Parse line-height *)
let line_height_of_string (value : string) : Css.line_height =
  let v = String.trim value in
  match String.lowercase_ascii v with
  | "normal" -> Css.Normal
  | "inherit" -> Css.Inherit
  | _ -> (
      if String.ends_with v ~suffix:"%" then
        try
          let pct = float_of_string (String.sub v 0 (String.length v - 1)) in
          Css.Percentage pct
        with Failure _ -> Css.Normal
      else
        try
          (* Try parsing as a number first *)
          let n = float_of_string v in
          Css.Number n
        with Failure _ ->
          (* Otherwise parse as length *)
          Css.Length (length_of_string v))

(* Align-items / Align-self / Justify-items / Justify-self / Align (content) *)
let align_items_of_string (value : string) : Css.align_items =
  match String.lowercase_ascii (String.trim value) with
  | "flex-start" -> Css.Flex_start
  | "flex-end" -> Css.Flex_end
  | "center" -> Css.Center
  | "baseline" -> Css.Baseline
  | "stretch" -> Css.Stretch
  | v -> err_unknown_value "align-items" v

let align_self_of_string (value : string) : Css.align_self =
  match String.lowercase_ascii (String.trim value) with
  | "auto" -> Css.Auto
  | "flex-start" -> Css.Flex_start
  | "flex-end" -> Css.Flex_end
  | "center" -> Css.Center
  | "baseline" -> Css.Baseline
  | "stretch" -> Css.Stretch
  | v -> err_unknown_value "align-self" v

let justify_items_of_string (value : string) : Css.justify =
  match String.lowercase_ascii (String.trim value) with
  | "auto" -> Css.Auto
  | "start" -> Css.Start
  | "end" -> Css.End
  | "center" -> Css.Center
  | "stretch" -> Css.Stretch
  | "flex-start" -> Css.Flex_start
  | "flex-end" -> Css.Flex_end
  | v -> err_unknown_value "justify-items" v

let align_of_string (value : string) : Css.align =
  match String.lowercase_ascii (String.trim value) with
  | "flex-start" -> Css.Flex_start
  | "flex-end" -> Css.Flex_end
  | "center" -> Css.Center
  | "space-between" -> Css.Space_between
  | "space-around" -> Css.Space_around
  | "space-evenly" -> Css.Space_evenly
  | "stretch" -> Css.Stretch
  | "start" -> Css.Start
  | "end" -> Css.End
  | "baseline" -> Css.Baseline
  | "auto" -> Css.Auto
  | v -> err_unknown_value "align" v

let justify_self_of_string (value : string) : Css.justify =
  justify_items_of_string value

(* Parse user-select *)
let user_select_of_string (value : string) : Css.user_select =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "auto" -> Css.Auto
  | "text" -> Css.Text
  | "all" -> Css.All
  | "contain" -> Css.Contain
  | _ -> Css.Auto

(* Parse pointer-events *)
let pointer_events_of_string (value : string) : Css.pointer_events =
  match String.trim value with
  | "auto" -> Css.Auto
  | "none" -> Css.None
  | "visiblePainted" -> Css.Visible_painted
  | "visibleFill" -> Css.Visible_fill
  | "visibleStroke" -> Css.Visible_stroke
  | "visible" -> Css.Visible
  | "painted" -> Css.Painted
  | "fill" -> Css.Fill
  | "stroke" -> Css.Stroke
  | "all" -> Css.All
  | "inherit" -> Css.Inherit
  | _ -> Css.Auto

(* Object-fit *)
let object_fit_of_string (value : string) : Css.object_fit =
  match String.lowercase_ascii (String.trim value) with
  | "fill" -> Css.Fill
  | "contain" -> Css.Contain
  | "cover" -> Css.Cover
  | "none" -> Css.None
  | "scale-down" -> Css.Scale_down
  | "inherit" -> Css.Inherit
  | v -> err_unknown_value "object-fit" v

(* Resize *)
let resize_of_string (value : string) : Css.resize =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "both" -> Css.Both
  | "horizontal" -> Css.Horizontal
  | "vertical" -> Css.Vertical
  | "block" -> Css.Block
  | "inline" -> Css.Inline
  | "inherit" -> Css.Inherit
  | _ -> Css.None

(* Scroll behavior *)
let scroll_behavior_of_string (value : string) : Css.scroll_behavior =
  match String.lowercase_ascii (String.trim value) with
  | "auto" -> Css.Auto
  | "smooth" -> Css.Smooth
  | "inherit" -> Css.Inherit
  | _ -> Css.Auto

(* Background-repeat *)
let background_repeat_of_string (value : string) : Css.background_repeat =
  match String.lowercase_ascii (String.trim value) with
  | "repeat" -> Css.Repeat
  | "repeat-x" -> Css.Repeat_x
  | "repeat-y" -> Css.Repeat_y
  | "no-repeat" -> Css.No_repeat
  | "space" -> Css.Space
  | "round" -> Css.Round
  | "inherit" -> Css.Inherit
  | _ -> Css.Repeat

(* Text overflow *)
let text_overflow_of_string (value : string) : Css.text_overflow =
  match String.lowercase_ascii (String.trim value) with
  | "clip" -> Css.Clip
  | "ellipsis" -> Css.Ellipsis
  | "inherit" -> Css.Inherit
  | s -> Css.String s

(* Text wrap *)
let text_wrap_of_string (value : string) : Css.text_wrap =
  match String.lowercase_ascii (String.trim value) with
  | "wrap" -> Css.Wrap
  | "nowrap" -> Css.No_wrap
  | "balance" -> Css.Balance
  | "pretty" -> Css.Pretty
  | "inherit" -> Css.Inherit
  | _ -> Css.Wrap

(* Word break *)
let word_break_of_string (value : string) : Css.word_break =
  match String.lowercase_ascii (String.trim value) with
  | "normal" -> Css.Normal
  | "break-all" -> Css.Break_all
  | "keep-all" -> Css.Keep_all
  | "break-word" -> Css.Break_word
  | "inherit" -> Css.Inherit
  | _ -> Css.Normal

(* Overflow wrap *)
let overflow_wrap_of_string (value : string) : Css.overflow_wrap =
  match String.lowercase_ascii (String.trim value) with
  | "normal" -> Css.Normal
  | "anywhere" -> Css.Anywhere
  | "break-word" -> Css.Break_word
  | "inherit" -> Css.Inherit
  | _ -> Css.Normal

(* Hyphens *)
let hyphens_of_string (value : string) : Css.hyphens =
  match String.lowercase_ascii (String.trim value) with
  | "none" -> Css.None
  | "manual" -> Css.Manual
  | "auto" -> Css.Auto
  | "inherit" -> Css.Inherit
  | _ -> Css.Manual

(* Aspect-ratio: auto | <number> | <ratio> *)
let aspect_ratio_of_string (value : string) : Css.aspect_ratio =
  let v = String.trim value in
  if String.lowercase_ascii v = "auto" then Css.Auto
  else if String.contains v '/' then
    let parts = String.split_on_char '/' v |> List.map String.trim in
    match parts with
    | [ w; h ] -> Css.Ratio (float_of_string w, float_of_string h)
    | _ -> Css.Auto
  else
    try
      let f = Stdlib.float_of_string v in
      Css.Ratio (f, 1.0)
      (* Convert single number to ratio n/1 *)
    with Failure _ -> Css.Auto

(* Scale property *)
let scale_of_string (value : string) : Css.scale =
  let v = String.trim value in
  if String.lowercase_ascii v = "none" then Css.None
  else if String.starts_with ~prefix:"var(" v then
    match var_of_string v with
    | None -> Css.String v
    | Some (name, None) -> Css.Vars [ Css.var_ref name ]
    | Some (name, Some _fb) ->
        Css.Vars
          [ Css.var_ref ~fallback:(Css.Num 1. : Css.transform_scale) name ]
  else if Astring.String.is_suffix ~affix:"%" v then
    let p = String.sub v 0 (String.length v - 1) |> Stdlib.float_of_string in
    Css.Pct p
  else try Css.Num (Stdlib.float_of_string v) with Failure _ -> Css.String v

(* Parse font-feature-settings values *)
let rec parse_font_feature_settings (value : string) : Css.font_feature_settings
    =
  let v = String.trim value in
  if v = "normal" then (Normal : Css.font_feature_settings)
  else if v = "inherit" then Inherit
  else if String.starts_with ~prefix:"var(" v then
    (* Parse var() with typed fallback *)
    match var_of_string v with
    | None -> Css.Normal
    | Some (var_name, None) -> Css.Var (Css.var_ref var_name)
    | Some (var_name, Some fallback) ->
        let fb = parse_font_feature_settings fallback in
        Css.Var (Css.var_ref ~fallback:fb var_name)
  else Css.String v

(* Parse font-variation-settings values *)
let rec font_variation_settings_of_string (value : string) :
    Css.font_variation_settings =
  let v = String.trim value in
  if v = "normal" then (Normal : Css.font_variation_settings)
  else if v = "inherit" then Inherit
  else if String.starts_with ~prefix:"var(" v then
    (* Parse var() with typed fallback *)
    match var_of_string v with
    | None -> Css.Normal
    | Some (var_name, None) -> Css.Var (Css.var_ref var_name)
    | Some (var_name, Some fallback) ->
        let fb = font_variation_settings_of_string fallback in
        Css.Var (Css.var_ref ~fallback:fb var_name)
  else Css.String v

(* Parse display values *)
let display_of_string value : Css.display =
  match String.trim value with
  | "block" -> Block
  | "inline" -> Inline
  | "inline-block" -> Inline_block
  | "flex" -> Flex
  | "inline-flex" -> Inline_flex
  | "grid" -> Grid
  | "inline-grid" -> Inline_grid
  | "none" -> None
  | "table" -> Table
  | _ -> Block

(* Parse position values *)
let position_of_string value : Css.position =
  match String.trim value with
  | "static" -> Css.Static
  | "relative" -> Css.Relative
  | "absolute" -> Css.Absolute
  | "fixed" -> Css.Fixed
  | "sticky" -> Css.Sticky
  | _ -> Css.Static

(* Parse box-sizing values *)
let box_sizing_of_string value : Css.box_sizing =
  match String.trim value with
  | "border-box" -> Css.Border_box
  | "content-box" -> Css.Content_box
  | "inherit" -> Css.Inherit
  | _ -> Css.Border_box

(* Parse font-weight values *)
let font_weight_of_string value : Css.font_weight =
  match String.trim value with
  | "normal" -> Css.Normal
  | "bold" -> Css.Bold
  | "bolder" -> Css.Bolder
  | "lighter" -> Css.Lighter
  | "inherit" -> Css.Inherit
  | v -> (
      try
        let weight = int_of_string v in
        Css.Weight weight
      with Failure _ -> Css.Normal)

(* Parse border-style values *)
let border_style_of_string value : Css.border_style =
  match String.trim value with
  | "none" -> Css.None
  | "solid" -> Css.Solid
  | "dashed" -> Css.Dashed
  | "dotted" -> Css.Dotted
  | "double" -> Css.Double
  | "groove" -> Css.Groove
  | "ridge" -> Css.Ridge
  | "inset" -> Css.Inset
  | "outset" -> Css.Outset
  | "hidden" -> Css.Hidden
  | _ -> Css.None

(* Parse visibility values *)
let visibility_of_string value : Css.visibility =
  match String.trim value with
  | "visible" -> Css.Visible
  | "hidden" -> Css.Hidden
  | "collapse" -> Css.Collapse
  | _ -> Css.Visible

(* Parse text-align values *)
let text_align_of_string value : Css.text_align =
  match String.trim value with
  | "left" -> Css.Left
  | "right" -> Css.Right
  | "center" -> Css.Center
  | "justify" -> Css.Justify
  | "start" -> Css.Start
  | "end" -> Css.End
  | "inherit" -> Css.Inherit
  | _ -> Css.Left

(* Parse z-index values *)

(* Parse vertical-align values *)
let vertical_align_of_string (value : string) : Css.vertical_align =
  let v = String.trim value in
  match String.lowercase_ascii v with
  | "baseline" -> Css.Baseline
  | "top" -> Css.Top
  | "middle" -> Css.Middle
  | "bottom" -> Css.Bottom
  | "text-top" -> Css.Text_top
  | "text-bottom" -> Css.Text_bottom
  | "sub" -> Css.Sub
  | "super" -> Css.Super
  | "inherit" -> Css.Inherit
  | _ ->
      if Astring.String.is_suffix ~affix:"%" v then
        let num =
          String.sub v 0 (String.length v - 1) |> Stdlib.float_of_string
        in
        Css.Percentage num
      else
        let len = length_of_string v in
        Css.Length len

(* Parse border-collapse values *)
let border_collapse_of_string (value : string) : Css.border_collapse =
  match String.lowercase_ascii (String.trim value) with
  | "collapse" -> Css.Collapse
  | "separate" -> Css.Separate
  | "inherit" -> Css.Inherit
  | _ -> Css.Collapse

let z_index_of_string value : Css.z_index =
  let v = String.trim value in
  if v = "auto" then Css.Auto
  else
    try
      let idx = int_of_string v in
      Css.Index idx
    with Failure _ -> err_not_implement ("z-index: invalid value '" ^ v ^ "'")

(* Parse float values *)
let float_of_string value : float =
  (* Safe float parser: trims and tolerates trailing '%' by normalizing to
     0.0-1.0 *)
  let v = String.trim value in
  try
    if Astring.String.is_suffix ~affix:"%" v then
      let pct =
        String.sub v 0 (String.length v - 1) |> Stdlib.float_of_string
      in
      pct /. 100.
    else Stdlib.float_of_string v
  with Failure _ -> 0.0

(* Parse content values *)
let content_of_string value : Css.content =
  let v = String.trim value in
  if v = "none" then Css.None
  else if v = "normal" then Css.Normal
  else Css.String (remove_quotes v)

(* Parse background-image values *)
let background_image_of_string value : Css.background_image =
  let v = String.trim value in
  if v = "none" then Css.None
  else if String.starts_with ~prefix:"url(" v then
    let url = String.sub v 4 (String.length v - 5) in
    (* Remove url() wrapper *)
    Css.Url url
  else if has_affix ~affix:"gradient" v then
    (* Simple gradient parsing - could be more sophisticated *)
    Css.Url v (* Fallback to URL for complex gradients *)
  else Css.Url v

(* Parse justify-content values *)
let justify_content_of_string value : Css.justify_content =
  match String.trim value with
  | "flex-start" | "start" -> Css.Flex_start
  | "flex-end" | "end" -> Css.Flex_end
  | "center" -> Css.Center
  | "space-between" -> Css.Space_between
  | "space-around" -> Css.Space_around
  | "space-evenly" -> Css.Space_evenly
  | _ -> Css.Center

(* Parse angle values *)
let angle_of_string value : Css.angle =
  let v = String.trim value in
  if String.ends_with ~suffix:"deg" v then
    let num = String.sub v 0 (String.length v - 3) |> float_of_string in
    Css.Deg num
  else if String.ends_with ~suffix:"rad" v then
    let num = String.sub v 0 (String.length v - 3) |> float_of_string in
    Css.Rad num
  else if String.ends_with ~suffix:"turn" v then
    let num = String.sub v 0 (String.length v - 4) |> float_of_string in
    Css.Turn num
  else if String.ends_with ~suffix:"grad" v then
    let num = String.sub v 0 (String.length v - 4) |> float_of_string in
    Css.Grad num
  else
    (* Default to degrees *)
    Css.Deg (Stdlib.float_of_string v)

(* ========================================================================
   Complex Type Parsers
   ======================================================================== *)

(* Parse transform functions - handles multiple space-separated functions *)
let single_transform_of_string v =
  let v = String.trim v in
  let parse name f =
    match function_params name v with [] -> None | params -> f params
  in
  if String.starts_with ~prefix:"rotate(" v then
    parse "rotate" (function
      | [ angle_str ] -> Some (Css.Rotate (angle_of_string angle_str))
      | _ -> None)
  else if String.starts_with ~prefix:"translateX(" v then
    parse "translateX" (function
      | [ len_str ] -> Some (Css.Translate_x (length_of_string len_str))
      | _ -> None)
  else if String.starts_with ~prefix:"translateY(" v then
    parse "translateY" (function
      | [ len_str ] -> Some (Css.Translate_y (length_of_string len_str))
      | _ -> None)
  else if String.starts_with ~prefix:"translate(" v then
    parse "translate" (function
      | [ x ] -> Some (Css.Translate (length_of_string x, None))
      | [ x; y ] ->
          Some (Css.Translate (length_of_string x, Some (length_of_string y)))
      | _ -> None)
  else if String.starts_with ~prefix:"scale(" v then
    parse "scale" (function
      | [ s ] ->
          let scale_val = Stdlib.float_of_string (String.trim s) in
          Some (Css.Scale (Css.Num scale_val, None))
      | [ sx; sy ] ->
          let sx_val = Stdlib.float_of_string (String.trim sx) in
          let sy_val = Stdlib.float_of_string (String.trim sy) in
          Some (Css.Scale (Css.Num sx_val, Some (Css.Num sy_val)))
      | _ -> None)
  else None

let transform_of_string value : Css.transform list =
  space_separated_values_of_string value
  |> List.filter_map single_transform_of_string

(* Parse font-family values *)
let font_family_of_string value : Css.font_family list =
  (* Split by comma and parse each family *)
  let families = String.split_on_char ',' value in
  List.filter_map
    (fun family ->
      let f = remove_quotes (String.trim family) in
      match String.lowercase_ascii f with
      | "sans-serif" | "sans serif" -> Some Css.Sans_serif
      | "serif" -> Some Css.Serif
      | "monospace" -> Some Css.Monospace
      | "cursive" -> Some Css.Cursive
      | "fantasy" -> Some Css.Fantasy
      | "system-ui" -> Some Css.System_ui
      | "ui-sans-serif" -> Some Css.Ui_sans_serif
      | "ui-serif" -> Some Css.Ui_serif
      | "ui-monospace" -> Some Css.Ui_monospace
      | "ui-rounded" -> Some Css.Ui_rounded
      | "emoji" -> Some Css.Emoji
      | "math" -> Some Css.Math
      | "fangsong" -> Some Css.Fangsong
      | "inter" -> Some Css.Inter
      | "roboto" -> Some Css.Roboto
      | "open sans" | "opensans" -> Some Css.Open_sans
      | "lato" -> Some Css.Lato
      | "montserrat" -> Some Css.Montserrat
      | "poppins" -> Some Css.Poppins
      | "source sans pro" -> Some Css.Source_sans_pro
      | "raleway" -> Some Css.Raleway
      | "oswald" -> Some Css.Oswald
      | "noto sans" -> Some Css.Noto_sans
      | "ubuntu" -> Some Css.Ubuntu
      | "playfair display" -> Some Css.Playfair_display
      | "merriweather" -> Some Css.Merriweather
      | "lora" -> Some Css.Lora
      | "pt sans" -> Some Css.PT_sans
      | "arial" -> Some Css.Arial
      | "helvetica" -> Some Css.Helvetica
      | "times" | "times new roman" -> Some Css.Serif (* Map Times to serif *)
      | _ -> Some Css.Sans_serif (* Default to sans-serif for unknown fonts *))
    families

(* ========================================================================
   Declaration Parser
   ======================================================================== *)

(* Parse a CSS property into a typed declaration *)
let declaration_of_string name value =
  (* Strip !important flag if present *)
  let value, is_important =
    let trimmed = String.trim value in
    if
      String.length trimmed > 10
      && String.sub trimmed (String.length trimmed - 10) 10 = "!important"
    then (String.sub trimmed 0 (String.length trimmed - 10) |> String.trim, true)
    else if
      String.length trimmed > 11
      && String.sub trimmed (String.length trimmed - 11) 11 = " !important"
    then (String.sub trimmed 0 (String.length trimmed - 11) |> String.trim, true)
    else (trimmed, false)
  in

  (* Parse the declaration *)
  let decl =
    (* Check if it's a CSS variable *)
    if String.length name > 2 && String.sub name 0 2 = "--" then
      Css.custom_property name value
    else
      (* Parse regular CSS properties into typed declarations *)
      try
        match name with
        | "color" -> Css.color (color_of_string value)
        | "background-color" -> Css.background_color (color_of_string value)
        | "padding" -> (
            (* Handle shorthand padding with multiple values *)
            (* Check if it's a single var() or calc() function *)
            let trimmed = String.trim value in
            if
              String.starts_with ~prefix:"var(" trimmed
              && String.ends_with ~suffix:")" trimmed
              || String.starts_with ~prefix:"calc(" trimmed
                 && String.ends_with ~suffix:")" trimmed
            then Css.padding (length_of_string value)
            else if
              has_affix ~affix:"var(" value || has_affix ~affix:"calc(" value
            then
              (* Multiple var() or calc() values - store as custom property *)
              Css.custom_property "--padding-shorthand" value
            else
              let values =
                String.split_on_char ' ' value
                |> List.map String.trim
                |> List.filter (( <> ) "")
              in
              match values with
              | [ v ] -> Css.padding (length_of_string v) (* Single value *)
              | _ ->
                  (* For shorthand padding with multiple values, store as custom
                     property for now *)
                  Css.custom_property "--padding-shorthand" value)
        | "padding-left" -> Css.padding_left (length_of_string value)
        | "padding-inline" -> Css.padding_inline (length_of_string value)
        | "padding-block" -> Css.padding_block (length_of_string value)
        | "padding-inline-start" ->
            Css.padding_inline_start (length_of_string value)
        | "padding-inline-end" ->
            Css.padding_inline_end (length_of_string value)
        | "padding-right" -> Css.padding_right (length_of_string value)
        | "padding-top" -> Css.padding_top (length_of_string value)
        | "padding-bottom" -> Css.padding_bottom (length_of_string value)
        | "margin" -> (
            (* Handle shorthand margin with multiple values *)
            (* Use state machine to properly parse space-separated values *)
            let values = space_separated_values_of_string value in
            match values with
            | [ v ] -> Css.margin (length_of_string v) (* Single value *)
            | _ ->
                (* For shorthand margin with multiple values, store as custom property for now *)
                (* This allows parsing to succeed - proper expansion would require parser restructure *)
                Css.custom_property "--margin-shorthand" value)
        | "margin-left" -> Css.margin_left (length_of_string value)
        | "margin-right" -> Css.margin_right (length_of_string value)
        | "margin-inline-end" -> Css.margin_inline_end (length_of_string value)
        | "margin-top" -> Css.margin_top (length_of_string value)
        | "margin-bottom" -> Css.margin_bottom (length_of_string value)
        | "width" -> Css.width (length_of_string value)
        | "height" -> Css.height (length_of_string value)
        | "display" -> Css.display (display_of_string value)
        | "position" -> Css.position (position_of_string value)
        | "box-sizing" -> Css.box_sizing (box_sizing_of_string value)
        | "font-style" -> Css.font_style (font_style_of_string value)
        | "font-size" -> Css.font_size (length_of_string value)
        | "font-weight" -> Css.font_weight (font_weight_of_string value)
        | "white-space" -> Css.white_space (white_space_of_string value)
        | "table-layout" -> Css.table_layout (table_layout_of_string value)
        | "appearance" -> Css.appearance (appearance_of_string value)
        | "border-style" -> Css.border_style (border_style_of_string value)
        | "border-top-style" ->
            Css.border_top_style (border_style_of_string value)
        | "border-right-style" ->
            Css.border_right_style (border_style_of_string value)
        | "border-bottom-style" ->
            Css.border_bottom_style (border_style_of_string value)
        | "border-left-style" ->
            Css.border_left_style (border_style_of_string value)
        | "border-width" -> Css.border_width (length_of_string value)
        | "border-top-width" -> Css.border_top_width (length_of_string value)
        | "border-right-width" ->
            Css.border_right_width (length_of_string value)
        | "border-bottom-width" ->
            Css.border_bottom_width (length_of_string value)
        | "border-left-width" -> Css.border_left_width (length_of_string value)
        | "border-color" -> Css.border_color (color_of_string value)
        | "border-top-color" -> Css.border_top_color (color_of_string value)
        | "border-right-color" -> Css.border_right_color (color_of_string value)
        | "border-bottom-color" ->
            Css.border_bottom_color (color_of_string value)
        | "border-left-color" -> Css.border_left_color (color_of_string value)
        | "border-radius" -> Css.border_radius (length_of_string value)
        | "border" -> Css.border value (* Generic border shorthand *)
        | "border-collapse" ->
            Css.border_collapse (border_collapse_of_string value)
        | "border-spacing" -> Css.border_spacing (length_of_string value)
        | "visibility" -> Css.visibility (visibility_of_string value)
        | "overflow" -> Css.overflow (overflow_of_string value)
        | "overflow-x" -> Css.overflow_x (overflow_of_string value)
        | "overflow-y" -> Css.overflow_y (overflow_of_string value)
        | "clear" -> Css.clear (clear_of_string value)
        | "float" -> Css.float (float_side_of_string value)
        | "text-align" -> Css.text_align (text_align_of_string value)
        | "z-index" -> Css.z_index (z_index_of_string value)
        | "top" -> Css.top (length_of_string value)
        | "right" -> Css.right (length_of_string value)
        | "bottom" -> Css.bottom (length_of_string value)
        | "left" -> Css.left (length_of_string value)
        | "min-width" -> Css.min_width (length_of_string value)
        | "min-height" -> Css.min_height (length_of_string value)
        | "max-width" -> Css.max_width (length_of_string value)
        | "max-height" -> Css.max_height (length_of_string value)
        | "flex-grow" -> Css.flex_grow (float_of_string value)
        | "flex-shrink" -> Css.flex_shrink (float_of_string value)
        | "line-height" -> Css.line_height (line_height_of_string value)
        | "opacity" -> Css.opacity (float_of_string value)
        | "user-select" -> Css.user_select (user_select_of_string value)
        | "pointer-events" ->
            Css.pointer_events (pointer_events_of_string value)
        | "letter-spacing" -> Css.letter_spacing (length_of_string value)
        | "font-family" -> Css.font_family (font_family_of_string value)
        | "content" -> Css.content (content_of_string value)
        | "list-style" -> Css.list_style (String.trim value)
        | "outline" -> Css.outline (String.trim value)
        | "outline-style" -> Css.outline_style (outline_style_of_string value)
        | "outline-width" -> Css.outline_width (length_of_string value)
        | "outline-color" -> Css.outline_color (color_of_string value)
        | "outline-offset" -> Css.outline_offset (length_of_string value)
        | "text-indent" -> Css.text_indent (length_of_string value)
        | "background-image" ->
            Css.background_image (background_image_of_string value)
        | "background-position" -> Css.background_position (String.trim value)
        | "background-repeat" ->
            Css.background_repeat (background_repeat_of_string value)
        | "background-size" ->
            Css.background_size (background_size_of_string value)
        | "transform" -> Css.transform (transform_of_string value)
        | "scale" -> Css.scale (scale_of_string value)
        | "vertical-align" ->
            Css.vertical_align (vertical_align_of_string value)
        | "text-decoration" ->
            Css.text_decoration (text_decoration_of_string value)
        | "text-decoration-style" ->
            Css.text_decoration_style (text_decoration_style_of_string value)
        | "text-decoration-color" ->
            Css.text_decoration_color (color_of_string value)
        | "text-decoration-thickness" ->
            Css.text_decoration_thickness (length_of_string value)
        | "text-underline-offset" ->
            Css.text_underline_offset (String.trim value)
        | "text-transform" ->
            Css.text_transform (text_transform_of_string value)
        | "text-overflow" -> Css.text_overflow (text_overflow_of_string value)
        | "text-wrap" -> Css.text_wrap (text_wrap_of_string value)
        | "word-break" -> Css.word_break (word_break_of_string value)
        | "overflow-wrap" -> Css.overflow_wrap (overflow_wrap_of_string value)
        | "hyphens" -> Css.hyphens (hyphens_of_string value)
        | "font-feature-settings" ->
            Css.font_feature_settings (parse_font_feature_settings value)
        | "font-variation-settings" ->
            Css.font_variation_settings
              (font_variation_settings_of_string value)
        | "justify-content" ->
            Css.justify_content (justify_content_of_string value)
        | "align-content" -> Css.align_content (align_of_string value)
        | "align-self" -> Css.align_self (align_self_of_string value)
        | "justify-self" -> Css.justify_self (justify_self_of_string value)
        | "box-shadow" -> Css.box_shadow (box_shadow_of_string value)
        | "resize" -> Css.resize (resize_of_string value)
        | "scroll-behavior" ->
            Css.scroll_behavior (scroll_behavior_of_string value)
        | "transition" -> Css.transition (Css.Multiple [])
        | "animation" -> Css.animation (animation_of_string value)
        | "grid-template-columns" -> Css.grid_template_columns Css.None
        | "grid-gap" | "gap" -> Css.custom_property "--grid-gap" value
        | "grid-template-rows" -> Css.grid_template_rows Css.None
        | "align-items" -> Css.align_items (align_items_of_string value)
        | "justify-items" -> Css.justify_items (justify_items_of_string value)
        | "font" -> Css.font value (* Keep shorthand as raw string *)
        | "object-fit" -> Css.object_fit (object_fit_of_string value)
        | "object-position" -> Css.object_position (String.trim value)
        | "quotes" -> Css.quotes (String.trim value)
        | "filter" -> Css.filter (filter_of_string value)
        | "aspect-ratio" -> Css.aspect_ratio (aspect_ratio_of_string value)
        | "tab-size" ->
            (* tab-size: integer (number of spaces). Keep it simple for now. *)
            let v = String.trim value in
            let n = int_of_string v in
            Css.tab_size n
        | "background" ->
            (* Background shorthand - treat simple colors as background-color *)
            if
              String.starts_with ~prefix:"#" value
              || String.starts_with ~prefix:"rgb" value
            then Css.background_color (color_of_string value)
            else if has_affix ~affix:"gradient" value then
              Css.background_image (background_image_of_string value)
            else if String.starts_with ~prefix:"url(" value then
              Css.background_image (background_image_of_string value)
            else
              (* For complex background shorthand, store as custom property *)
              Css.custom_property "--background-shorthand" value
        | _
          when String.starts_with ~prefix:"-webkit-" name
               || String.starts_with ~prefix:"-moz-" name
               || String.starts_with ~prefix:"-ms-" name
               || String.starts_with ~prefix:"-o-" name ->
            (* Vendor prefixes stored as CSS variables *)
            Css.custom_property ("--" ^ name) value
        | _ ->
            (* For truly unknown properties, fail with a message *)
            Fmt.failwith "Unknown CSS property: %s" name
      with exn ->
        (* If parsing fails, report the error with module context *)
        Fmt.failwith "css_parser.ml: failed to parse property '%s: %s' - %s"
          name value (Printexc.to_string exn)
  in

  (* Apply !important flag if present *)
  if is_important then Css.important decl else decl

(* ========================================================================
   Parser Utilities
   ======================================================================== *)

(* Skip a quoted string, handling escapes *)
let skip_string st quote_char =
  let rec loop () =
    match next st with
    | None -> ()
    | Some c when c = quote_char -> ()
    | Some '\\' ->
        ignore (next st);
        loop ()
    | Some _ -> loop ()
  in
  loop ()

(* Skip until we find a delimiter at the given depth *)
let skip_to_delimiter st =
  let rec loop depth =
    match next st with
    | None -> ()
    | Some '}' when depth = 0 -> st.i <- st.i - 1
    | Some ';' when depth = 0 -> ()
    | Some '(' | Some '[' -> loop (depth + 1)
    | (Some ')' | Some ']') when depth > 0 -> loop (depth - 1)
    | Some '"' ->
        skip_string st '"';
        loop depth
    | Some '\'' ->
        skip_string st '\'';
        loop depth
    | Some _ -> loop depth
  in
  loop 0

let read_prop_name st =
  let b = Buffer.create 64 in
  let depth = ref 0 in
  let rec loop () =
    match peek st with
    | None | Some '}' | Some ';' -> Buffer.contents b
    | Some '(' | Some '[' ->
        ignore (next st);
        Buffer.add_char b '(';
        incr depth;
        loop ()
    | Some ')' | Some ']' ->
        ignore (next st);
        Buffer.add_char b ')';
        if !depth > 0 then decr depth;
        loop ()
    | Some ':' when !depth = 0 -> Buffer.contents b
    | Some c ->
        ignore (next st);
        Buffer.add_char b c;
        loop ()
  in
  String.trim (loop ())

let read_declarations st =
  let skip_to_next () = skip_to_delimiter st in
  let rec loop acc =
    skip_ws st;
    match peek st with
    | None -> List.rev acc
    | Some '}' ->
        ignore (next st);
        List.rev acc
    | Some _ -> (
        let name = read_prop_name st in
        skip_ws st;
        match peek st with
        | Some '}' ->
            ignore (next st);
            List.rev acc
        | Some ':' ->
            ignore (next st);
            let value = read_value st in
            skip_ws st;
            (match peek st with Some ';' -> ignore (next st) | _ -> ());
            (* Ignore at-rules that slipped through as property names *)
            let acc =
              if String.length name > 0 && name.[0] = '@' then acc
              else declaration_of_string name value :: acc
            in
            loop acc
        | Some '{' when String.length name > 0 && name.[0] = '@' ->
            (* Proper nested at-rule inside a declaration block: skip its
               block *)
            ignore (next st);
            let rec skip depth =
              match next st with
              | None -> ()
              | Some '{' -> skip (depth + 1)
              | Some '}' -> if depth = 0 then () else skip (depth - 1)
              | Some '"' ->
                  skip_string st '"';
                  skip depth
              | Some '\'' ->
                  skip_string st '\'';
                  skip depth
              | Some _ -> skip depth
            in
            skip 0;
            loop acc
        | Some ';' ->
            ignore (next st);
            loop acc
        | _ ->
            (* malformed; try to recover to next delimiter *)
            skip_to_next ();
            (match peek st with Some ';' -> ignore (next st) | _ -> ());
            loop acc)
  in
  loop []

(* ======================================================================== Rule
   and Stylesheet Parsers
   ======================================================================== *)

(* Parse rules within a block (for @media, etc.) *)
let rules_of_string st =
  let rec loop acc =
    skip_ws st;
    match peek st with
    | None | Some '}' -> List.rev acc
    | _ -> (
        match read_selector st with
        | None -> List.rev acc
        | Some sel ->
            let decls = read_declarations st in
            let r = Css.rule ~selector:sel decls in
            loop (r :: acc))
  in
  loop []

(* Parse nested rules inside a block where both regular rules and @supports are
   allowed (e.g., inside @layer). Produces nested_rule list. *)
let nested_rules_of_string st : Css.nested_rule list =
  let rec loop acc =
    skip_ws st;
    match peek st with
    | None | Some '}' -> List.rev acc
    | Some '@' -> (
        ignore (next st);
        let name =
          read_until_char st ~stop:(fun c -> is_ws c || c = '{' || c = ';')
        in
        skip_ws st;
        match String.lowercase_ascii (trim name) with
        | "supports" -> (
            let condition = read_until_char st ~stop:(fun c -> c = '{') in
            match peek st with
            | Some '{' ->
                ignore (next st);
                let inner_rules = rules_of_string st in
                (match peek st with Some '}' -> ignore (next st) | _ -> ());
                let s = Css.supports ~condition:(trim condition) inner_rules in
                loop (Css.supports_to_nested s :: acc)
            | _ -> loop acc)
        | _ ->
            (* Unknown nested at-rule: skip its block or declaration *)
            let _ = read_until_char st ~stop:(fun c -> c = ';' || c = '{') in
            (match peek st with
            | Some ';' -> ignore (next st)
            | Some '{' ->
                ignore (next st);
                let rec skip depth =
                  match next st with
                  | None -> ()
                  | Some '{' -> skip (depth + 1)
                  | Some '}' -> if depth = 0 then () else skip (depth - 1)
                  | Some '"' ->
                      skip_string st '"';
                      skip depth
                  | Some '\'' ->
                      skip_string st '\'';
                      skip depth
                  | Some _ -> skip depth
                in
                skip 0
            | _ -> ());
            loop acc)
    | _ -> (
        match read_selector st with
        | None -> List.rev acc
        | Some sel ->
            let decls = read_declarations st in
            let r = Css.rule ~selector:sel decls in
            loop (Css.rule_to_nested r :: acc))
  in
  loop []

(* Skip a balanced block, handling strings *)
let skip_balanced_block st =
  let rec loop depth =
    match next st with
    | None -> ()
    | Some '{' -> loop (depth + 1)
    | Some '}' -> if depth = 0 then () else loop (depth - 1)
    | Some '"' ->
        skip_string st '"';
        loop depth
    | Some '\'' ->
        skip_string st '\'';
        loop depth
    | Some _ -> loop depth
  in
  loop 0

let read_at_rule st acc name =
  match String.lowercase_ascii (trim name) with
  | "media" -> (
      let condition = read_until_char st ~stop:(fun c -> c = '{') in
      match peek st with
      | Some '{' ->
          ignore (next st);
          let rules = rules_of_string st in
          (match peek st with Some '}' -> ignore (next st) | _ -> ());
          let media = Css.media ~condition:(trim condition) rules in
          Css.Media media :: acc
      | _ -> acc)
  | "layer" -> (
      let layer_name = read_until_char st ~stop:(fun c -> c = '{') in
      match peek st with
      | Some '{' ->
          ignore (next st);
          let nested = nested_rules_of_string st in
          (match peek st with Some '}' -> ignore (next st) | _ -> ());
          let layer = Css.layer ~name:(trim layer_name) nested in
          Css.Layer layer :: acc
      | _ -> acc)
  | "supports" -> (
      let condition = read_until_char st ~stop:(fun c -> c = '{') in
      match peek st with
      | Some '{' ->
          ignore (next st);
          let rules = rules_of_string st in
          (match peek st with Some '}' -> ignore (next st) | _ -> ());
          let s = Css.supports ~condition:(trim condition) rules in
          Css.Supports s :: acc
      | _ -> acc)
  | _ ->
      let _ = read_until_char st ~stop:(fun c -> c = ';' || c = '{') in
      (match peek st with
      | Some ';' -> ignore (next st)
      | Some '{' ->
          ignore (next st);
          skip_balanced_block st
      | _ -> ());
      acc

let read_items st =
  let rec loop acc =
    skip_ws st;
    if eof st then List.rev acc
    else
      match peek st with
      | Some '@' ->
          ignore (next st);
          let name =
            read_until_char st ~stop:(fun c -> is_ws c || c = '{' || c = ';')
          in
          skip_ws st;
          loop (read_at_rule st acc name)
      | _ -> (
          match read_selector st with
          | None ->
              skip_ws st;
              (match peek st with
              | Some '{' ->
                  ignore (next st);
                  skip_balanced_block st
              | _ -> skip_to_delimiter st);
              (match peek st with Some ';' -> ignore (next st) | _ -> ());
              loop acc
          | Some sel ->
              let decls = read_declarations st in
              let r = Css.rule ~selector:sel decls in
              loop (Css.Rule r :: acc))
  in
  loop []

(* ======================================================================== Main
   Entry Points
   ======================================================================== *)

let of_string s : (Css.t, string) result =
  try
    let st = init s in
    let items = read_items st in
    Ok (Css.stylesheet items)
  with exn -> Error (Printexc.to_string exn)

let of_string_exn s = match of_string s with Ok t -> t | Error e -> failwith e
