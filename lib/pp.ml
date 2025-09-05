type ctx = { minify : bool; indent : int; buf : Buffer.t; inline : bool }
type 'a t = ctx -> 'a -> unit

let to_string ?(minify = false) ?(inline = false) pp a =
  let buf = Buffer.create 1024 in
  let ctx = { minify; indent = 0; buf; inline } in
  pp ctx a;
  Buffer.contents buf

let to_buffer ?(minify = false) ?(inline = false) buf pp a =
  let ctx = { minify; indent = 0; buf; inline } in
  pp ctx a

let nop _ _ = ()
let str s ctx _ = Buffer.add_string ctx.buf s
let string ctx s = Buffer.add_string ctx.buf s
let char ctx c = Buffer.add_char ctx.buf c
let sp ctx () = if not ctx.minify then Buffer.add_char ctx.buf ' '
let cut ctx () = if not ctx.minify then Buffer.add_string ctx.buf "\n" else ()

let indent pp ctx a =
  (* Output indentation spaces when not minifying *)
  if not ctx.minify then
    Buffer.add_string ctx.buf (String.make (2 * (ctx.indent + 1)) ' ');
  let new_ctx = { ctx with indent = ctx.indent + 1 } in
  pp new_ctx a

let nest n pp ctx a =
  let new_ctx = { ctx with indent = ctx.indent + n } in
  pp new_ctx a

let ( ++ ) pp1 pp2 ctx a =
  pp1 ctx a;
  pp2 ctx a

let pair ?sep pp1 pp2 ctx (a, b) =
  pp1 ctx a;
  (match sep with Some s -> s ctx () | None -> ());
  pp2 ctx b

let triple ?sep pp1 pp2 pp3 ctx (a, b, c) =
  pp1 ctx a;
  (match sep with Some s -> s ctx () | None -> ());
  pp2 ctx b;
  (match sep with Some s -> s ctx () | None -> ());
  pp3 ctx c

let list ?sep pp ctx l =
  match l with
  | [] -> ()
  | [ x ] -> pp ctx x
  | h :: t ->
      pp ctx h;
      List.iter
        (fun x ->
          (match sep with Some s -> s ctx () | None -> ());
          pp ctx x)
        t

let list_with_last ?sep pp ctx l =
  let rec go = function
    | [] -> ()
    | [ x ] -> pp ~is_last:true ctx x
    | h :: t ->
        pp ~is_last:false ctx h;
        (match sep with Some s -> s ctx () | None -> ());
        go t
  in
  go l

let option ?(none = nop) pp ctx = function
  | None -> none ctx ()
  | Some x -> pp ctx x

let using f pp ctx a = pp ctx (f a)

let surround ~left ~right pp ctx a =
  left ctx ();
  pp ctx a;
  right ctx ()

(* Number formatting to be implemented according to the plan *)
let format_decimal ?(drop_leading_zero = false) s max_decimals is_neg =
  let len = String.length s in
  let s =
    if len < max_decimals then String.make (max_decimals - len) '0' ^ s else s
  in
  let len = String.length s in
  let point_pos = len - max_decimals in
  let int_part = if point_pos <= 0 then "0" else String.sub s 0 point_pos in
  let frac_part =
    if point_pos >= len then "" else String.sub s point_pos (len - point_pos)
  in

  (* Trim trailing zeros from fractional part *)
  let rec trim_zeros s =
    if String.ends_with ~suffix:"0" s then
      trim_zeros (String.sub s 0 (String.length s - 1))
    else s
  in
  let frac_part = trim_zeros frac_part in

  let final_str =
    if frac_part = "" then int_part
    else if drop_leading_zero && int_part = "0" then "." ^ frac_part
    else int_part ^ "." ^ frac_part
  in
  if is_neg then "-" ^ final_str else final_str

let float_to_string ?(drop_leading_zero = false) ?(max_decimals = 8) f =
  if f = 0.0 then "0"
  else if f <> f then "0" (* NaN *)
  else if f = infinity then "3.40282e38"
  else if f = neg_infinity then "-3.40282e38"
  else
    let is_neg = f < 0.0 in
    let abs_f = if is_neg then -.f else f in
    (* Check if this is an integer or needs decimal handling *)
    if abs_f = floor abs_f then
      (* It's an integer - convert directly *)
      if abs_f <= float_of_int max_int then
        let s = string_of_int (int_of_float abs_f) in
        if is_neg then "-" ^ s else s
      else
        (* Very large integer - use string_of_float and clean it up *)
        let s = string_of_float f in
        if String.ends_with ~suffix:".0" s then
          String.sub s 0 (String.length s - 2)
        else if String.ends_with ~suffix:"." s then
          String.sub s 0 (String.length s - 1)
        else s
    else
      (* Has decimal places - use the scaling approach *)
      let scale = 10.0 ** float_of_int max_decimals in
      let scaled = floor ((abs_f *. scale) +. 0.5) in
      let s =
        if scaled <= float_of_int max_int then
          string_of_int (int_of_float scaled)
        else string_of_float scaled
      in
      (* remove .0 or . *)
      let s =
        if String.ends_with ~suffix:".0" s then
          String.sub s 0 (String.length s - 2)
        else if String.ends_with ~suffix:"." s then
          String.sub s 0 (String.length s - 1)
        else s
      in
      format_decimal ~drop_leading_zero s max_decimals is_neg

let float ctx f =
  Buffer.add_string ctx.buf (float_to_string ~drop_leading_zero:ctx.minify f)

let float_n n ctx f =
  let s = float_to_string ~drop_leading_zero:ctx.minify ~max_decimals:n f in
  Buffer.add_string ctx.buf s

let int ctx i = Buffer.add_string ctx.buf (string_of_int i)
let colon ctx () = Buffer.add_char ctx.buf ':'

let sep ctx s =
  Buffer.add_string ctx.buf s;
  if not ctx.minify then Buffer.add_char ctx.buf ' '

let comma ctx () = sep ctx ","
let semicolon ctx () = Buffer.add_char ctx.buf ';'
let slash ctx () = Buffer.add_char ctx.buf '/'
let space ctx () = Buffer.add_char ctx.buf ' '
let block_open ctx () = Buffer.add_char ctx.buf '{'
let block_close ctx () = Buffer.add_char ctx.buf '}'

type sep = unit t

let minified ctx = ctx.minify
let cond p a b ctx x = if p ctx then a ctx x else b ctx x
let space_if_pretty = sp

let braces pp =
  let open_ ctx () =
    block_open ctx ();
    if not ctx.minify then cut ctx ()
  in
  let close_ ctx () =
    if not ctx.minify then cut ctx ();
    block_close ctx ()
  in
  surround ~left:open_ ~right:close_ (indent pp)

let call name pp_args ctx args =
  string ctx name;
  char ctx '(';
  pp_args ctx args;
  char ctx ')'

let call_list name pp_item = call name (list ~sep:comma pp_item)
