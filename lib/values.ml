(** CSS Values & Units parsing using Reader API *)

include Values_intf

let var_ref ?fallback ?default ?layer ?meta name =
  let fallback : _ fallback =
    match fallback with None -> None | Some x -> x
  in
  { name; fallback; default; layer; meta }

let syntax_fallback s = Syntax_fallback (Cursor.remaining (Cursor.of_string s))

let string_of_number_percentage (np : number_percentage) =
  match np with Num f | Pct f -> Pp.string_of_float f | _ -> "initial"

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
      Rgba
        {
          rgb = Channels { r = Int r; g = Int g; b = Int b };
          a = Num a;
          legacy = false;
        }

let hsl h s l = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = None }
let hsla h s l a = Hsl { h = Unitless h; s = Pct s; l = Pct l; a = Num a }
let hwb h w b = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = None }
let hwba h w b a = Hwb { h = Unitless h; w = Pct w; b = Pct b; a = Num a }

let oklch l c h =
  Oklch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = None }

let oklcha l c h a =
  Oklch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = Num a }

let oklab l a b =
  Oklab { l = Some (Pct l); a = Some a; b = Some b; alpha = None }

let oklaba l a b alpha =
  Oklab { l = Some (Pct l); a = Some a; b = Some b; alpha = Num alpha }

let oklaba_none_zeros l a b alpha =
  let a = if a = 0.0 then Stdlib.Option.None else Stdlib.Option.Some a in
  let b = if b = 0.0 then Stdlib.Option.None else Stdlib.Option.Some b in
  Oklab { l = Some (Pct l); a; b; alpha = Num alpha }

let lch l c h =
  Lch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = None }

let lcha l c h a =
  Lch { l = Some (Pct l); c = Some c; h = Unitless h; alpha = Num a }

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

let pp_syntax_fallback ctx value =
  Pp.string ctx
    (if Pp.minified ctx then Parser.to_string_custom_minified value
     else Parser.to_string_custom value)

let pp_var_ref ctx name =
  Pp.string ctx "var(--";
  Pp.string ctx name;
  Pp.char ctx ')'

let pp_empty_var ctx name =
  Pp.string ctx "var(--";
  Pp.string ctx name;
  Pp.char ctx ',';
  Pp.char ctx ')'

let pp_empty2_var ctx name =
  Pp.string ctx "var(--";
  Pp.string ctx name;
  Pp.string ctx ",  )"

let pp_theme_default_or ctx name none =
  match ctx.Pp.theme_defaults name with
  | Some resolved -> Pp.string ctx resolved
  | Option.None -> none ()

let pp_typed_var_fallback pp_value ctx name value =
  Pp.string ctx "var(--";
  Pp.string ctx name;
  Pp.comma ctx ();
  pp_value { ctx with in_function = true } value;
  Pp.char ctx ')'

let pp_syntax_var_fallback ctx name value =
  Pp.string ctx "var(--";
  Pp.string ctx name;
  Pp.comma ctx ();
  pp_syntax_fallback { ctx with in_function = true } value;
  Pp.char ctx ')'

let first_top_level_comma_segment s =
  let len = String.length s in
  let rec loop i depth (quote : char option) =
    if i >= len then s
    else
      match quote with
      | Some q ->
          let quote =
            if s.[i] = q && (i = 0 || s.[i - 1] <> '\\') then Option.None
            else quote
          in
          loop (i + 1) depth quote
      | Option.None -> (
          match s.[i] with
          | '"' | '\'' -> loop (i + 1) depth (Some s.[i])
          | '(' -> loop (i + 1) (depth + 1) Option.None
          | ')' when depth > 0 -> loop (i + 1) (depth - 1) Option.None
          | ',' when depth = 0 -> String.sub s 0 i
          | _ -> loop (i + 1) depth Option.None)
  in
  loop 0 0 Option.None

let pp_inline_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  match v.default with
  | Some value -> pp_value ctx value
  | Option.None -> (
      match v.fallback with
      | Fallback value -> pp_value ctx value
      | Var_fallback name ->
          pp_theme_default_or ctx name (fun () -> pp_var_ref ctx v.name)
      | Syntax_fallback value ->
          pp_syntax_fallback { ctx with in_function = true } value
      | Empty | Empty2 -> ()
      | None -> pp_var_ref ctx v.name)

let pp_var_without_fallback : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  if in_theme ctx v.name then pp_var_ref ctx v.name
  else
    pp_theme_default_or ctx v.name (fun () ->
        match v.default with
        | Some value -> pp_value ctx value
        | Option.None -> pp_var_ref ctx v.name)

let pp_stylesheet_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  match v.fallback with
  | None -> pp_var_without_fallback pp_value ctx v
  | Empty -> pp_empty_var ctx v.name
  | Empty2 -> pp_empty2_var ctx v.name
  | Fallback value ->
      if in_theme ctx v.name then
        pp_typed_var_fallback pp_value ctx v.name value
      else pp_theme_default_or ctx v.name (fun () -> pp_value ctx value)
  | Syntax_fallback value -> pp_syntax_var_fallback ctx v.name value
  | Var_fallback fallback_name ->
      Pp.string ctx "var(--";
      Pp.string ctx v.name;
      Pp.comma ctx ();
      pp_var_fallback ctx fallback_name

let pp_var : type a. a Pp.t -> a var Pp.t =
 fun pp_value ctx v ->
  if ctx.inline then pp_inline_var pp_value ctx v
  else pp_stylesheet_var pp_value ctx v

let pp_env : type a. a Pp.t -> a env Pp.t =
 fun pp_value ctx (env : a env) ->
  Pp.string ctx "env(";
  Pp.string ctx env.name;
  List.iter
    (fun i ->
      Pp.sp ctx ();
      Pp.string ctx (string_of_int i))
    env.indices;
  (match env.fallback with
  | Some fallback ->
      Pp.comma ctx ();
      pp_value ctx fallback
  | Option.None -> ());
  Pp.char ctx ')'

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

let string_of_attr_syntax : attr_syntax -> string = function
  | Length -> "<length>"
  | Length_percentage -> "<length-percentage>"
  | Color -> "<color>"
  | Number -> "<number>"
  | Percentage -> "<percentage>"

let pp_attr_syntax ctx syntax = Pp.string ctx (string_of_attr_syntax syntax)

let pp_attr_type ctx (type_ : attr_type) =
  match type_ with
  | Type syntax ->
      Pp.string ctx "type(";
      pp_attr_syntax ctx syntax;
      Pp.char ctx ')'
  | Unit unit_ -> Pp.string ctx unit_
  | Raw_string -> Pp.string ctx "raw-string"
  | Number_type -> Pp.string ctx "number"

let pp_attr_call pp_value ctx (attr : _ attr_call) =
  Pp.string ctx attr.name;
  Option.iter
    (fun type_ ->
      Pp.space ctx ();
      pp_attr_type ctx type_)
    attr.type_;
  match attr.fallback with
  | No_fallback -> ()
  | Empty_fallback -> Pp.comma ctx ()
  | Attr_fallback value ->
      Pp.comma ctx ();
      pp_value ctx value

let attr_syntax_of_string : string -> attr_syntax option = function
  | "<length>" -> Some Length
  | "<length-percentage>" -> Some Length_percentage
  | "<color>" -> Some Color
  | "<number>" -> Some Number
  | "<percentage>" -> Some Percentage
  | _ -> None

let read_attr_syntax t : attr_syntax =
  Cursor.ws t;
  let body = Cursor.consume_remaining_as_string ~trim:true t in
  match attr_syntax_of_string body with
  | Some syntax -> syntax
  | None -> Cursor.err_invalid t ("attr() type: " ^ body)

let read_attr_type t : attr_type =
  Cursor.ws t;
  let type_ =
    if Cursor.looking_at_func "type" t then
      Type (Cursor.call "type" t read_attr_syntax)
    else
      match Cursor.ident_opt t with
      | Some "raw-string" -> Raw_string
      | Some "number" -> Number_type
      | Some unit_ -> Unit unit_
      | None when Cursor.peek_delim t = Some '%' ->
          Cursor.expect '%' t;
          Unit "%"
      | None -> Cursor.err_expected t "attr() type"
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  type_

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

(* CSS Values 4 10.7 structural simplification of a typed calc AST. Folds [Expr
   (Num _, op, Num _)] subtrees and constant-identity patterns ([x + 0], [0 +
   x], [x - 0], [x * 1], [1 * x], [x / 1]) into shorter equivalents. The
   per-type "zero is the identity" cases involving typed [Val] leaves (e.g. [Val
   Zero] for [length]) are handled by per-type pre-passes that rewrite typed
   zeros to [Num 0.] before this generic fold. *)
let rec eval_calc : type a. a calc -> a calc = function
  | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Nested inner -> (
      match eval_calc inner with
      | (Val _ | Num _ | Var _) as leaf -> leaf
      | reduced -> Nested reduced)
  | Parens inner -> (
      match eval_calc inner with
      | (Val _ | Num _ | Var _) as leaf -> leaf
      | reduced -> Parens reduced)
  | Expr (l, op, r) -> (
      let l = eval_calc l in
      let r = eval_calc r in
      match (l, op, r) with
      | Num a, Add, Num b -> Num (a +. b)
      | Num a, Sub, Num b -> Num (a -. b)
      | Num a, Mul, Num b -> Num (a *. b)
      | Num a, Div, Num b when b <> 0. -> Num (a /. b)
      | x, Add, Num 0. -> x
      | Num 0., Add, x -> x
      | x, Sub, Num 0. -> x
      | x, Mul, Num 1. -> x
      | Num 1., Mul, x -> x
      | x, Div, Num 1. -> x
      | _ -> Expr (l, op, r))

let pp_calc : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  let calc = if Pp.minified ctx then eval_calc calc else calc in
  match calc with
  | Val v when Pp.minified ctx -> pp_value ctx v
  | Num n when Pp.minified ctx -> Pp.float ctx n
  | _ -> Pp.call "calc" (pp_calc_contents pp_value) ctx calc

(* Small helpers *)
let pp_unit ?(always = true) ctx f suffix =
  (* Dropping the unit on a zero ([0px] -> [0]) is a minify-only
     canonicalization per CSS Values 4 6.5; pretty mode preserves the source
     spelling. *)
  let always = always || not (Pp.minified ctx) in
  if f = 0. && not always then Pp.char ctx '0'
  else (
    (* CSSOM serialization (CSS Values 4 6.7.2) drops a leading zero on
       fractional values ([.25rem], not [0.25rem]); we follow that canonical
       form in both minified and pretty output. *)
    Pp.string ctx (Pp.string_of_float ~drop_leading_zero:true f);
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

(** Map [f] over every [Val] leaf, preserving the calc structure. Useful when
    the caller wants to simplify a typed calc by applying a per-type rewrite
    (e.g., [Length.simplify]) at every leaf without re-tokenising. *)
let rec map_calc : type a b. (a -> b) -> a calc -> b calc =
 fun f calc ->
  match calc with
  | Val v -> Val (f v)
  | Var v ->
      (* Fallbacks carry an [a]-typed value too. *)
      let fallback : b fallback =
        match v.fallback with
        | Empty -> Empty
        | Empty2 -> Empty2
        | None -> None
        | Fallback x -> Fallback (f x)
        | Syntax_fallback value -> Syntax_fallback value
        | Var_fallback s -> Var_fallback s
      in
      let default = Option.map f v.default in
      Var { name = v.name; fallback; default; layer = v.layer; meta = v.meta }
  | Num f -> Num f
  | Sibling_index -> Sibling_index
  | Sibling_count -> Sibling_count
  | Nested inner -> Nested (map_calc f inner)
  | Parens inner -> Parens (map_calc f inner)
  | Expr (l, op, r) -> Expr (map_calc f l, op, map_calc f r)

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
(* Parse a [<number><unit>] dimension, like "1px" or "-.5em". Returns
   [Some (value, unit)] for a clean numeric dimension, [None] otherwise. *)
let simple_dimension_of_string s : (float * string) option =
  let s = String.trim s in
  let len = String.length s in
  if len = 0 then Option.None
  else
    let i = ref 0 in
    if !i < len && s.[!i] = '-' then incr i;
    while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do
      incr i
    done;
    if !i < len && s.[!i] = '.' then (
      incr i;
      while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do
        incr i
      done);
    if !i = 0 || (!i = 1 && s.[0] = '-') then Option.None
    else
      let num_s = String.sub s 0 !i in
      let unit_s = String.sub s !i (len - !i) in
      try Option.Some (float_of_string num_s, unit_s)
      with Failure _ -> Option.None

(* Split [s] on top-level commas, ignoring commas inside nested parens. *)
let split_top_level_commas s =
  let parts = ref [] in
  let buf = Buffer.create 16 in
  let depth = ref 0 in
  String.iter
    (fun c ->
      match c with
      | '(' ->
          incr depth;
          Buffer.add_char buf c
      | ')' ->
          decr depth;
          Buffer.add_char buf c
      | ',' when !depth = 0 ->
          parts := Buffer.contents buf :: !parts;
          Buffer.clear buf
      | _ -> Buffer.add_char buf c)
    s;
  parts := Buffer.contents buf :: !parts;
  List.rev !parts

(* Parse one [min()] / [max()] argument: either a simple dimension or a nested
   [min()] / [max()] that itself reduces to a constant. *)
let rec min_max_arg_of_string s : (float * string) option =
  let s = String.trim s in
  match simple_dimension_of_string s with
  | Option.Some _ as r -> r
  | Option.None ->
      let len = String.length s in
      let try_call name op =
        let prefix = name ^ "(" in
        let plen = String.length prefix in
        if
          len > plen
          && String.equal (String.sub s 0 plen) prefix
          && s.[len - 1] = ')'
        then
          let inner = String.sub s plen (len - plen - 1) in
          try_reduce_min_max op inner
        else Option.None
      in
      let m = try_call "min" `Min in
      if Option.is_some m then m else try_call "max" `Max

and try_reduce_min_max op args : (float * string) option =
  let parts = split_top_level_commas args in
  let parsed = List.map min_max_arg_of_string parts in
  if List.exists Option.is_none parsed then Option.None
  else
    match List.filter_map (fun x -> x) parsed with
    | [] -> Option.None
    | (_, first_unit) :: _ as pairs
      when List.for_all (fun (_, u) -> u = first_unit) pairs ->
        let pick =
          match op with
          | `Min ->
              List.fold_left
                (fun a (b, _) -> if b < a then b else a)
                infinity pairs
          | `Max ->
              List.fold_left
                (fun a (b, _) -> if b > a then b else a)
                neg_infinity pairs
        in
        Option.Some (pick, first_unit)
    | _ -> Option.None

let format_simple_dimension f unit =
  Pp.string_of_float ~drop_leading_zero:true (Pp.round_sig 6 f) ^ unit

let try_reduce_min_max_args op args =
  let parts = split_top_level_commas args in
  let parsed = List.map simple_dimension_of_string parts in
  if List.exists Option.is_none parsed then Option.None
  else
    let groups = Hashtbl.create 4 in
    List.iteri
      (fun pos part ->
        match simple_dimension_of_string part with
        | Option.None -> ()
        | Option.Some (n, unit) -> (
            match Hashtbl.find_opt groups unit with
            | Option.None -> Hashtbl.add groups unit (pos, n)
            | Option.Some (old_pos, old_n) ->
                let better =
                  match op with `Min -> n < old_n | `Max -> n > old_n
                in
                Hashtbl.replace groups unit
                  (if better then (old_pos, n) else (old_pos, old_n))))
      parts;
    let reduced =
      Hashtbl.to_seq groups |> List.of_seq
      |> List.sort (fun (_, (a, _)) (_, (b, _)) -> compare a b)
      |> List.map (fun (unit, (_, n)) -> format_simple_dimension n unit)
    in
    match reduced with
    | [] -> Option.None
    | _ when List.length reduced = List.length parts -> Option.None
    | _ -> Option.Some (String.concat "," reduced)

let strip_top_level_calc_arg arg =
  let arg = String.trim arg in
  let prefix = "calc(" in
  let plen = String.length prefix in
  let len = String.length arg in
  if
    len > plen
    && String.equal (String.sub arg 0 plen) prefix
    && arg.[len - 1] = ')'
  then String.sub arg plen (len - plen - 1)
  else arg

let pp_math_call ctx name args =
  let args =
    if Pp.minified ctx then
      match name with
      | "min" -> Option.value ~default:args (try_reduce_min_max_args `Min args)
      | "max" -> Option.value ~default:args (try_reduce_min_max_args `Max args)
      | "clamp" ->
          split_top_level_commas args
          |> List.map strip_top_level_calc_arg
          |> String.concat ","
      | _ -> args
    else args
  in
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

(* In a length calc tree, any zero-valued length and the unitless [0] are
   spec-equivalent additive identities; rewriting typed zeros to [Num 0.] lets
   the generic [eval_calc] simplifier collapse [calc(1px + 0px)] the same way it
   collapses [calc(1px + 0)]. *)
let length_is_zero = function
  | Zero -> true
  | Px f | Cm f | Mm f | Q f | In f | Pt f | Pc f -> f = 0.
  | Em f | Rem f | Ex f | Cap f | Ic f | Ric f | Rlh f -> f = 0.
  | Ch f | Lh f -> f = 0.
  | Pct f -> f = 0.
  | Vw f | Vh f | Vmin f | Vmax f | Vi f | Vb f -> f = 0.
  | Dvh f | Dvw f | Dvmin f | Dvmax f -> f = 0.
  | Lvh f | Lvw f | Lvmin f | Lvmax f -> f = 0.
  | Svh f | Svw f | Svmin f | Svmax f -> f = 0.
  | Cqw f | Cqh f | Cqi f | Cqb f | Cqmin f | Cqmax f -> f = 0.
  | Dimension { value; _ } -> value = 0.
  | _ -> false

let rec normalize_length_calc_zeros : length calc -> length calc = function
  | Val v when length_is_zero v -> Num 0.
  | (Val _ | Num _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Nested c -> Nested (normalize_length_calc_zeros c)
  | Parens c -> Parens (normalize_length_calc_zeros c)
  | Expr (l, op, r) ->
      Expr (normalize_length_calc_zeros l, op, normalize_length_calc_zeros r)

let calc_length_unit = function
  | Zero -> Some ("px", 0.)
  | Px n -> Some ("px", n)
  | Cm n -> Some ("cm", n)
  | Mm n -> Some ("mm", n)
  | Q n -> Some ("q", n)
  | In n -> Some ("in", n)
  | Pt n -> Some ("pt", n)
  | Pc n -> Some ("pc", n)
  | Rem n -> Some ("rem", n)
  | Em n -> Some ("em", n)
  | Ex n -> Some ("ex", n)
  | Cap n -> Some ("cap", n)
  | Ic n -> Some ("ic", n)
  | Ric n -> Some ("ric", n)
  | Rlh n -> Some ("rlh", n)
  | Ch n -> Some ("ch", n)
  | Lh n -> Some ("lh", n)
  | Pct n -> Some ("%", n)
  | Vw n -> Some ("vw", n)
  | Vh n -> Some ("vh", n)
  | Vmin n -> Some ("vmin", n)
  | Vmax n -> Some ("vmax", n)
  | Vi n -> Some ("vi", n)
  | Vb n -> Some ("vb", n)
  | Dvh n -> Some ("dvh", n)
  | Dvw n -> Some ("dvw", n)
  | Dvmin n -> Some ("dvmin", n)
  | Dvmax n -> Some ("dvmax", n)
  | Lvh n -> Some ("lvh", n)
  | Lvw n -> Some ("lvw", n)
  | Lvmin n -> Some ("lvmin", n)
  | Lvmax n -> Some ("lvmax", n)
  | Svh n -> Some ("svh", n)
  | Svw n -> Some ("svw", n)
  | Svmin n -> Some ("svmin", n)
  | Svmax n -> Some ("svmax", n)
  | Cqw n -> Some ("cqw", n)
  | Cqh n -> Some ("cqh", n)
  | Cqi n -> Some ("cqi", n)
  | Cqb n -> Some ("cqb", n)
  | Cqmin n -> Some ("cqmin", n)
  | Cqmax n -> Some ("cqmax", n)
  | Dimension { value; unit; _ } -> Some (String.lowercase_ascii unit, value)
  | _ -> None

let length_from_calc_unit unit value =
  if String.equal unit "px" && value = 0. then Zero
  else Dimension { value; unit; repr = Pp.string_of_float value }

let length_of_default_string value =
  try
    let t = Cursor.of_string value in
    let value, unit = Cursor.number_with_unit t in
    Cursor.ws t;
    Cursor.expect_eof t;
    match unit with
    | Option.Some unit ->
        Option.Some (length_from_calc_unit (String.lowercase_ascii unit) value)
    | Option.None when value = 0. -> Option.Some Zero
    | Option.None -> Option.None
  with Cursor.Parse_error _ -> None

let length_of_var_resolution ctx (v : length var) =
  if in_theme ctx v.name then Option.None
  else
    match ctx.Pp.theme_defaults v.name with
    | Option.Some value -> length_of_default_string value
    | Option.None -> (
        match v.default with
        | Option.Some value -> Option.Some value
        | Option.None -> (
            match v.fallback with
            | Fallback value -> Option.Some value
            | Var_fallback name ->
                Option.bind (ctx.Pp.theme_defaults name) length_of_default_string
            | None | Empty | Empty2 | Syntax_fallback _ -> Option.None))

let rec resolve_length_calc_vars ctx : length calc -> length calc = function
  | Var v -> (
      match length_of_var_resolution ctx v with
      | Option.Some value -> Val value
      | Option.None -> Var v)
  | Nested inner -> Nested (resolve_length_calc_vars ctx inner)
  | Parens inner -> Parens (resolve_length_calc_vars ctx inner)
  | Expr (left, op, right) ->
      Expr
        ( resolve_length_calc_vars ctx left,
          op,
          resolve_length_calc_vars ctx right )
  | (Val _ | Num _ | Sibling_index | Sibling_count) as leaf -> leaf

(* CSS Values 4 10.7: same-unit add/sub of two typed lengths reduces to a single
   length. Mixed-unit cases stay as a [calc] expression because the resolved
   value depends on cascade context. *)
let length_combine op v1 v2 =
  let combine a b =
    match op with Add -> a +. b | Sub -> a -. b | Mul | Div -> nan
  in
  match (op, calc_length_unit v1, calc_length_unit v2) with
  | (Add | Sub), Some (unit1, a), Some (unit2, b)
    when String.equal unit1 unit2 ->
      Some (length_from_calc_unit unit1 (combine a b))
  | _ -> None

(* CSS Values 4 10.7: multiplying a typed length by a unitless number scales the
   length, leaving the unit unchanged. Same goes for division by a non- zero
   number. *)
let length_scale op v n =
  let scale a = match op with Mul -> a *. n | Div -> a /. n | _ -> nan in
  match (op, calc_length_unit v) with
  | (Mul | Div), Some (unit, value) ->
      Some (length_from_calc_unit unit (scale value))
  | _ -> None

let rec eval_length_calc : length calc -> length calc =
 fun calc ->
  match calc with
  | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Nested inner -> (
      match eval_length_calc inner with
      | (Val _ | Num _ | Var _) as leaf -> leaf
      | reduced -> Nested reduced)
  | Parens inner -> (
      match eval_length_calc inner with
      | (Val _ | Num _ | Var _) as leaf -> leaf
      | reduced -> Parens reduced)
  | Expr (l, op, r) -> (
      let l = eval_length_calc l in
      let r = eval_length_calc r in
      match (l, op, r) with
      | Num a, Add, Num b -> Num (a +. b)
      | Num a, Sub, Num b -> Num (a -. b)
      | Num a, Mul, Num b -> Num (a *. b)
      | Num a, Div, Num b when b <> 0. -> Num (a /. b)
      | x, Add, Num 0. -> x
      | Num 0., Add, x -> x
      | x, Sub, Num 0. -> x
      | x, Mul, Num 1. -> x
      | Num 1., Mul, x -> x
      | x, Div, Num 1. -> x
      | Val a, op, Val b -> (
          match length_combine op a b with
          | Some v -> Val v
          | None -> Expr (l, op, r))
      | Val _, Div, Num 0. -> Expr (l, op, r)
      | Val a, ((Mul | Div) as op), Num n -> (
          match length_scale op a n with
          | Some v -> Val v
          | None -> Expr (l, op, r))
      | Num n, Mul, Val a -> (
          match length_scale Mul a n with
          | Some v -> Val v
          | None -> Expr (l, op, r))
      | _ -> Expr (l, op, r))

type length_unit =
  | U_px
  | U_cm
  | U_mm
  | U_q
  | U_in
  | U_pt
  | U_pc
  | U_rem
  | U_em
  | U_ex
  | U_cap
  | U_ic
  | U_ric
  | U_rlh
  | U_ch
  | U_lh
  | U_pct
  | U_vw
  | U_vh
  | U_vmin
  | U_vmax
  | U_vi
  | U_vb
  | U_dvh
  | U_dvw
  | U_dvmin
  | U_dvmax
  | U_lvh
  | U_lvw
  | U_lvmin
  | U_lvmax
  | U_svh
  | U_svw
  | U_svmin
  | U_svmax
  | U_cqw
  | U_cqh
  | U_cqi
  | U_cqb
  | U_cqmin
  | U_cqmax

let length_unit_is_pct = function U_pct -> true | _ -> false

let length_unit_is_viewport = function
  | U_vw | U_vh | U_vmin | U_vmax | U_vi | U_vb | U_dvh | U_dvw | U_dvmin
  | U_dvmax | U_lvh | U_lvw | U_lvmin | U_lvmax | U_svh | U_svw | U_svmin
  | U_svmax ->
      true
  | _ -> false

let length_unit_is_font_relative = function
  | U_rem | U_em | U_ex | U_cap | U_ic | U_ric | U_rlh | U_ch | U_lh -> true
  | _ -> false

let length_unit_negative_rank = function
  | unit when length_unit_is_font_relative unit -> 0
  | _ -> 1

let unit_of_string = function
  | "px" -> Some U_px
  | "cm" -> Some U_cm
  | "mm" -> Some U_mm
  | "q" -> Some U_q
  | "in" -> Some U_in
  | "pt" -> Some U_pt
  | "pc" -> Some U_pc
  | "rem" -> Some U_rem
  | "em" -> Some U_em
  | "ex" -> Some U_ex
  | "cap" -> Some U_cap
  | "ic" -> Some U_ic
  | "ric" -> Some U_ric
  | "rlh" -> Some U_rlh
  | "ch" -> Some U_ch
  | "lh" -> Some U_lh
  | "%" -> Some U_pct
  | "vw" -> Some U_vw
  | "vh" -> Some U_vh
  | "vmin" -> Some U_vmin
  | "vmax" -> Some U_vmax
  | "vi" -> Some U_vi
  | "vb" -> Some U_vb
  | "dvh" -> Some U_dvh
  | "dvw" -> Some U_dvw
  | "dvmin" -> Some U_dvmin
  | "dvmax" -> Some U_dvmax
  | "lvh" -> Some U_lvh
  | "lvw" -> Some U_lvw
  | "lvmin" -> Some U_lvmin
  | "lvmax" -> Some U_lvmax
  | "svh" -> Some U_svh
  | "svw" -> Some U_svw
  | "svmin" -> Some U_svmin
  | "svmax" -> Some U_svmax
  | "cqw" -> Some U_cqw
  | "cqh" -> Some U_cqh
  | "cqi" -> Some U_cqi
  | "cqb" -> Some U_cqb
  | "cqmin" -> Some U_cqmin
  | "cqmax" -> Some U_cqmax
  | _ -> None

let unit_of_length = function
  | Zero -> Some (U_px, 0.)
  | Px n -> Some (U_px, n)
  | Cm n -> Some (U_cm, n)
  | Mm n -> Some (U_mm, n)
  | Q n -> Some (U_q, n)
  | In n -> Some (U_in, n)
  | Pt n -> Some (U_pt, n)
  | Pc n -> Some (U_pc, n)
  | Rem n -> Some (U_rem, n)
  | Em n -> Some (U_em, n)
  | Ex n -> Some (U_ex, n)
  | Cap n -> Some (U_cap, n)
  | Ic n -> Some (U_ic, n)
  | Ric n -> Some (U_ric, n)
  | Rlh n -> Some (U_rlh, n)
  | Ch n -> Some (U_ch, n)
  | Lh n -> Some (U_lh, n)
  | Pct n -> Some (U_pct, n)
  | Vw n -> Some (U_vw, n)
  | Vh n -> Some (U_vh, n)
  | Vmin n -> Some (U_vmin, n)
  | Vmax n -> Some (U_vmax, n)
  | Vi n -> Some (U_vi, n)
  | Vb n -> Some (U_vb, n)
  | Dvh n -> Some (U_dvh, n)
  | Dvw n -> Some (U_dvw, n)
  | Dvmin n -> Some (U_dvmin, n)
  | Dvmax n -> Some (U_dvmax, n)
  | Lvh n -> Some (U_lvh, n)
  | Lvw n -> Some (U_lvw, n)
  | Lvmin n -> Some (U_lvmin, n)
  | Lvmax n -> Some (U_lvmax, n)
  | Svh n -> Some (U_svh, n)
  | Svw n -> Some (U_svw, n)
  | Svmin n -> Some (U_svmin, n)
  | Svmax n -> Some (U_svmax, n)
  | Cqw n -> Some (U_cqw, n)
  | Cqh n -> Some (U_cqh, n)
  | Cqi n -> Some (U_cqi, n)
  | Cqb n -> Some (U_cqb, n)
  | Cqmin n -> Some (U_cqmin, n)
  | Cqmax n -> Some (U_cqmax, n)
  | Dimension { value; unit; _ } -> (
      match unit_of_string (String.lowercase_ascii unit) with
      | Some unit -> Some (unit, value)
      | None -> None)
  | _ -> None

let length_of_unit unit n =
  match unit with
  | U_px -> if n = 0. then Zero else Px n
  | U_cm -> Cm n
  | U_mm -> Mm n
  | U_q -> Q n
  | U_in -> In n
  | U_pt -> Pt n
  | U_pc -> Pc n
  | U_rem -> Rem n
  | U_em -> Em n
  | U_ex -> Ex n
  | U_cap -> Cap n
  | U_ic -> Ic n
  | U_ric -> Ric n
  | U_rlh -> Rlh n
  | U_ch -> Ch n
  | U_lh -> Lh n
  | U_pct -> Pct n
  | U_vw -> Vw n
  | U_vh -> Vh n
  | U_vmin -> Vmin n
  | U_vmax -> Vmax n
  | U_vi -> Vi n
  | U_vb -> Vb n
  | U_dvh -> Dvh n
  | U_dvw -> Dvw n
  | U_dvmin -> Dvmin n
  | U_dvmax -> Dvmax n
  | U_lvh -> Lvh n
  | U_lvw -> Lvw n
  | U_lvmin -> Lvmin n
  | U_lvmax -> Lvmax n
  | U_svh -> Svh n
  | U_svw -> Svw n
  | U_svmin -> Svmin n
  | U_svmax -> Svmax n
  | U_cqw -> Cqw n
  | U_cqh -> Cqh n
  | U_cqi -> Cqi n
  | U_cqb -> Cqb n
  | U_cqmin -> Cqmin n
  | U_cqmax -> Cqmax n

type linear_term = {
  unit : length_unit;
  value : float;
  first_pos : int;
  count : int;
}

let linear_term_priority ~first_pos term =
  match (term.value > 0., length_unit_is_pct term.unit) with
  | true, true -> 0
  | true, false when term.first_pos = first_pos && term.count = 1 -> 1
  | false, false when not (length_unit_is_viewport term.unit) -> 2
  | true, false when term.count = 1 -> 3
  | true, false -> 4
  | false, false -> 5
  | false, true -> 6

let compare_negative_linear_term a b =
  let c =
    compare
      (length_unit_negative_rank a.unit)
      (length_unit_negative_rank b.unit)
  in
  if c <> 0 then c else compare a.first_pos b.first_pos

let compare_linear_term ~first_pos a b =
  let a_priority = linear_term_priority ~first_pos a in
  let b_priority = linear_term_priority ~first_pos b in
  let c = compare a_priority b_priority in
  if c <> 0 then c
  else if a_priority = 2 then compare_negative_linear_term a b
  else compare a.first_pos b.first_pos

let ordered_linear_terms terms =
  let table = Hashtbl.create 8 in
  List.iteri
    (fun pos (unit, n) ->
      match Hashtbl.find_opt table unit with
      | None ->
          Hashtbl.add table unit { unit; value = n; first_pos = pos; count = 1 }
      | Some term ->
          Hashtbl.replace table unit
            { term with value = term.value +. n; count = term.count + 1 })
    terms;
  let terms =
    Hashtbl.to_seq_values table
    |> List.of_seq
    |> List.filter (fun term -> term.value <> 0.)
  in
  let first_pos =
    List.fold_left (fun acc term -> min acc term.first_pos) max_int terms
  in
  List.sort (compare_linear_term ~first_pos) terms

let linear_calc_op first_unit first_value unit n =
  if
    n < 0. && first_value > 0. && first_unit = U_px
    && length_unit_is_font_relative unit
  then (Add, n)
  else if n < 0. then (Sub, -.n)
  else (Add, n)

let linear_length_terms calc =
  let scale factor terms =
    List.map (fun (unit, n) -> (unit, factor *. n)) terms
  in
  let rec aux = function
    | Val length -> Option.map (fun term -> [ term ]) (unit_of_length length)
    | Num 0. -> Some []
    | Num _ | Var _ | Sibling_index | Sibling_count -> None
    | Nested inner | Parens inner -> aux inner
    | Expr (left, Add, right) -> (
        match (aux left, aux right) with
        | Some l, Some r -> Some (l @ r)
        | _ -> None)
    | Expr (left, Sub, right) -> (
        match (aux left, aux right) with
        | Some l, Some r -> Some (l @ scale (-1.) r)
        | _ -> None)
    | Expr (left, Mul, Num n) -> Option.map (scale n) (aux left)
    | Expr (Num n, Mul, right) -> Option.map (scale n) (aux right)
    | Expr (left, Div, Num n) when n <> 0. ->
        Option.map (scale (1. /. n)) (aux left)
    | Expr _ -> None
  in
  aux calc

let linear_length_calc calc =
  match linear_length_terms calc with
  | None -> calc
  | Some terms -> (
      let terms = ordered_linear_terms terms in
      match terms with
      | [] -> Val Zero
      | { unit; value = n; _ } :: rest ->
          let first_unit = unit in
          let first_value = n in
          List.fold_left
            (fun acc { unit; value = n; _ } ->
              let op, n = linear_calc_op first_unit first_value unit n in
              Expr (acc, op, Val (length_of_unit unit n)))
            (Val (length_of_unit unit n))
            rest)

let lp_is_zero (v : length_percentage) =
  match v with Length l -> length_is_zero l | Pct f -> f = 0. | _ -> false

let rec normalize_lp_calc_zeros :
    length_percentage calc -> length_percentage calc = function
  | Val v when lp_is_zero v -> Num 0.
  | (Val _ | Num _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Nested c -> Nested (normalize_lp_calc_zeros c)
  | Parens c -> Parens (normalize_lp_calc_zeros c)
  | Expr (l, op, r) ->
      Expr (normalize_lp_calc_zeros l, op, normalize_lp_calc_zeros r)

let lp_of_default_string value : length_percentage option =
  try
    let t = Cursor.of_string value in
    let value, unit = Cursor.number_with_unit t in
    Cursor.ws t;
    Cursor.expect_eof t;
    match unit with
    | Option.Some "%" -> Option.Some (Pct value)
    | Option.Some unit ->
        Option.Some
          (Length (length_from_calc_unit (String.lowercase_ascii unit) value))
    | Option.None when value = 0. -> Option.Some (Length Zero)
    | Option.None -> Option.None
  with Cursor.Parse_error _ -> Option.None

let lp_of_var_resolution ctx (v : length_percentage var) =
  if in_theme ctx v.name then Option.None
  else
    match ctx.Pp.theme_defaults v.name with
    | Option.Some value -> lp_of_default_string value
    | Option.None -> (
        match v.default with
        | Option.Some value -> Option.Some value
        | Option.None -> (
            match v.fallback with
            | Fallback value -> Option.Some value
            | Var_fallback name ->
                Option.bind (ctx.Pp.theme_defaults name) lp_of_default_string
            | None | Empty | Empty2 | Syntax_fallback _ -> Option.None))

let rec resolve_lp_calc_vars ctx : length_percentage calc -> length_percentage calc
    = function
  | Var v -> (
      match lp_of_var_resolution ctx v with
      | Option.Some value -> Val value
      | Option.None -> Var v)
  | Nested inner -> Nested (resolve_lp_calc_vars ctx inner)
  | Parens inner -> Parens (resolve_lp_calc_vars ctx inner)
  | Expr (left, op, right) ->
      Expr (resolve_lp_calc_vars ctx left, op, resolve_lp_calc_vars ctx right)
  | (Val _ | Num _ | Sibling_index | Sibling_count) as leaf -> leaf

let lp_combine op (v1 : length_percentage) (v2 : length_percentage) :
    length_percentage option =
  let combine a b =
    match op with Add -> a +. b | Sub -> a -. b | Mul | Div -> nan
  in
  match (op, v1, v2) with
  | (Add | Sub), Length a, Length b -> (
      match length_combine op a b with
      | Some v -> Some (Length v)
      | None -> None)
  | (Add | Sub), Pct a, Pct b -> Some (Pct (combine a b))
  | _ -> None

let lp_scale op (v : length_percentage) n : length_percentage option =
  let scale a = match op with Mul -> a *. n | Div -> a /. n | _ -> nan in
  match (op, v) with
  | (Mul | Div), Length a -> (
      match length_scale op a n with
      | Some lv -> Some (Length lv)
      | None -> None)
  | (Mul | Div), Pct a -> Some (Pct (scale a))
  | _ -> None

let rec eval_lp_calc : length_percentage calc -> length_percentage calc =
 fun calc ->
  match calc with
  | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Nested inner -> (
      match eval_lp_calc inner with
      | (Val _ | Num _ | Var _) as leaf -> leaf
      | reduced -> Nested reduced)
  | Parens inner -> (
      match eval_lp_calc inner with
      | (Val _ | Num _ | Var _) as leaf -> leaf
      | reduced -> Parens reduced)
  | Expr (l, op, r) -> (
      let l = eval_lp_calc l in
      let r = eval_lp_calc r in
      match (l, op, r) with
      | Num a, Add, Num b -> Num (a +. b)
      | Num a, Sub, Num b -> Num (a -. b)
      | Num a, Mul, Num b -> Num (a *. b)
      | Num a, Div, Num b when b <> 0. -> Num (a /. b)
      | x, Add, Num 0. -> x
      | Num 0., Add, x -> x
      | x, Sub, Num 0. -> x
      | x, Mul, Num 1. -> x
      | Num 1., Mul, x -> x
      | x, Div, Num 1. -> x
      | Val a, op, Val b -> (
          match lp_combine op a b with
          | Some v -> Val v
          | None -> Expr (l, op, r))
      | Val _, Div, Num 0. -> Expr (l, op, r)
      | Val a, ((Mul | Div) as op), Num n -> (
          match lp_scale op a n with Some v -> Val v | None -> Expr (l, op, r))
      | Num n, Mul, Val a -> (
          match lp_scale Mul a n with Some v -> Val v | None -> Expr (l, op, r))
      | _ -> Expr (l, op, r))

let unit_of_lp : length_percentage -> (length_unit * float) option = function
  | Pct n -> Some (U_pct, n)
  | Length l -> unit_of_length l
  | Env _ | Var _ | Calc _ | Invalid _ -> None

let lp_of_unit unit n : length_percentage =
  match unit with U_pct -> Pct n | unit -> Length (length_of_unit unit n)

let linear_lp_terms calc =
  let scale factor terms =
    List.map (fun (unit, n) -> (unit, factor *. n)) terms
  in
  let rec aux = function
    | Val value -> Option.map (fun term -> [ term ]) (unit_of_lp value)
    | Num 0. -> Some []
    | Num _ | Var _ | Sibling_index | Sibling_count -> None
    | Nested inner | Parens inner -> aux inner
    | Expr (left, Add, right) -> (
        match (aux left, aux right) with
        | Some l, Some r -> Some (l @ r)
        | _ -> None)
    | Expr (left, Sub, right) -> (
        match (aux left, aux right) with
        | Some l, Some r -> Some (l @ scale (-1.) r)
        | _ -> None)
    | Expr (left, Mul, Num n) -> Option.map (scale n) (aux left)
    | Expr (Num n, Mul, right) -> Option.map (scale n) (aux right)
    | Expr (left, Div, Num n) when n <> 0. ->
        Option.map (scale (1. /. n)) (aux left)
    | Expr _ -> None
  in
  aux calc

let linear_lp_calc calc =
  match linear_lp_terms calc with
  | None -> calc
  | Some terms -> (
      let terms = ordered_linear_terms terms in
      match terms with
      | [] -> Val (Length Zero)
      | { unit; value = n; _ } :: rest ->
          let first_unit = unit in
          let first_value = n in
          List.fold_left
            (fun acc { unit; value = n; _ } ->
              let op, n = linear_calc_op first_unit first_value unit n in
              Expr (acc, op, Val (lp_of_unit unit n)))
            (Val (lp_of_unit unit n))
            rest)

let rec pp_length ?(always = false) : length Pp.t =
 fun ctx v ->
  let pp_unit_fn = pp_unit ~always ctx in
  let is_math_length = function
    | Min _ | Max _ | Clamp _ -> true
    | _ -> false
  in
  let pp_calc_wrapped_length ctx length =
    Pp.string ctx "calc(";
    pp_length ~always ctx length;
    Pp.char ctx ')'
  in
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
  | Ric f -> pp_unit_fn f "ric"
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
  | Dimension { value; unit; repr } ->
      if ctx.minify then pp_unit_fn value unit
      else (
        Pp.string ctx repr;
        Pp.string ctx unit)
  | Unknown_dimension (f, unit) -> pp_unit_fn f unit
  | Size -> Pp.string ctx "size"
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Normal -> Pp.string ctx "normal"
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
  | Hairline -> Pp.string ctx "hairline"
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
  | Stretch -> Pp.string ctx "stretch"
  | Clamp s -> pp_math_call ctx "clamp" s
  | Min s when Pp.minified ctx -> (
      (* CSS Values 4 10.7: when every argument is a constant length in the same
         unit, [min()] / [max()] reduce to a single dimension. *)
      match try_reduce_min_max `Min s with
      | Some (v, u) -> pp_unit_fn v u
      | None -> pp_math_call ctx "min" s)
  | Max s when Pp.minified ctx -> (
      match try_reduce_min_max `Max s with
      | Some (v, u) -> pp_unit_fn v u
      | None -> pp_math_call ctx "max" s)
  | Min s -> pp_math_call ctx "min" s
  | Max s -> pp_math_call ctx "max" s
  | Minmax s -> pp_math_call ctx "minmax" s
  | Round (strategy, Px v, Px step) when Pp.minified ctx && step <> 0. ->
      (* CSS Values 4 10.7: [round(strategy, value, step)] reduces to a constant
         when both operands are typed lengths in the same unit and the step is
         non-zero. Default strategy is [nearest]. *)
      let r =
        match strategy with
        | "up" -> Float.ceil (v /. step) *. step
        | "down" -> Float.floor (v /. step) *. step
        | "to-zero" -> Float.trunc (v /. step) *. step
        | _ -> Float.round (v /. step) *. step
      in
      pp_unit_fn r "px"
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_length ~always ctx value;
          Pp.comma ctx ();
          pp_length ~always ctx step)
        ctx (strategy, value, step)
  | Mod (Px a, Px b) when Pp.minified ctx && b <> 0. ->
      (* CSS Values 4 10.7: [mod()] returns the remainder using floored division
         (sign of divisor). *)
      let q = Float.floor (a /. b) in
      pp_unit_fn (a -. (q *. b)) "px"
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_length ~always ctx a;
          Pp.comma ctx ();
          pp_length ~always ctx b)
        ctx (a, b)
  | Rem_fn (Px a, Px b) when Pp.minified ctx && b <> 0. ->
      (* CSS Values 4 10.7: [rem()] returns the remainder using truncated
         division (sign of dividend). *)
      pp_unit_fn (Float.rem a b) "px"
  | Rem_fn (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_length ~always ctx a;
          Pp.comma ctx ();
          pp_length ~always ctx b)
        ctx (a, b)
  | Hypot [ (Px _ as value) ] when Pp.minified ctx ->
      pp_length ~always ctx value
  | Hypot values when Pp.minified ctx -> (
      let px_values =
        List.fold_right
          (fun value acc ->
            match (value, acc) with
            | Px f, Some values -> Some (f :: values)
            | _ -> None)
          values (Some [])
      in
      match px_values with
      | Some (_ :: _ as values) ->
          let sum_sq =
            List.fold_left (fun acc f -> acc +. (f *. f)) 0. values
          in
          pp_unit_fn (Float.sqrt sum_sq) "px"
      | _ -> Pp.call_list "hypot" (pp_length ~always) ctx values)
  | Hypot values -> Pp.call_list "hypot" (pp_length ~always) ctx values
  | Abs (Px x) when Pp.minified ctx -> pp_unit_fn (Float.abs x) "px"
  | Abs v -> Pp.call "abs" (pp_length ~always) ctx v
  | Sign v ->
      (* CSS Values 4 10.7: [sign(<length>)] returns a [<number>], not a
         [<length>], so we cannot reduce to a single dimension without breaking
         the length round-trip. Keep [sign()] verbatim. *)
      Pp.call "sign" (pp_length ~always) ctx v
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
  | Attr attr -> Pp.call "attr" (pp_attr_call (pp_length ~always)) ctx attr
  | Env env -> pp_env (pp_length ~always) ctx env
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
      | Expr (Val length, Mul, Num -1.)
        when Pp.minified ctx && is_math_length length ->
          Pp.string ctx "calc(-1*";
          pp_length ~always ctx length;
          Pp.char ctx ')'
      | Expr (Num 0.5, Mul, Val length)
        when Pp.minified ctx && is_math_length length ->
          Pp.string ctx "calc(";
          pp_length ~always ctx length;
          Pp.string ctx "/2)"
      | Expr (Num 1., Mul, Val length)
        when Pp.minified ctx && is_math_length length ->
          pp_calc_wrapped_length ctx length
      | Expr (Expr (Num 2., Mul, Val length), Div, Num 2.)
        when Pp.minified ctx && is_math_length length ->
          pp_calc_wrapped_length ctx length
      | _ ->
          let cv =
            if Pp.minified ctx then
              cv |> resolve_length_calc_vars ctx |> normalize_length_calc_zeros
              |> eval_length_calc |> linear_length_calc |> eval_length_calc
            else cv
          in
          pp_calc (pp_length ~always) ctx cv)

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
  | Light_blue -> ("lightblue", "add8e6")
  | Silver -> ("silver", "c0c0c0")
  | Maroon -> ("maroon", "800000")
  | Fuchsia -> ("fuchsia", "f0f")
  | Lime -> ("lime", "0f0")
  | Olive -> ("olive", "808000")
  | Navy -> ("navy", "000080")
  | Teal -> ("teal", "008080")
  | Aqua -> ("aqua", "0ff")
  | Rebecca_purple -> ("rebeccapurple", "663399")
  | _ ->
      (* Other extended named colors aren't tabulated yet; the [Hex] arm of
         [pp_color] picks up only the colours listed here. Names that fall
         through emit verbatim. *)
      ("", "")

(* CSS Color 4 6.4: [transparent] is defined as [rgba(0, 0, 0, 0)]. The 4-digit
   shorthand [#0000] and the 8-digit form [#00000000] are spec-equivalent
   representations. *)
let hex_is_fully_transparent value =
  match String.length value with
  | 4 -> String.for_all (fun c -> c = '0') value
  | 8 -> String.for_all (fun c -> c = '0') value
  | _ -> false

let alpha_is_zero (a : alpha) =
  match a with
  | Num 0.0 | Numeric { value = 0.0; _ } | Pct 0.0 -> true
  | _ -> false

(* CSS Color 4 6.4: [transparent] is the canonical name for any fully-
   transparent colour. Any RGB triple paired with [alpha = 0] paints to the same
   pixel; the colour stack composites them identically. *)
let rgba_is_transparent _r _g _b a = alpha_is_zero a

(* The body of a [Relative_rgb] is stored as a verbatim string of the form
   ["from <origin> r g b/<alpha>"], because the channels and alpha are part of
   the [<calc>]-derived expression. To match the typed-color path's alpha
   canonicalisation under [~minify:true], rewrite a trailing ["/<n>%"] alpha
   suffix to its decimal equivalent. *)
let minify_relative_color_alpha body =
  let len = String.length body in
  if len = 0 || body.[len - 1] <> '%' then body
  else
    match String.rindex_opt body '/' with
    | None -> body
    | Some slash -> (
        let pct = String.sub body (slash + 1) (len - slash - 2) in
        match Float.of_string_opt pct with
        | Some f ->
            String.concat ""
              [
                String.sub body 0 (slash + 1);
                Pp.string_of_float ~drop_leading_zero:true (f /. 100.);
              ]
        | None -> body)

(* Hue value of an [Hsl] colour as a float in degrees, when the input is a plain
   numeric hue (number / [deg] angle). Other forms ([rad]/[turn]/
   [grad]/[var]/[calc]/[none]) return [None] so the printer keeps the authored
   representation. *)
let deg_of_hue = function
  | Unitless f -> Some f
  | Angle (Deg f) -> Some f
  | Angle (Turn f) -> Some (f *. 360.)
  | Angle (Grad f) -> Some (f *. 0.9)
  | Angle (Rad f) -> Some (f *. 180. /. Float.pi)
  | _ -> None

let float_of_percentage = function
  | (Pct f : percentage) -> Some f
  | (Num f : percentage) -> Some (f *. 100.)
  | _ -> None

let alpha_is_full = function
  | (None : alpha) -> true
  | Num 1.0 -> true
  | Numeric { value = 1.0; _ } -> true
  | Pct 100.0 -> true
  | _ -> false

let round_to_step strategy value step =
  if step = 0. then value
  else
    let q = value /. step in
    let q =
      match strategy with
      | "up" -> Float.ceil q
      | "down" -> Float.floor q
      | "to-zero" -> Float.trunc q
      | _ -> Float.round q
    in
    q *. step

let mod_value a b =
  if b = 0. then a
  else
    let q = Float.floor (a /. b) in
    a -. (q *. b)

let sign_float x = if x > 0. then 1. else if x < 0. then -1. else 0.

(* Byte value [0..255] for an alpha component, when the alpha is a static number
   or percentage. Returns [None] for symbolic forms ([Var] / [Calc]) that can't
   fold to a fixed byte. *)
let alpha_value_byte = function
  | (None : alpha) -> Some 255
  | Num f when f >= 0. && f <= 1. ->
      Some (Float.to_int (Float.round (f *. 255.)))
  | Numeric { value = f; _ } when f >= 0. && f <= 1. ->
      Some (Float.to_int (Float.round (f *. 255.)))
  | Pct f when f >= 0. && f <= 100. ->
      Some (Float.to_int (Float.round (f *. 255. /. 100.)))
  | _ -> Option.None

(* CSS Color 4 3: the hue is interpreted modulo 360 degrees. *)
let normalize_hue f =
  let m = Float.rem f 360. in
  if m < 0. then m +. 360. else m

(* CSS Color 4 4.2.4: convert HSL components to sRGB byte triples. [hue] is in
   degrees [0..360), [saturation] and [lightness] are percentages [0..100].
   Returns [(r, g, b)] each in [0..255]. *)
let hsl_to_rgb_float ~hue ~saturation ~lightness =
  let s = saturation /. 100. in
  let l = lightness /. 100. in
  let a = s *. Float.min l (1. -. l) in
  let f n =
    let k = Float.rem (n +. (hue /. 30.)) 12. in
    let k = if k < 0. then k +. 12. else k in
    let t = Float.max (Float.min (Float.min (k -. 3.) (9. -. k)) 1.) (-1.) in
    l -. (a *. t)
  in
  (f 0., f 8., f 4.)

let hsl_to_rgb_bytes ~hue ~saturation ~lightness =
  let r, g, b = hsl_to_rgb_float ~hue ~saturation ~lightness in
  let to_byte v = Float.to_int (Float.round ((v *. 255.) +. 1e-9)) in
  (to_byte r, to_byte g, to_byte b)

(* CSS Color 4 4.2.5: convert HWB components to sRGB byte triples. [hue] in
   degrees, [whiteness]/[blackness] as percentages. *)
let hwb_to_rgb_bytes ~hue ~whiteness ~blackness =
  let w = whiteness /. 100. in
  let bl = blackness /. 100. in
  if w +. bl >= 1. then
    let g = w /. (w +. bl) in
    let v = Float.to_int (Float.round (g *. 255.)) in
    (v, v, v)
  else if w = 0. && bl = 0. then
    hsl_to_rgb_bytes ~hue ~saturation:100. ~lightness:50.
  else
    let r, g, b = hsl_to_rgb_float ~hue ~saturation:100. ~lightness:50. in
    let scale c =
      let v = (c *. (1. -. w -. bl)) +. w in
      Float.to_int (Float.round (v *. 255.))
    in
    (scale r, scale g, scale b)

(* Map a fully-saturated, mid-lightness HSL hue onto its CSS named colour. Only
   the six primary/secondary hues are addressed, since they are the only ones
   whose name is shorter than the equivalent [#hex] form. *)

(* Parse a single hex digit. *)
let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> Option.None

(* Parse a [Hex.value] string ([RGB], [RGBA], [RRGGBB], [RRGGBBAA]) into (r, g,
   b, a) bytes. Returns [None] for malformed lengths. *)
let hex_to_rgba_bytes s =
  let pair a b =
    Option.bind (hex_digit a) (fun ah ->
        Option.map (fun bh -> (ah lsl 4) lor bh) (hex_digit b))
  in
  let single c = Option.map (fun h -> (h lsl 4) lor h) (hex_digit c) in
  match String.length s with
  | 3 -> (
      match (single s.[0], single s.[1], single s.[2]) with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | 4 -> (
      match (single s.[0], single s.[1], single s.[2], single s.[3]) with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | 6 -> (
      match (pair s.[0] s.[1], pair s.[2] s.[3], pair s.[4] s.[5]) with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | 8 -> (
      match
        (pair s.[0] s.[1], pair s.[2] s.[3], pair s.[4] s.[5], pair s.[6] s.[7])
      with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | _ -> Option.None

(* The numeric value of a channel when it is a plain [Int] or integer-valued
   [Num]/[Pct]. Returns [None] for [Var]/[Calc] inputs which the printer cannot
   canonicalise. *)
let channel_byte_value (c : channel) =
  match c with
  | Int i when i >= 0 && i <= 255 -> Some i
  | Num f when Float.is_integer f && f >= 0. && f <= 255. ->
      Some (Float.to_int f)
  | Pct f when f >= 0. && f <= 100. ->
      Some (Float.to_int (Float.round (f *. 255. /. 100.)))
  | _ -> None

let byte_to_hex_byte i =
  let hex_digits = "0123456789abcdef" in
  let s = Bytes.create 2 in
  Bytes.set s 0 hex_digits.[(i lsr 4) land 0xF];
  Bytes.set s 1 hex_digits.[i land 0xF];
  Bytes.to_string s

(* Convert an [(r, g, b)] triple of integer-valued channels to the [#rrggbb]
   string used as a key by [named_for_hex] / [shorten_hex]. *)
let rgb_to_hex_string r g b =
  match (channel_byte_value r, channel_byte_value g, channel_byte_value b) with
  | Some r, Some g, Some b ->
      Some (byte_to_hex_byte r ^ byte_to_hex_byte g ^ byte_to_hex_byte b)
  | _ -> None

(* Reduce any all-static colour to its sRGB byte channels and alpha. Returns
   [None] for colours that contain a [Var] / [Calc] / [Current] component or
   that can't be folded statically (e.g. an unhandled [Color { space; ... }] in
   a colour space other than sRGB). The caller can then route through the [Hex]
   arm to pick the shortest spec-equivalent spelling. *)
let static_color_to_srgb_bytes : color -> (int * int * int * int) option =
  function
  | Hex { value; _ } -> hex_to_rgba_bytes value
  | Rgb (Channels { r; g; b }) -> (
      match
        (channel_byte_value r, channel_byte_value g, channel_byte_value b)
      with
      | Some r, Some g, Some b -> Some (r, g, b, 255)
      | _ -> Option.None)
  | Rgba { rgb = Channels { r; g; b }; a; legacy = _ } -> (
      match
        ( channel_byte_value r,
          channel_byte_value g,
          channel_byte_value b,
          alpha_value_byte a )
      with
      | Some r, Some g, Some b, Some a -> Some (r, g, b, a)
      | _ -> Option.None)
  | Hsl { h; s; l; a } -> (
      match (deg_of_hue h, float_of_percentage s, float_of_percentage l) with
      | Some hue, Some saturation, Some lightness -> (
          let hue = normalize_hue hue in
          let r, g, b = hsl_to_rgb_bytes ~hue ~saturation ~lightness in
          match alpha_value_byte a with
          | Some av -> Some (r, g, b, av)
          | Option.None -> Option.None)
      | _ -> Option.None)
  | Hwb { h; w; b; a } -> (
      match (deg_of_hue h, float_of_percentage w, float_of_percentage b) with
      | Some hue, Some whiteness, Some blackness -> (
          let hue = normalize_hue hue in
          let r, g, blue = hwb_to_rgb_bytes ~hue ~whiteness ~blackness in
          match alpha_value_byte a with
          | Some av -> Some (r, g, blue, av)
          | Option.None -> Option.None)
      | _ -> Option.None)
  | Named name ->
      let _, hex = color_name_hex name in
      if hex = "" then Option.None else hex_to_rgba_bytes hex
  | Transparent -> Some (0, 0, 0, 0)
  | _ -> Option.None

(* CSS Color 5 5: combine two [color-mix] percentages into the final
   per-component weights and an [alpha] multiplier. [p1] / [p2] are in [0..100];
   missing values are normalised by the caller. *)
let mix_weights p1 p2 : (float * float * float) option =
  let sum = p1 +. p2 in
  if sum <= 0. then None
  else
    let w1 = p1 /. sum in
    let w2 = p2 /. sum in
    let alpha_mult = if sum >= 100. then 1. else sum /. 100. in
    Some (w1, w2, alpha_mult)

(* Linearly interpolate two byte channels to a byte. *)
let lerp_byte b1 b2 w1 w2 =
  Float.to_int
    (Float.round ((Float.of_int b1 *. w1) +. (Float.of_int b2 *. w2)))

(* Mix two static colours in sRGB per CSS Color 5 5; returns [None] if either
   colour can't be folded statically or the percentages reduce to zero
   weight. *)
let mix_srgb_bytes c1 c2 ~p1 ~p2 =
  match (static_color_to_srgb_bytes c1, static_color_to_srgb_bytes c2) with
  | Some (r1, g1, b1, a1), Some (r2, g2, b2, a2) -> (
      match mix_weights p1 p2 with
      | None -> Option.None
      | Some (w1, w2, alpha_mult) ->
          let r = lerp_byte r1 r2 w1 w2 in
          let g = lerp_byte g1 g2 w1 w2 in
          let b = lerp_byte b1 b2 w1 w2 in
          let alpha_pre = lerp_byte a1 a2 w1 w2 in
          let a =
            Float.to_int (Float.round (Float.of_int alpha_pre *. alpha_mult))
          in
          Some (r, g, b, a))
  | _ -> Option.None

(* Reverse of [color_name_hex] for the named-color set whose hex form is short
   enough to be a candidate. The map is keyed on the shortened hex spelling so
   [shorten_hex "#ff0000"] and [#f00] both resolve to the same name. *)
let named_for_hex value =
  match String.lowercase_ascii value with
  | "f00" -> Some "red"
  | "00f" -> Some "blue"
  | "008000" -> Some "green"
  | "fff" -> Some "white"
  | "000" -> Some "black"
  | "ff0" -> Some "yellow"
  | "0ff" -> Some "cyan"
  | "f0f" -> Some "magenta"
  | "808080" -> Some "gray"
  | "ffa500" -> Some "orange"
  | "800080" -> Some "purple"
  | "ffc0cb" -> Some "pink"
  | "c0c0c0" -> Some "silver"
  | "800000" -> Some "maroon"
  | "808000" -> Some "olive"
  | "000080" -> Some "navy"
  | "008080" -> Some "teal"
  | _ -> None

let _ = mix_srgb_bytes

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

(* CSS Color 4 §11 normalises system colour keywords to lowercase ASCII. *)
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

let rec pp_angle : angle Pp.t =
 fun ctx -> function
  | Deg 0. when Pp.minified ctx -> Pp.char ctx '0'
  | Deg f -> pp_unit ctx f "deg"
  | Rad f -> pp_unit ctx f "rad"
  | Turn f -> pp_unit ctx f "turn"
  | Grad f -> pp_unit ctx f "grad"
  | Round (strategy, Deg value, Deg step) when Pp.minified ctx && step <> 0. ->
      pp_angle ctx (Deg (round_to_step strategy value step))
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_angle ctx value;
          Pp.comma ctx ();
          pp_angle ctx step)
        ctx (strategy, value, step)
  | Rem (Deg a, Deg b) when Pp.minified ctx && b <> 0. ->
      pp_angle ctx (Deg (Float.rem a b))
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_angle ctx a;
          Pp.comma ctx ();
          pp_angle ctx b)
        ctx (a, b)
  | Mod (Deg a, Deg b) when Pp.minified ctx && b <> 0. ->
      pp_angle ctx (Deg (mod_value a b))
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_angle ctx a;
          Pp.comma ctx ();
          pp_angle ctx b)
        ctx (a, b)
  | Calc c -> pp_calc pp_angle ctx c
  | Var v -> pp_var pp_angle ctx v
  | Invalid tokens ->
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified tokens
         else Parser.to_string tokens)

let rec pp_hue : hue Pp.t =
 fun ctx -> function
  | Unitless f -> Pp.float ctx f
  | Angle (Deg f) when ctx.minify ->
      (* During minification, omit 'deg' since it's the default unit *)
      Pp.float ctx f
  | Angle a when ctx.minify -> (
      match deg_of_hue (Angle a) with
      | Some f -> Pp.float ctx (normalize_hue f)
      | None -> pp_angle ctx a)
  | Angle a -> pp_angle ctx a
  | Var v -> pp_var pp_hue ctx v
  | Hue_none -> Pp.string ctx "none"

and pp_alpha : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> Pp.float ctx f
  | Numeric { value; repr } ->
      if Pp.minified ctx then Pp.float ctx value else Pp.string ctx repr
  | Pct f when Pp.minified ctx ->
      (* CSS Color 4 1.3: an alpha [<percentage>] is spec-equivalent to the
         [<number>] form divided by 100. Under minification, emit the shorter
         number form so [50%] and [0.5] round-trip identically. *)
      Pp.float ctx (f /. 100.)
  | Pct f ->
      Pp.float ctx f;
      Pp.char ctx '%'
  | Var v -> pp_var pp_alpha ctx v
  | Calc c -> pp_calc pp_alpha ctx c

(* Helper to print optional alpha with the correct leading separator *)
let pp_opt_alpha ctx = function
  | None -> ()
  | a when Pp.minified ctx && alpha_is_full a -> ()
  | (Num _ | Numeric _ | Pct _ | Var _ | Calc _) as a ->
      Pp.op_char ctx '/';
      pp_alpha ctx a

(** Pretty printer for percentage types *)
let rec pp_percentage ?(always = false) : percentage Pp.t =
 fun ctx -> function
  | Pct f -> Pp.pct ~always ctx f
  | Num f -> Pp.float ctx f
  | Var v -> pp_var (pp_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_percentage ~always) ctx c

and pp_length_percentage ?(always = false) : length_percentage Pp.t =
 fun ctx -> function
  | Length l -> pp_length ~always ctx l
  | Pct f -> Pp.pct ~always ctx f
  | Env env -> pp_env (pp_length_percentage ~always) ctx env
  | Var v -> pp_var (pp_length_percentage ~always) ctx v
  | Calc c ->
      let c =
        if Pp.minified ctx then
          c |> resolve_lp_calc_vars ctx |> normalize_lp_calc_zeros |> eval_lp_calc
          |> linear_lp_calc |> eval_lp_calc
        else c
      in
      pp_calc (pp_length_percentage ~always) ctx c
  | Invalid tokens ->
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified tokens
         else Parser.to_string tokens)

and pp_number_percentage ?(always = false) : number_percentage Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f -> Pp.pct ~always ctx f
  | Var v -> pp_var (pp_number_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_number_percentage ~always) ctx c

and pp_component : component Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f when Pp.minified ctx -> Pp.float ctx (f /. 100.)
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

let pp_color_components : component list Pp.t =
 fun ctx -> function
  | [] -> ()
  | first :: rest ->
      pp_component ctx first;
      let rec loop prev = function
        | [] -> ()
        | component :: rest ->
            if
              not
                (Pp.minified ctx
                && static_component_can_touch_negative prev
                && static_component_starts_negative component)
            then Pp.space ctx ();
            pp_component ctx component;
            loop component rest
      in
      loop first rest

(* Helpers to pretty print CSS color functions using Pp.call *)
let pp_rgb_args : (channel * channel * channel * alpha) Pp.t =
 fun ctx (r, g, b, alpha) ->
  Pp.list ~sep:Pp.space pp_channel ctx [ r; g; b ];
  pp_opt_alpha ctx alpha

let pp_rgb_func = Pp.call "rgb" pp_rgb_args

let pp_rgba_args : (channel * channel * channel * alpha) Pp.t =
 fun ctx (r, g, b, alpha) ->
  pp_channel ctx r;
  Pp.comma ctx ();
  pp_channel ctx g;
  Pp.comma ctx ();
  pp_channel ctx b;
  Pp.comma ctx ();
  pp_alpha ctx alpha

let pp_rgba_func = Pp.call "rgba" pp_rgba_args

let rec pp_rgb : rgb Pp.t =
 fun ctx -> function
  | Channels { r; g; b } -> Pp.list ~sep:Pp.space pp_channel ctx [ r; g; b ]
  | Var v -> pp_var pp_rgb ctx v

let space_after_color_percentage ctx (l : percentage option) ~next =
  let can_follow_percentage_without_space = function
    | '0' .. '9' | '.' | '+' | '-' -> true
    | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
    | _ -> false
  in
  match (Pp.minified ctx, l, next) with
  | true, Some (Pct _), Some s
    when String.length s > 0 && can_follow_percentage_without_space s.[0] ->
      ()
  | _ -> Pp.space ctx ()

(** Lab-like float string with precision control. Non-minified output keeps the
    regular leading zero; minified output uses the shorter leading-zero-free
    spelling. *)
let string_of_lab_float ~max_decimals ctx f =
  Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) ~max_decimals
    (if ctx.Pp.minify then Pp.round_sig 6 f else f)

let pp_lab_float ~max_decimals ctx f =
  Buffer.add_string ctx.Pp.buf (string_of_lab_float ~max_decimals ctx f)

let string_of_scaled_color_axis ~max_decimals ~pct_scale ctx f =
  let n = string_of_lab_float ~max_decimals ctx f in
  if ctx.Pp.minify then
    let pct = string_of_lab_float ~max_decimals ctx (f /. pct_scale) ^ "%" in
    if String.length pct < String.length n then pct else n
  else n

let ends_with_pct s =
  let len = String.length s in
  len > 0 && s.[len - 1] = '%'

let starts_unsigned_number s =
  String.length s > 0
  && match s.[0] with '0' .. '9' | '.' -> true | _ -> false

let pp_pct_chroma_hue_alpha ~chroma_pct_scale :
    (percentage option * float option * hue * alpha) Pp.t =
 fun ctx (l, c, h, alpha) ->
  (match l with Some l -> pp_percentage ctx l | None -> Pp.string ctx "none");
  let c_ends_with_pct =
    match c with
    | Some c ->
        let c =
          string_of_scaled_color_axis ~max_decimals:8
            ~pct_scale:chroma_pct_scale ctx c
        in
        space_after_color_percentage ctx l ~next:(Some c);
        Pp.string ctx c;
        ends_with_pct c
    | None ->
        space_after_color_percentage ctx l ~next:(Some "none");
        Pp.string ctx "none";
        false
  in
  if not (ctx.Pp.minify && c_ends_with_pct) then Pp.space ctx ();
  pp_hue ctx h;
  pp_opt_alpha ctx alpha

let pp_oklch = Pp.call "oklch" (pp_pct_chroma_hue_alpha ~chroma_pct_scale:0.004)

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

(** Print a float always dropping leading zeros (for lab-like color axes) *)
let pp_float_drop_zero ctx f =
  Buffer.add_string ctx.Pp.buf (Pp.string_of_float ~drop_leading_zero:true f)

let pp_alpha_drop_zero : alpha Pp.t =
 fun ctx -> function
  | None -> ()
  | Num f -> pp_float_drop_zero ctx f
  | Numeric { value; _ } -> pp_float_drop_zero ctx value
  | Pct f -> pp_float_drop_zero ctx (f /. 100.)
  | Var v -> pp_var pp_alpha ctx v
  | Calc c -> pp_calc pp_alpha ctx c

let string_of_lab_axis ~pct_scale ctx f =
  string_of_scaled_color_axis ~max_decimals:3 ~pct_scale ctx f

let pp_lab_like_args ~axis_pct_scale :
    (percentage option * float option * float option * alpha) Pp.t =
 fun ctx (l, a, b, alpha) ->
  (* Oklab L: percentage with controlled precision *)
  (match l with
  | Some (Pct f) ->
      pp_lab_float ~max_decimals:4 ctx f;
      Pp.char ctx '%'
  | Some l -> pp_percentage ctx l
  | None -> Pp.string ctx "none");
  let string_of_axis = function
    | Some f -> string_of_lab_axis ~pct_scale:axis_pct_scale ctx f
    | None -> "none"
  in
  let a = string_of_axis a in
  let b = string_of_axis b in
  space_after_color_percentage ctx l ~next:(Some a);
  Pp.string ctx a;
  if
    not
      (ctx.Pp.minify && starts_unsigned_number a
      && String.length b > 0
      && (b.[0] = '-' || b.[0] = '+'))
  then Pp.space ctx ();
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
          pp_color_components ctx components);
      pp_opt_alpha ctx alpha)
    ctx (space, components, alpha)

and color_of_default_string value =
  let value = String.trim value in
  let len = String.length value in
  let is_hex c =
    ('0' <= c && c <= '9')
    || ('a' <= c && c <= 'f')
    || ('A' <= c && c <= 'F')
  in
  if len > 1 && value.[0] = '#' then
    let hex = String.sub value 1 (len - 1) in
    let hex_len = String.length hex in
    if
      (hex_len = 3 || hex_len = 4 || hex_len = 6 || hex_len = 8)
      && String.for_all is_hex hex
    then Option.Some (Hex { hash = true; value = hex })
    else Option.None
  else if len > 5 && String.starts_with ~prefix:"rgb(" value && value.[len - 1] = ')'
  then
    let body = String.sub value 4 (len - 5) in
    let body = String.map (function ',' -> ' ' | c -> c) body in
    let parts =
      body |> String.split_on_char ' ' |> List.filter (fun s -> s <> "")
    in
    match List.map int_of_string_opt parts with
    | [ Option.Some r; Option.Some g; Option.Some b ]
      when List.for_all (fun n -> 0 <= n && n <= 255) [ r; g; b ] ->
        Option.Some
          (Rgb (Channels { r = Int r; g = Int g; b = Int b }))
    | _ -> Option.None
  else Option.None

and color_of_var_resolution ctx (v : color var) =
  if in_theme ctx v.name then Option.None
  else
    match ctx.Pp.theme_defaults v.name with
    | Option.Some value -> color_of_default_string value
    | Option.None -> (
        match v.default with
        | Option.Some value -> Option.Some value
        | Option.None -> (
            match v.fallback with
            | Fallback value -> Option.Some value
            | Var_fallback name ->
                Option.bind (ctx.Pp.theme_defaults name) color_of_default_string
            | None | Empty | Empty2 | Syntax_fallback _ -> Option.None))

and pp_color : color Pp.t =
 fun ctx -> function
  | Hex { hash = _; value } ->
      (* CSS Color 4 12.1: 6-digit and 3-digit hex are spec-equivalent;
         shortening is part of minification (cssnano / Lightning CSS / clean-css
         conventions). When a CSS named colour represents the same sRGB value
         with a strictly shorter spelling than the [#hex] form, emit the name;
         on a tie the hex spelling wins (Lightning CSS / clean-css convention).
         CSS Color 4 6.4 makes [#0000] / [#00000000] spec-equivalent to the
         [transparent] keyword; [#0000] is the shortest of those, so that wins
         under minify. *)
      if Pp.minified ctx then (
        if hex_is_fully_transparent value then (
          Pp.char ctx '#';
          Pp.string ctx "0000")
        else
          let shortened = shorten_hex value in
          match named_for_hex shortened with
          | Some name when String.length name < String.length shortened + 1 ->
              Pp.string ctx name
          | _ ->
              Pp.char ctx '#';
              Pp.string ctx shortened)
      else (
        Pp.char ctx '#';
        Pp.string ctx value)
  | Rgb rgb -> (
      match rgb with
      | Channels { r; g; b } when Pp.minified ctx -> (
          (* CSS Color 4 1.4: [rgb(R G B)] (no alpha) is spec-equivalent to the
             [#hex] / named-colour forms. Re-emit through [pp_color] so the same
             shortening / named lookup applies. *)
          match rgb_to_hex_string r g b with
          | Some hex -> pp_color ctx (Hex { hash = true; value = hex })
          | None -> pp_rgb_func ctx (r, g, b, None))
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
  | Rgba { rgb; a; legacy } -> (
      match rgb with
      | Channels { r; g; b } when Pp.minified ctx && rgba_is_transparent r g b a
        ->
          Pp.char ctx '#';
          Pp.string ctx "0000"
      | Channels { r; g; b } when Pp.minified ctx && alpha_is_full a -> (
          match rgb_to_hex_string r g b with
          | Some hex -> pp_color ctx (Hex { hash = true; value = hex })
          | None -> pp_rgb_func ctx (r, g, b, None))
      | Channels { r; g; b } when Pp.minified ctx -> (
          match (rgb_to_hex_string r g b, alpha_value_byte a) with
          | Some hex, Some ab ->
              pp_color ctx
                (Hex { hash = true; value = hex ^ byte_to_hex_byte ab })
          | _ -> pp_rgb_func ctx (r, g, b, a))
      | Channels { r; g; b } when legacy -> pp_rgba_func ctx (r, g, b, a)
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
  | Hsl { h; s; l; a } when Pp.minified ctx -> (
      (* CSS Color 4 4.2.4: an [hsl()] with all-static components converts to
         sRGB byte channels. Route through the [Hex] arm so the printer picks
         the shortest spec-equivalent spelling - either [#rrggbb] (or its
         3-digit shorthand), [#rrggbbaa] (alpha included), or a named colour
         when one matches. Symbolic components fall through to [pp_hsl]. *)
      match (deg_of_hue h, float_of_percentage s, float_of_percentage l) with
      | Some hue, Some saturation, Some lightness -> (
          let hue = normalize_hue hue in
          let r, g, b = hsl_to_rgb_bytes ~hue ~saturation ~lightness in
          let hex =
            byte_to_hex_byte r ^ byte_to_hex_byte g ^ byte_to_hex_byte b
          in
          match alpha_value_byte a with
          | Some 255 -> pp_color ctx (Hex { hash = true; value = hex })
          | Some ab ->
              pp_color ctx
                (Hex { hash = true; value = hex ^ byte_to_hex_byte ab })
          | Option.None -> pp_hsl ctx (h, s, l, a))
      | _ -> pp_hsl ctx (h, s, l, a))
  | Hsl { h; s; l; a } -> pp_hsl ctx (h, s, l, a)
  | Hwb { h; w; b; a } when Pp.minified ctx -> (
      (* CSS Color 4 4.2.5: like [hsl()], an [hwb()] with all-static components
         folds to its sRGB byte channels and routes through [Hex]. *)
      match (deg_of_hue h, float_of_percentage w, float_of_percentage b) with
      | Some hue, Some whiteness, Some blackness -> (
          let hue = normalize_hue hue in
          let r, g, blue = hwb_to_rgb_bytes ~hue ~whiteness ~blackness in
          let hex =
            byte_to_hex_byte r ^ byte_to_hex_byte g ^ byte_to_hex_byte blue
          in
          match alpha_value_byte a with
          | Some 255 -> pp_color ctx (Hex { hash = true; value = hex })
          | Some ab ->
              pp_color ctx
                (Hex { hash = true; value = hex ^ byte_to_hex_byte ab })
          | Option.None -> pp_hwb ctx (h, w, b, a))
      | _ -> pp_hwb ctx (h, w, b, a))
  | Hwb { h; w; b; a } -> pp_hwb ctx (h, w, b, a)
  | Color { space; components; alpha } -> pp_color' ctx space components alpha
  | Relative_rgb body ->
      let body =
        if Pp.minified ctx then minify_relative_color_alpha body else body
      in
      Pp.call "rgb" (fun ctx body -> Pp.string ctx body) ctx body
  | Relative_color (name, body) ->
      let body =
        if Pp.minified ctx then minify_relative_color_alpha body else body
      in
      Pp.call name (fun ctx body -> Pp.string ctx body) ctx body
  | Contrast_color color -> Pp.call "contrast-color" pp_color ctx color
  | Light_dark (light, dark) ->
      Pp.call "light-dark"
        (fun ctx (light, dark) ->
          pp_color ctx light;
          Pp.comma ctx ();
          pp_color ctx dark)
        ctx (light, dark)
  | Attribute (name, fallback) ->
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
  | Lab { l; a; b; alpha } -> pp_lab ctx (l, a, b, alpha)
  | Oklch { l; c; h; alpha } -> pp_oklch ctx (l, c, h, alpha)
  | Oklab { l; a; b; alpha } -> pp_oklab ctx (l, a, b, alpha)
  | Lch { l; c; h; alpha } -> pp_lch ctx (l, c, h, alpha)
  | Named name when Pp.minified ctx ->
      (* Route a directly-spelled named colour through the [Hex] arm so the
         shortest-on-tie rule applies uniformly: a static named colour minifies
         to its shortest spec-equivalent spelling regardless of where it sits
         (top-level declaration, [color-mix] argument, [light-dark] fork).
         Unresolved [Var] residuals and other dynamic values stay structural via
         their own arms. [color_name_hex] returns the empty string for extended
         names whose hex form is never shorter ([rebeccapurple], [dodgerblue],
         etc.); in that case keep the source name spelling. *)
      let _, hex = color_name_hex name in
      if hex = "" then pp_color_name ctx name
      else pp_color ctx (Hex { hash = true; value = hex })
  | Named name -> pp_color_name ctx name
  | System sc -> pp_system_color ctx sc
  | Var { fallback = Syntax_fallback value; default = Option.None; _ }
    when ctx.inline ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_custom_minified value
        else Parser.to_string_custom value
      in
      Pp.string ctx (first_top_level_comma_segment rendered)
  | Var v when Pp.minified ctx -> (
      match color_of_var_resolution ctx v with
      | Option.Some color -> pp_color ctx color
      | Option.None -> pp_var pp_color ctx v)
  | Var v -> pp_var pp_color ctx v
  | Current ->
      Pp.string ctx (if ctx.in_function then "currentcolor" else "currentColor")
  | Transparent ->
      (* CSS Color 4 6.4: [transparent] is spec-equivalent to [#0000]; under
         minify pick the shorter hex form (Lightning CSS convention). *)
      if Pp.minified ctx then (
        Pp.char ctx '#';
        Pp.string ctx "0000")
      else Pp.string ctx "transparent"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Mix
      {
        in_space = Some Srgb;
        hue = Default;
        color1;
        percent1;
        color2;
        percent2;
      }
    when Pp.minified ctx -> (
      (* CSS Color 5 5: a [color-mix(in srgb, c1 P1%, c2 P2%)] with all-static
         components reduces to a concrete sRGB byte triple. Route through the
         [Hex] arm so the printer picks the shortest spec-equivalent spelling
         ([color-mix(in srgb,red,blue) -> purple], where the [#800080] hex
         resolves to the named [purple]). Symbolic percentages or inputs that
         can't fold fall through to the structural [pp_color_mix] form. *)
      let p1 =
        match percent1 with None -> Some 50. | Some p -> float_of_percentage p
      in
      let p2 =
        match (percent1, percent2) with
        | _, Some p -> float_of_percentage p
        | Some _, None ->
            Option.bind (Option.bind percent1 float_of_percentage) (fun p1 ->
                Some (Float.max 0. (100. -. p1)))
        | None, None -> Some 50.
      in
      match (p1, p2) with
      | Some p1, Some p2 -> (
          match mix_srgb_bytes color1 color2 ~p1 ~p2 with
          | Some (r, g, b, 255) ->
              pp_color ctx
                (Hex
                   {
                     hash = true;
                     value =
                       byte_to_hex_byte r ^ byte_to_hex_byte g
                       ^ byte_to_hex_byte b;
                   })
          | Some (r, g, b, a) ->
              pp_color ctx
                (Hex
                   {
                     hash = true;
                     value =
                       byte_to_hex_byte r ^ byte_to_hex_byte g
                       ^ byte_to_hex_byte b ^ byte_to_hex_byte a;
                   })
          | Option.None ->
              pp_color_mix ctx (Some Srgb) Default color1 percent1 color2
                percent2)
      | _ ->
          pp_color_mix ctx (Some Srgb) Default color1 percent1 color2 percent2)
  | Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      pp_color_mix ctx in_space hue color1 percent1 color2 percent2

(* CSS Values 4 §6.3: [ms] and [s] are interchangeable; pick the shorter
   spelling when minifying. The "s" suffix is one character shorter than "ms",
   so a millisecond value collapses to seconds when its second-form digits are
   no longer than the millisecond-form digits. *)
let pp_duration_unit ?(shorten_ms = true) ctx f suffix =
  if f = 0. then
    (* CSS Values 4 6.6: a zero [<time>] still requires the unit (unlike
       [<length>] where [0] is valid). Pretty mode keeps the source unit; under
       minify pick [s] as the canonical short spelling. *)
    if ctx.Pp.minify then Pp.string ctx "0s"
    else (
      Pp.string ctx "0";
      Pp.string ctx suffix)
  else if (not shorten_ms) || (not ctx.Pp.minify) || suffix <> "ms" then
    pp_unit ctx f suffix
  else
    let in_seconds = f /. 1000. in
    let ms_str = Pp.string_of_float ~drop_leading_zero:true f in
    let s_str = Pp.string_of_float ~drop_leading_zero:true in_seconds in
    if String.length s_str + 1 <= String.length ms_str + 2 then (
      Pp.string ctx s_str;
      Pp.string ctx "s")
    else (
      Pp.string ctx ms_str;
      Pp.string ctx "ms")

let rec pp_duration : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Durations durations -> Pp.list ~sep:Pp.comma pp_duration ctx durations
  | Round (strategy, Ms value, Ms step) when Pp.minified ctx && step <> 0. ->
      pp_duration_unit ~shorten_ms:false ctx
        (round_to_step strategy value step)
        "ms"
  | Round (strategy, S value, S step) when Pp.minified ctx && step <> 0. ->
      pp_duration ctx (S (round_to_step strategy value step))
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_duration ctx value;
          Pp.comma ctx ();
          pp_duration ctx step)
        ctx (strategy, value, step)
  | Rem (Ms a, Ms b) when Pp.minified ctx && b <> 0. ->
      pp_duration ctx (Ms (Float.rem a b))
  | Rem (S a, S b) when Pp.minified ctx && b <> 0. ->
      pp_duration ctx (S (Float.rem a b))
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_duration ctx a;
          Pp.comma ctx ();
          pp_duration ctx b)
        ctx (a, b)
  | Mod (Ms a, Ms b) when Pp.minified ctx && b <> 0. ->
      pp_duration ctx (Ms (mod_value a b))
  | Mod (S a, S b) when Pp.minified ctx && b <> 0. ->
      pp_duration ctx (S (mod_value a b))
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_duration ctx a;
          Pp.comma ctx ();
          pp_duration ctx b)
        ctx (a, b)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_duration ctx v
  | Calc c -> pp_calc pp_duration_in_calc ctx c

and pp_duration_preserve_ms : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ~shorten_ms:false ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Durations durations ->
      Pp.list ~sep:Pp.comma pp_duration_preserve_ms ctx durations
  | Round (strategy, Ms value, Ms step) when Pp.minified ctx && step <> 0. ->
      pp_duration_preserve_ms ctx (Ms (round_to_step strategy value step))
  | Round (strategy, S value, S step) when Pp.minified ctx && step <> 0. ->
      pp_duration_preserve_ms ctx (S (round_to_step strategy value step))
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_duration_preserve_ms ctx value;
          Pp.comma ctx ();
          pp_duration_preserve_ms ctx step)
        ctx (strategy, value, step)
  | Rem (Ms a, Ms b) when Pp.minified ctx && b <> 0. ->
      pp_duration_preserve_ms ctx (Ms (Float.rem a b))
  | Rem (S a, S b) when Pp.minified ctx && b <> 0. ->
      pp_duration_preserve_ms ctx (S (Float.rem a b))
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_duration_preserve_ms ctx a;
          Pp.comma ctx ();
          pp_duration_preserve_ms ctx b)
        ctx (a, b)
  | Mod (Ms a, Ms b) when Pp.minified ctx && b <> 0. ->
      pp_duration_preserve_ms ctx (Ms (mod_value a b))
  | Mod (S a, S b) when Pp.minified ctx && b <> 0. ->
      pp_duration_preserve_ms ctx (S (mod_value a b))
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_duration_preserve_ms ctx a;
          Pp.comma ctx ();
          pp_duration_preserve_ms ctx b)
        ctx (a, b)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_duration_preserve_ms ctx v
  | Calc c -> pp_calc pp_duration_in_calc ctx c

and pp_duration_in_calc : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ~shorten_ms:false ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Durations durations ->
      Pp.list ~sep:Pp.comma pp_duration_in_calc ctx durations
  | Round (strategy, value, step) ->
      pp_duration_preserve_ms ctx (Round (strategy, value, step))
  | Rem (a, b) -> pp_duration_preserve_ms ctx (Rem (a, b))
  | Mod (a, b) -> pp_duration_preserve_ms ctx (Mod (a, b))
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_duration ctx v
  | Calc c -> pp_calc pp_duration_in_calc ctx c

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Num f ->
      Pp.string ctx
        (Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f)
  | Var v -> pp_var pp_number ctx v
  | Calc c -> pp_calc pp_number ctx c
  | Round (strategy, Num value, Num step) when Pp.minified ctx && step <> 0. ->
      pp_number ctx (Num (round_to_step strategy value step))
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_number ctx value;
          Pp.comma ctx ();
          pp_number ctx step)
        ctx (strategy, value, step)
  | Mod (Num a, Num b) when Pp.minified ctx && b <> 0. ->
      pp_number ctx (Num (mod_value a b))
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
  | Rem (Num a, Num b) when Pp.minified ctx && b <> 0. ->
      pp_number ctx (Num (Float.rem a b))
  | Rem (a, b) ->
      Pp.call "rem"
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
  let rem f = Val (Rem f : length)
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
  (* CSS Custom Properties 1: a [var()] reference must name a [<dashed-ident>]
     (a token starting with [--]). Reject non-dashed names. *)
  if not (String.length name >= 2 && name.[0] = '-' && name.[1] = '-') then
    Cursor.err_invalid t ("var() name must start with '--': " ^ name);
  let var_name = String.sub name 2 (String.length name - 2) in
  Cursor.ws t;
  let fallback : _ fallback =
    if not (Cursor.comma_opt t) then None
    else (
      Cursor.ws t;
      if Cursor.is_done t then Empty
      else
        match Cursor.try_parse_full_err read_value t with
        | Ok fb -> Fallback fb
        | Error _ -> Syntax_fallback (Cursor.remaining t))
  in
  var_ref ~fallback var_name

(** Generic [var(...)] parser. Consumes the [var(...)] [Func] and applies
    [read_value] to the fallback, if any. *)
let read_var : type a. (Cursor.t -> a) -> Cursor.t -> a var =
 fun read_value t ->
  Cursor.call "var" t (fun inner -> read_var_body read_value inner)

let read_body : type a. (Cursor.t -> a) -> Cursor.t -> a env =
 fun read_value t ->
  Cursor.ws t;
  let name = Cursor.ident ~keep_case:true t in
  let rec indices acc =
    Cursor.ws t;
    match Cursor.integer_opt t with
    | Some i -> indices (i :: acc)
    | Option.None -> List.rev acc
  in
  let indices = indices [] in
  Cursor.ws t;
  let fallback =
    if Cursor.comma_opt t then (
      Cursor.ws t;
      if Cursor.is_done t then Option.None else Some (read_value t))
    else Option.None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  ({ name; indices; fallback } : a env)

let read_env : type a. (Cursor.t -> a) -> Cursor.t -> a env =
 fun read_value t ->
  Cursor.call "env" t (fun inner -> read_body read_value inner)

let read_length_unit ?(allow_negative = true) t =
  let n, repr, unit_raw = Cursor.number_repr_with_unit t in
  if (not allow_negative) && n < 0.0 then Cursor.err_invalid t "negative";
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  let authored unit =
    match unit_raw with
    | Some raw_unit when n = 0.0 -> Dimension { value = n; unit = raw_unit; repr }
    | _ when Pp.string_of_float n = repr -> length_of_unit unit n
    | _ -> Dimension { value = n; unit = Option.value unit_raw ~default:""; repr }
  in
  match unit with
  | "" when n = 0.0 -> Zero
  | "" -> Cursor.err t "length values must have units (except for zero)"
  | unit -> (
      match unit_of_string unit with
      | Some unit -> authored unit
      | None -> Cursor.err_invalid t ("length unit: " ^ unit))

let read_length_keyword t : length =
  Cursor.enum "length"
    [
      ("auto", (Auto : length));
      ("none", None);
      ("normal", Normal);
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
  (* CSS Values 4 10.7: [+] and [-] are left-associative, so [a - b - c] groups
     as [(a - b) - c]. Loop on subsequent operators rather than recursing on the
     right, which would group it as [a - (b - c)]. *)
  let rec loop left =
    Cursor.ws t;
    match Cursor.peek_delim t with
    | Some '+' ->
        let right =
          Cursor.atomic t (fun () ->
              Cursor.skip t;
              read_calc_term read_a t)
        in
        loop (Expr (left, Add, right))
    | Some '-' ->
        let right =
          Cursor.atomic t (fun () ->
              Cursor.skip t;
              read_calc_term read_a t)
        in
        loop (Expr (left, Sub, right))
    | _ -> left
  in
  loop (read_calc_term read_a t)

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
  (* A zero in calc is a plain Number_tok with value 0, not a dimension. *)
  let snap = Cursor.save t in
  match Cursor.number_opt t with
  | Some 0. -> Num 0.
  | _ ->
      Cursor.restore t snap;
      Cursor.err t "expected zero"

and read_calc_numeric_function : type a. Cursor.t -> a calc =
 fun t ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      match String.lowercase_ascii name with
      | "round" -> read_numeric_round t
      | "rem" -> read_numeric_rem t
      | "mod" -> read_numeric_binary_call "mod" mod_value t
      | "min" -> read_numeric_list_call "min" Float.min Float.infinity t
      | "max" -> read_numeric_list_call "max" Float.max Float.neg_infinity t
      | "clamp" -> read_numeric_clamp t
      (* CSS Values 4 §10.7 numeric math functions. We evaluate them at parse
         time to a [Num] so the surrounding [calc()] can fold further. *)
      | "sqrt" -> read_numeric_unary_call "sqrt" Float.sqrt t
      | "abs" -> read_numeric_unary_call "abs" Float.abs t
      | "sign" -> read_numeric_unary_call "sign" sign_float t
      | "exp" -> read_numeric_unary_call "exp" Float.exp t
      | "log" -> read_numeric_log t
      | "pow" -> read_numeric_binary_call "pow" Float.pow t
      | "hypot" -> read_numeric_hypot t
      | ("sin" | "cos" | "tan") as fn -> read_numeric_trig fn t
      | ("asin" | "acos" | "atan") as fn -> read_numeric_inverse_trig fn t
      | "atan2" -> read_numeric_atan2 t
      | _ -> Cursor.err t "expected numeric calc function")
  | _ -> Cursor.err t "expected numeric calc function"

and read_numeric_unary_call : type a.
    string -> (float -> float) -> Cursor.t -> a calc =
 fun name fn t ->
  Num
    (Cursor.call name t (fun inner ->
         let v = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         fn v))

and read_numeric_binary_call : type a.
    string -> (float -> float -> float) -> Cursor.t -> a calc =
 fun name fn t ->
  Num
    (Cursor.call name t (fun inner ->
         let a = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let b = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         fn a b))

and read_numeric_list_call : type a.
    string -> (float -> float -> float) -> float -> Cursor.t -> a calc =
 fun name fn initial t ->
  Num
    (Cursor.call name t (fun inner ->
         let nums =
           Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr inner
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         List.fold_left fn initial nums))

and read_numeric_round : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "round" t (fun inner ->
         let snap = Cursor.save inner in
         let strategy =
           match Cursor.peek_ident inner with
           | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
               Cursor.skip inner;
               Cursor.ws inner;
               Cursor.comma inner;
               kw
           | _ ->
               Cursor.restore inner snap;
               "nearest"
         in
         let value = Cursor.number inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let step = Cursor.number inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         round_to_step strategy value step))

and read_numeric_rem : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "rem" t (fun inner ->
         let a = Cursor.number inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let b = Cursor.number inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         Float.rem a b))

and read_numeric_clamp : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "clamp" t (fun inner ->
         let min_value = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let value = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let max_value = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         Float.max min_value (Float.min value max_value)))

and read_numeric_log : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "log" t (fun inner ->
         let v = read_num_expr inner in
         Cursor.ws inner;
         let base =
           if Cursor.comma_opt inner then Some (read_num_expr inner) else None
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         match base with
         | None -> Float.log v
         | Some b -> Float.log v /. Float.log b))

and read_numeric_hypot : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "hypot" t (fun inner ->
         let nums =
           Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr inner
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         let sum_sq = List.fold_left (fun acc x -> acc +. (x *. x)) 0. nums in
         Float.sqrt sum_sq))

and read_numeric_trig : type a. string -> Cursor.t -> a calc =
 fun fn t ->
  Num
    (Cursor.call fn t (fun inner ->
         let radians = read_trig_arg inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         match fn with
         | "sin" -> Float.sin radians
         | "cos" -> Float.cos radians
         | "tan" -> Float.tan radians
         | _ -> assert false))

and read_numeric_inverse_trig : type a. string -> Cursor.t -> a calc =
 fun fn t ->
  Num
    (Cursor.call fn t (fun inner ->
         let v = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         let result_rad =
           match fn with
           | "asin" -> Float.asin v
           | "acos" -> Float.acos v
           | "atan" -> Float.atan v
           | _ -> assert false
         in
         result_rad *. 180. /. Float.pi))

and read_numeric_atan2 : type a. Cursor.t -> a calc =
 fun t ->
  Num
    (Cursor.call "atan2" t (fun inner ->
         let y = read_num_expr inner in
         Cursor.ws inner;
         Cursor.comma inner;
         let x = read_num_expr inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         Float.atan2 y x *. 180. /. Float.pi))

(* Read a number-typed calc expression and fold it to a float. Used as the
   argument reader for [sqrt]/[pow]/[hypot]/etc. so [pow(2, sqrt(100))] and
   [hypot(3, 4)] evaluate end-to-end at parse time. *)
and read_num_expr t : float =
  let no_val t = Cursor.err t "expected number" in
  let calc = read_calc_expr no_val t in
  match eval_numeric_calc calc with
  | Some n -> n
  | None -> Cursor.err_invalid t "non-numeric calc expression"

(* CSS Values 4 §10.7.1: trig functions accept [<angle> | <number>] - a bare
   number is treated as radians. We support arithmetic over the input ([sin(pi /
   4)], [sin(22deg + 23deg)]) by recognising both shapes at every leaf and
   folding through a generic float-leaf calc evaluator. *)
and read_trig_arg t : float =
  let read_leaf t =
    let snap = Cursor.save t in
    match Cursor.number_with_unit t with
    | n, Some "deg" -> n *. Float.pi /. 180.
    | n, Some "rad" -> n
    | n, Some "turn" -> n *. 2. *. Float.pi
    | n, Some "grad" -> n *. Float.pi /. 200.
    | n, None -> n
    | _, Some _ ->
        Cursor.restore t snap;
        Cursor.err_invalid t "expected angle or number"
  in
  let calc = read_calc_expr read_leaf t in
  let rec eval : float calc -> float option = function
    | Num f -> Some f
    | Val f -> Some f
    | Sibling_index | Sibling_count | Var _ -> None
    | Nested inner | Parens inner -> eval inner
    | Expr (l, op, r) -> (
        match (eval l, eval r) with
        | Some lv, Some rv -> (
            match op with
            | Add -> Some (lv +. rv)
            | Sub -> Some (lv -. rv)
            | Mul -> Some (lv *. rv)
            | Div when rv <> 0. -> Some (lv /. rv)
            | Div -> None)
        | _ -> None)
  in
  match eval calc with
  | Some f -> f
  | None -> Cursor.err_invalid t "trig argument must be numeric"

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
        (* CSS Values 4 §10.7.1 math constants ([pi], [e], [infinity],
           [-infinity], [NaN]) appear as bare identifiers inside [calc()]. *)
        let read_math_constant t : a calc =
          let snap = Cursor.save t in
          match Cursor.ident_opt t with
          | Some name -> (
              match String.lowercase_ascii name with
              | "pi" -> Num Float.pi
              | "e" -> Num (Float.exp 1.)
              | "infinity" -> Num Float.infinity
              | "-infinity" -> Num Float.neg_infinity
              | "nan" -> Num Float.nan
              | _ ->
                  Cursor.restore t snap;
                  Cursor.err t "expected math constant")
          | None ->
              Cursor.restore t snap;
              Cursor.err t "expected math constant"
        in
        (* CSS Values 4 10.7: a dimension factor like [-1px] or [-5em] should be
           read as [Val] (full dimension) before falling back to [Num] (a raw
           number). Otherwise [Cursor.number] consumes the [-1] and leaves [px]
           hanging in the input. *)
        Cursor.one_of
          [
            read_calc_zero;
            read_val;
            read_calc_numeric_function;
            read_num;
            read_math_constant;
          ]
          t

and read_calc : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  if Cursor.looking_at_func "calc" t then
    Cursor.call "calc" t (fun inner ->
        let result = read_calc_expr read_a inner in
        (* CSS Values 4 10: a [calc()] body must be a single expression --
           [calc(1px 2px)] (missing operator) leaves [2px] unconsumed and must
           be rejected. *)
        Cursor.ws inner;
        Cursor.expect_eof inner;
        result)
  else if Cursor.looking_at_func "var" t then Var (read_var read_a t)
  else Cursor.err t "calc() or var()"

let read_numeric_expression t = read_num_expr t

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
    (* Same exception as [read_length_percentage]: inside [calc()] the
       non-negative constraint applies to the resolved value. *)
    Calc (read_calc (read_length ~with_keywords) t)
  in
  let read_as_length t : length =
    Env (read_env (read_length ~allow_negative ~with_keywords) t)
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
                Cursor.consume_remaining_as_string inner |> normalize_math_args
              in
              let groups = top_level_arg_count s in
              if groups <> 3 then
                Cursor.err_invalid inner
                  "clamp() requires three comma-separated arguments"
              else Clamp s
          | "minmax" ->
              let s =
                Cursor.consume_remaining_as_string inner |> normalize_math_args
              in
              if top_level_arg_count s <> 2 then
                Cursor.err_invalid inner
                  "minmax() requires two comma-separated arguments"
              else Minmax s
          | "min" ->
              let s =
                Cursor.consume_remaining_as_string inner |> normalize_math_args
              in
              if top_level_arg_count s < 1 then
                Cursor.err_invalid inner "min() requires at least one argument"
              else Min s
          | "max" ->
              let s =
                Cursor.consume_remaining_as_string inner |> normalize_math_args
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
              (* CSS Values 4 10.7: [round()] takes an optional strategy
                 ([nearest], [up], [down], [to-zero]); when omitted the default
                 is [nearest]. *)
              let snap = Cursor.save inner in
              let strategy =
                match Cursor.peek_ident inner with
                | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
                    Cursor.skip inner;
                    Cursor.ws inner;
                    Cursor.comma inner;
                    kw
                | _ ->
                    Cursor.restore inner snap;
                    "nearest"
              in
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
              let values =
                Cursor.list ~sep:Cursor.comma
                  (read_length ~allow_negative ~with_keywords)
                  inner
              in
              Cursor.expect_eof inner;
              Hypot values
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
              let size = Cursor.consume_remaining_as_string ~trim:true inner in
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
          | "attr" ->
              let read_type inner =
                Cursor.ws inner;
                let body =
                  Cursor.consume_remaining_as_string ~trim:true inner
                in
                match attr_syntax_of_string body with
                | Some syntax -> Type syntax
                | None -> Cursor.err_invalid inner ("attr() type: " ^ body)
              in
              let read_type_hint inner : attr_type option =
                Cursor.ws inner;
                if Cursor.is_done inner || Cursor.peek_comma inner then
                  Option.None
                else if Cursor.looking_at_func "type" inner then
                  Option.Some (Cursor.call "type" inner read_type)
                else
                  match Cursor.ident_opt inner with
                  | Some "raw-string" -> Option.Some Raw_string
                  | Some "number" -> Option.Some Number_type
                  | Some unit_ -> Option.Some (Unit unit_)
                  | None when Cursor.peek_delim inner = Some '%' ->
                      Cursor.expect '%' inner;
                      Option.Some (Unit "%")
                  | None -> Cursor.err_expected inner "attr() type"
              in
              Cursor.ws inner;
              let name = Cursor.ident ~keep_case:true inner in
              let type_ = read_type_hint inner in
              Cursor.ws inner;
              let fallback =
                if Cursor.comma_opt inner then (
                  Cursor.ws inner;
                  if Cursor.is_done inner then Empty_fallback
                  else
                    Attr_fallback
                      (read_length ~allow_negative ~with_keywords inner))
                else No_fallback
              in
              Cursor.ws inner;
              Cursor.expect_eof inner;
              Attr { name; type_; fallback }
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
      read_as_length;
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
let read_percentage_float t : float =
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    0.)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" | None -> max 0. (min 100. n)
    | Some unit -> Cursor.err_invalid t ("percentage unit: " ^ unit)

(** Read an alpha value *)
let rec read_alpha t : alpha =
  Cursor.ws t;
  let read_var_alpha t : alpha = Var (read_var read_alpha t) in
  let read_calc_alpha t : alpha = Calc (read_calc read_alpha t) in
  let read_none t : alpha =
    Cursor.expect_string "none" t;
    None
  in
  let read_pct t : alpha =
    (* Alpha percentages are clamped to 0-100 per CSS spec *)
    Pct (Cursor.pct ~clamp:true t)
  in
  let read_num t : alpha =
    (* Fall back to reading as numeric alpha *)
    let n, repr, unit = Cursor.number_repr_with_unit t in
    (match unit with
    | None -> ()
    | Some unit -> Cursor.err_invalid t ("alpha unit: " ^ unit));
    (* Clamp numeric alpha to 0-1 range per CSS spec *)
    let value = max 0. (min 1. n) in
    let repr = if value = n then repr else Pp.string_of_float value in
    Numeric { value; repr }
  in
  Cursor.one_of
    [ read_var_alpha; read_calc_alpha; read_none; read_pct; read_num ]
    t

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
  else if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Int 0)
  else if
    match Cursor.peek t with
    | Some (Component.Func { node = { name; _ }; _ })
      when String.lowercase_ascii name <> "var" ->
        true
    | _ -> false
  then Int (int_of_float (max 0. (min 255. (read_num_expr t))))
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
    let snap = Cursor.save t in
    let rgb_var = read_rgb_var t in
    Cursor.ws t;
    if Cursor.is_done t then Rgb (Var rgb_var)
    else if Cursor.peek_delim t = Some '/' then (
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      match alpha with
      | None -> Cursor.err t "expected alpha value after '/'"
      | _ -> Rgba { rgb = Var rgb_var; a = alpha; legacy = false })
    else (
      Cursor.restore t snap;
      let r = read_channel t in
      Cursor.ws t;
      let g = read_channel t in
      Cursor.ws t;
      let b = read_channel t in
      let alpha = read_optional_alpha t in
      Cursor.ws t;
      if not (Cursor.is_done t) then
        Cursor.err t "unexpected tokens after rgb()";
      match alpha with
      | None -> Rgb (Channels { r; g; b })
      | Num _ | Numeric _ | Pct _ | Var _ | Calc _ ->
          Rgba { rgb = Channels { r; g; b }; a = alpha; legacy = false }))
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
    | Num _ | Numeric _ | Pct _ | Var _ | Calc _ ->
        Rgba { rgb = Channels { r; g; b }; a = alpha; legacy = false }

and read_rgb_comma_separated ~legacy t : color =
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
  | a -> Rgba { rgb = Channels { r; g; b }; a; legacy }

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

let rec read_color_component t : component =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Component_none)
  else if Cursor.looking_at t "var(" then Var (read_var read_color_component t)
  else if Cursor.looking_at t "calc(" then
    Calc (read_calc read_color_component t)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> Pct n
    | Some u -> Cursor.err_invalid t ("unit: " ^ u)
    | None -> Num n

(** Read color components until the alpha separator ['/'] or end of input. *)
let rec read_color_components space t acc =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then List.rev acc
  else
    let component_count = List.length acc in
    let component = read_color_component t in
    (match ((space : color_space), component_count, component) with
    | (Lab | Oklab | Lch | Oklch), 0, (Pct _ | Var _ | Calc _ | Component_none)
      ->
        ()
    | (Lab | Oklab | Lch | Oklch), 0, _ ->
        Cursor.err_invalid t "L component must be percentage"
    | _ -> ());
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

(** Argument category for [atan2(y, x)]: both arguments must share a category
    for the result to be unit-free. The float is the value already reduced into
    the category's canonical base ([px] for length, [ms] for time, [Hz] for
    frequency, [deg] for angle, raw fraction for percent). *)
type atan2_category =
  | At_number
  | At_percentage
  | At_length
  | At_angle
  | At_time
  | At_frequency

let read_atan2_scalar t : atan2_category * float =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit = String.lowercase_ascii (Option.value ~default:"" unit_raw) in
  match unit with
  | "" -> (At_number, n)
  | "%" -> (At_percentage, n)
  (* CSS Values 4 §6.2 absolute lengths converted to [px]. *)
  | "px" -> (At_length, n)
  | "cm" -> (At_length, n *. (96. /. 2.54))
  | "mm" -> (At_length, n *. (96. /. 25.4))
  | "q" -> (At_length, n *. (96. /. 101.6))
  | "in" -> (At_length, n *. 96.)
  | "pt" -> (At_length, n *. (96. /. 72.))
  | "pc" -> (At_length, n *. 16.)
  (* Relative lengths share the [<length>] type but cannot reduce to a constant
     scalar at parse time; group them so [atan2(1vw, -1vw)] succeeds via
     raw-value ratio while [atan2(1vw, 1px)] is rejected. *)
  | "em" | "rem" | "ex" | "cap" | "ch" | "ic" | "lh" | "rlh" | "vw" | "vh"
  | "vmin" | "vmax" | "vi" | "vb" | "dvh" | "dvw" | "dvmin" | "dvmax" | "lvh"
  | "lvw" | "lvmin" | "lvmax" | "svh" | "svw" | "svmin" | "svmax" | "cqw"
  | "cqh" | "cqi" | "cqb" | "cqmin" | "cqmax" ->
      (At_length, n)
  (* Angles, times, frequencies in their canonical units. *)
  | "deg" -> (At_angle, n)
  | "rad" -> (At_angle, n *. 180. /. Float.pi)
  | "turn" -> (At_angle, n *. 360.)
  | "grad" -> (At_angle, n *. 0.9)
  | "ms" -> (At_time, n)
  | "s" -> (At_time, n *. 1000.)
  | "hz" -> (At_frequency, n)
  | "khz" -> (At_frequency, n *. 1000.)
  | _ -> Cursor.err_invalid t ("invalid atan2 argument unit: " ^ unit)

(** Read an angle value *)
let read_angle_round read_angle t =
  Cursor.call "round" t (fun inner ->
      let snap = Cursor.save inner in
      let strategy =
        match Cursor.peek_ident inner with
        | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
            Cursor.skip inner;
            Cursor.ws inner;
            Cursor.comma inner;
            kw
        | _ ->
            Cursor.restore inner snap;
            "nearest"
      in
      let value = read_angle inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let step = read_angle inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Round (strategy, value, step) : angle))

let read_angle_binary name make read_angle t =
  Cursor.call name t (fun inner ->
      let a = read_angle inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let b = read_angle inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make a b)

let angle_trig_function t =
  if Cursor.looking_at_func "asin" t then Some (`Asin, "asin")
  else if Cursor.looking_at_func "acos" t then Some (`Acos, "acos")
  else if Cursor.looking_at_func "atan" t then Some (`Atan, "atan")
  else None

let read_angle_trig kind name t =
  let typed t =
    Cursor.call name t (fun inner ->
        let v = read_num_expr inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        let radians =
          match kind with
          | `Asin -> Float.asin v
          | `Acos -> Float.acos v
          | `Atan -> Float.atan v
        in
        (Deg (radians *. 180. /. Float.pi) : angle))
  in
  match Cursor.try_typed_call typed t with
  | Ok value -> value
  | Error comp -> Invalid [ comp ]

let read_angle_atan2 t =
  let typed t =
    Cursor.call "atan2" t (fun inner ->
        (* CSS Values 4 §10.7: atan2(y, x) accepts <number>|<dimension>|
           <percentage> for both arguments (must match types). When both
           arguments reduce to a scalar in a shared category, the ratio is
           unit-free and the result folds to a [Deg] constant. *)
        let y = read_atan2_scalar inner in
        Cursor.ws inner;
        Cursor.comma inner;
        Cursor.ws inner;
        let x = read_atan2_scalar inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        match (y, x) with
        | (yk, yv), (xk, xv) when yk = xk ->
            (Deg (Float.atan2 yv xv *. 180. /. Float.pi) : angle)
        | _ -> Cursor.err_invalid inner "atan2 arguments have mismatched types")
  in
  match Cursor.try_typed_call typed t with
  | Ok value -> value
  | Error comp -> Invalid [ comp ]

let read_angle_unit ~unitless_zero t =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit_raw = Option.value unit_raw ~default:"" in
  match String.lowercase_ascii unit_raw with
  | "deg" -> Deg n
  | "rad" -> Rad n
  | "turn" -> Turn n
  | "grad" -> Grad n
  | "" when unitless_zero && n = 0. -> Deg 0.
  | "" ->
      Cursor.err_invalid t
        "angle values must have units (deg, rad, turn, or grad)"
  | unit -> Cursor.err_invalid t ("invalid angle unit: " ^ unit)

let rec read_angle_with ~unitless_zero t : angle =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Var (read_var (read_angle_with ~unitless_zero) t)
  else if Cursor.looking_at t "calc(" then
    Calc (read_calc (read_angle_with ~unitless_zero) t)
  else if Cursor.looking_at_func "round" t then
    read_angle_round (read_angle_with ~unitless_zero) t
  else if Cursor.looking_at_func "rem" t then
    read_angle_binary "rem"
      (fun a b -> (Rem (a, b) : angle))
      (read_angle_with ~unitless_zero)
      t
  else if Cursor.looking_at_func "mod" t then
    read_angle_binary "mod"
      (fun a b -> (Mod (a, b) : angle))
      (read_angle_with ~unitless_zero)
      t
  else
    match angle_trig_function t with
    | Some (kind, name) -> read_angle_trig kind name t
    | None ->
        if Cursor.looking_at_func "atan2" t then read_angle_atan2 t
        else read_angle_unit ~unitless_zero t

let read_angle t = read_angle_with ~unitless_zero:true t
let read_angle_unit_required t = read_angle_with ~unitless_zero:false t

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
  let h = match read_hue t with Hue_none -> Unitless 0. | h -> h in
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
  let h = match read_hue t with Hue_none -> Unitless 0. | h -> h in
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

let read_ok_lightness t : percentage option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    Option.None)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" -> Some (Pct n : percentage)
    | None when n >= 0. && n <= 1. -> Some (Pct (n *. 100.) : percentage)
    | _ ->
        Cursor.err_invalid t
          ("oklch() L value must be 0-1 or 0%-100%, got " ^ string_of_float n)

let read_number_or_none ?pct_scale t : float option =
  Cursor.ws t;
  if Cursor.looking_at t "none" then (
    Cursor.expect_string "none" t;
    None)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        Some
          (n
          *.
          match pct_scale with
          | Some scale -> scale
          | None -> Cursor.err_invalid t "unexpected percentage")
    | None -> Some n
    | Some unit -> Cursor.err_invalid t ("invalid unit: " ^ unit)

let read_oklch t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec. *)
  let l = read_ok_lightness t in
  Cursor.ws t;
  let c = read_number_or_none ~pct_scale:0.004 t in
  Cursor.ws t;
  let h = read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklch { l; c; h; alpha }

let read_oklab t : color =
  Cursor.ws t;
  (* L can be 0-1 or 0%-100% per CSS spec *)
  let l = read_ok_lightness t in
  let a = read_number_or_none ~pct_scale:0.004 t in
  let b = read_number_or_none ~pct_scale:0.004 t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Oklab { l; a; b; alpha }

let read_lab t : color =
  Cursor.ws t;
  (* CSS Color 4 10: [lab()]'s L axis accepts either [<percentage>] or
     [<number>]; the [Lab L*] coordinate is the same range either way, so a bare
     number is folded into the [Pct] storage. *)
  let l : percentage option =
    if Cursor.looking_at t "none" then (
      Cursor.expect_string "none" t;
      Option.None)
    else
      Some
        (Pct (Cursor.one_of [ read_percentage_float; Cursor.number ] t)
          : percentage)
  in
  let a = read_number_or_none ~pct_scale:1.25 t in
  let b = read_number_or_none ~pct_scale:1.25 t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lab { l; a; b; alpha }

let read_lch t : color =
  Cursor.ws t;
  let read_l_axis t : percentage option =
    if Cursor.looking_at t "none" then (
      Cursor.expect_string "none" t;
      Option.None)
    else
      Some
        (Pct (Cursor.one_of [ read_percentage_float; Cursor.number ] t)
          : percentage)
  in
  let read_c = read_number_or_none ~pct_scale:1.5 in
  let l, c, h = Cursor.triple ~sep:Cursor.ws read_l_axis read_c read_hue t in
  let alpha = read_optional_alpha t in
  Cursor.ws t;
  Cursor.expect_eof t;
  Lch { l; c; h; alpha }

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

let hue_interpolation_start = function
  | "shorter" | "longer" | "increasing" | "decreasing" | "specified" -> true
  | _ -> false

let read_hue_interpolation_direction t : hue_interpolation =
  Cursor.enum "hue-interpolation"
    [
      ("shorter", Shorter);
      ("longer", Longer);
      ("increasing", Increasing);
      ("decreasing", Decreasing);
      ("specified", Specified);
    ]
    t

let read_full_hue_interpolation t : hue_interpolation =
  let hue = read_hue_interpolation_direction t in
  Cursor.ws t;
  Cursor.expect_string "hue" t;
  Cursor.ws t;
  hue

let rec read_color_mix t : color =
  Cursor.ws t;
  let read_color_mix_component t =
    let read_prefix_percentage t =
      match read_optional_percentage t with
      | None -> Cursor.err_expected t "color-mix percentage"
      | Some percent -> (
          let color = read_color t in
          Cursor.ws t;
          match read_optional_percentage t with
          | None -> (color, Some percent)
          | Some _ ->
              Cursor.err_invalid t
                "color-mix component cannot have two percentages")
    in
    match Cursor.option read_prefix_percentage t with
    | Some component -> component
    | None -> (
        let color = read_color t in
        Cursor.ws t;
        match read_optional_percentage t with
        | None -> (color, None)
        | Some percent -> (color, Some percent))
  in

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
        | Some name when hue_interpolation_start (String.lowercase_ascii name)
          ->
            read_full_hue_interpolation t
        | _ -> Default
      in
      (Some space, hue))
    else (None, Default)
  in

  Cursor.ws t;
  Cursor.comma t;
  Cursor.ws t;

  (* Parse first color and optional percentage *)
  let color1, percent1 = read_color_mix_component t in

  Cursor.comma t;
  Cursor.ws t;

  (* Parse second color and optional percentage *)
  let color2, percent2 = read_color_mix_component t in

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

and relative_color_channel_count cvs =
  let is_ws = function
    | Component.Preserved { Token.kind = Whitespace; _ } -> true
    | _ -> false
  in
  let is_alpha_sep = function
    | Component.Preserved { Token.kind = Delim "/"; _ } -> true
    | _ -> false
  in
  let rec loop count in_channel = function
    | [] -> if in_channel then count + 1 else count
    | cv :: _ when is_alpha_sep cv -> if in_channel then count + 1 else count
    | cv :: rest when is_ws cv ->
        loop (if in_channel then count + 1 else count) false rest
    | _ :: rest -> loop count true rest
  in
  loop 0 false cvs

and relative_color_has_empty_alpha cvs =
  let is_ws = function
    | Component.Preserved { Token.kind = Whitespace; _ } -> true
    | _ -> false
  in
  let rec only_ws = function
    | [] -> true
    | cv :: rest when is_ws cv -> only_ws rest
    | _ -> false
  in
  let rec loop = function
    | [] -> false
    | Component.Preserved { Token.kind = Delim "/"; _ } :: rest -> only_ws rest
    | _ :: rest -> loop rest
  in
  loop cvs

and read_relative_rgb t : color =
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  let tail_components = Cursor.remaining t in
  if relative_color_has_empty_alpha tail_components then
    Cursor.err_expected t "relative rgb alpha";
  let tail =
    Cursor.consume_remaining_as_string ~trim:true t
    |> normalize_relative_color_tail
  in
  if tail = "" then Cursor.err_expected t "relative rgb channels";
  if relative_color_channel_count tail_components <> 3 then
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

and read_color_attr t : color =
  Cursor.ws t;
  let name = Cursor.ident t in
  Cursor.ws t;
  Cursor.call "type" t (fun inner ->
      Cursor.ws inner;
      Cursor.expect '<' inner;
      Cursor.expect_string "color" inner;
      Cursor.expect '>' inner;
      Cursor.ws inner;
      Cursor.expect_eof inner);
  Cursor.ws t;
  let fallback =
    if Cursor.comma_opt t then (
      Cursor.ws t;
      Some (read_color t))
    else None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Attribute (name, fallback)

and read_relative_color name t : color =
  (* CSS Color 5 §2: any colour function may take [from <origin> <c1> <c2> <c3>
     [/ <alpha>]?]. We capture the body verbatim so the printer re-emits the
     function name + parenthesised tail unchanged. *)
  Cursor.ws t;
  Cursor.expect_string "from" t;
  Cursor.ws t;
  let origin = read_color t in
  Cursor.ws t;
  let tail =
    Cursor.consume_remaining_as_string ~trim:true t
    |> normalize_relative_color_tail
  in
  if tail = "" then Cursor.err_expected t (name ^ " channels");
  let origin = Pp.to_string ~minify:true pp_color origin in
  Relative_color (name, "from " ^ origin ^ " " ^ tail)

and with_relative_fallback name fallback t =
  Cursor.ws t;
  if Cursor.looking_at t "from" then read_relative_color name t else fallback t

and color_parsers =
  [
    ( "rgb",
      fun t ->
        Cursor.ws t;
        if Cursor.looking_at t "from" then read_relative_rgb t
        else
          Cursor.one_of
            [ read_rgb_space_separated; read_rgb_comma_separated ~legacy:false ]
            t
    );
    ( "rgba",
      fun t ->
        Cursor.ws t;
        Cursor.one_of
          [ read_rgb_space_separated; read_rgb_comma_separated ~legacy:true ]
          t
    );
    ("hsl", fun t -> with_relative_fallback "hsl" read_hsl t);
    ("hsla", fun t -> with_relative_fallback "hsl" read_hsl t);
    ("hwb", fun t -> with_relative_fallback "hwb" read_hwb t);
    ("oklch", fun t -> with_relative_fallback "oklch" read_oklch t);
    ("lab", fun t -> with_relative_fallback "lab" read_lab t);
    ("oklab", fun t -> with_relative_fallback "oklab" read_oklab t);
    ("lch", fun t -> with_relative_fallback "lch" read_lch t);
    ("color", fun t -> with_relative_fallback "color" read_color_function t);
    ("contrast-color", read_contrast_color);
    ("light-dark", read_light_dark);
    ("attr", read_color_attr);
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

let rec color_has_specified_hue = function
  | Mix { hue = Specified; _ } -> true
  | Mix { color1; color2; _ } ->
      color_has_specified_hue color1 || color_has_specified_hue color2
  | Contrast_color color -> color_has_specified_hue color
  | Light_dark (a, b) -> color_has_specified_hue a || color_has_specified_hue b
  | Attribute (_, Some color) -> color_has_specified_hue color
  | _ -> false

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

let duration_css_wide =
  [
    ("inherit", (Inherit : duration));
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let read_duration_number ~canonicalize_ms:_ t : duration =
  let n, unit_raw = Cursor.number_with_unit t in
  if n < 0.0 then Cursor.err_invalid t "negative durations are not allowed"
  else
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "" when n = 0.0 -> S 0.0
    | "s" -> S n
    | "ms" -> Ms n
    | _ -> Cursor.err_invalid t ("duration unit: " ^ unit)

let read_time_number t : duration =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  match unit with
  | "" when n = 0.0 -> S 0.0
  | "s" -> S n
  | "ms" -> Ms n
  | _ -> Cursor.err_invalid t ("time unit: " ^ unit)

let rec read_duration_with ?(css_wide = true) ~canonicalize_ms t : duration =
  let read_duration_self t = read_duration_with ~css_wide ~canonicalize_ms t in
  let read_round t =
    Cursor.call "round" t (fun inner ->
        let snap = Cursor.save inner in
        let strategy =
          match Cursor.peek_ident inner with
          | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
              Cursor.skip inner;
              Cursor.ws inner;
              Cursor.comma inner;
              kw
          | _ ->
              Cursor.restore inner snap;
              "nearest"
        in
        let value = read_duration_self inner in
        Cursor.ws inner;
        Cursor.comma inner;
        let step = read_duration_self inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        (Round (strategy, value, step) : duration))
  in
  let read_rem t =
    Cursor.call "rem" t (fun inner ->
        let a = read_duration_self inner in
        Cursor.ws inner;
        Cursor.comma inner;
        let b = read_duration_self inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        (Rem (a, b) : duration))
  in
  let read_mod t =
    Cursor.call "mod" t (fun inner ->
        let a = read_duration_self inner in
        Cursor.ws inner;
        Cursor.comma inner;
        let b = read_duration_self inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        (Mod (a, b) : duration))
  in
  Cursor.enum_or_calls
    ~default:(read_duration_number ~canonicalize_ms)
    "duration"
    (if css_wide then duration_css_wide else [])
    ~calls:
      [
        ("var", fun t -> Var (read_var read_duration_self t));
        ("calc", fun t -> Calc (read_calc read_duration_in_calc t));
        ("round", read_round);
        ("rem", read_rem);
        ("mod", read_mod);
      ]
    t

(** Read a duration value *)
and read_duration t : duration = read_duration_with ~canonicalize_ms:true t

and read_duration_preserve_ms t : duration =
  read_duration_with ~canonicalize_ms:false t

and read_duration_in_calc t : duration =
  read_duration_with ~css_wide:false ~canonicalize_ms:false t

(** Read a time value that can be negative (for animation-delay,
    transition-delay) *)
let rec read_time_with ?(css_wide = true) t : duration =
  Cursor.enum_or_calls ~default:read_time_number "time"
    (if css_wide then duration_css_wide else [])
    ~calls:
      [
        ("var", fun t -> Var (read_var read_time t));
        ("calc", fun t -> Calc (read_calc read_time_in_calc t));
      ]
    t

and read_time t : duration = read_time_with t
and read_time_in_calc t : duration = read_time_with ~css_wide:false t

let number_binary_functions =
  [
    ("mod", fun a b -> Mod (a, b));
    ("rem", fun a b -> Rem (a, b));
    ("hypot", fun a b -> Hypot (a, b));
    ("pow", fun a b -> Pow (a, b));
  ]

let number_unary_functions =
  [
    ("sqrt", fun value -> Sqrt value);
    ("abs", fun value -> Abs value);
    ("sign", fun value -> Sign value);
  ]

let angle_number_functions = [ ("sin", fun value -> Sin value) ]

(** Read a number value *)
let rec read_number t : number =
  Cursor.ws t;
  let number =
    match read_number_function t with
    | Some value -> value
    | None -> Num (Cursor.number t)
  in
  Cursor.ws t;
  (match Cursor.peek t with
  | Some (Component.Func _)
  | Some (Component.Preserved { kind = Token.Number_tok _; _ }) ->
      Cursor.err_invalid t "unexpected tokens after number"
  | _ -> ());
  number

and read_number_function t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      let name = String.lowercase_ascii name in
      match name with
      | "var" -> Some (Var (read_var read_number t))
      | "calc" -> Some (Calc (read_calc read_number t))
      | "min" ->
          Some
            (Num
               (Cursor.call "min" t (fun inner ->
                    let nums =
                      Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr
                        inner
                    in
                    Cursor.ws inner;
                    Cursor.expect_eof inner;
                    List.fold_left Float.min Float.infinity nums)))
      | "max" ->
          Some
            (Num
               (Cursor.call "max" t (fun inner ->
                    let nums =
                      Cursor.list ~sep:Cursor.comma ~at_least:1 read_num_expr
                        inner
                    in
                    Cursor.ws inner;
                    Cursor.expect_eof inner;
                    List.fold_left Float.max Float.neg_infinity nums)))
      | "clamp" ->
          Some
            (Num
               (Cursor.call "clamp" t (fun inner ->
                    let min_value = read_num_expr inner in
                    Cursor.ws inner;
                    Cursor.comma inner;
                    let value = read_num_expr inner in
                    Cursor.ws inner;
                    Cursor.comma inner;
                    let max_value = read_num_expr inner in
                    Cursor.ws inner;
                    Cursor.expect_eof inner;
                    Float.max min_value (Float.min value max_value))))
      | "round" -> Some (read_round_number t)
      | _ -> read_math_number_function t name)
  | _ -> None

and read_math_number_function t name =
  match List.assoc_opt name number_binary_functions with
  | Some make -> Some (read_binary_number_function name make t)
  | None -> (
      match List.assoc_opt name number_unary_functions with
      | Some make -> Some (read_unary_number_function name make t)
      | None -> (
          match List.assoc_opt name angle_number_functions with
          | Some make -> Some (read_angle_number_function name make t)
          | None -> None))

and read_round_number t =
  Cursor.call "round" t (fun inner ->
      let snap = Cursor.save inner in
      let strategy =
        match Cursor.peek_ident inner with
        | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
            Cursor.skip inner;
            Cursor.ws inner;
            Cursor.comma inner;
            kw
        | _ ->
            Cursor.restore inner snap;
            "nearest"
      in
      let value = read_number inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let step = read_number inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Round (strategy, value, step))

and read_binary_number_function name make t =
  Cursor.call name t (fun inner ->
      let a = read_number inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let b = read_number inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make a b)

and read_unary_number_function name make t =
  Cursor.call name t (fun inner ->
      let value = read_number inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make value)

and read_angle_number_function name make t =
  Cursor.call name t (fun inner ->
      let value = read_angle inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make value)

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

(* CSS Values 5 §10: math functions that produce a non-length type ([asin] /
   [acos] / [atan] / [atan2] return angles, [sin] / [cos] / [tan] return
   numbers). Used in a [<length-percentage>] context they are spec-invalid;
   lightning et al. preserve verbatim, so cascade captures the original call as
   [Invalid [<call>]]. *)
let length_invalid_function_name name =
  match String.lowercase_ascii name with
  | "asin" | "acos" | "atan" | "atan2" | "sin" | "cos" | "tan" -> true
  | _ -> false

(** Read length_percentage value *)
let rec read_length_percentage ?(allow_negative = true) ?(with_keywords = true)
    t : length_percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then
    Var (read_var (read_length_percentage ~allow_negative ~with_keywords) t)
  else if Cursor.looking_at t "env(" then
    Env (read_env (read_length_percentage ~allow_negative ~with_keywords) t)
  else if Cursor.looking_at t "calc(" then
    (* CSS Values 4 10 (calc): inside [calc()] negative operands are always
       allowed even when the surrounding property is non-negative; the
       non-negative constraint applies to the resolved value, not to inner
       operands. *)
    Calc (read_calc (read_length_percentage ~with_keywords) t)
  else
    match Cursor.peek t with
    | Some (Component.Func { node = { name; _ }; _ } as comp)
      when length_invalid_function_name name ->
        Cursor.skip t;
        Invalid [ comp ]
    | _ ->
        (* Try to read as percentage or length *)
        let read_pct t : length_percentage =
          let n = Cursor.pct t in
          if (not allow_negative) && n < 0.0 then
            Cursor.err_invalid t "negative";
          Pct n
        in
        let read_length_as_lp t : length_percentage =
          Length (read_length ~allow_negative ~with_keywords t)
        in
        Cursor.one_of [ read_pct; read_length_as_lp ] t

(** Read number_percentage value. Inside a [<number-percentage>] [calc()], a raw
    number is modelled at the calc level as the [Num x] node rather than
    [Val (Num x)] (the [Num] sub-variant of [number_percentage] wrapped in
    [Val]); the dedicated [_dim_only] reader excludes the raw-number alternative
    so the generic [read_calc_factor] falls through to its own [Num] path. *)
let rec read_number_percentage t : number_percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_number_percentage t)
  else if Cursor.looking_at t "calc(" then
    Calc (read_calc read_number_percentage_dim_only t)
  else if
    Cursor.looking_at_func "min" t
    || Cursor.looking_at_func "max" t
    || Cursor.looking_at_func "clamp" t
  then Num (read_numeric_expression t)
  else
    (* Try to read as percentage or number *)
    Cursor.one_of
      [ (fun t -> Pct (Cursor.pct t)); (fun t -> Num (Cursor.number t)) ]
      t

and read_number_percentage_dim_only t : number_percentage =
  Cursor.ws t;
  Cursor.one_of
    [
      (fun t -> Pct (Cursor.pct t));
      (fun t ->
        (Var (read_var read_number_percentage_dim_only t) : number_percentage));
    ]
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
  let hue =
    Cursor.enum "hue-interpolation"
      [
        ("shorter", Shorter);
        ("longer", Longer);
        ("increasing", Increasing);
        ("decreasing", Decreasing);
        ("specified", Specified);
        ("default", Default);
      ]
      t
  in
  (match hue with
  | Default -> ()
  | _ ->
      Cursor.ws t;
      ignore (Cursor.option (Cursor.expect_string "hue") t));
  hue

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
let var_name (v : 'a var) = v.name
let var_layer (v : 'a var) = v.layer
let var_meta (v : 'a var) = v.meta

let with_fallback (v : 'a var) fallback_value : 'a var =
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
  let rec read_margin_component t : length =
    if Cursor.looking_at_func "var" t then
      Var (read_var read_margin_component t)
    else
      Cursor.enum "margin component"
        [ ("auto", (Auto : length)) ]
        ~default:(read_length ~with_keywords:false)
        t
  in
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
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4 read_margin_component t)
    t
