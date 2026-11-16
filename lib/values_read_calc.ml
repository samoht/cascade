(** Readers for [calc()], the math functions and their result types. *)

open Values_intf
open Values_var
open Values_calc
open Values_length

(* CSS Values 4 sec. 5.3: a signed zero is the same value as unsigned zero.
   Collapse any zero to the canonical [0]: normalise [-0.] to [0.] (so the AST
   never carries a negative-zero float, which breaks structural equality) and
   regenerate the repr from that value rather than echo the authored sign, so
   [-0px] / [+0px] serialise as [0px]. *)
(* CSS Values 4 sec. 5.1.1 font-relative and sec. 5.1.4 container-relative
   lengths resolve against something an element is given -- its font, its query
   container -- so a value carrying one has no computed form until an element
   has it. Sec. 5.1.3's viewport lengths resolve against the viewport, which
   every element shares, and sec. 5.2's absolute lengths against nothing, so
   neither depends on an element. *)
let element_relative_units =
  [
    "em";
    "rem";
    "ex";
    "rex";
    "cap";
    "rcap";
    "ch";
    "rch";
    "ic";
    "ric";
    "lh";
    "rlh";
    "cqw";
    "cqh";
    "cqi";
    "cqb";
    "cqmin";
    "cqmax";
  ]

let is_element_relative_unit unit =
  List.mem (String.lowercase_ascii unit) element_relative_units

let normalize_signed_zero n repr =
  if n = 0.0 then (0.0, Pp.string_of_float 0.0) else (n, repr)

let read_length_unit ?(allow_negative = true) ?(length_only = false) t =
  let n, repr, unit_raw = Cursor.number_repr_with_unit t in
  let n, repr = normalize_signed_zero n repr in
  if (not allow_negative) && n < 0.0 then Cursor.err_invalid t "negative";
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  let authored unit =
    match unit_raw with
    | Some raw_unit when n = 0.0 ->
        Dimension { value = n; unit = raw_unit; repr }
    | _ when Pp.string_of_float n = repr -> length_of_unit unit n
    | _ ->
        Dimension { value = n; unit = Option.value unit_raw ~default:""; repr }
  in
  match unit with
  (* CSS Values 4 sec. 5.5: a [<percentage>] is its own type, so [0%] is [Pct
     0.] and keeps [%]; folding it to the length zero would change its type (and
     is unsound for definiteness-sensitive properties like [height]). *)
  | "%" when not length_only -> (Pct n : length)
  | "%" -> Cursor.err_invalid t "percentage where a length is required"
  | "" when n = 0.0 -> Zero
  | "" -> Cursor.err t "length values must have units (except for zero)"
  | unit -> (
      match unit_of_string unit with
      | Some unit -> authored unit
      | None ->
          (* Unknown / future unit (CSS Values 4 section 5.4 reserves dimension
             tokens for future units). Readers stay binary: raise here, and the
             declaration-level recovery in [Declaration.read_declaration]
             catches the [Parse_error] and emits a warning that strict mode
             escalates to [Error]. Inside [calc()] / [sign()] / [abs()] etc. the
             math-arg path constructs a [Dim (n, unit)] leaf via
             [Cursor.number_with_unit] without going through this reader, so
             [calc(1x + 2x)] still parses. *)
          Cursor.err_invalid t ("unknown dimension unit: " ^ unit))

(* CSS Values 4 (ED) sec. 6 gives a [<length>] no keyword of its own beyond the
   CSS-wide ones, and [auto] belongs to nearly every property that reads one.
   The intrinsic sizes ([min-content] and the rest) belong to CSS Sizing 3 sec.
   5 alone, so a property reading a length asks for them; [top: min-content] and
   [margin-top: none] are values Chrome 146 refuses. *)
let length_cssvalue_keywords : (string * length) list =
  [
    ("auto", (Auto : length));
    ("inherit", Inherit);
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let length_sizing_keywords : (string * length) list =
  [
    ("none", (None : length));
    ("normal", Normal);
    ("size", Size);
    ("max-content", Max_content);
    ("min-content", Min_content);
    ("fit-content", Fit_content);
    (* Legacy vendor-prefixed intrinsic sizing keywords (kept as a fallback for
       old Safari / Firefox; the unprefixed forms above win in modern ones). *)
    ("-webkit-max-content", Webkit_max_content);
    ("-webkit-min-content", Webkit_min_content);
    ("-webkit-fit-content", Webkit_fit_content);
    ("-moz-max-content", Moz_max_content);
    ("-moz-min-content", Moz_min_content);
    ("-moz-fit-content", Moz_fit_content);
    ("contain", Contain);
    ("stretch", Stretch);
    ("from-font", From_font);
  ]

let read_length_keyword ?(sizing = true) t : length =
  Cursor.enum "length"
    (if sizing then length_cssvalue_keywords @ length_sizing_keywords
     else length_cssvalue_keywords)
    t

let calc_factor_is_dimension : type a. a calc -> bool = function
  | Val _ -> true
  | _ -> false

let read_calc_zero : type a. Cursor.t -> a calc =
 fun t ->
  (* A zero in calc is a plain Number_tok with value 0, not a dimension. *)
  let snap = Cursor.save t in
  match Cursor.number_opt t with
  | Some 0. -> Num 0.
  | _ ->
      Cursor.restore t snap;
      Cursor.err t "expected zero"

let math_arg_of_numeric_calc : type a. Cursor.t -> a calc -> math_arg =
 fun t -> function
  | Math_fn fn -> Math_call fn
  | Num f -> Lit f
  | _ -> Cursor.err t "expected numeric math arg"

let read_math_constant_name t name =
  match String.lowercase_ascii name with
  | "pi" -> Const Pi
  | "e" -> Const E
  | "infinity" -> Const Infinity
  | "-infinity" -> Const Neg_infinity
  | "nan" -> Const Nan
  | _ -> Cursor.err t "expected math constant"

let read_math_number_with_unit t =
  (* CSS Values 4 sec. 10.6 [sign()] / [abs()] accept a [<calc-sum>] over any
     numeric type, including dimensions and percentages. Capture the leading
     number plus its unit so [sign(-1vw)] and [sign(1%)] preserve their source
     shape. *)
  match Cursor.number_with_unit t with
  | n, Some unit -> Dim (n, unit)
  | n, None -> Lit n

let read_math_constant_or_number t =
  match Cursor.ident_opt t with
  | Some name -> read_math_constant_name t name
  | None -> read_math_number_with_unit t

(* CSS Values 4 (ED) sec. 10.7: [round()] takes an optional strategy ([nearest],
   [up], [down], [to-zero]); when omitted the default is [nearest]. *)
let read_round_strategy inner =
  let snap = Cursor.save inner in
  match Cursor.peek_keyword inner with
  | Some (("nearest" | "up" | "down" | "to-zero") as kw) ->
      Cursor.skip inner;
      Cursor.ws inner;
      Cursor.comma inner;
      kw
  | _ ->
      Cursor.restore inner snap;
      "nearest"

(* [round(<strategy>?, value, step)] over any argument reader. *)
let read_round_call make read_x t =
  Cursor.call "round" t (fun inner ->
      let strategy = read_round_strategy inner in
      let value = read_x inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let step = read_x inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make strategy value step)

(* [rem(a, b)] / [mod(a, b)] and the other two-argument math calls, over any
   argument reader. *)
let read_binary_call name make read_x t =
  Cursor.call name t (fun inner ->
      let a = read_x inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let b = read_x inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make a b)

let read_var_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  if Cursor.looking_at_func "var" t then Var (read_var read_a t)
  else Cursor.err t "expected var"

let read_sibling_index_factor : type a. Cursor.t -> a calc =
 fun t ->
  if Cursor.looking_at_func "sibling-index" t then
    Cursor.call "sibling-index" t (fun inner ->
        Cursor.expect_eof inner;
        Sibling_index)
  else Cursor.err t "expected sibling-index"

let read_sibling_count_factor : type a. Cursor.t -> a calc =
 fun t ->
  if Cursor.looking_at_func "sibling-count" t then
    Cursor.call "sibling-count" t (fun inner ->
        Cursor.expect_eof inner;
        Sibling_count)
  else Cursor.err t "expected sibling-count"

let math_constant_factor_of_name : type a. Cursor.t -> _ -> string -> a calc =
 fun t snap name ->
  match String.lowercase_ascii name with
  | "pi" -> Math_const Pi
  | "e" -> Math_const E
  | "infinity" -> Math_const Infinity
  | "-infinity" -> Math_const Neg_infinity
  | "nan" -> Math_const Nan
  | _ ->
      Cursor.restore t snap;
      Cursor.err t "expected math constant"

let read_math_constant_factor : type a. Cursor.t -> a calc =
 fun t ->
  (* CSS Values 4 sec. 10.7 math constants ([pi], [e], [infinity], [-infinity],
     [NaN]) appear as bare identifiers inside [calc()]. *)
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some name -> math_constant_factor_of_name t snap name
  | None ->
      Cursor.restore t snap;
      Cursor.err t "expected math constant"

(* CSS Values 4 (ED) sec. 10.8: "whitespace is required on both sides of the +
   and - operators. (The * and / operators can be used without white space
   around them.)" Browsers enforce it, so a one-sided sum is not a math function
   and the whole value is invalid. [ws_before] answers for the left side, which
   the caller must have measured with [skip_ws]: [peek_delim] drops the
   whitespace before it reports the delimiter. *)
let skip_sum_operator t ~ws_before =
  if not ws_before then Cursor.err t "expected whitespace before '+' or '-'";
  Cursor.skip t;
  if not (Cursor.skip_ws t) then
    Cursor.err t "expected whitespace after '+' or '-'"

(* Sec. 10.9 requires the arguments of a stepped-value function to "have a
   consistent type or else the function is invalid" and answers with that type,
   so a pair whose units do not combine is no calculation at all. A [var()] is
   substituted before the type check, so it defers rather than fails.

   A [<number>] result is the coefficient itself: folding it here keeps the call
   out of the calculation tree, where it would read as a type the inference
   cannot place and would then stand at a [<length>] slot that takes no number.
   A result carrying a unit is the one the tree has to hold. *)
let checked_stepped_call : type a. Cursor.t -> math_fn -> a calc =
 fun t fn ->
  match math_fn_result fn with
  | Some (Scalar v) -> Num v
  | Some (United _) -> Math_fn fn
  | None ->
      if math_fn_contains_var fn then Math_fn fn
      else Cursor.err_invalid t "inconsistent stepped-value arguments"

let rec read_calc_expr : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  (* CSS Values 4 sec. 10.1 evaluates same-precedence operators left-to-right,
     so [a - b - c] groups as [(a - b) - c]. Loop on subsequent operators rather
     than recursing on the right, which would group it as [a - (b - c)]. *)
  let rec loop left =
    let ws_before = Cursor.skip_ws t in
    match Cursor.peek_delim t with
    | Some '+' ->
        let right =
          Cursor.atomic t (fun () ->
              skip_sum_operator t ~ws_before;
              read_calc_term read_a t)
        in
        loop (Expr (left, Add, right))
    | Some '-' ->
        let right =
          Cursor.atomic t (fun () ->
              skip_sum_operator t ~ws_before;
              read_calc_term read_a t)
        in
        loop (Expr (left, Sub, right))
    | _ -> left
  in
  loop (read_calc_term read_a t)

and read_calc_term : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  read_calc_term_tail read_a t (read_calc_factor read_a t)

and read_calc_term_tail : type a.
    (Cursor.t -> a) -> Cursor.t -> a calc -> a calc =
 fun read_a t left ->
  (* Put back the whitespace when the next operator is not [*] or [/]: it is the
     left-hand side [read_calc_expr] measures for a [+] or [-]. *)
  let snap = Cursor.save t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '*' -> read_calc_product read_a t left
  | Some '/' -> read_calc_quotient read_a t left
  | _ ->
      Cursor.restore t snap;
      left

and read_calc_product : type a. (Cursor.t -> a) -> Cursor.t -> a calc -> a calc
    =
 fun read_a t left ->
  (* Use atomic to ensure we either parse the full multiplication or nothing. *)
  Cursor.atomic t (fun () ->
      Cursor.skip t;
      Cursor.ws t;
      let right = read_calc_factor read_a t in
      if calc_factor_is_dimension left && calc_factor_is_dimension right then
        Cursor.err t "invalid calc: cannot multiply two dimensions";
      read_calc_term_tail read_a t (Expr (left, Mul, right)))

and read_calc_quotient : type a. (Cursor.t -> a) -> Cursor.t -> a calc -> a calc
    =
 fun read_a t left ->
  (* Use atomic to ensure we either parse the full division or nothing. *)
  Cursor.atomic t (fun () ->
      Cursor.skip t;
      Cursor.ws t;
      let right = read_calc_factor read_a t in
      read_calc_term_tail read_a t (Expr (left, Div, right)))

and read_calc_parenthesized : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Parens (Cursor.parens (fun inner -> read_calc_expr read_a inner) t)

and read_calc_numeric_function : type a. Cursor.t -> a calc =
 fun t ->
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) -> (
      match String.lowercase_ascii name with
      (* CSS Values 4 (ED) sec. 10.9 spells all three arguments [<calc-sum>], so
         a dimension is one, and answers with the arguments' own type: the call
         is read into the typed [Math_fn] AST rather than folded to a
         coefficient, which is what kept it out of a [<length>] slot. *)
      | "round" -> read_stepped_round t
      | "mod" -> read_stepped_binary "mod" (fun a b -> Mod_n (a, b)) t
      | "rem" -> read_stepped_binary "rem" (fun a b -> Rem_n (a, b)) t
      | "min" -> read_numeric_list_call "min" Float.min Float.infinity t
      | "max" -> read_numeric_list_call "max" Float.max Float.neg_infinity t
      | "clamp" -> read_numeric_clamp t
      (* CSS Values 4 (ED) sec. 9.1 numeric math functions: parsed into the
         typed [Math_fn] AST so pretty pp re-emits [name(args)]; the optimizer
         (or minify pp) folds via [eval_math_fn]. *)
      | "sqrt" -> Math_fn (Sqrt (read_math_call_arg "sqrt" t))
      | "abs" -> Math_fn (Abs_n (read_math_call_arg "abs" t))
      | "sign" -> Math_fn (Sign_n (read_math_call_arg "sign" t))
      | "exp" -> Math_fn (Exp (read_math_call_arg "exp" t))
      | "log" -> read_math_log t
      | "pow" -> read_math_binary "pow" (fun a b -> Pow (a, b)) t
      | "hypot" -> read_math_hypot t
      | "sin" -> Math_fn (Sin (read_angle_call_arg "sin" t))
      | "cos" -> Math_fn (Cos (read_angle_call_arg "cos" t))
      | "tan" -> Math_fn (Tan (read_angle_call_arg "tan" t))
      | "asin" -> Math_fn (Asin (read_math_call_arg "asin" t))
      | "acos" -> Math_fn (Acos (read_math_call_arg "acos" t))
      | "atan" -> Math_fn (Atan (read_math_call_arg "atan" t))
      | "atan2" -> read_math_binary "atan2" (fun a b -> Atan2 (a, b)) t
      | _ -> Cursor.err t "expected numeric calc function")
  | _ -> Cursor.err t "expected numeric calc function"

and read_math_arg t : math_arg = read_math_arg_term t

and read_math_arg_term t =
  let left = read_math_arg_factor t in
  let ws_before = Cursor.skip_ws t in
  match Cursor.peek_delim t with
  | Some ('+' as c) | Some ('-' as c) ->
      skip_sum_operator t ~ws_before;
      let op : calc_op = match c with '+' -> Add | _ -> Sub in
      Op (left, op, read_math_arg_term t)
  | _ -> left

and read_math_arg_factor t =
  let left = read_math_arg_unary t in
  let snap = Cursor.save t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('*' as c) | Some ('/' as c) ->
      Cursor.skip t;
      Cursor.ws t;
      let op : calc_op = match c with '*' -> Mul | _ -> Div in
      Op (left, op, read_math_arg_factor t)
  | _ ->
      Cursor.restore t snap;
      left

and read_math_arg_unary t =
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren ->
      Parens_arg (Cursor.parens (fun inner -> read_math_arg inner) t)
  | _ -> read_math_arg_atom t

and read_math_arg_atom t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      read_math_arg_function t name
  | _ -> read_math_constant_or_number t

and read_math_arg_function t name =
  match String.lowercase_ascii name with
  | "var" ->
      Var_arg (read_var (fun inner -> read_math_arg inner) t : math_arg var)
  | _ -> math_arg_of_numeric_calc t (read_calc_numeric_function t)

and read_math_call_arg name t : math_arg =
  Cursor.call name t (fun inner ->
      Cursor.ws inner;
      let v = read_math_arg inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      v)

and read_math_binary : type a.
    string -> (math_arg -> math_arg -> math_fn) -> Cursor.t -> a calc =
 fun name mk t ->
  Math_fn
    (Cursor.call name t (fun inner ->
         Cursor.ws inner;
         let a = read_math_arg inner in
         Cursor.ws inner;
         Cursor.comma inner;
         Cursor.ws inner;
         let b = read_math_arg inner in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         mk a b))

and read_math_log : type a. Cursor.t -> a calc =
 fun t ->
  Math_fn
    (Cursor.call "log" t (fun inner ->
         Cursor.ws inner;
         let a = read_math_arg inner in
         Cursor.ws inner;
         let b =
           if Cursor.comma_opt inner then (
             Cursor.ws inner;
             let b = read_math_arg inner in
             Some b)
           else None
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         Log (a, b)))

and read_math_hypot : type a. Cursor.t -> a calc =
 fun t ->
  Math_fn
    (Cursor.call "hypot" t (fun inner ->
         Cursor.ws inner;
         let args =
           Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg inner
         in
         Cursor.ws inner;
         Cursor.expect_eof inner;
         (Hypot args : math_fn)))

and read_angle_arg t : angle_arg = read_angle_arg_term t

and read_angle_arg_term t =
  let left = read_angle_arg_factor t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('+' as c) | Some ('-' as c) ->
      Cursor.skip t;
      Cursor.ws t;
      let op : calc_op = match c with '+' -> Add | _ -> Sub in
      Operation (left, op, read_angle_arg_term t)
  | _ -> left

and read_angle_arg_factor t =
  let left = read_angle_arg_unary t in
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('*' as c) | Some ('/' as c) ->
      Cursor.skip t;
      Cursor.ws t;
      let op : calc_op = match c with '*' -> Mul | _ -> Div in
      Operation (left, op, read_angle_arg_factor t)
  | _ -> left

and read_angle_arg_unary t =
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren ->
      Grouped (Cursor.parens (fun inner -> read_angle_arg inner) t)
  | _ -> read_angle_unit_or_math t

and read_angle_unit_or_math t =
  let snap = Cursor.save t in
  match Cursor.number_with_unit t with
  | n, Some unit -> angle_arg_of_unit t snap n unit
  | _, None -> restore_and_read_math_angle t snap
  | exception Cursor.Parse_error _ -> restore_and_read_math_angle t snap

and angle_arg_of_unit t snap n unit =
  match String.lowercase_ascii unit with
  | "deg" -> Deg n
  | "rad" -> Rad n
  | "turn" -> Turn n
  | "grad" -> Grad n
  | _ -> restore_and_read_math_angle t snap

and restore_and_read_math_angle t snap =
  Cursor.restore t snap;
  Numeric_arg (read_math_arg t)

and read_angle_call_arg name t : angle_arg =
  Cursor.call name t (fun inner ->
      Cursor.ws inner;
      let arg = read_angle_arg inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      arg)

and read_math_arg_ws t =
  Cursor.ws t;
  read_math_arg t

and read_stepped_round : type a. Cursor.t -> a calc =
 fun t ->
  checked_stepped_call t
    (read_round_call
       (fun strategy value step -> (Round_n (strategy, value, step) : math_fn))
       read_math_arg_ws t)

and read_stepped_binary : type a.
    string -> (math_arg -> math_arg -> math_fn) -> Cursor.t -> a calc =
 fun name mk t ->
  checked_stepped_call t (read_binary_call name mk read_math_arg_ws t)

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

(* Read a number-typed calc expression and fold it to a float. Used as the
   argument reader for [round] / [mod] / [rem] / [min] / [max] / [clamp] which
   still flatten numeric args to [Num] under the existing typed-fold path. *)
and read_num_expr t : float =
  let no_val t = Cursor.err t "expected number" in
  let calc = read_calc_expr no_val t in
  match eval_numeric_calc calc with
  | Some n -> n
  | None -> Cursor.err_invalid t "non-numeric calc expression"

and read_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.ws t;
  match Cursor.peek_block t with
  | Some Token.Paren -> read_calc_parenthesized read_a t
  | _ -> read_calc_non_paren_factor read_a t

and read_calc_non_paren_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  Cursor.one_of
    [
      read_nested_calc_factor read_a;
      read_var_calc_factor read_a;
      read_sibling_index_factor;
      read_sibling_count_factor;
      read_calc_zero;
      read_calc_numeric_function;
      (* A plain unitless [<number>] in a calc is the [<number>] type, not the
         surrounding property's value, so read it as a [Num] leaf before the
         typed reader. Otherwise a property that accepts a bare number (e.g.
         line-height, opacity) reads it as a [Val], which the multiplication
         dimension check then mistakes for a dimension and rejects [2 * 3]. A
         dimension ([2px]) or percentage token is not a [Number_tok], so it
         still falls through to the typed [Val] reader. *)
      (fun t -> (Num (Cursor.number t) : a calc));
      (fun t -> Val (read_a t));
      read_math_constant_factor;
    ]
    t

and read_nested_calc_factor : type a. (Cursor.t -> a) -> Cursor.t -> a calc =
 fun read_a t ->
  if Cursor.looking_at_func "calc" t then
    Nested (Cursor.call "calc" t (fun inner -> read_calc_expr read_a inner))
  else if Cursor.looking_at_func "-webkit-calc" t then
    Nested
      (Cursor.call "-webkit-calc" t (fun inner -> read_calc_expr read_a inner))
  else Cursor.err t "expected nested calc"

type inferred_calc_type =
  | Dimension of int
    (* Exponent 0 is a number, 1 is the contextual numeric value, and
       multiplication/division add/subtract exponents. This retains valid
       cancellation such as [1 / (1 / 50px)]. *)
  | Result_unit of string
    (* A math function answering the type of its arguments (CSS Values 4 sec.
       10.5 [hypot()], sec. 10.6 [abs()]). Which slots take it turns on the
       unit, so it is not the contextual [Dimension 1]. *)
  | Deferred
  | Invalid

(* The type a call folding to a bare coefficient answers. Sec. 10.4 gives the
   inverse trigonometric functions an [<angle>] that [math_fn_result] carries as
   a bare degree count, so the coefficient names no type to check and they stay
   deferred. Every other function here is [<number>] out, so sec. 10.9 types the
   sum it sits in. *)
let math_fn_scalar_type = function
  | Asin _ | Acos _ | Atan _ | Atan2 _ -> Deferred
  | Sin _ | Cos _ | Tan _ | Sqrt _ | Exp _ | Log _ | Pow _ | Hypot _ | Sign_n _
  | Abs_n _ | Round_n _ | Mod_n _ | Rem_n _ ->
      Dimension 0

(* Inside an operand tree a call that answers its arguments' type is the
   contextual dimension: the leaf reader beside it has already vouched for the
   unit. Only a whole calculation keeps the unit for the slot to judge. *)
let rec infer_calc_operand : type a. a calc -> inferred_calc_type =
 fun calc ->
  match infer_calc_type calc with
  | Result_unit _ -> Dimension 1
  | inferred -> inferred

and infer_calc_type : type a. a calc -> inferred_calc_type = function
  | Num _ | Math_const _ | Sibling_index | Sibling_count -> Dimension 0
  | Val _ -> Dimension 1
  (* A var() is substituted before a math function is type-checked, so it stays
     deferred rather than being guessed. *)
  | Var _ -> Deferred
  | Math_fn fn -> (
      match math_fn_result fn with
      | Some (United (_, unit)) -> Result_unit unit
      | Some (Scalar _) -> math_fn_scalar_type fn
      (* A call that does not fold keeps no type to check against. *)
      | None -> Deferred)
  | Nested inner | Parens inner -> infer_calc_type inner
  | Expr (left, (Add | Sub), right) -> (
      match (infer_calc_operand left, infer_calc_operand right) with
      | Invalid, _ | _, Invalid -> Invalid
      | Result_unit _, _ | _, Result_unit _ -> Invalid
      | Deferred, _ | _, Deferred -> Deferred
      | Dimension l, Dimension r -> if l = r then Dimension l else Invalid)
  | Expr (left, Mul, right) -> (
      match (infer_calc_operand left, infer_calc_operand right) with
      | Invalid, _ | _, Invalid -> Invalid
      | Result_unit _, _ | _, Result_unit _ -> Invalid
      | Deferred, _ | _, Deferred -> Deferred
      | Dimension l, Dimension r -> Dimension (l + r))
  | Expr (left, Div, right) -> (
      match (infer_calc_operand left, infer_calc_operand right) with
      | Invalid, _ | _, Invalid -> Invalid
      | Result_unit _, _ | _, Result_unit _ -> Invalid
      | Deferred, _ | _, Deferred -> Deferred
      | Dimension l, Dimension r -> Dimension (l - r))

let validate_calc_type t result_type calc =
  let inferred = infer_calc_type calc in
  let accepted =
    match (result_type, inferred) with
    | _, Deferred -> true
    (* A call answering its arguments' type stands where that type stands: at a
       [<number>] slot it does not, and at an [<opacity-value>] only the
       [<percentage>] the slot resolves against its number does. [`Value] is the
       slot whose contextual dimension the leaf reader already vouched for, so
       the unit is that one; a [<percentage>] slot names its own. *)
    | `Number, Result_unit _ -> false
    | (`Number_or_percentage | `Percentage), Result_unit unit ->
        String.equal unit "%"
    | (`Value | `Number_or_value), Result_unit _ -> true
    | `Number, Dimension 0 -> true
    | (`Value | `Percentage), Dimension 1 -> true
    | (`Number_or_value | `Number_or_percentage), (Dimension 0 | Dimension 1) ->
        true
    | ( ( `Number | `Value | `Number_or_value | `Number_or_percentage
        | `Percentage ),
        (Invalid | Dimension _) ) ->
        false
  in
  if not accepted then Cursor.err_invalid t "incompatible calc types"

(* CSS Values 4 sec. 10.2 comparison functions. Their answer is one of their
   arguments, so a slot spelling the arguments' type already has a leaf for the
   result and the call folds to it rather than standing as a calculation. *)
let comparison_math_function_names = [ "min"; "max"; "clamp" ]

(* Sec. 10.9 stepped-value functions, sec. 10.5 [hypot()] and sec. 10.6 [abs()]:
   each answers the type of its arguments too, and each computes a value none of
   them spelled, so the call stays in the calculation. *)
let computed_typed_math_function_names =
  [ "round"; "mod"; "rem"; "hypot"; "abs" ]

(* Together they are the functions a [<length>] slot takes as readily as a
   [<number>] one. *)
let typed_math_function_names =
  comparison_math_function_names @ computed_typed_math_function_names

(* Sec. 10.5 exponential functions, sec. 10.4 trigonometric ones and sec. 10.6
   [sign()]: all answer a [<number>] whatever went in, so only a [<number>] slot
   takes them. Together with the list above these are the names
   [read_calc_numeric_function] dispatches; [calc()] is not among them, having
   its own entry. *)
let number_math_function_names =
  [
    "sign";
    "sqrt";
    "exp";
    "log";
    "pow";
    "sin";
    "cos";
    "tan";
    "asin";
    "acos";
    "atan";
    "atan2";
  ]

let looking_at_math_function t =
  match Cursor.peek_function_name t with
  | Some name ->
      List.mem name typed_math_function_names
      || List.mem name number_math_function_names
  | None -> false

let math_function_calls read =
  List.map
    (fun n -> (n, read))
    (typed_math_function_names @ number_math_function_names)

let typed_math_function_calls read =
  List.map (fun n -> (n, read)) typed_math_function_names

(* Sec. 10.2 requires a comparison function's arguments to have a consistent
   type and answers with that type, so at a [<percentage>] slot every argument
   is a [<calc-sum>] resolving to a percentage: a bare coefficient is a
   [<number>] the slot does not spell, and a length is a type of its own. *)
let read_percentage_argument t =
  let arg = read_math_arg t in
  match math_arg_result arg with
  | Some (United (v, unit)) when String.equal unit "%" -> v
  | Some (Scalar _ | United _) | None -> Cursor.err_expected t "percentage"

let read_percentage_list_call name pick initial t =
  Cursor.call name t (fun inner ->
      let args =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_percentage_argument inner
      in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      List.fold_left pick initial args)

let read_percentage_clamp t =
  Cursor.call "clamp" t (fun inner ->
      let low = read_percentage_argument inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let value = read_percentage_argument inner in
      Cursor.ws inner;
      Cursor.comma inner;
      let high = read_percentage_argument inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Float.max low (Float.min value high))

let math_function_calls_beside_comparisons read =
  List.map
    (fun n -> (n, read))
    (computed_typed_math_function_names @ number_math_function_names)

let percentage_math_function_calls ~pct read =
  ("min", fun t -> pct t (read_percentage_list_call "min" Float.min infinity t))
  :: ( "max",
       fun t -> pct t (read_percentage_list_call "max" Float.max neg_infinity t)
     )
  :: ("clamp", fun t -> pct t (read_percentage_clamp t))
  :: math_function_calls_beside_comparisons read

(* Sec. 10.1 puts a math function wherever the [<percentage>] stands and makes
   [calc()] one of them rather than the gate to the rest, so both spellings
   answer the same percentage. A slot holding a percentage and no calculation
   node takes the answer and nothing else: a call this cannot reduce -- a
   [var()] among its operands, or a result of another type -- is no value for
   it. *)
let read_folded_percentage_math t =
  let body name =
    Cursor.call name t (fun inner ->
        let value = read_percentage_argument inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        value)
  in
  if Cursor.looking_at_func "calc" t then body "calc"
  else if Cursor.looking_at_func "-webkit-calc" t then body "-webkit-calc"
  else
    match Cursor.peek_function_name t with
    | Some "min" -> read_percentage_list_call "min" Float.min infinity t
    | Some "max" -> read_percentage_list_call "max" Float.max neg_infinity t
    | Some "clamp" -> read_percentage_clamp t
    | Some _ when looking_at_math_function t -> read_percentage_argument t
    | Some _ | None -> Cursor.err_expected t "percentage"

let looking_at_percentage_math t =
  Cursor.looking_at_calc t || looking_at_math_function t

let read_calc : type a.
    ?result_type:
      [ `Number
      | `Number_or_percentage
      | `Number_or_value
      | `Percentage
      | `Value ] ->
    (Cursor.t -> a) ->
    Cursor.t ->
    a calc =
 fun ?result_type read_a t ->
  Cursor.ws t;
  let read name =
    Cursor.call name t (fun inner ->
        let result = read_calc_expr read_a inner in
        (* CSS Values 4 10: a [calc()] body must be a single expression --
           [calc(1px 2px)] (missing operator) leaves [2px] unconsumed and must
           be rejected. *)
        Cursor.ws inner;
        Cursor.expect_eof inner;
        Option.iter
          (fun result_type -> validate_calc_type inner result_type result)
          result_type;
        result)
  in
  if Cursor.looking_at_func "calc" t then read "calc"
  else if Cursor.looking_at_func "-webkit-calc" t then read "-webkit-calc"
  else if Cursor.looking_at_func "var" t then Var (read_var read_a t)
  else if looking_at_math_function t then (
    (* CSS Values 4 sec. 10.1: a math function is usable "wherever a <number>,
       <dimension>, or <percentage> is allowed", so [calc()] is one of them
       rather than the gate to the rest. A bare call is already a whole
       calculation and stands as the expression itself, with no wrapper node for
       a printer to put back, and takes the slot's type check the way the
       wrapped spelling does. *)
    let result = read_calc_numeric_function t in
    Option.iter
      (fun result_type -> validate_calc_type t result_type result)
      result_type;
    result)
  else Cursor.err t "calc() or var()"

(* CSS Values 4 (ED) sec. 10.9: a math function resolving to [<number>] stands
   wherever an [<integer>] is accepted. A constant expression folds to its
   integer; one holding a [var()] does not resolve here and stays a calc
   node. *)
let read_integer_calc : type a.
    string -> Cursor.t -> [ `Int of int | `Calc of a calc ] =
 fun name t ->
  let expr =
    read_calc ~result_type:`Number
      (fun _ ->
        Cursor.err t
          (String.concat "" [ "unexpected value in "; name; " calc" ]))
      t
  in
  (* CSS Values 4 sec. 10.12 rounds a math function at an [<integer>] to the
     nearest integer rather than refusing it, so a fractional result keeps the
     call and rounds where the value is used. *)
  match eval_numeric_calc expr with
  | Some f when Float.is_integer f -> (
      (* Past the [int] range the call stays, and sec. 10.12 clamps it to the
         range the user agent supports. *)
      match Common.int_of_float_opt f with
      | Some n -> `Int n
      | None -> `Calc expr)
  | Some _ | None -> `Calc expr

(* Sec. 10.12 rounds to the nearest integer with a tie going toward positive
   infinity, so this is the integer a slot's own range has to answer for: a call
   that rounds outside the range is as invalid as the literal would be. One
   holding a [var()] does not resolve here and has no integer yet, and an
   infinity or a NaN has none at all. One past the [int] range clamps to it, as
   the same section clamps an integer to the supported range, so it keeps its
   sign. *)
let calc_integer_value : type a. a calc -> int option =
 fun expr ->
  match eval_numeric_calc expr with
  | Some f when Float.is_finite f -> (
      let f = Float.floor (f +. 0.5) in
      match Common.int_of_float_opt f with
      | Some n -> Option.some n
      | None -> Option.some (if f > 0. then max_int else min_int))
  | Some _ | None -> Option.none

let read_integer name t =
  if Cursor.looking_at_calc t || looking_at_math_function t then
    match
      (read_integer_calc name t : [ `Int of int | `Calc of number calc ])
    with
    | `Int n -> n
    | `Calc _ ->
        Cursor.err_invalid t
          (String.concat "" [ name; " calc must evaluate to integer" ])
  else Cursor.int t

let read_numeric_expression t = read_num_expr t

(* An [<opacity-value>] slot takes [<number> | <percentage>], and CSS Values 4
   sec. 10.5 and 10.6 give [hypot()] and [abs()] the type of their arguments:
   the call stands here on the two types the slot takes from a literal, and the
   percentage resolves against the number the way [50%] does. *)
let read_number_percentage_expression t =
  let calc = read_calc_expr (fun t -> Cursor.err t "expected number") t in
  match numeric_calc_result calc with
  | Some (Scalar v) -> v
  | Some (United (v, unit)) when String.equal unit "%" -> v /. 100.
  | Some (United _) | None -> Cursor.err_invalid t "non-numeric math function"

(* The typed [clamp / minmax / min / max] length readers are part of the
   [read_length] mutual-recursion group (see below); the function-call
   dispatcher in [length_function_readers] references them. *)

let read_anchor_size_length inner =
  let size = Cursor.consume_remaining_as_string ~trim:true inner in
  if size = "" then Cursor.err_expected inner "anchor-size argument";
  Anchor_size size

let read_anchor_name_side inner first =
  if Custom_property_name.is_valid first then
    let side = Cursor.ident inner in
    (Some first, side)
  else (None, first)

let read_attr_length_type inner =
  Cursor.ws inner;
  let body = Cursor.consume_remaining_as_string ~trim:true inner in
  match attr_syntax_of_string body with
  | Some syntax -> Type syntax
  | None -> Cursor.err_invalid inner ("attr() type: " ^ body)

let read_plain_attr_type_hint inner : attr_type option =
  match Cursor.ident_opt inner with
  | Some "raw-string" -> Option.Some Raw_string
  | Some "number" -> Option.Some Number_type
  | Some unit_ -> Option.Some (Unit unit_)
  | None when Cursor.peek_delim inner = Some '%' ->
      Cursor.expect '%' inner;
      Option.Some (Unit "%")
  | None -> Cursor.err_expected inner "attr() type"

let read_attr_type_hint inner : attr_type option =
  Cursor.ws inner;
  if Cursor.is_done inner || Cursor.peek_comma inner then Option.None
  else if Cursor.looking_at_func "type" inner then
    Option.Some (Cursor.call "type" inner read_attr_length_type)
  else read_plain_attr_type_hint inner

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
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Number_or_value read_component t)
  else
    let n, unit = Cursor.number_with_unit t in
    match unit with
    | Some "%" ->
        (* Clamp percentage to 0-100 range per CSS spec *)
        Pct (max 0. (min 100. n))
    | _ ->
        (* Clamp numeric component to 0-255 range for RGB values per CSS spec *)
        Num (max 0. (min 255. n))
