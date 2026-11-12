(** Math functions and [calc()] trees: static evaluation, the typed reduction of
    CSS Values 4 sec. 10.10.1, and printing. *)

open Values_intf
open Values_var

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

let pp_math_const ctx = function
  | Pi -> Pp.string ctx "pi"
  | E -> Pp.string ctx "e"
  | Infinity -> Pp.string ctx "infinity"
  | Neg_infinity -> Pp.string ctx "-infinity"
  | Nan -> Pp.string ctx "NaN"

(* Inside a math function [NaN] is a keyword of the grammar (CSS Values 4 sec.
   10.7.2), so an operand spells a NaN with it; the [calc()] wrapper
   {!Pp.nan_value} adds is for the positions outside one. *)
let pp_calc_number ctx f =
  if Float.is_nan f then pp_math_const ctx Nan else Pp.float ctx f

let read_math_const t =
  match Cursor.ident_opt t with
  | Some name -> (
      match String.lowercase_ascii name with
      | "pi" -> Pi
      | "e" -> E
      | "infinity" -> Infinity
      | "-infinity" -> Neg_infinity
      | "nan" -> Nan
      | _ -> Cursor.err_expected t "math constant")
  | None -> Cursor.err_expected t "math constant"

let math_const_value = function
  | Pi -> Float.pi
  | E -> Float.exp 1.
  | Infinity -> Float.infinity
  | Neg_infinity -> Float.neg_infinity
  | Nan -> Float.nan

(* Inside a [calc()] [Expr], a math constant participates as its numeric value
   so the surrounding fold can run ([calc(pi * 100px)] -> [314.159px]). A
   standalone [calc(pi)] keeps the named constant, which serialises shorter than
   the decimal, so this is applied only to [Expr] operands. *)
let calc_operand_value : type a. a calc -> a calc = function
  | Math_const c -> Num (math_const_value c)
  | other -> other

(* The other direction, for a fold that lands on a NaN. Sec. 10.7.2 resolves
   [NaN] at parse time into a keyword of the [<number>] grammar and sec. 10.13
   serialises every NaN-valued calculation through that keyword, so CSS has one
   NaN and one node for it: a [Num] carrying the float would print [calc(NaN)]
   too and hash apart from the keyword the same text parses to. [infinity] /
   [-infinity] are constants of their own in that list and keep their values. *)
let calc_num : type a. float -> a calc =
 fun f -> if Float.is_nan f then Math_const Nan else Num f

let rec minify_angle_arg : angle_arg -> angle_arg = function
  | Operation (l, op, r) -> (
      let l = minify_angle_arg l in
      let r = minify_angle_arg r in
      match (l, op, r) with
      | Deg a, Add, Deg b -> Deg (a +. b)
      | Deg a, Sub, Deg b -> Deg (a -. b)
      | Rad a, Add, Rad b -> Rad (a +. b)
      | Rad a, Sub, Rad b -> Rad (a -. b)
      | Turn a, Add, Turn b -> Turn (a +. b)
      | Turn a, Sub, Turn b -> Turn (a -. b)
      | Grad a, Add, Grad b -> Grad (a +. b)
      | Grad a, Sub, Grad b -> Grad (a -. b)
      | _ -> Operation (l, op, r))
  | Grouped inner -> (
      match minify_angle_arg inner with
      | Operation _ as inner -> Grouped inner
      | inner -> inner)
  | arg -> arg

let round_to_step strategy value step =
  if step = 0. then value
  else
    let q = value /. step in
    let q =
      match strategy with
      | "up" -> Float.ceil q
      | "down" -> Float.floor q
      | "to-zero" -> Float.trunc q
      (* Sec. 10.7.3 sends a tie to the nearest multiple toward positive
         infinity, the rule sec. 10.12 gives an [<integer>] slot too.
         [Float.round] sends it away from zero, which answers the other way for
         a negative value. *)
      | _ -> Float.floor (q +. 0.5)
    in
    q *. step

let mod_value a b =
  if b = 0. then a
  else
    let q = Float.floor (a /. b) in
    a -. (q *. b)

let rec pp_math_arg ctx = function
  | Lit f -> pp_calc_number ctx f
  | Dim (f, unit_) -> Pp.unit ctx f unit_
  | Const c -> pp_math_const ctx c
  | Var_arg v -> pp_var pp_math_arg ctx v
  | Op (l, op, r) ->
      pp_math_arg ctx l;
      pp_calc_op ctx op;
      pp_math_arg ctx r
  | Parens_arg inner ->
      Pp.char ctx '(';
      pp_math_arg ctx inner;
      Pp.char ctx ')'
  | Math_call fn -> pp_math_fn ctx fn

and pp_math_fn ctx fn =
  let call name args =
    Pp.string ctx name;
    Pp.char ctx '(';
    Pp.list ~sep:Pp.comma pp_math_arg ctx args;
    Pp.char ctx ')'
  in
  match fn with
  | Sin a ->
      Pp.string ctx "sin(";
      pp_angle_arg ctx a;
      Pp.char ctx ')'
  | Cos a ->
      Pp.string ctx "cos(";
      pp_angle_arg ctx a;
      Pp.char ctx ')'
  | Tan a ->
      Pp.string ctx "tan(";
      pp_angle_arg ctx a;
      Pp.char ctx ')'
  | Asin a -> call "asin" [ a ]
  | Acos a -> call "acos" [ a ]
  | Atan a -> call "atan" [ a ]
  | Atan2 (y, x) -> call "atan2" [ y; x ]
  | Sqrt a -> call "sqrt" [ a ]
  | Exp a -> call "exp" [ a ]
  | Log (a, None) -> call "log" [ a ]
  | Log (a, Some b) -> call "log" [ a; b ]
  | Pow (a, b) -> call "pow" [ a; b ]
  | Hypot args -> call "hypot" args
  | Sign_n a -> call "sign" [ a ]
  | Abs_n a -> call "abs" [ a ]
  | Round_n (strategy, value, step) ->
      (* Sec. 10.9 defaults the strategy to [nearest], so the keyword is the
         longer spelling of the same call and is dropped. *)
      Pp.string ctx "round(";
      if not (String.equal strategy "nearest") then (
        Pp.string ctx strategy;
        Pp.comma ctx ());
      pp_math_arg ctx value;
      Pp.comma ctx ();
      pp_math_arg ctx step;
      Pp.char ctx ')'
  | Mod_n (a, b) -> call "mod" [ a; b ]
  | Rem_n (a, b) -> call "rem" [ a; b ]

and pp_angle_arg ctx arg =
  match if Pp.minified ctx then minify_angle_arg arg else arg with
  | Deg f -> Pp.unit ctx f "deg"
  | Rad f -> Pp.unit ctx f "rad"
  | Turn f -> Pp.unit ctx f "turn"
  | Grad f -> Pp.unit ctx f "grad"
  | Numeric_arg arg -> pp_math_arg ctx arg
  | Operation (l, op, r) ->
      pp_angle_arg ctx l;
      pp_calc_op ctx op;
      pp_angle_arg ctx r
  | Grouped inner ->
      Pp.char ctx '(';
      pp_angle_arg ctx inner;
      Pp.char ctx ')'

let rec eval_math_arg = function
  | Lit f -> Some f
  | Dim (f, _) -> Some f
  | Const c -> Some (math_const_value c)
  | Var_arg _ -> None
  | Op (l, op, r) -> (
      match (eval_math_arg l, eval_math_arg r) with
      | Some lv, Some rv -> (
          match op with
          | Add -> Some (lv +. rv)
          | Sub -> Some (lv -. rv)
          | Mul -> Some (lv *. rv)
          | Div when rv <> 0. -> Some (lv /. rv)
          | Div -> None)
      | _ -> None)
  | Parens_arg inner -> eval_math_arg inner
  | Math_call fn -> eval_math_fn fn

and eval_angle_arg : angle_arg -> float option = function
  | Deg f -> Some (f *. Float.pi /. 180.)
  | Rad f -> Some f
  | Turn f -> Some (f *. 2. *. Float.pi)
  | Grad f -> Some (f *. Float.pi /. 200.)
  | Numeric_arg arg -> eval_math_arg arg
  | Operation (l, op, r) -> (
      match (eval_angle_arg l, eval_angle_arg r) with
      | Some lv, Some rv -> (
          match op with
          | Add -> Some (lv +. rv)
          | Sub -> Some (lv -. rv)
          | Mul -> Some (lv *. rv)
          | Div when rv <> 0. -> Some (lv /. rv)
          | Div -> None)
      | _ -> None)
  | Grouped inner -> eval_angle_arg inner

and eval_math_fn fn =
  let unary f arg = Option.map f (eval_math_arg arg) in
  let binary f a b =
    match (eval_math_arg a, eval_math_arg b) with
    | Some av, Some bv -> Some (f av bv)
    | _ -> None
  in
  match fn with
  | Sin a -> Option.map Float.sin (eval_angle_arg a)
  | Cos a -> Option.map Float.cos (eval_angle_arg a)
  | Tan a -> Option.map Float.tan (eval_angle_arg a)
  | Asin a -> unary (fun v -> Float.asin v *. 180. /. Float.pi) a
  | Acos a -> unary (fun v -> Float.acos v *. 180. /. Float.pi) a
  | Atan a -> unary (fun v -> Float.atan v *. 180. /. Float.pi) a
  | Atan2 (y, x) -> binary (fun y x -> Float.atan2 y x *. 180. /. Float.pi) y x
  | Sqrt a -> unary Float.sqrt a
  | Exp a -> unary Float.exp a
  | Log (a, None) -> unary Float.log a
  | Log (a, Some b) -> (
      match (eval_math_arg a, eval_math_arg b) with
      | Some av, Some bv when bv > 0. && bv <> 1. ->
          Some (Float.log av /. Float.log bv)
      | _ -> None)
  | Pow (a, b) -> binary Float.pow a b
  | Hypot args ->
      let vs = List.filter_map eval_math_arg args in
      if List.length vs <> List.length args then None
      else
        let sum_sq = List.fold_left (fun acc v -> acc +. (v *. v)) 0. vs in
        Some (Float.sqrt sum_sq)
  | Sign_n a ->
      unary
        (fun v ->
          if Float.is_nan v then Float.nan
          else if v > 0. then 1.
          else if v < 0. then -1.
          else v)
        a
  | Abs_n a -> unary Float.abs a
  | Round_n (strategy, value, step) ->
      binary (round_to_step strategy) value step
  | Mod_n (a, b) -> binary mod_value a b
  | Rem_n (a, b) -> binary Float.rem a b

(* What a static math function reduces to: a plain coefficient, or one that
   carries a unit. CSS Values 4 sec. 10.7 gives [abs()] and [hypot()] the type
   of their arguments, so a typed [calc()] that reduces one has to learn the
   unit here; [eval_math_fn] answers with the coefficient alone, which is how
   [calc(hypot(3px, 4px))] came out as a bare [5]. *)
type math_result = Scalar of float | United of float * string

let math_result_unit = function
  | Scalar _ -> Option.none
  | United (_, unit) -> Option.some unit

let math_result_value = function Scalar v | United (v, _) -> v

let math_result_op op a b =
  match op with
  | Add -> Option.some (a +. b)
  | Sub -> Option.some (a -. b)
  | Mul -> Option.some (a *. b)
  | Div when b <> 0. -> Option.some (a /. b)
  | Div -> Option.none

let combine_math_result op l r =
  let united unit = Option.map (fun v -> United (v, unit)) in
  let scalar = Option.map (fun v -> Scalar v) in
  match (l, op, r) with
  | Scalar a, _, Scalar b -> scalar (math_result_op op a b)
  | United (a, u), (Add | Sub), United (b, v) when String.equal u v ->
      united u (math_result_op op a b)
  | United (a, u), (Mul | Div), Scalar b -> united u (math_result_op op a b)
  | Scalar a, Mul, United (b, u) -> united u (math_result_op Mul a b)
  | _ -> Option.none

(* CSS Values 4 sec. 10.11 types an operand tree: [+] and [-] need matching
   units, [*] takes at most one dimensioned operand and [/] a unitless divisor.
   Anything else is not an operand Cascade can reduce to a single value. *)
(* CSS Values 4 sec. 5.2 absolute lengths, and the canonical units of the other
   dimensions: each denotes the same quantity wherever it is written. Every
   other unit -- sec. 5.1's relative lengths, the container ones, a percentage
   -- resolves against something an element is given, and that reference can be
   zero. *)
let absolute_units =
  [
    "px";
    "cm";
    "mm";
    "q";
    "in";
    "pt";
    "pc";
    "s";
    "ms";
    "deg";
    "rad";
    "grad";
    "turn";
    "hz";
    "khz";
    "dpi";
    "dpcm";
    "dppx";
    "x";
  ]

let is_absolute_unit unit =
  List.mem (String.lowercase_ascii unit) absolute_units

let rec math_arg_result (arg : math_arg) : math_result option =
  match arg with
  | Lit f -> Option.some (Scalar f)
  | Dim (f, unit) -> Option.some (United (f, String.lowercase_ascii unit))
  | Const c -> Option.some (Scalar (math_const_value c))
  | Var_arg _ -> Option.none
  | Parens_arg inner -> math_arg_result inner
  | Math_call fn -> math_fn_result fn
  | Op (l, op, r) -> (
      match (math_arg_result l, math_arg_result r) with
      | Some l, Some r -> combine_math_result op l r
      | _ -> Option.none)

and math_fn_result (fn : math_fn) : math_result option =
  match fn with
  | Abs_n a -> (
      match math_arg_result a with
      | Some (Scalar v) -> Option.some (Scalar (Float.abs v))
      | Some (United (v, unit)) -> Option.some (United (Float.abs v, unit))
      | None -> Option.none)
  | Hypot args -> (
      let results = List.filter_map math_arg_result args in
      match results with
      | first :: rest
        when List.compare_lengths results args = 0
             && List.for_all
                  (fun r ->
                    Option.equal String.equal (math_result_unit r)
                      (math_result_unit first))
                  rest ->
          let square acc r =
            let v = math_result_value r in
            acc +. (v *. v)
          in
          let root = Float.sqrt (List.fold_left square 0. results) in
          Option.some
            (match math_result_unit first with
            | Some unit -> United (root, unit)
            | None -> Scalar root)
      | _ -> Option.none)
  | Round_n (strategy, value, step) ->
      stepped_result (round_to_step strategy) value step
  | Mod_n (a, b) -> stepped_result mod_value a b
  | Rem_n (a, b) -> stepped_result Float.rem a b
  (* Sec. 10.6: the answer turns on whether the argument is zero, and a relative
     unit has a reference that can be -- a zero font-size, a zero viewport, a
     zero container, a percentage of zero -- so the coefficient's sign is not
     the value's and the call waits for the reference. An absolute unit has none
     to wait for. *)
  | Sign_n a -> (
      let sign v =
        if Float.is_nan v then Float.nan
        else if v > 0. then 1.
        else if v < 0. then -1.
        else v
      in
      match math_arg_result a with
      | Some (Scalar v) -> Option.some (Scalar (sign v))
      | Some (United (v, unit)) when is_absolute_unit unit ->
          Option.some (Scalar (sign v))
      | Some (United _) | None -> Option.none)
  (* Every other math function is typed [<number>] in, [<number>] out, bar the
     inverse trig functions, whose [<angle>] result only the angle evaluator can
     place. *)
  | _ -> Option.map (fun v -> Scalar v) (eval_math_fn fn)

(* Sec. 10.9 gives the stepped-value functions arguments of one type and answers
   with that type, so two units have to match and the result keeps theirs. *)
and stepped_result f a b =
  match (math_arg_result a, math_arg_result b) with
  | Some (Scalar a), Some (Scalar b) -> Option.some (Scalar (f a b))
  | Some (United (a, u)), Some (United (b, v)) when String.equal u v ->
      Option.some (United (f a b, u))
  | _ -> Option.none

(* CSS Values 4 (ED) sec. 10.9.2: NaN belongs to a calculation tree and has no
   leaf spelling outside one, so folding a math function down to a NaN would
   lose the only form the value can take. Leave the function in place. *)
let fold_math_result = function
  | Some r when Float.is_nan (math_result_value r) -> Option.none
  | result -> result

(* A NaN coefficient may stay: it lands in the tree as a [Num], which sec.
   10.7.2 spells [NaN]. One that has picked up a unit would rebuild a leaf
   instead, and that leaf has no spelling, so the call keeps its own. *)
let fold_united_nan = function
  | Some (United (v, _)) when Float.is_nan v -> Option.none
  | result -> result

(* Reduce a math function inside a typed [calc()]: a result CSS types as the
   arguments' own type rebuilds this type's leaf, a [<number>] result stays a
   [Num] operand, and a call that does not reduce - or reduces to a unit this
   type cannot hold - keeps its spelling rather than shedding the unit. *)
let typed_math_fn : type a. (string -> float -> a option) -> math_fn -> a calc =
 fun of_unit fn ->
  match fold_math_result (math_fn_result fn) with
  | Some (United (v, unit)) -> (
      match of_unit unit v with Some leaf -> Val leaf | None -> Math_fn fn)
  | Some (Scalar v) -> Num v
  | None -> Math_fn fn

let rec math_arg_contains_var = function
  | Lit _ | Dim _ | Const _ -> false
  | Var_arg _ -> true
  | Op (l, _, r) -> math_arg_contains_var l || math_arg_contains_var r
  | Parens_arg inner -> math_arg_contains_var inner
  | Math_call fn -> math_fn_contains_var fn

and angle_arg_contains_var : angle_arg -> bool = function
  | Deg _ | Rad _ | Turn _ | Grad _ -> false
  | Numeric_arg arg -> math_arg_contains_var arg
  | Operation (l, _, r) -> angle_arg_contains_var l || angle_arg_contains_var r
  | Grouped inner -> angle_arg_contains_var inner

and math_fn_contains_var = function
  | Sin a | Cos a | Tan a -> angle_arg_contains_var a
  | Asin a | Acos a | Atan a | Sqrt a | Exp a | Sign_n a | Abs_n a ->
      math_arg_contains_var a
  | Atan2 (a, b)
  | Log (a, Some b)
  | Pow (a, b)
  | Round_n (_, a, b)
  | Mod_n (a, b)
  | Rem_n (a, b) ->
      math_arg_contains_var a || math_arg_contains_var b
  | Log (a, None) -> math_arg_contains_var a
  | Hypot args -> List.exists math_arg_contains_var args

let rec calc_contains_var : type a. a calc -> bool = function
  | Var _ -> true
  | Val _ -> false
  | Num _ | Math_const _ | Sibling_index | Sibling_count -> false
  | Math_fn fn -> math_fn_contains_var fn
  | Nested inner | Parens inner -> calc_contains_var inner
  | Expr (l, _, r) -> calc_contains_var l || calc_contains_var r

(* Drop authored grouping nodes while retaining the expression tree that gives
   them their meaning. The printer's precedence rules reconstruct parentheses
   exactly where serialization still needs them. This is an AST normalization,
   so a non-normalized value keeps every authored [Parens] node. *)
let rec normalize_calc_parens : type a. a calc -> a calc =
 fun calc ->
  match calc with
  | Parens inner -> normalize_calc_parens inner
  | Nested inner ->
      let inner' = normalize_calc_parens inner in
      if inner' == inner then calc else Nested inner'
  | Expr (left, op, right) ->
      let left' = normalize_calc_parens left in
      let right' = normalize_calc_parens right in
      if left' == left && right' == right then calc else Expr (left', op, right')
  | Val _ | Var _ | Num _ | Math_const _ | Math_fn _ | Sibling_index
  | Sibling_count ->
      calc

let pp_calc_contents : type a. a Pp.t -> a calc Pp.t =
 fun pp_value ctx calc ->
  let precedence = function Add | Sub -> 1 | Mul | Div -> 2 in
  (* Print a calc expression, tracking parent precedence and whether we're on
     the right side of a non-commutative operator (Sub/Div) *)
  let rec pp_calc_inner ~parent_prec ~right_of_noncommut ctx = function
    | Val v -> pp_value ctx v
    | Var v -> pp_var pp_value ctx v
    | Num n -> pp_calc_number ctx n
    | Math_const c -> pp_math_const ctx c
    | Math_fn fn -> pp_math_fn ctx fn
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

(* Six significant figures is the precision Cascade commits to in serialised
   output, and CSS Values 4 sec. 10.13 leaves the choice to the implementation.
   It applies to a coefficient Cascade computed itself - an irrational folded
   out of [pi] or [sqrt()], a conversion between absolute units - which carries
   digits that will never be printed. An authored coefficient is the author's
   own digits and keeps every one of them: at a [14px] font [.4285714em] is
   [6px] and [.428571em] is not.

   The budget buys a fractional tail, and six significant figures reach one only
   while the magnitude stays under [10^6]. Wider than that the only digits left
   to drop are integer ones the arithmetic got right: [1in] is exactly [96px]
   and [1pt] exactly [4/3px] (CSS Values 4 sec. 6.2), so an inch added to
   [999999999px] is [1000000095px], and [1000000000px] is 95px away from it. *)
let round_computed (f : float) : float =
  if Float.abs f < 1e6 then Pp.round_sig 6 f else f

(* Whether every digit of [f] already prints within the budget. *)
let fits_precision (f : float) : bool = Pp.round_sig 6 f = f

(* Fold [a / b] only when the float quotient round-trips through multiplication:
   exact divisions like [100/4 = 25] survive but [100/3 = 33.333...] does not,
   so the calc wrapper stays and CSS Values 4 sec. 10.13's precision requirement
   holds. *)
let exact_div (a : float) (b : float) : float option =
  if b = 0. then Option.none
  else
    let r = a /. b in
    (* Round-trip alone is too lax under IEEE 754 (33.333... * 3 = 100.0 due to
       multiplication rounding). Also require the quotient to fit the serialised
       precision, whatever its magnitude: an unfolded [calc()] keeps the digits
       the quotient would otherwise drop. *)
    if Float.is_finite r && r *. b = a && fits_precision r then Option.some r
    else Option.none

(* Scale the coefficient [v] of a typed leaf by [n], rebuilding the leaf in its
   own type. Multiplication folds whenever the product stays finite; division
   folds only on an exact quotient, unless [~exact:false] accepts the rounded
   one. *)
let scale_leaf ~exact (op : calc_op) n rebuild v =
  match op with
  | Mul ->
      let r = v *. n in
      if Float.is_finite r then Option.some (rebuild r) else Option.none
  | Div ->
      if exact then Option.map rebuild (exact_div v n)
      else if n = 0. then Option.none
      else
        let r = v /. n in
        if Float.is_finite r then Option.some (rebuild r) else Option.none
  | Add | Sub -> Option.none

(* Combine the coefficients of two same-unit typed leaves: [*] and [/] of two
   typed operands is not a typed value, so only [+] and [-] fold. *)
let combine_leaf (op : calc_op) x y =
  match op with
  | Add -> Option.some (x +. y)
  | Sub -> Option.some (x -. y)
  | Mul | Div -> Option.none

let numeric_leaf_value : type a. a calc -> float option = function
  | Num n -> Some n
  | Math_const c -> Some (math_const_value c)
  | _ -> None

let fold_zero_numeric_expr : type a.
    a calc -> calc_op -> a calc -> a calc option =
 fun l op r ->
  match (numeric_leaf_value l, numeric_leaf_value r) with
  | Some a, Some b -> (
      let value =
        match op with
        | Add -> Some (a +. b)
        | Sub -> Some (a -. b)
        | Mul -> Some (a *. b)
        | Div when b <> 0. -> Some (a /. b)
        | Div -> None
      in
      match value with Some 0. -> Some (Num 0.) | _ -> None)
  | _ -> None

(* CSS Values 4 sec. 10.10.1 value-independent multiplicative calc identities:
   these fold even across a kept [var()] ([var * 1 = var]). A zero sum term is
   deliberately not an identity here: the spec only combines sum children with
   identical units and warns that removing any other zero can change the calc's
   type. Zero-producing products take the type's canonical zero from [~zero],
   and [~is_zero] recognises a typed zero leaf ([0px], not just [0]). Operands
   are already evaluated; numeric op numeric is the caller's job. *)
let calc_identity : type a.
    zero:a calc ->
    is_zero:(a -> bool) ->
    a calc ->
    calc_op ->
    a calc ->
    a calc option =
 fun ~zero ~is_zero l op r ->
  match (l, op, r) with
  | _, Mul, Num 1. | _, Div, Num 1. -> Some l
  | Num 1., Mul, _ -> Some r
  | _, Mul, Num 0. | Num 0., Mul, _ -> Some zero
  (* A typed zero ([0px]) keeps its own spelling here; the optimizer's
     zero-length strip canonicalises it, so a non-optimised serialisation still
     reads [0px] rather than a bare [0]. *)
  | (Val v as z), Mul, _ when is_zero v -> Some z
  | _, Mul, (Val v as z) when is_zero v -> Some z
  | _ -> None

(* CSS Values 4 10.7 structural simplification of a typed calc AST. Folds [Expr
   (Num _, op, Num _)] subtrees and the value-independent identities
   ([calc_identity]). The per-type evaluators ([eval_length_calc] /
   [eval_lp_calc]) add the typed [Val] combinations ([1px + 2px], [2px * 3]) and
   pass their own [~zero] / [~is_zero] so a typed zero collapses too. *)
(* Context threaded through the calc simplifier. [var_is_single_valued n] reports
   whether [--n] has a single-component [@property] syntax, so [calc(var(--n))]
   substitutes one term and its redundant grouping may be dropped. The default
   knows nothing, so every such rewrite is a no-op. *)
type calc_ctx = { var_is_single_valued : string -> bool; budget : bool }

let default_calc_ctx =
  { var_is_single_valued = (fun _ -> false); budget = false }

(* A nested [calc()] or a parenthesised group around a single leaf is redundant
   grouping: drop it. A [var()] leaf is only safe to unwrap when the var is
   single-valued; otherwise its substitution could be a multi-term expression
   that needs the grouping (CSS Values 4 sec. 10.10). *)
let unwrap_grouping : type a.
    ctx:calc_ctx -> rewrap:(a calc -> a calc) -> a calc -> a calc =
 fun ~ctx ~rewrap reduced ->
  match reduced with
  | (Val _ | Num _ | Math_const _) as leaf -> leaf
  | Var v as leaf when ctx.var_is_single_valued v.name -> leaf
  | reduced -> rewrap reduced

let rec eval_calc_inner : type a. ctx:calc_ctx -> a calc -> a calc =
 fun ~ctx -> function
  | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf -> leaf
  | Math_const _ as leaf -> leaf
  | Math_fn fn when math_fn_contains_var fn -> Math_fn fn
  | Math_fn fn -> (
      (* This fold carries no type to rebuild a unit with, so a call CSS types
         as its arguments ([abs()], [hypot()]) keeps its spelling rather than
         collapsing to a coefficient the unit has fallen off. *)
      match fold_math_result (math_fn_result fn) with
      | Some (Scalar v) -> Num v
      | Some (United _) | None -> Math_fn fn)
  | Nested inner ->
      unwrap_grouping ~ctx
        ~rewrap:(fun r -> Nested r)
        (eval_calc_inner ~ctx inner)
  | Parens inner ->
      unwrap_grouping ~ctx
        ~rewrap:(fun r -> Parens r)
        (eval_calc_inner ~ctx inner)
  | Expr (l, op, r) -> (
      let l = eval_calc_inner ~ctx l in
      let r = eval_calc_inner ~ctx r in
      (* Match on the const-folded operands but keep [l] / [r] in the no-fold
         results, so an unreduced expression keeps the short [calc(.. pi ..)].
         Identity rules need a type-aware [Val] inspection (runtime subst?);
         per-type evaluators own them. *)
      let lc = calc_operand_value l in
      let rc = calc_operand_value r in
      match (lc, op, rc) with
      | Num a, Add, Num b -> calc_num (a +. b)
      | Num a, Sub, Num b -> calc_num (a -. b)
      | Num a, Mul, Num b -> calc_num (a *. b)
      | Num a, Div, Num b -> (
          match exact_div a b with
          | Some q -> calc_num q
          | None -> Expr (l, op, r))
      | _ -> (
          match
            calc_identity ~zero:(Num 0.) ~is_zero:(fun _ -> false) l op r
          with
          | Some folded -> folded
          | None -> (
              match fold_zero_numeric_expr lc op rc with
              | Some zero -> zero
              | None -> Expr (l, op, r))))

let eval_calc ?(ctx = default_calc_ctx) calc =
  normalize_calc_parens (eval_calc_inner ~ctx calc)

(* A subtree Cascade has to evaluate to a float of its own: a math constant or a
   math function. A fold that consumes one lands a computed coefficient in the
   typed leaf, so that leaf takes [round_computed]; a fold over authored
   operands alone keeps their digits. *)
let rec calc_is_computed : type a. a calc -> bool = function
  | Math_const _ | Math_fn _ -> true
  | Nested inner | Parens inner -> calc_is_computed inner
  | Expr (left, _, right) -> calc_is_computed left || calc_is_computed right
  | Num _ | Val _ | Var _ | Sibling_index | Sibling_count -> false

(* Spend the [round] budget on a typed value Cascade computed, once nothing more
   will fold it. A value the author wrote keeps its digits. *)
let settle_computed : type a. (a -> a) -> a calc * bool -> a calc =
 fun round (reduced, computed) ->
  match reduced with Val v when computed -> Val (round v) | reduced -> reduced

(* Fold one [Expr] over operands that are already reduced. [lf] / [rf] pair each
   operand with its own provenance flag; the result carries the flag of the
   typed value it produces, and an operand a kept [calc()] will print settles
   here because nothing further will fold it. *)
let fold_typed_expr : type a.
    budget:bool ->
    scale:(exact:bool -> calc_op -> a -> float -> a option) ->
    combine:(calc_op -> a -> a -> a option) ->
    round:(a -> a) ->
    is_zero:(a -> bool) ->
    zero:a calc ->
    computed:bool ->
    a calc * bool ->
    calc_op ->
    a calc * bool ->
    a calc * bool =
 fun ~budget ~scale ~combine ~round ~is_zero ~zero ~computed lf op rf ->
  let l = fst lf and r = fst rf in
  let lc = calc_operand_value l in
  let rc = calc_operand_value r in
  let keep () =
    (Expr (settle_computed round lf, op, settle_computed round rf), false)
  in
  let value v = (Val v, computed) in
  let zero_fold = fold_zero_numeric_expr lc op rc in
  match (lc, op, rc, zero_fold) with
  | Num a, Add, Num b, _ -> (calc_num (a +. b), false)
  | Num a, Sub, Num b, _ -> (calc_num (a -. b), false)
  | Num a, Mul, Num b, _ -> (calc_num (a *. b), false)
  | Num a, Div, Num b, _ -> (
      match exact_div a b with
      | Some q -> (calc_num q, false)
      (* A quotient the budget has to round is Cascade's own digits, the same
         six figures a number's fold spends; without the budget the author's
         quotient stays for the browser. *)
      | None when budget && b <> 0. && Float.is_finite (a /. b) ->
          (calc_num (round_computed (a /. b)), false)
      | None -> keep ())
  | _, _, _, Some folded -> (folded, false)
  | Val a, op, Val b, None -> (
      match combine op a b with Some v -> value v | None -> keep ())
  | Val _, Div, Num 0., None -> keep ()
  | Val a, ((Mul | Div) as op), Num n, None -> (
      (* An inexact quotient is Cascade's own digits and takes the budget the
         way a computed constant does; without it only the exact fold runs. *)
      let exact = match r with Math_const _ -> false | _ -> not budget in
      match scale ~exact op a n with
      | Some v -> if exact || computed then value v else (Val (round v), false)
      | None -> keep ())
  | Num n, Mul, Val a, None -> (
      match scale ~exact:true Mul a n with Some v -> value v | None -> keep ())
  (* CSS Values 4 sec. 10.10.1: [1 / (1 / x)] cancels the double inversion. *)
  | Num 1., Div, Parens (Expr (Num 1., Div, x)), None -> (x, false)
  | Num 1., Div, Expr (Num 1., Div, x), None -> (x, false)
  | _, _, _, None -> (
      (* An identity returns one operand verbatim, so the surviving value keeps
         that operand's provenance rather than the expression's. *)
      match calc_identity ~zero ~is_zero l op r with
      | Some folded -> (folded, snd lf || snd rf)
      | None -> keep ())

(* CSS Values 4 sec. 10.10.1 typed [calc()] reduction, shared by every
   dimensioned type: the [Expr] dispatch is identical, only the per-type leaves
   differ and are passed in ([math_fn], [scale], [combine], [round], [zero] /
   [is_zero] for the identities). A shape with no [scale] / [combine] returns
   [None] and the [calc()] is kept verbatim. *)
let eval_typed_calc : type a.
    math_fn:(math_fn -> a calc) ->
    scale:(exact:bool -> calc_op -> a -> float -> a option) ->
    combine:(calc_op -> a -> a -> a option) ->
    round:(a -> a) ->
    is_zero:(a -> bool) ->
    zero:a calc ->
    ctx:calc_ctx ->
    a calc ->
    a calc =
 fun ~math_fn ~scale ~combine ~round ~is_zero ~zero ~ctx calc ->
  (* [reduce] pairs the reduced tree with a flag: its root is a typed value
     Cascade computed rather than one the author wrote. The budget is spent once
     nothing more will fold that root - at the top, or where it is embedded in a
     [calc()] that stays. Rounding an intermediate product instead feeds the
     shortened coefficient back into the next fold and compounds the error:
     [calc(100px * sqrt(2) * sqrt(2))] is still [200px]. *)
  let rec reduce (calc : a calc) : a calc * bool =
    match calc with
    | (Num _ | Val _ | Var _ | Sibling_index | Sibling_count) as leaf ->
        (leaf, false)
    | Math_const _ as leaf -> (leaf, false)
    | Math_fn fn when math_fn_contains_var fn -> (Math_fn fn, false)
    | Math_fn fn -> (
        match math_fn fn with
        | Val _ as reduced -> (reduced, true)
        | reduced -> (reduced, false))
    | Nested inner -> wrap (fun r -> Nested r) inner
    | Parens inner -> wrap (fun r -> Parens r) inner
    | Expr (left, op, right) ->
        let computed = calc_is_computed left || calc_is_computed right in
        fold_typed_expr ~budget:ctx.budget ~scale ~combine ~round ~is_zero ~zero
          ~computed (reduce left) op (reduce right)
  and wrap rewrap inner =
    let reduced, computed = reduce inner in
    (unwrap_grouping ~ctx ~rewrap reduced, computed)
  in
  normalize_calc_parens (settle_computed round (reduce calc))

(* A dimension the author could not have written as a literal. CSS Values 4 sec.
   10.3 keeps a [calc()] valid wherever its range is exceeded and clamps at
   used-value time, so [width: calc(-10px)] is a declaration Chrome computes as
   [0px] while [width: -10px] is one it drops. Folding the call away on such a
   property has to stop at the sign. *)
let negative_length : length -> bool = function
  | Px f
  | Cm f
  | Mm f
  | Q f
  | In f
  | Pt f
  | Pc f
  | Rem f
  | Em f
  | Ex f
  | Cap f
  | Ic f
  | Ric f
  | Rlh f
  | Pct f
  | Vw f
  | Vh f
  | Vmin f
  | Vmax f
  | Vi f
  | Vb f
  | Dvh f
  | Dvw f
  | Dvmin f
  | Dvmax f
  | Lvh f
  | Lvw f
  | Lvmin f
  | Lvmax f
  | Svh f
  | Svw f
  | Svmin f
  | Svmax f
  | Cqw f
  | Cqh f
  | Cqi f
  | Cqb f
  | Cqmin f
  | Cqmax f
  | Ch f
  | Lh f
  | Dimension { value = f; _ } ->
      f < 0.
  | _ -> false

let pp_calc_with : type a.
    ?unwrap_num:bool ->
    ?unwrap:(a -> bool) ->
    ?pp_unwrapped:a Pp.t ->
    a Pp.t ->
    a calc Pp.t =
 fun ?(unwrap_num = true) ?(unwrap = fun _ -> true) ?pp_unwrapped pp_value ctx
     calc ->
  match calc with
  (* CSS Values 4 sec. 10.10: a [var()] inside [calc()] is a runtime
     substitution boundary - the substituted tokens go through calc's typed
     grammar, not the surrounding property's grammar. Unwrapping
     [calc(var(--x))] to bare [var(--x)] would change which substitution shape
     is valid. *)
  (* CSS Values 4 sec. 10.3 keeps a [calc()] valid where its range is exceeded
     and clamps at used-value time, so [width: calc(-10px)] is a declaration
     Chrome computes as [0px] and [width: -10px] one it drops. Which properties
     those are is not knowable here, so the wrapper stays on a value that reads
     back differently without it; [Declaration.normalize] unwraps the rest,
     where the property is in hand. *)
  (* A leaf that comes out of the call is no longer an operand, so it is written
     the way a leaf on its own is written. [pp_value] is the operand printer,
     which a caller whose two spellings differ passes [pp_unwrapped] beside: a
     duration operand keeps [ms] so two unequal calls do not print alike, while
     [120ms] on its own is [.12s], and printing the operand spelling here left
     an emission the next reader shortened. *)
  | Val v when Pp.minified ctx && unwrap v ->
      Option.value pp_unwrapped ~default:pp_value ctx v
  | Num n when Pp.minified ctx && unwrap_num -> Pp.float ctx n
  | _ ->
      let ctx = { ctx with in_calc = true } in
      Pp.call "calc" (pp_calc_contents pp_value) ctx calc

let pp_calc ?unwrap_num ?unwrap ?pp_unwrapped pp_value ctx calc =
  pp_calc_with ?unwrap_num ?unwrap ?pp_unwrapped pp_value ctx calc

(* Small helpers *)

let pp_unit ?always:_ ctx f suffix =
  (* Pure serialiser: the unit is always kept, even on a zero. Dropping it
     ([0px] -> [0]) changes the node ([Px 0.] -> [Zero]) and is gated to
     top-level (a calc / function operand keeps its unit), so it is an AST
     rewrite in the optimize pass, not a printer choice. *)
  (* CSSOM serialization (CSS Values 4 6.7.2) drops a leading zero on fractional
     values ([.25rem], not [0.25rem]) in both modes. The coefficient itself
     prints in full, like [Pp.pct]: rounding it changes the value, so the
     6-significant-figure budget ([round_computed]) belongs to the fold that
     computes a coefficient, not to the printer that serialises one. *)
  if Float.is_nan f then Pp.nan_value ctx suffix
  else (
    Pp.string ctx (Pp.string_of_float ~drop_leading_zero:true f);
    Pp.string ctx suffix)

(** What a calc expression over static operands reduces to, keeping the unit CSS
    Values 4 sec. 10.5 [hypot()] and sec. 10.6 [abs()] carry over from their
    arguments. Returns [None] on a variable or a typed leaf. *)
let rec numeric_calc_result : type a. a calc -> math_result option = function
  | Num f -> Some (Scalar f)
  | Math_const c -> Some (Scalar (math_const_value c))
  | Math_fn fn -> math_fn_result fn
  | Sibling_index -> None
  | Sibling_count -> None
  | Val _ -> None (* Can't evaluate typed values *)
  | Var _ -> None (* Can't evaluate variables *)
  | Nested inner -> numeric_calc_result inner
  | Parens inner -> numeric_calc_result inner
  | Expr (left, op, right) -> (
      match (numeric_calc_result left, numeric_calc_result right) with
      | Some l, Some r -> combine_math_result op l r
      | _ -> None)

(** Try to evaluate a calc expression containing only numbers to a float.
    Returns None if the expression contains variables or non-numeric values. *)
let eval_numeric_calc : type a. a calc -> float option =
 fun calc ->
  (* A result carrying a unit is not a [<number>]: sec. 10.6 gives [abs(-1px)]
     the type of its argument, so folding it to a bare coefficient here would
     hand a length to a slot that asked for a number. *)
  match numeric_calc_result calc with
  | Some (Scalar v) -> Some v
  | Some (United _) | None -> None

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
      Var
        {
          name = v.name;
          fallback;
          default;
          layer = v.layer;
          meta = v.meta;
          runtime = v.runtime;
        }
  | Num f -> Num f
  | Sibling_index -> Sibling_index
  | Sibling_count -> Sibling_count
  | Math_const c -> Math_const c
  | Math_fn fn -> Math_fn fn
  | Nested inner -> Nested (map_calc f inner)
  | Parens inner -> Parens (map_calc f inner)
  | Expr (l, op, r) -> Expr (map_calc f l, op, map_calc f r)

(** Partial sibling of {!map_calc}: [Val] leaves go through [f] and the whole
    tree fails when one of them does. A [Var] / [Sibling_index] /
    [Sibling_count] node carries no leaf to retype, so it fails too. *)
let rec map_calc_opt : type a b. (a -> b option) -> a calc -> b calc option =
 fun f calc ->
  match calc with
  | Val v -> Option.map (fun v -> Val v) (f v)
  | Num n -> Some (Num n)
  | Math_const c -> Some (Math_const c)
  | Math_fn fn -> Some (Math_fn fn)
  | Var _ | Sibling_index | Sibling_count -> None
  | Nested inner -> Option.map (fun i -> Nested i) (map_calc_opt f inner)
  | Parens inner -> Option.map (fun i -> Parens i) (map_calc_opt f inner)
  | Expr (left, op, right) -> (
      match (map_calc_opt f left, map_calc_opt f right) with
      | Some left, Some right -> Some (Expr (left, op, right))
      | _ -> None)

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
  let length : length -> length calc = function
    | Var v -> Var v
    | len -> Val len

  let var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc =
   fun ?default ?fallback name -> Var (var_ref ?default ?fallback name)

  let float f : 'a calc = Num f
  let infinity : 'a calc = Num infinity
  let px n = Val (Px n : length)
  let rem f = Val (Rem f : length)
  let em f = Val (Em f : length)
  let pct f : length calc = Val (Pct f : length)

  (* Wrap an expression in an explicit nested calc() *)
  let nested inner = Nested inner

  (* Wrap an expression in parentheses only *)
  let parens inner = Parens inner
end
