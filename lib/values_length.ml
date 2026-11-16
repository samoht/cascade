(** Lengths and length-percentages: units, [calc()] reduction and printing. *)

open Values_intf
open Values_var
open Values_calc

(* Recognise literal zeros for top-level canonicalisation and typed products.
   This does not make them interchangeable in a calc sum: CSS Values 4 sec.
   10.10.1 only combines identical units and requires other zero terms to
   stay. *)
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

let length_of_default_string value : length option =
  match
    let t = Cursor.of_string value in
    let value, unit = Cursor.number_with_unit t in
    Cursor.ws t;
    Cursor.expect_eof t;
    match unit with
    | Option.Some unit ->
        Option.Some (length_from_calc_unit (String.lowercase_ascii unit) value)
    | Option.None when value = 0. -> Option.Some Zero
    | Option.None -> Option.None
  with
  | value -> value
  | exception Cursor.Parse_error _ -> None

let fallback_of_var_resolution _parse_default : 'a fallback -> 'a option =
  function
  | Fallback value -> Option.Some value
  | Var_fallback _ | None | Empty | Empty2 | Syntax_fallback _ -> Option.None

(* Var resolution at print time covers only [inline] mode, which reads the
   already-resolved [v.default] from the AST. Theme resolution is a transform
   that runs earlier, so the printer never consults a resolver. *)
let value_of_var_resolution ctx parse_default (v : 'a var) : 'a option =
  if ctx.Pp.inline then
    Option.fold
      ~none:(fallback_of_var_resolution parse_default v.fallback)
      ~some:(fun value -> Option.Some value)
      v.default
  else Option.None

let length_of_var_resolution ctx (v : length var) =
  value_of_var_resolution ctx length_of_default_string v

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
  | (Val _ | Num _ | Math_const _ | Sibling_index | Sibling_count | Math_fn _)
    as leaf ->
      leaf

(* CSS Values 4 sec. 6.2: the absolute lengths share px as a canonical unit. *)
let absolute_unit_px_ratio = function
  | "px" -> Some 1.
  | "in" -> Some 96.
  | "cm" -> Some (96. /. 2.54)
  | "mm" -> Some (96. /. 2.54 /. 10.)
  | "q" -> Some (96. /. 2.54 /. 40.)
  | "pt" -> Some (96. /. 72.)
  | "pc" -> Some (96. /. 6.)
  | _ -> None

(* CSS Values 4 sec. 10.10.1: same-unit add/sub of two typed lengths reduces to
   a single length. Mixed cases reduce when both operands are absolute units
   (px-compatible per sec. 6.2) by combining in the canonical px form; relative
   units (em/rem/vw/...) still require cascade context and stay unfolded. *)
let length_combine op v1 v2 =
  let combine a b =
    match op with Add -> a +. b | Sub -> a -. b | Mul | Div -> nan
  in
  match (op, calc_length_unit v1, calc_length_unit v2) with
  | (Add | Sub), Some (unit1, a), Some (unit2, b) when String.equal unit1 unit2
    ->
      Some (length_from_calc_unit unit1 (combine a b))
  | (Add | Sub), Some (unit1, a), Some (unit2, b) -> (
      match (absolute_unit_px_ratio unit1, absolute_unit_px_ratio unit2) with
      | Some r1, Some r2 ->
          (* The px form is Cascade's own arithmetic, not the authored one. *)
          Some
            (length_from_calc_unit "px"
               (round_computed (combine (a *. r1) (b *. r2))))
      | _ -> None)
  | _ -> None

(* CSS Values 4 sec. 10.10.1: a unitless factor scales a typed length, unit
   unchanged. Division folds only when [exact_div] is exact, else the [calc()]
   is kept to avoid precision loss. Exception: an irrational divisor (a math
   constant [pi] / [e] / ...) can never be exact, and keeping [calc()] preserves
   no more precision than the browser computes, so fold to the rounded value. *)
let length_scale ?(exact = true) op v n =
  if not (Float.is_finite n) then Option.none
  else
    match (op, calc_length_unit v) with
    | Mul, Some (unit, value) ->
        let r = value *. n in
        if Float.is_finite r then Option.some (length_from_calc_unit unit r)
        else Option.none
    | Div, Some (unit, value) ->
        if exact then
          Option.map (length_from_calc_unit unit) (exact_div value n)
        else if n = 0. then Option.none
        else
          let r = value /. n in
          if Float.is_finite r then Option.some (length_from_calc_unit unit r)
          else Option.none
    | _ -> Option.none

(* CSS Values 4 sec. 10.10: identity-rule simplifications around a runtime
   substitution would change the substituted-grammar context. *)
(* A [-webkit-] / [-moz-] intrinsic sizing keyword, the legacy fallback an
   author pairs with the unprefixed form ([width:-webkit-max-content;
   width:max-content]). *)
let length_is_vendor_prefixed_sizing : length -> bool = function
  | Webkit_max_content | Webkit_min_content | Webkit_fit_content
  | Moz_max_content | Moz_min_content | Moz_fit_content ->
      true
  | _ -> false

let length_percentage_is_vendor_prefixed : length_percentage -> bool = function
  | Length l -> length_is_vendor_prefixed_sizing l
  | _ -> false

let rec length_has_runtime_subst : length -> bool = function
  | Var _ | Env _ | Attr _ | Anchor _ | Anchor_size _ -> true
  | Calc c -> length_calc_has_runtime_subst c
  | _ -> false

and length_calc_has_runtime_subst : length calc -> bool = function
  | Var _ -> true
  | Val v -> length_has_runtime_subst v
  | Num _ | Math_const _ | Sibling_index | Sibling_count -> false
  | Math_fn fn -> math_fn_contains_var fn
  | Nested inner | Parens inner -> length_calc_has_runtime_subst inner
  | Expr (l, _, r) ->
      length_calc_has_runtime_subst l || length_calc_has_runtime_subst r

(* Reduce a computed coefficient to the serialised precision, returning the leaf
   unchanged when every digit already prints. A [<percentage>] takes the same
   budget: [Pp.pct] prints its coefficient in full, so a computed one has to be
   reduced here or the digits the arithmetic left print with it. *)
let length_round (l : length) : length =
  match l with
  | Pct f ->
      let rounded = round_computed f in
      if rounded = f then l else Pct rounded
  | _ -> (
      match calc_length_unit l with
      | Some (unit, value) ->
          let rounded = round_computed value in
          if rounded = value then l else length_from_calc_unit unit rounded
      | None -> l)

(* CSS Values 4 sec. 10.6: [abs()] and [hypot()] preserve the input's type, so
   [hypot(<length>, <length>)] returns a [<length>] and the [Val] is rebuilt
   from the unit [math_fn_result] carries out. *)
let length_calc_math_fn (fn : math_fn) : length calc =
  match (fn, fold_united_nan (math_fn_result fn)) with
  (* [sin()] / [cos()] reduce to a coefficient no shorter than the call, and it
     is not a [<length>] either way, so the call stays. *)
  | (Sin _ | Cos _), _ -> Math_fn fn
  | _, Some (United (v, unit)) -> Val (length_from_calc_unit unit v)
  | _, Some (Scalar v) -> Num v
  | _, None -> Math_fn fn

let eval_length_calc ?(ctx = default_calc_ctx) (c : length calc) : length calc =
  eval_typed_calc ~math_fn:length_calc_math_fn
    ~scale:(fun ~exact op v n -> length_scale ~exact op v n)
    ~combine:length_combine ~round:length_round ~is_zero:length_is_zero
    ~zero:(Val Zero) ~ctx c

type length_unit =
  | Px
  | Cm
  | Mm
  | Q
  | In
  | Pt
  | Pc
  | Rem
  | Em
  | Ex
  | Cap
  | Ic
  | Ric
  | Rlh
  | Ch
  | Lh
  | Pct
  | Vw
  | Vh
  | Vmin
  | Vmax
  | Vi
  | Vb
  | Dvh
  | Dvw
  | Dvmin
  | Dvmax
  | Lvh
  | Lvw
  | Lvmin
  | Lvmax
  | Svh
  | Svw
  | Svmin
  | Svmax
  | Cqw
  | Cqh
  | Cqi
  | Cqb
  | Cqmin
  | Cqmax

let length_unit_is_pct = function Pct -> true | _ -> false

let unit_of_string = function
  | "px" -> Some Px
  | "cm" -> Some Cm
  | "mm" -> Some Mm
  | "q" -> Some Q
  | "in" -> Some In
  | "pt" -> Some Pt
  | "pc" -> Some Pc
  | "rem" -> Some Rem
  | "em" -> Some Em
  | "ex" -> Some Ex
  | "cap" -> Some Cap
  | "ic" -> Some Ic
  | "ric" -> Some Ric
  | "rlh" -> Some Rlh
  | "ch" -> Some Ch
  | "lh" -> Some Lh
  | "%" -> Some Pct
  | "vw" -> Some Vw
  | "vh" -> Some Vh
  | "vmin" -> Some Vmin
  | "vmax" -> Some Vmax
  | "vi" -> Some Vi
  | "vb" -> Some Vb
  | "dvh" -> Some Dvh
  | "dvw" -> Some Dvw
  | "dvmin" -> Some Dvmin
  | "dvmax" -> Some Dvmax
  | "lvh" -> Some Lvh
  | "lvw" -> Some Lvw
  | "lvmin" -> Some Lvmin
  | "lvmax" -> Some Lvmax
  | "svh" -> Some Svh
  | "svw" -> Some Svw
  | "svmin" -> Some Svmin
  | "svmax" -> Some Svmax
  | "cqw" -> Some Cqw
  | "cqh" -> Some Cqh
  | "cqi" -> Some Cqi
  | "cqb" -> Some Cqb
  | "cqmin" -> Some Cqmin
  | "cqmax" -> Some Cqmax
  | _ -> None

let is_length_unit unit =
  Option.is_some (unit_of_string (String.lowercase_ascii unit))

let unit_of_length = function
  | Zero -> Some (Px, 0.)
  | Px n -> Some (Px, n)
  | Cm n -> Some (Cm, n)
  | Mm n -> Some (Mm, n)
  | Q n -> Some (Q, n)
  | In n -> Some (In, n)
  | Pt n -> Some (Pt, n)
  | Pc n -> Some (Pc, n)
  | Rem n -> Some (Rem, n)
  | Em n -> Some (Em, n)
  | Ex n -> Some (Ex, n)
  | Cap n -> Some (Cap, n)
  | Ic n -> Some (Ic, n)
  | Ric n -> Some (Ric, n)
  | Rlh n -> Some (Rlh, n)
  | Ch n -> Some (Ch, n)
  | Lh n -> Some (Lh, n)
  | Pct n -> Some (Pct, n)
  | Vw n -> Some (Vw, n)
  | Vh n -> Some (Vh, n)
  | Vmin n -> Some (Vmin, n)
  | Vmax n -> Some (Vmax, n)
  | Vi n -> Some (Vi, n)
  | Vb n -> Some (Vb, n)
  | Dvh n -> Some (Dvh, n)
  | Dvw n -> Some (Dvw, n)
  | Dvmin n -> Some (Dvmin, n)
  | Dvmax n -> Some (Dvmax, n)
  | Lvh n -> Some (Lvh, n)
  | Lvw n -> Some (Lvw, n)
  | Lvmin n -> Some (Lvmin, n)
  | Lvmax n -> Some (Lvmax, n)
  | Svh n -> Some (Svh, n)
  | Svw n -> Some (Svw, n)
  | Svmin n -> Some (Svmin, n)
  | Svmax n -> Some (Svmax, n)
  | Cqw n -> Some (Cqw, n)
  | Cqh n -> Some (Cqh, n)
  | Cqi n -> Some (Cqi, n)
  | Cqb n -> Some (Cqb, n)
  | Cqmin n -> Some (Cqmin, n)
  | Cqmax n -> Some (Cqmax, n)
  | Dimension { value; unit; _ } -> (
      match unit_of_string (String.lowercase_ascii unit) with
      | Some unit -> Some (unit, value)
      | None -> None)
  | _ -> None

let length_of_unit unit n =
  match unit with
  | Px -> if n = 0. then Zero else Px n
  | Cm -> Cm n
  | Mm -> Mm n
  | Q -> Q n
  | In -> In n
  | Pt -> Pt n
  | Pc -> Pc n
  | Rem -> Rem n
  | Em -> Em n
  | Ex -> Ex n
  | Cap -> Cap n
  | Ic -> Ic n
  | Ric -> Ric n
  | Rlh -> Rlh n
  | Ch -> Ch n
  | Lh -> Lh n
  | Pct -> Pct n
  | Vw -> Vw n
  | Vh -> Vh n
  | Vmin -> Vmin n
  | Vmax -> Vmax n
  | Vi -> Vi n
  | Vb -> Vb n
  | Dvh -> Dvh n
  | Dvw -> Dvw n
  | Dvmin -> Dvmin n
  | Dvmax -> Dvmax n
  | Lvh -> Lvh n
  | Lvw -> Lvw n
  | Lvmin -> Lvmin n
  | Lvmax -> Lvmax n
  | Svh -> Svh n
  | Svw -> Svw n
  | Svmin -> Svmin n
  | Svmax -> Svmax n
  | Cqw -> Cqw n
  | Cqh -> Cqh n
  | Cqi -> Cqi n
  | Cqb -> Cqb n
  | Cqmin -> Cqmin n
  | Cqmax -> Cqmax n

(* A zero [px] term inside calc() must retain its unit. Only a completed
   top-level [<length>] can use the shorter unitless-zero representation. *)
let length_of_calc_unit (unit : length_unit) n : length =
  match unit with Px -> Px n | _ -> length_of_unit unit n

(* CSS Syntax 3 (ED) sec. 4.3.12 builds a number's value from its sign, digits,
   fraction and exponent, so [1.0px], [+1px], [01px] and [1e3px] name lengths
   the constructors above already hold. The reader keeps such a spelling in a
   [Dimension] for the unminified round-trip, and the printer drops it under
   [--minify]: two spellings then reach one minified text through two nodes,
   which anything keyed on the node reads as two values. Fold back to the
   constructor once the round-trip no longer needs the spelling. The minified
   printer lowercases the unit, as the constructors do, so the author's case
   moves no byte here either; a zero is left to [strip_zero_length], whose unit
   strip is a type change this is not. *)
let canonical_dimension (l : length) : length =
  match l with
  | Dimension { value; unit; _ } when value <> 0. -> (
      match unit_of_string (String.lowercase_ascii unit) with
      | Some unit -> length_of_unit unit value
      | None -> l)
  | _ -> l

type linear_term = {
  unit : length_unit;
  value : float;
  first_pos : int;
  count : int;
}

(* CSS Values 4 sec. 10.13 sorts a sum's children before serialising them, so
   the order carries no meaning and the shortest spelling wins. A positive term
   leads: [b - a] against [-a + b] spends one byte fewer. Ties keep the authored
   order. *)
let linear_term_priority ~first_pos term =
  match (term.value > 0., length_unit_is_pct term.unit) with
  | true, true -> 0
  | true, false when term.first_pos = first_pos && term.count = 1 -> 1
  | true, false when term.count = 1 -> 2
  | true, false -> 3
  | false, false -> 4
  | false, true -> 5

let compare_linear_term ~first_pos a b =
  let c =
    compare
      (linear_term_priority ~first_pos a)
      (linear_term_priority ~first_pos b)
  in
  if c <> 0 then c else compare a.first_pos b.first_pos

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
  (* CSS Values 4 sec. 10.10.1 combines identical units, but explicitly keeps
     zero-valued sum children otherwise: their unit contributes to the calc's
     type. The table has already combined each identical-unit set, so retain
     every resulting term, including zero. *)
  let terms = Hashtbl.to_seq_values table |> List.of_seq in
  let first_pos =
    List.fold_left (fun acc term -> min acc term.first_pos) max_int terms
  in
  List.sort (compare_linear_term ~first_pos) terms

let linear_calc_op n = if n < 0. then (Sub, -.n) else (Add, n)

let linear_terms_with unit_of_value calc =
  let scale factor terms =
    List.map (fun (unit, n) -> (unit, factor *. n)) terms
  in
  let rec aux = function
    | Val v -> Option.map (fun term -> [ term ]) (unit_of_value v)
    (* A unitless zero is still a [<number>] inside calc(), not a typed zero. *)
    | Num _ | Math_const _ | Var _ | Sibling_index | Sibling_count | Math_fn _
      ->
        None
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
    | Expr (left, Div, Num n) -> (
        match exact_div 1. n with
        | Some inv -> Option.map (scale inv) (aux left)
        | None -> None)
    | Expr _ -> None
  in
  aux calc

let linear_length_terms calc = linear_terms_with unit_of_length calc

let linear_length_calc calc =
  match linear_length_terms calc with
  | None -> calc
  | Some terms -> (
      let terms = ordered_linear_terms terms in
      match terms with
      | [] -> Val Zero
      | { unit; value = n; _ } :: rest ->
          List.fold_left
            (fun acc { unit; value = n; _ } ->
              let op, n = linear_calc_op n in
              Expr (acc, op, Val (length_of_calc_unit unit n)))
            (Val (length_of_calc_unit unit n))
            rest)

let lp_of_default_string value : length_percentage option =
  match
    let t = Cursor.of_string value in
    let value, unit = Cursor.number_with_unit t in
    Cursor.ws t;
    Cursor.expect_eof t;
    match unit with
    | Option.Some "%" -> Option.Some (Pct value : length_percentage)
    | Option.Some unit ->
        Option.Some
          (Length (length_from_calc_unit (String.lowercase_ascii unit) value)
            : length_percentage)
    | Option.None when value = 0. ->
        Option.Some (Length Zero : length_percentage)
    | Option.None -> Option.None
  with
  | value -> value
  | exception Cursor.Parse_error _ -> Option.None

let lp_of_var_resolution ctx (v : length_percentage var) =
  value_of_var_resolution ctx lp_of_default_string v

let rec resolve_lp_calc_vars ctx :
    length_percentage calc -> length_percentage calc = function
  | Var v -> (
      match lp_of_var_resolution ctx v with
      | Option.Some value -> Val value
      | Option.None -> Var v)
  | Nested inner -> Nested (resolve_lp_calc_vars ctx inner)
  | Parens inner -> Parens (resolve_lp_calc_vars ctx inner)
  | Expr (left, op, right) ->
      Expr (resolve_lp_calc_vars ctx left, op, resolve_lp_calc_vars ctx right)
  | (Val _ | Num _ | Math_const _ | Sibling_index | Sibling_count | Math_fn _)
    as leaf ->
      leaf

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

let lp_scale ?(exact = true) op (v : length_percentage) n :
    length_percentage option =
  match (op, v) with
  | (Mul | Div), Length a -> (
      match length_scale ~exact op a n with
      | Some lv -> Some (Length lv)
      | None -> None)
  | Mul, Pct a -> Some (Pct (a *. n) : length_percentage)
  | Div, Pct a ->
      if exact then
        Option.map (fun r -> (Pct r : length_percentage)) (exact_div a n)
      else if n = 0. then None
      else Some (Pct (a /. n) : length_percentage)
  | _ -> None

let lp_is_zero : length_percentage -> bool = function
  | Length l -> length_is_zero l
  | Pct f -> f = 0.
  | _ -> false

let lp_round : length_percentage -> length_percentage = function
  | Length l -> Length (length_round l)
  | lp -> lp

(* Same unit-preserving rule as [length_calc_math_fn], plus the percentage shape
   [abs(-5%)] keeps. *)
let lp_calc_math_fn (fn : math_fn) : length_percentage calc =
  match fold_united_nan (math_fn_result fn) with
  | Some (United (v, "%")) -> Val (Pct v : length_percentage)
  | _ -> (
      match length_calc_math_fn fn with
      | Val l -> Val (Length l : length_percentage)
      | Num n -> Num n
      | _ -> Math_fn fn)

let eval_lp_calc ?(ctx = default_calc_ctx) (c : length_percentage calc) :
    length_percentage calc =
  eval_typed_calc ~math_fn:lp_calc_math_fn
    ~scale:(fun ~exact op v n -> lp_scale ~exact op v n)
    ~combine:lp_combine ~round:lp_round ~is_zero:lp_is_zero
    ~zero:(Val (Length Zero)) ~ctx c

let unit_of_lp : length_percentage -> (length_unit * float) option = function
  | Pct n -> Some (Pct, n)
  | Length l -> unit_of_length l
  | Env _ | Var _ | Calc _ | Invalid _ -> None

let lp_of_unit unit n : length_percentage =
  match unit with Pct -> Pct n | unit -> Length (length_of_calc_unit unit n)

let linear_lp_terms calc = linear_terms_with unit_of_lp calc

let linear_lp_calc calc =
  match linear_lp_terms calc with
  | None -> calc
  | Some terms -> (
      let terms = ordered_linear_terms terms in
      match terms with
      | [] -> Val (Length Zero)
      | { unit; value = n; _ } :: rest ->
          List.fold_left
            (fun acc { unit; value = n; _ } ->
              let op, n = linear_calc_op n in
              Expr (acc, op, Val (lp_of_unit unit n)))
            (Val (lp_of_unit unit n))
            rest)

(* One rounding rule, written once: the strategy and its tie-break are sec.
   10.7.3's whatever the argument's type, and three copies of it is how a tie
   came to be broken two different ways. *)
let round_length_step = round_to_step

(* Typed math-call printer: emit [name(arg1,arg2,...)] from a typed list of
   length values, deferring to [pp_length] for each component (so nested
   [calc()] / [var()] / [min()] / [clamp()] argument shapes Just Work). *)
let pp_typed_math_call ctx name pp_arg args =
  Pp.string ctx name;
  Pp.char ctx '(';
  (* A calc-family math function's arguments are a typed-boundary calc context:
     keep the unit on a zero so [clamp(0px, 0em, 0vh)] stays unit-tagged rather
     than collapsing to the foldable-looking [clamp(0, 0, 0)] (which would lose
     round-trip stability). *)
  let ctx = { ctx with Pp.in_calc = true } in
  let sep ctx () =
    Pp.char ctx ',';
    if not (Pp.minified ctx) then Pp.char ctx ' '
  in
  Pp.list ~sep pp_arg ctx args;
  Pp.char ctx ')'

(* CSS Values 4 sec. 10.2 [min()] / [max()] reduce when every argument is a
   plain dimension of the same unit (so [min(1px, 2px)] -> [1px], [max(1em,
   .5em, 2em)] -> [2em]). Mixed units or any non-literal argument (variables,
   calc(), nested math) bail out and keep the function call. *)
let length_simple_dimension : length -> (float * length) option = function
  | Zero -> Some (0., Zero)
  | Px f -> Some (f, Px 0.)
  | Cm f -> Some (f, Cm 0.)
  | Mm f -> Some (f, Mm 0.)
  | Q f -> Some (f, Q 0.)
  | In f -> Some (f, In 0.)
  | Pt f -> Some (f, Pt 0.)
  | Pc f -> Some (f, Pc 0.)
  | Em f -> Some (f, Em 0.)
  | Rem f -> Some (f, Rem 0.)
  | Ex f -> Some (f, Ex 0.)
  | Cap f -> Some (f, Cap 0.)
  | Ic f -> Some (f, Ic 0.)
  | Ric f -> Some (f, Ric 0.)
  | Rlh f -> Some (f, Rlh 0.)
  | Vw f -> Some (f, Vw 0.)
  | Vh f -> Some (f, Vh 0.)
  | Vmin f -> Some (f, Vmin 0.)
  | Vmax f -> Some (f, Vmax 0.)
  | Vi f -> Some (f, Vi 0.)
  | Vb f -> Some (f, Vb 0.)
  | Ch f -> Some (f, Ch 0.)
  | Lh f -> Some (f, Lh 0.)
  | Pct f -> Some (f, Pct 0.)
  | _ -> None

let with_length_value l v : length =
  match l with
  | Zero | Px _ -> Px v
  | Cm _ -> Cm v
  | Mm _ -> Mm v
  | Q _ -> Q v
  | In _ -> In v
  | Pt _ -> Pt v
  | Pc _ -> Pc v
  | Em _ -> Em v
  | Rem _ -> Rem v
  | Ex _ -> Ex v
  | Cap _ -> Cap v
  | Ic _ -> Ic v
  | Ric _ -> Ric v
  | Rlh _ -> Rlh v
  | Vw _ -> Vw v
  | Vh _ -> Vh v
  | Vmin _ -> Vmin v
  | Vmax _ -> Vmax v
  | Vi _ -> Vi v
  | Vb _ -> Vb v
  | Ch _ -> Ch v
  | Lh _ -> Lh v
  | Pct _ -> Pct v
  | _ -> Px v

let rec reduce_length_min_max : length -> length = function
  | Min xs as orig -> (
      let xs = List.map reduce_length_min_max xs in
      match reduce_typed_min_max_simple xs Float.min with
      | Some v -> v
      | None ->
          if xs = match orig with Min ys -> ys | _ -> [] then orig else Min xs)
  | Max xs as orig -> (
      let xs = List.map reduce_length_min_max xs in
      match reduce_typed_min_max_simple xs Float.max with
      | Some v -> v
      | None ->
          if xs = match orig with Max ys -> ys | _ -> [] then orig else Max xs)
  | other -> other

and reduce_typed_min_max_simple (xs : length list) reduce : length option =
  let pairs = List.map length_simple_dimension xs in
  if List.exists Option.is_none pairs then None
  else
    let pairs = List.filter_map (fun x -> x) pairs in
    match pairs with
    | [] -> None
    | (_, sample) :: _ when List.for_all (fun (_, s) -> s = sample) pairs ->
        let values = List.map fst pairs in
        let reduced = List.fold_left reduce (List.hd values) (List.tl values) in
        Some (with_length_value sample reduced)
    | _ -> None

let try_reduce_typed_min_max xs reduce =
  let xs = List.map reduce_length_min_max xs in
  reduce_typed_min_max_simple xs reduce

let px_values values =
  List.fold_right
    (fun (value : length) acc ->
      match (value, acc) with
      | Px f, Some values -> Some (f :: values)
      | _ -> None)
    values (Some [])

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
  | Ric f -> pp_unit_fn f "ric"
  | Rlh f -> pp_unit_fn f "rlh"
  (* CSS Values 4 sec. 6: the unit-drop on a zero is for [<length>] only; [Pct
     0.] is a [<percentage>] zero and must keep [%] - dropping it would change
     the value's type from percentage to length. *)
  | Pct f -> Pp.pct ctx f
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
      (* CSS Values 4 sec. 6.7.2 serialises a dimension's unit in lowercase, and
         the unit constructors print that way, so a dimension naming one of them
         has to as well or the same width reads back two ways. A unit no
         constructor covers is a future or [calc()]-only dimension token this
         arm alone writes, so nothing contradicts the author's case there. The
         unminified branch round-trips what the author wrote either way. *)
      if ctx.minify then
        pp_unit_fn value
          (if is_length_unit unit then String.lowercase_ascii unit else unit)
      else (
        Pp.string ctx repr;
        Pp.string ctx unit)
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
  | Webkit_max_content -> Pp.string ctx "-webkit-max-content"
  | Webkit_min_content -> Pp.string ctx "-webkit-min-content"
  | Webkit_fit_content -> Pp.string ctx "-webkit-fit-content"
  | Moz_max_content -> Pp.string ctx "-moz-max-content"
  | Moz_min_content -> Pp.string ctx "-moz-min-content"
  | Moz_fit_content -> Pp.string ctx "-moz-fit-content"
  | From_font -> Pp.string ctx "from-font"
  | Hairline -> Pp.string ctx "hairline"
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
  | Stretch -> Pp.string ctx "stretch"
  | Clamp (mn, v, mx) ->
      pp_typed_math_call ctx "clamp" (pp_length_math_arg ~always) [ mn; v; mx ]
  | Min xs -> pp_typed_math_call ctx "min" (pp_length_math_arg ~always) xs
  | Max xs -> pp_typed_math_call ctx "max" (pp_length_math_arg ~always) xs
  | Minmax (mn, mx) ->
      pp_typed_math_call ctx "minmax" (pp_length_math_arg ~always) [ mn; mx ]
  | Round (strategy, value, step) ->
      pp_round_length ~always ctx strategy value step
  | Mod (a, b) -> pp_mod_length ~always ctx a b
  | Rem_fn (a, b) -> pp_rem_length ~always ctx a b
  | Hypot values -> pp_hypot_length ~always ctx values
  | Abs v -> Pp.call "abs" (pp_length ~always) ctx v
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
      pp_anchor_length ~always ctx name side fallback
  | Attr attr -> Pp.call "attr" (pp_attr_call (pp_length ~always)) ctx attr
  | Env env -> pp_env (pp_length ~always) ctx env
  | Var v -> pp_var (pp_length ~always) ctx v
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Content -> Pp.string ctx "content"
  | Calc cv -> pp_length_calc ~always ctx cv

and pp_calc_wrapped_length ~always ctx length =
  Pp.string ctx "calc(";
  pp_length ~always ctx length;
  Pp.char ctx ')'

(* CSS Values 4 sec. 10.2: arguments to math functions ([clamp()], [min()],
   [max()], [minmax()]) are implicit calc expressions. A top-level [Calc] node
   is emitted as its bare expression body (no surrounding [calc(...)]). Nested
   [Calc] inside other length shapes still serializes through [pp_length]. *)
and pp_length_math_arg ~always ctx (length : length) =
  match length with
  | Calc cv ->
      let ctx = { ctx with in_calc = true } in
      pp_calc_contents (pp_length ~always) ctx cv
  | _ -> pp_length ~always ctx length

and pp_anchor_length ~always ctx name side fallback =
  Pp.call "anchor"
    (fun ctx (name, side, fallback) ->
      Option.iter
        (fun name ->
          Pp.string ctx name;
          Pp.space ctx ())
        name;
      Pp.string ctx side;
      Option.iter
        (fun fallback ->
          Pp.comma ctx ();
          pp_length ~always ctx fallback)
        fallback)
    ctx (name, side, fallback)

and pp_round_length ~always ctx strategy value step =
  Pp.call "round"
    (fun ctx (strategy, value, step) ->
      (* [round()] is a math function (CSS Values 4 10.x): its value/step
         operands are a typed calc context, so keep their units. The [strategy]
         keyword is not a math operand and is emitted as-is. *)
      if strategy <> "nearest" then (
        Pp.string ctx strategy;
        Pp.comma ctx ());
      let ctx = { ctx with in_calc = true } in
      pp_length ~always ctx value;
      Pp.comma ctx ();
      pp_length ~always ctx step)
    ctx (strategy, value, step)

and pp_mod_length ~always ctx a b =
  Pp.call "mod"
    (fun ctx (a, b) ->
      let ctx = { ctx with in_calc = true } in
      pp_length ~always ctx a;
      Pp.comma ctx ();
      pp_length ~always ctx b)
    ctx (a, b)

and pp_rem_length ~always ctx a b =
  Pp.call "rem"
    (fun ctx (a, b) ->
      let ctx = { ctx with in_calc = true } in
      pp_length ~always ctx a;
      Pp.comma ctx ();
      pp_length ~always ctx b)
    ctx (a, b)

and pp_hypot_length ~always ctx values =
  Pp.call_list "hypot" (pp_length ~always) { ctx with in_calc = true } values

and pp_length_calc ~always ctx cv = pp_generic_length_calc ~always ctx cv

and pp_generic_length_calc ~always ctx cv =
  let cv = if Pp.minified ctx then resolve_length_calc_vars ctx cv else cv in
  match cv with
  | Val (Var _ as length) when Pp.minified ctx ->
      pp_calc_wrapped_length ~always ctx length
  | _ ->
      let always = always || calc_contains_var cv in
      pp_calc_with ~unwrap_num:false
        ~unwrap:(fun v -> not (negative_length v))
        (pp_length ~always) ctx cv
