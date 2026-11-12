(** Angles, numbers, percentages and durations: canonicalisation and printing.
*)

open Values_intf
open Values_var
open Values_calc
open Values_length

let angle_degrees_opt = function
  | Deg value -> Some value
  | Rad value -> Some (value *. 180. /. Float.pi)
  | Turn value -> Some (value *. 360.)
  | Grad value -> Some (value *. 0.9)
  | _ -> None

let rec pp_angle : angle Pp.t =
 fun ctx -> function
  (* Pure serialiser: the authored unit is kept. Converting between
     losslessly-interchangeable units (deg / turn / grad) and folding the static
     math functions are AST rewrites done by [normalize_angle]. *)
  | Deg f -> pp_unit ctx f "deg"
  | Rad f -> pp_unit ctx f "rad"
  | Turn f -> pp_unit ctx f "turn"
  | Grad f -> pp_unit ctx f "grad"
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
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_angle ctx a;
          Pp.comma ctx ();
          pp_angle ctx b)
        ctx (a, b)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_angle ctx a;
          Pp.comma ctx ();
          pp_angle ctx b)
        ctx (a, b)
  (* A math function is itself an [<angle>] production, so the [calc()] around
     one is redundant (CSS Values 4 sec. 10.8). *)
  | Calc (Math_fn fn) -> pp_math_fn ctx fn
  | Calc c -> pp_calc_with ~unwrap_num:false pp_angle ctx c
  | Var v -> pp_var pp_angle ctx v
  | Invalid tokens ->
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified tokens
         else Parser.string_of_components tokens)

(** Pretty printer for percentage types *)
let rec pp_percentage ?(always = false) : percentage Pp.t =
 fun ctx -> function
  | Pct f -> Pp.pct ctx f
  | Num f -> Pp.float ctx f
  | Var v -> pp_var (pp_percentage ~always) ctx v
  | Calc c -> pp_calc (pp_percentage ~always) ctx c

(* Evaluate the static CSS math functions on a [<length>] (the folds the printer
   used to do under minify), so the printer stays a pure serialiser. min / max /
   clamp reduce to one dimension when the operands share a unit; round / mod /
   rem / hypot / abs fold on [px] operands; calc folds through the generic
   simplifier. A non-static operand keeps the call. *)
(* CSS Values 4 sec. 6: a zero [<length>] drops its unit ([0px] -> [0]). That
   leaves [<length>] for [<number>], so it is a type-changing rewrite, not a
   shorter spelling - the printer keeps [0px] and the strip happens here. Only a
   top-level [<length>] strips: a calc / function operand keeps its unit, and a
   zero [<percentage>] never strips (unsound: [0%] is not [0] in every context). *)
let strip_zero_length (l : length) : length =
  match l with Pct _ | Zero -> l | _ -> if length_is_zero l then Zero else l

(* [strip] is true for a top-level [<length>] (a direct property/shorthand
   value) and false for a calc / math-function operand, which keeps its unit. *)
(* CSS Values 4 sec. 10.9: an infinite result clamps to the largest value the
   property can hold, and a finite length past every such bound clamps to the
   same one, so [calc(infinity * 1px)] is written as the largest float the
   printer spells, [3.40282e38px], which is also what lightningcss writes. Only a
   whole [calc()] of that one product folds: inside a sum the infinity is an
   operand whose arithmetic ([infinity - infinity] is NaN) the [calc()] keeps. *)
let rec infinite_length (c : length calc) : length option =
  let sign = function
    | Math_const Infinity -> Some 1.
    | Math_const Neg_infinity -> Some (-1.)
    | Num n when Float.abs n = Float.infinity -> Some (Float.copy_sign 1. n)
    | _ -> None
  in
  let fold s v =
    match calc_length_unit v with
    | Some (unit, value) when value <> 0. && not (Float.is_nan value) ->
        Some
          (length_from_calc_unit unit
             (Float.copy_sign Float.infinity (s *. value)))
    | _ -> None
  in
  match c with
  | Nested inner | Parens inner -> infinite_length inner
  | Expr (Val v, Mul, k) | Expr (k, Mul, Val v) -> (
      match sign k with Some s -> fold s v | None -> None)
  | _ -> None

let rec normalize_length ?(strip = true) ?(non_negative = false)
    ?(ctx = default_calc_ctx) (l : length) : length =
  let nf = normalize_length ~strip:false ~ctx in
  let result =
    match l with
    | Calc cv -> (
        match
          cv |> eval_length_calc ~ctx |> linear_length_calc
          |> eval_length_calc ~ctx
        with
        | Val v -> v
        | folded -> (
            match infinite_length folded with
            | Some v -> v
            | None -> Calc folded))
    | Clamp (mn, v, mx) -> (
        let mn = nf mn and v = nf v and mx = nf mx in
        if mn = v && v = mx then v
        else
          (* clamp(lo, v, hi) = max(lo, min(v, hi)); folds to one value when all
             three share a unit, returning lo when lo > hi. *)
          match try_reduce_typed_min_max [ v; mx ] Float.min with
          | Some inner -> (
              match try_reduce_typed_min_max [ mn; inner ] Float.max with
              | Some r -> r
              | None -> Clamp (mn, v, mx))
          | None -> Clamp (mn, v, mx))
    | Min xs -> (
        let xs = List.map nf xs in
        match try_reduce_typed_min_max xs Float.min with
        | Some r -> r
        | None -> Min xs)
    | Max xs -> (
        let xs = List.map nf xs in
        match try_reduce_typed_min_max xs Float.max with
        | Some r -> r
        | None -> Max xs)
    | Minmax (mn, mx) -> Minmax (nf mn, nf mx)
    | Round (strategy, value, step) -> (
        match (nf value, nf step) with
        | Px v, Px s when s <> 0. -> Px (round_length_step strategy v s)
        | value, step -> Round (strategy, value, step))
    | Mod (a, b) -> (
        match (nf a, nf b) with
        | Px a, Px b when b <> 0. -> Px (a -. (Float.floor (a /. b) *. b))
        | a, b -> Mod (a, b))
    | Rem_fn (a, b) -> (
        match (nf a, nf b) with
        | Px a, Px b when b <> 0. -> Px (Float.rem a b)
        | a, b -> Rem_fn (a, b))
    | Hypot xs -> (
        let xs = List.map nf xs in
        match xs with
        | [ (Px _ as v) ] -> v
        | _ -> (
            match px_values xs with
            | Some (_ :: _ as vs) ->
                Px
                  (round_computed
                     (Float.sqrt
                        (List.fold_left (fun acc f -> acc +. (f *. f)) 0. vs)))
            | _ -> Hypot xs))
    | Abs v -> ( match nf v with Px x -> Px (Float.abs x) | v -> Abs v)
    | Fit_content_arg arg -> Fit_content_arg (nf arg)
    | Calc_size (basis, calc) -> (
        let basis = nf basis in
        match
          calc |> eval_length_calc ~ctx |> linear_length_calc
          |> eval_length_calc ~ctx
        with
        | folded -> Calc_size (basis, folded))
    | _ -> canonical_dimension l
  in
  (* CSS Values 4 sec. 10.12 clamps a math function whose value falls outside
     the property's range at computed-value time and keeps the declaration, so
     folding one to a literal the reader then refuses would drop it: the
     arithmetic still folds, and the call stays around the result. A literal
     that was already negative is the reader's business, not this fold's. *)
  let result =
    if non_negative && negative_length result && not (negative_length l) then
      (Calc (Val result) : length)
    else result
  in
  if strip then strip_zero_length result else result

(* Fold the numeric parts of a length-percentage [calc()], keeping any [var()]:
   [calc(var(--x) + 1px + 2px)] -> [calc(var(--x) + 3px)], [calc(1px + 2px)] ->
   [3px]. A wrapped [<length>] folds its own math functions. *)
(* {!infinite_length} for a [<length-percentage>] calc, whose leaf is a length
   or a percentage: [calc(infinity * 1%)] is [3.40282e38%]. *)
let rec infinite_length_percentage (c : length_percentage calc) :
    length_percentage option =
  let sign = function
    | Math_const Infinity -> Some 1.
    | Math_const Neg_infinity -> Some (-1.)
    | Num n when Float.abs n = Float.infinity -> Some (Float.copy_sign 1. n)
    | _ -> None
  in
  let fold s (v : length_percentage) =
    match v with
    | Length l -> (
        match
          infinite_length (Expr (Val l, Mul, Num (s *. Float.infinity)))
        with
        | Some l -> Some (Length l : length_percentage)
        | None -> None)
    | Pct p when p <> 0. && not (Float.is_nan p) ->
        Some (Pct (Float.copy_sign Float.infinity (s *. p)))
    | _ -> None
  in
  match c with
  | Nested inner | Parens inner -> infinite_length_percentage inner
  | Expr (Val v, Mul, k) | Expr (k, Mul, Val v) -> (
      match sign k with Some s -> fold s v | None -> None)
  | _ -> None

let normalize_length_percentage ?(strip = true) ?(non_negative = false)
    ?(ctx = default_calc_ctx) (lp : length_percentage) : length_percentage =
  match lp with
  | Calc c -> (
      match c |> eval_lp_calc ~ctx |> linear_lp_calc |> eval_lp_calc ~ctx with
      (* Sec. 10.12 clamps a call past the property's range at computed-value
         time and keeps the declaration, so folding one to a literal the reader
         then refuses would drop it: the arithmetic still folds, and the call
         stays around the result. This is what [normalize_number] does for the
         number and time families. *)
      | Val (Length v) when non_negative && negative_length v ->
          Calc (Val (Length v))
      | Val v -> v
      | folded -> (
          match infinite_length_percentage folded with
          | Some v -> v
          | None -> Calc folded))
  | Length l ->
      let l' = normalize_length ~strip ~non_negative ~ctx l in
      if l' == l then lp else Length l'
  | _ -> lp

(* The [<number>] counterpart of [negative_length]: a number a property whose
   range starts at [0] refuses when it is written as a literal. *)
let negative_number : number -> bool = function Num f -> f < 0. | _ -> false

(* Evaluate the static CSS math functions on [<number>] (the folds the printer
   used to do under minify), so the printer stays a pure serialiser. Recurses so
   nested calls fold ([abs(hypot(3, 4))] -> [5]); a non-static operand keeps the
   call. *)
let rec normalize_number ?(ctx = default_calc_ctx) ?(non_negative = false)
    (n : number) : number =
  (* CSS Values 4 sec. 10.12 checks the property's range on the value the whole
     calculation resolves to rather than on each operand, so the operand fold
     drops [non_negative]. *)
  let nf = normalize_number ~ctx in
  let result =
    match n with
    | Calc c -> (
        match eval_calc ~ctx c with
        | Num f -> Num f
        | Val v -> nf v
        | folded -> Calc folded)
    | Round (strategy, a, b) -> (
        match (nf a, nf b) with
        | Num value, Num step when step <> 0. ->
            Num (round_to_step strategy value step)
        | a, b -> Round (strategy, a, b))
    | Mod (a, b) -> (
        match (nf a, nf b) with
        | Num a, Num b when b <> 0. -> Num (mod_value a b)
        | a, b -> Mod (a, b))
    | Rem (a, b) -> (
        match (nf a, nf b) with
        | Num a, Num b when b <> 0. -> Num (Float.rem a b)
        | a, b -> Rem (a, b))
    | Hypot (a, b) -> (
        match (nf a, nf b) with
        | Num a, Num b -> Num (Float.sqrt ((a *. a) +. (b *. b)))
        | a, b -> Hypot (a, b))
    | Pow (a, b) -> (
        match (nf a, nf b) with
        | Num a, Num b -> Num (Float.pow a b)
        | a, b -> Pow (a, b))
    | Sqrt v -> (
        match nf v with Num a when a >= 0. -> Num (Float.sqrt a) | v -> Sqrt v)
    | Abs v -> ( match nf v with Num a -> Num (Float.abs a) | v -> Abs v)
    | Sign v -> Sign (nf v)
    | Sin _ | Num _ | Var _ -> n
  in
  (* Sec. 10.12 clamps a math function past the property's range at
     computed-value time and keeps the declaration, so folding one to a literal
     the reader then refuses would drop it: the arithmetic still folds, and the
     call stays around the result. A literal that was already negative is the
     reader's business, not this fold's. *)
  match result with
  | Num f when non_negative && f < 0. && not (negative_number n) -> Calc (Num f)
  | _ -> result

(* Fold the value-independent parts of a [<percentage>] [calc()] ([calc(1 / 2 *
   100%)] -> [calc(.5*100%)]), keeping any [var()]. Replaces the numeric /
   identity reduction the printer did under minify, now that pp is a pure
   serialiser. *)
(* [<percentage>] scaling/combining: [calc(50% * 2)] -> [100%], [calc(10% +
   20%)] -> [30%]. A bare [<number>] operand stays a [Num] leaf and folds
   through the generic numeric arms. *)
let pct_scale ?(exact = true) op (p : percentage) n : percentage option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match p with
    | Pct v -> scaled (fun x -> (Pct x : percentage)) v
    | Num v -> scaled (fun x -> (Num x : percentage)) v
    | _ -> Option.none

let pct_combine op (a : percentage) (b : percentage) : percentage option =
  let f = combine_leaf op in
  match (a, b) with
  | Pct x, Pct y -> Option.map (fun r -> (Pct r : percentage)) (f x y)
  | Num x, Num y -> Option.map (fun r -> (Num r : percentage)) (f x y)
  | _ -> Option.none

let pct_is_zero : percentage -> bool = function
  | Pct f | Num f -> f = 0.
  | _ -> false

let pct_calc_math_fn (fn : math_fn) : percentage calc =
  typed_math_fn
    (fun unit v ->
      if String.equal unit "%" then Option.some (Pct v : percentage)
      else Option.none)
    fn

let eval_pct_calc ?(ctx = default_calc_ctx) (c : percentage calc) :
    percentage calc =
  eval_typed_calc ~math_fn:pct_calc_math_fn
    ~scale:(fun ~exact op v n -> pct_scale ~exact op v n)
      (* [Pp.pct] / [Pp.float] print a [<percentage>] or [<number>] coefficient
         in full in both modes, so there is no budget to apply here. *)
    ~combine:pct_combine ~round:Fun.id ~is_zero:pct_is_zero
    ~zero:(Val (Pct 0.) : percentage calc)
    ~ctx c

let normalize_percentage ?(ctx = default_calc_ctx) (p : percentage) : percentage
    =
  match p with
  | Calc c -> (
      match eval_pct_calc ~ctx c with
      | Num f -> Num f
      | Val v -> v
      | folded -> Calc folded)
  | Pct _ | Num _ | Var _ -> p

(* [<time>] scaling/combining ([calc(1s * 2)] -> [2s], [calc(.5s + .5s)] ->
   [1s]); same-unit only, so [s] and [ms] stay distinct. A bare-number reduction
   stays wrapped (a [<number>] is not a [<time>]). *)
let time_scale ?(exact = true) op (d : duration) n : duration option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match d with
    | S v -> scaled (fun x -> S x) v
    | Ms v -> scaled (fun x -> Ms x) v
    | _ -> Option.none

let time_combine op (a : duration) (b : duration) : duration option =
  let f = combine_leaf op in
  match (a, b) with
  | S x, S y -> Option.map (fun r -> S r) (f x y)
  | Ms x, Ms y -> Option.map (fun r -> Ms r) (f x y)
  | _ -> Option.none

let time_is_zero : duration -> bool = function
  | S f | Ms f -> f = 0.
  | _ -> false

let time_round : duration -> duration = function
  | S f -> S (round_computed f)
  | Ms f -> Ms (round_computed f)
  | d -> d

let time_calc_math_fn (fn : math_fn) : duration calc =
  typed_math_fn
    (fun unit v ->
      match unit with
      | "s" -> Option.some (S v : duration)
      | "ms" -> Option.some (Ms v : duration)
      | _ -> Option.none)
    fn

let eval_time_calc ?(ctx = default_calc_ctx) (c : duration calc) : duration calc
    =
  eval_typed_calc ~math_fn:time_calc_math_fn
    ~scale:(fun ~exact op v n -> time_scale ~exact op v n)
    ~combine:time_combine ~round:time_round ~is_zero:time_is_zero
    ~zero:(Val (S 0.) : duration calc)
    ~ctx c

(* The [<time>] counterpart of [negative_length]: a duration a property whose
   range starts at [0s] refuses when it is written as a literal. *)
let negative_duration : duration -> bool = function
  | Ms f | S f -> f < 0.
  | _ -> false

(* Choose equivalent time units and evaluate static stepped functions in the
   AST, before declaration hashes are compared. *)
let rec normalize_duration ?(ctx = default_calc_ctx) ?(canonicalize_ms = true)
    ?(non_negative = false) (d : duration) : duration =
  let normalize = normalize_duration ~ctx ~canonicalize_ms ~non_negative in
  let preserve rebuilt = if rebuilt = d then d else rebuilt in
  match d with
  | Ms f when canonicalize_ms ->
      let seconds = f /. 1000. in
      let ms = Pp.string_of_float ~drop_leading_zero:true f in
      let s = Pp.string_of_float ~drop_leading_zero:true seconds in
      if String.length s + 1 <= String.length ms + 2 then S seconds else d
  | Durations durations ->
      preserve (Durations (Common.List.map_preserve normalize durations))
  | Round (strategy, value, step) -> (
      let value = normalize value and step = normalize step in
      match (value, step) with
      | Ms value, Ms step when step <> 0. ->
          normalize (Ms (round_to_step strategy value step))
      | S value, S step when step <> 0. ->
          normalize (S (round_to_step strategy value step))
      | _ -> preserve (Round (strategy, value, step)))
  | Rem (a, b) -> (
      let a = normalize a and b = normalize b in
      match (a, b) with
      | Ms a, Ms b when b <> 0. -> normalize (Ms (Float.rem a b))
      | S a, S b when b <> 0. -> normalize (S (Float.rem a b))
      | _ -> preserve (Rem (a, b)))
  | Mod (a, b) -> (
      let a = normalize a and b = normalize b in
      match (a, b) with
      | Ms a, Ms b when b <> 0. -> normalize (Ms (mod_value a b))
      | S a, S b when b <> 0. -> normalize (S (mod_value a b))
      | _ -> preserve (Mod (a, b)))
  | Calc c -> (
      match eval_time_calc ~ctx c with
      (* CSS Values 4 sec. 10.12 clamps a math function past the property's
         range at computed-value time and keeps the declaration, so folding one
         to a literal the reader then refuses would drop it: keep the call. *)
      | Val v when non_negative && negative_duration v ->
          preserve (Calc (Val v))
      | Val v -> normalize v
      | folded -> preserve (Calc folded))
  | Var v ->
      let fallback =
        match v.fallback with
        | Fallback value ->
            let value' = normalize value in
            if value' == value then v.fallback else Fallback value'
        | (Empty | Empty2 | None | Syntax_fallback _ | Var_fallback _) as other
          ->
            other
      in
      let default =
        match v.default with
        | Some value ->
            let value' = normalize value in
            if value' == value then v.default else Some value'
        | None -> v.default
      in
      if fallback == v.fallback && default == v.default then d
      else Var { v with fallback; default }
  | _ -> d

(* CSS Values 4 sec. 10.10.1: a typed [<angle>] multiplied or divided by a
   unitless number scales the angle's coefficient and keeps its unit, so
   [calc(1deg * -45)] reduces to [-45deg]. Division folds only when the quotient
   is [exact] (see [exact_div]); dividing by a math constant ([pi] / ...) is
   irrational, so it rounds like the matching multiplication. *)
let angle_scale ?(exact = true) op (a : angle) n : angle option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match a with
    | Deg v -> scaled (fun x -> Deg x) v
    | Rad v -> scaled (fun x -> Rad x) v
    | Turn v -> scaled (fun x -> Turn x) v
    | Grad v -> scaled (fun x -> Grad x) v
    | _ -> Option.none

let angle_is_zero : angle -> bool = function
  | Deg f | Rad f | Turn f | Grad f -> f = 0.
  | _ -> false

(* CSS Values 4 sec. 10.10.1: same-unit add/sub of two typed angles reduces to
   one angle ([calc(45deg + 45deg)] -> [90deg]). Cross-unit operands (e.g.
   [1turn + 90deg]) stay unfolded; [normalize_angle] picks the shortest spelling
   of a single folded operand, not across a mixed sum. *)
let angle_combine op (a : angle) (b : angle) : angle option =
  let f = combine_leaf op in
  match (a, b) with
  | Deg x, Deg y -> Option.map (fun r -> Deg r) (f x y)
  | Rad x, Rad y -> Option.map (fun r -> Rad r) (f x y)
  | Turn x, Turn y -> Option.map (fun r -> Turn r) (f x y)
  | Grad x, Grad y -> Option.map (fun r -> Grad r) (f x y)
  | _ -> Option.none

let angle_round : angle -> angle = function
  | Deg f -> Deg (round_computed f)
  | Rad f -> Rad (round_computed f)
  | Turn f -> Turn (round_computed f)
  | Grad f -> Grad (round_computed f)
  | a -> a

let angle_calc_math_fn (fn : math_fn) : angle calc =
  match fn with
  (* CSS Values 4 sec. 10.9: the inverse trig functions return an [<angle>],
     which [eval_math_fn] reports in degrees. *)
  | Asin _ | Acos _ | Atan _ | Atan2 _ -> (
      match eval_math_fn fn with
      | Some v when not (Float.is_nan v) -> Val (Deg v : angle)
      | Some _ | None -> Math_fn fn)
  | fn ->
      typed_math_fn
        (fun unit v ->
          match unit with
          | "deg" -> Option.some (Deg v : angle)
          | "rad" -> Option.some (Rad v : angle)
          | "turn" -> Option.some (Turn v : angle)
          | "grad" -> Option.some (Grad v : angle)
          | _ -> Option.none)
        fn

let eval_angle_calc ?(ctx = default_calc_ctx) (c : angle calc) : angle calc =
  eval_typed_calc ~math_fn:angle_calc_math_fn
    ~scale:(fun ~exact op v n -> angle_scale ~exact op v n)
    ~combine:angle_combine ~round:angle_round ~is_zero:angle_is_zero
    ~zero:(Val (Deg 0.)) ~ctx c

(* Canonicalise an [<angle>]: fold the static math functions ([round] / [mod] /
   [rem] on [deg] operands), then pick the shortest of the
   losslessly-interconvertible spellings (deg / turn / grad). [rad] goes through
   pi, so it cannot share one magnitude and is left as-is. *)
(* Shortest spelling of a concrete angle, via unit conversions whose printed form
   still denotes the same value. [deg -> turn] can floor a small angle to [0turn]
   once the printer caps the mantissa; comparing printed forms back in degrees
   rejects such value changes. The original spelling seeds the fold; ties prefer
   deg. *)
let angle_shortest (a : angle) : angle =
  let printed f = Pp.string_of_float ~drop_leading_zero:true f in
  let render unit f = String.length (printed f) + String.length unit in
  let printed_deg unit f =
    match float_of_string_opt (printed f) with
    | Some v -> (
        match unit with "turn" -> v *. 360. | "grad" -> v *. 0.9 | _ -> v)
    | None -> Float.nan
  in
  let cands =
    match a with
    | Deg f -> [ (f, "deg", a); (f /. 360., "turn", Turn (f /. 360.)) ]
    | Turn f -> [ (f, "turn", a); (f *. 360., "deg", Deg (f *. 360.)) ]
    | Grad f -> [ (f, "grad", a); (f *. 0.9, "deg", Deg (f *. 0.9)) ]
    | _ -> []
  in
  match cands with
  | [] -> a
  | (f0, u0, n0) :: rest ->
      let ref_deg = printed_deg u0 f0 in
      let round_trips (f, u, _) =
        (* Exact conversion only: the converted spelling must denote the same
           angle, modulo the float noise of the unit arithmetic, not merely a
           close one. A [grad -> deg] re-spelling that drops a significant
           figure (or a small angle collapsing to [0turn]) is a value change, so
           the authored unit is kept. *)
        Float.abs (printed_deg u f -. ref_deg)
        <= (1e-9 *. Float.abs ref_deg) +. 1e-15
      in
      let rest = List.filter round_trips rest in
      snd
        (List.fold_left
           (fun (best_len, best) (f, u, n) ->
             let l = render u f in
             if l < best_len then (l, n) else (best_len, best))
           (render u0 f0, n0)
           rest)

(* CSS Values 4 sec. 6.1: [deg], [grad], [rad] and [turn] are units of one
   dimension. Under the [budget] every concrete angle is its degrees: a [turn]
   or [grad] converts exactly and a [rad] through pi, and either conversion is
   Cascade's own digits, so it takes the six-significant-figure budget a
   quotient does ([round_computed]). An authored degree keeps every digit. *)
let budget_degrees (a : angle) : angle =
  match a with
  | Rad _ | Turn _ | Grad _ -> (
      match angle_degrees_opt a with
      | Some d -> Deg (round_computed d)
      | None -> a)
  | a -> a

let normalize_angle ?(ctx = default_calc_ctx) =
  let spell = if ctx.budget then budget_degrees else angle_shortest in
  let rec go (a : angle) : angle =
    match a with
    | Round (strategy, v, s) -> (
        match (go v, go s) with
        | Deg v, Deg s when s <> 0. -> spell (Deg (round_to_step strategy v s))
        | v, s -> Round (strategy, v, s))
    | Mod (x, y) -> (
        match (go x, go y) with
        | Deg x, Deg y when y <> 0. -> spell (Deg (mod_value x y))
        | x, y -> Mod (x, y))
    | Rem (x, y) -> (
        match (go x, go y) with
        | Deg x, Deg y when y <> 0. -> spell (Deg (Float.rem x y))
        | x, y -> Rem (x, y))
    | Calc c -> (
        match eval_angle_calc ~ctx c with
        | Val v -> go v
        | folded -> Calc folded)
    | Deg _ | Turn _ | Grad _ -> spell a
    (* [angle_shortest] leaves radians alone because deg/rad conversion goes
       through pi and so is never exactly value-preserving. Zero is the one
       radian value that converts exactly, and it is the one that matters: a
       zero angle is what the grammars let you drop. The budget spends its six
       figures on the rest. *)
    | Rad f when f = 0. -> Deg 0.
    | Rad _ when ctx.budget -> spell a
    | Rad _ | Var _ | Invalid _ -> a
  in
  go

let rec pp_length_percentage ?(always = false) : length_percentage Pp.t =
 fun ctx -> function
  | Length l -> pp_length ~always ctx l
  | Pct f -> Pp.pct ctx f
  | Env env -> pp_env (pp_length_percentage ~always) ctx env
  | Var v -> pp_var (pp_length_percentage ~always) ctx v
  | Calc c ->
      (* Inline mode substitutes a [var()]'s default value into the calc. *)
      let c = if Pp.minified ctx then resolve_lp_calc_vars ctx c else c in
      let always = always || calc_contains_var c in
      pp_calc_with ~unwrap_num:false
        ~unwrap:(fun v ->
          match (v : length_percentage) with
          | Length l -> not (negative_length l)
          | Pct f -> f >= 0.
          | _ -> true)
        (pp_length_percentage ~always)
        ctx c
  | Invalid tokens ->
      Pp.string ctx
        (if Pp.minified ctx then Parser.to_string_minified tokens
         else Parser.string_of_components tokens)

(* Pure serialiser: the [Pct] <-> [Num] shortest-spelling choice ([Pct 0. -> 0]
   included) is a node-changing fold in [normalize_number_percentage], not
   here. *)
let rec pp_number_percentage : number_percentage Pp.t =
 fun ctx -> function
  | Num f -> Pp.float ctx f
  | Pct f -> Pp.pct ctx f
  | Var v -> pp_var pp_number_percentage ctx v
  | Calc c -> pp_calc pp_number_percentage ctx c

(* AST-level [<number-percentage>] canonicalisation: CSS Transforms 2 secs. 5 and
   12.1-12.2 and Filter Effects 1 sec. 6.1 define typed positions where [%] and
   number are equivalent (100% = 1). Pick the shorter leaf so
   [pp_number_percentage] serialises a canonical node. [Var] / [Calc] are left
   alone (inside calc() [%] and number aren't interchangeable). *)
(* [<number-percentage>] shares [<percentage>]'s scaling/combining; the type is
   distinct, so the helpers are too. *)
let np_scale ?(exact = true) op (np : number_percentage) n :
    number_percentage option =
  if not (Float.is_finite n) then Option.none
  else
    let scaled = scale_leaf ~exact op n in
    match np with
    | Pct v -> scaled (fun x -> (Pct x : number_percentage)) v
    | Num v -> scaled (fun x -> (Num x : number_percentage)) v
    | _ -> Option.none

let np_combine op (a : number_percentage) (b : number_percentage) :
    number_percentage option =
  let f = combine_leaf op in
  match (a, b) with
  | Pct x, Pct y -> Option.map (fun r -> (Pct r : number_percentage)) (f x y)
  | Num x, Num y -> Option.map (fun r -> (Num r : number_percentage)) (f x y)
  | _ -> Option.none

let np_is_zero : number_percentage -> bool = function
  | Pct f | Num f -> f = 0.
  | _ -> false

let np_calc_math_fn (fn : math_fn) : number_percentage calc =
  typed_math_fn
    (fun unit v ->
      if String.equal unit "%" then Option.some (Pct v : number_percentage)
      else Option.none)
    fn

let eval_np_calc ?(ctx = default_calc_ctx) (c : number_percentage calc) :
    number_percentage calc =
  eval_typed_calc ~math_fn:np_calc_math_fn
    ~scale:(fun ~exact op v n -> np_scale ~exact op v n)
      (* A [<number>] / [<percentage>] coefficient prints in full. *)
    ~combine:np_combine ~round:Fun.id ~is_zero:np_is_zero
    ~zero:(Val (Num 0.) : number_percentage calc)
    ~ctx c

let rec normalize_number_percentage ?(ctx = default_calc_ctx)
    (np : number_percentage) : number_percentage =
  let len f = String.length (Pp.string_of_float ~drop_leading_zero:true f) in
  match np with
  | Calc c -> (
      match eval_np_calc ~ctx c with
      | Num f -> normalize_number_percentage ~ctx (Num f)
      | Val v -> normalize_number_percentage ~ctx v
      | folded -> Calc folded)
  | Pct f ->
      let num_f = f /. 100. in
      (* num spelling vs pct spelling (one extra '%' character); Num wins on tie
         to match the original pp logic and keep the fold idempotent. *)
      if len num_f <= len f + 1 then (Num num_f : number_percentage) else np
  | Num f ->
      let pct_f = f *. 100. in
      if len pct_f + 1 < len f then (Pct pct_f : number_percentage) else np
  | other -> other

(* CSS Values 4 sec. 7.2: [ms] and [s] are interchangeable. Pure minified
   serialization picks the shorter exact leaf spelling as a fallback for callers
   that deliberately skip AST normalization. [shorten_ms:false] keeps the unit
   wherever the normalization would not have converted it either: a [calc()]
   operand, and the [interest-delay] grammar. *)
let pp_duration_unit ?(shorten_ms = true) ctx f suffix =
  if f = 0. then
    if Pp.minified ctx then Pp.string ctx "0s" else pp_unit ctx f suffix
  else if (not shorten_ms) || (not (Pp.minified ctx)) || suffix <> "ms" then
    pp_unit ctx f suffix
  else
    let seconds = f /. 1000. in
    let ms = Pp.string_of_float ~drop_leading_zero:true f in
    let s = Pp.string_of_float ~drop_leading_zero:true seconds in
    if String.length s + 1 <= String.length ms + 2 then (
      Pp.string ctx s;
      Pp.char ctx 's')
    else (
      Pp.string ctx ms;
      Pp.string ctx "ms")

let rec pp_duration_with ~shorten_ms : duration Pp.t =
 fun ctx -> function
  | Ms f -> pp_duration_unit ~shorten_ms ctx f "ms"
  | S f -> pp_duration_unit ctx f "s"
  | Auto -> Pp.string ctx "auto"
  | Durations durations ->
      Pp.list ~sep:Pp.comma (pp_duration_with ~shorten_ms) ctx durations
  | Round (strategy, value, step) ->
      Pp.call "round"
        (fun ctx (strategy, value, step) ->
          if strategy <> "nearest" then (
            Pp.string ctx strategy;
            Pp.comma ctx ());
          pp_duration_with ~shorten_ms ctx value;
          Pp.comma ctx ();
          pp_duration_with ~shorten_ms ctx step)
        ctx (strategy, value, step)
  | Rem (a, b) ->
      Pp.call "rem"
        (fun ctx (a, b) ->
          pp_duration_with ~shorten_ms ctx a;
          Pp.comma ctx ();
          pp_duration_with ~shorten_ms ctx b)
        ctx (a, b)
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_duration_with ~shorten_ms ctx a;
          Pp.comma ctx ();
          pp_duration_with ~shorten_ms ctx b)
        ctx (a, b)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var (pp_duration_with ~shorten_ms) ctx v
  (* [normalize_duration] canonicalises a typed [<time>] but folds a [calc()]
     only when it computes one, so the operands of a [calc()] that reaches the
     output stay as authored - a [var()] fallback among them. Shortening one
     here would print two unequal declarations alike and lose them a
     factoring. *)
  (* A negative time has no spelling at a property whose range starts at [0s],
     and which properties those are is not knowable here, so the wrapper stays
     on a leaf that reads back differently without it. *)
  | Calc c ->
      pp_calc_with ~unwrap_num:false
        ~unwrap:(fun d -> not (negative_duration d))
        ~pp_unwrapped:(pp_duration_with ~shorten_ms)
        (pp_duration_with ~shorten_ms:false)
        ctx c

let pp_duration : duration Pp.t = pp_duration_with ~shorten_ms:true
let pp_duration_preserve_ms : duration Pp.t = pp_duration_with ~shorten_ms:false

let rec pp_number : number Pp.t =
 fun ctx -> function
  | Num f when Float.is_nan f -> Pp.nan_value ctx ""
  | Num f ->
      Pp.string ctx (Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f)
  | Var v -> pp_var pp_number ctx v
  (* A negative number has no spelling at a property whose range starts at [0],
     and which properties those are is not knowable here, so the wrapper stays
     on a leaf that reads back differently without it. [normalize_number] has
     already unwrapped the rest, where the property is in hand. *)
  | Calc c ->
      pp_calc_with
        ~unwrap_num:(match c with Num f -> f >= 0. | _ -> true)
        ~unwrap:(fun v -> not (negative_number v))
        pp_number ctx c
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
  | Mod (a, b) ->
      Pp.call "mod"
        (fun ctx (a, b) ->
          pp_number ctx a;
          Pp.comma ctx ();
          pp_number ctx b)
        ctx (a, b)
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
