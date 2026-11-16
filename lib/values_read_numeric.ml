(** Readers for lengths, angles, durations, numbers and percentages. *)

open Values_intf
open Values_var
open Values_calc
open Values_read_calc

let rec read_length ?(allow_negative = true) ?(with_keywords = true)
    ?(sizing = false) ?(length_only = false) t : length =
  Cursor.ws t;
  let parsers =
    [
      read_var_length ~allow_negative ~length_only ~with_keywords;
      read_calc_length ~length_only;
      read_env_length ~allow_negative ~length_only ~with_keywords;
      read_function_length ~allow_negative ~length_only ~with_keywords;
      read_length_unit ~allow_negative ~length_only;
    ]
  in
  let parsers =
    if with_keywords then read_length_keyword ~sizing :: parsers else parsers
  in
  Cursor.one_of parsers t

and read_var_length ~allow_negative ~length_only ~with_keywords t : length =
  if Cursor.looking_at t "var(" then
    Var (read_var (read_length ~allow_negative ~length_only ~with_keywords) t)
  else Cursor.err t "expected var"

(* CSS Values 4 sec. 10.8: a [<calc-value>] is a number, a dimension, a
   percentage, a [<calc-keyword>] ([e], [pi], [infinity], [-infinity], [NaN]) or
   a parenthesised [<calc-sum>]. A CSS-wide keyword, [auto], an intrinsic size
   and the sizing functions are none of those, and sec. 10.9 makes the
   calculation's type failure for anything else, so a math operand reads a
   length with the keyword grammar off. *)
and read_math_operand_length ~allow_negative ~length_only t : length =
  read_length ~allow_negative ~length_only ~with_keywords:false t

and read_calc_length ~length_only t : length =
  if Cursor.looking_at_calc t then
    (* Same exception as [read_length_percentage]: inside [calc()] the
       non-negative constraint applies to the resolved value. *)
    Calc
      (read_calc ~result_type:`Value
         (read_math_operand_length ~allow_negative:true ~length_only)
         t)
  else Cursor.err t "expected calc"

and read_env_length ~allow_negative ~length_only ~with_keywords t : length =
  if Cursor.looking_at t "env(" then
    Env (read_env (read_length ~allow_negative ~length_only ~with_keywords) t)
  else Cursor.err t "expected env"

and read_function_length ~allow_negative ~length_only ~with_keywords t : length
    =
  match
    Cursor.any_function_call
      (read_length_function_body ~allow_negative ~length_only ~with_keywords t)
      t
  with
  | Some length -> length
  | None -> Cursor.err_expected t "function call"

and read_length_function_body ~allow_negative ~length_only ~with_keywords t name
    inner =
  let name = String.lowercase_ascii name in
  if
    length_only
    && List.mem name
         [ "minmax"; "fit-content"; "calc-size"; "anchor"; "anchor-size" ]
  then Cursor.err_invalid t "expected a length-valued math function";
  (* CSS Values 4 sec. 10.12 checks a property's range on the value a math
     function resolves to, not on each operand, the same exception
     [read_calc_length] makes for [calc()]: [width: abs(-1px)] reads where a
     literal [-1px] does not. The substitutions and the sizing functions are not
     math functions and keep the property's range. *)
  let allow_negative =
    allow_negative
    || List.mem name typed_math_function_names
    || (length_only && name <> "attr")
  in
  match
    List.assoc_opt name
      (length_function_readers ~allow_negative ~length_only ~with_keywords)
  with
  | Some read -> read inner
  | None -> Cursor.err t ("unknown function " ^ name)

and length_function_readers ~allow_negative ~length_only ~with_keywords =
  (* CSS Sizing 4 sec. 3.2 puts [fit-content()] and [calc-size()] in
     [<box-size>] and CSS Grid 2 sec. 7.2.1 puts [minmax()] in [<track-size>],
     so a caller reading a plain [<length>] takes none of the three. The math
     functions and the substitutions stay: those resolve to a length wherever
     one may stand. *)
  let sizing_functions =
    [
      ("minmax", read_minmax_length ~length_only);
      ( "fit-content",
        read_fit_content_length ~allow_negative ~length_only ~with_keywords );
      ( "calc-size",
        read_calc_size_length ~allow_negative ~length_only ~with_keywords );
    ]
  in
  (if with_keywords then sizing_functions else [])
  @ [
      ("clamp", read_clamp_length ~length_only);
      ("min", read_min_length ~length_only);
      ("max", read_max_length ~length_only);
      ("round", read_round_length ~allow_negative ~length_only);
      ("mod", read_mod_length ~allow_negative ~length_only);
      ("rem", read_rem_length ~allow_negative ~length_only);
      ("hypot", read_hypot_length ~allow_negative ~length_only);
      ("abs", read_abs_length ~allow_negative ~length_only);
      ("anchor-size", read_anchor_size_length);
      ("anchor", read_anchor_length ~allow_negative ~length_only ~with_keywords);
      ("attr", read_attr_length ~allow_negative ~length_only ~with_keywords);
    ]

(* CSS Values 4 sec. 10.2: arguments to [clamp()], [min()], [max()], [minmax()]
   are implicit math expressions, so [clamp(.5rem, 2vw + .5rem, 2rem)] is valid
   without a surrounding [calc()]. Parse each argument with [read_calc_expr] and
   collapse a singleton [Val] back to the plain length so the AST stays compact
   for the common case. The same section requires the arguments to "have a
   consistent type or else the function is invalid", so every one of them is
   checked against the length the surrounding property reads: [min(0, 1px)]
   mixes a [<number>] with a [<length>] and browsers drop it. *)
and read_implicit_calc_length ~length_only inner =
  let expr =
    read_calc_expr
      (read_math_operand_length ~allow_negative:true ~length_only)
      inner
  in
  validate_calc_type inner `Value expr;
  match expr with Val l -> l | expr -> Calc expr

and read_clamp_length ?(length_only = false) inner =
  Cursor.ws inner;
  let min = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.comma inner;
  Cursor.ws inner;
  let value = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.comma inner;
  Cursor.ws inner;
  let max = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Clamp (min, value, max)

and read_minmax_length ?(length_only = false) inner =
  Cursor.ws inner;
  let min = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.comma inner;
  Cursor.ws inner;
  let max = read_implicit_calc_length ~length_only inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Minmax (min, max)

and read_min_length ?(length_only = false) inner =
  let xs =
    Cursor.list ~at_least:1 ~sep:Cursor.comma
      (fun t ->
        Cursor.ws t;
        read_implicit_calc_length ~length_only t)
      inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Min xs

and read_max_length ?(length_only = false) inner =
  let xs =
    Cursor.list ~at_least:1 ~sep:Cursor.comma
      (fun t ->
        Cursor.ws t;
        read_implicit_calc_length ~length_only t)
      inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Max xs

and read_fit_content_length ~allow_negative ~length_only ~with_keywords inner =
  Cursor.ws inner;
  let arg = read_length ~allow_negative ~length_only ~with_keywords inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Fit_content_arg arg

and read_round_length ~allow_negative ~length_only inner =
  let strategy = read_round_strategy inner in
  let read = read_math_operand_length ~allow_negative ~length_only in
  let value = read inner in
  Cursor.ws inner;
  Cursor.comma inner;
  let step = read inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Round (strategy, value, step)

and read_binary_length ~allow_negative ~length_only make inner =
  let read = read_math_operand_length ~allow_negative ~length_only in
  let a = read inner in
  Cursor.ws inner;
  Cursor.comma inner;
  let b = read inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  make a b

and read_mod_length ~allow_negative ~length_only inner =
  read_binary_length ~allow_negative ~length_only
    (fun (a : length) (b : length) -> (Mod (a, b) : length))
    inner

and read_rem_length ~allow_negative ~length_only inner =
  read_binary_length ~allow_negative ~length_only
    (fun (a : length) (b : length) -> (Rem_fn (a, b) : length))
    inner

and read_hypot_length ~allow_negative ~length_only inner =
  let values =
    Cursor.list ~sep:Cursor.comma
      (read_math_operand_length ~allow_negative ~length_only)
      inner
  in
  Cursor.expect_eof inner;
  Hypot values

and read_unary_length ~allow_negative ~length_only make inner =
  let value = read_math_operand_length ~allow_negative ~length_only inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  make value

and read_abs_length ~allow_negative ~length_only inner =
  read_unary_length ~allow_negative ~length_only
    (fun (value : length) -> (Abs value : length))
    inner

(* CSS Values 5 (ED) sec. 12: [calc-size()] takes a sizing basis and then a
   calculation over [size], so both halves read the intrinsic sizes whatever
   property the function sits in. *)
and read_calc_size_length ~allow_negative ~length_only ~with_keywords inner =
  let read t =
    read_length ~allow_negative ~length_only ~with_keywords ~sizing:true t
  in
  let basis = read inner in
  Cursor.ws inner;
  Cursor.comma inner;
  let calc = read_calc_expr read inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Calc_size (basis, calc)

and read_anchor_length ~allow_negative ~length_only ~with_keywords inner =
  let first = Cursor.ident inner in
  Cursor.ws inner;
  let name, side = read_anchor_name_side inner first in
  Cursor.ws inner;
  let fallback =
    if Cursor.comma_opt inner then (
      Cursor.ws inner;
      Some (read_length ~allow_negative ~length_only ~with_keywords inner))
    else None
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Anchor (name, side, fallback)

and read_attr_length ~allow_negative ~length_only ~with_keywords inner =
  Cursor.ws inner;
  let name = Cursor.ident ~keep_case:true inner in
  let type_ = read_attr_type_hint inner in
  Cursor.ws inner;
  let fallback =
    read_attr_length_fallback ~allow_negative ~length_only ~with_keywords inner
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Attr { name; type_; fallback }

and read_attr_length_fallback ~allow_negative ~length_only ~with_keywords inner
    =
  if Cursor.comma_opt inner then (
    Cursor.ws inner;
    if Cursor.is_done inner then Empty_fallback
    else
      Attr_fallback
        (read_length ~allow_negative ~length_only ~with_keywords inner))
  else No_fallback

(** Read a non-negative length value (for padding properties) *)
let read_non_negative_length ?(with_keywords = true) ?(length_only = false) t :
    length =
  read_length ~allow_negative:false ~with_keywords ~length_only t

(** Argument category for [atan2(y, x)]: both arguments must share a category
    for the result to be unit-free. The float is the value already reduced into
    the category's canonical base ([px] for length, [ms] for time, [Hz] for
    frequency, [deg] for angle, raw fraction for percent). *)
type atan2_category = Number | Percentage | Length | Angle | Time | Frequency

let read_atan2_scalar t : atan2_category * float =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit = String.lowercase_ascii (Option.value ~default:"" unit_raw) in
  match unit with
  | "" -> (Number, n)
  | "%" -> (Percentage, n)
  (* CSS Values 4 sec. 6.2 absolute lengths converted to [px]. *)
  | "px" -> (Length, n)
  | "cm" -> (Length, n *. (96. /. 2.54))
  | "mm" -> (Length, n *. (96. /. 25.4))
  | "q" -> (Length, n *. (96. /. 101.6))
  | "in" -> (Length, n *. 96.)
  | "pt" -> (Length, n *. (96. /. 72.))
  | "pc" -> (Length, n *. 16.)
  (* Relative lengths share the [<length>] type but cannot reduce to a constant
     scalar at parse time; group them so [atan2(1vw, -1vw)] succeeds via
     raw-value ratio while [atan2(1vw, 1px)] is rejected. *)
  | "em" | "rem" | "ex" | "cap" | "ch" | "ic" | "lh" | "rlh" | "vw" | "vh"
  | "vmin" | "vmax" | "vi" | "vb" | "dvh" | "dvw" | "dvmin" | "dvmax" | "lvh"
  | "lvw" | "lvmin" | "lvmax" | "svh" | "svw" | "svmin" | "svmax" | "cqw"
  | "cqh" | "cqi" | "cqb" | "cqmin" | "cqmax" ->
      (Length, n)
  (* Angles, times, frequencies in their canonical units. *)
  | "deg" -> (Angle, n)
  | "rad" -> (Angle, n *. 180. /. Float.pi)
  | "turn" -> (Angle, n *. 360.)
  | "grad" -> (Angle, n *. 0.9)
  | "ms" -> (Time, n)
  | "s" -> (Time, n *. 1000.)
  | "hz" -> (Frequency, n)
  | "khz" -> (Frequency, n *. 1000.)
  | _ -> Cursor.err_invalid t ("invalid atan2 argument unit: " ^ unit)

(** Read an angle value *)
let read_angle_round read_angle t =
  read_round_call (fun s v step -> (Round (s, v, step) : angle)) read_angle t

let angle_trig_function t =
  if Cursor.looking_at_func "asin" t then Some (`Asin, "asin")
  else if Cursor.looking_at_func "acos" t then Some (`Acos, "acos")
  else if Cursor.looking_at_func "atan" t then Some (`Atan, "atan")
  else None

let read_angle_trig kind name t =
  let typed t =
    let snapshot = Cursor.save t in
    let degrees =
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
          radians *. 180. /. Float.pi)
    in
    if Float.is_nan degrees then (
      (* CSS Values 4 (ED) sec. 10.9.2: NaN lives inside a calculation tree and
         nowhere else, so it has no leaf spelling. Re-read the call and keep the
         function the author wrote. *)
      Cursor.restore t snapshot;
      let arg = read_math_call_arg name t in
      let fn : math_fn =
        match kind with
        | `Asin -> Asin arg
        | `Acos -> Acos arg
        | `Atan -> Atan arg
      in
      (Calc (Math_fn fn) : angle))
    else (Deg (round_computed degrees) : angle)
  in
  match Cursor.try_typed_call typed t with
  | Ok value -> value
  | Error comp -> Invalid [ comp ]

let read_angle_atan2 t =
  let typed t =
    Cursor.call "atan2" t (fun inner ->
        (* CSS Values 4 sec. 10.4: atan2(y, x) accepts <number>|<dimension>|
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
            (Deg (round_computed (Float.atan2 yv xv *. 180. /. Float.pi))
              : angle)
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
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Value (read_angle_with ~unitless_zero) t)
  else if Cursor.looking_at_func "round" t then
    read_angle_round (read_angle_with ~unitless_zero) t
  else if Cursor.looking_at_func "rem" t then
    read_binary_call "rem"
      (fun a b -> (Rem (a, b) : angle))
      (read_angle_with ~unitless_zero)
      t
  else if Cursor.looking_at_func "mod" t then
    read_binary_call "mod"
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

let duration_css_wide =
  [
    ("inherit", (Inherit : duration));
    ("initial", Initial);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

(* CSS Values 4 (ED) sec. 6.2: "the unit may be omitted [...] only for zero
   lengths", so a bare [0] is not a time. Reading one as [0s] turned input a
   browser refuses into a declaration that works. *)
let read_duration_number ~allow_negative t : duration =
  let n, unit_raw = Cursor.number_with_unit t in
  if (not allow_negative) && n < 0.0 then
    Cursor.err_invalid t "negative durations are not allowed"
  else
    let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
    match unit with
    | "s" -> S n
    | "ms" -> Ms n
    | "" -> Cursor.err_invalid t "a time needs a unit, [0] included"
    | _ -> Cursor.err_invalid t ("duration unit: " ^ unit)

let read_time_number t : duration =
  let n, unit_raw = Cursor.number_with_unit t in
  let unit = String.lowercase_ascii (Option.value unit_raw ~default:"") in
  match unit with
  | "s" -> S n
  | "ms" -> Ms n
  | "" -> Cursor.err_invalid t "a time needs a unit, [0] included"
  | _ -> Cursor.err_invalid t ("time unit: " ^ unit)

let read_duration_round read_duration_self t =
  read_round_call
    (fun s v step -> (Round (s, v, step) : duration))
    read_duration_self t

let rec read_duration_with ?(css_wide = true) ?(allow_negative = false)
    ~canonicalize_ms t : duration =
  (* CSS Values 4 sec. 10.8 gives a math operand no keyword, and sec. 10.12
     checks the property's range on the value the whole calculation resolves to
     rather than on each operand: whether a negative one stands is the caller's
     [allow_negative], not the operand reader's. *)
  let read_operand t =
    read_duration_with ~css_wide:false ~allow_negative ~canonicalize_ms:false t
  in
  Cursor.enum_or_calls
    ~default:(read_duration_number ~allow_negative)
    "duration"
    (if css_wide then duration_css_wide else [])
    ~calls:
      [
        ( "var",
          fun t ->
            Var
              (read_var
                 (read_duration_with ~css_wide ~allow_negative ~canonicalize_ms)
                 t) );
        ("calc", fun t -> Calc (read_calc ~result_type:`Value read_operand t));
        ("round", read_duration_round read_operand);
        ( "rem",
          read_binary_call "rem"
            (fun a b -> (Rem (a, b) : duration))
            read_operand );
        ( "mod",
          read_binary_call "mod"
            (fun a b -> (Mod (a, b) : duration))
            read_operand );
      ]
    t

(** Read a duration value *)
let read_duration t : duration = read_duration_with ~canonicalize_ms:true t

let read_duration_preserve_ms ?allow_negative t : duration =
  read_duration_with ?allow_negative ~canonicalize_ms:false t

(** Read a time value that can be negative (for animation-delay,
    transition-delay) *)
let rec read_time_with ?(css_wide = true) t : duration =
  Cursor.enum_or_calls ~default:read_time_number "time"
    (if css_wide then duration_css_wide else [])
    ~calls:
      [
        ("var", fun t -> Var (read_var read_time t));
        ( "calc",
          fun t -> Calc (read_calc ~result_type:`Value read_time_in_calc t) );
        ("round", read_duration_round read_time);
        ( "rem",
          read_binary_call "rem" (fun a b -> (Rem (a, b) : duration)) read_time
        );
        ( "mod",
          read_binary_call "mod" (fun a b -> (Mod (a, b) : duration)) read_time
        );
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

let read_angle_number_function name make t =
  Cursor.call name t (fun inner ->
      let value = read_angle inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      make value)

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
      | "calc" ->
          let read_number_dim_only t = Cursor.err t "expected numeric factor" in
          Some (Calc (read_calc ~result_type:`Number read_number_dim_only t))
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
      (* CSS Values 4 sec. 10.6: [sign(A)] answers a [<number>] whatever A's own
         type was, so [Sign] holds only the [<number>] case and an argument of
         another type reads as the untyped math node the calc grammar carries.
         [sign(-1px)] is a number the way [calc(sign(-1px))] is. *)
      | "sign" ->
          Some
            (Cursor.one_of
               [
                 (fun t ->
                   read_unary_number_function "sign"
                     (fun value -> (Sign value : number))
                     t);
                 (fun t -> (Calc (read_calc_numeric_function t) : number));
               ]
               t)
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
  read_round_call (fun s v step -> (Round (s, v, step) : number)) read_number t

and read_binary_number_function name make t =
  read_binary_call name make read_number t

and read_unary_number_function name make t =
  Cursor.call name t (fun inner ->
      let value = read_number inner in
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
  else if Cursor.looking_at_calc t then
    Calc (read_calc ~result_type:`Value read_percentage t)
  else Pct (Cursor.pct t)

(* CSS Values 4 sec. 10.4: math functions that produce a non-length type ([asin]
   / [acos] / [atan] / [atan2] return angles, [sin] / [cos] / [tan] return
   numbers). Used in a [<length-percentage>] context they are spec-invalid;
   lightning et al. preserve verbatim, so cascade captures the original call as
   [Invalid [<call>]]. *)
let length_invalid_function_name name =
  match String.lowercase_ascii name with
  | "asin" | "acos" | "atan" | "atan2" | "sin" | "cos" | "tan" -> true
  | _ -> false

let read_invalid_length_percentage_function t : length_percentage =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ } as comp)
    when length_invalid_function_name name ->
      Cursor.skip t;
      Invalid [ comp ]
  | _ -> Cursor.err t "expected invalid length-percentage function"

let read_length_percentage_pct ~allow_negative t : length_percentage =
  let n = Cursor.pct t in
  if (not allow_negative) && n < 0.0 then Cursor.err_invalid t "negative";
  Pct n

let read_length_percentage_length ~allow_negative ~with_keywords ~sizing t :
    length_percentage =
  Length (read_length ~allow_negative ~with_keywords ~sizing t)

(** Read length_percentage value *)
let rec read_length_percentage ?(allow_negative = true) ?(with_keywords = true)
    ?(sizing = false) t : length_percentage =
  Cursor.ws t;
  Cursor.one_of
    [
      read_length_percentage_var ~allow_negative ~with_keywords;
      read_length_percentage_env ~allow_negative ~with_keywords;
      read_length_percentage_calc;
      read_invalid_length_percentage_function;
      read_length_percentage_pct ~allow_negative;
      read_length_percentage_length ~allow_negative ~with_keywords ~sizing;
    ]
    t

and read_length_percentage_var ~allow_negative ~with_keywords t :
    length_percentage =
  if Cursor.looking_at t "var(" then
    Var (read_var (read_length_percentage ~allow_negative ~with_keywords) t)
  else Cursor.err t "expected var"

and read_length_percentage_env ~allow_negative ~with_keywords t :
    length_percentage =
  if Cursor.looking_at t "env(" then
    Env (read_env (read_length_percentage ~allow_negative ~with_keywords) t)
  else Cursor.err t "expected env"

and read_length_percentage_calc t : length_percentage =
  if Cursor.looking_at_calc t then
    (* CSS Values 4 10 (calc): inside [calc()] negative operands are always
       allowed even when the surrounding property is non-negative; the
       non-negative constraint applies to the resolved value, not to inner
       operands. Sec. 10.8 gives an operand no keyword, so the leaf reads with
       the keyword grammar off. *)
    Calc
      (read_calc ~result_type:`Value
         (read_length_percentage ~with_keywords:false)
         t)
  else Cursor.err t "expected calc"

(** Read number_percentage value. Inside a [<number-percentage>] [calc()], a raw
    number is modelled at the calc level as the [Num x] node rather than
    [Val (Num x)] (the [Num] sub-variant of [number_percentage] wrapped in
    [Val]); the dedicated [_dim_only] reader excludes the raw-number alternative
    so the generic [read_calc_factor] falls through to its own [Num] path. *)
let rec read_number_percentage_dim_only t : number_percentage =
  Cursor.ws t;
  Cursor.one_of
    [
      (fun t -> (Pct (Cursor.pct t) : number_percentage));
      (fun t ->
        (Var (read_var read_number_percentage_dim_only t) : number_percentage));
    ]
    t

let rec read_number_percentage t : number_percentage =
  Cursor.ws t;
  if Cursor.looking_at t "var(" then Var (read_var read_number_percentage t)
  else if Cursor.looking_at_calc t || looking_at_math_function t then
    (* CSS Values 4 sec. 10.1 puts every math function where [calc()] stands,
       and sec. 10.12 checks the property's range on what the call resolves to,
       so the call is held rather than folded to a literal the reader would then
       refuse. Sec. 10.9 admits only what the slot's two types name: a call
       answering a [<length>] is neither. *)
    Calc
      (read_calc ~result_type:`Number_or_percentage
         read_number_percentage_dim_only t)
  else
    (* Try to read as percentage or number *)
    Cursor.one_of
      [
        (fun t -> (Pct (Cursor.pct t) : number_percentage));
        (fun t -> (Num (Cursor.number t) : number_percentage));
      ]
      t

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

(* CSS Box 4 sec. 3.1 gives a margin [<length-percentage> | auto], so no sizing
   function reaches it. [global] adds the CSS-wide keywords of CSS Cascade 5
   sec. 7.3, which a longhand takes as its whole value and a shorthand component
   cannot; a [var()] fallback stands for a whole value, so it takes them too. *)

(** Read margin shorthand property (1-4 values). CSS Box 4 (ED) sec. 3.2 gives
    [margin] the value [<'margin-top'>{1,4}] and sec. 3.1 gives [margin-top] the
    value [<length-percentage> | auto], so [auto] is a component of the box and
    stands in any slot beside any length. Only the CSS-wide keywords of CSS
    Cascade 5 sec. 6 own the whole value. *)
let rec read_margin_length ?(global = false) t : length =
  if Cursor.looking_at_func "var" t then
    Var (read_var (read_margin_length ~global:true) t)
  else
    Cursor.enum "margin component"
      (("auto", (Auto : length))
      ::
      (if global then
         [
           ("inherit", (Inherit : length));
           ("initial", Initial);
           ("unset", Unset);
           ("revert", Revert);
           ("revert-layer", Revert_layer);
         ]
       else []))
      ~default:(read_length ~with_keywords:false)
      t

let read_margin_shorthand t : length list =
  let read_margin_component = read_margin_length in
  Cursor.enum "margin"
    [
      ("inherit", [ (Inherit : length) ]);
      ("initial", [ Initial ]);
      ("unset", [ Unset ]);
      ("revert", [ Revert ]);
      ("revert-layer", [ Revert_layer ]);
    ]
    ~default:(fun t ->
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4 read_margin_component t)
    t
