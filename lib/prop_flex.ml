(* css-flexbox-1: the flex container and item properties. flex-direction,
   flex-wrap, flex-flow, the flex shorthand and its grow/shrink/basis longhands,
   and order.

   The -webkit- aliases (-webkit-flex-direction, -webkit-flex-wrap,
   -webkit-flex-flow) reuse these printers and readers verbatim; only their
   dispatch arms live in properties.ml.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Values
open Properties_intf

let rec read_flex_direction t : flex_direction =
  Cursor.enum_or_var "flex-direction"
    [
      ("row", (Row : flex_direction));
      ("row-reverse", Row_reverse);
      ("column", Column);
      ("column-reverse", Column_reverse);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_flex_direction t))
    t

let rec numeric_flex_factor_calc_leaves : flex_factor calc -> flex_factor calc =
  function
  | Val (Number n) -> Num n
  | Nested inner -> Nested (numeric_flex_factor_calc_leaves inner)
  | Parens inner -> Parens (numeric_flex_factor_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_flex_factor_calc_leaves left,
          op,
          numeric_flex_factor_calc_leaves right )
  | other -> other

let rec normalize_flex_factor (value : flex_factor) : flex_factor =
  match value with
  | Calc c -> (
      match eval_calc (numeric_flex_factor_calc_leaves c) with
      | Num f -> Number f
      | Val v -> normalize_flex_factor v
      | folded -> if folded == c then value else Calc folded)
  | _ -> value

let rec normalize_flex_basis (value : flex_basis) : flex_basis =
  match value with
  | Px 0.
  | Cm 0.
  | Mm 0.
  | Q 0.
  | In 0.
  | Pt 0.
  | Pc 0.
  | Rem 0.
  | Em 0.
  | Ex 0.
  | Cap 0.
  | Ic 0.
  | Ric 0.
  | Rlh 0.
  | Vw 0.
  | Vh 0.
  | Vmin 0.
  | Vmax 0.
  | Vi 0.
  | Vb 0.
  | Dvh 0.
  | Dvw 0.
  | Dvmin 0.
  | Dvmax 0.
  | Lvh 0.
  | Lvw 0.
  | Lvmin 0.
  | Lvmax 0.
  | Svh 0.
  | Svw 0.
  | Svmin 0.
  | Svmax 0.
  | Ch 0.
  | Lh 0. ->
      Zero
  | Calc c -> (
      match eval_calc c with
      | Val v -> normalize_flex_basis v
      | folded -> if folded == c then value else Calc folded)
  | _ -> value

let normalize_flex (value : flex) : flex =
  match value with
  | Grow f ->
      let f' = normalize_flex_factor f in
      if f' == f then value else Grow f'
  | Basis b ->
      let b' = normalize_flex_basis b in
      if b' == b then value else Basis b'
  | Grow_shrink (g, s) ->
      let g' = normalize_flex_factor g in
      let s' = normalize_flex_factor s in
      if g' == g && s' == s then value else Grow_shrink (g', s')
  | Full (g, s, b) -> (
      let g' = normalize_flex_factor g in
      let s' = normalize_flex_factor s in
      let b' = normalize_flex_basis b in
      (* CSS Flexbox 1 sec. 7.1.1 gives [none] and [auto] as the one-word names
         of [0 0 auto] and [1 1 auto], and the shorter spelling wins. *)
      match (g', s', b') with
      | Number 0., Number 0., (Auto : flex_basis) -> None
      | Number 1., Number 1., Auto -> Auto
      | _ -> if g' == g && s' == s && b' == b then value else Full (g', s', b'))
  | _ -> value

let rec pp_order : order Pp.t =
 fun ctx -> function
  | Int i -> Pp.int ctx i
  | Calc c -> pp_calc pp_order ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_order ctx v

let rec pp_flex_direction : flex_direction Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_direction ctx v
  | Row -> Pp.string ctx "row"
  | Row_reverse -> Pp.string ctx "row-reverse"
  | Column -> Pp.string ctx "column"
  | Column_reverse -> Pp.string ctx "column-reverse"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_flex_wrap : flex_wrap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_wrap ctx v
  | Nowrap -> Pp.string ctx "nowrap"
  | Wrap -> Pp.string ctx "wrap"
  | Wrap_reverse -> Pp.string ctx "wrap-reverse"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_flex_flow : flex_flow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_flow ctx v
  | Flow (direction, wrap) -> (
      match (direction, wrap) with
      | Some direction, Some wrap ->
          pp_flex_direction ctx direction;
          Pp.space ctx ();
          pp_flex_wrap ctx wrap
      | Some direction, None -> pp_flex_direction ctx direction
      | None, Some wrap -> pp_flex_wrap ctx wrap
      | None, None -> Pp.string ctx "row")
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* CSS Flexbox 1 sec. 5.1: a component left out takes its longhand's initial -
   [row] for the direction (sec. 5.1) and [nowrap] for the wrap (sec. 5.2) - so
   writing one out names what leaving it out names, and the shorter spelling
   wins. Drained of both, the printer says [row], which names the same pair. *)
let normalize_flex_flow : flex_flow -> flex_flow =
 fun value ->
  match value with
  | Flow (direction, wrap) ->
      let direction =
        match direction with
        | Some (Row : flex_direction) -> Option.None
        | direction -> direction
      in
      let wrap =
        match wrap with
        | Some (Nowrap : flex_wrap) -> Option.None
        | wrap -> wrap
      in
      Flow (direction, wrap)
  | other -> other

let rec pp_flex_factor : flex_factor Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex_factor ctx v
  | Number value -> Pp.float ctx value
  | Calc c -> pp_calc pp_flex_factor ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_flex_basis : flex_basis Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Content -> Pp.string ctx "content"
  | Px f -> Pp.unit ctx f "px"
  | Cm f -> Pp.unit ctx f "cm"
  | Mm f -> Pp.unit ctx f "mm"
  | Q f -> Pp.unit ctx f "q"
  | In f -> Pp.unit ctx f "in"
  | Pt f -> Pp.unit ctx f "pt"
  | Pc f -> Pp.unit ctx f "pc"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Ex f -> Pp.unit ctx f "ex"
  | Cap f -> Pp.unit ctx f "cap"
  | Ic f -> Pp.unit ctx f "ic"
  | Ric f -> Pp.unit ctx f "ric"
  | Rlh f -> Pp.unit ctx f "rlh"
  | Pct f -> Pp.pct ctx f
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Vi f -> Pp.unit ctx f "vi"
  | Vb f -> Pp.unit ctx f "vb"
  | Dvh f -> Pp.unit ctx f "dvh"
  | Dvw f -> Pp.unit ctx f "dvw"
  | Dvmin f -> Pp.unit ctx f "dvmin"
  | Dvmax f -> Pp.unit ctx f "dvmax"
  | Lvh f -> Pp.unit ctx f "lvh"
  | Lvw f -> Pp.unit ctx f "lvw"
  | Lvmin f -> Pp.unit ctx f "lvmin"
  | Lvmax f -> Pp.unit ctx f "lvmax"
  | Svh f -> Pp.unit ctx f "svh"
  | Svw f -> Pp.unit ctx f "svw"
  | Svmin f -> Pp.unit ctx f "svmin"
  | Svmax f -> Pp.unit ctx f "svmax"
  | Ch f -> Pp.unit ctx f "ch"
  | Lh f -> Pp.unit ctx f "lh"
  | Num f -> Pp.float ctx f
  | Zero -> Pp.char ctx '0'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Fit_content -> Pp.string ctx "fit-content"
  | Fit_content_arg length ->
      Pp.string ctx "fit-content(";
      pp_length ctx length;
      Pp.char ctx ')'
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  (* Math functions mirror [length]; reuse its printer. *)
  | Clamp (a, b, c) -> pp_length ctx (Clamp (a, b, c))
  | Min xs -> pp_length ctx (Min xs)
  | Max xs -> pp_length ctx (Max xs)
  | Round (s, a, b) -> pp_length ctx (Round (s, a, b))
  | Mod (a, b) -> pp_length ctx (Mod (a, b))
  | Rem_fn (a, b) -> pp_length ctx (Rem_fn (a, b))
  | Hypot xs -> pp_length ctx (Hypot xs)
  | Abs a -> pp_length ctx (Abs a)
  | Var v -> pp_var pp_flex_basis ctx v
  | Calc cv -> pp_calc pp_flex_basis ctx cv

let rec pp_flex : flex Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_flex ctx v
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Auto -> Pp.string ctx "auto"
  | None -> Pp.string ctx "none"
  | Grow f -> pp_flex_factor ctx f
  | Basis fb -> pp_flex_basis ctx fb
  | Grow_shrink (grow, shrink) ->
      pp_flex_factor ctx grow;
      Pp.space ctx ();
      pp_flex_factor ctx shrink
  | Full (grow, shrink, basis) ->
      pp_flex_factor ctx grow;
      (* CSS Flexbox 1 sec. 7.1.1: the one-number [flex: g] form expands to [g 1
         0%], so only a [0%] basis (with the default [1] shrink) is the
         droppable shorthand default. A length [0] / [0px] basis is a different
         computed value and is kept. An omitted flex-shrink is [1]. *)
      let basis_is_default = match basis with Pct 0.0 -> true | _ -> false in
      let shrink_is_default =
        match shrink with Number 1.0 -> true | _ -> false
      in
      if basis_is_default then (
        if not shrink_is_default then (
          Pp.space ctx ();
          pp_flex_factor ctx shrink))
      else (
        Pp.space ctx ();
        (* A basis that serialises as a bare number ([0], [0px] -> [0], or a
           unitless basis) would reparse as the shrink factor, so keep the
           shrink to disambiguate. *)
        let basis_bare_number =
          match basis with Num _ | Zero | Px 0.0 -> true | _ -> false
        in
        if (not shrink_is_default) || basis_bare_number then (
          pp_flex_factor ctx shrink;
          Pp.space ctx ());
        pp_flex_basis ctx basis)

let rec read_order t : order =
  let read_calc_order t =
    match read_integer_calc "order" t with
    | `Int n -> (Int n : order)
    | `Calc expr -> (Calc expr : order)
  in
  let read_var t : order = Var (read_var read_order t) in
  Cursor.enum_or_calls "order"
    [
      ("inherit", (Inherit : order));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("calc", read_calc_order); ("var", read_var) ]
    ~default:(fun t -> (Int (Cursor.int t) : order))
    t

let rec read_flex_wrap t : flex_wrap =
  Cursor.enum_or_var "flex-wrap"
    [
      ("nowrap", (Nowrap : flex_wrap));
      ("wrap", Wrap);
      ("wrap-reverse", Wrap_reverse);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_flex_wrap t))
    t

let read_flex_flow_direction t : flex_direction =
  Cursor.enum "flex-flow direction"
    [
      ("row", (Row : flex_direction));
      ("row-reverse", Row_reverse);
      ("column", Column);
      ("column-reverse", Column_reverse);
    ]
    t

let read_flex_flow_wrap t : flex_wrap =
  Cursor.enum "flex-flow wrap"
    [
      ("nowrap", (Nowrap : flex_wrap));
      ("wrap", Wrap);
      ("wrap-reverse", Wrap_reverse);
    ]
    t

let read_flex_flow_part (direction : flex_direction option ref)
    (wrap : flex_wrap option ref) t =
  match (!direction, Cursor.option read_flex_flow_direction t) with
  | None, Some value ->
      direction := Some value;
      true
  | Some _, Some _ -> Cursor.err_invalid t "duplicate flex direction"
  | _, None -> (
      match (!wrap, Cursor.option read_flex_flow_wrap t) with
      | None, Some value ->
          wrap := Some value;
          true
      | Some _, Some _ -> Cursor.err_invalid t "duplicate flex wrap"
      | _, None -> Cursor.err_expected t "flex-flow")

let rec read_flex_flow t : flex_flow =
  Cursor.enum_or_var "flex-flow"
    [
      ("inherit", (Inherit : flex_flow));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_flex_flow t))
    ~default:(fun t ->
      let direction : flex_direction option ref =
        ref (None : flex_direction option)
      in
      let wrap : flex_wrap option ref = ref (None : flex_wrap option) in
      let seen = ref false in
      (* CSS Flexbox 1 sec. 5.3: [flex-flow] is at most two values
         ([flex-direction] || [flex-wrap]). Stop once both slots are filled, and
         break on the first iteration where neither matches instead of letting
         [read_flex_flow_part] raise on the trailing [;] / [!important]. *)
      let rec loop () =
        Cursor.ws t;
        if Cursor.is_done t then ()
        else if Option.is_some !direction && Option.is_some !wrap then ()
        else
          let before = Cursor.save t in
          match read_flex_flow_part direction wrap t with
          | true ->
              seen := true;
              loop ()
          | false -> Cursor.restore t before
          | exception Cursor.Parse_error _ -> Cursor.restore t before
      in
      loop ();
      if not !seen then Cursor.err_expected t "flex-flow";
      Flow (!direction, !wrap))
    t

let read_non_negative_flex_number t =
  let value =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Number_tok _; _ }) -> (
        let n, unit = Cursor.number_with_unit t in
        match unit with
        | None -> n
        | Some u -> Cursor.err_invalid t ("flex factor unit: " ^ u))
    | _ -> (
        match (Values.read_number t : Values.number) with
        | Values.Num value -> value
        | _ -> Cursor.err_invalid t "flex number must resolve to a number")
  in
  if value < 0. then Cursor.err_invalid t "negative number not allowed";
  value

let rec read_flex_factor t : flex_factor =
  let read_number t =
    (Number (read_non_negative_flex_number t) : flex_factor)
  in
  Cursor.enum_or_calls "flex factor"
    [
      ("inherit", (Inherit : flex_factor));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (Values.read_var read_flex_factor t));
        ( "calc",
          fun t -> Calc (read_calc ~result_type:`Number read_flex_factor t) );
      ]
    ~default:read_number t

let flex_basis_of_length t (length : length) : flex_basis =
  match length with
  | Px n -> Px n
  | Rem n -> Rem n
  | Em n -> Em n
  | Ex n -> Ex n
  | Pct n -> Pct n
  | Cm n -> Cm n
  | Mm n -> Mm n
  | Q n -> Q n
  | In n -> In n
  | Pt n -> Pt n
  | Pc n -> Pc n
  | Cap n -> Cap n
  | Ic n -> Ic n
  | Ric n -> Ric n
  | Rlh n -> Rlh n
  | Vw n -> Vw n
  | Vh n -> Vh n
  | Vmin n -> Vmin n
  | Vmax n -> Vmax n
  | Vi n -> Vi n
  | Vb n -> Vb n
  | Dvh n -> Dvh n
  | Dvw n -> Dvw n
  | Dvmin n -> Dvmin n
  | Dvmax n -> Dvmax n
  | Lvh n -> Lvh n
  | Lvw n -> Lvw n
  | Lvmin n -> Lvmin n
  | Lvmax n -> Lvmax n
  | Svh n -> Svh n
  | Svw n -> Svw n
  | Svmin n -> Svmin n
  | Svmax n -> Svmax n
  | Ch n -> Ch n
  | Lh n -> Lh n
  | Zero -> Zero
  | Fit_content -> Fit_content
  | Fit_content_arg arg -> (Fit_content_arg arg : flex_basis)
  | Max_content -> Max_content
  | Min_content -> Min_content
  | Inherit -> Inherit
  | Initial -> Initial
  | Unset -> Unset
  | Revert -> Revert
  | Revert_layer -> Revert_layer
  (* [read_length_unit] wraps a known-unit zero in [Dimension { unit; repr }] to
     preserve the authored spelling. Map back to the typed [flex_basis] form so
     [flex-basis: 0px] / [flex-basis: 0%] type-check. CSS Flexbox 1 sec. 7.2:
     [flex-basis] accepts [<'width'>] which accepts [<length-percentage>]. *)
  | Dimension { value = 0.; unit = "%"; _ } -> Pct 0.
  | Dimension { value = 0.; unit = "px"; _ } -> Px 0.
  | Dimension { value = 0.; unit = "cm"; _ } -> Cm 0.
  | Dimension { value = 0.; unit = "mm"; _ } -> Mm 0.
  | Dimension { value = 0.; unit = "q"; _ } -> Q 0.
  | Dimension { value = 0.; unit = "in"; _ } -> In 0.
  | Dimension { value = 0.; unit = "pt"; _ } -> Pt 0.
  | Dimension { value = 0.; unit = "pc"; _ } -> Pc 0.
  | Dimension { value = 0.; unit = "rem"; _ } -> Rem 0.
  | Dimension { value = 0.; unit = "em"; _ } -> Em 0.
  | Dimension { value = 0.; unit = "ex"; _ } -> Ex 0.
  | Dimension { value = 0.; unit = "cap"; _ } -> Cap 0.
  | Dimension { value = 0.; unit = "ic"; _ } -> Ic 0.
  | Dimension { value = 0.; unit = "ric"; _ } -> Ric 0.
  | Dimension { value = 0.; unit = "rlh"; _ } -> Rlh 0.
  | Dimension { value = 0.; unit = "vw"; _ } -> Vw 0.
  | Dimension { value = 0.; unit = "vh"; _ } -> Vh 0.
  | Dimension { value = 0.; unit = "vmin"; _ } -> Vmin 0.
  | Dimension { value = 0.; unit = "vmax"; _ } -> Vmax 0.
  | Dimension { value = 0.; unit = "vi"; _ } -> Vi 0.
  | Dimension { value = 0.; unit = "vb"; _ } -> Vb 0.
  | Dimension { value = 0.; unit = "dvh"; _ } -> Dvh 0.
  | Dimension { value = 0.; unit = "dvw"; _ } -> Dvw 0.
  | Dimension { value = 0.; unit = "dvmin"; _ } -> Dvmin 0.
  | Dimension { value = 0.; unit = "dvmax"; _ } -> Dvmax 0.
  | Dimension { value = 0.; unit = "lvh"; _ } -> Lvh 0.
  | Dimension { value = 0.; unit = "lvw"; _ } -> Lvw 0.
  | Dimension { value = 0.; unit = "lvmin"; _ } -> Lvmin 0.
  | Dimension { value = 0.; unit = "lvmax"; _ } -> Lvmax 0.
  | Dimension { value = 0.; unit = "svh"; _ } -> Svh 0.
  | Dimension { value = 0.; unit = "svw"; _ } -> Svw 0.
  | Dimension { value = 0.; unit = "svmin"; _ } -> Svmin 0.
  | Dimension { value = 0.; unit = "svmax"; _ } -> Svmax 0.
  | Dimension { value = 0.; unit = "ch"; _ } -> Ch 0.
  | Dimension { value = 0.; unit = "lh"; _ } -> Lh 0.
  (* Math functions over <length-percentage> carry across unchanged:
     [flex_basis] mirrors [length]'s constructors. [Sign] (a <number>) and
     [Minmax] (grid only) are not valid here, so they stay rejected. *)
  | Clamp (a, b, c) -> Clamp (a, b, c)
  | Min xs -> Min xs
  | Max xs -> Max xs
  | Round (s, a, b) -> Round (s, a, b)
  | Mod (a, b) -> Mod (a, b)
  | Rem_fn (a, b) -> Rem_fn (a, b)
  | Hypot xs -> Hypot xs
  | Abs a -> Abs a
  | _ -> Cursor.err_invalid t "unsupported flex-basis value"

let rec read_flex_basis t : flex_basis =
  Cursor.enum_or_calls "flex-basis"
    [
      ("auto", (Auto : flex_basis));
      ("content", Content);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (read_var read_flex_basis t));
        ("calc", fun t -> Calc (read_calc ~result_type:`Value read_flex_basis t));
      ]
      (* CSS Flexbox 1 sec. 7.2.3 reads the basis as a [<'width'>], so it takes
         the intrinsic sizes the box sizes take. *)
    ~default:(fun t ->
      let size t =
        read_length ~allow_negative:false ~sizing:true t
        |> flex_basis_of_length t
      in
      let pos = Cursor.save t in
      match Cursor.option Cursor.number_with_unit t with
      | Some (0.0, None) -> (Zero : flex_basis)
      | Some _ ->
          Cursor.restore t pos;
          size t
      | None -> size t)
    t

module Flex = struct
  (* Helper functions for flex parsing *)
  let read_basis_only t = Basis (read_flex_basis t)

  let read_factor t : flex_factor =
    (* A flex factor is a [<number>]: a [var()], a [calc()] (held unfolded; the
       optimize+minify pass folds a constant calc to a literal), or a literal
       number. *)
    if Cursor.looking_at_func "var" t then Var (read_var read_flex_factor t)
    else if Cursor.looking_at_func "calc" t then
      Calc (read_calc ~result_type:`Number read_flex_factor t)
    else Number (read_non_negative_flex_number t)

  let read_grow_shrink_basis t =
    (* Parse grow [shrink] [basis]; a [50%] / [10px] is a basis, not a
       factor. *)
    let grow = read_factor t in
    let shrink =
      Cursor.option
        (fun t ->
          Cursor.ws t;
          read_factor t)
        t
    in
    (* Optional basis (defaults to 0%) *)
    let basis =
      Cursor.option
        (fun t ->
          Cursor.ws t;
          read_flex_basis t)
        t
    in
    match (shrink, basis) with
    | None, None -> Grow grow
    | Some s, None -> Grow_shrink (grow, s)
    | _, Some b -> Full (grow, Option.value shrink ~default:(Number 1.0), b)
end

let rec read_flex t : flex =
  Cursor.enum_or_var "flex"
    [
      ("initial", (Initial : flex));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
      ("auto", Auto);
      ("none", (None : flex));
      ("content", Basis Content);
    ]
    ~var:(fun t ->
      (* A lone var() is the whole value; a var() followed by more components is
         a grow/shrink/basis sequence whose first factor happens to be a var. *)
      let snap = Cursor.save t in
      let v = read_var read_flex t in
      Cursor.ws t;
      if Cursor.is_done t || Cursor.peek_comma t then (Var v : flex)
      else (
        Cursor.restore t snap;
        Cursor.one_of [ Flex.read_grow_shrink_basis; Flex.read_basis_only ] t))
    ~default:
      (Cursor.one_of [ Flex.read_grow_shrink_basis; Flex.read_basis_only ])
    t
