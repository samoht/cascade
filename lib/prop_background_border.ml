open Common
open Values
open Properties_intf
open Prop_common

let rec pp_border_style : border_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Hidden -> Pp.string ctx "hidden"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_style ctx v

let rec pp_border_radius : border_radius Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border_radius ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Radius { horizontal; vertical } -> (
      pp_box_shorthand (pp_length_percentage ~always:true) ctx horizontal;
      match vertical with
      | None -> ()
      | Some vs ->
          Pp.sp ctx ();
          Pp.char ctx '/';
          Pp.sp ctx ();
          pp_box_shorthand (pp_length_percentage ~always:true) ctx vs)

(* CSS Backgrounds 3 (ED) sec. 4.1 puts the vertical radii after a [/], so each
   group collapses on its own and neither reaches across the slash. With no
   slash the values set both axes equally, so a vertical group equal to the
   horizontal one says what omitting it says. *)
let normalize_border_radius ?(strip = true) : border_radius -> border_radius =
 fun value ->
  (* Sec. 4.1 gives each radius a [0,inf] range, and the shorthand carries what
     its longhands carry: a call folding below it keeps its wrapper. *)
  let group =
    normalize_box_shorthand ~is_substitution:is_lp_substitution
      (Values.normalize_length_percentage ~strip ~non_negative:true)
  in
  match value with
  | Radius { horizontal; vertical } ->
      let horizontal = group horizontal in
      let vertical = option_map_preserve group vertical in
      let vertical =
        match vertical with
        | Some vs
          when (not (List.exists is_lp_substitution horizontal))
               && (not (List.exists is_lp_substitution vs))
               && List.equal Values.equal_length_percentage horizontal vs ->
            Option.None
        | _ -> vertical
      in
      preserve_if_equal value (Radius { horizontal; vertical })
  | other -> other

let normalize_logical_border_color ?(lossless = false) :
    logical_border_color -> logical_border_color =
 fun value ->
  match value with
  | Single c -> preserve_if_equal value (Single (normalize_color ~lossless c))
  | Pair (a, b) ->
      let a = normalize_color ~lossless a and b = normalize_color ~lossless b in
      if
        (not (is_color_substitution a))
        && (not (is_color_substitution b))
        && Values.equal_color a b
      then Single a
      else preserve_if_equal value (Pair (a, b))
  | other -> other

let length_of_border_width : border_width -> length option = function
  | Px n -> Some (Px n)
  | Cm n -> Some (Cm n)
  | Mm n -> Some (Mm n)
  | Q n -> Some (Q n)
  | In n -> Some (In n)
  | Pt n -> Some (Pt n)
  | Pc n -> Some (Pc n)
  | Rem n -> Some (Rem n)
  | Em n -> Some (Em n)
  | Ex n -> Some (Ex n)
  | Cap n -> Some (Cap n)
  | Ic n -> Some (Ic n)
  | Ric n -> Some (Ric n)
  | Rlh n -> Some (Rlh n)
  | Ch n -> Some (Ch n)
  | Lh n -> Some (Lh n)
  | Vh n -> Some (Vh n)
  | Vw n -> Some (Vw n)
  | Vmin n -> Some (Vmin n)
  | Vmax n -> Some (Vmax n)
  | Pct n -> Some (Pct n)
  | Dimension { value; unit; repr } -> Some (Dimension { value; unit; repr })
  | Zero -> Some Zero
  | _ -> None

let length_of_border_width_calc calc = map_calc_opt length_of_border_width calc

(* The reverse of [length_of_border_width]: every dimension the two types share
   round-trips, so a fold done in [length] space (the [min()]/[max()]/ [clamp()]
   reduction, [Values.normalize_length]) can rebuild a [border_width]
   afterwards. *)
let border_width_of_length : length -> border_width option = function
  | Px n -> Some (Px n)
  | Cm n -> Some (Cm n)
  | Mm n -> Some (Mm n)
  | Q n -> Some (Q n)
  | In n -> Some (In n)
  | Pt n -> Some (Pt n)
  | Pc n -> Some (Pc n)
  | Rem n -> Some (Rem n)
  | Em n -> Some (Em n)
  | Ex n -> Some (Ex n)
  | Cap n -> Some (Cap n)
  | Ic n -> Some (Ic n)
  | Ric n -> Some (Ric n)
  | Rlh n -> Some (Rlh n)
  | Ch n -> Some (Ch n)
  | Lh n -> Some (Lh n)
  | Vh n -> Some (Vh n)
  | Vw n -> Some (Vw n)
  | Vmin n -> Some (Vmin n)
  | Vmax n -> Some (Vmax n)
  | Pct n -> Some (Pct n)
  | Dimension { value; unit; repr } -> Some (Dimension { value; unit; repr })
  | Zero -> Some Zero
  | _ -> None

let border_width_of_length_calc calc = map_calc_opt border_width_of_length calc

let length_linear_term : length -> (string * float * (float -> length)) option =
  function
  | Zero -> Some ("px", 0., fun _ -> Zero)
  | Px n -> Some ("px", n, fun n -> if n = 0. then Zero else Px n)
  | Cm n -> Some ("cm", n, fun n -> Cm n)
  | Mm n -> Some ("mm", n, fun n -> Mm n)
  | Q n -> Some ("q", n, fun n -> Q n)
  | In n -> Some ("in", n, fun n -> In n)
  | Pt n -> Some ("pt", n, fun n -> Pt n)
  | Pc n -> Some ("pc", n, fun n -> Pc n)
  | Rem n -> Some ("rem", n, fun n -> Rem n)
  | Em n -> Some ("em", n, fun n -> Em n)
  | Ex n -> Some ("ex", n, fun n -> Ex n)
  | Cap n -> Some ("cap", n, fun n -> Cap n)
  | Ic n -> Some ("ic", n, fun n -> Ic n)
  | Ric n -> Some ("ric", n, fun n -> Ric n)
  | Rlh n -> Some ("rlh", n, fun n -> Rlh n)
  | Ch n -> Some ("ch", n, fun n -> Ch n)
  | Lh n -> Some ("lh", n, fun n -> Lh n)
  | Pct n -> Some ("%", n, fun n -> Pct n)
  | Vw n -> Some ("vw", n, fun n -> Vw n)
  | Vh n -> Some ("vh", n, fun n -> Vh n)
  | Vmin n -> Some ("vmin", n, fun n -> Vmin n)
  | Vmax n -> Some ("vmax", n, fun n -> Vmax n)
  | _ -> None

let add_border_width_term
    (table : (string, int * float * (float -> length)) Hashtbl.t) pos
    (key, n, make) =
  match Hashtbl.find_opt table key with
  | None -> Hashtbl.add table key (pos, n, make)
  | Some (old_pos, old_n, old_make) ->
      Hashtbl.replace table key (old_pos, old_n +. n, old_make)

let collect_border_width_terms
    (terms : (string * float * (float -> length)) list) =
  let table = Hashtbl.create 4 in
  List.iteri (add_border_width_term table) terms;
  Hashtbl.to_seq_values table
  |> List.of_seq
  |> List.filter (fun (_, n, _) -> n <> 0.)
  |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)

let append_border_width_term (acc : length calc)
    ((_, n, make) : int * float * (float -> length)) : length calc =
  if n < 0. then Expr (acc, Sub, Val (make (-.n)))
  else Expr (acc, Add, Val (make n))

let rebuild_border_width_length_terms
    (terms : (string * float * (float -> length)) list) : length calc =
  match collect_border_width_terms terms with
  | [] -> Val Zero
  | [ (_, n, make) ] -> Val (make n)
  | (_, n, make) :: rest ->
      List.fold_left append_border_width_term (Val (make n)) rest

let simplify_border_width_length_calc calc =
  let scale factor terms =
    List.map (fun (key, n, make) -> (key, factor *. n, make)) terms
  in
  let rec terms = function
    | Val length ->
        Option.map (fun term -> [ term ]) (length_linear_term length)
    | Num 0. -> Some []
    | Num _ | Math_const _ | Var _ | Sibling_index | Sibling_count | Math_fn _
      ->
        None
    | Nested inner | Parens inner -> terms inner
    | Expr (left, Add, right) -> (
        match (terms left, terms right) with
        | Some left, Some right -> Some (left @ right)
        | _ -> None)
    | Expr (left, Sub, right) -> (
        match (terms left, terms right) with
        | Some left, Some right -> Some (left @ scale (-1.) right)
        | _ -> None)
    | Expr (left, Mul, Num n) -> Option.map (scale n) (terms left)
    | Expr (Num n, Mul, right) -> Option.map (scale n) (terms right)
    | Expr (left, Div, Num n) when n <> 0. ->
        Option.map (scale (1. /. n)) (terms left)
    | Expr _ -> None
  in
  match terms calc with
  | None -> calc
  | Some terms -> rebuild_border_width_length_terms terms

let simplified_border_width_length calc =
  Option.map simplify_border_width_length_calc
    (length_of_border_width_calc calc)

let pp_length_calc_op = pp_calc_op

let pp_length_calc_contents ctx calc =
  let precedence (op : calc_op) =
    match op with Add | Sub -> 1 | Mul | Div -> 2
  in
  let rec pp_inner ~parent_prec ~right_of_noncommut ctx = function
    | Val length -> pp_length ctx length
    | Num n -> Pp.float ctx n
    | (Var _ | Sibling_index | Sibling_count | Math_const _ | Math_fn _) as calc
      ->
        pp_calc pp_length ctx calc
    | Nested inner ->
        Pp.call "calc"
          (pp_inner ~parent_prec:0 ~right_of_noncommut:false)
          ctx inner
    | Parens inner ->
        Pp.char ctx '(';
        pp_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner;
        Pp.char ctx ')'
    | Expr (left, op, right) ->
        let op_prec = precedence op in
        let needs_parens =
          op_prec < parent_prec || (right_of_noncommut && op_prec <= parent_prec)
        in
        if needs_parens then Pp.char ctx '(';
        pp_inner ~parent_prec:op_prec ~right_of_noncommut:false ctx left;
        pp_length_calc_op ctx op;
        let is_noncommut = match op with Sub | Div -> true | _ -> false in
        pp_inner ~parent_prec:op_prec ~right_of_noncommut:is_noncommut ctx right;
        if needs_parens then Pp.char ctx ')'
  in
  pp_inner ~parent_prec:0 ~right_of_noncommut:false ctx calc

(* Two arguments of a [min()] / [max()] compare only when they share a unit. CSS
   Values 4 sec. 6.2 puts the absolute units on one scale, so they share the
   [px] key; every other unit compares only with itself. *)
let border_width_length_measure length : (string * float) option =
  match Values.calc_length_unit length with
  | None -> None
  | Some (unit, n) -> (
      match Values.absolute_unit_px_ratio unit with
      | Some ratio -> Some ("px", n *. ratio)
      | None -> Some (unit, n))

let border_width_calc_length = function Val length -> Some length | _ -> None

let record_border_width_group kind groups pos (length, key, n) =
  let keep =
    match Hashtbl.find_opt groups key with
    | None -> (pos, length, n)
    | Some (old_pos, old_length, old_n) ->
        let better = match kind with `Min -> n < old_n | `Max -> n > old_n in
        if better then (old_pos, length, n) else (old_pos, old_length, old_n)
  in
  Hashtbl.replace groups key keep

(* Grouping keeps one argument per unit, so an argument no unit measures has to
   stop the reduction: dropping it would delete a bound from the function. *)
let measured_border_width_lengths lengths =
  List.fold_right
    (fun length acc ->
      match (border_width_length_measure length, acc) with
      | Some (key, n), Some acc -> Some ((length, key, n) :: acc)
      | _ -> None)
    lengths (Some [])

let reduce_border_width_minmax kind args : length calc list option =
  let simplified = List.map simplified_border_width_length args in
  if List.exists Option.is_none simplified then None
  else
    let vals = List.map Option.get simplified in
    match List.map border_width_calc_length vals with
    | lengths when List.exists Option.is_none lengths -> None
    | lengths -> (
        match measured_border_width_lengths (List.map Option.get lengths) with
        | None -> None
        | Some measured -> (
            let groups = Hashtbl.create 4 in
            List.iteri (record_border_width_group kind groups) measured;
            let reduced =
              Hashtbl.to_seq_values groups
              |> List.of_seq
              |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
              |> List.map (fun (_, length, _) -> Val length)
            in
            match reduced with [] -> None | _ -> Some reduced))

let reduce_border_width_clamp lower value upper =
  match
    ( simplified_border_width_length lower,
      simplified_border_width_length value,
      simplified_border_width_length upper )
  with
  | Some (Val lower), Some (Val value), Some (Val upper) -> (
      match
        ( border_width_length_measure lower,
          border_width_length_measure value,
          border_width_length_measure upper )
      with
      | ( Some (lower_key, lower_n),
          Some (value_key, value_n),
          Some (upper_key, upper_n) )
        when String.equal lower_key value_key
             && String.equal value_key upper_key ->
          Some
            (`Length
               (if value_n < lower_n then lower
                else if value_n > upper_n then upper
                else value))
      | Some _, Some (value_key, value_n), Some (upper_key, upper_n)
        when String.equal value_key upper_key && value_n <= upper_n ->
          Some (`Max [ Val lower; Val value ])
      | _ -> None)
  | _ -> None

(* [pp] is a pure serialiser (see the module note); every node-changing fold
   lives here instead, so it runs before [Declaration.hash] and two authored
   spellings of the same value converge to one node.

   A [min()]/[max()]/[clamp()] argument that survives grouping still gets its
   own same-unit terms combined (mirrors what [pp] used to do per-argument via
   [simplified_border_width_length]); the standalone [calc()] arm instead goes
   through [Values.normalize_length], the stronger fold [pp] used only when
   minifying. *)
let normalize_border_width_calc_arg (c : border_width calc) : border_width calc
    =
  match simplified_border_width_length c with
  | Some (Val length) -> (
      match border_width_of_length length with Some bw -> Val bw | None -> c)
  | Some simplified -> (
      match border_width_of_length_calc simplified with
      | Some c' -> c'
      | None -> c)
  | None -> c

let normalize_border_width_calc (c : border_width calc) : border_width calc =
  match length_of_border_width_calc c with
  | None -> Values.eval_calc c
  | Some lc -> (
      match Values.normalize_length (Calc lc) with
      | Calc lc' -> (
          match border_width_of_length_calc lc' with Some c' -> c' | None -> c)
      | length -> (
          match border_width_of_length length with
          | Some bw -> Val bw
          | None -> c))

let normalize_border_width_minmax kind (args : border_width calc list) :
    border_width =
  let rebuild args = match kind with `Min -> Min args | `Max -> Max args in
  match reduce_border_width_minmax kind args with
  | Some [ Val length ] -> (
      match border_width_of_length length with
      | Some bw -> bw
      | None -> rebuild args)
  | Some lengths -> (
      match List.map border_width_of_length_calc lengths with
      | terms when List.for_all Option.is_some terms ->
          rebuild (List.map Option.get terms)
      | _ -> rebuild args)
  | None -> rebuild (List.map normalize_border_width_calc_arg args)

let normalize_border_width_clamp lower value upper : border_width =
  match reduce_border_width_clamp lower value upper with
  | Some (`Length length) -> (
      match border_width_of_length length with
      | Some bw -> bw
      | None -> Clamp (lower, value, upper))
  | Some (`Max cargs) -> (
      match List.map border_width_of_length_calc cargs with
      | terms when List.for_all Option.is_some terms ->
          Max (List.map Option.get terms)
      | _ -> Clamp (lower, value, upper))
  | None ->
      Clamp
        ( normalize_border_width_calc_arg lower,
          normalize_border_width_calc_arg value,
          normalize_border_width_calc_arg upper )

(* CSS Values 4 sec. 10.12 keeps a math function valid where its range is
   exceeded and clamps at used-value time, so [calc(-1px)] is a border width
   Chrome computes as [0] and [-1px] one it drops. Unwrapping the call would
   turn the first into the second, which is why {!pp_length} guards its own
   unwrap the same way. *)
let negative_border_width : border_width -> bool = function
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
  | Ch f
  | Lh f
  | Vh f
  | Vw f
  | Vmin f
  | Vmax f
  | Pct f
  | Dimension { value = f; _ } ->
      f < 0.
  | _ -> false

let rec pp_border_width : border_width Pp.t =
 fun ctx -> function
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
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
  | Ch f -> Pp.unit ctx f "ch"
  | Lh f -> Pp.unit ctx f "lh"
  | Vh f -> Pp.unit ctx f "vh"
  | Vw f -> Pp.unit ctx f "vw"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Pct f -> Pp.pct ctx f
  | Dimension { value; unit; repr } ->
      if Pp.minified ctx then Pp.unit ctx value unit
      else (
        Pp.string ctx repr;
        Pp.string ctx unit)
  | Zero -> Pp.char ctx '0'
  | Auto -> Pp.string ctx "auto"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | Fit_content -> Pp.string ctx "fit-content"
  | From_font -> Pp.string ctx "from-font"
  | Calc cv ->
      pp_calc
        ~unwrap:(fun v -> not (negative_border_width v))
        pp_border_width ctx cv
  | Min args -> pp_border_width_minmax "min" ctx args
  | Max args -> pp_border_width_minmax "max" ctx args
  | Clamp (lower, value, upper) -> pp_border_width_clamp ctx lower value upper
  | Var v -> pp_var pp_border_width ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* Structural only (no [simplified_border_width_length]/[reduce_border_width_*]
   fold: those moved to [normalize_border_width]); [length_of_border_width_calc]
   is a type-level reshape, not a value fold. *)
and pp_border_width_calc_contents ctx calc =
  match length_of_border_width_calc calc with
  | Some lc -> pp_length_calc_contents ctx lc
  | None -> (
      (* Sec. 10.2 spells the operand slot [<calc-sum>], and sec. 10.1 makes a
         lone [var()] one, so the reference stands as authored; an operand that
         already carries its own [calc()] keeps that one rather than collecting
         a second. *)
      match calc with
      | Var v -> pp_var pp_border_width ctx v
      | Nested inner -> pp_calc pp_border_width ctx inner
      | calc -> pp_calc pp_border_width ctx calc)

and pp_border_width_minmax name ctx args =
  Pp.call name (Pp.list ~sep:Pp.comma pp_border_width_calc_contents) ctx args

and pp_border_width_clamp ctx lower value upper =
  Pp.call "clamp"
    (fun ctx (lower, value, upper) ->
      pp_border_width_calc_contents ctx lower;
      Pp.comma ctx ();
      pp_border_width_calc_contents ctx value;
      Pp.comma ctx ();
      pp_border_width_calc_contents ctx upper)
    ctx (lower, value, upper)

let pp_border_shorthand : border_shorthand Pp.t =
 fun ctx { width; style; color } ->
  let first = ref true in
  let add_space () = if !first then first := false else Pp.space ctx () in
  Option.iter
    (fun w ->
      add_space ();
      pp_border_width ctx w)
    width;
  Option.iter
    (fun s ->
      add_space ();
      pp_border_style ctx s)
    style;
  Option.iter
    (fun c ->
      let rendered =
        Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
          pp_color c
      in
      (* CSS Syntax: a [#hex] hash token is unambiguous after an ident, so
         minified output drops the separating space. *)
      let leads_with_delim =
        Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '#'
      in
      if not !first then if not leads_with_delim then Pp.space ctx ();
      first := false;
      Pp.string ctx rendered)
    color;
  if !first then Pp.string ctx "none"

let rec pp_border : border Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_border_shorthand ctx shorthand

let rec pp_logical_border_color : logical_border_color Pp.t =
 fun ctx -> function
  | Single color -> pp_color ctx color
  | Pair (start_, end_) ->
      pp_color ctx start_;
      Pp.space ctx ();
      pp_color ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_color ctx v

let rec pp_logical_border_width : logical_border_width Pp.t =
 fun ctx -> function
  | Single w -> pp_border_width ctx w
  | Pair (start_, end_) ->
      pp_border_width ctx start_;
      Pp.space ctx ();
      pp_border_width ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_width ctx v

let rec pp_logical_border_style : logical_border_style Pp.t =
 fun ctx -> function
  | Single s -> pp_border_style ctx s
  | Pair (start_, end_) ->
      pp_border_style ctx start_;
      Pp.space ctx ();
      pp_border_style ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_style ctx v

let rec pp_border_spacing : border_spacing Pp.t =
 fun ctx -> function
  | (Lengths lengths : border_spacing) ->
      Pp.list ~sep:Pp.space pp_length ctx lengths
  | Var v -> pp_var pp_border_spacing ctx v

(* CSS Tables 3 (ED) writes [border-spacing] as one or two non-negative lengths
   and reads a single one as "both the horizontal and vertical spacing", so a
   pair of equal lengths is the longer spelling of that one value. The per-side
   fold runs first, so a pair that only agrees once normalised collapses too. *)
and normalize_border_spacing : border_spacing -> border_spacing =
 fun value ->
  match value with
  | Lengths lengths -> (
      let normalized = map_preserve Values.normalize_length lengths in
      match normalized with
      | [ a; b ] when Values.equal_length a b -> Lengths [ a ]
      | _ when normalized == lengths -> value
      | _ -> Lengths normalized)
  | Var _ -> value

let rec pp_border_collapse : border_collapse Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border_collapse ctx v
  | Collapse -> Pp.string ctx "collapse"
  | Separate -> Pp.string ctx "separate"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_border_style t : border_style =
  Cursor.enum_or_var "border-style"
    [
      ("none", (None : border_style));
      ("solid", Solid);
      ("dashed", Dashed);
      ("dotted", Dotted);
      ("double", Double);
      ("groove", Groove);
      ("ridge", Ridge);
      ("inset", Inset);
      ("outset", Outset);
      ("hidden", Hidden);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_border_style t))
    t

(* CSS Backgrounds 3 (ED) sec. 3.2: [border-style] is [<line-style>{1,4}], the
   box over the four side styles, as sec. 3.1 gives [border-color] and sec. 3.3
   gives [border-width] theirs. A CSS-wide keyword reaches a slot only as the
   lone value; [validate_regular_property_components] rejects the mixes. *)
let read_border_style_box t : border_style list =
  Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4 read_border_style t

(* Helper: ensure border-width values are non-negative per CSS spec *)
let ensure_non_negative_border_width t value =
  if value < 0.0 then
    err_invalid_value t "border-width" "negative values not allowed"
  else value

(* [border-width] takes a [<length>], so every dimension [length] carries is
   one. The units [border_width] names get their own arm; the rest keep their
   value and spelling in [Dimension], the way [length] itself does, so a unit
   [length] learns needs no second table here. *)
let length_to_border_width ?(allow_negative = false) t (length : length) :
    border_width =
  let non_neg v =
    if allow_negative then v else ensure_non_negative_border_width t v
  in
  let typed_dimension ?repr value unit : border_width =
    let value = non_neg value in
    match String.lowercase_ascii unit with
    | "%" -> Pct value
    | "px" -> Px value
    | "cm" -> Cm value
    | "mm" -> Mm value
    | "q" -> Q value
    | "in" -> In value
    | "pt" -> Pt value
    | "pc" -> Pc value
    | "rem" -> Rem value
    | "em" -> Em value
    | "ex" -> Ex value
    | "cap" -> Cap value
    | "ic" -> Ic value
    | "ric" -> Ric value
    | "rlh" -> Rlh value
    | "ch" -> Ch value
    | "lh" -> Lh value
    | "vh" -> Vh value
    | "vw" -> Vw value
    | "vmin" -> Vmin value
    | "vmax" -> Vmax value
    | _ ->
        let repr =
          match repr with Some repr -> repr | None -> Pp.string_of_float value
        in
        Dimension { value; unit; repr }
  in
  match length with
  | Zero -> Zero
  | Dimension { value; unit; repr } -> typed_dimension ~repr value unit
  | length -> (
      match calc_length_unit length with
      | Some (unit, value) -> typed_dimension value unit
      | None -> err_invalid_value t "border-width" "unsupported length type")

(* CSS Backgrounds 3 (ED) sec. 3.3: [<line-width>] is [<length [0,inf]> | thin |
   medium | thick] and takes no percentage, which Chrome 146 refuses.
   [length_only] refuses one nested in math as well. *)
let read_length_as_border_width ?(allow_negative = false) t =
  let length =
    read_length ~allow_negative ~with_keywords:false ~length_only:true t
  in
  length_to_border_width ~allow_negative t length

(* CSS Values 4 sec. 10.12: a math function is valid wherever its type is, and
   the [0,inf] range of [<line-width>] is checked on the value it resolves to,
   not on each operand. So [calc(-1px)] reads and a literal [-1px] does not.
   Sec. 10.8 gives an operand no keyword either: [thin], [medium], [thick] and
   the CSS-wide keywords are not [<calc-value>]s, so [keywords] is off here and
   [calc(medium)] fails the way the browser drops it. *)
let rec read_border_width_in_math t : border_width =
  read_border_width_with ~keywords:false ~allow_negative:true t

(* CSS Values 4 sec. 10.2 requires the arguments of [min()], [max()] and
   [clamp()] to "have a consistent type or else the function is invalid", and
   sec. 10.9 gives a unitless zero inside a math function the [<number>] type.
   Without the check [min(0,1px)] folded to [0] rather than dropping. *)
and read_math_arg t =
  let expr = read_calc_expr read_border_width_in_math t in
  validate_calc_type t `Value expr;
  expr

and read_border_width_with ?(keywords = true) ~allow_negative t : border_width =
  let read_var t : border_width =
    Var (read_var (read_border_width_with ~keywords ~allow_negative) t)
  in
  let read_calc t : border_width =
    Calc (read_calc ~result_type:`Value read_border_width_in_math t)
  in
  let read_min t : border_width =
    Min
      (Cursor.call "min" t
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg))
  in
  let read_max t : border_width =
    Max
      (Cursor.call "max" t
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg))
  in
  let read_clamp t : border_width =
    match
      Cursor.call "clamp" t
        (Cursor.list ~sep:Cursor.comma ~at_least:3 ~at_most:3 read_math_arg)
    with
    | [ lower; value; upper ] -> Clamp (lower, value, upper)
    | _ -> Cursor.err_invalid t "invalid clamp"
  in
  Cursor.enum_or_calls "border-width"
    (if keywords then
       [
         ("thin", (Thin : border_width));
         ("medium", Medium);
         ("thick", Thick);
         ("inherit", Inherit);
         ("initial", Initial);
         ("unset", Unset);
         ("revert", Revert);
         ("revert-layer", Revert_layer);
       ]
     else [])
    ~calls:
      (("var", read_var) :: ("calc", read_calc) :: ("min", read_min)
     :: ("max", read_max) :: ("clamp", read_clamp)
      :: Values.typed_math_function_calls read_calc)
    ~default:(read_length_as_border_width ~allow_negative)
    t

let read_border_width t : border_width =
  read_border_width_with ~allow_negative:false t

module Border = struct
  type component =
    | Width of border_width
    | Style of border_style
    | Color of color

  type components = {
    width : border_width option;
    style : border_style option;
    color : color option;
  }

  let empty = { width = None; style = None; color = None }

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Color (read_color t));
        (fun t -> Width (read_border_width t));
        (fun t -> Style (read_border_style t));
      ]
      t

  let merge t acc = function
    | Width w when acc.width = None -> { acc with width = Some w }
    | Style s when acc.style = None -> { acc with style = Some s }
    | Color c when acc.color = None -> { acc with color = Some c }
    | Width _ -> Cursor.err_invalid t "duplicate border width"
    | Style _ -> Cursor.err_invalid t "duplicate border style"
    | Color _ -> Cursor.err_invalid t "duplicate border color"

  let to_shorthand (components : components) : border_shorthand =
    {
      width = components.width;
      style = components.style;
      color = components.color;
    }
end

(* CSS Backgrounds 3 (ED) sec. 3.4 writes the shorthand as [<line-width> ||
   <line-style> || <color>], and CSS Values 4 (ED) sec. 2.2 has [||] require one
   or more of its options to occur, so an empty value matches no border grammar.
   CSS Syntax 3 (ED) sec. 5.5.6 keeps only a declaration valid in its context,
   so the whole declaration goes; filling the slots in from nowhere would
   declare something the author did not write. *)
let read_border_shorthand t : border_shorthand =
  (* A [var()] in the border shorthand is type-ambiguous (it could substitute a
     width, style, or colour), so it cannot be assigned by matching a typed
     reader - assign it to the next unfilled slot in width/style/colour order
     and read its fallback with that slot's reader. Concrete components still
     bind by type. *)
  let acc = ref Border.empty in
  let consumed = ref true in
  while !consumed do
    Cursor.ws t;
    consumed :=
      if Cursor.is_done t || Cursor.peek_comma t then false
      else if Cursor.looking_at_func "var" t then (
        let a = !acc in
        acc :=
          if a.width = Option.None then
            { a with width = Some (Var (read_var read_border_width t)) }
          else if a.style = Option.None then
            { a with style = Some (Var (read_var read_border_style t)) }
          else if a.color = Option.None then
            { a with color = Some (Var (read_var read_color t)) }
          else Cursor.err_invalid t "too many border components";
        true)
      else
        match Cursor.option Border.read_component t with
        | Some c ->
            acc := Border.merge t !acc c;
            true
        | Option.None -> false
  done;
  let acc = !acc in
  if
    Option.is_none acc.width && Option.is_none acc.style
    && Option.is_none acc.color
  then Cursor.err_expected t "border width, style or color";
  Border.to_shorthand acc

let border_keyword = function
  | "inherit" -> Some (Inherit : border)
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | "none" -> Some None
  | _ -> None

let read_border_shorthand_from t snap : border =
  Cursor.restore t snap;
  Shorthand (read_border_shorthand t)

let read_border_keyword_or_shorthand t : border =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match border_keyword (String.lowercase_ascii ident) with
      | Some value ->
          Cursor.ws t;
          if Cursor.is_done t || Cursor.peek_comma t then value
          else read_border_shorthand_from t snap
      | None -> read_border_shorthand_from t snap)
  | None -> read_border_shorthand_from t snap

let rec read_border (t : Cursor.t) : border =
  if Cursor.looking_at_func "var" t then (
    (* A lone [var()] is the whole value; a [var()] followed by more components
       is a shorthand whose first component happens to be a var. *)
    let snap = Cursor.save t in
    let v = Values.read_var read_border t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_comma t then (Var v : border)
    else (
      Cursor.restore t snap;
      read_border_keyword_or_shorthand t))
  else read_border_keyword_or_shorthand t

let rec read_logical_border_color t : logical_border_color =
  let read_colors t =
    match Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_color t with
    | [ color ] -> (Single color : logical_border_color)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two colors"
  in
  Cursor.enum_or_var "logical border color"
    [
      ("inherit", (Inherit : logical_border_color));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_color t))
    ~default:read_colors t

let rec read_logical_border_width t : logical_border_width =
  let read_widths t =
    match
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_border_width t
    with
    | [ w ] -> (Single w : logical_border_width)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two widths"
  in
  Cursor.enum_or_var "logical border width"
    [
      ("inherit", (Inherit : logical_border_width));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_width t))
    ~default:read_widths t

(* CSS Logical 1 (ED) sec. 4.5.2: [border-inline-style] and [border-block-style]
   are [<'border-top-style'>{1,2}], the first value the start edge and the
   second the end edge. *)
let rec read_logical_border_style t : logical_border_style =
  let read_styles t =
    match
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_border_style t
    with
    | [ s ] -> (Single s : logical_border_style)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two line styles"
  in
  Cursor.enum_or_var "logical border style"
    [
      ("inherit", (Inherit : logical_border_style));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_style t))
    ~default:read_styles t

let rec read_border_collapse t : border_collapse =
  Cursor.enum_or_var "border-collapse"
    [
      ("collapse", (Collapse : border_collapse));
      ("separate", Separate);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_border_collapse t) : border_collapse))
    t

let border_shorthand ?width ?style ?color () : border =
  Shorthand { width; style; color }

(* CSS Backgrounds 3 sec. 5.1: [border-radius = <length-percentage [0,inf]>{1,4}
   [ / <length-percentage [0,inf]>{1,4} ]?]. Reads 1-4 horizontal radii then,
   after [/], 1-4 vertical radii. No keyword is a radius: the intrinsic-sizing
   keywords are no part of the grammar, and the CSS-wide keywords are only the
   whole property value (CSS Cascade 5 sec. 6), which [read_border_radius] takes
   before delegating here. Basic shapes spell their rounded-corner suffix [round
   <'border-radius'>] (CSS Shapes 1 sec. 3.1), a reference to this same value
   definition, so they read radii through [read_border_radius_inline] and get
   the keyword exclusion with it. *)
let read_border_radius_inline_radii t =
  let rec loop acc count =
    if count >= 4 then List.rev acc
    else
      match
        Cursor.option
          (read_length_percentage ~allow_negative:false ~with_keywords:false)
          t
      with
      | None -> List.rev acc
      | Some lp -> loop (lp :: acc) (count + 1)
  in
  match loop [] 0 with
  | [] -> Cursor.err_expected t "<length-percentage>"
  | radii -> radii

let read_border_radius_inline_vertical t =
  match Cursor.peek_delim t with
  | Some '/' ->
      Cursor.skip t;
      Cursor.ws t;
      Some (read_border_radius_inline_radii t)
  | _ -> None

let read_border_radius_inline t : border_radius =
  Cursor.ws t;
  let horizontal = read_border_radius_inline_radii t in
  Cursor.ws t;
  let vertical = read_border_radius_inline_vertical t in
  Radius { horizontal; vertical }

let rec read_border_radius (t : Cursor.t) : border_radius =
  Cursor.enum_or_whole_value_var "border-radius"
    [
      ("inherit", (Inherit : border_radius));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_border_radius t) : border_radius))
    ~default:read_border_radius_inline t

(* A zero-valued border-width unit ([0px], [0em]) is the bare length [0]
   ([border-width: 0] = [border-width: 0px]); collapse it to [Zero] like every
   other zero length ([width: 0px] -> [0]), which [<length>]-valued properties
   already do via [normalize_length]. *)
let border_width_is_zero (bw : border_width) =
  match length_of_border_width bw with
  | Some l -> (
      match Values.normalize_length l with Values.Zero -> true | _ -> false)
  | None -> false

let normalize_border_width (bw : border_width) : border_width =
  match bw with
  | Calc c -> (
      match normalize_border_width_calc c with
      (* The call stays around a negative for the reason {!pp_border_width}
         keeps it: sec. 10.12 clamps [calc(-1px)] at used-value time and drops a
         bare [-1px]. *)
      | Val v when not (negative_border_width v) -> v
      | folded -> Calc folded)
  | Min args -> normalize_border_width_minmax `Min args
  | Max args -> normalize_border_width_minmax `Max args
  | Clamp (lower, value, upper) ->
      normalize_border_width_clamp lower value upper
  | _ when border_width_is_zero bw -> Zero
  | _ -> bw

(* [<line-width>] and [<length>] share every shape but the three keywords, so
   read the substitution question in length space; a node that does not map over
   ([var()], a sibling index) answers as substituting, the safe side for a
   caller deciding whether the value can be folded into a shorthand. *)
let border_width_has_runtime_subst (bw : border_width) : bool =
  let calc c =
    match length_of_border_width_calc c with
    | Some lc -> Values.length_has_runtime_subst (Calc lc)
    | None -> true
  in
  match bw with
  | Var _ -> true
  | Calc c -> calc c
  | Min args | Max args -> List.exists calc args
  | Clamp (lower, value, upper) -> calc lower || calc value || calc upper
  | _ -> false

(* CSS Logical 1 sec. 4.3 and 4.4: an axis shorthand takes [<side>{1,2}], so a
   repeated side has a shorter spelling naming the same two sides, exactly as
   [collapse_box_shorthand] picks one for the four-side families. An arbitrary
   substitution defers the component count to computed-value time, so a pair
   holding one keeps both. *)
let normalize_logical_border_width :
    logical_border_width -> logical_border_width =
 fun value ->
  match value with
  | Single w -> Single (normalize_border_width w)
  | Pair (a, b) ->
      let a = normalize_border_width a and b = normalize_border_width b in
      if
        (not (is_border_width_substitution a))
        && (not (is_border_width_substitution b))
        && equal_border_width a b
      then Single a
      else Pair (a, b)
  | other -> other

(* [border-style] takes keywords, so the axis has nothing to canonicalise per
   value and only the spelling is at stake. *)
let normalize_logical_border_style :
    logical_border_style -> logical_border_style =
 fun value ->
  match value with
  | Pair (a, b)
    when (not (is_border_style_substitution a))
         && (not (is_border_style_substitution b))
         && equal_border_style a b ->
      Single a
  | other -> other

(* CSS Backgrounds 3 (ED) sec. 3.4: a shorthand sets every longhand it covers,
   so an omitted slot takes its initial value - sec. 3.3 makes that [medium] for
   the width, sec. 3.2 [none] for the style and sec. 3.1 [currentColor] for the
   colour. An explicit initial value therefore declares what leaving the slot
   out declares, and the shorter spelling wins. *)
let drop_initial_line_width (width : border_width option) : border_width option
    =
  match width with Some Medium -> Option.None | width -> width

let drop_initial_line_style (style : border_style option) : border_style option
    =
  match style with Some (None : border_style) -> Option.None | style -> style

(* CSS Multi-column 1 (ED) sec. 4.2 and css-logical-1 sec. 4.5.3 give
   column-rule and the logical borders the same initial colour, and they read
   this production. CSS UI 4 (ED) sec. 3.4 does not: outline-color starts at
   [auto], which [currentColor] does not name, so [normalize_outline] has no
   colour drop. *)
let drop_initial_line_color (color : color option) : color option =
  match color with Some (Current : color) -> Option.None | color -> color

(* The width slot of the border shorthands is a [<'border-width'>], so it takes
   the same fold as the longhand; the style slot is a [<line-style>] and the
   colour slot a [<color>]. Drained of every slot the shorthand declares nothing
   but initial values, which is what [none] declares, and [none] is the node the
   keyword parses to - so the two spellings meet there. *)
let normalize_border ?(lossless = false) : border -> border =
 fun value ->
  match value with
  | Shorthand s ->
      let width =
        drop_initial_line_width
          (option_map_preserve normalize_border_width s.width)
      in
      let style = drop_initial_line_style s.style in
      let color =
        drop_initial_line_color
          (option_map_preserve (normalize_color ~lossless) s.color)
      in
      if width == s.width && style == s.style && color == s.color then value
      else if
        Option.is_none width && Option.is_none style && Option.is_none color
      then (None : border)
      else Shorthand { width; style; color }
  | other -> other

let rec read_border_spacing t : border_spacing =
  let read_numeric_length t =
    let l = read_length ~allow_negative:false ~length_only:true t in
    match l with
    | Auto | Size | None | Normal | Fit_content | Content | Contain
    | Max_content | Min_content | From_font | Hairline | Thin | Medium | Thick
    | Stretch ->
        Cursor.err_invalid t "border-spacing requires a <length>"
    | _ -> l
  in
  Cursor.enum_or_whole_value_var "border-spacing" []
    ~var:(fun t -> Var (Values.read_var read_border_spacing t))
    ~default:(fun t ->
      (Lengths
         (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_numeric_length
            t)
        : border_spacing))
    t
