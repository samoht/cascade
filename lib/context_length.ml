open Context_intf
open Context_residual

(** {2 Length canonicalisation (CSS Values 4 sec. 6)}

    All length units convert to pixels. Absolute units come from the fixed 1in =
    96px conversion table; font-relative and viewport-relative units require the
    supplied base size. Returns [None] when a length depends on information not
    present in the context (e.g. [ch]/[ex] need glyph metrics, percentages need
    a containing-block size). *)

module Length = struct
  type ctx = {
    root_font_size : float option;
    parent_font_size : float option;
    viewport_width : float option;
    viewport_height : float option;
    container_width : float option;
    container_height : float option;
  }

  (* CSS Media Queries 4 sec. 1.3: relative units in @media/@container resolve
     against root font-size. Pre-fill [parent_font_size]/[root_font_size] with
     the 16px default so [em]/[rem] in queries always have a reference; a fresh
     [Length.ctx] without these rejects relative units. *)
  let media_default =
    {
      root_font_size = Some 16.;
      parent_font_size = Some 16.;
      viewport_width = None;
      viewport_height = None;
      container_width = None;
      container_height = None;
    }

  let in_to_px = 96.
  let cm_to_px = 96. /. 2.54
  let mm_to_px = cm_to_px /. 10.
  let q_to_px = cm_to_px /. 40.
  let pt_to_px = in_to_px /. 72.
  let pc_to_px = in_to_px /. 6.

  let to_px ctx (l : Values.length) : float option =
    let viewport_v fn =
      match (ctx.viewport_width, ctx.viewport_height) with
      | Some w, Some h -> Some (fn w h)
      | _ -> None
    in
    let container_v fn =
      match (ctx.container_width, ctx.container_height) with
      | Some w, Some h -> Some (fn w h)
      | _ -> None
    in
    match l with
    | Px f -> Some f
    | Cm f -> Some (f *. cm_to_px)
    | Mm f -> Some (f *. mm_to_px)
    | Q f -> Some (f *. q_to_px)
    | In f -> Some (f *. in_to_px)
    | Pt f -> Some (f *. pt_to_px)
    | Pc f -> Some (f *. pc_to_px)
    | Rem f -> Option.map (fun b -> f *. b) ctx.root_font_size
    | Em f -> Option.map (fun b -> f *. b) ctx.parent_font_size
    | Vw f | Lvw f | Svw f ->
        Option.map (fun w -> f *. w /. 100.) ctx.viewport_width
    | Vh f | Lvh f | Svh f ->
        Option.map (fun h -> f *. h /. 100.) ctx.viewport_height
    | Dvw f -> Option.map (fun w -> f *. w /. 100.) ctx.viewport_width
    | Dvh f -> Option.map (fun h -> f *. h /. 100.) ctx.viewport_height
    | Vmin f | Lvmin f | Svmin f | Dvmin f ->
        viewport_v (fun w h -> f *. Float.min w h /. 100.)
    | Vmax f | Lvmax f | Svmax f | Dvmax f ->
        viewport_v (fun w h -> f *. Float.max w h /. 100.)
    | Vi f -> Option.map (fun w -> f *. w /. 100.) ctx.viewport_width
    | Vb f -> Option.map (fun h -> f *. h /. 100.) ctx.viewport_height
    | Cqw f | Cqi f -> Option.map (fun w -> f *. w /. 100.) ctx.container_width
    | Cqh f | Cqb f -> Option.map (fun h -> f *. h /. 100.) ctx.container_height
    | Cqmin f -> container_v (fun w h -> f *. Float.min w h /. 100.)
    | Cqmax f -> container_v (fun w h -> f *. Float.max w h /. 100.)
    | Zero -> Some 0.
    | _ -> None

  let media_to_px (l : Values.length) : float option = to_px media_default l

  let normalize_zero = function
    | Values.Px px when Float.equal px 0. -> Values.Zero
    | value -> value

  let rec all_px ctx = function
    | [] -> Some []
    | value :: values -> (
        match (to_px ctx value, all_px ctx values) with
        | Some px, Some pxs -> Some (px :: pxs)
        | _ -> None)

  let of_t (ctx : t) : ctx =
    let unwrap_px = function Some (Values.Px p) -> Some p | _ -> None in
    {
      root_font_size = unwrap_px ctx.root_font_size;
      parent_font_size = unwrap_px ctx.parent_font_size;
      viewport_width = unwrap_px ctx.viewport_width;
      viewport_height = unwrap_px ctx.viewport_height;
      container_width = unwrap_px ctx.container_width;
      container_height = unwrap_px ctx.container_height;
    }

  (* CSS Values 4 sec. 10.11 simplification of a typed [length calc]: fold every
     subtree the [ctx] can collapse to absolute lengths, leaving
     [Var]/[Sibling_*]/unresolvable subtrees. The result stays a [length calc] -
     [Val (Px _)] when fully reducible, else [Expr] with simplified operands. *)
  let eval_combine_lengths ctx la lb (op : Values.calc_op) :
      Values.length option =
    match (to_px ctx la, to_px ctx lb, op) with
    | Some pa, Some pb, Add -> Some (Px (pa +. pb))
    | Some pa, Some pb, Sub -> Some (Px (pa -. pb))
    | _ -> None

  let eval_combine_length_num ctx l n (op : Values.calc_op) :
      Values.length option =
    match (to_px ctx l, op) with
    | Some p, Mul -> Some (Px (p *. n))
    | Some p, Div when n <> 0. -> Some (Px (p /. n))
    | _ -> None

  let eval_num_expr (l : Values.length Values.calc) op
      (r : Values.length Values.calc) : Values.length Values.calc option =
    let open Values in
    match (l, op, r) with
    | Num a, Add, Num b -> Some (Num (a +. b))
    | Num a, Sub, Num b -> Some (Num (a -. b))
    | Num a, Mul, Num b -> Some (Num (a *. b))
    | Num a, Div, Num b when b <> 0. -> Some (Num (a /. b))
    | _ -> None

  let eval_value_expr ctx (l : Values.length Values.calc) op
      (r : Values.length Values.calc) : Values.length Values.calc option =
    let open Values in
    match (l, op, r) with
    | Val la, _, Val lb ->
        Option.map (fun out -> Val out) (eval_combine_lengths ctx la lb op)
    | Val la, _, Num n ->
        Option.map (fun out -> Val out) (eval_combine_length_num ctx la n op)
    | Num n, Mul, Val lb ->
        (* Multiplication is commutative on length x number. *)
        Option.map (fun out -> Val out) (eval_combine_length_num ctx lb n Mul)
    | _ -> None

  let eval_calc_expr_values ctx (l : Values.length Values.calc) op
      (r : Values.length Values.calc) : Values.length Values.calc =
    let open Values in
    match eval_num_expr l op r with
    | Some folded -> folded
    | None -> (
        match eval_value_expr ctx l op r with
        | Some folded -> folded
        | None -> Expr (l, op, r))

  let eval_calc_wrapper_value wrap reduced =
    let open Values in
    match reduced with
    | (Val _ | Num _ | Math_const _ | Var _) as leaf -> leaf
    | reduced -> wrap reduced

  let eval_calc ctx (calc : Values.length Values.calc) :
      Values.length Values.calc =
    let rec eval (calc : Values.length Values.calc) : Values.length Values.calc
        =
      let open Values in
      match calc with
      | Num _ | Val _ | Var _ | Math_const _ | Math_fn _ | Sibling_index
      | Sibling_count ->
          calc
      | Nested inner -> eval_calc_wrapper_value (fun v -> Nested v) (eval inner)
      | Parens inner -> eval_calc_wrapper_value (fun v -> Parens v) (eval inner)
      | Expr (l, op, r) -> eval_calc_expr_values ctx (eval l) op (eval r)
    in
    eval calc

  let simplify_math_args simplify ~visited args =
    List.map (simplify ~visited) args

  let simplify_length_min ctx simplify ~visited args =
    let args = simplify_math_args simplify ~visited args in
    match all_px ctx args with
    | Some (first :: rest) ->
        normalize_zero (Values.Px (List.fold_left Float.min first rest))
    | _ -> Values.Min args

  let simplify_length_max ctx simplify ~visited args =
    let args = simplify_math_args simplify ~visited args in
    match all_px ctx args with
    | Some (first :: rest) ->
        normalize_zero (Values.Px (List.fold_left Float.max first rest))
    | _ -> Values.Max args

  let simplify_length_clamp ctx simplify ~visited (min, preferred, max) =
    let args = simplify_math_args simplify ~visited [ min; preferred; max ] in
    match (all_px ctx args, args) with
    | Some [ min; preferred; max ], _ ->
        normalize_zero (Values.Px (Float.max min (Float.min preferred max)))
    | _, [ min; preferred; max ] -> Values.Clamp (min, preferred, max)
    | _ -> Values.Clamp (min, preferred, max)

  let simplify ?(preserve_authored_calc = true) ?layer_order ?layer cascade ctx
      value =
    let simplify_leaf simplify simplify_calc ~visited value =
      match value with
      | Values.Min args -> simplify_length_min ctx simplify ~visited args
      | Values.Max args -> simplify_length_max ctx simplify ~visited args
      | Values.Clamp (mn, v, mx) ->
          simplify_length_clamp ctx simplify ~visited (mn, v, mx)
      | Values.Fit_content_arg value ->
          Values.Fit_content_arg (simplify ~visited value)
      | Values.Round (strategy, value, step) ->
          Values.Round
            (strategy, simplify ~visited value, simplify ~visited step)
      | Values.Mod (left, right) ->
          Values.Mod (simplify ~visited left, simplify ~visited right)
      | Values.Rem_fn (left, right) ->
          Values.Rem_fn (simplify ~visited left, simplify ~visited right)
      | Values.Hypot values ->
          Values.Hypot (List.map (simplify ~visited) values)
      | Values.Abs value -> Values.Abs (simplify ~visited value)
      | Values.Calc_size (basis, calc) ->
          Values.Calc_size (simplify ~visited basis, simplify_calc ~visited calc)
      | Values.Anchor (name, side, fallback) ->
          Values.Anchor (name, side, Option.map (simplify ~visited) fallback)
      | value -> value
    in
    let ops : Values.length Calc_residual.ops =
      let to_number = to_px ctx in
      let of_number px =
        if Float.equal px 0. then Values.Zero else Values.Px px
      in
      {
        of_unitless_number = (fun _ -> None);
        zero = Values.Val Values.Zero;
        is_zero = Values.length_is_zero;
        combine_values = combine_numeric_values ~to_number ~of_number;
        combine_value_num = combine_numeric_value_num ~to_number ~of_number;
        normalize_value = normalize_numeric_value ~to_number ~of_number;
        as_var = (function Values.Var var -> Some var | _ -> None);
        of_var = (fun var -> Values.Var var);
        as_calc = (function Values.Calc calc -> Some calc | _ -> None);
        of_calc = (fun calc -> Values.Calc calc);
        read_custom = read_custom_value Properties.Length Values.read_length;
        simplify_leaf;
      }
    in
    let resolve_fallback (value : Values.length) =
      match value with Values.Revert_layer -> false | _ -> true
    in
    let preserve_authored_calc =
      preserve_authored_calc
      &&
      match (value : Values.length) with
      | Values.Calc calc -> Calc_residual.contains_var calc
      | _ -> false
    in
    let value : Values.length =
      Calc_residual.simplify ~resolve_fallback ?layer_order ?layer cascade ops
        value
    in
    match value with
    | Values.Calc _ when preserve_authored_calc -> value
    | Values.Calc calc -> (
        match eval_calc ctx calc with
        | Values.Val value -> normalize_zero value
        | calc -> Values.Calc calc)
    | value -> normalize_zero value
end
