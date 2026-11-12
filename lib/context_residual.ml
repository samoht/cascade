open Context_intf
open Context_core

type (_, _) type_eq = Refl : ('a, 'a) type_eq

let kind_equal : type a b.
    a Properties.kind -> b Properties.kind -> (a, b) type_eq option =
 fun a b ->
  match (a, b) with
  | Properties.Length, Properties.Length -> Some Refl
  | Properties.Length_percentage, Properties.Length_percentage -> Some Refl
  | Properties.Opacity, Properties.Opacity -> Some Refl
  | Properties.Angle, Properties.Angle -> Some Refl
  | Properties.Rotate, Properties.Rotate -> Some Refl
  | Properties.Scale, Properties.Scale -> Some Refl
  | Properties.Duration, Properties.Duration -> Some Refl
  | Properties.Animation, Properties.Animation -> Some Refl
  | Properties.Number, Properties.Number -> Some Refl
  | Properties.Number_percentage, Properties.Number_percentage -> Some Refl
  | Properties.Font_size, Properties.Font_size -> Some Refl
  | Properties.Filter, Properties.Filter -> Some Refl
  | Properties.Shadow, Properties.Shadow -> Some Refl
  | Properties.Color, Properties.Color -> Some Refl
  | Properties.Gradient_stop, Properties.Gradient_stop -> Some Refl
  | Properties.Background_image, Properties.Background_image -> Some Refl
  | Properties.Font_src, Properties.Font_src -> Some Refl
  | _ -> None

let read_full_components read components =
  match
    let cursor = Cursor.of_components components in
    let value = read cursor in
    Cursor.ws cursor;
    Cursor.expect_eof cursor;
    value
  with
  | value -> Some value
  | exception Cursor.Parse_error _ -> None

let read_custom_value : type a.
    a Properties.kind -> (Cursor.t -> a) -> Declaration.declaration -> a option
    =
 fun target_kind read decl ->
  match decl with
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } -> (
      match value with
      | Properties.Typed { kind; value } -> (
          match kind_equal kind target_kind with
          | Some Refl -> Some value
          | None ->
              read_full_components read
                (Properties.components_of_custom_property_value
                   (Properties.Typed { kind; value })))
      | Properties.Tokens components -> read_full_components read components)
  | _ -> None

let read_custom_components read = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } ->
      read_full_components read
        (Properties.components_of_custom_property_value value)
  | _ -> None

let map_var_fallback f = function
  | Values.Empty -> Values.Empty
  | Values.Empty2 -> Values.Empty2
  | Values.None -> Values.None
  | Values.Fallback value -> Values.Fallback (f value)
  | Values.Syntax_fallback value -> Values.Syntax_fallback value
  | Values.Var_fallback name -> Values.Var_fallback name

let combine_numeric_values ~to_number ~of_number left op right =
  let canonical f =
    try float_of_string (Pp.string_of_float f) with Failure _ -> f
  in
  match (to_number left, op, to_number right) with
  | Some a, Values.Add, Some b -> Some (of_number (canonical (a +. b)))
  | Some a, Values.Sub, Some b -> Some (of_number (canonical (a -. b)))
  | _ -> None

let combine_numeric_value_num ~to_number ~of_number value op num =
  let canonical f =
    try float_of_string (Pp.string_of_float f) with Failure _ -> f
  in
  match (to_number value, op) with
  | Some n, Values.Mul -> Some (of_number (canonical (n *. num)))
  | Some n, Values.Div when num <> 0. -> Some (of_number (canonical (n /. num)))
  | _ -> None

let normalize_numeric_value ~to_number ~of_number value =
  match to_number value with
  | Some n -> (
      try of_number (float_of_string (Pp.string_of_float n))
      with Failure _ -> of_number n)
  | None -> value

module Var_residual = struct
  type 'a simplifier = authored:bool -> visited:string list -> 'a -> 'a

  type 'a ops = {
    as_var : 'a -> 'a Values.var option;
    of_var : 'a Values.var -> 'a;
    read_custom : Declaration.declaration -> 'a option;
    simplify_leaf : 'a simplifier -> 'a simplifier;
  }

  let with_resolver (type a) ?(resolve_fallback = fun _ -> true) ?layer_order
      ?layer cascade ~(read_custom : Declaration.declaration -> a option) f =
    let parsed_custom = Hashtbl.create 16 in
    (* The cache key is only [var.name] because [layer] and [layer_order] are
       fixed for one simplification call. If callers ever vary scope inside a
       single call, the key must include that scope. *)
    let simplify_var_record ~(simplify : a simplifier) ~visited
        (var : a Values.var) : a Values.var =
      {
        var with
        Values.fallback =
          map_var_fallback (simplify ~authored:false ~visited) var.fallback;
        default = Option.map (simplify ~authored:false ~visited) var.default;
      }
    in
    (* CSS Variables 1 sec. 2.1 gives a CSS-wide keyword written to a custom
       property "its usual meaning", so the binding is what the cascade makes of
       the keyword against the element the sheet is read for, and never the
       keyword's own tokens. This resolver has no element, so such a binding
       holds no value it can put in place of the reference. *)
    let css_wide_binding decl =
      match
        String.lowercase_ascii
          (String.trim (Declaration.string_of_value ~minify:true decl))
      with
      | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
      | _ -> false
    in
    let lookup_parsed name =
      match Hashtbl.find_opt parsed_custom name with
      | Some value -> value
      | None ->
          let value =
            Option.bind
              (lookup_custom_property ?layer ?layer_order cascade name)
              (fun decl ->
                if css_wide_binding decl then Option.None else read_custom decl)
          in
          Hashtbl.add parsed_custom name value;
          value
    in
    let resolve_fallback_value ~(simplify : a simplifier) ~visited
        (var : a Values.var) : a option =
      match var.fallback with
      | Values.Fallback fallback when resolve_fallback fallback ->
          Some (simplify ~authored:false ~visited fallback)
      | Values.Fallback _ -> None
      | Values.Empty | Values.Empty2 | Values.None | Values.Syntax_fallback _
      | Values.Var_fallback _ ->
          None
    in
    (* CSS Variables 1 sec. 3 replaces a [var()] with the custom property's
       value "if the value of the custom property ... is anything but the
       initial value", and puts the fallback in only where it is not. A binding
       this reader cannot make a value of - an empty token stream, a stream the
       property's grammar refuses - is one of the former, so the declaration is
       invalid at computed-value time and the fallback is no answer to it. *)
    let unreadable name =
      match lookup_custom_property ?layer ?layer_order cascade name with
      | None -> false
      | Some _ -> Option.is_none (lookup_parsed name)
    in
    let resolve_var ~(simplify : a simplifier) ~visited (var : a Values.var) :
        a option =
      (* A [runtime] var - either flagged on the value or named in the context's
         [runtime_vars] - stays a [var()] reference so a script can override the
         custom property; never fold it to its theme value. *)
      if
        var.runtime
        || List.mem var.name cascade.runtime_vars
        || List.mem var.name visited
      then None
      else
        match lookup_parsed var.name with
        | Some value ->
            Some (simplify ~authored:false ~visited:(var.name :: visited) value)
        | None when unreadable var.name ->
            (* The binding is there and holds no value for this property, so the
               declaration is invalid at computed-value time and the fallback is
               no answer to it. *)
            None
        | None -> (
            (* Typed [default] supplied by [Css.var ~default:...] is the
               authoritative compile-time value of the variable, so it wins over
               the var() [Fallback] arm (which is a browser-time hint for when
               the [--name] declaration is missing at runtime). *)
            match var.default with
            | Some d ->
                Some (simplify ~authored:false ~visited:(var.name :: visited) d)
            | None -> resolve_fallback_value ~simplify ~visited var)
    in
    f ~resolve_var ~simplify_var_record ~unreadable

  let simplify (type a) ?layer_order ?layer cascade (ops : a ops) (value : a) :
      a =
    with_resolver ?layer_order ?layer cascade ~read_custom:ops.read_custom
    @@ fun ~resolve_var ~simplify_var_record ~unreadable ->
    (* The reference resolved to a value that is itself a [var()]. Whether the
       fallback answers for that depends on why: CSS Variables 1 sec. 3 puts the
       fallback in only where the custom property IS the guaranteed-invalid
       value, so a chain ending in an UNBOUND name takes it, and one ending in a
       binding whose value this property's grammar refuses does not. The second
       is invalid at computed-value time, which sec. 2.2 makes [unset], and the
       fallback is no answer to it. *)
    let residual_holds_a_value result =
      match ops.as_var result with
      | Some (inner : a Values.var) -> (
          (* Bound, and holding a value this property's grammar refuses. *)
          unreadable inner.name
          ||
          match inner.fallback with
          (* [Syntax_fallback] is a fallback the reader kept but could not type
             in this property's position, so the chain resolves to a value the
             property refuses. A typed [Fallback] is the opposite: it resolves
             to a value the property takes, and the resolver hands that back
             rather than a residual. *)
          | Values.Syntax_fallback _ -> true
          | Values.Fallback _ | Values.Empty | Values.Empty2 | Values.None
          | Values.Var_fallback _ ->
              false)
      | None -> false
    in
    let rec on_var_residual ~visited (var : a Values.var) result =
      match var.fallback with
      | Values.Fallback _ when residual_holds_a_value result ->
          ops.of_var (simplify_var_record ~simplify ~visited var)
      | Values.Fallback fb -> simplify ~authored:false ~visited fb
      | Values.Syntax_fallback _ | Values.Var_fallback _ ->
          ops.of_var (simplify_var_record ~simplify ~visited var)
      | _ -> result
    and on_var_unresolved ~visited (var : a Values.var) =
      match var.fallback with
      | Values.Fallback fb -> simplify ~authored:false ~visited fb
      | Values.Syntax_fallback _ | Values.Var_fallback _ | Values.Empty
      | Values.Empty2 | Values.None ->
          ops.of_var (simplify_var_record ~simplify ~visited var)
    and on_var ~visited (var : a Values.var) =
      if var.runtime || List.mem var.name cascade.runtime_vars then
        (* A runtime var remains a live browser-time reference, including its
           fallback. Treating it like an ordinary unresolved variable would
           select a typed [Fallback] here and erase the override point. *)
        ops.of_var (simplify_var_record ~simplify ~visited var)
      else if List.mem var.name visited then
        (* A var() forming a cycle is invalid at computed-value time; its own
           fallback does not rescue it (CSS Variables L1 sec 3). Leave it as an
           unresolved residual so an outer consumer's fallback can apply. *)
        ops.of_var (simplify_var_record ~simplify ~visited var)
      else
        match resolve_var ~simplify ~visited var with
        | Some result when ops.as_var result <> None ->
            on_var_residual ~visited var result
        | Some result -> result
        | None when unreadable var.name ->
            (* The binding is there and holds no value for this property, so the
               declaration is invalid at computed-value time. Leave the residual
               rather than answering with the fallback. *)
            ops.of_var (simplify_var_record ~simplify ~visited var)
        | None -> on_var_unresolved ~visited var
    and simplify ~authored ~visited value =
      match ops.as_var value with
      | Some var -> on_var ~visited var
      | None -> ops.simplify_leaf simplify ~authored ~visited value
    in
    simplify ~authored:true ~visited:[] value
end

module Calc_residual = struct
  type 'a simplifier = visited:string list -> 'a -> 'a

  type 'a calc_simplifier =
    visited:string list -> 'a Values.calc -> 'a Values.calc

  type 'a ops = {
    of_unitless_number : float -> 'a option;
    zero : 'a Values.calc;
        (** The type's canonical unitless zero, returned by [x * 0] when neither
            operand carries a unit. *)
    is_zero : 'a -> bool;
        (** Recognises a literal zero value, so [0 * x] collapses to zero even
            when [x] is an unresolved [var()]. Conservative [false] is always
            safe (keeps the multiplication). *)
    combine_values : 'a -> Values.calc_op -> 'a -> 'a option;
    combine_value_num : 'a -> Values.calc_op -> float -> 'a option;
    normalize_value : 'a -> 'a;
    as_var : 'a -> 'a Values.var option;
    of_var : 'a Values.var -> 'a;
    as_calc : 'a -> 'a Values.calc option;
    of_calc : 'a Values.calc -> 'a;
    read_custom : Declaration.declaration -> 'a option;
    simplify_leaf : 'a simplifier -> 'a calc_simplifier -> 'a simplifier;
  }

  let rec math_arg_contains_var = function
    | Values.Lit _ | Values.Dim _ | Values.Const _ -> false
    | Values.Var_arg _ -> true
    | Values.Op (l, _, r) -> math_arg_contains_var l || math_arg_contains_var r
    | Values.Parens_arg inner -> math_arg_contains_var inner
    | Values.Math_call fn -> math_fn_contains_var fn

  and angle_arg_contains_var : Values.angle_arg -> bool = function
    | Values.Deg _ | Values.Rad _ | Values.Turn _ | Values.Grad _ -> false
    | Values.Numeric_arg arg -> math_arg_contains_var arg
    | Values.Operation (l, _, r) ->
        angle_arg_contains_var l || angle_arg_contains_var r
    | Values.Grouped inner -> angle_arg_contains_var inner

  and math_fn_contains_var = function
    | Values.Sin a | Values.Cos a | Values.Tan a -> angle_arg_contains_var a
    | Values.Asin a
    | Values.Acos a
    | Values.Atan a
    | Values.Sqrt a
    | Values.Exp a
    | Values.Sign_n a
    | Values.Abs_n a ->
        math_arg_contains_var a
    | Values.Atan2 (a, b)
    | Values.Log (a, Some b)
    | Values.Pow (a, b)
    | Values.Round_n (_, a, b)
    | Values.Mod_n (a, b)
    | Values.Rem_n (a, b) ->
        math_arg_contains_var a || math_arg_contains_var b
    | Values.Log (a, None) -> math_arg_contains_var a
    | Values.Hypot args -> List.exists math_arg_contains_var args

  let rec contains_var : type a. a Values.calc -> bool = function
    | Values.Var _ -> true
    | Values.Nested inner | Values.Parens inner -> contains_var inner
    | Values.Expr (left, _, right) -> contains_var left || contains_var right
    | Values.Math_fn fn -> math_fn_contains_var fn
    | Values.Num _ | Values.Val _ | Values.Math_const _ | Values.Sibling_index
    | Values.Sibling_count ->
        false

  let fold_num_expr : type a.
      a Values.calc -> Values.calc_op -> a Values.calc -> a Values.calc option =
   fun left op right ->
    let open Values in
    match (left, op, right) with
    | Num a, Add, Num b -> Some (Num (a +. b))
    | Num a, Sub, Num b -> Some (Num (a -. b))
    | Num a, Mul, Num b -> Some (Num (a *. b))
    | Num a, Div, Num b when b <> 0. -> Some (Num (a /. b))
    | _ -> None

  let fold_value_expr : type a.
      a ops ->
      a Values.calc ->
      Values.calc_op ->
      a Values.calc ->
      a Values.calc option =
   fun ops left op right ->
    let open Values in
    match (left, op, right) with
    | Val a, _, Val b ->
        Option.map (fun value -> Val value) (ops.combine_values a op b)
    | Val value, _, Num n ->
        Option.map (fun value -> Val value) (ops.combine_value_num value op n)
    | Num n, Mul, Val value ->
        Option.map
          (fun value -> Val value)
          (ops.combine_value_num value Values.Mul n)
    | _ -> None

  (* Value-independent multiplicative identities ([x * 0], [x * 1], ...) live in
     [Values.calc_identity], shared with the AST evaluators so the paths never
     drift. They hold for any operand, so they apply to an unresolved [var()]
     too; [ops.is_zero] collapses a literal zero ([0px], not just [0]). Additive
     terms fold only through the preceding same-type value combination. *)
  let fold_identity_expr ops left op right =
    Values.calc_identity ~zero:ops.zero ~is_zero:ops.is_zero left op right

  let fold_calc_expr ops left op right =
    match fold_num_expr left op right with
    | Some folded -> folded
    | None -> (
        match fold_value_expr ops left op right with
        | Some folded -> folded
        | None -> (
            match fold_identity_expr ops left op right with
            | Some folded -> folded
            | None -> Values.Expr (left, op, right)))

  let fold_calc_wrapper wrap reduced =
    let open Values in
    match reduced with
    | (Val _ | Num _ | Math_const _ | Var _) as leaf -> leaf
    | reduced -> wrap reduced

  let rec fold_calc : type a. a ops -> a Values.calc -> a Values.calc =
   fun ops calc ->
    let open Values in
    match calc with
    | Expr (left, op, right) ->
        let left = fold_calc ops left in
        let right = fold_calc ops right in
        fold_calc_expr ops left op right
    | Nested inner ->
        fold_calc_wrapper (fun v -> Nested v) (fold_calc ops inner)
    | Parens inner ->
        fold_calc_wrapper (fun v -> Parens v) (fold_calc ops inner)
    | leaf -> leaf

  (* Apply only the value-independent identities ([fold_identity_expr]), leaving
     every other subexpression verbatim. Used when a calc keeps an unresolved
     [var()]: the var() reference is preserved, but [x * 0] / [x * 1] still
     simplify because they never read the variable. *)
  let rec fold_calc_identities : type a. a ops -> a Values.calc -> a Values.calc
      =
   fun ops calc ->
    let open Values in
    match calc with
    | Expr (left, op, right) -> (
        let left = fold_calc_identities ops left in
        let right = fold_calc_identities ops right in
        match fold_identity_expr ops left op right with
        | Some folded -> folded
        | None -> Expr (left, op, right))
    | Nested inner ->
        fold_calc_wrapper (fun v -> Nested v) (fold_calc_identities ops inner)
    | Parens inner ->
        fold_calc_wrapper (fun v -> Parens v) (fold_calc_identities ops inner)
    | leaf -> leaf

  let calc_result_to_value (type a) (ops : a ops) simplify ~visited
      (calc : a Values.calc) : a =
    match calc with
    | Values.Val value -> simplify ~authored:false ~visited value
    | Values.Num n -> (
        match ops.of_unitless_number n with
        | Some value -> value
        | None -> ops.of_calc (Values.Num n))
    | Values.Var var -> ops.of_var var
    | calc -> ops.of_calc calc

  let simplify_calc_value ops simplify simplify_calc ~authored ~visited calc =
    let preserve = authored && contains_var calc in
    simplify_calc ~preserve ~visited calc
    |> calc_result_to_value ops simplify ~visited

  let simplify_plain_value ops simplify simplify_calc ~visited value =
    let simplify_authored = simplify ~authored:true in
    let simplify_calc_authored = simplify_calc ~preserve:false in
    let value =
      ops.simplify_leaf simplify_authored simplify_calc_authored ~visited value
    in
    ops.normalize_value value

  let simplify_calc_non_var ops simplify simplify_calc ~authored ~visited value
      =
    match ops.as_calc value with
    | Some calc ->
        simplify_calc_value ops simplify simplify_calc ~authored ~visited calc
    | None -> simplify_plain_value ops simplify simplify_calc ~visited value

  (* A binding that resolves to a reference of its own is a cycle, which CSS
     Variables 1 sec. 3 makes the guaranteed-invalid value, so the consumer's
     fallback answers for it. This is the reading {!Var_residual} already takes
     for a value with no calc arm. *)
  let calc_var_result (type a) (ops : a ops) simplify_resolved ~visited
      (var : a Values.var) (value : a) : a =
    if Option.is_none (ops.as_var value) then value
    else
      match (var.Values.fallback : a Values.fallback) with
      | Values.Fallback fb -> simplify_resolved ~authored:false ~visited fb
      | Values.Syntax_fallback _ | Values.Var_fallback _ | Values.Empty
      | Values.Empty2 | Values.None ->
          value

  let simplify_calc_var ops resolve_var simplify_var_record simplify_resolved
      ~visited var =
    match resolve_var ~simplify:simplify_resolved ~visited var with
    | Some value -> calc_var_result ops simplify_resolved ~visited var value
    | None ->
        ops.of_var
          (simplify_var_record ~simplify:simplify_resolved ~visited var)

  let run_simplify ops ~resolve_var ~simplify_var_record value =
    let rec calc_of_value ~visited value =
      match simplify ~authored:false ~visited value |> ops.as_calc with
      | Some inner -> Values.Nested (simplify_calc ~visited inner)
      | None -> Values.Val (simplify ~authored:false ~visited value)
    and walk_calc ~visited calc =
      let simplify_resolved ~authored:_ = simplify ~authored:false in
      match calc with
      | Values.Val value -> calc_of_value ~visited value
      | Values.Var var -> (
          match resolve_var ~simplify:simplify_resolved ~visited var with
          | Some value ->
              calc_of_value ~visited
                (calc_var_result ops simplify_resolved ~visited var value)
          | None ->
              Values.Var
                (simplify_var_record ~simplify:simplify_resolved ~visited var))
      | Values.Num _ as leaf -> leaf
      | Values.Math_const _ as leaf -> leaf
      | Values.Math_fn _ as leaf -> leaf
      | Values.Sibling_index -> Values.Sibling_index
      | Values.Sibling_count -> Values.Sibling_count
      | Values.Nested inner -> Values.Nested (walk_calc ~visited inner)
      | Values.Parens inner -> Values.Parens (walk_calc ~visited inner)
      | Values.Expr (left, op, right) ->
          Values.Expr (walk_calc ~visited left, op, walk_calc ~visited right)
    and simplify_calc ?(preserve = false) ~visited calc =
      let calc = walk_calc ~visited calc in
      if preserve && contains_var calc then fold_calc_identities ops calc
      else fold_calc ops calc
    and simplify ~authored ~visited value =
      let simplify_resolved ~authored:_ = simplify ~authored:false in
      let run_calc ~preserve ~visited calc =
        simplify_calc ~preserve ~visited calc
      in
      match ops.as_var value with
      | Some var ->
          simplify_calc_var ops resolve_var simplify_var_record
            simplify_resolved ~visited var
      | None ->
          simplify_calc_non_var ops simplify run_calc ~authored ~visited value
    in
    simplify ~authored:true ~visited:[] value

  let simplify (type a) ?resolve_fallback ?layer_order ?layer cascade
      (ops : a ops) (value : a) : a =
    Var_residual.with_resolver ?resolve_fallback ?layer_order ?layer cascade
      ~read_custom:ops.read_custom
    @@ fun ~resolve_var ~simplify_var_record ~unreadable:_ ->
    (* A calc leaf already leaves the residual where the reference does not
       resolve, whatever the reason, so it needs no separate answer. *)
    run_simplify ops ~resolve_var ~simplify_var_record value
end
