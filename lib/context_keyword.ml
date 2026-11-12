open Context_intf
open Context_core
open Context_residual
open Context_length

let simplify_transition_property_value ?layer_order ?layer ctx value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : Properties.transition_property_value Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.transition_property_value) ->
            Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_components Properties.read_transition_property_value;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_animation_name ?layer_order ?layer ctx value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : Properties.animation_name Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.animation_name) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_animation_name;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_font_family ?layer_order ?layer ctx value =
  let simplify_leaf simplify ~authored:_ ~visited
      (value : Properties.font_family) : Properties.font_family =
    match value with
    | Properties.List items ->
        (Properties.List (List.map (simplify ~authored:false ~visited) items)
          : Properties.font_family)
    | _ -> value
  in
  let ops : Properties.font_family Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.font_family) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_font_family;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_font_src ?layer_order ?layer ctx value =
  let simplify_leaf simplify ~authored ~visited entries =
    List.concat_map
      (function
        | Font_face.Var var -> (
            match simplify ~authored ~visited [ Font_face.Var var ] with
            | [ (Font_face.Var _ as unresolved) ] -> [ unresolved ]
            | entries -> entries)
        | entry -> [ entry ])
      entries
  in
  let ops : Font_face.src Var_residual.ops =
    {
      Var_residual.as_var =
        (function [ Font_face.Var var ] -> Some var | _ -> None);
      of_var = (fun var -> [ Font_face.Var var ]);
      read_custom = read_custom_value Properties.Font_src Font_face.read_src;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let animation_name_of_timing_var ?layer_order ?layer ctx
    (var : Properties.timing_function Values.var) =
  Option.bind
    (lookup_custom_property ?layer ?layer_order ctx var.Values.name)
    (read_custom_components Properties.read_animation_name)

let resolve_animation_name_from_timing ?layer_order ?layer ctx
    (shorthand : Properties.animation_shorthand) =
  match (shorthand.name, shorthand.timing_function) with
  | None, Some (Properties.Var var) -> (
      match animation_name_of_timing_var ?layer_order ?layer ctx var with
      | Some name -> (Some name, None)
      | None -> (shorthand.name, shorthand.timing_function))
  | _ -> (shorthand.name, shorthand.timing_function)

let rebuild_animation_shorthand ?layer_order ?layer ctx duration
    (shorthand : Properties.animation_shorthand) =
  let name, timing_function =
    resolve_animation_name_from_timing ?layer_order ?layer ctx shorthand
  in
  let shorthand : Properties.animation_shorthand =
    {
      name = Option.map (simplify_animation_name ?layer_order ?layer ctx) name;
      duration = Option.map duration shorthand.duration;
      timing_function;
      delay = Option.map duration shorthand.delay;
      iteration_count = shorthand.iteration_count;
      direction = shorthand.direction;
      fill_mode = shorthand.fill_mode;
      play_state = shorthand.play_state;
      timeline = shorthand.timeline;
    }
  in
  (Properties.Shorthand shorthand : Properties.animation)

let simplify_animation_item ?layer_order ?layer ctx duration value =
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.animation) =
    match value with
    | Properties.Shorthand shorthand ->
        rebuild_animation_shorthand ?layer_order ?layer ctx duration shorthand
    | value -> value
  in
  let ops : Properties.animation Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.animation) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Animation : Properties.animation Properties.kind)
          Properties.read_animation;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_transition_item ?layer_order ?layer ctx duration value =
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.transition) =
    match value with
    | Properties.Shorthand (shorthand : Properties.transition_shorthand) ->
        let shorthand : Properties.transition_shorthand =
          {
            property =
              simplify_transition_property_value ?layer_order ?layer ctx
                shorthand.property;
            duration = Option.map duration shorthand.duration;
            timing_function = shorthand.timing_function;
            delay = Option.map duration shorthand.delay;
            behavior = shorthand.behavior;
          }
        in
        (Properties.Shorthand shorthand : Properties.transition)
    | value -> value
  in
  let ops : Properties.transition Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.transition) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_transition;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_var_value ?layer_order ?layer ctx ~as_var ~of_var ~read_custom
    value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : _ Var_residual.ops =
    { Var_residual.as_var; of_var; read_custom; simplify_leaf }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_display ~layer_order ?layer ctx (value : Properties.display) =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.display) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_display)
    value

let simplify_position ~layer_order ?layer ctx (value : Properties.position) =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.position) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_position)
    value

let simplify_visibility ~layer_order ?layer ctx (value : Properties.visibility)
    =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.visibility) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_visibility)
    value

let simplify_clear ~layer_order ?layer ctx (value : Properties.clear) =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.clear) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_clear)
    value

let simplify_float_side ~layer_order ?layer ctx (value : Properties.float_side)
    =
  simplify_var_value ~layer_order ?layer ctx
    ~as_var:(function
      | (Properties.Var var : Properties.float_side) -> Some var | _ -> None)
    ~of_var:(fun var -> Properties.Var var)
    ~read_custom:(read_custom_components Properties.read_float_side)
    value

let simplify_lengths_value ~layer_order ?layer ctx length_ctx value =
  let simplify_one = Length.simplify ~layer_order ?layer ctx length_ctx in
  match value with
  | [ (Values.Var var : Values.length) ]
    when var.Values.runtime || List.mem var.Values.name ctx.runtime_vars ->
      [ simplify_one (Values.Var var) ]
  | [ (Values.Var var : Values.length) ] -> (
      match
        Option.bind
          (lookup_custom_property ?layer ~layer_order ctx var.Values.name)
          (read_custom_components Values.read_margin_shorthand)
      with
      | Some lengths -> List.map simplify_one lengths
      | None -> (
          match var.Values.fallback with
          | Values.Fallback fallback -> [ simplify_one fallback ]
          | _ -> [ simplify_one (Values.Var var) ]))
  | _ -> List.map simplify_one value
