open Context_residual
open Context_length

let simplify_length_percentage ?layer_order ?layer cascade length_ctx value =
  let to_px = function
    | Values.Length length -> Length.to_px length_ctx length
    | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ = function
    | Values.Length length ->
        Values.Length
          (Length.simplify ?layer_order ?layer cascade length_ctx length)
    | value -> value
  in
  let ops =
    let of_number px =
      Values.Length (if Float.equal px 0. then Values.Zero else Values.Px px)
    in
    let combine_values (left : Values.length_percentage) op
        (right : Values.length_percentage) =
      match (left, op, right) with
      | Values.Pct a, Values.Add, Values.Pct b ->
          Some (Values.Pct (a +. b) : Values.length_percentage)
      | Values.Pct a, Values.Sub, Values.Pct b ->
          Some (Values.Pct (a -. b) : Values.length_percentage)
      | _ -> combine_numeric_values ~to_number:to_px ~of_number left op right
    in
    let combine_value_num (value : Values.length_percentage) op num =
      match (value, op) with
      | Values.Pct n, Values.Mul ->
          Some (Values.Pct (n *. num) : Values.length_percentage)
      | Values.Pct n, Values.Div when num <> 0. ->
          Some (Values.Pct (n /. num) : Values.length_percentage)
      | _ -> combine_numeric_value_num ~to_number:to_px ~of_number value op num
    in
    {
      Calc_residual.of_unitless_number = (fun _ -> None);
      zero = Values.Val (Values.Length Values.Zero);
      is_zero = (fun v -> match to_px v with Some n -> n = 0. | None -> false);
      combine_values;
      combine_value_num;
      normalize_value = normalize_numeric_value ~to_number:to_px ~of_number;
      as_var = (function Values.Var var -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc = (function Values.Calc calc -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom =
        read_custom_value Properties.Length_percentage
          (Values.read_length_percentage ~with_keywords:true);
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let simplify_border_width ?layer_order ?layer cascade (length_ctx : Length.ctx)
    value =
  let to_number (value : Properties.border_width) =
    match value with
    | Properties.Px n -> Some n
    | Properties.Rem n -> Option.map (fun b -> n *. b) length_ctx.root_font_size
    | Properties.Em n ->
        Option.map (fun b -> n *. b) length_ctx.parent_font_size
    | Properties.Vw n ->
        Option.map (fun w -> n *. w /. 100.) length_ctx.viewport_width
    | Properties.Vh n ->
        Option.map (fun h -> n *. h /. 100.) length_ctx.viewport_height
    | Properties.Vmin n -> (
        match (length_ctx.viewport_width, length_ctx.viewport_height) with
        | Some w, Some h -> Some (n *. Float.min w h /. 100.)
        | _ -> None)
    | Properties.Vmax n -> (
        match (length_ctx.viewport_width, length_ctx.viewport_height) with
        | Some w, Some h -> Some (n *. Float.max w h /. 100.)
        | _ -> None)
    | Properties.Zero -> Some 0.
    | _ -> None
  in
  let of_number px = (Properties.Px px : Properties.border_width) in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Properties.border_width Calc_residual.ops =
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function
        | (Properties.Var var : Properties.border_width) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      as_calc =
        (function
        | (Properties.Calc calc : Properties.border_width) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Properties.Calc calc);
      read_custom = read_custom_components Properties.read_border_width;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let simplify_font_size ?layer_order ?layer cascade (length_ctx : Length.ctx)
    value =
  let length = Length.simplify ?layer_order ?layer cascade length_ctx in
  let to_number (value : Properties.font_size) =
    match value with
    | Properties.Length (Values.Pct n) ->
        Option.map (fun basis -> basis *. n /. 100.) length_ctx.parent_font_size
    | Properties.Length length -> Length.to_px length_ctx length
    | Properties.Pct n ->
        Option.map (fun basis -> basis *. n /. 100.) length_ctx.parent_font_size
    | _ -> None
  in
  let of_number px =
    (Properties.Length (Length.normalize_zero (Values.Px px))
      : Properties.font_size)
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_
      (value : Properties.font_size) =
    match value with
    | Properties.Length value ->
        (Properties.Length (length value) : Properties.font_size)
    | value -> value
  in
  let ops : Properties.font_size Calc_residual.ops =
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function
        | (Properties.Var var : Properties.font_size) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      as_calc =
        (function
        | (Properties.Calc calc : Properties.font_size) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Properties.Calc calc);
      read_custom =
        read_custom_value
          (Properties.Font_size : Properties.font_size Properties.kind)
          Properties.read_font_size;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

(* Only the <length-percentage> branch carries anything to simplify: a bare
   number is already a user-unit width. *)
let simplify_stroke_width ?layer_order ?layer cascade length_ctx
    (value : Properties.stroke_width) : Properties.stroke_width =
  match value with
  | Length lp ->
      let lp' =
        simplify_length_percentage ?layer_order ?layer cascade length_ctx lp
      in
      if lp' == lp then value else Length lp'
  | _ -> value

let simplify_opacity ?layer_order ?layer cascade value =
  let to_number = function
    | Properties.Opacity_number n -> Some n
    | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Properties.opacity Calc_residual.ops =
    let of_number n = Properties.Opacity_number n in
    {
      of_unitless_number = (fun n -> Some (Properties.Opacity_number n));
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function
        | (Properties.Var var : Properties.opacity) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      as_calc =
        (function
        | (Properties.Calc calc : Properties.opacity) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Properties.Calc calc);
      read_custom =
        read_custom_value
          (Properties.Opacity : Properties.opacity Properties.kind)
          Properties.read_opacity;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let degrees_of_angle (value : Values.angle) =
  let pi = 4. *. atan 1. in
  match value with
  | Values.Deg n -> Some n
  | Values.Rad n -> Some (n *. 180. /. pi)
  | Values.Turn n -> Some (n *. 360.)
  | Values.Grad n -> Some (n *. 0.9)
  | _ -> None

let simplify_angle ?layer_order ?layer cascade value =
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.angle Calc_residual.ops =
    let to_number = degrees_of_angle in
    let of_number deg = Values.Deg deg in
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function (Values.Var var : Values.angle) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function (Values.Calc calc : Values.angle) -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_value Properties.Angle Values.read_angle;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let seconds_of_duration (value : Values.duration) =
  match value with
  | Values.S n -> Some n
  | Values.Ms n -> Some (n /. 1000.)
  | _ -> None

let simplify_duration ?layer_order ?layer cascade value =
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.duration Calc_residual.ops =
    let to_number = seconds_of_duration in
    let of_number seconds = Values.S seconds in
    {
      of_unitless_number = (fun _ -> None);
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value = normalize_numeric_value ~to_number ~of_number;
      as_var =
        (function (Values.Var var : Values.duration) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function
        | (Values.Calc calc : Values.duration) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_value Properties.Duration Values.read_duration;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let number_percentage_num n : Values.number_percentage = Values.Num n
let number_percentage_pct n : Values.number_percentage = Values.Pct n

let combine_number_percentage_values (left : Values.number_percentage) op
    (right : Values.number_percentage) =
  match (left, op, right) with
  | Values.Num a, Values.Add, Values.Num b ->
      Some (number_percentage_num (a +. b))
  | Values.Num a, Values.Sub, Values.Num b ->
      Some (number_percentage_num (a -. b))
  | Values.Pct a, Values.Add, Values.Pct b ->
      Some (number_percentage_pct (a +. b))
  | Values.Pct a, Values.Sub, Values.Pct b ->
      Some (number_percentage_pct (a -. b))
  | _ -> None

let combine_number_percentage_value_num (value : Values.number_percentage) op
    num =
  match (value, op) with
  | Values.Num a, Values.Mul -> Some (number_percentage_num (a *. num))
  | Values.Num a, Values.Div when num <> 0. ->
      Some (number_percentage_num (a /. num))
  | Values.Pct a, Values.Mul -> Some (number_percentage_pct (a *. num))
  | Values.Pct a, Values.Div when num <> 0. ->
      Some (number_percentage_pct (a /. num))
  | _ -> None

let simplify_number_percentage ?layer_order ?layer cascade value =
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.number_percentage Calc_residual.ops =
    {
      Calc_residual.of_unitless_number =
        (fun n -> Some (number_percentage_num n));
      zero = Values.Num 0.;
      is_zero = (fun _ -> false);
      combine_values = combine_number_percentage_values;
      combine_value_num = combine_number_percentage_value_num;
      normalize_value = (fun value -> value);
      as_var =
        (function
        | (Values.Var var : Values.number_percentage) -> Some var
        | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function
        | (Values.Calc calc : Values.number_percentage) -> Some calc
        | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom =
        read_custom_value Properties.Number_percentage
          Values.read_number_percentage;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer cascade ops value

let simplify_rotate_value ?layer_order ?layer cascade value =
  let angle = simplify_angle ?layer_order ?layer cascade in
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.rotate_value) =
    match value with
    | Properties.Angle value ->
        (Properties.Angle (angle value) : Properties.rotate_value)
    | Properties.X value -> Properties.X (angle value)
    | Properties.Y value -> Properties.Y (angle value)
    | Properties.Z value -> Properties.Z (angle value)
    | Properties.Axis (x, y, z, value) -> Properties.Axis (x, y, z, angle value)
    | value -> value
  in
  let ops : Properties.rotate_value Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.rotate_value) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Rotate : Properties.rotate_value Properties.kind)
          Properties.read_rotate_value;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_translate_value ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let simplify_leaf _simplify ~authored ~visited:_
      (value : Properties.translate_value) =
    match value with
    | Properties.X value ->
        (Properties.X (length authored value) : Properties.translate_value)
    | Properties.XY (x, y) ->
        (Properties.XY (length authored x, length authored y)
          : Properties.translate_value)
    | Properties.XYZ (x, y, z) ->
        (Properties.XYZ (length authored x, length authored y, length authored z)
          : Properties.translate_value)
    | value -> value
  in
  let ops : Properties.translate_value Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.translate_value) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_translate_value;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_scale ?layer_order ?layer cascade value =
  let number_percentage =
    simplify_number_percentage ?layer_order ?layer cascade
  in
  let simplify_leaf _simplify ~authored:_ ~visited:_ (value : Properties.scale)
      =
    match value with
    | Properties.X value ->
        (Properties.X (number_percentage value) : Properties.scale)
    | Properties.XY (x, y) ->
        (Properties.XY (number_percentage x, number_percentage y)
          : Properties.scale)
    | Properties.XYZ (x, y, z) ->
        (Properties.XYZ
           (number_percentage x, number_percentage y, number_percentage z)
          : Properties.scale)
    | value -> value
  in
  let ops : Properties.scale Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.scale) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Scale : Properties.scale Properties.kind)
          Properties.read_scale;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_translate_transform length authored (value : Properties.transform)
    =
  match value with
  | Properties.Translate (x, y) ->
      (Properties.Translate (length authored x, Option.map (length authored) y)
        : Properties.transform)
  | Properties.Translate_x x ->
      (Properties.Translate_x (length authored x) : Properties.transform)
  | Properties.Translate_y y ->
      (Properties.Translate_y (length authored y) : Properties.transform)
  | Properties.Translate_z z ->
      (Properties.Translate_z (length authored z) : Properties.transform)
  | Properties.Translate_3d (x, y, z) ->
      (Properties.Translate_3d
         (length authored x, length authored y, length authored z)
        : Properties.transform)
  | value -> value

let simplify_rotate_transform angle (value : Properties.transform) =
  match value with
  | Properties.Rotate value ->
      (Properties.Rotate (angle value) : Properties.transform)
  | Properties.Rotate_x value ->
      (Properties.Rotate_x (angle value) : Properties.transform)
  | Properties.Rotate_y value ->
      (Properties.Rotate_y (angle value) : Properties.transform)
  | Properties.Rotate_z value ->
      (Properties.Rotate_z (angle value) : Properties.transform)
  | Properties.Rotate_3d (x, y, z, value) ->
      (Properties.Rotate_3d (x, y, z, angle value) : Properties.transform)
  | Properties.Rotate_axis (x, y, z, value) ->
      (Properties.Rotate_axis (x, y, z, angle value) : Properties.transform)
  | value -> value

let simplify_scale_transform number_percentage (value : Properties.transform) =
  match value with
  | Properties.Scale (x, y) ->
      (Properties.Scale (number_percentage x, Option.map number_percentage y)
        : Properties.transform)
  | Properties.Scale_space (x, y) ->
      (Properties.Scale_space (number_percentage x, number_percentage y)
        : Properties.transform)
  | Properties.Scale_x value ->
      (Properties.Scale_x (number_percentage value) : Properties.transform)
  | Properties.Scale_y value ->
      (Properties.Scale_y (number_percentage value) : Properties.transform)
  | Properties.Scale_z value ->
      (Properties.Scale_z (number_percentage value) : Properties.transform)
  | Properties.Scale_3d (x, y, z) ->
      (Properties.Scale_3d
         (number_percentage x, number_percentage y, number_percentage z)
        : Properties.transform)
  | value -> value

let simplify_transform_leaf length angle number_percentage simplify ~authored
    ~visited (value : Properties.transform) =
  match value with
  | Properties.Translate _ | Properties.Translate_x _ | Properties.Translate_y _
  | Properties.Translate_z _ | Properties.Translate_3d _ ->
      simplify_translate_transform length authored value
  | Properties.Rotate _ | Properties.Rotate_x _ | Properties.Rotate_y _
  | Properties.Rotate_z _ | Properties.Rotate_3d _ | Properties.Rotate_axis _ ->
      simplify_rotate_transform angle value
  | Properties.Scale _ | Properties.Scale_space _ | Properties.Scale_x _
  | Properties.Scale_y _ | Properties.Scale_z _ | Properties.Scale_3d _ ->
      simplify_scale_transform number_percentage value
  | Properties.Skew (x, y) ->
      (Properties.Skew (angle x, Option.map angle y) : Properties.transform)
  | Properties.Skew_x value ->
      (Properties.Skew_x (angle value) : Properties.transform)
  | Properties.Skew_y value ->
      (Properties.Skew_y (angle value) : Properties.transform)
  | Properties.Perspective value ->
      (Properties.Perspective (length authored value) : Properties.transform)
  | Properties.List values ->
      (Properties.List (List.map (simplify ~authored ~visited) values)
        : Properties.transform)
  | value -> value

let simplify_transform ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let angle = simplify_angle ?layer_order ?layer cascade in
  let number_percentage =
    simplify_number_percentage ?layer_order ?layer cascade
  in
  let simplify_leaf = simplify_transform_leaf length angle number_percentage in
  let ops : Properties.transform Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.transform) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_transform;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_filter ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let number_percentage =
    simplify_number_percentage ?layer_order ?layer cascade
  in
  let angle = simplify_angle ?layer_order ?layer cascade in
  let simplify_leaf simplify ~authored ~visited (value : Properties.filter) =
    match value with
    | Properties.Blur value ->
        (Properties.Blur (length authored value) : Properties.filter)
    | Properties.Brightness value ->
        (Properties.Brightness (number_percentage value) : Properties.filter)
    | Properties.Contrast value ->
        (Properties.Contrast (number_percentage value) : Properties.filter)
    | Properties.Grayscale value ->
        (Properties.Grayscale (number_percentage value) : Properties.filter)
    | Properties.Hue_rotate value ->
        (Properties.Hue_rotate (angle value) : Properties.filter)
    | Properties.Invert value ->
        (Properties.Invert (number_percentage value) : Properties.filter)
    | Properties.Opacity value ->
        (Properties.Opacity (number_percentage value) : Properties.filter)
    | Properties.Saturate value ->
        (Properties.Saturate (number_percentage value) : Properties.filter)
    | Properties.Sepia value ->
        (Properties.Sepia (number_percentage value) : Properties.filter)
    | Properties.List values ->
        (Properties.List (List.map (simplify ~authored ~visited) values)
          : Properties.filter)
    | value -> value
  in
  let ops : Properties.filter Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.filter) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Filter : Properties.filter Properties.kind)
          Properties.read_filter;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value
