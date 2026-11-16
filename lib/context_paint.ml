open Context_intf
open Context_residual
open Context_length
open Context_match
open Context_numeric

let simplify_component ?layer_order ?layer ctx value =
  let combine_values (left : Values.component) op (right : Values.component) =
    match (left, op, right) with
    | Values.Num a, Values.Add, Values.Num b ->
        Some (Values.Num (a +. b) : Values.component)
    | Values.Num a, Values.Sub, Values.Num b ->
        Some (Values.Num (a -. b) : Values.component)
    | Values.Pct a, Values.Add, Values.Pct b ->
        Some (Values.Pct (a +. b) : Values.component)
    | Values.Pct a, Values.Sub, Values.Pct b ->
        Some (Values.Pct (a -. b) : Values.component)
    | _ -> None
  in
  let combine_value_num (value : Values.component) op num =
    match (value, op) with
    | Values.Num n, Values.Mul ->
        Some (Values.Num (n *. num) : Values.component)
    | Values.Num n, Values.Div when num <> 0. ->
        Some (Values.Num (n /. num) : Values.component)
    | Values.Pct n, Values.Mul ->
        Some (Values.Pct (n *. num) : Values.component)
    | Values.Pct n, Values.Div when num <> 0. ->
        Some (Values.Pct (n /. num) : Values.component)
    | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.component Calc_residual.ops =
    {
      Calc_residual.of_unitless_number = (fun n -> Some (Values.Num n));
      zero = Values.Num 0.;
      is_zero = (fun _ -> false);
      combine_values;
      combine_value_num;
      normalize_value = Fun.id;
      as_var =
        (function (Values.Var var : Values.component) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc = (function Values.Calc calc -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_components Values.read_component;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer ctx ops value

let simplify_percentage ?layer_order ?layer ctx value =
  let to_number (value : Values.percentage) =
    match value with
    | Values.Pct n -> Some n
    | Values.Num n -> Some n
    | _ -> None
  in
  let of_number n = (Values.Pct n : Values.percentage) in
  (* A [<percentage>] slot also accepts a bare [<number>] (e.g. an oklch
     lightness: [.7] and [70%] are equal, but [.7%] is not [.7]). Canonicalise
     the float spelling without collapsing the [Num] / [Pct] distinction, which
     [of_number] would, since it always reissues a [Pct]. *)
  let normalize_value (value : Values.percentage) : Values.percentage =
    let canon n =
      try float_of_string (Pp.string_of_float n) with Failure _ -> n
    in
    match value with
    | Values.Num n -> Values.Num (canon n)
    | Values.Pct n -> Values.Pct (canon n)
    | other -> other
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.percentage Calc_residual.ops =
    {
      of_unitless_number = (fun n -> Some (Values.Num n));
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values = combine_numeric_values ~to_number ~of_number;
      combine_value_num = combine_numeric_value_num ~to_number ~of_number;
      normalize_value;
      as_var =
        (function (Values.Var var : Values.percentage) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc = (function Values.Calc calc -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_components Values.read_percentage;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer ctx ops value

let simplify_alpha ?layer_order ?layer ctx value =
  let alpha_num n : Values.alpha = Values.Num (Float.max 0. (Float.min 1. n)) in
  let alpha_pct n : Values.alpha =
    Values.Pct (Float.max 0. (Float.min 100. n))
  in
  let combine_values (left : Values.alpha) op (right : Values.alpha) =
    match (left, op, right) with
    | Values.Num a, Values.Add, Values.Num b -> Some (alpha_num (a +. b))
    | Values.Num a, Values.Sub, Values.Num b -> Some (alpha_num (a -. b))
    | Values.Pct a, Values.Add, Values.Pct b -> Some (alpha_pct (a +. b))
    | Values.Pct a, Values.Sub, Values.Pct b -> Some (alpha_pct (a -. b))
    | _ -> None
  in
  let combine_value_num (value : Values.alpha) op num =
    match (value, op) with
    | Values.Num a, Values.Mul -> Some (alpha_num (a *. num))
    | Values.Num a, Values.Div when num <> 0. -> Some (alpha_num (a /. num))
    | Values.Pct a, Values.Mul -> Some (alpha_pct (a *. num))
    | Values.Pct a, Values.Div when num <> 0. -> Some (alpha_pct (a /. num))
    | _ -> None
  in
  let to_number (value : Values.alpha) =
    match value with Values.Num n -> Some n | _ -> None
  in
  let simplify_leaf _simplify _simplify_calc ~visited:_ value = value in
  let ops : Values.alpha Calc_residual.ops =
    {
      of_unitless_number = (fun n -> Some (alpha_num n));
      zero = Values.Num 0.;
      is_zero =
        (fun v -> match to_number v with Some n -> n = 0. | None -> false);
      combine_values;
      combine_value_num;
      normalize_value = normalize_numeric_value ~to_number ~of_number:alpha_num;
      as_var =
        (function (Values.Var var : Values.alpha) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      as_calc =
        (function (Values.Calc calc : Values.alpha) -> Some calc | _ -> None);
      of_calc = (fun calc -> Values.Calc calc);
      read_custom = read_custom_components Values.read_alpha;
      simplify_leaf;
    }
  in
  Calc_residual.simplify ?layer_order ?layer ctx ops value

let simplify_channel ?layer_order ?layer ctx value =
  let simplify_leaf _simplify ~authored:_ ~visited:_ value = value in
  let ops : Values.channel Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.channel) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_components Values.read_channel;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_rgb ?layer_order ?layer ctx value =
  let channel = simplify_channel ?layer_order ?layer ctx in
  let simplify_leaf _simplify ~authored:_ ~visited:_ (value : Values.rgb) =
    match value with
    | Values.Channels { r; g; b } ->
        (Values.Channels { r = channel r; g = channel g; b = channel b }
          : Values.rgb)
    | value -> value
  in
  let ops : Values.rgb Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.rgb) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_components Values.read_rgb;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_hue ?layer_order ?layer ctx value =
  let angle_value = simplify_angle ?layer_order ?layer ctx in
  let simplify_leaf _simplify ~authored:_ ~visited:_ (value : Values.hue) =
    match value with
    | Values.Angle (value : Values.angle) ->
        (Values.Angle (angle_value value) : Values.hue)
    | value -> value
  in
  let ops : Values.hue Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.hue) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_components Values.read_hue;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer ctx ops value

let simplify_rgb_color component percentage alpha_value rgb hue
    (value : Values.color) =
  match value with
  | Values.Rgb value -> Values.Rgb (rgb value)
  | Values.Rgba { rgb = value; a } ->
      Values.Rgba { rgb = rgb value; a = alpha_value a }
  | Values.Hsl { h; s; l; a } ->
      Values.Hsl
        { h = hue h; s = percentage s; l = percentage l; a = alpha_value a }
  | Values.Hwb { h; w; b; a } ->
      Values.Hwb
        { h = hue h; w = percentage w; b = percentage b; a = alpha_value a }
  | Values.Color { space; components; alpha } ->
      Values.Color
        {
          space;
          components = List.map component components;
          alpha = alpha_value alpha;
        }
  | value -> value

let simplify_lab_color percentage alpha_value hue (value : Values.color) =
  match value with
  | Values.Lab { l; a; b; alpha = value } ->
      Values.Lab
        { l = Option.map percentage l; a; b; alpha = alpha_value value }
  | Values.Oklch { l; c; h; alpha = value } ->
      Values.Oklch
        { l = Option.map percentage l; c; h = hue h; alpha = alpha_value value }
  | Values.Oklab { l; a; b; alpha = value } ->
      Values.Oklab
        { l = Option.map percentage l; a; b; alpha = alpha_value value }
  | Values.Lch { l; c; h; alpha = value } ->
      Values.Lch
        { l = Option.map percentage l; c; h = hue h; alpha = alpha_value value }
  | value -> value

let simplify_nested_color ctx percentage simplify ~authored ~visited
    (value : Values.color) =
  match value with
  | Values.Current -> (
      match ctx.current_color with
      | Some color -> color
      | None -> Values.Current)
  | Values.Contrast_color color ->
      Values.Contrast_color (simplify ~authored ~visited color)
  | Values.Light_dark (light, dark) ->
      Values.Light_dark
        (simplify ~authored ~visited light, simplify ~authored ~visited dark)
  | Values.Attribute (name, fallback) ->
      Values.Attribute (name, Option.map (simplify ~authored ~visited) fallback)
  | Values.Mix mix ->
      Values.Mix
        {
          mix with
          color1 = simplify ~authored ~visited mix.color1;
          percent1 = Option.map percentage mix.percent1;
          color2 = simplify ~authored ~visited mix.color2;
          percent2 = Option.map percentage mix.percent2;
        }
  | value -> value

let simplify_color ?(layer_order = []) ?layer ctx (value : Values.color) :
    Values.color =
  let component = simplify_component ~layer_order ?layer ctx in
  let percentage = simplify_percentage ~layer_order ?layer ctx in
  let alpha_value = simplify_alpha ~layer_order ?layer ctx in
  let rgb = simplify_rgb ~layer_order ?layer ctx in
  let hue = simplify_hue ~layer_order ?layer ctx in
  let simplify_leaf simplify ~authored ~visited value =
    value
    |> simplify_rgb_color component percentage alpha_value rgb hue
    |> simplify_lab_color percentage alpha_value hue
    |> simplify_nested_color ctx percentage simplify ~authored ~visited
  in
  let ops : Values.color Var_residual.ops =
    {
      Var_residual.as_var =
        (function (Values.Var var : Values.color) -> Some var | _ -> None);
      of_var = (fun var -> Values.Var var);
      read_custom = read_custom_value Properties.Color Values.read_color;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let simplify_shadow_leaf length color simplify ~authored ~visited
    (value : Properties.shadow) =
  let simplify_body
      ({ h_offset; v_offset; blur; spread; color = shadow_color } :
        Properties.shadow_body) : Properties.shadow_body =
    {
      h_offset = length authored h_offset;
      v_offset = length authored v_offset;
      blur = Option.map (length authored) blur;
      spread = Option.map (length authored) spread;
      color = Option.map color shadow_color;
    }
  in
  match value with
  | Properties.Shadow body ->
      (Properties.Shadow (simplify_body body) : Properties.shadow)
  | Properties.Inset (Properties.Body body) ->
      (Properties.Inset (Properties.Body (simplify_body body))
        : Properties.shadow)
  | Properties.Inset (Properties.Toggle { name; no_fallback; body }) ->
      (Properties.Inset
         (Properties.Toggle { name; no_fallback; body = simplify_body body })
        : Properties.shadow)
  | Properties.List shadows ->
      (Properties.List (List.map (simplify ~authored ~visited) shadows)
        : Properties.shadow)
  | value -> value

let simplify_shadow ?layer_order ?layer cascade length_ctx value =
  let length authored =
    Length.simplify ~preserve_authored_calc:authored ?layer_order ?layer cascade
      length_ctx
  in
  let color = simplify_color ?layer_order ?layer cascade in
  let simplify_leaf = simplify_shadow_leaf length color in
  let ops : Properties.shadow Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.shadow) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value
          (Properties.Shadow : Properties.shadow Properties.kind)
          Properties.read_shadow;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let simplify_border_radius ?layer_order ?layer cascade length_ctx value =
  let length_percentage =
    simplify_length_percentage ?layer_order ?layer cascade length_ctx
  in
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.border_radius) =
    match value with
    | Properties.Radius { horizontal; vertical } ->
        (Properties.Radius
           {
             horizontal = List.map length_percentage horizontal;
             vertical = Option.map (List.map length_percentage) vertical;
           }
          : Properties.border_radius)
    | value -> value
  in
  let ops : Properties.border_radius Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.border_radius) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_border_radius;
      simplify_leaf;
    }
  in
  Var_residual.simplify ?layer_order ?layer cascade ops value

let resolve_url_leaf (ctx : t) url =
  let loader = { base_url = ctx.base_url; imports = [] } in
  match Url.resolve loader url with Ok resolved -> resolved | Error _ -> url

let gradient_stop_of_color_var (var : Values.color Values.var) :
    Properties.gradient_stop Values.var =
  {
    Values.name = var.name;
    fallback =
      map_var_fallback
        (fun color -> Properties.Color_percentage (color, None, None))
        var.fallback;
    default =
      Option.map
        (fun color -> Properties.Color_percentage (color, None, None))
        var.default;
    layer = var.layer;
    meta = var.meta;
    runtime = var.runtime;
  }

let simplify_color_var_gradient_stop (color : Values.color -> Values.color)
    simplify ~authored ~visited var =
  let stop_var = gradient_stop_of_color_var var in
  let stop =
    simplify ~authored ~visited
      (Properties.Var stop_var : Properties.gradient_stop)
  in
  match (stop : Properties.gradient_stop) with
  | Properties.Var unresolved when unresolved.Values.name = stop_var.name ->
      Properties.Color_percentage (color (Values.Var var), None, None)
  | stop -> stop

let simplify_gradient_stop_leaf ~layer_order ?layer ctx length length_percentage
    percentage color simplify ~authored ~visited
    (value : Properties.gradient_stop) =
  match value with
  | Properties.Color_percentage (Values.Var var, None, None) ->
      simplify_color_var_gradient_stop color simplify ~authored ~visited var
  | Properties.Color_percentage (color_value, first, second) ->
      (Properties.Color_percentage
         ( color color_value,
           Option.map length_percentage first,
           Option.map length_percentage second )
        : Properties.gradient_stop)
  | Properties.Color_length (color_value, first, second) ->
      (Properties.Color_length
         (color color_value, Option.map length first, Option.map length second)
        : Properties.gradient_stop)
  | Properties.Length value ->
      (Properties.Length (length value) : Properties.gradient_stop)
  | Properties.Channel value ->
      (Properties.Channel (simplify_channel ~layer_order ?layer ctx value)
        : Properties.gradient_stop)
  | Properties.Percentage value ->
      (Properties.Percentage (percentage value) : Properties.gradient_stop)
  | Properties.List values ->
      (Properties.List (List.map (simplify ~authored ~visited) values)
        : Properties.gradient_stop)
  | value -> value

let simplify_gradient_stop ?(layer_order = []) ?layer ctx
    (value : Properties.gradient_stop) =
  let length_ctx = Length.of_t ctx in
  let length = Length.simplify ~layer_order ?layer ctx length_ctx in
  let length_percentage =
    simplify_length_percentage ~layer_order ?layer ctx length_ctx
  in
  let percentage = simplify_percentage ~layer_order ?layer ctx in
  let color = simplify_color ~layer_order ?layer ctx in
  let simplify_leaf =
    simplify_gradient_stop_leaf ~layer_order ?layer ctx length length_percentage
      percentage color
  in
  let ops : Properties.gradient_stop Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.gradient_stop) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value Properties.Gradient_stop
          Properties.read_gradient_stop_list;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let resolve_image_set_option ctx (option : Properties.image_set_option) =
  let source =
    match option.Properties.source with
    | Properties.Url url ->
        (Properties.Url (resolve_url_leaf ctx url)
          : Properties.image_set_source)
    | Properties.String _ as source -> source
  in
  { option with Properties.source }

let simplify_background_gradient_stops
    (gradient_stop : Properties.gradient_stop -> Properties.gradient_stop)
    (values : Properties.gradient_stop list) =
  List.concat_map
    (fun value ->
      match gradient_stop value with
      | Properties.List values -> values
      | value -> [ value ])
    values

let simplify_background_gradient_stop_var
    (gradient_stop : Properties.gradient_stop -> Properties.gradient_stop)
    (var : Properties.gradient_stop Values.var) =
  {
    var with
    Values.fallback = map_var_fallback gradient_stop var.Values.fallback;
    default = Option.map gradient_stop var.default;
  }

let simplify_background_image_leaf ctx
    (gradient_stop : Properties.gradient_stop -> Properties.gradient_stop)
    simplify ~authored ~visited (value : Properties.background_image) =
  let gradient_stops = simplify_background_gradient_stops gradient_stop in
  let gradient_stop_var = simplify_background_gradient_stop_var gradient_stop in
  match value with
  | Properties.Url url ->
      (Properties.Url (resolve_url_leaf ctx url) : Properties.background_image)
  | Properties.Quoted (url, quote) ->
      Properties.Quoted (resolve_url_leaf ctx url, quote)
  | Properties.Linear_gradient (direction, stops) ->
      Properties.Linear_gradient (direction, gradient_stops stops)
  | Properties.Radial_gradient (config, stops) ->
      Properties.Radial_gradient (config, gradient_stops stops)
  | Properties.Conic_gradient (config, stops) ->
      Properties.Conic_gradient (config, gradient_stops stops)
  | Properties.Linear_gradient_var var ->
      Properties.Linear_gradient_var (gradient_stop_var var)
  | Properties.Radial_gradient_var var ->
      Properties.Radial_gradient_var (gradient_stop_var var)
  | Properties.Conic_gradient_var var ->
      Properties.Conic_gradient_var (gradient_stop_var var)
  | Properties.Image_set options ->
      Properties.Image_set (List.map (resolve_image_set_option ctx) options)
  | Properties.Cross_fade options ->
      Properties.Cross_fade
        (List.map
           (fun (option : Properties.cross_fade_option) ->
             {
               option with
               Properties.image =
                 simplify ~authored ~visited option.Properties.image;
             })
           options)
  | Properties.List images ->
      Properties.List (List.map (simplify ~authored ~visited) images)
  | value -> value

let simplify_background_image ?(layer_order = []) ?layer ctx
    (value : Properties.background_image) : Properties.background_image =
  let gradient_stop = simplify_gradient_stop ~layer_order ?layer ctx in
  let simplify_leaf = simplify_background_image_leaf ctx gradient_stop in
  let ops : Properties.background_image Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.background_image) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom =
        read_custom_value Properties.Background_image
          Properties.read_background_image;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value

let simplify_background ?(layer_order = []) ?layer ctx
    (value : Properties.background) : Properties.background =
  let color = simplify_color ~layer_order ?layer ctx in
  let image = simplify_background_image ~layer_order ?layer ctx in
  let simplify_leaf _simplify ~authored:_ ~visited:_
      (value : Properties.background) =
    match value with
    | Properties.Shorthand (shorthand : Properties.background_shorthand) ->
        let shorthand : Properties.background_shorthand =
          {
            color = Option.map color shorthand.color;
            image = Option.map image shorthand.image;
            position = shorthand.position;
            size = shorthand.size;
            repeat = shorthand.repeat;
            attachment = shorthand.attachment;
            clip = shorthand.clip;
            origin = shorthand.origin;
          }
        in
        (Properties.Shorthand shorthand : Properties.background)
    | value -> value
  in
  let ops : Properties.background Var_residual.ops =
    {
      Var_residual.as_var =
        (function
        | (Properties.Var var : Properties.background) -> Some var
        | _ -> None);
      of_var = (fun var -> Properties.Var var);
      read_custom = read_custom_components Properties.read_background;
      simplify_leaf;
    }
  in
  Var_residual.simplify ~layer_order ?layer ctx ops value
