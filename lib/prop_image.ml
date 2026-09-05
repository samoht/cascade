(* CSS Images 4: the shared <image> value layer. Gradients and their vendor
   variants, image-set(), cross-fade(), <position>, url() and colour
   interpolation, plus image-orientation, image-rendering and image-resolution.

   Every family that embeds an <image> or a <position> (backgrounds, masks,
   border-image, transform-origin, offset-path, clip-path) reads and prints
   through the definitions here.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common

let pp_hue_interpolation_method ctx = function
  | Shorter -> Pp.string ctx "shorter hue"
  | Longer -> Pp.string ctx "longer hue"
  | Increasing -> Pp.string ctx "increasing hue"
  | Decreasing -> Pp.string ctx "decreasing hue"

let pp_polar_with_hue ctx space (hue : hue_interpolation_method option) =
  Pp.string ctx "in ";
  Pp.string ctx space;
  match hue with
  | None -> ()
  | Some hue ->
      Pp.space ctx ();
      pp_hue_interpolation_method ctx hue

let rec pp_color_interpolation : color_interpolation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_color_interpolation ctx v
  | In_oklab -> Pp.string ctx "in oklab"
  | In_oklch hue -> pp_polar_with_hue ctx "oklch" hue
  | In_srgb -> Pp.string ctx "in srgb"
  | In_hsl hue -> pp_polar_with_hue ctx "hsl" hue
  | In_lab -> Pp.string ctx "in lab"
  | In_lch hue -> pp_polar_with_hue ctx "lch" hue

(* CSS Color 5 section 9.1: after a polar color space (lch / oklch / hsl / hwb),
   the [<color-interpolation-method>] may carry a trailing
   [<hue-interpolation-method>] followed by [hue]. *)
let read_hue_interpolation_method t =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some (("shorter" | "longer" | "increasing" | "decreasing") as kw) -> (
      Cursor.ws t;
      match Cursor.ident_opt t with
      | Some "hue" ->
          Some
            (match kw with
            | "shorter" -> Shorter
            | "longer" -> Longer
            | "increasing" -> Increasing
            | "decreasing" -> Decreasing
            | _ -> assert false)
      | _ ->
          Cursor.restore t snap;
          None)
  | _ ->
      Cursor.restore t snap;
      None

let read_color_interpolation (t : Cursor.t) : color_interpolation =
  Cursor.with_context t "color-interpolation" (fun () ->
      Cursor.expect_string "in" t;
      (* At the component-value level, [in oklab] lexes as two separate idents;
         [inoklab] lexes as a single ident and would fail [expect_string "in"]
         above, so no extra whitespace check is needed here. *)
      let space = Cursor.ident t in
      let hue () =
        Cursor.ws t;
        read_hue_interpolation_method t
      in
      match space with
      | "oklab" -> In_oklab
      | "oklch" -> In_oklch (hue ())
      | "srgb" -> In_srgb
      | "hsl" -> In_hsl (hue ())
      | "lab" -> In_lab
      | "lch" -> In_lch (hue ())
      | _ -> Cursor.err_invalid t "color-interpolation")

let rec pp_gradient_direction : gradient_direction Pp.t =
 fun ctx -> function
  | Default_direction -> Pp.string ctx "to bottom"
  | To_top -> Pp.string ctx "to top"
  | To_top_right -> Pp.string ctx "to top right"
  | To_right -> Pp.string ctx "to right"
  | To_bottom_right -> Pp.string ctx "to bottom right"
  | To_bottom -> Pp.string ctx "to bottom"
  | To_bottom_left -> Pp.string ctx "to bottom left"
  | To_left -> Pp.string ctx "to left"
  | To_top_left -> Pp.string ctx "to top left"
  | Angle a -> pp_angle ctx a
  | With_interpolation (Default_direction, interp) ->
      (* default direction is omitted: [in <interp>], not [to bottom in
         <interp>] *)
      pp_color_interpolation ctx interp
  | With_interpolation (dir, interp) ->
      pp_gradient_direction ctx dir;
      (* After calc() or var(), the closing ) is a delimiter token so no space
         is needed before "in oklab" in minified mode. After keywords like "to
         right", a space is always needed to separate tokens. *)
      (match dir with
      | Angle (Calc _) | Var _ -> Pp.sp ctx ()
      | _ -> Pp.space ctx ());
      pp_color_interpolation ctx interp
  | Var v -> pp_var pp_gradient_direction ctx v

let rec pp_webkit_gradient_direction : gradient_direction Pp.t =
 fun ctx -> function
  | Default_direction -> Pp.string ctx "bottom"
  | To_top -> Pp.string ctx "top"
  | To_top_right -> Pp.string ctx "top right"
  | To_right -> Pp.string ctx "right"
  | To_bottom_right -> Pp.string ctx "bottom right"
  | To_bottom -> Pp.string ctx "bottom"
  | To_bottom_left -> Pp.string ctx "bottom left"
  | To_left -> Pp.string ctx "left"
  | To_top_left -> Pp.string ctx "top left"
  | Angle a -> pp_angle ctx a
  | With_interpolation (dir, interp) ->
      pp_webkit_gradient_direction ctx dir;
      Pp.space ctx ();
      pp_color_interpolation ctx interp
  | Var v -> pp_var pp_webkit_gradient_direction ctx v

let rec pp_radial_shape : radial_shape Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_radial_shape ctx v
  | Circle -> Pp.string ctx "circle"
  | Ellipse -> Pp.string ctx "ellipse"

let rec pp_radial_size : radial_size Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_radial_size ctx v
  | Closest_side -> Pp.string ctx "closest-side"
  | Farthest_side -> Pp.string ctx "farthest-side"
  | Closest_corner -> Pp.string ctx "closest-corner"
  | Farthest_corner -> Pp.string ctx "farthest-corner"
  | Circle_radius l -> pp_length ctx l
  | Ellipse_radii (a, b) ->
      pp_length_percentage ~always:true ctx a;
      Pp.space ctx ();
      pp_length_percentage ~always:true ctx b

(* CSS Images 4 sec. 3.5.3 color-stop fixup: an omitted position on the first
   stop defaults to [0%], on the last stop to [100%]. The reverse holds for
   minify: drop an explicit [Pct 0.] from the first stop and [Pct 100.] from the
   last when there's no second-position carrier; the resolved color stop list
   stays identical. *)
let canonicalise_gradient_stops stops =
  let strip_pos stop =
    match stop with
    | Color_percentage (c, _, _) -> Color_percentage (c, None, None)
    | _ -> stop
  in
  let trailing_default stop expected =
    match stop with
    | Color_percentage (_, Some (Pct p), None) when p = expected -> true
    | _ -> false
  in
  let rec strip_last = function
    | [] -> []
    | [ last ] when trailing_default last 100. -> [ strip_pos last ]
    | x :: rest -> x :: strip_last rest
  in
  match stops with
  | [] -> []
  | first :: rest when trailing_default first 0. ->
      strip_pos first :: strip_last rest
  | _ -> strip_last stops

let pp_position_offset ctx (lp : Values.length_percentage) =
  pp_length_percentage ~always:true ctx lp

let rec pp_position_value : position_value Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Center -> Pp.string ctx "center"
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Left_top -> Pp.string ctx "left top"
  | Left_center -> Pp.string ctx "left center"
  | Left_bottom -> Pp.string ctx "left bottom"
  | Right_top -> Pp.string ctx "right top"
  | Right_center -> Pp.string ctx "right center"
  | Right_bottom -> Pp.string ctx "right bottom"
  | Center_top -> Pp.string ctx "center top"
  | Center_bottom -> Pp.string ctx "center bottom"
  | Top_left -> Pp.string ctx "top left"
  | Top_right -> Pp.string ctx "top right"
  | Bottom_left -> Pp.string ctx "bottom left"
  | Bottom_right -> Pp.string ctx "bottom right"
  | XY (a, b) ->
      pp_length ctx a;
      Pp.pct_sp ctx ();
      pp_length ctx b
  | Single l -> pp_length ctx l
  | Edge_offset_axis (edge, offset, axis) ->
      Pp.string ctx edge;
      Pp.space ctx ();
      pp_position_offset ctx offset;
      Pp.pct_sp ctx ();
      Pp.string ctx axis
  | Axis_edge_offset (axis, edge, offset) ->
      Pp.string ctx axis;
      Pp.space ctx ();
      Pp.string ctx edge;
      Pp.space ctx ();
      pp_position_offset ctx offset
  | Edge_offset_edge_offset (edge1, offset1, edge2, offset2) ->
      Pp.string ctx edge1;
      Pp.space ctx ();
      pp_position_offset ctx offset1;
      Pp.pct_sp ctx ();
      Pp.string ctx edge2;
      Pp.space ctx ();
      pp_position_offset ctx offset2
  | Var v -> pp_var pp_position_value ctx v

(* CSS Images 3 sec. 3.2.1: an omitted shape defaults to [ellipse], an omitted
   size to [farthest-corner], and an omitted position to [center]. Under minify,
   drop a component that equals its default - the resolved gradient stays
   identical. *)
let radial_shape_is_default : radial_shape -> bool = function
  | Ellipse -> true
  | _ -> false

let radial_size_is_default : radial_size -> bool = function
  | Farthest_corner -> true
  | _ -> false

let position_value_is_center : position_value -> bool = function
  | Center -> true
  | XY (Pct 50., Pct 50.) -> true
  | Single (Pct 50.) -> true
  | _ -> false

let radial_shape_kept (s : radial_shape option) : radial_shape option =
  match s with Some s when radial_shape_is_default s -> None | s -> s

let radial_size_kept (s : radial_size option) : radial_size option =
  match s with Some s when radial_size_is_default s -> None | s -> s

let radial_position_kept (p : position_value option) : position_value option =
  match p with Some p when position_value_is_center p -> None | p -> p

let pp_radial_gradient_config : radial_gradient_config Pp.t =
 fun ctx config ->
  let shape = config.shape in
  let size = config.size in
  let position = config.position in
  let has_output = ref false in
  let emit_space_if_needed () =
    if !has_output then Pp.space ctx () else has_output := true
  in
  Option.iter
    (fun i ->
      pp_color_interpolation ctx i;
      has_output := true)
    config.interpolation;
  Option.iter
    (fun s ->
      emit_space_if_needed ();
      pp_radial_shape ctx s)
    shape;
  Option.iter
    (fun s ->
      emit_space_if_needed ();
      pp_radial_size ctx s)
    size;
  Option.iter
    (fun p ->
      emit_space_if_needed ();
      Pp.string ctx "at";
      Pp.space ctx ();
      pp_position_value ctx p)
    position

let pp_conic_gradient_config : conic_gradient_config Pp.t =
 fun ctx config ->
  let has_output = ref false in
  (match config.interpolation with
  | Some i ->
      pp_color_interpolation ctx i;
      has_output := true
  | None -> ());
  (match config.angle with
  | Some a ->
      if !has_output then Pp.space ctx ();
      Pp.string ctx "from";
      Pp.space ctx ();
      pp_angle ctx a;
      has_output := true
  | None -> ());
  match config.position with
  | Some p ->
      if !has_output then Pp.space ctx ();
      Pp.string ctx "at";
      Pp.space ctx ();
      pp_position_value ctx p
  | None -> ()

let rec pp_gradient_position : gradient_position Pp.t =
 fun ctx -> function
  | Linear_position dir -> pp_gradient_direction ctx dir
  | Radial_position config -> pp_radial_gradient_config ctx config
  | Conic_position config -> pp_conic_gradient_config ctx config
  | Var v -> pp_var pp_gradient_position ctx v

let rec pp_gradient_stop : gradient_stop Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_gradient_stop ctx v
  | Color_percentage (c, pos1_opt, pos2_opt) -> (
      let rendered_color =
        Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
          pp_color c
      in
      Pp.string ctx rendered_color;
      match pos1_opt with
      | None -> ()
      | Some pos1 -> (
          (* CSS Syntax 3 (ED) sec. 4: ident- and hash-typed colours absorb the
             following digit/hex into the same token ([red0%] -> ident [red0] +
             [%]), so the separator is mandatory there; and when the stop lives
             in a custom-property token stream, the whitespace token between a
             function-shaped colour and its position is part of the value a
             var() substitution receives, so it is never elided either. *)
          Pp.space ctx ();
          pp_length_percentage ~always:true ctx pos1;
          match pos2_opt with
          | None -> ()
          | Some pos2 ->
              Pp.space ctx ();
              pp_length_percentage ~always:true ctx pos2))
  | Color_length (c, len1_opt, len2_opt) -> (
      pp_color ctx c;
      match len1_opt with
      | None -> ()
      | Some len1 -> (
          Pp.space ctx ();
          pp_length ctx len1;
          match len2_opt with
          | None -> ()
          | Some len2 ->
              Pp.space ctx ();
              pp_length ctx len2))
  | Length len -> pp_length ctx len
  | Channel channel -> pp_channel ctx channel
  | Percentage pct -> pp_percentage ctx pct
  | List stops ->
      Pp.list ~sep:Pp.comma pp_gradient_stop ctx
        (if Pp.minified ctx then canonicalise_gradient_stops stops else stops)
  | Position pos -> pp_gradient_position ctx pos
  | Direction dir -> pp_gradient_direction ctx dir

let pp_webkit_gradient_point ctx = function
  | Webkit_gradient.Left_top ->
      Pp.string ctx (if Pp.minified ctx then "0 0" else "left top")
  | Webkit_gradient.Left_bottom ->
      Pp.string ctx (if Pp.minified ctx then "0 100%" else "left bottom")
  | Webkit_gradient.Center ->
      Pp.string ctx (if Pp.minified ctx then "50% 50%" else "center center")
  | Webkit_gradient.Position position -> pp_position_value ctx position

let pp_webkit_gradient_position ctx (pct : percentage) =
  match (Pp.minified ctx, pct) with
  | true, Pct value -> Pp.float ctx (value /. 100.)
  | _ -> pp_percentage ctx pct

let pp_webkit_gradient_stop ctx = function
  | Webkit_gradient.From color -> Pp.call "from" pp_color ctx color
  | Webkit_gradient.To color -> Pp.call "to" pp_color ctx color
  | Webkit_gradient.Color_stop (Pct 0., color)
  | Webkit_gradient.Color_stop (Num 0., color) ->
      Pp.call "from" pp_color ctx color
  | Webkit_gradient.Color_stop (Pct 100., color)
  | Webkit_gradient.Color_stop (Num 1., color) ->
      Pp.call "to" pp_color ctx color
  | Webkit_gradient.Color_stop (position, color) ->
      Pp.call "color-stop"
        (fun ctx (position, color) ->
          pp_webkit_gradient_position ctx position;
          Pp.comma ctx ();
          pp_color ctx color)
        ctx (position, color)

let pp_webkit_gradient : Webkit_gradient.t Pp.t =
 fun ctx -> function
  | Webkit_gradient.Linear { start; finish; stops } ->
      Pp.call "-webkit-gradient"
        (fun ctx (start, finish, stops) ->
          Pp.string ctx "linear";
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx start;
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx finish;
          Pp.comma ctx ();
          Pp.list ~sep:Pp.comma pp_webkit_gradient_stop ctx stops)
        ctx (start, finish, stops)
  | Webkit_gradient.Radial
      { inner_center; inner_radius; outer_center; outer_radius; stops } ->
      Pp.call "-webkit-gradient"
        (fun ctx (inner_center, inner_radius, outer_center, outer_radius, stops)
           ->
          Pp.string ctx "radial";
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx inner_center;
          Pp.comma ctx ();
          Pp.float ctx inner_radius;
          Pp.comma ctx ();
          pp_webkit_gradient_point ctx outer_center;
          Pp.comma ctx ();
          Pp.float ctx outer_radius;
          Pp.comma ctx ();
          Pp.list ~sep:Pp.comma pp_webkit_gradient_stop ctx stops)
        ctx
        (inner_center, inner_radius, outer_center, outer_radius, stops)

let pp_quoted_url quote ctx url =
  Pp.string ctx "url(";
  Pp.quoted_string ~quote ctx url;
  Pp.char ctx ')'

let conic_gradient_has_config (config : conic_gradient_config) =
  config.angle <> None || config.position <> None
  || config.interpolation <> None

let pp_conic_gradient_named name ctx (config, stops) =
  Pp.call name
    (fun ctx (config, stops) ->
      if conic_gradient_has_config config then (
        pp_conic_gradient_config ctx config;
        match stops with [] -> () | _ -> Pp.comma ctx ());
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (config, stops)

let pp_linear_gradient_named name ctx (dir, stops) =
  Pp.call name
    (fun ctx (dir, stops) ->
      (* CSS Images 4 sec. 3.1: the default linear-gradient direction is [to
         bottom], equivalent to [180deg]; both spellings can be elided. *)
      let is_default_direction = function
        | Default_direction -> true
        | _ -> false
      in
      let head : [ `Skip | `Direction | `Interp_only of color_interpolation ] =
        match dir with
        | dir when is_default_direction dir -> `Skip
        | With_interpolation (inner, interp) when is_default_direction inner ->
            `Interp_only interp
        | _ -> `Direction
      in
      (match head with
      | `Skip -> ()
      | `Direction -> (
          pp_gradient_direction ctx dir;
          match stops with [] -> () | _ -> Pp.comma ctx ())
      | `Interp_only interp -> (
          pp_color_interpolation ctx interp;
          match stops with [] -> () | _ -> Pp.comma ctx ()));
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (dir, stops)

let pp_webkit_linear_gradient_named name ctx (dir, stops) =
  Pp.call name
    (fun ctx (dir, stops) ->
      let print_direction =
        match dir with Default_direction -> false | _ -> true
      in
      if print_direction then (
        pp_webkit_gradient_direction ctx dir;
        match stops with [] -> () | _ -> Pp.comma ctx ());
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (dir, stops)

let pp_radial_gradient_named name ctx (config, stops) =
  Pp.call name
    (fun ctx (config, stops) ->
      let has_config =
        config.shape <> None || config.size <> None || config.position <> None
        || config.interpolation <> None
      in
      if has_config then (
        pp_radial_gradient_config ctx config;
        match stops with [] -> () | _ -> Pp.comma ctx ());
      match stops with
      | [] -> ()
      | _ ->
          Pp.list ~sep:Pp.comma pp_gradient_stop ctx
            (if Pp.minified ctx then canonicalise_gradient_stops stops
             else stops))
    ctx (config, stops)

let pp_image_set_option ctx { source; resolution; mime_type } =
  (match source with Url u -> Pp.url ctx u | String s -> Pp.quoted ctx s);
  Option.iter
    (fun mime ->
      Pp.space ctx ();
      Pp.string ctx "type(";
      Pp.quoted ctx mime;
      Pp.char ctx ')')
    mime_type;
  Option.iter
    (fun res ->
      Pp.sp ctx ();
      Pp.string ctx res)
    resolution

let rec pp_background_image : background_image Pp.t =
 fun ctx (image : background_image) ->
  match image with
  | Url url -> Pp.url ctx url
  | Quoted (url, quote) ->
      if Pp.minified ctx then Pp.url ctx url else pp_quoted_url quote ctx url
  | Linear_gradient (dir, stops) ->
      pp_linear_gradient_named "linear-gradient" ctx (dir, stops)
  | Linear_gradient_var var_ref ->
      Pp.call "linear-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_linear_gradient (dir, stops) ->
      pp_linear_gradient_named "repeating-linear-gradient" ctx (dir, stops)
  | Radial_gradient (config, stops) ->
      pp_radial_gradient_named "radial-gradient" ctx (config, stops)
  | Radial_gradient_var var_ref ->
      Pp.call "radial-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "repeating-radial-gradient" ctx (config, stops)
  | Conic_gradient (config, stops) ->
      pp_conic_gradient_named "conic-gradient" ctx (config, stops)
  | Conic_gradient_var var_ref ->
      Pp.call "conic-gradient"
        (fun ctx v -> pp_var pp_gradient_stop ctx v)
        ctx var_ref
  | Repeating_conic_gradient (config, stops) ->
      pp_conic_gradient_named "repeating-conic-gradient" ctx (config, stops)
  | Webkit_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-webkit-linear-gradient" ctx (dir, stops)
  | Webkit_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-webkit-repeating-linear-gradient" ctx
        (dir, stops)
  | Webkit_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-webkit-radial-gradient" ctx (config, stops)
  | Webkit_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-webkit-repeating-radial-gradient" ctx
        (config, stops)
  | Moz_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-moz-linear-gradient" ctx (dir, stops)
  | Moz_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-moz-repeating-linear-gradient" ctx
        (dir, stops)
  | Moz_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-moz-radial-gradient" ctx (config, stops)
  | Moz_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-moz-repeating-radial-gradient" ctx
        (config, stops)
  | O_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-o-linear-gradient" ctx (dir, stops)
  | O_repeating_linear_gradient (dir, stops) ->
      pp_webkit_linear_gradient_named "-o-repeating-linear-gradient" ctx
        (dir, stops)
  | O_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-o-radial-gradient" ctx (config, stops)
  | O_repeating_radial_gradient (config, stops) ->
      pp_radial_gradient_named "-o-repeating-radial-gradient" ctx (config, stops)
  | Image_set options ->
      Pp.call "image-set"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_image_set_option ctx os)
        ctx options
  | Webkit_image_set options ->
      Pp.call "-webkit-image-set"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_image_set_option ctx os)
        ctx options
  | Cross_fade options ->
      Pp.call "cross-fade"
        (fun ctx os -> Pp.list ~sep:Pp.comma pp_cross_fade_option ctx os)
        ctx options
  | Webkit_gradient gradient -> pp_webkit_gradient ctx gradient
  | Var v -> pp_var pp_background_image ctx v
  | List images -> Pp.list ~sep:Pp.comma pp_background_image ctx images
  | None -> Pp.string ctx "none"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

and pp_cross_fade_option ctx { image; percent } =
  pp_background_image ctx image;
  Option.iter
    (fun pct ->
      Pp.space ctx ();
      Values.pp_percentage ~always:true ctx pct)
    percent

let normalize_radial_size : radial_size -> radial_size =
 fun value ->
  match value with
  | Ellipse_radii (a, b) ->
      preserve_if_equal value
        (Ellipse_radii
           ( Values.normalize_length_percentage ~strip:false a,
             Values.normalize_length_percentage ~strip:false b ))
  | Circle_radius r ->
      preserve_if_equal value
        (Circle_radius (Values.normalize_length ~strip:false r))
  | other -> other

let rec normalize_gradient_direction : gradient_direction -> gradient_direction
    =
 fun value ->
  let normalize_angle_direction angle =
    match Values.angle_degrees_opt angle with
    | Some deg when Float.rem (deg -. 180.) 360. = 0. -> Default_direction
    | _ -> Angle (Values.normalize_angle angle)
  in
  match value with
  | Default_direction -> value
  | To_top -> Angle (Values.Deg 0.)
  | To_right -> Angle (Values.Deg 90.)
  | To_bottom -> Default_direction
  | To_left -> Angle (Values.Deg 270.)
  | Angle a -> preserve_if_equal value (normalize_angle_direction a)
  | With_interpolation (dir, interp) ->
      let dir' =
        match dir with
        | Default_direction -> dir
        | _ -> normalize_gradient_direction dir
      in
      preserve_if_equal value (With_interpolation (dir', interp))
  | (To_top_right | To_bottom_right | To_bottom_left | To_top_left | Var _) as
    other ->
      other

let rec normalize_gradient_stop ?(lossless = false) :
    gradient_stop -> gradient_stop =
 fun value ->
  match value with
  | Color_percentage (c, p1, p2) ->
      preserve_if_equal value
        (Color_percentage
           ( normalize_color ~lossless c,
             option_map_preserve
               (Values.normalize_length_percentage ~strip:false)
               p1,
             option_map_preserve
               (Values.normalize_length_percentage ~strip:false)
               p2 ))
  | Color_length (c, l1, l2) ->
      preserve_if_equal value
        (Color_length
           ( normalize_color ~lossless c,
             option_map_preserve (Values.normalize_length ~strip:false) l1,
             option_map_preserve (Values.normalize_length ~strip:false) l2 ))
  | Length l ->
      preserve_if_equal value (Length (Values.normalize_length ~strip:false l))
  | Direction dir ->
      preserve_if_equal value (Direction (normalize_gradient_direction dir))
  | List stops ->
      preserve_if_equal value
        (List (map_preserve (normalize_gradient_stop ~lossless) stops))
  | other -> other

let normalize_webkit_gradient_stop :
    ?lossless:bool -> Webkit_gradient.stop -> Webkit_gradient.stop =
 fun ?(lossless = false) value ->
  match value with
  | From c -> preserve_if_equal value (From (normalize_color ~lossless c))
  | To c -> preserve_if_equal value (To (normalize_color ~lossless c))
  | Color_stop (p, c) ->
      preserve_if_equal value (Color_stop (p, normalize_color ~lossless c))

let normalize_webkit_gradient ?(lossless = false) :
    Webkit_gradient.t -> Webkit_gradient.t =
 fun value ->
  match value with
  | Linear r ->
      let stops =
        map_preserve (normalize_webkit_gradient_stop ~lossless) r.stops
      in
      if stops == r.stops then value else Linear { r with stops }
  | Radial r ->
      let stops =
        map_preserve (normalize_webkit_gradient_stop ~lossless) r.stops
      in
      if stops == r.stops then value else Radial { r with stops }

(* [<position>] spelling canonicalization. CSS Values 4 sec. 8.3: the edge
   keywords are percentage synonyms ([left] = [top] = [0%], [center] = [50%],
   [right] = [bottom] = [100%]), a percentage offset of [0%] and the length [0]
   resolve to the same point, and a single value defaults the vertical component
   to [center]. Fold every statically-known position to the node the parser
   produces for its shortest spelling; the printer stays faithful to the node,
   so a normalized AST and the reparse of its output agree (the structural-hash
   invariant of [Declaration.v]). *)
let position_zero (l : Values.length) : Values.length =
  match l with Pct 0. -> Zero | l -> l

let position_offset_zero (lp : Values.length_percentage) :
    Values.length_percentage =
  match lp with Pct 0. | Length (Pct 0.) -> Length Zero | lp -> lp

let position_pct n : Values.length = Pct n

let static_position_offset : Values.length_percentage -> Values.length option =
  function
  | Length l -> Some l
  | Pct n -> Some (position_pct n)
  | Env _ | Var _ | Calc _ | Invalid _ -> None

let position_edge_offset edge offset =
  match edge with
  | "left" | "top" -> static_position_offset offset
  | "right" | "bottom" -> (
      match offset with
      | Pct n -> Some (position_pct (100. -. n))
      | Length l -> (
          match Values.normalize_length l with
          | Zero -> Some (position_pct 100.)
          | Pct n -> Some (position_pct (100. -. n))
          | _ -> None)
      | Env _ | Var _ | Calc _ | Invalid _ -> None)
  | _ -> None

let horizontal_position = function
  | "left" -> Some (position_pct 0.)
  | "center" -> Some (position_pct 50.)
  | "right" -> Some (position_pct 100.)
  | _ -> None

let vertical_position = function
  | "top" -> Some (position_pct 0.)
  | "center" -> Some (position_pct 50.)
  | "bottom" -> Some (position_pct 100.)
  | _ -> None

let position_pair x y =
  match (x, y) with Some x, Some y -> Some (x, y) | _ -> None

(* The horizontal and vertical components of a statically-known position; [None]
   when a component is dynamic ([var()], [calc()], [env()]) or uses a non-zero
   length offset from [right]/[bottom], which needs the box size. *)
let position_xy : position_value -> (Values.length * Values.length) option =
  function
  | Center -> Some (position_pct 50., position_pct 50.)
  | Left -> Some (position_pct 0., position_pct 50.)
  | Right -> Some (position_pct 100., position_pct 50.)
  | Top -> Some (position_pct 50., position_pct 0.)
  | Bottom -> Some (position_pct 50., position_pct 100.)
  | Left_top | Top_left -> Some (position_pct 0., position_pct 0.)
  | Left_center -> Some (position_pct 0., position_pct 50.)
  | Left_bottom | Bottom_left -> Some (position_pct 0., position_pct 100.)
  | Right_top | Top_right -> Some (position_pct 100., position_pct 0.)
  | Right_center -> Some (position_pct 100., position_pct 50.)
  | Right_bottom | Bottom_right -> Some (position_pct 100., position_pct 100.)
  | Center_top -> Some (position_pct 50., position_pct 0.)
  | Center_bottom -> Some (position_pct 50., position_pct 100.)
  | XY (x, y) -> Some (x, y)
  | Single x -> Some (x, position_pct 50.)
  | Edge_offset_axis ((("left" | "right") as edge), off, axis) ->
      position_pair (position_edge_offset edge off) (vertical_position axis)
  | Edge_offset_axis ((("top" | "bottom") as edge), off, axis) ->
      position_pair (horizontal_position axis) (position_edge_offset edge off)
  | Axis_edge_offset (axis, (("top" | "bottom") as edge), off) ->
      position_pair (horizontal_position axis) (position_edge_offset edge off)
  | Edge_offset_edge_offset
      ((("left" | "right") as x_edge), x, (("top" | "bottom") as y_edge), y) ->
      position_pair
        (position_edge_offset x_edge x)
        (position_edge_offset y_edge y)
  | Edge_offset_edge_offset
      ((("top" | "bottom") as y_edge), y, (("left" | "right") as x_edge), x) ->
      position_pair
        (position_edge_offset x_edge x)
        (position_edge_offset y_edge y)
  | _ -> None

(* The parser's node for the shortest spelling of a static (x, y) position: [x]
   alone when [y] is centred, the [top] / [bottom] keywords when they beat [50%
   0] / [50% 100%], a plain pair otherwise. *)
let position_of_xy (x : Values.length) (y : Values.length) : position_value =
  match (position_zero x, position_zero y) with
  | x, Pct 50. -> Single x
  | Pct 50., Zero -> Top
  | Pct 50., Pct 100. -> Bottom
  | x, y -> XY (x, y)

let normalize_position_value ?(strip = true) : position_value -> position_value
    =
 fun value ->
  let length l = position_zero (Values.normalize_length ~strip l) in
  let offset lp =
    position_offset_zero (Values.normalize_length_percentage ~strip lp)
  in
  match position_xy value with
  | Some (x, y) ->
      preserve_if_equal value (position_of_xy (length x) (length y))
  | None -> (
      match value with
      | Edge_offset_axis (e, lp, a) ->
          preserve_if_equal value (Edge_offset_axis (e, offset lp, a))
      | Axis_edge_offset (a, e, l) ->
          preserve_if_equal value (Axis_edge_offset (a, e, offset l))
      | Edge_offset_edge_offset (e1, lp1, e2, lp2) ->
          preserve_if_equal value
            (Edge_offset_edge_offset (e1, offset lp1, e2, offset lp2))
      | other -> other)

let normalize_radial_config (c : radial_gradient_config) =
  (* CSS Images 4 section 3.2: inside a radial-gradient() prelude the default
     shape (ellipse), size (farthest-corner) and position (center) are implied,
     so the canonical form drops them. pp stays faithful to the AST; this
     elision is an optimize-side rewrite. *)
  let shape = radial_shape_kept c.shape in
  let size =
    option_map_preserve normalize_radial_size (radial_size_kept c.size)
  in
  let position =
    option_map_preserve
      (normalize_position_value ~strip:false)
      (radial_position_kept c.position)
  in
  if shape == c.shape && size == c.size && position == c.position then c
  else { shape; size; position; interpolation = c.interpolation }

let normalize_conic_config (c : conic_gradient_config) =
  let angle = option_map_preserve Values.normalize_angle c.angle in
  let position =
    option_map_preserve (normalize_position_value ~strip:false) c.position
  in
  if angle == c.angle && position == c.position then c
  else { c with angle; position }

(* CSS Images 4 sec. 3.5.1: [<color> <p1> <p2>] is defined as exactly [<color>
   <p1>, <color> <p2>], so two adjacent stops of one colour, each carrying a
   single position, are one stop carrying both. Left to right, so a longer run
   folds pairwise; a stop already holding two positions has nothing left to
   absorb. Colours are compared after [normalize_gradient_stop] has
   canonicalised them, so [red] and [#f00] still fold together. *)
let rec fold_double_position_stops stops =
  match stops with
  | Color_percentage (c1, Option.Some p1, Option.None)
    :: Color_percentage (c2, Option.Some p2, Option.None)
    :: rest
    when Values.equal_color c1 c2 ->
      Color_percentage (c1, Option.Some p1, Option.Some p2)
      :: fold_double_position_stops rest
  | Color_length (c1, Option.Some l1, Option.None)
    :: Color_length (c2, Option.Some l2, Option.None)
    :: rest
    when Values.equal_color c1 c2 ->
      Color_length (c1, Option.Some l1, Option.Some l2)
      :: fold_double_position_stops rest
  | stop :: rest ->
      let rest' = fold_double_position_stops rest in
      if rest' == rest then stops else stop :: rest'
  | [] -> stops

(* [linear-gradient(0deg, A, B)] paints the same pixels as [linear-gradient(B,
   A)]: turning the gradient line 180 degrees reaches the default [to bottom],
   and reversing the stops undoes the turn. Only sound when no stop carries a
   position and no interpolation hint is present, since those would each have to
   be mirrored to [100% - p], which is longer rather than shorter for lengths.
   The legacy prefixed gradients measure their angle from a different zero, so
   they are deliberately not folded here. *)
let reversible_unpositioned_stops (stops : gradient_stop list) =
  List.length stops > 1
  && List.for_all
       (fun stop ->
         match stop with
         | Color_percentage (_, Option.None, Option.None)
         | Color_length (_, Option.None, Option.None) ->
             true
         | _ -> false)
       stops

let drop_to_top_direction (direction : gradient_direction)
    (stops : gradient_stop list) =
  match direction with
  | Angle a
    when Values.angle_degrees_opt a = Option.Some 0.
         && reversible_unpositioned_stops stops ->
      Option.Some (Default_direction, List.rev stops)
  | _ -> Option.None

let rec normalize_background_image ?(lossless = false) :
    background_image -> background_image =
 fun value ->
  let stops s =
    fold_double_position_stops
      (map_preserve (normalize_gradient_stop ~lossless) s)
  in
  match value with
  | Linear_gradient (d, s) ->
      let d = normalize_gradient_direction d in
      let s = stops s in
      let d, s =
        match drop_to_top_direction d s with
        | Option.Some (d, s) -> (d, s)
        | Option.None -> (d, s)
      in
      preserve_if_equal value (Linear_gradient (d, s))
  | Radial_gradient (c, s) ->
      preserve_if_equal value
        (Radial_gradient (normalize_radial_config c, stops s))
  | Conic_gradient (c, s) ->
      preserve_if_equal value
        (Conic_gradient (normalize_conic_config c, stops s))
  | Repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | Repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Repeating_radial_gradient (normalize_radial_config c, stops s))
  | Repeating_conic_gradient (c, s) ->
      preserve_if_equal value
        (Repeating_conic_gradient (normalize_conic_config c, stops s))
  | Webkit_linear_gradient (d, s) ->
      preserve_if_equal value
        (Webkit_linear_gradient (normalize_gradient_direction d, stops s))
  | Webkit_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Webkit_repeating_linear_gradient
           (normalize_gradient_direction d, stops s))
  | Webkit_radial_gradient (c, s) ->
      preserve_if_equal value
        (Webkit_radial_gradient (normalize_radial_config c, stops s))
  | Webkit_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Webkit_repeating_radial_gradient (normalize_radial_config c, stops s))
  | Moz_linear_gradient (d, s) ->
      preserve_if_equal value
        (Moz_linear_gradient (normalize_gradient_direction d, stops s))
  | Moz_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (Moz_repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | Moz_radial_gradient (c, s) ->
      preserve_if_equal value
        (Moz_radial_gradient (normalize_radial_config c, stops s))
  | Moz_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (Moz_repeating_radial_gradient (normalize_radial_config c, stops s))
  | O_linear_gradient (d, s) ->
      preserve_if_equal value
        (O_linear_gradient (normalize_gradient_direction d, stops s))
  | O_repeating_linear_gradient (d, s) ->
      preserve_if_equal value
        (O_repeating_linear_gradient (normalize_gradient_direction d, stops s))
  | O_radial_gradient (c, s) ->
      preserve_if_equal value
        (O_radial_gradient (normalize_radial_config c, stops s))
  | O_repeating_radial_gradient (c, s) ->
      preserve_if_equal value
        (O_repeating_radial_gradient (normalize_radial_config c, stops s))
  | Webkit_gradient g ->
      preserve_if_equal value
        (Webkit_gradient (normalize_webkit_gradient ~lossless g))
  | Cross_fade opts ->
      let normalize_opt (o : cross_fade_option) =
        let image = normalize_background_image ~lossless o.image in
        if image == o.image then o else { o with image }
      in
      preserve_if_equal value (Cross_fade (map_preserve normalize_opt opts))
  | List imgs ->
      preserve_if_equal value
        (List (map_preserve (normalize_background_image ~lossless) imgs))
  | other -> other

let rec pp_image_orientation : image_orientation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_orientation ctx v
  | None -> Pp.string ctx "none"
  | From_image -> Pp.string ctx "from-image"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_image_rendering : image_rendering Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_rendering ctx v
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | High_quality -> Pp.string ctx "high-quality"
  | Crisp_edges -> Pp.string ctx "crisp-edges"
  | Pixelated -> Pp.string ctx "pixelated"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_resolution ctx = function
  | Dpi f -> Pp.unit ctx f "dpi"
  | Dpcm f -> Pp.unit ctx f "dpcm"
  | Dppx f -> Pp.unit ctx f "dppx"
  | X f -> Pp.unit ctx f "x"

let rec pp_image_resolution : image_resolution Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_image_resolution ctx v
  | Resolution r -> pp_resolution ctx r
  | From_image -> Pp.string ctx "from-image"
  | From_image_resolution r ->
      Pp.string ctx "from-image";
      Pp.space ctx ();
      pp_resolution ctx r
  | Snap r ->
      pp_resolution ctx r;
      Pp.space ctx ();
      Pp.string ctx "snap"
  | From_image_snap -> Pp.string ctx "from-image snap"
  | From_image_snap_resolution r ->
      Pp.string ctx "from-image";
      Pp.space ctx ();
      pp_resolution ctx r;
      Pp.space ctx ();
      Pp.string ctx "snap"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_image_orientation (t : Cursor.t) : image_orientation =
  let keywords : (string * image_orientation) list =
    [
      ("none", None);
      ("from-image", From_image);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "image-orientation" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_image_orientation t) : image_orientation))
     t
    : image_orientation)

let rec read_image_rendering (t : Cursor.t) : image_rendering =
  let keywords : (string * image_rendering) list =
    [
      ("auto", Auto);
      ("smooth", Smooth);
      ("high-quality", High_quality);
      ("crisp-edges", Crisp_edges);
      ("pixelated", Pixelated);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "image-rendering" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_image_rendering t) : image_rendering))
     t
    : image_rendering)

let read_resolution t : resolution =
  match Cursor.dimension_opt t with
  | None -> Cursor.err_expected t "<resolution>"
  | Some (value, unit_) -> (
      if value < 0. then Cursor.err_invalid t "negative resolution";
      match String.lowercase_ascii unit_ with
      | "dpi" -> Dpi value
      | "dpcm" -> Dpcm value
      | "dppx" -> Dppx value
      | "x" -> (X value : resolution)
      | _ -> Cursor.err_expected t "<resolution>")

let image_resolution_of_parts (t : Cursor.t)
    ~(parts : [ `From_image | `Snap ] list) (resolution : resolution option) :
    image_resolution =
  let from_image = List.mem `From_image parts in
  let snap = List.mem `Snap parts in
  match (from_image, snap, resolution) with
  | false, false, None -> Cursor.err_invalid t "image-resolution"
  | false, false, Some r -> Resolution r
  | true, false, None -> From_image
  | true, false, Some r -> From_image_resolution r
  | false, true, None -> Cursor.err_invalid t "image-resolution"
  | false, true, Some r -> Snap r
  | true, true, None -> From_image_snap
  | true, true, Some r -> From_image_snap_resolution r

let image_resolution_has_dimension t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Dimension _; _ }) -> true
  | _ -> false

let read_image_resolution_from_image t parts resolution =
  if List.mem `From_image parts then Cursor.err_invalid t "duplicate from-image";
  if parts <> [] || Option.is_some resolution then
    Cursor.err_invalid t "from-image must precede image resolution";
  ignore (Cursor.ident t : string);
  (`Continue (`From_image :: parts, resolution)
    : [ `Continue of [ `From_image | `Snap ] list * resolution option
      | `Done of image_resolution ])

let read_image_resolution_snap t parts resolution =
  if List.mem `Snap parts then Cursor.err_invalid t "duplicate snap";
  if (not (List.mem `From_image parts)) && Option.is_none resolution then
    Cursor.err_invalid t "snap must follow image resolution";
  ignore (Cursor.ident t : string);
  (`Continue (`Snap :: parts, resolution)
    : [ `Continue of [ `From_image | `Snap ] list * resolution option
      | `Done of image_resolution ])

let read_image_resolution_dimension t (parts : [ `From_image | `Snap ] list)
    (resolution : resolution option) =
  match resolution with
  | None ->
      if List.mem `Snap parts then
        Cursor.err_invalid t "image resolution must precede snap";
      `Continue (parts, Some (read_resolution t))
  | Some _ -> Cursor.err_invalid t "duplicate resolution"

let read_image_resolution_step t parts resolution =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "from-image" -> read_image_resolution_from_image t parts resolution
  | Some "snap" -> read_image_resolution_snap t parts resolution
  | _ when image_resolution_has_dimension t ->
      read_image_resolution_dimension t parts resolution
  | _ -> `Done (image_resolution_of_parts t ~parts resolution)

let read_image_resolution_value t =
  let rec loop parts resolution =
    match read_image_resolution_step t parts resolution with
    | `Continue (parts, resolution) -> loop parts resolution
    | `Done result -> result
  in
  loop [] (None : resolution option)

let rec read_image_resolution (t : Cursor.t) : image_resolution =
  Cursor.enum_or_var "image-resolution"
    [
      ("initial", (Initial : image_resolution));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_image_resolution t))
    ~default:read_image_resolution_value t

module Gradient_direction = struct
  type keyword = Top | Bottom | Left | Right

  let read_keyword t : keyword =
    Cursor.enum "direction"
      [ ("top", Top); ("bottom", Bottom); ("left", Left); ("right", Right) ]
      t

  let merge_keywords t (keywords : keyword list) =
    match keywords with
    | [ Top ] -> To_top
    | [ Bottom ] -> To_bottom
    | [ Left ] -> To_left
    | [ Right ] -> To_right
    | [ Top; Left ] | [ Left; Top ] -> To_top_left
    | [ Top; Right ] | [ Right; Top ] -> To_top_right
    | [ Bottom; Left ] | [ Left; Bottom ] -> To_bottom_left
    | [ Bottom; Right ] | [ Right; Bottom ] -> To_bottom_right
    | _ ->
        err_invalid_value t "gradient-direction" "invalid direction combination"

  let read_to_direction t =
    Cursor.expect_string "to" t;
    Cursor.ws t;
    let directions, _ = Cursor.many read_keyword t in
    merge_keywords t directions

  let read_legacy t =
    let directions, _ = Cursor.many read_keyword t in
    match directions with
    | [] -> Cursor.err_expected t "legacy gradient direction"
    | _ -> merge_keywords t directions

  let read_angle t = (Angle (Values.read_angle t) : gradient_direction)

  let read t : gradient_direction =
    Cursor.one_of [ read_to_direction; read_angle ] t
end

let read_gradient_direction t : gradient_direction = Gradient_direction.read t

module Gradient_stop = struct
  (* Parse specific combinations *)
  let read_color_lp_lp t =
    let color, lp1, lp2 =
      Cursor.triple ~sep:Cursor.ws read_color read_length_percentage
        read_length_percentage t
    in
    Color_percentage (color, Some lp1, Some lp2)

  let read_color_lp t =
    let color, lp =
      Cursor.pair ~sep:Cursor.ws read_color read_length_percentage t
    in
    Color_percentage (color, Some lp, None)

  let read_color_len_len t =
    let color, len1, len2 =
      Cursor.triple ~sep:Cursor.ws read_color read_length read_length t
    in
    Color_length (color, Some len1, Some len2)

  let read_color_len t =
    let color = read_color t in
    Cursor.ws t;
    let len = read_length t in
    Color_length (color, Some len, None)

  let read_color_only t =
    let color = read_color t in
    Color_percentage (color, None, None)

  let read_pct t : gradient_stop = Percentage (read_percentage t)
  let read_len t : gradient_stop = Length (read_length t)
  let read_channel t : gradient_stop = Channel (Values.read_channel t)
end

let rec read_gradient_stop_single t : gradient_stop =
  let read_var t : gradient_stop = Var (read_var read_gradient_stop_list t) in
  Cursor.ws t;
  (* Try from most specific to most general, letting individual parsers handle
     their own variables *)
  Cursor.one_of
    [
      (* 3 elements: color + two length-percentages/lengths (most specific) *)
      Gradient_stop.read_color_lp_lp;
      Gradient_stop.read_color_len_len;
      (* 2 elements: color + length-percentage/length *)
      Gradient_stop.read_color_lp;
      Gradient_stop.read_color_len;
      (* 1 element: single values *)
      Gradient_stop.read_color_only;
      Gradient_stop.read_pct;
      Gradient_stop.read_len;
      Gradient_stop.read_channel;
      (* Full gradient_stop variables (e.g., var(--tw-gradient-stops)) - last
         resort *)
      read_var;
    ]
    t

and read_gradient_stop_list t : gradient_stop =
  (* Parse a list of gradient stops - used only for var fallbacks *)
  match
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_gradient_stop_single t
  with
  | [ x ] -> x
  | l -> List l

let read_gradient_stop t : gradient_stop =
  (* Only parse single gradient stops - lists are created by var fallback
     parsing *)
  read_gradient_stop_single t

let read_gradient_stops t =
  match
    Cursor.option
      (Cursor.list ~at_least:1 ~sep:Cursor.comma read_gradient_stop)
      t
  with
  | Some stops -> stops
  | None -> []

module Position_value = struct
  type keyword =
    | Center
    | Left
    | Right
    | Top
    | Bottom
    | Inherit
    | Initial
    | Unset
    | Revert
    | Revert_layer

  let horizontal = function "left" | "right" -> true | _ -> false
  let vertical = function "top" | "bottom" -> true | _ -> false

  let valid_edge_axis edge axis =
    (horizontal edge && (vertical axis || axis = "center"))
    || (vertical edge && (horizontal axis || axis = "center"))

  let read_xy (t : Cursor.t) : position_value =
    let x = read_length ~with_keywords:false t in
    Cursor.ws t;
    match Cursor.option (read_length ~with_keywords:false) t with
    | Some y -> XY (x, y)
    | None -> Single x

  let read_keyword t : keyword =
    Cursor.enum "position-keyword"
      [
        ("center", Center);
        ("left", Left);
        ("right", Right);
        ("top", Top);
        ("bottom", Bottom);
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      t

  (* Read single keyword value *)
  let read_1_value t : position_value =
    let kw = read_keyword t in
    match kw with
    | Center -> Center
    | Inherit -> Inherit
    | Initial -> Initial
    | Unset -> Unset
    | Revert -> Revert
    | Revert_layer -> Revert_layer
    | Left -> Left
    | Right -> Right
    | Top -> Top
    | Bottom -> Bottom

  (* Read two keyword values *)
  let read_2_value t : position_value =
    let first = read_keyword t in
    (* Global keywords cannot be combined with other keywords *)
    (match first with
    | Inherit | Initial | Unset | Revert | Revert_layer ->
        Cursor.err_invalid t "global keywords cannot be combined"
    | _ -> ());
    Cursor.ws t;
    let second = read_keyword t in
    (* Preserve the source-level order of the two keywords; the AST has both a
       [horizontal_then_vertical] and [vertical_then_horizontal] variant per
       pair, and the printer emits them in their source spelling. *)
    match (first, second) with
    | Left, Top -> Left_top
    | Top, Left -> Top_left
    | Left, Center -> Left_center
    | Center, Left -> Left_center
    | Left, Bottom -> Left_bottom
    | Bottom, Left -> Bottom_left
    | Right, Top -> Right_top
    | Top, Right -> Top_right
    | Right, Center -> Right_center
    | Center, Right -> Right_center
    | Right, Bottom -> Right_bottom
    | Bottom, Right -> Bottom_right
    | Center, Top -> Center_top
    | Top, Center -> Center_top
    | Center, Bottom -> Center_bottom
    | Bottom, Center -> Center_bottom
    | Center, Center -> Center
    | _ -> Cursor.err_invalid t "invalid position keyword combination"

  (* Read 3-value syntax: keyword offset keyword *)
  let read_3_value t : position_value =
    let edge1 = Cursor.ident t in
    Cursor.ws t;
    let offset = read_length_percentage ~with_keywords:false t in
    Cursor.ws t;
    let axis = Cursor.ident t in
    if valid_edge_axis edge1 axis then Edge_offset_axis (edge1, offset, axis)
    else Cursor.err_invalid t "invalid three-value position"

  let read_axis_edge_offset t : position_value =
    let axis = Cursor.ident t in
    Cursor.ws t;
    let edge = Cursor.ident t in
    Cursor.ws t;
    let offset = read_length_percentage ~with_keywords:false t in
    if valid_edge_axis edge axis then Axis_edge_offset (axis, edge, offset)
    else Cursor.err_invalid t "invalid position axis edge offset"

  let read_horizontal_keyword_length t : position_value =
    let keyword = Cursor.ident t in
    Cursor.ws t;
    let y = read_length ~with_keywords:false t in
    match keyword with
    | "left" -> XY ((Pct 0. : length), y)
    | "right" -> XY ((Pct 100. : length), y)
    | "center" -> XY ((Pct 50. : length), y)
    | _ -> Cursor.err_invalid t "invalid horizontal position keyword length"

  let read_length_keyword t : position_value =
    let offset = read_length ~with_keywords:false t in
    Cursor.ws t;
    match Cursor.ident t with
    | "center" -> Single offset
    | _ -> Cursor.err_invalid t "invalid position length keyword"

  (* Read 4-value syntax: keyword offset keyword offset *)
  let read_4_value t : position_value =
    let edge1 = Cursor.ident t in
    Cursor.ws t;
    let offset1 = read_length_percentage ~with_keywords:false t in
    Cursor.ws t;
    let edge2 = Cursor.ident t in
    Cursor.ws t;
    let offset2 = read_length_percentage ~with_keywords:false t in
    if
      (horizontal edge1 && vertical edge2)
      || (vertical edge1 && horizontal edge2)
    then Edge_offset_edge_offset (edge1, offset1, edge2, offset2)
    else Cursor.err_invalid t "invalid four-value position"
end

let common_position_readers read_var =
  [
    Position_value.read_horizontal_keyword_length;
    Position_value.read_length_keyword;
    Position_value.read_xy;
    Position_value.read_2_value;
    Position_value.read_1_value;
    read_var;
  ]

let rec read_position_value t : position_value =
  let read_var t : position_value = Var (read_var read_position_value t) in
  Cursor.one_of
    (Position_value.read_4_value :: common_position_readers read_var)
    t

let rec read_background_position_value t : position_value =
  let read_var t : position_value =
    Var (read_var read_background_position_value t)
  in
  Cursor.one_of
    (Position_value.read_4_value :: Position_value.read_axis_edge_offset
   :: Position_value.read_3_value
    :: common_position_readers read_var)
    t

module Radial_config = struct
  let read_shape t : radial_shape =
    Cursor.enum "radial-shape" [ ("circle", Circle); ("ellipse", Ellipse) ] t

  let read_size_keyword t : radial_size =
    Cursor.enum "radial-size"
      [
        ("closest-side", (Closest_side : radial_size));
        ("farthest-side", Farthest_side);
        ("closest-corner", Closest_corner);
        ("farthest-corner", Farthest_corner);
      ]
      t

  let read_explicit_size t : radial_size =
    let first = read_length_percentage t in
    Cursor.ws t;
    match Cursor.option read_length_percentage t with
    | Some second -> Ellipse_radii (first, second)
    | None -> (
        match first with
        | Length l -> Circle_radius l
        | _ ->
            Cursor.err_invalid t
              "circle radius must be a length, not a percentage")

  let read_at_position t : position_value =
    Cursor.expect_string "at" t;
    Cursor.ws t;
    read_position_value t

  let read t : radial_gradient_config =
    (* CSS Images 4 section 3.2: the [radial-gradient] prelude is
       [<ending-shape> || <size> || at <position> ||
       <color-interpolation-method>]? - the [||] combinator allows any order.
       Loop trying each missing slot until no progress is made. *)
    let shape : radial_shape option ref = ref Option.None in
    let size : radial_size option ref = ref Option.None in
    let position : position_value option ref = ref Option.None in
    let interpolation : color_interpolation option ref = ref Option.None in
    let try_slot : 'a. 'a option ref -> (Cursor.t -> 'a) -> bool =
     fun slot reader ->
      if !slot <> Option.None then false
      else
        match Cursor.option reader t with
        | Option.Some v ->
            slot := Option.Some v;
            true
        | Option.None -> false
    in
    let rec loop () =
      Cursor.ws t;
      if
        try_slot shape read_shape
        || try_slot size (fun t ->
            Cursor.one_of [ read_size_keyword; read_explicit_size ] t)
        || try_slot position read_at_position
        || try_slot interpolation read_color_interpolation
      then loop ()
    in
    loop ();
    {
      shape = !shape;
      size = !size;
      position = !position;
      interpolation = !interpolation;
    }
end

let read_radial_shape t : radial_shape = Radial_config.read_shape t

let read_radial_size t : radial_size =
  Cursor.one_of
    [ Radial_config.read_size_keyword; Radial_config.read_explicit_size ]
    t

let read_radial_gradient_config t : radial_gradient_config =
  Radial_config.read t

let read_conic_gradient_config t : conic_gradient_config =
  (* CSS Images 4 section 3.3.1: the [conic-gradient] prelude is [[from
     <angle>]? || [at <position>]? || <color-interpolation-method>]? - the [||]
     combinator allows any order. Loop trying each missing slot until no
     progress is made. *)
  let angle : angle option ref = ref Option.None in
  let position : position_value option ref = ref Option.None in
  let interpolation : color_interpolation option ref = ref Option.None in
  let read_from t =
    match Cursor.peek_ident t with
    | Some "from" ->
        let _ = Cursor.ident t in
        Cursor.ws t;
        Values.read_angle t
    | _ -> Cursor.err_expected t "from"
  in
  let read_at t =
    match Cursor.peek_ident t with
    | Some "at" ->
        let _ = Cursor.ident t in
        Cursor.ws t;
        read_position_value t
    | _ -> Cursor.err_expected t "at"
  in
  let try_slot : 'a. 'a option ref -> (Cursor.t -> 'a) -> bool =
   fun slot reader ->
    if !slot <> Option.None then false
    else
      match Cursor.option reader t with
      | Option.Some v ->
          slot := Option.Some v;
          true
      | Option.None -> false
  in
  let rec loop () =
    Cursor.ws t;
    if
      try_slot angle read_from || try_slot position read_at
      || try_slot interpolation read_color_interpolation
    then loop ()
  in
  loop ();
  if
    !angle = Option.None && !position = Option.None
    && !interpolation = Option.None
  then Cursor.err_invalid t "conic-gradient config";
  { angle = !angle; position = !position; interpolation = !interpolation }

(* CSS Images 4 sec. 3.1 [linear-gradient] prelude: [ <angle> | to
   <side-or-corner> ]? || <color-interpolation-method> A bare interpolation, a
   bare direction, or both in either order are all valid. Returns [None] when
   the prelude consumed no tokens. *)
let read_linear_prelude_opt t : gradient_direction option =
  let direction : gradient_direction option ref = ref Option.None in
  let interpolation : color_interpolation option ref = ref Option.None in
  let try_direction () =
    if !direction <> Option.None then false
    else
      match Cursor.option read_gradient_direction t with
      | Some d ->
          direction := Option.Some d;
          true
      | Option.None -> false
  in
  let try_interpolation () =
    if !interpolation <> Option.None then false
    else
      match Cursor.option read_color_interpolation t with
      | Some i ->
          interpolation := Option.Some i;
          true
      | Option.None -> false
  in
  let rec loop () =
    Cursor.ws t;
    if try_direction () || try_interpolation () then loop ()
  in
  loop ();
  match (!direction, !interpolation) with
  | Some d, Some i -> Option.Some (With_interpolation (d, i))
  | Some d, None -> Option.Some d
  | None, Some i -> Option.Some (With_interpolation (Default_direction, i))
  | None, None -> Option.None

(* Direction plus the optional [in <interpolation>] tail ([45deg in oklab]). *)
let read_gradient_prelude t : gradient_direction =
  match read_linear_prelude_opt t with
  | Some d -> d
  | None -> Cursor.err_expected t "gradient direction"

let rec read_gradient_position t : gradient_position =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      (Var (Values.read_var read_gradient_position t) : gradient_position)
  | _ -> (
      match Cursor.peek_ident t with
      | Some "from" -> Conic_position (read_conic_gradient_config t)
      | Some
          ( "circle" | "ellipse" | "closest-side" | "closest-corner"
          | "farthest-side" | "farthest-corner" | "at" ) ->
          Radial_position (read_radial_gradient_config t)
      | _ ->
          Cursor.one_of
            [
              (fun t ->
                match read_linear_prelude_opt t with
                | Option.Some direction ->
                    (Linear_position direction : gradient_position)
                | Option.None -> Cursor.err_expected t "gradient position");
              (fun t -> Radial_position (read_radial_gradient_config t));
            ]
            t)

let read_linear_gradient_body_stops t =
  (* After the prelude the comma separating it from the colour-stop list is
     required if and only if the prelude consumed any tokens. *)
  let prelude = read_linear_prelude_opt t in
  let prelude_consumed = prelude <> Option.None in
  let direction =
    match prelude with Option.Some d -> d | Option.None -> Default_direction
  in
  if prelude_consumed then (
    Cursor.ws t;
    if not (Cursor.comma_opt t) then
      Cursor.err_expected t "',' after linear-gradient direction");
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in linear-gradient()";
  Linear_gradient (direction, stops)

let read_gradient_var_only t =
  Cursor.ws t;
  (* CSS Variables 1 sec. 3: a single [var()] can stand in for the entire body
     of a gradient, since the variable's value may itself contain commas and
     stops. Check this case first so a linear gradient's var() does not get
     mis-parsed as an [Angle (Var _)] direction by the prelude reader. *)
  let var_only =
    Cursor.lookahead
      (fun t ->
        let v = Cursor.option (Values.read_var read_gradient_stop) t in
        Cursor.ws t;
        if Cursor.is_done t then v else None)
      t
  in
  match var_only with
  | Some _ ->
      let v = Values.read_var read_gradient_stop t in
      Cursor.ws t;
      Cursor.expect_eof t;
      Some v
  | None -> None

let read_linear_gradient_body t =
  match read_gradient_var_only t with
  | Some v -> Linear_gradient_var v
  | None -> read_linear_gradient_body_stops t

let read_webkit_linear_gradient_body t =
  Cursor.ws t;
  let direction =
    Cursor.option
      (fun t ->
        Cursor.one_of
          [ Gradient_direction.read_legacy; Gradient_direction.read_angle ]
          t)
      t
  in
  if direction <> None then (
    Cursor.ws t;
    if not (Cursor.comma_opt t) then
      Cursor.err_expected t "',' after -webkit-linear-gradient direction");
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in -webkit-linear-gradient()";
  Webkit_linear_gradient (Option.value ~default:To_bottom direction, stops)

let read_radial_gradient_body_stops t =
  Cursor.ws t;
  let config =
    match
      Cursor.option
        (fun t ->
          let cfg = Radial_config.read t in
          if
            cfg.shape = None && cfg.size = None && cfg.position = None
            && cfg.interpolation = None
          then Cursor.err_invalid t "no radial config";
          Cursor.ws t;
          ignore (Cursor.comma_opt t);
          cfg)
        t
    with
    | Some cfg -> cfg
    | None ->
        { shape = None; size = None; position = None; interpolation = None }
  in
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in radial-gradient()";
  Radial_gradient (config, stops)

let read_radial_gradient_body t =
  match read_gradient_var_only t with
  | Some v -> Radial_gradient_var v
  | None -> read_radial_gradient_body_stops t

let read_conic_gradient_body_stops t =
  (* [conic-gradient([from <angle>]? [at <position>]? ,? <color-stop-list>)] *)
  let config = Cursor.option read_conic_gradient_config t in
  Cursor.ws t;
  let has_config = config <> None in
  if has_config && not (Cursor.peek_comma t || Cursor.is_done t) then
    Cursor.err_expected t "',' or end of conic-gradient prefix";
  if Cursor.peek_comma t then Cursor.skip t;
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in conic-gradient()";
  let config =
    match config with
    | Some config -> config
    | None ->
        ({ angle = None; position = None; interpolation = None }
          : conic_gradient_config)
  in
  Conic_gradient (config, stops)

let read_conic_gradient_body t =
  match read_gradient_var_only t with
  | Some v -> Conic_gradient_var v
  | None -> read_conic_gradient_body_stops t

let read_bg_url_arg inner =
  Cursor.ws inner;
  match Cursor.string_with_quote_opt inner with
  | Some (url, quote) ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Quoted (url, quote) : background_image)
  | None ->
      let url = Cursor.consume_remaining_as_string ~trim:true inner in
      if url = "" then Cursor.err_expected inner "url argument" else Url url

let read_bg_url_call t terminated =
  if not terminated then Cursor.err_expected t "terminated url";
  Cursor.call "url" t read_bg_url_arg

let read_bg_url t : background_image =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Url _; _ }) -> Url (Cursor.url t)
  | Some (Component.Func { node = { name; terminated; _ }; _ })
    when String.lowercase_ascii_preserve name = "url" ->
      read_bg_url_call t terminated
  | _ -> Cursor.err_expected t "url"

let read_webkit_gradient_point t =
  match read_position_value t with
  | Left_top | Top_left -> Webkit_gradient.Left_top
  | Left_bottom | Bottom_left -> Webkit_gradient.Left_bottom
  | Center -> Webkit_gradient.Center
  | position -> Webkit_gradient.Position position

let read_webkit_gradient_stop t =
  let color_arg name inner =
    Cursor.ws inner;
    let color = read_color inner in
    Cursor.ws inner;
    Cursor.expect_eof inner;
    match name with
    | "from" -> Webkit_gradient.From color
    | "to" -> Webkit_gradient.To color
    | _ -> assert false
  in
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "from" ->
      Cursor.call "from" t (color_arg "from")
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "to" ->
      Cursor.call "to" t (color_arg "to")
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "color-stop" ->
      Cursor.call "color-stop" t (fun inner ->
          Cursor.ws inner;
          (* The legacy stop position is a number or a percentage, and the
             minified spelling is the number, so both read back. *)
          let position : percentage =
            match Cursor.number_opt inner with
            | Some n -> Num n
            | None -> read_percentage inner
          in
          Cursor.ws inner;
          Cursor.comma inner;
          Cursor.ws inner;
          let color = read_color inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          Webkit_gradient.Color_stop (position, color))
  | _ -> Cursor.err_expected t "-webkit-gradient color stop"

let read_webkit_gradient_radius t =
  match Cursor.number_opt t with
  | Some radius -> radius
  | None -> Cursor.err_expected t "-webkit-gradient radius"

let read_webkit_gradient_body t =
  Cursor.ws t;
  let kind = Cursor.ident t |> String.lowercase_ascii in
  Cursor.ws t;
  Cursor.comma t;
  match kind with
  | "linear" ->
      Cursor.ws t;
      let start = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let finish = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let stops =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_webkit_gradient_stop t
      in
      Cursor.ws t;
      Cursor.expect_eof t;
      Webkit_gradient (Webkit_gradient.Linear { start; finish; stops })
  | "radial" ->
      Cursor.ws t;
      let inner_center = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let inner_radius = read_webkit_gradient_radius t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let outer_center = read_webkit_gradient_point t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let outer_radius = read_webkit_gradient_radius t in
      Cursor.ws t;
      Cursor.comma t;
      Cursor.ws t;
      let stops =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_webkit_gradient_stop t
      in
      Cursor.ws t;
      Cursor.expect_eof t;
      Webkit_gradient
        (Webkit_gradient.Radial
           { inner_center; inner_radius; outer_center; outer_radius; stops })
  | _ -> Cursor.err_invalid t ("-webkit-gradient kind: " ^ kind)

let read_image_set_option t : image_set_option =
  let source : image_set_source =
    Cursor.one_of
      [
        (fun t -> (Url (Cursor.url t) : image_set_source));
        (fun t -> String (Cursor.string t));
      ]
      t
  in
  (* [<resolution> || type(<string>)] in any order; both optional. Recurse,
     accepting whichever modifier hasn't been seen yet, until neither
     matches. *)
  let read_resolution_opt t : string option =
    match Cursor.dimension_opt t with
    | None -> None
    | Some (value, unit_) ->
        let buf = Buffer.create 8 in
        let pp_ctx = Pp.ctx ~minify:true buf in
        Pp.float pp_ctx value;
        Buffer.add_string buf unit_;
        Some (Buffer.contents buf)
  in
  let rec loop (mime : string option) (resolution : string option) =
    Cursor.ws t;
    match (mime, Cursor.function_call "type" Cursor.string t) with
    | None, Some s -> loop (Some s) resolution
    | _ -> (
        match (resolution, read_resolution_opt t) with
        | None, Some r -> loop mime (Some r)
        | _ -> (mime, resolution))
  in
  let mime, resolution = loop None None in
  if Option.is_none mime && Option.is_none resolution then
    Cursor.err_invalid t
      "image-set option requires a <resolution> or type(<string>)";
  { source; resolution; mime_type = mime }

let read_image_set_body t : background_image =
  Image_set (Cursor.list ~sep:Cursor.comma ~at_least:1 read_image_set_option t)

let read_repeating_linear_gradient t =
  match Cursor.call "repeating-linear-gradient" t read_linear_gradient_body with
  | Linear_gradient (d, stops) -> Repeating_linear_gradient (d, stops)
  | Linear_gradient_var v ->
      Repeating_linear_gradient (Default_direction, [ Var v ])
  | other -> other

let read_repeating_radial_gradient t =
  match
    Cursor.call "repeating-radial-gradient" t read_radial_gradient_body_stops
  with
  | Radial_gradient (c, stops) -> Repeating_radial_gradient (c, stops)
  | other -> other

let read_repeating_conic_gradient t =
  match
    Cursor.call "repeating-conic-gradient" t read_conic_gradient_body_stops
  with
  | Conic_gradient (c, stops) -> Repeating_conic_gradient (c, stops)
  | other -> other

let read_webkit_repeating_linear_gradient t =
  match
    Cursor.call "-webkit-repeating-linear-gradient" t
      read_webkit_linear_gradient_body
  with
  | Webkit_linear_gradient (d, stops) ->
      Webkit_repeating_linear_gradient (d, stops)
  | other -> other

let webkit_radial_gradient_of_radial = function
  | Radial_gradient (c, stops) -> Webkit_radial_gradient (c, stops)
  | other -> other

let webkit_repeat_radial_of_radial = function
  | Radial_gradient (c, stops) -> Webkit_repeating_radial_gradient (c, stops)
  | other -> other

let moz_linear_gradient_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> Moz_linear_gradient (d, stops)
  | other -> other

let moz_repeat_linear_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> Moz_repeating_linear_gradient (d, stops)
  | other -> other

let moz_radial_gradient_of_radial = function
  | Radial_gradient (c, stops) -> Moz_radial_gradient (c, stops)
  | other -> other

let moz_repeat_radial_of_radial = function
  | Radial_gradient (c, stops) -> Moz_repeating_radial_gradient (c, stops)
  | other -> other

let o_linear_gradient_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> O_linear_gradient (d, stops)
  | other -> other

let o_repeat_linear_of_webkit = function
  | Webkit_linear_gradient (d, stops) -> O_repeating_linear_gradient (d, stops)
  | other -> other

let o_radial_gradient_of_radial = function
  | Radial_gradient (c, stops) -> O_radial_gradient (c, stops)
  | other -> other

let o_repeat_radial_of_radial = function
  | Radial_gradient (c, stops) -> O_repeating_radial_gradient (c, stops)
  | other -> other

let webkit_image_set_of_set = function
  | Image_set options -> Webkit_image_set options
  | other -> other

let read_bg_image_standard_calls =
  [
    ( "linear-gradient",
      fun t -> Cursor.call "linear-gradient" t read_linear_gradient_body );
    ( "radial-gradient",
      fun t -> Cursor.call "radial-gradient" t read_radial_gradient_body );
    ( "conic-gradient",
      fun t -> Cursor.call "conic-gradient" t read_conic_gradient_body );
    ("repeating-linear-gradient", read_repeating_linear_gradient);
    ("repeating-radial-gradient", read_repeating_radial_gradient);
    ("repeating-conic-gradient", read_repeating_conic_gradient);
  ]

let webkit_bg_image_calls =
  [
    ( "-webkit-linear-gradient",
      fun t ->
        Cursor.call "-webkit-linear-gradient" t read_webkit_linear_gradient_body
    );
    ("-webkit-repeating-linear-gradient", read_webkit_repeating_linear_gradient);
    ( "-webkit-radial-gradient",
      fun t ->
        Cursor.call "-webkit-radial-gradient" t read_radial_gradient_body_stops
        |> webkit_radial_gradient_of_radial );
    ( "-webkit-repeating-radial-gradient",
      fun t ->
        Cursor.call "-webkit-repeating-radial-gradient" t
          read_radial_gradient_body_stops
        |> webkit_repeat_radial_of_radial );
  ]

let legacy_bg_image_calls =
  [
    ( "-moz-linear-gradient",
      fun t ->
        Cursor.call "-moz-linear-gradient" t read_webkit_linear_gradient_body
        |> moz_linear_gradient_of_webkit );
    ( "-moz-repeating-linear-gradient",
      fun t ->
        Cursor.call "-moz-repeating-linear-gradient" t
          read_webkit_linear_gradient_body
        |> moz_repeat_linear_of_webkit );
    ( "-moz-radial-gradient",
      fun t ->
        Cursor.call "-moz-radial-gradient" t read_radial_gradient_body_stops
        |> moz_radial_gradient_of_radial );
    ( "-moz-repeating-radial-gradient",
      fun t ->
        Cursor.call "-moz-repeating-radial-gradient" t
          read_radial_gradient_body_stops
        |> moz_repeat_radial_of_radial );
    ( "-o-linear-gradient",
      fun t ->
        Cursor.call "-o-linear-gradient" t read_webkit_linear_gradient_body
        |> o_linear_gradient_of_webkit );
    ( "-o-repeating-linear-gradient",
      fun t ->
        Cursor.call "-o-repeating-linear-gradient" t
          read_webkit_linear_gradient_body
        |> o_repeat_linear_of_webkit );
    ( "-o-radial-gradient",
      fun t ->
        Cursor.call "-o-radial-gradient" t read_radial_gradient_body_stops
        |> o_radial_gradient_of_radial );
    ( "-o-repeating-radial-gradient",
      fun t ->
        Cursor.call "-o-repeating-radial-gradient" t
          read_radial_gradient_body_stops
        |> o_repeat_radial_of_radial );
  ]

let read_bg_image_vendor_calls = webkit_bg_image_calls @ legacy_bg_image_calls

let rec read_bg_image t : background_image =
  (* Bare [url(foo)] is a single [Token.Url] component, not a [Func]; handle it
     explicitly before dispatching on the function/ident shape. *)
  Cursor.one_of
    [
      read_bg_url;
      (fun t ->
        Cursor.enum_or_calls "background-image"
          [
            ("none", (None : background_image));
            ("initial", Initial);
            ("inherit", Inherit);
            ("unset", Unset);
            ("revert", Revert);
            ("revert-layer", Revert_layer);
          ]
          ~calls:(read_bg_image_calls ()) t);
    ]
    t

and read_bg_image_misc_calls () =
  [
    ("image-set", fun t -> Cursor.call "image-set" t read_image_set_body);
    ( "-webkit-image-set",
      fun t ->
        Cursor.call "-webkit-image-set" t read_image_set_body
        |> webkit_image_set_of_set );
    ("cross-fade", fun t -> Cursor.call "cross-fade" t read_cross_fade_body);
    ( "-webkit-gradient",
      fun t -> Cursor.call "-webkit-gradient" t read_webkit_gradient_body );
    ("var", fun t -> Var (Values.read_var read_bg_image t));
  ]

and read_bg_image_calls () =
  read_bg_image_standard_calls @ read_bg_image_vendor_calls
  @ read_bg_image_misc_calls ()

and read_cross_fade_body t : background_image =
  (* Parse a non-empty comma-separated list where every comma must be followed
     by another option. Cursor.list is permissive about trailing separators
     (rollback on failure leaves the comma silently consumed), so the spec's
     [<cf-mixing-image>#] grammar is enforced manually. *)
  let first = read_cross_fade_option t in
  let rec loop acc =
    Cursor.ws t;
    if Cursor.peek_comma t then (
      Cursor.skip t;
      loop (read_cross_fade_option t :: acc))
    else List.rev acc
  in
  let opts = loop [ first ] in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err_expected t "end of cross-fade() arguments";
  Cross_fade opts

and read_cross_fade_option t : cross_fade_option =
  Cursor.ws t;
  (* [<cf-mixing-image> = <percentage>? && <image>]. The two parts are
     order-independent in the spec; try percentage first, then image, and pick
     up a trailing percentage if the image came first. *)
  let pct =
    match Cursor.peek t with
    | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
        Some (Values.read_percentage t)
    | _ -> None
  in
  Cursor.ws t;
  let image = read_bg_image t in
  Cursor.ws t;
  let pct =
    match pct with
    | Some _ -> pct
    | None -> (
        match Cursor.peek t with
        | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
            Some (Values.read_percentage t)
        | _ -> None)
  in
  { image; percent = pct }

let read_background_image t : background_image =
  let first = read_bg_image t in
  Cursor.ws t;
  if Cursor.comma_opt t then
    let rest = Cursor.list ~sep:Cursor.comma ~at_least:1 read_bg_image t in
    List (first :: rest)
  else first

let read_background_images t : background_image list =
  Cursor.list ~sep:Cursor.comma read_background_image t

let minify_gradient_stop : gradient_stop -> gradient_stop = function
  | Color_percentage (c, p1, p2) -> Color_percentage (minify_color c, p1, p2)
  | s -> s

let minify_webkit_gradient_stop = function
  | Webkit_gradient.From color -> Webkit_gradient.From (minify_color color)
  | Webkit_gradient.To color -> Webkit_gradient.To (minify_color color)
  | Webkit_gradient.Color_stop (position, color) ->
      Webkit_gradient.Color_stop (position, minify_color color)

let minify_conic_gradient_config config = config

let minify_background_image : background_image -> background_image = function
  | Linear_gradient (dir, stops) ->
      Linear_gradient (dir, List.map minify_gradient_stop stops)
  | Repeating_linear_gradient (dir, stops) ->
      Repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Webkit_linear_gradient (dir, stops) ->
      Webkit_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Webkit_repeating_linear_gradient (dir, stops) ->
      Webkit_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Moz_linear_gradient (dir, stops) ->
      Moz_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Moz_repeating_linear_gradient (dir, stops) ->
      Moz_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | O_linear_gradient (dir, stops) ->
      O_linear_gradient (dir, List.map minify_gradient_stop stops)
  | O_repeating_linear_gradient (dir, stops) ->
      O_repeating_linear_gradient (dir, List.map minify_gradient_stop stops)
  | Radial_gradient (config, stops) ->
      Radial_gradient (config, List.map minify_gradient_stop stops)
  | Repeating_radial_gradient (config, stops) ->
      Repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | Webkit_radial_gradient (config, stops) ->
      Webkit_radial_gradient (config, List.map minify_gradient_stop stops)
  | Webkit_repeating_radial_gradient (config, stops) ->
      Webkit_repeating_radial_gradient
        (config, List.map minify_gradient_stop stops)
  | Moz_radial_gradient (config, stops) ->
      Moz_radial_gradient (config, List.map minify_gradient_stop stops)
  | Moz_repeating_radial_gradient (config, stops) ->
      Moz_repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | O_radial_gradient (config, stops) ->
      O_radial_gradient (config, List.map minify_gradient_stop stops)
  | O_repeating_radial_gradient (config, stops) ->
      O_repeating_radial_gradient (config, List.map minify_gradient_stop stops)
  | Conic_gradient (config, stops) ->
      Conic_gradient
        ( minify_conic_gradient_config config,
          List.map minify_gradient_stop stops )
  | Repeating_conic_gradient (config, stops) ->
      Repeating_conic_gradient
        ( minify_conic_gradient_config config,
          List.map minify_gradient_stop stops )
  | Webkit_gradient (Webkit_gradient.Linear ({ stops; _ } as gradient)) ->
      Webkit_gradient
        (Webkit_gradient.Linear
           { gradient with stops = List.map minify_webkit_gradient_stop stops })
  | Webkit_gradient (Webkit_gradient.Radial ({ stops; _ } as gradient)) ->
      Webkit_gradient
        (Webkit_gradient.Radial
           { gradient with stops = List.map minify_webkit_gradient_stop stops })
  | img -> img

let linear_gradient dir stops = Linear_gradient (dir, stops)

let radial_gradient
    ?(config =
      { shape = None; size = None; position = None; interpolation = None })
    stops =
  Radial_gradient (config, stops)

let color_stop c = (Color_percentage (c, None, None) : gradient_stop)
let color_position c pos = (Color_length (c, Some pos, None) : gradient_stop)
