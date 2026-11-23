(* CSS Images 4 sec. 3: linear, radial and conic gradients, their repeating and
   vendor-prefixed spellings, colour stops and colour interpolation. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image_position

let pp_hue_interpolation_method ctx = function
  | Shorter -> Pp.string ctx "shorter hue"
  | Longer -> Pp.string ctx "longer hue"
  | Increasing -> Pp.string ctx "increasing hue"
  | Decreasing -> Pp.string ctx "decreasing hue"

let rec pp_color_interpolation : color_interpolation Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_color_interpolation ctx v
  | In (space, hue) -> (
      Pp.string ctx "in ";
      Values.pp_color_space ctx space;
      match hue with
      | None -> ()
      | Some hue ->
          Pp.space ctx ();
          pp_hue_interpolation_method ctx hue)

(* CSS Color 5 section 9.1: after a polar color space (lch / oklch / hsl / hwb),
   the [<color-interpolation-method>] may carry a trailing
   [<hue-interpolation-method>] followed by [hue]. *)
let read_hue_interpolation_method t =
  let snap = Cursor.save t in
  let keyword t =
    Option.map String.lowercase_ascii_preserve (Cursor.ident_opt t)
  in
  match keyword t with
  | Some (("shorter" | "longer" | "increasing" | "decreasing") as kw) -> (
      Cursor.ws t;
      match keyword t with
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
      (* Sec. 9 gives the method the same spaces [color-mix()] takes, so it
         reads through the one reader that answers for all fifteen. Sec. 9.1
         puts a [<hue-interpolation-method>] after a polar space only, so a
         rectangular one never looks for it and a hue written after it is left
         unread, which the caller reports. *)
      let space = Values.read_color_space t in
      let hue =
        match space with
        | Hsl | Hwb | Lch | Oklch ->
            Cursor.ws t;
            read_hue_interpolation_method t
        | Srgb | Srgb_linear | Display_p3 | A98_rgb | Prophoto_rgb | Rec2020
        | Lab | Oklab | Xyz | Xyz_d50 | Xyz_d65 ->
            None
      in
      In (space, hue))

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

let rec pp_angular_position : angular_position Pp.t =
 fun ctx -> function
  | Angle a -> pp_angle ctx a
  | Percentage p -> pp_percentage ctx p
  | Zero -> Pp.char ctx '0'
  | Calc c -> pp_calc pp_angular_position ctx c

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
  | Color_angle (c, pos1, pos2_opt) -> (
      pp_color ctx c;
      Pp.space ctx ();
      pp_angular_position ctx pos1;
      match pos2_opt with
      | None -> ()
      | Some pos2 ->
          Pp.space ctx ();
          pp_angular_position ctx pos2)
  | Length len -> pp_length ctx len
  | Angle_hint a -> pp_angle ctx a
  | Channel channel -> pp_channel ctx channel
  | Percentage pct -> pp_percentage ctx pct
  | List stops -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops
  | Position pos -> pp_gradient_position ctx pos
  | Direction dir -> pp_gradient_direction ctx dir

let pp_webkit_gradient_point ctx = function
  | Webkit_gradient.Left_top -> Pp.string ctx "left top"
  | Webkit_gradient.Left_bottom -> Pp.string ctx "left bottom"
  | Webkit_gradient.Center -> Pp.string ctx "center center"
  | Webkit_gradient.Position position -> pp_position_value ctx position

let pp_webkit_gradient_stop ctx = function
  | Webkit_gradient.From color -> Pp.call "from" pp_color ctx color
  | Webkit_gradient.To color -> Pp.call "to" pp_color ctx color
  | Webkit_gradient.Color_stop (position, color) ->
      Pp.call "color-stop"
        (fun ctx (position, color) ->
          pp_percentage ctx position;
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
      | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
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
      | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
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
      | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
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
      | _ -> Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
    ctx (config, stops)

(* CSS Image Values 3 (W3C Working Draft, 17 February 2011) sec. 5.2.1, the
   syntax the Compatibility Standard sec. 3.3.2 gives the prefixed radial
   gradients: [[<bg-position>,]? [[<shape> || <size>] | [<length> |
   <percentage>]{2},]? <color-stop>#]. The position comes first, with no [at],
   and [contain] and [cover] are the shorter synonyms of [closest-side] and
   [farthest-corner]. *)
let pp_legacy_radial_size : radial_size Pp.t =
 fun ctx -> function
  | Closest_side -> Pp.string ctx "contain"
  | Farthest_corner -> Pp.string ctx "cover"
  | Ellipse_radii (a, b) ->
      pp_length_percentage ctx a;
      Pp.space ctx ();
      pp_length_percentage ctx b
  | size -> pp_radial_size ctx size

let pp_legacy_radial_gradient_named name ctx (config, stops) =
  Pp.call name
    (fun ctx ((config : radial_gradient_config), stops) ->
      (* Sec. 5.2.1 reads a lone argument that is a position or a size as a
         position, so two radii keep the position before them. *)
      let position =
        match (config.position, config.shape, config.size) with
        | None, None, Some (Ellipse_radii _) -> Some (Center : position_value)
        | position, _, _ -> position
      in
      Option.iter
        (fun position ->
          pp_position_value ctx position;
          Pp.comma ctx ())
        position;
      (match (config.shape, config.size) with
      | None, None -> ()
      | shape, size ->
          Option.iter (pp_radial_shape ctx) shape;
          Option.iter
            (fun size ->
              if Option.is_some shape then Pp.space ctx ();
              pp_legacy_radial_size ctx size)
            size;
          Pp.comma ctx ());
      Pp.list ~sep:Pp.comma pp_gradient_stop ctx stops)
    ctx (config, stops)

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

(* CSS Image Values 3 (W3C Working Draft, 17 February 2011) sec. 5.1.1, the
   syntax the Compatibility Standard sec. 3.3.1 gives the prefixed linear
   gradients: a side keyword names where the gradient starts, [left], [bottom],
   [right] and [top] being 0deg, 90deg, 180deg and 270deg measured
   counterclockwise from the right, and an omitted direction defaults to [top].
   The reader keeps the keyword the sheet wrote in the [To_] constructor of the
   same name, and the legacy printer writes it back, so only the default folds:
   [top], and an angle on 270deg. *)
let rec normalize_legacy_gradient_direction :
    gradient_direction -> gradient_direction =
 fun value ->
  match value with
  | Default_direction -> value
  | To_top -> Default_direction
  | Angle a -> (
      match Values.angle_degrees_opt a with
      | Some deg when Float.rem (deg -. 270.) 360. = 0. -> Default_direction
      | _ -> preserve_if_equal value (Angle (Values.normalize_angle a)))
  | With_interpolation (dir, interp) ->
      preserve_if_equal value
        (With_interpolation (normalize_legacy_gradient_direction dir, interp))
  | To_right | To_bottom | To_left | To_top_right | To_bottom_right
  | To_bottom_left | To_top_left | Var _ ->
      value

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

(* A [var()] or [attr()] standing for a stop's colour is substituted before the
   stop list is parsed (CSS Values 5 sec. 7), so it may hold a position or
   several stops ([--a:red 20%]). A stop whose colour is one is not known to be
   a single stop, and no rewrite that moves or drops positions applies to it. *)
let single_stop_color : color -> bool = function
  | Var _ | Attribute _ -> false
  | _ -> true

(* CSS Images 4 sec. 3.5.3: an omitted position on the first stop is [0%] and on
   the last [100%], so a stop holding exactly that one position drops it and the
   resolved stop list stays the same. On a conic gradient's circle [100%] is
   [360deg] (sec. 3.3.2). *)
let drop_default_stop_positions stops =
  let angular_at expected : angular_position -> bool = function
    | Percentage (Pct p) -> Float.equal p expected
    | Angle a -> (
        match Values.angle_degrees_opt a with
        | Some deg -> Float.equal deg (expected *. 3.6)
        | None -> false)
    | Zero -> Float.equal expected 0.
    | Percentage _ | Calc _ -> false
  in
  let at expected = function
    | Color_percentage (c, Some (Pct p), None) ->
        single_stop_color c && Float.equal p expected
    | Color_angle (c, pos, None) ->
        single_stop_color c && angular_at expected pos
    | _ -> false
  in
  let strip = function
    | Color_percentage (c, _, _) | Color_angle (c, _, _) ->
        Color_percentage (c, None, None)
    | stop -> stop
  in
  let rec last = function
    | [] -> []
    | [ stop ] when at 100. stop -> [ strip stop ]
    | stop :: rest as stops ->
        let rest' = last rest in
        if rest' == rest then stops else stop :: rest'
  in
  match stops with
  | first :: rest when at 0. first -> strip first :: last rest
  | stops -> last stops

(* Sec. 3.3.2 measures a conic percentage against 360deg, so an angle that is a
   short exact percentage of the turn is that percentage, and a zero angle is
   [0]; a radian other than zero never is. *)
let angle_turn_percentage (a : angle) =
  let pct =
    match a with
    | Deg d -> Some (d *. 100. /. 360.)
    | Turn t -> Some (t *. 100.)
    | Grad g -> Some (g /. 4.)
    | Rad r when Float.equal r 0. -> Some 0.
    | Rad _ | Round _ | Mod _ | Rem _ | Calc _ | Var _ | Invalid _ -> None
  in
  match pct with
  | Some p when Float.is_integer (p *. 1000.) -> Some p
  | Some _ | None -> None

let normalize_angular_position (value : angular_position) : angular_position =
  match value with
  | Angle a -> (
      let a' = Values.normalize_angle a in
      match angle_turn_percentage a' with
      | Some p when Float.equal p 0. -> Zero
      | Some p -> Percentage (Pct p)
      | None -> if a' == a then value else Angle a')
  | Percentage _ | Zero | Calc _ -> value

(* A conic stop every position of which is a percentage or a zero is the stop a
   linear gradient reads, so the rewrites over those apply to it. A lone zero
   stays an angle, since a lone [0] reads back as a length. *)
let percentage_stop c p1 p2 : gradient_stop option =
  let lp : angular_position -> length_percentage option = function
    | Percentage (Pct p) -> Some (Pct p)
    | Zero -> Some (Length Zero)
    | Percentage _ | Angle _ | Calc _ -> None
  in
  match (lp p1, Option.map lp p2) with
  | Some a, None -> Some (Color_percentage (c, Some a, None))
  | Some a, Some (Some b) -> Some (Color_percentage (c, Some a, Some b))
  | _ -> None

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
  | Color_angle (c0, p1_0, p2_0) -> (
      let c = normalize_color ~lossless c0 in
      let p1 = normalize_angular_position p1_0 in
      let p2 = option_map_preserve normalize_angular_position p2_0 in
      match (p1, p2, p1_0) with
      | Zero, None, Angle (Deg d) when Float.equal d 0. && c == c0 -> value
      | Zero, None, _ -> Color_angle (c, Angle (Deg 0.), None)
      | _ -> (
          match percentage_stop c p1 p2 with
          | Some stop -> stop
          | None ->
              if c == c0 && p1 == p1_0 && p2 == p2_0 then value
              else Color_angle (c, p1, p2)))
  | Length l ->
      preserve_if_equal value (Length (Values.normalize_length ~strip:false l))
  | Angle_hint a -> (
      match normalize_angular_position (Angle a) with
      | Percentage p -> Percentage p
      | Zero -> Length Zero
      | Angle a' -> preserve_if_equal value (Angle_hint a')
      | Calc _ -> value)
  | Direction dir ->
      preserve_if_equal value (Direction (normalize_gradient_direction dir))
  | List stops ->
      preserve_if_equal value
        (List (map_preserve (normalize_gradient_stop ~lossless) stops))
  | other -> other

(* The legacy WebKit gradient takes a point as numbers and percentages as well
   as keywords, and a stop position as a number in [0, 1] as well as a
   percentage, with [from()] and [to()] the stops at [0] and [1]. The shortest
   spellings are those. *)
let normalize_webkit_gradient_point (point : Webkit_gradient.point) :
    Webkit_gradient.point =
  match point with
  | Left_top -> Position (XY (Zero, Zero))
  | Left_bottom -> Position (XY (Zero, Pct 100.))
  | Center -> Position (XY (Pct 50., Pct 50.))
  | Position _ -> point

let normalize_webkit_gradient_stop :
    ?lossless:bool -> Webkit_gradient.stop -> Webkit_gradient.stop =
 fun ?(lossless = false) value ->
  match value with
  | From c -> preserve_if_equal value (From (normalize_color ~lossless c))
  | To c -> preserve_if_equal value (To (normalize_color ~lossless c))
  | Color_stop (p, c) -> (
      let c = normalize_color ~lossless c in
      let p : percentage =
        match p with Pct f -> Num (Pp.printed (f /. 100.)) | p -> p
      in
      match (p, value) with
      | Num 0., _ -> From c
      | Num 1., _ -> To c
      | p, Color_stop (p0, c0) when p == p0 && c == c0 -> value
      | p, _ -> Color_stop (p, c))

let normalize_webkit_gradient ?(lossless = false) :
    Webkit_gradient.t -> Webkit_gradient.t =
 fun value ->
  let point = normalize_webkit_gradient_point in
  let stops = map_preserve (normalize_webkit_gradient_stop ~lossless) in
  match value with
  | Linear r ->
      let start = point r.start
      and finish = point r.finish
      and stops = stops r.stops in
      if start == r.start && finish == r.finish && stops == r.stops then value
      else Linear { start; finish; stops }
  | Radial r ->
      let inner_center = point r.inner_center
      and outer_center = point r.outer_center
      and stops = stops r.stops in
      if
        inner_center == r.inner_center
        && outer_center == r.outer_center
        && stops == r.stops
      then value
      else Radial { r with inner_center; outer_center; stops }

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
    when single_stop_color c1 && Values.equal_color c1 c2 ->
      Color_percentage (c1, Option.Some p1, Option.Some p2)
      :: fold_double_position_stops rest
  | Color_length (c1, Option.Some l1, Option.None)
    :: Color_length (c2, Option.Some l2, Option.None)
    :: rest
    when single_stop_color c1 && Values.equal_color c1 c2 ->
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
   be mirrored to [100% - p], which is longer rather than shorter for lengths,
   and only when each stop is one stop ([single_stop_color]). The legacy
   prefixed gradients measure their angle from a different zero, so they are
   deliberately not folded here. *)
let reversible_unpositioned_stops (stops : gradient_stop list) =
  List.length stops > 1
  && List.for_all
       (fun stop ->
         match stop with
         | Color_percentage (c, Option.None, Option.None)
         | Color_length (c, Option.None, Option.None) ->
             single_stop_color c
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

  (* CSS Images 4 sec. 3.3.2: a conic stop is placed by [[<angle-percentage> |
     <zero>]{1,2}] and a hint by one. These read only a stop holding an angle,
     not a [var()] that may hold one, so a stop of percentages and zeros keeps
     the reading every gradient gives it. *)
  let read_angular_zero t : angular_position =
    let n, unit = Cursor.number_with_unit t in
    if Float.equal n 0. && Option.is_none unit then Zero
    else Cursor.err_expected t "0"

  (* A math function over angles and percentages together, which neither the
     angle nor the percentage reader takes. *)
  let rec read_angular_operand t : angular_position =
    Cursor.one_of
      [
        (fun t : angular_position -> Angle (Values.read_angle_unit_required t));
        (fun t : angular_position -> Percentage (read_percentage t));
        (fun t : angular_position ->
          if Cursor.looking_at_calc t then
            Calc (Values.read_calc ~result_type:`Value read_angular_operand t)
          else Cursor.err_expected t "an angle or a percentage");
      ]
      t

  let read_angular_position t : angular_position =
    Cursor.one_of [ read_angular_operand; read_angular_zero ] t

  let is_angle : angular_position -> bool = function
    | Angle (Var _) -> false
    | Angle _ | Calc _ -> true
    | Percentage _ | Zero -> false

  let read_color_angle_angle t =
    let color, p1, p2 =
      Cursor.triple ~sep:Cursor.ws read_color read_angular_position
        read_angular_position t
    in
    if is_angle p1 || is_angle p2 then Color_angle (color, p1, Some p2)
    else Cursor.err_expected t "an angle"

  let read_color_angle t =
    let color, p1 =
      Cursor.pair ~sep:Cursor.ws read_color read_angular_position t
    in
    if is_angle p1 then Color_angle (color, p1, None)
    else Cursor.err_expected t "an angle"

  let read_angle_hint t : gradient_stop =
    match Values.read_angle_unit_required t with
    | Var _ -> Cursor.err_expected t "an angle"
    | a -> Angle_hint a
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

(* A conic stop or hint: one placed by an angle first, then every other. Sec.
   3.3.2 places neither by a length, so a stop the linear reader takes with a
   length is no conic stop. *)
let read_angular_gradient_stop t : gradient_stop =
  let no_length t : gradient_stop =
    let is_length : Values.length -> bool = function
      | Zero | Var _ | Calc _ -> false
      | _ -> true
    in
    let placed_by_length : length_percentage option -> bool = function
      | Some (Length l) -> is_length l
      | Some _ | None -> false
    in
    match read_gradient_stop_single t with
    | Color_percentage (_, p1, p2)
      when placed_by_length p1 || placed_by_length p2 ->
        Cursor.err_expected t "an angle or a percentage"
    | Color_length (_, Some l, _) when is_length l ->
        Cursor.err_expected t "an angle or a percentage"
    | Length l when is_length l ->
        Cursor.err_expected t "an angle or a percentage"
    | stop -> stop
  in
  Cursor.ws t;
  Cursor.one_of
    [
      Gradient_stop.read_color_angle_angle;
      Gradient_stop.read_color_angle;
      Gradient_stop.read_angle_hint;
      no_length;
    ]
    t

let read_gradient_stops ?(angular = false) t =
  let stop =
    if angular then read_angular_gradient_stop else read_gradient_stop
  in
  match Cursor.option (Cursor.list ~at_least:1 ~sep:Cursor.comma stop) t with
  | Some stops -> stops
  | None -> []

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
    match Cursor.peek_keyword t with
    | Some "from" ->
        let _ = Cursor.ident t in
        Cursor.ws t;
        Values.read_angle t
    | _ -> Cursor.err_expected t "from"
  in
  let read_at t =
    match Cursor.peek_keyword t with
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
      match Cursor.peek_keyword t with
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
  Webkit_linear_gradient
    (Option.value ~default:Default_direction direction, stops)

let read_radial_gradient_body_stops t =
  Cursor.ws t;
  (* CSS Values 4 sec. 2.6: the comma after the prelude is omitted only with the
     prelude, so a prelude the reader stops inside is an error rather than a
     prelude followed by stops. *)
  let config =
    match
      Cursor.option
        (fun t ->
          let cfg = Radial_config.read t in
          if
            cfg.shape = None && cfg.size = None && cfg.position = None
            && cfg.interpolation = None
          then Cursor.err_invalid t "no radial config";
          cfg)
        t
    with
    | Some cfg ->
        Cursor.ws t;
        if not (Cursor.comma_opt t) then
          Cursor.err_expected t "',' after radial-gradient prelude";
        cfg
    | None ->
        { shape = None; size = None; position = None; interpolation = None }
  in
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in radial-gradient()";
  Radial_gradient (config, stops)

(* The legacy prelude of [pp_legacy_radial_gradient_named], as Chromium reads
   it. The position is CSS 2.1's one- or two-value one. A radius is
   non-negative, as Chromium takes a zero one. An argument that reads as a
   position or a size is a position (sec. 5.2.1). Chromium also takes the comma
   after the shape and size as optional. The stops are colour stops only, with
   no interpolation hint and no colour space. *)
let read_legacy_radial_size_keyword t : radial_size =
  Cursor.enum "radial-size"
    [
      ("closest-side", (Closest_side : radial_size));
      ("contain", Closest_side);
      ("farthest-side", Farthest_side);
      ("closest-corner", Closest_corner);
      ("farthest-corner", Farthest_corner);
      ("cover", Farthest_corner);
    ]
    t

let read_legacy_radial_gradient_body t =
  let read_position t =
    let position = read_legacy_position_value t in
    Cursor.ws t;
    Cursor.comma t;
    position
  in
  let read_radius t =
    let radius = read_length_percentage ~allow_negative:false t in
    Cursor.ws t;
    radius
  in
  let read_radii t : radial_size =
    let a = read_radius t in
    let b = read_radius t in
    Ellipse_radii (a, b)
  in
  Cursor.ws t;
  let position = Cursor.option read_position t in
  Cursor.ws t;
  let shape = Cursor.option Radial_config.read_shape t in
  Cursor.ws t;
  let size = Cursor.option read_legacy_radial_size_keyword t in
  Cursor.ws t;
  let shape =
    match shape with
    | None -> Cursor.option Radial_config.read_shape t
    | shape -> shape
  in
  let size =
    match (shape, size) with
    | None, None -> Cursor.option read_radii t
    | _ -> size
  in
  if Option.is_some shape || Option.is_some size then (
    Cursor.ws t;
    ignore (Cursor.comma_opt t));
  let stops = read_gradient_stops t in
  if stops = [] then
    Cursor.err_expected t "at least one color stop in -webkit-radial-gradient()";
  List.iter
    (function
      | (Length _ | Percentage _ : gradient_stop) ->
          Cursor.err_invalid t "an interpolation hint in a legacy gradient"
      | _ -> ())
    stops;
  Radial_gradient ({ shape; size; position; interpolation = None }, stops)

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
  let stops = read_gradient_stops ~angular:true t in
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
  | _ ->
      Cursor.err_invalid t
        (String.concat "" [ "-webkit-gradient kind: "; kind ])

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

let minify_gradient_stop : gradient_stop -> gradient_stop = function
  | Color_percentage (c, p1, p2) -> Color_percentage (minify_color c, p1, p2)
  | s -> s

let minify_webkit_gradient_stop = function
  | Webkit_gradient.From color -> Webkit_gradient.From (minify_color color)
  | Webkit_gradient.To color -> Webkit_gradient.To (minify_color color)
  | Webkit_gradient.Color_stop (position, color) ->
      Webkit_gradient.Color_stop (position, minify_color color)

let linear_gradient dir stops = Linear_gradient (dir, stops)

let radial_gradient
    ?(config =
      { shape = None; size = None; position = None; interpolation = None })
    stops =
  Radial_gradient (config, stops)

let color_stop c = (Color_percentage (c, None, None) : gradient_stop)
let color_position c pos = (Color_length (c, Some pos, None) : gradient_stop)
