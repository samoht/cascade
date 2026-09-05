(* css-transforms-2 and motion-1: the [transform] shorthand and its functions,
   the individual [rotate] / [scale] / [translate] properties,
   [transform-origin] / [-box] / [-style], [backface-visibility],
   [perspective-origin], and the [offset-*] motion path properties.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image

module Transform = struct
  let read_translate_x t =
    Cursor.call "translatex" t (fun t -> Translate_x (read_length t))

  let read_translate_y t =
    Cursor.call "translatey" t (fun t -> Translate_y (read_length t))

  let read_translate_z t =
    Cursor.call "translatez" t (fun t -> Translate_z (read_length t))

  let read_translate3d t =
    Cursor.call "translate3d" t (fun t ->
        let x, y, z =
          Cursor.(triple ~sep:comma read_length read_length read_length) t
        in
        Translate_3d (x, y, z))

  let read_translate t =
    Cursor.call "translate" t (fun t ->
        let x = read_length t in
        let y =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_length t)
            t
        in
        (Translate (x, y) : transform))

  let read_rotate_x t =
    Cursor.call "rotatex" t (fun t -> Rotate_x (read_angle t))

  let read_rotate_y t =
    Cursor.call "rotatey" t (fun t -> Rotate_y (read_angle t))

  let read_rotate_z t =
    Cursor.call "rotatez" t (fun t -> Rotate_z (read_angle t))

  let read_rotate t : transform =
    Cursor.call "rotate" t (fun t ->
        Cursor.one_of
          [
            (fun t ->
              let x = Cursor.number t in
              Cursor.ws t;
              let y = Cursor.number t in
              Cursor.ws t;
              let z = Cursor.number t in
              Cursor.ws t;
              let angle = read_angle t in
              (Rotate_axis (x, y, z, angle) : transform));
            (fun t -> (Rotate (read_angle t) : transform));
          ]
          t)

  let read_scale_x t =
    Cursor.call "scalex" t (fun t -> Scale_x (Values.read_number_percentage t))

  let read_scale_y t =
    Cursor.call "scaley" t (fun t -> Scale_y (Values.read_number_percentage t))

  let read_scale_z t =
    Cursor.call "scalez" t (fun t -> Scale_z (Values.read_number_percentage t))

  let read_rotate3d t =
    Cursor.call "rotate3d" t (fun t ->
        let x, y, z = Cursor.(triple ~sep:comma number number number) t in
        Cursor.comma t;
        let angle = read_angle t in
        Rotate_3d (x, y, z, angle))

  let read_scale3d t =
    Cursor.call "scale3d" t (fun t ->
        let x, y, z =
          Cursor.(
            triple ~sep:comma Values.read_number_percentage
              Values.read_number_percentage Values.read_number_percentage)
            t
        in
        Scale_3d (x, y, z))

  let read_scale t : transform =
    Cursor.call "scale" t (fun t ->
        let x = Values.read_number_percentage t in
        let y =
          Cursor.option
            (fun t ->
              let has_comma = Cursor.comma_opt t in
              Cursor.ws t;
              (has_comma, Values.read_number_percentage t))
            t
        in
        match y with
        | None ->
            Cursor.ws t;
            Cursor.expect_eof t;
            (Scale (x, None) : transform)
        | Some (true, y) ->
            Cursor.ws t;
            Cursor.expect_eof t;
            Scale (x, Some y)
        | Some (false, y) ->
            Cursor.ws t;
            Cursor.expect_eof t;
            Scale_space (x, y))

  let read_skew_x t = Cursor.call "skewx" t (fun t -> Skew_x (read_angle t))
  let read_skew_y t = Cursor.call "skewy" t (fun t -> Skew_y (read_angle t))

  let read_skew t =
    Cursor.call "skew" t (fun t ->
        let x = read_angle t in
        let y =
          Cursor.option
            (fun t ->
              Cursor.comma t;
              read_angle t)
            t
        in
        Skew (x, y))

  let read_matrix t =
    Cursor.call "matrix" t (fun t ->
        match Cursor.list ~sep:Cursor.comma Cursor.number t with
        | [ a; b; c; d; e; f ] -> Matrix (a, b, c, d, e, f)
        | _ -> err_invalid_value t "matrix" "expected 6 arguments")

  let read_matrix3d t =
    Cursor.call "matrix3d" t (fun t ->
        match Cursor.list ~sep:Cursor.comma Cursor.number t with
        | [
         m11;
         m12;
         m13;
         m14;
         m21;
         m22;
         m23;
         m24;
         m31;
         m32;
         m33;
         m34;
         m41;
         m42;
         m43;
         m44;
        ] ->
            Matrix_3d
              ( m11,
                m12,
                m13,
                m14,
                m21,
                m22,
                m23,
                m24,
                m31,
                m32,
                m33,
                m34,
                m41,
                m42,
                m43,
                m44 )
        | _ -> err_invalid_value t "matrix3d" "expected 16 arguments")

  let read_perspective t : transform =
    Cursor.call "perspective" t (fun inner ->
        Cursor.ws inner;
        let len = read_length inner in
        Cursor.ws inner;
        Cursor.expect_eof inner;
        (Perspective len : transform))

  let parsers =
    [
      ("translatex", read_translate_x);
      ("translatey", read_translate_y);
      ("translatez", read_translate_z);
      ("translate3d", read_translate3d);
      ("translate", read_translate);
      ("rotatex", read_rotate_x);
      ("rotatey", read_rotate_y);
      ("rotatez", read_rotate_z);
      ("rotate3d", read_rotate3d);
      ("rotate", read_rotate);
      ("scalex", read_scale_x);
      ("scaley", read_scale_y);
      ("scalez", read_scale_z);
      ("scale3d", read_scale3d);
      ("scale", read_scale);
      ("skewx", read_skew_x);
      ("skewy", read_skew_y);
      ("skew", read_skew);
      ("matrix", read_matrix);
      ("matrix3d", read_matrix3d);
      ("perspective", read_perspective);
    ]
end

let is_transform_none (value : transform) =
  match value with None -> true | _ -> false

let rec read_transform t : transform =
  (* Add var support to the parsers list *)
  let read_var_transform t : transform =
    Var (Values.read_var read_transform_value t)
  in
  Cursor.enum_or_calls "transform"
    [
      ("none", (None : transform));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:(("var", read_var_transform) :: Transform.parsers)
    t

and read_transform_value t : transform =
  match read_transforms t with [ x ] -> x | xs -> (List xs : transform)

and read_transforms t : transform list =
  let transforms, error_opt = Cursor.many read_transform t in
  if List.length transforms = 0 then
    match error_opt with
    | Some msg -> Cursor.err_invalid t ("transform: " ^ msg)
    | None -> Cursor.err_invalid t "transform value"
  else if List.length transforms > 1 && List.exists is_transform_none transforms
  then Cursor.err_invalid t "transform none cannot be combined"
  else transforms

let normalize_rotate : rotate_value -> rotate_value =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Angle a -> preserve_if_equal value (Angle (na a))
    | X a -> preserve_if_equal value (X (na a))
    | Y a -> preserve_if_equal value (Y (na a))
    | Z a -> preserve_if_equal value (Z (na a))
    | Axis (x, y, z, a) -> preserve_if_equal value (Axis (x, y, z, na a))
    | other -> other

let normalize_translate_value : translate_value -> translate_value =
  let nl = Values.normalize_length in
  fun value ->
    match value with
    | X x -> preserve_if_equal value (X (nl x))
    | XY (x, y) -> preserve_if_equal value (XY (nl x, nl y))
    | XYZ (x, y, z) -> preserve_if_equal value (XYZ (nl x, nl y, nl z))
    | other -> other

(* The CSS [scale] property: pick the shorter [<number-percentage>] spelling so
   pp serialises the canonical node. The X/XY/XYZ variants are distinct from the
   transform [scale()] family - those live in [normalize_transform_leaves]. *)
let normalize_scale : scale -> scale =
 fun value ->
  let np = Values.normalize_number_percentage in
  match value with
  | X x -> preserve_if_equal value (X (np x))
  | XY (x, y) -> preserve_if_equal value (XY (np x, np y))
  | XYZ (x, y, z) -> preserve_if_equal value (XYZ (np x, np y, np z))
  | other -> other

(* Canonicalise equivalent origin keywords and coordinates before factoring.
   [0], [0px] and [0%] all place the origin at the same edge. *)
let rec normalize_transform_origin : transform_origin -> transform_origin =
  let z l =
    match (l : Values.length) with
    | Pct 0. -> (Zero : Values.length)
    | _ -> Values.normalize_length l
  in
  let pct n = (Pct n : Values.length) in
  let zero = (Zero : Values.length) in
  fun value ->
    match value with
    | Center | Center_center -> X (pct 50.)
    | Left | Left_center -> X zero
    | Right | Right_center -> X (pct 100.)
    | Left_top | Top_left -> XY (zero, zero)
    | Right_top | Top_right -> XY (pct 100., zero)
    | Left_bottom | Bottom_left -> XY (zero, pct 100.)
    | Right_bottom | Bottom_right -> XY (pct 100., pct 100.)
    | Center_top -> Top
    | Center_bottom -> Bottom
    | X a -> preserve_if_equal value (X (z a))
    | XY (a, b) -> (
        let a = z a and b = z b in
        match b with Pct 50. -> X a | _ -> preserve_if_equal value (XY (a, b)))
    | XYZ (a, b, c) -> preserve_if_equal value (XYZ (z a, z b, z c))
    | Position p -> (
        let p = normalize_position_value p in
        match position_xy p with
        | Some (a, b) -> normalize_transform_origin (XY (a, b))
        | None -> preserve_if_equal value (Position p))
    | Position_z (p, c) -> (
        let p = normalize_position_value p in
        let c = z c in
        match position_xy p with
        | Some (a, b) -> normalize_transform_origin (XYZ (a, b, c))
        | None -> preserve_if_equal value (Position_z (p, c)))
    | Var v ->
        let v' = map_var_preserve normalize_transform_origin v in
        if v' == v then value else Var v'
    | other -> other

let normalize_offset_path : offset_path -> offset_path =
 fun value ->
  match value with
  | Ray ray ->
      preserve_if_equal value
        (Ray
           {
             ray with
             angle = Values.normalize_angle ray.angle;
             position =
               option_map_preserve
                 (normalize_position_value ~strip:false)
                 ray.position;
           })
  | other -> other

let rec normalize_offset_anchor (value : offset_anchor) =
  match value with
  | Position position ->
      preserve_if_equal value (Position (normalize_position_value position))
  | Var var ->
      let var' = map_var_preserve normalize_offset_anchor var in
      if var' == var then value else Var var'
  | other -> other

let rec normalize_offset_position (value : offset_position) =
  match value with
  | Position position ->
      preserve_if_equal value (Position (normalize_position_value position))
  | Var var ->
      let var' = map_var_preserve normalize_offset_position var in
      if var' == var then value else Var var'
  | other -> other

let normalize_offset_rotate : offset_rotate -> offset_rotate =
  let na = Values.normalize_angle in
  fun value ->
    match value with
    | Angle a -> preserve_if_equal value (Angle (na a))
    | With_angle (mode, a) -> preserve_if_equal value (With_angle (mode, na a))
    | other -> other

let pp_perspective_origin : perspective_origin Pp.t = pp_position_value

let pp_rotate_3d : (float * float * float * angle) Pp.t =
 fun ctx (x, y, z, a) ->
  Pp.string ctx "rotate3d(";
  Pp.float ctx x;
  Pp.comma ctx ();
  Pp.float ctx y;
  Pp.comma ctx ();
  Pp.float ctx z;
  Pp.comma ctx ();
  pp_angle ctx a;
  Pp.char ctx ')'

let pp_translate_3d : (length * length * length) Pp.t =
 fun ctx (x, y, z) ->
  Pp.string ctx "translate3d(";
  pp_length ctx x;
  Pp.comma ctx ();
  pp_length ctx y;
  Pp.comma ctx ();
  pp_length ctx z;
  Pp.char ctx ')'

let pp_matrix_3d : _ Pp.t =
 fun ctx (a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4) ->
  let values =
    [ a1; a2; a3; a4; b1; b2; b3; b4; c1; c2; c3; c4; d1; d2; d3; d4 ]
  in
  Pp.string ctx "matrix3d(";
  Pp.list ~sep:Pp.comma Pp.float ctx values;
  Pp.char ctx ')'

(* Tailwind concatenates var() calls without spaces, but uses spaces between
   normal transform functions. This helper determines if a transform is a Var *)
let is_transform_var : transform -> bool = function Var _ -> true | _ -> false

(* CSS Transforms 1 sec. 12: collapse a transform function to a shorter
   equivalent when an axis is zero / unity / matches another. Spec-equivalent in
   every case - they all map to the same matrix. *)
let canonicalise_transform : transform -> transform = function
  | Translate_3d (x, y, z)
    when Values.length_is_zero y && Values.length_is_zero z ->
      Translate (x, None)
  | Translate_3d (x, y, z)
    when Values.length_is_zero x && Values.length_is_zero z ->
      Translate_y y
  | Translate_3d (x, y, z)
    when Values.length_is_zero x && Values.length_is_zero y ->
      Translate_z z
  | Translate (x, Some y) when Values.length_is_zero y -> Translate (x, None)
  | Translate (x, Some y) when Values.length_is_zero x -> Translate_y y
  | Translate_x x -> Translate (x, None)
  | Scale (x, Some (Num 1.)) -> Scale_x x
  | Scale (Num 1., Some y) -> Scale_y y
  | Scale (x, Some y) when Values.equal_number_percentage x y -> Scale (x, None)
  | Scale_3d (x, y, Num 1.) -> Scale (x, Some y)
  | Rotate_z a -> Rotate a
  | Rotate_3d (1., 0., 0., a) -> Rotate_x a
  | Rotate_3d (0., 1., 0., a) -> Rotate_y a
  | Rotate_3d (0., 0., 1., a) -> Rotate a
  | other -> other

(* Drive [canonicalise_transform] to a fixed point and recurse into a transform
   list: one rewrite can expose another (e.g. [scale3d(a,a,1)] -> [scale(a,a)]
   -> [scale(a)]), so a single application is not idempotent. This is the
   AST-level normaliser; [pp_transform] stays a pure serialiser of its
   result. *)
(* Fold the [<length>] operands of the translate / perspective functions; the
   angle and number-percentage operands still fold through their own printers.
   Running this before [canonicalise_transform] lets the zero-checks see a folded
   [calc()]. *)
let normalize_transform_leaves : transform -> transform =
  (* The translate / perspective operands are inside a function, so they keep a
     zero's unit ([translate(0px)] stays). The rotate / skew operands are
     angles, converted to the shortest unit. The scale operands are
     [<number-percentage>]: pick the shorter spelling so pp does not have to
     fold the [Pct]/[Num] node distinction. *)
  let nl = Values.normalize_length ~strip:false in
  let na = Values.normalize_angle in
  let np = Values.normalize_number_percentage in
  function
  | Translate (x, y) -> Translate (nl x, Option.map nl y)
  | Translate_x x -> Translate_x (nl x)
  | Translate_y y -> Translate_y (nl y)
  | Translate_z z -> Translate_z (nl z)
  | Translate_3d (x, y, z) -> Translate_3d (nl x, nl y, nl z)
  | Perspective l -> Perspective (nl l)
  | Rotate a -> Rotate (na a)
  | Rotate_x a -> Rotate_x (na a)
  | Rotate_y a -> Rotate_y (na a)
  | Rotate_z a -> Rotate_z (na a)
  | Rotate_3d (x, y, z, a) -> Rotate_3d (x, y, z, na a)
  | Rotate_axis (x, y, z, a) -> Rotate_axis (x, y, z, na a)
  | Skew (a, b) -> Skew (na a, Option.map na b)
  | Skew_x a -> Skew_x (na a)
  | Skew_y a -> Skew_y (na a)
  | Scale (x, y) -> Scale (np x, Option.map np y)
  | Scale_space (x, y) -> Scale_space (np x, np y)
  | Scale_x x -> Scale_x (np x)
  | Scale_y y -> Scale_y (np y)
  | Scale_z z -> Scale_z (np z)
  | Scale_3d (x, y, z) -> Scale_3d (np x, np y, np z)
  | other -> other

let rec normalize_transform (t : transform) : transform =
  match t with
  | List ts -> List (List.map normalize_transform ts)
  | _ ->
      let t = normalize_transform_leaves t in
      let t' = canonicalise_transform t in
      if t' == t then t else normalize_transform t'

let rec pp_transform : transform Pp.t =
 fun ctx t ->
  (* Pure serialiser: the [canonicalise_transform] fold lives in
     [normalize_transform], so minify makes no semantic choice here. *)
  pp_transform_raw ctx t

and pp_transform_raw : transform Pp.t =
 fun ctx -> function
  | None -> pp_keyword "none" ctx
  | Translate (x, None) -> Pp.call "translate" pp_length ctx x
  | Translate (x, Some y) -> Pp.call_2 "translate" pp_length pp_length ctx (x, y)
  | Translate_x x -> Pp.call "translateX" pp_length ctx x
  | Translate_y y -> Pp.call "translateY" pp_length ctx y
  | Translate_z z -> Pp.call "translateZ" pp_length ctx z
  | Translate_3d (x, y, z) -> pp_translate_3d ctx (x, y, z)
  | Rotate a -> Pp.call "rotate" pp_angle ctx a
  | Rotate_x a -> Pp.call "rotateX" pp_angle ctx a
  | Rotate_y a -> Pp.call "rotateY" pp_angle ctx a
  | Rotate_z a -> Pp.call "rotateZ" pp_angle ctx a
  | Rotate_3d (x, y, z, a) -> pp_rotate_3d ctx (x, y, z, a)
  | Rotate_axis (x, y, z, a) ->
      Pp.string ctx "rotate(";
      Pp.float ctx x;
      Pp.space ctx ();
      Pp.float ctx y;
      Pp.space ctx ();
      Pp.float ctx z;
      Pp.space ctx ();
      pp_angle ctx a;
      Pp.char ctx ')'
  | Scale (x, None) -> Pp.call "scale" pp_number_percentage ctx x
  | Scale (x, Some y) ->
      Pp.call_2 "scale" pp_number_percentage pp_number_percentage ctx (x, y)
  | Scale_space (x, y) ->
      Pp.string ctx "scale(";
      pp_number_percentage ctx x;
      Pp.space ctx ();
      pp_number_percentage ctx y;
      Pp.char ctx ')'
  | Scale_x v -> Pp.call "scaleX" pp_number_percentage ctx v
  | Scale_y v -> Pp.call "scaleY" pp_number_percentage ctx v
  | Scale_z v -> Pp.call "scaleZ" pp_number_percentage ctx v
  | Scale_3d (x, y, z) ->
      Pp.call_3 "scale3d" pp_number_percentage pp_number_percentage
        pp_number_percentage ctx (x, y, z)
  | Skew (x, None) -> Pp.call "skew" pp_angle ctx x
  | Skew (x, Some y) -> Pp.call_2 "skew" pp_angle pp_angle ctx (x, y)
  | Skew_x x -> Pp.call "skewX" pp_angle ctx x
  | Skew_y y -> Pp.call "skewY" pp_angle ctx y
  | Matrix (a, b, c, d, e, f) ->
      Pp.call_list "matrix" Pp.float ctx [ a; b; c; d; e; f ]
  | Matrix_3d m -> pp_matrix_3d ctx m
  | Perspective p -> Pp.call "perspective" pp_length ctx p
  | Inherit -> pp_keyword "inherit" ctx
  | Initial -> pp_keyword "initial" ctx
  | Unset -> pp_keyword "unset" ctx
  | Revert -> pp_keyword "revert" ctx
  | Revert_layer -> pp_keyword "revert-layer" ctx
  | Var v -> pp_var pp_transform ctx v
  | List transforms -> pp_transforms ctx transforms

and pp_transforms : transform list Pp.t =
 fun ctx transforms ->
  let rec loop prev = function
    | [] -> ()
    | x :: rest ->
        (* CSS Transforms 1: chained transform functions are separated by
           whitespace in the source. The closing [)] and following [(] serve as
           token boundaries, so under minify the space is redundant. [Var]
           adjacencies always need a space because var fallback parsing is
           whitespace-sensitive. *)
        if is_transform_var prev && is_transform_var x then Pp.space ctx ()
        else Pp.sp ctx ();
        pp_transform ctx x;
        loop x rest
  in
  match transforms with
  | [] -> ()
  | [ x ] -> pp_transform ctx x
  | h :: t ->
      pp_transform ctx h;
      loop h t

let rec pp_transform_style : transform_style Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_transform_style ctx v
  | Flat -> Pp.string ctx "flat"
  | Preserve_3d -> Pp.string ctx "preserve-3d"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_transform_origin_keywords ctx = function
  | Center -> Pp.string ctx "center"
  | Center_center -> Pp.string ctx "center center"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
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
  | _ -> ()

let rec pp_transform_origin : transform_origin Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | ( Center | Center_center | Left | Right | Top | Bottom | Left_top
    | Left_center | Left_bottom | Right_top | Right_center | Right_bottom
    | Center_top | Center_bottom | Top_left | Top_right | Bottom_left
    | Bottom_right ) as value ->
      pp_transform_origin_keywords ctx value
  | Position position -> pp_position_value ctx position
  | X a -> pp_length ctx a
  | XY (a, b) ->
      pp_length ctx a;
      Pp.space ctx ();
      pp_length ctx b
  | XYZ (a, b, z) ->
      pp_length ctx a;
      Pp.space ctx ();
      pp_length ctx b;
      Pp.space ctx ();
      pp_length ctx z
  | Position_z (position, z) ->
      pp_position_value ctx position;
      Pp.space ctx ();
      pp_length ctx z
  | Var v -> pp_var pp_transform_origin ctx v

let rec pp_transform_box : transform_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_transform_box ctx v
  | Content_box -> Pp.string ctx "content-box"
  | Border_box -> Pp.string ctx "border-box"
  | Fill_box -> Pp.string ctx "fill-box"
  | Stroke_box -> Pp.string ctx "stroke-box"
  | View_box -> Pp.string ctx "view-box"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* Helpers for transform-origin *)
let origin (a : length) (b : length) : transform_origin = XY (a, b)

let origin3d (a : length) (b : length) (z : length) : transform_origin =
  XYZ (a, b, z)

let rec pp_backface_visibility : backface_visibility Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_backface_visibility ctx v
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_ray_size : ray_size Pp.t =
 fun ctx -> function
  | Closest_side -> Pp.string ctx "closest-side"
  | Closest_corner -> Pp.string ctx "closest-corner"
  | Farthest_side -> Pp.string ctx "farthest-side"
  | Farthest_corner -> Pp.string ctx "farthest-corner"
  | Sides -> Pp.string ctx "sides"

let pp_ray : ray Pp.t =
 fun ctx ({ angle; size; contain; position } : ray) ->
  pp_angle ctx angle;
  Option.iter
    (fun size ->
      Pp.space ctx ();
      pp_ray_size ctx size)
    size;
  if contain then (
    Pp.space ctx ();
    Pp.string ctx "contain");
  Option.iter
    (fun position ->
      Pp.space ctx ();
      Pp.string ctx "at";
      Pp.space ctx ();
      pp_position_value ctx position)
    position

let rec pp_offset_path : offset_path Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_offset_path ctx v
  | None -> Pp.string ctx "none"
  | Url url -> Pp.url ctx url
  | Path path -> Pp.call "path" Pp.quoted_string ctx path
  | Ray ray -> Pp.call "ray" pp_ray ctx ray
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_offset_anchor : offset_anchor Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Position position -> pp_position_value ctx position
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var var -> pp_var pp_offset_anchor ctx var

let rec pp_offset_position : offset_position Pp.t =
 fun ctx -> function
  | Normal -> Pp.string ctx "normal"
  | Auto -> Pp.string ctx "auto"
  | Position position -> pp_position_value ctx position
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var var -> pp_var pp_offset_position ctx var

let rec read_offset_anchor t : offset_anchor =
  Cursor.enum_or_calls "offset-anchor"
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_offset_anchor t)) ]
    ~default:(fun t -> (Position (read_position_value t) : offset_anchor))
    t

let rec read_offset_position t : offset_position =
  Cursor.enum_or_calls "offset-position"
    [
      ("normal", Normal);
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_offset_position t)) ]
    ~default:(fun t -> (Position (read_position_value t) : offset_position))
    t

let pp_offset_rotate_mode : offset_rotate_mode Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Reverse -> Pp.string ctx "reverse"

let rec pp_offset_rotate : offset_rotate Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Reverse -> Pp.string ctx "reverse"
  | Angle angle -> pp_angle ctx angle
  | With_angle (mode, angle) ->
      pp_offset_rotate_mode ctx mode;
      Pp.space ctx ();
      pp_angle ctx angle
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_offset_rotate ctx v

let read_offset_rotate_mode t : offset_rotate_mode =
  Cursor.enum "offset-rotate mode"
    [ ("auto", (Auto : offset_rotate_mode)); ("reverse", Reverse) ]
    t

let rec read_offset_rotate t : offset_rotate =
  let read_var t : offset_rotate = Var (read_var read_offset_rotate t) in
  let read_mode_first t : offset_rotate =
    let mode = read_offset_rotate_mode t in
    Cursor.ws t;
    match Cursor.option read_angle_unit_required t with
    | Some angle -> With_angle (mode, angle)
    | None -> (
        match mode with Auto -> (Auto : offset_rotate) | Reverse -> Reverse)
  in
  let read_angle_first t : offset_rotate =
    (* Same reading as [rotate]: the angle carries a unit. *)
    let angle = read_angle_unit_required t in
    Cursor.ws t;
    match Cursor.option read_offset_rotate_mode t with
    | Some mode -> With_angle (mode, angle)
    | None -> Angle angle
  in
  Cursor.enum_or_calls "offset-rotate"
    [
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    ~default:(fun t -> Cursor.one_of [ read_mode_first; read_angle_first ] t)
    t

(* CSS Syntax 3 (ED) sec. 4: two adjacent [<number-percentage>] tokens need a
   separator unless the boundary is unambiguous - the previous ends with [%]
   (the unit terminates the token), or the next starts with [-]/[+] (a sign
   starts a new number). Render values to strings first since
   [pp_number_percentage] picks between [<number>] and [<percentage>] spelling
   and the spacing depends on the choice. *)
let render_number_percentages ctx vs =
  List.map (Pp.to_string ~minify:(Pp.minified ctx) pp_number_percentage) vs

let pp_number_percentage_separated ctx vs =
  let needs_sep prev next =
    (not (Pp.minified ctx))
    ||
    let last = prev.[String.length prev - 1] in
    (* Tokens that end with [%] / [)] (function-shaped values like [var(...)],
       [calc(...)]) close cleanly and need no separator; numbers and
       neg-prefixed digits stay separated. *)
    last <> '%' && last <> ')' && (String.length next = 0 || next.[0] <> '-')
  in
  let rec loop = function
    | [] -> ()
    | [ s ] -> Pp.string ctx s
    | s :: (next :: _ as rest) ->
        Pp.string ctx s;
        if needs_sep s next then Pp.space ctx ();
        loop rest
  in
  loop vs

let rec pp_scale : scale Pp.t =
 fun ctx -> function
  | X n -> pp_number_percentage ctx n
  | XY (x, y) ->
      pp_number_percentage_separated ctx
        (render_number_percentages ctx [ x; y ])
  | XYZ (x, y, z) ->
      pp_number_percentage_separated ctx
        (render_number_percentages ctx [ x; y; z ])
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_scale ctx v

let rec pp_translate_value : translate_value Pp.t =
 fun ctx -> function
  | X len -> pp_length ctx len
  | XY (Var x, Var y) ->
      (* In minified mode, Tailwind omits spaces between var() values *)
      pp_length ctx (Var x);
      Pp.space_if_pretty ctx ();
      pp_length ctx (Var y)
  | XY (x, y) ->
      pp_length ctx x;
      Pp.space ctx ();
      pp_length ctx y
  | XYZ (Var x, Var y, Var z) ->
      (* In minified mode, Tailwind omits spaces between var() values *)
      pp_length ctx (Var x);
      Pp.space_if_pretty ctx ();
      pp_length ctx (Var y);
      Pp.space_if_pretty ctx ();
      pp_length ctx (Var z)
  | XYZ (x, y, z) ->
      pp_length ctx x;
      Pp.space ctx ();
      pp_length ctx y;
      Pp.space ctx ();
      pp_length ctx z
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_translate_value ctx v

let rec read_translate_value t : translate_value =
  (* Per CSS Transforms 2 sec. 5: [<length-percentage> <length-percentage>?
     <length>?]. Same two [var()] shapes as [read_scale]:

     - [translate: var(--t)] is a whole-value [var()] -- produce [Var _]. -
     [translate: var(--x) var(--y)] is per-slot -- produce [XY (_, _)]. *)
  let read_lengths_from t (x : length) : translate_value =
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some y -> (
        Cursor.ws t;
        match Cursor.option read_length t with
        | Some z -> XYZ (x, y, z)
        | None -> XY (x, y))
    | None -> X x
  in
  let read_lengths t : translate_value =
    let x = read_length t in
    read_lengths_from t x
  in
  let read_var_or_components t : translate_value =
    let snap = Cursor.save t in
    let whole_var : translate_value = Var (read_var read_translate_value t) in
    Cursor.ws t;
    if Cursor.is_done t then whole_var
    else
      let () = Cursor.restore t snap in
      let x = read_length t in
      read_lengths_from t x
  in
  Cursor.enum_or_calls "translate"
    [
      ("none", (None : translate_value));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var_or_components) ]
    ~default:read_lengths t

(* CSS Transforms 2 sec. 5: the standalone [rotate] / individual transform
   properties accept [<angle>] but reject the bare [0] from the CSS Values
   [<zero>] token shortcut (browsers don't apply the parse-time fallback). Emit
   the unit even on zero so [rotate: 0deg] survives minification. *)
let pp_required_unit_angle ctx = function
  | (Deg f | Rad f | Turn f | Grad f) as _a when f = 0. ->
      (* Round-trip stable: input [0deg] / [0rad] / etc. keeps its unit. *)
      let suffix =
        match _a with
        | Deg _ -> "deg"
        | Rad _ -> "rad"
        | Turn _ -> "turn"
        | Grad _ -> "grad"
        | _ -> "deg"
      in
      Pp.char ctx '0';
      Pp.string ctx suffix
  | a -> pp_angle ctx a

let rec pp_rotate_value : rotate_value Pp.t =
 fun ctx -> function
  | Angle a -> pp_required_unit_angle ctx a
  | X a ->
      Pp.string ctx "x";
      Pp.space ctx ();
      pp_required_unit_angle ctx a
  | Y a ->
      Pp.string ctx "y";
      Pp.space ctx ();
      pp_required_unit_angle ctx a
  | Z a ->
      Pp.string ctx "z";
      Pp.space ctx ();
      pp_required_unit_angle ctx a
  | Axis (1., 0., 0., a) when Pp.minified ctx -> pp_rotate_value ctx (X a)
  | Axis (0., 1., 0., a) when Pp.minified ctx -> pp_rotate_value ctx (Y a)
  | Axis (0., 0., 1., a) when Pp.minified ctx -> pp_rotate_value ctx (Z a)
  | Axis (x, y, z, a) ->
      (* CSS Transforms 2 sec. 5 [rotate] [<angle> <number>{3}] is shorter under
         minify when the angle leads (csso convention) and a following number
         drops the separator if it starts with a sign. The first number keeps
         it: the angle ends in its unit, so [0deg-1] is the one dimension
         [0deg-1] rather than an angle and a number. *)
      let pp_sep ctx (next : float) =
        if Pp.minified ctx && next < 0. then () else Pp.space ctx ()
      in
      pp_required_unit_angle ctx a;
      Pp.space ctx ();
      Pp.float ctx x;
      pp_sep ctx y;
      Pp.float ctx y;
      pp_sep ctx z;
      Pp.float ctx z
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_rotate_value ctx v

let read_rotate_axis prefix make t =
  Cursor.expect_string prefix t;
  Cursor.ws t;
  make (read_angle t)

let read_rotate_x t : rotate_value =
  read_rotate_axis "x" (fun a -> (X a : rotate_value)) t

let read_rotate_y t : rotate_value =
  read_rotate_axis "y" (fun a -> (Y a : rotate_value)) t

let read_rotate_z t : rotate_value =
  read_rotate_axis "z" (fun a -> (Z a : rotate_value)) t

(* Read custom axis: x y z angle. *)
let read_rotate_axis_angle t : rotate_value =
  let first = Cursor.number t in
  Cursor.ws t;
  let second = Cursor.number t in
  Cursor.ws t;
  let third = Cursor.number t in
  Cursor.ws t;
  let angle = read_angle t in
  Axis (first, second, third, angle)

let read_rotate_angle_axis_tail angle t =
  match Cursor.peek_ident t with
  | Some "x" ->
      Cursor.skip t;
      (X angle : rotate_value)
  | Some "y" ->
      Cursor.skip t;
      (Y angle : rotate_value)
  | Some "z" ->
      Cursor.skip t;
      (Z angle : rotate_value)
  | _ ->
      let first = Cursor.number t in
      Cursor.ws t;
      let second = Cursor.number t in
      Cursor.ws t;
      let third = Cursor.number t in
      (Axis (first, second, third, angle) : rotate_value)

(* CSS Transforms 2 sec. 5 [rotate] also accepts angle then axis: [<angle>
   x|y|z] or [<angle> <number>{3}]. Try angle-first after the plain forms;
   consume the angle, then look for a trailing axis. *)
let read_rotate_angle_then_axis t : rotate_value =
  (* CSS Values 4 (ED) sec. 6.2 omits the unit only for a zero length, so the
     angle here carries one; Chrome 146 refuses [rotate: 0]. *)
  let angle = read_angle_unit_required t in
  Cursor.ws t;
  if Cursor.is_done t then Angle angle else read_rotate_angle_axis_tail angle t

let rec read_rotate_value t : rotate_value =
  Cursor.enum_or_calls "rotate"
    [
      ("none", (None : rotate_value));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> (Var (read_var read_rotate_value t) : rotate_value));
        ( "calc",
          fun t -> Angle (Calc (read_calc ~result_type:`Value read_angle t)) );
      ]
    ~default:(fun t ->
      Cursor.one_of
        [
          read_rotate_x;
          read_rotate_y;
          read_rotate_z;
          read_rotate_axis_angle;
          read_rotate_angle_then_axis;
        ]
        t)
    t

let rec read_transform_style t : transform_style =
  Cursor.enum_or_var "transform-style"
    [
      ("flat", (Flat : transform_style));
      ("preserve-3d", Preserve_3d);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_transform_style t))
    t

let rec read_backface_visibility t : backface_visibility =
  Cursor.enum_or_var "backface-visibility"
    [
      ("visible", (Visible : backface_visibility));
      ("hidden", Hidden);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_backface_visibility t))
    t

let rec read_scale t : scale =
  (* CSS Transforms 2 sec. 5: [<number-percentage>{1,3}]. Two [var()] shapes to
     keep distinct: [scale: var(--s)] is a whole-value var (produce [Var _]);
     [scale: var(--x) var(--y)] is per-slot (produce [XY _]). They are
     indistinguishable until a second component appears, so peel the first
     [var()] as a whole-value [Var]; if another follows, the saved snapshot
     re-reads it as a per-slot number-percentage. *)
  let read_numbers_from t (x : number_percentage) : scale =
    match Cursor.option Values.read_number_percentage t with
    | None -> X x
    | Some y -> (
        match Cursor.option Values.read_number_percentage t with
        | None -> XY (x, y)
        | Some z -> XYZ (x, y, z))
  in
  let read_numbers t : scale =
    let x = Values.read_number_percentage t in
    read_numbers_from t x
  in
  let read_var_or_components t : scale =
    let snap = Cursor.save t in
    let whole_var : scale = Var (read_var read_scale t) in
    Cursor.ws t;
    if Cursor.is_done t then whole_var
    else
      (* Another value follows the [var()]: this was per-slot syntax, not a
         whole-value var. Rewind and re-read the first [var()] as a number-
         percentage so it ends up inside [XY] / [XYZ]. *)
      let () = Cursor.restore t snap in
      let x = Values.read_number_percentage t in
      read_numbers_from t x
  in
  Cursor.enum_or_calls "scale"
    [
      ("none", (None : scale));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var_or_components) ]
    ~default:read_numbers t

module Transform_origin = struct
  type keyword = Center | Left | Right | Top | Bottom

  let read_position t : transform_origin =
    let position =
      match read_position_value t with
      | Edge_offset_axis _ | Axis_edge_offset _ | Edge_offset_edge_offset _ ->
          err_invalid_value t "transform-origin" "edge-offset position"
      | position -> position
    in
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some z -> (
        match position with
        | XY (x, y) -> XYZ (x, y, z)
        | Single x -> XYZ (x, (Pct 50. : length), z)
        | position -> Position_z (position, z))
    | None -> (
        match position with
        | XY (x, y) -> XY (x, y)
        | Single x -> X x
        | position -> Position position)

  let read_xyz (t : Cursor.t) : transform_origin =
    let x = read_length t in
    Cursor.ws t;
    match Cursor.option read_length t with
    | Some y -> (
        Cursor.ws t;
        match Cursor.option read_length t with
        | Some z -> XYZ (x, y, z)
        | None -> XY (x, y))
    (* CSS Transforms 1 sec. 4: a single <length-percentage> sets the X origin
       and defaults Y to [center] ([50%]); it is not duplicated into Y. *)
    | None -> X x

  let read_keyword t : keyword =
    Cursor.enum "transform-origin-keyword"
      [
        ("center", Center);
        ("left", Left);
        ("right", Right);
        ("top", Top);
        ("bottom", Bottom);
      ]
      t

  let merge_keywords t (keywords : keyword list) : transform_origin =
    match keywords with
    | [ Center ] -> Center
    | [ Left ] -> Left
    | [ Right ] -> Right
    | [ Top ] -> Top
    | [ Bottom ] -> Bottom
    (* Two keyword combinations - order matters for output *)
    | [ Left; Top ] -> Left_top
    | [ Top; Left ] -> Top_left
    | [ Left; Center ] -> Left_center
    | [ Left; Bottom ] -> Left_bottom
    | [ Bottom; Left ] -> Bottom_left
    | [ Right; Top ] -> Right_top
    | [ Top; Right ] -> Top_right
    | [ Right; Center ] -> Right_center
    | [ Right; Bottom ] -> Right_bottom
    | [ Bottom; Right ] -> Bottom_right
    | [ Center; Top ] -> Center_top
    | [ Top; Center ] ->
        Center_top (* center can be horizontal, top is vertical *)
    | [ Center; Bottom ] -> Center_bottom
    | [ Bottom; Center ] ->
        Center_bottom (* center can be horizontal, bottom is vertical *)
    | [ Center; Center ] -> Center
    | _ -> err_invalid_value t "transform-origin" "invalid keyword combination"

  let read_keywords t =
    let keywords = Cursor.list ~at_least:1 ~at_most:2 read_keyword t in
    let origin = merge_keywords t keywords in
    Cursor.ws t;
    Cursor.expect_eof t;
    origin

  let read_center_center t =
    let first = Cursor.ident t in
    let second = Cursor.ident t in
    if first = "center" && second = "center" && Cursor.is_done t then
      Center_center
    else Cursor.err_invalid t "transform-origin center center"
end

let rec read_transform_origin (t : Cursor.t) : transform_origin =
  Cursor.enum_or_whole_value_var "transform-origin"
    [
      ("initial", (Initial : transform_origin));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_transform_origin t))
    ~default:(fun t ->
      Cursor.one_of
        [
          Transform_origin.read_center_center;
          Transform_origin.read_xyz;
          Transform_origin.read_keywords;
          Transform_origin.read_position;
        ]
        t)
    t

let rec read_transform_box (t : Cursor.t) : transform_box =
  Cursor.enum_or_var "transform-box"
    [
      ("content-box", (Content_box : transform_box));
      ("border-box", Border_box);
      ("fill-box", Fill_box);
      ("stroke-box", Stroke_box);
      ("view-box", View_box);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_transform_box t))
    t

let read_perspective_origin : Cursor.t -> perspective_origin =
  read_position_value

let read_ray_size t : ray_size =
  Cursor.enum "ray size"
    [
      ("closest-side", (Closest_side : ray_size));
      ("closest-corner", Closest_corner);
      ("farthest-side", Farthest_side);
      ("farthest-corner", Farthest_corner);
      ("sides", Sides);
    ]
    t

let read_offset_path_path t =
  Cursor.call "path" t @@ fun inner ->
  Cursor.ws inner;
  match Cursor.string_opt inner with
  | Some path when path <> "" ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Path path : offset_path)
  | Some _ -> Cursor.err_invalid inner "empty offset path"
  | None -> Cursor.err_expected inner "path string"

let read_offset_path_ray_contain inner =
  match Cursor.peek_ident inner with
  | Some "contain" ->
      let _ = Cursor.ident inner in
      true
  | _ -> false

let read_offset_path_ray_position inner =
  match Cursor.peek_ident inner with
  | Some "at" ->
      let _ = Cursor.ident inner in
      Cursor.ws inner;
      Some (read_position_value inner)
  | _ -> None

let read_offset_path_ray t =
  Cursor.call "ray" t @@ fun inner ->
  Cursor.ws inner;
  let angle = Values.read_angle inner in
  Cursor.ws inner;
  let size = Cursor.option read_ray_size inner in
  Cursor.ws inner;
  let contain = read_offset_path_ray_contain inner in
  Cursor.ws inner;
  let position = read_offset_path_ray_position inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  (Ray { angle; size; contain; position } : offset_path)

let read_offset_path_url_function t =
  Cursor.call "url" t @@ fun inner ->
  Cursor.ws inner;
  match Cursor.string_opt inner with
  | Some url when url <> "" ->
      Cursor.ws inner;
      Cursor.expect_eof inner;
      (Url url : offset_path)
  | Some _ -> Cursor.err_invalid inner "empty offset path url"
  | None -> Cursor.err_expected inner "url string"

let read_offset_path_url_token t =
  match Cursor.url_opt t with
  | Some url when url <> "" -> (Url url : offset_path)
  | Some _ -> Cursor.err_invalid t "empty offset path url"
  | None -> Cursor.err_expected t "offset-path"

let rec read_offset_path t : offset_path =
  Cursor.enum_or_calls "offset-path"
    [
      ("none", (None : offset_path));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("path", read_offset_path_path);
        ("ray", read_offset_path_ray);
        ("url", read_offset_path_url_function);
        ( "var",
          fun t -> (Var (Values.read_var read_offset_path t) : offset_path) );
      ]
    ~default:read_offset_path_url_token t

let read_ray t : ray =
  Cursor.call "ray" t @@ fun inner ->
  Cursor.ws inner;
  let angle = Values.read_angle inner in
  Cursor.ws inner;
  let size = Cursor.option read_ray_size inner in
  Cursor.ws inner;
  let contain =
    match Cursor.peek_ident inner with
    | Some "contain" ->
        let _ = Cursor.ident inner in
        true
    | _ -> false
  in
  Cursor.ws inner;
  let position =
    match Cursor.peek_ident inner with
    | Some "at" ->
        let _ = Cursor.ident inner in
        Cursor.ws inner;
        Some (read_position_value inner)
    | _ -> None
  in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  { angle; size; contain; position }

(* Motion Path 1 (ED) sec. 2.6: the [offset] shorthand is

   [ <'offset-position'>? [ <'offset-path'> [ <'offset-distance'> ||
   <'offset-rotate'> ]? ]? ]! [ / <'offset-anchor'> ]?

   Serialised in that order, so the [||] pair always comes back out as the
   distance then the rotation. Pure serialiser: the initial-slot drops are
   [normalize_offset]'s. *)
let pp_offset_target : offset_target Pp.t =
 fun ctx -> function
  | Position_only position -> pp_offset_position ctx position
  | With_path { position; path; distance; rotate } ->
      Option.iter
        (fun position ->
          pp_offset_position ctx position;
          Pp.token_sp ctx ())
        position;
      pp_offset_path ctx path;
      Option.iter
        (fun distance ->
          Pp.token_sp ctx ();
          pp_length_percentage ~always:true ctx distance)
        distance;
      Option.iter
        (fun rotate ->
          Pp.token_sp ctx ();
          pp_offset_rotate ctx rotate)
        rotate

let rec pp_offset : offset Pp.t =
 fun ctx -> function
  | Shorthand { target; anchor } ->
      pp_offset_target ctx target;
      Option.iter
        (fun anchor ->
          Pp.slash ctx ();
          pp_offset_anchor ctx anchor)
        anchor
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_offset ctx v

(* Whether the cursor sits on a value [read_offset_path] takes as a path rather
   than as a CSS-wide keyword or a [var()]. None of [none], [path()], [ray()]
   and a url can start an [<'offset-position'>], so a single lookahead settles
   which of the two leading slots is being read and the two readers cannot drift
   apart. *)
let starts_offset_path t =
  Cursor.lookahead
    (fun t ->
      match Cursor.option read_offset_path t with
      | Some (None | Url _ | Path _ | Ray _ : offset_path) -> true
      | Some _ | Option.None -> false)
    t

(* A [var()] in the leading group could substitute into any of its four slots,
   so it names no longhand before substitution and the shorthand refuses it; the
   declaration then keeps its authored text through the unknown-property
   fallback. The anchor stands alone behind the [/] and takes one. *)
let reject_var_slot t =
  if Cursor.looking_at_func "var" t then
    Cursor.err_invalid t "var() spans no single offset slot"

(* Sec. 2.2 writes [offset-distance] as a plain [<length-percentage>], but
   cascade's longhand reader is non-negative and an existing oracle
   ([test_declaration.ml], "offset-distance: -10%") pins that. The slot takes
   exactly what the longhand takes rather than splitting the model in two; the
   range itself is one arbitration, not this change. *)
let read_offset_slot_distance t =
  reject_var_slot t;
  Values.read_length_percentage ~allow_negative:false ~with_keywords:false t

let read_offset_slot_rotate t =
  reject_var_slot t;
  read_offset_rotate t

(* [<'offset-distance'> || <'offset-rotate'>]: either order, each at most
   once. *)
let read_offset_path_tail t =
  let distance : length_percentage option ref = ref Option.None in
  let rotate : offset_rotate option ref = ref Option.None in
  let try_slot : 'a. 'a option ref -> (Cursor.t -> 'a) -> bool =
   fun slot reader ->
    if Option.is_some !slot then false
    else (
      Cursor.ws t;
      match Cursor.option reader t with
      | Option.Some value ->
          slot := Option.Some value;
          true
      | Option.None -> false)
  in
  let rec loop () =
    if
      try_slot distance read_offset_slot_distance
      || try_slot rotate read_offset_slot_rotate
    then loop ()
  in
  loop ();
  (!distance, !rotate)

let read_offset_target t : offset_target =
  Cursor.ws t;
  reject_var_slot t;
  let position =
    if starts_offset_path t then Option.None
    else Option.Some (read_offset_position t)
  in
  Cursor.ws t;
  reject_var_slot t;
  if starts_offset_path t then
    let path = read_offset_path t in
    let distance, rotate = read_offset_path_tail t in
    With_path { position; path; distance; rotate }
  else
    match position with
    | Option.Some position -> Position_only position
    | Option.None -> Cursor.err_expected t "offset-position or offset-path"

let read_offset_body t : offset =
  let target = read_offset_target t in
  Cursor.ws t;
  let anchor =
    if Cursor.slash_opt t then (
      Cursor.ws t;
      Option.Some (read_offset_anchor t))
    else Option.None
  in
  Cursor.ws t;
  Cursor.expect_eof t;
  Shorthand { target; anchor }

let rec read_offset t : offset =
  (* The shorthand is read whole, so a failure is the whole value's; [t] has
     passed the end of it by the time one is raised. *)
  let loc = Cursor.decl_value_loc t in
  let raw = Cursor.consume_to_decl_end ~trim:true t in
  match String.lowercase_ascii (String.trim raw) with
  | "inherit" -> Inherit
  | "initial" -> Initial
  | "unset" -> Unset
  | "revert" -> Revert
  | "revert-layer" -> Revert_layer
  | _ -> (
      let is_whole_var () =
        let r = Cursor.of_string raw in
        match Values.read_var read_offset r with
        | (_ : offset var) ->
            Cursor.ws r;
            Cursor.is_done r
        | exception Cursor.Parse_error _ -> false
      in
      if is_whole_var () then
        Var (Values.read_var read_offset (Cursor.of_string raw))
      else if value_has_css_wide_mix raw then
        Cursor.err_invalid ~loc t "CSS-wide keyword mixed with other values"
      else
        try read_offset_body (Cursor.of_string raw)
        with Cursor.Parse_error _ ->
          Cursor.err_invalid ~loc t "invalid offset shorthand")

(* Sec. 2.2 gives [offset-distance] the initial value [0]. A zero percentage is
   a different computed value from a zero length, so only a zero length says
   what omitting the slot says. *)
let offset_distance_is_initial : length_percentage -> bool = function
  | Length (Pct _) | Pct _ -> false
  | Length length -> Values.length_is_zero length
  | Env _ | Var _ | Calc _ | Invalid _ -> false

let drop_offset_initial_slot (type a) ~(is_initial : a -> bool)
    (slot : a option) : a option =
  match slot with Option.Some v when is_initial v -> Option.None | _ -> slot

let normalize_offset_anchor_slot anchor =
  option_map_preserve normalize_offset_anchor anchor
  |> drop_offset_initial_slot ~is_initial:(function
    | (Auto : offset_anchor) -> true
    | _ -> false)

(* Sec. 2.6 resets all five longhands before applying the slots written
   explicitly, so a slot holding its longhand's initial says exactly what
   leaving it out says: drop it. The initials are [normal] (sec. 2.3), [none]
   (sec. 2.1), [0] (sec. 2.2), [auto] (sec. 2.5) and [auto] (sec. 2.4).

   The path is the one slot whose drop is conditional. [offset-distance] and
   [offset-rotate] are reachable only behind a path, so dropping it would leave
   a trailing [50%] to re-read as the position; and the [!] forbids emptying the
   group, so when every other slot has gone the path [none] stays behind and
   spells the whole value. *)
let normalize_offset_target ~ctx (target : offset_target) : offset_target =
  match target with
  | Position_only position -> (
      match normalize_offset_position position with
      | (Normal : offset_position) ->
          With_path
            {
              position = Option.None;
              path = (None : offset_path);
              distance = Option.None;
              rotate = Option.None;
            }
      | position -> Position_only position)
  | With_path group -> (
      let position =
        option_map_preserve normalize_offset_position group.position
        |> drop_offset_initial_slot ~is_initial:(function
          | (Normal : offset_position) -> true
          | _ -> false)
      in
      let path = normalize_offset_path group.path in
      let distance =
        option_map_preserve
          (Values.normalize_length_percentage ~ctx)
          group.distance
        |> drop_offset_initial_slot ~is_initial:offset_distance_is_initial
      in
      let rotate =
        option_map_preserve normalize_offset_rotate group.rotate
        |> drop_offset_initial_slot ~is_initial:(function
          | (Auto : offset_rotate) -> true
          | _ -> false)
      in
      match (position, path, distance, rotate) with
      | Option.Some position, (None : offset_path), Option.None, Option.None ->
          Position_only position
      | _ ->
          if
            position == group.position && path == group.path
            && distance == group.distance && rotate == group.rotate
          then target
          else With_path { position; path; distance; rotate })

let normalize_offset ~ctx (value : offset) : offset =
  match value with
  | Shorthand { target; anchor } ->
      let target' = normalize_offset_target ~ctx target in
      let anchor' = normalize_offset_anchor_slot anchor in
      if target' == target && anchor' == anchor then value
      else Shorthand { target = target'; anchor = anchor' }
  | Initial | Inherit | Unset | Revert | Revert_layer | Var _ -> value
