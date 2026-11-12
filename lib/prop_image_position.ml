(* CSS Values 4 sec. 8.3 and CSS Backgrounds 3 sec. 3.6: the <position> value
   shared by gradients, backgrounds, masks and the transform and offset
   families. *)

open Values
open Properties_intf
open Prop_common

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

let position_value_is_center : position_value -> bool = function
  | Center -> true
  | XY (Pct 50., Pct 50.) -> true
  | Single (Pct 50.) -> true
  | _ -> false

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
    let edge1 = Cursor.ident ~keep_case:false t in
    Cursor.ws t;
    let offset = read_length_percentage ~with_keywords:false t in
    Cursor.ws t;
    let axis = Cursor.ident ~keep_case:false t in
    if valid_edge_axis edge1 axis then Edge_offset_axis (edge1, offset, axis)
    else Cursor.err_invalid t "invalid three-value position"

  let read_axis_edge_offset t : position_value =
    let axis = Cursor.ident ~keep_case:false t in
    Cursor.ws t;
    let edge = Cursor.ident ~keep_case:false t in
    Cursor.ws t;
    let offset = read_length_percentage ~with_keywords:false t in
    if valid_edge_axis edge axis then Axis_edge_offset (axis, edge, offset)
    else Cursor.err_invalid t "invalid position axis edge offset"

  let read_horizontal_keyword_length t : position_value =
    let keyword = Cursor.ident ~keep_case:false t in
    Cursor.ws t;
    let y = read_length ~with_keywords:false t in
    match keyword with
    | "left" -> XY ((Pct 0. : length), y)
    | "right" -> XY ((Pct 100. : length), y)
    | "center" -> XY ((Pct 50. : length), y)
    | _ -> Cursor.err_invalid t "invalid horizontal position keyword length"

  (* CSS Values 4 (ED) sec. 8.3 spells one alternative [[ left | center | right
     | <length-percentage> ] [ top | center | bottom | <length-percentage> ]],
     so the second slot takes any vertical edge and not only [center]. The
     reverse order is no alternative of the production, so a keyword there is
     still refused. *)
  let read_length_keyword t : position_value =
    let offset = read_length ~with_keywords:false t in
    Cursor.ws t;
    match Cursor.ident ~keep_case:false t with
    | "center" -> Single offset
    | "top" -> XY (offset, (Pct 0. : length))
    | "bottom" -> XY (offset, (Pct 100. : length))
    | _ -> Cursor.err_invalid t "invalid position length keyword"

  (* Read 4-value syntax: keyword offset keyword offset *)
  let read_4_value t : position_value =
    let edge1 = Cursor.ident ~keep_case:false t in
    Cursor.ws t;
    let offset1 = read_length_percentage ~with_keywords:false t in
    Cursor.ws t;
    let edge2 = Cursor.ident ~keep_case:false t in
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
