open Declaration
open Common
open Shorthand_box

(* Compose 2-longhand shorthands ([gap] from [row-gap] / [column-gap],
   [place-items] from [align-items] / [justify-items], etc) when both longhands
   appear contiguously with matching importance. *)
type pair_side = Row | Column

let extract_gap_side : declaration -> (pair_side * Values.length * bool) option
    = function
  | Declaration { property = Row_gap; value; important; _ } ->
      Some (Row, value, important)
  | Declaration { property = Column_gap; value; important; _ } ->
      Some (Column, value, important)
  | _ -> None

let try_compose_gap_at idx i =
  let n = Rule_index.length idx in
  if
    i + 1 >= n
    || Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    match (extract_gap_side d1, extract_gap_side d2) with
    | Some (s1, v1, imp1), Some (s2, v2, imp2)
      when imp1 = imp2 && s1 <> s2
           && (not (Values.length_has_runtime_subst v1))
           && not (Values.length_has_runtime_subst v2) ->
        let pair = [ (s1, v1); (s2, v2) ] in
        let find s = List.assoc s pair in
        Some
          (Declaration.v ~important:imp1 Gap
             (Lengths
                { row_gap = Some (find Row); column_gap = Some (find Column) }))
    | _ -> None

(* Compose [<base>-inline] / [<base>-block] from the matching [-start] / [-end]
   longhands, and the same walk over the two physical axes of an
   [overflow]-shaped family, where [Start] is the x value and [End] the y. The
   longhands carry one value of the family's own type, and [build] turns the
   ordered pair into the shorthand's payload - a [length list] for the box
   families, a [Single] / [Pair] for the border ones - or declines a pair the
   shorthand has no spelling for. *)
type axis_side = Start | End

let try_compose_axis_pair_at idx ~foldable ~extract ~build i =
  let n = Rule_index.length idx in
  if
    i + 1 >= n
    || Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    match (extract d1, extract d2) with
    | Some (s1, v1, imp1), Some (s2, v2, imp2)
      when imp1 = imp2 && s1 <> s2 && foldable v1 && foldable v2 ->
        let pair = [ (s1, v1); (s2, v2) ] in
        build ~important:imp1 ~start:(List.assoc Start pair)
          ~end_:(List.assoc End pair)
    | _ -> None

let extract_margin_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_inline_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Margin_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_margin_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Margin_block_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Margin_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Scroll Snap 1 sec. 6.1 and 6.2: the scroll-margin and scroll-padding
   logical axes take [<length>{1,2}], the shape the margin and padding axes
   take, so the same pair composition applies. *)
let extract_scroll_margin_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_margin_block_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Scroll_margin_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_scroll_margin_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_margin_inline_start; value; important; _ }
    ->
      Some (Start, value, important)
  | Declaration { property = Scroll_margin_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_scroll_padding_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_padding_block_start; value; important; _ }
    ->
      Some (Start, value, important)
  | Declaration { property = Scroll_padding_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_scroll_padding_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Scroll_padding_inline_start; value; important; _ }
    ->
      Some (Start, value, important)
  | Declaration { property = Scroll_padding_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_padding_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_inline_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Padding_inline_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_padding_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Padding_block_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Padding_block_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_inset_inline_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_inline_start; value = [ v ]; important; _ }
    ->
      Some (Start, v, important)
  | Declaration { property = Inset_inline_end; value = [ v ]; important; _ } ->
      Some (End, v, important)
  | _ -> None

let extract_inset_block_side :
    declaration -> (axis_side * Values.length * bool) option = function
  | Declaration { property = Inset_block_start; value = [ v ]; important; _ } ->
      Some (Start, v, important)
  | Declaration { property = Inset_block_end; value = [ v ]; important; _ } ->
      Some (End, v, important)
  | _ -> None

(* CSS Logical 1 sec. 4.3 and 4.4: [border-block-*] and [border-inline-*] take
   one or two values of the side longhand's own type, so the two sides compose
   the way the length axes do. *)
let extract_border_inline_width_side :
    declaration -> (axis_side * Properties.border_width * bool) option =
  function
  | Declaration { property = Border_inline_start_width; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_inline_end_width; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_block_width_side :
    declaration -> (axis_side * Properties.border_width * bool) option =
  function
  | Declaration { property = Border_block_start_width; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_block_end_width; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_inline_style_side :
    declaration -> (axis_side * Properties.border_style * bool) option =
  function
  | Declaration { property = Border_inline_start_style; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_inline_end_style; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_block_style_side :
    declaration -> (axis_side * Properties.border_style * bool) option =
  function
  | Declaration { property = Border_block_start_style; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_block_end_style; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_inline_color_side :
    declaration -> (axis_side * Values.color * bool) option = function
  | Declaration { property = Border_inline_start_color; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_inline_end_color; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_border_block_color_side :
    declaration -> (axis_side * Values.color * bool) option = function
  | Declaration { property = Border_block_start_color; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Border_block_end_color; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Overscroll 1 sec. 2.1 and CSS Sizing 4 sec. 5.1 write the x axis first
   and the y axis second, the order [overflow] uses, so the physical pair takes
   the same walk as a logical axis. *)
let extract_overscroll_side :
    declaration -> (axis_side * Properties.overscroll_behavior * bool) option =
  function
  | Declaration { property = Overscroll_behavior_x; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Overscroll_behavior_y; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_contain_intrinsic_side :
    declaration ->
    (axis_side * Properties.contain_intrinsic_longhand * bool) option = function
  | Declaration { property = Contain_intrinsic_width; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Contain_intrinsic_height; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Grid 2 sec. 8.3: [grid-row] and [grid-column] are [<grid-line> [/
   <grid-line>]?] over their own start and end longhands. A substituted line can
   stand for the whole [<start> / <end>], so only a resolved one takes part. *)
let extract_grid_row_side :
    declaration -> (axis_side * Properties.grid_line * bool) option = function
  | Declaration { property = Grid_row_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Grid_row_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

let extract_grid_column_side :
    declaration -> (axis_side * Properties.grid_line * bool) option = function
  | Declaration { property = Grid_column_start; value; important; _ } ->
      Some (Start, value, important)
  | Declaration { property = Grid_column_end; value; important; _ } ->
      Some (End, value, important)
  | _ -> None

(* CSS Align 3 sec. 5.2, 6.3 and 7.3: [place-items] / [place-content] /
   [place-self] are the [<align> <justify>] shorthands. When the two longhands
   appear contiguously with matching importance, fold them; the per-property
   printer then collapses matching pairs to a single value. *)
let try_compose_place_at idx i =
  let n = Rule_index.length idx in
  if
    i + 1 >= n
    || Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    match (d1, d2) with
    | ( Declaration { property = Align_items; value = a; important = i1; _ },
        Declaration { property = Justify_items; value = j; important = i2; _ } )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_items
             (Align_justify (a, j) : Properties.place_items))
    | ( Declaration { property = Justify_items; value = j; important = i1; _ },
        Declaration { property = Align_items; value = a; important = i2; _ } )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_items
             (Align_justify (a, j) : Properties.place_items))
    | ( Declaration { property = Align_content; value = a; important = i1; _ },
        Declaration { property = Justify_content; value = j; important = i2; _ }
      )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_content
             (Align_justify (a, j) : Properties.place_content))
    | ( Declaration { property = Justify_content; value = j; important = i1; _ },
        Declaration { property = Align_content; value = a; important = i2; _ } )
      when i1 = i2 ->
        Some
          (Declaration.v ~important:i1 Place_content
             (Align_justify (a, j) : Properties.place_content))
    | ( Declaration { property = Align_self; value = a; important = i1; _ },
        Declaration { property = Justify_self; value = j; important = i2; _ } )
      when i1 = i2 ->
        Some (Declaration.v ~important:i1 Place_self (a, j))
    | ( Declaration { property = Justify_self; value = j; important = i1; _ },
        Declaration { property = Align_self; value = a; important = i2; _ } )
      when i1 = i2 ->
        Some (Declaration.v ~important:i1 Place_self (a, j))
    | _ -> None

(* Fold an axis pair into the shorthand's own payload: a length list for the box
   families, a [Single] / [Pair] for the border ones. *)
let axis_length idx property extract i =
  let build ~important ~start ~end_ =
    let value =
      if Values.equal_length start end_ then [ start ] else [ start; end_ ]
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx ~foldable:foldable_length_strict ~extract ~build
    i

let axis_border_width ~ctx idx property extract i =
  let build ~important ~start ~end_ =
    let value : Properties.logical_border_width =
      if same_border_width start end_ then Single start else Pair (start, end_)
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx
    ~foldable:(foldable_border_width ~ctx)
    ~extract ~build i

let axis_border_style ~ctx idx property extract i =
  let build ~important ~start ~end_ =
    let value : Properties.logical_border_style =
      if same_border_style start end_ then Single start else Pair (start, end_)
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx
    ~foldable:(foldable_border_style ~ctx)
    ~extract ~build i

let axis_overscroll idx i =
  let build ~important ~start ~end_ =
    let value =
      if Properties.equal_overscroll_behavior start end_ then [ start ]
      else [ start; end_ ]
    in
    Some (Declaration.v ~important Overscroll_behavior value)
  in
  let foldable : Properties.overscroll_behavior -> bool = function
    | Var _ -> false
    | _ -> true
  in
  try_compose_axis_pair_at idx ~foldable ~extract:extract_overscroll_side ~build
    i

(* [contain-intrinsic-size] says one axis at [none] only by saying it for both,
   so a [none] beside a sized axis has no shorthand spelling. *)
let axis_contain_intrinsic idx i =
  let build ~important ~start ~end_ =
    let value : Properties.contain_intrinsic_size option =
      match
        ( (start : Properties.contain_intrinsic_longhand),
          (end_ : Properties.contain_intrinsic_longhand) )
      with
      | None, None -> Some None
      | Size a, Size b ->
          if Properties.equal_contain_intrinsic_size_item a b then
            Some (Intrinsic (a, Option.None))
          else Some (Intrinsic (a, Some b))
      | _ -> Option.None
    in
    Option.map (Declaration.v ~important Contain_intrinsic_size) value
  in
  let foldable : Properties.contain_intrinsic_longhand -> bool = function
    | Var _ -> false
    | _ -> true
  in
  try_compose_axis_pair_at idx ~foldable ~extract:extract_contain_intrinsic_side
    ~build i

let axis_grid_line idx property extract i =
  let build ~important ~start ~end_ =
    Some
      (Declaration.v ~important property
         (Lines (start, end_) : Properties.grid_line_pair))
  in
  try_compose_axis_pair_at idx ~foldable:foldable_grid_line ~extract ~build i

let axis_border_color ~ctx idx property extract i =
  let build ~important ~start ~end_ =
    let value : Properties.logical_border_color =
      if Values.equal_color start end_ then Single start else Pair (start, end_)
    in
    Some (Declaration.v ~important property value)
  in
  try_compose_axis_pair_at idx
    ~foldable:(foldable_border_color ~ctx)
    ~extract ~build i

(* CSS Flexbox 1 sec. 5.1 and CSS Text Decoration 4 sec. 3.4: [flex-flow] and
   [text-emphasis] each take two longhands, one per component. A longhand
   written [initial] fills its slot in the run and contributes no value, the
   normalisers then dropping any component that names its own initial. A
   substituted longhand can stand for the whole value and is left alone. *)
let extract_flex_flow_part :
    declaration ->
    (axis_side
    * (Properties.flex_direction option * Properties.flex_wrap option)
    * bool)
    option = function
  | Declaration { property = Flex_direction; value = Var _; _ }
  | Declaration { property = Flex_wrap; value = Var _; _ } ->
      None
  | Declaration { property = Flex_direction; value; important; _ } ->
      let d = match value with Initial -> Option.None | v -> Some v in
      Some (Start, (d, Option.None), important)
  | Declaration { property = Flex_wrap; value; important; _ } ->
      let w = match value with Initial -> Option.None | v -> Some v in
      Some (End, (Option.None, w), important)
  | _ -> None

let extract_text_emphasis_part :
    declaration ->
    (axis_side
    * (Properties.text_emphasis_style option * Values.color option)
    * bool)
    option = function
  | Declaration { property = Text_emphasis_style; value = Var _; _ }
  | Declaration { property = Text_emphasis_color; value = Var _; _ } ->
      None
  | Declaration { property = Text_emphasis_style; value; important; _ } ->
      let st = match value with Initial -> Option.None | v -> Some v in
      Some (Start, (st, Option.None), important)
  | Declaration { property = Text_emphasis_color; value; important; _ } ->
      let c = match value with Initial -> Option.None | v -> Some v in
      Some (End, (Option.None, c), important)
  | _ -> None

let duo_flex_flow idx i =
  let build ~important ~start ~end_ =
    let direction, _ = start and _, wrap = end_ in
    Some
      (Declaration.v ~important Flex_flow
         (Flow (direction, wrap) : Properties.flex_flow))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_flex_flow_part ~build i

let duo_text_emphasis idx i =
  let build ~important ~start ~end_ =
    let style, _ = start and _, color = end_ in
    Some
      (Declaration.v ~important Text_emphasis
         (Emphasis (style, color) : Properties.text_emphasis))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_text_emphasis_part ~build i

(* CSS Animations 2 sec. 6.3 and CSS Scroll Animations 1 sec. 4.3:
   [animation-range] is [<start> <end>?] and [scroll-timeline] is [<name>
   <axis>?], each written over its own two longhands. *)
let extract_animation_range_part :
    declaration ->
    (axis_side
    * (Properties.animation_range_item option
      * Properties.animation_range_item option)
    * bool)
    option = function
  | Declaration { property = Animation_range_start; value = Var _; _ }
  | Declaration { property = Animation_range_end; value = Var _; _ } ->
      None
  | Declaration { property = Animation_range_start; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Animation_range_end; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let extract_scroll_timeline_part :
    declaration ->
    (axis_side
    * (Properties.timeline_name option * Properties.timeline_axis option)
    * bool)
    option = function
  | Declaration { property = Scroll_timeline_name; value = Var _; _ }
  | Declaration { property = Scroll_timeline_axis; value = Var _; _ } ->
      None
  | Declaration { property = Scroll_timeline_name; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Scroll_timeline_axis; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let duo_animation_range idx i =
  let build ~important ~start ~end_ =
    let range_start, _ = start and _, range_end = end_ in
    match range_start with
    | Option.None -> Option.None
    | Some range_start ->
        Some
          (Declaration.v ~important Animation_range
             (Range (range_start, range_end) : Properties.animation_range))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_animation_range_part ~build i

(* [scroll-timeline: none] leaves the axis at [block], so a named axis beside an
   unnamed timeline has no shorthand spelling, and neither has a name list: the
   shorthand pairs each name with its own axis. *)
let duo_scroll_timeline idx i =
  let build ~important ~start ~end_ =
    let name, _ = start and _, axis = end_ in
    let axis =
      match axis with
      | Some (Block : Properties.timeline_axis) | Some Initial -> Option.None
      | axis -> axis
    in
    let value : Properties.timeline_shorthand option =
      match (name : Properties.timeline_name option) with
      | Some (Names [ n ]) -> Some (Timelines [ { name = n; axis } ])
      | _ -> Option.None
    in
    Option.map (Declaration.v ~important Scroll_timeline) value
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_scroll_timeline_part ~build i

(* CSS Contain 3 sec. 4.3: [container] is [<name> [/ <type>]?] over its two
   longhands, and [normal] is the type's initial (sec. 4.2), so writing it out
   names what leaving it out names. The shorthand pairs one name with one type,
   so a name list has no spelling in it. *)
let extract_container_part :
    declaration ->
    (axis_side
    * (Properties.container_name option * Properties.container_type option)
    * bool)
    option = function
  | Declaration { property = Container_name; value = Var _; _ }
  | Declaration { property = Container_type; value = Var _; _ } ->
      None
  | Declaration { property = Container_name; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Container_type; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let duo_container idx i =
  let build ~important ~start ~end_ =
    let name, _ = start and _, ctype = end_ in
    let ctype =
      match ctype with
      | Some (Normal : Properties.container_type) | Some Initial -> Option.None
      | ctype -> ctype
    in
    let value : Properties.container_shorthand option =
      match (name : Properties.container_name option) with
      | Some ((None | Names _) as name) -> Some (Shorthand { name; ctype })
      | _ -> Option.None
    in
    Option.map (Declaration.v ~important Container) value
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_container_part ~build i

(* CSS Backgrounds 4 sec. 3.3: [background-position] is [<x> <y>] over the two
   axis longhands, [Start] here the x axis. The shorthand carries one position
   per layer, so a single-valued pair is what composes; the four-value edge form
   needs each axis to name its own edge. *)
let extract_webkit_mask_position_part :
    declaration ->
    (axis_side
    * (Properties.background_position_axis option
      * Properties.background_position_axis option)
    * bool)
    option = function
  | Declaration { property = Webkit_mask_position_x; value = Var _; _ }
  | Declaration { property = Webkit_mask_position_y; value = Var _; _ } ->
      None
  | Declaration { property = Webkit_mask_position_x; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Webkit_mask_position_y; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let extract_background_position_part :
    declaration ->
    (axis_side
    * (Properties.background_position_axis option
      * Properties.background_position_axis option)
    * bool)
    option = function
  | Declaration { property = Background_position_x; value = Var _; _ }
  | Declaration { property = Background_position_y; value = Var _; _ } ->
      None
  | Declaration { property = Background_position_x; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Background_position_y; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let position_of_axes (x : Properties.background_position_axis)
    (y : Properties.background_position_axis) : Properties.position_value option
    =
  let axis_length : Properties.background_position_axis -> Values.length option
      = function
    (* CSS Backgrounds 3 sec. 3.6 starts the position at [0% 0%], so [initial]
       on an axis names the zero offset. *)
    | Initial -> Some (Pct 0.)
    | Center -> Some (Pct 50.)
    | Edge Left | Edge Top -> Some Zero
    | Edge Right | Edge Bottom -> Some (Pct 100.)
    | Offset (Length l) -> Some l
    | Offset (Pct p) -> Some (Pct p)
    | _ -> Option.None
  in
  let edge_name : Properties.position_axis_edge -> string = function
    | Left -> "left"
    | Right -> "right"
    | Top -> "top"
    | Bottom -> "bottom"
  in
  match (axis_length x, axis_length y) with
  | Some lx, Some ly -> Some (Prop_image.position_of_xy lx ly)
  | _ -> (
      match (x, y) with
      | Edge_offset (ex, ox), Edge_offset (ey, oy) ->
          Some (Edge_offset_edge_offset (edge_name ex, ox, edge_name ey, oy))
      | _ -> Option.None)

let duo_position_axes idx property extract i =
  let build ~important ~start ~end_ =
    let x, _ = start and _, y = end_ in
    match (x, y) with
    | Some x, Some y ->
        Option.map
          (fun p -> Declaration.v ~important property [ p ])
          (position_of_axes x y)
    | _ -> Option.None
  in
  try_compose_axis_pair_at idx ~foldable:(fun _ -> true) ~extract ~build i

(* CSS Text 4 sec. 3: [white-space] is a shorthand of [white-space-collapse] and
   [text-wrap-mode], and sec. 3 table 1 gives the four pairs a single keyword
   names. A pair outside that table has no one-keyword spelling and stays two
   declarations. *)
let extract_white_space_part :
    declaration ->
    (axis_side
    * (Properties.white_space_collapse option
      * Properties.text_wrap_mode option)
    * bool)
    option = function
  | Declaration { property = White_space_collapse; value = Var _; _ }
  | Declaration { property = Text_wrap_mode; value = Var _; _ } ->
      None
  | Declaration { property = White_space_collapse; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Text_wrap_mode; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let white_space_of_pair (c : Properties.white_space_collapse)
    (m : Properties.text_wrap_mode) : Properties.white_space option =
  match (c, m) with
  | Collapse, Wrap -> Some Normal
  | Collapse, No_wrap -> Some Nowrap
  | Preserve, Wrap -> Some Pre_wrap
  | Preserve, No_wrap -> Some Pre
  | Preserve_breaks, Wrap -> Some Pre_line
  | Break_spaces, Wrap -> Some Break_spaces
  | _ -> Option.None

let duo_white_space idx i =
  let build ~important ~start ~end_ =
    let collapse, _ = start and _, mode = end_ in
    match (collapse, mode) with
    | Some c, Some m ->
        Option.map
          (Declaration.v ~important White_space)
          (white_space_of_pair c m)
    | _ -> Option.None
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_white_space_part ~build i

(* CSS Text 4 sec. 5.1: [text-wrap] is [<'text-wrap-mode'> ||
   <'text-wrap-style'>] over its two longhands, with [wrap] the mode's initial
   (sec. 5.2) and [auto] the style's (sec. 5.3), so a longhand at its initial
   names what leaving the component out names. *)
let extract_text_wrap_part :
    declaration ->
    (axis_side
    * (Properties.text_wrap_mode option * Properties.text_wrap_style option)
    * bool)
    option = function
  | Declaration { property = Text_wrap_mode; value = Var _; _ }
  | Declaration { property = Text_wrap_style; value = Var _; _ } ->
      None
  | Declaration { property = Text_wrap_mode; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Text_wrap_style; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let text_wrap_of_pair (m : Properties.text_wrap_mode)
    (st : Properties.text_wrap_style) : Properties.text_wrap option =
  let mode =
    match m with
    | Wrap | Initial -> Some `Wrap
    | No_wrap -> Some `No_wrap
    | _ -> Option.None
  in
  let style =
    match st with
    | Auto | Initial -> Some `Auto
    | Balance -> Some `Balance
    | Stable -> Some `Stable
    | Pretty -> Some `Pretty
    | _ -> Option.None
  in
  match (mode, style) with
  | Some `Wrap, Some `Auto -> Some (Wrap : Properties.text_wrap)
  | Some `No_wrap, Some `Auto -> Some No_wrap
  | Some `Wrap, Some `Balance -> Some Balance
  | Some `Wrap, Some `Stable -> Some Stable
  | Some `Wrap, Some `Pretty -> Some Pretty
  | Some m, Some st -> Some (Mode_style (m, st))
  | _ -> Option.None

let duo_text_wrap idx i =
  let build ~important ~start ~end_ =
    let mode, _ = start and _, style = end_ in
    match (mode, style) with
    | Some m, Some st ->
        Option.map (Declaration.v ~important Text_wrap) (text_wrap_of_pair m st)
    | _ -> Option.None
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_text_wrap_part ~build i

(* [-webkit-text-stroke] is [<line-width> || <color>] over its two longhands.
   Both are typed differently, so either order names the same declaration. *)
let extract_webkit_stroke_part :
    declaration ->
    (axis_side * (Properties.border_width option * Values.color option) * bool)
    option = function
  | Declaration { property = Webkit_text_stroke_width; value = Var _; _ }
  | Declaration { property = Webkit_text_stroke_color; value = Var _; _ } ->
      None
  | Declaration { property = Webkit_text_stroke_width; value; important; _ } ->
      Some (Start, (Some value, Option.None), important)
  | Declaration { property = Webkit_text_stroke_color; value; important; _ } ->
      Some (End, (Option.None, Some value), important)
  | _ -> None

let duo_webkit_text_stroke idx i =
  let build ~important ~start ~end_ =
    let width, _ = start and _, color = end_ in
    Some
      (Declaration.v ~important Webkit_text_stroke
         ({ width; color } : Properties.webkit_text_stroke))
  in
  try_compose_axis_pair_at idx
    ~foldable:(fun _ -> true)
    ~extract:extract_webkit_stroke_part ~build i

(* One entry per logical axis family; each pairs a start longhand with its end
   longhand under the shorthand that names the axis. *)
let pair_axes ~ctx idx i =
  let axis = axis_length idx in
  let width = axis_border_width ~ctx idx in
  let style = axis_border_style ~ctx idx in
  let color = axis_border_color ~ctx idx in
  [
    (fun () -> axis Margin_inline extract_margin_inline_side i);
    (fun () -> axis Margin_block extract_margin_block_side i);
    (fun () -> axis Padding_inline extract_padding_inline_side i);
    (fun () -> axis Padding_block extract_padding_block_side i);
    (fun () -> axis Inset_inline extract_inset_inline_side i);
    (fun () -> axis Inset_block extract_inset_block_side i);
    (fun () -> axis Scroll_margin_inline extract_scroll_margin_inline_side i);
    (fun () -> axis Scroll_margin_block extract_scroll_margin_block_side i);
    (fun () -> axis Scroll_padding_inline extract_scroll_padding_inline_side i);
    (fun () -> axis Scroll_padding_block extract_scroll_padding_block_side i);
    (fun () -> width Border_inline_width extract_border_inline_width_side i);
    (fun () -> width Border_block_width extract_border_block_width_side i);
    (fun () -> style Border_inline_style extract_border_inline_style_side i);
    (fun () -> style Border_block_style extract_border_block_style_side i);
    (fun () -> color Border_inline_color extract_border_inline_color_side i);
    (fun () -> color Border_block_color extract_border_block_color_side i);
    (fun () -> axis_overscroll idx i);
    (fun () -> axis_contain_intrinsic idx i);
    (fun () -> axis_grid_line idx Grid_row extract_grid_row_side i);
    (fun () -> axis_grid_line idx Grid_column extract_grid_column_side i);
    (fun () -> duo_flex_flow idx i);
    (fun () -> duo_text_emphasis idx i);
    (fun () -> duo_animation_range idx i);
    (fun () -> duo_scroll_timeline idx i);
    (fun () -> duo_container idx i);
    (fun () ->
      duo_position_axes idx Background_position extract_background_position_part
        i);
    (fun () ->
      duo_position_axes idx Webkit_mask_position
        extract_webkit_mask_position_part i);
    (fun () -> duo_white_space idx i);
    (fun () -> duo_text_wrap idx i);
    (fun () -> duo_webkit_text_stroke idx i);
  ]

let compose_pair_via_index ~ctx idx =
  let try_any i =
    match try_compose_gap_at idx i with
    | Some _ as r -> r
    | None -> (
        match List.find_map (fun f -> f ()) (pair_axes ~ctx idx i) with
        | Some _ as r -> r
        | None -> try_compose_place_at idx i)
  in
  let n = Rule_index.length idx in
  let i = ref 0 in
  while !i < n do
    match try_any !i with
    | Some shorthand ->
        if Rule_index.absorb idx ~at:!i ~absorbed:[ !i; !i + 1 ] ~shorthand then
          i := !i + 2
        else incr i
    | None -> incr i
  done
