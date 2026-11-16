open Declaration
open Common
open Shorthand_common

(* CSS Flexbox 1 sec. 7.2: [flex] shorthand is grow / shrink / basis. Cascade
   types [Flex] as [Full of grow * shrink * basis]; the composition extracts the
   three typed longhands and builds the constructor. *)
type flex_kind = FGrow | FShrink | FBasis

let flex_kind_of : declaration -> flex_kind option = function
  | Declaration { property = Flex_grow; _ } -> Some FGrow
  | Declaration { property = Flex_shrink; _ } -> Some FShrink
  | Declaration { property = Flex_basis; _ } -> Some FBasis
  | _ -> None

let flex_grow_of : declaration -> Properties.flex_factor option = function
  | Declaration { property = Flex_grow; value = (Number _ | Var _) as v; _ } ->
      Some v
  | _ -> None

let flex_shrink_of : declaration -> Properties.flex_factor option = function
  | Declaration { property = Flex_shrink; value = (Number _ | Var _) as v; _ }
    ->
      Some v
  | _ -> None

let flex_basis_of : declaration -> Properties.flex_basis option = function
  | Declaration { property = Flex_basis; value; _ } -> Some value
  | _ -> None

let try_compose_flex_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else if
    Rule_index.is_absorbed idx i
    || Rule_index.is_absorbed idx (i + 1)
    || Rule_index.is_absorbed idx (i + 2)
  then None
  else
    let d1 = Rule_index.decl_at idx i in
    let d2 = Rule_index.decl_at idx (i + 1) in
    let d3 = Rule_index.decl_at idx (i + 2) in
    match (flex_kind_of d1, flex_kind_of d2, flex_kind_of d3) with
    | Some k1, Some k2, Some k3
      when is_important d1 = is_important d2
           && is_important d2 = is_important d3
           && List.length (List.sort_uniq compare [ k1; k2; k3 ]) = 3 -> (
        let triple = [ d1; d2; d3 ] in
        let grow = List.find_map flex_grow_of triple in
        let shrink = List.find_map flex_shrink_of triple in
        let basis = List.find_map flex_basis_of triple in
        match (grow, shrink, basis) with
        | Some g, Some s, Some b ->
            Some
              (Declaration.v ~important:(is_important d1) Flex (Full (g, s, b)))
        | _ -> None)
    | _ -> None

let compose_flex_via_index idx =
  compose_fixed3_via_index idx ~try_compose:try_compose_flex_at

(* Motion Path 1 sec. 2.6: [offset] is [<position>? [<path> [<distance> ||
   <rotate>]?]? [/ <anchor>]?] over its five longhands, and it resets every one
   of them, so the run that names the same declaration is all five. A component
   at its initial - [normal] for the position (sec. 2.1), [0] for the distance
   (sec. 2.3), [auto] for the rotate (sec. 2.4) and for the anchor (sec. 2.5) -
   is what leaving it out names. *)
type offset_part = Position | Path | Distance | Rotate | Anchor

let offset_part_of : declaration -> offset_part option = function
  | Declaration { property = Offset_position; _ } -> Some Position
  | Declaration { property = Offset_path; _ } -> Some Path
  | Declaration { property = Offset_distance; _ } -> Some Distance
  | Declaration { property = Offset_rotate; _ } -> Some Rotate
  | Declaration { property = Offset_anchor; _ } -> Some Anchor
  | _ -> None

let offset_position_of : declaration -> Properties.offset_position option =
  function
  | Declaration { property = Offset_position; value = Normal | Initial; _ } ->
      Option.None
  | Declaration { property = Offset_position; value; _ } -> Some value
  | _ -> Option.None

let offset_path_of : declaration -> Properties.offset_path option = function
  | Declaration { property = Offset_path; value; _ } -> Some value
  | _ -> Option.None

let offset_distance_of : declaration -> Values.length_percentage option =
  function
  | Declaration { property = Offset_distance; value = Length Zero; _ } ->
      Option.None
  | Declaration { property = Offset_distance; value; _ } -> Some value
  | _ -> Option.None

let offset_rotate_of : declaration -> Properties.offset_rotate option = function
  | Declaration { property = Offset_rotate; value = Auto | Initial; _ } ->
      Option.None
  | Declaration { property = Offset_rotate; value; _ } -> Some value
  | _ -> Option.None

let offset_anchor_of : declaration -> Properties.offset_anchor option = function
  | Declaration { property = Offset_anchor; value = Auto | Initial; _ } ->
      Option.None
  | Declaration { property = Offset_anchor; value; _ } -> Some value
  | _ -> Option.None

let offset_of_run raw : Properties.offset option =
  let parts = List.filter_map offset_part_of raw in
  if List.length (List.sort_uniq compare parts) <> 5 then Option.None
  else
    let position = List.find_map offset_position_of raw in
    let distance = List.find_map offset_distance_of raw in
    let rotate = List.find_map offset_rotate_of raw in
    let anchor = List.find_map offset_anchor_of raw in
    match List.find_map offset_path_of raw with
    | Some path ->
        Some
          (Shorthand
             { target = With_path { position; path; distance; rotate }; anchor })
    | Option.None -> Option.None

let try_compose_offset_at idx i =
  let n = Rule_index.length idx in
  if i + 4 >= n then None
  else
    let positions = List.init 5 (fun j -> i + j) in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      if (not (same_importance raw)) || List.exists has_runtime_substitution raw
      then None
      else
        Option.map
          (fun v ->
            (Declaration.v ~important:(is_important (List.hd raw)) Offset v, 5))
          (offset_of_run raw)

let compose_offset_via_index idx =
  compose_run_via_index idx ~try_compose:try_compose_offset_at

(* CSS Multicol 2 sec. 4.5: [columns] sets the width, the count and the height,
   and Chrome 146 has it reset [column-wrap] too, so the run naming the same
   declaration is all four. The shorthand has no spelling for a height other
   than its [auto] initial, so a run carrying one keeps its longhands. *)
type columns_part = Width | Count | Height | Wrap

let columns_part_of : declaration -> columns_part option = function
  | Declaration { property = Column_width; _ } -> Some Width
  | Declaration { property = Column_count; _ } -> Some Count
  | Declaration { property = Column_height; _ } -> Some Height
  | Declaration { property = Column_wrap; _ } -> Some Wrap
  | _ -> None

let columns_initial_tail : declaration -> bool = function
  | Declaration { property = Column_height; value; _ } -> (
      match (value : Properties.column_height) with
      | Auto | Initial -> true
      | _ -> false)
  | Declaration { property = Column_wrap; value; _ } -> (
      match (value : Properties.column_wrap) with
      | Auto | Initial -> true
      | _ -> false)
  | _ -> true

let columns_width_of : declaration -> Properties.column_width option = function
  | Declaration { property = Column_width; value; _ } -> Some value
  | _ -> Option.None

let columns_count_of : declaration -> Properties.column_count option = function
  | Declaration { property = Column_count; value; _ } -> Some value
  | _ -> Option.None

let columns_of_run raw : Properties.columns_value option =
  let width = List.find_map columns_width_of raw in
  let count = List.find_map columns_count_of raw in
  match (width, count) with
  | Some Auto, Some Auto -> Some (Auto : Properties.columns_value)
  | Some Auto, Some (Count n) -> Some (Auto_count n)
  | Some (Width w), Some Auto -> Some (Width w)
  | Some (Width w), Some (Count n) -> Some (Both (w, n))
  | _ -> Option.None

let try_compose_columns_at idx i =
  let n = Rule_index.length idx in
  if i + 3 >= n then None
  else
    let positions = List.init 4 (fun j -> i + j) in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      let parts = List.filter_map columns_part_of raw in
      if
        (not (same_importance raw))
        || List.length (List.sort_uniq compare parts) <> 4
        || not (List.for_all columns_initial_tail raw)
      then None
      else
        Option.map
          (Declaration.v ~important:(is_important (List.hd raw)) Columns)
          (columns_of_run raw)

let compose_columns_via_index idx =
  compose_run_via_index idx ~try_compose:(fun idx i ->
      Option.map (fun d -> (d, 4)) (try_compose_columns_at idx i))

(* CSS Grid 2 (ED) sec. 7.4: [grid-template] is [none | [<'grid-template-rows'>
   / <'grid-template-columns'>] | [<line-names>? <string> <track-size>?
   <line-names>?]+ [/ <explicit-track-list>]?], and it resets all three
   longhands, so the run naming the same declaration is all three. The areas
   form writes each row's string beside that row's size, so composing it needs
   one track per area row. *)
type gt_kind = Rows | Columns | Areas

let gt_kind_of : declaration -> gt_kind option = function
  | Declaration { property = Grid_template_rows; _ } -> Some Rows
  | Declaration { property = Grid_template_columns; _ } -> Some Columns
  | Declaration { property = Grid_template_areas; _ } -> Some Areas
  | _ -> None

(* A track list the shorthand can write out: a CSS-wide keyword is the whole
   declaration value, and the slash and raw-template forms belong to the
   shorthand itself, not to a longhand it reads. *)
let rec gt_track_is_plain : Properties.grid_template -> bool = function
  | Inherit | Initial | Unset | Revert | Revert_layer | Split _ | Template _
  | Auto_flow_columns _ | Auto_flow_rows _ ->
      false
  | Tracks tracks -> List.for_all gt_track_is_plain tracks
  | _ -> true

let gt_tracks_of : Properties.grid_template -> Properties.grid_template list =
  function
  | Tracks tracks -> tracks
  | track -> [ track ]

let gt_track_text (track : Properties.grid_template) =
  Declaration.string_of_value ~minify:true
    (Declaration.v Grid_template_rows track)

(* The rendered rows of an [Areas] value, quotes included. A cell name carries
   no quote of its own, so the delimiters alternate. *)
let gt_area_rows text =
  let rec loop i acc =
    match String.index_from_opt text i '"' with
    | Option.None -> List.rev acc
    | Some start -> (
        match String.index_from_opt text (start + 1) '"' with
        | Option.None -> List.rev acc
        | Some stop ->
            loop (stop + 1) (String.sub text start (stop - start + 1) :: acc))
  in
  loop 0 []

let gt_shorthand_text ~rows ~columns ~areas =
  let buf = Buffer.create 64 in
  let add_track track =
    if Buffer.length buf > 0 then Buffer.add_char buf ' ';
    Buffer.add_string buf (gt_track_text track)
  in
  let ok =
    match (areas : Properties.grid_template_areas) with
    | No_areas ->
        List.iter add_track (gt_tracks_of rows);
        true
    | Areas text ->
        let area_rows = gt_area_rows text in
        let tracks = gt_tracks_of rows in
        List.length area_rows = List.length tracks
        && area_rows <> []
        &&
        (List.iter2
           (fun row track ->
             if Buffer.length buf > 0 then Buffer.add_char buf ' ';
             Buffer.add_string buf row;
             Buffer.add_char buf ' ';
             Buffer.add_string buf (gt_track_text track))
           area_rows tracks;
         true)
    | Inherit | Initial | Unset | Revert | Revert_layer | Var _ -> false
  in
  if not ok then Option.None
  else (
    Buffer.add_char buf '/';
    List.iter
      (fun track ->
        Buffer.add_string buf (gt_track_text track);
        Buffer.add_char buf ' ')
      (gt_tracks_of columns);
    Some (String.trim (Buffer.contents buf)))

let gt_rows_of : declaration -> Properties.grid_template option = function
  | Declaration { property = Grid_template_rows; value; _ } -> Some value
  | _ -> None

let gt_columns_of : declaration -> Properties.grid_template option = function
  | Declaration { property = Grid_template_columns; value; _ } -> Some value
  | _ -> None

let gt_areas_of : declaration -> Properties.grid_template_areas option =
  function
  | Declaration { property = Grid_template_areas; value; _ } -> Some value
  | _ -> None

let grid_template_of_run raw : Properties.grid_template option =
  let rows = List.find_map gt_rows_of raw in
  let columns = List.find_map gt_columns_of raw in
  let areas = List.find_map gt_areas_of raw in
  match (rows, columns, areas) with
  | Some rows, Some columns, Some areas
    when gt_track_is_plain rows && gt_track_is_plain columns -> (
      match (rows, columns, areas) with
      | None, None, No_areas -> Some (None : Properties.grid_template)
      | _ -> (
          match gt_shorthand_text ~rows ~columns ~areas with
          | Option.None -> Option.None
          | Some text -> (
              let t = Cursor.of_string text in
              match Properties.read_grid_template t with
              | value when Cursor.is_done t -> Some value
              | _ -> Option.None
              | exception Cursor.Parse_error _ -> Option.None)))
  | _ -> Option.None

let try_compose_grid_template_at idx i =
  let n = Rule_index.length idx in
  if i + 2 >= n then None
  else
    let positions = List.init 3 (fun j -> i + j) in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      let kinds = List.filter_map gt_kind_of raw in
      if
        (not (same_importance raw))
        || List.length (List.sort_uniq compare kinds) <> 3
      then None
      else
        Option.map
          (Declaration.v ~important:(is_important (List.hd raw)) Grid_template)
          (grid_template_of_run raw)

let compose_grid_template_via_index idx =
  compose_run_via_index idx ~try_compose:(fun idx i ->
      Option.map (fun d -> (d, 3)) (try_compose_grid_template_at idx i))

(* CSS Grid 2 (ED) sec. 7.8: [grid] is [<'grid-template'> |
   <'grid-template-rows'> / [auto-flow && dense?] <'grid-auto-columns'>? |
   [auto-flow && dense?] <'grid-auto-rows'>? / <'grid-template-columns'>], and
   it resets all six longhands. Only one of the two auto-flow axes can be
   written, and the other three longhands have to be at the initials the
   shorthand leaves them. *)
let gd_auto_flow_of : declaration -> Properties.grid_auto_flow option = function
  | Declaration { property = Grid_auto_flow; value; _ } -> Some value
  | _ -> None

let gd_auto_rows_of : declaration -> Properties.grid_template option = function
  | Declaration { property = Grid_auto_rows; value; _ } -> Some value
  | _ -> None

let gd_auto_columns_of : declaration -> Properties.grid_template option =
  function
  | Declaration { property = Grid_auto_columns; value; _ } -> Some value
  | _ -> None

type gd_kind =
  | Template_rows
  | Template_columns
  | Template_areas
  | Flow
  | Auto_rows
  | Auto_columns

let gd_kind_of : declaration -> gd_kind option = function
  | Declaration { property = Grid_template_rows; _ } -> Some Template_rows
  | Declaration { property = Grid_template_columns; _ } -> Some Template_columns
  | Declaration { property = Grid_template_areas; _ } -> Some Template_areas
  | Declaration { property = Grid_auto_flow; _ } -> Some Flow
  | Declaration { property = Grid_auto_rows; _ } -> Some Auto_rows
  | Declaration { property = Grid_auto_columns; _ } -> Some Auto_columns
  | _ -> None

(* [auto] is the initial of both auto-track properties (sec. 7.6), so a slot
   holding it is one the shorthand can leave out. *)
let gd_is_auto : Properties.grid_template -> bool = function
  | Auto -> true
  | _ -> false

let gd_flow_text (flow : Properties.grid_auto_flow) ~axis =
  match flow with
  | Row | Column -> Some "auto-flow"
  | Row_dense | Column_dense | Dense -> Some "auto-flow dense"
  | Components _ | Inherit | Initial | Unset | Revert | Revert_layer | Var _ ->
      ignore axis;
      Option.None

let gd_flow_axis : Properties.grid_auto_flow -> [ `Row | `Column | `Other ] =
  function
  | Row | Row_dense -> `Row
  | Column | Column_dense -> `Column
  (* [dense] alone leaves the axis at its initial, which is [row]. *)
  | Dense -> `Row
  | Components _ | Inherit | Initial | Unset | Revert | Revert_layer | Var _ ->
      `Other

let gd_auto_flow_rows_text ~flow ~auto_rows ~columns =
  match gd_flow_text flow ~axis:`Row with
  | Option.None -> Option.None
  | Some flow_text ->
      let buf = Buffer.create 32 in
      Buffer.add_string buf flow_text;
      if not (gd_is_auto auto_rows) then (
        Buffer.add_char buf ' ';
        Buffer.add_string buf (gt_track_text auto_rows));
      Buffer.add_char buf '/';
      Buffer.add_string buf
        (String.concat " " (List.map gt_track_text (gt_tracks_of columns)));
      Some (Buffer.contents buf)

let gd_auto_flow_columns_text ~rows ~flow ~auto_columns =
  match gd_flow_text flow ~axis:`Column with
  | Option.None -> Option.None
  | Some flow_text ->
      let buf = Buffer.create 32 in
      Buffer.add_string buf
        (String.concat " " (List.map gt_track_text (gt_tracks_of rows)));
      Buffer.add_char buf '/';
      Buffer.add_string buf flow_text;
      if not (gd_is_auto auto_columns) then (
        Buffer.add_char buf ' ';
        Buffer.add_string buf (gt_track_text auto_columns));
      Some (Buffer.contents buf)

let gd_read_grid text =
  let t = Cursor.of_string text in
  match Properties.read_grid t with
  | value when Cursor.is_done t -> Some value
  | _ -> Option.None
  | exception Cursor.Parse_error _ -> Option.None

let gd_is_none : Properties.grid_template -> bool = function
  | None -> true
  | _ -> false

let gd_template_form ~(rows : Properties.grid_template)
    ~(columns : Properties.grid_template)
    ~(areas : Properties.grid_template_areas) =
  match (gd_is_none rows, gd_is_none columns, areas) with
  | true, true, No_areas -> Some (Properties.None : Properties.grid_template)
  | _ -> (
      match gt_shorthand_text ~rows ~columns ~areas with
      | Option.None -> Option.None
      | Some text -> gd_read_grid text)

let grid_of_run raw : Properties.grid_template option =
  let rows = List.find_map gt_rows_of raw in
  let columns = List.find_map gt_columns_of raw in
  let areas = List.find_map gt_areas_of raw in
  let flow = List.find_map gd_auto_flow_of raw in
  let auto_rows = List.find_map gd_auto_rows_of raw in
  let auto_columns = List.find_map gd_auto_columns_of raw in
  match (rows, columns, areas, flow, auto_rows, auto_columns) with
  | ( Some rows,
      Some columns,
      Some areas,
      Some flow,
      Some auto_rows,
      Some auto_columns )
    when gt_track_is_plain rows && gt_track_is_plain columns
         && gt_track_is_plain auto_rows
         && gt_track_is_plain auto_columns -> (
      match (gd_flow_axis flow, areas) with
      | `Row, _
        when gd_is_auto auto_rows && gd_is_auto auto_columns
             && flow = (Row : Properties.grid_auto_flow) ->
          gd_template_form ~rows ~columns ~areas
      | `Row, No_areas when gd_is_none rows && gd_is_auto auto_columns ->
          Option.bind
            (gd_auto_flow_rows_text ~flow ~auto_rows ~columns)
            gd_read_grid
      | `Column, No_areas when gd_is_none columns && gd_is_auto auto_rows ->
          Option.bind
            (gd_auto_flow_columns_text ~rows ~flow ~auto_columns)
            gd_read_grid
      | (`Row | `Column | `Other), _ -> Option.None)
  | _ -> Option.None

let try_compose_grid_at idx i =
  let n = Rule_index.length idx in
  if i + 5 >= n then None
  else
    let positions = List.init 6 (fun j -> i + j) in
    if List.exists (Rule_index.is_absorbed idx) positions then None
    else
      let raw = List.map (Rule_index.decl_at idx) positions in
      let kinds = List.filter_map gd_kind_of raw in
      if
        (not (same_importance raw))
        || List.length (List.sort_uniq compare kinds) <> 6
      then None
      else
        Option.map
          (Declaration.v ~important:(is_important (List.hd raw)) Grid)
          (grid_of_run raw)

let compose_grid_via_index idx =
  compose_run_via_index idx ~try_compose:(fun idx i ->
      Option.map (fun d -> (d, 6)) (try_compose_grid_at idx i))
