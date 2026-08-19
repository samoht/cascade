(* css-grid-2: the grid container and placement properties. grid-template and
   its track lists, grid-template-areas and the area-string validator,
   grid-auto-flow / grid-auto-rows / grid-auto-columns, the grid shorthand,
   grid-line / grid-row / grid-column / grid-area, and the place-* shorthands.

   place-content/place-items compose the align and justify printers and readers
   from [Prop_align], so this module must be included after it.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_align

(* CSS Grid 2 sec. 7.6: [grid-auto-flow] is [ row | column ] || dense, and an
   omitted axis means [row], so [row dense] and [dense] are one value. *)
let normalize_grid_auto_flow : grid_auto_flow -> grid_auto_flow =
 fun value -> match value with Row_dense -> Dense | other -> other

let rec pp_grid_auto_flow : grid_auto_flow Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_grid_auto_flow ctx v
  | Row -> Pp.string ctx "row"
  | Column -> Pp.string ctx "column"
  | Dense -> Pp.string ctx "dense"
  | Row_dense -> Pp.string ctx "row dense"
  | Column_dense -> Pp.string ctx "column dense"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_grid_line : grid_line Pp.t =
 fun ctx -> function
  | Auto -> Pp.string ctx "auto"
  | Num n -> Pp.int ctx n
  | Name s -> Pp.string ctx s
  | Num_name (n, name) ->
      Pp.int ctx n;
      Pp.char ctx ' ';
      Pp.string ctx name
  | Span n ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.int ctx n
  | Span_name name ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.string ctx name
  | Span_num_name (1, name) when Pp.minified ctx ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.string ctx name
  | Span_num_name (n, name) ->
      Pp.string ctx "span";
      Pp.char ctx ' ';
      Pp.int ctx n;
      Pp.char ctx ' ';
      Pp.string ctx name
  | Calc c -> pp_calc pp_grid_line ctx c
  | Var v -> pp_var pp_grid_line ctx v

let rec pp_grid_line_pair : grid_line_pair Pp.t =
 fun ctx -> function
  | (Lines (start, end_) : grid_line_pair) -> (
      pp_grid_line ctx start;
      match end_ with
      | Auto -> ()
      | _ ->
          Pp.sp ctx ();
          Pp.slash ctx ();
          Pp.sp ctx ();
          pp_grid_line ctx end_)
  | Var v -> pp_var pp_grid_line_pair ctx v

(* Inverse of [grid_area_default_from]: drop trailing slots that match the value
   the spec would have implied from the preceding slot. Yields the shortest
   equivalent 1-/2-/3-/4-value spelling. *)
let grid_area_default_from (line : grid_line) : grid_line =
  match line with Name _ -> line | _ -> Auto

let grid_area_compact ~(row_start : grid_line) ~(column_start : grid_line)
    ~(row_end : grid_line) ~(column_end : grid_line) =
  let drop_column_end =
    equal_grid_line column_end (grid_area_default_from column_start)
  in
  let drop_row_end =
    drop_column_end
    && equal_grid_line row_end (grid_area_default_from row_start)
  in
  let drop_column_start =
    drop_row_end
    && equal_grid_line column_start (grid_area_default_from row_start)
  in
  match (drop_column_start, drop_row_end, drop_column_end) with
  | true, true, true -> [ row_start ]
  | _, true, true -> [ row_start; column_start ]
  | _, _, true -> [ row_start; column_start; row_end ]
  | _ -> [ row_start; column_start; row_end; column_end ]

let rec pp_grid_area : grid_area Pp.t =
 fun ctx -> function
  | Lines { row_start; column_start; row_end; column_end } ->
      let lines =
        grid_area_compact ~row_start ~column_start ~row_end ~column_end
      in
      let sep ctx () =
        Pp.sp ctx ();
        Pp.slash ctx ();
        Pp.sp ctx ()
      in
      Pp.list ~sep pp_grid_line ctx lines
  | Var v -> pp_var pp_grid_area ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_repeat_count ctx (count : repeat_count) =
  match count with
  | Count n -> Pp.int ctx n
  | Auto_fill -> Pp.string ctx "auto-fill"
  | Auto_fit -> Pp.string ctx "auto-fit"
  | Var v -> pp_var pp_repeat_count ctx v

let pp_grid_auto_flow_shorthand ctx = function
  | Row | Column -> Pp.string ctx "auto-flow"
  | Row_dense | Column_dense ->
      Pp.string ctx "auto-flow";
      Pp.space ctx ();
      Pp.string ctx "dense"
  | Dense ->
      Pp.string ctx "auto-flow";
      Pp.space ctx ();
      Pp.string ctx "dense"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_grid_auto_flow ctx v

let rec pp_grid_template : grid_template Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Px f -> Pp.unit ctx f "px"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Pct f -> Pp.pct ctx f
  | Vw f -> Pp.unit ctx f "vw"
  | Vh f -> Pp.unit ctx f "vh"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Zero -> Pp.char ctx '0'
  | Length l -> pp_length ctx l
  (* CSS Grid 2 sec. 7.2: [<flex>] is [<number>fr]; the unit-drop rule is for
     [<length>] only. [0fr] is a zero flex factor, distinct from a [0]
     [<length>] in [grid-template]'s union grammar. *)
  | Fr f ->
      Pp.float ctx f;
      Pp.string ctx "fr"
  | Auto -> Pp.string ctx "auto"
  | Min_content -> Pp.string ctx "min-content"
  | Max_content -> Pp.string ctx "max-content"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Min_max (min, max) ->
      Pp.call_2 "minmax" pp_grid_template pp_grid_template ctx (min, max)
  | Fit_content l -> Pp.call "fit-content" pp_length ctx l
  | Repeat (count, sizes) ->
      Pp.call "repeat"
        (fun ctx (count, sizes) ->
          pp_repeat_count ctx count;
          Pp.comma ctx ();
          pp_grid_track_list ctx sizes)
        ctx (count, sizes)
  | Tracks sizes -> pp_grid_track_list ctx sizes
  | Split (rows, columns) ->
      pp_grid_template ctx rows;
      Pp.slash ctx ();
      pp_grid_template ctx columns
  | Auto_flow_columns (rows, flow, auto_columns) ->
      pp_grid_template ctx rows;
      Pp.slash ctx ();
      pp_grid_auto_flow_shorthand ctx flow;
      Option.iter
        (fun columns ->
          Pp.space ctx ();
          pp_grid_template ctx columns)
        auto_columns
  | Auto_flow_rows (Row, None, columns) ->
      Pp.string ctx "none";
      Pp.slash ctx ();
      pp_grid_template ctx columns
  | Auto_flow_rows (flow, auto_rows, columns) ->
      pp_grid_auto_flow_shorthand ctx flow;
      Option.iter
        (fun rows ->
          Pp.space ctx ();
          pp_grid_template ctx rows)
        auto_rows;
      Pp.slash ctx ();
      pp_grid_template ctx columns
  | Named_tracks tracks ->
      let pp_named_track ctx (name, size) =
        (match name with
        | Some n ->
            Pp.char ctx '[';
            Pp.string ctx n;
            Pp.string ctx "] "
        | None -> ());
        pp_grid_template ctx size
      in
      Pp.list ~sep:Pp.space pp_named_track ctx tracks
  | Line_names names ->
      Pp.char ctx '[';
      Pp.list ~sep:Pp.space Pp.string ctx names;
      Pp.char ctx ']'
  | Template raw ->
      (* The complex grid-template syntax (with [<grid-template-areas>] string
         tracks) is stored as a canonical component-value slice because the
         typed grammar can't easily express it yet. *)
      Pp.string ctx raw
  | Subgrid -> Pp.string ctx "subgrid"
  | Masonry -> Pp.string ctx "masonry"
  | Var v -> pp_var pp_grid_template ctx v

and pp_grid_track_list ctx tracks =
  (* Track-list items are separated by whitespace, but [[...]] blocks have
     self-delimiting brackets, so [[col-start]minmax(...)] tokenises the same as
     [[col-start] minmax(...)]. Drop the inter-item space whenever the previous
     output ends with [\]] or [\)] (line-name block / function call) or the next
     item is a line-name block - matching the lightning / csso minified
     spelling. *)
  let buf_last_char ctx : char option = Pp.last_char ctx in
  let starts_with_bracket = function Line_names _ -> true | _ -> false in
  let rec loop = function
    | [] -> ()
    | [ x ] -> pp_grid_template ctx x
    | x :: y :: rest ->
        pp_grid_template ctx x;
        let needs_space =
          if not (Pp.minified ctx) then true
          else
            match buf_last_char ctx with
            | Some (']' | ')') -> false
            | _ -> not (starts_with_bracket y)
        in
        if needs_space then Pp.space ctx ();
        loop (y :: rest)
  in
  loop tracks

let rec normalize_grid_template (value : grid_template) : grid_template =
  match value with
  | Px 0. | Rem 0. | Em 0. | Vw 0. | Vh 0. | Vmin 0. | Vmax 0. -> Zero
  | Min_max (min, max) ->
      let min' = normalize_grid_template min in
      let max' = normalize_grid_template max in
      if min' == min && max' == max then value else Min_max (min', max')
  | Fit_content length ->
      let length' = Values.normalize_length length in
      if length' == length then value else Fit_content length'
  | Repeat (count, sizes) ->
      let sizes' = map_preserve normalize_grid_template sizes in
      if sizes' == sizes then value else Repeat (count, sizes')
  | Tracks sizes ->
      let sizes' = map_preserve normalize_grid_template sizes in
      if sizes' == sizes then value else Tracks sizes'
  | Split (rows, columns) ->
      let rows' = normalize_grid_template rows in
      let columns' = normalize_grid_template columns in
      if rows' == rows && columns' == columns then value
      else Split (rows', columns')
  | Auto_flow_columns (rows, flow, columns) ->
      let rows' = normalize_grid_template rows in
      let columns' = option_map_preserve normalize_grid_template columns in
      if rows' == rows && columns' == columns then value
      else Auto_flow_columns (rows', flow, columns')
  | Auto_flow_rows (flow, rows, columns) ->
      let rows' = option_map_preserve normalize_grid_template rows in
      let columns' = normalize_grid_template columns in
      if rows' == rows && columns' == columns then value
      else Auto_flow_rows (flow, rows', columns')
  | Named_tracks tracks ->
      let tracks' =
        map_preserve
          (fun ((name, track) as item) ->
            let track' = normalize_grid_template track in
            if track' == track then item else (name, track'))
          tracks
      in
      if tracks' == tracks then value else Named_tracks tracks'
  | _ -> value

let grid_area_row_ws = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let grid_template_area_row_cells row =
  let row_len = String.length row in
  let rec skip_ws i =
    if i < row_len && grid_area_row_ws row.[i] then skip_ws (i + 1) else i
  in
  let rec take_cell start i =
    if i < row_len && not (grid_area_row_ws row.[i]) then take_cell start (i + 1)
    else (String.sub row start (i - start), i)
  in
  let rec loop acc i =
    let start = skip_ws i in
    if start >= row_len then List.rev acc
    else
      let cell, next = take_cell start start in
      loop (cell :: acc) next
  in
  loop [] 0

(* CSS Grid Layout 2 sec. 7.3: a "null cell token" is one or more sequential
   periods, all denoting the same single empty cell. Collapse multi-dot
   spellings ([....] / [..]) to the canonical single [.]. *)
let normalize_grid_template_area_cell c =
  let n = String.length c in
  let rec all_dots i = i >= n || (c.[i] = '.' && all_dots (i + 1)) in
  if n > 1 && all_dots 0 then "." else c

let minify_grid_template_area_row row =
  String.concat " "
    (List.map normalize_grid_template_area_cell
       (grid_template_area_row_cells row))

let minify_grid_template_areas_string value =
  let len = String.length value in
  let buf = Buffer.create len in
  let rec take_quoted quote start i =
    if i >= len then (String.sub value start (i - start), i)
    else if value.[i] = quote then (String.sub value start (i - start), i + 1)
    else take_quoted quote start (i + 1)
  in
  let rec loop i =
    if i < len then
      match value.[i] with
      | ('"' | '\'') as quote ->
          let row, next = take_quoted quote (i + 1) (i + 1) in
          Buffer.add_char buf '"';
          Buffer.add_string buf (minify_grid_template_area_row row);
          Buffer.add_char buf '"';
          loop next
      | ' ' | '\t' | '\n' | '\r' | '\012' -> loop (i + 1)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents buf

let rec pp_grid_template_areas : grid_template_areas Pp.t =
 fun ctx -> function
  | No_areas -> Pp.string ctx "none"
  | Areas value ->
      if Pp.minified ctx then
        Pp.string ctx (minify_grid_template_areas_string value)
      else Pp.string ctx value
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_grid_template_areas ctx v

let rec pp_place_content : place_content Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_place_content ctx v
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Space_between -> Pp.string ctx "space-between"
  | Space_around -> Pp.string ctx "space-around"
  | Space_evenly -> Pp.string ctx "space-evenly"
  | Safe_center -> Pp.string ctx "safe center"
  | Safe_start -> Pp.string ctx "safe start"
  | Safe_end -> Pp.string ctx "safe end"
  | Safe_stretch -> Pp.string ctx "safe stretch"
  | Unsafe_center -> Pp.string ctx "unsafe center"
  | Unsafe_start -> Pp.string ctx "unsafe start"
  | Unsafe_end -> Pp.string ctx "unsafe end"
  | Unsafe_stretch -> Pp.string ctx "unsafe stretch"
  | Align_justify (a, j) ->
      let a_s = Pp.to_string ~minify:(Pp.minified ctx) pp_align_content a in
      let j_s = Pp.to_string ~minify:(Pp.minified ctx) pp_justify_content j in
      if Pp.minified ctx && a_s = j_s then Pp.string ctx a_s
      else (
        Pp.string ctx a_s;
        Pp.space ctx ();
        Pp.string ctx j_s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_place_items : place_items Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_place_items ctx v
  | Normal -> Pp.string ctx "normal"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Stretch -> Pp.string ctx "stretch"
  | Baseline -> Pp.string ctx "baseline"
  | Start_safe -> Pp.string ctx "safe start"
  | End_safe -> Pp.string ctx "safe end"
  | Center_safe -> Pp.string ctx "safe center"
  | Stretch_stretch when Pp.minified ctx -> Pp.string ctx "stretch"
  | Stretch_stretch -> Pp.string ctx "stretch stretch"
  | Align_justify (a, j) ->
      let a_s = Pp.to_string ~minify:(Pp.minified ctx) pp_align_items a in
      let j_s = Pp.to_string ~minify:(Pp.minified ctx) pp_justify_items j in
      (* CSS Align 3 sec. 6.1: when align and justify render to the same token,
         the single-value spelling is canonical. *)
      if Pp.minified ctx && a_s = j_s then Pp.string ctx a_s
      else (
        Pp.string ctx a_s;
        Pp.space ctx ();
        Pp.string ctx j_s)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let read_place_align_content t =
  match read_align_content t with
  | Left | Right | Safe_left | Safe_right | Unsafe_left | Unsafe_right ->
      Cursor.err_invalid t "place-content align value cannot be left or right"
  | value -> value

let read_place_content_pair t =
  let a, j = Cursor.pair read_place_align_content read_justify_content t in
  (Align_justify (a, j) : place_content)

let read_place_content_safe t =
  Cursor.expect_string "safe" t;
  Cursor.ws t;
  Cursor.enum "place-content safe"
    [
      ("center", (Safe_center : place_content));
      ("start", Safe_start);
      ("end", Safe_end);
      ("stretch", Safe_stretch);
    ]
    t

let read_place_content_unsafe t =
  Cursor.expect_string "unsafe" t;
  Cursor.ws t;
  Cursor.enum "place-content unsafe"
    [
      ("center", (Unsafe_center : place_content));
      ("start", Unsafe_start);
      ("end", Unsafe_end);
      ("stretch", Unsafe_stretch);
    ]
    t

let read_place_content_single t =
  Cursor.enum "place-content"
    [
      ("normal", (Normal : place_content));
      ("start", Start);
      ("end", End);
      ("center", Center);
      ("stretch", Stretch);
      ("space-between", Space_between);
      ("space-around", Space_around);
      ("space-evenly", Space_evenly);
      ("inherit", Inherit);
    ]
    t

let rec read_place_content t : place_content =
  Cursor.enum_or_var "place-content"
    [
      ("inherit", (Inherit : place_content));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_place_content t))
    ~default:
      (Cursor.one_of
         [
           read_place_content_pair;
           read_place_content_safe;
           read_place_content_unsafe;
           read_place_content_single;
         ])
    t

let read_place_items_safe t =
  Cursor.expect_string "safe" t;
  Cursor.ws t;
  match Cursor.ident t with
  | "start" -> Start_safe
  | "end" -> End_safe
  | "center" -> Center_safe
  | kw -> Cursor.err_invalid t ("place-items safe " ^ kw)

let read_place_items_stretch t =
  Cursor.expect_string "stretch" t;
  Cursor.ws t;
  if
    Cursor.option (fun t -> Cursor.expect_string "stretch" t) t
    |> Option.is_some
  then Stretch_stretch
  else Stretch

let place_items_align : place_items -> align_items option = function
  | Normal -> Some (Normal : align_items)
  | Start -> Some Start
  | End -> Some End
  | Center -> Some Center
  | Baseline -> Some Baseline
  | Stretch -> Some Stretch
  | _ -> None

let read_place_items_first t =
  Cursor.enum "place-items"
    [
      ("normal", (Normal : place_items));
      ("start", Start);
      ("end", End);
      ("center", Center);
      ("baseline", Baseline);
      ("inherit", Inherit);
    ]
    t

let read_place_items_default t =
  if Cursor.looking_at t "safe" then read_place_items_safe t
  else if Cursor.looking_at t "stretch" then read_place_items_stretch t
  else
    let first = read_place_items_first t in
    Cursor.ws t;
    match Cursor.option read_justify_items t with
    | None -> first
    | Some justify -> (
        match place_items_align first with
        | Some align -> Align_justify (align, justify)
        | None -> Cursor.err_invalid t "place-items two-value")

let rec read_place_items t : place_items =
  Cursor.ws t;
  Cursor.enum_or_var "place-items"
    [
      ("inherit", (Inherit : place_items));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_place_items t))
    ~default:read_place_items_default t

let rec read_grid_auto_flow t : grid_auto_flow =
  Cursor.enum_or_var "grid-auto-flow"
    [
      ("inherit", (Inherit : grid_auto_flow));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_grid_auto_flow t))
    ~default:(fun t ->
      let v = Cursor.ident t in
      Cursor.ws t;
      let second = Cursor.option Cursor.ident t in
      match (v, second) with
      | "row", Some "dense" -> Row_dense
      | "row", None -> Row
      | "column", Some "dense" -> Column_dense
      | "column", None -> Column
      | "dense", Some "row" -> Row_dense
      | "dense", Some "column" -> Column_dense
      | "dense", None -> Dense
      | _, Some _ ->
          err_invalid_value t "grid-auto-flow"
            (v ^ " " ^ Option.value second ~default:"")
      | _ -> err_invalid_value t "grid-auto-flow" v)
    t

let grid_line_at_end t =
  Cursor.is_done t || Cursor.peek_semicolon t
  ||
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Delim "/"; _ }) -> true
  | _ -> false

let read_grid_line_name t =
  let name = Cursor.ident t in
  if name = "span" then Cursor.err_invalid t "duplicate span grid line"
  else name

let read_grid_span t =
  let span_word = Cursor.ident t in
  if span_word <> "span" then
    Cursor.err t ("Expected 'span' but got " ^ span_word);
  Cursor.ws t;
  let first_int : int option = Cursor.option Cursor.int t in
  Cursor.ws t;
  let first_name : string option =
    if Option.is_none first_int && not (grid_line_at_end t) then
      Cursor.option read_grid_line_name t
    else None
  in
  Cursor.ws t;
  let second : [ `Name of string | `Num of int ] option =
    if grid_line_at_end t then None
    else
      match first_int with
      | Some _ ->
          Option.map
            (fun name -> `Name name)
            (Cursor.option read_grid_line_name t)
      | None -> Option.map (fun n -> `Num n) (Cursor.option Cursor.int t)
  in
  match (first_int, first_name, second) with
  | Some n, None, Some (`Name name) -> Span_num_name (n, name)
  | Some n, None, None -> Span n
  | None, Some name, Some (`Num n) -> Span_num_name (n, name)
  | None, Some name, None -> Span_name name
  | _ -> Cursor.err_invalid t "invalid span grid line"

let read_grid_line_number t : grid_line =
  let n = Cursor.int t in
  Cursor.ws t;
  let name : string option =
    if grid_line_at_end t then None else Cursor.option read_grid_line_name t
  in
  match name with Some name -> Num_name (n, name) | None -> Num n

let read_grid_line_name_value t : grid_line =
  let name = read_grid_line_name t in
  Cursor.ws t;
  let n : int option =
    if grid_line_at_end t then None else Cursor.option Cursor.int t
  in
  match n with Some n -> Num_name (n, name) | None -> Name name

let read_grid_line_calc t : grid_line =
  (* read_calc handles the calc(...) wrapper itself *)
  let expr =
    read_calc (fun _ -> Cursor.err t "unexpected value in grid-line calc") t
  in
  match eval_numeric_calc expr with
  | Some f when Float.is_integer f -> Num (int_of_float f)
  | Some _ -> Cursor.err_invalid t "grid-line calc must evaluate to integer"
  | None -> Calc expr

let rec read_grid_line t : grid_line =
  Cursor.enum_or_calls "grid-line"
    [ ("auto", (Auto : grid_line)) ]
    ~calls:
      [
        ("calc", read_grid_line_calc);
        ("var", fun t -> (Var (Values.read_var read_grid_line t) : grid_line));
      ]
    ~default:(fun t ->
      Cursor.one_of
        [ read_grid_line_number; read_grid_span; read_grid_line_name_value ]
        t)
    t

let read_grid_line_pair t : grid_line_pair =
  let read_pair t =
    let start = read_grid_line t in
    if Cursor.slash_opt t then
      let end_ = read_grid_line t in
      (Lines (start, end_) : grid_line_pair)
    else (Lines (start, Auto) : grid_line_pair)
  in
  if Cursor.looking_at_func "var" t then
    (Var (Values.read_var read_pair t) : grid_line_pair)
  else read_pair t

(* CSS Grid 2 sec. 8.4: an omitted slot inherits from the corresponding
   row/column start when that's a [<custom-ident>], else defaults to [auto]. The
   forward helper is [grid_area_default_from] (defined above with the printer);
   both directions share it. *)
let read_grid_area t : grid_area =
  let first = read_grid_line t in
  let rest =
    Cursor.many
      (fun t ->
        Cursor.slash t;
        read_grid_line t)
      t
    |> fst
  in
  let row_start, column_start, row_end, column_end =
    match rest with
    | [] ->
        let other = grid_area_default_from first in
        (first, other, other, other)
    | [ v2 ] ->
        let row_end = grid_area_default_from first in
        let column_end = grid_area_default_from v2 in
        (first, v2, row_end, column_end)
    | [ v2; v3 ] ->
        let column_end = grid_area_default_from v2 in
        (first, v2, v3, column_end)
    | [ v2; v3; v4 ] -> (first, v2, v3, v4)
    | _ -> err_invalid_value t "grid-area" "too many grid lines"
  in
  Lines { row_start; column_start; row_end; column_end }

module Grid_template = struct
  let read_length_as_grid t : grid_template =
    (* [~with_keywords:false]: track keywords (auto / min-content / ...) are a
       separate [one_of] alternative, so this reader handles only real lengths -
       the unit-specific cases below, plus a general [Length] carrier for a
       [calc()], a [var()] in a [calc()], or a less common unit. *)
    match read_length ~with_keywords:false t with
    | Px n -> (Px n : grid_template)
    | Rem n -> Rem n
    | Em n -> Em n
    | Vw n -> Vw n
    | Vh n -> Vh n
    | Vmin n -> Vmin n
    | Vmax n -> Vmax n
    | Pct n -> Pct n
    | Zero -> Zero
    | other -> Length other

  let read_fr t : grid_template =
    (* [1fr] lexes as a single [Dimension] with unit "fr". *)
    match Cursor.dimension_opt t with
    | Some (n, "fr") -> Fr n
    | _ -> Cursor.err_expected t "<fr>"

  let read_track_breadth t : grid_template =
    (* Accept a single breadth: length, fr, or keywords *)
    Cursor.one_of
      [
        read_fr;
        read_length_as_grid;
        (fun t ->
          Cursor.enum "grid-breadth"
            [
              ("auto", (Auto : grid_template));
              ("min-content", (Min_content : grid_template));
              ("max-content", (Max_content : grid_template));
              ("inherit", (Inherit : grid_template));
            ]
            t);
      ]
      t

  let read_minmax t : grid_template =
    Cursor.call "minmax" t @@ fun inner ->
    Cursor.ws inner;
    let minv = read_track_breadth inner in
    Cursor.ws inner;
    Cursor.comma inner;
    Cursor.ws inner;
    let maxv = read_track_breadth inner in
    Min_max (minv, maxv)

  let read_fit_content t : grid_template =
    Cursor.call "fit-content" t @@ fun inner ->
    Cursor.ws inner;
    Fit_content (read_length inner)

  let rec read_repeat_count t : repeat_count =
    if Cursor.looking_at t "var(" then Var (Values.read_var read_repeat_count t)
    else
      match Cursor.option Cursor.int t with
      | Some n -> Count n
      | None -> (
          match Cursor.ident t with
          | "auto-fill" -> Auto_fill
          | "auto-fit" -> Auto_fit
          | ident -> Cursor.err_invalid t ("repeat count: " ^ ident))

  let read_line_names t : grid_template =
    Cursor.brackets
      (fun inner ->
        Cursor.ws inner;
        let names =
          Cursor.list
            ~sep:(fun i -> Cursor.ws i)
            (fun i -> Cursor.ident i)
            inner
        in
        Line_names names)
      t

  let rec read_single_track t =
    if Cursor.peek_block t = Some Token.Square then read_line_names t
    else
      Cursor.enum_or_calls "grid-template"
        [
          ("none", (None : grid_template));
          ("auto", Auto);
          ("min-content", Min_content);
          ("max-content", Max_content);
          ("subgrid", Subgrid);
          ("masonry", Masonry);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~calls:
          [
            ("minmax", read_minmax);
            ("fit-content", read_fit_content);
            ( "repeat",
              fun t ->
                Cursor.call "repeat" t @@ fun inner ->
                Cursor.ws inner;
                let count = read_repeat_count inner in
                Cursor.ws inner;
                Cursor.comma inner;
                Cursor.ws inner;
                let tracks =
                  Cursor.list
                    ~sep:(fun i -> Cursor.ws i)
                    read_single_track inner
                in
                (Repeat (count, tracks) : grid_template) );
          ]
        ~default:(fun t -> Cursor.one_of [ read_length_as_grid; read_fr ] t)
        t
end

let grid_template_needs_raw_template cvs =
  let rec has_string = function
    | [] -> false
    | Component.Preserved { kind = Token.String _; _ } :: _ -> true
    | Component.Block { node = { value; _ }; _ } :: rest ->
        has_string value || has_string rest
    | Component.Func { node = { arguments; _ }; _ } :: rest ->
        has_string arguments || has_string rest
    | _ :: rest -> has_string rest
  in
  has_string cvs

let grid_template_top_level_slashes cvs =
  List.fold_left
    (fun count -> function
      | Component.Preserved { kind = Token.Delim "/"; _ } -> count + 1
      | _ -> count)
    0 cvs

let grid_template_components_well_formed cvs =
  let rec well_formed = function
    | [] -> true
    | Component.Preserved
        { kind = Token.Close _ | Token.Bad_string | Token.Bad_url; _ }
      :: _ ->
        false
    | Component.Block { node = { value; closed; _ }; _ } :: rest ->
        closed && well_formed value && well_formed rest
    | Component.Func { node = { arguments; terminated; _ }; _ } :: rest ->
        terminated && well_formed arguments && well_formed rest
    | _ :: rest -> well_formed rest
  in
  well_formed cvs

let read_grid_template_tracks t =
  let tracks =
    Cursor.list ~sep:(fun t -> Cursor.ws t) Grid_template.read_single_track t
  in
  match tracks with
  | [] -> Cursor.err t "Expected at least one grid track"
  | [ single ] -> single
  | multiple
    when List.exists
           (fun (track : grid_template) ->
             match track with None | Subgrid | Masonry -> true | _ -> false)
           multiple ->
      Cursor.err_invalid t "grid-template standalone keyword in track list"
  | multiple -> Tracks multiple

let rec read_grid_template t : grid_template =
  if Cursor.looking_at_func "var" t then
    (Var (Values.read_var read_grid_template t) : grid_template)
  else if grid_template_needs_raw_template (Cursor.remaining t) then (
    let cvs = Cursor.remaining t in
    if grid_template_top_level_slashes cvs > 1 then
      Cursor.err_invalid t "grid-template duplicate slash form";
    if not (grid_template_components_well_formed cvs) then
      Cursor.err_invalid t "grid-template malformed raw template";
    let raw = Cursor.consume_to_decl_end ~trim:true t in
    Template
      (Parser.to_string_minified (Cursor.remaining (Cursor.of_string raw))))
  else
    let rows = read_grid_template_tracks t in
    Cursor.ws t;
    if Cursor.slash_opt t then (
      Cursor.ws t;
      let columns = read_grid_template_tracks t in
      match (rows, columns) with
      | None, _ | _, None ->
          err_invalid_value t "grid-template" "none in slash form"
      | _ -> Split (rows, columns))
    else rows

let read_grid_auto_flow_clause side t =
  let rec loop seen_auto_flow seen_dense =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some "auto-flow" when not seen_auto_flow ->
        let _ = Cursor.ident t in
        loop true seen_dense
    | Some "dense" when not seen_dense ->
        let _ = Cursor.ident t in
        loop seen_auto_flow true
    | _ -> (
        if not seen_auto_flow then Cursor.err_expected t "auto-flow";
        match (side, seen_dense) with
        | `Rows, false -> (Row : grid_auto_flow)
        | `Rows, true -> Row_dense
        | `Columns, false -> Column
        | `Columns, true -> Column_dense)
  in
  loop false false

let read_grid_auto_flow_tracks t : grid_template option =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Delim "/"; _ }) | None -> None
  | Some _ -> Cursor.option read_grid_template_tracks t

let grid_starts_auto_flow t =
  match Cursor.peek_ident t with
  | Some ("auto-flow" | "dense") -> true
  | _ -> false

let read_grid_split t (rows : grid_template) (columns : grid_template) :
    grid_template =
  match (rows, columns) with
  | None, _ | _, None -> err_invalid_value t "grid" "none in slash form"
  | _ -> Split (rows, columns)

let read_grid_auto_flow_rows t =
  let flow = read_grid_auto_flow_clause `Rows t in
  let auto_rows = read_grid_auto_flow_tracks t in
  Cursor.ws t;
  Cursor.slash t;
  Cursor.ws t;
  let columns = read_grid_template_tracks t in
  Auto_flow_rows (flow, auto_rows, columns)

let read_grid_auto_flow_columns t rows =
  let flow = read_grid_auto_flow_clause `Columns t in
  let auto_columns = read_grid_auto_flow_tracks t in
  Auto_flow_columns (rows, flow, auto_columns)

let read_grid_slash_rhs t rows =
  Cursor.ws t;
  if grid_starts_auto_flow t then read_grid_auto_flow_columns t rows
  else read_grid_split t rows (read_grid_template_tracks t)

let read_grid_template_or_split t =
  let rows = read_grid_template_tracks t in
  Cursor.ws t;
  if Cursor.slash_opt t then read_grid_slash_rhs t rows else rows

let rec read_grid t : grid_template =
  if Cursor.looking_at_func "var" t then
    (Var (Values.read_var read_grid t) : grid_template)
  else if grid_template_needs_raw_template (Cursor.remaining t) then
    (* CSS Grid 1 sec. 10.1 [<'grid-template'>] form of [grid]: when the input
       contains a [<string>] token, the value is the [<line-names>? <string>
       <track-size>? <line-names>?]+ form, which [read_grid_template] already
       handles. *)
    read_grid_template t
  else if grid_starts_auto_flow t then read_grid_auto_flow_rows t
  else read_grid_template_or_split t

let is_grid_area_ws = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let grid_area_row_cells row =
  let len = String.length row in
  let rec skip_ws i =
    if i < len && is_grid_area_ws row.[i] then skip_ws (i + 1) else i
  in
  let rec take_cell start i =
    if i < len && not (is_grid_area_ws row.[i]) then take_cell start (i + 1)
    else (String.sub row start (i - start), i)
  in
  let rec loop acc i =
    let start = skip_ws i in
    if start >= len then List.rev acc
    else
      let cell, next = take_cell start start in
      loop (cell :: acc) next
  in
  loop [] 0

let grid_area_null_cell cell =
  let len = String.length cell in
  len > 0
  &&
  let rec loop i = i = len || (cell.[i] = '.' && loop (i + 1)) in
  loop 0

(* CSS Grid Layout 2 section 7.3: each row string is a sequence of [.] (null
   cell) tokens or [<custom-ident>] cell names. A [<custom-ident>] starts with a
   letter, [_], or [-]-followed-by-letter, and continues with letters / digits /
   [_] / [-]. *)
let grid_area_ident_cell cell =
  let len = String.length cell in
  let is_start c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'
  in
  let is_continue c = is_start c || (c >= '0' && c <= '9') in
  len > 0
  && is_start cell.[0]
  &&
  let rec loop i = i = len || (is_continue cell.[i] && loop (i + 1)) in
  loop 1

let validate_grid_area_cell t cell =
  if not (grid_area_null_cell cell || grid_area_ident_cell cell) then
    Cursor.err_invalid t ("invalid grid-template-areas cell: " ^ cell)

let validate_grid_area_width t (expected : int option) cells =
  match expected with
  | None -> Some (List.length cells)
  | Some width when List.length cells = width -> expected
  | Some _ -> Cursor.err_invalid t "grid-template-areas rows differ in width"

let grid_area_positions rows =
  rows
  |> List.mapi (fun row cells ->
      cells
      |> List.mapi (fun col cell -> (cell, row, col))
      |> List.filter (fun (cell, _, _) -> not (grid_area_null_cell cell)))
  |> List.flatten

let grid_area_names positions =
  positions
  |> List.fold_left
       (fun names (cell, _, _) ->
         if List.mem cell names then names else cell :: names)
       []

let validate_grid_area_rectangles t rows =
  let positions = grid_area_positions rows in
  let cell_at row col = List.nth (List.nth rows row) col in
  let validate_name name =
    let coords =
      positions
      |> List.filter_map (fun (cell, row, col) ->
          if cell = name then Some (row, col) else None)
    in
    let rows = List.map fst coords in
    let cols = List.map snd coords in
    let min_row = List.fold_left min max_int rows in
    let max_row = List.fold_left max min_int rows in
    let min_col = List.fold_left min max_int cols in
    let max_col = List.fold_left max min_int cols in
    for row = min_row to max_row do
      for col = min_col to max_col do
        if cell_at row col <> name then
          Cursor.err_invalid t
            "grid-template-areas named area is not rectangular"
      done
    done
  in
  List.iter validate_name (grid_area_names positions)

let read_grid_template_areas_row t width rows rendered =
  Cursor.ws t;
  match Cursor.string_opt t with
  | None -> `Stop
  | Some s ->
      let cells = grid_area_row_cells s in
      if cells = [] then Cursor.err_invalid t "empty grid-template-areas row";
      List.iter (validate_grid_area_cell t) cells;
      let width = validate_grid_area_width t width cells in
      `Continue (width, cells :: rows, ("\"" ^ s ^ "\"") :: rendered)

let read_grid_template_areas_rows t =
  let rec loop width rows rendered =
    match read_grid_template_areas_row t width rows rendered with
    | `Stop ->
        let rows = List.rev rows in
        if rows = [] then Cursor.err_expected t "grid-template-areas row";
        validate_grid_area_rectangles t rows;
        (Areas (String.concat " " (List.rev rendered)) : grid_template_areas)
    | `Continue (width, rows, rendered) -> loop width rows rendered
  in
  loop (None : int option) [] []

let rec read_grid_template_areas t : grid_template_areas =
  Cursor.enum_or_var "grid-template-areas"
    [
      ("none", (No_areas : grid_template_areas));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_grid_template_areas t) : grid_template_areas))
    ~default:read_grid_template_areas_rows t
