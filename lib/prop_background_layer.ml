open Common
open Values
open Properties_intf
open Prop_common
open Prop_image

(* CSS Backgrounds 3 (ED) sec. 2.10: the shorthand resets every longhand it
   covers before setting the ones written, so a slot holding its own initial
   value declares what leaving it out declares and the shorter spelling wins. *)
let drop_initial_slot : 'a. 'a -> 'a option -> 'a option =
 fun initial opt ->
  match opt with Some x when x = initial -> (None : _ option) | _ -> opt

(* sec. 2.6 gives background-position the initial value [0% 0%] and reads a lone
   value as the horizontal offset with [center] vertically, so a [Single] names
   [<x> 50%] and never the initial position however small [<x>] is. *)
let position_is_layer_initial (p : position_value) =
  match p with
  | XY (x, y) -> is_zero_length x && is_zero_length y
  | Left_top | Top_left -> true
  | _ -> false

(* sec. 2.10 reads a single [<box>] as setting both background-origin and
   background-clip, and a pair as setting origin then clip, so an absent clip
   means the origin's box wherever one is written, and only sec. 2.7's initial
   [border-box] when none is. Read the pair back to those two boxes and write
   the shortest spelling of them: both initial (sec. 2.8 [padding-box] for the
   origin) and neither is written, equal and the origin alone says both,
   otherwise both are written. Dropping just the one at its initial value would
   leave a [<box>] that reassigns the other. *)
let drop_initial_boxes (origin : background_box option)
    (clip : background_box option) :
    background_box option * background_box option =
  let o = Option.value origin ~default:(Padding_box : background_box) in
  let c =
    match (clip, origin) with
    | Option.Some c, _ -> c
    | Option.None, Option.Some o -> o
    | Option.None, Option.None -> (Border_box : background_box)
  in
  if o = Padding_box && c = Border_box then (Option.None, Option.None)
  else if equal_background_box o c then (Option.Some o, Option.None)
  else (Option.Some o, Option.Some c)

(* A layer that fills no slot declares what [background: none] declares, and
   sec. 2.6's initial position is the shortest spelling of it, so that is where
   the two meet. *)
let drained_background_layer : background_shorthand =
  {
    color = None;
    image = None;
    position = Some (XY (Zero, Zero) : position_value);
    size = None;
    repeat = None;
    attachment = None;
    clip = None;
    origin = None;
  }

(* CSS Backgrounds 3 (ED) sec. 2.4 lists the pair each single [<repeat-style>]
   keyword computes to, so every pair that appears there has a one-keyword
   spelling of the same value and the shorter one wins. The pairs with two
   different axes and no alias stay as written. *)
let rec normalize_background_repeat : background_repeat -> background_repeat =
 fun value ->
  match value with
  | Layers layers ->
      preserve_if_equal value
        (Layers (map_preserve normalize_background_repeat layers))
  | Repeat_repeat -> Repeat
  | Space_space -> Space
  | Round_round -> Round
  | No_repeat_no_repeat -> No_repeat
  | Repeat_no_repeat -> Repeat_x
  | No_repeat_repeat -> Repeat_y
  | other -> other

let background_layer_is_empty (b : background_shorthand) =
  Option.is_none b.color && Option.is_none b.image && Option.is_none b.position
  && Option.is_none b.size && Option.is_none b.repeat
  && Option.is_none b.attachment
  && Option.is_none b.clip && Option.is_none b.origin

let background_layer_slots_shared (a : background_shorthand)
    (b : background_shorthand) =
  a.color == b.color && a.image == b.image && a.position == b.position
  && a.size == b.size && a.repeat == b.repeat
  && a.attachment == b.attachment
  && a.clip == b.clip && a.origin == b.origin

let normalize_background_shorthand ?(lossless = false)
    (b : background_shorthand) =
  let color =
    option_map_preserve
      (normalize_color ~lossless)
      (drop_initial_slot (Transparent : color) b.color)
  in
  let image =
    option_map_preserve
      (normalize_background_image ~lossless)
      (drop_initial_slot (None : background_image) b.image)
  in
  let repeat =
    drop_initial_slot
      (Repeat : background_repeat)
      (option_map_preserve normalize_background_repeat b.repeat)
  in
  let attachment =
    drop_initial_slot (Scroll : background_attachment) b.attachment
  in
  let size = drop_initial_slot (Auto : background_size) b.size in
  (* The size is written after the position and a [/], so the position can only
     go once the size has. *)
  let position = option_map_preserve normalize_position_value b.position in
  let position =
    match position with
    | Some p when Option.is_none size && position_is_layer_initial p ->
        (None : position_value option)
    | _ -> position
  in
  let origin, clip = drop_initial_boxes b.origin b.clip in
  let b' = { color; image; position; size; repeat; attachment; clip; origin } in
  if background_layer_is_empty b' then drained_background_layer
  else if background_layer_slots_shared b b' then b
  else b'

let normalize_background ?(lossless = false) : background -> background =
 fun value ->
  match value with
  | Shorthand s ->
      let s' = normalize_background_shorthand ~lossless s in
      if s' == s then value else Shorthand s'
  | None -> Shorthand drained_background_layer
  | other -> other

let rec pp_background_attachment : background_attachment Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_background_attachment ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_attachment ctx layers
  | Fixed -> Pp.string ctx "fixed"
  | Local -> Pp.string ctx "local"
  | Scroll -> Pp.string ctx "scroll"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_background_repeat : background_repeat Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_background_repeat ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_repeat ctx layers
  | Repeat -> Pp.string ctx "repeat"
  | Space -> Pp.string ctx "space"
  | Round -> Pp.string ctx "round"
  | No_repeat -> Pp.string ctx "no-repeat"
  | Repeat_x -> Pp.string ctx "repeat-x"
  | Repeat_y -> Pp.string ctx "repeat-y"
  | Repeat_repeat -> Pp.string ctx "repeat repeat"
  | Repeat_space -> Pp.string ctx "repeat space"
  | Repeat_round -> Pp.string ctx "repeat round"
  | Repeat_no_repeat -> Pp.string ctx "repeat no-repeat"
  | Space_repeat -> Pp.string ctx "space repeat"
  | Space_space -> Pp.string ctx "space space"
  | Space_round -> Pp.string ctx "space round"
  | Space_no_repeat -> Pp.string ctx "space no-repeat"
  | Round_repeat -> Pp.string ctx "round repeat"
  | Round_space -> Pp.string ctx "round space"
  | Round_round -> Pp.string ctx "round round"
  | Round_no_repeat -> Pp.string ctx "round no-repeat"
  | No_repeat_repeat -> Pp.string ctx "no-repeat repeat"
  | No_repeat_space -> Pp.string ctx "no-repeat space"
  | No_repeat_round -> Pp.string ctx "no-repeat round"
  | No_repeat_no_repeat -> Pp.string ctx "no-repeat no-repeat"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_background_box : background_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_background_box ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_box ctx layers
  | Border_box -> Pp.string ctx "border-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Content_box -> Pp.string ctx "content-box"
  | Text -> Pp.string ctx "text"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* [background-size] and the two [mask-size] spellings share this value, and
   nothing folded the length inside it, so an authored [10.0px] stayed a node of
   its own beside the [10px] the canonical spelling reads as. *)
let rec normalize_background_size : background_size -> background_size =
 fun value ->
  match value with
  | Length len ->
      let len' = Values.canonical_dimension len in
      if len' == len then value else Length len'
  | Size (a, b) ->
      let a' = Values.canonical_dimension a in
      let b' = Values.canonical_dimension b in
      if a' == a && b' == b then value else Size (a', b')
  | Layers layers ->
      let layers' = List.map normalize_background_size layers in
      if List.equal ( == ) layers layers' then value else Layers layers'
  | _ -> value

let rec pp_background_size : background_size Pp.t =
 fun ctx -> function
  | Layers layers -> Pp.list ~sep:Pp.comma pp_background_size ctx layers
  | Auto -> Pp.string ctx "auto"
  | Cover -> Pp.string ctx "cover"
  | Contain -> Pp.string ctx "contain"
  | Length l -> pp_length ctx l
  | Size (w, h) ->
      pp_length ctx w;
      Pp.char ctx ' ';
      pp_length ctx h
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_background_size ctx v

(* CSS Backgrounds 3 sec. 2.6: the value is one position per background layer,
   comma-separated. It is not a box shorthand, so the layers neither join with
   spaces nor collapse the way margins do. *)
let pp_background_position : background_position Pp.t =
 fun ctx positions -> Pp.list ~sep:Pp.comma pp_position_value ctx positions

(* CSS Backgrounds 4 sec. 3.3: each axis longhand is [center | [<edge>?
   <length-percentage>]], the edge naming that axis only. *)
let pp_position_axis_edge : position_axis_edge Pp.t =
 fun ctx -> function
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"

let rec pp_background_position_axis : background_position_axis Pp.t =
 fun ctx -> function
  | Center -> Pp.string ctx "center"
  | Edge e -> pp_position_axis_edge ctx e
  | Offset lp -> pp_length_percentage ctx lp
  | Edge_offset (e, lp) ->
      pp_position_axis_edge ctx e;
      Pp.space ctx ();
      pp_length_percentage ctx lp
  | Layers positions ->
      Pp.list ~sep:Pp.comma pp_background_position_axis ctx positions
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_background_position_axis ctx v

let pp_bg_prop maybe_space pp_func ctx = function
  | Some value ->
      maybe_space ();
      pp_func ctx value
  | None -> ()

let pp_bg_size_with_position maybe_space (bg : background_shorthand) ctx =
  match bg.size with
  | Some size when bg.position <> None ->
      Pp.string ctx "/";
      pp_background_size ctx size
  | Some size ->
      (* A [<bg-size>] is only reachable after [<position> /], so emit the
         initial position [0 0] (= [0% 0%]) when none was given, otherwise the
         shorthand fails to reparse. *)
      maybe_space ();
      Pp.string ctx "0 0/";
      pp_background_size ctx size
  | None -> ()

let pp_background_shorthand : background_shorthand Pp.t =
 fun ctx bg ->
  let first = ref true in
  let maybe_space () = if !first then first := false else Pp.token_sp ctx () in
  pp_bg_prop maybe_space pp_background_image ctx bg.image;
  pp_bg_prop maybe_space pp_position_value ctx bg.position;
  pp_bg_size_with_position maybe_space bg ctx;
  pp_bg_prop maybe_space pp_background_repeat ctx bg.repeat;
  pp_bg_prop maybe_space pp_background_attachment ctx bg.attachment;
  pp_bg_prop maybe_space pp_background_box ctx bg.origin;
  pp_bg_prop maybe_space pp_background_box ctx bg.clip;
  pp_bg_prop maybe_space pp_color ctx bg.color;
  (* The record is public, so a caller can hand over a layer with no slot
     filled. It declares nothing but the initial longhands, which is what
     [background: none] declares (CSS Backgrounds 3 (ED) sec. 2.10); the empty
     string is not a value any parser reads back. *)
  if !first then Pp.string ctx "none"

let rec pp_background : background Pp.t =
 fun ctx -> function
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | None -> Pp.string ctx "none"
  | Var v -> pp_var pp_background ctx v
  | Vars vars -> Pp.list ~sep:Pp.space (pp_var pp_background) ctx vars
  | Shorthand s -> pp_background_shorthand ctx s

(* Background-related readers *)
let rec read_background_attachment t : background_attachment =
  let read_layer t =
    Cursor.enum "background-attachment layer"
      [
        ("scroll", (Scroll : background_attachment));
        ("fixed", Fixed);
        ("local", Local);
      ]
      t
  in
  let read_layers t =
    match Cursor.list ~sep:Cursor.comma ~at_least:1 read_layer t with
    | [ layer ] -> layer
    | layers -> Layers layers
  in
  Cursor.enum_or_var "background-attachment"
    [
      ("initial", (Initial : background_attachment));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_background_attachment t))
    ~default:read_layers t

let rec read_background_repeat t : background_repeat =
  let read_style t =
    Cursor.enum "background-repeat"
      [
        ("repeat", (Repeat : background_repeat));
        ("space", Space);
        ("round", Round);
        ("no-repeat", No_repeat);
      ]
      t
  in
  let pair (a : background_repeat) (b : background_repeat) =
    match (a, b) with
    | Repeat, Repeat -> Repeat_repeat
    | Repeat, Space -> Repeat_space
    | Repeat, Round -> Repeat_round
    | Repeat, No_repeat -> Repeat_no_repeat
    | Space, Repeat -> Space_repeat
    | Space, Space -> Space_space
    | Space, Round -> Space_round
    | Space, No_repeat -> Space_no_repeat
    | Round, Repeat -> Round_repeat
    | Round, Space -> Round_space
    | Round, Round -> Round_round
    | Round, No_repeat -> Round_no_repeat
    | No_repeat, Repeat -> No_repeat_repeat
    | No_repeat, Space -> No_repeat_space
    | No_repeat, Round -> No_repeat_round
    | No_repeat, No_repeat -> No_repeat_no_repeat
    | _ -> a
  in
  let read_repeats t =
    let first = read_style t in
    Cursor.ws t;
    match Cursor.option read_style t with
    | None -> first
    | Some second -> pair first second
  in
  Cursor.enum_or_var "background-repeat"
    [
      ("repeat-x", Repeat_x);
      ("repeat-y", Repeat_y);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_background_repeat t))
    ~default:read_repeats t

(* The standalone [background-repeat] / [mask-repeat] longhand is a
   comma-separated layer list (CSS Backgrounds 3 sec. 2.6); the [background] /
   [mask] shorthand reuses the single-value [read_background_repeat] so it does
   not eat the layer comma. Same split for the box / size / composite readers
   below. *)
let read_background_repeat_list t : background_repeat =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_background_repeat t with
  | [ one ] -> one
  | many -> Layers many

(* CSS Backgrounds 3 sec. 3.9 spells [<bg-size>] as [[ <length-percentage
   [0,inf]> | auto ]{1,2} | cover | contain], so [auto] fills one slot of the
   pair as readily as the whole value, and no sizing function stands in either
   slot. *)
let rec read_background_size t : background_size =
  let read_slot t : length =
    Cursor.one_of
      [
        (fun t ->
          Cursor.expect_string "auto" t;
          (Auto : length));
        read_length ~allow_negative:false ~with_keywords:false;
      ]
      t
  in
  let read_pair t : background_size =
    let a, b = Cursor.pair read_slot read_slot t in
    Size (a, b)
  in
  let read_single t : background_size =
    match read_slot t with
    | (Auto : length) -> (Auto : background_size)
    | len -> Length len
  in
  let read_var_call t : background_size =
    (Var (read_var read_background_size t) : background_size)
  in
  Cursor.enum_or_var "background-size"
    [
      ("cover", (Cover : background_size));
      ("contain", Contain);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:read_var_call
    ~default:(fun t -> Cursor.one_of [ read_pair; read_single ] t)
    t

let read_background_size_list t : background_size =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_background_size t with
  | [ one ] -> one
  | many -> Layers many

let background_shorthand ?color ?image ?position ?size ?repeat ?attachment ?clip
    ?origin () : background =
  Shorthand { color; image; position; size; repeat; attachment; clip; origin }

(* Parser for background_box values *)
let rec read_background_box t : background_box =
  Cursor.enum_or_var "background-box"
    [
      ("border-box", (Border_box : background_box));
      ("padding-box", Padding_box);
      ("content-box", Content_box);
      ("text", Text);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_background_box t))
    t

let read_background_box_list t : background_box =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_background_box t with
  | [ one ] -> one
  | many -> Layers many

(* CSS Backgrounds 4 sec. 3.3 measures a bare offset from the start edge, so
   [left 10px] and [10px] name the same position and the shorter wins. The zero
   percentage of the start edge is what [left] / [top] names, and the hundred
   percent is [right] / [bottom]. *)
let rec normalize_background_position_axis :
    background_position_axis -> background_position_axis =
 fun value ->
  let lp = Values.normalize_length_percentage in
  let start_edge : position_axis_edge -> bool = function
    | Left | Top -> true
    | _ -> false
  in
  match value with
  | Offset o -> preserve_if_equal value (Offset (lp o))
  | Edge_offset (e, o) when start_edge e -> Offset (lp o)
  | Edge_offset (e, o) -> preserve_if_equal value (Edge_offset (e, lp o))
  (* Each layer holds a position of its own, so the fold reaches every one of
     them: a list left alone is a second node printing what the folded one
     prints. *)
  | Layers positions ->
      preserve_if_equal value
        (Layers (List.map normalize_background_position_axis positions))
  | other -> other

let read_background_position t : background_position =
  Cursor.list ~at_least:1 ~sep:Cursor.comma read_background_position_value t

(* The edge keywords belong to one axis each, so the reader takes the pair its
   property accepts and refuses the other axis's. An edge may carry an offset
   from it; a bare offset is measured from the start edge. *)
let read_background_position_axis ~label ~start_edge ~end_edge =
  let rec read t : background_position_axis =
    let edges =
      [
        (Pp.to_string pp_position_axis_edge start_edge, start_edge);
        (Pp.to_string pp_position_axis_edge end_edge, end_edge);
      ]
    in
    let after_edge e t : background_position_axis =
      if Cursor.is_done t || Cursor.peek_comma t then Edge e
      else
        let snap = Cursor.save t in
        match Values.read_length_percentage t with
        | lp -> Edge_offset (e, lp)
        | exception Cursor.Parse_error _ ->
            Cursor.restore t snap;
            Edge e
    in
    Cursor.enum_or_whole_value_var label
      [
        ("inherit", (Inherit : background_position_axis));
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~var:(fun t -> Var (Values.read_var read t))
      ~default:(fun t : background_position_axis ->
        match Cursor.peek_keyword t with
        | Some "center" ->
            ignore (Cursor.ident_opt t);
            Center
        | Some name -> (
            match List.assoc_opt name edges with
            | Some e ->
                ignore (Cursor.ident_opt t);
                after_edge e t
            | Option.None -> Cursor.err_expected t label)
        | Option.None -> Offset (Values.read_length_percentage t))
      t
  in
  read

(* Sec. 3.6 spells the longhand with the same [#] the pair carries, so a comma
   separates one layer's position from the next. One position is the bare
   constructor: a lone [Layers [p]] would be a second node printing the same
   text. A CSS-wide keyword is the whole value, which the reader below answers
   before this sees a comma. *)
let read_background_position_axis_list ~label ~start_edge ~end_edge t =
  let read t = read_background_position_axis ~label ~start_edge ~end_edge t in
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read t with
  | [ position ] -> position
  | positions -> (Layers positions : background_position_axis)

let read_background_position_x t =
  read_background_position_axis_list ~label:"background-position-x"
    ~start_edge:Left ~end_edge:Right t

let read_background_position_y t =
  read_background_position_axis_list ~label:"background-position-y"
    ~start_edge:Top ~end_edge:Bottom t

module Background_shorthand = struct
  let read_image_item t =
    (* A single image per layer: commas in the [background] shorthand separate
       layers, not images (that comma-list is the [background-image]
       longhand). *)
    let img = read_bg_image t in
    fun (bg : background_shorthand) ->
      if bg.image = None then { bg with image = Some img } else bg

  let read_position_size_item t =
    let pos = read_background_position_value t in
    Cursor.ws t;
    let size_opt =
      if Cursor.slash_opt t then Some (read_background_size t) else None
    in
    fun (bg : background_shorthand) ->
      if bg.position <> None then bg
      else
        let bg' = { bg with position = Some pos } in
        match size_opt with
        | Some s when bg'.size = None -> { bg' with size = Some s }
        | _ -> bg'

  let read_repeat_item t =
    let rep = read_background_repeat t in
    fun (bg : background_shorthand) ->
      if bg.repeat = None then { bg with repeat = Some rep } else bg

  let read_attachment_item t =
    let att = read_background_attachment t in
    fun (bg : background_shorthand) ->
      if bg.attachment = None then { bg with attachment = Some att } else bg

  let read_box_item t =
    let box = read_background_box t in
    fun (bg : background_shorthand) ->
      if bg.origin = None then { bg with origin = Some box }
      else if bg.clip = None then { bg with clip = Some box }
      else bg

  let read_color_item t =
    let col = read_color t in
    fun (bg : background_shorthand) ->
      if bg.color = None then { bg with color = Some col } else bg

  let read_item t =
    Cursor.one_of
      [
        read_image_item;
        read_position_size_item;
        read_repeat_item;
        read_attachment_item;
        read_box_item;
        read_color_item;
      ]
      t
end

let read_background_shorthand t : background_shorthand =
  Cursor.ws t;
  let init =
    {
      color = None;
      image = None;
      position = None;
      size = None;
      repeat = None;
      attachment = None;
      clip = None;
      origin = None;
    }
  in
  let apply acc upd =
    let new_acc = upd acc in
    (* Check if the update actually changed anything *)
    if equal_background_shorthand new_acc acc then
      (* Nothing changed, meaning we tried to set a duplicate property *)
      Cursor.err t "Duplicate property in background shorthand"
    else new_acc
  in
  let acc, _ =
    Cursor.fold_many Background_shorthand.read_item ~init ~f:apply t
  in
  if equal_background_shorthand acc init then
    Cursor.err_expected t "background value";
  acc

let read_background_vars read_self t =
  let rec loop acc =
    Cursor.ws t;
    if Cursor.looking_at_func "var" t then loop (read_var read_self t :: acc)
    else List.rev acc
  in
  loop []

let read_background_var_sequence read_self t : background =
  let snap = Cursor.save t in
  match read_background_vars read_self t with
  | _ :: _ :: _ as vars -> Vars vars
  | _ ->
      Cursor.restore t snap;
      Cursor.err_expected t "background var() sequence"

let background_keyword_value ident : background option =
  match String.lowercase_ascii ident with
  | "inherit" -> Some Inherit
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "none" -> Some None
  | _ -> None

let background_value_boundary t =
  Cursor.ws t;
  Cursor.is_done t || Cursor.peek_comma t

let read_background_shorthand_from t snap : background =
  Cursor.restore t snap;
  Shorthand (read_background_shorthand t)

(* CSS Backgrounds 3 (ED) sec. 2.1 orders a layer's slots rather than the
   author's words, so pp writes the image before the colour and [background: red
   var(--x)] comes out as [var(--x)red]. A leading var() therefore only stands
   for the whole value when it reaches the value boundary; with a slot behind it
   the layer is what was written, and reading it as the whole value would leave
   pp's own emission unreadable. *)
let read_background_var_call read_self t : background =
  let snap = Cursor.save t in
  let first = read_var read_self t in
  let rest = read_background_vars read_self t in
  if not (background_value_boundary t) then
    read_background_shorthand_from t snap
  else match rest with [] -> Var first | rest -> Vars (first :: rest)

let read_background_keyword_or_shorthand t : background =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match background_keyword_value ident with
      | Some value when background_value_boundary t -> value
      | _ -> read_background_shorthand_from t snap)
  | None -> read_background_shorthand_from t snap

let read_background_default read_self t =
  Cursor.one_of
    [
      read_background_var_sequence read_self;
      read_background_keyword_or_shorthand;
    ]
    t

let rec read_background t : background =
  Cursor.enum_or_calls "background"
    [ ("inherit", Inherit); ("initial", Initial); ("unset", Unset) ]
    ~calls:[ ("var", read_background_var_call read_background) ]
    ~default:(read_background_default read_background)
    t

(* CSS Backgrounds 3 sec. 2.1 gives the [background] shorthand a [<bg-layer># ,
   <final-bg-layer>]: only the LAST layer carries a [<background-color>],
   because the colour paints once behind every layer rather than per layer. A
   colour in an earlier one fills no slot. *)
let read_backgrounds t : background list =
  let layers = Cursor.list ~sep:Cursor.comma ~at_least:1 read_background t in
  let count = List.length layers in
  List.iteri
    (fun i (layer : background) ->
      match layer with
      | Shorthand { color = Some _; _ } when i < count - 1 ->
          Cursor.err_invalid t "only the final background layer takes a colour"
      | _ -> ())
    layers;
  layers
