(* css-masking-1: the [mask] shorthand and its longhands (plus the -webkit-
   prefixed aliases), [clip-path] with its <basic-shape> functions, and the
   deprecated [clip] property.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image
open Prop_background

let normalize_clip_path_extent v =
  match v with
  | Extent_length l ->
      let l' = Values.normalize_length ~strip:false l in
      if l' == l then v else Extent_length l'
  | other -> other

let drop_default_clip_path_position (opt : position_value option) =
  match opt with
  | Option.Some p when position_value_is_center p -> Option.None
  | _ -> opt

let drop_default_clip_path_extent (opt : clip_path_extent option) =
  match opt with Option.Some Closest_side -> Option.None | _ -> opt

let normalize_clip_path_inset value =
  match value with
  | Clip_path_inset r ->
      (* The four inset slots and the rounded clause are plain
         [<length-percentage>] / [<border-radius>] positions, not [calc()]
         operands, so a zero length can drop its unit per CSS Values L4 sec.
         6.1.1. *)
      let lp = Values.normalize_length_percentage ~strip:true in
      let top = lp r.top in
      let right = option_map_preserve lp r.right in
      let bottom = option_map_preserve lp r.bottom in
      let left = option_map_preserve lp r.left in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        top == r.top && right == r.right && bottom == r.bottom && left == r.left
        && rounded == r.rounded
      then value
      else Clip_path_inset { top; right; bottom; left; rounded }
  | _ -> value

let normalize_clip_path_xywh value =
  match value with
  | Clip_path_xywh r ->
      (* xywh slots are plain [<length-percentage>] / [<border-radius>], not
         calc operands - zero lengths can drop the unit. *)
      let lp = Values.normalize_length_percentage ~strip:true in
      let x = lp r.x in
      let y = lp r.y in
      let width = lp r.width in
      let height = lp r.height in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        x == r.x && y == r.y && width == r.width && height == r.height
        && rounded == r.rounded
      then value
      else Clip_path_xywh { x; y; width; height; rounded }
  | _ -> value

let normalize_clip_path_rect value =
  match value with
  | Clip_path_rect r ->
      (* rect() slots are plain [<length-percentage>] / [<border-radius>], not
         calc operands - zero lengths can drop the unit. *)
      let lp = Values.normalize_length_percentage ~strip:true in
      let top = lp r.top in
      let right = lp r.right in
      let bottom = lp r.bottom in
      let left = lp r.left in
      let rounded =
        option_map_preserve (normalize_border_radius ~strip:true) r.rounded
      in
      if
        top == r.top && right == r.right && bottom == r.bottom && left == r.left
        && rounded == r.rounded
      then value
      else Clip_path_rect { top; right; bottom; left; rounded }
  | _ -> value

let normalize_clip_path_circle value =
  match value with
  | Clip_path_circle r ->
      let radius =
        option_map_preserve normalize_clip_path_extent r.radius
        |> drop_default_clip_path_extent
      in
      let position =
        option_map_preserve (normalize_position_value ~strip:false) r.position
        |> drop_default_clip_path_position
      in
      if
        option_is_phys_same radius r.radius
        && option_is_phys_same position r.position
      then value
      else Clip_path_circle { radius; position }
  | _ -> value

let normalize_clip_path_ellipse value =
  match value with
  | Clip_path_ellipse r ->
      let rx =
        option_map_preserve normalize_clip_path_extent r.rx
        |> drop_default_clip_path_extent
      in
      let ry =
        option_map_preserve normalize_clip_path_extent r.ry
        |> drop_default_clip_path_extent
      in
      let position =
        option_map_preserve (normalize_position_value ~strip:false) r.position
        |> drop_default_clip_path_position
      in
      if
        option_is_phys_same rx r.rx
        && option_is_phys_same ry r.ry
        && option_is_phys_same position r.position
      then value
      else Clip_path_ellipse { rx; ry; position }
  | _ -> value

let normalize_clip_path_polygon value =
  match value with
  | Clip_path_polygon r ->
      (* polygon vertex coordinates are plain [<length-percentage>], not calc
         operands - zero lengths can drop the unit. *)
      let len = Values.normalize_length ~strip:true in
      let normalize_point (x, y) =
        let x' = len x in
        let y' = len y in
        if x' == x && y' == y then (x, y) else (x', y')
      in
      let points = map_preserve normalize_point r.points in
      if points == r.points then value else Clip_path_polygon { r with points }
  | _ -> value

let rec normalize_clip_path : clip_path -> clip_path =
 fun value ->
  match value with
  | Clip_path_inset _ -> normalize_clip_path_inset value
  | Clip_path_xywh _ -> normalize_clip_path_xywh value
  | Clip_path_rect _ -> normalize_clip_path_rect value
  | Clip_path_circle _ -> normalize_clip_path_circle value
  | Clip_path_ellipse _ -> normalize_clip_path_ellipse value
  | Clip_path_polygon _ -> normalize_clip_path_polygon value
  | Clip_path_with_box r ->
      let shape = normalize_clip_path r.shape in
      if shape == r.shape then value else Clip_path_with_box { r with shape }
  | other -> other

(* CSS Masking 1 sec. 7.9: a component the shorthand leaves out takes its
   longhand's initial - [none] for the image (sec. 7.1), [0% 0%] for the
   position (sec. 7.4), [auto] for the size (sec. 7.5), [repeat] for the repeat
   (sec. 7.3), [border-box] for the origin and the clip (sec. 7.6 and 7.7),
   [match-source] for the mode (sec. 7.2) and [add] for the composite (sec. 7.8)
   - so writing one out names what leaving it out names. *)
let drop_mask_initial : 'a. 'a -> 'a option -> 'a option =
 fun initial opt ->
  match opt with Some x when x = initial -> (None : _ option) | _ -> opt

let mask_layer_slots_shared (a : mask_layer) (b : mask_layer) =
  a.image == b.image && a.position == b.position && a.size == b.size
  && a.repeat == b.repeat && a.origin == b.origin && a.clip == b.clip
  && a.mode == b.mode && a.composite == b.composite

let normalize_mask_layer ?(lossless = false) (l : mask_layer) =
  let image =
    option_map_preserve
      (normalize_background_image ~lossless)
      (drop_mask_initial (None : background_image) l.image)
  in
  let position = option_map_preserve normalize_position_value l.position in
  let size = drop_mask_initial (Auto : background_size) l.size in
  (* The size follows the position and a [/], so the position can only go once
     the size has. *)
  let position =
    match position with
    | Some p when Option.is_none size && p = (XY (Zero, Zero) : position_value)
      ->
        (None : position_value option)
    | _ -> position
  in
  let repeat = drop_mask_initial (Repeat : background_repeat) l.repeat in
  let origin = drop_mask_initial (Border_box : mask_box) l.origin in
  let clip = drop_mask_initial (Border_box : mask_box) l.clip in
  let mode = drop_mask_initial (Match_source : mask_mode) l.mode in
  let composite = drop_mask_initial (Add : mask_composite) l.composite in
  let l' = { image; position; size; repeat; origin; clip; mode; composite } in
  if mask_layer_slots_shared l l' then l else l'

let normalize_mask ?(lossless = false) : mask -> mask =
 fun value ->
  match value with
  | Layer l ->
      let l' = normalize_mask_layer ~lossless l in
      if l' == l then value else Layer l'
  | Layers ls ->
      let ls' = map_preserve (normalize_mask_layer ~lossless) ls in
      if ls' == ls then value else Layers ls'
  | other -> other

let rec pp_clip : clip Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_clip ctx v
  | Clip_auto -> Pp.string ctx "auto"
  | Clip_rect (top, right, bottom, left) ->
      Pp.string ctx "rect(";
      pp_length ctx top;
      Pp.char ctx ',';
      pp_length ctx right;
      Pp.char ctx ',';
      pp_length ctx bottom;
      Pp.char ctx ',';
      pp_length ctx left;
      Pp.char ctx ')'
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_clip_geometry_box ctx = function
  | (Margin_box : clip_geometry_box) -> Pp.string ctx "margin-box"
  | Border_box -> Pp.string ctx "border-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Content_box -> Pp.string ctx "content-box"
  | Fill_box -> Pp.string ctx "fill-box"
  | Stroke_box -> Pp.string ctx "stroke-box"
  | View_box -> Pp.string ctx "view-box"

let pp_clip_path_extent ctx = function
  | (Extent_length l : clip_path_extent) -> pp_length ctx l
  | Closest_side -> Pp.string ctx "closest-side"
  | Farthest_side -> Pp.string ctx "farthest-side"

let pp_clip_path_fill_rule ctx = function
  | (Nonzero : clip_path_fill_rule) -> Pp.string ctx "nonzero"
  | Evenodd -> Pp.string ctx "evenodd"

let pp_clip_path_circle_args ctx radius position =
  let printed = ref false in
  Option.iter
    (fun r ->
      pp_clip_path_extent ctx r;
      printed := true)
    radius;
  Option.iter
    (fun p ->
      if !printed then Pp.space ctx ();
      Pp.string ctx "at ";
      pp_position_value ctx p;
      printed := true)
    position;
  ignore !printed

let pp_clip_path_ellipse_args ctx rx ry position =
  let printed = ref false in
  (match (rx, ry) with
  | Some rx, Some ry ->
      pp_clip_path_extent ctx rx;
      Pp.space ctx ();
      pp_clip_path_extent ctx ry;
      printed := true
  | Some rx, None ->
      pp_clip_path_extent ctx rx;
      printed := true
  | None, Some ry ->
      pp_clip_path_extent ctx ry;
      printed := true
  | None, None -> ());
  Option.iter
    (fun p ->
      if !printed then Pp.space ctx ();
      Pp.string ctx "at ";
      pp_position_value ctx p;
      printed := true)
    position;
  ignore !printed

let pp_clip_path_inset_quad ctx a b c d =
  pp_length_percentage ~always:true ctx a;
  Pp.space ctx ();
  pp_length_percentage ~always:true ctx b;
  Pp.space ctx ();
  pp_length_percentage ~always:true ctx c;
  Pp.space ctx ();
  pp_length_percentage ~always:true ctx d

let pp_clip_path_round ctx (rounded : border_radius option) =
  match rounded with
  | None -> ()
  | Some r ->
      Pp.space ctx ();
      Pp.string ctx "round";
      Pp.space ctx ();
      pp_border_radius ctx r

let minified_clip_path_fill_rule ctx (fill_rule : clip_path_fill_rule option) :
    clip_path_fill_rule option =
  match fill_rule with
  | Some Nonzero when Pp.minified ctx -> None
  | other -> other

let pp_clip_path_polygon_rule ctx fill_rule points =
  Option.iter
    (fun rule ->
      pp_clip_path_fill_rule ctx rule;
      if points <> [] then (
        Pp.char ctx ',';
        if not (Pp.minified ctx) then Pp.space ctx ()))
    (minified_clip_path_fill_rule ctx fill_rule)

let pp_clip_path_polygon_sep spaced ctx () =
  Pp.char ctx ',';
  if spaced && not (Pp.minified ctx) then Pp.space ctx ()

let pp_clip_path_axis_pair ctx (x, y) =
  pp_length ctx x;
  Pp.token_sp ctx ();
  pp_length ctx y

let pp_clip_path_polygon_body ctx fill_rule points spaced =
  pp_clip_path_polygon_rule ctx fill_rule points;
  Pp.list
    ~sep:(pp_clip_path_polygon_sep spaced)
    pp_clip_path_axis_pair ctx points

let pp_optional_inset_sides : type a.
    a Pp.t -> Pp.ctx -> a option -> a option -> a option -> unit =
 fun pp ctx right bottom left ->
  Option.iter
    (fun r ->
      Pp.token_sp ctx ();
      pp ctx r;
      Option.iter
        (fun b ->
          Pp.token_sp ctx ();
          pp ctx b;
          Option.iter
            (fun l ->
              Pp.token_sp ctx ();
              pp ctx l)
            left)
        bottom)
    right

let rec pp_clip_path : clip_path Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_clip_path ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Invalid tokens ->
      let rendered =
        if Pp.minified ctx then Parser.to_string_minified tokens
        else Parser.string_of_components tokens
      in
      Pp.string ctx rendered
  | Clip_path_none -> Pp.string ctx "none"
  | Clip_path_url url ->
      Pp.string ctx "url(";
      Pp.string ctx url;
      Pp.char ctx ')'
  | Clip_path_inset { top; right; bottom; left; rounded } ->
      Pp.string ctx "inset(";
      pp_length_percentage ctx top;
      pp_optional_inset_sides pp_length_percentage ctx right bottom left;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Clip_path_circle { radius; position } ->
      Pp.string ctx "circle(";
      pp_clip_path_circle_args ctx radius position;
      Pp.char ctx ')'
  | Clip_path_ellipse { rx; ry; position } ->
      Pp.string ctx "ellipse(";
      pp_clip_path_ellipse_args ctx rx ry position;
      Pp.char ctx ')'
  | Clip_path_polygon { fill_rule; points; spaced } ->
      Pp.string ctx "polygon(";
      pp_clip_path_polygon_body ctx fill_rule points spaced;
      Pp.char ctx ')'
  | Clip_path_path d ->
      Pp.string ctx "path(\"";
      Pp.string ctx d;
      Pp.string ctx "\")"
  | Clip_path_shape body ->
      Pp.string ctx "shape(";
      Pp.string ctx body;
      Pp.char ctx ')'
  | Clip_path_box box -> pp_clip_geometry_box ctx box
  | Clip_path_with_box { shape; box; box_first } ->
      if box_first then (
        pp_clip_geometry_box ctx box;
        Pp.space ctx ();
        pp_clip_path ctx shape)
      else (
        pp_clip_path ctx shape;
        Pp.space ctx ();
        pp_clip_geometry_box ctx box)
  | Clip_path_xywh { x; y; width; height; rounded } ->
      Pp.string ctx "xywh(";
      pp_clip_path_inset_quad ctx x y width height;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'
  | Clip_path_rect { top; right; bottom; left; rounded } ->
      Pp.string ctx "rect(";
      pp_clip_path_inset_quad ctx top right bottom left;
      pp_clip_path_round ctx rounded;
      Pp.char ctx ')'

let rec pp_webkit_mask_composite : webkit_mask_composite Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_mask_composite ctx v
  | Composites composites ->
      Pp.list ~sep:Pp.comma pp_webkit_mask_composite ctx composites
  | Source_over -> Pp.string ctx "source-over"
  | Xor -> Pp.string ctx "xor"
  | Source_in -> Pp.string ctx "source-in"
  | Source_out -> Pp.string ctx "source-out"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_composite : mask_composite Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_composite ctx v
  | Composites composites ->
      Pp.list ~sep:Pp.comma pp_mask_composite ctx composites
  | Add -> Pp.string ctx "add"
  | Subtract -> Pp.string ctx "subtract"
  | Intersect -> Pp.string ctx "intersect"
  | Exclude -> Pp.string ctx "exclude"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_webkit_mask_source_type : webkit_mask_source_type Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_webkit_mask_source_type ctx v
  | Alpha -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"
  | Auto -> Pp.string ctx "auto"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_mode : mask_mode Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_mode ctx v
  | Alpha -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"
  | Match_source -> Pp.string ctx "match-source"
  | Modes modes -> Pp.list ~sep:Pp.comma pp_mask_mode ctx modes
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_type : mask_type Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_type ctx v
  | Alpha -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_mask_box : mask_box Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_mask_box ctx v
  | Layers layers -> Pp.list ~sep:Pp.comma pp_mask_box ctx layers
  | Border_box -> Pp.string ctx "border-box"
  | Content_box -> Pp.string ctx "content-box"
  | Fill_box -> Pp.string ctx "fill-box"
  | Padding_box -> Pp.string ctx "padding-box"
  | Stroke_box -> Pp.string ctx "stroke-box"
  | View_box -> Pp.string ctx "view-box"
  | No_clip -> Pp.string ctx "no-clip"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_mask_layer : mask_layer Pp.t =
 fun ctx layer ->
  let first = ref true in
  (* CSS Syntax 3 (ED) sec. 9: a token ending with [)], [\]] or [}] is
     self-delimiting, so under minify we can drop the inter-slot space after
     [url(...)] / [<image>]. *)
  let last_is_self_delim () =
    match Pp.last_char ctx with Some (')' | ']' | '}') -> true | _ -> false
  in
  let maybe_space () =
    if !first then first := false
    else if Pp.minified ctx && last_is_self_delim () then ()
    else Pp.space ctx ()
  in
  pp_bg_prop maybe_space pp_background_image ctx layer.image;
  (match (layer.position, layer.size) with
  | Some position, Some size ->
      maybe_space ();
      pp_position_value ctx position;
      Pp.string ctx "/";
      pp_background_size ctx size
  | Some position, None ->
      maybe_space ();
      pp_position_value ctx position
  | None, Some size ->
      (* A [<bg-size>] is only reachable after [<position> /]; emit the initial
         position [0 0] so the layer round-trips. *)
      maybe_space ();
      Pp.string ctx "0 0/";
      pp_background_size ctx size
  | None, None -> ());
  pp_bg_prop maybe_space pp_background_repeat ctx layer.repeat;
  pp_bg_prop maybe_space pp_mask_box ctx layer.origin;
  pp_bg_prop maybe_space pp_mask_box ctx layer.clip;
  pp_bg_prop maybe_space pp_mask_mode ctx layer.mode;
  pp_bg_prop maybe_space pp_mask_composite ctx layer.composite

let rec pp_mask : mask Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Layer layer -> pp_mask_layer ctx layer
  | Layers layers -> Pp.list ~sep:Pp.comma pp_mask_layer ctx layers
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_mask ctx v

(* Parser for webkit_mask_composite values *)
let rec read_webkit_mask_composite t : webkit_mask_composite =
  let read_item t =
    Cursor.enum "webkit-mask-composite-item"
      [
        ("source-over", (Source_over : webkit_mask_composite));
        ("xor", Xor);
        ("source-in", Source_in);
        ("source-out", Source_out);
      ]
      t
  in
  Cursor.enum_or_var "webkit-mask-composite"
    [
      ("inherit", (Inherit : webkit_mask_composite));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_mask_composite t))
    ~default:(fun t ->
      let composites = Cursor.list ~sep:Cursor.comma ~at_least:1 read_item t in
      match composites with
      | [ composite ] -> composite
      | composites -> Composites composites)
    t

(* Parser for mask_composite values (standard, not webkit) *)
let rec read_mask_composite t : mask_composite =
  Cursor.enum_or_var "mask-composite"
    [
      ("add", (Add : mask_composite));
      ("subtract", Subtract);
      ("intersect", Intersect);
      ("exclude", Exclude);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask_composite t))
    t

let read_mask_composite_list t : mask_composite =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_mask_composite t with
  | [ one ] -> one
  | many -> Composites many

(* Parser for webkit_mask_source_type values *)
let rec read_webkit_mask_source_type t : webkit_mask_source_type =
  Cursor.enum_or_var "webkit-mask-source-type"
    [
      ("alpha", (Alpha : webkit_mask_source_type));
      ("luminance", Luminance);
      ("auto", Auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_webkit_mask_source_type t))
    t

(* Parser for mask_mode values (standard) *)
let rec read_mask_mode t : mask_mode =
  let read_single t =
    Cursor.enum "mask-mode"
      [
        ("alpha", (Alpha : mask_mode));
        ("luminance", Luminance);
        ("match-source", Match_source);
      ]
      t
  in
  let read_modes t =
    let modes = Cursor.list ~sep:Cursor.comma ~at_least:1 read_single t in
    match modes with [ mode ] -> mode | _ -> Modes modes
  in
  Cursor.enum_or_var "mask-mode"
    [
      ("initial", (Initial : mask_mode));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_mask_mode t) : mask_mode))
    ~default:read_modes t

(* Parser for mask_type values *)
let rec read_mask_type t : mask_type =
  Cursor.enum_or_var "mask-type"
    [
      ("alpha", (Alpha : mask_type));
      ("luminance", Luminance);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask_type t))
    t

(* Parser for mask_box values (mask-clip and mask-origin) *)
let rec read_mask_box t : mask_box =
  Cursor.enum_or_var "mask-box"
    [
      ("border-box", (Border_box : mask_box));
      ("content-box", Content_box);
      ("fill-box", Fill_box);
      ("padding-box", Padding_box);
      ("stroke-box", Stroke_box);
      ("view-box", View_box);
      ("no-clip", No_clip);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask_box t))
    t

let read_mask_box_list t : mask_box =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_mask_box t with
  | [ one ] -> one
  | many -> Layers many

module Mask_shorthand = struct
  let init =
    {
      image = None;
      position = None;
      size = None;
      repeat = None;
      origin = None;
      clip = None;
      mode = None;
      composite = None;
    }

  let read_image_item t =
    let image = read_background_image t in
    fun (mask : mask_layer) ->
      if mask.image = None then { mask with image = Some image } else mask

  let read_position_size_item t =
    let position = read_position_value t in
    Cursor.ws t;
    let size =
      if Cursor.slash_opt t then Some (read_background_size t) else None
    in
    fun (mask : mask_layer) ->
      if mask.position <> None then mask
      else
        let mask = { mask with position = Some position } in
        match size with
        | Some size when mask.size = None -> { mask with size = Some size }
        | _ -> mask

  let read_repeat_item t =
    let repeat = read_background_repeat t in
    fun (mask : mask_layer) ->
      if mask.repeat = None then { mask with repeat = Some repeat } else mask

  let read_box_item t =
    let box = read_mask_box t in
    fun (mask : mask_layer) ->
      if mask.origin = None then { mask with origin = Some box }
      else if mask.clip = None then { mask with clip = Some box }
      else mask

  let read_mode_item t =
    let mode = read_mask_mode t in
    fun (mask : mask_layer) ->
      if mask.mode = None then { mask with mode = Some mode } else mask

  let read_composite_item t =
    let composite = read_mask_composite t in
    fun (mask : mask_layer) ->
      if mask.composite = None then { mask with composite = Some composite }
      else mask

  let read_item t =
    Cursor.one_of
      [
        read_image_item;
        read_position_size_item;
        read_repeat_item;
        read_box_item;
        read_mode_item;
        read_composite_item;
      ]
      t
end

let read_mask_layer t : mask_layer =
  let apply acc upd =
    let next = upd acc in
    if equal_mask_layer next acc then
      Cursor.err t "Duplicate property in mask shorthand"
    else next
  in
  let layer, _ =
    Cursor.fold_many Mask_shorthand.read_item ~init:Mask_shorthand.init ~f:apply
      t
  in
  if equal_mask_layer layer Mask_shorthand.init then
    Cursor.err_expected t "mask value";
  layer

let rec read_mask t : mask =
  Cursor.enum_or_var "mask"
    [
      ("none", (None : mask));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_mask t))
    ~default:(fun t ->
      let layers =
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_mask_layer t
      in
      match layers with [ layer ] -> Layer layer | layers -> Layers layers)
    t

(* Reader for clip property (deprecated) *)
let rec read_clip t : clip =
  Cursor.ws t;
  Cursor.enum_or_var "clip"
    [
      ("auto", Clip_auto);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_clip t) : clip))
    ~default:(fun t ->
      Cursor.call "rect" t @@ fun inner ->
      Cursor.ws inner;
      let top = read_length inner in
      Cursor.ws inner;
      ignore (Cursor.comma_opt inner : bool);
      Cursor.ws inner;
      let right = read_length inner in
      Cursor.ws inner;
      ignore (Cursor.comma_opt inner : bool);
      Cursor.ws inner;
      let bottom = read_length inner in
      Cursor.ws inner;
      ignore (Cursor.comma_opt inner : bool);
      Cursor.ws inner;
      let left = read_length inner in
      Clip_rect (top, right, bottom, left))
    t

let read_clip_path_round t : border_radius option =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "round" ->
      let _ = Cursor.ident t in
      Cursor.ws t;
      Some (read_border_radius_inline t)
  | _ -> None

let read_clip_path_inset_side t : length_percentage option =
  Cursor.ws t;
  match (Cursor.is_done t, Cursor.peek_ident t) with
  | true, _ | _, Some "round" -> None
  | false, _ -> Some (read_length_percentage t)

let read_clip_path_inset t =
  Cursor.call "inset" t (fun t ->
      Cursor.ws t;
      let top = read_length_percentage t in
      let right = read_clip_path_inset_side t in
      let bottom = Option.bind right (fun _ -> read_clip_path_inset_side t) in
      let left = Option.bind bottom (fun _ -> read_clip_path_inset_side t) in
      let rounded = read_clip_path_round t in
      Cursor.ws t;
      Cursor.expect_eof t;
      Clip_path_inset { top; right; bottom; left; rounded })

let read_clip_path_extent inner : clip_path_extent =
  match Cursor.peek_ident inner with
  | Some "closest-side" ->
      Cursor.skip inner;
      Closest_side
  | Some "farthest-side" ->
      Cursor.skip inner;
      Farthest_side
  | _ -> Extent_length (read_length inner)

let read_clip_path_fill_rule t =
  Cursor.enum "clip-path fill-rule"
    [ ("nonzero", (Nonzero : clip_path_fill_rule)); ("evenodd", Evenodd) ]
    t

let read_clip_path_position_clause inner =
  Cursor.ws inner;
  match Cursor.peek_ident inner with
  | Some "at" ->
      Cursor.skip inner;
      Cursor.ws inner;
      Some (read_position_value inner)
  | _ -> None

(* Wrap a typed [<basic-shape>] reader so it falls back to [Invalid
   [<original-call>]] when the typed reduction refuses an admittedly-invalid
   input that upstream tools preserve verbatim. *)
let with_basic_shape_fallback t f : clip_path =
  match Cursor.try_typed_call f t with
  | Ok value -> value
  | Error comp -> (Invalid [ comp ] : clip_path)

let read_clip_path_circle t : clip_path =
  with_basic_shape_fallback t (fun t ->
      Cursor.call "circle" t @@ fun inner ->
      Cursor.ws inner;
      let radius : clip_path_extent option =
        if Cursor.is_done inner || Cursor.peek_ident inner = Some "at" then None
        else Some (read_clip_path_extent inner)
      in
      let position = read_clip_path_position_clause inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Clip_path_circle { radius; position })

let read_clip_path_ellipse t : clip_path =
  with_basic_shape_fallback t (fun t ->
      Cursor.call "ellipse" t @@ fun inner ->
      Cursor.ws inner;
      let rx : clip_path_extent option =
        if Cursor.is_done inner || Cursor.peek_ident inner = Some "at" then None
        else Some (read_clip_path_extent inner)
      in
      Cursor.ws inner;
      let ry : clip_path_extent option =
        if
          Option.is_none rx || Cursor.is_done inner
          || Cursor.peek_ident inner = Some "at"
        then None
        else Some (read_clip_path_extent inner)
      in
      let position = read_clip_path_position_clause inner in
      Cursor.ws inner;
      Cursor.expect_eof inner;
      Clip_path_ellipse { rx; ry; position })

let read_polygon_fill_rule inner =
  match Cursor.peek_ident inner with
  | Some "nonzero" ->
      Cursor.skip inner;
      Cursor.ws inner;
      Cursor.comma inner;
      Some Nonzero
  | Some "evenodd" ->
      Cursor.skip inner;
      Cursor.ws inner;
      Cursor.comma inner;
      Some Evenodd
  | _ -> None

let read_polygon_point inner =
  let x = read_length inner in
  Cursor.ws inner;
  let y = read_length inner in
  (x, y)

let polygon_has_ws_after_comma inner =
  match Cursor.peek_raw inner with
  | Some (Component.Preserved { kind = Token.Whitespace; _ }) -> true
  | _ -> false

let read_polygon_points inner =
  let rec loop acc spaced =
    Cursor.ws inner;
    if Cursor.comma_opt inner then (
      let spaced = spaced || polygon_has_ws_after_comma inner in
      Cursor.ws inner;
      loop (read_polygon_point inner :: acc) spaced)
    else (List.rev acc, spaced)
  in
  loop [ read_polygon_point inner ] false

let read_clip_path_polygon t : clip_path =
  Cursor.call "polygon" t @@ fun inner ->
  Cursor.ws inner;
  let fill_rule = read_polygon_fill_rule inner in
  Cursor.ws inner;
  let points, spaced = read_polygon_points inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Clip_path_polygon { fill_rule; points; spaced }

let read_clip_path_path t =
  Cursor.call "path" t @@ fun inner ->
  Cursor.ws inner;
  let data = Cursor.string inner in
  Cursor.ws inner;
  Cursor.expect_eof inner;
  Clip_path_path data

(* Read 4 length-percentages and an optional [round <border-radius>] suffix
   shared by xywh() and rect(). *)
let read_clip_path_inset_quad t =
  let a = read_length_percentage t in
  Cursor.ws t;
  let b = read_length_percentage t in
  Cursor.ws t;
  let c = read_length_percentage t in
  Cursor.ws t;
  let d = read_length_percentage t in
  (a, b, c, d)

let read_clip_path_xywh t =
  Cursor.call "xywh" t (fun inner ->
      Cursor.ws inner;
      let x, y, width, height = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      Clip_path_xywh { x; y; width; height; rounded })

let read_clip_path_rect t =
  Cursor.call "rect" t (fun inner ->
      Cursor.ws inner;
      let top, right, bottom, left = read_clip_path_inset_quad inner in
      let rounded = read_clip_path_round inner in
      Clip_path_rect { top; right; bottom; left; rounded })

let read_clip_path_shape t =
  Cursor.call "shape" t (fun inner ->
      Clip_path_shape (Cursor.consume_remaining_as_string ~trim:true inner))

let read_clip_geometry_box_opt t : clip_geometry_box option =
  match Cursor.peek_ident t with
  | Some "margin-box" ->
      Cursor.skip t;
      Some Margin_box
  | Some "border-box" ->
      Cursor.skip t;
      Some Border_box
  | Some "padding-box" ->
      Cursor.skip t;
      Some Padding_box
  | Some "content-box" ->
      Cursor.skip t;
      Some Content_box
  | Some "fill-box" ->
      Cursor.skip t;
      Some Fill_box
  | Some "stroke-box" ->
      Cursor.skip t;
      Some Stroke_box
  | Some "view-box" ->
      Cursor.skip t;
      Some View_box
  | _ -> None

let read_clip_geometry_box t =
  match read_clip_geometry_box_opt t with
  | Some box -> box
  | None -> Cursor.err_expected t "clip geometry box"

let rec read_clip_path (t : Cursor.t) : clip_path =
  Cursor.ws t;
  let read_basic_shape t : clip_path =
    Cursor.one_of
      [
        (fun t -> Clip_path_url (Cursor.url t));
        (fun t ->
          Cursor.enum_or_calls "clip-path"
            [
              ("none", Clip_path_none);
              ("inherit", Inherit);
              ("initial", Initial);
              ("unset", Unset);
              ("revert", Revert);
              ("revert-layer", Revert_layer);
            ]
            ~calls:
              [
                ("inset", read_clip_path_inset);
                ("circle", read_clip_path_circle);
                ("ellipse", read_clip_path_ellipse);
                ("polygon", read_clip_path_polygon);
                ("path", read_clip_path_path);
                ("xywh", read_clip_path_xywh);
                ("rect", read_clip_path_rect);
                ("shape", read_clip_path_shape);
                ( "var",
                  fun t -> (Var (Values.read_var read_clip_path t) : clip_path)
                );
              ]
            t);
      ]
      t
  in
  (* CSS Masking 1 sec. 5.1 [<basic-shape> || <geometry-box>]: a shape and a
     reference box may appear in either order, or just a box on its own. *)
  match read_clip_geometry_box_opt t with
  | Some box ->
      Cursor.ws t;
      if Cursor.is_done t then Clip_path_box box
      else
        let shape = read_basic_shape t in
        Clip_path_with_box { shape; box; box_first = true }
  | None -> (
      let shape = read_basic_shape t in
      Cursor.ws t;
      if Cursor.is_done t then shape
      else
        match read_clip_geometry_box_opt t with
        | Some box -> Clip_path_with_box { shape; box; box_first = false }
        | None -> shape)
