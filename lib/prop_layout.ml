(* css-contain-3, css-anchor-position-1, css-scroll-snap-1 and css-overscroll-1:
   [contain] and [contain-intrinsic-*], the container query properties,
   [content-visibility], [will-change], [anchor-name], [position-anchor] /
   [position-area] / [position-try*] / [position-visibility], [scroll-behavior],
   the scroll-snap properties, [overscroll-behavior] and [margin-trim].

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common

let rec pp_will_change : will_change Pp.t =
 fun ctx -> function
  | Will_change_auto -> Pp.string ctx "auto"
  | Scroll_position -> Pp.string ctx "scroll-position"
  | Contents -> Pp.string ctx "contents"
  | Transform -> Pp.string ctx "transform"
  | Opacity -> Pp.string ctx "opacity"
  | Properties props -> Pp.list ~sep:Pp.comma pp_ident ctx props
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_will_change ctx v

let rec pp_contain : contain Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Strict -> Pp.string ctx "strict"
  | Content -> Pp.string ctx "content"
  | Size -> Pp.string ctx "size"
  | Layout -> Pp.string ctx "layout"
  | Style -> Pp.string ctx "style"
  | Paint -> Pp.string ctx "paint"
  | Inline_size -> Pp.string ctx "inline-size"
  | List items -> Pp.list ~sep:Pp.space pp_contain ctx items
  | Var v -> pp_var pp_contain ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_container_type : container_type Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_container_type ctx v
  | Normal -> Pp.string ctx "normal"
  | Size -> Pp.string ctx "size"
  | Inline_size -> Pp.string ctx "inline-size"
  | Scroll_state -> Pp.string ctx "scroll-state"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_container_name : container_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_container_name ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.space pp_ident ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_anchor_name : anchor_name Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_anchor_name ctx v
  | None -> Pp.string ctx "none"
  | Names names -> Pp.list ~sep:Pp.comma pp_ident ctx names
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_position_anchor : position_anchor Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_anchor ctx v
  | Auto -> Pp.string ctx "auto"
  | Anchor name -> pp_ident ctx name
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_position_try_fallback : position_try_fallback Pp.t =
 fun ctx -> function
  | Flip_block -> Pp.string ctx "flip-block"
  | Flip_inline -> Pp.string ctx "flip-inline"
  | Flip_start -> Pp.string ctx "flip-start"
  | Name name -> pp_ident ctx name

let rec pp_position_try_fallbacks : position_try_fallbacks Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_try_fallbacks ctx v
  | None -> Pp.string ctx "none"
  | Fallbacks fallbacks ->
      Pp.list ~sep:Pp.comma
        (Pp.list ~sep:Pp.space pp_position_try_fallback)
        ctx fallbacks
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_position_try_order : position_try_order Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_try_order ctx v
  | Normal -> Pp.string ctx "normal"
  | Most_width -> Pp.string ctx "most-width"
  | Most_height -> Pp.string ctx "most-height"
  | Most_block_size -> Pp.string ctx "most-block-size"
  | Most_inline_size -> Pp.string ctx "most-inline-size"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_position_try : position_try Pp.t =
 fun ctx -> function
  | Try (Normal, fallbacks) -> pp_position_try_fallbacks ctx fallbacks
  | Try (order, None) -> pp_position_try_order ctx order
  | Try (order, fallbacks) ->
      pp_position_try_order ctx order;
      Pp.space ctx ();
      pp_position_try_fallbacks ctx fallbacks
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_position_try ctx v

let pp_position_visibility_condition : position_visibility_condition Pp.t =
 fun ctx -> function
  | Anchors_visible -> Pp.string ctx "anchors-visible"
  | No_overflow -> Pp.string ctx "no-overflow"

let rec pp_position_visibility : position_visibility Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_visibility ctx v
  | Always -> Pp.string ctx "always"
  | Conditions conditions ->
      Pp.list ~sep:Pp.space pp_position_visibility_condition ctx conditions
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_position_area_keyword : position_area_keyword Pp.t =
 fun ctx -> function
  | Top -> Pp.string ctx "top"
  | Bottom -> Pp.string ctx "bottom"
  | Left -> Pp.string ctx "left"
  | Right -> Pp.string ctx "right"
  | Center -> Pp.string ctx "center"
  | Span_top -> Pp.string ctx "span-top"
  | Span_bottom -> Pp.string ctx "span-bottom"
  | Span_left -> Pp.string ctx "span-left"
  | Span_right -> Pp.string ctx "span-right"
  | X_start -> Pp.string ctx "x-start"
  | X_end -> Pp.string ctx "x-end"
  | Y_start -> Pp.string ctx "y-start"
  | Y_end -> Pp.string ctx "y-end"
  | Span_x_start -> Pp.string ctx "span-x-start"
  | Span_x_end -> Pp.string ctx "span-x-end"
  | Span_y_start -> Pp.string ctx "span-y-start"
  | Span_y_end -> Pp.string ctx "span-y-end"
  | Inline_start -> Pp.string ctx "inline-start"
  | Inline_end -> Pp.string ctx "inline-end"
  | Block_start -> Pp.string ctx "block-start"
  | Block_end -> Pp.string ctx "block-end"
  | Span_inline_start -> Pp.string ctx "span-inline-start"
  | Span_inline_end -> Pp.string ctx "span-inline-end"
  | Span_block_start -> Pp.string ctx "span-block-start"
  | Span_block_end -> Pp.string ctx "span-block-end"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Span_start -> Pp.string ctx "span-start"
  | Span_end -> Pp.string ctx "span-end"
  | Self_start -> Pp.string ctx "self-start"
  | Self_end -> Pp.string ctx "self-end"
  | Span_self_start -> Pp.string ctx "span-self-start"
  | Span_self_end -> Pp.string ctx "span-self-end"
  | Self_x_start -> Pp.string ctx "self-x-start"
  | Self_x_end -> Pp.string ctx "self-x-end"
  | Self_y_start -> Pp.string ctx "self-y-start"
  | Self_y_end -> Pp.string ctx "self-y-end"
  | Span_self_x_start -> Pp.string ctx "span-self-x-start"
  | Span_self_x_end -> Pp.string ctx "span-self-x-end"
  | Span_self_y_start -> Pp.string ctx "span-self-y-start"
  | Span_self_y_end -> Pp.string ctx "span-self-y-end"
  | Self_block_start -> Pp.string ctx "self-block-start"
  | Self_block_end -> Pp.string ctx "self-block-end"
  | Self_inline_start -> Pp.string ctx "self-inline-start"
  | Self_inline_end -> Pp.string ctx "self-inline-end"
  | Span_self_block_start -> Pp.string ctx "span-self-block-start"
  | Span_self_block_end -> Pp.string ctx "span-self-block-end"
  | Span_self_inline_start -> Pp.string ctx "span-self-inline-start"
  | Span_self_inline_end -> Pp.string ctx "span-self-inline-end"
  | Span_all -> Pp.string ctx "span-all"

let rec pp_position_area : position_area Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_position_area ctx v
  | None -> Pp.string ctx "none"
  (* css-anchor-position-1 sec. 3.1.2: a lone keyword stands for [X span-all]
     when it names an axis and repeats itself only when it names neither, so
     only an axis-ambiguous pair collapses. [top center] is a different area
     from [top]. *)
  | Area (first, Some second)
    when Pp.minified ctx && equal_position_area_keyword first second ->
      pp_position_area_keyword ctx first
  | Area (first, second) ->
      pp_position_area_keyword ctx first;
      Option.iter
        (fun keyword ->
          Pp.space ctx ();
          pp_position_area_keyword ctx keyword)
        second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_contain_intrinsic_size_item : contain_intrinsic_size_item Pp.t =
 fun ctx -> function
  | Length len -> pp_length ~always:true ctx len
  | Auto len ->
      Pp.string ctx "auto";
      Pp.space ctx ();
      pp_length ~always:true ctx len

let rec pp_contain_intrinsic_size : contain_intrinsic_size Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_contain_intrinsic_size ctx v
  | None -> Pp.string ctx "none"
  | Intrinsic (first, None) -> pp_contain_intrinsic_size_item ctx first
  | Intrinsic (first, Some second) ->
      pp_contain_intrinsic_size_item ctx first;
      Pp.space ctx ();
      pp_contain_intrinsic_size_item ctx second
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_contain_intrinsic_longhand : contain_intrinsic_longhand Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_contain_intrinsic_longhand ctx v
  | None -> Pp.string ctx "none"
  | Size size -> pp_contain_intrinsic_size_item ctx size
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_margin_trim_edge : margin_trim_edge Pp.t =
 fun ctx -> function
  | Block_start -> Pp.string ctx "block-start"
  | Inline_start -> Pp.string ctx "inline-start"
  | Block_end -> Pp.string ctx "block-end"
  | Inline_end -> Pp.string ctx "inline-end"

let pp_margin_trim_axis : margin_trim_axis Pp.t =
 fun ctx -> function
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"

let rec pp_margin_trim : margin_trim Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_margin_trim ctx v
  | None -> Pp.string ctx "none"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Axes axes -> Pp.list ~sep:Pp.space pp_margin_trim_axis ctx axes
  | Edges edges -> Pp.list ~sep:Pp.space pp_margin_trim_edge ctx edges
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_container_shorthand : container_shorthand Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_container_shorthand ctx v
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Shorthand { name; ctype } -> (
      match (name, ctype) with
      | None, None -> () (* Should not happen, but emit nothing *)
      | Some n, None -> Pp.string ctx n
      | None, Some t -> pp_container_type ctx t
      | Some n, Some t ->
          Pp.string ctx n;
          Pp.sp ctx ();
          Pp.char ctx '/';
          Pp.sp ctx ();
          pp_container_type ctx t)

let rec pp_content_visibility : content_visibility Pp.t =
 fun ctx -> function
  | Visible -> Pp.string ctx "visible"
  | Hidden -> Pp.string ctx "hidden"
  | Auto -> Pp.string ctx "auto"
  | Initial -> Pp.string ctx "initial"
  | Inherit -> Pp.string ctx "inherit"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_content_visibility ctx v

let rec pp_scroll_snap_align : scroll_snap_align Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scroll_snap_align ctx v
  | None -> Pp.string ctx "none"
  | Start -> Pp.string ctx "start"
  | End -> Pp.string ctx "end"
  | Center -> Pp.string ctx "center"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Snap_align_pair (block, inline) ->
      pp_scroll_snap_align ctx block;
      Pp.space ctx ();
      pp_scroll_snap_align ctx inline

let rec pp_scroll_snap_stop : scroll_snap_stop Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scroll_snap_stop ctx v
  | Normal -> Pp.string ctx "normal"
  | Always -> Pp.string ctx "always"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scroll_behavior : scroll_behavior Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_scroll_behavior ctx v
  | Auto -> Pp.string ctx "auto"
  | Smooth -> Pp.string ctx "smooth"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_scroll_snap_strictness : scroll_snap_strictness Pp.t =
 fun ctx -> function
  | Proximity -> Pp.string ctx "proximity"
  | Mandatory -> Pp.string ctx "mandatory"
  | Var v -> pp_var pp_scroll_snap_strictness ctx v

let rec pp_scroll_snap_axis : scroll_snap_axis Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | X -> Pp.string ctx "x"
  | Y -> Pp.string ctx "y"
  | Block -> Pp.string ctx "block"
  | Inline -> Pp.string ctx "inline"
  | Both -> Pp.string ctx "both"
  | Var v -> pp_var pp_scroll_snap_axis ctx v

let rec pp_scroll_snap_type : scroll_snap_type Pp.t =
 fun ctx -> function
  | Axis axis -> pp_scroll_snap_axis ctx axis
  | Axis_with_strictness (axis, strictness) -> (
      pp_scroll_snap_axis ctx axis;
      match axis with
      | None | Var _ -> () (* "none" and vars don't take strictness *)
      | _ ->
          Pp.space ctx ();
          pp_scroll_snap_strictness ctx strictness)
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_scroll_snap_type ctx v

let rec pp_overscroll_behavior : overscroll_behavior Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_overscroll_behavior ctx v
  | Auto -> Pp.string ctx "auto"
  | Contain -> Pp.string ctx "contain"
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_content_visibility t : content_visibility =
  let read_var t : content_visibility =
    Var (read_var read_content_visibility t)
  in
  Cursor.enum_or_calls "content-visibility"
    [
      ("visible", (Visible : content_visibility));
      ("auto", Auto);
      ("hidden", Hidden);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", read_var) ]
    t

let rec read_container_type (t : Cursor.t) : container_type =
  Cursor.enum_or_var "container-type"
    [
      ("normal", (Normal : container_type));
      ("inline-size", Inline_size);
      ("size", Size);
      ("scroll-state", Scroll_state);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_container_type t) : container_type))
    t

let container_name_reserved =
  [ "none"; "default"; "initial"; "inherit"; "unset"; "revert"; "revert-layer" ]

let read_container_custom_ident t =
  let ident = Cursor.ident ~keep_case:true t in
  if List.mem (String.lowercase_ascii ident) container_name_reserved then
    Cursor.err_invalid t ("reserved container-name ident: " ^ ident)
  else ident

let rec read_container_name (t : Cursor.t) : container_name =
  let keywords : (string * container_name) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "container-name" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_container_name t) : container_name))
     ~default:(fun t ->
       (Names
          (Cursor.list ~sep:Cursor.ws ~at_least:1 read_container_custom_ident t)
         : container_name))
     t
    : container_name)

let rec read_anchor_name (t : Cursor.t) : anchor_name =
  let keywords : (string * anchor_name) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "anchor-name" keywords
     ~var:(fun t -> (Var (Values.read_var read_anchor_name t) : anchor_name))
     ~default:(fun t ->
       (Names (Cursor.list ~sep:Cursor.comma ~at_least:1 read_dashed_ident t)
         : anchor_name))
     t
    : anchor_name)

let rec read_position_anchor (t : Cursor.t) : position_anchor =
  let keywords : (string * position_anchor) list =
    [
      ("auto", Auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "position-anchor" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_position_anchor t) : position_anchor))
     ~default:(fun t -> (Anchor (read_dashed_ident t) : position_anchor))
     t
    : position_anchor)

let read_contain_intrinsic_size_item t : contain_intrinsic_size_item =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some "auto" ->
      let _ = Cursor.ident t in
      Cursor.ws t;
      (Auto (Values.read_length ~allow_negative:false ~with_keywords:false t)
        : contain_intrinsic_size_item)
  | _ ->
      Length (Values.read_length ~allow_negative:false ~with_keywords:false t)

let rec read_contain_intrinsic_size (t : Cursor.t) : contain_intrinsic_size =
  let keywords : (string * contain_intrinsic_size) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  let read_intrinsic t =
    let first = read_contain_intrinsic_size_item t in
    Cursor.ws t;
    if Cursor.is_done t then Intrinsic (first, None)
    else
      let second = read_contain_intrinsic_size_item t in
      Intrinsic (first, Some second)
  in
  (Cursor.enum_or_var "contain-intrinsic-size" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_contain_intrinsic_size t)
         : contain_intrinsic_size))
     ~default:read_intrinsic t
    : contain_intrinsic_size)

let rec read_contain_intrinsic_longhand (t : Cursor.t) :
    contain_intrinsic_longhand =
  let keywords : (string * contain_intrinsic_longhand) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "contain-intrinsic longhand" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_contain_intrinsic_longhand t)
         : contain_intrinsic_longhand))
     ~default:(fun t ->
       (Size (read_contain_intrinsic_size_item t) : contain_intrinsic_longhand))
     t
    : contain_intrinsic_longhand)

let rec read_container_shorthand (t : Cursor.t) : container_shorthand =
  (* Syntax: container: [<custom-ident>] [ / <container-type> ]? *)
  let is_container_type_keyword ident =
    match String.lowercase_ascii ident with
    | "normal" | "inline-size" | "size" | "scroll-state" -> true
    | _ -> false
  in
  let validate_name_before_slash ident =
    if is_container_type_keyword ident then
      Cursor.err_invalid t
        ("container shorthand type keyword used as name: " ^ ident);
    if List.mem (String.lowercase_ascii ident) container_name_reserved then
      Cursor.err_invalid t ("reserved container-name ident: " ^ ident)
  in
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Func { node = { name; _ }; _ })
    when String.lowercase_ascii_preserve name = "var" ->
      (Var (Values.read_var read_container_shorthand t) : container_shorthand)
  | _ -> (
      let first = Cursor.ident t in
      Cursor.ws t;
      match Cursor.peek_delim t with
      | Some '/' ->
          (* We have: name / type *)
          validate_name_before_slash first;
          Cursor.expect '/' t;
          Cursor.ws t;
          let ctype = read_container_type t in
          Shorthand { name = Some first; ctype = Some ctype }
      | _ -> (
          (* Just a name, or just a type? Check if it's a valid
             container-type *)
          match first with
          | "normal" -> Shorthand { name = None; ctype = Some Normal }
          | "inline-size" -> Shorthand { name = None; ctype = Some Inline_size }
          | "size" -> Shorthand { name = None; ctype = Some Size }
          | "scroll-state" ->
              Shorthand { name = None; ctype = Some Scroll_state }
          | "initial" -> Initial
          | "inherit" -> Inherit
          | "unset" -> Unset
          | "revert" -> Revert
          | "revert-layer" -> Revert_layer
          | _ -> Shorthand { name = Some first; ctype = None }))

let rec read_contain t : contain =
  let read_contain_value t : contain =
    if Cursor.looking_at t "var(" then Var (read_var read_contain t)
    else
      Cursor.enum "contain-value"
        [
          ("size", Size);
          ("inline-size", Inline_size);
          ("layout", Layout);
          ("style", Style);
          ("paint", Paint);
        ]
        t
  in
  let read_contain_list t =
    let values = Cursor.list ~sep:Cursor.ws read_contain_value t in
    (* Per CSS Contain Module: each contain value may appear at most once.
       Compare on the constructor shape (Var omitted from the duplicate check
       since two equal var refs are not authored ambiguity). *)
    let rec has_duplicate = function
      | [] | [ _ ] -> false
      | x :: rest -> List.exists (fun y -> y = x) rest || has_duplicate rest
    in
    match values with
    | [] -> err_invalid_value t "contain" "expected contain value(s)"
    | _ when has_duplicate values ->
        err_invalid_value t "contain" "duplicate contain value"
    | [ v ] -> v
    | vs -> List vs
  in
  Cursor.enum "contain"
    [
      ("none", (None : contain));
      ("strict", Strict);
      ("content", Content);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~default:read_contain_list t

let rec read_scroll_behavior (t : Cursor.t) : scroll_behavior =
  Cursor.enum_or_var "scroll-behavior"
    [
      ("auto", (Auto : scroll_behavior));
      ("smooth", Smooth);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_scroll_behavior t) : scroll_behavior))
    t

let rec read_scroll_snap_align (t : Cursor.t) : scroll_snap_align =
  Cursor.ws t;
  let read_single t =
    Cursor.enum_or_var "scroll-snap-align"
      [
        ("none", (None : scroll_snap_align));
        ("start", Start);
        ("end", End);
        ("center", Center);
        ("inherit", Inherit);
        ("initial", Initial);
        ("unset", Unset);
        ("revert", Revert);
        ("revert-layer", Revert_layer);
      ]
      ~var:(fun t ->
        (Var (Values.read_var read_scroll_snap_align t) : scroll_snap_align))
      t
  in
  let first = read_single t in
  (* The optional second keyword is probed with backtracking so the reader stops
     cleanly at whatever follows the value (the declaration's [;], a block
     close, ...). *)
  match Cursor.option read_single t with
  | Option.None -> first
  | Option.Some second ->
      (match first with
      | Inherit | Initial | Unset | Revert | Revert_layer ->
          Cursor.err_invalid t "scroll-snap-align CSS-wide value in pair"
      | _ -> ());
      Snap_align_pair (first, second)

let read_position_try_fallback t =
  Cursor.ws t;
  match Cursor.peek_ident t with
  | Some (("flip-block" | "flip-inline" | "flip-start") as keyword) -> (
      let _ = Cursor.ident t in
      match keyword with
      | "flip-block" -> (Flip_block : position_try_fallback)
      | "flip-inline" -> Flip_inline
      | "flip-start" -> Flip_start
      | _ -> assert false)
  | _ -> Name (read_dashed_ident t)

(* One [<dashed-ident> || <try-tactic>] entry. [||] is order-free and takes each
   component at most once, so the group reads in any order and is held
   name-first with the tactics in block, inline, start order. *)
let read_position_try_fallback_group t =
  let components =
    Cursor.list ~sep:Cursor.ws ~at_least:1 read_position_try_fallback t
  in
  let pick p = List.filter p components in
  let name =
    pick (function (Name _ : position_try_fallback) -> true | _ -> false)
  in
  let block = pick (function Flip_block -> true | _ -> false) in
  let inline = pick (function Flip_inline -> true | _ -> false) in
  let start = pick (function Flip_start -> true | _ -> false) in
  if
    List.length name > 1
    || List.length block > 1
    || List.length inline > 1
    || List.length start > 1
  then Cursor.err_invalid t "position-try-fallbacks repeats a component";
  name @ block @ inline @ start

let rec read_position_try_fallbacks t : position_try_fallbacks =
  let keywords : (string * position_try_fallbacks) list =
    [
      ("none", None);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
  in
  (Cursor.enum_or_var "position-try-fallbacks" keywords
     ~var:(fun t ->
       (Var (Values.read_var read_position_try_fallbacks t)
         : position_try_fallbacks))
     ~default:(fun t ->
       (Fallbacks
          (Cursor.list ~sep:Cursor.comma ~at_least:1
             read_position_try_fallback_group t)
         : position_try_fallbacks))
     t
    : position_try_fallbacks)

let rec read_position_try_order t : position_try_order =
  Cursor.enum_or_var "position-try-order"
    [
      ("normal", (Normal : position_try_order));
      ("most-width", Most_width);
      ("most-height", Most_height);
      ("most-block-size", Most_block_size);
      ("most-inline-size", Most_inline_size);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_position_try_order t) : position_try_order))
    t

let rec read_position_try t : position_try =
  Cursor.enum_or_var "position-try"
    [
      ("initial", (Initial : position_try));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_position_try t))
    ~default:(fun t ->
      (* [<order> || <fallbacks>]: an optional leading order keyword (which
         never collides with a fallback dashed-ident or try-tactic), then the
         fallbacks. An order alone leaves fallbacks at its initial [none]. *)
      let order : position_try_order =
        match Cursor.peek_ident t with
        | Some
            ( "normal" | "most-width" | "most-height" | "most-block-size"
            | "most-inline-size" ) ->
            let o = read_position_try_order t in
            Cursor.ws t;
            o
        | _ -> Normal
      in
      let fallbacks =
        match Cursor.option read_position_try_fallbacks t with
        | Some f -> f
        | None -> (None : position_try_fallbacks)
      in
      Try (order, fallbacks))
    t

let rec read_position_visibility t : position_visibility =
  Cursor.enum_or_var "position-visibility"
    [
      ("always", (Always : position_visibility));
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_position_visibility t) : position_visibility))
    ~default:(fun t ->
      let read_condition t =
        Cursor.enum "position-visibility condition"
          [
            ( "anchors-visible",
              (Anchors_visible : position_visibility_condition) );
            ("no-overflow", No_overflow);
          ]
          t
      in
      let conditions =
        Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_condition t
      in
      let duplicate condition =
        List.length
          (List.filter
             (equal_position_visibility_condition condition)
             conditions)
        > 1
      in
      if List.exists duplicate conditions then
        Cursor.err_invalid t "position-visibility";
      (Conditions conditions : position_visibility))
    t

let position_area_keywords : (string * position_area_keyword) list =
  [
    ("top", Top);
    ("bottom", Bottom);
    ("left", Left);
    ("right", Right);
    ("center", Center);
    ("span-top", Span_top);
    ("span-bottom", Span_bottom);
    ("span-left", Span_left);
    ("span-right", Span_right);
    ("x-start", X_start);
    ("x-end", X_end);
    ("y-start", Y_start);
    ("y-end", Y_end);
    ("span-x-start", Span_x_start);
    ("span-x-end", Span_x_end);
    ("span-y-start", Span_y_start);
    ("span-y-end", Span_y_end);
    ("inline-start", Inline_start);
    ("inline-end", Inline_end);
    ("block-start", Block_start);
    ("block-end", Block_end);
    ("span-inline-start", Span_inline_start);
    ("span-inline-end", Span_inline_end);
    ("span-block-start", Span_block_start);
    ("span-block-end", Span_block_end);
    ("start", Start);
    ("end", End);
    ("span-start", Span_start);
    ("span-end", Span_end);
    ("self-start", Self_start);
    ("self-end", Self_end);
    ("span-self-start", Span_self_start);
    ("span-self-end", Span_self_end);
    ("self-x-start", Self_x_start);
    ("self-x-end", Self_x_end);
    ("self-y-start", Self_y_start);
    ("self-y-end", Self_y_end);
    ("span-self-x-start", Span_self_x_start);
    ("span-self-x-end", Span_self_x_end);
    ("span-self-y-start", Span_self_y_start);
    ("span-self-y-end", Span_self_y_end);
    ("self-block-start", Self_block_start);
    ("self-block-end", Self_block_end);
    ("self-inline-start", Self_inline_start);
    ("self-inline-end", Self_inline_end);
    ("span-self-block-start", Span_self_block_start);
    ("span-self-block-end", Span_self_block_end);
    ("span-self-inline-start", Span_self_inline_start);
    ("span-self-inline-end", Span_self_inline_end);
    ("span-all", Span_all);
  ]

let read_position_area_keyword t : position_area_keyword =
  Cursor.enum "position-area keyword" position_area_keywords t

type position_area_axis = Horizontal | Vertical

(* css-anchor-position-1 sec. 3.1.2 spells <position-area> as five top-level
   alternatives: three join two groups with [||], and two repeat a single group
   with [{1,2}]. The axis below is only ever read against the other side of the
   same [||], never across alternatives. [center] and [span-all] are the two
   keywords listed in every group. *)
type position_area_group =
  | Physical of position_area_axis
  | Logical of position_area_axis
  | Self_logical of position_area_axis
  | Plain
  | Self_plain
  | Every_group

let position_area_group (keyword : position_area_keyword) =
  match keyword with
  | Left | Right | Span_left | Span_right | X_start | X_end | Span_x_start
  | Span_x_end | Self_x_start | Self_x_end | Span_self_x_start | Span_self_x_end
    ->
      Physical Horizontal
  | Top | Bottom | Span_top | Span_bottom | Y_start | Y_end | Span_y_start
  | Span_y_end | Self_y_start | Self_y_end | Span_self_y_start | Span_self_y_end
    ->
      Physical Vertical
  | Inline_start | Inline_end | Span_inline_start | Span_inline_end ->
      Logical Horizontal
  | Block_start | Block_end | Span_block_start | Span_block_end ->
      Logical Vertical
  | Self_inline_start | Self_inline_end | Span_self_inline_start
  | Span_self_inline_end ->
      Self_logical Horizontal
  | Self_block_start | Self_block_end | Span_self_block_start
  | Span_self_block_end ->
      Self_logical Vertical
  | Start | End | Span_start | Span_end -> Plain
  | Self_start | Self_end | Span_self_start | Span_self_end -> Self_plain
  | Center | Span_all -> Every_group

let compatible_position_area_keywords first second =
  match (position_area_group first, position_area_group second) with
  (* Being in every group, these pair with anything and take whichever side is
     left over. *)
  | Every_group, _ | _, Every_group -> true
  (* Each side of a [||] contributes at most one keyword, so a two-keyword value
     has to take both sides of one alternative. *)
  | Physical Horizontal, Physical Vertical
  | Physical Vertical, Physical Horizontal
  | Logical Horizontal, Logical Vertical
  | Logical Vertical, Logical Horizontal
  | Self_logical Horizontal, Self_logical Vertical
  | Self_logical Vertical, Self_logical Horizontal ->
      true
  (* A [{1,2}] repeats one group, so any two of its keywords pair, a repeat
     included. *)
  | Plain, Plain | Self_plain, Self_plain -> true
  (* Everything else crosses two alternatives, which the grammar never
     produces. *)
  | (Physical _ | Logical _ | Self_logical _ | Plain | Self_plain), _ -> false

let rec read_position_area t : position_area =
  Cursor.enum_or_var "position-area"
    [
      ("none", (None : position_area));
      ("initial", (Initial : position_area));
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_position_area t) : position_area))
    ~default:(fun t ->
      match Cursor.list ~at_least:1 ~at_most:2 read_position_area_keyword t with
      | [ first ] -> (Area (first, None) : position_area)
      | [ first; second ] ->
          if not (compatible_position_area_keywords first second) then
            Cursor.err_invalid t
              "position-area keywords must come from one branch of the grammar";
          (Area (first, Some second) : position_area)
      | _ -> Cursor.err_expected t "position-area")
    t

let rec read_scroll_snap_stop (t : Cursor.t) : scroll_snap_stop =
  Cursor.ws t;
  Cursor.enum_or_var "scroll-snap-stop"
    [
      ("normal", (Normal : scroll_snap_stop));
      ("always", Always);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_scroll_snap_stop t) : scroll_snap_stop))
    t

let rec read_scroll_snap_strictness t : scroll_snap_strictness =
  Cursor.enum_or_calls "scroll-snap-strictness"
    [
      ("proximity", (Proximity : scroll_snap_strictness));
      ("mandatory", Mandatory);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_scroll_snap_strictness t)) ]
    t

let rec read_scroll_snap_axis t : scroll_snap_axis =
  Cursor.enum_or_calls "scroll-snap axis"
    [
      ("none", (None : scroll_snap_axis));
      ("x", X);
      ("y", Y);
      ("block", Block);
      ("inline", Inline);
      ("both", Both);
    ]
    ~calls:[ ("var", fun t -> Var (read_var read_scroll_snap_axis t)) ]
    t

let rec read_scroll_snap_type t : scroll_snap_type =
  let read_axis_with_optional_strictness t =
    let axis = read_scroll_snap_axis t in
    Cursor.ws t;
    match axis with
    | None | Var _ ->
        (* "none" and vars don't take strictness *)
        Axis axis
    | _ -> (
        (* Try to read strictness *)
        match Cursor.option read_scroll_snap_strictness t with
        | Some strictness -> Axis_with_strictness (axis, strictness)
        | None -> Axis axis)
  in
  Cursor.enum_or_var "scroll-snap-type"
    [
      ("inherit", (Inherit : scroll_snap_type));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (read_var read_scroll_snap_type t) : scroll_snap_type))
    ~default:read_axis_with_optional_strictness t

let rec read_overscroll_behavior t : overscroll_behavior =
  Cursor.enum_or_var "overscroll-behavior"
    [
      ("auto", (Auto : overscroll_behavior));
      ("contain", Contain);
      ("none", None);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_overscroll_behavior t))
    t

(* Reader for will-change property *)
let rec read_will_change t : will_change =
  let read_ident t =
    let ident = Cursor.ident ~keep_case:true t in
    if ident = "auto" then
      Cursor.err_invalid t "auto cannot be used in a will-change list";
    if ident = "will-change" then
      Cursor.err_invalid t "will-change cannot reference itself";
    ident
  in
  Cursor.enum_or_var "will-change"
    [
      ("auto", Will_change_auto);
      ("initial", Initial);
      ("inherit", Inherit);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_will_change t))
    ~default:(fun t ->
      match Cursor.list ~sep:Cursor.comma ~at_least:1 read_ident t with
      | [ "scroll-position" ] -> Scroll_position
      | [ "contents" ] -> Contents
      | [ "transform" ] -> Transform
      | [ "opacity" ] -> Opacity
      | props -> Properties props)
    t

let read_margin_trim_axis t : margin_trim_axis =
  Cursor.enum "margin-trim axis"
    [ ("block", (Block : margin_trim_axis)); ("inline", Inline) ]
    t

let read_margin_trim_edge t : margin_trim_edge =
  Cursor.enum "margin-trim edge"
    [
      ("block-start", (Block_start : margin_trim_edge));
      ("inline-start", Inline_start);
      ("block-end", Block_end);
      ("inline-end", Inline_end);
    ]
    t

let margin_trim_axis_of_ident t = function
  | "block" -> (Block : margin_trim_axis)
  | "inline" -> Inline
  | s -> Cursor.err_invalid t ("invalid margin-trim axis: " ^ s)

let read_margin_trim_axes t : margin_trim option =
  let axes = [ "block"; "inline" ] in
  let rec loop acc =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some s
      when List.mem s axes && not (List.mem (margin_trim_axis_of_ident t s) acc)
      ->
        let _ = Cursor.ident t in
        loop (margin_trim_axis_of_ident t s :: acc)
    | Some s when List.mem s axes ->
        Cursor.err_invalid t
          (String.concat "" [ "duplicate margin-trim axis: "; s ])
    | _ -> List.rev acc
  in
  match loop [] with
  | [] -> None
  | [ Block ] -> Some (Block : margin_trim)
  | [ Inline ] -> Some (Inline : margin_trim)
  | axes -> Some (Axes axes : margin_trim)

let margin_trim_edge_of_ident t = function
  | "block-start" -> (Block_start : margin_trim_edge)
  | "inline-start" -> Inline_start
  | "block-end" -> Block_end
  | "inline-end" -> Inline_end
  | s -> Cursor.err_invalid t ("invalid margin-trim edge: " ^ s)

let read_margin_trim_edges t =
  let edges = [ "block-start"; "inline-start"; "block-end"; "inline-end" ] in
  let rec loop acc =
    Cursor.ws t;
    match Cursor.peek_ident t with
    | Some s
      when List.mem s edges
           && not (List.mem (margin_trim_edge_of_ident t s) acc) ->
        let _ = Cursor.ident t in
        loop (margin_trim_edge_of_ident t s :: acc)
    | Some s when List.mem s edges ->
        Cursor.err_invalid t
          (String.concat "" [ "duplicate margin-trim edge: "; s ])
    | _ -> List.rev acc
  in
  let chosen = loop [] in
  if chosen = [] then Cursor.err_expected t "margin-trim value"
  else (Edges chosen : margin_trim)

let margin_trim_keywords : (string * margin_trim) list =
  [
    ("none", None);
    ("initial", Initial);
    ("inherit", Inherit);
    ("unset", Unset);
    ("revert", Revert);
    ("revert-layer", Revert_layer);
  ]

let rec read_margin_trim t : margin_trim =
  (Cursor.enum_or_var "margin-trim" margin_trim_keywords
     ~var:(fun t -> (Var (Values.read_var read_margin_trim t) : margin_trim))
     ~default:(fun t ->
       match read_margin_trim_axes t with
       | Some value -> value
       | None -> read_margin_trim_edges t)
     t
    : margin_trim)

let read_position_visibility_condition t : position_visibility_condition =
  Cursor.enum "position-visibility"
    [
      ("anchors-visible", Anchors_visible);
      ("no-overflow", (No_overflow : position_visibility_condition));
    ]
    t
