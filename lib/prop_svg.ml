(* SVG 2 and css-fill-stroke-3: fill, stroke and its longhands, paint order,
   fill rule and vector effect.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common

let rec numeric_miterlimit_calc_leaves :
    stroke_miterlimit calc -> stroke_miterlimit calc = function
  | Val (Number n) -> Num n
  | Nested inner -> Nested (numeric_miterlimit_calc_leaves inner)
  | Parens inner -> Parens (numeric_miterlimit_calc_leaves inner)
  | Expr (left, op, right) ->
      Expr
        ( numeric_miterlimit_calc_leaves left,
          op,
          numeric_miterlimit_calc_leaves right )
  | other -> other

(* SVG 2 sec. 13.8: keywords left out are painted last, in the order [normal]
   would use. So a value denotes the full order [written @ missing-in-normal-
   order], and the shortest spelling is the shortest prefix of that order which
   expands back to it. [fill stroke markers] expands from nothing, so it is
   [normal]; [stroke fill] expands from [stroke]. *)
let paint_order_normal : paint_order_keyword list = [ Fill; Stroke; Markers ]

let paint_order_expand (written : paint_order_keyword list) =
  written @ List.filter (fun k -> not (List.mem k written)) paint_order_normal

(* SVG 2 sec. 8.13: [viewport] is what an omitted space means, so writing it is
   redundant. *)
let normalize_vector_effect (value : vector_effect) : vector_effect =
  match value with
  | Effects (ks, Some Viewport) -> Effects (ks, Option.None)
  | _ -> value

let normalize_paint_order (value : paint_order) : paint_order =
  match value with
  | Order written ->
      let full = paint_order_expand written in
      let rec shortest n =
        if n > List.length full then value
        else
          let prefix = List.filteri (fun i _ -> i < n) full in
          if
            List.equal equal_paint_order_keyword
              (paint_order_expand prefix)
              full
          then if prefix = [] then (Normal : paint_order) else Order prefix
          else shortest (n + 1)
      in
      shortest 0
  | _ -> value

let normalize_stroke_width ~ctx (value : stroke_width) : stroke_width =
  match value with
  | Length lp ->
      let lp' = Values.normalize_length_percentage ~ctx lp in
      if lp' == lp then value else Length lp'
  | _ -> value

let normalize_dash_length ~ctx (value : dash_length) : dash_length =
  match value with
  | Number _ -> value
  | Length lp ->
      let lp' = Values.normalize_length_percentage ~ctx lp in
      if lp' == lp then value else Length lp'

let normalize_stroke_dashoffset ~ctx (value : stroke_dashoffset) :
    stroke_dashoffset =
  match value with
  | Dash d ->
      let d' = normalize_dash_length ~ctx d in
      if d' == d then value else Dash d'
  | _ -> value

let normalize_stroke_dasharray ~ctx (value : stroke_dasharray) :
    stroke_dasharray =
  match value with
  | Dashes ds ->
      let ds' = map_preserve (normalize_dash_length ~ctx) ds in
      if ds' == ds then value else Dashes ds'
  | _ -> value

let rec normalize_stroke_miterlimit (value : stroke_miterlimit) :
    stroke_miterlimit =
  match value with
  | Calc c -> (
      match eval_calc (numeric_miterlimit_calc_leaves c) with
      | Num f -> Number f
      | Val v -> normalize_stroke_miterlimit v
      | folded -> if folded == c then value else Calc folded)
  | _ -> value

let rec normalize_svg_paint ?(lossless = false) : svg_paint -> svg_paint =
 fun value ->
  match value with
  | Color c -> preserve_if_equal value (Color (normalize_color ~lossless c))
  | Url (u, fallback) ->
      preserve_if_equal value
        (Url (u, option_map_preserve (normalize_svg_paint ~lossless) fallback))
  | other -> other

let pp_vector_effect_keyword : vector_effect_keyword Pp.t =
 fun ctx -> function
  | Non_scaling_stroke -> Pp.string ctx "non-scaling-stroke"
  | Non_scaling_size -> Pp.string ctx "non-scaling-size"
  | Non_rotation -> Pp.string ctx "non-rotation"
  | Fixed_position -> Pp.string ctx "fixed-position"

let pp_vector_effect_space : vector_effect_space Pp.t =
 fun ctx -> function
  | Viewport -> Pp.string ctx "viewport"
  | Screen -> Pp.string ctx "screen"

let rec pp_vector_effect : vector_effect Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_vector_effect ctx v
  | None -> Pp.string ctx "none"
  | Effects (ks, space) -> (
      Pp.list ~sep:Pp.space pp_vector_effect_keyword ctx ks;
      match space with
      | Some s ->
          Pp.space ctx ();
          pp_vector_effect_space ctx s
      | Option.None -> ())
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_paint_order_keyword : paint_order_keyword Pp.t =
 fun ctx -> function
  | Fill -> Pp.string ctx "fill"
  | Stroke -> Pp.string ctx "stroke"
  | Markers -> Pp.string ctx "markers"

let rec pp_paint_order : paint_order Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_paint_order ctx v
  | Normal -> Pp.string ctx "normal"
  | Order ks -> Pp.list ~sep:Pp.space pp_paint_order_keyword ctx ks
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_stroke_width : stroke_width Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_stroke_width ctx v
  | Number n -> Pp.float ctx n
  | Length lp -> Values.pp_length_percentage ctx lp
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let pp_dash_length : dash_length Pp.t =
 fun ctx -> function
  | Number n -> Pp.float ctx n
  | Length lp -> Values.pp_length_percentage ctx lp

let rec pp_stroke_dashoffset : stroke_dashoffset Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_stroke_dashoffset ctx v
  | Dash d -> pp_dash_length ctx d
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_stroke_dasharray : stroke_dasharray Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_stroke_dasharray ctx v
  | None -> Pp.string ctx "none"
  (* Whitespace is the shorter of the two separators the grammar allows. *)
  | Dashes ds -> Pp.list ~sep:Pp.space pp_dash_length ctx ds
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_stroke_miterlimit : stroke_miterlimit Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_stroke_miterlimit ctx v
  | Number value -> Pp.float ctx value
  | Calc c -> pp_calc pp_stroke_miterlimit ctx c
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_stroke_linecap : stroke_linecap Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_stroke_linecap ctx v
  | Butt -> Pp.string ctx "butt"
  | Round -> Pp.string ctx "round"
  | Square -> Pp.string ctx "square"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_stroke_linejoin : stroke_linejoin Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_stroke_linejoin ctx v
  | Miter -> Pp.string ctx "miter"
  | Miter_clip -> Pp.string ctx "miter-clip"
  | Round -> Pp.string ctx "round"
  | Bevel -> Pp.string ctx "bevel"
  | Arcs -> Pp.string ctx "arcs"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_fill_rule : fill_rule Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_fill_rule ctx v
  | Nonzero -> Pp.string ctx "nonzero"
  | Evenodd -> Pp.string ctx "evenodd"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec pp_svg_paint : svg_paint Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_svg_paint ctx v
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Current_color -> Pp.string ctx "currentcolor"
  | Color c -> pp_color ctx c
  | Context_fill -> Pp.string ctx "context-fill"
  | Context_stroke -> Pp.string ctx "context-stroke"
  | Url (u, fallback) -> (
      Pp.url ctx u;
      match fallback with
      | None -> ()
      | Some fb ->
          (* CSS Syntax 3 (ED) sec. 9: a [url(...)] token closes with [)], so
             the whitespace before a fallback keyword/colour can be elided under
             minify. *)
          Pp.sp ctx ();
          pp_svg_paint ctx fb)

let read_svg_paint t : svg_paint =
  let read_url_with_fallback t =
    let u = Cursor.url t in
    (* Empty URLs are invalid in SVG paint context *)
    if u = "" then Cursor.err t "svg-paint url() must have a non-empty URL";
    Cursor.ws t;
    let fb =
      Cursor.option
        (fun t ->
          Cursor.enum "svg-paint-fallback"
            [ ("none", (None : svg_paint)); ("currentcolor", Current_color) ]
            ~default:(fun t -> (Color (read_color t) : svg_paint))
            t)
        t
    in
    Url (u, fb)
  in
  (* Bare [url(#grad)] is a single [Token.Url] component; handle before the
     function/ident dispatch. *)
  Cursor.one_of
    [
      read_url_with_fallback;
      (fun t ->
        Cursor.enum_or_calls "svg-paint"
          [
            ("none", (None : svg_paint));
            ("inherit", Inherit);
            ("currentcolor", Current_color);
            ("context-fill", Context_fill);
            ("context-stroke", Context_stroke);
          ]
          ~default:(fun t -> (Color (read_color t) : svg_paint))
          t);
    ]
    t

let vector_effect_keyword_of = function
  | "non-scaling-stroke" -> Some (Non_scaling_stroke : vector_effect_keyword)
  | "non-scaling-size" -> Some Non_scaling_size
  | "non-rotation" -> Some Non_rotation
  | "fixed-position" -> Some Fixed_position
  | _ -> Option.None

let vector_effect_space_of = function
  | "viewport" -> Some (Viewport : vector_effect_space)
  | "screen" -> Some Screen
  | _ -> Option.None

let read_vector_effect_keyword t : vector_effect_keyword =
  let loc = Cursor.position t in
  let name = Cursor.ident t in
  match vector_effect_keyword_of name with
  | Some k -> k
  | Option.None -> err_invalid_value ~loc t "vector-effect" name

let read_vector_effect_space t : vector_effect_space =
  let loc = Cursor.position t in
  let name = Cursor.ident t in
  match vector_effect_space_of name with
  | Some s -> s
  | Option.None -> err_invalid_value ~loc t "vector-effect" name

(* [ <effect> ]+ then an optional space keyword, so an ident that names a space
   ends the effect run. *)
let rec read_vector_effect t : vector_effect =
  Cursor.enum_or_calls "vector-effect"
    [
      ("none", (None : vector_effect));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_vector_effect t)) ]
    ~default:(fun t ->
      let rec go acc =
        Cursor.ws t;
        match Option.map vector_effect_keyword_of (Cursor.peek_ident t) with
        | Some (Some k) ->
            let _ = Cursor.ident t in
            go (k :: acc)
        | _ -> List.rev acc
      in
      let effects = go [ read_vector_effect_keyword t ] in
      Cursor.ws t;
      let space =
        match Option.map vector_effect_space_of (Cursor.peek_ident t) with
        | Some (Some _) -> Some (read_vector_effect_space t)
        | _ -> Option.None
      in
      (Effects (effects, space) : vector_effect))
    t

let paint_order_keyword_of = function
  | "fill" -> Some (Fill : paint_order_keyword)
  | "stroke" -> Some Stroke
  | "markers" -> Some Markers
  | _ -> None

let read_paint_order_keyword t : paint_order_keyword =
  let loc = Cursor.position t in
  let name = Cursor.ident t in
  match paint_order_keyword_of name with
  | Some k -> k
  | None -> err_invalid_value ~loc t "paint-order" name

(* [||] takes each operand at most once, so a repeat ends the list rather than
   extending it. *)
let rec read_paint_order t : paint_order =
  Cursor.enum_or_calls "paint-order"
    [
      ("normal", (Normal : paint_order));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_paint_order t)) ]
    ~default:(fun t ->
      let rec go acc =
        if List.length acc = 3 then List.rev acc
        else begin
          Cursor.ws t;
          match Option.map paint_order_keyword_of (Cursor.peek_ident t) with
          | Some (Some k) when not (List.mem k acc) ->
              let _ = Cursor.ident t in
              go (k :: acc)
          | _ -> List.rev acc
        end
      in
      (Order (go [ read_paint_order_keyword t ]) : paint_order))
    t

(* A bare number is user units; anything with a unit or a percent sign is a
   <length-percentage>. *)
let read_dash_length ?(allow_negative = true) t : dash_length =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Number_tok _; _ }) ->
      let n = Cursor.number t in
      if (not allow_negative) && n < 0. then
        Cursor.err_invalid t "stroke-dasharray cannot be negative";
      Number n
  (* The grammar is <length-percentage> | <number>, with no keyword branch, so
     the intrinsic-sizing keywords a bare length would accept are out. *)
  | _ ->
      Length
        (Values.read_length_percentage ~allow_negative ~with_keywords:false t)

let rec read_stroke_dashoffset t : stroke_dashoffset =
  Cursor.enum_or_calls "stroke-dashoffset"
    [
      ("inherit", (Inherit : stroke_dashoffset));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_stroke_dashoffset t)) ]
    ~default:(fun t -> (Dash (read_dash_length t) : stroke_dashoffset))
    t

(* The grammar separates dashes by comma and/or whitespace and the rendered
   pattern is the flat sequence either way, so both spellings read to one
   list. *)
let rec read_stroke_dasharray t : stroke_dasharray =
  Cursor.enum_or_calls "stroke-dasharray"
    [
      ("none", (None : stroke_dasharray));
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_stroke_dasharray t)) ]
    ~default:(fun t ->
      (* Only a numeric token continues the pattern. Anything else ends it, so a
         trailing [;] or [!important] is left for the caller rather than read as
         another dash. *)
      let starts_dash t =
        match Cursor.peek t with
        | Some
            (Component.Preserved
               {
                 kind =
                   Token.Number_tok _ | Token.Percentage _ | Token.Dimension _;
                 _;
               }) ->
            true
        | _ -> false
      in
      let rec go acc =
        (* SVG 2 sec. 13.3 gives each dash a non-negative value; only
           [stroke-dashoffset] takes a signed one. *)
        let acc = read_dash_length ~allow_negative:false t :: acc in
        Cursor.ws t;
        if Cursor.peek_comma t then begin
          Cursor.comma t;
          Cursor.ws t;
          go acc
        end
        else if starts_dash t then go acc
        else List.rev acc
      in
      (Dashes (go []) : stroke_dasharray))
    t

(* SVG 2 sec. 13.5.3: "A <number> value represents a value in user units", and
   "A negative value is invalid", so both branches of the production refuse one.
   Only a literal can be checked here; calc() and var() resolve later. *)
let read_stroke_width_value t : stroke_width =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Number_tok _; _ }) ->
      let n = Cursor.number t in
      if n < 0. then Cursor.err_invalid t "negative stroke-width";
      Number n
  (* The grammar has no keyword branch, so the intrinsic-sizing keywords a bare
     length would accept are out. *)
  | _ ->
      Length
        (Values.read_length_percentage ~allow_negative:false
           ~with_keywords:false t)

let rec read_stroke_width t : stroke_width =
  Cursor.enum_or_calls "stroke-width"
    [
      ("inherit", (Inherit : stroke_width));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:[ ("var", fun t -> Var (Values.read_var read_stroke_width t)) ]
    ~default:read_stroke_width_value t

(* SVG 2 sec. 13.5.5: "A negative value for stroke-miterlimit must be treated as
   an illegal value". SVG 1.1 sec. 11.4 also required at least 1, but SVG 2
   dropped that because CSS parsers never enforced it. Only a literal can be
   checked here; calc() and var() resolve later. *)
let read_miterlimit_number t =
  let value =
    match (Values.read_number t : Values.number) with
    | Values.Num value -> value
    | _ -> Cursor.err_invalid t "stroke-miterlimit must resolve to a number"
  in
  if value < 0. then Cursor.err_invalid t "negative stroke-miterlimit";
  value

let rec read_stroke_miterlimit t : stroke_miterlimit =
  Cursor.enum_or_calls "stroke-miterlimit"
    [
      ("inherit", (Inherit : stroke_miterlimit));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", fun t -> Var (Values.read_var read_stroke_miterlimit t));
        ( "calc",
          fun t ->
            Calc (read_calc ~result_type:`Number read_stroke_miterlimit t) );
      ]
    ~default:(fun t -> (Number (read_miterlimit_number t) : stroke_miterlimit))
    t

let rec read_stroke_linecap t : stroke_linecap =
  Cursor.enum_or_var "stroke-linecap"
    [
      ("butt", (Butt : stroke_linecap));
      ("round", Round);
      ("square", Square);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_stroke_linecap t))
    t

let rec read_stroke_linejoin t : stroke_linejoin =
  Cursor.enum_or_var "stroke-linejoin"
    [
      ("miter", (Miter : stroke_linejoin));
      ("miter-clip", Miter_clip);
      ("round", Round);
      ("bevel", Bevel);
      ("arcs", Arcs);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_stroke_linejoin t))
    t

let rec read_fill_rule t : fill_rule =
  Cursor.enum_or_var "fill-rule"
    [
      ("nonzero", (Nonzero : fill_rule));
      ("evenodd", Evenodd);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_fill_rule t))
    t
