(* CSS Backgrounds 3: the background and border families. The background
   longhands and shorthand, border style / width / radius / image,
   border-collapse and border-spacing, and box-shadow with its [Shadow] reader.

   [Shadow], [pp_shadow] and [normalize_shadow] are shared with the filter
   family: [drop-shadow()] reuses them. Prop_filter must therefore be ordered
   after this module, or the filter family stays in properties.ml.

   This module has no .mli and is private to the library; [Properties] includes
   it, so every name here stays visible under [Css.Properties] exactly as when
   it lived in properties.ml. *)

open Common
open Values
open Properties_intf
open Prop_common
open Prop_image

module Shadow = struct
  let read_lengths lengths =
    match lengths with
    | h_offset :: v_offset :: rest ->
        let blur, spread =
          match rest with
          | b :: s :: _ -> (Some b, Some s)
          | b :: [] -> (Some b, None)
          | [] -> (None, None)
        in
        Some (h_offset, v_offset, blur, spread)
    | _ -> None

  let read t =
    (* Drive [inset? && <length>{2,4} && <color>?] positionally so a trailing
       [var()] in the length run isn't greedily folded into the colour slot. *)
    let inset = ref false in
    let color : color option ref = ref (None : color option) in
    let try_inset () =
      if !inset then false
      else
        match
          Cursor.option
            (fun t ->
              Cursor.expect_string "inset" t;
              true)
            t
        with
        | Some _ ->
            inset := true;
            Cursor.ws t;
            true
        | None -> false
    in
    let try_color () =
      match !color with
      | Some _ -> false
      | None -> (
          match Cursor.option (fun t -> read_color t) t with
          | Some c ->
              color := Some c;
              Cursor.ws t;
              true
          | None -> false)
    in
    let _ : bool = try_inset () in
    let _ : bool = try_color () in
    let _ : bool = try_inset () in
    let lengths_rev = ref [] in
    (* Sec. 6.2 writes the run [<length>{2} [ <length [0,inf]> <length>? ]?], so
       every slot is a plain length - no percentage, nested in math or not, and
       no keyword - and the blur is the one slot with a floor. A math function
       holds no value to compare, so only a literal is turned away there. *)
    let rec read_lengths_loop n =
      if n >= 4 then ()
      else
        let allow_negative = n <> 2 in
        match
          Cursor.option
            (fun t ->
              read_length ~allow_negative ~with_keywords:false ~length_only:true
                t)
            t
        with
        | Some l ->
            lengths_rev := l :: !lengths_rev;
            Cursor.ws t;
            read_lengths_loop (n + 1)
        | None -> ()
    in
    read_lengths_loop 0;
    let _ : bool = try_inset () in
    let _ : bool = try_color () in
    let _ : bool = try_inset () in
    let lengths = List.rev !lengths_rev in
    match read_lengths lengths with
    | Some (h_offset, v_offset, blur, spread) ->
        let body = { h_offset; v_offset; blur; spread; color = !color } in
        (if !inset then Inset (Body body) else Shadow body : shadow)
    | None -> err_invalid_value t "shadow" "at least two lengths are required"
end

let rec read_shadow_single t : shadow =
  let read_var_shadow t : shadow = Var (read_var read_shadow_single t) in
  Cursor.ws t;
  (* inset var(--x): Shadow.read needs concrete offsets, so handle it here. *)
  let snap = Cursor.save t in
  let inset_var : shadow option =
    match Cursor.ident_opt t with
    | Some s when String.lowercase_ascii_preserve s = "inset" -> (
        Cursor.ws t;
        match Cursor.peek t with
        | Some (Component.Func { node = { name; _ }; _ })
          when String.lowercase_ascii_preserve name = "var" ->
            Some (Inset (Var (read_var read_shadow_single t)))
        | _ -> Option.None)
    | _ -> Option.None
  in
  match inset_var with
  | Some shadow -> shadow
  | Option.None ->
      Cursor.restore t snap;
      Cursor.enum_or_calls "shadow"
        [
          ("none", None);
          ("inherit", Inherit);
          ("initial", Initial);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~calls:[ ("var", read_var_shadow) ]
        ~default:Shadow.read t

let read_shadow t : shadow =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_shadow_single t with
  | [ x ] -> x
  | l -> List l

let pp_color_after_length ctx color =
  Pp.space ctx ();
  pp_color ctx color

let pp_shadow_body ctx { h_offset; v_offset; blur; spread; color } =
  pp_length ctx h_offset;
  Pp.space ctx ();
  pp_length ctx v_offset;
  pp_opt_space pp_length ctx blur;
  pp_opt_space pp_length ctx spread;
  match color with Some c -> pp_color_after_length ctx c | None -> ()

(* The [var(--name,)] prefix stands in for the optional [inset] keyword (a var
   resolving to [inset] must read [inset 0 ...], never [inset0 ...]), so the
   separator after it is load-bearing. *)
let pp_inset_toggle ctx ~name ~no_fallback =
  Pp.string ctx "var(--";
  (* [name] is the custom property's name without its [--] prefix; CSS Syntax 3
     (ED) sec. 4.3.7 lets an escape carry a [;] or a [}] into it. *)
  Pp.string ctx (Parser.escape_name name);
  if no_fallback then Pp.string ctx ")"
  else (
    (* Empty fallback: var(--name, ) in pretty, var(--name,) in minified. *)
    Pp.char ctx ',';
    Pp.space_if_pretty ctx ();
    Pp.space_if_pretty ctx ();
    Pp.string ctx ")")

let rec pp_shadow : shadow Pp.t =
 fun ctx -> function
  | Shadow body -> pp_shadow_body ctx body
  | Inset (Body body) ->
      Pp.string ctx "inset";
      Pp.space ctx ();
      pp_shadow_body ctx body
  | Inset (Var v) ->
      Pp.string ctx "inset";
      Pp.space ctx ();
      pp_var pp_shadow ctx v
  | Inset (Toggle { name; no_fallback; body }) ->
      pp_inset_toggle ctx ~name ~no_fallback;
      Pp.space ctx ();
      pp_shadow_body ctx body
  | None -> Pp.string ctx "none"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_shadow ctx v
  | List shadows -> Pp.list ~sep:Pp.comma pp_shadow ctx shadows

let rec pp_border_style : border_style Pp.t =
 fun ctx -> function
  | None -> Pp.string ctx "none"
  | Hidden -> Pp.string ctx "hidden"
  | Dotted -> Pp.string ctx "dotted"
  | Dashed -> Pp.string ctx "dashed"
  | Solid -> Pp.string ctx "solid"
  | Double -> Pp.string ctx "double"
  | Groove -> Pp.string ctx "groove"
  | Ridge -> Pp.string ctx "ridge"
  | Inset -> Pp.string ctx "inset"
  | Outset -> Pp.string ctx "outset"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_style ctx v

let rec pp_border_radius : border_radius Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border_radius ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Radius { horizontal; vertical } -> (
      pp_box_shorthand (pp_length_percentage ~always:true) ctx horizontal;
      match vertical with
      | None -> ()
      | Some vs ->
          Pp.sp ctx ();
          Pp.char ctx '/';
          Pp.sp ctx ();
          pp_box_shorthand (pp_length_percentage ~always:true) ctx vs)

(* CSS Backgrounds 3 (ED) sec. 4.1 puts the vertical radii after a [/], so each
   group collapses on its own and neither reaches across the slash. With no
   slash the values set both axes equally, so a vertical group equal to the
   horizontal one says what omitting it says. *)
let normalize_border_radius ?(strip = true) : border_radius -> border_radius =
 fun value ->
  let group =
    normalize_box_shorthand ~is_substitution:is_lp_substitution
      (Values.normalize_length_percentage ~strip)
  in
  match value with
  | Radius { horizontal; vertical } ->
      let horizontal = group horizontal in
      let vertical = option_map_preserve group vertical in
      let vertical =
        match vertical with
        | Some vs
          when (not (List.exists is_lp_substitution horizontal))
               && (not (List.exists is_lp_substitution vs))
               && List.equal Values.equal_length_percentage horizontal vs ->
            Option.None
        | _ -> vertical
      in
      preserve_if_equal value (Radius { horizontal; vertical })
  | other -> other

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

let normalize_logical_border_color ?(lossless = false) :
    logical_border_color -> logical_border_color =
 fun value ->
  match value with
  | Single c -> preserve_if_equal value (Single (normalize_color ~lossless c))
  | Pair (a, b) ->
      let a = normalize_color ~lossless a and b = normalize_color ~lossless b in
      if
        (not (is_color_substitution a))
        && (not (is_color_substitution b))
        && Values.equal_color a b
      then Single a
      else preserve_if_equal value (Pair (a, b))
  | other -> other

let rec normalize_shadow ?(lossless = false) : shadow -> shadow =
 fun value ->
  let normalize_body (s : shadow_body) : shadow_body =
    (* The blur is the one slot with a floor, so unwrapping a math function
       there could turn a value browsers take into one they drop. *)
    let blur =
      option_map_preserve (Values.normalize_length ~non_negative:true) s.blur
    in
    let spread = option_map_preserve Values.normalize_length s.spread in
    let color = option_map_preserve (normalize_color ~lossless) s.color in
    (* CSS Backgrounds 3 sec. 6.1 orders the optional blur then spread lengths
       and defaults each missing value to [0]. Drop a trailing zero contiguously
       from the end: [spread] is last and drops freely; a zero blur drops only
       when no spread follows - otherwise it is positional and dropping it would
       re-bind the spread as the blur (e.g. [0 1px 0 5px] must keep the [0]
       blur). *)
    let spread : length option =
      match spread with Some sp when is_zero_length sp -> None | _ -> spread
    in
    (* A [var()] colour could resolve to a length, so dropping a zero blur
       before it lets the shortened form re-bind the colour as the blur: [0 3px
       0 var(--c)] is blur [0] + colour, but [0 3px var(--c)] parses [var(--c)]
       as the blur. Keep the explicit [0] there; a concrete colour is
       unambiguous and the [0] still drops. *)
    let colour_may_be_length =
      match color with Some (Var _) -> true | _ -> false
    in
    let blur : length option =
      match blur with
      | Some b
        when is_zero_length b && Option.is_none spread
             && not colour_may_be_length ->
          None
      | _ -> blur
    in
    {
      h_offset = Values.normalize_length s.h_offset;
      v_offset = Values.normalize_length s.v_offset;
      blur;
      spread;
      color;
    }
  in
  match value with
  | Shadow s -> preserve_if_equal value (Shadow (normalize_body s))
  | Inset (Body s) ->
      preserve_if_equal value (Inset (Body (normalize_body s)) : shadow)
  | Inset (Toggle { name; no_fallback; body }) ->
      preserve_if_equal value
        (Inset (Toggle { name; no_fallback; body = normalize_body body })
          : shadow)
  | List shadows ->
      preserve_if_equal value
        (List (map_preserve (normalize_shadow ~lossless) shadows))
  | other -> other

let length_of_border_width : border_width -> length option = function
  | Px n -> Some (Px n)
  | Cm n -> Some (Cm n)
  | Mm n -> Some (Mm n)
  | Q n -> Some (Q n)
  | In n -> Some (In n)
  | Pt n -> Some (Pt n)
  | Pc n -> Some (Pc n)
  | Rem n -> Some (Rem n)
  | Em n -> Some (Em n)
  | Ex n -> Some (Ex n)
  | Cap n -> Some (Cap n)
  | Ic n -> Some (Ic n)
  | Ric n -> Some (Ric n)
  | Rlh n -> Some (Rlh n)
  | Ch n -> Some (Ch n)
  | Lh n -> Some (Lh n)
  | Vh n -> Some (Vh n)
  | Vw n -> Some (Vw n)
  | Vmin n -> Some (Vmin n)
  | Vmax n -> Some (Vmax n)
  | Pct n -> Some (Pct n)
  | Dimension { value; unit; repr } -> Some (Dimension { value; unit; repr })
  | Zero -> Some Zero
  | _ -> None

let length_of_border_width_calc calc = map_calc_opt length_of_border_width calc

(* The reverse of [length_of_border_width]: every dimension the two types share
   round-trips, so a fold done in [length] space (the [min()]/[max()]/ [clamp()]
   reduction, [Values.normalize_length]) can rebuild a [border_width]
   afterwards. *)
let border_width_of_length : length -> border_width option = function
  | Px n -> Some (Px n)
  | Cm n -> Some (Cm n)
  | Mm n -> Some (Mm n)
  | Q n -> Some (Q n)
  | In n -> Some (In n)
  | Pt n -> Some (Pt n)
  | Pc n -> Some (Pc n)
  | Rem n -> Some (Rem n)
  | Em n -> Some (Em n)
  | Ex n -> Some (Ex n)
  | Cap n -> Some (Cap n)
  | Ic n -> Some (Ic n)
  | Ric n -> Some (Ric n)
  | Rlh n -> Some (Rlh n)
  | Ch n -> Some (Ch n)
  | Lh n -> Some (Lh n)
  | Vh n -> Some (Vh n)
  | Vw n -> Some (Vw n)
  | Vmin n -> Some (Vmin n)
  | Vmax n -> Some (Vmax n)
  | Pct n -> Some (Pct n)
  | Dimension { value; unit; repr } -> Some (Dimension { value; unit; repr })
  | Zero -> Some Zero
  | _ -> None

let border_width_of_length_calc calc = map_calc_opt border_width_of_length calc

let length_linear_term : length -> (string * float * (float -> length)) option =
  function
  | Zero -> Some ("px", 0., fun _ -> Zero)
  | Px n -> Some ("px", n, fun n -> if n = 0. then Zero else Px n)
  | Cm n -> Some ("cm", n, fun n -> Cm n)
  | Mm n -> Some ("mm", n, fun n -> Mm n)
  | Q n -> Some ("q", n, fun n -> Q n)
  | In n -> Some ("in", n, fun n -> In n)
  | Pt n -> Some ("pt", n, fun n -> Pt n)
  | Pc n -> Some ("pc", n, fun n -> Pc n)
  | Rem n -> Some ("rem", n, fun n -> Rem n)
  | Em n -> Some ("em", n, fun n -> Em n)
  | Ex n -> Some ("ex", n, fun n -> Ex n)
  | Cap n -> Some ("cap", n, fun n -> Cap n)
  | Ic n -> Some ("ic", n, fun n -> Ic n)
  | Ric n -> Some ("ric", n, fun n -> Ric n)
  | Rlh n -> Some ("rlh", n, fun n -> Rlh n)
  | Ch n -> Some ("ch", n, fun n -> Ch n)
  | Lh n -> Some ("lh", n, fun n -> Lh n)
  | Pct n -> Some ("%", n, fun n -> Pct n)
  | Vw n -> Some ("vw", n, fun n -> Vw n)
  | Vh n -> Some ("vh", n, fun n -> Vh n)
  | Vmin n -> Some ("vmin", n, fun n -> Vmin n)
  | Vmax n -> Some ("vmax", n, fun n -> Vmax n)
  | _ -> None

let add_border_width_term
    (table : (string, int * float * (float -> length)) Hashtbl.t) pos
    (key, n, make) =
  match Hashtbl.find_opt table key with
  | None -> Hashtbl.add table key (pos, n, make)
  | Some (old_pos, old_n, old_make) ->
      Hashtbl.replace table key (old_pos, old_n +. n, old_make)

let collect_border_width_terms
    (terms : (string * float * (float -> length)) list) =
  let table = Hashtbl.create 4 in
  List.iteri (add_border_width_term table) terms;
  Hashtbl.to_seq_values table
  |> List.of_seq
  |> List.filter (fun (_, n, _) -> n <> 0.)
  |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)

let append_border_width_term (acc : length calc)
    ((_, n, make) : int * float * (float -> length)) : length calc =
  if n < 0. then Expr (acc, Sub, Val (make (-.n)))
  else Expr (acc, Add, Val (make n))

let rebuild_border_width_length_terms
    (terms : (string * float * (float -> length)) list) : length calc =
  match collect_border_width_terms terms with
  | [] -> Val Zero
  | [ (_, n, make) ] -> Val (make n)
  | (_, n, make) :: rest ->
      List.fold_left append_border_width_term (Val (make n)) rest

let simplify_border_width_length_calc calc =
  let scale factor terms =
    List.map (fun (key, n, make) -> (key, factor *. n, make)) terms
  in
  let rec terms = function
    | Val length ->
        Option.map (fun term -> [ term ]) (length_linear_term length)
    | Num 0. -> Some []
    | Num _ | Math_const _ | Var _ | Sibling_index | Sibling_count | Math_fn _
      ->
        None
    | Nested inner | Parens inner -> terms inner
    | Expr (left, Add, right) -> (
        match (terms left, terms right) with
        | Some left, Some right -> Some (left @ right)
        | _ -> None)
    | Expr (left, Sub, right) -> (
        match (terms left, terms right) with
        | Some left, Some right -> Some (left @ scale (-1.) right)
        | _ -> None)
    | Expr (left, Mul, Num n) -> Option.map (scale n) (terms left)
    | Expr (Num n, Mul, right) -> Option.map (scale n) (terms right)
    | Expr (left, Div, Num n) when n <> 0. ->
        Option.map (scale (1. /. n)) (terms left)
    | Expr _ -> None
  in
  match terms calc with
  | None -> calc
  | Some terms -> rebuild_border_width_length_terms terms

let simplified_border_width_length calc =
  Option.map simplify_border_width_length_calc
    (length_of_border_width_calc calc)

let pp_length_calc_op = pp_calc_op

let pp_length_calc_contents ctx calc =
  let precedence (op : calc_op) =
    match op with Add | Sub -> 1 | Mul | Div -> 2
  in
  let rec pp_inner ~parent_prec ~right_of_noncommut ctx = function
    | Val length -> pp_length ctx length
    | Num n -> Pp.float ctx n
    | (Var _ | Sibling_index | Sibling_count | Math_const _ | Math_fn _) as calc
      ->
        pp_calc pp_length ctx calc
    | Nested inner ->
        Pp.call "calc"
          (pp_inner ~parent_prec:0 ~right_of_noncommut:false)
          ctx inner
    | Parens inner ->
        Pp.char ctx '(';
        pp_inner ~parent_prec:0 ~right_of_noncommut:false ctx inner;
        Pp.char ctx ')'
    | Expr (left, op, right) ->
        let op_prec = precedence op in
        let needs_parens =
          op_prec < parent_prec || (right_of_noncommut && op_prec <= parent_prec)
        in
        if needs_parens then Pp.char ctx '(';
        pp_inner ~parent_prec:op_prec ~right_of_noncommut:false ctx left;
        pp_length_calc_op ctx op;
        let is_noncommut = match op with Sub | Div -> true | _ -> false in
        pp_inner ~parent_prec:op_prec ~right_of_noncommut:is_noncommut ctx right;
        if needs_parens then Pp.char ctx ')'
  in
  pp_inner ~parent_prec:0 ~right_of_noncommut:false ctx calc

(* Two arguments of a [min()] / [max()] compare only when they share a unit. CSS
   Values 4 sec. 6.2 puts the absolute units on one scale, so they share the
   [px] key; every other unit compares only with itself. *)
let border_width_length_measure length : (string * float) option =
  match Values.calc_length_unit length with
  | None -> None
  | Some (unit, n) -> (
      match Values.absolute_unit_px_ratio unit with
      | Some ratio -> Some ("px", n *. ratio)
      | None -> Some (unit, n))

let border_width_calc_length = function Val length -> Some length | _ -> None

let record_border_width_group kind groups pos (length, key, n) =
  let keep =
    match Hashtbl.find_opt groups key with
    | None -> (pos, length, n)
    | Some (old_pos, old_length, old_n) ->
        let better = match kind with `Min -> n < old_n | `Max -> n > old_n in
        if better then (old_pos, length, n) else (old_pos, old_length, old_n)
  in
  Hashtbl.replace groups key keep

(* Grouping keeps one argument per unit, so an argument no unit measures has to
   stop the reduction: dropping it would delete a bound from the function. *)
let measured_border_width_lengths lengths =
  List.fold_right
    (fun length acc ->
      match (border_width_length_measure length, acc) with
      | Some (key, n), Some acc -> Some ((length, key, n) :: acc)
      | _ -> None)
    lengths (Some [])

let reduce_border_width_minmax kind args : length calc list option =
  let simplified = List.map simplified_border_width_length args in
  if List.exists Option.is_none simplified then None
  else
    let vals = List.map Option.get simplified in
    match List.map border_width_calc_length vals with
    | lengths when List.exists Option.is_none lengths -> None
    | lengths -> (
        match measured_border_width_lengths (List.map Option.get lengths) with
        | None -> None
        | Some measured -> (
            let groups = Hashtbl.create 4 in
            List.iteri (record_border_width_group kind groups) measured;
            let reduced =
              Hashtbl.to_seq_values groups
              |> List.of_seq
              |> List.sort (fun (a, _, _) (b, _, _) -> compare a b)
              |> List.map (fun (_, length, _) -> Val length)
            in
            match reduced with [] -> None | _ -> Some reduced))

let reduce_border_width_clamp lower value upper =
  match
    ( simplified_border_width_length lower,
      simplified_border_width_length value,
      simplified_border_width_length upper )
  with
  | Some (Val lower), Some (Val value), Some (Val upper) -> (
      match
        ( border_width_length_measure lower,
          border_width_length_measure value,
          border_width_length_measure upper )
      with
      | ( Some (lower_key, lower_n),
          Some (value_key, value_n),
          Some (upper_key, upper_n) )
        when String.equal lower_key value_key
             && String.equal value_key upper_key ->
          Some
            (`Length
               (if value_n < lower_n then lower
                else if value_n > upper_n then upper
                else value))
      | Some _, Some (value_key, value_n), Some (upper_key, upper_n)
        when String.equal value_key upper_key && value_n <= upper_n ->
          Some (`Max [ Val lower; Val value ])
      | _ -> None)
  | _ -> None

(* [pp] is a pure serialiser (see the module note); every node-changing fold
   lives here instead, so it runs before [Declaration.hash] and two authored
   spellings of the same value converge to one node.

   A [min()]/[max()]/[clamp()] argument that survives grouping still gets its
   own same-unit terms combined (mirrors what [pp] used to do per-argument via
   [simplified_border_width_length]); the standalone [calc()] arm instead goes
   through [Values.normalize_length], the stronger fold [pp] used only when
   minifying. *)
let normalize_border_width_calc_arg (c : border_width calc) : border_width calc
    =
  match simplified_border_width_length c with
  | Some (Val length) -> (
      match border_width_of_length length with Some bw -> Val bw | None -> c)
  | Some simplified -> (
      match border_width_of_length_calc simplified with
      | Some c' -> c'
      | None -> c)
  | None -> c

let normalize_border_width_calc (c : border_width calc) : border_width calc =
  match length_of_border_width_calc c with
  | None -> Values.eval_calc c
  | Some lc -> (
      match Values.normalize_length (Calc lc) with
      | Calc lc' -> (
          match border_width_of_length_calc lc' with Some c' -> c' | None -> c)
      | length -> (
          match border_width_of_length length with
          | Some bw -> Val bw
          | None -> c))

let normalize_border_width_minmax kind (args : border_width calc list) :
    border_width =
  let rebuild args = match kind with `Min -> Min args | `Max -> Max args in
  match reduce_border_width_minmax kind args with
  | Some [ Val length ] -> (
      match border_width_of_length length with
      | Some bw -> bw
      | None -> rebuild args)
  | Some lengths -> (
      match List.map border_width_of_length_calc lengths with
      | terms when List.for_all Option.is_some terms ->
          rebuild (List.map Option.get terms)
      | _ -> rebuild args)
  | None -> rebuild (List.map normalize_border_width_calc_arg args)

let normalize_border_width_clamp lower value upper : border_width =
  match reduce_border_width_clamp lower value upper with
  | Some (`Length length) -> (
      match border_width_of_length length with
      | Some bw -> bw
      | None -> Clamp (lower, value, upper))
  | Some (`Max cargs) -> (
      match List.map border_width_of_length_calc cargs with
      | terms when List.for_all Option.is_some terms ->
          Max (List.map Option.get terms)
      | _ -> Clamp (lower, value, upper))
  | None ->
      Clamp
        ( normalize_border_width_calc_arg lower,
          normalize_border_width_calc_arg value,
          normalize_border_width_calc_arg upper )

(* CSS Values 4 sec. 10.12 keeps a math function valid where its range is
   exceeded and clamps at used-value time, so [calc(-1px)] is a border width
   Chrome computes as [0] and [-1px] one it drops. Unwrapping the call would
   turn the first into the second, which is why {!pp_length} guards its own
   unwrap the same way. *)
let negative_border_width : border_width -> bool = function
  | Px f
  | Cm f
  | Mm f
  | Q f
  | In f
  | Pt f
  | Pc f
  | Rem f
  | Em f
  | Ex f
  | Cap f
  | Ic f
  | Ric f
  | Rlh f
  | Ch f
  | Lh f
  | Vh f
  | Vw f
  | Vmin f
  | Vmax f
  | Pct f
  | Dimension { value = f; _ } ->
      f < 0.
  | _ -> false

let rec pp_border_width : border_width Pp.t =
 fun ctx -> function
  | Thin -> Pp.string ctx "thin"
  | Medium -> Pp.string ctx "medium"
  | Thick -> Pp.string ctx "thick"
  | Px f -> Pp.unit ctx f "px"
  | Cm f -> Pp.unit ctx f "cm"
  | Mm f -> Pp.unit ctx f "mm"
  | Q f -> Pp.unit ctx f "q"
  | In f -> Pp.unit ctx f "in"
  | Pt f -> Pp.unit ctx f "pt"
  | Pc f -> Pp.unit ctx f "pc"
  | Rem f -> Pp.unit ctx f "rem"
  | Em f -> Pp.unit ctx f "em"
  | Ex f -> Pp.unit ctx f "ex"
  | Cap f -> Pp.unit ctx f "cap"
  | Ic f -> Pp.unit ctx f "ic"
  | Ric f -> Pp.unit ctx f "ric"
  | Rlh f -> Pp.unit ctx f "rlh"
  | Ch f -> Pp.unit ctx f "ch"
  | Lh f -> Pp.unit ctx f "lh"
  | Vh f -> Pp.unit ctx f "vh"
  | Vw f -> Pp.unit ctx f "vw"
  | Vmin f -> Pp.unit ctx f "vmin"
  | Vmax f -> Pp.unit ctx f "vmax"
  | Pct f -> Pp.pct ctx f
  | Dimension { value; unit; repr } ->
      if Pp.minified ctx then Pp.unit ctx value unit
      else (
        Pp.string ctx repr;
        Pp.string ctx unit)
  | Zero -> Pp.char ctx '0'
  | Auto -> Pp.string ctx "auto"
  | Max_content -> Pp.string ctx "max-content"
  | Min_content -> Pp.string ctx "min-content"
  | Fit_content -> Pp.string ctx "fit-content"
  | From_font -> Pp.string ctx "from-font"
  | Calc cv ->
      pp_calc
        ~unwrap:(fun v -> not (negative_border_width v))
        pp_border_width ctx cv
  | Min args -> pp_border_width_minmax "min" ctx args
  | Max args -> pp_border_width_minmax "max" ctx args
  | Clamp (lower, value, upper) -> pp_border_width_clamp ctx lower value upper
  | Var v -> pp_var pp_border_width ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

(* Structural only (no [simplified_border_width_length]/[reduce_border_width_*]
   fold: those moved to [normalize_border_width]); [length_of_border_width_calc]
   is a type-level reshape, not a value fold. *)
and pp_border_width_calc_contents ctx calc =
  match length_of_border_width_calc calc with
  | Some lc -> pp_length_calc_contents ctx lc
  | None -> pp_calc pp_border_width ctx calc

and pp_border_width_minmax name ctx args =
  Pp.call name (Pp.list ~sep:Pp.comma pp_border_width_calc_contents) ctx args

and pp_border_width_clamp ctx lower value upper =
  Pp.call "clamp"
    (fun ctx (lower, value, upper) ->
      pp_border_width_calc_contents ctx lower;
      Pp.comma ctx ();
      pp_border_width_calc_contents ctx value;
      Pp.comma ctx ();
      pp_border_width_calc_contents ctx upper)
    ctx (lower, value, upper)

let pp_border_shorthand : border_shorthand Pp.t =
 fun ctx { width; style; color } ->
  let first = ref true in
  let add_space () = if !first then first := false else Pp.space ctx () in
  Option.iter
    (fun w ->
      add_space ();
      pp_border_width ctx w)
    width;
  Option.iter
    (fun s ->
      add_space ();
      pp_border_style ctx s)
    style;
  Option.iter
    (fun c ->
      let rendered =
        Pp.to_string ~minify:(Pp.minified ctx) ~lossless:ctx.Pp.lossless
          pp_color c
      in
      (* CSS Syntax: a [#hex] hash token is unambiguous after an ident, so
         minified output drops the separating space. *)
      let leads_with_delim =
        Pp.minified ctx && String.length rendered > 0 && rendered.[0] = '#'
      in
      if not !first then if not leads_with_delim then Pp.space ctx ();
      first := false;
      Pp.string ctx rendered)
    color;
  if !first then Pp.string ctx "none"

let rec pp_border : border Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border ctx v
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | None -> Pp.string ctx "none"
  | Shorthand shorthand -> pp_border_shorthand ctx shorthand

let rec pp_logical_border_color : logical_border_color Pp.t =
 fun ctx -> function
  | Single color -> pp_color ctx color
  | Pair (start_, end_) ->
      pp_color ctx start_;
      Pp.space ctx ();
      pp_color ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_color ctx v

let rec pp_logical_border_width : logical_border_width Pp.t =
 fun ctx -> function
  | Single w -> pp_border_width ctx w
  | Pair (start_, end_) ->
      pp_border_width ctx start_;
      Pp.space ctx ();
      pp_border_width ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_width ctx v

let rec pp_logical_border_style : logical_border_style Pp.t =
 fun ctx -> function
  | Single s -> pp_border_style ctx s
  | Pair (start_, end_) ->
      pp_border_style ctx start_;
      Pp.space ctx ();
      pp_border_style ctx end_
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_logical_border_style ctx v

let rec pp_border_spacing : border_spacing Pp.t =
 fun ctx -> function
  | (Lengths lengths : border_spacing) ->
      Pp.list ~sep:Pp.space pp_length ctx lengths
  | Var v -> pp_var pp_border_spacing ctx v

(* CSS Tables 3 (ED) writes [border-spacing] as one or two non-negative lengths
   and reads a single one as "both the horizontal and vertical spacing", so a
   pair of equal lengths is the longer spelling of that one value. The per-side
   fold runs first, so a pair that only agrees once normalised collapses too. *)
and normalize_border_spacing : border_spacing -> border_spacing =
 fun value ->
  match value with
  | Lengths lengths -> (
      let normalized = map_preserve Values.normalize_length lengths in
      match normalized with
      | [ a; b ] when Values.equal_length a b -> Lengths [ a ]
      | _ when normalized == lengths -> value
      | _ -> Lengths normalized)
  | Var _ -> value

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

let pp_border_image_slice_item ctx (value : border_image_slice_item) =
  match value with Number n -> Values.pp_number ctx n | Pct n -> Pp.pct ctx n

let pp_border_image_slice_offsets ctx { offsets; fill } =
  Pp.list ~sep:Pp.space pp_border_image_slice_item ctx offsets;
  if fill then (
    Pp.space ctx ();
    Pp.string ctx "fill")

let rec pp_border_image_slice ctx (value : border_image_slice) =
  match value with
  | Slices offsets -> pp_border_image_slice_offsets ctx offsets
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_slice ctx v

let pp_border_image_width_item ctx (value : border_image_width_item) =
  match value with
  | Number n -> Values.pp_number ctx n
  | Pct n -> Pp.pct ctx n
  | Length len -> pp_length ctx len
  | Auto -> Pp.string ctx "auto"

let pp_border_image_outset_item ctx (value : border_image_outset_item) =
  match value with
  | Number n -> Values.pp_number ctx n
  | Length len -> pp_length ctx len

let pp_border_image_repeat_keyword ctx (value : border_image_repeat_keyword) =
  match value with
  | Stretch -> Pp.string ctx "stretch"
  | Repeat -> Pp.string ctx "repeat"
  | Round -> Pp.string ctx "round"
  | Space -> Pp.string ctx "space"

let rec pp_border_image_repeat ctx : border_image_repeat -> unit = function
  | Repeats l -> Pp.list ~sep:Pp.space pp_border_image_repeat_keyword ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_repeat ctx v

let rec pp_border_image_width ctx : border_image_width -> unit = function
  | Widths l -> Pp.list ~sep:Pp.space pp_border_image_width_item ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_width ctx v

let rec pp_border_image_outset ctx : border_image_outset -> unit = function
  | Outsets l -> Pp.list ~sep:Pp.space pp_border_image_outset_item ctx l
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"
  | Var v -> pp_var pp_border_image_outset ctx v

let pp_mask_border_mode ctx = function
  | (Alpha : mask_border_mode) -> Pp.string ctx "alpha"
  | Luminance -> Pp.string ctx "luminance"

(* CSS Backgrounds 3 sec. 6.1: a component the shorthand leaves out takes its
   longhand's initial - [none] for the source (sec. 5.1), [100%] for the slice
   (sec. 5.2), [1] for the width (sec. 5.3), [0] for the outset (sec. 5.4) and
   [stretch] for the repeat (sec. 5.5) - so writing one out names what leaving
   it out names. Drained of every slot the shorthand declares nothing but
   initials, which is what [none] declares, so the source stays to say it. *)
let normalize_border_image : border_image -> border_image =
 fun value ->
  let drop is_initial slot =
    match slot with Some v when is_initial v -> Option.None | slot -> slot
  in
  let source =
    drop
      (function (None : background_image) | Initial -> true | _ -> false)
      value.source
  in
  let slice_initial =
    drop
      (fun (s : border_image_slice_offsets) ->
        (not s.fill)
        &&
        match s.offsets with
        | [ (Pct 100. : border_image_slice_item) ] -> true
        | _ -> false)
      value.slice
  in
  let outset =
    drop
      (function
        | [ (Number (Num 0.) : border_image_outset_item) ] | [ Length Zero ] ->
            true
        | _ -> false)
      value.outset
  in
  (* The printer writes one [/] per slot, so a dropped width with a kept outset
     would put the outset in the width's place. The width only goes when the
     outset does. *)
  let width =
    if Option.is_some outset then value.width
    else
      drop
        (function
          | [ (Number (Num 1.) : border_image_width_item) ] -> true | _ -> false)
        value.width
  in
  let repeat =
    drop (fun r -> r = [ (Stretch : border_image_repeat_keyword) ]) value.repeat
  in
  (* The printer writes the slice before the first [/], so a kept width or
     outset needs it even at its initial. *)
  let slice =
    if Option.is_some width || Option.is_some outset then value.slice
    else slice_initial
  in
  let drained =
    Option.is_none source && Option.is_none slice && Option.is_none width
    && Option.is_none outset && Option.is_none repeat
  in
  let source = if drained then Some (None : background_image) else source in
  { value with source; slice; width; outset; repeat }

let pp_border_image : border_image Pp.t =
 fun ctx { source; slice; width; outset; repeat; mode } ->
  let first = ref true in
  (* CSS Syntax 3 (ED) sec. 9: tokens ending with [)] are self-delimiting, so
     the inter-slot space after [url(...)] / [<image>] can be elided under
     minify. *)
  let last_is_self_delim () =
    match Pp.last_char ctx with Some (')' | ']' | '}') -> true | _ -> false
  in
  let maybe_space () =
    if !first then first := false
    else if Pp.minified ctx && last_is_self_delim () then ()
    else Pp.space ctx ()
  in
  pp_bg_prop maybe_space pp_background_image ctx source;
  pp_bg_prop maybe_space pp_border_image_slice_offsets ctx slice;
  (match width with
  | None -> ()
  | Some width ->
      first := false;
      Pp.char ctx '/';
      Pp.list ~sep:Pp.space pp_border_image_width_item ctx width);
  (match outset with
  | None -> ()
  | Some outset ->
      first := false;
      Pp.char ctx '/';
      Pp.list ~sep:Pp.space pp_border_image_outset_item ctx outset);
  pp_bg_prop maybe_space
    (Pp.list ~sep:Pp.space pp_border_image_repeat_keyword)
    ctx repeat;
  pp_bg_prop maybe_space pp_mask_border_mode ctx mode;
  (* The record is public, so a caller can hand over a value with no slot
     filled. It declares nothing but the initial longhands, which is what
     [border-image: none] and [mask-border: none] declare (CSS Backgrounds 3
     (ED) sec. 5.7, CSS Masking 1 (ED) sec. 8.7); the empty string is not a
     value any parser reads back. *)
  if !first then Pp.string ctx "none"

(* A mask-border that fills no slot declares what [mask-border: none] declares,
   and CSS Masking 1 (ED) sec. 8.1 gives mask-border-source the initial value
   [none], so that keyword is where the two spellings meet. *)
let drained_mask_border : border_image =
  {
    source = Some (None : background_image);
    slice = None;
    width = None;
    outset = None;
    repeat = None;
    mode = None;
  }

(* CSS Masking 1 (ED) sec. 8.2 gives mask-border-mode the initial value [alpha],
   and sec. 8.7 sets an omitted shorthand slot to its initial value, so an
   explicit [alpha] declares what leaving the slot out declares. [luminance] is
   the other mode and stays. Written on its own the mode is the whole value, so
   dropping it drains the shorthand. *)
let normalize_mask_border (value : border_image) : border_image =
  match value.mode with
  | Some (Alpha : mask_border_mode) -> (
      match
        (value.source, value.slice, value.width, value.outset, value.repeat)
      with
      | None, None, None, None, None -> drained_mask_border
      | _ -> { value with mode = Option.None })
  | _ -> value

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

let rec pp_border_collapse : border_collapse Pp.t =
 fun ctx -> function
  | Var v -> pp_var pp_border_collapse ctx v
  | Collapse -> Pp.string ctx "collapse"
  | Separate -> Pp.string ctx "separate"
  | Inherit -> Pp.string ctx "inherit"
  | Initial -> Pp.string ctx "initial"
  | Unset -> Pp.string ctx "unset"
  | Revert -> Pp.string ctx "revert"
  | Revert_layer -> Pp.string ctx "revert-layer"

let rec read_border_style t : border_style =
  Cursor.enum_or_var "border-style"
    [
      ("none", (None : border_style));
      ("solid", Solid);
      ("dashed", Dashed);
      ("dotted", Dotted);
      ("double", Double);
      ("groove", Groove);
      ("ridge", Ridge);
      ("inset", Inset);
      ("outset", Outset);
      ("hidden", Hidden);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (read_var read_border_style t))
    t

(* CSS Backgrounds 3 (ED) sec. 3.2: [border-style] is [<line-style>{1,4}], the
   box over the four side styles, as sec. 3.1 gives [border-color] and sec. 3.3
   gives [border-width] theirs. A CSS-wide keyword reaches a slot only as the
   lone value; [validate_regular_property_components] rejects the mixes. *)
let read_border_style_box t : border_style list =
  Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:4 read_border_style t

(* Helper: ensure border-width values are non-negative per CSS spec *)
let ensure_non_negative_border_width t value =
  if value < 0.0 then
    err_invalid_value t "border-width" "negative values not allowed"
  else value

(* [border-width] takes a [<length>], so every dimension [length] carries is
   one. The units [border_width] names get their own arm; the rest keep their
   value and spelling in [Dimension], the way [length] itself does, so a unit
   [length] learns needs no second table here. *)
let length_to_border_width ?(allow_negative = false) t (length : length) :
    border_width =
  let non_neg v =
    if allow_negative then v else ensure_non_negative_border_width t v
  in
  let typed_dimension ?repr value unit : border_width =
    let value = non_neg value in
    match String.lowercase_ascii unit with
    | "%" -> Pct value
    | "px" -> Px value
    | "cm" -> Cm value
    | "mm" -> Mm value
    | "q" -> Q value
    | "in" -> In value
    | "pt" -> Pt value
    | "pc" -> Pc value
    | "rem" -> Rem value
    | "em" -> Em value
    | "ex" -> Ex value
    | "cap" -> Cap value
    | "ic" -> Ic value
    | "ric" -> Ric value
    | "rlh" -> Rlh value
    | "ch" -> Ch value
    | "lh" -> Lh value
    | "vh" -> Vh value
    | "vw" -> Vw value
    | "vmin" -> Vmin value
    | "vmax" -> Vmax value
    | _ ->
        let repr =
          match repr with Some repr -> repr | None -> Pp.string_of_float value
        in
        Dimension { value; unit; repr }
  in
  match length with
  | Zero -> Zero
  | Dimension { value; unit; repr } -> typed_dimension ~repr value unit
  | length -> (
      match calc_length_unit length with
      | Some (unit, value) -> typed_dimension value unit
      | None -> err_invalid_value t "border-width" "unsupported length type")

(* CSS Backgrounds 3 (ED) sec. 3.3: [<line-width>] is [<length [0,inf]> | thin |
   medium | thick] and takes no percentage, which Chrome 146 refuses.
   [length_only] refuses one nested in math as well. *)
let read_length_as_border_width ?(allow_negative = false) t =
  let length =
    read_length ~allow_negative ~with_keywords:false ~length_only:true t
  in
  length_to_border_width ~allow_negative t length

(* CSS Values 4 sec. 10.12: a math function is valid wherever its type is, and
   the [0,inf] range of [<line-width>] is checked on the value it resolves to,
   not on each operand. So [calc(-1px)] reads and a literal [-1px] does not. *)
let rec read_border_width_in_math t : border_width =
  read_border_width_with ~allow_negative:true t

and read_border_width_with ~allow_negative t : border_width =
  let read_var t : border_width =
    Var (read_var (read_border_width_with ~allow_negative) t)
  in
  let read_calc t : border_width =
    Calc (read_calc ~result_type:`Value read_border_width_in_math t)
  in
  let read_math_arg t = read_calc_expr read_border_width_in_math t in
  let read_min t : border_width =
    Min
      (Cursor.call "min" t
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg))
  in
  let read_max t : border_width =
    Max
      (Cursor.call "max" t
         (Cursor.list ~sep:Cursor.comma ~at_least:1 read_math_arg))
  in
  let read_clamp t : border_width =
    match
      Cursor.call "clamp" t
        (Cursor.list ~sep:Cursor.comma ~at_least:3 ~at_most:3 read_math_arg)
    with
    | [ lower; value; upper ] -> Clamp (lower, value, upper)
    | _ -> Cursor.err_invalid t "invalid clamp"
  in
  Cursor.enum_or_calls "border-width"
    [
      ("thin", (Thin : border_width));
      ("medium", Medium);
      ("thick", Thick);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~calls:
      [
        ("var", read_var);
        ("calc", read_calc);
        ("min", read_min);
        ("max", read_max);
        ("clamp", read_clamp);
      ]
    ~default:(read_length_as_border_width ~allow_negative)
    t

let read_border_width t : border_width =
  read_border_width_with ~allow_negative:false t

module Border = struct
  type component =
    | Width of border_width
    | Style of border_style
    | Color of color

  type components = {
    width : border_width option;
    style : border_style option;
    color : color option;
  }

  let empty = { width = None; style = None; color = None }

  let read_component t =
    Cursor.one_of
      [
        (fun t -> Color (read_color t));
        (fun t -> Width (read_border_width t));
        (fun t -> Style (read_border_style t));
      ]
      t

  let merge t acc = function
    | Width w when acc.width = None -> { acc with width = Some w }
    | Style s when acc.style = None -> { acc with style = Some s }
    | Color c when acc.color = None -> { acc with color = Some c }
    | Width _ -> Cursor.err_invalid t "duplicate border width"
    | Style _ -> Cursor.err_invalid t "duplicate border style"
    | Color _ -> Cursor.err_invalid t "duplicate border color"

  let to_shorthand (components : components) : border_shorthand =
    {
      width = components.width;
      style = components.style;
      color = components.color;
    }
end

(* CSS Backgrounds 3 (ED) sec. 3.4 writes the shorthand as [<line-width> ||
   <line-style> || <color>], and CSS Values 4 (ED) sec. 2.2 has [||] require one
   or more of its options to occur, so an empty value matches no border grammar.
   CSS Syntax 3 (ED) sec. 5.5.6 keeps only a declaration valid in its context,
   so the whole declaration goes; filling the slots in from nowhere would
   declare something the author did not write. *)
let read_border_shorthand t : border_shorthand =
  (* A [var()] in the border shorthand is type-ambiguous (it could substitute a
     width, style, or colour), so it cannot be assigned by matching a typed
     reader - assign it to the next unfilled slot in width/style/colour order
     and read its fallback with that slot's reader. Concrete components still
     bind by type. *)
  let acc = ref Border.empty in
  let consumed = ref true in
  while !consumed do
    Cursor.ws t;
    consumed :=
      if Cursor.is_done t || Cursor.peek_comma t then false
      else if Cursor.looking_at_func "var" t then (
        let a = !acc in
        acc :=
          if a.width = Option.None then
            { a with width = Some (Var (read_var read_border_width t)) }
          else if a.style = Option.None then
            { a with style = Some (Var (read_var read_border_style t)) }
          else if a.color = Option.None then
            { a with color = Some (Var (read_var read_color t)) }
          else Cursor.err_invalid t "too many border components";
        true)
      else
        match Cursor.option Border.read_component t with
        | Some c ->
            acc := Border.merge t !acc c;
            true
        | Option.None -> false
  done;
  let acc = !acc in
  if
    Option.is_none acc.width && Option.is_none acc.style
    && Option.is_none acc.color
  then Cursor.err_expected t "border width, style or color";
  Border.to_shorthand acc

let border_keyword = function
  | "inherit" -> Some (Inherit : border)
  | "initial" -> Some Initial
  | "unset" -> Some Unset
  | "revert" -> Some Revert
  | "revert-layer" -> Some Revert_layer
  | "none" -> Some None
  | _ -> None

let read_border_shorthand_from t snap : border =
  Cursor.restore t snap;
  Shorthand (read_border_shorthand t)

let read_border_keyword_or_shorthand t : border =
  let snap = Cursor.save t in
  match Cursor.ident_opt t with
  | Some ident -> (
      match border_keyword (String.lowercase_ascii ident) with
      | Some value ->
          Cursor.ws t;
          if Cursor.is_done t || Cursor.peek_comma t then value
          else read_border_shorthand_from t snap
      | None -> read_border_shorthand_from t snap)
  | None -> read_border_shorthand_from t snap

let rec read_border (t : Cursor.t) : border =
  if Cursor.looking_at_func "var" t then (
    (* A lone [var()] is the whole value; a [var()] followed by more components
       is a shorthand whose first component happens to be a var. *)
    let snap = Cursor.save t in
    let v = Values.read_var read_border t in
    Cursor.ws t;
    if Cursor.is_done t || Cursor.peek_comma t then (Var v : border)
    else (
      Cursor.restore t snap;
      read_border_keyword_or_shorthand t))
  else read_border_keyword_or_shorthand t

let rec read_logical_border_color t : logical_border_color =
  let read_colors t =
    match Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_color t with
    | [ color ] -> (Single color : logical_border_color)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two colors"
  in
  Cursor.enum_or_var "logical border color"
    [
      ("inherit", (Inherit : logical_border_color));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_color t))
    ~default:read_colors t

let rec read_logical_border_width t : logical_border_width =
  let read_widths t =
    match
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_border_width t
    with
    | [ w ] -> (Single w : logical_border_width)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two widths"
  in
  Cursor.enum_or_var "logical border width"
    [
      ("inherit", (Inherit : logical_border_width));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_width t))
    ~default:read_widths t

(* CSS Logical 1 (ED) sec. 4.5.2: [border-inline-style] and [border-block-style]
   are [<'border-top-style'>{1,2}], the first value the start edge and the
   second the end edge. *)
let rec read_logical_border_style t : logical_border_style =
  let read_styles t =
    match
      Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_border_style t
    with
    | [ s ] -> (Single s : logical_border_style)
    | [ start_; end_ ] -> Pair (start_, end_)
    | _ -> Cursor.err_expected t "one or two line styles"
  in
  Cursor.enum_or_var "logical border style"
    [
      ("inherit", (Inherit : logical_border_style));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_logical_border_style t))
    ~default:read_styles t

let rec read_border_collapse t : border_collapse =
  Cursor.enum_or_var "border-collapse"
    [
      ("collapse", (Collapse : border_collapse));
      ("separate", Separate);
      ("inherit", Inherit);
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t ->
      (Var (Values.read_var read_border_collapse t) : border_collapse))
    t

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

let shadow ?(inset = false) ?(inset_var : string option)
    ?(inset_var_no_fallback = false) ?(h_offset : length option)
    ?(v_offset : length option) ?(blur : length option)
    ?(spread : length option) ?(color : color option) () : shadow =
  let default_color = rgb_black in
  let body =
    {
      h_offset = Option.value h_offset ~default:(Px 0.);
      v_offset = Option.value v_offset ~default:(Px 0.);
      blur;
      spread;
      color = Some (Option.value color ~default:default_color);
    }
  in
  match inset_var with
  | Some name ->
      Inset (Toggle { name; no_fallback = inset_var_no_fallback; body })
  | None -> if inset then Inset (Body body) else Shadow body

let border_shorthand ?width ?style ?color () : border =
  Shorthand { width; style; color }

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
let normalize_background_position_axis :
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
        match Cursor.peek_ident t with
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

let read_background_position_x t =
  read_background_position_axis ~label:"background-position-x" ~start_edge:Left
    ~end_edge:Right t

let read_background_position_y t =
  read_background_position_axis ~label:"background-position-y" ~start_edge:Top
    ~end_edge:Bottom t

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

let read_background_var_call read_self t : background =
  let first = read_var read_self t in
  match read_background_vars read_self t with
  | [] -> Var first
  | rest -> Vars (first :: rest)

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

let read_backgrounds t : background list =
  Cursor.list ~sep:Cursor.comma ~at_least:1 read_background t

(* CSS Backgrounds 3 sec. 5.1: [border-radius = <length-percentage [0,inf]>{1,4}
   [ / <length-percentage [0,inf]>{1,4} ]?]. Reads 1-4 horizontal radii then,
   after [/], 1-4 vertical radii. No keyword is a radius: the intrinsic-sizing
   keywords are no part of the grammar, and the CSS-wide keywords are only the
   whole property value (CSS Cascade 5 sec. 6), which [read_border_radius] takes
   before delegating here. Basic shapes spell their rounded-corner suffix [round
   <'border-radius'>] (CSS Shapes 1 sec. 3.1), a reference to this same value
   definition, so they read radii through [read_border_radius_inline] and get
   the keyword exclusion with it. *)
let read_border_radius_inline_radii t =
  let rec loop acc count =
    if count >= 4 then List.rev acc
    else
      match
        Cursor.option
          (read_length_percentage ~allow_negative:false ~with_keywords:false)
          t
      with
      | None -> List.rev acc
      | Some lp -> loop (lp :: acc) (count + 1)
  in
  match loop [] 0 with
  | [] -> Cursor.err_expected t "<length-percentage>"
  | radii -> radii

let read_border_radius_inline_vertical t =
  match Cursor.peek_delim t with
  | Some '/' ->
      Cursor.skip t;
      Cursor.ws t;
      Some (read_border_radius_inline_radii t)
  | _ -> None

let read_border_radius_inline t : border_radius =
  Cursor.ws t;
  let horizontal = read_border_radius_inline_radii t in
  Cursor.ws t;
  let vertical = read_border_radius_inline_vertical t in
  Radius { horizontal; vertical }

let rec read_border_radius (t : Cursor.t) : border_radius =
  Cursor.enum_or_whole_value_var "border-radius"
    [
      ("inherit", (Inherit : border_radius));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> (Var (Values.read_var read_border_radius t) : border_radius))
    ~default:read_border_radius_inline t

(* A zero-valued border-width unit ([0px], [0em]) is the bare length [0]
   ([border-width: 0] = [border-width: 0px]); collapse it to [Zero] like every
   other zero length ([width: 0px] -> [0]), which [<length>]-valued properties
   already do via [normalize_length]. *)
let border_width_is_zero (bw : border_width) =
  match length_of_border_width bw with
  | Some l -> (
      match Values.normalize_length l with Values.Zero -> true | _ -> false)
  | None -> false

let normalize_border_width (bw : border_width) : border_width =
  match bw with
  | Calc c -> (
      match normalize_border_width_calc c with
      (* The call stays around a negative for the reason {!pp_border_width}
         keeps it: sec. 10.12 clamps [calc(-1px)] at used-value time and drops a
         bare [-1px]. *)
      | Val v when not (negative_border_width v) -> v
      | folded -> Calc folded)
  | Min args -> normalize_border_width_minmax `Min args
  | Max args -> normalize_border_width_minmax `Max args
  | Clamp (lower, value, upper) ->
      normalize_border_width_clamp lower value upper
  | _ when border_width_is_zero bw -> Zero
  | _ -> bw

(* [<line-width>] and [<length>] share every shape but the three keywords, so
   read the substitution question in length space; a node that does not map over
   ([var()], a sibling index) answers as substituting, the safe side for a
   caller deciding whether the value can be folded into a shorthand. *)
let border_width_has_runtime_subst (bw : border_width) : bool =
  let calc c =
    match length_of_border_width_calc c with
    | Some lc -> Values.length_has_runtime_subst (Calc lc)
    | None -> true
  in
  match bw with
  | Var _ -> true
  | Calc c -> calc c
  | Min args | Max args -> List.exists calc args
  | Clamp (lower, value, upper) -> calc lower || calc value || calc upper
  | _ -> false

(* CSS Logical 1 sec. 4.3 and 4.4: an axis shorthand takes [<side>{1,2}], so a
   repeated side has a shorter spelling naming the same two sides, exactly as
   [collapse_box_shorthand] picks one for the four-side families. An arbitrary
   substitution defers the component count to computed-value time, so a pair
   holding one keeps both. *)
let normalize_logical_border_width :
    logical_border_width -> logical_border_width =
 fun value ->
  match value with
  | Single w -> Single (normalize_border_width w)
  | Pair (a, b) ->
      let a = normalize_border_width a and b = normalize_border_width b in
      if
        (not (is_border_width_substitution a))
        && (not (is_border_width_substitution b))
        && equal_border_width a b
      then Single a
      else Pair (a, b)
  | other -> other

(* [border-style] takes keywords, so the axis has nothing to canonicalise per
   value and only the spelling is at stake. *)
let normalize_logical_border_style :
    logical_border_style -> logical_border_style =
 fun value ->
  match value with
  | Pair (a, b)
    when (not (is_border_style_substitution a))
         && (not (is_border_style_substitution b))
         && equal_border_style a b ->
      Single a
  | other -> other

(* CSS Backgrounds 3 (ED) sec. 3.4: a shorthand sets every longhand it covers,
   so an omitted slot takes its initial value - sec. 3.3 makes that [medium] for
   the width, sec. 3.2 [none] for the style and sec. 3.1 [currentColor] for the
   colour. An explicit initial value therefore declares what leaving the slot
   out declares, and the shorter spelling wins. *)
let drop_initial_line_width (width : border_width option) : border_width option
    =
  match width with Some Medium -> Option.None | width -> width

let drop_initial_line_style (style : border_style option) : border_style option
    =
  match style with Some (None : border_style) -> Option.None | style -> style

(* CSS Multi-column 1 (ED) sec. 4.2 and css-logical-1 sec. 4.5.3 give
   column-rule and the logical borders the same initial colour, and they read
   this production. CSS UI 4 (ED) sec. 3.4 does not: outline-color starts at
   [auto], which [currentColor] does not name, so [normalize_outline] has no
   colour drop. *)
let drop_initial_line_color (color : color option) : color option =
  match color with Some (Current : color) -> Option.None | color -> color

(* The width slot of the border shorthands is a [<'border-width'>], so it takes
   the same fold as the longhand; the style slot is a [<line-style>] and the
   colour slot a [<color>]. Drained of every slot the shorthand declares nothing
   but initial values, which is what [none] declares, and [none] is the node the
   keyword parses to - so the two spellings meet there. *)
let normalize_border ?(lossless = false) : border -> border =
 fun value ->
  match value with
  | Shorthand s ->
      let width =
        drop_initial_line_width
          (option_map_preserve normalize_border_width s.width)
      in
      let style = drop_initial_line_style s.style in
      let color =
        drop_initial_line_color
          (option_map_preserve (normalize_color ~lossless) s.color)
      in
      if width == s.width && style == s.style && color == s.color then value
      else if
        Option.is_none width && Option.is_none style && Option.is_none color
      then (None : border)
      else Shorthand { width; style; color }
  | other -> other

let read_border_image_repeat_keyword t : border_image_repeat_keyword =
  Cursor.enum "border-image-repeat"
    [
      ("stretch", (Stretch : border_image_repeat_keyword));
      ("repeat", Repeat);
      ("round", Round);
      ("space", Space);
    ]
    t

let rec read_border_spacing t : border_spacing =
  let read_numeric_length t =
    let l = read_length ~allow_negative:false ~length_only:true t in
    match l with
    | Auto | Size | None | Normal | Fit_content | Content | Contain
    | Max_content | Min_content | From_font | Hairline | Thin | Medium | Thick
    | Stretch ->
        Cursor.err_invalid t "border-spacing requires a <length>"
    | _ -> l
  in
  Cursor.enum_or_whole_value_var "border-spacing" []
    ~var:(fun t -> Var (Values.read_var read_border_spacing t))
    ~default:(fun t ->
      (Lengths
         (Cursor.list ~sep:Cursor.ws ~at_least:1 ~at_most:2 read_numeric_length
            t)
        : border_spacing))
    t

(* Sec. 5.2 to 5.4 keep the numeric halves at [0,inf]. A [calc()] holds no value
   to compare, so only a literal is turned away here. Each side is one component
   of a list, and the whole-value number reader refuses a number after the one
   it read, so the component is handed to it on its own. *)
let read_border_image_number t =
  let value : number =
    match Cursor.peek t with
    | Some (Component.Func _ as component) ->
        let _ = Cursor.next t in
        Values.read_number (Cursor.of_components [ component ])
    | _ -> Num (Cursor.number t)
  in
  (match value with
  | (Num n : number) when n < 0. ->
      Cursor.err_invalid t "border-image value cannot be negative"
  | _ -> ());
  value

let read_border_image_slice_item t : border_image_slice_item =
  match Cursor.percentage_opt t with
  | Some n when n >= 0. -> Pct n
  | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
  | None -> Number (read_border_image_number t)

let read_border_image_slice_value t values has_fill =
  match Cursor.option read_border_image_slice_item t with
  | Some value ->
      if List.length values >= 4 then
        Cursor.err_invalid t "too many border-image slice values";
      `Continue (value :: values, has_fill)
  | None -> `Stop

let read_border_image_slice_step t values has_fill =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then `Stop
  else
    match Cursor.peek_ident t with
    | Some "fill" ->
        if has_fill then
          Cursor.err_invalid t "duplicate border-image fill keyword";
        let _ = Cursor.ident t in
        `Continue (values, true)
    | _ -> read_border_image_slice_value t values has_fill

let read_border_image_slice_offsets t : border_image_slice_offsets =
  let rec loop values has_fill =
    match read_border_image_slice_step t values has_fill with
    | `Stop -> (values, has_fill)
    | `Continue (values, has_fill) -> loop values has_fill
  in
  let values, has_fill = loop [] false in
  match (List.rev values, has_fill) with
  | [], true -> Cursor.err_invalid t "border-image fill requires slice values"
  | [], false -> Cursor.err_expected t "border-image slice"
  | offsets, fill -> { offsets; fill }

(* CSS Cascade 5 sec. 7.3 gives the longhand the CSS-wide keywords; the
   shorthand takes offsets alone. *)
let rec read_border_image_slice t : border_image_slice =
  match Cursor.peek_ident t with
  | Some ("initial" | "inherit" | "unset" | "revert" | "revert-layer" | "var")
    ->
      Cursor.enum_or_var "border-image-slice"
        [
          ("initial", (Initial : border_image_slice));
          ("inherit", Inherit);
          ("unset", Unset);
          ("revert", Revert);
          ("revert-layer", Revert_layer);
        ]
        ~var:(fun t -> Var (Values.read_var read_border_image_slice t))
        t
  | Some _ | None -> Slices (read_border_image_slice_offsets t)

let read_border_image_width_item t : border_image_width_item =
  let auto t =
    Cursor.enum "border-image-width"
      [ ("auto", (Auto : border_image_width_item)) ]
      t
  in
  match Cursor.option auto t with
  | Some value -> value
  | None -> (
      match Cursor.percentage_opt t with
      | Some n when n >= 0. -> Pct n
      | Some _ -> Cursor.err_invalid t "border-image value cannot be negative"
      | None ->
          Cursor.one_of
            [
              (fun t ->
                (Number (read_border_image_number t) : border_image_width_item));
              (* Sec. 5.3 gives the width a number, a length-percentage or
                 [auto], read above. The generic length reader carries keywords
                 of its own - [stretch] and [contain] among them, which name a
                 repeat here - so this call takes none. *)
              (fun t ->
                Length
                  (read_length ~allow_negative:false ~with_keywords:false t));
            ]
            t)

(* Sec. 5.4 gives the outset a number or a length per side and no keyword. *)
let read_border_image_outset_item t : border_image_outset_item =
  Cursor.one_of
    [
      (fun t ->
        (Number (read_border_image_number t) : border_image_outset_item));
      (* Sec. 5.4 gives the outset a number or a length, and no percentage. *)
      (fun t ->
        Length
          (read_length ~allow_negative:false ~with_keywords:false
             ~length_only:true t));
    ]
    t

let read_border_image_box_step ~what read_item t acc =
  Cursor.ws t;
  if Cursor.is_done t || Cursor.peek_delim t = Some '/' then `Stop
  else
    match Cursor.option read_item t with
    | Some value ->
        if List.length acc >= 4 then
          Cursor.err_invalid t ("too many border-image " ^ what ^ " values");
        `Continue (value :: acc)
    | None -> `Stop

let read_border_image_box_values ~what read_item t =
  let rec loop acc =
    match read_border_image_box_step ~what read_item t acc with
    | `Stop -> List.rev acc
    | `Continue acc -> loop acc
  in
  match loop [] with
  | [] -> Cursor.err_expected t ("border-image " ^ what)
  | values -> values

let read_border_image_repeat_keywords t =
  let first = read_border_image_repeat_keyword t in
  Cursor.ws t;
  match Cursor.option read_border_image_repeat_keyword t with
  | None -> [ first ]
  | Some second -> [ first; second ]

let rec read_border_image_repeat t : border_image_repeat =
  Cursor.enum_or_var "border-image-repeat"
    [
      ("inherit", (Inherit : border_image_repeat));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_repeat t))
    ~default:(fun t -> Repeats (read_border_image_repeat_keywords t))
    t

let rec read_border_image_width t : border_image_width =
  Cursor.enum_or_whole_value_var "border-image-width"
    [
      ("inherit", (Inherit : border_image_width));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_width t))
    ~default:(fun t ->
      Widths
        (read_border_image_box_values ~what:"width" read_border_image_width_item
           t))
    t

let rec read_border_image_outset t : border_image_outset =
  Cursor.enum_or_whole_value_var "border-image-outset"
    [
      ("inherit", (Inherit : border_image_outset));
      ("initial", Initial);
      ("unset", Unset);
      ("revert", Revert);
      ("revert-layer", Revert_layer);
    ]
    ~var:(fun t -> Var (Values.read_var read_border_image_outset t))
    ~default:(fun t ->
      Outsets
        (read_border_image_box_values ~what:"outset"
           read_border_image_outset_item t))
    t

let read_mask_border_mode t =
  Cursor.enum "mask-border-mode"
    [ ("alpha", (Alpha : mask_border_mode)); ("luminance", Luminance) ]
    t

(* CSS Backgrounds 3 (ED) sec. 5.7 gives border-image a source, a slice with its
   width and outset, and a repeat. CSS Masking 1 (ED) sec. 8.7 gives mask-border
   those same slots and one more, [|| <'mask-border-mode'>]. Nothing else tells
   the two grammars apart, so [mask_mode] is what says which one the reader is
   holding. CSS Values 4 (ED) sec. 2.2 has [||] ask for one or more of its
   options, so a value that fills no slot matches neither grammar. *)
let read_border_image_shorthand ~mask_mode t : border_image =
  let read_mode t =
    if mask_mode then Cursor.option read_mask_border_mode t
    else (None : mask_border_mode option)
  in
  let source = Cursor.option read_background_image t in
  Cursor.ws t;
  (* sec. 8.7 puts [mask-border-mode] in [||] combination with the other slots,
     so the keyword may appear after [<source>] (before the slice) or after
     [<repeat>]. Try the early slot first; combine with the trailing slot
     below. *)
  let mode_early = read_mode t in
  Cursor.ws t;
  let slice = Cursor.option read_border_image_slice_offsets t in
  let width, outset =
    Cursor.ws t;
    if Cursor.slash_opt t then (
      let width =
        Some
          (read_border_image_box_values ~what:"width"
             read_border_image_width_item t)
      in
      Cursor.ws t;
      if Cursor.slash_opt t then
        ( width,
          Some
            (read_border_image_box_values ~what:"outset"
               read_border_image_outset_item t) )
      else (width, None))
    else (None, None)
  in
  Cursor.ws t;
  let repeat = Cursor.option read_border_image_repeat_keywords t in
  Cursor.ws t;
  let mode_late : mask_border_mode option =
    if Option.is_some mode_early then (None : mask_border_mode option)
    else read_mode t
  in
  let mode = match mode_early with Some _ -> mode_early | None -> mode_late in
  (match (source, slice, repeat, mode) with
  | None, None, None, None ->
      Cursor.err_expected t
        (if mask_mode then "mask-border source, slice, repeat, or mode"
         else "border-image source, slice, or repeat")
  | _ -> ());
  { source; slice; width; outset; repeat; mode }

let read_border_image t : border_image =
  read_border_image_shorthand ~mask_mode:false t

let read_mask_border t : border_image =
  read_border_image_shorthand ~mask_mode:true t
