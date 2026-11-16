open Common
open Values
open Properties_intf
open Prop_common

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
    let lengths, color =
      take_trailing_color_var (List.rev !lengths_rev) !color
    in
    match read_lengths lengths with
    | Some (h_offset, v_offset, blur, spread) ->
        let body = { h_offset; v_offset; blur; spread; color } in
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

(* CSS Backgrounds 3 sec. 6.1 spells the property [none | <shadow>#], so [none]
   names the whole value and is no item of the list. *)
let read_shadow t : shadow =
  match Cursor.list ~sep:Cursor.comma ~at_least:1 read_shadow_single t with
  | [ x ] -> x
  | l ->
      let whole_value : shadow -> bool = function
        | None | Inherit | Initial | Unset | Revert | Revert_layer -> true
        | Shadow _ | Inset _ | List _ | Var _ -> false
      in
      if List.exists whole_value l then
        Cursor.err_invalid t "box-shadow keyword beside another shadow";
      List l

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
