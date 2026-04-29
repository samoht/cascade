(** CSS container query condition types *)

type t =
  | Min_width_rem of float
  | Min_width_px of int
  | Named of string * t
  | Style of string * string option
  | Scroll_state of string * string
  | Feature_query of Media.t
  | Custom of Media.t

(* Format float without trailing period (24. -> 24, 24.5 -> 24.5) *)
let format_rem f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let rec to_string = function
  | Min_width_rem rem -> "(min-width:" ^ format_rem rem ^ "rem)"
  | Min_width_px px -> "(min-width:" ^ Int.to_string px ^ "px)"
  | Named (name, cond) -> name ^ " " ^ to_string cond
  | Style (name, None) -> "style(" ^ name ^ ")"
  | Style (name, Some value) -> "style(" ^ name ^ ": " ^ value ^ ")"
  | Scroll_state (name, value) -> "scroll-state(" ^ name ^ ": " ^ value ^ ")"
  | Feature_query f -> Media.to_string f
  | Custom cond -> Media.to_string cond

let pp = to_string

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Style (n1, v1), Style (n2, v2) -> (
      match String.compare n1 n2 with
      | 0 -> Option.compare String.compare v1 v2
      | cmp -> cmp)
  | Scroll_state (n1, v1), Scroll_state (n2, v2) -> (
      match String.compare n1 n2 with 0 -> String.compare v1 v2 | cmp -> cmp)
  | Feature_query q1, Feature_query q2 -> Media.compare q1 q2
  | Custom c1, Custom c2 -> Media.compare c1 c2
  (* Order: Min_width_rem < Min_width_px < Named < Style < Scroll_state <
     Feature_query < Custom *)
  | Min_width_rem _, _ -> -1
  | _, Min_width_rem _ -> 1
  | Min_width_px _, _ -> -1
  | _, Min_width_px _ -> 1
  | Named _, (Style _ | Scroll_state _ | Feature_query _ | Custom _) -> -1
  | (Style _ | Scroll_state _ | Feature_query _ | Custom _), Named _ -> 1
  | Style _, (Scroll_state _ | Feature_query _ | Custom _) -> -1
  | (Scroll_state _ | Feature_query _ | Custom _), Style _ -> 1
  | Scroll_state _, (Feature_query _ | Custom _) -> -1
  | (Feature_query _ | Custom _), Scroll_state _ -> 1
  | Feature_query _, Custom _ -> -1
  | Custom _, Feature_query _ -> 1

type kind = Min_width | Other

let rec kind = function
  | Min_width_rem _ | Min_width_px _ -> Min_width
  | Named (_, cond) -> kind cond
  | Style _ | Scroll_state _ | Feature_query _ | Custom _ -> Other

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '-'

let is_ident_cont c = is_ident_start c || (c >= '0' && c <= '9')

let first_non_ws s =
  let rec loop i =
    if i >= String.length s then None
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' | '\012' -> loop (i + 1)
      | c -> Some (i, c)
  in
  loop

let split_named s =
  let s = String.trim s in
  let len = String.length s in
  let rec ident_end i =
    if i < len && is_ident_cont s.[i] then ident_end (i + 1) else i
  in
  if len = 0 || not (is_ident_start s.[0]) then None
  else
    let stop = ident_end 0 in
    match first_non_ws s stop with
    | Some (i, ('(' | 's')) when stop > 0 && i > stop ->
        Some (String.sub s 0 stop, String.sub s i (len - i))
    | _ -> None

let style_body body =
  let body = String.trim body in
  if body = "" then failwith "empty style() container query";
  if String.contains body ':' then
    match String.split_on_char ':' body with
    | [ name; value ] when String.trim name <> "" && String.trim value <> "" ->
        Style (String.trim name, Some (String.trim value))
    | _ -> failwith "invalid style() container query"
  else if String.length body >= 2 && body.[0] = '-' && body.[1] = '-' then
    Style (body, None)
  else failwith "invalid style() container query"

let scroll_state_body body =
  match String.split_on_char ':' (String.trim body) with
  | [ name; value ] -> (
      match (String.trim name, String.trim value) with
      | "stuck", ("top" | "right" | "bottom" | "left")
      | "snapped", ("block" | "inline" | "both") ->
          Scroll_state (String.trim name, String.trim value)
      | _ -> failwith "invalid scroll-state() container query")
  | _ -> failwith "invalid scroll-state() container query"

type query_surface =
  | Style_func of { canonical_name : bool; body : string }
  | Scroll_state_func of { canonical_name : bool; body : string }
  | Parenthesized_feature
  | Other_query

type balance = Balanced | Unbalanced
type range_direction = Lt_range | Gt_range

let paren_balance raw =
  let rec loop depth i =
    if i = String.length raw then if depth = 0 then Balanced else Unbalanced
    else
      match raw.[i] with
      | '(' -> loop (depth + 1) (i + 1)
      | ')' when depth > 0 -> loop (depth - 1) (i + 1)
      | ')' -> Unbalanced
      | _ -> loop depth (i + 1)
  in
  loop 0 0

let range_direction_of_component = function
  | Component.Preserved { kind = Token.Delim "<"; _ } -> Some Lt_range
  | Component.Preserved { kind = Token.Delim ">"; _ } -> Some Gt_range
  | _ -> None

let rec strip_ws = function
  | Component.Preserved { kind = Token.Whitespace; _ } :: rest -> strip_ws rest
  | cvs -> cvs

let non_ws cvs =
  List.filter
    (function
      | Component.Preserved { kind = Token.Whitespace; _ } -> false | _ -> true)
    cvs

let has_opposing_interval_components cvs =
  let rec first_op = function
    | [] -> None
    | cv :: rest -> (
        match range_direction_of_component cv with
        | Some _ as op -> op
        | None -> first_op rest)
  in
  let rec second_op seen_first = function
    | [] -> None
    | cv :: rest -> (
        match (range_direction_of_component cv, seen_first) with
        | Some _, false -> second_op true rest
        | Some op, true -> Some op
        | None, _ -> second_op seen_first rest)
  in
  match (first_op cvs, second_op false cvs) with
  | Some Lt_range, Some Gt_range | Some Gt_range, Some Lt_range -> true
  | _ -> false

let has_dangling_range_operator cvs =
  match List.rev (non_ws cvs) with
  | Component.Preserved { kind = Token.Delim ("<" | ">"); _ } :: _ -> true
  | _ -> false

let classify_query_surface raw =
  match paren_balance raw with
  | Unbalanced -> failwith "unmatched container query parentheses"
  | Balanced -> (
      let cursor = Cursor.of_string raw in
      match Cursor.remaining cursor with
      | [ Component.Func { node = { name; arguments; terminated }; _ } ] -> (
          if not terminated then failwith "unmatched container query function";
          let lower = String.lowercase_ascii name in
          let canonical_name = name = lower in
          let body = Cursor.components_to_string ~trim:true arguments in
          match lower with
          | "style" -> Style_func { canonical_name; body }
          | "scroll-state" -> Scroll_state_func { canonical_name; body }
          | _ -> Other_query)
      | [
       Component.Block
         { node = { opening = Token.Paren; value = _ :: _ as value }; _ };
      ] ->
          let value = strip_ws value in
          if has_dangling_range_operator value then
            failwith "dangling range operator in container query";
          if has_opposing_interval_components value then
            failwith "opposing interval operators in container query";
          Parenthesized_feature
      | _ -> Other_query)

(* Try to lift a typed [Media.t] that wraps a single feature into the more
   precise [Feature_query (feature)]. Anything richer (logical combinations,
   media-type prefixes, etc.) stays in [Custom]. *)
let single_feature_of_media (media : Media.t) =
  match media with
  | Media.Range _ | Media.Range_rev _ | Media.Interval _ | Media.Boolean _ ->
      Some media
  | _ -> None

let parse_container_specific raw =
  let raw = String.trim raw in
  if raw = "" then failwith "empty container query";
  match classify_query_surface raw with
  | Style_func { canonical_name = true; body } -> style_body body
  | Style_func { canonical_name = false; body } -> style_body body
  | Scroll_state_func { canonical_name = true; body } -> scroll_state_body body
  | Scroll_state_func { canonical_name = false; body } -> scroll_state_body body
  | Parenthesized_feature -> failwith "unrecognised container feature query"
  | Other_query -> failwith "not a container-specific query"

let parse_unnamed s =
  match Media.of_string s with
  | Media.Min_width_rem rem -> Min_width_rem rem
  | Media.Min_width px when Float.is_integer px ->
      Min_width_px (int_of_float px)
  | media -> (
      match single_feature_of_media media with
      | Some f -> Feature_query f
      | None -> Custom media)
  | exception Failure _ -> parse_container_specific s

let of_string s =
  match split_named s with
  | Some (name, raw) -> Named (name, parse_unnamed raw)
  | None -> parse_unnamed s

let feature name value = Feature_query (Media.feature name value)
let style ?value prop = Style (prop, value)
let scroll_state prop value = Scroll_state (prop, value)
