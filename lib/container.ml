(** CSS container query condition types *)

type t =
  | Min_width_rem of float
  | Min_width_px of int
  | Named of string * t
  | Style of { name : string; value : string option; uppercase : bool }
  | Scroll_state of { name : string; value : string; uppercase : bool }
  | And of t * t
  | Or of t * t
  | Not of t
  | Feature_query of Media.t

(* Format float without trailing period (24. -> 24, 24.5 -> 24.5) *)
let format_rem f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let rec to_string = function
  | Min_width_rem rem -> "(min-width:" ^ format_rem rem ^ "rem)"
  | Min_width_px px -> "(min-width:" ^ Int.to_string px ^ "px)"
  | Named (name, cond) -> name ^ " " ^ to_string cond
  | Style { name; value = None; uppercase } ->
      let head = if uppercase then "STYLE(" else "style(" in
      head ^ name ^ ")"
  | Style { name; value = Some value; uppercase } ->
      let head = if uppercase then "STYLE(" else "style(" in
      head ^ name ^ ": " ^ value ^ ")"
  | Scroll_state { name; value; uppercase } ->
      let head = if uppercase then "SCROLL-STATE(" else "scroll-state(" in
      head ^ name ^ ": " ^ value ^ ")"
  | And (a, b) -> "(" ^ to_string a ^ " and " ^ to_string b ^ ")"
  | Or (a, b) -> "(" ^ to_string a ^ " or " ^ to_string b ^ ")"
  | Not c -> "(not " ^ to_string c ^ ")"
  | Feature_query f -> Media.to_string f

let pp = to_string

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Style { name = n1; value = v1; _ }, Style { name = n2; value = v2; _ } -> (
      match String.compare n1 n2 with
      | 0 -> Option.compare String.compare v1 v2
      | cmp -> cmp)
  | ( Scroll_state { name = n1; value = v1; _ },
      Scroll_state { name = n2; value = v2; _ } ) -> (
      match String.compare n1 n2 with 0 -> String.compare v1 v2 | cmp -> cmp)
  | And (a1, b1), And (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Or (a1, b1), Or (a2, b2) ->
      let c = compare a1 a2 in
      if c <> 0 then c else compare b1 b2
  | Not a, Not b -> compare a b
  | Feature_query q1, Feature_query q2 -> Media.compare q1 q2
  | _ -> Stdlib.compare t1 t2

type kind = Min_width | Other

let rec kind = function
  | Min_width_rem _ | Min_width_px _ -> Min_width
  | Named (_, cond) -> kind cond
  | And _ | Or _ | Not _ | Style _ | Scroll_state _ | Feature_query _ -> Other

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

let style_body ~uppercase body =
  let body = String.trim body in
  if body = "" then failwith "empty style() container query";
  if String.contains body ':' then
    match String.split_on_char ':' body with
    | [ name; value ] when String.trim name <> "" && String.trim value <> "" ->
        Style
          {
            name = String.trim name;
            value = Some (String.trim value);
            uppercase;
          }
    | _ -> failwith "invalid style() container query"
  else if String.length body >= 2 && body.[0] = '-' && body.[1] = '-' then
    Style { name = body; value = None; uppercase }
  else failwith "invalid style() container query"

let scroll_state_body ~uppercase body =
  match String.split_on_char ':' (String.trim body) with
  | [ name; value ] -> (
      match (String.trim name, String.trim value) with
      | (("stuck", ("top" | "right" | "bottom" | "left")) as pair)
      | (("snapped", ("block" | "inline" | "both")) as pair) ->
          let name, value = pair in
          Scroll_state { name; value; uppercase }
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

(* Lift a typed [Media.t] into a [Feature_query]. The container parser only
   produces single-feature media leaves at this point (compound forms are peeled
   off by [parse_unnamed] before [parse_atom]), so anything that's not a single
   feature is a parse error. *)
let single_feature_of_media (media : Media.t) =
  match media with
  | Media.And _ | Media.Or _ | Media.Negated _ | Media.List _
  | Media.Type_query _ ->
      None
  | _ -> Some media

let parse_container_specific raw =
  let raw = String.trim raw in
  if raw = "" then failwith "empty container query";
  match classify_query_surface raw with
  | Style_func { canonical_name; body } ->
      style_body ~uppercase:(not canonical_name) body
  | Scroll_state_func { canonical_name; body } ->
      scroll_state_body ~uppercase:(not canonical_name) body
  | Parenthesized_feature -> failwith "unrecognised container feature query"
  | Other_query -> failwith "not a container-specific query"

(* Find the position of [keyword] at parenthesis depth 0 inside [s]. The keyword
   is matched literally (caller passes [" and "], [" or "], etc.). *)
let find_top_level_keyword s keyword =
  let len = String.length s in
  let klen = String.length keyword in
  let depth = ref 0 in
  let result = ref None in
  let i = ref 0 in
  while !result = None && !i + klen <= len do
    (match s.[!i] with '(' -> incr depth | ')' -> decr depth | _ -> ());
    if !depth = 0 && String.sub s !i klen = keyword then result := Some !i;
    incr i
  done;
  !result

let outer_parens_wrap_all s =
  let len = String.length s in
  let rec loop depth i =
    if i >= len - 1 then true
    else
      match s.[i] with
      | '(' -> loop (depth + 1) (i + 1)
      | ')' when depth = 0 -> false
      | ')' -> loop (depth - 1) (i + 1)
      | _ -> loop depth (i + 1)
  in
  len >= 2 && s.[0] = '(' && s.[len - 1] = ')' && loop 0 1

let strip_outer_parens s =
  let s = String.trim s in
  if outer_parens_wrap_all s then
    String.sub s 1 (String.length s - 2) |> String.trim
  else s

let rec parse_unnamed s =
  let s = String.trim s in
  let stripped = strip_outer_parens s in
  match find_top_level_keyword stripped " or " with
  | Some i ->
      let lhs = String.sub stripped 0 i in
      let rhs = String.sub stripped (i + 4) (String.length stripped - i - 4) in
      Or (parse_unnamed lhs, parse_unnamed rhs)
  | None -> (
      match find_top_level_keyword stripped " and " with
      | Some i ->
          let lhs = String.sub stripped 0 i in
          let rhs =
            String.sub stripped (i + 5) (String.length stripped - i - 5)
          in
          And (parse_unnamed lhs, parse_unnamed rhs)
      | None ->
          if String.length stripped >= 4 && String.sub stripped 0 4 = "not "
          then
            (* CSS Containment 3 §4 and Conditional Rules: [not] takes exactly
               one [<query-in-parens>], so [not not (x)] is a parse error. The
               inner expression must be wrapped in parens (a feature query, a
               style()/scroll-state() function, or a parenthesised compound
               condition). *)
            let inner =
              String.trim (String.sub stripped 4 (String.length stripped - 4))
            in
            if String.length inner = 0 || inner.[0] <> '(' then
              failwith "container query: 'not' requires a parenthesised operand"
            else Not (parse_unnamed inner)
          else parse_atom s)

and parse_atom s =
  match Media.of_string s with
  | Media.Min_width_rem rem -> Min_width_rem rem
  | Media.Min_width px when Float.is_integer px ->
      Min_width_px (int_of_float px)
  | media -> (
      match single_feature_of_media media with
      | Some f -> Feature_query f
      | None -> failwith ("non-leaf media expression in container query: " ^ s))
  | exception Failure _ -> parse_container_specific s

let of_string s =
  match split_named s with
  | Some (name, raw) -> Named (name, parse_unnamed raw)
  | None -> parse_unnamed s

let feature name value = Feature_query (Media.feature name value)
let style ?value prop = Style { name = prop; value; uppercase = false }
let scroll_state name value = Scroll_state { name; value; uppercase = false }
