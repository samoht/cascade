(** CSS container query condition types *)

type t =
  | Min_width_rem of float
  | Min_width_px of int
  | Named of string * t
  | Style of { query : style_query; uppercase : bool }
  | Scroll_state of { query : scroll_state_query; uppercase : bool }
  | And of t * t
  | Or of t * t
  | Not of t
  | Feature_query of Media.t

and style_query =
  | Boolean of string
  | Declaration of { name : string; value : Component.t list }
  | Range of style_range

and style_range = {
  lower : Component.t list;
  lower_op : range_operator;
  name : string;
  upper_op : range_operator;
  upper : Component.t list;
}

and range_operator = Lt | Lte | Gt | Gte

and scroll_state_query =
  | State of { name : string; value : string }
  | Both of scroll_state_query * scroll_state_query
  | Either of scroll_state_query * scroll_state_query
  | Negated of scroll_state_query

(* Format float without trailing period (24. -> 24, 24.5 -> 24.5) *)
let format_rem f =
  let s = string_of_float f in
  if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
  else s

let components_to_string cvs = Cursor.components_to_string ~trim:true cvs

let range_operator_to_string = function
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="

let style_query_to_string = function
  | Boolean name -> name
  | Declaration { name; value } -> name ^ ": " ^ components_to_string value
  | Range { lower; lower_op; name; upper_op; upper } ->
      components_to_string lower ^ " "
      ^ range_operator_to_string lower_op
      ^ " " ^ name ^ " "
      ^ range_operator_to_string upper_op
      ^ " " ^ components_to_string upper

let rec scroll_state_query_to_string = function
  | State { name; value } -> name ^ ": " ^ value
  | Both (a, b) ->
      "("
      ^ scroll_state_query_to_string a
      ^ ") and ("
      ^ scroll_state_query_to_string b
      ^ ")"
  | Either (a, b) ->
      "("
      ^ scroll_state_query_to_string a
      ^ ") or ("
      ^ scroll_state_query_to_string b
      ^ ")"
  | Negated q -> "not (" ^ scroll_state_query_to_string q ^ ")"

let rec to_string_with ~minify t =
  match t with
  (* The typed [Min_width_*] shorthand always prints in its compact no-space
     form: it predates the [?minify] argument and existing direct callers expect
     that exact spelling. *)
  | Min_width_rem rem -> "(min-width:" ^ format_rem rem ^ "rem)"
  | Min_width_px px -> "(min-width:" ^ Int.to_string px ^ "px)"
  | Named (name, cond) -> name ^ " " ^ to_string_with ~minify cond
  | Style { query; uppercase } ->
      let head = if uppercase then "STYLE(" else "style(" in
      head ^ style_query_to_string query ^ ")"
  | Scroll_state { query; uppercase } ->
      let head = if uppercase then "SCROLL-STATE(" else "scroll-state(" in
      head ^ scroll_state_query_to_string query ^ ")"
  | And (a, b) ->
      "(" ^ to_string_with ~minify a ^ " and " ^ to_string_with ~minify b ^ ")"
  | Or (a, b) ->
      "(" ^ to_string_with ~minify a ^ " or " ^ to_string_with ~minify b ^ ")"
  | Not c -> "(not " ^ to_string_with ~minify c ^ ")"
  | Feature_query f -> Media.to_string ~minify f

let to_string ?(minify = false) t = to_string_with ~minify t
let pp t = to_string t

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Style { query = q1; _ }, Style { query = q2; _ } -> Stdlib.compare q1 q2
  | Scroll_state { query = q1; _ }, Scroll_state { query = q2; _ } ->
      Stdlib.compare q1 q2
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

let ident_component = function
  | Component.Preserved { kind = Token.Ident name; _ } -> Some name
  | _ -> None

let is_custom_property name =
  String.length name >= 2 && name.[0] = '-' && name.[1] = '-'

let split_declaration_components cvs =
  let rec loop before = function
    | [] -> None
    | Component.Preserved { kind = Token.Colon; _ } :: after ->
        Some (List.rev before, after)
    | cv :: rest -> loop (cv :: before) rest
  in
  loop [] cvs

let has_semicolon_component =
  List.exists (function
    | Component.Preserved { kind = Token.Semicolon; _ } -> true
    | _ -> false)

let style_strip_ws =
  List.filter (function
    | Component.Preserved { kind = Token.Whitespace; _ } -> false
    | _ -> true)

let take_range_operator = function
  | Component.Preserved { kind = Token.Delim "<"; _ }
    :: Component.Preserved { kind = Token.Delim "="; _ }
    :: rest ->
      Some (Lte, rest)
  | Component.Preserved { kind = Token.Delim ">"; _ }
    :: Component.Preserved { kind = Token.Delim "="; _ }
    :: rest ->
      Some (Gte, rest)
  | Component.Preserved { kind = Token.Delim "<"; _ } :: rest -> Some (Lt, rest)
  | Component.Preserved { kind = Token.Delim ">"; _ } :: rest -> Some (Gt, rest)
  | _ -> None

let split_before_range_operator cvs =
  let rec loop before rest =
    match take_range_operator rest with
    | Some (op, after) -> Some (List.rev before, op, after)
    | None -> (
        match rest with [] -> None | cv :: rest -> loop (cv :: before) rest)
  in
  loop [] cvs

let style_range_query cvs =
  match split_before_range_operator (style_strip_ws cvs) with
  | Some (lower, lower_op, prop :: rest) when lower <> [] -> (
      match (ident_component prop, split_before_range_operator rest) with
      | Some name, Some ([], upper_op, upper)
        when is_custom_property name && upper <> [] ->
          Some (Range { lower; lower_op; name; upper_op; upper })
      | _ -> None)
  | _ -> None

let style_body ~uppercase body =
  let body = String.trim body in
  if body = "" then failwith "empty style() container query";
  let components = Cursor.remaining (Cursor.of_string body) in
  match split_declaration_components components with
  | Some (name_components, value) -> (
      match (style_strip_ws name_components, style_strip_ws value) with
      | [ name_component ], _ :: _ when not (has_semicolon_component value) -> (
          match ident_component name_component with
          | Some name ->
              Style { query = Declaration { name; value }; uppercase }
          | None -> failwith "invalid style() container query")
      | _ -> failwith "invalid style() container query")
  | None -> (
      match style_range_query components with
      | Some query -> Style { query; uppercase }
      | None -> (
          match style_strip_ws components with
          | [ name_component ] -> (
              match ident_component name_component with
              | Some name when is_custom_property name ->
                  Style { query = Boolean name; uppercase }
              | _ -> failwith "invalid style() container query")
          | _ -> failwith "invalid style() container query"))

(* Find the position of [keyword] at parenthesis depth 0 inside [s]. The keyword
   is matched literally (caller passes [" and "], [" or "], etc.). *)
let top_level_keyword s keyword =
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

let top_level_word s word =
  let len = String.length s in
  let wlen = String.length word in
  let depth = ref 0 in
  let result = ref None in
  let i = ref 0 in
  let is_boundary i = i < 0 || i >= len || not (is_ident_cont s.[i]) in
  while !result = None && !i + wlen <= len do
    (match s.[!i] with '(' -> incr depth | ')' -> decr depth | _ -> ());
    if
      !depth = 0
      && String.sub s !i wlen = word
      && is_boundary (!i - 1)
      && is_boundary (!i + wlen)
    then result := Some !i;
    incr i
  done;
  !result

let has_top_level_word s word = Option.is_some (top_level_word s word)

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

let scroll_state_value_allowed name value =
  match name with
  | "stuck" -> (
      match value with
      | "top" | "right" | "bottom" | "left" | "block-start" | "block-end"
      | "inline-start" | "inline-end" | "none" ->
          true
      | _ -> false)
  | "snapped" -> (
      match value with
      | "block" | "inline" | "x" | "y" | "both" -> true
      | _ -> false)
  | "scrollable" -> (
      match value with
      | "top" | "right" | "bottom" | "left" | "block" | "inline" | "x" | "y" ->
          true
      | _ -> false)
  | "scrolled" -> (
      match value with
      | "top" | "right" | "bottom" | "left" | "block" | "inline" | "x" | "y"
      | "block-start" | "block-end" | "inline-start" | "inline-end" ->
          true
      | _ -> false)
  | _ -> false

let rec scroll_state_query_body body =
  let body = strip_outer_parens body in
  if String.length body >= 4 && String.sub body 0 4 = "not " then
    Negated
      (scroll_state_query_body
         (String.sub body 4 (String.length body - 4) |> String.trim))
  else if has_top_level_word body "and" && has_top_level_word body "or" then
    failwith "mixed scroll-state() operators require grouping"
  else
    match top_level_word body "or" with
    | Some i ->
        let lhs = String.sub body 0 i in
        let rhs = String.sub body (i + 2) (String.length body - i - 2) in
        Either (scroll_state_query_body lhs, scroll_state_query_body rhs)
    | None -> (
        match top_level_word body "and" with
        | Some i ->
            let lhs = String.sub body 0 i in
            let rhs = String.sub body (i + 3) (String.length body - i - 3) in
            Both (scroll_state_query_body lhs, scroll_state_query_body rhs)
        | None -> (
            match String.split_on_char ':' body with
            | [ name; value ] -> (
                match (String.trim name, String.trim value) with
                | name, value when scroll_state_value_allowed name value ->
                    State { name; value }
                | _ -> failwith "invalid scroll-state() container query")
            | _ -> failwith "invalid scroll-state() container query"))

let scroll_state_body ~uppercase body =
  let body = String.trim body in
  if body = "" then failwith "empty scroll-state() container query";
  try Scroll_state { query = scroll_state_query_body body; uppercase }
  with Failure msg -> failwith (msg ^ ": " ^ body)

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
         { node = { opening = Token.Paren; value = _ :: _ as value; _ }; _ };
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

let rec parse_unnamed s =
  let s = String.trim s in
  let stripped = strip_outer_parens s in
  if has_top_level_word stripped "and" && has_top_level_word stripped "or" then
    failwith "mixed container query operators require grouping"
  else
    match top_level_keyword stripped " or " with
    | Some i ->
        let lhs = String.sub stripped 0 i in
        let rhs =
          String.sub stripped (i + 4) (String.length stripped - i - 4)
        in
        Or (parse_unnamed lhs, parse_unnamed rhs)
    | None -> (
        match top_level_keyword stripped " and " with
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
                failwith
                  "container query: 'not' requires a parenthesised operand"
              else Not (parse_unnamed inner)
            else parse_atom s)

and parse_atom s =
  match classify_query_surface (String.trim s) with
  | Style_func _ | Scroll_state_func _ -> parse_container_specific s
  | Parenthesized_feature | Other_query -> (
      match Media.of_string s with
      | Media.Min_width_rem rem -> Min_width_rem rem
      | Media.Min_width px when Float.is_integer px ->
          Min_width_px (int_of_float px)
      | media -> (
          match single_feature_of_media media with
          | Some f -> Feature_query f
          | None ->
              failwith ("non-leaf media expression in container query: " ^ s))
      | exception Failure _ -> parse_container_specific s)

let of_string s =
  match split_named s with
  | Some (name, raw) -> Named (name, parse_unnamed raw)
  | None -> parse_unnamed s

let feature name value = Feature_query (Media.feature name value)

let style ?value prop =
  let query =
    match value with
    | None -> Boolean prop
    | Some value ->
        Declaration
          { name = prop; value = Cursor.remaining (Cursor.of_string value) }
  in
  Style { query; uppercase = false }

let scroll_state name value =
  Scroll_state { query = State { name; value }; uppercase = false }
