(** CSS container query condition types *)

open Syntax

type component_values = Values.component_values

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
  | Declaration of { name : string; value : component_values }
  | Range of style_range
  | All of style_query * style_query
  | Any of style_query * style_query
  | Neg of style_query

and style_range = {
  lower : component_values;
  lower_op : range_operator;
  name : string;
  upper_op : range_operator;
  upper : component_values;
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

let string_of_components cvs = Cursor.string_of_components ~trim:true cvs

let string_of_range_operator = function
  | Lt -> "<"
  | Lte -> "<="
  | Gt -> ">"
  | Gte -> ">="

let rec string_of_style_query ~minify = function
  | Boolean name -> name
  | Declaration { name; value } ->
      let sep = if minify then ":" else ": " in
      String.concat "" [ name; sep; string_of_components value ]
  | Range { lower; lower_op; name; upper_op; upper } ->
      let sep = if minify then "" else " " in
      String.concat ""
        [
          string_of_components lower;
          sep;
          string_of_range_operator lower_op;
          sep;
          name;
          sep;
          string_of_range_operator upper_op;
          sep;
          string_of_components upper;
        ]
  | All (a, b) ->
      String.concat ""
        [
          style_query_operand ~minify a; " and "; style_query_operand ~minify b;
        ]
  | Any (a, b) ->
      String.concat ""
        [ style_query_operand ~minify a; " or "; style_query_operand ~minify b ]
  | Neg q -> String.concat "" [ "not "; style_query_operand ~minify q ]

and style_query_operand ~minify q =
  String.concat "" [ "("; string_of_style_query ~minify q; ")" ]

let rec string_of_scroll_state_query ~minify = function
  | State { name; value } ->
      let sep = if minify then ":" else ": " in
      String.concat "" [ name; sep; value ]
  | Both (a, b) ->
      String.concat ""
        [
          "(";
          string_of_scroll_state_query ~minify a;
          ") and (";
          string_of_scroll_state_query ~minify b;
          ")";
        ]
  | Either (a, b) ->
      String.concat ""
        [
          "(";
          string_of_scroll_state_query ~minify a;
          ") or (";
          string_of_scroll_state_query ~minify b;
          ")";
        ]
  | Negated q ->
      String.concat "" [ "not ("; string_of_scroll_state_query ~minify q; ")" ]

let rec minified_condition = function
  | And (a, b) -> minified_operand a ^ " and " ^ minified_operand b
  | Or (a, b) -> minified_operand a ^ " or " ^ minified_operand b
  | Not c -> "not " ^ minified_operand c
  | t -> to_string_with ~pretty:false ~minify:true t

and minified_operand = function
  | ( Min_width_rem _ | Min_width_px _ | Style _ | Scroll_state _
    | Feature_query _ ) as t ->
      to_string_with ~pretty:false ~minify:true t
  | t -> "(" ^ minified_condition t ^ ")"

and to_string_with ~pretty ~minify t =
  match t with
  | Min_width_rem rem ->
      (* The [width>=] range upgrade is a target-fact rewrite applied by
         [lower_for_minify] in the optimize phase, not here. *)
      let sep = if pretty && not minify then ": " else ":" in
      "(min-width" ^ sep ^ format_rem rem ^ "rem)"
  | Min_width_px px ->
      let sep = if pretty && not minify then ": " else ":" in
      "(min-width" ^ sep ^ Int.to_string px ^ "px)"
  | Named (name, cond) -> name ^ " " ^ to_string_with ~pretty ~minify cond
  | Style { query; uppercase } ->
      let head = if uppercase then "STYLE(" else "style(" in
      head ^ string_of_style_query ~minify query ^ ")"
  | Scroll_state { query; uppercase } ->
      let head = if uppercase then "SCROLL-STATE(" else "scroll-state(" in
      String.concat "" [ head; string_of_scroll_state_query ~minify query; ")" ]
  | And (a, b) ->
      if minify then minified_condition t
      else
        "("
        ^ to_string_with ~pretty ~minify a
        ^ " and "
        ^ to_string_with ~pretty ~minify b
        ^ ")"
  | Or (a, b) ->
      if minify then minified_condition t
      else
        "("
        ^ to_string_with ~pretty ~minify a
        ^ " or "
        ^ to_string_with ~pretty ~minify b
        ^ ")"
  | Not c ->
      if minify then minified_condition t
      else "(not " ^ to_string_with ~pretty ~minify c ^ ")"
  | Feature_query f -> Media.to_string ~minify f

let to_string ?(minify = false) t = to_string_with ~pretty:false ~minify t

let to_stylesheet_string ?(minify = false) t =
  to_string_with ~pretty:true ~minify t

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

(* Container feature queries reuse [Media] features, so they share the same
   minify-time grammar upgrades. The dedicated [Min_width_*] shorthands carry an
   implicit lower [width] bound, so they lower to the same [width>=V] range. *)
let width_ge l : t =
  Feature_query
    (Media.Cond
       (Media.Feature (Media.Range (Media.Width, Media.Ge, Media.Length l))))

let rec lower_for_minify c =
  match c with
  | Feature_query q ->
      let q' = Media.lower_for_minify q in
      if q' == q then c else Feature_query q'
  | Min_width_rem rem -> width_ge (Values.Rem rem)
  | Min_width_px px -> width_ge (Values.Px (float_of_int px))
  | Named (name, cond) ->
      let cond' = lower_for_minify cond in
      if cond' == cond then c else Named (name, cond')
  | And (a, b) ->
      let a' = lower_for_minify a and b' = lower_for_minify b in
      if a' == a && b' == b then c else And (a', b')
  | Or (a, b) ->
      let a' = lower_for_minify a and b' = lower_for_minify b in
      if a' == a && b' == b then c else Or (a', b')
  | Not c' ->
      let c'' = lower_for_minify c' in
      if c'' == c' then c else Not c''
  | Style _ | Scroll_state _ -> c

(* A real [var()] function anywhere in the components, recursing into function
   arguments and bracketed blocks. A [var(] inside a string or url() is an
   atomic [Preserved] token, never a [Func], so it is data, not a reference. *)
let rec components_have_var (components : Component.t list) =
  List.exists
    (fun (c : Component.t) ->
      match c with
      | Component.Func { node = { name; arguments; _ }; _ } ->
          String.lowercase_ascii name = "var" || components_have_var arguments
      | Component.Block { node = { value; _ }; _ } -> components_have_var value
      | Component.Preserved _ -> false)
    components

let contains_var_function s =
  Cursor.of_string s |> Cursor.remaining |> components_have_var

let unresolved_media_feature s =
  let s = String.trim s in
  let len = String.length s in
  if len >= 2 && s.[0] = '(' && s.[len - 1] = ')' then
    let body = String.sub s 1 (len - 2) in
    match String.index_opt body ':' with
    | Some colon ->
        let name = String.sub body 0 colon |> String.trim in
        let value =
          String.sub body (colon + 1) (String.length body - colon - 1)
          |> String.trim
        in
        if name <> "" && value <> "" && contains_var_function value then
          Some
            (Feature_query
               (Media.Cond
                  (Media.Feature
                     (Media.Plain
                        ( Media.name_of_string name,
                          Media.Ident (Media.ident_of_string value) )))))
        else None
    | None -> None
  else None

let first_non_ws s =
  let rec loop i =
    if i >= String.length s then None
    else
      match s.[i] with
      | ' ' | '\t' | '\n' | '\r' | '\012' -> loop (i + 1)
      | c -> Some (i, c)
  in
  loop

(* CSS Containment 3 section 4: [<container-name>] excludes the keywords [none],
   [and], [not], [or]; without this guard [Container.of_string "not (width)"]
   would split as [Named ("not", "(width)")] instead of [Not (width)]. *)
let is_reserved_container_name name =
  match String.lowercase_ascii name with
  | "none" | "and" | "not" | "or" -> true
  | _ -> false

let split_named s =
  let s = String.trim s in
  let len = String.length s in
  let rec ident_end i =
    if i < len && is_ascii_ident_continue s.[i] then ident_end (i + 1) else i
  in
  if len = 0 || not (is_ascii_ident_start s.[0]) then None
  else
    let stop = ident_end 0 in
    let name = String.sub s 0 stop in
    if is_reserved_container_name name then None
    else
      match first_non_ws s stop with
      | Some (i, ('(' | 's')) when stop > 0 && i > stop ->
          Some (name, String.sub s i (len - i))
      | _ -> None

let ident_component = function
  | Component.Preserved { kind = Token.Ident name; _ } -> Some name
  | _ -> None

let is_custom_property name =
  String.length name >= 2 && name.[0] = '-' && name.[1] = '-'

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

let style_leaf_declaration name_components value =
  match (style_strip_ws name_components, style_strip_ws value) with
  | [ name_component ], stripped_value when not (has_semicolon_component value)
    -> (
      match ident_component name_component with
      | Some name when stripped_value <> [] || is_custom_property name ->
          Declaration { name; value }
      | Some _ | None -> failwith "invalid style() container query")
  | _ -> failwith "invalid style() container query"

let style_leaf_boolean components =
  match style_strip_ws components with
  | [ name_component ] -> (
      match ident_component name_component with
      (* CSS Conditional Rules 5 section 4.4: a boolean [style()] query tests
         whether a custom property has any value, so the ident must start with
         [--]. A bare property name like [style(color)] is not a valid boolean
         form. *)
      | Some name when is_custom_property name -> Boolean name
      | _ -> failwith "invalid style() container query")
  | _ -> failwith "invalid style() container query"

let style_leaf_body body =
  let body = String.trim body in
  if body = "" then failwith "empty style() container query";
  let components = Cursor.remaining (Cursor.of_string body) in
  match split_top_level_colon components with
  | Some (name_components, value) ->
      style_leaf_declaration name_components value
  | None -> (
      match style_range_query components with
      | Some query -> query
      | None -> style_leaf_boolean components)

let top_level_word s word =
  let len = String.length s in
  let wlen = String.length word in
  let depth = ref 0 in
  let result = ref None in
  let i = ref 0 in
  let is_boundary i =
    i < 0 || i >= len || not (is_ascii_ident_continue s.[i])
  in
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

let rec strip_outer_parens s =
  let s = String.trim s in
  if outer_parens_wrap_all s then
    String.sub s 1 (String.length s - 2) |> strip_outer_parens
  else s

let rec style_query_body body =
  let body = strip_outer_parens body in
  if String.length body >= 4 && String.sub body 0 4 = "not " then
    Neg
      (style_query_body
         (String.sub body 4 (String.length body - 4) |> String.trim))
  else if has_top_level_word body "and" && has_top_level_word body "or" then
    failwith "mixed style() operators require grouping"
  else
    match top_level_word body "or" with
    | Some i ->
        let lhs = String.sub body 0 i in
        let rhs = String.sub body (i + 2) (String.length body - i - 2) in
        Any (style_query_body lhs, style_query_body rhs)
    | None -> (
        match top_level_word body "and" with
        | Some i ->
            let lhs = String.sub body 0 i in
            let rhs = String.sub body (i + 3) (String.length body - i - 3) in
            All (style_query_body lhs, style_query_body rhs)
        | None -> style_leaf_body body)

let style_body ~uppercase body =
  let body = String.trim body in
  if body = "" then failwith "empty style() container query";
  try Style { query = style_query_body body; uppercase }
  with Failure msg -> failwith (msg ^ ": " ^ body)

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
      | "top" | "right" | "bottom" | "left" | "block" | "inline" | "x" | "y"
      | "block-start" | "block-end" | "inline-start" | "inline-end" ->
          true
      | _ -> false)
  | "scrolled" -> (
      match value with
      | "top" | "right" | "bottom" | "left" | "block" | "inline" | "x" | "y"
      | "block-start" | "block-end" | "inline-start" | "inline-end" ->
          true
      | _ -> false)
  | _ -> false

let scroll_state_query_leaf body =
  match String.split_on_char ':' body with
  | [ name; value ] ->
      let name = String.trim name in
      let value = String.trim value in
      if scroll_state_value_allowed name value then State { name; value }
      else failwith "invalid scroll-state() container query"
  | _ -> failwith "invalid scroll-state() container query"

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
        | None -> scroll_state_query_leaf body)

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
          let body = Cursor.string_of_components ~trim:true arguments in
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
   accepts single-feature media leaves at this point (compound forms are peeled
   off by [unnamed_of_string] before [atom_of_string]), so anything that's not a
   single feature is a parse error. *)
let single_feature_of_media (media : Media.t) =
  match media with
  | Media.Cond (Media.Feature _) -> Some media
  | Media.Cond _ | Media.List _ | Media.Type _ -> None

let specific_of_string raw =
  let raw = String.trim raw in
  if raw = "" then failwith "empty container query";
  match classify_query_surface raw with
  | Style_func { canonical_name; body } ->
      style_body ~uppercase:(not canonical_name) body
  | Scroll_state_func { canonical_name; body } ->
      scroll_state_body ~uppercase:(not canonical_name) body
  | Parenthesized_feature -> failwith "unrecognised container feature query"
  | Other_query -> failwith "not a container-specific query"

let atom_of_string s =
  let s = String.trim s in
  let stripped = strip_outer_parens s in
  match classify_query_surface stripped with
  | _ when String.contains s 'v' && contains_var_function s -> (
      match unresolved_media_feature s with
      | Some query -> query
      | None -> specific_of_string s)
  | Style_func _ | Scroll_state_func _ -> specific_of_string stripped
  | Parenthesized_feature | Other_query -> (
      match Media.of_string_strict s with
      | Media.Cond (Media.Feature (Media.Plain (Media.Min Media.Width, value)))
        as media -> (
          match value with
          | Media.Length (Values.Rem rem) -> Min_width_rem rem
          | Media.Length (Values.Px px) when Float.is_integer px ->
              Min_width_px (int_of_float px)
          | _ -> Feature_query media)
      | media -> (
          match single_feature_of_media media with
          | Some f -> Feature_query f
          | None -> failwith "not a container feature query")
      | exception Failure _ -> specific_of_string s)

let rec unnamed_query_not s stripped =
  if String.length stripped >= 4 && String.sub stripped 0 4 = "not " then
    (* CSS Containment 3 §4 and Conditional Rules: [not] takes exactly one
       [<query-in-parens>], so [not not (x)] is a parse error. The operand is a
       parenthesised compound condition or a [style()] / [scroll-state()]
       function; the latter carry their own parentheses, so they need no extra
       wrapping ([not style(--a)] is valid, not just [not (style(--a))]). *)
    let inner =
      String.trim (String.sub stripped 4 (String.length stripped - 4))
    in
    let is_function_operand =
      match classify_query_surface inner with
      | Style_func _ | Scroll_state_func _ -> true
      | Parenthesized_feature | Other_query -> false
      | exception Failure _ -> false
    in
    if String.length inner = 0 || (inner.[0] <> '(' && not is_function_operand)
    then failwith "container query: 'not' requires a query-in-parens operand"
    else Not (unnamed_of_string inner)
  else atom_of_string s

and unnamed_of_string s =
  let s = String.trim s in
  let stripped = strip_outer_parens s in
  if has_top_level_word stripped "and" && has_top_level_word stripped "or" then
    failwith "mixed container query operators require grouping"
  else
    match top_level_word stripped "or" with
    | Some i ->
        let lhs = String.sub stripped 0 i in
        let rhs =
          String.sub stripped (i + 2) (String.length stripped - i - 2)
        in
        Or (unnamed_of_string lhs, unnamed_of_string rhs)
    | None -> (
        match top_level_word stripped "and" with
        | Some i ->
            let lhs = String.sub stripped 0 i in
            let rhs =
              String.sub stripped (i + 3) (String.length stripped - i - 3)
            in
            And (unnamed_of_string lhs, unnamed_of_string rhs)
        | None -> unnamed_query_not s stripped)

let of_string s =
  match split_named s with
  | Some (name, raw) -> Named (name, unnamed_of_string raw)
  | None -> unnamed_of_string s

let feature name value = Feature_query (Media.feature name value)

let style ?value prop =
  let query =
    match value with
    | None ->
        if is_custom_property prop then Boolean prop
        else
          invalid_arg
            "Container.style: boolean style queries require a custom property"
    | Some value ->
        Declaration
          { name = prop; value = Cursor.remaining (Cursor.of_string value) }
  in
  Style { query; uppercase = false }

let scroll_state name value =
  Scroll_state { query = State { name; value }; uppercase = false }
