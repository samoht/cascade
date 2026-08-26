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

(* CSS Syntax 3 sec. 4.3.7: a [style()] query names a custom property, and an
   escape can carry a [;] or a [}] into that name, so it is written back with
   the escapes that read the same name (see [Properties.pp_property]). *)
let rec string_of_style_query ~minify = function
  | Boolean name -> Parser.escape_ident name
  | Declaration { name; value } ->
      let sep = if minify then ":" else ": " in
      String.concat ""
        [ Parser.escape_ident name; sep; string_of_components value ]
  | Range { lower; lower_op; name; upper_op; upper } ->
      let sep = if minify then "" else " " in
      String.concat ""
        [
          string_of_components lower;
          sep;
          string_of_range_operator lower_op;
          sep;
          Parser.escape_ident name;
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
  | Named (name, cond) ->
      Parser.escape_ident name ^ " " ^ to_string_with ~pretty ~minify cond
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

let pp ctx t = Pp.string ctx (to_string ~minify:(Pp.minified ctx) t)

(* A style query holds component values, which carry the source positions they
   were read from, so a structural walk would call two spellings of one query
   different. Compare what the query asks, not where it was written. *)
let style_query_rank = function
  | Boolean _ -> 0
  | Declaration _ -> 1
  | Range _ -> 2
  | All _ -> 3
  | Any _ -> 4
  | Neg _ -> 5

let compare_values (a : component_values) b = List.compare Component.compare a b
let compare_range_operator (a : range_operator) b = Stdlib.compare a b

let compare_style_range (r1 : style_range) (r2 : style_range) =
  let c = String.compare r1.name r2.name in
  if c <> 0 then c
  else
    let c = compare_range_operator r1.lower_op r2.lower_op in
    if c <> 0 then c
    else
      let c = compare_range_operator r1.upper_op r2.upper_op in
      if c <> 0 then c
      else
        let c = compare_values r1.lower r2.lower in
        if c <> 0 then c else compare_values r1.upper r2.upper

let rec compare_style_query q1 q2 =
  match (q1, q2) with
  | Boolean n1, Boolean n2 -> String.compare n1 n2
  | Declaration d1, Declaration d2 ->
      let c = String.compare d1.name d2.name in
      if c <> 0 then c else compare_values d1.value d2.value
  | Range r1, Range r2 -> compare_style_range r1 r2
  | All (a1, b1), All (a2, b2) | Any (a1, b1), Any (a2, b2) ->
      let c = compare_style_query a1 a2 in
      if c <> 0 then c else compare_style_query b1 b2
  | Neg a, Neg b -> compare_style_query a b
  | _ -> Int.compare (style_query_rank q1) (style_query_rank q2)

let rec compare t1 t2 =
  match (t1, t2) with
  | Min_width_rem r1, Min_width_rem r2 -> Float.compare r1 r2
  | Min_width_px p1, Min_width_px p2 -> Int.compare p1 p2
  | Named (n1, c1), Named (n2, c2) ->
      let name_cmp = String.compare n1 n2 in
      if name_cmp <> 0 then name_cmp else compare c1 c2
  | Style { query = q1; _ }, Style { query = q2; _ } ->
      compare_style_query q1 q2
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

let compare_scroll_state_query (a : scroll_state_query) b = Stdlib.compare a b

type kind = Min_width | Other

let equal_kind (a : kind) b = a = b

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

let is_whitespace_component = function
  | Component.Preserved { kind = Token.Whitespace; _ } -> true
  | _ -> false

let rec drop_leading_whitespace = function
  | component :: rest when is_whitespace_component component ->
      drop_leading_whitespace rest
  | components -> components

let trim_components components =
  components |> drop_leading_whitespace |> List.rev |> drop_leading_whitespace
  |> List.rev

let components_empty = function [] -> true | _ :: _ -> false

let ident_component = function
  | Component.Preserved { kind = Token.Ident name; _ } -> Some name
  | _ -> None

let ident_is word component =
  match ident_component component with
  | Some name -> String.equal (String.lowercase_ascii name) word
  | None -> false

let split_keyword word components =
  let rec loop before = function
    | [] -> None
    | component :: after when ident_is word component ->
        Some (trim_components (List.rev before), trim_components after)
    | component :: after -> loop (component :: before) after
  in
  loop [] components

let has_keyword word components = Option.is_some (split_keyword word components)

let single_paren_body components =
  match trim_components components with
  | [
   Component.Block { node = { opening = Token.Paren; value; closed = true }; _ };
  ] ->
      Some value
  | _ -> None

let is_condition_function = function
  | Component.Func { node = { name; terminated = true; _ }; _ } ->
      List.mem (String.lowercase_ascii name) [ "style"; "scroll-state" ]
  | _ -> false

let is_query_operand components =
  match trim_components components with
  | [
   Component.Block { node = { opening = Token.Paren; closed = true; _ }; _ };
  ] ->
      true
  | [ component ] -> is_condition_function component
  | _ -> false

let starts_query = function
  | Component.Block { node = { opening = Token.Paren; closed = true; _ }; _ }
    :: _ ->
      true
  | component :: _ when is_condition_function component -> true
  | first :: _ when ident_is "not" first -> true
  | _ -> false

(* CSS Containment 3 section 4: [<container-name>] excludes the keywords [none],
   [and], [not], [or]; without this guard [Container.of_string "not (width)"]
   would split as [Named ("not", "(width)")] instead of [Not (width)]. *)
let is_reserved_container_name name =
  match String.lowercase_ascii name with
  | "none" | "and" | "not" | "or" -> true
  | _ -> false

let split_named_components components =
  match drop_leading_whitespace components with
  | Component.Preserved { kind = Token.Ident name; _ }
    :: (Component.Preserved { kind = Token.Whitespace; _ } :: _ as after)
    when not (is_reserved_container_name name) ->
      let query = trim_components after in
      if starts_query query then Some (name, query) else None
  | _ -> None

let is_custom_property name = Custom_property_name.is_valid name

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

(* Every failure below is an [@container] prelude failure, and the slice of the
   query that failed carries the span the caret must point at. [t] anchors the
   smallest enclosing construct, for a failure that has no components of its
   own. *)
let err t cvs reason =
  let at = match cvs with [] -> t | _ :: _ -> Cursor.sub t cvs in
  Cursor.err_condition at ~at_rule:"@container" reason

let style_leaf_declaration t name_components value =
  match (style_strip_ws name_components, style_strip_ws value) with
  | [ name_component ], stripped_value when not (has_semicolon_component value)
    -> (
      match ident_component name_component with
      | Some name when stripped_value <> [] || is_custom_property name ->
          Declaration { name; value }
      | Some _ | None -> err t name_components "invalid style() container query"
      )
  | _ -> err t name_components "invalid style() container query"

let style_leaf_boolean t components =
  match style_strip_ws components with
  | [ name_component ] -> (
      match ident_component name_component with
      (* CSS Conditional Rules 5 section 6.2: a boolean [style()] query tests
         whether a custom property has any value, so the ident must start with
         [--]. A bare property name like [style(color)] is not a valid boolean
         form. *)
      | Some name when is_custom_property name -> Boolean name
      | _ -> err t components "invalid style() container query")
  | _ -> err t components "invalid style() container query"

let style_leaf_components t components =
  match split_top_level_colon components with
  | Some (name_components, value) ->
      style_leaf_declaration t name_components value
  | None -> (
      match style_range_query components with
      | Some query -> query
      | None -> style_leaf_boolean t components)

let rec style_query_components t components =
  let components = trim_components components in
  if components_empty components then
    err t components "empty style() container query";
  match split_top_level_colon components with
  | Some _ -> style_leaf_components t components
  | None -> style_query_operator t components

and style_query_operator t components =
  let level, unwrapped =
    match single_paren_body components with
    | Some body -> (trim_components body, true)
    | None -> (components, false)
  in
  if has_keyword "and" level && has_keyword "or" level then
    err t level "mixed style() operators require grouping"
  else
    match split_keyword "or" level with
    | Some (lhs, rhs) ->
        Any (style_query_components t lhs, style_query_components t rhs)
    | None -> style_query_conjunction t ~components ~level ~unwrapped

and style_query_conjunction t ~components ~level ~unwrapped =
  match split_keyword "and" level with
  | Some (lhs, rhs) ->
      All (style_query_components t lhs, style_query_components t rhs)
  | None -> style_query_unary t ~components ~level ~unwrapped

and style_query_unary t ~components ~level ~unwrapped =
  match level with
  | first :: rest when ident_is "not" first ->
      Neg (style_query_components t rest)
  | _ when unwrapped -> style_query_components t level
  | _ -> style_leaf_components t components

let style_body t ~uppercase (fn : Component.func Component.node) =
  let t = Cursor.sub t [ Component.Func fn ] in
  Style { query = style_query_components t fn.node.arguments; uppercase }

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

let scroll_state_query_leaf t components =
  match split_top_level_colon components with
  | Some (name_components, value_components) -> (
      match
        (style_strip_ws name_components, style_strip_ws value_components)
      with
      | [ name_component ], [ value_component ] -> (
          match
            (ident_component name_component, ident_component value_component)
          with
          | Some name, Some value ->
              let name = String.lowercase_ascii name in
              let value = String.lowercase_ascii value in
              if scroll_state_value_allowed name value then
                State { name; value }
              else
                err t value_components "invalid scroll-state() container query"
          | Some _, None | None, Some _ | None, None ->
              err t components "invalid scroll-state() container query")
      | _ -> err t components "invalid scroll-state() container query")
  | None -> err t components "invalid scroll-state() container query"

let rec scroll_state_query_components t components =
  let components = trim_components components in
  if components_empty components then
    err t components "empty scroll-state() container query";
  match split_top_level_colon components with
  | Some _ -> scroll_state_query_leaf t components
  | None -> scroll_state_query_operator t components

and scroll_state_query_operator t components =
  let level, unwrapped =
    match single_paren_body components with
    | Some body -> (trim_components body, true)
    | None -> (components, false)
  in
  if has_keyword "and" level && has_keyword "or" level then
    err t level "mixed scroll-state() operators require grouping"
  else
    match split_keyword "or" level with
    | Some (lhs, rhs) ->
        Either
          ( scroll_state_query_components t lhs,
            scroll_state_query_components t rhs )
    | None -> scroll_state_query_conjunction t ~components ~level ~unwrapped

and scroll_state_query_conjunction t ~components ~level ~unwrapped =
  match split_keyword "and" level with
  | Some (lhs, rhs) ->
      Both
        ( scroll_state_query_components t lhs,
          scroll_state_query_components t rhs )
  | None -> scroll_state_query_unary t ~components ~level ~unwrapped

and scroll_state_query_unary t ~components ~level ~unwrapped =
  match level with
  | first :: rest when ident_is "not" first ->
      Negated (scroll_state_query_components t rest)
  | _ when unwrapped -> scroll_state_query_components t level
  | _ -> scroll_state_query_leaf t components

let scroll_state_body t ~uppercase (fn : Component.func Component.node) =
  let t = Cursor.sub t [ Component.Func fn ] in
  Scroll_state
    { query = scroll_state_query_components t fn.node.arguments; uppercase }

type query_surface =
  | Style_func of { canonical_name : bool; fn : Component.func Component.node }
  | Scroll_state_func of {
      canonical_name : bool;
      fn : Component.func Component.node;
    }
  | Parenthesized_feature
  | Other_query

type range_direction = Lt_range | Gt_range

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

let classify_query_surface t components =
  match trim_components components with
  | [ (Component.Func ({ node = { name; terminated; _ }; _ } as fn) as cv) ]
    -> (
      if not terminated then err t [ cv ] "unmatched container query function";
      let lower = String.lowercase_ascii name in
      let canonical_name = name = lower in
      match lower with
      | "style" -> Style_func { canonical_name; fn }
      | "scroll-state" -> Scroll_state_func { canonical_name; fn }
      | _ -> Other_query)
  | [
   (Component.Block { node = { opening = Token.Paren; value; closed }; _ } as cv);
  ] ->
      if not closed then err t [ cv ] "unmatched container query parentheses";
      let value = strip_ws value in
      if components_empty value then Other_query
      else if has_dangling_range_operator value then
        err t value "dangling range operator in container query"
      else if has_opposing_interval_components value then
        err t value "opposing interval operators in container query"
      else Parenthesized_feature
  | _ -> Other_query

(* Lift a typed [Media.t] into a [Feature_query]. The container parser only
   accepts single-feature media leaves at this point (compound forms are peeled
   off by [unnamed_of_string] before [atom_of_string]), so anything that's not a
   single feature is a parse error. *)
let single_feature_of_media (media : Media.t) =
  match media with
  | Media.Cond (Media.Feature _) -> Some media
  | Media.Cond _ | Media.List _ | Media.Type _ -> None

let specific_of_components t components =
  if trim_components components |> components_empty then
    err t components "empty container query";
  match classify_query_surface t components with
  | Style_func { canonical_name; fn } ->
      style_body t ~uppercase:(not canonical_name) fn
  | Scroll_state_func { canonical_name; fn } ->
      scroll_state_body t ~uppercase:(not canonical_name) fn
  | Parenthesized_feature ->
      err t components "unrecognised container feature query"
  | Other_query -> err t components "not a container-specific query"

let unresolved_media_feature components =
  match trim_components components with
  | [
   Component.Block { node = { opening = Token.Paren; value; closed = true }; _ };
  ] -> (
      match split_top_level_colon value with
      | Some (name_components, value_components) -> (
          match style_strip_ws name_components with
          | [ name_component ] -> (
              match ident_component name_component with
              | Some name
                when (not
                        (trim_components value_components |> components_empty))
                     && components_have_var value_components ->
                  let value = string_of_components value_components in
                  Some
                    (Feature_query
                       (Media.Cond
                          (Media.Feature
                             (Media.Plain
                                ( Media.name_of_string name,
                                  Media.value_of_string value )))))
              | Some _ | None -> None)
          | _ -> None)
      | None -> None)
  | _ -> None

let rec strip_outer_components components =
  match single_paren_body components with
  | Some body -> strip_outer_components body
  | None -> trim_components components

let atom_of_components t components =
  let components = trim_components components in
  let stripped = strip_outer_components components in
  match classify_query_surface t stripped with
  | _ when components_have_var components -> (
      match unresolved_media_feature components with
      | Some query -> query
      | None -> specific_of_components t stripped)
  | Style_func _ | Scroll_state_func _ -> specific_of_components t stripped
  | Parenthesized_feature | Other_query -> (
      let source = string_of_components components in
      match Media.of_string_strict source with
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
          | None -> err t components "not a container feature query")
      | exception Error.Parse_error _ -> specific_of_components t stripped)

let rec unnamed_of_components t components =
  let components = trim_components components in
  if components_empty components then err t components "empty container query";
  let level =
    match single_paren_body components with
    | Some body when Option.is_none (split_top_level_colon body) ->
        trim_components body
    | Some _ | None -> components
  in
  if has_keyword "and" level && has_keyword "or" level then
    err t level "mixed container query operators require grouping"
  else unnamed_or_components t ~components level

and unnamed_or_components t ~components level =
  match split_keyword "or" level with
  | Some (lhs, rhs) ->
      Or (unnamed_of_components t lhs, unnamed_of_components t rhs)
  | None -> unnamed_and_components t ~components level

and unnamed_and_components t ~components level =
  match split_keyword "and" level with
  | Some (lhs, rhs) ->
      And (unnamed_of_components t lhs, unnamed_of_components t rhs)
  | None -> unnamed_unary_components t ~components level

and unnamed_unary_components t ~components = function
  | first :: rest when ident_is "not" first ->
      if not (is_query_operand rest) then
        err t rest "container query: 'not' requires a query-in-parens operand";
      Not (unnamed_of_components t rest)
  | _ -> atom_of_components t components

let read t =
  let components = Cursor.remaining t in
  match split_named_components components with
  | Some (name, query) -> Named (name, unnamed_of_components t query)
  | None -> unnamed_of_components t components

let of_string s = read (Cursor.of_string s)
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

(* ===== Canonical form ===== *)

(* [normalize] is the one place that decides which spellings name one container
   query, and its invariant runs one way only:

   normalize a = normalize b => a and b select the same containers

   Nothing promises the converse, and nothing should: two equivalent queries may
   normalise apart, which costs a merge and never correctness. So every rewrite
   below is one of the spec's own equivalences, never a heuristic. *)

(* CSS Conditional Rules 5 sec. 6.1: a [<size-feature>] is spelled like a media
   feature, so the [min-] prefix, the value-first bound and the two interval
   directions are the Media Queries 4 sec. 2.4.3 and sec. 2.4.4 equivalences
   here too. [Media.normalize] owns that fold; the [Min_width_*] shorthands are
   cascade's compact spelling of [(min-width: V)], so they enter it as that
   feature rather than being folded a second way. *)
let min_width l : Media.t =
  Media.Cond
    (Media.Feature (Media.Plain (Media.Min Media.Width, Media.Length l)))

(* CSS Syntax 3 sec. 5.5.6 consumes a declaration by discarding the whitespace
   after the colon and removing the trailing whitespace tokens, so a
   [<style-feature-plain>] means the same however much space the source left
   around its value. A [<style-range>] carries no whitespace at all: the parser
   strips it before splitting on the comparison. *)
let rec normalize_style_query (q : style_query) : style_query =
  match q with
  | Declaration { name; value } ->
      let value' = trim_components value in
      if value' == value then q else Declaration { name; value = value' }
  | All (a, b) ->
      let a' = normalize_style_query a and b' = normalize_style_query b in
      if a' == a && b' == b then q else All (a', b')
  | Any (a, b) ->
      let a' = normalize_style_query a and b' = normalize_style_query b in
      if a' == a && b' == b then q else Any (a', b')
  | Neg a ->
      let a' = normalize_style_query a in
      if a' == a then q else Neg a'
  | Boolean _ | Range _ -> q

(* Rebuild only what changed, as [lower_for_minify] does: [equal] runs this on
   both sides of every merge test, and the queries it is asked about are usually
   already normal. *)
let rec normalize (c : t) : t =
  match c with
  | Min_width_rem rem -> Feature_query (Media.normalize (min_width (Rem rem)))
  | Min_width_px px ->
      Feature_query (Media.normalize (min_width (Px (float_of_int px))))
  | Feature_query q ->
      let q' = Media.normalize q in
      if q' == q then c else Feature_query q'
  | Named (name, cond) ->
      let cond' = normalize cond in
      if cond' == cond then c else Named (name, cond')
  (* CSS Values 4 sec. 9: function names are ASCII case-insensitive, so the
     [STYLE(] / [SCROLL-STATE(] spelling the AST keeps for round-trip says
     nothing about which query this is. *)
  | Style { query; uppercase } ->
      let query' = normalize_style_query query in
      if query' == query && not uppercase then c
      else Style { query = query'; uppercase = false }
  | Scroll_state { query; uppercase } ->
      if not uppercase then c else Scroll_state { query; uppercase = false }
  | And (a, b) ->
      let a' = normalize a and b' = normalize b in
      if a' == a && b' == b then c else And (a', b')
  | Or (a, b) ->
      let a' = normalize a and b' = normalize b in
      if a' == a && b' == b then c else Or (a', b')
  | Not a ->
      let a' = normalize a in
      if a' == a then c else Not a'

(* Written out rather than left to the structural operators: [equal] is the gate
   on block merging, and a comparison that walks a runtime representation is how
   a spelling difference becomes a merge. *)
let equal_components (a : component_values) b = List.equal Component.equal a b

let equal_range_operator (a : range_operator) b =
  match (a, b) with
  | Lt, Lt | Lte, Lte | Gt, Gt | Gte, Gte -> true
  | (Lt | Lte | Gt | Gte), _ -> false

let equal_style_range (a : style_range) b =
  String.equal a.name b.name
  && equal_range_operator a.lower_op b.lower_op
  && equal_range_operator a.upper_op b.upper_op
  && equal_components a.lower b.lower
  && equal_components a.upper b.upper

let rec equal_style_query (a : style_query) b =
  match (a, b) with
  | Boolean a, Boolean b -> String.equal a b
  | Declaration a, Declaration b ->
      String.equal a.name b.name && equal_components a.value b.value
  | Range a, Range b -> equal_style_range a b
  | All (a1, b1), All (a2, b2) | Any (a1, b1), Any (a2, b2) ->
      equal_style_query a1 a2 && equal_style_query b1 b2
  | Neg a, Neg b -> equal_style_query a b
  | (Boolean _ | Declaration _ | Range _ | All _ | Any _ | Neg _), _ -> false

let rec equal_scroll_state_query (a : scroll_state_query) b =
  match (a, b) with
  | State a, State b ->
      String.equal a.name b.name && String.equal a.value b.value
  | Both (a1, b1), Both (a2, b2) | Either (a1, b1), Either (a2, b2) ->
      equal_scroll_state_query a1 a2 && equal_scroll_state_query b1 b2
  | Negated a, Negated b -> equal_scroll_state_query a b
  | (State _ | Both _ | Either _ | Negated _), _ -> false

(* A [<container-name>] is a [<custom-ident>] (Conditional Rules 5 sec. 5.4), so
   it is compared by the name the parser unescaped, not by how it was
   spelled. *)
let rec equal_normalized (a : t) b =
  match (a, b) with
  | Min_width_rem a, Min_width_rem b -> Float.equal a b
  | Min_width_px a, Min_width_px b -> Int.equal a b
  | Named (n1, c1), Named (n2, c2) ->
      String.equal n1 n2 && equal_normalized c1 c2
  | Style a, Style b -> equal_style_query a.query b.query
  | Scroll_state a, Scroll_state b -> equal_scroll_state_query a.query b.query
  | And (a1, b1), And (a2, b2) | Or (a1, b1), Or (a2, b2) ->
      equal_normalized a1 a2 && equal_normalized b1 b2
  | Not a, Not b -> equal_normalized a b
  | Feature_query a, Feature_query b -> Media.equal a b
  | ( ( Min_width_rem _ | Min_width_px _ | Named _ | Style _ | Scroll_state _
      | And _ | Or _ | Not _ | Feature_query _ ),
      _ ) ->
      false

let equal a b = equal_normalized (normalize a) (normalize b)
