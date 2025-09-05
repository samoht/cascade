(** CSS selector parser using simple Reader API. *)

open Reader

(** Error helpers *)
let err_expected what = raise (Parse_error ("expected " ^ what))

(** Parse a class selector (.classname) *)
let class_selector t =
  expect t '.';
  Css.Selector.class_ (ident t)

(** Parse an ID selector (#id) *)
let id_selector t =
  expect t '#';
  Css.Selector.id (ident t)

(** Parse a type selector (element name) *)
let type_selector t = Css.Selector.element (ident t)

(** Parse universal selector (asterisk) *)
let universal_selector t =
  expect t '*';
  Css.Selector.universal

(** Parse attribute value (quoted or unquoted) *)
let attribute_value t =
  try_parse string t |> function
  | Some s -> s
  | None -> while_ t (fun c -> c <> ']' && c <> ' ' && c <> '\t' && c <> '\n')

(** Parse attribute selector [attr] or [attr=value] *)
let attribute_selector t =
  expect t '[';
  ws t;
  let attr = ident t in
  ws t;
  let matcher =
    match peek_string t 2 with
    | "~=" ->
        expect_string t "~=";
        ws t;
        Css.Selector.Whitespace_list (attribute_value t)
    | "|=" ->
        expect_string t "|=";
        ws t;
        Css.Selector.Hyphen_list (attribute_value t)
    | "^=" ->
        expect_string t "^=";
        ws t;
        Css.Selector.Prefix (attribute_value t)
    | "$=" ->
        expect_string t "$=";
        ws t;
        Css.Selector.Suffix (attribute_value t)
    | "*=" ->
        expect_string t "*=";
        ws t;
        Css.Selector.Substring (attribute_value t)
    | _ -> (
        peek t |> function
        | Some '=' ->
            expect t '=';
            ws t;
            Css.Selector.Exact (attribute_value t)
        | _ -> Css.Selector.Presence)
  in
  ws t;
  expect t ']';
  Css.Selector.attribute attr matcher

(** Parse pseudo-class (:hover, :nth-child(2n+1), etc.) *)
let pseudo_class t =
  expect t ':';
  let name = ident t in
  peek t |> function
  | Some '(' ->
      skip t;
      let args = until t ')' in
      expect t ')';
      Css.Selector.pseudo_class (name ^ "(" ^ args ^ ")")
  | _ -> Css.Selector.pseudo_class name

(** Parse pseudo-element (::before, ::after, etc.) *)
let pseudo_element t =
  expect_string t "::";
  Css.Selector.pseudo_element (ident t)

(** Parse a simple selector (one part) *)
let simple_selector t =
  peek t |> function
  | Some '.' -> class_selector t
  | Some '#' -> id_selector t
  | Some '[' -> attribute_selector t
  | Some ':' -> if looking_at t "::" then pseudo_element t else pseudo_class t
  | Some '*' -> universal_selector t
  | Some c when is_ident_start c -> type_selector t
  | _ -> err_expected "selector"

(** Parse a compound selector (multiple simple selectors without spaces) *)
let compound_selector t =
  let rec loop acc =
    match try_parse simple_selector t with
    | None -> acc
    | Some s -> loop (s :: acc)
  in
  match loop [] with
  | [] -> err_expected "at least one selector"
  | [ s ] -> s
  | selectors -> Css.Selector.compound (List.rev selectors)

(** Parse a complex selector (with combinators) *)
let rec complex_selector t =
  let left = compound_selector t in
  ws t;
  peek t |> function
  | Some '>' ->
      skip t;
      ws t;
      Css.Selector.combine left Css.Selector.Child (complex_selector t)
  | Some '+' ->
      skip t;
      ws t;
      Css.Selector.combine left Css.Selector.Next_sibling (complex_selector t)
  | Some '~' ->
      skip t;
      ws t;
      Css.Selector.combine left Css.Selector.Subsequent_sibling
        (complex_selector t)
  | Some ',' | Some '{' | None -> left
  | _ -> (
      (* Could be descendant combinator *)
      try_parse complex_selector t
      |> function
      | Some right -> Css.Selector.combine left Css.Selector.Descendant right
      | None -> left)

(** Main selector parser. *)
let one t = complex_selector t

(** Parse selector, return [None] on failure. *)
let one_opt t = try_parse one t
