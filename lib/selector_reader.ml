open Selector_intf
open Selector_make
open Selector_pseudo

let read_aria_attr t : aria_attr =
  let s = Cursor.ident t in
  match aria_attr_of_string s with
  | Some v -> v
  | None -> Cursor.err t ("not a supported aria attribute: " ^ s)

let read_attr_name t : attr_name =
  let s = Cursor.ident t in
  attr_name_of_string s

(* Simple readers that don't need recursion *)
let ensure_call_done t name =
  Cursor.ws t;
  if not (Cursor.is_done t) then Cursor.err t ("unexpected tokens after " ^ name)

let read_lang_range t =
  match Cursor.ident_opt t with
  | Some lang -> lang
  | None -> (
      match Cursor.string_opt t with
      | Some lang -> lang
      | None -> Cursor.err_expected t "language range")

let read_lang_content t =
  let langs = Cursor.list ~sep:Cursor.comma ~at_least:1 read_lang_range t in
  ensure_call_done t "lang";
  Lang langs

(* CSS Values 4 sec. 4.1 interprets a keyword ASCII case-insensitively, so the
   two directionalities Selectors 4 sec. 7.1 names reach one node however they
   are spelled. Any other identifier is no keyword, so it keeps its case. *)
let dir_keyword ident =
  match String.lowercase_ascii ident with
  | ("ltr" | "rtl") as keyword -> keyword
  | _ -> ident

let read_dir_content t =
  (* Selectors 4 sec. 7.1: the argument is a single identifier, and one other
     than [ltr] or [rtl] "is not invalid, but does not match anything". Keeping
     it verbatim also leaves room for a directionality a later markup spec
     defines. *)
  let dir = dir_keyword (Cursor.ident t) in
  ensure_call_done t "dir";
  Dir dir

let read_state_content t =
  let name = Cursor.ident t in
  ensure_call_done t "state";
  State name

let read_heading_content t =
  ensure_call_done t "heading";
  Heading

let read_active_view_transition_content t =
  let names = Cursor.list ~sep:Cursor.comma ~at_least:1 Cursor.ident t in
  ensure_call_done t "active view transition type";
  Active_view_transition_type (Some names)

let read_lang t = Cursor.call "lang" t read_lang_content
let read_dir t = Cursor.call "dir" t read_dir_content
let read_state t = Cursor.call "state" t read_state_content
let read_heading t = Cursor.call "heading" t read_heading_content

let read_active_view_transition_type t =
  Cursor.call "active-view-transition-type" t
    read_active_view_transition_content

let read_part_content t =
  (* CSS Shadow 1 section 5.4 [::part()]: a whitespace-separated list of ident
     tokens, *not* comma-separated. *)
  let rec read_idents acc =
    Cursor.ws t;
    if Cursor.is_done t then List.rev acc
    else
      let name = Cursor.ident t in
      read_idents (name :: acc)
  in
  let idents = read_idents [] in
  if idents = [] then Cursor.err_expected t "part name" else Part idents

let read_part t = Cursor.call "part" t read_part_content
let err_expected t what = Cursor.err_expected t what

(** Parse attribute value (quoted or unquoted) *)
let read_attribute_value_ident t s (loc : Loc.t) =
  Cursor.skip t;
  let raw =
    match Cursor.source t with
    | Some source ->
        Some (String.sub source loc.start_pos (loc.end_pos - loc.start_pos))
    | None -> None
  in
  match raw with Some raw when String.contains raw '\\' -> raw | _ -> s

(* Selectors 4 sec. 6.1: an unquoted value is an [<ident-token>]; a number, a
   dimension or a percentage there is an invalid selector, as Chrome reads
   it. *)
let read_attribute_value_unquoted t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident s; loc; _ }) ->
      read_attribute_value_ident t s loc
  | _ -> ""

let read_attribute_value t =
  (* Check if we start with a quote - if so, we MUST parse as quoted string *)
  let value, quote =
    match Cursor.string_with_quote_opt t with
    | Some (s, quote) -> (s, Some quote)
    | None -> (read_attribute_value_unquoted t, None)
  in
  (* CSS spec allows empty quoted strings but not empty unquoted values *)
  if value = "" && Option.is_none quote then
    match Cursor.peek t with
    | None -> Cursor.err_expected_but_eof t "']'"
    | Some _ -> Cursor.err_invalid t "attribute value"
  else (value, quote)

(* Selectors 4 sec. 5.1 spells a class as a [.] "immediately followed by" an
   ident, and sec. 3.5 a pseudo-class or pseudo-element as its colons followed
   by the name. Every other reader has the cursor skip whitespace for it, so
   these three look for it themselves: [. x] names no class, and reading past
   the space gave the author [.x]. *)
let expect_adjacent t what =
  match Cursor.peek_raw t with
  | Some c when Component.is_whitespace c ->
      Cursor.err_invalid t (String.concat "" [ "whitespace before the "; what ])
  | _ -> ()

(** Parse a class selector (.classname) *)
let read_class t =
  Cursor.expect '.' t;
  expect_adjacent t "class name";
  let name = Cursor.ident ~keep_case:true t in
  (* No validation needed: Cursor.ident already enforces CSS identifier syntax,
     including parser-valid double-dash identifiers such as .--x. *)
  Class name

(** Parse an ID selector ([#id]). Per CSS Selectors sec. 6.7, an ID must be an
    ident-type hash; unrestricted hashes such as digit-only [#123] are not valid
    IDs. *)
let read_id t =
  match Cursor.peek t with
  | Some
      (Component.Preserved
         { kind = Token.Hash { value; hash_flag = Token.Id }; _ }) ->
      Cursor.skip t;
      Id value
  | Some (Component.Preserved { kind = Token.Hash _; _ }) ->
      Cursor.err_invalid t "expected identifier"
  | _ -> Cursor.err_expected t "'#'"

let peek_is_namespaced_name t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident _; _ }) -> true
  | Some (Component.Preserved { kind = Token.Delim "*"; _ }) -> true
  | _ -> false

let lookahead_bare_pipe_ns t =
  (* Bare ['|'] selects the default (no) namespace. Selectors Level 4 section
     6.2 distinguishes [[|attr]] from [[attr]]: the former explicitly matches
     the empty namespace, the latter matches any. *)
  Cursor.lookahead
    (fun t ->
      match Cursor.peek_delim t with
      | Some '|' ->
          let _ = Cursor.next t in
          (* Reject ['|='] (the dash-match operator). *)
          if Cursor.peek_delim t = Some '=' then false
          else
            (* Bare ['|'] is only a namespace prefix when a namespaced element
               or attribute name follows it. *)
            peek_is_namespaced_name t
      | _ -> false)
    t

(* CSS Selectors 4 sec. 15.2: [||] is the column combinator, so a pipe opening
   one never separates a namespace prefix from a name. *)
let at_column_combinator t =
  Cursor.lookahead
    (fun t -> Cursor.try_kind_pair (Token.Delim "|") (Token.Delim "|") t)
    t

let read_prefixed_ns t =
  let p = Cursor.ident ~keep_case:true t in
  (* Avoid treating '|=' as a namespace separator: peek for the pair. *)
  let is_eq_pair =
    Cursor.lookahead
      (fun t -> Cursor.try_kind_pair (Token.Delim "|") (Token.Delim "=") t)
      t
  in
  if is_eq_pair || at_column_combinator t then Cursor.err t "not a namespace";
  Cursor.expect '|' t;
  Prefix p

let read_ns_inner t =
  if Cursor.try_kind_pair (Token.Delim "*") (Token.Delim "|") t then
    (* Another pipe means [*||td]: the universal selector, then a column. *)
    if Cursor.peek_delim t = Some '|' then Cursor.err t "not a namespace"
    else Any
  else if lookahead_bare_pipe_ns t then (
    Cursor.expect '|' t;
    None)
  else read_prefixed_ns t

let read_ns t : ns option = Cursor.option read_ns_inner t

(** Parse a namespaced type or universal selector *)
let read_type_or_universal t =
  let ns = read_ns t in

  (* Now read the selector itself *)
  match Cursor.peek_delim t with
  | Some '*' -> (
      Cursor.skip t;
      match ns with None -> universal | Some ns -> universal_ns ns)
  | _ -> (
      let name = Cursor.ident ~keep_case:true t in
      (* Cursor.ident is the parser contract here. Constructors can keep their
         stricter policy, but parsed CSS identifiers such as --x are valid. *)
      match ns with
      | None -> Element (None, name)
      | Some ns -> Element (Some ns, name))

(** Parse attribute selector [attr] or [attr=value] *)
let try_shadow_piercing t =
  (* Legacy [>>>]: three consecutive [>] delims; emit Shadow_piercing only when
     the entire run matches. *)
  let snap = Cursor.save t in
  if
    Cursor.try_kind (Token.Delim ">") t
    && Cursor.try_kind (Token.Delim ">") t
    && Cursor.try_kind (Token.Delim ">") t
  then true
  else (
    Cursor.restore t snap;
    false)

let try_shadow_deep t =
  (* Legacy [/deep/]: [/] [ident "deep"] [/]. *)
  let snap = Cursor.save t in
  if
    Cursor.try_kind (Token.Delim "/") t
    && Cursor.try_ident "deep" t
    && Cursor.try_kind (Token.Delim "/") t
  then true
  else (
    Cursor.restore t snap;
    false)

let read_combinator t =
  if try_shadow_piercing t then Shadow_piercing
  else if try_shadow_deep t then Shadow_deep
  else
    match Cursor.peek_delim t with
    | Some '>' ->
        Cursor.skip t;
        Child
    | Some '+' ->
        Cursor.skip t;
        Next_sibling
    | Some '~' ->
        Cursor.skip t;
        Subsequent_sibling
    | Some '|' when Cursor.try_kind_pair (Token.Delim "|") (Token.Delim "|") t
      ->
        Column
    | Some '!' -> Cursor.err t "invalid combinator character"
    | None when Cursor.is_done t -> Cursor.err t "empty combinator"
    | _ -> Descendant

let attribute_match cons cons_quoted (value, quote) =
  match quote with Some quote -> cons_quoted value quote | None -> cons value

let try_attribute_op c cons cons_quoted t : attribute_match option =
  if Cursor.try_kind_pair (Token.Delim (String.make 1 c)) (Token.Delim "=") t
  then Some (attribute_match cons cons_quoted (read_attribute_value t))
  else None

let try_whitespace_list_match t =
  try_attribute_op '~'
    (fun v -> Whitespace_list v)
    (fun v q -> Whitespace_list_quoted (v, q))
    t

let try_hyphen_list_match t =
  try_attribute_op '|'
    (fun v -> Hyphen_list v)
    (fun v q -> Hyphen_list_quoted (v, q))
    t

let try_prefix_match t =
  try_attribute_op '^' (fun v -> Prefix v) (fun v q -> Prefix_quoted (v, q)) t

let try_suffix_match t =
  try_attribute_op '$' (fun v -> Suffix v) (fun v q -> Suffix_quoted (v, q)) t

let try_substring_match t =
  try_attribute_op '*'
    (fun v -> Substring v)
    (fun v q -> Substring_quoted (v, q))
    t

let read_exact_or_presence t =
  if Cursor.peek_delim t = Some '=' then (
    Cursor.skip t;
    attribute_match
      (fun v -> Exact v)
      (fun v q -> Exact_quoted (v, q))
      (read_attribute_value t))
  else Presence

let read_attribute_match t : attribute_match =
  let try_ops =
    [
      try_whitespace_list_match;
      try_hyphen_list_match;
      try_prefix_match;
      try_suffix_match;
      try_substring_match;
    ]
  in
  let rec find = function
    | [] -> read_exact_or_presence t
    | op :: rest -> ( match op t with Some v -> v | None -> find rest)
  in
  find try_ops

let read_attr_flag t : attr_flag option =
  Cursor.ws t;
  Cursor.option
    (fun t ->
      match Cursor.ident_opt t with
      | Some s when String.lowercase_ascii s = "i" -> Insensitive
      | Some s when String.lowercase_ascii s = "s" -> Sensitive
      | Some s -> Cursor.err t ~got:s "'i' or 's'"
      | None -> Cursor.err_unexpected t)
    t

let read_attribute t =
  Cursor.brackets
    (fun inner ->
      Cursor.ws inner;
      let ns = read_ns inner in
      let attr = Cursor.ident ~keep_case:true inner in
      Cursor.ws inner;
      let matcher = read_attribute_match inner in
      Cursor.ws inner;
      let flag = read_attr_flag inner in
      Cursor.ws inner;
      if not (Cursor.is_done inner) then
        Cursor.err_invalid inner "trailing tokens in attribute selector";
      let attr_name = attr_name_of_string attr in
      Attribute (ns, attr_name, matcher, flag))
    t

(** Parse the An+B microsyntax (Selectors 4 section 13.3.1 / CSS Syntax 3 (ED)
    section 6) as shape patterns over the component stream: [odd]/[even], bare
    [<integer>], [<n-dimension>] with optional offset, the [5n-5]/[5n-] token
    variants, and the [n]/[-n]/[n-5]/... ident forms with an optional leading
    [+]. Idents are case-insensitive (CSS Values 4 sec. 4.1); [+ n] (whitespace
    after [+]) is invalid since [+] is lexically part of the ident form. *)

(* Numeric helpers: split an arbitrary ident's tail into an optional [-digits]
   suffix, for ndashdigit / ndash / n patterns. *)
let all_digits s =
  String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s

let ndashdigit_b unit_ =
  let n = String.length unit_ in
  if n >= 3 && Char.lowercase_ascii unit_.[0] = 'n' && unit_.[1] = '-' then
    let tail = String.sub unit_ 2 (n - 2) in
    if all_digits tail then int_of_string_opt tail else None
  else None

let is_ndash unit_ =
  String.length unit_ = 2
  && Char.lowercase_ascii unit_.[0] = 'n'
  && unit_.[1] = '-'

let is_n_unit unit_ =
  String.length unit_ = 1 && Char.lowercase_ascii unit_.[0] = 'n'

let dashndashdigit_b ident =
  let n = String.length ident in
  if
    n >= 4
    && ident.[0] = '-'
    && Char.lowercase_ascii ident.[1] = 'n'
    && ident.[2] = '-'
  then
    let tail = String.sub ident 3 (n - 3) in
    if all_digits tail then int_of_string_opt tail else None
  else None

let is_n_ident ident = String.lowercase_ascii ident = "n"
let is_neg_n_ident ident = String.lowercase_ascii ident = "-n"

let is_ndash_ident ident =
  String.length ident = 2
  && Char.lowercase_ascii ident.[0] = 'n'
  && ident.[1] = '-'

let is_dashndash_ident ident =
  String.length ident = 3
  && ident.[0] = '-'
  && Char.lowercase_ascii ident.[1] = 'n'
  && ident.[2] = '-'

(* A signed number's repr starts with [+] or [-]; signless means it doesn't. *)
let repr_is_signed (number : Token.number) =
  let r = number.repr in
  String.length r > 0 && (r.[0] = '+' || r.[0] = '-')

let integer_token t (number : Token.number) =
  match Token.integer_opt number with
  | Some value -> value
  | None when number.number_flag = Token.Integer ->
      Cursor.err_invalid t "integer outside supported range"
  | None -> Cursor.err_expected t "integer"

(* Parse a [<signless-integer>] (no leading [+]/[-] in the token repr). *)
let read_signless_integer t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Number_tok n; _ })
    when not (repr_is_signed n) ->
      let value = integer_token t n in
      Cursor.skip t;
      value
  | _ -> Cursor.err_expected t "signless integer"

(* Parse a [<signed-integer>]: a single number token whose repr starts with [+]
   or [-]. *)
let read_signed_integer_opt t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Number_tok n; _ })
    when repr_is_signed n && n.number_flag = Token.Integer -> (
      match Token.integer_opt n with
      | Some value ->
          Cursor.skip t;
          Some value
      | None -> None)
  | _ -> None

(* After an [<n-dimension>] or n-ident, consume the optional offset tail: -
   nothing (EOF / comma / close-paren in enclosing context), -
   [<signed-integer>] (a single signed number), - ['+' | '-']
   [<signless-integer>] (explicit operator + unsigned int). *)
let read_an_tail t =
  match read_signed_integer_opt t with
  | Some n -> n
  | None -> (
      match Cursor.peek_delim t with
      | Some '+' ->
          Cursor.skip t;
          read_signless_integer t
      | Some '-' ->
          Cursor.skip t;
          -read_signless_integer t
      | _ -> 0)

(* Reject a leading [+] that is separated from the following ident by
   whitespace. Per the grammar, the ['+'? n] form does not admit whitespace
   between the [+] and [n]. *)
let ensure_no_ws_after_plus t =
  match Cursor.peek_raw t with
  | Some (Component.Preserved { kind = Token.Whitespace _; _ }) ->
      Cursor.err_invalid t "whitespace after '+'"
  | _ -> ()

(* Dimension forms [<n-dimension>, <ndashdigit-dimension>, <ndash-dimension>]
   from CSS Syntax 3 (ED) section 6.2. Assumes the cursor is positioned on a
   [Dimension] component. *)
let read_nth_dimension t number unit_ =
  let coefficient = integer_token t number in
  if is_n_unit unit_ then (
    Cursor.skip t;
    An_plus_b (coefficient, read_an_tail t))
  else
    match ndashdigit_b unit_ with
    | Some b ->
        Cursor.skip t;
        An_plus_b (coefficient, -b)
    | None ->
        if is_ndash unit_ then (
          Cursor.skip t;
          An_plus_b (coefficient, -read_signless_integer t))
        else Cursor.err_expected t "An+B dimension (n / n-N / n-)"

(* Fall-through for ident forms not caught by the explicit predicates: must be
   [<ndashdigit-ident>] [n-<digits>] or [<dashndashdigit-ident>]
   [-n-<digits>]. *)
let read_nth_ident_tail t s =
  match ndashdigit_b s with
  | Some b ->
      Cursor.skip t;
      An_plus_b (1, -b)
  | None -> (
      match dashndashdigit_b s with
      | Some b ->
          Cursor.skip t;
          An_plus_b (-1, -b)
      | None -> Cursor.err t ("not an An+B ident: " ^ s))

(* After a leading [+] delim, only the positive ident forms are valid. *)
let read_nth_after_plus t =
  ensure_no_ws_after_plus t;
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ }) when is_n_ident s ->
      Cursor.skip t;
      An_plus_b (1, read_an_tail t)
  | Some (Component.Preserved { kind = Token.Ident s; _ }) when is_ndash_ident s
    ->
      Cursor.skip t;
      An_plus_b (1, -read_signless_integer t)
  | Some (Component.Preserved { kind = Token.Ident s; _ }) -> (
      match ndashdigit_b s with
      | Some b ->
          Cursor.skip t;
          An_plus_b (1, -b)
      | None -> Cursor.err_expected t "An+B after '+'")
  | _ -> Cursor.err_expected t "An+B after '+'"

let read_nth t : nth =
  Cursor.ws t;
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident s; _ })
    when String.lowercase_ascii s = "odd" ->
      Cursor.skip t;
      Odd
  | Some (Component.Preserved { kind = Token.Ident s; _ })
    when String.lowercase_ascii s = "even" ->
      Cursor.skip t;
      Even
  | Some
      (Component.Preserved
         { kind = Token.Number_tok { number_flag = Integer; _ }; _ }) ->
      Index (Cursor.int t)
  | Some (Component.Preserved { kind = Token.Dimension { number; unit_ }; _ })
    ->
      read_nth_dimension t number unit_
  | Some (Component.Preserved { kind = Token.Ident s; _ }) when is_n_ident s ->
      Cursor.skip t;
      An_plus_b (1, read_an_tail t)
  | Some (Component.Preserved { kind = Token.Ident s; _ }) when is_neg_n_ident s
    ->
      Cursor.skip t;
      An_plus_b (-1, read_an_tail t)
  | Some (Component.Preserved { kind = Token.Ident s; _ }) when is_ndash_ident s
    ->
      Cursor.skip t;
      An_plus_b (1, -read_signless_integer t)
  | Some (Component.Preserved { kind = Token.Ident s; _ })
    when is_dashndash_ident s ->
      Cursor.skip t;
      An_plus_b (-1, -read_signless_integer t)
  | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
      read_nth_ident_tail t s
  | Some (Component.Preserved { kind = Token.Delim "+"; _ }) ->
      Cursor.skip t;
      read_nth_after_plus t
  | _ -> Cursor.err t "expected 'odd', 'even', or An+B expression"

let read_unknown_pseudo_class_call ~all_idents t =
  match Cursor.peek t with
  | Some (Component.Func { node = { name; arguments; _ }; _ }) ->
      (* CSS Selectors 4 sec. 3.5: a known non-functional pseudo ([:checked],
         [:hover], ...) called with parens ([:checked()]) is invalid. Reject so
         the rule reader drops it rather than passing through as an unknown
         call. *)
      let lower = String.lowercase_ascii name in
      let is_known_non_functional =
        List.exists (fun (n, _) -> String.lowercase_ascii n = lower) all_idents
      in
      if is_known_non_functional then
        Cursor.err_invalid t ("pseudo-class is not functional: " ^ name);
      Cursor.skip t;
      Unknown_pseudo_class_call (name, arguments)
  | _ -> Cursor.err_expected t "pseudo-class call"

let read_unknown_pseudo_class_ident t =
  match Cursor.ident_opt t with
  | Some name -> Unknown_pseudo_class name
  | None -> Cursor.err_expected t "pseudo-class"

let read_unknown_pseudo_class ~all_idents t =
  Cursor.one_of
    [
      read_unknown_pseudo_class_call ~all_idents;
      read_unknown_pseudo_class_ident;
    ]
    t

let rec forgiving_take_next_in_segment t acc =
  match Cursor.next_raw t with
  | None -> List.rev acc
  | Some cv -> forgiving_take_segment t (cv :: acc)

and forgiving_take_segment t acc =
  let is_comma = function
    | Component.Preserved { kind = Token.Comma; _ } -> true
    | _ -> false
  in
  match Cursor.peek_raw t with
  | None -> List.rev acc
  | Some cv when is_comma cv ->
      ignore (Cursor.next_raw t : Component.t option);
      List.rev acc
  | Some _ -> forgiving_take_next_in_segment t acc

let read_forgiving_segment read_item t acc =
  let item = Cursor.sub t (forgiving_take_segment t []) in
  match read_item item with
  | sel ->
      Cursor.ws item;
      if Cursor.is_done item then sel :: acc else acc
  (* Selectors 4 sec. 16.1 drops an invalid selector from a forgiving list. One
     nested deeper than cascade reads may well be valid, so it is not dropped as
     if it were: the rule holding it fails instead. *)
  | exception Cursor.Parse_error e -> (
      match e.kind with
      | Error.Too_deep _ -> raise (Cursor.Parse_error e)
      | _ -> acc)

let read_nth_expr t =
  let expr = read_nth t in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "unexpected tokens after An+B expression";
  expr

let read_nth_col_content t = Nth_col (read_nth_expr t)
let read_nth_last_col_content t = Nth_last_col (read_nth_expr t)
let read_nth_col t = Cursor.call "nth-col" t read_nth_col_content
let read_nth_last_col t = Cursor.call "nth-last-col" t read_nth_last_col_content

let read_highlight_content t =
  (* ::highlight() takes a single custom-ident per CSS Custom Highlight API sec.
     3.1; comma-separated names are rejected. *)
  let name = Cursor.ident t in
  ensure_call_done t "highlight";
  Highlight [ name ]

let read_vt_class_selector t : vt_class_selector =
  (* CSS View Transitions 2 sec. 10.4 [<vt-class-selector>] = [<vt-name>?
     [.<custom-ident>]*]. The name is [<custom-ident> | *]; either the name or
     at least one class must be present. *)
  Cursor.ws t;
  let name =
    match Cursor.peek_delim t with
    | Some '*' ->
        Cursor.skip t;
        Some "*"
    | Some '.' -> None
    | _ -> Some (Cursor.ident t)
  in
  let rec read_classes acc =
    match Cursor.peek_delim t with
    | Some '.' ->
        Cursor.skip t;
        let cls = Cursor.ident t in
        read_classes (cls :: acc)
    | _ -> List.rev acc
  in
  let classes = read_classes [] in
  { name; classes }

let read_view_transition_group_content t =
  let sel = read_vt_class_selector t in
  ensure_call_done t "view transition group";
  View_transition_group sel

let read_vt_image_pair_content t =
  let sel = read_vt_class_selector t in
  ensure_call_done t "view transition image pair";
  View_transition_image_pair sel

let read_view_transition_old_content t =
  let sel = read_vt_class_selector t in
  ensure_call_done t "view transition old";
  View_transition_old sel

let read_view_transition_new_content t =
  let sel = read_vt_class_selector t in
  ensure_call_done t "view transition new";
  View_transition_new sel

let rec read_selector_list_tail read_item t acc =
  let sel = read_item t in
  let acc = sel :: acc in
  Cursor.ws t;
  if Cursor.comma_opt t then (
    Cursor.ws t;
    if Cursor.is_done t then Cursor.err t "expected at least one selector";
    read_selector_list_tail read_item t acc)
  else if Cursor.is_done t then List.rev acc
  else Cursor.err t "unexpected tokens after selector"

let read_selector_list_with read_item t =
  Cursor.ws t;
  if Cursor.is_done t then Cursor.err t "expected at least one selector"
  else read_selector_list_tail read_item t []

let read_forgiving_list read_item t =
  let rec loop acc =
    if Cursor.is_done t then List.rev acc
    else loop (read_forgiving_segment read_item t acc)
  in
  loop []

(* Forward declarations for mutually recursive functions *)
let rec read_complex_list t = read_selector_list_with read_complex t

and read_forgiving_complex_list t =
  read_forgiving_list read_forgiving_complex_item t

and read_forgiving_complex_item t =
  let sel = read_complex t in
  if has_pseudo_element sel then Cursor.err t "pseudo-element not allowed here";
  if has_unknown_pseudo_class sel then Cursor.err t "unknown pseudo-class";
  sel

(** Read nth selector with optional "of S" clause *)
and read_nth_selector t : nth * t list option =
  let expr = read_nth t in
  Cursor.ws t;

  (* Check for "of S" clause *)
  let of_clause =
    Cursor.option
      (fun t ->
        Cursor.expect_string "of" t;
        Cursor.ws t;
        Cursor.list ~sep:Cursor.comma ~at_least:1 read_complex t)
      t
  in
  (* Per Selectors Level 4 section 13.3.1, the An+B (plus optional [of S]) must
     consume the entire [<nth-child>] argument list. Leftover tokens (e.g.
     [:nth-child(1 - n)] or [:nth-child(2 n + 2)]) are a parse error, not a
     silently-dropped tail. *)
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "unexpected tokens after An+B expression";
  (expr, of_clause)

(** Parse a relative selector (used inside :has()). A relative selector can
    start with a combinator (+, >, ~) without a left operand. *)
and read_relative_selector t =
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some ('+' | '>' | '~') ->
      let comb = read_combinator t in
      Cursor.ws t;
      let right = read_complex t in
      Relative (comb, right)
  | Some '/' when Cursor.lookahead try_shadow_deep t ->
      let comb = read_combinator t in
      Cursor.ws t;
      let right = read_complex t in
      Relative (comb, right)
  | _ -> read_complex t

and read_relative_selector_list t =
  read_selector_list_with read_relative_selector t

(* Helper readers for functional pseudo-class content *)
and read_is_content t = Is (read_forgiving_complex_list t)
and read_moz_any_content t = Moz_any_call (read_forgiving_complex_list t)
and read_webkit_any_content t = Webkit_any_call (read_forgiving_complex_list t)

and read_has_content t =
  let selectors = read_relative_selector_list t in
  let contains_has sel = any (function Has _ -> true | _ -> false) sel in
  List.iter
    (fun sel ->
      if contains_has sel then Cursor.err t ":has() cannot contain :has()";
      if has_pseudo_element sel then
        Cursor.err t ":has() cannot contain pseudo-elements";
      if has_unknown_pseudo_class sel then
        Cursor.err t ":has() cannot contain an unknown pseudo-class")
    selectors;
  Has selectors

and read_not_content t =
  let selectors = read_complex_list t in
  (* CSS Selectors 4 sec. 4.3: [:not()] takes a [<complex-real-selector-list>],
     which sec. 16 builds out of [<compound-selector>]s alone, with no
     [<pseudo-compound-selector>] and so no pseudo-element. It is also
     non-forgiving, so a pseudo-element or an unknown selector anywhere in the
     argument invalidates the whole rule instead of just its own item. Top-level
     lists keep unknown pseudo-classes for forward compatibility. *)
  List.iter
    (fun sel ->
      if has_pseudo_element sel then
        Cursor.err t ":not() cannot contain pseudo-elements";
      if has_unknown_pseudo_class sel then
        Cursor.err t ":not() cannot contain an unknown pseudo-class")
    selectors;
  Not selectors

and read_where_content t = Where (read_forgiving_complex_list t)
and read_local_content t = Local_call (read_complex_list t)
and read_global_content t = Global_call (read_complex_list t)

and read_nth_child_content t =
  let expr, of_sel = read_nth_selector t in
  Nth_child (expr, of_sel)

and read_nth_last_child_content t =
  let expr, of_sel = read_nth_selector t in
  Nth_last_child (expr, of_sel)

and read_nth_of_type_content t =
  let expr, of_sel = read_nth_selector t in
  Nth_of_type (expr, of_sel)

and read_nth_last_type_content t =
  let expr, of_sel = read_nth_selector t in
  Nth_last_of_type (expr, of_sel)

and read_host_content t = Host (Cursor.option read_complex_list t)
and read_host_context_content t = Host_context (read_complex_list t)
and read_current_content t = Current_of (read_complex_list t)

(* Read helper functions for functional pseudo-classes *)
and read_is t = Cursor.call "is" t read_is_content
and read_moz_any t = Cursor.call "-moz-any" t read_moz_any_content
and read_webkit_any t = Cursor.call "-webkit-any" t read_webkit_any_content
and read_has t = Cursor.call "has" t read_has_content
and read_not t = Cursor.call "not" t read_not_content
and read_where t = Cursor.call "where" t read_where_content
and read_local t = Cursor.call "local" t read_local_content
and read_global t = Cursor.call "global" t read_global_content
and read_nth_child t = Cursor.call "nth-child" t read_nth_child_content

and read_nth_last_child t =
  Cursor.call "nth-last-child" t read_nth_last_child_content

and read_nth_of_type t = Cursor.call "nth-of-type" t read_nth_of_type_content

and read_nth_last_of_type t =
  Cursor.call "nth-last-of-type" t read_nth_last_type_content

and read_host t = Cursor.call "host" t read_host_content
and read_host_context t = Cursor.call "host-context" t read_host_context_content
and read_current t = Cursor.call "current" t read_current_content

(* Helper readers for pseudo-element functions that need recursion *)
and read_slotted_content t =
  (* CSS Shadow 1 section 3.2.4 [::slotted()] takes a single compound selector;
     comma-separated lists are a syntax error. *)
  let sel = read_complex t in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "::slotted() accepts a single compound selector";
  Slotted [ sel ]

and read_cue_content t =
  let sels = read_complex_list t in
  Cue (Some sels)

and read_cue_region_content t =
  let sels = read_complex_list t in
  Cue_region (Some sels)

and read_slotted t = Cursor.call "slotted" t read_slotted_content
and read_cue t = Cursor.call "cue" t read_cue_content
and read_cue_region t = Cursor.call "cue-region" t read_cue_region_content

and pseudo_class_calls_lazy =
  lazy
    [
      ("is", read_is);
      ("-moz-any", read_moz_any);
      ("-webkit-any", read_webkit_any);
      ("has", read_has);
      ("not", read_not);
      ("where", read_where);
      ("local", read_local);
      ("global", read_global);
      ("nth-child", read_nth_child);
      ("nth-last-child", read_nth_last_child);
      ("nth-of-type", read_nth_of_type);
      ("nth-last-of-type", read_nth_last_of_type);
      ("nth-col", read_nth_col);
      ("nth-last-col", read_nth_last_col);
      ("lang", read_lang);
      ("dir", read_dir);
      ("state", read_state);
      ("host", read_host);
      ("host-context", read_host_context);
      ("current", read_current);
      ("heading", read_heading);
      ("active-view-transition-type", read_active_view_transition_type);
    ]

and pseudo_class_calls () = Lazy.force pseudo_class_calls_lazy

(** Parse pseudo-class (:hover, :nth-child(2n+1), etc.) *)
and read_pseudo_class ?(allow_unknown = false) t =
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  expect_adjacent t "pseudo-class name";
  let all_idents = pseudo_class_all_idents () in
  let calls = pseudo_class_calls () in
  let read_unknown = read_unknown_pseudo_class ~all_idents in
  if allow_unknown then
    Cursor.enum_or_calls "pseudo-class" all_idents ~calls ~default:read_unknown
      t
  else Cursor.enum_or_calls "pseudo-class" all_idents ~calls t

(** Parse pseudo-element (::before, ::after, etc.) *)
and read_pseudo_element t =
  if not (Cursor.try_kind_pair Token.Colon Token.Colon t) then
    Cursor.err_expected t "'::'";
  expect_adjacent t "pseudo-element name";
  Cursor.enum_calls
    [
      ("part", read_part);
      ("slotted", read_slotted);
      ("cue", read_cue);
      ("cue-region", read_cue_region);
      ("highlight", fun t -> Cursor.call "highlight" t read_highlight_content);
      ( "view-transition-group",
        fun t ->
          Cursor.call "view-transition-group" t
            read_view_transition_group_content );
      ( "view-transition-image-pair",
        fun t ->
          Cursor.call "view-transition-image-pair" t read_vt_image_pair_content
      );
      ( "view-transition-old",
        fun t ->
          Cursor.call "view-transition-old" t read_view_transition_old_content
      );
      ( "view-transition-new",
        fun t ->
          Cursor.call "view-transition-new" t read_view_transition_new_content
      );
    ]
    ~default:(fun t ->
      let read_unknown_call t =
        (* Unknown functional pseudo-element: keep the call body verbatim so the
           printer can re-emit the exact same source. *)
        match Cursor.peek t with
        | Some (Component.Func { node = { name; arguments; _ }; _ }) ->
            Cursor.skip t;
            Unknown_pseudo_element_call (name, arguments)
        | _ -> Cursor.err_expected t "pseudo-element call"
      in
      let read_unknown_ident t =
        match Cursor.ident_opt t with
        | Some name -> Unknown_pseudo_element name
        | None -> Cursor.err_expected t "pseudo-element"
      in
      Cursor.enum "pseudo-element"
        (Lazy.force pseudo_element_unknown_idents)
        ~default:(fun t ->
          Cursor.one_of [ read_unknown_call; read_unknown_ident ] t)
        t)
    t

(** Parse a simple selector (one part). Does not skip leading whitespace -- the
    caller (read_compound) uses whitespace as a compound / descendant boundary
    marker. *)
and read_simple ?(allow_unknown_pseudo_class = false) t =
  match Cursor.peek_delim t with
  | Some '.' -> read_class t
  | Some ('*' | '|') -> read_type_or_universal t
  | Some '&' ->
      Cursor.skip t;
      Nesting
  | _ -> (
      if Cursor.peek_hash t <> None then read_id t
      else if Cursor.peek_block t = Some Token.Square then read_attribute t
      else if Cursor.peek_colon t then
        (* [::] for pseudo-element, [:] for pseudo-class. *)
        let snap = Cursor.save t in
        if Cursor.try_kind_pair Token.Colon Token.Colon t then (
          Cursor.restore t snap;
          read_pseudo_element t)
        else read_pseudo_class ~allow_unknown:allow_unknown_pseudo_class t
      else
        match Cursor.peek_ident t with
        | Some _ -> read_type_or_universal t
        | None -> err_expected t "selector")

(** Parse a compound selector (multiple simple selectors without spaces).
    Leading whitespace is skipped; whitespace {e between} simple selectors stops
    the compound (it marks the descendant combinator at the enclosing
    complex-selector level). *)
and read_compound t =
  Cursor.ws t;
  let can_start () =
    match Cursor.peek_raw t with
    | Some (Component.Preserved { kind = Token.Whitespace _; _ }) -> false
    | _ ->
        (match Cursor.peek_delim t with
          | Some ('.' | '*' | '&') -> true
          (* A pipe extends the compound only as the [|name] prefix; a [||] ends
             it so [read_complex] can take the column combinator. *)
          | Some '|' -> lookahead_bare_pipe_ns t
          | _ -> false)
        || Cursor.peek_hash t <> None
        || Cursor.peek_colon t
        || Cursor.peek_block t = Some Token.Square
        || Cursor.peek_ident t <> None
  in
  let prepend_simple acc =
    (* CSS Selectors 4 sec. 3.5: tolerate unknown pseudo-classes for forward
       compat (vendor pseudos, authored future-pseudos must round-trip).
       Non-forgiving rejection lives where the spec requires it: inside
       [:not()]/[:has()] ([read_not_content]/[read_has_content]) and in the rule
       reader when a whole selector list is unknown. *)
    let s = read_simple ~allow_unknown_pseudo_class:true t in
    match compound_refusal ~seen:acc s with
    | Some Sub_pseudo_element ->
        Cursor.err t "pseudo-element not allowed after this pseudo-element"
    | Some Simple_after_pseudo_element ->
        Cursor.err t "pseudo-element must be last in compound selector"
    | Some Pseudo_class_after_pseudo_element ->
        Cursor.err t "pseudo-class not allowed after this pseudo-element"
    | None -> s :: acc
  in
  let rec loop acc = if can_start () then loop (prepend_simple acc) else acc in
  match loop [] with
  | [] -> err_expected t "at least one selector"
  | [ s ] -> s
  | selectors -> compound (List.rev selectors)

(** Parse a complex selector (with combinators) *)
and read_complex t =
  let left = read_compound t in
  Cursor.ws t;
  let can_start_selector () =
    (match Cursor.peek_delim t with
      | Some ('.' | '*' | '|' | '&') -> true
      | _ -> false)
    || Cursor.peek_hash t <> None
    || Cursor.peek_colon t
    || Cursor.peek_block t = Some Token.Square
    || Cursor.peek_ident t <> None
  in
  (* CSS Selectors 4 sec. 3.6.5, via [bars_following_combinator]. [>>>] and
     [/deep/] are Vue / Angular tooling spellings rather than CSS combinators,
     and no engine parses either, so the rule never reaches them and cascade
     passes them through as it does an unmodelled pseudo-element name. *)
  let check_combinator = function
    | Shadow_piercing | Shadow_deep -> ()
    | Descendant | Child | Next_sibling | Subsequent_sibling | Column ->
        if bars_following_combinator left then
          Cursor.err t "combinator not allowed after a pseudo-element"
  in
  let read_combined () =
    let comb = read_combinator t in
    check_combinator comb;
    Cursor.ws t;
    combine left comb (read_complex t)
  in
  match Cursor.peek_delim t with
  | Some '|'
    when Cursor.lookahead
           (fun t -> Cursor.try_kind_pair (Token.Delim "|") (Token.Delim "|") t)
           t ->
      read_combined ()
  | Some ('>' | '+' | '~') -> read_combined ()
  | Some '/' when Cursor.lookahead try_shadow_deep t -> read_combined ()
  | _ ->
      if Cursor.peek_comma t || Cursor.is_done t then left
      else if can_start_selector () then (
        check_combinator Descendant;
        combine left Descendant (read_complex t))
      else left

(* CSS Selectors 4 section 3.9: the top-level rule selector list is an
   unforgiving site. [read_compound] keeps [Unknown_pseudo_class] so vendor and
   forward-compat pseudos round-trip, but one at top level is a spec deviation;
   raise so [Selector.of_string ".ok,:future-pseudo"] surfaces a
   [Parse_error]. *)
let validate_unforgiving_pseudo t sel =
  if has_unknown_pseudo_class sel then
    Cursor.err t "unknown pseudo-class in unforgiving selector list"

let read_selector_list t =
  Cursor.with_context t "list" @@ fun () ->
  Cursor.ws t;
  (* Parse the selector list manually to properly handle trailing commas *)
  let rec collect_list acc =
    let sel = read_complex t in
    let acc = sel :: acc in
    Cursor.ws t;
    if Cursor.comma_opt t then (
      Cursor.ws t;
      (* After a comma, we must have another selector - trailing commas are
         invalid *)
      collect_list acc)
    else List.rev acc
  in
  let selectors = collect_list [] in
  let result =
    match selectors with [ s ] -> s | selectors -> List selectors
  in
  validate_unforgiving_pseudo t result;
  result

let read_strict_selector_list t =
  Cursor.with_context t "list" @@ fun () ->
  let result =
    match read_complex_list t with [ s ] -> s | selectors -> List selectors
  in
  validate_unforgiving_pseudo t result;
  result

let read t =
  let selector = read_selector_list t in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "unexpected characters after selector";
  selector

let read_relative t =
  let selectors = read_relative_selector_list t in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "unexpected characters after selector";
  let result = match selectors with [ s ] -> s | _ -> List selectors in
  (* A nested style rule's prelude is a selector list like any other, so it is
     unforgiving too. *)
  validate_unforgiving_pseudo t result;
  result

(* CSS Nesting 1 sec. 3: a nested selector is implicitly relative to [&], so a
   leading [& <combinator>] is redundant: [& .bar] -> [.bar], [& > .bar] -> [>
   .bar]. Only a leading [&] that is the whole left operand of a combinator is
   removed; [&.bar] (compound) and a deeper [&] stay. *)
let rec drop_redundant_nesting_prefix (sel : t) : t =
  match sel with
  | Combined (Nesting, Descendant, right) -> right
  | Combined (Nesting, comb, right) -> Relative (comb, right)
  | List sels -> List (List.map drop_redundant_nesting_prefix sels)
  | other -> other

(* CSS Syntax 3 (ED) sec. 4.3.11 builds an identifier from ident code points -
   letters, digits, [-], [_] and anything non-ASCII - and escapes. The shortcut
   below reads the whole string as one name, so anything else in it means the
   string is not a name: selector punctuation ([:], [>], [,] ...) but equally a
   [}] or a [;], which name nothing and used to be taken for an element. *)
let is_ident_code_point c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
  | c -> Char.code c >= 0x80

let is_unescaped_selector_syntax s start =
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else
      match s.[i] with
      | '\\' ->
          let j = ref i in
          skip_css_escape s j;
          loop !j
      | c when is_ident_code_point c -> loop (i + 1)
      | _ -> true
  in
  loop start

let has_invalid_css_escape s =
  let len = String.length s in
  let rec loop i =
    if i >= len then false
    else if s.[i] <> '\\' then loop (i + 1)
    else if i + 1 >= len then true
    else
      match s.[i + 1] with
      | '\n' | '\r' | '\012' -> true
      | _ ->
          let j = ref i in
          skip_css_escape s j;
          loop !j
  in
  loop 0

let can_fallback_shortcut s =
  let len = String.length s in
  len > 0
  && (not (has_invalid_css_escape s))
  &&
  match s.[0] with
  | '.' | '#' -> len > 1 && not (is_unescaped_selector_syntax s 1)
  | _ -> not (is_unescaped_selector_syntax s 0)

(* Use the full selector parser; fall back to the single-token shortcut for
   ['.foo' / '#foo' / 'foo'] when the cursor parser would reject the input. *)
let of_string_fallback s =
  match s.[0] with
  | '.' ->
      class_ (unescape_selector_name (String.sub s 1 (String.length s - 1)))
  | '#' -> id (unescape_selector_name (String.sub s 1 (String.length s - 1)))
  | _ -> Element (None, unescape_selector_name s)

let of_string s =
  Error.catch @@ fun () ->
  try read (Cursor.of_string s)
  with Cursor.Parse_error _ as exn ->
    if not (can_fallback_shortcut s) then raise exn else of_string_fallback s

let v s =
  match of_string s with
  | Ok sel -> sel
  | Error e ->
      invalid_arg (String.concat "" [ "Selector.v: "; Error.to_string e ])
