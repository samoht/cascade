(** CSS Selectors - types and pretty printing *)

open Syntax
include Selector_intf

let pp_component_values = Values.pp_component_values
let read_component_values = Values.read_component_values

(** Helper function for invalid identifiers *)
let err_invalid_identifier name reason =
  invalid_arg (String.concat "" [ "CSS identifier '"; name; "' "; reason ])

(* CSS identifier validation functions *)
let is_valid_nmstart c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || c = '_'
  || Char.code c > 127
  || c = '\\'

let is_valid_nmchar c = is_valid_nmstart c || (c >= '0' && c <= '9') || c = '-'

let pp_attr_flag ctx = function
  | Some Insensitive ->
      Pp.char ctx ' ';
      Pp.char ctx 'i'
  | Some Sensitive ->
      Pp.char ctx ' ';
      Pp.char ctx 's'
  | None -> ()

(* CSS Selectors 4 sec. 6.2 accepts an [<ident-token>] or [<string-token>] as an
   attribute match value. Quotes can drop only when this text is a CSS ident. *)
let attr_value_needs_quoting value =
  if value = "" then true
  else
    let first = value.[0] in
    (* Numbers need quoting; double-dash identifiers are valid CSS idents. *)
    if first >= '0' && first <= '9' then true
    else
      (* Must quote if contains non-identifier characters *)
      not
        (String.for_all
           (function
             | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
             | c when Char.code c >= 0xA0 ->
                 true (* Unicode chars U+00A0 and higher *)
             | _ -> false)
           value)

(* Helper to pretty-print attribute values with smart quoting. *)
let pp_quoted_attr_value quote ctx value =
  Pp.char ctx quote;
  String.iter
    (fun c ->
      if c = quote then (
        Pp.char ctx '\\';
        Pp.char ctx c)
      else Pp.char ctx c)
    value;
  Pp.char ctx quote

let pp_attr_value ?quote ctx value =
  (* Under minify, drop the surrounding quotes when the value is a CSS ident
     since CSS Selectors 4 sec. 6.2 accepts the ident and string forms at the
     same grammar position and the bare form is shorter. The non-minified path
     keeps the quotes for source fidelity - the user who wrote [type="text"]
     expects to read [type="text"] back, even if [type=text] would parse the
     same way. *)
  if String.contains value '\\' then Pp.string ctx value
  else if Pp.minified ctx && not (attr_value_needs_quoting value) then
    Pp.string ctx value
  else
    match quote with
    | Some quote when not (Pp.minified ctx) ->
        pp_quoted_attr_value quote ctx value
    | _ -> Pp.quoted_string ctx value

(* Helper to print a token with pretty spacing when not minifying. *)
let pp_token : string Pp.t =
 fun ctx token ->
  if Pp.minified ctx then Pp.string ctx token
  else (
    Pp.space ctx ();
    Pp.string ctx token;
    Pp.space ctx ())

let pp_attribute_match : attribute_match Pp.t =
 fun ctx -> function
  | Presence -> ()
  | Exact value ->
      Pp.char ctx '=';
      pp_attr_value ctx value
  | Exact_quoted (value, quote) ->
      Pp.char ctx '=';
      pp_attr_value ~quote ctx value
  | Whitespace_list value ->
      Pp.string ctx "~=";
      pp_attr_value ctx value
  | Whitespace_list_quoted (value, quote) ->
      Pp.string ctx "~=";
      pp_attr_value ~quote ctx value
  | Hyphen_list value ->
      Pp.string ctx "|=";
      pp_attr_value ctx value
  | Hyphen_list_quoted (value, quote) ->
      Pp.string ctx "|=";
      pp_attr_value ~quote ctx value
  | Prefix value ->
      Pp.string ctx "^=";
      pp_attr_value ctx value
  | Prefix_quoted (value, quote) ->
      Pp.string ctx "^=";
      pp_attr_value ~quote ctx value
  | Suffix value ->
      Pp.string ctx "$=";
      pp_attr_value ctx value
  | Suffix_quoted (value, quote) ->
      Pp.string ctx "$=";
      pp_attr_value ~quote ctx value
  | Substring value ->
      Pp.string ctx "*=";
      pp_attr_value ctx value
  | Substring_quoted (value, quote) ->
      Pp.string ctx "*=";
      pp_attr_value ~quote ctx value

let skip_css_escape name i =
  (* Skip escaped sequence: either next char or up to 6 hex digits + optional
     space *)
  let len = String.length name in
  incr i;
  if !i >= len then ()
  else
    let start = !i in
    let rec consume_hex n =
      if n = 6 || !i >= len then ()
      else if is_hex name.[!i] then (
        incr i;
        consume_hex (n + 1))
    in
    consume_hex 0;
    if !i = start then incr i (* single escaped char *)
    else if !i < len && name.[!i] = ' ' then incr i

let validate_identifier_start name =
  if String.length name = 0 then err_invalid_identifier name "cannot be empty";
  let first_char = name.[0] in
  if first_char >= '0' && first_char <= '9' then
    err_invalid_identifier name "cannot start with digit";
  if String.length name >= 2 then (
    if Custom_property_name.has_prefix name then
      err_invalid_identifier name
        "cannot start with '--' (reserved for custom properties)";
    if name.[0] = '-' && name.[1] >= '0' && name.[1] <= '9' then
      err_invalid_identifier name "cannot start with '-' followed by digit")

let identifier_char_valid idx c =
  if idx = 0 then is_valid_nmstart c || c = '-' else is_valid_nmchar c

let invalid_identifier_char_message c idx =
  String.concat ""
    [
      "contains invalid character '";
      String.make 1 c;
      "' at position ";
      Int.to_string idx;
    ]

let validate_css_identifier name =
  validate_identifier_start name;
  let len = String.length name in
  let i = ref 0 in
  while !i < len do
    let c = name.[!i] in
    if c = '\\' then skip_css_escape name i
    else
      let idx = !i in
      if (not (identifier_char_valid idx c)) && Char.code c <= 127 then
        err_invalid_identifier name (invalid_identifier_char_message c idx);
      incr i
  done

let element ?ns name =
  validate_css_identifier name;
  Element (ns, name)

(* Looser validation for class names: allow Tailwind-style tokens and escape at
   print time. Only reject control/unprintable characters and double-dash
   prefix. *)
let validate_serializable_class name =
  if String.length name = 0 then err_invalid_identifier name "cannot be empty";
  if Custom_property_name.has_prefix name then
    err_invalid_identifier name
      "cannot start with '--' (reserved for custom properties)";
  String.iter
    (fun c ->
      let code = Char.code c in
      if code < 0x20 || code = 0x7F then
        err_invalid_identifier name "contains control character")
    name

let class_ name =
  validate_serializable_class name;
  Class name

let id name =
  validate_serializable_class name;
  Id name

let universal = Universal None
let universal_ns ns = Universal (Some ns)

(** Parse an ARIA attribute name into its structured type *)
let aria_attr_of_string : string -> aria_attr = Aria.of_string

(** Categorize an attribute name into its structured type *)
let attr_name_of_string name =
  let len = String.length name in
  if len > 5 && String.sub name 0 5 = "aria-" then
    match aria_attr_of_string name with
    | attr -> Aria attr
    | exception Invalid_argument _ -> Regular name
  else if len > 5 && String.sub name 0 5 = "data-" then
    Data (String.sub name 5 (len - 5))
  else Regular name

(** Convert attr_name back to string for printing *)
let string_of_aria_attr : aria_attr -> string = Aria.to_string

let string_of_attr_name = function
  | Aria a -> string_of_aria_attr a
  | Data s -> "data-" ^ s
  | Regular s -> s

let pp_aria_attr : aria_attr Pp.t =
 fun ctx a -> Pp.string ctx (string_of_aria_attr a)

let read_aria_attr t : aria_attr =
  let s = Cursor.ident t in
  match aria_attr_of_string s with
  | v -> v
  | exception Invalid_argument msg -> Cursor.err t msg

let pp_attr_name : attr_name Pp.t =
 fun ctx a -> Pp.string ctx (string_of_attr_name a)

let read_attr_name t : attr_name =
  let s = Cursor.ident t in
  attr_name_of_string s

let attribute ?ns ?flag name match_type =
  validate_css_identifier name;
  let attr_name = attr_name_of_string name in
  Attribute (ns, attr_name, match_type, flag)

(* Convenience: build a class selector from a raw class token. Escaping happens
   in [pp]/[to_string]. Equivalent to [class_]. *)
(* Convert a hex digit character to its integer value *)
let int_of_hex c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> invalid_arg "not a hex digit"

(* Unescape CSS escapes per CSS Syntax 3 sec. 4.3.7: \XX...XX (1-6 hex) or \X
   (any char). Handles both hex escapes (e.g., \3A for ':') and simple escapes
   (e.g., \:). *)
(* Helper to process hex escape sequences. Returns (codepoint, next_index) *)
let process_hex_escape s i len =
  let rec consume_hex acc n idx =
    if n = 6 || idx >= len || not (is_hex s.[idx]) then (acc, idx)
    else consume_hex ((acc * 16) + int_of_hex s.[idx]) (n + 1) (idx + 1)
  in
  let codepoint, next_idx = consume_hex 0 0 (i + 1) in
  (* Skip optional whitespace after hex escape *)
  let final_idx =
    if next_idx < len && s.[next_idx] = ' ' then next_idx + 1 else next_idx
  in
  (codepoint, final_idx)

let unescape_selector_name s =
  let len = String.length s in
  let buf = Buffer.create len in
  let process_escape i =
    if i + 1 >= len then
      (* Trailing backslash - ignore *)
      len
    else if is_hex s.[i + 1] then (
      let codepoint, final_idx = process_hex_escape s i len in
      (* CSS Syntax 3 (ED) sec. 4.3.7: U+0000, surrogates, and out-of-range code
         points are replaced with U+FFFD rather than passed through. *)
      let cp =
        if
          codepoint <= 0 || codepoint > 0x10FFFF
          || (codepoint >= 0xD800 && codepoint <= 0xDFFF)
        then 0xFFFD
        else codepoint
      in
      Buffer.add_utf_8_uchar buf (Uchar.of_int cp);
      final_idx)
    else (
      (* Simple escape: just take the next character literally *)
      Buffer.add_char buf s.[i + 1];
      i + 2)
  in
  let rec loop i =
    if i >= len then ()
    else if s.[i] <> '\\' then (
      Buffer.add_char buf s.[i];
      loop (i + 1))
    else loop (process_escape i)
  in
  loop 0;
  Buffer.contents buf

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

let rec combine s1 comb s2 =
  match s2 with
  | List selectors ->
      (* When combining with a List, distribute the combinator over each
         element *)
      List (List.map (combine s1 comb) selectors)
  | _ ->
      (* For all other cases, create a Combined node *)
      Combined (s1, comb, s2)

let ( ++ ) s1 s2 = combine s1 Descendant s2
let ( >> ) s1 s2 = combine s1 Child s2
let where selectors = Where selectors

let list selectors =
  match selectors with
  | [] -> invalid_arg "CSS selector list cannot be empty"
  | _ -> List selectors

let unescape_attribute_value = unescape_selector_name
let is_compound_list = function List _ -> true | _ -> false
let as_list = function List sels -> Some sels | _ -> None
let compound selectors = Compound selectors
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

let format_attribute_number n (unit : string option) =
  match unit with
  | None ->
      if Float.is_integer n then string_of_int (int_of_float n)
      else string_of_float n
  | Some u ->
      if Float.is_integer n then string_of_int (int_of_float n) ^ u
      else string_of_float n ^ u

let read_attribute_value_unquoted t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Ident s; loc }) ->
      read_attribute_value_ident t s loc
  | Some (Component.Preserved { kind = Token.Number_tok _; _ })
  | Some (Component.Preserved { kind = Token.Dimension _; _ })
  | Some (Component.Preserved { kind = Token.Percentage _; _ }) ->
      let n, unit = Cursor.number_with_unit t in
      format_attribute_number n unit
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

(** Parse a class selector (.classname) *)
let read_class t =
  Cursor.expect '.' t;
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
  | Some (Component.Preserved { kind = Token.Whitespace; _ }) ->
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

(** Pretty print nth expression *)
let pp_nth : nth Pp.t =
 fun ctx -> function
  (* Source-shape preserving: the parser keeps [Odd]/[Even] for the keyword
     spellings and [An_plus_b (2, 1)] / [An_plus_b (2, 0)] for the explicit An+B
     forms, so the printer just emits whichever the author actually wrote in
     pretty mode. Under minify, CSS Selectors 4 14 makes [2n+1]/[odd] and
     [2n]/[even] spec-equivalent; pick the shorter spelling. *)
  | An_plus_b (2, 1) when Pp.minified ctx -> Pp.string ctx "odd"
  | Even when Pp.minified ctx -> Pp.string ctx "2n"
  | Odd -> Pp.string ctx "odd"
  | Even -> Pp.string ctx "even"
  | Index n -> Pp.int ctx n
  | An_plus_b (a, b) ->
      if a = 0 then Pp.int ctx b
      else (
        (* Print coefficient *)
        if a = 1 then Pp.string ctx "n"
        else if a = -1 then Pp.string ctx "-n"
        else (
          Pp.int ctx a;
          Pp.char ctx 'n');

        (* Print offset *)
        if b > 0 then (
          Pp.char ctx '+';
          Pp.int ctx b)
        else if b < 0 then Pp.int ctx b (* b = 0: print nothing *))

(* Pseudo-class and pseudo-element identifier mappings *)
let pseudo_class_base_idents =
  [
    (* Interactive *)
    ("hover", Hover);
    ("active", Active);
    ("focus", Focus);
    ("focus-visible", Focus_visible);
    ("focus-within", Focus_within);
    ("target", Target);
    ("target-within", Target_within);
    (* Link *)
    ("link", Link);
    ("visited", Visited);
    ("any-link", Any_link);
    ("local-link", Local_link);
    (* Structural *)
    ("root", Root);
    ("empty", Empty);
    ("first-child", First_child);
    ("last-child", Last_child);
    ("only-child", Only_child);
    ("first-of-type", First_of_type);
    ("last-of-type", Last_of_type);
    ("only-of-type", Only_of_type);
    (* Input *)
    ("enabled", Enabled);
    ("disabled", Disabled);
    ("read-only", Read_only);
    ("read-write", Read_write);
    ("placeholder-shown", Placeholder_shown);
    ("default", Default);
    ("checked", Checked);
    ("indeterminate", Indeterminate);
    ("blank", Blank);
    ("valid", Valid);
    ("invalid", Invalid);
    ("in-range", In_range);
    ("out-of-range", Out_of_range);
    ("required", Required);
    ("optional", Optional);
    ("user-invalid", User_invalid);
    ("user-valid", User_valid);
    ("inert", Inert);
    ("autofill", Autofill);
    (* Display *)
    ("fullscreen", Fullscreen);
    ("modal", Modal);
    ("picture-in-picture", Picture_in_picture);
    ("popover-open", Popover_open);
    ("open", Open);
    (* CSS Modules scope keywords - non-standard but emitted by tooling *)
    ("local", Local_scope);
    ("global", Global_scope);
    (* Paged *)
    ("left", Left);
    ("right", Right);
    ("first", First);
    (* Component *)
    ("defined", Defined);
    ("scope", Scope);
    ("host", Host None);
    (* Media *)
    ("playing", Playing);
    ("paused", Paused);
    ("seeking", Seeking);
    ("buffering", Buffering);
    ("stalled", Stalled);
    ("muted", Muted);
    ("volume-locked", Volume_locked);
    ("current", Current);
    ("past", Past);
    ("future", Future);
    (* View transitions *)
    ("active-view-transition", Active_view_transition);
  ]

let scrollbar_state_ident = function
  | Horizontal -> "horizontal"
  | Vertical -> "vertical"
  | Decrement -> "decrement"
  | Increment -> "increment"
  | Start -> "start"
  | End -> "end"
  | Double_button -> "double-button"
  | Single_button -> "single-button"
  | No_button -> "no-button"
  | Corner_present -> "corner-present"
  | Window_inactive -> "window-inactive"

let scrollbar_part_ident = function
  | Scrollbar -> "-webkit-scrollbar"
  | Button -> "-webkit-scrollbar-button"
  | Track -> "-webkit-scrollbar-track"
  | Track_piece -> "-webkit-scrollbar-track-piece"
  | Thumb -> "-webkit-scrollbar-thumb"
  | Corner -> "-webkit-scrollbar-corner"
  | Resizer -> "-webkit-resizer"

(* WebKit, "Styling Scrollbars". Both engines read these on any element, so they
   are ordinary pseudo-classes here; which pseudo-elements take them is
   [pseudo_element_allows]'s business. *)
let pseudo_class_scrollbar_idents =
  List.map
    (fun state -> (scrollbar_state_ident state, Scrollbar_state state))
    [
      Horizontal;
      Vertical;
      Decrement;
      Increment;
      Start;
      End;
      Double_button;
      Single_button;
      No_button;
      Corner_present;
      Window_inactive;
    ]

let pseudo_element_legacy_idents form =
  [
    (* Legacy pseudo-elements: parser records [Single] or [Double] colon for
       tests and compatibility, but the printer canonicalizes pretty output to
       the modern double-colon spelling. *)
    ("before", Before form);
    ("after", After form);
    ("first-letter", First_letter form);
    ("first-line", First_line form);
  ]

let pseudo_element_modern_idents =
  [
    (* Modern pseudo-elements *)
    ("backdrop", Backdrop);
    ("marker", Marker);
    ("placeholder", Placeholder);
    ("selection", Selection);
    ("target-text", Target_text);
    ("spelling-error", Spelling_error);
    ("grammar-error", Grammar_error);
    ("file-selector-button", File_selector_button);
    ("view-transition", View_transition);
  ]

let pseudo_vendor_idents =
  [
    (* Vendor-specific *)
    ("-moz-focusring", Moz_focusring);
    ("-webkit-any", Webkit_any);
    ("-webkit-autofill", Webkit_autofill);
    ("-moz-placeholder", Moz_placeholder);
    ("-webkit-input-placeholder", Webkit_input_placeholder);
    ("-ms-input-placeholder", Ms_input_placeholder);
    ("-moz-ui-invalid", Moz_ui_invalid);
    ("-moz-ui-valid", Moz_ui_valid);
    ("-webkit-scrollbar", Webkit_scrollbar Scrollbar);
    ("-webkit-search-cancel-button", Webkit_search_cancel_button);
    ("-webkit-search-decoration", Webkit_search_decoration);
    (* Webkit datetime pseudo-elements *)
    ("-webkit-datetime-edit-fields-wrapper", Webkit_datetime_edit_fields_wrapper);
    ("-webkit-date-and-time-value", Webkit_date_and_time_value);
    ("-webkit-datetime-edit", Webkit_datetime_edit);
    ("-webkit-datetime-edit-year-field", Webkit_datetime_edit_year_field);
    ("-webkit-datetime-edit-month-field", Webkit_datetime_edit_month_field);
    ("-webkit-datetime-edit-day-field", Webkit_datetime_edit_day_field);
    ("-webkit-datetime-edit-hour-field", Webkit_datetime_edit_hour_field);
    ("-webkit-datetime-edit-minute-field", Webkit_datetime_edit_minute_field);
    ("-webkit-datetime-edit-second-field", Webkit_datetime_edit_second_field);
    ( "-webkit-datetime-edit-millisecond-field",
      Webkit_datetime_edit_millisecond_field );
    ("-webkit-datetime-edit-meridiem-field", Webkit_datetime_edit_meridiem_field);
    ("-webkit-inner-spin-button", Webkit_inner_spin_button);
    ("-webkit-outer-spin-button", Webkit_outer_spin_button);
    ("-webkit-calendar-picker-indicator", Webkit_calendar_picker_indicator);
    ("-webkit-details-marker", Webkit_details_marker);
    ("details-content", Details_content);
  ]

(* Names the [::] reader takes and the [:] reader does not. Chrome 151 and
   WebKit 26.5 drop [:cue] and [:-webkit-scrollbar-thumb] while keeping the [::]
   spelling of both, so these stay out of [pseudo_class_all_idents];
   [::-webkit-scrollbar] itself keeps the single-colon spelling cascade has
   always read for it and stays in [pseudo_vendor_idents].

   WebVTT 1 sec. 8.2 requires "the ::cue, ::cue(selector), ::cue-region and
   ::cue-region(selector) pseudo-elements", and sec. 8.2.1 and sec. 8.2.3 define
   the argument-less pair the [calls] table above cannot reach. Chrome, WebKit
   and Lightning CSS all take [::cue]; only Lightning CSS takes [::cue-region],
   the other two implement neither of its forms. *)
let pseudo_element_double_colon_idents =
  ("cue", Cue None)
  :: ("cue-region", Cue_region None)
  :: List.map
       (fun part -> (scrollbar_part_ident part, Webkit_scrollbar part))
       [ Button; Track; Track_piece; Thumb; Corner; Resizer ]

(* Every [::] form, and only those: a pseudo-element names a box other than the
   originating element, where an element-tied pseudo-class ([:hover], [:focus])
   only narrows which elements match. An unrecognised [::foo] belongs here too.
   Selectors 4 sec. 16 builds a pseudo-compound out of
   [<pseudo-element-selector> <pseudo-class-selector>*] whatever the
   pseudo-element's name is, so the shape rules that read this predicate cannot
   wait for cascade to learn the name; the framework-only [::deep] / [::v-deep]
   / [::ng-deep] are unknown [::] names like any other, since Selectors 4 has no
   piercing combinator. *)
let is_pseudo_element = function
  | Before _ | After _ | First_letter _ | First_line _ | Backdrop | Marker
  | Placeholder | Selection | Target_text | Spelling_error | Grammar_error
  | File_selector_button | Moz_placeholder | Webkit_input_placeholder
  | Ms_input_placeholder | Webkit_scrollbar _ | Webkit_search_cancel_button
  | Webkit_search_decoration | Webkit_datetime_edit_fields_wrapper
  | Webkit_date_and_time_value | Webkit_datetime_edit
  | Webkit_datetime_edit_year_field | Webkit_datetime_edit_month_field
  | Webkit_datetime_edit_day_field | Webkit_datetime_edit_hour_field
  | Webkit_datetime_edit_minute_field | Webkit_datetime_edit_second_field
  | Webkit_datetime_edit_millisecond_field | Webkit_datetime_edit_meridiem_field
  | Webkit_inner_spin_button | Webkit_outer_spin_button
  | Webkit_calendar_picker_indicator | Webkit_details_marker | Details_content
  | Part _ | Slotted _ | Cue _ | Cue_region _ | Highlight _ | View_transition
  | View_transition_group _ | View_transition_image_pair _
  | View_transition_old _ | View_transition_new _ | Unknown_pseudo_element _
  | Unknown_pseudo_element_call _ ->
      true
  | _ -> false

let rec any p = function
  | Compound xs as sel -> List.exists (any p) xs || p sel
  | Combined (a, _, b) as sel -> any p a || any p b || p sel
  | Relative (_, b) as sel -> any p b || p sel
  | List xs as sel -> List.exists (any p) xs || p sel
  | ( Is xs
    | Where xs
    | Not xs
    | Has xs
    | Moz_any_call xs
    | Webkit_any_call xs
    | Slotted xs
    | Current_of xs ) as sel ->
      List.exists (any p) xs || p sel
  | ( Nth_child (_, Some xs)
    | Nth_last_child (_, Some xs)
    | Nth_of_type (_, Some xs)
    | Nth_last_of_type (_, Some xs)
    | Host (Some xs)
    | Cue (Some xs)
    | Cue_region (Some xs)
    | Host_context xs ) as sel ->
      List.exists (any p) xs || p sel
  | Part _ as sel -> p sel
  | s -> p s

let has_pseudo_element sel = any is_pseudo_element sel

let has_unknown_pseudo_class =
  any (function
    | Unknown_pseudo_class _ | Unknown_pseudo_class_call _ -> true
    | _ -> false)

(* CSS Selectors 4 17.1: a forgiving [:is()] / [:where()] with no surviving
   valid argument matches nothing, and a compound or combined selector that
   contains such a sub-selector inherits the same behaviour. A selector list
   matches nothing only when every entry does. Useful for dropping dead rules
   under [Optimize.stylesheet]. *)
let rec matches_nothing = function
  | Is [] | Where [] -> true
  | Compound xs -> List.exists matches_nothing xs
  | Combined (a, _, b) -> matches_nothing a || matches_nothing b
  | Relative (_, b) -> matches_nothing b
  | List [] -> true
  | List xs -> List.for_all matches_nothing xs
  | _ -> false

(* CSS Selectors 4 sec. 3.6.3: what may follow a pseudo-element in its compound
   is a pseudo-class, and sec. 3.6.4 adds a sub-pseudo-element
   ([pseudo_element_allows_sub]). A class, id, type or attribute selector after
   the pseudo-element makes the compound invalid whatever the pseudo-element
   is. *)
let is_pe_action = function
  | Element _ | Class _ | Id _ | Universal _ | Attribute _ | Nesting -> false
  | sel -> not (is_pseudo_element sel)

(* CSS Selectors 4 sec. 9. *)
let is_user_action_pseudo_class = function
  | Hover | Active | Focus | Focus_visible | Focus_within -> true
  | _ -> false

(* CSS Selectors 4 sec. 13: the tree-structural pseudo-classes, the ones that
   answer a question about the element's place among its siblings. *)
let is_structural_pseudo_class = function
  | Root | Empty | First_child | Last_child | Only_child | First_of_type
  | Last_of_type | Only_of_type | Nth_child _ | Nth_last_child _ | Nth_of_type _
  | Nth_last_of_type _ ->
      true
  | _ -> false

(* Which pseudo-classes each pseudo-element takes after it. CSS Selectors 4 sec.
   3.6.3 allows the logical combinations and hands the rest of the list to
   "other specifications", so the rows below come from CSS Pseudo-Elements 4
   sec. 5 for the element-backed pseudo-elements and, for the UA widgets whose
   list only exists in the engines, from Chrome and WebKit, taking a
   pseudo-class as allowed when either engine keeps the rule. *)
let rec pseudo_element_allows pe pc =
  match pe with
  (* A name no engine recognises: cascade keeps it so a pseudo-element newer
     than this list survives a format pass, and knows nothing about its rules,
     so it keeps taking any pseudo-class after it. [::cue-region()] keeps the
     same bargain: Chrome 151 and WebKit 26.5 implement neither of its forms. *)
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ | Moz_placeholder
  | Ms_input_placeholder
  | Cue_region (Some _) ->
      true
  | pe -> (
      match pc with
      (* Sec. 3.6.3 allows the logical combinations after every pseudo-element
         and passes the row below on to their arguments; what an argument the
         row refuses costs is the argument list's own business. [:is()] and
         [:where()] take a [<forgiving-selector-list>] (sec. 4.1), which drops
         it and leaves a selector that still parses and matches nothing, so both
         engines keep the rule; the [:-moz-any()] / [:-webkit-any()] aliases
         read back the same way. *)
      | Is _ | Where _ | Moz_any_call _ | Webkit_any_call _ -> true
      (* [:not()] takes an unforgiving list, so the refused argument takes the
         compound down with it: both engines drop [::before:not(:hover)] and
         keep [::part(p):not(:hover)]. *)
      | Not args -> List.for_all (pseudo_element_allows_argument pe) args
      (* WebKit, "Styling Scrollbars": a scrollbar part reports its own state,
         and [:window-inactive] reaches past the scrollbar to a selection and to
         a shadow part, where both engines take it. Past that they disagree one
         cell each way (only WebKit takes the other ten after [::part()], only
         Chrome takes [:window-inactive] after [::details-content]), so the list
         stops where they agree. *)
      | Scrollbar_state state -> (
          match (pe, state) with
          | Webkit_scrollbar _, _ -> true
          | (Selection | Part _), Window_inactive -> true
          | _ -> false)
      (* Same forward-compatibility bargain as an unknown pseudo-element. *)
      | Unknown_pseudo_class _ | Unknown_pseudo_class_call _ -> true
      | pc -> (
          match pe with
          (* CSS Pseudo-Elements 4 sec. 5: an element-backed pseudo-element
             takes what a real element takes, bar the pseudo-classes that would
             report on the tree it sits in. *)
          | Part _ | Details_content -> (
              match pc with
              | Has _ -> false
              | pc -> not (is_structural_pseudo_class pc))
          (* A scrollbar part takes no focus, and reports which part of which
             scrollbar it is through the state pseudo-classes below. *)
          | Webkit_scrollbar _ -> (
              match pc with
              | Hover | Active | Enabled | Disabled -> true
              | _ -> false)
          (* CSS View Transitions 1 sec. 3.1: [:only-child] matches a view
             transition pseudo with no sibling in the pseudo-element tree. *)
          | View_transition_group _ | View_transition_image_pair _
          | View_transition_old _ | View_transition_new _ -> (
              match pc with Only_child -> true | _ -> false)
          (* The UA widgets that stand in for a real control, and the cue and
             region boxes WebVTT 1 sec. 8.2.1 and sec. 8.2.3 define: all three
             engines keep [::cue:focus-within] and drop [::cue:enabled].
             [::cue(...)] selects inside the cue and takes none of them. *)
          | Cue None
          | Cue_region None
          | Placeholder | File_selector_button | Webkit_input_placeholder
          | Webkit_search_cancel_button | Webkit_search_decoration
          | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
          | Webkit_datetime_edit | Webkit_datetime_edit_year_field
          | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
          | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
          | Webkit_datetime_edit_second_field
          | Webkit_datetime_edit_millisecond_field
          | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
          | Webkit_outer_spin_button | Webkit_calendar_picker_indicator
          | Webkit_details_marker ->
              is_user_action_pseudo_class pc
          | _ -> false))

(* An argument of a logical combination sits where the pseudo-element's own
   pseudo-classes sit, so it reads as a pseudo-compound tail: pseudo-classes the
   pseudo-element takes, and nothing else. A combinator or a selector list makes
   the argument a whole complex selector, which no pseudo-element takes. *)
and pseudo_element_allows_argument pe = function
  | Compound components ->
      List.for_all (pseudo_element_allows_argument pe) components
  | Combined _ | Relative _ | List _ -> false
  | c -> is_pe_action c && pseudo_element_allows pe c

(* CSS Pseudo-Elements 4 sec. 4 names the tree-abiding pseudo-elements, sec. 7.1
   lists [::backdrop] and [::view-transition] among them, and sec. 5 adds that
   "element-backed pseudo-elements are always tree-abiding". [::part()] is
   element-backed and so belongs here on that reading, and is left out because
   sec. 5 also says it never matches after a pseudo-element. *)
let is_tree_abiding_pseudo_element = function
  | Before _ | After _ | Marker | Placeholder | Backdrop | View_transition
  | Details_content | File_selector_button ->
      true
  | _ -> false

(* Which pseudo-elements each pseudo-element takes after it, in the same
   compound. CSS Selectors 4 sec. 3.6.4 refuses the shape "unless the
   corresponding sub-pseudo-element is explicitly defined to exist in another
   specification", so the rows below are those definitions and nothing else:
   [::before::before] goes, [::before::marker] stays.

   CSS Pseudo-Elements 4 sec. 4.2 defines the [::marker] of a [::before] or
   [::after] that is a list item, and rules [::marker::marker] out. Sec. 5 lets
   an element-backed pseudo-element take every pseudo-element "just as if the
   pseudo-element were a type selector", bar [::part()], which "never matches"
   there; cascade turns that never-matching row into a refusal, as it does one
   row up for [:has()] and the structural pseudo-classes. Sec. 5.1 files
   [::file-selector-button] as element-backed, but Chrome 151, WebKit 27 and
   Lightning CSS all drop [::file-selector-button::before], so the originating
   row keeps the [::part()] / [::details-content] pair [pseudo_element_allows]
   carries.

   CSS Shadow 1 sec. 3.2.4 gives [::slotted()] a narrower row of its own: "the
   ::slotted() pseudo-element can be followed by a tree-abiding pseudo-element".
   The engines show the difference, dropping [::slotted(a)::first-line] while
   keeping [::part(x)::first-line].

   One adjacent pair at a time, so a longer chain is as valid as each of its
   links: Chrome keeps [::part(x)::before::marker] and drops
   [::part(x)::marker::before].

   An unmodelled [::] name keeps the bargain [pseudo_element_allows] makes for
   it: cascade preserves the name rather than judging what may follow it. *)
let pseudo_element_allows_sub pe sub =
  match pe with
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ -> true
  | Part _ | Details_content -> ( match sub with Part _ -> false | _ -> true)
  | Slotted _ -> is_tree_abiding_pseudo_element sub
  | Before _ | After _ -> ( match sub with Marker -> true | _ -> false)
  | _ -> false

(* CSS Selectors 4 sec. 3.6.5: a pseudo-element defined to have internal
   structure may be followed by a child or descendant combinator, and a selector
   with a combinator after the pseudo-element is invalid otherwise. Nothing
   shipping claims that structure, so the exception stays empty: Chrome 151 and
   WebKit 26.5 drop [::part(x) > .b], [::details-content > div] and
   [::first-letter em] alike, and the Servo selectors crate raises
   UnexpectedSelectorAfterPseudoElement for the same shapes.

   A [::] name cascade does not model is exempt, for the reason
   [pseudo_element_allows] already exempts it from sec. 3.6.3: cascade preserves
   such a name rather than judging it. test/interop/lightning carries seven
   rules of that kind ([.foo ::deep .bar], [.foo ::unknown(.foo) .bar] and their
   kin) and all six reference minifiers keep every one verbatim, while Lightning
   CSS rejects the identical shape as soon as the name is one it knows
   ([::-moz-placeholder .b]). Dropping the exemption would delete those
   rules. *)
let is_modelled_pseudo_element = function
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _ -> false
  | sel -> is_pseudo_element sel

(* Only the compound's own components. A pseudo-element inside a functional
   pseudo-class argument belongs to that argument's own rules, so
   [.a:has(.b::before) .c] is not this one's to judge. *)
let bars_following_combinator = function
  | Compound components -> List.exists is_modelled_pseudo_element components
  | sel -> is_modelled_pseudo_element sel

(* The same sec. 3.6.5 rule read off a built selector rather than applied while
   reading one. Nesting reaches a selector the reader would refuse, because it
   joins a valid parent to a valid child without either passing the check. The
   two tooling combinators are exempt for the reason [check_combinator] exempts
   them: no engine parses them, so the rule never reaches them. *)
let rec has_combinator_after_pseudo_element = function
  | List branches -> List.exists has_combinator_after_pseudo_element branches
  | Combined (left, comb, right) ->
      (match comb with
        | Shadow_piercing | Shadow_deep -> false
        | Descendant | Child | Next_sibling | Subsequent_sibling | Column ->
            bars_following_combinator left)
      || has_combinator_after_pseudo_element left
      || has_combinator_after_pseudo_element right
  | _ -> false

(* Why a compound may not carry [s] after the pseudo-element it follows.
   [read_compound] turns each into its own message and [has_refused_simple_-
   after_pseudo_element] reads the same answer off a built selector, so the two
   cannot drift. [seen] is the compound's earlier components, most recent
   first. *)
type compound_refusal =
  | Sub_pseudo_element  (** sec. 3.6.4, via [pseudo_element_allows_sub]. *)
  | Simple_after_pseudo_element  (** sec. 3.6.3, via [is_pe_action]. *)
  | Pseudo_class_after_pseudo_element
      (** sec. 3.6.3, via [pseudo_element_allows]. *)

let compound_refusal ~seen s =
  match List.find_opt is_pseudo_element seen with
  | Some pe when is_pseudo_element s ->
      if pseudo_element_allows_sub pe s then Option.None
      else Option.Some Sub_pseudo_element
  | Some _ when not (is_pe_action s) -> Option.Some Simple_after_pseudo_element
  | Some pe when not (pseudo_element_allows pe s) ->
      Option.Some Pseudo_class_after_pseudo_element
  | _ -> Option.None

(* The sec. 3.6.3 / 3.6.4 companion of [has_combinator_after_pseudo_element]:
   nesting extends a pseudo-element's own compound the same way it puts a
   combinator after one, out of a valid parent and a valid child that neither
   one meets the reader's check. A pseudo-element inside a functional
   pseudo-class argument is that argument's own business, as it is for
   [bars_following_combinator]. *)
(* Substituting [&] splices the parent compound in as one component, so the walk
   flattens a nested [Compound] rather than treating it as a simple selector the
   pseudo-element would refuse outright. *)
let rec refuses_compound seen = function
  | [] -> false
  | Compound inner :: rest -> refuses_compound seen (inner @ rest)
  | c :: rest -> (
      match compound_refusal ~seen c with
      | Some _ -> true
      | None -> refuses_compound (c :: seen) rest)

let rec has_refused_simple_in_compound = function
  | List branches -> List.exists has_refused_simple_in_compound branches
  | Combined (left, _, right) ->
      has_refused_simple_in_compound left
      || has_refused_simple_in_compound right
  | Relative (_, right) -> has_refused_simple_in_compound right
  | Compound components -> refuses_compound [] components
  | _ -> false

(* The merged lists are static across the lifetime of the program (every
   constituent is a [let] binding above); memoise them so the [@] cons-chain
   only happens once instead of per [:foo] / [::foo] pseudo read. *)
let pseudo_class_all_idents_lazy =
  lazy
    (pseudo_class_base_idents @ pseudo_class_scrollbar_idents
    @ pseudo_element_legacy_idents Single
    @ pseudo_element_modern_idents @ pseudo_vendor_idents)

let pseudo_class_all_idents () = Lazy.force pseudo_class_all_idents_lazy

let pseudo_element_unknown_idents =
  lazy
    (pseudo_element_modern_idents @ pseudo_vendor_idents
   @ pseudo_element_double_colon_idents
    @ pseudo_element_legacy_idents Double)

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
  | exception Cursor.Parse_error _ -> acc

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
    | Some (Component.Preserved { kind = Token.Whitespace; _ }) -> false
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
  try read (Cursor.of_string s)
  with Cursor.Parse_error _ as exn ->
    if not (can_fallback_shortcut s) then raise exn else of_string_fallback s

(** Pretty print a function-like pseudo-class or pseudo-element *)
let pp_func : 'a. Pp.ctx -> prefix:string -> string -> 'a Pp.t -> 'a -> unit =
 fun ctx ~prefix name content_pp value ->
  Pp.string ctx prefix;
  Pp.string ctx name;
  Pp.char ctx '(';
  content_pp ctx value;
  Pp.char ctx ')'

(** Helper functions for common patterns *)
let pseudo ctx name = Pp.string ctx (":" ^ name)

let elem ctx name = Pp.string ctx ("::" ^ name)
let vendor ctx name = Pp.string ctx (":-" ^ name)
let vendor_elem ctx name = Pp.string ctx ("::-" ^ name)

(* CSS Selectors 4 sec. 3.6.1 keeps [:before] (CSS 2.1) as a deprecated
   compatibility spelling for the four original pseudo-elements. Minified output
   uses the shorter valid alias; pretty output preserves the parsed colon form
   so the authored spelling round-trips. *)
let legacy_elem ctx form name =
  let prefix =
    if Pp.minified ctx then ":"
    else match form with Single -> ":" | Double -> "::"
  in
  Pp.string ctx (prefix ^ name)

let func ctx name pp_content value =
  pp_func ctx ~prefix:":" name pp_content value

let elem_func ctx name pp_content value =
  pp_func ctx ~prefix:"::" name pp_content value

let pp_vt_class_selector ctx (sel : vt_class_selector) =
  (match sel.name with Some n -> Pp.string ctx n | None -> ());
  List.iter
    (fun cls ->
      Pp.char ctx '.';
      Pp.string ctx cls)
    sel.classes

let pp_combinator ctx = function
  | Descendant -> Pp.space ctx ()
  | Child -> pp_token ctx ">"
  | Next_sibling -> pp_token ctx "+"
  | Subsequent_sibling -> pp_token ctx "~"
  | Column -> pp_token ctx "||"
  | Shadow_piercing -> pp_token ctx ">>>"
  | Shadow_deep -> pp_token ctx "/deep/"

let pp_relative_combinator ctx = function
  | Descendant -> Pp.space ctx ()
  | Child ->
      Pp.string ctx ">";
      Pp.space_if_pretty ctx ()
  | Next_sibling ->
      Pp.string ctx "+";
      Pp.space_if_pretty ctx ()
  | Subsequent_sibling ->
      Pp.string ctx "~";
      Pp.space_if_pretty ctx ()
  | Column ->
      Pp.string ctx "||";
      Pp.space_if_pretty ctx ()
  | Shadow_piercing ->
      Pp.string ctx ">>>";
      Pp.space_if_pretty ctx ()
  | Shadow_deep ->
      Pp.string ctx "/deep/";
      Pp.space_if_pretty ctx ()

let strs ctx strings = Pp.list ~sep:Pp.comma Pp.string ctx strings

let lang_range ctx string =
  if attr_value_needs_quoting string then Pp.quoted_string ctx string
  else Pp.string ctx string

let langs ctx strings = Pp.list ~sep:Pp.comma lang_range ctx strings
let hex_digits = "0123456789abcdef"

let add_selector_hex_escape buf code =
  Buffer.add_char buf '\\';
  let rec emit n acc =
    match (n, acc) with
    | 0, [] -> Buffer.add_char buf '0'
    | 0, digits -> List.iter (Buffer.add_char buf) digits
    | _ -> emit (n / 16) (hex_digits.[n mod 16] :: acc)
  in
  emit code [];
  Buffer.add_char buf ' '

let first_needs_hex_escape name =
  match String.length name with
  | 0 -> false
  | len ->
      let first = name.[0] in
      (first >= '0' && first <= '9')
      || (first = '-' && len > 1 && name.[1] >= '0' && name.[1] <= '9')

let add_selector_ascii buf ~first_needs_hex_escape i c =
  if i = 0 && first_needs_hex_escape then
    add_selector_hex_escape buf (Char.code c)
  else
    match c with
    | '\x00' .. '\x1F' | '\x7F' -> add_selector_hex_escape buf (Char.code c)
    | '[' -> Buffer.add_string buf "\\["
    | ']' -> Buffer.add_string buf "\\]"
    | '\\' -> Buffer.add_string buf "\\\\"
    | '(' -> Buffer.add_string buf "\\("
    | ')' -> Buffer.add_string buf "\\)"
    | ',' -> Buffer.add_string buf "\\,"
    | '/' -> Buffer.add_string buf "\\/"
    | ':' -> Buffer.add_string buf "\\:"
    | '%' -> Buffer.add_string buf "\\%"
    | '.' -> Buffer.add_string buf "\\."
    | '#' -> Buffer.add_string buf "\\#"
    | ' ' -> Buffer.add_string buf "\\ "
    | '"' -> Buffer.add_string buf "\\\""
    | '\'' -> Buffer.add_string buf "\\'"
    | '@' -> Buffer.add_string buf "\\@"
    | '*' -> Buffer.add_string buf "\\*"
    | '>' -> Buffer.add_string buf "\\>"
    | '+' -> Buffer.add_string buf "\\+"
    | '~' -> Buffer.add_string buf "\\~"
    | '&' -> Buffer.add_string buf "\\&"
    | '^' -> Buffer.add_string buf "\\^"
    | '$' -> Buffer.add_string buf "\\$"
    | '=' -> Buffer.add_string buf "\\="
    | '!' -> Buffer.add_string buf "\\!"
    | '|' -> Buffer.add_string buf "\\|"
    | c when is_valid_nmchar c -> Buffer.add_char buf c
    | c ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c

let add_selector_uchar buf ~add_ascii i u =
  let cp = Uchar.to_int u in
  if cp < 0x80 then add_ascii i (Char.chr cp)
  else add_selector_hex_escape buf cp

let add_selector_malformed buf ~add_ascii name i len =
  for j = i to i + len - 1 do
    let c = name.[j] in
    if Char.code c < 0x80 then add_ascii j c
    else add_selector_hex_escape buf (Char.code c)
  done

(* Fast path: most identifiers in real CSS are pure ASCII without any of the
   characters the full escaper would need to handle. [is_safe_nmchar] is the
   subset of {!is_valid_nmchar} that also rules out the leading-digit /
   leading-dash-digit pattern so the unmodified bytes are a valid CSS ident
   already. *)

(** Escape a class or ID name for use inside a selector, following CSS section
    9.1 rules: hex-escape control bytes and leading digits (or a leading dash
    followed by a digit), and backslash-escape the punctuation characters that
    otherwise terminate or reframe the selector. *)
let is_safe_nmchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

(* Toplevel: [let exception] allocates the constructor on every call, and both
   of these run once per selector name in the optimizer's inner loops. *)
exception Char_rejected

let name_is_plain_ascii_ident name =
  let len = String.length name in
  if len = 0 then false
  else if first_needs_hex_escape name then false
  else
    try
      for i = 0 to len - 1 do
        if not (is_safe_nmchar name.[i]) then raise Char_rejected
      done;
      true
    with Char_rejected -> false

let name_is_ascii name =
  let len = String.length name in
  try
    for i = 0 to len - 1 do
      if Char.code (String.unsafe_get name i) >= 0x80 then raise Char_rejected
    done;
    true
  with Char_rejected -> false

let escape_selector_name name =
  if String.length name = 0 then ""
  else if name = "-" then "\\-"
  else if name_is_plain_ascii_ident name then name
  else
    let buf = Buffer.create (String.length name * 2) in
    let first_needs_hex_escape = first_needs_hex_escape name in
    let add_ascii = add_selector_ascii buf ~first_needs_hex_escape in
    (* A name with escapable punctuation but no byte >= 0x80 (the common
       Tailwind case: [hover:p-4], [w-1/2], [bg-[#fff]]) has no multi-byte
       sequence to decode, so escape it byte by byte and skip the decoder, which
       would box a [Uchar] per character. [String.iteri]'s index is the byte
       offset, which equals the fold's index for single-byte input. *)
    if name_is_ascii name then String.iteri add_ascii name
    else begin
      let folder () i = function
        | Common.String.Scalar u -> add_selector_uchar buf ~add_ascii i u
        | Common.String.Malformed len ->
            add_selector_malformed buf ~add_ascii name i len
      in
      Common.String.utf8_fold folder () name
    end;
    Buffer.contents buf

let pp_ns ctx = function
  | Any -> Pp.string ctx "*|"
  | None ->
      (* Explicit "no namespace" prefix [(|)] -- distinct from omitting the
         prefix entirely, which is encoded by passing [None : ns option]. *)
      Pp.char ctx '|'
  | Prefix p ->
      Pp.string ctx (escape_selector_name p);
      Pp.char ctx '|'

(** Pretty print nth function with optional "of" clause *)
let rec can_follow_nth_of = function
  | Element (None, _)
  | Element (Some (Prefix _), _)
  | Universal (Some (Prefix _)) ->
      false
  | Compound (first :: _) | Combined (first, _, _) | List (first :: _) ->
      can_follow_nth_of first
  | _ -> true

let pp_nth_col_func ctx name expr =
  let pp_nth_col ctx = function
    | Odd -> Pp.string ctx "odd"
    | Even -> Pp.string ctx "even"
    | expr -> pp_nth ctx expr
  in
  Pp.char ctx ':';
  Pp.string ctx name;
  Pp.char ctx '(';
  pp_nth_col ctx expr;
  Pp.char ctx ')'

let comma_space ctx () = Pp.string ctx ", "

(* CSS Selectors 4 3.5: when the universal selector [*] is not the only
   component of a compound, the [*] may be omitted. Namespaced universals
   ([ns|*], [*|*]) carry namespace information and are preserved. *)
let drop_redundant_universal = function
  | [ _ ] as singleton -> singleton
  | components ->
      let kept =
        List.filter (function Universal None -> false | _ -> true) components
      in
      if kept = [] then components else kept

(* CSS Selectors 4 3.5: a compound selector that "contains a type selector or
   universal selector [...] must come first in the sequence". So a rewrite that
   splices a wrapped selector into the surrounding compound has to leave a
   type-bearing argument wrapped: spliced, the two names fuse and [.a:is(code)]
   reads as [.acode], a class nobody wrote. *)
let rec carries_type_selector = function
  | Element _ | Universal _ -> true
  | Compound parts -> List.exists carries_type_selector parts
  | _ -> false

(* Which elements [HTML] gives each state pseudo-class pair to. The three sets
   differ. [:enabled] and [:disabled] split every form control plus [optgroup],
   [option] and [fieldset] (HTML sec. 4.15, sec. 4.16.3). [:valid] and
   [:invalid] split a [form] and a [fieldset] whole, but reach a form control
   only while it is a candidate for constraint validation, which a disabled,
   readonly or [type=hidden] one is not. [:required] and [:optional] split
   [select] and [textarea]; [:optional] wants an [input] "to which the required
   attribute applies", and it does not apply in e.g. the Hidden state. *)
let enabled_carriers =
  [ "button"; "fieldset"; "input"; "optgroup"; "option"; "select"; "textarea" ]

let validity_carriers = [ "fieldset"; "form" ]
let optionality_carriers = [ "select"; "textarea" ]

let state_pair = function
  | Enabled -> Some (Disabled, enabled_carriers)
  | Disabled -> Some (Enabled, enabled_carriers)
  | Valid -> Some (Invalid, validity_carriers)
  | Invalid -> Some (Valid, validity_carriers)
  | Required -> Some (Optional, optionality_carriers)
  | Optional -> Some (Required, optionality_carriers)
  | _ -> None

(* Whether every element [sel] can select is one of [carriers]. A type selector
   proves it, and [:is()]/[:where()] prove it when every branch does; a class,
   an attribute, a negation or [:has()] constrains something other than the
   subject's element type, so it proves nothing. Selectors 4 sec. 3.5: the
   subject of a complex selector is its rightmost compound. A namespaced type
   selector is left out: the prefix binds to a namespace declared elsewhere in
   the stylesheet, and these names are the HTML ones. *)
let rec selects_only carriers = function
  | Element (None, name) -> List.mem (String.lowercase_ascii name) carriers
  | Compound parts -> List.exists (selects_only carriers) parts
  | Is branches | Where branches ->
      branches <> [] && List.for_all (selects_only carriers) branches
  | Combined (_, _, right) -> selects_only carriers right
  | _ -> false

(* CSS Selectors 4 sec. 12.1.1, 12.3.1 and 12.3.3: an element outside a pair's
   set matches neither half, so [X:not(:enabled)] is [X:disabled] only inside a
   compound that proves its subject carries the state. Which elements those are
   is a [HTML] fact rather than one the CSS text proves, hence the
   [enforce_spec] gate. *)
let fold_state_negations ctx components =
  if (not (Pp.minified ctx)) || ctx.Pp.enforce_spec then components
  else
    List.map
      (function
        | Not [ state ] as component -> (
            match state_pair state with
            | Some (complement, carriers)
              when List.exists (selects_only carriers) components ->
                complement
            | _ -> component)
        | component -> component)
      components

let rec pp_nth_func ctx name expr of_sel =
  Pp.char ctx ':';
  Pp.string ctx name;
  Pp.char ctx '(';
  pp_nth ctx expr;
  (match of_sel with
  | Some sels ->
      Pp.string ctx " of";
      (match sels with
      | first :: _ when Pp.minified ctx && can_follow_nth_of first -> ()
      | _ -> Pp.char ctx ' ');
      Pp.list ~sep:Pp.comma pp ctx sels
  | None -> ());
  Pp.char ctx ')'

and sels ctx selectors = Pp.list ~sep:Pp.comma pp ctx selectors

and sels_nested_function_lists ctx selectors =
  Pp.list ~sep:Pp.comma pp_nested_function_lists ctx selectors

and spaced_sels_nested_function_lists ctx selectors =
  Pp.list ~sep:comma_space pp_nested_function_lists ctx selectors

and pp_nested_function_lists ctx = function
  | Is selectors -> func ctx "is" spaced_sels_nested_function_lists selectors
  | Where selectors ->
      func ctx "where" spaced_sels_nested_function_lists selectors
  | Compound selectors ->
      List.iter
        (pp_nested_function_lists ctx)
        (fold_state_negations ctx selectors)
  | Combined (left, comb, right) ->
      pp_nested_function_lists ctx left;
      pp_combinator ctx comb;
      pp_nested_function_lists ctx right
  | Relative (comb, right) ->
      pp_relative_combinator ctx comb;
      pp_nested_function_lists ctx right
  | List selectors ->
      Pp.list ~sep:Pp.comma pp_nested_function_lists ctx selectors
  | selector -> pp ctx selector

and pp : t Pp.t =
 fun ctx -> function
  | Element (ns, name) ->
      Pp.option pp_ns ctx ns;
      Pp.string ctx (escape_selector_name name)
  | Class name ->
      Pp.char ctx '.';
      Pp.string ctx (escape_selector_name name)
  | Id name ->
      Pp.char ctx '#';
      Pp.string ctx (escape_selector_name name)
  | Universal ns ->
      Pp.option pp_ns ctx ns;
      Pp.char ctx '*'
  | Attribute (ns, attr_name, match_type, flag) ->
      Pp.char ctx '[';
      Pp.option pp_ns ctx ns;
      Pp.string ctx (string_of_attr_name attr_name);
      pp_attribute_match ctx match_type;
      pp_attr_flag ctx flag;
      Pp.char ctx ']'
  (* Simple pseudo-classes *)
  | Hover -> pseudo ctx "hover"
  | Active -> pseudo ctx "active"
  | Focus -> pseudo ctx "focus"
  | Focus_visible -> pseudo ctx "focus-visible"
  | Focus_within -> pseudo ctx "focus-within"
  | Target -> pseudo ctx "target"
  | Link -> pseudo ctx "link"
  | Visited -> pseudo ctx "visited"
  | Any_link -> pseudo ctx "any-link"
  | Local_link -> pseudo ctx "local-link"
  | Target_within -> pseudo ctx "target-within"
  | Scope -> pseudo ctx "scope"
  | Root -> pseudo ctx "root"
  | Empty -> pseudo ctx "empty"
  | First_child -> pseudo ctx "first-child"
  | Last_child -> pseudo ctx "last-child"
  | Only_child -> pseudo ctx "only-child"
  | First_of_type -> pseudo ctx "first-of-type"
  | Last_of_type -> pseudo ctx "last-of-type"
  | Only_of_type -> pseudo ctx "only-of-type"
  | Enabled -> pseudo ctx "enabled"
  | Disabled -> pseudo ctx "disabled"
  | Read_only -> pseudo ctx "read-only"
  | Read_write -> pseudo ctx "read-write"
  | Placeholder_shown -> pseudo ctx "placeholder-shown"
  | Default -> pseudo ctx "default"
  | Checked -> pseudo ctx "checked"
  | Indeterminate -> pseudo ctx "indeterminate"
  | Blank -> pseudo ctx "blank"
  | Valid -> pseudo ctx "valid"
  | Invalid -> pseudo ctx "invalid"
  | In_range -> pseudo ctx "in-range"
  | Out_of_range -> pseudo ctx "out-of-range"
  | Required -> pseudo ctx "required"
  | Optional -> pseudo ctx "optional"
  | User_invalid -> pseudo ctx "user-invalid"
  | User_valid -> pseudo ctx "user-valid"
  | Inert -> pseudo ctx "inert"
  | Autofill -> pseudo ctx "autofill"
  | Fullscreen -> pseudo ctx "fullscreen"
  | Modal -> pseudo ctx "modal"
  | Picture_in_picture -> pseudo ctx "picture-in-picture"
  | Left -> pseudo ctx "left"
  | Right -> pseudo ctx "right"
  | First -> pseudo ctx "first"
  | Defined -> pseudo ctx "defined"
  | Playing -> pseudo ctx "playing"
  | Paused -> pseudo ctx "paused"
  | Seeking -> pseudo ctx "seeking"
  | Buffering -> pseudo ctx "buffering"
  | Stalled -> pseudo ctx "stalled"
  | Muted -> pseudo ctx "muted"
  | Volume_locked -> pseudo ctx "volume-locked"
  | Future -> pseudo ctx "future"
  | Past -> pseudo ctx "past"
  | Current -> pseudo ctx "current"
  | Popover_open -> pseudo ctx "popover-open"
  | Open -> pseudo ctx "open"
  | Unknown_pseudo_class name -> pseudo ctx name
  | Unknown_pseudo_class_call (name, args) ->
      pp_func ctx ~prefix:":" name
        (fun ctx args ->
          Pp.string ctx
            (if Pp.minified ctx then Parser.to_string_minified args
             else Parser.string_of_components args))
        args
  | Local_scope -> pseudo ctx "local"
  | Global_scope -> pseudo ctx "global"
  | Local_call selectors -> func ctx "local" sels selectors
  | Global_call selectors -> func ctx "global" sels selectors
  (* Legacy pseudo-elements (use single colon in minified mode) *)
  | Before form -> legacy_elem ctx form "before"
  | After form -> legacy_elem ctx form "after"
  | First_letter form -> legacy_elem ctx form "first-letter"
  | First_line form -> legacy_elem ctx form "first-line"
  (* Modern double-colon pseudo-elements *)
  | Backdrop -> elem ctx "backdrop"
  | Marker -> elem ctx "marker"
  | Placeholder -> elem ctx "placeholder"
  | Selection -> elem ctx "selection"
  | Target_text -> elem ctx "target-text"
  | Spelling_error -> elem ctx "spelling-error"
  | Grammar_error -> elem ctx "grammar-error"
  | File_selector_button -> elem ctx "file-selector-button"
  (* Vendor-specific pseudo-classes *)
  | Moz_focusring -> vendor ctx "moz-focusring"
  | Moz_any_call selectors -> func ctx "-moz-any" sels selectors
  | Webkit_any -> vendor ctx "webkit-any"
  | Webkit_any_call selectors -> func ctx "-webkit-any" sels selectors
  | Webkit_autofill -> vendor ctx "webkit-autofill"
  | Moz_ui_invalid -> vendor ctx "moz-ui-invalid"
  | Moz_ui_valid -> vendor ctx "moz-ui-valid"
  | Scrollbar_state state -> pseudo ctx (scrollbar_state_ident state)
  (* Vendor-specific pseudo-elements *)
  | Moz_placeholder -> vendor_elem ctx "moz-placeholder"
  | Webkit_input_placeholder -> vendor_elem ctx "webkit-input-placeholder"
  | Ms_input_placeholder -> vendor_elem ctx "ms-input-placeholder"
  | Webkit_scrollbar part -> elem ctx (scrollbar_part_ident part)
  | Webkit_search_cancel_button -> vendor_elem ctx "webkit-search-cancel-button"
  | Webkit_search_decoration -> vendor_elem ctx "webkit-search-decoration"
  (* Webkit datetime pseudo-elements *)
  | Webkit_datetime_edit_fields_wrapper ->
      vendor_elem ctx "webkit-datetime-edit-fields-wrapper"
  | Webkit_date_and_time_value -> vendor_elem ctx "webkit-date-and-time-value"
  | Webkit_datetime_edit -> vendor_elem ctx "webkit-datetime-edit"
  | Webkit_datetime_edit_year_field ->
      vendor_elem ctx "webkit-datetime-edit-year-field"
  | Webkit_datetime_edit_month_field ->
      vendor_elem ctx "webkit-datetime-edit-month-field"
  | Webkit_datetime_edit_day_field ->
      vendor_elem ctx "webkit-datetime-edit-day-field"
  | Webkit_datetime_edit_hour_field ->
      vendor_elem ctx "webkit-datetime-edit-hour-field"
  | Webkit_datetime_edit_minute_field ->
      vendor_elem ctx "webkit-datetime-edit-minute-field"
  | Webkit_datetime_edit_second_field ->
      vendor_elem ctx "webkit-datetime-edit-second-field"
  | Webkit_datetime_edit_millisecond_field ->
      vendor_elem ctx "webkit-datetime-edit-millisecond-field"
  | Webkit_datetime_edit_meridiem_field ->
      vendor_elem ctx "webkit-datetime-edit-meridiem-field"
  | Webkit_inner_spin_button -> vendor_elem ctx "webkit-inner-spin-button"
  | Webkit_outer_spin_button -> vendor_elem ctx "webkit-outer-spin-button"
  | Webkit_calendar_picker_indicator ->
      vendor_elem ctx "webkit-calendar-picker-indicator"
  | Webkit_details_marker -> vendor_elem ctx "webkit-details-marker"
  | Details_content -> elem ctx "details-content"
  (* Functional pseudo-elements *)
  | Part idents -> elem_func ctx "part" (Pp.list ~sep:Pp.space Pp.string) idents
  | Slotted selectors -> elem_func ctx "slotted" sels selectors
  | Cue None -> elem ctx "cue"
  | Cue (Some selectors) -> elem_func ctx "cue" sels selectors
  | Cue_region None -> elem ctx "cue-region"
  | Cue_region (Some selectors) -> elem_func ctx "cue-region" sels selectors
  (* Functional pseudo-classes *)
  | Is selectors
    when Pp.minified ctx && List.sort compare selectors = [ Link; Visited ] ->
      (* CSS Selectors 4 sec. 8.1: [:any-link] is defined as equivalent to
         [:is(:link, :visited)], same specificity and shorter. *)
      pp ctx Any_link
  | Is selectors -> func ctx "is" sels selectors
  | Where selectors -> func ctx "where" sels selectors
  | Not [ Not [ inner ] ]
    when Pp.minified ctx && not (carries_type_selector inner) ->
      (* CSS Selectors 4 sec. 4.3: double negation [:not(:not(X))] is
         spec-equivalent to [X] (and shorter under minify). [X] is spliced into
         whatever compound holds the [:not()], so a type-bearing one stays
         wrapped (Selectors 4 3.5, [carries_type_selector]). *)
      pp ctx inner
  | Not [ Dir "ltr" ] when Pp.minified ctx && not ctx.Pp.enforce_spec ->
      (* CSS Selectors 4 sec. 7.1 leaves directionality to the document
         language, so CSS alone does not make [ltr] and [rtl] a partition: an
         element the language gives no directionality matches neither. [HTML]
         makes it one for every element in an HTML document, which is the host
         fact [enforce_spec] drops. *)
      func ctx "dir" Pp.string "rtl"
  | Not [ Dir "rtl" ] when Pp.minified ctx && not ctx.Pp.enforce_spec ->
      func ctx "dir" Pp.string "ltr"
  | Not selectors -> func ctx "not" sels selectors
  | Has selectors -> func ctx "has" sels_nested_function_lists selectors
  | Nth_child (Index 1, None) when Pp.minified ctx ->
      (* CSS Selectors 4 14: [:nth-child(1)] is spec-equivalent to
         [:first-child]; the keyword form is shorter. *)
      Pp.string ctx ":first-child"
  | Nth_last_child (Index 1, None) when Pp.minified ctx ->
      (* Likewise [:nth-last-child(1)] is [:last-child]. *)
      Pp.string ctx ":last-child"
  | Nth_of_type (Index 1, None) when Pp.minified ctx ->
      Pp.string ctx ":first-of-type"
  | Nth_last_of_type (Index 1, None) when Pp.minified ctx ->
      Pp.string ctx ":last-of-type"
  | Nth_child (expr, of_sel) -> pp_nth_func ctx "nth-child" expr of_sel
  | Nth_last_child (expr, of_sel) ->
      pp_nth_func ctx "nth-last-child" expr of_sel
  | Nth_of_type (expr, of_sel) -> pp_nth_func ctx "nth-of-type" expr of_sel
  | Nth_last_of_type (expr, of_sel) ->
      pp_nth_func ctx "nth-last-of-type" expr of_sel
  | Nth_col expr -> pp_nth_col_func ctx "nth-col" expr
  | Nth_last_col expr -> pp_nth_col_func ctx "nth-last-col" expr
  | Dir dir -> func ctx "dir" Pp.string dir
  | Lang names -> func ctx "lang" langs names
  | State name -> func ctx "state" Pp.string name
  | Current_of selectors -> func ctx "current" sels selectors
  | Host None -> pseudo ctx "host"
  | Host (Some selectors) -> func ctx "host" sels selectors
  | Host_context selectors -> func ctx "host-context" sels selectors
  | Heading -> Pp.string ctx ":heading()"
  | Active_view_transition -> pseudo ctx "active-view-transition"
  | Active_view_transition_type None ->
      Pp.string ctx ":active-view-transition-type()"
  | Active_view_transition_type (Some t) ->
      func ctx "active-view-transition-type" strs t
  | Highlight names -> elem_func ctx "highlight" strs names
  | View_transition -> elem ctx "view-transition"
  | View_transition_group sel ->
      elem_func ctx "view-transition-group" pp_vt_class_selector sel
  | View_transition_image_pair sel ->
      elem_func ctx "view-transition-image-pair" pp_vt_class_selector sel
  | View_transition_old sel ->
      pp_func ctx ~prefix:"::" "view-transition-old" pp_vt_class_selector sel
  | View_transition_new sel ->
      pp_func ctx ~prefix:"::" "view-transition-new" pp_vt_class_selector sel
  | Unknown_pseudo_element name -> elem ctx name
  | Unknown_pseudo_element_call (name, args) ->
      pp_func ctx ~prefix:"::" name
        (fun ctx args ->
          Pp.string ctx
            (if Pp.minified ctx then Parser.to_string_minified args
             else Parser.string_of_components args))
        args
  | Compound selectors ->
      List.iter (pp ctx) (fold_state_negations ctx selectors)
  | Combined (left, comb, right) ->
      pp ctx left;
      pp_combinator ctx comb;
      pp ctx right
  | Relative (comb, right) ->
      pp_relative_combinator ctx comb;
      pp ctx right
  | List selectors -> Pp.list ~sep:Pp.comma pp ctx selectors
  | Nesting -> Pp.char ctx '&'

open Common

let list_map_preserve = List.map_preserve
let list_same = List.same

(** Recursively map over all selectors in the tree. [f] is applied bottom-up; a
    node whose children are unchanged and for which [f] returns its argument
    keeps its physical identity. *)
let rec map f node =
  let lst ctor xs =
    let xs' = list_map_preserve (map f) xs in
    if xs' == xs then node else ctor xs'
  in
  let node' =
    match node with
    | Combined (left, combinator, right) ->
        let left' = map f left and right' = map f right in
        if left' == left && right' == right then node
        else Combined (left', combinator, right')
    | Relative (combinator, right) ->
        let right' = map f right in
        if right' == right then node else Relative (combinator, right')
    | Compound xs -> lst (fun xs -> Compound xs) xs
    | Where xs -> lst (fun xs -> Where xs) xs
    | Is xs -> lst (fun xs -> Is xs) xs
    | Not xs -> lst (fun xs -> Not xs) xs
    | Has xs -> lst (fun xs -> Has xs) xs
    | Moz_any_call xs -> lst (fun xs -> Moz_any_call xs) xs
    | Webkit_any_call xs -> lst (fun xs -> Webkit_any_call xs) xs
    | List xs -> lst (fun xs -> List xs) xs
    | Nth_child (nth, Some xs) -> lst (fun xs -> Nth_child (nth, Some xs)) xs
    | Nth_last_child (nth, Some xs) ->
        lst (fun xs -> Nth_last_child (nth, Some xs)) xs
    | Nth_of_type (nth, Some xs) ->
        lst (fun xs -> Nth_of_type (nth, Some xs)) xs
    | Nth_last_of_type (nth, Some xs) ->
        lst (fun xs -> Nth_last_of_type (nth, Some xs)) xs
    | Host (Some xs) -> lst (fun xs -> Host (Some xs)) xs
    | Current_of xs -> lst (fun xs -> Current_of xs) xs
    | Host_context xs -> lst (fun xs -> Host_context xs) xs
    | Slotted xs -> lst (fun xs -> Slotted xs) xs
    | Cue (Some xs) -> lst (fun xs -> Cue (Some xs)) xs
    | Cue_region (Some xs) -> lst (fun xs -> Cue_region (Some xs)) xs
    | other -> other
  in
  f node'

(* Dedup and sort an unordered selector list by minified-printed form so that
   permutations of the same alternatives collapse to a single canonical AST. *)
let canonicalize_unordered_list selectors =
  let seen = Hashtbl.create (List.length selectors) in
  let uniq =
    List.filter_map
      (fun s ->
        let key = Pp.to_string ~minify:true pp s in
        if Hashtbl.mem seen key then None
        else (
          Hashtbl.add seen key ();
          Some (key, s)))
      selectors
  in
  List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) uniq |> List.map snd

(* Substituting [&] splices a complex parent into a compound's leading slot,
   giving [Compound [Combined (.L, a); :where(.dark)]] where reading the same
   selector back gives [Combined (.L, Compound [a; :where(.dark)])]. The two
   serialise alike and only the second is a compound in the grammar's sense, a
   sequence of simple selectors, so the trailing components move onto the
   subject. Without this the two spellings stay structurally distinct while
   printing identically, and every structural comparison reads them apart. *)
let rec lift_leading_combinator = function
  | Combined (left, comb, right) :: rest ->
      let subject =
        match lift_leading_combinator (right :: rest) with
        | Some lifted -> lifted
        | None -> (
            match right :: rest with [ one ] -> one | cs -> Compound cs)
      in
      Some (Combined (left, comb, subject))
  | _ -> None

(* [map] rewrites a compound's components before the compound itself, so the
   [Is] branch below has already spliced a single-argument [:is()] into this
   list. A component the reader refuses after the pseudo-element can only have
   arrived that way, so the compound puts the wrapper back: Selectors 4 sec. 16
   builds a pseudo-compound out of [<pseudo-element-selector>
   <pseudo-class-selector>*] and sec. 3.6.3 makes the pseudo-class list per
   pseudo-element. *)
let rewrap_pseudo_compound components =
  let rec loop seen = function
    | [] -> []
    | c :: rest when is_pseudo_element c -> c :: loop (Some c) rest
    | c :: rest ->
        let c =
          match seen with
          | Some pe when not (is_pe_action c && pseudo_element_allows pe c) ->
              Is [ c ]
          | _ -> c
        in
        c :: loop seen rest
  in
  if List.exists is_pseudo_element components then loop None components
  else components

(* CSS Selectors 4 sec. 4.2: a single-argument [:is(s)] matches the same
   elements as [s] with the same specificity, so it reduces to [s]. Sound only
   when [s] is a single compound: a combinator ([Combined] / [Relative]) or a
   [List] makes [:is()] a grouping boundary that cannot be spliced into the
   surrounding compound. A type or universal selector cannot be spliced either:
   this rewrite is node-local and cannot see whether the [:is()] heads its
   compound, and only there may such a selector stand
   ([carries_type_selector]). *)
let canonicalize_is node selectors =
  let sorted = canonicalize_unordered_list selectors in
  match sorted with
  | [ single ]
    when match single with
         | Combined _ | Relative _ | List _ -> false
         | _ -> not (carries_type_selector single) ->
      single
  | _ -> if list_same sorted selectors then node else Is sorted

(* Canonicalise so selectors denoting the same thing are structurally equal:
   drop the implied [*] from a multi-part compound ([*.foo] -> [.foo]), collapse
   a one-part compound, and dedup/sort selector-list alternatives by printed
   form (both [List] and the set-based [:is]/[:where]/[:not]/[:has] lists, incl.
   the [:-moz-any]/[:-webkit-any] aliases, whose order Selectors 4 makes
   irrelevant to matching and specificity). [map] hands each node over without
   saying where it sits, so every rewrite below has to be sound in any position;
   the one that needs the top of a rule selector is [canonicalize]. *)
let canonicalize_nodes sel =
  map
    (fun node ->
      let canon ctor selectors =
        let sorted = canonicalize_unordered_list selectors in
        if list_same sorted selectors then node else ctor sorted
      in
      match node with
      | Compound components -> (
          match lift_leading_combinator components with
          | Some lifted -> lifted
          | None -> (
              match
                rewrap_pseudo_compound (drop_redundant_universal components)
              with
              | [ single ] -> single
              | components' ->
                  if list_same components' components then node
                  else Compound components'))
      | List selectors -> canon (fun xs -> List xs) selectors
      | Where selectors -> canon (fun xs -> Where xs) selectors
      | Is selectors -> canonicalize_is node selectors
      | Not selectors -> canon (fun xs -> Not xs) selectors
      | Has selectors -> canon (fun xs -> Has xs) selectors
      | Moz_any_call selectors -> canon (fun xs -> Moz_any_call xs) selectors
      | Webkit_any_call selectors ->
          canon (fun xs -> Webkit_any_call xs) selectors
      | Nth_child (nth, Some selectors) ->
          canon (fun xs -> Nth_child (nth, Some xs)) selectors
      | Nth_last_child (nth, Some selectors) ->
          canon (fun xs -> Nth_last_child (nth, Some xs)) selectors
      | Nth_of_type (nth, Some selectors) ->
          canon (fun xs -> Nth_of_type (nth, Some xs)) selectors
      | Nth_last_of_type (nth, Some selectors) ->
          canon (fun xs -> Nth_last_of_type (nth, Some xs)) selectors
      | other -> other)
    sel

let is_ sels = Is sels
let has sels = Has sels
let not selectors = Not selectors
let nth_child ?of_ nth = Nth_child (nth, of_)
let host ?selectors () = Host selectors

(* ========================= *)
(* Analysis helpers          *)
(* ========================= *)

let has_focus sel = any (function Focus -> true | _ -> false) sel

let has_focus_within sel =
  any (function Focus_within -> true | _ -> false) sel

let has_focus_visible sel =
  any (function Focus_visible -> true | _ -> false) sel

let zero_specificity = { ids = 0; classes = 0; elements = 0 }

let add_specificity a b =
  {
    ids = a.ids + b.ids;
    classes = a.classes + b.classes;
    elements = a.elements + b.elements;
  }

let compare_specificity a b =
  match compare a.ids b.ids with
  | 0 -> (
      match compare a.classes b.classes with
      | 0 -> compare a.elements b.elements
      | n -> n)
  | n -> n

let max_specificity xs =
  List.fold_left
    (fun acc x -> if compare_specificity x acc > 0 then x else acc)
    zero_specificity xs

let rec specificity = function
  | Id _ -> { ids = 1; classes = 0; elements = 0 }
  | Class _ | Attribute _ | Hover | Active | Focus | Focus_visible
  | Focus_within | Target | Link | Visited | Any_link | Local_link
  | Target_within | Scope | Root | Empty | First_child | Last_child | Only_child
  | First_of_type | Last_of_type | Only_of_type | Enabled | Disabled | Read_only
  | Read_write | Placeholder_shown | Default | Checked | Indeterminate | Blank
  | Valid | Invalid | In_range | Out_of_range | Required | Optional
  | User_invalid | User_valid | Inert | Autofill | Fullscreen | Modal
  | Picture_in_picture | Left | Right | First | Defined | Playing | Paused
  | Seeking | Buffering | Stalled | Muted | Volume_locked | Future | Past
  | Current | Popover_open | Open | Moz_focusring | Webkit_any | Webkit_autofill
  | Unknown_pseudo_class _ | Unknown_pseudo_class_call _ | Moz_placeholder
  | Webkit_input_placeholder | Ms_input_placeholder | Moz_ui_invalid
  | Moz_ui_valid | Scrollbar_state _
  | Webkit_scrollbar Scrollbar
  | Webkit_search_cancel_button | Webkit_search_decoration
  | Webkit_datetime_edit_fields_wrapper | Webkit_date_and_time_value
  | Webkit_datetime_edit | Webkit_datetime_edit_year_field
  | Webkit_datetime_edit_month_field | Webkit_datetime_edit_day_field
  | Webkit_datetime_edit_hour_field | Webkit_datetime_edit_minute_field
  | Webkit_datetime_edit_second_field | Webkit_datetime_edit_millisecond_field
  | Webkit_datetime_edit_meridiem_field | Webkit_inner_spin_button
  | Webkit_outer_spin_button | Webkit_calendar_picker_indicator
  | Webkit_details_marker | Details_content | Nth_col _ | Nth_last_col _ | Dir _
  | Lang _ | State _ | Active_view_transition | Active_view_transition_type _
  | Heading | Local_scope | Global_scope ->
      { ids = 0; classes = 1; elements = 0 }
  | Element _ -> { ids = 0; classes = 0; elements = 1 }
  | Universal _ | Nesting -> zero_specificity
  | Before _ | After _ | First_letter _ | First_line _ | Backdrop | Marker
  | Placeholder | Selection | Target_text | Spelling_error | Grammar_error
  | File_selector_button | Part _ | View_transition | View_transition_group _
  | View_transition_image_pair _ | View_transition_old _ | View_transition_new _
  | Unknown_pseudo_element _ | Unknown_pseudo_element_call _
  (* Sec. 17 counts a pseudo-element in C. [::-webkit-scrollbar] is read with
     one colon too and weighs a class above, as every [pseudo_vendor_idents]
     name does; the parts are [::]-only. *)
  | Webkit_scrollbar (Button | Track | Track_piece | Thumb | Corner | Resizer)
  | Cue None
  | Cue_region None ->
      { ids = 0; classes = 0; elements = 1 }
  | Where _ -> zero_specificity
  | Is xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Not xs
  | Has xs
  | Current_of xs ->
      xs |> List.map specificity |> max_specificity
  | Nth_child (_, of_)
  | Nth_last_child (_, of_)
  | Nth_of_type (_, of_)
  | Nth_last_of_type (_, of_) ->
      add_specificity
        { ids = 0; classes = 1; elements = 0 }
        (match of_ with
        | None -> zero_specificity
        | Some xs -> xs |> List.map specificity |> max_specificity)
  | Host None -> { ids = 0; classes = 1; elements = 0 }
  | Host (Some xs) | Host_context xs ->
      add_specificity
        { ids = 0; classes = 1; elements = 0 }
        (xs |> List.map specificity |> max_specificity)
  | Slotted xs | Cue (Some xs) | Cue_region (Some xs) ->
      add_specificity
        { ids = 0; classes = 0; elements = 1 }
        (xs |> List.map specificity |> max_specificity)
  | Local_call xs | Global_call xs ->
      xs |> List.map specificity |> max_specificity
  | Highlight _ -> { ids = 0; classes = 0; elements = 1 }
  | Compound xs ->
      List.fold_left
        (fun acc sel -> add_specificity acc (specificity sel))
        zero_specificity xs
  | Combined (a, _, b) -> add_specificity (specificity a) (specificity b)
  | Relative (_, sel) -> specificity sel
  | List xs -> xs |> List.map specificity |> max_specificity

(* Selectors 4 sec. 4.2 replaces the specificity of [:is()] with that of its
   most specific argument, so splitting [:is(s1, s2, ...)] into the selector
   list [s1, s2, ...] holds the weight of every match only when the arguments
   already agree on one specificity. The arguments also have to be structurally
   simple: [&] is out because [specificity] reads [Nesting] as zero while CSS
   Nesting 1 sec. 3 weighs it as the most specific selector in the parent rule,
   so the equality below would pass on a weight it never measured. *)
let rec is_unwrap_safe_is_arg : t -> bool = function
  | Element _ | Class _ | Id _ | Universal _ | Attribute _ -> true
  | Compound parts -> List.for_all is_unwrap_safe_is_arg parts
  | _ -> false

let is_unwrap_safe selectors =
  List.for_all is_unwrap_safe_is_arg selectors
  &&
  match List.map specificity selectors with
  | [] -> false
  | s :: rest -> List.for_all (fun s' -> s' = s) rest

(* Sound only where the [:is()] is a whole rule selector, or a whole member of
   one top-level list: there the split lands in a selector list, which weighs
   each branch on its own. Nested inside a [Compound] or a [Combined] it is a
   grouping boundary and the split would change what matches. A lone argument
   goes further than [canonicalize_is] can: that one is node-local and has to
   leave a type or universal argument wrapped rather than splice it into a
   compound it cannot see, and here there is no compound to splice into. *)
let rec top_level_is_unwrap sel =
  match sel with
  | Is [ single ] when is_unwrap_safe_is_arg single -> single
  | Is selectors when is_unwrap_safe selectors -> List selectors
  | List selectors ->
      let expanded =
        List.concat_map
          (fun sel ->
            match top_level_is_unwrap sel with
            | List members -> members
            | other -> [ other ])
          selectors
      in
      if list_same expanded selectors then sel
      else List (canonicalize_unordered_list expanded)
  | _ -> sel

let canonicalize sel = top_level_is_unwrap (canonicalize_nodes sel)
let to_string ?minify t = Pp.to_string ?minify pp t
let to_buffer ?minify buf t = Pp.to_buffer ?minify buf pp t

let exists_class pred sel =
  any (function Class name -> pred name | _ -> false) sel

let rec first_class = function
  | Class n -> Some n
  | Compound xs -> List.find_map first_class xs
  | Combined (a, _, _) -> first_class a
  | List (h :: _) -> first_class h
  | Is xs
  | Where xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs -> (
      match xs with [] -> None | h :: _ -> first_class h)
  | Cue (Some (h :: _)) | Cue_region (Some (h :: _)) -> first_class h
  | Part _ -> None
  | _ -> None

let contains_modifier_colon sel =
  exists_class (fun name -> String.contains name ':') sel

(** Check if selector contains :where(.group) - used for group-* modifiers.
    Note: can't use 'any' because it transforms Where to List before calling p.
*)
let is_group_class = function
  | Class s ->
      s = "group" || (String.length s > 6 && String.sub s 0 6 = "group/")
  | _ -> false

let rec has_group_marker = function
  | Where xs ->
      List.exists
        (function
          | sel when is_group_class sel -> true
          | Compound cs -> List.exists is_group_class cs
          | other -> has_group_marker other)
        xs
  | Compound xs -> List.exists has_group_marker xs
  | Combined (a, _, b) -> has_group_marker a || has_group_marker b
  | Relative (_, b) -> has_group_marker b
  | List xs -> List.exists has_group_marker xs
  | Cue (Some xs) | Cue_region (Some xs) -> List.exists has_group_marker xs
  | Is xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs ->
      List.exists has_group_marker xs
  | _ -> false

(** Check if selector contains :where(.peer) - used for peer-* modifiers *)
let is_peer_class = function
  | Class s -> s = "peer" || (String.length s > 5 && String.sub s 0 5 = "peer/")
  | _ -> false

let rec has_peer_marker = function
  | Where xs ->
      List.exists
        (function
          | sel when is_peer_class sel -> true
          | Compound cs -> List.exists is_peer_class cs
          | other -> has_peer_marker other)
        xs
  | Compound xs -> List.exists has_peer_marker xs
  | Combined (a, _, b) -> has_peer_marker a || has_peer_marker b
  | Relative (_, b) -> has_peer_marker b
  | List xs -> List.exists has_peer_marker xs
  | Cue (Some xs) | Cue_region (Some xs) -> List.exists has_peer_marker xs
  | Is xs
  | Not xs
  | Has xs
  | Moz_any_call xs
  | Webkit_any_call xs
  | Slotted xs
  | Current_of xs ->
      List.exists has_peer_marker xs
  | _ -> false

(** Check if selector uses the :is(:where(...)) pattern used by group-* and
    peer-* variants. *)
let has_is_where_pattern sel = has_group_marker sel || has_peer_marker sel

(** A "newer" pseudo-class with limited browser support: it must not be combined
    in a selector list with an [:is(:where())] variant, since a browser that
    lacks it drops the whole rule, whereas the forgiving variant would survive.
*)
let is_newer_pseudo_class = function
  | User_valid | User_invalid -> true
  | _ -> false

(** Check if a selector directly uses a newer pseudo-class (not nested inside
    :is()/:where() which provides forgiving parsing). *)
let rec has_newer_pseudo_class = function
  | User_valid | User_invalid -> true
  | Compound xs -> List.exists has_newer_pseudo_class xs
  | Combined (a, _, b) -> has_newer_pseudo_class a || has_newer_pseudo_class b
  | Relative (_, b) -> has_newer_pseudo_class b
  | List xs -> List.exists has_newer_pseudo_class xs
  | Current_of xs -> List.exists has_newer_pseudo_class xs
  (* Stop recursion at forgiving selectors -- :is()/:where() have forgiving
     parsing, so newer pseudo-classes inside them don't cause the whole rule to
     fail *)
  | Is _ | Where _ | Moz_any_call _ | Webkit_any_call _ -> false
  | Not xs | Has xs -> List.exists has_newer_pseudo_class xs
  | _ -> false

let modifier_prefix sel =
  match first_class sel with
  | Option.None -> Option.None
  | Option.Some class_name -> (
      match String.index_opt class_name ':' with
      | Option.Some idx -> Option.Some (String.sub class_name 0 (idx + 1))
      | Option.None -> Option.None)

let ( && ) sel1 sel2 = compound [ sel1; sel2 ]
let ( || ) s1 s2 = combine s1 Column s2
let compare (a : t) b = Stdlib.compare a b
