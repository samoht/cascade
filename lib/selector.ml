(** CSS Selectors - types and pretty printing *)

include Selector_intf

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

let pp_ns ctx = function
  | Any -> Pp.string ctx "*|"
  | None -> ()
  | Prefix p ->
      Pp.string ctx p;
      Pp.char ctx '|'

let pp_attr_flag ctx = function
  | Some Case_insensitive ->
      Pp.char ctx ' ';
      Pp.char ctx 'i'
  | Some Case_sensitive ->
      Pp.char ctx ' ';
      Pp.char ctx 's'
  | None -> ()

(* Check if an attribute value needs quoting according to CSS specs *)
let attr_value_needs_quoting value =
  if value = "" then true
  else
    let first = value.[0] in
    (* Must quote if starts with digit or two hyphens *)
    if
      (first >= '0' && first <= '9')
      || (first = '-' && String.length value > 1 && value.[1] = '-')
    then true
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
let pp_attr_value : string Pp.t =
 fun ctx value ->
  (* Only quote attribute values when necessary per CSS specs. This preserves
     the original format when possible. *)
  if attr_value_needs_quoting value then Pp.quoted_string ctx value
  else Pp.string ctx value

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
  | Whitespace_list value ->
      Pp.string ctx "~=";
      pp_attr_value ctx value
  | Hyphen_list value ->
      Pp.string ctx "|=";
      pp_attr_value ctx value
  | Prefix value ->
      Pp.string ctx "^=";
      pp_attr_value ctx value
  | Suffix value ->
      Pp.string ctx "$=";
      pp_attr_value ctx value
  | Substring value ->
      Pp.string ctx "*=";
      pp_attr_value ctx value

let is_hex_char c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

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
      else if is_hex_char name.[!i] then (
        incr i;
        consume_hex (n + 1))
    in
    consume_hex 0;
    if !i = start then incr i (* single escaped char *)
    else if !i < len && name.[!i] = ' ' then incr i

let validate_css_identifier name =
  if String.length name = 0 then err_invalid_identifier name "cannot be empty";

  let first_char = name.[0] in

  (* Check for invalid starting patterns *)
  if first_char >= '0' && first_char <= '9' then
    err_invalid_identifier name "cannot start with digit";

  if String.length name >= 2 then (
    if name.[0] = '-' && name.[1] = '-' then
      err_invalid_identifier name
        "cannot start with '--' (reserved for custom properties)";
    if name.[0] = '-' && name.[1] >= '0' && name.[1] <= '9' then
      err_invalid_identifier name "cannot start with '-' followed by digit");

  (* Validate characters with CSS escape support *)
  let len = String.length name in
  let i = ref 0 in
  while !i < len do
    let c = name.[!i] in
    if c = '\\' then skip_css_escape name i
    else
      let idx = !i in
      let is_valid =
        if idx = 0 then is_valid_nmstart c || c = '-' else is_valid_nmchar c
      in
      if (not is_valid) && Char.code c <= 127 then
        err_invalid_identifier name
          (String.concat ""
             [
               "contains invalid character '";
               String.make 1 c;
               "' at position ";
               Int.to_string idx;
             ]);
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
  if String.length name >= 2 && name.[0] = '-' && name.[1] = '-' then
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
let aria_attr_of_string s : aria_attr =
  match s with
  | "aria-busy" -> Busy
  | "aria-checked" -> Checked
  | "aria-disabled" -> Disabled
  | "aria-expanded" -> Expanded
  | "aria-hidden" -> Hidden
  | "aria-pressed" -> Pressed
  | "aria-readonly" -> Readonly
  | "aria-required" -> Required
  | "aria-selected" -> Selected
  | s when String.length s > 5 && String.sub s 0 5 = "aria-" ->
      Custom (String.sub s 5 (String.length s - 5))
  | _ -> invalid_arg ("not an aria attribute: " ^ s)

(** Categorize an attribute name into its structured type *)
let attr_name_of_string name =
  let len = String.length name in
  if len > 5 && String.sub name 0 5 = "aria-" then
    Aria (aria_attr_of_string name)
  else if len > 5 && String.sub name 0 5 = "data-" then
    Data (String.sub name 5 (len - 5))
  else Regular name

(** Convert attr_name back to string for printing *)
let string_of_aria_attr : aria_attr -> string = function
  | Busy -> "aria-busy"
  | Checked -> "aria-checked"
  | Disabled -> "aria-disabled"
  | Expanded -> "aria-expanded"
  | Hidden -> "aria-hidden"
  | Pressed -> "aria-pressed"
  | Readonly -> "aria-readonly"
  | Required -> "aria-required"
  | Selected -> "aria-selected"
  | Custom s -> "aria-" ^ s

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
let hex_to_int c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> invalid_arg "not a hex digit"

(* Unescape CSS escapes per spec: \XX...XX (1-6 hex) or \X (any char). Handles
   both hex escapes (e.g., \3A for ':') and simple escapes (e.g., \:). *)
(* Helper to process hex escape sequences. Returns (codepoint, next_index) *)
let process_hex_escape s i len =
  let rec consume_hex acc n idx =
    if n = 6 || idx >= len || not (is_hex_char s.[idx]) then (acc, idx)
    else consume_hex ((acc * 16) + hex_to_int s.[idx]) (n + 1) (idx + 1)
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
    else if is_hex_char s.[i + 1] then (
      let codepoint, final_idx = process_hex_escape s i len in
      (* Add the unescaped character if it's valid *)
      if codepoint > 0 && codepoint <= 0x10FFFF then
        Buffer.add_utf_8_uchar buf (Uchar.of_int codepoint);
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

let of_string s =
  if String.length s = 0 then invalid_arg "of_string: empty selector string";
  let first_char = s.[0] in
  match first_char with
  | '.' ->
      (* Class selector: .classname *)
      if String.length s = 1 then
        invalid_arg "of_string: incomplete class selector";
      let raw = unescape_selector_name (String.sub s 1 (String.length s - 1)) in
      class_ raw
  | '#' ->
      (* ID selector: #idname *)
      if String.length s = 1 then
        invalid_arg "of_string: incomplete id selector";
      let raw = unescape_selector_name (String.sub s 1 (String.length s - 1)) in
      id raw
  | _ ->
      (* Element selector (no prefix) *)
      let raw = unescape_selector_name s in
      element raw

(* Simple readers that don't need recursion *)
let read_lang_content t =
  Lang (Cursor.list ~sep:Cursor.comma ~at_least:1 Cursor.ident t)

let read_dir_content t = Dir (Cursor.ident t)
let read_state_content t = State (Cursor.ident t)
let read_heading_content _t = Heading

let read_active_view_transition_content t =
  Active_view_transition_type
    (Cursor.option (Cursor.list ~sep:Cursor.comma ~at_least:1 Cursor.ident) t)

let read_lang t = Cursor.call "lang" t read_lang_content
let read_dir t = Cursor.call "dir" t read_dir_content
let read_state t = Cursor.call "state" t read_state_content
let read_heading t = Cursor.call "heading" t read_heading_content

let read_active_view_transition_type t =
  Cursor.call "active-view-transition-type" t
    read_active_view_transition_content

let read_part_content t =
  let idents = Cursor.list ~sep:Cursor.comma ~at_least:1 Cursor.ident t in
  Part idents

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

let is_compound_list = function List _ -> true | _ -> false
let as_list = function List sels -> Some sels | _ -> None
let compound selectors = Compound selectors
let err_expected t what = Cursor.err_expected t what

(** Parse attribute value (quoted or unquoted) *)
let read_attribute_value t =
  (* Check if we start with a quote - if so, we MUST parse as quoted string *)
  let value, was_quoted =
    match Cursor.string_opt t with
    | Some s -> (s, true)
    | None ->
        (* Otherwise parse as an ident / dimension / number *)
        let v =
          match Cursor.peek t with
          | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
              Cursor.skip t;
              s
          | Some (Component.Preserved { kind = Token.Number_tok _; _ })
          | Some (Component.Preserved { kind = Token.Dimension _; _ })
          | Some (Component.Preserved { kind = Token.Percentage _; _ }) -> (
              let n, unit = Cursor.number_with_unit t in
              match unit with
              | None ->
                  if Float.is_integer n then string_of_int (int_of_float n)
                  else string_of_float n
              | Some u ->
                  if Float.is_integer n then string_of_int (int_of_float n) ^ u
                  else string_of_float n ^ u)
          | _ -> ""
        in
        (v, false)
  in
  (* CSS spec allows empty quoted strings but not empty unquoted values *)
  if value = "" && not was_quoted then
    match Cursor.peek t with
    | None -> Cursor.err_expected_but_eof t "']'"
    | Some _ -> Cursor.err_invalid t "attribute value"
  else value

(** Validate CSS identifier with proper reader error context *)
let validate_css_identifier_with_reader t name =
  try validate_css_identifier name
  with Invalid_argument msg ->
    (* Extract just the validation reason from the message *)
    let clean_msg =
      if String.contains msg '\'' then
        let parts = String.split_on_char '\'' msg in
        match parts with
        | _ :: _ :: reason :: _ -> "invalid identifier: " ^ String.trim reason
        | _ -> msg
      else msg
    in
    Cursor.err t clean_msg

(** Parse a class selector (.classname) *)
let read_class t =
  Cursor.expect '.' t;
  let name = Cursor.ident ~keep_case:true t in
  (* No validation needed - Cursor.ident already ensures valid identifier *)
  Class name

(** Parse an ID selector ([#id]). Per CSS Selectors §6.6, an ID must be an
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

(** Parse a namespaced type or universal selector *)
let read_type_or_universal t =
  (* Try to read namespace prefix first *)
  let ns =
    Cursor.option
      (fun t ->
        if Cursor.try_kind_pair (Token.Delim '*') (Token.Delim '|') t then Any
        else
          let p = Cursor.ident ~keep_case:true t in
          Cursor.expect '|' t;
          Prefix p)
      t
  in

  (* Now read the selector itself *)
  match Cursor.peek_delim t with
  | Some '*' -> (
      Cursor.skip t;
      match ns with None -> universal | Some ns -> universal_ns ns)
  | _ -> (
      let name = Cursor.ident ~keep_case:true t in
      validate_css_identifier_with_reader t name;
      match ns with
      | None -> Element (None, name)
      | Some ns -> Element (Some ns, name))

(** Parse attribute selector [attr] or [attr=value] *)
let read_combinator t =
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
  | Some '|' when Cursor.try_kind_pair (Token.Delim '|') (Token.Delim '|') t ->
      Column
  | Some '!' -> Cursor.err t "invalid combinator character"
  | None when Cursor.is_done t -> Cursor.err t "empty combinator"
  | _ -> Descendant

let read_attribute_match t : attribute_match =
  let try_eq c (cons : string -> attribute_match) : attribute_match option =
    if Cursor.try_kind_pair (Token.Delim c) (Token.Delim '=') t then
      Some (cons (read_attribute_value t))
    else None
  in
  match try_eq '~' (fun v -> Whitespace_list v) with
  | Some v -> v
  | None -> (
      match try_eq '|' (fun v -> Hyphen_list v) with
      | Some v -> v
      | None -> (
          match try_eq '^' (fun v -> Prefix v) with
          | Some v -> v
          | None -> (
              match try_eq '$' (fun v -> Suffix v) with
              | Some v -> v
              | None -> (
                  match try_eq '*' (fun v -> Substring v) with
                  | Some v -> v
                  | None ->
                      if Cursor.peek_delim t = Some '=' then (
                        Cursor.skip t;
                        Exact (read_attribute_value t))
                      else Presence))))

let read_ns t : ns option =
  Cursor.option
    (fun t ->
      if Cursor.try_kind_pair (Token.Delim '*') (Token.Delim '|') t then Any
      else
        let p = Cursor.ident ~keep_case:true t in
        (* Avoid treating '|=' as a namespace separator: peek for the pair. *)
        let is_eq_pair =
          Cursor.lookahead
            (fun t ->
              Cursor.try_kind_pair (Token.Delim '|') (Token.Delim '=') t)
            t
        in
        if is_eq_pair then Cursor.err t "not a namespace";
        (* Expect the namespace separator *)
        Cursor.expect '|' t;
        Prefix p)
    t

let read_attr_flag t : attr_flag option =
  Cursor.ws t;
  Cursor.option
    (fun t ->
      match Cursor.ident_opt t with
      | Some "i" -> Case_insensitive
      | Some "s" -> Case_sensitive
      | Some s -> Cursor.err t ~got:s "'i' or 's'"
      | None -> Cursor.err_unexpected t)
    t

let read_attribute t =
  Cursor.brackets
    (fun inner ->
      Cursor.ws inner;
      let ns = read_ns inner in
      let attr = Cursor.ident ~keep_case:true inner in
      validate_css_identifier_with_reader inner attr;
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

(** Parse the An+B microsyntax per Selectors Level 4 section 9.2 / CSS Syntax
    Level 3 section 6. The grammar is handled as a set of shape patterns against
    the component stream:

    - Keywords [odd] / [even].
    - Bare [<integer>].
    - [<n-dimension>] (e.g. [5n]) optionally followed by an offset.
    - [<ndashdigit-dimension>] like [5n-5] (single token with unit [n-5]).
    - [<ndash-dimension>] like [5n-] followed by a signless integer.
    - Ident forms: [n], [-n], [n-5], [-n-5], [n-], [-n-] with same offset
      handling.
    - A leading [+] Delim (no whitespace before [n]) promoting the ident forms.

    Case-insensitivity per CSS idents (section 3.3). Whitespace between a
    leading [+] sign and the ident is invalid: the [+] is part of the ident form
    lexically, so [+n] is valid but [+ n] is not. *)

(* Numeric helpers: split an arbitrary ident's tail into an optional [-digits]
   suffix, for ndashdigit / ndash / n patterns. *)
let ndashdigit_b unit_ =
  (* Match [n-<digits>] case-insensitively; return [Some digits] or [None]. *)
  let n = String.length unit_ in
  if n >= 3 && Char.lowercase_ascii unit_.[0] = 'n' && unit_.[1] = '-' then
    let tail = String.sub unit_ 2 (n - 2) in
    match int_of_string_opt tail with Some b -> Some b | None -> None
  else None

let is_ndash unit_ =
  String.length unit_ = 2
  && Char.lowercase_ascii unit_.[0] = 'n'
  && unit_.[1] = '-'

let is_n_unit unit_ =
  String.length unit_ = 1 && Char.lowercase_ascii unit_.[0] = 'n'

let dashndashdigit_b ident =
  (* Match [-n-<digits>] case-insensitively; return [Some digits]. *)
  let n = String.length ident in
  if
    n >= 4
    && ident.[0] = '-'
    && Char.lowercase_ascii ident.[1] = 'n'
    && ident.[2] = '-'
  then
    let tail = String.sub ident 3 (n - 3) in
    match int_of_string_opt tail with Some b -> Some b | None -> None
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

(* Parse a [<signless-integer>] (no leading [+]/[-] in the token repr). *)
let read_signless_integer t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Number_tok n; _ })
    when not (repr_is_signed n) ->
      Cursor.skip t;
      int_of_float n.value
  | _ -> Cursor.err_expected t "signless integer"

(* Parse a [<signed-integer>]: a single number token whose repr starts with [+]
   or [-]. *)
let read_signed_integer_opt t =
  match Cursor.peek t with
  | Some (Component.Preserved { kind = Token.Number_tok n; _ })
    when repr_is_signed n ->
      Cursor.skip t;
      Some (int_of_float n.value)
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
   from Selectors Level 4 section 9.2. Assumes the cursor is positioned on a
   [Dimension] component. *)
let read_nth_dimension t number unit_ =
  if is_n_unit unit_ then (
    Cursor.skip t;
    An_plus_b (int_of_float number.Token.value, read_an_tail t))
  else
    match ndashdigit_b unit_ with
    | Some b ->
        Cursor.skip t;
        An_plus_b (int_of_float number.Token.value, -b)
    | None ->
        if is_ndash unit_ then (
          Cursor.skip t;
          An_plus_b (int_of_float number.Token.value, -read_signless_integer t))
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
  | Some (Component.Preserved { kind = Token.Delim '+'; _ }) ->
      Cursor.skip t;
      read_nth_after_plus t
  | _ -> Cursor.err t "expected 'odd', 'even', or An+B expression"

(** Pretty print nth expression *)
let pp_nth : nth Pp.t =
 fun ctx -> function
  (* Tailwind uses 2n+1 and 2n instead of odd/even keywords *)
  | Odd -> Pp.string ctx "2n+1"
  | Even -> Pp.string ctx "2n"
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
  ]

let pseudo_element_legacy_idents =
  [
    (* Legacy pseudo-elements *)
    ("before", Before);
    ("after", After);
    ("first-letter", First_letter);
    ("first-line", First_line);
  ]

let pseudo_element_modern_idents =
  [
    (* Modern pseudo-elements *)
    ("backdrop", Backdrop);
    ("marker", Marker);
    ("placeholder", Placeholder);
    ("selection", Selection);
    ("file-selector-button", File_selector_button);
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
    ("-webkit-scrollbar", Webkit_scrollbar);
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

(* Forward declarations for mutually recursive functions *)
let rec read_complex_list t =
  Cursor.ws t;
  if Cursor.is_done t then Cursor.err t "expected at least one selector"
  else
    try Cursor.list ~sep:Cursor.comma ~at_least:1 read_complex t
    with Cursor.Parse_error _ -> Cursor.err t "expected at least one selector"

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
  (* Per Selectors Level 4 section 9.2, the An+B (plus optional [of S]) must
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
  | _ -> read_complex t

and read_relative_selector_list t =
  Cursor.ws t;
  if Cursor.is_done t then Cursor.err t "expected at least one selector"
  else
    try Cursor.list ~sep:Cursor.comma ~at_least:1 read_relative_selector t
    with Cursor.Parse_error _ -> Cursor.err t "expected at least one selector"

(* Helper readers for functional pseudo-class content *)
and read_is_content t = Is (read_complex_list t)
and read_has_content t = Has (read_relative_selector_list t)
and read_not_content t = Not (read_complex_list t)
and read_where_content t = Where (read_complex_list t)

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

(* Read helper functions for functional pseudo-classes *)
and read_is t = Cursor.call "is" t read_is_content
and read_has t = Cursor.call "has" t read_has_content
and read_not t = Cursor.call "not" t read_not_content
and read_where t = Cursor.call "where" t read_where_content
and read_nth_child t = Cursor.call "nth-child" t read_nth_child_content

and read_nth_last_child t =
  Cursor.call "nth-last-child" t read_nth_last_child_content

and read_nth_of_type t = Cursor.call "nth-of-type" t read_nth_of_type_content

and read_nth_last_of_type t =
  Cursor.call "nth-last-of-type" t read_nth_last_type_content

and read_host t = Cursor.call "host" t read_host_content
and read_host_context t = Cursor.call "host-context" t read_host_context_content

(* Helper readers for pseudo-element functions that need recursion *)
and read_slotted_content t =
  let sels = read_complex_list t in
  Slotted sels

and read_cue_content t =
  let sels = read_complex_list t in
  Cue sels

and read_cue_region_content t =
  let sels = read_complex_list t in
  Cue_region sels

and read_highlight_content t =
  let names = Cursor.list ~sep:Cursor.comma ~at_least:1 Cursor.ident t in
  Highlight names

and read_view_transition_group_content t =
  let name = Cursor.ident t in
  View_transition_group name

and read_vt_image_pair_content t =
  let name = Cursor.ident t in
  View_transition_image_pair name

and read_view_transition_old_content t =
  let name = Cursor.ident t in
  View_transition_old name

and read_view_transition_new_content t =
  let name = Cursor.ident t in
  View_transition_new name

and read_slotted t = Cursor.call "slotted" t read_slotted_content
and read_cue t = Cursor.call "cue" t read_cue_content
and read_cue_region t = Cursor.call "cue-region" t read_cue_region_content
and read_highlight t = Cursor.call "highlight" t read_highlight_content

and read_view_transition_group t =
  Cursor.call "view-transition-group" t read_view_transition_group_content

and read_view_transition_image_pair t =
  Cursor.call "view-transition-image-pair" t read_vt_image_pair_content

and read_view_transition_old t =
  Cursor.call "view-transition-old" t read_view_transition_old_content

and read_view_transition_new t =
  Cursor.call "view-transition-new" t read_view_transition_new_content

(** Parse pseudo-class (:hover, :nth-child(2n+1), etc.) *)
and read_pseudo_class t =
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  let all_idents =
    pseudo_class_base_idents @ pseudo_element_legacy_idents
    @ pseudo_element_modern_idents @ pseudo_vendor_idents
  in
  Cursor.enum_or_calls "pseudo-class" all_idents
    ~calls:
      [
        ("is", read_is);
        ("has", read_has);
        ("not", read_not);
        ("where", read_where);
        ("nth-child", read_nth_child);
        ("nth-last-child", read_nth_last_child);
        ("nth-of-type", read_nth_of_type);
        ("nth-last-of-type", read_nth_last_of_type);
        ("lang", read_lang);
        ("dir", read_dir);
        ("state", read_state);
        ("host", read_host);
        ("host-context", read_host_context);
        ("heading", read_heading);
        ("active-view-transition-type", read_active_view_transition_type);
      ]
    t

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
      ("highlight", read_highlight);
      ("view-transition-group", read_view_transition_group);
      ("view-transition-image-pair", read_view_transition_image_pair);
      ("view-transition-old", read_view_transition_old);
      ("view-transition-new", read_view_transition_new);
    ]
    ~default:(fun t ->
      Cursor.enum "pseudo-element"
        (pseudo_element_modern_idents @ pseudo_vendor_idents
       @ pseudo_element_legacy_idents)
        t)
    t

(** Parse a simple selector (one part). Does not skip leading whitespace — the
    caller (read_compound) uses whitespace as a compound / descendant boundary
    marker. *)
and read_simple t =
  match Cursor.peek_delim t with
  | Some '.' -> read_class t
  | Some '*' -> read_type_or_universal t
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
        else read_pseudo_class t
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
          | _ -> false)
        || Cursor.peek_hash t <> None
        || Cursor.peek_colon t
        || Cursor.peek_block t = Some Token.Square
        || Cursor.peek_ident t <> None
  in
  let rec loop acc =
    if can_start () then
      let s = read_simple t in
      loop (s :: acc)
    else acc
  in
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
      | Some ('.' | '*' | '&') -> true
      | _ -> false)
    || Cursor.peek_hash t <> None
    || Cursor.peek_colon t
    || Cursor.peek_block t = Some Token.Square
    || Cursor.peek_ident t <> None
  in
  match Cursor.peek_delim t with
  | Some '|'
    when Cursor.lookahead
           (fun t -> Cursor.try_kind_pair (Token.Delim '|') (Token.Delim '|') t)
           t ->
      let comb = read_combinator t in
      Cursor.ws t;
      combine left comb (read_complex t)
  | Some ('>' | '+' | '~') ->
      let comb = read_combinator t in
      Cursor.ws t;
      combine left comb (read_complex t)
  | _ ->
      if Cursor.peek_comma t || Cursor.is_done t then left
      else if can_start_selector () then
        combine left Descendant (read_complex t)
      else left

let read_selector_list t =
  Cursor.with_context t "list" @@ fun () ->
  Cursor.ws t;
  (* Parse the selector list manually to properly handle trailing commas *)
  let rec parse_list acc =
    let sel = read_complex t in
    let acc = sel :: acc in
    Cursor.ws t;
    if Cursor.comma_opt t then (
      Cursor.ws t;
      (* After a comma, we must have another selector - trailing commas are
         invalid *)
      parse_list acc)
    else List.rev acc
  in
  let selectors = parse_list [] in
  match selectors with [ s ] -> s | selectors -> List selectors

let read t =
  let selector = read_selector_list t in
  (* Ensure we've consumed all input - any remaining non-whitespace is an
     error *)
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "unexpected characters after selector";
  selector

let read_relative t =
  let selectors = read_relative_selector_list t in
  Cursor.ws t;
  if not (Cursor.is_done t) then
    Cursor.err t "unexpected characters after selector";
  match selectors with [ s ] -> s | _ -> List selectors

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

let legacy_elem (ctx : Pp.ctx) name =
  if ctx.minify then Pp.string ctx (":" ^ name) else Pp.string ctx ("::" ^ name)

let func ctx name pp_content value =
  pp_func ctx ~prefix:":" name pp_content value

let elem_func ctx name pp_content value =
  pp_func ctx ~prefix:"::" name pp_content value

let pp_combinator ctx = function
  | Descendant -> Pp.space ctx ()
  | Child -> pp_token ctx ">"
  | Next_sibling -> pp_token ctx "+"
  | Subsequent_sibling -> pp_token ctx "~"
  | Column -> pp_token ctx "||"

let strs ctx strings = Pp.list ~sep:Pp.comma Pp.string ctx strings

(** Escape a class or ID name for use inside a selector, following CSS section
    9.1 rules: hex-escape control bytes and leading digits (or a leading dash
    followed by a digit), and backslash-escape the punctuation characters that
    otherwise terminate or reframe the selector. *)
let escape_selector_name name =
  if String.length name = 0 then ""
  else
    let buf = Buffer.create (String.length name * 2) in
    let hex_digits = "0123456789abcdef" in
    let hex_escape c =
      let code = Char.code c in
      Buffer.add_char buf '\\';
      if code >= 0x10 then Buffer.add_char buf hex_digits.[code lsr 4];
      Buffer.add_char buf hex_digits.[code land 0xF];
      Buffer.add_char buf ' '
    in
    let first_char = name.[0] in
    let first_needs_hex_escape =
      (first_char >= '0' && first_char <= '9')
      || first_char = '-'
         && String.length name > 1
         && name.[1] >= '0'
         && name.[1] <= '9'
    in
    String.iteri
      (fun i c ->
        if i = 0 && first_needs_hex_escape then hex_escape c
        else
          match c with
          | '\x00' .. '\x1F' | '\x7F' -> hex_escape c
          | '[' -> Buffer.add_string buf "\\["
          | ']' -> Buffer.add_string buf "\\]"
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
          | c -> Buffer.add_char buf c)
      name;
    Buffer.contents buf

(** Pretty print nth function with optional "of" clause *)
let rec pp_nth_func ctx name expr of_sel =
  Pp.char ctx ':';
  Pp.string ctx name;
  Pp.char ctx '(';
  pp_nth ctx expr;
  (match of_sel with
  | Some sels ->
      Pp.string ctx " of ";
      Pp.list ~sep:Pp.comma pp ctx sels
  | None -> ());
  Pp.char ctx ')'

and sels ctx selectors = Pp.list ~sep:Pp.comma pp ctx selectors

and pp : t Pp.t =
 fun ctx -> function
  | Element (ns, name) ->
      Pp.option pp_ns ctx ns;
      Pp.string ctx name
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
  (* Legacy pseudo-elements (use single colon in minified mode) *)
  | Before -> legacy_elem ctx "before"
  | After -> legacy_elem ctx "after"
  | First_letter -> legacy_elem ctx "first-letter"
  | First_line -> legacy_elem ctx "first-line"
  (* Modern double-colon pseudo-elements *)
  | Backdrop -> elem ctx "backdrop"
  | Marker -> elem ctx "marker"
  | Placeholder -> elem ctx "placeholder"
  | Selection -> elem ctx "selection"
  | File_selector_button -> elem ctx "file-selector-button"
  (* Vendor-specific pseudo-classes *)
  | Moz_focusring -> vendor ctx "moz-focusring"
  | Webkit_any -> vendor ctx "webkit-any"
  | Webkit_autofill -> vendor ctx "webkit-autofill"
  | Moz_ui_invalid -> vendor ctx "moz-ui-invalid"
  | Moz_ui_valid -> vendor ctx "moz-ui-valid"
  (* Vendor-specific pseudo-elements *)
  | Moz_placeholder -> vendor_elem ctx "moz-placeholder"
  | Webkit_input_placeholder -> vendor_elem ctx "webkit-input-placeholder"
  | Ms_input_placeholder -> vendor_elem ctx "ms-input-placeholder"
  | Webkit_scrollbar -> vendor_elem ctx "webkit-scrollbar"
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
  | Part idents -> elem_func ctx "part" (Pp.list ~sep:Pp.comma Pp.string) idents
  | Slotted selectors -> elem_func ctx "slotted" sels selectors
  | Cue selectors -> elem_func ctx "cue" sels selectors
  | Cue_region selectors -> elem_func ctx "cue-region" sels selectors
  (* Functional pseudo-classes *)
  | Is selectors -> func ctx "is" sels selectors
  | Where selectors -> func ctx "where" sels selectors
  | Not selectors -> func ctx "not" sels selectors
  | Has selectors -> func ctx "has" sels selectors
  | Nth_child (expr, of_sel) -> pp_nth_func ctx "nth-child" expr of_sel
  | Nth_last_child (expr, of_sel) ->
      pp_nth_func ctx "nth-last-child" expr of_sel
  | Nth_of_type (expr, of_sel) -> pp_nth_func ctx "nth-of-type" expr of_sel
  | Nth_last_of_type (expr, of_sel) ->
      pp_nth_func ctx "nth-last-of-type" expr of_sel
  | Dir dir -> func ctx "dir" Pp.string dir
  | Lang langs -> func ctx "lang" strs langs
  | State name -> func ctx "state" Pp.string name
  | Host None -> pseudo ctx "host"
  | Host (Some selectors) -> func ctx "host" sels selectors
  | Host_context selectors -> func ctx "host-context" sels selectors
  | Heading -> Pp.string ctx ":heading()"
  | Active_view_transition_type None ->
      Pp.string ctx ":active-view-transition-type()"
  | Active_view_transition_type (Some t) ->
      func ctx "active-view-transition-type" strs t
  | Highlight names -> elem_func ctx "highlight" strs names
  | View_transition_group name ->
      elem_func ctx "view-transition-group" Pp.string name
  | View_transition_image_pair name ->
      elem_func ctx "view-transition-image-pair" Pp.string name
  | View_transition_old name ->
      pp_func ctx ~prefix:"::" "view-transition-old" Pp.string name
  | View_transition_new name ->
      pp_func ctx ~prefix:"::" "view-transition-new" Pp.string name
  | Compound selectors -> List.iter (pp ctx) selectors
  | Combined (left, comb, right) ->
      pp ctx left;
      pp_combinator ctx comb;
      pp ctx right
  | Relative (comb, right) ->
      pp_combinator ctx comb;
      pp ctx right
  | List selectors -> Pp.list ~sep:Pp.comma pp ctx selectors
  | Nesting -> Pp.char ctx '&'

let to_string ?minify t = Pp.to_string ?minify pp t
let to_buffer ?minify buf t = Pp.to_buffer ?minify buf pp t

(** Recursively map over all selectors in the tree *)
let rec map f = function
  | Combined (left, combinator, right) ->
      let left' = map f left in
      let right' = map f right in
      f (Combined (left', combinator, right'))
  | Relative (combinator, right) ->
      let right' = map f right in
      f (Relative (combinator, right'))
  | Compound selectors ->
      let selectors' = List.map (map f) selectors in
      f (Compound selectors')
  | Where selectors ->
      let selectors' = List.map (map f) selectors in
      f (Where selectors')
  | Is selectors ->
      let selectors' = List.map (map f) selectors in
      f (Is selectors')
  | Not selectors ->
      let selectors' = List.map (map f) selectors in
      f (Not selectors')
  | Has selectors ->
      let selectors' = List.map (map f) selectors in
      f (Has selectors')
  | List selectors ->
      let selectors' = List.map (map f) selectors in
      f (List selectors')
  | Nth_child (nth, Some selectors) ->
      let selectors' = List.map (map f) selectors in
      f (Nth_child (nth, Some selectors'))
  | Nth_last_child (nth, Some selectors) ->
      let selectors' = List.map (map f) selectors in
      f (Nth_last_child (nth, Some selectors'))
  | Nth_of_type (nth, Some selectors) ->
      let selectors' = List.map (map f) selectors in
      f (Nth_of_type (nth, Some selectors'))
  | Nth_last_of_type (nth, Some selectors) ->
      let selectors' = List.map (map f) selectors in
      f (Nth_last_of_type (nth, Some selectors'))
  | Host (Some selectors) ->
      let selectors' = List.map (map f) selectors in
      f (Host (Some selectors'))
  | Host_context selectors ->
      let selectors' = List.map (map f) selectors in
      f (Host_context selectors')
  | Slotted selectors ->
      let selectors' = List.map (map f) selectors in
      f (Slotted selectors')
  | Cue selectors ->
      let selectors' = List.map (map f) selectors in
      f (Cue selectors')
  | Cue_region selectors ->
      let selectors' = List.map (map f) selectors in
      f (Cue_region selectors')
  | other -> f other

let is_ sels = Is sels
let has sels = Has sels
let not selectors = Not selectors
let nth_child ?of_ nth = Nth_child (nth, of_)
let host ?selectors () = Host selectors

(* ========================= *)
(* Analysis helpers          *)
(* ========================= *)

let rec any p = function
  | Compound xs -> List.exists (any p) xs || p (Compound xs)
  | Combined (a, comb, b) -> any p a || any p b || p (Combined (a, comb, b))
  | Relative (comb, b) -> any p b || p (Relative (comb, b))
  | List xs -> List.exists (any p) xs || p (List xs)
  | Is xs | Where xs | Not xs | Has xs | Slotted xs | Cue xs | Cue_region xs ->
      List.exists (any p) xs || p (List xs)
  | Part xs -> p (Part xs)
  | Nth_child (_, Some xs)
  | Nth_last_child (_, Some xs)
  | Nth_of_type (_, Some xs)
  | Nth_last_of_type (_, Some xs) ->
      List.exists (any p) xs || p (List xs)
  | s -> p s

let has_focus sel = any (function Focus -> true | _ -> false) sel

let has_focus_within sel =
  any (function Focus_within -> true | _ -> false) sel

let has_focus_visible sel =
  any (function Focus_visible -> true | _ -> false) sel

let has_pseudo_element sel =
  any
    (function
      | Before | After | First_letter | First_line | Backdrop | Marker
      | Placeholder | Selection | File_selector_button ->
          true
      | _ -> false)
    sel

let exists_class pred sel =
  any (function Class name -> pred name | _ -> false) sel

let rec first_class = function
  | Class n -> Some n
  | Compound xs -> List.find_map first_class xs
  | Combined (a, _, _) -> first_class a
  | List (h :: _) -> first_class h
  | Is xs | Where xs | Not xs | Has xs | Slotted xs | Cue xs | Cue_region xs
    -> (
      match xs with [] -> None | h :: _ -> first_class h)
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
  | Is xs | Not xs | Has xs | Slotted xs | Cue xs | Cue_region xs ->
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
  | Is xs | Not xs | Has xs | Slotted xs | Cue xs | Cue_region xs ->
      List.exists has_peer_marker xs
  | _ -> false

(** Check if selector uses the :is(:where(...)) pattern used by group-* and
    peer-* variants. *)
let has_is_where_pattern sel = has_group_marker sel || has_peer_marker sel

(** Check if a pseudo-class is a "newer" one with limited browser support. These
    should not be combined in selector lists with :is(:where()) variants because
    if the browser doesn't support the pseudo-class, the entire rule would be
    dropped — whereas the :is(:where()) variant would survive on its own due to
    forgiving selector parsing. *)
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
  (* Stop recursion at forgiving selectors — :is()/:where() have forgiving
     parsing, so newer pseudo-classes inside them don't cause the whole rule to
     fail *)
  | Is _ | Where _ -> false
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
