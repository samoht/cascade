(** CSS Selectors - types and pretty printing *)

include Selector_intf

(* Sort tag passed to {!Cursor} helpers for typed errors. *)
let sort = Sort.Selector

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

let is_hex_char = Reader.is_hex

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

let read_aria_attr (c : Cursor.t) : aria_attr =
  let loc = Cursor.position c in
  let s = Cursor.ident c in
  try aria_attr_of_string s
  with Invalid_argument msg -> Error.fail_bad_selector loc msg

let pp_attr_name : attr_name Pp.t =
 fun ctx a -> Pp.string ctx (string_of_attr_name a)

let read_attr_name (c : Cursor.t) : attr_name =
  attr_name_of_string (Cursor.ident c)

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

(** Parse attribute value (quoted or unquoted) *)

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

(* ----- Cursor-based selector parser (Stage 1: internals only) -----

   All functions below take {!Cursor.t} and consume {!Component.t} values. They
   are not yet wired in; the existing [read_*] family below is still the active
   parser. Subsequent commits will route public entry points through these. *)

let pseudo_ident_table =
  pseudo_class_base_idents @ pseudo_element_legacy_idents
  @ pseudo_element_modern_idents @ pseudo_vendor_idents

(* Convenience helpers used in dispatchers. *)
let is_simple_start_kind : Token.kind -> bool = function
  | Token.Delim ('.' | '*' | '&') | Token.Hash _ | Token.Colon | Token.Ident _
    ->
      true
  | _ -> false

let starts_simple_selector (c : Cursor.t) : bool =
  match Cursor.peek_raw c with
  | Some (Component.Preserved tok) -> is_simple_start_kind tok.kind
  | Some (Component.Block { node = { opening = Token.Square; _ }; _ }) -> true
  | _ -> false

let parse_class (c : Cursor.t) : t =
  Cursor.expect '.' c;
  Class (Cursor.ident c)

let parse_id (c : Cursor.t) : t =
  match Cursor.hash_opt c with
  | Some name -> Id name
  | None -> Cursor.err_unexpected c

(* Per Selectors 4: the explicit combinators are [>], [+], [~], [||]; whitespace
   by itself is the descendant combinator. Empty input has no combinator at all
   and is an error. *)
let read_combinator (c : Cursor.t) : combinator =
  let had_ws = Cursor.skip_ws c in
  if Cursor.try_kind_pair (Token.Delim '|') (Token.Delim '|') c then Column
  else if Cursor.try_kind (Token.Delim '>') c then Child
  else if Cursor.try_kind (Token.Delim '+') c then Next_sibling
  else if Cursor.try_kind (Token.Delim '~') c then Subsequent_sibling
  else
    match Cursor.peek_raw c with
    | Some (Component.Preserved { kind = Token.Delim d; loc }) ->
        Error.fail_unexpected_token loc ~sort (Token.Delim d)
    | None when had_ws -> Descendant
    | None -> Error.fail_missing_token (Cursor.position c) ~sort "combinator"
    | Some cv ->
        Error.fail_missing_token (Component.source_loc cv) ~sort "combinator"

(* Inside a [attr ... ] block. The value is one Preserved component: ident,
   string, number, hash, dimension, etc. *)
let parse_attribute_value (c : Cursor.t) : string =
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.String s; _ }) ->
      Cursor.skip c;
      s
  | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
      Cursor.skip c;
      s
  | Some (Component.Preserved { kind = Token.Number_tok { repr; _ }; _ }) ->
      Cursor.skip c;
      repr
  | Some (Component.Preserved { kind = Token.Hash { value; _ }; _ }) ->
      Cursor.skip c;
      "#" ^ value
  | _ -> Cursor.err_unexpected c

let read_attribute_match (c : Cursor.t) : attribute_match =
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Delim '='; _ }) ->
      Cursor.skip c;
      Exact (parse_attribute_value c)
  | Some
      (Component.Preserved
         { kind = Token.Delim (('~' | '|' | '^' | '$' | '*') as op); _ }) -> (
      let snap = Cursor.save c in
      Cursor.skip c;
      match Cursor.peek_raw c with
      | Some (Component.Preserved { kind = Token.Delim '='; _ }) -> (
          let (_ : Component.t option) = Cursor.next_raw c in
          let v = parse_attribute_value c in
          match op with
          | '~' -> Whitespace_list v
          | '|' -> Hyphen_list v
          | '^' -> Prefix v
          | '$' -> Suffix v
          | '*' -> Substring v
          | _ -> assert false)
      | _ ->
          Cursor.restore c snap;
          Presence)
  | _ -> Presence

let read_attr_flag (c : Cursor.t) : attr_flag option =
  let snap = Cursor.save c in
  match Cursor.ident_opt c with
  | Some "i" -> Some Case_insensitive
  | Some "s" -> Some Case_sensitive
  | Some _ ->
      Cursor.restore c snap;
      None
  | None -> None

(* `prefix|name`, `*|name`, or no prefix. Disambiguates from `name|=...`
   (attribute match) using snapshot/restore. *)
let read_ns (c : Cursor.t) : ns option =
  let snap = Cursor.save c in
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Delim '*'; _ }) -> (
      Cursor.skip c;
      match Cursor.peek_raw c with
      | Some (Component.Preserved { kind = Token.Delim '|'; _ }) ->
          let (_ : Component.t option) = Cursor.next_raw c in
          Some Any
      | _ ->
          Cursor.restore c snap;
          None)
  | Some (Component.Preserved { kind = Token.Ident name; _ }) -> (
      Cursor.skip c;
      match Cursor.peek_raw c with
      | Some (Component.Preserved { kind = Token.Delim '|'; _ }) -> (
          let mid_snap = Cursor.save c in
          let (_ : Component.t option) = Cursor.next_raw c in
          match Cursor.peek_raw c with
          | Some (Component.Preserved { kind = Token.Delim '='; _ }) ->
              (* It was [name|=value], not a namespace. *)
              Cursor.restore c snap;
              ignore mid_snap;
              None
          | _ -> Some (Prefix name))
      | _ ->
          Cursor.restore c snap;
          None)
  | _ -> None

let parse_attribute (c : Cursor.t) : t =
  match Cursor.peek c with
  | Some (Component.Block { node = { opening = Token.Square; value }; _ }) ->
      Cursor.skip c;
      let inner = Cursor.of_components value in
      Error.with_context "[]" @@ fun () ->
      let ns = read_ns inner in
      let attr = Cursor.ident inner in
      let matcher = read_attribute_match inner in
      let flag = read_attr_flag inner in
      Cursor.expect_eof inner;
      Attribute (ns, attr_name_of_string attr, matcher, flag)
  | _ -> Cursor.err_unexpected c

let parse_type_or_universal (c : Cursor.t) : t =
  let ns = read_ns c in
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Delim '*'; _ }) -> (
      Cursor.skip c;
      match ns with None -> universal | Some ns -> universal_ns ns)
  | _ -> (
      let name = Cursor.ident c in
      match ns with
      | None -> Element (None, name)
      | Some ns -> Element (Some ns, name))

(* An+B microsyntax. At the component level, "2n+1" is [Dimension {value=2;
   unit_="n"}, Delim '+', Number 1]; "n" alone is an Ident; "+n" is Delim '+'
   then Ident "n", etc. *)
let parse_offset (c : Cursor.t) : int =
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Delim '+'; _ }) -> (
      Cursor.skip c;
      match Cursor.integer_opt c with
      | Some n -> n
      | None -> Cursor.err_unexpected c)
  | Some (Component.Preserved { kind = Token.Delim '-'; _ }) -> (
      Cursor.skip c;
      match Cursor.integer_opt c with
      | Some n -> -n
      | None -> Cursor.err_unexpected c)
  | Some (Component.Preserved { kind = Token.Number_tok _; _ }) -> (
      match Cursor.integer_opt c with
      | Some n -> n
      | None -> Cursor.err_unexpected c)
  | _ -> 0

(* CSS lexer treats `-` as part of an ident, so `2n-1` is one
   [Dimension{value=2; unit_="n-1"}] token. Parse the trailing "-N" as a
   negative offset baked into the unit. *)
let split_n_unit unit_ : (int option, unit) result =
  if unit_ = "n" then Ok None
  else if String.length unit_ > 2 && unit_.[0] = 'n' && unit_.[1] = '-' then
    let tail = String.sub unit_ 2 (String.length unit_ - 2) in
    match int_of_string_opt tail with
    | Some n -> Ok (Some (-n))
    | None -> Error ()
  else Error ()

let read_nth (c : Cursor.t) : nth =
  let (_ : bool) = Cursor.skip_ws c in
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Ident "odd"; _ }) ->
      Cursor.skip c;
      Odd
  | Some (Component.Preserved { kind = Token.Ident "even"; _ }) ->
      Cursor.skip c;
      Even
  | Some (Component.Preserved { kind = Token.Ident "n"; _ }) ->
      Cursor.skip c;
      An_plus_b (1, parse_offset c)
  | Some (Component.Preserved { kind = Token.Ident "-n"; _ }) ->
      Cursor.skip c;
      An_plus_b (-1, parse_offset c)
  | Some (Component.Preserved { kind = Token.Dimension { number; unit_ }; loc })
    -> (
      let a = int_of_float number.value in
      match split_n_unit unit_ with
      | Ok None ->
          Cursor.skip c;
          An_plus_b (a, parse_offset c)
      | Ok (Some b) ->
          Cursor.skip c;
          An_plus_b (a, b)
      | Error () -> Error.fail_bad_selector loc ("unexpected An+B unit " ^ unit_)
      )
  | Some (Component.Preserved { kind = Token.Number_tok { value; _ }; _ }) ->
      Cursor.skip c;
      Index (int_of_float value)
  | _ -> Cursor.err_unexpected c

(* Forward refs: parse_complex_list and parse_relative_selector_list below are
   mutually recursive with the pseudo-class handlers. *)
let rec read_nth_selector (c : Cursor.t) : nth * t list option =
  let n = read_nth c in
  (n, parse_optional_of c)

and parse_complex_list (c : Cursor.t) : t list =
  let (_ : bool) = Cursor.skip_ws c in
  let first = parse_complex c in
  let rec loop acc =
    let (_ : bool) = Cursor.skip_ws c in
    if Cursor.comma_opt c then
      let (_ : bool) = Cursor.skip_ws c in
      loop (parse_complex c :: acc)
    else List.rev acc
  in
  first :: loop []

and parse_complex (c : Cursor.t) : t =
  let left = parse_compound c in
  let had_ws = Cursor.skip_ws c in
  match Cursor.peek_raw c with
  | Some (Component.Preserved { kind = Token.Delim ('>' | '+' | '~' | '|'); _ })
    ->
      let comb = read_combinator c in
      let (_ : bool) = Cursor.skip_ws c in
      combine left comb (parse_complex c)
  | Some (Component.Preserved { kind = Token.Comma; _ }) | None -> left
  | Some _ when had_ws && starts_simple_selector c ->
      combine left Descendant (parse_complex c)
  | Some _ -> left

and parse_compound (c : Cursor.t) : t =
  let (_ : bool) = Cursor.skip_ws c in
  let first = parse_simple c in
  let rec loop acc =
    if starts_simple_selector c then
      let s = parse_simple c in
      loop (s :: acc)
    else List.rev acc
  in
  match loop [] with [] -> first | rest -> compound (first :: rest)

and parse_simple (c : Cursor.t) : t =
  match Cursor.peek_raw c with
  | Some (Component.Preserved { kind = Token.Delim '.'; _ }) -> parse_class c
  | Some (Component.Preserved { kind = Token.Hash _; _ }) -> parse_id c
  | Some (Component.Block { node = { opening = Token.Square; _ }; _ }) ->
      parse_attribute c
  | Some (Component.Preserved { kind = Token.Colon; _ }) -> (
      (* `::` -> pseudo-element, `:` -> pseudo-class. *)
      let snap = Cursor.save c in
      let (_ : Component.t option) = Cursor.next_raw c in
      match Cursor.peek_raw c with
      | Some (Component.Preserved { kind = Token.Colon; _ }) ->
          Cursor.restore c snap;
          parse_pseudo_element c
      | _ ->
          Cursor.restore c snap;
          parse_pseudo_class c)
  | Some (Component.Preserved { kind = Token.Delim '*'; _ }) ->
      parse_type_or_universal c
  | Some (Component.Preserved { kind = Token.Delim '&'; _ }) ->
      let (_ : Component.t option) = Cursor.next_raw c in
      Nesting
  | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
      parse_type_or_universal c
  | _ -> Cursor.err_unexpected c

and parse_relative_selector (c : Cursor.t) : t =
  let (_ : bool) = Cursor.skip_ws c in
  match Cursor.peek c with
  | Some (Component.Preserved { kind = Token.Delim ('+' | '>' | '~'); _ }) ->
      let comb = read_combinator c in
      let (_ : bool) = Cursor.skip_ws c in
      Relative (comb, parse_complex c)
  | _ -> parse_complex c

and parse_relative_selector_list (c : Cursor.t) : t list =
  let (_ : bool) = Cursor.skip_ws c in
  let first = parse_relative_selector c in
  let rec loop acc =
    let (_ : bool) = Cursor.skip_ws c in
    if Cursor.comma_opt c then
      let (_ : bool) = Cursor.skip_ws c in
      loop (parse_relative_selector c :: acc)
    else List.rev acc
  in
  first :: loop []

and parse_pseudo_class (c : Cursor.t) : t =
  let (_ : bool) = Cursor.colon c in
  match Cursor.peek_raw c with
  | Some (Component.Func { node = { name; arguments }; loc }) -> (
      let (_ : Component.t option) = Cursor.next_raw c in
      let inner = Cursor.of_components arguments in
      Error.with_context (":" ^ name ^ "()") @@ fun () ->
      match name with
      | "is" -> Is (parse_complex_list inner)
      | "where" -> Where (parse_complex_list inner)
      | "not" -> Not (parse_complex_list inner)
      | "has" -> Has (parse_relative_selector_list inner)
      | "nth-child" ->
          let n = read_nth inner in
          Nth_child (n, parse_optional_of inner)
      | "nth-last-child" ->
          let n = read_nth inner in
          Nth_last_child (n, parse_optional_of inner)
      | "nth-of-type" ->
          let n = read_nth inner in
          Nth_of_type (n, parse_optional_of inner)
      | "nth-last-of-type" ->
          let n = read_nth inner in
          Nth_last_of_type (n, parse_optional_of inner)
      | "lang" -> Lang (parse_ident_list inner)
      | "dir" -> Dir (Cursor.ident inner)
      | "state" -> State (Cursor.ident inner)
      | "host" ->
          if Cursor.is_done inner then Host None
          else Host (Some (parse_complex_list inner))
      | "host-context" -> Host_context (parse_complex_list inner)
      | "heading" -> Heading
      | "active-view-transition-type" ->
          if Cursor.is_done inner then Active_view_transition_type None
          else Active_view_transition_type (Some (parse_ident_list inner))
      | _ ->
          Error.fail_bad_selector loc
            ("unknown functional pseudo-class :" ^ name))
  | Some (Component.Preserved { kind = Token.Ident name; loc }) -> (
      let (_ : Component.t option) = Cursor.next_raw c in
      match List.assoc_opt name pseudo_ident_table with
      | Some sel -> sel
      | None -> Error.fail_bad_selector loc ("unknown pseudo-class :" ^ name))
  | _ -> Cursor.err_unexpected c

and parse_pseudo_element (c : Cursor.t) : t =
  let (_ : bool) = Cursor.colon c in
  let (_ : bool) = Cursor.colon c in
  match Cursor.peek_raw c with
  | Some (Component.Func { node = { name; arguments }; loc }) -> (
      let (_ : Component.t option) = Cursor.next_raw c in
      let inner = Cursor.of_components arguments in
      Error.with_context ("::" ^ name ^ "()") @@ fun () ->
      match name with
      | "part" -> Part (parse_ident_list inner)
      | "slotted" -> Slotted (parse_complex_list inner)
      | "cue" -> Cue (parse_complex_list inner)
      | "cue-region" -> Cue_region (parse_complex_list inner)
      | "highlight" -> Highlight (parse_ident_list inner)
      | "view-transition-group" -> View_transition_group (Cursor.ident inner)
      | "view-transition-image-pair" ->
          View_transition_image_pair (Cursor.ident inner)
      | "view-transition-old" -> View_transition_old (Cursor.ident inner)
      | "view-transition-new" -> View_transition_new (Cursor.ident inner)
      | _ ->
          Error.fail_bad_selector loc
            ("unknown functional pseudo-element ::" ^ name))
  | Some (Component.Preserved { kind = Token.Ident name; loc }) -> (
      let (_ : Component.t option) = Cursor.next_raw c in
      match List.assoc_opt name pseudo_ident_table with
      | Some sel -> sel
      | None -> Error.fail_bad_selector loc ("unknown pseudo-element ::" ^ name)
      )
  | _ -> Cursor.err_unexpected c

and parse_optional_of (c : Cursor.t) : t list option =
  let (_ : bool) = Cursor.skip_ws c in
  let snap = Cursor.save c in
  match Cursor.ident_opt c with
  | Some "of" ->
      let (_ : bool) = Cursor.skip_ws c in
      Some (parse_complex_list c)
  | _ ->
      Cursor.restore c snap;
      None

and parse_ident_list (c : Cursor.t) : string list =
  let (_ : bool) = Cursor.skip_ws c in
  let first = Cursor.ident c in
  let rec loop acc =
    let (_ : bool) = Cursor.skip_ws c in
    if Cursor.comma_opt c then
      let (_ : bool) = Cursor.skip_ws c in
      loop (Cursor.ident c :: acc)
    else List.rev acc
  in
  first :: loop []

(* ----- End cursor-based parser internals ----- *)

(* Collect the rule prelude by driving {!Parser} over the Reader and stopping
   when the next raw char is the rule-body [{]. [Parser] does all the
   token/block grouping (quotes, attribute brackets, function calls);
   [Reader.peek] is only used as a single-char lookahead for the stop condition,
   never to re-tokenise anything. *)
let read_selector_list (c : Cursor.t) : t =
  let sels = parse_complex_list c in
  match sels with [ s ] -> s | sels -> List sels

let read (c : Cursor.t) : t =
  let sels = parse_complex_list c in
  Cursor.expect_eof c;
  match sels with [ s ] -> s | sels -> List sels

let read_relative (c : Cursor.t) : t =
  let sels = parse_relative_selector_list c in
  Cursor.expect_eof c;
  match sels with [ s ] -> s | sels -> List sels

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

(** Escape special CSS selector characters for class and ID names. This handles
    characters commonly found in Tailwind utilities like fractions (w-1/2),
    arbitrary values (p-[10px]), etc. Also handles identifiers starting with
    digits or other invalid start characters using hex escapes. *)
let escape_selector_name name =
  if String.length name = 0 then ""
  else
    let buf = Buffer.create (String.length name * 2) in
    (* Helper to convert char to hex escape *)
    let hex_escape c =
      let code = Char.code c in
      let hex_digits = "0123456789abcdef" in
      let rec to_hex n acc =
        if n = 0 then acc
        else to_hex (n / 16) (String.make 1 hex_digits.[n mod 16] ^ acc)
      in
      let hex_str = if code = 0 then "0" else to_hex code "" in
      "\\" ^ hex_str ^ " "
    in
    (* Check if first character needs special hex escaping *)
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
        (* First character gets hex escape if it's a digit or dash-digit *)
        if i = 0 && first_needs_hex_escape then
          Buffer.add_string buf (hex_escape c)
        else
          match c with
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
