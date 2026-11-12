open Syntax
open Selector_intf

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
