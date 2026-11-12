open Selector_intf
open Selector_make
open Selector_pseudo

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

let pp_aria_attr : aria_attr Pp.t =
 fun ctx a -> Pp.string ctx (string_of_aria_attr a)

let pp_attr_name : attr_name Pp.t =
 fun ctx a -> Pp.string ctx (string_of_attr_name a)

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
