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

let needs_quotes value =
  String.length value = 0
  || (not (is_valid_nmstart value.[0]))
  || not (String.for_all is_valid_nmchar value)

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

(* Helper to pretty-print attribute values with minify-aware quoting. *)
let pp_attr_value : string Pp.t =
 fun ctx value ->
  if Pp.minified ctx && not (needs_quotes value) then Pp.string ctx value
  else Pp.quoted_string ctx value

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
    if c = '\\' then (
      (* Skip escaped sequence payload: either next char or up to 6 hex digits
         optionally followed by a space *)
      incr i;
      if !i < len then (
        let is_hex c =
          (c >= '0' && c <= '9')
          || (c >= 'a' && c <= 'f')
          || (c >= 'A' && c <= 'F')
        in
        let start = !i in
        let rec consume_hex n =
          if n = 6 || !i >= len then ()
          else if is_hex name.[!i] then (
            incr i;
            consume_hex (n + 1))
          else ()
        in
        consume_hex 0;
        if !i = start then incr i (* single escaped char *)
        else if !i < len && name.[!i] = ' ' then incr i))
    else
      let idx = !i in
      let is_valid =
        if idx = 0 then
          (* Allow - at start for vendor prefixes, but other rules still
             apply *)
          is_valid_nmstart c || c = '-'
        else is_valid_nmchar c
      in
      if (not is_valid) && Char.code c <= 127 then
        (* Only validate ASCII, allow non-ASCII *)
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

let class_ name =
  validate_css_identifier name;
  Class name

let id name =
  validate_css_identifier name;
  Id name

let universal = Universal None
let universal_ns ns = Universal (Some ns)

let attribute ?ns ?flag name match_type =
  validate_css_identifier name;
  Attribute (ns, name, match_type, flag)

(* Simple readers that don't need recursion *)
let read_lang_content t =
  Lang (Reader.list ~sep:Reader.comma ~at_least:1 Reader.ident t)

let read_dir_content t = Dir (Reader.ident t)
let read_state_content t = State (Reader.ident t)
let read_heading_content _t = Heading

let read_active_view_transition_type_content t =
  Active_view_transition_type
    (Reader.option (Reader.list ~sep:Reader.comma ~at_least:1 Reader.ident) t)

let read_lang t = Reader.call "lang" t read_lang_content
let read_dir t = Reader.call "dir" t read_dir_content
let read_state t = Reader.call "state" t read_state_content
let read_heading t = Reader.call "heading" t read_heading_content

let read_active_view_transition_type t =
  Reader.call "active-view-transition-type" t
    read_active_view_transition_type_content

let read_part_content t =
  let idents = Reader.list ~sep:Reader.comma ~at_least:1 Reader.ident t in
  Part idents

let read_part t = Reader.call "part" t read_part_content

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
let compound selectors = Compound selectors
let err_expected t what = Reader.err_expected t what

(** Parse attribute value (quoted or unquoted) *)
let read_attribute_value t =
  (* Try quoted string first, fallback to unquoted identifier *)
  Reader.option Reader.string t
  |> Option.value
       ~default:
         (Reader.while_ t (fun c ->
              c <> ']' && c <> ' ' && c <> '\t' && c <> '\n'))

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
    Reader.err t clean_msg

(** Parse a class selector (.classname) *)
let read_class t =
  Reader.expect '.' t;
  let name = Reader.ident ~keep_case:true t in
  validate_css_identifier_with_reader t name;
  Class name

(** Parse an ID selector (#id) *)
let read_id t =
  Reader.expect '#' t;
  let name = Reader.ident ~keep_case:true t in
  validate_css_identifier_with_reader t name;
  Id name

(** Parse a namespaced type or universal selector *)
let read_type_or_universal t =
  (* Try to read namespace prefix first *)
  let ns =
    Reader.option
      (fun t ->
        if Reader.looking_at t "*|" then (
          Reader.expect_string "*|" t;
          Any)
        else
          let p = Reader.ident ~keep_case:true t in
          Reader.expect '|' t;
          Prefix p)
      t
  in

  (* Now read the selector itself *)
  match Reader.peek t with
  | Some '*' -> (
      Reader.skip t;
      match ns with None -> universal | Some ns -> universal_ns ns)
  | _ -> (
      let name = Reader.ident ~keep_case:true t in
      validate_css_identifier_with_reader t name;
      match ns with
      | None -> Element (None, name)
      | Some ns -> Element (Some ns, name))

(** Parse attribute selector [attr] or [attr=value] *)
let read_combinator t =
  match Reader.peek t with
  | Some '>' ->
      Reader.skip t;
      Child
  | Some '+' ->
      Reader.skip t;
      Next_sibling
  | Some '~' ->
      Reader.skip t;
      Subsequent_sibling
  | Some '|' when Reader.looking_at t "||" ->
      Reader.expect_string "||" t;
      Column
  | Some '!' ->
      (* Invalid combinator character *)
      Reader.err t "invalid combinator character"
  | None when Reader.is_done t ->
      (* Empty input should fail in isolation *)
      Reader.err t "empty combinator"
  | _ -> Descendant

let read_attribute_match t : attribute_match =
  let two_chars = Reader.peek_string t 2 in
  if two_chars = "~=" then (
    Reader.expect_string "~=" t;
    Whitespace_list (read_attribute_value t))
  else if two_chars = "|=" then (
    Reader.expect_string "|=" t;
    Hyphen_list (read_attribute_value t))
  else if two_chars = "^=" then (
    Reader.expect_string "^=" t;
    Prefix (read_attribute_value t))
  else if two_chars = "$=" then (
    Reader.expect_string "$=" t;
    Suffix (read_attribute_value t))
  else if two_chars = "*=" then (
    Reader.expect_string "*=" t;
    Substring (read_attribute_value t))
  else if Reader.peek t = Some '=' then (
    Reader.skip t;
    Exact (read_attribute_value t))
  else Presence

let read_ns t : ns option =
  Reader.option
    (fun t ->
      if Reader.looking_at t "*|" then (
        Reader.expect_string "*|" t;
        Any)
      else
        let p = Reader.ident ~keep_case:true t in
        (* Avoid treating '|=' as a namespace separator *)
        if Reader.peek_string t 2 = "|=" then Reader.err t "not a namespace";
        (* Check if we're at end of input - this should fail *)
        if Reader.is_done t then
          Reader.err t "expected | after namespace prefix";
        Reader.expect '|' t;
        Prefix p)
    t

let read_attr_flag t : attr_flag option =
  Reader.ws t;
  Reader.option
    (fun t ->
      match Reader.char t with
      | 'i' -> Case_insensitive
      | 's' -> Case_sensitive
      | c -> Reader.err t ~got:(String.make 1 c) "'i' or 's'")
    t

let read_attribute t =
  Reader.expect '[' t;
  Reader.ws t;
  let ns = read_ns t in
  let attr = Reader.ident ~keep_case:true t in
  Reader.ws t;
  let matcher = read_attribute_match t in
  Reader.ws t;
  let flag = read_attr_flag t in
  Reader.expect ']' t;
  attribute ?ns attr matcher ?flag

(** Read An+B microsyntax for nth expressions *)
let read_nth t : nth =
  Reader.ws t;
  (* Use enum with default case for An+B or integer fallback *)
  Reader.enum "nth expression"
    [ ("odd", Odd); ("even", Even) ]
    ~default:(fun t ->
      (* Try to parse An+B expression or fall back to integer *)
      match
        Reader.option
          (fun t ->
            (* Try to read what could be an An+B expression *)
            let rec read_an_plus_b_chars acc =
              match Reader.peek t with
              | Some ('0' .. '9' | 'n' | '+' | '-') as c ->
                  Reader.skip t;
                  read_an_plus_b_chars (acc ^ String.make 1 (Option.get c))
              | Some ('a' .. 'z' | 'A' .. 'Z')
                when acc <> "" && String.contains acc 'n' = false ->
                  (* Letter after digit sequence - might be 'n' *)
                  let c = Reader.char t in
                  if c = 'n' then read_an_plus_b_chars (acc ^ "n")
                  else Reader.err t "not an nth expression"
              | Some ' ' when acc <> "" && not (String.contains acc 'n') ->
                  (* Look ahead past spaces to see if there's an 'n' *)
                  let lookahead = Reader.peek_string t 5 in
                  let has_n_after_space =
                    let rec check i =
                      if i >= String.length lookahead then false
                      else
                        match lookahead.[i] with
                        | ' ' -> check (i + 1)
                        | 'n' | 'N' -> true
                        | _ -> false
                    in
                    check 1 (* skip current space *)
                  in
                  if has_n_after_space then
                    Reader.err t "invalid spacing in nth expression"
                  else acc
              | _ -> acc
            in
            let expr_str = read_an_plus_b_chars "" in

            if String.contains expr_str 'n' then
              (* Parse An+B form from the string we collected *)
              let len = String.length expr_str in
              let n_pos = String.index expr_str 'n' in

              (* Parse coefficient before 'n' *)
              let a =
                if n_pos = 0 then 1 (* just "n" *)
                else
                  let coef_str = String.sub expr_str 0 n_pos in
                  if coef_str = "" || coef_str = "+" then 1
                  else if coef_str = "-" then -1
                  else int_of_string coef_str
              in

              (* Parse offset after 'n' *)
              let b =
                if n_pos + 1 >= len then 0 (* no offset like "2n" *)
                else
                  let rest =
                    String.sub expr_str (n_pos + 1) (len - n_pos - 1)
                  in
                  if rest = "" then 0
                  else int_of_string rest (* Handles +/- automatically *)
              in

              An_plus_b (a, b)
            else if expr_str <> "" then
              (* Just a number without 'n' *)
              Index (int_of_string expr_str)
            else Reader.err t "empty expression")
          t
      with
      | Some result -> result
      | None ->
          (* If An+B parsing failed, check if there's suspicious remaining
             input *)
          let remaining = Reader.peek_string t 5 in
          if String.contains remaining 'n' then
            Reader.err t "malformed nth expression"
          else Index (Reader.int t))
    t

(** Pretty print nth expression *)
let pp_nth : nth Pp.t =
 fun ctx -> function
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

(* Forward declarations for mutually recursive functions *)
let rec read_complex_list t =
  Reader.ws t;
  (* Check if we have empty content right away *)
  match Reader.peek t with
  | Some ')' -> Reader.err t "expected at least one selector"
  | _ -> (
      try Reader.list ~sep:Reader.comma ~at_least:1 read_complex t
      with Reader.Parse_error _ ->
        Reader.err t "expected at least one selector")

(** Read nth selector with optional "of S" clause *)
and read_nth_selector t : nth * t list option =
  let expr = read_nth t in
  Reader.ws t;

  (* Check for "of S" clause *)
  let of_clause =
    Reader.option
      (fun t ->
        Reader.expect_string "of" t;
        Reader.ws t;
        Reader.list ~sep:Reader.comma ~at_least:1 read_complex t)
      t
  in
  (expr, of_clause)

(* Helper readers for functional pseudo-class content *)
and read_is_content t = Is (read_complex_list t)
and read_has_content t = Has (read_complex_list t)
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

and read_nth_last_of_type_content t =
  let expr, of_sel = read_nth_selector t in
  Nth_last_of_type (expr, of_sel)

and read_host_content t = Host (Reader.option read_complex_list t)
and read_host_context_content t = Host_context (read_complex_list t)

(* Read helper functions for functional pseudo-classes *)
and read_is t = Reader.call "is" t read_is_content
and read_has t = Reader.call "has" t read_has_content
and read_not t = Reader.call "not" t read_not_content
and read_where t = Reader.call "where" t read_where_content
and read_nth_child t = Reader.call "nth-child" t read_nth_child_content

and read_nth_last_child t =
  Reader.call "nth-last-child" t read_nth_last_child_content

and read_nth_of_type t = Reader.call "nth-of-type" t read_nth_of_type_content

and read_nth_last_of_type t =
  Reader.call "nth-last-of-type" t read_nth_last_of_type_content

and read_host t = Reader.call "host" t read_host_content
and read_host_context t = Reader.call "host-context" t read_host_context_content

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

and read_slotted t = Reader.call "slotted" t read_slotted_content
and read_cue t = Reader.call "cue" t read_cue_content
and read_cue_region t = Reader.call "cue-region" t read_cue_region_content

(** Parse pseudo-class (:hover, :nth-child(2n+1), etc.) *)
and read_pseudo_class t =
  Reader.expect ':' t;
  (* Use enum_or_calls to handle both simple and functional pseudo-classes *)
  Reader.enum_or_calls "pseudo-class"
    [
      (* Simple pseudo-classes *)
      ("hover", Hover);
      ("active", Active);
      ("focus", Focus);
      ("focus-visible", Focus_visible);
      ("focus-within", Focus_within);
      ("target", Target);
      ("link", Link);
      ("visited", Visited);
      ("any-link", Any_link);
      ("local-link", Local_link);
      ("target-within", Target_within);
      ("scope", Scope);
      ("root", Root);
      ("empty", Empty);
      ("first-child", First_child);
      ("last-child", Last_child);
      ("only-child", Only_child);
      ("first-of-type", First_of_type);
      ("last-of-type", Last_of_type);
      ("only-of-type", Only_of_type);
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
      ("autofill", Autofill);
      ("fullscreen", Fullscreen);
      ("modal", Modal);
      ("picture-in-picture", Picture_in_picture);
      ("left", Left);
      ("right", Right);
      ("first", First);
      ("defined", Defined);
      ("playing", Playing);
      ("paused", Paused);
      ("seeking", Seeking);
      ("buffering", Buffering);
      ("stalled", Stalled);
      ("muted", Muted);
      ("volume-locked", Volume_locked);
      ("future", Future);
      ("past", Past);
      ("current", Current);
      ("popover-open", Popover_open);
      (* :host can be used without arguments *)
      ("host", Host None);
      (* Legacy single-colon pseudo-elements *)
      ("before", Before);
      ("after", After);
      ("first-letter", First_letter);
      ("first-line", First_line);
      (* Modern double-colon pseudo-elements *)
      ("backdrop", Backdrop);
      ("marker", Marker);
      ("placeholder", Placeholder);
      ("selection", Selection);
      ("file-selector-button", File_selector_button);
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
    ]
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
  Reader.expect_string "::" t;
  let default t =
    let name = Reader.ident t in
    validate_css_identifier_with_reader t name;
    match Reader.peek t with
    | Some '(' ->
        Reader.expect '(' t;
        let inner = Reader.until t ')' in
        Reader.expect ')' t;
        Pseudo_element (name ^ "(" ^ inner ^ ")")
    | _ -> Pseudo_element name
  in
  Reader.enum_calls
    [
      ("part", read_part);
      ("slotted", read_slotted);
      ("cue", read_cue);
      ("cue-region", read_cue_region);
    ]
    ~default t

(** Parse a simple selector (one part) *)
and read_simple t =
  Reader.ws t;
  match Reader.peek t with
  | Some '.' -> read_class t
  | Some '#' -> read_id t
  | Some '[' -> read_attribute t
  | Some ':' ->
      (* Use peek2 to check for :: vs : *)
      if Reader.peek2 t = "::" then read_pseudo_element t
      else read_pseudo_class t
  | Some '*' -> read_type_or_universal t
  | Some c when Reader.is_ident_start c -> read_type_or_universal t
  | _ -> err_expected t "selector"

(** Parse a compound selector (multiple simple selectors without spaces) *)
and read_compound t =
  let rec loop acc =
    (* Check if we can parse another simple selector *)
    match Reader.peek t with
    | Some ('.' | '#' | '[' | ':' | '*') ->
        let s = read_simple t in
        loop (s :: acc)
    | Some c when Reader.is_ident_start c ->
        let s = read_simple t in
        loop (s :: acc)
    | _ -> acc
  in
  match loop [] with
  | [] -> err_expected t "at least one selector"
  | [ s ] -> s
  | selectors -> compound (List.rev selectors)

(** Parse a complex selector (with combinators) *)
and read_complex t =
  let left = read_compound t in
  Reader.ws t;
  match Reader.peek t with
  | Some '|' when Reader.looking_at t "||" ->
      let comb = read_combinator t in
      Reader.ws t;
      combine left comb (read_complex t)
  | Some ('>' | '+' | '~') ->
      let comb = read_combinator t in
      Reader.ws t;
      combine left comb (read_complex t)
  | Some ',' | Some '{' | Some ')' | Some ']' | None -> left
  | _ ->
      (* Could be descendant combinator - check if next chars form a selector *)
      let can_parse_selector =
        match Reader.peek t with
        | Some ('.' | '#' | '[' | ':' | '*') -> true
        | Some c when Reader.is_ident_start c -> true
        | _ -> false
      in
      if can_parse_selector then combine left Descendant (read_complex t)
      else left

let read_selector_list t =
  Reader.with_context t "list" @@ fun () ->
  Reader.ws t;
  (* Parse the selector list manually to properly handle trailing commas *)
  let rec parse_list acc =
    let sel = read_complex t in
    let acc = sel :: acc in
    Reader.ws t;
    if Reader.consume_if ',' t then (
      Reader.ws t;
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
  Reader.ws t;
  if not (Reader.is_done t) then
    Reader.err t "unexpected characters after selector";
  selector

(** Parse selector, return [None] on failure. *)
let read_opt t = Reader.option read t

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
      Pp.string ctx name
  | Id name ->
      Pp.char ctx '#';
      Pp.string ctx name
  | Universal ns ->
      Pp.option pp_ns ctx ns;
      Pp.char ctx '*'
  | Attribute (ns, name, match_type, flag) ->
      Pp.char ctx '[';
      Pp.option pp_ns ctx ns;
      Pp.string ctx name;
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
  (* Legacy pseudo-elements (use single colon in minified mode) *)
  | Before -> legacy_elem ctx "before"
  | After -> legacy_elem ctx "after"
  | First_letter -> pseudo ctx "first-letter"
  | First_line -> pseudo ctx "first-line"
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
  | Moz_placeholder -> vendor ctx "moz-placeholder"
  | Webkit_input_placeholder -> vendor ctx "webkit-input-placeholder"
  | Ms_input_placeholder -> vendor ctx "ms-input-placeholder"
  | Moz_ui_invalid -> vendor ctx "moz-ui-invalid"
  | Moz_ui_valid -> vendor ctx "moz-ui-valid"
  | Webkit_scrollbar -> vendor ctx "webkit-scrollbar"
  | Webkit_search_cancel_button -> vendor ctx "webkit-search-cancel-button"
  | Webkit_search_decoration -> vendor ctx "webkit-search-decoration"
  (* Custom pseudo-elements *)
  | Pseudo_element n -> elem ctx n
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
  | List selectors -> Pp.list ~sep:Pp.comma pp ctx selectors

let to_string ?minify t = Pp.to_string ?minify pp t
let is_ sels = Is sels
let has sels = Has sels
let not selectors = Not selectors
let nth_child ?of_ nth = Nth_child (nth, of_)
let nth_last_child ?of_ nth = Nth_last_child (nth, of_)
let nth_of_type ?of_ nth = Nth_of_type (nth, of_)
let nth_last_of_type ?of_ nth = Nth_last_of_type (nth, of_)
let dir direction = Dir direction
let lang languages = Lang languages
let host ?selectors () = Host selectors
let host_context selectors = Host_context selectors
let state name = State name
let heading () = Heading
let active_view_transition_type ?types () = Active_view_transition_type types
let part idents = Part idents
let slotted sels = Slotted sels
let cue sels = Cue sels
let cue_region sels = Cue_region sels
let ( && ) sel1 sel2 = compound [ sel1; sel2 ]
let ( || ) s1 s2 = combine s1 Column s2
