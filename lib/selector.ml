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

let pseudo_class name =
  (* Skip validation for functional pseudo-classes that contain parentheses *)
  if not (String.contains name '(') then validate_css_identifier name;
  Pseudo_class name

let pseudo_element name =
  validate_css_identifier name;
  Pseudo_element name

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
let fun_ name selectors = Fun (name, selectors)
let list selectors = List selectors
let is_compound_list = function List _ -> true | _ -> false
let compound selectors = Compound selectors

let err_expected t what =
  raise (Reader.Parse_error ("expected " ^ what, None, t))

(** Parse attribute value (quoted or unquoted) *)
let read_attribute_value t =
  (* Try quoted string first, fallback to unquoted identifier *)
  Reader.option Reader.string t
  |> Option.value
       ~default:
         (Reader.while_ t (fun c ->
              c <> ']' && c <> ' ' && c <> '\t' && c <> '\n'))

(** Parse a class selector (.classname) *)
let read_class t =
  Reader.expect '.' t;
  class_ (Reader.ident ~keep_case:true t)

(** Parse an ID selector (#id) *)
let read_id t =
  Reader.expect '#' t;
  id (Reader.ident ~keep_case:true t)

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
      match ns with None -> element name | Some ns -> element ~ns name)

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
        if Reader.peek_string t 2 = "|=" then
          raise (Reader.Parse_error ("not a namespace", None, t));
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
      | None -> Index (Reader.int t))
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

(** Read nth selector with optional "of S" clause *)
let rec read_nth_selector t : nth * t list option =
  let expr = read_nth t in
  Reader.ws t;

  (* Check for "of S" clause *)
  let of_clause =
    Reader.option
      (fun t ->
        Reader.expect_string "of" t;
        Reader.ws t;
        Reader.list ~sep:Reader.comma read_complex t)
      t
  in
  (expr, of_clause)

(** Parse pseudo-class (:hover, :nth-child(2n+1), etc.) *)
and read_pseudo_class t =
  Reader.expect ':' t;
  (* Dispatch on functional pseudo-classes via enum_calls; default handles both
     unknown functional and non-functional cases. *)
  let default t =
    let name = Reader.ident t in
    match Reader.peek t with
    | Some '(' ->
        Reader.expect '(' t;
        let inner = Reader.until t ')' in
        Reader.expect ')' t;
        pseudo_class (name ^ "(" ^ inner ^ ")")
    | _ -> pseudo_class name
  in
  Reader.enum_calls
    [
      ( "is",
        fun t ->
          let sels = read_selector_list t in
          fun_ "is" sels );
      ( "has",
        fun t ->
          let sels = read_selector_list t in
          fun_ "has" sels );
      ( "not",
        fun t ->
          let sels = read_selector_list t in
          fun_ "not" sels );
      ( "where",
        fun t ->
          let sels = read_selector_list t in
          where sels );
      ( "nth-child",
        fun t ->
          let expr, of_sel = read_nth_selector t in
          Nth_child (expr, of_sel) );
      ( "nth-last-child",
        fun t ->
          let expr, of_sel = read_nth_selector t in
          Nth_last_child (expr, of_sel) );
      ( "nth-of-type",
        fun t ->
          let expr, of_sel = read_nth_selector t in
          Nth_of_type (expr, of_sel) );
      ( "nth-last-of-type",
        fun t ->
          let expr, of_sel = read_nth_selector t in
          Nth_last_of_type (expr, of_sel) );
    ]
    ~default t

(** Parse pseudo-element (::before, ::after, etc.) *)
and read_pseudo_element t =
  Reader.expect_string "::" t;
  let default t =
    let name = Reader.ident t in
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
      ( "part",
        fun t ->
          let idents = Reader.list ~sep:Reader.comma Reader.ident t in
          Pseudo_element_fun_idents ("part", idents) );
      ( "slotted",
        fun t ->
          let sels = read_selector_list t in
          Pseudo_element_fun ("slotted", sels) );
      ( "cue",
        fun t ->
          let sels = read_selector_list t in
          Pseudo_element_fun ("cue", sels) );
      ( "cue-region",
        fun t ->
          let sels = read_selector_list t in
          Pseudo_element_fun ("cue-region", sels) );
    ]
    ~default t

(* Parse a selector list directly from the current reader *)
and read_selector_list t = Reader.list ~sep:Reader.comma read_complex t

(** Parse a simple selector (one part) *)
and read_simple t =
  let read_class_sel t = read_class t in
  let read_id_sel t = read_id t in
  let read_attr_sel t = read_attribute t in
  let read_pseudo_element_sel t = read_pseudo_element t in
  let read_pseudo_class_sel t = read_pseudo_class t in
  let read_type_sel t = read_type_or_universal t in

  Reader.one_of
    [
      read_class_sel;
      read_id_sel;
      read_attr_sel;
      read_pseudo_element_sel;
      read_pseudo_class_sel;
      read_type_sel;
    ]
    t

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
  Reader.ws t;
  let selectors = Reader.list ~at_least:1 ~sep:Reader.comma read_complex t in
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

let pp_combinator ctx = function
  | Descendant -> Pp.space ctx ()
  | Child -> pp_token ctx ">"
  | Next_sibling -> pp_token ctx "+"
  | Subsequent_sibling -> pp_token ctx "~"
  | Column -> pp_token ctx "||"

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
  | Pseudo_class name ->
      Pp.char ctx ':';
      Pp.string ctx name
  | Pseudo_element name ->
      Pp.string ctx "::";
      Pp.string ctx name
  | Pseudo_element_fun (name, selectors) ->
      pp_func ctx ~prefix:"::" name (Pp.list ~sep:Pp.comma pp) selectors
  | Pseudo_element_fun_idents (name, idents) ->
      let strict_comma ctx () = Pp.char ctx ',' in
      pp_func ctx ~prefix:"::" name (Pp.list ~sep:strict_comma Pp.string) idents
  | Where selectors ->
      pp_func ctx ~prefix:":" "where" (Pp.list ~sep:Pp.comma pp) selectors
  | Not selectors ->
      pp_func ctx ~prefix:":" "not" (Pp.list ~sep:Pp.comma pp) selectors
  | Fun (name, selectors) ->
      pp_func ctx ~prefix:":" name (Pp.list ~sep:Pp.comma pp) selectors
  | Nth_child (expr, of_sel) -> pp_nth_func ctx "nth-child" expr of_sel
  | Nth_last_child (expr, of_sel) ->
      pp_nth_func ctx "nth-last-child" expr of_sel
  | Nth_of_type (expr, of_sel) -> pp_nth_func ctx "nth-of-type" expr of_sel
  | Nth_last_of_type (expr, of_sel) ->
      pp_nth_func ctx "nth-last-of-type" expr of_sel
  | Compound selectors -> List.iter (pp ctx) selectors
  | Combined (left, comb, right) ->
      pp ctx left;
      pp_combinator ctx comb;
      pp ctx right
  | List selectors -> Pp.list ~sep:Pp.comma pp ctx selectors

let to_string ?minify t = Pp.to_string ?minify pp t

(* Simple helpers, reusing the pp_nth defined above *)
let is_ sels = Fun ("is", sels)
let has sels = Fun ("has", sels)
let nth_to_string nth = Pp.to_string pp_nth nth

let nth_fun name ?of_ nth =
  let nth_str = nth_to_string nth in
  match of_ with
  | None -> Pseudo_class (name ^ "(" ^ nth_str ^ ")")
  | Some sels ->
      let sels_str = Pp.to_string (Pp.list ~sep:Pp.comma pp) sels in
      Pseudo_class (name ^ "(" ^ nth_str ^ " of " ^ sels_str ^ ")")

let nth_child ?of_ nth = nth_fun "nth-child" ?of_ nth
let nth_last_child ?of_ nth = nth_fun "nth-last-child" ?of_ nth
let nth_of_type ?of_ nth = nth_fun "nth-of-type" ?of_ nth
let nth_last_of_type ?of_ nth = nth_fun "nth-last-of-type" ?of_ nth
let pseudo_element_fun name sels = Pseudo_element_fun (name, sels)
let part idents = Pseudo_element_fun_idents ("part", idents)
let slotted sels = pseudo_element_fun "slotted" sels
let cue sels = pseudo_element_fun "cue" sels
let cue_region sels = pseudo_element_fun "cue-region" sels
let ( && ) sel1 sel2 = compound [ sel1; sel2 ]
let ( || ) s1 s2 = combine s1 Column s2
let not selectors = Not selectors
