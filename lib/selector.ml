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
  else (
    Pp.char ctx '"';
    Pp.string ctx value;
    Pp.char ctx '"')

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
let err_expected t what = raise (Reader.Parse_error ("expected " ^ what, t))

(** Parse attribute value (quoted or unquoted) *)
let read_attribute_value t =
  Reader.try_parse Reader.string t |> function
  | Some s -> s
  | None ->
      Reader.while_ t (fun c -> c <> ']' && c <> ' ' && c <> '\t' && c <> '\n')

(** Parse a class selector (.classname) *)
let read_class t =
  Reader.expect t '.';
  class_ (Reader.ident t)

(** Parse an ID selector (#id) *)
let read_id t =
  Reader.expect t '#';
  id (Reader.ident t)

(** Parse a namespaced type or universal selector *)
let read_type_or_universal t =
  let ns =
    match
      Reader.try_parse
        (fun t ->
          let p = Reader.ident t in
          Reader.expect t '|';
          p)
        t
    with
    | Some p -> Some (Prefix p)
    | None -> (
        match
          Reader.try_parse
            (fun t ->
              Reader.expect t '*';
              Reader.expect t '|')
            t
        with
        | Some _ -> Some Any
        | None -> None)
  in
  match Reader.peek t with
  | Some '*' -> (
      Reader.skip t;
      match ns with None -> universal | Some ns -> universal_ns ns)
  | _ -> (
      let name = Reader.ident t in
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
      Reader.expect_string t "||";
      Column
  | _ -> Descendant

let read_attribute_match t : attribute_match =
  match Reader.peek_string t 2 with
  | "~=" ->
      Reader.expect_string t "~=";
      Whitespace_list (read_attribute_value t)
  | "|=" ->
      Reader.expect_string t "|=";
      Hyphen_list (read_attribute_value t)
  | "^=" ->
      Reader.expect_string t "^=";
      Prefix (read_attribute_value t)
  | "$=" ->
      Reader.expect_string t "$=";
      Suffix (read_attribute_value t)
  | "*=" ->
      Reader.expect_string t "*=";
      Substring (read_attribute_value t)
  | _ -> (
      match Reader.peek t with
      | Some '=' ->
          Reader.skip t;
          Exact (read_attribute_value t)
      | _ -> Presence)

let read_ns t : ns option =
  match
    Reader.try_parse
      (fun t ->
        Reader.expect t '*';
        Reader.expect t '|')
      t
  with
  | Some () -> Some Any
  | None -> (
      match
        Reader.try_parse
          (fun t ->
            let p = Reader.ident t in
            (* Avoid treating '|=' as a namespace separator *)
            if Reader.peek_string t 2 = "|=" then
              raise (Reader.Parse_error ("not a namespace", t));
            Reader.expect t '|';
            p)
          t
      with
      | Some p -> Some (Prefix p)
      | None -> None)

let read_attr_flag t : attr_flag option =
  Reader.ws t;
  match Reader.peek t with
  | Some 'i' ->
      Reader.skip t;
      Some Case_insensitive
  | Some 's' ->
      Reader.skip t;
      Some Case_sensitive
  | _ -> None

let read_attribute t =
  Reader.expect t '[';
  Reader.ws t;
  let ns = read_ns t in
  let attr = Reader.ident t in
  Reader.ws t;
  let matcher = read_attribute_match t in
  Reader.ws t;
  let flag = read_attr_flag t in
  Reader.expect t ']';
  attribute ?ns attr matcher ?flag

(** Parse pseudo-class (:hover, :nth-child(2n+1), etc.) *)
let rec read_pseudo_class t =
  Reader.expect t ':';
  let name = Reader.ident t in
  Reader.peek t |> function
  | Some '(' -> (
      Reader.skip t;
      let inner = Reader.until t ')' in
      Reader.expect t ')';
      (* Only selector-list functions get parsed structurally *)
      match String.lowercase_ascii name with
      | "is" | "has" | "not" | "where" ->
          fun_ name (parse_selector_list_from_string inner)
      | _ -> pseudo_class (name ^ "(" ^ inner ^ ")"))
  | _ -> pseudo_class name

(** Parse pseudo-element (::before, ::after, etc.) *)
and read_pseudo_element t =
  Reader.expect_string t "::";
  let name = Reader.ident t in
  match Reader.peek t with
  | Some '(' -> (
      Reader.skip t;
      let inner = Reader.until t ')' in
      Reader.expect t ')';
      match String.lowercase_ascii name with
      | "part" ->
          let sub = Reader.of_string inner in
          let rec loop acc =
            Reader.ws sub;
            match Reader.try_parse Reader.ident sub with
            | None -> List.rev acc
            | Some id -> (
                Reader.ws sub;
                match Reader.peek sub with
                | Some ',' ->
                    Reader.skip sub;
                    loop (id :: acc)
                | None -> List.rev (id :: acc)
                | _ -> List.rev (id :: acc))
          in
          Pseudo_element_fun_idents (name, loop [])
      | "slotted" | "cue" | "cue-region" ->
          Pseudo_element_fun (name, parse_selector_list_from_string inner)
      | _ -> Pseudo_element (name ^ "(" ^ inner ^ ")"))
  | _ -> Pseudo_element name

and parse_selector_list_from_string s =
  let sub = Reader.of_string s in
  let rec loop acc =
    Reader.ws sub;
    match Reader.try_parse read_complex sub with
    | None -> List.rev acc
    | Some sel -> (
        Reader.ws sub;
        match Reader.peek sub with
        | Some ',' ->
            Reader.comma sub;
            loop (sel :: acc)
        | None -> List.rev (sel :: acc)
        | _ -> List.rev (sel :: acc))
  in
  loop []

(** Parse a simple selector (one part) *)
and read_simple t =
  Reader.peek t |> function
  | Some '.' -> read_class t
  | Some '#' -> read_id t
  | Some '[' -> read_attribute t
  | Some ':' ->
      if Reader.looking_at t "::" then read_pseudo_element t
      else read_pseudo_class t
  | Some '*' -> read_type_or_universal t
  | Some c when Reader.is_ident_start c -> read_type_or_universal t
  | _ -> err_expected t "selector"

(** Parse a compound selector (multiple simple selectors without spaces) *)
and read_compound t =
  let rec loop acc =
    match Reader.try_parse read_simple t with
    | None -> acc
    | Some s -> loop (s :: acc)
  in
  match loop [] with
  | [] -> err_expected t "at least one selector"
  | [ s ] -> s
  | selectors -> compound (List.rev selectors)

(** Parse a complex selector (with combinators) *)
and read_complex t =
  let left = read_compound t in
  Reader.ws t;
  Reader.peek t |> function
  | Some '>' ->
      Reader.skip t;
      Reader.ws t;
      combine left Child (read_complex t)
  | Some '+' ->
      Reader.skip t;
      Reader.ws t;
      combine left Next_sibling (read_complex t)
  | Some '~' ->
      Reader.skip t;
      Reader.ws t;
      combine left Subsequent_sibling (read_complex t)
  | Some '|' when Reader.looking_at t "||" ->
      Reader.expect_string t "||";
      Reader.ws t;
      combine left Column (read_complex t)
  | Some ',' | Some '{' | None -> left
  | _ -> (
      (* Could be descendant combinator *)
      Reader.try_parse read_complex t
      |> function
      | Some right -> combine left Descendant right
      | None -> left)

let read t =
  Reader.ws t;
  let first = read_complex t in
  Reader.ws t;
  let rec loop acc =
    match Reader.peek t with
    | Some ',' ->
        Reader.comma t;
        let next = read_complex t in
        Reader.ws t;
        loop (next :: acc)
    | _ -> List.rev acc
  in
  match loop [ first ] with [ s ] -> s | selectors -> List selectors

(** Parse selector, return [None] on failure. *)
let read_opt t = Reader.try_parse read t

let rec pp : t Pp.t =
 fun ctx -> function
  | Element (ns, name) ->
      (match ns with Some ns -> pp_ns ctx ns | None -> ());
      Pp.string ctx name
  | Class name ->
      Pp.char ctx '.';
      Pp.string ctx name
  | Id name ->
      Pp.char ctx '#';
      Pp.string ctx name
  | Universal ns ->
      (match ns with Some ns -> pp_ns ctx ns | None -> ());
      Pp.char ctx '*'
  | Attribute (ns, name, match_type, flag) ->
      Pp.char ctx '[';
      (match ns with Some ns -> pp_ns ctx ns | None -> ());
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
      let pp_header ctx () =
        Pp.string ctx "::";
        Pp.string ctx name
      in
      pp_header ctx ();
      Pp.char ctx '(';
      Pp.list ~sep:Pp.comma pp ctx selectors;
      Pp.char ctx ')'
  | Pseudo_element_fun_idents (name, idents) ->
      let strict_comma ctx () = Pp.char ctx ',' in
      let pp_header ctx () =
        Pp.string ctx "::";
        Pp.string ctx name
      in
      pp_header ctx ();
      Pp.char ctx '(';
      Pp.list ~sep:strict_comma Pp.string ctx idents;
      Pp.char ctx ')'
  | Where selectors ->
      Pp.string ctx ":where(";
      Pp.list ~sep:Pp.comma pp ctx selectors;
      Pp.char ctx ')'
  | Not selectors ->
      Pp.string ctx ":not(";
      Pp.list ~sep:Pp.comma pp ctx selectors;
      Pp.char ctx ')'
  | Fun (name, selectors) ->
      Pp.char ctx ':';
      Pp.string ctx name;
      Pp.char ctx '(';
      Pp.list ~sep:Pp.comma pp ctx selectors;
      Pp.char ctx ')'
  | Compound selectors -> List.iter (pp ctx) selectors
  | Combined (left, comb, right) ->
      pp ctx left;
      pp_combinator ctx comb;
      pp ctx right
  | List selectors -> Pp.list ~sep:Pp.comma pp ctx selectors

and pp_combinator ctx = function
  | Descendant -> Pp.space ctx ()
  | Child -> if ctx.minify then Pp.char ctx '>' else Pp.string ctx " > "
  | Next_sibling -> if ctx.minify then Pp.char ctx '+' else Pp.string ctx " + "
  | Subsequent_sibling ->
      if ctx.minify then Pp.char ctx '~' else Pp.string ctx " ~ "
  | Column ->
      if ctx.minify then (
        Pp.char ctx '|';
        Pp.char ctx '|')
      else Pp.string ctx " || "

let to_string ?minify t = Pp.to_string ?minify pp t

(* nth helpers *)
let pp_nth ctx = function
  | Even -> Pp.string ctx "even"
  | Odd -> Pp.string ctx "odd"
  | An_plus_b (a, b) ->
      (* Special case: if a=0, just print the constant b *)
      if a = 0 then Pp.int ctx b
      else (
        (match a with 1 -> () | -1 -> Pp.char ctx '-' | _ -> Pp.int ctx a);
        Pp.char ctx 'n';
        if b <> 0 then (
          if b > 0 then Pp.char ctx '+';
          Pp.int ctx b))

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

let read_nth t : nth =
  Reader.ws t;
  match
    Reader.try_parse
      (fun t ->
        let s = Reader.ident t in
        String.lowercase_ascii s)
      t
  with
  | Some "even" -> Even
  | Some "odd" -> Odd
  | Some _ | None ->
      (* Parse An+B form; a may be +/-1 or omitted when just 'n' *)
      let sign_a =
        Reader.try_parse
          (fun t ->
            match Reader.char t with
            | '+' -> 1
            | '-' -> -1
            | _ -> err_expected t "sign")
          t
      in
      let a =
        match Reader.try_parse Reader.int t with
        | Some n -> ( match sign_a with Some s -> s * n | None -> n)
        | None -> ( match sign_a with Some 1 -> 1 | Some -1 -> -1 | _ -> 1)
      in
      Reader.expect t 'n';
      let b =
        match
          Reader.try_parse
            (fun t ->
              Reader.ws t;
              match Reader.char t with
              | '+' -> 1
              | '-' -> -1
              | _ -> err_expected t "+ or -")
            t
        with
        | None -> 0
        | Some sb ->
            Reader.ws t;
            let n = Reader.int t in
            sb * n
      in
      An_plus_b (a, b)

let ( && ) sel1 sel2 = compound [ sel1; sel2 ]
let ( || ) s1 s2 = combine s1 Column s2
let not selectors = Not selectors
