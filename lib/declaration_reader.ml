open Declaration_intf
open Common
open Properties
open Declaration_core
open Declaration_value

let is_custom_property_name = Custom_property_name.is_valid

(* CSS Syntax 3 (ED) sec. 7.2: a [<declaration-value>] is one or more component
   values with no [<bad-string-token>], no [<bad-url-token>] and no unmatched
   closing bracket. A component breaking one of those does not stay inside the
   declaration it is written into: it closes the enclosing block, or turns the
   rest of the stylesheet into a string. *)
let rec component_stays_in_declaration = function
  (* sec. 4.3.5 ends a string at EOF as the string it read and only a newline
     makes it a [<bad-string-token>], so the first stays in the value. *)
  | Component.Preserved
      { kind = Token.Close _ | Token.Bad_string | Token.Bad_url; _ } ->
      false
  | Component.Preserved _ -> true
  | Component.Block { node = { closed; value; _ }; _ } ->
      closed && List.for_all component_stays_in_declaration value
  | Component.Func { node = { terminated; arguments; _ }; _ } ->
      terminated && List.for_all component_stays_in_declaration arguments
  | Component.Flat { node; _ } ->
      Component.flat_is_valid ~require_closed:true node

(* A top-level [;] ends the declaration, so the tail becomes a second one. It is
   only a stop at top level: [(a;b)] keeps it inside the block. Sec. 7.2 keeps a
   top-level [!] out of the production for the same reason: sec. 5.5.6 takes the
   [!important] flag off the end before the value is read, so a [!] still in it
   belongs to no flag and to no value. *)
let is_top_level_stop = function
  | Component.Preserved { kind = Token.Semicolon; _ } -> true
  | _ -> false

let is_top_level_bang = function
  | Component.Preserved { kind = Token.Delim "!"; _ } -> true
  | _ -> false

let components_stay_in_declaration components =
  List.for_all
    (fun c ->
      (not (is_top_level_stop c))
      && (not (is_top_level_bang c))
      && component_stays_in_declaration c)
    components

(* CSS Variables 1 sec. 2 gives a custom property the value grammar
   [<declaration-value>?], so the empty value is one of its values. *)
let is_optional_declaration_value value =
  components_stay_in_declaration (Cursor.remaining (Cursor.of_string value))

let is_declaration_value value =
  match Cursor.remaining (Cursor.of_string value) with
  | [] -> false
  | components -> components_stay_in_declaration components

let refuse name detail =
  failwith (String.concat "" [ "custom_property: "; name; ": "; detail ])

(* CSS Syntax 3 (ED) sec. 5.5.6 discards whitespace before a declaration's value
   and again from its end, so a stream built from text carries neither. Only the
   ends go: sec. 4.1 of CSS Custom Properties 1 (ED) forbids normalizing what is
   between them. *)
let trim_declaration_value components =
  let rec drop_leading = function
    | cv :: rest when Component.is_whitespace cv -> drop_leading rest
    | kept -> kept
  in
  components |> drop_leading |> List.rev |> drop_leading |> List.rev

(* Helper for raw custom properties - primarily for internal use *)

let custom_property ?layer name value =
  (* The pair binds only when it makes the one declaration it names: a
     [<dashed-ident>] name, and the [<declaration-value>?] CSS Variables 1 sec.
     2 gives a custom property. A value outside that - a top-level [;] or [}],
     an unmatched closing bracket, an unterminated function, block or string -
     leaves the declaration as soon as the text is read back. A name carrying
     one of those is written back with the escapes that read it (CSS Syntax 3
     (ED) sec. 4.3.7), so it stays the name of this declaration. *)
  if not (is_custom_property_name name) then
    refuse name "not a custom-property name";
  if not (is_optional_declaration_value value) then
    refuse name (String.concat "" [ value; " is not a declaration value" ]);
  (* Parse the value into a CSS Syntax 3 component stream so the declaration
     never carries a raw author string; the printer can then re-serialise with
     the active [Pp] context (handles minification). *)
  let components =
    trim_declaration_value (Cursor.remaining (Cursor.of_string value))
  in
  v (Custom_property name)
    (Custom_value { value = Tokens components; layer; meta = None })

(* Access the layer associated with a custom declaration, if any *)
let rec custom_declaration_layer = function
  | Declaration
      { property = Custom_property _; value = Custom_value { layer; _ }; _ } ->
      layer
  | Declaration _ -> None
  | Theme_guarded { decl; _ } -> custom_declaration_layer decl

(* Equivalence-only normalisation for structural diffing: rewrite a quoted
   family name in a custom-property stream as the equivalent unquoted [<ident>]
   sequence, one word or several. The forms substitute identically into
   [font-family] so cascade treats them as equal, but keeps both verbatim on
   output (unquoting an opaque custom property could corrupt a [content] use).
   Gated on a generic family being present, since an unregistered property is
   otherwise type-unknown. *)
let unquote_custom_font_strings = function
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value ({ value = Tokens components; _ } as cv);
        important;
        _;
      }
    when Properties.components_have_generic_family components ->
      v ~important property
        (Custom_value
           {
             cv with
             value = Tokens (Properties.unquote_font_family_strings components);
           })
  | decl -> decl

(* Equivalence-only normalisation for structural diffing: a time token in a
   custom stream is the same time under [s] and [ms], and emission keeps the
   author's unit. *)
let canonicalize_custom_time = function
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value ({ value = Tokens components; _ } as cv);
        important;
        _;
      } ->
      v ~important property
        (Custom_value
           {
             cv with
             value = Tokens (Properties.canonicalize_time_components components);
           })
  | decl -> decl

(* Equivalence-only normalisation for structural diffing: an angle token in a
   custom stream is the same angle under [deg], [rad], [turn] and [grad], and
   emission keeps the author's unit. *)
let canonicalize_custom_angle = function
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value ({ value = Tokens components; _ } as cv);
        important;
        _;
      } ->
      v ~important property
        (Custom_value
           {
             cv with
             value =
               Tokens (Properties.canonicalize_angle_components components);
           })
  | decl -> decl

(* Equivalence-only normalisation for structural diffing: drop the whitespace of
   a custom-property stream that CSS reads as nothing. Cascade keeps that
   whitespace verbatim on output, since the stream is opaque and the author's
   bytes are what a [var()] substitutes; two streams that differ only there are
   still the same value. *)
let canonicalize_custom_whitespace = function
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value ({ value = Tokens components; _ } as cv);
        important;
        _;
      } ->
      v ~important property
        (Custom_value
           {
             cv with
             value =
               Tokens
                 (Properties.canonicalize_math_whitespace_components components);
           })
  | decl -> decl

(* Parser functions *)

(** Parse a property name. Property names are plain idents in the component
    stream ([--custom] idents include the leading [--]). *)
let read_property_name t =
  Cursor.ws t;
  Cursor.ident ~keep_case:true t

(** Parse property value. Components up to the next [;] or [!important] mark the
    value. Whitespace is preserved in the drained value so multi-token values
    like "10px 20px" serialise back with their spaces. *)
let read_property_value t =
  Cursor.with_context t "property-value" @@ fun () ->
  Cursor.consume_to_decl_end ~trim:true t

let is_decl_value_stop = function
  | Component.Preserved { kind = Token.Semicolon | Token.Delim "!"; _ } -> true
  | _ -> false

let components_before stop t =
  let rec take acc = function
    | [] -> List.rev acc
    | cv :: _ when stop cv -> List.rev acc
    | cv :: rest -> take (cv :: acc) rest
  in
  take [] (Cursor.remaining t)

let value_components t = components_before is_decl_value_stop t

let rec component_is_complete = function
  | Component.Preserved _ -> true
  | Component.Block { node = { closed; value; _ }; _ } ->
      closed && List.for_all component_is_complete value
  | Component.Func { node = { terminated; arguments; _ }; _ } ->
      terminated && List.for_all component_is_complete arguments
  | Component.Flat { node; _ } -> Component.flat_left_open node = []

let validate_complete_declaration_value t =
  if not (List.for_all component_is_complete (value_components t)) then
    Cursor.err_invalid t "unterminated component value"

let rec component_has_curly_block = function
  | Component.Block { node = { opening = Token.Curly; _ }; _ } -> true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_has_curly_block value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_has_curly_block arguments
  | Component.Flat { node; _ } ->
      List.exists
        (fun (tok : Token.t) ->
          match tok.kind with Token.Open Token.Curly -> true | _ -> false)
        node
  | Component.Preserved _ -> false

let reject_curly_block_value t =
  if List.exists component_has_curly_block (value_components t) then
    Cursor.err_invalid t "curly block in declaration value"

(* CSS Syntax 3 (ED) sec. 4.3.5 ends a string at a newline as a
   [<bad-string-token>] and at EOF as the [<string-token>] it read, and sec. 7.2
   excludes only the first from a [<declaration-value>]. So a value the input
   ended in the middle of a string carries a string like any other, and Chrome
   151 keeps the declaration. *)
let rec component_has_unterminated_string = function
  | Component.Preserved { kind = Token.Bad_string; _ } -> true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_has_unterminated_string value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_has_unterminated_string arguments
  | Component.Flat { node; _ } ->
      List.exists
        (fun (tok : Token.t) ->
          match tok.kind with Token.Bad_string -> true | _ -> false)
        node
  | Component.Preserved _ -> false

let reject_unterminated_string_value t =
  if List.exists component_has_unterminated_string (value_components t) then
    Cursor.err_invalid t "unterminated string in declaration value"

(* CSS Syntax 3 (ED) sec. 7.2: a [<declaration-value>] is any token sequence
   excluding a [<bad-string-token>], a [<bad-url-token>], and an unmatched [)],
   []] or [}]. A custom property takes [<declaration-value>?] and nothing wider,
   so a value carrying one of those is invalid rather than opaque. *)
let rec component_leaves_declaration_value = function
  | Component.Preserved
      { kind = Token.Bad_string | Token.Bad_url | Token.Close _; _ } ->
      true
  | Component.Block { node = { value; _ }; _ } ->
      List.exists component_leaves_declaration_value value
  | Component.Func { node = { arguments; _ }; _ } ->
      List.exists component_leaves_declaration_value arguments
  | Component.Flat { node; _ } ->
      not (Component.flat_is_valid ~require_closed:false node)
  | Component.Preserved _ -> false

let reject_custom_bad_string t =
  if
    List.exists component_leaves_declaration_value
      (components_before is_top_level_stop t)
  then Cursor.err_invalid t "custom property value is no <declaration-value>"

(* The same bar for a value the typed readers hand to the opaque path: a [var()]
   postpones the property grammar to computed-value time, not the token
   grammar. *)
let reject_opaque_bad_token t =
  if List.exists component_leaves_declaration_value (value_components t) then
    Cursor.err_invalid t "declaration value is no <declaration-value>"

let invalid_var_arguments arguments =
  match
    List.filter
      (fun component -> not (Component.is_whitespace component))
      (Component.shallow arguments)
  with
  | Component.Preserved { kind = Token.Ident name; _ } :: rest
    when Custom_property_name.is_valid name -> (
      (* CSS Custom Properties 1 sec. 3: nothing follows the name but a comma
         opening the fallback. This raw-component check gates the opaque
         keep-verbatim path, so it has to agree with the typed reader; without
         the comma case [var(--x 10px)] took that path and was preserved instead
         of invalidating the declaration. *)
      match rest with
      | [] -> false
      | Component.Preserved { kind = Token.Comma; _ } :: _ -> false
      | _ -> true)
  | _ -> true

(* [component_has_invalid_substitution] over a {!Component.Flat} run, in one
   pass and with no recursion: each block and function the run opens is a frame
   on [frames], and a substitution call's frame tracks how far its arguments
   have got through their grammar. *)
type call_arguments =
  | Var_name  (** [var(] waiting for its custom property name. *)
  | Var_fallback_comma  (** The name read; only a comma may follow. *)
  | Substitution_empty  (** [env(] or [attr(] with no argument yet. *)
  | Free  (** Anything goes: a fallback, an argument read, any other block. *)

let flat_has_invalid_substitution tokens =
  let frames = Stack.create () in
  let invalid = ref false in
  let argument (tok : Token.t) =
    match Stack.top_opt frames with
    | None -> ()
    | Some (_, _, state) -> (
        match (!state, tok.kind) with
        | Var_name, Token.Ident name when Custom_property_name.is_valid name ->
            state := Var_fallback_comma
        | Var_fallback_comma, Token.Comma -> state := Free
        | (Var_name | Var_fallback_comma), _ -> invalid := true
        | Substitution_empty, _ -> state := Free
        | Free, _ -> ())
  in
  let closes (b : Token.bracket) =
    match Stack.top_opt frames with
    | Some (opening, _, _) -> Token.equal_bracket opening b
    | None -> false
  in
  List.iter
    (fun (tok : Token.t) ->
      match tok.kind with
      | Token.Whitespace _ -> ()
      | Token.Close b when closes b -> (
          let _, _, state = Stack.pop frames in
          match !state with
          | Var_name | Substitution_empty -> invalid := true
          | Var_fallback_comma | Free -> ())
      | Token.Function name ->
          argument tok;
          let substitution, state =
            match String.lowercase_ascii_preserve name with
            | "var" -> (true, Var_name)
            | "env" | "attr" -> (true, Substitution_empty)
            | _ -> (false, Free)
          in
          Stack.push (Token.Paren, substitution, ref state) frames
      | Token.Open b ->
          argument tok;
          Stack.push (b, false, ref Free) frames
      | _ -> argument tok)
    tokens;
  (* A substitution call the run leaves open is unterminated. *)
  !invalid
  || Stack.fold
       (fun unterminated (_, substitution, _) -> unterminated || substitution)
       false frames

let rec components_have_invalid_substitution components =
  List.exists component_has_invalid_substitution components

and component_has_invalid_substitution = function
  | Component.Func { node = { name; arguments; terminated; _ }; _ } ->
      let invalid_call =
        match String.lowercase_ascii_preserve name with
        | "var" -> (not terminated) || invalid_var_arguments arguments
        | "env" | "attr" ->
            (not terminated)
            || not
                 (List.exists
                    (fun component -> not (Component.is_whitespace component))
                    arguments)
        | _ -> false
      in
      invalid_call || components_have_invalid_substitution arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_invalid_substitution value
  | Component.Flat { node; _ } -> flat_has_invalid_substitution node
  | Component.Preserved _ -> false

let rec components_contain_var components =
  List.exists component_contains_var components

and component_contains_var = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      String.lowercase_ascii_preserve name = "var"
      || components_contain_var arguments
  | Component.Block { node = { value; _ }; _ } -> components_contain_var value
  | Component.Flat { node; _ } -> Component.flat_calls [ "var" ] node
  | Component.Preserved _ -> false

(* CSS Values 5 (ED) sec. 11 groups [var()], [env()] and [attr()] as arbitrary
   substitution functions: each carries an unknown token stream into the value,
   so a declaration holding one is valid at parse time and only decided once
   substitution has run. A call with an empty argument list matches no
   substitution grammar and so defers nothing. *)
let is_substitution_call name arguments =
  (match String.lowercase_ascii_preserve name with
    | "var" | "env" | "attr" -> true
    | _ -> false)
  && List.exists
       (fun component -> not (Component.is_whitespace component))
       arguments

let rec components_have_substitution components =
  List.exists component_has_substitution components

and component_has_substitution = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      is_substitution_call name arguments
      || components_have_substitution arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_substitution value
  (* A call with no argument defers nothing, but it is also invalid, which
     {!flat_has_invalid_substitution} answers; this only has to see the call. *)
  | Component.Flat { node; _ } ->
      Component.flat_calls [ "var"; "env"; "attr" ] node
  | Component.Preserved _ -> false

let raw_value_has_invalid_substitution raw_value =
  Cursor.of_string raw_value |> Cursor.remaining
  |> components_have_invalid_substitution

let raw_value_contains_substitution raw_value =
  (* CSS Variables 1 sec. 3: "if a property contains one or more var()
     functions, and those functions are syntactically valid, the entire
     property's grammar must be assumed to be valid at parse time". A call
     written inside another function is one the property contains, so the
     leniency reaches the tokens around it too. *)
  Cursor.of_string raw_value |> Cursor.remaining |> components_have_substitution

(** Check for and consume [!important] (case-insensitive per CSS Syntax). *)
let read_importance t =
  Cursor.ws t;
  match Cursor.peek_delim t with
  | Some '!' ->
      Cursor.skip t;
      Cursor.ws t;
      let ident = Cursor.ident t in
      if String.lowercase_ascii_preserve ident = "important" then true
      else Cursor.err_invalid t ("invalid !important declaration: !" ^ ident)
  | _ -> false

let is_plain_property_name name =
  let is_start = function
    | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | '-' -> true
    | _ -> false
  in
  let is_continue = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  let len = String.length name in
  len > 0
  && is_start name.[0]
  && (not (len = 1 && name.[0] = '-'))
  && (not (len >= 2 && name.[0] = '-' && name.[1] >= '0' && name.[1] <= '9'))
  && String.for_all is_continue name

let validate_printable_property_name t name =
  if not (is_plain_property_name name) then
    Cursor.err_invalid t ("unprintable property name: " ^ name)

let is_vendor_extension_property_name name =
  let len = String.length name in
  let rec has_vendor_separator i =
    i < len
    &&
    if name.[i] = '-' then i > 1 && i < len - 1 else has_vendor_separator (i + 1)
  in
  len > 0 && name.[0] = '-' && has_vendor_separator 1

let validate_unknown_property_name t name =
  validate_printable_property_name t name;
  if
    String.length name > 0
    && name.[0] = '-'
    && not (is_vendor_extension_property_name name)
  then Cursor.err_invalid t ("invalid vendor extension property name: " ^ name)

(* Check if a custom property name is a font-family variable *)

(** Parse a custom property (--name: value) *)
let is_font_family_var name =
  let bare = Custom_property_name.strip_prefix name in
  let starts_with prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix
  in
  starts_with "font-" bare
  || starts_with "default-font-family" bare
  || starts_with "default-mono-font-family" bare

(* CSS Syntax 3 (ED) sec. 5.5.6 takes the flag off when the last two
   non-whitespace values of the declaration are a [!] delim and an ident
   matching [important], so the whitespace [! important] writes between the two
   is inside the flag. Chrome 151 reads it that way, and what is left is the
   value. *)
let is_ws_char = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let trim_end s =
  let n = ref (String.length s) in
  while !n > 0 && is_ws_char s.[!n - 1] do
    decr n
  done;
  String.sub s 0 !n

let split_important value =
  let trimmed = String.trim value in
  let len = String.length trimmed in
  let word = "important" in
  let word_len = String.length word in
  if
    len > word_len
    && String.lowercase_ascii (String.sub trimmed (len - word_len) word_len)
       = word
  then
    let head = trim_end (String.sub trimmed 0 (len - word_len)) in
    let head_len = String.length head in
    if head_len > 0 && Char.equal head.[head_len - 1] '!' then
      (String.trim (String.sub head 0 (head_len - 1)), true)
    else (trimmed, false)
  else (trimmed, false)

let read_custom_property_payload name value_str =
  if is_font_family_var name then
    let trimmed = String.trim value_str in
    if String.length trimmed >= 4 && String.sub trimmed 0 4 = "var(" then
      read_custom_property_value (Cursor.of_string value_str)
    else
      read_custom_property_value ~font_family:true (Cursor.of_string value_str)
  else read_custom_property_value (Cursor.of_string value_str)

(* The [name] declaration formed over the value at [t]; split from
   [read_custom_property_declaration] for a caller that already holds the
   name. *)
let read_custom_value_declaration t name : declaration =
  (* CSS Syntax 3 (ED) sec. 5.5.6 discards whitespace before a declaration's
     value and again from its end, so [--foo: ;] carries no token at all. CSS
     Custom Properties 1 (ED) sec. 2 writes the [--*] family's value
     [<declaration-value>?] and sec. 2.2 calls that empty value valid, which is
     what is left. Read the raw text untrimmed all the same: the trim belongs to
     [split_important], which needs the [!] and the flag word together. *)
  reject_custom_bad_string t;
  let raw_value = Cursor.consume_until_semicolon ~trim:false t in
  let value_str, is_important = split_important raw_value in
  if not (is_optional_declaration_value value_str) then
    Cursor.err_invalid t "custom property value is no <declaration-value>";
  (* custom_property may raise Failure for invalid names like "--" *)
  try
    let custom_value = read_custom_property_payload name value_str in
    let decl =
      v (Custom_property name)
        (Custom_value { value = custom_value; layer = None; meta = None })
    in
    if is_important then important decl else decl
  with Failure msg -> Cursor.err_invalid t msg

let read_custom_property_declaration t : declaration =
  let name = read_property_name t in
  (* CSS Syntax 3 (ED) sec. 4.3.7 lets [\X] escapes carry any code point into an
     ident, so the name may contain characters ([/], whitespace, etc.) that
     don't tokenize as a bare ident on a string round-trip. We trust the
     original lexer's tokenization: the only validation we still run is the
     [<dashed-ident>] prefix check. *)
  if String.length name <= 2 || name.[0] <> '-' || name.[1] <> '-' then
    Cursor.err_invalid t ("expected <dashed-ident>, got: " ^ name);
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  read_custom_value_declaration t name

(* Properties whose grammar allows multi-token values where a CSS-wide keyword
   can legitimately appear as a non-special ident. [animation-name] /
   [will-change] / etc. accept arbitrary ident lists. [font-family] also takes a
   [<custom-ident>#] list, so a CSS-wide keyword inside the list is invalid CSS
   (CSS Cascade 5 sec. 7.3) but upstream tools (lightningcss, csso) preserve the
   source verbatim. *)
let property_allows_keyword_as_ident = function
  | "animation-name" | "will-change" | "view-transition-name" | "font-family"
  | "font" ->
      true
  | _ -> false

(* [all] is the only property whose value must be a lone CSS-wide keyword (or a
   [var()] that may resolve to one); detect that directly on the component list,
   ignoring leading and trailing whitespace, without rebuilding a string. *)
let components_are_lone_css_wide cvs =
  let non_ws =
    List.filter
      (function
        | Component.Preserved { kind = Token.Whitespace _; _ } -> false
        | _ -> true)
      cvs
  in
  match non_ws with
  | [ Component.Preserved { kind = Token.Ident ident; _ } ] ->
      Properties.is_css_wide_keyword ident
  | [ Component.Func { node = { name; _ }; _ } ] ->
      let n = String.lowercase_ascii_preserve name in
      String.equal n "var" || String.equal n "env" || String.equal n "attr"
  | _ -> false

(* CSS Anchor Positioning 1 sec. 3.2 allows [anchor()] in the inset properties
   and nowhere else, and sec. 5.1 allows [anchor-size()] in the properties
   [@position-try] accepts: the inset, margin, sizing and self-alignment ones,
   plus [position-anchor] and [position-area]. A call written anywhere else is
   invalid, so the declaration goes. *)
let inset_properties =
  [
    "top";
    "right";
    "bottom";
    "left";
    "inset";
    "inset-block";
    "inset-block-start";
    "inset-block-end";
    "inset-inline";
    "inset-inline-start";
    "inset-inline-end";
  ]

let anchor_size_properties =
  inset_properties
  @ [
      "margin";
      "margin-top";
      "margin-right";
      "margin-bottom";
      "margin-left";
      "margin-block";
      "margin-block-start";
      "margin-block-end";
      "margin-inline";
      "margin-inline-start";
      "margin-inline-end";
      "width";
      "height";
      "min-width";
      "min-height";
      "max-width";
      "max-height";
      "block-size";
      "inline-size";
      "min-block-size";
      "min-inline-size";
      "max-block-size";
      "max-inline-size";
      "align-self";
      "justify-self";
      "place-self";
      "position-anchor";
      "position-area";
    ]

let rec components_call_named names components =
  List.exists (component_calls_named names) components

and component_calls_named names = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      List.exists (String.equal (String.lowercase_ascii_preserve name)) names
      || components_call_named names arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_call_named names value
  | Component.Flat { node; _ } -> Component.flat_calls names node
  | Component.Preserved _ -> false

let validate_anchor_queries t name components =
  let allowed table = List.exists (String.equal name) table in
  if
    components_call_named [ "anchor" ] components
    && not (allowed inset_properties)
  then Cursor.err_invalid t ("anchor() is not a value of " ^ name);
  if
    components_call_named [ "anchor-size" ] components
    && not (allowed anchor_size_properties)
  then Cursor.err_invalid t ("anchor-size() is not a value of " ^ name)

let validate_regular_property_components t name components =
  (* CSS Syntax 3 (ED) sec. 5.5.6 gives a declaration a value of one or more
     component values, and only a custom property takes the empty one (CSS
     Variables 1 sec. 2). *)
  if List.for_all Component.is_whitespace components then
    Cursor.err_expected t "a value";
  validate_anchor_queries t name components;
  (* A substitution function makes the declaration's grammar a computed-value
     question. This includes [all]: the substitution result can be empty and
     leave its neighbouring CSS-wide keyword as the whole value. *)
  let has_substitution = components_have_substitution components in
  if
    (not (property_allows_keyword_as_ident name))
    && (not has_substitution)
    && Properties.components_have_css_wide_mix components
  then Cursor.err_invalid t "CSS-wide keyword mixed with other values";
  if
    String.equal name "all" && (not has_substitution)
    && not (components_are_lone_css_wide components)
  then Cursor.err_invalid t "all accepts only CSS-wide keywords"

(* Color functions cascade types directly. A vendor one it does not is kept
   verbatim: the prefix says the vocabulary is an engine's own, so no
   specification settles it and no browser but that engine reads it either. An
   unprefixed name cascade does not know is no colour - CSS Color 5 sec. 3
   closes the production - and every browser drops the declaration. *)
let color_fallback_function raw_value =
  let components =
    Cursor.of_string raw_value |> Cursor.remaining
    |> List.filter (fun component -> not (Component.is_whitespace component))
  in
  match components with
  | [ Component.Func { node = { name; _ }; _ } ] ->
      let fn = String.lowercase_ascii_preserve name in
      String.length fn > 0
      && Char.equal fn.[0] '-'
      && not
           (List.mem fn
              [
                "rgb";
                "rgba";
                "hsl";
                "hsla";
                "hwb";
                "lab";
                "lch";
                "oklab";
                "oklch";
                "color";
                "color-mix";
                "light-dark";
                "var";
              ])
  | _ -> false

(* A colour function whose arguments contain a [var()] can't be validated until
   substitution (CSS Variables L1 section 3): the var() may stand for several
   channels or the alpha, so the arity and legacy/modern separator style aren't
   known at parse time. [rgba(var(--rgb), var(--a))] with a comma-list [--rgb]
   only stays valid as the legacy comma form, so cascade keeps the declaration
   verbatim. The function may be nested (a gradient stop, a shadow colour), so
   the search recurses. (A math function whose return type is wrong regardless
   of the var, e.g. [flex-basis: sign(var(--x))], is still rejected by the
   reader.) *)
let rec components_have_var_color_function components =
  List.exists component_has_var_color_function components

and component_has_var_color_function = function
  | Component.Func { node = { name; arguments; _ }; _ } ->
      List.mem
        (String.lowercase_ascii_preserve name)
        [ "rgb"; "rgba"; "hsl"; "hsla"; "hwb"; "lab"; "lch"; "oklab"; "oklch" ]
      && components_contain_var arguments
      || components_have_var_color_function arguments
  | Component.Block { node = { value; _ }; _ } ->
      components_have_var_color_function value
  (* Over a run, a colour function and a [var()] anywhere in it: the run keeps
     no tree to say which call holds which. Answering yes keeps the value
     verbatim, which is what the run prints anyway. *)
  | Component.Flat { node; _ } ->
      Component.flat_calls
        [ "rgb"; "rgba"; "hsl"; "hsla"; "hwb"; "lab"; "lch"; "oklab"; "oklch" ]
        node
      && Component.flat_calls [ "var" ] node
  | Component.Preserved _ -> false

let has_var_color_function raw_value =
  Cursor.of_string raw_value |> Cursor.remaining
  |> components_have_var_color_function

let is_color_property = function
  | "color" | "background-color" | "border-color" | "border-top-color"
  | "border-right-color" | "border-bottom-color" | "border-left-color"
  | "border-inline-start-color" | "border-inline-end-color"
  | "border-block-start-color" | "border-block-end-color"
  | "text-decoration-color" | "-webkit-text-decoration-color"
  | "-webkit-tap-highlight-color" | "outline-color" | "accent-color"
  | "caret-color" | "fill" | "stroke" ->
      true
  | _ -> false

let is_unsupported_color_fallback name raw_value =
  color_fallback_function raw_value && is_color_property name

let is_unknown_property_name = is_decl_unknown_property_name

(* The typed readers carry their own [Invalid] arms for spec-violations they
   detect ([Values.angle.Invalid] / [Properties.clip_path.Invalid]), so the
   declaration-level unknown fallback only needs to handle truly unknown
   properties or property-specific colour fallback edges. *)
let allows_unknown_fallback name raw_value =
  (not (raw_value_has_invalid_substitution raw_value))
  && (is_unknown_property_name name
     || is_unsupported_color_fallback name raw_value
     || has_var_color_function raw_value
     || raw_value_contains_substitution raw_value)

let read_font_src_declaration t raw_value =
  ignore raw_value;
  let value = Cursor.declaration_value t in
  let decl = v Source (Properties.read_font_src value) in
  validate_no_extra_tokens value;
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  if is_important then important decl else decl

let read_unknown_property_declaration t name =
  validate_unknown_property_name t name;
  validate_complete_declaration_value t;
  reject_curly_block_value t;
  reject_unterminated_string_value t;
  reject_opaque_bad_token t;
  let raw_value = Cursor.consume_to_decl_end ~trim:true t in
  let is_important = read_importance t in
  validate_no_extra_tokens t;
  (match Cursor.peek_delim t with
  | Some '!' -> Cursor.err_invalid t "duplicate !important"
  | _ -> ());
  unknown_property ~important:is_important name raw_value

(* The declaration [prop_type] forms over the value at [t]. Takes the property
   rather than reading it, so a caller holding one does not have to spell it out
   for the parser to read back: [pp_property] under minify writes [page-break-*]
   as its CSS Fragmentation 3 sec. 3.4 [break-*] alias, which names a different
   property. *)
let read_typed_value_declaration : type a. a property -> Cursor.t -> declaration
    =
 fun prop_type t ->
  (* CSS Syntax 3 (ED) sec. 2.2 / sec. 4.3.5 auto-close unterminated functions,
     brackets and strings at EOF. Typed readers consume the spec-recovered
     tokens; the declaration survives with the auto-closed shape. *)
  match prop_type with
  | Unknown_property name -> read_unknown_property_declaration t name
  | _ -> (
      (* The typed value readers and [validate_no_extra_tokens] reject a
         right-hand side without knowing which property they serve, so a
         [Bad_value] surfaces with an empty property name ([bad value for :]).
         Stamp the property back on so the warning names it ([bad value for
         background-image:]). *)
      let read () =
        let value = Cursor.declaration_value t in
        let decl = read_value prop_type value in
        validate_no_extra_tokens value;
        let is_important = read_importance t in
        validate_no_extra_tokens t;
        (match Cursor.peek_delim t with
        | Some '!' -> Cursor.err_invalid t "duplicate !important"
        | _ -> ());
        if is_important then important decl else decl
      in
      try read ()
      with Cursor.Parse_error e ->
        Error.fail (Error.with_property (canonical_prop_name prop_type) e))

let read_typed_property_declaration t start =
  Cursor.restore t start;
  let (Prop prop_type) = read_any_property t in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  read_typed_value_declaration prop_type t

(** Parse a regular property (name: value) *)
let read_regular_property_declaration t : declaration =
  let start = Cursor.save t in
  let name = String.lowercase_ascii_preserve (read_property_name t) in
  Cursor.ws t;
  if not (Cursor.colon t) then Cursor.err_expected t "':'";
  Cursor.ws t;
  let components = Cursor.lookahead Cursor.drain_to_decl_end t in
  validate_regular_property_components t name components;
  if String.equal name "src" then
    read_font_src_declaration t
      (String.trim (Parser.string_of_components components))
  else
    try read_typed_property_declaration t start
    with Cursor.Parse_error _ as exn ->
      (* A declaration-shaped token stream is not a valid declaration: a value
         the typed reader rejects is one a browser rejects, and README states
         that those are dropped. The fallback carries the values that are valid
         CSS this reader cannot represent, which is what
         [allows_unknown_fallback] names. *)
      let raw_value = String.trim (Parser.string_of_components components) in
      if not (allows_unknown_fallback name raw_value) then raise exn;
      Cursor.restore t start;
      let name = String.lowercase_ascii_preserve (read_property_name t) in
      Cursor.ws t;
      if not (Cursor.colon t) then Cursor.err_expected t "':'";
      Cursor.ws t;
      read_unknown_property_declaration t name

(* [read_regular_property_declaration] with the name step replaced by the
   property itself. [name] serves the validation and the opaque fallback; the
   identity of what is read back comes from [prop]. *)
let read_regular_value_declaration : type a.
    a property -> Cursor.t -> declaration =
 fun prop t ->
  let name = canonical_prop_name prop in
  let start = Cursor.save t in
  let components = Cursor.lookahead Cursor.drain_to_decl_end t in
  validate_regular_property_components t name components;
  match prop with
  | Source ->
      read_font_src_declaration t
        (String.trim (Parser.string_of_components components))
  | _ -> (
      try read_typed_value_declaration prop t
      with Cursor.Parse_error _ as exn ->
        let raw_value = String.trim (Parser.string_of_components components) in
        if not (allows_unknown_fallback name raw_value) then raise exn;
        Cursor.restore t start;
        read_unknown_property_declaration t name)

(* CSS Nesting 1: distinguish a declaration ([<ident> : <value>]) from a nested
   rule whose selector starts with an ident ([html &:hover { ... }]). The
   starts-with-ident shape isn't conclusive on its own, so look at the next
   non-ident component: if it's [:] this is a declaration, otherwise walk the
   lookahead window for a [{ ... }] block before the next [;]. *)
let rec scan_for_curly_block t =
  match Cursor.peek_head_shape t with
  | `Eof | `Semicolon -> false
  | `Curly_block -> true
  | _ ->
      Cursor.skip t;
      scan_for_curly_block t

let is_nested_rule_inner t =
  (match Cursor.peek_head_shape t with `Ident -> Cursor.skip t | _ -> ());
  match Cursor.peek_head_shape t with
  | `Colon -> false
  | _ -> scan_for_curly_block t

let is_nested_rule t = Cursor.lookahead is_nested_rule_inner t

(** Parse a single declaration directly from stream - no string roundtrips *)
let read_declaration t : declaration option =
  let read_one () =
    Cursor.with_context t "read_declaration" @@ fun () ->
    (* Custom properties are idents starting with [--]. *)
    let is_custom =
      match Cursor.peek t with
      | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
          Custom_property_name.has_prefix s
      | _ -> false
    in
    if is_custom then read_custom_property_declaration t
    else read_regular_property_declaration t
  in
  Cursor.ws t;
  match Cursor.peek t with
  | None -> None (* EOF is acceptable at top-level parsing *)
  | Some (Component.Preserved { kind = Token.Colon; _ })
  | Some (Component.Preserved { kind = Token.Hash _; _ })
  | Some (Component.Block { node = { opening = Token.Square; _ }; _ })
  | Some (Component.Preserved { kind = Token.Delim ("." | "*" | "&"); _ }) ->
      (* Selector-like components indicate a nested rule. *)
      None
  | Some _ ->
      (* CSS Nesting 1: a nested rule may start with an ident-shaped selector
         ([html &:hover], [li:nth-child(2)]). A declaration always starts with
         [<ident> :]; anything else followed by a [{ ... }] block (before the
         next [;]) is a nested rule. *)
      if is_nested_rule t then None else Some (read_one ())

type parse_step =
  | Done of declaration list
  | Continue of declaration list
  | Recover of {
      decls : declaration list;
      error : Error.t;
      from : Cursor.snapshot;
          (* Where the item that is about to be dropped starts. A separator that
             fails comes after a declaration already committed, so the two
             recovery arms carry different snapshots. *)
    }

let check_declaration_separator t acc =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> Done (List.rev acc)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip t;
      Continue acc
  | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
      Cursor.err t "missing semicolon between declarations"
  | _ -> Done (List.rev acc)

(* [read_declaration] where [refuse] may name why a declaration is invalid at
   that site: it then fails as a value the reader rejects does. *)
let read_declaration_at ~refuse t =
  Cursor.ws t;
  let loc = Cursor.position t in
  match read_declaration t with
  | Option.Some decl as read -> (
      match refuse decl with
      | Option.None -> read
      | Option.Some reason -> Cursor.err_invalid ~loc t reason)
  | Option.None -> Option.None

let read_declaration_no_recovery ~refuse t acc =
  match read_declaration_at ~refuse t with
  | None -> Done (List.rev acc)
  | Some decl -> check_declaration_separator t (decl :: acc)

let read_declaration_with_recovery ~refuse t acc =
  let start = Cursor.save t in
  match read_declaration_at ~refuse t with
  | None -> Done (List.rev acc)
  | Some decl -> (
      let acc = decl :: acc in
      let after = Cursor.save t in
      match check_declaration_separator t acc with
      | step -> step
      | exception Error.Parse_error e ->
          Recover { decls = acc; error = e; from = after })
  | exception Error.Parse_error e ->
      Recover { decls = acc; error = e; from = start }

let read_declaration_step ~refuse t acc =
  if Cursor.recover t then read_declaration_with_recovery ~refuse t acc
  else read_declaration_no_recovery ~refuse t acc

(* CSS Syntax 3 (ED) sec. 5.5.5 ("consume a block's contents"): the invalid
   declaration is dropped, reading resumes past the next [;], and the
   surrounding rule survives. *)
let recover_declaration_step t acc e ~from =
  Cursor.skip_past_semicolon t;
  Cursor.push_warning t
    ~recovery:(Cursor.dropped_since t from Error.Recovery.Declaration)
    e;
  acc

(* CSS Syntax 3 (ED) sec. 5.5.5 consumes an AT-RULE when a block's contents meet
   an at-keyword, so it ends at its own block or [;], not at the next [;] the
   way an invalid declaration does. A keyframe or descriptor block has no
   grammar for one, so it is dropped - but only it: [@keyframes
   k{to{@e{}opacity:1}}] keeps the opacity, which is what the browser keeps
   too. *)
let skip_at_rule t =
  Cursor.skip t;
  let rec go () =
    match Cursor.peek_head_shape t with
    | `Eof -> ()
    | `Curly_block | `Semicolon -> Cursor.skip t
    | _ ->
        Cursor.skip t;
        go ()
  in
  go ()

let rec read_declarations_loop ~refuse t acc =
  Cursor.ws t;
  match Cursor.peek t with
  | None -> List.rev acc
  | Some _ when Option.is_some (Cursor.peek_at_keyword t) ->
      let from = Cursor.save t in
      let error =
        try Cursor.err t "at-rule in a declaration block"
        with Cursor.Parse_error e -> e
      in
      skip_at_rule t;
      Cursor.push_warning t
        ~recovery:(Cursor.dropped_since t from Error.Recovery.Rule)
        error;
      read_declarations_loop ~refuse t acc
  | _ -> (
      match read_declaration_step ~refuse t acc with
      | Done decls -> decls
      | Continue acc -> read_declarations_loop ~refuse t acc
      | Recover { decls; error; from } ->
          read_declarations_loop ~refuse t
            (recover_declaration_step t decls error ~from))

let read_declarations t =
  Cursor.with_context t "declarations" @@ fun () ->
  read_declarations_loop ~refuse:(fun _ -> Option.None) t []

(* CSS Animations 1 sec. 3: in a keyframe "properties qualified with !important
   are invalid and ignored", and "the properties defined by this specification
   are ignored in these rules, with the exception of animation-timing-function".
   The name is read off the declaration, so one kept as an unknown property (a
   [var()] value) is refused as well. *)
let keyframe_refusal decl =
  if is_important decl then Option.Some "!important in a keyframe"
  else
    match property_name decl with
    | ( "animation" | "animation-name" | "animation-duration"
      | "animation-delay" | "animation-iteration-count" | "animation-direction"
      | "animation-fill-mode" | "animation-play-state" ) as name ->
        Option.Some (String.concat "" [ name; " in a keyframe" ])
    | _ -> Option.None

let read_keyframe_declarations t =
  Cursor.with_context t "declarations" @@ fun () ->
  read_declarations_loop ~refuse:keyframe_refusal t []

let read_block t =
  Cursor.ws t;
  Cursor.braces (fun inner -> read_declarations inner) t

let of_string s =
  let r = Cursor.of_string s in
  match read_declaration r with
  | Some d -> d
  (* [read_declaration] answers [None] for text a declaration cannot start with,
     which here is the whole input: report it where the reader stopped rather
     than as a second, position-free exception. *)
  | None -> Cursor.err_expected r "a declaration"

(* The declaration [property] and [value] name, as the tokens they are. The name
   is one [<ident-token>] by construction and the value is parsed on its own, so
   neither reaches the other's position: written into one text, a name carrying
   a [;] or a [}] (CSS Syntax 3 (ED) sec. 4.3.7 puts either there through an
   escape) would end its own declaration, and one carrying a [:] would name the
   property in front of it. *)
let name_value_components property value =
  Component.Preserved (Token.synthetic (Token.Ident property))
  :: Component.Preserved (Token.synthetic Token.Colon)
  :: Cursor.remaining (Cursor.of_string value)

let parse_declaration ?layer property value =
  (* Read [property] and [value] with the full declaration parser: a known
     property (e.g. [mask-type], [display]) becomes a typed declaration, a
     custom property ([--x]) or an unknown property keeps its parsed component
     stream (so [vars_of_declarations] still finds its [var()] references),
     unlike [custom_property] which forces an opaque [Tokens] value. A [layer]
     only applies to a custom property; [custom_property] attaches it and yields
     the same parsed token stream the declaration parser would. *)
  match layer with
  | Some _ when is_custom_property_name property -> (
      try Some (custom_property ?layer property value) with Failure _ -> None)
  | _ -> (
      try
        read_declaration
          (Cursor.of_components (name_value_components property value))
      with Cursor.Parse_error _ -> None)

(* [parse_declaration] with the typed grammar skipped: the value stays the
   component stream it was written as, the way an unknown property's does. A
   custom property already reads that way, so it takes the ordinary path. *)
let parse_opaque_declaration property value =
  if is_custom_property_name property then parse_declaration property value
  else
    let t = Cursor.of_components (name_value_components property value) in
    try
      let name = String.lowercase_ascii_preserve (read_property_name t) in
      Cursor.ws t;
      if not (Cursor.colon t) then Cursor.err_expected t "':'";
      Cursor.ws t;
      Some (read_unknown_property_declaration t name)
    with Cursor.Parse_error _ -> None

(* CSS Variables 1 sec. 2 gives a custom property the value grammar
   [<declaration-value>?], the empty value included, so this takes the pairs
   [custom_property] takes and answers with an option rather than raising. *)
let parse_custom_property name value =
  if is_custom_property_name name && is_optional_declaration_value value then
    parse_declaration name value
  else None

let read t =
  match read_declaration t with
  | Some d -> d
  | None -> Cursor.err_expected t "declaration"

(* A cascade layer and caller metadata belong to the declaration, not to its
   value, so a value read back from text starts without them. A value-only
   rewrite copies over what [from] holds, the way [map_custom_value] keeps them
   when it rewrites in place. *)
let keep_custom_fields ~from decl =
  match decl with
  | Declaration
      {
        property = Custom_property _ as property;
        value = Custom_value cv;
        important;
        _;
      } ->
      let layer = custom_declaration_layer from in
      let meta = meta_of_declaration from in
      v ~important property (Custom_value { cv with layer; meta })
  | Declaration _ | Theme_guarded _ -> decl

let with_value decl value =
  let important = if is_important decl then "!important" else "" in
  let tail () = Cursor.of_string (String.concat "" [ value; important ]) in
  match property_key decl with
  | Key (Custom_property name) ->
      keep_custom_fields ~from:decl
        (read_custom_value_declaration (tail ()) name)
  | Key (Unknown_property name) ->
      (* An unknown property is its name, and a substituted value may now be one
         a typed reader accepts, so read the pair back whole: that is the step
         that types [--x: 1px] once [width: var(--x)] has folded. *)
      read (Cursor.of_string (String.concat "" [ name; ":"; value; important ]))
  | Key prop -> read_regular_value_declaration prop (tail ())

let with_opaque_value decl value =
  let (Key prop) = property_key decl in
  let opaque =
    let name = canonical_prop_name prop in
    v (Unknown_property name) (Cursor.remaining (Cursor.of_string value))
  in
  if is_important decl then important opaque else opaque
