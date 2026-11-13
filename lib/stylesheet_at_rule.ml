open Stylesheet_intf
open Stylesheet_pp

(** {1 Reading/Parsing} *)

let read_keyframe (r : Cursor.t) : keyframe =
  Cursor.ws r;
  let selector_str = Cursor.drain_until_block_as_string ~trim:true r in
  let selector =
    try Keyframe.selector_of_string selector_str
    with Invalid_argument msg -> Cursor.err_invalid r msg
  in
  let declarations = Cursor.braces Declaration.read_keyframe_declarations r in
  { selector; declarations }

(* Helper functions for reading specific at-rules *)

(* CSS Syntax 3 sec. 3.2 reads an encoding only from the first bytes of a sheet,
   and only when they are [@charset "<label>";]: one space, double quotes, and
   the semicolon right after the closing quote. Sec. 8.3 makes any other
   [@charset] an unrecognized rule, so it is a syntax error; of the labels only
   ["UTF-8"] is recognised. *)
let read_charset (r : Cursor.t) : statement =
  let at = Cursor.position r in
  let fail loc reason = Error.fail_bad_value loc ~property:"@charset" ~reason in
  if at.Loc.start_pos <> 0 then
    fail at "@charset must be the first bytes of the stylesheet";
  Cursor.expect_at_keyword "charset" r;
  (match Cursor.next_raw r with
  | Some (Component.Preserved { kind = Token.Whitespace " "; _ }) -> ()
  | _ -> fail at "@charset takes one space before its quote");
  let encoding =
    match Cursor.string_with_quote_opt r with
    | Some (value, '"') when value = "UTF-8" -> value
    | _ -> fail (Cursor.position r) "@charset only recognises \"UTF-8\""
  in
  (match Cursor.next_raw r with
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> ()
  | _ -> fail at "@charset must end with ';' right after its quote");
  Charset encoding

(* CSS Cascade section 6.4.2: a layer name is one or more idents joined by '.'
   with no whitespace around the dot. CSS-wide keywords are reserved. *)
let read_layer_name_component (r : Cursor.t) : layer_name =
  let reserved = function
    | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
    | _ -> false
  in
  let part p =
    if reserved (String.lowercase_ascii p) then
      Cursor.err_invalid r ("layer name reserves CSS-wide keyword: " ^ p);
    p
  in
  let first = part (Cursor.ident ~keep_case:true r) in
  (* Only a [.] delimiter separates two idents. One an ident carries arrives as
     part of that ident's own token, so it never reaches here. *)
  let rec extend acc =
    match Cursor.peek_raw r with
    | Some (Component.Preserved { kind = Token.Delim "."; _ }) ->
        Cursor.skip r;
        let next =
          match Cursor.peek_raw r with
          | Some (Component.Preserved { kind = Token.Ident s; _ }) ->
              Cursor.skip r;
              s
          | _ -> Cursor.err_expected r "ident after '.' in layer name"
        in
        extend (part next :: acc)
    | _ -> List.rev acc
  in
  extend [ first ]

(* CSS Cascade 5 sec. 2 [@import] prelude: [<url> [layer | layer(<layer-name>)]?
   [supports(<supports-condition>)]? <media-query-list>?]. The [~keep_url_repr]
   flavour preserves the original quote / [url(...)] form in [url] so the
   at-rule dispatch can round-trip author input verbatim; the canonical flavour
   ([keep_url_repr=false]) stores the decoded string for the top-level
   [read_import_rule] reader. *)
let read_import_url ~keep_url_repr (r : Cursor.t) =
  let pos = Cursor.position r in
  let value = Cursor.one_of [ Cursor.url; Cursor.string ] r in
  if keep_url_repr then
    let end_pos = Cursor.position r in
    match Cursor.source r with
    | Some source
      when end_pos.start_pos <= String.length source
           && pos.start_pos <= end_pos.start_pos
           && pos.start_pos >= 0 ->
        String.sub source pos.start_pos (end_pos.start_pos - pos.start_pos)
        |> String.trim
    | _ -> value
  else value

let read_import_layer (r : Cursor.t) =
  match
    Cursor.function_call "layer"
      (fun inner ->
        Cursor.ws inner;
        if Cursor.is_done inner then []
        else
          let name = read_layer_name_component inner in
          Cursor.ws inner;
          Cursor.expect_eof inner;
          name)
      r
  with
  | Some _ as some -> some
  | None -> (
      match Cursor.peek_keyword r with
      | Some "layer" ->
          let _ = Cursor.ident r in
          Some []
      | _ -> None)

let read_import_supports (r : Cursor.t) =
  Cursor.function_call "supports"
    (fun inner -> Supports.read ~allow_unwrapped_decl:true inner)
    r

(* In the [@import] prelude [layer(...)] and [supports(...)] are structural, and
   the grammar allows each once, before the media query list. Reaching the media
   position still holding one means a duplicate or a misordered prelude. They
   are function tokens, so the media grammar would otherwise take them as a
   [<general-enclosed>] query and quietly accept the rule. *)
let starts_with_import_keyword components =
  let cursor = Cursor.of_components components in
  match Cursor.peek cursor with
  | Some (Component.Func { node = { name; _ }; _ }) ->
      let name = String.lowercase_ascii name in
      String.equal name "layer" || String.equal name "supports"
  | Some (Component.Preserved { kind = Token.Ident name; _ }) ->
      String.equal (String.lowercase_ascii name) "layer"
  | Some (Component.Block _ | Component.Preserved _ | Component.Flat _) | None
    ->
      false

let drain_until_semicolon r =
  let rec loop acc =
    match Cursor.peek_raw r with
    | None | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        List.rev acc
    | Some component ->
        ignore (Cursor.next_raw r);
        loop (component :: acc)
  in
  loop []

let read_import_media (r : Cursor.t) : Media.t option =
  if Cursor.peek_semicolon r || Cursor.is_done r then None
  else
    let loc = Cursor.position r in
    let components = drain_until_semicolon r in
    if starts_with_import_keyword components then
      Error.fail_bad_condition loc ~at_rule:"@media"
        ~reason:"layer()/supports() must precede the media query, and once only"
    else Some (Media.read ~recover:false (Cursor.sub r components))

let read_import_prelude ~keep_url_repr (r : Cursor.t) : import_rule =
  Cursor.expect_at_keyword "import" r;
  Cursor.ws r;
  let url = read_import_url ~keep_url_repr r in
  Cursor.ws r;
  let layer = read_import_layer r in
  Cursor.ws r;
  let supports = read_import_supports r in
  Cursor.ws r;
  let media = read_import_media r in
  if Cursor.peek_semicolon r then Cursor.skip r;
  { url; layer; supports; media }

let read_import (r : Cursor.t) : statement =
  Import (read_import_prelude ~keep_url_repr:true r)

let read_namespace (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "namespace" r;
  Cursor.ws r;
  (* Optional prefix ident; absent when the URL comes first. *)
  let prefix =
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.Ident _; _ }) ->
        Some (Cursor.ident ~keep_case:true r)
    | _ -> None
  in
  Cursor.ws r;
  let uri =
    match Cursor.peek r with
    | Some (Component.Preserved { kind = Token.String { value; _ }; _ }) ->
        Cursor.skip r;
        Quoted value
    | _ ->
        let pos = Cursor.position r in
        let value = Cursor.url r in
        let end_pos = Cursor.position r in
        let form =
          match Cursor.source r with
          | Some source
            when end_pos.start_pos <= String.length source
                 && pos.start_pos <= end_pos.start_pos
                 && pos.start_pos >= 0 ->
              let raw =
                String.sub source pos.start_pos
                  (end_pos.start_pos - pos.start_pos)
              in
              if String.contains raw '\'' then (Quoted '\'' : url_form)
              else if String.contains raw '"' then (Quoted '"' : url_form)
              else (Bare : url_form)
          | _ -> (Bare : url_form)
        in
        Url (value, form)
  in
  Cursor.ws r;
  if Cursor.peek_semicolon r then Cursor.skip r;
  Namespace (prefix, uri)

(* Discard the rule the cursor sits on, stopping at its [{}] block or at a
   top-level [;]: CSS Syntax 3 (ED) sec. 5.5.2 ends an at-rule at whichever
   comes first, and sec. 5.5.3 ends a qualified rule at its block. Consuming
   exactly that far leaves what was written after the rule - the declarations
   around it, or the next rule - to be read on its own. A [(] or a [[] met
   before the block is a component value of the prelude being discarded, so
   stopping there would offer the tail of that prelude as a rule of its own. *)
let rec skip_past_rule r =
  match Cursor.next_raw r with
  | None -> ()
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ })
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      ()
  | Some _ -> skip_past_rule r

(* Discard the item the cursor sits on. A body that holds declarations and
   at-rules alike has to pick by what the item starts with: the two ends differ,
   and taking an at-rule for a declaration would eat the item written after it.
   The caller rewinds first, so a reader that already consumed part of the item
   is skipped from its start. *)
let skip_invalid_item r =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) ->
      skip_past_rule r
  | _ -> Cursor.skip_past_semicolon r

(* Read a body one item at a time, [step] committing each item to the
   accumulator before the next is read, so an item dropped in recovery costs
   only itself and the items around it are kept. CSS Syntax 3 (ED) sec. 5.5.5
   keeps what a block's contents already yielded when one item fails to parse,
   and CSS Paged Media 3 sec. 4.1 says as much of a page or a margin context in
   so many words: "valid declarations within the block are applied". Every
   statement of a sheet is read from a recovering cursor ({!cursor_of_rule}
   passes [~recover:true]), and [Css.of_string ~strict:true] filters the
   warnings that parse pushed rather than selecting another one. [skip] discards
   the item that failed, and which one it is depends on the body:
   {!skip_invalid_item} for a body of declarations, {!skip_past_rule} for a body
   of rules, where an item that opens a block ends at that block rather than at
   a [;] it does not have. [construct] names the same two bodies: the dropped
   item is a declaration in the first and a rule in the second. The warning is
   pushed after the skip, which is what fixes how far the loss reaches: the item
   is dropped from [start] to wherever [skip] leaves the cursor. *)
let read_items_with_recovery ~skip ~construct step r init =
  let rec loop state =
    if Cursor.recover r then recovering state else continue (step r state)
  and recovering state =
    let start = Cursor.save r in
    match step r state with
    | committed -> continue committed
    | exception Error.Parse_error e ->
        Cursor.restore r start;
        skip r;
        Cursor.push_warning r
          ~recovery:(Cursor.dropped_since r start construct)
          e;
        loop state
  and continue = function
    | `Done result -> result
    | `More state -> loop state
  in
  loop init

(* A selector that is no keyframe selector - the [to] of [entry to], which is no
   [<length-percentage>] - drops its rule and the block keeps parsing. *)
let read_keyframe_or_skip inner acc =
  let snap = Cursor.save inner in
  match read_keyframe inner with
  | frame -> frame :: acc
  | exception Cursor.Parse_error e ->
      Cursor.restore inner snap;
      Cursor.push_warning inner ~recovery:Error.Recovery.(dropped Rule) e;
      skip_past_rule inner;
      acc

(* One item of the body: a keyframe rule, or the end of it. CSS Animations 1
   sec. 3 fills a [@keyframes] body with keyframe rules, so an at-rule has no
   place there; rejecting it here rather than around the loop leaves the
   rejection inside the recovery the loop runs, which costs the at-rule alone
   instead of the animation and the sheet holding it. *)
let read_keyframes_step inner acc =
  Cursor.ws inner;
  if Cursor.is_done inner then `Done (List.rev acc)
  else
    match Cursor.peek inner with
    | Some (Component.Preserved { kind = Token.At_keyword name; _ }) ->
        Cursor.err_invalid inner ("@keyframes nested @" ^ name)
    | _ -> `More (read_keyframe_or_skip inner acc)

let read_keyframes_block inner =
  read_items_with_recovery ~skip:skip_past_rule ~construct:Error.Recovery.Rule
    read_keyframes_step inner []

(* CSS Animations 1 sec. 3: [@keyframes <keyframes-name>], [<keyframes-name> =
   <custom-ident> | <string>]. The ident arm loses [none] on top of what sec.
   4.2 already keeps out of a [<custom-ident>]; the string arm takes every one
   of those names and loses the empty string instead. *)
let read_keyframes_name r =
  Cursor.ws r;
  let loc = Cursor.position r in
  match Cursor.string_opt r with
  | Some "" -> Cursor.err_invalid ~loc r "empty keyframes name"
  | Some s -> s
  | None -> Cursor.custom_ident ~reserved:[ "none" ] "keyframes name" r

let read_keyframes_named at_keyword make_statement (r : Cursor.t) : statement =
  Cursor.with_context r ("@" ^ at_keyword) @@ fun () ->
  Cursor.expect_at_keyword at_keyword r;
  let name = read_keyframes_name r in
  Cursor.ws r;
  let frames = Cursor.braces read_keyframes_block r in
  make_statement name frames

let read_keyframes (r : Cursor.t) : statement =
  read_keyframes_named "keyframes"
    (fun name frames -> Keyframes (name, frames))
    r

let read_webkit_keyframes r =
  read_keyframes_named "-webkit-keyframes"
    (fun name frames -> Webkit_keyframes (name, frames))
    r

let read_moz_keyframes r =
  read_keyframes_named "-moz-keyframes"
    (fun name frames -> Moz_keyframes (name, frames))
    r

(* [url-prefix()] with no argument is the prelude that matches every document,
   so an empty argument list and an empty string both read as [None]. *)
let read_moz_url_prefix arg_cursor =
  match Cursor.string_opt arg_cursor with
  | Some "" -> Option.None
  | Some prefix -> Some prefix
  | None ->
      Cursor.ws arg_cursor;
      if Cursor.is_done arg_cursor then Option.None
      else Cursor.err_expected arg_cursor "url-prefix string argument"

let read_moz_document_function r name arguments : moz_document_condition =
  let arg_cursor = Cursor.of_components arguments in
  Cursor.ws arg_cursor;
  let finish condition =
    Cursor.ws arg_cursor;
    Cursor.expect_eof arg_cursor;
    condition
  in
  match name with
  | "url" -> finish (Url_exact (Cursor.string arg_cursor))
  | "url-prefix" -> finish (Url_prefix (read_moz_url_prefix arg_cursor))
  | "domain" -> finish (Domain (Cursor.string arg_cursor))
  | "media-document" -> finish (Media_document (Cursor.string arg_cursor))
  | "regexp" -> finish (Regexp (Cursor.string arg_cursor))
  | _ -> Cursor.err_expected r "a @-moz-document URL-matching function"

(* Gecko's [@document] prelude takes [<url>], [url-prefix(<string>)],
   [domain(<string>)], [media-document(<string>)] and [regexp(<string>)]. A
   prelude function outside that list has no grammar, so the at-rule goes down
   with it, which is what CSS Syntax 3 (ED) sec. 5.5.2 does with any prelude no
   grammar accepts. *)
let read_moz_document_condition r : moz_document_condition =
  Cursor.ws r;
  (* [url(x)] lexes as a [<url-token>] and [url("x")] as a function, so the
     [<url>] form is read before the function arms. *)
  match Cursor.url_opt r with
  | Some url -> Url_exact url
  | None -> (
      match Cursor.next r with
      | Some (Component.Func { node = { name; arguments; _ }; _ }) ->
          read_moz_document_function r name arguments
      | _ -> Cursor.err_expected r "a @-moz-document URL-matching function")

(* CSS Syntax 3 (ED) sec. 4.3.5 closes a string at end of input, 4.3.6 a url,
   4.3.2 a comment, and 5.5.9 and 5.5.10 a simple block and a function. Raw text
   that stops mid-construct therefore means the closed form, but written back as
   it stands it hands the next reader an opener that eats the [;] or [}] the
   at-rule ends with. These close it, once, so the AST holds text that reads
   back as itself. *)
let bracket_closer = function
  | Token.Curly -> "}"
  | Token.Paren -> ")"
  | Token.Square -> "]"

(* One pass: the closers the open blocks and functions still want, innermost
   first, with the last token and the token count for [tail_closer]. *)
let scan_raw text =
  let lexer = Lexer.of_string text in
  let rec loop stack last n =
    let tok = Lexer.next lexer in
    match tok.Token.kind with
    | Token.Eof -> (stack, last, n)
    | Token.Function _ -> loop (")" :: stack) (Some tok) (n + 1)
    | Token.Open b -> loop (bracket_closer b :: stack) (Some tok) (n + 1)
    | Token.Close b ->
        let stack =
          match stack with
          | c :: rest when String.equal c (bracket_closer b) -> rest
          | _ -> stack
        in
        loop stack (Some tok) (n + 1)
    | _ -> loop stack (Some tok) (n + 1)
  in
  loop [] None 0

(* A string, a url and a comment run to end of input and close there without
   leaving an opener on the stack, so ask the tokenizer rather than re-deriving
   its state: text that swallows the probe gives back no token for it. Two probe
   code points, since a [\] at end of input eats one and returns it as an
   ident. *)
let tail_closer text last n =
  let lexer = Lexer.of_string (String.concat "" [ text; ";;" ]) in
  let rec count n =
    match (Lexer.next lexer).Token.kind with
    | Token.Eof -> n
    | _ -> count (n + 1)
  in
  if count 0 <> n then []
  else
    match last with
    | Some { Token.kind = Token.String { quote; terminated = false; _ }; _ } ->
        [ String.make 1 quote ]
    | Some { Token.kind = Token.Url _ | Token.Bad_url; loc; _ }
      when loc.Loc.end_pos = String.length text ->
        [ ")" ]
    | _ -> [ "*/" ]

(* Closing one construct can uncover another: a [\] before the closing quote
   escapes it and leaves the string open, so re-read until the text settles. *)
let rec close_raw fuel text =
  let stack, last, n = scan_raw text in
  match List.append (tail_closer text last n) stack with
  | [] -> text
  | closers ->
      let closed = String.concat "" (text :: closers) in
      if fuel <= 1 then closed else close_raw (fuel - 1) closed

(* One pass closes what the stack holds; the other two are for a closer that a
   [\] at end of input escapes before it lands. *)
let close_open_constructs = close_raw 3

(* CSS Syntax 3 (ED) sec. 5.5.9: an unclosed block runs to EOF, so its slice has
   no closer to exclude and instead carries whatever [}] an unterminated nested
   construct swallowed on the way. The serializer supplies its own closer, so
   drop those or each round-trip stacks another one. *)
let trim_unknown_block_body body =
  let rec trim_end i =
    if i < 0 then 0
    else
      match body.[i] with
      | '}' | ' ' | '\t' | '\n' | '\r' -> trim_end (i - 1)
      | _ -> i + 1
  in
  String.sub body 0 (trim_end (String.length body - 1))

(* An unrecognised at-rule has no grammar to re-serialise its body from, so the
   body travels as the source text between its own braces. Slice the block's own
   span, not its child components: the children stop short of the closer
   whenever the body ends in a nested block ([@foo{a{b:c}}]). *)
let unknown_block_body slice (block : Component.block Component.node) =
  let start = block.loc.Loc.start_pos + 1 in
  if block.node.Component.closed then slice start (block.loc.Loc.end_pos - 1)
  else
    slice start block.loc.Loc.end_pos
    |> trim_unknown_block_body |> close_open_constructs

(* CSS Syntax 3 (ED) sec. 5.5.2 "consume an at-rule": after the at-keyword has
   been consumed, walk components until we hit [;] (no block) or [{...}]
   (block). Raw prelude/block strings are sliced from the original source so the
   at-rule round-trips byte-for-byte even when its grammar is unknown. *)
let read_unknown_at_rule name (r : Cursor.t) : statement =
  let source = Option.value (Cursor.source r) ~default:"" in
  let source_len = String.length source in
  let slice start stop =
    if start < 0 || stop > source_len || start > stop then ""
    else String.sub source start (stop - start)
  in
  let prelude_start = ref (-1) in
  let prelude_end = ref (-1) in
  let block = ref Option.None in
  let rec gather () =
    match Cursor.peek r with
    | None -> ()
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
        ignore (Cursor.next_raw r)
    | Some (Component.Block ({ node = { opening = Token.Curly; _ }; _ } as b))
      ->
        block := Option.Some (unknown_block_body slice b);
        ignore (Cursor.next_raw r)
    | Some comp ->
        let loc = Component.source_loc comp in
        if !prelude_start < 0 then prelude_start := loc.Loc.start_pos;
        prelude_end := loc.Loc.end_pos;
        ignore (Cursor.next_raw r);
        gather ()
  in
  gather ();
  (* The block form knows it ran to EOF from [closed] and pays for the scan only
     then; a statement one cannot, because [cursor_of_rule] gives the at-rule a
     [;] whether the source ended in one or in EOF. A prelude that is already
     closed comes back unchanged, so close it either way - after the trim the
     printer applies, so the closer lands where the trim would have ended the
     text rather than after the run of whitespace it takes off. *)
  let prelude =
    if !prelude_start < 0 then ""
    else
      slice !prelude_start !prelude_end
      |> trim_unknown_at_prelude |> close_open_constructs
  in
  Unknown_at_rule { name; prelude; block = !block }

(* Reader for import_rule *)
let read_import_rule (r : Cursor.t) : import_rule =
  Cursor.ws r;
  read_import_prelude ~keep_url_repr:false r
