open Stylesheet_intf
open Stylesheet_at_rule
open Stylesheet_descriptor

type property_reader_state = {
  syntax : Variables.any_syntax option;
  inherits : bool option;
  initial_value : string option;
}

let css_wide_keyword s =
  match String.lowercase_ascii (String.trim s) with
  | "initial" | "inherit" | "unset" | "revert" | "revert-layer" -> true
  | _ -> false

(* An [initial-value] is registered before any element is styled, so one
   carrying an arbitrary substitution function has nothing to substitute from
   and cannot become the value the registration stores. Chrome 153 drops the
   whole rule at every syntax, the universal one included. That is the revision
   of Properties and Values API 1 this reader follows throughout, the one that
   also makes syntax, inherits and a non-universal initial-value required;
   today's ED sec. 3.3 ignores the descriptor alone instead.

   Sec. 4.1's computational independence is the second filter and reads the same
   way. A non-universal syntax registers its initial value before any element
   exists, so a length that resolves against one has nothing to resolve against;
   the universal syntax stores the value as written and takes every unit. The ED
   binds sec. 4.1 to registerProperty alone, and Chrome enforces it on the CSS
   path too: the maintainer settled on 2026-09-09 that cascade follows Chrome
   here, as it already does for the substitution half above. *)
let read_property_initial_value ~universal r syntax str =
  if css_wide_keyword str then
    Cursor.err_invalid r "@property: initial-value cannot be CSS-wide keyword";
  (match Variables.substitution_fn_in_value_string str with
  | Some fn ->
      Cursor.err_invalid r
        (String.concat ""
           [ "@property: initial-value cannot contain "; fn; "()" ])
  | None -> ());
  (if not universal then
     match Variables.element_relative_in_value_string str with
     | Some unit ->
         Cursor.err_invalid r
           (String.concat ""
              [
                "@property: initial-value cannot resolve against an element (";
                unit;
                ") at a non-universal syntax";
              ])
     | None -> ());
  let value_reader = Cursor.of_string str in
  let value = Variables.read_value value_reader syntax in
  Cursor.ws value_reader;
  Cursor.expect_eof value_reader;
  value

(* Every failure below is a failure of the [\@when] / [\@else] prelude, so it is
   raised on the cursor the prelude's own components sit in and the caret lands
   on the one that failed rather than on the block that follows it. *)
let conditional_terminated r ~at_rule (fn : Component.func Component.node) =
  if not fn.node.terminated then
    Cursor.err_condition r ~at_rule "unterminated condition function"

let conditional_atom r ~at_rule (fn : Component.func Component.node) =
  conditional_terminated r ~at_rule fn;
  match String.lowercase_ascii fn.Component.node.name with
  | "media" -> Media_condition (Media.of_function_components fn.node.arguments)
  | "supports" ->
      Supports_condition_test
        (Supports.of_string ~allow_unwrapped_decl:true
           (Cursor.string_of_components ~trim:true fn.node.arguments))
  | name ->
      Cursor.err_condition r ~at_rule ("unknown condition function: " ^ name)

let conditional_components ~at_rule cursor =
  let read_atom () =
    Cursor.ws cursor;
    match Cursor.peek cursor with
    | Some (Component.Func fn) ->
        let atom = conditional_atom cursor ~at_rule fn in
        Cursor.skip cursor;
        atom
    | _ -> Cursor.err_condition cursor ~at_rule "expected a condition function"
  in
  let mixed op = Cursor.err_condition cursor ~at_rule ("cannot mix " ^ op) in
  let rec chain op acc =
    Cursor.ws cursor;
    match Cursor.peek_keyword cursor with
    | Some "and" ->
        (match op with Some `Or -> mixed "or and and" | _ -> ());
        Cursor.skip cursor;
        chain (Some `And) (And (acc, read_atom ()))
    | Some "or" ->
        (match op with Some `And -> mixed "and and or" | _ -> ());
        Cursor.skip cursor;
        chain (Some `Or) (Or (acc, read_atom ()))
    | _ -> acc
  in
  let condition = chain None (read_atom ()) in
  Cursor.ws cursor;
  if not (Cursor.is_done cursor) then
    Cursor.err_condition cursor ~at_rule "trailing content";
  condition

let follows_conditional = function When _ | Else _ -> true | _ -> false

let scope_prelude r prelude_components : Selector.t option * Selector.t option =
  let prelude = Cursor.string_of_components ~trim:true prelude_components in
  let rec split_at_to seen rest =
    match rest with
    | [] -> (List.rev seen, [])
    | Component.Preserved { kind = Token.Ident s; _ } :: tail
      when String.lowercase_ascii s = "to" ->
        (List.rev seen, tail)
    | hd :: tail -> split_at_to (hd :: seen) tail
  in
  let strip_parens cvs =
    match Cursor.string_of_components ~trim:true cvs with
    | s
      when String.length s >= 2 && s.[0] = '(' && s.[String.length s - 1] = ')'
      ->
        (true, String.sub s 1 (String.length s - 2) |> String.trim)
    | s -> (false, s)
  in
  if prelude = "" then (Option.None, Option.None)
  else
    let start_cvs, end_cvs = split_at_to [] prelude_components in
    let start_parens, start = strip_parens start_cvs in
    let end_parens, end_ = strip_parens end_cvs in
    if start_parens && start = "" then
      Cursor.err_invalid r "@scope start selector cannot be empty";
    if end_cvs <> [] && end_parens && end_ = "" then
      Cursor.err_invalid r "@scope end selector cannot be empty";
    (* CSS Cascade 6 sec. 3.5.2 gives each bound a [<complex-selector-list>], so
       a bound that does not parse leaves the prelude invalid and the rule with
       it, which is what Chrome 146 does. [Selector.Invalid] stood here as a
       placeholder, but it is the [:invalid] pseudo-class: emitting it scoped
       the block to whatever form controls are in an invalid state. *)
    let opt s : Selector.t option =
      if s = "" then None
      else
        match Selector.of_string s with
        | sel -> Some sel
        | exception Invalid_argument m -> Cursor.err_invalid r m
    in
    (opt start, opt end_)

let read_supports_condition (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports-condition" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid r "@supports-condition: invalid custom-property name";
  Cursor.ws r;
  let declarations = Cursor.braces Declaration.read_declarations r in
  Supports_condition (name, declarations)

let read_layer_name (r : Cursor.t) : layer_name = read_layer_name_component r

(* Media Queries 5 sec. 3.2: "A media query that does not match the grammar
   [...] must be replaced by not all during parsing", so a prelude the reader
   refuses leaves the rule standing and matching nothing rather than taking the
   rule and its contents with it. The replacement is per entry of the list,
   which the recovering read does; the strict one runs first for the error to
   report, since the recovery is {!Error.Recovery.Recovered}, the rule
   surviving. *)
let read_nested_media_condition t =
  if Cursor.is_done t then Media.List []
  else
    try Media.read ~recover:false t
    with Error.Parse_error e when Cursor.recover t ->
      Cursor.push_warning t ~recovery:Error.Recovery.Recovered e;
      Media.read ~recover:true t

let read_rule_selector ?(nested = false) r =
  let prelude = Cursor.drain_until_block r in
  (* Anchor selector-level EOF errors at the block's opening delimiter so a
     prelude like [.a >] reports its error just before the [{], not at the end
     of the whole stylesheet. The block itself stays in the parent cursor. *)
  let eof_loc =
    match Cursor.peek r with
    | Some (Component.Block { loc; _ }) ->
        Some (Loc.v ~start_pos:loc.start_pos ~end_pos:loc.start_pos)
    | _ -> None
  in
  let c = Cursor.sub ?eof_loc r prelude in
  (* Re-raise the original [Parse_error] so its loc/kind/path/snippet reach the
     caller intact, rather than rewrapping (which erases the structured error
     and relocates it). CSS Nesting 1 sec. 3: a nested rule's prelude is a
     [<relative-selector-list>], so it may start with a combinator ([> .bar])
     implicitly relative to the parent [&]. *)
  Cursor.with_context c "selector" (fun () ->
      if nested then Selector.read_relative c
      else Selector.read_strict_selector_list c)

let is_bare_nesting_selector : Selector.t -> bool = function
  | Selector.Nesting -> true
  | _ -> false

let validate_nested_rule_selector inner selector nested_selector =
  if
    is_bare_nesting_selector selector
    && is_bare_nesting_selector nested_selector
  then
    Cursor.err_invalid inner
      "bare nesting selector cannot directly nest another bare nesting selector"

(* CSS Nesting 1 sec. 3.3: "any at-rule whose body contains style rules can be
   nested inside of a style rule as well, unless otherwise specified". A
   descriptor rule, a keyframe list and a declaration-list rule contain none, so
   none of them nests, and Blink 146 drops each one. [\@view-transition] is a
   descriptor rule too, yet Blink keeps it inside a style rule; dropping what a
   shipping engine still reads is the one lossy direction, so it stays. *)
let nests_in_style_rule = function
  | "keyframes" | "-webkit-keyframes" | "-moz-keyframes" | "font-face"
  | "counter-style" | "page" | "font-palette-values" | "font-feature-values"
  | "position-try" | "viewport" | "-ms-viewport" | "property"
  | "supports-condition" ->
      false
  | _ -> true

(* One descriptor of an [@property] body. The state is returned rather than
   assigned as the value is read, so a descriptor that fails leaves the ones
   before it untouched. CSS Properties and Values API 1 sec. 2 gives each
   descriptor a grammar of its own, and the declaration is the whole of it: a
   trailing [!important] or a stray ident makes the declaration invalid rather
   than the leftover alone, as it does in Blink 146. *)
let read_property_descriptor (r : Cursor.t) state =
  let key = Cursor.ident ~keep_case:false r in
  Cursor.ws r;
  if not (Cursor.colon r) then Cursor.err_expected r "':'";
  Cursor.ws r;
  let state =
    match key with
    (* PROPERTY_DESCRIPTOR_START - Used by test/spec/browser *)
    | "syntax" -> { state with syntax = Some (Variables.read_syntax r) }
    | "inherits" -> { state with inherits = Some (Cursor.bool r) }
    | "initial-value" ->
        {
          state with
          initial_value = Some (Cursor.consume_until_semicolon ~trim:true r);
        }
    (* PROPERTY_DESCRIPTOR_END - Used by test/spec/browser *)
    | _ -> Cursor.err_invalid r "unknown property descriptor"
  in
  Cursor.ws r;
  if not (Cursor.is_done r || Cursor.peek_semicolon r) then
    Cursor.err_invalid r ("trailing tokens after @property " ^ key);
  state

let read_property_step r state =
  Cursor.ws r;
  if Cursor.is_done r then `Done state
  else if Cursor.peek_semicolon r then (
    Cursor.skip r;
    `More state)
  else `More (read_property_descriptor r state)

let read_property_descriptors (r : Cursor.t) : property_reader_state =
  read_items_with_recovery ~skip:skip_invalid_item
    ~construct:Error.Recovery.Declaration read_property_step r
    { syntax = None; inherits = None; initial_value = None }

let read_property_rule (r : Cursor.t) : statement =
  (* Read @property descriptors as a separate helper to keep the reader tidy. *)
  Cursor.expect_at_keyword "property" r;
  Cursor.ws r;
  let name = Cursor.ident ~keep_case:true r in
  if not (Custom_property_name.is_valid name) then
    Cursor.err_invalid r ("@property: invalid custom-property name: " ^ name);
  Cursor.ws r;
  let state = Cursor.braces (fun inner -> read_property_descriptors inner) r in
  match (state.syntax, state.inherits) with
  | None, _ ->
      Cursor.err_invalid r "@property: missing required 'syntax' descriptor"
  | _, None ->
      Cursor.err_invalid r "@property: missing required 'inherits' descriptor"
  | Some (Variables.Syntax syntax), Some inherits ->
      let is_universal_syntax =
        match syntax with Universal -> true | _ -> false
      in
      let initial_value =
        match state.initial_value with
        | None when not is_universal_syntax ->
            Cursor.err_invalid r
              "@property: initial-value is required for non-universal syntax"
        | None -> Option.None
        | Some str ->
            Some
              (read_property_initial_value ~universal:is_universal_syntax r
                 syntax str)
      in
      Property { name; syntax; inherits; initial_value }

(* Does the item ahead hold a curly block before its terminating [;]? Then it is
   a nested rule, not a declaration, however much its prelude looks like one.
   Leaves the cursor where it found it. *)
(* CSS Syntax 3 (ED) sec. 5.5.3 consumes a qualified rule whole, block and all,
   before deciding it is invalid, so sec. 5.5.5 resumes right after that block.
   Recovering a rule to the next semicolon, which is what sec. 5.5.6 does for a
   bad declaration, would take the items written after it. The caller rewinds
   first so the item is skipped from its start. *)
let item_opens_block inner =
  let start = Cursor.save inner in
  let rec scan () =
    match Cursor.peek inner with
    | None -> false
    | Some (Component.Preserved { kind = Token.Semicolon; _ }) -> false
    | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) -> true
    | Some _ ->
        Cursor.skip inner;
        scan ()
  in
  let found = scan () in
  Cursor.restore inner start;
  found

(* CSS Syntax 3 (ED) sec. 5.5.3 drops a qualified rule whose prelude is an ident
   starting with [--] followed by a colon, block and all: that item is a
   declaration however much it looks like a rule, so sec. 5.5.6 recovery takes
   it, not sec. 5.5.5's. *)
let item_is_custom_property inner =
  let start = Cursor.save inner in
  let answer =
    match Cursor.peek inner with
    | Some (Component.Preserved { kind = Token.Ident name; _ })
      when Custom_property_name.has_prefix name ->
        Cursor.skip inner;
        Cursor.ws inner;
        Cursor.peek_colon inner
    | _ -> false
  in
  Cursor.restore inner start;
  answer

let skip_invalid_nesting_item r =
  if item_opens_block r && not (item_is_custom_property r) then (
    skip_past_rule r;
    Error.Recovery.Rule)
  else (
    Cursor.skip_past_semicolon r;
    Error.Recovery.Declaration)

(* Discard an at-rule that is invalid in a style rule and resume at the next
   item, the recovery CSS Syntax 3 (ED) sec. 5.5.5 describes. The warning is
   what [~strict:true] turns into an error. *)
let drop_nested_at_rule r ~loc reason : statement option =
  skip_past_rule r;
  Cursor.push_warning r
    ~recovery:Error.Recovery.(dropped Rule)
    (Error.bad_value loc ~property:"rule" ~reason);
  None

(* CSS Nesting 1 sec. 3.4 wraps a run of declarations written after a nested
   statement in a nested declarations rule, so it keeps its place among them.
   The run a rule body is reading is the head of its [nested] accumulator and
   holds its declarations reversed, like [decls]; sealing it puts them back in
   source order. *)
let seal_declaration_run = function
  | Declarations run :: rest -> Declarations (List.rev run) :: rest
  | nested -> nested

(* Add one declaration to the run being read, which is the head of the
   accumulator and holds its declarations reversed until it is sealed. *)
let add_to_declaration_run decl = function
  | Declarations run :: rest -> Declarations (decl :: run) :: rest
  | nested -> Declarations [ decl ] :: nested

let read_starting_style ~body (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "starting-style" r;
  Cursor.ws r;
  Starting_style (Cursor.braces body r)

let read_when ~body (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "when" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  if Cursor.string_of_components ~trim:true prelude = "" then
    Cursor.err_condition r ~at_rule:"@when" "missing condition";
  let condition : conditional =
    conditional_components ~at_rule:"@when" (Cursor.sub r prelude)
  in
  When (condition, Cursor.braces body r)

let read_else ~body (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "else" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block r in
  let condition =
    match
      List.filter
        (function
          | Component.Preserved { kind = Token.Whitespace _; _ } -> false
          | _ -> true)
        prelude
    with
    | [] -> Option.None
    | _ -> Some (conditional_components ~at_rule:"@else" (Cursor.sub r prelude))
  in
  Else (condition, Cursor.braces body r)

let read_moz_document ~body (r : Cursor.t) : statement =
  Cursor.with_context r "@-moz-document" @@ fun () ->
  Cursor.expect_at_keyword "-moz-document" r;
  Cursor.ws r;
  let prelude = Cursor.drain_until_block_as_string ~trim:true r in
  let prelude_cursor = Cursor.of_string prelude in
  let conditions =
    Cursor.list ~sep:Cursor.comma ~at_least:1 read_moz_document_condition
      prelude_cursor
  in
  Cursor.ws prelude_cursor;
  Cursor.expect_eof prelude_cursor;
  Moz_document (conditions, Cursor.braces body r)

(* CSS Values 4 sec. 4.1 reads an at-rule name as a keyword, so its case does
   not pick between two grammars. CSS Syntax 3 (ED) sec. 8.3 is the one
   exception: it matches [@charset] and the quote after it as an exact byte
   sequence, so any other spelling of that name is a name with no grammar behind
   it. *)
let at_rule_keyword name =
  match Common.String.lowercase_ascii_preserve name with
  | "charset" when name <> "charset" -> name
  | folded -> folded

let rec read_statement (r : Cursor.t) : statement =
  Cursor.ws r;
  let table : (string * (Cursor.t -> statement)) list =
    [
      ("charset", read_charset);
      ("import", read_import);
      ("namespace", read_namespace);
      ("layer", read_layer);
      ("media", read_media);
      ("container", read_container);
      ("supports", read_supports);
      ("-moz-document", read_moz_document ~body:read_block);
      ("when", read_when ~body:read_block);
      ("else", read_else ~body:read_block);
      ("supports-condition", read_supports_condition);
      ("starting-style", read_starting_style ~body:read_block);
      ("scope", read_scope);
      ("keyframes", read_keyframes);
      ("-webkit-keyframes", read_webkit_keyframes);
      ("-moz-keyframes", read_moz_keyframes);
      ("font-face", read_font_face);
      ("counter-style", read_counter_style);
      ("page", read_page);
      ("font-palette-values", read_font_palette_values);
      ("font-feature-values", read_font_feature_values);
      ("view-transition", read_view_transition);
      ("position-try", read_position_try);
      ("viewport", read_viewport);
      ("-ms-viewport", read_ms_viewport);
      ("property", read_property_rule);
    ]
  in
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) -> (
      match List.assoc_opt (at_rule_keyword name) table with
      | Some p -> p r
      | None ->
          (* CSS Syntax 3 (ED) sec. 5.5.2 consumes an at-rule whatever its name,
             so the prelude and block stay in the AST as [Unknown_at_rule] and
             reach the output. The typed warning tells the caller cascade could
             not interpret it; [Optimize.drop_unknown_at_rules] is there for a
             caller that wants it gone. *)
          ignore (Cursor.next_raw r);
          let stmt = read_unknown_at_rule name r in
          Cursor.push_warning r ~recovery:Error.Recovery.Recovered
            (Error.unknown_at_rule loc name);
          stmt)
  | _ -> Rule (read_rule r)

and read_block (r : Cursor.t) : block =
  let rec read_statements acc =
    Cursor.ws r;
    if Cursor.is_done r then List.rev acc
    else if
      (* CSS Syntax 3 (ED) sec. 5.5.5 discards a [;] among a block's contents,
         so a stray one costs nothing and the rule after it reads. Chrome 151
         reads it as the start of a prelude instead and loses the block. *)
      match Cursor.peek r with
      | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
          Cursor.skip r;
          true
      | _ -> false
    then read_statements acc
    else
      let loc = Cursor.position r in
      let snap = Cursor.save r in
      match read_statement r with
      (* CSS Syntax 3 (ED) sec. 5.5.1: a rule that fails to parse (e.g. an
         invalid selector) is dropped, and parsing resumes at the next rule -
         one bad rule must not take the rest of the [@layer] / [@media] block
         with it. The guard leaves the exception to a caller reading from a
         non-recovering cursor; no statement parse builds one. *)
      | exception Error.Parse_error e when Cursor.recover r ->
          Cursor.restore r snap;
          Cursor.push_warning r ~recovery:Error.Recovery.(dropped Rule) e;
          skip_past_rule r;
          read_statements acc
      | Import _ ->
          (* CSS Cascade L6 sec. 2: @import is only valid at the top of the
             stylesheet. Drop a misplaced one rather than emitting it. *)
          Cursor.push_warning r
            ~recovery:Error.Recovery.(dropped Rule)
            (Error.bad_value loc ~property:"stylesheet"
               ~reason:"@import is only valid at the top of a stylesheet");
          read_statements acc
      | Else _ when not (List.exists follows_conditional acc) ->
          Cursor.err_invalid r "@else without preceding @when"
      | stmt -> read_statements (stmt :: acc)
  in
  read_statements []

and read_media (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "media" r;
  Cursor.ws r;
  let condition_components = Cursor.drain_until_block r in
  let query = Cursor.sub r condition_components in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition = read_nested_media_condition query in
  List.iter
    (fun w -> Cursor.push_warning r ~recovery:Error.Recovery.Recovered w)
    (Cursor.drain_warnings query);
  Media (condition, content)

and read_supports (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "supports" r;
  Cursor.ws r;
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  if Cursor.is_done query then
    Cursor.err r "@supports rule requires a condition";
  let content = Cursor.braces (fun inner -> read_block inner) r in
  Supports (Supports.read query, content)

and read_scope (r : Cursor.t) : statement =
  (* CSS Cascade 6 sec. 3.5.2 spells it [@scope <scope-boundaries>? {
     <block-contents> }], and [<block-contents>] holds declarations as well as
     rules: a declaration written straight into the body applies to the scoping
     root, which Chrome reports as a [CSSNestedDeclarations]. The two selectors
     are kept as raw strings. *)
  Cursor.expect_at_keyword "scope" r;
  Cursor.ws r;
  let prelude_components = Cursor.drain_until_block r in
  let scope_start, scope_end = scope_prelude r prelude_components in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Scope (scope_start, scope_end, content)

and read_container (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "container" r;
  Cursor.ws r;
  (* CSS Containment 3 section 4: [<container-name>] is a [<custom-ident>] that
     excludes the keywords [none], [and], [not], [or]. Without this filter,
     [@container not (width)] would parse [not] as the name and accept the
     malformed [(width)] as the query. *)
  let container_name : string option =
    let snap = Cursor.save r in
    match Cursor.option Cursor.ident r with
    | Some name
      when List.mem (String.lowercase_ascii name) [ "none"; "and"; "not"; "or" ]
      ->
        Cursor.restore r snap;
        Option.None
    | other -> other
  in
  Cursor.ws r;
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_block inner) r in
  let condition : Container.t option =
    if Cursor.is_done query then Option.None else Some (Container.read query)
  in
  (* CSS Containment 3 section 4: [@container] requires a query (with an
     optional [<container-name>] in front). Bare [@container { ... }] is a parse
     error. *)
  if container_name = None && condition = None then
    Cursor.err_invalid r "@container requires a container query";
  Container (container_name, condition, content)

and read_layer (r : Cursor.t) : statement =
  Cursor.expect_at_keyword "layer" r;
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      let content = Cursor.braces (fun inner -> read_block inner) r in
      Layer (None, content)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.err_invalid r "@layer requires at least one name"
  | _ -> (
      let first = read_layer_name r in
      Cursor.ws r;
      match Cursor.peek r with
      | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
          Cursor.skip r;
          Layer_decl [ first ]
      | Some (Component.Preserved { kind = Token.Comma; _ }) ->
          Cursor.skip r;
          Cursor.ws r;
          let rest =
            Cursor.list ~sep:Cursor.comma ~at_least:1
              (fun r ->
                Cursor.ws r;
                read_layer_name r)
              r
          in
          Cursor.ws r;
          (match Cursor.peek r with
          | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
              Cursor.skip r
          | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
              Cursor.err_invalid r
                "@layer with multiple names cannot have a block"
          | _ -> ());
          Layer_decl (first :: rest)
      | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
          let content = Cursor.braces (fun inner -> read_block inner) r in
          Layer (Some first, content)
      | _ -> Cursor.err_invalid r "expected ';' or '{' after @layer name")

(* Helper: Read a block that can contain either bare declarations or statements.
   Used for CSS nesting contexts where content inside @media/@supports/etc can
   be either bare declarations (inheriting the parent selector) or nested
   rules. *)
and read_nesting_block (r : Cursor.t) : block =
  let rec read_items acc =
    Cursor.ws r;
    if Cursor.recover r then read_recovering_item acc
    else add_item acc (read_nesting_item ~prev:acc r)
  (* CSS Syntax 3 (ED) sec. 5.5.5: a declaration that fails to parse is dropped
     and reading resumes past the next top-level [;], a [{}] met on the way
     counting as one component value of the value being skipped. A nested
     at-rule's body is <block-contents> like a style rule's, so it recovers the
     same way and one bad declaration takes neither the group rule holding it
     nor the rest of the sheet. The guard leaves the failure to a caller reading
     from a non-recovering cursor; no statement parse builds one. *)
  and read_recovering_item acc =
    let start = Cursor.save r in
    match read_nesting_item ~prev:acc r with
    | item -> add_item acc item
    | exception Error.Parse_error e ->
        Cursor.restore r start;
        let construct = skip_invalid_nesting_item r in
        Cursor.push_warning r
          ~recovery:(Cursor.dropped_since r start construct)
          e;
        read_items acc
  and add_item acc = function
    | `Done -> List.rev (seal_declaration_run acc)
    | `Skip -> read_items acc
    | `Decl decl -> read_items (add_to_declaration_run decl acc)
    | `Stmt stmt -> read_items (stmt :: seal_declaration_run acc)
  in
  read_items []

and read_nesting_item ~prev r =
  match Cursor.peek r with
  | None -> `Done
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) -> (
      match read_nested_at_within_rule ~prev r with
      | Some stmt -> `Stmt stmt
      | None -> `Skip)
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip r;
      `Skip
  | _ -> read_nesting_declaration_or_statement r

and read_nesting_declaration_or_statement r =
  let start = Cursor.save r in
  match Declaration.read_declaration r with
  | Some decl ->
      Cursor.ws r;
      if Cursor.peek_semicolon r then Cursor.skip r;
      `Decl decl
  | None -> `Stmt (read_statement r)
  (* CSS Nesting 1 sec. 3 lets a nested rule start with an identifier, so
     [h2:where(...) { ... }] reads as a declaration up to the [{]. Rewind and
     take it as a rule; a genuine bad declaration has no block and still reports
     as one, and a [--x:] prelude is a declaration whatever follows it. *)
  | exception Error.Parse_error _
    when Cursor.restore r start;
         item_opens_block r && not (item_is_custom_property r) ->
      `Stmt (read_statement r)

(* Helper: Read nested at-rule with declarations content *)
and read_nested_at_rule (r : Cursor.t) (at_rule : string) : statement =
  Cursor.with_context r at_rule @@ fun () ->
  let name = String.sub at_rule 1 (String.length at_rule - 1) in
  Cursor.expect_at_keyword name r;
  Cursor.ws r;
  match at_rule with
  | "@container" -> read_nested_container_rule r
  | "@supports" -> read_nested_supports_rule r
  | "@media" -> read_nested_media_rule r
  | "@scope" -> read_nested_scope_rule r
  | _ -> Cursor.err_invalid r ("Unexpected nested at-rule: " ^ at_rule)

and read_nested_container_rule r =
  let container_name : string option = Cursor.option Cursor.ident r in
  Cursor.ws r;
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  let condition : Container.t option =
    if Cursor.is_done query then Option.None else Some (Container.read query)
  in
  Container (container_name, condition, content)

and read_nested_supports_rule r =
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Supports (Supports.read query, content)

and read_nested_media_rule r =
  let query = Cursor.sub r (Cursor.drain_until_block r) in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Media (read_nested_media_condition query, content)

and read_nested_scope_rule r =
  let prelude_components = Cursor.drain_until_block r in
  let scope_start, scope_end = scope_prelude r prelude_components in
  let content = Cursor.braces (fun inner -> read_nesting_block inner) r in
  Scope (scope_start, scope_end, content)

and read_nested_layer_rule r =
  Cursor.expect_at_keyword "layer" r;
  Cursor.ws r;
  match Cursor.peek r with
  | Some (Component.Block { node = { opening = Token.Curly; _ }; _ }) ->
      Layer (None, Cursor.braces (fun inner -> read_nesting_block inner) r)
  | _ ->
      let name = read_layer_name r in
      Cursor.ws r;
      Layer (Some name, Cursor.braces (fun inner -> read_nesting_block inner) r)

and read_nested_at_within_rule ~prev (r : Cursor.t) : statement option =
  match Cursor.peek r with
  | Some (Component.Preserved { kind = Token.At_keyword name; loc; _ }) ->
      read_nested_at_keyword r ~loc ~prev (at_rule_keyword name)
  | _ -> Some (read_statement r)

(* CSS Nesting 1 sec. 3.3: "any at-rule whose body contains style rules can be
   nested inside of a style rule as well", which is every group rule cascade
   models - the conditional group rules @media/@supports/@container, @when and
   @else that generalise them (CSS Conditional 5 sec. 3, sec. 4),
   @-moz-document, @layer, @scope, and @starting-style (CSS Transitions 2 sec.
   3.3). Nested, the body is <block-contents>, so a bare declaration run in it
   belongs to the parent selector. An at-rule with no style rule in its body is
   invalid here and dropped; a top-of-sheet rule is invalid here outright. *)
and read_nested_at_keyword r ~loc ~prev = function
  | ("supports" | "media" | "container" | "scope") as name ->
      Some (read_nested_at_rule r ("@" ^ name))
  | "layer" -> Some (read_nested_layer_rule r)
  | "starting-style" -> Some (read_starting_style ~body:read_nesting_block r)
  | "-moz-document" -> Some (read_moz_document ~body:read_nesting_block r)
  | "when" -> Some (read_when ~body:read_nesting_block r)
  | "else" when List.exists follows_conditional prev ->
      Some (read_else ~body:read_nesting_block r)
  | "else" -> drop_nested_at_rule r ~loc "@else without preceding @when"
  | ("charset" | "import" | "namespace") as name ->
      drop_nested_at_rule r ~loc
        ("@" ^ name ^ " is only valid at the top of a stylesheet")
  | name when not (nests_in_style_rule name) ->
      drop_nested_at_rule r ~loc
        ("@" ^ name ^ " has no style rule in it, so it does not nest")
  | _ -> Some (read_statement r)

and read_rule_item selector inner decls nested =
  match Cursor.peek inner with
  | None -> `Done (List.rev decls, List.rev (seal_declaration_run nested))
  | Some (Component.Preserved { kind = Token.At_keyword _; _ }) -> (
      match read_nested_at_within_rule ~prev:nested inner with
      | Some stmt -> `Continue (decls, stmt :: seal_declaration_run nested)
      (* A dropped at-rule leaves no gap: the declaration run written around it
         is one run, as it is in Blink. *)
      | None -> `Continue (decls, nested))
  | Some (Component.Preserved { kind = Token.Semicolon; _ }) ->
      Cursor.skip inner;
      `Continue (decls, nested)
  | _ -> read_rule_decl_or_nested selector inner decls nested

and read_rule_decl_or_nested selector inner decls nested =
  let start = Cursor.save inner in
  match Declaration.read_declaration inner with
  | Some d -> (
      Cursor.ws inner;
      if Cursor.peek_semicolon inner then Cursor.skip inner;
      (* Only the run written before the first nested statement is the rule's
         own; a later one joins the block it follows. *)
      match nested with
      | [] -> `Continue (d :: decls, nested)
      | Declarations run :: rest ->
          `Continue (decls, Declarations (d :: run) :: rest)
      | _ -> `Continue (decls, Declarations [ d ] :: nested))
  | None -> read_nested_rule_or_done selector inner decls nested
  (* CSS Nesting 1 sec. 3 lets a nested rule start with an identifier, so
     [h2:where(...) { ... }] reads as a declaration up to the [{]. Rewind and
     take it as a rule; a genuine bad declaration has no block and still reports
     as one, and a [--x:] prelude is a declaration whatever follows it. *)
  | exception Error.Parse_error _
    when Cursor.restore inner start;
         item_opens_block inner && not (item_is_custom_property inner) ->
      read_nested_rule_or_done selector inner decls nested

and read_nested_rule_or_done selector inner decls nested =
  if Cursor.is_done inner then
    `Done (List.rev decls, List.rev (seal_declaration_run nested))
  else
    let nr = read_rule ~nested:true inner in
    validate_nested_rule_selector inner selector nr.selector;
    `Continue (decls, Rule nr :: seal_declaration_run nested)

and read_rule_body selector inner =
  let rec loop decls nested =
    Cursor.ws inner;
    if Cursor.recover inner then
      read_recovering_rule_item selector inner loop decls nested
    else
      match read_rule_item selector inner decls nested with
      | `Done result -> result
      | `Continue (decls, nested) -> loop decls nested
  in
  loop [] []

and read_recovering_rule_item selector inner loop decls nested =
  let start = Cursor.save inner in
  match read_rule_item selector inner decls nested with
  | `Done result -> result
  | `Continue (decls, nested) -> loop decls nested
  | exception Error.Parse_error e ->
      Cursor.restore inner start;
      let construct = skip_invalid_nesting_item inner in
      Cursor.push_warning inner
        ~recovery:(Cursor.dropped_since inner start construct)
        e;
      loop decls nested

and read_rule ?(nested = false) (r : Cursor.t) : rule =
  Cursor.with_context r "rule" @@ fun () ->
  let selector = read_rule_selector ~nested r in
  let declarations, nested = Cursor.braces (read_rule_body selector) r in
  { selector; declarations; nested; merge_key = None }

(* The order CSS fixes at the head of a sheet. CSS Syntax 3 sec. 3.2 honours
   [@charset] only as the first bytes of the sheet. CSS Cascade 5 sec. 2 puts
   [@import] before every other valid at-rule and style rule but [@charset] and
   [@layer] statements, with nothing valid between two imports, and CSS
   Namespaces 3 sec. 3 puts [@namespace] after the imports and before every
   other rule. A browser ignores a comment, an at-rule it has no grammar for and
   a misplaced rule, so none of them moves the prelude on. *)
type prelude = Start | Before_imports | Imports | Namespaces | Body

let prelude_start = Start

let misplaced prelude stmt =
  match stmt with
  | Charset _ -> (
      match prelude with
      | Start -> Option.None
      | Before_imports | Imports | Namespaces | Body ->
          Option.Some "@charset must precede all rules")
  | Import _ -> (
      match prelude with
      | Start | Before_imports | Imports -> Option.None
      | Namespaces | Body -> Option.Some "@import must precede style rules")
  | Namespace _ -> (
      match prelude with
      | Start | Before_imports | Imports | Namespaces -> Option.None
      | Body -> Option.Some "@namespace must precede style rules")
  | Rule _ | Declarations _ | Bang_comment _ | Property _ | Layer_decl _
  | Layer _ | Media _ | Container _ | Supports _ | Moz_document _
  | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
  | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _ | Font_face _
  | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
  | Unknown_at_rule _ ->
      Option.None

let prelude_next prelude stmt =
  if Option.is_some (misplaced prelude stmt) then prelude
  else
    match stmt with
    | Bang_comment _ | Charset _ | Unknown_at_rule _ -> (
        match prelude with
        | Start -> Before_imports
        | Before_imports | Imports | Namespaces | Body -> prelude)
    | Layer_decl _ -> (
        match prelude with
        | Start | Before_imports -> Before_imports
        | Imports | Namespaces | Body -> Body)
    | Import _ -> Imports
    | Namespace _ -> Namespaces
    | Rule _ | Declarations _ | Property _ | Layer _ | Media _ | Container _
    | Supports _ | Moz_document _ | Starting_style _ | When _ | Else _
    | Supports_condition _ | Origin _ | Scope _ | Keyframes _
    | Webkit_keyframes _ | Moz_keyframes _ | Font_face _ | Counter_style _
    | Page _ | Page_with_margins _ | Font_palette_values _
    | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _ ->
        Body

let validate_stylesheet_else r acc stmt =
  match stmt with
  | Else _ when not (List.exists follows_conditional acc) ->
      Cursor.err_invalid r "@else without preceding @when"
  | _ -> ()

let rec read_stylesheet_statements r prelude acc =
  Cursor.ws r;
  if Cursor.is_done r then List.rev acc
  else
    let stmt = read_statement r in
    validate_stylesheet_else r acc stmt;
    Option.iter (Cursor.err_invalid r) (misplaced prelude stmt);
    read_stylesheet_statements r (prelude_next prelude stmt) (stmt :: acc)

let read (r : Cursor.t) : stylesheet =
  Cursor.with_context r "stylesheet" (fun () ->
      read_stylesheet_statements r prelude_start [])
