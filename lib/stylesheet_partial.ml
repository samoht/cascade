open Stylesheet_intf
open Stylesheet_walk
open Stylesheet_read

(* Replay a Parser-recovered rule as a Cursor by flattening its components.
   [Component.at_rule] stores the at-keyword as a bare [name] string; the
   original [At_keyword] token was already consumed during section 5.3, so
   synthesize one at the rule's location so dispatch in [read_statement] still
   sees the opening [\@name]. [?source] flows through so errors raised by
   validators downstream pick up source-context snippets, all of them sharing
   its one index. *)
let cursor_of_rule ?source ?meta ?enforce_spec : Component.rule -> Cursor.t =
  function
  | Qualified { node = { prelude; block }; _ } ->
      Cursor.of_components ?source ?meta ?enforce_spec ~recover:true
        (prelude @ [ Component.Block block ])
  | At { node = { name; prelude; block }; loc } ->
      let at_kw = Token.v ~kind:(Token.At_keyword name) ~loc in
      let at_cv = Component.Preserved at_kw in
      let tail_cv =
        match block with
        | Some b -> [ Component.Block b ]
        | None ->
            (* Section 5.5.2 "consume an at-rule" terminates on [;] or EOF and
               drops the terminator. Readers like [read_layer] and
               [read_charset] then expect a trailing semicolon; synthesize one
               so the replayed cursor matches the original token shape
               regardless of whether [;] or EOF ended the rule. *)
            [ Component.Preserved (Token.v ~kind:Token.Semicolon ~loc) ]
      in
      Cursor.of_components ?source ?meta ?enforce_spec ~recover:true
        ((at_cv :: prelude) @ tail_cv)

(* Validate one Parser-recovered rule to a typed statement, or convert the
   validator's [Parse_error] into a rule-level error and drop the rule. Per-
   declaration warnings accumulated on the cursor are drained separately by the
   caller. *)
let read_statement_of_rule ?source ?meta ?enforce_spec (rule : Component.rule) :
    Cursor.t * (statement, Error.t) result =
  let r = cursor_of_rule ?source ?meta ?enforce_spec rule in
  let result =
    match read_statement r with
    | stmt -> Ok stmt
    | exception Error.Parse_error e -> Error e
  in
  (r, result)

(* A check on a whole statement reports against the statement, in the source [r]
   read it from. *)
let invalid r loc reason =
  Cursor.error ~sort:Sort.Stylesheet r ~loc (Error.Invalid_statement reason)

let validate_partial_prelude () =
  let prelude = ref prelude_start in
  fun r loc stmt ->
    let misplaced = misplaced !prelude stmt in
    prelude := prelude_next !prelude stmt;
    Option.map (invalid r loc) misplaced

let validate_partial_statement r loc = function
  | Rule { selector; _ } when Selector.matches_nothing selector ->
      Some
        (Cursor.error ~sort:Sort.Selector r ~loc
           (Error.Bad_selector "selector matches nothing"))
  | Font_face descriptors when not (font_face_participates descriptors) ->
      Some (invalid r loc "@font-face without a font-family or src descriptor")
  (* CSS Fonts 4 sec. 9.2 makes font-family the one mandatory descriptor; sec.
     9.2.2 defaults a missing base-palette to 0. *)
  | Font_palette_values (_, descriptors)
    when not
           (List.exists
              (function Palette_font_family _ -> true | _ -> false)
              descriptors) ->
      Some
        (invalid r loc "@font-palette-values without a font-family descriptor")
  | _ -> None

(* [fold_declarations] reaches every declaration site, the frames of
   [@keyframes] and the margin boxes of [@page] included. *)
let statement_has_declaration p stmt =
  fold_declarations
    (fun found decls -> found || List.exists p decls)
    false [ stmt ]

let validate_partial_invalid_declarations r loc stmt =
  if statement_has_declaration Declaration.is_invalid stmt then
    Some (invalid r loc "invalid declaration value")
  else None

let rec declaration_has_strict_warning = function
  | Declaration.Declaration { property = Properties.Color; value; _ } ->
      Values.color_has_specified_hue value
  | Declaration.Theme_guarded { decl; _ } -> declaration_has_strict_warning decl
  | _ -> false

let validate_partial_strict_warnings r loc stmt =
  if statement_has_declaration declaration_has_strict_warning stmt then
    Some (invalid r loc "strict stylesheet warning")
  else None

(* A warning drained from a cursor is stamped where the cursor recorded it. *)
let add_warning warnings warning = warnings := warning :: !warnings

(* A warning raised beside the statement rather than inside it, so this site
   says what became of the statement. *)
let add_statement_warning warnings ~recovery warning =
  add_warning warnings (Error.with_recovery recovery warning)

let drain_statement_warnings warnings cursor =
  List.iter (add_warning warnings) (Cursor.drain_warnings cursor)

let validate_else_orphan r previous rule stmt =
  match stmt with
  | Else _
    when not (Option.fold ~none:false ~some:follows_conditional !previous) ->
      Some (invalid r (Component.rule_loc rule) "@else without preceding @when")
  | _ -> None

(* These four run on a statement the reader read whole and the sheet keeps, so
   the material each reports on reaches the output. *)
let validate_partial_statement_warnings r warnings validate_prelude rule stmt =
  let loc = Component.rule_loc rule in
  let add =
    Option.iter
      (add_statement_warning warnings ~recovery:Error.Recovery.Recovered)
  in
  add (validate_prelude r loc stmt);
  add (validate_partial_statement r loc stmt);
  add (validate_partial_invalid_declarations r loc stmt);
  add (validate_partial_strict_warnings r loc stmt)

type reading = {
  source : Error.Source.t option;
  meta : Loc.meta_level option;
  enforce_spec : bool;
  warnings : Error.t list ref;
  validate_prelude : Cursor.t -> Loc.t -> statement -> Error.t option;
  (* CSS Conditional Rules 5 section 4: [@else] is a continuation rule and must
     follow [@when] or another [@else]. A bare top-level [@else] is a parse
     error. *)
  previous : statement option ref;
}

let reading ?source ?meta ?(enforce_spec = false) () =
  {
    source;
    meta;
    enforce_spec;
    warnings = ref [];
    validate_prelude = validate_partial_prelude ();
    previous = ref Option.None;
  }

(* What becomes of [rule], read through [cursor] to [result]. *)
let finish_read t cursor result rule : statement option =
  let source = t.source and warnings = t.warnings in
  (* [rule_loc] spans the rule the parser handed over, prelude and block alike,
     so it names what a drop costs the sheet. The error's own location marks
     where the read gave up inside it. *)
  let lost () =
    let text = Option.map Error.Source.text source in
    Error.Recovery.(dropped ?source:text ~loc:(Component.rule_loc rule) Rule)
  in
  (* Drain declaration-level warnings first so source order is preserved: decl
     warnings come from inside the rule, the rule-level error (if any) comes
     after them. *)
  drain_statement_warnings warnings cursor;
  match result with
  | Ok stmt -> (
      validate_partial_statement_warnings cursor warnings t.validate_prelude
        rule stmt;
      let orphan = validate_else_orphan cursor t.previous rule stmt in
      t.previous := Some stmt;
      match orphan with
      | Some w ->
          add_statement_warning warnings ~recovery:(lost ()) w;
          None
      | None -> Some stmt)
  | Error e ->
      (* [read_statement] refused the rule, so the sheet loses it whole: its
         text reaches no caller reading the parse. *)
      add_statement_warning warnings ~recovery:(lost ()) e;
      None

let read_next t rule =
  let cursor, result =
    read_statement_of_rule ?source:t.source ?meta:t.meta
      ~enforce_spec:t.enforce_spec rule
  in
  finish_read t cursor result rule

(* A rule whose block comes as a stream is read as it is parsed, and so is the
   block; the rule is then whole, and what becomes of it is decided as for any
   other. The placeholder standing for the block, and the at-keyword before it,
   carry a location that only covers their first token, so a reader that gave
   its answer without taking the block may have read one of them: that rule is
   read again with its whole block, as it would have been. *)
let read_streamed t (s : Parser.streamed) =
  let at_kw =
    Component.Preserved
      (Token.v ~kind:(Token.At_keyword s.name) ~loc:s.start_loc)
  in
  let cursor =
    Cursor.of_streamed ?source:t.source ?meta:t.meta
      ~enforce_spec:t.enforce_spec
      ((at_kw :: s.prelude) @ [ Component.Block s.body.placeholder ])
      s.body
  in
  let result =
    match read_statement cursor with
    | stmt -> Ok stmt
    | exception Error.Parse_error e -> Error e
  in
  if s.body.taken then
    let rule = Parser.closed_rule s in
    (finish_read t cursor result rule, rule)
  else
    let rule = Parser.materialize s in
    (read_next t rule, rule)

(* The at-rules whose block holds a list of rules, which a sheet can hold most
   of itself in, as Tailwind's [@layer]s do: theirs is the block worth
   streaming. *)
let streams name =
  match at_rule_keyword name with
  | "layer" | "media" | "supports" | "container" -> true
  | _ -> false

let reading_warnings t = List.rev !(t.warnings)

let read_stylesheet_of_rules ?source ?meta rules =
  let t = reading ?source ?meta () in
  let statements = List.filter_map (read_next t) rules in
  (statements, reading_warnings t)
