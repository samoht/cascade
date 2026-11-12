open Stylesheet_intf
open Stylesheet_pp
open Stylesheet_read

(* Replay a Parser-recovered rule as a Cursor by flattening its components.
   [Component.at_rule] stores the at-keyword as a bare [name] string; the
   original [At_keyword] token was already consumed during section 5.3, so
   synthesize one at the rule's location so dispatch in [read_statement] still
   sees the opening [\@name]. [?source] flows through so errors raised by
   validators downstream pick up source-context snippets. *)
let cursor_of_rule ?source ?meta : Component.rule -> Cursor.t = function
  | Qualified { node = { prelude; block }; _ } ->
      Cursor.of_components ?source ?meta ~recover:true
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
      Cursor.of_components ?source ?meta ~recover:true
        ((at_cv :: prelude) @ tail_cv)

(* Validate one Parser-recovered rule to a typed statement, or convert the
   validator's [Parse_error] into a rule-level error and drop the rule. Per-
   declaration warnings accumulated on the cursor are drained separately by the
   caller. *)
let read_statement_of_rule ?source ?meta (rule : Component.rule) :
    Cursor.t * (statement, Error.t) result =
  let r = cursor_of_rule ?source ?meta rule in
  let result =
    match read_statement r with
    | stmt -> Ok stmt
    | exception Error.Parse_error e -> Error e
  in
  (r, result)

let rule_loc : Component.rule -> Loc.t = function
  | Qualified { loc; _ } | At { loc; _ } -> loc

let validate_partial_prelude () =
  let charset_seen = ref false in
  let import_seen = ref false in
  let namespace_seen = ref false in
  let body_seen = ref false in
  fun loc stmt ->
    let warning reason =
      Some (Error.bad_value loc ~property:"stylesheet" ~reason)
    in
    match stmt with
    | Charset _ ->
        let error =
          if !charset_seen || !import_seen || !namespace_seen || !body_seen then
            warning "@charset must precede all rules"
          else None
        in
        charset_seen := true;
        error
    | Layer_decl _ ->
        if !import_seen || !namespace_seen then body_seen := true;
        None
    | Import _ ->
        let error =
          if !namespace_seen || !body_seen then
            warning "@import must precede style rules"
          else None
        in
        import_seen := true;
        error
    | Namespace _ ->
        let error =
          if !body_seen then warning "@namespace must precede style rules"
          else None
        in
        namespace_seen := true;
        error
    | _ ->
        body_seen := true;
        None

let validate_partial_statement loc = function
  | Rule { selector; _ } when Selector.matches_nothing selector ->
      Some (Error.bad_selector loc "selector matches nothing")
  | Font_face descriptors when not (font_face_participates descriptors) ->
      Some
        (Error.bad_value loc ~property:"@font-face"
           ~reason:"missing font-family or src descriptor")
  (* CSS Fonts 4 sec. 9.2 makes font-family the one mandatory descriptor; sec.
     9.2.2 defaults a missing base-palette to 0. *)
  | Font_palette_values (_, descriptors)
    when not
           (List.exists
              (function Palette_font_family _ -> true | _ -> false)
              descriptors) ->
      Some
        (Error.bad_value loc ~property:"@font-palette-values"
           ~reason:"missing font-family descriptor")
  | _ -> None

let rec statement_has_invalid_declaration = function
  | Rule { declarations; nested; _ } ->
      List.exists Declaration.is_invalid declarations
      || List.exists statement_has_invalid_declaration nested
  | Declarations decls -> List.exists Declaration.is_invalid decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.exists statement_has_invalid_declaration block
  | Page (_, decls) -> List.exists Declaration.is_invalid decls
  | Page_with_margins (_, descs, margins) ->
      List.exists Declaration.is_invalid descs
      || List.exists
           (fun { descriptors; _ } ->
             List.exists Declaration.is_invalid descriptors)
           margins
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      List.exists Declaration.is_invalid decls
  | _ -> false

let validate_partial_invalid_declarations loc stmt =
  if statement_has_invalid_declaration stmt then
    Some
      (Error.bad_value loc ~property:"stylesheet"
         ~reason:"invalid declaration value")
  else None

let rec declaration_has_strict_warning = function
  | Declaration.Declaration { property = Properties.Color; value; _ } ->
      Values.color_has_specified_hue value
  | Declaration.Theme_guarded { decl; _ } -> declaration_has_strict_warning decl
  | _ -> false

let rec statement_has_strict_warning = function
  | Rule { declarations; nested; _ } ->
      List.exists declaration_has_strict_warning declarations
      || List.exists statement_has_strict_warning nested
  | Declarations decls -> List.exists declaration_has_strict_warning decls
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Layer (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.exists statement_has_strict_warning block
  | Page (_, decls) -> List.exists declaration_has_strict_warning decls
  | Page_with_margins (_, descs, margins) ->
      List.exists declaration_has_strict_warning descs
      || List.exists
           (fun { descriptors; _ } ->
             List.exists declaration_has_strict_warning descriptors)
           margins
  | Position_try (_, decls) | Supports_condition (_, decls) ->
      List.exists declaration_has_strict_warning decls
  | _ -> false

let validate_partial_strict_warnings loc stmt =
  if statement_has_strict_warning stmt then
    Some
      (Error.bad_value loc ~property:"stylesheet"
         ~reason:"strict stylesheet warning")
  else None

(* A warning drained from a cursor is stamped where the cursor recorded it. *)
let add_warning warnings warning = warnings := warning :: !warnings

(* A warning raised beside the statement rather than inside it, so this site
   says what became of the statement. *)
let add_statement_warning warnings ~recovery warning =
  add_warning warnings (Error.with_recovery recovery warning)

let drain_statement_warnings warnings cursor =
  List.iter (add_warning warnings) (Cursor.drain_warnings cursor)

let validate_else_orphan previous rule stmt =
  match stmt with
  | Else _
    when not (Option.fold ~none:false ~some:follows_conditional !previous) ->
      Some
        (Error.bad_value (rule_loc rule) ~property:"stylesheet"
           ~reason:"@else without preceding @when")
  | _ -> None

(* These four run on a statement the reader read whole and the sheet keeps, so
   the material each reports on reaches the output. *)
let validate_partial_statement_warnings warnings validate_prelude rule stmt =
  let loc = rule_loc rule in
  let add =
    Option.iter
      (add_statement_warning warnings ~recovery:Error.Recovery.Recovered)
  in
  add (validate_prelude loc stmt);
  add (validate_partial_statement loc stmt);
  add (validate_partial_invalid_declarations loc stmt);
  add (validate_partial_strict_warnings loc stmt)

let read_stylesheet_of_rules ?source ?meta (rules : Component.rule list) :
    stylesheet * Error.t list =
  let warnings = ref [] in
  let validate_prelude = validate_partial_prelude () in
  (* CSS Conditional Rules 5 section 4: [@else] is a continuation rule and must
     follow [@when] or another [@else]. A bare top-level [@else] is a parse
     error. *)
  let previous : statement option ref = ref Option.None in
  let statements =
    List.filter_map
      (fun rule ->
        let cursor, result = read_statement_of_rule ?source ?meta rule in
        (* [rule_loc] spans the rule the parser handed over, prelude and block
           alike, so it names what a drop costs the sheet. The error's own
           location marks where the read gave up inside it. *)
        let lost () =
          Error.Recovery.(dropped ?source ~loc:(rule_loc rule) Rule)
        in
        (* Drain declaration-level warnings first so source order is preserved:
           decl warnings come from inside the rule, the rule-level error (if
           any) comes after them. *)
        drain_statement_warnings warnings cursor;
        match result with
        | Ok stmt -> (
            validate_partial_statement_warnings warnings validate_prelude rule
              stmt;
            match validate_else_orphan previous rule stmt with
            | Some w ->
                add_statement_warning warnings ~recovery:(lost ()) w;
                previous := Some stmt;
                None
            | None ->
                previous := Some stmt;
                Some stmt)
        | Error e ->
            (* [read_statement] refused the rule, so the sheet loses it whole:
               its text reaches no caller reading the parse. *)
            add_statement_warning warnings ~recovery:(lost ()) e;
            None)
      rules
  in
  (statements, List.rev !warnings)
