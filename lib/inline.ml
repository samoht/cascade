(** Closed-world inlining transforms (var() and \@import).

    The [vars] entry point implements a typed substitution pass layered on top
    of {!Context} (typed evaluator) and {!Selector}. Visibility of a custom
    property is computed from the rule structure: a custom property defined on a
    rule applies to itself and to any rule whose effective selector descends
    from it, and only if the consumer is inside the same chain of [\@media],
    [\@layer], [\@supports] blocks.

    The [imports] entry point inlines [\@import] rules from a closed
    [Context.loader] table. Layer / supports / media guards on the [\@import]
    prelude are evaluated against [?query] and [?layer_order] when supplied;
    rejected imports are dropped, accepted ones lose the matched guard from
    their wrapping. Cyclic imports terminate by dropping the repeat visit. *)

open Stylesheet

(** {1 Selector cover} *)

let contains_nesting sel =
  Selector.any (function Selector.Nesting -> true | _ -> false) sel

let substitute_nesting ~parent sel =
  Selector.map (function Selector.Nesting -> parent | s -> s) sel

let combine_with_parent (parent : Selector.t) (child : Selector.t) : Selector.t
    =
  if contains_nesting child then substitute_nesting ~parent child
  else Selector.Combined (parent, Selector.Descendant, child)

(* Effective selector after substituting [&] / nesting against any parent
   selectors on the recursion stack. *)
let effective_selector ~parents sel =
  match parents with
  | [] -> sel
  | _ ->
      List.fold_left
        (fun child parent -> combine_with_parent parent child)
        sel parents

let universal_selector_text s = s = ":root" || s = "html" || s = "*"

(* [.theme] is an ancestor of [.theme .descendant] (descendant-prefix); not of
   [.other]. Universal selectors always cover. The text comparison is
   conservative - exact rather than structural - which mirrors the "static
   prefix" semantics expected by the cram suite. *)
let selector_covers ~ancestor ~consumer =
  let a = Selector.to_string ~minify:true ancestor in
  let c = Selector.to_string ~minify:true consumer in
  if universal_selector_text a then true
  else if a = c then true
  else
    let prefix = a ^ " " in
    String.length c >= String.length prefix
    && String.sub c 0 (String.length prefix) = prefix

(** {1 At-rule path} *)

type at_node =
  | A_media of Media.t
  | A_layer of string option
  | A_supports of Supports.t
  | A_container of string option * Container.t
  | A_starting_style
  | A_when of conditional
  | A_else of conditional option
  | A_origin of cascade_origin
  | A_scope of string option * string option

(* Visibility through at-rule wrappers: a custom property defined outside
   (shorter path) is visible to consumers further inside (longer path). *)
let rec at_path_prefix ~outer ~inner =
  match (outer, inner) with
  | [], _ -> true
  | _, [] -> false
  | a :: outer, b :: inner -> a = b && at_path_prefix ~outer ~inner

let at_wrapper : statement -> (at_node * t * (t -> statement)) option = function
  | Layer (n, b) -> Some (A_layer n, b, fun b -> Layer (n, b))
  | Media (q, b) -> Some (A_media q, b, fun b -> Media (q, b))
  | Supports (q, b) -> Some (A_supports q, b, fun b -> Supports (q, b))
  | Container (n, q, b) ->
      Some (A_container (n, q), b, fun b -> Container (n, q, b))
  | Starting_style b -> Some (A_starting_style, b, fun b -> Starting_style b)
  | When (c, b) -> Some (A_when c, b, fun b -> When (c, b))
  | Else (c, b) -> Some (A_else c, b, fun b -> Else (c, b))
  | Origin (o, b) -> Some (A_origin o, b, fun b -> Origin (o, b))
  | Scope (a, b, body) ->
      Some (A_scope (a, b), body, fun body -> Scope (a, b, body))
  | _ -> None

(** {1 Scope record} *)

type scope = {
  at_path : at_node list;
  selector : Selector.t;
  customs : Declaration.declaration list;
}

let custom_name = Variables.custom_declaration_name

let local_customs ~kept decls =
  List.filter
    (fun d ->
      match custom_name d with None -> false | Some n -> not (List.mem n kept))
    decls

let property_initial_custom_decl : type a.
    kept:string list -> a property_rule -> Declaration.declaration option =
 fun ~kept rule ->
  if List.mem rule.name kept then None
  else
    Option.map
      (fun value ->
        Declaration.custom_property rule.name
          (Pp.to_string ~minify:true (Variables.pp_value rule.syntax) value))
      rule.initial_value

(** {1 Pass 1 - collect every rule's scope} *)

let collect_scopes ~kept stylesheet =
  let acc = ref [] in
  let record at_path selector customs =
    if customs <> [] then acc := { at_path; selector; customs } :: !acc
  in
  let rec walk_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, _) ->
        List.iter (walk_stmt ~parents ~at_path:(at_path @ [ node ])) body
    | None -> (
        match stmt with
        | Rule rule ->
            let eff = effective_selector ~parents rule.selector in
            record at_path eff (local_customs ~kept rule.declarations);
            List.iter (walk_stmt ~parents:(eff :: parents) ~at_path) rule.nested
        | Property rule -> (
            match property_initial_custom_decl ~kept rule with
            | None -> ()
            | Some decl -> record at_path (Selector.Universal None) [ decl ])
        | Declarations decls ->
            let sel =
              match parents with p :: _ -> p | [] -> Selector.Universal None
            in
            record at_path sel (local_customs ~kept decls)
        | _ -> ())
  in
  List.iter (walk_stmt ~parents:[] ~at_path:[]) stylesheet;
  List.rev !acc

(** {1 Pass 2 - substitute var() in every declaration} *)

let visible_customs ~scopes ~at_path ~selector =
  List.concat_map
    (fun s ->
      if
        at_path_prefix ~outer:s.at_path ~inner:at_path
        && selector_covers ~ancestor:s.selector ~consumer:selector
      then s.customs
      else [])
    scopes

let context_for visible = Context.v ~custom_properties:(List.rev visible) ()

let read_custom_components read = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { kind = Properties.Value; value; _ };
        _;
      } -> (
      try
        let cursor = Cursor.of_components value in
        let parsed = read cursor in
        Cursor.ws cursor;
        Cursor.expect_eof cursor;
        Some parsed
      with Cursor.Parse_error _ -> None)
  | _ -> None

let lookup_visible_custom visible name read =
  let dashed =
    if String.length name >= 2 && String.sub name 0 2 = "--" then name
    else "--" ^ name
  in
  List.find_map
    (function
      | Declaration.Declaration
          { property = Properties.Custom_property n; value = _; _ } as decl
        when n = name || n = dashed ->
          read_custom_components read decl
      | _ -> None)
    visible

let simplify_font_src_descriptor visible entries =
  let normalize_entry = function
    | Font_face.Quoted_url { url; format; tech; _ } ->
        Font_face.Url { url; format; tech }
    | entry -> entry
  in
  let rec simplify ~visited entries =
    List.concat_map
      (function
        | Font_face.Var var when not (List.mem var.Values.name visited) -> (
            match
              lookup_visible_custom visible var.Values.name Font_face.read_src
            with
            | Some value -> simplify ~visited:(var.Values.name :: visited) value
            | None -> (
                match var.Values.fallback with
                | Values.Fallback value -> simplify ~visited value
                | _ -> [ Font_face.Var var ]))
        | entry -> [ normalize_entry entry ])
      entries
  in
  simplify ~visited:[] entries

let simplify_unicode_range_descriptor visible (value : Properties.unicode_range)
    =
  let rec simplify ~visited = function
    | (Properties.Var var : Properties.unicode_range)
      when not (List.mem var.Values.name visited) -> (
        match
          lookup_visible_custom visible var.Values.name
            Properties.read_unicode_range
        with
        | Some value -> simplify ~visited:(var.Values.name :: visited) value
        | None -> (
            match var.Values.fallback with
            | Values.Fallback value -> simplify ~visited value
            | _ -> (Properties.Var var : Properties.unicode_range)))
    | value -> value
  in
  simplify ~visited:[] value

let simplify_font_face_descriptor visible = function
  | Src value -> Src (simplify_font_src_descriptor visible value)
  | Unicode_range value ->
      Unicode_range (simplify_unicode_range_descriptor visible value)
  | descriptor -> descriptor

let eval_page_declaration visible ctx decl =
  let resolve_length_var var =
    match lookup_visible_custom visible var.Values.name Values.read_length with
    | Some value -> value
    | None -> (
        match var.Values.fallback with
        | Values.Fallback value -> value
        | _ -> Values.Var var)
  in
  match decl with
  | Declaration.Declaration
      {
        property = Properties.Margin_top as property;
        value = (Values.Var var : Values.length);
        important;
      } ->
      Declaration.Declaration
        { property; value = resolve_length_var var; important }
  | _ -> Context.eval ctx decl

let map_keyframe_decls f frames =
  List.map
    (fun frame ->
      {
        frame with
        keyframe_declarations = List.map f frame.keyframe_declarations;
      })
    frames

let rec substitute ~scopes ~parents ~at_path stmts =
  List.map (substitute_stmt ~scopes ~parents ~at_path) stmts

and substitute_stmt ~scopes ~parents ~at_path stmt =
  match at_wrapper stmt with
  | Some (node, body, rebuild) ->
      rebuild (substitute ~scopes ~parents ~at_path:(at_path @ [ node ]) body)
  | None -> (
      let eval_with sel decls =
        let visible = visible_customs ~scopes ~at_path ~selector:sel in
        let ctx = context_for visible in
        List.map (Context.eval ctx) decls
      in
      (* @page, @keyframes, and @position-try declarations apply to elements
         whose effective selector is universal at this at-path, so use the
         universal selector for visibility. Customs declared on [:root] / [html]
         / [*] cover any element and remain reachable. *)
      let universal_decls decls =
        let visible =
          visible_customs ~scopes ~at_path ~selector:(Selector.Universal None)
        in
        let ctx = context_for visible in
        List.map (Context.eval ctx) decls
      in
      let universal_frame =
        Context.eval
          (context_for
             (visible_customs ~scopes ~at_path
                ~selector:(Selector.Universal None)))
      in
      match stmt with
      | Rule rule ->
          let eff = effective_selector ~parents rule.selector in
          Rule
            {
              rule with
              declarations = eval_with eff rule.declarations;
              nested =
                substitute ~scopes ~parents:(eff :: parents) ~at_path
                  rule.nested;
            }
      | Declarations decls ->
          let sel =
            match parents with p :: _ -> p | [] -> Selector.Universal None
          in
          Declarations (eval_with sel decls)
      | Page (sel, decls) ->
          let visible =
            visible_customs ~scopes ~at_path ~selector:(Selector.Universal None)
          in
          let ctx = context_for visible in
          Page (sel, List.map (eval_page_declaration visible ctx) decls)
      | Position_try (n, decls) -> Position_try (n, universal_decls decls)
      | Keyframes (n, frames) ->
          Keyframes (n, map_keyframe_decls universal_frame frames)
      | Webkit_keyframes (n, frames) ->
          Webkit_keyframes (n, map_keyframe_decls universal_frame frames)
      | Font_face descriptors ->
          let visible =
            visible_customs ~scopes ~at_path ~selector:(Selector.Universal None)
          in
          Font_face
            (List.map (simplify_font_face_descriptor visible) descriptors)
      | Page_with_margins (sel, descriptors, margins) ->
          let visible =
            visible_customs ~scopes ~at_path ~selector:(Selector.Universal None)
          in
          let ctx = context_for visible in
          let eval_descriptor (d : raw_descriptor) : raw_descriptor =
            (* Round-trip the descriptor through a synthetic typed declaration
               so [Context.eval] resolves any [var()] in its value. CSS at-rule
               descriptors aren't true property declarations but their values
               share the property-value grammar. *)
            let synthetic_token kind =
              Component.Preserved (Token.synthetic kind)
            in
            let components =
              synthetic_token (Token.Ident d.descriptor_name)
              :: synthetic_token Token.Colon
              :: d.descriptor_value
            in
            match
              Declaration.read_declaration (Cursor.of_components components)
            with
            | None -> d
            | Some decl ->
                let evaluated = eval_page_declaration visible ctx decl in
                {
                  descriptor_name = Declaration.property_name evaluated;
                  descriptor_value =
                    Cursor.remaining
                      (Cursor.of_string
                         (Declaration.string_of_value ~minify:true evaluated));
                }
          in
          let new_descriptors = List.map eval_descriptor descriptors in
          let new_margins =
            List.map
              (fun (m : page_margin_rule) ->
                {
                  m with
                  margin_descriptors =
                    List.map eval_descriptor m.margin_descriptors;
                })
              margins
          in
          Page_with_margins (sel, new_descriptors, new_margins)
      | other -> other)

(** {1 Pass 3 - dead-code elimination} *)

let scan_referenced_names s =
  let acc = ref [] in
  let len = String.length s in
  let in_string = ref None in
  let rec scan i =
    if i >= len then ()
    else
      match !in_string with
      | Some q ->
          if s.[i] = q && (i = 0 || s.[i - 1] <> '\\') then in_string := None;
          scan (i + 1)
      | None ->
          if s.[i] = '"' || s.[i] = '\'' then begin
            in_string := Some s.[i];
            scan (i + 1)
          end
          else if i + 4 <= len && String.sub s i 4 = "var(" then begin
            let j = ref (i + 4) in
            while !j < len && (s.[!j] = ' ' || s.[!j] = '\t') do
              incr j
            done;
            let start = !j in
            while
              !j < len
              &&
              let c = s.[!j] in
              c = '-'
              || (c >= 'a' && c <= 'z')
              || (c >= 'A' && c <= 'Z')
              || (c >= '0' && c <= '9')
              || c = '_'
            do
              incr j
            done;
            if !j > start then begin
              let name = String.sub s start (!j - start) in
              if String.length name >= 2 && String.sub name 0 2 = "--" then
                acc := name :: !acc
            end;
            scan (i + 4)
          end
          else scan (i + 1)
  in
  scan 0;
  !acc

(* Walk the stylesheet to collect, for each declaration, the scope at which it
   appears and the var names its body references. [consumers] lists non-custom
   declarations (which determine direct liveness); [customs] lists custom-prop
   declarations along with the var names their bodies reference (used to
   propagate liveness through chains like [--quad: calc(var(--double) * 2)]). *)
let collect_scoped_refs stylesheet =
  let consumers = ref [] in
  let customs = ref [] in
  let refs_of_at_node = function
    | A_container (_, query) ->
        scan_referenced_names (Container.to_string query)
    | _ -> []
  in
  let refs_of decl =
    scan_referenced_names (Declaration.string_of_value ~minify:true decl)
  in
  let record_decl ~at_path ~selector decl =
    let refs = refs_of decl in
    match custom_name decl with
    | Some name -> customs := (at_path, selector, name, refs) :: !customs
    | None -> consumers := (at_path, selector, refs) :: !consumers
  in
  let rec walk_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, _) ->
        (match refs_of_at_node node with
        | [] -> ()
        | refs ->
            let sel =
              match parents with p :: _ -> p | [] -> Selector.Universal None
            in
            consumers := (at_path, sel, refs) :: !consumers);
        List.iter (walk_stmt ~parents ~at_path:(at_path @ [ node ])) body
    | None -> (
        match stmt with
        | Rule rule ->
            let eff = effective_selector ~parents rule.selector in
            List.iter (record_decl ~at_path ~selector:eff) rule.declarations;
            List.iter (walk_stmt ~parents:(eff :: parents) ~at_path) rule.nested
        | Declarations decls ->
            let sel =
              match parents with p :: _ -> p | [] -> Selector.Universal None
            in
            List.iter (record_decl ~at_path ~selector:sel) decls
        | Page (_, decls) | Position_try (_, decls) ->
            let sel = Selector.Universal None in
            List.iter (record_decl ~at_path ~selector:sel) decls
        | Keyframes (_, frames) | Webkit_keyframes (_, frames) ->
            let sel = Selector.Universal None in
            List.iter
              (fun fr ->
                List.iter
                  (record_decl ~at_path ~selector:sel)
                  fr.keyframe_declarations)
              frames
        | _ -> ())
  in
  List.iter (walk_stmt ~parents:[] ~at_path:[]) stylesheet;
  (!consumers, !customs)

(* Closure: a custom-prop declaration is live iff some consumer at a compatible
   at-rule path (any [@media]/[@layer]/[@supports] chain that contains the
   custom's chain) references its name, transitively through other live customs.
   The selector cover that {!substitute} uses is intentionally not applied here:
   at runtime a consumer that does not statically descend from the custom's
   selector can still inherit the custom (e.g. [.other] sitting inside a
   [.theme] element), so we keep the custom-prop declaration in source. The
   at-rule path is a hard barrier though - a [@media]-wrapped declaration cannot
   affect anything outside that block. *)
let live_customs ~consumers ~customs =
  let path_visible ~scope_path ~consumer_path =
    at_path_prefix ~outer:scope_path ~inner:consumer_path
  in
  let directly_visible (path, _sel) =
    List.concat_map
      (fun (cp, _cs, refs) ->
        if path_visible ~scope_path:path ~consumer_path:cp then refs else [])
      consumers
  in
  let live = ref [] in
  let is_live (path, sel, name) =
    List.exists (fun (p, s, n) -> p = path && s = sel && n = name) !live
  in
  let mark entry = if not (is_live entry) then live := entry :: !live in
  (* Seed: every custom whose scope has a path-compatible consumer referencing
     its name is directly live. *)
  List.iter
    (fun (path, sel, name, _) ->
      if List.mem name (directly_visible (path, sel)) then mark (path, sel, name))
    customs;
  (* Propagate: if a live custom [A] references a name [N], any custom [B] named
     [N] at a path that contains [A]'s path is also live. Iterate to
     fixpoint. *)
  let any_at_scope ~path ~sel =
    List.exists (fun (p, s, _) -> p = path && s = sel) !live
  in
  let propagate_from (a_path, a_sel, _, a_refs) =
    if any_at_scope ~path:a_path ~sel:a_sel then
      List.iter
        (fun ref_name ->
          List.iter
            (fun (b_path, b_sel, b_name, _) ->
              if
                b_name = ref_name
                && path_visible ~scope_path:b_path ~consumer_path:a_path
                && not (is_live (b_path, b_sel, b_name))
              then mark (b_path, b_sel, b_name))
            customs)
        a_refs
  in
  let changed = ref true in
  while !changed do
    let before = List.length !live in
    List.iter propagate_from customs;
    changed := List.length !live > before
  done;
  !live

let strip_dead ~keep ~live_set stmts =
  let normalize_custom_value decl =
    match custom_name decl with
    | None -> decl
    | Some name -> (
        let value = Declaration.string_of_value ~minify:true decl in
        let important = Declaration.is_important decl in
        try
          let cursor = Cursor.of_string value in
          let color = Values.read_color cursor in
          Cursor.ws cursor;
          Cursor.expect_eof cursor;
          let decl =
            Declaration.custom_property name
              (Pp.to_string ~minify:true Values.pp_color color)
          in
          if important then Declaration.important decl else decl
        with _ -> decl)
  in
  let is_live ~at_path ~selector ~name =
    List.mem name keep
    || List.exists
         (fun (p, s, n) -> p = at_path && s = selector && n = name)
         live_set
  in
  let filter_decls ~at_path ~selector =
    List.filter_map (fun d ->
        match custom_name d with
        | None -> Some d
        | Some name ->
            if is_live ~at_path ~selector ~name then
              Some (normalize_custom_value d)
            else None)
  in
  let rec map_stmts ~parents ~at_path stmts =
    List.filter_map (map_stmt ~parents ~at_path) stmts
  and map_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, rebuild) -> (
        match map_stmts ~parents ~at_path:(at_path @ [ node ]) body with
        | [] -> None
        | b -> Some (rebuild b))
    | None -> (
        match stmt with
        | Rule rule ->
            let eff = effective_selector ~parents rule.selector in
            let nested =
              map_stmts ~parents:(eff :: parents) ~at_path rule.nested
            in
            let decls = filter_decls ~at_path ~selector:eff rule.declarations in
            if decls = [] && nested = [] then None
            else Some (Rule { rule with declarations = decls; nested })
        | Property rule ->
            if
              List.mem rule.name keep
              || List.exists
                   (fun (p, s, n) ->
                     p = at_path && s = Selector.Universal None && n = rule.name)
                   live_set
            then Some stmt
            else None
        | Declarations decls -> (
            let sel =
              match parents with p :: _ -> p | [] -> Selector.Universal None
            in
            match filter_decls ~at_path ~selector:sel decls with
            | [] -> None
            | decls -> Some (Declarations decls))
        | Page (sel, decls) ->
            Some
              (Page
                 ( sel,
                   filter_decls ~at_path ~selector:(Selector.Universal None)
                     decls ))
        | other -> Some other)
  in
  map_stmts ~parents:[] ~at_path:[] stmts

let normalise_var_name name =
  if String.length name >= 2 && String.sub name 0 2 = "--" then name
  else String.concat "" [ "--"; name ]

let vars ?(keep_vars = []) stylesheet =
  let keep = List.map normalise_var_name keep_vars in
  let scopes = collect_scopes ~kept:keep stylesheet in
  let substituted = substitute ~scopes ~parents:[] ~at_path:[] stylesheet in
  let consumers, customs = collect_scoped_refs substituted in
  let live_set = live_customs ~consumers ~customs in
  strip_dead ~keep ~live_set substituted

(** {1 [@import] inlining helpers} *)

let decode_import_url s =
  let trimmed = String.trim s in
  if trimmed = "" then trimmed
  else
    try
      let r = Cursor.of_string trimmed in
      Cursor.one_of [ Cursor.url; Cursor.string ] r
    with Cursor.Parse_error _ -> trimmed

let strip_url_suffix url =
  let cut_at c s =
    match String.index_opt s c with Some i -> String.sub s 0 i | None -> s
  in
  url |> cut_at '?' |> cut_at '#'

let wrap_import_body (ir : import_rule) body =
  let body =
    match ir.media with None -> body | Some m -> [ Media (m, body) ]
  in
  let body =
    match ir.supports with None -> body | Some s -> [ Supports (s, body) ]
  in
  match ir.layer with
  | None -> body
  | Some "" -> [ Layer (None, body) ]
  | Some n -> [ Layer (Some n, body) ]

(* Layer/supports/media guard checks. When [layer_order] is empty, every layer
   name is treated as known (the caller hasn't declared an order, so we defer to
   the runtime cascade). When [query] is [None], supports/media guards pass
   through and survive as wrapping at-rules in the inlined body; when a query is
   supplied the guard is evaluated and the import is dropped on rejection. *)
let layer_guard_passes ~layer_order (rule : import_rule) =
  match (rule.layer, layer_order) with
  | None, _ | _, [] -> true
  | Some name, order -> List.mem name order

let supports_guard_passes ~query (rule : import_rule) =
  match (rule.supports, query) with
  | None, _ | Some _, None -> true
  | Some cond, Some q -> Context.matches_supports q cond

let media_guard_passes ~query (rule : import_rule) =
  match (rule.media, query) with
  | None, _ | Some _, None -> true
  | Some m, Some q -> Context.matches_media q m

(* When [query] is provided, the matching at-rule wrapper is no longer needed:
   we have already evaluated the guard and decided to load. Strip the matched
   wrapper from the rule so [wrap_import_body] doesn't re-emit it. *)
let strip_evaluated_guards ~query (rule : import_rule) =
  match query with
  | None -> rule
  | Some _ -> { rule with media = None; supports = None }

let imports ?query ?(layer_order = []) (loader : Context.loader) stylesheet =
  let imports = loader.imports in
  let resolve ~base url =
    let l = Context.loader ?base_url:base ~imports () in
    let url = strip_url_suffix url in
    match Context.resolve_url l url with
    | Error _ -> None
    | Ok resolved ->
        if List.mem_assoc resolved imports then Some resolved else None
  in
  let guards_pass rule =
    layer_guard_passes ~layer_order rule
    && supports_guard_passes ~query rule
    && media_guard_passes ~query rule
  in
  (* Track URLs currently on the recursion stack to break cyclic imports (a -> b
     -> a drops the second visit). *)
  let rec replace_stmts ~base ~stack stmts =
    List.concat_map (replace ~base ~stack) stmts
  and replace ~base ~stack = function
    | Import import_rule when not (guards_pass import_rule) -> []
    | Import import_rule -> (
        let url = decode_import_url import_rule.url in
        match resolve ~base url with
        | None -> [ Import import_rule ]
        | Some resolved -> (
            if List.mem resolved stack then []
            else
              let content = List.assoc resolved imports in
              let parsed =
                let cursor = Cursor.of_string content in
                try Some (read_stylesheet cursor)
                with Cursor.Parse_error _ -> (
                  try
                    let inner, _warnings = parse_stylesheet_partial content in
                    Some inner
                  with Invalid_argument _ -> None)
              in
              match parsed with
              | None -> [ Import import_rule ]
              | Some inner ->
                  let inner =
                    List.filter
                      (function Charset _ -> false | _ -> true)
                      inner
                  in
                  let processed =
                    replace_stmts ~base:(Some resolved)
                      ~stack:(resolved :: stack) inner
                  in
                  wrap_import_body
                    (strip_evaluated_guards ~query import_rule)
                    processed))
    | stmt -> (
        match at_wrapper stmt with
        | Some (_, body, rebuild) ->
            [ rebuild (replace_stmts ~base ~stack body) ]
        | None -> (
            match stmt with
            | Rule rule ->
                [
                  Rule
                    {
                      rule with
                      nested = replace_stmts ~base ~stack rule.nested;
                    };
                ]
            | other -> [ other ]))
  in
  let initial_stack =
    match loader.base_url with Some b -> [ b ] | None -> []
  in
  replace_stmts ~base:loader.base_url ~stack:initial_stack stylesheet
