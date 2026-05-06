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
  | Media of Media.t
  | Layer of string option
  | Supports of Supports.t
  | Container of string option * Container.t
  | Starting_style
  | When of conditional
  | Else of conditional option
  | Origin of cascade_origin
  | Scope of string option * string option

(* Visibility through at-rule wrappers: a custom property defined outside
   (shorter path) is visible to consumers further inside (longer path). *)
let rec at_path_prefix ~outer ~inner =
  match (outer, inner) with
  | [], _ -> true
  | _, [] -> false
  | a :: outer, b :: inner -> a = b && at_path_prefix ~outer ~inner

let at_wrapper : statement -> (at_node * t * (t -> statement)) option = function
  | Stylesheet.Layer (n, b) ->
      Some ((Layer n : at_node), b, fun b -> Stylesheet.Layer (n, b))
  | Stylesheet.Media (q, b) ->
      Some ((Media q : at_node), b, fun b -> Stylesheet.Media (q, b))
  | Stylesheet.Supports (q, b) ->
      Some ((Supports q : at_node), b, fun b -> Stylesheet.Supports (q, b))
  | Stylesheet.Container (n, q, b) ->
      Some
        ( (Container (n, q) : at_node),
          b,
          fun b -> Stylesheet.Container (n, q, b) )
  | Stylesheet.Starting_style b ->
      Some ((Starting_style : at_node), b, fun b -> Stylesheet.Starting_style b)
  | Stylesheet.When (c, b) ->
      Some ((When c : at_node), b, fun b -> Stylesheet.When (c, b))
  | Stylesheet.Else (c, b) ->
      Some ((Else c : at_node), b, fun b -> Stylesheet.Else (c, b))
  | Stylesheet.Origin (o, b) ->
      Some ((Origin o : at_node), b, fun b -> Stylesheet.Origin (o, b))
  | Stylesheet.Scope (a, b, body) ->
      Some
        ( (Scope (a, b) : at_node),
          body,
          fun body -> Stylesheet.Scope (a, b, body) )
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
        value = Properties.Custom_value { kind; value; _ };
        _;
      } -> (
      match kind with
      | Properties.Value -> (
          try
            let cursor = Cursor.of_components value in
            let parsed = read cursor in
            Cursor.ws cursor;
            Cursor.expect_eof cursor;
            Some parsed
          with Cursor.Parse_error _ -> None)
      | _ -> None)
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
      | Moz_keyframes (n, frames) ->
          Moz_keyframes (n, map_keyframe_decls universal_frame frames)
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

let rec refs_of_components components =
  let strip_ws =
    List.filter (function
      | Component.Preserved { kind = Token.Whitespace; _ } -> false
      | _ -> true)
  in
  let rec split_fallback acc = function
    | [] -> []
    | Component.Preserved { kind = Token.Comma; _ } :: rest ->
        List.rev_append acc rest
    | cv :: rest -> split_fallback (cv :: acc) rest
  in
  let refs_of_var_args args =
    match
      let func : Component.func =
        { name = "var"; arguments = args; terminated = true }
      in
      let cursor =
        Cursor.of_components [ Component.Func { node = func; loc = Loc.dummy } ]
      in
      let var = Values.read_var (fun t -> Cursor.remaining t) cursor in
      Cursor.ws cursor;
      Cursor.expect_eof cursor;
      let fallback_refs =
        match var.Values.fallback with
        | Values.Fallback components | Values.Syntax_fallback components ->
            refs_of_components components
        | Values.Empty | Values.Empty2 | Values.None | Values.Var_fallback _ ->
            []
      in
      Some (("--" ^ var.Values.name) :: fallback_refs)
    with
    | Some refs -> refs
    | None -> []
    | exception _ -> (
        match strip_ws args with
        | Component.Preserved { kind = Token.Ident name; _ } :: rest
          when String.length name >= 2 && String.sub name 0 2 = "--" ->
            name :: refs_of_components (split_fallback [] rest)
        | Component.Preserved { kind = Token.Delim "--"; _ }
          :: Component.Preserved { kind = Token.Ident name; _ }
          :: rest ->
            ("--" ^ name) :: refs_of_components (split_fallback [] rest)
        | Component.Preserved { kind = Token.Delim "-"; _ }
          :: Component.Preserved { kind = Token.Delim "-"; _ }
          :: Component.Preserved { kind = Token.Ident name; _ }
          :: rest ->
            ("--" ^ name) :: refs_of_components (split_fallback [] rest)
        | rest -> refs_of_components rest)
  in
  List.concat_map
    (function
      | Component.Func { node = { name; arguments; _ }; _ }
        when String.lowercase_ascii name = "var" ->
          refs_of_var_args arguments
      | Component.Func { node = { arguments; _ }; _ } ->
          refs_of_components arguments
      | Component.Block { node = { value; _ }; _ } -> refs_of_components value
      | Component.Preserved _ -> [])
    components

let refs_of_component_string value =
  try refs_of_components (Cursor.remaining (Cursor.of_string value))
  with _ -> []

let names_of_vars vars =
  List.map (fun (Variables.V v) -> "--" ^ v.Values.name) vars

let refs_of_media_value : Media.value -> string list = function
  | Ident value -> refs_of_component_string value
  | Function (name, args) ->
      let func : Component.func =
        { name; arguments = args; terminated = true }
      in
      refs_of_components [ Component.Func { node = func; loc = Loc.dummy } ]
  | Length _ | Integer _ | Number _ | Ratio _ | Resolution_value _ -> []

let rec refs_of_media : Media.t -> string list = function
  | Width _ | Height _ | Min_width _ | Max_width _ | Not_min_width _
  | Min_width_rem _ | Not_min_width_rem _ | Min_width_length _
  | Not_min_width_length _ | Aspect_ratio _ | Resolution _ | Color _
  | Color_index _ | Monochrome _ | Color_gamut _ | Video_color_gamut _
  | Dynamic_range _ | Video_dynamic_range _ | Scan _ | Update _
  | Overflow_block _ | Overflow_inline _ | Prefers_reduced_motion _
  | Prefers_reduced_transparency _ | Prefers_reduced_data _ | Prefers_contrast _
  | Prefers_color_scheme _ | Forced_colors _ | Inverted_colors _ | Pointer _
  | Any_pointer _ | Hover _ | Any_hover _ | Scripting _ | Nav_controls _ | Print
  | Orientation _ ->
      []
  | And (a, b) | Or (a, b) -> refs_of_media a @ refs_of_media b
  | Negated query -> refs_of_media query
  | Range (_, _, value) | Range_rev (value, _, _) | Plain (_, value) ->
      refs_of_media_value value
  | Interval (lower, _, _, _, upper) ->
      refs_of_media_value lower @ refs_of_media_value upper
  | Type_query { trailing; _ } ->
      Option.fold ~none:[] ~some:refs_of_media trailing
  | Boolean _ -> []
  | List queries -> List.concat_map refs_of_media queries

let refs_of_supports_feature : Supports.declaration_feature -> string list =
  function
  | Declaration decl -> names_of_vars (Variables.vars_of_declarations [ decl ])
  | Unparseable { value; _ } -> refs_of_components value
  | Empty _ | Vendor_flag_enabled -> []

let rec refs_of_supports : Supports.t -> string list = function
  | Property feature -> refs_of_supports_feature feature
  | Func (_, args) -> refs_of_components args
  | Not condition -> refs_of_supports condition
  | And (a, b) | Or (a, b) -> refs_of_supports a @ refs_of_supports b

let rec refs_of_style_query : Container.style_query -> string list = function
  | Boolean _ -> []
  | Declaration { value; _ } -> refs_of_components value
  | Range { lower; upper; _ } ->
      refs_of_components lower @ refs_of_components upper
  | All (a, b) | Any (a, b) -> refs_of_style_query a @ refs_of_style_query b
  | Neg query -> refs_of_style_query query

let rec refs_of_scroll_state : Container.scroll_state_query -> string list =
  function
  | State _ -> []
  | Both (a, b) | Either (a, b) ->
      refs_of_scroll_state a @ refs_of_scroll_state b
  | Negated query -> refs_of_scroll_state query

let rec refs_of_container : Container.t -> string list = function
  | Min_width_rem _ | Min_width_px _ -> []
  | Named (_, query) | Not query -> refs_of_container query
  | Style { query; _ } -> refs_of_style_query query
  | Scroll_state { query; _ } -> refs_of_scroll_state query
  | And (a, b) | Or (a, b) -> refs_of_container a @ refs_of_container b
  | Feature_query query -> refs_of_media query

let refs_of_declaration decl =
  match decl with
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { kind; value; _ };
        _;
      } -> (
      match kind with Properties.Value -> refs_of_components value | _ -> [])
  | _ -> names_of_vars (Variables.vars_of_declarations [ decl ])

(* Walk the stylesheet to collect, for each declaration, the scope at which it
   appears and the var names its body references. [consumers] lists non-custom
   declarations (which determine direct liveness); [customs] lists custom-prop
   declarations along with the var names their bodies reference (used to
   propagate liveness through chains like [--quad: calc(var(--double) * 2)]). *)
let collect_scoped_refs stylesheet =
  let consumers = ref [] in
  let customs = ref [] in
  let refs_of_at_node = function
    | Media query -> refs_of_media query
    | Supports query -> refs_of_supports query
    | Container (_, query) -> refs_of_container query
    | _ -> []
  in
  let record_decl ~at_path ~selector decl =
    let refs = refs_of_declaration decl in
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
        | Keyframes (_, frames) | Webkit_keyframes (_, frames)
        | Moz_keyframes (_, frames) ->
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
    match ir.media with
    | None -> body
    | Some m -> [ Stylesheet.Media (m, body) ]
  in
  let body =
    match ir.supports with
    | None -> body
    | Some s -> [ Stylesheet.Supports (s, body) ]
  in
  match ir.layer with
  | None -> body
  | Some "" -> [ Stylesheet.Layer (None, body) ]
  | Some n -> [ Stylesheet.Layer (Some n, body) ]

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
