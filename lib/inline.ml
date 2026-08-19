(** Closed-world inlining transforms (var() and \@import).

    [vars] is a typed substitution pass over {!Context} and {!Selector}. A
    custom property is visible to itself and to any rule whose effective
    selector descends from its own, within the same chain of
    [\@media]/[\@layer]/ [\@supports] blocks.

    [imports] inlines [\@import] rules from a closed [Context.loader] table.
    Layer/supports/media guards on the prelude are evaluated against [?query]
    and [?layer_order]; rejected imports drop, accepted ones lose the matched
    guard, and a repeat visit (cycle) is dropped. *)

open Stylesheet
open Syntax

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

(* A selector reaches the whole subtree when a comma branch is universal/root:
   [:root]/[html] inherit to every element and [*] matches every element, so
   [:root,:host] covers via [:root] but a lone [:host] (shadow root) does
   not. *)
let universal_selector_text s =
  String.split_on_char ',' s
  |> List.exists (fun p ->
      match String.trim p with ":root" | "html" | "*" -> true | _ -> false)

(* [.theme] is an ancestor of [.theme .descendant] (descendant-prefix), not of
   [.other]; universals always cover. The comparison is conservatively exact
   (not structural), matching the "static prefix" semantics the cram suite
   expects. *)
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
  | Moz_document of moz_document_condition list
  | Container of string option * Container.t option
  | Starting_style
  | When of conditional
  | Else of conditional option
  | Origin of cascade_origin
  | Scope of Selector.t option * Selector.t option

(* A cascade layer never gates custom-property visibility: [--x] defined in one
   [@layer] resolves for a consumer in any other layer (or none), because layers
   only order competing declarations, they do not scope the value. Only the
   conditional wrappers (@media/@supports/@container/...) are real barriers, so
   drop [Layer] nodes from a path before comparing. *)
let rec drop_layers = function
  | [] -> []
  | Layer _ :: rest -> drop_layers rest
  | node :: rest -> node :: drop_layers rest

(* Visibility through at-rule wrappers: a custom property defined outside
   (shorter path) is visible to consumers further inside (longer path). *)
let at_path_prefix ~outer ~inner =
  let rec prefix outer inner =
    match (outer, inner) with
    | [], _ -> true
    | _, [] -> false
    | a :: outer, b :: inner -> a = b && prefix outer inner
  in
  prefix (drop_layers outer) (drop_layers inner)

let at_wrapper : statement -> (at_node * t * (t -> statement)) option = function
  | Stylesheet.Layer (n, b) ->
      Some ((Layer n : at_node), b, fun b -> Stylesheet.Layer (n, b))
  | Stylesheet.Media (q, b) ->
      Some ((Media q : at_node), b, fun b -> Stylesheet.Media (q, b))
  | Stylesheet.Supports (q, b) ->
      Some ((Supports q : at_node), b, fun b -> Stylesheet.Supports (q, b))
  | Stylesheet.Moz_document (q, b) ->
      Some
        ((Moz_document q : at_node), b, fun b -> Stylesheet.Moz_document (q, b))
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

let collect_scopes_record acc at_path selector customs =
  if customs <> [] then acc := { at_path; selector; customs } :: !acc

let collect_scopes_property ~kept ~record ~at_path rule =
  match property_initial_custom_decl ~kept rule with
  | None -> ()
  | Some decl -> record at_path (Selector.Universal None) [ decl ]

let collect_scopes ~kept stylesheet =
  let acc = ref [] in
  let record = collect_scopes_record acc in
  let rec walk_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, _) ->
        List.iter (walk_stmt ~parents ~at_path:(at_path @ [ node ])) body
    | None -> walk_non_at ~parents ~at_path stmt
  and walk_non_at ~parents ~at_path = function
    | Rule rule ->
        let eff = effective_selector ~parents rule.selector in
        record at_path eff (local_customs ~kept rule.declarations);
        List.iter (walk_stmt ~parents:(eff :: parents) ~at_path) rule.nested
    | Property rule -> collect_scopes_property ~kept ~record ~at_path rule
    | Declarations decls ->
        let sel =
          match parents with p :: _ -> p | [] -> Selector.Universal None
        in
        record at_path sel (local_customs ~kept decls)
    | _ -> ()
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

(* [kept] names carry the [--] prefix; [Context.runtime_vars] expects the bare
   custom-property name. *)
let runtime_var_names kept =
  List.map
    (fun name ->
      if String.length name >= 2 && String.sub name 0 2 = "--" then
        String.sub name 2 (String.length name - 2)
      else name)
    kept

let context_for ?(kept = []) visible =
  Context.v ~custom_properties:(List.rev visible)
    ~runtime_vars:(runtime_var_names kept) ()

let read_custom_components read = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } -> (
      try
        let cursor =
          Cursor.of_components
            (Properties.components_of_custom_property_value value)
        in
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

let custom_value_components = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } ->
      Some (Properties.components_of_custom_property_value value)
  | _ -> None

let better_custom_candidate ~important ~idx
    (best : (bool * int * Component.t list) option) =
  match best with
  | None -> true
  | Some (best_important, best_idx, _) ->
      (important && not best_important)
      || (important = best_important && idx > best_idx)

let consider_custom_candidate idx best decl value =
  let important = Declaration.is_important decl in
  let candidate = (important, idx, value) in
  if better_custom_candidate ~important ~idx best then Some candidate else best

let lookup_visible_custom_components visible name =
  let dashed =
    if String.length name >= 2 && String.sub name 0 2 = "--" then name
    else "--" ^ name
  in
  let choose idx best decl =
    match decl with
    | Declaration.Declaration { property = Properties.Custom_property n; _ }
      when n = name || n = dashed -> (
        match custom_value_components decl with
        | None -> best
        | Some value -> consider_custom_candidate idx best decl value)
    | _ -> best
  in
  let rec loop idx best = function
    | [] -> best
    | decl :: rest -> loop (idx + 1) (choose idx best decl) rest
  in
  loop 0 None visible |> Option.map (fun (_, _, value) -> value)

let trim_components components =
  let is_ws = function
    | Component.Preserved { kind = Token.Whitespace; _ } -> true
    | _ -> false
  in
  let rec drop = function hd :: tl when is_ws hd -> drop tl | xs -> xs in
  List.rev (drop (List.rev (drop components)))

type subst_result = Components of Component.t list | Cycle

let rec components_contain preserved components =
  List.exists
    (function
      | Component.Preserved token -> preserved token
      | Component.Func { node = { arguments; _ }; _ } ->
          components_contain preserved arguments
      | Component.Block { node = { value; _ }; _ } ->
          components_contain preserved value)
    components

let parse_var_components args : (string * Component.t list option) option =
  try
    let cursor = Cursor.of_components args in
    let raw_name = Cursor.ident ~keep_case:true cursor in
    if
      not
        (String.length raw_name >= 3
        && raw_name.[0] = '-'
        && raw_name.[1] = '-'
        && raw_name.[2] <> '-')
    then None
    else
      let name = String.sub raw_name 2 (String.length raw_name - 2) in
      let fallback =
        Cursor.ws cursor;
        if Cursor.comma_opt cursor then Some (Cursor.remaining cursor) else None
      in
      Some (name, Option.map trim_components fallback)
  with Cursor.Parse_error _ -> None

let rec substitute_components ~kept visible ~visited components =
  let one = function
    | Component.Func ({ node = { name; arguments; _ }; _ } as fn)
      when String.lowercase_ascii name = "var" -> (
        match parse_var_components arguments with
        | None -> Components [ Component.Func fn ]
        | Some (name, fallback) ->
            substitute_var ~kept visible ~visited fn name fallback)
    | Component.Func fn -> (
        match
          substitute_components ~kept visible ~visited fn.node.arguments
        with
        | Components arguments ->
            Components
              [ Component.Func { fn with node = { fn.node with arguments } } ]
        | Cycle -> Cycle)
    | Component.Block block -> (
        match substitute_components ~kept visible ~visited block.node.value with
        | Components value ->
            Components
              [
                Component.Block { block with node = { block.node with value } };
              ]
        | Cycle -> Cycle)
    | Component.Preserved _ as cv -> Components [ cv ]
  in
  let rec loop acc = function
    | [] -> Components (List.rev acc)
    | cv :: rest -> (
        match one cv with
        | Components cvs -> loop (List.rev_append cvs acc) rest
        | Cycle -> Cycle)
  in
  loop [] components

and substitute_var ~kept visible ~visited original name fallback =
  (* An unresolved var() with no fallback is kept verbatim at the top level, but
     inside another custom property's value it is guaranteed-invalid and
     propagates failure to the nearest enclosing fallback. *)
  let keep_or_fail () =
    if visited = [] then Components [ Component.Func original ] else Cycle
  in
  let fallback_or_original () =
    match fallback with
    | None -> keep_or_fail ()
    | Some components -> substitute_components ~kept visible ~visited components
  in
  let resolved_or_fallback value =
    match
      substitute_components ~kept visible ~visited:(name :: visited) value
    with
    | Components _ as resolved -> resolved
    | Cycle -> (
        match fallback with None -> Cycle | Some _ -> fallback_or_original ())
  in
  (* A var() forming a cycle is invalid at computed-value time; its own fallback
     does not rescue it, so propagate failure to a consumer fallback. *)
  if List.mem name visited then Cycle
  else
    match lookup_visible_custom_components visible name with
    | None ->
        if List.mem (String.concat "" [ "--"; name ]) kept then
          keep_wrapper ~kept visible ~visited original fallback
        else fallback_or_original ()
    | Some value -> resolved_or_fallback value

(* A kept var stays a live [var(--name, ...)], but its fallback may hold
   resolvable vars ([var(--tw-ease, var(--default-ease))] -> [var(--tw-ease,
   ease)]), so substitute inside the fallback and rebuild the wrapper. *)
and keep_wrapper ~kept visible ~visited original fallback =
  match fallback with
  | None -> Components [ Component.Func original ]
  | Some components -> (
      match substitute_components ~kept visible ~visited components with
      | Cycle -> Components [ Component.Func original ]
      | Components subst ->
          let rec name_prefix acc = function
            | (Component.Preserved { kind = Token.Comma; _ } as c) :: _ ->
                List.rev (c :: acc)
            | x :: rest -> name_prefix (x :: acc) rest
            | [] -> List.rev acc
          in
          let prefix = name_prefix [] original.Component.node.arguments in
          Components
            [
              Component.Func
                {
                  original with
                  node =
                    { original.Component.node with arguments = prefix @ subst };
                };
            ])

let declaration_with_components decl components : Declaration.declaration option
    =
  let property = Declaration.property_name decl in
  let value =
    Parser.to_string_custom_minified ~fold_ident:Values.fold_custom_value_ident
      components
  in
  if String.trim value = "" then None
  else
    let important =
      if Declaration.is_important decl then "!important" else ""
    in
    let has_string =
      components_contain (function
        | { kind = Token.String _; _ } -> true
        | _ -> false)
    in
    let has_comma =
      components_contain (function
        | { kind = Token.Comma; _ } -> true
        | _ -> false)
    in
    let opaque () =
      let fallback =
        Declaration.v (Properties.Unknown_property property)
          (Cursor.remaining (Cursor.of_string value))
      in
      Some
        (if Declaration.is_important decl then Declaration.important fallback
         else fallback)
    in
    let source = String.concat "" [ property; ":"; value; important ] in
    match Declaration.read (Cursor.of_string source) with
    | decl -> Some decl
    | exception Cursor.Parse_error _ ->
        if property = "font-family" && has_string components then opaque ()
        else if has_comma components then None
        else opaque ()

let should_use_typed_default ~kept visible vars =
  vars <> []
  && List.for_all
       (fun (Variables.V var) ->
         Option.is_some var.Values.default
         && Option.is_none
              (lookup_visible_custom_components visible var.Values.name)
         (* A kept var must keep its live [var()] reference, so do not collapse
            it to its typed default. *)
         && not (List.mem (String.concat "" [ "--"; var.Values.name ]) kept))
       vars

(* [Context.eval] leaves a kept var's [var()] intact (it is in [runtime_vars])
   while still applying value-independent simplifications like calc identities,
   so the substituted declaration is always evaluated, kept var or not. *)
let apply_substituted_components ctx decl ~original_components components =
  if List.equal Component.equal components original_components then
    Some (Context.eval ctx decl)
  else
    match declaration_with_components decl components with
    | None -> None
    | Some decl -> Some (Context.eval ctx decl)

let substitute_non_custom ~kept visible ctx decl =
  let vars = Variables.vars_of_declarations [ decl ] in
  if should_use_typed_default ~kept visible vars then
    Some (Context.eval ctx decl)
  else
    let value = Declaration.string_of_value ~minify:false decl in
    let original_components = Cursor.remaining (Cursor.of_string value) in
    match
      substitute_components ~kept visible ~visited:[] original_components
    with
    | Cycle -> Some (Context.eval ctx decl)
    | Components components ->
        apply_substituted_components ctx decl ~original_components components

(* A custom property's own value: fold the inlinable (non-kept) variables it
   references in place so they can be deleted, while a [kept] variable it
   references stays a live [var()]. *)
let fold_custom_value ~kept visible decl =
  match custom_value_components decl with
  | None -> Some decl
  | Some original -> (
      match substitute_components ~kept visible ~visited:[] original with
      | Cycle -> Some decl
      | Components components -> (
          if List.equal Component.equal components original then Some decl
          else
            match declaration_with_components decl components with
            | Some decl' -> Some decl'
            | None -> Some decl))

let substitute_declaration ~kept visible ctx decl =
  match custom_name decl with
  | Some _ -> fold_custom_value ~kept visible decl
  | None -> substitute_non_custom ~kept visible ctx decl

let font_src_var_fallback ~simplify ~visited (var : Font_face.src Values.var) =
  match var.Values.fallback with
  | Values.Fallback value -> simplify ~visited value
  | _ -> [ Font_face.Var var ]

let simplify_font_src_descriptor visible entries =
  let normalize_entry = function
    | Font_face.Quoted_url { url; format; tech; _ } ->
        Font_face.Url { url; format; tech }
    | entry -> entry
  in
  let rec simplify ~visited entries =
    List.concat_map (simplify_entry ~visited) entries
  and simplify_entry ~visited = function
    | Font_face.Var var when not (List.mem var.Values.name visited) -> (
        match
          lookup_visible_custom visible var.Values.name Font_face.read_src
        with
        | Some value -> simplify ~visited:(var.Values.name :: visited) value
        | None -> font_src_var_fallback ~simplify ~visited var)
    | entry -> [ normalize_entry entry ]
  in
  simplify ~visited:[] entries

let unicode_range_var_fallback ~simplify ~visited
    (var : Properties.unicode_range Values.var) : Properties.unicode_range =
  match var.Values.fallback with
  | Values.Fallback value -> simplify ~visited value
  | _ -> (Properties.Var var : Properties.unicode_range)

let simplify_unicode_range_descriptor visible (value : Properties.unicode_range)
    =
  let rec simplify ~visited : Properties.unicode_range -> _ = function
    | Properties.Var var when not (List.mem var.Values.name visited) -> (
        match
          lookup_visible_custom visible var.Values.name
            Properties.read_unicode_range
        with
        | Some value -> simplify ~visited:(var.Values.name :: visited) value
        | None -> unicode_range_var_fallback ~simplify ~visited var)
    | value -> value
  in
  simplify ~visited:[] value

let simplify_font_face_descriptor visible = function
  | Src value -> Src (simplify_font_src_descriptor visible value)
  | Unicode_range values ->
      Unicode_range
        (List.map (simplify_unicode_range_descriptor visible) values)
  | descriptor -> descriptor

let eval_page_declaration visible ctx decl =
  let resolve_length_var (var : Values.length Values.var) =
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
        _;
      } ->
      Declaration.v ~important property (resolve_length_var var)
  | _ -> Context.eval ctx decl

let map_keyframe_decls f frames =
  List.map
    (fun frame -> { frame with declarations = List.map f frame.declarations })
    frames

let universal_selector = Selector.Universal None
let selector_for_parents = function [] -> universal_selector | p :: _ -> p

let universal_visible_customs ~scopes ~at_path =
  visible_customs ~scopes ~at_path ~selector:universal_selector

let eval_decls ~kept ~scopes ~at_path selector decls =
  let visible = visible_customs ~scopes ~at_path ~selector in
  let ctx = context_for ~kept visible in
  List.filter_map (substitute_declaration ~kept visible ctx) decls

(* @page, @keyframes, and @position-try declarations apply to elements whose
   effective selector is universal at this at-path. Customs declared on [:root],
   [html], or [*] cover any element and remain reachable. *)
let eval_universal_decls ~scopes ~at_path decls =
  let visible = universal_visible_customs ~scopes ~at_path in
  List.map (Context.eval (context_for visible)) decls

let universal_eval ~scopes ~at_path =
  let visible = universal_visible_customs ~scopes ~at_path in
  Context.eval (context_for visible)

let substitute_font_face ~scopes ~at_path descriptors =
  let visible = universal_visible_customs ~scopes ~at_path in
  List.map (simplify_font_face_descriptor visible) descriptors

let substitute_page_with_margins ~scopes ~at_path sel descriptors margins =
  let visible = universal_visible_customs ~scopes ~at_path in
  let eval_page = eval_page_declaration visible (context_for visible) in
  let update_margin (m : page_margin_rule) =
    { m with descriptors = List.map eval_page m.descriptors }
  in
  Page_with_margins
    (sel, List.map eval_page descriptors, List.map update_margin margins)

let rec substitute ~kept ~scopes ~parents ~at_path stmts =
  List.map (substitute_stmt ~kept ~scopes ~parents ~at_path) stmts

and substitute_stmt ~kept ~scopes ~parents ~at_path stmt =
  match at_wrapper stmt with
  | Some (node, body, rebuild) ->
      rebuild
        (substitute ~kept ~scopes ~parents ~at_path:(at_path @ [ node ]) body)
  | None -> (
      match stmt with
      | Rule rule ->
          let eff = effective_selector ~parents rule.selector in
          Rule
            {
              rule with
              declarations =
                eval_decls ~kept ~scopes ~at_path eff rule.declarations;
              nested =
                substitute ~kept ~scopes ~parents:(eff :: parents) ~at_path
                  rule.nested;
            }
      | Declarations decls ->
          Declarations
            (eval_decls ~kept ~scopes ~at_path
               (selector_for_parents parents)
               decls)
      | Page (sel, decls) ->
          let visible = universal_visible_customs ~scopes ~at_path in
          let ctx = context_for visible in
          Page (sel, List.map (eval_page_declaration visible ctx) decls)
      | Position_try (n, decls) ->
          Position_try (n, eval_universal_decls ~scopes ~at_path decls)
      | Keyframes (n, frames) ->
          Keyframes
            (n, map_keyframe_decls (universal_eval ~scopes ~at_path) frames)
      | Webkit_keyframes (n, frames) ->
          Webkit_keyframes
            (n, map_keyframe_decls (universal_eval ~scopes ~at_path) frames)
      | Moz_keyframes (n, frames) ->
          Moz_keyframes
            (n, map_keyframe_decls (universal_eval ~scopes ~at_path) frames)
      | Font_face descriptors ->
          Font_face (substitute_font_face ~scopes ~at_path descriptors)
      | Page_with_margins (sel, descriptors, margins) ->
          substitute_page_with_margins ~scopes ~at_path sel descriptors margins
      | other -> other)

(** {1 Pass 3 - dead-code elimination} *)

let strip_component_ws =
  List.filter (function
    | Component.Preserved { kind = Token.Whitespace; _ } -> false
    | _ -> true)

let rec split_var_fallback acc = function
  | [] -> []
  | Component.Preserved { kind = Token.Comma; _ } :: rest ->
      List.rev_append acc rest
  | cv :: rest -> split_var_fallback (cv :: acc) rest

let rec refs_of_components components =
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

and refs_of_var_args args =
  try
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
      | Values.Empty | Values.Empty2 | Values.None | Values.Var_fallback _ -> []
    in
    ("--" ^ var.Values.name) :: fallback_refs
  with Cursor.Parse_error _ -> (
    match strip_component_ws args with
    | Component.Preserved { kind = Token.Ident name; _ } :: rest
      when String.length name >= 2 && String.sub name 0 2 = "--" ->
        name :: refs_of_components (split_var_fallback [] rest)
    | Component.Preserved { kind = Token.Delim "--"; _ }
      :: Component.Preserved { kind = Token.Ident name; _ }
      :: rest ->
        ("--" ^ name) :: refs_of_components (split_var_fallback [] rest)
    | Component.Preserved { kind = Token.Delim "-"; _ }
      :: Component.Preserved { kind = Token.Delim "-"; _ }
      :: Component.Preserved { kind = Token.Ident name; _ }
      :: rest ->
        ("--" ^ name) :: refs_of_components (split_var_fallback [] rest)
    | rest -> refs_of_components rest)

let refs_of_component_string value =
  try refs_of_components (Cursor.remaining (Cursor.of_string value))
  with Cursor.Parse_error _ -> []

let names_of_vars vars =
  List.map (fun (Variables.V v) -> "--" ^ v.Values.name) vars

let refs_of_media_value : Media.value -> string list = function
  | Ident value -> refs_of_component_string (Media.string_of_ident value)
  | Length value ->
      refs_of_component_string
        (Pp.to_string (Values.pp_length ~always:true) value)
  | Function (_, args) -> refs_of_component_string args
  | Integer _ | Number _ | Ratio _ | Resolution_value _ -> []

let refs_of_media_feature : Media.feature -> string list = function
  | Boolean _ -> []
  | Plain (_, value) | Range (_, _, value) | Range_rev (value, _, _) ->
      refs_of_media_value value
  | Interval (lower, _, _, _, upper) ->
      refs_of_media_value lower @ refs_of_media_value upper
  | General_enclosed _ -> []

let rec refs_of_media_condition : Media.condition -> string list = function
  | Feature f -> refs_of_media_feature f
  | Not c -> refs_of_media_condition c
  | And (a, b) | Or (a, b) ->
      refs_of_media_condition a @ refs_of_media_condition b

let rec refs_of_media : Media.t -> string list = function
  | Cond c -> refs_of_media_condition c
  | Type { trailing; _ } ->
      Option.fold ~none:[] ~some:refs_of_media_condition trailing
  | List queries -> List.concat_map refs_of_media queries

let refs_of_supports_feature : Supports.declaration_feature -> string list =
  function
  | Declaration decl -> names_of_vars (Variables.vars_of_declarations [ decl ])
  | Empty _ | Unsupported _ | Vendor_flag_enabled -> []

let rec refs_of_supports : Supports.t -> string list = function
  | Property feature -> refs_of_supports_feature feature
  | Function _ -> []
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
        value = Properties.Custom_value { value; _ };
        _;
      } ->
      refs_of_components (Properties.components_of_custom_property_value value)
  | _ -> names_of_vars (Variables.vars_of_declarations [ decl ])

(* Collect, per declaration, its scope and the var names its body references.
   [consumers] are non-custom declarations (direct liveness); [customs] are
   custom-prop declarations with their referenced vars, propagating liveness
   through chains like [--quad: calc(var(--double) * 2)]. *)
let refs_of_at_node = function
  | Media query -> refs_of_media query
  | Supports query -> refs_of_supports query
  | Container (_, Some query) -> refs_of_container query
  | Container (_, None) -> []
  | _ -> []

let selector_for_parents_universal = function
  | p :: _ -> p
  | [] -> Selector.Universal None

let record_at_node_refs consumers ~parents ~at_path node =
  match refs_of_at_node node with
  | [] -> ()
  | refs ->
      let sel = selector_for_parents_universal parents in
      consumers := (at_path, sel, refs) :: !consumers

let record_keyframe_decls ~record_decl ~at_path ~sel frames =
  List.iter
    (fun fr -> List.iter (record_decl ~at_path ~selector:sel) fr.declarations)
    frames

let collect_scoped_refs stylesheet =
  let consumers = ref [] in
  let customs = ref [] in
  let record_decl ~at_path ~selector decl =
    let refs = refs_of_declaration decl in
    match custom_name decl with
    | Some name -> customs := (at_path, selector, name, refs) :: !customs
    | None -> consumers := (at_path, selector, refs) :: !consumers
  in
  let rec walk_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, _) ->
        record_at_node_refs consumers ~parents ~at_path node;
        List.iter (walk_stmt ~parents ~at_path:(at_path @ [ node ])) body
    | None -> walk_non_at ~parents ~at_path stmt
  and walk_non_at ~parents ~at_path = function
    | Rule rule ->
        let eff = effective_selector ~parents rule.selector in
        List.iter (record_decl ~at_path ~selector:eff) rule.declarations;
        List.iter (walk_stmt ~parents:(eff :: parents) ~at_path) rule.nested
    | Declarations decls ->
        let sel = selector_for_parents_universal parents in
        List.iter (record_decl ~at_path ~selector:sel) decls
    | Page (_, decls) | Position_try (_, decls) ->
        let sel = Selector.Universal None in
        List.iter (record_decl ~at_path ~selector:sel) decls
    | Keyframes (_, frames)
    | Webkit_keyframes (_, frames)
    | Moz_keyframes (_, frames) ->
        let sel = Selector.Universal None in
        record_keyframe_decls ~record_decl ~at_path ~sel frames
    | _ -> ()
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
let visible_refs ~path_visible consumers path =
  List.concat_map
    (fun (cp, _cs, refs) ->
      if path_visible ~scope_path:path ~consumer_path:cp then refs else [])
    consumers

let marked_live live (path, sel, name) =
  List.exists (fun (p, s, n) -> p = path && s = sel && n = name) !live

let mark_live live entry =
  if not (marked_live live entry) then live := entry :: !live

let live_at_scope live ~path ~sel =
  List.exists (fun (p, s, _) -> p = path && s = sel) !live

let mark_referenced_custom ~path_visible ~live ~consumer_path ref_name
    (b_path, b_sel, b_name, _) =
  if
    b_name = ref_name
    && path_visible ~scope_path:b_path ~consumer_path
    && not (marked_live live (b_path, b_sel, b_name))
  then mark_live live (b_path, b_sel, b_name)

let live_customs ~consumers ~customs =
  let path_visible ~scope_path ~consumer_path =
    at_path_prefix ~outer:scope_path ~inner:consumer_path
  in
  let live = ref [] in
  (* Seed: every custom whose scope has a path-compatible consumer referencing
     its name is directly live. *)
  List.iter
    (fun (path, sel, name, _) ->
      if List.mem name (visible_refs ~path_visible consumers path) then
        mark_live live (path, sel, name))
    customs;
  (* Propagate: if a live custom [A] references a name [N], any custom [B] named
     [N] at a path that contains [A]'s path is also live. Iterate to
     fixpoint. *)
  let propagate_from (a_path, a_sel, _, a_refs) =
    if live_at_scope live ~path:a_path ~sel:a_sel then
      List.iter
        (fun ref_name ->
          List.iter
            (mark_referenced_custom ~path_visible ~live ~consumer_path:a_path
               ref_name)
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

let same_live_custom (a_path, a_sel, a_name) (b_path, b_sel, b_name, _) =
  a_path = b_path && a_sel = b_sel && a_name = b_name

let custom_ref_visible ~path_visible ~path ref_name (next_path, _, next_name, _)
    =
  next_name = ref_name && path_visible ~scope_path:next_path ~consumer_path:path

let rec reaches_live_custom ~customs ~path_visible target seen
    (path, sel, name, refs) =
  let key = (path, sel, name) in
  if List.mem key seen then false
  else
    let reaches_ref ref_name =
      List.exists
        (fun next ->
          custom_ref_visible ~path_visible ~path ref_name next
          && ((seen <> [] && same_live_custom target next)
             || reaches_live_custom ~customs ~path_visible target (key :: seen)
                  next))
        customs
    in
    List.exists reaches_ref refs

let cyclic_live_customs ~consumers ~customs =
  let initially_live = live_customs ~consumers ~customs in
  let path_visible ~scope_path ~consumer_path =
    at_path_prefix ~outer:scope_path ~inner:consumer_path
  in
  List.filter
    (fun entry ->
      List.exists
        (fun custom ->
          same_live_custom entry custom
          && reaches_live_custom ~customs ~path_visible entry [] custom)
        customs)
    initially_live

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
      with Cursor.Parse_error _ -> decl)

let custom_is_live ~keep ~live_set ~at_path ~selector name =
  List.mem name keep
  || List.exists
       (fun (p, s, n) -> p = at_path && s = selector && n = name)
       live_set

let filter_live_custom_decls ~keep ~live_set ~at_path ~selector =
  List.filter_map (fun d ->
      match custom_name d with
      | None -> Some d
      | Some name ->
          if custom_is_live ~keep ~live_set ~at_path ~selector name then
            Some (normalize_custom_value d)
          else None)

let strip_dead_rule ~filter_decls ~map_stmts ~parents ~at_path
    (rule : Stylesheet.rule) : Stylesheet.statement option =
  let eff = effective_selector ~parents rule.selector in
  let nested = map_stmts ~parents:(eff :: parents) ~at_path rule.nested in
  let decls = filter_decls ~at_path ~selector:eff rule.declarations in
  if decls = [] && nested = [] then None
  else Some (Rule { rule with declarations = decls; nested })

let strip_dead_declarations ~filter_decls ~parents ~at_path decls :
    Stylesheet.statement option =
  let sel = selector_for_parents parents in
  match filter_decls ~at_path ~selector:sel decls with
  | [] -> None
  | decls -> Some (Declarations decls)

let strip_dead ~keep ~live_set stmts =
  let filter_decls ~at_path ~selector =
    filter_live_custom_decls ~keep ~live_set ~at_path ~selector
  in
  let rec map_stmts ~parents ~at_path stmts =
    List.filter_map (map_stmt ~parents ~at_path) stmts
  and map_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, rebuild) -> (
        match map_stmts ~parents ~at_path:(at_path @ [ node ]) body with
        | [] -> None
        | b -> Some (rebuild b))
    | None -> map_non_at ~parents ~at_path stmt
  and map_non_at ~parents ~at_path stmt =
    match stmt with
    | Rule rule ->
        strip_dead_rule ~filter_decls ~map_stmts ~parents ~at_path rule
    | Property _ ->
        (* A [@property] registration is a global, author-declared binding, not
           dead code in the cascade sense. The closed-world inline drops the
           ones for fully-substituted vars via [statements_for_inline]; the
           dead-declaration pass must not drop them here, or a partial inline
           (resolve_theme) would lose unrelated registrations. *)
        Some stmt
    | Declarations decls ->
        strip_dead_declarations ~filter_decls ~parents ~at_path decls
    | Page (sel, decls) ->
        Some
          (Page (sel, filter_decls ~at_path ~selector:universal_selector decls))
    | other -> Some other
  in
  map_stmts ~parents:[] ~at_path:[] stmts

let normalise_var_name name =
  if String.length name >= 2 && String.sub name 0 2 = "--" then name
  else String.concat "" [ "--"; name ]

(* Per custom-property name, how many definitions it has across the whole
   stylesheet, plus the set of names referenced anywhere. A variable is safe to
   inline and delete only when it has a single definition: its value is then
   unambiguous. A variable redefined in another scope is a real cascade override
   (dark mode, a media query, a component) and must stay a live [var()] so its
   consumers still track it - inlining it would freeze the value. *)
let var_census stylesheet =
  (* Count the distinct cascade scopes (selector + at-rule context) that define
     each variable, so a redeclaration within one scope counts once while a real
     override in another scope counts as two. Plus the set of referenced
     names. *)
  let scopes_of : (string, (at_node list * string) list) Hashtbl.t =
    Hashtbl.create 64
  in
  let add name key =
    let prev = Option.value ~default:[] (Hashtbl.find_opt scopes_of name) in
    if not (List.mem key prev) then Hashtbl.replace scopes_of name (key :: prev)
  in
  List.iter
    (fun (s : scope) ->
      let key = (s.at_path, Selector.to_string ~minify:true s.selector) in
      s.customs
      |> List.filter_map (fun d ->
          Option.map normalise_var_name (custom_name d))
      |> List.sort_uniq compare
      |> List.iter (fun n -> add n key))
    (collect_scopes ~kept:[] stylesheet);
  let counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
  Hashtbl.iter
    (fun name keys -> Hashtbl.replace counts name (List.length keys))
    scopes_of;
  let referenced = ref [] in
  let add_refs decls =
    referenced :=
      names_of_vars (Variables.vars_of_declarations decls) @ !referenced
  in
  let rec walk stmt =
    match at_wrapper stmt with
    | Some (_, body, _) -> List.iter walk body
    | None -> (
        match stmt with
        | Rule r ->
            add_refs r.declarations;
            List.iter walk r.nested
        | Declarations decls -> add_refs decls
        | _ -> ())
  in
  List.iter walk stylesheet;
  (counts, List.sort_uniq compare !referenced)

(** {1 Layer-decided custom-property folding} *)

(* A variable redefined across cascade layers on the same element is a real
   override, but - unlike an @media or dark-mode override - its winner is
   statically decidable, so it can be folded to that winner instead of kept
   live. [statements_for_inline] later drops the [@layer] wrappers, which would
   otherwise leave the losing definitions competing by document order and pick
   the wrong value. Resolving the winner here keeps the folded value correct. *)

(* Document-order cascade layer names (dotted for nesting), low precedence
   first: the order in which layers are first introduced. *)
let stylesheet_layer_order stylesheet =
  let seen = Hashtbl.create 16 in
  let order = ref [] in
  let add name =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      order := name :: !order
    end
  in
  let dotted prefix name =
    if prefix = "" then name else String.concat "." [ prefix; name ]
  in
  let rec walk prefix = function
    | [] -> ()
    | stmt :: rest ->
        (match stmt with
        | Stylesheet.Layer_decl names ->
            List.iter (fun n -> add (dotted prefix n)) names
        | Stylesheet.Layer (Some n, body) ->
            let full = dotted prefix n in
            add full;
            walk full body
        | Stylesheet.Layer (None, body) -> walk prefix body
        | _ -> ());
        walk prefix rest
  in
  walk "" stylesheet;
  List.rev !order

(* [Some layer] for a purely-layered path ([layer = None] unlayered, [Some name]
   the dotted layer), [None] when the path is conditional or holds an anonymous
   layer - those are not statically foldable. *)
let foldable_layer_of_at_path at_path : string option option =
  let rec go names : at_node list -> string option option = function
    | [] -> (
        match List.rev names with
        | [] -> Some None
        | ns -> Some (Some (String.concat "." ns)))
    | Layer (Some n) :: rest -> go (n :: names) rest
    | Layer None :: _ -> None
    | _ :: _ -> None
  in
  go [] at_path

(* A copy of [d] tagged with [layer] and its own importance, for the cascade
   resolver. *)
let annotate_layer (layer : string option) d =
  let name = Declaration.property_name d in
  let value = Declaration.string_of_value ~minify:true d in
  let base =
    match layer with
    | None -> Declaration.custom_property name value
    | Some l -> Declaration.custom_property ~layer:l name value
  in
  if Declaration.is_important d then Declaration.important base else base

(* Names whose every definition is unconditional, on one selector, and spread
   over two or more layers, mapped to the resolved cascade winner ([`Unset] when
   it resolves to no value). A single scope with repeated declarations is left
   alone: the census already treats it as one inlinable definition. *)
let layer_decided_customs ~keep stylesheet =
  let scopes = collect_scopes ~kept:keep stylesheet in
  let by_name = Hashtbl.create 64 in
  List.iter
    (fun (s : scope) ->
      let sel = Selector.to_string ~minify:true s.selector in
      let fl = foldable_layer_of_at_path s.at_path in
      List.iter
        (fun d ->
          match custom_name d with
          | None -> ()
          | Some name ->
              let prev =
                Option.value ~default:[] (Hashtbl.find_opt by_name name)
              in
              Hashtbl.replace by_name name ((sel, fl, d) :: prev))
        s.customs)
    scopes;
  let layer_order = stylesheet_layer_order stylesheet in
  let fold = Hashtbl.create 16 in
  Hashtbl.iter
    (fun name entries ->
      let entries = List.rev entries in
      let unconditional =
        List.for_all (fun (_, fl, _) -> Option.is_some fl) entries
      in
      let one_selector =
        match entries with
        | (sel0, _, _) :: rest -> List.for_all (fun (s, _, _) -> s = sel0) rest
        | [] -> true
      in
      let distinct_layers =
        entries
        |> List.filter_map (fun (_, fl, _) -> fl)
        |> List.sort_uniq compare |> List.length
      in
      if unconditional && one_selector && distinct_layers >= 2 then
        let annotated =
          List.filter_map
            (fun (_, fl, d) -> Option.map (fun l -> annotate_layer l d) fl)
            entries
        in
        match Context.winning_custom_declaration ~layer_order annotated with
        | Some w ->
            Hashtbl.replace fold name
              (`Value (Declaration.string_of_value ~minify:true w))
        | None -> Hashtbl.replace fold name `Unset)
    by_name;
  fold

(* Replace every layer-decided definition with a single unlayered definition of
   the winner (or drop it entirely when unset), so the census sees one inlinable
   definition. *)
let collapse_layer_decided ~keep stylesheet =
  let fold = layer_decided_customs ~keep stylesheet in
  if Hashtbl.length fold = 0 then stylesheet
  else
    let emitted = Hashtbl.create 16 in
    let filter_decls decls =
      List.filter_map
        (fun d ->
          match custom_name d with
          | Some n when Hashtbl.mem fold n -> (
              match Hashtbl.find fold n with
              | `Unset -> None
              | `Value _ when Hashtbl.mem emitted n -> None
              | `Value v ->
                  Hashtbl.add emitted n ();
                  Some (Declaration.custom_property n v))
          | _ -> Some d)
        decls
    in
    let rec map_stmt stmt =
      match at_wrapper stmt with
      | Some (_, body, rebuild) -> rebuild (List.map map_stmt body)
      | None -> (
          match stmt with
          | Rule r ->
              Rule
                {
                  r with
                  declarations = filter_decls r.declarations;
                  nested = List.map map_stmt r.nested;
                }
          | Declarations decls -> Declarations (filter_decls decls)
          | other -> other)
    in
    List.map map_stmt stylesheet

let vars ?(keep_vars = []) ?(warn = fun _ -> ()) stylesheet =
  let keep = List.map normalise_var_name keep_vars in
  let stylesheet = collapse_layer_decided ~keep stylesheet in
  let counts, referenced = var_census stylesheet in
  let inlinable name =
    (not (List.mem name keep)) && Hashtbl.find_opt counts name = Some 1
  in
  (* Everything not inlinable stays live: the keep-set, plus any non-kept
     variable redefined across scopes. Those are folded by the substitution as
     if kept, so the single-def variables fold into them and get stripped. *)
  let kept =
    Hashtbl.fold
      (fun name _ acc -> if inlinable name then acc else name :: acc)
      counts keep
    |> List.sort_uniq compare
  in
  (* Warn for a non-kept variable kept only because it is redefined in a
     different scope, so the caller knows it escaped inlining. *)
  Hashtbl.iter
    (fun name count ->
      if count > 1 && (not (List.mem name keep)) && List.mem name referenced
      then warn name)
    counts;
  let scopes = collect_scopes ~kept stylesheet in
  let original_consumers, original_customs = collect_scoped_refs stylesheet in
  let cyclic_live_set =
    cyclic_live_customs ~consumers:original_consumers ~customs:original_customs
  in
  let substituted =
    substitute ~kept ~scopes ~parents:[] ~at_path:[] stylesheet
  in
  let consumers, customs = collect_scoped_refs substituted in
  let live_set = live_customs ~consumers ~customs @ cyclic_live_set in
  (* Keep every definition of a kept variable (including a cross-scope override
     like an @media one), so the live chain stays complete; single-def
     inlinables are not in [kept] and are stripped once folded. *)
  strip_dead ~keep:kept ~live_set substituted

(** {1 [@import] inlining helpers} *)

let decode_import_url s =
  let trimmed = String.trim s in
  if trimmed = "" then trimmed
  else
    try
      let r = Cursor.of_string trimmed in
      Cursor.one_of [ Cursor.url; Cursor.string ] r
    with Cursor.Parse_error _ -> trimmed

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
let layer_guard_passes ~(layer_order : string list) (rule : import_rule) =
  match ((rule.layer : string option), layer_order) with
  | None, _ | _, [] -> true
  | Some name, order -> List.mem name order

let supports_guard_passes ~(query : Context.query option) (rule : import_rule) =
  match ((rule.supports : Supports.t option), query) with
  | None, _ | Some _, None -> true
  | Some cond, Some q -> Context.matches_supports q cond

let media_guard_passes ~(query : Context.query option) (rule : import_rule) =
  match ((rule.media : Media.t option), query) with
  | None, _ | Some _, None -> true
  | Some m, Some q -> Context.matches_media q m

(* When [query] is provided, the matching at-rule wrapper is no longer needed:
   we have already evaluated the guard and decided to load. Strip the matched
   wrapper from the rule so [wrap_import_body] doesn't re-emit it. *)
let strip_evaluated_guards ~(query : Context.query option) (rule : import_rule)
    : import_rule =
  match query with
  | None -> rule
  | Some _ -> { rule with media = Option.None; supports = Option.None }

let parse_import_content content =
  let cursor = Cursor.of_string content in
  match read_stylesheet cursor with
  | stylesheet -> Some stylesheet
  | exception Cursor.Parse_error _ -> (
      match
        let inner, _warnings = parse_stylesheet_partial content in
        inner
      with
      | inner -> Some inner
      | exception Invalid_argument _ -> None)

let strip_import_charset =
  List.filter (function Charset _ -> false | _ -> true)

let imports ?query ?(layer_order = []) (loader : Context.loader) stylesheet =
  let imports = loader.imports in
  let resolve ~base url : string option =
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
    | Import import_rule -> replace_import ~base ~stack import_rule
    | stmt -> replace_non_import ~base ~stack stmt
  and replace_import ~base ~stack import_rule =
    let url = decode_import_url import_rule.url in
    match resolve ~base url with
    | None -> [ Import import_rule ]
    | Some resolved -> replace_resolved_import ~stack import_rule resolved
  and replace_resolved_import ~stack import_rule resolved =
    if List.mem resolved stack then []
    else
      let content = List.assoc resolved imports in
      match parse_import_content content with
      | None -> [ Import import_rule ]
      | Some inner -> inline_parsed_import ~stack import_rule resolved inner
  and inline_parsed_import ~stack import_rule resolved inner =
    let inner = strip_import_charset inner in
    let processed =
      replace_stmts ~base:(Some resolved) ~stack:(resolved :: stack) inner
    in
    wrap_import_body (strip_evaluated_guards ~query import_rule) processed
  and replace_non_import ~base ~stack stmt =
    match at_wrapper stmt with
    | Some (_, body, rebuild) -> [ rebuild (replace_stmts ~base ~stack body) ]
    | None -> replace_plain_stmt ~base ~stack stmt
  and replace_plain_stmt ~base ~stack = function
    | Rule rule ->
        [ Rule { rule with nested = replace_stmts ~base ~stack rule.nested } ]
    | other -> [ other ]
  in
  let initial_stack =
    match loader.base_url with Some b -> [ b ] | None -> []
  in
  replace_stmts ~base:loader.base_url ~stack:initial_stack stylesheet
