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

(* One at-rule wrapper a statement sits inside. A path is the whole chain, held
   innermost-first so that descending conses onto the parent's chain rather than
   copying it. *)
type at_node =
  | Media of Media.t
  | Layer of Stylesheet.layer_name option
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

let rec drop_deeper n (path : at_node list) =
  if n <= 0 then path
  else match path with [] -> [] | _ :: rest -> drop_deeper (n - 1) rest

(* Nodes compare structurally, as the prefix form compared them. [Media.equal]
   and its neighbours fold the spec's spelling equivalences, which would make
   [(min-width:10px)] and [(width>=10px)] one barrier rather than two. *)
let rec same_path (a : at_node list) (b : at_node list) =
  match (a, b) with
  | [], [] -> true
  | x :: a, y :: b -> x = y && same_path a b
  | _ -> false

(* Visibility through at-rule wrappers: a custom property defined outside
   (shorter path) is visible to consumers further inside (longer path). A path
   is held innermost-first, so "outside" is the tail: [outer] reaches [inner]
   when it is a suffix of it. *)
let at_path_prefix ~outer ~inner =
  let outer = drop_layers outer and inner = drop_layers inner in
  let deeper = List.length inner - List.length outer in
  deeper >= 0 && same_path outer (drop_deeper deeper inner)

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
  (* Listed rather than closed with a wildcard: a statement that grows a block
     later has to be classified here before it compiles, so it cannot fall
     through the path-tracking walks as a leaf. [Rule] is a block too, but its
     selector is a scope boundary rather than an at-rule wrapper, so the callers
     descend into it themselves. *)
  | Stylesheet.Rule _ | Stylesheet.Property _ | Stylesheet.Declarations _
  | Stylesheet.Bang_comment _ | Stylesheet.Charset _ | Stylesheet.Import _
  | Stylesheet.Namespace _ | Stylesheet.Layer_decl _
  | Stylesheet.Supports_condition _ | Stylesheet.Keyframes _
  | Stylesheet.Webkit_keyframes _ | Stylesheet.Moz_keyframes _
  | Stylesheet.Font_face _ | Stylesheet.Counter_style _ | Stylesheet.Page _
  | Stylesheet.Page_with_margins _ | Stylesheet.Font_palette_values _
  | Stylesheet.Font_feature_values _ | Stylesheet.View_transition _
  | Stylesheet.Position_try _ | Stylesheet.Viewport _
  | Stylesheet.Unknown_at_rule _ ->
      None

(** {1 Scope record} *)

type scope = {
  at_path : at_node list;
  selector : Selector.t;
  customs : Declaration.declaration list;
}

(* The scopes plus the names the sheet defines anywhere. A consumer sees only
   the scopes that cover it, and a name it cannot see is not a name the sheet is
   without: the two answers part ways in [substitute_var], where only the second
   lets the fallback stand in. *)
type scope_set = { all : scope list; declared : (string, unit) Hashtbl.t }

let custom_name = Variables.custom_declaration_name

(* [Declaration.custom_property] refuses a value it cannot write back as part of
   the declaration it belongs to. A parsed stylesheet can hold such a value -
   one CSS Syntax 3 would drop - so a rewrite that reaches one keeps what it
   started from instead of raising. *)
let rebuild_custom ?layer name value =
  try Some (Declaration.custom_property ?layer name value)
  with Failure _ -> None

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
    Option.bind rule.initial_value (fun value ->
        rebuild_custom rule.name
          (Pp.to_string ~minify:true (Variables.pp_value rule.syntax) value))

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
        List.iter (walk_stmt ~parents ~at_path:(node :: at_path)) body
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
  let all = List.rev !acc in
  let declared = Hashtbl.create 64 in
  List.iter
    (fun s ->
      List.iter
        (fun d ->
          match custom_name d with
          | Some name ->
              Hashtbl.replace declared (Custom_property_name.add_prefix name) ()
          | None -> ())
        s.customs)
    all;
  { all; declared }

(** {1 Pass 2 - substitute var() in every declaration} *)

(* The customs a consumer can see, with an index from name to the declarations
   that answer to it. Both lookups below used to scan the whole list, and the
   walk asks once per declaration, so the pass cost a square in the declaration
   count. The index is built once per consumer scope instead. *)
type visible = {
  decls : Declaration.declaration list;
  by_name : (string, (int * Declaration.declaration) list) Hashtbl.t;
  declared : (string, unit) Hashtbl.t;
}

let visible_of_decls ~declared decls =
  let by_name = Hashtbl.create 64 in
  List.iteri
    (fun idx decl ->
      match decl with
      | Declaration.Declaration { property = Properties.Custom_property n; _ }
        ->
          let prev = Option.value ~default:[] (Hashtbl.find_opt by_name n) in
          Hashtbl.replace by_name n ((idx, decl) :: prev)
      | _ -> ())
    decls;
  (* Consed while walking, so put each bucket back in source order once rather
     than appending to its end per entry. *)
  Hashtbl.iter
    (fun name entries -> Hashtbl.replace by_name name (List.rev entries))
    (Hashtbl.copy by_name);
  { decls; by_name; declared }

(* A name reaches its declarations under the spelling it was written with, so
   ask for both: [--x] is stored as written and [x] is the bare form callers
   hand in. *)
let named visible name =
  let dashed = Custom_property_name.add_prefix name in
  match Hashtbl.find_opt visible.by_name name with
  | Some entries when name = dashed -> entries
  | Some entries -> (
      match Hashtbl.find_opt visible.by_name dashed with
      | None -> entries
      | Some more ->
          List.merge (fun (a, _) (b, _) -> Int.compare a b) entries more)
  | None -> Option.value ~default:[] (Hashtbl.find_opt visible.by_name dashed)

let visible_customs ~(scopes : scope_set) ~at_path ~selector =
  visible_of_decls ~declared:scopes.declared
    (List.concat_map
       (fun s ->
         if
           at_path_prefix ~outer:s.at_path ~inner:at_path
           && selector_covers ~ancestor:s.selector ~consumer:selector
         then s.customs
         else [])
       scopes.all)

(* [kept] names carry the [--] prefix; [Context.runtime_vars] expects the bare
   custom-property name. *)
let runtime_var_names kept = List.map Custom_property_name.strip_prefix kept

let context_for ?(kept = []) visible =
  Context.v ~custom_properties:(List.rev visible.decls)
    ~runtime_vars:(runtime_var_names kept) ()

let read_custom_components ?(unicode_ranges = false) read = function
  | Declaration.Declaration
      {
        property = Properties.Custom_property _;
        value = Properties.Custom_value { value; _ };
        _;
      } -> (
      try
        let components = Properties.components_of_custom_property_value value in
        let cursor =
          (* CSS Syntax 3 (ED) sec. 5.5.11: a custom property holds the tokens
             it was written with, lexed without unicode ranges. A descriptor
             that asks for them reads the substituted text afresh. *)
          if unicode_ranges then
            Cursor.of_string ~unicode_ranges:true
              (Parser.to_string_minified components)
          else Cursor.of_components components
        in
        let parsed = read cursor in
        Cursor.ws cursor;
        Cursor.expect_eof cursor;
        Some parsed
      with Cursor.Parse_error _ -> None)
  | _ -> None

let lookup_visible_custom ?unicode_ranges visible name read =
  List.find_map
    (fun (_, decl) -> read_custom_components ?unicode_ranges read decl)
    (named visible name)

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
  List.fold_left
    (fun best (idx, decl) ->
      match custom_value_components decl with
      | None -> best
      | Some value -> consider_custom_candidate idx best decl value)
    None (named visible name)
  |> Option.map (fun (_, _, value) -> value)

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
    if not (Custom_property_name.is_valid raw_name) then None
    else
      let name = Custom_property_name.strip_prefix raw_name in
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
        (* CSS Variables 1 sec. 3 puts the fallback in only where the custom
           property holds its guaranteed-invalid initial value. A definition
           this consumer cannot see is not an absent one: the element it styles
           may sit under the element the definition is written for, so the
           reference stays live. *)
        let dashed = Custom_property_name.add_prefix name in
        if List.mem dashed kept || Hashtbl.mem visible.declared dashed then
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

(* The property is carried through as the tag [decl] already holds, never as its
   rendered name: [Declaration.property_name] minifies, and [page-break-*]
   minifies to the CSS Fragmentation 3 sec. 3.4 [break-*] alias of a different
   property. *)
let declaration_with_components decl components : Declaration.declaration option
    =
  let value =
    Parser.to_string_custom_minified ~fold_ident:Values.fold_custom_value_ident
      components
  in
  if String.trim value = "" then None
  else
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
    (* [font-family] reaches here typed or, when its value never parsed as one,
       as the unknown property of that name; both are the same property. *)
    let is_font_family =
      match Declaration.property_key decl with
      | Declaration.Key Properties.Font_family -> true
      | Declaration.Key (Properties.Unknown_property name) ->
          String.equal name "font-family"
      | _ -> false
    in
    let opaque () = Some (Declaration.with_opaque_value decl value) in
    match Declaration.with_value decl value with
    | decl -> Some decl
    | exception Cursor.Parse_error _ ->
        if is_font_family && has_string components then opaque ()
        else if has_comma components then None
        else opaque ()

let should_use_typed_default ~kept visible vars =
  vars <> []
  && List.for_all
       (fun (Variables.V var) ->
         let dashed = Custom_property_name.add_prefix var.Values.name in
         Option.is_some var.Values.default
         && Option.is_none
              (lookup_visible_custom_components visible var.Values.name)
         (* A kept var must keep its live [var()] reference, so do not collapse
            it to its typed default, and neither may a name the sheet defines
            out of this consumer's sight: the fallback stands in only for a
            custom property that holds its guaranteed-invalid initial value. *)
         && (not (List.mem dashed kept))
         && not (Hashtbl.mem visible.declared dashed))
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

(* A name the sheet defines out of this consumer's sight is live here for the
   same reason a kept one is: the evaluator resolves a name it does not hold to
   the reference's fallback, and CSS Variables 1 sec. 3 puts the fallback in
   only where the custom property holds its guaranteed-invalid initial value.
   Only the names this declaration references have to be named, so the list
   stays as short as the declaration is. *)
let out_of_sight visible vars =
  List.filter_map
    (fun (Variables.V var) ->
      let name = var.Values.name in
      let dashed = Custom_property_name.add_prefix name in
      if
        Hashtbl.mem visible.declared dashed
        && Option.is_none (lookup_visible_custom_components visible name)
      then Some dashed
      else None)
    vars

let substitute_non_custom ~kept visible ctx decl =
  let vars = Variables.vars_of_declarations [ decl ] in
  let ctx =
    match out_of_sight visible vars with
    | [] -> ctx
    | live -> context_for ~kept:(kept @ live) visible
  in
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

(* Every descriptor whose value type carries a [Var] arm resolves the same way:
   follow the reference to the custom it names, then the references that value
   holds in turn, and stop on a name already seen so a cycle terminates. What
   differs per descriptor is only how to read the value, how to see a [Var], and
   what a value that is no [Var] still holds, so take those and share the walk.
   [leaf] receives the walk so a value nesting its own type carries the same
   [visited] set inwards; without that a reference reachable both directly and
   through the nesting would not terminate. *)
let simplify_nested_var ?unicode_ranges visible ~read
    ~(as_var : 'a -> 'a Values.var option) ~(of_var : 'a Values.var -> 'a)
    ~(leaf :
       (visited:string list -> 'a -> 'a) -> visited:string list -> 'a -> 'a)
    (value : 'a) : 'a =
  let rec simplify ~visited (value : 'a) : 'a =
    match as_var value with
    | Some var when not (List.mem var.Values.name visited) -> (
        match
          lookup_visible_custom ?unicode_ranges visible var.Values.name read
        with
        | Some found -> simplify ~visited:(var.Values.name :: visited) found
        | None -> (
            (* Nothing defines the name: CSS Variables 1 sec. 3 falls back to
               the second argument, and with none the reference stands. *)
            match (var.Values.fallback : 'a Values.fallback) with
            | Values.Fallback fallback -> simplify ~visited fallback
            | Values.Empty | Values.Empty2 | Values.None
            | Values.Syntax_fallback _ | Values.Var_fallback _ ->
                of_var var))
    | Some _ | None -> leaf simplify ~visited value
  in
  simplify ~visited:[] value

(* The common case: the value is flat, so a non-[Var] holds no reference. *)
let simplify_typed_var ?unicode_ranges visible ~read ~as_var ~of_var value =
  simplify_nested_var ?unicode_ranges visible ~read ~as_var ~of_var
    ~leaf:(fun _simplify ~visited:_ value -> value)
    value

let simplify_unicode_range_descriptor visible =
  simplify_typed_var ~unicode_ranges:true visible
    ~read:Properties.read_unicode_range
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.unicode_range))

(* Read a referenced custom value with the property parser so a property-style
   stack remains visible as [List]. [substitute_font_face] validates the result
   against the descriptor's single-name grammar below. Using the narrower reader
   here would conflate an invalid referenced value with a missing custom
   property and retain the unresolved [var()] instead. *)
let simplify_font_family_descriptor visible =
  simplify_nested_var visible ~read:Properties.read_font_family
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_family))
    ~leaf:(fun simplify ~visited value ->
      match (value : Properties.font_family) with
      | Properties.List items ->
          (Properties.List (List.map (simplify ~visited) items)
            : Properties.font_family)
      | value -> value)

let simplify_font_style_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_style
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_style))

let simplify_font_weight_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_weight
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_weight))

let simplify_font_stretch_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_stretch
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_stretch))

let simplify_font_display_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_display
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_display))

let simplify_font_feature_settings_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_feature_settings
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_feature_settings))

let simplify_font_variation_settings_descriptor visible =
  simplify_typed_var visible ~read:Properties.read_font_variation_settings
    ~as_var:(function Properties.Var v -> Some v | _ -> None)
    ~of_var:(fun v -> (Properties.Var v : Properties.font_variation_settings))

let simplify_font_variant_descriptor visible =
  simplify_typed_var visible ~read:read_font_variant_descriptor
    ~as_var:(function Var v -> Some v | _ -> Option.None)
    ~of_var:(fun v -> (Var v : font_variant_descriptor))

(* One resolver covers [ascent-override], [descent-override] and
   [line-gap-override]: CSS Fonts 4 sec. 4.10 gives the three the same
   grammar. *)
let simplify_metric_override_descriptor visible =
  simplify_typed_var visible ~read:Font_face.read_metric_override
    ~as_var:(function Font_face.Var v -> Some v | _ -> Option.None)
    ~of_var:(fun v -> (Font_face.Var v : Font_face.metric_override))

let simplify_font_tech_descriptor visible =
  simplify_typed_var visible ~read:read_font_tech_descriptor
    ~as_var:(function Var v -> Some v | Tech _ -> Option.None)
    ~of_var:(fun v -> (Var v : font_tech_descriptor))

let simplify_size_adjust_descriptor visible =
  simplify_typed_var visible ~read:Font_face.read_size_adjust
    ~as_var:(function
      | Font_face.Var v -> Some v | Font_face.Pct _ -> Option.None)
    ~of_var:(fun v -> (Font_face.Var v : Font_face.size_adjust))

(* [resolve_font_face_var] names the descriptors whose var() survives the parse
   and asks for one resolver each, so this pass and the parser cannot grow
   apart: a descriptor added to that table takes a resolver argument the call
   below has to supply. *)
let simplify_font_face_descriptor visible descriptor =
  match
    resolve_font_face_var
      ~src:(simplify_font_src_descriptor visible)
      ~unicode_range:(List.map (simplify_unicode_range_descriptor visible))
      ~font_family:(List.map (simplify_font_family_descriptor visible))
      ~font_style:(simplify_font_style_descriptor visible)
      ~font_weight:(simplify_font_weight_descriptor visible)
      ~font_stretch:(simplify_font_stretch_descriptor visible)
      ~font_display:(simplify_font_display_descriptor visible)
      ~font_variant:(simplify_font_variant_descriptor visible)
      ~font_feature_settings:(simplify_font_feature_settings_descriptor visible)
      ~font_variation_settings:
        (simplify_font_variation_settings_descriptor visible)
      ~metric_override:(simplify_metric_override_descriptor visible)
      ~font_tech:(simplify_font_tech_descriptor visible)
      ~size_adjust:(simplify_size_adjust_descriptor visible)
      descriptor
  with
  | Some resolved -> resolved
  | Option.None -> descriptor

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
  List.filter_map
    (fun descriptor ->
      match simplify_font_face_descriptor visible descriptor with
      | Font_family [ family ] as descriptor
        when Properties.is_font_family_name_value family ->
          Some descriptor
      | Font_family _ -> None
      | descriptor -> Some descriptor)
    descriptors

let substitute_page_with_margins ~scopes ~at_path sel descriptors margins =
  let visible = universal_visible_customs ~scopes ~at_path in
  let eval_page = Context.eval (context_for visible) in
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
        (substitute ~kept ~scopes ~parents ~at_path:(node :: at_path) body)
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
          Page (sel, List.map (Context.eval ctx) decls)
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
      when Custom_property_name.is_valid name ->
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
  | General_enclosed text -> refs_of_component_string text
  | Not condition -> refs_of_supports condition
  | And (a, b) | Or (a, b) -> refs_of_supports a @ refs_of_supports b

(* A style() query names a property; only a custom one is at risk here, and a
   real property like [style(color: red)] keeps its value through substitution.
   The name is dashed as written, so it needs no normalisation. *)
let refs_of_queried_name name =
  if Custom_property_name.is_valid name then [ name ] else []

(* CSS Conditional 5 sec. 6.2: a <style-range-value> that is a
   <custom-property-name> is substituted as if it were wrapped in a var(), so a
   bare [--gap] operand reads that property just as [var(--gap)] would. *)
let refs_of_style_range_value value =
  let named =
    match trim_components value with
    | [ Component.Preserved { kind = Token.Ident name; _ } ] ->
        refs_of_queried_name name
    | _ -> []
  in
  named @ refs_of_components value

let refs_of_style_range : Container.style_range -> string list = function
  | Compare { left; right; _ } ->
      refs_of_style_range_value left @ refs_of_style_range_value right
  | Interval { lower; name; upper; _ } ->
      refs_of_queried_name name
      @ refs_of_style_range_value lower
      @ refs_of_style_range_value upper

(* CSS Conditional 5 sec. 6.2: a [style()] query is evaluated against the
   computed value the queried property has on the query container, its boolean
   form against that property's initial value. The queried name is therefore a
   reference to whatever supplies that value, exactly like a [var()]: drop the
   declaration and the block stops matching. *)
let rec refs_of_style_query : Container.style_query -> string list = function
  | Boolean name -> refs_of_queried_name name
  | Declaration { name; value } ->
      refs_of_queried_name name @ refs_of_components value
  | Range range -> refs_of_style_range range
  | All (a, b) | Any (a, b) -> refs_of_style_query a @ refs_of_style_query b
  | Neg query -> refs_of_style_query query

(* CSS Conditional 5 sec. 6.3: scroll-state features are a fixed keyword set, so
   a scroll-state query reads no custom property. *)
let rec refs_of_scroll_state : Container.scroll_state_query -> string list =
  function
  | State _ -> []
  | Both (a, b) | Either (a, b) ->
      refs_of_scroll_state a @ refs_of_scroll_state b
  | Negated query -> refs_of_scroll_state query

let rec refs_of_container : Container.t -> string list = function
  | Min_width_rem _ | Min_width_px _ -> []
  (* A container name is a [<custom-ident>], so one spelled [--name] selects a
     container and reads no custom property. *)
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

let collect_scoped_refs stylesheet =
  let consumers = ref [] in
  let customs = ref [] in
  let runtime_refs = ref [] in
  let record_decl ~at_path ~selector decl =
    let refs = refs_of_declaration decl in
    Variables.vars_of_declarations [ decl ]
    |> List.iter (fun (Variables.V var) ->
        if var.Values.runtime then
          runtime_refs := ("--" ^ var.Values.name) :: !runtime_refs);
    match custom_name decl with
    | Some name -> customs := (at_path, selector, name, refs) :: !customs
    | None -> consumers := (at_path, selector, refs) :: !consumers
  in
  let rec walk_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, _) ->
        record_at_node_refs consumers ~parents ~at_path node;
        List.iter (walk_stmt ~parents ~at_path:(node :: at_path)) body
    | None -> walk_non_at ~parents ~at_path stmt
  and walk_non_at ~parents ~at_path stmt =
    match stmt with
    | Rule rule ->
        let eff = effective_selector ~parents rule.selector in
        List.iter (record_decl ~at_path ~selector:eff) rule.declarations;
        List.iter (walk_stmt ~parents:(eff :: parents) ~at_path) rule.nested
    | Declarations decls ->
        let sel = selector_for_parents_universal parents in
        List.iter (record_decl ~at_path ~selector:sel) decls
    (* Everything else that carries declarations - [@keyframes] frames, [@page]
       and its margin boxes, [@position-try], [@supports-condition] - through
       the exhaustive reader, so a reference cannot escape the census by sitting
       in an at-rule this match never listed. They apply to no element in
       particular, so a custom declared on [:root], [html] or [*] covers
       them. *)
    | stmt ->
        List.iter
          (record_decl ~at_path ~selector:(Selector.Universal None))
          (Stylesheet.statement_declarations stmt);
        List.iter
          (walk_stmt ~parents ~at_path)
          (Stylesheet.statement_children stmt)
  in
  List.iter (walk_stmt ~parents:[] ~at_path:[]) stylesheet;
  (!consumers, !customs, List.sort_uniq compare !runtime_refs)

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

(* Each question the liveness fixpoint asks used to scan a whole list, which
   made the walk cost a square in the variable count. Index them once: the
   reference names a path can see, the customs answering to a name, and the
   customs sharing a scope. *)
let index_customs key_of customs =
  let table = Hashtbl.create 64 in
  List.iter
    (fun custom ->
      let key = key_of custom in
      let prev = Option.value ~default:[] (Hashtbl.find_opt table key) in
      Hashtbl.replace table key (custom :: prev))
    customs;
  fun key -> Option.value ~default:[] (Hashtbl.find_opt table key)

let visible_ref_sets ~path_visible ~consumers =
  let cache = Hashtbl.create 16 in
  fun path ->
    match Hashtbl.find_opt cache path with
    | Some set -> set
    | None ->
        let set =
          List.fold_left
            (fun acc name -> Pp.String_set.add name acc)
            Pp.String_set.empty
            (visible_refs ~path_visible consumers path)
        in
        Hashtbl.replace cache path set;
        set

(* A scope enters the queue the first time anything in it goes live, and every
   custom in a live scope propagates - which is what the scan-to-fixpoint did,
   since it gated on the scope rather than on the entry. *)
let live_marker () =
  let live = Hashtbl.create 64 in
  let live_scopes = Hashtbl.create 64 in
  let pending = Queue.create () in
  let mark (path, sel, name) =
    if not (Hashtbl.mem live (path, sel, name)) then begin
      Hashtbl.replace live (path, sel, name) ();
      if not (Hashtbl.mem live_scopes (path, sel)) then begin
        Hashtbl.replace live_scopes (path, sel) ();
        Queue.add (path, sel) pending
      end
    end
  in
  (live, pending, mark)

let live_customs ~consumers ~customs =
  let path_visible ~scope_path ~consumer_path =
    at_path_prefix ~outer:scope_path ~inner:consumer_path
  in
  let visible_ref_set = visible_ref_sets ~path_visible ~consumers in
  let by_name = index_customs (fun (_, _, name, _) -> name) customs in
  let by_scope = index_customs (fun (path, sel, _, _) -> (path, sel)) customs in
  let live, pending, mark = live_marker () in
  (* Seed: every custom whose scope has a path-compatible consumer referencing
     its name is directly live. *)
  List.iter
    (fun (path, sel, name, _) ->
      if Pp.String_set.mem name (visible_ref_set path) then
        mark (path, sel, name))
    customs;
  (* Propagate: if a live custom [A] references a name [N], any custom [B] named
     [N] at a path that contains [A]'s path is also live. *)
  while not (Queue.is_empty pending) do
    let a_path, a_sel = Queue.pop pending in
    List.iter
      (fun (_, _, _, a_refs) ->
        List.iter
          (fun ref_name ->
            List.iter
              (fun (b_path, b_sel, b_name, _) ->
                if path_visible ~scope_path:b_path ~consumer_path:a_path then
                  mark (b_path, b_sel, b_name))
              (by_name ref_name))
          a_refs)
      (by_scope (a_path, a_sel))
  done;
  (* Read the answer off [customs], so it does not depend on the order the queue
     happened to take. *)
  List.filter_map
    (fun (path, sel, name, _) ->
      if Hashtbl.mem live (path, sel, name) then Some (path, sel, name)
      else None)
    customs

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

(* Write a value that is one colour back in its canonical spelling.
   [map_custom_value] rewrites the value in place, so the declaration keeps its
   importance, its cascade layer, its metadata and any theme guard; rebuilding
   from the name and the new text drops all four. *)
let normalize_custom_value decl =
  match custom_name decl with
  | None -> decl
  | Some _ -> (
      let value = Declaration.string_of_value ~minify:true decl in
      try
        let cursor = Cursor.of_string value in
        let color = Values.read_color cursor in
        Cursor.ws cursor;
        Cursor.expect_eof cursor;
        let canonical = Pp.to_string ~minify:true Values.pp_color color in
        Declaration.map_custom_value (fun _ -> canonical) decl
      with Cursor.Parse_error _ -> decl)

(* [keep] and [live_set] are fixed for the whole walk, and the walk asks about
   them once per declaration, so scanning either one costs a square in the
   declaration count. Read them into a set and a table first. *)
let live_lookup ~keep ~live_set =
  let kept = Pp.String_set.of_list keep in
  let live = Hashtbl.create (List.length live_set) in
  List.iter (fun entry -> Hashtbl.replace live entry ()) live_set;
  fun ~at_path ~selector name ->
    Pp.String_set.mem name kept || Hashtbl.mem live (at_path, selector, name)

let filter_live_custom_decls ~is_live ~at_path ~selector =
  List.filter_map (fun d ->
      match custom_name d with
      | None -> Some d
      | Some name ->
          if is_live ~at_path ~selector name then
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
  let is_live = live_lookup ~keep ~live_set in
  let filter_decls ~at_path ~selector =
    filter_live_custom_decls ~is_live ~at_path ~selector
  in
  let rec map_stmts ~parents ~at_path stmts =
    List.filter_map (map_stmt ~parents ~at_path) stmts
  and map_stmt ~parents ~at_path stmt =
    match at_wrapper stmt with
    | Some (node, body, rebuild) -> (
        match map_stmts ~parents ~at_path:(node :: at_path) body with
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

let normalise_var_name = Custom_property_name.add_prefix

(* Every custom-property name the stylesheet still mentions: declared by a
   declaration, or referenced by a [var()] in a declaration or in an at-rule
   condition, fallbacks included. [collect_scoped_refs] never descends into a
   [@property] body, so a registration is not a mention of the property it
   registers and cannot keep itself. *)
let mentioned_custom_names stylesheet =
  let consumers, customs, _ = collect_scoped_refs stylesheet in
  List.fold_left
    (fun acc (_, _, refs) -> List.rev_append refs acc)
    (List.fold_left
       (fun acc (_, _, name, refs) ->
         List.rev_append refs (normalise_var_name name :: acc))
       [] customs)
    consumers
  |> List.sort_uniq compare

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
    (collect_scopes ~kept:[] stylesheet).all;
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
    | None ->
        add_refs (Stylesheet.statement_declarations stmt);
        List.iter walk (Stylesheet.statement_children stmt)
  in
  List.iter walk stylesheet;
  (counts, List.sort_uniq compare !referenced)

(** {1 Layer-decided custom-property folding} *)

(* A variable redefined across cascade layers on the same element is a real
   override, but - unlike an @media or dark-mode override - its winner is
   statically decidable, so it can be folded to that winner instead of kept
   live. Resolving the winner here is what lets [statements_for_inline] drop the
   [@layer] wrappers: flattened, the losing definitions would compete by
   document order and pick the wrong value. *)

(* Document-order cascade layer names (dotted for nesting), low precedence
   first: the order in which layers are first introduced (css-cascade-5 sec.
   6.4.2). Keyed by the CSS text of the whole path, so a [.] one ident carries
   is not the separator between two (sec. 6.4.1). [None] when a layer sits where
   cascade cannot place it: a conditional group introduces its layers only when
   its condition holds, and an [Origin] block carries its own layer stack. What
   a [@container], [@scope] or [@starting-style] block holds, and what a rule
   nests, always exists, so a layer named there counts where it is written. *)
let layer_order stylesheet : string list option =
  let seen = Hashtbl.create 16 in
  let order = ref [] in
  let undecided = ref false in
  let add path =
    let name = Stylesheet.string_of_layer_name path in
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      order := name :: !order
    end
  in
  let rec walk ~placed prefix stmts = List.iter (statement ~placed prefix) stmts
  and statement ~placed prefix stmt =
    match stmt with
    | Stylesheet.Layer_decl names ->
        if not placed then undecided := true;
        List.iter (fun n -> add (prefix @ n)) names
    | Stylesheet.Layer (name, body) ->
        if not placed then undecided := true;
        let prefix =
          match name with
          | None -> prefix
          | Some n ->
              let full = prefix @ n in
              add full;
              full
        in
        walk ~placed prefix body
    | Stylesheet.Media (_, body)
    | Stylesheet.Supports (_, body)
    | Stylesheet.Moz_document (_, body)
    | Stylesheet.When (_, body)
    | Stylesheet.Else (_, body)
    | Stylesheet.Origin (_, body) ->
        walk ~placed:false prefix body
    | stmt -> walk ~placed prefix (Stylesheet.statement_children stmt)
  in
  walk ~placed:true [] stylesheet;
  if !undecided then None else Some (List.rev !order)

module Slot_table = Hashtbl.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

(* One declaration's stake in one cascade slot: where the layer stack ranks it,
   and where specificity and document order leave it once the wrappers are gone.
   [index] keeps both orders total, so two claims a single rule writes never
   compare equal. *)
type layer_claim = {
  rank : int;
  position : int;
  specificity : Selector.specificity;
  important : bool;
  index : int;
}

(* Where specificity and document order leave two claims once the wrappers are
   gone. Ascending, so the last claim standing is the one that wins. *)
let compare_flattened a b =
  match Bool.compare a.important b.important with
  | 0 -> (
      match Selector.compare_specificity a.specificity b.specificity with
      | 0 -> (
          match Int.compare a.position b.position with
          | 0 -> Int.compare a.index b.index
          | order -> order)
      | order -> order)
  | order -> order

(* And where the layer stack leaves them. Importance outranks the stack and
   comes through flattening untouched, so it settles a pair either way; between
   two layers the stack decides, reversed for important declarations (CSS
   Cascade 5 sec. 6.4.2); inside one layer the same things decide as after. *)
let compare_layered a b =
  match Bool.compare a.important b.important with
  | 0 ->
      let rank =
        if a.important then Int.compare b.rank a.rank
        else Int.compare a.rank b.rank
      in
      if rank <> 0 then rank else compare_flattened a b
  | order -> order

(* Two total orders over the same claims are the same order exactly when one's
   sequence is sorted under the other, so whatever subset of them an element
   matches, it keeps the winner it had. *)
let slot_survives_flattening claims =
  let rec ascending = function
    | a :: (b :: _ as rest) -> compare_flattened a b < 0 && ascending rest
    | _ -> true
  in
  ascending (List.stable_sort compare_layered claims)

(* A selector list matches an element through one branch at a time, and it is
   that branch's specificity the cascade weighs. *)
let selector_specificities selector =
  match Selector.as_list selector with
  | Some branches -> List.map Selector.specificity branches
  | None -> [ Selector.specificity selector ]

(* Record what one declaration stakes in every slot it writes. [false] when it
   is broad enough that no footprint tells it apart from anything. *)
let add_claim ~slots ~index ~rank ~position ~specificities decl =
  if Shorthand.declaration_is_broad decl then false
  else begin
    let important = Declaration.is_important decl in
    List.iter
      (fun specificity ->
        incr index;
        let claim =
          { rank; position; specificity; important; index = !index }
        in
        List.iter
          (fun slot ->
            let prev =
              Option.value ~default:[] (Slot_table.find_opt slots slot)
            in
            Slot_table.replace slots slot (claim :: prev))
          (Shorthand.declaration_overlap_keys decl))
      specificities;
    true
  end

(* Every declaration's stake in the slots it writes, indexed by slot, or [None]
   for a sheet holding what this does not reach: an anonymous layer, another
   origin's stack or a [@scope] block's proximity rule, a nested rule whose
   subject it does not resolve, a declaration broad enough to write any slot at
   all, or anything but a style rule inside a layer, [@keyframes] and friends
   included, whose name resolution the stack also orders. *)
let layer_claims ~ranks ~unlayered stylesheet =
  let slots = Slot_table.create 64 in
  let reaches = ref true in
  let position = ref 0 in
  let index = ref 0 in
  let rec walk ~layer ~rank stmts = List.iter (statement ~layer ~rank) stmts
  and statement ~layer ~rank stmt =
    match stmt with
    | Stylesheet.Layer (None, _) | Stylesheet.Origin _ | Stylesheet.Scope _ ->
        reaches := false
    | Stylesheet.Layer (Some name, body) ->
        let layer = layer @ name in
        let rank =
          Option.value ~default:unlayered
            (Hashtbl.find_opt ranks (Stylesheet.string_of_layer_name layer))
        in
        walk ~layer ~rank body
    | Stylesheet.Layer_decl _ -> ()
    | Stylesheet.Rule r ->
        incr position;
        if r.nested <> [] then reaches := false
        else
          let specificities = selector_specificities r.selector in
          let position = !position in
          List.iter
            (fun decl ->
              if
                not
                  (add_claim ~slots ~index ~rank ~position ~specificities decl)
              then reaches := false)
            r.declarations
    | Stylesheet.Media (_, body)
    | Stylesheet.Supports (_, body)
    | Stylesheet.Container (_, _, body)
    | Stylesheet.Moz_document (_, body)
    | Stylesheet.When (_, body)
    | Stylesheet.Else (_, body)
    | Stylesheet.Starting_style body ->
        walk ~layer ~rank body
    | _ -> if rank <> unlayered then reaches := false
  in
  walk ~layer:[] ~rank:unlayered stylesheet;
  if !reaches then Some slots else None

(* Whether dropping every [@layer] wrapper leaves the same declaration winning
   each cascade slot. Unwrapping replays the layer stack as document order and
   hands the decision back to specificity, so it holds only where the two orders
   already agree on every slot two layers write, and [layer_order] is what ranks
   them: a sheet it cannot rank is one this cannot answer for. *)
let flattening_layers_is_safe stylesheet =
  match layer_order stylesheet with
  | None -> false
  | Some order -> (
      let ranks = Hashtbl.create 16 in
      List.iteri (fun rank name -> Hashtbl.replace ranks name rank) order;
      match layer_claims ~ranks ~unlayered:(List.length order) stylesheet with
      | None -> false
      | Some slots ->
          Slot_table.fold
            (fun _ claims ok -> ok && slot_survives_flattening claims)
            slots true)

(* [Some layer] for a purely-layered path ([layer = None] unlayered, [Some name]
   the dotted layer), [None] when the path is conditional or holds an anonymous
   layer - those are not statically foldable. *)
let foldable_layer_of_at_path at_path : string option option =
  (* Walking an innermost-first path and consing leaves [path] outermost-first,
     which is the order the dotted layer name reads in. *)
  let rec go path : at_node list -> string option option = function
    | [] -> (
        match path with
        | [] -> Some None
        | ns -> Some (Some (Stylesheet.string_of_layer_name (List.concat ns))))
    | Layer (Some n) :: rest -> go (n :: path) rest
    | Layer None :: _ -> None
    | _ :: _ -> None
  in
  go [] at_path

(* A copy of [d] tagged with [layer] and its own importance, for the cascade
   resolver, or [None] when [d] is not a pair this library writes back. *)
let annotate_layer (layer : string option) d =
  let name = Declaration.property_name d in
  let value = Declaration.string_of_value ~minify:true d in
  Option.map
    (fun base ->
      if Declaration.is_important d then Declaration.important base else base)
    (rebuild_custom ?layer name value)

(* Every definition of one name tagged with its layer, or [None] as soon as one
   of them is not a pair this library writes back: resolving a subset would
   resolve a different cascade. *)
let annotate_every_layer entries =
  List.fold_left
    (fun acc (_, fl, d) ->
      Option.bind acc (fun acc ->
          Option.bind fl (fun l ->
              Option.map (fun a -> a :: acc) (annotate_layer l d))))
    (Option.Some []) entries

(* Names whose every definition is unconditional, on one selector, and spread
   over two or more layers, mapped to the resolved cascade winner ([`Unset] when
   it resolves to no value). A single scope with repeated declarations is left
   alone: the census already treats it as one inlinable definition. *)
let layer_decided_customs ~keep stylesheet =
  let fold = Hashtbl.create 16 in
  match layer_order stylesheet with
  | None -> fold
  | Some layer_order ->
      let scopes = (collect_scopes ~kept:keep stylesheet).all in
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
      Hashtbl.iter
        (fun name entries ->
          let entries = List.rev entries in
          let unconditional =
            List.for_all (fun (_, fl, _) -> Option.is_some fl) entries
          in
          let one_selector =
            match entries with
            | (sel0, _, _) :: rest ->
                List.for_all (fun (s, _, _) -> s = sel0) rest
            | [] -> true
          in
          let distinct_layers =
            entries
            |> List.filter_map (fun (_, fl, _) -> fl)
            |> List.sort_uniq compare |> List.length
          in
          if unconditional && one_selector && distinct_layers >= 2 then
            match annotate_every_layer entries with
            | Option.None -> ()
            | Option.Some annotated -> (
                match
                  Context.winning_custom_declaration ~layer_order annotated
                with
                | Some w ->
                    Hashtbl.replace fold name
                      (`Value (Declaration.string_of_value ~minify:true w))
                | None -> Hashtbl.replace fold name `Unset))
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
                  Some (Option.value ~default:d (rebuild_custom n v)))
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
  let _, _, runtime_refs = collect_scoped_refs stylesheet in
  let keep =
    List.map normalise_var_name keep_vars @ runtime_refs
    |> List.sort_uniq compare
  in
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
  let original_consumers, original_customs, _ =
    collect_scoped_refs stylesheet
  in
  let cyclic_live_set =
    cyclic_live_customs ~consumers:original_consumers ~customs:original_customs
  in
  let substituted =
    substitute ~kept ~scopes ~parents:[] ~at_path:[] stylesheet
  in
  let consumers, customs, _ = collect_scoped_refs substituted in
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
  | Some [] -> [ Stylesheet.Layer (None, body) ]
  | Some n -> [ Stylesheet.Layer (Some n, body) ]

(* Layer/supports/media guard checks. When [layer_order] is empty, every layer
   name is treated as known (the caller hasn't declared an order, so we defer to
   the runtime cascade). When [query] is [None], supports/media guards pass
   through and survive as wrapping at-rules in the inlined body; when a query is
   supplied the guard is evaluated and the import is dropped on rejection. *)
let layer_guard_passes ~(layer_order : string list) (rule : import_rule) =
  match ((rule.layer : Stylesheet.layer_name option), layer_order) with
  | None, _ | _, [] -> true
  | Some name, order -> List.mem (Stylesheet.string_of_layer_name name) order

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
  match read cursor with
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
