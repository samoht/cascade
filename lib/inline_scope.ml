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
    let n = String.length a in
    String.length c > n && String.starts_with ~prefix:a c && c.[n] = ' '

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
    | Stylesheet.(
        ( Bang_comment _ | Charset _ | Import _ | Namespace _ | Layer_decl _
        | Layer _ | Media _ | Container _ | Supports _ | Moz_document _
        | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
        | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
        | Font_face _ | Counter_style _ | Page _ | Page_with_margins _
        | Font_palette_values _ | Font_feature_values _ | View_transition _
        | Position_try _ | Viewport _ | Unknown_at_rule _ )) ->
        ()
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
