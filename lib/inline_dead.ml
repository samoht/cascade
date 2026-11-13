open Stylesheet
open Inline_scope
open Inline_subst

(** {1 Pass 3 - dead-code elimination} *)

let strip_component_ws =
  List.filter (function
    | Component.Preserved { kind = Token.Whitespace _; _ } -> false
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
  | Declaration (_, decl) ->
      names_of_vars (Variables.vars_of_declarations [ decl ])
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
  | _ ->
      (* The typed reading names the [var()] the value is built on; a name
         written inside another reference's fallback reaches the census only
         through the token stream, and it is referenced like any other. *)
      names_of_vars (Variables.vars_of_declarations [ decl ])
      @ refs_of_component_string
          (Declaration.string_of_value ~minify:false decl)
      |> List.sort_uniq String.compare

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
  (* A reference still in the sheet is answered by the browser's own cascade, so
     a definition of the name it holds is live wherever it sits. The at-rule
     path decides what may be folded here; it never decides what a reference
     left standing can reach. *)
  let path_visible ~scope_path:_ ~consumer_path:_ = true in
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
    | Stylesheet.(
        ( Bang_comment _ | Charset _ | Import _ | Namespace _ | Layer_decl _
        | Layer _ | Media _ | Container _ | Supports _ | Moz_document _
        | Starting_style _ | When _ | Else _ | Supports_condition _ | Origin _
        | Scope _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
        | Font_face _ | Counter_style _ | Page_with_margins _
        | Font_palette_values _ | Font_feature_values _ | View_transition _
        | Position_try _ | Viewport _ | Unknown_at_rule _ )) as other ->
        Some other
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
