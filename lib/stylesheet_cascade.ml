open Stylesheet_intf

let origin_importance_rank ~important = function
  | Transition -> 9
  | User_agent when important -> 8
  | User when important -> 7
  | Author when important -> 6
  | Animation -> 5
  | Author -> 4
  | Author_presentational_hint -> 3
  | User -> 2
  | User_agent -> 1

let import_layer_name ({ layer; _ } : import_rule) = layer

let layer_block_name = function
  | Layer (Some name, _) -> Some name
  | Layer (None, _) -> Some []
  | _ -> None

let layer_statement_name_list = function
  | Layer_decl names -> Some names
  | _ -> None

let rec index_of x i : string list -> int option = function
  | [] -> None
  | y :: ys -> if x = y then Some i else index_of x (i + 1) ys

let cascade_layer_precedence_rank ~layer_order ~important
    (layer : string option) =
  let layer_count = List.length layer_order in
  match (important, layer) with
  | false, None -> layer_count
  | false, Some name ->
      Option.value ~default:layer_count (index_of name 0 layer_order)
  | true, None -> 0
  | true, Some name ->
      let i = Option.value ~default:layer_count (index_of name 0 layer_order) in
      layer_count - i

let compare_cascade_layer_candidate ~layer_order (a : cascade_layer_candidate)
    (b : cascade_layer_candidate) =
  let key (c : cascade_layer_candidate) =
    ( (if c.important then 1 else 0),
      cascade_layer_precedence_rank ~layer_order ~important:c.important c.layer,
      c.source_order )
  in
  compare (key a) (key b)

let winning_cascade_layer_candidate ~layer_order
    (candidates : cascade_layer_candidate list) : cascade_layer_candidate option
    =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if
               compare_cascade_layer_candidate ~layer_order winner candidate < 0
             then candidate
             else winner)
           first rest)

let cascade_revert_layer_candidates ~layer_order ~important ~current_layer
    (candidates : cascade_layer_candidate list) =
  let current_rank =
    cascade_layer_precedence_rank ~layer_order ~important current_layer
  in
  List.filter
    (fun (candidate : cascade_layer_candidate) ->
      candidate.important = important
      && cascade_layer_precedence_rank ~layer_order ~important candidate.layer
         < current_rank)
    candidates

let compare_cascade_origin_candidate (a : cascade_origin_candidate)
    (b : cascade_origin_candidate) =
  let key (c : cascade_origin_candidate) =
    (origin_importance_rank ~important:c.important c.origin, c.source_order)
  in
  compare (key a) (key b)

let winning_cascade_origin_candidate
    (candidates : cascade_origin_candidate list) :
    cascade_origin_candidate option =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if compare_cascade_origin_candidate winner candidate < 0 then
               candidate
             else winner)
           first rest)

let revert_origin_rollback_origins = function
  | User_agent -> []
  | User -> [ User_agent ]
  | Author_presentational_hint | Author | Animation -> [ User_agent; User ]
  | Transition ->
      [ User_agent; User; Author_presentational_hint; Author; Animation ]

let cascade_revert_origin_candidates ~important ~current_origin
    (candidates : cascade_origin_candidate list) =
  let origins = revert_origin_rollback_origins current_origin in
  List.filter
    (fun (candidate : cascade_origin_candidate) ->
      candidate.important = important
      && List.exists (( = ) candidate.origin) origins)
    candidates

let declared_values ?property declarations =
  declarations
  |> List.mapi (fun source_order declaration ->
      let declaration_property = Declaration.property_name declaration in
      let value = Declaration.string_of_value ~minify:true declaration in
      {
        property = declaration_property;
        value;
        important = Declaration.is_important declaration;
        source_order;
      })
  |> List.filter (fun declared ->
      match property with
      | None -> true
      | Some property -> declared.property = property)

let cascaded_value candidates =
  winning_cascade_origin_candidate candidates
  |> Option.map (fun (c : cascade_origin_candidate) -> c.value)

let scope_proximity_rank : int option -> int = function
  | None -> min_int
  | Some hops -> -hops

let compare_cascade_candidate ~layer_order (a : cascade_candidate)
    (b : cascade_candidate) =
  let key (c : cascade_candidate) =
    ( origin_importance_rank ~important:c.important c.origin,
      cascade_layer_precedence_rank ~layer_order ~important:c.important c.layer,
      c.specificity,
      scope_proximity_rank c.scope_hops,
      c.source_order )
  in
  compare (key a) (key b)

let winning_cascade_candidate ~layer_order (candidates : cascade_candidate list)
    : cascade_candidate option =
  match candidates with
  | [] -> None
  | first :: rest ->
      Some
        (List.fold_left
           (fun winner candidate ->
             if compare_cascade_candidate ~layer_order winner candidate < 0 then
               candidate
             else winner)
           first rest)

let value ~inherits ~initial ~inherited ~cascaded =
  let inherited_or_initial () = Option.value ~default:initial inherited in
  match cascaded with
  | Some "initial" -> { value = initial; value_source = Initial_keyword }
  | Some "inherit" ->
      { value = inherited_or_initial (); value_source = Inherit_keyword }
  | Some "unset" when inherits ->
      { value = inherited_or_initial (); value_source = Unset_inherited }
  | Some "unset" -> { value = initial; value_source = Unset_initial }
  | Some value -> { value; value_source = Cascaded }
  | None when inherits ->
      { value = inherited_or_initial (); value_source = Inherited_default }
  | None -> { value = initial; value_source = Initial_default }

(* Resolve a chain of [revert] winners. Each revert exposes the next-lower
   origin's winning candidate; if THAT is also [revert], peel another origin
   off, and so on until a non-revert winner or no candidates remain. The
   rollback context comes from the winner itself so callers cannot pass a
   [current_origin] that disagrees with the candidate they exposed. *)
let rec resolve_revert_chain candidates =
  match winning_cascade_origin_candidate candidates with
  | Some winner when winner.value = "revert" ->
      cascade_revert_origin_candidates ~important:winner.important
        ~current_origin:winner.origin candidates
      |> resolve_revert_chain
  | other -> other

let specified_value_after_revert ~inherits ~initial ~inherited candidates =
  let cascaded =
    resolve_revert_chain candidates
    |> Option.map (fun (c : cascade_origin_candidate) -> c.value)
  in
  value ~inherits ~initial ~inherited ~cascaded

let rec resolve_revert_layer_chain ~layer_order candidates =
  match winning_cascade_layer_candidate ~layer_order candidates with
  | Some winner when winner.value = "revert-layer" ->
      cascade_revert_layer_candidates ~layer_order ~important:winner.important
        ~current_layer:winner.layer candidates
      |> resolve_revert_layer_chain ~layer_order
  | other -> other

let specified_value_after_revert_layer ~inherits ~initial ~inherited
    ~layer_order candidates =
  match resolve_revert_layer_chain ~layer_order candidates with
  | Some winner ->
      value ~inherits ~initial ~inherited ~cascaded:(Some winner.value)
  | None -> value ~inherits ~initial ~inherited ~cascaded:None

let value_processing_requires_document_context = function
  | Declared_value | Cascaded_value | Specified_value -> false
  | Computed_value | Used_value | Actual_value -> true
