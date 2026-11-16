(* The container walk: [@media], [@layer], [@supports], [@container],
   [@property], [@keyframes], nesting and other at-rule blocks, plus imports and
   the cascade layer order. *)

open Cascade
open Tree_diff_intf
open Tree_diff_rules

(* Generic helpers for processing nested containers *)
let extract_items_with_positions extract_fn stmts =
  List.mapi
    (fun i stmt ->
      match extract_fn stmt with
      | Some (cond, rules) -> Some (i, cond, rules)
      | None -> None)
    stmts
  |> List.filter_map (fun x -> x)

let group_by_condition items =
  Group.by (fun (pos, cond, rules) -> (cond, (pos, rules))) items

(* Two sides holding a different number of blocks under one condition split or
   merged them. Where those blocks sit is a separate question, and one
   [moved_conditions] answers: comparing their absolute indices here reported
   the whole tail of a stylesheet as restructured whenever a block was inserted
   ahead of it, which is what the slack distance was there to hide. *)
let detect_block_structure_changes blocks1 blocks2 =
  let block_structure_changed = Hashtbl.create 16 in
  Hashtbl.iter
    (fun cond blocks1_list ->
      match Hashtbl.find_opt blocks2 cond with
      | Some blocks2_list ->
          if List.length blocks1_list <> List.length blocks2_list then
            Hashtbl.replace block_structure_changed cond
              (blocks1_list, blocks2_list)
      | _ -> ())
    blocks1;
  block_structure_changed

let condition_position ~condition_of cond stmts =
  let rec go i = function
    | [] -> None
    | stmt :: rest -> (
        match condition_of stmt with
        | Some c when c = cond -> Some i
        | _ -> go (i + 1) rest)
  in
  go 0 stmts

let reordered_container container_type cond rules1 pos1 pos2 =
  Reordered
    {
      info = { container_type; condition = cond; rules = rules1 };
      expected_pos = pos1;
      actual_pos = pos2;
    }

(* The entry for a container that changed places, naming where it went. *)
let container_moved ~container_type ~condition_of ~stmts1 ~stmts2 cond rules =
  match
    ( condition_position ~condition_of cond stmts1,
      condition_position ~condition_of cond stmts2 )
  with
  | Some pos1, Some pos2 ->
      Some (reordered_container container_type cond rules pos1 pos2)
  | None, _ | _, None -> None

(* One entry per container that only changed places. [reported] holds the
   conditions an entry already names, so a block that also changed content is
   named once, by the entry that says what changed, and a condition several
   blocks share is named once for the group. Every condition both sides hold is
   asked, not only the ones whose bodies differ: a block that kept its body and
   swapped with the rule below it changes which declaration wins. *)
(* Take one value from a condition's bucket, leaving the rest. A stylesheet may
   write one condition several times, so a bucket holds an occurrence apiece and
   each is answered where the sheet writes it. *)
let take_bucket tbl key =
  match Hashtbl.find_opt tbl key with
  | Some (v :: rest) ->
      Hashtbl.replace tbl key rest;
      Some v
  | Some [] | None -> None

let container_reorders ~container_type ~condition_of ~moved_conds ~reported
    ~stmts1 ~stmts2 items =
  List.filter_map
    (fun (cond, rules) ->
      if (not (Hashtbl.mem moved_conds cond)) || Hashtbl.mem reported cond then
        None
      else (
        Hashtbl.replace reported cond ();
        container_moved ~container_type ~condition_of ~stmts1 ~stmts2 cond rules))
    items

let modified_container container_type cond rules1 rules2 rule_changes
    nested_containers =
  Modified
    {
      info = { container_type; condition = cond; rules = rules1 };
      actual_rules = rules2;
      rule_changes;
      container_changes = nested_containers;
    }

let detect_order_only_change ~container_type added removed items1 items2 =
  if added <> [] || removed <> [] then None
  else if List.length items1 <> List.length items2 || items1 = [] then None
  else
    let conds1 = List.map fst items1 in
    let conds2 = List.map fst items2 in
    if conds1 = conds2 then None
    else
      match (items1, items2) with
      | (cond, rules1) :: _, (_, rules2) :: _ ->
          Some
            (Modified
               {
                 info = { container_type; condition = cond; rules = rules1 };
                 actual_rules = rules2;
                 rule_changes = [];
                 container_changes = [];
               })
      | _ -> None

(* The descriptors an [@property] body carries, in the order CSS Properties and
   Values 1 sec. 2 defines them. The syntax and the initial value are
   existentially typed and share that existential, so they are compared on the
   form they serialise to rather than on the value. *)
let property_descriptors = function
  | Css.Property_info { syntax; inherits; initial_value; _ } ->
      ("syntax", Pp.to_string ~minify:true Css.Variables.pp_syntax syntax)
      :: ("inherits", if inherits then "true" else "false")
      ::
      (match initial_value with
      | None -> []
      | Some value ->
          [
            ( "initial-value",
              Pp.to_string ~minify:true (Css.Variables.pp_value syntax) value );
          ])

(* A registration decides how every use of the custom property parses, animates
   and inherits, so a descriptor that differs is a difference. *)
let property_descriptor_changes prop1 prop2 =
  let descs1 = property_descriptors prop1 in
  let descs2 = property_descriptors prop2 in
  let changed =
    List.filter_map
      (fun (name, expected_value) ->
        match List.assoc_opt name descs2 with
        | Some actual_value when actual_value <> expected_value ->
            Some { property_name = name; expected_value; actual_value }
        | _ -> None)
      descs1
  in
  let only_in others ((name, _) as descriptor) =
    if List.mem_assoc name others then None else Some descriptor
  in
  ( changed,
    List.filter_map (only_in descs1) descs2,
    List.filter_map (only_in descs2) descs1 )

let property_diff items1 items2 =
  let key_of (Css.Property_info { name; _ }) = name in
  let is_empty_diff prop1 prop2 =
    let (Css.Property_info { name = n1; _ }) = prop1 in
    let (Css.Property_info { name = n2; _ }) = prop2 in
    n1 = n2 && property_descriptors prop1 = property_descriptors prop2
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~is_empty_diff items1 items2
  in
  let added =
    List.map (fun (Css.Property_info { name; _ }) -> (name, [])) added
  in
  let removed =
    List.map (fun (Css.Property_info { name; _ }) -> (name, [])) removed
  in
  let modified =
    List.map
      (fun ((Css.Property_info { name; _ } as prop1), prop2) ->
        let changed, added, removed = property_descriptor_changes prop1 prop2 in
        (name, changed, added, removed))
      modified_pairs
  in
  (added, removed, modified)

let property_reorder_diff names2 (i1, name1) =
  let i2 = List.find_index (( = ) name1) names2 |> Option.value ~default:i1 in
  if i1 = i2 then None
  else
    let swapped_with =
      if i1 < List.length names2 then Some ("@property " ^ List.nth names2 i1)
      else None
    in
    (Some
       (Reordered
          {
            selector = "@property " ^ name1;
            expected_pos = i1;
            actual_pos = i2;
            swapped_with;
            old_declarations = None;
            new_declarations = None;
          })
      : rule_diff option)

let property_reorder_container stmts1 stmts2 reorder_diffs =
  match reorder_diffs with
  | [] -> []
  | _ ->
      [
        Modified
          {
            info =
              {
                container_type = `Property;
                condition = "@property rules";
                rules = stmts1;
              };
            actual_rules = stmts2;
            rule_changes = reorder_diffs;
            container_changes = [];
          };
      ]

let property_reorder_diffs stmts1 stmts2 items1 items2 =
  let get_names items =
    List.map (fun (Css.Property_info { name; _ }) -> name) items
  in
  let names1 = get_names items1 in
  let names2 = get_names items2 in
  let names1_set = List.sort String.compare names1 in
  let names2_set = List.sort String.compare names2 in
  if not (names1_set = names2_set && names1 <> names2 && names1 <> []) then []
  else
    let reorder_diffs =
      List.filter_map
        (property_reorder_diff names2)
        (List.mapi (fun i n -> (i, n)) names1)
    in
    property_reorder_container stmts1 stmts2 reorder_diffs

let extract_media_as_string stmt =
  match Css.as_media stmt with
  | Some (cond, rules) -> Some (Css.Media.to_string cond, rules)
  | None -> None

let extract_supports_as_string stmt =
  match Css.as_supports stmt with
  | Some (cond, rules) ->
      (* Two spellings of one condition are one block: CSS Conditional 3 (ED)
         sec. 7.4 calls a token stream simplification the same condition, so a
         sheet writing [(color:color-mix(in lab, red, red))] and one writing it
         without the spaces must compare inside the block rather than across
         two. *)
      Some (Css.Supports.to_string ~verbatim:false cond, rules)
  | None -> None

(* The name [layer_diff] keys a layer on: the CSS text of the name, so a [.] one
   ident carries is not the separator between two (CSS Cascade 5 sec. 6.4.1). An
   anonymous [@layer { ... }] has no name, so it keys on the empty string like
   every other anonymous one. *)
let layer_key name_opt =
  Option.fold ~none:"" ~some:Css.Stylesheet.string_of_layer_name name_opt

let extract_layer_name stmt =
  match Css.as_layer stmt with
  | Some (name_opt, rules) -> Some (layer_key name_opt, rules)
  | None -> None

let keyframes_container_info name =
  {
    container_type = `At_rule;
    condition = String.concat "" [ "@keyframes "; name ];
    rules = [];
  }

let keyframe_frames_diff frames1 frames2 =
  let key_of (frame : Css.keyframe) = Css.Keyframe.to_string frame.selector in
  let is_empty_diff (f1 : Css.keyframe) (f2 : Css.keyframe) =
    Css.Keyframe.selector_equal f1.selector f2.selector
    && List.equal Declaration.equal_declaration f1.declarations f2.declarations
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~is_empty_diff frames1 frames2
  in
  let selector_str (frame : Css.keyframe) =
    Css.Keyframe.to_string frame.selector
  in
  (* A frame carries its declarations like any other rule: naming one modified
     without saying what changed states a difference the report then
     withholds. *)
  let added_changes =
    List.map
      (fun (frame : Css.keyframe) ->
        (Added
           { selector = selector_str frame; declarations = frame.declarations }
          : rule_diff))
      added
  in
  let removed_changes =
    List.map
      (fun (frame : Css.keyframe) ->
        (Removed
           { selector = selector_str frame; declarations = frame.declarations }
          : rule_diff))
      removed
  in
  let modified_changes =
    List.filter_map
      (fun ((f1 : Css.keyframe), (f2 : Css.keyframe)) ->
        if
          not
            (List.equal Declaration.equal_declaration f1.declarations
               f2.declarations)
        then
          Some
            (content_changed (selector_str f1) f1.declarations f2.declarations)
        else None)
      modified_pairs
  in
  added_changes @ removed_changes @ modified_changes

let keyframes_diff items1 items2 =
  let key_of (name, _) = name in
  let is_empty_diff (name1, frames1) (name2, frames2) =
    name1 = name2 && frames1 = frames2
  in
  diffs ~key_of ~is_empty_diff items1 items2

let process_nested_keyframes stmts1 stmts2 =
  let items1 = List.filter_map Css.as_keyframes stmts1 in
  let items2 = List.filter_map Css.as_keyframes stmts2 in
  let added, removed, modified = keyframes_diff items1 items2 in
  let added_diffs =
    List.map
      (fun (name, _frames) -> Added (keyframes_container_info name))
      added
  in
  let removed_diffs =
    List.map
      (fun (name, _frames) -> Removed (keyframes_container_info name))
      removed
  in
  let modified_diffs =
    List.filter_map
      (fun ((name, frames1), (_, frames2)) ->
        let frame_diffs = keyframe_frames_diff frames1 frames2 in
        if frame_diffs <> [] then
          Some
            (Modified
               {
                 info = keyframes_container_info name;
                 actual_rules = [];
                 rule_changes = frame_diffs;
                 container_changes = [];
               })
        else None)
      modified
  in
  added_diffs @ removed_diffs @ modified_diffs

let container_condition_string name_opt condition =
  let cond_str =
    match condition with Some c -> Css.Container.to_string c | None -> ""
  in
  match name_opt with Some name -> name ^ " " ^ cond_str | None -> cond_str

let container_key (name_opt, condition, _) =
  (* Use both name and condition as key to distinguish different containers. *)
  String.concat ":"
    [
      Option.value ~default:"" name_opt;
      Option.fold ~none:"" ~some:Css.Container.to_string condition;
    ]

let condition_rules_of_container (name_opt, condition, rules) =
  (container_condition_string name_opt condition, rules)

let extract_container_as_string stmt =
  Option.map condition_rules_of_container (Css.as_container stmt)

let modified_container_of_pair ((name_opt, condition, rules1), (_, _, rules2)) =
  (container_condition_string name_opt condition, rules1, rules2)

(* Process property rules. An [@property] body holds descriptors, not
   statements, so there is nothing below it to recurse into. *)
let process_nested_properties stmts1 stmts2 =
  let items1 = List.filter_map Css.as_property stmts1 in
  let items2 = List.filter_map Css.as_property stmts2 in
  let added, removed, modified = property_diff items1 items2 in
  let diffs = ref [] in
  List.iter
    (fun (name, rules) ->
      diffs :=
        Added { container_type = `Property; condition = name; rules } :: !diffs)
    added;
  List.iter
    (fun (name, rules) ->
      diffs :=
        Removed { container_type = `Property; condition = name; rules }
        :: !diffs)
    removed;
  List.iter
    (fun (name, property_changes, added_properties, removed_properties) ->
      (* The body is reported as the descriptors that changed. Left empty, the
         renderer has nothing to show and falls back to calling the entry a
         position change, which is not what differs. *)
      let rule_changes =
        [
          Content_changed
            {
              selector = "";
              old_declarations = [];
              new_declarations = [];
              property_changes;
              added_properties;
              removed_properties;
            };
        ]
      in
      diffs :=
        Modified
          {
            info = { container_type = `Property; condition = name; rules = [] };
            actual_rules = [];
            rule_changes;
            container_changes = [];
          }
        :: !diffs)
    modified;
  !diffs @ property_reorder_diffs stmts1 stmts2 items1 items2

(* Every rule as [(selector_key, label, nested_statements)], including the ones
   with an empty nested body so an added or removed body is seen. The two sides
   pair on [selector_key_of_selector], so a rule whose comma group they write in
   a different order is still the same rule and its body is still compared;
   pairing on the printed selector dropped the difference inside it. The label
   is what the report shows. *)
let rule_nesting_items stmts =
  List.filter_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (sel, _decls, nested) ->
          Some (selector_key_of_selector sel, Css.Selector.to_string sel, nested)
      | None -> None)
    stmts

(* Mutual recursion declarations *)
(* Check if two rule-lists under the same media condition differ *)
let rec media_condition_differs rules_list1 rules_list2 =
  let block_count_differs =
    List.length rules_list1 <> List.length rules_list2
  in
  let all_rules1 = List.concat rules_list1 in
  let all_rules2 = List.concat rules_list2 in
  let added_r, removed_r, modified_r, regrouped_r =
    rule_diffs all_rules1 all_rules2
  in
  let has_immediate =
    added_r <> [] || removed_r <> [] || modified_r <> [] || regrouped_r <> []
  in
  let has_nested = nested_differences all_rules1 all_rules2 <> [] in
  if has_immediate || has_nested || block_count_differs then
    Some (all_rules1, all_rules2)
  else None

and media_diff items1 items2 =
  let group items = Group.by Fun.id items in
  let groups1 = group items1 in
  let groups2 = group items2 in
  let added = ref [] in
  let removed = ref [] in
  let modified = ref [] in
  Hashtbl.iter
    (fun cond rules_list1 ->
      match Hashtbl.find_opt groups2 cond with
      | None ->
          List.iter
            (fun rules -> removed := (cond, rules) :: !removed)
            rules_list1
      | Some rules_list2 -> (
          match media_condition_differs rules_list1 rules_list2 with
          | Some (r1, r2) -> modified := (cond, r1, r2) :: !modified
          | None -> ()))
    groups1;
  Hashtbl.iter
    (fun cond rules_list2 ->
      if not (Hashtbl.mem groups1 cond) then
        List.iter (fun rules -> added := (cond, rules) :: !added) rules_list2)
    groups2;
  (!added, !removed, !modified)

and process_modified_container ~container_type ~condition_of ~moved_conds
    ~stmts1 ~stmts2 ~block_structure_changed ~reported cond rules1 rules2 =
  (* Skip if this condition has a block structure change *)
  if Hashtbl.mem block_structure_changed cond then None
  else
    let rule_changes = to_rule_changes rules1 rules2 in
    (* Recursively check deeper nesting *)
    let nested_containers = nested_differences rules1 rules2 in
    Hashtbl.replace reported cond ();
    if rule_changes <> [] || nested_containers <> [] then
      (* Container was modified in content, not just position *)
      Some
        (modified_container container_type cond rules1 rules2 rule_changes
           nested_containers)
    else if Hashtbl.mem moved_conds cond then
      container_moved ~container_type ~condition_of ~stmts1 ~stmts2 cond rules1
    else None

(* What one occurrence of a condition on the expected side contributes. A
   removal comes first and can repeat, since two blocks of one condition may
   both be gone; everything else is answered once per condition, [settled]
   recording which, so a later occurrence does not restate it. *)
and expected_container_entry ~container_type ~condition_of ~moved_conds ~stmts1
    ~stmts2 ~block_structure_changed ~pending_removed ~pending_modified ~settled
    ~reported (_pos, cond, block_rules) =
  match take_bucket pending_removed cond with
  (* The bucket says a block of this condition is gone; which one is this
     occurrence, whose own rules are the ones to report. Taking the bucket's
     copy would pair the first occurrence with whichever block the diff happened
     to name first. *)
  | Some _ ->
      Hashtbl.replace reported cond ();
      Some (Removed { container_type; condition = cond; rules = block_rules })
  | None when Hashtbl.mem settled cond -> None
  | None -> (
      Hashtbl.replace settled cond ();
      match Hashtbl.find_opt block_structure_changed cond with
      | Some (expected_blocks, actual_blocks) ->
          Hashtbl.replace reported cond ();
          Some
            (Block_structure_changed
               {
                 container_type;
                 condition = cond;
                 expected_blocks;
                 actual_blocks;
               })
      | None -> (
          match take_bucket pending_modified cond with
          | Some (rules1, rules2) ->
              process_modified_container ~container_type ~condition_of
                ~moved_conds ~stmts1 ~stmts2 ~block_structure_changed ~reported
                cond rules1 rules2
          | None when Hashtbl.mem moved_conds cond ->
              Hashtbl.replace reported cond ();
              container_moved ~container_type ~condition_of ~stmts1 ~stmts2 cond
                block_rules
          | None -> None))

and process_nested_containers ~container_type ~extract_fn ~diff_fn stmts1 stmts2
    =
  let condition_of stmt = Option.map fst (extract_fn stmt) in
  let items_with_pos1 = extract_items_with_positions extract_fn stmts1 in
  let items_with_pos2 = extract_items_with_positions extract_fn stmts2 in
  let block_structure_changed =
    detect_block_structure_changes
      (group_by_condition items_with_pos1)
      (group_by_condition items_with_pos2)
  in
  let moved_conds = moved_conditions ~condition_of stmts1 stmts2 in
  let reported = Hashtbl.create 8 in
  let items1 = List.filter_map extract_fn stmts1 in
  let items2 = List.filter_map extract_fn stmts2 in
  let added, removed, modified = diff_fn items1 items2 in
  let pending_removed = Group.by Fun.id removed in
  let pending_added = Group.by Fun.id added in
  let pending_modified =
    Group.by (fun (cond, rules1, rules2) -> (cond, (rules1, rules2))) modified
  in
  let settled = Hashtbl.create 8 in
  let expected_diffs =
    List.filter_map
      (expected_container_entry ~container_type ~condition_of ~moved_conds
         ~stmts1 ~stmts2 ~block_structure_changed ~pending_removed
         ~pending_modified ~settled ~reported)
      items_with_pos1
  in
  let added_diffs =
    List.filter_map
      (fun (_pos, cond, _rules) ->
        Option.map
          (fun rules ->
            Hashtbl.replace reported cond ();
            Added { container_type; condition = cond; rules })
          (take_bucket pending_added cond))
      items_with_pos2
  in
  let diffs = ref (expected_diffs @ added_diffs) in
  diffs :=
    !diffs
    @ container_reorders ~container_type ~condition_of ~moved_conds ~reported
        ~stmts1 ~stmts2 items1;
  (if !diffs = [] then
     match
       detect_order_only_change ~container_type added removed items1 items2
     with
     | Some d -> diffs := [ d ]
     | None -> ());
  !diffs

(* Layer diff function *)
and layer_diff items1 items2 =
  let key_of (name_opt, _) = layer_key name_opt in
  let is_empty_diff (_, rules1) (_, rules2) =
    let a_r, r_r, m_r, rg_r = rule_diffs rules1 rules2 in
    let has_immediate_diffs =
      a_r <> [] || r_r <> [] || m_r <> [] || rg_r <> []
    in
    if has_immediate_diffs then false
    else
      (* Also check for nested differences *)
      let nested_diffs = nested_differences rules1 rules2 in
      nested_diffs = []
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~is_empty_diff items1 items2
  in
  (* Transform to consistent format with media_diff *)
  let added =
    List.map (fun (name_opt, rules) -> (layer_key name_opt, rules)) added
  in
  let removed =
    List.map (fun (name_opt, rules) -> (layer_key name_opt, rules)) removed
  in
  let modified =
    List.map
      (fun ((name_opt, rules1), (_, rules2)) ->
        (layer_key name_opt, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Shared helper: collect added/removed container diffs and process modified
   containers with the standard rule-change + nesting logic. [extract_fn] names
   the containers in the enclosing statement list, which is what decides whether
   one of them moved. *)
and collect_container_diffs ~container_type ~extract_fn ~stmts1 ~stmts2 added
    removed modified =
  let condition_of stmt = Option.map fst (extract_fn stmt) in
  let moved_conds = moved_conditions ~condition_of stmts1 stmts2 in
  let reported = Hashtbl.create 8 in
  let diffs = ref [] in
  List.iter
    (fun (condition, rules) ->
      Hashtbl.replace reported condition ();
      diffs := Added { container_type; condition; rules } :: !diffs)
    added;
  List.iter
    (fun (condition, rules) ->
      Hashtbl.replace reported condition ();
      diffs := Removed { container_type; condition; rules } :: !diffs)
    removed;
  List.iter
    (fun (condition, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      let nested_containers = nested_differences rules1 rules2 in
      Hashtbl.replace reported condition ();
      if rule_changes <> [] || nested_containers <> [] then
        diffs :=
          Modified
            {
              info = { container_type; condition; rules = rules1 };
              actual_rules = rules2;
              rule_changes;
              container_changes = nested_containers;
            }
          :: !diffs
      else if Hashtbl.mem moved_conds condition then
        match
          container_moved ~container_type ~condition_of ~stmts1 ~stmts2
            condition rules1
        with
        | Some diff -> diffs := diff :: !diffs
        | None -> ())
    modified;
  container_reorders ~container_type ~condition_of ~moved_conds ~reported
    ~stmts1 ~stmts2
    (List.filter_map extract_fn stmts1)
  @ !diffs

(* Process layers separately due to different type signature *)
and process_nested_layers stmts1 stmts2 =
  let items1 = List.filter_map Css.as_layer stmts1 in
  let items2 = List.filter_map Css.as_layer stmts2 in
  let added, removed, modified = layer_diff items1 items2 in
  collect_container_diffs ~container_type:`Layer ~extract_fn:extract_layer_name
    ~stmts1 ~stmts2 added removed modified

and container_has_no_diff (_, _, rules1) (_, _, rules2) =
  let a_r, r_r, m_r, rg_r = rule_diffs rules1 rules2 in
  let has_immediate_diffs = a_r <> [] || r_r <> [] || m_r <> [] || rg_r <> [] in
  if has_immediate_diffs then false else nested_differences rules1 rules2 = []

(* Container diff function for @container rules *)
and container_diff items1 items2 =
  let added, removed, modified_pairs =
    diffs ~key_of:container_key ~is_empty_diff:container_has_no_diff items1
      items2
  in
  (* Transform to consistent format with media_diff. *)
  let added = List.map condition_rules_of_container added in
  let removed = List.map condition_rules_of_container removed in
  let modified = List.map modified_container_of_pair modified_pairs in
  (added, removed, modified)

(* Process container rules *)
and process_nested_containers_with_name stmts1 stmts2 =
  let items1 = List.filter_map Css.as_container stmts1 in
  let items2 = List.filter_map Css.as_container stmts2 in
  let added, removed, modified = container_diff items1 items2 in
  collect_container_diffs ~container_type:`Container
    ~extract_fn:extract_container_as_string ~stmts1 ~stmts2 added removed
    modified

(* Process CSS nesting: rules with nested child rules (& .foo { ... }) *)
and process_nested_rules stmts1 stmts2 =
  let items1 = rule_nesting_items stmts1 in
  let items2 = rule_nesting_items stmts2 in
  (* [List.find_opt] previously searched all of [items2] for every rule in
     [items1], including the overwhelmingly common rules with no nested body.
     Keep its first-occurrence semantics for repeated selectors, but index that
     first occurrence once. *)
  let items2_by_selector = Hashtbl.create (List.length items2) in
  List.iter
    (fun (key, _label, nested) ->
      if not (Hashtbl.mem items2_by_selector key) then
        Hashtbl.add items2_by_selector key nested)
    items2;
  (* Match by selector key and diff nested statements *)
  let diffs = ref [] in
  List.iter
    (fun (key1, label1, nested1) ->
      match Hashtbl.find_opt items2_by_selector key1 with
      | Some nested2 when not (Stylesheet.equal nested1 nested2) ->
          let rule_changes = to_rule_changes nested1 nested2 in
          let nested_containers = nested_differences nested1 nested2 in
          if rule_changes <> [] || nested_containers <> [] then
            diffs :=
              Modified
                {
                  info =
                    {
                      container_type = `Nesting;
                      condition = label1;
                      rules = nested1;
                    };
                  actual_rules = nested2;
                  rule_changes;
                  container_changes = nested_containers;
                }
              :: !diffs
      | Some _ -> () (* Same nesting *)
      | None -> ())
    items1;
  !diffs

(* Compare one pair of at-rule blocks that occupy the same position under the
   same head. *)
and at_rule_pair_diff (head, body1, text1) (_, body2, text2) =
  let modified rules actual_rules rule_changes container_changes =
    Modified
      {
        info = { container_type = `At_rule; condition = head; rules };
        actual_rules;
        rule_changes;
        container_changes;
      }
  in
  match (body1, body2) with
  | Block block1, Block block2 ->
      let rule_changes = to_rule_changes block1 block2 in
      let container_changes = nested_differences block1 block2 in
      if rule_changes = [] && container_changes = [] then None
      else Some (modified block1 block2 rule_changes container_changes)
  | Declarations decls1, Declarations decls2 ->
      Option.map
        (fun change -> modified [] [] [ change ] [])
        (at_rule_declarations_change decls1 decls2)
  | Opaque, Opaque when text1 <> text2 ->
      Some (modified [] [] [ at_rule_text_change text1 text2 ] [])
  | Block _, _ | Declarations _, _ | Opaque, _ -> None

and process_at_rules stmts1 stmts2 =
  let items1 = at_rule_items stmts1 and items2 = at_rule_items stmts2 in
  let added, removed, pairs =
    diffs ~key_of:fst ~is_empty_diff:(fun _ _ -> false) items1 items2
  in
  let block_rules = function Block block -> block | _ -> [] in
  let info (head, body, _) =
    { container_type = `At_rule; condition = head; rules = block_rules body }
  in
  List.map (fun (_, item) -> Added (info item)) added
  @ List.map (fun (_, item) -> Removed (info item)) removed
  @ List.filter_map
      (fun ((_, item1), (_, item2)) -> at_rule_pair_diff item1 item2)
      pairs

(* Main recursive function for nested differences *)
(* Every branch below recurses on the statements of a block, which is a strictly
   smaller list than the one holding it, so the walk terminates on the depth of
   the stylesheet. A cutoff here is a cutoff of the answer: the detection
   helpers ([media_condition_differs], [layer_diff]'s [is_empty_diff],
   [container_has_no_diff]) call back in to decide whether a container differs
   at all, so a container past the cutoff was reported as identical, verdict and
   exit code included. *)
and nested_differences (stmts1 : Css.statement list)
    (stmts2 : Css.statement list) : container_diff list =
  (* Process CSS nesting (& .foo { ... } inside rules) *)
  process_nested_rules stmts1 stmts2
  (* Process media queries *)
  @ process_nested_containers ~container_type:`Media
      ~extract_fn:extract_media_as_string ~diff_fn:media_diff stmts1 stmts2
  (* Process layers - different type signature *)
  @ process_nested_layers stmts1 stmts2
  (* Process supports - reuses media_diff since they have the same structure *)
  @ process_nested_containers ~container_type:`Supports
      ~extract_fn:extract_supports_as_string ~diff_fn:media_diff stmts1 stmts2
  (* Process container queries *)
  @ process_nested_containers_with_name stmts1 stmts2
  (* Process property declarations *)
  @ process_nested_properties stmts1 stmts2
  (* Process keyframes animations *)
  @ process_nested_keyframes stmts1 stmts2
  (* Process the at-rules that carry no selector of their own *)
  @ process_at_rules stmts1 stmts2

(* Main diff function *)
(* @import and the other selectorless leaf rules collapse onto the universal
   selector key in [rule_diffs], so two distinct imports match as identical and
   their differences vanish. Compare them here on their serialised form, which
   captures the target URL, layer, supports condition and media query. Import
   order is cascade-significant, so a pure reorder is a difference too. *)
let import_strings stmts =
  List.filter_map
    (fun s ->
      match Css.as_import s with
      | Some _ ->
          Some
            (Css.Stylesheet.to_string ~minify:true (Css.v [ s ]) |> String.trim)
      | None -> None)
    stmts

(* [items] minus one occurrence for each element of [remove]. *)
let multiset_remove_each ~remove items =
  let counts = Hashtbl.create 16 in
  List.iter
    (fun x ->
      Hashtbl.replace counts x
        (1 + try Hashtbl.find counts x with Not_found -> 0))
    remove;
  List.filter
    (fun x ->
      match Hashtbl.find_opt counts x with
      | Some n when n > 0 ->
          Hashtbl.replace counts x (n - 1);
          false
      | _ -> true)
    items

(* Precondition: [l1] and [l2] hold the same imports in a different order. *)
let import_reorder l1 l2 : rule_diff option =
  let arr2 = Array.of_list l2 in
  let index_in_l2 s =
    let rec idx j =
      if j >= Array.length arr2 then 0
      else if arr2.(j) = s then j
      else idx (j + 1)
    in
    idx 0
  in
  let rec first_moved i = function
    | x :: rest ->
        if i < Array.length arr2 && arr2.(i) = x then first_moved (i + 1) rest
        else Some (i, x)
    | [] -> None
  in
  match first_moved 0 l1 with
  | None -> None
  | Some (expected_pos, moved) ->
      Some
        (Reordered
           {
             selector = moved;
             expected_pos;
             actual_pos = index_in_l2 moved;
             swapped_with = None;
             old_declarations = None;
             new_declarations = None;
           })

let process_imports stmts1 stmts2 : rule_diff list =
  let l1 = import_strings stmts1 and l2 = import_strings stmts2 in
  if l1 = l2 then []
  else if List.sort compare l1 = List.sort compare l2 then
    Option.to_list (import_reorder l1 l2)
  else
    List.map
      (fun s -> (Removed { selector = s; declarations = [] } : rule_diff))
      (multiset_remove_each ~remove:l2 l1)
    @ List.map
        (fun s -> (Added { selector = s; declarations = [] } : rule_diff))
        (multiset_remove_each ~remove:l1 l2)

(* Cascade layer order. Two sheets can hold the same [@layer] blocks with the
   same bodies and still resolve a conflict between two layers the opposite way,
   because a layer's strength comes from where its name is first declared, not
   from where its rules stand: an [@layer a;] statement ahead of the blocks pins
   [a] as the weaker layer wherever its block ends up. Nothing else in the walk
   reads that, so compare the declared orders here. *)

(* A layer only one side declares is the rule and container walk's business: it
   reports the [@layer] block that came or went. What only the order shows is a
   pair of layers both sides declare in the opposite relative order, so restrict
   each order to the shared names before comparing. *)
let shared_layer_order order other =
  List.filter (fun name -> List.exists (String.equal name) other) order

(* The pairs [(weaker, stronger)] that [expected] declares weaker-then-stronger
   and [actual] the other way round. Both lists hold the same names, so a
   position lookup in [actual] settles each pair. *)
let swapped_layer_pairs ~expected ~actual =
  let positions = Hashtbl.create 16 in
  List.iteri (fun i name -> Hashtbl.replace positions name i) actual;
  let position name =
    match Hashtbl.find_opt positions name with Some i -> i | None -> -1
  in
  let rec pairs = function
    | [] -> []
    | earlier :: rest ->
        List.filter_map
          (fun later ->
            if position later < position earlier then Some (earlier, later)
            else None)
          rest
        @ pairs rest
  in
  pairs expected

(* [Resolve.layer_order] keys a sheet's layers by dotted path, so the two orders
   compare across spellings: [@layer a.b] and [@layer a { @layer b }] reach the
   same path, and an [@layer a, b;] statement declares its names the same way a
   block does. *)
let layer_order_diff stmts1 stmts2 =
  let order1 = Resolve.layer_order stmts1 in
  let order2 = Resolve.layer_order stmts2 in
  let expected_order = shared_layer_order order1 order2 in
  let actual_order = shared_layer_order order2 order1 in
  if List.equal String.equal expected_order actual_order then None
  else
    Some
      {
        expected_order;
        actual_order;
        swapped =
          swapped_layer_pairs ~expected:expected_order ~actual:actual_order;
      }
