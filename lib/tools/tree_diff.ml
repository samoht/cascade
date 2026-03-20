(** CSS tree difference analysis for structural comparison. *)

open Cascade

(* ===== Type Definitions ===== *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}

type rule_diff =
  | Rule_added of { selector : string; declarations : Css.declaration list }
  | Rule_removed of { selector : string; declarations : Css.declaration list }
  | Rule_content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
      added_properties : string list;
      removed_properties : string list;
    }
  | Rule_selector_changed of {
      old_selector : string;
      new_selector : string;
      declarations : Css.declaration list;
    }
  | Rule_reordered of {
      selector : string;
      expected_pos : int;
      actual_pos : int;
      swapped_with : string option; (* Selector that moved to old position *)
      (* When only declarations order changed within the rule, we carry the
         before/after declarations to pretty-print a property reorder
         summary. *)
      old_declarations : Css.declaration list option;
      new_declarations : Css.declaration list option;
    }

type container_info = {
  container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
  condition : string;
  rules : Css.statement list;
}

type container_diff =
  | Container_added of container_info
  | Container_removed of container_info
  | Container_modified of {
      info : container_info; (* expected *)
      actual_rules : Css.statement list; (* actual *)
      rule_changes : rule_diff list;
      container_changes : container_diff list; (* Nested container changes *)
    }
  | Container_reordered of {
      info : container_info;
      expected_pos : int;
      actual_pos : int;
    }
  | Container_block_structure_changed of {
      container_type : [ `Media | `Layer | `Supports | `Container | `Property ];
      condition : string;
      expected_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in expected *)
      actual_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in actual *)
    }

type t = { rules : rule_diff list; containers : container_diff list }

(* ===== Constants ===== *)

let default_truncation_length = String_diff.default_max_width

(* ===== Helper Functions ===== *)

let is_empty d = d.rules = [] && d.containers = []

(* ===== Pretty Printing Functions ===== *)

(* Tree-style formatting helpers *)
type tree_style = {
  use_tree : bool; (* Whether to use tree-style box-drawing characters *)
}

let default_style = { use_tree = false }
let tree_style = { use_tree = true }

(* Get Fmt style for change type - using git diff colors *)
let fmt_style action =
  match action with
  | "add" ->
      (* Green foreground *)
      Fmt.(styled (`Fg `Green) string)
  | "remove" ->
      (* Red foreground *)
      Fmt.(styled (`Fg `Red) string)
  | _ -> Fmt.nop

(* Get the appropriate prefix for tree-style formatting *)
let tree_prefix ~style ~is_last ~parent_prefix =
  if not style.use_tree then ""
  else
    let connector = if is_last then "└─ " else "├─ " in
    parent_prefix ^ connector

(* Get the continuation prefix for children *)
let tree_continuation ~style ~is_last ~parent_prefix =
  if not style.use_tree then parent_prefix
  else
    let continuation = if is_last then "   " else "│  " in
    parent_prefix ^ continuation

(* Print a list of CSS declarations with an action prefix *)
let pp_declarations ?(style = default_style) ?(parent_prefix = "") fmt action
    decls =
  let prefix_symbol =
    match action with
    | "add" -> "+"
    | "remove" -> "-"
    | _ -> action (* fallback for other actions like "declarations" *)
  in
  let styled = fmt_style action in
  (* Properties don't get tree connectors - just indentation continuation *)
  let indent =
    if style.use_tree then parent_prefix ^ "   " else parent_prefix ^ "    "
  in
  List.iter
    (fun decl ->
      let prop_name = Css.declaration_name decl in
      (* Use non-minified values to preserve unit differences like 0px vs 0 *)
      let prop_value = Css.declaration_value ~minify:false decl in
      let truncated_value =
        String_diff.truncate_middle default_truncation_length prop_value
      in
      Fmt.pf fmt "%s%a@," indent styled
        (Fmt.str "%s %s %s" prefix_symbol prop_name truncated_value))
    decls

let pp_property_diff ?(style = default_style) ?(parent_prefix = "") fmt
    { property_name; expected_value; actual_value } =
  let indent =
    if style.use_tree then parent_prefix ^ "   " else parent_prefix ^ "    "
  in
  let red_styled = fmt_style "remove" in
  let green_styled = fmt_style "add" in
  match String_diff.first_diff_pos expected_value actual_value with
  | None ->
      (* Shouldn't happen but handle gracefully *)
      Fmt.pf fmt "%s* %s: (no diff detected)@," indent property_name
  | Some _ ->
      let len1 = String.length expected_value in
      let len2 = String.length actual_value in
      if len1 <= 30 && len2 <= 30 then
        (* Short values: show inline with red for old, green for new *)
        Fmt.pf fmt "%s* %s: %a -> %a@," indent property_name red_styled
          expected_value green_styled actual_value
      else
        (* Long values: truncate and show as separate lines *)
        let exp_truncated =
          String_diff.truncate_middle default_truncation_length expected_value
        in
        let act_truncated =
          String_diff.truncate_middle default_truncation_length actual_value
        in
        Fmt.pf fmt "%s* %s:@," indent property_name;
        Fmt.pf fmt "%s  %a@," indent red_styled (Fmt.str "- %s" exp_truncated);
        Fmt.pf fmt "%s  %a@," indent green_styled (Fmt.str "+ %s" act_truncated)

let pp_property_diffs ?(style = default_style) ?(parent_prefix = "") fmt
    prop_diffs =
  List.iter (pp_property_diff ~style ~parent_prefix fmt) prop_diffs

(* Helper to find adjacent property swap *)
let adjacent_swap lst1 lst2 =
  let rec scan l1 l2 =
    match (l1, l2) with
    | x1 :: x2 :: _, y1 :: y2 :: _ when x1 = y2 && x2 = y1 -> Some (x1, x2)
    | _ :: rest1, _ :: rest2 -> scan rest1 rest2
    | _, _ -> None
  in
  scan lst1 lst2

(* Helper to find property moves (up to max_count) *)
let property_moves ~max_count prop_names1 prop_names2 =
  let rec scan lst1 lst2 acc count =
    if count >= max_count then List.rev acc
    else
      match (lst1, lst2) with
      | x1 :: rest1, x2 :: rest2 when x1 <> x2 ->
          let new_pos =
            let rec find_idx i = function
              | [] -> -1
              | x :: _ when x = x1 -> i
              | _ :: rest -> find_idx (i + 1) rest
            in
            find_idx 0 prop_names2
          in
          scan rest1 rest2 ((x1, new_pos) :: acc) (count + 1)
      | _ :: rest1, _ :: rest2 -> scan rest1 rest2 acc count
      | _, _ -> List.rev acc
  in
  scan prop_names1 prop_names2 [] 0

(* Helper to print property moves *)
let pp_property_moves fmt indent moves total_diffs =
  Fmt.pf fmt "%s* reorder: " indent;
  List.iteri
    (fun i (prop, new_pos) ->
      if i > 0 then Fmt.pf fmt ", ";
      if new_pos >= 0 then Fmt.pf fmt "%s→%d" prop new_pos
      else Fmt.pf fmt "%s" prop)
    moves;
  if total_diffs > List.length moves then
    Fmt.pf fmt " (and %d more)" (total_diffs - List.length moves);
  Fmt.pf fmt "@,"

let pp_reorder ?(style = default_style) ?(parent_prefix = "") decls1 decls2 fmt
    =
  let indent =
    if style.use_tree then parent_prefix ^ "   " else parent_prefix ^ "    "
  in
  let prop_names1 = List.map Css.declaration_name decls1 in
  let prop_names2 = List.map Css.declaration_name decls2 in
  let same_props =
    List.length prop_names1 = List.length prop_names2
    && List.sort String.compare prop_names1
       = List.sort String.compare prop_names2
  in
  if same_props && prop_names1 <> prop_names2 then
    match adjacent_swap prop_names1 prop_names2 with
    | Some (prop1, prop2) ->
        let truncate s = String_diff.truncate_middle 20 s in
        Fmt.pf fmt "%s* %s ↔ %s@," indent (truncate prop1) (truncate prop2)
    | None ->
        let moves = property_moves ~max_count:3 prop_names1 prop_names2 in
        if moves <> [] then
          let total_diffs =
            List.fold_left2
              (fun acc p1 p2 -> if p1 <> p2 then acc + 1 else acc)
              0 prop_names1 prop_names2
          in
          pp_property_moves fmt indent moves total_diffs

let pp_content_changed ~style ~prefix ~child_prefix fmt ~selector
    ~old_declarations ~new_declarations ~property_changes ~added_properties
    ~removed_properties =
  let indent =
    if style.use_tree then child_prefix ^ "   " else child_prefix ^ "    "
  in
  let green_styled = fmt_style "add" in
  let red_styled = fmt_style "remove" in
  let has_any_changes =
    property_changes <> [] || added_properties <> [] || removed_properties <> []
  in
  if (not has_any_changes) && old_declarations = new_declarations then ()
  else (
    Fmt.pf fmt "%s%s@," prefix selector;
    List.iter
      (fun prop_name ->
        Fmt.pf fmt "%s%a@," indent red_styled (Fmt.str "- %s" prop_name))
      removed_properties;
    List.iter
      (fun prop_name ->
        Fmt.pf fmt "%s%a@," indent green_styled (Fmt.str "+ %s" prop_name))
      added_properties;
    pp_property_diffs ~style ~parent_prefix:child_prefix fmt property_changes;
    pp_reorder ~style ~parent_prefix:child_prefix old_declarations
      new_declarations fmt;
    if (not has_any_changes) && old_declarations <> new_declarations then
      let old_count = List.length old_declarations in
      let new_count = List.length new_declarations in
      if old_count <> new_count then
        Fmt.pf fmt "%s(declaration count: %d -> %d)@," indent old_count
          new_count
      else Fmt.pf fmt "%s(declarations differ in subtle ways)@," indent)

let pp_position_reorder ~prefix fmt ~selector ~expected_pos ~actual_pos
    ~swapped_with =
  assert (expected_pos <> actual_pos);
  let truncate s = String_diff.truncate_middle 40 s in
  match swapped_with with
  | Some other when abs (expected_pos - actual_pos) = 1 ->
      Fmt.pf fmt "%s%s ↔  %s@," prefix (truncate selector) (truncate other)
  | Some other ->
      Fmt.pf fmt "%s%s (position %d) ↔  %s (position %d)@," prefix
        (truncate selector) actual_pos (truncate other) expected_pos
  | None ->
      Fmt.pf fmt "%s%s (position %d → %d)@," prefix (truncate selector)
        expected_pos actual_pos

let pp_rule_diff ?(style = default_style) ?(is_last = false)
    ?(parent_prefix = "") fmt = function
  | Rule_added { selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      Fmt.pf fmt "%s%s@," prefix selector;
      pp_declarations ~style ~parent_prefix:child_prefix fmt "add" declarations
  | Rule_removed { selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      Fmt.pf fmt "%s%s@," prefix selector;
      pp_declarations ~style ~parent_prefix:child_prefix fmt "remove"
        declarations
  | Rule_content_changed r ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      pp_content_changed ~style ~prefix ~child_prefix fmt ~selector:r.selector
        ~old_declarations:r.old_declarations
        ~new_declarations:r.new_declarations
        ~property_changes:r.property_changes
        ~added_properties:r.added_properties
        ~removed_properties:r.removed_properties
  | Rule_selector_changed { old_selector; new_selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      Fmt.pf fmt "%sselector changed:@," prefix;
      let indent =
        if style.use_tree then child_prefix ^ "   " else child_prefix ^ "    "
      in
      Fmt.pf fmt "%sfrom: %s@," indent old_selector;
      Fmt.pf fmt "%sto:   %s@," indent new_selector;
      if declarations <> [] then
        pp_declarations ~style ~parent_prefix:child_prefix fmt "declarations"
          declarations
  | Rule_reordered r -> (
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      match (r.old_declarations, r.new_declarations) with
      | Some old_decls, Some new_decls ->
          let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
          Fmt.pf fmt "%s%s@," prefix r.selector;
          pp_reorder ~style ~parent_prefix:child_prefix old_decls new_decls fmt
      | _ ->
          pp_position_reorder ~prefix fmt ~selector:r.selector
            ~expected_pos:r.expected_pos ~actual_pos:r.actual_pos
            ~swapped_with:r.swapped_with)

let pp_rule_diff_simple fmt = function
  | Rule_added { selector; _ } -> Fmt.pf fmt "Added(%s)" selector
  | Rule_removed { selector; _ } -> Fmt.pf fmt "Removed(%s)" selector
  | Rule_content_changed { selector; _ } -> Fmt.pf fmt "Changed(%s)" selector
  | Rule_selector_changed { old_selector; new_selector; _ } ->
      Fmt.pf fmt "SelectorChanged(%s->%s)" old_selector new_selector
  | Rule_reordered { selector; expected_pos; actual_pos; _ } ->
      Fmt.pf fmt "Reordered(%s:%d->%d)" selector expected_pos actual_pos

let meaningful_rules rules =
  List.filter
    (function
      | Rule_reordered _ -> false
      | Rule_content_changed
          {
            property_changes = [];
            added_properties = [];
            removed_properties = [];
            old_declarations;
            new_declarations;
            _;
          }
        when old_declarations = new_declarations ->
          (* Filter out rules that moved to different nesting but have no
             changes *)
          false
      | _ -> true)
    rules

(** Query functions *)
let single_rule_diff (diff : t) =
  match diff.rules with [ rule ] -> Some rule | _ -> None

let rec count_containers_in_list container_type containers =
  List.fold_left
    (fun count cont ->
      let this_count =
        match cont with
        | Container_added { container_type = ct; _ }
        | Container_removed { container_type = ct; _ }
        | Container_reordered { info = { container_type = ct; _ }; _ }
        | Container_block_structure_changed { container_type = ct; _ } ->
            if ct = container_type then 1 else 0
        | Container_modified
            { info = { container_type = ct; _ }; container_changes; _ } ->
            let nested_count =
              count_containers_in_list container_type container_changes
            in
            (if ct = container_type then 1 else 0) + nested_count
      in
      count + this_count)
    0 containers

let count_containers_by_type container_type (diff : t) =
  count_containers_in_list container_type diff.containers

let has_container_added_of_type container_type (diff : t) =
  List.exists
    (function
      | Container_added { container_type = ct; _ } -> ct = container_type
      | _ -> false)
    diff.containers

let has_container_removed_of_type container_type (diff : t) =
  List.exists
    (function
      | Container_removed { container_type = ct; _ } -> ct = container_type
      | _ -> false)
    diff.containers

let container_prefix = function
  | `Media -> "@media"
  | `Layer -> "@layer"
  | `Supports -> "@supports"
  | `Container -> "@container"
  | `Property -> "@property"

let pp_container_rules ~style ~parent_prefix ~label fmt rules =
  if rules <> [] then
    let rule_count = List.length rules in
    List.iteri
      (fun i stmt ->
        match Css.as_rule stmt with
        | Some (selector, _, _) ->
            let rule_prefix =
              tree_prefix ~style ~is_last:(i = rule_count - 1) ~parent_prefix
            in
            Fmt.pf fmt "%s%s (%s)@," rule_prefix
              (Css.Selector.to_string selector)
              label
        | None -> ())
      rules

let count_rule_changes rule_changes =
  let count pred = List.length (List.filter pred rule_changes) in
  let parts =
    List.filter_map Fun.id
      [
        (let n = count (function Rule_added _ -> true | _ -> false) in
         if n > 0 then Some (Fmt.str "%d added" n) else None);
        (let n = count (function Rule_removed _ -> true | _ -> false) in
         if n > 0 then Some (Fmt.str "%d removed" n) else None);
        (let n =
           count (function Rule_content_changed _ -> true | _ -> false)
         in
         if n > 0 then Some (Fmt.str "%d modified" n) else None);
        (let n = count (function Rule_reordered _ -> true | _ -> false) in
         if n > 0 then Some (Fmt.str "%d reordered" n) else None);
        (let n =
           count (function Rule_selector_changed _ -> true | _ -> false)
         in
         if n > 0 then Some (Fmt.str "%d selector changed" n) else None);
      ]
  in
  parts

let selectors_of_rules rules =
  List.filter_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (sel, _, _) -> Some (Css.Selector.to_string sel)
      | None -> None)
    rules

let pp_block_structure_changed ~style ~is_last ~parent_prefix fmt
    ~container_type ~condition ~expected_blocks ~actual_blocks =
  let cont_prefix = container_prefix container_type in
  let prefix = tree_prefix ~style ~is_last ~parent_prefix in
  let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
  let indent =
    if style.use_tree then child_prefix ^ "   " else child_prefix ^ "    "
  in
  let red_styled = fmt_style "remove" in
  let green_styled = fmt_style "add" in

  (* Show the merge/split summary *)
  let exp_count = List.length expected_blocks in
  let act_count = List.length actual_blocks in

  (* Report block structure changes - this is a meaningful difference even if
     selectors are identical *)
  if exp_count > act_count then
    Fmt.pf fmt "%s%s %s (%d blocks merged into %d)@," prefix cont_prefix
      condition exp_count act_count
  else if exp_count < act_count then
    Fmt.pf fmt "%s%s %s (%d block split into %d)@," prefix cont_prefix condition
      exp_count act_count
  else
    (* Same count but different positions *)
    Fmt.pf fmt "%s%s %s (%d blocks at different positions)@," prefix cont_prefix
      condition exp_count;

  let pp_blocks sign styled blocks =
    List.iter
      (fun (pos, rules) ->
        let selectors = selectors_of_rules rules in
        if selectors <> [] then
          Fmt.pf fmt "%s%a@," indent styled
            (Fmt.str "%s Block at position %d: %s" sign pos
               (String.concat ", " selectors)))
      blocks
  in
  pp_blocks "-" red_styled expected_blocks;
  pp_blocks "+" green_styled actual_blocks

let pp_container_add_remove ~style ~is_last ~parent_prefix ~label fmt
    container_type condition rules =
  let prefix = tree_prefix ~style ~is_last ~parent_prefix in
  let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
  Fmt.pf fmt "%s%s %s (%s)@," prefix
    (container_prefix container_type)
    condition label;
  pp_container_rules ~style ~parent_prefix:child_prefix ~label fmt rules

let rec pp_container_diff ?(style = default_style) ?(is_last = false)
    ?(parent_prefix = "") fmt = function
  | Container_added { container_type; condition; rules } ->
      pp_container_add_remove ~style ~is_last ~parent_prefix ~label:"added" fmt
        container_type condition rules
  | Container_removed { container_type; condition; rules } ->
      pp_container_add_remove ~style ~is_last ~parent_prefix ~label:"removed"
        fmt container_type condition rules
  | Container_modified
      {
        info = { container_type; condition; rules = _ };
        actual_rules = _;
        rule_changes;
        container_changes;
      } ->
      let cont_prefix = container_prefix container_type in
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      let changes_parts = count_rule_changes rule_changes in
      Fmt.pf fmt "%s%s %s " prefix cont_prefix condition;
      if changes_parts <> [] then
        Fmt.pf fmt "(%s)@," (String.concat ", " changes_parts)
      else if container_changes = [] then Fmt.pf fmt "(position changed)@,"
      else Fmt.pf fmt "@,";

      (* Show rule changes at this level *)
      List.iteri
        (fun i rule_diff ->
          let is_last_item =
            i = List.length rule_changes - 1 && container_changes = []
          in
          pp_rule_diff ~style ~is_last:is_last_item ~parent_prefix:child_prefix
            fmt rule_diff)
        rule_changes;
      (* Show nested container changes with increased indentation *)
      let container_count = List.length container_changes in
      List.iteri
        (fun i cont_diff ->
          let is_last_cont = i = container_count - 1 in
          pp_container_diff ~style ~is_last:is_last_cont
            ~parent_prefix:child_prefix fmt cont_diff)
        container_changes
  | Container_reordered
      { info = { container_type; condition; _ }; expected_pos; actual_pos } ->
      let cont_prefix = container_prefix container_type in
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      Fmt.pf fmt "%s%s %s (position %d → %d)@," prefix cont_prefix condition
        expected_pos actual_pos
  | Container_block_structure_changed
      { container_type; condition; expected_blocks; actual_blocks } ->
      pp_block_structure_changed ~style ~is_last ~parent_prefix fmt
        ~container_type ~condition ~expected_blocks ~actual_blocks

let pp_diff_headers fmt expected actual =
  Fmt.pf fmt "%a %a@."
    Fmt.(styled (`Fg `Yellow) string)
    "---"
    Fmt.(styled (`Fg `Yellow) string)
    expected;
  Fmt.pf fmt "%a %a@."
    Fmt.(styled (`Fg `Yellow) string)
    "+++"
    Fmt.(styled (`Fg `Yellow) string)
    actual

let pp_rule_list ~style ~container_count fmt rule_list =
  let rule_count = List.length rule_list in
  List.iteri
    (fun i rule_diff ->
      let is_last = i = rule_count - 1 && container_count = 0 in
      pp_rule_diff ~style ~is_last ~parent_prefix:"" fmt rule_diff)
    rule_list

let pp_reordered_section ~style ~container_count fmt = function
  | [] -> ()
  | lst ->
      Fmt.pf fmt "Rules reordered (%d rules):@," (List.length lst);
      pp_rule_list ~style ~container_count fmt lst

let pp_containers_section ~style fmt containers =
  let container_count = List.length containers in
  List.iteri
    (fun i cont_diff ->
      let is_last = i = container_count - 1 in
      pp_container_diff ~style ~is_last ~parent_prefix:"" fmt cont_diff)
    containers

let pp ?(expected = "Expected") ?(actual = "Actual") fmt { rules; containers } =
  if rules = [] && containers = [] then
    Fmt.pf fmt
      "Structural differences detected in nested contexts (e.g., @media inside \
       @layer)@,\
       but no rule-level differences found.@,\
       This may indicate reordering or subtle changes in rule organization."
  else (
    pp_diff_headers fmt expected actual;
    Fmt.pf fmt "@[<v>";
    let meaningful = meaningful_rules rules in
    let reordered_rules =
      List.filter (function Rule_reordered _ -> true | _ -> false) rules
    in
    let style = tree_style in
    let container_count = List.length containers in
    pp_rule_list ~style ~container_count fmt meaningful;
    pp_reordered_section ~style ~container_count fmt reordered_rules;
    pp_containers_section ~style fmt containers;
    Fmt.pf fmt "@]")

(* ===== Tree Diff Computation Functions ===== *)

(* Helper to extract rule information from statements *)
let convert_rule_to_strings stmt =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      (selector_str, decls)
  | None -> ("", [])

(* Helper to extract property-value pairs from declarations for comparison *)
let decl_to_prop_value decl =
  let name = Css.declaration_name decl in
  (* Preserve original formatting to catch differences such as 0px vs 0 *)
  let value = Css.declaration_value ~minify:false decl in
  let value =
    if Css.declaration_is_important decl then value ^ " !important" else value
  in
  (name, value)

(* Normalized signature of a declaration list for comparison/reordering
   checks *)
let decls_signature (decls : Css.declaration list) =
  List.map decl_to_prop_value decls |> List.sort compare

(* Normalize a selector string by sorting comma-separated selector items. This
   ensures we consider ".a,.b" equivalent to ".b,.a" when matching.

   Policy: Selector lists with the same items in different orders are considered
   equivalent for matching purposes. This means: - ".a, .b" and ".b, .a" will
   match as the same selector - Reordering within a list is not considered a
   structural change - This prevents false positives when CSS tools reorder
   selector lists *)
let normalize_selector_string s =
  s |> String.split_on_char ',' |> List.map String.trim
  |> List.filter (fun x -> x <> "")
  |> List.sort String.compare |> String.concat ","

let rule_selector stmt =
  match Css.statement_selector stmt with
  | Some s -> s
  | None -> Css.Selector.universal

let selector_key_of_selector sel =
  Css.Selector.to_string sel |> normalize_selector_string

let selector_key_of_stmt stmt = selector_key_of_selector (rule_selector stmt)

let rule_declarations stmt =
  match Css.statement_declarations stmt with Some d -> d | None -> []

(* Generic helper for finding added/removed/modified items between two lists.
   Works with any item type that has a key for comparison. *)
let diffs ~(key_of : 'item -> 'key) ~(key_equal : 'key -> 'key -> bool)
    ~(is_empty_diff : 'item -> 'item -> bool) items1 items2 =
  let find_by_key key items =
    List.find_opt (fun item -> key_equal (key_of item) key) items
  in
  let added =
    List.filter
      (fun item2 ->
        not
          (List.exists
             (fun item1 -> key_equal (key_of item1) (key_of item2))
             items1))
      items2
  in
  let removed =
    List.filter
      (fun item1 ->
        not
          (List.exists
             (fun item2 -> key_equal (key_of item1) (key_of item2))
             items2))
      items1
  in
  let modified =
    List.filter_map
      (fun item1 ->
        match find_by_key (key_of item1) items2 with
        | Some item2 when not (is_empty_diff item1 item2) -> Some (item1, item2)
        | _ -> None)
      items1
  in
  (added, removed, modified)

let rules_added_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let key_equal = String.equal in
  let is_empty_diff _ _ = true in
  let added, _removed, _modified =
    diffs ~key_of ~key_equal ~is_empty_diff rules1 rules2
  in
  added

let rules_removed_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let key_equal = String.equal in
  let is_empty_diff _ _ = true in
  let _added, removed, _modified =
    diffs ~key_of ~key_equal ~is_empty_diff rules1 rules2
  in
  removed

let selectors_share_parent sel1_str sel2_str =
  (* Check if two selectors share a common parent context *)
  let parts1 = String.split_on_char ' ' sel1_str |> List.rev in
  let parts2 = String.split_on_char ' ' sel2_str |> List.rev in
  match (parts1, parts2) with
  | _ :: p1_rest, _ :: p2_rest ->
      List.rev p1_rest = List.rev p2_rest && p1_rest <> []
  | _ -> false

let build_rule_lookup_tables rules2 =
  (* Create lookup tables for O(1) access *)
  let rules2_by_key = Hashtbl.create (List.length rules2) in
  let rules2_by_props = Hashtbl.create (List.length rules2) in

  (* Populate lookup tables *)
  List.iter
    (fun r ->
      let key = selector_key_of_stmt r in
      let decls = rule_declarations r in
      let props = decls_signature decls in

      (* Add to key-based lookup (multiple rules can have same key) *)
      let existing_key =
        try Hashtbl.find rules2_by_key key with Not_found -> []
      in
      Hashtbl.replace rules2_by_key key (r :: existing_key);

      (* Add to props-based lookup (multiple rules can have same props) *)
      let existing_props =
        try Hashtbl.find rules2_by_props props with Not_found -> []
      in
      Hashtbl.replace rules2_by_props props (r :: existing_props))
    rules2;
  (rules2_by_key, rules2_by_props)

(* Try to find an exact match by selector key and declarations *)
(* Returns: Some (Some diff) if selectors differ, Some None if exact match with same selectors, None if no exact match *)
let try_exact_match rules2_by_key used_rules r1 key1 d1 =
  let candidates = try Hashtbl.find rules2_by_key key1 with Not_found -> [] in
  match
    List.find_opt
      (fun r -> (not (Hashtbl.mem used_rules r)) && rule_declarations r = d1)
      candidates
  with
  | Some exact ->
      Hashtbl.replace used_rules exact ();
      let sel1 = rule_selector r1 in
      let sel2 = rule_selector exact in
      let sel1_str = Css.Selector.to_string sel1 in
      let sel2_str = Css.Selector.to_string sel2 in
      if sel1_str <> sel2_str then Some (Some (sel1, sel2, d1, d1))
      else Some None
  | None -> None

(* Try to find any rule with the same selector key *)
let try_same_key_match rules2_by_key used_rules r1 key1 d1 =
  let candidates = try Hashtbl.find rules2_by_key key1 with Not_found -> [] in
  match List.find_opt (fun r -> not (Hashtbl.mem used_rules r)) candidates with
  | Some r2 ->
      Hashtbl.replace used_rules r2 ();
      let d2 = rule_declarations r2 in
      Some (rule_selector r1, rule_selector r2, d1, d2)
  | None -> None

(* Try to find equivalent rule by properties with shared parent *)
let try_equivalent_props_match rules2_by_props used_rules r1 d1 props1 =
  let candidates =
    try Hashtbl.find rules2_by_props props1 with Not_found -> []
  in
  let sel1_str = Css.Selector.to_string (rule_selector r1) in
  match
    List.find_opt
      (fun r ->
        if Hashtbl.mem used_rules r then false
        else
          let sel2_str = Css.Selector.to_string (rule_selector r) in
          selectors_share_parent sel1_str sel2_str)
      candidates
  with
  | Some r2 ->
      Hashtbl.replace used_rules r2 ();
      let d2 = rule_declarations r2 in
      Some (rule_selector r1, rule_selector r2, d1, d2)
  | None -> None

let rules_modified_diff rules1 rules2 =
  let rules2_by_key, rules2_by_props = build_rule_lookup_tables rules2 in
  let used_rules = Hashtbl.create (List.length rules2) in

  let rec aux acc = function
    | [] -> List.rev acc
    | r1 :: t1 ->
        let key1 = selector_key_of_stmt r1 in
        let d1 = rule_declarations r1 in
        let props1 = decls_signature d1 in

        let pick =
          match try_exact_match rules2_by_key used_rules r1 key1 d1 with
          | Some (Some result) -> Some result
          | Some None ->
              None (* Exact match found but selectors are same - no diff *)
          | None -> (
              (* No exact match found - try other strategies *)
              match try_same_key_match rules2_by_key used_rules r1 key1 d1 with
              | Some result -> Some result
              | None ->
                  try_equivalent_props_match rules2_by_props used_rules r1 d1
                    props1)
        in

        let acc = match pick with None -> acc | Some x -> x :: acc in
        aux acc t1
  in
  aux [] rules1

let has_same_selectors rules1 rules2 =
  if List.length rules1 <> List.length rules2 then false
  else
    (* Use hash table for O(n) comparison instead of O(n log n) sorting *)
    let keys1_counts = Hashtbl.create (List.length rules1) in
    List.iter
      (fun r ->
        let key = selector_key_of_stmt r in
        let count = try Hashtbl.find keys1_counts key with Not_found -> 0 in
        Hashtbl.replace keys1_counts key (count + 1))
      rules1;

    let keys2_counts = Hashtbl.create (List.length rules2) in
    List.iter
      (fun r ->
        let key = selector_key_of_stmt r in
        let count = try Hashtbl.find keys2_counts key with Not_found -> 0 in
        Hashtbl.replace keys2_counts key (count + 1))
      rules2;

    (* Check if hash tables are equivalent *)
    try
      Hashtbl.iter
        (fun key count1 ->
          let count2 =
            try Hashtbl.find keys2_counts key with Not_found -> 0
          in
          if count1 <> count2 then raise Exit)
        keys1_counts;

      Hashtbl.iter
        (fun key count2 ->
          let count1 =
            try Hashtbl.find keys1_counts key with Not_found -> 0
          in
          if count1 <> count2 then raise Exit)
        keys2_counts;

      true
    with Exit -> false

let build_selector_map rules =
  (* Create map from selector to declarations *)
  List.fold_left
    (fun acc rule ->
      let sel = rule_selector rule in
      let decls = rule_declarations rule in
      (sel, decls) :: acc)
    [] rules
  |> List.rev

let selector_in_list sel_key remaining =
  (* Check if selector key exists in remaining list *)
  List.exists (fun (s, _) -> selector_key_of_selector s = sel_key) remaining

(* Locate matching declarations in map2 for a given selector key *)
let matching_decls_in_map2 sel1_key decls1 map2 decls2 =
  (* Prefer an exact declaration match for the same selector key if available *)
  match
    List.find_opt
      (fun (s, d) -> selector_key_of_selector s = sel1_key && d = decls1)
      map2
  with
  | Some (s, d) -> (d, Some s)
  | None -> (
      match
        List.find_opt (fun (s, _) -> selector_key_of_selector s = sel1_key) map2
      with
      | Some (s, d) -> (d, Some s)
      | None -> (decls2, None))

let ordering_diff rules1 rules2 =
  let map1 = build_selector_map rules1 in
  let map2 = build_selector_map rules2 in

  let rec find_ordering_issues acc remaining1 remaining2 =
    match (remaining1, remaining2) with
    | [], [] -> List.rev acc
    | (sel1, decls1) :: rest1, (sel2, decls2) :: rest2 ->
        let sel1_key = selector_key_of_selector sel1 in
        let sel2_key = selector_key_of_selector sel2 in
        if sel1_key <> sel2_key then
          let sel1_in_remaining2 = selector_in_list sel1_key remaining2 in
          let sel2_in_remaining1 = selector_in_list sel2_key remaining1 in

          if sel1_in_remaining2 && sel2_in_remaining1 then
            let decls1_from_map2, sel2_opt =
              matching_decls_in_map2 sel1_key decls1 map2 decls2
            in
            let sel2 = match sel2_opt with Some s -> s | None -> sel1 in
            find_ordering_issues
              ((sel1, sel2, decls1, decls1_from_map2) :: acc)
              rest1 rest2
          else find_ordering_issues acc rest1 rest2
        else find_ordering_issues acc rest1 rest2
    | _, _ -> List.rev acc
  in

  find_ordering_issues [] map1 map2

(* no-op: pure rule ordering is handled in handle_structural_diff via
   has_ordering_changes/ordering_diff *)

let extract_base_parent_selector sel =
  (* Extract parent from selector string and strip pseudo-classes *)
  let sel_str = Css.Selector.to_string sel in
  let parts = String.split_on_char ' ' sel_str in
  match parts with
  | [] | [ _ ] -> None
  | parent :: _ -> (
      match String.index_opt parent ':' with
      | Some idx -> Some (String.sub parent 0 idx)
      | None -> Some parent)

let selectors_share_parent_ast sel1 sel2 =
  (* Check if two selectors share meaningful parent context *)
  match
    (extract_base_parent_selector sel1, extract_base_parent_selector sel2)
  with
  | Some p1, Some p2 -> p1 = p2
  | _ -> false

let selector_changes all_added_candidates all_removed_candidates =
  (* Try to match removed rules with added rules for selector changes *)
  let matched_added = ref [] in
  let matched_removed = ref [] in
  let changes = ref [] in

  List.iter
    (fun removed_rule ->
      let removed_sel = rule_selector removed_rule in
      let removed_decls = rule_declarations removed_rule in
      let removed_props = decls_signature removed_decls in

      (* Look for a matching added rule with same properties but different
         selector *)
      let matching_added =
        List.find_opt
          (fun added_rule ->
            let added_sel = rule_selector added_rule in
            let added_decls = rule_declarations added_rule in
            let added_props = decls_signature added_decls in

            (* Same properties but different selectors that share parent
               context *)
            removed_props = added_props
            && removed_sel <> added_sel
            && selectors_share_parent_ast removed_sel added_sel)
          all_added_candidates
      in

      match matching_added with
      | Some added_rule ->
          let added_sel = rule_selector added_rule in
          changes :=
            (removed_sel, added_sel, removed_decls, removed_decls) :: !changes;
          matched_removed := removed_rule :: !matched_removed;
          matched_added := added_rule :: !matched_added
      | None -> ())
    all_removed_candidates;

  (!changes, !matched_added, !matched_removed)

(* Filter other_modified to exclude changes already captured as selector
   changes *)
let exclude_selector_changes_from_modified sel_changes other_modified =
  let sel_change_selectors =
    List.map
      (fun (sel1, sel2, _, _) ->
        (Css.Selector.to_string sel1, Css.Selector.to_string sel2))
      sel_changes
  in
  List.filter
    (fun (sel1, sel2, _, _) ->
      let sel1_str = Css.Selector.to_string sel1 in
      let sel2_str = Css.Selector.to_string sel2 in
      not (List.mem (sel1_str, sel2_str) sel_change_selectors))
    other_modified

let handle_structural_diff rules1 rules2 =
  let all_added_candidates = rules_added_diff rules1 rules2 in
  let all_removed_candidates = rules_removed_diff rules1 rules2 in

  let sel_changes, matched_added, matched_removed =
    selector_changes all_added_candidates all_removed_candidates
  in

  let added =
    List.filter (fun r -> not (List.memq r matched_added)) all_added_candidates
  in
  let removed =
    List.filter
      (fun r -> not (List.memq r matched_removed))
      all_removed_candidates
  in

  let other_modified = rules_modified_diff rules1 rules2 in
  let filtered_other_modified =
    exclude_selector_changes_from_modified sel_changes other_modified
  in

  let modified = sel_changes @ filtered_other_modified in

  let has_structural_changes = added <> [] || removed <> [] || modified <> [] in
  let has_ordering_changes =
    (not has_structural_changes)
    && has_same_selectors rules1 rules2
    && List.map selector_key_of_stmt rules1
       <> List.map selector_key_of_stmt rules2
  in

  let modified_with_order =
    if has_ordering_changes then ordering_diff rules1 rules2 @ modified
    else modified
  in

  (added, removed, modified_with_order)

let rule_diffs rules1 rules2 = handle_structural_diff rules1 rules2

(* Helper function to compute property diffs between two declaration lists,
   including added and removed properties *)
let properties_diff decls1 decls2 : declaration list * string list * string list
    =
  let props1 = List.map decl_to_prop_value decls1 in
  let props2 = List.map decl_to_prop_value decls2 in

  (* Find modified properties *)
  let modified =
    List.fold_left
      (fun acc (p1, v1) ->
        match List.assoc_opt p1 props2 with
        | Some v2 when v1 <> v2 ->
            { property_name = p1; expected_value = v1; actual_value = v2 }
            :: acc
        | _ -> acc)
      [] props1
    |> List.rev
  in

  (* Find added properties (in actual but not in expected) *)
  let added =
    List.fold_left
      (fun acc (p2, _v2) ->
        if not (List.mem_assoc p2 props1) then p2 :: acc else acc)
      [] props2
    |> List.rev
  in

  (* Find removed properties (in expected but not in actual) *)
  let removed =
    List.fold_left
      (fun acc (p1, _v1) ->
        if not (List.mem_assoc p1 props2) then p1 :: acc else acc)
      [] props1
    |> List.rev
  in

  (modified, added, removed)

(* Helper functions for converting rule changes - moved here for mutual
   recursion *)
let convert_added_rule stmt =
  let sel, decls = convert_rule_to_strings stmt in
  Rule_added { selector = sel; declarations = decls }

let convert_removed_rule stmt =
  let sel, decls = convert_rule_to_strings stmt in
  Rule_removed { selector = sel; declarations = decls }

let describe_statement stmt =
  let try_desc f = f stmt in
  let matchers =
    [
      (fun s ->
        Option.map (fun (s, _, _) -> Css.Selector.to_string s) (Css.as_rule s));
      (fun s ->
        Option.map
          (fun (c, _) -> "@media " ^ Css.Media.to_string c)
          (Css.as_media s));
      (fun s ->
        Option.map
          (fun (n, _) ->
            match n with Some name -> "@layer " ^ name | None -> "@layer")
          (Css.as_layer s));
      (fun s ->
        Option.map
          (fun (n, c, _) ->
            let prefix = match n with Some n -> n ^ " " | None -> "" in
            "@container " ^ prefix ^ Css.Container.to_string c)
          (Css.as_container s));
      (fun s ->
        Option.map
          (fun (c, _) -> "@supports " ^ Css.Supports.to_string c)
          (Css.as_supports s));
      (fun s -> Option.map (fun _ -> "@property") (Css.as_property s));
      (fun s ->
        Option.map (fun (name, _) -> "@keyframes " ^ name) (Css.as_keyframes s));
      (fun s -> Option.map (fun _ -> "@font-face") (Css.as_font_face s));
    ]
  in
  match List.find_map try_desc matchers with
  | Some desc -> Some desc
  | None -> Some "(other statement)"

let selector_position sel rules =
  let sel_key = selector_key_of_selector sel in
  List.mapi
    (fun i stmt ->
      match Css.as_rule stmt with
      | Some (s, _, _) when selector_key_of_selector s = sel_key -> Some i
      | _ -> None)
    rules
  |> List.find_map Fun.id |> Option.value ~default:(-1)

let selector_at_position pos rules =
  Option.bind (List.nth_opt rules pos) describe_statement

let content_changed selector old_decls new_decls =
  let property_changes, added_props, removed_props =
    properties_diff old_decls new_decls
  in
  Rule_content_changed
    {
      selector;
      old_declarations = old_decls;
      new_declarations = new_decls;
      property_changes;
      added_properties = added_props;
      removed_properties = removed_props;
    }

let reordered ~rules1 ~rules2 sel1 sel2 selector =
  let expected_pos = selector_position sel1 rules1 in
  let actual_pos = selector_position sel2 rules2 in
  let swapped_with = selector_at_position expected_pos rules2 in
  Rule_reordered
    {
      selector;
      expected_pos;
      actual_pos;
      swapped_with;
      old_declarations = None;
      new_declarations = None;
    }

let position_changed ~rules1 ~rules2 sel1 sel2 =
  let expected_pos = selector_position sel1 rules1 in
  let actual_pos = selector_position sel2 rules2 in
  expected_pos <> actual_pos

let is_pure_decl_reordering decls1 decls2 =
  let property_changes, added_props, removed_props =
    properties_diff decls1 decls2
  in
  let pure =
    property_changes = [] && added_props = [] && removed_props = []
    && decls_signature decls1 = decls_signature decls2
  in
  (pure, property_changes, added_props, removed_props)

let decl_level_reorder selector decls1 decls2 =
  Rule_reordered
    {
      selector;
      expected_pos = -1;
      actual_pos = -1;
      swapped_with = None;
      old_declarations = Some decls1;
      new_declarations = Some decls2;
    }

let decls_str_equal d1 d2 =
  List.length d1 = List.length d2
  && List.for_all2
       (fun x y -> decl_to_prop_value x = decl_to_prop_value y)
       d1 d2

let convert_modified_rule ~rules1 ~rules2 (sel1, sel2, decls1, decls2) =
  let sel1_str = Css.Selector.to_string sel1 in
  let sel2_str = Css.Selector.to_string sel2 in
  let position_changed () = position_changed ~rules1 ~rules2 sel1 sel2 in
  let reordered selector = reordered ~rules1 ~rules2 sel1 sel2 selector in
  let reorder_or_content selector d1 d2 =
    if position_changed () then Some (reordered selector)
    else Some (content_changed selector d1 d2)
  in

  (* Handle each modification case *)
  match (decls1, decls2) with
  | [], [] -> reorder_or_content sel1_str decls1 decls2
  | [], _ | _, [] -> Some (content_changed sel1_str decls1 decls2)
  | _, _ when sel1_str <> sel2_str ->
      Some
        (Rule_selector_changed
           {
             old_selector = sel1_str;
             new_selector = sel2_str;
             declarations = decls2;
           })
  | _, _ when decls1 = decls2 -> reorder_or_content sel1_str decls1 decls2
  | _, _ ->
      let pure, property_changes, added_props, removed_props =
        is_pure_decl_reordering decls1 decls2
      in
      if pure then
        if position_changed () then Some (reordered sel1_str)
        else if decls_str_equal decls1 decls2 then
          (* OCaml ASTs differ but string output is identical (e.g., Nested vs
             bare expression after calc() normalization) — no real difference *)
          None
        else Some (decl_level_reorder sel1_str decls1 decls2)
      else if property_changes <> [] || added_props <> [] || removed_props <> []
      then Some (content_changed sel1_str decls1 decls2)
      else reorder_or_content sel1_str decls1 decls2

(* Assemble rule changes (added/removed/modified) between two rule lists *)
let to_rule_changes rules1 rules2 : rule_diff list =
  let r_added, r_removed, r_modified = rule_diffs rules1 rules2 in
  List.map convert_added_rule r_added
  @ List.map convert_removed_rule r_removed
  @ List.filter_map (convert_modified_rule ~rules1 ~rules2) r_modified

(* Mutual recursion declarations *)
(* Check if two rule-lists under the same media condition differ *)
let rec media_condition_differs rules_list1 rules_list2 =
  let block_count_differs =
    List.length rules_list1 <> List.length rules_list2
  in
  let all_rules1 = List.concat rules_list1 in
  let all_rules2 = List.concat rules_list2 in
  let added_r, removed_r, modified_r = rule_diffs all_rules1 all_rules2 in
  let has_immediate = added_r <> [] || removed_r <> [] || modified_r <> [] in
  let has_nested = nested_differences ~depth:1 all_rules1 all_rules2 <> [] in
  if has_immediate || has_nested || block_count_differs then
    Some (all_rules1, all_rules2)
  else None

and media_diff items1 items2 =
  let group items =
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (cond, rules) ->
        let existing = try Hashtbl.find tbl cond with Not_found -> [] in
        Hashtbl.replace tbl cond (existing @ [ rules ]))
      items;
    tbl
  in
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

(* Generic helper for processing nested containers *)
(* Extract items with their positions from statements *)
and extract_items_with_positions extract_fn stmts =
  List.mapi
    (fun i stmt ->
      match extract_fn stmt with
      | Some (cond, rules) -> Some (i, cond, rules)
      | None -> None)
    stmts
  |> List.filter_map (fun x -> x)

(* Group items by condition *)
and group_by_condition items =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun (pos, cond, rules) ->
      let existing = try Hashtbl.find tbl cond with Not_found -> [] in
      Hashtbl.replace tbl cond (existing @ [ (pos, rules) ]))
    items;
  tbl

(* Detect block structure changes between two groups *)
and detect_block_structure_changes blocks1 blocks2 =
  let block_structure_changed = Hashtbl.create 16 in
  Hashtbl.iter
    (fun cond blocks1_list ->
      match Hashtbl.find_opt blocks2 cond with
      | Some blocks2_list ->
          let count_differs =
            List.length blocks1_list <> List.length blocks2_list
          in
          (* Check if block positions differ significantly *)
          let positions_differ_significantly =
            if List.length blocks1_list = List.length blocks2_list then
              let pos1_list = List.map fst blocks1_list in
              let pos2_list = List.map fst blocks2_list in
              (* Check if any corresponding positions differ by more than 10 *)
              List.exists2 (fun p1 p2 -> abs (p1 - p2) > 10) pos1_list pos2_list
            else false
          in
          if count_differs || positions_differ_significantly then
            Hashtbl.replace block_structure_changed cond
              (blocks1_list, blocks2_list)
      | _ -> ())
    blocks1;
  block_structure_changed

(* Check if only declaration reorders occurred *)
and only_declaration_reorders rule_changes nested_containers =
  rule_changes <> []
  && List.for_all
       (function
         | Rule_reordered
             { old_declarations = Some _; new_declarations = Some _; _ } ->
             true
         | _ -> false)
       rule_changes
  && nested_containers = []

(* Process a modified container and generate appropriate diff *)
and process_modified_container ~container_type ~extract_fn ~depth ~stmts1
    ~stmts2 ~block_structure_changed cond rules1 rules2 =
  (* Skip if this condition has a block structure change *)
  if Hashtbl.mem block_structure_changed cond then None
  else
    let rule_changes = to_rule_changes rules1 rules2 in
    (* Recursively check deeper nesting *)
    let nested_containers =
      nested_differences ~depth:(depth + 1) rules1 rules2
    in
    (* Check for position changes within parent container *)
    let find_position cond stmts =
      let rec go i = function
        | [] -> None
        | stmt :: rest -> (
            match extract_fn stmt with
            | Some (c, _) when c = cond -> Some i
            | _ -> go (i + 1) rest)
      in
      go 0 stmts
    in
    let pos1 = find_position cond stmts1 |> Option.value ~default:(-1) in
    let pos2 = find_position cond stmts2 |> Option.value ~default:(-1) in
    let position_changed = pos1 >= 0 && pos2 >= 0 && abs (pos2 - pos1) > 3 in

    (* If only position changed with no content changes, report as reordered *)
    if position_changed && rule_changes = [] && nested_containers = [] then
      Some
        (Container_reordered
           {
             info = { container_type; condition = cond; rules = rules1 };
             expected_pos = pos1;
             actual_pos = pos2;
           })
    else if not (only_declaration_reorders rule_changes nested_containers) then
      (* Container was modified in content, not just position *)
      Some
        (Container_modified
           {
             info = { container_type; condition = cond; rules = rules1 };
             actual_rules = rules2;
             rule_changes;
             container_changes = nested_containers;
           })
    else None

(* Detect if containers have only changed order (no content changes) *)
and detect_order_only_change ~container_type added removed items1 items2 =
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
            (Container_modified
               {
                 info = { container_type; condition = cond; rules = rules1 };
                 actual_rules = rules2;
                 rule_changes = [];
                 container_changes = [];
               })
      | _ -> None

and process_nested_containers ~container_type ~extract_fn ~diff_fn ~depth stmts1
    stmts2 =
  let items_with_pos1 = extract_items_with_positions extract_fn stmts1 in
  let items_with_pos2 = extract_items_with_positions extract_fn stmts2 in
  let block_structure_changed =
    detect_block_structure_changes
      (group_by_condition items_with_pos1)
      (group_by_condition items_with_pos2)
  in
  let items1 = List.filter_map extract_fn stmts1 in
  let items2 = List.filter_map extract_fn stmts2 in
  let added, removed, modified = diff_fn items1 items2 in
  let diffs = ref [] in
  Hashtbl.iter
    (fun cond (expected_blocks, actual_blocks) ->
      diffs :=
        Container_block_structure_changed
          { container_type; condition = cond; expected_blocks; actual_blocks }
        :: !diffs)
    block_structure_changed;
  List.iter
    (fun (cond, rules) ->
      diffs :=
        Container_added { container_type; condition = cond; rules } :: !diffs)
    added;
  List.iter
    (fun (cond, rules) ->
      diffs :=
        Container_removed { container_type; condition = cond; rules } :: !diffs)
    removed;
  List.iter
    (fun (cond, rules1, rules2) ->
      match
        process_modified_container ~container_type ~extract_fn ~depth ~stmts1
          ~stmts2 ~block_structure_changed cond rules1 rules2
      with
      | Some diff -> diffs := diff :: !diffs
      | None -> ())
    modified;
  (if !diffs = [] then
     match
       detect_order_only_change ~container_type added removed items1 items2
     with
     | Some d -> diffs := [ d ]
     | None -> ());
  !diffs

(* Layer diff function *)
and layer_diff items1 items2 =
  let key_of (name_opt, _) = Option.value ~default:"" name_opt in
  let key_equal = String.equal in
  let is_empty_diff (_, rules1) (_, rules2) =
    let a_r, r_r, m_r = rule_diffs rules1 rules2 in
    let has_immediate_diffs = a_r <> [] || r_r <> [] || m_r <> [] in
    if has_immediate_diffs then false
    else
      (* Also check for nested differences *)
      let nested_diffs = nested_differences ~depth:1 rules1 rules2 in
      nested_diffs = []
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to consistent format with media_diff *)
  let added =
    List.map
      (fun (name_opt, rules) -> (Option.value ~default:"" name_opt, rules))
      added
  in
  let removed =
    List.map
      (fun (name_opt, rules) -> (Option.value ~default:"" name_opt, rules))
      removed
  in
  let modified =
    List.map
      (fun ((name_opt, rules1), (_, rules2)) ->
        (Option.value ~default:"" name_opt, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Shared helper: collect added/removed container diffs and process modified
   containers with the standard rule-change + nesting logic. *)
and collect_container_diffs ~container_type ~depth added removed modified =
  let diffs = ref [] in
  List.iter
    (fun (condition, rules) ->
      diffs := Container_added { container_type; condition; rules } :: !diffs)
    added;
  List.iter
    (fun (condition, rules) ->
      diffs := Container_removed { container_type; condition; rules } :: !diffs)
    removed;
  List.iter
    (fun (condition, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      let nested_containers =
        nested_differences ~depth:(depth + 1) rules1 rules2
      in
      if
        (rule_changes <> [] || nested_containers <> [])
        && not (only_declaration_reorders rule_changes nested_containers)
      then
        diffs :=
          Container_modified
            {
              info = { container_type; condition; rules = rules1 };
              actual_rules = rules2;
              rule_changes;
              container_changes = nested_containers;
            }
          :: !diffs)
    modified;
  !diffs

(* Process layers separately due to different type signature *)
and process_nested_layers ~depth stmts1 stmts2 =
  let items1 = List.filter_map Css.as_layer stmts1 in
  let items2 = List.filter_map Css.as_layer stmts2 in
  let added, removed, modified = layer_diff items1 items2 in
  collect_container_diffs ~container_type:`Layer ~depth added removed modified

(* Container diff function for @container rules *)
and container_diff items1 items2 =
  let key_of (name_opt, condition, _) =
    (* Use both name and condition as key to distinguish different containers *)
    let cond_str = Css.Container.to_string condition in
    match name_opt with Some name -> name ^ ":" ^ cond_str | None -> cond_str
  in
  let key_equal = String.equal in
  let is_empty_diff (_, _, rules1) (_, _, rules2) =
    let a_r, r_r, m_r = rule_diffs rules1 rules2 in
    let has_immediate_diffs = a_r <> [] || r_r <> [] || m_r <> [] in
    if has_immediate_diffs then false
    else
      (* Also check for nested differences *)
      let nested_diffs = nested_differences ~depth:1 rules1 rules2 in
      nested_diffs = []
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to consistent format with media_diff *)
  let added =
    List.map
      (fun (name_opt, condition, rules) ->
        let cond_str = Css.Container.to_string condition in
        let condition_str =
          match name_opt with
          | Some name -> name ^ " " ^ cond_str
          | None -> cond_str
        in
        (condition_str, rules))
      added
  in
  let removed =
    List.map
      (fun (name_opt, condition, rules) ->
        let cond_str = Css.Container.to_string condition in
        let condition_str =
          match name_opt with
          | Some name -> name ^ " " ^ cond_str
          | None -> cond_str
        in
        (condition_str, rules))
      removed
  in
  let modified =
    List.map
      (fun ((name_opt, condition, rules1), (_, _, rules2)) ->
        let cond_str = Css.Container.to_string condition in
        let condition_str =
          match name_opt with
          | Some name -> name ^ " " ^ cond_str
          | None -> cond_str
        in
        (condition_str, rules1, rules2))
      modified_pairs
  in
  (added, removed, modified)

(* Property diff function for @property rules *)
and property_diff items1 items2 =
  let key_of (Css.Property_info { name; _ }) = name in
  let key_equal = String.equal in
  let is_empty_diff prop1 prop2 =
    (* For @property, we consider them empty diff if they're structurally
       equivalent *)
    let (Css.Property_info { name = n1; inherits = i1; _ }) = prop1 in
    let (Css.Property_info { name = n2; inherits = i2; _ }) = prop2 in
    n1 = n2 && i1 = i2
    (* Note: We can't easily compare syntax and initial_value due to existential
       types *)
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~key_equal ~is_empty_diff items1 items2
  in
  (* Transform to format compatible with container processing *)
  let added =
    List.map (fun (Css.Property_info { name; _ }) -> (name, [])) added
  in
  let removed =
    List.map (fun (Css.Property_info { name; _ }) -> (name, [])) removed
  in
  let modified =
    List.map
      (fun (Css.Property_info { name; _ }, _) -> (name, [], []))
      modified_pairs
  in
  (added, removed, modified)

(* Process container rules *)
and process_nested_containers_with_name ~depth stmts1 stmts2 =
  let items1 = List.filter_map Css.as_container stmts1 in
  let items2 = List.filter_map Css.as_container stmts2 in
  let added, removed, modified = container_diff items1 items2 in
  collect_container_diffs ~container_type:`Container ~depth added removed
    modified

(* Detect @property reordering: same names in different order *)
and property_reorder_diffs stmts1 stmts2 items1 items2 =
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
        (fun (i1, name1) ->
          let i2 =
            (let rec go i = function
               | [] -> None
               | x :: _ when x = name1 -> Some i
               | _ :: rest -> go (i + 1) rest
             in
             go 0 names2)
            |> Option.value ~default:i1
          in
          if i1 = i2 then None
          else
            let swapped_with =
              if i1 < List.length names2 then
                Some ("@property " ^ List.nth names2 i1)
              else None
            in
            Some
              (Rule_reordered
                 {
                   selector = "@property " ^ name1;
                   expected_pos = i1;
                   actual_pos = i2;
                   swapped_with;
                   old_declarations = None;
                   new_declarations = None;
                 }))
        (List.mapi (fun i n -> (i, n)) names1)
    in
    match reorder_diffs with
    | [] -> []
    | _ ->
        [
          Container_modified
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

(* Process property rules *)
and process_nested_properties ~depth stmts1 stmts2 =
  let items1 = List.filter_map Css.as_property stmts1 in
  let items2 = List.filter_map Css.as_property stmts2 in
  let added, removed, modified = property_diff items1 items2 in
  let diffs = ref [] in
  List.iter
    (fun (name, rules) ->
      diffs :=
        Container_added { container_type = `Property; condition = name; rules }
        :: !diffs)
    added;
  List.iter
    (fun (name, rules) ->
      diffs :=
        Container_removed
          { container_type = `Property; condition = name; rules }
        :: !diffs)
    removed;
  List.iter
    (fun (name, rules1, rules2) ->
      let rule_changes = to_rule_changes rules1 rules2 in
      let nested_containers =
        nested_differences ~depth:(depth + 1) rules1 rules2
      in
      diffs :=
        Container_modified
          {
            info =
              { container_type = `Property; condition = name; rules = rules1 };
            actual_rules = rules2;
            rule_changes;
            container_changes = nested_containers;
          }
        :: !diffs)
    modified;
  !diffs @ property_reorder_diffs stmts1 stmts2 items1 items2

(* Wrapper to extract media with string condition for diffing *)
and extract_media_as_string stmt =
  match Css.as_media stmt with
  | Some (cond, rules) -> Some (Css.Media.to_string cond, rules)
  | None -> None

(* Wrapper to extract supports with string condition for diffing *)
and extract_supports_as_string stmt =
  match Css.as_supports stmt with
  | Some (cond, rules) -> Some (Css.Supports.to_string cond, rules)
  | None -> None

(* Main recursive function for nested differences *)
and nested_differences ?(depth = 0) (stmts1 : Css.statement list)
    (stmts2 : Css.statement list) : container_diff list =
  if depth > 3 then [] (* Prevent infinite recursion *)
  else
    (* Process media queries *)
    process_nested_containers ~container_type:`Media
      ~extract_fn:extract_media_as_string ~diff_fn:media_diff ~depth stmts1
      stmts2
    (* Process layers - different type signature *)
    @ process_nested_layers ~depth stmts1 stmts2
    (* Process supports - reuses media_diff since they have the same
       structure *)
    @ process_nested_containers ~container_type:`Supports
        ~extract_fn:extract_supports_as_string ~diff_fn:media_diff ~depth stmts1
        stmts2
    (* Process container queries *)
    @ process_nested_containers_with_name ~depth stmts1 stmts2
    (* Process property declarations *)
    @ process_nested_properties ~depth stmts1 stmts2
    (* Process keyframes animations *)
    @ process_nested_keyframes ~depth stmts1 stmts2
    (* Process font-face rules *)
    @ process_font_face_rules ~depth stmts1 stmts2

(* Process keyframes animations *)
and keyframes_container_info name =
  { container_type = `Layer; condition = "@keyframes " ^ name; rules = [] }

and process_nested_keyframes ~depth:_ stmts1 stmts2 =
  let items1 = List.filter_map Css.as_keyframes stmts1 in
  let items2 = List.filter_map Css.as_keyframes stmts2 in
  let added, removed, modified = keyframes_diff items1 items2 in

  let added_diffs =
    List.map
      (fun (name, _frames) -> Container_added (keyframes_container_info name))
      added
  in
  let removed_diffs =
    List.map
      (fun (name, _frames) -> Container_removed (keyframes_container_info name))
      removed
  in
  let modified_diffs =
    List.filter_map
      (fun ((name, frames1), (_, frames2)) ->
        let frame_diffs = keyframe_frames_diff frames1 frames2 in
        if frame_diffs <> [] then
          Some
            (Container_modified
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

(* Diff keyframe frames *)
and keyframe_frames_diff frames1 frames2 =
  (* Key by frame selector (e.g., "0%", "from", "to", "100%") *)
  let key_of (frame : Css.keyframe) = frame.keyframe_selector in
  let key_equal = Css.Keyframe.selector_equal in
  let is_empty_diff (f1 : Css.keyframe) (f2 : Css.keyframe) =
    Css.Keyframe.selector_equal f1.keyframe_selector f2.keyframe_selector
    && f1.keyframe_declarations = f2.keyframe_declarations
  in
  let added, removed, modified_pairs =
    diffs ~key_of ~key_equal ~is_empty_diff frames1 frames2
  in

  (* Convert frame changes to rule_diff format *)
  let selector_str (frame : Css.keyframe) =
    Css.Keyframe.selector_to_string frame.keyframe_selector
  in
  let added_changes =
    List.map
      (fun (frame : Css.keyframe) ->
        Rule_added { selector = selector_str frame; declarations = [] })
      added
  in
  let removed_changes =
    List.map
      (fun (frame : Css.keyframe) ->
        Rule_removed { selector = selector_str frame; declarations = [] })
      removed
  in
  let modified_changes =
    List.filter_map
      (fun ((f1 : Css.keyframe), (f2 : Css.keyframe)) ->
        if f1.keyframe_declarations <> f2.keyframe_declarations then
          Some
            (Rule_content_changed
               {
                 selector = selector_str f1;
                 old_declarations = [];
                 new_declarations = [];
                 property_changes = [];
                 added_properties = [];
                 removed_properties = [];
               })
        else None)
      modified_pairs
  in

  added_changes @ removed_changes @ modified_changes

(* Keyframes diff function *)
and keyframes_diff items1 items2 =
  let key_of (name, _) = name in
  let key_equal = String.equal in
  let is_empty_diff (name1, frames1) (name2, frames2) =
    name1 = name2 && frames1 = frames2
  in
  diffs ~key_of ~key_equal ~is_empty_diff items1 items2

(* Process font-face rules *)
and process_font_face_rules ~depth:_ stmts1 stmts2 =
  let items1 = List.filter_map Css.as_font_face stmts1 in
  let items2 = List.filter_map Css.as_font_face stmts2 in

  (* For font-face, we compare the entire descriptor list *)
  let diffs = ref [] in

  (* If there are any font-face rules in either side, check if they differ *)
  match (items1, items2) with
  | [], [] -> []
  | [], _ ->
      (* Font-face added *)
      diffs :=
        Container_added
          { container_type = `Layer; condition = "@font-face"; rules = [] }
        :: !diffs;
      !diffs
  | _, [] ->
      (* Font-face removed *)
      diffs :=
        Container_removed
          { container_type = `Layer; condition = "@font-face"; rules = [] }
        :: !diffs;
      !diffs
  | descs1 :: _, descs2 :: _ ->
      (* Font-face modified - compare descriptors *)
      if descs1 <> descs2 then
        diffs :=
          Container_modified
            {
              info =
                {
                  container_type = `Layer;
                  condition = "@font-face";
                  rules = [];
                };
              actual_rules = [];
              rule_changes = [];
              (* Font-face descriptors are not rules *)
              container_changes = [];
              (* Font-face doesn't have nested containers *)
            }
          :: !diffs;
      !diffs

(* Check if containers appear at different positions in statement sequence *)
let detect_container_position_changes stmts1 stmts2 containers =
  (* Build position maps for @media containers *)
  let build_media_position_map stmts =
    List.mapi
      (fun i stmt ->
        match Css.as_media stmt with
        | Some (cond, _) -> Some (Css.Media.to_string cond, i)
        | None -> None)
      stmts
    |> List.filter_map (fun x -> x)
    |> List.fold_left
         (fun acc (cond, pos) ->
           let existing = try List.assoc cond acc with Not_found -> [] in
           (cond, pos :: existing) :: List.remove_assoc cond acc)
         []
  in

  let pos_map1 = build_media_position_map stmts1 in
  let pos_map2 = build_media_position_map stmts2 in

  (* Enhance container_diffs with position info *)
  List.map
    (function
      | Container_modified
          ({
             info = { container_type = `Media; condition; _ };
             rule_changes;
             container_changes;
             _;
           } as cm)
        when rule_changes = [] && container_changes = [] ->
          (* No content changes - check if position changed *)
          let pos1 =
            try List.assoc condition pos_map1 |> List.hd
            with Not_found | Failure _ -> -1
          in
          let pos2 =
            try List.assoc condition pos_map2 |> List.hd
            with Not_found | Failure _ -> -1
          in
          if pos1 >= 0 && pos2 >= 0 && abs (pos2 - pos1) > 5 then
            (* Significant position change - report as structure difference *)
            Container_modified
              { cm with info = { cm.info with rules = [] }; actual_rules = [] }
          else Container_modified cm
      | other -> other)
    containers

(* Main diff function *)
let diff ~(expected : Css.t) ~(actual : Css.t) : t =
  let rules1 = Css.statements expected in
  let rules2 = Css.statements actual in
  let added, removed, modified = rule_diffs rules1 rules2 in

  let rule_changes =
    List.map convert_added_rule added
    @ List.map convert_removed_rule removed
    @ List.filter_map (convert_modified_rule ~rules1 ~rules2) modified
  in

  (* Delegate all container and nested-container diffs to the generic walker *)
  let containers =
    let stmts1 = Css.statements expected in
    let stmts2 = Css.statements actual in
    let base_containers = nested_differences ~depth:0 stmts1 stmts2 in
    detect_container_position_changes stmts1 stmts2 base_containers
  in

  { rules = rule_changes; containers }
