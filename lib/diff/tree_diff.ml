(** CSS tree difference analysis for structural comparison. *)

open Cascade

(* ===== Type Definitions ===== *)

type declaration = {
  property_name : string;
  expected_value : string;
  actual_value : string;
}

type rule_diff =
  | Added of { selector : string; declarations : Css.declaration list }
  | Removed of { selector : string; declarations : Css.declaration list }
  | Content_changed of {
      selector : string;
      old_declarations : Css.declaration list;
      new_declarations : Css.declaration list;
      property_changes : declaration list;
      added_properties : (string * string) list;
      removed_properties : (string * string) list;
    }
  | Selector_changed of {
      old_selector : string;
      new_selector : string;
      declarations : Css.declaration list;
    }
  | Reordered of {
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
  | Rearranged of { selector : string; declarations : Css.declaration list }
    (* Every declaration the selector carries survives on both sides, spread
       differently over the rules that write it. An element that also matches an
       overlapping selector can resolve differently. *)
  | Regrouped of {
      from_selectors : string list; (* rule selectors in expected *)
      to_selectors : string list; (* rule selectors in actual *)
    }
(* A comma group merged or split across rules with identical declarations: the
   same selectors survive, only the grouping differs. *)

type container_info = {
  container_type :
    [ `Media
    | `Layer
    | `Supports
    | `Container
    | `Property
    | `Nesting
    | `At_rule ];
  condition : string;
  rules : Css.statement list;
}

type container_diff =
  | Added of container_info
  | Removed of container_info
  | Modified of {
      info : container_info; (* expected *)
      actual_rules : Css.statement list; (* actual *)
      rule_changes : rule_diff list;
      container_changes : container_diff list; (* Nested container changes *)
    }
  | Reordered of { info : container_info; expected_pos : int; actual_pos : int }
  | Block_structure_changed of {
      container_type :
        [ `Media
        | `Layer
        | `Supports
        | `Container
        | `Property
        | `Nesting
        | `At_rule ];
      condition : string;
      expected_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in expected *)
      actual_blocks : (int * Css.statement list) list;
          (** (position, rules) for each block in actual *)
    }

type layer_order_diff = {
  expected_order : string list;
  actual_order : string list;
  swapped : (string * string) list;
}

type t = {
  rules : rule_diff list;
  containers : container_diff list;
  layer_order : layer_order_diff option;
}

(* ===== Constants ===== *)

let default_truncation_length = String_diff.default_max_width

(* How many swapped layer pairs the report names before counting the rest. A
   reversed order of n layers inverts n(n-1)/2 pairs, and reading the first few
   is enough to place the change. *)
let max_layer_swaps = 5

(* ===== Helper Functions ===== *)

let is_empty d =
  d.rules = [] && d.containers = [] && Option.is_none d.layer_order

(* ===== Pretty Printing Functions ===== *)

(* Tree-style formatting helpers *)
type tree_style = {
  use_tree : bool; (* Whether to use tree-style box-drawing characters *)
  color : bool; (* Whether to wrap diff markers in ANSI colors *)
  depth : int; (* Levels still renderable below the current node *)
}

let unlimited_depth = max_int
let unlimited_entries = max_int
let default_style = { use_tree = false; color = false; depth = unlimited_depth }
let tree_style = { use_tree = true; color = false; depth = unlimited_depth }

(* ANSI color helpers. Plain text unless [color] is set: the printers write into
   a [Buffer.t], so tty detection cannot happen here; the caller decides. *)
let ansi code ~color s =
  if color then String.concat "" [ "\027["; code; "m"; s; "\027[0m" ] else s

let ansi_green ~color s = ansi "32" ~color s
let ansi_red ~color s = ansi "31" ~color s
let ansi_yellow ~color s = ansi "33" ~color s

let style_text ~color action s =
  match action with
  | "add" -> ansi_green ~color s
  | "remove" -> ansi_red ~color s
  | _ -> s

(* Get the appropriate prefix for tree-style formatting *)
let tree_prefix ~style ~is_last ~parent_prefix =
  if not style.use_tree then ""
  else
    let connector =
      if is_last then "\u{2514}\u{2500} " else "\u{251c}\u{2500} "
    in
    parent_prefix ^ connector

(* Get the continuation prefix for children *)
let tree_continuation ~style ~is_last ~parent_prefix =
  if not style.use_tree then parent_prefix
  else
    let continuation = if is_last then "   " else "\u{2502}  " in
    parent_prefix ^ continuation

(* Leaf lines (declarations, block listings) hang off the continuation prefix
   without a connector of their own. *)
let child_indent ~style ~parent_prefix =
  if style.use_tree then parent_prefix ^ "   " else parent_prefix ^ "    "

let add_strings buf ls = List.iter (Buffer.add_string buf) ls

let count_lines buf =
  let n = ref 0 in
  String.iter (fun c -> if c = '\n' then incr n) (Buffer.contents buf);
  !n

(* Render a node's children under the depth budget. Past the budget the subtree
   is still rendered, but only to report how much it hides: a diff that silently
   stopped at depth N would read as "nothing more to see". *)
let pp_children ~style ~parent_prefix buf render =
  if style.depth > 0 then render { style with depth = style.depth - 1 } buf
  else
    let sub = Buffer.create 256 in
    render { style with depth = unlimited_depth } sub;
    match count_lines sub with
    | 0 -> ()
    | n ->
        let noun = if n = 1 then " more line\n" else " more lines\n" in
        add_strings buf
          [ child_indent ~style ~parent_prefix; "..."; string_of_int n; noun ]

let decl_with_important decl value =
  if Css.declaration_is_important decl then
    String.concat "" [ value; " !important" ]
  else value

(* The one answer to how a declaration reads in the report: the author's own
   spelling, which keeps a unit difference like [0px] against [0] visible, and
   the flag, without which a declaration and the one it outranks read alike. *)
let decl_shown_value decl =
  decl_with_important decl (Css.declaration_value ~minify:false decl)

(* Print a list of CSS declarations with an action prefix *)
let pp_declarations ?(style = default_style) ?(parent_prefix = "") buf action
    decls =
  let prefix_symbol =
    match action with
    | "add" -> "+"
    | "remove" -> "-"
    | "same" -> " " (* context marker: present on both sides *)
    | _ -> action (* fallback for other actions like "declarations" *)
  in
  (* Properties don't get tree connectors - just indentation continuation *)
  let indent =
    if style.use_tree then parent_prefix ^ "   " else parent_prefix ^ "    "
  in
  List.iter
    (fun decl ->
      let shown =
        String_diff.truncate_middle default_truncation_length
          (decl_shown_value decl)
      in
      let line =
        String.concat ""
          [ prefix_symbol; " "; Css.declaration_name decl; ": "; shown ]
      in
      add_strings buf
        [ indent; style_text ~color:style.color action line; "\n" ])
    decls

let pp_property_diff ?(style = default_style) ?(parent_prefix = "") buf
    { property_name; expected_value; actual_value } =
  let indent =
    if style.use_tree then parent_prefix ^ "   " else parent_prefix ^ "    "
  in
  match String_diff.first_diff_pos expected_value actual_value with
  | None ->
      (* Shouldn't happen but handle gracefully *)
      add_strings buf [ indent; "* "; property_name; ": (no diff detected)\n" ]
  | Some _ ->
      let len1 = String.length expected_value in
      let len2 = String.length actual_value in
      if len1 <= 30 && len2 <= 30 then
        (* Short values: show inline with red for old, green for new *)
        add_strings buf
          [
            indent;
            "* ";
            property_name;
            ": ";
            ansi_red ~color:style.color expected_value;
            " -> ";
            ansi_green ~color:style.color actual_value;
            "\n";
          ]
      else
        (* Long values: truncate and show as separate lines *)
        let exp_truncated =
          String_diff.truncate_middle default_truncation_length expected_value
        in
        let act_truncated =
          String_diff.truncate_middle default_truncation_length actual_value
        in
        add_strings buf [ indent; "* "; property_name; ":\n" ];
        add_strings buf
          [
            indent;
            "  ";
            ansi_red ~color:style.color ("- " ^ exp_truncated);
            "\n";
          ];
        add_strings buf
          [
            indent;
            "  ";
            ansi_green ~color:style.color ("+ " ^ act_truncated);
            "\n";
          ]

let pp_property_diffs ?(style = default_style) ?(parent_prefix = "") buf
    prop_diffs =
  List.iter (pp_property_diff ~style ~parent_prefix buf) prop_diffs

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
let index_of_property name names =
  let rec find_idx i = function
    | [] -> -1
    | x :: _ when x = name -> i
    | _ :: rest -> find_idx (i + 1) rest
  in
  find_idx 0 names

let property_moves ~max_count prop_names1 prop_names2 =
  let rec scan lst1 lst2 acc count =
    if count >= max_count then List.rev acc
    else
      match (lst1, lst2) with
      | x1 :: rest1, x2 :: rest2 when x1 <> x2 ->
          let new_pos = index_of_property x1 prop_names2 in
          scan rest1 rest2 ((x1, new_pos) :: acc) (count + 1)
      | _ :: rest1, _ :: rest2 -> scan rest1 rest2 acc count
      | _, _ -> List.rev acc
  in
  scan prop_names1 prop_names2 [] 0

(* Helper to print property moves *)
let pp_property_moves buf indent moves total_diffs =
  add_strings buf [ indent; "* reorder: " ];
  List.iteri
    (fun i (prop, new_pos) ->
      if i > 0 then Buffer.add_string buf ", ";
      if new_pos >= 0 then
        add_strings buf [ prop; "\xe2\x86\x92"; string_of_int new_pos ]
      else Buffer.add_string buf prop)
    moves;
  if total_diffs > List.length moves then
    add_strings buf
      [ " (and "; string_of_int (total_diffs - List.length moves); " more)" ];
  Buffer.add_char buf '\n'

let pp_property_move_summary buf indent prop_names1 prop_names2 =
  let moves = property_moves ~max_count:3 prop_names1 prop_names2 in
  if moves <> [] then
    let total_diffs =
      List.fold_left2
        (fun acc p1 p2 -> if p1 <> p2 then acc + 1 else acc)
        0 prop_names1 prop_names2
    in
    pp_property_moves buf indent moves total_diffs

let pp_same_property_reorder buf indent prop_names1 prop_names2 =
  match adjacent_swap prop_names1 prop_names2 with
  | Some (prop1, prop2) ->
      let truncate s = String_diff.truncate_middle 20 s in
      add_strings buf
        [ indent; "* "; truncate prop1; " \xe2\x86\x94 "; truncate prop2; "\n" ]
  | None -> pp_property_move_summary buf indent prop_names1 prop_names2

(* A declaration reorder changes the cascade only when two overlapping
   declarations swap relative order; disjoint declarations commute, so their
   reorder is no difference (README contract). Duplicated property names are a
   same-property override, reported conservatively. *)
let reorder_is_significant decls1 decls2 =
  let name = Css.declaration_name in
  let names1 = List.map name decls1 in
  let has_dup =
    let s = List.sort String.compare names1 in
    let rec go = function a :: (b :: _ as t) -> a = b || go t | _ -> false in
    go s
  in
  has_dup
  ||
  let pos2 = Hashtbl.create 16 in
  List.iteri (fun i d -> Hashtbl.replace pos2 (name d) i) decls2;
  let arr = Array.of_list decls1 in
  let n = Array.length arr in
  (* Both hoisted out of the pair loop: naming a declaration and reading its
     overlap keys each serialize the property through a fresh buffer. *)
  let pos =
    Array.of_list
      (List.map
         (fun prop -> Option.value ~default:(-1) (Hashtbl.find_opt pos2 prop))
         names1)
  in
  let footprints = Array.map Shorthand.declaration_overlap_keys arr in
  let flipped = ref false in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if
        Shorthand.declarations_overlap_with_keys arr.(i) footprints.(i) arr.(j)
          footprints.(j)
        && pos.(i) >= pos.(j)
      then flipped := true
    done
  done;
  !flipped

let pp_reorder ?(style = default_style) ?(parent_prefix = "") decls1 decls2 buf
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
  if
    same_props && prop_names1 <> prop_names2
    && reorder_is_significant decls1 decls2
  then pp_same_property_reorder buf indent prop_names1 prop_names2

let pp_content_changed_body ~style ~child_prefix buf ~old_declarations
    ~new_declarations ~property_changes ~added_properties ~removed_properties
    ~has_any_changes =
  let indent = child_indent ~style ~parent_prefix:child_prefix in
  let pp_presence marker paint (prop_name, value) =
    let value = String_diff.truncate_middle default_truncation_length value in
    add_strings buf
      [
        indent;
        paint ~color:style.color
          (String.concat "" [ marker; " "; prop_name; ": "; value ]);
        "\n";
      ]
  in
  List.iter (pp_presence "-" ansi_red) removed_properties;
  List.iter (pp_presence "+" ansi_green) added_properties;
  pp_property_diffs ~style ~parent_prefix:child_prefix buf property_changes;
  pp_reorder ~style ~parent_prefix:child_prefix old_declarations
    new_declarations buf;
  if
    (not has_any_changes)
    && not
         (List.equal Declaration.equal_declaration old_declarations
            new_declarations)
  then
    let old_count = List.length old_declarations in
    let new_count = List.length new_declarations in
    if old_count <> new_count then
      add_strings buf
        [
          indent;
          "(declaration count: ";
          string_of_int old_count;
          " -> ";
          string_of_int new_count;
          ")\n";
        ]
    else add_strings buf [ indent; "(declarations differ in subtle ways)\n" ]

let pp_content_changed ~style ~prefix ~child_prefix buf ~selector
    ~old_declarations ~new_declarations ~property_changes ~added_properties
    ~removed_properties =
  let has_any_changes =
    property_changes <> [] || added_properties <> [] || removed_properties <> []
  in
  if
    (not has_any_changes)
    && List.equal Declaration.equal_declaration old_declarations
         new_declarations
  then ()
  else if selector = "" then
    (* The parent already named the subject, as it does for an [@property] whose
       descriptors changed. Repeating it as a child label reads as two entries
       for one registration. *)
    pp_content_changed_body ~style ~child_prefix buf ~old_declarations
      ~new_declarations ~property_changes ~added_properties ~removed_properties
      ~has_any_changes
  else (
    add_strings buf [ prefix; selector; "\n" ];
    pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
        pp_content_changed_body ~style ~child_prefix buf ~old_declarations
          ~new_declarations ~property_changes ~added_properties
          ~removed_properties ~has_any_changes))

(* The index counts the rules of a container, while whether the selector moved
   is judged against the selectors both sides share, so the two disagree: the
   middle selector of a reversal keeps its index, and a rule dropped ahead of a
   selector shifts it back onto the one it had. Pairing that index with itself
   states nothing, so the move is named without a coordinate. *)
let pp_position_reorder ~prefix buf ~selector ~expected_pos ~actual_pos
    ~swapped_with =
  let truncate s = String_diff.truncate_middle 40 s in
  if expected_pos = actual_pos then
    add_strings buf [ prefix; truncate selector; " (moved)\n" ]
  else
    match swapped_with with
    | Some other when abs (expected_pos - actual_pos) = 1 ->
        add_strings buf
          [ prefix; truncate selector; " \xe2\x86\x94  "; truncate other; "\n" ]
    | Some other ->
        add_strings buf
          [
            prefix;
            truncate selector;
            " (position ";
            string_of_int actual_pos;
            ") \xe2\x86\x94  ";
            truncate other;
            " (position ";
            string_of_int expected_pos;
            ")\n";
          ]
    | None ->
        add_strings buf
          [
            prefix;
            truncate selector;
            " (position ";
            string_of_int expected_pos;
            " \xe2\x86\x92 ";
            string_of_int actual_pos;
            ")\n";
          ]

let pp_regrouped ~style ~prefix ~child_prefix buf ~from_selectors ~to_selectors
    =
  let nf = List.length from_selectors and nt = List.length to_selectors in
  let verb =
    if nf > nt then "merged" else if nf < nt then "split" else "regrouped"
  in
  add_strings buf [ prefix; "selectors "; verb; "\n" ];
  pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
      let indent = child_indent ~style ~parent_prefix:child_prefix in
      List.iter
        (fun s ->
          add_strings buf
            [
              indent;
              ansi_red ~color:style.color (String.concat "" [ "- "; s ]);
              "\n";
            ])
        from_selectors;
      List.iter
        (fun s ->
          add_strings buf
            [
              indent;
              ansi_green ~color:style.color (String.concat "" [ "+ "; s ]);
              "\n";
            ])
        to_selectors)

let pp_rule_diff ?(style = default_style) ?(is_last = false)
    ?(parent_prefix = "") buf (diff : rule_diff) =
  match diff with
  | Added { selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      add_strings buf [ prefix; selector; "\n" ];
      pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
          pp_declarations ~style ~parent_prefix:child_prefix buf "add"
            declarations)
  | Removed { selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      add_strings buf [ prefix; selector; "\n" ];
      pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
          pp_declarations ~style ~parent_prefix:child_prefix buf "remove"
            declarations)
  | Content_changed r ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      pp_content_changed ~style ~prefix ~child_prefix buf ~selector:r.selector
        ~old_declarations:r.old_declarations
        ~new_declarations:r.new_declarations
        ~property_changes:r.property_changes
        ~added_properties:r.added_properties
        ~removed_properties:r.removed_properties
  | Selector_changed { old_selector; new_selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      add_strings buf [ prefix; "selector changed:\n" ];
      pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
          let indent = child_indent ~style ~parent_prefix:child_prefix in
          add_strings buf [ indent; "from: "; old_selector; "\n" ];
          add_strings buf [ indent; "to:   "; new_selector; "\n" ];
          if declarations <> [] then
            pp_declarations ~style ~parent_prefix:child_prefix buf
              "declarations" declarations)
  | Reordered r -> (
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      match (r.old_declarations, r.new_declarations) with
      | Some old_decls, Some new_decls ->
          let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
          add_strings buf [ prefix; r.selector; "\n" ];
          pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
              pp_reorder ~style ~parent_prefix:child_prefix old_decls new_decls
                buf)
      | _ ->
          pp_position_reorder ~prefix buf ~selector:r.selector
            ~expected_pos:r.expected_pos ~actual_pos:r.actual_pos
            ~swapped_with:r.swapped_with)
  | Rearranged { selector; declarations } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      add_strings buf [ prefix; selector; " (moved between rules)\n" ];
      pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
          pp_declarations ~style ~parent_prefix:child_prefix buf "same"
            declarations)
  | Regrouped { from_selectors; to_selectors } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      pp_regrouped ~style ~prefix ~child_prefix buf ~from_selectors
        ~to_selectors

let pp_rule_diff_simple buf (diff : rule_diff) =
  match diff with
  | Added { selector; _ } -> add_strings buf [ "Added("; selector; ")" ]
  | Removed { selector; _ } -> add_strings buf [ "Removed("; selector; ")" ]
  | Content_changed { selector; _ } ->
      add_strings buf [ "Changed("; selector; ")" ]
  | Selector_changed { old_selector; new_selector; _ } ->
      add_strings buf
        [ "SelectorChanged("; old_selector; "->"; new_selector; ")" ]
  | Reordered { selector; expected_pos; actual_pos; _ } ->
      add_strings buf
        [
          "Reordered(";
          selector;
          ":";
          string_of_int expected_pos;
          "->";
          string_of_int actual_pos;
          ")";
        ]
  | Rearranged { selector; _ } ->
      add_strings buf [ "Rearranged("; selector; ")" ]
  | Regrouped { from_selectors; to_selectors } ->
      add_strings buf
        [
          "Regrouped(";
          String.concat " | " from_selectors;
          "->";
          String.concat " | " to_selectors;
          ")";
        ]

let meaningful_rules (rules : rule_diff list) =
  List.filter
    (fun (diff : rule_diff) ->
      match diff with
      | Reordered _ -> false
      | Content_changed
          {
            property_changes = [];
            added_properties = [];
            removed_properties = [];
            old_declarations;
            new_declarations;
            _;
          }
        when List.equal Declaration.equal_declaration old_declarations
               new_declarations ->
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
        | Added { container_type = ct; _ }
        | Removed { container_type = ct; _ }
        | Reordered { info = { container_type = ct; _ }; _ }
        | Block_structure_changed { container_type = ct; _ } ->
            if ct = container_type then 1 else 0
        | Modified { info = { container_type = ct; _ }; container_changes; _ }
          ->
            let nested_count =
              count_containers_in_list container_type container_changes
            in
            (if ct = container_type then 1 else 0) + nested_count
      in
      count + this_count)
    0 containers

let count_containers_by_type container_type (diff : t) =
  count_containers_in_list container_type diff.containers

(* [Modified] holds the containers that came and went inside a container that
   survived, so an existence query reads the same tree
   [count_containers_in_list] counts: a flat [List.exists] would answer no about
   a container the count reports, which is an answer about the shape of the diff
   rather than about the stylesheets. *)
let rec exists_container here containers =
  List.exists
    (fun cont ->
      here cont
      ||
      match cont with
      | Modified { container_changes; _ } ->
          exists_container here container_changes
      | Added _ | Removed _ | Reordered _ | Block_structure_changed _ -> false)
    containers

let has_container_added_of_type container_type (diff : t) =
  exists_container
    (function
      | Added { container_type = ct; _ } -> ct = container_type
      | Removed _ | Modified _ | Reordered _ | Block_structure_changed _ ->
          false)
    diff.containers

let has_container_removed_of_type container_type (diff : t) =
  exists_container
    (function
      | Removed { container_type = ct; _ } -> ct = container_type
      | Added _ | Modified _ | Reordered _ | Block_structure_changed _ -> false)
    diff.containers

let container_prefix = function
  | `Media -> "@media"
  | `Layer -> "@layer"
  | `Supports -> "@supports"
  | `Container -> "@container"
  | `Property -> "@property"
  | `Nesting -> "&"
  (* The condition already spells the at-rule out, keyword included. *)
  | `At_rule -> ""

let container_label container_type condition =
  match container_type with
  | `At_rule -> condition
  (* A nesting container is a rule's own block, and the condition names the rule
     it belongs to. Prefixing it as the others are prints [& .a], which is a
     selector matching a [.a] inside the parent, the one thing it is not. *)
  | `Nesting -> String.concat "" [ condition; " { & }" ]
  | container_type ->
      String.concat "" [ container_prefix container_type; " "; condition ]

(* The whole of the [@layer] statement form. The semicolon is what tells a
   layer-order pin from the block of the same name: [@layer a;] ahead of [@layer
   a { ... }] is a second statement, not the block again. *)
let layer_decl_text names =
  String.concat ""
    [
      "@layer ";
      String.concat ", " (List.map Css.Stylesheet.string_of_layer_name names);
      ";";
    ]

(* The statements that carry neither a selector nor a block. Naming them apart
   also keeps them apart in the order keys, where one shared "(other statement)"
   made a [@charset] and a [@namespace] the same statement. *)
let describe_prelude_statement (s : Css.statement) =
  match s with
  | Charset encoding ->
      Some (String.concat "" [ "@charset \""; encoding; "\";" ])
  | Namespace (prefix, _) ->
      Some
        (String.concat ""
           [
             "@namespace";
             (match prefix with
             | Some p -> String.concat "" [ " "; p ]
             | None -> "");
             ";";
           ])
  | Layer_decl names -> Some (layer_decl_text names)
  | _ -> None

(* A statement's own text, split at its block: the head names it, the body is
   what a descriptor-only at-rule is compared on. *)
let statement_text_split stmt =
  let text = Css.Stylesheet.to_string ~minify:true (Css.v [ stmt ]) in
  match String.index_opt text '{' with
  | None -> (String.trim text, "")
  | Some i ->
      let head = String.sub text 0 i in
      let last = String.length text - 1 in
      let body =
        if last > i && text.[last] = '}' then
          String.sub text (i + 1) (last - i - 1)
        else String.sub text (i + 1) (last - i)
      in
      (String.trim head, body)

(* The head of an [@layer]. *)
let layer_block_head name =
  match name with
  | Some name ->
      String.concat "" [ "@layer "; Css.Stylesheet.string_of_layer_name name ]
  | None -> "@layer"

let describe_statement stmt =
  let try_desc f = f stmt in
  let matchers =
    [
      (fun s ->
        Option.map (fun (s, _, _) -> Css.Selector.to_string s) (Css.as_rule s));
      (fun s ->
        Option.map
          (fun (c, _) -> String.concat "" [ "@media "; Css.Media.to_string c ])
          (Css.as_media s));
      (fun s -> Option.map (fun (n, _) -> layer_block_head n) (Css.as_layer s));
      (fun s ->
        Option.map
          (fun (n, c, _) ->
            let prefix =
              match n with Some n -> String.concat "" [ n; " " ] | None -> ""
            in
            let cond_str =
              match c with Some c -> Css.Container.to_string c | None -> ""
            in
            String.concat "" [ "@container "; prefix; cond_str ])
          (Css.as_container s));
      (fun s ->
        Option.map
          (fun (c, _) ->
            String.concat "" [ "@supports "; Css.Supports.to_string c ])
          (Css.as_supports s));
      (fun s -> Option.map (fun _ -> "@property") (Css.as_property s));
      (fun s ->
        Option.map
          (fun (name, _) -> String.concat "" [ "@keyframes "; name ])
          (Css.as_keyframes s));
      (fun s -> Option.map (fun _ -> "@font-face") (Css.as_font_face s));
      describe_prelude_statement;
    ]
  in
  match List.find_map try_desc matchers with
  | Some desc -> Some desc
  (* Everything left names itself by the head it prints to, for the same reason
     [@charset] and [@namespace] are named apart above: one shared description
     is one shared order key, and [@page] swapped with [@starting-style] then
     reads as no change at all. *)
  | None -> (
      match fst (statement_text_split stmt) with
      | "" -> Some "(other statement)"
      | head -> Some head)

(* The body of a container that was added or removed wholesale. Every statement
   gets a line, including the ones [Css.as_rule] cannot see: a statement the
   tree drops silently leaves the reader counting fewer entries than the header
   claims, and shifts the last-child connector onto the wrong one. The body
   comes from the shared reader, so a header never stands over children the walk
   could not name. *)
let statement_children = Css.Stylesheet.statement_children

let rec pp_container_rules ~style ~parent_prefix ~label buf rules =
  let count = List.length rules in
  List.iteri
    (fun i stmt ->
      let is_last = i = count - 1 in
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let desc =
        Option.value (describe_statement stmt) ~default:"(other statement)"
      in
      add_strings buf [ prefix; desc; " ("; label; ")\n" ];
      match statement_children stmt with
      | [] -> ()
      | children ->
          let parent_prefix =
            tree_continuation ~style ~is_last ~parent_prefix
          in
          pp_container_rules ~style ~parent_prefix ~label buf children)
    rules

let count_rule_changes (rule_changes : rule_diff list) =
  let count pred = List.length (List.filter pred rule_changes) in
  let parts =
    List.filter_map Fun.id
      [
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Added _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " added") else None);
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Removed _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " removed") else None);
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Content_changed _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " modified") else None);
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Reordered _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " reordered") else None);
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Rearranged _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " rearranged") else None);
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Selector_changed _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " selector changed") else None);
        (let n =
           count (fun (diff : rule_diff) ->
               match diff with Regrouped _ -> true | _ -> false)
         in
         if n > 0 then Some (string_of_int n ^ " regrouped") else None);
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

(* Position + selector signature for every block that names at least one
   rule. *)
let block_signatures blocks =
  List.filter_map
    (fun (pos, rules) ->
      match selectors_of_rules rules with
      | [] -> None
      | selectors -> Some (pos, String.concat ", " selectors))
    blocks

(* Queue of still-unmatched positions per signature, in ascending order, so
   repeated signatures pair off one-for-one between the two sides. *)
let signature_queues blocks =
  let tbl = Hashtbl.create 64 in
  List.iter
    (fun (pos, sign) ->
      let q = Option.value ~default:[] (Hashtbl.find_opt tbl sign) in
      Hashtbl.replace tbl sign (pos :: q))
    (List.rev blocks);
  tbl

let take_signature tbl sign =
  match Hashtbl.find_opt tbl sign with
  | Some (pos :: rest) ->
      Hashtbl.replace tbl sign rest;
      Some pos
  | Some [] | None -> None

type block_pairing = {
  removed : (int * string) list; (* expected-only blocks *)
  added : (int * string) list; (* actual-only blocks *)
  shifts : (int * int) list; (* (delta, run length), expected order *)
  unchanged : int; (* paired blocks that kept their position *)
}

(* Group consecutive equal deltas so one insertion upstream reads as a single
   run rather than one line per renumbered block. *)
let shift_runs deltas =
  let flush acc = function Some (d, n) -> (d, n) :: acc | None -> acc in
  let acc, current =
    List.fold_left
      (fun (acc, current) d ->
        match current with
        | Some (d', n) when d' = d -> (acc, Some (d, n + 1))
        | _ -> (flush acc current, Some (d, 1)))
      ([], None) deltas
  in
  List.rev (flush acc current)

let pair_blocks ~expected_blocks ~actual_blocks =
  let expected = block_signatures expected_blocks in
  let actual = block_signatures actual_blocks in
  let queues = signature_queues actual in
  let matched = Hashtbl.create 64 in
  let removed, deltas =
    List.fold_left
      (fun (removed, deltas) (pos, sign) ->
        match take_signature queues sign with
        | None -> ((pos, sign) :: removed, deltas)
        | Some actual_pos ->
            Hashtbl.replace matched actual_pos ();
            (removed, (actual_pos - pos) :: deltas))
      ([], []) expected
  in
  let deltas = List.rev deltas in
  {
    removed = List.rev removed;
    added = List.filter (fun (pos, _) -> not (Hashtbl.mem matched pos)) actual;
    shifts = shift_runs (List.filter (fun d -> d <> 0) deltas);
    unchanged = List.length (List.filter (fun d -> d = 0) deltas);
  }

let pp_block_pairing ~style ~child_prefix buf pairing =
  let indent = child_indent ~style ~parent_prefix:child_prefix in
  let pp_block sign style_fn (pos, selectors) =
    add_strings buf
      [
        indent;
        style_fn
          (String.concat ""
             [ sign; " Block at position "; string_of_int pos; ": "; selectors ]);
        "\n";
      ]
  in
  List.iter (pp_block "-" (ansi_red ~color:style.color)) pairing.removed;
  List.iter (pp_block "+" (ansi_green ~color:style.color)) pairing.added;
  List.iter
    (fun (delta, n) ->
      let sign = if delta > 0 then "+" else "-" in
      add_strings buf
        [
          indent;
          string_of_int n;
          (if n = 1 then " block shifted by " else " blocks shifted by ");
          sign;
          string_of_int (abs delta);
          "\n";
        ])
    pairing.shifts;
  if pairing.unchanged > 0 then
    add_strings buf
      [
        indent;
        string_of_int pairing.unchanged;
        (if pairing.unchanged = 1 then " block unchanged\n"
         else " blocks unchanged\n");
      ]

let pp_block_structure_changed ~style ~is_last ~parent_prefix buf
    ~container_type ~condition ~expected_blocks ~actual_blocks =
  let label = container_label container_type condition in
  let prefix = tree_prefix ~style ~is_last ~parent_prefix in
  let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
  let exp_count = List.length expected_blocks in
  let act_count = List.length actual_blocks in
  (* Report block structure changes - this is a meaningful difference even if
     selectors are identical *)
  let summary =
    if exp_count > act_count then
      [
        string_of_int exp_count; " blocks merged into "; string_of_int act_count;
      ]
    else if exp_count < act_count then
      [ string_of_int exp_count; " block split into "; string_of_int act_count ]
    else [ string_of_int exp_count; " blocks at different positions" ]
  in
  add_strings buf ([ prefix; label; " (" ] @ summary);
  Buffer.add_string buf ")\n";
  let pairing = pair_blocks ~expected_blocks ~actual_blocks in
  pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
      pp_block_pairing ~style ~child_prefix buf pairing)

let pp_container_add_remove ~style ~is_last ~parent_prefix ~label buf
    container_type condition rules =
  let prefix = tree_prefix ~style ~is_last ~parent_prefix in
  let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
  add_strings buf
    [ prefix; container_label container_type condition; " ("; label; ")\n" ];
  pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
      pp_container_rules ~style ~parent_prefix:child_prefix ~label buf rules)

let pp_container_reorder ~style ~is_last ~parent_prefix buf container_type
    condition expected_pos actual_pos =
  let prefix = tree_prefix ~style ~is_last ~parent_prefix in
  let label = container_label container_type condition in
  if expected_pos = actual_pos then
    add_strings buf [ prefix; label; " (moved)\n" ]
  else
    add_strings buf
      [
        prefix;
        label;
        " (position ";
        string_of_int expected_pos;
        " \xe2\x86\x92 ";
        string_of_int actual_pos;
        ")\n";
      ]

let rec pp_container_diff ?(style = default_style) ?(is_last = false)
    ?(parent_prefix = "") buf = function
  | Added { container_type; condition; rules } ->
      pp_container_add_remove ~style ~is_last ~parent_prefix ~label:"added" buf
        container_type condition rules
  | Removed { container_type; condition; rules } ->
      pp_container_add_remove ~style ~is_last ~parent_prefix ~label:"removed"
        buf container_type condition rules
  | Modified
      {
        info = { container_type; condition; rules = _ };
        actual_rules = _;
        rule_changes;
        container_changes;
      } ->
      let prefix = tree_prefix ~style ~is_last ~parent_prefix in
      let child_prefix = tree_continuation ~style ~is_last ~parent_prefix in
      let changes_parts = count_rule_changes rule_changes in
      add_strings buf [ prefix; container_label container_type condition; " " ];
      if changes_parts <> [] then
        add_strings buf [ "("; String.concat ", " changes_parts; ")\n" ]
      else if container_changes = [] then
        (* A [Modified] carrying neither a rule nor a container change says only
           that the container differs. Calling it a position change names a
           difference nobody established; [Reordered] is what reports one. *)
        Buffer.add_string buf "(modified, no details)\n"
      else Buffer.add_char buf '\n';
      pp_children ~style ~parent_prefix:child_prefix buf (fun style buf ->
          (* Show rule changes at this level *)
          List.iteri
            (fun i rule_diff ->
              let is_last_item =
                i = List.length rule_changes - 1 && container_changes = []
              in
              pp_rule_diff ~style ~is_last:is_last_item
                ~parent_prefix:child_prefix buf rule_diff)
            rule_changes;
          (* Show nested container changes with increased indentation *)
          let container_count = List.length container_changes in
          List.iteri
            (fun i cont_diff ->
              let is_last_cont = i = container_count - 1 in
              pp_container_diff ~style ~is_last:is_last_cont
                ~parent_prefix:child_prefix buf cont_diff)
            container_changes)
  | Reordered
      { info = { container_type; condition; _ }; expected_pos; actual_pos } ->
      pp_container_reorder ~style ~is_last ~parent_prefix buf container_type
        condition expected_pos actual_pos
  | Block_structure_changed
      { container_type; condition; expected_blocks; actual_blocks } ->
      pp_block_structure_changed ~style ~is_last ~parent_prefix buf
        ~container_type ~condition ~expected_blocks ~actual_blocks

let pp_diff_headers ~color buf expected actual =
  add_strings buf
    [ ansi_yellow ~color "---"; " "; ansi_yellow ~color expected; "\n" ];
  add_strings buf
    [ ansi_yellow ~color "+++"; " "; ansi_yellow ~color actual; "\n" ]

(* [trailing] is how many top-level entries the report still prints after this
   section: the closing connector belongs to the last of them, not to the last
   of a section a breadth limit cut short. *)
let pp_rule_list ~style ~trailing buf rule_list =
  let rule_count = List.length rule_list in
  List.iteri
    (fun i rule_diff ->
      let is_last = i = rule_count - 1 && trailing = 0 in
      pp_rule_diff ~style ~is_last ~parent_prefix:"" buf rule_diff)
    rule_list

let pp_reordered_section ~style ~trailing buf = function
  | [] -> ()
  | lst ->
      add_strings buf
        [ "Rules reordered ("; string_of_int (List.length lst); " rules):\n" ];
      pp_rule_list ~style ~trailing buf lst

let pp_containers_section ~style ~trailing buf containers =
  let container_count = List.length containers in
  List.iteri
    (fun i cont_diff ->
      let is_last = i = container_count - 1 && trailing = 0 in
      pp_container_diff ~style ~is_last ~parent_prefix:"" buf cont_diff)
    containers

(* [Resolve.layer_order] escapes each ident of a path (CSS Syntax 3 (ED) sec.
   2.1), so only a bare [.] separates two of them: one an ident carries is
   written [\.] and stays inside its own segment. *)
let split_layer_path path =
  let len = String.length path in
  let rec go start i acc =
    if i >= len then List.rev (String.sub path start (len - start) :: acc)
    else
      match path.[i] with
      | '\\' -> go start (i + 2) acc
      | '.' -> go (i + 1) (i + 1) (String.sub path start (i - start) :: acc)
      | _ -> go start (i + 1) acc
  in
  go 0 0 []

(* A layer path is keyed for the cascade, not for reading: an anonymous [@layer
   { }] block is a segment starting with U+0000, which a report has to name some
   other way. *)
let layer_path_name path =
  split_layer_path path
  |> List.map (fun segment ->
      if String.length segment > 0 && segment.[0] = '\000' then
        String.concat ""
          [
            "(anonymous "; String.sub segment 1 (String.length segment - 1); ")";
          ]
      else segment)
  |> String.concat "."

let layer_path_list paths = String.concat ", " (List.map layer_path_name paths)

let pp_layer_swaps ~style buf swapped =
  let line ~is_last text =
    add_strings buf
      [ tree_prefix ~style ~is_last ~parent_prefix:""; text; "\n" ]
  in
  List.iteri
    (fun i (weaker, stronger) ->
      if i < max_layer_swaps then
        line ~is_last:false
          (String.concat ""
             [
               layer_path_name stronger;
               " now precedes ";
               layer_path_name weaker;
             ]))
    swapped;
  match List.length swapped - max_layer_swaps with
  | hidden when hidden > 0 ->
      let noun = if hidden = 1 then " more pair\n" else " more pairs\n" in
      add_strings buf
        [
          tree_prefix ~style ~is_last:false ~parent_prefix:"";
          "...";
          string_of_int hidden;
          noun;
        ]
  | _ -> ()

let pp_layer_order_section ~style buf { expected_order; actual_order; swapped }
    =
  Buffer.add_string buf "Cascade layer order changed:\n";
  pp_children ~style ~parent_prefix:"" buf (fun style buf ->
      pp_layer_swaps ~style buf swapped;
      add_strings buf
        [
          tree_prefix ~style ~is_last:true ~parent_prefix:"";
          "order: ";
          ansi_red ~color:style.color (layer_path_list expected_order);
          " -> ";
          ansi_green ~color:style.color (layer_path_list actual_order);
          "\n";
        ])

(* Closes a breadth-limited report the way [pp_layer_swaps] closes a capped swap
   listing: the count the reader needs to know they are seeing a prefix. *)
let pp_hidden_entries ~style buf hidden =
  let noun =
    if hidden = 1 then " more difference\n" else " more differences\n"
  in
  add_strings buf
    [
      tree_prefix ~style ~is_last:true ~parent_prefix:"";
      "...";
      string_of_int hidden;
      noun;
    ]

let first n l = List.filteri (fun i _ -> i < n) l

let pp ?(expected = "Expected") ?(actual = "Actual") ?(color = false)
    ?(depth = unlimited_depth) ?(entries = unlimited_entries) buf
    { rules; containers; layer_order } =
  if rules = [] && containers = [] && Option.is_none layer_order then
    Buffer.add_string buf
      "Structural differences detected in nested contexts (e.g., @media inside \
       @layer)\n\
       but no rule-level differences found.\n\
       This may indicate reordering or subtle changes in rule organization."
  else (
    pp_diff_headers ~color buf expected actual;
    let meaningful = meaningful_rules rules in
    let reordered_rules =
      List.filter
        (fun (diff : rule_diff) ->
          match diff with Reordered _ -> true | _ -> false)
        rules
    in
    (* The budget is spent in printing order, so the entries a reader is left
       with are the ones the report would have led with anyway. *)
    let layer_entries o = if Option.is_none o then 0 else 1 in
    let budget = max 0 entries in
    let shown_layers = if budget > 0 then layer_order else None in
    let budget = budget - layer_entries shown_layers in
    let shown_rules = first budget meaningful in
    let budget = budget - List.length shown_rules in
    let shown_reordered = first budget reordered_rules in
    let budget = budget - List.length shown_reordered in
    let shown_containers = first budget containers in
    let hidden =
      layer_entries layer_order - layer_entries shown_layers
      + (List.length meaningful - List.length shown_rules)
      + (List.length reordered_rules - List.length shown_reordered)
      + (List.length containers - List.length shown_containers)
    in
    (* [depth] counts renderable levels; the roots printed here are level 1. *)
    let style = { tree_style with color; depth = max 0 (depth - 1) } in
    (* The count line closes the tree, so a section the budget cut short is no
       longer the last thing the report prints. *)
    let hidden_line = if hidden > 0 then 1 else 0 in
    let after_rules = List.length shown_containers + hidden_line in
    (* The layer order leads: it decides which of the rules below it wins. *)
    Option.iter (pp_layer_order_section ~style buf) shown_layers;
    pp_rule_list ~style ~trailing:after_rules buf shown_rules;
    pp_reordered_section ~style ~trailing:after_rules buf shown_reordered;
    pp_containers_section ~style ~trailing:hidden_line buf shown_containers;
    if hidden > 0 then pp_hidden_entries ~style buf hidden)

(* ===== Tree Diff Computation Functions ===== *)

(* The text a statement prints to, cut at its block, for the statements no
   selector names: [@charset "UTF-8";], [@layer a, b;], [@namespace ...]. An
   entry with no name cannot be classified, so it corrupts every count the
   summary derives from the same list, and it renders as a bare tree
   connector. *)
let statement_head stmt =
  let text =
    Css.Stylesheet.to_string ~minify:true (Css.v [ stmt ]) |> String.trim
  in
  let head =
    match String.index_opt text '{' with
    | Some i -> String.trim (String.sub text 0 i)
    | None -> text
  in
  if head = "" then
    Option.value ~default:"(other statement)" (describe_statement stmt)
  else head

(* Helper to extract rule information from statements *)
let strings_of_rule (stmt : Css.statement) =
  match Css.as_rule stmt with
  | Some (selector, decls, _) ->
      let selector_str = Css.Selector.to_string selector in
      (selector_str, decls)
  (* CSS Nesting 1 sec. 3.4: a run written after a nested statement is a nested
     declarations rule, which acts as [&]. It has no head to cut at, so the text
     up to the first brace is its first declaration, which reads in the selector
     column as a selector it is not. *)
  | None -> (
      match stmt with
      | Declarations decls -> ("&", decls)
      | _ -> (statement_head stmt, Css.Stylesheet.statement_declarations stmt))

let decl_to_prop_value decl =
  ( Css.declaration_name decl,
    decl_with_important decl (Css.declaration_value_for_equivalence decl) )

(* What a changed declaration puts in the report: the key answers whether the
   two sides differ, and the author's own spelling is what the reader is shown.
   Printing the key instead quotes a value neither file holds, since the key
   folds the spellings the two sides chose onto one. *)
let decl_to_reported_value decl =
  let name, key = decl_to_prop_value decl in
  (name, (key, decl_shown_value decl))

let compare_prop_value (name1, value1) (name2, value2) =
  let by_name = String.compare name1 name2 in
  if by_name <> 0 then by_name else String.compare value1 value2

let equal_prop_value (name1, value1) (name2, value2) =
  String.equal name1 name2 && String.equal value1 value2

let decls_signature (decls : Css.declaration list) =
  List.map decl_to_prop_value decls |> List.sort compare_prop_value

let equal_decls_signature = List.equal equal_prop_value

(* Normalize a selector string by sorting comma-separated selector items. This
   ensures we consider ".a,.b" equivalent to ".b,.a" when matching.

   Policy: Selector lists with the same items in different orders are considered
   equivalent for matching purposes. This means: - ".a, .b" and ".b, .a" will
   match as the same selector - Reordering within a list is not considered a
   structural change - This prevents false positives when CSS tools reorder
   selector lists *)
let rule_selector stmt =
  match Css.statement_selector stmt with
  | Some s -> s
  | None -> Css.Selector.universal

(* [selector_key_of_*] is called O(N M) times during structural rule diffs. Use
   the typed selector AST as the key: normalise a [List] of selectors by sorting
   the alternatives so [h1, h2] and [h2, h1] map to the same key, then rely on
   structural equality + [Hashtbl.hash]. Avoids serialising through
   [Pp.to_string] for every comparison. *)
let selector_key_of_selector (sel : Css.Selector.t) : Css.Selector.t =
  match sel with
  | List subs -> List (List.sort Selector.compare subs)
  | _ -> sel

let selector_key_of_stmt stmt = selector_key_of_selector (rule_selector stmt)
let rule_declarations = Css.Stylesheet.statement_declarations

let rule_nested stmt =
  match Css.as_rule stmt with Some (_, _, nested) -> nested | None -> []

(* A container takes part in the ordering comparison too: swapping a rule with
   an [@media] that writes the same property flips the cascade winner. *)
type order_key = Rule_order of Css.Selector.t | Block_order of string

let order_key_of_stmt stmt =
  match Css.as_rule stmt with
  | Some (sel, _, _) -> Some (Rule_order (selector_key_of_selector sel))
  | None -> Option.map (fun desc -> Block_order desc) (describe_statement stmt)

(* Rules take part once per selector: a selector split over several rules moves
   as one cascade participant. Containers do not have that identity. Separate
   blocks under the same condition can sit on opposite sides of another rule, so
   number their occurrences instead of collapsing them onto the first. *)
let order_keys_in_order stmts =
  let seen_rules = Hashtbl.create (List.length stmts) in
  let block_occurrences = Hashtbl.create 8 in
  List.filter_map
    (fun stmt ->
      match order_key_of_stmt stmt with
      | Some (Rule_order _ as key) when not (Hashtbl.mem seen_rules key) ->
          Hashtbl.add seen_rules key ();
          Some (key, 0)
      | Some (Block_order _ as key) ->
          let occurrence =
            Option.value ~default:0 (Hashtbl.find_opt block_occurrences key)
          in
          Hashtbl.replace block_occurrences key (occurrence + 1);
          Some (key, occurrence)
      | Some (Rule_order _) | None -> None)
    stmts

(* Positions of one longest increasing subsequence of [ranks], by patience
   sorting: [tails.(l)] is the position ending the smallest subsequence of
   length [l + 1] seen so far, and [prev] chains each position to its
   predecessor. *)
let increasing_subsequence ranks =
  let n = Array.length ranks in
  let tails = Array.make n 0 in
  let prev = Array.make n (-1) in
  let len = ref 0 in
  for i = 0 to n - 1 do
    let lo = ref 0 and hi = ref !len in
    while !lo < !hi do
      let mid = (!lo + !hi) / 2 in
      if ranks.(tails.(mid)) < ranks.(i) then lo := mid + 1 else hi := mid
    done;
    let pos = !lo in
    prev.(i) <- (if pos > 0 then tails.(pos - 1) else -1);
    tails.(pos) <- i;
    if pos = !len then incr len
  done;
  let members = Array.make n false in
  (if !len > 0 then
     let i = ref tails.(!len - 1) in
     while !i >= 0 do
       members.(!i) <- true;
       i := prev.(!i)
     done);
  members

(* Statements whose order against the rest actually inverted. Judged on the
   statements both sides share, since one the other side never had shifts every
   absolute position after it without transposing anything: comparing positions
   on each side reports the whole tail of the stylesheet as reordered whenever a
   rule is added or dropped. Anchoring on a longest order-preserving matching of
   that common sequence also keeps one move to one entry, where comparing rank
   pairwise would name every statement the move passed. *)
let moved_order_keys stmts1 stmts2 =
  let keys2 = order_keys_in_order stmts2 in
  let rank2 = Hashtbl.create (List.length keys2) in
  List.iteri (fun i key -> Hashtbl.replace rank2 key i) keys2;
  let common = List.filter (Hashtbl.mem rank2) (order_keys_in_order stmts1) in
  let ranks = Array.of_list (List.map (Hashtbl.find rank2) common) in
  let anchored = increasing_subsequence ranks in
  let moved = Hashtbl.create (Array.length ranks) in
  List.iteri
    (fun i (key, _) -> if not anchored.(i) then Hashtbl.replace moved key ())
    common;
  moved

let selector_moved moved sel = Hashtbl.mem moved (Rule_order sel)

(* Generic helper for finding added/removed/modified items between two lists.
   [key_of] returns a canonical structural key. *)
let diffs ~(key_of : 'item -> 'key) ~(is_empty_diff : 'item -> 'item -> bool)
    items1 items2 =
  let items1_keyed = List.map (fun i -> (i, key_of i)) items1 in
  let items2_keyed = Array.of_list (List.map (fun i -> (i, key_of i)) items2) in
  (* A FIFO per key gives duplicate keys the same source-order pairing as the
     old left-to-right scan, without restarting that scan for every item. *)
  let available = Hashtbl.create (Array.length items2_keyed) in
  Array.iteri
    (fun i (item, key) ->
      let bucket =
        match Hashtbl.find_opt available key with
        | Some bucket -> bucket
        | None ->
            let bucket = Queue.create () in
            Hashtbl.add available key bucket;
            bucket
      in
      Queue.add (i, item) bucket)
    items2_keyed;
  let claimed = Array.make (Array.length items2_keyed) false in
  let claim key =
    match Hashtbl.find_opt available key with
    | None -> None
    | Some bucket -> (
        match Queue.take_opt bucket with
        | None -> None
        | Some (i, item) ->
            claimed.(i) <- true;
            Some item)
  in
  let pairs, removed =
    List.fold_left
      (fun (pairs, removed) (item1, key1) ->
        match claim key1 with
        | Some item2 -> ((item1, item2) :: pairs, removed)
        | None -> (pairs, item1 :: removed))
      ([], []) items1_keyed
  in
  let pairs = List.rev pairs and removed = List.rev removed in
  let added =
    Array.to_list items2_keyed
    |> List.filteri (fun i _ -> not claimed.(i))
    |> List.map fst
  in
  let modified =
    List.filter (fun (item1, item2) -> not (is_empty_diff item1 item2)) pairs
  in
  (added, removed, modified)

let rules_added_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let is_empty_diff _ _ = true in
  let added, _removed, _modified = diffs ~key_of ~is_empty_diff rules1 rules2 in
  added

let rules_removed_diff rules1 rules2 =
  let key_of = selector_key_of_stmt in
  let is_empty_diff _ _ = true in
  let _added, removed, _modified = diffs ~key_of ~is_empty_diff rules1 rules2 in
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
  (* Buckets were built by prepending. Put them back in source order so every
     equal-quality choice below has a stable positional tie-break. *)
  Hashtbl.iter
    (fun key rules -> Hashtbl.replace rules2_by_key key (List.rev rules))
    rules2_by_key;
  Hashtbl.iter
    (fun props rules -> Hashtbl.replace rules2_by_props props (List.rev rules))
    rules2_by_props;
  (rules2_by_key, rules2_by_props)

(* A rule paired with its partner on the other side: the two selectors and the
   two declaration lists, in the order the rule matcher hands them on. *)
type rule_modification =
  Css.Selector.t * Css.Selector.t * Css.declaration list * Css.declaration list

(* What an exact match reports. It usually says nothing: the same selector
   carrying the same declarations is the same rule. One exception: the rule sits
   somewhere else against the rest of the sheet, which is a cascade change
   however identical the block is, so the pair has to reach
   [convert_modified_rule] for the reorder entry to be made. *)
type exact_match = Same | Moved of rule_modification

(* Try to find an exact match by selector key and declarations. [None] when
   nothing on the other side matches exactly. *)
let try_exact_match ~moved rules2_by_key used_rules r1 key1 d1 =
  let candidates = try Hashtbl.find rules2_by_key key1 with Not_found -> [] in
  match
    List.find_opt
      (fun r ->
        (not (Hashtbl.mem used_rules r))
        && List.equal Declaration.equal_declaration (rule_declarations r) d1
        && Stylesheet.equal (rule_nested r) (rule_nested r1))
      candidates
  with
  | Some exact ->
      Hashtbl.replace used_rules exact ();
      let sel1 = rule_selector r1 in
      let sel2 = rule_selector exact in
      if selector_moved moved key1 then Some (Moved (sel1, sel2, d1, d1))
      else Some Same
  | None -> None

(* The multiset of properties a rule writes. Values are deliberately absent: two
   repeated occurrences that both write [color] stay tied and source order
   exposes a changed last winner instead of pairing equal values across the
   sheet. Repeated properties remain repeated in the footprint. *)
let declaration_footprint declarations =
  List.map Css.declaration_name declarations |> List.sort String.compare

(* The size of the symmetric multiset difference between two sorted declaration
   footprints. *)
let rec footprint_distance one other =
  match (one, other) with
  | [], rest | rest, [] -> List.length rest
  | name1 :: rest1, name2 :: rest2 ->
      let order = String.compare name1 name2 in
      if order = 0 then footprint_distance rest1 rest2
      else if order < 0 then 1 + footprint_distance rest1 other
      else 1 + footprint_distance one rest2

(* Find the unused same-key rule with the closest declaration footprint. Buckets
   and this fold are in source order, so the earlier occurrence wins a tie. *)
let try_same_key_match rules2_by_key used_rules r1 key1 d1 =
  let candidates = try Hashtbl.find rules2_by_key key1 with Not_found -> [] in
  let footprint1 = declaration_footprint d1 in
  let closest =
    List.fold_left
      (fun best r ->
        if Hashtbl.mem used_rules r then best
        else
          let distance =
            footprint_distance footprint1
              (declaration_footprint (rule_declarations r))
          in
          match best with
          | None -> Some (distance, r)
          | Some (best_distance, _) when distance < best_distance ->
              Some (distance, r)
          | Some _ -> best)
      None candidates
    |> Option.map snd
  in
  match closest with
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

let pick_non_exact_rule rules2_by_key rules2_by_props used_rules r1 key1 d1
    props1 =
  match try_same_key_match rules2_by_key used_rules r1 key1 d1 with
  | Some result -> Some result
  | None -> try_equivalent_props_match rules2_by_props used_rules r1 d1 props1

(* The pairs the rules of [rules1] make with [rules2], as [(moved, changed)]:
   the rules that only changed places, and the rules whose content or selector
   differs. They are kept apart because only the second answers whether the two
   sheets differ structurally, which is what decides how the ordering is read
   back. *)
let rules_modified_diff ~moved rules1 rules2 =
  let rules2_by_key, rules2_by_props = build_rule_lookup_tables rules2 in
  let used_rules = Hashtbl.create (List.length rules2) in
  (* Claim every exact match first, wherever it sits. Matching in one greedy
     pass lets an early rule take, through the property fallback, the partner a
     later rule matches exactly, so a page of [.to-*] gradient rules, all with
     the same property signature, pairs off by position and every one reports as
     modified. *)
  let exact, pending =
    List.partition_map
      (fun r1 ->
        let key1 = selector_key_of_stmt r1 in
        let d1 = rule_declarations r1 in
        match try_exact_match ~moved rules2_by_key used_rules r1 key1 d1 with
        | Some pick -> Left pick
        | None -> Right r1)
      rules1
  in
  let moved_exact =
    List.filter_map (function Moved pair -> Some pair | _ -> None) exact
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | r1 :: t1 ->
        let key1 = selector_key_of_stmt r1 in
        let d1 = rule_declarations r1 in
        let props1 = decls_signature d1 in
        let pick =
          pick_non_exact_rule rules2_by_key rules2_by_props used_rules r1 key1
            d1 props1
        in
        let acc = match pick with None -> acc | Some x -> x :: acc in
        aux acc t1
  in
  (moved_exact, aux [] pending)

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

(* The conditions of the containers that changed places against the rest of the
   enclosing statement list, judged on the same key space as the rule ordering.
   The absolute index a container sits at is not that coordinate: one statement
   inserted ahead of a block renumbers it and everything after it without
   transposing anything, which is why the index comparisons this replaces
   carried a slack distance and still answered the wrong question on both sides
   of it. [moved_order_keys] anchors on a longest order-preserving matching of
   the statements both sides share, so an insertion moves nothing at any
   distance and a block that swapped with a rule or another block moves even by
   one position. *)
let moved_conditions ~condition_of stmts1 stmts2 =
  let moved = moved_order_keys stmts1 stmts2 in
  let conds = Hashtbl.create 8 in
  List.iter
    (fun stmt ->
      match (condition_of stmt, order_key_of_stmt stmt) with
      | Some cond, Some key when Hashtbl.mem moved key ->
          Hashtbl.replace conds cond ()
      | _ -> ())
    stmts1;
  conds

(* Locate matching declarations in map2 for a given selector key *)
let matching_decls_in_map2 sel1_key decls1 map2 decls2 =
  (* Prefer an exact declaration match for the same selector key if available *)
  match
    List.find_opt
      (fun (s, d) ->
        Selector.equal (selector_key_of_selector s) sel1_key
        && List.equal Declaration.equal_declaration d decls1)
      map2
  with
  | Some (s, d) -> (d, Some s)
  | None -> (
      match
        List.find_opt
          (fun (s, _) -> Selector.equal (selector_key_of_selector s) sel1_key)
          map2
      with
      | Some (s, d) -> (d, Some s)
      | None -> (decls2, None))

let add_ordering_issue ~moved map2 acc sel1 decls1 sel2 decls2 =
  let sel1_key = selector_key_of_selector sel1 in
  let sel2_key = selector_key_of_selector sel2 in
  if Selector.equal sel1_key sel2_key then
    (* Same selector at this position: a difference only when its declarations
       differ, i.e. same-selector rules were reordered so the cascade winner
       flips. *)
    if equal_decls_signature (decls_signature decls1) (decls_signature decls2)
    then acc
    else (sel1, sel2, decls1, decls2) :: acc
  else if selector_moved moved sel1_key then
    (* Report the selector that moved, not the ones it displaced: a rule pulled
       to the front sits opposite a different selector at every position it
       passed, and pairing on that named each of them instead. *)
    let decls1_from_map2, sel2_opt =
      matching_decls_in_map2 sel1_key decls1 map2 decls2
    in
    let sel2 = match sel2_opt with Some s -> s | None -> sel1 in
    (sel1, sel2, decls1, decls1_from_map2) :: acc
  else acc

(* no-op: pure rule ordering is handled in handle_structural_diff via
   has_ordering_changes/ordering_diff *)

let ordering_diff ~moved rules1 rules2 =
  let map1 = build_selector_map rules1 in
  let map2 = build_selector_map rules2 in

  let rec find_ordering_issues acc remaining1 remaining2 =
    match (remaining1, remaining2) with
    | [], [] -> List.rev acc
    | (sel1, decls1) :: rest1, (sel2, decls2) :: rest2 ->
        let acc = add_ordering_issue ~moved map2 acc sel1 decls1 sel2 decls2 in
        find_ordering_issues acc rest1 rest2
    | _, _ -> List.rev acc
  in

  find_ordering_issues [] map1 map2

let extract_base_parent_selector sel =
  let sel_str = Css.Selector.to_string sel in
  match String.index_opt sel_str ' ' with
  | None -> None
  | Some sp ->
      let parent = String.sub sel_str 0 sp in
      let stripped =
        match String.index_opt parent ':' with
        | Some idx -> String.sub parent 0 idx
        | None -> parent
      in
      Some stripped

let selectors_share_parent_ast sel1 sel2 =
  match
    (extract_base_parent_selector sel1, extract_base_parent_selector sel2)
  with
  | Some p1, Some p2 -> p1 = p2
  | _ -> false

let selector_changes all_added_candidates all_removed_candidates =
  (* Index added rules by their declaration signature so the inner loop is a
     hashtable lookup, not a linear scan over [all_added_candidates]. With N
     removed and M added rules, the previous shape was O(N M) [decls_signature]
     computations; now it's O(N + M) plus the per-bucket scan for the
     share-parent check (buckets are typically small). *)
  let added_by_props : (string list, Css.statement list) Hashtbl.t =
    Hashtbl.create (List.length all_added_candidates)
  in
  List.iter
    (fun added ->
      let props = decls_signature (rule_declarations added) |> List.map snd in
      let prev =
        Hashtbl.find_opt added_by_props props |> Option.value ~default:[]
      in
      Hashtbl.replace added_by_props props (added :: prev))
    all_added_candidates;
  let added_with_props_sig sig_strings =
    Hashtbl.find_opt added_by_props sig_strings |> Option.value ~default:[]
  in
  let matched_added = ref [] in
  let matched_removed = ref [] in
  let changes = ref [] in
  List.iter
    (fun removed_rule ->
      let removed_sel = rule_selector removed_rule in
      let removed_decls = rule_declarations removed_rule in
      let removed_props = decls_signature removed_decls |> List.map snd in
      let matching_added =
        List.find_opt
          (fun added_rule ->
            let added_sel = rule_selector added_rule in
            (not (Selector.equal removed_sel added_sel))
            && selectors_share_parent_ast removed_sel added_sel)
          (added_with_props_sig removed_props)
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
let exclude_modified_selector_changes sel_changes other_modified =
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

(* The single selectors and declaration signature of a flat rule (no nested
   body); [None] for any other statement. *)
let flat_rule_parts stmt =
  match Css.as_rule stmt with
  | Some (sel, decls, []) ->
      let subs = match sel with List subs -> subs | s -> [ s ] in
      Some (decls, subs, decls_signature decls)
  | _ -> None

let grouping_pair_count rules =
  let h = Hashtbl.create 16 in
  List.iter
    (fun stmt ->
      match flat_rule_parts stmt with
      | Some (_, subs, sign) ->
          List.iter
            (fun sub ->
              let p = (selector_key_of_selector sub, sign) in
              Hashtbl.replace h p
                (1 + Option.value ~default:0 (Hashtbl.find_opt h p)))
            subs
      | None -> ())
    rules;
  h

(* Drop each selector whose pair the [common] budget still covers; keep the rule
   unchanged when none drop, trim it to the survivors otherwise, remove it when
   all drop. *)
let trim_reconciled_grouping common rules =
  let budget = Hashtbl.copy common in
  List.filter_map
    (fun stmt ->
      match flat_rule_parts stmt with
      | Some (decls, subs, sign) ->
          let kept =
            List.filter
              (fun sub ->
                let p = (selector_key_of_selector sub, sign) in
                match Hashtbl.find_opt budget p with
                | Some n when n > 0 ->
                    Hashtbl.replace budget p (n - 1);
                    false
                | _ -> true)
              subs
          in
          if kept = [] then None
          else if List.compare_lengths kept subs = 0 then Some stmt
          else
            let selector =
              match kept with [ s ] -> s | many -> Css.Selector.list many
            in
            Some (Css.rule ~selector decls)
      | None -> Some stmt)
    rules

(* A comma-grouped rule split or merged across rules with identical declarations
   ([.a, .b { x }] vs [.a { x } .b { x }]) is not a semantic change: the same
   [(single selector, declarations)] pairs survive, only regrouped. Reconcile
   the leftover add/remove candidates at the pair level so the regrouping does
   not read as add/remove noise - a pair on both sides is unchanged and drops
   from each, trimming the rule's selector list, or dropping the rule when no
   selector survives. Restricted to flat rules: a nested rule's [(selector,
   declarations)] pair does not capture its nested body. *)
let partial_trim added removed =
  let added_count = grouping_pair_count added in
  let removed_count = grouping_pair_count removed in
  let common = Hashtbl.create 16 in
  Hashtbl.iter
    (fun p ac ->
      match Hashtbl.find_opt removed_count p with
      | Some rc -> Hashtbl.replace common p (min ac rc)
      | None -> ())
    added_count;
  if Hashtbl.length common = 0 then (added, removed)
  else
    ( trim_reconciled_grouping common added,
      trim_reconciled_grouping common removed )

let rule_sig stmt = Option.map (fun (_, _, s) -> s) (flat_rule_parts stmt)

let rule_selector_str stmt =
  Option.map (fun (s, _, _) -> Css.Selector.to_string s) (Css.as_rule stmt)

(* A declaration signature is a pure regroup when its removed and added flat
   rules carry the same multiset of single selectors (only the grouping moved).
   Emit a [Regrouped] note for it; the rules are dropped from add/remove. *)
let detect_pure_regroups added removed =
  let with_sig s rules = List.filter (fun r -> rule_sig r = Some s) rules in
  let single_keys rules =
    List.concat_map
      (fun r ->
        match flat_rule_parts r with
        | Some (_, subs, _) -> List.map selector_key_of_selector subs
        | None -> [])
      rules
    |> List.sort Selector.compare
  in
  List.filter_map rule_sig (added @ removed)
  |> List.sort_uniq compare
  |> List.filter_map (fun s ->
      let radd = with_sig s added and rrem = with_sig s removed in
      if
        radd <> [] && rrem <> []
        && List.equal Selector.equal (single_keys radd) (single_keys rrem)
      then
        Some
          ( s,
            (Regrouped
               {
                 from_selectors = List.filter_map rule_selector_str rrem;
                 to_selectors = List.filter_map rule_selector_str radd;
               }
              : rule_diff) )
      else None)

let reconcile_selector_grouping added removed =
  let pure = detect_pure_regroups added removed in
  let pure_sigs = List.map fst pure in
  let in_pure r =
    match rule_sig r with Some s -> List.mem s pure_sigs | None -> false
  in
  let added = List.filter (fun r -> not (in_pure r)) added in
  let removed = List.filter (fun r -> not (in_pure r)) removed in
  let added, removed = partial_trim added removed in
  (added, removed, List.map snd pure)

(* Key reorder detection uses both selector and declarations: two same-selector
   rules with conflicting declarations cascade last-wins. *)
let order_signature stmts =
  List.map
    (fun stmt ->
      (selector_key_of_stmt stmt, decls_signature (rule_declarations stmt)))
    stmts

let equal_order_signature =
  List.equal (fun (selector1, declarations1) (selector2, declarations2) ->
      Selector.equal selector1 selector2
      && equal_decls_signature declarations1 declarations2)

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
  let added, removed, regrouped = reconcile_selector_grouping added removed in

  let moved = moved_order_keys rules1 rules2 in
  let moved_exact, other_modified = rules_modified_diff ~moved rules1 rules2 in
  let filtered_other_modified =
    exclude_modified_selector_changes sel_changes other_modified
  in

  let modified = sel_changes @ filtered_other_modified in

  let has_structural_changes =
    added <> [] || removed <> [] || modified <> [] || regrouped <> []
  in
  let has_ordering_changes =
    (not has_structural_changes)
    && has_same_selectors rules1 rules2
    && not
         (equal_order_signature (order_signature rules1)
            (order_signature rules2))
  in

  (* With nothing else to report, the two sides line up position by position and
     [ordering_diff] can also name a swap of two rules that share a selector,
     which no order key distinguishes. Once something else did change that walk
     no longer lines up, and the rules that only changed places are the ones the
     order keys name. *)
  let modified_with_order =
    if has_ordering_changes then ordering_diff ~moved rules1 rules2 @ modified
    else moved_exact @ modified
  in

  (added, removed, modified_with_order, regrouped)

let rule_diffs rules1 rules2 = handle_structural_diff rules1 rules2

(* Each key and shown value a rule writes for [name], in the order it writes
   them. *)
let occurrences_of name props =
  List.filter_map (fun (p, v) -> if p = name then Some v else None) props

(* The property names of [props], each once, in first-appearance order. *)
let names_of props =
  List.fold_left
    (fun acc (p, _) -> if List.mem p acc then acc else p :: acc)
    [] props
  |> List.rev

(* Zip one name's occurrence lists. A rule may write a property several times -
   a fallback chain is the usual reason - so occurrence n on one side answers
   occurrence n on the other, and whichever side has more occurrences carries
   the surplus. Matching by name alone binds every occurrence to the first entry
   opposite and reports values neither side holds. *)
let rec zip_occurrences name (modified, added, removed) values1 values2 =
  match (values1, values2) with
  | [], [] -> (modified, added, removed)
  | (key1, shown1) :: rest1, (key2, shown2) :: rest2 ->
      let modified =
        if String.equal key1 key2 then modified
        else
          {
            property_name = name;
            expected_value = shown1;
            actual_value = shown2;
          }
          :: modified
      in
      zip_occurrences name (modified, added, removed) rest1 rest2
  | (_, shown) :: rest1, [] ->
      zip_occurrences name (modified, added, (name, shown) :: removed) rest1 []
  | [], (_, shown) :: rest2 ->
      zip_occurrences name (modified, (name, shown) :: added, removed) [] rest2

(* Helper function to compute property diffs between two declaration lists,
   including added and removed properties *)
let properties_diff decls1 decls2 :
    declaration list * (string * string) list * (string * string) list =
  let props1 = List.map decl_to_reported_value decls1 in
  let props2 = List.map decl_to_reported_value decls2 in
  (* Names the expected side writes first, then the ones only the actual side
     writes, so the report reads in source order. *)
  let names =
    let names1 = names_of props1 in
    names1 @ List.filter (fun p -> not (List.mem p names1)) (names_of props2)
  in
  let modified, added, removed =
    List.fold_left
      (fun acc name ->
        zip_occurrences name acc
          (occurrences_of name props1)
          (occurrences_of name props2))
      ([], [], []) names
  in
  (List.rev modified, List.rev added, List.rev removed)

(* Helper functions for converting rule changes - moved here for mutual
   recursion *)
(* A container still takes part in the ordering comparison, since swapping a
   rule with an [@media] is cascade-significant, but [container_changes] is what
   reports it. Converting it here as well gives a second entry for the same
   block. *)
let is_container_statement stmt =
  Css.as_media stmt <> None
  || Css.as_supports stmt <> None
  || Css.as_layer stmt <> None
  || Css.as_container stmt <> None

let convert_added_rule stmt =
  if is_container_statement stmt then None
  else
    let sel, decls = strings_of_rule stmt in
    Some (Added { selector = sel; declarations = decls } : rule_diff)

let convert_removed_rule stmt =
  if is_container_statement stmt then None
  else
    let sel, decls = strings_of_rule stmt in
    Some (Removed { selector = sel; declarations = decls } : rule_diff)

let selector_position sel rules =
  let sel_key = selector_key_of_selector sel in
  List.mapi
    (fun i stmt ->
      match Css.as_rule stmt with
      | Some (s, _, _) when Selector.equal (selector_key_of_selector s) sel_key
        ->
          Some i
      | _ -> None)
    rules
  |> List.find_map Fun.id |> Option.value ~default:(-1)

let selector_at_position pos rules =
  Option.bind (List.nth_opt rules pos) describe_statement

let content_changed selector old_decls new_decls =
  let property_changes, added_props, removed_props =
    properties_diff old_decls new_decls
  in
  Content_changed
    {
      selector;
      old_declarations = old_decls;
      new_declarations = new_decls;
      property_changes;
      added_properties = added_props;
      removed_properties = removed_props;
    }

let reordered ~rules1 ~rules2 sel1 sel2 selector : rule_diff =
  let expected_pos = selector_position sel1 rules1 in
  let actual_pos = selector_position sel2 rules2 in
  let swapped_with = selector_at_position expected_pos rules2 in
  (Reordered
     {
       selector;
       expected_pos;
       actual_pos;
       swapped_with;
       old_declarations = None;
       new_declarations = None;
     }
    : rule_diff)

(* The change is reported under [sel1], so [sel1] is what has to have moved. *)
let position_changed ~moved sel1 =
  selector_moved moved (selector_key_of_selector sel1)

let is_pure_decl_reordering decls1 decls2 =
  let property_changes, added_props, removed_props =
    properties_diff decls1 decls2
  in
  let pure =
    property_changes = [] && added_props = [] && removed_props = []
    && equal_decls_signature (decls_signature decls1) (decls_signature decls2)
  in
  (pure, property_changes, added_props, removed_props)

let decl_level_reorder selector decls1 decls2 : rule_diff =
  (Reordered
     {
       selector;
       expected_pos = -1;
       actual_pos = -1;
       swapped_with = None;
       old_declarations = Some decls1;
       new_declarations = Some decls2;
     }
    : rule_diff)

let decls_str_equal d1 d2 =
  List.length d1 = List.length d2
  && List.for_all2
       (fun x y -> decl_to_prop_value x = decl_to_prop_value y)
       d1 d2

let convert_modified_rule ~moved ~rules1 ~rules2 (sel1, sel2, decls1, decls2) =
  let sel1_str = Css.Selector.to_string sel1 in
  let sel2_str = Css.Selector.to_string sel2 in
  let position_changed () = position_changed ~moved sel1 in
  let reordered selector = reordered ~rules1 ~rules2 sel1 sel2 selector in
  let reorder_or_content selector d1 d2 =
    if position_changed () then Some (reordered selector)
    else Some (content_changed selector d1 d2)
  in

  (* Handle each modification case *)
  match (decls1, decls2) with
  | [], [] -> reorder_or_content sel1_str decls1 decls2
  | [], _ | _, [] -> Some (content_changed sel1_str decls1 decls2)
  (* The selector key ignores the order of a comma group, so the same
     alternatives written the other way round select the same elements with the
     same specificities and the rule is left alone. *)
  | _, _
    when not
           (Selector.equal
              (selector_key_of_selector sel1)
              (selector_key_of_selector sel2)) ->
      Some
        (Selector_changed
           {
             old_selector = sel1_str;
             new_selector = sel2_str;
             declarations = decls2;
           })
  | _, _ when List.equal Declaration.equal_declaration decls1 decls2 ->
      reorder_or_content sel1_str decls1 decls2
  | _, _ ->
      let pure, property_changes, added_props, removed_props =
        is_pure_decl_reordering decls1 decls2
      in
      if pure then
        if position_changed () then Some (reordered sel1_str)
        else if decls_str_equal decls1 decls2 then
          (* OCaml ASTs differ but string output is identical (e.g., Nested vs
             bare expression after calc() normalization) -- no real
             difference *)
          None
        else if reorder_is_significant decls1 decls2 then
          Some (decl_level_reorder sel1_str decls1 decls2)
        else (* cascade-neutral reorder of disjoint declarations *) None
      else if property_changes <> [] || added_props <> [] || removed_props <> []
      then Some (content_changed sel1_str decls1 decls2)
      else reorder_or_content sel1_str decls1 decls2

(* Assemble rule changes (added/removed/modified) between two rule lists *)

(* The selector a change is about, when it names one. *)
let changed_selector : rule_diff -> string option = function
  | Added { selector; _ }
  | Removed { selector; _ }
  | Content_changed { selector; _ } ->
      Some selector
  | Rearranged { selector; _ } -> Some selector
  | Reordered _ | Selector_changed _ | Regrouped _ -> None

let change_sides : rule_diff -> Css.declaration list * Css.declaration list =
  function
  | Added { declarations; _ } -> ([], declarations)
  | Removed { declarations; _ } -> (declarations, [])
  | Content_changed { old_declarations; new_declarations; _ } ->
      (old_declarations, new_declarations)
  | _ -> ([], [])

(* A property arriving and a property leaving, judged on what the entry names
   rather than on which constructor it is: a [Content_changed] that only
   restates a value, or that names nothing at all, moves no declaration. *)
let change_gains : rule_diff -> bool = function
  | Added _ -> true
  | Content_changed { added_properties; _ } -> added_properties <> []
  | _ -> false

let change_loses : rule_diff -> bool = function
  | Removed _ -> true
  | Content_changed { removed_properties; _ } -> removed_properties <> []
  | _ -> false

(* Every declaration [sel] writes on one side, across all of its rules. *)
let declarations_of_selector sel stmts =
  List.concat_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, decls, _) when Css.Selector.to_string selector = sel ->
          decls
      | _ -> [])
    stmts

(* Judge on every rule of the selector, not only the differing ones: a
   declaration a matching rule already carries distinguishes a move from a
   loss. *)
let merge_selector_group ~rules1 ~rules2 sel peers =
  let old_all = declarations_of_selector sel rules1
  and new_all = declarations_of_selector sel rules2 in
  if
    old_all <> []
    && equal_decls_signature (decls_signature old_all) (decls_signature new_all)
  then Rearranged { selector = sel; declarations = new_all }
  else
    content_changed sel
      (List.concat_map (fun d -> fst (change_sides d)) peers)
      (List.concat_map (fun d -> snd (change_sides d)) peers)

(* One selector, one node. Two rules writing the same selector in a container
   produced two sibling entries under the same label, one reporting a
   declaration added and the other a different one removed, which reads as a
   contradiction rather than as a declaration moving between them. The group
   collapses to a single before-and-after for that selector.

   Only a group that both gains and loses collapses: several rules added under
   one selector really are several additions, and merging those would hide the
   count. A single entry carries both sides of that on its own, and does when a
   container sits between two rules of one selector: the two sheets then no
   longer line up rule for rule, and the positional walk pairs one rule of the
   selector against another, reading as a swap of declarations that never left
   the selector. What the selector writes across all of its rules is what
   settles it. *)
let merge_same_selector_changes ~rules1 ~rules2 (changes : rule_diff list) :
    rule_diff list =
  (* Index the peers once. Looking for them with [List.filter] in the walk below
     scanned every reported change once per reported change, even though the
     common case is one entry per selector. The flags also replace two scans of
     a group; the reversed peer list is restored only for a group that actually
     collapses. *)
  let groups = Hashtbl.create (List.length changes) in
  List.iter
    (fun diff ->
      match changed_selector diff with
      | None -> ()
      | Some sel ->
          let peers, gains, loses =
            Option.value ~default:([], false, false)
              (Hashtbl.find_opt groups sel)
          in
          Hashtbl.replace groups sel
            ( diff :: peers,
              gains || change_gains diff,
              loses || change_loses diff ))
    changes;
  let done_ = Hashtbl.create 8 in
  List.filter_map
    (fun diff ->
      match changed_selector diff with
      | None -> Some diff
      | Some sel when Hashtbl.mem done_ sel -> None
      | Some sel -> (
          match Hashtbl.find_opt groups sel with
          | Some (reversed_peers, true, true) ->
              Hashtbl.replace done_ sel ();
              Some
                (merge_selector_group ~rules1 ~rules2 sel
                   (List.rev reversed_peers))
          | Some _ | None -> Some diff))
    changes

(* Everything a [Reordered] entry puts in the report, as one string; [None] for
   any other entry. Two entries sharing a key print the same line. *)
let reorder_key : rule_diff -> string option = function
  | Reordered r ->
      let buf = Buffer.create 64 in
      let add_decls = function
        | None -> Buffer.add_char buf '-'
        | Some decls ->
            List.iter
              (fun decl ->
                let name, value = decl_to_prop_value decl in
                add_strings buf [ name; ":"; value; ";" ])
              decls
      in
      add_strings buf
        [
          r.selector;
          "|";
          string_of_int r.expected_pos;
          "|";
          string_of_int r.actual_pos;
          "|";
          Option.value r.swapped_with ~default:"-";
          "|";
        ];
      add_decls r.old_declarations;
      Buffer.add_char buf '|';
      add_decls r.new_declarations;
      Some (Buffer.contents buf)
  | Added _ | Removed _ | Content_changed _ | Selector_changed _ | Rearranged _
  | Regrouped _ ->
      None

(* One move, one entry. A reorder names the selector and the position that
   selector holds on each side; which of its rules carried the declarations
   across is no part of it. A selector written by several rules that travel
   together therefore builds the same entry once per rule, and the node then
   states the move as many times as it has rules and counts it as many times
   over. Keyed on what the entry prints rather than on the selector alone, so a
   declaration-level reorder inside another rule of that selector is a different
   entry and stays. *)
let dedup_reorders (changes : rule_diff list) : rule_diff list =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun diff ->
      match reorder_key diff with
      | None -> true
      | Some key ->
          if Hashtbl.mem seen key then false
          else (
            Hashtbl.replace seen key ();
            true))
    changes

(* At-rules that carry neither a selector nor a condition the other processors
   key on: [@page], [@font-face], [@counter-style], [@scope], [@starting-style]
   and friends. [rule_diffs] gives every one of them the universal selector, so
   it pairs them without ever reading their bodies. What they hold below the
   brace decides how a pair is compared. *)
type at_rule_body =
  | Block of Css.statement list  (** statements, walked like a container *)
  | Declarations of Css.declaration list  (** a rule body without a rule *)
  | Opaque  (** descriptors, compared as the text they print to *)

(* Which of the three a statement is, is the only thing said here: the body
   itself comes from the shared readers. Listed one by one rather than closed
   with a wildcard, so an at-rule added to the AST does not compile until this
   processor has claimed it or handed it to another. *)
let at_rule_body (stmt : Css.statement) : at_rule_body option =
  match stmt with
  | Starting_style _ | Scope _ | Moz_document _ | When _ | Else _ ->
      Some (Block (Css.Stylesheet.statement_children stmt))
  (* With margin rules the declarations are only part of the body, so the whole
     block is compared as text instead. *)
  | Page _
  | Page_with_margins (_, _, [])
  | Position_try _ | Supports_condition _ ->
      Some (Declarations (Css.Stylesheet.statement_declarations stmt))
  | Font_face _ | Counter_style _ | Page_with_margins _ | Font_palette_values _
  | Font_feature_values _ | View_transition _ | Viewport _ | Webkit_keyframes _
  | Moz_keyframes _ | Unknown_at_rule _ | Namespace _ ->
      Some Opaque
  (* Owned by another processor: a rule and a bare nesting block by the rule
     matcher, a container by [moved_order_keys], [@keyframes] and [@property] by
     their own, and the rest carry no body to compare. *)
  | Rule _ | Declarations _ | Layer _ | Media _ | Container _ | Supports _
  | Origin _ | Keyframes _ | Property _ | Layer_decl _ | Import _ | Charset _
  | Bang_comment _ ->
      None

(* [process_at_rules] owns these statements, so leaving them in the rule diff as
   well would report one change twice, once against the universal selector. *)
let is_selectorless_at_rule stmt = at_rule_body stmt <> None

(* Every statement a processor of its own reads and names.
   [selector_key_of_stmt] gives all of them the universal selector, so the rule
   matcher pairs a [@property] with a [@keyframes] with a [@media] and hands
   whichever it has one too many of to the report, where nothing can name it.
   Containers are the exception and stay: [moved_order_keys] reads their
   position, and [convert_added_rule]/[convert_removed_rule] keep them out of
   the entries. *)
let is_reported_by_own_processor stmt =
  is_selectorless_at_rule stmt
  || Css.as_property stmt <> None
  || Css.as_keyframes stmt <> None

(* A stylesheet may repeat one at-rule (several [@font-face] blocks, several
   [@page] rules), so the head alone does not name a block. Number the blocks
   that share a head and pair them in order. *)
let at_rule_items stmts =
  let seen = Hashtbl.create 8 in
  List.filter_map
    (fun stmt ->
      match at_rule_body stmt with
      | None -> None
      | Some body ->
          let head, text = statement_text_split stmt in
          let n = Option.value ~default:0 (Hashtbl.find_opt seen head) in
          Hashtbl.replace seen head (n + 1);
          Some ((head, n), (head, body, text)))
    stmts

(* The container line already names the at-rule, so these changes carry no
   selector of their own; a second label would read as a second subject. *)

let at_rule_declarations_change decls1 decls2 =
  let property_changes, added_properties, removed_properties =
    properties_diff decls1 decls2
  in
  if
    property_changes = [] && added_properties = [] && removed_properties = []
    && not (reorder_is_significant decls1 decls2)
  then None
  else
    Some
      (Content_changed
         {
           selector = "";
           old_declarations = decls1;
           new_declarations = decls2;
           property_changes;
           added_properties;
           removed_properties;
         })

(* Descriptors hold neither statements nor declarations, so a pair of them is
   compared on the text it prints to. *)
let at_rule_text_change text1 text2 =
  Content_changed
    {
      selector = "";
      old_declarations = [];
      new_declarations = [];
      property_changes =
        [
          {
            property_name = "descriptors";
            expected_value = text1;
            actual_value = text2;
          };
        ];
      added_properties = [];
      removed_properties = [];
    }

let to_rule_changes rules1 rules2 : rule_diff list =
  let rules1 =
    List.filter (fun s -> not (is_reported_by_own_processor s)) rules1
  in
  let rules2 =
    List.filter (fun s -> not (is_reported_by_own_processor s)) rules2
  in
  let r_added, r_removed, r_modified, r_regrouped = rule_diffs rules1 rules2 in
  let moved = moved_order_keys rules1 rules2 in
  List.filter_map convert_added_rule r_added
  @ List.filter_map convert_removed_rule r_removed
  @ List.filter_map (convert_modified_rule ~moved ~rules1 ~rules2) r_modified
  @ r_regrouped
  |> merge_same_selector_changes ~rules1 ~rules2
  |> dedup_reorders

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
  | Some (cond, rules) -> Some (Css.Supports.to_string cond, rules)
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

let diff ~(expected : Css.t) ~(actual : Css.t) : t =
  let all1 = Css.statements expected in
  let all2 = Css.statements actual in
  (* Imports are diffed separately ([process_imports]); excluding them here
     keeps [rule_diffs] from matching every import on the universal key. *)
  let rules1 = List.filter (fun s -> Css.as_import s = None) all1 in
  let rules2 = List.filter (fun s -> Css.as_import s = None) all2 in
  (* Same assembly as inside a container, so a difference reports the same way
     at either depth. *)
  let rule_changes =
    to_rule_changes rules1 rules2 @ process_imports all1 all2
  in

  (* Delegate all container and nested-container diffs to the generic walker *)
  let containers = nested_differences all1 all2 in
  { rules = rule_changes; containers; layer_order = layer_order_diff all1 all2 }
