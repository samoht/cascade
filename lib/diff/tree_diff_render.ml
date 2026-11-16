(* The tree diff report: tree-style and flat rendering of rule and container
   changes under depth and entry budgets. *)

open Cascade
open Tree_diff_intf
open Tree_diff_text

(* ===== Constants ===== *)

let default_truncation_length = String_diff.default_max_width

(* How many swapped layer pairs the report names before counting the rest. A
   reversed order of n layers inverts n(n-1)/2 pairs, and reading the first few
   is enough to place the change. *)
let max_layer_swaps = 5

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
    && Tree_diff_rules.reorder_is_significant decls1 decls2
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
