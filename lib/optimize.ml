(** CSS optimization implementation *)

open Declaration
open Stylesheet
open Common
module String_set = Set.Make (String)

let src = Logs.Src.create "cascade.optimize" ~doc:"Cascade CSS optimizer"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Edge Model} *)

type scope = Ctx.scope

(* Optimisation context threaded by the entry points ([stylesheet], [rules],
   [single_rule], [deduplicate_declarations]) down to the shorthand composers.
   [scope] drives the fragment-vs-stylesheet decisions; [registered] reports
   whether a custom property is registered with an [@property] initial-value, so
   folding its [var()] into a shorthand cannot widen an invalid-at-
   computed-value failure. Default: fragment, nothing registered. *)
type ctx = Ctx.t

let ctx_of_scope = Ctx.of_scope
let list_map_preserve = List.map_preserve
let list_filter_preserve = List.filter_preserve
let list_edit_preserve = List.edit_preserve

let option_map_preserve (f : 'a -> 'a) (value : 'a option) : 'a option =
  match value with
  | None -> (None : 'a option)
  | Some x ->
      let y = f x in
      if y == x then value else Some y

let preserve_list = List.preserve

let rule_with_nested (rule : rule) nested =
  if nested == rule.nested then rule else { rule with nested }

let rule_with_declarations_and_nested (rule : rule) declarations nested =
  if declarations == rule.declarations && nested == rule.nested then rule
  else { rule with declarations; nested }

type packed_property = Edge.packed_property =
  | Packed : 'a Properties.property -> packed_property

type edge = Edge.t = {
  summary : Selector_summary.t;
  property : packed_property;
  important : bool;
}

let edges_of_rule = Edge.of_rule

(** {1 Declaration Optimization} *)

let duplicate_buggy_properties = Shorthand.duplicate_buggy_properties
let merge_overflow_longhands = Shorthand.merge_overflow_longhands
let merge_box_shorthand_longhands = Shorthand.merge_box_shorthand_longhands
let compose_shorthands = Shorthand.compose_shorthands
let deduplicate_declarations_with = Shorthand.deduplicate_declarations_with
let deduplicate_declarations = Shorthand.deduplicate_declarations
let single_rule_without_nested = Rule.single
let finalize_rule_without_nested = Rule.finalize
let merge_rules = Rule.merge
let combine_identical_rules = Rule.identical
let factor_anchor_gaps = Factor.anchor

(* Per-pass and global counters for the --profile CLI flag. Reset at each
   [Optimize.stylesheet] entry; bumped from the hot loops below. *)
type pass_stat = Stats.pass_stat = {
  mutable time : float;
  mutable calls : int;
  mutable changes : int;
  mutable rules_in : int;
  mutable rules_out : int;
}

let pass_times = Stats.pass_times
let set_profile = Stats.set_profile

type iteration_stat = Stats.iteration_stat = {
  fixpoint : int;
  iteration : int;
  local_iteration : int;
  before_rules : int;
  after_rules : int;
  before_bytes : int;
  after_bytes : int;
  bytes_saved : int;
  active_passes : int;
  changed_passes : int;
  elapsed : float;
}

let iteration_stats = Stats.iteration_stats

type counters = Stats.counters = {
  mutable iterations : int;
  mutable factor_fixpoints_run : int;
  mutable marginal_stops : int;
  mutable factor_fixpoints_skipped : int;
  mutable factor_preflight_gain : int;
  mutable factor_bytes_saved : int;
  mutable anchors_scored : int;
  mutable anchors_prefiltered : int;
  mutable factorings_applied : int;
  mutable interval_candidates : int;
  mutable interval_pruned : int;
  mutable interval_scored : int;
  mutable interval_selected : int;
}

let counters = Stats.counters

let reset_counters () =
  Stats.reset ();
  Summary.reset ()

(** {1 Statement Optimization} *)

let merge_consecutive_layers = Block.merge_consecutive_layers
let merge_consecutive_media = Block.merge_consecutive_media
let merge_distant_media = Block.merge_distant_media
let merge_consecutive_supports = Block.merge_consecutive_supports
let merge_consecutive_containers = Block.merge_consecutive_containers
let is_layer_empty = Block.is_layer_empty
let collect_empty_layer_names = Block.collect_empty_layer_names
let merge_layer_declarations = Block.merge_layer_declarations
let drop_redundant_layer_decls = Block.drop_redundant_layer_decls
let drop_empty_rules = Block.drop_empty_rules
let drop_misplaced_imports = Block.drop_misplaced_imports
let merge_named_layers_by_name = Block.merge_named_layers_by_name
let merge_lone_nested_rule = Nest.merge_lone
let synthesize_nesting_statements = Nest.statements

(* A block holds a conditional named layer when it directly contains a named
   [@layer] block with content. Unwrapping a known-true [@supports] around such
   a block would move that layer's rules - and its place in the layer order (CSS
   Cascade 6.4) - from conditional to unconditional, so the wrapper must stay
   even when the condition is baseline-true. A bare [@layer name;] declaration
   carries no rules, and self-guarding declarations carry no side effect, so
   both unwrap freely. *)
let block_introduces_layer_order stmts =
  List.exists (function Layer (Some _, _ :: _) -> true | _ -> false) stmts

(* Pop the run of [Rule]s most recently pushed onto a reversed accumulator,
   returning them in forward order alongside the remaining accumulator. Used
   when unwrapping a known-true [@supports] exposes its inner rules to rules
   already emitted: those preceding rules must rejoin the merge pass so an
   adjacency the wrapper had hidden can collapse. *)
let pop_trailing_rules acc =
  let rec loop acc rules =
    match acc with
    | (Rule _ as r) :: rest -> loop rest (r :: rules)
    | _ -> (rules, acc)
  in
  loop acc []

let rec collect_rules (stmt_acc : statement list) (rules_acc : rule list) :
    statement list -> statement list * rule list * statement list = function
  | (Rule r as stmt) :: rest ->
      collect_rules (stmt :: stmt_acc) (r :: rules_acc) rest
  | rest -> (List.rev stmt_acc, List.rev rules_acc, rest)

let factor_rules_incremental ~ctx rules =
  Factor.run ~ctx ~finalize:(finalize_rule_without_nested ~ctx) rules

(* [@scope] bounds are parsed selectors; canonicalize them like any other
   selector so a list bound is de-duplicated and ordered consistently. *)
let canonicalize_scope_selector sel = Selector.canonicalize sel

(* Nested rule selectors are implicitly relative to the parent [&], so drop a
   redundant leading [& <combinator>] (CSS Nesting 1 sec. 2). *)
let drop_nesting_prefix (stmt : statement) : statement =
  match stmt with
  | Rule nr ->
      Rule
        {
          nr with
          selector = Selector.drop_redundant_nesting_prefix nr.selector;
        }
  | other -> other

let rec statements ~ctx ~enforce_spec (stmts : statement list) : statement list
    =
  match stmts with
  | [] -> stmts
  | _ ->
      let optimize_merged_block = statements ~ctx ~enforce_spec in
      (* [drop_misplaced_imports] runs first: an [@import] after a style rule is
         invalid and ignored by every browser, so it is a no-op that must not
         act as a cascade boundary. Stripping it up front lets the rules it
         falsely separated merge in this same pass, which keeps [statements]
         idempotent - stripping after the merge would leave two adjacent
         same-selector rules that only a re-run would combine. *)
      let stmts' =
        let stmts =
          drop_misplaced_imports stmts |> merge_named_layers_by_name
        in
        let stmts = process_statements ~ctx ~enforce_spec [] stmts in
        let stmts = synthesize_nesting_statements stmts in
        let stmts =
          stmts
          |> merge_consecutive_media ~optimize_merged_block
          |> merge_distant_media ~optimize_merged_block
          |> merge_consecutive_supports ~optimize_merged_block
          |> merge_consecutive_containers ~optimize_merged_block
        in
        stmts |> merge_layer_declarations |> drop_empty_rules
      in
      preserve_list stmts stmts'

(* Process a "cursor" of (reverse acc, current remaining list, pending segment
   stack). When the current list is exhausted, pop the next segment from
   pending; when both are empty, return the final reversed list. This shape lets
   [process_supports_statement] splice three segments without first
   materialising their concatenation. *)
and process_statements ~ctx ~enforce_spec ?(pending : statement list list = [])
    (acc : statement list) (remaining : statement list) : statement list =
  match remaining with
  | [] -> (
      match pending with
      | [] -> List.rev acc
      | next :: rest_pending ->
          process_statements ~ctx ~enforce_spec ~pending:rest_pending acc next)
  | (Rule r as stmt) :: rest ->
      process_rule_run ~ctx ~enforce_spec ~pending acc stmt r rest
  | (Media (cond, block) as stmt) :: rest ->
      process_media_statement ~ctx ~enforce_spec ~pending acc stmt cond block
        rest
  | (Container (name, cond, block) as stmt) :: rest ->
      process_container_statement ~ctx ~enforce_spec ~pending acc stmt name cond
        block rest
  | Supports (cond, block) :: rest ->
      process_supports_statement ~ctx ~enforce_spec ~pending acc cond block rest
  | (Scope (start, end_, block) as stmt) :: rest ->
      let start' = option_map_preserve canonicalize_scope_selector start in
      let end_' = option_map_preserve canonicalize_scope_selector end_ in
      let optimized_block = statements ~ctx ~enforce_spec block in
      let optimized =
        if start' == start && end_' == end_ && optimized_block == block then
          stmt
        else Scope (start', end_', optimized_block)
      in
      process_statements ~ctx ~enforce_spec ~pending (optimized :: acc) rest
  | (Origin (origin, block) as stmt) :: rest ->
      let optimized_block = statements ~ctx ~enforce_spec block in
      let optimized =
        if optimized_block == block then stmt
        else Origin (origin, optimized_block)
      in
      process_statements ~ctx ~enforce_spec ~pending (optimized :: acc) rest
  | (Layer (name, block) as stmt) :: rest ->
      process_layer_statement ~ctx ~enforce_spec ~pending acc stmt name block
        rest
  | (Import import as stmt) :: rest ->
      process_import_statement ~ctx ~enforce_spec ~pending acc stmt import rest
  | hd :: rest ->
      (* Other statement types - keep as-is *)
      process_statements ~ctx ~enforce_spec ~pending (hd :: acc) rest

and process_rule_run ~ctx ~enforce_spec ~pending acc stmt r rest =
  let plain_stmts, plain_rules, rest = collect_rules [ stmt ] [ r ] rest in
  let optimized = rules_aux ~ctx ~enforce_spec plain_rules in
  let as_statements =
    if optimized == plain_rules then plain_stmts
    else List.map (fun r -> Rule r) optimized
  in
  process_statements ~ctx ~enforce_spec ~pending
    (List.rev_append as_statements acc)
    rest

and process_media_statement ~ctx ~enforce_spec ~pending acc stmt cond block rest
    =
  let cond = if enforce_spec then cond else Media.lower_for_minify cond in
  let optimized_block = statements ~ctx ~enforce_spec block in
  let optimized =
    if
      (cond == match stmt with Media (c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Media (cond, optimized_block)
  in
  process_statements ~ctx ~enforce_spec ~pending (optimized :: acc) rest

and process_container_statement ~ctx ~enforce_spec ~pending acc stmt name cond
    block rest =
  let cond =
    if enforce_spec then cond
    else option_map_preserve Container.lower_for_minify cond
  in
  let optimized_block = statements ~ctx ~enforce_spec block in
  let optimized =
    if
      (cond == match stmt with Container (_, c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Container (name, cond, optimized_block)
  in
  process_statements ~ctx ~enforce_spec ~pending (optimized :: acc) rest

and process_supports_statement ~ctx ~enforce_spec ~pending acc cond block rest =
  let optimized_block = statements ~ctx ~enforce_spec block in
  let baseline =
    if enforce_spec then `Cond cond else Supports.simplify_baseline cond
  in
  match baseline with
  | `True when block_introduces_layer_order optimized_block ->
      process_statements ~ctx ~enforce_spec ~pending
        (Supports (cond, optimized_block) :: acc)
        rest
  | `True ->
      let trailing, acc = pop_trailing_rules acc in
      (* [trailing], [optimized_block], and [rest] must be visible to
         [process_statements] as a SINGLE list so [collect_rules] can pull
         adjacent rules across the segment boundary into one rule run - that
         adjacency-aware merge is exactly the point of unwrapping a
         baseline-true @supports. Use tail-recursive [List.concat] so a long
         [optimized_block] doesn't stack-overflow. *)
      process_statements ~ctx ~enforce_spec ~pending acc
        (List.concat [ trailing; optimized_block; rest ])
  | `False -> process_statements ~ctx ~enforce_spec ~pending acc rest
  | `Cond cond' ->
      process_statements ~ctx ~enforce_spec ~pending
        (Supports (cond', optimized_block) :: acc)
        rest

and process_layer_statement ~ctx ~enforce_spec ~pending acc stmt name block rest
    =
  let optimized_block = statements ~ctx ~enforce_spec block in
  if is_layer_empty optimized_block then
    match name with
    | Some layer_name ->
        let all_names, remaining =
          collect_empty_layer_names [ layer_name ] rest
        in
        process_statements ~ctx ~enforce_spec ~pending
          (Layer_decl all_names :: acc)
          remaining
    | None -> process_statements ~ctx ~enforce_spec ~pending acc rest
  else
    let optimized =
      if optimized_block == block then stmt else Layer (name, optimized_block)
    in
    process_statements ~ctx ~enforce_spec ~pending (optimized :: acc) rest

and process_import_statement ~ctx ~enforce_spec ~pending acc stmt import rest =
  let stmt =
    match import.supports with
    | Some cond
      when (not enforce_spec) && Supports.simplify_baseline cond = `True ->
        Import { import with supports = None }
    | _ -> stmt
  in
  process_statements ~ctx ~enforce_spec ~pending (stmt :: acc) rest

and rules_aux ~ctx ~enforce_spec (rules : rule list) : rule list =
  (* First optimize each rule's nested statements recursively, then drop the
     redundant nesting prefix (see [drop_nesting_prefix]). *)
  let with_optimized_nested =
    list_map_preserve
      (fun rule ->
        let nested =
          match rule.nested with
          | [] -> []
          | nested ->
              let nested = statements ~ctx ~enforce_spec nested in
              if enforce_spec then nested
              else list_map_preserve drop_nesting_prefix nested
        in
        let rule = rule_with_nested rule nested in
        let rule =
          let selector = Selector.canonicalize rule.selector in
          if selector == rule.selector then rule else { rule with selector }
        in
        merge_lone_nested_rule rule)
      rules
  in
  (* Apply standard rule optimizations. Adjacent same-selector rules merge:
     [.x{a}] [.x{b}] -> [.x{a;b}], which is safe because cascade order within
     the merged block matches the source order of the originals.
     [combine_identical_rules] then groups same-declaration rules under a
     selector list ([.a, .b, .c{...}]). *)
  let prepared =
    list_map_preserve (single_rule_without_nested ~ctx) with_optimized_nested
  in
  let prepared = prepared |> Rule.drop_shadowed |> merge_rules in
  let prepared =
    list_map_preserve
      (finalize_rule_without_nested ~canonicalize_selector:false ~ctx)
      prepared
  in
  (* Factoring is greedy and global: extracting one shared declaration subset
     can leave behind leftovers that are themselves factorable. The local linear
     optimizations above always run; this incremental gate only decides whether
     the expensive global factoring fixpoint is likely to buy enough bytes to
     justify the full indexed scheduler walk. *)
  factor_rules_incremental ~ctx prepared

(* CSS Animations 2 sec. 4.1: [@keyframes name] re-declaration overrides the
   earlier definition in source order. Drop earlier same-name keyframes; the
   later one wins. Vendor-prefixed [-webkit-] / [-moz-] variants are separate
   namespaces, so they are not dedup'd against the unprefixed form. *)
let drop_shadowed_keyframes (stmts : statement list) : statement list =
  let exists_later kind name tail =
    List.exists
      (fun stmt ->
        match (kind, stmt) with
        | `Plain, Keyframes (n, _) -> n = name
        | `Webkit, Webkit_keyframes (n, _) -> n = name
        | `Moz, Moz_keyframes (n, _) -> n = name
        | _ -> false)
      tail
  in
  let rec walk acc = function
    | [] -> List.rev acc
    | (Keyframes (name, _) as kf) :: rest ->
        if exists_later `Plain name rest then walk acc rest
        else walk (kf :: acc) rest
    | (Webkit_keyframes (name, _) as kf) :: rest ->
        if exists_later `Webkit name rest then walk acc rest
        else walk (kf :: acc) rest
    | (Moz_keyframes (name, _) as kf) :: rest ->
        if exists_later `Moz name rest then walk acc rest
        else walk (kf :: acc) rest
    | stmt :: rest -> walk (stmt :: acc) rest
  in
  walk [] stmts

(* CSS Cascade 5 sec. 6.4.2: when a named layer is declared multiple times the
   rules from all occurrences accumulate into the layer. Merge same-name blocks
   at the position of the FIRST NON-EMPTY occurrence so the merged content stays
   where the author placed the layer's first real declaration. Leading empty
   blocks ([@layer name {}]) stay in place so the [empty-named-layer ->
   Layer_decl] normalisation in [process_statements] still folds them into the
   order-only declaration. *)
let statements_top_level ~ctx ~enforce_spec (stmts : statement list) :
    statement list =
  let optimize_merged_block = statements ~ctx ~enforce_spec in
  let stmts' =
    statements ~ctx ~enforce_spec stmts
    |> merge_consecutive_layers ~optimize_merged_block
    |> drop_redundant_layer_decls |> drop_shadowed_keyframes
  in
  preserve_list stmts stmts'

let single_rule ?scope (rule : rule) : rule =
  let ctx = ctx_of_scope scope in
  {
    rule with
    declarations = deduplicate_declarations_with ~ctx rule.declarations;
    nested = statements ~ctx ~enforce_spec:false rule.nested;
  }

let rules ?scope (rules : rule list) : rule list =
  rules_aux ~ctx:(ctx_of_scope scope) ~enforce_spec:false rules

(** {1 Nesting Flattening} *)

let flatten_nesting = Flatten.block

(** {1 Stylesheet Optimization} *)

let apply_property_duplication (stylesheet : t) : t =
  (* Apply only property duplication without other optimizations. Each level
     keeps its node when nothing below changed, so an untouched subtree stays
     physically shared (no whole-tree rebuild on a no-op). *)
  let rec apply_to_statements stmts =
    list_map_preserve
      (fun stmt ->
        match stmt with
        | Rule rule ->
            let declarations = duplicate_buggy_properties rule.declarations in
            if declarations == rule.declarations then stmt
            else Rule { rule with declarations }
        | Media (cond, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Media (cond, inner')
        | Layer (name, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Layer (name, inner')
        | Container (name, cond, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Container (name, cond, inner')
        | Supports (cond, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Supports (cond, inner')
        | Origin (origin, inner) ->
            let inner' = apply_to_statements inner in
            if inner' == inner then stmt else Origin (origin, inner')
        | other -> other)
      stmts
  in
  apply_to_statements stylesheet

let map_statement_block_preserve f stmt =
  let map block = list_map_preserve f block in
  match stmt with
  | Layer (name, block) ->
      let block' = map block in
      if block' == block then stmt else Layer (name, block')
  | Media (m, block) ->
      let block' = map block in
      if block' == block then stmt else Media (m, block')
  | Container (n, c, block) ->
      let block' = map block in
      if block' == block then stmt else Container (n, c, block')
  | Supports (s, block) ->
      let block' = map block in
      if block' == block then stmt else Supports (s, block')
  | Moz_document (c, block) ->
      let block' = map block in
      if block' == block then stmt else Moz_document (c, block')
  | When (c, block) ->
      let block' = map block in
      if block' == block then stmt else When (c, block')
  | Else (c, block) ->
      let block' = map block in
      if block' == block then stmt else Else (c, block')
  | Starting_style block ->
      let block' = map block in
      if block' == block then stmt else Starting_style block'
  | Origin (o, block) ->
      let block' = map block in
      if block' == block then stmt else Origin (o, block')
  | Scope (a, b, block) ->
      let block' = map block in
      if block' == block then stmt else Scope (a, b, block')
  | _ -> stmt

let iter_statement_block f stmt =
  match stmt with
  | Layer (_, block)
  | Media (_, block)
  | Container (_, _, block)
  | Supports (_, block)
  | Moz_document (_, block)
  | When (_, block)
  | Else (_, block)
  | Starting_style block
  | Origin (_, block)
  | Scope (_, _, block) ->
      List.iter f block
  | _ -> ()

(** [drop_invalid] walks every declaration list in the stylesheet (rules, bare
    nesting blocks, [@page] / [@font-palette-values] / [@view-transition] /
    [@position-try]) and removes declarations whose typed value contains an
    [Invalid] arm. *)
let drop_invalid (stylesheet : t) : t =
  let filter_decls =
    list_filter_preserve (fun d -> not (Declaration.is_invalid d))
  in
  let rec statement stmt =
    match stmt with
    | Rule rule ->
        let declarations = filter_decls rule.declarations in
        let nested = list_map_preserve statement rule.nested in
        let rule' =
          rule_with_declarations_and_nested rule declarations nested
        in
        if rule' == rule then stmt else Rule rule'
    | Declarations decls ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Declarations decls'
    | Layer _ | Media _ | Container _ | Supports _ | Moz_document _ | When _
    | Else _ | Starting_style _ | Origin _ | Scope _ ->
        map_statement_block_preserve statement stmt
    | Page (sel, decls) ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Page (sel, decls')
    | Page_with_margins (sel, descs, margins) ->
        let descs' = filter_decls descs in
        let margins' =
          list_map_preserve
            (fun (m : page_margin_rule) ->
              let descriptors = filter_decls m.descriptors in
              if descriptors == m.descriptors then m else { m with descriptors })
            margins
        in
        if descs' == descs && margins' == margins then stmt
        else Page_with_margins (sel, descs', margins')
    | Position_try (name, decls) ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Position_try (name, decls')
    | Supports_condition (name, decls) ->
        let decls' = filter_decls decls in
        if decls' == decls then stmt else Supports_condition (name, decls')
    | other -> other
  in
  list_map_preserve statement stylesheet

(** [drop_unknown_at_rules] removes [Unknown_at_rule] statements at every block
    depth. Used in [--minify] alongside [drop_invalid] so the typed warnings
    emitted at parse time materialise as a dropped rule, matching CSS Syntax 3
    §5.4.1 (unknown at-rules are discarded). *)
let drop_unknown_at_rules (stylesheet : t) : t =
  let rec statement stmt =
    match stmt with
    | Rule rule ->
        let nested = list_edit_preserve statement rule.nested in
        let rule' = rule_with_nested rule nested in
        if rule' == rule then List.Keep else List.Replace (Rule rule')
    | Layer (name, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep
        else List.Replace (Layer (name, block'))
    | Media (m, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep else List.Replace (Media (m, block'))
    | Container (n, c, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep
        else List.Replace (Container (n, c, block'))
    | Supports (s, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep
        else List.Replace (Supports (s, block'))
    | Moz_document (c, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep
        else List.Replace (Moz_document (c, block'))
    | When (c, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep else List.Replace (When (c, block'))
    | Else (c, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep else List.Replace (Else (c, block'))
    | Starting_style block ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep
        else List.Replace (Starting_style block')
    | Origin (o, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep else List.Replace (Origin (o, block'))
    | Scope (a, b, block) ->
        let block' = list_edit_preserve statement block in
        if block' == block then List.Keep
        else List.Replace (Scope (a, b, block'))
    | Unknown_at_rule _ -> List.Drop
    | _ -> List.Keep
  in
  list_edit_preserve statement stylesheet

(* CSS Properties and Values API 1 sec. 2: an [@property --name { syntax: ... }]
   declaration registers [name] with a typed CSS syntax, lifting later [--name:
   ...] uses out of the unregistered opaque-token-stream rule. Apply
   registrations in source order so a [@property] only affects uses that follow
   it; later registrations of the same name overwrite, matching how the browser
   registry resolves duplicate declarations. *)
let try_promote_custom_with (type a) (syntax : a Variables.syntax) components =
  match syntax with
  | Variables.Color -> Properties.try_read_custom_color components
  | Variables.Length -> Properties.try_read_custom_length components
  | Variables.Length_percentage ->
      Properties.try_read_custom_length_percentage components
  | Variables.Number -> Properties.try_read_custom_number components
  | Variables.Percentage -> Properties.try_read_custom_percentage components
  | _ -> None

(* Does the registered syntax somewhere accept a [<custom-ident>] (possibly
   under [+] / [#] / [|] modifiers)? When yes, a [<string>] whose content is a
   multi-word identifier sequence is spec-equivalent (CSS Fonts 4 sec. 15.3 for
   font-family-shaped registrations), so the promotion pass rewrites it to the
   equivalent ident sequence before parsing. *)
let rec syntax_accepts_ident_sequence : type a. a Variables.syntax -> bool =
  function
  | Variables.Custom_ident -> true
  | Variables.Plus s -> syntax_accepts_ident_sequence s
  | Variables.Hash s -> syntax_accepts_ident_sequence s
  | Variables.Or (s1, s2) ->
      syntax_accepts_ident_sequence s1 || syntax_accepts_ident_sequence s2
  | _ -> false

let promote_registered_custom_decl ~lossless registry decl =
  match decl with
  | Declaration
      {
        property = Custom_property name;
        value = Custom_value { value = Tokens components; layer; meta };
        important;
      } -> (
      match Hashtbl.find_opt registry name with
      | None -> decl
      | Some (Variables.Syntax syntax) -> (
          let components' =
            if syntax_accepts_ident_sequence syntax then
              Properties.unquote_font_family_strings components
            else components
          in
          match try_promote_custom_with syntax components' with
          | Some typed ->
              Declaration.v ~important (Custom_property name)
                (Custom_value
                   {
                     value =
                       Properties.normalize_custom_property_value ~lossless
                         typed;
                     layer;
                     meta;
                   })
          | None when components' == components -> decl
          | None ->
              (* Promotion failed (e.g. [<custom-ident>+] has no typed promotion
                 path yet) but the string-to-ident rewrite still produces the
                 canonical opaque AST. *)
              Declaration.v ~important (Custom_property name)
                (Custom_value { value = Tokens components'; layer; meta })))
  | _ -> decl

let promote_registered_custom_properties ~lossless (stmts : statement list) =
  let registry : (string, Variables.any_syntax) Hashtbl.t = Hashtbl.create 8 in
  (* An [@property] registration is document-global regardless of source order
     (CSS Properties and Values API 1 SS 2). Tailwind emits its [@property]
     rules after the [@layer] that uses them, so collect every registration in a
     first pass before promoting any declaration. *)
  let rec collect_stmt (stmt : statement) : unit =
    match stmt with
    | Property pr ->
        Hashtbl.replace registry pr.name (Variables.Syntax pr.syntax)
    | Rule r -> List.iter collect_stmt r.nested
    | _ -> iter_statement_block collect_stmt stmt
  in
  List.iter collect_stmt stmts;
  let promote_decl = promote_registered_custom_decl ~lossless registry in
  let rec walk_stmt (stmt : statement) : statement =
    match stmt with
    | Property _ -> stmt
    | Rule r ->
        let declarations = list_map_preserve promote_decl r.declarations in
        let nested = list_map_preserve walk_stmt r.nested in
        let r' = rule_with_declarations_and_nested r declarations nested in
        if r' == r then stmt else Rule r'
    | Declarations decls ->
        let decls' = list_map_preserve promote_decl decls in
        if decls' == decls then stmt else Declarations decls'
    | Media _ | Container _ | Supports _ | Layer _ | Origin _ | Scope _
    | Starting_style _ | Moz_document _ | When _ | Else _ ->
        map_statement_block_preserve walk_stmt stmt
    | _ -> stmt
  in
  list_map_preserve walk_stmt stmts

(* Under closed-stylesheet scope the optimiser knows every [@position-try
   --name] rule defined in the sheet. A [position-try-fallbacks: --x, --y] entry
   whose name has no matching [@position-try] rule cannot match at runtime, so
   prune unknown [Name] arms. Keep the [Flip_*] tactics and any [Var]
   indirection untouched. When every arm gets pruned the whole declaration drops
   (the property becomes equivalent to its initial). *)
let collect_position_try_names stylesheet =
  let known : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec collect (stmt : statement) =
    match stmt with
    | Position_try (name, _) -> Hashtbl.replace known name ()
    | Rule rule -> List.iter collect rule.nested
    | Layer (_, b)
    | Media (_, b)
    | Container (_, _, b)
    | Supports (_, b)
    | Moz_document (_, b)
    | When (_, b)
    | Else (_, b)
    | Starting_style b
    | Origin (_, b)
    | Scope (_, _, b) ->
        List.iter collect b
    | _ -> ()
  in
  List.iter collect stylesheet;
  known

let rec prune_position_try_decl known (decl : Declaration.declaration) :
    Declaration.declaration option =
  match decl with
  | Declaration
      {
        property = Position_try_fallbacks;
        value = (Fallbacks items : Properties.position_try_fallbacks);
        important;
      } -> (
      let keep = function
        | (Properties.Name s : Properties.position_try_fallback) ->
            Hashtbl.mem known s
        | _ -> true
      in
      match list_filter_preserve keep items with
      | [] -> None
      | kept when kept == items -> Some decl
      | kept ->
          Some
            (Declaration.v ~important Position_try_fallbacks (Fallbacks kept)))
  | Theme_guarded { var_name; decl; _ } -> (
      match prune_position_try_decl known decl with
      | None -> None
      | Some decl' ->
          if decl' == decl then Some decl
          else Some (Declaration.theme_guarded ~var_name decl'))
  | other -> Some other

let prune_position_try_fallbacks ~scope (stylesheet : t) : t =
  match scope with
  | `Fragment -> stylesheet
  | `Stylesheet ->
      let known = collect_position_try_names stylesheet in
      let prune_decls decls =
        list_edit_preserve
          (fun decl ->
            match prune_position_try_decl known decl with
            | Some decl' when decl' == decl -> List.Keep
            | Some decl' -> List.Replace decl'
            | None -> List.Drop)
          decls
      in
      let rec walk (stmt : statement) : statement =
        match stmt with
        | Rule rule ->
            let declarations = prune_decls rule.declarations in
            let nested = list_map_preserve walk rule.nested in
            let rule' =
              rule_with_declarations_and_nested rule declarations nested
            in
            if rule' == rule then stmt else Rule rule'
        | Declarations decls ->
            let decls' = prune_decls decls in
            if decls' == decls then stmt else Declarations decls'
        | Layer _ | Media _ | Container _ | Supports _ | Moz_document _ | When _
        | Else _ | Starting_style _ | Origin _ | Scope _ ->
            map_statement_block_preserve walk stmt
        | Page (sel, decls) ->
            let decls' = prune_decls decls in
            if decls' == decls then stmt else Page (sel, decls')
        | Position_try (n, decls) ->
            let decls' = prune_decls decls in
            if decls' == decls then stmt else Position_try (n, decls')
        | Supports_condition (n, decls) ->
            let decls' = prune_decls decls in
            if decls' == decls then stmt else Supports_condition (n, decls')
        | other -> other
      in
      list_map_preserve walk stylesheet

(* Collect the custom properties registered with an [@property] initial-value.
   Such a property is never invalid at computed-value time, so folding its
   [var()] into a shorthand cannot widen a failure across the other
   longhands. *)
let registered_foldable (stylesheet : t) : string -> bool =
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let rec collect (stmt : statement) =
    match stmt with
    | Property pr -> (
        match pr.initial_value with
        | Some _ ->
            (* [@property] names carry the [--] prefix; [var()] references store
               the bare name, so normalise to the bare form for lookup. *)
            let key =
              if String.length pr.name >= 2 && String.sub pr.name 0 2 = "--"
              then String.sub pr.name 2 (String.length pr.name - 2)
              else pr.name
            in
            Hashtbl.replace tbl key ()
        | None -> ())
    | Rule rule -> List.iter collect rule.nested
    | Layer (_, b)
    | Media (_, b)
    | Container (_, _, b)
    | Supports (_, b)
    | Moz_document (_, b)
    | When (_, b)
    | Else (_, b)
    | Starting_style b
    | Origin (_, b)
    | Scope (_, _, b) ->
        List.iter collect b
    | _ -> ()
  in
  List.iter collect stylesheet;
  fun name -> Hashtbl.mem tbl name

(* CSS Properties and Values API 1: a custom property registered with a
   single-component [@property] syntax (not a [+]/[#] list, [*], or a transform
   list) substitutes exactly one calc term, so a redundant [calc(var(--n))]
   nested in another [calc()] may be unwrapped. *)
let rec syntax_is_single_valued : type a. a Variables.syntax -> bool = function
  | Variables.Universal | Variables.Transform_list -> false
  | Variables.Plus _ | Variables.Hash _ -> false
  | Variables.Or (a, b) ->
      syntax_is_single_valued a && syntax_is_single_valued b
  | _ -> true

(* [@property] registrations are document-global, so collect every single-valued
   registration up front to build the calc-simplifier context. *)
let single_valued_calc_ctx (stmts : statement list) : Values.calc_ctx =
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  (* [@property] names carry the [--] prefix; calc [Var] leaves store the bare
     name (the reader strips it), so key the table on the bare name. *)
  let bare name =
    if String.length name >= 2 && name.[0] = '-' && name.[1] = '-' then
      String.sub name 2 (String.length name - 2)
    else name
  in
  let rec collect (stmt : statement) =
    match stmt with
    | Property pr ->
        if syntax_is_single_valued pr.syntax then
          Hashtbl.replace tbl (bare pr.name) ()
    | Rule r -> List.iter collect r.nested
    | _ -> iter_statement_block collect stmt
  in
  List.iter collect stmts;
  { Values.var_is_single_valued = (fun name -> Hashtbl.mem tbl name) }

let normalize_live_declarations ~ctx ~lossless decls =
  list_edit_preserve
    (fun decl ->
      let decl' = Declaration.normalize ~ctx ~lossless decl in
      if Declaration.is_invalid decl' then List.Drop
      else if decl' == decl then List.Keep
      else List.Replace decl')
    decls

let sanitize_keyframe ~ctx ~lossless (k : keyframe) : keyframe =
  let declarations =
    normalize_live_declarations ~ctx ~lossless k.declarations
  in
  if declarations == k.declarations then k else { k with declarations }

let rec sanitize_block ~ctx ~lossless (b : statement list) : statement list =
  list_edit_preserve (sanitize_statement ~ctx ~lossless) b

and sanitize_statement ~ctx ~lossless (s : statement) : statement List.edit =
  let nd = normalize_live_declarations ~ctx ~lossless in
  match s with
  | Rule r ->
      let declarations = nd r.declarations in
      let nested = sanitize_block ~ctx ~lossless r.nested in
      let r' = rule_with_declarations_and_nested r declarations nested in
      if r' == r then List.Keep else List.Replace (Rule r')
  | Declarations d ->
      let d' = nd d in
      if d' == d then List.Keep else List.Replace (Declarations d')
  | Page (n, d) ->
      let d' = nd d in
      if d' == d then List.Keep else List.Replace (Page (n, d'))
  | Page_with_margins (n, descs, margins) ->
      let descs' = nd descs in
      let margins' =
        list_map_preserve
          (fun m ->
            let descriptors = nd m.descriptors in
            if descriptors == m.descriptors then m else { m with descriptors })
          margins
      in
      if descs' == descs && margins' == margins then List.Keep
      else List.Replace (Page_with_margins (n, descs', margins'))
  | Position_try (n, d) ->
      let d' = nd d in
      if d' == d then List.Keep else List.Replace (Position_try (n, d'))
  | Supports_condition (n, d) ->
      let d' = nd d in
      if d' == d then List.Keep else List.Replace (Supports_condition (n, d'))
  | Keyframes (n, ks) ->
      let ks' = list_map_preserve (sanitize_keyframe ~ctx ~lossless) ks in
      if ks' == ks then List.Keep else List.Replace (Keyframes (n, ks'))
  | Webkit_keyframes (n, ks) ->
      let ks' = list_map_preserve (sanitize_keyframe ~ctx ~lossless) ks in
      if ks' == ks then List.Keep else List.Replace (Webkit_keyframes (n, ks'))
  | Moz_keyframes (n, ks) ->
      let ks' = list_map_preserve (sanitize_keyframe ~ctx ~lossless) ks in
      if ks' == ks then List.Keep else List.Replace (Moz_keyframes (n, ks'))
  | Layer (n, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Layer (n, b'))
  | Media (c, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Media (c, b'))
  | Container (n, c, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Container (n, c, b'))
  | Supports (c, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Supports (c, b'))
  | Moz_document (c, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Moz_document (c, b'))
  | Starting_style b ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Starting_style b')
  | When (c, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (When (c, b'))
  | Else (c, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Else (c, b'))
  | Origin (o, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Origin (o, b'))
  | Scope (s1, s2, b) ->
      let b' = sanitize_block ~ctx ~lossless b in
      if b' == b then List.Keep else List.Replace (Scope (s1, s2, b'))
  | Property r ->
      let initial_value =
        match r.initial_value with
        | None -> r.initial_value
        | Some value ->
            let value' = Variables.normalize_value ~lossless r.syntax value in
            if value' == value then r.initial_value else Some value'
      in
      if initial_value == r.initial_value then List.Keep
      else List.Replace (Property { r with initial_value })
  | Unknown_at_rule _ -> List.Drop
  | _ -> List.Keep

let run_pipeline ~ctx ~enforce_spec ~aggressive stylesheet =
  if not aggressive then statements_top_level ~ctx ~enforce_spec stylesheet
  else
    (* Re-run the top-level pipeline until the AST stops changing or a small
       iteration cap fires. Each subsequent pass may shrink the input again
       because earlier passes (vendor-alias drop, shorthand composition) may
       have exposed new merge / factoring opportunities the previous pass didn't
       see. *)
    let rec loop n stmts =
      if n <= 0 then stmts
      else
        let next = statements_top_level ~ctx ~enforce_spec stmts in
        if next == stmts then stmts else loop (n - 1) next
    in
    loop 5 stylesheet

let has_top_level_selector_list (stylesheet : t) =
  List.exists
    (function
      | Stylesheet.Rule r -> Selector.is_compound_list r.selector | _ -> false)
    stylesheet

(* Compound-list extension in [combine_identical_global] reads more of the
   factor pipeline's potential moves when each commit can land, but it can also
   block better downstream [factor_anchor] decisions by locking in a greedy
   local merge. The trade-off is input-specific and not predictable from the AST
   shape alone, so we race the two settings and emit whichever stylesheet
   serializes shorter. The race is skipped when no [Selector.List] rule exists
   at the top level - the relaxation can only matter for those, so the second
   pipeline run would just pay wall-clock for nothing. *)
let race_extend_lists ~run ~strict stylesheet =
  if not (has_top_level_selector_list stylesheet) then strict
  else
    let extended = run ~extend_lists:true in
    let size s = Pp.size ~minify:true pp_stylesheet s in
    if size extended < size strict then extended else strict

(* CSS Custom Properties: a rule whose declarations are all custom properties,
   each declared nowhere else in the stylesheet, is position-independent - a
   [var()] resolves at computed-value time, and a globally-unique custom
   property cannot conflict with another declaration. Canonicalise such a rule's
   position (hoist to the front of its context, sorted) so two stylesheets that
   differ only by where the rule sits optimise to the same output. A custom
   property declared more than once is left untouched: its declarations' order
   is cascade-significant. *)
let count_custom_prop_decls (stmts : statement list) : (string, int) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  let bump n =
    Hashtbl.replace tbl n (1 + Option.value ~default:0 (Hashtbl.find_opt tbl n))
  in
  let count_decls decls =
    List.iter
      (fun d ->
        match Variables.custom_declaration_name d with
        | Some n -> bump n
        | None -> ())
      decls
  in
  let rec walk (stmt : statement) =
    match stmt with
    | Rule r ->
        count_decls r.declarations;
        List.iter walk r.nested
    | Declarations decls -> count_decls decls
    | _ -> iter_statement_block walk stmt
  in
  List.iter walk stmts;
  tbl

let is_independent_custom_prop_rule counts (stmt : statement) : bool =
  match stmt with
  | Rule r when r.nested = [] && r.declarations <> [] ->
      List.for_all
        (fun d ->
          match Variables.custom_declaration_name d with
          | Some n -> Hashtbl.find_opt counts n = Some 1
          | None -> false)
        r.declarations
  | _ -> false

(* Anchors that bound a hoist segment: order-significant prelude rules
   ([@charset] / [@import] / a bare [@layer] order declaration must precede
   style rules) and [@property] registrations (kept before their uses so the
   reorder stays minimal). Independent custom-prop rules move only within an
   anchor-bounded segment, never across an anchor. *)
let is_hoist_anchor (stmt : statement) : bool =
  match stmt with
  | Charset _ | Import _ | Property _ -> true
  | Layer (_, []) -> true
  | _ -> false

let rec phys_equal_list a b =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys -> x == y && phys_equal_list xs ys
  | _ -> false

let custom_prop_sort_key stmt =
  Pp.to_string ~minify:true (fun ctx s -> pp_stylesheet ctx [ s ]) stmt

let hoist_custom_prop_segment counts seg =
  let hoistable, rest =
    List.partition (is_independent_custom_prop_rule counts) seg
  in
  match hoistable with
  | [] -> seg
  | _ ->
      let hoistable =
        List.stable_sort
          (fun a b ->
            String.compare (custom_prop_sort_key a) (custom_prop_sort_key b))
          hoistable
      in
      let result = hoistable @ rest in
      if phys_equal_list result seg then seg else result

(* Split a level into anchor-bounded segments and hoist within each. *)
let rec segment_hoist counts = function
  | [] -> []
  | stmt :: rest when is_hoist_anchor stmt -> stmt :: segment_hoist counts rest
  | stmts ->
      let rec span acc = function
        | s :: tl when not (is_hoist_anchor s) -> span (s :: acc) tl
        | tl -> (List.rev acc, tl)
      in
      let seg, after = span [] stmts in
      hoist_custom_prop_segment counts seg @ segment_hoist counts after

let rec recurse_custom_prop_blocks counts (stmt : statement) : statement =
  let go b = canonicalize_custom_prop_blocks counts b in
  match stmt with
  | Rule r ->
      let nested = go r.nested in
      if nested == r.nested then stmt else Rule { r with nested }
  | Layer (n, b) ->
      let b' = go b in
      if b' == b then stmt else Layer (n, b')
  | Media (m, b) ->
      let b' = go b in
      if b' == b then stmt else Media (m, b')
  | Container (n, c, b) ->
      let b' = go b in
      if b' == b then stmt else Container (n, c, b')
  | Supports (s, b) ->
      let b' = go b in
      if b' == b then stmt else Supports (s, b')
  | Moz_document (c, b) ->
      let b' = go b in
      if b' == b then stmt else Moz_document (c, b')
  | When (c, b) ->
      let b' = go b in
      if b' == b then stmt else When (c, b')
  | Else (c, b) ->
      let b' = go b in
      if b' == b then stmt else Else (c, b')
  | Starting_style b ->
      let b' = go b in
      if b' == b then stmt else Starting_style b'
  | Origin (o, b) ->
      let b' = go b in
      if b' == b then stmt else Origin (o, b')
  | Scope (s, e, b) ->
      let b' = go b in
      if b' == b then stmt else Scope (s, e, b')
  | other -> other

and canonicalize_custom_prop_blocks counts (stmts : statement list) :
    statement list =
  let recursed = list_map_preserve (recurse_custom_prop_blocks counts) stmts in
  let hoisted = segment_hoist counts recursed in
  if phys_equal_list hoisted recursed then recursed else hoisted

let canonicalize_custom_prop_position (stmts : statement list) : statement list
    =
  canonicalize_custom_prop_blocks (count_custom_prop_decls stmts) stmts

let stylesheet ?scope ?(flatten_nesting = false) ?(lossless = false)
    ?(enforce_spec = false) ?(aggressive = false) (stylesheet : t) : t =
  Selector_summary.clear_memo ();
  reset_counters ();
  let scope = Option.value scope ~default:`Fragment in
  let ctx = single_valued_calc_ctx stylesheet in
  let stylesheet = sanitize_block ~ctx ~lossless stylesheet in
  let stylesheet =
    if flatten_nesting then Flatten.block stylesheet else stylesheet
  in
  let stylesheet = promote_registered_custom_properties ~lossless stylesheet in
  let registered = registered_foldable stylesheet in
  let stylesheet = prune_position_try_fallbacks ~scope stylesheet in
  let run ~extend_lists =
    let ctx = Ctx.v ~lossless ~aggressive ~extend_lists ~registered scope in
    run_pipeline ~ctx ~enforce_spec ~aggressive stylesheet
  in
  let strict = run ~extend_lists:false in
  race_extend_lists ~run ~strict stylesheet |> canonicalize_custom_prop_position
