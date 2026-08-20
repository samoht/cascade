(** CSS optimization implementation *)

open Declaration
open Stylesheet
open Common
module String_set = Set.Make (String)

let src = Logs.Src.create "cascade.optimize" ~doc:"Cascade CSS optimizer"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Edge Model} *)

type scope = Ctx.scope

(* Optimisation context threaded from the entry points to the shorthand
   composers. [scope] drives fragment-vs-stylesheet decisions; [registered]
   reports whether a custom property has an [@property] initial-value, so
   folding its [var()] into a shorthand cannot widen an
   invalid-at-computed-value failure. Default: fragment, nothing registered. *)
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
let stylesheet_key stmts = Pp.to_string ~minify:true pp_stylesheet stmts

(* A block holds a conditional named layer when it directly contains a named
   [@layer] block with content. Unwrapping a known-true [@supports] around it
   would move that layer's rules and its place in the layer order (CSS Cascade
   6.4) from conditional to unconditional, so the wrapper stays even when
   baseline-true. Bare [@layer name;] and self-guarding declarations carry no
   rules or side effect, so both unwrap freely. *)
let block_introduces_layer_order stmts =
  List.exists (function Layer (Some _, _ :: _) -> true | _ -> false) stmts

(* Pop the run of [Rule]s most recently pushed onto a reversed accumulator, in
   forward order, with the remaining accumulator. When unwrapping a known-true
   [@supports] exposes its inner rules, the preceding rules rejoin the merge
   pass so an adjacency the wrapper hid can collapse. *)
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

let factor_rules_incremental ?cache ~ctx rules =
  Factor.run ?cache ~ctx ~finalize:(finalize_rule_without_nested ~ctx) rules

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

(* A selector list holding a vendor pseudo-element is invalidated as a whole by
   a browser that does not know that pseudo-element, so every other selector in
   the list silently loses the declarations too. The grouping passes already
   refuse to build such a list; one the author wrote gets the same treatment
   here, so the risky branches stand alone and the rest keep their rule. *)
let split_vendor_selector_lists (stmts : statement list) : statement list =
  List.concat_map
    (fun stmt ->
      match stmt with
      | Rule ({ selector = Selector.List parts; _ } as nr)
        when List.length parts > 1
             && List.exists Merge.vendor parts
             && not (List.for_all Merge.vendor parts) ->
          let risky, safe = List.partition Merge.vendor parts in
          let of_parts = function
            | [] -> []
            | [ one ] -> [ Rule { nr with selector = one } ]
            | many -> [ Rule { nr with selector = Selector.List many } ]
          in
          List.concat_map (fun sel -> of_parts [ sel ]) risky @ of_parts safe
      | other -> [ other ])
    stmts

let rec statements ?factor_cache ~ctx ~enforce_spec (stmts : statement list) :
    statement list =
  match stmts with
  | [] -> stmts
  | _ ->
      let optimize_merged_block = statements ?factor_cache ~ctx ~enforce_spec in
      (* [drop_misplaced_imports] runs first: an [@import] after a style rule is
         invalid and ignored by browsers, so it must not act as a cascade
         boundary. Stripping it up front lets the rules it falsely separated
         merge in this same pass, keeping [statements] idempotent (stripping
         after the merge would leave a re-run to combine them). *)
      let stmts' =
        let stmts =
          drop_misplaced_imports stmts
          |> merge_named_layers_by_name |> split_vendor_selector_lists
        in
        let stmts =
          process_statements ?factor_cache ~ctx ~enforce_spec [] stmts
        in
        let stmts =
          if Ctx.regroup ctx then synthesize_nesting_statements stmts else stmts
        in
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

(* Process a cursor of (reverse acc, remaining list, pending segment stack),
   popping the next pending segment when the list is exhausted. This lets
   [process_supports_statement] splice three segments without materialising
   their concatenation. *)
and process_statements ?factor_cache ~ctx ~enforce_spec
    ?(pending : statement list list = []) (acc : statement list)
    (remaining : statement list) : statement list =
  match remaining with
  | [] -> (
      match pending with
      | [] -> List.rev acc
      | next :: rest_pending ->
          process_statements ?factor_cache ~ctx ~enforce_spec
            ~pending:rest_pending acc next)
  | (Rule r as stmt) :: rest ->
      process_rule_run ?factor_cache ~ctx ~enforce_spec ~pending acc stmt r rest
  | (Media (cond, block) as stmt) :: rest ->
      process_media_statement ?factor_cache ~ctx ~enforce_spec ~pending acc stmt
        cond block rest
  | (Container (name, cond, block) as stmt) :: rest ->
      process_container_statement ?factor_cache ~ctx ~enforce_spec ~pending acc
        stmt name cond block rest
  | Supports (cond, block) :: rest ->
      process_supports_statement ?factor_cache ~ctx ~enforce_spec ~pending acc
        cond block rest
  | (Scope (start, end_, block) as stmt) :: rest ->
      let start' = option_map_preserve canonicalize_scope_selector start in
      let end_' = option_map_preserve canonicalize_scope_selector end_ in
      let optimized_block = statements ?factor_cache ~ctx ~enforce_spec block in
      let optimized =
        if start' == start && end_' == end_ && optimized_block == block then
          stmt
        else Scope (start', end_', optimized_block)
      in
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending
        (optimized :: acc) rest
  | (Origin (origin, block) as stmt) :: rest ->
      let optimized_block = statements ?factor_cache ~ctx ~enforce_spec block in
      let optimized =
        if optimized_block == block then stmt
        else Origin (origin, optimized_block)
      in
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending
        (optimized :: acc) rest
  | (Layer (name, block) as stmt) :: rest ->
      process_layer_statement ?factor_cache ~ctx ~enforce_spec ~pending acc stmt
        name block rest
  | (Import import as stmt) :: rest ->
      process_import_statement ?factor_cache ~ctx ~enforce_spec ~pending acc
        stmt import rest
  | hd :: rest ->
      (* Other statement types - keep as-is *)
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending (hd :: acc)
        rest

and process_rule_run ?factor_cache ~ctx ~enforce_spec ~pending acc stmt r rest =
  let plain_stmts, plain_rules, rest = collect_rules [ stmt ] [ r ] rest in
  let optimized = rules_aux ?factor_cache ~ctx ~enforce_spec plain_rules in
  let as_statements =
    if optimized == plain_rules then plain_stmts
    else List.map (fun r -> Rule r) optimized
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~pending
    (List.rev_append as_statements acc)
    rest

and process_media_statement ?factor_cache ~ctx ~enforce_spec ~pending acc stmt
    cond block rest =
  let cond = if enforce_spec then cond else Media.lower_for_minify cond in
  let optimized_block = statements ?factor_cache ~ctx ~enforce_spec block in
  let optimized =
    if
      (cond == match stmt with Media (c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Media (cond, optimized_block)
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~pending
    (optimized :: acc) rest

and process_container_statement ?factor_cache ~ctx ~enforce_spec ~pending acc
    stmt name cond block rest =
  let cond =
    if enforce_spec then cond
    else option_map_preserve Container.lower_for_minify cond
  in
  let optimized_block = statements ?factor_cache ~ctx ~enforce_spec block in
  let optimized =
    if
      (cond == match stmt with Container (_, c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Container (name, cond, optimized_block)
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~pending
    (optimized :: acc) rest

and process_supports_statement ?factor_cache ~ctx ~enforce_spec ~pending acc
    cond block rest =
  let optimized_block = statements ?factor_cache ~ctx ~enforce_spec block in
  let baseline =
    if enforce_spec then `Cond cond else Supports.simplify_baseline cond
  in
  match baseline with
  | `True when block_introduces_layer_order optimized_block ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending
        (Supports (cond, optimized_block) :: acc)
        rest
  | `True ->
      let trailing, acc = pop_trailing_rules acc in
      (* [trailing], [optimized_block], and [rest] must reach
         [process_statements] as a SINGLE list so [collect_rules] can pull
         adjacent rules across the segment boundary into one run - the whole
         point of unwrapping a baseline-true @supports. Tail-recursive
         [List.concat] so a long [optimized_block] cannot stack-overflow. *)
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending acc
        (List.concat [ trailing; optimized_block; rest ])
  | `False ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending acc rest
  | `Cond cond' ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~pending
        (Supports (cond', optimized_block) :: acc)
        rest

and process_layer_statement ?factor_cache ~ctx ~enforce_spec ~pending acc stmt
    name block rest =
  let optimized_block = statements ?factor_cache ~ctx ~enforce_spec block in
  if is_layer_empty optimized_block then
    match name with
    | Some layer_name ->
        let all_names, remaining =
          collect_empty_layer_names [ layer_name ] rest
        in
        process_statements ?factor_cache ~ctx ~enforce_spec ~pending
          (Layer_decl all_names :: acc)
          remaining
    | None ->
        process_statements ?factor_cache ~ctx ~enforce_spec ~pending acc rest
  else
    let optimized =
      if optimized_block == block then stmt else Layer (name, optimized_block)
    in
    process_statements ?factor_cache ~ctx ~enforce_spec ~pending
      (optimized :: acc) rest

and process_import_statement ?factor_cache ~ctx ~enforce_spec ~pending acc stmt
    import rest =
  let stmt =
    match import.supports with
    | Some cond
      when (not enforce_spec) && Supports.simplify_baseline cond = `True ->
        Import { import with supports = None }
    | _ -> stmt
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~pending (stmt :: acc)
    rest

and rules_aux ?factor_cache ~ctx ~enforce_spec (rules : rule list) : rule list =
  (* First optimize each rule's nested statements recursively, then drop the
     redundant nesting prefix (see [drop_nesting_prefix]). *)
  let with_optimized_nested =
    list_map_preserve
      (fun rule ->
        let nested =
          match rule.nested with
          | [] -> []
          | nested ->
              let nested = statements ?factor_cache ~ctx ~enforce_spec nested in
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
  (* Apply local rule normalization before the DAG factor scheduler decides
     global selector/declaration grouping. *)
  let prepared =
    list_map_preserve (single_rule_without_nested ~ctx) with_optimized_nested
  in
  let prepared = Rule.drop_shadowed prepared in
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
  let factored =
    if Ctx.regroup ctx then
      factor_rules_incremental ?cache:factor_cache ~ctx prepared
    else prepared
  in
  (* After factoring so the greedy scheduler sees the unconstrained input (a
     local pre-merge can lock a pair together and hide a larger group): this
     only picks up the merges factoring did not make because its preflight
     skipped the run or the transfer gate discarded its result. *)
  Rule.merge_adjacent_identical ~ctx factored

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

(* CSS Cascade 5 sec. 6.4.2: rules from all occurrences of a named layer
   accumulate. Merge same-name blocks at the FIRST NON-EMPTY occurrence so
   content stays where the author placed the layer's first real declaration.
   Leading empty blocks ([@layer name {}]) stay in place for the
   [empty-named-layer -> Layer_decl] normalisation in [process_statements]. *)
let statements_top_level ?factor_cache ~ctx ~enforce_spec
    (stmts : statement list) : statement list =
  let optimize_merged_block = statements ?factor_cache ~ctx ~enforce_spec in
  let stmts' =
    statements ?factor_cache ~ctx ~enforce_spec stmts
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
    sec. 5.4.1 (unknown at-rules are discarded). *)
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

(* CSS Properties and Values API 1 sec. 2: an [@property --name] registers a
   typed syntax, lifting later [--name: ...] uses out of the opaque-token-stream
   rule. Apply in source order so a registration only affects later uses; a
   duplicate name overwrites, matching the browser registry. *)
let try_promote_custom_with (type a) (syntax : a Variables.syntax) components =
  match syntax with
  | Variables.Color -> Properties.try_read_custom_color components
  | Variables.Length -> Properties.try_read_custom_length components
  | Variables.Length_percentage ->
      Properties.try_read_custom_length_percentage components
  | Variables.Number -> Properties.try_read_custom_number components
  | Variables.Percentage -> Properties.try_read_custom_percentage components
  | Variables.Angle -> Properties.try_read_custom_angle components
  | Variables.Time -> Properties.try_read_custom_time components
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
        _;
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

(* Compressibility pass: put each rule's declarations in a deterministic
   cross-rule order so gzip back-references line up, keeping cascade-significant
   (overlapping) pairs in place. A transfer-objective win, so gated to it. *)
let canonicalize_declaration_order stmts =
  let rec walk_stmt (stmt : statement) : statement =
    match stmt with
    | Rule r ->
        let declarations = Rule_order.canonical_declarations r.declarations in
        let nested = list_map_preserve walk_stmt r.nested in
        let r' = rule_with_declarations_and_nested r declarations nested in
        if r' == r then stmt else Rule r'
    | Media _ | Container _ | Supports _ | Layer _ | Origin _ | Scope _
    | Starting_style _ | Moz_document _ | When _ | Else _ ->
        map_statement_block_preserve walk_stmt stmt
    | _ -> stmt
  in
  list_map_preserve walk_stmt stmts

(* Under closed-stylesheet scope every [@position-try --name] rule is known. A
   [position-try-fallbacks] entry whose name has no matching rule cannot match
   at runtime, so prune unknown [Name] arms (keeping [Flip_*] and [Var]). When
   every arm is pruned the whole declaration drops (equivalent to its
   initial). *)
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
        _;
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

let rec statement_rule_count = function
  | Rule _ -> 1
  | stmt ->
      let count = ref 0 in
      iter_statement_block
        (fun stmt -> count := !count + statement_rule_count stmt)
        stmt;
      !count

let stylesheet_rule_count stmts =
  List.fold_left (fun count stmt -> count + statement_rule_count stmt) 0 stmts

let run_pipeline ~ctx ~enforce_spec ~aggressive stylesheet =
  (* Re-run the top-level pipeline until the AST stops changing or a small cap
     fires. A pass may shrink the input again because an earlier one
     (vendor-alias drop, shorthand composition, media/support merging) exposed a
     rule run the DAG factor pass can now optimize. Rule count is
     non-increasing, so this converges; [aggressive] only widens the cap. *)
  let cap =
    if aggressive then 6
    else if stylesheet_rule_count stylesheet > 128 then 1
    else 5
  in
  let factor_cache = Factor.cache () in
  let rec loop n key stmts =
    if n <= 0 then stmts
    else
      let next = statements_top_level ~factor_cache ~ctx ~enforce_spec stmts in
      if next == stmts then stmts
      else if Stylesheet.equal next stmts then stmts
      else
        let next_key = stylesheet_key next in
        if String.equal key next_key then next else loop (n - 1) next_key next
  in
  loop cap (stylesheet_key stylesheet) stylesheet

(* Bare names of every custom property referenced through [var()] in the tree.
   Scans the serialized declarations, so it sees references inside opaque values
   too and over-keeps (a [var(] inside a string counts): it must never miss a
   live reference, or the prune below drops a binding still in use. *)
let referenced_custom_props (stmts : statement list) : (string, unit) Hashtbl.t
    =
  let tbl = Hashtbl.create 64 in
  let bare n =
    if String.length n >= 2 && n.[0] = '-' && n.[1] = '-' then
      String.sub n 2 (String.length n - 2)
    else n
  in
  let note_decls =
    List.iter (fun d ->
        List.iter
          (fun n -> Hashtbl.replace tbl (bare n) ())
          (Variables.var_refs_in_value_string
             (Declaration.string_of_declaration ~minify:true d)))
  in
  let rec walk stmt =
    match stmt with
    | Rule r ->
        note_decls r.declarations;
        List.iter walk r.nested
    | Declarations decls -> note_decls decls
    | _ -> iter_statement_block walk stmt
  in
  List.iter walk stmts;
  tbl

(* Drop custom-property bindings referenced by no [var()] - a dead binding has
   no rendering effect (an emptied rule is removed at serialization). Caller
   opt-in only: the no-runtime-reader, complete-stylesheet assumption is theirs,
   like [Inline.vars]. *)
let drop_unused_custom_props (stmts : statement list) : statement list =
  let referenced = referenced_custom_props stmts in
  let bare name =
    if String.length name >= 2 && name.[0] = '-' && name.[1] = '-' then
      String.sub name 2 (String.length name - 2)
    else name
  in
  let keep_decl d =
    match Variables.custom_declaration_name d with
    | Some name -> Hashtbl.mem referenced (bare name)
    | None -> true
  in
  let rec prune stmt =
    match stmt with
    | Rule r ->
        let declarations = list_filter_preserve keep_decl r.declarations in
        let nested = list_map_preserve prune r.nested in
        if declarations == r.declarations && nested == r.nested then stmt
        else Rule { r with declarations; nested }
    | Declarations decls ->
        let decls' = list_filter_preserve keep_decl decls in
        if decls' == decls then stmt else Declarations decls'
    | _ -> map_statement_block_preserve prune stmt
  in
  list_map_preserve prune stmts

let stylesheet ?scope ?(flatten_nesting = false) ?(lossless = false)
    ?(enforce_spec = false) ?(aggressive = false) ?(regroup = true)
    ?(closed_world = false) ?(objective = `Transfer)
    ?(prune_unused_custom_props = false) ?stats (stylesheet : t) : t =
  Selector_summary.clear_memo ();
  let scope = Option.value scope ~default:`Fragment in
  let ctx = single_valued_calc_ctx stylesheet in
  let stylesheet = sanitize_block ~ctx ~lossless stylesheet in
  let stylesheet =
    if flatten_nesting then Flatten.block stylesheet else stylesheet
  in
  let stylesheet = promote_registered_custom_properties ~lossless stylesheet in
  let registered = registered_foldable stylesheet in
  let stylesheet = prune_position_try_fallbacks ~scope stylesheet in
  let ctx =
    Ctx.v ~lossless ~aggressive ~regroup ~closed_world ~objective ~enforce_spec
      ~registered ?stats scope
  in
  let result =
    run_pipeline
      ~ctx:(Ctx.with_extend_lists true ctx)
      ~enforce_spec ~aggressive stylesheet
  in
  let result =
    if prune_unused_custom_props then drop_unused_custom_props result
    else result
  in
  let result =
    if lossless then canonicalize_declaration_order result else result
  in
  Log.debug (fun m ->
      let c = (Stats.snapshot (Ctx.stats ctx)).counters in
      m
        "optimized: %d factoring fixpoints run, %d skipped, %d reverted by the \
         transfer gate, %d bytes saved"
        c.factor_fixpoints_run c.factor_fixpoints_skipped
        c.factor_transfer_reverts c.factor_bytes_saved);
  result
