(** CSS optimization implementation *)

open Declaration
open Stylesheet
open Common
module String_set = Set.Make (String)

let src = Logs.Src.create "cascade.optimize" ~doc:"Cascade CSS optimizer"

module Log = (val Logs.src_log src : Logs.LOG)

(** {1 Edge Model} *)

type scope = Ctx.scope
type objective = [ `Raw | `Transfer ]
type browser_version = int * int

type targets = {
  chrome : browser_version;
  firefox : browser_version;
  safari : browser_version;
  ios_safari : browser_version;
}

let evergreen_targets =
  {
    chrome = (111, 0);
    firefox = (128, 0);
    safari = (16, 4);
    ios_safari = (16, 4);
  }

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
let declaration_run = Rule.declaration_run
let finalize_rule_without_nested = Rule.finalize

(** {1 Statement Optimization} *)

let merge_consecutive_layers = Block.merge_consecutive_layers
let merge_consecutive_media = Block.merge_consecutive_media
let merge_distant_media = Block.merge_distant_media
let merge_consecutive_supports = Block.merge_consecutive_supports
let merge_consecutive_containers = Block.merge_consecutive_containers
let merge_distant_containers = Block.merge_distant_containers
let merge_consecutive_starting_style = Block.merge_consecutive_starting_style
let is_layer_empty = Block.is_layer_empty
let collect_empty_layer_names = Block.collect_empty_layer_names
let merge_layer_declarations = Block.merge_layer_declarations
let drop_redundant_layer_decls = Block.drop_redundant_layer_decls
let drop_empty_rules = Block.drop_empty_rules
let drop_misplaced_imports = Block.drop_misplaced_imports
let merge_named_layers_by_name = Block.merge_named_layers_by_name
let drop_dead_nested_rules = Nest.drop_dead_nested
let merge_lone_nested_rule = Nest.merge_lone
let hoist_declaration_runs = Nest.hoist_declaration_runs
let synthesize_nesting_statements = Nest.statements

(* Pop the run of [Rule]s most recently pushed onto a reversed accumulator, in
   forward order, with the remaining accumulator. Unwrapping an [@supports] its
   context already answers exposes its inner rules, and the preceding rules
   rejoin the merge pass so an adjacency the wrapper hid can collapse. *)
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

let factor_rules_incremental ?cache ~settle ~ctx rules =
  Factor.run ?cache ~settle ~ctx
    ~finalize:(finalize_rule_without_nested ~ctx)
    rules

(* Keep the cheap rule-local pipeline independent of the stylesheet-wide
   factoring budget. [Rule.finalize] normalizes any shorthand it synthesizes, so
   one linear preparation pass reaches the local fixed point even when a large
   stylesheet limits the global walks. *)
let normalize_rules_locally ~ctx rules =
  list_map_preserve (single_rule_without_nested ~ctx) rules
  |> Rule.drop_shadowed
  |> list_map_preserve
       (finalize_rule_without_nested ~canonicalize_selector:false ~ctx)

let merge_adjacent_and_mark ~ctx invalidated rules =
  let settled = Rule.merge_adjacent_identical ~ctx rules in
  (* Each produced rule is fully normalized by the scheduler's finalizer. A node
     merge is therefore the only settling step that changes the candidate graph
     and requires another global walk. *)
  if settled != rules then invalidated := settled :: !invalidated;
  settled

let factor_rules_until_settled ?factor_cache ~ctx rules =
  let rec loop remaining rules =
    let invalidated = ref [] in
    let next =
      factor_rules_incremental ?cache:factor_cache
        ~settle:(merge_adjacent_and_mark ~ctx invalidated)
        ~ctx rules
    in
    let needs_refactor =
      List.exists (fun rules -> rules == next) !invalidated
    in
    if needs_refactor && remaining > 1 then loop (remaining - 1) next else next
  in
  loop 2 rules

(* [@scope] bounds are parsed selectors; canonicalize them like any other
   selector so a list bound is de-duplicated and ordered consistently. *)
let canonicalize_scope_selector sel = Selector.canonicalize sel

(* CSS Fonts 4 (ED) sec. 4.4 gives the [font-weight] and [font-width]
   descriptors the values of the properties of the same name, so a descriptor
   takes its property's fold. A descriptor is not a declaration and never
   reaches factoring, but the fold is still a node question, so it belongs here
   and not in the serializer. *)
let normalize_font_face_descriptor (desc : font_face_descriptor) :
    font_face_descriptor =
  let weight w = Properties.normalize_property_value Properties.Font_weight w in
  let stretch w =
    Properties.normalize_property_value Properties.Font_stretch w
  in
  match desc with
  | Font_weight value ->
      let value' = weight value in
      if value' == value then desc else Font_weight value'
  | Font_weight_range (low, high) ->
      let low' = weight low in
      let high' = weight high in
      if low' == low && high' == high then desc
      else Font_weight_range (low', high')
  | Font_stretch value ->
      let value' = stretch value in
      if value' == value then desc else Font_stretch value'
  | Font_stretch_range (low, high) ->
      let low' = stretch low in
      let high' = stretch high in
      if low' == low && high' == high then desc
      else Font_stretch_range (low', high')
  | Font_family _ | Src _ | Font_style _ | Font_style_range _ | Font_display _
  | Unicode_range _ | Font_variant _ | Font_feature_settings _
  | Font_variation_settings _ | Font_tech _ | Size_adjust _ | Ascent_override _
  | Descent_override _ | Line_gap_override _ ->
      desc

(* [stmt] is returned unchanged when no descriptor moved: the factoring fixpoint
   reads a no-op off physical identity. *)
let normalize_font_face stmt descriptors =
  let descriptors' =
    list_map_preserve normalize_font_face_descriptor descriptors
  in
  if descriptors' == descriptors then stmt else Font_face descriptors'

(* Nested rule selectors are implicitly relative to the parent [&], so drop a
   redundant leading [& <combinator>] (CSS Nesting 1 sec. 3). *)
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

let rec statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (stmts : statement list) : statement list =
  match stmts with
  | [] -> stmts
  | _ ->
      (* A merge concatenates blocks whose children [process_statements] has
         already optimized. Re-enter only at that new seam: descending again
         walks a nested child once per ancestor and turns linear depth into
         quadratic work. Further merges in the seam pass stay shallow too. *)
      let optimize_merged_block =
        statements ?factor_cache
          ~ctx:(Ctx.with_recurse_blocks false ctx)
          ~enforce_spec ~owner ~supports
      in
      (* [drop_misplaced_imports] runs first: an [@import] after a style rule is
         invalid and ignored by browsers (CSS Cascade L6 sec. 2), so it must not
         act as a cascade boundary. Stripping it up front lets the rules it
         falsely separated merge in this same pass, keeping [statements]
         idempotent (stripping after the merge would leave a re-run to combine
         them). *)
      let stmts' =
        let stmts =
          drop_misplaced_imports stmts
          |> merge_named_layers_by_name |> split_vendor_selector_lists
        in
        let stmts =
          process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
            [] stmts
        in
        let stmts =
          if Ctx.regroup ctx then synthesize_nesting_statements stmts else stmts
        in
        let stmts =
          stmts
          |> merge_consecutive_media ~optimize_merged_block
          |> merge_distant_media ?owner ~optimize_merged_block
          |> merge_consecutive_supports ~optimize_merged_block
          |> merge_consecutive_containers ~optimize_merged_block
          |> merge_distant_containers ?owner ~optimize_merged_block
          |> merge_consecutive_starting_style ~optimize_merged_block
        in
        stmts |> merge_layer_declarations |> drop_empty_rules
      in
      preserve_list stmts stmts'

(* Walk the statement list left to right over a reversed accumulator. *)
and process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (acc : statement list) (remaining : statement list) : statement list =
  match remaining with
  | [] -> List.rev acc
  | (Rule r as stmt) :: rest ->
      process_rule_run ?factor_cache ~ctx ~enforce_spec ~owner ~supports acc
        stmt r rest
  | (Media (cond, block) as stmt) :: rest ->
      process_media_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt cond block rest
  | (Container (name, cond, block) as stmt) :: rest ->
      process_container_statement ?factor_cache ~ctx ~enforce_spec ~owner
        ~supports acc stmt name cond block rest
  | (Supports (cond, block) as stmt) :: rest ->
      process_supports_statement ?factor_cache ~ctx ~enforce_spec ~owner
        ~supports acc stmt cond block rest
  | (Scope (start, end_, block) as stmt) :: rest ->
      let start' = option_map_preserve canonicalize_scope_selector start in
      let end_' = option_map_preserve canonicalize_scope_selector end_ in
      let optimized_block =
        descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
      in
      let optimized =
        if start' == start && end_' == end_ && optimized_block == block then
          stmt
        else Scope (start', end_', optimized_block)
      in
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        (optimized :: acc) rest
  | (Origin (origin, block) as stmt) :: rest ->
      process_group_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt block
        (fun block -> Origin (origin, block))
        rest
  (* The wrapper is opaque but its body is an ordinary block, so it optimises
     like [@media]'s: a group left out here keeps whatever the author wrote and
     [--minify] does nothing inside it. *)
  | (Moz_document (cond, block) as stmt) :: rest ->
      process_group_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt block
        (fun block -> Moz_document (cond, block))
        rest
  | (When (cond, block) as stmt) :: rest ->
      process_group_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt block
        (fun block -> When (cond, block))
        rest
  | (Else (cond, block) as stmt) :: rest ->
      process_group_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt block
        (fun block -> Else (cond, block))
        rest
  | (Starting_style block as stmt) :: rest ->
      process_group_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt block
        (fun block -> Starting_style block)
        rest
  | (Layer (name, block) as stmt) :: rest ->
      process_layer_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        acc stmt name block rest
  | (Import _ as stmt) :: rest ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        (stmt :: acc) rest
  | (Declarations decls as stmt) :: rest ->
      process_declarations_statement ?factor_cache ~ctx ~enforce_spec ~owner
        ~supports acc stmt decls rest
  | (Font_face descriptors as stmt) :: rest ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        (normalize_font_face stmt descriptors :: acc)
        rest
  (* Listed rather than closed with a wildcard: everything left holds no block,
     so a statement that grows one has to be classified above before it
     compiles. *)
  | (( Property _ | Bang_comment _ | Charset _ | Namespace _ | Layer_decl _
     | Supports_condition _ | Keyframes _ | Webkit_keyframes _ | Moz_keyframes _
     | Counter_style _ | Page _ | Page_with_margins _ | Font_palette_values _
     | Font_feature_values _ | View_transition _ | Position_try _ | Viewport _
     | Unknown_at_rule _ ) as hd)
    :: rest ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        (hd :: acc) rest

and descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block =
  if Ctx.recurse_blocks ctx then
    statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
  else block

(* A run of declarations is a declaration list wherever it sits, and nothing
   comes between two writes inside one, so the same deduplication a rule body
   gets applies (CSS Cascade 5 sec. 6.4.4: the later write wins). *)
and process_declarations_statement ?factor_cache ~ctx ~enforce_spec ~owner
    ~supports acc stmt decls rest =
  let decls' = declaration_run ~ctx decls in
  let stmt = if decls' == decls then stmt else Declarations decls' in
  process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (stmt :: acc) rest

and process_group_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    acc stmt block rebuild rest =
  let optimized_block =
    descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
  in
  let optimized =
    if optimized_block == block then stmt else rebuild optimized_block
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (optimized :: acc) rest

and process_rule_run ?factor_cache ~ctx ~enforce_spec ~owner ~supports acc stmt
    r rest =
  let plain_stmts, plain_rules, rest = collect_rules [ stmt ] [ r ] rest in
  let optimized =
    rules_aux ?factor_cache ~ctx ~enforce_spec ~supports plain_rules
  in
  let as_statements =
    if optimized == plain_rules then plain_stmts
    else List.map (fun r -> Rule r) optimized
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (List.rev_append as_statements acc)
    rest

and process_media_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    acc stmt cond block rest =
  let cond = if enforce_spec then cond else Media.lower_for_minify cond in
  let optimized_block =
    descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
  in
  let optimized =
    if
      (cond == match stmt with Media (c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Media (cond, optimized_block)
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (optimized :: acc) rest

and process_container_statement ?factor_cache ~ctx ~enforce_spec ~owner
    ~supports acc stmt name cond block rest =
  let cond =
    if enforce_spec then cond
    else option_map_preserve Container.lower_for_minify cond
  in
  let optimized_block =
    descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
  in
  let optimized =
    if
      (cond == match stmt with Container (_, c, _) -> c | _ -> assert false)
      && optimized_block == block
    then stmt
    else Container (name, cond, optimized_block)
  in
  process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    (optimized :: acc) rest

and process_supports_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    acc stmt cond block rest =
  match Supports.simplify_under ~context:supports cond with
  (* The guard selects no user agent, so nothing in its block is ever applied
     (CSS Conditional 3 sec. 2). Its cascade layers go with it: CSS Cascade 5
     sec. 6.4.1 keeps a layer defined inside a conditional group rule out of the
     layer order unless the condition is true, and a feature query is answered
     once for the whole document rather than per element. *)
  | `False ->
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports acc
        rest
  (* The guard holds wherever the block enclosing it does, so its contents apply
     exactly where they already sit. Splicing them into the enclosing stream -
     with the run of rules already accumulated - lets an adjacency the wrapper
     hid collapse. *)
  | `True ->
      let block' =
        descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
      in
      let trailing, acc = pop_trailing_rules acc in
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports acc
        (List.concat [ trailing; block'; rest ])
  | `Cond cond' ->
      let block' =
        descend_block ?factor_cache ~ctx ~enforce_spec ~owner
          ~supports:(cond' :: supports) block
      in
      let optimized =
        if cond' == cond && block' == block then stmt
        else Supports (cond', block')
      in
      process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
        (optimized :: acc) rest

and process_layer_statement ?factor_cache ~ctx ~enforce_spec ~owner ~supports
    acc stmt name block rest =
  let optimized_block =
    descend_block ?factor_cache ~ctx ~enforce_spec ~owner ~supports block
  in
  if is_layer_empty optimized_block then
    match name with
    (* A style rule holds no layer-order declaration, so inside one the empty
       block keeps its block form; folding it to [Layer_decl] emits CSS every
       engine drops. *)
    | Some _ when Option.is_some owner ->
        process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
          (Layer (name, optimized_block) :: acc)
          rest
    | Some layer_name ->
        let all_names, remaining =
          collect_empty_layer_names [ layer_name ] rest
        in
        process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
          (Layer_decl all_names :: acc)
          remaining
    | None ->
        process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports acc
          rest
  else
    let optimized =
      if optimized_block == block then stmt else Layer (name, optimized_block)
    in
    process_statements ?factor_cache ~ctx ~enforce_spec ~owner ~supports
      (optimized :: acc) rest

(* Optimize one rule's nested statements recursively, then drop the redundant
   nesting prefix (see [drop_nesting_prefix]) and let a run written after them
   rejoin the rule's own declarations wherever the cascade allows. The body is
   the rule's, so it optimizes with the rule as owner. Dead nested rules go
   before the hoist, so a declaration behind one is no longer held back. *)
and rule_with_optimized_nested ?factor_cache ~ctx ~enforce_spec ~supports rule =
  let nested =
    match (Ctx.recurse_blocks ctx, rule.nested) with
    | false, nested -> nested
    | true, [] -> []
    | true, nested ->
        let nested =
          statements ?factor_cache ~ctx ~enforce_spec ~owner:(Some rule)
            ~supports nested
        in
        if enforce_spec then nested
        else list_map_preserve drop_nesting_prefix nested
  in
  let rule = drop_dead_nested_rules (rule_with_nested rule nested) in
  let rule = hoist_declaration_runs rule in
  let rule =
    let selector = Selector.canonicalize rule.selector in
    if selector == rule.selector then rule else { rule with selector }
  in
  merge_lone_nested_rule rule

and rules_aux ?factor_cache ~ctx ~enforce_spec ~supports (rules : rule list) :
    rule list =
  let with_optimized_nested =
    list_map_preserve
      (rule_with_optimized_nested ?factor_cache ~ctx ~enforce_spec ~supports)
      rules
  in
  (* Apply local rule normalization before the DAG factor scheduler decides
     global selector/declaration grouping. *)
  let prepared = normalize_rules_locally ~ctx with_optimized_nested in
  (* Factoring is greedy and global: extracting one shared declaration subset
     can leave behind leftovers that are themselves factorable. The local linear
     optimizations above always run; this incremental gate only decides whether
     the expensive global factoring fixpoint is likely to buy enough bytes to
     justify the full indexed scheduler walk. *)
  let factored =
    if Ctx.regroup ctx then begin
      (* The global scheduler reaches a fixed point for the graph it receives,
         but settling its result can merge nodes and expose a new graph. Repeat
         only when that cheap postprocessing changed one of the transfer gate's
         alternatives; most sheets still pay for one global walk. *)
      factor_rules_until_settled ?factor_cache ~ctx prepared
    end
    else merge_adjacent_and_mark ~ctx (ref []) prepared
  in
  (* After factoring so the greedy scheduler sees the unconstrained input (a
     local pre-merge can lock a pair together and hide a larger group): this
     only picks up the merges factoring did not make because its preflight
     skipped the run or the transfer gate discarded its result. *)
  factored

(* CSS Animations 1 sec. 3: [@keyframes name] re-declaration overrides the
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
  let optimize_merged_block =
    statements ?factor_cache
      ~ctx:(Ctx.with_recurse_blocks false ctx)
      ~enforce_spec ~owner:None ~supports:[]
  in
  let stmts' =
    statements ?factor_cache ~ctx ~enforce_spec ~owner:None ~supports:[] stmts
    |> merge_consecutive_layers ~optimize_merged_block
    |> drop_redundant_layer_decls |> drop_shadowed_keyframes
  in
  preserve_list stmts stmts'

let single_rule ?scope (rule : rule) : rule =
  let ctx = ctx_of_scope scope in
  let rule =
    hoist_declaration_runs
      {
        rule with
        nested =
          statements ~ctx ~enforce_spec:false ~owner:(Some rule) ~supports:[]
            rule.nested;
      }
  in
  {
    rule with
    declarations = deduplicate_declarations_with ~ctx rule.declarations;
  }

let rules ?scope (rules : rule list) : rule list =
  rules_aux ~ctx:(ctx_of_scope scope) ~enforce_spec:false ~supports:[] rules

(** {1 Nesting Flattening} *)

let flatten_nesting = Flatten.block

(** {1 Stylesheet Optimization} *)

type webkit_fallback =
  | User_select_fallback
  | Backdrop_filter_fallback
  | Hyphens_fallback
  | Text_decoration_color_fallback
  | Mask_fallback
  | Mask_image_fallback
  | Mask_position_fallback
  | Mask_size_fallback
  | Mask_repeat_fallback
  | Mask_clip_fallback
  | Mask_origin_fallback

(* A target that cannot read the standard property at all needs the fallback for
   every value of it. A target that reads both spellings needs it only where the
   value is not settled at parse time. *)
type fallback_condition = Any_value | Unresolved_value

type webkit_fallback_spec =
  | Typed_fallback : {
      kind : webkit_fallback;
      property : 'a Properties.property;
      webkit_property : 'a Properties.property;
      name : string;
      webkit_name : string;
      condition : fallback_condition;
    }
      -> webkit_fallback_spec
  | Mask_fallback_spec of { name : string; webkit_name : string }

let typed_fallback ?(condition = Any_value) kind property webkit_property name
    webkit_name =
  Typed_fallback
    { kind; property; webkit_property; name; webkit_name; condition }

let webkit_fallback_specs =
  [
    typed_fallback User_select_fallback User_select Webkit_user_select
      "user-select" "-webkit-user-select";
    typed_fallback Backdrop_filter_fallback Backdrop_filter
      Webkit_backdrop_filter "backdrop-filter" "-webkit-backdrop-filter";
    typed_fallback Hyphens_fallback Hyphens Webkit_hyphens "hyphens"
      "-webkit-hyphens";
    typed_fallback ~condition:Unresolved_value Text_decoration_color_fallback
      Text_decoration_color Webkit_text_decoration_color "text-decoration-color"
      "-webkit-text-decoration-color";
    Mask_fallback_spec { name = "mask"; webkit_name = "-webkit-mask" };
    typed_fallback Mask_image_fallback Mask_image Webkit_mask_image "mask-image"
      "-webkit-mask-image";
    typed_fallback Mask_position_fallback Mask_position Webkit_mask_position
      "mask-position" "-webkit-mask-position";
    typed_fallback Mask_size_fallback Mask_size Webkit_mask_size "mask-size"
      "-webkit-mask-size";
    typed_fallback Mask_repeat_fallback Mask_repeat Webkit_mask_repeat
      "mask-repeat" "-webkit-mask-repeat";
    typed_fallback Mask_clip_fallback Mask_clip Webkit_mask_clip "mask-clip"
      "-webkit-mask-clip";
    typed_fallback Mask_origin_fallback Mask_origin Webkit_mask_origin
      "mask-origin" "-webkit-mask-origin";
  ]

let fallback_spec_kind = function
  | Typed_fallback { kind; _ } -> kind
  | Mask_fallback_spec _ -> Mask_fallback

let fallback_spec_condition = function
  | Typed_fallback { condition; _ } -> condition
  | Mask_fallback_spec _ -> Any_value

let fallback_spec_names = function
  | Typed_fallback { name; webkit_name; _ } -> (name, webkit_name)
  | Mask_fallback_spec { name; webkit_name } -> (name, webkit_name)

let fallback_spec_by_kind kind =
  List.find_opt
    (fun spec -> fallback_spec_kind spec = kind)
    webkit_fallback_specs

let fallback_spec_by_name select_name name =
  List.find_opt
    (fun spec -> String.equal (select_name (fallback_spec_names spec)) name)
    webkit_fallback_specs

let fallback_spec_by_standard_name = fallback_spec_by_name fst

let version_compare (major_a, minor_a) (major_b, minor_b) =
  match Int.compare major_a major_b with
  | 0 -> Int.compare minor_a minor_b
  | order -> order

let version_at_most version maximum = version_compare version maximum <= 0
let version_before version minimum = version_compare version minimum < 0

(* The target contract is deliberately owned here rather than by the printer:
   adding a fallback changes the AST and must therefore be explicit to API
   callers. Safari/iOS still require [-webkit-user-select] at the declared
   baseline, while unprefixed [backdrop-filter] arrived after 17.6, unprefixed
   [hyphens] arrived in 17, and Chrome needs the compatible [-webkit-mask]
   shorthand and longhands through 119. [mask-mode] and [mask-composite] are
   excluded because their prefixed forms have different grammars. Safari/iOS
   answer [text-decoration-color] under both spellings through 26.1, so it pairs
   this boundary with [Unresolved_value]: a settled colour is served by the
   standard longhand on every declared target. *)
let required_fallback kind targets =
  match kind with
  | User_select_fallback -> true
  | Backdrop_filter_fallback ->
      version_at_most targets.safari (17, 6)
      || version_at_most targets.ios_safari (17, 6)
  | Hyphens_fallback ->
      version_before targets.safari (17, 0)
      || version_before targets.ios_safari (17, 0)
  | Text_decoration_color_fallback ->
      version_at_most targets.safari (26, 1)
      || version_at_most targets.ios_safari (26, 1)
  | Mask_fallback | Mask_image_fallback | Mask_position_fallback
  | Mask_size_fallback | Mask_repeat_fallback | Mask_clip_fallback
  | Mask_origin_fallback ->
      version_at_most targets.chrome (119, 0)

let webkit_compatible_mask : Properties.mask -> Properties.mask =
  let strip_layer (layer : Properties.mask_layer) =
    { layer with mode = Option.none; composite = Option.none }
  in
  function
  | Layer layer -> Layer (strip_layer layer)
  | Layers layers -> Layers (List.map strip_layer layers)
  | value -> value

let webkit_fallback_of_declaration targets decl : Declaration.declaration option
    =
  let fallback : type a.
      webkit_fallback ->
      a Properties.property ->
      a ->
      bool ->
      Declaration.declaration option =
   fun kind property value important ->
    if required_fallback kind targets then
      Some (Declaration.v ~important property value)
    else None
  in
  let opaque_fallback kind property source important =
    if required_fallback kind targets then
      match
        Declaration.parse_opaque_declaration property
          (Declaration.string_of_value ~minify:true source)
      with
      | Some prefixed ->
          Some (if important then Declaration.important prefixed else prefixed)
      | None -> None
    else None
  in
  let condition_holds spec =
    match fallback_spec_condition spec with
    | Any_value -> true
    | Unresolved_value -> Variables.declaration_uses_var decl
  in
  let fallback_from_spec spec =
    match (spec, decl) with
    | ( Typed_fallback { kind; property; webkit_property; _ },
        Declaration { property = actual; value; important; _ } ) -> (
        match Properties.eq_property actual property with
        | Some Equal -> fallback kind webkit_property value important
        | None -> None)
    | ( Mask_fallback_spec { webkit_name; _ },
        Declaration { property = Mask; value; important; _ } ) ->
        let source = Declaration.v Mask (webkit_compatible_mask value) in
        opaque_fallback Mask_fallback webkit_name source important
    | _, (Theme_guarded _ | Declaration _) -> None
  in
  match decl with
  | Theme_guarded _ -> None
  | Declaration
      { property = Unknown_property name; value = components; important; _ }
    -> (
      match fallback_spec_by_standard_name name with
      | Some (Mask_fallback_spec { webkit_name; _ }) -> (
          let source = Declaration.v (Unknown_property name) components in
          let rendered = Declaration.string_of_value ~minify:false source in
          match Declaration.parse_declaration "mask" rendered with
          | Some (Declaration { property = Mask; value; _ }) ->
              let source = Declaration.v Mask (webkit_compatible_mask value) in
              opaque_fallback Mask_fallback webkit_name source important
          | Some _ | None ->
              fallback Mask_fallback (Unknown_property webkit_name) components
                important)
      | Some spec when condition_holds spec ->
          let kind = fallback_spec_kind spec in
          let _, webkit_name = fallback_spec_names spec in
          fallback kind (Unknown_property webkit_name) components important
      | Some _ | None -> None)
  | Declaration _ -> (
      match fallback_spec_by_standard_name (Declaration.property_name decl) with
      | Some spec when condition_holds spec -> fallback_from_spec spec
      | Some _ | None -> None)

let is_webkit_fallback kind decl =
  match (fallback_spec_by_kind kind, decl) with
  | Some spec, Declaration _ ->
      let _, webkit_name = fallback_spec_names spec in
      String.equal (Declaration.property_name decl) webkit_name
  | None, Declaration _ -> false
  | (None | Some _), Theme_guarded _ -> false

let fallback_kind decl : webkit_fallback option =
  match decl with
  | Theme_guarded _ -> None
  | Declaration _ ->
      Option.map fallback_spec_kind
        (fallback_spec_by_standard_name (Declaration.property_name decl))

(* Once an author supplied a prefix for a property, that spelling is theirs:
   synthesising another value could change what WebKit sees. Otherwise mirror
   every standard declaration so source-order fallback semantics stay intact. *)
let add_declaration_prefixes ~targets decls =
  let author_owns kind = List.exists (is_webkit_fallback kind) decls in
  let rec loop changed acc = function
    | [] -> if changed then List.rev acc else decls
    | decl :: rest -> (
        match fallback_kind decl with
        | Some kind when not (author_owns kind) -> (
            match webkit_fallback_of_declaration targets decl with
            | Some prefixed -> loop true (decl :: prefixed :: acc) rest
            | None -> loop changed (decl :: acc) rest)
        | Some _ | None -> loop changed (decl :: acc) rest)
  in
  loop false [] decls

let rec condition_has_webkit kind = function
  | Supports.Property (Supports.Declaration decl) ->
      is_webkit_fallback kind decl
  | Supports.Property _ | Supports.Function _ -> false
  | Supports.Not condition -> condition_has_webkit kind condition
  | Supports.And (a, b) | Supports.Or (a, b) ->
      condition_has_webkit kind a || condition_has_webkit kind b

let add_condition_prefixes ~targets condition =
  let author_owns kind = condition_has_webkit kind condition in
  let rec map = function
    | Supports.Property (Supports.Declaration decl) as original -> (
        match fallback_kind decl with
        | Some kind when not (author_owns kind) -> (
            match webkit_fallback_of_declaration targets decl with
            | Some prefixed ->
                Supports.Or
                  (Supports.Property (Supports.Declaration prefixed), original)
            | None -> original)
        | Some _ | None -> original)
    | (Supports.Property _ | Supports.Function _) as leaf -> leaf
    | Supports.Not condition as original ->
        let condition' = map condition in
        if condition' == condition then original else Supports.Not condition'
    | Supports.And (a, b) as original ->
        let a' = map a and b' = map b in
        if a' == a && b' == b then original else Supports.And (a', b')
    | Supports.Or (a, b) as original ->
        let a' = map a and b' = map b in
        if a' == a && b' == b then original else Supports.Or (a', b')
  in
  map condition

let rec add_conditional_prefixes ~targets = function
  | Media_condition _ as condition -> condition
  | Supports_condition_test condition as original ->
      let condition' = add_condition_prefixes ~targets condition in
      if condition' == condition then original
      else Supports_condition_test condition'
  | And (a, b) as original ->
      let a' = add_conditional_prefixes ~targets a
      and b' = add_conditional_prefixes ~targets b in
      if a' == a && b' == b then original else And (a', b')
  | Or (a, b) as original ->
      let a' = add_conditional_prefixes ~targets a
      and b' = add_conditional_prefixes ~targets b in
      if a' == a && b' == b then original else Or (a', b')

let compatibility_declaration_sites =
  {
    element_rule = true;
    animation_frame = true;
    page_box = true;
    position_fallback = true;
    condition_test = false;
  }

let add_compatibility_prefixes ~targets stylesheet =
  let rewrite original =
    let stmt =
      match original with
      | Supports (condition, body) ->
          let condition' = add_condition_prefixes ~targets condition in
          if condition' == condition then original
          else Supports (condition', body)
      | Import ({ supports = Some condition; _ } as rule) ->
          let condition' = add_condition_prefixes ~targets condition in
          if condition' == condition then original
          else Import { rule with supports = Some condition' }
      | When (condition, body) ->
          let condition' = add_conditional_prefixes ~targets condition in
          if condition' == condition then original else When (condition', body)
      | Else (Some condition, body) ->
          let condition' = add_conditional_prefixes ~targets condition in
          if condition' == condition then original
          else Else (Some condition', body)
      | _ -> original
    in
    let stmt =
      if at_declaration_site compatibility_declaration_sites stmt then
        map_statement_declarations (add_declaration_prefixes ~targets) stmt
      else stmt
    in
    if stmt == original then Keep else Replace stmt
  in
  edit_statements rewrite stylesheet

(* A WebKit workaround is a property of the declaration, so it applies wherever
   the declaration sits. *)
let apply_property_duplication (stylesheet : t) : t =
  map_declarations duplicate_buggy_properties stylesheet

(** [drop_invalid] walks every declaration list in the stylesheet (rules, bare
    nesting blocks, [@keyframes] frames, [@page] and its margin boxes,
    [@position-try], [@supports-condition]) and removes declarations whose typed
    value contains an [Invalid] arm. *)
let drop_invalid (stylesheet : t) : t =
  map_declarations
    (list_filter_preserve (fun d -> not (Declaration.is_invalid d)))
    stylesheet

(* CSS Syntax 3 (ED) sec. 5.5.2 keeps an unrecognised at-rule's body as raw
   source text, so no typed node stands between the author's layout and the
   output and the serializer may not touch it: rewriting the body is an AST
   change. Reading it back as a component-value stream and writing it out with
   the separator rules that already serve custom-property streams keeps every
   token boundary, string, escape and nested block while the layout between them
   goes. A body the lexer cannot re-serialise to the same stream is left
   alone. *)
let compact_unknown_at_rule_body body =
  let reader = Reader.of_string body in
  let { Parser.value = components; warnings } =
    Parser.list_of_component_values reader
  in
  match warnings with
  | _ :: _ -> body
  | [] ->
      let compacted = Parser.to_string_minified components in
      if String.length compacted < String.length body then compacted else body

let compact_unknown_at_rule_bodies (stylesheet : t) : t =
  edit_statements
    (function
      | Unknown_at_rule ({ block = Some body; _ } as at) ->
          let compacted = compact_unknown_at_rule_body body in
          if String.equal compacted body then Keep
          else Replace (Unknown_at_rule { at with block = Some compacted })
      | _ -> Keep)
    stylesheet

(** [drop_unknown_at_rules] removes [Unknown_at_rule] statements at every block
    depth, matching what a user agent applies of a stylesheet (CSS 2.1 sec. 4.2)
    rather than what a transform may hand the next reader of one. Opt in when
    the output has exactly one consumer and it is a browser. *)
let drop_unknown_at_rules (stylesheet : t) : t =
  edit_statements (function Unknown_at_rule _ -> Drop | _ -> Keep) stylesheet

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
          match try_promote_custom_with syntax components with
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
          | None -> decl))
  | _ -> decl

let promote_registered_custom_properties ~lossless (stmts : statement list) =
  let registry : (string, Variables.any_syntax) Hashtbl.t = Hashtbl.create 8 in
  (* An [@property] registration is document-global regardless of source order
     (CSS Properties and Values API 1 SS 2). Tailwind emits its [@property]
     rules after the [@layer] that uses them, so collect every registration in a
     first pass before promoting any declaration. *)
  iter_statements
    (fun (stmt : statement) ->
      match stmt with
      | Property pr ->
          Hashtbl.replace registry pr.name (Variables.Syntax pr.syntax)
      | _ -> ())
    stmts;
  let promote_decls =
    list_map_preserve (promote_registered_custom_decl ~lossless registry)
  in
  (* A registration is document-global, so every declaration of that name is
     typed wherever it is written: through the declaration walker rather than a
     list of the at-rules that came to mind. *)
  map_declarations promote_decls stmts

(* Under closed-stylesheet scope every [@position-try --name] rule is known. A
   [position-try-fallbacks] entry whose name has no matching rule cannot match
   at runtime, so prune unknown [Name] arms (keeping [Flip_*] and [Var]). When
   every arm is pruned the whole declaration drops (equivalent to its
   initial). *)
let collect_position_try_names stylesheet =
  let known : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  iter_statements
    (fun (stmt : statement) ->
      match stmt with
      | Position_try (name, _) -> Hashtbl.replace known name ()
      | _ -> ())
    stylesheet;
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
      (* A name with no [@position-try] rule cannot match wherever it is
         written, so this goes through the declaration walker rather than a list
         of the at-rules that came to mind. *)
      map_declarations prune_decls stylesheet

(* Collect the custom properties registered with an [@property] initial-value.
   Such a property is never invalid at computed-value time, so folding its
   [var()] into a shorthand cannot widen a failure across the other
   longhands. *)
let registered_foldable (stylesheet : t) : string -> bool =
  let tbl : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  iter_statements
    (fun (stmt : statement) ->
      match stmt with
      | Property pr -> (
          match pr.initial_value with
          | Some _ ->
              (* [@property] names carry the [--] prefix; [var()] references
                 store the bare name, so normalise to the bare form for
                 lookup. *)
              let key = Custom_property_name.strip_prefix pr.name in
              Hashtbl.replace tbl key ()
          | None -> ())
      | _ -> ())
    stylesheet;
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
  let bare = Custom_property_name.strip_prefix in
  iter_statements
    (fun (stmt : statement) ->
      match stmt with
      | Property pr ->
          if syntax_is_single_valued pr.syntax then
            Hashtbl.replace tbl (bare pr.name) ()
      | _ -> ())
    stmts;
  { Values.var_is_single_valued = (fun name -> Hashtbl.mem tbl name) }

let normalize_live_declarations ~ctx ~lossless decls =
  list_edit_preserve
    (fun decl ->
      let decl' = Declaration.normalize ~ctx ~lossless decl in
      if Declaration.is_invalid decl' then List.Drop
      else if decl' == decl then List.Keep
      else List.Replace decl')
    decls

(* An [@property] registration holds a typed initial value rather than a
   declaration, so it is the one statement whose value
   [map_statement_declarations] does not reach; every other statement either
   carries an unknown at-rule's body as opaque source text, which has no grammar
   to normalise against, or has its declarations normalised wherever that walk
   finds them. *)
let sanitize_statement ~ctx ~lossless (s : statement) : statement edit =
  match s with
  | Unknown_at_rule _ -> Keep
  | Property r ->
      let initial_value =
        match r.initial_value with
        | None -> r.initial_value
        | Some value ->
            let value' = Variables.normalize_value ~lossless r.syntax value in
            if value' == value then r.initial_value else Some value'
      in
      if initial_value == r.initial_value then Keep
      else Replace (Property { r with initial_value })
  | _ ->
      let s' =
        map_statement_declarations
          (normalize_live_declarations ~ctx ~lossless)
          s
      in
      if s' == s then Keep else Replace s'

let sanitize_block ~ctx ~lossless (b : statement list) : statement list =
  edit_statements (sanitize_statement ~ctx ~lossless) b

let rec statement_rule_count = function
  | Rule _ -> 1
  | stmt ->
      let count = ref 0 in
      List.iter
        (fun stmt -> count := !count + statement_rule_count stmt)
        (statement_children stmt);
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
  let rec loop n stmts =
    if n <= 0 then stmts
    else
      let next = statements_top_level ~factor_cache ~ctx ~enforce_spec stmts in
      if next == stmts || Stylesheet.equal next stmts then stmts
      else loop (n - 1) next
  in
  loop cap stylesheet

(* Bare names of every custom property referenced through [var()] in the tree.
   Scans the serialized declarations, so it sees references inside opaque values
   too and over-keeps (a [var(] inside a string counts): it must never miss a
   live reference, or the prune below drops a binding still in use. *)
let referenced_custom_props (stmts : statement list) : (string, unit) Hashtbl.t
    =
  let tbl = Hashtbl.create 64 in
  let bare = Custom_property_name.strip_prefix in
  let note_decls =
    List.iter (fun d ->
        List.iter
          (fun n -> Hashtbl.replace tbl (bare n) ())
          (Variables.var_refs_in_value_string
             (Declaration.to_string ~minify:true d)))
  in
  (* Through the exhaustive walk rather than a local match: a reference it
     cannot reach reads as no reference at all, and [@keyframes], [@page] and
     [@position-try] hold their declarations outside any block. *)
  iter_declarations note_decls stmts;
  tbl

(* Drop custom-property bindings referenced by no [var()] - a dead binding has
   no rendering effect (an emptied rule is removed at serialization). Caller
   opt-in only: the no-runtime-reader, complete-stylesheet assumption is theirs,
   like [Inline.vars]. *)
let drop_unused_custom_props (stmts : statement list) : statement list =
  let referenced = referenced_custom_props stmts in
  let bare = Custom_property_name.strip_prefix in
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
    | _ -> map_statement_children (list_map_preserve prune) stmt
  in
  list_map_preserve prune stmts

let stylesheet ?scope ?(targets = evergreen_targets) ?(flatten_nesting = false)
    ?(lossless = false) ?(enforce_spec = false) ?(aggressive = false)
    ?(regroup = true) ?(closed_world = false) ?(objective = `Transfer)
    ?(prune_unused_custom_props = false) ?stats (stylesheet : t) : t =
  let scope = Option.value scope ~default:`Fragment in
  let ctx = single_valued_calc_ctx stylesheet in
  let stylesheet = sanitize_block ~ctx ~lossless stylesheet in
  (* Under [lossless] the raw body stands as authored: nothing types it, so
     nothing proves the layout inside it carries no meaning to its reader. *)
  let stylesheet =
    if lossless then stylesheet else compact_unknown_at_rule_bodies stylesheet
  in
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
    if enforce_spec then result else add_compatibility_prefixes ~targets result
  in
  let result = if flatten_nesting then Flatten.block result else result in
  Log.debug (fun m ->
      let c = (Stats.snapshot (Ctx.stats ctx)).counters in
      m
        "optimized: %d factoring fixpoints run, %d skipped, %d reverted by the \
         transfer gate, %d bytes saved"
        c.factor_fixpoints_run c.factor_fixpoints_skipped
        c.factor_transfer_reverts c.factor_bytes_saved);
  result
