(** CSS stylesheet interface.

    This module models stylesheet syntax and CSS-file-local structure:
    construction, parsing, printing, traversal helpers, and context-free cascade
    ordering helpers. Operations that need information beyond stylesheet text
    should take an explicit closed context record from a dedicated module, such
    as {!module:Context}. *)

open Declaration
include module type of Stylesheet_intf

type t = stylesheet
(** Stylesheet value type. *)

(** {1 Construction Functions} *)

val rule :
  selector:Selector.t ->
  ?nested:statement list ->
  ?merge_key:string ->
  declaration list ->
  rule
(** [rule ~selector ?nested ?merge_key declarations] creates a CSS rule with
    optional nested rules/at-rules and an optional merge key for combining rules
    with identical declarations. *)

val property :
  syntax:'a Variables.syntax ->
  ?initial_value:'a ->
  ?inherits:bool ->
  string ->
  statement
(** [property ~syntax ?initial_value ?inherits name] creates a [@property] rule
    with typed syntax and initial value. *)

val layer_decl : string list -> statement
(** [layer_decl names] creates a layer declaration statement. *)

val layer : ?name:string -> block -> statement
(** [layer ?name content] creates a [@layer] rule. *)

val media : condition:Media.t -> block -> statement
(** [media ~condition content] creates a [@media] rule. *)

val media_nested :
  condition:Media.t -> Declaration.declaration list -> statement
(** [media_nested ~condition declarations] creates a [@media] rule for CSS
    nesting, containing bare declarations (no selector). Used inside rules where
    the selector is inherited from the parent. *)

val container : ?name:string -> ?condition:Container.t -> block -> statement
(** [container ?name ~condition content] creates a [@container] rule. *)

val supports : condition:Supports.t -> block -> statement
(** [supports ~condition content] creates a [@supports] rule. *)

val starting_style : block -> statement
(** [starting_style content] creates a [@starting-style] rule. *)

val with_origin : cascade_origin -> block -> statement
(** [with_origin cascade_origin content] records the cascade origin for a
    stylesheet block. This is an API-level wrapper with no CSS syntax. *)

val origin_importance_rank : important:bool -> cascade_origin -> int
(** [origin_importance_rank ~important origin] returns the cascade precedence
    rank for the origin/importance criterion. Larger ranks have higher
    precedence. *)

val import_layer_name : import_rule -> string option
(** [import_layer_name rule] returns the layer name declared by an [@import]
    rule: [None] means the import does not declare a layer, [Some ""] means the
    import declares an anonymous layer, and [Some name] is the declared layer
    name. *)

val layer_block_name : statement -> string option
(** [layer_block_name stmt] returns the declared name for an [@layer] block
    rule. It returns [Some ""] for anonymous layer blocks, [Some name] for named
    layer blocks, and [None] for non-layer-block statements. The returned name
    is the at-rule's own declared name, not a parent-prefixed name. *)

val layer_statement_name_list : statement -> string list option
(** [layer_statement_name_list stmt] returns the declared name list for
    statement-form [@layer] rules. *)

val cascade_layer_precedence_rank :
  layer_order:string list -> important:bool -> string option -> int
(** [cascade_layer_precedence_rank ~layer_order ~important layer] returns the
    same-origin layer precedence rank. For normal declarations, later explicit
    layers and then the implicit unlayered layer rank higher. For important
    declarations, that order is reversed, with important unlayered declarations
    ranked below important explicit layers. *)

val compare_cascade_layer_candidate :
  layer_order:string list ->
  cascade_layer_candidate ->
  cascade_layer_candidate ->
  int
(** [compare_cascade_layer_candidate] compares same-origin/same-specificity
    candidates by importance, layer precedence, then source order. *)

val winning_cascade_layer_candidate :
  layer_order:string list ->
  cascade_layer_candidate list ->
  cascade_layer_candidate option
(** [winning_cascade_layer_candidate] returns the winning candidate using
    {!compare_cascade_layer_candidate}. *)

val cascade_revert_layer_candidates :
  layer_order:string list ->
  important:bool ->
  current_layer:string option ->
  cascade_layer_candidate list ->
  cascade_layer_candidate list
(** [cascade_revert_layer_candidates] returns the same-importance candidates in
    lower-priority layers than [current_layer], modeling the candidate set used
    after [revert-layer] removes declarations from the current layer. *)

val compare_cascade_origin_candidate :
  cascade_origin_candidate -> cascade_origin_candidate -> int
(** [compare_cascade_origin_candidate] compares same-specificity candidates by
    origin/importance precedence, then source order. *)

val winning_cascade_origin_candidate :
  cascade_origin_candidate list -> cascade_origin_candidate option
(** [winning_cascade_origin_candidate] returns the winning candidate using
    {!compare_cascade_origin_candidate}. *)

val cascade_revert_origin_candidates :
  important:bool ->
  current_origin:cascade_origin ->
  cascade_origin_candidate list ->
  cascade_origin_candidate list
(** [cascade_revert_origin_candidates] returns same-importance candidates in the
    origins exposed by a [revert] declaration from [current_origin]. *)

val declared_values :
  ?property:string -> Declaration.declaration list -> declared_value list
(** [declared_values ?property declarations] returns the declared values
    contributed by [declarations], preserving declaration source order. When
    [property] is supplied, only declarations for that property are returned. *)

val cascaded_value : cascade_origin_candidate list -> string option
(** [cascaded_value candidates] returns the winning cascaded value payload, or
    [None] when no candidate contributes a value. *)

val compare_cascade_candidate :
  layer_order:string list -> cascade_candidate -> cascade_candidate -> int
(** [compare_cascade_candidate ~layer_order a b] compares full same-property
    cascade candidates by origin/importance, layer, specificity, scoping
    proximity, and source order. *)

val winning_cascade_candidate :
  layer_order:string list -> cascade_candidate list -> cascade_candidate option
(** [winning_cascade_candidate ~layer_order candidates] returns the highest
    priority full cascade candidate. *)

val value :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  cascaded:string option ->
  value
(** [value ~inherits ~initial ~inherited ~cascaded] models the defaulting step
    that produces a specified value from a cascaded value for [initial],
    [inherit], and [unset]. [inherited = None] means the element has no parent
    value and falls back to [initial]. *)

val specified_value_after_revert :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  cascade_origin_candidate list ->
  value
(** [specified_value_after_revert] resolves a chain of [revert] winners by
    rolling back to the next-lower origin until a non-revert candidate (or none)
    survives, then defaults the result. The rollback context is taken from each
    winning candidate, so callers do not need to pass [current_origin]. *)

val specified_value_after_revert_layer :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  layer_order:string list ->
  cascade_layer_candidate list ->
  value
(** [specified_value_after_revert_layer] is the [revert-layer] analogue of
    {!specified_value_after_revert}: chains rollback through the lower-priority
    layers until a non-[revert-layer] winner remains. *)

val value_processing_requires_document_context : value_processing_stage -> bool
(** [value_processing_requires_document_context stage] is [true] for stages this
    parser/serializer cannot compute from CSS text alone without caller-supplied
    document, inheritance, layout, rendering, or device context. *)

val starting_style_nested : Declaration.declaration list -> statement
(** [starting_style_nested declarations] creates a [@starting-style] rule for
    CSS nesting, containing bare declarations (no selector). Used inside rules
    where the selector is inherited from the parent. *)

val keyframes : string -> keyframe list -> statement
(** [keyframes name frames] creates a [@keyframes] animation rule. *)

val v : statement list -> stylesheet
(** [v statements] creates a stylesheet from a list of statements. *)

val empty_stylesheet : stylesheet
(** [empty_stylesheet] is an empty stylesheet. *)

(** {1 Accessors} *)

val selector : rule -> Selector.t
(** [selector rule] returns the selector of a rule. *)

val declarations : rule -> declaration list
(** [declarations rule] returns the declarations of a rule. *)

val nested : rule -> statement list
(** [nested rule] returns the nested statements of a rule. *)

val statement_children : statement -> block
(** [statement_children stmt] is the block [stmt] wraps: a rule's nested
    statements, the body of a grouping at-rule ([@media], [@supports],
    [@container], [@layer], [@scope], [@starting-style], [@when], [@else],
    [@-moz-document], and the [Origin] wrapper), and [[]] for a statement that
    holds no statements of its own. It is the one place that knows which
    at-rules nest, so a traversal written on top of it cannot miss one: the
    match is exhaustive, and a block at-rule added to the AST later does not
    compile until it is listed here. *)

(** {1 Reading/Parsing} *)

val read_rule : ?nested:bool -> Cursor.t -> rule
(** [read_rule r] reads a CSS rule from the reader. With [~nested:true] the
    prelude is parsed as a CSS Nesting [<relative-selector-list>], so it may
    start with a combinator ([> .bar]) taken relative to the parent [&]. *)

val read_block : Cursor.t -> block
(** [read_block r] reads a CSS block from the reader. *)

val read_stylesheet : Cursor.t -> stylesheet
(** [read_stylesheet r] reads a complete CSS stylesheet from the reader. Raises
    {!Cursor.Parse_error} on the first validator failure; use
    {!parse_stylesheet_partial} to get the recovered sheet with warnings
    instead. *)

val read_stylesheet_of_rules :
  ?source:string ->
  ?meta:Loc.meta_level ->
  Component.rule list ->
  stylesheet * Error.t list
(** [read_stylesheet_of_rules ?source ?meta rules] validates each Parser-
    recovered {!Component.rule} to a typed {!statement} independently. A
    validator failure on one rule is captured as a warning and the rule is
    dropped; the remaining rules are returned. Pass [?source] (and keep [?meta]
    at its default [`Full]) so dropped-rule warnings carry source- context
    snippets. *)

val parse_stylesheet_partial :
  ?meta:Loc.meta_level ->
  ?enforce_spec:bool ->
  string ->
  stylesheet * Error.t list
(** [parse_stylesheet_partial ?meta source] runs section 5.3 recovery via
    {!Parser.stylesheet} and then typed-validates each recovered rule via
    {!read_stylesheet_of_rules}. Warnings from both stages are combined in
    source order. *)

(** {1 Pretty Printing} *)

val pp_rule : rule Pp.t
(** [pp_rule] pretty-prints CSS rules. *)

val pp_stylesheet : stylesheet Pp.t
(** [pp_stylesheet] pretty-prints CSS stylesheets. *)

(** {1 Variable Extraction} *)

val vars_of_stylesheet : stylesheet -> Variables.any_var list
(** [vars_of_stylesheet ss] extracts all variables referenced in a stylesheet.
*)

(** {1 Rendering} *)

val to_string :
  ?minify:bool ->
  ?indent:int ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  t ->
  string
(** [to_string ?minify ?indent stylesheet] serialises a stylesheet to CSS. Pure
    formatter - no optimisation, no theme resolution. *)

val pp :
  ?minify:bool ->
  ?indent:int ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  t ->
  string
(** [pp] is {!to_string}. *)

val inline_style_of_declarations :
  ?minify:bool -> ?mode:mode -> declaration list -> string
(** [inline_style_of_declarations declarations] converts declarations to inline
    style string. *)

(** {1 Legacy Compatibility} *)

val empty : t
(** [empty] is an empty stylesheet. *)

val rules : t -> rule list
(** [rules t] returns the top-level rules from the stylesheet. *)

val layers : t -> string list
(** [layers t] returns the layer names from the stylesheet. *)

val media_queries : t -> (Media.t * rule list) list
(** [media_queries t] returns the media queries from the stylesheet. *)

val container_queries :
  t -> (string option * Container.t option * rule list) list
(** [container_queries t] returns the container queries from the stylesheet. *)

(** {1 Parsing and Pretty-printing} *)

val read : Cursor.t -> t
(** [read r] parses a stylesheet from the reader. *)

val pp_import_rule : import_rule Pp.t
(** [pp_import_rule] pretty-prints an import rule. *)

val read_import_rule : Cursor.t -> import_rule
(** [read_import_rule r] parses an import rule. *)

val rule_hash : rule -> int
(** [rule_hash r] is a cheap hash that discriminates rules by their cached
    declaration hashes. Equal rules hash equally; unequal rules may collide, so
    callers still confirm with structural equality. *)
