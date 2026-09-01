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

val layer_decl : layer_name list -> statement
(** [layer_decl names] creates a layer declaration statement. *)

val layer : ?name:layer_name -> block -> statement
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

val unknown_at_rule :
  name:string ->
  prelude:string ->
  ?block:string ->
  unit ->
  (statement, Error.t) result
(** [unknown_at_rule ~name ~prelude ?block ()] is the at-rule [\@name prelude]
    with [block] as its body, or the reason its parts cannot make one. Omitting
    [block] gives the statement form, [\@name prelude;].

    [name] is the at-keyword without its [@]. An unknown at-rule has no grammar
    to re-serialise a body from, so [block] is the text between its braces, the
    same text {!read} slices out of the source. Pass
    [to_string ~minify:true statements] to put a block cascade does model inside
    one.

    Text ends the at-rule wherever CSS Syntax 3 says it does: at a top-level [;]
    or [{] in the prelude (sec. 5.5.2), at the closer matching an opener in the
    block (sec. 5.5.9), at EOF once an unclosed [/*] has started (sec. 4.3.2). A
    part reaching one of those first prints a sheet that re-consumes to
    statements the caller never wrote, so the parts are read back and refused
    when they do. The refusal names one at-rule, so a caller keeps the rest of
    the sheet rather than losing it to one bad part. *)

val with_origin : cascade_origin -> block -> statement
(** [with_origin cascade_origin content] records the cascade origin for a
    stylesheet block. This is an API-level wrapper with no CSS syntax. *)

val origin_importance_rank : important:bool -> cascade_origin -> int
(** [origin_importance_rank ~important origin] returns the cascade precedence
    rank for the origin/importance criterion. Larger ranks have higher
    precedence. *)

val import_layer_name : import_rule -> layer_name option
(** [import_layer_name rule] returns the layer name declared by an [@import]
    rule: [None] means the import does not declare a layer, [Some []] means the
    import declares an anonymous layer, and [Some name] is the declared layer
    name. *)

val equal_layer_name : layer_name -> layer_name -> bool
(** [equal_layer_name a b] is whether [a] and [b] are the same layer, that is
    the same idents in the same order. Two layers whose CSS text differs only in
    how an ident is escaped are the same layer; [@layer a\2e b] and [@layer a.b]
    are not. *)

val equal_statement : statement -> statement -> bool
(** [equal_statement a b] is whether [a] and [b] are the same statement. Every
    part is read through the equality its own module states, so two [\@media]
    blocks whose queries select the same media are one statement even where the
    two queries are spelled apart, while an [\@supports] guard naming a
    different value stays a second statement. *)

val hash_statement : statement -> int
(** [hash_statement stmt] is a fingerprint of [stmt] consistent with
    {!equal_statement}: two statements that are equal always return the same
    value, and the converse may fail on a collision, so use it as a cheap
    pre-filter before falling back to {!equal_statement}.

    It reads the statement's shape, {!Declaration.hash} of every declaration it
    holds, {!Selector.hash} of every selector, the names and descriptors it
    carries, and, recursively, the statements of its block. It does not read the
    condition of an [\@media], [\@container] or [\@supports] rule, since none of
    the three states a hash agreeing with its equality; two statements differing
    only in a condition share a bucket. *)

val layer_block_name : statement -> layer_name option
(** [layer_block_name stmt] returns the declared name for an [@layer] block
    rule. It returns [Some []] for anonymous layer blocks, [Some name] for named
    layer blocks, and [None] for non-layer-block statements. The returned name
    is the at-rule's own declared name, not a parent-prefixed name. *)

val layer_statement_name_list : statement -> layer_name list option
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

val empty : t
(** [empty] is an empty stylesheet. *)

(** {1 Accessors} *)

val selector : rule -> Selector.t
(** [selector rule] returns the selector of a rule. *)

val declarations : rule -> declaration list
(** [declarations rule] returns the run of declarations written before the
    rule's first nested statement. CSS Nesting 1 sec. 3.4 wraps a run written
    after one in a nested declarations rule, so it stays in {!nested} at the
    position it was written; this is the whole body only for a rule that nests
    nothing. *)

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

val statement_declarations : statement -> declaration list
(** [statement_declarations stmt] is the declarations [stmt] holds directly: a
    rule's or a bare [Declarations] block's own declarations, the descriptors of
    [@page], [@position-try] and [@supports-condition], the margin-rule
    descriptors of a [@page] with margins, and the concatenated declarations of
    every frame of [@keyframes] (and its [-webkit-]/[-moz-] spellings). It is
    [[]] for a grouping at-rule, whose declarations live in the block
    {!statement_children} returns, and for a descriptor at-rule such as
    [@font-face] or [@counter-style], which holds descriptor values rather than
    declarations. Paired with {!statement_children} it visits every declaration
    in a stylesheet, and it is exhaustive for the same reason: a
    declaration-carrying at-rule added to the AST later does not compile until
    it is listed here. *)

val map_statement_children : (block -> block) -> statement -> statement
(** [map_statement_children f stmt] rebuilds [stmt] with [f] applied to the
    block {!statement_children} reads, and returns a statement that holds no
    block unchanged. It is the rebuilding counterpart of {!statement_children},
    for a walk that rewrites the tree rather than only reading it. It preserves
    physical identity: when [f] returns the block it was given, the result is
    [stmt] itself. *)

val map_statement_declarations :
  (declaration list -> declaration list) -> statement -> statement
(** [map_statement_declarations f stmt] rebuilds [stmt] with [f] applied to the
    declarations it holds directly, and returns a statement that holds none
    unchanged. It is the rebuilding counterpart of {!statement_declarations},
    with one difference: [f] sees each declaration list as its own list rather
    than the concatenation, so every frame of [@keyframes] and every margin rule
    of [@page] keeps its own block. It preserves physical identity: when [f]
    returns every list it was given, the result is [stmt] itself. *)

type declaration_sites = {
  element_rule : bool;
      (** A style rule or a bare nesting block: declarations that apply to an
          element. *)
  animation_frame : bool;
      (** A frame of [@keyframes] (and of its [-webkit-] / [-moz-] spellings):
          declarations in the animation origin. *)
  page_box : bool;
      (** [@page] and its margin boxes: declarations that apply to a page box
          rather than to an element. *)
  position_fallback : bool;
      (** [@position-try]: declarations in the position fallback origin. *)
  condition_test : bool;
      (** [@supports-condition]: declarations that are tested rather than
          applied. *)
}
(** The places a stylesheet holds declarations, grouped by what the declarations
    there mean rather than by which at-rule spells them. A walk that wants only
    some of them says so with this record instead of matching on the statements
    it expects to meet, which separates a narrow walk from one that forgot an
    at-rule, and makes a place added here a compile error in every walk that
    made a choice. *)

val at_declaration_site : declaration_sites -> statement -> bool
(** [at_declaration_site sites stmt] holds when the declarations [stmt] carries
    sit in one of the places [sites] names. It is the test {!fold_declarations}
    makes, exposed for a walk that carries something down the tree, such as the
    cascade layer a declaration sits in or an [@supports] nesting depth. An
    accumulator travels sideways rather than down, so such a walk cannot be a
    fold and recurses on {!statement_children} instead; this keeps the sites it
    reads as compile-checked as the fold's. *)

val fold_statements : ('a -> statement -> 'a) -> 'a -> block -> 'a
(** [fold_statements f acc block] folds [f] over [block] and over every
    statement reachable from it through {!statement_children}, in source order,
    a statement before the statements it holds. *)

val iter_statements : (statement -> unit) -> block -> unit
(** [iter_statements f block] applies [f] to every statement {!fold_statements}
    reaches. *)

val edit_statements : (statement -> statement edit) -> block -> block
(** [edit_statements f block] rewrites the statements {!fold_statements}
    reaches: [f] keeps, replaces or drops each one, and the walk descends
    through {!map_statement_children} into what survives, so a caller names only
    the statements it acts on rather than the at-rules they nest inside.
    Dropping a statement drops what it holds. [f] sees a statement before the
    statements it holds, and the walk continues into a replacement rather than
    into the statement it replaced. It preserves physical identity: when [f]
    keeps every statement, the result is [block] itself. *)

val fold_declarations :
  ?sites:declaration_sites ->
  ('a -> declaration list -> 'a) ->
  'a ->
  block ->
  'a
(** [fold_declarations f acc block] folds [f] over the declarations of every
    statement {!fold_statements} reaches, so a rule nested in a rule and an
    at-rule that holds declarations outside a block are both covered. [f] sees
    one statement's declarations at a time, as {!statement_declarations} returns
    them. [sites] defaults to every place a declaration sits; pass it to fold
    over some of them, and write the record out in full so that a place added to
    it does not compile until this walk has been read again. *)

val iter_declarations :
  ?sites:declaration_sites -> (declaration list -> unit) -> block -> unit
(** [iter_declarations f block] applies [f] to the declaration lists
    {!fold_declarations} folds over. *)

val map_declarations : (declaration list -> declaration list) -> block -> block
(** [map_declarations f block] rewrites the declarations of every statement
    {!fold_statements} reaches. [f] sees each declaration list as its own list,
    as {!map_statement_declarations} hands them over, so every frame of
    [@keyframes] and every margin rule of [@page] keeps its own block. It
    preserves physical identity: when [f] returns every list it was given, the
    result is [block] itself. *)

(** {1 Reading/Parsing} *)

val read_rule : ?nested:bool -> Cursor.t -> rule
(** [read_rule r] reads a CSS rule from the reader. With [~nested:true] the
    prelude is parsed as a CSS Nesting [<relative-selector-list>], so it may
    start with a combinator ([> .bar]) taken relative to the parent [&]. *)

val read_block : Cursor.t -> block
(** [read_block r] reads a CSS block from the reader. *)

val read_font_variant_descriptor : Cursor.t -> font_variant_descriptor
(** [read_font_variant_descriptor r] reads the [font-variant] descriptor of an
    [\@font-face] rule, or a [var()] standing for one. {!Inline} reads a custom
    property back through it when resolving such a reference. *)

val read : Cursor.t -> t
(** [read r] reads a complete CSS stylesheet from the cursor. Raises
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

module Source : sig
  (** Immutable source fidelity captured by
      [parse_stylesheet_partial ~on_source].

      This is a separate syntax snapshot rather than metadata attached to the
      typed stylesheet. The separation keeps ordinary parsing and constructed
      ASTs allocation-neutral and avoids claiming false provenance after a
      transform splits, merges, drops, or synthesises typed nodes. *)

  type t

  type comment = { loc : Loc.t; terminated : bool }
  (** A lexer-recognised comment. [loc] indexes {!preprocessed}; [terminated] is
      [false] when the closing [*/] was recovered at end of input. *)

  type rule = {
    syntax : Component.rule;
        (** The complete located CSS Syntax component tree for this rule. *)
    loc : Loc.t;
        (** The rule's semantic range in {!preprocessed}. A directly adjacent
            leading comment can be included because comments disappear before
            token emission. *)
    owned_loc : Loc.t;
        (** The non-overlapping range from the previous rule's end (or byte 0)
            through this rule's end. It gives all leading whitespace, comments,
            and recovered material one deterministic owner. *)
  }

  type position = {
    byte : int;
        (** Byte offset in the exact caller input returned by {!contents}. *)
    line : int;  (** One-based line in the CSS-preprocessed character stream. *)
    column : int;
        (** One-based Unicode-scalar column in the preprocessed stream. *)
  }

  type span = { start : position; end_ : position }

  val contents : t -> string
  (** [contents t] is the caller's byte-exact input, including a UTF-8 BOM,
      CRLF, form feed, NUL, whitespace, and comments. *)

  val preprocessed : t -> string
  (** [preprocessed t] is the CSS Syntax section 3.3 input indexed by every
      {!Loc.t}: BOM removed, NUL replaced by U+FFFD, and CR/FF/CRLF normalised
      to LF. When preprocessing changes nothing this is the same immutable
      string as {!contents}, so no duplicate source buffer is retained. *)

  val comments : t -> comment list
  (** [comments t] lists every actual lexer comment in source order;
      comment-like bytes inside strings and URL tokens are not reported. *)

  val rules : t -> rule list
  (** [rules t] is every syntax-recovered top-level rule, including rules later
      rejected by typed validation. *)

  val trailing_loc : t -> Loc.t
  (** [trailing_loc t] owns everything after the final rule. Together with the
      {!rule.owned_loc} ranges it partitions {!preprocessed} exactly. *)

  val slice : t -> Loc.t -> string
  (** [slice t loc] extracts [loc] from {!preprocessed}. Raises
      [Invalid_argument] when [loc] is outside the snapshot. *)

  val original_loc : t -> Loc.t -> Loc.t
  (** [original_loc t loc] maps preprocessed boundaries back to byte offsets in
      {!contents}. Boundaries emitted by the lexer/components map exactly; the
      two interior UTF-8 byte boundaries of a replacement U+FFFD map to the
      original NUL's start. *)

  val original_slice : t -> Loc.t -> string
  (** [original_slice t loc] extracts the caller bytes covered by
      [original_loc t loc]. *)

  val position : t -> int -> position
  (** [position t offset] maps a preprocessed byte boundary to source-map-ready
      original byte offset plus CSS line and Unicode-scalar column. *)

  val span : t -> Loc.t -> span
  (** [span t loc] maps both boundaries of [loc] with {!position}. *)
end

val parse_stylesheet_partial :
  ?meta:Loc.meta_level ->
  ?enforce_spec:bool ->
  ?on_source:(Source.t -> unit) ->
  string ->
  stylesheet * Error.t list
(** [parse_stylesheet_partial ?meta source] runs section 5.3 recovery via
    {!Parser.stylesheet} and then typed-validates each recovered rule via
    {!read_stylesheet_of_rules}. Warnings from both stages are combined in
    source order.

    Pass [~on_source] to opt into one immutable authored-syntax snapshot from
    the same parse. It retains the exact input, preprocessed input when
    different, located syntax tree, comment records, line index, and original-
    byte boundary map. Transforming the returned typed stylesheet never mutates
    the snapshot and deliberately creates no inferred source map for split,
    merged, dropped, or synthetic nodes. *)

(** {1 Pretty Printing} *)

val pp_rule : rule Pp.t
(** [pp_rule] pretty-prints CSS rules. *)

val pp_stylesheet : stylesheet Pp.t
(** [pp_stylesheet] pretty-prints CSS stylesheets. *)

val pp : t Pp.t
(** [pp] is {!pp_stylesheet}. *)

(** {1 Variable Extraction} *)

val vars_of_stylesheet : stylesheet -> Variables.any_var list
(** [vars_of_stylesheet ss] is every variable [ss] references, from the
    declarations {!fold_declarations} reaches, so a rule nested in a rule and an
    at-rule carrying declarations of its own are both covered. Deduplicated, in
    source order. *)

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

(** {1 Legacy Compatibility} *)

val rules : t -> rule list
(** [rules t] returns the top-level rules from the stylesheet. *)

val layers : t -> layer_name list
(** [layers t] is every cascade layer [t] declares, one path per layer ([a.b] is
    the sublayer [b] of [a], however it was written), in the order the sheet
    first names them. Each path is its idents, so a [.] one ident carries is not
    the separator between two. A layer named inside a conditional group counts:
    the group decides whether its contents apply, not whether the layer exists.
    A sublayer of an anonymous [@layer { ... }] has no name to report. *)

val layer_block : layer_name -> t -> block option
(** [layer_block name t] is the statements of the layer [name], wherever it is
    declared and whatever form declares it: a dotted name, a nested block, or a
    block inside a conditional group. It is [None] when no [@layer] block opens
    that layer, so a name only an [@layer a, b;] statement declares is [None] as
    well. *)

val media_queries : t -> (Media.t * rule list) list
(** [media_queries t] is every [@media] in [t], at any depth, paired with every
    rule below its brace. A query inside a group at-rule counts, and a rule
    nested in another rule or held by an inner group is one of the query's
    rules; a nested rule keeps the relative selector it was written with. *)

val container_queries :
  t -> (string option * Container.t option * rule list) list
(** [container_queries t] is every [@container] in [t], paired with every rule
    below its brace, on the same terms as {!media_queries}. *)

(** {1 Parsing and Pretty-printing} *)

val pp_import_rule : import_rule Pp.t
(** [pp_import_rule] pretty-prints an import rule. *)

val read_import_rule : Cursor.t -> import_rule
(** [read_import_rule r] parses an import rule. *)

val read_font_tech_descriptor : Cursor.t -> font_tech_descriptor
(** [read_font_tech_descriptor r] parses the [font-tech] descriptor of an
    [\@font-face] rule: one [<font-tech>] keyword (CSS Fonts 4 sec. 11.1), or a
    [var()] reference the inline pass resolves. *)

val pp_layer_name : layer_name Pp.t
(** [pp_layer_name] prints a [<layer-name>]: each ident with the escapes that
    read it back (CSS Syntax 3 (ED) sec. 2.1), joined by the [.] separators of
    CSS Cascade 5 sec. 6.4.1. A [.] an ident carries is escaped, so it never
    reads back as a separator. *)

val read_layer_name : Cursor.t -> layer_name
(** [read_layer_name r] parses a [<layer-name>]. It rejects a CSS-wide keyword,
    which CSS Cascade 5 sec. 6.4.1 reserves. *)

val string_of_layer_name : layer_name -> string
(** [string_of_layer_name name] is what {!pp_layer_name} prints. Two names never
    share their text, so it keys a layer. *)

val rule_hash : rule -> int
(** [rule_hash r] is a cheap hash that discriminates rules by their cached
    declaration hashes. Equal rules hash equally; unequal rules may collide, so
    callers still confirm with structural equality. *)
