(** Statement-block cleanup passes. *)

val rules_conflict : Stylesheet.rule -> Stylesheet.rule -> bool
(** [rules_conflict a b] is [true] when one element could match both selectors
    and the two rules write a common property slot at equal importance, so that
    element computes something different once the two swap source order. The
    relation is symmetric.

    [false] is the side a caller acts on: it says no element can tell the two
    rules apart, so a pass may reorder them. Both halves of the question lean
    towards [true] when they cannot decide. {!Selector_summary.may_overlap}
    answers [true] unless it can prove the two subjects disjoint, and a
    shorthand counts as writing every longhand it expands to, so a pair that
    happens to agree on the shared slot is still a conflict. Importance does not
    lean: a normal declaration and an [!important] one on one property are
    ordered by importance rather than by appearance, and one declaration written
    twice reads the same either way, so neither is a conflict. *)

val merge_consecutive_layers :
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent named [@layer] blocks with the same layer name. *)

val merge_consecutive_media :
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@media] blocks with identical conditions. *)

val merge_distant_media :
  ?owner:Stylesheet.rule ->
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge a later same-condition [@media] block into the first occurrence when
    the hoist reorders no conflicting rule. *)

val merge_consecutive_supports :
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@supports] blocks with identical conditions. *)

val merge_consecutive_containers :
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@container] blocks with identical names and conditions. *)

val merge_distant_containers :
  ?owner:Stylesheet.rule ->
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge a later [@container] block with the same name and condition into the
    first occurrence when the hoist reorders no conflicting rule. *)

val merge_consecutive_starting_style :
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@starting-style] blocks. *)

val is_layer_empty : Stylesheet.statement list -> bool
(** Whether a layer block has no cascade-contributing statements. *)

val collect_empty_layer_names :
  Stylesheet.layer_name list ->
  Stylesheet.statement list ->
  Stylesheet.layer_name list * Stylesheet.statement list
(** Collect consecutive empty named layers into layer declaration names. *)

val merge_layer_declarations :
  Stylesheet.statement list -> Stylesheet.statement list
(** Merge adjacent [@layer name;] declarations while preserving order. *)

val drop_redundant_layer_decls :
  Stylesheet.statement list -> Stylesheet.statement list
(** Drop layer declarations whose ordering is already established. *)

val drop_empty_rules : Stylesheet.statement list -> Stylesheet.statement list
(** Drop the statements of a block that contribute nothing. *)

val drop_misplaced_imports :
  Stylesheet.statement list -> Stylesheet.statement list
(** Drop imports that appear after stylesheet body statements. *)

val merge_named_layers_by_name :
  Stylesheet.statement list -> Stylesheet.statement list
(** Merge same-name layer blocks within one enclosing block. *)
