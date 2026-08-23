(** Statement-block cleanup passes. *)

val merge_consecutive_layers :
  optimize_merged_block:(Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent named [@layer] blocks with the same layer name. *)

val merge_consecutive_media :
  optimize_merged_block:(Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@media] blocks with identical conditions. *)

val merge_distant_media :
  ?owner:Stylesheet.rule ->
  optimize_merged_block:(Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge a later same-condition [@media] block into the first occurrence when
    hoisting it past the intervening statements cannot reorder a conflicting
    rule (overlapping selector with a shared property set to a different value).

    [owner] is the style rule whose body the statements are, when they are one.
    A declarations run in that body sets properties on [owner] (CSS Nesting 1
    sec. 3.4), so without it the run is a conflict the hoist never sees. *)

val merge_consecutive_supports :
  optimize_merged_block:(Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@supports] blocks with identical conditions. *)

val merge_consecutive_containers :
  optimize_merged_block:(Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge adjacent [@container] blocks with identical names and conditions. *)

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
(** Drop the statements of a block that contribute nothing: a rule with no
    declarations and no nested rules, a rule whose selector
    {!Selector.matches_nothing}, a conditional group rule with an empty body,
    and an empty [@scope], [@starting-style] or [@page] box. An empty [@when] or
    [@else] goes only when no [@else] chains onto it, since css-conditional-5
    sec. 3 binds an [@else] to the branch before it. An empty [@layer] stays,
    the name still ordering the layer, and so does an empty origin wrapper,
    which gates nothing. *)

val drop_misplaced_imports :
  Stylesheet.statement list -> Stylesheet.statement list
(** Drop imports that appear after stylesheet body statements. *)

val merge_named_layers_by_name :
  Stylesheet.statement list -> Stylesheet.statement list
(** Merge same-name layer blocks within one enclosing block. *)
