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
  optimize_merged_block:(Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list
(** Merge a later same-condition [@media] block into the first occurrence when
    hoisting it past the intervening statements cannot reorder a conflicting
    rule (overlapping selector with a shared property set to a different value).
*)

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
  string list ->
  Stylesheet.statement list ->
  string list * Stylesheet.statement list
(** Collect consecutive empty named layers into layer declaration names. *)

val merge_layer_declarations :
  Stylesheet.statement list -> Stylesheet.statement list
(** Merge adjacent [@layer name;] declarations while preserving order. *)

val drop_redundant_layer_decls :
  Stylesheet.statement list -> Stylesheet.statement list
(** Drop layer declarations whose ordering is already established. *)

val drop_empty_rules : Stylesheet.statement list -> Stylesheet.statement list
(** Drop empty rules and empty conditional wrappers. *)

val drop_misplaced_imports :
  Stylesheet.statement list -> Stylesheet.statement list
(** Drop imports that appear after stylesheet body statements. *)

val merge_named_layers_by_name :
  Stylesheet.statement list -> Stylesheet.statement list
(** Merge same-name layer blocks within one enclosing block. *)
