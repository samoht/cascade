(** Cascade ordering and value processing over stylesheet candidates.

    The origin, layer and candidate rankings of CSS Cascade 5, and the
    defaulting step that turns a cascaded value into a specified one. None of it
    reads document context: every input is a candidate record. *)

open Stylesheet_intf

val origin_importance_rank : important:bool -> cascade_origin -> int
(** [origin_importance_rank ~important origin] is the precedence rank of the
    origin and importance criterion; a larger rank wins. *)

val import_layer_name : import_rule -> layer_name option
(** [import_layer_name rule] is the layer an [@import] declares: [None] for no
    layer, [Some []] for an anonymous one. *)

val layer_block_name : statement -> layer_name option
(** [layer_block_name stmt] is the declared name of an [@layer] block, [Some []]
    when it is anonymous and [None] for any other statement. *)

val layer_statement_name_list : statement -> layer_name list option
(** [layer_statement_name_list stmt] is the name list of a statement-form
    [@layer], and [None] for any other statement. *)

val cascade_layer_precedence_rank :
  layer_order:string list -> important:bool -> string option -> int
(** [cascade_layer_precedence_rank ~layer_order ~important layer] is the
    same-origin layer rank of [layer]; important declarations reverse the order.
*)

val compare_cascade_layer_candidate :
  layer_order:string list ->
  cascade_layer_candidate ->
  cascade_layer_candidate ->
  int
(** [compare_cascade_layer_candidate ~layer_order a b] orders candidates by
    importance, layer rank, then source order. *)

val winning_cascade_layer_candidate :
  layer_order:string list ->
  cascade_layer_candidate list ->
  cascade_layer_candidate option
(** [winning_cascade_layer_candidate ~layer_order cs] is the greatest of [cs]
    under {!compare_cascade_layer_candidate}. *)

val cascade_revert_layer_candidates :
  layer_order:string list ->
  important:bool ->
  current_layer:string option ->
  cascade_layer_candidate list ->
  cascade_layer_candidate list
(** [cascade_revert_layer_candidates ~layer_order ~important ~current_layer cs]
    is the candidates of [cs] that [revert-layer] in [current_layer] exposes. *)

val compare_cascade_origin_candidate :
  cascade_origin_candidate -> cascade_origin_candidate -> int
(** [compare_cascade_origin_candidate a b] orders candidates by origin and
    importance, then source order. *)

val winning_cascade_origin_candidate :
  cascade_origin_candidate list -> cascade_origin_candidate option
(** [winning_cascade_origin_candidate cs] is the greatest of [cs] under
    {!compare_cascade_origin_candidate}. *)

val cascade_revert_origin_candidates :
  important:bool ->
  current_origin:cascade_origin ->
  cascade_origin_candidate list ->
  cascade_origin_candidate list
(** [cascade_revert_origin_candidates ~important ~current_origin cs] is the
    candidates of [cs] that [revert] from [current_origin] exposes. *)

val declared_values :
  ?property:string -> Declaration.declaration list -> declared_value list
(** [declared_values ?property decls] is the declared values of [decls] in
    source order, only those of [property] when it is given. *)

val cascaded_value : cascade_origin_candidate list -> string option
(** [cascaded_value cs] is the payload of the winning candidate of [cs]. *)

val compare_cascade_candidate :
  layer_order:string list -> cascade_candidate -> cascade_candidate -> int
(** [compare_cascade_candidate ~layer_order a b] orders full candidates by
    origin and importance, layer, specificity, scope proximity and source order.
*)

val winning_cascade_candidate :
  layer_order:string list -> cascade_candidate list -> cascade_candidate option
(** [winning_cascade_candidate ~layer_order cs] is the greatest of [cs] under
    {!compare_cascade_candidate}. *)

val value :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  cascaded:string option ->
  value
(** [value ~inherits ~initial ~inherited ~cascaded] is the specified value the
    defaulting step makes of [cascaded]. *)

val specified_value_after_revert :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  cascade_origin_candidate list ->
  value
(** [specified_value_after_revert ~inherits ~initial ~inherited cs] rolls a
    chain of [revert] winners back origin by origin, then defaults the result.
*)

val specified_value_after_revert_layer :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  layer_order:string list ->
  cascade_layer_candidate list ->
  value
(** [specified_value_after_revert_layer] is {!specified_value_after_revert} for
    [revert-layer], rolling back layer by layer. *)

val value_processing_requires_document_context : value_processing_stage -> bool
(** [value_processing_requires_document_context stage] is [true] for the stages
    CSS text alone cannot compute. *)
