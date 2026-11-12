(** Rule alignment for the tree diff: pairing the rules of two statement lists
    into added, removed, changed, moved and regrouped entries. *)

open Cascade
open Tree_diff_intf

val reorder_is_significant :
  Css.declaration list -> Css.declaration list -> bool
(** [reorder_is_significant d1 d2] is [true] when going from [d1] to [d2] swaps
    the relative order of two overlapping declarations. *)

val selector_key_of_selector : Css.Selector.t -> Css.Selector.t
(** [selector_key_of_selector sel] is [sel] with the items of a selector list
    sorted, the key two rules pair on. *)

val diffs :
  key_of:('item -> 'key) ->
  is_empty_diff:('item -> 'item -> bool) ->
  'item list ->
  'item list ->
  'item list * 'item list * ('item * 'item) list
(** [diffs ~key_of ~is_empty_diff items1 items2] is
    [(added, removed, modified)]: the items of [items2] no item of [items1]
    pairs with, the items of [items1] left unpaired, and the pairs
    [is_empty_diff] does not hold for. Items pair on equal [key_of], in source
    order. *)

val moved_conditions :
  condition_of:(Css.statement -> 'c option) ->
  Css.statement list ->
  Css.statement list ->
  ('c, unit) Hashtbl.t
(** [moved_conditions ~condition_of stmts1 stmts2] is the set of conditions of
    the containers of [stmts1] that changed places against the rest of the list
    in [stmts2]. *)

type rule_modification =
  Css.Selector.t * Css.Selector.t * Css.declaration list * Css.declaration list
(** A rule paired with its partner on the other side: the two selectors and the
    two declaration lists. *)

val rule_diffs :
  Css.statement list ->
  Css.statement list ->
  Css.statement list
  * Css.statement list
  * rule_modification list
  * rule_diff list
(** [rule_diffs rules1 rules2] is [(added, removed, modified, regrouped)], the
    structural pairing of the rules of [rules1] and [rules2]. *)

val content_changed :
  string -> Css.declaration list -> Css.declaration list -> rule_diff
(** [content_changed selector old_decls new_decls] is the [Content_changed]
    entry for [selector] going from [old_decls] to [new_decls]. *)

val to_rule_changes : Css.statement list -> Css.statement list -> rule_diff list
(** [to_rule_changes rules1 rules2] is the rule entries of the report for one
    statement list, leaving out the statements a container processor names. *)

(** {1 Selectorless at-rules} *)

(** What a selectorless at-rule holds below its brace. *)
type at_rule_body =
  | Block of Css.statement list  (** statements, walked like a container *)
  | Declarations of Css.declaration list  (** a rule body without a rule *)
  | Opaque  (** descriptors, compared as the text they print to *)

val at_rule_items :
  Css.statement list -> ((string * int) * (string * at_rule_body * string)) list
(** [at_rule_items stmts] is the selectorless at-rules of [stmts] as
    [((head, n), (head, body, text))], [n] numbering the blocks that share
    [head]. *)

val at_rule_declarations_change :
  Css.declaration list -> Css.declaration list -> rule_diff option
(** [at_rule_declarations_change decls1 decls2] is the change between two
    at-rule bodies of declarations, [None] when nothing visible changed. *)

val at_rule_text_change : string -> string -> rule_diff
(** [at_rule_text_change text1 text2] is the change between two descriptor
    bodies, compared as text. *)
