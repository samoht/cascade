(** The tree diff's container walk: the blocks of [@media], [@layer],
    [@supports], [@container], [@property], [@keyframes], CSS nesting and the
    selectorless at-rules, plus the [@import] list and the cascade layer order.
*)

open Cascade
open Tree_diff_intf

val nested_differences :
  Css.statement list -> Css.statement list -> container_diff list
(** [nested_differences stmts1 stmts2] is every container that was added,
    removed, moved or changed between [stmts1] and [stmts2], recursing into the
    blocks both sides hold. *)

val process_imports : Css.statement list -> Css.statement list -> rule_diff list
(** [process_imports stmts1 stmts2] is the change in the [@import] statements of
    [stmts1] and [stmts2]: imports added or removed, or the same imports in a
    different order. *)

val layer_order_diff :
  Css.statement list -> Css.statement list -> layer_order_diff option
(** [layer_order_diff stmts1 stmts2] is the difference in the cascade layer
    order the two lists declare, over the layers both declare; [None] when that
    order is the same. *)
