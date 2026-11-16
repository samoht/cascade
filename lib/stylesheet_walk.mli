(** Reading and rewriting the statement tree: accessors, the one-level children
    and declarations of a statement, the folds and maps built on them, and the
    queries that collect rules, layers and conditions from a sheet. *)

open Stylesheet_intf

(** {1 Accessors} *)

val selector : rule -> Selector.t
(** [selector rule] is the selector of [rule]. *)

val declarations : rule -> Declaration.declaration list
(** [declarations rule] is the declarations written before the first nested
    statement of [rule]. *)

val nested : rule -> statement list
(** [nested rule] is the nested statements of [rule]. *)

(** {1 One level} *)

val statement_children : statement -> block
(** [statement_children stmt] is the block [stmt] wraps, [[]] when it holds no
    statements. *)

val statement_declarations : statement -> Declaration.declaration list
(** [statement_declarations stmt] is the declarations [stmt] holds directly. *)

val map_statement_children : (block -> block) -> statement -> statement
(** [map_statement_children f stmt] rebuilds [stmt] with [f] applied to its
    {!statement_children}, keeping [stmt] physically when [f] keeps its block.
*)

val map_statement_declarations :
  (Declaration.declaration list -> Declaration.declaration list) ->
  statement ->
  statement
(** [map_statement_declarations f stmt] rebuilds [stmt] with [f] applied to each
    declaration list it holds directly, keeping [stmt] physically when [f] keeps
    every list. *)

(** {1 Declaration sites} *)

type declaration_sites = {
  element_rule : bool;  (** A style rule or a bare nesting block. *)
  animation_frame : bool;  (** A frame of [@keyframes] in any spelling. *)
  page_box : bool;  (** [@page] and its margin boxes. *)
  position_fallback : bool;  (** [@position-try]. *)
  condition_test : bool;  (** [@supports-condition]. *)
}
(** The places a stylesheet holds declarations, by what they mean there. *)

val at_declaration_site : declaration_sites -> statement -> bool
(** [at_declaration_site sites stmt] holds when the declarations of [stmt] sit
    in one of [sites]. *)

(** {1 Whole-tree walks} *)

val fold_statements : ('a -> statement -> 'a) -> 'a -> block -> 'a
(** [fold_statements f acc block] folds [f] over every statement reachable from
    [block], a statement before the statements it holds. *)

val iter_statements : (statement -> unit) -> block -> unit
(** [iter_statements f block] applies [f] to every statement {!fold_statements}
    reaches. *)

val edit_statements : (statement -> statement edit) -> block -> block
(** [edit_statements f block] keeps, replaces or drops every statement
    {!fold_statements} reaches, descending into what survives. *)

val fold_declarations :
  ?sites:declaration_sites ->
  ('a -> Declaration.declaration list -> 'a) ->
  'a ->
  block ->
  'a
(** [fold_declarations ?sites f acc block] folds [f] over the declarations of
    every statement {!fold_statements} reaches at one of [sites] (default: all).
*)

val iter_declarations :
  ?sites:declaration_sites ->
  (Declaration.declaration list -> unit) ->
  block ->
  unit
(** [iter_declarations ?sites f block] applies [f] to the lists
    {!fold_declarations} folds over. *)

val map_declarations :
  (Declaration.declaration list -> Declaration.declaration list) ->
  block ->
  block
(** [map_declarations f block] rewrites the declarations of every statement
    {!fold_statements} reaches, through {!map_statement_declarations}. *)

val vars_of_stylesheet : stylesheet -> Variables.any_var list
(** [vars_of_stylesheet ss] is every variable the declarations of [ss]
    reference, deduplicated, in source order. *)

(** {1 Queries} *)

val rules : stylesheet -> rule list
(** [rules ss] is the top-level rules of [ss]. *)

val layers : stylesheet -> layer_name list
(** [layers ss] is every cascade layer [ss] declares, in first-named order. *)

val layer_block : layer_name -> stylesheet -> block option
(** [layer_block name ss] is the statements of layer [name], [None] when no
    [@layer] block opens it. *)

val media_queries : stylesheet -> (Media.t * rule list) list
(** [media_queries ss] is every [@media] in [ss] with every rule below its
    brace. *)

val container_queries :
  stylesheet -> (string option * Container.t option * rule list) list
(** [container_queries ss] is every [@container] in [ss] with every rule below
    its brace. *)
