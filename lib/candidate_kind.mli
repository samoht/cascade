(** What every kind of {!Rule_candidate} shares: the graph the search reads, the
    offers it makes, and the interface each kind implements.

    A kind files every live rule under the keys that decide which of its
    candidates the rule takes part in, and offers the candidates of the keys the
    rewrites since the last refresh touched. {!Rule_candidate} asks every kind
    the same questions in one order. *)

(** {1:view The graph} *)

type view
(** The graph the search reads, which a rewrite replaces, with the rules it has
    consumed and how many it prints. *)

val graph : view -> Rule_graph.t
(** [graph g] is the graph [g] reads. *)

val live : view -> int
(** [live g] is how many rules [graph g] prints. *)

val is_live : view -> Rule_graph.node_id -> bool
(** [is_live g id] is [true] if no rewrite has consumed [id]. *)

val node_rule : view -> Rule_graph.node_id -> Stylesheet.rule
(** [node_rule g id] is the rule of [id]. *)

(** {1:offers Offers} *)

type offer
(** A candidate and the key it is queued under. *)

val key : offer -> int
(** [key o] is a key no other offer of the same run ever carries. *)

val candidate : offer -> Rule_rewrite.candidate
(** [candidate o] is the candidate [o] offers. *)

val withdraw : offer -> unit
(** [withdraw o] drops [o] from the offers the next {!close} returns. *)

val withdraw_option : offer option -> unit
(** [withdraw_option o] withdraws [o] if there is one. *)

(** {1:core The state every kind reads} *)

type core
(** The context, the graph, and the offers and rewrites since the last {!close}.
*)

val v :
  ctx:Ctx.t ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  Rule_graph.t ->
  core
(** [v ~ctx ~finalize graph] reads [graph], with no offer made yet. *)

val ctx : core -> Ctx.t
(** [ctx core] is the optimizer context of the run. *)

val finalize : core -> Stylesheet.rule -> Stylesheet.rule
(** [finalize core r] is the produced rule [r] scored and normalized. *)

val view : core -> view
(** [view core] is the graph [core] reads. *)

val epoch : core -> int
(** [epoch core] is how many times [core] was {!close}d. *)

val offer : core -> Rule_rewrite.candidate -> offer
(** [offer core c] offers [c] under a fresh key. *)

type changes = {
  consumed : Rule_graph.node_id list;  (** the rules rewrites consumed *)
  produced : Rule_graph.node_id list;  (** the rules they produced *)
  moved : Rule_graph.node_id list;
      (** the rules they moved in the printed order *)
}
(** What rewrites changed. *)

val rewrite : core -> Rule_graph.t -> consume:Rule_graph.node_id list -> changes
(** [rewrite core graph ~consume] makes [core] read [graph], which
    {!Rule_graph.rewrite} returned consuming [consume], and is what that rewrite
    changed. *)

val pending : core -> changes option
(** [pending core] is what the rewrites since the last {!close} changed, keeping
    the produced and moved rules still live, or [None] once [core] was closed
    and no rewrite consumed a rule since. *)

val close : core -> offer list
(** [close core] starts the next epoch, and is the offers made since the last
    close and not withdrawn, in the order they were made. *)

(** {1:kind The interface of a kind} *)

module type S = sig
  type t
  (** The rules a kind has filed and the offers it holds. *)

  val add : core -> t -> Rule_graph.node_id -> unit
  (** [add core t id] files the live rule [id] under its keys. *)

  val remove : core -> t -> Rule_graph.node_id -> unit
  (** [remove core t id] takes the consumed rule [id] out of its keys. *)

  val reorder : core -> t -> Rule_graph.node_id list -> unit
  (** [reorder core t moved] marks what reads where the rules [moved] print. *)

  val settle : core -> t -> changes -> unit
  (** [settle core t changes] offers the candidates of the keys touched since
      the last settle, and asks again what [changes] can have answered
      differently. *)

  val offers : t -> offer list -> offer list
  (** [offers t acc] is the offers [t] holds, prepended to [acc]. *)
end

(** {1:shared What the kinds share} *)

module Int_table : sig
  include Hashtbl.S with type key = int

  val push : 'a list t -> key -> 'a -> unit
  (** [push tbl key value] prepends [value] to the list bound to [key]. *)
end

val same_id : Rule_graph.node_id -> Rule_graph.node_id -> bool
(** [same_id a b] is [true] if [a] and [b] are one node. *)

val same_decl : Declaration.declaration -> Declaration.declaration -> bool
(** [same_decl a b] is [true] if [a] and [b] print the same minified. *)

val without :
  Rule_graph.node_id -> Rule_graph.node_id list -> Rule_graph.node_id list
(** [without id ids] is [ids] less [id]. *)

val hash_ints : int list -> int
(** [hash_ints xs] mixes [xs] into one hash. *)

val rule_eligible : Stylesheet.rule -> bool
(** [rule_eligible r] is [true] if factoring and inlining may consume [r]: it
    nests nothing, has no merge key, no vendor pseudo-element and no [all]. *)

val rules_with_ids :
  view -> Rule_graph.node_id list -> (Rule_graph.node_id * Stylesheet.rule) list
(** [rules_with_ids g ids] pairs each of [ids] with its rule. *)

val ordered_ids : view -> Rule_graph.node_id list -> Rule_graph.node_id list
(** [ordered_ids g ids] is [ids] once each, in the order [graph g] prints them.
*)

val grouped_rule : Stylesheet.rule list -> Stylesheet.rule option
(** [grouped_rule rules] is the first of [rules] under the selector list of all
    of them, with nothing nested, or [None] when the selectors cannot share a
    list. *)

val rewrite_candidate :
  kind:Rule_rewrite.kind ->
  finalize:(Stylesheet.rule -> Stylesheet.rule) ->
  view ->
  consume:Rule_graph.node_id list ->
  produce:Stylesheet.rule list ->
  Rule_rewrite.candidate option
(** [rewrite_candidate ~kind ~finalize g ~consume ~produce] is
    {!Rule_rewrite.v}, unless the rewrite would leave the two halves of a WebKit
    fallback pair in two rules. *)

val selector_size : view -> Rule_graph.node_id -> int
(** [selector_size g id] is the printed size of the selector of [id]. *)

val decls_inline_cost : Declaration.declaration list -> int
(** [decls_inline_cost decls] is what printing [decls] in one more rule costs.
*)
