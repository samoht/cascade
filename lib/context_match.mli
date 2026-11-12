(** Condition and selector matching against a closed context, URL resolution and
    [@import] loading: the evaluators behind {!Context.matches_media} and its
    siblings. *)

open Context_intf

(** [@supports] evaluation (CSS Conditional 3 sec. 6). *)
module Match_supports : sig
  val eval : query -> Supports.t -> bool
  (** [eval q cond] is whether [q] supports [cond]. *)
end

(** [@media] evaluation (CSS Media Queries 4). *)
module Match_media : sig
  val eval : query -> Media.t -> bool
  (** [eval q m] is whether [m] is true in [q]; an unknown result does not
      match. *)
end

(** Container-query evaluation (CSS Conditional 5 sec. 5.4). *)
module Match_container : sig
  val eval : query -> ?name:string -> Container.t -> bool
  (** [eval q ?name cond] is whether the container [q] describes, named [name]
      when given, satisfies [cond]. *)
end

(** Selector matching (CSS Selectors 4) against one element, not a tree. *)
module Match_selector : sig
  val eval : document -> Selector.t -> bool
  (** [eval doc sel] is whether [sel] matches the element [doc] describes. *)
end

(** URL resolution (RFC 3986). *)
module Url : sig
  val resolve : loader -> string -> (string, string) result
  (** [resolve l href] is [href] resolved against the base URL of [l]. *)
end

(** [@import] loading (CSS Cascade 5 sec. 2). *)
module Import : sig
  val load :
    ?query:query ->
    ?layer_order:string list ->
    loader ->
    Stylesheet.import_rule ->
    (Stylesheet.block, string) result
  (** [load ?query ?layer_order l rule] is the stylesheet [rule] imports from
      [l], once its media, supports and layer guards pass. *)
end
