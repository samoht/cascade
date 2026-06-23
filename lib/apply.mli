(** Project a stylesheet onto an element tree: resolve the cascade for each
    element and return the declarations to write into its [style] attribute,
    along with the rules that have no inline form. The cascade itself is
    {!Resolve}; this adds the inline-specific policy. Pure - it returns the
    declarations to apply, the caller writes them onto the tree. *)

type 'node assignment = 'node * Declaration.declaration list
(** A node paired with the inline-style declarations to write onto it. *)

type 'node result = {
  styles : 'node assignment list;
      (** Each element with the declarations to set on its [style] attribute. *)
  keep_css : string;
      (** The rules with no inline form, serialised as a minified [<style>]
          body, or [""] when there are none. *)
  kept : int;  (** Count of kept rules, for reporting. *)
}

module Make (Node : Resolve.NODE) : sig
  val compute : ?minimal:bool -> css:string -> Node.t list -> Node.t result
  (** [compute ?minimal ~css roots] resolves [css] against the element trees
      rooted at [roots] and returns, for each element, the declarations to set
      on its [style] attribute, the un-inlinable rules as a [<style>] body, and
      the count of kept rules. [minimal] (default [false]) drops an inherited
      declaration that only restates the value the element already inherits from
      its ancestors. A [css] that does not parse yields an empty result. *)
end
