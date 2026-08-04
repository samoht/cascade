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
  kept : int;
      (** How many rules {!field-keep_css} holds, for reporting. A block at-rule
          contributes the rules inside it rather than itself; one that holds no
          statements of its own ([@font-face], [@keyframes]) counts as the one
          thing it keeps. *)
}

module Make (Node : Resolve.NODE) : sig
  val compute :
    ?minimal:bool -> sheet:Stylesheet.t -> Node.t list -> Node.t result
  (** [compute ?minimal ~sheet roots] resolves [sheet] against the element trees
      rooted at [roots] and returns, for each element, the declarations to set
      on its [style] attribute, the un-inlinable rules as a [<style>] body, and
      the count of kept rules. [minimal] (default [false]) drops an inherited
      declaration that only restates the value the element already inherits from
      its ancestors.

      The argument is a parsed stylesheet rather than CSS text so that the parse
      stays with the caller: a stylesheet {!Css.of_string} had to recover, and
      one that was empty to begin with, both project onto nothing, and only the
      {!Css.field-warnings} it returns tell them apart. Parsing here would
      swallow that difference and report the two as the same empty result. *)
end
