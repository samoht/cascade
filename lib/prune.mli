(** Remove the rules a set of documents cannot use.

    A rule is removed only when {!Resolve.supported} holds for its selector and
    every element of every document answers {!Resolve.constructor-No_match}. A
    selector the matcher has no model for is kept and never counted as unused:
    {!Resolve.constructor-Unsupported} is not "does not match", so reading it as
    one deletes a rule nothing ruled out. The matcher is asked under
    {!Resolve.constructor-Browser}, so a rule the page's own browser still
    applies is never removed on the strength of a reading no engine has shipped.

    Only selectors are read. A [@media], [@supports] or [@container] condition
    asks about a device, a user agent or a layout container, none of which a
    document carries, so the rules inside a group block are judged by their own
    selectors and the condition is left alone. A statement with no selector -
    [@keyframes], [@font-face], [@property], [@import], [@layer], [@charset] -
    names nothing a document can rule out and is kept.

    Nesting is flattened as by {!Css.flatten_nesting} before anything is judged,
    so the result is flat: a nested selector is written against its parent and
    decides nothing on its own.

    The documents are the whole of what this sees. A class a script adds at
    runtime is not in them, so a rule waiting for one is removed. *)

(** What the documents said about a rule. *)
type verdict =
  | Unused
      (** Every element answered no, so nothing in the documents uses it. *)
  | Used of int  (** This many elements matched. *)
  | Unmodelled
      (** Nothing ruled it out: {!Resolve.supported} does not hold for its
          selector, or the rule still carries nesting, whose children are
          written against it. *)

type entry = { selector : Selector.t; verdict : verdict }
(** One rule of the analysed sheet, with the selector as it was written. A rule
    whose selector list lost an unused branch keeps the whole list here: the
    entry reports on the source, the sheet carries the result. *)

type analysis = {
  sheet : Stylesheet.t;  (** The flattened sheet with every unused rule gone. *)
  entries : entry list;  (** Every rule that was judged, in source order. *)
  elements : int;  (** How many elements the documents hold. *)
}
(** What a set of documents says about a stylesheet. *)

module Make (Node : Resolve.NODE) : sig
  val analyse : sheet:Stylesheet.t -> Node.t list -> analysis
  (** [analyse ~sheet roots] judges every rule of [sheet] against the elements
      of the trees rooted at [roots], and is [sheet] with the unused ones
      removed alongside the verdict for each.

      [roots] holding no element between them makes every rule look unused. That
      is an answer about the documents rather than about [sheet], and the caller
      has to tell the two apart: {!field-elements} is [0] and every rule is
      reported {!constructor-Unused}. *)
end
