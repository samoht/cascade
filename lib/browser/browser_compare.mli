(** Rendering stylesheets over one document in a headless Chromium and reporting
    every computed-style value they disagree on.

    The oracle is the browser and nothing else: values are compared as the
    browser spells them, and the one reading added, whether two values paint the
    same, is the browser's too, taken off a canvas. The document's own style
    elements and stylesheet links are removed and its inline style attributes
    kept. Every element and its [::before], [::after], [::marker],
    [::placeholder], [::first-letter] and [::first-line] are sampled at every
    viewport width a media condition in either sheet names and under every
    interaction state either sheet names, a state applied to every element at
    once. *)

type difference = {
  viewport : string;  (** The viewport sampled, as [WIDTHxHEIGHT]. *)
  state : string;  (** The interaction state applied, or [none]. *)
  element : string;  (** A path to the element sampled. *)
  pseudo : string;  (** The pseudo-element sampled, or [""]. *)
  property : string;
  first : string;  (** The value under the first sheet. *)
  second : string;  (** The value under the second sheet. *)
  paints_same : bool;
      (** Whether the two values paint the same to a viewer: a colour as the
          pixel it paints, a length within 0.05px, a number within 0.001. *)
}
(** One computed-style value the first two sheets disagree on. *)

type t = {
  meta : (string * string) list;
      (** What was sampled, as pairs keyed "browser" (its version),
          "browser_path", "elements", "properties", "pseudos", "states",
          "viewports", "samples", "differences" (how many values differed),
          "document_styles_removed", "doctype_added" and, when the listing
          stopped short, "truncated". *)
  differences : difference list;
      (** The values that differ, in the order they were sampled. *)
}

val run : html:string -> (string * string) list -> (t, string) result
(** [run ~html sheets] renders [html] under each [(name, css)] of [sheets] and
    compares every sheet to the first. It is [Error] with the reason when no
    node or no headless Chromium is found, when the driver fails, when the page
    reports an error, and when nothing was sampled: a run that compared nothing
    never reads as an equivalence. *)

val meta_int : t -> string -> int option
(** [meta_int report key] is the count [key] names in [report]'s metadata. *)

val visible : t -> difference list
(** [visible report] is the differences that do not paint the same. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf report] prints the differences of [report] as {!to_string} lists
    them, without the header naming the sheets and the document. *)

val to_string : first:string -> second:string -> html:string -> t -> string
(** [to_string ~first ~second ~html report] is [report] for a reader: what was
    sampled, then each difference once with the viewports and states it holds
    under, a pseudo-element that repeats its element's difference counted rather
    than listed. [first], [second] and [html] name the sheets and the document.
*)
