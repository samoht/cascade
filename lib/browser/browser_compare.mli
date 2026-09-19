(** Rendering stylesheets over one document in a headless Chromium and reporting
    where the renders differ.

    The oracle is the browser's raster and nothing else. The document is loaded
    afresh under each sheet, at every viewport width a media condition in either
    sheet names and under every interaction state either sheet names, a state
    applied to every element at once; every animation is held at its start and
    every transition finished, and the whole page is captured and compared pixel
    for pixel. Two sheets render the same when every capture is the same
    picture. Where one differs, the computed styles of the elements under the
    differing pixels are read under both sheets and what they disagree on is
    listed, so a difference names a property; a computed value that differs over
    pixels that do not is not a difference, since what a script reads back is
    not what the browser renders.

    The document's own style elements and stylesheet links are removed and its
    inline style attributes kept. A width or state named only by rules no
    element of the document can match is not rendered, since such a rule can
    change nothing on the page; the rules are judged as [cascade prune] judges
    them, so a selector the matcher has no model for, [:hover] among them,
    counts as matching. The sheets themselves are rendered as written. *)

type render = {
  viewport : string;  (** The viewport rendered, as [WIDTHxHEIGHT]. *)
  state : string;  (** The interaction state applied, or [none]. *)
  x : int;
  y : int;
  width : int;
  height : int;
      (** The box the differing pixels span, in page pixels; the whole page when
          the two captures are not the same size. *)
  first_size : string;  (** The first capture's size, as [WIDTHxHEIGHT]. *)
  second_size : string;  (** The second capture's size. *)
}
(** One viewport and state under which the two sheets render differently. *)

type difference = {
  viewport : string;
  state : string;
  element : string;  (** A path to the element under the differing pixels. *)
  pseudo : string;  (** The pseudo-element sampled, or [""]. *)
  property : string;
  first : string;  (** The value under the first sheet. *)
  second : string;  (** The value under the second sheet. *)
}
(** One computed-style value that an element under a differing render's pixels
    computes differently under the two sheets. *)

type t = {
  meta : (string * string) list;
      (** What was rendered, as pairs keyed "browser" (its version),
          "browser_path", "elements", "properties", "pseudos", "states",
          "viewports", "captures" (how many renders were taken), "renders" (how
          many differed), "samples" and "differences" (the computed values read
          under differing renders, and how many differed),
          "document_styles_removed", "doctype_added" and, when the listing
          stopped short, "truncated". *)
  renders : render list;  (** The renders that differ, in the order taken. *)
  differences : difference list;
      (** The computed values that differ under them, in the order read. *)
}

val run : html:string -> (string * string) list -> (t, string) result
(** [run ~html sheets] renders [html] under the two [(name, css)] of [sheets]
    and compares them to each other. The report carries one first and one
    second, so exactly two sheets are taken; fewer or more is [Error]. It is
    [Error] with the reason, too, when no node or no headless Chromium is found,
    when the driver fails, when the page reports an error, and when nothing was
    rendered: a run that compared nothing never reads as an equivalence. *)

val identical : t -> bool
(** [identical report] is whether every render was the same picture under every
    sheet. *)

val meta_int : t -> string -> int option
(** [meta_int report key] is the count [key] names in [report]'s metadata. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf report] prints the renders and differences of [report] as
    {!to_string} lists them, without the header naming the sheets and the
    document. *)

val to_string : first:string -> second:string -> html:string -> t -> string
(** [to_string ~first ~second ~html report] is [report] for a reader: what was
    rendered, each render that differs with where, then each computed difference
    once with the viewports and states it holds under, a pseudo-element that
    repeats its element's difference counted rather than listed. [first],
    [second] and [html] name the sheets and the document. *)
