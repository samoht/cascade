(** A mutable HTML tree, parsed and printed with markup.ml.

    {!Cascade.Resolve} needs a tree with parent links and settable attributes;
    markup.ml gives a signal stream. This is the tree in between, and it holds
    every node HTML has, comments included: [<p>v<!-- -->4.3</p>] keeps two text
    nodes, and a browser measures the two apart, so dropping the comment changes
    the render. Licence headers and conditional comments are the other reason
    they are content, not decoration. *)

type t = node list
(** A parsed document: the top-level nodes, in order. *)

and node =
  | Element of element
  | Text of string
  | Comment of string
  | Doctype of Markup.doctype

and element = {
  ns : string;  (** Namespace URI, so SVG and MathML print as themselves. *)
  tag : string;  (** Local name, lowercased by the HTML parser. *)
  mutable attributes : (string * string) list;
  mutable children : node list;
  mutable parent : element option;
}

val parse : string -> t
(** [parse html] is the document [html] denotes: the top-level nodes the parser
    leaves, in order - a doctype, any comment outside the markup, and the
    elements themselves. The [<html>] wrapper is among them only when the source
    wrote that tag or a doctype, so [<p>hi</p>] parses to the [<p>] alone and a
    source carrying no element at all leaves {!roots} empty. *)

val to_string : t -> string
(** [to_string doc] serialises [doc] as HTML5. *)

val pp : t Fmt.t
(** [pp] prints a document as HTML5. *)

val find_all : string -> t -> element list
(** [find_all tag doc] is every [tag] element in [doc], in document order. *)

val find : string -> t -> element option
(** [find tag doc] is the first [tag] element in [doc]. *)

val roots : t -> element list
(** [roots doc] is the elements among [doc]'s top-level nodes, the trees to
    resolve against. *)

val text : element -> string
(** [text e] is the data of every text node under [e], concatenated. *)

val attribute : element -> string -> string option
(** [attribute e name] is the value of [e]'s [name] attribute, or [None]. *)

val set_attribute : element -> string -> string -> unit
(** [set_attribute e name value] sets [e]'s [name] attribute. *)

val classes : element -> string list
(** [classes e] is the class names in [e]'s [class] attribute (HTML sec. 2.4.7:
    a set of space-separated tokens splits on ASCII whitespace). *)

val element_children : element -> element list
(** [element_children e] is [e]'s child elements, in document order. *)

val text_children : element -> string list
(** [text_children e] is the data of [e]'s direct child text nodes. A comment is
    not one: it does not stop an element being [:empty]. *)

val clear : element -> unit
(** [clear e] removes [e]'s children, leaving the element in the tree. *)

val element : string -> text:string -> element
(** [element tag ~text] is a new HTML element named [tag] holding [text]. *)

val append_child : element -> element -> unit
(** [append_child parent child] adds [child] as [parent]'s last child. *)

val prepend_child : element -> element -> unit
(** [prepend_child parent child] adds [child] as [parent]'s first child. *)
