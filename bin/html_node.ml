(* The tree {!Cascade.Resolve} walks: an {!Html} element, read through the
   parent and child links markup.ml left on it. *)

type t = Html.element

let equal = ( == )
let name (n : t) = Some n.Html.tag
let id n = Html.attribute n "id"
let classes = Html.classes
let attribute = Html.attribute
let parent (n : t) = n.Html.parent

(* [children] is the element children the structural selectors count; text is
   reported apart because [:empty] counts that too, and a comment counts for
   neither. *)
let children = Html.element_children
let text_children = Html.text_children
