(** Property identity: the order on property constructors and the type-equality
    proof between two properties. *)

open Properties_intf

val compare_property : 'a property -> 'b property -> int
(** [compare_property a b] is a total order on property identities. It is [0]
    exactly when [a] and [b] are the same property, so an ordered container
    keyed on a property agrees with equality on it. *)

val eq_property : 'a property -> 'b property -> ('a, 'b) Type.eq option
(** [eq_property a b] is [Some Equal] when [a] and [b] are the same property,
    and [None] otherwise. It answers {!compare_property}'s question and carries
    the proof that the two properties value the same type, which the comparison
    on its own cannot express. *)

(** The families shorthand composition reads together. A composer only ever
    combines, reorders or drops declarations of one family, so a body holding at
    most one declaration per family composes to itself. Each family is named
    after its shorthand, or the shorthand its members reset ([Columns] holds the
    gaps, [Place] the alignment longhands, [White_space] the [text-wrap] ones,
    [Text_stroke] [-webkit-text-stroke]). *)
module Family : sig
  type t =
    | Animation
    | Background
    | Border
    | Caret
    | Columns
    | Contain_intrinsic_size
    | Container
    | Flex
    | Font
    | Grid
    | Inset
    | Interest_delay
    | List_style
    | Margin
    | Mask
    | Offset
    | Outline
    | Overflow
    | Overscroll_behavior
    | Padding
    | Place
    | Position_try
    | Scroll_margin
    | Scroll_padding
    | Scroll_timeline
    | Text_box
    | Text_decoration
    | Text_emphasis
    | Text_stroke
    | Transition
    | View_timeline
    | White_space
end

val composition_family : 'a property -> Family.t option
(** [composition_family p] is the family shorthand composition reads [p] in,
    [None] for a property no composer reads. Two declarations a composer
    combines, reorders or drops one against the other are of one family. *)

val family_rank : Family.t -> int
(** [family_rank f] is [f]'s position in {!Family.t}, below 32. *)
