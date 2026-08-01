(** Seeded stylesheets built from cascade's typed constructors.

    The same seed always gives the same sheet, so a failure the sweep finds
    reproduces from its seed alone. Breadth matters less than shape: the
    generator leans on what the optimizer rewrites - a shorthand and a longhand
    of one family in either order, a property written twice, two rules sharing a
    selector, [!important], and a [@media] block. *)

val stylesheet : seed:int -> Cascade.Css.t
(** [stylesheet ~seed] is the sheet for [seed]. *)
