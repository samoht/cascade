(** Seeded stylesheets for the render differential.

    The same seed always gives the same sheet, so a failure the sweep finds
    reproduces from its seed alone. The declaration pool is drawn from
    {!Property_names.all}, which is generated from the library's own property
    table; the shapes are the ones the optimizer rewrites: a shorthand and one
    of its longhands in either order, a longhand run cut across a rule boundary
    with the slots it leaves unwritten set elsewhere, a selector repeated
    adjacently and across an unrelated rule, [!important] on either side,
    selectors that overlap without being equal, nesting, and [@media],
    [@supports] and [@layer] wrappers around all of it. *)

val stylesheet : seed:int -> Cascade.Css.t
(** [stylesheet ~seed] is the sheet for [seed]. *)
