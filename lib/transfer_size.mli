(** Estimated compressed size of printed rules: the cost model of the transfer
    objective.

    The model reads rules as LZ77 does: each selector branch and each
    declaration is a token, printed with its separator. A token printed earlier
    in the list costs a match, and a token continuing a run that was printed in
    the same order earlier extends that match for almost nothing; any other
    token costs its bytes. The constants are fitted against gzip [-9] on the
    SatCSS corpus. *)

val per_byte : int
(** [per_byte] is how many units of {!rules} make one compressed byte. *)

val rules : Size.print -> Stylesheet.rule list -> int
(** [rules p rs] is the estimated compressed size of [rs] printed in order under
    [p], in units of [1 / per_byte] byte. It reads each rule's selector branches
    and declarations, as {!Size.rule} does. *)
