(** Estimated DEFLATE (gzip) transfer size. *)

val estimate : string -> int
(** [estimate s] is the approximate size in bytes of [s] after DEFLATE
    compression: a greedy LZ77 parse over a 32 KiB window, costed with RFC
    1951's extra-bit tables and an order-0 entropy estimate for literals.
    Absolute values are rough; compare candidate spellings of the same CSS,
    where the sign of the difference is what matters. *)
