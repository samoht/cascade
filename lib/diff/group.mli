(** Grouping a list into a hash table of buckets. *)

val by : ?size:int -> ('a -> 'k * 'v) -> 'a list -> ('k, 'v list) Hashtbl.t
(** [by key items] binds each key [key] returns to the values it returned for,
    in the order [items] gave them. [size] (default [16]) is the table's initial
    capacity, a sizing hint: it decides which bucket a key lands in and so the
    order [Hashtbl.iter] walks them, which is why a caller that needs a defined
    order over the keys takes it from [items]. *)
