(** Grouping a list into a hash table of buckets. *)

val by : size:int -> ('a -> 'k * 'v) -> 'a list -> ('k, 'v list) Hashtbl.t
(** [by ~size key items] splits [items] into a table of [size] initial capacity,
    binding each key [key] returns to the values it returned for, in the order
    [items] gave them. *)
