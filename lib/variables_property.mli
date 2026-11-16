(** Variable extraction from a whole property value: the dispatch over every
    typed property and the scan of custom-property token streams. *)

val vars_of_custom_property_value :
  Properties.custom_property_value -> Variables_intf.any_var list
(** [vars_of_custom_property_value v] is the variables [v] references. *)

val vars_of_property :
  'a Properties.property -> 'a -> Variables_intf.any_var list
(** [vars_of_property p v] is the variables the value [v] of [p] references. *)
