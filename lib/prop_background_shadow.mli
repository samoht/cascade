(** The [box-shadow] property: the [Shadow] layer reader, the printer and the
    normal form, shared with the [drop-shadow()] filter. *)

(** The reader of a single shadow layer, shared with [drop-shadow()]. *)
module Shadow : sig
  val read : Cursor.t -> Properties_intf.shadow
  (** [read t] reads one [box-shadow] layer from [t]: [inset], two to four
      lengths and an optional colour, in any order the grammar allows. *)
end

val read_shadow : Cursor.t -> Properties_intf.shadow
(** [read_shadow t] reads a [box-shadow] value from [t]. *)

val pp_color_after_length : Pp.ctx -> Values.color -> unit
(** [pp_color_after_length ctx c] prints a space then the colour [c], the
    trailing colour slot of a shadow. *)

val pp_shadow : Properties_intf.shadow Pp.t
(** [pp_shadow] prints a [box-shadow] value. *)

val normalize_shadow :
  ?lossless:bool -> Properties_intf.shadow -> Properties_intf.shadow
(** [normalize_shadow ?lossless v] is [v] in canonical form. *)

val shadow :
  ?inset:bool ->
  ?inset_var:string ->
  ?inset_var_no_fallback:bool ->
  ?h_offset:Values.length ->
  ?v_offset:Values.length ->
  ?blur:Values.length ->
  ?spread:Values.length ->
  ?color:Values.color ->
  unit ->
  Properties_intf.shadow
(** [shadow ?inset ?inset_var ?inset_var_no_fallback ?h_offset ?v_offset ?blur
     ?spread ?color ()] is a shadow value; see [Properties.shadow]. *)
