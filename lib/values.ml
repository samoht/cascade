(** CSS Values & Units parsing using Reader API *)

include Values_intf
include Values_color_name
include Values_var
include Values_calc
include Values_length
include Values_color_fold
include Values_numeric
include Values_color_pp
include Values_read_calc
include Values_read_numeric
include Values_read_color
include Values_color_normalize

let equal_length (a : length) b = a = b
let equal_length_percentage (a : length_percentage) b = a = b
let equal_number_percentage (a : number_percentage) b = a = b
let equal_color (a : color) b = a = b

(* Bounded like [Declaration.hash], for the same reason: the depth cap keeps the
   cost of fingerprinting a deep [color-mix()] tree off the caller's inner
   loop. *)
let hash_color (c : color) = Hashtbl.hash_param 30 100 c
