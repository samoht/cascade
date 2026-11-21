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

(* CSS Values 4 sec. 10.1 puts a math function wherever its resolved type
   stands. A slot whose typed value holds a literal number takes one that folds
   to a constant. *)
let read_constant_number t =
  if Cursor.looking_at_func "calc" t || looking_at_math_function t then
    (* The call alone: [read_number] refuses a number after it, which the slot
       after this one may hold. *)
    let call = Option.to_list (Cursor.next t) in
    match normalize_number (read_number (Cursor.sub t call)) with
    | Num f -> f
    | _ -> Cursor.err_invalid t "a math function here must fold to a number"
  else Cursor.number t

(* Sec. 10.9 rounds a math function at an [<integer>] to the nearest integer,
   the one towards positive infinity on a tie. *)
let read_constant_integer t =
  if Cursor.looking_at_func "calc" t || looking_at_math_function t then
    int_of_float (Float.floor (read_constant_number t +. 0.5))
  else Cursor.int t
