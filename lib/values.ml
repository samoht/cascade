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

(* A slot holding a [<number>] takes a math function that does not fold as well,
   as the calculation it is. *)
let read_number_slot t : number =
  if Cursor.looking_at_func "calc" t || looking_at_math_function t then
    let call = Option.to_list (Cursor.next t) in
    normalize_number (read_number (Cursor.sub t call))
  else Num (Cursor.number t)

(* At an [<integer>] a call that folds is rounded as sec. 10.9 rounds it, and a
   literal must be an integer token. *)
let read_integer_slot t : number =
  if Cursor.looking_at_func "calc" t || looking_at_math_function t then
    match read_number_slot t with
    | Num f when Float.is_finite f -> Num (Float.floor (f +. 0.5))
    | n -> n
  else Num (Float.of_int (Cursor.int t))

(* CSS Syntax 3 sec. 4.3.13 types digits alone as an integer and an exponent as
   a number, so a value at an [<integer>] is written in digits. Past the [int]
   range the shorter of the range's end, which every target clamps alike, and
   the value kept in its [calc()] stands for it. *)
let pp_integer_slot ctx (n : number) =
  match n with
  | Num f when Float.is_integer f && Float.abs f < 0x1p62 ->
      Pp.int ctx (int_of_float f)
  | Num f when Float.is_integer f ->
      let clamped = string_of_int (if f < 0. then min_int else max_int) in
      let call =
        String.concat ""
          [
            "calc(";
            Pp.string_of_float ~drop_leading_zero:(Pp.minified ctx) f;
            ")";
          ]
      in
      Pp.string ctx
        (if String.length call < String.length clamped then call else clamped)
  | n -> pp_number ctx n
