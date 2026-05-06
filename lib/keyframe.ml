(** Keyframe position types for type-safe [\@keyframes] construction. *)

(** A single keyframe position. *)
type position =
  | From  (** [from] or [0%] *)
  | To  (** [to] or [100%] *)
  | Percent of float  (** Percentage like [50%] *)

let string_of_position = function
  | From -> "from"
  | To -> "to"
  | Percent p ->
      let p_str =
        if Float.is_integer p then Int.to_string (Float.to_int p)
        else Float.to_string p
      in
      p_str ^ "%"

(** A keyframe selector (one or more positions). *)
type selector = Positions of position list

let string_of_selector = function
  | Positions positions ->
      String.concat ", " (List.map string_of_position positions)

let position_to_percent = function From -> 0. | To -> 100. | Percent p -> p

let position_compare a b =
  Float.compare (position_to_percent a) (position_to_percent b)

(** Parse a position string like "from", "to", or "50%". *)
let position_of_string s =
  let s = String.trim s in
  if String.equal s "from" then Some From
  else if String.equal s "to" then Some To
  else if String.length s > 0 && s.[String.length s - 1] = '%' then
    try
      let p = float_of_string (String.sub s 0 (String.length s - 1)) in
      if p >= 0. && p <= 100. then Some (Percent p) else None
    with Failure _ | Invalid_argument _ -> None
  else None

(** Parse a selector string like "from", "50%", or "from, 50%". *)
let selector_of_string s =
  let parts = String.split_on_char ',' s in
  let positions = List.filter_map position_of_string parts in
  if List.length positions = List.length parts && positions <> [] then
    Positions positions
  else invalid_arg ("invalid keyframe selector: " ^ s)

let selector_equal a b = string_of_selector a = string_of_selector b
