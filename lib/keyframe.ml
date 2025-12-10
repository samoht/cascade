(** Keyframe position types for type-safe [\@keyframes] construction. *)

(** A single keyframe position. *)
type position =
  | From  (** [from] or [0%] *)
  | To  (** [to] or [100%] *)
  | Percent of float  (** Percentage like [50%] *)

let position_to_string = function
  | From -> "from"
  | To -> "to"
  | Percent p -> Printf.sprintf "%g%%" p

(** A keyframe selector (one or more positions, or raw string). *)
type selector = Positions of position list | Raw of string

let selector_to_string = function
  | Positions positions ->
      String.concat ", " (List.map position_to_string positions)
  | Raw s -> s

let position_compare a b =
  match (a, b) with
  | From, From -> 0
  | To, To -> 0
  | Percent p1, Percent p2 -> Float.compare p1 p2
  | From, _ -> -1
  | _, From -> 1
  | To, Percent _ -> 1
  | Percent _, To -> -1

(** Parse a position string like "from", "to", or "50%". *)
let position_of_string s =
  let s = String.trim s in
  if String.equal s "from" then Some From
  else if String.equal s "to" then Some To
  else if String.length s > 0 && s.[String.length s - 1] = '%' then
    try
      let p = float_of_string (String.sub s 0 (String.length s - 1)) in
      Some (Percent p)
    with _ -> None
  else None

(** Parse a selector string like "from", "50%", or "from, 50%". Always succeeds
    \- returns Raw if parsing fails. *)
let selector_of_string s =
  let parts = String.split_on_char ',' s in
  let positions = List.filter_map position_of_string parts in
  if List.length positions = List.length parts && positions <> [] then
    Positions positions
  else Raw s

let selector_equal a b = selector_to_string a = selector_to_string b
