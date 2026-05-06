(** Keyframe position types for type-safe [\@keyframes] construction. *)

(** A single keyframe position. *)
type position =
  | From  (** [from] or [0%] *)
  | To  (** [to] or [100%] *)
  | Percent of float  (** Percentage like [50%] *)
  | Timeline_range of string * float
      (** Scroll-Driven Animations 1: a [<timeline-range-name> <percentage>]
          selector such as [entry 0%]. The string is the range name keyword
          ([cover], [contain], [entry], [exit], [entry-crossing],
          [exit-crossing]). *)

let string_of_pct p =
  if Float.is_integer p then Int.to_string (Float.to_int p)
  else Float.to_string p

let string_of_position = function
  | From -> "from"
  | To -> "to"
  | Percent p -> string_of_pct p ^ "%"
  | Timeline_range (name, p) -> name ^ " " ^ string_of_pct p ^ "%"

(** A keyframe selector (one or more positions). *)
type selector = Positions of position list

let string_of_selector = function
  | Positions positions ->
      String.concat ", " (List.map string_of_position positions)

let position_to_percent = function
  | From -> 0.
  | To -> 100.
  | Percent p -> p
  | Timeline_range (_, p) -> p

(* Range-named selectors live on different timeline progress axes than the
   default 0..100% one, so they don't compare meaningfully against bare
   percentages. We fall back to comparing the offsets within their own
   namespace, then break ties by the (alphabetic) range-name order. *)
let position_compare a b =
  match (a, b) with
  | Timeline_range (n1, p1), Timeline_range (n2, p2) ->
      let c = String.compare n1 n2 in
      if c <> 0 then c else Float.compare p1 p2
  | Timeline_range _, _ -> 1
  | _, Timeline_range _ -> -1
  | _ -> Float.compare (position_to_percent a) (position_to_percent b)

let timeline_range_names =
  [ "cover"; "contain"; "entry"; "exit"; "entry-crossing"; "exit-crossing" ]

let parse_percent s =
  if String.length s > 0 && s.[String.length s - 1] = '%' then
    try Some (float_of_string (String.sub s 0 (String.length s - 1)))
    with Failure _ | Invalid_argument _ -> None
  else None

(** Parse a position string like "from", "to", "50%", or "entry 0%". *)
let position_of_string s =
  let s = String.trim s in
  if String.equal s "from" then Some From
  else if String.equal s "to" then Some To
  else
    match parse_percent s with
    | Some p when p >= 0. && p <= 100. -> Some (Percent p)
    | Some _ -> None
    | None ->
        (* Try [<timeline-range-name> <percentage>] (Scroll-Driven Animations
           1 §8.1). *)
        (match String.index_opt s ' ' with
        | None -> None
        | Some i ->
            let name = String.sub s 0 i in
            let rest = String.sub s (i + 1) (String.length s - i - 1) in
            let rest = String.trim rest in
            if List.mem (String.lowercase_ascii name) timeline_range_names then
              match parse_percent rest with
              | Some p -> Some (Timeline_range (name, p))
              | None -> None
            else None)

(** Parse a selector string like "from", "50%", or "from, 50%". *)
let selector_of_string s =
  let parts = String.split_on_char ',' s in
  let positions = List.filter_map position_of_string parts in
  if List.length positions = List.length parts && positions <> [] then
    Positions positions
  else invalid_arg ("invalid keyframe selector: " ^ s)

let selector_equal a b = string_of_selector a = string_of_selector b
