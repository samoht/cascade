module List = struct
  include Stdlib.List

  type 'a edit = Keep | Replace of 'a | Drop

  let rec same xs ys =
    match (xs, ys) with
    | [], [] -> true
    | x :: xs, y :: ys -> x == y && same xs ys
    | _ -> false

  let preserve before after = if same before after then before else after

  let map_preserve f xs =
    let rec loop = function
      | [] -> []
      | x :: rest as xs ->
          let y = f x in
          let rest' = loop rest in
          if y == x && rest' == rest then xs else y :: rest'
    in
    loop xs

  let filter_preserve f xs =
    let rec loop = function
      | [] -> []
      | x :: rest as xs ->
          if f x then
            let rest' = loop rest in
            if rest' == rest then xs else x :: rest'
          else loop rest
    in
    loop xs

  let filter_map_preserve f xs =
    let rec loop = function
      | [] -> []
      | x :: rest as xs -> (
          match f x with
          | Some y ->
              let rest' = loop rest in
              if y == x && rest' == rest then xs else y :: rest'
          | None -> loop rest)
    in
    loop xs

  let edit_preserve f xs =
    let rec loop = function
      | [] -> []
      | x :: rest as xs -> (
          match f x with
          | Keep ->
              let rest' = loop rest in
              if rest' == rest then xs else x :: rest'
          | Replace y ->
              let rest' = loop rest in
              if y == x && rest' == rest then xs else y :: rest'
          | Drop -> loop rest)
    in
    loop xs
end

module String = struct
  include Stdlib.String

  let is_ascii_lower_or_digit s =
    let n = length s in
    let rec loop i =
      if i >= n then true
      else
        match unsafe_get s i with
        | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> loop (i + 1)
        | _ -> false
    in
    loop 0

  let lowercase_ascii_preserve s =
    if is_ascii_lower_or_digit s then s else Stdlib.String.lowercase_ascii s

  type utf8 = Scalar of Uchar.t | Malformed of int

  (* The window is [[pos, stop)]: a sequence reaching past [stop] is truncated,
     and the bytes of it that are inside are a maximal subpart of their own. *)
  let utf8_window pos len s =
    match len with None -> length s | Some n -> min (pos + n) (length s)

  let utf8_element s stop i =
    let d = get_utf_8_uchar s i in
    let n = Uchar.utf_decode_length d in
    if i + n > stop then Malformed (stop - i)
    else if Uchar.utf_decode_is_valid d then Scalar (Uchar.utf_decode_uchar d)
    else Malformed n

  let utf8_decode ?(pos = 0) ?len s =
    let stop = utf8_window pos len s in
    if pos >= stop then None else Some (utf8_element s stop pos)

  (* [s], [stop] and [f] ride in parameters rather than in an enclosing scope,
     which would cost a closure per walk. *)
  let rec utf8_fold_from f acc s stop i =
    if i >= stop then acc
    else
      let e = utf8_element s stop i in
      let n =
        match e with Scalar u -> Uchar.utf_8_byte_length u | Malformed n -> n
      in
      utf8_fold_from f (f acc i e) s stop (i + n)

  let utf8_fold ?(pos = 0) ?len f acc s =
    utf8_fold_from f acc s (utf8_window pos len s) pos

  let utf8_length ?pos ?len s = utf8_fold ?pos ?len (fun n _ _ -> n + 1) 0 s

  (* A UTF-8 continuation byte, [10xxxxxx]: an index sitting on one is inside a
     sequence rather than at its start. *)
  let continuation s i = Char.code s.[i] land 0xc0 = 0x80

  (* Walk out of a sequence by no more than the three continuation bytes one can
     hold, so bytes that are not UTF-8 to begin with leave the index where it
     was rather than running off. *)
  let utf8_lead_before s i =
    let n = length s in
    let rec go i room =
      if room = 0 || i <= 0 || i >= n || not (continuation s i) then i
      else go (i - 1) (room - 1)
    in
    go i 3

  let utf8_lead_after s i =
    let n = length s in
    let rec go i room =
      if room = 0 || i >= n || not (continuation s i) then i
      else go (i + 1) (room - 1)
    in
    go i 3

  (* A diagnostic caret goes under the one line it marks, so a marker offset
     into a whole snippet has to be resolved against the line holding it. The
     line's own length comes back with it: a caret run is clamped to the line's
     end, and a caller drawing a single caret ignores it. The newline separating
     two lines belongs to neither, hence the extra character dropped per line
     walked past. *)
  let marker_line lines marker_pos =
    let rec find line remaining = function
      | [] -> (0, 0, 0)
      | [ last ] ->
          let len = utf8_length last in
          (line, min remaining len, len)
      | current :: rest ->
          let len = utf8_length current in
          if remaining <= len then (line, remaining, len)
          else find (line + 1) (remaining - len - 1) rest
    in
    find 0 (max 0 marker_pos) lines
end

let mix_int acc x = ((acc lsl 5) - acc) lxor x

(* [String.iter] would allocate a closure over the accumulator ref on every
   call, and an inner loop reading [s] and [n] out of the enclosing scope
   allocates one just the same; the loop carries all four in parameters. *)
let rec hash_from s n acc i =
  if i >= n then acc
  else hash_from s n (mix_int acc (Char.code (String.unsafe_get s i))) (i + 1)

let hash_string s = hash_from s (String.length s) 0x811c9dc5 0

module Table = struct
  module Make (H : Hashtbl.HashedType) = struct
    include Hashtbl.Make (H)

    let push tbl key value =
      let prev = find_opt tbl key |> Option.value ~default:[] in
      replace tbl key (value :: prev)
  end
end
