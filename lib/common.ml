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
end
