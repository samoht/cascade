let has_prefix name =
  String.length name >= 2 && name.[0] = '-' && name.[1] = '-'

let is_valid name = String.length name > 2 && has_prefix name
let add_prefix name = if has_prefix name then name else "--" ^ name

let strip_prefix name =
  if has_prefix name then String.sub name 2 (String.length name - 2) else name
