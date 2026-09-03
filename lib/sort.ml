type t =
  | Token
  | Component
  | Block
  | Function
  | At_rule
  | Qualified_rule
  | Declaration
  | Selector
  | Property_value
  | Stylesheet

(* A closed enum with no payloads, so structural equality is its equality. *)
let equal (a : t) (b : t) = a = b

let pp : t Pp.t =
 fun ctx t ->
  let s =
    match t with
    | Token -> "token"
    | Component -> "component"
    | Block -> "block"
    | Function -> "function"
    | At_rule -> "at-rule"
    | Qualified_rule -> "qualified-rule"
    | Declaration -> "declaration"
    | Selector -> "selector"
    | Property_value -> "property-value"
    | Stylesheet -> "stylesheet"
  in
  Pp.string ctx s

let to_string t = Pp.to_string pp t
