type attribute_match =
  | Presence
  | Exact of string
  | Whitespace_list of string
  | Hyphen_list of string
  | Prefix of string
  | Suffix of string
  | Substring of string

type combinator =
  | Descendant
  | Child
  | Next_sibling
  | Subsequent_sibling
  | Column

type ns = Any | None | Prefix of string
type attr_flag = Case_insensitive | Case_sensitive

type t =
  | Element of ns option * string
  | Class of string
  | Id of string
  | Universal of ns option
  | Attribute of ns option * string * attribute_match * attr_flag option
  | Pseudo_class of string
  | Pseudo_element of string
  | Pseudo_element_fun of string * t list
  | Pseudo_element_fun_idents of string * string list
  | Where of t list
  | Not of t list
  | Fun of string * t list
  | Compound of t list
  | Combined of t * combinator * t
  | List of t list

type nth = Even | Odd | An_plus_b of int * int
