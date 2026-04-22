type kind =
  | Sort_mismatch of { expected : Sort.t; found : Sort.t }
  | Unexpected_token of Token.kind
  | Missing_token of string
  | Bad_selector of string
  | Bad_value of { property : string; reason : string }
  | Unknown_at_rule of string
  | Unterminated of Sort.t

type t = { loc : Loc.t; sort : Sort.t; kind : kind }

let pp_kind : kind Pp.t =
 fun ctx -> function
  | Sort_mismatch { expected; found } ->
      Pp.string ctx "expected ";
      Sort.pp ctx expected;
      Pp.string ctx " but found ";
      Sort.pp ctx found
  | Unexpected_token k ->
      Pp.string ctx "unexpected ";
      Token.pp_kind ctx k
  | Missing_token what ->
      Pp.string ctx "missing ";
      Pp.string ctx what
  | Bad_selector reason ->
      Pp.string ctx "bad selector: ";
      Pp.string ctx reason
  | Bad_value { property; reason } ->
      Pp.string ctx "bad value for ";
      Pp.string ctx property;
      Pp.string ctx ": ";
      Pp.string ctx reason
  | Unknown_at_rule name ->
      Pp.string ctx "unknown at-rule @";
      Pp.string ctx name
  | Unterminated s ->
      Pp.string ctx "unterminated ";
      Sort.pp ctx s

let pp : t Pp.t =
 fun ctx { loc; sort; kind } ->
  pp_kind ctx kind;
  Pp.string ctx " at ";
  Loc.pp ctx loc;
  Pp.string ctx " (in ";
  Sort.pp ctx sort;
  Pp.char ctx ')'

let to_string t = Pp.to_string pp t

let sort_mismatch loc ~sort ~expected ~found =
  { loc; sort; kind = Sort_mismatch { expected; found } }

let unexpected_token loc ~sort k = { loc; sort; kind = Unexpected_token k }
let missing_token loc ~sort what = { loc; sort; kind = Missing_token what }

let bad_selector loc reason =
  { loc; sort = Sort.Selector; kind = Bad_selector reason }

let bad_value loc ~property ~reason =
  { loc; sort = Sort.Property_value; kind = Bad_value { property; reason } }

let unknown_at_rule loc name =
  { loc; sort = Sort.At_rule; kind = Unknown_at_rule name }

let unterminated loc s = { loc; sort = s; kind = Unterminated s }
