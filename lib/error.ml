type kind =
  | Sort_mismatch of { expected : Sort.t; found : Sort.t }
  | Unexpected_token of Token.kind
  | Missing_token of string
  | Bad_selector of string
  | Bad_value of { property : string; reason : string }
  | Unknown_at_rule of string
  | Unterminated of Sort.t

type t = { loc : Loc.t; sort : Sort.t; path : string list; kind : kind }

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
 fun ctx { loc; sort; path; kind } ->
  (match path with
  | [] -> ()
  | _ ->
      Pp.string ctx (String.concat "/" path);
      Pp.string ctx ": ");
  pp_kind ctx kind;
  Pp.string ctx " at ";
  Loc.pp ctx loc;
  Pp.string ctx " (in ";
  Sort.pp ctx sort;
  Pp.char ctx ')'

let to_string t = Pp.to_string pp t

exception Parse_error of t

let v ~loc ~sort kind = { loc; sort; path = []; kind }
let fail e = Stdlib.raise (Parse_error e)

let with_context label f =
  try f ()
  with Parse_error e ->
    Stdlib.raise (Parse_error { e with path = label :: e.path })

let sort_mismatch loc ~sort ~expected ~found =
  { loc; sort; path = []; kind = Sort_mismatch { expected; found } }

let unexpected_token loc ~sort k =
  { loc; sort; path = []; kind = Unexpected_token k }

let missing_token loc ~sort what =
  { loc; sort; path = []; kind = Missing_token what }

let bad_selector loc reason =
  { loc; sort = Sort.Selector; path = []; kind = Bad_selector reason }

let bad_value loc ~property ~reason =
  {
    loc;
    sort = Sort.Property_value;
    path = [];
    kind = Bad_value { property; reason };
  }

let unknown_at_rule loc name =
  { loc; sort = Sort.At_rule; path = []; kind = Unknown_at_rule name }

let unterminated loc s = { loc; sort = s; path = []; kind = Unterminated s }

let fail_sort_mismatch loc ~sort ~expected ~found =
  fail (sort_mismatch loc ~sort ~expected ~found)

let fail_unexpected_token loc ~sort k = fail (unexpected_token loc ~sort k)
let fail_missing_token loc ~sort what = fail (missing_token loc ~sort what)
let fail_bad_selector loc reason = fail (bad_selector loc reason)

let fail_bad_value loc ~property ~reason =
  fail (bad_value loc ~property ~reason)

let fail_unknown_at_rule loc name = fail (unknown_at_rule loc name)
let fail_unterminated loc s = fail (unterminated loc s)
