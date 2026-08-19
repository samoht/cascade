open Stylesheet

let contains sel =
  Selector.any (function Selector.Nesting -> true | _ -> false) sel

(* CSS Nesting 1 sec. 2.1: [&] stands for [:is(<parent selector list>)]. A
   parent that carries a combinator or lists alternatives needs that wrapper, or
   its own structure escapes: dropping it turns [.dark &] over parent [.a .b]
   into [.dark .a .b], which demands that [.a] itself sit inside [.dark]. Where
   [&] heads the selector nothing can escape to its left, so the wrapper is
   redundant there and the parent goes in verbatim. *)
let complex = function
  | Selector.List _ | Selector.Combined _ | Selector.Relative _ -> true
  | _ -> false

let count_nesting sel =
  let n = ref 0 in
  ignore
    (Selector.any
       (fun s ->
         (match s with Selector.Nesting -> incr n | _ -> ());
         false)
       sel);
  !n

let rec heads = function
  | Selector.Nesting -> true
  | Selector.Compound (x :: _) -> heads x
  | Selector.Combined (l, _, _) -> heads l
  | _ -> false

let substitute ?(leftmost = true) ~parent sel =
  let verbatim =
    (not (complex parent)) || (leftmost && heads sel && count_nesting sel = 1)
  in
  let parent = if verbatim then parent else Selector.Is [ parent ] in
  Selector.map (function Selector.Nesting -> parent | s -> s) sel

(* CSS Nesting 1 §2: a nested selector list is relative to the parent branch by
   branch, so [.p { a, b { ... } }] is [.p a, .p b]. Combining the parent with
   the list as a whole would put the combinator on the first branch only and let
   the rest escape as top-level selectors. *)
let rec combine parent child =
  match child with
  | Selector.List branches -> Selector.List (List.map (combine parent) branches)
  | Selector.Relative (comb, right) ->
      Selector.Combined (parent, comb, substitute ~leftmost:false ~parent right)
  | _ when contains child -> substitute ~parent child
  | _ -> Selector.Combined (parent, Selector.Descendant, child)

let is_list = function Selector.List _ -> true | _ -> false

let rec merge_lone (rule : rule) =
  match (rule.declarations, rule.nested) with
  | [], [ Rule child ] when not (is_list rule.selector) ->
      merge_lone { child with selector = combine rule.selector child.selector }
  | _ -> rule

let rec strip_prefix (parent : Selector.t) (child : Selector.t) =
  match (parent, child) with
  | _, Selector.Combined (cp, comb, crest) when Selector.equal cp parent ->
      Some
        (if comb = Selector.Descendant then crest
         else Selector.Relative (comb, crest))
  | Selector.Combined (pp, pcomb, prest), Selector.Combined (cp, ccomb, crest)
    when Selector.equal_combinator pcomb ccomb && Selector.equal pp cp ->
      strip_prefix prest crest
  | _, Selector.Compound cps -> (
      let pps =
        match parent with Selector.Compound l -> l | single -> [ single ]
      in
      let rec drop p c =
        match (p, c) with
        | [], rest -> Some rest
        | ph :: pt, ch :: ct when ph = ch -> drop pt ct
        | _ -> None
      in
      match drop pps cps with
      | Some (_ :: _ as suffix) ->
          Some (Selector.Compound (Selector.Nesting :: suffix))
      | _ -> None)
  | _ -> None

let extends a b = strip_prefix a b <> None

let identifying_components sel =
  let acc = ref [] in
  ignore
    (Selector.any
       (fun s ->
         (match s with
         | Selector.Class _ | Selector.Id _
         | Selector.Element (_, _)
         | Selector.Attribute _ ->
             acc := s :: !acc
         | _ -> ());
         false)
       sel);
  !acc

let compete a b =
  let ca = identifying_components a in
  List.exists (fun t -> List.mem t (identifying_components b)) ca

let rec chain (root : rule) = function
  | [] -> root
  | (child : rule) :: rest -> (
      match strip_prefix root.selector child.selector with
      | Some rel ->
          let nested_child = chain child rest in
          let nested_child = { nested_child with selector = rel } in
          { root with nested = root.nested @ [ Rule nested_child ] }
      | None -> root)

let shortens before after =
  let len rules =
    List.fold_left
      (fun acc r -> acc + Pp.size ~minify:true Stylesheet.pp_rule r)
      0 rules
  in
  len [ after ] < len before

open Common

let preserve = List.preserve

let rules (rules : rule list) =
  let arr : rule array = Array.of_list rules in
  let n = Array.length arr in
  let chains = ref [] in
  let i = ref 0 in
  while !i < n do
    let start = !i in
    incr i;
    while !i < n && extends arr.(!i - 1).selector arr.(!i).selector do
      incr i
    done;
    chains := (start, !i - start) :: !chains
  done;
  let chains = List.rev !chains in
  let isolated start len =
    let members = Array.sub arr start len in
    let related_outside =
      Array.to_list arr
      |> List.mapi (fun idx r -> (idx, r))
      |> List.exists (fun (idx, (r : rule)) ->
          (idx < start || idx >= start + len)
          && Array.exists
               (fun (m : rule) -> compete m.selector r.selector)
               members)
    in
    not related_outside
  in
  let rules' =
    List.concat_map
      (fun (start, len) ->
        let members = Array.to_list (Array.sub arr start len) in
        match members with
        | root :: (_ :: _ as rest)
          when (not (is_list root.selector)) && isolated start len ->
            let nested = chain root rest in
            if shortens members nested then [ nested ] else members
        | _ -> members)
      chains
  in
  preserve rules rules'

let statements (stmts : statement list) =
  let rec should_try count = function
    | [] -> count <= 128
    | Rule r :: _ when r.Stylesheet_intf.nested <> [] -> true
    | Rule _ :: rest when count < 128 -> should_try (count + 1) rest
    | Rule _ :: _ -> false
    | _ :: rest -> should_try count rest
  in
  if not (should_try 0 stmts) then stmts
  else
    let rec span stmt_acc rule_acc = function
      | (Rule r as stmt) :: rest -> span (stmt :: stmt_acc) (r :: rule_acc) rest
      | rest -> (List.rev stmt_acc, List.rev rule_acc, rest)
    in
    let rec go acc = function
      | [] -> List.rev acc
      | Rule _ :: _ as l ->
          let stmts, rule_list, rest = span [] [] l in
          let rules' = rules rule_list in
          let synthesized =
            if rules' == rule_list then stmts
            else List.map (fun r -> Rule r) rules'
          in
          go (List.rev_append synthesized acc) rest
      | s :: rest -> go (s :: acc) rest
    in
    preserve stmts (go [] stmts)
