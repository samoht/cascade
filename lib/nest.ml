open Stylesheet

let contains sel =
  Selector.any (function Selector.Nesting -> true | _ -> false) sel

(* CSS Nesting 1 sec. 4: [&] stands for [:is(<parent selector list>)]. A parent
   that carries a combinator or lists alternatives needs that wrapper, or its
   own structure escapes: dropping it turns [.dark &] over parent [.a .b] into
   [.dark .a .b], which demands that [.a] itself sit inside [.dark]. Where the
   parent goes in with nothing to its left, [splices_bare] below reads whether
   the wrapper is redundant. *)
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

let is_list = function Selector.List _ -> true | _ -> false

(* Selectors 4 sec. 4.2 weighs [:is()] as its most specific argument, so
   spelling a parent selector list out in place of the wrapper holds each branch
   at its own weight only when the branches already agree on one. *)
let one_weight = function
  | Selector.List (branch :: rest) ->
      let weight = Selector.specificity branch in
      List.for_all
        (fun b -> Selector.equal_specificity (Selector.specificity b) weight)
        rest
  | _ -> true

(* The leading-[&] shortcut needs the parent to have no comma of its own: a
   [List] spliced into a larger selector hands its branches to that selector, so
   [.a, .b] under [&:hover] reads back as [.a] and [.b:hover] and the
   pseudo-class binds to the last branch alone. Where the whole selector is the
   parent there is nothing to splice into, and the list stands for itself. *)
let splices_bare ~leftmost ~parent sel =
  if is_list parent then
    match sel with Selector.Nesting -> one_weight parent | _ -> false
  else leftmost && heads sel && count_nesting sel = 1

let substitute ?(leftmost = true) ~parent sel =
  let verbatim = (not (complex parent)) || splices_bare ~leftmost ~parent sel in
  let parent = if verbatim then parent else Selector.Is [ parent ] in
  Selector.map (function Selector.Nesting -> parent | s -> s) sel

(* A parent the nested selector never names still lands inside it, to the left
   of a combinator, so it needs the same wrapper. *)
let grouped parent = if is_list parent then Selector.Is [ parent ] else parent

(* CSS Nesting 1 sec. 3: a nested selector list is relative to the parent branch
   by branch, so [.p { a, b { ... } }] is [.p a, .p b]. Combining the parent
   with the list as a whole would put the combinator on the first branch only
   and let the rest escape as top-level selectors. *)
let rec combine parent child =
  match child with
  | Selector.List branches -> Selector.List (List.map (combine parent) branches)
  | Selector.Relative (comb, right) ->
      Selector.Combined
        (grouped parent, comb, substitute ~leftmost:false ~parent right)
  | _ when contains child -> substitute ~parent child
  | _ -> Selector.Combined (grouped parent, Selector.Descendant, child)

(* CSS Selectors 4 sec. 3.6.3 to sec. 3.6.5 bound what may follow a
   pseudo-element: no combinator, and in its own compound only the
   pseudo-classes and sub-pseudo-elements that pseudo-element takes. Nesting
   composes exactly those selectors out of a valid parent and a valid child, so
   neither half meets the reader's check. Such a rule matches nothing in any
   engine, so drop the branches that overstep and keep the ones a reader
   accepts. *)
let keep_readable_branches (selector : Selector.t) =
  let keeps sel =
    (not (Selector.has_combinator_after_pseudo_element sel))
    && not (Selector.has_refused_simple_in_compound sel)
  in
  match selector with
  | Selector.List branches -> (
      match List.filter keeps branches with
      | [] -> Option.None
      | [ branch ] -> Option.Some branch
      | branches -> Option.Some (Selector.List branches))
  | sel when keeps sel -> Option.Some sel
  | _ -> Option.None

(* Merging composes the same selector flattening does, so it drops the same
   branches. A wrapper left with no branch to merge into keeps nothing of its
   own, and the empty-rule pass takes it. *)
let rec merge_lone (rule : rule) =
  match (rule.declarations, rule.nested) with
  | [], [ Rule child ] when not (is_list rule.selector) -> (
      match keep_readable_branches (combine rule.selector child.selector) with
      | Option.Some selector -> merge_lone { child with selector }
      | Option.None -> { rule with nested = [] })
  | _ -> rule

(* The same question, asked of a rule that stays nested: what has to be readable
   is the branch composed with the parent, but what the rule keeps is the branch
   in its own spelling. *)
let live_under parent branch =
  Option.is_some (keep_readable_branches (combine parent branch))

let live_branches parent (selector : Selector.t) =
  match selector with
  | Selector.List branches -> (
      match Common.List.filter_preserve (live_under parent) branches with
      | kept when kept == branches -> Option.Some selector
      | [] -> Option.None
      | [ branch ] -> Option.Some branch
      | kept -> Option.Some (Selector.List kept))
  | sel -> if live_under parent sel then Option.Some sel else Option.None

(* Only a pseudo-element the parent carries can bar what nests under it, and
   every rule is walked with its own selector as parent, so a parent without one
   has nothing dead below to look for. *)
let under_pseudo_element sel = Selector.any Selector.is_pseudo_element sel

(* Merging is one shape of the composition, and the branches it drops are dead
   wherever they sit. Walk the body under the parent selector and take them
   there too, subtree and all, as flattening does: a conditional block keeps its
   parent's selector for what it wraps, and a kept child becomes the parent of
   its own. *)
let rec live_statements parent (stmts : statement list) =
  Common.List.filter_map_preserve (live_statement parent) stmts

and live_statement parent (stmt : statement) =
  match stmt with
  | Rule child -> (
      match live_branches parent child.selector with
      | Option.None -> Option.None
      | Option.Some selector ->
          let nested = live_statements (combine parent selector) child.nested in
          if selector == child.selector && nested == child.nested then
            Option.Some stmt
          else Option.Some (Rule { child with selector; nested }))
  | stmt -> Option.Some (map_statement_children (live_statements parent) stmt)

let drop_dead_nested (rule : rule) =
  match rule.nested with
  | [] -> rule
  | _ when not (under_pseudo_element rule.selector) -> rule
  | body ->
      let nested = live_statements rule.selector body in
      if nested == body then rule else { rule with nested }

(* What a run of nested statements would have to cross to move ahead of them:
   every declaration they can write at any depth, read through
   [statement_declarations] and [statement_children] so no block at-rule hides
   one. An unknown at-rule is raw text whose body cannot be read at all, so it
   is [Opaque]: a barrier nothing crosses. *)
type barrier = Opaque | Crossed of Declaration.declaration list

let rec crossing barrier (stmts : statement list) =
  match (barrier, stmts) with
  | Opaque, _ | _, [] -> barrier
  | Crossed decls, stmt :: rest ->
      let barrier =
        match stmt with
        | Unknown_at_rule _ -> Opaque
        | _ ->
            crossing
              (Crossed (List.rev_append (statement_declarations stmt) decls))
              (statement_children stmt)
      in
      crossing barrier rest

let crosses_freely barrier decl =
  match barrier with
  | Opaque -> false
  | Crossed decls -> Shorthand.declarations_commute [ decl ] decls

(* CSS Nesting 1 sec. 3.4 keeps a declaration written after a nested statement
   behind it. Moving one back into the rule's own run is cascade-neutral exactly
   when it commutes with everything it would cross, so the body is walked once,
   growing the barrier with each statement kept and with each declaration that
   had to stay. Selectors are left out of the question: a nested rule matches
   other elements than its parent, but assuming it matches the same one only
   refuses a hoist the cascade would have allowed. *)
let hoist_declaration_runs (rule : rule) =
  let hoist_run barrier run =
    List.fold_left
      (fun (hoisted, barrier, kept) decl ->
        if crosses_freely barrier decl then (decl :: hoisted, barrier, kept)
        else
          let barrier =
            match barrier with
            | Opaque -> Opaque
            | Crossed decls -> Crossed (decl :: decls)
          in
          (hoisted, barrier, decl :: kept))
      ([], barrier, []) run
  in
  let rec go hoisted barrier nested = function
    | [] -> (List.rev hoisted, List.rev nested)
    | Declarations run :: rest ->
        let run_hoisted, barrier, kept = hoist_run barrier run in
        let nested =
          match List.rev kept with
          | [] -> nested
          | kept -> Declarations kept :: nested
        in
        go (run_hoisted @ hoisted) barrier nested rest
    | stmt :: rest ->
        go hoisted (crossing barrier [ stmt ]) (stmt :: nested) rest
  in
  match go [] (Crossed []) [] rule.nested with
  | [], _ -> rule
  | hoisted, nested ->
      { rule with declarations = rule.declarations @ hoisted; nested }

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
