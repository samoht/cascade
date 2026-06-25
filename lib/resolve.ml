module type NODE = sig
  type t

  val equal : t -> t -> bool
  val name : t -> string option
  val id : t -> string option
  val classes : t -> string list
  val attribute : t -> string -> string option
  val parent : t -> t option
  val children : t -> t list
end

let ci a b = String.lowercase_ascii a = String.lowercase_ascii b
let words s = String.split_on_char ' ' s |> List.filter (( <> ) "")

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  if ln = 0 then true
  else
    let rec go i =
      if i + ln > lh then false
      else if String.sub hay i ln = needle then true
      else go (i + 1)
    in
    go 0

let starts s p =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let ends s p =
  let ls = String.length s and lp = String.length p in
  ls >= lp && String.sub s (ls - lp) lp = p

let attr_key : Selector.attr_name -> string = function
  | Selector.Regular s -> s
  | Selector.Data s -> "data-" ^ s
  | Selector.Aria a -> Aria.to_string a

module Make (N : NODE) = struct
  let preceding_siblings n =
    match N.parent n with
    | None -> []
    | Some p ->
        let rec before acc = function
          | [] -> List.rev acc
          | x :: _ when N.equal x n -> List.rev acc
          | x :: rest -> before (x :: acc) rest
        in
        before [] (N.children p)

  let imm_pred n =
    match List.rev (preceding_siblings n) with x :: _ -> Some x | [] -> None

  let is_first n = preceding_siblings n = []

  let is_last n =
    match N.parent n with
    | None -> true
    | Some p -> (
        match List.rev (N.children p) with x :: _ -> N.equal x n | [] -> true)

  let rec ancestors n =
    match N.parent n with None -> [] | Some p -> p :: ancestors p

  let attr_matches n name (m : Selector.attribute_match) =
    match N.attribute n (attr_key name) with
    | None -> false
    | Some v -> (
        match m with
        | Selector.Presence -> true
        | Selector.Exact s | Selector.Exact_quoted (s, _) -> v = s
        | Selector.Whitespace_list s | Selector.Whitespace_list_quoted (s, _) ->
            List.mem s (words v)
        | Selector.Prefix s | Selector.Prefix_quoted (s, _) ->
            s <> "" && starts v s
        | Selector.Suffix s | Selector.Suffix_quoted (s, _) ->
            s <> "" && ends v s
        | Selector.Substring s | Selector.Substring_quoted (s, _) ->
            s <> "" && contains v s
        | Selector.Hyphen_list s | Selector.Hyphen_list_quoted (s, _) ->
            v = s || starts v (s ^ "-"))

  let rec matches (sel : Selector.t) n : bool =
    match sel with
    | Selector.Universal _ -> true
    | Selector.Element (_, name) -> (
        match N.name n with Some nm -> ci nm name | None -> false)
    | Selector.Class c -> List.mem c (N.classes n)
    | Selector.Id i -> N.id n = Some i
    | Selector.Attribute (_, name, m, _) -> attr_matches n name m
    | Selector.Compound ps -> List.for_all (fun p -> matches p n) ps
    | Selector.List ss | Selector.Is ss | Selector.Where ss ->
        List.exists (fun s -> matches s n) ss
    | Selector.Not ss -> not (List.exists (fun s -> matches s n) ss)
    | Selector.Combined _ -> anchors sel n <> []
    | Selector.Root -> N.parent n = None
    | Selector.First_child -> is_first n
    | Selector.Last_child -> is_last n
    | Selector.Only_child -> is_first n && is_last n
    | Selector.Empty -> N.children n = []
    (* stateful / generated / unknown forms cannot be matched statically *)
    | _ -> false

  (* [Selector] is right-leaning: [a b > c] is [Combined (a, Descendant,
     Combined (b, Child, c))], so the rightmost compound (the subject) sits at
     the bottom of [right] and each combinator joins its [left] compound to the
     compound *after* it, not to the subject. [anchors sel n] returns the nodes
     where [sel]'s leftmost compound matches when [sel] matches with subject [n]
     (empty = no match): [right] must match [n], then for every node [a] that
     [right]'s leftmost compound anchored at, [left] must be [comb]-related to
     [a]. Threading the anchor this way is what the old subject-only
     [combinator] missed for any selector with two or more combinators. *)
  and anchors sel n =
    match sel with
    | Selector.Combined (left, comb, right) ->
        anchors right n |> List.concat_map (related left comb)
    | _ -> if matches sel n then [ n ] else []

  and related left comb a =
    match comb with
    | Selector.Descendant -> List.filter (matches left) (ancestors a)
    | Selector.Child -> (
        match N.parent a with Some p when matches left p -> [ p ] | _ -> [])
    | Selector.Next_sibling -> (
        match imm_pred a with Some s when matches left s -> [ s ] | _ -> [])
    | Selector.Subsequent_sibling ->
        List.filter (matches left) (preceding_siblings a)
    | _ -> []

  (* A selector list has no single specificity: a rule [s1, s2 {...}] cascades
     as if duplicated per branch, so the specificity that applies to [node] is
     that of the highest-specificity branch matching [node], not the whole
     list. *)
  let matched_specificity sel node =
    match Selector.as_list sel with
    | Some branches -> (
        match List.filter (fun b -> matches b node) branches with
        | [] -> Selector.specificity sel
        | b :: rest ->
            List.fold_left
              (fun acc b ->
                let s = Selector.specificity b in
                if compare s acc > 0 then s else acc)
              (Selector.specificity b) rest)
    | None -> Selector.specificity sel

  let resolve sheet node =
    let upsert acc d =
      let k = Declaration.property_name d in
      (k, d) :: List.remove_assoc k acc
    in
    let matched =
      Flatten.block sheet
      |> List.filter_map (function Stylesheet.Rule r -> Some r | _ -> None)
      |> List.mapi (fun i r ->
          let sel = Stylesheet.selector r in
          if matches sel node then
            Some (matched_specificity sel node, i, Stylesheet.declarations r)
          else None)
      |> List.filter_map Fun.id
      |> List.stable_sort (fun (s1, i1, _) (s2, i2, _) ->
          match compare s1 s2 with 0 -> compare i1 i2 | c -> c)
    in
    let decls = List.concat_map (fun (_, _, ds) -> ds) matched in
    let normal =
      List.filter (fun d -> not (Declaration.is_important d)) decls
    in
    let important = List.filter Declaration.is_important decls in
    (* normal first, then !important, last wins per property *)
    List.fold_left upsert [] (normal @ important) |> List.rev_map snd
end
