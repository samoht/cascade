module type NODE = sig
  type t

  val equal : t -> t -> bool
  val name : t -> string option
  val id : t -> string option
  val classes : t -> string list
  val attribute : t -> string -> string option
  val parent : t -> t option
  val children : t -> t list
  val text_children : t -> string list
end

type match_result = Matches | No_match | Unsupported

let of_bool b = if b then Matches else No_match

(* [Unsupported] beats both answers in every combining form. That is what keeps
   it a fact about the selector alone: were a compound to settle on [No_match]
   because one part failed before an unsupported part was looked at, the same
   selector would come back unsupported against one node and merely unmatched
   against another, and {!supported} could not exist. *)
let conj a b =
  match (a, b) with
  | Unsupported, _ | _, Unsupported -> Unsupported
  | No_match, _ | _, No_match -> No_match
  | Matches, Matches -> Matches

let disj a b =
  match (a, b) with
  | Unsupported, _ | _, Unsupported -> Unsupported
  | Matches, _ | _, Matches -> Matches
  | No_match, No_match -> No_match

let negate = function
  | Matches -> No_match
  | No_match -> Matches
  | Unsupported -> Unsupported

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

(* Cascade layers. Layer names form a tree: [@layer a.b] names the sublayer [b]
   of [a], and so does [@layer a { @layer b { ... } }]. The cascade only needs
   the flattened pre-order of that tree, so a layer is keyed by its dotted path
   and the order is a list of paths, oldest first (css-cascade-5 sec. 6.4.2). *)

let parent_layer name =
  match String.rindex_opt name '.' with
  | None -> None
  | Some i -> Some (String.sub name 0 i)

(* Each unnamed [@layer { }] block is a layer of its own, distinct from every
   other one, so key it by a counter behind a prefix no author can write: NUL
   never survives tokenisation, css-syntax-3 sec. 3.3 turns it into U+FFFD. *)
let anonymous_layer n = "\000" ^ string_of_int n

let qualify parent name =
  match parent with None -> name | Some p -> p ^ "." ^ name

(* A sublayer is ordered inside its parent, not at the end of the sheet: in
   [@layer a.b {} @layer c {} @layer a.d {}] the layer [a.d] joins [a]'s subtree
   and so still precedes [c]. *)
let insert_layer order name =
  if List.mem name order then order
  else
    match parent_layer name with
    | None -> order @ [ name ]
    | Some p ->
        let inside n = n = p || starts n (p ^ ".") in
        (* [p]'s subtree is contiguous, so the insertion point is just past its
           last member; [entered] says the scan has reached it. *)
        let rec insert entered = function
          | x :: rest when inside x -> x :: insert true rest
          | rest when entered -> name :: rest
          | [] -> [ name ]
          | x :: rest -> x :: insert entered rest
        in
        insert false order

(* Naming a sublayer creates its ancestors first, so [@layer a.b] declares [a]
   then [a.b]. *)
let declare_layer order name =
  let rec path n acc =
    match parent_layer n with None -> n :: acc | Some p -> path p (n :: acc)
  in
  List.fold_left insert_layer order (path name [])

(* [layered_rules stmts] is the sheet's layer order together with every rule
   paired with the layer it sits in ([None] for the unlayered ones), both in
   source order. An [@layer a, b;] statement declares its names without
   contributing a rule. *)
let layered_rules stmts =
  let anon = ref 0 in
  let rec go parent acc stmts =
    List.fold_left
      (fun (order, rules) stmt ->
        match stmt with
        | Stylesheet.Rule r -> (order, (parent, r) :: rules)
        | Stylesheet.Layer_decl names ->
            ( List.fold_left
                (fun order name -> declare_layer order (qualify parent name))
                order names,
              rules )
        | Stylesheet.Layer (name, body) ->
            let name =
              match name with
              | Some n -> qualify parent n
              | None ->
                  incr anon;
                  qualify parent (anonymous_layer !anon)
            in
            go (Some name) (declare_layer order name, rules) body
        | _ -> (order, rules))
      acc stmts
  in
  let order, rules = go None ([], []) stmts in
  (order, List.rev rules)

let layer_order stmts = fst (layered_rules stmts)

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

  (* selectors-4 sec. 13.2: [:empty] represents "an element that has no children
     except, optionally, document white space characters", counting "only
     element nodes and content nodes ... whose data has a non-zero length", so a
     comment or a processing instruction never makes an element non-empty and
     never reaches {!NODE.text_children}. Document white space characters are
     spaces (U+0020), tabs (U+0009) and segment breaks (css-text-4 sec. 4.3);
     carriage returns and form feeds become segment breaks when the document is
     parsed. U+00A0 is none of them, which is why the spec lists
     [<div>&nbsp;</div>] among the elements [div:empty] does not represent. *)
  let is_document_white_space = function
    | ' ' | '\t' | '\n' | '\r' | '\012' -> true
    | _ -> false

  let is_empty n =
    N.children n = []
    && List.for_all (String.for_all is_document_white_space) (N.text_children n)

  (* Selectors 4 is far wider than a matcher with no document behind it can
     decide, so the answer has three values. [Unsupported] is not "does not
     match": it is "this library has no model for this selector", and a caller
     must not read it as a negative - {!Apply} keeps such a rule in the
     stylesheet rather than inline it and drop it. Every arm returning it does
     so without consulting the node, which is what {!supported} rests on. *)
  let rec match_selector (sel : Selector.t) n : match_result =
    match sel with
    | Selector.Universal None -> Matches
    | Selector.Element (None, name) ->
        of_bool (match N.name n with Some nm -> ci nm name | None -> false)
    | Selector.Class c -> of_bool (List.mem c (N.classes n))
    | Selector.Id i -> of_bool (N.id n = Some i)
    | Selector.Attribute (None, name, m, None) ->
        of_bool (attr_matches n name m)
    | Selector.Compound ps ->
        List.fold_left (fun acc p -> conj acc (match_selector p n)) Matches ps
    | Selector.List ss | Selector.Is ss | Selector.Where ss -> any ss n
    | Selector.Not ss -> negate (any ss n)
    | Selector.Combined _ -> (
        match anchors sel n with
        | None -> Unsupported
        | Some [] -> No_match
        | Some (_ :: _) -> Matches)
    | Selector.Root -> of_bool (N.parent n = None)
    | Selector.First_child -> of_bool (is_first n)
    | Selector.Last_child -> of_bool (is_last n)
    | Selector.Only_child -> of_bool (is_first n && is_last n)
    | Selector.Empty -> of_bool (is_empty n)
    (* A namespace, an attribute case flag, and every stateful, generated or
       tree-external form. The values compared above are verbatim, so [i] and
       [s] would both be ignored, and no node here carries a namespace. *)
    | _ -> Unsupported

  and any ss n =
    List.fold_left (fun acc s -> disj acc (match_selector s n)) No_match ss

  (* [Selector] is right-leaning: [a b > c] is [Combined (a, Descendant,
     Combined (b, Child, c))], so the rightmost compound (the subject) sits at
     the bottom of [right] and each combinator joins its [left] compound to the
     compound *after* it, not to the subject. [anchors sel n] returns the nodes
     where [sel]'s leftmost compound matches when [sel] matches with subject [n]
     ([Some []] = no match, [None] = no model): [right] must match [n], then for
     every node [a] that [right]'s leftmost compound anchored at, [left] must be
     [comb]-related to [a]. Threading the anchor this way is what the old
     subject-only [combinator] missed for any selector with two or more
     combinators. *)
  and anchors sel n =
    match sel with
    | Selector.Combined (left, comb, right) -> (
        (* [left] is asked for its own support, separately from whether [right]
           found an anchor: stopping at an empty anchor list would let the node
           decide whether the selector is supported. *)
        match (anchors right n, match_selector left n, relation comb) with
        | Some found, left_answer, Some related when left_answer <> Unsupported
          ->
            Some (List.concat_map (related left) found)
        | _ -> None)
    | _ -> (
        match match_selector sel n with
        | Unsupported -> None
        | No_match -> Some []
        | Matches -> Some [ n ])

  (* [None] for a combinator that is not a relation between two nodes of this
     tree: [||] picks a cell out of the column it belongs to, and the legacy
     [>>>] and [/deep/] cross a shadow boundary the tree does not have. *)
  and relation comb =
    let hits left x = match_selector left x = Matches in
    match comb with
    | Selector.Descendant ->
        Some (fun left a -> List.filter (hits left) (ancestors a))
    | Selector.Child ->
        Some
          (fun left a ->
            match N.parent a with Some p when hits left p -> [ p ] | _ -> [])
    | Selector.Next_sibling ->
        Some
          (fun left a ->
            match imm_pred a with Some s when hits left s -> [ s ] | _ -> [])
    | Selector.Subsequent_sibling ->
        Some (fun left a -> List.filter (hits left) (preceding_siblings a))
    | Selector.Column | Selector.Shadow_piercing | Selector.Shadow_deep -> None

  let matches sel n = match_selector sel n = Matches

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
                if Selector.compare_specificity s acc > 0 then s else acc)
              (Selector.specificity b) rest)
    | None -> Selector.specificity sel

  let resolve sheet node =
    let upsert acc d =
      let k = Declaration.property_name d in
      (k, d) :: List.remove_assoc k acc
    in
    let layer_order, rules = layered_rules (Flatten.block sheet) in
    let matched =
      rules
      |> List.mapi (fun i (layer, r) ->
          let sel = Stylesheet.selector r in
          if matches sel node then
            Some
              (layer, matched_specificity sel node, i, Stylesheet.declarations r)
          else None)
      |> List.filter_map Fun.id
    in
    (* The cascade sorting order (css-cascade-5 sec. 6) weighs the layer above
       specificity and above source order, and its direction depends on
       importance: among normal declarations the last layer wins and the
       unlayered ones beat them all, among important ones that reverses. So rank
       each importance bucket on its own. *)
    let bucket ~important =
      matched
      |> List.filter_map (fun (layer, spec, i, ds) ->
          match
            List.filter (fun d -> Declaration.is_important d = important) ds
          with
          | [] -> None
          | ds ->
              Some
                ( Stylesheet.cascade_layer_precedence_rank ~layer_order
                    ~important layer,
                  spec,
                  i,
                  ds ))
      |> List.stable_sort (fun (l1, s1, i1, _) (l2, s2, i2, _) ->
          let layer = Int.compare l1 l2 in
          if layer <> 0 then layer
          else
            let specificity = Selector.compare_specificity s1 s2 in
            if specificity <> 0 then specificity else Int.compare i1 i2)
      |> List.concat_map (fun (_, _, _, ds) -> ds)
    in
    (* normal first, then !important, last wins per property *)
    List.fold_left upsert [] (bucket ~important:false @ bucket ~important:true)
    |> List.rev_map snd
end

(* The capability side of the matcher, read off the matcher rather than restated
   beside it: a second list of the forms it models would be free to drift from
   the first, and every selector it then wrongly called supported would be one a
   caller inlines and drops. [Unsupported] never depends on the node, so any
   node settles the question - this one has nothing on it at all. *)
module Probe = struct
  type t = unit

  let equal () () = true
  let name () = None
  let id () = None
  let classes () = []
  let attribute () _ = None
  let parent () = None
  let children () = []
  let text_children () = []
end

module Probe_matcher = Make (Probe)

let supported sel = Probe_matcher.match_selector sel () <> Unsupported
