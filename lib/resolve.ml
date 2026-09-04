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
type reading = Browser | Spec

let of_bool b = if b then Matches else No_match

(* The forms selectors-4 defines and no engine implements as defined. Sec. 6.3
   gives the [s] flag "identical to" semantics; engines refuse the selector, and
   the rule carrying it with it. Sec. 13.2 represents an element "that has no
   children except, optionally, document white space characters", and its own
   note records that Level 2 and Level 3 did not - the change engines have not
   taken. Under [Browser] the matcher declines both: an answer would be a fact
   about the specification, and its callers rewrite pages a browser renders. The
   selector_match differential is what keeps this list in step, since a form
   cascade decides and the browser refuses shows up there. *)
let unimplemented =
  Selector.any (function
    | Selector.Empty | Selector.Attribute (_, _, _, Some Selector.Sensitive) ->
        true
    | _ -> false)

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

(* The An+B microsyntax (css-syntax-3 sec. 6), where [odd] is [2n+1] and [even]
   is [2n]. It "represents any index i = An + B for any non-negative integer n"
   (selectors-4 sec. 13.3.1), so an index hits when [i - B] is [A] times a
   non-negative integer: with [A] zero that leaves the single index [B], and
   with [A] negative it counts back down towards the start of the list. Indices
   run from 1 (sec. 13), so a B below it, as in [10n-1], simply drops the terms
   the list has no room for. *)
let an_plus_b : Selector.nth -> int * int = function
  | Selector.Odd -> (2, 1)
  | Selector.Even -> (2, 0)
  | Selector.Index b -> (0, b)
  | Selector.An_plus_b (a, b) -> (a, b)

let nth_hits nth i =
  let a, b = an_plus_b nth in
  let d = i - b in
  if a = 0 then d = 0 else d mod a = 0 && d / a >= 0

(* Cascade layers. Layer names form a tree: [@layer a.b] names the sublayer [b]
   of [a], and so does [@layer a { @layer b { ... } }]. The cascade only needs
   that tree flattened, so a layer is keyed by its path - the idents from the
   root down - and the order is a list of paths, weakest first: siblings by
   first declaration (css-cascade-5 sec. 6.4.2), each parent after the whole of
   its subtree (sec. 6.4.3). *)

let parent_layer path =
  match List.rev path with [] | [ _ ] -> None | _ :: up -> Some (List.rev up)

(* Each unnamed [@layer { }] block is a layer of its own, distinct from every
   other one, so key it by a counter behind a prefix no author can write: NUL
   never survives tokenisation, css-syntax-3 sec. 3.3 turns it into U+FFFD. *)
let anonymous_layer n = [ "\000" ^ string_of_int n ]
let is_anonymous_ident p = p <> "" && p.[0] = '\000'

(* A layer is keyed by the CSS text of its path: each ident with the escapes
   that read it back (css-syntax-3 sec. 2.1), joined by the [.] separators of
   css-cascade-5 sec. 6.4.1. A [.] an ident carries is escaped, so it never
   passes for a separator and the layer named [a.b] stays apart from the
   sublayer [a.b]. An anonymous layer's ident is left as it is: escaping its NUL
   would only hide the marker a caller looks for. *)
let layer_key path =
  let ident p = if is_anonymous_ident p then p else Parser.escape_ident p in
  String.concat "." (List.map ident path)

let qualify parent name = match parent with None -> name | Some p -> p @ name

(* A sublayer is ordered inside its parent, not at the end of the sheet: in
   [@layer a.b {} @layer c {} @layer a.d {}] the layer [a.d] joins [a]'s subtree
   and so still precedes [c]. A parent's own rules close that subtree
   (css-cascade-5 sec. 6.4.3: "Unlayered rules are sorted later than any layered
   rules within the same parent layer"), so [p]'s entry stands for them and sits
   last in its run: a new sublayer goes immediately before it, which is past
   every sublayer declared earlier. *)
let insert_layer order name =
  if List.exists (Stylesheet.equal_layer_name name) order then order
  else
    match parent_layer name with
    | None -> order @ [ name ]
    | Some p ->
        let rec insert = function
          | [] -> [ name ]
          | x :: rest when Stylesheet.equal_layer_name p x -> name :: x :: rest
          | x :: rest -> x :: insert rest
        in
        insert order

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
   contributing a rule.

   The statements it does not walk into are listed one by one rather than closed
   with a wildcard, since which ones those are is the contract of {!resolve} and
   {!layer_order} both. A conditional group rule ([@media], [@supports],
   [@container], [@-moz-document], [@when], [@else]) gates its contents on an
   environment nothing here models: there is no viewport, no UA feature table,
   no container layout and no document URL. [@starting-style] declares a
   before-change style (css-transitions-2 sec. 5) rather than an ordinary one.
   [@scope] adds a scoping root, a scoping limit and the proximity criterion
   (css-cascade-6), none of which the matcher models. [Origin] carries the
   origin that outranks the layer in the cascade sorting order (css-cascade-5
   sec. 6.1), which the ranking below does not weigh. Each is left to the
   browser instead: {!Apply.Make} keeps these blocks whole in the stylesheet it
   emits and inlines only what [resolve] returns, so a rule counted in both
   places would apply twice. *)
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
        | Stylesheet.Media _ | Stylesheet.Supports _ | Stylesheet.Container _
        | Stylesheet.Moz_document _ | Stylesheet.When _ | Stylesheet.Else _
        | Stylesheet.Starting_style _ | Stylesheet.Scope _ | Stylesheet.Origin _
        | Stylesheet.Declarations _ | Stylesheet.Bang_comment _
        | Stylesheet.Charset _ | Stylesheet.Import _ | Stylesheet.Namespace _
        | Stylesheet.Property _ | Stylesheet.Supports_condition _
        | Stylesheet.Keyframes _ | Stylesheet.Webkit_keyframes _
        | Stylesheet.Moz_keyframes _ | Stylesheet.Font_face _
        | Stylesheet.Counter_style _ | Stylesheet.Page _
        | Stylesheet.Page_with_margins _ | Stylesheet.Font_palette_values _
        | Stylesheet.Font_feature_values _ | Stylesheet.View_transition _
        | Stylesheet.Position_try _ | Stylesheet.Viewport _
        | Stylesheet.Unknown_at_rule _ ->
            (order, rules))
      acc stmts
  in
  let order, rules = go None ([], []) stmts in
  (order, List.rev rules)

let layer_order stmts = List.map layer_key (fst (layered_rules stmts))

(* Everything [resolve] can work out from the sheet alone: the flattened rule
   list with each rule's layer, and the layer order the cascade ranks by.
   Neither depends on the node, so a caller walking a document pays for them
   once rather than per element. *)
type prepared = {
  layer_order : string list;
  rules : (string list option * Stylesheet.rule) list;
}

let prepare sheet =
  let paths, rules = layered_rules (Flatten.block sheet) in
  { layer_order = List.map layer_key paths; rules }

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

  let following_siblings n =
    match N.parent n with
    | None -> []
    | Some p ->
        let rec after = function
          | [] -> []
          | x :: rest when N.equal x n -> rest
          | _ :: rest -> after rest
        in
        after (N.children p)

  let imm_pred n =
    match List.rev (preceding_siblings n) with x :: _ -> Some x | [] -> None

  let rec subtree n = n :: List.concat_map subtree (N.children n)
  let descendants n = List.concat_map subtree (N.children n)
  let is_first n = preceding_siblings n = []

  let is_last n =
    match N.parent n with
    | None -> true
    | Some p -> (
        match List.rev (N.children p) with x :: _ -> N.equal x n | [] -> true)

  let rec ancestors n =
    match N.parent n with None -> [] | Some p -> p :: ancestors p

  (* selectors-4 sec. 6.3: an [i] flag makes a UA "match the attribute's value
     ASCII case-insensitively", an [s] flag makes it match "case-sensitively,
     with 'identical to' semantics". With no flag the case-sensitivity "depends
     on the document language", which a {!NODE} does not carry, so the value is
     read as written - the answer [s] gives. Folding is ASCII-only, which is why
     the spec has [green] match [GREEN] but not the umlauted [grun] match its
     own upper case. *)
  let attr_fold : Selector.attr_flag option -> string -> string = function
    | Some Selector.Insensitive -> String.lowercase_ascii
    | None | Some Selector.Sensitive -> Fun.id

  let attr_matches ~fold n name (m : Selector.attribute_match) =
    match N.attribute n (attr_key name) with
    | None -> false
    | Some v -> (
        let v = fold v in
        match m with
        | Selector.Presence -> true
        | Selector.Exact s | Selector.Exact_quoted (s, _) -> v = fold s
        | Selector.Whitespace_list s | Selector.Whitespace_list_quoted (s, _) ->
            List.mem (fold s) (words v)
        | Selector.Prefix s | Selector.Prefix_quoted (s, _) ->
            s <> "" && starts v (fold s)
        | Selector.Suffix s | Selector.Suffix_quoted (s, _) ->
            s <> "" && ends v (fold s)
        | Selector.Substring s | Selector.Substring_quoted (s, _) ->
            s <> "" && contains v (fold s)
        | Selector.Hyphen_list s | Selector.Hyphen_list_quoted (s, _) ->
            let s = fold s in
            v = s || starts v (String.concat "" [ s; "-" ]))

  (* selectors-4 sec. 13.3: the child-indexed pseudo-classes read an element's
     "relative index amongst its siblings", counting the inclusive ones, since
     there was "no reason to exclude them from matching elements without
     parents". A parentless element is a list of one, not no list at all. *)
  let inclusive_siblings n =
    match N.parent n with None -> [ n ] | Some p -> N.children p

  (* [nth_position ~from_end n keep] is [n]'s 1-based index among the inclusive
     siblings [keep] admits, or [None] when [n] is not one of them and so is
     among the An+Bth of nothing. Only elements are listed: a {!NODE} keeps text
     out of [children], which is what sec. 13 asks for. *)
  let nth_position ~from_end n keep =
    let sibs = List.filter keep (inclusive_siblings n) in
    let sibs = if from_end then List.rev sibs else sibs in
    let rec index i = function
      | [] -> None
      | x :: _ when N.equal x n -> Some i
      | _ :: rest -> index (i + 1) rest
    in
    index 1 sibs

  let nth_matches ~from_end nth keep n =
    match nth_position ~from_end n keep with
    | None -> false
    | Some i -> nth_hits nth i

  (* selectors-4 sec. 13.4: the typed child-indexed pseudo-classes resolve on an
     element's index "among elements of the same type (tag name) in their
     sibling list", the type being the one a type selector matching the element
     would name. So the comparison is the type selector's own, which is why
     [:nth-of-type(2)] and [:nth-child(2 of <type>)] agree as sec. 13.4.1
     requires. A node {!NODE} gives no name has no tag name to share with a
     named one. *)
  let same_type n x =
    match (N.name n, N.name x) with
    | Some a, Some b -> ci a b
    | None, None -> true
    | Some _, None | None, Some _ -> false

  (* sec. 13.4.3 makes [:first-of-type] the same element as [:nth-of-type(1)]
     and sec. 13.4.4 makes [:last-of-type] the same as [:nth-last-of-type(1)];
     sec. 13.4.5 makes [:only-of-type] the two at once. *)
  let first_of_type n =
    nth_matches ~from_end:false (Selector.Index 1) (same_type n) n

  let last_of_type n =
    nth_matches ~from_end:true (Selector.Index 1) (same_type n) n

  (* Every combinator reaches down the tree or to the right, so the subject of a
     relative selector absolutised against [n] never lies before [n]: a [:has()]
     over a descendant or child combinator looks no further than [n]'s subtree,
     and one over a sibling combinator no further than the subtrees of [n]'s
     following siblings. The three combinators {!relation} has no model for
     never reach here, since it is asked first. *)
  let has_subjects comb n =
    match comb with
    | Selector.Descendant | Selector.Child -> descendants n
    | Selector.Next_sibling | Selector.Subsequent_sibling ->
        List.concat_map subtree (following_siblings n)
    | Selector.Column | Selector.Shadow_piercing | Selector.Shadow_deep -> []

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
    (* selectors-4 sec. 16: an [<attr-modifier>] follows a matcher and a value,
       so a presence test carrying one is not an attribute selector. *)
    | Selector.Attribute (None, _, Selector.Presence, Some _) -> Unsupported
    | Selector.Attribute (None, name, m, flag) ->
        of_bool (attr_matches ~fold:(attr_fold flag) n name m)
    | Selector.Compound ps ->
        List.fold_left (fun acc p -> conj acc (match_selector p n)) Matches ps
    | Selector.List ss | Selector.Is ss | Selector.Where ss -> any ss n
    | Selector.Not ss -> negate (any ss n)
    | Selector.Combined _ -> (
        match anchors sel n with
        | None -> Unsupported
        | Some [] -> No_match
        | Some (_ :: _) -> Matches)
    (* selectors-4 sec. 8.4: [:scope] is the scoping root, and "if there is no
       scoping root then :scope represents the root of the tree the element is
       in", which is [:root] outside a shadow tree. Nothing hands this matcher a
       scoping root and a {!NODE} tree has no shadow boundary, so the two ask
       the same question. *)
    | Selector.Root | Selector.Scope -> of_bool (N.parent n = None)
    | Selector.First_child -> of_bool (is_first n)
    | Selector.Last_child -> of_bool (is_last n)
    | Selector.Only_child -> of_bool (is_first n && is_last n)
    | Selector.Empty -> of_bool (is_empty n)
    | Selector.Nth_child (nth, of_) -> nth_child ~from_end:false nth of_ n
    | Selector.Nth_last_child (nth, of_) -> nth_child ~from_end:true nth of_ n
    | Selector.Nth_of_type (nth, None) ->
        of_bool (nth_matches ~from_end:false nth (same_type n) n)
    | Selector.Nth_last_of_type (nth, None) ->
        of_bool (nth_matches ~from_end:true nth (same_type n) n)
    | Selector.First_of_type -> of_bool (first_of_type n)
    | Selector.Last_of_type -> of_bool (last_of_type n)
    | Selector.Only_of_type -> of_bool (first_of_type n && last_of_type n)
    | Selector.Has rs ->
        List.fold_left (fun acc r -> disj acc (has_relative n r)) No_match rs
    (* sec. 13.4.1 and sec. 13.4.2 give the typed forms an [An+B] and nothing
       else, so an [of S] on one is not a selector any UA takes. *)
    | Selector.Nth_of_type (_, Some _) | Selector.Nth_last_of_type (_, Some _)
      ->
        Unsupported
    (* A namespace, which no node here carries; [:lang()], whose content
       language the document language defines rather than the tree; and every
       stateful, generated or tree-external form. *)
    | _ -> Unsupported

  and any ss n =
    List.fold_left (fun acc s -> disj acc (match_selector s n)) No_match ss

  (* selectors-4 sec. 13.3.1: with [of S] the element must be among the An+Bth
     of "their inclusive siblings that match the selector list S", so [S] both
     filters the list and decides whether [n] is in it at all; without it, [S]
     defaults to [*|*] and the list is every sibling. *)
  and nth_child ~from_end nth of_ n =
    match of_ with
    | None -> of_bool (nth_matches ~from_end nth (fun _ -> true) n)
    | Some s -> (
        (* Whether [S] is modelled is a fact about [S] alone, so any node
           settles it, and asking [n] keeps that answer clear of which siblings
           happen to be there. *)
        match any s n with
        | Unsupported -> Unsupported
        | Matches | No_match ->
            of_bool (nth_matches ~from_end nth (fun x -> any s x = Matches) n))

  (* selectors-4 sec. 4.5: [:has()] "represents an element if any of the
     relative selectors would match at least one element when anchored against
     this element". A relative selector is a combinator and a complex selector
     with "a selector representing the anchor element implied at the start", the
     descendant combinator standing in when none is written (sec. 3.4). So
     absolutise it against [n] and ask whether anything in reach is its subject
     with [n] as the anchor. [complex] is matched against [n] itself only for
     the support it reports, which no node can change; stopping at an empty
     subject list would let the tree decide whether the selector is modelled. *)
  and has_relative n r =
    let comb, complex =
      match r with
      | Selector.Relative (comb, complex) -> (comb, complex)
      | complex -> (Selector.Descendant, complex)
    in
    match (relation comb, match_selector complex n) with
    | Some _, complex_answer when complex_answer <> Unsupported ->
        let absolute =
          Selector.Combined (Selector.Universal None, comb, complex)
        in
        of_bool
          (List.exists
             (fun e ->
               match anchors absolute e with
               | None -> false
               | Some found -> List.exists (N.equal n) found)
             (has_subjects comb n))
    | _ -> Unsupported

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

  (* [Spec] is the matcher above, which reads selectors-4 as written. [Browser]
     declines the forms no engine implements, so a caller that inlines or
     deletes never acts on an answer the page's own browser does not give. *)
  let match_selector ?(reading = Browser) sel n =
    match reading with
    | Browser when unimplemented sel -> Unsupported
    | Browser | Spec -> match_selector sel n

  let matches ?reading sel n = match_selector ?reading sel n = Matches

  (* A selector list has no single specificity: a rule [s1, s2 {...}] cascades
     as if duplicated per branch, so the specificity that applies to [node] is
     that of the highest-specificity branch matching [node], not the whole
     list. *)
  let matched_specificity ~reading sel node =
    match Selector.as_list sel with
    | Some branches -> (
        match List.filter (fun b -> matches ~reading b node) branches with
        | [] -> Selector.specificity sel
        | b :: rest ->
            List.fold_left
              (fun acc b ->
                let s = Selector.specificity b in
                if Selector.compare_specificity s acc > 0 then s else acc)
              (Selector.specificity b) rest)
    | None -> Selector.specificity sel

  let resolve_prepared ?(reading = Browser) { layer_order; rules } node =
    let upsert acc d =
      let k = Declaration.property_name d in
      (k, d) :: List.remove_assoc k acc
    in
    let matched =
      rules
      |> List.mapi (fun i (layer, r) ->
          let sel = Stylesheet.selector r in
          if matches ~reading sel node then
            Some
              ( Option.map layer_key layer,
                matched_specificity ~reading sel node,
                i,
                Stylesheet.declarations r )
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

  let resolve ?reading sheet node =
    resolve_prepared ?reading (prepare sheet) node
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

let supported ?reading sel =
  Probe_matcher.match_selector ?reading sel () <> Unsupported
