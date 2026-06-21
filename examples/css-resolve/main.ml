(* Project a global stylesheet onto each node of a foreign (non-DOM) tree:
   selector match + specificity cascade -> the node's resolved declarations.
   This is the browser's style-resolution stage, renderer-agnostic. *)

module Sel = Cascade.Selector
module Sheet = Cascade.Stylesheet
module Decl = Cascade.Declaration
module Css = Cascade.Css

(* ---- a tiny tree that is NOT a DOM ---- *)
type elt = {
  name : string;
  id : string option;
  classes : string list;
  attrs : (string * string) list;
  kids : elt list;
}

let e ?id ?(cls = []) ?(attrs = []) ?(kids = []) name =
  { name; id; classes = cls; attrs; kids }

(* zipper: element + parent + index among siblings *)
type loc = { self : elt; up : loc option; idx : int; sibs : elt list }

let root e = { self = e; up = None; idx = 0; sibs = [ e ] }

let kids_of l =
  List.mapi
    (fun i k -> { self = k; up = Some l; idx = i; sibs = l.self.kids })
    l.self.kids

let rec ancestors l = match l.up with None -> [] | Some p -> p :: ancestors p

let sib_loc l j =
  { self = List.nth l.sibs j; up = l.up; idx = j; sibs = l.sibs }

let preceding l = List.init l.idx (fun j -> sib_loc l j)
let imm_pred l = if l.idx > 0 then Some (sib_loc l (l.idx - 1)) else None
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

let attr_key : Sel.attr_name -> string = function
  | Sel.Regular s -> s
  | Sel.Data s -> "data-" ^ s
  | Sel.Aria a -> Cascade.Aria.to_string a

let attr_matches self name (m : Sel.attribute_match) =
  match List.assoc_opt (attr_key name) self.attrs with
  | None -> false
  | Some v -> (
      match m with
      | Sel.Presence -> true
      | Sel.Exact s | Sel.Exact_quoted (s, _) -> v = s
      | Sel.Whitespace_list s | Sel.Whitespace_list_quoted (s, _) ->
          List.mem s (words v)
      | Sel.Prefix s | Sel.Prefix_quoted (s, _) -> s <> "" && starts v s
      | Sel.Suffix s | Sel.Suffix_quoted (s, _) -> s <> "" && ends v s
      | Sel.Substring s | Sel.Substring_quoted (s, _) -> s <> "" && contains v s
      | Sel.Hyphen_list s | Sel.Hyphen_list_quoted (s, _) ->
          v = s || starts v (s ^ "-"))

let rec matches (sel : Sel.t) l : bool =
  match sel with
  | Sel.Universal _ -> true
  | Sel.Element (_, n) -> ci l.self.name n
  | Sel.Class c -> List.mem c l.self.classes
  | Sel.Id i -> l.self.id = Some i
  | Sel.Attribute (_, name, m, _) -> attr_matches l.self name m
  | Sel.Compound ps -> List.for_all (fun p -> matches p l) ps
  | Sel.List ss | Sel.Is ss | Sel.Where ss ->
      List.exists (fun s -> matches s l) ss
  | Sel.Not ss -> not (List.exists (fun s -> matches s l) ss)
  | Sel.Combined (left, comb, right) ->
      matches right l && combinator left comb l
  | Sel.Root -> l.up = None
  | Sel.First_child -> l.idx = 0
  | Sel.Last_child -> l.idx = List.length l.sibs - 1
  | Sel.Only_child -> List.length l.sibs = 1
  | Sel.Empty -> l.self.kids = []
  (* dynamic pseudo-classes, pseudo-elements, etc.: not statically matchable *)
  | _ -> false

and combinator left comb l =
  match comb with
  | Sel.Descendant -> List.exists (matches left) (ancestors l)
  | Sel.Child -> ( match l.up with Some p -> matches left p | None -> false)
  | Sel.Next_sibling -> (
      match imm_pred l with Some s -> matches left s | None -> false)
  | Sel.Subsequent_sibling -> List.exists (matches left) (preceding l)
  | _ -> false

let spec_key s = Sel.(s.ids, s.classes, s.elements)

(* project: the cascade. gather matching rules, order by (specificity, source),
   last wins per property. *)
let project sheet l : Decl.declaration list =
  let matched =
    Sheet.rules sheet
    |> List.mapi (fun i r ->
        let sel = Sheet.selector r in
        if matches sel l then
          Some (spec_key (Sel.specificity sel), i, Sheet.declarations r)
        else None)
    |> List.filter_map Fun.id
  in
  let sorted =
    List.stable_sort
      (fun (s1, i1, _) (s2, i2, _) ->
        match compare s1 s2 with 0 -> compare i1 i2 | c -> c)
      matched
  in
  let upsert acc d =
    let k = Decl.property_name d in
    (k, d) :: List.remove_assoc k acc
  in
  List.fold_left (fun acc (_, _, ds) -> List.fold_left upsert acc ds) [] sorted
  |> List.rev_map snd

let style_of sheet l =
  Sheet.inline_style_of_declarations ~minify:false (project sheet l)

let () =
  let css =
    {|
      section          { color: black; padding: 4px }
      .card            { color: navy; border: 1px solid gray }
      #hero            { color: crimson }
      .card > p        { font-weight: bold }
      .card p          { margin: 2px }
      a[href^="https"] { text-decoration: underline }
      ul > li + li     { color: teal }
    |}
  in
  let sheet =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> failwith "parse failed"
  in
  let tree =
    e "section" ~id:"hero" ~cls:[ "card" ]
      ~kids:
        [
          e "p" ~kids:[ e "a" ~attrs:[ ("href", "https://x") ] ];
          e "ul" ~kids:[ e "li"; e "li" ];
        ]
  in
  let label l =
    let id = match l.self.id with Some i -> "#" ^ i | None -> "" in
    let cls = String.concat "" (List.map (fun c -> "." ^ c) l.self.classes) in
    l.self.name ^ id ^ cls
  in
  let rec walk pad l =
    Printf.printf "%s%-18s -> %s\n" pad (label l) (style_of sheet l);
    List.iter (walk (pad ^ "  ")) (kids_of l)
  in
  print_endline "project global CSS onto each node (no DOM):";
  walk "  " (root tree)
