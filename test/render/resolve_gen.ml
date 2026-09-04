open Cascade

(* A linear congruential generator: a finding has to reproduce from its seed on
   every machine, which Random does not promise across versions. *)
type rng = { mutable state : int }

let rng seed = { state = seed * 2654435761 land max_int }

let next r =
  r.state <- ((r.state * 2862933555777941757) + 3037000493) land max_int;
  r.state

let int r n = if n <= 1 then 0 else next r mod n
let pick r l = List.nth l (int r (List.length l))
let chance r n = int r n = 0

(* ===== Vocabulary =====

   One fixed vocabulary for the document and the selectors, so a generated rule
   has elements to match: random markup and random selectors meet nowhere. None
   of it names [html], [head] or [body]. *)

let tags = [ "div"; "p"; "span"; "section"; "li" ]
let classes = [ "a"; "b"; "card" ]
let ids = [ "lead"; "foot" ]
let attr_values = [ "v"; "w" ]

(* ===== Documents ===== *)

type element = {
  tag : string;
  eid : string option;
  cls : string list;
  attrs : (string * string) list;
  kids : element list;
  mutable up : element option;
}

type doc = { root : element; subjects : element list }

let elt ?id ?(classes = []) ?(attrs = []) tag kids =
  { tag; eid = id; cls = classes; attrs; kids; up = None }

let rec tie parent e =
  e.up <- parent;
  List.iter (tie (Some e)) e.kids

(* Document order, [body]'s descendants only: the driver enumerates the same set
   with body.querySelectorAll('*'), and the two lists have to line up. *)
let rec preorder e = e :: List.concat_map preorder e.kids

let doc children =
  let body = elt "body" children in
  let root = elt "html" [ elt "head" []; body ] in
  tie None root;
  { root; subjects = List.concat_map preorder children }

let subjects d = d.subjects
let tag e = e.tag

module Node = struct
  type t = element

  (* Identity, not structure: two sibling [<div>] with the same classes are
     different elements and a matcher locating one among its siblings has to
     tell them apart. Every element carries a mutable field, so no two are
     shared. *)
  let equal (a : t) (b : t) = a == b
  let name e = Some e.tag
  let id e = e.eid
  let classes e = e.cls

  (* [id] and [class] are attributes as much as any other, and a document that
     reports them only through the accessors named after them makes [\[id\]] and
     [\[class~="a"\]] match nothing. *)
  let attribute e n =
    match (n, e.cls) with
    | "id", _ -> e.eid
    | "class", [] -> None
    | "class", cls -> Some (String.concat " " cls)
    | _ -> List.assoc_opt n e.attrs

  let parent e = e.up
  let children e = e.kids
  let text_children _ = []
end

(* [body] and [head] are the document's own scaffolding, so a path starts at
   [body]'s children: the elements a sheet is resolved against. *)
let path d e =
  let rec up e acc =
    match e.up with
    | None -> acc
    | Some p when Node.equal p d.root -> acc
    | Some p ->
        let rec index i = function
          | [] -> i
          | x :: _ when Node.equal x e -> i
          | _ :: rest -> index (i + 1) rest
        in
        up p
          (String.concat ""
             [ e.tag; ":nth-child("; string_of_int (index 1 p.kids); ")" ]
          :: acc)
  in
  String.concat ">" (up e [])

let label d e =
  let buf = Buffer.create 32 in
  Buffer.add_string buf (path d e);
  (match e.eid with
  | None -> ()
  | Some i ->
      Buffer.add_char buf '#';
      Buffer.add_string buf i);
  List.iter
    (fun c ->
      Buffer.add_char buf '.';
      Buffer.add_string buf c)
    e.cls;
  Buffer.contents buf

let rec json_of_element e =
  Json.Obj
    ([ ("t", Json.Str e.tag) ]
    @ (match e.eid with None -> [] | Some i -> [ ("i", Json.Str i) ])
    @ [
        ("c", Json.Arr (List.map (fun c -> Json.Str c) e.cls));
        ( "a",
          Json.Arr
            (List.map
               (fun (k, v) -> Json.Arr [ Json.Str k; Json.Str v ])
               e.attrs) );
        ("k", Json.Arr (List.map json_of_element e.kids));
      ])

let json_of_doc d =
  let body =
    match d.root.kids with _ :: body :: _ -> body.kids | _ -> d.subjects
  in
  Json.Obj
    [ ("dom", Json.Obj [ ("k", Json.Arr (List.map json_of_element body)) ]) ]

let rec take n l =
  match (n, l) with 0, _ | _, [] -> [] | n, x :: tl -> x :: take (n - 1) tl

let some r n l =
  let shuffled =
    List.map (fun x -> (int r 1000, x)) l
    |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
    |> List.map snd
  in
  take n shuffled

let element r ~depth ~free_ids =
  let rec go depth =
    let cls = some r (int r 3) classes in
    let id =
      if chance r 5 then (
        match !free_ids with
        | [] -> None
        | i :: rest ->
            free_ids := rest;
            Some i)
      else None
    in
    let attrs = if chance r 3 then [ ("data-k", pick r attr_values) ] else [] in
    let kids =
      if depth <= 0 then [] else List.init (int r 3) (fun _ -> go (depth - 1))
    in
    elt ?id ~classes:cls ~attrs (pick r tags) kids
  in
  go depth

let document ~seed =
  let r = rng ((seed * 7919) + 13) in
  let free_ids = ref ids in
  let n = 3 + int r 3 in
  doc (List.init n (fun _ -> element r ~depth:3 ~free_ids))

(* ===== Selectors =====

   Every form the cascade weighs differently: a class, an id and a type for the
   three specificity components, [:where()] for a part that adds none, [:is()]
   and [:not()] for a part that adds its argument's, and the combinators, whose
   left-hand compounds count too. *)

let simple r =
  let cls () = Selector.class_ (pick r classes) in
  let typ () = Selector.element (pick r tags) in
  match int r 18 with
  | 0 -> cls ()
  | 1 -> typ ()
  | 2 -> Selector.id (pick r ids)
  | 3 -> Selector.attribute "data-k" (Selector.Exact (pick r attr_values))
  | 4 -> Selector.compound [ typ (); cls () ]
  | 5 -> Selector.compound [ cls (); Selector.First_child ]
  | 6 ->
      Selector.compound [ typ (); Selector.Nth_child (Selector.Index 2, None) ]
  | 7 -> Selector.compound [ typ (); Selector.not [ cls () ] ]
  | 8 -> Selector.compound [ cls (); Selector.where [ cls (); typ () ] ]
  | 9 ->
      Selector.compound
        [ typ (); Selector.Is [ cls (); Selector.id (pick r ids) ] ]
  | 10 -> Selector.where [ typ () ]
  | 11 -> Selector.compound [ typ (); Selector.Last_child ]
  | 12 ->
      Selector.compound
        [ cls (); Selector.has [ Selector.Relative (Selector.Child, typ ()) ] ]
  | 13 -> Selector.compound [ typ (); Selector.Empty ]
  | 14 ->
      Selector.compound
        [ cls (); Selector.Nth_last_child (Selector.Index 1, None) ]
  (* The [i] flag has to match a value the unflagged form misses, so the value
     is spelled in the case the document does not carry. *)
  | 15 ->
      Selector.attribute ~flag:Selector.Insensitive "data-k"
        (Selector.Exact (String.uppercase_ascii (pick r attr_values)))
  | 16 -> Selector.compound [ typ (); Selector.Only_child ]
  | _ ->
      Selector.compound
        [ cls (); Selector.Nth_child (Selector.Odd, Some [ cls () ]) ]

let combinators =
  [
    Selector.Descendant;
    Selector.Child;
    Selector.Next_sibling;
    Selector.Subsequent_sibling;
  ]

let selector r =
  match int r 6 with
  | 0 | 1 -> Selector.combine (simple r) (pick r combinators) (simple r)
  | 2 -> Selector.list [ simple r; simple r ]
  | _ -> simple r

(* ===== Declarations =====

   A sheet writes a handful of slots over and over: what makes the cascade
   decide is several rules reaching for one slot on one element, so breadth of
   properties would only dilute the run. Every value of a slot differs from
   every other, so a declaration that wins where another should have shows up in
   the computed style. *)

type slot = Color | Background | Opacity | Weight | Margin | Padding

let slots = [ Color; Background; Opacity; Weight; Margin; Padding ]
let palette = [ "#f00"; "#0f0"; "#00f"; "#123456"; "#ff0" ]
let px r = Css.Values.Px (float_of_int (4 + (2 * int r 10)))

let declaration r = function
  | Color -> Css.color (Css.Values.hex (pick r palette))
  | Background -> Css.background_color (Css.Values.hex (pick r palette))
  | Opacity ->
      Css.opacity (Css.Opacity_number (float_of_int (1 + int r 9) /. 10.))
  | Weight ->
      Css.font_weight
        (Css.Weight (float_of_int (pick r [ 100; 400; 700; 900 ])))
  (* A shorthand and a longhand of one family: whichever the cascade ranks last
     resets the slot the other wrote, so their order in the winning set is as
     much a cascade answer as the winner itself. *)
  | Margin -> if chance r 3 then Css.margin [ px r ] else Css.margin_top (px r)
  | Padding ->
      if chance r 3 then Css.padding [ px r; px r ] else Css.padding_left (px r)

let declarations r contended =
  let n = 1 + int r 3 in
  List.init n (fun _ ->
      let d = declaration r (pick r contended) in
      if chance r 4 then Css.important d else d)

(* ===== Statements ===== *)

let layer_names = [ [ "base" ]; [ "theme" ]; [ "theme"; "dark" ]; [ "util" ] ]

let rule r contended =
  Css.rule ~selector:(selector r) (declarations r contended)

(* CSS Nesting 1 sec. 2: a nested rule's selector is relative to the parent, and
   one written without [&] carries an implied descendant combinator. Both forms
   are generated: the flattening the cascade runs first has to compose either
   into the same selector the browser reads directly. *)
let nested_selector r =
  match int r 3 with
  | 0 -> Selector.combine Selector.Nesting Selector.Descendant (simple r)
  | 1 ->
      Selector.compound [ Selector.Nesting; Selector.class_ (pick r classes) ]
  | _ -> simple r

let nested_rule r contended =
  Css.rule ~selector:(selector r)
    ~nested:
      [ Css.rule ~selector:(nested_selector r) (declarations r contended) ]
    (declarations r contended)

let statement r contended =
  match int r 12 with
  | 0 | 1 | 2 | 3 | 4 -> rule r contended
  | 5 | 6 ->
      Css.layer ~name:(pick r layer_names)
        (List.init
           (1 + int r 2)
           (fun _ ->
             if chance r 3 then nested_rule r contended else rule r contended))
  | 7 -> Css.layer (List.init (1 + int r 2) (fun _ -> rule r contended))
  | 8 | 9 -> nested_rule r contended
  | 10 -> Css.layer_decl (some r 2 layer_names)
  | _ ->
      Css.layer ~name:[ "base" ]
        [ Css.layer ~name:[ "inner" ] [ rule r contended ]; rule r contended ]

let stylesheet ~seed =
  let r = rng ((seed * 104729) + 7) in
  let contended = some r (3 + int r 2) slots in
  let n = 4 + int r 6 in
  let statements = List.init n (fun _ -> statement r contended) in
  (* A layer statement up front orders layers before any block declares them,
     which is the order the cascade has to rank by. *)
  let head =
    if chance r 2 then [ Css.layer_decl [ [ "util" ]; [ "base" ] ] ] else []
  in
  Css.v (head @ statements)
