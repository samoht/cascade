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
   of it names [html], [head] or [body], and none of it is a tag the HTML parser
   closes or moves on its own ([p], [li], a table part), so the tree the
   document is written as is the tree every parser builds from it. *)

let tags = [ "div"; "span"; "section"; "article"; "nav" ]
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
  text : bool;
}

type doc = element list

let elt ?id ?(classes = []) ?(attrs = []) ?(text = true) tag kids =
  { tag; eid = id; cls = classes; attrs; kids; text }

let doc children = children

(* ===== The document as HTML ===== *)

let add_escaped buf s =
  String.iter
    (function
      | '&' -> Buffer.add_string buf "&amp;"
      | '<' -> Buffer.add_string buf "&lt;"
      | '>' -> Buffer.add_string buf "&gt;"
      | '"' -> Buffer.add_string buf "&quot;"
      | c -> Buffer.add_char buf c)
    s

let add_attr buf name value =
  Buffer.add_char buf ' ';
  Buffer.add_string buf name;
  Buffer.add_string buf "=\"";
  add_escaped buf value;
  Buffer.add_char buf '"'

(* An element holds a letter before its children, so a colour, a weight and an
   opacity paint on it and a margin or a padding moves something. *)
let rec add_element buf e =
  Buffer.add_char buf '<';
  Buffer.add_string buf e.tag;
  Option.iter (add_attr buf "id") e.eid;
  if e.cls <> [] then add_attr buf "class" (String.concat " " e.cls);
  List.iter (fun (k, v) -> add_attr buf k v) e.attrs;
  Buffer.add_char buf '>';
  if e.text then Buffer.add_char buf 'x';
  List.iter (add_element buf) e.kids;
  Buffer.add_string buf "</";
  Buffer.add_string buf e.tag;
  Buffer.add_char buf '>'

let html_of_doc d =
  let buf = Buffer.create 1024 in
  List.iter (add_element buf) d;
  Buffer.contents buf

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
  (* Not [:empty]: every generated element holds a letter, so it would match
     nothing, and the default reading declines it, which would keep every rule
     writing the same slots out of the projection with it. *)
  | 13 -> Selector.compound [ typ (); Selector.Nth_child (Selector.Even, None) ]
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
   every other and every one of them paints on an element holding a letter, so a
   declaration that wins where another should have shows up in the render. *)

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
