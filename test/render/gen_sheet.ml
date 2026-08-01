open Cascade

(* A linear congruential generator: the sweep needs the same sheet for the same
   seed on every machine, which Random does not promise across versions. *)
type rng = { mutable state : int }

let rng seed = { state = seed * 2654435761 land max_int }

let next r =
  r.state <- ((r.state * 2862933555777941757) + 3037000493) land max_int;
  r.state

let int r n = if n <= 1 then 0 else next r mod n
let pick r l = List.nth l (int r (List.length l))
let chance r n = int r n = 0

(* ===== Selectors ===== *)

let simple =
  [
    Selector.class_ "a";
    Selector.class_ "b";
    Selector.class_ "card";
    Selector.element "div";
    Selector.element "p";
    Selector.element "li";
    Selector.element "span";
    Selector.id "lead";
    Selector.compound [ Selector.element "li"; Selector.First_child ];
    Selector.compound [ Selector.element "li"; Selector.Last_child ];
    Selector.compound
      [ Selector.class_ "a"; Selector.Nth_child (Selector.Index 2, None) ];
    Selector.attribute "data-k" (Selector.Exact "v");
    Selector.compound
      [ Selector.element "p"; Selector.Not [ Selector.class_ "b" ] ];
  ]

let combinators =
  [
    Selector.Descendant;
    Selector.Child;
    Selector.Next_sibling;
    Selector.Subsequent_sibling;
  ]

let selector r =
  let base = pick r simple in
  if chance r 3 then Selector.combine (pick r simple) (pick r combinators) base
  else base

(* ===== Values ===== *)

let px r = Css.Values.Px (float_of_int (int r 20))
let colors = [ "#f00"; "#0f0"; "#00f"; "#123456"; "#abc" ]
let color r = Css.Values.hex (pick r colors)

(* ===== Declarations =====

   Each family writes cascade slots its partner also writes, so the two
   declarations render differently when swapped: this is the shape the canonical
   declaration order has to leave alone. *)
let overflows : Css.overflow list = [ Hidden; Visible; Auto; Scroll ]
let displays : Css.display list = [ Block; Flex; Inline_block; Grid ]

let families =
  [
    (fun r -> [ Css.margin [ px r ]; Css.margin_top (px r) ]);
    (fun r -> [ Css.padding [ px r; px r ]; Css.padding_left (px r) ]);
    (fun r -> [ Css.gap (Css.gaps (px r)); Css.row_gap (px r) ]);
    (fun r ->
      [ Css.overflow (pick r overflows); Css.overflow_x (pick r overflows) ]);
  ]

let singles =
  [
    (fun r -> Css.color (color r));
    (fun r -> Css.background_color (color r));
    (fun r -> Css.width (px r));
    (fun r -> Css.opacity (Css.Opacity_number (float_of_int (int r 10) /. 10.)));
    (fun r -> Css.display (pick r displays));
  ]

let declarations r =
  let family = if chance r 2 then (pick r families) r else [] in
  (* Either order, so the sweep sees the longhand before and after the shorthand
     that resets it. *)
  let family = if chance r 2 then List.rev family else family in
  let rest = List.init (1 + int r 3) (fun _ -> (pick r singles) r) in
  let all = family @ rest in
  if chance r 6 then
    match all with d :: tl -> Css.important d :: tl | [] -> all
  else all

(* ===== Statements ===== *)

let media_min_width w : Css.Media.t =
  Css.Media.Cond
    (Css.Media.Feature
       (Css.Media.Plain
          (Css.Media.Min Css.Media.Width, Css.Media.Length (Css.Values.Px w))))

let statement r =
  let rule = Css.rule ~selector:(selector r) (declarations r) in
  if chance r 5 then
    Css.media
      ~condition:(media_min_width (float_of_int (pick r [ 400; 800 ])))
      [ rule ]
  else rule

let stylesheet ~seed =
  let r = rng seed in
  let n = 3 + int r 6 in
  let statements = List.init n (fun _ -> statement r) in
  (* A repeated selector gives the optimizer rules to merge, and a merge that
     crosses a conflicting write is the other way it can change a render. *)
  let repeat =
    if chance r 2 then
      match statements with
      | first :: _ -> (
          match Css.as_rule first with
          | Some (sel, _, _) -> [ Css.rule ~selector:sel (declarations r) ]
          | None -> [])
      | [] -> []
    else []
  in
  Css.v (statements @ repeat)
