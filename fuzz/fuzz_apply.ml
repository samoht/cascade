(** Fuzz tests for projecting a stylesheet onto an element tree
    ([Cascade.Apply]). *)

open Cascade
open Alcobar

type node = { name : string; classes : string list; children : node list }

module Node = struct
  type t = node

  let equal = ( == )
  let name n = Some n.name
  let id _ = None
  let classes n = n.classes
  let attribute _ _ = None
  let parent _ = None
  let children n = n.children
  let text_children _ = []
end

module A = Apply.Make (Node)

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let color buf i = pick [ "#abc"; "#123"; "#f00"; "#00f"; "red"; "blue" ] buf i

let inline_style ds =
  Stylesheet.inline_style_of_declarations ~minify:true ~mode:Variables ds

(* Compare two projections structurally: the inline style written onto each node
   in tree order, plus the number of rules kept in the <style>. The kept body
   itself is left out because a kept rule stays inside its layer, so its text
   differs by the wrapper. *)
let summary (r : node Apply.result) =
  (List.map (fun (_, ds) -> inline_style ds) r.styles, r.kept)

let layer name body = String.concat "" [ "@layer "; name; "{"; body; "}" ]

(* A @layer block applies unconditionally, so wrapping author rules in one (or
   several) layers must not change what [Apply.compute] projects onto the tree,
   nor how many rules it keeps in the <style>. Each class carries a single
   declaration so no two rules compete, which is what makes the wrapping
   invisible: layers only decide between competing declarations. *)
let test_layer_wrapping_invariant buf =
  let k = 1 + (byte_at buf 0 mod 4) in
  let classes =
    List.init k (fun i -> String.concat "" [ "c"; string_of_int i ])
  in
  let roots =
    List.map (fun c -> { name = "div"; classes = [ c ]; children = [] }) classes
  in
  let static_rule i c =
    String.concat ""
      [ "."; c; "{color:var(--"; string_of_int i; ","; color buf (i + 1); ")}" ]
  in
  (* Give one class a :hover rule, so a property turns dynamic and its static
     declaration is kept in <style> rather than inlined - the split must survive
     layer wrapping too. *)
  let hover_at = byte_at buf 1 mod k in
  let hover_rule =
    String.concat "" [ ".c"; string_of_int hover_at; ":hover{color:red}" ]
  in
  let rules = List.mapi static_rule classes @ [ hover_rule ] in
  let reference = String.concat "" rules in
  let layered =
    if byte_at buf 2 land 1 = 0 then layer "utilities" reference
    else
      (* static rules in one layer, the stateful rule in another *)
      String.concat ""
        [
          layer "base" (String.concat "" (List.mapi static_rule classes));
          layer "states" hover_rule;
        ]
  in
  let r0 = summary (A.compute ~css:reference roots) in
  let r1 = summary (A.compute ~css:layered roots) in
  if r0 <> r1 then fail "layer wrapping changed the projected styles"

let suite =
  ( "apply",
    [
      test_case "layer wrapping leaves the projection unchanged" [ bytes ]
        test_layer_wrapping_invariant;
    ] )
