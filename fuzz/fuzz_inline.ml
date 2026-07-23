(** Fuzz tests for closed-world var() inlining ([Css.inline_vars]). *)

open Cascade
open Alcobar

let byte_at buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte_at buf i mod List.length xs)
let color buf i = pick [ "#abc"; "#123"; "#f00"; "#00f"; "red"; "blue" ] buf i

let inlined_min css =
  match Css.of_string_exn ~strict:false css with
  | sheet -> Some (Css.to_string ~minify:true (Css.inline_vars sheet))
  | exception Error.Parse_error _ -> None

let layer name body = String.concat "" [ "@layer "; name; "{"; body; "}" ]

(* A cascade layer only orders competing declarations; it never scopes
   custom-property visibility (CSS Cascade 5 section 6.4, CSS Custom Properties
   1). So a single-definition variable inlines to the same result no matter
   which layer - or none - holds its definition or its consumers. Build a set of
   single-def [:root] variables and one consumer each, place them across layers
   several ways, and assert every placement inlines identically to the fully
   unlayered stylesheet (which never mishandled layers). *)
let test_layer_placement_invariant buf =
  let n = 1 + (byte_at buf 0 mod 4) in
  let root_block =
    let body =
      List.init n (fun i ->
          String.concat "" [ "--v"; string_of_int i; ":"; color buf (i + 1) ])
      |> String.concat ";"
    in
    String.concat "" [ ":root{"; body; "}" ]
  in
  let consumer i =
    let s = string_of_int i in
    String.concat "" [ ".c"; s; "{color:var(--v"; s; ")}" ]
  in
  let consumers = List.init n consumer in
  let reference = String.concat "" (root_block :: consumers) in
  let layered =
    match byte_at buf 1 mod 4 with
    | 0 ->
        (* definition and consumers in two different layers *)
        String.concat ""
          [
            layer "theme" root_block;
            layer "utilities" (String.concat "" consumers);
          ]
    | 1 ->
        (* definition layered, consumers unlayered *)
        String.concat "" (layer "theme" root_block :: consumers)
    | 2 ->
        (* definition unlayered, consumers layered *)
        String.concat ""
          [ root_block; layer "utilities" (String.concat "" consumers) ]
    | _ ->
        (* definition layered, each consumer in an alternating layer *)
        String.concat ""
          (layer "theme" root_block
          :: List.mapi
               (fun i c -> layer (if i land 1 = 0 then "a" else "b") c)
               consumers)
  in
  match (inlined_min reference, inlined_min layered) with
  | Some r, Some l ->
      if l <> r then
        failf "layer placement changed inlining: unlayered %S vs layered %S" r l
  | _ -> ()

(* CSS Cascade 5 §6.4.2: for normal declarations an unlayered definition beats
   every layered one, whatever the layers or their order. A variable defined on
   [:root] across a stack of named layers plus one unlayered definition must
   therefore fold to the unlayered value. Independent of how the winner is
   computed, so it catches a wrong winner (e.g. picking the last document
   order). *)
let test_unlayered_definition_wins buf =
  let n = 1 + (byte_at buf 0 mod 3) in
  let vals = [ "1px"; "2px"; "3px"; "4px" ] in
  let layered =
    List.init n (fun i ->
        let v = List.nth vals (byte_at buf (i + 1) mod List.length vals) in
        layer
          (String.concat "" [ "l"; string_of_int i ])
          (String.concat "" [ ":root{--x:"; v; "}" ]))
  in
  let sentinel = "9px" in
  let unlayered = String.concat "" [ ":root{--x:"; sentinel; "}" ] in
  let consumer = ".z{width:var(--x)}" in
  (* Place the unlayered definition before or after the layer stack: it wins
     from either position. *)
  let parts =
    if byte_at buf 9 land 1 = 0 then (unlayered :: layered) @ [ consumer ]
    else layered @ [ unlayered; consumer ]
  in
  match inlined_min (String.concat "" parts) with
  | None -> ()
  | Some out ->
      let expected = String.concat "" [ ".z{width:"; sentinel; "}" ] in
      if out <> expected then
        failf "unlayered definition did not win: %S (expected %S)" out expected

let suite =
  ( "inline",
    [
      test_case "layer placement leaves single-def inlining unchanged" [ bytes ]
        test_layer_placement_invariant;
      test_case "an unlayered definition wins over any layer stack" [ bytes ]
        test_unlayered_definition_wins;
    ] )
