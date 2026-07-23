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

let suite =
  ( "inline",
    [
      test_case "layer placement leaves single-def inlining unchanged" [ bytes ]
        test_layer_placement_invariant;
    ] )
