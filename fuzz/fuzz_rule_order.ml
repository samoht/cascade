(** Fuzz tests for the canonical projection's layer normal form.

    The projection is a canonical form: it must be confluent, so two sheets that
    establish the same layer order and hold the same rules project to one
    string, and idempotent, so projecting its own output changes nothing. These
    cases generate a layer order, spell it several ways (one statement,
    per-layer statements, blocks alone), and check the projection brings them
    together. *)

open Cascade
open Alcobar

let byte buf i =
  if String.length buf = 0 then 0 else Char.code buf.[i mod String.length buf]

let pick xs buf i = List.nth xs (byte buf i mod List.length xs)
let idents = [| "a"; "b"; "c"; "d" |]
let ident buf i = pick (Array.to_list idents) buf i

(* A layer name: one ident, a dotted path of two, or a single ident that holds
   an escaped dot and so is not the same layer as the path. *)
let name buf i =
  match byte buf i mod 3 with
  | 0 -> [ ident buf (i + 1) ]
  | 1 -> [ ident buf (i + 1); ident buf (i + 2) ]
  | _ -> [ ident buf (i + 1) ^ "." ^ ident buf (i + 2) ]

let layer_names buf =
  let n = 1 + (byte buf 0 mod 4) in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let name = name buf (i + 1) in
      let acc =
        if List.exists (Stylesheet.equal_layer_name name) acc then acc
        else name :: acc
      in
      go (i + 1) acc
  in
  go 0 []

(* The CSS spelling of a layer name: [a.b] is the sublayer [b] of [a], and an
   ident that holds a dot is spelled with it escaped. *)
let spell name =
  let escape s = String.concat "\\." (String.split_on_char '.' s) in
  String.concat "." (List.map escape name)

let has_block buf i = byte buf (i + 40) land 1 = 0

let body buf i =
  if byte buf (i + 60) land 1 = 0 then "x{top:0}" else "x{color:red}"

(* One [@layer a,b;] statement naming every layer, then a block per layer that
   has one. *)
let sheet_one_statement buf names =
  let statement = "@layer " ^ String.concat "," (List.map spell names) ^ ";" in
  let blocks =
    List.mapi
      (fun i name ->
        if has_block buf i then "@layer " ^ spell name ^ "{" ^ body buf i ^ "}"
        else "")
      names
  in
  statement ^ String.concat "" blocks

(* Per-layer statements for the layers that have no block, blocks for the rest,
   the same order. *)
let sheet_per_name buf names =
  String.concat ""
    (List.mapi
       (fun i name ->
         if has_block buf i then "@layer " ^ spell name ^ "{" ^ body buf i ^ "}"
         else "@layer " ^ spell name ^ ";")
       names)

let canonical css =
  match Css.of_string ~strict:false css with
  | Ok { stylesheet; _ } ->
      Css.statements stylesheet |> Rule_order.canonicalize
      |> Pp.to_string ~minify:true Stylesheet.pp_stylesheet
  | Error e ->
      failf "layer sheet did not parse (%s): %S" (Error.to_string e) css

let test_layer_respellings_converge buf =
  let names = layer_names buf in
  let one = canonical (sheet_one_statement buf names) in
  let per = canonical (sheet_per_name buf names) in
  if one <> per then
    failf "two spellings of one layer order project apart:\n  %S\n  %S" one per

let test_layer_projection_is_idempotent buf =
  let names = layer_names buf in
  let once = canonical (sheet_one_statement buf names) in
  let twice = canonical once in
  if once <> twice then
    failf "projecting the projection changed it:\n  %S\n  %S" once twice

(* The order a name list establishes, with each [a.b] path expanded to [a] then
   [a.b]: reversing [a.b, a] establishes the same order as [a, a.b]. *)
let established_order names =
  let rec go seen = function
    | [] -> List.rev seen
    | name :: rest ->
        let rec prefixes acc cur = function
          | [] -> List.rev acc
          | x :: tl ->
              let cur = cur @ [ x ] in
              prefixes (cur :: acc) cur tl
        in
        let seen =
          List.fold_left
            (fun seen prefix ->
              if List.exists (Stylesheet.equal_layer_name prefix) seen then seen
              else prefix :: seen)
            seen (prefixes [] [] name)
        in
        go seen rest
  in
  go [] names

let test_reversed_order_stays_distinct buf =
  let names = layer_names buf in
  let reversed = List.rev names in
  if
    List.length names > 1
    && not
         (List.equal (List.equal String.equal) (established_order names)
            (established_order reversed))
  then
    let forward = canonical (sheet_per_name buf names) in
    let backward = canonical (sheet_per_name buf reversed) in
    if forward = backward then
      failf "two layer orders project together: %S" forward

let suite =
  ( "rule_order",
    [
      test_case "layer respellings converge" [ bytes ]
        test_layer_respellings_converge;
      test_case "layer projection is idempotent" [ bytes ]
        test_layer_projection_is_idempotent;
      test_case "reversed layer order stays distinct" [ bytes ]
        test_reversed_order_stays_distinct;
    ] )
