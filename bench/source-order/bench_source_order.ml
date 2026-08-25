open Cascade

let rules_of s =
  match Css.of_string ~strict:false s with
  | Ok p ->
      List.filter_map
        (function Stylesheet.Rule r -> Some r | _ -> None)
        p.Css.stylesheet
  | Error _ -> failwith "parse"

let shape name n =
  let b = Buffer.create (n * 32) in
  for i = 0 to n - 1 do
    (match name with
      | "A" -> Fmt.str ".c%d{color:rgb(%d,%d,0)}" i (i mod 256) (i / 256)
      | "B" -> Fmt.str ".c%d{color:red}" i
      | "C" -> Fmt.str "#i%d{color:rgb(%d,%d,0)}" i (i mod 256) (i / 256)
      | "D" -> Fmt.str "e%d{color:rgb(%d,%d,0)}" i (i mod 256) (i / 256)
      | _ -> assert false)
    |> Buffer.add_string b
  done;
  rules_of (Buffer.contents b)

(* Differenced across two iteration counts, so the parse and the one-off startup
   cancel out. Three builds are enough because allocation is deterministic; all
   four shapes should remain linear when the rule count doubles. *)
let words rules =
  let run k =
    Gc.full_major ();
    let w0 = Gc.minor_words () in
    for _ = 1 to k do
      ignore (Sys.opaque_identity (Rule_graph.of_rules rules))
    done;
    Gc.minor_words () -. w0
  in
  let w1 = run 1 in
  let w3 = run 3 in
  (w3 -. w1) /. 2.

let () =
  print_endline "shape n words_per_build";
  List.iter
    (fun s ->
      List.iter
        (fun n ->
          let rules = shape s n in
          Fmt.pr "%s %d %.0f@." s n (words rules))
        [ 500; 1000; 2000; 4000 ])
    [ "A"; "B"; "C"; "D" ]
