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

(* What a graph costs to build says nothing about what it costs to use, and this
   bench weighed only the build while an asymptotic regression shipped in the
   questions. Orienting a factoring asks which of two nodes has to come first,
   once per ordered pair, so a run of N rules is asked about N^2 pairs; counting
   the nodes those answers expand is immune to whatever else the machine is
   doing, and stays flat per node when the edges are only passed over once. *)
let expansions rules =
  let g = Rule_graph.of_rules rules in
  let n = Rule_graph.node_count g in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      ignore
        (Sys.opaque_identity
           (Rule_graph.precedes g
              (Rule_graph.Node_id.of_int_exn i)
              (Rule_graph.Node_id.of_int_exn j)))
    done
  done;
  (n, Rule_graph.reachability_expansions g)

let sizes = [ 500; 1000; 2000; 4000 ]
let shapes = [ "A"; "B"; "C"; "D" ]

let () =
  print_endline "shape n words_per_build";
  List.iter
    (fun s ->
      List.iter (fun n -> Fmt.pr "%s %d %.0f@." s n (words (shape s n))) sizes)
    shapes;
  print_endline "shape n pairs expansions per_node";
  List.iter
    (fun s ->
      List.iter
        (fun n ->
          let nodes, spent = expansions (shape s n) in
          Fmt.pr "%s %d %d %d %.2f@." s n (nodes * nodes) spent
            (float_of_int spent /. float_of_int (max nodes 1)))
        sizes)
    shapes
