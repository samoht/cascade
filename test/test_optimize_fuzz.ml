(** Optimize tests: property-based invariants of [Css.optimize] and nested-rule
    merging. *)

open Cascade

(* A rule's [declarations] is the run written before its first nested statement
   (CSS Nesting 1 sec. 3.4), not its whole body, so absorbing a later
   same-selector rule moves that rule's declarations ahead of body content the
   merge has to read for itself. It reads the whole body, so a rule carrying
   nested children can still absorb a later one: the move is only observable for
   a property that body also sets. *)
let same_selector_merge_past_nested () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "disjoint nested children do not block the merge"
    ".a{color:red;padding:1rem;&:hover{background:#00f}}"
    (canon ".a{color:red;&:hover{background:blue}}.a{padding:1rem}");
  Alcotest.(check string)
    "a nested child setting the same property keeps its place"
    ".a{color:red;&:hover{padding:2rem}}.a{padding:1rem}"
    (canon ".a{color:red;&:hover{padding:2rem}}.a{padding:1rem}");
  (* A nested conditional group sets properties just as a nested rule does:
     hoisting [padding:1rem] ahead of it hands the conditional the win wherever
     it applies, which is a different rendered padding. *)
  Alcotest.(check string)
    "a nested @media setting the same property blocks it"
    ".a{color:red;@media(width>=1px){padding:2rem}}.a{padding:1rem}"
    (canon ".a{color:red;@media (min-width:1px){padding:2rem}}.a{padding:1rem}");
  Alcotest.(check string)
    "a nested @container setting the same property blocks it"
    ".a{color:red;@container(width>=1px){padding:2rem}}.a{padding:1rem}"
    (canon
       ".a{color:red;@container (min-width:1px){padding:2rem}}.a{padding:1rem}");
  (* The merged rule replays every declaration ahead of every nested block, so
     no order of the rules rescues a merge a later declaration would have to
     cross: [color:green] wins over the [@media] wherever it applies, and
     merging hands the win back to the [@media]. *)
  Alcotest.(check string)
    "a later declaration overriding a nested @media blocks the merge"
    ".a{padding:1rem;@media(width>=1px){color:#00f}}.a{color:green}"
    (canon ".a{padding:1rem;@media (min-width:1px){color:blue}}.a{color:green}");
  (* Two nested blocks setting a common property race in the order the merged
     rule replays them, which stays the order they were written in. *)
  Alcotest.(check string)
    "nested blocks setting a common property keep their order"
    ".a{padding:1rem;margin:1rem;@media(width>=1px){color:#00f}@media(width>=2px){color:green}}"
    (canon
       ".a{padding:1rem;@media \
        (min-width:1px){color:blue}}.a{margin:1rem;@media \
        (min-width:2px){color:green}}");
  (* A shorthand and a longhand of its family write a common cascade slot, so a
     nested [margin] and a later [margin-top] compete: hoisting the longhand
     ahead of the nested block hands [margin] the win, a different rendered
     margin-top wherever the query applies. *)
  Alcotest.(check string)
    "a later longhand overriding a nested shorthand blocks the merge"
    ".a{color:red;@media(width>=1px){margin:2rem}}.a{margin-top:1rem}"
    (canon
       ".a{color:red;@media (min-width:1px){margin:2rem}}.a{margin-top:1rem}");
  (* The same slot written the other way round: a later shorthand resets the
     longhand the nested block set. *)
  Alcotest.(check string)
    "a later shorthand overriding a nested longhand blocks the merge"
    ".a{color:red;@media(width>=1px){margin-top:2rem}}.a{margin:1rem}"
    (canon
       ".a{color:red;@media (min-width:1px){margin-top:2rem}}.a{margin:1rem}");
  (* Two properties of different families share no slot, so the merge stands:
     the guard reads footprints, not the presence of a nested block. *)
  Alcotest.(check string)
    "a nested block of a disjoint family still merges"
    ".a{color:red;padding:1rem;@media(width>=1px){margin:2rem}}"
    (canon ".a{color:red;@media (min-width:1px){margin:2rem}}.a{padding:1rem}");
  (* Nested blocks race in the order the merged rule replays them, and shorthand
     against longhand is such a race: swapping these two blocks changes
     margin-top wherever both queries apply. *)
  Alcotest.(check string)
    "nested blocks sharing a slot through a shorthand keep their order"
    ".a{padding:1rem;color:red;@media(width>=1px){margin:2rem}@media(width>=2px){margin-top:3rem}}"
    (canon
       ".a{padding:1rem;@media \
        (min-width:1px){margin:2rem}}.a{color:red;@media \
        (min-width:2px){margin-top:3rem}}")

(* Whether a nested body and a later declaration block share a slot reads the
   same whichever of the two is asked about: a custom property overlaps the same
   name and nothing else, [all] reaches every non-exempt slot, and a property
   the footprint model cannot place reaches whatever it meets. Each of those is
   stated over an ordered pair, so each is checked here from both ends. The
   shorthand-against-longhand pair is above. *)
let nested_merge_reads_the_slot_from_either_side () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "a later write of the nested custom property blocks the merge"
    ".a{color:red;&:hover{--x:2}}.a{--x:1}"
    (canon ".a{color:red;&:hover{--x:2}}.a{--x:1}");
  Alcotest.(check string)
    "another custom property still merges" ".a{--y:1;color:red;&:hover{--x:2}}"
    (canon ".a{color:red;&:hover{--x:2}}.a{--y:1}");
  Alcotest.(check string)
    "a custom property and a longhand share no slot"
    ".a{color:red;padding:1rem;&:hover{--x:2}}"
    (canon ".a{color:red;&:hover{--x:2}}.a{padding:1rem}");
  Alcotest.(check string)
    "nor do they the other way round"
    ".a{--x:1;color:red;&:hover{padding:2rem}}"
    (canon ".a{color:red;&:hover{padding:2rem}}.a{--x:1}");
  Alcotest.(check string)
    "a nested all reset blocks the merge"
    ".a{color:red;&:hover{all:unset}}.a{padding:1rem}"
    (canon ".a{color:red;&:hover{all:unset}}.a{padding:1rem}");
  Alcotest.(check string)
    "a nested unplaceable name blocks the merge"
    ".a{color:red;&:hover{unknown-thing:2}}.a{padding:1rem}"
    (canon ".a{color:red;&:hover{unknown-thing:2}}.a{padding:1rem}");
  Alcotest.(check string)
    "and so does a later one"
    ".a{color:red;&:hover{padding:2rem}}.a{unknown-thing:1}"
    (canon ".a{color:red;&:hover{padding:2rem}}.a{unknown-thing:1}")

(* A run of declarations written after a nested statement (CSS Nesting 1 sec.
   3.4) is a declaration list like any other, with nothing between two writes
   inside it, so the deduplication a rule body gets applies to it. *)
let nested_declaration_run_dedupes () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  (* The run cannot move: [color] is what the nested rule sets too. *)
  Alcotest.(check string)
    "a repeated declaration inside a run collapses" ".a{b{color:red}color:#00f}"
    (canon ".a{& b{color:red}color:blue;color:blue}");
  Alcotest.(check string)
    "a declaration the run itself overrides goes" ".a{b{color:red}color:green}"
    (canon ".a{& b{color:red}color:blue;color:green}");
  (* The crossed shorthand pins every one of the four longhands, so they compose
     where they stand. *)
  Alcotest.(check string)
    "longhands inside a run compose" ".a{b{margin:9px}margin:1px}"
    (canon
       ".a{& \
        b{margin:9px}margin-top:1px;margin-right:1px;margin-bottom:1px;margin-left:1px}")

(* A selector list holding a vendor pseudo-element is invalidated as a whole by
   a browser that does not know it, so the other selectors silently lose the
   declarations. The grouping passes refuse to build such a list; one the author
   wrote is split so the risky branches stand alone. *)
let vendor_pseudo_list_is_split () =
  let of_string css =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Alcotest.failf "could not parse %s" css
  in
  let canon css =
    Css.Optimize.stylesheet (Css.statements (of_string css))
    |> Css.Stylesheet.to_string ~minify:true
    |> String.trim
  in
  Alcotest.(check string)
    "the risky branches leave the list"
    ".i::-webkit-search-cancel-button{display:none}.i::-webkit-search-decoration{display:none}.i::-webkit-search-results-button,.reset{display:none}"
    (canon
       ".i::-webkit-search-cancel-button,.i::-webkit-search-decoration,.i::-webkit-search-results-button{display:none}.reset{display:none}");
  Alcotest.(check string)
    "a list of only risky branches is left alone"
    ".i::-webkit-search-cancel-button,.i::-webkit-search-decoration{display:none}"
    (canon
       ".i::-webkit-search-cancel-button,.i::-webkit-search-decoration{display:none}")

(* Property-based fuzz tests for [Css.optimize]'s high-level invariants. *)
module Fuzz = struct
  open Cascade

  let error_str e = Error.to_string e

  let parse s =
    match Css.of_string ~strict:false s with
    | Ok p -> p.Css.stylesheet
    | Error e -> Alcotest.failf "parse failed: %s\n  for: %s" (error_str e) s

  let minify_str s = parse s |> Css.to_string ~minify:true
  let optimize_str s = parse s |> Css.optimize |> Css.to_string ~minify:true

  (* --- deterministic generator --- *)

  let selectors =
    [| ".a"; ".b"; ".c"; ".a.b"; ".a.c"; ".b.c"; "#x"; "div"; "p"; "div.a" |]

  (* Only independent property names (no shorthand/longhand aliasing), so an
     element's computed style is a clean per-name map - and a name-level cascade
     change is exactly what a reordering bug would cause. *)
  let decls =
    [|
      "color:red";
      "color:green";
      "color:blue";
      "margin:0";
      "margin:1px";
      "display:block";
      "display:flex";
      "padding:0";
      "padding:2px";
      "top:0";
      "top:1px";
      "z-index:1";
      "z-index:2";
    |]

  let pick rng a = a.(Random.State.int rng (Array.length a))

  let gen_rule rng =
    let sel = pick rng selectors in
    let n = 1 + Random.State.int rng 3 in
    let ds = List.init n (fun _ -> pick rng decls) in
    Fmt.str "%s{%s}" sel (String.concat ";" ds)

  let gen_rules rng =
    List.init (2 + Random.State.int rng 6) (fun _ -> gen_rule rng)

  (* --- elements for the cascade resolver --- *)

  type node = { nname : string; nid : string option; nclasses : string list }

  module Node = struct
    type t = node

    let equal = ( = )
    let name t = Some t.nname
    let id t = t.nid
    let classes t = t.nclasses
    let attribute _ _ : string option = None
    let parent _ : node option = None
    let children _ = []
    let text_children _ = []
  end

  module R = Resolve.Make (Node)

  (* Every element distinguishable by the selector alphabet: tag x optional id x
     class subset. Checking all of them makes the cascade-equivalence and
     permutation-neutrality judgements complete for this alphabet. *)
  let all_elements =
    let class_subsets =
      [
        [];
        [ "a" ];
        [ "b" ];
        [ "c" ];
        [ "a"; "b" ];
        [ "a"; "c" ];
        [ "b"; "c" ];
        [ "a"; "b"; "c" ];
      ]
    in
    List.concat_map
      (fun nname ->
        List.concat_map
          (fun nid ->
            List.map (fun nclasses -> { nname; nid; nclasses }) class_subsets)
          [ None; Some "x" ])
      [ "div"; "p"; "span" ]

  (* Canonical form of one declaration's value, so the comparison is semantic,
     not textual: optimize legitimately rewrites a value (e.g. [blue] -> [#00f],
     [0px] -> [0]) without changing the cascade. Memoized - the same
     declarations recur across thousands of iterations. *)
  let canon_memo : (string, string) Hashtbl.t = Hashtbl.create 256

  let canon_decl d =
    let s = Pp.to_string ~minify:true Declaration.pp d in
    match Hashtbl.find_opt canon_memo s with
    | Some c -> c
    | None ->
        let c =
          match Css.of_string ~strict:false ("x{" ^ s ^ "}") with
          | Ok p ->
              let out =
                Css.to_string ~minify:true (Css.optimize p.Css.stylesheet)
              in
              if String.length out >= 3 then
                String.sub out 2 (String.length out - 3)
              else out
          | Error _ -> s
        in
        Hashtbl.replace canon_memo s c;
        c

  let computed sheet elt =
    R.resolve sheet elt |> List.map canon_decl |> List.sort String.compare

  let describe e =
    Fmt.str "%s%s%s" e.nname
      (match e.nid with Some i -> "#" ^ i | None -> "")
      (match e.nclasses with [] -> "" | cs -> "." ^ String.concat "." cs)

  let iters = 1000

  (* Invariant 1 - idempotence: reparsing and re-optimizing the output changes
     nothing. Guards against any pass that only converges on a second run. *)
  let fuzz_idempotent () =
    let rng = Random.State.make [| 0x1d3a |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_rules rng) in
      let once = optimize_str css in
      let twice = optimize_str once in
      if once <> twice then
        Alcotest.failf
          "not a fixed point:\n  in:    %s\n  once:  %s\n  twice: %s" css once
          twice
    done

  (* Invariant 2 - validity and size: the output reparses and is never larger
     than plain (non-optimizing) minification of the same input. *)
  let fuzz_valid_and_no_larger () =
    let rng = Random.State.make [| 0x5a1d |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_rules rng) in
      let opt = optimize_str css in
      (match Css.of_string ~strict:false opt with
      | Ok _ -> ()
      | Error e ->
          Alcotest.failf
            "optimize produced invalid CSS: %s\n  in:  %s\n  out: %s"
            (error_str e) css opt);
      if String.length opt > String.length (minify_str css) then
        Alcotest.failf
          "optimize larger than minify:\n  in:  %s\n  min: %s\n  opt: %s" css
          (minify_str css) opt
    done

  (* Invariant 3 - cascade equivalence (soundness): the computed style of every
     element is identical before and after optimization. *)
  let fuzz_cascade_equivalent () =
    let rng = Random.State.make [| 0xca5c |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_rules rng) in
      let before = parse css and after = parse (optimize_str css) in
      List.iter
        (fun e ->
          let b = computed before e and a = computed after e in
          if b <> a then
            Alcotest.failf
              "optimize changed the cascade for %s:\n\
              \  css:    %s\n\
              \  before: %s\n\
              \  after:  %s"
              (describe e) css (String.concat " " b) (String.concat " " a))
        all_elements
    done

  (* Invariant 4 - neutral-permutation soundness: a permutation that preserves
     every element's computed style (a cascade-neutral reorder) must still
     compute the same styles after optimization. The outputs need not be
     byte-identical: source order is part of the generator-facing textual
     contract. Neutrality is judged over the complete element set, so it is
     never falsely claimed. *)
  let shuffle rng lst =
    let a = Array.of_list lst in
    for i = Array.length a - 1 downto 1 do
      let j = Random.State.int rng (i + 1) in
      let t = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- t
    done;
    Array.to_list a

  let fuzz_neutral_permutation_soundness () =
    let rng = Random.State.make [| 0xbeef |] in
    let checked = ref 0 in
    for _ = 1 to iters do
      let rules = gen_rules rng in
      let x = String.concat "" rules
      and x' = String.concat "" (shuffle rng rules) in
      let bx = parse x and bx' = parse x' in
      let neutral =
        List.for_all (fun e -> computed bx e = computed bx' e) all_elements
      in
      if neutral then begin
        incr checked;
        let ax = parse (optimize_str x) and ax' = parse (optimize_str x') in
        List.iter
          (fun e ->
            let sx = computed ax e and sx' = computed ax' e in
            if sx <> sx' then
              Alcotest.failf
                "cascade-neutral permutation diverged after optimization for %s:\n\
                \  x:   %s\n\
                \  x':  %s\n\
                \  x:   %s\n\
                \  x':  %s"
                (describe e) x x' (String.concat " " sx) (String.concat " " sx'))
          all_elements
      end
    done;
    (* a permutation is neutral often enough that this should fire many times;
       guard against the test silently checking nothing. *)
    Alcotest.(check bool)
      (Fmt.str "exercised %d neutral permutations" !checked)
      true (!checked > 0)

  (* --- shorthand cascade-order soundness ---

     The per-name [computed] above is blind to shorthand/longhand overlap: it
     treats [border-top] and [border-color] as different properties, missing
     that both write [border-top-color]. This oracle compares the computed style
     at the *longhand* level, expanding each declaration through a hand-written
     footprint table independent of cascade's own shorthand code. Values are
     chosen not to fold under minify (5px/7px/red/green/solid all stay), so an
     optimised declaration minifies back to its alphabet key. *)
  let sh_alphabet =
    [
      ( "margin:5px",
        [
          ("margin-top", "5px");
          ("margin-right", "5px");
          ("margin-bottom", "5px");
          ("margin-left", "5px");
        ] );
      ( "margin:9px",
        [
          ("margin-top", "9px");
          ("margin-right", "9px");
          ("margin-bottom", "9px");
          ("margin-left", "9px");
        ] );
      ("margin-top:7px", [ ("margin-top", "7px") ]);
      ( "padding:5px",
        [
          ("padding-top", "5px");
          ("padding-right", "5px");
          ("padding-bottom", "5px");
          ("padding-left", "5px");
        ] );
      ( "border-top:3px solid red",
        [
          ("border-top-width", "3px");
          ("border-top-style", "solid");
          ("border-top-color", "red");
        ] );
      ( "border-color:green",
        [
          ("border-top-color", "green");
          ("border-right-color", "green");
          ("border-bottom-color", "green");
          ("border-left-color", "green");
        ] );
      ("border-top-color:red", [ ("border-top-color", "red") ]);
      ("color:red", [ ("color", "red") ]);
    ]

  let sh_decls = Array.of_list (List.map fst sh_alphabet)
  let sh_decl_string d = Pp.to_string ~minify:true Declaration.pp d

  (* Four-side box shorthands: physical longhand names, in top/right/bottom/left
     order. optimize composes longhand runs back into these, so the oracle must
     expand any box shorthand, not just the uniform alphabet forms. *)
  let box_sides =
    [
      ( "margin",
        [ "margin-top"; "margin-right"; "margin-bottom"; "margin-left" ] );
      ( "padding",
        [ "padding-top"; "padding-right"; "padding-bottom"; "padding-left" ] );
      ("inset", [ "top"; "right"; "bottom"; "left" ]);
      ( "border-color",
        [
          "border-top-color";
          "border-right-color";
          "border-bottom-color";
          "border-left-color";
        ] );
      ( "border-width",
        [
          "border-top-width";
          "border-right-width";
          "border-bottom-width";
          "border-left-width";
        ] );
      ( "border-style",
        [
          "border-top-style";
          "border-right-style";
          "border-bottom-style";
          "border-left-style";
        ] );
    ]

  (* Distribute 1..4 box values over top/right/bottom/left per the CSS box
     rule. *)
  let box_values value =
    match String.split_on_char ' ' value |> List.filter (fun s -> s <> "") with
    | [ a ] -> [ a; a; a; a ]
    | [ a; b ] -> [ a; b; a; b ]
    | [ a; b; c ] -> [ a; b; c; b ]
    | a :: b :: c :: d :: _ -> [ a; b; c; d ]
    | [] -> [ value; value; value; value ]

  (* Expand a declaration to its (longhand, value) footprint, independent of
     cascade's own shorthand code. [box_sides] handles the four-side families
     (including composed forms like [margin:7px 9px 9px]); [sh_alphabet] handles
     the non-box shorthands ([border-top]); anything else is its own
     longhand. *)
  let longhands_of s =
    match String.index_opt s ':' with
    | None -> [ (s, "") ]
    | Some i -> (
        let prop = String.sub s 0 i in
        let value = String.sub s (i + 1) (String.length s - i - 1) in
        match List.assoc_opt prop box_sides with
        | Some sides -> List.combine sides (box_values value)
        | None -> (
            match List.assoc_opt s sh_alphabet with
            | Some lhs -> lhs
            | None -> [ (prop, value) ]))

  (* Class-only branches: nested [&.c] output can compose into nested [Compound]
     nodes, so collect class parts recursively inside a compound. Selectors with
     combinators or non-class parts still contribute no branch; the shorthand
     alphabet is class-only. *)
  let rec class_only_compound : Selector.t -> string list option = function
    | Selector.Class c -> Some [ c ]
    | Selector.Compound ps ->
        List.fold_left
          (fun acc part ->
            match (acc, class_only_compound part) with
            | Some acc, Some classes -> Some (acc @ classes)
            | _ -> None)
          (Some []) ps
    | _ -> None

  let rec branch_class_sets (sel : Selector.t) =
    match sel with
    | Selector.Class c -> [ [ c ] ]
    | Selector.Compound _ -> (
        match class_only_compound sel with
        | Some classes -> [ classes ]
        | None -> [])
    | Selector.List ss -> List.concat_map branch_class_sets ss
    | _ -> []

  let class_subset small big = List.for_all (fun x -> List.mem x big) small

  (* applicable specificity = max class count over matching branches, or None *)
  let sh_match sel classes : int option =
    match
      branch_class_sets sel |> List.filter (fun cs -> class_subset cs classes)
    with
    | [] -> None
    | bs -> Some (List.fold_left (fun m cs -> max m (List.length cs)) 0 bs)

  let sh_rules sheet =
    let rec rule ?parent (rule : Stylesheet.rule) =
      let selector =
        match parent with
        | None -> Stylesheet.selector rule
        | Some parent -> Nest.combine parent (Stylesheet.selector rule)
      in
      let direct =
        if Stylesheet.declarations rule = [] then []
        else [ { rule with selector; nested = [] } ]
      in
      direct @ statements ~parent:selector (Stylesheet.nested rule)
    and statements ?parent stmts =
      List.concat_map
        (function Stylesheet.Rule r -> rule ?parent r | _ -> [])
        stmts
    in
    statements (Css.statements sheet)

  (* computed longhand -> value map for one element, in cascade order *)
  let sh_computed sheet classes =
    let matched =
      sh_rules sheet
      |> List.mapi (fun i r -> (i, r))
      |> List.filter_map (fun (i, r) ->
          match sh_match (Stylesheet.selector r) classes with
          | Some spec -> Some (spec, i, Stylesheet.declarations r)
          | None -> None)
      |> List.sort (fun (s1, i1, _) (s2, i2, _) ->
          match compare s1 s2 with 0 -> compare i1 i2 | c -> c)
    in
    let tbl = Hashtbl.create 16 in
    List.iter
      (fun (_, _, decls) ->
        List.iter
          (fun d ->
            List.iter
              (fun (lh, v) -> Hashtbl.replace tbl lh v)
              (longhands_of (sh_decl_string d)))
          decls)
      matched;
    Hashtbl.fold (fun k v acc -> (k ^ ":" ^ v) :: acc) tbl []
    |> List.sort String.compare

  let sh_class_subsets =
    [
      [];
      [ "a" ];
      [ "b" ];
      [ "c" ];
      [ "a"; "b" ];
      [ "a"; "c" ];
      [ "b"; "c" ];
      [ "a"; "b"; "c" ];
    ]

  let sh_selectors = [| ".a"; ".b"; ".c"; ".a.b"; ".a.c"; ".b.c"; ".a.b.c" |]

  let gen_sh_rule rng =
    let sel = pick rng sh_selectors in
    let n = 1 + Random.State.int rng 2 in
    let ds = List.init n (fun _ -> pick rng sh_decls) in
    Fmt.str "%s{%s}" sel (String.concat ";" ds)

  let gen_sh_rules rng =
    List.init (2 + Random.State.int rng 6) (fun _ -> gen_sh_rule rng)

  (* The DAG must keep two rules ordered (or refuse to merge) whenever their
     declarations share a longhand, even through different shorthands
     ([border-top] vs [border-color] over [border-top-color]). A false-disjoint
     reorders them and flips a longhand winner; this catches it. *)
  let fuzz_shorthand_cascade_equivalent () =
    let rng = Random.State.make [| 0x5304 |] in
    for _ = 1 to iters do
      let css = String.concat "" (gen_sh_rules rng) in
      let before = parse css and after = parse (optimize_str css) in
      List.iter
        (fun classes ->
          let b = sh_computed before classes
          and a = sh_computed after classes in
          if b <> a then
            Alcotest.failf
              "optimize changed the longhand cascade for .%s:\n\
              \  css:    %s\n\
              \  before: %s\n\
              \  after:  %s"
              (String.concat "." classes)
              css (String.concat " " b) (String.concat " " a))
        sh_class_subsets
    done

  (* Two source orders that agree on every element's longhand cascade must
     optimise to cascade-equivalent output: the DAG result cannot depend on a
     shorthand-overlap order it is free to reorder. *)
  let fuzz_shorthand_permutation_sound () =
    let rng = Random.State.make [| 0x5305 |] in
    let checked = ref 0 in
    for _ = 1 to iters do
      let rules = gen_sh_rules rng in
      let x = String.concat "" rules
      and x' = String.concat "" (shuffle rng rules) in
      let bx = parse x and bx' = parse x' in
      let neutral =
        List.for_all
          (fun e -> sh_computed bx e = sh_computed bx' e)
          sh_class_subsets
      in
      if neutral then begin
        incr checked;
        let ax = parse (optimize_str x) and ax' = parse (optimize_str x') in
        List.iter
          (fun e ->
            if sh_computed ax e <> sh_computed ax' e then
              Alcotest.failf
                "shorthand-neutral permutation diverged for .%s:\n\
                \  x:  %s\n\
                \  x': %s"
                (String.concat "." e) x x')
          sh_class_subsets
      end
    done;
    Alcotest.(check bool)
      (Fmt.str "exercised %d shorthand-neutral permutations" !checked)
      true (!checked > 0)

  let cases =
    [
      Alcotest.test_case "optimize is idempotent" `Slow fuzz_idempotent;
      Alcotest.test_case "optimize output is valid and no larger" `Slow
        fuzz_valid_and_no_larger;
      Alcotest.test_case "optimize preserves the cascade" `Slow
        fuzz_cascade_equivalent;
      Alcotest.test_case "same-selector merge past nested children" `Quick
        same_selector_merge_past_nested;
      Alcotest.test_case "nested merge reads the slot from either side" `Quick
        nested_merge_reads_the_slot_from_either_side;
      Alcotest.test_case "a nested declarations run is deduplicated" `Quick
        nested_declaration_run_dedupes;
      Alcotest.test_case "vendor pseudo-element list is split" `Quick
        vendor_pseudo_list_is_split;
      Alcotest.test_case "cascade-neutral permutations stay equivalent" `Slow
        fuzz_neutral_permutation_soundness;
      Alcotest.test_case "shorthand longhand cascade preserved" `Slow
        fuzz_shorthand_cascade_equivalent;
      Alcotest.test_case "shorthand-neutral permutations stay equivalent" `Slow
        fuzz_shorthand_permutation_sound;
    ]
end

let suite = ("optimize_fuzz", Fuzz.cases)
