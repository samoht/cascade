(** Fuzz tests for the canonical projection.

    The projection is a canonical form: it must be confluent, so two sheets that
    differ only by the order of rules the cascade does not order, or by where
    adjacent occurrences of one selector are folded, project to one string, and
    so must two sheets that establish the same layer order and hold the same
    rules; and it is idempotent, so projecting its own output changes nothing.
    The layer cases generate a layer order and spell it several ways (one
    statement, per-layer statements, blocks alone); the rule cases generate a
    run of rules over a few selectors, conditions and declarations that meet,
    and swap or fold two neighbours. *)

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
  | Error e -> failf "sheet did not parse (%s): %S" (Error.to_string e) css

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

(* ===== Runs of rules ===== *)

let selectors =
  [| ".a"; ".b"; ".c"; "a.b"; "div"; ".a::before"; "#x"; ".a .b"; ".b:hover" |]

(* Declarations that meet one another every way the order relation reads: one
   property with two values, a shorthand and its longhands, a value one rule
   repeats, an important write, a legacy alias and its twin, and a name the
   footprint model cannot place. *)
let declarations =
  [|
    "color:red";
    "color:blue";
    "color:red!important";
    "margin:0";
    "margin-top:1px";
    "margin-left:2px";
    "width:1px";
    "width:2px";
    "display:none";
    "display:block";
    "border:1px solid red";
    "border-color:blue";
    "border-bottom-color:green";
    "top:0";
    "-webkit-transform:none";
    "transform:none";
    "speak:none";
  |]

(* No wrapper most of the time, then a media query, its negation, and a feature
   query. *)
let wrappers =
  [|
    "";
    "";
    "";
    "";
    "@media (min-width:1px)";
    "@media not (min-width:1px)";
    "@supports (display:grid)";
  |]

type run_rule = { wrapper : string; selector : string; body : string list }

let property d = List.hd (String.split_on_char ':' d)

(* One declaration of each property: two writes of one property in one rule are
   the dedup's to read, and a rule split or folded would read them across rules
   instead. *)
let distinct_properties ds =
  List.fold_left
    (fun acc d ->
      if List.exists (fun e -> String.equal (property e) (property d)) acc then
        acc
      else acc @ [ d ])
    [] ds

let run_rule =
  map
    [
      range (Array.length wrappers);
      range (Array.length selectors);
      list1 (range (Array.length declarations));
    ]
    (fun w s ds ->
      {
        wrapper = wrappers.(w);
        selector = selectors.(s);
        body =
          distinct_properties
            (List.filteri
               (fun i _ -> i < 4)
               (List.map (fun d -> declarations.(d)) ds));
      })

let rule_text r =
  let rule = r.selector ^ "{" ^ String.concat ";" r.body ^ "}" in
  if String.equal r.wrapper "" then rule else r.wrapper ^ "{" ^ rule ^ "}"

let sheet rules = String.concat "" (List.map rule_text rules)

let plain_rule r =
  match
    Css.of_string ~strict:false
      (r.selector ^ "{" ^ String.concat ";" r.body ^ "}")
  with
  | Ok { stylesheet; _ } -> (
      match Css.statements stylesheet with
      | [ Stylesheet.Rule rule ] -> rule
      | _ -> failf "not one rule: %s" (rule_text r))
  | Error e ->
      failf "rule did not parse (%s): %s" (Error.to_string e) (rule_text r)

let first n l = List.filteri (fun i _ -> i < n) l

(* Two neighbours on different selectors that {!Rule_graph.rules_conflict}
   leaves unordered, under any conditions, may swap: no element can tell the two
   orders apart, so the canonical form may not either. *)
let test_independent_rules_commute rules k =
  let rules = Array.of_list (first 8 rules) in
  let n = Array.length rules in
  guard (n >= 2);
  let k = k mod (n - 1) in
  let a = rules.(k) and b = rules.(k + 1) in
  guard (not (String.equal a.selector b.selector));
  guard (not (Rule_graph.rules_conflict (plain_rule a) (plain_rule b)));
  let swapped = Array.copy rules in
  swapped.(k) <- b;
  swapped.(k + 1) <- a;
  let one = sheet (Array.to_list rules)
  and two = sheet (Array.to_list swapped) in
  let c1 = canonical one and c2 = canonical two in
  if not (String.equal c1 c2) then
    failf
      "two orders of independent rules project apart:\n  %S -> %S\n  %S -> %S"
      one c1 two c2

(* Two neighbours on one selector under one wrapper are one rule holding both
   bodies, so folding them must not move the canonical form. *)
let test_adjacent_occurrences_fold rules k =
  let rules = Array.of_list (first 8 rules) in
  let n = Array.length rules in
  guard (n >= 2);
  let k = k mod (n - 1) in
  let a = rules.(k) and b = rules.(k + 1) in
  guard (String.equal a.selector b.selector && String.equal a.wrapper b.wrapper);
  guard
    (not
       (List.exists
          (fun d ->
            List.exists (fun e -> String.equal (property d) (property e)) b.body)
          a.body));
  let folded =
    Array.to_list (Array.sub rules 0 k)
    @ [ { a with body = a.body @ b.body } ]
    @ Array.to_list (Array.sub rules (k + 2) (n - k - 2))
  in
  let one = sheet (Array.to_list rules) and two = sheet folded in
  let c1 = canonical one and c2 = canonical two in
  if not (String.equal c1 c2) then
    failf
      "a fold of two adjacent occurrences moves the form:\n\
      \  %S -> %S\n\
      \  %S -> %S"
      one c1 two c2

(* Each declaration as a rule of its own, in place: the same sheet. *)
let singletons rules =
  List.concat_map
    (fun r -> List.map (fun d -> { r with body = [ d ] }) r.body)
    rules

(* Neighbours under one wrapper and one selector folded into one rule. *)
let rec regroup = function
  | a :: b :: rest
    when String.equal a.wrapper b.wrapper && String.equal a.selector b.selector
    ->
      regroup ({ a with body = a.body @ b.body } :: rest)
  | r :: rest -> r :: regroup rest
  | [] -> []

(* Any sequence of moves the cascade cannot see reaches one canonical form:
   write each declaration as a rule of its own, swap neighbours that
   {!Rule_graph.rules_conflict} leaves unordered, which reads no wrapper and so
   keeps every pair a condition could order, and fold the neighbours that end up
   under one wrapper and one selector. Which occurrences of a selector fold, and
   in which order the folds come, depends on the moves; the form may not. *)
let test_independent_moves_converge rules moves =
  let rules = first 8 rules in
  let moved = Array.of_list (singletons rules) in
  let n = Array.length moved in
  guard (n >= 2);
  List.iter
    (fun k ->
      let i = k mod (n - 1) in
      let a = moved.(i) and b = moved.(i + 1) in
      if not (Rule_graph.rules_conflict (plain_rule a) (plain_rule b)) then begin
        moved.(i) <- b;
        moved.(i + 1) <- a
      end)
    moves;
  let one = sheet rules and two = sheet (regroup (Array.to_list moved)) in
  let c1 = canonical one and c2 = canonical two in
  if not (String.equal c1 c2) then
    failf "independent moves change the form:\n  %S -> %S\n  %S -> %S" one c1
      two c2

let test_run_projection_is_idempotent rules =
  let once = canonical (sheet (first 8 rules)) in
  let twice = canonical once in
  if not (String.equal once twice) then
    failf "projecting the projection changed it:\n  %S\n  %S" once twice

let suite =
  ( "rule_order",
    [
      test_case "independent rules commute"
        [ list1 run_rule; range 8 ]
        test_independent_rules_commute;
      test_case "adjacent occurrences fold"
        [ list1 run_rule; range 8 ]
        test_adjacent_occurrences_fold;
      test_case "independent moves converge"
        [ list1 run_rule; list (range 64) ]
        test_independent_moves_converge;
      test_case "run projection is idempotent"
        [ list1 run_rule ]
        test_run_projection_is_idempotent;
      test_case "layer respellings converge" [ bytes ]
        test_layer_respellings_converge;
      test_case "layer projection is idempotent" [ bytes ]
        test_layer_projection_is_idempotent;
      test_case "reversed layer order stays distinct" [ bytes ]
        test_reversed_order_stays_distinct;
    ] )
