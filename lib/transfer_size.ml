(* The constants, in thousandths of a byte, are least-squares fits of what gzip
   -9 saves on the cascade output of the 504 SatCSS sheets, ref.css and
   tw_all.css when one token is deleted: a new token, 2.42 + 0.061 per byte; one
   run of declarations printed in that order earlier, 1.32 + 0.005 per byte; one
   such run of selector branches, 1.47 + 0.029 per byte. A run's cost hardly
   grows with its length, as an LZ77 match's does not. Brotli -q 11 agrees in
   direction: about 2.9 for a new token, 1.7 for a declaration run and 3.2 for a
   selector run. *)
let per_byte = 1000
let novel = 2420
let novel_byte = 61
let declaration_match = 1320
let declaration_match_byte = 5
let selector_match = 1470
let selector_match_byte = 29

type token = Branch of Selector.t | Declaration of Declaration.declaration

(* A token's identity is its minified text. *)
module Seen = Hashtbl.Make (struct
  type t = token

  let equal a b =
    match (a, b) with
    | Branch a, Branch b -> Selector.equal a b
    | Declaration a, Declaration b -> Declaration.same_minified a b
    | Branch _, Declaration _ | Declaration _, Branch _ -> false

  let hash = function
    | Branch selector -> Selector.hash selector
    | Declaration declaration -> Declaration.hash declaration
end)

(* Where each token was last printed, by its rank among the tokens read. *)
type walk = { seen : int Seen.t; mutable next : int; mutable cost : int }

(* The tokens of one selector list or one body, printed together. A token
   printed before starts a match, unless the token before it was printed before
   too, right ahead of where this one was: then the match goes on. [previous] is
   the rank the token before was last printed at, when it was printed before. *)
let group walk ~match_cost ~match_byte tokens =
  ignore
    (List.fold_left
       (fun previous (token, size) ->
         let bytes = size + 1 in
         let rank = walk.next in
         walk.next <- rank + 1;
         let earlier = Seen.find_opt walk.seen token in
         Seen.replace walk.seen token rank;
         match earlier with
         | Option.Some earlier ->
             let start =
               match previous with
               | Option.Some previous when earlier = previous + 1 -> 0
               | Option.Some _ | Option.None -> match_cost
             in
             walk.cost <- walk.cost + start + (match_byte * bytes);
             Option.Some earlier
         | Option.None ->
             walk.cost <- walk.cost + novel + (novel_byte * bytes);
             Option.None)
       Option.None tokens)

let rules print (rules : Stylesheet.rule list) =
  let walk = { seen = Seen.create 16; next = 0; cost = 0 } in
  List.iter
    (fun (rule : Stylesheet.rule) ->
      group walk ~match_cost:selector_match ~match_byte:selector_match_byte
        (List.map
           (fun branch -> (Branch branch, Size.selector branch))
           (Edge.selectors rule.selector));
      group walk ~match_cost:declaration_match
        ~match_byte:declaration_match_byte
        (List.map
           (fun declaration ->
             (Declaration declaration, Size.decl print declaration))
           rule.declarations))
    rules;
  walk.cost
