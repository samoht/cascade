module P = Rule_pool

let same_declarations (a : Stylesheet.rule) (b : Stylesheet.rule) =
  a.declarations = b.declarations

(* Union two selectors into a flat comma list (no nesting), so a run of merges
   yields [a, b, c] rather than [[a, b], c]. *)
let union_selectors (a : Stylesheet.rule) (b : Stylesheet.rule) :
    Stylesheet.rule =
  let parts (s : Selector.t) =
    match Selector.as_list s with Some xs -> xs | None -> [ s ]
  in
  { a with selector = Selector.list (parts a.selector @ parts b.selector) }

let combine_identical rules =
  let pool = P.of_rules rules in
  (* Sweep left to right; while the focus and its successor share a declaration
     block, fold the successor in and re-examine the focus against its new
     successor. Each fold removes one rule, so the sweep is linear. *)
  let rec sweep = function
    | None -> ()
    | Some n -> (
        match P.next n with
        | Some m when same_declarations (P.rule n) (P.rule m) ->
            ignore (P.combine pool n m union_selectors);
            sweep (Some n)
        | _ -> sweep (P.next n))
  in
  (match P.nodes pool with [] -> () | first :: _ -> sweep (Some first));
  P.to_rules pool
