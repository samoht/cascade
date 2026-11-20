type print = { lossless : bool; enforce_spec : bool }

let default = { lossless = false; enforce_spec = false }
let print ~lossless ~enforce_spec = { lossless; enforce_spec }

(* The constructor caches the default print's length; a flag can lengthen a
   spelling, so a flagged run measures the print it makes. *)
let decl print d =
  if print.lossless || print.enforce_spec then
    Pp.size ~minify:true ~lossless:print.lossless
      ~enforce_spec:print.enforce_spec Declaration.pp d
  else Declaration.size d

(* A top-level loop, so a cost model's measure allocates no closure. *)
let rec sum_decls print acc = function
  | [] -> acc
  | d :: rest -> sum_decls print (acc + decl print d) rest

let decls print ds = sum_decls print 0 ds
let selector s = Pp.size ~minify:true Selector.pp s

let body print (ds : Declaration.declaration list) =
  let separators =
    match ds with [] | [ _ ] -> 0 | _ :: rest -> List.length rest
  in
  2 + decls print ds + separators

let rule print (r : Stylesheet.rule) =
  selector r.Stylesheet_intf.selector
  + body print r.Stylesheet_intf.declarations

let rules print rules = List.fold_left (fun acc r -> acc + rule print r) 0 rules
