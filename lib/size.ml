let decls ds = List.fold_left (fun acc d -> acc + Declaration.size d) 0 ds
let selector s = Pp.size ~minify:true Selector.pp s

let body (ds : Declaration.declaration list) =
  let separators =
    match ds with [] | [ _ ] -> 0 | _ :: rest -> List.length rest
  in
  2 + decls ds + separators

let rule (r : Stylesheet.rule) =
  selector r.Stylesheet_intf.selector + body r.Stylesheet_intf.declarations

let rules rules = List.fold_left (fun acc r -> acc + rule r) 0 rules
