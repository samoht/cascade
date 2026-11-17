let decls ds = List.fold_left (fun acc d -> acc + Declaration.size d) 0 ds

let rule (r : Stylesheet.rule) =
  let sel = Pp.size ~minify:true Selector.pp r.Stylesheet_intf.selector in
  let decls = decls r.Stylesheet_intf.declarations in
  let separators =
    match r.Stylesheet_intf.declarations with
    | [] | [ _ ] -> 0
    | _ :: rest -> List.length rest
  in
  sel + 2 + decls + separators

let rules rules = List.fold_left (fun acc r -> acc + rule r) 0 rules
