let decls ds =
  let pp ctx ds = List.iter (Declaration.pp_declaration ctx) ds in
  Pp.size ~minify:true pp ds

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
let decl_list decl_bytes decl_count = decl_bytes + max 0 (decl_count - 1)

let rule_from_parts selector_bytes decl_bytes decl_count =
  selector_bytes + 2 + decl_list decl_bytes decl_count
