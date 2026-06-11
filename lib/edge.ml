type packed_property = Packed : 'a Properties.property -> packed_property

type t = {
  summary : Selector_summary.t;
  property : packed_property;
  important : bool;
}

let selectors sel =
  match Selector.as_list sel with Some xs -> xs | None -> [ sel ]

let of_decl summary = function
  | Declaration.Declaration { property; _ } as d ->
      Some
        {
          summary;
          property = Packed property;
          important = Declaration.is_important d;
        }
  | _ -> None

let of_rule (rule : Stylesheet.rule) =
  let summaries =
    selectors rule.Stylesheet_intf.selector
    |> List.map Selector_summary.of_selector
  in
  List.concat_map
    (fun summary ->
      List.filter_map (of_decl summary) rule.Stylesheet_intf.declarations)
    summaries

let pp_property ctx (Packed property) = Properties.pp_property ctx property

let pp ctx t =
  Pp.string ctx "{property=";
  pp_property ctx t.property;
  Pp.string ctx ";important=";
  Pp.string ctx (string_of_bool t.important);
  Pp.string ctx "}"
