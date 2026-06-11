type t = {
  same_minified_declaration : Declaration.t -> Declaration.t -> bool;
  declaration_covers : Declaration.t -> Declaration.t -> bool;
  contains_vendor_pseudo_element : Selector.t -> bool;
  rule_factor_boundary : Stylesheet.rule -> bool;
  decl_property : Declaration.t -> Declaration.prop_key;
}

type summary = {
  rule : Stylesheet.rule;
  selector_summary : Selector_summary.t Lazy.t;
}

let v ~same_minified_declaration ~declaration_covers
    ~contains_vendor_pseudo_element ~rule_factor_boundary ~decl_property =
  {
    same_minified_declaration;
    declaration_covers;
    contains_vendor_pseudo_element;
    rule_factor_boundary;
    decl_property;
  }

let summary rule ~selectors = { rule; selector_summary = selectors }
let rule summary = summary.rule

let zero_non_percentage_length (value : Values.length) =
  match value with
  | Values.Pct _ | Values.Dimension { unit = "%"; _ } -> false
  | value -> Values.length_is_zero value

let zero_box_shorthand = function
  | Declaration.Declaration
      { property = Properties.Margin; value = _ :: _ as value; important; _ }
    when List.for_all zero_non_percentage_length value ->
      Some (`Margin, important)
  | Declaration.Declaration
      { property = Properties.Padding; value = _ :: _ as value; important; _ }
    when List.for_all zero_non_percentage_length value ->
      Some (`Padding, important)
  | _ -> None

let zero_box_side = function
  | Declaration.Declaration
      { property = Properties.Margin_top; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Margin, important)
  | Declaration.Declaration
      { property = Properties.Margin_right; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Margin, important)
  | Declaration.Declaration
      { property = Properties.Margin_bottom; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Margin, important)
  | Declaration.Declaration
      { property = Properties.Margin_left; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Margin, important)
  | Declaration.Declaration
      { property = Properties.Padding_top; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Padding, important)
  | Declaration.Declaration
      { property = Properties.Padding_right; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Padding, important)
  | Declaration.Declaration
      { property = Properties.Padding_bottom; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Padding, important)
  | Declaration.Declaration
      { property = Properties.Padding_left; value; important; _ }
    when zero_non_percentage_length value ->
      Some (`Padding, important)
  | _ -> None

let same_box_kind a b =
  match (a, b) with
  | `Margin, `Margin | `Padding, `Padding -> true
  | (`Margin | `Padding), _ -> false

let same_effective_box_zero a b =
  match (zero_box_shorthand a, zero_box_side b) with
  | Some (box_a, important_a), Some (box_b, important_b) ->
      same_box_kind box_a box_b && Bool.equal important_a important_b
  | _ -> (
      match (zero_box_shorthand b, zero_box_side a) with
      | Some (box_b, important_b), Some (box_a, important_a) ->
          same_box_kind box_a box_b && Bool.equal important_a important_b
      | _ -> false)

let same_effective_declaration a b = same_effective_box_zero a b

let selectors_of_rule_selector sel =
  match Selector.as_list sel with Some xs -> xs | None -> [ sel ]

let overlap t common decls =
  List.exists
    (fun common_decl ->
      List.exists
        (fun decl ->
          (not
             (t.same_minified_declaration common_decl decl
             || same_effective_declaration common_decl decl))
          && Bool.equal
               (Declaration.is_important common_decl)
               (Declaration.is_important decl)
          && (t.declaration_covers common_decl decl
             || t.declaration_covers decl common_decl))
        decls)
    common

let selector_overlap (rule : Stylesheet.rule) target_summary =
  selectors_of_rule_selector rule.Stylesheet_intf.selector
  |> List.exists (fun selector ->
      Selector_summary.may_overlap
        (Selector_summary.of_selector selector)
        target_summary)

let specificity_strictly_greater a b =
  let a = Selector.specificity a in
  let b = Selector.specificity b in
  a.ids > b.ids
  || a.ids = b.ids
     && (a.classes > b.classes
        || (a.classes = b.classes && a.elements > b.elements))

let specificity_equal a b =
  let a = Selector.specificity a in
  let b = Selector.specificity b in
  a.ids = b.ids && a.classes = b.classes && a.elements = b.elements

let rule_specificity_beats_on_overlap (target : Stylesheet.rule)
    (skipped : Stylesheet.rule) =
  let target_selectors =
    selectors_of_rule_selector target.Stylesheet_intf.selector
  in
  let skipped_selectors =
    selectors_of_rule_selector skipped.Stylesheet_intf.selector
  in
  List.for_all
    (fun skipped_selector ->
      let skipped_summary = Selector_summary.of_selector skipped_selector in
      List.for_all
        (fun target_selector ->
          (not
             (Selector_summary.may_overlap
                (Selector_summary.of_selector target_selector)
                skipped_summary))
          || specificity_strictly_greater target_selector skipped_selector)
        target_selectors)
    skipped_selectors

let specificity_ties (target : Stylesheet.rule) (skipped : Stylesheet.rule) =
  let target_selectors =
    selectors_of_rule_selector target.Stylesheet_intf.selector
  in
  let skipped_selectors =
    selectors_of_rule_selector skipped.Stylesheet_intf.selector
  in
  List.exists
    (fun skipped_selector ->
      let skipped_summary = Selector_summary.of_selector skipped_selector in
      List.exists
        (fun target_selector ->
          Selector_summary.may_overlap
            (Selector_summary.of_selector target_selector)
            skipped_summary
          && specificity_equal target_selector skipped_selector)
        target_selectors)
    skipped_selectors

let blocks_factor t common target skipped =
  selector_overlap skipped.rule (Lazy.force target.selector_summary)
  && overlap t common skipped.rule.declarations
  && not (rule_specificity_beats_on_overlap target.rule skipped.rule)

let blocks_tie t common target skipped =
  specificity_ties target.rule skipped.rule
  && overlap t common skipped.rule.declarations

let rec nested_statements_touch_props t props stmts =
  List.exists
    (fun stmt ->
      match stmt with
      | Stylesheet.Rule r ->
          List.exists
            (fun d -> List.mem (t.decl_property d) props)
            r.declarations
          || nested_statements_touch_props t props r.nested
      | Stylesheet.Declarations ds ->
          List.exists (fun d -> List.mem (t.decl_property d) props) ds
      | Stylesheet.Bang_comment _ -> false
      | _ -> true)
    stmts

let boundary_safe_to_skip t common (rule : Stylesheet.rule) =
  rule.Stylesheet_intf.merge_key = None
  && (not (t.contains_vendor_pseudo_element rule.selector))
  && not
       (nested_statements_touch_props t
          (List.map t.decl_property common)
          rule.nested)

let can_cross t common_opt candidate =
  (not (t.rule_factor_boundary candidate))
  ||
  match common_opt with
  | Some common -> boundary_safe_to_skip t common candidate
  | None -> false
