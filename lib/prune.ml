(* Remove the rules a set of documents cannot use. The matching is {!Resolve};
   this decides what a No_match may be read as, which is the whole of the
   analysis: [Unsupported] is not a No_match, so a selector with no model is
   kept and never counted against the stylesheet. *)

type verdict = Unused | Used of int | Unmodelled
type entry = { selector : Selector.t; verdict : verdict }
type analysis = { sheet : Stylesheet.t; entries : entry list; elements : int }

module Make (Node : Resolve.NODE) = struct
  module R = Resolve.Make (Node)

  let rec collect acc node =
    List.fold_left collect (node :: acc) (Node.children node)

  let matched elements sel =
    List.fold_left
      (fun n e ->
        match R.match_selector sel e with
        | Resolve.Matches -> n + 1
        | Resolve.No_match | Resolve.Unsupported -> n)
      0 elements

  (* [supported] settles the question before any element is looked at, which is
     what keeps [Unused] a statement about the documents: were the count taken
     first, a selector the matcher cannot decide would come back at zero and
     read as ruled out. *)
  let judge elements (r : Stylesheet.rule) =
    match r.nested with
    | _ :: _ -> Unmodelled
    | [] -> (
        if not (Resolve.supported r.selector) then Unmodelled
        else match matched elements r.selector with 0 -> Unused | n -> Used n)

  (* Selectors 4 sec. 3.3: a selector list is a set of selectors, each matching
     on its own and carrying its own specificity (sec. 17), so a branch nothing
     matches goes while the rest of the rule stays. A list is supported only
     when every branch is, so every branch reached here is modelled. *)
  let live_branches elements sel =
    match Selector.as_list sel with
    | None -> None
    | Some branches -> (
        match List.filter (fun b -> matched elements b > 0) branches with
        | kept when List.compare_lengths kept branches = 0 -> None
        | [] -> None
        | [ one ] -> Some one
        | kept -> Some (Selector.list kept))

  let rec block elements acc stmts =
    let acc, out =
      List.fold_left
        (fun (acc, out) stmt ->
          match statement elements acc stmt with
          | acc, None -> (acc, out)
          | acc, Some s -> (acc, s :: out))
        (acc, []) stmts
    in
    (acc, List.rev out)

  and statement elements acc stmt =
    (* A group block left with nothing goes with its rules: the condition it
       carries guards no declaration any more. [@layer] is the exception, since
       an empty one still declares a layer the cascade orders by (css-cascade-5
       sec. 6.4.4). *)
    let group acc body rebuild =
      let acc, pruned = block elements acc body in
      match (body, pruned) with
      | _ :: _, [] -> (acc, None)
      | _ -> (acc, Some (rebuild pruned))
    in
    match stmt with
    | Stylesheet.Rule r ->
        let selector = r.selector in
        let verdict = judge elements r in
        let acc = { selector; verdict } :: acc in
        let kept =
          match verdict with
          | Unused -> None
          | Unmodelled -> Some stmt
          | Used _ -> (
              match live_branches elements selector with
              | None -> Some stmt
              | Some selector -> Some (Stylesheet.Rule { r with selector }))
        in
        (acc, kept)
    | Stylesheet.Layer (name, body) ->
        let acc, pruned = block elements acc body in
        (acc, Some (Stylesheet.Layer (name, pruned)))
    | Stylesheet.Media (query, body) ->
        group acc body (fun b -> Stylesheet.Media (query, b))
    | Stylesheet.Supports (cond, body) ->
        group acc body (fun b -> Stylesheet.Supports (cond, b))
    | Stylesheet.Container (name, cond, body) ->
        group acc body (fun b -> Stylesheet.Container (name, cond, b))
    | Stylesheet.Moz_document (conds, body) ->
        group acc body (fun b -> Stylesheet.Moz_document (conds, b))
    | Stylesheet.Starting_style body ->
        group acc body (fun b -> Stylesheet.Starting_style b)
    | Stylesheet.When (cond, body) ->
        group acc body (fun b -> Stylesheet.When (cond, b))
    | Stylesheet.Else (cond, body) ->
        group acc body (fun b -> Stylesheet.Else (cond, b))
    | Stylesheet.Scope (root, limit, body) ->
        (* The scoping root is not judged: css-cascade-6 puts the elements in
           scope behind a proximity criterion the matcher does not model, so a
           root that matches nothing here says nothing about the rules
           inside. *)
        group acc body (fun b -> Stylesheet.Scope (root, limit, b))
    | Stylesheet.Origin (origin, body) ->
        group acc body (fun b -> Stylesheet.Origin (origin, b))
    (* Nothing here names an element, so no document can rule it out. *)
    | Stylesheet.Declarations _ | Stylesheet.Bang_comment _
    | Stylesheet.Charset _ | Stylesheet.Import _ | Stylesheet.Namespace _
    | Stylesheet.Property _ | Stylesheet.Layer_decl _
    | Stylesheet.Supports_condition _ | Stylesheet.Keyframes _
    | Stylesheet.Webkit_keyframes _ | Stylesheet.Moz_keyframes _
    | Stylesheet.Font_face _ | Stylesheet.Counter_style _ | Stylesheet.Page _
    | Stylesheet.Page_with_margins _ | Stylesheet.Font_palette_values _
    | Stylesheet.Font_feature_values _ | Stylesheet.View_transition _
    | Stylesheet.Position_try _ | Stylesheet.Viewport _
    | Stylesheet.Unknown_at_rule _ ->
        (acc, Some stmt)

  let analyse ~sheet roots =
    let elements = List.fold_left collect [] roots in
    let entries, sheet = block elements [] (Flatten.block sheet) in
    { sheet; entries = List.rev entries; elements = List.length elements }
end
