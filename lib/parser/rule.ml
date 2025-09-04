(** CSS rule parser using simple Reader API. *)

open Reader

(** Parse a CSS rule (selector + declarations) *)
let one t =
  try
    ws t;
    (* Try to parse selector *)
    match Selector.one_opt t with
    | None -> None
    | Some selector ->
        ws t;
        (* Parse declaration block *)
        let declarations = Declaration.block t in

        (* Convert to Css.t *)
        let properties =
          List.map
            (fun (name, value, important) ->
              if important then Css.important (Css.custom_property name value)
              else Css.custom_property name value)
            declarations
        in

        Some (Css.Rule (Css.rule ~selector properties))
  with Failure _ | Invalid_argument _ -> None

(** Parse multiple CSS rules (including at-rules) *)
let rec rules t =
  ws t;
  if is_done t then []
  else
    match one t with
    | None -> [] (* Stop if we can't parse a rule *)
    | Some r -> r :: rules t

(** Parse only regular CSS rules (no at-rules) *)
let rec rules_only t =
  ws t;
  if is_done t then []
  else
    match one t with
    | Some (Css.Rule r) -> r :: rules_only t
    | _ -> rules_only t (* Skip non-rule items *)

(** Parse @media rule *)
let media_rule t =
  expect_string t "@media";
  ws t;
  (* Parse media query until opening brace *)
  let condition = until t '{' in
  ws t;
  expect t '{';
  let content = rules_only t in
  ws t;
  expect t '}';
  Css.Media (Css.media ~condition content)

(** Parse @layer rule *)
let layer_rule t =
  expect_string t "@layer";
  ws t;
  (* Parse layer name until opening brace or semicolon *)
  let name = while_ t (fun c -> c <> '{' && c <> ';') in
  ws t;
  peek t |> function
  | Some '{' ->
      skip t;
      let content = rules_only t in
      ws t;
      expect t '}';
      let nested_content = List.map Css.rule_to_nested content in
      Css.Layer (Css.layer ~name nested_content ~media:[] ~container:[])
  | Some ';' ->
      skip t;
      (* Layer declaration without rules *)
      Css.Layer (Css.layer ~name [] ~media:[] ~container:[])
  | _ -> failwith "Invalid @layer rule"

(** Parse @supports rule *)
let supports_rule t =
  expect_string t "@supports";
  ws t;
  (* Parse supports condition until opening brace *)
  let condition = until t '{' in
  ws t;
  expect t '{';
  let content = rules_only t in
  ws t;
  expect t '}';
  Css.Supports (Css.supports ~condition content)

(** Parse any at-rule *)
let at_rule t =
  ws t;
  peek t |> function
  | Some '@' -> (
      (* Look ahead to determine which at-rule *)
      save t;
      skip t;
      (* skip @ *)
      let rule_name =
        while_ t (fun c ->
            c <> ' ' && c <> '\t' && c <> '\n' && c <> '(' && c <> '{')
      in
      restore t;

      (* Parse based on rule name *)
      match rule_name with
      | "media" -> Some (media_rule t)
      | "layer" -> Some (layer_rule t)
      | "supports" -> Some (supports_rule t)
      | _ -> (
          (* Unknown at-rule, skip it *)
          skip t;
          (* @ *)
          let _ = while_ t (fun c -> c <> ';' && c <> '{') in
          peek t |> function
          | Some ';' ->
              skip t;
              None
          | Some '{' ->
              skip t;
              let _ = rules t in
              expect t '}';
              None
          | _ -> None))
  | _ -> None

(** Parse a complete stylesheet *)
let stylesheet t =
  let rec parse_items acc =
    ws t;
    if is_done t then List.rev acc
    else
      peek t |> function
      | Some '@' -> (
          (* Try to parse at-rule *)
          match at_rule t with
          | Some item -> parse_items (item :: acc)
          | None -> parse_items acc)
      | _ -> (
          (* Try to parse regular rule *)
          match one t with
          | Some item -> parse_items (item :: acc)
          | None ->
              (* Skip any unparseable content *)
              if not (is_done t) then (
                skip t;
                parse_items acc)
              else List.rev acc)
  in
  Css.stylesheet (parse_items [])
