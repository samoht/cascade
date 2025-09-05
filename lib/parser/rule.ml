(** CSS rule parser using simple Reader API. *)

open Css.Reader

(** Parse a CSS rule (selector + declarations) or at-rule *)
let rec one t =
  ws t;
  (* Check if it's an at-rule *)
  if peek t = Some '@' then at_rule t
  else
    (* Try to parse selector *)
    match Css.Selector.read_opt t with
    | None -> None
    | Some selector ->
        ws t;
        (* Parse declaration block *)
        let _declarations = Declaration.block t in

        (* ARCHITECTURAL LIMITATION: Creating placeholder rule with empty
           declarations because full declaration processing requires CSS type
           conversion which creates circular dependency. This means parsed CSS
           rules lose their declaration content. *)
        Some (Css.Rule (Css.rule ~selector []))

(** Parse multiple CSS rules (including at-rules) *)
and rules t =
  ws t;
  if is_done t then []
  else if peek t = Some '}' then
    (* Stop at closing brace *)
    []
  else
    match one t with
    | None -> [] (* Stop if we can't parse a rule *)
    | Some r -> r :: rules t

(** Parse only regular CSS rules (no at-rules) *)
and rules_only t =
  ws t;
  if is_done t then []
  else if peek t = Some '}' then
    (* Stop at closing brace *)
    []
  else
    (* Try to parse a style rule without at-rules *)
    match Css.Selector.read_opt t with
    | None -> []
    | Some selector ->
        ws t;
        (* Parse declaration block *)
        let _declarations = Declaration.block t in
        (* Create a temporary rule with empty declarations *)
        (* The actual conversion to typed declarations happens in css_parser.ml *)
        let rule = Css.rule ~selector [] in
        rule :: rules_only t

(** Parse @media rule *)
and media_rule t =
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
and layer_rule t =
  expect_string t "@layer";
  ws t;
  (* Parse layer name until opening brace or semicolon *)
  let name = String.trim (while_ t (fun c -> c <> '{' && c <> ';')) in
  ws t;
  peek t |> function
  | Some '{' ->
      skip t;
      let content = rules t in
      ws t;
      expect t '}';
      (* Separate sheet items into categories for layer *)
      let nested_rules =
        List.filter_map
          (function Css.Rule r -> Some (Css.rule_to_nested r) | _ -> None)
          content
      in
      let media_queries =
        List.filter_map (function Css.Media m -> Some m | _ -> None) content
      in
      let supports_queries =
        List.filter_map
          (function Css.Supports s -> Some s | _ -> None)
          content
      in
      let container_queries =
        List.filter_map
          (function Css.Container c -> Some c | _ -> None)
          content
      in
      Css.Layer
        (Css.layer ~name nested_rules ~media:media_queries
           ~container:container_queries ~supports:supports_queries)
  | Some ';' ->
      skip t;
      (* Layer declaration without rules *)
      Css.Layer (Css.layer ~name [] ~media:[] ~container:[] ~supports:[])
  | _ -> failwith "Invalid @layer rule"

(** Parse @supports rule *)
and supports_rule t =
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
and at_rule t =
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
