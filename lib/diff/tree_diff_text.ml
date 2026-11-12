(* How statements and declarations read in the report and in the keys the
   comparison matches on. *)

open Cascade

let add_strings buf ls = List.iter (Buffer.add_string buf) ls

let decl_with_important decl value =
  if Css.declaration_is_important decl then
    String.concat "" [ value; " !important" ]
  else value

(* The one answer to how a declaration reads in the report: the author's own
   spelling, which keeps a unit difference like [0px] against [0] visible, and
   the flag, without which a declaration and the one it outranks read alike. *)
let decl_shown_value decl =
  decl_with_important decl (Css.declaration_value ~minify:false decl)

(* The whole of the [@layer] statement form. The semicolon is what tells a
   layer-order pin from the block of the same name: [@layer a;] ahead of [@layer
   a { ... }] is a second statement, not the block again. *)
let layer_decl_text names =
  String.concat ""
    [
      "@layer ";
      String.concat ", " (List.map Css.Stylesheet.string_of_layer_name names);
      ";";
    ]

(* The statements that carry neither a selector nor a block. Naming them apart
   also keeps them apart in the order keys, where one shared "(other statement)"
   made a [@charset] and a [@namespace] the same statement. *)
let describe_prelude_statement (s : Css.statement) =
  match s with
  | Charset encoding ->
      Some (String.concat "" [ "@charset \""; encoding; "\";" ])
  | Namespace (prefix, _) ->
      Some
        (String.concat ""
           [
             "@namespace";
             (match prefix with
             | Some p -> String.concat "" [ " "; p ]
             | None -> "");
             ";";
           ])
  | Layer_decl names -> Some (layer_decl_text names)
  | _ -> None

(* A statement's own text, split at its block: the head names it, the body is
   what a descriptor-only at-rule is compared on. *)
let statement_text_split stmt =
  let text = Css.Stylesheet.to_string ~minify:true (Css.v [ stmt ]) in
  match String.index_opt text '{' with
  | None -> (String.trim text, "")
  | Some i ->
      let head = String.sub text 0 i in
      let last = String.length text - 1 in
      let body =
        if last > i && text.[last] = '}' then
          String.sub text (i + 1) (last - i - 1)
        else String.sub text (i + 1) (last - i)
      in
      (String.trim head, body)

(* The head of an [@layer]. *)
let layer_block_head name =
  match name with
  | Some name ->
      String.concat "" [ "@layer "; Css.Stylesheet.string_of_layer_name name ]
  | None -> "@layer"

let describe_statement stmt =
  let try_desc f = f stmt in
  let matchers =
    [
      (fun s ->
        Option.map (fun (s, _, _) -> Css.Selector.to_string s) (Css.as_rule s));
      (fun s ->
        Option.map
          (fun (c, _) -> String.concat "" [ "@media "; Css.Media.to_string c ])
          (Css.as_media s));
      (fun s -> Option.map (fun (n, _) -> layer_block_head n) (Css.as_layer s));
      (fun s ->
        Option.map
          (fun (n, c, _) ->
            let prefix =
              match n with Some n -> String.concat "" [ n; " " ] | None -> ""
            in
            let cond_str =
              match c with Some c -> Css.Container.to_string c | None -> ""
            in
            String.concat "" [ "@container "; prefix; cond_str ])
          (Css.as_container s));
      (fun s ->
        Option.map
          (fun (c, _) ->
            String.concat "" [ "@supports "; Css.Supports.to_string c ])
          (Css.as_supports s));
      (fun s -> Option.map (fun _ -> "@property") (Css.as_property s));
      (fun s ->
        Option.map
          (fun (name, _) -> String.concat "" [ "@keyframes "; name ])
          (Css.as_keyframes s));
      (fun s -> Option.map (fun _ -> "@font-face") (Css.as_font_face s));
      describe_prelude_statement;
    ]
  in
  match List.find_map try_desc matchers with
  | Some desc -> Some desc
  (* Everything left names itself by the head it prints to, for the same reason
     [@charset] and [@namespace] are named apart above: one shared description
     is one shared order key, and [@page] swapped with [@starting-style] then
     reads as no change at all. *)
  | None -> (
      match fst (statement_text_split stmt) with
      | "" -> Some "(other statement)"
      | head -> Some head)
