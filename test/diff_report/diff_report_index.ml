open Cascade
module C = Cascade_diff.Css_compare

let contains haystack needle =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n = 0 || at 0

(* ===== Reading a stylesheet the way a report names it ===== *)

(* The report spells a value with [Css.declaration_value ~minify:false] and the
   [!important] flag, and a selector with [Css.Selector.to_string]. An index
   built the same way answers whether a claim about a sheet is true of it. Both
   are serialisers, not the code under test. *)
let shown_value decl =
  let v = Css.declaration_value ~minify:false decl in
  if Css.declaration_is_important decl then
    String.concat "" [ v; " !important" ]
  else v

type rule_entry = { selector : string; props : (string * string) list }

(* A statement no selector names is named by its text up to the block, as the
   report names it. *)
let head stmt =
  let text = String.trim (Css.to_string ~minify:true (Css.v [ stmt ])) in
  match String.index_opt text '{' with
  | Some i -> String.trim (String.sub text 0 i)
  | None -> text

let block_of stmt =
  match Css.as_rule stmt with
  | Some (_, _, nested) -> nested
  | None -> (
      match Css.as_layer stmt with
      | Some (_, block) -> block
      | None -> (
          match Css.as_media stmt with
          | Some (_, block) -> block
          | None -> (
              match Css.as_container stmt with
              | Some (_, _, block) -> block
              | None -> (
                  match Css.as_supports stmt with
                  | Some (_, block) -> block
                  | None -> (
                      match Css.as_origin stmt with
                      | Some (_, block) -> block
                      | None -> [])))))

let rec heads_of stmts =
  List.concat_map (fun stmt -> head stmt :: heads_of (block_of stmt)) stmts

type sheet_index = {
  rules : rule_entry list;
  heads : string list;
  source : string;  (** the file as written, for a head the printer drops *)
}

(* [Css.map] reaches every rule at any depth, depth-first, so the index is the
   sheet's rules in document order however they are nested. *)
let rule_index sheet =
  let acc = ref [] in
  ignore
    (Css.map
       (fun selector decls ->
         acc :=
           {
             selector = Css.Selector.to_string selector;
             props =
               List.map (fun d -> (Css.declaration_name d, shown_value d)) decls;
           }
           :: !acc;
         Css.rule ~selector decls)
       (Css.statements sheet));
  List.rev !acc

(* A frame of [@keyframes] is not a rule, and the report names it by its
   keyframe selector, so the index carries one entry per frame. *)
let frames_of sheet =
  let acc = ref [] in
  Css.Stylesheet.iter_statements
    (fun stmt ->
      match Css.as_keyframes stmt with
      | None -> ()
      | Some (_, frames) ->
          List.iter
            (fun (f : Css.keyframe) ->
              acc :=
                {
                  selector = Css.Keyframe.to_string f.selector;
                  props =
                    List.map
                      (fun d -> (Css.declaration_name d, shown_value d))
                      f.declarations;
                }
                :: !acc)
            frames)
    (Css.statements sheet);
  List.rev !acc

(* Every statement the sheet holds, whatever at-rule nests it, named the way a
   report with no selector to show names it. *)
let all_heads sheet =
  let acc = ref [] in
  Css.Stylesheet.iter_statements
    (fun stmt -> acc := head stmt :: !acc)
    (Css.statements sheet);
  List.rev !acc

let index ~source sheet =
  {
    rules = rule_index sheet @ frames_of sheet;
    heads = heads_of (Css.statements sheet) @ all_heads sheet;
    source;
  }

let parse text =
  match Css.of_string ~strict:false text with
  | Ok { stylesheet; _ } -> Some stylesheet
  | Error _ -> None
  | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) -> None

let index_of_text text = Option.map (index ~source:text) (parse text)

(* Compared without whitespace: a spelling difference in the layout is not a
   difference in what is named. *)
let tight s =
  String.concat ""
    (List.filter_map
       (fun c ->
         if Char.equal c ' ' || Char.equal c '\n' || Char.equal c '\t' then None
         else Some (String.make 1 c))
       (List.init (String.length s) (String.get s)))

(* Mode [`Canonical] compares two generated canonical forms, and the tree it
   reports describes those, not the files as written: a value the author spelled
   [blue] is named [#00f]. A claim about that pair is checked against the file
   and its canonical form together, so the check still catches a name in neither
   and does not fail on the projection's own spelling. *)
let union a b =
  {
    rules = a.rules @ b.rules;
    heads = a.heads @ b.heads;
    source = String.concat "\n" [ a.source; b.source ];
  }

let index_for (mode : C.mode) text =
  match (mode, index_of_text text) with
  | `Canonical, Some idx -> (
      match parse text with
      | None -> Some idx
      | Some sheet -> (
          match Css.to_string ~minify:true (Css.optimize sheet) with
          | canonical -> (
              match index_of_text canonical with
              | None -> Some idx
              | Some c -> Some (union idx c))
          | exception (Reader.Parse_error _ | Failure _ | Invalid_argument _) ->
              Some idx))
  | (`Auto | `Tree | `String | `Canonical), idx -> idx

(* An at-rule with no selector is named by its own text. [@charset] is dropped
   by the printer, so the index cannot hold its head; the file it came from
   still does, and that is the claim being checked. *)
let has_pair (s, p) l =
  List.exists (fun (s', p') -> String.equal s s' && String.equal p p') l

let has_selector idx s =
  List.exists (fun e -> String.equal e.selector s) idx.rules
  || List.exists (String.equal s) idx.heads
  || String.length s > 0
     && Char.equal s.[0] '@'
     && contains (tight idx.source) (tight s)

let has_decl idx s p v =
  List.exists
    (fun e ->
      String.equal e.selector s
      && List.exists
           (fun (p', v') -> String.equal p p' && String.equal v v')
           e.props)
    idx.rules
