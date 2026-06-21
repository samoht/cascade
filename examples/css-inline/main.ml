(* cascade "inline" mode: HTML page + CSS -> fully resolved HTML. Parse HTML
   (lambdasoup), run cascade's selector match + specificity cascade over every
   element, and write the resolved declarations into style="". cascade is the
   cascade engine; lambdasoup only does HTML parse/serialise. *)

module Sel = Cascade.Selector
module Sheet = Cascade.Stylesheet
module Decl = Cascade.Declaration
module Css = Cascade.Css

(* ---------- selector matching over a lambdasoup element node ---------- *)
let ci a b = String.lowercase_ascii a = String.lowercase_ascii b
let words s = String.split_on_char ' ' s |> List.filter (( <> ) "")

let contains hay needle =
  let lh = String.length hay and ln = String.length needle in
  if ln = 0 then true
  else
    let rec go i =
      if i + ln > lh then false
      else if String.sub hay i ln = needle then true
      else go (i + 1)
    in
    go 0

let starts s p =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let ends s p =
  let ls = String.length s and lp = String.length p in
  ls >= lp && String.sub s (ls - lp) lp = p

let attr_key : Sel.attr_name -> string = function
  | Sel.Regular s -> s
  | Sel.Data s -> "data-" ^ s
  | Sel.Aria a -> Cascade.Aria.to_string a

let attr_matches n name (m : Sel.attribute_match) =
  match Soup.attribute (attr_key name) n with
  | None -> false
  | Some v -> (
      match m with
      | Sel.Presence -> true
      | Sel.Exact s | Sel.Exact_quoted (s, _) -> v = s
      | Sel.Whitespace_list s | Sel.Whitespace_list_quoted (s, _) ->
          List.mem s (words v)
      | Sel.Prefix s | Sel.Prefix_quoted (s, _) -> s <> "" && starts v s
      | Sel.Suffix s | Sel.Suffix_quoted (s, _) -> s <> "" && ends v s
      | Sel.Substring s | Sel.Substring_quoted (s, _) -> s <> "" && contains v s
      | Sel.Hyphen_list s | Sel.Hyphen_list_quoted (s, _) ->
          v = s || starts v (s ^ "-"))

let child_elements p = Soup.children p |> Soup.elements |> Soup.to_list

let preceding_siblings n =
  match Soup.parent n with
  | None -> []
  | Some p ->
      let rec before acc = function
        | [] -> List.rev acc
        | x :: _ when x == n -> List.rev acc
        | x :: rest -> before (x :: acc) rest
      in
      before [] (child_elements p)

let imm_pred n =
  match List.rev (preceding_siblings n) with x :: _ -> Some x | [] -> None

let is_first n = preceding_siblings n = []

let is_last n =
  match Soup.parent n with
  | None -> true
  | Some p -> (
      match List.rev (child_elements p) with x :: _ -> x == n | [] -> true)

let rec matches (sel : Sel.t) n : bool =
  match sel with
  | Sel.Universal _ -> true
  | Sel.Element (_, name) -> ci (Soup.name n) name
  | Sel.Class c -> List.mem c (Soup.classes n)
  | Sel.Id i -> Soup.id n = Some i
  | Sel.Attribute (_, name, m, _) -> attr_matches n name m
  | Sel.Compound ps -> List.for_all (fun p -> matches p n) ps
  | Sel.List ss | Sel.Is ss | Sel.Where ss ->
      List.exists (fun s -> matches s n) ss
  | Sel.Not ss -> not (List.exists (fun s -> matches s n) ss)
  | Sel.Combined (left, comb, right) ->
      matches right n && combinator left comb n
  | Sel.Root -> Soup.parent n = None
  | Sel.First_child -> is_first n
  | Sel.Last_child -> is_last n
  | Sel.Only_child -> is_first n && is_last n
  | Sel.Empty -> child_elements n = []
  | _ -> false (* dynamic pseudo / pseudo-elements: not statically inlinable *)

and combinator left comb n =
  match comb with
  | Sel.Descendant ->
      List.exists (matches left) (Soup.ancestors n |> Soup.to_list)
  | Sel.Child -> (
      match Soup.parent n with Some p -> matches left p | None -> false)
  | Sel.Next_sibling -> (
      match imm_pred n with Some s -> matches left s | None -> false)
  | Sel.Subsequent_sibling -> List.exists (matches left) (preceding_siblings n)
  | _ -> false

(* ---------- the cascade ---------- *)
let spec_key s = Sel.(s.ids, s.classes, s.elements)

let upsert acc d =
  let k = Decl.property_name d in
  (k, d) :: List.remove_assoc k acc

let parse_inline s =
  match Css.of_string ("a{" ^ s ^ "}") with
  | Ok p -> (
      match Sheet.rules p.Css.stylesheet with
      | r :: _ -> Sheet.declarations r
      | [] -> [])
  | Error _ -> []

let resolved_style sheet n =
  let matched =
    Sheet.rules sheet
    |> List.mapi (fun i r ->
        let sel = Sheet.selector r in
        if matches sel n then
          Some (spec_key (Sel.specificity sel), i, Sheet.declarations r)
        else None)
    |> List.filter_map Fun.id
    |> List.stable_sort (fun (s1, i1, _) (s2, i2, _) ->
        match compare s1 s2 with 0 -> compare i1 i2 | c -> c)
  in
  let existing =
    match Soup.attribute "style" n with Some s -> parse_inline s | None -> []
  in
  (* author inline style is highest priority, applied last *)
  let final =
    List.fold_left
      (fun acc (_, _, ds) -> List.fold_left upsert acc ds)
      [] matched
    |> fun acc -> List.fold_left upsert acc existing
  in
  List.rev_map snd final

(* ---------- driver ---------- *)
let () =
  let html = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let extra =
    if Array.length Sys.argv > 2 then
      In_channel.with_open_bin Sys.argv.(2) In_channel.input_all
    else ""
  in
  let soup = Soup.parse html in
  let page_css =
    Soup.select "style" soup |> Soup.to_list |> List.concat_map Soup.texts
    |> String.concat "\n"
  in
  let css = page_css ^ "\n" ^ extra in
  let sheet =
    match Css.of_string css with
    | Ok p -> p.Css.stylesheet
    | Error _ -> Sheet.empty
  in
  Soup.descendants soup |> Soup.elements
  |> Soup.iter (fun n ->
      match (ci (Soup.name n) "style", resolved_style sheet n) with
      | true, _ | _, [] -> ()
      | false, decls ->
          Soup.set_attribute "style"
            (Sheet.inline_style_of_declarations ~minify:true decls)
            n);
  print_string (Soup.to_string soup)
