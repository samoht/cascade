(* [cascade inline <page.html> [extra.css]]: resolve a stylesheet against the
   HTML, write every winning declaration into each element's style attribute,
   and drop the <style> blocks. cascade runs the cascade (selector match +
   specificity + !important + inline-style priority) and minifies each result;
   lambdasoup only parses and serialises the HTML. *)

open Cmdliner
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
  (* :hover, media queries, pseudo-elements, etc. are stateful or generated and
     have no inline-style form; they cannot be resolved statically. *)
  | _ -> false

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

let inlinable (sel : Sel.t) =
  let rec ok = function
    | Sel.Universal _ | Sel.Element _ | Sel.Class _ | Sel.Id _ | Sel.Attribute _
    | Sel.Root | Sel.First_child | Sel.Last_child | Sel.Only_child | Sel.Empty
      ->
        true
    | Sel.Compound ps | Sel.List ps | Sel.Is ps | Sel.Where ps | Sel.Not ps ->
        List.for_all ok ps
    | Sel.Combined (a, _, b) -> ok a && ok b
    | _ -> false
  in
  ok sel

(* ---------- the cascade (specificity + source order + !important + inline)
   ---------- *)
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

(* declarations applied in ascending cascade order; later wins per property *)
let resolved rules n =
  let matched =
    rules
    |> List.mapi (fun i r ->
        let sel = Sheet.selector r in
        if matches sel n then
          Some (spec_key (Sel.specificity sel), i, Sheet.declarations r)
        else None)
    |> List.filter_map Fun.id
    |> List.stable_sort (fun (s1, i1, _) (s2, i2, _) ->
        match compare s1 s2 with 0 -> compare i1 i2 | c -> c)
  in
  let rule_decls = List.concat_map (fun (_, _, ds) -> ds) matched in
  let inline =
    match Soup.attribute "style" n with Some s -> parse_inline s | None -> []
  in
  let normal = List.filter (fun d -> not (Decl.is_important d)) in
  let important = List.filter Decl.is_important in
  let rule_normal, rule_imp = (normal rule_decls, important rule_decls) in
  let in_normal, in_imp = (normal inline, important inline) in
  (* author cascade order, low to high: rule-normal, inline-normal,
     rule-!important, inline-!important *)
  List.fold_left upsert [] (rule_normal @ in_normal @ rule_imp @ in_imp)
  |> List.rev_map snd

let no_style =
  [ "html"; "head"; "meta"; "title"; "base"; "link"; "style"; "script" ]

(* CSS inherited properties (a conservative set: missing one only keeps a
   redundant declaration; the x-test guards against ever dropping a needed
   one). *)
let inherited =
  [
    "color";
    "font";
    "font-family";
    "font-size";
    "font-weight";
    "font-style";
    "font-variant";
    "font-stretch";
    "font-feature-settings";
    "line-height";
    "letter-spacing";
    "word-spacing";
    "text-align";
    "text-indent";
    "text-transform";
    "text-shadow";
    "white-space";
    "word-break";
    "overflow-wrap";
    "hyphens";
    "tab-size";
    "visibility";
    "cursor";
    "direction";
    "list-style";
    "list-style-type";
    "list-style-position";
    "list-style-image";
    "quotes";
    "caption-side";
    "border-collapse";
    "border-spacing";
    "empty-cells";
  ]

let is_inherited p = List.mem (String.lowercase_ascii p) inherited

let decl_value d =
  let s = Sheet.inline_style_of_declarations ~minify:true [ d ] in
  match String.index_opt s ':' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

let style_node node = function
  | [] -> ()
  | decls ->
      Soup.set_attribute "style"
        (Sheet.inline_style_of_declarations ~minify:true decls)
        node

(* top-down walk: [ctx] maps each inherited property to the value in force from
   the ancestors, so [minimal] can drop a declaration that only restates it. *)
let rec walk ~minimal rules ctx node =
  if List.mem (String.lowercase_ascii (Soup.name node)) no_style then
    List.iter (walk ~minimal rules ctx) (child_elements node)
  else begin
    let decls = resolved rules node in
    let kept, ctx' =
      List.fold_left
        (fun (kept, ctx) d ->
          let p = String.lowercase_ascii (Decl.property_name d) in
          if minimal && is_inherited p then
            let v = decl_value d in
            match List.assoc_opt p ctx with
            | Some pv when pv = v -> (kept, ctx) (* inherits the same value *)
            | _ -> (d :: kept, (p, v) :: List.remove_assoc p ctx)
          else (d :: kept, ctx))
        ([], ctx) decls
    in
    style_node node (List.rev kept);
    List.iter (walk ~minimal rules ctx') (child_elements node)
  end

let inline_html ~minimal ~html ~extra =
  let soup = Soup.parse html in
  let page_css =
    Soup.select "style" soup |> Soup.to_list |> List.concat_map Soup.texts
    |> String.concat "\n"
  in
  let css = page_css ^ "\n" ^ extra in
  let rules, dropped =
    match Css.of_string css with
    | Ok p ->
        let all = Sheet.rules p.Css.stylesheet in
        let kept = List.filter (fun r -> inlinable (Sheet.selector r)) all in
        (kept, List.length all - List.length kept)
    | Error _ -> ([], 0)
  in
  Soup.children soup |> Soup.elements |> Soup.to_list
  |> List.iter (walk ~minimal rules []);
  (* drop all <style> blocks: the CSS now lives on the elements *)
  Soup.select "style" soup |> Soup.iter Soup.delete;
  (Soup.to_string soup, dropped)

(* ---------- cmdliner ---------- *)
let read_file path =
  try
    let ic = open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Ok s
  with Sys_error msg -> Error (`Msg (Fmt.str "Error reading %s: %s" path msg))

let run minimal html_file css_file () =
  match read_file html_file with
  | Error _ as e -> e
  | Ok html ->
      let extra =
        match css_file with
        | Some f -> ( match read_file f with Ok s -> s | Error _ -> "")
        | None -> ""
      in
      let out, dropped = inline_html ~minimal ~html ~extra in
      if dropped > 0 then
        Fmt.epr
          "Warning: %d non-inlinable rule(s) dropped (e.g. :hover, media \
           queries)@."
          dropped;
      print_string out;
      Ok ()

let html_arg =
  let doc = "The HTML page to resolve." in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"PAGE.html" ~doc)

let css_arg =
  let doc =
    "An additional stylesheet to apply on top of the page's <style> blocks."
  in
  Arg.(value & pos 1 (some file) None & info [] ~docv:"EXTRA.css" ~doc)

let minimal_arg =
  let doc =
    "Drop inherited declarations that only restate the parent's value, for the \
     smallest styled page. By default every matched declaration is kept."
  in
  Arg.(value & flag & info [ "minimal" ] ~doc)

let term = Term.(const run $ minimal_arg $ html_arg $ css_arg $ const ())

let cmd =
  let doc = "Resolve a stylesheet into an HTML page's inline styles" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Project the cascade onto every element of $(i,PAGE.html): selector \
         matching, specificity, !important and inline-style priority decide \
         which declarations win, and the result is written into each element's \
         $(b,style) attribute. The $(b,<style>) blocks are removed, so the \
         output is fully resolved HTML.";
      `P
        "Rules that cannot be expressed as an inline style (e.g. $(b,:hover), \
         media queries, pseudo-elements) are dropped with a warning.";
    ]
  in
  Cmd.v (Cmd.info "inline" ~doc ~man) Term.(term_result term)
