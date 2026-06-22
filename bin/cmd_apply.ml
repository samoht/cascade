(* [cascade apply <page.html> [extra.css]]: resolve a stylesheet against the
   HTML, write each element's winning declarations into its style attribute, and
   keep the rules with no inline form (:hover, @media, ...) in a <style>. The
   cascade itself lives in {!Cascade.Resolve}; this command adds the
   inline-specific policy and the lambdasoup glue. *)

open Cmdliner
module Sheet = Cascade.Stylesheet
module Sel = Cascade.Selector
module Decl = Cascade.Declaration
module Css = Cascade.Css

(* ---------- the tree: lambdasoup element nodes ---------- *)
module Node = struct
  type t = Soup.element Soup.node

  let equal = ( == )
  let name n = Some (Soup.name n)
  let id = Soup.id
  let classes = Soup.classes
  let attribute n k = Soup.attribute k n
  let parent = Soup.parent
  let children n = Soup.children n |> Soup.elements |> Soup.to_list
end

module R = Cascade.Resolve.Make (Node)

(* ---------- inline policy ---------- *)

(* selectors that can be written as an inline style (no state / condition) *)
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

(* [html] is stylable so that :root custom properties land on it. *)
let no_style = [ "head"; "meta"; "title"; "base"; "link"; "style"; "script" ]

(* CSS inherited properties (a conservative set: missing one only keeps a
   redundant declaration; the differential test guards against dropping a needed
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

let parse_inline s =
  match Css.of_string ("a{" ^ s ^ "}") with
  | Ok p -> (
      match Sheet.rules p.Css.stylesheet with
      | r :: _ -> Sheet.declarations r
      | [] -> [])
  | Error _ -> []

let decl_value d =
  let s =
    Sheet.inline_style_of_declarations ~minify:true ~mode:Variables [ d ]
  in
  match String.index_opt s ':' with
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
  | None -> s

(* a node's style: the author cascade from {!R.resolve} with the element's own
   inline style overlaid (inline beats a selector, but an author !important
   beats a normal inline declaration). *)
let resolved sheet n =
  let author = R.resolve sheet n in
  match Soup.attribute "style" n with
  | None -> author
  | Some s ->
      let overlay map d =
        let k = Decl.property_name d in
        match List.assoc_opt k map with
        | Some cur when Decl.is_important cur && not (Decl.is_important d) ->
            map
        | _ -> (k, d) :: List.remove_assoc k map
      in
      List.fold_left overlay
        (List.map (fun d -> (Decl.property_name d, d)) author)
        (parse_inline s)
      |> List.rev_map snd

let style_node node = function
  | [] -> ()
  | decls ->
      (* [~mode:Variables] keeps [var()] references; the element's own custom
         properties are resolved onto it too, so the browser still resolves
         them. The [Inline] mode would substitute a var() with its fallback,
         dropping the reference and changing the render. *)
      Soup.set_attribute "style"
        (Sheet.inline_style_of_declarations ~minify:true ~mode:Variables decls)
        node

(* ---------- splitting static / dynamic ---------- *)
module SSet = Set.Make (String)

(* every property a kept (conditional / stateful) rule can set, recursing into
   @media / @supports / @container / @layer blocks. *)
let rec props_of_stmts acc stmts =
  List.fold_left
    (fun acc s ->
      match s with
      | Sheet.Rule r ->
          List.fold_left
            (fun a d ->
              SSet.add (String.lowercase_ascii (Decl.property_name d)) a)
            acc (Sheet.declarations r)
      | Sheet.Media (_, b) | Sheet.Supports (_, b) | Sheet.Layer (_, b) ->
          props_of_stmts acc b
      | Sheet.Container (_, _, b) -> props_of_stmts acc b
      | _ -> acc)
    acc stmts

(* ---------- the walk ---------- *)
(* [ctx] maps each inherited property to the value in force from the ancestors,
   so [minimal] can drop a declaration that only restates it. *)
let rec walk ~minimal sheet ctx node =
  if List.mem (String.lowercase_ascii (Soup.name node)) no_style then
    List.iter (walk ~minimal sheet ctx) (Node.children node)
  else begin
    let kept, ctx' =
      List.fold_left
        (fun (kept, ctx) d ->
          let p = String.lowercase_ascii (Decl.property_name d) in
          if minimal && is_inherited p then
            let v = decl_value d in
            match List.assoc_opt p ctx with
            | Some pv when pv = v -> (kept, ctx)
            | _ -> (d :: kept, (p, v) :: List.remove_assoc p ctx)
          else (d :: kept, ctx))
        ([], ctx) (resolved sheet node)
    in
    style_node node (List.rev kept);
    List.iter (walk ~minimal sheet ctx') (Node.children node)
  end

let inline_html ~minimal ~html ~extra =
  let soup = Soup.parse html in
  let page_css =
    Soup.select "style" soup |> Soup.to_list |> List.concat_map Soup.texts
    |> String.concat "\n"
  in
  let css = page_css ^ "\n" ^ extra in
  let inline_sheet, keep_css, kept =
    match Css.of_string css with
    | Ok p ->
        (* flatten nesting up front, so the static/dynamic split and
           {!R.resolve} see the same flat rules. *)
        let stmts = Cascade.Flatten.block p.Css.stylesheet in
        (* properties a kept rule can override must stay in the cascade. *)
        let dyn =
          props_of_stmts SSet.empty
            (List.filter
               (function
                 | Sheet.Rule r -> not (inlinable (Sheet.selector r))
                 | _ -> true)
               stmts)
        in
        let is_dyn d =
          SSet.mem (String.lowercase_ascii (Decl.property_name d)) dyn
        in
        let inline_rules = ref [] in
        let keep =
          List.filter_map
            (fun stmt ->
              match stmt with
              | Sheet.Rule r when inlinable (Sheet.selector r) -> (
                  let sel = Sheet.selector r and ds = Sheet.declarations r in
                  (match List.filter (fun d -> not (is_dyn d)) ds with
                  | [] -> ()
                  | inl ->
                      inline_rules :=
                        Sheet.Rule (Sheet.rule ~selector:sel inl)
                        :: !inline_rules);
                  match List.filter is_dyn ds with
                  | [] -> None
                  | res -> Some (Sheet.Rule (Sheet.rule ~selector:sel res)))
              | other -> Some other)
            stmts
        in
        let keep_css =
          if keep = [] then "" else Css.to_string ~minify:true (Sheet.v keep)
        in
        (Sheet.v (List.rev !inline_rules), keep_css, List.length keep)
    | Error _ -> (Sheet.empty, "", 0)
  in
  Soup.children soup |> Soup.elements |> Soup.to_list
  |> List.iter (walk ~minimal inline_sheet []);
  (* the static rules now live on the elements; keep only the un-inlinable
     ones. *)
  Soup.select "style" soup |> Soup.iter Soup.delete;
  (if keep_css <> "" then
     let style = Soup.create_element ~inner_text:keep_css "style" in
     match Soup.select_one "head" soup with
     | Some head -> Soup.append_child head style
     | None -> (
         match Soup.select_one "html" soup with
         | Some h -> Soup.prepend_child h style
         | None -> ()));
  (Soup.to_string soup, kept)

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
      let out, kept = inline_html ~minimal ~html ~extra in
      if kept > 0 then
        Fmt.epr "Kept %d rule(s) with no inline form in a <style> block.@." kept;
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
     smallest styled page."
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
         $(b,style) attribute.";
      `P
        "Rules that have no inline form ($(b,:hover), media queries, \
         pseudo-elements, $(b,@keyframes)) cannot be projected onto an element \
         and are kept in a single $(b,<style>) block.";
    ]
  in
  Cmd.v (Cmd.info "apply" ~doc ~man) Term.(term_result term)
