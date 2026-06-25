(* [cascade apply <page.html> [extra.css]]: resolve a stylesheet against the
   HTML, write each element's winning declarations into its style attribute, and
   keep the rules with no inline form (:hover, @media, ...) in a <style>. The
   projection lives in {!Cascade.Apply}; this command is the lambdasoup glue:
   parse the HTML, hand the element tree to the library, and write the result
   back. *)

open Cmdliner
module Sheet = Cascade.Stylesheet
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

module Apply = Cascade.Apply.Make (Node)

let err_msg fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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

let inline_html ~minimal ~html ~extra =
  let soup = Soup.parse html in
  let page_css =
    Soup.select "style" soup |> Soup.to_list |> List.concat_map Soup.texts
    |> String.concat "\n"
  in
  let css = page_css ^ "\n" ^ extra in
  let roots = Soup.children soup |> Soup.elements |> Soup.to_list in
  let { Cascade.Apply.styles; keep_css; kept } =
    Apply.compute ~minimal ~css roots
  in
  List.iter (fun (node, decls) -> style_node node decls) styles;
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
  with Sys_error msg -> err_msg "Error reading %s: %s" path msg

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
