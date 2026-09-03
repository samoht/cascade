(* [cascade apply <page.html> [extra.css]]: resolve a stylesheet against the
   HTML, write each element's winning declarations into its style attribute, and
   keep the rules with no inline form (:hover, @media, ...) in a <style>. The
   projection lives in {!Cascade.Apply}; this command is the HTML glue: parse
   the page into an {!Html} tree, hand the element tree to the library, and
   write the result back. *)

open Cmdliner
module Sheet = Cascade.Stylesheet
module Css = Cascade.Css
module Apply = Cascade.Apply.Make (Html_node)

let err_msg fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

let style_node node = function
  | [] -> ()
  | decls ->
      (* [~mode:Variables] keeps [var()] references; the element's own custom
         properties are resolved onto it too, so the browser still resolves
         them. The [Inline] mode would substitute a var() with its fallback,
         dropping the reference and changing the render. *)
      Html.set_attribute node "style"
        (Css.inline_style_of_declarations ~minify:true ~mode:Variables decls)

(* [Some sheet] is CSS the projection can use. [None] is a source the parser
   could not turn into any: a fatal syntax error, or a recovery that left no
   statement at all from an input that had something to drop. Neither is a
   source that was legitimately empty, which parses without a word and
   contributes nothing. [note] says what the caller does about the loss. *)
let parse_source ~filename ~note css =
  match Css.of_string ~filename css with
  | Error e ->
      Fmt.epr "Error: %s@." (Cascade.Error.to_string e);
      None
  | Ok { Css.stylesheet; warnings; _ } -> (
      List.iter Cli_io.report_warning warnings;
      (* Whether the parse produced anything is a question about the statement
         list. Serialising the sheet to answer it asks a different one, and gets
         it wrong: [@charset "UTF-8"] and an [src]-less [@font-face] parse, are
         kept, and still print nothing. What [Apply.compute] consumes is the
         list, not the text. *)
      match (stylesheet, warnings) with
      | [], _ :: _ ->
          Fmt.epr "Error: %s: parse dropped every rule%s@." filename note;
          None
      | _ -> Some stylesheet)

let inline_html ~minimal ~filename ~html ~extra =
  let doc = Html.parse html in
  (* Each <style> block parses on its own, which is how a browser reads them
     too, so a block the parser cannot use is known apart from the rest. *)
  let blocks =
    Html.find_all "style" doc
    |> List.mapi (fun i node ->
        let filename = Fmt.str "%s:<style>#%d" filename (i + 1) in
        let css = Html.text node in
        (node, parse_source ~filename ~note:"; keeping the block verbatim" css))
  in
  let sheet =
    List.concat_map (fun (_, s) -> Option.value s ~default:[]) blocks @ extra
  in
  let roots = Html.roots doc in
  let { Cascade.Apply.styles; keep_css; kept } =
    Apply.compute ~minimal ~sheet roots
  in
  List.iter (fun (node, decls) -> style_node node decls) styles;
  (* The static rules now live on the elements, so the CSS the blocks they came
     from held goes. The element itself stays, emptied: a [<style>] is a sibling
     like any other, and [.navbox + style + .portal-bar] is a real selector on a
     real page, so unlinking the node would stop a kept rule from matching what
     it matches in the browser. A block the parser could not use keeps its text
     too: emptying it would ship a page with neither the inline styles it should
     have had nor the CSS text a browser might still make something of. *)
  List.iter
    (fun (node, sheet) -> if Option.is_some sheet then Html.clear node)
    blocks;
  (if keep_css <> "" then
     let style = Html.element "style" ~text:keep_css in
     match Html.find "head" doc with
     | Some head -> Html.append_child head style
     | None -> (
         match Html.find "html" doc with
         | Some h -> Html.prepend_child h style
         | None -> ()));
  let lost = List.exists (fun (_, s) -> Option.is_none s) blocks in
  (Html.to_string doc, kept, lost)

(* ---------- cmdliner ---------- *)
let read_file path =
  try Ok (Cli_io.read_file path)
  with Sys_error msg -> err_msg "Error reading %s: %s" path msg

(* Cmdliner's [file] conv only checks that the path exists, so a directory or a
   file with no read permission reaches the command. That is a usage error, and
   it stops the run before it writes a page silently missing those styles - a
   stylesheet nobody can read is not the same thing as no stylesheet. A file
   that reads but does not parse is reported and the page is still written,
   without it. *)
let read_extra = function
  | None -> Ok (Some Sheet.empty)
  | Some f -> Result.map (parse_source ~filename:f ~note:"") (read_file f)

let run minimal html_file css_file () =
  match
    Result.bind (read_file html_file) (fun html ->
        Result.map (fun extra -> (html, extra)) (read_extra css_file))
  with
  | Error e -> Error e
  | Ok (html, extra) ->
      let out, kept, lost =
        inline_html ~minimal ~filename:html_file ~html
          ~extra:(Option.value extra ~default:Sheet.empty)
      in
      if kept > 0 then
        Fmt.epr "Kept %d rule(s) with no inline form in a <style> block.@." kept;
      print_string out;
      (* CSS that went missing is a result, not a usage error: exit 1, distinct
         from cmdliner's reserved codes, so a build can gate on it. *)
      if lost || Option.is_none extra then Stdlib.exit 1;
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
  let exits =
    Cli_exit.with_defaults
      [
        Cmd.Exit.info
          ~doc:"on success, including a parse that recovered some of the input"
          0;
        Cmd.Exit.info
          ~doc:
            "if a $(b,<style>) block or $(i,EXTRA.css) parsed to nothing; the \
             page is still written, without those styles"
          1;
        Cmd.Exit.info ~doc:"on command-line errors or unreadable input files"
          124;
      ]
  in
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
      `P
        "A projected $(b,<style>) block is emptied rather than removed: the \
         element is a sibling like any other, and a kept rule may select \
         across it. A block the parser cannot use keeps its text as well, \
         rather than being emptied along with the blocks that were projected.";
    ]
  in
  Cmd.v (Cmd.info "apply" ~doc ~man ~exits) Term.(term_result term)
