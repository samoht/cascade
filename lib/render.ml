(** CSS rendering implementation *)

open Declaration
open Stylesheet
open Optimize
include Render_intf

(** {1 Inline Styles} *)

let inline_style_of_declarations ?(optimize = false) ?(minify = false)
    ?(mode : mode = Inline) props =
  let config = { mode; minify; optimize } in
  props
  |> List.map (fun decl ->
         let name = Declaration.property_name decl in
         let value = Declaration.string_of_value ~minify:config.minify decl in
         let is_important = Declaration.is_important decl in

         if is_important then
           if config.minify then
             String.concat "" [ name; ":"; value; "!important" ]
           else String.concat "" [ name; ": "; value; " !important" ]
         else if config.minify then String.concat "" [ name; ":"; value ]
         else String.concat "" [ name; ": "; value ])
  |> String.concat "; "

(** {1 Pretty-Printing} *)

(* Forward declarations for mutually recursive functions *)
let rec pp_rule : rule Pp.t =
 fun ctx rule ->
  Selector.pp ctx rule.selector;
  Pp.sp ctx ();
  let pp_body ctx () =
    (* Apply deduplication when printing to ensure cascade rules are
       respected *)
    let deduped_decls = deduplicate_declarations rule.declarations in
    match deduped_decls with
    | [] -> ()
    | decls ->
        Pp.cut ctx ();
        Pp.nest 2
          (Pp.list
             ~sep:(fun ctx () ->
               Pp.semicolon ctx ();
               Pp.cut ctx ())
             pp_declaration)
          ctx decls;
        Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

and pp_supports_content : supports_content Pp.t =
 fun ctx -> function
  | Support_rules rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules
  | Support_nested (rules, nested_queries) ->
      Pp.list ~sep:Pp.cut pp_rule ctx rules;
      if rules <> [] && nested_queries <> [] then Pp.cut ctx ();
      Pp.list ~sep:Pp.cut pp_supports_query ctx nested_queries

and pp_supports_query : supports_rule Pp.t =
 fun ctx sq ->
  Pp.string ctx "@supports ";
  Pp.string ctx sq.supports_condition;
  Pp.sp ctx ();
  Pp.braces pp_supports_content ctx sq.supports_content

let pp_nested_rule : nested_rule Pp.t =
 fun ctx -> function
  | Rule r ->
      let r =
        { r with declarations = deduplicate_declarations r.declarations }
      in
      pp_rule ctx r
  | Supports sq -> pp_supports_query ctx sq

let pp_layer_rules : nested_rule list Pp.t =
 fun ctx rules -> Pp.list ~sep:Pp.cut pp_nested_rule ctx rules

(* Generic at-rule pretty printer *)
let pp_at_rules ~at_rule ~condition ~name_part : rule list Pp.t =
 fun ctx rules ->
  Pp.char ctx '@';
  Pp.string ctx at_rule;
  Pp.space ctx ();
  if name_part <> "" then Pp.string ctx name_part;
  Pp.string ctx condition;
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx rules

let pp_media_rule : media_rule Pp.t =
 fun ctx mq ->
  pp_at_rules ~at_rule:"media" ~condition:mq.media_condition ~name_part:"" ctx
    mq.media_rules

let pp_layer_media : media_rule list Pp.t =
 fun ctx media_queries -> Pp.list ~sep:Pp.cut pp_media_rule ctx media_queries

let pp_container_rule : container_rule Pp.t =
 fun ctx cq ->
  let name_part =
    match cq.container_name with None -> "" | Some name -> name ^ " "
  in
  pp_at_rules ~at_rule:"container" ~condition:cq.container_condition ~name_part
    ctx cq.container_rules

let pp_layer_containers : container_rule list Pp.t =
 fun ctx container_queries ->
  Pp.list ~sep:Pp.cut pp_container_rule ctx container_queries

let pp_layer_supports : supports_rule list Pp.t =
 fun ctx supports_queries ->
  Pp.list ~sep:Pp.cut pp_supports_query ctx supports_queries

let pp_stylesheet_rules : rule list Pp.t =
 fun ctx rules -> Pp.list ~sep:Pp.cut pp_rule ctx rules

let pp_stylesheet_media : media_rule list Pp.t =
 fun ctx media_queries -> Pp.list ~sep:Pp.cut pp_media_rule ctx media_queries

let pp_stylesheet_containers : container_rule list Pp.t =
 fun ctx container_queries ->
  Pp.list ~sep:Pp.cut pp_container_rule ctx container_queries

let pp_stylesheet_supports : supports_rule list Pp.t =
 fun ctx supports_queries ->
  Pp.list ~sep:Pp.cut pp_supports_query ctx supports_queries

let pp_starting_style_rule ~optimize : starting_style_rule Pp.t =
 fun ctx ss ->
  let optimized =
    if optimize then ss.starting_rules |> merge_rules else ss.starting_rules
  in
  Pp.string ctx "@starting-style";
  Pp.sp ctx ();
  Pp.braces (Pp.list ~sep:Pp.cut pp_rule) ctx optimized

let pp_starting_styles ~optimize : starting_style_rule list Pp.t =
 fun ctx starting_styles ->
  Pp.list ~sep:Pp.cut (pp_starting_style_rule ~optimize) ctx starting_styles

let pp_property_rule : property_rule Pp.t =
 fun ctx at ->
  Pp.string ctx "@property ";
  Pp.string ctx at.name;
  Pp.sp ctx ();
  let pp_body ctx () =
    Pp.cut ctx ();
    Pp.nest 2
      (fun ctx () ->
        Pp.string ctx "syntax: \"";
        Pp.string ctx at.syntax;
        Pp.string ctx "\"";
        Pp.semicolon ctx ();
        Pp.cut ctx ();
        Pp.string ctx "inherits: ";
        Pp.string ctx (if at.inherits then "true" else "false");
        (match at.initial_value with
        | None | Some "" | Some "initial" -> ()
        | Some v ->
            Pp.semicolon ctx ();
            Pp.cut ctx ();
            Pp.string ctx "initial-value: ";
            Pp.string ctx v);
        Pp.semicolon ctx ())
      ctx ();
    Pp.cut ctx ()
  in
  Pp.surround ~left:Pp.block_open ~right:Pp.block_close pp_body ctx ()

let pp_at_properties : property_rule list Pp.t =
 fun ctx at_properties -> Pp.list ~sep:Pp.cut pp_property_rule ctx at_properties

(* Helper functions for to_string *)
let is_layer_empty (layer : layer_rule) =
  layer.rules = [] && layer.media_queries = []
  && layer.container_queries = []
  && layer.supports_queries = []

let pp_layer : layer_rule Pp.t =
 fun ctx layer_rules ->
  let layer_name = layer_rules.layer in
  Pp.string ctx "@layer ";
  Pp.string ctx layer_name;
  if is_layer_empty layer_rules then Pp.semicolon ctx ()
  else (
    Pp.sp ctx ();
    let pp_body ctx () =
      let sep_if_needed prev_empty current =
        if (not prev_empty) && current <> [] then Pp.cut ctx ()
      in
      let rules_empty = layer_rules.rules = [] in
      pp_layer_rules ctx layer_rules.rules;
      sep_if_needed rules_empty layer_rules.media_queries;
      let media_empty = layer_rules.media_queries = [] in
      pp_layer_media ctx layer_rules.media_queries;
      sep_if_needed (rules_empty && media_empty) layer_rules.container_queries;
      let container_empty = layer_rules.container_queries = [] in
      pp_layer_containers ctx layer_rules.container_queries;
      sep_if_needed
        (rules_empty && media_empty && container_empty)
        layer_rules.supports_queries;
      pp_layer_supports ctx layer_rules.supports_queries
    in
    Pp.braces pp_body ctx ())

let pp_stylesheet_sections ~optimize : t Pp.t =
 fun ctx stylesheet ->
  let sep_if_needed prev_empty current =
    if (not prev_empty) && current <> [] then Pp.cut ctx ()
  in

  let rules_empty = stylesheet.rules = [] in
  pp_stylesheet_rules ctx stylesheet.rules;

  sep_if_needed rules_empty stylesheet.starting_styles;
  let starting_empty = stylesheet.starting_styles = [] in
  pp_starting_styles ~optimize ctx stylesheet.starting_styles;

  sep_if_needed (rules_empty && starting_empty) stylesheet.container_queries;
  let container_empty = stylesheet.container_queries = [] in
  pp_stylesheet_containers ctx stylesheet.container_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty)
    stylesheet.supports_queries;
  let supports_empty = stylesheet.supports_queries = [] in
  pp_stylesheet_supports ctx stylesheet.supports_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty && supports_empty)
    stylesheet.media_queries;
  let media_empty = stylesheet.media_queries = [] in
  pp_stylesheet_media ctx stylesheet.media_queries;

  sep_if_needed
    (rules_empty && starting_empty && container_empty && supports_empty
   && media_empty)
    stylesheet.at_properties;
  pp_at_properties ctx stylesheet.at_properties

let pp_layers ~minify : layer_rule list Pp.t =
 fun ctx layers ->
  if not minify then Pp.list ~sep:Pp.cut pp_layer ctx layers
  else
    (* Merge consecutive empty layers into single @layer declarations *)
    let rec process_layers prev_empty current_empty_group = function
      | [] ->
          (* Finish any remaining empty group *)
          if current_empty_group = [] then ()
          else (
            if not prev_empty then Pp.cut ctx ();
            Pp.string ctx "@layer ";
            Pp.string ctx (String.concat "," (List.rev current_empty_group));
            Pp.semicolon ctx ())
      | layer :: rest ->
          if is_layer_empty layer then
            (* Collect empty layer names *)
            process_layers prev_empty (layer.layer :: current_empty_group) rest
          else (
            (* Non-empty layer, flush any empty group and add this layer *)
            if current_empty_group <> [] then (
              if not prev_empty then Pp.cut ctx ();
              Pp.string ctx "@layer ";
              Pp.string ctx (String.concat "," (List.rev current_empty_group));
              Pp.semicolon ctx ();
              Pp.cut ctx ())
            else if not prev_empty then Pp.cut ctx ();
            pp_layer ctx layer;
            process_layers false [] rest)
    in
    process_layers true [] layers

(** {1 Stylesheet Rendering} *)

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

let to_string ?(minify = false) ?optimize:(opt = false) ?(mode = Variables)
    stylesheet =
  let optimized_stylesheet =
    if opt then Optimize.optimize stylesheet else stylesheet
  in
  let pp ctx () =
    (* Add header if there are layers *)
    if List.length stylesheet.layers > 0 then (
      Pp.string ctx header;
      Pp.cut ctx ());

    (* Render layers with merging for minified mode *)
    pp_layers ~minify ctx optimized_stylesheet.layers;

    (* Add separator if needed *)
    if
      optimized_stylesheet.layers <> []
      && (optimized_stylesheet.rules <> []
         || optimized_stylesheet.starting_styles <> []
         || optimized_stylesheet.container_queries <> []
         || optimized_stylesheet.supports_queries <> []
         || optimized_stylesheet.media_queries <> []
         || optimized_stylesheet.at_properties <> [])
    then Pp.cut ctx ();

    (* Render stylesheet sections *)
    pp_stylesheet_sections ~optimize:opt ctx optimized_stylesheet
  in
  Pp.to_string ~minify ~inline:(mode = Inline) pp ()

let pp ?minify ?optimize ?mode stylesheet =
  to_string ?minify ?optimize ?mode stylesheet
