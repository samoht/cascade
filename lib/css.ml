(** CSS generation utilities *)

module Pp = Pp
module Reader = Reader
module Values = Values
module Properties = Properties
module Declaration = Declaration
module Selector = Selector
module Stylesheet = Stylesheet
include Values
include Declaration
include Properties
include Stylesheet

type any_var = V : 'a var -> any_var
type mode = Variables | Inline
type config = { minify : bool; mode : mode; optimize : bool }

let meta (type t) () =
  let module M = struct
    type meta += V : t -> meta
  end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)

(* Typed variable setters *)
let var : type a.
    ?fallback:a ->
    ?layer:string ->
    ?meta:meta ->
    string ->
    a kind ->
    a ->
    declaration * a var =
 fun ?fallback ?layer ?meta name kind value ->
  let declaration =
    Custom_declaration
      { name = String.concat "" [ "--"; name ]; kind; value; layer; meta }
  in
  let var_handle = { name; fallback; default = Some value; layer; meta } in
  (declaration, var_handle)

(** {1 Utilities} *)

let rec vars_of_calc : type a. a calc -> any_var list = function
  | Val _ -> []
  | Var v -> [ V v ]
  | Num _ -> []
  | Expr (left, _, right) -> vars_of_calc left @ vars_of_calc right

(* Extract variables from any property value *)
let vars_of_property : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Width, Var v -> [ V v ]
  | Width, Calc calc -> vars_of_calc calc
  | Height, Var v -> [ V v ]
  | Height, Calc calc -> vars_of_calc calc
  | Min_width, Var v -> [ V v ]
  | Min_width, Calc calc -> vars_of_calc calc
  | Min_height, Var v -> [ V v ]
  | Min_height, Calc calc -> vars_of_calc calc
  | Max_width, Var v -> [ V v ]
  | Max_width, Calc calc -> vars_of_calc calc
  | Max_height, Var v -> [ V v ]
  | Max_height, Calc calc -> vars_of_calc calc
  | Padding, Var v -> [ V v ]
  | Padding, Calc calc -> vars_of_calc calc
  | Padding_top, Var v -> [ V v ]
  | Padding_top, Calc calc -> vars_of_calc calc
  | Padding_right, Var v -> [ V v ]
  | Padding_right, Calc calc -> vars_of_calc calc
  | Padding_bottom, Var v -> [ V v ]
  | Padding_bottom, Calc calc -> vars_of_calc calc
  | Padding_left, Var v -> [ V v ]
  | Padding_left, Calc calc -> vars_of_calc calc
  | Padding_inline, Var v -> [ V v ]
  | Padding_inline, Calc calc -> vars_of_calc calc
  | Padding_inline_start, Var v -> [ V v ]
  | Padding_inline_start, Calc calc -> vars_of_calc calc
  | Padding_inline_end, Var v -> [ V v ]
  | Padding_inline_end, Calc calc -> vars_of_calc calc
  | Padding_block, Var v -> [ V v ]
  | Padding_block, Calc calc -> vars_of_calc calc
  | Margin, Var v -> [ V v ]
  | Margin, Calc calc -> vars_of_calc calc
  | Margin_top, Var v -> [ V v ]
  | Margin_top, Calc calc -> vars_of_calc calc
  | Margin_right, Var v -> [ V v ]
  | Margin_right, Calc calc -> vars_of_calc calc
  | Margin_bottom, Var v -> [ V v ]
  | Margin_bottom, Calc calc -> vars_of_calc calc
  | Margin_left, Var v -> [ V v ]
  | Margin_left, Calc calc -> vars_of_calc calc
  | Margin_inline, Var v -> [ V v ]
  | Margin_inline, Calc calc -> vars_of_calc calc
  | Margin_block, Var v -> [ V v ]
  | Margin_block, Calc calc -> vars_of_calc calc
  | Top, Var v -> [ V v ]
  | Top, Calc calc -> vars_of_calc calc
  | Right, Var v -> [ V v ]
  | Right, Calc calc -> vars_of_calc calc
  | Bottom, Var v -> [ V v ]
  | Bottom, Calc calc -> vars_of_calc calc
  | Left, Var v -> [ V v ]
  | Left, Calc calc -> vars_of_calc calc
  | Font_size, Var v -> [ V v ]
  | Font_size, Calc calc -> vars_of_calc calc
  | Letter_spacing, Var v -> [ V v ]
  | Letter_spacing, Calc calc -> vars_of_calc calc
  | Line_height, Normal -> []
  | Line_height, Length (Var v) -> [ V v ]
  | Line_height, Length (Calc calc) -> vars_of_calc calc
  | Line_height, Length _ -> []
  | Line_height, Number _ -> []
  | Line_height, Percentage _ -> []
  | Line_height, Inherit -> []
  | Line_height, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Border_width, Calc calc -> vars_of_calc calc
  | Border_top_width, Var v -> [ V v ]
  | Border_top_width, Calc calc -> vars_of_calc calc
  | Border_right_width, Var v -> [ V v ]
  | Border_right_width, Calc calc -> vars_of_calc calc
  | Border_bottom_width, Var v -> [ V v ]
  | Border_bottom_width, Calc calc -> vars_of_calc calc
  | Border_left_width, Var v -> [ V v ]
  | Border_left_width, Calc calc -> vars_of_calc calc
  | Border_inline_start_width, Var v -> [ V v ]
  | Border_inline_start_width, Calc calc -> vars_of_calc calc
  | Border_inline_end_width, Var v -> [ V v ]
  | Border_inline_end_width, Calc calc -> vars_of_calc calc
  | Outline_width, Var v -> [ V v ]
  | Outline_width, Calc calc -> vars_of_calc calc
  | Column_gap, Var v -> [ V v ]
  | Column_gap, Calc calc -> vars_of_calc calc
  | Row_gap, Var v -> [ V v ]
  | Row_gap, Calc calc -> vars_of_calc calc
  | Gap, Var v -> [ V v ]
  | Gap, Calc calc -> vars_of_calc calc
  (* Color properties *)
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Border_inline_start_color, Var v -> [ V v ]
  | Border_inline_end_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Outline_color, Var v -> [ V v ]
  (* Border radius *)
  | Border_radius, Var v -> [ V v ]
  | Border_radius, Calc calc -> vars_of_calc calc
  (* Outline offset *)
  | Outline_offset, Var v -> [ V v ]
  | Outline_offset, Calc calc -> vars_of_calc calc
  (* Other properties don't support Var *)
  (* All other cases *)
  | _ -> []

let rec vars_of_value : type a. a kind -> a -> any_var list =
 fun kind value ->
  match (kind, value) with
  | Length, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Duration, Var v -> [ V v ]
  | Blend_mode, _ -> []
  | Scroll_snap_strictness, _ -> []
  | Angle, Var v -> [ V v ]
  | Angle, _ -> []
  | Length, Calc calc -> vars_of_calc calc
  | Color, Mix _ -> [] (* TODO: extend to extract from color mix *)
  | Int, _ -> []
  | Float, _ -> []
  | Aspect_ratio, _ -> []
  | Border_style, _ -> []
  | Font_weight, _ -> []
  | String, _ -> []
  | Font_variant_numeric, Var v -> [ V v ]
  | ( Font_variant_numeric,
      Composed
        {
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        } ) ->
      vars_of_values_opt
        [
          ordinal;
          slashed_zero;
          numeric_figure;
          numeric_spacing;
          numeric_fraction;
        ]
  | Font_variant_numeric, _ -> []
  | Font_variant_numeric_token, Var v -> [ V v ]
  | Font_variant_numeric_token, _ -> []
  | Box_shadow, _ -> []
  | _ -> []

and vars_of_values_opt values =
  let collect_vars (opt_fv : font_variant_numeric_token option) =
    match opt_fv with None -> [] | Some (Var v) -> [ V v ] | Some _token -> []
  in
  List.concat_map collect_vars values

let compare_vars_by_name (V x) (V y) = String.compare x.name y.name

(** Extract all CSS variables referenced in properties (for theme layer) *)
let vars_of_declarations properties =
  List.concat_map
    (function
      | Declaration (prop, value) -> vars_of_property prop value
      | Important_declaration (prop, value) -> vars_of_property prop value
      | Custom_declaration { kind; value; _ } -> vars_of_value kind value)
    properties
  |> List.sort_uniq compare_vars_by_name

(* Helper function to check if a property should allow duplicates Some webkit
   properties need to be duplicated for browser compatibility. This is a
   workaround for older WebKit/Safari versions that had bugs with certain CSS
   properties.

   See: https://bugs.webkit.org/show_bug.cgi?id=101180 and:
   https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color#browser_compatibility

   Modern CSS libraries include these duplicates for maximum compatibility. *)

(* Duplicate known buggy properties for browser compatibility *)
let duplicate_buggy_properties decls =
  List.concat_map
    (fun decl ->
      match decl with
      | Declaration (Webkit_text_decoration, Inherit)
      | Important_declaration (Webkit_text_decoration, Inherit) ->
          [ decl; decl; decl ] (* Triplicate only when inherit *)
      | Declaration (Webkit_text_decoration_color, _)
      | Important_declaration (Webkit_text_decoration_color, _) ->
          [ decl; decl ] (* Always duplicate webkit-text-decoration-color *)
      | _ -> [ decl ])
    decls

let deduplicate_declarations props =
  (* CSS cascade rules: 1. !important declarations always win over normal
     declarations 2. Among declarations of same importance, last one wins We
     need to track normal and important separately *)
  let normal_seen = Hashtbl.create 16 in
  let important_seen = Hashtbl.create 16 in

  (* First pass: collect last occurrence of each property by importance *)
  List.iter
    (fun decl ->
      match decl with
      | Declaration (prop, value) ->
          let prop_name = Pp.to_string ~minify:true pp_property prop in
          Hashtbl.replace normal_seen prop_name (Declaration (prop, value))
      | Important_declaration (prop, value) ->
          let prop_name = Pp.to_string ~minify:true pp_property prop in
          Hashtbl.replace important_seen prop_name
            (Important_declaration (prop, value))
      | Custom_declaration { name; _ } as cdecl ->
          (* Custom properties can't be important, just track last occurrence *)
          Hashtbl.replace normal_seen name cdecl)
    props;

  (* Second pass: build result, important wins over normal for same property *)
  let deduped = ref [] in
  let processed = Hashtbl.create 16 in

  (* Process in reverse order to maintain first occurrence position *)
  List.iter
    (fun decl ->
      let prop_name =
        match decl with
        | Declaration (prop, _) -> Pp.to_string ~minify:true pp_property prop
        | Important_declaration (prop, _) ->
            Pp.to_string ~minify:true pp_property prop
        | Custom_declaration { name; _ } -> name
      in

      if not (Hashtbl.mem processed prop_name) then (
        Hashtbl.add processed prop_name ();
        (* If there's an important version, use it; otherwise use normal *)
        let final_decl =
          if Hashtbl.mem important_seen prop_name then
            Hashtbl.find important_seen prop_name
          else if Hashtbl.mem normal_seen prop_name then
            Hashtbl.find normal_seen prop_name
          else decl (* Should not happen *)
        in
        deduped := final_decl :: !deduped))
    props;

  (* Apply buggy property duplication after deduplication *)
  duplicate_buggy_properties (List.rev !deduped)

(* Get the name of a variable *)
let any_var_name (V v) = String.concat "" [ "--"; v.name ]

(* Extract variables from a typed value - needs to handle each property type *)
let extract_vars_from_prop_value : type a. a property -> a -> any_var list =
 fun prop value ->
  match (prop, value) with
  | Background_color, Var v -> [ V v ]
  | Color, Var v -> [ V v ]
  | Border_color, Var v -> [ V v ]
  | Border_top_color, Var v -> [ V v ]
  | Border_right_color, Var v -> [ V v ]
  | Border_bottom_color, Var v -> [ V v ]
  | Border_left_color, Var v -> [ V v ]
  | Border_inline_start_color, Var v -> [ V v ]
  | Border_inline_end_color, Var v -> [ V v ]
  | Text_decoration_color, Var v -> [ V v ]
  | Webkit_text_decoration_color, Var v -> [ V v ]
  | Webkit_tap_highlight_color, Var v -> [ V v ]
  | Padding, Var v -> [ V v ]
  | Padding_left, Var v -> [ V v ]
  | Padding_right, Var v -> [ V v ]
  | Padding_top, Var v -> [ V v ]
  | Padding_bottom, Var v -> [ V v ]
  | Margin, Var v -> [ V v ]
  | Margin_left, Var v -> [ V v ]
  | Margin_right, Var v -> [ V v ]
  | Margin_top, Var v -> [ V v ]
  | Margin_bottom, Var v -> [ V v ]
  | Gap, Var v -> [ V v ]
  | Column_gap, Var v -> [ V v ]
  | Row_gap, Var v -> [ V v ]
  | Width, Var v -> [ V v ]
  | Height, Var v -> [ V v ]
  | Min_width, Var v -> [ V v ]
  | Min_height, Var v -> [ V v ]
  | Max_width, Var v -> [ V v ]
  | Max_height, Var v -> [ V v ]
  | Font_size, Var v -> [ V v ]
  | Line_height, Var v -> [ V v ]
  | Letter_spacing, Var v -> [ V v ]
  | Top, Var v -> [ V v ]
  | Right, Var v -> [ V v ]
  | Bottom, Var v -> [ V v ]
  | Left, Var v -> [ V v ]
  | Border_radius, Var v -> [ V v ]
  | Border_width, Var v -> [ V v ]
  | Outline_offset, Var v -> [ V v ]
  | _ -> [] (* No variables in this value *)

let extract_vars_from_declaration : declaration -> any_var list = function
  | Custom_declaration _ -> [] (* Custom properties don't have typed vars *)
  | Declaration (prop, value) -> extract_vars_from_prop_value prop value
  | Important_declaration (prop, value) ->
      extract_vars_from_prop_value prop value

(* Analyze declarations to find all variable references *)
let analyze_declarations (decls : declaration list) : any_var list =
  List.concat_map extract_vars_from_declaration decls

(* Extract only custom property declarations (variable definitions) *)
let extract_custom_declarations (decls : declaration list) : declaration list =
  List.filter (function Custom_declaration _ -> true | _ -> false) decls

(* Extract the variable name from a custom declaration *)
let custom_declaration_name (decl : declaration) : string option =
  match decl with Custom_declaration { name; _ } -> Some name | _ -> None

let inline_style_of_declarations ?(optimize = false) ?(minify = false)
    ?(mode : mode = Inline) props =
  let config = { mode; minify; optimize } in
  props
  |> List.map (function
       | Declaration (prop, value) ->
           let name = Pp.to_string ~minify:true pp_property prop in
           let value_str =
             let pp ctx v = pp_property_value ctx v in
             Pp.to_string ~minify:config.minify pp (prop, value)
           in
           if config.minify then String.concat "" [ name; ":"; value_str ]
           else String.concat "" [ name; ": "; value_str ]
       | Important_declaration (prop, value) ->
           let name = Pp.to_string ~minify:true pp_property prop in
           let value_str =
             let pp ctx v = pp_property_value ctx v in
             Pp.to_string ~minify:config.minify pp (prop, value)
           in
           if config.minify then
             String.concat "" [ name; ":"; value_str; "!important" ]
           else String.concat "" [ name; ": "; value_str; " !important" ]
       | Custom_declaration { name; kind; value; _ } ->
           let value_str =
             let pp ctx v = pp_value ctx v in
             Pp.to_string ~minify:config.minify pp (kind, value)
           in
           if config.minify then String.concat "" [ name; ":"; value_str ]
           else String.concat "" [ name; ": "; value_str ])
  |> String.concat "; "

let merge_rules rules =
  (* Only merge truly adjacent rules with the same selector to preserve cascade
     order. This is safe because we don't reorder rules - we only combine
     immediately adjacent rules with identical selectors, which maintains
     cascade semantics. *)
  let rec merge_adjacent acc prev_rule = function
    | [] -> List.rev (match prev_rule with Some r -> r :: acc | None -> acc)
    | rule :: rest -> (
        match prev_rule with
        | None ->
            (* First rule - just store it *)
            merge_adjacent acc (Some rule) rest
        | Some prev ->
            if prev.selector = rule.selector then
              (* Same selector immediately following - safe to merge *)
              let merged =
                {
                  selector = prev.selector;
                  declarations =
                    deduplicate_declarations
                      (prev.declarations @ rule.declarations);
                }
              in
              merge_adjacent acc (Some merged) rest
            else
              (* Different selector - emit previous rule and continue with
                 current *)
              merge_adjacent (prev :: acc) (Some rule) rest)
  in
  merge_adjacent [] None rules

(* Check if a selector should not be combined with others *)
let should_not_combine selector =
  (* Already a list selector - don't combine *)
  Selector.is_compound_list selector
  ||
  (* Check string representation for specific prefixes *)
  let s = Pp.to_string Selector.pp selector in
  String.starts_with ~prefix:"::file-selector-button" s
  || String.starts_with ~prefix:"::-webkit-" s

(* Convert group of selectors to a rule *)
let group_to_rule = function
  | [ (sel, decls) ] -> Some { selector = sel; declarations = decls }
  | [] -> None
  | group ->
      let selector_list = List.map fst (List.rev group) in
      let decls = snd (List.hd group) in
      (* Create a List selector from all the selectors *)
      let combined_selector =
        if List.length selector_list = 1 then List.hd selector_list
        else Selector.list selector_list
      in
      Some { selector = combined_selector; declarations = decls }

(* Flush current group to accumulator *)
let flush_group acc group =
  match group_to_rule group with Some rule -> rule :: acc | None -> acc

(* Combine consecutive rules with identical declarations into comma-separated
   selectors *)
let combine_identical_rules rules =
  (* Only combine consecutive rules to preserve cascade semantics *)
  let rec combine_consecutive acc current_group = function
    | [] -> List.rev (flush_group acc current_group)
    | rule :: rest -> (
        if should_not_combine rule.selector then
          (* Don't combine this selector, flush current group and start fresh *)
          let acc' = rule :: flush_group acc current_group in
          combine_consecutive acc' [] rest
        else
          match current_group with
          | [] ->
              (* Start a new group *)
              combine_consecutive acc
                [ (rule.selector, rule.declarations) ]
                rest
          | (_prev_sel, prev_decls) :: _ ->
              if prev_decls = rule.declarations then
                (* Same declarations, add to current group *)
                combine_consecutive acc
                  ((rule.selector, rule.declarations) :: current_group)
                  rest
              else
                (* Different declarations, flush current group and start new
                   one *)
                let acc' = flush_group acc current_group in
                combine_consecutive acc'
                  [ (rule.selector, rule.declarations) ]
                  rest)
  in
  combine_consecutive [] [] rules

(** {1 Rendering} *)

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

(* Version information *)
let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  String.concat ""
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]

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

(* ======================================================================== *)
(* CSS Optimization *)
(* ======================================================================== *)

type layer_stats = {
  name : string;
  rules : int;
  selectors : string list; (* First few selectors as examples *)
}

(* Optimize a single rule by deduplicating its declarations *)
let optimize_single_rule (rule : rule) : rule =
  { rule with declarations = deduplicate_declarations rule.declarations }

(* Optimize a list of plain CSS rules *)
let optimize_rule_list (rules : rule list) : rule list =
  let deduped = List.map optimize_single_rule rules in
  let merged = merge_rules deduped in
  combine_identical_rules merged

(* Optimize nested rules (Rule | Supports) while preserving order *)
let optimize_nested_rules (rules : nested_rule list) : nested_rule list =
  (* Process rules in batches separated by non-Rule items *)
  let rec process_nested (acc : nested_rule list) (remaining : nested_rule list)
      : nested_rule list =
    match remaining with
    | [] -> List.rev acc
    | Rule r :: rest ->
        (* Collect consecutive Rule items *)
        let rec collect_rules (rules_acc : rule list) :
            nested_rule list -> rule list * nested_rule list = function
          | Rule r :: rest -> collect_rules (r :: rules_acc) rest
          | rest -> (List.rev rules_acc, rest)
        in
        let plain_rules, rest = collect_rules [ r ] rest in
        (* Optimize this batch of consecutive rules *)
        let optimized = optimize_rule_list plain_rules in
        let as_nested = List.map rule_to_nested optimized in
        process_nested (List.rev_append as_nested acc) rest
    | hd :: rest ->
        (* Non-Rule item (e.g., Supports) - keep as-is *)
        process_nested (hd :: acc) rest
  in
  process_nested [] rules

(* Optimize a layer_rule *)
let optimize_layer (layer : layer_rule) : layer_rule =
  let optimized_rules = optimize_nested_rules layer.rules in
  { layer with rules = optimized_rules }

(* Optimize a media rule *)
let optimize_media_rule (mq : media_rule) : media_rule =
  { mq with media_rules = optimize_rule_list mq.media_rules }

(* Optimize a container rule *)
let optimize_container_rule (cq : container_rule) : container_rule =
  { cq with container_rules = optimize_rule_list cq.container_rules }

let rec optimize_supports_rule (sq : supports_rule) : supports_rule =
  let optimized_content =
    match sq.supports_content with
    | Support_rules rules -> Support_rules (optimize_rule_list rules)
    | Support_nested (rules, nested) ->
        Support_nested
          (optimize_rule_list rules, List.map optimize_supports_rule nested)
  in
  { sq with supports_content = optimized_content }

let optimize (stylesheet : t) : t =
  (* Apply CSS optimizations while preserving cascade semantics *)
  let optimized_layers = List.map optimize_layer stylesheet.layers in
  (* When @supports blocks are present alongside top-level rules, we cannot
     safely merge the top-level rules because the stylesheet structure separates
     rules from @supports blocks into different lists, losing their relative
     ordering.

     However, we can still optimize if there are no top-level rules (everything
     is in layers/@supports/@media), or if there are no @supports blocks. *)
  let optimized_rules =
    if stylesheet.supports_queries = [] || stylesheet.rules = [] then
      (* Safe to optimize: either no @supports or no top-level rules to
         interfere *)
      optimize_rule_list stylesheet.rules
    else
      (* Both top-level rules and @supports exist - can't merge safely *)
      List.map optimize_single_rule stylesheet.rules
  in
  {
    stylesheet with
    layers = optimized_layers;
    rules = optimized_rules;
    media_queries = List.map optimize_media_rule stylesheet.media_queries;
    container_queries =
      List.map optimize_container_rule stylesheet.container_queries;
    supports_queries =
      List.map optimize_supports_rule stylesheet.supports_queries;
  }

let to_string ?(minify = false) ?optimize:(opt = false) ?(mode = Variables)
    stylesheet =
  let optimized_stylesheet = if opt then optimize stylesheet else stylesheet in
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

(** Extract all CSS variables from different input types *)

let vars_of_rules rules =
  List.concat_map (fun rule -> vars_of_declarations rule.declarations) rules

let vars_of_media_queries media_queries =
  List.concat_map (fun mq -> vars_of_rules mq.media_rules) media_queries

let vars_of_container_queries container_queries =
  List.concat_map (fun cq -> vars_of_rules cq.container_rules) container_queries

let vars_of_stylesheet (ss : t) =
  vars_of_rules ss.rules
  @ vars_of_media_queries ss.media_queries
  @ vars_of_container_queries ss.container_queries

let pp ?minify ?optimize ?mode stylesheet =
  to_string ?minify ?optimize ?mode stylesheet
