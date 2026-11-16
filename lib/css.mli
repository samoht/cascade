(** Typed CSS construction.

    This library provides types and functions to construct CSS declarations,
    rules and stylesheets. It avoids stringly-typed CSS by keeping close to the
    CSS syntax and specifications.

    Cascade is scoped to CSS text and CSS ASTs: parsing, printing, minification,
    structural traversal, structural transforms, structural diffs, and safe
    optimizations. When a transform needs information beyond CSS text, that
    information is passed as an explicit closed context record. Theme/default
    based [var()] output is part of the current API; {!module:Context} contains
    context records for value-oriented transforms.

    The main notions are:
    - A {!type:declaration} is a property/value pair.
    - A rule couples a selector with declarations.
    - A {!type:t} is a stylesheet, built with the facade helpers here or with
      the lower-level {!module:Stylesheet} AST API when direct inspection is
      useful.
    - Values are typed (e.g., {!type:length}, {!type:color}); invalid constructs
      raise [Invalid_argument].

    Minimal example:
    {[
    open Cascade.Css

    (* Build a ".btn" rule and render a stylesheet from it. *)
    let button =
      rule ~selector:(Selector.class_ "btn")
        [
          display Inline_block;
          background_color (hex "#3b82f6");
          color (hex "#ffffff");
          padding [ Rem 0.5 ];
          border_radius (radius (Rem 0.375));
        ]

    let css = to_string (v [ button ])
    ]}

    Custom properties:
    {[
    open Cascade.Css

    let def, primary = var "primary-color" Color (hex "#3b82f6")
    let root = rule ~selector:(Selector.v ":root") [ def ]
    let card = rule ~selector:(Selector.class_ "card") [ color (Var primary) ]
    let css = to_string (v [ root; card ])
    ]}

    Start with {!val:rule}, {!val:media}, {!val:container}, {!val:supports},
    {!val:v}, and {!val:to_string}. Property helpers are grouped by CSS feature
    below. Parser building blocks such as cursors, tokens and component values
    are available from the library root, for example {!module:Cascade.Cursor}
    and {!module:Cascade.Parser}, not through [Css].

    See {:https://www.w3.org/Style/CSS/specs.en.html W3C CSS Specifications} and
    {:https://developer.mozilla.org/en-US/docs/Web/CSS MDN CSS Documentation}.
*)

(** {1 Core Concepts}

    Core CSS system setup and construction tools for building stylesheets. *)

(** {2:feature_modules Feature Modules}

    The facade re-exports the CSS-facing modules users normally combine with the
    top-level helpers. These are aliases, so odoc links to the focused module
    page instead of duplicating each full signature here. *)

module Selector = Selector
(** Selector syntax, matching and specificity.

    @see <https://www.w3.org/TR/selectors-4/> Selectors Level 4 *)

module Selector_summary = Selector_summary
module Aria = Aria
module Color_space = Color_space
module Context = Context
module Pp = Pp
module Values = Values
module Declaration = Declaration
module Properties = Properties
module Variables = Variables
module Optimize = Optimize
module Stylesheet = Stylesheet
module Source = Stylesheet.Source
module Media = Media
module Container = Container
module Supports = Supports
module Keyframe = Keyframe
module Font_face = Font_face
module Nest = Nest

(** Parser building blocks live at the library root ([Cascade.Cursor],
    [Cascade.Parser], [Cascade.Token], ...), not under [Css]. *)

(** {2:value_parsers Per-type value parsers} *)

module Gradient_direction : sig
  val of_string : string -> (Properties.gradient_direction, Error.t) result
  (** [of_string s] parses [s] as a gradient [<direction>] (a side keyword, an
      [<angle>], or a [<direction> in <color-interpolation>] form). *)
end

(** {2:types Value and property types}

    The CSS value and property types are the ones {!Values}, {!Properties},
    {!Variables} and {!Stylesheet} define, re-exported here with their
    constructors so that [open Cascade.Css] brings them into scope. A
    stylesheet's own AST types and the cascade's internal records stay in
    {!Stylesheet} and {!Properties}. *)

include module type of struct
    include Values_intf
  end
  with type 'a var := 'a Values.var
   and type transition_behavior := Values.transition_behavior

include module type of struct
    include Properties_intf
  end
  with type 'a property := 'a Properties.property
   and type font_display := Properties.font_display
   and type unicode_range := Properties.unicode_range
   and type transforms := Properties.transforms
   and type background_position := Properties.background_position
   and type container_shorthand := Properties.container_shorthand
   and type moz_orient := Properties.moz_orient
   and type custom_property := Properties.custom_property
   and type custom_property_value := Properties.custom_property_value
   and type css_wide := Properties.css_wide
   and type any_property := Properties.any_property
   and type 'a property_value_kind := 'a Properties.property_value_kind

include module type of struct
    include Variables_intf
  end
  with type custom_value := Values.custom_value
   and type any_syntax := Variables.any_syntax

include module type of struct
    include Stylesheet_intf
  end
  with type layer_name := Stylesheet.layer_name
   and type import_rule := Stylesheet.import_rule
   and type 'a property_rule := 'a Stylesheet.property_rule
   and type cascade_layer_candidate := Stylesheet.cascade_layer_candidate
   and type cascade_origin_candidate := Stylesheet.cascade_origin_candidate
   and type declared_value := Stylesheet.declared_value
   and type value_source := Stylesheet.value_source
   and type value := Stylesheet.value
   and type value_processing_stage := Stylesheet.value_processing_stage
   and type url_form := Stylesheet.url_form
   and type namespace_url := Stylesheet.namespace_url
   and type cascade_candidate := Stylesheet.cascade_candidate
   and type rule := Stylesheet.rule
   and type statement := Stylesheet.statement
   and type block := Stylesheet.block
   and type conditional := Stylesheet.conditional
   and type moz_document_condition := Stylesheet.moz_document_condition
   and type viewport_prefix := Stylesheet.viewport_prefix
   and type viewport_descriptor := Stylesheet.viewport_descriptor
   and type keyframe := Stylesheet.keyframe
   and type page_pseudo := Stylesheet.page_pseudo
   and type page_selector := Stylesheet.page_selector
   and type page_descriptor := Stylesheet.page_descriptor
   and type font_palette_base := Stylesheet.font_palette_base
   and type font_palette_descriptor := Stylesheet.font_palette_descriptor
   and type font_feature_values_block := Stylesheet.font_feature_values_block
   and type counter_style_system := Stylesheet.counter_style_system
   and type counter_style_descriptor := Stylesheet.counter_style_descriptor
   and type view_transition_descriptor := Stylesheet.view_transition_descriptor
   and type font_variant_descriptor := Stylesheet.font_variant_descriptor
   and type font_variant_descriptor_value :=
    Stylesheet.font_variant_descriptor_value
   and type page_margin_rule := Stylesheet.page_margin_rule
   and type font_face_descriptor := Stylesheet.font_face_descriptor
   and type stylesheet := Stylesheet.stylesheet
   and type t := Stylesheet.t
   and type 'a edit := 'a Stylesheet.edit

(** {2:css_rules CSS Rules and Stylesheets}

    Core building blocks for CSS rules and stylesheet construction.

    See {:https://www.w3.org/TR/css-syntax-3/ CSS Syntax Module Level 3} and
    {:https://www.w3.org/TR/css-nesting-1/ CSS Nesting Module Level 1}. *)

type declaration = Declaration.declaration
(** The type for CSS declarations (property-value pairs). *)

type statement = Stylesheet.statement
(** The type for CSS statements. *)

val rule :
  selector:Selector.t ->
  ?nested:statement list ->
  ?merge_key:string ->
  declaration list ->
  statement
(** [rule ~selector ?nested ?merge_key declarations] creates a CSS rule
    statement with the given selector and declarations. When [merge_key] is
    provided, the optimizer can combine this rule with other rules sharing the
    same key and identical declarations. *)

val statement_selector : statement -> Selector.t option
(** [statement_selector stmt] returns [Some selector] if the statement is a
    rule, {!constructor-None} otherwise. *)

val as_rule :
  statement -> (Selector.t * declaration list * statement list) option
(** [as_rule stmt] returns [Some (selector, declarations, nested)] if the
    statement is a rule, {!constructor-None} otherwise. The declarations are the
    run written before the first nested statement; a run written after one is a
    nested declarations rule inside [nested], at the position it was written. *)

val as_layer :
  statement -> (Stylesheet.layer_name option * statement list) option
(** [as_layer stmt] returns [Some (name, statements)] if the statement is a
    layer, {!constructor-None} otherwise. *)

val as_media : statement -> (Media.t * statement list) option
(** [as_media stmt] returns [Some (condition, statements)] if the statement is a
    media query, {!constructor-None} otherwise. *)

val as_container :
  statement -> (string option * Container.t option * statement list) option
(** [as_container stmt] returns [Some (name, condition, statements)] if the
    statement is a container query, {!constructor-None} otherwise. *)

val as_supports : statement -> (Supports.t * statement list) option
(** [as_supports stmt] returns [Some (condition, statements)] if the statement
    is a supports query, {!constructor-None} otherwise. *)

val is_nested_media : statement -> bool
(** [is_nested_media stmt] returns [true] if the statement is a media query
    containing bare declarations (CSS nesting style), [false] otherwise. *)

val is_nested_supports : statement -> bool
(** [is_nested_supports stmt] returns [true] if the statement is a supports
    query containing bare declarations (CSS nesting style), [false] otherwise.
*)

val as_declarations : statement -> declaration list option
(** [as_declarations stmt] returns [Some decls] if the statement is a bare
    declarations block (used in CSS nesting), {!constructor-None} otherwise. *)

val unknown_at_rule :
  name:string ->
  prelude:string ->
  ?block:string ->
  unit ->
  (statement, Error.t) result
(** [unknown_at_rule ~name ~prelude ?block ()] is the at-rule [\@name prelude]
    with [block] as its body, or the reason its parts cannot make one. It is the
    way to emit an at-rule cascade has no grammar for, such as one a tool of the
    caller's own defines. Omitting [block] gives the statement form,
    [\@name prelude;].

    [name] is the at-keyword without its [@]. [block] is the text between the
    at-rule's braces, since an unknown at-rule has no grammar to re-serialise a
    body from; [to_string ~minify:true statements] is that text for a block
    cascade does model, so placing one needs no re-read of a printed sheet.

    A part that would not read back as that part is refused rather than printed,
    and the refusal names one at-rule rather than the sheet it sits in: building
    each at-rule on its own loses the malformed one, where re-reading an
    assembled sheet loses every at-rule in it. *)

val with_origin : cascade_origin -> statement list -> statement
(** [with_origin cascade_origin statements] records the cascade origin for a
    stylesheet block. This is an API-level wrapper with no CSS syntax. *)

val as_origin : statement -> (cascade_origin * statement list) option
(** [as_origin stmt] returns [Some (origin, statements)] if the statement is an
    origin wrapper, {!constructor-None} otherwise. *)

val origin_importance_rank : important:bool -> cascade_origin -> int
(** [origin_importance_rank ~important origin] returns the cascade precedence
    rank for the origin/importance criterion. Larger ranks have higher
    precedence. *)

val eval_declaration :
  ?layer_order:string list ->
  ?layer:string ->
  Context.t ->
  declaration ->
  declaration
(** [eval_declaration ctx decl] rewrites [decl] to a more-defined declaration
    under [ctx], preserving unresolved subtrees as CSS syntax. *)

val eval_value :
  ?layer_order:string list ->
  ?layer:string ->
  Context.t ->
  'a Properties.property ->
  'a ->
  declaration
(** [eval_value ctx property value] evaluates [value] in the CSS declaration
    context of [property], returning the evaluated declaration. *)

val eval_rule :
  ?layer_order:string list ->
  ?layer:string ->
  Context.t ->
  Stylesheet.rule ->
  Stylesheet.rule
(** [eval_rule ctx rule] evaluates every declaration in [rule] and its nested
    statements. *)

val eval_stylesheet :
  ?layer_order:string list ->
  ?layer:string ->
  Context.t ->
  Stylesheet.t ->
  Stylesheet.t
(** [eval_stylesheet ctx stylesheet] evaluates every declaration in
    [stylesheet]. *)

val import_layer_name : Stylesheet.import_rule -> Stylesheet.layer_name option
(** [import_layer_name rule] returns the layer name declared by an [@import]
    rule: {!constructor-None} means no layer, [Some []] means an anonymous
    layer, and [Some name] is a named layer. *)

val layer_block_name : statement -> Stylesheet.layer_name option
(** [layer_block_name stmt] returns the declared name of an [@layer] block rule.
    Anonymous layer blocks return [Some []]. *)

val layer_statement_name_list : statement -> Stylesheet.layer_name list option
(** [layer_statement_name_list stmt] returns the declared name list for
    statement-form [@layer] rules. *)

val cascade_layer_precedence_rank :
  layer_order:string list -> important:bool -> string option -> int
(** [cascade_layer_precedence_rank] returns the same-origin layer precedence
    rank for a layer. Larger ranks have higher precedence. *)

val compare_cascade_layer_candidate :
  layer_order:string list ->
  Stylesheet.cascade_layer_candidate ->
  Stylesheet.cascade_layer_candidate ->
  int
(** [compare_cascade_layer_candidate] compares same-origin/same-specificity
    candidates by importance, layer precedence, then source order. *)

val winning_cascade_layer_candidate :
  layer_order:string list ->
  Stylesheet.cascade_layer_candidate list ->
  Stylesheet.cascade_layer_candidate option
(** [winning_cascade_layer_candidate] returns the winning candidate using
    {!compare_cascade_layer_candidate}. *)

val cascade_revert_layer_candidates :
  layer_order:string list ->
  important:bool ->
  current_layer:string option ->
  Stylesheet.cascade_layer_candidate list ->
  Stylesheet.cascade_layer_candidate list
(** [cascade_revert_layer_candidates] returns same-importance candidates in
    lower-priority layers than the current [revert-layer] declaration. *)

val compare_cascade_origin_candidate :
  Stylesheet.cascade_origin_candidate ->
  Stylesheet.cascade_origin_candidate ->
  int
(** [compare_cascade_origin_candidate] compares same-specificity candidates by
    origin/importance precedence, then source order. *)

val winning_cascade_origin_candidate :
  Stylesheet.cascade_origin_candidate list ->
  Stylesheet.cascade_origin_candidate option
(** [winning_cascade_origin_candidate] returns the winning candidate using
    {!compare_cascade_origin_candidate}. *)

val cascade_revert_origin_candidates :
  important:bool ->
  current_origin:cascade_origin ->
  Stylesheet.cascade_origin_candidate list ->
  Stylesheet.cascade_origin_candidate list
(** [cascade_revert_origin_candidates] returns same-importance candidates in the
    origins exposed by a [revert] declaration from [current_origin]. *)

val declared_values :
  ?property:string -> declaration list -> Stylesheet.declared_value list
(** [declared_values ?property declarations] returns declared values in source
    order, optionally filtered to one property. *)

val cascaded_value : Stylesheet.cascade_origin_candidate list -> string option
(** [cascaded_value candidates] returns the winning cascaded value payload, or
    {!constructor-None} when no candidate contributes a value. *)

val compare_cascade_candidate :
  layer_order:string list ->
  Stylesheet.cascade_candidate ->
  Stylesheet.cascade_candidate ->
  int
(** [compare_cascade_candidate ~layer_order a b] compares full same-property
    cascade candidates by origin/importance, layer, specificity, scoping
    proximity, and source order. *)

val winning_cascade_candidate :
  layer_order:string list ->
  Stylesheet.cascade_candidate list ->
  Stylesheet.cascade_candidate option
(** [winning_cascade_candidate ~layer_order candidates] returns the highest
    priority full cascade candidate. *)

val value :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  cascaded:string option ->
  Stylesheet.value
(** [value ~inherits ~initial ~inherited ~cascaded] models the defaulting step
    from cascaded value to specified value for the non-layout cases represented
    by this library. *)

val specified_value_after_revert :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  Stylesheet.cascade_origin_candidate list ->
  Stylesheet.value
(** [specified_value_after_revert] chains [revert] rollbacks through the origin
    stack until a non-[revert] winner remains, then defaults. *)

val specified_value_after_revert_layer :
  inherits:bool ->
  initial:string ->
  inherited:string option ->
  layer_order:string list ->
  Stylesheet.cascade_layer_candidate list ->
  Stylesheet.value
(** [specified_value_after_revert_layer] is the [revert-layer] analogue, chained
    through the layer stack. *)

val value_processing_requires_document_context :
  Stylesheet.value_processing_stage -> bool
(** [value_processing_requires_document_context stage] reports whether [stage]
    needs caller-supplied document, layout, rendering, or device context rather
    than CSS text alone. *)

val map :
  (Selector.t -> declaration list -> statement) ->
  statement list ->
  statement list
(** [map f stmts] applies [f] to every rule in [stmts]: the ones at the top
    level, the ones inside a conditional group at-rule such as [@media],
    [@supports], [@layer], [@container], [@scope] or [@starting-style], however
    deeply nested, and the ones nested inside a rule.

    - Traversal is depth-first, and a statement that is not a rule is kept with
      its block rewritten.
    - Non-rule statements maintain their relative order.
    - When [f] returns a rule holding no nested statements, the original nested
      tree is kept with [map] applied to it; one holding its own replaces it. *)

val sort :
  (Selector.t * declaration list -> Selector.t * declaration list -> int) ->
  statement list ->
  statement list
(** [sort cmp stmts] reorders the rules of [stmts] with [cmp], and the rules of
    every block below them: inside a conditional group at-rule such as [@media],
    [@supports], [@layer], [@container], [@scope] or [@starting-style], however
    deeply nested, and inside the nested statements of a rule.

    - Each block is sorted on its own, so a rule never leaves the block it sits
      in.
    - Sort is stable: rules [cmp] calls equal maintain their relative order.
    - Non-rule statements sort after the rules of their block and maintain their
      relative order among themselves, so an [@else] still follows the [@when]
      it answers. *)

(** Existential type for property information that preserves type safety *)
type property_info =
  | Property_info : {
      name : string;
      syntax : 'a Variables.syntax;
      inherits : bool;
      initial_value : 'a option;
    }
      -> property_info

val as_property : statement -> property_info option
(** [as_property stmt] returns [Some (Property_info {...})] if the statement is
    a [@property] declaration, {!constructor-None} otherwise. The existential
    type preserves the relationship between syntax type and initial value type.
*)

type keyframe = Stylesheet.keyframe
(** Type for keyframe selectors and their declarations *)

val keyframe : selector:string -> declarations:declaration list -> keyframe
(** [keyframe ~selector ~declarations] is a single keyframe whose [selector] is
    parsed via {!Keyframe.selector_of_string} (e.g. ["from"], ["to"], ["50%"],
    ["50%, 100%"]). Raises [Invalid_argument] if [selector] is not a valid
    keyframe selector. *)

val keyframes : string -> keyframe list -> statement
(** [keyframes name frames] creates a [@keyframes] rule.

    Example:
    {[
    open Cascade.Css

    let pulse =
      keyframes "pulse"
        [
          keyframe ~selector:"50%"
            ~declarations:[ opacity (Opacity_number 0.5) ];
        ]
    ]}
    produces [@keyframes pulse { 50% { opacity: 0.5 } }]. *)

val as_keyframes : statement -> (string * keyframe list) option
(** [as_keyframes stmt] returns [Some (name, frames)] if the statement is a
    [@keyframes] animation, {!constructor-None} otherwise. *)

val as_font_face : statement -> Stylesheet.font_face_descriptor list option
(** [as_font_face stmt] returns [Some descriptors] if the statement is a
    [@font-face] rule, {!constructor-None} otherwise. *)

val as_import : statement -> Stylesheet.import_rule option
(** [as_import stmt] returns [Some import_rule] if the statement is an [@import]
    rule, {!constructor-None} otherwise. *)

(** {2:at_rules At-Rules}

    At-rules are CSS statements that instruct CSS how to behave. They begin with
    an at sign (@) followed by an identifier and include everything up to the
    next semicolon or CSS block.

    See {:https://www.w3.org/TR/css-conditional-5/ CSS Conditional Rules Module
    Level 5} and {:https://developer.mozilla.org/en-US/docs/Web/CSS/At-rule MDN
    At-rules}. *)

(** {2:stylesheet_construction Stylesheet Construction}

    Tools for building complete CSS stylesheets from rules and declarations.

    See {:https://www.w3.org/TR/css-cascade-5/ CSS Cascading and Inheritance
    Level 5}, which gives the origins, the [@layer] ordering and the CSS-wide
    keywords the builders below write. *)

type t = Stylesheet.t
(** The type for CSS stylesheets. *)

val empty : t
(** [empty] is an empty stylesheet. *)

val concat : t list -> t
(** [concat stylesheets] concatenates multiple stylesheets into one. *)

val v : statement list -> t
(** [v statements] creates a stylesheet from a list of statements. *)

val rule_statements : t -> statement list
(** [rule_statements t] returns the top-level rule statements from the
    stylesheet. *)

val statements : t -> statement list
(** [statements t] returns all top-level statements from the stylesheet. *)

val equal_statement : statement -> statement -> bool
(** [equal_statement a b] is {!Stylesheet.equal_statement}: whether [a] and [b]
    are the same statement, each part read through the equality its own module
    states. *)

val hash_statement : statement -> int
(** [hash_statement stmt] is {!Stylesheet.hash_statement}: a fingerprint
    consistent with {!equal_statement}, for keying a statement in a hash table
    without rendering it to CSS text. *)

val fold : ('a -> statement -> 'a) -> 'a -> t -> 'a
(** [fold f acc css] folds [f] over every statement in [css] and over every
    statement reachable from one, in source order: a rule nested in a rule, a
    block at-rule inside a group, and whatever those hold in turn. The walk
    descends through {!Stylesheet.statement_children}, so it reaches every
    statement the AST can hold rather than a listed set of at-rules.

    Example: Collect all selectors from all rules (including nested ones):
    {[
    open Cascade

    let selectors css =
      Css.fold
        (fun acc stmt ->
          match Css.as_rule stmt with
          | Some (sel, _, _) -> Css.Selector.to_string sel :: acc
          | None -> acc)
        [] css
    ]} *)

val media_queries : t -> (Media.t * statement list) list
(** [media_queries t] is every [@media] in [t], at any depth, paired with the
    rule statements below its brace. A query inside a group at-rule counts, and
    a rule nested in another rule or held by an inner group is one of the
    query's rules; a nested rule keeps the relative selector it was written
    with. *)

val layers : t -> Stylesheet.layer_name list
(** [layers t] is every cascade layer [t] declares, one path per layer ([a.b] is
    the sublayer [b] of [a], however it was written), in the order the sheet
    first names them. Each path is its idents, so a [.] one ident carries is not
    the separator between two. A layer named inside a conditional group counts:
    the group decides whether its contents apply, not whether the layer exists.
    A sublayer of an anonymous [@layer { ... }] has no name to report.

    This is what a sheet declares, not the order a cascade resolves in.
    {!Resolve.layer_order} answers that, and leaves out a layer named inside any
    block the resolver does not enter: a conditional group rule,
    [@starting-style], [@scope] or an origin wrapper. *)

(** {3 AST Introspection Helpers} *)

val layer_block : Stylesheet.layer_name -> t -> statement list option
(** [layer_block name sheet] is the statements of the layer [name], wherever it
    is declared and whatever form declares it: a dotted name, a nested block, or
    a block inside a conditional group. It is {!constructor-None} when no
    [@layer] block opens that layer, so a name only an [@layer a, b;] statement
    declares is {!constructor-None} as well. *)

val rules_of_statements : statement list -> (Selector.t * declaration list) list
(** [rules_of_statements stmts] extracts all CSS rules (selector + declarations)
    from a list of statements, filtering out at-rules and other non-rule
    statements. *)

val custom_prop_names : declaration list -> string list
(** [custom_prop_names decls] extracts all custom property names from a list of
    declarations. *)

val theme_guarded : var_name:string -> declaration -> declaration
(** [theme_guarded ~var_name decl] wraps [decl] so it is only emitted when
    [var_name] is present in the theme. *)

val as_theme_guarded : declaration -> (string * declaration) option
(** [as_theme_guarded decl] returns [Some (var_name, inner_decl)] if [decl] is a
    theme-guarded declaration, {!constructor-None} otherwise. *)

val custom_props_of_rules : (Selector.t * declaration list) list -> string list
(** [custom_props_of_rules rules] extracts all custom property names from the
    declarations in the rules. *)

val custom_props : ?layer:Stylesheet.layer_name -> t -> string list
(** [custom_props ?layer sheet] is the name of every custom property [sheet]
    declares for an element: the ones in a style rule or a bare nesting block,
    at the top level and inside a conditional group at-rule such as [@media],
    [@supports], [@container], [@scope] or [@starting-style], however deeply
    nested. A name declared in [@keyframes], [@page], [@position-try] or
    [@supports-condition] belongs to another cascade origin or to no element at
    all (CSS Cascading 5 sec. 6.1) and is not among them. When [layer] is given,
    the names are those declared inside the [@layer] of that name. *)

val media : condition:Media.t -> statement list -> statement
(** [media ~condition statements] creates a [@media] statement with the given
    condition. *)

val media_nested : condition:Media.t -> declaration list -> statement
(** [media_nested ~condition declarations] creates a [@media] statement for CSS
    nesting, containing bare declarations (no selector). Used inside rules where
    the selector is inherited from the parent. *)

val declarations : declaration list -> statement
(** [declarations decls] creates a bare declarations block (used in CSS
    nesting). *)

val layer : ?name:Stylesheet.layer_name -> statement list -> statement
(** [layer ?name statements] creates a [@layer] statement with the given
    statements. *)

val layer_decl : Stylesheet.layer_name list -> statement
(** [layer_decl names] creates a [@layer] declaration statement that declares
    layer names without any content (e.g.,
    [@layer theme, base, components, utilities;]). *)

val layer_of : ?name:Stylesheet.layer_name -> t -> t
(** [layer_of ?name stylesheet] wraps an entire stylesheet in [@layer],
    preserving [@supports] and other at-rules within it. *)

val container :
  ?name:string -> ?condition:Container.t -> statement list -> statement
(** [container ?name ~condition statements] creates a [@container] statement
    with the given statements. *)

val supports : condition:Supports.t -> statement list -> statement
(** [supports ~condition statements] creates a [@supports] statement with the
    given condition. *)

val starting_style : statement list -> statement
(** [starting_style statements] creates a [@starting-style] statement with the
    given statements. Used for CSS entry animations. *)

val starting_style_nested : declaration list -> statement
(** [starting_style_nested declarations] creates a [@starting-style] statement
    for CSS nesting, containing bare declarations (no selector). Used inside
    rules where the selector is inherited from the parent. *)

(** {1 Declarations}

    Core value types and declaration building blocks. *)

(** {2:variables Custom Properties (Variables)}

    See {:https://www.w3.org/TR/css-variables-1/ CSS Custom Properties for
    Cascading Variables Module Level 1} for [var()] and
    {:https://www.w3.org/TR/css-properties-values-api-1/ CSS Properties and
    Values API Level 1} for the [@property] registration. *)

type 'a var = 'a Values.var
(** The type of CSS variable holding values of type ['a]. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without [--]). *)

val var_layer : 'a var -> string option
(** [var_layer v] is the optional layer where [v] is defined. *)

val with_fallback : 'a var -> 'a -> 'a var
(** [with_fallback var_ref fallback_value] creates a new variable reference with
    the same variable name but a different fallback value. This is useful when
    you need to reference a variable from another module with a specific
    fallback, without creating a declaration. *)

val vars_of_rules : statement list -> any_var list
(** [vars_of_rules statements] is {!vars_of_stylesheet} of [statements]: a
    statement list is a stylesheet, and the two answer the same question. *)

val vars_of_declarations : declaration list -> any_var list
(** [vars_of_declarations decls] extracts all CSS variables referenced in the
    declarations list. *)

val vars_of_stylesheet : t -> any_var list
(** [vars_of_stylesheet stylesheet] is every variable [stylesheet] references,
    from the declarations of every statement it holds: a rule nested in a rule,
    a rule inside any grouping at-rule, and an at-rule carrying declarations of
    its own such as [@keyframes] or [@page]. Deduplicated, in source order. *)

val any_var_name : any_var -> string
(** [any_var_name v] is the name of a CSS variable (with [--] prefix). *)

val custom_declarations : ?layer:string -> declaration list -> declaration list
(** [custom_declarations ?layer decls] is only the custom property declarations
    from [decls]. If [layer] is provided, only declarations from that layer are
    returned. *)

val all : Properties.css_wide -> declaration
(** [all v] is the {{:https://www.w3.org/TR/css-cascade-5/#all-shorthand} all}
    shorthand. It resets every longhand to [v] but the two writing-mode ones CSS
    Cascading 5 sec. 3.3 excepts. *)

(** {2:core_types Core Types & Calculations}

    Fundamental types for CSS values, variables, and calculations that underpin
    the entire CSS system.

    See {:https://www.w3.org/TR/css-variables-1/ CSS Custom Properties for
    Cascading Variables Module Level 1} and
    {:https://www.w3.org/TR/css-values-4/ CSS Values and Units Module Level 4}.
*)

(** {2:values CSS Values & Units}

    Core value types used across CSS properties.

    See {:https://www.w3.org/TR/css-values-4/ CSS Values and Units Module Level
    4}. *)

module Calc = Values.Calc
(** Builder functions for calc() expressions. *)

type 'a property = 'a Properties.property
(** GADT for typed CSS properties. *)

(* CSS Color 4 [none] sentinel *)

(* calc(...) that resolves to a % *)

val hex : string -> color
(** [hex s] is a hexadecimal color. Accepts with or without leading [#].
    Examples: [hex "#3b82f6"], [hex "ffffff"]. Raises [Invalid_argument] when
    [s] is not one of [#rgb], [#rrggbb], [#rgba] or [#rrggbbaa]; see
    {!val-hex_opt} to decide. *)

val hex_opt : string -> color option
(** [hex_opt s] is {!val-hex} without the exception: the colour when [s] is a
    hex spelling, and nothing otherwise. *)

val rgb : ?alpha:float -> int -> int -> int -> color
(** [rgb ?alpha r g b] is an RGB color (0-255 components) with optional alpha.
*)

val hsl : float -> float -> float -> color
(** [hsl h s l] is an HSL color with h in degrees, s and l in percentages
    (0-100). *)

val hsla : float -> float -> float -> float -> color
(** [hsla h s l a] is an HSLA color with alpha in [0., 1.]. *)

val hwb : float -> float -> float -> color
(** [hwb h w b] is an HWB color with h in degrees, w and b in percentages
    (0-100). *)

val hwba : float -> float -> float -> float -> color
(** [hwba h w b a] is an HWB color with alpha in [0., 1.]. *)

val oklch : float -> float -> float -> color
(** [oklch l c h] is an OKLCH color. L in percentage (0-100), h in degrees. *)

val oklcha : float -> float -> float -> float -> color
(** [oklcha l c h a] is an OKLCH color with alpha in [0., 1.]. *)

val oklch_none_hue : float -> float -> color
(** [oklch_none_hue l c] is an OKLCH color whose hue is [none]. The hue of an
    achromatic color is powerless, and [none] keeps the component missing, so
    interpolation takes the other color's hue rather than 0. *)

val oklab : float -> float -> float -> color
(** [oklab l a b] is an OKLAB color. L in percentage (0-100). *)

val oklaba : float -> float -> float -> float -> color
(** [oklaba l a b alpha] is an OKLAB color with alpha in [0., 1.]. *)

val oklaba_none_zeros : float -> float -> float -> float -> color
(** [oklaba_none_zeros l a b alpha] is like {!val-oklaba} but uses [none] for
    zero a/b components. *)

val lch : float -> float -> float -> color
(** [lch l c h] is an LCH color. L in percentage (0-100), h in degrees. *)

val lcha : float -> float -> float -> float -> color
(** [lcha l c h a] is an LCH color with alpha in [0., 1.]. *)

val color_name : color_name -> color
(** [color_name n] is a named color as defined in the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/named-color} CSS Color
     specification}. *)

val current_color : color
(** [current_color] is the CSS
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#currentcolor_keyword}
     currentcolor} value. *)

val transparent : color
(** [transparent] is the CSS
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color_value#transparent_keyword}
     transparent} value. *)

val color_mix :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  ?percent1:float ->
  ?percent2:float ->
  color ->
  color ->
  color
(** [color_mix ?in_space ?percent1 ?percent2 c1 c2] is a
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color_value/color-mix}
     color-mix} value. Defaults: [in_space = Srgb], [percent1 = None],
    [percent2 = None]. *)

val color_mix_var_percent :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  color ->
  color ->
  color
(** [color_mix_var_percent ?in_space ?hue ~var_name c1 c2] is like
    {!val-color_mix} but uses a CSS var reference for the first percentage. *)

val color_mix_var_pct_fallback :
  ?in_space:color_space ->
  ?hue:hue_interpolation ->
  var_name:string ->
  fallback:percentage fallback ->
  color ->
  color ->
  color
(** [color_mix_var_pct_fallback ?in_space ?hue ~var_name ~fallback c1 c2] is
    like {!val-color_mix_var_percent} but with an explicit fallback on the
    percentage variable. Used for named opacity modifiers. *)

val ratio : float -> float -> aspect_ratio
(** [ratio width height] is an [aspect-ratio] value such as [16 / 9]. *)

val auto_ratio : float -> float -> aspect_ratio
(** [auto_ratio width height] is an [aspect-ratio] value such as [auto 16 / 9].
*)

val important : declaration -> declaration
(** [important decl] is [decl] marked as [!important]. *)

val declaration_is_important : declaration -> bool
(** [declaration_is_important decl] returns [true] if [decl] has the
    [!important] flag. *)

val declaration_name : declaration -> string
(** [declaration_name decl] returns the property name of [decl]. *)

val declaration_value : ?minify:bool -> ?inline:bool -> declaration -> string
(** [declaration_value ~minify ~inline decl] returns the value of [decl] as a
    string. If [minify] is [true] (default: [false]), the output is minified. If
    [inline] is [true] (default: [false]), variables are resolved to their
    default values. *)

val declaration_value_for_equivalence : declaration -> string
(** [declaration_value_for_equivalence decl] is the minified value of [decl], so
    a structural diff keys a typed property on its shortest spelling and
    [padding: 0.50px] and [padding: .5px] compare equal.

    A custom-property token stream, whose bytes {!declaration_value} keeps
    verbatim, also loses the whitespace CSS reads as nothing: the whitespace CSS
    Values 4 (ED) sec. 10.8 leaves optional around a math [*] and [/], and the
    whitespace a closing bracket already accounts for. So [--r: 16 / 9] and
    [--r: 16/9] compare equal, while the space sec. 10.8 requires around a math
    [+] or [-], and the space beside a [var()], [env()] or [attr()] that sec.
    2.5 substitutes textually into its neighbour, keep two spellings apart.

    A quoted family name in that stream is rewritten as the equivalent unquoted
    [<ident>] sequence, one word or several, when a generic family in the stream
    proves the stream is a font-family list, where CSS Fonts 4 sec. 2.1.1 spells
    the one name both ways: [--font: ui-sans-serif,"Noto Color Emoji"] and
    [--font: ui-sans-serif,Noto Color Emoji] compare equal, as do
    [--font: "Arial",sans-serif] and [--font: Arial,sans-serif]. Without that
    proof the stream is arbitrary tokens, in which one [<string>] is not an
    [<ident>] sequence, and the two spellings keep distinct keys.

    Not for emission, where every one of these forms stays verbatim. *)

(** {1 Property Categories}

    CSS properties organized by functionality and usage patterns. *)

(** {2:box_model Box Model & Sizing}

    The CSS Box Model defines how element dimensions are calculated and how
    space is distributed around content. This includes width/height properties,
    padding, margins, and box sizing behavior.

    @see <https://www.w3.org/TR/css-box-3/> CSS Box Model Module Level 3
    @see <https://www.w3.org/TR/css-sizing-3/>
      CSS Intrinsic & Extrinsic Sizing Module Level 3 *)

val width : length -> declaration
(** [width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/width} width} property.
*)

val height : length -> declaration
(** [height len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/height} height}
    property. *)

val min_width : length -> declaration
(** [min_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-width} min-width}
    property. *)

val max_width : length -> declaration
(** [max_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-width} max-width}
    property. *)

val min_height : length -> declaration
(** [min_height len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-height} min-height}
    property. *)

val max_height : length -> declaration
(** [max_height len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-height} max-height}
    property. *)

val inline_size : length -> declaration
(** [inline_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inline-size}
     inline-size} logical property. *)

val min_inline_size : length -> declaration
(** [min_inline_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-inline-size}
     min-inline-size} logical property. *)

val max_inline_size : length -> declaration
(** [max_inline_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-inline-size}
     max-inline-size} logical property. *)

val block_size : length -> declaration
(** [block_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/block-size} block-size}
    logical property. *)

val min_block_size : length -> declaration
(** [min_block_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/min-block-size}
     min-block-size} logical property. *)

val max_block_size : length -> declaration
(** [max_block_size len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/max-block-size}
     max-block-size} logical property. *)

val padding : length list -> declaration
(** [padding values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding} padding}
    shorthand property. Accepts 1-4 values. *)

val padding_top : length -> declaration
(** [padding_top len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-top}
     padding-top} property. *)

val padding_right : length -> declaration
(** [padding_right len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-right}
     padding-right} property. *)

val padding_bottom : length -> declaration
(** [padding_bottom len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-bottom}
     padding-bottom} property. *)

val padding_left : length -> declaration
(** [padding_left len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-left}
     padding-left} property. *)

val margin : length list -> declaration
(** [margin values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin} margin}
    shorthand property. Accepts 1-4 values. *)

val margin_top : length -> declaration
(** [margin_top len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-top} margin-top}
    property. *)

val margin_right : length -> declaration
(** [margin_right len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-right}
     margin-right} property. *)

val margin_bottom : length -> declaration
(** [margin_bottom len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-bottom}
     margin-bottom} property. *)

val margin_left : length -> declaration
(** [margin_left len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-left}
     margin-left} property. *)

val box_sizing : box_sizing -> declaration
(** [box_sizing sizing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-sizing} box-sizing}
    property. *)

val field_sizing : field_sizing -> declaration
(** [field_sizing sizing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/field-sizing}
     field-sizing} property. *)

val caption_side : caption_side -> declaration
(** [caption_side side] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/caption-side}
     caption-side} property. *)

val aspect_ratio : aspect_ratio -> declaration
(** [aspect_ratio ratio] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/aspect-ratio}
     aspect-ratio} property. *)

(** {2:logical_properties Logical Properties}

    CSS Logical Properties provide writing-mode-relative property equivalents
    for physical properties. These adapt to different writing directions and
    text orientations.

    @see <https://www.w3.org/TR/css-logical-1/>
      CSS Logical Properties and Values Level 1 *)

val border_inline_start_width : border_width -> declaration
(** [border_inline_start_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-width}
     border-inline-start-width} property. *)

val border_inline_end_width : border_width -> declaration
(** [border_inline_end_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-width}
     border-inline-end-width} property. *)

val border_block_start_width : border_width -> declaration
(** [border_block_start_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-width}
     border-block-start-width} property. *)

val border_block_end_width : border_width -> declaration
(** [border_block_end_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-width}
     border-block-end-width} property. *)

val border_inline_start_color : color -> declaration
(** [border_inline_start_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-color}
     border-inline-start-color} property. *)

val border_inline_end_color : color -> declaration
(** [border_inline_end_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-color}
     border-inline-end-color} property. *)

val border_block_start_color : color -> declaration
(** [border_block_start_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-color}
     border-block-start-color} property. *)

val border_block_end_color : color -> declaration
(** [border_block_end_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-color}
     border-block-end-color} property. *)

val padding_inline_start : length -> declaration
(** [padding_inline_start len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-start}
     padding-inline-start} property. *)

val padding_inline_end : length -> declaration
(** [padding_inline_end len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline-end}
     padding-inline-end} property. *)

val padding_inline : length list -> declaration
(** [padding_inline lens] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-inline}
     padding-inline} shorthand property. *)

val padding_block : length list -> declaration
(** [padding_block lens] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block}
     padding-block} shorthand property. *)

val padding_block_start : length -> declaration
(** [padding_block_start len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block-start}
     padding-block-start} property. *)

val padding_block_end : length -> declaration
(** [padding_block_end len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/padding-block-end}
     padding-block-end} property. *)

val margin_inline : length -> declaration
(** [margin_inline len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline}
     margin-inline} property with a length value. *)

val margin_inline_start : length -> declaration
(** [margin_inline_start len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-start}
     margin-inline-start} property. *)

val margin_inline_end : length -> declaration
(** [margin_inline_end len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-inline-end}
     margin-inline-end} property. *)

val margin_block : length -> declaration
(** [margin_block len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block}
     margin-block} property with a length value. *)

val margin_block_start : length -> declaration
(** [margin_block_start len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block-start}
     margin-block-start} property. *)

val margin_block_end : length -> declaration
(** [margin_block_end len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/margin-block-end}
     margin-block-end} property. *)

(** {2:display_positioning Display & Positioning}

    Controls how elements are displayed and positioned in the document flow.
    This includes the display model, positioning schemes, and stacking context.

    @see <https://www.w3.org/TR/css-display-3/> CSS Display Module Level 3
    @see <https://www.w3.org/TR/css-position-3/>
      CSS Positioned Layout Module Level 3
    @see <https://www.w3.org/TR/css-break-3/> CSS Fragmentation Module Level 3
*)

val display : display -> declaration
(** [display d] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/display} display}
    property. *)

val position : position -> declaration
(** [position p] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/position} position}
    property. *)

val inset : length list -> declaration
(** [inset len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset} inset} property
    for positioned elements. *)

val inset_inline : length list -> declaration
(** [inset_inline len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset-inline}
     inset-inline} logical property. *)

val inset_inline_start : length -> declaration
(** [inset_inline_start len] is the inset-inline-start logical property. *)

val inset_inline_end : length -> declaration
(** [inset_inline_end len] is the inset-inline-end logical property. *)

val inset_block : length list -> declaration
(** [inset_block len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/inset-block}
     inset-block} logical property. *)

val inset_block_start : length -> declaration
(** [inset_block_start len] is the inset-block-start logical property. *)

val inset_block_end : length -> declaration
(** [inset_block_end len] is the inset-block-end logical property. *)

val top : length -> declaration
(** [top len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/top} top} property for
    positioned elements. *)

val right : length -> declaration
(** [right len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/right} right} property
    for positioned elements. *)

val bottom : length -> declaration
(** [bottom len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/bottom} bottom} property
    for positioned elements. *)

val left : length -> declaration
(** [left len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/left} left} property for
    positioned elements. *)

val z_index : z_index -> declaration
(** [z_index z] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/z-index} z-index}
    property. *)

val z_index_auto : declaration
(** [z_index_auto] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/z-index} z-index}
    property set to [auto]. *)

val isolation : isolation -> declaration
(** [isolation iso] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/isolation} isolation}
    property for stacking context control. *)

val break_before : break_value -> declaration
(** [break_before v] is the break-before property. *)

val break_after : break_value -> declaration
(** [break_after v] is the break-after property. *)

val break_inside : break_inside_value -> declaration
(** [break_inside v] is the break-inside property. *)

val page_break_before : page_break_value -> declaration
(** [page_break_before v] is the legacy [page-break-before] property. *)

val page_break_after : page_break_value -> declaration
(** [page_break_after v] is the legacy [page-break-after] property. *)

val page_break_inside : page_break_inside_value -> declaration
(** [page_break_inside v] is the legacy [page-break-inside] property. *)

val columns_count : int -> columns_value
(** [columns_count count] is a column-count value for the {!val-columns}
    shorthand. *)

val columns_width : length -> columns_value
(** [columns_width width] is a column-width value for the {!val-columns}
    shorthand. *)

val columns_both : length -> int -> columns_value
(** [columns_both width count] is a combined {!val-columns} shorthand value. *)

val columns : columns_value -> declaration
(** [columns v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/columns} columns}
    property for multi-column layout. *)

val column_span : column_span -> declaration
(** [column_span v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-span}
     column-span} property. *)

val column_width : column_width -> declaration
(** [column_width v] is the [column-width] longhand of {!val-columns}. *)

val column_count : column_count -> declaration
(** [column_count v] is the [column-count] longhand of {!val-columns}. *)

val column_height : column_height -> declaration
(** [column_height v] is the [column-height] property. *)

val column_wrap : column_wrap -> declaration
(** [column_wrap v] is the [column-wrap] property. *)

val visibility : visibility -> declaration
(** [visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/visibility} visibility}
    property. *)

val float : float_side -> declaration
(** [float side] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/float} float} property.
*)

val clear : clear -> declaration
(** [clear clr] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clear} clear} property.
*)

val overflow : overflow -> declaration
(** [overflow ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow} overflow}
    property. *)

val overflow_x : overflow -> declaration
(** [overflow_x ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-x} overflow-x}
    property. *)

val overflow_y : overflow -> declaration
(** [overflow_y ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-y} overflow-y}
    property. *)

val content_string : string -> content
(** [content_string value] is a quoted string content item. *)

val content_attr : string -> content
(** [content_attr name] is an [attr(name)] content item. *)

val content_counter : string -> content
(** [content_counter name] is a [counter(name)] content item. *)

val content_counters : string -> string -> content
(** [content_counters name separator] is a [counters(name, separator)] content
    item. *)

val content_list : content list -> content
(** [content_list items] is a space-separated content value. *)

val counter_item : ?value:int -> string -> counter_item
(** [counter_item ?value name] is one named counter item. *)

val counter_set : counter_item list -> counter_set
(** [counter_set items] is a counter-reset/increment/set list. *)

val content : content -> declaration
(** [content c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/content} content}
    property. *)

val counter_reset : counter_set -> declaration
(** [counter_reset c] is the CSS [counter-reset] property. *)

val counter_increment : counter_set -> declaration
(** [counter_increment c] is the CSS [counter-increment] property. *)

val object_fit : object_fit -> declaration
(** [object_fit fit] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit} object-fit}
    property. *)

val object_position : position_value -> declaration
(** [object_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-position}
     object-position} property. *)

val position_xy : length -> length -> position_value
(** [position_xy x y] is a two-axis position value. *)

val position_length : length -> position_value
(** [position_length value] is a one-value position. *)

val text_overflow_string : string -> text_overflow
(** [text_overflow_string value] is a custom text-overflow marker. *)

val text_overflow_pair : text_overflow -> text_overflow -> text_overflow
(** [text_overflow_pair start end_] is the two-value text-overflow form. *)

val text_overflow : text_overflow -> declaration
(** [text_overflow ov] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-overflow}
     text-overflow} property. *)

val text_wrap : text_wrap -> declaration
(** [text_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-wrap} text-wrap}
    property. *)

val text_wrap_mode : text_wrap_mode -> declaration
(** [text_wrap_mode wrap] is the CSS [text-wrap-mode] property. *)

val text_underline_position : text_underline_position -> declaration
(** [text_underline_position position] is the CSS [text-underline-position]
    property. *)

val text_box_edge : text_box_edge -> declaration
(** [text_box_edge edge] is the CSS [text-box-edge] property. *)

val inline_sizing : inline_sizing -> declaration
(** [inline_sizing sizing] is the CSS [inline-sizing] property. *)

val line_fit_edge : line_fit_edge -> declaration
(** [line_fit_edge edge] is the CSS [line-fit-edge] property. *)

val interpolate_size : interpolate_size -> declaration
(** [interpolate_size sizing] is the CSS [interpolate-size] property. *)

val min_intrinsic_sizing : min_intrinsic_sizing -> declaration
(** [min_intrinsic_sizing sizing] is the CSS [min-intrinsic-sizing] property. *)

val ruby_align : ruby_align -> declaration
(** [ruby_align align] is the CSS [ruby-align] property. *)

val ruby_merge : ruby_merge -> declaration
(** [ruby_merge merge] is the CSS [ruby-merge] property. *)

val ruby_overhang : ruby_overhang -> declaration
(** [ruby_overhang overhang] is the CSS [ruby-overhang] property. *)

val ruby_position : ruby_position -> declaration
(** [ruby_position position] is the CSS [ruby-position] property. *)

val backface_visibility : backface_visibility -> declaration
(** [backface_visibility vis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/backface-visibility}
     backface-visibility} property (3D transforms). *)

val content_visibility : content_visibility -> declaration
(** [content_visibility vis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/content-visibility}
     content-visibility} property. *)

val quotes : quotes -> declaration
(** [quotes q] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/quotes} quotes}
    property. *)

val list_style_position : list_style_position -> declaration
(** [list_style_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-position}
     list-style-position} property. *)

(** {2:colors_backgrounds Colors & Backgrounds}

    Properties for controlling foreground colors, background colors, images, and
    related visual styling for element backgrounds.

    @see <https://www.w3.org/TR/css-color-4/> CSS Color Module Level 4
    @see <https://www.w3.org/TR/css-color-5/> CSS Color Module Level 5
    @see <https://www.w3.org/TR/css-backgrounds-3/>
      CSS Backgrounds and Borders Module Level 3
    @see <https://www.w3.org/TR/css-images-4/> CSS Images Module Level 4 *)

val background_size_pair : length -> length -> background_size
(** [background_size_pair width height] is a two-value [background-size]. *)

val gradient_stops : gradient_stop list -> gradient_stop
(** [gradient_stops stops] groups multiple gradient stops, usually for variable
    fallbacks. *)

val gradient_hint_length : length -> gradient_stop
(** [gradient_hint_length value] is a length interpolation hint. *)

val gradient_hint_percentage : percentage -> gradient_stop
(** [gradient_hint_percentage value] is a percentage interpolation hint. *)

val radial_gradient_config :
  ?shape:radial_shape ->
  ?size:radial_size ->
  ?position:position_value ->
  ?interpolation:color_interpolation ->
  unit ->
  radial_gradient_config
(** [radial_gradient_config ?shape ?size ?position ?interpolation ()] builds a
    radial-gradient prefix. *)

val conic_gradient_config :
  ?angle:angle ->
  ?position:position_value ->
  ?interpolation:color_interpolation ->
  unit ->
  conic_gradient_config
(** [conic_gradient_config ?angle ?position ?interpolation ()] builds a
    conic-gradient prefix. *)

val radius : length -> border_radius
(** [radius len] is a one-value [border-radius] shorthand value.

    For example, [border_radius (radius (Rem 0.375))] renders
    [border-radius: 0.375rem]. *)

val object_view_box_inset :
  ?right:length -> ?bottom:length -> ?left:length -> length -> object_view_box
(** [object_view_box_inset ?right ?bottom ?left top] is an [inset()] object view
    box. *)

val object_view_box : object_view_box -> declaration
(** [object_view_box box] is the CSS [object-view-box] property. *)

(* Mask-related types *)

val mask_layer :
  ?image:background_image ->
  ?position:position_value ->
  ?size:background_size ->
  ?repeat:background_repeat ->
  ?origin:mask_box ->
  ?clip:mask_box ->
  ?mode:mask_mode ->
  ?composite:mask_composite ->
  unit ->
  mask_layer
(** [mask_layer ?image ?position ?size ?repeat ?origin ?clip ?mode ?composite
     ()] is one layer for the {!val-mask} shorthand. *)

val mask_layers : mask_layer list -> mask
(** [mask_layers layers] is a comma-separated {!val-mask} shorthand value. *)

val background_shorthand :
  ?color:color ->
  ?image:background_image ->
  ?position:position_value ->
  ?size:background_size ->
  ?repeat:background_repeat ->
  ?attachment:background_attachment ->
  ?clip:background_box ->
  ?origin:background_box ->
  unit ->
  background
(** [background_shorthand ?color ?image ?position ?size ?repeat ?attachment
     ?clip ?origin ()] is the background shorthand.
    - [color]: background color
    - [image]: background image (url or gradient)
    - [position]: image position
    - [size]: image size (cover, contain, or specific size)
    - [repeat]: repeat behavior (repeat, no-repeat, etc.)
    - [attachment]: scroll behavior (scroll, fixed, local)
    - [clip]: clipping area
    - [origin]: positioning area. *)

val color : color -> declaration
(** [color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color} color} property.
*)

val background : background -> declaration
(** [background bg] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background} background}
    shorthand property. *)

val background_color : color -> declaration
(** [background_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-color}
     background-color} property. *)

val background_image : background_image -> declaration
(** [background_image img] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-image}
     background-image} property. *)

val background_position : position_value list -> declaration
(** [background_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-position}
     background-position} property. *)

val background_size : background_size -> declaration
(** [background_size sz] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-size}
     background-size} property. *)

val background_repeat : background_repeat -> declaration
(** [background_repeat rep] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-repeat}
     background-repeat} property. *)

val background_attachment : background_attachment -> declaration
(** [background_attachment att] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-attachment}
     background-attachment} property. *)

val opacity : opacity -> declaration
(** [opacity op] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/opacity} opacity}
    property. *)

val url : string -> background_image
(** [url path] is a URL background image value. *)

val linear_gradient :
  gradient_direction -> gradient_stop list -> background_image
(** [linear_gradient dir stops] is a linear gradient background. *)

val radial_gradient :
  ?config:radial_gradient_config -> gradient_stop list -> background_image
(** [radial_gradient ?config stops] is a radial gradient background. *)

val conic_gradient :
  ?config:conic_gradient_config -> gradient_stop list -> background_image
(** [conic_gradient ?config stops] is a conic gradient background. *)

val color_stop : color -> gradient_stop
(** [color_stop c] is a simple color stop. *)

val color_position : color -> length -> gradient_stop
(** [color_position c pos] is a color stop at a specific position. *)

(** {2:alignment_properties Alignment Properties}

    CSS Box Alignment properties for flexbox and grid layouts.

    @see <https://www.w3.org/TR/css-align-3/> CSS Box Alignment Module Level 3
*)

val align_content : align_content -> declaration
(** [align_content alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content}
     align-content} property. Sets how content is aligned along the cross axis.
    Common values: Normal, Baseline, Center, Start, End, Space_between, Stretch.
*)

val justify_content : justify_content -> declaration
(** [justify_content alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content}
     justify-content} property. Sets how content is aligned along the main axis.
    Common values: Normal, Center, Start, End, Space_between, Space_around,
    Stretch. *)

val align_items : align_items -> declaration
(** [align_items alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items}
     align-items} property. Sets alignment for all items along the cross axis.
    Common values: Normal, Baseline, Center, Start, End, Stretch. *)

val align_self : align_self -> declaration
(** [align_self alignment] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} align-self}
    property. Overrides align-items for an individual item. Common values: Auto,
    Normal, Baseline, Center, Start, End, Stretch. *)

val justify_items : justify_items -> declaration
(** [justify_items justification] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items}
     justify-items} property. Sets default justification for all items. Common
    values: Normal, Baseline, Center, Start, End, Stretch, Legacy. *)

val justify_self : justify_self -> declaration
(** [justify_self justification] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self}
     justify-self} property. Sets justification for an individual item on the
    inline (main) axis. *)

(** {2:flexbox Flexbox Layout}

    Properties for CSS Flexible Box Layout, a one-dimensional layout method for
    distributing space between items and providing alignment capabilities.

    @see <https://www.w3.org/TR/css-flexbox-1/>
      CSS Flexible Box Layout Module Level 1 *)

val flex_direction : flex_direction -> declaration
(** [flex_direction direction] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction}
     flex-direction} property. *)

val flex_wrap : flex_wrap -> declaration
(** [flex_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap} flex-wrap}
    property. *)

val flex_flow : flex_flow -> declaration
(** [flex_flow flow] is the CSS [flex-flow] property. *)

val flex : flex -> declaration
(** [flex flex] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex} flex} shorthand
    property. *)

val flex_grow : float -> declaration
(** [flex_grow amount] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-grow} flex-grow}
    property. *)

val flex_shrink : float -> declaration
(** [flex_shrink amount] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-shrink}
     flex-shrink} property. *)

val flex_basis : flex_basis -> declaration
(** [flex_basis basis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/flex-basis} flex-basis}
    property. *)

val order : order -> declaration
(** [order order] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/order} order} property.
*)

val gaps : ?column:length -> length -> gap
(** [gaps row] is a one-value {!val-gap} shorthand value. [gaps ~column row] is
    a two-value {!val-gap} shorthand value with separate row and column gaps. *)

val gap : gap -> declaration
(** [gap gap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/gap} gap} property
    shorthand (applies to both row and column gaps). *)

val row_gap : length -> declaration
(** [row_gap gap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/row-gap} row-gap}
    property. *)

val column_gap : length -> declaration
(** [column_gap gap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-gap} column-gap}
    property. *)

(** {2:grid Grid Layout}

    Properties for CSS Grid Layout, a two-dimensional layout system optimized
    for user interface design with explicit row and column positioning.

    @see <https://www.w3.org/TR/css-grid-1/> CSS Grid Layout Module Level 1
    @see <https://www.w3.org/TR/css-grid-2/> CSS Grid Layout Module Level 2 *)

val grid_tracks : grid_template list -> grid_template
(** [grid_tracks tracks] is a track list. *)

val grid_repeat : repeat_count -> grid_template list -> grid_template
(** [grid_repeat count tracks] is a [repeat(...)] track list item. *)

val grid_line_num : int -> grid_line
(** [grid_line_num n] is a numeric grid line. *)

val grid_line_name : string -> grid_line
(** [grid_line_name name] is a named grid line. *)

val grid_line_span : int -> grid_line
(** [grid_line_span n] is [span n]. *)

val grid_line_span_name : string -> grid_line
(** [grid_line_span_name name] is [span name]. *)

val grid_lines : grid_line -> grid_line -> grid_line_pair
(** [grid_lines start end_] is a grid line pair for row/column shorthands. *)

val grid_template_columns : grid_template -> declaration
(** [grid_template_columns cols] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns}
     grid-template-columns} property. *)

val grid_template_rows : grid_template -> declaration
(** [grid_template_rows rows] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-rows}
     grid-template-rows} property. *)

val grid_template_areas : grid_template_areas -> declaration
(** [grid_template_areas areas] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-areas}
     grid-template-areas} property. *)

val grid_template : grid_template -> declaration
(** [grid_template template] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template}
     grid-template} shorthand property. *)

val grid_auto_columns : grid_template -> declaration
(** [grid_auto_columns cols] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-columns}
     grid-auto-columns} property. *)

val grid_auto_rows : grid_template -> declaration
(** [grid_auto_rows rows] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-rows}
     grid-auto-rows} property. *)

val grid_auto_flow : grid_auto_flow -> declaration
(** [grid_auto_flow flow] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-auto-flow}
     grid-auto-flow} property. *)

val grid_row_start : grid_line -> declaration
(** [grid_row_start start] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-start}
     grid-row-start} property. *)

val grid_row_end : grid_line -> declaration
(** [grid_row_end end_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row-end}
     grid-row-end} property. *)

val grid_column_start : grid_line -> declaration
(** [grid_column_start start] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-start}
     grid-column-start} property. *)

val grid_column_end : grid_line -> declaration
(** [grid_column_end end_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column-end}
     grid-column-end} property. *)

val grid_row : grid_line * grid_line -> declaration
(** [grid_row v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-row} grid-row}
    shorthand property. *)

val grid_column : grid_line * grid_line -> declaration
(** [grid_column v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-column}
     grid-column} shorthand property. *)

val grid_area : grid_area -> declaration
(** [grid_area area] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-area} grid-area}
    property. *)

val place_items : place_items -> declaration
(** [place_items items] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-items}
     place-items} shorthand property. *)

val place_content : place_content -> declaration
(** [place_content content] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-content}
     place-content} shorthand property. *)

val place_self : align_self * justify_self -> declaration
(** [place_self self_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-self} place-self}
    shorthand property. *)

(** {2:typography Typography}

    Properties for controlling text appearance, fonts, and text layout. This
    includes font properties, text decoration, alignment, and spacing.

    @see <https://www.w3.org/TR/css-fonts-4/> CSS Fonts Module Level 4
    @see <https://www.w3.org/TR/css-text-3/> CSS Text Module Level 3
    @see <https://www.w3.org/TR/css-text-4/> CSS Text Module Level 4 *)

val text_decoration_shorthand :
  ?lines:text_decoration_line list ->
  ?style:text_decoration_style ->
  ?color:color ->
  ?thickness:length ->
  unit ->
  text_decoration
(** [text_decoration_shorthand ?lines ?style ?color ?thickness ()] is the
    text-decoration shorthand.
    - [lines]: decoration lines (underline, overline, line-through)
    - [style]: line style (solid, double, dotted, dashed, wavy)
    - [color]: decoration color
    - [thickness]: line thickness. *)

val font_stack : font_family list -> font_family
(** [font_stack fonts] is a comma-separated [font-family] stack. *)

val font_family : font_family -> declaration
(** [font_family fonts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property. *)

val font_families : font_family list -> declaration
(** [font_families fonts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-family}
     font-family} property from a comma-separated list. Raises
    [Invalid_argument] when [fonts] is empty. *)

val font_size : length -> declaration
(** [font_size size] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size} font-size}
    property. *)

val font_size_kw : font_size -> declaration
(** [font_size_kw fs] is the font-size property accepting the full
    {!val-font_size} type including absolute/relative size keywords like
    {!constructor-Larger} and {!constructor-Xx_large}. *)

val font_weight : font_weight -> declaration
(** [font_weight weight] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-weight}
     font-weight} property. *)

val font_style : font_style -> declaration
(** [font_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-style} font-style}
    property. *)

val line_height : line_height -> declaration
(** [line_height height] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-height}
     line-height} property. Accepts {!constructor-Normal}, Length values (e.g.,
    `Length (Rem 1.5)`), Number values (e.g., `Num 1.5`), or Percentage values.
*)

val letter_spacing : length -> declaration
(** [letter_spacing spacing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing}
     letter-spacing} property. *)

val word_spacing : length -> declaration
(** [word_spacing spacing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-spacing}
     word-spacing} property. *)

val text_align : text_align -> declaration
(** [text_align align] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-align} text-align}
    property. *)

val text_decoration : text_decoration -> declaration
(** [text_decoration decoration] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration}
     text-decoration} property. *)

val text_transform : text_transform -> declaration
(** [text_transform transform] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform}
     text-transform} property. *)

val text_indent : text_indent_value -> declaration
(** [text_indent indent] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent}
     text-indent} property. *)

val white_space : white_space -> declaration
(** [white_space space] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/white-space}
     white-space} property. *)

val word_break : word_break -> declaration
(** [word_break break] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/word-break} word-break}
    property. *)

val text_decoration_color : color -> declaration
(** [text_decoration_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color}
     text-decoration-color} property. *)

val text_size_adjust : text_size_adjust -> declaration
(** [text_size_adjust adjust] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust}
     text-size-adjust} property. *)

(* CSS text-decoration-style values. *)

val text_decoration_style : text_decoration_style -> declaration
(** [text_decoration_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-style}
     text-decoration-style} property. *)

val text_decoration_line : text_decoration_line -> declaration
(** [text_decoration_line line] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-line}
     text-decoration-line} property. *)

val text_underline_offset : length -> declaration
(** [text_underline_offset offset] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-underline-offset}
     text-underline-offset} property. *)

val text_emphasis : text_emphasis -> declaration
(** [text_emphasis emphasis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis}
     text-emphasis} property. *)

val text_emphasis_style : text_emphasis_style -> declaration
(** [text_emphasis_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-style}
     text-emphasis-style} property. *)

val text_emphasis_color : color -> declaration
(** [text_emphasis_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-color}
     text-emphasis-color} property. *)

val text_emphasis_position : text_emphasis_position -> declaration
(** [text_emphasis_position position] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-emphasis-position}
     text-emphasis-position} property. *)

val text_orientation : text_orientation -> declaration
(** [text_orientation orientation] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-orientation}
     text-orientation} property. *)

val glyph_orientation_vertical : glyph_orientation_vertical -> declaration
(** [glyph_orientation_vertical orientation] is the CSS
    [glyph-orientation-vertical] property. *)

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap}
     overflow-wrap} property. *)

val line_break : line_break -> declaration
(** [line_break break] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-break} line-break}
    property. *)

val hyphens : hyphens -> declaration
(** [hyphens hyphens] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens} hyphens}
    property. *)

val font_stretch : font_stretch -> declaration
(** [font_stretch stretch] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-stretch}
     font-stretch} property. *)

val font_optical_sizing : font_optical_sizing -> declaration
(** [font_optical_sizing sizing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-optical-sizing}
     font-optical-sizing} property. *)

val font_kerning : font_kerning -> declaration
(** [font_kerning kerning] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-kerning}
     font-kerning} property. *)

val font_language_override : font_language_override -> declaration
(** [font_language_override override] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-language-override}
     font-language-override} property. *)

val font_synthesis_style : font_synthesis_style -> declaration
(** [font_synthesis_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-style}
     font-synthesis-style} property. *)

val font_synthesis_weight : font_synthesis_weight -> declaration
(** [font_synthesis_weight weight] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-weight}
     font-synthesis-weight} property. *)

val font_synthesis_small_caps : font_synthesis_small_caps -> declaration
(** [font_synthesis_small_caps small_caps] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-small-caps}
     font-synthesis-small-caps} property. *)

val font_synthesis_position : font_synthesis_position -> declaration
(** [font_synthesis_position position] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-position}
     font-synthesis-position} property. *)

val font_variant_ligatures : font_variant_ligatures -> declaration
(** [font_variant_ligatures ligatures] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-ligatures}
     font-variant-ligatures} property. *)

val font_variant_caps : font_variant_caps -> declaration
(** [font_variant_caps caps] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-caps}
     font-variant-caps} property. *)

val font_variant_position : font_variant_position -> declaration
(** [font_variant_position position] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-position}
     font-variant-position} property. *)

val font_variant_east_asian : font_variant_east_asian -> declaration
(** [font_variant_east_asian east_asian] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-east-asian}
     font-variant-east-asian} property. *)

val font_variant_numeric : font_variant_numeric -> declaration
(** [font_variant_numeric numeric] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-numeric}
     font-variant-numeric} property using a list of tokens or a composed value.
*)

val font_variant_numeric_tokens :
  font_variant_numeric_token list -> font_variant_numeric
(** [font_variant_numeric_tokens tokens] is a font-variant-numeric value from
    tokens. *)

val font_variant_numeric_composed :
  ?ordinal:font_variant_numeric_token ->
  ?slashed_zero:font_variant_numeric_token ->
  ?numeric_figure:font_variant_numeric_token ->
  ?numeric_spacing:font_variant_numeric_token ->
  ?numeric_fraction:font_variant_numeric_token ->
  unit ->
  font_variant_numeric
(** [font_variant_numeric_composed ...] is a composed font-variant-numeric value
    using CSS variables for style composition. *)

val font_feature_settings : font_feature_settings -> declaration
(** [font_feature_settings settings] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-feature-settings}
     font-feature-settings} property. *)

val shadow :
  ?inset:bool ->
  ?inset_var:string ->
  ?inset_var_no_fallback:bool ->
  ?h_offset:length ->
  ?v_offset:length ->
  ?blur:length ->
  ?spread:length ->
  ?color:color ->
  unit ->
  shadow
(** [shadow ?inset ?inset_var ?inset_var_no_fallback ?h_offset ?v_offset ?blur
     ?spread ?color ()] is a shadow value with optional parameters. When
    [inset_var] is set, outputs [var(--<name>,)] (with empty fallback) or
    [var(--<name>)] (no fallback, when [inset_var_no_fallback] is true) before
    the shadow values. Used by Tailwind's ring system. Defaults: inset=false,
    inset_var=None, inset_var_no_fallback=false, h_offset=0px, v_offset=0px,
    blur=0px, spread=0px, color=Transparent. *)

val text_shadow_value :
  ?blur:length -> ?color:color -> length -> length -> text_shadow
(** [text_shadow_value ?blur ?color x y] is a single [text-shadow] value. *)

val text_shadow : text_shadow -> declaration
(** [text_shadow shadow] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-shadow}
     text-shadow} property. *)

val text_shadows : text_shadow list -> declaration
(** [text_shadows shadows] is the text-shadow property with multiple shadows. *)

val font : font -> declaration
(** [font spec] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font} font} shorthand
    property. *)

val direction : direction -> declaration
(** [direction dir] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/direction} direction}
    property. *)

val unicode_bidi : unicode_bidi -> declaration
(** [unicode_bidi bidi] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/unicode-bidi}
     unicode-bidi} property. *)

val writing_mode : writing_mode -> declaration
(** [writing_mode mode] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/writing-mode}
     writing-mode} property. *)

val text_combine_upright : text_combine_upright -> declaration
(** [text_combine_upright value] is the CSS [text-combine-upright] property. *)

val text_decoration_thickness : length -> declaration
(** [text_decoration_thickness thick] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-thickness}
     text-decoration-thickness} property. *)

val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
(** [text_decoration_skip_ink skip] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-skip-ink}
     text-decoration-skip-ink} property. *)

val text_decoration_skip : text_decoration_skip -> declaration
(** [text_decoration_skip v] is the [text-decoration-skip] shorthand. *)

val text_decoration_skip_self : text_decoration_skip_self -> declaration
(** [text_decoration_skip_self v] is the [text-decoration-skip-self] property.
*)

val text_decoration_skip_box : text_decoration_skip_box -> declaration
(** [text_decoration_skip_box v] is the [text-decoration-skip-box] property. *)

val text_decoration_skip_inset : text_decoration_skip_inset -> declaration
(** [text_decoration_skip_inset v] is the [text-decoration-skip-inset] property.
*)

val text_decoration_skip_spaces : text_decoration_skip_spaces -> declaration
(** [text_decoration_skip_spaces v] is the [text-decoration-skip-spaces]
    property. *)

val text_emphasis_skip : text_emphasis_skip -> declaration
(** [text_emphasis_skip v] is the [text-emphasis-skip] property. *)

val white_space_collapse : white_space_collapse -> declaration
(** [white_space_collapse v] is the [white-space-collapse] property. *)

val line_height_step : length -> declaration
(** [line_height_step v] is the [line-height-step] property. *)

val font_palette : font_palette -> declaration
(** [font_palette v] is the [font-palette] property. *)

val font_synthesis : font_synthesis -> declaration
(** [font_synthesis v] is the [font-synthesis] shorthand. *)

val font_size_adjust : font_size_adjust -> declaration
(** [font_size_adjust v] is the [font-size-adjust] property. *)

val font_variant_emoji : font_variant_emoji -> declaration
(** [font_variant_emoji v] is the [font-variant-emoji] property. *)

val font_variant_alternates : font_variant_alternates -> declaration
(** [font_variant_alternates v] is the [font-variant-alternates] property. *)

val font_variant : font_variant -> declaration
(** [font_variant v] is the [font-variant] shorthand. *)

val text_wrap_style : text_wrap_style -> declaration
(** [text_wrap_style v] is the [text-wrap-style] property. *)

val text_box_trim : text_box_trim -> declaration
(** [text_box_trim v] is the [text-box-trim] property. *)

val text_box : text_box -> declaration
(** [text_box v] is the [text-box] shorthand. *)

val text_spacing_trim : text_spacing_trim -> declaration
(** [text_spacing_trim v] is the [text-spacing-trim] property. *)

val hyphenate_limit_chars : hyphenate_limit_chars -> declaration
(** [hyphenate_limit_chars v] is the [hyphenate-limit-chars] property. *)

val initial_letter : initial_letter -> declaration
(** [initial_letter v] is the [initial-letter] property. *)

val initial_letter_align : initial_letter_align -> declaration
(** [initial_letter_align v] is the [initial-letter-align] property. *)

val initial_letter_wrap : initial_letter_wrap -> declaration
(** [initial_letter_wrap v] is the [initial-letter-wrap] property. *)

val shape_image_threshold : shape_image_threshold -> declaration
(** [shape_image_threshold v] is the [shape-image-threshold] property. *)

val shape_margin : length_percentage -> declaration
(** [shape_margin v] is the [shape-margin] property. *)

val shape_outside : string -> declaration
(** [shape_outside v] is the [shape-outside] property, held as the authored text
    of its shape. *)

val overflow_clip_margin : overflow_clip_margin -> declaration
(** [overflow_clip_margin v] is the [overflow-clip-margin] property. *)

val overflow_anchor : overflow_anchor -> declaration
(** [overflow_anchor v] is the [overflow-anchor] property. *)

val overflow_block : overflow -> declaration
(** [overflow_block v] is the [overflow-block] property. *)

val overflow_inline : overflow -> declaration
(** [overflow_inline v] is the [overflow-inline] property. *)

val image_orientation : image_orientation -> declaration
(** [image_orientation v] is the [image-orientation] property. *)

val image_rendering : image_rendering -> declaration
(** [image_rendering v] is the [image-rendering] property. *)

val image_resolution : image_resolution -> declaration
(** [image_resolution v] is the [image-resolution] property. *)

val margin_trim : margin_trim -> declaration
(** [margin_trim v] is the [margin-trim] property. *)

val overlay : overlay -> declaration
(** [overlay v] is the [overlay] property. *)

val animation_composition : animation_composition -> declaration
(** [animation_composition v] is the [animation-composition] property. *)

val background_position_x : background_position_axis -> declaration
(** [background_position_x v] is the [background-position-x] property. *)

val background_position_y : background_position_axis -> declaration
(** [background_position_y v] is the [background-position-y] property. *)

val webkit_mask_position_x : background_position_axis -> declaration
(** [webkit_mask_position_x v] is the [-webkit-mask-position-x] property. *)

val webkit_mask_position_y : background_position_axis -> declaration
(** [webkit_mask_position_y v] is the [-webkit-mask-position-y] property. *)

val moz_orient : Properties.moz_orient -> declaration
(** [moz_orient v] is the [-moz-orient] property. *)

val webkit_text_stroke : webkit_text_stroke -> declaration
(** [webkit_text_stroke v] is the [-webkit-text-stroke] shorthand. *)

val page_size : page_size -> declaration
(** [page_size v] is the [size] descriptor of an [@page] rule. *)

val grid : grid_template -> declaration
(** [grid v] is the [grid] shorthand. *)

(** {2:borders_outlines Borders & Outlines}

    Properties for styling element borders, outlines, and related decorative
    features including border radius for rounded corners.

    @see <https://www.w3.org/TR/css-backgrounds-3/>
      CSS Backgrounds and Borders Module Level 3
    @see <https://www.w3.org/TR/css-ui-4/>
      CSS Basic User Interface Module Level 4 *)

val logical_border_color : color -> logical_border_color
(** [logical_border_color color] is a one-value logical border color. *)

val logical_border_colors : color -> color -> logical_border_color
(** [logical_border_colors start end_] is a two-value logical border color. *)

val logical_border_width : border_width -> logical_border_width
(** [logical_border_width w] is a one-value logical border width. *)

val logical_border_widths : border_width -> border_width -> logical_border_width
(** [logical_border_widths start end_] is a two-value logical border width. *)

val logical_border_style : border_style -> logical_border_style
(** [logical_border_style s] is a one-value logical border style. *)

val logical_border_styles : border_style -> border_style -> logical_border_style
(** [logical_border_styles start end_] is a two-value logical border style. *)

val outline_shorthand :
  ?width:border_width -> ?style:outline_style -> ?color:color -> unit -> outline
(** [outline_shorthand ?width ?style ?color ()] is the outline shorthand. *)

val border_shorthand :
  ?width:border_width -> ?style:border_style -> ?color:color -> unit -> border
(** [border_shorthand ?width ?style ?color ()] is the border shorthand.
    - [width]: border width (thin, medium, thick, or specific length)
    - [style]: border style (solid, dashed, dotted, etc.)
    - [color]: border color. *)

val border :
  ?width:border_width ->
  ?style:border_style ->
  ?color:color ->
  unit ->
  declaration
(** [border border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border} border}
    shorthand property. *)

val column_rule : border list -> declaration
(** [column_rule v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-rule}
     column-rule} shorthand property. *)

(** CSS Gaps 1 gives each gap decoration longhand a comma-separated list, one
    entry per rule line, where {!val-column_rule} writes one.

    @see <https://drafts.csswg.org/css-gaps-1/#propdef-column-rule-style>
      column-rule-style *)

(** CSS Logical 1 gives the flow-relative borders the same shorthand shape the
    physical ones have.

    @see <https://drafts.csswg.org/css-logical-1/#propdef-border-block-start>
      border-block-start *)

val border_block_start : border -> declaration
(** [border_block_start v] is the [border-block-start] shorthand. *)

val border_block_end : border -> declaration
(** [border_block_end v] is the [border-block-end] shorthand. *)

val border_inline : border -> declaration
(** [border_inline v] is the [border-inline] shorthand. *)

val border_inline_start : border -> declaration
(** [border_inline_start v] is the [border-inline-start] shorthand. *)

val border_inline_end : border -> declaration
(** [border_inline_end v] is the [border-inline-end] shorthand. *)

val column_rule_width : border_width list -> declaration
(** [column_rule_width v] is the [column-rule-width] longhand, one entry per gap
    decoration line. *)

val column_rule_style : border_style list -> declaration
(** [column_rule_style v] is the [column-rule-style] longhand, one entry per gap
    decoration line. *)

val column_rule_color : color list -> declaration
(** [column_rule_color v] is the [column-rule-color] longhand, one entry per gap
    decoration line. *)

val border_image_slice : border_image_slice -> declaration
(** [border_image_slice v] is the [border-image-slice] property. *)

val border_image_width : border_image_width -> declaration
(** [border_image_width v] is the [border-image-width] property. *)

val border_image_outset : border_image_outset -> declaration
(** [border_image_outset v] is the [border-image-outset] property. *)

val border_image_repeat : border_image_repeat -> declaration
(** [border_image_repeat v] is the [border-image-repeat] property. *)

val border_image_source : background_image -> declaration
(** [border_image_source v] is the [border-image-source] property. *)

val border_image : border_image -> declaration
(** [border_image v] is the [border-image] shorthand. *)

val mask_border : border_image -> declaration
(** [mask_border v] is the [mask-border] shorthand, which takes what
    [border-image] takes plus the [mode] slot. *)

val border_width : border_width -> declaration
(** [border_width width] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-width}
     border-width} property. *)

val border_style : border_style -> declaration
(** [border_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-style}
     border-style} property. *)

val border_color : color -> declaration
(** [border_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-color}
     border-color} property. *)

val border_block : border -> declaration
(** [border_block v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block}
     border-block} shorthand property. *)

val border_inline_color : logical_border_color -> declaration
(** [border_inline_color v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-color}
     border-inline-color} property. *)

val border_block_color : logical_border_color -> declaration
(** [border_block_color v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-color}
     border-block-color} property. *)

val border_inline_width : logical_border_width -> declaration
(** [border_inline_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-width}
     border-inline-width} property. *)

val border_block_width : logical_border_width -> declaration
(** [border_block_width v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-width}
     border-block-width} property. *)

val border_radius : border_radius -> declaration
(** [border_radius v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-radius}
     border-radius} property; takes a typed value with 1-4 horizontal radii and
    optional 1-4 vertical radii separated by {!val-/}. *)

val border_top_left_radius : length -> declaration
(** [border_top_left_radius radius] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-left-radius}
     border-top-left-radius} property. *)

val border_top_right_radius : length -> declaration
(** [border_top_right_radius radius] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-right-radius}
     border-top-right-radius} property. *)

val border_bottom_left_radius : length -> declaration
(** [border_bottom_left_radius radius] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-left-radius}
     border-bottom-left-radius} property. *)

val border_bottom_right_radius : length -> declaration
(** [border_bottom_right_radius radius] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-right-radius}
     border-bottom-right-radius} property. *)

val border_top : border -> declaration
(** [border_top border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top} border-top}
    property. *)

val border_right : border -> declaration
(** [border_right border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right}
     border-right} property. *)

val border_bottom : border -> declaration
(** [border_bottom border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom}
     border-bottom} property. *)

val border_left : border -> declaration
(** [border_left border] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left}
     border-left} property. *)

val outline : outline -> declaration
(** [outline outline] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline} outline}
    property. *)

val outline_width : border_width -> declaration
(** [outline_width width] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-width}
     outline-width} property. *)

val outline_style : outline_style -> declaration
(** [outline_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-style}
     outline-style} property. *)

val outline_color : color -> declaration
(** [outline_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-color}
     outline-color} property. *)

val outline_offset : length -> declaration
(** [outline_offset offset] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/outline-offset}
     outline-offset} property. *)

val border_top_style : border_style -> declaration
(** [border_top_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-style}
     border-top-style} property. *)

val border_right_style : border_style -> declaration
(** [border_right_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-style}
     border-right-style} property. *)

val border_bottom_style : border_style -> declaration
(** [border_bottom_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-style}
     border-bottom-style} property. *)

val border_left_style : border_style -> declaration
(** [border_left_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-style}
     border-left-style} property. *)

val border_inline_style : logical_border_style -> declaration
(** [border_inline_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-style}
     border-inline-style} property. *)

val border_block_style : logical_border_style -> declaration
(** [border_block_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-style}
     border-block-style} property. *)

val border_inline_start_style : border_style -> declaration
(** [border_inline_start_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-start-style}
     border-inline-start-style} property. *)

val border_inline_end_style : border_style -> declaration
(** [border_inline_end_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-inline-end-style}
     border-inline-end-style} property. *)

val border_block_start_style : border_style -> declaration
(** [border_block_start_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-start-style}
     border-block-start-style} property. *)

val border_block_end_style : border_style -> declaration
(** [border_block_end_style s] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-block-end-style}
     border-block-end-style} property. *)

val border_start_start_radius : length -> declaration
(** [border_start_start_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-start-start-radius}
     border-start-start-radius} property. *)

val border_start_end_radius : length -> declaration
(** [border_start_end_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-start-end-radius}
     border-start-end-radius} property. *)

val border_end_start_radius : length -> declaration
(** [border_end_start_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-end-start-radius}
     border-end-start-radius} property. *)

val border_end_end_radius : length -> declaration
(** [border_end_end_radius len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-end-end-radius}
     border-end-end-radius} property. *)

val border_left_width : border_width -> declaration
(** [border_left_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-width}
     border-left-width} property. *)

val border_top_width : border_width -> declaration
(** [border_top_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-width}
     border-top-width} property. *)

val border_right_width : border_width -> declaration
(** [border_right_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-width}
     border-right-width} property. *)

val border_bottom_width : border_width -> declaration
(** [border_bottom_width len] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-width}
     border-bottom-width} property. *)

val border_top_color : color -> declaration
(** [border_top_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-top-color}
     border-top-color} property. *)

val border_right_color : color -> declaration
(** [border_right_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-right-color}
     border-right-color} property. *)

val border_bottom_color : color -> declaration
(** [border_bottom_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-bottom-color}
     border-bottom-color} property. *)

val border_left_color : color -> declaration
(** [border_left_color c] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-left-color}
     border-left-color} property. *)

val border_collapse : border_collapse -> declaration
(** [border_collapse value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-collapse}
     border-collapse} property. *)

(** {2:transforms_animations Transforms & Animations}

    Properties for 2D/3D transformations, CSS animations, and transitions. Based
    on multiple CSS specification modules for comprehensive animation support.

    @see <https://www.w3.org/TR/css-transforms-1/> CSS Transforms Module Level 1
    @see <https://www.w3.org/TR/css-animations-1/> CSS Animations Level 1
    @see <https://www.w3.org/TR/css-transitions-1/> CSS Transitions Level 1 *)

val transform_list : transform list -> transform
(** [transform_list items] is a multi-function transform value. *)

val transform : transform -> declaration
(** [transform t] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform} transform}
    property with a single transformation. *)

val transforms : transform list -> declaration
(** [transforms ts] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform} transform}
    property with multiple transformations. *)

val origin : length -> length -> transform_origin
(** [origin x y] is a transform-origin helper for 2D positions. *)

val origin3d : length -> length -> length -> transform_origin
(** [origin3d x y z] is a transform-origin helper for 3D positions. *)

val transform_origin : transform_origin -> declaration
(** [transform_origin origin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin}
     transform-origin} property. *)

val transform_box : transform_box -> declaration
(** [transform_box value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box}
     transform-box} property. *)

val rotate : rotate_value -> declaration
(** [rotate v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/rotate} rotate}
    property. *)

val perspective : length -> declaration
(** [perspective perspective] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective}
     perspective} property (3D transforms). *)

val perspective_origin : perspective_origin -> declaration
(** [perspective_origin origin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin}
     perspective-origin} property. *)

val transform_style : transform_style -> declaration
(** [transform_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style}
     transform-style} property (3D transforms). *)

val transition_shorthand :
  ?property:transition_property_value ->
  ?duration:duration ->
  ?timing_function:timing_function ->
  ?delay:duration ->
  ?behavior:transition_behavior ->
  unit ->
  transition
(** [transition_shorthand ?property ?duration ?timing_function ?delay ?behavior
     ()] is the transition shorthand.
    - [property]: CSS property to transition (defaults to All)
    - [duration]: transition duration
    - [timing_function]: easing function (ease, linear, ease-in, etc.)
    - [delay]: delay before transition starts
    - [behavior]: transition-behavior (Transitions Level 2). *)

val transition : transition -> declaration
(** [transition transition] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property. *)

val transitions : transition list -> declaration
(** [transitions values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition} transition}
    property from a comma-separated list. *)

val transition_timing_function : timing_function -> declaration
(** [transition_timing_function tf] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-timing-function}
     transition-timing-function} property. *)

val transition_duration : duration -> declaration
(** [transition_duration dur] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-duration}
     transition-duration} property. *)

val transition_delay : duration -> declaration
(** [transition_delay delay] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-delay}
     transition-delay} property. *)

val transition_property : transition_property -> declaration
(** [transition_property v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-property}
     transition-property} property. *)

val transition_behavior : Properties.transition_behavior -> declaration
(** [transition_behavior v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transition-behavior}
     transition-behavior} property. *)

val animation_shorthand :
  ?name:string ->
  ?duration:duration ->
  ?timing_function:timing_function ->
  ?delay:duration ->
  ?iteration_count:animation_iteration_count ->
  ?direction:animation_direction ->
  ?fill_mode:animation_fill_mode ->
  ?play_state:animation_play_state ->
  ?timeline:animation_timeline ->
  unit ->
  animation
(** [animation_shorthand ?name ?duration ?timing_function ?delay
     ?iteration_count ?direction ?fill_mode ?play_state ?timeline ()] is the
    animation shorthand.
    - [name]: animation name
    - [duration]: animation duration
    - [timing_function]: easing function
    - [delay]: delay before animation starts
    - [iteration_count]: number of iterations (or Infinite)
    - [direction]: animation direction (normal, reverse, alternate, etc.)
    - [fill_mode]: how styles apply before/after animation
    - [play_state]: running or paused
    - [timeline]: animation timeline. *)

val animation : animation -> declaration
(** [animation props] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation} animation}
    shorthand property. *)

val animation_name : animation_name -> declaration
(** [animation_name name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name}
     animation-name} property. *)

val animation_duration : duration -> declaration
(** [animation_duration dur] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-duration}
     animation-duration} property. *)

val animation_timing_function : timing_function -> declaration
(** [animation_timing_function tf] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function}
     animation-timing-function} property. *)

val animation_delay : duration -> declaration
(** [animation_delay delay] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-delay}
     animation-delay} property. *)

val animation_iteration_count : animation_iteration_count -> declaration
(** [animation_iteration_count count] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-iteration-count}
     animation-iteration-count} property. *)

val animation_direction : animation_direction -> declaration
(** [animation_direction dir] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-direction}
     animation-direction} property. *)

val animation_fill_mode : animation_fill_mode -> declaration
(** [animation_fill_mode mode] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-fill-mode}
     animation-fill-mode} property. *)

val animation_play_state : animation_play_state -> declaration
(** [animation_play_state state] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/animation-play-state}
     animation-play-state} property. *)

(** {2:visual_effects Visual Effects}

    Properties for visual effects including shadows, filters, clipping, and
    other advanced rendering features.

    @see <https://www.w3.org/TR/filter-effects-1/> Filter Effects Module Level 1
    @see <https://www.w3.org/TR/css-masking-1/> CSS Masking Module Level 1 *)

val box_shadow : shadow -> declaration
(** [box_shadow shadow] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow} box-shadow}
    property. *)

val box_shadows : shadow list -> declaration
(** [box_shadows values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-shadow} box-shadow}
    property. Raises [Invalid_argument] when [values] is empty. *)

val scale : scale -> declaration
(** [scale scale] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scale} scale} property.
*)

val translate : translate_value -> declaration
(** [translate v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/translate} translate}
    property. *)

val filter_list : filter list -> filter
(** [filter_list items] is a multi-function filter value. *)

val filter : filter -> declaration
(** [filter values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/filter} filter}
    property. *)

val filter_var_empty : string -> filter
(** [filter_var_empty name] creates a filter var reference with empty fallback,
    i.e., [var(--name, )]. Used for composable filter utilities. *)

val background_image_var_none : string -> background_image
(** [background_image_var_none name] creates a background_image var reference
    with no fallback, i.e., [var(--name)]. Used for mask gradient utilities. *)

val minify_color : color -> color
(** [minify_color c] shortens hex colors (e.g., [#0088cc] to [#08c]) and
    converts named colors to shorter hex equivalents when possible. *)

val minify_background_image : background_image -> background_image
(** [minify_background_image img] converts named colors in gradient stops to
    their shortest hex form, matching Lightning CSS behavior. *)

val backdrop_filter : filter -> declaration
(** [backdrop_filter values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/backdrop-filter}
     backdrop-filter} property. *)

val webkit_backdrop_filter : filter -> declaration
(** [webkit_backdrop_filter values] is the [-webkit-backdrop-filter] property.
*)

val clip : clip -> declaration
(** [clip clip] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip} clip} property
    (deprecated). *)

val clip_path : clip_path -> declaration
(** [clip_path path] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/clip-path} clip-path}
    property. *)

val mask : mask -> declaration
(** [mask mask] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mask} mask} property. *)

val webkit_mask_image : background_image -> declaration
(** [webkit_mask_image img] is the [-webkit-mask-image] property. *)

val mask_image : background_image -> declaration
(** [mask_image img] is the [mask-image] property. *)

val webkit_mask_composite : webkit_mask_composite -> declaration
(** [webkit_mask_composite v] is the [-webkit-mask-composite] property. *)

val mask_composite : mask_composite -> declaration
(** [mask_composite v] is the [mask-composite] property. *)

val webkit_mask_source_type : webkit_mask_source_type -> declaration
(** [webkit_mask_source_type v] is the [-webkit-mask-source-type] property. *)

val mask_mode : mask_mode -> declaration
(** [mask_mode v] is the [mask-mode] property. *)

val mask_type : mask_type -> declaration
(** [mask_type v] is the [mask-type] property. *)

val webkit_mask_size : background_size -> declaration
(** [webkit_mask_size v] is the [-webkit-mask-size] property. *)

val mask_size : background_size -> declaration
(** [mask_size v] is the [mask-size] property. *)

val webkit_mask_position : position_value list -> declaration
(** [webkit_mask_position v] is the [-webkit-mask-position] property. *)

val mask_position : position_value list -> declaration
(** [mask_position v] is the [mask-position] property. *)

val webkit_mask_repeat : background_repeat -> declaration
(** [webkit_mask_repeat v] is the [-webkit-mask-repeat] property. *)

val mask_repeat : background_repeat -> declaration
(** [mask_repeat v] is the [mask-repeat] property. *)

val webkit_mask_clip : webkit_mask_box -> declaration
(** [webkit_mask_clip v] is the [-webkit-mask-clip] property. *)

val mask_clip : mask_box -> declaration
(** [mask_clip v] is the [mask-clip] property. *)

val webkit_mask_origin : webkit_mask_box -> declaration
(** [webkit_mask_origin v] is the [-webkit-mask-origin] property. *)

val mask_origin : mask_box -> declaration
(** [mask_origin v] is the [mask-origin] property. *)

val mix_blend_mode : blend_mode -> declaration
(** [mix_blend_mode mode] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/mix-blend-mode}
     mix-blend-mode} property. *)

val background_blend_mode : blend_mode -> declaration
(** [background_blend_mode values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-blend-mode}
     background-blend-mode} property. *)

(** {2:interaction User Interaction}

    Properties that affect user interaction with elements including cursor
    appearance, user selection behavior, and pointer events.

    @see <https://www.w3.org/TR/css-ui-4/>
      CSS Basic User Interface Module Level 4 *)

val cursor_url : ?hotspot:float * float -> fallback:cursor -> string -> cursor
(** [cursor_url ?hotspot ~fallback url] is a URL cursor with its required
    fallback. *)

val cursor : cursor -> declaration
(** [cursor cursor] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/cursor} cursor}
    property. *)

val interactivity : interactivity -> declaration
(** [interactivity interactivity] is the CSS [interactivity] property. *)

val caret_animation : caret_animation -> declaration
(** [caret_animation animation] is the CSS [caret-animation] property. *)

val caret_shape : caret_shape -> declaration
(** [caret_shape shape] is the CSS [caret-shape] property. *)

val caret : caret -> declaration
(** [caret caret] is the CSS [caret] property. *)

val interest_delay : interest_delay -> declaration
(** [interest_delay delay] is the CSS [interest-delay] property. *)

val interest_delay_start : interest_delay -> declaration
(** [interest_delay_start delay] is the CSS [interest-delay-start] property. *)

val interest_delay_end : interest_delay -> declaration
(** [interest_delay_end delay] is the CSS [interest-delay-end] property. *)

val nav_up : nav -> declaration
(** [nav_up nav] is the CSS [nav-up] property. *)

val nav_right : nav -> declaration
(** [nav_right nav] is the CSS [nav-right] property. *)

val nav_down : nav -> declaration
(** [nav_down nav] is the CSS [nav-down] property. *)

val nav_left : nav -> declaration
(** [nav_left nav] is the CSS [nav-left] property. *)

val pointer_events : pointer_events -> declaration
(** [pointer_events events] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events}
     pointer-events} property. *)

val user_select : user_select -> declaration
(** [user_select select] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/user-select}
     user-select} property. *)

val webkit_user_select : user_select -> declaration
(** [webkit_user_select select] is the [-webkit-user-select] property. *)

val resize : resize -> declaration
(** [resize resize] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/resize} resize}
    property. *)

val print_color_adjust : print_color_adjust -> declaration
(** [print_color_adjust v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/print-color-adjust}
     print-color-adjust} property. *)

val webkit_print_color_adjust : print_color_adjust -> declaration
(** [webkit_print_color_adjust v] is the [-webkit-print-color-adjust] property,
    the legacy WebKit-prefixed alias of [print-color-adjust]. *)

val box_decoration_break : box_decoration_break -> declaration
(** [box_decoration_break v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/box-decoration-break}
     box-decoration-break} property. *)

val webkit_box_decoration_break : box_decoration_break -> declaration
(** [webkit_box_decoration_break v] is the [-webkit-box-decoration-break]
    property. *)

val background_origin : background_box -> declaration
(** [background_origin v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-origin}
     background-origin} property. *)

val background_clip : background_box -> declaration
(** [background_clip v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/background-clip}
     background-clip} property. *)

val webkit_background_clip : background_box -> declaration
(** [webkit_background_clip v] is the [-webkit-background-clip] property. *)

(** {2:anchor_positioning Anchor Positioning}

    Properties that tie an absolutely positioned box to an anchor element.

    @see <https://www.w3.org/TR/css-anchor-position-1/>
      CSS Anchor Positioning Level 1 *)

val anchor_name : anchor_name -> declaration
(** [anchor_name v] is the [anchor-name] property. *)

val position_anchor : position_anchor -> declaration
(** [position_anchor v] is the [position-anchor] property. *)

val position_area : position_area -> declaration
(** [position_area v] is the [position-area] property. *)

val position_try_fallbacks : position_try_fallbacks -> declaration
(** [position_try_fallbacks v] is the [position-try-fallbacks] property. *)

val position_try_order : position_try_order -> declaration
(** [position_try_order v] is the [position-try-order] property. *)

val position_try : position_try -> declaration
(** [position_try v] is the [position-try] shorthand. *)

val position_visibility : position_visibility -> declaration
(** [position_visibility v] is the [position-visibility] property. *)

(** {2:view_transitions View Transitions}

    Properties that name the elements a view transition animates independently.

    @see <https://www.w3.org/TR/css-view-transitions-1/>
      CSS View Transitions Module Level 1
    @see <https://www.w3.org/TR/css-view-transitions-2/>
      CSS View Transitions Module Level 2, which adds [match-element] *)

val view_transition_name : view_transition_name -> declaration
(** [view_transition_name v] is the [view-transition-name] property. *)

val view_transition_class : view_transition_class -> declaration
(** [view_transition_class v] is the [view-transition-class] property. *)

(** {2:motion_path Motion Path}

    Properties that move a box along a path rather than by an offset.

    @see <https://www.w3.org/TR/motion-1/> Motion Path Module Level 1 *)

val offset_path : offset_path -> declaration
(** [offset_path v] is the [offset-path] property. *)

val offset_distance : length_percentage -> declaration
(** [offset_distance v] is the [offset-distance] property. *)

val offset_rotate : offset_rotate -> declaration
(** [offset_rotate v] is the [offset-rotate] property. *)

val offset_anchor : offset_anchor -> declaration
(** [offset_anchor v] is the [offset-anchor] property. *)

val offset_position : offset_position -> declaration
(** [offset_position v] is the [offset-position] property. *)

val offset : offset -> declaration
(** [offset v] is the [offset] shorthand. *)

(** {2:container_containment Container Queries & Containment}

    CSS container queries and containment features for component-based
    responsive design and performance optimization through layout isolation.

    @see <https://www.w3.org/TR/css-conditional-5/>
      CSS Conditional Rules Module Level 5, which owns [@container]
    @see <https://www.w3.org/TR/css-contain-3/> CSS Containment Module Level 3
    @see <https://www.w3.org/TR/css-contain-2/> CSS Containment Module Level 2
*)

val container_type : container_type -> declaration
(** [container_type type_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-type}
     container-type} property for container queries.

    The shorthand that sets this and the name together is
    {!val-Declaration.container}, because {!val-container} at this level is the
    at-rule builder. *)

val contain_intrinsic_size : contain_intrinsic_size -> declaration
(** [contain_intrinsic_size v] is the [contain-intrinsic-size] shorthand. *)

val contain_intrinsic_width : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_width v] is the [contain-intrinsic-width] property. *)

val contain_intrinsic_height : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_height v] is the [contain-intrinsic-height] property. *)

val contain_intrinsic_block_size : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_block_size v] is the [contain-intrinsic-block-size]
    property. *)

val contain_intrinsic_inline_size : contain_intrinsic_longhand -> declaration
(** [contain_intrinsic_inline_size v] is the [contain-intrinsic-inline-size]
    property. *)

val container_name : string -> declaration
(** [container_name name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-name}
     container-name} property. *)

val contain_list : contain list -> contain
(** [contain_list items] is a combined {!val-contain} value. *)

val contain : contain -> declaration
(** [contain contain] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/contain} contain}
    property. *)

(** {1 Advanced Features}

    Specialized functionality for advanced CSS features and legacy support. *)

(** {2:vendor_specific Vendor-Specific Properties}

    Vendor-prefixed properties for browser compatibility and legacy support.
    These are implementation-specific extensions that may be needed for older
    browsers.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Extensions>
      MDN: CSS Extensions *)

(** {3:vendor_builders Vendor-prefixed longhands}

    Each writes the prefixed spelling of the unprefixed property beside it and
    takes the same value type. *)

val moz_user_select : user_select -> declaration
(** [moz_user_select v] is the [-moz-user-select] property. *)

val ms_user_select : user_select -> declaration
(** [ms_user_select v] is the [-ms-user-select] property. *)

val webkit_text_fill_color : color -> declaration
(** [webkit_text_fill_color v] is the [-webkit-text-fill-color] property. *)

val webkit_text_stroke_width : border_width -> declaration
(** [webkit_text_stroke_width v] is the [-webkit-text-stroke-width] property. *)

val webkit_text_stroke_color : color -> declaration
(** [webkit_text_stroke_color v] is the [-webkit-text-stroke-color] property. *)

val webkit_transform : transform list -> declaration
(** [webkit_transform v] is the [-webkit-transform] property. *)

val moz_transform : transform list -> declaration
(** [moz_transform v] is the [-moz-transform] property. *)

val ms_transform : transform list -> declaration
(** [ms_transform v] is the [-ms-transform] property. *)

val o_transform : transform list -> declaration
(** [o_transform v] is the [-o-transform] property. *)

val webkit_transition : transition list -> declaration
(** [webkit_transition v] is the [-webkit-transition] property. *)

val webkit_transition_delay : duration -> declaration
(** [webkit_transition_delay v] is the [-webkit-transition-delay] property. *)

val webkit_transition_duration : duration -> declaration
(** [webkit_transition_duration v] is the [-webkit-transition-duration]
    property. *)

val webkit_transition_property : transition_property -> declaration
(** [webkit_transition_property v] is the [-webkit-transition-property]
    property. *)

val webkit_transition_timing_function : timing_function -> declaration
(** [webkit_transition_timing_function v] is the
    [-webkit-transition-timing-function] property. *)

val webkit_animation : animation list -> declaration
(** [webkit_animation v] is the [-webkit-animation] property. *)

val webkit_animation_delay : duration -> declaration
(** [webkit_animation_delay v] is the [-webkit-animation-delay] property. *)

val webkit_animation_duration : duration -> declaration
(** [webkit_animation_duration v] is the [-webkit-animation-duration] property.
*)

val webkit_animation_direction : animation_direction -> declaration
(** [webkit_animation_direction v] is the [-webkit-animation-direction]
    property. *)

val webkit_animation_iteration_count : animation_iteration_count -> declaration
(** [webkit_animation_iteration_count v] is the
    [-webkit-animation-iteration-count] property. *)

val webkit_animation_name : animation_name -> declaration
(** [webkit_animation_name v] is the [-webkit-animation-name] property. *)

val webkit_animation_timing_function : timing_function -> declaration
(** [webkit_animation_timing_function v] is the
    [-webkit-animation-timing-function] property. *)

val webkit_animation_fill_mode : animation_fill_mode -> declaration
(** [webkit_animation_fill_mode v] is the [-webkit-animation-fill-mode]
    property. *)

val webkit_animation_play_state : animation_play_state -> declaration
(** [webkit_animation_play_state v] is the [-webkit-animation-play-state]
    property. *)

val webkit_flex_direction : flex_direction -> declaration
(** [webkit_flex_direction v] is the [-webkit-flex-direction] property. *)

val webkit_flex_wrap : flex_wrap -> declaration
(** [webkit_flex_wrap v] is the [-webkit-flex-wrap] property. *)

val webkit_flex_flow : flex_flow -> declaration
(** [webkit_flex_flow v] is the [-webkit-flex-flow] property. *)

val webkit_justify_content : justify_content -> declaration
(** [webkit_justify_content v] is the [-webkit-justify-content] property. *)

val webkit_align_items : align_items -> declaration
(** [webkit_align_items v] is the [-webkit-align-items] property. *)

val webkit_align_content : align_content -> declaration
(** [webkit_align_content v] is the [-webkit-align-content] property. *)

val webkit_align_self : align_self -> declaration
(** [webkit_align_self v] is the [-webkit-align-self] property. *)

val webkit_border_radius : border_radius -> declaration
(** [webkit_border_radius v] is the [-webkit-border-radius] property. *)

val webkit_box_sizing : box_sizing -> declaration
(** [webkit_box_sizing v] is the [-webkit-box-sizing] property. *)

val moz_box_sizing : box_sizing -> declaration
(** [moz_box_sizing v] is the [-moz-box-sizing] property. *)

val webkit_box_shadow : shadow -> declaration
(** [webkit_box_shadow v] is the [-webkit-box-shadow] property. *)

val webkit_background_size : background_size -> declaration
(** [webkit_background_size v] is the [-webkit-background-size] property. *)

val webkit_filter : filter -> declaration
(** [webkit_filter v] is the [-webkit-filter] property. *)

val moz_animation : animation list -> declaration
(** [moz_animation v] is the [-moz-animation] property. *)

val moz_animation_delay : duration -> declaration
(** [moz_animation_delay v] is the [-moz-animation-delay] property. *)

val moz_animation_duration : duration -> declaration
(** [moz_animation_duration v] is the [-moz-animation-duration] property. *)

val moz_animation_direction : animation_direction -> declaration
(** [moz_animation_direction v] is the [-moz-animation-direction] property. *)

val moz_animation_iteration_count : animation_iteration_count -> declaration
(** [moz_animation_iteration_count v] is the [-moz-animation-iteration-count]
    property. *)

val moz_animation_name : animation_name -> declaration
(** [moz_animation_name v] is the [-moz-animation-name] property. *)

val moz_animation_timing_function : timing_function -> declaration
(** [moz_animation_timing_function v] is the [-moz-animation-timing-function]
    property. *)

val moz_animation_fill_mode : animation_fill_mode -> declaration
(** [moz_animation_fill_mode v] is the [-moz-animation-fill-mode] property. *)

val moz_animation_play_state : animation_play_state -> declaration
(** [moz_animation_play_state v] is the [-moz-animation-play-state] property. *)

val moz_transition : transition list -> declaration
(** [moz_transition v] is the [-moz-transition] property. *)

val moz_transition_delay : duration -> declaration
(** [moz_transition_delay v] is the [-moz-transition-delay] property. *)

val moz_transition_duration : duration -> declaration
(** [moz_transition_duration v] is the [-moz-transition-duration] property. *)

val moz_transition_property : transition_property -> declaration
(** [moz_transition_property v] is the [-moz-transition-property] property. *)

val moz_transition_timing_function : timing_function -> declaration
(** [moz_transition_timing_function v] is the [-moz-transition-timing-function]
    property. *)

val moz_border_radius : border_radius -> declaration
(** [moz_border_radius v] is the [-moz-border-radius] property. *)

val moz_box_shadow : shadow -> declaration
(** [moz_box_shadow v] is the [-moz-box-shadow] property. *)

val ms_filter : filter -> declaration
(** [ms_filter v] is the [-ms-filter] property. *)

val o_transition : transition list -> declaration
(** [o_transition v] is the [-o-transition] property. *)

val webkit_appearance : webkit_appearance -> declaration
(** [webkit_appearance app] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-appearance}
     -webkit-appearance} property. *)

val webkit_font_smoothing : webkit_font_smoothing -> declaration
(** [webkit_font_smoothing smoothing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-font-smoothing}
     -webkit-font-smoothing} property. *)

val moz_osx_font_smoothing : moz_osx_font_smoothing -> declaration
(** [moz_osx_font_smoothing smoothing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-moz-osx-font-smoothing}
     -moz-osx-font-smoothing} property. *)

val webkit_tap_highlight_color : color -> declaration
(** [webkit_tap_highlight_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-tap-highlight-color}
     -webkit-tap-highlight-color} property. *)

val webkit_text_decoration : text_decoration -> declaration
(** [webkit_text_decoration decoration] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration}
     -webkit-text-decoration} property. *)

val webkit_text_decoration_color : color -> declaration
(** [webkit_text_decoration_color color] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-color}
     -webkit-text-decoration-color} property. *)

val webkit_line_clamp : webkit_line_clamp -> declaration
(** [webkit_line_clamp clamp] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-line-clamp}
     -webkit-line-clamp} property. *)

val webkit_box_orient : webkit_box_orient -> declaration
(** [webkit_box_orient orient] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-box-orient}
     -webkit-box-orient} property. *)

val webkit_hyphens : hyphens -> declaration
(** [webkit_hyphens hyphens] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens}
     -webkit-hyphens} property. *)

val webkit_text_size_adjust : text_size_adjust -> declaration
(** [webkit_text_size_adjust adjust] is the WebKit-only
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-size-adjust}
     -webkit-text-size-adjust} property. *)

(** {1 Additional Properties}

    Specialized CSS properties organized by their functional purpose. *)

(** {2:lists_tables Lists & Tables}

    Properties for styling HTML lists and tables.

    @see <https://www.w3.org/TR/css-lists-3/>
      CSS Lists and Counters Module Level 3
    @see <https://www.w3.org/TR/css-tables-3/> CSS Table Module Level 3 *)

val list_style_symbol_string : string -> list_style_symbol
(** [list_style_symbol_string value] is a string symbol for [symbols()]. *)

val list_style_symbol_url : string -> list_style_symbol
(** [list_style_symbol_url value] is a URL symbol for [symbols()]. *)

val list_style_string : string -> list_style_type
(** [list_style_string value] is a string [list-style-type]. *)

val list_style_symbols :
  ?kind:symbols_type -> list_style_symbol list -> list_style_type
(** [list_style_symbols ?kind symbols] is a [symbols(...)] list-style type. *)

val list_style_image_url : string -> list_style_image
(** [list_style_image_url value] is a URL [list-style-image]. *)

val list_style_type : list_style_type -> declaration
(** [list_style_type lst] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-type}
     list-style-type} property. *)

val list_style_image : list_style_image -> declaration
(** [list_style_image img] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style-image}
     list-style-image} property. *)

(* Table layout and vertical-align types *)

val table_layout : table_layout -> declaration
(** [table_layout value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/table-layout}
     table-layout} property. *)

val vertical_align : vertical_align -> declaration
(** [vertical_align value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/vertical-align}
     vertical-align} property. *)

val list_style : list_style -> declaration
(** [list_style value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/list-style} list-style}
    shorthand property. *)

val border_spacing : border_spacing -> declaration
(** [border_spacing values] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/border-spacing}
     border-spacing} property. Accepts 1 or 2 length values. *)

val border_spacing_values : length list -> border_spacing
(** [border_spacing_values values] is a one- or two-value [border-spacing]. *)

(** {2:svg_properties SVG Properties}

    Properties specific to SVG rendering and styling.

    @see <https://www.w3.org/TR/SVG2/> Scalable Vector Graphics (SVG) 2 *)

val svg_paint_color : color -> svg_paint
(** [svg_paint_color color] is a color paint value. *)

val svg_paint_url : ?fallback:svg_paint -> string -> svg_paint
(** [svg_paint_url ?fallback url] is a URL paint value with an optional
    fallback. *)

val fill : svg_paint -> declaration
(** [fill paint] is the SVG fill property. *)

val stroke : svg_paint -> declaration
(** [stroke paint] is the SVG stroke property. *)

val stroke_width : stroke_width -> declaration
(** [stroke_width width] is the SVG stroke-width property. *)

val fill_rule : fill_rule -> declaration
(** [fill_rule v] is the SVG [fill-rule] property. *)

val clip_rule : fill_rule -> declaration
(** [clip_rule v] is the SVG [clip-rule] property, which takes what [fill-rule]
    takes. *)

val fill_opacity : opacity -> declaration
(** [fill_opacity v] is the SVG [fill-opacity] property. *)

val stroke_opacity : opacity -> declaration
(** [stroke_opacity v] is the SVG [stroke-opacity] property. *)

val stroke_linecap : stroke_linecap -> declaration
(** [stroke_linecap v] is the SVG [stroke-linecap] property. *)

val stroke_linejoin : stroke_linejoin -> declaration
(** [stroke_linejoin v] is the SVG [stroke-linejoin] property. *)

val stroke_miterlimit : stroke_miterlimit -> declaration
(** [stroke_miterlimit v] is the SVG [stroke-miterlimit] property. *)

val stroke_dashoffset : stroke_dashoffset -> declaration
(** [stroke_dashoffset v] is the SVG [stroke-dashoffset] property. *)

val stroke_dasharray : stroke_dasharray -> declaration
(** [stroke_dasharray v] is the SVG [stroke-dasharray] property. *)

val paint_order : paint_order -> declaration
(** [paint_order v] is the SVG [paint-order] property. *)

val vector_effect : vector_effect -> declaration
(** [vector_effect v] is the SVG [vector-effect] property. *)

val stop_color : color -> declaration
(** [stop_color v] is the SVG [stop-color] property of a gradient stop. *)

val stop_opacity : opacity -> declaration
(** [stop_opacity v] is the SVG [stop-opacity] property of a gradient stop. *)

val flood_color : color -> declaration
(** [flood_color v] is the SVG [flood-color] property of [feFlood]. *)

val flood_opacity : opacity -> declaration
(** [flood_opacity v] is the SVG [flood-opacity] property of [feFlood]. *)

val lighting_color : color -> declaration
(** [lighting_color v] is the SVG [lighting-color] property of a light filter.
*)

val dominant_baseline : dominant_baseline -> declaration
(** [dominant_baseline v] is the [dominant-baseline] property. *)

val alignment_baseline : alignment_baseline -> declaration
(** [alignment_baseline v] is the [alignment-baseline] property. *)

val baseline_shift : baseline_shift -> declaration
(** [baseline_shift v] is the [baseline-shift] property. *)

val baseline_source : baseline_source -> declaration
(** [baseline_source v] is the [baseline-source] property. *)

(** {2:scroll_touch Scroll & Touch}

    Properties for scroll behavior and touch interaction.

    @see <https://www.w3.org/TR/css-scroll-snap-1/>
      CSS Scroll Snap Module Level 1
    @see <https://www.w3.org/TR/css-overscroll-1/>
      CSS Overscroll Behavior Module Level 1
    @see <https://www.w3.org/TR/pointerevents3/>
      Pointer Events Level 3, which owns [touch-action] *)

val animation_timeline : animation_timeline -> declaration
(** [animation_timeline v] is the [animation-timeline] property. *)

val animation_range : animation_range -> declaration
(** [animation_range v] is the [animation-range] shorthand. *)

val animation_range_start : animation_range_item -> declaration
(** [animation_range_start v] is the [animation-range-start] property. *)

val animation_range_end : animation_range_item -> declaration
(** [animation_range_end v] is the [animation-range-end] property. *)

val scroll_timeline : timeline_shorthand -> declaration
(** [scroll_timeline v] is the [scroll-timeline] shorthand. *)

val scroll_timeline_name : timeline_name -> declaration
(** [scroll_timeline_name v] is the [scroll-timeline-name] property. *)

val scroll_timeline_axis : timeline_axis -> declaration
(** [scroll_timeline_axis v] is the [scroll-timeline-axis] property. *)

val view_timeline : view_timeline_shorthand -> declaration
(** [view_timeline v] is the [view-timeline] shorthand. *)

val view_timeline_name : timeline_name -> declaration
(** [view_timeline_name v] is the [view-timeline-name] property. *)

val view_timeline_axis : timeline_axis -> declaration
(** [view_timeline_axis v] is the [view-timeline-axis] property. *)

val view_timeline_inset : timeline_inset -> declaration
(** [view_timeline_inset v] is the [view-timeline-inset] property. *)

val timeline_scope : timeline_scope -> declaration
(** [timeline_scope v] is the [timeline-scope] property. *)

val touch_action : touch_action -> declaration
(** [touch_action action] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/touch-action}
     touch-action} property. *)

val scroll_snap_type : scroll_snap_type -> declaration
(** [scroll_snap_type type_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-type}
     scroll-snap-type} property. *)

val scroll_snap_align : scroll_snap_align -> declaration
(** [scroll_snap_align align] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-align}
     scroll-snap-align} property. *)

val scroll_snap_stop : scroll_snap_stop -> declaration
(** [scroll_snap_stop stop] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-stop}
     scroll-snap-stop} property. *)

val scroll_behavior : scroll_behavior -> declaration
(** [scroll_behavior behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-behavior}
     scroll-behavior} property for smooth scrolling. *)

val color_scheme : color_scheme -> declaration
(** [color_scheme scheme] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/color-scheme}
     color-scheme} property for light/dark mode preference. *)

val scroll_margin : length list -> declaration
(** [scroll_margin margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin}
     scroll-margin} property. *)

val scroll_margin_top : length -> declaration
(** [scroll_margin_top margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-top}
     scroll-margin-top} property. *)

val scroll_margin_right : length -> declaration
(** [scroll_margin_right margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-right}
     scroll-margin-right} property. *)

val scroll_margin_bottom : length -> declaration
(** [scroll_margin_bottom margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-bottom}
     scroll-margin-bottom} property. *)

val scroll_margin_left : length -> declaration
(** [scroll_margin_left margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-left}
     scroll-margin-left} property. *)

val scroll_margin_inline : length list -> declaration
(** [scroll_margin_inline margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-inline}
     scroll-margin-inline} property. *)

val scroll_margin_inline_start : length -> declaration
(** [scroll_margin_inline_start margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-inline-start}
     scroll-margin-inline-start} property. *)

val scroll_margin_inline_end : length -> declaration
(** [scroll_margin_inline_end margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-inline-end}
     scroll-margin-inline-end} property. *)

val scroll_margin_block : length list -> declaration
(** [scroll_margin_block margins] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-block}
     scroll-margin-block} property; takes 1 (both edges) or 2 (start, end)
    length values per the spec. *)

val scroll_margin_block_start : length -> declaration
(** [scroll_margin_block_start margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-block-start}
     scroll-margin-block-start} property. *)

val scroll_margin_block_end : length -> declaration
(** [scroll_margin_block_end margin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-margin-block-end}
     scroll-margin-block-end} property. *)

val scroll_padding : length list -> declaration
(** [scroll_padding padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding}
     scroll-padding} property. *)

val scroll_padding_top : length -> declaration
(** [scroll_padding_top padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-top}
     scroll-padding-top} property. *)

val scroll_padding_right : length -> declaration
(** [scroll_padding_right padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-right}
     scroll-padding-right} property. *)

val scroll_padding_bottom : length -> declaration
(** [scroll_padding_bottom padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-bottom}
     scroll-padding-bottom} property. *)

val scroll_padding_left : length -> declaration
(** [scroll_padding_left padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-left}
     scroll-padding-left} property. *)

val scroll_padding_inline : length list -> declaration
(** [scroll_padding_inline padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-inline}
     scroll-padding-inline} property. *)

val scroll_padding_inline_start : length -> declaration
(** [scroll_padding_inline_start padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-inline-start}
     scroll-padding-inline-start} property. *)

val scroll_padding_inline_end : length -> declaration
(** [scroll_padding_inline_end padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-inline-end}
     scroll-padding-inline-end} property. *)

val scroll_padding_block : length list -> declaration
(** [scroll_padding_block padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-block}
     scroll-padding-block} property. *)

val scroll_padding_block_start : length -> declaration
(** [scroll_padding_block_start padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-block-start}
     scroll-padding-block-start} property. *)

val scroll_padding_block_end : length -> declaration
(** [scroll_padding_block_end padding] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-padding-block-end}
     scroll-padding-block-end} property. *)

val overscroll_behavior : overscroll_behavior list -> declaration
(** [overscroll_behavior behaviors] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior}
     overscroll-behavior} property. *)

val overscroll_behavior_x : overscroll_behavior -> declaration
(** [overscroll_behavior_x behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior-x}
     overscroll-behavior-x} property. *)

val overscroll_behavior_block : overscroll_behavior -> declaration
(** [overscroll_behavior_block v] is the [overscroll-behavior-block] property.
*)

val overscroll_behavior_inline : overscroll_behavior -> declaration
(** [overscroll_behavior_inline v] is the [overscroll-behavior-inline] property.
*)

val overscroll_behavior_y : overscroll_behavior -> declaration
(** [overscroll_behavior_y behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overscroll-behavior-y}
     overscroll-behavior-y} property. *)

val accent_color : color -> declaration
(** [accent_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/accent-color}
     accent-color} property for form controls. *)

val caret_color : color -> declaration
(** [caret_color color] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color}
     caret-color} property for the text input cursor. *)

(** {2:misc_properties Miscellaneous}

    Other properties that don't fit into specific categories. *)

val forced_color_adjust : forced_color_adjust -> declaration
(** [forced_color_adjust adjust] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/forced-color-adjust}
     forced-color-adjust} property. *)

val appearance : appearance -> declaration
(** [appearance app] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/appearance} appearance}
    property. *)

val moz_appearance : appearance -> declaration
(** [moz_appearance v] is the [-moz-appearance] property. *)

val tab_size : int -> declaration
(** [tab_size size] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size} tab-size}
    property. *)

val tab_size_value : tab_size -> declaration
(** [tab_size_value v] is the [tab-size] property from a typed value, allowing a
    [<length>] in addition to an integer. *)

val scrollbar_width : scrollbar_width -> declaration
(** [scrollbar_width v] is the [scrollbar-width] property. *)

val scrollbar_color : scrollbar_color -> declaration
(** [scrollbar_color v] is the [scrollbar-color] property. *)

val scrollbar_gutter : scrollbar_gutter -> declaration
(** [scrollbar_gutter v] is the [scrollbar-gutter] property. *)

val zoom : zoom -> declaration
(** [zoom v] is the CSS [zoom] property. *)

val font_variation_settings : font_variation_settings -> declaration
(** [font_variation_settings settings] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings}
     font-variation-settings} property. *)

(** {1:custom_properties Custom Properties}

    Type-safe CSS custom properties (CSS variables) with GADT-based type
    checking. *)

val var_meta : 'a var -> meta option
(** [var_meta v] is the optional metadata associated with [v]. *)

val meta : unit -> ('a -> meta) * (meta -> 'a option)
(** [meta ()] returns a fresh injection/projection pair for storing values of
    type ['a] inside {!type-meta}. *)

val var_ref :
  ?fallback:'a fallback ->
  ?default:'a ->
  ?layer:string ->
  ?meta:meta ->
  ?runtime:bool ->
  string ->
  'a var
(** [var_ref ?fallback ?default ?layer ?meta ?runtime name] is a CSS variable
    reference. This is primarily for the CSS parser to create var() references.

    - [name] is the variable name (without the -- prefix)
    - [fallback] is used inside var(--name, fallback) in CSS output
    - [default] is the resolved value when mode is Inline
    - [layer] is an optional CSS layer name
    - [meta] is optional metadata. *)

(** {2 CSS [@property] Support} *)

val property :
  name:string -> 'a syntax -> ?initial_value:'a -> ?inherits:bool -> unit -> t
(** [property ~name syntax ?initial_value ?inherits ()] creates a [@property]
    rule for registering a custom CSS property with type-safe syntax and initial
    value.

    Examples:
    - [property ~name:"--my-color" Variables.Color ~initial_value:(hex
       "#ff0000") ()]
    - [property ~name:"--my-size" Variables.Length ~initial_value:(Px 10.) ()]

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/@property} MDN
     \@property}. *)

val var :
  ?default:'a ->
  ?fallback:'a fallback ->
  ?layer:string ->
  ?meta:meta ->
  ?runtime:bool ->
  string ->
  'a kind ->
  'a ->
  declaration * 'a var
(** [var ?default ?fallback ?layer ?runtime name kind value] returns a
    declaration and a variable handle. With [~runtime:true] a context keeps the
    [var()] reference instead of folding it to [value], so a runtime stylesheet
    or script can still override [--name].

    - [name] is the variable name without the [--] prefix
    - [kind] specifies the value type (Length, Color, Angle, Float, etc.)
    - [default] specifies the value to use in inline mode instead of var()
      reference
    - [fallback] is used inside [var(--name, fallback)] in CSS output
    - [layer] is an optional CSS layer name where the variable should be placed

    Example:
    {[
    open Cascade.Css

    let def_radius, radius_var = var "radius-md" Length (Rem 0.5)

    let card =
      rule ~selector:(Selector.class_ "card")
        [ def_radius; border_top_left_radius (Var radius_var) ]
    ]}

    The returned [radius_var] must be wrapped with {!constructor-Var} when used
    in CSS properties. In variables mode, it emits "--radius-md: 0.5rem" and
    uses "var(--radius-md)". In inline mode, it uses "0.5rem" directly when the
    default equals the defined value. *)

val meta_of_declaration : declaration -> meta option
(** [meta_of_declaration decl] extracts metadata from a declaration if it has
    any. *)

val custom_property : ?layer:string -> string -> string -> declaration
(** [custom_property ?layer name value] is a CSS custom property declaration.

    For type-safe variable declarations and usage, prefer using the {!val:var}
    API which provides compile-time checking and automatic variable management.

    @param layer Optional CSS layer name for the custom property
    @param name CSS custom property name: a [<dashed-ident>]
    @param value
      the [<declaration-value>?] CSS Variables 1 sec. 2 gives a custom property,
      as CSS text

    It raises [Invalid_argument] on a pair that does not make the one
    declaration it names, such as a value carrying a top-level [;] or [}] or an
    unterminated function, block or string. A name holding a code point no bare
    ident carries is written back with the escapes that read it.
    {!Declaration.parse_custom_property} is the same check as an option.

    Example: [custom_property "--primary-color" "#3b82f6"]

    See also {!val:var} (type-safe CSS variable API). *)

val parse_declaration : ?layer:string -> string -> string -> declaration option
(** [parse_declaration ?layer property value] reads [property] and [value] with
    the full declaration parser. The two are read as the tokens they are rather
    than as one ["property: value"] text, so a [property] carrying a [;], a [}]
    or a [:] names this declaration or names none:
    - a known property (e.g. [mask-type], {!val-display}) becomes a typed
      declaration;
    - a custom property ([--x]) or an unknown property keeps its parsed
      component stream, so [var()] references in [value] are visible to
      {!vars_of_declarations} (unlike {!custom_property}, which forces an opaque
      token value);
    - {!constructor-None} if [value] does not parse.

    [layer] applies only to a custom property. *)

val custom_declaration_name : declaration -> string option
(** [custom_declaration_name decl] is the variable name if [decl] is a custom
    property declaration, {!constructor-None} otherwise. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer decl] is the declared layer for a custom property
    declaration if present (e.g., "theme" or "utilities"). It is
    {!constructor-None} for non-custom declarations or when no layer metadata is
    attached. *)

(** {1 Printing & Optimization}

    CSS output generation and performance optimization tools. *)

(** {2:rendering Printing}

    Functions for converting CSS structures to string output. *)

val to_string :
  ?minify:bool ->
  ?indent:int ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  ?rename_custom_property:(string -> string) ->
  t ->
  string
(** [to_string ?minify ?indent ?lossless ?enforce_spec ?rename_custom_property
     stylesheet] serialises a stylesheet to CSS. [rename_custom_property]
    rewrites every custom property name, without its leading [--], wherever one
    is written: the name a declaration declares and the name a [var()] reads.
    That is how a sheet is namespaced without rebuilding the typed values its
    references live in. Pure formatter - no optimisation, no theme resolution,
    no [var()] substitution. Run {!optimize}, {!resolve_theme}, and
    {!inline_vars} explicitly when those passes are needed. Spec recovery (drop
    invalid declarations and empty rules) still applies because the parser
    preserved those shapes for round-trip and browsers discard them during
    parse. Unknown at-rules are preserved. Output never ends with a newline.

    - [minify] toggles compact serialisation (no insignificant whitespace).
    - [indent] sets the per-level indent width.
    - [lossless] suppresses colour-channel rounding in minified output.
    - [enforce_spec] suppresses target-dependent minified shortenings and keeps
      their spec-canonical serialisations.

    @see <https://developer.mozilla.org/en-US/docs/Web/CSS> "MDN: CSS". *)

val pp : t Pp.t
(** [pp] is the composable form of {!to_string}. It applies the same invalid
    declaration and empty-rule filtering before printing. *)

val to_buffer :
  Buffer.t ->
  ?minify:bool ->
  ?indent:int ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  ?rename_custom_property:(string -> string) ->
  t ->
  unit
(** [to_buffer buf stylesheet] appends the serialised stylesheet to [buf]. Same
    options as {!to_string}. *)

type parse = {
  stylesheet : t;
  warnings : Error.t list;
  source : Source.t option;
}
(** A partially-recovered parse: the {!field-stylesheet} composed of every rule
    that validated successfully, plus the {!field-warnings} accumulated for
    rules that were dropped or section 5.3-recovered. Each warning is an
    [Error.t] stamped with the source [filename] when one was supplied.
    {!field-source} is [Some] only when parsing requested source fidelity. *)

val of_string :
  ?strict:bool ->
  ?filename:string ->
  ?meta:Loc.meta_level ->
  ?enforce_spec:bool ->
  ?preserve_source:bool ->
  string ->
  (parse, Error.t) result
(** [of_string ?strict css] parses [css] with CSS Syntax 3 (ED) section 5.4
    recovery. Returns [Ok { stylesheet; warnings }] when no fatal syntax error
    escapes recovery; {!field-warnings} carries every typed diagnostic the
    parser collected (unknown at-rules, unknown properties, invalid values,
    ...). With [~strict:true] a non-empty {!field-warnings} list collapses to
    [Error] (first warning) - useful in linters and CI gates that want to fail
    on any spec deviation. [?meta] controls diagnostic richness; see
    {!Loc.meta_level}.

    [enforce_spec] (default [false]) restricts non-ASCII identifiers to the CSS
    Syntax 3 (ED) sec. 4.2 range list, which excludes most BMP symbols. The
    default accepts any code point [>= U+0080], so a selector such as
    [.text-\u{2197}] reads rather than being dropped with a warning. Output is
    unaffected either way: a code point outside the range list is hex-escaped.

    [preserve_source] (default [false]) retains the byte-exact input, located
    recovered syntax tree, comments, trivia ownership, and source-coordinate
    mapping in {!field-source}. The snapshot describes only the authored parse:
    optimising, flattening, mapping, or otherwise transforming
    {!field-stylesheet} neither mutates it nor fabricates locations for split,
    merged, dropped, or synthetic nodes. {!val-stylesheet} is the raising form
    for CSS written in a program. *)

val stylesheet :
  ?strict:bool ->
  ?filename:string ->
  ?meta:Loc.meta_level ->
  ?enforce_spec:bool ->
  string ->
  t
(** [stylesheet css] is the stylesheet {!of_string} reads from [css], for CSS
    written in a program. It discards the {!field-warnings}; with [~strict:true]
    any warning is an error.

    @raise Invalid_argument
      if [css] does not read. Use {!of_string} to deal with untrusted input. *)

(** {2:optimization Optimization}

    Tools for optimizing CSS output for performance and file size. *)

val canonicalize_rule_order :
  ?lossless:bool -> ?enforce_spec:bool -> ?judge:Optimize.targets -> t -> t
(** [canonicalize_rule_order t] projects cascade-equivalent stylesheets to one
    deterministic form: selector-list rules expand onto their branches,
    same-selector rules coalesce when no intervening write can observe the move
    (so a declaration hoisted into a shared group and the same declaration
    written inline converge), each rule's declarations sort into a canonical
    order among those with disjoint footprints (a shorthand and its longhand, or
    two writes of the same property, keep their cascade-significant order), and
    cascade-independent statements sort into a content-keyed linear extension of
    the cascade-conflict graph. Sharing a selector branch alone is not a
    conflict in this projection: after branch expansion, only overlapping
    cascade-property writes constrain their order. A [@media] / [@supports] /
    [@container] block whose transitive content is plain rules reads as one
    block per rule, so each rule moves on its own footprint and how the rules
    were grouped into blocks is no difference; adjacent blocks of one condition
    fold back together, and conflicting statements keep their relative order.
    Two equal [@supports] blocks may merge across an intervening non-important
    write that the later block shadows with the same selector and property
    whenever the condition holds. Two rules under a condition and its exact
    negation
    - an [@media] query, an [@supports] condition, or an [@container] query on
      one container name - never apply together, so their order is no
      difference. CSS Cascade 5 sec. 6.1 sorts by layer before order of
      appearance, so a named [@layer] block of style rules reads in the sheet's
      layer order, however a pin and the blocks spell that order, and its place
      among unlayered rules, [@property] and [@keyframes] is no difference; two
      blocks of one layer keep their order, and an [@layer] statement pins the
      layer order where it stands. A run of [@property] rules sorts by name,
      keeping the last registration of each, since CSS Properties and Values API
      1 sec. 2 makes registrations for different names order-independent. A
      [@media] prelude is keyed as the Level 4 query Media Queries 4 makes it
      equal to - [not all and (X)] as [not (X)], [min-X]/[max-X] as the range
      form, and a lower bound met by an upper bound as the two-sided interval -
      and an [@container] prelude the same way, which emission cannot do because
      a Level 3 parser rejects the shorter forms. A [color(srgb ...)] whose
      channels all land on a whole byte is keyed as the [rgb()] spelling of the
      same colour, which emission cannot do either because [color()] needs a
      browser that parses it. A [none] channel of a Lab-family colour standing
      as a whole colour-longhand value, or as a colour function of a
      custom-property token stream, is keyed as the zero CSS Color 4 sec. 4.4
      says a missing component behaves as, so a converted achromatic [oklab()]
      meets the hex a minifier writes for it; sec. 13.3 keeps that off the
      positions the sheet interpolates, so a gradient stop, a [color-mix()]
      operand, [@keyframes], [@starting-style] and a colour whose own rule
      transitions the property it writes, a custom property named by its own
      name included, keep their [none], and [lossless] bounds how far the
      resolved colour respells. An angle is keyed as the degrees it names, which
      CSS Values 4 sec. 6.1 makes one dimension under [deg], [grad], [rad] and
      [turn], in the rotate property, in a transform function and in an
      unregistered custom property's stream, a converted unit rounded to the
      six-significant-figure budget a quotient takes; [lossless] keeps the unit
      as written. An [@supports] guard false however a user agent answers its
      feature tests, reading a parenthesised [<general-enclosed>] term as the
      false CSS Conditional 3 sec. 6.1 makes it ({!Supports.never_holds}), is
      dropped with its block under either reading, since no reading answers it
      yes. An identical [-webkit-text-decoration-color] compatibility
      declaration is dropped when its unprefixed twin is present; a differing or
      prefixed-only declaration is retained. These are comparison-side
      normalisations; this function does not change {!val-optimize}'s configured
      emission policy. *)

val optimize :
  ?scope:Optimize.scope ->
  ?targets:Optimize.targets ->
  ?judge:Optimize.targets ->
  ?flatten_nesting:bool ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  ?aggressive:bool ->
  ?regroup:bool ->
  ?closed_world:bool ->
  ?objective:Optimize.objective ->
  ?prune_unused_custom_props:bool ->
  ?stats:Stats.t ->
  t ->
  t
(** [optimize ?scope ?targets ?judge ?flatten_nesting ?lossless ?enforce_spec
     ?aggressive ?regroup ?closed_world ?objective ?prune_unused_custom_props
     ?stats stylesheet] applies CSS optimizations to the stylesheet, including
    merging consecutive identical selectors and combining rules with identical
    properties. Preserves CSS cascade semantics for any DOM, unless
    [closed_world] is set.

    [scope] (default [`Fragment]) gates partial-coverage shorthand synthesis.
    Pass [`Stylesheet] when the caller controls the whole author stylesheet
    graph. See {!Optimize.scope} for the details.

    [targets] defaults to {!Optimize.evergreen_targets} and owns compatibility
    prefix generation. It is ignored when [enforce_spec] is [true].

    [judge], unset by default, names the browsers the sheet is judged for; see
    {!Optimize.stylesheet}.

    When [flatten_nesting] is [true] (default [false]) the optimizer also
    desugars nested rules into flat top-level rules; see {!Optimize.stylesheet}.

    When [lossless] is [true] (default [false]), bounded colour and numeric
    approximation is disabled while exact canonicalisation still runs.
    Independent declarations retain their authored order rather than being
    sorted for compression, preserving stylesheet-text and CSSOM observability.

    When [enforce_spec] is [true] (default [false]) the optimizer drops the
    evergreen-browser target facts: a vendor-prefixed declaration is kept beside
    its unprefixed twin, a media or container feature keeps its [min-]/[max-]
    form rather than the shorter Media Queries 4 range grammar, and a nested
    selector keeps its [&] prefix.

    When [aggressive] is [true] (default [false]) the global factoring fixpoint
    runs even when the preflight predicts low gain, and the top-level
    statement-optimisation pipeline iterates until the AST reaches a structural
    fixpoint (capped at a small bound).

    When [regroup] is [true] (the default), order-dependent adjacent rule runs
    may be regrouped by factoring shared declarations and synthesising nesting.
    Canonical diff projection disables it to remain confluent; see
    {!Optimize.stylesheet}.

    When [closed_world] is [true] (default [false]) the optimizer assumes the
    caller knows the exact HTML and that no element ever matches two clashing
    selectors, so it may merge rules it would otherwise keep apart. Unsafe: the
    page can render wrong if such an element appears, including one a script
    adds at runtime. This is about the HTML, separate from [scope] (how much of
    the CSS you control). The default is safe for any page.

    [objective] (default [`Transfer]) is the size metric factoring is judged by:
    under [`Transfer], a global factoring result that grows the estimated
    DEFLATE (gzip) size of the output is discarded even when it shrinks raw
    bytes, since repeated declaration text is nearly free once compressed. Pass
    [`Raw] to keep every raw-byte win, the right objective when the output ships
    uncompressed (inline HTML style attributes, email HTML).

    When [prune_unused_custom_props] is [true] (default [false]) custom-property
    bindings referenced by no [var()] anywhere are dropped. Opt-in: it assumes a
    complete stylesheet with no out-of-band reader (another stylesheet, or
    [getComputedStyle]), the same closed-world assumption as {!inline_vars}.

    [stats] records what this run did; read it back with {!Stats.snapshot}.
    Without it the run counts into a recorder of its own that nobody reads. *)

val flatten_nesting : t -> t
(** [flatten_nesting stylesheet] returns the stylesheet with every nested rule
    flattened into a top-level rule (without running the rest of the
    optimization passes). Child selectors with [&] have the parent substituted
    in; selectors without [&] are joined to the parent with the descendant
    combinator; at-rules nested inside a rule are emitted at the top level with
    the parent selector applied to their inner rules. *)

(** {2 Closed-world inlining}

    Transforms that assume the caller controls properties the open web cannot
    guarantee (no undeclared runtime mutation, full file resolution). *)

val inline_vars :
  ?keep_vars:string list ->
  ?inline_runtime:bool ->
  ?warn:(string -> unit) ->
  t ->
  t
(** [inline_vars ?keep_vars ?inline_runtime ?warn stylesheet] substitutes
    [var(--name)] references with the value of the corresponding [--name]
    declaration and deletes the definition, but only for a variable with a
    single definition. A variable in [keep_vars], or one redefined in a
    different scope (a real cascade override such as dark mode), keeps its
    definition and stays a live [var()] reference; [warn] is called with each
    such name. The transform assumes no runtime mutation of the variables it
    inlines: a reference marked [~runtime] on {!var_ref} also stays live,
    fallback included, so a browser-time override point survives, unless
    [inline_runtime] (default [false]) is [true], in which case such a reference
    folds like any other whose variable the sheet defines once. A [style()]
    container query reads the computed value of the custom property it names, so
    that property stays live as well.

    Every [@layer] wrapper is spliced into its parent and the [@layer-decl]
    rules ordering them go with it. A [@property] registration goes only when
    the substitution left neither a declaration of its property nor a [var()]
    reading it: its [initial-value] and [inherits] descriptors decide computed
    values, so a property that stays live keeps its registration. *)

val resolve_theme :
  ?theme:Pp.String_set.t -> ?theme_defaults:(string -> string option) -> t -> t
(** [resolve_theme ?theme ?theme_defaults stylesheet] resolves theme guards and
    external theme defaults as an explicit AST transformation. {!to_string} is a
    pure formatter and does not perform this step.

    [theme] names the variables whose [var()] references stay live. When [theme]
    is given, references to any other name are inlined to the value
    [theme_defaults] resolves for it.

    [theme_defaults] maps a custom-property name to its value and is the source
    of global theme-token definitions. An answer binds only when the name and
    the value make one custom-property declaration - a [<dashed-ident>] name and
    a CSS Syntax 3 (ED) sec. 7.2 [<declaration-value>], as
    {!Declaration.parse_custom_property} checks. Any other answer, such as one
    carrying a [}] or a top-level [;], reads as no default at all and leaves the
    reference live. Every [var()] reference that is undefined in [stylesheet]
    and resolvable through [theme_defaults] - transitively, and only when the
    whole chain closes without a cycle or dead end - is emitted as a definition
    in the root-scope theme block: merged into an existing [:root] / [:host]
    rule, or a fresh [:root]. A name with no [theme_defaults] value (e.g. a
    runtime [--tw-*] variable) is left free, so non-theme variables are gated
    out.

    Root scope is deliberate. Per CSS Custom Properties L1 a custom property is
    inherited and resolved per element at computed-value time, so [var(--x)]
    needs [--x] defined on the element or an ancestor. A value [theme_defaults]
    supplies is a global token: defining it at [:root] / [:host] makes it
    inherit to every element and stay globally overridable, whereas defining it
    on the element-scoped rule that happens to reference it would confine and
    shadow it. *)

val add_var_fallbacks : (string -> string option) -> t -> t
(** [add_var_fallbacks lookup stylesheet] gives every [var(--name)] reference in
    [stylesheet] that has no fallback of its own the value [lookup "--name"]
    answers, as its fallback, in every declaration a custom property's value
    included. A reference that already carries a fallback keeps it, and the
    references inside that fallback are given theirs, so [var(--a, var(--b))]
    can become [var(--a, var(--b, 1px))]. A name [lookup] does not answer, and
    an answer that is not a CSS Syntax 3 (ED) sec. 7.2 [<declaration-value>],
    leave the reference as it was.

    This is how a sheet whose tokens are declared somewhere else still resolves
    each one where no declaration reaches it: CSS Variables 1 sec. 3 substitutes
    the fallback for a custom property left at its guaranteed-invalid initial
    value, and a declared one still wins. Unlike {!resolve_theme}, no reference
    is inlined and no definition is emitted. *)

val decode_import_url : string -> string
(** [decode_import_url s] strips the [url(...)] wrapper and any surrounding
    quotes from an [@import] URL string as held in
    {!Stylesheet.import_rule.url}. The parser preserves the verbatim source form
    there for round-tripping; this helper recovers the bare URL. *)

val inline_imports :
  ?query:Context.query -> ?layer_order:string list -> Context.loader -> t -> t
(** [inline_imports ?query ?layer_order loader stylesheet] replaces every
    [@import] rule in [stylesheet] with the body of the imported stylesheet
    looked up through [loader]. Imports the loader cannot resolve, or that fail
    their {!val-media}/{!val-supports}/{!val-layer} guard, are left in place.
    The walk descends into nested at-rules and rule bodies, so imports declared
    inside them are inlined too; the caller is responsible for preloading
    [loader.imports] with every transitively-referenced stylesheet body. *)

val will_change : will_change -> declaration
(** [will_change value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/will-change}
     will-change} property for performance optimization. *)

val inline_style_of_declarations :
  ?optimize:bool -> ?minify:bool -> ?mode:mode -> declaration list -> string
(** [inline_style_of_declarations declarations] converts a list of declarations
    to an inline style string. *)

(** {2 Pretty-printing functions for types} *)

val pp_display : display Pp.t
(** [pp_display] is the pretty printer for display values. *)

val pp_position : position Pp.t
(** [pp_position] is the pretty printer for position values. *)

val pp_length : ?always:bool -> length Pp.t
(** [pp_length ?always] is the pretty printer for length values. When [always]
    is true, units are always included even for zero values. *)

val pp_color : color Pp.t
(** [pp_color] is the pretty printer for color values. *)

val pp_angle : angle Pp.t
(** [pp_angle] is the pretty printer for angle values. *)

val pp_duration : duration Pp.t
(** [pp_duration] is the pretty printer for duration values. *)

val pp_font_weight : font_weight Pp.t
(** [pp_font_weight] is the pretty printer for font-weight values. *)

val pp_cursor : cursor Pp.t
(** [pp_cursor] is the pretty printer for cursor values. *)

val pp_animation : animation Pp.t
(** [pp_animation] is the pretty printer for animation values. *)

val pp_gradient_direction : gradient_direction Pp.t
(** [pp_gradient_direction] is the pretty printer for gradient directions. *)

val pp_transform : transform Pp.t
(** [pp_transform] is the pretty printer for transform values. *)

val pp_calc :
  ?unwrap_num:bool ->
  ?unwrap:('a -> bool) ->
  ?pp_unwrapped:'a Pp.t ->
  'a Pp.t ->
  'a calc Pp.t
(** [pp_calc ?unwrap_num ?unwrap ?pp_unwrapped pp_value] is the pretty printer
    for calc expressions. [pp_unwrapped] writes the one leaf that comes out of
    the call, which is no longer an operand; it defaults to [pp_value]. Minified
    output drops the call around a single leaf, and [unwrap] says which leaves
    that is safe for. [unwrap_num] is the same question for a bare number leaf,
    which a property taking an [<integer>] answers no to. *)

val pp_font_style : font_style Pp.t
(** [pp_font_style] is the pretty printer for font-style values. *)

val pp_text_align : text_align Pp.t
(** [pp_text_align] is the pretty printer for text-align values. *)

val pp_text_decoration : text_decoration Pp.t
(** [pp_text_decoration] is the pretty printer for text-decoration values. *)

val pp_text_transform : text_transform Pp.t
(** [pp_text_transform] is the pretty printer for text-transform values. *)

val pp_text_wrap_mode : text_wrap_mode Pp.t
(** [pp_text_wrap_mode] is the pretty printer for text-wrap-mode values. *)

val pp_text_wrap_style : text_wrap_style Pp.t
(** [pp_text_wrap_style] is the pretty printer for text-wrap-style values. *)

val pp_text_box_trim : text_box_trim Pp.t
(** [pp_text_box_trim] is the pretty printer for text-box-trim values. *)

val pp_text_spacing_trim : text_spacing_trim Pp.t
(** [pp_text_spacing_trim] is the pretty printer for text-spacing-trim values.
*)

val pp_hyphenate_limit_chars : hyphenate_limit_chars Pp.t
(** [pp_hyphenate_limit_chars] is the pretty printer for hyphenate-limit-chars
    values. *)

val pp_initial_letter : initial_letter Pp.t
(** [pp_initial_letter] is the pretty printer for initial-letter values. *)

val pp_overflow : overflow Pp.t
(** [pp_overflow] is the pretty printer for overflow values. *)

val pp_border_spacing : border_spacing Pp.t
(** [pp_border_spacing] is the pretty printer for border-spacing values. *)

val pp_border_style : border_style Pp.t
(** [pp_border_style] is the pretty printer for border-style values. *)

val pp_outline_style : outline_style Pp.t
(** [pp_outline_style] is the pretty printer for outline-style values. *)

val pp_scroll_snap_strictness : scroll_snap_strictness Pp.t
(** [pp_scroll_snap_strictness] is the pretty printer for scroll-snap-strictness
    values. *)

val pp_flex_direction : flex_direction Pp.t
(** [pp_flex_direction] is the pretty printer for flex-direction values. *)

val pp_flex_flow : flex_flow Pp.t
(** [pp_flex_flow] is the pretty printer for flex-flow values. *)

val pp_flex_factor : flex_factor Pp.t
(** [pp_flex_factor] is the pretty printer for flex factor values. *)

val pp_align_items : align_items Pp.t
(** [pp_align_items] is the pretty printer for align-items values. *)

val pp_justify_content : justify_content Pp.t
(** [pp_justify_content] is the pretty printer for justify-content values. *)

val media_min_width_length : length -> Media.t
(** [media_min_width_length l] creates a [min-width] media condition from a CSS
    length. Bridges the type gap between [Css.length] and {!module-Media}'s
    internal length type. *)

val media_not_min_width_length : length -> Media.t
(** [media_not_min_width_length l] creates a negated [min-width] media condition
    from a CSS length. *)

val parse_length : string -> length option
(** [parse_length s] parses a CSS length string (including [calc()] expressions)
    using the CSS reader. Returns {!constructor-None} if parsing fails. *)

val parse_color : string -> color option
(** [parse_color s] parses a CSS color string (e.g., ["rgba(48,163,0,0.14)"],
    ["oklch(0.5 0.2 240)"]) using the CSS reader. Returns {!constructor-None} if
    parsing fails. *)

val parse_shadow : string -> shadow option
(** [parse_shadow s] parses a CSS shadow string, including comma-separated
    multi-shadow values. Returns {!constructor-None} if parsing fails. *)

val parse_font_family : string -> font_family option
(** [parse_font_family s] parses a CSS [font-family] value: a single family, a
    generic keyword, or a comma-separated stack. Returns {!constructor-None} if
    parsing fails. *)

val parse_list_style_type : string -> list_style_type option
(** [parse_list_style_type s] parses a CSS [list-style-type] value (a counter
    style keyword, a string, or [symbols()]). Returns {!constructor-None} if
    parsing fails. *)

val parse_list_style_image : string -> list_style_image option
(** [parse_list_style_image s] parses a CSS [list-style-image] value ([none], a
    [url()], or a gradient). Returns {!constructor-None} if parsing fails. *)

val parse_background_image : string -> background_image list option
(** [parse_background_image s] parses a CSS background-image value, including
    comma-separated multiple images. Returns {!constructor-None} if parsing
    fails. *)
