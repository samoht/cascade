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
    let root = rule ~selector:(Selector.of_string ":root") [ def ]
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

(** {2:css_rules CSS Rules and Stylesheets}

    Core building blocks for CSS rules and stylesheet construction.

    See {:https://www.w3.org/TR/css-syntax-3/ CSS Syntax Module Level 3} and
    {:https://www.w3.org/TR/css-nesting-1/ CSS Nesting Module Level 1}. *)

type declaration = Declaration.declaration
(** The type for CSS declarations (property-value pairs). *)

type statement = Stylesheet.statement
(** The type for CSS statements. *)

(** Cascade origins from CSS Cascading and Inheritance. *)
type cascade_origin = Stylesheet.cascade_origin =
  | User_agent
  | User
  | Author_presentational_hint
  | Author
  | Animation
  | Transition

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

type 'a env = 'a Values.env = {
  name : string;
  indices : int list;
  fallback : 'a option;
}
(** CSS [env()] reference. *)

val var_name : 'a var -> string
(** [var_name v] is [v]'s variable name (without [--]). *)

val var_layer : 'a var -> string option
(** [var_layer v] is the optional layer where [v] is defined. *)

val with_fallback : 'a var -> 'a -> 'a var
(** [with_fallback var_ref fallback_value] creates a new variable reference with
    the same variable name but a different fallback value. This is useful when
    you need to reference a variable from another module with a specific
    fallback, without creating a declaration. *)

(** The type of CSS variables. *)
type any_var = Variables.any_var = V : 'a var -> any_var

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

(** CSS calc operations. *)
type calc_op = Values.calc_op = Add | Sub | Mul | Div

(** CSS Values 4 sec. 10.7 math constants - emitted at the source byte sequence
    so pretty pp preserves [calc(2 * pi)] instead of writing
    [calc(6.28318530718)]. *)
type math_const = Values.math_const = Pi | E | Infinity | Neg_infinity | Nan

(** CSS Values 4 (ED) sec. 9.1 numeric math function arguments. *)
type math_arg = Values.math_arg =
  | Lit of float
  | Dim of float * string  (** A dimension argument (e.g. [1vw], [1%]). *)
  | Const of math_const
  | Var_arg of math_arg var
  | Op of math_arg * calc_op * math_arg
  | Parens_arg of math_arg
  | Math_call of math_fn

(** CSS Values 4 (ED) sec. 9.1 numeric math functions. *)
and math_fn = Values.math_fn =
  | Sin of angle_arg
  | Cos of angle_arg
  | Tan of angle_arg
  | Asin of math_arg
  | Acos of math_arg
  | Atan of math_arg
  | Atan2 of math_arg * math_arg
  | Sqrt of math_arg
  | Exp of math_arg
  | Log of math_arg * math_arg option
  | Pow of math_arg * math_arg
  | Hypot of math_arg list
  | Sign_n of math_arg
  | Abs_n of math_arg

(** [sin] / [cos] / [tan] arg: an [<angle>] or unitless [<number>] (radians).
    {!constructor-Operation} and {!constructor-Grouped} support arithmetic over
    angles. *)
and angle_arg = Values.angle_arg =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Numeric_arg of math_arg
  | Operation of angle_arg * calc_op * angle_arg
  | Grouped of angle_arg

(** CSS calc values. *)
type 'a calc = 'a Values.calc =
  | Var of 'a var  (** CSS variable *)
  | Val of 'a
  | Num of float  (** Unitless number *)
  | Math_const of math_const
      (** CSS Values 4 sec. 10.7 math constant ([pi], [e], [infinity],
          [-infinity], [NaN]) preserved verbatim through pretty pp. *)
  | Sibling_index  (** CSS [sibling-index()] math function. *)
  | Sibling_count  (** CSS [sibling-count()] math function. *)
  | Expr of 'a calc * calc_op * 'a calc
  | Nested of 'a calc  (** Explicitly nested calc() *)
  | Parens of 'a calc  (** Parenthesized expression *)
  | Math_fn of math_fn
      (** CSS Values 4 (ED) sec. 9.1 numeric math function call. *)

type component_values = Component.t list
(** Parsed CSS component values preserved for fallback and invalid-value
    round-tripping. Prefer typed values in normal user code. *)

type invalid_value = component_values
(** Spec-invalid value fragments preserved until optimization decides whether to
    drop the containing declaration. *)

type custom_value = component_values
(** CSS custom-property token stream. *)

type 'a fallback = 'a Values.fallback =
  | Empty  (** Empty fallback: var(--name,) *)
  | Empty2
      (** 2-char empty fallback: var(--name, ) -- matches tailwindcss output,
          likely a bug in tailwindcss *)
  | None  (** No fallback: var(--name) *)
  | Fallback of 'a  (** Value fallback: var(--name, value) *)
  | Syntax_fallback of component_values
      (** Syntactic declaration-value fallback when it is not a typed value. *)
  | Var_fallback of string
      (** Nested var fallback: var(--name, var(--fallback)) *)

type attr_syntax = Values.attr_syntax =
  | Length
  | Length_percentage
  | Color
  | Number
  | Percentage

type attr_type = Values.attr_type =
  | Type of attr_syntax
  | Unit of string
  | Raw_string
  | Number_type

type 'a attr_fallback = 'a Values.attr_fallback =
  | No_fallback
  | Empty_fallback
  | Attr_fallback of 'a

type 'a attr_call = 'a Values.attr_call = {
  name : string;
  type_ : attr_type option;
  fallback : 'a attr_fallback;
}

(** {2:values CSS Values & Units}

    Core value types used across CSS properties.

    See {:https://www.w3.org/TR/css-values-4/ CSS Values and Units Module Level
    4}. *)

(** CSS length values.

    Supports absolute, relative, viewport (including dynamic/large/small),
    character-based units, keywords, and calculated expressions. *)
type length = Values.length =
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Ric of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Cqw of float  (** Container query width units *)
  | Cqh of float  (** Container query height units *)
  | Cqi of float  (** Container query inline-size units *)
  | Cqb of float  (** Container query block-size units *)
  | Cqmin of float  (** Smaller container query dimension units *)
  | Cqmax of float  (** Larger container query dimension units *)
  | Ch of float  (** Character units *)
  | Lh of float  (** Line height units *)
  | Dimension of { value : float; unit : string; repr : string }
      (** Dimension with authored numeric spelling preserved for pretty
          printing. *)
  | Size  (** [size] keyword inside [calc-size()]. *)
  | Auto
  | None  (** none keyword (e.g., for max-width) *)
  | Normal  (** [normal] keyword (letter-spacing, word-spacing, line-height) *)
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content  (** fit-content keyword *)
  | Fit_content_arg of length
      (** [fit-content(<length-percentage>)]; the argument is stored via
          {!type-length} because that type already has a [Pct of float] case for
          the percentage form. *)
  | Content  (** content keyword *)
  | Contain  (** contain keyword (intrinsic sizing) *)
  | Max_content  (** max-content keyword *)
  | Min_content  (** min-content keyword *)
  | Webkit_max_content  (** -webkit-max-content (legacy intrinsic sizing) *)
  | Webkit_min_content  (** -webkit-min-content (legacy intrinsic sizing) *)
  | Webkit_fit_content  (** -webkit-fit-content (legacy intrinsic sizing) *)
  | Moz_max_content  (** -moz-max-content (legacy intrinsic sizing) *)
  | Moz_min_content  (** -moz-min-content (legacy intrinsic sizing) *)
  | Moz_fit_content  (** -moz-fit-content (legacy intrinsic sizing) *)
  | From_font  (** from-font keyword for text-decoration-thickness *)
  | Hairline  (** hairline line-width keyword for text-decoration-thickness *)
  | Thin  (** thin line-width keyword for text-decoration-thickness *)
  | Medium  (** medium line-width keyword for text-decoration-thickness *)
  | Thick  (** thick line-width keyword for text-decoration-thickness *)
  | Stretch  (** stretch keyword (intrinsic sizing) *)
  | Clamp of length * length * length  (** CSS [clamp(min, val, max)]. *)
  | Min of length list  (** CSS [min(a, b, ...)]. *)
  | Max of length list  (** CSS [max(a, b, ...)]. *)
  | Minmax of length * length  (** CSS [minmax(min, max)] (grid). *)
  | Round of string * length * length  (** CSS [round()] math function *)
  | Mod of length * length  (** CSS [mod()] math function *)
  | Rem_fn of length * length  (** CSS [rem()] math function *)
  | Hypot of length list  (** CSS [hypot()] math function *)
  | Abs of length  (** CSS [abs()] math function *)
  | Sign of length  (** CSS [sign()] math function *)
  | Calc_size of length * length calc  (** CSS [calc-size()] function *)
  | Anchor_size of string
      (** CSS [anchor-size()] function, from
          {{:https://www.w3.org/TR/css-anchor-position-1/} CSS Anchor
           Positioning Level 1}. *)
  | Anchor of string option * string * length option
      (** CSS [anchor()] function: optional anchor name, side, and fallback. *)
  | Attr of length attr_call
      (** CSS [attr()] in typed value contexts (CSS Values 5 sec. 8.7). *)
  | Env of length env  (** CSS [env()] reference. *)
  | Var of length var  (** CSS variable reference *)
  | Calc of length calc  (** Calculated expressions *)

(** Builder functions for calc() expressions. *)
module Calc : sig
  val add : 'a calc -> 'a calc -> 'a calc
  (** [add left right] is [left + right]. *)

  val sub : 'a calc -> 'a calc -> 'a calc
  (** [sub left right] is [left - right]. *)

  val mul : 'a calc -> 'a calc -> 'a calc
  (** [mul left right] is [left * right]. *)

  val div : 'a calc -> 'a calc -> 'a calc
  (** [div left right] is [left / right]. *)

  val ( + ) : 'a calc -> 'a calc -> 'a calc
  (** [x + y] is {!add} [x] [y]. *)

  val ( - ) : 'a calc -> 'a calc -> 'a calc
  (** [x - y] is {!sub} [x] [y]. *)

  val ( * ) : 'a calc -> 'a calc -> 'a calc
  (** [x * y] is {!mul} [x] [y]. *)

  val ( / ) : 'a calc -> 'a calc -> 'a calc
  (** [x / y] is {!div} [x] [y]. *)

  val length : length -> length calc
  (** [length len] is [len] lifted into {!type-calc}. *)

  val var : ?default:'a -> ?fallback:'a fallback -> string -> 'a calc
  (** [var ?default ?fallback name] is a variable reference for {!type-calc}
      expressions. Example: [var "spacing"] or
      [var ~fallback:(Rem 1.2) "tw-leading"]. *)

  val float : float -> 'a calc
  (** [float f] is a numeric value for {!type-calc} expressions. It is unitless,
      so it works in a calc of any leaf type (e.g. a flex-basis multiplier), not
      only lengths. *)

  val infinity : 'a calc
  (** [infinity] is the CSS infinity value for {!type-calc} expressions. *)

  val px : float -> length calc
  (** [px n] is a pixel value for {!type-calc} expressions. *)

  val rem : float -> length calc
  (** [rem f] is a rem value for {!type-calc} expressions. *)

  val em : float -> length calc
  (** [em f] is an em value for {!type-calc} expressions. *)

  val pct : float -> length calc
  (** [pct f] is a percentage value for {!type-calc} expressions. *)

  val nested : 'a calc -> 'a calc
  (** [nested inner] wraps [inner] in an explicit nested [calc()] call. This
      produces output like [calc(calc(...)*...)] instead of [calc(...*...)]. *)

  val parens : 'a calc -> 'a calc
  (** [parens inner] wraps [inner] in parentheses only. This produces output
      like [calc(...*(...))] instead of [calc(...*calc(...))]. *)
end

type 'a property = 'a Properties.property
(** GADT for typed CSS properties. *)

(** CSS color spaces for color-mix() *)
type color_space = Values.color_space =
  | Srgb
  | Srgb_linear
  | Display_p3
  | A98_rgb
  | Prophoto_rgb
  | Rec2020
  | Lab
  | Oklab
  | Xyz
  | Xyz_d50
  | Xyz_d65
  | Lch
  | Oklch
  | Hsl
  | Hwb

(** CSS named colors as defined in the CSS Color Module specification. *)
type color_name = Values.color_name =
  | Red
  | Blue
  | Green
  | White
  | Black
  | Yellow
  | Cyan
  | Magenta
  | Gray
  | Grey
  | Orange
  | Purple
  | Pink
  | Silver
  | Maroon
  | Fuchsia
  | Lime
  | Olive
  | Navy
  | Teal
  | Aqua
  | Alice_blue
  | Antique_white
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Blanched_almond
  | Blue_violet
  | Brown
  | Burlywood
  | Cadet_blue
  | Chartreuse
  | Chocolate
  | Coral
  | Cornflower_blue
  | Cornsilk
  | Crimson
  | Dark_blue
  | Dark_cyan
  | Dark_goldenrod
  | Dark_gray
  | Dark_green
  | Dark_grey
  | Dark_khaki
  | Dark_magenta
  | Dark_olive_green
  | Dark_orange
  | Dark_orchid
  | Dark_red
  | Dark_salmon
  | Dark_sea_green
  | Dark_slate_blue
  | Dark_slate_gray
  | Dark_slate_grey
  | Dark_turquoise
  | Dark_violet
  | Deep_pink
  | Deep_sky_blue
  | Dim_gray
  | Dim_grey
  | Dodger_blue
  | Firebrick
  | Floral_white
  | Forest_green
  | Gainsboro
  | Ghost_white
  | Gold
  | Goldenrod
  | Green_yellow
  | Honeydew
  | Hot_pink
  | Indian_red
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | Lavender_blush
  | Lawn_green
  | Lemon_chiffon
  | Light_blue
  | Light_coral
  | Light_cyan
  | Light_goldenrod_yellow
  | Light_gray
  | Light_green
  | Light_grey
  | Light_pink
  | Light_salmon
  | Light_sea_green
  | Light_sky_blue
  | Light_slate_gray
  | Light_slate_grey
  | Light_steel_blue
  | Light_yellow
  | Lime_green
  | Linen
  | Medium_aquamarine
  | Medium_blue
  | Medium_orchid
  | Medium_purple
  | Medium_sea_green
  | Medium_slate_blue
  | Medium_spring_green
  | Medium_turquoise
  | Medium_violet_red
  | Midnight_blue
  | Mint_cream
  | Misty_rose
  | Moccasin
  | Navajo_white
  | Old_lace
  | Olive_drab
  | Orange_red
  | Orchid
  | Pale_goldenrod
  | Pale_green
  | Pale_turquoise
  | Pale_violet_red
  | Papaya_whip
  | Peach_puff
  | Peru
  | Plum
  | Powder_blue
  | Rebecca_purple
  | Rosy_brown
  | Royal_blue
  | Saddle_brown
  | Salmon
  | Sandy_brown
  | Sea_green
  | Sea_shell
  | Sienna
  | Sky_blue
  | Slate_blue
  | Slate_gray
  | Slate_grey
  | Snow
  | Spring_green
  | Steel_blue
  | Tan
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White_smoke
  | Yellow_green

(** CSS channel values (for RGB) *)
type channel = Values.channel =
  | Int of int (* 0-255, legacy/comma syntax *)
  | Num of float (* 0-255, modern/space syntax *)
  | Pct of float (* 0%-100% *)
  | Var of channel var
  | None (* CSS Color 4 [none] sentinel *)

type rgb = Values.rgb =
  | Channels of { r : channel; g : channel; b : channel }
  | Var of rgb var

(** CSS alpha values (for HSL/HWB/etc) *)
type alpha = Values.alpha =
  | None
  | Num of float (* Number value (0-1) *)
  | Pct of float (* Percentage value (0%-100%) *)
  | Var of alpha var
  | Calc of alpha calc

(** CSS hue values (for HSL/HWB) *)
type hue = Values.hue =
  | Unitless of float (* Unitless number, defaults to degrees *)
  | Angle of Values.angle (* Explicit angle unit *)
  | Var of hue var
  | Hue_none

(** CSS color component values *)
type component = Values.component =
  | Num of float
  | Pct of float
  | Angle of hue (* for color(lch ...) / color(lab ...) syntaxes *)
  | Var of component var
  | Calc of component calc
  | Component_none

(** CSS percentage values *)
type percentage = Values.percentage =
  | Pct of float (* 0%-100% as a % token *)
  | Num of float (* Numeric value in percentage context, e.g., opacity 0.5 *)
  | Var of percentage var
  | Calc of percentage calc (* calc(...) that resolves to a % *)

(** CSS length or percentage values. *)
type length_percentage = Values.length_percentage =
  | Length of length
  | Pct of float
  | Env of length_percentage env
  | Var of length_percentage var
  | Calc of length_percentage calc
  | Invalid of invalid_value  (** Spec-invalid input preserved verbatim. *)

(** CSS number or percentage values (for properties like scale, brightness) *)
type number_percentage = Values.number_percentage =
  | Num of float
  | Pct of float
  | Var of number_percentage var
  | Calc of number_percentage calc

(** CSS hue interpolation options *)
type hue_interpolation = Values.hue_interpolation =
  | Shorter
  | Longer
  | Increasing
  | Decreasing
  | Specified
  | Default

(** CSS system colors - case-insensitive keywords that map to OS/browser colors.
    These are semantic colors that adapt to user preferences and system
    settings. *)
type system_color = Values.system_color =
  | Accent_color  (** Background of accented user interface controls *)
  | Accent_color_text  (** Text of accented user interface controls *)
  | Active_text  (** Text of active links *)
  | Button_border  (** Base border color of controls *)
  | Button_face  (** Background color of controls *)
  | Button_text  (** Text color of controls *)
  | Canvas  (** Background of application content or documents *)
  | Canvas_text  (** Text color in application content or documents *)
  | Field  (** Background of input fields *)
  | Field_text  (** Text in input fields *)
  | Gray_text  (** Text color for disabled items *)
  | Highlight  (** Background of selected items *)
  | Highlight_text  (** Text color of selected items *)
  | Link_text  (** Text of non-active, non-visited links *)
  | Mark  (** Background of specially marked text *)
  | Mark_text  (** Text that has been specially marked *)
  | Selected_item  (** Background of selected items (e.g., selected checkbox) *)
  | Selected_item_text  (** Text of selected items *)
  | Visited_text  (** Text of visited links *)
  | Webkit_focus_ring_color  (** WebKit-specific focus ring color *)

(** CSS color values. *)
type color = Values.color =
  | Hex of { r : int; g : int; b : int; a : int }
      (** Hex colour decoded to sRGB byte components ([a = 255] when opaque). *)
  | Authored_hex of { value : string; r : int; g : int; b : int; a : int }
      (** Parsed hex colour preserving the source spelling without the leading
          [#]. Optimisation folds this to the canonical semantic colour. *)
  | Rgb of rgb
  | Rgba of { rgb : rgb; a : alpha }
  | Hsl of { h : hue; s : percentage; l : percentage; a : alpha }
  | Hwb of { h : hue; w : percentage; b : percentage; a : alpha }
  | Color of { space : color_space; components : component list; alpha : alpha }
  | Relative_rgb of color * string
      (** [rgb(from <origin> <channels> [/ <alpha>]?)] with a parsed origin and
          an opaque channel-expression tail. *)
  | Relative_color of string * color * string
      (** [<fn>(from <origin> <c1> <c2> <c3> [/ <alpha>]?)] for relative color
          functions other than [rgb()]. *)
  | Contrast_color of color
  | Light_dark of color * color
  | Attribute of string * color option
  | Lab of {
      l : percentage option;
      a : float option;
      b : float option;
      alpha : alpha;
    }  (** Lab color space. l, a and b can be [None] to represent CSS [none]. *)
  | Oklch of { l : percentage option; c : float option; h : hue; alpha : alpha }
      (** OKLCH color space. l and c can be [None] to represent CSS [none]. *)
  | Oklab of {
      l : percentage option;
      a : float option;
      b : float option;
      alpha : alpha;
    }
      (** Oklab color space. l, a and b can be [None] to represent CSS 'none'
          keyword. *)
  | Lch of { l : percentage option; c : float option; h : hue; alpha : alpha }
      (** LCH color space. l and c can be [None] to represent CSS [none]. *)
  | Named of color_name  (** Named colors like Red, Blue, etc. *)
  | System of system_color
      (** CSS system colors like Button_text, Canvas, etc. *)
  | Var of color var
  | Current
  | Transparent
  | Auto  (** [auto] keyword, e.g. [accent-color: auto], [caret-color: auto]. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Mix of {
      in_space : color_space option; (* None => default per spec *)
      hue : hue_interpolation;
      color1 : color;
      percent1 : percentage option;
      color2 : color;
      percent2 : percentage option;
    }

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

(** CSS angle values *)
type angle = Values.angle =
  | Deg of float
  | Rad of float
  | Turn of float
  | Grad of float
  | Round of string * angle * angle
  | Mod of angle * angle
  | Rem of angle * angle
  | Calc of angle calc  (** Calculated angle expressions *)
  | Var of angle var
  | Invalid of invalid_value
      (** Spec-invalid input the parser keeps verbatim; [Optimize.drop_invalid]
          drops the declaration on every serialisation. *)

(** CSS number values (unitless numbers for filters, transforms, etc.) *)
type number = Values.number =
  | Num of float  (** Number value *)
  | Var of number var  (** CSS variable reference *)
  | Calc of number calc
  | Round of string * number * number  (** CSS [round()] math function *)
  | Mod of number * number  (** CSS [mod()] math function *)
  | Rem of number * number  (** CSS [rem()] math function *)
  | Hypot of number * number  (** CSS [hypot()] math function *)
  | Pow of number * number  (** CSS [pow()] math function *)
  | Sqrt of number  (** CSS [sqrt()] math function *)
  | Abs of number  (** CSS [abs()] math function *)
  | Sign of number  (** CSS [sign()] math function *)
  | Sin of angle  (** CSS [sin()] math function *)

(** CSS aspect-ratio values *)
type aspect_ratio = Properties.aspect_ratio =
  | Auto
  | Auto_ratio of float * float
  | Ratio of float * float
  | Auto_ratio_calc of number * number
  | Ratio_calc of number * number
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of aspect_ratio var

val ratio : float -> float -> aspect_ratio
(** [ratio width height] is an [aspect-ratio] value such as [16 / 9]. *)

val auto_ratio : float -> float -> aspect_ratio
(** [auto_ratio width height] is an [aspect-ratio] value such as [auto 16 / 9].
*)

(** CSS blend-mode values *)
type blend_mode = Properties.blend_mode =
  | Normal
  | Multiply
  | Screen
  | Overlay
  | Darken
  | Lighten
  | Color_dodge
  | Color_burn
  | Hard_light
  | Soft_light
  | Difference
  | Exclusion
  | Hue
  | Saturation
  | Color
  | Luminosity
  | Plus_darker
  | Plus_lighter
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of blend_mode var

(** The optional value paired with an OpenType feature tag. *)
type font_feature_value = Properties.font_feature_value =
  | On
  | Off
  | Index of int

type font_feature_setting = Properties.font_feature_setting = {
  tag : string;
  value : font_feature_value option;
}
(** One OpenType feature tag and its optional value. *)

(** CSS font-feature-settings values. *)
type font_feature_settings = Properties.font_feature_settings =
  | Normal
  | Feature_list of font_feature_setting list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_feature_settings var

type font_variation_setting = Properties.font_variation_setting = {
  tag : string;
  value : float;
}
(** One OpenType variation axis and its numeric value. *)

(** CSS font-variation-settings values. *)
type font_variation_settings = Properties.font_variation_settings =
  | Normal
  | Axis_list of font_variation_setting list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variation_settings var

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

(** CSS box sizing values. *)
type box_sizing = Properties.box_sizing =
  | Border_box
  | Content_box
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of box_sizing var

(** CSS field sizing values. *)
type field_sizing = Properties.field_sizing =
  | Content
  | Fixed
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of field_sizing var

(** CSS caption side values. *)
type caption_side = Properties.caption_side =
  | Top
  | Bottom
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caption_side var

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

type border_width = Properties.border_width =
  | Thin
  | Medium
  | Thick
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Ric of float
  | Rlh of float
  | Ch of float
  | Lh of float
  | Vh of float
  | Vw of float
  | Vmin of float
  | Vmax of float
  | Pct of float
  | Dimension of { value : float; unit : string; repr : string }
      (** A length in a unit [border_width] does not name, carrying the authored
          spelling in [repr] the way {!length} does. *)
  | Zero
  | Auto
  | Max_content
  | Min_content
  | Fit_content
  | From_font
  | Calc of border_width calc
  | Min of border_width calc list
  | Max of border_width calc list
  | Clamp of border_width calc * border_width calc * border_width calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_width var

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

(** CSS display values. *)
type display = Properties.display =
  | Block
  | Inline
  | Inline_block
  | Flex
  | Inline_flex
  | Grid
  | Inline_grid
  | Grid_lanes
  | Inline_grid_lanes
  | None
  | Flow_root
  | Table
  | Table_row
  | Table_cell
  | Table_caption
  | Table_column
  | Table_column_group
  | Table_footer_group
  | Table_header_group
  | Table_row_group
  | Inline_table
  | List_item
  | Contents
  | Run_in
  | Ruby
  | Ruby_base
  | Ruby_text
  | Ruby_base_container
  | Ruby_text_container
  | Math
  | Webkit_flex
  | Webkit_inline_flex
  | Ms_flexbox
  | Webkit_box
  | Moz_box
  | Moz_inline_box
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Multi of display * display
      (** Two-value [<display-outside> <display-inside>] syntax per CSS Display
          3 sec. 2.1, e.g. [inline flow-root] or [list-item flow-root]. *)
  | Var of display var

(** CSS position values. *)
type position = Properties.position =
  | Static
  | Relative
  | Absolute
  | Fixed
  | Sticky
  | Webkit_sticky
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position var

(** CSS visibility values. *)
type visibility = Properties.visibility =
  | Visible
  | Hidden
  | Collapse
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of visibility var

(** CSS z-index values. *)
type z_index = Properties.z_index =
  | Auto
  | Index of int
  | Calc of z_index calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of z_index var

(** CSS opacity values. *)
type opacity = Properties.opacity =
  | Opacity_number of float
  | Calc of opacity calc
  | Abs of opacity  (** [abs(<opacity>)] *)
  | Sign of opacity  (** [sign(<opacity>)] *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of opacity var

(** CSS order values (flexbox order). *)
type order = Properties.order =
  | Int of int
  | Calc of order calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of order var

(** CSS overflow values. *)
type overflow = Properties.overflow =
  | Visible
  | Hidden
  | Scroll
  | Auto
  | Clip
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Overflow_pair of overflow * overflow
  | Var of overflow var

type border_spacing = Properties.border_spacing =
  | Lengths of length list
  | Var of border_spacing var

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

(** CSS isolation values *)
type isolation = Properties.isolation =
  | Auto
  | Isolate
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of isolation var

val isolation : isolation -> declaration
(** [isolation iso] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/isolation} isolation}
    property for stacking context control. *)

(** CSS break-before/break-after values for page/column/region breaks. *)
type break_value = Properties.break_value =
  | Auto
  | Avoid
  | All
  | Avoid_page
  | Page
  | Left
  | Right
  | Recto
  | Verso
  | Avoid_column
  | Column
  | Avoid_region
  | Region
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of break_value var

val break_before : break_value -> declaration
(** [break_before v] is the break-before property. *)

val break_after : break_value -> declaration
(** [break_after v] is the break-after property. *)

(** CSS break-inside values. *)
type break_inside_value = Properties.break_inside_value =
  | Auto
  | Avoid
  | Avoid_page
  | Avoid_column
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of break_inside_value var

val break_inside : break_inside_value -> declaration
(** [break_inside v] is the break-inside property. *)

(** CSS Fragmentation 3 sec. 3.4 deprecated [page-break-before / -after] alias
    vocabulary; the shorter value list makes these their own type rather than
    overload {!type-break_value}. *)
type page_break_value = Properties.page_break_value =
  | Auto
  | Always
  | Avoid
  | Left
  | Right
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of page_break_value var
      (** CSS Fragmentation 3 sec. 3.4 deprecated [page-break-inside]
          vocabulary. *)

type page_break_inside_value = Properties.page_break_inside_value =
  | Auto
  | Avoid
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of page_break_inside_value var

val page_break_before : page_break_value -> declaration
(** [page_break_before v] is the legacy [page-break-before] property. *)

val page_break_after : page_break_value -> declaration
(** [page_break_after v] is the legacy [page-break-after] property. *)

val page_break_inside : page_break_inside_value -> declaration
(** [page_break_inside v] is the legacy [page-break-inside] property. *)

type page_size_name = Properties.page_size_name =
  | A5
  | A4
  | A3
  | B5
  | B4
  | Jis_b5
  | Jis_b4
  | Letter
  | Legal
  | Ledger
  | Var of page_size_name var

type page_size_orientation = Properties.page_size_orientation =
  | Portrait
  | Landscape
  | Var of page_size_orientation var

(** CSS paged-media [size] descriptor values. *)
type page_size = Properties.page_size =
  | Auto
  | Single of length
  | Pair of length * length
  | Named of page_size_name
  | Named_oriented of page_size_name * page_size_orientation
  | Oriented of page_size_orientation
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of page_size var

(** CSS columns values for multi-column layout. *)
type columns_value = Properties.columns_value =
  | Auto
  | Count of int
  | Width of length
  | Both of length * int
  | Auto_count of int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of columns_value var

val columns_count : int -> columns_value
(** [columns_count count] is a column-count value for the {!val-columns}
    shorthand. *)

val columns_width : length -> columns_value
(** [columns_width width] is a column-width value for the {!val-columns}
    shorthand. *)

val columns_both : length -> int -> columns_value
(** [columns_both width count] is a combined {!val-columns} shorthand value. *)

type column_span = Properties.column_span =
  | None
  | All
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_span var

val columns : columns_value -> declaration
(** [columns v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/columns} columns}
    property for multi-column layout. *)

val column_span : column_span -> declaration
(** [column_span v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/column-span}
     column-span} property. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-width}
     [column-width]}: [auto | <length [0,inf]>]. *)
type column_width = Properties.column_width =
  | Auto
  | Width of length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_width var

val column_width : column_width -> declaration
(** [column_width v] is the [column-width] longhand of {!val-columns}. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-count}
     [column-count]}: [auto | <integer [1,inf]>]. *)
type column_count = Properties.column_count =
  | Auto
  | Count of int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_count var

val column_count : column_count -> declaration
(** [column_count v] is the [column-count] longhand of {!val-columns}. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-height}
     [column-height]}: [auto | <length [0,inf]>]. *)
type column_height = Properties.column_height =
  | Auto
  | Height of length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_height var

val column_height : column_height -> declaration
(** [column_height v] is the [column-height] property. *)

(** CSS Multicol 2
    {{:https://drafts.csswg.org/css-multicol-2/#propdef-column-wrap}
     [column-wrap]}: [auto | nowrap | wrap]. *)
type column_wrap = Properties.column_wrap =
  | Auto
  | Nowrap
  | Wrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of column_wrap var

val column_wrap : column_wrap -> declaration
(** [column_wrap v] is the [column-wrap] property. *)

val visibility : visibility -> declaration
(** [visibility v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/visibility} visibility}
    property. *)

(** CSS float side values. *)
type float_side = Properties.float_side =
  | None
  | Left
  | Right
  | Inline_start
  | Inline_end
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of float_side var

val float : float_side -> declaration
(** [float side] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/float} float} property.
*)

(** CSS clear values. *)
type clear = Properties.clear =
  | None
  | Left
  | Right
  | Both
  | Inline_start
  | Inline_end
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of clear var

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

(** CSS content values *)
type content = Properties.content =
  | String of string
  | Quoted of { value : string; quote : char; repr : string option }
  | Image of Properties.background_image
      (** The [<image>] of {!type-background_image}, aliased below. *)
  | None
  | Normal
  | Open_quote
  | Close_quote
  | Attr of content attr_call
  | Counter of string
  | Counters of string * string
  | String_ref of string
  | Content_list of content list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of content var

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

type counter_item = Properties.counter_item = {
  name : string;
  value : int option;
}

val counter_item : ?value:int -> string -> counter_item
(** [counter_item ?value name] is one named counter item. *)

type counter_set = Properties.counter_set =
  | None
  | Counters of counter_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of counter_set var

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

(** CSS object-fit values *)
type object_fit = Properties.object_fit =
  | Fill
  | Contain
  | Cover
  | None
  | Scale_down
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of object_fit var

val object_fit : object_fit -> declaration
(** [object_fit fit] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit} object-fit}
    property. *)

type position_value = Properties.position_value =
  | Center
  | Top
  | Bottom
  | Left
  | Right
  | Left_top
  | Left_center
  | Left_bottom
  | Right_top
  | Right_center
  | Right_bottom
  | Center_top
  | Center_bottom
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | XY of length * length
  | Single of length
      (** Single length/percentage value for background-position *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Edge_offset_axis of string * length_percentage * string
  | Axis_edge_offset of string * string * length_percentage
  | Edge_offset_edge_offset of
      string * length_percentage * string * length_percentage
  | Var of position_value var

val object_position : position_value -> declaration
(** [object_position pos] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/object-position}
     object-position} property. *)

(** CSS text-overflow values *)
type text_overflow = Properties.text_overflow =
  | Clip
  | Ellipsis
  | String of string
  | Pair of text_overflow * text_overflow
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_overflow var

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

(** CSS text-wrap values *)
type text_wrap = Properties.text_wrap =
  | Wrap
  | No_wrap
  | Auto
  | Balance
  | Stable
  | Pretty
  | Mode_style of [ `Wrap | `No_wrap ] * [ `Auto | `Balance | `Stable | `Pretty ]
      (** both components, printed mode-first *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap var

type text_wrap_mode = Properties.text_wrap_mode =
  | Wrap
  | No_wrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap_mode var

type text_wrap_style = Properties.text_wrap_style =
  | Auto
  | Balance
  | Pretty
  | Stable
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_wrap_style var

type text_box_trim = Properties.text_box_trim =
  | None
  | Trim_start
  | Trim_end
  | Trim_both
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_box_trim var

type text_underline_position_keyword =
      Properties.text_underline_position_keyword =
  | Under
  | Left
  | Right

type text_underline_position = Properties.text_underline_position =
  | Auto
  | From_font
  | Position of text_underline_position_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_underline_position var

type text_box_edge_keyword = Properties.text_box_edge_keyword =
  | Text
  | Cap
  | Ex
  | Alphabetic
  | Ideographic
  | Ideographic_ink

type text_box_edge = Properties.text_box_edge =
  | Auto
  | Edge of text_box_edge_keyword * text_box_edge_keyword option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_box_edge var

type inline_sizing = Properties.inline_sizing =
  | Normal
  | Stretch
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of inline_sizing var

type line_fit_edge_keyword = Properties.line_fit_edge_keyword =
  | Leading
  | Text
  | Cap
  | Ex
  | Alphabetic
  | Ideographic
  | Ideographic_ink

type line_fit_edge = Properties.line_fit_edge =
  | Edge of line_fit_edge_keyword * line_fit_edge_keyword option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of line_fit_edge var

type interpolate_size = Properties.interpolate_size =
  | Numeric_only
  | Allow_keywords
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of interpolate_size var

type min_intrinsic_sizing_keyword = Properties.min_intrinsic_sizing_keyword =
  | Legacy
  | Zero_if_scroll
  | Zero_if_extrinsic

type min_intrinsic_sizing = Properties.min_intrinsic_sizing =
  | Sizing of min_intrinsic_sizing_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of min_intrinsic_sizing var

type ruby_merge = Properties.ruby_merge =
  | Separate
  | Merge
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_merge var

type ruby_align = Properties.ruby_align =
  | Start
  | Center
  | Space_between
  | Space_around
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_align var

type ruby_overhang = Properties.ruby_overhang =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_overhang var

type ruby_position_keyword = Properties.ruby_position_keyword =
  | Alternate
  | Over
  | Under
  | Inter_character

type ruby_position = Properties.ruby_position =
  | Position of ruby_position_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of ruby_position var

type text_spacing_trim = Properties.text_spacing_trim =
  | Normal
  | Space_all
  | Trim_start
  | Space_first
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_spacing_trim var

type hyphenate_limit_chars = Properties.hyphenate_limit_chars =
  | Auto
  | One of int
  | Two of int * int
  | Three of int * int * int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of hyphenate_limit_chars var

type initial_letter = Properties.initial_letter =
  | Normal
  | Drop
  | Raise
  | Size of float
  | Size_sink of float * int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter var

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

(** CSS backface-visibility values *)
type backface_visibility = Properties.backface_visibility =
  | Visible
  | Hidden
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of backface_visibility var

val backface_visibility : backface_visibility -> declaration
(** [backface_visibility vis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/backface-visibility}
     backface-visibility} property (3D transforms). *)

(** CSS content-visibility values. *)
type content_visibility = Properties.content_visibility =
  | Visible  (** Content is visible and rendered *)
  | Hidden  (** Content is hidden from rendering *)
  | Auto  (** Browser determines visibility based on relevance *)
  | Initial
  | Inherit  (** Inherit from parent *)
  | Unset
  | Revert
  | Revert_layer
  | Var of content_visibility var

val content_visibility : content_visibility -> declaration
(** [content_visibility vis] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/content-visibility}
     content-visibility} property. *)

(** CSS quotes property values - defines quotation marks for q and blockquote.
*)
type quotes = Properties.quotes =
  | Auto  (** Browser default based on language *)
  | None  (** No quotation marks *)
  | Pairs of (string * string) list  (** One or more open/close quote pairs *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of quotes var

val quotes : quotes -> declaration
(** [quotes q] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/quotes} quotes}
    property. *)

(** CSS list-style-position values *)
type list_style_position = Properties.list_style_position =
  | Inside
  | Outside
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_position var

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

(** CSS forced-color-adjust values. *)
type forced_color_adjust = Properties.forced_color_adjust =
  | Auto
  | None
  | Preserve_parent_color
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of forced_color_adjust var

(** CSS background-repeat values. *)
type background_repeat = Properties.background_repeat =
  | Repeat
  | Space
  | Round
  | No_repeat
  | Repeat_x
  | Repeat_y
  | Layers of background_repeat list
  | Repeat_repeat
  | Repeat_space
  | Repeat_round
  | Repeat_no_repeat
  | Space_repeat
  | Space_space
  | Space_round
  | Space_no_repeat
  | Round_repeat
  | Round_space
  | Round_round
  | Round_no_repeat
  | No_repeat_repeat
  | No_repeat_space
  | No_repeat_round
  | No_repeat_no_repeat
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_repeat var

(** CSS background-size values. *)
type background_size = Properties.background_size =
  | Auto
  | Cover
  | Contain
  | Length of length
  | Size of length * length
  | Layers of background_size list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_size var

val background_size_pair : length -> length -> background_size
(** [background_size_pair width height] is a two-value [background-size]. *)

(** CSS background-attachment values. *)
type background_attachment = Properties.background_attachment =
  | Scroll
  | Fixed
  | Local
  | Layers of background_attachment list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of background_attachment var

(** CSS Color 5 section 9.1: hue-interpolation method for polar color spaces
    (lch / oklch / hsl / hwb). *)
type hue_interpolation_method = Properties.hue_interpolation_method =
  | Shorter
  | Longer
  | Increasing
  | Decreasing

(** Color interpolation for gradients *)
type color_interpolation = Properties.color_interpolation =
  | In_oklab
  | In_oklch of hue_interpolation_method option
  | In_srgb
  | In_hsl of hue_interpolation_method option
  | In_lab
  | In_lch of hue_interpolation_method option
  | Var of color_interpolation var

(** Gradient direction values *)
type gradient_direction = Properties.gradient_direction =
  | Default_direction
  | To_top
  | To_top_right
  | To_right
  | To_bottom_right
  | To_bottom
  | To_bottom_left
  | To_left
  | To_top_left
  | Angle of angle
  | With_interpolation of gradient_direction * color_interpolation
  | Var of gradient_direction var

(** Shape of a radial gradient *)
type radial_shape = Properties.radial_shape =
  | Circle
  | Ellipse
  | Var of radial_shape var

(** Size of a radial gradient *)
type radial_size = Properties.radial_size =
  | Closest_side
  | Farthest_side
  | Closest_corner
  | Farthest_corner
  | Circle_radius of length
  | Ellipse_radii of length_percentage * length_percentage
  | Var of radial_size var

type radial_gradient_config = Properties.radial_gradient_config = {
  shape : radial_shape option;
  size : radial_size option;
  position : position_value option;
  interpolation : color_interpolation option;
}
(** Configuration for radial-gradient prefix: shape, size, position, and
    optional [in <color-interpolation-method>] clause. *)

type conic_gradient_config = Properties.conic_gradient_config = {
  angle : angle option;  (** [from <angle>] starting angle *)
  position : position_value option;  (** [at <position>] center *)
  interpolation : color_interpolation option;
      (** Optional [in <color-interpolation-method>] clause. *)
}
(** Configuration for conic-gradient prefix: starting angle, center, and
    optional [in <color-interpolation-method>] clause. *)

type gradient_position = Properties.gradient_position =
  | Linear_position of gradient_direction
  | Radial_position of radial_gradient_config
  | Conic_position of conic_gradient_config
  | Var of gradient_position var

(** Gradient stop values *)
type gradient_stop = Properties.gradient_stop =
  | Color_percentage of
      color * length_percentage option * length_percentage option
      (** Color with optional percentage positions *)
  | Color_length of color * length option * length option
      (** Color with optional length positions *)
  | Length of length  (** Interpolation hint with length, e.g., "50px" *)
  | Channel of channel
      (** Residual numeric channel token from custom-property substitution. *)
  | List of gradient_stop list
      (** Multiple gradient stops - used for var fallbacks *)
  | Percentage of percentage
      (** Interpolation hint with percentage, e.g., "50%" *)
  | Position of gradient_position
  | Direction of gradient_direction
      (** Gradient direction for stops, e.g., "to right" or Var *)
  | Var of gradient_stop var

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

(** Per CSS Backgrounds and Borders 3 sec. 4.1. *)
type border_radius = Properties.border_radius =
  | Radius of {
      horizontal : length_percentage list;
          (** 1-4 horizontal radii (top-left, top-right, bottom-right,
              bottom-left). *)
      vertical : length_percentage list option;
          (** Optional 1-4 vertical radii after [/]; when [None] the horizontal
              values are used for both axes. *)
    }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_radius var

val radius : length -> border_radius
(** [radius len] is a one-value [border-radius] shorthand value.

    For example, [border_radius (radius (Rem 0.375))] renders
    [border-radius: 0.375rem]. *)

type object_view_box = Properties.object_view_box =
  | None
  | Inset of length * length option * length option * length option
  | Xywh of {
      x : length_percentage;
      y : length_percentage;
      width : length_percentage;
      height : length_percentage;
      rounded : border_radius option;
    }
  | Rect of {
      top : length_percentage;
      right : length_percentage;
      bottom : length_percentage;
      left : length_percentage;
      rounded : border_radius option;
    }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of object_view_box var

val object_view_box_inset :
  ?right:length -> ?bottom:length -> ?left:length -> length -> object_view_box
(** [object_view_box_inset ?right ?bottom ?left top] is an [inset()] object view
    box. *)

val object_view_box : object_view_box -> declaration
(** [object_view_box box] is the CSS [object-view-box] property. *)

(** Background image values *)
module Webkit_gradient : sig
  type point = Properties.Webkit_gradient.point =
    | Left_top
    | Left_bottom
    | Center
    | Position of position_value

  type stop = Properties.Webkit_gradient.stop =
    | From of color
    | Color_stop of percentage * color
    | To of color

  type t = Properties.Webkit_gradient.t =
    | Linear of { start : point; finish : point; stops : stop list }
    | Radial of {
        inner_center : point;
        inner_radius : float;
        outer_center : point;
        outer_radius : float;
        stops : stop list;
      }
end

type background_image = Properties.background_image =
  | Url of string
  | Quoted of string * char
  | Linear_gradient of gradient_direction * gradient_stop list
  | Linear_gradient_var of gradient_stop var
      (** Linear gradient using a single variable for all stops including
          position. Outputs: linear-gradient(var(--tw-gradient-stops)) *)
  | Radial_gradient of radial_gradient_config * gradient_stop list
  | Radial_gradient_var of gradient_stop var
      (** Radial gradient using a single variable for all stops. Outputs:
          radial-gradient(var(--tw-gradient-stops)) *)
  | Conic_gradient of conic_gradient_config * gradient_stop list
  | Conic_gradient_var of gradient_stop var
      (** Conic gradient using a single variable for all stops. Outputs:
          conic-gradient(var(--tw-gradient-stops)) *)
  | Repeating_linear_gradient of gradient_direction * gradient_stop list
  | Repeating_radial_gradient of radial_gradient_config * gradient_stop list
  | Repeating_conic_gradient of conic_gradient_config * gradient_stop list
      (** [repeating-{linear,radial,conic}-gradient()] CSS Images 4 sec. 3. *)
  | Webkit_linear_gradient of gradient_direction * gradient_stop list
  | Webkit_repeating_linear_gradient of gradient_direction * gradient_stop list
  | Webkit_radial_gradient of radial_gradient_config * gradient_stop list
  | Webkit_repeating_radial_gradient of
      radial_gradient_config * gradient_stop list
  | Moz_linear_gradient of gradient_direction * gradient_stop list
  | Moz_repeating_linear_gradient of gradient_direction * gradient_stop list
  | Moz_radial_gradient of radial_gradient_config * gradient_stop list
  | Moz_repeating_radial_gradient of radial_gradient_config * gradient_stop list
  | O_linear_gradient of gradient_direction * gradient_stop list
  | O_repeating_linear_gradient of gradient_direction * gradient_stop list
  | O_radial_gradient of radial_gradient_config * gradient_stop list
  | O_repeating_radial_gradient of radial_gradient_config * gradient_stop list
  | Image_set of image_set_option list
      (** [image-set(<source>#)] CSS Images 4 *)
  | Webkit_image_set of image_set_option list
      (** [-webkit-image-set(<source>#)] legacy spelling *)
  | Cross_fade of cross_fade_option list
      (** [cross-fade(<cf-mixing-image>#)] CSS Images 4 *)
  | Webkit_gradient of Webkit_gradient.t
  | List of background_image list
      (** Comma-separated list of background images *)
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of background_image var
      (** CSS variable reference: var(--my-gradient) *)

and image_set_option = Properties.image_set_option = {
  source : image_set_source;
  resolution : string option;  (** [<resolution>] like ["1x"] or ["300dpi"] *)
  mime_type : string option;  (** [type("image/avif")] *)
}

and image_set_source = Properties.image_set_source =
  | Url of string
  | String of string

and cross_fade_option = Properties.cross_fade_option = {
  image : background_image;
  percent : percentage option;
}

(** CSS background and mask box values. *)
type background_box = Properties.background_box =
  | Border_box
  | Padding_box
  | Content_box
  | Text
  | Layers of background_box list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_box var

(* Mask-related types *)
type webkit_mask_composite = Properties.webkit_mask_composite =
  | Source_over
  | Xor
  | Source_in
  | Source_out
  | Composites of webkit_mask_composite list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_mask_composite var

type mask_composite = Properties.mask_composite =
  | Add
  | Subtract
  | Intersect
  | Exclude
  | Composites of mask_composite list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_composite var

type webkit_mask_source_type = Properties.webkit_mask_source_type =
  | Alpha
  | Luminance
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_mask_source_type var

type mask_mode = Properties.mask_mode =
  | Alpha
  | Luminance
  | Match_source
  | Modes of mask_mode list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_mode var

type mask_type = Properties.mask_type =
  | Alpha
  | Luminance
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_type var

type mask_box = Properties.mask_box =
  | Border_box
  | Content_box
  | Fill_box
  | Padding_box
  | Stroke_box
  | View_box
  | No_clip  (** Only valid for mask-clip *)
  | Layers of mask_box list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of mask_box var

type mask_layer = Properties.mask_layer = {
  image : background_image option;
  position : position_value option;
  size : background_size option;
  repeat : background_repeat option;
  origin : mask_box option;
  clip : mask_box option;
  mode : mask_mode option;
  composite : mask_composite option;
}

type mask = Properties.mask =
  | None
  | Layer of mask_layer
  | Layers of mask_layer list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of mask var

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

type background_shorthand = Properties.background_shorthand = {
  color : color option;
  image : background_image option;
  position : position_value option;
  size : background_size option;
  repeat : background_repeat option;
  attachment : background_attachment option;
  clip : background_box option;
  origin : background_box option;
}
(** CSS background shorthand values. *)

type background = Properties.background =
  | Inherit
  | Initial
  | Unset
  | None
  | Shorthand of background_shorthand  (** CSS background values. *)
  | Var of background var
  | Vars of background var list

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

(** {2:flexbox Flexbox Layout}

    Properties for CSS Flexible Box Layout, a one-dimensional layout method for
    distributing space between items and providing alignment capabilities.

    @see <https://www.w3.org/TR/css-flexbox-1/>
      CSS Flexible Box Layout Module Level 1 *)

(** CSS flex direction values. *)
type flex_direction = Properties.flex_direction =
  | Row
  | Row_reverse
  | Column
  | Column_reverse
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_direction var

(** CSS flex wrap values. *)
type flex_wrap = Properties.flex_wrap =
  | Nowrap
  | Wrap
  | Wrap_reverse
  | Balance
  | Wrap_reverse_balance
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_wrap var

type flex_flow = Properties.flex_flow =
  | Flow of flex_direction option * flex_wrap option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of flex_flow var

type flex_factor = Properties.flex_factor =
  | Number of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Calc of flex_factor calc
  | Var of flex_factor var

(** CSS flex basis values. *)
type flex_basis = Properties.flex_basis =
  | Auto
  | Content
  | Px of float
  | Cm of float
  | Mm of float
  | Q of float
  | In of float
  | Pt of float
  | Pc of float
  | Rem of float
  | Em of float
  | Ex of float
  | Cap of float
  | Ic of float
  | Ric of float
  | Rlh of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Vi of float
  | Vb of float
  | Dvh of float
  | Dvw of float
  | Dvmin of float
  | Dvmax of float
  | Lvh of float
  | Lvw of float
  | Lvmin of float
  | Lvmax of float
  | Svh of float
  | Svw of float
  | Svmin of float
  | Svmax of float
  | Ch of float
  | Lh of float
  | Num of float
  | Zero
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Fit_content
  | Fit_content_arg of length
  | Max_content
  | Min_content
  | Clamp of length * length * length
  | Min of length list
  | Max of length list
  | Round of string * length * length
  | Mod of length * length
  | Rem_fn of length * length
  | Hypot of length list
  | Abs of length
  | Dimension of { value : float; unit : string; repr : string }
  | Calc of flex_basis calc
  | Var of flex_basis var

(** CSS flex shorthand values. *)
type flex = Properties.flex =
  | Initial  (** 0 1 auto *)
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Auto  (** 1 1 auto *)
  | None  (** 0 0 auto *)
  | Grow of flex_factor  (** Single grow value *)
  | Basis of flex_basis  (** 1 1 <flex-basis> *)
  | Grow_shrink of flex_factor * flex_factor  (** grow shrink 0% *)
  | Full of flex_factor * flex_factor * flex_basis  (** grow shrink basis *)
  | Var of flex var

(** CSS font-size values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-size} MDN:
     font-size} *)
type font_size = Properties.font_size =
  | Length of length
  | Pct of float
  | Calc of font_size calc
  | Xx_small
  | X_small
  | Small
  | Medium
  | Large
  | X_large
  | Xx_large
  | Xxx_large
  | Larger
  | Smaller
  | Math
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_size var

(** {2:alignment_properties Alignment Properties}

    CSS Box Alignment properties for flexbox and grid layouts.

    @see <https://www.w3.org/TR/css-align-3/> CSS Box Alignment Module Level 3
*)

(** CSS align-content values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-content} MDN:
     align-content} *)
type align_content = Properties.align_content =
  | Normal
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Flex_start
  | Flex_end
  (* Safe content position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  (* Unsafe content position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of align_content var

(** CSS align-items values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-items} MDN:
     align-items} *)
type align_items = Properties.align_items =
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Anchor_center
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of align_items var

(** CSS justify-content values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content} MDN:
     justify-content} *)
type justify_content = Properties.justify_content =
  | Normal
  | Center
  | Start
  | End
  | Flex_start
  | Flex_end
  | Left
  | Right
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Space_between
  | Space_around
  | Space_evenly
  | Stretch
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of justify_content var

(** CSS align-self values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/align-self} MDN:
     align-self} *)
type align_self = Properties.align_self =
  | Auto
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_flex_start
  | Safe_flex_end
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of align_self var

(** CSS justify-items values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-items} MDN:
     justify-items} *)
type justify_items = Properties.justify_items =
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Left
  | Right
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_self_start
  | Safe_self_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Anchor_center
  | Legacy
  | Legacy_center
  | Legacy_left
  | Legacy_right
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of justify_items var

(** CSS justify-self values.
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/justify-self} MDN:
     justify-self} *)
type justify_self = Properties.justify_self =
  | Auto
  | Normal
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Center
  | Start
  | End
  | Self_start
  | Self_end
  | Flex_start
  | Flex_end
  | Left
  | Right
  (* Safe self position values *)
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_self_start
  | Safe_self_end
  | Safe_flex_start
  | Safe_flex_end
  | Safe_left
  | Safe_right
  (* Unsafe self position values *)
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_self_start
  | Unsafe_self_end
  | Unsafe_flex_start
  | Unsafe_flex_end
  | Unsafe_left
  | Unsafe_right
  | Anchor_center
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of justify_self var

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

(** CSS gap shorthand type. *)
type gap = Properties.gap =
  | Lengths of { row_gap : length option; column_gap : length option }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of gap var

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

(** [repeat()] count argument: an integer or [auto-fill] / [auto-fit] (CSS Grid
    1 sec. 7.2.3.1). *)
type repeat_count = Properties.repeat_count =
  | Count of int
  | Auto_fill
  | Auto_fit
  | Var of repeat_count var

(** One component in a [grid-auto-flow] value. *)
type grid_auto_flow_component = Properties.grid_auto_flow_component =
  | Axis of [ `Row | `Column ]
  | Dense
  | Var of grid_auto_flow_component var

(** CSS grid-auto-flow values *)
type grid_auto_flow = Properties.grid_auto_flow =
  | Row
  | Column
  | Dense
  | Row_dense
  | Column_dense
  | Components of grid_auto_flow_component list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of grid_auto_flow var

(** CSS grid template values *)
type grid_flex_math = Properties.grid_flex_math =
  | Calc_flex of math_arg
  | Min_flex of math_arg list
  | Max_flex of math_arg list
  | Clamp_flex of math_arg * math_arg * math_arg

type grid_template = Properties.grid_template =
  | None
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Vw of float
  | Vh of float
  | Vmin of float
  | Vmax of float
  | Zero
  | Length of length
  | Fr of float
  | Flex_math of grid_flex_math
  | Auto
  | Min_content
  | Max_content
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Min_max of grid_template * grid_template
  | Fit_content of length
  | Repeat of repeat_count * grid_template list
  | Tracks of grid_template list
  | Split of grid_template * grid_template
  | Auto_flow_columns of grid_template * grid_auto_flow * grid_template option
      (** [<grid-template-rows> / auto-flow [dense]? <grid-auto-columns>?]. *)
  | Auto_flow_rows of grid_auto_flow * grid_template option * grid_template
      (** [auto-flow [dense]? <grid-auto-rows>? / <grid-template-columns>]. *)
  | Named_tracks of (string option * grid_template) list
  | Line_names of string list
      (** [[col-start a b]] line-names block, kept as its own track-list element
          so the printer preserves the surrounding track positions. *)
  | Template of string
  | Subgrid
  | Masonry
  | Var of grid_template var

(** CSS grid-template-areas values *)
type grid_template_areas = Properties.grid_template_areas =
  | No_areas
  | Areas of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of grid_template_areas var

(** CSS grid line values *)
type grid_line = Properties.grid_line =
  | Auto  (** auto *)
  | Num of int  (** 1, 2, 3, ... or -1, -2, ... *)
  | Name of string  (** "header-start", "main-end", etc. *)
  | Num_name of int * string  (** <integer> <custom-ident> *)
  | Span of int  (** span 2, span 3, etc. *)
  | Span_name of string  (** span <custom-ident> *)
  | Span_num_name of int * string  (** span <integer> <custom-ident> *)
  | Calc of grid_line calc  (** calc(12 * -1), etc. *)
  | Var of grid_line var

type grid_line_pair = Properties.grid_line_pair =
  | Lines of grid_line * grid_line
  | Var of grid_line_pair var

type grid_area = Properties.grid_area =
  | Lines of {
      row_start : grid_line;
      column_start : grid_line;
      row_end : grid_line;
      column_end : grid_line;
    }
  | Var of grid_area var
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer

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

(** CSS place-items values *)
type place_items = Properties.place_items =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Baseline
  | First_baseline
  | Last_baseline
  | Start_safe
  | End_safe
  | Center_safe
  | Stretch_stretch  (** Explicit stretch on both axes. *)
  | Align_justify of align_items * justify_items
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of place_items var

val place_items : place_items -> declaration
(** [place_items items] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/place-items}
     place-items} shorthand property. *)

(** CSS place-content values *)
type place_content = Properties.place_content =
  | Normal
  | Start
  | End
  | Center
  | Stretch
  | Space_between
  | Space_around
  | Space_evenly
  | Safe_center
  | Safe_start
  | Safe_end
  | Safe_stretch
  | Unsafe_center
  | Unsafe_start
  | Unsafe_end
  | Unsafe_stretch
  | Align_justify of align_content * justify_content
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of place_content var

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

(** CSS font weight values. *)
type font_weight = Properties.font_weight =
  | Weight of float
  | Normal
  | Bold
  | Bolder
  | Lighter
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_weight var

(** CSS text align values. *)
type text_align = Properties.text_align =
  | Left
  | Right
  | Center
  | Justify
  | Start
  | End
  | Match_parent
  | Webkit_match_parent
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_align var

type text_decoration_line = Properties.text_decoration_line =
  | None
  | Underline
  | Overline
  | Line_through
  | Blink
  | Spelling_error
  | Grammar_error
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_line var

type text_decoration_style = Properties.text_decoration_style =
  | Solid
  | Double
  | Dotted
  | Dashed
  | Wavy
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_style var

type text_decoration_shorthand = Properties.text_decoration_shorthand = {
  lines : text_decoration_line list;
  style : text_decoration_style option;
  color : color option;
  thickness : length option;
}

(** CSS text decoration values. *)
type text_decoration = Properties.text_decoration =
  | None
  | Shorthand of text_decoration_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration var

type text_emphasis_fill = Properties.text_emphasis_fill = Filled | Open

type text_emphasis_shape = Properties.text_emphasis_shape =
  | Dot
  | Circle
  | Double_circle
  | Triangle
  | Sesame

type text_emphasis_style = Properties.text_emphasis_style =
  | None
  | Mark of text_emphasis_fill option * text_emphasis_shape option
  | String of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis_style var

type text_emphasis = Properties.text_emphasis =
  | Emphasis of text_emphasis_style option * color option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis var

type text_emphasis_line = Properties.text_emphasis_line = Over | Under
type text_emphasis_side = Properties.text_emphasis_side = Left | Right

type text_emphasis_position = Properties.text_emphasis_position =
  | Position of text_emphasis_line * text_emphasis_side option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis_position var

type text_orientation = Properties.text_orientation =
  | Mixed
  | Upright
  | Sideways
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_orientation var

type glyph_orientation_vertical = Properties.glyph_orientation_vertical =
  | Auto
  | Angle of angle
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of glyph_orientation_vertical var

type line_break = Properties.line_break =
  | Auto
  | Loose
  | Normal
  | Strict
  | Anywhere
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of line_break var

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

(** CSS font style values. *)
type font_style = Properties.font_style =
  | Normal
  | Italic
  | Oblique
  | Oblique_angle of angle
  | Oblique_range of angle * angle
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_style var

type text_transform_case = Properties.text_transform_case =
  | Capitalize
  | Uppercase
  | Lowercase

(** CSS text transform values. *)
type text_transform = Properties.text_transform =
  | None
  | Case of text_transform_case
  | Combo of {
      case : text_transform_case option;
      full_width : bool;
      full_size_kana : bool;
    }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_transform var

(** CSS text-size-adjust values (including vendor prefixes). *)
type text_size_adjust = Properties.text_size_adjust =
  | None
  | Auto
  | Pct of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_size_adjust var

(** CSS font-family values *)
type font_family = Properties.font_family =
  (* Generic CSS font families *)
  | Sans_serif
  | Serif
  | Monospace
  | Cursive
  | Fantasy
  | System_ui
  | Ui_sans_serif
  | Ui_serif
  | Ui_monospace
  | Ui_rounded
  | Emoji
  | Math
  | Fangsong
  (* Named families, for building a font stack in OCaml. CSS Fonts 4 sec. 2.1
     makes these [<custom-ident>]s rather than keywords, so the reader never
     produces one: an authored name is read as [Name] and printed back
     verbatim. Each constructor prints exactly as the [Name] carrying its
     spelling. *)
  (* Popular web fonts *)
  | Inter
  | Roboto
  | Open_sans
  | Lato
  | Montserrat
  | Poppins
  | Source_sans_pro
  | Raleway
  | Oswald
  | Noto_sans
  | Ubuntu
  | Playfair_display
  | Merriweather
  | Lora
  | PT_sans
  | PT_serif
  | Nunito
  | Nunito_sans
  | Work_sans
  | Rubik
  | Fira_sans
  | Fira_code
  | JetBrains_mono
  | IBM_plex_sans
  | IBM_plex_serif
  | IBM_plex_mono
  | Source_code_pro
  | Space_mono
  | DM_sans
  | DM_serif_display
  | Bebas_neue
  | Barlow
  | Mulish
  | Josefin_sans
  (* Platform-specific fonts *)
  | Helvetica
  | Helvetica_neue
  | Arial
  | Verdana
  | Tahoma
  | Trebuchet_ms
  | Times_new_roman
  | Times
  | Georgia
  | Cambria
  | Garamond
  | Courier_new
  | Courier
  | Lucida_console
  | SF_pro
  | SF_pro_display
  | SF_pro_text
  | SF_mono
  | NY
  | Segoe_ui
  | Segoe_ui_emoji
  | Segoe_ui_symbol
  | Apple_color_emoji
  | Noto_color_emoji
  | Android_emoji
  | Twemoji_mozilla
  (* Developer fonts *)
  | Menlo
  | Monaco
  | Consolas
  | Liberation_mono
  | SFMono_regular
  | Cascadia_code
  | Cascadia_mono
  | Victor_mono
  | Inconsolata
  | Hack
  (* CSS keywords *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  (* Custom font family name *)
  | Name of string
  (* CSS variables *)
  (* List of fonts for composition *)
  | List of font_family list
  | Var of font_family var
  | Invalid of invalid_value
      (** CSS-wide keyword mixed in a [<custom-ident>#] list, preserved verbatim
          and dropped by [Optimize.drop_invalid] on every serialisation. *)

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

(** CSS line-height values *)
type line_height = Properties.line_height =
  | Normal
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Num of float
  | Number of { value : float; unit : string option; repr : string }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Calc of line_height calc
  | Var of line_height var

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

type text_indent_value = Properties.text_indent_value =
  | Indent of { length : length_percentage; hanging : bool; each_line : bool }
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_indent_value var

val text_indent : text_indent_value -> declaration
(** [text_indent indent] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent}
     text-indent} property. *)

(** CSS white-space values *)
type white_space = Properties.white_space =
  | Normal
  | Nowrap
  | Pre
  | Pre_wrap
  | Pre_line
  | Break_spaces
  | Collapse
  | Preserve_nowrap
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of white_space var

val white_space : white_space -> declaration
(** [white_space space] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/white-space}
     white-space} property. *)

(** CSS word-break values *)
type word_break = Properties.word_break =
  | Normal
  | Break_all
  | Keep_all
  | Break_word
  | Auto_phrase
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of word_break var

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

(** CSS overflow-wrap values *)
type overflow_wrap = Properties.overflow_wrap =
  | Normal
  | Break_word
  | Anywhere
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_wrap var

val overflow_wrap : overflow_wrap -> declaration
(** [overflow_wrap wrap] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/overflow-wrap}
     overflow-wrap} property. *)

val line_break : line_break -> declaration
(** [line_break break] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/line-break} line-break}
    property. *)

(** CSS hyphens values *)
type hyphens = Properties.hyphens =
  | None
  | Manual
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of hyphens var

val hyphens : hyphens -> declaration
(** [hyphens hyphens] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/hyphens} hyphens}
    property. *)

(** CSS font-stretch values *)
type font_stretch = Properties.font_stretch =
  | Pct of float  (** Percentage values from 50% to 200% *)
  | Ultra_condensed
  | Extra_condensed
  | Condensed
  | Semi_condensed
  | Normal
  | Semi_expanded
  | Expanded
  | Extra_expanded
  | Ultra_expanded
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_stretch var

type font_variant_css21 = Properties.font_variant_css21 = Normal | Small_caps

type font_shorthand = Properties.font_shorthand = {
  style : font_style option;
  variant : font_variant_css21 option;
  weight : font_weight option;
  stretch : font_stretch option;
  size : font_size;
  line_height : line_height option;
  family : font_family;
}

type font = Properties.font =
  | Shorthand of font_shorthand
  | Caption
  | Icon
  | Menu
  | Message_box
  | Small_caption
  | Status_bar
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font var

val font_stretch : font_stretch -> declaration
(** [font_stretch stretch] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-stretch}
     font-stretch} property. *)

type font_optical_sizing = Properties.font_optical_sizing =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_optical_sizing var

val font_optical_sizing : font_optical_sizing -> declaration
(** [font_optical_sizing sizing] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-optical-sizing}
     font-optical-sizing} property. *)

type font_kerning = Properties.font_kerning =
  | Auto
  | Normal
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_kerning var

val font_kerning : font_kerning -> declaration
(** [font_kerning kerning] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-kerning}
     font-kerning} property. *)

type font_language_override = Properties.font_language_override =
  | Normal
  | String of string
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_language_override var

val font_language_override : font_language_override -> declaration
(** [font_language_override override] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-language-override}
     font-language-override} property. *)

type font_synthesis_style = Properties.font_synthesis_style =
  | Auto
  | None
  | Oblique_only
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_style var

val font_synthesis_style : font_synthesis_style -> declaration
(** [font_synthesis_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-style}
     font-synthesis-style} property. *)

type font_synthesis_weight = Properties.font_synthesis_weight =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_weight var

val font_synthesis_weight : font_synthesis_weight -> declaration
(** [font_synthesis_weight weight] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-weight}
     font-synthesis-weight} property. *)

type font_synthesis_small_caps = Properties.font_synthesis_small_caps =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_small_caps var

val font_synthesis_small_caps : font_synthesis_small_caps -> declaration
(** [font_synthesis_small_caps small_caps] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-small-caps}
     font-synthesis-small-caps} property. *)

type font_synthesis_position = Properties.font_synthesis_position =
  | Auto
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis_position var

val font_synthesis_position : font_synthesis_position -> declaration
(** [font_synthesis_position position] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-synthesis-position}
     font-synthesis-position} property. *)

type font_variant_ligature = Properties.font_variant_ligature =
  | Common_ligatures
  | No_common_ligatures
  | Discretionary_ligatures
  | No_discretionary_ligatures
  | Historical_ligatures
  | No_historical_ligatures
  | Contextual
  | No_contextual

type font_variant_ligatures = Properties.font_variant_ligatures =
  | Normal
  | None
  | Ligatures of font_variant_ligature list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_ligatures var

val font_variant_ligatures : font_variant_ligatures -> declaration
(** [font_variant_ligatures ligatures] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-ligatures}
     font-variant-ligatures} property. *)

type font_variant_caps = Properties.font_variant_caps =
  | Normal
  | Small_caps
  | All_small_caps
  | Petite_caps
  | All_petite_caps
  | Unicase
  | Titling_caps
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_caps var

val font_variant_caps : font_variant_caps -> declaration
(** [font_variant_caps caps] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-caps}
     font-variant-caps} property. *)

type font_variant_position = Properties.font_variant_position =
  | Normal
  | Sub
  | Super
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_position var

val font_variant_position : font_variant_position -> declaration
(** [font_variant_position position] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-position}
     font-variant-position} property. *)

type east_asian_feature = Properties.east_asian_feature =
  | Jis78
  | Jis83
  | Jis90
  | Jis04
  | Simplified
  | Traditional
  | Full_width
  | Proportional_width
  | Ruby

type font_variant_east_asian = Properties.font_variant_east_asian =
  | Normal
  | Features of east_asian_feature list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_east_asian var

val font_variant_east_asian : font_variant_east_asian -> declaration
(** [font_variant_east_asian east_asian] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variant-east-asian}
     font-variant-east-asian} property. *)

(** CSS font-size-adjust metric keywords *)
type font_size_adjust_metric = Properties.font_size_adjust_metric =
  | Ex_height
  | Cap_height
  | Ch_width
  | Ic_width
  | Ic_height

(** CSS font-size-adjust values *)
type font_size_adjust = Properties.font_size_adjust =
  | None
  | Number of float
  | From_font
  | Metric_number of font_size_adjust_metric * float
  | Metric_from_font of font_size_adjust_metric
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_size_adjust var

(** CSS font-variant-emoji values *)
type font_variant_emoji = Properties.font_variant_emoji =
  | Normal
  | Text
  | Emoji
  | Unicode
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_emoji var

(** CSS font-variant-numeric token values *)
type font_variant_numeric_token = Properties.font_variant_numeric_token =
  | Normal  (** Reset to normal font variant *)
  | Lining_nums
  | Oldstyle_nums
  | Proportional_nums
  | Tabular_nums
  | Diagonal_fractions
  | Stacked_fractions
  | Ordinal
  | Slashed_zero
  | Var of font_variant_numeric_token var
      (** CSS font-variant-numeric values *)

type font_variant_numeric = Properties.font_variant_numeric =
  | Normal
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Tokens of font_variant_numeric_token list
  | Composed of {
      ordinal : font_variant_numeric_token option;
      slashed_zero : font_variant_numeric_token option;
      numeric_figure : font_variant_numeric_token option;
      numeric_spacing : font_variant_numeric_token option;
      numeric_fraction : font_variant_numeric_token option;
    }
  | Var of font_variant_numeric var

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

(** CSS shadow values *)
type shadow_body = Properties.shadow_body = {
  h_offset : length;
  v_offset : length;
  blur : length option;
  spread : length option;
  color : color option;
}
(** The [<length>{2,4} && <color>?] part of a single [<shadow>]. *)

and inset = Properties.inset =
  | Var of shadow var  (** [inset var(--x)]: the whole body from one var. *)
  | Body of shadow_body  (** [inset 2px 4px red]: a concrete inset body. *)
  | Toggle of { name : string; no_fallback : bool; body : shadow_body }
      (** [var(--name) <body>]: a dynamic inset toggle (Tailwind's ring system).
      *)

and shadow = Properties.shadow =
  | Shadow of shadow_body  (** A non-inset shadow. *)
  | Inset of inset  (** An inset shadow. *)
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | List of shadow list
  | Var of shadow var

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

(** CSS text-shadow values *)
type text_shadow = Properties.text_shadow =
  | None
  | Text_shadow of {
      h_offset : length;
      v_offset : length;
      blur : length option;
      color : color option;
    }
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of text_shadow var

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

(** CSS direction values *)
type direction = Properties.direction =
  | Ltr
  | Rtl
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of direction var

val direction : direction -> declaration
(** [direction dir] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/direction} direction}
    property. *)

(** CSS unicode-bidi values *)
type unicode_bidi = Properties.unicode_bidi =
  | Normal
  | Embed
  | Isolate
  | Bidi_override
  | Isolate_override
  | Plaintext
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of unicode_bidi var

val unicode_bidi : unicode_bidi -> declaration
(** [unicode_bidi bidi] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/unicode-bidi}
     unicode-bidi} property. *)

(** CSS writing-mode values *)
type writing_mode = Properties.writing_mode =
  | Horizontal_tb
  | Vertical_rl
  | Vertical_lr
  | Sideways_lr
  | Sideways_rl
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of writing_mode var

type text_combine_upright = Properties.text_combine_upright =
  | None
  | All
  | Digits of int option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_combine_upright var

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

(** CSS text-decoration-skip-ink values *)
type text_decoration_skip_ink = Properties.text_decoration_skip_ink =
  | Auto
  | None
  | All
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_ink var

val text_decoration_skip_ink : text_decoration_skip_ink -> declaration
(** [text_decoration_skip_ink skip] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/text-decoration-skip-ink}
     text-decoration-skip-ink} property. *)

(** CSS Text Decoration 4
    {{:https://drafts.csswg.org/css-text-decor-4/#propdef-text-decoration-skip}
     [text-decoration-skip]}: the shorthand over the four longhands below. *)
type text_decoration_skip = Properties.text_decoration_skip =
  | None
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip var

val text_decoration_skip : text_decoration_skip -> declaration
(** [text_decoration_skip v] is the [text-decoration-skip] shorthand. *)

(** Sec. 2.5.1 [text-decoration-skip-self]: whether the box's own decoration
    skips it. *)
type text_decoration_skip_self = Properties.text_decoration_skip_self =
  | None
  | Objects
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_self var

val text_decoration_skip_self : text_decoration_skip_self -> declaration
(** [text_decoration_skip_self v] is the [text-decoration-skip-self] property.
*)

(** Sec. 2.5.2 [text-decoration-skip-box]: whether an ancestor's decoration
    skips the box's edges. *)
type text_decoration_skip_box = Properties.text_decoration_skip_box =
  | All
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_box var

val text_decoration_skip_box : text_decoration_skip_box -> declaration
(** [text_decoration_skip_box v] is the [text-decoration-skip-box] property. *)

(** Sec. 2.5.3 [text-decoration-skip-inset]: whether the decoration is inset
    from the glyph edges. *)
type text_decoration_skip_inset = Properties.text_decoration_skip_inset =
  | None
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_inset var

val text_decoration_skip_inset : text_decoration_skip_inset -> declaration
(** [text_decoration_skip_inset v] is the [text-decoration-skip-inset] property.
*)

(** Sec. 2.5.4: one span of spaces a decoration skips. *)
type text_decoration_skip_space = Properties.text_decoration_skip_space =
  | All
  | Start
  | End

(** Sec. 2.5.4 [text-decoration-skip-spaces]. *)
type text_decoration_skip_spaces = Properties.text_decoration_skip_spaces =
  | Spaces of text_decoration_skip_space list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_decoration_skip_spaces var

val text_decoration_skip_spaces : text_decoration_skip_spaces -> declaration
(** [text_decoration_skip_spaces v] is the [text-decoration-skip-spaces]
    property. *)

(** One class of character the emphasis marks skip, for CSS Text Decoration 4
    {{:https://drafts.csswg.org/css-text-decor-4/#propdef-text-emphasis-skip}
     [text-emphasis-skip]}. *)
type text_emphasis_skip_keyword = Properties.text_emphasis_skip_keyword =
  | Spaces
  | Punctuation
  | Symbols
  | Narrow

(** Sec. 4.3 [text-emphasis-skip]. *)
type text_emphasis_skip = Properties.text_emphasis_skip =
  | Skip of text_emphasis_skip_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_emphasis_skip var

val text_emphasis_skip : text_emphasis_skip -> declaration
(** [text_emphasis_skip v] is the [text-emphasis-skip] property. *)

(** CSS Text 4
    {{:https://drafts.csswg.org/css-text-4/#propdef-white-space-collapse}
     [white-space-collapse]}: how white space and line breaks collapse. *)
type white_space_collapse = Properties.white_space_collapse =
  | Collapse
  | Discard
  | Preserve
  | Preserve_breaks
  | Preserve_spaces
  | Break_spaces
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of white_space_collapse var

val white_space_collapse : white_space_collapse -> declaration
(** [white_space_collapse v] is the [white-space-collapse] property. *)

val line_height_step : length -> declaration
(** [line_height_step v] is the [line-height-step] property. *)

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-palette}
     [font-palette]}. *)
type font_palette = Properties.font_palette =
  | Normal
  | Light
  | Dark
  | Palette of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of font_palette var

val font_palette : font_palette -> declaration
(** [font_palette v] is the [font-palette] property. *)

(** One face the browser may synthesise, for CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-synthesis}
     [font-synthesis]}. *)
type font_synthesis_feature = Properties.font_synthesis_feature =
  | Weight
  | Style
  | Small_caps
  | Position

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-synthesis}
     [font-synthesis]}. *)
type font_synthesis = Properties.font_synthesis =
  | None
  | Features of font_synthesis_feature list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of font_synthesis var

val font_synthesis : font_synthesis -> declaration
(** [font_synthesis v] is the [font-synthesis] shorthand. *)

val font_size_adjust : font_size_adjust -> declaration
(** [font_size_adjust v] is the [font-size-adjust] property. *)

val font_variant_emoji : font_variant_emoji -> declaration
(** [font_variant_emoji v] is the [font-variant-emoji] property. *)

(** One feature of CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant-alternates}
     [font-variant-alternates]}. *)
type font_variant_alternates_item = Properties.font_variant_alternates_item =
  | Stylistic of string
  | Historical_forms
  | Styleset of string list
  | Character_variant of string list
  | Swash of string
  | Ornaments of string
  | Annotation of string

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant-alternates}
     [font-variant-alternates]}. *)
type font_variant_alternates = Properties.font_variant_alternates =
  | Normal
  | Alternates of font_variant_alternates_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant_alternates var

val font_variant_alternates : font_variant_alternates -> declaration
(** [font_variant_alternates v] is the [font-variant-alternates] property. *)

type font_variant_shorthand = Properties.font_variant_shorthand = {
  ligatures : font_variant_ligature list;
  alternates : font_variant_alternates_item list;
  caps : font_variant_caps option;
  numeric : font_variant_numeric_token list;
  east_asian : east_asian_feature list;
  position : font_variant_position option;
  emoji : font_variant_emoji option;
}
(** The slots of the CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant}
     [font-variant]} shorthand. *)

(** CSS Fonts 4
    {{:https://drafts.csswg.org/css-fonts-4/#propdef-font-variant}
     [font-variant]}. *)
type font_variant = Properties.font_variant =
  | Normal
  | None
  | Shorthand of font_variant_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of font_variant var

val font_variant : font_variant -> declaration
(** [font_variant v] is the [font-variant] shorthand. *)

val text_wrap_style : text_wrap_style -> declaration
(** [text_wrap_style v] is the [text-wrap-style] property. *)

val text_box_trim : text_box_trim -> declaration
(** [text_box_trim v] is the [text-box-trim] property. *)

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-text-box} [text-box]}:
    [normal | <'text-box-trim'> || <'text-box-edge'>]. *)
type text_box = Properties.text_box =
  | Normal
  | Box of text_box_trim option * text_box_edge option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of text_box var

val text_box : text_box -> declaration
(** [text_box v] is the [text-box] shorthand. *)

val text_spacing_trim : text_spacing_trim -> declaration
(** [text_spacing_trim v] is the [text-spacing-trim] property. *)

val hyphenate_limit_chars : hyphenate_limit_chars -> declaration
(** [hyphenate_limit_chars v] is the [hyphenate-limit-chars] property. *)

val initial_letter : initial_letter -> declaration
(** [initial_letter v] is the [initial-letter] property. *)

(** One alignment point of CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-initial-letter-align}
     [initial-letter-align]}. *)
type initial_letter_align_keyword = Properties.initial_letter_align_keyword =
  | Alphabetic
  | Ideographic
  | Hanging
  | Leading
  | Border_box

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-initial-letter-align}
     [initial-letter-align]}. *)
type initial_letter_align = Properties.initial_letter_align =
  | Align of initial_letter_align_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter_align var

val initial_letter_align : initial_letter_align -> declaration
(** [initial_letter_align v] is the [initial-letter-align] property. *)

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-initial-letter-wrap}
     [initial-letter-wrap]}. *)
type initial_letter_wrap = Properties.initial_letter_wrap =
  | None
  | First
  | All
  | Grid
  | Length of length_percentage
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of initial_letter_wrap var

val initial_letter_wrap : initial_letter_wrap -> declaration
(** [initial_letter_wrap v] is the [initial-letter-wrap] property. *)

(** CSS Shapes 1
    {{:https://drafts.csswg.org/css-shapes-1/#propdef-shape-image-threshold}
     [shape-image-threshold]}: the alpha above which a pixel of the shape image
    is inside the shape. *)
type shape_image_threshold = Properties.shape_image_threshold =
  | Number of float
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of shape_image_threshold var

val shape_image_threshold : shape_image_threshold -> declaration
(** [shape_image_threshold v] is the [shape-image-threshold] property. *)

val shape_margin : length_percentage -> declaration
(** [shape_margin v] is the [shape-margin] property. *)

val shape_outside : string -> declaration
(** [shape_outside v] is the [shape-outside] property, held as the authored text
    of its shape. *)

(** CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#typedef-visual-box} [<visual-box>]}:
    the box edge an overflow clip margin is measured from. *)
type overflow_clip_box = Properties.overflow_clip_box =
  | Content_box
  | Padding_box
  | Border_box

(** CSS Overflow 4
    {{:https://drafts.csswg.org/css-overflow-4/#propdef-overflow-clip-margin}
     [overflow-clip-margin]}: [<visual-box> || <length>]. *)
type overflow_clip_margin = Properties.overflow_clip_margin =
  | Clip_margin of overflow_clip_box option * length option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_clip_margin var

val overflow_clip_margin : overflow_clip_margin -> declaration
(** [overflow_clip_margin v] is the [overflow-clip-margin] property. *)

(** CSS Scroll Anchoring 1
    {{:https://drafts.csswg.org/css-scroll-anchoring-1/#propdef-overflow-anchor}
     [overflow-anchor]}. *)
type overflow_anchor = Properties.overflow_anchor =
  | Auto
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overflow_anchor var

val overflow_anchor : overflow_anchor -> declaration
(** [overflow_anchor v] is the [overflow-anchor] property. *)

val overflow_block : overflow -> declaration
(** [overflow_block v] is the [overflow-block] property. *)

val overflow_inline : overflow -> declaration
(** [overflow_inline v] is the [overflow-inline] property. *)

(** CSS Images 3
    {{:https://drafts.csswg.org/css-images-3/#propdef-image-orientation}
     [image-orientation]}. *)
type image_orientation = Properties.image_orientation =
  | None
  | From_image
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_orientation var

val image_orientation : image_orientation -> declaration
(** [image_orientation v] is the [image-orientation] property. *)

(** CSS Images 3
    {{:https://drafts.csswg.org/css-images-3/#propdef-image-rendering}
     [image-rendering]}. *)
type image_rendering = Properties.image_rendering =
  | Auto
  | Smooth
  | High_quality
  | Crisp_edges
  | Pixelated
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_rendering var

val image_rendering : image_rendering -> declaration
(** [image_rendering v] is the [image-rendering] property. *)

(** CSS Values 4
    {{:https://drafts.csswg.org/css-values-4/#resolution} [<resolution>]}. *)
type resolution = Properties.resolution =
  | Dpi of float
  | Dpcm of float
  | Dppx of float
  | X of float

(** CSS Images 4
    {{:https://drafts.csswg.org/css-images-4/#propdef-image-resolution}
     [image-resolution]}: [[ from-image || <resolution> ] && snap?]. *)
type image_resolution = Properties.image_resolution =
  | Resolution of resolution
  | From_image
  | From_image_resolution of resolution
  | Snap of resolution
  | From_image_snap
  | From_image_snap_resolution of resolution
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of image_resolution var

val image_resolution : image_resolution -> declaration
(** [image_resolution v] is the [image-resolution] property. *)

(** One axis whose margins CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#propdef-margin-trim} [margin-trim]}
    trims. *)
type margin_trim_axis = Properties.margin_trim_axis = Block | Inline

(** One edge whose margin CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#propdef-margin-trim} [margin-trim]}
    trims. *)
type margin_trim_edge = Properties.margin_trim_edge =
  | Block_start
  | Inline_start
  | Block_end
  | Inline_end

(** CSS Box 4
    {{:https://drafts.csswg.org/css-box-4/#propdef-margin-trim} [margin-trim]}.
*)
type margin_trim = Properties.margin_trim =
  | None
  | Block
  | Inline
  | Axes of margin_trim_axis list
  | Edges of margin_trim_edge list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of margin_trim var

val margin_trim : margin_trim -> declaration
(** [margin_trim v] is the [margin-trim] property. *)

(** CSS Positioned Layout 4
    {{:https://drafts.csswg.org/css-position-4/#propdef-overlay} [overlay]}:
    whether the box is in the top layer. *)
type overlay = Properties.overlay =
  | Auto
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of overlay var

val overlay : overlay -> declaration
(** [overlay v] is the [overlay] property. *)

(** How one animation composes with the value beneath it, for CSS Animations 2
    {{:https://drafts.csswg.org/css-animations-2/#propdef-animation-composition}
     [animation-composition]}. *)
type animation_composition_item = Properties.animation_composition_item =
  | Replace
  | Add
  | Accumulate

(** CSS Animations 2
    {{:https://drafts.csswg.org/css-animations-2/#propdef-animation-composition}
     [animation-composition]}. *)
type animation_composition = Properties.animation_composition =
  | Compositions of animation_composition_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_composition var

val animation_composition : animation_composition -> declaration
(** [animation_composition v] is the [animation-composition] property. *)

(** One physical edge a [<position>] offsets from, for CSS Backgrounds 4
    {{:https://drafts.csswg.org/css-backgrounds-4/#propdef-background-position-x}
     [background-position-x]}. *)
type position_axis_edge = Properties.position_axis_edge =
  | Left
  | Right
  | Top
  | Bottom

(** One axis of CSS Backgrounds 4
    {{:https://drafts.csswg.org/css-backgrounds-4/#propdef-background-position-x}
     [background-position-x]}. *)
type background_position_axis = Properties.background_position_axis =
  | Center
  | Edge of position_axis_edge
  | Offset of length_percentage
  | Edge_offset of position_axis_edge * length_percentage
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of background_position_axis var

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

type webkit_text_stroke = Properties.webkit_text_stroke = {
  width : border_width option;
  color : color option;
}
(** {{:https://developer.mozilla.org/en-US/docs/Web/CSS/-webkit-text-stroke}
     [-webkit-text-stroke]}: a width and a colour, either of which may be
    absent. No CSS specification defines it. *)

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

(** CSS border style values. *)
type border_style = Properties.border_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Hidden
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_style var

type border_shorthand = Properties.border_shorthand = {
  width : border_width option;
  style : border_style option;
  color : color option;
}
(** CSS border shorthand type. *)

(** CSS border property values. *)
type border = Properties.border =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of border_shorthand
  | Var of border var

type logical_border_color = Properties.logical_border_color =
  | Single of color
  | Pair of color * color
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of logical_border_color var

val logical_border_color : color -> logical_border_color
(** [logical_border_color color] is a one-value logical border color. *)

val logical_border_colors : color -> color -> logical_border_color
(** [logical_border_colors start end_] is a two-value logical border color. *)

type logical_border_width = Properties.logical_border_width =
  | Single of border_width
  | Pair of border_width * border_width
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of logical_border_width var

val logical_border_width : border_width -> logical_border_width
(** [logical_border_width w] is a one-value logical border width. *)

val logical_border_widths : border_width -> border_width -> logical_border_width
(** [logical_border_widths start end_] is a two-value logical border width. *)

type logical_border_style = Properties.logical_border_style =
  | Single of border_style
  | Pair of border_style * border_style
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of logical_border_style var

val logical_border_style : border_style -> logical_border_style
(** [logical_border_style s] is a one-value logical border style. *)

val logical_border_styles : border_style -> border_style -> logical_border_style
(** [logical_border_styles start end_] is a two-value logical border style. *)

(** CSS outline style values. *)
type outline_style = Properties.outline_style =
  | None
  | Solid
  | Dashed
  | Dotted
  | Double
  | Groove
  | Ridge
  | Inset
  | Outset
  | Auto
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of outline_style var

type outline_shorthand = Properties.outline_shorthand = {
  width : border_width option;
  style : outline_style option;
  color : color option;
}
(** CSS outline shorthand components. *)

(** CSS outline property values. *)
type outline = Properties.outline =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of outline_shorthand
  | Var of outline var

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

val column_rule : border -> declaration
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

(** One offset of CSS Backgrounds 3
    {{:https://drafts.csswg.org/css-backgrounds-3/#propdef-border-image-slice}
     [border-image-slice]}, a number in units of the image's own pixels or a
    percentage of its size. *)
type border_image_slice_item = Properties.border_image_slice_item =
  | Number of number
  | Pct of float

type border_image_slice_offsets = Properties.border_image_slice_offsets = {
  offsets : border_image_slice_item list;
  fill : bool;
}
(** Sec. 5.2: the one to four offsets and the [fill] keyword. *)

(** Sec. 5.2 [border-image-slice]. *)
type border_image_slice = Properties.border_image_slice =
  | Slices of border_image_slice_offsets
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_slice var

val border_image_slice : border_image_slice -> declaration
(** [border_image_slice v] is the [border-image-slice] property. *)

(** Sec. 5.3: one [border-image-width], which unlike a border width takes a bare
    number as a multiple of the border width. *)
type border_image_width_item = Properties.border_image_width_item =
  | Number of number
  | Pct of float
  | Length of length
  | Auto

(** Sec. 5.3 [border-image-width]. *)
type border_image_width = Properties.border_image_width =
  | Widths of border_image_width_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_width var

val border_image_width : border_image_width -> declaration
(** [border_image_width v] is the [border-image-width] property. *)

(** Sec. 5.4: one [border-image-outset], a number or a length. *)
type border_image_outset_item = Properties.border_image_outset_item =
  | Number of number
  | Length of length

(** Sec. 5.4 [border-image-outset]. *)
type border_image_outset = Properties.border_image_outset =
  | Outsets of border_image_outset_item list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_outset var

val border_image_outset : border_image_outset -> declaration
(** [border_image_outset v] is the [border-image-outset] property. *)

(** Sec. 5.5: how the middle of an edge is filled. *)
type border_image_repeat_keyword = Properties.border_image_repeat_keyword =
  | Stretch
  | Repeat
  | Round
  | Space

(** Sec. 5.5 [border-image-repeat]: the block edge then the inline edge. *)
type border_image_repeat = Properties.border_image_repeat =
  | Repeats of border_image_repeat_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_image_repeat var

val border_image_repeat : border_image_repeat -> declaration
(** [border_image_repeat v] is the [border-image-repeat] property. *)

val border_image_source : background_image -> declaration
(** [border_image_source v] is the [border-image-source] property. *)

(** CSS Masking 1
    {{:https://drafts.csswg.org/css-masking-1/#propdef-mask-border-mode}
     [mask-border-mode]}: which channel of the source image is the mask. *)
type mask_border_mode = Properties.mask_border_mode = Alpha | Luminance

type border_image = Properties.border_image = {
  source : background_image option;
  slice : border_image_slice_offsets option;
  width : border_image_width_item list option;
  outset : border_image_outset_item list option;
  repeat : border_image_repeat_keyword list option;
  mode : mask_border_mode option;
}
(** CSS Backgrounds 3
    {{:https://drafts.csswg.org/css-backgrounds-3/#propdef-border-image}
     [border-image]} and CSS Masking 1
    {{:https://drafts.csswg.org/css-masking-1/#propdef-mask-border}
     [mask-border]}, which share every slot but the [mode] only the mask
    carries. *)

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

(** CSS border-collapse values *)
type border_collapse = Properties.border_collapse =
  | Collapse
  | Separate
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of border_collapse var

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

(** CSS transform values *)
type transform = Properties.transform =
  | Translate of length * length option
  | Translate_x of length
  | Translate_y of length
  | Translate_z of length
  | Translate_3d of length * length * length
  | Rotate of angle
  | Rotate_x of angle
  | Rotate_y of angle
  | Rotate_z of angle
  | Rotate_3d of float * float * float * angle
  | Rotate_axis of float * float * float * angle
  | Scale of number_percentage * number_percentage option
  | Scale_space of number_percentage * number_percentage
  | Scale_x of number_percentage
  | Scale_y of number_percentage
  | Scale_z of number_percentage
  | Scale_3d of number_percentage * number_percentage * number_percentage
  | Skew of angle * angle option
  | Skew_x of angle
  | Skew_y of angle
  | Matrix of float * float * float * float * float * float
  | Matrix_3d of
      (float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float
      * float)
  | Perspective of length
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | List of transform list
  | Var of transform var

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

type transform_origin = Properties.transform_origin =
  | Center
  | Center_center
  | Left
  | Right
  | Top
  | Bottom
  | Left_top
  | Left_center
  | Left_bottom
  | Right_top
  | Right_center
  | Right_bottom
  | Center_top
  | Center_bottom
  | Top_left
  | Top_right
  | Bottom_left
  | Bottom_right
  | Position of position_value
  | X of length  (** Single x-offset, y defaults to 50%. *)
  | XY of length * length
  | XYZ of length * length * length
  | Position_z of position_value * length
  | Initial
  | Inherit  (** Transform origin (2D or 3D). *)
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_origin var

val origin : length -> length -> transform_origin
(** [origin x y] is a transform-origin helper for 2D positions. *)

val origin3d : length -> length -> length -> transform_origin
(** [origin3d x y z] is a transform-origin helper for 3D positions. *)

val transform_origin : transform_origin -> declaration
(** [transform_origin origin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-origin}
     transform-origin} property. *)

(** CSS transform-box property values *)
type transform_box = Properties.transform_box =
  | Content_box
  | Border_box
  | Fill_box
  | Stroke_box
  | View_box
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_box var

val transform_box : transform_box -> declaration
(** [transform_box value] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-box}
     transform-box} property. *)

(** CSS rotate property values *)
type rotate_value = Properties.rotate_value =
  | Angle of angle  (** z-axis rotation *)
  | X of angle  (** x-axis rotation *)
  | Y of angle  (** y-axis rotation *)
  | Z of angle  (** z-axis rotation (explicit) *)
  | Axis of float * float * float * angle  (** custom axis rotation *)
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of rotate_value var

val rotate : rotate_value -> declaration
(** [rotate v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/rotate} rotate}
    property. *)

val perspective : length -> declaration
(** [perspective perspective] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective}
     perspective} property (3D transforms). *)

type perspective_origin = position_value
(** CSS perspective-origin values for 3D transforms. *)

val perspective_origin : perspective_origin -> declaration
(** [perspective_origin origin] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/perspective-origin}
     perspective-origin} property. *)

(** CSS transform-style values *)
type transform_style = Properties.transform_style =
  | Flat
  | Preserve_3d
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transform_style var

val transform_style : transform_style -> declaration
(** [transform_style style] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/transform-style}
     transform-style} property (3D transforms). *)

(** CSS steps direction values. *)
type steps_direction = Properties.steps_direction =
  | Jump_start
  | Jump_end
  | Jump_none
  | Jump_both
  | Start
  | End
  | Var of steps_direction var

(** CSS animation timing function values. *)
type timing_function = Properties.timing_function =
  | Ease
  | Linear
  | Ease_in
  | Ease_out
  | Ease_in_out
  | Step_start
  | Step_end
  | Steps of int * steps_direction option
  | Cubic_bezier of float * float * float * float
  | Linear_function of string
  | Timing_functions of timing_function list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of timing_function var

(** CSS duration values. *)
type duration = Values.duration =
  | Ms of float  (** milliseconds *)
  | S of float  (** seconds *)
  | Auto  (** [animation-duration] only *)
  | Durations of duration list  (** comma-separated list of durations *)
  | Round of string * duration * duration
  | Mod of duration * duration
  | Rem of duration * duration
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of duration var  (** CSS variable reference *)
  | Calc of duration calc

(** CSS transition property value. *)
type transition_property_value = Properties.transition_property_value =
  | All
  | None
  | Property of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of transition_property_value var

type transition_property = transition_property_value list
(** CSS transition property (list of property values). *)

(** CSS transition-behavior values (Transitions Level 2). *)
type transition_behavior = Properties.transition_behavior =
  | Normal
  | Allow_discrete
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of transition_behavior var

type transition_shorthand = Properties.transition_shorthand = {
  property : transition_property_value;
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  behavior : transition_behavior option;
}
(** CSS transition shorthand values. *)

type transition = Properties.transition =
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | None
  | Shorthand of transition_shorthand  (** CSS transition values. *)
  | Var of transition var

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

(** CSS animation fill mode values *)
type animation_fill_mode = Properties.animation_fill_mode =
  | None
  | Forwards
  | Backwards
  | Both
  | Fill_modes of animation_fill_mode list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_fill_mode var

(** CSS animation direction values *)
type animation_direction = Properties.animation_direction =
  | Normal
  | Reverse
  | Alternate
  | Alternate_reverse
  | Directions of animation_direction list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_direction var

(** CSS animation play state values *)
type animation_play_state = Properties.animation_play_state =
  | Running
  | Paused
  | States of animation_play_state list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_play_state var

(** CSS animation iteration count values *)
type animation_iteration_count = Properties.animation_iteration_count =
  | Count of number
  | Infinite
  | Counts of animation_iteration_count list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_iteration_count var

type animation_name = Properties.animation_name =
  | None
  | Name of string
  | Ambiguous of string
  | Quoted of string
  | Names of animation_name list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_name var

type animation_shorthand = Properties.animation_shorthand = {
  name : animation_name option; (* Optional animation name, defaults to None *)
  duration : duration option;
  timing_function : timing_function option;
  delay : duration option;
  iteration_count : animation_iteration_count option;
  direction : animation_direction option;
  fill_mode : animation_fill_mode option;
  play_state : animation_play_state option;
  timeline : animation_timeline option;
}
(** CSS animation shorthand values *)

and animation_timeline = Properties.animation_timeline =
  | None
  | Auto
  | Name of string
  | Scroll of string
  | View of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_timeline var

type animation = Properties.animation =
  | Inherit
  | Initial
  | None
  | Shorthand of animation_shorthand
  | Var of animation var

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

(** CSS scale property values *)
type scale = Properties.scale =
  | X of number_percentage
  | XY of number_percentage * number_percentage
  | XYZ of number_percentage * number_percentage * number_percentage
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scale var

val scale : scale -> declaration
(** [scale scale] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scale} scale} property.
*)

type translate_value = Properties.translate_value =
  | X of length
  | XY of length * length
  | XYZ of length * length * length
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of translate_value var

val translate : translate_value -> declaration
(** [translate v] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/translate} translate}
    property. *)

(** Filter functions with an optional argument. *)
type filter_function = Properties.filter_function =
  | Blur_function
  | Brightness_function
  | Contrast_function
  | Grayscale_function
  | Hue_rotate_function
  | Invert_function
  | Opacity_function
  | Saturate_function
  | Sepia_function

(** CSS filter values *)
type filter = Properties.filter =
  | None  (** No filter *)
  | Omitted of filter_function  (** Function with its argument omitted. *)
  | Blur of length  (** blur(px) *)
  | Brightness of number_percentage  (** brightness(%) *)
  | Contrast of number_percentage  (** contrast(%) *)
  | Drop_shadow of shadow  (** drop-shadow(...) *)
  | Grayscale of number_percentage  (** grayscale(%) *)
  | Hue_rotate of angle  (** hue-rotate(deg) *)
  | Invert of number_percentage  (** invert(%) *)
  | Opacity of number_percentage  (** opacity(%) *)
  | Saturate of number_percentage  (** saturate(%) *)
  | Sepia of number_percentage  (** sepia(%) *)
  | Url of string  (** url(...) *)
  | List of filter list  (** Multiple filters *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of filter var

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

(** CSS clip property values (deprecated, but needed for sr-only). *)
type clip = Properties.clip =
  | Clip_auto
  | Clip_rect of length * length * length * length
      (** top, right, bottom, left *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of clip var

type clip_geometry_box = Properties.clip_geometry_box =
  | Margin_box
  | Border_box
  | Padding_box
  | Content_box
  | Fill_box
  | Stroke_box
  | View_box

type clip_path_extent = Properties.clip_path_extent =
  | Extent_length of length
  | Closest_side
  | Farthest_side

type clip_path_fill_rule = Properties.clip_path_fill_rule = Nonzero | Evenodd

(** CSS clip-path property values for clipping regions. *)
type clip_path = Properties.clip_path =
  | Clip_path_none
  | Clip_path_url of string
  | Clip_path_inset of {
      top : length_percentage;
      right : length_percentage option;
      bottom : length_percentage option;
      left : length_percentage option;
      rounded : border_radius option;
    }
  | Clip_path_circle of {
      radius : clip_path_extent option;
      position : position_value option;
    }
  | Clip_path_ellipse of {
      rx : clip_path_extent option;
      ry : clip_path_extent option;
      position : position_value option;
    }
  | Clip_path_polygon of {
      fill_rule : clip_path_fill_rule option;
      points : (length * length) list;
      spaced : bool;
    }
  | Clip_path_path of string
  | Clip_path_shape of string
  | Clip_path_box of clip_geometry_box
  | Clip_path_with_box of {
      shape : clip_path;
      box : clip_geometry_box;
      box_first : bool;
    }
  | Clip_path_xywh of {
      x : length_percentage;
      y : length_percentage;
      width : length_percentage;
      height : length_percentage;
      rounded : border_radius option;
    }
      (** [xywh(<length-percentage>{4} [round <border-radius>]?)] - CSS Shapes
          2. *)
  | Clip_path_rect of {
      top : length_percentage;
      right : length_percentage;
      bottom : length_percentage;
      left : length_percentage;
      rounded : border_radius option;
    }
      (** [rect(<length-percentage>{4} [round <border-radius>]?)] - CSS Shapes
          2. *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of clip_path var
  | Invalid of invalid_value
      (** Spec-invalid [<basic-shape>] preserved verbatim. *)

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

val webkit_mask_clip : mask_box -> declaration
(** [webkit_mask_clip v] is the [-webkit-mask-clip] property. *)

val mask_clip : mask_box -> declaration
(** [mask_clip v] is the [mask-clip] property. *)

val webkit_mask_origin : mask_box -> declaration
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

(** CSS cursor values. *)
type cursor = Properties.cursor =
  | Auto
  | Default
  | None
  | Context_menu
  | Help
  | Pointer
  | Progress
  | Wait
  | Cell
  | Crosshair
  | Text
  | Vertical_text
  | Alias
  | Copy
  | Move
  | No_drop
  | Not_allowed
  | Grab
  | Grabbing
  | E_resize
  | N_resize
  | Ne_resize
  | Nw_resize
  | S_resize
  | Se_resize
  | Sw_resize
  | W_resize
  | Ew_resize
  | Ns_resize
  | Nesw_resize
  | Nwse_resize
  | Col_resize
  | Row_resize
  | All_scroll
  | Zoom_in
  | Zoom_out
  | Url of string * (float * float) option * cursor
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of cursor var

val cursor_url : ?hotspot:float * float -> fallback:cursor -> string -> cursor
(** [cursor_url ?hotspot ~fallback url] is a URL cursor with its required
    fallback. *)

(** CSS user-select values. *)
type user_select = Properties.user_select =
  | None
  | Auto
  | Text
  | All
  | Contain
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of user_select var

(** CSS resize values. *)
type resize = Properties.resize =
  | None
  | Both
  | Horizontal
  | Vertical
  | Block
  | Inline
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of resize var

(** CSS print-color-adjust values. *)
type print_color_adjust = Properties.print_color_adjust =
  | Economy
  | Exact
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of print_color_adjust var

val cursor : cursor -> declaration
(** [cursor cursor] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/cursor} cursor}
    property. *)

type interactivity = Properties.interactivity =
  | Auto
  | Inert
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of interactivity var

val interactivity : interactivity -> declaration
(** [interactivity interactivity] is the CSS [interactivity] property. *)

type caret_animation = Properties.caret_animation =
  | Auto
  | Manual
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caret_animation var

val caret_animation : caret_animation -> declaration
(** [caret_animation animation] is the CSS [caret-animation] property. *)

type caret_shape = Properties.caret_shape =
  | Auto
  | Bar
  | Block
  | Underscore
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caret_shape var

val caret_shape : caret_shape -> declaration
(** [caret_shape shape] is the CSS [caret-shape] property. *)

type caret = Properties.caret =
  | Auto
  | Caret of color option * caret_animation option * caret_shape option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of caret var

val caret : caret -> declaration
(** [caret caret] is the CSS [caret] property. *)

type interest_delay = Properties.interest_delay =
  | Normal
  | Durations of duration list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of interest_delay var

val interest_delay : interest_delay -> declaration
(** [interest_delay delay] is the CSS [interest-delay] property. *)

val interest_delay_start : interest_delay -> declaration
(** [interest_delay_start delay] is the CSS [interest-delay-start] property. *)

val interest_delay_end : interest_delay -> declaration
(** [interest_delay_end delay] is the CSS [interest-delay-end] property. *)

type nav_scope = Properties.nav_scope = Current | Root | Named of string

type nav = Properties.nav =
  | Auto
  | Target of string * nav_scope option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of nav var

val nav_up : nav -> declaration
(** [nav_up nav] is the CSS [nav-up] property. *)

val nav_right : nav -> declaration
(** [nav_right nav] is the CSS [nav-right] property. *)

val nav_down : nav -> declaration
(** [nav_down nav] is the CSS [nav-down] property. *)

val nav_left : nav -> declaration
(** [nav_left nav] is the CSS [nav-left] property. *)

(** CSS pointer-events values *)
type pointer_events = Properties.pointer_events =
  | Auto
  | None
  | Visible_painted
  | Visible_fill
  | Visible_stroke
  | Visible
  | Painted
  | Fill
  | Stroke
  | All
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of pointer_events var

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

type box_decoration_break = Properties.box_decoration_break =
  | Clone
  | Slice
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of box_decoration_break var

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

(** Sec. 2.1 [anchor-name]: [none | <dashed-ident>#]. *)
type anchor_name = Properties.anchor_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of anchor_name var

val anchor_name : anchor_name -> declaration
(** [anchor_name v] is the [anchor-name] property. *)

(** Sec. 4.1 [position-anchor]: [normal | none | auto | <anchor-name>]. *)
type position_anchor = Properties.position_anchor =
  | Normal
  | None
  | Auto
  | Anchor of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_anchor var

val position_anchor : position_anchor -> declaration
(** [position_anchor v] is the [position-anchor] property. *)

(** Sec. 3.1.2 [<position-area>]: one of the grid keywords naming a region
    around the anchor. *)
type position_area_keyword = Properties.position_area_keyword =
  | Top
  | Bottom
  | Left
  | Right
  | Center
  | Span_top
  | Span_bottom
  | Span_left
  | Span_right
  | X_start
  | X_end
  | Y_start
  | Y_end
  | Span_x_start
  | Span_x_end
  | Span_y_start
  | Span_y_end
  | Inline_start
  | Inline_end
  | Block_start
  | Block_end
  | Span_inline_start
  | Span_inline_end
  | Span_block_start
  | Span_block_end
  | Start
  | End
  | Span_start
  | Span_end
  | Self_start
  | Self_end
  | Span_self_start
  | Span_self_end
  | Self_x_start
  | Self_x_end
  | Self_y_start
  | Self_y_end
  | Span_self_x_start
  | Span_self_x_end
  | Span_self_y_start
  | Span_self_y_end
  | Self_block_start
  | Self_block_end
  | Self_inline_start
  | Self_inline_end
  | Span_self_block_start
  | Span_self_block_end
  | Span_self_inline_start
  | Span_self_inline_end
  | Span_all

(** Sec. 3.1.2 [position-area]: one or two keywords from a single branch of the
    grammar. *)
type position_area = Properties.position_area =
  | None
  | Area of position_area_keyword * position_area_keyword option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_area var

val position_area : position_area -> declaration
(** [position_area v] is the [position-area] property. *)

(** Sec. 6.1 [<try-tactic>] and the [<dashed-ident>] naming a [@position-try]
    rule. *)
type position_try_fallback = Properties.position_try_fallback =
  | Flip_block
  | Flip_inline
  | Flip_start
  | Name of string

(** Sec. 6.1: one comma-separated fallback entry, which is either a tactic group
    or a [<position-area>], never a mix of the two. *)
type position_try_fallback_entry = Properties.position_try_fallback_entry =
  | Tactics of position_try_fallback list
  | Area of position_area_keyword * position_area_keyword option

(** Sec. 6.1 [position-try-fallbacks]. *)
type position_try_fallbacks = Properties.position_try_fallbacks =
  | None
  | Fallbacks of position_try_fallback_entry list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try_fallbacks var

val position_try_fallbacks : position_try_fallbacks -> declaration
(** [position_try_fallbacks v] is the [position-try-fallbacks] property. *)

(** Sec. 6.2 [position-try-order]: [normal | <try-size>]. *)
type position_try_order = Properties.position_try_order =
  | Normal
  | Most_width
  | Most_height
  | Most_block_size
  | Most_inline_size
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try_order var

val position_try_order : position_try_order -> declaration
(** [position_try_order v] is the [position-try-order] property. *)

(** Sec. 6.3 [position-try]:
    [<'position-try-order'>? <'position-try-fallbacks'>]. *)
type position_try = Properties.position_try =
  | Try of position_try_order * position_try_fallbacks
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_try var

val position_try : position_try -> declaration
(** [position_try v] is the [position-try] shorthand. *)

(** Sec. 7 [<anchor-visibility>]: one condition that hides the box. *)
type position_visibility_condition = Properties.position_visibility_condition =
  | Anchors_visible
  | No_overflow

(** Sec. 7 [position-visibility]. *)
type position_visibility = Properties.position_visibility =
  | Always
  | Conditions of position_visibility_condition list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of position_visibility var

val position_visibility : position_visibility -> declaration
(** [position_visibility v] is the [position-visibility] property. *)

(** {2:view_transitions View Transitions}

    Properties that name the elements a view transition animates independently.

    @see <https://www.w3.org/TR/css-view-transitions-1/>
      CSS View Transitions Module Level 1
    @see <https://www.w3.org/TR/css-view-transitions-2/>
      CSS View Transitions Module Level 2, which adds [match-element] *)

(** View Transitions 1
    {{:https://drafts.csswg.org/css-view-transitions-1/#propdef-view-transition-name}
     [view-transition-name]}, with the [match-element] of Level 2. *)
type view_transition_name = Properties.view_transition_name =
  | None
  | Match_element
  | Name of string
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_transition_name var

val view_transition_name : view_transition_name -> declaration
(** [view_transition_name v] is the [view-transition-name] property. *)

(** View Transitions 2
    {{:https://drafts.csswg.org/css-view-transitions-2/#propdef-view-transition-class}
     [view-transition-class]}: [none | <custom-ident>+]. *)
type view_transition_class = Properties.view_transition_class =
  | None
  | Classes of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_transition_class var

val view_transition_class : view_transition_class -> declaration
(** [view_transition_class v] is the [view-transition-class] property. *)

(** {2:motion_path Motion Path}

    Properties that move a box along a path rather than by an offset.

    @see <https://www.w3.org/TR/motion-1/> Motion Path Module Level 1 *)

(** Sec. 3.2 [<ray-size>]: how far the ray reaches. *)
type ray_size = Properties.ray_size =
  | Closest_side
  | Closest_corner
  | Farthest_side
  | Farthest_corner
  | Sides

type ray = Properties.ray = {
  angle : angle;
  size : ray_size option;
  contain : bool;
  position : position_value option;
}
(** Sec. 3.2 [ray()]: an angle, a size, whether the path is contained, and the
    position it starts from. *)

(** Sec. 2.1 [offset-path]: [none | <offset-path> || <coord-box>], where the
    shape branch reuses {!type-clip_path}. *)
type offset_path = Properties.offset_path =
  | None
  | Url of string
  | Path of string
  | Ray of ray
  | Shape of clip_path
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_path var

val offset_path : offset_path -> declaration
(** [offset_path v] is the [offset-path] property. *)

val offset_distance : length_percentage -> declaration
(** [offset_distance v] is the [offset-distance] property. *)

(** Sec. 2.3 [offset-rotate]: which of [auto] and [reverse] an explicit angle is
    measured from. *)
type offset_rotate_mode = Properties.offset_rotate_mode = Auto | Reverse

(** Sec. 2.3 [offset-rotate]: [[ auto | reverse ] || <angle>]. *)
type offset_rotate = Properties.offset_rotate =
  | Auto
  | Reverse
  | Angle of angle
  | With_angle of offset_rotate_mode * angle
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_rotate var

val offset_rotate : offset_rotate -> declaration
(** [offset_rotate v] is the [offset-rotate] property. *)

(** Sec. 2.4 [offset-anchor]: [auto | <position>]. *)
type offset_anchor = Properties.offset_anchor =
  | Auto
  | Position of position_value
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_anchor var

val offset_anchor : offset_anchor -> declaration
(** [offset_anchor v] is the [offset-anchor] property. *)

(** Sec. 2.5 [offset-position]: [normal | auto | <position>]. *)
type offset_position = Properties.offset_position =
  | Normal
  | Auto
  | Position of position_value
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset_position var

val offset_position : offset_position -> declaration
(** [offset_position v] is the [offset-position] property. *)

(** Sec. 2.6: the leading group of the [offset] shorthand, which is either a
    position on its own or a path with the slots that follow it. *)
type offset_target = Properties.offset_target =
  | Position_only of offset_position
  | With_path of {
      position : offset_position option;
      path : offset_path;
      distance : length_percentage option;
      rotate : offset_rotate option;
    }

(** Sec. 2.6 [offset]:
    [[ <'offset-position'>? [ <'offset-path'> [ <'offset-distance'> ||
     <'offset-rotate'> ]? ]? ]! [ / <'offset-anchor'> ]?]. *)
type offset = Properties.offset =
  | Shorthand of { target : offset_target; anchor : offset_anchor option }
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of offset var

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

(** CSS container-type values *)
type container_type = Properties.container_type =
  | Size
  | Inline_size
  | Scroll_state
  | Normal
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of container_type var

val container_type : container_type -> declaration
(** [container_type type_] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-type}
     container-type} property for container queries.

    The shorthand that sets this and the name together is
    {!val-Declaration.container}, because {!val-container} at this level is the
    at-rule builder. *)

(** One axis of CSS Sizing 4
    {{:https://drafts.csswg.org/css-sizing-4/#propdef-contain-intrinsic-size}
     [contain-intrinsic-size]}, a length that the [auto] prefix lets a
    remembered size override. *)
type contain_intrinsic_size_item = Properties.contain_intrinsic_size_item =
  | Length of length
  | Auto of length

(** CSS Sizing 4
    {{:https://drafts.csswg.org/css-sizing-4/#propdef-contain-intrinsic-size}
     [contain-intrinsic-size]}: one axis or both. *)
type contain_intrinsic_size = Properties.contain_intrinsic_size =
  | None
  | Intrinsic of
      contain_intrinsic_size_item * contain_intrinsic_size_item option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of contain_intrinsic_size var

val contain_intrinsic_size : contain_intrinsic_size -> declaration
(** [contain_intrinsic_size v] is the [contain-intrinsic-size] shorthand. *)

(** One axis longhand of {!type-contain_intrinsic_size}.

    @see <https://drafts.csswg.org/css-sizing-4/#propdef-contain-intrinsic-width>
      contain-intrinsic-width *)
type contain_intrinsic_longhand = Properties.contain_intrinsic_longhand =
  | None
  | Size of contain_intrinsic_size_item
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of contain_intrinsic_longhand var

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

type container_name = Properties.container_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of container_name var

val container_name : string -> declaration
(** [container_name name] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/container-name}
     container-name} property. *)

(** CSS contain values *)
type contain = Properties.contain =
  | None
  | Strict
  | Content
  | Size
  | Layout
  | Style
  | Paint
  | Inline_size
  | List of contain list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of contain var

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

(** CSS webkit-box-orient values. *)
type webkit_box_orient = Properties.webkit_box_orient =
  | Horizontal
  | Vertical
  | Inline_axis
  | Block_axis
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_box_orient var

(** CSS -webkit-line-clamp values. *)
type webkit_line_clamp = Properties.webkit_line_clamp =
  | None
  | Lines of int
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_line_clamp var

(** CSS -webkit-appearance values. *)
type webkit_appearance = Properties.webkit_appearance =
  | None  (** No appearance styling *)
  | Auto  (** Default browser styling *)
  | Button  (** Button appearance *)
  | Textfield  (** Text field appearance *)
  | Menulist  (** Select/dropdown appearance *)
  | Base_select  (** The base appearance of a select (Chrome alias) *)
  | Listbox  (** List box appearance *)
  | Checkbox  (** Checkbox appearance *)
  | Radio  (** Radio button appearance *)
  | Push_button  (** Push button appearance *)
  | Square_button  (** Square button appearance *)
  | Apple_pay_button  (** Apple Pay button appearance *)
  | Inherit  (** Inherit from parent *)
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_appearance var

(** CSS -webkit-font-smoothing values. *)
type webkit_font_smoothing = Properties.webkit_font_smoothing =
  | Auto
  | None
  | Antialiased
  | Subpixel_antialiased
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of webkit_font_smoothing var

(** CSS -moz-osx-font-smoothing values. *)
type moz_osx_font_smoothing = Properties.moz_osx_font_smoothing =
  | Auto
  | Grayscale
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of moz_osx_font_smoothing var

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

(** CSS [symbols()] counter-system keywords *)
type symbols_type = Properties.symbols_type =
  | Cyclic
  | Numeric
  | Alphabetic
  | Symbolic
  | Fixed

type list_style_symbol = Properties.list_style_symbol =
  | String of string
  | Url of string

val list_style_symbol_string : string -> list_style_symbol
(** [list_style_symbol_string value] is a string symbol for [symbols()]. *)

val list_style_symbol_url : string -> list_style_symbol
(** [list_style_symbol_url value] is a URL symbol for [symbols()]. *)

(** CSS list-style-type values *)
type list_style_type = Properties.list_style_type =
  | None
  | Disc
  | Circle
  | Square
  | Decimal
  | Lower_alpha
  | Upper_alpha
  | Lower_roman
  | Upper_roman
  | Decimal_leading_zero
  | Arabic_indic
  | Armenian
  | Upper_armenian
  | Lower_armenian
  | Bengali
  | Cambodian
  | Khmer
  | Cjk_decimal
  | Devanagari
  | Georgian
  | Gujarati
  | Gurmukhi
  | Hebrew
  | Kannada
  | Lao
  | Malayalam
  | Mongolian
  | Myanmar
  | Oriya
  | Persian
  | Tamil
  | Telugu
  | Thai
  | Tibetan
  | Lower_latin
  | Upper_latin
  | Cjk_earthly_branch
  | Cjk_heavenly_stem
  | Lower_greek
  | Hiragana
  | Hiragana_iroha
  | Katakana
  | Katakana_iroha
  | Disclosure_open
  | Disclosure_closed
  | Cjk_ideographic
  | Japanese_informal
  | Japanese_formal
  | Korean_hangul_formal
  | Korean_hanja_informal
  | Korean_hanja_formal
  | Simp_chinese_informal
  | Simp_chinese_formal
  | Trad_chinese_informal
  | Trad_chinese_formal
  | Ethiopic_numeric
  | Name of string  (** A case-sensitive custom counter-style name. *)
  | String of string
  | Symbols of symbols_type option * list_style_symbol list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_type var

val list_style_string : string -> list_style_type
(** [list_style_string value] is a string [list-style-type]. *)

val list_style_symbols :
  ?kind:symbols_type -> list_style_symbol list -> list_style_type
(** [list_style_symbols ?kind symbols] is a [symbols(...)] list-style type. *)

(** CSS list-style-image values *)
type list_style_image = Properties.list_style_image =
  | None
  | Image of background_image
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style_image var

type list_style_shorthand = Properties.list_style_shorthand = {
  type_ : list_style_type option;
  position : list_style_position option;
  image : list_style_image option;
}

type list_style = Properties.list_style =
  | Shorthand of list_style_shorthand
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of list_style var

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
type table_layout = Properties.table_layout =
  | Auto
  | Fixed
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of table_layout var

type vertical_align = Properties.vertical_align =
  | Baseline
  | Top
  | Middle
  | Bottom
  | Text_top
  | Text_bottom
  | Sub
  | Super
  | Zero
  | Px of float
  | Rem of float
  | Em of float
  | Pct of float
  | Calc of vertical_align calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of vertical_align var

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

(** SVG paint values for fill and stroke properties *)
type svg_paint = Properties.svg_paint =
  | None  (** No paint *)
  | Inherit  (** Inherited value *)
  | Current_color  (** Current color value *)
  | Color of color  (** Specific color value *)
  | Url of string * svg_paint option  (** url(#id) with optional fallback *)
  | Context_fill  (** SVG2 [context-fill] keyword *)
  | Context_stroke  (** SVG2 [context-stroke] keyword *)
  | Var of svg_paint var

val svg_paint_color : color -> svg_paint
(** [svg_paint_color color] is a color paint value. *)

val svg_paint_url : ?fallback:svg_paint -> string -> svg_paint
(** [svg_paint_url ?fallback url] is a URL paint value with an optional
    fallback. *)

val fill : svg_paint -> declaration
(** [fill paint] is the SVG fill property. *)

val stroke : svg_paint -> declaration
(** [stroke paint] is the SVG stroke property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeWidthProperty}
     [stroke-width]}: [<length-percentage> | <number>], where a bare number is a
    width in user units rather than a CSS [<length>]. *)
type stroke_width = Properties.stroke_width =
  | Number of float  (** A width in user units *)
  | Length of length_percentage
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_width var

val stroke_width : stroke_width -> declaration
(** [stroke_width width] is the SVG stroke-width property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#FillRuleProperty} [fill-rule]}:
    which points count as inside a shape when its subpaths overlap. CSS Masking
    1 {{:https://drafts.csswg.org/css-masking-1/#propdef-clip-rule} [clip-rule]}
    takes the same values. *)
type fill_rule = Properties.fill_rule =
  | Nonzero
  | Evenodd
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of fill_rule var

val fill_rule : fill_rule -> declaration
(** [fill_rule v] is the SVG [fill-rule] property. *)

val clip_rule : fill_rule -> declaration
(** [clip_rule v] is the SVG [clip-rule] property, which takes what [fill-rule]
    takes. *)

val fill_opacity : opacity -> declaration
(** [fill_opacity v] is the SVG [fill-opacity] property. *)

val stroke_opacity : opacity -> declaration
(** [stroke_opacity v] is the SVG [stroke-opacity] property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeLinecapProperty}
     [stroke-linecap]}: the shape at the ends of an open subpath. *)
type stroke_linecap = Properties.stroke_linecap =
  | Butt
  | Round
  | Square
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_linecap var

val stroke_linecap : stroke_linecap -> declaration
(** [stroke_linecap v] is the SVG [stroke-linecap] property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeLinejoinProperty}
     [stroke-linejoin]}: the shape at a corner between two stroke segments. *)
type stroke_linejoin = Properties.stroke_linejoin =
  | Miter
  | Miter_clip
  | Round
  | Bevel
  | Arcs
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_linejoin var

val stroke_linejoin : stroke_linejoin -> declaration
(** [stroke_linejoin v] is the SVG [stroke-linejoin] property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeMiterlimitProperty}
     [stroke-miterlimit]}: the ratio past which a miter join falls back to a
    bevel. *)
type stroke_miterlimit = Properties.stroke_miterlimit =
  | Number of float
  | Calc of stroke_miterlimit calc
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_miterlimit var

val stroke_miterlimit : stroke_miterlimit -> declaration
(** [stroke_miterlimit v] is the SVG [stroke-miterlimit] property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeDasharrayProperty}
     [stroke-dasharray]} writes each dash as a [<length-percentage>] or a bare
    number in user units, the way {!type-stroke_width} does. *)
type dash_length = Properties.dash_length =
  | Number of number
  | Length of length_percentage

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeDashoffsetProperty}
     [stroke-dashoffset]}: where the dash pattern starts. *)
type stroke_dashoffset = Properties.stroke_dashoffset =
  | Dash of dash_length
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_dashoffset var

val stroke_dashoffset : stroke_dashoffset -> declaration
(** [stroke_dashoffset v] is the SVG [stroke-dashoffset] property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#StrokeDasharrayProperty}
     [stroke-dasharray]}: the dash and gap lengths. *)
type stroke_dasharray = Properties.stroke_dasharray =
  | None
  | Dashes of dash_length list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of stroke_dasharray var

val stroke_dasharray : stroke_dasharray -> declaration
(** [stroke_dasharray v] is the SVG [stroke-dasharray] property. *)

(** One of the three painting operations SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#PaintOrderProperty}
     [paint-order]} orders. *)
type paint_order_keyword = Properties.paint_order_keyword =
  | Fill
  | Stroke
  | Markers

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/painting.html#PaintOrderProperty}
     [paint-order]}: the order fill, stroke and markers paint in. *)
type paint_order = Properties.paint_order =
  | Normal
  | Order of paint_order_keyword list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of paint_order var

val paint_order : paint_order -> declaration
(** [paint_order v] is the SVG [paint-order] property. *)

(** One effect the transform does not scale, for SVG 2
    {{:https://www.w3.org/TR/SVG2/coords.html#VectorEffectProperty}
     [vector-effect]}. *)
type vector_effect_keyword = Properties.vector_effect_keyword =
  | Non_scaling_stroke
  | Non_scaling_size
  | Non_rotation
  | Fixed_position

(** The coordinate space an SVG 2
    {{:https://www.w3.org/TR/SVG2/coords.html#VectorEffectProperty}
     [vector-effect]} effect is taken against. *)
type vector_effect_space = Properties.vector_effect_space = Viewport | Screen

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/coords.html#VectorEffectProperty}
     [vector-effect]}. *)
type vector_effect = Properties.vector_effect =
  | None
  | Effects of vector_effect_keyword list * vector_effect_space option
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of vector_effect var

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

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-dominant-baseline}
     [dominant-baseline]}: [auto | <baseline-metric>]. *)
type dominant_baseline = Properties.dominant_baseline =
  | Auto
  | Alphabetic
  | Ideographic
  | Mathematical
  | Central
  | Middle
  | Text_top
  | Text_bottom
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of dominant_baseline var

val dominant_baseline : dominant_baseline -> declaration
(** [dominant_baseline v] is the [dominant-baseline] property. *)

(** SVG 2
    {{:https://www.w3.org/TR/SVG2/text.html#AlignmentBaselineProperty}
     [alignment-baseline]}: the baseline of the box aligned against its parent's
    dominant baseline. *)
type alignment_baseline = Properties.alignment_baseline =
  | Baseline
  | Text_bottom
  | Middle
  | Central
  | Text_top
  | Ideographic
  | Alphabetic
  | Hanging
  | Mathematical
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of alignment_baseline var

val alignment_baseline : alignment_baseline -> declaration
(** [alignment_baseline v] is the [alignment-baseline] property. *)

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-baseline-shift}
     [baseline-shift]}:
    [<length-percentage> | sub | super | top | center | bottom]. *)
type baseline_shift = Properties.baseline_shift =
  | Shift of length_percentage
  | Sub
  | Super
  | Top
  | Center
  | Bottom
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of baseline_shift var

val baseline_shift : baseline_shift -> declaration
(** [baseline_shift v] is the [baseline-shift] property. *)

(** CSS Inline 3
    {{:https://drafts.csswg.org/css-inline-3/#propdef-baseline-source}
     [baseline-source]}: which line box baseline an inline block aligns on. *)
type baseline_source = Properties.baseline_source =
  | Auto
  | First
  | Last
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of baseline_source var

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

(** CSS touch-action values *)
type touch_action = Properties.touch_action =
  | Auto
  | None
  | Pan_x
  | Pan_y
  | Pan_left
  | Pan_right
  | Pan_up
  | Pan_down
  | Pinch_zoom
  | Manipulation
  | Actions of touch_action list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Vars of touch_action var list
  | Var of touch_action var

(** CSS scroll-snap-strictness values *)
type scroll_snap_strictness = Properties.scroll_snap_strictness =
  | Mandatory
  | Proximity
  | Var of scroll_snap_strictness var

(** CSS scroll-snap axis values *)
type scroll_snap_axis = Properties.scroll_snap_axis =
  | None
  | X
  | Y
  | Block
  | Inline
  | Both
  | Var of scroll_snap_axis var

(** CSS scroll-snap-type values *)
type scroll_snap_type = Properties.scroll_snap_type =
  | Axis of scroll_snap_axis (* Just the axis, no strictness *)
  | Axis_with_strictness of
      scroll_snap_axis
      * scroll_snap_strictness (* Axis with explicit strictness or var *)
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_type var

(** CSS scroll-snap-align values *)
type scroll_snap_align = Properties.scroll_snap_align =
  | None
  | Start
  | End
  | Center
  | Snap_align_pair of scroll_snap_align * scroll_snap_align
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_align var

type timeline_axis = Properties.timeline_axis =
  | Block
  | Inline
  | X
  | Y
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_axis var

type timeline_shorthand_item = Properties.timeline_shorthand_item = {
  name : string;
  axis : timeline_axis option;
}

type timeline_shorthand = Properties.timeline_shorthand =
  | None
  | Timelines of timeline_shorthand_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_shorthand var

type view_timeline_shorthand_item = Properties.view_timeline_shorthand_item = {
  name : string;
  axis : timeline_axis option;
  inset : Properties.timeline_inset option;
}

type view_timeline_shorthand = Properties.view_timeline_shorthand =
  | None
  | Timelines of view_timeline_shorthand_item list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of view_timeline_shorthand var

(** [none | <dashed-ident>#], shared by [scroll-timeline-name],
    [view-timeline-name] and Scroll-driven Animations 1
    {{:https://drafts.csswg.org/scroll-animations-1/#propdef-timeline-scope}
     [timeline-scope]}. *)
type timeline_name = Properties.timeline_name =
  | None
  | Names of string list
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_name var

(** One edge of Scroll-driven Animations 1
    {{:https://drafts.csswg.org/scroll-animations-1/#propdef-view-timeline-inset}
     [view-timeline-inset]}. *)
type timeline_inset_item = Properties.timeline_inset_item =
  | Auto
  | Length of length_percentage

(** Sec. 5.2 [view-timeline-inset]: the start edge then the end edge. *)
type timeline_inset = Properties.timeline_inset =
  | Inset of timeline_inset_item * timeline_inset_item option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of timeline_inset var

(** Sec. 6.2 [<timeline-range-name>]: the named part of a view progress
    timeline. *)
type animation_range_name = Properties.animation_range_name =
  | Cover
  | Contain
  | Entry
  | Exit
  | Entry_crossing
  | Exit_crossing

(** Sec. 6.2: one end of [animation-range]. *)
type animation_range_item = Properties.animation_range_item =
  | Normal
  | Offset of length_percentage
  | Named of animation_range_name * length_percentage option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_range_item var

(** Sec. 6.2 [animation-range]: the start then the end. *)
type animation_range = Properties.animation_range =
  | Range of animation_range_item * animation_range_item option
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of animation_range var

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

val timeline_scope : timeline_name -> declaration
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

(** CSS scroll-snap-stop values *)
type scroll_snap_stop = Properties.scroll_snap_stop =
  | Normal
  | Always
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_snap_stop var

val scroll_snap_stop : scroll_snap_stop -> declaration
(** [scroll_snap_stop stop] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-snap-stop}
     scroll-snap-stop} property. *)

(** CSS scroll behavior values *)
type scroll_behavior = Properties.scroll_behavior =
  | Auto
  | Smooth
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of scroll_behavior var

val scroll_behavior : scroll_behavior -> declaration
(** [scroll_behavior behavior] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/scroll-behavior}
     scroll-behavior} property for smooth scrolling. *)

type color_scheme = Properties.color_scheme =
  | Normal
  | Light
  | Dark
  | Light_dark
  | Only_light
  | Only_dark
  | Only_light_dark
  | Custom of string list
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of color_scheme var

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

(** CSS overscroll behavior values *)
type overscroll_behavior = Properties.overscroll_behavior =
  | Auto
  | Contain
  | None
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of overscroll_behavior var

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

type appearance = Properties.appearance =
  | None
  | Auto
  | Button
  | Textfield
  | Menulist
  | Base_select
  | Inherit
  | Initial
  | Unset
  | Revert
  | Revert_layer
  | Var of appearance var

val appearance : appearance -> declaration
(** [appearance app] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/appearance} appearance}
    property. *)

val moz_appearance : appearance -> declaration
(** [moz_appearance v] is the [-moz-appearance] property. *)

type tab_size = Properties.tab_size =
  | Int of int
  | Length of length
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of tab_size var

val tab_size : int -> declaration
(** [tab_size size] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/tab-size} tab-size}
    property. *)

val tab_size_value : tab_size -> declaration
(** [tab_size_value v] is the [tab-size] property from a typed value, allowing a
    [<length>] in addition to an integer. *)

type scrollbar_width = Properties.scrollbar_width =
  | Auto
  | Thin
  | None
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of scrollbar_width var

type scrollbar_color = Properties.scrollbar_color =
  | Auto
  | Colors of color * color
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of scrollbar_color var

type scrollbar_gutter = Properties.scrollbar_gutter =
  | Auto
  | Stable
  | Stable_both_edges
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of scrollbar_gutter var

val scrollbar_width : scrollbar_width -> declaration
(** [scrollbar_width v] is the [scrollbar-width] property. *)

val scrollbar_color : scrollbar_color -> declaration
(** [scrollbar_color v] is the [scrollbar-color] property. *)

val scrollbar_gutter : scrollbar_gutter -> declaration
(** [scrollbar_gutter v] is the [scrollbar-gutter] property. *)

type zoom = Properties.zoom =
  | Normal
  | Reset
  | Num of float
  | Pct of float
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of zoom var

val zoom : zoom -> declaration
(** [zoom v] is the CSS [zoom] property. *)

val font_variation_settings : font_variation_settings -> declaration
(** [font_variation_settings settings] is the
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/font-variation-settings}
     font-variation-settings} property. *)

(** {1:custom_properties Custom Properties}

    Type-safe CSS custom properties (CSS variables) with GADT-based type
    checking. *)

(** Value kind GADT for typed custom properties *)
type 'a kind = 'a Properties.kind =
  | Length : length kind
  | Color : color kind
  | Rgb : rgb kind
  | Number : number kind
  | Int : int kind
  | Float : float kind
  | Percentage : percentage kind
  | Length_percentage : length_percentage kind
  | Number_percentage : number_percentage kind
  | Opacity : opacity kind
  | Value : custom_value kind
  | Duration : duration kind
  | Aspect_ratio : aspect_ratio kind
  | Border_style : border_style kind
  | Outline_style : outline_style kind
  | Border : border kind
  | Font_weight : font_weight kind
  | Font_size : font_size kind
  | Line_height : line_height kind
  | Font_family : font_family kind
  | Font_feature_settings : font_feature_settings kind
  | Font_variation_settings : font_variation_settings kind
  | Numeric : font_variant_numeric kind
  | Font_variant_numeric_token : font_variant_numeric_token kind
  | Blend_mode : blend_mode kind
  | Scroll_snap_strictness : scroll_snap_strictness kind
  | Angle : angle kind
  | Rotate : rotate_value kind
  | Scale : scale kind
  | Shadow : shadow kind
  | Content : content kind
  | Gradient_stop : gradient_stop kind
  | Gradient_direction : gradient_direction kind
  | Gradient_position : gradient_position kind
  | Radial_shape : radial_shape kind
  | Radial_size : radial_size kind
  | Position_value : position_value kind
  | Animation : animation kind
  | Timing_function : timing_function kind
  | Transform : transform kind
  | Touch_action : touch_action kind
  | Transition_property_value : transition_property_value kind
  | Background_image : background_image kind
  | Z_index : z_index kind
  | Filter : filter kind
  | Font_src : Font_face.src kind

type meta = Values.meta = ..
(** The type for CSS variable metadata. *)

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

(** Type-safe syntax descriptors for CSS [@property] rules per CSS Properties
    and Values API 1 sec. 2. *)
type 'a syntax = 'a Variables.syntax =
  | Length : length syntax
  | Color : color syntax
  | Number : float syntax
  | Integer : int syntax
  | Percentage : percentage syntax
  | Length_percentage : length_percentage syntax
  | Angle : angle syntax
  | Time : duration syntax
  | Resolution : string syntax
  | Custom_ident : string syntax
  | String : string syntax
  | Url : string syntax
  | Image : background_image syntax
  | Transform_function : string syntax
  | Transform_list : string syntax
  | Universal : string syntax
  | Or : 'a syntax * 'b syntax -> ('a, 'b) Either.t syntax
  | Plus : 'a syntax -> 'a list syntax
  | Hash : 'a syntax -> 'a list syntax
  | Ident_keyword : string -> unit syntax

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

    It raises [Failure] on a pair that does not make the one declaration it
    names, such as a value carrying a top-level [;] or [}] or an unterminated
    function, block or string. A name holding a code point no bare ident carries
    is written back with the escapes that read it.
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

(** Rendering mode for CSS output.
    - [Variables]: Standard rendering with CSS custom properties support
    - [Inline]: For inline styles (no at-rules, variables expanded with their
      values) *)
type mode = Stylesheet.mode = Variables | Inline

val to_string :
  ?minify:bool ->
  ?indent:int ->
  ?lossless:bool ->
  ?enforce_spec:bool ->
  t ->
  string
(** [to_string ?minify ?indent ?lossless ?enforce_spec stylesheet] serialises a
    stylesheet to CSS. Pure formatter - no optimisation, no theme resolution, no
    [var()] substitution. Run {!optimize}, {!resolve_theme}, and {!inline_vars}
    explicitly when those passes are needed. Spec recovery (drop invalid
    declarations and empty rules) still applies because the parser preserved
    those shapes for round-trip and browsers discard them during parse. Unknown
    at-rules are preserved. Output never ends with a newline.

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
    merged, dropped, or synthetic nodes. *)

val of_string_exn :
  ?strict:bool ->
  ?filename:string ->
  ?meta:Loc.meta_level ->
  ?enforce_spec:bool ->
  string ->
  t
(** [of_string_exn ?strict css] parses [css] like {!of_string}, raises
    {!Error.Parse_error} instead of returning [Error], and discards the
    {!field-warnings} list. In non-strict mode warnings are silently dropped;
    with [~strict:true] any warning escalates to a raise. *)

(** {2:optimization Optimization}

    Tools for optimizing CSS output for performance and file size. *)

val canonicalize_rule_order : ?lossless:bool -> t -> t
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
    [@container] block whose transitive content is plain rules moves as one
    unit, keyed by the union of its rules' conflict footprints; conflicting
    statements keep their relative order. Two equal [@supports] blocks may merge
    across an intervening non-important write that the later block shadows with
    the same selector and property whenever the condition holds. Named [@layer]
    blocks pin the layer order where they stand. A run of [@property] rules
    sorts by name, keeping the last registration of each, since CSS Properties
    and Values API 1 sec. 2 makes registrations for different names
    order-independent. A [@media] prelude is keyed as the Level 4 query Media
    Queries 4 makes it equal to - [not all and (X)] as [not (X)],
    [min-X]/[max-X] as the range form, and a lower bound met by an upper bound
    as the two-sided interval - and an [@container] prelude the same way, which
    emission cannot do because a Level 3 parser rejects the shorter forms. A
    [color(srgb ...)] whose channels all land on a whole byte is keyed as the
    [rgb()] spelling of the same colour, which emission cannot do either because
    [color()] needs a browser that parses it. A [none] channel of a Lab-family
    colour standing as a whole colour-longhand value is keyed as the zero CSS
    Color 4 sec. 4.4 says a missing component behaves as, so a converted
    achromatic [oklab()] meets the hex a minifier writes for it; sec. 13.3 keeps
    that off the positions the sheet interpolates, so a gradient stop, a
    [color-mix()] operand, a custom-property token stream, [@keyframes],
    [@starting-style] and a colour whose own rule transitions the property it
    writes keep their [none], and [lossless] bounds how far the resolved colour
    respells. An identical [-webkit-text-decoration-color] compatibility
    declaration is dropped when its unprefixed twin is present; a differing or
    prefixed-only declaration is retained. These are comparison-side
    normalisations; this function does not change {!val-optimize}'s configured
    emission policy. *)

val optimize :
  ?scope:Optimize.scope ->
  ?targets:Optimize.targets ->
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
(** [optimize ?scope ?targets ?flatten_nesting ?lossless ?enforce_spec
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

val inline_vars : ?keep_vars:string list -> ?warn:(string -> unit) -> t -> t
(** [inline_vars ?keep_vars ?warn stylesheet] substitutes [var(--name)]
    references with the value of the corresponding [--name] declaration and
    deletes the definition, but only for a variable with a single definition. A
    variable in [keep_vars], or one redefined in a different scope (a real
    cascade override such as dark mode), keeps its definition and stays a live
    [var()] reference; [warn] is called with each such name. The transform
    assumes no runtime mutation of the variables it inlines: a reference marked
    [~runtime] on {!var_ref} also stays live, fallback included, so a
    browser-time override point survives. A [style()] container query reads the
    computed value of the custom property it names, so that property stays live
    as well.

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

(** CSS will-change property values for performance optimization hints. *)
type will_change = Properties.will_change =
  | Will_change_auto
  | Scroll_position
  | Contents
  | Transform
  | Opacity
  | Properties of string list  (** Custom CSS property names *)
  | Initial
  | Inherit
  | Unset
  | Revert
  | Revert_layer
  | Var of will_change var

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

val pp_calc : ?unwrap:('a -> bool) -> 'a Pp.t -> 'a calc Pp.t
(** [pp_calc ?unwrap pp_value] is the pretty printer for calc expressions.
    Minified output drops the call around a single leaf, and [unwrap] says which
    leaves that is safe for. *)

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
