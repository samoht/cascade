(** CSS Syntax Module Level 3 section 5: parser algorithms.

    Ports the "consume a ..." algorithms from
    https://www.w3.org/TR/css-syntax-3/#parser-algorithms over the token stream
    produced by {!Token.next}. Produces the generic intermediate representation
    (component values, simple blocks, functions, rules, declarations) defined in
    section 5.1. Higher-level typed parsing runs afterwards as validation on
    those component values. *)

(** {1 Intermediate representation (section 5.1)} *)

(** A component value: a preserved token, a simple block, or a function. See
    section 5.1.2 and 5.3.8. *)
type component_value =
  | Preserved of Token.t
  | Block of simple_block
  | Func of function_cv

and simple_block = { opening : Token.bracket; value : component_value list }
(** A balanced [{...\}], [(...)] or [[...]] group (section 5.1.5). *)

and function_cv = { name : string; arguments : component_value list }
(** A [name(...)] group; the arguments are a list of component values (section
    5.1.4). *)

type at_rule = {
  name : string;
  prelude : component_value list;
  block : simple_block option;
}
(** An at-rule (section 5.1.7): name, prelude (component values between [\@name]
    and the block or terminating [;]), and optional block. *)

type qualified_rule = { prelude : component_value list; block : simple_block }
(** A qualified rule / style rule (section 5.1.6): prelude (typically a selector
    list) followed by a block. *)

type rule = Qualified of qualified_rule | At of at_rule

type declaration = {
  name : string;
  value : component_value list;
  important : bool;
}
(** A declaration extracted by section 5.3.7. [value] has trailing whitespace
    and any [!important] marker stripped. *)

(** {1 Reserialization} *)

val to_string : component_value list -> string
(** [to_string cvs] reconstitutes the source text for a component-value list.
    Whitespace tokens serialize to a single space; the output is not
    byte-identical to the input but is parse-equivalent, so downstream typed
    validators can re-parse the result. *)

(** {1 Parser entry points (section 5.4)} *)

val parse_stylesheet : Reader.t -> rule list
(** [parse_stylesheet r] runs section 5.4.3: consume a list of rules with the
    top-level flag set. CDO and CDC are skipped; anything else becomes a
    qualified rule or at-rule. *)

val parse_list_of_rules : Reader.t -> rule list
(** [parse_list_of_rules r] runs section 5.4.4. Unlike {!parse_stylesheet},
    CDO/CDC tokens are not discarded; suitable for nested rule bodies. *)

val parse_list_of_declarations :
  Reader.t -> [ `Decl of declaration | `At of at_rule ] list
(** [parse_list_of_declarations r] runs section 5.4.8. The result can mix
    declarations and nested at-rules. *)
