open Cascade

let _ = Declaration.pp
let _ = Declaration.to_string
let _ = Stylesheet.empty
let _ = Stylesheet.read
let _ = Stylesheet.to_string
let _ = Stylesheet.pp_stylesheet
let _ = Keyframe.to_string
let _ = Css.to_string
let _ = Container.to_string
let _ : Container.t Pp.t = Container.pp
let _ : Stylesheet.t Pp.t = Stylesheet.pp
let _ : Css.t Pp.t = Css.pp
let _ : Optimize.objective = `Transfer
let _ : Css.Optimize.objective = `Raw
let _ :
    ?optimize:bool ->
    ?minify:bool ->
    ?mode:Css.mode ->
    Css.declaration list ->
    string =
  Css.inline_style_of_declarations
let _ = Pp.float
let _ = Css.Gradient_direction.of_string
let _ = Css.shadow
let _ : Css.shadow Css.kind = Css.Shadow

(* The statement-merging passes are callable on their own, so a caller that
   does not run the whole optimizer can still collapse a run of blocks. *)
type merge_block =
  ?optimize_merged_block:
    (Stylesheet.statement list -> Stylesheet.statement list) ->
  Stylesheet.statement list ->
  Stylesheet.statement list

let _ : merge_block = Optimize.merge_consecutive_layers
let _ : merge_block = Optimize.merge_consecutive_media
let _ : merge_block = Optimize.merge_consecutive_supports
let _ : merge_block = Optimize.merge_consecutive_containers
let _ : merge_block = Optimize.merge_consecutive_starting_style
let _ : ?owner:Stylesheet.rule -> merge_block = Optimize.merge_distant_media

let _ : ?owner:Stylesheet.rule -> merge_block =
  Optimize.merge_distant_containers

let _ : Stylesheet.statement list -> Stylesheet.statement list =
  Optimize.merge_named_layers_by_name

let _ : Stylesheet.statement list -> Stylesheet.statement list =
  Optimize.drop_empty_rules

(* Two statements can be compared and keyed on without rendering either to CSS
   text, and a colour can be given an alpha without re-spelling it by hand. *)
let _ : Css.statement -> Css.statement -> bool = Css.equal_statement
let _ : Css.statement -> int = Css.hash_statement
let _ : Css.Values.color -> Css.Values.color -> bool = Css.Values.equal_color
let _ : Css.Values.color -> int = Css.Values.hash_color

let _ : Css.Values.color -> Css.Values.alpha -> Css.Values.color =
  Css.Values.with_alpha

(* An at-rule cascade has no grammar for is constructible, so a caller emitting
   one does not assemble a sheet as text and read it back to get a statement. *)
let _ :
    name:string ->
    prelude:string ->
    ?block:string ->
    unit ->
    (Css.statement, Error.t) result =
  Css.unknown_at_rule

(* A declaration's typed value is reachable without naming an implementation
   module. The witness ties the result to the type that property carries, so
   telling a [var()] carrier apart from a declared [border-style] is a pattern
   match rather than a comparison against printed text. *)
let _ : Declaration.t -> bool =
 fun d ->
  match Declaration.value_of d Properties.Border_style with
  | Some [ Var _ ] -> true
  | Some _ | None -> false

(* The witness the value is read at is the one {!Declaration.property_key}
   wraps, so a caller that already holds a key can spend it here. *)
let _ : Declaration.t -> bool =
 fun d ->
  match Declaration.property_key d with
  | Declaration.Key p -> Option.is_some (Declaration.value_of d p)

(* Property identities compare and, where they agree, carry the type equality
   the comparison alone cannot express. *)
let _ : 'a Properties.property -> 'b Properties.property -> int =
  Properties.compare_property

let _ :
    'a Properties.property -> 'b Properties.property -> ('a, 'b) Type.eq option =
  Properties.eq_property

(* A name is checked whole. Rebuilding CSS Syntax 3 sec. 4.3.11 out of the
   per-character predicates drops the two cases they cannot see: a lone [-] and
   a [-] followed by a digit open no ident. *)
let _ : string -> bool = Syntax.is_ident

(* The math and colour function names are one table each, not one per caller: a
   generator that has to tell calc() from a colour asks the same list the
   optimizer folds by. *)
let _ : string -> bool = Properties.is_math_function
let _ : string -> bool = Properties.is_color_function

(* A registration read back at its syntax carries a value typed by it, so
   assigning that value is one call and not a match over every syntax arm with
   a printer per arm. The existential is the point: the name pins nothing
   unless it takes the syntax and the value the same pack hands out. *)
let _ : Css.statement -> Css.declaration option =
 fun stmt ->
  match Css.as_property stmt with
  | Some (Css.Property_info { name; syntax; initial_value = Some v; _ }) ->
      Some (Css.Variables.typed_custom_property name syntax v)
  | Some (Css.Property_info { initial_value = None; _ }) | None -> None

let _ :
    ?layer:string ->
    string ->
    Values.length Css.Variables.syntax ->
    Values.length ->
    Css.declaration =
  Css.Variables.typed_custom_property

(* The contents of a [var()] - a name and optional fallback - are readable
   from a cursor already positioned at them, without assembling the [var(]
   and [)] wrapper around a string first: the same relationship
   [Values.read_calc_expr] has to a [calc()] body. *)
let _ : (Cursor.t -> Values.length) -> Cursor.t -> Values.length Css.var =
  Css.Variables.read_reference_body
