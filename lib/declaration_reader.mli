(** Declaration reading: names, values and importance, custom-property
    validation and canonicalisation, and error recovery over a block. *)

open Declaration_intf

val is_declaration_value : string -> bool
(** [is_declaration_value s] is whether [s] is a [<declaration-value>]: one or
    more component values with no unmatched closing bracket, no top-level [;] or
    [!], no [<bad-string-token>] or [<bad-url-token>] and no unterminated
    function, block or string (CSS Syntax 3 (ED) sec. 7.2). Text outside it
    stops being part of the declaration it is written into. Pass the value with
    its [!important] flag already off, as {!split_important} takes it. *)

val custom_property : ?layer:string -> string -> string -> declaration
(** [custom_property ?layer name value] is a custom property declaration from
    authored CSS text.

    @raise Failure
      if the pair does not make the one declaration it names. [name] has to be a
      [<dashed-ident>], and [value] the [<declaration-value>?] CSS Variables 1
      sec. 2 gives a custom property: no top-level [;], no unmatched closing
      bracket, no unterminated function, block or string. A [name] holding a
      code point no bare ident carries is written back with the escapes that
      read it. {!parse_custom_property} is the same check as an option. *)

val custom_declaration_layer : declaration -> string option
(** [custom_declaration_layer d] is the layer of [d], if any. *)

val unquote_custom_font_strings : declaration -> declaration
(** [unquote_custom_font_strings d] rewrites a quoted family name in a
    custom-property token stream as the equivalent unquoted [<ident>] sequence,
    one word or several, when a generic family in the stream proves the stream
    is a font-family list. Equivalence-only normalisation for structural
    diffing; a stream without that proof, and any other declaration, passes
    through unchanged. *)

val canonicalize_custom_time : declaration -> declaration
(** [canonicalize_custom_time d] spells every [<time>] token of a
    custom-property token stream as the shorter of its [s] and [ms] forms, which
    CSS Values 4 sec. 7.2 makes one quantity. Equivalence-only normalisation for
    structural diffing; any other declaration passes through unchanged. A
    container [style()] query on an unregistered property compares the declared
    tokens as written, so the caller keeps a property such a query names out of
    this fold. *)

val canonicalize_custom_angle : declaration -> declaration
(** [canonicalize_custom_angle d] spells every [<angle>] token of a
    custom-property token stream as the degrees it names, a converted unit
    rounded to the six-significant-figure budget a computed value takes, which
    CSS Values 4 sec. 6.1 makes one dimension. Equivalence-only normalisation
    for structural diffing; any other declaration passes through unchanged. As
    for {!canonicalize_custom_time}, the caller keeps a property a container
    [style()] query names out of this fold. *)

val canonicalize_custom_whitespace : declaration -> declaration
(** [canonicalize_custom_whitespace d] drops from a custom-property token stream
    the whitespace CSS reads as nothing: around the [*] and [/] of CSS Values 4
    (ED) sec. 10.8 arithmetic, and after a function or block whose closing
    bracket already separates it from what follows. The whitespace sec. 10.8
    requires around a math [+] or [-] stays, and so does the whitespace next to
    a [var()], [env()] or [attr()] whose substitution would otherwise merge with
    its neighbour. Equivalence-only normalisation for structural diffing; any
    other declaration passes through unchanged. *)

val read_property_name : Cursor.t -> string
(** [read_property_name t] is the property name read from [t]. *)

val read_property_value : Cursor.t -> string
(** [read_property_value t] is the value read from [t] (until ';' or '\}'). *)

val split_important : string -> string * bool
(** [split_important s] is [s] without its [!important] flag, and whether it
    carried one. CSS Syntax 3 (ED) sec. 5.5.6 takes the flag off when the last
    two non-whitespace values are a [!] delim and the ident naming the flag, so
    [1 ! important] carries one and [1 !importantly] does not. *)

val read_declaration : Cursor.t -> declaration option
(** [read_declaration t] is one typed declaration, or [None] when no more valid
    declarations. Performs full property name and value validation per CSS spec.
*)

val read_declarations : Cursor.t -> declaration list
(** [read_declarations t] is all typed declarations in an unbraced block. *)

val read_block : Cursor.t -> declaration list
(** [read_block t] is the typed declarations parsed from a braced block. *)

val of_string : string -> declaration
(** [of_string s] parses a single declaration from [s] (e.g. ["color: red"]).
    Raises {!Cursor.exception-Parse_error}, anchored on the offending text, when
    [s] is not a valid declaration. *)

val parse_declaration : ?layer:string -> string -> string -> declaration option
(** [parse_declaration ?layer property value] reads [property] and [value] with
    the declaration parser into a fully-typed declaration. The two are read as
    the tokens they are rather than as one ["property: value"] text, so a
    [property] carrying a [;], a [}] or a [:] names this declaration or names
    none:
    - a known property (e.g. [mask-type], {!val-display}) becomes a typed
      declaration;
    - a custom property ([--x]) or an unknown property keeps its parsed
      component stream (not an opaque [Tokens] wrapper), so [var()] references
      in [value] are visible to [vars_of_declarations];
    - [None] if [value] does not parse.

    [layer] applies only to a custom property (a typed declaration has no
    layer). Unlike {!custom_property}, this parses the full property grammar. *)

val parse_opaque_declaration : string -> string -> declaration option
(** [parse_opaque_declaration property value] is {!parse_declaration} with the
    typed grammar skipped: [value] keeps the component stream it was written as
    even where [property] has a typed grammar, so nothing about its spelling is
    canonicalised. A custom property already reads that way and is unaffected.
    It serves an [\@supports] condition, where the declaration is handed to
    another parser rather than rendered, so the spelling is the question being
    asked. [None] on a [value] no declaration can hold. *)

val parse_custom_property : string -> string -> declaration option
(** [parse_custom_property name value] is {!parse_declaration} restricted to a
    custom property that a rule can hold. It is [None] unless [name] is a
    [<dashed-ident>] and [value] is the [<declaration-value>?] CSS Variables 1
    sec. 2 gives a custom property: no [<bad-string-token>], no
    [<bad-url-token>], no unmatched closing bracket, no unterminated function or
    block, and no top-level [;] or [!] (CSS Syntax 3 (ED) sec. 7.2). The empty
    value is one of its values.

    Use it for a name or value that comes from outside the parser, where the
    string may close the block it is placed in or start a second declaration.
    {!custom_property} takes the same pairs and raises on the rest. *)

val read : Cursor.t -> declaration
(** [read t] parses one typed declaration from [t]. *)

val with_value : declaration -> string -> declaration
(** [with_value decl value] is [decl] carrying [value], read as a value of the
    property [decl] already holds and re-typed where that property accepts it.
    The property is taken from [decl] rather than from its name, so one whose
    minified spelling belongs to another property ([page-break-*] renders as the
    CSS Fragmentation 3 sec. 3.4 [break-*] alias) is rebuilt as itself. Raises
    [Cursor.Parse_error] when the property accepts no such value. *)

val with_opaque_value : declaration -> string -> declaration
(** [with_opaque_value decl value] is [decl]'s property carrying [value] as an
    unknown property's token stream, for a [value] {!with_value} rejects. The
    property is named as it parses back, not as it minifies. *)
