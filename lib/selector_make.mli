(** Selector constructors: identifier validation and unescaping, attribute-name
    classification and the combinators {!Selector} re-exports. *)

open Selector_intf

val is_valid_nmchar : char -> bool
(** [is_valid_nmchar c] is whether [c] may continue a CSS identifier. *)

val skip_css_escape : string -> int ref -> unit
(** [skip_css_escape name i] advances [i] past the escape that starts at the
    backslash [name.[!i]]: up to six hex digits and one space, or one character.
*)

val element : ?ns:ns -> string -> t
(** [element ?ns name] is the type selector [name].

    @raise Invalid_argument if [name] is not a valid CSS identifier. *)

val class_ : string -> t
(** [class_ name] is the class selector [.name], escaped when printed.

    @raise Invalid_argument
      if [name] is empty, starts with [--] or holds a control character. *)

val id : string -> t
(** [id name] is the ID selector [#name], under the same rules as {!class_}. *)

val universal : t
(** [universal] is [*]. *)

val universal_ns : ns -> t
(** [universal_ns ns] is [ns|*]. *)

val aria_attr_of_string : string -> aria_attr
(** [aria_attr_of_string s] is the ARIA attribute named [s].

    @raise Invalid_argument if [s] names none. *)

val attr_name_of_string : string -> attr_name
(** [attr_name_of_string name] classifies [name] as an ARIA, [data-] or regular
    attribute. *)

val string_of_aria_attr : aria_attr -> string
(** [string_of_aria_attr a] is the attribute name of [a]. *)

val string_of_attr_name : attr_name -> string
(** [string_of_attr_name a] is the attribute name [a] classifies. *)

val attribute : ?ns:ns -> ?flag:attr_flag -> string -> attribute_match -> t
(** [attribute ?ns ?flag name m] is the attribute selector on [name].

    @raise Invalid_argument if [name] is not a valid CSS identifier. *)

val unescape_selector_name : string -> string
(** [unescape_selector_name s] is [s] with its CSS escapes decoded. *)

val combine : t -> combinator -> t -> t
(** [combine s1 comb s2] joins [s1] and [s2] with [comb], distributed over each
    entry when [s2] is a list. *)

val ( ++ ) : t -> t -> t
(** [s1 ++ s2] is [combine s1 Descendant s2]. *)

val ( >> ) : t -> t -> t
(** [s1 >> s2] is [combine s1 Child s2]. *)

val where : t list -> t
(** [where sels] is [:where(sels)]. *)

val list : t list -> t
(** [list sels] is the selector list [sels].

    @raise Invalid_argument if [sels] is empty. *)

val unescape_attribute_value : string -> string
(** [unescape_attribute_value s] is {!unescape_selector_name}. *)

val is_compound_list : t -> bool
(** [is_compound_list s] is whether [s] is a selector list. *)

val as_list : t -> t list option
(** [as_list s] is the entries of [s] when it is a selector list. *)

val compound : t list -> t
(** [compound sels] is the compound selector of [sels]. *)
