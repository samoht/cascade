(** Structured media conditions for type-safe media query construction. *)

type cmp = Lt | Le | Eq | Gt | Ge

type name =
  | Width
  | Height
  | Inline_size
  | Block_size
  | Aspect_ratio
  | Resolution
  | Color
  | Color_index
  | Monochrome
  | Grid
  | Horizontal_viewport_segments
  | Vertical_viewport_segments
  | Orientation
  | Hover
  | Any_hover
  | Pointer
  | Any_pointer
  | Update
  | Overflow_block
  | Overflow_inline
  | Scan
  | Color_gamut
  | Video_color_gamut
  | Dynamic_range
  | Video_dynamic_range
  | Display_mode
  | Environment_blending
  | Prefers_color_scheme
  | Prefers_reduced_motion
  | Prefers_reduced_transparency
  | Prefers_reduced_data
  | Prefers_contrast
  | Forced_colors
  | Inverted_colors
  | Nav_controls
  | Scripting
  | Min of name
  | Max of name
  | Other of string

type ident =
  | Infinite
  | Portrait
  | Landscape
  | None
  | Hover
  | Coarse
  | Fine
  | Slow
  | Fast
  | Interlace
  | Progressive
  | Srgb
  | P3
  | Rec2020
  | Standard
  | High
  | Optional_paged
  | Paged
  | Scroll
  | Fullscreen
  | Standalone
  | Minimal_ui
  | Browser
  | Picture_in_picture
  | Opaque
  | Additive
  | Subtractive
  | Light
  | Dark
  | No_preference
  | Reduce
  | Less
  | More
  | Custom
  | Active
  | Inverted
  | Back
  | Initial_only
  | Enabled
  | Other of string

type value =
  | Length of Values.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution_value of float * string
  | Ident of ident
  | Function of string * string
      (** [env(--name)] / [var(...)] / [calc(...)] etc. captured as a function
          name plus its raw argument body. *)

type feature =
  | Plain of name * value
  | Boolean of name
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value

type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

type medium = All | Screen | Print | Other of string
type prefix = Not | Only

type query =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : medium;
      trailing : condition option;
    }
  | List of query list

type t =
  | Width of Values.length
  | Height of Values.length
  | Min_width of float
  | Max_width of float
  | Not_min_width of float
  | Min_width_rem of float
  | Not_min_width_rem of float
  | Min_width_length of Values.length
  | Not_min_width_length of Values.length
  | Aspect_ratio of int * int
  | Resolution of float * string
  | Color of int
  | Color_index of int
  | Monochrome of int
  | Color_gamut of ident
  | Video_color_gamut of ident
  | Dynamic_range of ident
  | Video_dynamic_range of ident
  | Scan of ident
  | Update of ident
  | Overflow_block of ident
  | Overflow_inline of ident
  | Prefers_reduced_motion of ident
  | Prefers_reduced_transparency of ident
  | Prefers_reduced_data of ident
  | Prefers_contrast of ident
  | Prefers_color_scheme of ident
  | Forced_colors of ident
  | Inverted_colors of ident
  | Pointer of ident
  | Any_pointer of ident
  | Hover of ident
  | Any_hover of ident
  | Scripting of ident
  | Nav_controls of ident
  | Print
  | Orientation of ident
  | And of t * t
  | Or of t * t
  | Negated of t
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value
  | Type_query of {
      prefix : prefix option;
      type_ : medium;
      trailing : t option;
    }
  | Plain of name * value
  | Boolean of name
  | List of t list

val of_string : string -> t
(** [of_string s] parses [s] as a media query. *)

val of_string_strict : string -> t
(** [of_string_strict s] parses [s] as a media query without branch recovery. *)

val of_function_body : string -> t
(** [of_function_body s] parses the body of a conditional [media(...)] function.
    Unlike a standalone media query, a single feature appears without its outer
    parentheses in this grammar. *)

val value_of_string : string -> value
(** [value_of_string s] parses [s] as a media-feature value. *)

val name_of_string : string -> name
val string_of_name : name -> string
val ident_of_string : string -> ident
val string_of_ident : ident -> string

val feature : string -> value -> t
(** [feature name v] is the plain feature [(name: v)]. *)

val boolean : string -> t
(** [boolean name] is the boolean feature [(name)]. *)

val to_string : ?minify:bool -> t -> string
(** [to_string ?minify t] serialises [t] as CSS source text. The default
    [~minify:false] keeps the pretty form ([(min-width: 30em)]); pass
    [~minify:true] for the compact form ([(min-width:30em)]). *)

val pp : t Pp.t
(** Pretty-printer for media queries. *)

val pp_query : query Pp.t
(** Pretty-printer for parsed media queries. *)

val pp_condition : condition Pp.t
(** Pretty-printer for media query conditions. *)

val pp_feature : feature Pp.t
(** Pretty-printer for media features. *)

val compare : t -> t -> int
(** Total order on media queries. *)

val equal : t -> t -> bool
(** Structural equality on media queries. *)

type kind =
  | Hover
  | Responsive of int * float
  | Responsive_max of int * float
  | Preference_accessibility
  | Preference_appearance
  | Other

val kind : t -> kind
(** [kind t] classifies [t] for grouping and ordering. *)

val group_order : kind -> int * float
(** [group_order k] is the sort key used to group queries by [kind]. *)

val preference_order : t -> int
(** [preference_order t] orders preference queries within their group. *)
