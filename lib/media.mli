(** Structured media conditions for type-safe media query construction. *)

type cmp = Lt | Le | Eq | Gt | Ge

type value =
  | Length of Values.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution_value of float * string
  | Ident of string

type feature =
  | Plain of string * value
  | Boolean of string
  | Range of string * cmp * value
  | Range_rev of value * cmp * string
  | Interval of value * cmp * string * cmp * value

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
  | Color_gamut of [ `Srgb | `P3 | `Rec2020 ]
  | Video_color_gamut of [ `Srgb | `P3 | `Rec2020 ]
  | Dynamic_range of [ `Standard | `High ]
  | Video_dynamic_range of [ `Standard | `High ]
  | Scan of [ `Interlace | `Progressive ]
  | Update of [ `None | `Slow | `Fast ]
  | Overflow_block of [ `None | `Scroll | `Optional_paged | `Paged ]
  | Overflow_inline of [ `None | `Scroll ]
  | Prefers_reduced_motion of [ `No_preference | `Reduce ]
  | Prefers_reduced_transparency of [ `No_preference | `Reduce ]
  | Prefers_reduced_data of [ `No_preference | `Reduce ]
  | Prefers_contrast of [ `No_preference | `Less | `More | `Custom ]
  | Prefers_color_scheme of [ `Dark | `Light ]
  | Forced_colors of [ `Active | `None ]
  | Inverted_colors of [ `Inverted | `None ]
  | Pointer of [ `None | `Coarse | `Fine ]
  | Any_pointer of [ `None | `Coarse | `Fine ]
  | Hover of [ `None | `Hover ]
  | Any_hover of [ `None | `Hover ]
  | Scripting of [ `None | `Initial_only | `Enabled ]
  | Nav_controls of [ `None | `Back_button ]
  | Print
  | Orientation of [ `Portrait | `Landscape ]
  | And of t * t
  | Or of t * t
  | Negated of t
  | Range of string * cmp * value
  | Range_rev of value * cmp * string
  | Interval of value * cmp * string * cmp * value
  | Type_query of {
      prefix : prefix option;
      type_ : medium;
      trailing : t option;
    }
  | Plain of string * value
  | Boolean of string
  | List of t list

val of_string : string -> t
(** [of_string s] parses [s] as a media query. *)

val of_string_strict : string -> t
(** [of_string_strict s] parses [s] as a media query without branch recovery. *)

val of_function_body : string -> t
(** [of_function_body s] parses the body of a conditional [media(...)]
    function. Unlike a standalone media query, a single feature appears without
    its outer parentheses in this grammar. *)

val value_of_string : string -> value
(** [value_of_string s] parses [s] as a media-feature value. *)

val feature : string -> value -> t
(** [feature name v] is the plain feature [(name: v)]. *)

val boolean : string -> t
(** [boolean name] is the boolean feature [(name)]. *)

val to_string : t -> string
(** [to_string t] serialises [t] as CSS source text. *)

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
