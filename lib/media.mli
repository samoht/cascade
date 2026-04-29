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
val value_of_string : string -> value
val feature : string -> value -> t
val boolean : string -> t
val to_string : t -> string
val pp : t Pp.t
val pp_query : query Pp.t
val pp_condition : condition Pp.t
val pp_feature : feature Pp.t
val compare : t -> t -> int
val equal : t -> t -> bool

type kind =
  | Hover
  | Responsive of int * float
  | Responsive_max of int * float
  | Preference_accessibility
  | Preference_appearance
  | Other

val kind : t -> kind
val group_order : kind -> int * float
val preference_order : t -> int
