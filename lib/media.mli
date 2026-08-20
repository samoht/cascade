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

(** [env(--name)] / [var(...)] / [calc(...)] etc. captured as a function name
    plus its raw argument body. *)
type value =
  | Length of Values.length
  | Integer of int
  | Number of float
  | Ratio of int * int
  | Resolution_value of float * string
  | Ident of ident
  | Function of string * string

val equal_value : value -> value -> bool
(** [equal_value a b] tests media feature values structurally. *)

(** Media Queries 4 sec. 3.1 [<general-enclosed>]: a grammatical but
    unrecognised query, kept verbatim. Its result is [unknown], which becomes
    false wherever a boolean is expected. *)
type feature =
  | Plain of name * value
  | Boolean of name
  | Range of name * cmp * value
  | Range_rev of value * cmp * name
  | Interval of value * cmp * name * cmp * value
  | General_enclosed of string

type condition =
  | Feature of feature
  | Not of condition
  | And of condition * condition
  | Or of condition * condition

type medium = All | Screen | Print | Other of string
type prefix = Not | Only

val equal_name : name -> name -> bool
(** [equal_name a b] tests media feature names for equality. *)

(** Comma-separated media query list. *)
type t =
  | Cond of condition
  | Type of {
      prefix : prefix option;
      type_ : medium;
      trailing : condition option;
    }
  | List of t list

val of_string : string -> t
(** [of_string s] parses [s] as a media query. *)

val of_string_strict : string -> t
(** [of_string_strict s] parses [s] as a media query without branch recovery. *)

val of_components : ?recover:bool -> Component.t list -> t
(** [of_components components] parses an already-tokenized media query. Invalid
    branches recover to [not all] unless [recover] is [false]. *)

val of_function_body : string -> t
(** [of_function_body s] parses the body of a conditional [media(...)] function.
    Unlike a standalone media query, a single feature appears without its outer
    parentheses in this grammar. *)

val of_function_components : Component.t list -> t
(** [of_function_components components] parses an already-tokenized conditional
    [media(...)] body. *)

val value_of_string : string -> value
(** [value_of_string s] parses [s] as a media-feature value. *)

val name_of_string : string -> name
(** [name_of_string s] parses a media feature name. *)

val string_of_name : name -> string
(** [string_of_name name] serializes a media feature name. *)

val ident_of_string : string -> ident
(** [ident_of_string s] parses a media identifier value. *)

val string_of_ident : ident -> string
(** [string_of_ident ident] serializes a media identifier value. *)

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

val lower_for_minify : t -> t
(** [lower_for_minify t] applies the target-fact grammar upgrades used under
    minify: [min-X]/[max-X] plain features become the range form [X>=V]/[X<=V],
    and a lower bound paired with an upper bound on the same feature across an
    [and] collapses into the two-sided interval [V<=name<=V]. *)

val pp_condition : condition Pp.t
(** Pretty-printer for media query conditions. *)

val pp_feature : feature Pp.t
(** Pretty-printer for media features. *)

val compare : t -> t -> int
(** Total order on media queries. *)

val equal : t -> t -> bool
(** Structural equality on media queries. *)

type key
(** A precomputed sort key for {!val-compare}. Deriving the order serializes the
    query and re-extracts its kind, both of which allocate; a sort that compares
    raw queries pays that on every comparison. Precompute a [key] once per query
    with {!sort_key} and compare with {!compare_keys}. *)

val sort_key : t -> key
(** [sort_key t] precomputes the components {!compare} derives from [t]. *)

val compare_keys : key -> key -> int
(** [compare_keys k1 k2] orders two queries by their precomputed keys, with no
    allocation. [compare_keys (sort_key a) (sort_key b) = compare a b]. *)

val sort_by : ('a -> t) -> 'a list -> 'a list
(** [sort_by project items] orders [items] by the media query [project] returns
    for each, serializing each query exactly once. Sorting this way, rather than
    with [List.sort (fun a b -> compare (project a) (project b))], is the point
    of {!sort_key}: the comparator form re-derives and re-serializes a query on
    every comparison, which a sort does O(n log n) times. The sort is stable. *)

type kind =
  | Hover
  | Responsive of int * float
  | Responsive_max of int * float
  | Preference_accessibility
  | Preference_appearance
  | Other

val equal_kind : kind -> kind -> bool
(** [equal_kind a b] tests media query categories for equality. *)

val kind : t -> kind
(** [kind t] classifies [t] for grouping and ordering. *)

val group_order : kind -> int * float
(** [group_order k] is the sort key used to group queries by {!val-kind}. *)

val preference_order : t -> int
(** [preference_order t] orders preference queries within their group. *)
