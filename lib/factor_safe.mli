(** Cascade legality checks shared by optimization factoring passes. *)

type t
(** Configured oracle. The callbacks capture optimizer-local policy that is not
    owned by this module, such as shorthand coverage and vendor-pseudo
    boundaries. *)

type summary
(** Minimal rule summary needed by the legality checks. *)

val v :
  same_minified_declaration:(Declaration.t -> Declaration.t -> bool) ->
  declaration_covers:(Declaration.t -> Declaration.t -> bool) ->
  contains_vendor_pseudo_element:(Selector.t -> bool) ->
  rule_factor_boundary:(Stylesheet.rule -> bool) ->
  decl_property:(Declaration.t -> Declaration.prop_key) ->
  t
(** [v ~same_minified_declaration ~declaration_covers
     ~contains_vendor_pseudo_element ~rule_factor_boundary ~decl_property]
    builds a legality oracle from optimizer-owned policy callbacks. *)

val summary : Stylesheet.rule -> selectors:Selector_summary.t Lazy.t -> summary
(** [summary rule ~selectors] stores the rule and its lazy selector summary. *)

val rule : summary -> Stylesheet.rule
(** [rule summary] returns the summarized rule. *)

val overlap : t -> Declaration.t list -> Declaration.t list -> bool
(** [overlap t common decls] is true when [decls] can change a property in
    [common] for the same importance bucket. *)

val selector_overlap : Stylesheet.rule -> Selector_summary.t -> bool
(** [selector_overlap rule target] is true when [rule]'s selector may match an
    element represented by [target]. *)

val specificity_ties : Stylesheet.rule -> Stylesheet.rule -> bool
(** [specificity_ties a b] is true when overlapping selectors from [a] and [b]
    can have equal specificity. *)

val blocks_factor : t -> Declaration.t list -> summary -> summary -> bool
(** [blocks_factor t common target skipped] is true when [skipped] prevents
    factoring [common] out of [target]. *)

val blocks_tie : t -> Declaration.t list -> summary -> summary -> bool
(** [blocks_tie t common target skipped] is true when [skipped] ties [target] on
    specificity while touching [common]. *)

val can_cross : t -> Declaration.t list option -> Stylesheet.rule -> bool
(** [can_cross t common rule] is true when a factor scan can move across [rule].
    [None] means the scan has no common declaration set yet. *)
