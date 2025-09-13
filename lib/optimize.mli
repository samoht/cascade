(** CSS optimization utilities *)

open Declaration
open Stylesheet
include module type of Optimize_intf

(** {1 Declaration Optimization} *)

val duplicate_buggy_properties : declaration list -> declaration list
(** [duplicate_buggy_properties decls] duplicates known buggy properties for
    browser compatibility. Some WebKit properties need to be duplicated for
    older Safari versions. See: https://bugs.webkit.org/show_bug.cgi?id=101180.
*)

val deduplicate_declarations : declaration list -> declaration list
(** [deduplicate_declarations decls] removes overridden declarations following
    CSS cascade rules: !important wins over normal, and among same importance
    the last one wins. *)

(** {1 Rule Optimization} *)

val optimize_single_rule : rule -> rule
(** [optimize_single_rule rule] deduplicates declarations in one rule. *)

val merge_rules : rule list -> rule list
(** [merge_rules rules] merges adjacent rules with identical selectors while
    preserving cascade order. *)

val combine_identical_rules : rule list -> rule list
(** [combine_identical_rules rules] combines consecutive rules with identical
    declarations into comma-separated selectors. *)

val optimize_rule_list : rule list -> rule list
(** [optimize_rule_list rules] optimizes a list of flat rules. *)

(** {1 Nested Structure Optimization} *)

val optimize_nested_rules : nested_rule list -> nested_rule list
(** [optimize_nested_rules rules] optimizes nested rules while preserving order.
*)

val optimize_layer : layer_rule -> layer_rule
(** [optimize_layer layer] optimizes a layer rule. *)

val optimize_media_rule : media_rule -> media_rule
(** [optimize_media_rule rule] optimizes a media rule. *)

val optimize_container_rule : container_rule -> container_rule
(** [optimize_container_rule rule] optimizes a container rule. *)

val optimize_supports_rule : supports_rule -> supports_rule
(** [optimize_supports_rule rule] optimizes a supports rule (including nested).
*)

(** {1 Stylesheet Optimization} *)

val optimize : t -> t
(** [optimize stylesheet] optimizes an entire stylesheet while preserving
    cascade semantics. When [@supports] blocks are present alongside top-level
    rules, optimization is limited because the stylesheet structure separates
    rules from [@supports] blocks, losing their relative ordering. *)
