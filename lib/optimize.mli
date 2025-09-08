(** CSS optimization utilities *)

open Declaration
open Stylesheet
include module type of Optimize_intf

(** {1 Declaration Optimization} *)

val duplicate_buggy_properties : declaration list -> declaration list
(** Duplicate known buggy properties for browser compatibility. Some webkit
    properties need to be duplicated for older WebKit/Safari versions. See:
    https://bugs.webkit.org/show_bug.cgi?id=101180 *)

val deduplicate_declarations : declaration list -> declaration list
(** Deduplicate declarations following CSS cascade rules: 1. !important
    declarations always win over normal declarations 2. Among declarations of
    same importance, last one wins *)

(** {1 Rule Optimization} *)

val optimize_single_rule : rule -> rule
(** Optimize a single rule by deduplicating its declarations *)

val merge_rules : rule list -> rule list
(** Merge adjacent rules with identical selectors while preserving cascade
    order. Only merges truly adjacent rules to maintain CSS semantics. *)

val combine_identical_rules : rule list -> rule list
(** Combine consecutive rules with identical declarations into comma-separated
    selectors. Only combines consecutive rules to preserve cascade semantics. *)

val optimize_rule_list : rule list -> rule list
(** Optimize a list of plain CSS rules *)

(** {1 Nested Structure Optimization} *)

val optimize_nested_rules : nested_rule list -> nested_rule list
(** Optimize nested rules (Rule | Supports) while preserving order *)

val optimize_layer : layer_rule -> layer_rule
(** Optimize a layer_rule *)

val optimize_media_rule : media_rule -> media_rule
(** Optimize a media rule *)

val optimize_container_rule : container_rule -> container_rule
(** Optimize a container rule *)

val optimize_supports_rule : supports_rule -> supports_rule
(** Optimize a supports rule (including nested supports) *)

(** {1 Stylesheet Optimization} *)

val optimize : t -> t
(** Optimize an entire stylesheet while preserving cascade semantics. When
    [@supports] blocks are present alongside top-level rules, optimization is
    limited because the stylesheet structure separates rules from [@supports]
    blocks, losing their relative ordering. *)
