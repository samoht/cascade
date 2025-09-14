(** CSS optimization utilities *)

open Declaration
open Stylesheet

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

val single_rule : rule -> rule
(** [single_rule rule] deduplicates declarations in one rule. *)

val merge_rules : rule list -> rule list
(** [merge_rules rules] merges adjacent rules with identical selectors while
    preserving cascade order. *)

val combine_identical_rules : rule list -> rule list
(** [combine_identical_rules rules] combines consecutive rules with identical
    declarations into comma-separated selectors. *)

val rules : rule list -> rule list
(** [rules rs] optimizes a list of flat rules. *)

(** {1 Nested Structure Optimization} *)

(** {1 Stylesheet Optimization} *)

val stylesheet : t -> t
(** [stylesheet ss] optimizes an entire stylesheet while preserving cascade
    semantics. When [@supports] blocks are present alongside top-level rules,
    optimization is limited because the stylesheet structure separates rules
    from [@supports] blocks, losing their relative ordering. *)
