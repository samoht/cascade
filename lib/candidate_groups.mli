(** Identical-body and same-selector merges for {!Rule_candidate}.

    Rules with one body, or one selector, form a bucket, and the candidates of a
    bucket read its members' rules and nothing else. Under the transfer
    objective a body's members merge in runs within {!Subsets.max_span} printed
    rules, so every refresh splits those buckets again. *)

type grouping =
  | Bodies  (** rules with one body merge under one selector list *)
  | Selectors  (** rules with one selector merge into one body *)

include Candidate_kind.S

val v : int -> grouping -> t
(** [v size grouping] has filed no rule; [size] sizes its tables. *)
