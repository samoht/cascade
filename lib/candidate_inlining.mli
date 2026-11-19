(** Selector-branch inlining for {!Rule_candidate}.

    A receiver with one selector branch and a group listing that branch meet on
    the branch key, and are a candidate when the receiver precedes the group.
    The graph pins both in source order, so the answer is settled when they meet
    and only their own rewrite changes it. *)

include Candidate_kind.S

val v : int -> t
(** [v size] has filed no rule; [size] sizes its tables. *)
