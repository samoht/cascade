(** CSS optimization interface types *)

(** {1 Types} *)

type layer_stats = {
  name : string;
  rules : int;
  selectors : string list;  (** First few selectors as examples *)
}

(** {1 Optimization Configuration} *)

type config = {
  deduplicate : bool;  (** Remove duplicate declarations *)
  merge_adjacent : bool;  (** Merge adjacent rules with same selector *)
  combine_selectors : bool;  (** Combine rules with identical declarations *)
}
