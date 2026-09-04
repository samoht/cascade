(** The committed Lightning trace: one record per input CSS, carrying each
    oracle tool's cached answer and the tools that produced none.

    The format is a length-prefixed binary stream written by
    [../scripts/generate.sh]; harnesses read records, never the bytes. *)

type candidate = { tool : string; css : string }
type failed_candidate = { tool : string; command : string; reason : string }

type t = {
  input : string;
  candidates : candidate list;
  failures : failed_candidate list;
}

val read : string -> t list
(** [read path] is the records of the trace at [path], in file order. A record
    the format does not account for raises [Failure] or [Scanf.Scan_failure]. *)

val pp : Format.formatter -> t -> unit
(** [pp] prints a record over several lines: the input, then one line per cached
    answer and per tool that failed. *)
