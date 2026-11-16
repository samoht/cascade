(** The index that finds the rules that may conflict with one another. *)

open Stdlib

(* Packed specificities, so the bucket index is keyed by an immediate rather
   than by a record a generic table would hash and compare field by field. *)
module Spec_table = Common.Table.Make (struct
  type t = int

  let equal = Int.equal
  let hash = Hashtbl.hash
end)

module Overlap_key_table = Hashtbl.Make (struct
  type t = Shorthand.overlap_key

  let equal = Shorthand.overlap_key_equal
  let hash = Shorthand.overlap_key_hash
end)

(* A cascade conflict needs two selector branches of equal specificity, so a
   candidate bucket is keyed by specificity as well as by overlap key: rules
   that write the same property at specificities that can never tie are not
   enumerated at all, which is four candidate pairs in five of a real
   stylesheet. The three components count simple selectors and sit far below the
   20-bit field; one past it saturates, which can only merge two specificities
   into one bucket, never split one. *)
let spec_field = 0xFFFFF
let spec_component x = if x > spec_field then spec_field else x

let spec_key (s : Selector.specificity) =
  (spec_component s.ids lsl 40)
  lor (spec_component s.classes lsl 20)
  lor spec_component s.elements

let rec spec_key_mem (key : int) = function
  | [] -> false
  | k :: rest -> key = k || spec_key_mem key rest

(* The distinct specificities of a node's branches. A rule has a handful of
   branches, so the linear membership test beats sorting them. *)
let rec distinct_spec_keys acc = function
  | [] -> acc
  | spec :: rest ->
      let key = spec_key spec in
      distinct_spec_keys (if spec_key_mem key acc then acc else key :: acc) rest

let spec_bucket bucket key =
  match Overlap_key_table.find_opt bucket key with
  | Option.Some inner -> inner
  | Option.None ->
      let inner = Spec_table.create 4 in
      Overlap_key_table.replace bucket key inner;
      inner

let rec add_spec_buckets bucket value = function
  | [] -> ()
  | spec :: rest ->
      Spec_table.push bucket spec value;
      add_spec_buckets bucket value rest

let add_bucket bucket key specs value =
  add_spec_buckets (spec_bucket bucket key) value specs

(* Values, each at a position, bucketed by what two rules need to share to
   conflict: a branch specificity, and an overlap key or a broad footprint on
   either side. A query reads only the buckets the asking rule names, so it
   tests the values that could conflict with it rather than the whole run.
   [tested] holds the stamp of the last query that tested a value, since one
   value sits in a bucket per key and per specificity. *)
type 'a entry = { at : int; value : 'a; mutable tested : int }

type 'a positioned = {
  by_key : 'a entry list Spec_table.t Overlap_key_table.t;
  by_spec : 'a entry list Spec_table.t;
  mutable stamp : int;
}

let positioned () =
  {
    by_key = Overlap_key_table.create 64;
    by_spec = Spec_table.create 16;
    stamp = 0;
  }

let add_at index ~at ~specs ~keys value =
  let entry = { at; value; tested = 0 } in
  add_spec_buckets index.by_spec entry specs;
  List.iter (fun key -> add_bucket index.by_key key specs entry) keys

(* A bucket lists its values newest first, so the walk stops at the first value
   at or before [after]. *)
let rec found_in ~after test stamp = function
  | e :: rest when e.at > after ->
      (not (Int.equal e.tested stamp))
      &&
      (e.tested <- stamp;
       test e.value)
      || found_in ~after test stamp rest
  | _ -> false

let found_in_specs ~after test stamp table specs =
  List.exists
    (fun spec ->
      match Spec_table.find_opt table spec with
      | Option.None -> false
      | Option.Some entries -> found_in ~after test stamp entries)
    specs

(* A rule whose footprint is broad overlaps every key, so it reads every value
   of a shared specificity; any other reads its own keys and the broad one. *)
let exists_after index ~after ~specs ~keys test =
  index.stamp <- index.stamp + 1;
  let stamp = index.stamp in
  if Shorthand.overlap_keys_broad keys then
    found_in_specs ~after test stamp index.by_spec specs
  else
    List.exists
      (fun key ->
        match Overlap_key_table.find_opt index.by_key key with
        | Option.None -> false
        | Option.Some table -> found_in_specs ~after test stamp table specs)
      (Shorthand.broad_overlap_key :: keys)
