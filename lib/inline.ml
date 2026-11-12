(** Closed-world inlining transforms (var() and \@import).

    [vars] is a typed substitution pass over {!Context} and {!Selector}. A
    custom property is visible to itself and to any rule whose effective
    selector descends from its own, within the same chain of
    [\@media]/[\@layer]/ [\@supports] blocks.

    [imports] inlines [\@import] rules from a closed [Context.loader] table.
    Layer/supports/media guards on the prelude are evaluated against [?query]
    and [?layer_order]; rejected imports drop, accepted ones lose the matched
    guard, and a repeat visit (cycle) is dropped. *)

include Inline_scope
include Inline_subst
include Inline_dead
include Inline_layer
include Inline_import

let vars ?(keep_vars = []) ?(inline_runtime = false) ?(warn = fun _ -> ())
    stylesheet =
  let _, _, runtime_refs = collect_scoped_refs stylesheet in
  (* A runtime reference is a variable the caller expects to change after the
     sheet is written, so it stays live - unless the caller asked for every
     variable the sheet defines resolved away. *)
  let runtime_refs = if inline_runtime then [] else runtime_refs in
  let keep =
    List.map normalise_var_name keep_vars @ runtime_refs
    |> List.sort_uniq compare
  in
  let stylesheet = collapse_layer_decided ~keep stylesheet in
  let counts, referenced = var_census stylesheet in
  let inlinable name =
    (not (List.mem name keep)) && Hashtbl.find_opt counts name = Some 1
  in
  (* Everything not inlinable stays live: the keep-set, plus any non-kept
     variable redefined across scopes. Those are folded by the substitution as
     if kept, so the single-def variables fold into them and get stripped. *)
  let kept =
    Hashtbl.fold
      (fun name _ acc -> if inlinable name then acc else name :: acc)
      counts keep
    |> List.sort_uniq compare
  in
  (* Warn for a non-kept variable kept only because it is redefined in a
     different scope, so the caller knows it escaped inlining. *)
  Hashtbl.iter
    (fun name count ->
      if count > 1 && (not (List.mem name keep)) && List.mem name referenced
      then warn name)
    counts;
  let scopes = collect_scopes ~kept stylesheet in
  let original_consumers, original_customs, _ =
    collect_scoped_refs stylesheet
  in
  let cyclic_live_set =
    cyclic_live_customs ~consumers:original_consumers ~customs:original_customs
  in
  let substituted =
    substitute ~kept ~scopes ~parents:[] ~at_path:[] stylesheet
  in
  let consumers, customs, _ = collect_scoped_refs substituted in
  let live_set = live_customs ~consumers ~customs @ cyclic_live_set in
  (* Keep every definition of a kept variable (including a cross-scope override
     like an @media one), so the live chain stays complete; single-def
     inlinables are not in [kept] and are stripped once folded. *)
  strip_dead ~keep:kept ~live_set substituted
