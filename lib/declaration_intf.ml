(** Declaration interface types *)

open Properties_intf

type 'a kind = 'a Properties_intf.kind

(* [hash] is a structural fingerprint of the declaration and [size] the length
   of its minified rendering, both fixed at construction. Computed eagerly by
   the [Declaration.v] / [Declaration.theme_guarded] smart constructors -- every
   code path that builds a declaration goes through them, so the fields stay
   consistent with the [(property, value, important)] / [(var_name, decl)]
   payload and equal declarations are interned. *)
type declaration =
  | Declaration : {
      property : 'a property;
      value : 'a;
      important : bool;
      hash : int;
      size : int;
    }
      -> declaration
  | Theme_guarded : {
      var_name : string;
      decl : declaration;
      hash : int;
    }
      -> declaration

type t = declaration
