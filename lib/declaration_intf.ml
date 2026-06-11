(** Declaration interface types *)

open Properties_intf

type 'a kind = 'a Properties_intf.kind

(* [hash] is a structural fingerprint of the declaration, fixed at construction.
   The optimizer's inner-loop equality ([Optimize.same_minified_declaration])
   consults it so an inequality verdict is one field load + int compare instead
   of walking the value AST. Computed eagerly by the [Declaration.v] /
   [Declaration.theme_guarded] smart constructors -- every code path that builds
   a declaration must go through them so the field stays consistent with the
   [(property, value, important)] / [(var_name, decl)] payload. *)
type declaration =
  | Declaration : {
      property : 'a property;
      value : 'a;
      important : bool;
      hash : int;
    }
      -> declaration
  | Theme_guarded : {
      var_name : string;
      decl : declaration;
      hash : int;
    }
      -> declaration

type t = declaration
