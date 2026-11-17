A declaration's cached structural hash and minified size are implementation
details.  Public clients can inspect declarations, but cannot construct one
with arbitrary cache values instead of using the smart constructors.

  $ cat > forge_hash.ml <<'EOF'
  > open Cascade
  > let forged =
  >   match Declaration.of_string "color:currentColor" with
  >   | Ok (Declaration.Declaration { property; value; important; _ }) ->
  >       Declaration.Declaration { property; value; important; hash = 0; size = 0 }
  >   | Ok (Declaration.Theme_guarded _) | Error _ -> assert false
  > EOF
  $ ocamlfind ocamlc -package cascade -c forge_hash.ml >/dev/null 2>&1
  [2]
