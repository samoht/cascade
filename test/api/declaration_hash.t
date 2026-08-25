A declaration's cached structural hash is an implementation detail.  Public
clients can inspect declarations, but cannot construct one with an arbitrary
cache value instead of using the smart constructors.

  $ cat > forge_hash.ml <<'EOF'
  > open Cascade
  > let forged =
  >   match Declaration.of_string "color:currentColor" with
  >   | Declaration.Declaration { property; value; important; _ } ->
  >       Declaration.Declaration { property; value; important; hash = 0 }
  >   | Declaration.Theme_guarded _ -> assert false
  > EOF
  $ ocamlfind ocamlc -package cascade -c forge_hash.ml >/dev/null 2>&1
  [2]
