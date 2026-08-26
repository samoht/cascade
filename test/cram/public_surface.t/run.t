The compact public surface keeps the conventional names callers need:

  $ ocamlfind ocamlc -package cascade -c retained.ml

Compatibility aliases and one-off helpers are not part of the public API. Each
name is compiled separately so that one missing name cannot hide another that
was accidentally retained:

  $ status=0
  > while IFS= read -r value; do
  >   printf 'open Cascade\nlet _ = %s\n' "$value" > probe.ml
  >   if ocamlfind ocamlc -package cascade -c probe.ml >/dev/null 2>&1; then
  >     echo "still public: $value"
  >     status=1
  >   fi
  > done < removed_values
  > for probe in old_container_pp.ml old_stylesheet_pp.ml old_css_pp.ml; do
  >   if ocamlfind ocamlc -package cascade -c "$probe" >/dev/null 2>&1; then
  >     echo "old string printer still public: $probe"
  >     status=1
  >   fi
  > done
  > if ocamlfind ocamlc -package cascade -c removed_box_shadow_kind.ml \
  >      >/dev/null 2>&1; then
  >   echo "still public: Css.Box_shadow as a shadow kind"
  >   status=1
  > fi
  > test $status -eq 0
