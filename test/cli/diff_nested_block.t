CLI: cascade diff - a rule's nested block is named as a block.

The container is the block of a rule, not a selector, so it is named after the
rule it belongs to. Prefixing it the way an at-rule container is named prints
"& .a", which is a valid selector matching a ".a" inside the parent.

  $ cat > nest-a.css <<'EOF'
  > .a{color:red;& b{color:blue}}
  > EOF
  $ cat > nest-b.css <<'EOF'
  > .a{color:red;& b{color:green}}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree nest-a.css nest-b.css
  CSS: 30 chars vs 31 chars (3.3% diff)
  Changes: 1 changed container
  
  --- nest-a.css
  +++ nest-b.css
  └─ .a { & } (1 modified)
     └─ & b
           * color: blue -> green
  
  [1]

A run of declarations written after a nested statement is the nested
declarations rule of CSS Nesting 1 sec. 3.4, which acts as "&". It has no
head, so naming it by the text up to its first brace puts its first
declaration in the selector column.

  $ cat > run-a.css <<'EOF'
  > .a{color:red;& b{color:blue}}
  > EOF
  $ cat > run-b.css <<'EOF'
  > .a{& b{color:blue}color:red}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree run-a.css run-b.css
  CSS: 30 chars vs 29 chars (3.3% diff)
  Changes: 1 modified rule, 1 changed container
  
  --- run-a.css
  +++ run-b.css
  ├─ .a
  │     - color: red
  └─ .a { & } (1 added)
     └─ &
           + color: red
  
  [1]
