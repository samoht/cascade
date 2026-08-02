CLI: cascade diff - containers nested several levels deep.

A leaf difference under five at-rules is still a difference, and the
report walks down to it rather than stopping partway and labelling the
node it stopped at.

  $ cat > deep-a.css <<EOF
  > @media (min-width:1px){@supports (display:grid){@media (min-width:2px){@supports (display:flex){@media (min-width:3px){a{color:red}}}}}}
  > EOF
  $ cat > deep-b.css <<EOF
  > @media (min-width:1px){@supports (display:grid){@media (min-width:2px){@supports (display:flex){@media (min-width:3px){a{color:blue}}}}}}
  > EOF
  $ NO_COLOR=1 cascade diff --diff=tree --depth=max deep-a.css deep-b.css
  CSS: 137 chars vs 138 chars (0.7% diff)
  Changes: 1 changed container
  
  --- deep-a.css
  +++ deep-b.css
  └─ @media (min-width: 1px) 
     └─ @supports (display: grid) 
        └─ @media (min-width: 2px) 
           └─ @supports (display: flex) 
              └─ @media (min-width: 3px) (1 modified)
                 └─ a
                       * color: red -> blue
  
  [1]



The same nesting with no leaf change stays identical.

  $ cascade diff --diff=tree deep-a.css deep-a.css
  CSS files are identical
