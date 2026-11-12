CLI: a number too large for an OCaml int keeps its value.

An integer-valued float at or past 2^62 has no `int`: `int_of_float`
returns `min_int` at 2^62 and an unspecified value above it (-1 on
arm64, 0 on x86_64). Every place that turns a CSS number into an `int`
answers for that range: it prints the number as a float, keeps the math
function that produced it, or leaves the text as written.

A length or a number printed by the minifier.

  $ printf 'a{width:4611686018427387904px}' | cascade fmt - 2>&1
  a {
    width: 4611686018427387904px;
  }
  $ printf 'a{width:4611686018427387904px}' | cascade fmt --minify - 2>&1
  a{width:4.61168601843e18px}
  $ printf 'a{opacity:4611686018427387904}' | cascade fmt - 2>&1
  a {
    opacity: 4.61168601843e18;
  }
  $ printf 'a{opacity:4611686018427387904}' | cascade fmt --minify - 2>&1
  a{opacity:4.61168601843e18}

A fraction whose digits, scaled to the printed precision, reach 2^62.

  $ printf 'a{width:46116860184.27387904px}' | cascade fmt - 2>&1
  a {
    width: 46116860184.27387904px;
  }
  $ printf 'a{width:46116860184.27387904px}' | cascade fmt --minify - 2>&1
  a{width:46116860184.3px}

A media feature length, and a resolution.

  $ printf '@media (min-width:4611686018427387904px){a{color:red}}' | cascade fmt - 2>&1
  @media (min-width: 4.61168601843e18px) {
    a {
      color: red;
    }
  }
  $ printf '@media (min-width:4611686018427387904px){a{color:red}}' | cascade fmt --minify - 2>&1
  @media(width>=4.61168601843e18px){a{color:red}}
  $ printf '@media (min-resolution:1e30dpi){a{color:red}}' | cascade fmt - 2>&1
  @media (min-resolution: 1e30dpi) {
    a {
      color: red;
    }
  }
  $ printf '@media (min-resolution:1e30dpi){a{color:red}}' | cascade fmt --minify - 2>&1
  @media(resolution>=1e30dpi){a{color:red}}

A ratio whose terms have no `int`, or whose numerator is not an
integer, stays as written.

  $ printf '@media (aspect-ratio:100000000000000000000/1){a{color:red}}' | cascade fmt - 2>&1
  @media (aspect-ratio:100000000000000000000/1) {
    a {
      color: red;
    }
  }
  $ printf '@media (aspect-ratio:100000000000000000000/1){a{color:red}}' | cascade fmt --minify - 2>&1
  @media(aspect-ratio:100000000000000000000/1){a{color:red}}
  $ printf '@media (aspect-ratio:1.5/1){a{color:red}}' | cascade fmt - 2>&1
  @media (aspect-ratio:1.5/1) {
    a {
      color: red;
    }
  }
  $ printf '@media (aspect-ratio:1.5/1){a{color:red}}' | cascade fmt --minify - 2>&1
  @media(aspect-ratio:1.5/1){a{color:red}}

A container width.

  $ printf '@container (min-width:1e30px){a{color:red}}' | cascade fmt - 2>&1
  @container (min-width: 1e30px) {
    a {
      color: red;
    }
  }
  $ printf '@container (min-width:1e30px){a{color:red}}' | cascade fmt --minify - 2>&1
  @container(width>=1e30px){a{color:red}}

An attribute value written as a number is no value at all: Selectors 4
sec. 6.1 takes an ident or a string there, and Chrome drops the rule
rather than read the number as a string. So does cascade, with a warning.

  $ printf '[data-x=1e30]{color:red}.b{color:green}' | cascade fmt - 2>/dev/null
  .b {
    color: green;
  }
  $ printf '[data-x=1e30]{color:red}.b{color:green}' | cascade fmt --minify - 2>/dev/null
  .b{color:green}
  $ printf '[data-x=1e30]{color:red}.b{color:green}' | cascade fmt --minify - 2>&1 >/dev/null | grep -c 'attribute value'
  1

A keyframe timeline range offset.

  $ printf '@keyframes k{entry 1e30%%{color:red}}' | cascade fmt - 2>&1
  @keyframes k {
    entry 1e30% {
      color: red;
    }
  }
  $ printf '@keyframes k{entry 1e30%%{color:red}}' | cascade fmt --minify - 2>&1
  @keyframes k{entry 1e30%{color:red}}

A math function at an `<integer>` slot. CSS Values 4 sec. 10.12 rounds
it and clamps the result to the supported range at computed-value time,
so a call that rounds past 2^62 keeps its wrapper.

  $ printf 'a{z-index:calc(1e30)}' | cascade fmt - 2>&1
  a {
    z-index: calc(1e30);
  }
  $ printf 'a{z-index:calc(1e30)}' | cascade fmt --minify - 2>&1
  a{z-index:calc(1e30)}
  $ printf 'a{column-count:calc(1e30)}' | cascade fmt - 2>&1
  a {
    column-count: calc(1e30);
  }
  $ printf 'a{column-count:calc(1e30)}' | cascade fmt --minify - 2>&1
  a{column-count:calc(1e30)}
  $ printf 'a{columns:calc(1e30)}' | cascade fmt - 2>&1
  a {
    columns: calc(1e30);
  }
  $ printf 'a{columns:calc(1e30)}' | cascade fmt --minify - 2>&1
  a{columns:calc(1e30)}
  $ printf 'a{-webkit-line-clamp:calc(1e30)}' | cascade fmt - 2>&1
  a {
    -webkit-line-clamp: calc(1e30);
  }
  $ printf 'a{-webkit-line-clamp:calc(1e30)}' | cascade fmt --minify - 2>&1
  a{-webkit-line-clamp:calc(1e30)}

A grid line index has to be non-zero, and a call that clamps to the
range is: it is not the zero line.

  $ printf 'a{grid-row:calc(-1e30)}' | cascade fmt - 2>&1
  a {
    grid-row: calc(-1e30);
  }
  $ printf 'a{grid-row:calc(-1e30)}' | cascade fmt --minify - 2>&1
  a{grid-row:calc(-1e30)}
