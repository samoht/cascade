CLI: `cascade diff` reading a stylesheet from standard input.

Diffing what a build just produced against a reference is what this command
is for, and a pipe is how a build hands it over, so `-` names the stream on
standard input for either side. The report calls that side `<stdin>`, the
name the rest of the CLI already gives it.

  $ cat > ref.css <<EOF
  > .a{color:red}
  > EOF

The expected side read from the stream.

  $ printf '.a{color:blue}\n' | NO_COLOR=1 cascade diff - ref.css
  CSS: 15 chars vs 14 chars (6.7% diff)
  Changes: 1 modified rule
  
  --- <stdin>
  +++ ref.css
  └─ .a
        * color: blue -> red
  
  [1]

The actual side read from the stream.

  $ printf '.a{color:blue}\n' | NO_COLOR=1 cascade diff ref.css -
  CSS: 14 chars vs 15 chars (7.1% diff)
  Changes: 1 modified rule
  
  --- ref.css
  +++ <stdin>
  └─ .a
        * color: red -> blue
  
  [1]

A stream that matches the file it is compared against reports identity.

  $ printf '.a{color:red}\n' | cascade diff - ref.css
  CSS files are identical

Standard input cannot be read twice, so `-` on both sides is a command-line
error rather than a stream compared with itself.

  $ printf '.a{color:red}\n' | cascade diff - - 2>&1
  cascade: cannot compare standard input with itself
  [124]

Every other argument still has to name something that exists.

  $ NO_COLOR=1 cascade diff missing.css ref.css 2> err.txt
  [124]
  $ grep -c "no 'missing.css' file or directory" err.txt
  1
