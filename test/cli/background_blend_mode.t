CLI: cascade --minify - the background shorthand and background-blend-mode.

Compositing and Blending 1 sec. 3.4.3 has the [background] shorthand reset
[background-blend-mode], but Chrome 157, Chromium 151 and WebKit 26.5 do not:
after [background-blend-mode:multiply;background:red] each still computes
[multiply]. So a later [background] does not make an earlier
[background-blend-mode] dead, and dropping it would stop the layers blending.

  $ printf '.a{background-blend-mode:multiply;background:red}' | cascade --minify -
  .a{background-blend-mode:multiply;background:red}
  $ printf '.a{background-blend-mode:multiply}.a{background:red}' | cascade --minify -
  .a{background-blend-mode:multiply;background:red}

The comparator agrees: the two sides blend differently.

  $ printf '.a{background-blend-mode:multiply;background:red}' > a.css
  $ printf '.a{background:red}' > b.css
  $ cascade diff --diff=canonical a.css b.css > /dev/null
  [1]

In an engine that follows the specification the shorthand does reset it, so
[background] still keeps its place after a [background-blend-mode] another
rule writes, where a [background-color] may move past it.

  $ printf '.a{color:blue}.b{background-blend-mode:multiply}.a{background:red}' | cascade --minify -
  .b{background-blend-mode:multiply}.a{background:red;color:#00f}
  $ printf '.a{color:blue}.b{background-blend-mode:multiply}.a{background-color:red}' | cascade --minify -
  .a{background-color:red;color:#00f}.b{background-blend-mode:multiply}

Under --enforce-spec cascade relies only on what the specifications
guarantee, and Compositing and Blending 1 (ED) sec. 3.4.3 has the shorthand
reset the blend mode, so a later [background] makes it dead: it is dropped,
and the comparator calls the two sides equal.

  $ printf '.a{background-blend-mode:multiply;background:red}' | cascade --minify --enforce-spec -
  .a{background:red}
  $ printf '.a{background-blend-mode:multiply}.a{background:red}' | cascade --minify --enforce-spec -
  .a{background:red}
  $ cascade diff --diff=canonical --enforce-spec a.css b.css
  CSS files are identical

The order against a blend mode another rule writes is the same in both
modes.

  $ printf '.a{color:blue}.b{background-blend-mode:multiply}.a{background:red}' | cascade --minify --enforce-spec -
  .b{background-blend-mode:multiply}.a{background:red;color:#00f}
  $ printf '.a{color:blue}.b{background-blend-mode:multiply}.a{background-color:red}' | cascade --minify --enforce-spec -
  .a{background-color:red;color:#00f}.b{background-blend-mode:multiply}
