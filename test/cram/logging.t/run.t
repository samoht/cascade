Optimizer logging
=================

The CLI accepts a source-specific log level, so a normal minification can
surface the optimizer's summary without enabling unrelated debug records.

  $ cascade fmt --minify --log=cascade.optimize:debug input.css >output.css 2>log

  $ cat output.css
  .a{color:red}

  $ sed -E 's/[0-9]+/N/g' log
  cascade: [DEBUG] optimized: N factoring fixpoints run, N skipped, N reverted by the transfer gate, N bytes saved
