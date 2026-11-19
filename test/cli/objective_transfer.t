The transfer objective weighs a rewrite by what it saves once the output is
compressed, not by its raw bytes.

Twenty sprite rules share three declarations. Factoring them out of the
sprites shrinks the raw output by two fifths, but gzip encodes each sprite's
copy of the three declarations as one match against the sprite before it,
and the factoring prints every sprite's selector a second time, once in the
group's list and once in the rule it leaves behind. Under the transfer
objective the output is no larger under gzip than the sheet as written.

  $ for i in $(seq 0 19); do
  >   if [ "$i" = 0 ]; then p='0 0'; else p="-$((i * 16))px 0"; fi
  >   printf '.icon-%s{background-position:%s;display:inline-block;width:16px;height:16px}' "$i" "$p"
  > done > sprites.css
  $ echo >> sprites.css
  $ cascade --minify sprites.css > transfer.css
  $ test "$(gzip -9 -c < transfer.css | wc -c)" -le "$(gzip -9 -c < sprites.css | wc -c)" && echo no larger
  no larger

The raw objective factors them.

  $ cascade --minify --objective=raw sprites.css | grep -o 'display:inline-block' | grep -c .
  1
