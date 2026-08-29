CLI: outline keeps a <line-width> keyword or comparison.

CSS Basic User Interface 4 (ED) sec. 3.2 gives outline-width the value
<line-width> and says it accepts the same values as border-width with the
same meaning, and sec. 3.1 makes the outline shorthand's width slot
<'outline-width'>. Reading that slot as a bare <length> made thin/medium/thick
and min()/max()/clamp() fall through to the colour reader, and CSS Syntax 3
sec. 8.2 then dropped the whole declaration: valid outlines disappeared from
the output.

  $ cat > outline.css <<CSS
  > a { outline: thin solid red; color: blue }
  > b { outline-width: thick }
  > c { outline: min(3px) dashed red }
  > CSS
  $ cascade --minify outline.css
  a{outline:thin solid red;color:#00f}b{outline-width:thick}c{outline:3px dashed red}

border and column-rule read the same <line-width> and always kept it.

  $ cat > border.css <<CSS
  > a { border: thin solid red }
  > b { column-rule: thin solid red }
  > CSS
  $ cascade --minify border.css
  a{border:thin solid red}b{column-rule:thin solid red}
