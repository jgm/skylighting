# Revision history for skylighting

## 0.5 -- 2017-12-10

  * Fix line spacing and overflowing content in generated HTML
    (David Baynard, #25, see jgm/pandoc#4128).

    + Fix empty line height, explicitly
    + Ensure long lines scroll on screen
    + Only apply colour to the outer div
    + Don't reset line number colour
    + Fix borders on empty code lines
    + Collapse divs correctly.

  * Changes to Style types and JSON instances.  Previously we could not
    successfully round-trip through JSON.

    + `tokenStyles` is now a map rather than an association list.
    + We now use `line-number-color` instead of `line-numbers` at
      the top level in the JSON instances, falling back to
      `line-numbers` in `editor-colors`, for KDE theme compatibility.
    + We use `line-number-background-color` at the top level, falling
      back to the text background color.
    + We use `text-color` at the top level, falling back to the `text-color`
      of the `Normal` token style if it exists, for KDE compatibility.
    + We use `background-color` at the top level, falling back to
      the `background-color` in `editor-colors`, for KDE compatibility.
    + A round-trip JSON test has been added.

## 0.4.4.1 -- 2017-11-27

  * HTML formatting: fix color, bgcolor when numbering enabled.
    Previously, the code got the color and background color
    the numbers were supposed to get.  See jgm/pandoc #4103.
  * Updated syntax descriptions for bash, clojure, commonlisp, diff,
    dockerfile, doxygen, doxygenlua, fsharp, hamlet, haskell, haxe,
    java, javascript, julia, latex, literate-curry, literate-haskell,
    makefile, mediawiki, monobasic, ocaml, prolog, r, relaxng, scala,
    sci, sql-mysql, sql-postgresql, sql, xslt.
  * test program:  use --accept instead of --regen.

## 0.4.4 -- 2017-11-21

  * HTML formatter: always use an a element (rather than a div)
    for source lines.  The divs were invalid, because code
    elements must contain phrasing content.  Previously we used
    a elements when line anchors were called for, but there is
    no clear reason not to use them always.
  * skylighting binary: add doctype to generated HTML.

## 0.4.3.2 -- 2017-11-04

  * Fixed regression in `data-line-number` attributes in HTML
    output.  We should not include the `lineIdPrefix` in
    front of this.

## 0.4.3.1 -- 2017-11-03

  * Fixed typo in css (Artymort).
  * Fixed travis build times.

## 0.4.3 --- 2017-11-02

  * `FormatOptions`: added `lineIdPrefix` (jgm/pandoc#4031).
    This is needed because a web page may contain several
    code samples, and we need to make sure that the ids on
    lines are unique.
  * HTML formatter: use `lineIdPrefix` in ids for lines.
  * HTML formatter: Don't put href attributes on divs.

## 0.4.2 --- 2017-10-26

  * HTML output: remove outer div.  This prevented margin
    collapsing between the pre and surrounding block elements,
    and often gave us excess white space around code blocks.
    See jgm/pandoc#3996.

## 0.4.1 --- 2017-09-28

  * Updated XML definitions from KDE repository.
    Changed: actionscript, ada, agda, alert, alert_indent, asn1, awk, bash,
    boo, c, changelog, clojure, cmake, coldfusion, commonlisp, cpp, cs, css,
    curry, d, dockerfile, dot, doxygen, doxygenlua, eiffel, email, erlang,
    fortran, fsharp, gcc, haskell, haxe, isocpp, java, javascript, jsp, julia,
    lilypond, lua, m4, makefile, matlab, maxima, mips, modelines, modula-2,
    monobasic, objectivec, objectivecpp, ocaml, octave, opencl, pascal, perl,
    php, pike, postscript, prolog, purebasic, python, r, relaxng,
    relaxngcompact, rest, rhtml, ruby, rust, scala, scheme, sci, sql-mysql,
    sql-postgresql, sql, tcl, tcsh, verilog, vhdl, xml, xslt, xul, zsh.
  * Added support for powershell, using definition from KDE repository.
  * Stop terminating long builds on TravisCI (Kyle Ondy).

## 0.4 --- 2017-09-15

  * Removed ToJSON/FromJSON instances for KeywordAttr,
    WordSet, Matcher, Context, ContextSwitch, Rule, Syntax
    (added in 0.3.5).  Creating these increased the memory
    requirements for compiling skylighting to a degree not
    justified by the usefulness of these instances.

## 0.3.5 --- 2017-09-14

  * Added ToJSON/FromJSON instances for all basic types.
  * Added some background colors to 'kate' style, matching default.theme,
    and made FromJSON for Style sensitive to background-color.

## 0.3.4.1 -- 2017-09-09

  * HTML formatting: do not use `div` elements for source
    lines in inline output.

## 0.3.4 -- 2017-09-09

  * HTML formatting changes (David Baynard).  Note that these
    changes may require changes in hard-coded CSS that is
    used with skylighting's HTML output.

    + Wrap lines of source code in a `div` with `display` set to
      `inline-block`.  The `div`s make per-line processing easier.
      They cannot be set as `display: block` as that introduces extra
      new lines when copying and pasting.
    + Render line numbers in html using CSS pseudo elements rather than
      a table.  The line numbers are always produced, as `data-line-number`
      attributes, and css to display them as `::before` elements are
      always produced. The option to switch on line numbering only
      toggles a class; this means it is possible to toggle line numbering
      without re-running skylighting.
    + If the `linkAnchors` option is set, wrap with an `a` element rather
      than a `div`, set so that clicking the line number (and only the
      line number) will jump to that line.
    + Code wraps by default when html is printed, with wrapped lines
      indented.

## 0.3.3.1 -- 2017-06-26

  * Updated xml syntax definitions and clojure test.
  * Improved 'make update-xml' target.
  * Updated version bound for criterion.

## 0.3.3 -- 2017-04-22

  * Revert change from 0.3.2; now entities from a DTD are
    resolved if the DTD element is included.  This is needed
    for agda.xml and some others.  If you want to use a custom
    xml definition without needing language.dtd, just remove
    the reference to language.dtd in your xml file.
  * Added `case` keyword for ats (Hanwen (Steinway) Wu).

## 0.3.2 -- 2017-04-01

  * Parse xml definitions without resolving entities from
    language.dtd.  This allows xml definitions to be used even
    when language.dtd isn't available.  Existing definitions
    don't rely on the two or three entities definied in
    language.dtd, so this is harmless.
  * Small optimizations.

## 0.3.1 -- 2017-02-28

  * Use handwritten parser for Float instead of regex.  Fixes
    jgm/highlighting-kate#57 (again).
  * Added float parsing tests.
  * Use parsec parsers, not regex, for CStringChar and CChar.
  * Rewrote Int, Hex, Oct, CStringChar, CCHar parsers with parsec
    instead of regex.  This speeds things up.
  * Don't include leading + in Int, Hex, Oct parsers.  That gives
    bad results for things like `i+1`.

## 0.3 -- 2017-02-19

  * Store literal binary-encoded representations of the Syntax
    structure in Skylighting.Syntax.*.hs, instead of the
    structure itself.  This works around a problem ghc has
    compiling modules with large structures.  This indirect method
    produces faster compile times and avoids massive memory usage by
    ghc (especially in profiling builds).  For background see
    http://stackoverflow.com/questions/16348340/.  Closes #7.
  * Types: Internals of 'WordSet' are no longer exposed.

## 0.2 -- 2017-02-19

  * Restore Data, Typeable, Generic instances for basic types.
    To do this, we remove reCompiled field from RE (API change).  Instead, we
    have the tokenizer compile regexes the first time it encounters them,
    memoizing the results.  Performance is not significantly worse.
  * Skylighting.Syntax.*:  use string representation of the Syntax,
    which is then 'read', rather than including the code for the data
    structure directly (#7).  This indirect method produces faster compile
    times and avoids massive memory usage by ghc (especially in profiling
    builds).  For background see
    http://stackoverflow.com/questions/16348340/compiling-very-large-constants-with-ghc
  * Use -fprof-auto-exported for profiling builds.
  * Added benchmark for xml syntax definition parsing.
  * Patched perl.xml (improperly escaped regex) (#8).
    This fixes a perl regex compilation error for regex patterns
    with backslash as delimiter, e.g. m\a\.
  * Add a flag to build with system pcre (Felix Yan).

## 0.1.1.5  -- 2017-02-03

  * Avoid depending on a PrintfArg instance for Text (#5).
    This isn't provided in some older versions of the text library.
    This change should allow the package to build on older platforms.

## 0.1.1.4  -- 2017-01-31

  * Properly escape characters in subDynamic.
    This fixes a problem that caused failures with

        echo -e "s\0b\0c" | skylighting -s perl

  * More efficient rewrite of char escaping for regexes.

## 0.1.1.3  -- 2017-01-29

  * Increase test timeout to 6s.
  * Avoid double {{}} in latex macros.
  * Fixed problem compiling Format.LaTeX on older ghc versions (7.8.3)
    that can't take a Text as PrintfArg.

## 0.1.1.2  -- 2017-01-28

  * Added much more extensive testing to ensure that tokenizers
    don't hang or drop input.
  * Fixed some issues with line-end and fallthrough context
    handling.
  * Fixed a bug in WordDetect handling which caused it to drop
    input (#2).

## 0.1.1.1  -- 2017-01-21

  * Optimized.  Speed is now comparable to highlighting-kate
    and often better.
  * Added benchmarks.

## 0.1.1    -- 2017-01-19

  * Added breezeDark style, from breeze-dark.theme.
  * Fixed performance bug in regex application (#1).  This gives a
    significant speedup, especially for inputs with long lines.

## 0.1.0.1  -- 2016-12-23

  * Fixed bug in LaTeX renderer (backslash in wrong position).

## 0.1.0.0  -- 2016-12-23

  * Initial release

