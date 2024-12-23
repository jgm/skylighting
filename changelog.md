# Revision history for skylighting and skylighting-core

## 0.14.5

  * Update xml syntax definitions for bash, cmake, commonlisp, isocpp,
    javascript-react, julia, latex, lua, markdown, modelines, nix, orgmode,
    php, python, rhtml, ruby, swift, xml, yaml, zig, zsh. Add odin (required
    by orgmode).

  * Update JSON syntax definition from upstream. (#203)

## 0.14.4

  * Add `gdscript`, `typst`, `tlaplus` syntax (#184, #199).

## 0.14.3

  * Add crystal, racket, zip syntax (#194).

## 0.14.2

  * Add `loadValidSyntaxesFromDir` (Kevin Quick) [API change].
    The `loadSyntaxesFromDir` function is an all-or-nothing function:
    a single invalid file results in a error and *no* loaded syntaxes.
    This adds the `loadValidSyntaxesFromDir`, which is resilient
    against individual syntax file load failures.  It returns a map
    of the failure messages, and the SyntaxMap that is created from
    all the successful parsing.

## 0.14.1.2

  * Add terraform syntax (#190).

## 0.14.1.1

  * Add GPRBuild syntax.

  * Update syntax descriptions for clojure, cmake, coffee, cpp,
    cs, erlang, graphql, haskell, javascript, julia, kotlin,
    lilypond, markdown, objectivec, objectivecpp, ocaml, perl,
    python, ruby, rust, stan, typescript, yaml.

## 0.14.1

  * Update syntax definitions for julia, actionscript, bash, cpp, css,
    djangotemplate, dosbat, groovy, html, ini, isocpp, java, javadoc, json,
    jsp, latex, makefile, mustache, php, powershell, rest, rhtml,
    sass, scala, scss, toml, xml, raku (with patch).

  * Many improvements to regular expression compilation:

    - Properly recognize unicode characters.
    - Handle `\Q..\E` literal sequences.
    - Handle group modifiers like `(?i:etc.)`. (The only one
      we actually pay attention to is `i`.)
    - Allow unicode category escapes outside of character classes.
    - Support one-letter unicode category escapes like `\p{L}`.
      These are found in some of the newer syntax definitions.

  * Improve regex matching:

    - Fix `wordDetect` to work around an issue in the new html.xml.
    - Refactor and fix `atWordBoundary`: remove redundant checks, and
      fix behavior (`/x\b/` was behaving like `/\bx\b/`) (Côme ALLART).
    - Fix bug in `lastCharOffset` (Côme ALLART).

  * Style: Allow colors to be specified in ARGB format (#178).
    We currently just ignore the A part, but at least we don't fail.

## 0.14

  * Add rWeakDeliminators field to Rule. [API change]

  * Make WordDetect sensitive to weakDeliminator. This fixes
    parsing of floats beginning with '0.' in C (#174).

  * Add debiancontrol syntax (#173).

## 0.13.4.1

  * Update syntax definitions: ada, bash, cmake, css, html, isocpp, java,
    javascript, kotlin, latex, makefile, markdown, php, python, qml, r,
    sass, scss, typescript, zsh.

  * Don't require word boundary at end of Int, Float, HlCHex, HlCOct (#170).
    KDE does not.  This fixes things like `7L` in R.

## 0.13.4

  * Add `dosbat` syntax (MS DOS batch file) (#169).

  * Derive `Bounded` Instance for `TokenType` (#168, Pavan Pikhi).
    Add `Bounded` to the derived instances for the `TokenType` type. This
    allows consumers to use `[minBound .. maxBound]` to generate a list of
    all token types when writing a `Style`.

  * Require xml-conduit >= 1.9.1.3. This fixes a bug that prevents
    parsing certain DOCTYPE declarations, e.g. in `agda.xml`.

  * Updated `cmake` syntax definition.

## 0.13.3

  * Add gap language (#167).

  * Update syntax definitions.

  * Add patches for agda.xml and dtd.xml, to wor around a bug in xml-conduit:
    https://github.com/snoyberg/xml/pull/187

  * Store compiled regexes in RE (#166, Jonathan Coates).
    This changes the RE type to (lazily) compile the regex when constructed,
    rather than in the tokenizer. This allows us to avoid re-compiling
    regexes for each separate tokenize call, instead sharing them globally.
    We try to hide the internals of this, exposing the previous interface
    `(RE { reString, reCaseSensitive })` with pattern synonyms.

  * ConTeXt: fix handling of spaces in non-normal tokens (Albert Krewinkel).
    This ensures that multiple spaces won't be collapsed into a single space.

## 0.13.2.1

  * Update tango style for new token types (#164). The original
    tango style didn't have colors defined for many token types
    that have been added since it was added.  This commit
    updates the style to support them. Thanks to @danbraswell
    for providing the values needed.

## 0.13.2

  * Support ConTeXt output via skylighting-format-context (Albert Krewinkel).

  * Update syntax definitions for markdown, nim, javascript.

## 0.13.1.2

  * Update syntax definitions for c, cmake, commonmlisp, css, go, ini, isocpp,
    javascript, json, powershell, python, rust, sass, scss, toml, xml.

  * Associate regex captures with a context. This is necessary for proper
    implementation of the KDE syntax highlighting algorithm.  Captures are
    only available within the context to which the match creating the
    capture switches. (Closes #160.)

## 0.13.1.1

  * Add dart, purescript syntax definitions.

  * Update syntax definitions for isocpp, latex, alert, bash,
    cmake, nix, sql-postgresql, vhdl, zsh.

  * Remove obsoleote patch for lua.xml.

  * Add patches for latex.xml and isocpp.xml.

## 0.13.1

  * `getCapture`: fail instead of throwing error if dynamic match not found.
    I believe this is the intended behavior for StringDetect, judging
    from examples in the KDE documentation.

  * Update xml syntax definitions:
    asn1, bash, c, cmake, cpp, cs, d, elixir, fortran-fixed, gcc, glsl,
    go, html, java, javascript, lex, lua, markdown, mediawiki, noweb,
    ocaml, orgmode, php, powershell, prolog, python, r, ruby, rust,
    scheme, sql-postgresql, typescript, vhdl, xml, yacc, yaml, zsh

  * Replace a lambda with pointfree notation (shaving off some RAM usage)
    (0xd34df00d).

  * Use `newtype` for `TokenizerM`, giving about 5-10% boost on benchmark
    (0xd34df00d).


## 0.13

  * Update syntax definitions from upstream: bash, cmake, diff,
    ini, perl, php, sgml, xml.

  * Split out formatters into separate packages (#152). API changes:
    - Skylighting.Core no longer exports Skylighting.Format.ANSI,
      Skylighting.Format.HTML, Skylighting.Format.LaTeX.  These are
      now provided by separate packages, skylighting-format-ansi,
      skylighting-format-blaze-html, skylighting-format-latex.
    - Skylighting.Types no longer exports XTerm256ColorCode.
      This has been moved to skylighting-format-ansi.

  * Change Makefile to use cabal instead of stack.


## 0.12.3.1

  * Allow mtl 2.3.

  * Update syntax defs from upstream: bash, cmake, go, haxe, lua, zsh.

  * Add nix.xml (#149).

  * Add Pygments styles for `Import` and `BuiltIn` token types (#147,
    Bryan A. Danielak).

  * Use StrictData.

  * Remove unused dependencies (silences cabal warnings) (Andreas Abel).

## 0.12.3

  * Add scss, sass, systemverilog, orgmode.

  * Update xml definitions from upstream: bash, css, python, r, zsh.


## 0.12.2

  * Parser: handle context shift to external context, e.g.,
    `BashOneLine##Bash`.  Closes #139 (issue with Dockerfile).

  * Update xml files from upstream:
    bash, cmake, markdown, objectivecpp, php, sql-postgresql, sql, stan, zshr

  * Fix formatting in `Color` doc-comment (Janek Spaderna).

  * Add stan.xml (Brian Ward).

## 0.12.1

  * Update syntax definitions:  bash, cmake, dockerfile,
    gnuassembler, markdown, spdx-comments.

  * Support hex escapes using `\x` in regex char classes (#135).
    These occur in a number of syntax definitions and weren't
    correctly interpreted before. Thanks to @Agnishom.

  * Support regex property syntax, e.g. `\p{Lu}`.

  * Regex: support `\B` (non-word-boundary).

## 0.12

  * Properly handle include elements in keyword lists (#124).
    A number of syntaxes (e.g. typescript, scala) include keyword
    lists from other syntaxe,s and previously we weren't able to
    handle this.

    There are several pieces to this change.  We need to store lists
    where other Syntaxes can look them up, so we add an `sLists`
    field to `Syntax` [API change], and modify the parser to fill this.
    We change lists so that their values are not just a `Text`, but a
    `ListItem` that can either be a textual value or an include directive,
    specifying a `ListName` (syntax name and list name).

    The `Keyword` constructor for `Matcher` now takes, instead of a
    `WordSet`, Either a `ListName` or a `WordSet` (API change).

    Skylighting.Parser now exports `resolveKeywords` (API change),
    which modifies all `Keyword` matchers in a syntax so that Left
    values with a `ListName` become Right values with resolved `WordSet`s.
    The tokenizer applies this function automatically to the SyntaxMap
    given in Config.  But it is more efficient to do this conversion
    just once, rather than every time `tokenize` is called. So we have
    `loadSyntaxesFromDir` call it on the `SyntaxMap`.  With this
    optimization, there is not an appreciable performance cost to the
    changes described above.

  * Skylighting.Regex: Fix bug with regexes like `a{10}b` (#133).
    This requires exactly 10 a's; previously we interpreted it as
    "at least 10."

  * skylighting-extract: take a directory as argument rather than files.
    This allows us to use `loadSyntaxesFromDir`.

  * Update xml syntax definitions from upstream:
    julia, cmake, cpp, isocpp, markdown, python, toml.


## 0.11

  * Skylighting.Regex: Support regex subroutines (#118).  For example,
    `(?1)` is replaced by the regex in the first capturing group.  So far
    we only support this simple, absolute form, not the relative
    form `(?-1)` supported by some engines (but not used, I think, in
    KDE's syntax highlighters).  This change involves an API change:
    Regex in Skylighting.Regex has a new Subroutine constructor,
    and the Recurse constructor has been removed.  Instead of Recurse we use
    Subroutine 0, which unifies the code.

  * Skylighting.Regex: handle e.g. `[\1]` and `[\123]` (without
    initial 0) as octal escapes (#118).  These occur in the zsh.xml
    syntax definition.

  * Pull xml definitions for bash, cmake, python, zsh from upstream.

  * README: Add a note about pulling syntax definitions from upstream (#138).
    Update build instructions for recent cabal versions (#131).

## 0.10.5.2

  * Added swift grammar definition (Igor Ranieri).

  * Simplify README.md instructions for two-step build.

  * Fix link to KDE documentation.

## 0.10.5.1

  * Regex: Allow lookaheads to capture groups.  Previously
    captures in lookaheads, like `(?=(a*))`, were ignored.  This
    led to errors highlighting xml and probably other formats (#121).

  * Throw an exception if a capture group isn't defined (with 'dynamic')
    rather than simply having getCapture fail so that the rule fails.
    This will make it easier to debug issues like #121.

## 0.10.5

  * Fix regression from 0.10.3 with Haskell highlighting of Char (#120).

  * Update xml syntax definitions from upstream.  Updated
    syntaxes: abc apache asp bash bibtex boo c changelog clojure
    cmake commonlisp cs curry d diff djangotemplate doxygen
    elixir elm email erlang fortran-fixed fortran-free fsharp
    hamlet haskell haxe html idris ini javascript-react
    javascript json julia kotlin latex lex lilypond
    literate-curry literate-haskell makefile markdown
    mathematica maxima mediawiki metafont modula-2 mustache nasm
    nim noweb ocaml octave opencl perl powershell prolog pure
    python r roff ruby rust sed spdx-comments sql-mysql
    sql-postgresql tcl tcsh toml typescript verilog xml xslt xul
    yacc yaml.  Not updated: rhtml (causes an error on our test
    suite), zsh (has a regex we can't parse).

## 0.10.4.1

  * Fixed logic for checking line-end-context (#119).

  * Use NonEmpty for the context stack.

  * Remove unneeded build-depends.

## 0.10.4

  * Move from hxt to xml-conduit for XML parsing.

    This gives about a 4X speedup in parsing syntax definitions.
    It also reduces the pandoc build dependency footprint, as we
    depend on xml-conduit anyway and now no longer need to compile
    HXT and its dependencies.

    There are improvements in accuracy as well: the change to
    xml-conduit improved parsing for one of the prolog rules;
    a matcher that should have been for tab characters had been set for a
    space instead.

  * Removed some unnecessary build-depends in skylighting-extract.

## 0.10.3

  * Add support for raku (#114).

  * Reimplement PR #40 and add haskell.xml.patch (#116).

  * Update syntax definitions: actionscript ada asp awk bash
    bibtex boo c cmake cpp cs d elm email fasm fsharp glsl
    gnuassembler go haskell haskell idris isocpp
    javascript julia latex lilypond makefile mediawiki metafont
    mustache objectivec objectivecpp octave opencl perl php
    php powershell prolog purebasic raku rest ruby sed
    spdx-comments sql-mysql sql-postgresql sql typescript
    verilog vhdl.

  * Fix php.xml.patch so it applies again.

## 0.10.2

  * Update syntax definitions for abc, actionscript, asn1, ats,
    bash, boo, coffee, comments, cpp, cs, css, curry, d,
    djangotemplate, dockerfile, doxygen, dtd, elixir, graphql,
    groovy, hamlet, haskell, haxe, idris, ini, j, java,
    javadoc, javascript-react, javascript, jsp, kotlin, lex,
    lilypond, literate-curry, literate-haskell, m4, makefile,
    mandoc, markdown, mediawiki, mips, modula-2, modula-3,
    monobasic, mustache, nim, noweb, objectivec, objectivecpp,
    ocaml, opencl, pascal, perl, php, pike, postscript,
    prolog, protobuf, pure, purebasic, python, qml, relaxng,
    relaxngcompact, rest, rhtml, roff, ruby, scala, sci, sed,
    sgml, sml, spdx-comments, stata, tcsh, texinfo, verilog,
    xml, xorg, xul.

  * Re-insert CSS line needed to make line numbers appear in
    HTML.  See jgm/pandoc#6625.

## 0.10.1

  * Regex: handle `(?|)` modifier.  This is used in bash.xml now.
    It resets the numbers of capturing groups in alternatives.

  * Improve regex handling of `{` and `}` not in quantifiers:
    - `{}` is literal (not a quantifier).
    - loose unescaped `{` and `}` that are not part of a quantifier are
      literal matchers.

  * Update xml syntax definitions from upstream.
    For: abc, actionscript, ada, agda, alert, apache, asn1, asp,
    ats, awk, bash, boo, c, clojure, cmake, coffee, coldfusion,
    commonlisp, cs, css, curry, d, djangotemplate, dockerfile,
    dot, doxygen, dtd, eiffel, elixir, elm, erlang, fasm,
    fortran-fixed, fortran-free, fsharp, glsl, gnuassembler, go,
    graphql, groovy, hamlet, haskell, haxe, html, idris, ini,
    isocpp, j, java, javadoc, javascript, jsp, julia, kotlin,
    latex, lex, lilypond, literate-curry, literate-haskell,
    llvm, lua, m4, makefile, markdown, mathematica, matlab,
    maxima, mediawiki, metafont, mips, modula-2, modula-3,
    monobasic, mustache, nasm, nim, noweb, objectivec,
    objectivecpp, ocaml, octave, opencl, pascal, perl, php,
    pike, postscript, povray, powershell, prolog, protobuf,
    pure, purebasic, python, r, relaxng, relaxngcompact, rest,
    rhtml, roff, ruby, rust, scala, scheme, sci, sed, sgml, sml,
    sql-mysql, sql-postgresql, sql, stata, tcl, tcsh, texinfo,
    toml, verilog, vhdl, xml, xorg, xslt, xul, yacc, yaml, zsh.
    Also added spdx-comments.xml and comments.xml, which are
    needed for these.  Closes #111 (latex bug in matrix).


## 0.10.0.3

  * Add groovy syntax.

## 0.10.0.2

  * Improve DetectIdentifier to ensure that identifiers can't
    include non-ASCII characters.

## 0.10.0.1

  * Fix identifier detection in non-ASCII context (#110).

## 0.10

  * Add instructions for submitting patches upstream to KDE (#106).

  * Synced syntax definitions from KDE repo.  Note that fortran
    has split into two: `fortran-fixed` and `fortran-free`.

  * Add test to ensure that all regexes in rules compile.

  * Regex: allow unescaped `}`.

  * Regex: allow empty regexes and groups.

  * Regex: support lazy and possessive quantifiers (#109).

  * Regex: support recursive regexes `(?R)` (#108).

  * Hide invisible line numbers from keyboard focus (#107, d10n).
    This fixes tabbing through elements on a page.

  * Remove some obsolete patches for xml definitions.


## 0.9

  * Use a pure Haskell regex implementation (in unexported module
    Text.Regex.KDE) instead of pcre.  The implementation is not
    as efficient as pcre, but it seems good enough for this
    application, and it is desirable to avoid depending on a C
    library.  (Available Haskell libraries weren't up to the
    task, because they don't do back-references, captures,
    lookahead/behind.) Some benchmarks (old/new):
    haskell (4.6/7.9) java (13.4/23.3) c (2.8/3.7) rhtml
    (4.7/6.1) lua (10.6/13.2) javascript (4.2/6.6).
    Though this is a significant slowdown, the tradeoff seems
    worth it to have a pure Haskell implementation.

  * Removed old `system-pcre` flag.

  * More efficient treatment of dynamic regexes.
    We put something in the Regex itself to represent the `%1`,
    and modify it later.  This allows us to cache dynamic
    regexes in a way we couldn't before.

  * Add support for TOML (#105, Shiming Wang),
    GraphQL, and Nim syntax (#102, Daniel Pozo Escalona).

  * Update xml definitions for actionscript, bash, boo, c,
    cmake, elm, erlang, glsl, isocpp, java, lua, m4, mediawiki,
    perl, powershell, scala, tcsh, xul, zsh.

  * Fix fallthrough behavior (don't always consume a token).

  * Fix word boundary detection.

  * Remove RegexException. (API change)

  * Skylighting.Regex now exports `isWordChar` and `testRegex`,
    as well as the constructors underlying the new `Regex` type.

  * Remove some obsolete xml definition patches.

  * Fix escaped % in dynamic regex.


## 0.8.5

  * Respect dynamic flag on StringDetect elements (#99, Albert
    Krewinkel).

  * Increase test timeout to avoid failures with qemu-emulated
    environments, such as qemu and riscv64 in Ubuntu builders
    (William Grant).

  * Fix attribute for opening double quote in sql-postgresql.xml
    (Benjamin Wuethrich).

  * Update syntax descriptions for javascript, bash, coffee,
    javascript-react, javascript, latex, sql-postgresql, typescript.

## 0.8.4

  * HTML output: use aria-hidden="true" on empty a elements
    unless numberLines is specified (in which case the element
    is still empty but will have content added by CSS).
    This is to avoid excess noise when the code blocks are
    read by screen readers.  See jgm/pandoc#6352.

## 0.8.3.4

  * Update syntax descriptions for: cmake agda c coffee doxygenlua html
    isocpp latex lua makefile markdown metafont mustache nasm python rust
    sql-postgresql typescript vhdl xml yacc yaml.

  * Fix invalid CSS: in some cases, the library produced
    `code span.` with no class for the NormalTok style (Michał Miszczyszyn).
    Now it produces a rule for `code span`.

## 0.8.3.3

  * Agda keywords updated to 2.6.0 (Andreas Abel).

## 0.8.3.2 -- 2020-01-21

  * This fixes a packaging error in 0.8.3.1, which didn't include
    the results of the bootsrap step and hence didn't update the
    syntax modules.

## 0.8.3.1 -- 2020-01-21

  * Escape `<` and `>` in latex to avoid ligatures (#91).

  * Fix and extend Idris keywords (Denis Buzdalov).

  * Update XML syntax definitions for javascript-react, qml,
    typescript, javascript, cmake, cofee, latex, perl, python,
    rest.

  * Fixed bug with context shifts using `##` syntax for foreign context.
    Previously we only handled these in IncludeRules, but
    starting with the current batches of xml files they can occur
    elsewhere too.

## 0.8.3 -- 2019-11-26

  * Update XML syntax definitions for c, cmake, coffee, cpp, css,
    diff, djangotemplate, elixir, elm, glsl, haskell, html, isocpp,
    javascript-react, javascript, json, jsp, julia, latex, lua,
    markdown, matlab, mediawiki, modelines, mustache, perl, php,
    powershell, qml, rest, rust, typescript, vhdl.

  * Fix highlighting errors with non-ASCII characters in JSON (#90).
    This error was due to an optimization that made some incorrect
    assumptions.

## 0.8.2.3 -- 2019-10-03

  * Tests: ensure we read test files as UTF-8.

  * Fix compiler warnings around Monoid imports.

  * Add MonadFail constraint (allowing compilation with ghc 8.8.x).

  * Add cabal.project and move CI from Travis to GitHub Actions.

## 0.8.2.2 -- 2019-09-30

  * Remove upper bound on regex-base and require
    regex-pcre-builtin >= 0.95, to avoid a build conflict
    with earlier versions of regex-pcre-builtin and
    regex-base >= 0.94.

  * Remove unused system-pcre flag in skylighting.cabal.
    This flag only affects skylighting-core.

## 0.8.2.1 -- 2019-09-25

  * Add upper bound for regex-base if regex-pcre-builtin is used.
    Currently regex-pcre-builtin won't build on ghc 8.4+ with
    regex-base 0.94.0.0.  When regex-pcre-builtin is updated, we
    can remove this.

  * Correct cabal wildcard to include .xml.patch files in distribution.

  * Update breezedark style (Kien Dang).

  * Update markdown.xml so first list marker is included in list (#84).

  * skylighting.cabal - removed some build dependencies
    which are not necessary (they're dependencies of skylighting-core).

## 0.8.2 -- 2019-07-14

  * Change matchRegex so it gives "no match" on a regex error
    instead of raising an exception. This seems to be how Kate
    works.  Fixes an error on long integer literals (#81).

## 0.8.1.2 -- 2019-07-14

  * Fix HlCChar for one-character octal escapes like '\0' (#82).
    Due to a bug in pCStringChar, only multi-character octal
    escapes were being recognized. This affects not just C
    highlighting, but all of the following highlighters which
    use HlCChar: fasm eiffel pike objectivec ruby vhdl scala
    java jsp nasm protobuf pure go objectivecpp gnuassembler povray
    actionscript c cs opencl boo rhtml elixir.  This fixes a
    regression introduced in version 0.3.1.
  * Ensure line span css only applied to lines (David Baynard).

## 0.8.1.1 -- 2019-06-13

  * Improved LaTeX escaping (#78).

## 0.8.1 -- 2019-06-04

  * Added support for stata (#74).
  * Added support for javascript-react, i.e. jsx (#56).
  * Added support for qml and mustache (new dependencies for
    markdown and html).
  * Update xml syntax definitions from KDE repository:
    cmake, doxygen, html, ini, javascript, julia, literate-haskell,
    markdown, octave, perl, tcsh, typescript, yaml.
  * Fix bug in detectIdentifier (#76).  It behaved wrongly when
    applied to a string that was an valid identifier but had nothing
    else after it (as would naturally occur with inline code in pandoc).
    The bug caused it to match just one character in this case.
  * Add --number-lines-from option to cli program (David Baynard).
  * Override start number if nonzero (David Baynard, fixes regression
    in 0.8).

## 0.8 -- 2019-05-27

 *  Wrap html source lines with `<span>`, not `<a>` (David
    Baynard).  Fixes jgm/pandoc#4386. Also fixes #33 (and
    therefore fixes jgm/pandoc#4278).
  * Prevent line-wrap overlap in html by increasing padding
    (David Baynard).
  * Remove superfluous 'pointer-events: all' (David Baynard).
  * Simplify HTML generation code (David Baynard).
  * Remove sourceLine class from spans (David Baynard).
  * Display line numbers using css counters (David Baynard).
  * Add Elm syntax definition (#66, #68, Bonghyun Kim).
  * Put changelog in base directory, use symlinks to point to it.
    We have the same changelog for skylighting and skylighting-core,
    for simplicity.  (Version numbers are kept in sync.)

## 0.7.7 -- 2019-02-27

  * Add parseSyntaxDefinitionFromString [API change].
  * Fix parseSyntaxDefinition so that language.dtd no longer needed.
    We strip out the SYSTEM "language.dtd" part of the doctype
    if present, leaving any inline entity definitions.
    This applies to both parseSyntaxDefinition and
    parseSyntaxDefinitionFromString.
  * Revised documentation of bootstrap build.
  * Makefile - fix bootstrap target.
  * Use FilePath instead of String for path (#64, anchpop).

## 0.7.6 -- 2019-02-08

  * Update xml definitions for C, CPP, D, Haskell, Maxima, PHP,
    Ruby, Typescript.
  * Fix regex problem in erlang.xml (#63).
  * Export WordSet constructors (#62, Chris Martin).
  * Add support for protobuf (#60).

## 0.7.5 -- 2018-12-01

  * Updated definitions for cmake, lua, prolog, rust, yacc,
    javascript.
  * Added definition for sml (#53).
  * Added definition for J (#50).
  * Added definition for typescript (#57).
  * Fix comments preceded by multiple spaces in bash.xml (#54).
  * Add *.txt extension and text/plain mime type to default.xml
    (#48).
  * Add file:/// if we have a Windows path with a drive.
    Otherwise HXT will take it to be a URI.  See jgm/pandoc#4836.

## 0.7.4 -- 2018-10-08

  * HTML output: use title instead of data-line-number.
    data- attributes are not valid HTML 4, and we would like this to
    work with HTML 4 (e.g. in epub v2).  See jgm/pandoc#4920.
  * Print FALLTHROUGH in --trace output.
  * Updated syntax definitions for actionscript, awk, bash, c, clojure, cmake,
    coffee, cs, css, dockerfile, email, fortran, gcc, haskell, ini, isocpp,
    java, javadoc, javascript, jsp, kotlin, latex, lua, mediawiki, modelines,
    modula-2, objectivec, objectivecpp, opencl, perl, powershell, prolog,
    python, r, rest, rhtml, ruby, rust, scala, sql-mysql, sql-postgresql, sql,
    tcl, vhdl, xml, xul, yaml, zsh.

## 0.7.3 -- 2018-08-27

  * Add 'default.xml' to syntax descriptions.
  * Raise base lower bounds to 4.8.  Drop support for ghc 7.8.
  * Use absolute number for cabal version, as now required.

## 0.7.2 -- 2018-06-08

  * Update syntax definitions from upstream.
  * Added support for POV-Ray syntax (#46).
  * Display line numbers without absolute positioning (David
    Baynard, #32).

## 0.7.1 -- 2018-03-15

  * Fix benchmarks, which previously depended on a module
    defined in skylighting (#42).
  * Export Skylighting.Loader from Skylighting.Core.
  * Bump version bound for criterion.

## 0.7.0.2 -- 2018-03-06

  * Ensure that regex captures are not overwritten by regexes
    without captures.
  * Fixed bug in subDynamic (#41).
  * Added tracing information about rules tried and dynamic
    regexes, for debugging.
  * Fix highlighting for Haskell chars which breaks `-XDataKinds` (#40,
    Artyom Kazak).

## 0.7.0.1 -- 2018-03-03

  * Updated changelog.md.

## 0.7 -- 2018-03-03

  * Split skylighting into skylighting-core (BSD3) and skylighting (GPL)
    (#37, Jonathan Daugherty).  skylighting-core (BSD3 licensed) includes
    the core library functionality and the source of the XML syntax, but
    not the compiled Syntax modules, and skylighting (GPL2 licensed)
    includes Syntax modules compiled from the XML syntax files.

  * We discontinue support for GHC before 7.10 (because we need to support the
    reexport-modules Cabal directive.)

  * Print message about loaded syntax defs to stderr (#36).

  * New module Skylighting.Loader, exporting
    `loadSyntaxFromFile` and `loadSyntaxesFromDir` (Jonathan Daugherty).

  * Adjust tests to load xml definitions dynamically.

## 0.6 -- 2018-01-18

  * Add ANSI color output (Alexander Ronald Altman, #22).

    + New package dependencies: `ansi-terminal` and `colour`.
    + New module: `Skylighting.Format.ANSI`, exporting `formatANSI`,
      (also reexported from the module `Skylighting`).
    + In `Skylighting.Types`, new types `ANSIColorLevel` and
      `Xterm256ColorCode`, and a new field `ansiColorLevel` in
      `FormatOptions`.
    + Main `skylighting` executable now supports ANSI output (use
      `ansi` as argument to `--format`), which is now the default output
      format. A new flag `--color-level` has been added to select
      the number of colors (with options `16`, `256`, `true`, and
      the default `auto`).

  * Reword error for missing contexts in IncludeRules.

  * Use ordNub instead of nub for missingIncludes.

## 0.5.1 -- 2018-01-07

  * Fixed tokenizer exceptions (#31).  Previously `throwError`
    did not actually generate `Left` results in `tokenize`,
    because of the way the MonadPlus instance worked. Rewrote
    the TokenizerM monad from scratch so that exceptions would
    not be skipped.

  * Improved error message about undefined contexts.

  * Makefile: use cabal for bootstrap target.

  * README: remove stack bootstrap instructions.
    You can no longer bootstrap with stack, because it
    insists on building ALL the executables the first time,
    and of course that can't be done with this library.

  * Use quickcheck for fuzz test.

## 0.5.0.1 -- 2017-12-18

  * Small improvements to fuzz tests in test suite.  We now
    ensure that we print the random text on test failure.  Also,
    we now run the test with many smaller samples rather than
    one big one.

  * Add aeson lower bound (because of toEncoding) (#28).

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
