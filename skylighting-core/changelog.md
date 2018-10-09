# Revision history for skylighting-core

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

  * Initial release of this library, which includes the core
    functionality of skylighting, relicensed as BSD3
    (Jonathan Daugherty).
