# Revision history for skylighting

## 0.3 -- 2017-02-19

  * Store literal binary-encoded representations of the Syntax
    structure in Skylighting.Syntax.*.hs, instead of the
    structure itself.  This works around a problem ghc has
    compiling modules with large structures.  This indirect method
    produces faster compile times and avoids massive memory usage by
    ghc (especially in profiling builds).  For background see
    http://stackoverflow.com/questions/16348340/.  Closes #7.

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

