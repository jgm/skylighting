# Revision history for skylighting-core

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
