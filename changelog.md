# Revision history for skylighting

## 0.1.0.0  -- 2016-12-23

* Initial release

## 0.1.0.1  -- 2016-12-23

* Fixed bug in LaTeX renderer (backslash in wrong position).

## 0.1.1    -- 2017-01-19

* Added breezeDark style, from breeze-dark.theme.
* Fixed performance bug in regex application (#1).  This gives a
  significant speedup, especially for inputs with long lines.

## 0.1.1.1  -- 2017-01-21

* Optimized.  Speed is now comparable to highlighting-kate
  and often better.
* Added benchmarks.

## 0.1.1.2  -- 2017-01-28

* Added much more extensive testing to ensure that tokenizers
  don't hang or drop input.
* Fixed some issues with line-end and fallthrough context
  handling.
* Fixed a bug in WordDetect handling which caused it to drop
  input (#2).

