# Changelog for skylighting-format-blaze-html

## 0.1.1.2

* Re-add `display: inline-block` to code line spans, but only
  for print media. See jgm/pandoc#9520.

## 0.1.1.1

* Remove `display: inline-block` from the code line spans.  This
  caused odd size changes in iOS.  Closes jgm/pandoc#7248.

## 0.1.1

* Export `formatHtml4Block`, which should be used instead of
  `formatHtmlBlock` if HTML4 compliance is required.

