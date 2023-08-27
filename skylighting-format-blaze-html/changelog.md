# Changelog for skylighting-format-blaze-html

## 0.1.1.1

* Remove `display: inline-block` from the code line spans.  This
  caused odd size changes in iOS.  Closes #7248.

## 0.1.1

* Export `formatHtml4Block`, which should be used instead of
  `formatHtmlBlock` if HTML4 compliance is required.

