skylighting
===========

[![license](https://img.shields.io/badge/license-GPLv2+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)
[![CI tests](https://github.com/jgm/skylighting/workflows/CI%20tests/badge.svg)](https://github.com/jgm/skylighting/actions)

A Haskell syntax highlighting library with tokenizers derived
from KDE XML syntax highlighting descriptions.

A command-line highlighter program, `skylighting`, is also provided.

This project is divided up into five packages:

 * `skylighting-core`: this provides KDE XML parsing, data types,
   and a tokenizer. This includes the core functionality of the
   skylighting project licensed under the BSD3 license, along with the
   KDE XML files, some of which are licensed under the LGPL or GPL. This
   package does not provide any built-in parsers corresponding to the
   XML descriptions, however. For that, use `skylighting`.
* `skylighting-format-ansi`: this provides formatters for
  rendering skylighting tokens as colored ANSI text.
* `skylighting-format-blaze-html`: this provides formatters for
  rendering skylighting tokens as HTML, using the `blaze-html` library.
* `skylighting-format-context`: this provides formatters for
   rendering skylighting tokens as ConTeXt.
* `skylighting-format-latex`: this provides formatters for
  rendering skylighting tokens as LaTeX.
* `skylighting-format-typst`: this provides formatters for
  rendering skylighting tokens as Typst.
* `skylighting`: this exposes the `skylighting-core` API and
   ANSI, HTML, ConTeXt, and LaTeX formatters, and also
   provides bundled Haskell parser modules derived from the XML
   descriptions in the `core` package. This package is entirely licensed
   under the GPL.

Motivation
----------

This library is the successor to [highlighting-kate], which had
some problems that were difficult to resolve given its
architecture.

In highlighting-kate, the XML syntax descriptions were converted
into individual parsec parsers, which were then compiled.  This
made it difficult to handle IncludeRules properly without
circular imports.  There was also no way to load a syntax
description dynamically.

Skylighting, by contrast, parses the XML syntax descriptions
into Haskell data structures, which are then interpreted by
a "tokenize" function.  IncludeRules can now be handled
properly, and users can add new syntax descriptions
dynamically.  It is also now possible to convert `.theme` files
directly into styles.

Skylighting is also faster than highlighting-kate, by a
factor of 3 in some cases.

Installing
----------

To install the latest release from Hackage, do

    stack install skylighting
or

    cabal install skylighting

If you want the command-line tool, set the `executable` flag
using `--flag "skylighting:executable"` in stack or
`-fexecutable` in cabal.

The release tarball for the `skylighting` package includes generated
files not present in this repository. Building from this repository is
a two-step process. In the first step we build the `skylighting-core`
package, which provides a program, `skylighting-extract`, which reads
XML syntax highlighting definitions from the `xml` directory and writes
Haskell source files. In the second we actually build the `skylighting`
package.

Using cabal:

    # First, build skylighting-extract
    cabal build -fexecutable skylighting-core
    # This will print the path of the built executable.
    # Replace $EXE with this path in the following steps
    # Now, generate the syntax files
    cd ../skylighting
    $EXE ../skylighting-core/xml
    cabal install -fexecutable

Using stack:

    stack build --flag skylighting-core:executable skylighting-core
    cd skylighting
    stack exec skylighting-extract -- ../skylighting-core/xml
    cd ..
    stack install --flag skylighting:executable


Command-line tool
-----------------

A command-line executable, `skylighting`, is installed if
the `executable` cabal flag is set in building.

For help, `skylighting --help`.

Adding new syntaxes
-------------------

To compile with additional syntaxes, simply add the syntax definition
(XML) file to the `xml` directory of the `skylighting-core` package and
repeat the bootstrap build described above.

Note that both the library and the executable can dynamically load
syntax definitions, so you may not need to compile them in. If you
prefer this approach, you can use the `skylighting-core` package
directly which provides the XML files without the generated code
produced by the bootstrap process described above.

If the syntax definition you are adding is not already in the
[KDE repository], please submit it upstream so it can be
included there.  You can do that here, providing the file (or
changes) and a test:
<https://invent.kde.org/frameworks/syntax-highlighting/-/merge_requests>.
Here is a sample merge request:
<https://invent.kde.org/frameworks/syntax-highlighting/-/merge_requests/20/diffs>.
If creating a proper merge request is too much work, at least
submit an issue to
<https://invent.kde.org/frameworks/syntax-highlighting/-/issues>
alerting the KDE developers of the availability of a new or
changed syntax definition; they can then decide whether to
integrate it.

We normally pull changes in syntax definitions from upstream
before each release.

License
-------

The `skylighting` package is licensed under the GPL because some of the
XML syntax descriptions from which its tokenizers are generated are
GPL-licensed. However, the `skylighting-core` package, which provides
the core types and functions of this project is licensed under the BSD3
license and bundles the GPL-licensed XML files separately.

The KDE project now recommends that new syntax highlighting
files be MIT licensed.

References
----------

Kate syntax highlighting documentation:
<https://docs.kde.org/stable5/en/kate/katepart/highlight.html>

Kate highlighting definitions: [KDE repository]

[KDE repository]: <https://github.com/KDE/syntax-highlighting/tree/master/data/syntax>

[highlighting-kate]: https://github.com/jgm/highlighting-kate
