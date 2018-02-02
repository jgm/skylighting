skylighting
===========

[![license](https://img.shields.io/badge/license-GPLv2+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)
[![skylighting build status](https://img.shields.io/travis/jgm/skylighting.svg)](https://travis-ci.org/jgm/skylighting)

A Haskell syntax highlighting library with tokenizers derived
from KDE XML syntax highlighting descriptions.

A command-line highlighter, `skylighting`, is also provided.

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

    cd skylighting-core
    cabal install
    cd skylighting && skylighting-extract -- xml/*.xml
    cabal install

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

License
-------

The `skylighting` package is licensed under the GPL because some of the
XML syntax descriptions from which its tokenizers are generated are
GPL-licensed. However, the `skylighting-core` package, which provides
the core types and functions of this project is licensed under the BSD3
license and bundles the GPL-licensed XML files separately.

References
----------

Kate syntax highlighting documentation:
<https://docs.kde.org/stable5/en/applications/katepart/highlight.html>

Kate highlighting definitions:
<https://github.com/KDE/syntax-highlighting/tree/master/data/syntax>

[highlighting-kate]: https://github.com/jgm/highlighting-kate
