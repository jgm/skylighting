skylighting
===========

[![license](https://img.shields.io/badge/license-GPLv2+-brightgreen.svg)](https://www.gnu.org/licenses/gpl.html)
[![travis build status](https://img.shields.io/travis/jgm/skylighting.svg)](https://travis-ci.org/jgm/skylighting)

A Haskell syntax highlighting library with tokenizers derived
from KDE syntax highlighting descriptions.

A command-line highlighter, `skylighting`, is also provided.

History
-------

This is an experiment to rewrite
[highlighting-kate](https://github.com/jgm/highlighting-kate)
in a way that fixes many problems.  (TODO - elaborate)

Installing
----------

If you are installing from a release tarball from Hackage,
then a simple `stack install` or `cabal install` will work.
The release tarballs include generated files not present in
this repository.

Building from this repository is currently a two-step process.
In the first step we build a program, `skylighting-extract`,
which reads XML syntax highlighting definitions from the `xml`
directory and writes Haskell source files.  In the second we
actually build the library.

Using stack:

    make bootstrap
    make

Using cabal:

    cabal install -fbootstrap --disable-optimizations
    cabal run skylighting-extract -- xml/*.xml
    cabal install -f-bootstrap --disable-optimizations

License
-------

Skylighting is licensed under the GPL, because some of the xml
syntax descriptions from which its tokenizers are generated are
GPL-licensed.

References
----------

Kate syntax highlighting documentation:
<https://docs.kde.org/stable5/en/applications/katepart/highlight.html>

Kate highlighting definitions:
<https://github.com/KDE/syntax-highlighting/tree/master/data/syntax>

