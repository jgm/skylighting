skylighting
===========

A Haskell syntax highlighting library with tokenizers derived
from KDE syntax highlighting descriptions.

A command-line highlighter, `skylighting`, is also provided.

Installing
----------

Building from this repository is currently a two-step process.
In the first step we build a program, `skylighting-extract`,
which reads XML syntax highlighting definitions and writes
Haskell source files.  In the second we actually build the
library.

Using stack:

    make bootstrap
    make

Using cabal:

    cabal install -fbootstrap --disable-optimizations
    skylighting-extract xml/*.xml
    cabal install -f-bootstrap --disable-optimizations

History
-------

This is an experiment to rewrite highlighting-kate in a way that
fixes many problems.  (TODO - elaborate)

References
----------

Kate syntax highlighting documentation:
https://docs.kde.org/stable5/en/applications/katepart/highlight.html

Kate highlighting definitions:
https://github.com/KDE/syntax-highlighting/tree/master/data/syntax

