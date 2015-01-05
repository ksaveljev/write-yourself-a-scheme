Write Yourself a Scheme in 48 Hours
=======================

Going through a [tutorial](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) to learn something new.

    cabal sandbox init
    cabal install --dependencies-only
    cabal build

REPL mode:

    cabal run
    Lisp>>> (load "src/stdlib.scm")
    (lambda ("pred" . lst) ...)
    Lisp>>> (map (curry + 2) '(1 2 3 4))
    (3 4 5 6)
    Lisp>>> (filter even? '(1 2 3 4))
    (2 4)
    Lisp>>> quit

Interpreter mode:

    cabal run -- test.scm
