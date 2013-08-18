#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme (this-package-in scheme)))

@title[#:style 'quiet #:tag "exn"]{Exceptions}

@defmodule/this-package[exn]

This module provides tools for dealing with exceptions.

@defform[(try expr ...+)]{

Executes the first expression @scheme[expr] in the sequence, producing its
result value(s) if it returns any.  If it raises an exception instead,
@scheme[try] continues with the next @scheme[expr].  Exceptions raised by
intermediate expressions are reported to the @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{current logger} at the @scheme['debug]
level before continuing.  Exceptions raised by the final expression are not
caught by @scheme[try].

@defexamples[
#:eval (evaluator)
(try (+ 1 2) (+ 3 4))
(try (+ 'one 'two) (+ 3 4))
(try (+ 'one 'two) (+ 'three 'four))
]

}
