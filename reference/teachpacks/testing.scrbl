#lang scribble/doc
@(require
   scribble/manual
   scribble/eval
   racket/require
   (path-up "self/require.rkt")
   (path-up "self/scribble.rkt")
   (path-up "self/module-path.rkt")
   "../evaluator.rkt"
   (for-label
     (dracula-in main)
     (teachpack-in testing)))

@title[(scheme "testing")]

@(declare-exporting/dracula teachpacks/testing)

@specform[(include-book "testing" :dir :teachpacks)]

This teachpack provides a graphical interface for unit tests.

@defform[(check-expect actual expected)]{
Tests @scheme[(equal actual expected)].
}

@defform[(check-within actual expected delta)]{
Tests
@scheme[(and (<= (- expected delta) actual) (<= actual (+ expected delta)))].
}

@defform[(check-error expr)]{
Tests that @scheme[expr] triggers a guard violation.
}
