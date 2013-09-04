#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require (for-label dracula/lang/dracula
                     dracula/teachpacks/testing))

@title[(scheme "testing")]

@(declare-exporting dracula/teachpacks/testing)

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
