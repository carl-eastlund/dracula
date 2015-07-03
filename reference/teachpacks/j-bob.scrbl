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

@title[(scheme "j-bob")]

@(declare-exporting/dracula teachpacks/j-bob)

@specform[(include-book "j-bob" :dir :teachpacks)]

This teachpack provides the proof assistant "J-Bob" from
@link["http://the-little-prover.org/"]{The Little Prover} (MIT Press, 2015).
