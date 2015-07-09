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
     (teachpack-in little-prover)))

@title[(scheme "little-prover")]

@(declare-exporting/dracula teachpacks/little-prover)

@specform[(include-book "little-prover" :dir :teachpacks)]

This teachpack provides definitions of all of the proofs in
@link["http://the-little-prover.org/"]{The Little Prover} (MIT Press, 2015).
