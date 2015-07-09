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
     (teachpack-in j-bob-lang)))

@title[(scheme "j-bob-lang")]

@(declare-exporting/dracula teachpacks/j-bob-lang)

@specform[(include-book "j-bob-lang" :dir :teachpacks)]

This teachpack provides the definition of the language used in
@link["http://the-little-prover.org/"]{The Little Prover} (MIT Press, 2015).
