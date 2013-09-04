#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require (for-label dracula/lang/dracula
                     dracula/teachpacks/rand))

@title[(scheme "rand")]

@(declare-exporting dracula/teachpacks/rand)

@specform[(include-book "rand" :dir :teachpacks)]

This teachpack provides functional pseudorandom number generation.

@defproc[(initial-seed) posp]{
Produces a starting point for the pseudorandom number generator.
}

@defproc[(next-seed [seed posp]) posp]{
Updates the pseudorandom seed.  Call after each use of @scheme[rand] to avoid
reusing a seed value.
}

@defproc[(rand [max posp] [seed posp]) natp]{
Produces a number between @scheme[0] and @scheme[(1- max)], dependent on
@scheme[seed].
}
