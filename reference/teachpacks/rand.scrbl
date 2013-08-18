#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)
                     (this-package-in teachpacks/rand)))

@title[(scheme "rand")]

@(declare-exporting/this-package [teachpacks/rand] [])

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
