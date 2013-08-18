#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Ordinal Arithmetic}

@defform[(make-ord xs)]

@defform[(o< x y)]
@defform[(o<= x y)]
@defform[(o> x y)]
@defform[(o>= x y)]

@defform[(o-finp x)]
@defform[(o-infp x)]
@defform[(o-p x)]

@defform[(o-first-coeff x)]
@defform[(o-first-expt x)]
@defform[(o-rst x)]
