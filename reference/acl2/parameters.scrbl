#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Theorem Prover Controls}

These terms control options for the logic of the ACL2 theorem prover.

@defform[(in-package "ACL2")]{
Required at the start of an ACL2 @tech{book}.  See @scheme[include-book] for
more details. 
}

@defform[(set-compile-fns flg)]{
If passed @scheme[t], functions will be compiled as you go along.
@examples[
#:eval the-evaluator
(set-compile-fns nil)
(set-compile-fns t)
]
}

@defform[(set-ignore-ok flg)]{
If passed @scheme[t], unused formals and locals will be allowed without an
@scheme[ignore] or @scheme[ignorable] declaration.
@examples[
#:eval the-evaluator
(set-ignore-ok t)
(set-ignore-ok :warn)
(set-ignore-ok nil)
]
}

@defform[(set-irrelevant-formals-ok flg)]{
If passed @scheme[t], irrelevant formals will be allowed in definitions.
@examples[
#:eval the-evaluator
(set-irrelevant-formals-ok t)
(set-irrelevant-formals-ok :warn)
(set-irrelevant-formals-ok nil)
]
}

@defform[(set-state-ok x)]{
If passed @scheme[t], @scheme[STATE] will be allowed as a formal parameter.
@examples[
#:eval the-evaluator
(set-state-ok nil)
(set-state-ok t)
]
}

