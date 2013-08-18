#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title[#:tag "booleans"]{Booleans}

@defthing[t booleanp]{
The boolean truth value.
}

@defthing[nil booleanp]{
The boolean falsehood value.
}

@defform[(and bool ...)]{
Returns @scheme[nil] if and only if one or more of its arguments is @scheme[nil]. Otherwise, returns the last argument given. If given no arguments, returns @scheme[t].
@examples[
#:eval the-evaluator
(and)
(and t t t)
(and t nil t)
(and 1 2 3 4 5)
]
}

@defproc[(booleanp [x t]) t]{
Determines if @scheme[x] is either @scheme[t] or @scheme[nil].
@examples[
#:eval the-evaluator
(booleanp t)
(booleanp nil)
(booleanp 'yes)
]
}

@defproc[(iff [p t] [q t]) t]{
Returns @scheme[t] if and only if @scheme[p] and @scheme[q] are either both @scheme[nil] or both non-@scheme[nil].
@examples[
#:eval the-evaluator
(iff t t)
(iff nil nil)
(iff t nil)
(iff nil t)
(iff 5 6)
(iff 5 nil)
]
}

@defproc[(implies [p t] [q t]) t]{
Returns @scheme[nil] if and only if @scheme[q] is @scheme[nil] and @scheme[p] is non-nil. Otherwise, returns @scheme[t].
@examples[
#:eval the-evaluator
(implies t t)
(implies t nil)
(implies nil t)
(implies nil nil)
]
}

@defproc[(not [x t]) t]{
If @scheme[x] is @scheme[nil], then returns @scheme[t]. Otherwise, returns @scheme[nil].
@examples[
#:eval the-evaluator
(not t)
(not nil)
(not 5)
(not 0)
]
}

@defform[(or x ...)]{
Returns @scheme[t] if and only if one or more of its arguments is @scheme[t]. Otherwise, returns the last argument given. If given no arguments, returns @scheme[nil].
@examples[
#:eval the-evaluator
(or)
(or nil nil t)
(or nil nil 5 6)
]
}
