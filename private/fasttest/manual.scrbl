#lang scribble/doc
@(require scribble/manual scribble/eval scheme/sandbox
          "private/utils.rkt")
@(require (cce scribble))
@(require (for-label scheme/base
                     (this-package-in random)
                     (this-package-in rackunit)))

@(define the-evaluator
   (make-scribble-evaluator 'scheme/base
                            #:requires
                            (list "random.rkt" "rackunit.rkt")))

@title{@bold{FastTest} Random Test Cases in
       @scheme[(planet #,(this-package-version-symbol))]}

@table-of-contents[]

@section{Random Distributions}

@defmodule/this-package[random]

This module provides utilities for generating random values.

@subsection{Booleans}

@defproc[(random-boolean [p (prob/c 0 1) 1/2]) boolean?]{

Produces @scheme[#t] with probability @scheme[p], and @scheme[f] with
probability @scheme[(- 1 p)].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean 1/4)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean 3/4)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean 0)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean 1)))
]

}

@defproc[(random-boolean/fair) boolean?]{

Produces @scheme[#t] or @scheme[#f] with equal probability.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean/fair)))
]

}

@defproc[(random-boolean/bernoulli [p (prob/c 0 1)]) boolean?]{

Produces @scheme[#t] with probability @scheme[p], and @scheme[f] with
probability @scheme[(- 1 p)].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean/bernoulli 1/4)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean/bernoulli 3/4)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean/bernoulli 0)))
(random-seed 1)
(build-list 20 (lambda (i) (random-boolean/bernoulli 1)))
]

}

@subsection{Numbers}

@deftogether[(
@defproc[(random-natural) natural-number/c]
@defproc[(random-integer) exact-integer?]
@defproc[(random-rational) (and/c rational? exact?)]
@defproc[(random-exact) (and/c number? exact?)]
@defproc[(random-positive-real) (and/c inexact-real? positive?)]
@defproc[(random-real) inexact-real?]
@defproc[(random-inexact) (and/c number? inexact?)]
@defproc[(random-number) number?]
)]{

These procedures represent distributions over various subsets of the Scheme
numbers.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-natural)))
(random-seed 1)
(build-list 20 (lambda (i) (random-integer)))
(random-seed 1)
(build-list 20 (lambda (i) (random-rational)))
(random-seed 1)
(build-list 20 (lambda (i) (random-exact)))
(random-seed 1)
(build-list 20 (lambda (i) (random-positive-real)))
(random-seed 1)
(build-list 20 (lambda (i) (random-real)))
(random-seed 1)
(build-list 20 (lambda (i) (random-inexact)))
(random-seed 1)
(build-list 20 (lambda (i) (random-number)))
]

}

@defproc[(random-integer/uniform [lo exact-integer?]
                                 [hi (and/c exact-integer? (>=/c lo))])
         (integer-in lo hi)]{

Produces an integer chosen uniformly in the range [@scheme[lo],@scheme[hi]],
inclusive.

@examples[
#:eval the-evaluator
(build-list 20 (lambda (i) (random-integer/uniform 0 19)))
(build-list 20 (lambda (i) (random-integer/uniform -1000 1000)))
(build-list 20 (lambda (i) (random-integer/uniform 10 10)))
]

Note: out-of-order arguments cause an exception!
@interaction[
#:eval the-evaluator
(build-list 10 (lambda (i) (random-integer/uniform 1 -1)))
]

}

@defproc[(random-natural/binomial [n natural-number/c] [p (prob/c 0 1)])
         (integer-in 0 n)]{

Produces the number of successes (@scheme[#t]) in @scheme[n] Bernoulli trials
with probability @scheme[p] (@scheme[(random-boolean/bernoulli p)]).

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/binomial 20 1/2)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/binomial 20 1/4)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/binomial 20 3/4)))
]

}

@defproc[(random-natural/geometric [p (prob/c)] [base (one-of/c 0 1)])
         natural-number/c]{

Produces a natural number from a geometric distribution with minimum value
@scheme[base] and mean @scheme[(+ base (/ (- 1 p) p))].  The probability of each
possible result @scheme[n+base] is @scheme[(* p (expt (- 1 p) n))].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/geometric 1/2 0)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/geometric 1/2 1)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/geometric 1/10 0)))
]

}

@defproc[(random-natural/pascal [n exact-positive-integer?] [p (prob/c)])
         natural-number/c]{

Produces the sum of @scheme[n] geometric natural numbers based on probability
@scheme[p] (@scheme[(random-natural/geometric p 0)]).

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/pascal 10 1/2)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/pascal 10 1/10)))
]

}

@defproc[(random-natural/poisson [r (and rational? positive?)])
         natural-number/c]{

Produces a natural number with a Poisson distribution, meaning the number of
events occurring in a unit of time if the events occur independently and with an
average rate of @scheme[r] per unit of time.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/poisson 1)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/poisson 10)))
(random-seed 1)
(build-list 20 (lambda (i) (random-natural/poisson 100)))
]

}

@defproc[(random-integer/skellam [r1 (and rational? positive?)]
                                 [r2 (and rational? positive?) r1])
         exact-integer?]{

Produces the difference between two Poisson random variables, chosen with rates
@scheme[r1] and @scheme[r2] respectively.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-integer/skellam 10)))
(random-seed 1)
(build-list 20 (lambda (i) (random-integer/skellam 10 20)))
(random-seed 1)
(build-list 20 (lambda (i) (random-integer/skellam 20 10)))
]

}

@defproc[(random-real/uniform [lo real?] [hi (and/c real? (>=/c lo))])
         (real-in lo hi)]{

Produces an inexact real number from a uniform distribution in the range
[@scheme[lo],@scheme[hi]], inclusive.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-real/uniform -1/2 1/2)))
(random-seed 1)
(build-list 20 (lambda (i) (random-real/uniform 10.0 20.0)))
(random-seed 1)
(build-list 20 (lambda (i) (random-real/uniform 0.05 0.05)))
]

Note: out-of-order arguments cause an exception!
@interaction[
#:eval the-evaluator
(build-list 10 (lambda (i) (random-real/uniform 0.5 -0.5)))
]

}

@subsection{Textual Data}

@defproc[(random-char) char?]{

Produces a random lower-case letter.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-char)))
]

}

@defproc[(random-string [#:char make-char (-> char?) random-char]
                        [#:len len natural-number/c (random-natural/poisson 4)])
         string?]{

Produces a string of length @scheme[len] with characters generated by
@scheme[make-char].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-string)))
(random-seed 1)
(build-list
 20
 (lambda (i)
   (random-string #:char (lambda () (random-choice #\A #\B))
                  #:len 4)))
]

}

@defproc[(random-symbol [#:string string string? (random-string)]) symbol?]{

Produces an interned symbol with underlying string @scheme[string].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-symbol)))
(random-seed 1)
(build-list 20 (lambda (i) (random-symbol #:string "cat")))
]

}

@defproc[(random-keyword [#:string string string? (random-string)]) keyword?]{

Produces an interned keyword with underlying string @scheme[string].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-keyword)))
(random-seed 1)
(build-list 20 (lambda (i) (random-keyword #:string "cat")))
]

}

@subsection{Lists and S-expressions}

@defproc[(random-list [thunk (-> any/c)]
                      [#:len len natural-number/c (random-natural/poisson 4)])
         list?]{

Produces a list of length @scheme[len] of elements generated by @scheme[thunk].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-list random-string)))
(random-seed 1)
(build-list 20 (lambda (i) (random-list random-string #:len 4)))
]

}

@defproc[(random-atom) (not/c pair?)]{

Produces a random boolean, symbol, character, number, or string.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-atom)))
]

}

@defproc[(random-sexp [#:atom make-atom (-> (not/c pair?)) random-atom]
                      [#:improper improper? boolean? #f]
                      [#:size size natural-number/c (random-natural/poisson 4)])
         any/c]{

Produces a random @scheme[cons]-tree (if @scheme[improper?]) or nested
@scheme[list] (if not) containing @scheme[size] atoms generated by
@scheme[make-atom].

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 10 (lambda (i) (random-sexp)))
(random-seed 1)
(build-list 10 (lambda (i) (random-sexp #:atom random-symbol
                                        #:improper #t
                                        #:size 4)))
]

}

@subsection{Choices}

@defproc[(random-choice [v any/c] ...+) (one-of/c v ...+)]{

Produces one of its arguments, chosen fairly.

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-choice 1 2 3 4 5 6 7 8 9 10)))
(random-seed 1)
(build-list 20 (lambda (i) (random-choice 'north 'south 'east 'west)))
(random-seed 1)
(build-list 20 (lambda (i) (random-choice "single")))
]

}

@defproc[(random-choice-weighted [alist (listof (cons/c (>/c 0) any/c))])
         (one-of/c (map cdr alist))]{

Produces one of the values (@scheme[cdr]) of @scheme[alist], weighted by their
keys (@scheme[car]).

@examples[
#:eval the-evaluator
(random-seed 1)
(build-list 20 (lambda (i) (random-choice-weighted '((1 . 1) (2 . 2) (3 . 3) (4 . 4)))))
(random-seed 1)
(build-list 20 (lambda (i) (random-choice-weighted '((1 . A) (10 . B)))))
]

}

@defform/subs[
(random-case clause ...+)
([clause expr (code:line expr #:weight weight)])
]{

Chooses one of the @scheme[expr]s and evaluates it in tail position.  Each
expression is chosen with the associated @scheme[weight], default 1.

@defexamples[
#:eval the-evaluator
(random-seed 1)
(for ([i (in-range 0 20)])
  (random-case (printf "Hello!\n")
               (printf "Goodbye.\n")))
(define (random-tree)
  (random-case 'leaf #:weight 2
               (list 'node (random-tree) (random-tree)) #:weight 1))
(random-seed 1)
(build-list 20 (lambda (i) (random-tree)))
]

}

@subsection{Probabilities}

@defproc*[
([(prob/c) flat-contract?]
 [(prob/c [inc (one-of/c 0 1)]) flat-contract?]
 [(prob/c [zero 0] [one 1]) flat-contract?])
]{

This contract is intended to accept probability values.  It accepts real numbers
in the range [0,1], inclusive of zero and/or one if passed as an argument, and
exclusive otherwise.

}

@section{RackUnit Integration}

@defmodule/this-package[rackunit]

This module provides the @scheme[test-random] macro for generating RackUnit
test suites that generate and record random inputs.

@defform/subs[
(test-random opt ...
  ([lhs rhs bind-opt ...]
   ...)
  body ...+)
([opt (code:line #:name name)
      (code:line #:repeat repeat)
      (code:line #:limit limit)]
 [bind-opt (code:line #:where bind-where)
           (code:line #:limit bind-limit)])
]{

The @scheme[test-random] form constructs a RackUnit @scheme[test-suite] that
conducts multiple trials of @scheme[body ...+].  During a trial, each variable
@scheme[lhs] is bound to the value of the corresponding expression @scheme[rhs].
Bindings proceed left-to-right with @scheme[let*]-scope, and each @scheme[rhs]
is re-run to permit new random values.

The suite conducts @scheme[repeat] trials (default 100).

After each @scheme[rhs] is run, its corresponding @scheme[where] expression (if
any) is run; if @scheme[where] evaluates to @scheme[#t], the @scheme[rhs] is
re-run.  Each @scheme[rhs] is run at most the minimum of @scheme[limit] and the
corresponding @scheme[bind-limit], defaulting to 10000.

@examples[
#:eval the-evaluator
(require rackunit rackunit/text-ui)

(random-seed 1)
(run-tests
 (test-random
  ([x (random-integer)]
   [y (random-integer)])
  (check-pred integer? (+ x y))))

(random-seed 1)
(run-tests
 (test-random #:name "test w/ options"
              #:limit 4
              #:repeat 5
  ([x (random-integer)]
   [y (random-integer)])
  (check-pred positive? (+ x y))))

(random-seed 1)
(run-tests
 (test-random #:name "test w/ bind options"
              #:repeat 4
  ([x (random-integer)
      #:where (odd? x)
      #:limit 4]
   [y (random-integer)
      #:where (odd? y)
      #:limit 4])
  (check-pred even? (+ x y))))
]

}
