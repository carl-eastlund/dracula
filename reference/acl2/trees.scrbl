#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Trees}

@defproc[(alphorder [x (atom x)] [y (atom y)]) t]{
Determines if @scheme[x] is lexicographically less than @scheme[y]. Works on any atomic input. 
@examples[
#:eval the-evaluator
(alphorder 6 4)
(alphorder 4 6)
(alphorder 4 4)
(alphorder "abc" "bcd")
(alphorder "bcd" "abc")
(alphorder #\a "a")
(alphorder "a" #\a)
]
}

@defproc[(atom [x t]) t]{
Returns true if @scheme[x] is not a @scheme[cons] pair.
@examples[
#:eval the-evaluator
(atom 4)
(atom 'hello)
(atom (cons 4 nil))
]
}

@defproc[(eq [x t] [y (or (symbolp x) (symbolp y))]) t]{
Tests two symbols for equality.
@examples[
#:eval the-evaluator
(eq 'yes 'yes)
(eq 'yes 'no)
]
}

@defproc[(eql [x t] [y (or (eqlablep x) (eqlablep y))]) t]{
Tests equality of two numbers, symbols, or characters.
@examples[
#:eval the-evaluator
(eql 'yes 'yes)
(eql 'yes 'no)
(eql 5 35/7)
(eql #\a #\b)
(eql #\5 5)
]
}

@defproc[(eqlablep [x t]) t]{
Returns @scheme[t] if and only if @scheme[x] is either a number, a symbol, or a character.
@examples[
#:eval the-evaluator
(eqlablep nil)
(eqlablep 4)
(eqlablep #\a)
(eqlablep 'symbol)
(eqlablep "string")
]
}

@defform[(equal x y)]{
Tests @scheme[x] and @scheme[y] for equality. Returns @scheme[t] if and only if they are the same value.
@examples[
#:eval the-evaluator
(equal "yes" "yes")
(equal 'yes "no")
(equal 'yes "yes")
]
}

@defproc[(identity [x t]) t]{
The identity function. Returns its argument unchanged.
@examples[
#:eval the-evaluator
(identity 'x)
]
}

@defproc[(lexorder [a t] [b t]) t]{
Determines if the two given items are in lexicographic order.
@examples[
#:eval the-evaluator
(lexorder 6 4)
(lexorder 4 6)
(lexorder #\a #\b)
(lexorder #\b #\a)
(lexorder 'a 'b)
(lexorder 'b 'a)
(lexorder "abc" "bcd")
(lexorder "bcd" "abc")
(lexorder (list 1 2) (list 3 4))
(lexorder (list 3 4) (list 1 2))
]
}

@deftogether[(
@defform[(caar x)]
@defform[(cdar x)]
@defform[(cadr x)]
@defform[(cddr x)]
@defform[(caaar x)]
@defform[(cdaar x)]
@defform[(cadar x)]
@defform[(cddar x)]
@defform[(caadr x)]
@defform[(cdadr x)]
@defform[(caddr x)]
@defform[(cdddr x)]
@defform[(caaaar x)]
@defform[(cdaaar x)]
@defform[(cadaar x)]
@defform[(cddaar x)]
@defform[(caadar x)]
@defform[(cdadar x)]
@defform[(caddar x)]
@defform[(cdddar x)]
@defform[(caaadr x)]
@defform[(cdaadr x)]
@defform[(cadadr x)]
@defform[(cddadr x)]
@defform[(caaddr x)]
@defform[(cdaddr x)]
@defform[(cadddr x)]
@defform[(cddddr x)]
)]{
Shorthand macros for compositions of @scheme[car] and @scheme[cdr].  For
instance, @scheme[(caddr x)] is equivalent to @scheme[(car (cdr (cdr x)))].

@examples[
#:eval the-evaluator
(defconst *tree*
  (cons (cons (cons (cons 0 1) (cons 2 3))
              (cons (cons 4 5) (cons 6 7)))
        (cons (cons (cons 8 9) (cons 10 11))
              (cons (cons 12 13) (cons 14 15)))))
(caar *tree*)
(cdar *tree*)
(cadr *tree*)
(cddr *tree*)
(caaar *tree*)
(cdaar *tree*)
(cadar *tree*)
(cddar *tree*)
(caadr *tree*)
(cdadr *tree*)
(caddr *tree*)
(cdddr *tree*)
(caaaar *tree*)
(cdaaar *tree*)
(cadaar *tree*)
(cddaar *tree*)
(caadar *tree*)
(cdadar *tree*)
(caddar *tree*)
(cdddar *tree*)
(caaadr *tree*)
(cdaadr *tree*)
(cadadr *tree*)
(cddadr *tree*)
(caaddr *tree*)
(cdaddr *tree*)
(cadddr *tree*)
(cddddr *tree*)
]

}

@defform[(quote ...)]{
@examples[
#:eval the-evaluator
(quote a)
(quote (1 2 3 4))
]
}

@defform[(quasiquote ...)]{
@examples[
#:eval the-evaluator
(quasiquote a)
(quasiquote (1 2 3 4))
]
}

@defform[(unquote ...)]{
@examples[
#:eval the-evaluator
(quote (list (unquote a) b c d))
]
}

@defproc[(subst [new t] [old (eqlablep old)] [tree t]) t]{
Substitutes every occurrence of @scheme[old] with @scheme[new] in the given @scheme[tree]. Uses @scheme[eql] as the test.
@examples[
#:eval the-evaluator
(subst 2 1 (list 1 1 1 3 1 1 1))
(subst 'z 'a (list 'a 'b (list 'd 'a (list 'a 'e)) 'a))
]
}

@defproc[(acl2-count [v t]) natp]{
Calculates the size of a value.  This is the default recursion
metric used for ACL2 termination proofs.

The size of a @scheme[cons]-pair is one more than the sum of the sizes of its
@scheme[car] and @scheme[cdr].  The size of an integer is its absolute value,
the size of a rational number is the sum of the sizes of its numerator and
denominator, and the size of a complex number is one more than the sum of the
sizes of its real and imaginary parts.  The size of a string is its length.  The
size of all other values (characters and symbols) is 0.

@examples[
#:eval the-evaluator
(acl2-count 3/4)
(acl2-count (complex 3 4))
(acl2-count "ABCD")
(acl2-count 'ABCD)
(acl2-count '(a b c d))
]

}
