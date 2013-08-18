#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Bitwise Operations}

@defproc[(ash [x (integerp x)] [y (integerp y)]) t]{
Multiplies @scheme[x] by 2^@scheme[y]. This is equivalent to shifting the binary representation of @scheme[x] to the left by @scheme[y] bits.
@examples[
#:eval the-evaluator
(ash 1 3)
(ash 5 2)
(ash '5 2)
]
}

@deftogether[(
@defform[(logand x ...)]{}
@defform[(logior x ...)]{}
@defform[(logxor x ...)]{}
@defform[(logeqv x ...)]{}
)]{
Bitwise logical operations on numbers. These forms compute the bitwise AND, inclusive OR, exclusive OR, and equivalence (a.k.a. exclusive NOR), respectively. These macros expand into calls of binary functions such as @scheme[binary-logand], @scheme[binary-logior], etc. The guards of these functions require that all inputs be integers. When passed one argument, these functions return the argument unchanged. When passed no arguments, @scheme[logand] and @scheme[logeqv] return -1, while @scheme[logior] and @scheme[logxor] return 0.
@examples[
#:eval the-evaluator
(logand)
(logior)
(logxor)
(logeqv)
(logand 1)
(logand 10 6)
(logior 10 5)
(logxor 15 9)
(logeqv 5 6)
(logior "5")
]
}

@defproc[(lognand [x (integerp x)] [y (integerp y)]) t]{
Computes the bitwise logical NAND of the two given numbers.
@examples[
#:eval the-evaluator
(lognand 10 6)
]
}

@defproc[(lognor [x (integerp x)] [y (integerp y)]) t]{
Computes the bitwise logical NOR of @scheme[x] and @scheme[y].
@examples[
#:eval the-evaluator
(lognor 10 6)
]    
}

@defproc[(lognot [x (integerp x)]) t]{
Computes the bitwise logical NOT of the given number.
@examples[
#:eval the-evaluator
(lognot 5)
]
}

@defproc[(logbitp [i (and (integerp i) (>= i 0))] [j (integerp j)]) t]{
Returns the @scheme[i]th bit in the two's complement binary representation of @scheme[j].
@examples[
#:eval the-evaluator
(logbitp 3 15)
(logbitp 3 16)
(logbitp 0 1)
]
}

@defproc[(logcount [x (integerp x)]) t]{
Returns the number of "on" bits in the binary representation of @scheme[x].
@examples[
#:eval the-evaluator
(logcount -1)
(logcount 4)
(logcount 7)
]
}

@defproc[(logorc1 [x (integerp x)] [y (integerp y)]) t]{
Computes the bitwise logical Inclusive OR of @scheme[y] with the bitwise logical NOT of @scheme[x].
@examples[
#:eval the-evaluator
(logorc1 10 6)
]
}

@defproc[(logorc2 [x (integerp x)] [y (integerp y)]) t]{
Computes the bitwise logical Inclusive OR of @scheme[x] with the bitwise logical NOT of @scheme[y].
@examples[
#:eval the-evaluator
(logorc2 10 6)
]
}

@defproc[(logtest [x (integerp x)] [y (integerp y)]) t]{
Returns true if and only if @scheme[x] and @scheme[y] share a '1' bit somewhere in their binary representation (i.e. @scheme[(logand x y)] is not zero).
@examples[
#:eval the-evaluator
(logtest 4 15)
(logtest 4 16)
]
}
