#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Rational and Complex Arithmetic}

@defform[(* num ...)]{
Multiplies the given numbers together, taking any number of arguments. This is a macro that expands into calls of the function @scheme[binary-*]
@examples[
#:eval the-evaluator
(*)
(* 2)
(* 1 2 3)
]
}

@defform[(+ num ...)]{
Adds the given numbers together, taking any number of arguments. This is a macro that expands into calls of the function @scheme[binary-+].
@examples[
#:eval the-evaluator
(+)
(+ 2)
(+ 1 2 3)
]
}

@defform[(- num num)]{
Subtracts the given numbers. If only one argument is given, returns the negation of the input.
@examples[
#:eval the-evaluator
(- 5)
(- 5 3)
]
}

@defform[(/ num num)]{
Divides the first number by the second. If only one argument is given, returns the reciprocal of the input.
@examples[
#:eval the-evaluator
(/ 2)
(/ 16 4)
]
}

@defform[(1+ num)]{
Adds 1 to the given number.
@examples[
#:eval the-evaluator
(1+ 1)
]
}

@defform[(1- num)]{
Subtracts 1 from the given number.
@examples[
#:eval the-evaluator
(1- 1)
]
}

@defproc[(binary-* [x (acl2-numberp x)] [y (acl2-numberp y)]) t]{
Takes exactly two numbers and multiplies them together.
@examples[
#:eval the-evaluator
(binary-* 9 8)
]
}

@defproc[(binary-+ [x (acl2-numberp x)] [y (acl2-numberp y)]) t]{
Takes exactly two numbers and adds them together.
@examples[
#:eval the-evaluator
(binary-+ 9 8)
]
}

@defproc[(/= [x (acl2-numberp x)] [y (acl2-numberp y)]) t]{
Determines if the given numbers are not equal. Logically equivalent to @scheme[(not (equal x y))]
@examples[
#:eval the-evaluator
(/= 2 2)
(/= 3 2)
]
}

@defproc[(< [x (rationalp x)] [y (rationalp y)]) t]{
Determines if @scheme[x] is less than @scheme[y].
@examples[
#:eval the-evaluator
(< 1 2)
(< 2 1)
(< 2 2)
]
}

@defproc[(<= [x (rationalp x)] [y (rationalp y)]) t]{
Determines if @scheme[x] is less than or equal to @scheme[y].
@examples[
#:eval the-evaluator
(<= 1 2)
(<= 2 1)
(<= 2 2)
]
}

@defproc[(= [x (acl2-numberp x)] [y (acl2-numberp y)]) t]{
Determines if @scheme[x] is equal to @scheme[y]. This is like @scheme[equal], but has the guard that both of its arguments must be numbers. It usually executes more efficiently thatn @scheme[equal].
@examples[
#:eval the-evaluator
(= 1 2)
(= 2 1)
(= 2 2)
]
}

@defproc[(> [x (rationalp x)] [y (rationalp y)]) t]{
Determines if @scheme[x] is greater than @scheme[y].
@examples[
#:eval the-evaluator
(> 1 2)
(> 2 1)
(> 2 2)
]
}

@defproc[(>= [x (rationalp x)] [y (rationalp y)]) t]{
Determines if @scheme[x] is greater than or equal to @scheme[y].
@examples[
#:eval the-evaluator
(>= 1 2)
(>= 2 1)
(>= 2 2)
]
}

@defform[(acl2-numberp x)]{
Returns true if and only if @scheme[x] is a rational or complex rational number.
@examples[
#:eval the-evaluator
(acl2-numberp 1)
(acl2-numberp 12/5)
(acl2-numberp "no")
]
}

@defform[(complex-rationalp z)]{
Determines if @scheme[z] is a complex number consisting of rational parts.
@examples[
#:eval the-evaluator
(complex-rationalp 3)
(complex-rationalp (complex 3 0))
(complex-rationalp t)
(complex-rationalp (complex 3 1))
]
}

@defproc[(complex/complex-rationalp [z t]) t]{
For most cases, this is simply a macro abbreviating @scheme[complex-rationalp]. 
@examples[
#:eval the-evaluator
(complex/complex-rationalp 3)
(complex/complex-rationalp (complex 3 0))
(complex/complex-rationalp t)
(complex/complex-rationalp (complex 3 1))
]
}

@defproc[(zp [v natp]) t]{
This is a test for the base case (zero) of recursion on the natural numbers.
@examples[
@:eval the-evaluator
(zp 0)
(zp 1)
]
}

@defproc[(minusp [x (real/rationalp x)]) t]{
Determines whether @scheme[x] is a negative number.
@examples[
#:eval the-evaluator
(minusp 1)
(minusp -1)
]
}

@defproc[(natp [x t]) t]{
Determines if @scheme[x] is a natural number.
@examples[
#:eval the-evaluator
(natp 1)
(natp 0)
(natp -1)
]
}

@defproc[(oddp [x (integerp x)]) t]{
Determines if @scheme[x] is odd.
@examples[
#:eval the-evaluator
(oddp 3)
(oddp 2)
]
}

@defproc[(plusp [x (real/rationalp x)]) t]{
Determines if @scheme[x] is positive.
@examples[
#:eval the-evaluator
(plusp 1)
(plusp -1)
(plusp 1/2)
]
}

@defproc[(posp [x t]) t]{
Determines if @scheme[x] is a positive integer.
@examples[
#:eval the-evaluator
(posp 1)
(posp -1)
(posp 1/2)
(posp (complex 1 2))
(posp "asdf")
]
}

@defform[(rationalp x)]{
Determines if @scheme[x] is a rational number.
@examples[
#:eval the-evaluator
(rationalp 2/5)
(rationalp 2)
(rationalp (complex 1 2))
(rationalp "number")
]
}

@defform[(real/rationalp x)]{
In most cases, this is just a macro abbreviating @scheme[rationalp].
@examples[
#:eval the-evaluator
(real/rationalp 2/5)
(real/rationalp "number")
]
}

@defproc[(abs [x (real/rationalp x)]) t]{
Computes the absolute value of @scheme[x].
@examples[
#:eval the-evaluator
(abs 1)
(abs -1)
]
}

@defproc[(ceiling [i (real/rationalp i)] [j (and (real/rationalp j) (not (eql j 0)))]) t]{
Returns the smallest integer greater the value of @scheme[(/ i j)].
@examples[
#:eval the-evaluator
(ceiling 4 2)
(ceiling 4 3)
]
}

@defproc[(complex [n (rationalp n)] [i (rationalp i)]) t]{
Creates a complex number with real part @scheme[n] and imaginary part @scheme[i].
@examples[
#:eval the-evaluator
(complex 2 1)
(complex 2 0)
(complex 0 2)
(complex 1/2 3/2)
]
}

@defproc[(conjugate [x (acl2-numberp x)]) t]{
Computes the complex conjugate of @scheme[x] (the result of negating its imaginary part).
@examples[
#:eval the-evaluator
(conjugate (complex 3 1))
]
}

@defproc[(denominator [x (rationalp x)]) t]{
Returns the divisor of a rational number in lowest terms.
@examples[
#:eval the-evaluator
(denominator 5)
(denominator 5/3)
(denominator 10/6)
]
}

@defproc[(evenp [x (integerp x)]) t]{
Determines if @scheme[x] is even.
@examples[
#:eval the-evaluator
(evenp 1)
(evenp 2)
]
}

@defproc[(explode-nonnegative-integer [n (and (integerp n) (>= n 0))] [r (print-base-p r)]) l]{
Returns a list of characters representing @scheme[n] in base-@scheme[r], and appends @scheme[l] to the end.
@examples[
#:eval the-evaluator
(explode-nonnegative-integer 925 10 nil)
(explode-nonnegative-integer 325 16 nil)
(explode-nonnegative-integer 325 16 (list 'a 'b 'c))
(explode-nonnegative-integer 325 16 'a)
]
}

@defproc[(expt [i (acl2-numberp i)] [j (and (integerp j) (not (and (eql i 0) (< j 0))))]) t]{
Raises @scheme[i] to the @scheme[j]th power.
@examples[
#:eval the-evaluator
(expt 10 2)
(expt 10 -2)
]
}

@defproc[(fix [x t]) t]{
Coerces @scheme[x] to a number. If @scheme[x] is a number, @scheme[(fix x)] returns the argument unchanged. Otherwise, it returns 0.
@examples[
#:eval the-evaluator
(fix 20)
(fix 2/3)
(fix "hello")
(fix nil)
]
}

@defproc[(floor [i (real/rationalp i)] [j (and (real/rationalp j) (not (eql j 0)))]) t]{
Returns the greatest integer not exceeding the value of @scheme[(/ i j)].
@examples[
#:eval the-evaluator
(floor 4 2)
(floor 4 3)
]
}

@defproc[(ifix [x t]) t]{
Coerces @scheme[x] to an integer. If @scheme[x] is an integer, @scheme[(ifix x)] returns the argument unchanged. Otherwise, it returns 0.
@examples[
#:eval the-evaluator
(ifix 16)
(ifix 22/3)
(ifix "hello")
]
}

@defproc[(imagpart [i (acl2-numberp i)]) t]{
Returns the imaginary part of a complex number.
@examples[
#:eval the-evaluator
(imagpart (complex 3 2))
(imagpart 5)
]
}

@defform[(int= i j)]{
Checks to see if the two integers @scheme[i] and @scheme[j] are equal. This is like @scheme[equal] and @scheme[=], but with the added guard that the inputs are integers. This generally executes more efficiently on integers than @scheme[equal] or @scheme[=].
@examples[
#:eval the-evaluator
(int= 1 2)
(int= 2 1)
(int= 2 2)
]
}

@defproc[(integer-length [x (integerp x)]) t]{
Returns the number of bits in the two's complement binary representation of @scheme[x].
@examples[
#:eval the-evaluator
(integer-length 12)
(integer-length 1234)
]
}

@defform[(integerp x)]{
Determines whether @scheme[x] is an integer.
@examples[
#:eval the-evaluator
(integerp 12)
(integerp '12)
(integerp nil)
]
}

@defproc[(max [i (real/rationalp i)] [j (real/rationalp j)]) t]{
Returns the greater of the two given numbers.
@examples[
#:eval the-evaluator
(max 1 2)
(max 4 3)
]
}

@defproc[(min [i (real/rationalp i)] [j (real/rationalp j)]) t]{
Returns the lesser of the two given numbers.
@examples[
#:eval the-evaluator
(min 1 2)
(min 4 3)
]
}

@defproc[(mod [i (real/rationalp i)] [j (and (real/rationalp j) (not (eql j 0)))]) t]{
Computes the remainder of dividing @scheme[i] by @scheme[j].
@examples[
#:eval the-evaluator
(mod 4 2)
(mod 8 3)
]
}

@defproc[(nfix [x t]) t]{
Coerces @scheme[x] to a natural number. If @scheme[x] is a natural number, @scheme[(nfix x)] returns the argument unchanged. Otherwise, it returns 0.
@examples[
#:eval the-evaluator
(nfix 1)
(nfix -1)
(nfix 1/2)
(nfix "5")
]
}

@defproc[(nonnegative-integer-quotient [x (and (integerp x) (not (< x 0)))] [y (and (integerp j) (< 0 j))]) t]{
Returns the integer quotient of @scheme[x] and @scheme[y]. That is, @scheme[(nonnegative-integer-quotient x y)] is the largest integer @scheme[k] such that @scheme[(* j k)] is less than or equal to @scheme[x].
@examples[
#:eval the-evaluator
(nonnegative-integer-quotient 14 3)
(nonnegative-integer-quotient 15 3)
]
}

@defproc[(numerator [x (rationalp x)]) t]{
Returns the dividend of a rational number in lowest terms.
@examples[
#:eval the-evaluator
(numerator 4)
(numerator 6/7)
(numerator 4/8)
]
}

@defproc[(realfix [x t]) t]{
Coerces @scheme[x] to a real number. If @scheme[x] satisfies @scheme[(real/rationalp x)], then it returns the argument unchanged. Otherwise, returns 0.
@examples[
#:eval the-evaluator
(realfix 2/5)
(realfix (complex 3 2))
(realfix "5")
]
}

@defproc[(realpart [x (acl2-numberp x)]) t]{
Returns the real part of a complex number.
@examples[
#:eval the-evaluator
(realpart (complex 3 2))
(realpart 1/2)
]
}

@defproc[(rem [i (real/rationalp i)] [j (and (real/rationalp j) (not (eql j 0)))]) t]{
Calculates the remainder of @scheme[(/ i j)] using @scheme[truncate].
@examples[
#:eval the-evaluator
(rem 4 2)
(rem 8 3)
]
}

@defproc[(rfix [x t]) t]{
Coerces @scheme[x] into a rational number. If @scheme[x] is a rational number, then it returns @scheme[x] unchanged. Otherwise, it returns 0.
@examples[
#:eval the-evaluator
(rfix 2/5)
(rfix (complex 3 2))
(rfix "5")
]
}

@defproc[(round [i (real/rationalp i)] [j (real/rationalp j)]) t]{
Rounds @scheme[(/ i j)] to the nearest integer. When the quotient is exactly halfway between to integers, it rounds to the even one.
@examples[
#:eval the-evaluator
(round 4 2)
(round 3 2)
(round 5 2)
(round 4 3)
]
}

@defproc[(signum [x (real/rationalp x)]) t]{
Returns 0 if @scheme[x] is 0, -1 if it is negative, and 1 if it is positive.
@examples[
#:eval the-evaluator
(signum 5)
(signum 0)
(signum -5)
]
}

@defproc[(truncate [i (real/rationalp i)] [j (and (real/rationalp j) (not (eql j 0)))]) t]{
Computes @scheme[(/ i j)] and rounds down to the nearest integer.
@examples[
#:eval the-evaluator
(truncate 5 2)
(truncate 4 2)
(truncate 19 10)
(truncate 1 10)
]
}

@defproc[(unary-- [x (acl2-numberp x)]) t]{
Computes the negative of the input. 
@examples[
#:eval the-evaluator
(unary-- 5)
(unary-- -5)
(unary-- (complex 1 -2))
]
}

@defproc[(unary-/ [x (and (acl2-numberp x) (not (eql x 0)))]) t]{
Computes the reciprocal of the input.
@examples[
#:eval the-evaluator
(unary-/ 5)
(unary-/ 1/5)
(unary-/ (complex 1 2))
]
}
