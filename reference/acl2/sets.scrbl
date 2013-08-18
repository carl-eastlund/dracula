#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title[#:tag "sets"]{Sets}

@defproc[(add-to-set-eq [sym t] [lst (if (symbolp sym) (true-listp lst) (symbol-listp lst))]) t]{
Adds @scheme[sym] to @scheme[lst] if it is not already present. Uses @scheme[eq] for comparisons.
@examples[
#:eval the-evaluator
(add-to-set-eq 'hello nil)
(add-to-set-eq 'hello (list 'hello 'goodbye))
(add-to-set-eq "hello" (list 'hello 'goodbye))
]
}

@defproc[(add-to-set-eql [x t] [lst (if (eqlablep x) (true-listp lst) (eqlable-listp lst))]) t]{
Adds @scheme[x] to @scheme[lst] if it is not already present. Uses @scheme[eql] for comparisons. 
@examples[
#:eval the-evaluator
(add-to-set-eql 'hello nil)
(add-to-set-eql 'hello (list 'hello 'goodbye))
(add-to-set-eql "hello" (list 'hello 'goodbye))
]
}

@defproc[(add-to-set-equal [s t] [lst (true-listp lst)]) t]{
Adds @scheme[x] to @scheme[lst] if it is not already present. Uses @scheme[equal] for comparisons. 
@examples[
#:eval the-evaluator
(add-to-set-equal "hello" (list "hello"))
(add-to-set-equal 4 (list 1 2 3))
]
}

@defproc[(intersectp-eq [x (symbol-listp x)] [y (symbol-listp y)]) t]{
Determines if @scheme[x] and @scheme[y] share at least one element in common. Uses @scheme[eq] for comparisons.
@examples[
#:eval the-evaluator
(intersectp-eq (list 'a 'b 'c) (list 'x 'y 'z))
(intersectp-eq (list 'a 'b 'c) (list 'a 'y 'z))
]
}

@defproc[(intersectp-equal [x (true-listp x)] [y (true-listp y)]) t]{
Determines if @scheme[x] and @scheme[y] share at least one element in common. Uses @scheme[equal] for comparisons.
@examples[
#:eval the-evaluator
(intersectp-equal (list "a" "b" "c") (list "x" "y" "z"))
(intersectp-equal (list "a" "b" "c") (list "a" "y" "z"))
]
}

@defproc[(set-difference-eq [x (true-listp x)] [y (and (true-listp y) (or (symbol-listp x) (symbol-listp y)))]) t]{
Computes the set difference of @scheme[x] from @scheme[y]. Uses @scheme[eq] for comparisons. Use this function for sets of symbols.
@examples[
#:eval the-evaluator
(set-difference-eq (list 'a 'b 'c) (list 'a 'c))
(set-difference-eq (list "string") (list 'a 'c))
(set-difference-eq (list 'a 'c) (list "string"))
]
}

@defproc[(set-difference-equal [x (true-listp x)] [y (true-listp y)]) t]{
Computes the set difference of the lists @scheme[x] and @scheme[y]. Uses @scheme[equal] for comparisons.
@examples[
#:eval the-evaluator
(set-difference-equal (list "a" "b" "c") (list "a" "c"))
]
}

@defproc[(subsetp [x t] [y (if (eqlable-listp y) (true-listp x) (if (eqlable-listp x) (true-listp y) nil))]) t]{
Determines whether every element of @scheme[x] a @scheme[member] of @scheme[y].
@examples[
#:eval the-evaluator
(subsetp (list 1 2 3) (list 2 3))
(subsetp (list 1 2 3) (list 1 2 3 4 5))
(subsetp (list "1" "2") (list 1 2 3))
(subsetp (list 1 2) (list 1 "1" 2 "2"))
]
}

@defproc[(subsetp-equal [x (true-listp x)] [y (true-listp y)]) t]{
Like @scheme[subsetp], but uses @scheme[member-equal] to check each element for membership.
@examples[
#:eval the-evaluator
(subsetp-equal (list "a" "b" "c") (list "b" "c"))
(subsetp-equal (list "a" "b" "c") (list "a" "b" "c" "d" "e"))
]
}

@defproc[(union-eq [x (symbol-listp x)] [y (true-listp y)]) t]{
Creates the union of two lists of symbols. See @scheme[union-equal].
@examples[
#:eval the-evaluator
(union-eq (list 'a 'b 'c) (list 'c 'd 'e))
]
}

@defproc[(union-equal [x (true-listp x)] [y (true-listp y)]) t]{
Creates the union of two lists. Specifically, the resulting list is the same as one would get by first deleting the members of @scheme[y] from @scheme[x], and then concatenating the result to the front of @scheme[y]
@examples[
#:eval the-evaluator
(union-equal (list "a" "b" "c") (list "c" "d" "e"))
]
}

@defproc[(member [x t] [lst (if (eqlablep x) (true-listp l) (eqlable-listp l))]) t]{
If @scheme[x] is in @scheme[lst], returns @scheme[x] and all elements after it in @scheme[lst]. Otherwise returns @scheme[nil]. Uses @scheme[eql] as test.
@examples[
#:eval the-evaluator
(member 3 '(1 2 3 4 5))
(member 3 '(2 4 6 8 10))
(member "abc" '(1 2 3))
]
}

@defproc[(member-eq [x t] [lst (if (symbolp x) (true-listp lst) (symbol-listp lst))]) t]{
Like @scheme[member], but uses @scheme[eq] as test.
@examples[
#:eval the-evaluator
(member-eq 'a '(a b c))
(member-eq 'a '(x y z))
]
}

@defproc[(member-equal [a t] [lst (true-listp lst)]) t]{
Like @scheme[member], but uses @scheme[equal] as test.
@examples[
#:eval the-evaluator
(member-equal "a" '("a" "b" "c"))
(member-equal "a" '("x" "y" "z"))
(member-equal 3 '(1 2 3 4 5))
(member-equal "abc" (list "a" "b" "abc"))
]
}

@defproc[(no-duplicatesp [lst (eqlable-listp lst)]) t]{
Returns true if @scheme[lst] contains no duplicate elements. Uses @scheme[eql] as the test.
@examples[
#:eval the-evaluator
(no-duplicatesp (list 1 2 3))
(no-duplicatesp (list 1 2 1))
]
}

@defproc[(no-duplicatesp-equal [lst (true-listp lst)]) t]{
Like @scheme[no-duplicatesp], but uses @scheme[equal] as the test.
@examples[
#:eval the-evaluator
(no-duplicatesp-equal (list "a" "b" "c"))
(no-duplicatesp-equal (list "a" "b" "a"))
]
}

@defproc[(remove [x t] [lst (if (eqlablep x) (true-listp lst) (eqlable-listp lst))]) t]{
Removes all occurrences of @scheme[x] from @scheme[lst], using @scheme[eql] as the test
@examples[
#:eval the-evaluator
(remove 3 (list 1 2 3 4))
(remove 3 (list 5 6 7 8))
(remove "abc" (list 1 2 3 4))
(remove 2 (list 1 2 "abc" 4))
]
}

@defproc[(remove-eq [x t] [lst (if (symbolp x) (true-listp lst) (symbol-listp lst))]) t]{
Removes all occurrences of @scheme[x] from @scheme[lst], using @scheme[eq] as the test
@examples[
#:eval the-evaluator
(remove-eq 'x (list 'w 'x 'y 'z))
]
}

@defproc[(remove-equal [x t] [lst (true-listp lst)]) t]{
@examples[
#:eval the-evaluator
(remove-equal "x" (list "w" "x" "y" "z"))
]
}

@defproc[(remove-duplicates [lst (or (stringp l) (eqlable-listp l))]) t]{
Remove duplicate items in the given list or string, using @scheme[eql] as the test.
@examples[
#:eval the-evaluator
(remove-duplicates (list 1 2 2 3 2 4))
(remove-duplicates "abCdCeCfFgh")
]
}

@defproc[(remove-duplicates-equal [lst (true-listp lst)]) t]{
Remove duplicate items in the given list or string, using @scheme[equal] as the test.
@examples[
#:eval the-evaluator
(remove-duplicates-equal (list "a" "b" "b" "c" "d" "b"))
(remove-duplicates-equal (list 1 2 2 3 2 4))
]
}

@defproc[(remove1 [x t] [lst (if (eqlablep x) (true-listp lst) (eqlable-listp lst))]) t]{
like @scheme[remove], but only removes the first instance
@examples[
#:eval the-evaluator
(remove1 3 (list 1 2 3 4))
(remove1 3 (list 5 6 7 8))
(remove1 "abc" (list 1 2 3 4))
(remove1 2 (list 1 2 "abc" 4))
]
}

@defproc[(remove1-eq [x t] [lst (if (symbolp x) (true-listp lst) (symbol-listp lst))]) t]{
like @scheme[remove-eq], but only removes the first instance
@examples[
#:eval the-evaluator
(remove1-eq 'x (list 'w 'x 'x 'y 'x 'z))
]
}

@defproc[(remove1-equal [x t] [lst (true-listp lst)]) t]{
Like @scheme[remove-equal], but only removes the first instance
@examples[
#:eval the-evaluator
(remove1-equal "x" (list "w" "x" "x" "y" "x" "z"))
]
}
