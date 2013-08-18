#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title[#:tag "sequences"]{Sequences}

@defproc[(coerce [x (if (equal y 'list) (stringp x) (if (equal y 'string) (character-listp x) nil))] [y t]) t]{
Coerces a character list into a string or a string into a list. The second argument must be either @scheme['string] or @scheme['list]. 
@examples[
#:eval the-evaluator
(coerce "hello" 'list)
(coerce '(#\h #\e #\l #\l #\o) 'string)
]
}

@defform[(concatenate sym seq ...)]{
Concatenates strings or lists together. If the first argument is @scheme['string], @scheme[concatenate] will accept strings to concatenate. If the first argument is @scheme['list], it will accept lists.
@examples[
#:eval the-evaluator
(concatenate 'string "ab" "cd" "ef")
(concatenate 'string "ab")
(concatenate 'string)
(concatenate 'list '(a b) '(c d) '(e f))
(concatenate 'list)
]
}

@defproc[(length [x (or (true-listp x) (stringp x))]) t]{
Returns the number of elements in the given list, or the number of characters in the given string.
@examples[
#:eval the-evaluator
(length (list 1 2 3 4 5))
(length "hello")
]
}

@defproc[(position [x t] [seq (or (stringp seq) (and (true-listp seq) (or (eqlablep x) (eqlable-listp seq))))]) t]{
Determines the (0-based) position in @scheme[seq] at which @scheme[x] first occurs. Uses @scheme[eql] for comparisons. Returns @scheme[nil] if @scheme[x] is not found.
@examples[
#:eval the-evaluator
(position 1 (list 3 2 1))
(position #\o "lion")
(position 5 (list 1 2 3))
]
}

@defproc[(position-eq [x t] [lst (and (true-listp lst) (or (symbolp x) (symbol-listp lst)))]) t]{
Determines the (0-based) position in @scheme[lst] at which @scheme[x] first occurs. Uses @scheme[eq] for comparisons. Returns @scheme[nil] if @scheme[x] is not found.
@examples[
#:eval the-evaluator
(position-eq 'a (list 'c 'b 'a))
]
}

@defproc[(position-equal [x t] [seq (or (stringp seq) (true-listp seq))]) t]{
Determines the (0-based) position in @scheme[seq] at which @scheme[x] first occurs. Uses @scheme[equal] for comparisons. Returns @scheme[nil] if @scheme[x] is not found.
@examples[
#:eval the-evaluator
(position-equal 'a (list 'c 'b 'a))
(position-equal #\o "lion")
(position-equal "5" (list "1" "3" "5"))
]
}

@defproc[(subseq [seq (or (true-listp seq) (stringp seq))] [i ((integerp i) (<= 0 i))] [j (and (or (null j) (and (integerp j) (<= j (length seq)))) (<= i (or j (length seq))))]) t]{
Returns a subsection of the given sequence, starting at the @scheme[i]th (inclusive, 0-based) position and ending at the @scheme[j]th (exclusive) position. If @scheme[j] is @scheme[nil], then @scheme[subseq] will return the list to the end.
@examples[
#:eval the-evaluator
(subseq "0123456789" 2 6)
(subseq (list 0 1 2 3 4 5) 2 4)
(subseq (list 0 1 2 3 4 5) 2 nil)
]
}

@defproc[(substitute [new t] [old t] [seq (or (and (stringp seq) (characterp new)) (and (true-listp seq) (or (eqlablep old) (eqlable-listp seq))))]) t]{
Substitutes every occurrence of @scheme[old] with @scheme[new] in the given list or string.
@examples[
#:eval the-evaluator
(substitute 2 1 (list 1 1 1 3 1 1 1))
(substitute #\Z #\a "abcdabcdabcd")
]
}
