#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title[#:tag "alists"]{Association Lists}

An association list is a list of @scheme[cons]-pairs, representing a table in
which the @scheme[car] of each pair maps to its @scheme[cdr].

@defproc[(acons [key t] [datum t] [alist (alistp alist)]) alistp]{
For creating association lists. @scheme[(acons key datum alist)] = @scheme[(cons (cons key datum) alist)]
@examples[
#:eval the-evaluator
(acons "hello" 5 nil)
(acons "hello" 5 (list (cons "hello2" 6)))
]
}

@defproc[(alistp [any t]) t]{
Reports whether @scheme[x] is a proper association list.
@examples[
#:eval the-evaluator
(alistp nil)
(alistp (acons "four" 4 nil))
(alistp t)
]
}

@defproc[(assoc [x t] [alist (if (eqlablep x) (alistp alist) (eqlable-alistp alist))]) t]{
Lookup function for association lists, using @scheme[eql] as test. Returns the first pair in @scheme[alist] whose key matches @scheme[x]. Otherwise returns @scheme[nil].
@examples[
#:eval the-evaluator
(assoc 'a (list (cons 'a 'b) (cons 'c 'd)))
(assoc 'z (list (cons 'a 'b) (cons 'c 'd)))
]
}

@defproc[(assoc-eq [x t] [alist (if (symbolp x) (alistp alist) (symbol-alistp alist))]) t]{
Like @scheme[assoc], but uses @scheme[eq] as the test.
@examples[
#:eval the-evaluator
(assoc-eq 'a (list (cons 'a 'b) (cons 'c 'd)))
(assoc-eq 'z (list (cons 'a 'b) (cons 'c 'd)))
]
}

@defproc[(assoc-equal [x t] [alist (alistp alist)]) t]{
Like @scheme[assoc], but uses @scheme[equal] as the test.
@examples[
#:eval the-evaluator
(assoc-equal "c" (list (cons "a" "b") (cons "c" "d")))
(assoc-equal "z" (list (cons "a" "b") (cons "c" "d")))
]
}

@defproc[(assoc-keyword [key t] [l (keyword-value-listp l)]) t]{
Lookup function for a @scheme[keyword-value-listp]
@examples[
#:eval the-evaluator
(assoc-keyword :b (list :a 1 :b 2 :c 3))
]
}

@defproc[(assoc-string-equal [str (and (stringp str) (standard-char-listp (coerce str 'list)))] [alist (standard-string-alistp alist)]) t]{
Lookup function for association lists that have strings for keys
@examples[
#:eval the-evaluator
(assoc-string-equal "c" (list (cons "a" "b") (cons "c" "d")))
(assoc-string-equal "z" (list (cons "a" "b") (cons "c" "d")))
]
}

@defproc[(eqlable-alistp [x t]) t]{
Recognizer for association lists of @scheme[eqlablep] items
@examples[
#:eval the-evaluator
(eqlable-alistp nil)
(eqlable-alistp (list (cons 4 6) (cons 7 8)))
(eqlable-alistp t)
]
}

@defproc[(put-assoc-eq [name t] [val t] [alist (if (symbolp name) (alistp alist) (symbol-alistp alist))]) t]{
Modify an association list by associating a value with a key. Use this function for association lists whose keys are symbols. 
@examples[
#:eval the-evaluator
(put-assoc-eq 'a 5 nil)
(put-assoc-eq 'a 5 (list (cons 'a 4) (cons 'b 6)))
]
}

@defproc[(put-assoc-eql [name t] [val t] [alist (if (eqlable name) (alistp alist) (eqlable-alistp alist))]) t]{
Modify an association list by associating a value with a key. Use this function for association lists whose keys are numbers, symbols, or characters.
@examples[
#:eval the-evaluator
(put-assoc-eql 'a 5 nil)
(put-assoc-eql 'a 5 (list (cons 'a 4) (cons 'b 6)))
(put-assoc-eql "string" 5 (list (cons 1 'a)))
]
}

@defproc[(put-assoc-equal [name t] [val t] [alist (alistp alist)]) t]{
Modify an association list by associating a value with a key.
@examples[
#:eval the-evaluator
(put-assoc-equal "a" 5 nil)
(put-assoc-equal "a" 5 (list (cons "a" 4) (cons "b" 6)))
]
}

@defproc[(rassoc [x t] [alist (if (eqlablep x) (alistp alist) (r-eqlable-alistp alist))]) t]{
Lookup function for association lists, using @scheme[eql] as test. Returns the first pair in @scheme[alist] whose value matches @scheme[x]. Otherwise returns @scheme[nil]. This function is similar to @scheme[assoc], but whereas @scheme[assoc] checks for a pair whose @scheme[car] matches @scheme[x], @scheme[rassoc] checks for a pair whose @scheme[cdr] matches.
@examples[
#:eval the-evaluator
(rassoc 'd (list (cons 'a 'b) (cons 'c 'd)))
(rassoc 'z (list (cons 'a 'b) (cons 'c 'd)))
]
}

@defproc[(rassoc-eq [x t] [lst (if (symbolp x) (alisp lst) (r-symbol-alisp lst))]) t]{
Like @scheme[rassoc], but uses @scheme[eq] as the test.
@examples[
#:eval the-evaluator
(rassoc-eq 'd (list (cons 'a 'b) (cons 'c 'd)))
(rassoc-eq 'z (list (cons 'a 'b) (cons 'c 'd)))
]
}

@defproc[(rassoc-equal [x t] [lst (alistp lst)]) t]{
Like @scheme[rassoc], but uses @scheme[equal] as the test.
@examples[
#:eval the-evaluator
(rassoc-equal "d" (list (cons "a" "b") (cons "c" "d")))
(rassoc-equal "z" (list (cons "a" "b") (cons "c" "d")))
]
}

@defproc[(standard-string-alistp [x t]) t]{
Reports whether @scheme[x] is an association list of @scheme[stringp] elements, in which each string contains only @scheme[standard-char-p] characters.
@examples[
#:eval the-evaluator
(standard-string-alistp (list (cons "abc" 1) (cons "def" 2)))
]
}

@defproc[(strip-cars [x (alistp x)]) t]{
Returns a list containing the @scheme[car]s of all the pairs in the given association list.
@examples[
#:eval the-evaluator
(strip-cars (list (cons 'a 'b) (cons 'c 'd)))
]
}

@defproc[(strip-cdrs [x (alistp x)]) t]{
Returns a list containing the @scheme[cdr]s of all the pairs in the given association list.
@examples[
#:eval the-evaluator
(strip-cdrs (list (cons 'a 'b) (cons 'c 'd)))
]
}

@defproc[(sublis [alst (eqlable-alistp alst)] [tree t]) t]{
Replaces every leaf of @scheme[tree] with the result of looking that leaf up in the given @scheme[alist].
}

@defproc[(symbol-alistp [x t]) t]{
Reports whether @scheme[x] is an association list of @scheme[symbolp] elements.
@examples[
#:eval the-evaluator
(symbol-alistp (list (cons 'a 'b) (cons 'c 'd)))
(symbol-alistp (list 'ab 'cd 'ef))
]
}

@defproc[(pairlis$ [x (true-listp x)] [y (true-listp y)]) t]{
Zipper together two lists
@examples[
#:eval the-evaluator
(pairlis$ (list 'a 'b 'c) (list 1 2 3))
(pairlis$ nil nil)
]
}
