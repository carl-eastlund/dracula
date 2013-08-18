#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Lists}

List functions can also be found in the sections on @secref["alists"],
@secref["sets"], and @secref["sequences"].

@defform[(cons x y)]{
Creates a new pair containing x and y. 
@examples[
#:eval the-evaluator
(cons 1 nil)
(cons 2 (cons 3 nil))
(cons 4 5)
]
}

@defform[(consp x)]{
Returns true when x is a pair.
@examples[
#:eval the-evaluator
(consp nil)
(consp 5)
(consp "string")
(consp (cons 1 nil))
(consp '(1 2 3))
]
}

@defproc[(car [x (or (consp x) (equal x nil))]) t]{
Returns the first element of a @scheme[cons]-pair, or @scheme[nil] for
@scheme[nil].
@examples[
#:eval the-evaluator
(car '(1 2 4 6))
(car nil)
]
}

@defproc[(cdr [x (or (consp x) (equal x nil))]) t]{
Returns the second element of a @scheme[cons]-pair, or @scheme[nil] for
@scheme[nil].
@examples[
#:eval the-evaluator
(cdr '(1 2 4 6))
(cdr nil)
]
}

@defform[(append lst ...)]{
Concatenates all the elements in the given lists into one list. This a macro
that expands into calls of the function @scheme[binary-append]. 
@examples[
#:eval the-evaluator
(append nil nil)
(append nil (list 1 2))
(append (list 1 2) (list 3 4))
]
}

@defproc[(binary-append [x (true-listp x)] [y t]) t]{
Concatenates two lists. Returns a new list containing all the items from
@scheme[x] followed by all the items from @scheme[y].  
@examples[
#:eval the-evaluator
(binary-append (list 1 2 3) (list 4 5 6))
(binary-append (list 1 2 3) 4)
(binary-append 5 "<-error")
]
}

@defform[(mv x ...)]{
Form for returning multiple values. This is like @scheme[list], but with the
restriction that if a function returns using @scheme[mv], then it must always
return the same number of values.
@examples[
#:eval the-evaluator
(mv 1 2 3 4)
]
}

@defproc[(revappend [x (true-listp x)] [y t]) t]{
@scheme[append] the @scheme[reverse] of @scheme[x] to @scheme[y].
@examples[
#:eval the-evaluator
(revappend nil nil)
(revappend nil (list 1 2))
(revappend (list 1 2) (list 3 4))
(revappend (list 1 2) 3)
]
}

@defform[(list elem ...)]{
Creates a new true list containing all the given elements.
@examples[
#:eval the-evaluator
(list)
(list 1 2 3)
]
}

@defform[(list* elem ... tail)]{
Uses @scheme[cons] to add each @scheme[elem] to @scheme[tail].
@examples[
#:eval the-evaluator
(list* 1 2 nil)
(list* 5 6 '(7 8 9))
(list* 1 2 3)
]
}

@defproc[(atom-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[atom]s.
@examples[
#:eval the-evaluator
(atom-listp (list 2 3 4))
(atom-listp (list (list 23 34) 45))
]
}

@defproc[(character-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[characterp] elements.
@examples[
#:eval the-evaluator
(character-listp (list #\a #\b))
(character-listp (list #\a "b"))
]
}

@defproc[(endp [x (or (consp lst) (equal x nil))]) t]{
Same as @scheme[atom], but with the guard that @scheme[x] is either a @scheme[consp] or is @scheme[nil].
@examples[
#:eval the-evaluator
(endp nil)
(endp (cons 2 nil))
]
}

@defproc[(eqlable-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[eqlablep] elements.
@examples[
#:eval the-evaluator
(eqlable-listp nil)
(eqlable-listp (cons 4 nil))
(eqlable-listp t)
]
}

@defproc[(integer-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[integerp] elements.
@examples[
#:eval the-evaluator
(integer-listp nil)
(integer-listp (list 24 -21 95))
(integer-listp (list 53 44 "number"))
]
}

@defproc[(keyword-value-listp [x t]) t]{
Reports whether @scheme[x] is of even length and every other element in the list satisfies @scheme[keywordp].
@examples[
#:eval the-evaluator
(keyword-value-listp (list :a 1 :b 2 :c 3))
(keyword-value-listp (list 'a 1 'b 'c 3))
]
}

@defproc[(listp [x t]) t]{
Reports whether @scheme[x] is either a @scheme[consp] or @scheme[nil]. 
@examples[
#:eval the-evaluator
(listp nil)
(listp (cons 4 nil))
(listp t)
]
}

@defproc[(null [x t]) t]{
Returns true if @scheme[x] equals @scheme[nil] (using @scheme[eq]).
@examples[
#:eval the-evaluator
(null nil)
(null (list 1 2 3))
]
}

@defproc[(proper-consp [x t]) t]{
Reports whether @scheme[x] is a proper (@scheme[nil]-terminated) nonempty list
@examples[
#:eval the-evaluator
(proper-consp nil)
(proper-consp (list 1 2 3))
(proper-consp (cons 1 2))
]
}

@defproc[(rational-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[rationalp] elements.
@examples[
#:eval the-evaluator
(rational-listp (list 1 2/5 3))
(rational-listp (list 1 2/5 "number"))
]
}

@defproc[(standard-char-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[standard-char-p] elements.
@examples[
#:eval the-evaluator
(standard-char-listp (list #\a #\b #\c))
(standard-char-listp (list 1 2 3))
]
}

@defproc[(string-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[stringp] elements.
@examples[
#:eval the-evaluator
(string-listp (list "ab" "cd" "ef"))
(string-listp (list 1 2 3))
]
}

@defproc[(symbol-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[symbolp] elements.
@examples[
#:eval the-evaluator
(symbol-listp (list 'ab 'cd 'ef))
(symbol-listp (list 1 2 3))
]
}

@defproc[(true-list-listp [x t]) t]{
Reports whether @scheme[x] is a list of @scheme[true-listp] elements.
@examples[
#:eval the-evaluator
(true-list-listp (list 1 2 3 4 5))
(true-list-listp '((1) (2) (3) (4) (5)))
]
}

@defproc[(true-listp [x t]) t]{
Reports whether @scheme[x] is a proper @scheme[nil]-terminated list.
@examples[
#:eval the-evaluator
(true-listp (list 1 2 3 4 5))
(true-listp "list")
]
}

@defproc[(butlast [l (true-listp l)] [n (and (integerp n) (<= 0 n))]) t]{
@scheme[(butlast l n)] is the list obtained by removing the last n elements from the true list l.
@examples[
#:eval the-evaluator
(butlast (list 1 2 3) 1)
(butlast (list 1 2 3) 2)
(butlast (list 1 2 3) 3)
]
}

@defproc[(fix-true-list [x t]) t]{
Coerces @scheme[x] to a true list by replacing the final @scheme[cdr] with @scheme[nil].
@examples[
#:eval the-evaluator
(fix-true-list (list 1 2 3))
(fix-true-list (cons 1 (cons 2 3)))
(fix-true-list "abc")
(fix-true-list 5)
]
}

@defproc[(len [lst t]) t]{
Finds the length of the given list. Returns 0 if @scheme[lst] is not a list.
@examples[
#:eval the-evaluator
(len nil)
(len (list 1 2 3 4 5))
(len "string")
(len t)
]
}

@defproc[(make-list [n (and (integerp n) (<= 0 n))]) true-listp]{
Makes a list of the given length, filling it in with @scheme[nil].
@examples[
#:eval the-evaluator
(make-list 0)
(make-list 3)
]
}

@defproc[(nth [n (and (integerp n) (>= n 0))] [lst (true-listp lst)]) t]{
Gets the @scheme[n]th element of @scheme[lst]
@examples[
#:eval the-evaluator
(nth 2 (list 1 2 3))
(nth 4 (list 1 2 1))
]
}

@defproc[(nthcdr [n (and (integerp n) (>= n 0))] [lst (true-listp lst)]) t]{
Gets the @scheme[n]th @scheme[cdr] of the given list
@examples[
#:eval the-evaluator
(nthcdr 2 (list 1 2 3))
(nthcdr 3 (list 1 2 1))
]
}

@defproc[(reverse [x (or (true-listp x) (stringp x))]) t]{
Reverse the given list or string
@examples[
#:eval the-evaluator
(reverse (list 1 2 3 4))
(reverse "abcd")
]
}

@defproc[(take [n (and (integerp n) (not (< n 0)))] [l (true-listp l)]) t]{
@examples[
#:eval the-evaluator
(take 3 (list 1 2 3 4 5))
(take 0 (list 1 2 3 4 5))
]
}

@defproc[(update-nth [n (and (integerp n) (<= 0 n))] [v t] [lst (true-listp l)]) t]{
Replaces the @scheme[n]th (0-based) position in @scheme[lst] with the value @scheme[v]. If @scheme[n] is greater than the length of @scheme[lst], then @scheme[update-nth] will add padding to the end of the list consisting of @scheme[nil] values.
@examples[
#:eval the-evaluator
(update-nth 0 'z '(a b c d e))
(update-nth 8 'z '(a b c d e))
]
}

@defform[(first lst)]{
first member of the list
@examples[
#:eval the-evaluator
(first (list 1 2 3 4 5 6 7 8))
]
}

@defform[(second lst)]{
@examples[
#:eval the-evaluator
(second (list 1 2 3 4 5))
]
}

@defform[(third lst)]{
@examples[
#:eval the-evaluator
(third (list 1 2 3 4 5 6 7 8 9 10))
]
}

@defform[(fourth lst)]{
fourth member of the list
@examples[
#:eval the-evaluator
(fourth (list 1 2 3 4 5 6 7 8))
]
}

@defform[(fifth lst)]{
fifth member of the list
@examples[
#:eval the-evaluator
(fifth (list 1 2 3 4 5 6 7 8))
]
}

@defform[(sixth lst)]{
Sixth member of the list 
@examples[
#:eval the-evaluator
(sixth (list 1 2 3 4 5 6 7 8 9 10))
]
}

@defform[(seventh lst)]{
@examples[
#:eval the-evaluator
(seventh (list 1 2 3 4 5 6 7 8 9 10))
]
}

@defform[(eighth lst)]{
eighth member of the list
@examples[
#:eval the-evaluator
(eighth (list 1 2 3 4 5 6 7 8))
]
}

@defform[(ninth x)]{
@examples[
#:eval the-evaluator
(ninth (list 1 2 3 4 5 6 7 8 9 10))
]
}

@defform[(tenth lst)]{
@examples[
#:eval the-evaluator
(tenth (list 1 2 3 4 5 6 7 8 9 10))
]
}

@defform[(last lst)]{
last member of the list
@examples[
#:eval the-evaluator
(last (list 1 2 3 4 5))
]
}

@defform[(rest lst)]{
The same as @scheme[cdr]
@examples[
#:eval the-evaluator
(rest (list 'w 'x 'y 'z))
]
}
