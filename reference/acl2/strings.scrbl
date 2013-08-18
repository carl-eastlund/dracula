#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Strings}

Some string functions can be found in the section on @secref["sequences"] as
well.

@defproc[(make-character-list [x t]) character-listp]{
This function will @scheme[coerce] a value to a list of characters.
@examples[
#:eval the-evaluator
(make-character-list "hello")
]
}

@defproc[(string [x (or (stringp x) (symbolp x) (characterp x))]) t]{
Uses @scheme[coerce] to convert the input into a string.
@examples[
#:eval the-evaluator
(string "abc")
(string 'abc)
(string #\a)
]
}

@defproc[(string-append [x (stringp x)] [y (stringp y)]) t]{
Concatenates two strings together.
@examples[
#:eval the-evaluator
(string-append "ab" "cd")
]
}

@defproc[(string-downcase [str (and (stringp str) (standard-char-listp (coerce str 'list)))]) t]{
Converts the characters in @scheme[str] to lowercase.
@examples[
#:eval the-evaluator
(string-downcase "ABCdef")
]
}

@defproc[(string-equal [x (and (stringp x) (standard-char-listp (coerce x 'list)))] [y (and (stringp y) (standard-char-listp (coerce y 'list)))]) t]{
Compares two strings to see if they are equal.
@examples[
#:eval the-evaluator
(string-equal "ab" "cd")
(string-equal "ab" "ab")
]
}

@defproc[(string-upcase [str (and (stringp str) (standard-char-listp (coerce str 'list)))]) t]{
Converts the characters in @scheme[str] to uppercase.
@examples[
#:eval the-evaluator
(string-upcase "ABCdef")
]
}

@defproc[(string< [x (stringp x)] [y (stringp y)]) t]{
Compares the two strings for lexicographical ordering. Returns non-@scheme[nil] if and only if @scheme[x] precedes @scheme[y] lexicographically. When non-@scheme[nil], the number returned is the first position at which the strings differ.
@examples[
#:eval the-evaluator
(string< "ab" "cd")
(string< "ab" "abc")
]
}

@defproc[(string<= [x (stringp x)] [y (stringp y)]) t]{
Compares the two strings for lexicographical ordering. Returns non-@scheme[nil] if and only if @scheme[x] precedes @scheme[y] lexicographically. If they differ, the number returned is the first position at which the strings differ. Otherwise, the number returned is their common length.
@examples[
#:eval the-evaluator
(string<= "ab" "cd")
(string<= "ab" "ab")
]
}

@defproc[(string> [x (stringp x)] [y (stringp y)]) t]{
Compares the two strings for lexicographical ordering. Returns non-@scheme[nil] if and only if @scheme[y] precedes @scheme[x] lexicographically. When non-@scheme[nil], the number returned is the first position at which the strings differ.
@examples[
#:eval the-evaluator
(string> "ab" "cd")
(string> "ab" "ab")
(string> "ba" "ab")
]
}

@defproc[(string>= [x (stringp x)] [y (stringp y)]) t]{
Compares the two strings for lexicographical ordering. Returns non-@scheme[nil] if and only if @scheme[y] precedes @scheme[x] lexicographically. If they differ, the number returned is the first position at which the strings differ. Otherwise, the number returned is their common length.
@examples[
#:eval the-evaluator
(string>= "ab" "cd")
(string>= "ab" "ab")
(string>= "ba" "ab")
]
}

@defproc[(stringp [x t]) t]{
Determines whether @scheme[x] is a string.
@examples[
#:eval the-evaluator
(stringp "abcd")
(stringp nil)
]
}
