#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Characters}

@defproc[(alpha-char-p [x (characterp x)]) t]{
Determines whether @scheme[x] is an alphabetic character.
@examples[
#:eval the-evaluator
(alpha-char-p #\a)
(alpha-char-p #\3)
]
}

@defproc[(char [str (stringp str)] [n (and (integerp n) (>= n 0) (< n (length str)))]) t]{
Extracts the character at the @scheme[n]th (0-based) position in the string @scheme[str].
@examples[
#:eval the-evaluator
(char "hello" 0)
(char "hello" 4)
]
}

@defproc[(char< [x (characterp x)] [y (characterp y)]) t]{
Determines whether the character code of @scheme[x] is less than that of @scheme[y].
@examples[
#:eval the-evaluator
(char< #\a #\b)
(char< #\b #\a)
(char< #\b #\b)
(char< #\A #\a)
]
}

@defproc[(char<= [x (characterp x)] [y (characterp y)]) t]{
Determines whether the character code of @scheme[x] is less than or equal to that of @scheme[y].
@examples[
#:eval the-evaluator
(char<= #\a #\b)
(char<= #\b #\a)
(char<= #\b #\b)
(char<= #\A #\a)
]
}

@defproc[(char> [x (characterp x)] [y (characterp y)]) t]{
Determines whether the character code of @scheme[x] is greater than that of @scheme[y].
@examples[
#:eval the-evaluator
(char> #\a #\b)
(char> #\b #\a)
(char> #\b #\b)
(char> #\A #\a)
]
}

@defproc[(char>= [x (characterp x)] [y (characterp y)]) t]{
Determines whether the character code of @scheme[x] is greater than or equal to that of @scheme[y].
@examples[
#:eval the-evaluator
(char>= #\a #\b)
(char>= #\b #\a)
(char>= #\b #\b)
(char>= #\A #\a)
]
}

@defproc[(char-code [char (characterp char)]) t]{
Returns the numeric code for the given character.
@examples[
#:eval the-evaluator
(char-code #\a)
(char-code #\Z)
]
}

@defproc[(char-downcase [char (and (characterp char) (standard-char-p str))]) t]{
Converts the given character to lowercase
@examples[
#:eval the-evaluator
(char-downcase #\A)
(char-downcase #\a)
]
}

@defproc[(char-equal [x (and (characterp x) (standard-char-p x))] [y (and (characterp y) (standard-char-p y))]) t]{
Checks if the given characters are equal, ignoring to case.
@examples[
#:eval the-evaluator
(char-equal #\a #\a)
(char-equal #\A #\a)
]
}

@defproc[(char-upcase [char (and (characterp char) (standard-char-p str))]) t]{
Converts the given character to uppercase
@examples[
#:eval the-evaluator
(char-upcase #\A)
(char-upcase #\a)
]
}

@defproc[(characterp [x t]) t]{
@examples[
#:eval the-evaluator
(characterp #\a)
(characterp "a")
]
}

@defproc[(code-char [x (and (integerp x) (>= x 0) (< x 256))]) t]{
Converts the given number into its character equivalent.
@examples[
#:eval the-evaluator
(code-char 0)
(code-char 97)
(code-char 255)
(code-char 1000)
]
}

@defproc[(digit-char-p [x (characterp x)]) t]{
Determines whether the given character represents a numerical digit.
@examples[
#:eval the-evaluator
(digit-char-p #\3)
(digit-char-p #\a)
]
}

@defproc[(digit-to-char [n (and (integerp n) (<= 0 n) (>= 15 n))]) t]{
Converts the given number into its equivalent character in hex notation.
@examples[
#:eval the-evaluator
(digit-to-char 7)
(digit-to-char 10)
(digit-to-char 15)
]
}

@defproc[(lower-case-p [x (and (characterp x) (standard-char-p x))]) t]{
Determines if @scheme[x] is a lowercase alphabetic character.
@examples[
#:eval the-evaluator
(lower-case-p #\a)
(lower-case-p #\A)
]
}

@defproc[(standard-char-p [x (characterp x)]) t]{
Checks if the given character is a member of the @scheme[*standard-chars*]. This includes the standard punctuation and alphanumeric characters, along with @scheme[#\newline] and @scheme[#\space].
@examples[
#:eval the-evaluator
(standard-char-p #\a)
(standard-char-p #\5)
(standard-char-p #\Tab)
]
}

@defproc[(upper-case-p [x (and (characterp x) (standard-char-p x))]) t]{
Determines if @scheme[x] is an upper-case alphabetic character.
@examples[
#:eval the-evaluator
(upper-case-p #\A)
(upper-case-p #\a)
]
}

