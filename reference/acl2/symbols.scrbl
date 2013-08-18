#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Symbols}

Booleans are also symbols; see @secref["booleans"] for more operators.

@defproc[(keywordp [x t]) t]{
Returns @scheme[t] if and only if @scheme[x] is a symbol in the "KEYWORD" package.
@examples[
#:eval the-evaluator
(keywordp :hints)
(keywordp 'hints)
(keywordp 5)
]
}

@defproc[(symbol-< [x (symbolp x)] [y (symbolp y)]) t]{
Returns non-nil when the @scheme[symbol-name] of @scheme[x] lexicographically precedes that of @scheme[y]. The returned number is the (0-based) position at which the names differ.
@examples[
#:eval the-evaluator
(symbol-< 'aaa 'aab)
(symbol-< 'ab 'ab)
(symbol-< 'bb 'aa)
]
}

@defproc[(symbol-name [x (symbolp x)]) t]{
Returns a string containing the name of the given symbol
@examples[
#:eval the-evaluator
(symbol-name 'hello)
(symbol-name 'qwerty)
]
}

@defproc[(symbol-package-name [x (symbolp x)]) t]{
Returns the name of the package for the given symbol.
@examples[
#:eval the-evaluator
(symbol-package-name 'hello)
(symbol-package-name :hello)
]
}

@defproc[(symbolp [x t]) t]{
Determines whether @scheme[x] is a symbol.
@examples[
#:eval the-evaluator
(symbolp 'hello)
(symbolp "world")
]
}

@defproc[(intern$ [name (stringp name)] [package (stringp package)]) t]{
Produces a symbol of the given name in the given package.
@examples[
#:eval the-evaluator
(intern$ "a" "ACL2")
(intern$ "b" "KEYWORD")
]
}

@defform[(intern name package)]{
Produces a symbol of the given name in the given package.  Restricts its input
to the packages "ACL2" or "KEYWORD".
@examples[
#:eval the-evaluator
(intern "c" "ACL2")
(intern "d" "KEYWORD")
]
}

@defproc[(intern-in-package-of-symbol [name (stringp name)]
                                      [symbol (symbolp symbol)])
         t]{
Produces a symbol with the given name in the package of the given symbol.
@examples[
#:eval the-evaluator
(intern-in-package-of-symbol "e" 'symbol)
(intern-in-package-of-symbol "f" ':keyword)
]
}
