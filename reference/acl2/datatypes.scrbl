#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../evaluator.ss"
          "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Data Types}

Applicative Common Lisp is constructed from the following data types:

@schemegrammar*[#:literals (cons complex)
                [Any Atom (cons Any Any)]
                [Atom Symbol String Character Number]
                [Symbol 'A 'AB 'ABC ...]
                [String "a" "ab" "abc" ...]
                [Character #\a #\b #\c ...]
                [Number Rational (complex Rational Rational)]
                [Rational 0 1 -1 1/2 -1/2 ...]]

All values are immutable, and any values constructed identically are
indistinguishable (e.g. copying a list does not yield a "different" list).  By
convention, booleans and lists are encoded as follows:

@schemegrammar*[#:literals (cons t nil)
                [Boolean 'T 'NIL]
                [List 'NIL (cons Any List)]]
