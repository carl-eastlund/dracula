#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme (this-package-in scheme)))

@title[#:style 'quiet #:tag "class"]{Classes and Objects}

@defmodule/this-package[class]

This module provides tools for classes, objects, and mixins.

@section{Predicates and Contracts}

@defthing[class-or-interface/c flat-contract?]{

Recognizes classes and interfaces.

}

@defproc[(object-provides/c [spec class-or-interface/c] ...) flat-contract?]{

Recognizes objects which are instances of all the given classes and interfaces.

}

@defproc[(class-provides/c [spec class-or-interface/c] ...) flat-contract?]{

Recognizes classes which are subclasses (not strictly) and implementations,
respectively, of all the given classes and interfaces.

}

@defform[(mixin-provides/c [super-expr ...] [sub-expr ...])]{

Function contract for a mixin whose argument is the parent class @var[c%]
matching @scheme[(class-provides/c super-expr ...)] and whose result matches
@scheme[(class-provides/c #,(var c%) sub-expr ...)].

}

@section{Mixins}

@defproc[(ensure-interface [i<%> interface?]
                           [mx (mixin-provides/c [] [i<%>])]
                           [c% class?])
         (class-provides/c c% i<%>)]{

Returns @scheme[c%] if it implements @scheme[i<%>]; otherwise, returns
@scheme[(mx c%)].

}

@section{Methods}

@defform[(send-each objs message arg ...)]{

Sends the message to each object in the list @scheme[objs], returning
@scheme[(void)].

@defexamples[
#:eval (evaluator)
(define c%
  (class object%
    (super-new)
    (init-field msg)
    (define/public (say to) (printf "~a, ~a!\n" msg to))))
(send-each
 (list (new c% [msg 'Hello])
       (new c% [msg 'Good-bye]))
 say 'World)
]

}
