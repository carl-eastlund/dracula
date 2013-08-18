#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme scheme/sandbox (this-package-in scheme)))

@title[#:style 'quiet #:tag "sandbox"]{Sandboxed Evaluation}

@defmodule/this-package[sandbox]

This module provides tools for sandboxed evaluation.

@deftogether[(
@defproc[(make-trusted-evaluator
          [language (or/c module-path?
                          (list/c 'special symbol?)
                          (cons/c 'begin list?))]
          [input-program any/c] ...
          [#:requires requires (listof (or/c module-path? path?))]
          [#:allow-read allow (listof or/c module-path? path?)])
         (any/c . -> . any)]
@defproc[(make-trusted-module-evaluator
          [module-decl (or/c syntax? pair?)]
          [#:language lang (or/c #f module-path?)]
          [#:allow-read allow (listof (or/c module-path? path?))])
         (any/c . -> . any)]
)]{
These procedures wrap calls to @scheme[make-evaluator] and
@scheme[make-module-evaluator], respectively, with
@scheme[call-with-trusted-sandbox-configuration] (introduced in PLT 4.1.3.6).
In older versions of PLT Scheme, they simulate the trusted configuration as
closely as possible.
}

@deftogether[(
@defproc[(make-scribble-evaluator
          [language (or/c module-path?
                          (list/c 'special symbol?)
                          (cons/c 'begin list?))]
          [input-program any/c] ...
          [#:requires requires (listof (or/c module-path? path?))]
          [#:allow-read allow (listof or/c module-path? path?)])
         (any/c . -> . any)]
@defproc[(make-scribble-module-evaluator
          [module-decl (or/c syntax? pair?)]
          [#:language lang (or/c #f module-path?)]
          [#:allow-read allow (listof (or/c module-path? path?))])
         (any/c . -> . any)]
)]{
These procedures wrap calls to @scheme[make-trusted-evaluator] and
@scheme[make-trusted-module-evaluator], respectively, with parameterizations
setting @scheme[sandbox-output] and @scheme[sandbox-error-output] to
@scheme['string].
}

@defproc[(make-sandbox-namespace-specs [make-ns (-> namespace?)]
                                       [path module-path?] ...)
         (cons/c (-> namespace?) (listof module-path?))]{

This function produces a value for the parameter
@scheme[sandbox-namespace-specs] such that new sandbox evaluators start with a
namespace constructed by @scheme[make-ns] and share a set of instances of the
modules referred to by the given @scheme[path]s.

}