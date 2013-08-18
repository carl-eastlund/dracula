#lang scribble/doc
@(require scribble/manual
          "../scribble.ss"
          (for-label scheme/base))

@title[#:style '(toc)]{@bold{Scheme Utilities} in @schememodname/this-package[]}

This library provides supplements for the built-in PLT Scheme languages.
Feel free to use it, copy my code, or ask me questions
(@author+email["Carl Eastlund" "cce@ccs.neu.edu"]).

@defmodule/this-package[]

The default module provides all the bindings described below, except those in
the @secref["scribble"] and @secref["graphics-section"] sections.

@table-of-contents[]

@include-section["function.scrbl"]

@include-section["values.scrbl"]

@include-section["text-section.scrbl"]

@include-section["collection-section.scrbl"]

@include-section["macro-section.scrbl"]

@include-section["match.scrbl"]

@include-section["class.scrbl"]

@include-section["contract.scrbl"]

@include-section["module-section.scrbl"]

@include-section["exn.scrbl"]

@include-section["port.scrbl"]

@include-section["debug.scrbl"]

@include-section["sandbox.scrbl"]
@include-section["scribble.scrbl"]

@include-section["graphics-section.scrbl"]
