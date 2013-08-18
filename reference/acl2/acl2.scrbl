#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")

@title[#:style 'toc]{ACL2}

Applicative Common Lisp (ACL) is the executable component of the ACL2
programming language.  This section documents the syntax and behavior of ACL
programs as simulated by Dracula.  For more information on the ACL2 language,
see @link["http://www.cs.utexas.edu/~moore/acl2/"]{the ACL2 webpage}.

@local-table-of-contents[#:style 'immediate]

@include-section["datatypes.scrbl"]
@include-section["expressions.scrbl"]
@include-section["events.scrbl"]
@include-section["library.scrbl"]
@include-section["parameters.scrbl"]
@include-section["books.scrbl"]
@include-section["unsupported.scrbl"]
