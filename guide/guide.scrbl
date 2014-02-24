#lang scribble/doc
@(require scribble/manual racket/require (path-up "self/module-path.rkt"))

@title{Dracula: A Guide to ACL2 Theorem Proving in DrRacket}

This manual provides an introduction to Dracula, the programming environment for
the ACL2 theorem prover in DrRacket.  See section 1 for instructions on
installing, upgrading, and uninstalling Dracula.  See section 2 for a tutorial
on running and verifying programs using Dracula and ACL2.  Section 3 introduces
DoubleCheck, the teachpack for using automated testing to check your
conjectures.  Section 4 introduces Modular ACL2, an extension of ACL2 that
supports systematic, piece-by-piece development and verification of ACL2 proofs.

See @other-manual[(dracula-module-path "reference/reference.scrbl")] for a full
description of Dracula's languages and libraries in detail, or
@link["http://www.cs.utexas.edu/~moore/acl2/"]{the ACL2 home page} for more
information on the ACL2 theorem prover.

@local-table-of-contents[#:style 'immediate-only]

@include-section["dracula.scrbl"]

@include-section["tutorial.scrbl"]

@include-section["doublecheck.scrbl"]

@include-section["modular.scrbl"]
