#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")

@title[#:tag "teachpacks" #:style 'toc]{Teachpacks}

These books are not a part of the ACL2 distribution; they were developed for
classroom use and are distributed with Dracula.  Some of them make use of
DrScheme's imperative capabilities such as interactive animations and automated
unit testing.

@local-table-of-contents[]

@include-section["audio.scrbl"]
@include-section["avl-rational-keys.scrbl"]
@include-section["binary-io-utilities.scrbl"]
@include-section["doublecheck.scrbl"]
@include-section["io-utilities.scrbl"]
@include-section["list-utilities.scrbl"]
@include-section["rand.scrbl"]
@include-section["testing.scrbl"]
@include-section["world.scrbl"]
