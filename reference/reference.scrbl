#lang scribble/doc
@(require scribble/manual
          "../lang/acl2-module-v.ss")

@title{Dracula: Reference Manual}

This manual defines the ACL2 and Modular ACL2 languages provided by Dracula.
For a gentler introduction to Dracula, see
@other-manual[(make-dracula-spec "guide/guide.scrbl")].  For documentation on
the ACL2 theorem prover itself, see
@link["http://www.cs.utexas.edu/~moore/acl2/"]{the ACL2 home page}.

@(defmodule dracula
  #:use-sources [dracula/lang/acl dracula/lang/dracula])

@local-table-of-contents[#:style 'immediate-only]

@include-section["acl2/acl2.scrbl"]

@include-section["teachpacks/teachpacks.scrbl"]

@include-section["modular/modular.scrbl"]

@index-section[]
