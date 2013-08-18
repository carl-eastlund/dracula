#lang scribble/doc
@(require scribble/manual scheme/runtime-path "../lang/acl2-module-v.ss")

@(define-runtime-path doublecheck-fail "images/doublecheck-fail.png")
@(define-runtime-path doublecheck-ok "images/doublecheck-ok.png")

@(define quickcheck-url "http://www.cs.chalmers.se/~rjmh/QuickCheck/")
@(define fasttest-url "http://planet.plt-scheme.org/display.ss?package=fasttest.plt&owner=cce")

@title[#:tag "doublecheck"]{Doublecheck}

DoubleCheck is a Dracula teachpack for randomized testing. It is inspired 
by the @link[quickcheck-url]{QuickCheck} library for Haskell, and 
implemented with the @link[fasttest-url]{FastTest} library for PLT Scheme.
DoubleCheck may be used before theorem proving to establish confidence in a
program property, or after a failed proof attempt to search for counterexamples.

@section{Examples}

Here is an example of a DoubleCheck property with an error, and the resulting
output from both DoubleCheck and ACL2. The property attempts to prove that 
@scheme[nth] produces a member of its input, without establishing that its
input is in range. Note that DoubleCheck detects 19 errors out of the visible
21 trials, the first of which is shown in the SchemeUnit window (on the 
right). We can see here that in this trial, @scheme[lst] was
@scheme[("yyhhp")] and @scheme[idx] was @scheme[386]. Doublecheck also 
automatically provides a @scheme[check-expect] expression, to incorporate 
this trial into a test suite. The ACL2 output (middle) shows checkpoints from
the faulty theorem corresponding to the DoubleCheck property.

@image[doublecheck-fail]

Here is a corrected version of the same DoubleCheck property. Now the index
to @scheme[nth] is constrained based on the length of the list, and its 
random distribution is stated explicitly. Both the DoubleCheck and ACL2
output show success. 

@image[doublecheck-ok]

For more detailed information, see @secref["doublecheck" #:doc
(make-dracula-spec "reference/reference.scrbl")] in
@other-manual[(make-dracula-spec "reference/reference.scrbl")].
