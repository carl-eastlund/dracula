#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title[#:tag "events"]{Events and Definitions}

ACL2 events are special terms which define new functions or logical rules.
Defined names are in scope within their own definition and below; forward
references are not allowed.  Use the @scheme[mutual-recursion] event for
mutually recursive functions.

@defform[(assert$ test form)]{
Raises an error if the given test fails.
@examples[
#:eval the-evaluator
(assert$ (< 0 1) t)
(assert$ (< 1 0) t)
]
}

@defform[(defaxiom name term :rule-classes rule-classes :doc doc-string)]{
Defines an axiom. Using @scheme[defaxiom] is not recommended.
@examples[
#:eval the-evaluator
(defaxiom sbar (equal t nil)
          :rule-classes nil
          :doc ":Doc-Section ...")
]
}

@defform[(deflabel name :doc doc-string)]{
Defines a label, for adding a landmark and/or adding a documentation topic.
@examples[
#:eval the-evaluator
(deflabel interp-section
          :doc ":Doc-Section ...")
]
}

@defform[(defstub name args-sig => output-sig :doc doc-string)]{
}

@defform[(deftheory name term :doc doc-string)]{
Defines a theory (to enable or disable a set of rules)
@examples[
#:eval the-evaluator
(deftheory interp-theory
           (set-difference-theories
             (universal-theory :here)
             (universal-theory 'interp-section)))
]
}

@deftogether[(
@defform[(defthm name term
        :rule-classes rule-classes
        :instructions instructions
        :hints        hints
        :otf-flg      otf-flg
        :doc          doc-string)]
@defform[(defthmd name term
        :rule-classes rule-classes
        :instructions instructions
        :hints        hints
        :otf-flg      otf-flg
        :doc          doc-string)]
)]{
These two forms both define a theorem to be proved and named.
The @scheme[defthm] form defines an enabled function that will be
automatically used in subsequent proof attempts.
The @scheme[defthmd] form defines a disabled function that must be
explicitly mentioned in hints.
@examples[
#:eval the-evaluator
(defthm x+x=2x (= (+ x x) (* 2 x)))
(defthmd x=1x (= x (* 1 x)))
]
}

@defform[(in-theory term :doc doc-string)]{
@examples[
#:eval the-evaluator
(in-theory (set-difference-theories
             (universal-theory :here)
             '(flatten (:executable-counterpart flatten))))
]
}

@defform*[#:id include-book
          #:literals (:dir :system :teachpacks)
          [(include-book "<basename>")
           (include-book "<basename>" :dir :system)
           (include-book "<basename>" :dir :teachpacks)]]{

The @scheme[include-book] form imports definitions from a file called a
@deftech{book}.  Dracula supports three variants.

@schemeblock[(include-book "my-path/my-book")]
Without a @scheme[:dir] option, Dracula adds a ".lisp" extension to the base
name and attempts to load a file relative to the current directory.  In the case
above, the program must reside in a directory with a "my-path" subdirectory
containing a book named "my-book.lisp".

Books must be valid Dracula programs; they must start with
@scheme[(in-package "ACL2")]; and they must contain only events, no top-level
expressions.

@schemeblock[(include-book "my-path/my-book" :dir :system)]
This variant loads a book from the system directory included with an ACL2
installation.  Dracula simulates only a couple of the definitions from these
books, but allows other books to be included for theorem proving purposes.  See
@secref["books"] for the list of books supported by Dracula.

@schemeblock[(include-book "my-path/my-book" :dir :teachpack)]
The third variant loads one of the books provided with Dracula, called
teachpacks.  These books make use of DrScheme features such as interactive
animations and other i/o, and are also reflected in the ACL2 logic.  See
@secref["teachpacks"] for the list of provided teachpacks.

}

@defform[(mutual-recursion def1 ... defn)]{
For defining mutually-recursive functions.
@examples[
#:eval the-evaluator
(mutual-recursion
 (defun evenlp (x)
   (if (consp x) (oddlp (cdr x)) t))
 (defun oddlp (x)
   (if (consp x) (evenlp (cdr x)) nil)))
]
}

@defform[(theory-invariant term :key key :error t/nil)]{}

@defform[(defconst name val)]{
Defines a constant value. The name must begin and end with asterisks.
@examples[
#:eval the-evaluator
(defconst *PI* 22/7)
(* 2 *PI*)
]
}

@deftogether[(
@defform[(defun name (args) (declare decl ...) ... body)]
@defform[(defund name (args) (declare decl ...) ... body)]
)]{
These two forms both define a function.
The @scheme[defun] form defines a logically-enabled function, about which
proof attempts can reason automatically.
The @scheme[defund] form defines a logically-disabled function, about which
proof attempts may only reason if given a hint.
@examples[
#:eval the-evaluator
(defun absolute-value (x)
  (cond 
    ((< x 0) (* -1 x))
    (t x)))
(absolute-value 5)
(absolute-value -5)
(defun square-of (x)
  (* x x))
(square-of 3)
(square-of -3)
]
}

@defform[(declare d ...)]{
Used in @scheme[defun] and @scheme[defund] to give ACL2 extra information
about a function.
}

@defform[(defequiv pred)]{
Declares @scheme[pred] to be an equivalence predicate.  It is equivalent to the
following, with the supplied name substituted for @scheme[pred] in all
identifiers:
@schemeblock[
(defthm pred-is-an-equivalence
  (and (booleanp (pred x y))
       (pred x x)
       (implies (pred x y) (pred y x))
       (implies (and (pred x y)
                     (pred y z))
                (pred x z)))
  :rule-classes (:equivalence))
]
}
