#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title[#:tag "books"]{ACL2 Books}

@section[(scheme "data-structures/list-theory")]
@deftogether[(
@defform/none[(include-book "data-structures/list-theory" :dir :system)]
@defform[#:literals [<list-pred> <pred>] (deflist <list-pred> (l) <pred>)]
)]{
Dracula supports the @scheme[deflist] form for defining new list types from the
"data-structures/list-theory" book.  It defines the predicate
@scheme[<list-pred>], recognizing lists whose elements satisfy @scheme[<pred>].
The @scheme[(l)] identifies the list as the predicate's only parameter; ACL2
supports other parameter lists but Dracula currently does not.
@examples[
#:eval the-evaluator
(deflist even-listp (l) evenp)
(even-listp (list 2 4 6))
(even-listp (list 1 3 5))
(even-listp 'not-a-list)
]
}

@section[(scheme "data-structures/structures")]
@deftogether[(
@defform/none[(include-book "data-structures/structures" :dir :system)]
@defform/subs[#:literals [name]
              (defstructure <name> field ...)
              ([field <field-name> (<field-name> (:assert <guard-expr>))])]
)]{
Dracula supports the @scheme[defstructure] form for defining new structure types
from the "data-structures/structures" book.  It defines a constructor
@scheme[<name>], a predicate @scheme[<name>-p] a weak predicate
@scheme[weak-<name>-p], and a selector @scheme[<name>-<field-name>] for each
field, with optional guards for the structure and each field which are
incorporated into the predicate (but not the weak predicate).
@examples[
#:eval the-evaluator
(defstructure circle radius)
(circle 10)
(circle-radius (circle 10))
(circle-p (circle 10))
(circle-p 'not-a-circle)
(defstructure rect
  (width (:assert (natp width)))
  (height (:assert (natp height))))
(rect 10 20)
(rect-width (rect 10 20))
(rect-height (rect 10 20))
(rect-p (rect 10 20))
(rect-p (rect -10 -20))
(rect-p 'not-a-rect)
]
}
