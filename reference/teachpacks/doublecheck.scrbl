#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../evaluator.ss"
          "../../lang/acl2-module-v.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)
                     (this-package-in teachpacks/doublecheck)))

@title[#:style 'quiet #:tag "doublecheck" (scheme "doublecheck")]

@(declare-exporting/this-package [teachpacks/doublecheck] [])

@specform[(include-book "doublecheck" :dir :teachpacks)]

This teachpack defines automated random test generation as a complement to
theorem proving.  Each property declared via DoubleCheck is tested randomly by
Dracula, and verified logically by ACL2.  For a gentle introduction, see
@secref[#:doc (make-dracula-spec "guide/guide.scrbl") "doublecheck"] in
@other-manual[(make-dracula-spec "guide/guide.scrbl")].

@section{Properties}

@defform/subs[
#:literals
[:repeat :limit :where :value :rule-classes :instructions :hints :otf-flg :doc]
(defproperty name property-option ... (bind ...) test theorem-option ...)
([property-option (code:line :repeat repeat-expr)
                  (code:line :limit  limit-expr)]
 [bind (code:line var bind-option ...)]
 [bind-option (code:line :where hypothesis-expr)
              (code:line :value random-expr)
              (code:line :limit limit-expr)]
 [theorem-option (code:line :rule-classes rule-classes)
                 (code:line :instructions instructions)
                 (code:line :hints        hints)
                 (code:line :otf-flg      otf-flg)
                 (code:line :doc          doc-string)])
]{
Use @scheme[defproperty] to define DoubleCheck properties.  Running the
properties opens a GUI displaying results, including success, failure, and the
values of randomly chosen variables.

Each @scheme[defproperty] form defines a property called @scheme[name] which
states that @scheme[test] must be true for all assignments to each @scheme[var]
in the bindings.  DoubleCheck attempts to run the test @scheme[repeat] times
(default 50), but limits the attempts to generate satisfactory random data to
@scheme[limit] times (default 2500).

Dracula generates values for each @scheme[var] by running @scheme[random-expr],
which defaults to @scheme[(random-sexp)].  The @scheme[var] is generated until
@scheme[hypothesis-expr] evaluates to true (non-@scheme[nil]), or @scheme[limit]
attempts have been made (defaulting to the property's @scheme[limit], defaulting
to 50 as noted above).

ACL2 evaluates each @scheme[defproperty] as a theorem (@scheme[defthm])
equivalent to:
@schemeblock[(defthm name (implies (and hypothesis-expr ...) test) theorem-option ...)]

Here are some examples to illustrate the translation from theorems to
DoubleCheck properties.

@bold{Example 1:}
@schemeblock[
(include-book "doublecheck" :dir :teachpacks)

(code:comment #, @t{This theorem has no hypotheses.})
(defthm acl2-count-cons-theorem
  (> (acl2-count (cons a b)) (+ (acl2-count a) (acl2-count b))))

(code:comment #, @t{The corresponding @scheme[defproperty] lists the free variables.})
(defproperty acl2-count-cons-property
  (a b)
  (> (acl2-count (cons a b)) (+ (acl2-count a) (acl2-count b))))
]

@bold{Example 2:}
@schemeblock[
(include-book "doublecheck" :dir :teachpacks)

(code:comment #, @t{This theorem needs some hypotheses.})
(defthm rationalp-/-theorem
  (implies (and (integerp x) (posp y))
           (rationalp (/ x y))))

(code:comment #, @t{DoubleCheck expresses these as @scheme[:where] clauses.})
(defproperty rationalp-/-property
  (x :where (integerp x)
   y :where (posp y))
  (rationalp (/ x y)))
]

@bold{Example 3:}
@schemeblock[
(include-book "doublecheck" :dir :teachpacks)

(code:comment #, @t{These hypotheses state a relationship between variables.})
(defthm member-equal-nth-theorem
  (implies (and (proper-consp lst)
                (natp idx)
                (< idx (len lst)))
           (member-equal (nth idx lst) lst))
  :rule-classes (:rewrite :forward-chaining))

(code:comment #, @t{We can help DoubleCheck by picking random distributions})
(code:comment #, @t{that are likely to satisfy the hypotheses.})
(defproperty member-equal-nth-property
  (lst :where (proper-consp lst)
       :value (random-list-of (random-sexp))
   idx :where (and (natp idx) (< idx (len lst)))
       :value (random-between 0 (1- (len lst))))
  (member-equal (nth idx lst) lst)
  :rule-classes (:rewrite :forward-chaining))
]

}

@section{Random Distributions}

Randomness is an inherently imperative process.  As such, it is not reflected in
the logic of ACL2.  The random distribution functions of DoubleCheck may only be
used within @scheme[:value] clauses of @scheme[defproperty], or in other random
distributions.

@deftogether[(
@defproc[(random-sexp) t]
@defproc[(random-atom) atom]
@defproc[(random-boolean) booleanp]
@defproc[(random-symbol) symbolp]
@defproc[(random-char) characterp]
@defproc[(random-string) stringp]
@defproc[(random-number) acl2-numberp]
@defproc[(random-rational) rationalp]
@defproc[(random-integer) integerp]
@defproc[(random-natural) natp]
)]{
These distributions produce random elements of the builtin Dracula types.  When
no distribution is given for a property binding, @scheme[defproperty] uses
@scheme[random-sexp] by default.
}

@defproc[(random-between [lo integerp] [hi integerp]) integerp]{
Produces an integer uniformly distributed between @scheme[lo] and @scheme[hi],
inclusive; @scheme[lo] must be less than or equal to @scheme[hi].
}

@defproc[(random-data-size) natp]{
Produces a natural number weighted to prefer small numbers, appropriate for
limiting the size of randomly produced values.  This is the default distribution
for the length of random lists and the size of random s-expressions.
}

@defproc[(random-element-of [lst proper-consp]) t]{
Chooses among the elements of @scheme[lst], distributed uniformly.
}

@defform/subs[
#:literals [:size]
(random-list-of expr maybe-size)
([maybe-size code:blank (code:line :size size)])
]{
Constructs a random list of length @scheme[size] (default
@scheme[(random-data-size)]), each of whose elements is the result of evaluating
@scheme[expr].
}

@defform/subs[
#:literals [:size]
(random-sexp-of expr maybe-size)
([maybe-size code:blank (code:line :size size)])
]{
Constructs a random @scheme[cons]-tree with @scheme[size] total
@scheme[cons]-pairs (default @scheme[(random-data-size)]), each of whose leaves
is the result of evaluating @scheme[expr].
}

@defform[(defrandom name (arg ...) body)]{
The @scheme[defrandom] form defines new random distributions.  It takes the same
form as @scheme[defun], but the body may refer to other random distributions.

@bold{Example:}
@schemeblock[
(code:comment #, @t{Construct a distribution for random association lists:})
(defrandom random-alist (len)
  (random-list-of (cons (random-atom) (random-sexp)) :size len))

(code:comment #, @t{...and now use it:})
(defproperty acons-preserves-alistp
  (alist :where (alistp alist) :value (random-alist (random-between 0 10))
   key :where (atom key) :value (random-atom)
   datum)
  (alistp (acons key datum alist)))
]
}

@defform/subs[
#:literals [:weight]
(random-case clause ...)
([clause expr (code:line expr :weight weight)])
]{
Chooses an expression from the clauses, each with the associated weight
(defaulting to 1), and yields its result; the other expressions are not
evaluated.  This is useful with @scheme[defrandom] for defining recursive
distributions.

Be careful of the branching factor; a distribution with a high probability of
unbounded recursion is often unlikely to terminate.  It is useful to give a
depth parameter to limit recursion.

@bold{Example:}
@schemeblock[
(code:comment #, @t{Create a distribution for random expressions:})
(defrandom random-expression (max-depth)
  (random-case
   (random-symbol) (code:comment #, @t{variable})
   (random-string) (code:comment #, @t{string literal})
   (random-number) (code:comment #, @t{number literal})
   (code:comment #, @t{one-argument function call})
   (code:comment #, @t{(probability decreases with max-depth)})
   (list (random-symbol) (random-expression (1- max-depth)))
   :weight (- 1 (/ 1 max-depth))))
]
}
