#lang mischief

(provide
  alpha-expr=?
  alpha-rule-classes=?)

(require
  dracula/expansion/grammar)

(define (alpha-rule-classes=? ones twos table)
  (alpha-list=? alpha-rule-class=? ones twos table))

(define (alpha-rule-class=? one two table)
  (syntax-parse (list one two)
    [(~delimit-cut {a:rule/rewrite-expr b:rule/rewrite-expr})
     (alpha-optional-expr=? (@ a.corollary) (@ b.corollary) table)]
    [(~delimit-cut {a:rule/forward-chaining-expr b:rule/forward-chaining-expr})
     (alpha-optional-expr=? (@ a.corollary) (@ b.corollary) table)]
    [(~delimit-cut {a:rule/elim-expr b:rule/elim-expr})
     (alpha-optional-expr=? (@ a.corollary) (@ b.corollary) table)]
    [(~delimit-cut
         {a:rule/type-prescription-expr b:rule/type-prescription-expr})
     (alpha-optional-expr=? (@ a.corollary) (@ b.corollary) table)]
    [(~delimit-cut {a:rule/definition-expr b:rule/definition-expr})
     (and
       (alpha-optional-expr=? (@ a.corollary) (@ b.corollary) table)
       (alpha-clique-expr=? (@ a.clique) (@ b.clique) table))]
    [(~delimit-cut {a:rule/induction-expr b:rule/induction-expr})
     (and
       (alpha-optional-expr=? (@ a.corollary) (@ b.corollary) table)
       (alpha-expr=? (@ a.pattern) (@ b.pattern) table)
       (alpha-optional-expr=? (@ a.condition) (@ b.condition) table)
       (alpha-expr=? (@ a.scheme) (@ b.scheme) table))]
    [{_ _} #false]))

(define (alpha-clique-expr=? one two table)
  (syntax-parse (list one two)
    [(~delimit-cut {a:optional/none-expr b:optional/none-expr}) #true]
    [(~delimit-cut {a:option/clique-expr b:option/clique-expr})
     (alpha-refs=? (@ a.fun) (@ b.fun) table)]
    [(~delimit-cut {a:option/controllers-expr b:option/controllers-expr})
     (and (alpha-refs=? (@ a.fun) (@ b.fun) table)
       (alpha-lists=? alpha-datum=? (@ a.control) (@ b.control) table))]
    [{_ _} #false]))

(define (alpha-optional-expr=? one two table)
  (syntax-parse (list one two)
    [{a:optional/some-expr b:optional/some-expr}
     (alpha-expr=? (@ a.arg) (@ b.arg) table)]
    [{a:optional/none-expr b:optional/none-expr} #true]
    [{_ _} #false]))

(define (alpha-expr=? one two table)
  (syntax-parse (list one two)
    [(~delimit-cut {a:quote-expr b:quote-expr})
     (equal?
       (to-datum (@ a.value))
       (to-datum (@ b.value)))]
    [(~delimit-cut {a:if-expr b:if-expr})
     (and
       (alpha-expr=? (@ a.test) (@ b.test) table)
       (alpha-expr=? (@ a.then) (@ b.then) table)
       (alpha-expr=? (@ a.else) (@ b.else) table))]
    [(~delimit-cut {a:let-expr b:let-expr})
     (and
       (alpha-exprs=? (@ a.rhs) (@ b.rhs) table)
       (alpha-expr=? (@ a.body) (@ b.body)
         (alpha-extend (@ a.lhs) (@ b.lhs) table)))]
    [(~delimit-cut {a:app-expr b:app-expr})
     (and
       (alpha-ref=? (@ a.fun) (@ b.fun) table)
       (alpha-exprs=? (@ a.arg) (@ b.arg) table))]
    [(~delimit-cut {a:var-expr b:var-expr})
     (alpha-id=? (@ a.name) (@ b.name) table)]
    [{_ _} #false]))

(define (alpha-exprs=? ones twos table)
  (alpha-list=? alpha-expr=? ones twos table))

(define (alpha-ref=? one two table)
  (syntax-parse (list one two)
    [(~delimit-cut {a:deref-expr b:deref-expr})
     (and (alpha-ref=? (@ a.comp) (@ b.comp) table)
       (alpha-ref=? (@ a.field) (@ b.field) table))]
    [(~delimit-cut {a:var-expr b:var-expr})
     (alpha-id=? (@ a.name) (@ b.name) table)]
    [{_ _} #false]))

(define (alpha-refs=? ones twos table)
  (alpha-list=? alpha-ref=? ones twos table))

(define (alpha-id=? one two table)
  (free-identifier=?
    (dict-ref table one one)
    two))

(define (alpha-lists=? alpha-elem=? oness twoss table)
  (alpha-list=? (arg+ alpha-list=? alpha-elem=?) oness twoss table))

(define (alpha-list=? alpha-elem=? ones twos table)
  (match*! {ones twos}
    [{'() '()} #true]
    [{(cons one ones) (cons two twos)}
     (and (alpha-elem=? one two table)
       (alpha-list=? alpha-elem=? ones twos table))]
    [{_ _} #false]))

(define (alpha-optional=? alpha-value=? one two table)
  (cond!
    [(and one two) (alpha-value=? one two table)]
    [else (and (not one) (not two))]))

(define (alpha-datum=? one two table)
  (equal?
    (to-datum one)
    (to-datum two)))

(define (alpha-extend ones twos table)
  (for/fold
      {[table table]}
      {[one (in-list ones)]
       [two (in-list twos)]}
    (dict-set table one two)))
