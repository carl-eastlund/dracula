#|
Implement some of the boolean & conditional forms.
if and cond are currently in (file "acl2-prims-scheme.rkt").
|#
#lang racket

(require (file "constants.rkt")
         (file "nil-macros.rkt")
         "../private/collects.rkt")
(require (for-syntax (cce text)))

(provide (rename-out [acl2-and and]
                     [acl2-or or]
                     [acl2-if if]
                     [acl2-cond cond]
                     [acl2-case case]))

(define (false->nil x)
  (if x
      x
      nil))

(define-syntax (acl2-and stx)
  (syntax-case stx ()
    [(_) #'t]
    [(_ expr) #'(false->nil expr)]
    [(_ expr . exprs)
     #'(let ([temp expr])
         (acl2-if temp
                  (acl2-and . exprs)
                  nil))]))

(define-syntax (acl2-or stx)
  (syntax-case stx ()
    [(_) #'nil]
    [(_ expr) #'(false->nil expr)]
    [(_ expr . exprs)
     #'(let ([temp expr])
         (acl2-if temp
                  temp
                  (acl2-or . exprs)))]))

(define (void->nil x)
  (if (void? x) nil x))

;; ACL2 allows a question to be a datum or a list of data, but
;; Scheme only allows the latter.
(define-for-syntax (xform-question q)
  (syntax-case* q (t otherwise) text=?
    [(_ ...) q]
    [t (datum->syntax (quote-syntax here) 'else)]
    [otherwise (datum->syntax (quote-syntax here) 'else)]
    [x #`(#,q)]))


(define-for-syntax (transform-clause cl)
  (syntax-case cl (t)
    [(t body) #'(#t body)]
    [(q a) #'((not-nil q) a)]))

(define-syntax (acl2-cond stx)
  (syntax-case stx (acl2:not t)
    [(_ [(acl2:not x) body] . others)
     #'(acl2-if x (acl2-cond . others) body)]
    [(_) #'nil]
    [(_ (question answer) . others)
     #'(acl2-if question
                answer
                (acl2-cond . others))]))

(define-syntax (acl2-if stx)
  (syntax-case stx (acl2:not)
    [(_ (acl2:not x) y z) #'(acl2-if x z y)]
    [(_ test conseq)
     (raise-syntax-error
      #f
      "missing an alternative expression:  if requires a test, a consequent, and an alternative."
      stx)]
    [(_ test conseq alt)
     #'(if (nil? test) alt conseq)]))

(define-syntax (acl2-case stx)
  (syntax-case stx ()
    [(_ expr (question answer) ...)
     (with-syntax ([(new-question ...)
                    (map xform-question (syntax->list #'(question ...)))])
       #'(void->nil (case expr
                      (new-question answer) ...)))]))
