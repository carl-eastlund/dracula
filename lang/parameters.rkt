#lang racket

#|

The state recorded here does not affect programs that run in
ACL2-language-level, but these bindings occur in enough ACL2 code that we just
fake their presence.

We make a modest effort to report a useful error wrt ACL2's `state'.  It's a
reserved name unless state is available, in which case it may be used as a
parameter name.

Real ACL2 enforces more restrictions than we do.

|#

(require "constants.rkt"
         "nil-macros.rkt"
         "equality.rkt"
         (for-syntax syntax/parse))

(provide
 set-compile-fns
 set-guard-checking
 set-ignore-ok
 set-irrelevant-formals-ok
 set-state-ok
 set-well-founded-relation
 state
 )

(define-syntax (define-acl2-parameter stx)
  (syntax-parse stx
    [(_ name:id [opt:expr ...])
     #'(define-syntax (name stx*)
         (syntax-parse stx*
           [(_ value:expr)
            #'(define-values []
                (let* ([x value])
                  (unless (or (acl2-equal? x 'opt) ...)
                    (error 'name "expected one of ~a; got: ~e"
                           (list->english (list (format "~a" 'opt) ...))
                           x))
                  (values)))]))]))

(define (list->english strs)
  (match strs
    [(list) "none at all"]
    [(list str) str]
    [(list one two) (format "~a and ~a" one two)]
    [(list strs ... last)
     (apply string-append
       (append (for/list ([str (in-list strs)]) (format "~a, " str))
               (list (format "and ~a" last))))]))

(define-acl2-parameter set-compile-fns [t nil])
(define-acl2-parameter set-guard-checking [t nil :all :none])
(define-acl2-parameter set-ignore-ok [t nil :warn])
(define-acl2-parameter set-irrelevant-formals-ok [t nil :warn])
(define-acl2-parameter set-state-ok [t nil])

(define-syntax (set-well-founded-relation stx)
  (syntax-parse stx
    [(_ f:id)
     #'(define-values []
         (let* ([rel (lambda (x y) (f x y))])
           (values)))]))

(define-syntax state
  (make-set!-transformer
   (lambda (stx)
     (syntax-parse stx
       [name:id #'(quote STATE)]))))
