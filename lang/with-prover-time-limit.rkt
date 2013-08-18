#lang racket

(provide with-prover-time-limit skip-proofs)

(define (timeout-number? v)
  (and (number? v)
       (exact? v)
       (rational? v)
       (positive? v)))

(define (timeout-value? v)
  (match v
    [(or (list n) n) (timeout-number? n)]))

(define-syntax (assert-timeout-expression! stx)
  (syntax-case stx ()
    [(_ timeout)
     (syntax/loc stx
       (let* ([tv timeout])
         (unless (timeout-value? tv)
           (error 'with-prover-time-limit
                  "timeout must be a positive number, alone or in a singleton list; got: ~s"
                  tv))))]))

(define-syntax (assert-timeout! stx)
  (syntax-case stx ()
    [(_ timeout)
     (case (syntax-local-context)
       [( expression )
        (syntax/loc stx
          (assert-timeout-expression! timeout))]
       [else
        (syntax/loc stx
          (define-values ()
            (begin (assert-timeout-expression! timeout)
                   (values))))])]))

(define-syntax (with-prover-time-limit stx)
  (syntax-case stx ()
    [(_ timeout form)
     (syntax/loc stx (begin (assert-timeout! timeout) form))]
    [_
     (raise-syntax-error
      #f
      "expected a timeout number followed by an ACL2 term"
      stx)]))

(define-syntax (skip-proofs stx)
  (syntax-case stx ()
    [(_ form) #'form]))
