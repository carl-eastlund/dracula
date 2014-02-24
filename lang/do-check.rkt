#lang racket

(require
  racket/require
  (path-up "self/require.rkt")
  racket/stxparam
  (cce-in define)
  (for-syntax
    syntax/parse
    racket/match
    racket/unit-exptime
    racket/require-transform
    (cce-in syntax)))

(define-syntax-parameter check-arity? #t)

(define-syntax-rule (unchecked-arity e)
  (syntax-parameterize ([check-arity? #f]) e))

(define-for-syntax (check-arity! arity stx)
  (syntax-parse stx
    [(_ arg ...)
     (let* ([len (length (syntax->list #'(arg ...)))])
       (unless (= len arity)
         (raise-syntax-error #f
           (format "expected ~a arguments, but got ~a arguments instead"
             arity len)
           stx)))]
    [(_ . args)
     (raise-syntax-error #f
       "expected a proper list of arguments, but got an improper list instead"
       stx)]
    [_ (raise-syntax-error #f
         "functions must be applied to arguments"
         stx)]))

(define-for-syntax ((check-arity-transformer arity f) stx)
  (when (syntax-parameter-value #'check-arity?)
    (check-arity! arity stx))
  (f stx))

(define-for-syntax all-scopes (box null))

(define-for-syntax (push! x stack)
  (set-box! stack (cons x (unbox stack))))

(define-for-syntax (pop! x stack)
  (unless (and (pair? (unbox stack)) (eq? (car (unbox stack)) x))
    (error 'begin-below
      "internal error: push!/pop! mismatch: ~s <- ~s" x stack))
  (set-box! stack (cdr (unbox stack)))
  x)

(define-for-syntax (register-marker marker stx)
  (unless (pair? (unbox all-scopes))
    (raise-syntax-error #f
      "defined outside of begin-below" stx marker))
  (let* ([scope (car (unbox all-scopes))])
    (push! (syntax-local-value marker) scope)))

(define-syntax-rule (define-below-marker-for name form)
  (begin (define-syntax name (box #t))
         (in-phase1 (register-marker #'name #'form))
         (in-phase1/pass2 (set-box! (syntax-local-value #'name) #t))))

(define-syntax (begin-below stx)
  (match (syntax-local-context)
    ['expression (raise-syntax-error #f "cannot be used as an expression" stx)]
    [_ (void)])
  (syntax-parse stx
    [(_ term:expr ...)
     #'(begin
         (define-syntax scope (box null))
         (in-phase1 (push! (syntax-local-value #'scope) all-scopes))
         (begin term ...)
         (in-phase1
          (let ([scope (pop! (syntax-local-value #'scope) all-scopes)])
            (for ([marker (in-list (unbox scope))])
              (set-box! marker #f)))))]))

(define-for-syntax ((check-below-transformer marker f) stx)
  (unless (unbox (syntax-local-value marker))
    (raise-syntax-error #f
      "used before definition" stx))
  (f stx))

(define-syntax (rename-below stx)
  (syntax-parse stx
    [(_ [above:id below:id] ...)
     #`(begin
         (define-below-marker-for here #,stx)
         (define-syntaxes [ below ... ]
           (values (check-below-transformer #'here
                     (redirect-transformer #'above))
                   ...)))]))

(define-syntax (define-values-below stx)
  (syntax-parse stx
    [(_ [below:id ...] body:expr)
     (with-syntax* ([(above ...) (generate-temporaries #'(below ...))])
       #`(begin
           (rename-below [above below] ...)
           (define-values [above ...] body)))]))

(define-single-definition define-below define-values-below)

(define-syntax (require-below stx)
  (syntax-parse stx
    [(_ spec:expr ...)
     (define specs #'(only-meta-in 0 spec ...))
     (define-values [ imports sources ] (expand-import specs))
     (define names (map import-local-id imports))
     (define temps (generate-temporaries names))
     (with-syntax ([(name ...) names]
                   [(temp ...) temps])
       #`(begin
           (require (rename-in #,specs [name temp] ...))
           (rename-below [temp name] ...)))]))

(provide
 (for-syntax check-arity-transformer) unchecked-arity
 (for-syntax check-below-transformer) begin-below define-below-marker-for
 rename-below
 define-below
 define-values-below
 require-below
 )
