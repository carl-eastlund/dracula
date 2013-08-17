#lang mischief

(provide
  simplify-local-expansion)

(require
  (for-template
    racket/base))

(define (simplify-local-expansion stx)
  (syntax-parse stx
    #:literal-sets {kernel-literals}
    [(#%expression ~! e)
     (to-syntax #:stx stx
       (list #'#%expression
         (simplify-local-expansion (@ e))))]
    [({~literal module*} name lang body) #false]
    [({~literal module} name lang body) #false]
    [(#%plain-module-begin ~! term ...)
     (to-syntax #:stx stx
       (list* #'#%plain-module-begin
         (filter-map simplify-local-expansion (@ term))))]
    [(begin ~! term ...)
     (to-syntax #:stx stx
       (list* #'begin
         (filter-map simplify-local-expansion (@ term))))]
    [(begin-for-syntax ~! . _) #false]
    [(#%require ~! . _) stx]
    [(#%provide ~! . _) stx]
    [(define-syntaxes ~! . _) #false]
    [(define-values ~! {name ...} body)
     (to-syntax #:stx stx
       (list #'define-values (@ name)
         (simplify-local-expansion (@ body))))]
    [_:id stx]
    [(#%plain-lambda ~! {name ...} body ...)
     (to-syntax #:stx stx
       (list* '#%plain-lambda (@ name)
         (simplify-body (@ body))))]
    [(case-lambda ~! [formals body ...] ...)
     (to-syntax #:stx stx
       (list* #'case-lambda
         (map list*
           (@ formals)
           (map simplify-body (@ body)))))]
    [(if ~! test then else)
     (to-syntax #:stx stx
       (list #'if
         (simplify-local-expansion (@ test))
         (simplify-local-expansion (@ then))
         (simplify-local-expansion (@ else))))]
    [(begin0 ~! result effect ...)
     (to-syntax #:stx stx
       (list* #'begin0
         (simplify-local-expansion (@ result))
         (map simplify-local-expansion (@ effect))))]
    [(let-values {} body) (simplify-local-expansion (@ body))]
    [(letrec-values {} body) (simplify-local-expansion (@ body))]
    [(letrec-syntaxes+values _ {} body) (simplify-local-expansion (@ body))]
    [(let-values ~! {[(lhs ...) rhs] ...} body ...)
     (to-syntax #:stx stx
       (list* #'let-values
         (map list
           (@ lhs)
           (map simplify-local-expansion (@ rhs)))
         (simplify-body (@ body))))]
    [(letrec-values ~! {[(lhs ...) rhs] ...} body ...)
     (to-syntax #:stx stx
       (list* #'letrec-values
         (map list
           (@ lhs)
           (map simplify-local-expansion (@ rhs)))
         (simplify-body (@ body))))]
    [(letrec-syntaxes+values _ {} body ...)
     (to-syntax #:stx stx
       (list* #'let-values ;; For Dracula!  Can't handle letrec-values.
         '()
         (simplify-body (@ body))))]
    [(letrec-syntaxes+values ~! _ {[(lhs ...) rhs] ...} body ...)
     (to-syntax #:stx stx
       (list* #'letrec-values
         (map list
           (@ lhs)
           (map simplify-local-expansion (@ rhs)))
         (simplify-body (@ body))))]
    [(set! ~! name value)
     (to-syntax #:stx stx
       (list #'set!
         (@ name)
         (simplify-local-expansion (@ value))))]
    [(quote ~! . _) stx]
    [(quote-syntax ~! . _) stx]
    [(with-continuation-mark ~! key value body)
     (to-syntax #:stx stx
       (list #'with-continuation-mark
         (simplify-local-expansion (@ key))
         (simplify-local-expansion (@ value))
         (simplify-local-expansion (@ body))))]
    [(#%plain-app ~! proc arg ...)
     (to-syntax #:stx stx
       (list* #'#%plain-app
         (simplify-local-expansion (@ proc))
         (map simplify-local-expansion (@ arg))))]
    [(#%top ~! . _) stx]
    [(#%variable-reference ~! . _) stx]))

(define (simplify-body stxs)
  (syntax-parse stxs
    [{(letrec-syntaxes+values
          {[(stx-lhs ...) stx-rhs] ...}
          {}
        body ...)}
     (simplify-body (@ body))]
    [{(letrec-syntaxes+values
          {[(stx-lhs ...) stx-rhs] ...}
          {[(val-lhs ...) val-rhs] ...+}
        body ...)}
     (list
       (to-syntax #:stx (first stxs)
         (list* #'letrec-values
           (map list
             (@ val-lhs)
             (map simplify-local-expansion (@ val-rhs)))
           (simplify-body (@ body)))))]
    [{body ...}
     (map simplify-local-expansion (@ body))]))
