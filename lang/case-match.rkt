#|
ACL2's case-match, for destructuring s-expressions.
|#
#lang racket

(require "../private/collects.rkt")
(require (for-syntax "syntax-checks.rkt"
                     (cce text)))

(provide case-match)

;; Produce #t iff identifier's name begins with #\!
(define-for-syntax (starts-with-bang? id)
  (char=? #\! (string-ref (symbol->string (syntax-e id)) 0)))

;; identifier whose name starts with character #\! -> identifier
(define-for-syntax (remove-bang !id)
  (let ([name (symbol->string (syntax-e !id))])
    (let ([new-name (substring name 1)])
      (datum->syntax !id (string->symbol new-name)))))

(define-for-syntax (compile-pattern pat)
  (syntax-case* pat (quote & nil t) text=?
    [& (syntax _)]
    [nil (syntax '())] ;; nil is implemented as Scheme null
    [t   (syntax 't)]
    [*const*
     (legal-constant-name? #'*const*)
     ;; NB:  Relies on (Scheme) equal? =/bool-rep (ACL2) equal
     (syntax (? (lambda (to-match) (equal? to-match *const*))))]
    [!name
     ;; NOTE:  The ACL2 pattern ! gets compiled to a match against whatever
     ;; is bound to the variable whose name is "".  This is consistent with ACL2.
     (and (identifier? #'!name)
          (starts-with-bang? #'!name))
     (with-syntax ([name (remove-bang #'!name)])
       (syntax (? (lambda (x) (equal? x name)))))]
    [var
     (identifier? #'var)
     (syntax var)]
    [(quote v) (syntax/loc pat 'v)]
    [(p ...)
     (with-syntax ([(p* ...) (map compile-pattern 
                                  (syntax->list #'(p ...)))])
       (syntax (list p* ...)))]
    [(p q ... . r)
     (with-syntax ([(p* ...) (map compile-pattern
                                  (syntax->list #'(p q ...)))]
                   [r* (compile-pattern (syntax r))])
       (syntax (list-rest p* ... r*)))]
    [x #'x]
    ))
(define-syntax (case-match stx)
  (syntax-case stx ()
    [(_ id)
     (unless (identifier? #'id)
       (raise-syntax-error #f "Expected an identifier" stx #'id))
     #'()]
    [(_ id [pat body] ...)
     (unless (identifier? #'id)
       (raise-syntax-error #f "Expected an identifier" stx #'id))
     (with-syntax ([(plt-pat ...) (map compile-pattern
                                       (syntax->list #'(pat ...)))])
       (if (memq '& (syntax->datum #'(pat ...)))
           #'(match id [plt-pat body] ...)
           #'(match id
               [plt-pat body] ... 
               [else '()])))]))
