#lang racket

#|
Needed to override some Scheme printer behavior (nil, complex)
|#

(provide 
  ;;acl2-print
  acl2-convert)

(define (acl2-convert x)
  (cond [(null? x) 'nil]
    [(pair? x) 
     (let ([fst (acl2-convert (car x))]
           [rst (cdr x)])
       (cons fst (if (null? rst) rst (acl2-convert rst))))]
    [(rational? x) x] ;; otherwise 5 would print as (complex 5 0)
    [(complex? x) `(complex ,(real-part x) ,(imag-part x))]
    [else x]))

;;(define old-print (current-print))
;;(define (acl2-print x) (old-print (acl2-convert x)))
