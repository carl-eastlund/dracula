#lang mischief

(provide
  size
  implies
  illegal)

(define (implies x y)
  (if x (if y #true #false) #true))

(define (size x)
  (cond!
    [(cons? x) (+ (size (car x)) (size (cdr x)) 1)]
    [(number? x) (number-size (inexact->exact x))]
    [(string? x) (string-length x)]
    [else 0]))

(define (number-size x)
  (cond!
    [(integer? x) (abs x)]
    [(rational? x)
     (+ (abs (numerator x))
        (denominator x))]
    [else
     (+ (number-size (real-part x))
        (number-size (imag-part x))
        1)]))

(define (illegal name fmt alist)
  (define-values {parsed args}
    (parse-format (string->list fmt)
      (make-immutable-hash alist)))
  (apply error name (list->string parsed) args))

(define (parse-format chars dict)
  (match! chars
    [(list* #\~ spec key others)
     (define type
       (match! spec
         [#\x #\s]
         [#\f #\a]
         [_ (error 'illegal
              "invalid ~~ specifier; expected only ~~x or ~~f, but got ~~~a"
              spec)]))
     (define arg
       (dict-ref dict key
         (lambda {}
           (error 'illegal
             "invalid ~~~a specifier; key ~s not found in arguments"
             spec
             key))))
     (define-values {parsed args}
       (parse-format others dict))
     (values (list* #\~ type parsed) (cons arg args))]
    [(list* #\~ others)
     (error 'illegal
       "invalid ~~ specifier; expected at least two characters after ~~")]
    [(cons c others)
     (define-values {parsed args}
       (parse-format others dict))
     (values (cons c parsed) args)]
    ['() (values '() '())]))
