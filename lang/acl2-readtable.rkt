#lang racket

#|
Reject #t and #f, allow #c for complex numbers.
Also hack around some ACL2 preprocessor directives.  This was necessary
for `include-book', but support of include-book has been dropped for now.
|#

(require syntax/readerr)

(provide acl2-readtable)

;; I've hard-coded some preprocessor definitions here.
;; #+expr1 expr2 -- expr1 and expr2 are never evaluated.
;; #-expr1 expr2 -- gcl non-standard-analysis :non-standard-analysis are all undefined
(define (hash-minus char port w x y z)
  (parameterize ([read-case-sensitive #f])
    (let ([test-expr (read port)])
      (if (memq test-expr '(gcl non-standard-analysis :non-standard-analysis))
        (make-special-comment test-expr)
        (make-special-comment (list test-expr (read port)))))))

(define (hash-plus char port w x y z)
  (parameterize ([read-case-sensitive #f])
    (let ([test-expr (read port)])
      (if (memq test-expr '(non-standard-analysis :non-standard-analysis))
        (make-special-comment (list test-expr (read port)))
        (make-special-comment (list test-expr (read port)))))))

;; Raise an error on #t and #f
(define hash-bool
  (lambda (char port w x y z)
    (parameterize ([read-case-sensitive #f])
      (raise-read-error (string-append (string #\# char)  " is not a valid ACL2 datum.  "
                          "The booleans are t and nil.")
        w x y z 2))))

;; #C(M N) is a complex number when M and N are numerals
(define (hash-c char port w x y z)
  (parameterize ([read-case-sensitive #f])
    (case (peek-char port)
      [(#\() (let ([expr (read port)])
               (if (and (list? expr) (= 2 (length expr)))
                 (let ([fst (car expr)]
                       [snd (cadr expr)])
                   (if (and (number? fst) (number? snd))
                     (+ fst (* snd 0+i))
                     (raise-read-error "Expected #c(x y) where x and y are both numerals"
                       w x y z 1)))
                 (raise-read-error "Expected #c(x y) where x and y are both numerals"
                   w x y z 1)))]
      [else (read-syntax/recursive w (input-port-append #f (open-input-string (string char)) port)
              #\# #f)])))

(define (hash-double-quote char port w x y z)
  (raise-read-error (string-append (string #\# char) " is not a valid ACL2 reader syntax.  ")
    w x y z 2))

(define (hash-semicolon char port w x y z)
  (raise-read-error (string-append (string #\# char) " cannot be used for ACL2 comments.  ")
    w x y z 2))

(define check-infinity
  (let ([inf-str "inf.0"])
    (lambda (char port src x y z)
      (parameterize ([read-case-sensitive #f])
        (let* ([next-ch (peek-char port)]
               [next (if (and (char? next-ch) (char-ci=? next-ch #\i)) (read port) #f)])
          (cond [(and (symbol? next) (string-ci=? inf-str (symbol->string next)))
                 (raise-read-error "ACL2 cannot perform floating-point arithmetic"
                   src x y z 6)]
            [(eof-object? next) (read-syntax/recursive src port char #f)]
            [(char-ci=? next-ch #\i)
             (let ([prefix (open-input-string (let ([x (open-output-string)])
                                                (write next x)
                                                (get-output-string x)))])
               (read-syntax/recursive src (input-port-append #f prefix port) char #f))]
            [else (read-syntax/recursive src port char #f)])))
      )))

(define acl2-readtable
  (make-readtable (current-readtable)
    #\; 'dispatch-macro hash-semicolon
    #\t 'dispatch-macro hash-bool
    #\T 'dispatch-macro hash-bool
    #\f 'dispatch-macro hash-bool
    #\F 'dispatch-macro hash-bool
    #\" 'dispatch-macro hash-double-quote
    #\c 'dispatch-macro hash-c
    #\C 'dispatch-macro hash-c
    #\- 'dispatch-macro hash-minus
    #\+ 'dispatch-macro hash-plus
    #\+ 'non-terminating-macro check-infinity
    #\- 'non-terminating-macro check-infinity
    ))
