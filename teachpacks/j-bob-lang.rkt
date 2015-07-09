#lang racket/base
(require
  "../lang/dracula.rkt"
  "../lang/do-check.rkt")
(require (prefix-in r. racket/base))
(provide (all-defined-out))

(begin-below

  (define-syntax dethm
    (syntax-rules ()
      ((_ name (arg ...) body)
       (define (name arg ...) body))))

  (defun size (x)
    (if (atom x)
        '0
        (+ '1 (size (car x)) (size (cdr x))))))
