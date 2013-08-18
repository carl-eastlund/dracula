#lang racket

(require "../private/collects.rkt"
         racket/sandbox)
(require rackunit
         rackunit/gui
         (cce main))

(provide test-private)

(define define-below-require
  '(require (lib "dracula" "lang" "check.rkt")))

(define (eval/module code)
  ((make-trusted-evaluator 'racket)
   `(module temp racket (#%module-begin ,define-below-require ,@code))))

(define (eval/internal code)
  (eval/module `[(let () ,@code (void))]))

(define (sexps->string first . rest)
  (apply format
         (apply string-append "~s" (map (lambda args "\n~s") rest))
         first rest))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: not testing the top-level,
;; which generates no or runtime error
;; rather than syntax error for
;; unbound names.
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (test-eval/success stx)
  (syntax-case stx ()
    [(te/s code)
     (syntax/loc stx
       (let ([c code])
         (test-suite (apply sexps->string c)
           (test-not-exn "module" (lambda () (eval/module c)))
           (test-not-exn "internal" (lambda () (eval/internal c))))))]))

(define-syntax (test-eval/failure stx)
  (syntax-case stx ()
    [(te/f pred code)
     (syntax/loc stx
       (let ([c code]
             [p pred])
         (test-suite (apply sexps->string c)
           (test-exn "module" p (lambda () (eval/module c)))
           (test-exn "internal" p (lambda () (eval/internal c))))))]))

;; Test references outside of head-expansion position,
;; as that does not work in internal contexts.

(define test-private
  (test-suite "private"
    (test-suite "define-below"
      (test-eval/success
       '[(begin-below
          (define-below (f x) (+ 1 x))
          (define-below (g x) (+ 1 (f x))))
         (list (f 1))
         (list (g 1))])
      (test-eval/success
       '[(begin-below
          (define-values-below (evenp oddp)
            (values (lambda (x) (if (zero? x) #t (oddp (- x 1))))
                    (lambda (x) (if (zero? x) #f (evenp (- x 1)))))))
         (list (evenp 4))
         (list (oddp 4))])
      (test-eval/success
       '[(lambda () (begin-below (define-below (f x) 1)) (list (f 2)))])
      (test-eval/failure
       exn:fail:syntax?
       '[(define-below x 1)])
      (test-eval/failure
       exn:fail:syntax?
       '[(begin-below
          (define-below (j x) (+ 1 (h x)))
          (define-below (h x) (+ 1 x)))])
      (test-eval/failure
       exn:fail:syntax?
       '[(list (k 1))
         (begin-below
          (define-below k (lambda (x) (+ 1 x))))]))))
