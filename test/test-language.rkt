#lang racket

(require "../private/collects.rkt"
         "../proof/proof.rkt"
         "../proof/syntax.rkt"
         "../lang/acl2-module-v.rkt"
         "program-tests.rkt")
(require rackunit
         (cce main))

(provide test-language)

(define loc (make-loc #f #f #f))

(define program
  (papply make-module-program/mred acl2-module-v))

(define (proof . args)
  (make-proof (make-part 'Dracula loc (map sexp->term args))))

(define (sexp->term sexp)
  (make-term sexp loc))

(define (check-prove program proof)
  (check-equal? (get-proof (car (check-expand-success program))) proof))

(define check-run
  (case-lambda
    [(program) (check-eval-success program)]
    [(program result)
     (check-equal? (check-eval-success program) result)]))

(define (check-run/prove program result proof)
  (check-run program result)
  (check-prove program proof))

(define (check-syntax-equal? a b)
  (check-equal? (datum a) (datum b)))

(define (datum v) (syntax->datum (datum->syntax #f v)))

(define (expr-term stx) (make-term stx #f))
(define (event-term stx) (make-term stx #t))

(define test-language
  (test-suite "ACL2"
    (test-suite "defun"
      #| ;; Allowed now, for Scribble purposes
      (test-case "repl definition"
      (check-expand-failure
      (program '[] '[(defun id (x) x)]))) |#
      (test-case "bad function name"
        (check-expand-failure
         (program '[(defun t () 1)])))
      (test-case "bad argument name"
        (check-expand-failure
         (program '[(defun f (nil) 1)])))
      (test-case "recursion arity error"
        (check-expand-failure
         (program '[(defun f (x) (f x x))])))
      (test-case "arity error"
        (check-expand-failure
         (program '[(defun f (x) x)] '[(f 1 2)])))
      (test-case "redefinition"
        (check-expand-failure
         (program '[(defun f () 1) (defun f () 2)])))
      (test-case "forward reference"
        (check-expand-failure
         (program '[(defun a () (b)) (defun b () 1)])))
      (test-case "identity"
        (check-run/prove
         (program '[(defun id (x) x)] '[(id 2)])
         2
         (proof '(defun id (x) x))))
      (test-case "backward reference"
        (check-run/prove
         (program '[(defun a () 1) (defun b () (a))] '[(b)])
         1
         (proof '(defun a () 1) '(defun b () (a)))))
      (test-case "recursion"
        (check-run/prove
         (program '[(defun depth (v)
                      (if (atom v) 0 (1+ (max (depth (car v))
                                              (depth (cdr v))))))]
                  '[(depth '((1 . 2) . ((3 . 4) . 5)))])
         3
         (proof
          '(defun depth (v)
             (if (atom v) 0 (1+ (max (depth (car v))
                                     (depth (cdr v))))))))))
    (test-suite "defstub"
      (test-case "invalid name"
        (check-expand-failure
         (program '[(defstub nil () t)])))
      (test-case "valid stub"
        (check-eval-success
         (program '[(defstub s (x y) t)])))
      (test-case "direct call"
        (check-eval-failure
         (program '[(defstub s () t)] '[(s)])))
      (test-case "indirect call"
        (check-eval-failure
         (program '[(defstub s () t) (defun f () (s))] '[(f)]))))
    (test-suite "mutual recursion"
      (test-case "two functions"
        (check-run/prove
         (program '[(mutual-recursion
                     (defun even-p (x)
                       (if (zp x) t (odd-p (1- x))))
                     (defun odd-p (x)
                       (if (zp x) nil (even-p (1- x)))))]
                  '[(even-p 4)])
         't
         (proof
          '(mutual-recursion
            (defun even-p (x)
              (if (zp x) t (odd-p (1- x))))
            (defun odd-p (x)
              (if (zp x) nil (even-p (1- x))))))))
      (test-case "arity error"
        (check-expand-failure
         (program '[(mutual-recursion
                     (defun a (x y) (b x y))
                     (defun b (x) (a x x)))]))))
    (test-suite "defconst"
      (test-case "reference"
        (check-run/prove
         (program '[(defconst *five* 5)] '[*five*])
         5
         (proof '(defconst *five* 5))))
      (test-case "bad name"
        (check-expand-failure
         (program '[(defconst five 5)]))))
    (test-suite "#%app"
      (test-case "function"
        (check-run/prove
         (program '[] '[(max 1 2)])
         2
         (proof)))
      (test-case "variable arity"
        (check-run/prove
         (program '[] '[(list (+ 1) (+ 1 2) (+ 1 2 3))])
         '(1 3 6)
         (proof))))
    (test-suite "defstructure"
      (test-case "defined functions"
        (check-run/prove
         (program '[(defstructure point x y)]
                  '[(list (point-x (point 1 2))
                          (point-y (point 1 2))
                          (point-p (point 1 2))
                          (weak-point-p (point 1 2)))])
         '(1 2 t t)
         (proof '(defstructure point x y))))
      (test-case "forward reference"
        (check-expand-failure
         (program '[(defun origin () (point 0 0))
                    (defstructure point x y)]))))
    ))
