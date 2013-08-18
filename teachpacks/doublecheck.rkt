#lang racket

(require "../private/collects.rkt"
         "../lang/defun.rkt"
         "../lang/check.rkt"
         racket/gui/dynamic
         racket/stxparam
         (cce function)
         (prefix-in r: (fasttest random))
         (fasttest rackunit)
         rackunit
         (for-syntax syntax/stx
                     racket/stxparam-exptime
                     (cce syntax)
                     (cce text)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM GENERATOR FUNCTIONS
;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-acl2-boolean) (r:random-choice 't '()))
(define (random-acl2-symbol) (r:random-symbol))
(define (random-acl2-string) (r:random-string))
(define (random-acl2-char) (r:random-char))
(define (random-acl2-number) (r:random-exact))
(define (random-acl2-rational) (r:random-rational))
(define (random-acl2-integer) (r:random-integer))
(define (random-acl2-natural) (r:random-natural))
(define (random-acl2-between lo hi) (r:random-integer/uniform lo hi))
(define (random-acl2-data-size) (r:random-natural/geometric 1/4 0))

(define (random-acl2-element-of l)
  (list-ref l (random-acl2-between 0 (- (length l) 1))))

(define (random-acl2-atom)
  (r:random-case
   (random-acl2-boolean)
   (random-acl2-symbol)
   (random-acl2-string)
   (random-acl2-char)
   (random-acl2-number)))

(define (random-acl2-sexp-of/proc make-atom size)
  (if (<= size 0)
      (make-atom)
      (let* ([left (random-acl2-between 0 (- size 1))]
             [right (- size left 1)])
        (cons (random-acl2-sexp-of/proc make-atom left)
              (random-acl2-sexp-of/proc make-atom right)))))

(define-syntax (random-acl2-sexp-of stx)
  (syntax-case stx (:size)
    [(_ atom :size size)
     (syntax/loc stx
       (random-acl2-sexp-of/proc (lambda () atom) size))]
    [(_ atom)
     (syntax/loc stx
       (random-acl2-sexp-of atom :size (random-acl2-data-size)))]))

(define (random-acl2-sexp)
  (random-acl2-sexp-of (random-acl2-atom)))

(define (random-acl2-list-of/proc make-elem size)
  (if (<= size 0)
      '()
      (cons (make-elem) (random-acl2-list-of/proc make-elem (- size 1)))))

(define-syntax (random-acl2-list-of stx)
  (syntax-case stx (:size)
    [(_ elem :size size)
     (syntax/loc stx
       (random-acl2-list-of/proc (lambda () elem) size))]
    [(_ elem)
     (syntax/loc stx
       (random-acl2-list-of elem :size (random-acl2-data-size)))]))

(define-for-syntax (expand-random-case-args stx)
  (syntax-case stx (:weight)
    [() stx]
    [(:weight . _)
     (syntax-error (stx-car stx)
                   "got :weight keyword instead of an expression")]
    [(arg :weight)
     (syntax-error (stx-car (stx-cdr stx))
                   "got :weight keyword without a weight")]
    [(arg :weight :weight . _)
     (syntax-error (stx-car (stx-cdr (stx-cdr stx)))
                   "got duplicate :weight keyword")]
    [(arg :weight wt . rest)
     (quasisyntax/loc stx
       (arg #:weight wt #,@(expand-random-case-args #'rest)))]
    [(arg . rest)
     (quasisyntax/loc stx
       (arg #,@(expand-random-case-args #'rest)))]
    [_ (syntax-error
        stx
        "expected a sequence of arguments with :weight keywords")]))

(define-syntax (random-acl2-case stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ . rest)
       (quasisyntax/loc stx
         (r:random-case #,@(expand-random-case-args #'rest)))])))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM GENERATOR MACROS (unusable in regular code)
;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-parameter dracula-random? #f)

(define-for-syntax (make-random-transformer id)
  (lambda (stx)
    (if (syntax-parameter-value #'dracula-random?)
        (syntax-case stx ()
          [(_ . rest) (quasisyntax/loc stx (#,id . rest))]
          [_ id])
        (raise-syntax-error
         #f
         "cannot be used outside random testing"
         stx))))

(define-syntax (macro:defrandom stx)
  (syntax-case stx ()
    [(_ f (arg ...) body)
     (with-syntax ([(formal ...) (generate-temporaries #'(arg ...))]
                   [g (syntax-local-introduce #'f)])
       (syntax/loc stx
         (begin (defun g (arg ...)
                  (syntax-parameterize ([dracula-random? #t]) body))
                (define-syntax f (make-random-transformer #'g)))))]))

(define-syntax (define-random-macro stx)
  (syntax-case stx ()
    [(_ macro (function arg ...))
     (syntax/loc stx
       (macro:defrandom
        macro (arg ...)
        (function arg ...)))]))

(define-syntax macro:random-case
  (make-random-transformer #'random-acl2-case))
(define-syntax macro:random-list-of
  (make-random-transformer #'random-acl2-list-of))
(define-syntax macro:random-sexp-of
  (make-random-transformer #'random-acl2-sexp-of))

(begin-below
 (define-random-macro macro:random-boolean (random-acl2-boolean))
 (define-random-macro macro:random-symbol (random-acl2-symbol))
 (define-random-macro macro:random-char (random-acl2-char))
 (define-random-macro macro:random-string (random-acl2-string))
 (define-random-macro macro:random-number (random-acl2-number))
 (define-random-macro macro:random-rational (random-acl2-rational))
 (define-random-macro macro:random-integer (random-acl2-integer))
 (define-random-macro macro:random-natural (random-acl2-natural))
 (define-random-macro macro:random-between (random-acl2-between lo hi))
 (define-random-macro macro:random-atom (random-acl2-atom))
 (define-random-macro macro:random-sexp (random-acl2-sexp))
 (define-random-macro macro:random-data-size (random-acl2-data-size))
 (define-random-macro macro:random-element-of (random-acl2-element-of l)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DEFPROPERTY and CHECK-PROPERTIES
;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-repeat 50)
(define default-limit 2500)
(define current-limit (make-parameter #f))

;; reversed-properties : (Listof (-> TestSuite))
;; Mutable variable holding thunks generated but not run so far.
;; Each thunk constructs a test suite for a doublecheck property.
(define reversed-properties null)

(define (add-property! prop)
  (set! reversed-properties
        (cons prop reversed-properties)))

(define (acl2-true? v)
  (not (or (eq? v 'nil) (eq? v '()))))

(define-check (check-acl2-true v)
  (unless (acl2-true? v)
    (fail-check)))

(define-for-syntax (keyword-identifier? stx)
  (and (identifier? stx)
       (regexp-match? #rx"^:" (symbol->string (syntax-e stx)))))

(define-for-syntax (name-identifier? stx)
  (and (identifier? stx)
       (not (keyword-identifier? stx))))

(define-for-syntax (positive-constant? stx)
  (and (syntax? stx)
       (exact-positive-integer? (syntax-e stx))))

(define-for-syntax (extract-options stx)
  (syntax-case stx ()
    [(keyword value . rest)
     (and (keyword-identifier? #'keyword)
          (not (keyword-identifier? #'value)))
     (with-syntax ([(((k v) ...) . body)
                    (extract-options #'rest)])
       (syntax/loc stx
         (((keyword value) (k v) ...) . body)))]
    [(keyword . rest)
     (keyword-identifier? #'keyword)
     (syntax-error #'keyword "no value provided")]
    [body (syntax/loc stx (() . body))]))

(define-for-syntax (parse-options stx)
  (with-syntax ([(opts . rest) (extract-options stx)])
    (cons (sort (syntax->list #'opts) text<? #:key stx-car) #'rest)))

(define-for-syntax (get-clause-options src var stx)
  (syntax-case stx (:limit :value :where)

    ;; Subsets of the options:
    [((:limit limit) (:value value) (:where where)) #'(limit value where)]
    [((:limit limit) (:value value)) #'(limit value 't)]
    [((:limit limit) (:where where)) #'(limit (random-acl2-sexp) where)]
    [((:value value) (:where where)) #'((current-limit) value where)]
    [((:limit limit)) #'(limit (random-acl2-sexp) 't)]
    [((:value value)) #'((current-limit) value 't)]
    [((:where where)) #'((current-limit) (random-acl2-sexp) where)]
    [() #'((current-limit) (random-acl2-sexp) 't)]

    ;; Error:
    [((key val) ...)
     (raise-syntax-error
      #f
      (format "Unsupported list of variable options: ~s"
              (syntax->datum #'(key ...)))
      src var)]))

(define-for-syntax (parse-clauses src stx)
  (syntax-case stx ()
    [() null]
    [(var . rest)
     (name-identifier? #'var)
     (with-syntax ([(opts . vars) (parse-options #'rest)])
       (with-syntax ([(limit value where)
                      (get-clause-options src #'var #'opts)])
         (cons #'(var value where limit) (parse-clauses src #'vars))))]
    [(var . rest)
     (raise-syntax-error
      #f "expected a variable, possibly followed by options" #'src #'var)]))

(define-for-syntax (expand-defproperty stx)
  (syntax-case stx (:repeat :limit)
    [(orig name ((:limit limit) (:repeat repeat)) (var ...) body opt ...)
     (with-syntax ([((lhs rhs hyp lim) ...)
                    (parse-clauses #'orig #'(var ...))])
       (syntax/loc #'orig
         (begin
           (define-syntax (name stx*)
             (raise-syntax-error
              #f
              "DoubleCheck properties may not be used as expressions."
              stx*))
           (define-values []
             (begin
               (add-property!
                (lambda ()
                  (let* ([the-limit limit])
                    (test-random
                     #:name (symbol->string 'name)
                     #:repeat repeat
                     #:limit the-limit
                     ([lhs (syntax-parameterize ([dracula-random? #t]) rhs)
                           #:where (acl2-true? hyp)
                           #:limit (parameterize ([current-limit the-limit])
                                     lim)]
                      ...)
                     (with-check-info
                      (['check-expect
                        `(check-expect (let ((lhs ',lhs) ...) body) t)])
                      (check-acl2-true body))))))
               (values))))))]

    [(orig name ((:limit limit)) (var ...) body opt ...)
     (expand-defproperty
      #'(orig name
              ((:limit limit) (:repeat default-repeat))
              (var ...) body opt ...))]

    [(orig name ((:repeat repeat)) (var ...) body opt ...)
     (expand-defproperty
      #'(orig name
              ((:limit default-limit) (:repeat repeat))
              (var ...) body opt ...))]

    [(orig name () (var ...) body opt ...)
     (expand-defproperty
      #'(orig name
              ((:limit default-limit) (:repeat default-repeat))
              (var ...) body opt ...))]

    [(orig name ((key val) ...) (var ...) body opt ...)
     (raise-syntax-error
      #f
      (format "Unsupported list of defproperty options: ~s"
              (syntax->datum #'(key ...)))
      #'orig)]

    [(orig name ((key val) ...) . rest)
     (raise-syntax-error
      #f
      (format "Expected variables, a body, and theorem options, got: ~s"
              (syntax->datum #'rest))
      #'orig #'rest)]

    [_ (raise-syntax-error 'defproperty "internal error" stx)]))

(define-syntax (macro:defproperty stx)
  (syntax-case stx ()
    [(_ orig name . rest)
     (name-identifier? #'name)
     (with-syntax ([(options . body) (parse-options #'rest)])
       (expand-defproperty
        #'(orig name options . body)))]
    [(_ orig other . rest)
     (raise-syntax-error
      #f
      (format "expected a property name, got: ~s"
              (syntax->datum #'other))
      #'orig #'other)]
    [(_ orig . rest)
     (raise-syntax-error
      #f
      "expected a property name, variables, and a body, got nothing"
      #'orig)]
    [_ (raise-syntax-error 'defproperty "internal error" stx)]))

(define ui
  (if (gui-available?)
      (let* ([gui (dynamic-require 'rackunit/gui 'test/gui)])
        (lambda (test) (thread (lambda () (gui test)))))
      (dynamic-require 'rackunit/text-ui 'run-tests)))

(define (get-test-ui) ui)

(define (real-check-properties!)
  (when (cons? reversed-properties)
    (let* ([properties (reverse reversed-properties)])
      (set! reversed-properties null)
      ((get-test-ui)
       (make-test-suite "DoubleCheck"
         (for/list {[property (in-list properties)]}
           (property)))))))

(define-syntax-rule (check-properties!)
  (define-values []
    (begin
      (real-check-properties!)
      (values))))

(define-for-syntax (make-rename/original-transformer id)
  (lambda (stx)
    (with-syntax ([src stx] [dst id])
      (syntax-case stx ()
        [(head . rest)
         (identifier? #'head)
         (syntax/loc stx (dst src . rest))]
        [head
         (identifier? #'head)
         (syntax/loc stx (dst . src))]))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SIGNATURE AND UNIT
;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide defproperty defrandom random-case
         random-boolean random-symbol random-char random-string
         random-number random-rational random-integer random-natural
         random-data-size random-between
         random-atom random-sexp
         random-list-of random-sexp-of random-element-of
         check-properties check-properties!)

;; Random macros
(define-syntaxes (defproperty)
  (make-rename/original-transformer #'macro:defproperty))
(define-syntaxes (defrandom)
  (make-rename-transformer #'macro:defrandom))
(define-syntaxes (random-case)
  (make-rename-transformer #'macro:random-case))

;; Random primitive types
(define-syntaxes (random-boolean)
  (make-rename-transformer #'macro:random-boolean))
(define-syntaxes (random-symbol)
  (make-rename-transformer #'macro:random-symbol))
(define-syntaxes (random-char)
  (make-rename-transformer #'macro:random-char))
(define-syntaxes (random-string)
  (make-rename-transformer #'macro:random-string))
(define-syntaxes (random-number)
  (make-rename-transformer #'macro:random-number))
(define-syntaxes (random-rational)
  (make-rename-transformer #'macro:random-rational))
(define-syntaxes (random-integer)
  (make-rename-transformer #'macro:random-integer))
(define-syntaxes (random-natural)
  (make-rename-transformer #'macro:random-natural))

;; Random custom atoms
(define-syntaxes (random-data-size)
  (make-rename-transformer #'macro:random-data-size))
(define-syntaxes (random-between)
  (make-rename-transformer #'macro:random-between))

;; Random atoms and s-expressions
(define-syntaxes (random-atom)
  (make-rename-transformer #'macro:random-atom))
(define-syntaxes (random-sexp)
  (make-rename-transformer #'macro:random-sexp))

;; Random higher-order data
(define-syntaxes (random-list-of)
  (make-rename-transformer #'macro:random-list-of))
(define-syntaxes (random-sexp-of)
  (make-rename-transformer #'macro:random-sexp-of))

;; Random selection
(define-syntaxes (random-element-of)
  (make-rename-transformer #'macro:random-element-of))

;; Boilerplate function
(define-syntaxes (check-properties)
  (lambda (stx)
    (case (syntax-local-context)
      [(expression) (raise-syntax-error #f
                      "may not be used as an expression"
                      stx)])
    (syntax-case stx ()
      [(_) #'(begin)])))
