#lang mischief

(provide macro-tests)

(require
  rackunit
  mischief/preserve-expensive-metadata
  dracula/tests/harness
  syntax/modread)

(define macro-tests
  (test-suite "obsolete macro tests"

    (test-dracula #:name 'macro-in-description
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-description D {}
           (declare-value x0)
           (declare-function f 2)
           (define-metafunction m
             (syntax-parser
               [(_) #'x0]
               [(_ e . es) #'(call f e (m . es))])))
         (define-component C D {}
           (define-value x0 '#false)
           (define-function (f x1 x2) #:infer
             (if x1 x1 (if x2 x2 (m)))))
         (define-value five
           ((deref C m) '#false '5 '#true))
         ((deref C m))
         (#%provide five)])
      #:exports '[five]
      #:checks
      (lambda (five)
        (check-equal? five 5))
      #:results (list #false)
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN X0 ()
                 (,QUOTE ,NIL)))
           (,LOCAL
              (,DEFUN F (X1 X2)
                 (,IF X1 X1 (,IF X2 X2 (X0))))))
        (,DEFSTUB C_X0 () #s(:: ACL2 =>) ,*)
        (,DEFSTUB C_F (,* ,*)
           #s(:: ACL2 =>) ,*)
        (,DEFUN FIVE ()
           (C_F (,QUOTE ,NIL)
             (C_F (,QUOTE 5)
               (C_F (,QUOTE ,T)
                 (C_X0)))))])

    (test-dracula/syntax-error #:name 'label-for-nonexistent-definition
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-description Empty {[some-label-name an-absent-name]})])
      #:error-message
      '[#px"define|definition"
        #px"Empty"
        #px"an-absent-name"])

    (test-dracula/syntax-error #:name 'rename-to-nonexistent-name
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-description One-Value {}
           (declare-value some-value))
         (define-component one-value One-Value
             {[some-value an-absent-name]})])
      #:error-message
      '[#px"an-absent-name"
        #px"one-value"])

    (test-dracula/syntax-error #:name 'rename-to-external-name
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-value an-external-value '1)
         (define-description One-Value {}
           (declare-value some-value))
         (define-component one-value One-Value
             {[some-value an-external-value]})])
      #:error-message
      '[#px"an-external-value"
        #px"one-value"])

    (test-dracula/syntax-error #:name 'rename-nonexistent-label
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-description Empty {})
         (define-component empty Empty
             {[an-absent-label some-internal-name]}
           (define-value some-internal-name '1))])
      #:error-message
      '[#px"an-absent-label"
        #px"Empty"
        #px"empty"])

    (test-dracula/syntax-error #:name 'refine-nonexistent-label
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-value some-external-name '1)
         (define-description Empty {})
         (define-component empty
           (refine Empty [an-absent-label some-external-name])
             {})])
      #:error-message
      '[#px"an-absent-label"
        #px"Empty"])

    (test-dracula #:name 'generate-temporary
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction phantom
           (lambda (stx)
             #`(define-value #,(fresh 'phantom #:add-suffix? #false)
                 'phantom)))
         (define-description desc {}
           (phantom))
         (define-component comp desc {})])
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN PHANTOM ()
                 (,QUOTE phantom))))
        (,SKIP-PROOFS
           (,DEFUN COMP2_PHANTOM ()
              (,QUOTE phantom)))])

    (test-dracula #:name 'mixing-marks
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction phantom
           (lambda (stx)
             #`(splice-definitions
                 (define-description desc {}
                   #,(syntax-local-introduce
                       #'(define-value i 'eye))
                   (define-value y 'why))
                 (define-component comp desc {}))))
         (phantom)])
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN I ()
                 (,QUOTE eye)))
           (,LOCAL
              (,DEFUN Y ()
                 (,QUOTE why))))
        (,SKIP-PROOFS
           (,DEFUN COMP2_I ()
              (,QUOTE eye)))
        (,SKIP-PROOFS
           (,DEFUN COMP2_Y ()
              (,QUOTE why)))])

    (test-dracula #:name 'mixing-marks-twice
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction double-phantom
           (lambda (stx)
             #'(splice-definitions
                 (define-metafunction phantom
                   (lambda (stx)
                     #`(splice-definitions
                         (define-description desc {}
                           #,(syntax-local-introduce
                               #'(define-value i 'eye))
                           (define-value y 'why))
                         (define-component comp desc {}))))
                 (phantom))))
         (double-phantom)])
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN I ()
                 (,QUOTE eye)))
           (,LOCAL
              (,DEFUN Y ()
                 (,QUOTE why))))
        (,SKIP-PROOFS
           (,DEFUN COMP2_I ()
              (,QUOTE eye)))
        (,SKIP-PROOFS
           (,DEFUN COMP2_Y ()
              (,QUOTE why)))])

    (test-dracula #:name 'mixing-marks/explicit-labels
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction phantom
           (lambda (stx)
             (with-syntax {[i (syntax-local-introduce #'i)]}
               #'(splice-definitions
                   (define-description desc {[i i] [y y]}
                     (define-value i 'eye)
                     (define-value y 'why))
                   (define-component comp desc {[i i] [y y]})))))
         (phantom)])
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN I ()
                 (,QUOTE eye)))
           (,LOCAL
              (,DEFUN Y ()
                 (,QUOTE why))))
        (,SKIP-PROOFS
           (,DEFUN COMP2_I ()
              (,QUOTE eye)))
        (,SKIP-PROOFS
           (,DEFUN COMP2_Y ()
              (,QUOTE why)))])

    (test-dracula #:name 'mixing-marks-twice/explicit-labels
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction double-phantom
           (lambda (stx)
             #'(splice-definitions
                 (define-metafunction phantom
                   (lambda (stx)
                     (with-syntax {[i (syntax-local-introduce #'i)]}
                       #'(splice-definitions
                           (define-description desc {[i i] [y y]}
                             (define-value i 'eye)
                             (define-value y 'why))
                           (define-component comp desc {[i i] [y y]})))))
                 (phantom))))
         (double-phantom)])
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN I ()
                 (,QUOTE eye)))
           (,LOCAL
              (,DEFUN Y ()
                 (,QUOTE why))))
        (,SKIP-PROOFS
           (,DEFUN COMP2_I ()
              (,QUOTE eye)))
        (,SKIP-PROOFS
           (,DEFUN COMP2_Y ()
              (,QUOTE why)))])

    (test-dracula #:name 'desc/comp-with-differing-marks
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction make-desc
           (syntax-parser
             [(_ make-comp:id decl:expr ...)
              #'(splice-definitions
                  (define-description desc {}
                    decl ...)
                  (define-metafunction make-comp
                    (syntax-parser
                      [(_ comp:id defn:expr [... ...])
                       #'(define-component comp desc {}
                           defn [... ...])])))]))
         (make-desc make-comp (declare-value x))
         (make-comp comp (define-value x '1))
         (define-value y (deref comp x))
         (#%provide y)])
      #:exports '(y)
      #:checks (lambda (y) (equal? y 1))
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN X ()
                 (,QUOTE 1))))
        (,DEFSTUB COMP2_X () #s(:: ACL2 =>) ,*)
        (,DEFUN Y () (COMP2_X))])

    (test-dracula #:name 'desc/comp-with-unrelated-marks
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-metafunction macro
           (lambda (stx)
             (define desc-id
               ((make-syntax-introducer)
                (datum->syntax #false 'desc)))
             (define comp-id
               ((make-syntax-introducer)
                (datum->syntax #false 'comp)))
             #`(splice-definitions
                 (define-description #,desc-id {}
                   (declare-value x))
                 (define-component #,comp-id #,desc-id {}
                   (define-value x '#false)))))
         (macro)])
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN X ()
                 (,QUOTE ,NIL))))
        (,DEFSTUB COMP2_X () #s(:: ACL2 =>) ,*)])

    (test-dracula #:name 'labels-should-ignore-bindings
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-description desc {}
           (declare-value x))
         (define-component comp desc {}
           (define-value x '#true))
         (define-value true
           (let {[x 'shadow]}
             (deref comp x)))
         (#%provide true)])
      #:exports '(true)
      #:checks (lambda (true) (check-equal? true #true))
      #:proof
      `[(,ENCAPSULATE ()
           (,LOCAL
              (,DEFUN X ()
                 (,QUOTE ,T))))
        (,DEFSTUB COMP2_X () #s(:: ACL2 =>) ,*)
        (,DEFUN TRUE ()
           (,LET {[X2 (,QUOTE shadow)]}
              (COMP2_X)))])

    (test-case "underscore reader"
      (check-not-exn
        (lambda ()
          (define expected
            '(module anonymous-module dracula
               (#%module-begin
                 (deref a b)
                 (deref (deref c d) e)
                 (1 2 (deref (deref x y) z)))))
          (define text
            (string-append
              "#lang dracula\n"
              "a_b\n"
              "c_d_e\n"
              "(1 2 (deref x_y z))\n"))
          (define stx
            (with-module-reading-parameterization
              (lambda ()
                (parameterize
                    {[current-input-port
                      (open-input-string text)]}
                  (read-syntax)))))
          (define actual
            (to-datum stx))
          (check-equal? actual expected))))))
