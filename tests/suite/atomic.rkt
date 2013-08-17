#lang mischief

(provide atomic-tests)

(require
  rackunit
  mischief/preserve-expensive-metadata
  dracula/tests/harness)

(define atomic-tests
  (test-suite "expressions and atomic definitions"

    (test-dracula #:name 'empty-program
      #:lang 'dracula/kernel)

    (test-dracula/syntax-error #:name 'toplevel-keyword
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [#:illegal]))

    (test-dracula #:name 'fully-expanded-expressions
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        ['value
         (if 'a 'b 'c)
         (let-values {[(x) 'y]} x)])
      #:results (list 'value 'b 'y))

    (test-dracula #:name 'fully-expanded-value-definitions
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {v1}
           (#%plain-lambda {}
             '#:function
             '#:no-measure
             '#:no-goals
             '"literal"))
         (define-values {v2}
           (#%plain-lambda {}
             '#:function
             '#:no-measure
             '#:no-goals
             'value))
         (define-values {v3}
           (#%plain-lambda {}
             '#:function
             '#:no-measure
             '#:no-goals
             (if 'a (#%plain-app v1) (#%plain-app v2))))
         (define-values {v4}
           (#%plain-lambda {}
             '#:function
             '#:no-measure
             '#:no-goals
             (let-values {[(x) (#%plain-app v2)]} x)))
         (#%provide v1 v2 v3 v4)])
      #:exports '(v1 v2 v3 v4)
      #:checks
      (lambda (v1 v2 v3 v4)
        (check-equal? (v1) "literal")
        (check-equal? (v2) 'value)
        (check-equal? (v3) "literal")
        (check-equal? (v4) 'value))
      #:proof
      `[(,DEFUN V1 () (,QUOTE "literal"))
        (,DEFUN V2 () (,QUOTE value))
        (,DEFUN V3 ()
          (,IF (,QUOTE a) (V1) (V2)))
        (,DEFUN V4 () (,LET ((X (V2))) X))])

    (test-dracula #:name 'fully-expanded-function-definitions
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {neg}
           (#%plain-lambda {a}
             '#:function
             '#:no-measure
             '#:no-goals
             (if a '#false '#true)))
         (define-values {conj}
           (#%plain-lambda {a b}
             '#:function
             '#:no-measure
             '#:no-goals
             (if a b '#false)))
         (define-values {disj}
           (#%plain-lambda {a b}
             '#:function
             '#:no-measure
             '#:no-goals
             (#%plain-app neg
               (#%plain-app conj
                 (#%plain-app neg a)
                 (#%plain-app neg b)))))
         (#%provide neg conj disj)])
      #:exports '(neg conj disj)
      #:checks
      (lambda (neg conj disj)
        (check-equal? (neg #true) #false)
        (check-equal? (neg #false) #true)
        (check-equal? (conj #true #true) #true)
        (check-equal? (conj #true #false) #false)
        (check-equal? (conj #false #true) #false)
        (check-equal? (conj #false #false) #false)
        (check-equal? (disj #true #true) #true)
        (check-equal? (disj #true #false) #true)
        (check-equal? (disj #false #true) #true)
        (check-equal? (disj #false #false) #false))
      #:proof
      `[(,DEFUN NEG (A)
          (,IF A
            (,QUOTE ,NIL)
            (,QUOTE ,T)))
        (,DEFUN CONJ (A B)
          (,IF A B (,QUOTE ,NIL)))
        (,DEFUN DISJ (A B)
          (NEG (CONJ (NEG A) (NEG B))))])

    (test-dracula #:name 'explicit-measure
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {f}
           (#%plain-lambda {x}
             '#:function
             (#%plain-lambda {} '#:measure '1)
             '#:no-goals
             x))])
      #:proof
      `[(,DEFUN F (X)
          (,DECLARE
            (,XARGS
              #:MEASURE
              (,QUOTE 1)))
          X)])

    (test-dracula #:name 'successful-assertion
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {}
           (begin0 (values)
             (#%assert (quote success))))])
      #:proof
      `[(,ASSERT-EVENT (,QUOTE success))])

    (test-dracula/expansion/runtime-error #:name 'failed-assertion
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {}
           (begin0 (values)
             (#%assert (quote #false))))])
      #:proof
      `[(,ASSERT-EVENT (,QUOTE ,NIL))]
      #:error-message '[#px"(?i:fail)"])

    (test-dracula/expansion/runtime-error #:name 'failed-assertion-with-source
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {}
           (begin0 (values)
             (#%assert (quote #false) (quote-syntax (assert false)))))])
      #:proof
      `[(,ASSERT-EVENT (,QUOTE ,NIL))]
      #:error-message
      (list #px"(?i:fail)" (regexp-quote "(assert false)")))

    (test-dracula #:name 'in-theory
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {f}
           (#%plain-lambda {x}
             '#:function
             '#:no-measure
             '#:no-goals
             x))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:in-theory
               (#%plain-lambda {} '#:disable f))))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:in-theory
               (#%plain-lambda {} '#:enable f))))])
      #:proof
      `[(,DEFUN F (X) X)
        (,IN-THEORY
          (,DISABLE F))
        (,IN-THEORY
          (,ENABLE F))])

    (test-dracula #:name 'include-book
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:include-book "arithmetic-5/top"
               (#%plain-lambda {} '#:dir '#:system))))])
      #:proof
      `[(,INCLUDE-BOOK "arithmetic-5/top" #:DIR #:SYSTEM)])

    (test-dracula #:name 'fully-expanded-theorem-definitions
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {claim}
           (#%plain-lambda {x}
             '#:theorem
             '#:no-rules
             '#:no-goals
             (if x '#true '#true)))])
      #:proof
      `[(,DEFTHM CLAIM
          (,IF X
            (,QUOTE ,T)
            (,QUOTE ,T)))])

    (test-dracula #:name 'theorem-with-rule-classes
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {f}
           (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals x))
         (define-values {minimal}
           (#%plain-lambda {x}
             '#:theorem
             (#%plain-lambda {} '#:rules
               (#%plain-lambda {} '#:rewrite '#:no-corollary)
               (#%plain-lambda {} '#:forward-chaining '#:no-corollary)
               (#%plain-lambda {} '#:elim '#:no-corollary)
               (#%plain-lambda {} '#:type-prescription '#:no-corollary)
               (#%plain-lambda {} '#:linear '#:no-corollary)
               (#%plain-lambda {} '#:definition '#:no-corollary '#:no-clique)
               (#%plain-lambda {} '#:induction
                 '#:no-corollary
                 (#%plain-lambda {} '#:pattern (f x))
                 '#:no-condition
                 (#%plain-lambda {} '#:scheme (f (if x #false #true)))))
             '#:no-goals
             x))
         (define-values {maximal}
           (#%plain-lambda {x}
             '#:theorem
             (#%plain-lambda {} '#:rules
               (#%plain-lambda {} '#:rewrite
                 (#%plain-lambda {} '#:corollary (f "rewrite")))
               (#%plain-lambda {} '#:forward-chaining
                 (#%plain-lambda {} '#:corollary (f "forward chaining")))
               (#%plain-lambda {} '#:elim
                 (#%plain-lambda {} '#:corollary (f "elimination")))
               (#%plain-lambda {} '#:type-prescription
                 (#%plain-lambda {} '#:corollary (f "type prescription")))
               (#%plain-lambda {} '#:linear
                 (#%plain-lambda {} '#:corollary (f "linear")))
               (#%plain-lambda {} '#:definition
                 (#%plain-lambda {} '#:corollary (f "definition"))
                 (#%plain-lambda {} '#:clique f))
               (#%plain-lambda {} '#:definition
                 (#%plain-lambda {} '#:corollary (f "alternate definition"))
                 (#%plain-lambda {} '#:controllers (f #true)))
               (#%plain-lambda {} '#:induction
                 (#%plain-lambda {} '#:corollary (f "induction"))
                 (#%plain-lambda {} '#:pattern (f x))
                 (#%plain-lambda {} '#:condition (if x #false #true))
                 (#%plain-lambda {} '#:scheme (f (if x #false #true)))))
             '#:no-goals
             x))])
      #:proof
      `[(,DEFUN F (X) X)
        (,DEFTHM MINIMAL X
          #:RULE-CLASSES
          ((#:REWRITE)
           (#:FORWARD-CHAINING)
           (#:ELIM)
           (#:TYPE-PRESCRIPTION)
           (#:LINEAR)
           (#:DEFINITION)
           (#:INDUCTION
             #:PATTERN (F X)
             #:SCHEME (F (,IF X
                           (,QUOTE ,NIL)
                           (,QUOTE ,T))))))
        (,DEFTHM MAXIMAL X
          #:RULE-CLASSES
          ((#:REWRITE
             #:COROLLARY (F (,QUOTE "rewrite")))
           (#:FORWARD-CHAINING
             #:COROLLARY (F (,QUOTE "forward chaining")))
           (#:ELIM
             #:COROLLARY (F (,QUOTE "elimination")))
           (#:TYPE-PRESCRIPTION
             #:COROLLARY (F (,QUOTE "type prescription")))
           (#:LINEAR
             #:COROLLARY (F (,QUOTE "linear")))
           (#:DEFINITION
             #:COROLLARY (F (,QUOTE "definition"))
             #:CLIQUE (F))
           (#:DEFINITION
             #:COROLLARY (F (,QUOTE "alternate definition"))
             #:CLIQUE (F)
             #:CONTROLLER-ALIST ((F ,T)))
           (#:INDUCTION
             #:COROLLARY (F (,QUOTE "induction"))
             #:PATTERN (F X)
             #:CONDITION (,IF X
                           (,QUOTE ,NIL)
                           (,QUOTE ,T))
             #:SCHEME (F (,IF X
                           (,QUOTE ,NIL)
                           (,QUOTE ,T))))))])

    (test-dracula #:name 'theorem-with-hints
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {claim}
           (#%plain-lambda {x}
             '#:theorem
             '#:no-rules
             (#%plain-lambda {} '#:goals
               (#%plain-lambda {} '#:goal '"Goal"
                 (#%plain-lambda {} '#:by)
                 (#%plain-lambda {} '#:use (lemma x))
                 (#%plain-lambda {} '#:in-theory
                   (#%plain-lambda {} '#:disable lemma))
                 (#%plain-lambda {} '#:induct (if x #false #true))))
             (if x '#true '#true)))
         (define-values {lemma}
           (#%plain-lambda {x}
             '#:theorem
             '#:no-rules
             '#:no-goals
             (if x x '#false)))])
      #:proof
      `[(,DEFTHM LEMMA
          (,IF X
            X
            (,QUOTE ,NIL)))
        (,DEFTHM CLAIM
          (,IF X
            (,QUOTE ,T)
            (,QUOTE ,T))
          #:HINTS
          (("Goal"
            #:BY ,NIL
            #:USE ((#:INSTANCE LEMMA (X X)))
            #:IN-THEORY (,DISABLE LEMMA)
            #:INDUCT (,IF X
                       (,QUOTE ,NIL)
                       (,QUOTE ,T)))))])

    (test-suite "arity errors"
      (test-dracula/syntax-error #:name '1-vs-2
        #:lang 'dracula/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-values {f}
             (#%plain-lambda {x}
               '#:function
               '#:no-measure
               '#:no-goals
               x))
           (#%plain-app f '1 '2)])
        #:error-message '[#px"argument|arity"])
      (test-dracula/syntax-error #:name '2-vs-1
        #:lang 'dracula/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-values {f}
             (#%plain-lambda {x y}
               '#:function
               '#:no-measure
               '#:no-goals
               (if x y '#false)))
           (#%plain-app f '1)])
        #:error-message '[#px"argument|arity"]))

    (test-suite "value vs. function errors"
      (test-dracula/syntax-error #:name 'value-as-function
        #:lang 'dracula/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-values {f}
             (#%plain-lambda {x}
               '#:function
               '#:no-measure
               '#:no-goals
               (#%plain-app x)))])
        #:error-message '[#px"expected a function"])
      (test-dracula/syntax-error #:name 'function-as-value
        #:lang 'dracula/kernel
        #:program
        (quote-syntax/preserve-expensive-metadata
          [(define-values {f}
             (#%plain-lambda {x}
               '#:function
               '#:no-measure
               '#:no-goals
               f))])
        #:error-message '[#px"expected a value"]))

    (test-dracula #:name 'unquoted-literals
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {one}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals 1))
         (define-values {false}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals #false))
         (#%provide one false)])
      #:exports '[one false]
      #:checks
      (lambda (one false)
        (check-equal? (one) 1)
        (check-equal? (false) #false))
      #:proof
      `[(,DEFUN ONE () (,QUOTE 1))
        (,DEFUN FALSE () (,QUOTE ,NIL))])

    (test-dracula/syntax-error #:name 'unquoted-keywords
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-syntaxes {bad}
           (lambda (stx) #'#:bad))
         (if (bad) #true #false)]))

    (test-dracula #:name 'function-application-without-call-form
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {f}
           (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals x))
         (define-values {y}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (f 1)))
         (#%provide f y)])
      #:exports '(f y)
      #:checks (lambda (f y) (check-equal? (y) 1))
      #:proof
      `[(,DEFUN F (X) X)
        (,DEFUN Y () (F (,QUOTE 1)))])

    (test-dracula #:name 'primitives
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(#%require (only racket/base cons car cdr pair?))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:primitive cons '2 'ACL2 'CONS)))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:primitive car '1 'ACL2 'CAR)))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:primitive cdr '1 'ACL2 'CDR)))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:primitive pair? '1 'ACL2 'CONSP)))
         (define-values {mirror}
           (#%plain-lambda {x} '#:function '#:no-measure '#:no-goals
             (if (pair? x)
               (cons (mirror (cdr x)) (mirror (car x)))
               x)))
         (#%provide mirror)])
      #:exports '[mirror]
      #:checks
      (lambda (mirror)
        (check-equal? (mirror '((1 . 2) . (3 . (4 . 5))))
          '(((5 . 4) . 3) . (2 . 1))))
      #:proof
      `[(,ASSERT-EVENT
          (,IF
            (,QUOTE ,T)
            (,QUOTE ,T)
            (,CONS
              (,QUOTE 1)
              (,QUOTE 2))))
        (,ASSERT-EVENT
          (,IF
            (,QUOTE ,T)
            (,QUOTE ,T)
            (,CAR
              (,QUOTE 1))))
        (,ASSERT-EVENT
          (,IF
            (,QUOTE ,T)
            (,QUOTE ,T)
            (,CDR
              (,QUOTE 1))))
        (,ASSERT-EVENT
          (,IF
            (,QUOTE ,T)
            (,QUOTE ,T)
            (,CONSP
              (,QUOTE 1))))
        (,DEFUN MIRROR (X)
          (,IF (,CONSP X)
            (,CONS
              (MIRROR (,CDR X))
              (MIRROR (,CAR X)))
            X))])

    (test-dracula/expansion/runtime-error #:name 'improper-empty?
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(#%require racket/list)
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:primitive first '1 'ACL2 'FIRST)))
         (first '5)])
      #:proof
      `[(,ASSERT-EVENT
          (,IF
            (,QUOTE ,T)
            (,QUOTE ,T)
            (,FIRST
              (,QUOTE 1))))]
      #:error-message '[#px"5" #px"first" #px"list"])

    (test-dracula #:name 'reordered-value-definitions
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {x}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (y)))
         (define-values {y}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (z)))
         (define-values {z}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals 'same))
         (#%provide x y z)])
      #:exports '(x y z)
      #:checks
      (lambda (x y z)
        (check-equal? (x) 'same)
        (check-equal? (y) 'same)
        (check-equal? (z) 'same))
      #:proof
      `[(,DEFUN Z () (,QUOTE same))
        (,DEFUN Y () (Z))
        (,DEFUN X () (Y))])

    (test-dracula #:name 'nested-begin
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(begin
           (begin
             (define-values {x}
               (#%plain-lambda {} '#:function '#:no-measure '#:no-goals '1))
             (define-values {y}
               (#%plain-lambda {} '#:function '#:no-measure '#:no-goals '2)))
           (#%provide x y))])
      #:exports '(x y)
      #:checks
      (lambda (x y)
        (check-equal? (x) 1)
        (check-equal? (y) 2))
      #:proof
      `[(,DEFUN X () (,QUOTE 1))
        (,DEFUN Y () (,QUOTE 2))])

    (test-dracula/syntax-error #:name 'mutually-recursive-functions
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-values {f}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app g)))
         (define-values {g}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (#%plain-app f)))])
      #:error-message mutual-recursion-regexps)

    (test-dracula #:name 'expression-macro
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-syntaxes {or}
           (syntax-parser
             [(_) #''#false]
             [(_ e) #'e]
             [(_ e . es) #'(let-values {[(x) e]} (if x x (or . es)))]))
         (define-values {x}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals
             (or '#false '5 '#true)))
         (#%provide x)])
      #:exports '(x)
      #:checks (lambda (x) (check-equal? (x) 5))
      #:proof
      `[(,DEFUN X ()
          (,LET ((X.2 (,QUOTE ,NIL)))
            (,IF X.2
              X.2
              (,LET ((X.3 (,QUOTE 5)))
                (,IF X.3
                  X.3
                  (,QUOTE ,T))))))])

    (test-dracula #:name 'definition-macro
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-syntaxes {define}
           (syntax-parser
             [(_ x:id e:expr)
              #'(define-values {x}
                  (#%plain-lambda {}
                    '#:function '#:no-measure '#:no-goals
                    e))]
             [(_ (f:id x:id ...) e:expr)
              #'(define-values {f}
                  (#%plain-lambda {x ...}
                    '#:function '#:no-measure '#:no-goals
                    e))]))
         (define (f x y) (if x y '#false))
         (define x '7)
         (#%provide f x)])
      #:exports '(f x)
      #:checks
      (lambda (f x)
        (check-equal? (x) 7)
        (check-equal? (f #false 5) #false)
        (check-equal? (f #true 5) 5))
      #:proof
      `[(,DEFUN F (X Y)
          (,IF X Y (,QUOTE ,NIL)))
        (,DEFUN X () (,QUOTE 7))])

    (test-dracula #:name 'higher-order-macro
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-syntaxes {define-associative}
           (syntax-parser
             [(_ op:id binop:id zero:expr)
              #'(define-syntaxes {op}
                  (syntax-parser
                    [(_) #'zero]
                    [(_ e:expr . es:expr)
                     #'(binop e (op . es))]))]))
         (#%require (rename racket/base ++ +))
         (define-values {}
           (begin0 (#%plain-app values)
             (#%plain-lambda {} '#:primitive ++ '2 'ACL2 'BINARY-+)))
         (define-associative + ++ 0)
         (define-values {six}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals (+ 1 2 3)))
         (#%provide six)])
      #:exports '(six)
      #:checks
      (lambda (six)
        (check-equal? (six) 6))
      #:proof
      `[(,ASSERT-EVENT
          (,IF
            (,QUOTE ,T)
            (,QUOTE ,T)
            (,BINARY-+
              (,QUOTE 1)
              (,QUOTE 2))))
        (,DEFUN SIX ()
          (,BINARY-+ (,QUOTE 1)
            (,BINARY-+ (,QUOTE 2)
              (,BINARY-+ (,QUOTE 3)
                (,QUOTE 0)))))])

    (test-dracula #:name 'macro-with-literals
      #:lang 'dracula/kernel
      #:program
      (quote-syntax/preserve-expensive-metadata
        [(define-syntaxes {same}
           (lambda (stx)
             (define arg (cadr (syntax->list stx)))
             #`(quote #,(free-identifier=? arg #'same))))
         (same same)
         (same diff)
         (#%provide same)])
      #:results '(#true #false))))
