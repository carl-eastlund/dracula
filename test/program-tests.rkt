#lang racket

(require syntax/toplevel
         mred/mred
         "../private/planet.rkt")
(require rackunit
         (cce main))

(define (top-level->module lang-path terms)
  (let* ([name (gensym 'program)])
    (list
     (datum->syntax
      #'here
      `(module ,name ,lang-path
         ,(datum->syntax #f `(#%module-begin ,@terms))))
     (datum->syntax
      #'here `(require ',name))
     (datum->syntax
      #'here `(current-namespace (module->namespace '',name))))))

;; A Program is (make-prog Namespace (Listof Term) (Promise ProgramResult))
;; A Term is any eval-able value (Syntax or Sexp or CompiledExpr)
(define-struct prog (make-ns terms results))

;; A ProgramResult is a (Listof TermResult)
;; A TermResult is (make-result Term ExpandResult EvalResult)
;; An ExpandResult is one of:
;; - Syntax, representing successful expansion
;; - Exn, representing a compile-time error
;; - #f, representing no compilation due to prior error
;; An EvalResult is one of:
;; - (Listof Any), representing all values returned
;; - Exn, representing a run-time error
;; - #f, representing no execution due to prior error
(define-struct result (term expand eval))

;; A Summary is one of:
;; - (make-success (Listof Syntax) (Listof Any))
;; - (make-compile-error Term Exn)
;; - (make-runtime-error Term Exn)
(define-struct summary ())
(define-struct (success summary) (expansion evaluation))
(define-struct (expand-failure summary) (term exn))
(define-struct (eval-failure summary) (term exn))

(provide/contract
 [top-level->module (-> any/c list? (listof syntax?))]
 [make-program (-> list? prog?)]
 [make-program/mred (-> list? prog?)]
 [make-module-program (->* [any/c list?] [list?] prog?)]
 [make-module-program/mred (->* [any/c list?] [list?] prog?)]
 [make-program/namespace (-> (-> namespace?) list? prog?)]
 [check-expand-failure (-> prog? exn?)]
 [check-expand-success (-> prog? (listof syntax?))]
 [check-eval-failure (-> prog? exn?)]
 [check-eval-success (-> prog? any)])

;; check-expand-failure : Program -> Exception
;; Produces a compile-time exception raised by the program,
;; or raises a SchemeUnit failure if none.
(define (check-expand-failure program)
  (let* ([summary (summarize (force (prog-results program)))])
    (if (expand-failure? summary)
        (expand-failure-exn summary)
        (summary->failure summary))))

;; check-eval-failure : Program -> Exception
;; Produces a compile-time exception raised by the program,
;; or raises a SchemeUnit failure if none.
(define (check-eval-failure program)
  (let* ([summary (summarize (force (prog-results program)))])
    (if (eval-failure? summary)
        (eval-failure-exn summary)
        (summary->failure summary))))

;; check-expand-success : Program -> (Listof Syntax)
;; Produces the expanded form(s) of the program, or
;; raises a SchemeUnit failure if none.
(define (check-expand-success program)
  (let* ([summary (summarize (force (prog-results program)))])
    (if (success? summary)
        (success-expansion summary)
        (summary->failure summary))))

;; check-eval-success : Program -> (Listof Syntax)
;; Produces the evaled form(s) of the program, or
;; raises a SchemeUnit failure if none.
(define (check-eval-success program)
  (let* ([summary (summarize (force (prog-results program)))])
    (if (success? summary)
        (apply values (success-evaluation summary))
        (summary->failure summary))))

;; summary->failure : Summary -> <SchemeUnit-failure>
;; Raises a SchemeUnit failure appropriate to the summary type.
(define (summary->failure summary)
  (cond
   [(success? summary)
    (fail (format "The program succeeded with values: ~s"
                  (success-evaluation summary)))]
   [(eval-failure? summary)
    (fail (format "Term ~s failed to eval with error ~s"
                  (datum (eval-failure-term summary))
                  (eval-failure-exn summary)))]
   [(expand-failure? summary)
    (fail (format "Term ~s failed to expand with error ~s"
                  (datum (expand-failure-term summary))
                  (expand-failure-exn summary)))]))

(define (datum v) (syntax->datum (datum->syntax #f v)))

;; summarize : ProgramResults -> Summary
;; Generates a summary of program completion.
(define (summarize results)
  (cond
   [(findf (lambda (res) (exn? (result-expand res))) results) =>
    (lambda (res)
      (make-expand-failure (result-term res) (result-expand res)))]
   [(findf (lambda (res) (exn? (result-eval res))) results) =>
    (lambda (res)
      (make-eval-failure (result-term res) (result-eval res)))]
   [else (make-success (map result-expand results)
                       (result-eval (last results)))]))

;; last : (NEListof Any) -> Any
;; Produces the last element of a list.
(define (last l)
  (cond
   [(null? l) (error 'last "got an empty list")]
   [(null? (cdr l)) (car l)]
   [else (last (cdr l))]))

;; make-program/namespace : (-> Namespace) (Listof Term) -> Program
;; Produces a program that evaluates each term in the generated namespace.
(define (make-program/namespace generate-namespace terms)
  (make-prog generate-namespace
             terms
             (delay (execute generate-namespace terms))))

;; make-program : (Listof Term) -> Program
;; Produces a program in the default namespace.
(define (make-program terms)
  (make-program/namespace (lambda () (make-base-namespace)) terms))

;; make-module-program : RequireSpec (Listof Term) [Listof Term] -> Program
;; Constructs a program from a path to a language, the body of a module
;; in that language, and optional terms to evaluate in the context of
;; that module.
(define make-module-program
  (lambda (path body [rest null])
    (make-program (append (top-level->module path body) rest))))

;; make-program/mred : (Listof Term) -> Program
;; Produces a program in the default namespace, with MrEd attached.
(define (make-program/mred terms)
  (make-program/namespace
   (lambda () (make-gui-namespace))
   terms))

;; make-module-program/mred : RequireSpec (Listof Term) [Listof Term] -> Program
;; Constructs a program from a path to a language, the body of a module
;; in that language, and optional terms to evaluate in the context of
;; that module; attaches MrEd.
(define make-module-program/mred
  (lambda (path body [rest null])
    (make-program/mred (append (top-level->module path body) rest))))

;; execute : (-> Namespace) (Listof Term) -> ProgramResults
;; Execute terms in a given namespace and report their results.
(define (execute gen-ns terms)
  (parameterize ([current-namespace (gen-ns)]
                 [current-custodian (make-custodian)])
    (begin0
      (execute-terms terms)
      (custodian-shutdown-all
       (current-custodian)))))

;; execute-terms : (Listof Term) -> ProgramResult
;; Executes each term in order.
(define (execute-terms terms)
  (if (null? terms)
      null
      (execute-term (car terms) (cdr terms))))

;; execute-term : Term (Listof Term) -> ProgramResult
;; Produces the result of executing the current term followed by the
;; remaining terms (if applicable).
(define (execute-term term rest)
  (let* ([stx (expand-term term)]
         [vals (and (syntax? stx) (eval-term stx))])
    (cons (make-result term stx vals)
          (if (list? vals)
              (execute-terms rest)
              (map skip-term rest)))))

;; expand-term : Term -> ExpandResult
;; Produces the result of expanding the term.
(define (expand-term term)
  (call-with-continuation-barrier
   (lambda ()
     (with-handlers ([exn? identity]
                     [any? (non-exception-failure 'expand term)])
       (expand term)))))

;; eval-term : Syntax -> EvalResult
;; Produces the result of evaluating the term.
(define (eval-term stx)
  (call-with-continuation-barrier
   (lambda ()
     (with-handlers ([exn? identity]
                     [any? (non-exception-failure
                            'eval (syntax->datum stx))])
       (call-with-values (lambda () (eval-syntax stx)) list)))))

;; skip-term : Term -> TermResult
;; Produces a result indicating a skipped term.
(define (skip-term term)
  (make-result term #f #f))

;; any? : Any -> Boolean
;; Always returns true.
(define any? (const #t))

;; non-exception-failure : Symbol Term -> Any -> <SchemeUnit-failure>
;; Raises a SchemeUnit fail-check exception describing a non-exception
;; value being raised during a given operation on a term.
(define ((non-exception-failure op term) non-exn)
  (fail (format "Attempting to ~a term ~s raised non-exception value ~s."
                op term non-exn)))

