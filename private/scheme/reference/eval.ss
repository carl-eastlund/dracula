#lang scheme

(require scheme/path scheme/runtime-path scheme/sandbox "../sandbox.ss")

(define-runtime-path long-main.ss "../main.ss")

(define main.ss (simplify-path long-main.ss))

(define shared-specs
  (make-sandbox-namespace-specs make-base-namespace 'scheme main.ss))

(define (evaluator)
  (parameterize ([sandbox-namespace-specs shared-specs])
    (let* ([ev (make-scribble-evaluator 'scheme #:requires (list main.ss))]
           [path (find-relative-path (current-directory) main.ss)])
      (ev `(require (for-syntax ,(path->string path))))
      ev)))

(provide evaluator)
