#lang mischief

(module reader syntax/module-reader dracula/kernel)

(provide

  ;; Phase 0
  (rename-out
    [#%top-interaction #%top-interaction]
    [#%dracula-module-begin #%module-begin]
    [#%plain-app #%app]
    [#%datum #%datum]
    [#%top #%top])
  #%require
  #%provide
  #%plain-lambda
  #%plain-app
  quote-syntax
  begin
  begin0
  quote
  if
  let-values
  letrec-values
  letrec-syntaxes+values
  values
  begin-for-syntax
  define-values
  define-syntaxes
  define-syntax
  define-for-syntax
  define-values-for-syntax
  let-syntax
  let-syntaxes
  require
  provide
  only-in
  except-in
  prefix-in
  rename-in
  combine-in
  only-meta-in
  all-defined-out
  all-from-out
  rename-out
  except-out
  prefix-out
  struct-out
  combine-out
  protect-out
  for-meta
  for-syntax
  for-template
  for-label
  lib
  file
  planet
  submod
  module
  module*
  require/provide)

(require/provide
  dracula/expansion/runtime
  (for-syntax
    mischief))

(require
  (for-syntax
    dracula/expansion/simplify
    dracula/model
    macro-debugger/emit))

(begin-for-syntax
  (define (expand-module-begin stx0)
    (syntax-parse stx0
      [(form:id term:expr ...)
       (define mod
         (resolved-module-path-name
           (variable-reference->resolved-module-path
             (syntax-local-variable-reference))))
       (define stx1 #'(#%plain-module-begin term ...))
       (emit-remark "unexpanded Dracula -> unexpanded Racket" stx0 stx1)
       (define stx2 (expand-in-scope stx1))
       (emit-remark "unexpanded Racket -> expanded Racket" stx1 stx2)
       (define stx3 (simplify-local-expansion stx2))
       (emit-remark "expanded Racket -> simplified Racket" stx2 stx3)
       (syntax-parse stx3
         #:literal-sets {kernel-literals}
         [(#%plain-module-begin defn ...)
          (define/syntax-parse residual
            (check-program!/residual (@ defn)))
          (syntax-parse stx2
            #:literal-sets {kernel-literals}
            [(#%plain-module-begin defn0 ...)
             (define stx4
               #'(#%module-begin
                   defn0 ...
                   (begin-for-syntax
                     residual)))
             (emit-remark "expanded Racket -> annotated Racket" stx3 stx4)
             stx4])])])))

(define-syntax #%dracula-module-begin
  expand-module-begin)
