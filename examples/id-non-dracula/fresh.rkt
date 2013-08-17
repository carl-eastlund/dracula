#lang racket
(begin-for-syntax
  (require racket/syntax)
  (define (fresh)
    #;(generate-temporary)
    #;(gensym)
    (begin
      (define id0 (datum->syntax #false 'fresh))
      (define ctx (syntax-local-make-definition-context))
      (syntax-local-bind-syntaxes (list id0) #false ctx)
      (internal-definition-context-seal ctx)
      (internal-definition-context-apply ctx id0)))
  (provide fresh))
