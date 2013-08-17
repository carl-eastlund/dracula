#lang mischief

(provide
  (contract-out
    [check-program!/residual (-> (listof syntax?) syntax?)]
    [lookup-proof-obligation (-> module-name? acl2/c)]
    [all-proof-obligations (-> (dict/c module-name? acl2/c))]
    [all-modules-with-proofs (-> (listof module-name?))]))

(require
  dracula/legacy/check
  dracula/legacy/type
  dracula/legacy/proof
  dracula/legacy/registry
  dracula/expansion/names
  dracula/proof/term
  dracula/expansion/paths
  macro-debugger/emit
  (for-template
    mischief))

(define (all-proof-obligations)
  (for/hash {[{mod pf} (in-dict module-name~>obligations)]}
    (values mod pf)))

(define (lookup-proof-obligation name)
  (dict-ref module-name~>obligations name
    (lambda ()
      (error 'lookup-proof-obligation
        "no proof obligation for module ~a"
        name))))

(define (all-modules-with-proofs)
  (queue->list dependency-queue))

(define (check-program!/residual stxs)
  (check-program! stxs)
  (emit-remark "Dracula syntax check succeeded.")
  (define obs (program-obligations stxs))
  (define/syntax-parse [ob ...] obs)
  (emit-remark "Dracula proof obligation" #'(ob ...))
  (define cseqs
    (map quote-transformer
      (program-consequences stxs)))
  (define/syntax-parse [cseq ...] cseqs)
  (emit-remark "Dracula proof consequences" #'(cseq ...))
  (define/syntax-parse {[x type] ...}
    (for/list {[x-id (in-list (program-names stxs))]
               #:when (dict-has-key? identifier~>type x-id)}
      (list x-id
        (type->expr
          (dict-ref identifier~>type x-id impossible)))))
  (emit-remark "Dracula type annotations" #'([x type] ...))
  #'(lambda {name}
      (enqueue! dependency-queue name)
      (dict-set! module-name~>obligations name '[ob ...])
      (dict-set! module-name~>consequences name
        (list cseq ...))
      (dict-set! identifier~>type (quote-syntax x) type)
      ...))
