#lang mischief

(provide
  check-program!/residual
  lookup-proof-obligation
  all-proof-obligations
  all-modules-with-proofs
  minimal-modules-with-proofs
  proof-dependencies)

(require
  dracula/model/data
  dracula/model/parse
  dracula/model/quote
  dracula/model/verify
  dracula/model/acl2
  dracula/expansion/simplify
  mischief/preserve-expensive-metadata)

(define (all-proof-obligations)
  (for/hash {[{mod ob} (in-hash module~>obligations)]}
    (values mod (proof->acl2 ob))))

(define (all-modules-with-proofs)
  (remove-duplicates
    (queue->list module-queue)))

(define (minimal-modules-with-proofs)
  (minimize-dependencies (all-modules-with-proofs)))

(define (lookup-proof-obligation mod)
  (proof->acl2
    (lookup mod module~>obligations)))

(define (check-program!/residual stxs)
  (define deps (all-modules-with-proofs))
  (define-values {prelude-obs}
    (prelude-obligations deps))
  (define-values {env0 shapes0 prelude-csqs}
    (prelude-consequences deps))
  (define-values {env shapes obligations consequences}
    (verify-program env0 shapes0
      (parse-program
        (map simplify-local-expansion stxs))))
  (define proof (append prelude-obs prelude-csqs obligations))
  #`(begin
      (define ref (#%variable-reference))
      (define rmp (variable-reference->resolved-module-path ref))
      (define mod (resolved-module-path-name rmp))
      (define var (quote-syntax/preserve-expensive-metadata var))
      (define dep #,(quote-dependencies deps))
      (define shs #,(quote-shape-map (shape-map-subtract shapes shapes0)))
      (define env #,(quote-environment (environment-subtract env env0)))
      (define obs #,(quote-proof proof))
      (define csq #,(quote-proof consequences))
      (enqueue! module-queue mod)
      (dict-set! module~>identifier mod var)
      (dict-set! module~>dependencies mod dep)
      (dict-set! module~>shape-map mod shs)
      (dict-set! module~>environment mod env)
      (dict-set! module~>obligations mod obs)
      (dict-set! module~>consequences mod csq)))

(define (quote-dependencies deps)
  #`(list #,@(map quote-dependency deps)))

(define (quote-dependency dep)
  (define/syntax-parse var
    (lookup dep module~>identifier))
  #'(resolved-module-path-name
      (variable-reference->resolved-module-path
        (#%variable-reference var))))

(define (prelude-obligations mods)
  (append-map verify-dependency
    (minimize-dependencies mods)))

(define (minimize-dependencies mods)
  (define redundant
    (for*/set {[mod (in-list mods)]
               [dep (in-list (lookup mod module~>dependencies))]}
      dep))
  (for/list {[mod (in-list mods)]
             #:unless (set-member? redundant mod)}
    mod))

(define (prelude-consequences mods)
  (define/for/fold/append-lists
      {[env0 (empty-environment)]
       [shape0 (empty-shape-map)]}
      {all-csqs}
      {[mod (in-list mods)]}
    (define env (lookup mod module~>environment))
    (define shapes (lookup mod module~>shape-map))
    (define csqs (lookup mod module~>consequences))
    (values
      (environment-union env0 env)
      (shape-map-union shape0 shapes)
      csqs))
  (values env0 shape0 all-csqs))

(define (proof-dependencies)
  (for/hash {[{mod deps} (in-hash module~>dependencies)]}
    (values mod deps)))

(define (lookup mod dict)
  (dict-ref dict mod
    (lambda {}
      (error 'Dracula "no record for module ~a" mod))))

(define module~>identifier (make-hash))
(define module~>dependencies (make-hash))
(define module~>shape-map (make-hash))
(define module~>environment (make-hash))
(define module~>obligations (make-hash))
(define module~>consequences (make-hash))
(define module-queue (make-queue))
