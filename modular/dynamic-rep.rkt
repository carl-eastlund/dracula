#lang racket/base
(require racket/contract
         "../private/hash.rkt")

;; Run-time representation of interfaces/modules.

;; A Dynamic Module (DMod) is:
;;   (make-module/dynamic (-> DIfc DIfc))
;; A Dynamic Interface (DIfc) is:
;;  (make-interface-dynamic (Hashof Symbol Procedure))
(define-struct module/dynamic (implementation) #:prefab)
(define-struct interface/dynamic (functions) #:prefab)

(define (empty-interface/dynamic)
  (make-interface/dynamic (make-hasheq)))

(define (interface/dynamic-put-function! i/d sym fn)
  (hash-set! (interface/dynamic-functions i/d) sym fn))

(define (interface/dynamic-get-function i/d sym)
  (hash-ref/check (interface/dynamic-functions i/d) sym))

(define (interface/dynamic-join one two)
  (let* ([hash (make-hasheq)])
    (for ([(k v) (interface/dynamic-functions one)])
      (hash-set! hash k v))
    (for ([(k v) (interface/dynamic-functions two)])
      (if (hash-contains? hash k)
          (unless (eq? (hash-ref/check hash k) v)
            (error 'interface/dynamic-join "inconsistent values for ~s" k))
          (hash-set! hash k v)))
    (make-interface/dynamic hash)))

(provide/contract

 [module/dynamic? (-> any/c boolean?)]
 [make-module/dynamic
  (-> (-> interface/dynamic? interface/dynamic?) module/dynamic?)]
 [module/dynamic-implementation
  (-> module/dynamic? (-> interface/dynamic? interface/dynamic?))]

 [interface/dynamic? (-> any/c boolean?)]
 [empty-interface/dynamic (-> interface/dynamic?)]
 [interface/dynamic-join
  (-> interface/dynamic? interface/dynamic? interface/dynamic?)]
 [interface/dynamic-get-function
  (-> interface/dynamic? symbol? procedure?)]
 [interface/dynamic-put-function!
  (-> interface/dynamic? symbol? procedure? void?)])
