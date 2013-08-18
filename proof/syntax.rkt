#lang racket

(require "proof.rkt")

(define key 'dracula)

;; annotate : ? Syntax -> Syntax
;; Stores a Dracula annotation with the given syntax.
(define (annotate v stx)
  (syntax-property stx key v))

;; extract : Syntax -> ?
;; Extracts a Dracula annotation from the given syntax
(define (extract stx)
  (syntax-property stx key))

;; get : Any -> List
;; Get all Dracula annotations stored with a value.
(define (get v)
  (cond
   [(and (syntax? v) (extract v)) => list]
   [(syntax? v) (get (syntax-e v))]
   [(pair? v) (append (get (car v)) (get (cdr v)))]
   [else null]))

;; get-proof : Syntax -> Proof
;; Gets the Dracula proof stored in a value.
(define (get-proof stx)
  (match (filter proof? (get stx))
    [(list pf) pf]
    [v
     (error 'get-proof
            "expected (list Proof); got:\n~s\nfrom:\n~s"
            v (syntax->datum stx))]))

(provide/contract
 [rename annotate annotate-term (-> term? syntax? syntax?)]
 [rename annotate annotate-part (-> part? syntax? syntax?)]
 [rename annotate annotate-proof (-> proof? syntax? syntax?)]
 [rename get get-terms (-> syntax? (listof term?))]
 [rename get get-parts (-> syntax? (listof part?))]
 [get-proof (-> syntax? proof?)])
