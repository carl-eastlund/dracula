#lang racket

(require "../acl2/rep.rkt"
         "../private/hash.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Loc is (make-loc Any Nat Nat)
;; Source represents the source location of the term that a Loc corresponds to.
;; Start and End represent the location of the term in the source file.
(define-struct loc (source start end) #:prefab)

;; A Term is (make-term SExp Loc)
;; Sexp is the term in S-Expression format.
;; Loc contains information on where the term came from.
(define-struct term (sexp loc) #:prefab)

;; A Part is (make-part Symbol [Vector Term] Loc
;; A Part represents a part of a Proof (e.g. a module in Modular ACL2).
;; Name is the name of the part (usually a module name).
;; Terms is a vector of all terms found in the part.
;; Loc contains information on where the part came from.
(define-struct part (name terms loc) #:prefab)

;; A Proof is (make-proof Nat [Listof Symbol] [Vector Part] [Hash Symbol Part])
;; Size represents the number of parts in the Proof.
;; Parts lists the names of all the parts in the proof.
;; By-number is a vector that indexes each part in order.
;; By-name is a hash table that indexes each part by name.
(define-struct proof (size parts by-number by-name) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; build-part : Symbol Loc (Listof Term) -> Part
;; Construct a part.
(define (build-part name loc terms)
  (make-part name (list->vector terms) loc))

;; syntax->loc : Syntax -> Loc
;; Generates a Loc containing the source location of the given syntax,
;; and its starting and ending positions in the source.
(define (syntax->loc stx)
  (let* ([source (syntax-source stx)]
         [pos (syntax-position stx)]
         [span (syntax-span stx)]
         [start (and pos (sub1 pos))]
         [end (and start span (+ start span))])
    (make-loc source start end)))

;; syntax->term : Syntax -> Term
;; Generates a term from the given syntax.
(define (syntax->term stx)
  (make-term (syntax->datum stx) (syntax->loc stx)))

;; syntax->part : Syntax [:name Symbol] -> Part
;; Generates a part from the given syntax, containing the given name
;; (if one is given; otherwise 'Proof is used), a vector of all terms
;; found in the syntax, and the location information of the syntax.
(define (syntax->part stx #:name [name 'Proof])
  (make-part name
             (apply vector-immutable
                    (map syntax->term (syntax->list stx)))
             (syntax->loc stx)))

;; build-proof : Part ... -> Proof
;; Generates a Proof from the given parts.
(define (build-proof . parts)
  (let* ([by-number (apply vector-immutable parts)]
         [size (vector-length by-number)]
         [names (map part-name parts)]
         [by-name (make-immutable-hasheq (map cons names parts))])
    (make-proof size names by-number by-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; part-length : Part -> Nat
;; Determines the number of terms in the given Part.
(define (part-length part)
  (vector-length (part-terms part)))

;; part-nth : Part Nat -> Term
;; Extracts the nth term from the given part.
(define (part-nth part n)
  (vector-ref (part-terms part) n))

;; proof-part : Proof Symbol -> Part
;; Extracts the named Part from the given Proof.
(define (proof-part proof name)
  (hash-ref/check (proof-by-name proof) name))

;; proof-nth : Proof Nat -> Part
;; Extracts the nth Part from the given Proof.
(define (proof-nth proof n)
  (vector-ref (proof-by-number proof) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nat? exact-nonnegative-integer?)

(define nat/f (or/c nat? #f))

(provide/contract

 ;; Source locations:
 [loc? (-> any/c boolean?)]
 [syntax->loc (-> syntax? loc?)]
 [make-loc (-> any/c nat/f nat/f loc?)]
 [loc-source (-> loc? any/c)]
 [loc-start (-> loc? nat/f)]
 [loc-end (-> loc? nat/f)]

 ;; Terms:
 [term? (-> any/c boolean?)]
 [syntax->term (-> syntax? term?)]
 [make-term (-> sexp/c loc? term?)]
 [term-sexp (-> term? sexp/c)]
 [term-loc (-> term? loc?)]

 ;; Parts:
 [part? (-> any/c boolean?)]
 [syntax->part (->* [(and/c syntax? syntax->list)] [#:name symbol?] part?)]
 [rename build-part make-part (-> symbol? loc? (listof term?) part?)]
 [part-name (-> part? symbol?)]
 [part-loc (-> part? loc?)]
 [part-length (-> part? nat?)]
 [part-nth
  (->d ([part part?] [n (and/c nat? (</c (part-length part)))]) ()
       [_ term?])]

 ;; Proofs:
 [proof? (-> any/c boolean?)]
 [rename build-proof make-proof (->* [] [] #:rest (listof part?) proof?)]
 [proof-size (-> proof? nat?)]
 [proof-parts (-> proof? (listof symbol?))]
 [proof-part (-> proof? symbol? part?)]
 [proof-nth
  (->d ([proof proof?] [n (and/c nat? (</c (proof-size proof)))]) ()
       [_ part?])]

 )
