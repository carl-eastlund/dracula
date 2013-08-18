#lang racket

(require racket/pretty
         "../proof/proof.rkt"
         "../acl2/acl2.rkt"
         "../acl2/rep.rkt"
         "../private/hash.rkt"
         "term-state.rkt")

;; A ProofState is:
;;   (make-proof-state Loc Nat (Or Nat #f) (Hash Nat TermState))
(define-struct proof-state (loc size index terms) #:prefab)

;; ======================================================================
;;
;;  IMPLEMENTATION
;;
;; ======================================================================

;; ==================================================
;;
;;  Constructors.
;;
;; ==================================================

(define (initial-proof-state part)
  (let* ([loc0 (part-loc part)]
         [loc1 (make-loc (loc-source loc0) (loc-start loc0) (loc-start loc0))]
         [terms (make-immutable-hash null)]
         [terms (hash-set terms 0 (empty-term-state loc1))]
         [terms (for/fold
                    ([terms terms])
                    ([n (in-range (part-length part))])
                  (hash-set terms
                            (+ n 1)
                            (initial-term-state (part-nth part n))))])
    (make-proof-state loc0 (+ (part-length part) 1) #f terms)))

(define (proof-state-populate old-state new-part)
  (let* ([new-state (initial-proof-state new-part)])
    (match (list old-state new-state)
      [(list (struct proof-state [_ _ #f _]) _) new-state]
      [(list (struct proof-state [old-loc old-size old-index old-terms])
             (struct proof-state [new-loc new-size new-index new-terms]))
       (make-proof-state
        new-loc
        new-size
        old-index
        (for/fold
            ([terms new-terms])
            ([n (in-range 0 (+ old-index 1))])
          (hash-set
           terms n
           (term-state-populate (hash-ref/check old-terms n)
                                (hash-ref/check new-terms n)))))])))

(define (proof-state-start-term state acl2-state index saved)
  (make-proof-state
   (proof-state-loc state)
   (proof-state-size state)
   index
   (hash-update (proof-state-terms state)
                index
                (lambda (term) (term-state-start-acl2 term acl2-state saved)))))

(define (proof-state-start-acl2 state acl2-state)
  (proof-state-start-term state acl2-state 0 #t))

(define (proof-state-update-acl2 state acl2-state)
  (struct-copy proof-state state
               [terms
                (hash-update (proof-state-terms state)
                             (proof-state-index state)
                             (lambda (term)
                               (term-state-update-acl2 term acl2-state)))]))

(define (proof-state-advance state acl2-state saved)
  (proof-state-start-term
   state acl2-state (+ (proof-state-index state) 1) saved))

(define (proof-state-rewind state)
  (make-proof-state
   (proof-state-loc state)
   (proof-state-size state)
   (- (proof-state-index state) 1)
   (hash-update (proof-state-terms state)
                (proof-state-index state)
                (lambda (term) (term-state-stop-acl2 term)))))

(define (proof-state-edit state)
  (if (proof-state-admitted? state)
      (error 'proof-state-edit "Attempted to edit an admitted term")
      (make-proof-state
       (proof-state-loc state)
       (proof-state-size state)
       (proof-state-index state)
       (hash-update (proof-state-terms state)
                    (proof-state-index state)
                    (lambda (term) (term-state-edit term))))))

(define (proof-state-stop-acl2 state)
  (make-proof-state
   (proof-state-loc state)
   (proof-state-size state)
   #f
   (for/fold
       ([terms (proof-state-terms state)])
       ([n (in-range 0 (+ (proof-state-index state) 1))])
     (hash-update terms n term-state-stop-acl2))))

;; ==================================================
;;
;;  ACL2 text.
;;
;; ==================================================

(define (proof-state-initial-prompt state)
  (term-state-initial-prompt (proof-state-current-term state)))

(define (proof-state-acl2-input state)
  (term-state-acl2-input (proof-state-current-term state)))

(define (proof-state-acl2-output state)
  (term-state-acl2-output (proof-state-current-term state)))

(define (proof-state-final-prompt state)
  (term-state-final-prompt (proof-state-current-term state)))

(define (proof-state-proof-tree state)
  (term-state-proof-tree (proof-state-current-term state)))

(define (proof-state-total-output state)
  (let loop ([i 0])
    (let* ([term-state (hash-ref/default (proof-state-terms state) i #f)])
      (if (term-state-active? term-state)
          (string-append (term-state-total-output term-state)
                         "\n"
                         (loop (add1 i)))
          "\n"))))

;; ==================================================
;;
;;  Predicates.
;;
;; ==================================================

(define (proof-state-acl2-open? v)
  (and (proof-state? v) (integer? (proof-state-index v))))

(define (proof-state-start-of-proof? state)
  (or (= (proof-state-index state) 0)
      (and (= (proof-state-index state) 1)
           (not (proof-state-admitted? state)))))

(define (proof-state-end-of-proof? state)
  (and (= (proof-state-index state)
          (- (proof-state-size state) 1))
       (proof-state-admitted? state)))

(define (proof-state-at-first-term? state)
  (= (proof-state-index state) 0))

(define (proof-state-at-last-term? state)
  (= (proof-state-index state)
     (- (proof-state-size state) 1)))

(define (proof-state-finished? state)
  (term-state-finished? (proof-state-current-term state)))

(define (proof-state-admitted? state)
  (term-state-admitted? (proof-state-current-term state)))

(define (proof-state-edited? state)
  (term-state-edited? (proof-state-current-term state)))

;; ==================================================
;;
;;  Highlighting positions.
;;
;; ==================================================

(define (proof-state-first-admitted-position state)
  (loc-start (proof-state-loc state)))

(define (proof-state-last-admitted-position state)
  (match state
    [(struct proof-state [loc size index terms])
     (let* ([term-state (hash-ref/check terms index)])
       (if (term-state-admitted? term-state)
           (if (= index (sub1 size))
               (loc-end loc)
               (loc-end (term-state-loc term-state)))
           (if (= index 0)
               (loc-start loc)
               (loc-end
                (term-state-loc
                 (hash-ref/check terms (sub1 index)))))))]))

(define (proof-state-last-attempted-position state)
  (let* ([term-state (proof-state-current-term state)])
    (if (term-state-admitted? term-state)
        (proof-state-last-admitted-position state)
        (if (term-state-has-input? term-state)
            (loc-end (term-state-loc term-state))
            (proof-state-last-admitted-position state)))))

;; ==================================================
;;
;;  Cursor search.
;;
;; ==================================================

;; proof-state-index-before-pos : ProofState Nat -> Nat
(define (proof-state-index-before-pos state pos)
  (let loop ([index (proof-state-index state)])
    (if (or (= (add1 index) (proof-state-size state))
            (<= pos
                (loc-start
                 (term-state-loc
                  (hash-ref/check (proof-state-terms state) (add1 index))))))
        index
        (loop (add1 index)))))

;; proof-state-index-after-pos : ProofState Nat -> Nat
(define (proof-state-index-after-pos state pos)
  (let loop ([index (add1 (proof-state-index state))])
    (if (or (= index 1)
            (>= pos
                (loc-end
                 (term-state-loc
                  (hash-ref/check (proof-state-terms state) (sub1 index))))))
        index
        (loop (sub1 index)))))

(define (proof-state-next-sexp state)
  (term-state-sexp (proof-state-next-term state)))

(define (proof-state-save-before-next-sexp state)
  (term-state-save-before-sexp (proof-state-next-term state)))

(define (proof-state-restore-saved-sexp state)
  (term-state-restore-saved-sexp (proof-state-current-term state)))

;; =====================================================================
;;
;;  HELPERS
;;
;; ======================================================================

(define (proof-state-term-index state)
  (proof-state-index state))

(define (proof-state-current-term state)
  (hash-ref/check (proof-state-terms state) (proof-state-index state)))

(define (proof-state-next-term state)
  (hash-ref/check (proof-state-terms state) (add1 (proof-state-index state))))

;; ======================================================================
;;
;;  CONTRACTS
;;
;; ======================================================================

(define (state-pos/c state)
  (and/c exact-nonnegative-integer?
         (>=/c (loc-start (proof-state-loc state)))
         (<=/c (loc-end (proof-state-loc state)))))

(define (state-index/c state)
  (and/c exact-nonnegative-integer?
         (>=/c 0)
         (<=/c (proof-state-size state))))

(provide/contract

 [proof-state? (-> any/c boolean?)]
 [proof-state-acl2-open? (-> any/c boolean?)]

 [initial-proof-state (-> part? proof-state?)]
 [proof-state-populate (-> proof-state? part? proof-state?)]

 [proof-state-start-acl2
  (-> (and/c proof-state? (not/c proof-state-acl2-open?)) acl2?
      proof-state-acl2-open?)]

 [proof-state-update-acl2
  (-> proof-state-acl2-open? acl2? proof-state-acl2-open?)]

 [proof-state-advance
  (-> (and/c proof-state-acl2-open? (not/c proof-state-at-last-term?))
      acl2?
      (or/c sexp/c #f)
      proof-state-acl2-open?)]

 [proof-state-rewind
  (-> (and/c proof-state-acl2-open? (not/c proof-state-at-first-term?))
      proof-state-acl2-open?)]

 [proof-state-edit (-> proof-state-acl2-open? proof-state-acl2-open?)]

 [proof-state-stop-acl2
  (-> proof-state-acl2-open?
      (and/c proof-state? (not/c proof-state-acl2-open?)))]

 [proof-state-start-of-proof? (-> proof-state-acl2-open? boolean?)]
 [proof-state-end-of-proof? (-> proof-state-acl2-open? boolean?)]
 [proof-state-at-first-term? (-> proof-state-acl2-open? boolean?)]
 [proof-state-at-last-term? (-> proof-state-acl2-open? boolean?)]
 [proof-state-finished? (-> proof-state-acl2-open? boolean?)]
 [proof-state-admitted? (-> proof-state-acl2-open? boolean?)]
 [proof-state-edited? (-> proof-state-acl2-open? boolean?)]
 [proof-state-proof-tree (-> proof-state-acl2-open? string?)]
 [proof-state-total-output (-> proof-state-acl2-open? string?)]
 [proof-state-initial-prompt (-> proof-state-acl2-open? string?)]
 [proof-state-acl2-input (-> proof-state-acl2-open? string?)]
 [proof-state-acl2-output (-> proof-state-acl2-open? string?)]
 [proof-state-final-prompt (-> proof-state-acl2-open? string?)]
 [proof-state-next-sexp
  (-> (and/c proof-state-acl2-open? (not/c proof-state-at-last-term?)) sexp/c)]
 [proof-state-save-before-next-sexp
  (-> (and/c proof-state-acl2-open? (not/c proof-state-at-last-term?)) sexp/c)]
 [proof-state-restore-saved-sexp
  (-> (and/c proof-state-admitted? (not/c proof-state-at-first-term?)) sexp/c)]

 [proof-state-size (-> proof-state? exact-positive-integer?)]
 [proof-state-term-index
  (->d ([state proof-state-acl2-open?]) () [_ (state-index/c state)])]
 [proof-state-index-before-pos
  (->d ([state proof-state-acl2-open?] [pos exact-nonnegative-integer?]) ()
       [_ (state-index/c state)])]
 [proof-state-index-after-pos
  (->d ([state proof-state-acl2-open?] [pos exact-nonnegative-integer?]) ()
       [_ (state-index/c state)])]

 [proof-state-first-admitted-position
  (->d ([state proof-state-acl2-open?]) () [_ (state-pos/c state)])]
 [proof-state-last-admitted-position
  (->d ([state proof-state-acl2-open?]) () [_ (state-pos/c state)])]
 [proof-state-last-attempted-position
  (->d ([state proof-state-acl2-open?]) () [_ (state-pos/c state)])]

 )
