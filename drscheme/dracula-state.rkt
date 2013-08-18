#lang racket

(require "../proof/proof.rkt"
         "../acl2/acl2.rkt"
         "../acl2/rep.rkt"
         "../private/hash.rkt"
         "proof-state.rkt")

;; ======================================================================
;;
;;  DATA DEFINITION
;;
;; ======================================================================

;; A DraculaState is: (make-dracula-state Action ProofTable)
;; An Action is either:
;;  - #f
;;  - (make-error-action String)
;;  - (make-normal-action String)
;;  - (make-interrupt-action String)
;; A ProofTable is: (make-proof-table names current hash)
;; where names : (Listof Symbol)
;;   and current : (Or Symbol #f)
;;   and hash : (IHasheq Symbol ProofState)
(define-struct dracula-state (action table) #:prefab)
(define-struct action (desc) #:prefab)
(define-struct (error-action action) () #:prefab)
(define-struct (normal-action action) () #:prefab)
(define-struct (interrupt-action action) () #:prefab)
(define-struct proof-table (names current hash) #:prefab)

;; ======================================================================
;;
;;  PREDICATES
;;
;; ======================================================================

;; dracula-state-active? : Any -> Boolean
;; Recognizes Dracula states with active computation.
(define (dracula-state-active? state)
  (and (dracula-state? state)
       (action? (dracula-state-action state))))

;; dracula-state-busy? : Any -> Boolean
;; Recognizes Dracula states with ongoing computation.
(define (dracula-state-busy? state)
  (and (dracula-state? state)
       (normal-action? (dracula-state-action state))))

;; dracula-state-error? : Any -> Boolean
;; Recognizes Dracula states with error in the computation.
(define (dracula-state-error? state)
  (and (dracula-state? state)
       (error-action? (dracula-state-action state))))

;; dracula-state-interrupt? : Any -> Boolean
;; Recognizes Dracula states with computation about to be interrupted.
(define (dracula-state-interrupt? state)
  (and (dracula-state? state)
       (interrupt-action? (dracula-state-action state))))

;; dracula-state-proof-chosen? : Any -> Boolean
;; Recognizes Dracula states with a selected proof.
(define (dracula-state-proof-chosen? state)
  (and (dracula-state? state)
       (symbol? (proof-table-current (dracula-state-table state)))))

;; dracula-state-acl2-open? : Any -> Boolean
;; Recognizes Dracula states with an ACL2 session for a selected proof.
(define (dracula-state-acl2-open? state)
  (and (dracula-state-proof-chosen? state)
       (proof-state-acl2-open? (dracula-state-current-proof state))))

;; ======================================================================
;;
;;  CONSTRUCTORS
;;
;; ======================================================================

(define initial-proof-table
  (make-proof-table null #f (make-immutable-hasheq null)))

;; initial-dracula-state : DraculaState
;; A plain vanilla empty default initial state.
(define initial-dracula-state
  (make-dracula-state #f initial-proof-table))

;; dracula-state-populate : DraculaState Proof -> DraculaState
;; Populates state with new proof text.
(define (dracula-state-populate state proof)
  (make-dracula-state
   (dracula-state-action state)
   (proof-table-populate (dracula-state-table state) proof)))

(define (proof-table-populate table proof)
  (match table
    [(struct proof-table (old-names old-name old-hash))
     (let* ([new-names (proof-parts proof)]
            [new-name
             (cond [(memq old-name new-names) old-name]
                   [else #f])]
            [new-hash (proof-hash-populate old-hash proof)])
       (make-proof-table new-names new-name new-hash))]))

(define (proof-hash-populate old-hash proof)
  (for/fold
      ([hash old-hash])
      ([name (proof-parts proof)])
    (let* ([part (proof-part proof name)]
           [old-pstate (hash-ref/default hash name #f)]
           [new-pstate (if old-pstate
                           (proof-state-populate old-pstate part)
                           (initial-proof-state part))])
      (hash-set hash name new-pstate))))

;; ==================================================
;;
;;  Activity updates
;;
;; ==================================================

;; dracula-state-pending : DraculaState String -> DraculaState
;; Report a pending operation in the current proof.
(define (dracula-state-pending state desc)
  (make-dracula-state (make-normal-action desc) (dracula-state-table state)))

;; dracula-state-interrupt : DraculaState -> DraculaState
;; Report an interrupted operation in the current proof.
(define (dracula-state-interrupt state)
  (make-dracula-state
   (interrupt (dracula-state-action state))
   (dracula-state-table state)))

(define (interrupt action)
  (make-interrupt-action (action-desc action)))

;; dracula-state-error : DraculaState -> DraculaState
;; Report a failed operation in the current proof.
(define (dracula-state-error state)
  (make-dracula-state
   (if (dracula-state-action state)
     (err (dracula-state-action state))
     (make-error-action "internal failure"))
   (dracula-state-table state)))

(define (err action)
  (make-error-action (action-desc action)))

;; dracula-state-done : DraculaState -> DraculaState
;; Drop a pending or interrupted operation in the current proof.
(define (dracula-state-done state)
  (make-dracula-state #f (dracula-state-table state)))

;; ==================================================
;;
;;  Current proof updates
;;
;; ==================================================

;; dracula-state-choose : DraculaState (Or Symbol #f) -> DraculaState
;; Update the choice of current proof.
(define (dracula-state-choose state name)
  (make-dracula-state
   (dracula-state-action state)
   (proof-table-choose (dracula-state-table state) name)))

(define (proof-table-choose table name)
  (make-proof-table (proof-table-names table) name (proof-table-hash table)))

;; dracula-state-start-acl2 : DraculaState -> DraculaState
;; Records the start of ACL2 in the current proof.
(define (dracula-state-start-acl2 state acl2-state)
  (dracula-state-update-current state proof-state-start-acl2 acl2-state))

;; dracula-state-update-acl2 : DraculaState ACL2State -> DraculaState
;; Records input from ACL2.
(define (dracula-state-update-acl2 state acl2-state)
  (dracula-state-update-current state proof-state-update-acl2 acl2-state))

;; dracula-state-advance : DraculaState ACL2State (Or Sexp #f) -> DraculaState
;; Advance the current proof to the next term, saving the current state.
(define (dracula-state-advance state acl2-state saved)
  (dracula-state-update-current state proof-state-advance acl2-state saved))

;; dracula-state-rewind : DraculaState -> DraculaState
;; Rewind the current proof.
(define (dracula-state-rewind state)
  (dracula-state-update-current state proof-state-rewind))

;; dracula-state-edit : DraculaState -> DraculaState
;; Marks the current proof as edited.
(define (dracula-state-edit state)
  (dracula-state-update-current state proof-state-edit))

;; dracula-state-stop-acl2 : DraculaState -> DraculaState
;; Records the termination of ACL2 in the current proof.
(define (dracula-state-stop-acl2 state)
  (dracula-state-update-current
   (dracula-state-done state)
   proof-state-stop-acl2))

;; ======================================================================
;;
;;  ACCESSORS
;;
;; ======================================================================

;; dracula-state-names : DraculaState -> (Listof String)
;; Produces the set of names of proof parts.
(define (dracula-state-names state)
  (proof-table-names (dracula-state-table state)))

;; dracula-state-activity : DraculaState -> String
;; Describes an active computation in a Dracula state.
(define (dracula-state-activity state)
  (action-desc (dracula-state-action state)))

;; dracula-state-current-name : DraculaState -> Symbol
;; Reports the currently chosen proof part.
(define (dracula-state-current-name state)
  (proof-table-current (dracula-state-table state)))

;; dracula-state-proof-tree : DraculaState -> String
;; Extracts the proof tree from ACL2 in the current proof.
(define (dracula-state-proof-tree state)
  (proof-state-proof-tree (dracula-state-current-proof state)))

;; dracula-state-initial-prompt : DraculaState -> String
;; Extracts the prompt before the current term.
(define (dracula-state-initial-prompt state)
  (proof-state-initial-prompt (dracula-state-current-proof state)))

;; dracula-state-acl2-input : DraculaState -> String
;; Extracts the input from ACL2 in the current proof.
(define (dracula-state-acl2-input state)
  (proof-state-acl2-input (dracula-state-current-proof state)))

;; dracula-state-acl2-output : DraculaState -> String
;; Extracts the output from ACL2 in the current proof.
(define (dracula-state-acl2-output state)
  (proof-state-acl2-output (dracula-state-current-proof state)))

;; dracula-state-final-prompt : DraculaState -> String
;; Extracts the prompt after the current term.
(define (dracula-state-final-prompt state)
  (proof-state-final-prompt (dracula-state-current-proof state)))

;; dracula-state-total-output : DraculaState -> String
;; Returns all of ACL2's output for the current proof.
(define (dracula-state-total-output state)
  (proof-state-total-output (dracula-state-current-proof state)))

;; dracula-state-start-of-proof? : DraculaState -> Boolean
;; Reports whether the current proof is at the start or not.
(define (dracula-state-start-of-proof? state)
  (proof-state-start-of-proof? (dracula-state-current-proof state)))

;; dracula-state-end-of-proof? : DraculaState -> Boolean
;; Reports whether the current proof is at the end or not.
(define (dracula-state-end-of-proof? state)
  (proof-state-end-of-proof? (dracula-state-current-proof state)))

;; dracula-state-at-first-term? : DraculaState -> Boolean
;; Reports whether the current proof is at the start or not.
(define (dracula-state-at-first-term? state)
  (proof-state-at-first-term? (dracula-state-current-proof state)))

;; dracula-state-at-last-term? : DraculaState -> Boolean
;; Reports whether the current proof is at the end or not.
(define (dracula-state-at-last-term? state)
  (proof-state-at-last-term? (dracula-state-current-proof state)))

;; dracula-state-finished? : DraculaState -> Boolean
;; Reports whether the current term has been successfully finished yet.
(define (dracula-state-finished? state)
  (proof-state-finished? (dracula-state-current-proof state)))

;; dracula-state-admitted? : DraculaState -> Boolean
;; Reports whether the current term has been successfully admitted yet.
(define (dracula-state-admitted? state)
  (proof-state-admitted? (dracula-state-current-proof state)))

;; dracula-state-edited? : DraculaState -> Boolean
;; Reports whether the current term has been edited since it was sent to ACL2.
(define (dracula-state-edited? state)
  (proof-state-edited? (dracula-state-current-proof state)))

;; dracula-state-next-sexp : DraculaState -> Sexp
;; Reports the next term to admit.
(define (dracula-state-next-sexp state)
  (proof-state-next-sexp (dracula-state-current-proof state)))

;; dracula-state-save-before-next-sexp : DraculaState -> Sexp
;; Reports the expression to save the current state.
(define (dracula-state-save-before-next-sexp state)
  (proof-state-save-before-next-sexp (dracula-state-current-proof state)))

;; dracula-state-restore-saved-sexp : DraculaState -> Sexp
;; Reports the event to restore the saved state.
(define (dracula-state-restore-saved-sexp state)
  (proof-state-restore-saved-sexp (dracula-state-current-proof state)))

;; dracula-state-first-admitted-position : DraculaState -> Nat
(define (dracula-state-first-admitted-position state)
  (proof-state-first-admitted-position (dracula-state-current-proof state)))

;; dracula-state-last-admitted-position : DraculaState -> Nat
(define (dracula-state-last-admitted-position state)
  (proof-state-last-admitted-position (dracula-state-current-proof state)))

;; dracula-state-last-attempted-position : DraculaState -> Nat
(define (dracula-state-last-attempted-position state)
  (proof-state-last-attempted-position (dracula-state-current-proof state)))

;; dracula-state-term-index : DraculaState -> Nat
(define (dracula-state-term-index state)
  (proof-state-term-index (dracula-state-current-proof state)))

;; dracula-state-proof-size : DraculaState -> Nat
(define (dracula-state-proof-size state)
  (proof-state-size (dracula-state-current-proof state)))

;; dracula-state-index-before-pos : DraculaState Nat -> Nat
(define (dracula-state-index-before-pos state pos)
  (proof-state-index-before-pos (dracula-state-current-proof state) pos))

;; dracula-state-index-after-pos : DraculaState Nat -> Nat
(define (dracula-state-index-after-pos state pos)
  (proof-state-index-after-pos (dracula-state-current-proof state) pos))

;; ======================================================================
;;
;;  HELPERS
;;
;; ======================================================================

;; dracula-state-update-current :
;;  DraculaState (ProofState T ... -> ProofState) T ... -> DraculaState
;; Transform the current proof state.
(define (dracula-state-update-current state update . args)
  (make-dracula-state
   (dracula-state-action state)
   (apply proof-table-update-current (dracula-state-table state) update args)))

(define (proof-table-update-current table update . args)
  (make-proof-table
   (proof-table-names table)
   (proof-table-current table)
   (hash-update (proof-table-hash table)
                (proof-table-current table)
                (lambda (pstate) (apply update pstate args)))))

;; dracula-state-current-proof : DraculaState -> ProofState
;; Produce the current proof state.
(define (dracula-state-current-proof state)
  (let* ([table (dracula-state-table state)]
         [hash (proof-table-hash table)]
         [current (proof-table-current table)])
    (hash-ref/check hash current)))

;; ======================================================================
;;
;;  EXPORTS
;;
;; ======================================================================

(provide/contract

 ;; ==================================================
 ;;
 ;;  Predicates
 ;;
 ;; ==================================================

 [dracula-state? (-> any/c boolean?)]

 ;; ==============================
 ;;  Activity predicates
 ;; ==============================

 [dracula-state-active? (-> any/c boolean?)]    ;; any activity
 [dracula-state-busy? (-> any/c boolean?)]      ;; normal activity
 [dracula-state-error? (-> any/c boolean?)]     ;; error activity
 [dracula-state-interrupt? (-> any/c boolean?)] ;; interrupted activity

 ;; ==============================
 ;;  Current proof predicates.
 ;; ==============================

 [dracula-state-proof-chosen? (-> any/c boolean?)]
 [dracula-state-acl2-open? (-> any/c boolean?)] ;; implies previous

 ;; ==================================================
 ;;
 ;;  Constructors
 ;;
 ;; ==================================================

 [initial-dracula-state dracula-state?]
 [dracula-state-populate (-> dracula-state? proof? dracula-state?)]

 ;; ==============================
 ;;  Activity updates
 ;; ==============================

 [dracula-state-pending
  (-> (and/c dracula-state?
             (not/c dracula-state-interrupt?)
             (not/c dracula-state-error?))
      string?
      dracula-state-busy?)]

 [dracula-state-error
  (-> dracula-state-acl2-open? dracula-state-error?)]

 [dracula-state-interrupt
  (-> (and/c dracula-state? (not/c dracula-state-error?))
      dracula-state-interrupt?)]

 [dracula-state-done
  (-> (and/c dracula-state-active? (not/c dracula-state-error?))
      (and/c dracula-state? (not/c dracula-state-active?)))]

 ;; ==============================
 ;;  Current proof updates
 ;; ==============================

 [dracula-state-choose
  (-> dracula-state? symbol? dracula-state-proof-chosen?)]

 [dracula-state-start-acl2
  (-> (and/c dracula-state-proof-chosen? (not/c dracula-state-acl2-open?)) acl2?
      dracula-state-acl2-open?)]

 [dracula-state-update-acl2
  (-> dracula-state-acl2-open? acl2? dracula-state-acl2-open?)]

 [dracula-state-advance
  (-> (and/c dracula-state-acl2-open?
             dracula-state-admitted?
             (not/c dracula-state-at-last-term?))
      acl2?
      (or/c sexp/c #f)
      dracula-state-acl2-open?)]

 [dracula-state-rewind
  (-> (and/c dracula-state-acl2-open? (not/c dracula-state-at-first-term?))
      dracula-state-acl2-open?)]
 
 [dracula-state-edit (-> dracula-state-acl2-open? dracula-state-acl2-open?)]

 [dracula-state-stop-acl2
  (-> dracula-state-acl2-open?
      (and/c dracula-state-proof-chosen?
             (not/c dracula-state-acl2-open?)
             (not/c dracula-state-active?)))]

 ;; ==================================================
 ;;
 ;;  Accessors
 ;;
 ;; ==================================================

 [dracula-state-names (-> dracula-state? (listof symbol?))]

 ;; ==============================
 ;;  Activity data
 ;; ==============================

 [dracula-state-activity (-> dracula-state-active? string?)]

 ;; ==============================
 ;;  Current proof data
 ;; ==============================

 [dracula-state-at-first-term? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-at-last-term? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-current-name (-> dracula-state-proof-chosen? symbol?)]
 [dracula-state-next-sexp
  (-> (and/c dracula-state-acl2-open? (not/c dracula-state-at-last-term?))
      sexp/c)]
 [dracula-state-save-before-next-sexp
  (-> (and/c dracula-state-acl2-open? (not/c dracula-state-at-last-term?))
      sexp/c)]
 [dracula-state-restore-saved-sexp
  (-> (and/c dracula-state-admitted? (not/c dracula-state-at-first-term?))
      sexp/c)]
 [dracula-state-start-of-proof? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-end-of-proof? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-finished? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-admitted? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-edited? (-> dracula-state-acl2-open? boolean?)]
 [dracula-state-proof-tree (-> dracula-state-acl2-open? string?)]
 [dracula-state-initial-prompt (-> dracula-state-acl2-open? string?)]
 [dracula-state-acl2-input (-> dracula-state-acl2-open? string?)]
 [dracula-state-acl2-output (-> dracula-state-acl2-open? string?)]
 [dracula-state-final-prompt (-> dracula-state-acl2-open? string?)]
 [dracula-state-total-output (-> dracula-state-acl2-open? string?)]

 [dracula-state-proof-size
  (-> dracula-state-proof-chosen? exact-nonnegative-integer?)]
 [dracula-state-term-index
  (-> dracula-state-acl2-open? exact-nonnegative-integer?)]
 [dracula-state-first-admitted-position
  (-> dracula-state-acl2-open? exact-nonnegative-integer?)]
 [dracula-state-last-admitted-position
  (-> dracula-state-acl2-open? exact-nonnegative-integer?)]
 [dracula-state-last-attempted-position
  (-> dracula-state-acl2-open? exact-nonnegative-integer?)]
 [dracula-state-index-before-pos
  (-> dracula-state-acl2-open? exact-nonnegative-integer?
      exact-nonnegative-integer?)]
 [dracula-state-index-after-pos
  (-> dracula-state-acl2-open? exact-nonnegative-integer?
      exact-nonnegative-integer?)]

 )
