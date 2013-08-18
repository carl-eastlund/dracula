#lang racket

(require "../proof/proof.rkt" "../acl2/rep.rkt" "../acl2/acl2.rkt")

;; A TermState is:
;;  (make-term-state Loc (Or Sexp #f) (or Sexp Boolean) (Or Interaction #f))
;; An Interaction is (make-interaction String String String String Status)
;; A Status is 'admitted, 'failed, 'failed+edited, or #f.
(define-struct term-state (loc sexp saved output) #:prefab)
(define-struct interaction (initial input output final tree status) #:prefab)

(define (term-state-populate old new)
  (match (list old new)
    [(list (struct term-state [_ old-sexp _ (app interaction-status 'admitted)])
           (struct term-state [_ (and new-sexp (not old-sexp)) _ _]))
     (error 'term-state-populate
            "mismatch between ~s and ~s"
            old-sexp new-sexp)]
    [(list (struct term-state [_ _ saved output])
           (struct term-state [loc sexp _ _]))
     (make-term-state loc sexp saved output)]))

(define (term-state-edit state)
  (struct-copy term-state state
               [output (interaction-edit (term-state-output state))]))

(define (interaction-edit inter)
  (struct-copy interaction inter
               [status (status-edit (interaction-status inter))]))

(define (status-edit status)
  (if (eq? status 'failed) 'failed+edited status))

(define (term-state-active? v)
  (and (term-state? v) (interaction? (term-state-output v))))

(define (term-state-has-input? v)
  (and (term-state? v) (not (false? (term-state-sexp v)))))

(define (empty-term-state loc) (make-term-state loc #f #f #f))

(define (initial-term-state term)
  (make-term-state (term-loc term) (term-sexp term) #f #f))

(define (term-state-update-acl2 state acl2)
  (struct-copy term-state state [output (acl2->interaction acl2)]))

(define (term-state-start-acl2 state acl2 saved)
  (struct-copy term-state state
               [saved saved]
               [output (acl2->interaction acl2)]))

(define (term-state-save-before-sexp state)
  '(absolute-to-relative-command-number
    (max-absolute-command-number (w state))
    (w state)))

(define (term-state-restore-saved-sexp state)
  `(ubu! ,(term-state-saved state)))

(define (term-state-stop-acl2 state)
  (struct-copy term-state state [saved #f] [output #f]))

(define (term-state-initial-prompt state)
  (interaction-initial (term-state-output state)))

(define (term-state-acl2-input state)
  (interaction-input (term-state-output state)))

(define (term-state-acl2-output state)
  (interaction-output (term-state-output state)))

(define (term-state-final-prompt state)
  (interaction-final (term-state-output state)))

(define (term-state-proof-tree state)
  (interaction-tree (term-state-output state)))

(define (term-state-total-output state)
  (string-append (term-state-initial-prompt state)
                 (term-state-acl2-input state)
                 (term-state-acl2-output state)))

(define (term-state-finished? state)
  (symbol? (interaction-status (term-state-output state))))

(define (term-state-admitted? state)
  (and (term-state-saved state)
       (equal? 'admitted (interaction-status (term-state-output state)))))

(define (term-state-edited? state)
  (equal? 'failed+edited (interaction-status (term-state-output state))))

(define (acl2->interaction acl2)
  (make-interaction (acl2-initial-prompt acl2)
                    (acl2-input acl2)
                    (acl2-output acl2)
                    (acl2-final-prompt acl2)
                    (acl2-proof-tree acl2)
                    (if (acl2-done? acl2)
                        (if (acl2-admitted? acl2) 'admitted 'failed)
                        #f)))

(provide/contract
 [term-state? (-> any/c boolean?)]
 [term-state-active? (-> any/c boolean?)]
 [term-state-has-input? (-> any/c boolean?)]
 [empty-term-state (-> loc? (and/c term-state?
                                   (not/c term-state-has-input?)
                                   (not/c term-state-active?)))]
 [initial-term-state (-> term? (and/c term-state?
                                      term-state-has-input?
                                      (not/c term-state-active?)))]
 [term-state-populate (-> term-state? term-state? term-state?)]
 [term-state-edit (-> term-state? term-state?)]
 [term-state-start-acl2
  (-> (and/c term-state? (not/c term-state-active?))
      acl2?
      (or/c sexp/c boolean?)
      term-state-active?)]
 [term-state-stop-acl2
  (-> term-state-active? (and/c term-state? (not/c term-state-active?)))]
 [term-state-save-before-sexp (-> term-state? sexp/c)]
 [term-state-restore-saved-sexp (-> term-state-admitted? sexp/c)]
 [term-state-update-acl2 (-> term-state-active? acl2? term-state-active?)]
 [term-state-sexp (-> term-state-has-input? (or/c sexp/c #f))]
 [term-state-loc (-> term-state? loc?)]
 [term-state-finished? (-> term-state-active? boolean?)]
 [term-state-admitted? (-> term-state-active? boolean?)]
 [term-state-edited? (-> term-state-active? boolean?)]
 [term-state-initial-prompt (-> term-state-active? string?)]
 [term-state-acl2-input (-> term-state-active? string?)]
 [term-state-acl2-output (-> term-state-active? string?)]
 [term-state-final-prompt (-> term-state-active? string?)]
 [term-state-proof-tree (-> term-state-active? string?)]
 [term-state-total-output (-> term-state-active? string?)])
