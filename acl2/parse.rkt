#lang racket

(require "../private/regexp.rkt"
         (only-in mzlib/etc [this-expression-file-name src-file]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Data Definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ParseState is (make-parse-state FinalBlock [Listof ParseBlock]).
;; Final represents the end of the text (partial input or a prompt).
;; Prior is a list of parse blocks that have finished parsing.
(define-struct parse-state (final prior) #:prefab)

;; A FinalBlock is either a PromptBlock or a PartialBlock.
;; A PromptBlock is (make-prompt-block String).
;; A PartialBlock is (make-final-block String ParseBlock).
(define-struct final-block () #:prefab)
(define-struct (prompt-block final-block) (text) #:prefab)
(define-struct (partial-block final-block) (line block) #:prefab)

;; A ParseBlock is either a TextBlock or a TreeBlock.
;; A TextBlock is (make-text-block String).
;; A TreeBlock is (make-tree-block String).
(define-struct parse-block (text) #:prefab)
(define-struct (text-block parse-block) () #:prefab)
(define-struct (tree-block parse-block) () #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Block Parsing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; empty-parse-state : ParseState
;; The initial parse state.  Starts in an empty text block
;; with no complete blocks.
(define empty-parse-state
  (make-parse-state (make-partial-block "" (make-text-block "")) null))

(define-syntax-rule (define/match (f x ...) [(p ...) e ...] ...)
  (define (f x ...)
    (match* [x ...]
      [(p ...) e ...]
      ...
      [(x ...)
       (error "match error in ~a while evaluating: ~s"
         (src-file f)
         `(f ',x ...))])))

;; parse : String ParseState -> ParseState
;; Updates the parse state with new input.
(define/match (parse string state)
  ;; Continue a partial parse.
  ([_ (struct parse-state [(struct partial-block [line block]) blocks])]
   (parse-partial (string-append line string) block blocks))
  ;; Oops, we detected a prompt prematurely:
  ([_ (struct parse-state [(struct prompt-block [prompt]) blocks])]
   (parse-partial (string-append prompt string) (make-text-block "") blocks)))

;; parse-partial : String Block [Listof Block]
;; Continue parsing a block.
(define/match (parse-partial text block blocks)
  ([_ (struct text-block [old-text]) _]
   (parse-text text old-text blocks))
  ([_ (struct tree-block [old-text]) _]
   (parse-tree text old-text blocks)))

;; parse-text : String String [Listof Block]
;; Parse new-text following old-text, adding results to complete blocks.
(define/match (parse-text new-text old-text complete)

    ;; If we find the start of a proof tree...
  ([(pregexp #px"(?s:^(.*?)#<\\\\<0(.*)$)" (list _ pre post)) _ _]
   (parse-tree
    post "" (cons (make-text-block (string-append old-text pre)) complete)))

  ;; ...or if we find a prompt at the very end...
  ([(pregexp #px"(?s:^(.*\n|)(ACL2 p?!?s?>)$)" (list _ pre post)) _ _]
   (make-parse-state
    (make-prompt-block post)
    (cons (make-text-block (string-append old-text pre)) complete)))

  ;; ...otherwise, save any complete lines.
  ([(pregexp #px"(?s:^(.*?)([^\n]*)$)" (list _ pre post)) _ _]
   (make-parse-state
    (make-partial-block post (make-text-block (string-append old-text pre)))
    complete)))

;; parse-tree : String String [Listof Block]
;; Parse new-text following old-text of a proof tree,
;; adding results to complete blocks.
(define/match (parse-tree new-text old-text complete)

    ;; If we find the end of the proof tree...
  ([(pregexp #px"(?s:^(.*?)#>\\\\>(.*)$)" (list _ pre post)) _ _]
   (parse-text
    post "" (cons (make-tree-block (string-append old-text pre)) complete)))

  ;; ...otherwise, save any complete lines.
  ([(pregexp #px"(?s:^(.*?)([^\n]*)$)" (list _ pre post)) _ _]
   (make-parse-state
    (make-partial-block post (make-tree-block (string-append old-text pre)))
    complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Error Message Parsing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse-success? : ParseState -> Boolean
;; Reports whether a parse represents success by ACL2 or not.
(define (parse-success? state)
  (not (parse-failure? state)))

;; parse-failure? : ParseState -> Boolean
;; Reports whether a parse contains an ACL2 error message.
(define (parse-failure? state)
  (ormap block-failure? (parse-state-prior state)))

;; block-failure? : ParseBlock -> Boolean
;; Reports whether a block contains an ACL2 error message.
(define (block-failure? block)
  (cond
   [(tree-block? block) #f]
   [(text-block? block) (line-success? (parse-block-text block))]))

;; line-failure? : String -> Boolean
;; Reports whether a string contains an ACL2 error mesage.
(define (line-success? line)
  (regexp-match?
   (pregexp
    (regexp-or
     (regexp-quote "************ ABORTING from raw Lisp ***********")
     (regexp-quote "******** FAILED ********")
     (regexp-quote "HARD ACL2 ERROR in")
     (regexp-quote "ACL2 Error in")))
   line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Proof Tree Parsing
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lbracket (regexp-quote "["))
(define rbracket (regexp-quote "]"))
(define prime (regexp-quote "'"))
(define slash (regexp-quote "/"))
(define asterisk (regexp-quote "*"))
(define dot (regexp-quote "."))
(define bar (regexp-quote "|"))

(define start "^")
(define end "$")
(define number "\\d+")
(define space (regexp-star (regexp-quote " ")))

(define dotted-number-regexp
  (regexp-sequence number (regexp-star dot number)))

(define forcing-round-regexp
  (regexp-sequence lbracket number rbracket))

(define prime-regexp
  (regexp-or (regexp-star prime)
             (regexp-sequence prime number prime)))

;; Subgoal spec ("Subgoal 1" or "[2]Subgoal *1.2/3.4''", etc.)
(define subgoal-regexp
  (regexp-sequence (regexp-maybe forcing-round-regexp)
                   (regexp-quote "Subgoal ")
                   (regexp-maybe asterisk dotted-number-regexp slash)
                   dotted-number-regexp
                   prime-regexp))

;; Goal spec ("Goal", "Goal''", etc.)
(define goal-regexp
  (regexp-sequence (regexp-quote "Goal") prime-regexp))

;; Pushed goal spec ("*1.3", etc.)
(define pushed-goal-regexp
  (regexp-sequence asterisk dotted-number-regexp))

;; Goal Spec ("Goal'" or "[2]Subgoal *1.2/3.4''" or "*1.4").
(define goal-spec-regexp
  (regexp-or goal-regexp subgoal-regexp pushed-goal-regexp))

(define checkpoint-regexp (regexp-maybe (regexp-quote "c")))

(define process-regexp ".*")

;; Proof tree goal line ("c | | | Goal preprocess\n", etc.)
(define goal-line-regexp
  (regexp-multi
   (regexp-sequence #:start start #:end end #:between space
                    checkpoint-regexp
                    (regexp-maybe number)
                    (regexp-star bar space)
                    (regexp-save goal-spec-regexp)
                    process-regexp)))

(define (parse-subgoals proof-tree)
  (let loop ([index 0])
    (match (regexp-match-positions (pregexp goal-line-regexp) proof-tree index)
      [#f null]
      [(list (cons line-start line-end) (cons name-start name-end))
       (cons (list (substring proof-tree name-start name-end)
                   line-start line-end)
             (loop line-end))]
      [_ (error 'parse-subgoals "error matching ~s at index ~s" proof-tree index)])))

(define (parse-finished? parse)
  (prompt-block? (parse-state-final parse)))

(define (parse-state-blocks parse)
  (append
   (cond
    [(parse-state-final parse) => list]
    [else null])
   (parse-state-prior parse)))

(define (parse-prompt parse)
  (match (parse-state-final parse)
    [(struct prompt-block [text]) text]
    [_ ""]))

(define (parse-last-proof-tree parse)
  (cond
   [(findf tree-block? (parse-state-prior parse)) => parse-block-text]
   [else ""]))

(define (parse-normal-text parse)
  (apply string-append (parse-normal-strings parse)))

(define (parse-normal-strings parse)
  (append (prior-normal-strings (parse-state-prior parse))
          (final-normal-strings (parse-state-final parse))))

(define (prior-normal-strings priors)
  (block-normal-strings (reverse priors)))

(define (block-normal-strings blocks)
  (map block-normal-string blocks))

(define (block-normal-string block)
  (match block
    [(struct text-block [text]) text]
    [(struct tree-block [_]) ""]
    [_ (error 'block-normal-string "not a block: ~s" block)]))

(define (final-normal-strings final)
  (match final
    [(struct prompt-block [prompt]) null]
    [(struct partial-block [partial block])
     (list (block-normal-string block) partial)]
    [_ (error 'final-normal-strings "not a final block: ~s" final)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Exports
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract

 ;; Predicates
 [parse-state? (-> any/c boolean?)]
 [parse-block? (-> any/c boolean?)]
 [text-block? (-> any/c boolean?)]
 [tree-block? (-> any/c boolean?)]

 ;; Constructors
 [make-parse-state
  (-> final-block? (listof parse-block?) parse-state?)]
 [make-text-block (-> string? parse-block?)]
 [make-tree-block (-> string? parse-block?)]
 [make-partial-block (-> string? parse-block? final-block?)]
 [make-prompt-block (-> string? final-block?)]

 ;; Selectors
 [parse-state-blocks
  (-> parse-state? (listof (or/c parse-block? partial-block?)))]
 [parse-block-text (-> parse-block? string?)]

 ;; Initial state
 [empty-parse-state parse-state?]

 ;; Parsing functions
 [parse (-> string? parse-state? parse-state?)]
 [parse-finished? (-> parse-state? boolean?)]
 [parse-success? (-> (and/c parse-state? parse-finished?) boolean?)]
 [parse-prompt (-> parse-state? string?)]
 [parse-normal-text (-> parse-state? string?)]
 [parse-last-proof-tree (-> parse-state? string?)]
 [parse-subgoals
  (-> string? (listof (list/c string?
                              exact-nonnegative-integer?
                              exact-nonnegative-integer?)))])
