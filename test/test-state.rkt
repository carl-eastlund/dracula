#lang racket

(require "../private/planet.rkt"
         "../drscheme/dracula-state.rkt"
         "data-state.rkt")
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Helpers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; split-all : String -> [Listof String]
;; Produces every character in a string.
(define (split-all str)
  (regexp-split "" str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Macros
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (test/equal? stx)
  (syntax-case stx ()
    [(_ actual expected)
     (syntax/loc stx
       (test-equal? (format "(equal? ~s ~s)" 'actual 'expected)
                    actual expected))]))

(define-syntax (test/true stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx
       (test-true (format "~s" 'e) e))]))

(define-syntax (test/false stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx
       (test-false (format "(not ~s)" 'e) e))]))

(define-syntax (test/error stx)
  (syntax-case stx ()
    [(_ e)
     (syntax/loc stx
       (test-exn (format "Error?: ~s" 'e) exn:fail? (lambda () e)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test Suite
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define state-test-suite
 (test-suite "Proof State"

   #|
   (test-suite "Empty State"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size empty-proof-state) 0))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part empty-proof-state) #f)))

   (test-suite "Merge Proof"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size merged-state) 2))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part merged-state) 0))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size merged-state 0) 3)
       (test/equal? (proof-state-part-size merged-state 1) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position merged-state 1) 0)
       (test/equal? (proof-state-position merged-state 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output merged-state 1 0) "")
       (test/equal? (proof-state-output merged-state 1 1) "")
       (test/equal? (proof-state-output merged-state 1 2) "")
       (test/equal? (proof-state-output merged-state 0 0) "")
       (test/equal? (proof-state-output merged-state 0 1) "")
       (test/equal? (proof-state-output merged-state 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary merged-state 1 0) "")
       (test/equal? (proof-state-summary merged-state 1 1) "")
       (test/equal? (proof-state-summary merged-state 1 2) "")
       (test/equal? (proof-state-summary merged-state 0 0) "")
       (test/equal? (proof-state-summary merged-state 0 2) "")
       (test/equal? (proof-state-summary merged-state 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/false (proof-state-finished? merged-state 1 0))
       (test/false (proof-state-finished? merged-state 1 1))
       (test/false (proof-state-finished? merged-state 1 2))
       (test/false (proof-state-finished? merged-state 0 0))
       (test/false (proof-state-finished? merged-state 0 1))
       (test/false (proof-state-finished? merged-state 0 2)))
     (test-suite "proof-state-successful?"
       (test/error (proof-state-successful? merged-state 1 0))
       (test/error (proof-state-successful? merged-state 1 1))
       (test/error (proof-state-successful? merged-state 1 2))
       (test/error (proof-state-successful? merged-state 0 0))
       (test/error (proof-state-successful? merged-state 0 1))
       (test/error (proof-state-successful? merged-state 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input merged-state 1 0))
       (test/equal? (proof-state-input merged-state 1 1) "(+ 1 2)")
       (test/equal? (proof-state-input merged-state 1 2) "(* 3 4)")
       (test/error (proof-state-input merged-state 0 0))
       (test/equal? (proof-state-input merged-state 0 1) "(list (quote a) (quote b))")
       (test/equal? (proof-state-input merged-state 0 2) "(quote (c d))")))

   (test-suite "Select Second Part"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size select-state) 2))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part select-state) 1))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size select-state 0) 3)
       (test/equal? (proof-state-part-size select-state 1) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position select-state 1) 0)
       (test/equal? (proof-state-position select-state 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output select-state 1 0) "")
       (test/equal? (proof-state-output select-state 1 1) "")
       (test/equal? (proof-state-output select-state 1 2) "")
       (test/equal? (proof-state-output select-state 0 0) "")
       (test/equal? (proof-state-output select-state 0 1) "")
       (test/equal? (proof-state-output select-state 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary select-state 1 0) "")
       (test/equal? (proof-state-summary select-state 1 1) "")
       (test/equal? (proof-state-summary select-state 1 2) "")
       (test/equal? (proof-state-summary select-state 0 0) "")
       (test/equal? (proof-state-summary select-state 0 2) "")
       (test/equal? (proof-state-summary select-state 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/false (proof-state-finished? select-state 1 0))
       (test/false (proof-state-finished? select-state 1 1))
       (test/false (proof-state-finished? select-state 1 2))
       (test/false (proof-state-finished? select-state 0 0))
       (test/false (proof-state-finished? select-state 0 1))
       (test/false (proof-state-finished? select-state 0 2)))
     (test-suite "proof-state-successful?"
       (test/error (proof-state-successful? select-state 1 0))
       (test/error (proof-state-successful? select-state 1 1))
       (test/error (proof-state-successful? select-state 1 2))
       (test/error (proof-state-successful? select-state 0 0))
       (test/error (proof-state-successful? select-state 0 1))
       (test/error (proof-state-successful? select-state 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input select-state 1 0))
       (test/equal? (proof-state-input select-state 1 1) "(+ 1 2)")
       (test/equal? (proof-state-input select-state 1 2) "(* 3 4)")
       (test/error (proof-state-input select-state 0 0))
       (test/equal? (proof-state-input select-state 0 1) "(list (quote a) (quote b))")
       (test/equal? (proof-state-input select-state 0 2) "(quote (c d))")))

   (test-suite "Initial Prompt"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size initial-prompt) 2))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part initial-prompt) 1))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size initial-prompt 0) 3)
       (test/equal? (proof-state-part-size initial-prompt 1) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position initial-prompt 1) 0)
       (test/equal? (proof-state-position initial-prompt 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output initial-prompt 1 0) initial-text)
       (test/equal? (proof-state-output initial-prompt 1 1) "")
       (test/equal? (proof-state-output initial-prompt 1 2) "")
       (test/equal? (proof-state-output initial-prompt 0 0) "")
       (test/equal? (proof-state-output initial-prompt 0 1) "")
       (test/equal? (proof-state-output initial-prompt 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary initial-prompt 1 0) "")
       (test/equal? (proof-state-summary initial-prompt 1 1) "")
       (test/equal? (proof-state-summary initial-prompt 1 2) "")
       (test/equal? (proof-state-summary initial-prompt 0 0) "")
       (test/equal? (proof-state-summary initial-prompt 0 2) "")
       (test/equal? (proof-state-summary initial-prompt 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/true (proof-state-finished? initial-prompt 1 0))
       (test/false (proof-state-finished? initial-prompt 1 1))
       (test/false (proof-state-finished? initial-prompt 1 2))
       (test/false (proof-state-finished? initial-prompt 0 0))
       (test/false (proof-state-finished? initial-prompt 0 1))
       (test/false (proof-state-finished? initial-prompt 0 2)))
     (test-suite "proof-state-successful?"
       (test/true (proof-state-successful? initial-prompt 1 0))
       (test/error (proof-state-successful? initial-prompt 1 1))
       (test/error (proof-state-successful? initial-prompt 1 2))
       (test/error (proof-state-successful? initial-prompt 0 0))
       (test/error (proof-state-successful? initial-prompt 0 1))
       (test/error (proof-state-successful? initial-prompt 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input initial-prompt 1 0))
       (test/equal? (proof-state-input initial-prompt 1 1) "(+ 1 2)")
       (test/equal? (proof-state-input initial-prompt 1 2) "(* 3 4)")
       (test/error (proof-state-input initial-prompt 0 0))
       (test/equal? (proof-state-input initial-prompt 0 1)
                    "(list (quote a) (quote b))")
       (test/equal? (proof-state-input initial-prompt 0 2) "(quote (c d))")))

   (test-suite "Started A Term"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size start-a-term) 2))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part start-a-term) 1))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size start-a-term 0) 3)
       (test/equal? (proof-state-part-size start-a-term 1) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position start-a-term 1) 1)
       (test/equal? (proof-state-position start-a-term 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output start-a-term 1 0) initial-text)
       (test/equal? (proof-state-output start-a-term 1 1) "")
       (test/equal? (proof-state-output start-a-term 1 2) "")
       (test/equal? (proof-state-output start-a-term 0 0) "")
       (test/equal? (proof-state-output start-a-term 0 1) "")
       (test/equal? (proof-state-output start-a-term 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary start-a-term 1 0) "")
       (test/equal? (proof-state-summary start-a-term 1 1) "")
       (test/equal? (proof-state-summary start-a-term 1 2) "")
       (test/equal? (proof-state-summary start-a-term 0 0) "")
       (test/equal? (proof-state-summary start-a-term 0 2) "")
       (test/equal? (proof-state-summary start-a-term 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/true (proof-state-finished? start-a-term 1 0))
       (test/false (proof-state-finished? start-a-term 1 1))
       (test/false (proof-state-finished? start-a-term 1 2))
       (test/false (proof-state-finished? start-a-term 0 0))
       (test/false (proof-state-finished? start-a-term 0 1))
       (test/false (proof-state-finished? start-a-term 0 2)))
     (test-suite "proof-state-successful?"
       (test/true (proof-state-successful? start-a-term 1 0))
       (test/error (proof-state-successful? start-a-term 1 1))
       (test/error (proof-state-successful? start-a-term 1 2))
       (test/error (proof-state-successful? start-a-term 0 0))
       (test/error (proof-state-successful? start-a-term 0 1))
       (test/error (proof-state-successful? start-a-term 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input start-a-term 1 0))
       (test/equal? (proof-state-input start-a-term 1 1) "(+ 1 2)")
       (test/equal? (proof-state-input start-a-term 1 2) "(* 3 4)")
       (test/error (proof-state-input start-a-term 0 0))
       (test/equal? (proof-state-input start-a-term 0 1)
                    "(list (quote a) (quote b))")
       (test/equal? (proof-state-input start-a-term 0 2) "(quote (c d))")))

   (test-suite "One Admitted Term"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size admit-a-term) 2))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part admit-a-term) 1))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size admit-a-term 0) 3)
       (test/equal? (proof-state-part-size admit-a-term 1) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position admit-a-term 1) 1)
       (test/equal? (proof-state-position admit-a-term 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output admit-a-term 1 0) initial-text)
       (test/equal? (proof-state-output admit-a-term 1 1) three-text)
       (test/equal? (proof-state-output admit-a-term 1 2) "")
       (test/equal? (proof-state-output admit-a-term 0 0) "")
       (test/equal? (proof-state-output admit-a-term 0 1) "")
       (test/equal? (proof-state-output admit-a-term 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary admit-a-term 1 0) "")
       (test/equal? (proof-state-summary admit-a-term 1 1) "")
       (test/equal? (proof-state-summary admit-a-term 1 2) "")
       (test/equal? (proof-state-summary admit-a-term 0 0) "")
       (test/equal? (proof-state-summary admit-a-term 0 2) "")
       (test/equal? (proof-state-summary admit-a-term 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/true (proof-state-finished? admit-a-term 1 0))
       (test/true (proof-state-finished? admit-a-term 1 1))
       (test/false (proof-state-finished? admit-a-term 1 2))
       (test/false (proof-state-finished? admit-a-term 0 0))
       (test/false (proof-state-finished? admit-a-term 0 1))
       (test/false (proof-state-finished? admit-a-term 0 2)))
     (test-suite "proof-state-successful?"
       (test/true (proof-state-successful? admit-a-term 1 0))
       (test/true (proof-state-successful? admit-a-term 1 1))
       (test/error (proof-state-successful? admit-a-term 1 2))
       (test/error (proof-state-successful? admit-a-term 0 0))
       (test/error (proof-state-successful? admit-a-term 0 1))
       (test/error (proof-state-successful? admit-a-term 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input admit-a-term 1 0))
       (test/equal? (proof-state-input admit-a-term 1 1) "(+ 1 2)")
       (test/equal? (proof-state-input admit-a-term 1 2) "(* 3 4)")
       (test/error (proof-state-input admit-a-term 0 0))
       (test/equal? (proof-state-input admit-a-term 0 1)
                    "(list (quote a) (quote b))")
       (test/equal? (proof-state-input admit-a-term 0 2) "(quote (c d))")))

   (test-suite "Unchanged Parts"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size unchanged-merge) 1))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part unchanged-merge) 0))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size unchanged-merge 0) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position unchanged-merge 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output unchanged-merge 0 0) "")
       (test/equal? (proof-state-output unchanged-merge 0 1) "")
       (test/equal? (proof-state-output unchanged-merge 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary unchanged-merge 0 0) "")
       (test/equal? (proof-state-summary unchanged-merge 0 2) "")
       (test/equal? (proof-state-summary unchanged-merge 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/false (proof-state-finished? unchanged-merge 0 0))
       (test/false (proof-state-finished? unchanged-merge 0 1))
       (test/false (proof-state-finished? unchanged-merge 0 2)))
     (test-suite "proof-state-successful?"
       (test/error (proof-state-successful? unchanged-merge 0 0))
       (test/error (proof-state-successful? unchanged-merge 0 1))
       (test/error (proof-state-successful? unchanged-merge 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input unchanged-merge 0 0))
       (test/equal? (proof-state-input unchanged-merge 0 1)
                    "(list (quote a) (quote b))")
       (test/equal? (proof-state-input unchanged-merge 0 2)
                    "(quote (c d))")))

   (test-suite "Part Replaced"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size replaced-merge) 1))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part replaced-merge) 0))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size replaced-merge 0) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position replaced-merge 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output replaced-merge 0 0) "")
       (test/equal? (proof-state-output replaced-merge 0 1) "")
       (test/equal? (proof-state-output replaced-merge 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary replaced-merge 0 0) "")
       (test/equal? (proof-state-summary replaced-merge 0 2) "")
       (test/equal? (proof-state-summary replaced-merge 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/false (proof-state-finished? replaced-merge 0 0))
       (test/false (proof-state-finished? replaced-merge 0 1))
       (test/false (proof-state-finished? replaced-merge 0 2)))
     (test-suite "proof-state-successful?"
       (test/error (proof-state-successful? replaced-merge 0 0))
       (test/error (proof-state-successful? replaced-merge 0 1))
       (test/error (proof-state-successful? replaced-merge 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input replaced-merge 0 0))
       (test/equal? (proof-state-input replaced-merge 0 1) "(+ 1 2)")
       (test/equal? (proof-state-input replaced-merge 0 2) "(* 3 4)")))

   (test-suite "Renamed Merge"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size replaced-merge) 1))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part renamed-merge) 0))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size renamed-merge 0) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position renamed-merge 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output renamed-merge 0 0) "")
       (test/equal? (proof-state-output renamed-merge 0 1) "")
       (test/equal? (proof-state-output renamed-merge 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary renamed-merge 0 0) "")
       (test/equal? (proof-state-summary renamed-merge 0 2) "")
       (test/equal? (proof-state-summary renamed-merge 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/false (proof-state-finished? renamed-merge 0 0))
       (test/false (proof-state-finished? renamed-merge 0 1))
       (test/false (proof-state-finished? renamed-merge 0 2)))
     (test-suite "proof-state-successful?"
       (test/error (proof-state-successful? renamed-merge 0 0))
       (test/error (proof-state-successful? renamed-merge 0 1))
       (test/error (proof-state-successful? renamed-merge 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input renamed-merge 0 0))
       (test/equal? (proof-state-input renamed-merge 0 1) "(+ 1 2)")
       (test/equal? (proof-state-input renamed-merge 0 2) "(* 3 4)")))

   (test-suite "Reordered Merge"
     (test-case "proof-state-size"
       (test/equal? (proof-state-size replaced-merge) 2))
     (test-case "proof-state-current-part"
       (test/equal? (proof-state-current-part reordered-merge) 0))
     (test-suite "proof-state-part-size"
       (test/equal? (proof-state-part-size reordered-merge 0) 3)
       (test/equal? (proof-state-part-size reordered-merge 1) 3))
     (test-suite "proof-state-position"
       (test/equal? (proof-state-position reordered-merge 1) 0)
       (test/equal? (proof-state-position reordered-merge 0) 0))
     (test-suite "proof-state-output"
       (test/equal? (proof-state-output reordered-merge 1 0) "")
       (test/equal? (proof-state-output reordered-merge 1 1) "")
       (test/equal? (proof-state-output reordered-merge 1 2) "")
       (test/equal? (proof-state-output reordered-merge 0 0) "")
       (test/equal? (proof-state-output reordered-merge 0 1) "")
       (test/equal? (proof-state-output reordered-merge 0 2) ""))
     (test-suite "proof-state-summary"
       (test/equal? (proof-state-summary reordered-merge 1 0) "")
       (test/equal? (proof-state-summary reordered-merge 1 1) "")
       (test/equal? (proof-state-summary reordered-merge 1 2) "")
       (test/equal? (proof-state-summary reordered-merge 0 0) "")
       (test/equal? (proof-state-summary reordered-merge 0 2) "")
       (test/equal? (proof-state-summary reordered-merge 0 1) ""))
     (test-suite "proof-state-finished?"
       (test/false (proof-state-finished? reordered-merge 1 0))
       (test/false (proof-state-finished? reordered-merge 1 1))
       (test/false (proof-state-finished? reordered-merge 1 2))
       (test/false (proof-state-finished? reordered-merge 0 0))
       (test/false (proof-state-finished? reordered-merge 0 1))
       (test/false (proof-state-finished? reordered-merge 0 2)))
     (test-suite "proof-state-successful?"
       (test/error (proof-state-successful? reordered-merge 1 0))
       (test/error (proof-state-successful? reordered-merge 1 1))
       (test/error (proof-state-successful? reordered-merge 1 2))
       (test/error (proof-state-successful? reordered-merge 0 0))
       (test/error (proof-state-successful? reordered-merge 0 1))
       (test/error (proof-state-successful? reordered-merge 0 2)))
     (test-suite "proof-state-input"
       (test/error (proof-state-input reordered-merge 1 0))
       (test/equal? (proof-state-input reordered-merge 1 1)
                    "(list (quote a) (quote b))")
       (test/equal? (proof-state-input reordered-merge 1 2)
                    "(quote (c d))")
       (test/error (proof-state-input reordered-merge 0 0))
       (test/equal? (proof-state-input reordered-merge 0 1) "(+ 1 2)")
       (test/equal? (proof-state-input reordered-merge 0 2) "(* 3 4)")))
   |#
   ))

(provide state-test-suite)
