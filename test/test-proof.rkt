#lang racket

(require "../private/planet.rkt"
         "../proof/proof.rkt"
         "data-proof.rkt")
(require rackunit)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Test Suite
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define proof-test-suite
   (test-suite "ACL2 Proof Representation"
     (test-suite "Locations"
       (test-suite "0:7"
         (test/equal? (loc-source loc-0:7) "here")
         (test/equal? (loc-start loc-0:7) 0)
         (test/equal? (loc-end loc-0:7) 7))
       (test-suite "8:15"
         (test/equal? (loc-source loc-8:15) "here")
         (test/equal? (loc-start loc-8:15) 8)
         (test/equal? (loc-end loc-8:15) 15))
       (test-suite "0:15"
         (test/equal? (loc-source loc-0:15) "here")
         (test/equal? (loc-start loc-0:15) 0)
         (test/equal? (loc-end loc-0:15) 15))
       (test-suite "16:28"
         (test/equal? (loc-source loc-16:28) "here")
         (test/equal? (loc-start loc-16:28) 16)
         (test/equal? (loc-end loc-16:28) 28))
       (test-suite "29:35"
         (test/equal? (loc-source loc-29:35) "here")
         (test/equal? (loc-start loc-29:35) 29)
         (test/equal? (loc-end loc-29:35) 35))
       (test-suite "16:35"
         (test/equal? (loc-source loc-16:35) "here")
         (test/equal? (loc-start loc-16:35) 16)
         (test/equal? (loc-end loc-16:35) 35)))
     (test-suite "Terms"
       (test-suite "(+ 1 2)"
         (test/equal? (term-sexp term-1+2) '(+ 1 2))
         (test/equal? (term-loc term-1+2) loc-0:7))
       (test-suite "(* 3 4)"
         (test/equal? (term-sexp term-3*4) '(* 3 4))
         (test/equal? (term-loc term-3*4) loc-8:15))
       (test-suite "(list 'a 'b)"
         (test/equal? (term-sexp term-list-a-b) '(list (quote a) (quote b)))
         (test/equal? (term-loc term-list-a-b) loc-16:28))
       (test-suite "'(c d)"
         (test/equal? (term-sexp term-quote-c-d) '(quote (c d)))
         (test/equal? (term-loc term-quote-c-d) loc-29:35)))
     (test-suite "Parts"
       (test-suite "Main: (+ 1 2) (* 3 4)"
         (test/equal? (part-name part-1234) 'Main)
         (test/equal? (part-loc part-1234) loc-0:15)
         (test/equal? (part-length part-1234) 2)
         (test/equal? (part-nth part-1234 0) term-1+2)
         (test/equal? (part-nth part-1234 1) term-3*4))
       (test-suite "Helper: (list 'a 'b) '(c d)"
         (test/equal? (part-name part-abcd) 'Helper)
         (test/equal? (part-loc part-abcd) loc-16:35)
         (test/equal? (part-length part-abcd) 2)
         (test/equal? (part-nth part-abcd 0) term-list-a-b)
         (test/equal? (part-nth part-abcd 1) term-quote-c-d)))
     (test-suite "Proof"
       (test-suite "Main + Helper"
         (test/equal? (proof-size proof-main+helper) 2)
         (test/equal? (proof-nth proof-main+helper 0) part-abcd)
         (test/equal? (proof-nth proof-main+helper 1) part-1234)
         (test/equal? (proof-parts proof-main+helper) '(Helper Main))
         (test/equal? (proof-part proof-main+helper 'Helper) part-abcd)
         (test/equal? (proof-part proof-main+helper 'Main) part-1234)))))

(provide proof-test-suite)
