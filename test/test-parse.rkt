#lang racket

(require "../private/planet.rkt"
         "../acl2/parse.rkt"
         "data-parse.rkt")
(require rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Tests
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parse-test-suite
   (test-suite "Parsing"
     (test-suite "Block Structure"
       (test-suite "Preamble"
         (test-case "whole"
           (check-equal? (parse preamble+prompt empty-parse-state)
                         preamble/parsed))
         (test-case "separate"
           (check-equal? (foldl parse
                                empty-parse-state
                                (split-all preamble+prompt))
                         preamble/parsed)))
       (test-suite "Theorem"
         (test-case "whole"
           (check-equal? (parse x=x+prompt empty-parse-state) x=x/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state (split-all x=x+prompt))
                         x=x/parsed)))
       (test-suite "Undo"
         (test-case "whole"
           (check-equal? (parse undo+prompt empty-parse-state) undo/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state (split-all undo+prompt))
                         undo/parsed)))
       (test-suite "Start Proof Trees"
         (test-case "whole"
           (check-equal? (parse start-proof-tree+prompt empty-parse-state)
                         start-proof-tree/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state
                                (split-all start-proof-tree+prompt))
                         start-proof-tree/parsed)))
       (test-suite "Theorem+Trees"
         (test-case "whole"
           (check-equal? (parse x=x+trees+prompt empty-parse-state)
                         x=x+trees/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state
                                (split-all x=x+trees+prompt))
                         x=x+trees/parsed)))
       (test-suite "Guards Off"
         (test-case "whole"
           (check-equal? (parse guards-off+prompt-no-! empty-parse-state)
                         guards-off/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state
                                (split-all guards-off+prompt-no-!))
                         guards-off/parsed)))
       (test-suite "Failing Theorem"
         (test-case "whole"
           (check-equal? (parse incorrect+trees+prompt empty-parse-state)
                         incorrect+trees/parsed))
         (test-case "partial"
           (check-equal? (foldl parse empty-parse-state
                                (split-all incorrect+trees+prompt))
                         incorrect+trees/parsed)))
       (test-suite "Bad Expression"
         (test-case "whole"
           (check-equal? (parse unbound-variable+prompt empty-parse-state)
                         unbound-variable/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state
                                (split-all unbound-variable+prompt))
                         unbound-variable/parsed)))
       (test-suite "Hard Error"
         (test-case "whole"
           (check-equal? (parse app/associative-len+prompt empty-parse-state)
                         app/associative-len/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state
                                (split-all app/associative-len+prompt))
                         app/associative-len/parsed)))
       (test-suite "Long Output"
         (test-case "whole"
           (check-equal? (parse app/associative+trees+prompt empty-parse-state)
                         app/associative+trees/parsed))
         (test-case "separate"
           (check-equal? (foldl parse empty-parse-state
                                (split-all app/associative+trees+prompt))
                         app/associative+trees/parsed))))
     (test-suite "Incomplete Parsings"
       (test-equal? "incorrect theorem"
                    (parse incorrect+trees empty-parse-state)
                    incorrect/incomplete/parsed))

     (test-suite "Success/Failure"
       (test-case "Preamble" (check-true (parse-success? preamble/parsed)))
       (test-case "Theorem" (check-true (parse-success? x=x/parsed)))
       (test-case "Undo" (check-true (parse-success? undo/parsed)))
       (test-case "Start Proof Trees"
         (check-true (parse-success? start-proof-tree/parsed)))
       (test-case "Theorem+Trees"
         (check-true (parse-success? x=x+trees/parsed)))
       (test-case "Guards Off" (check-true (parse-success? guards-off/parsed)))
       (test-case "Failing Theorem"
         (check-false (parse-success? incorrect+trees/parsed)))
       (test-case "Bad Expression"
         (check-false (parse-success? unbound-variable/parsed)))
       (test-case "Hard Error"
         (check-false (parse-success? app/associative-len/parsed)))
       (test-case "Long Output"
         (check-true (parse-success? app/associative+trees/parsed))))

     (test-suite "Proof Tree Goals"
       (test-case "Not a tree"
         (check-equal? (parse-subgoals "No trees here.") (list)))
       (test-case "One Goal"
         (check-equal? (parse-subgoals one-subgoal-tree) one-subgoal-goals))
       (test-case "Three Goals"
         (check-equal? (parse-subgoals three-subgoal-tree) three-subgoal-goals))
       (test-case "Q.E.D"
         (check-equal? (parse-subgoals qed-tree) (list)))
       (test-case "Failed Theorem 1"
         (check-equal? (parse-subgoals failed-tree1) failed1-goals))
       (test-case "Failed Theorem 2"
         (check-equal? (parse-subgoals failed-tree2) failed2-goals)))

     (test-suite "Finished"
       (test-suite "finished states"
         (test-true "preamble" (parse-finished? preamble/parsed))
         (test-true "x=x" (parse-finished? x=x/parsed))
         (test-true "undo" (parse-finished? undo/parsed))
         (test-true "x=x+trees" (parse-finished? x=x+trees/parsed))
         (test-true "start-proof-tree"
                    (parse-finished? start-proof-tree/parsed))
         (test-true "guards-off" (parse-finished? guards-off/parsed))
         (test-true "incorrect thm" (parse-finished? incorrect+trees/parsed))
         (test-true "unbound variable"
                    (parse-finished? unbound-variable/parsed))
         (test-true "app/associative"
                    (parse-finished? app/associative+trees/parsed)))
       (test-suite "unfinished states"
         (test-false "incorrect (incompletely parsed)"
                     (parse-finished? incorrect/incomplete/parsed))))
     ))

(provide parse-test-suite)