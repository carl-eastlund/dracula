#lang racket

(require "../acl2/parse.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Helper Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; split-all : String -> [Listof String]
;; Produces every character in a string.
(define (split-all str)
  (map string (string->list str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sample Output
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prompt "ACL2 !>")
(define prompt-block (make-prompt-block prompt))

(define prompt-no-! "ACL2 >")
(define prompt-no-!-block (make-prompt-block prompt-no-!))

(define tree-prefix "#<\\<0")
(define tree-suffix "#>\\>")

;; One possible ACL2 preamble.
(define preamble
  (string-append
   "Welcome to Clozure Common Lisp Version 1.2-r9226-RC1  (DarwinX8664)!\n\n"
   " ACL2 Version 3.3 built July 5, 2008  17:20:55.\n Copyright (C) 2007"
   "  University of Texas at Austin\n ACL2 comes with ABSOLUTELY NO WARRANTY."
   "  This is free software and you\n are welcome to redistribute it under"
   " certain conditions.  For details,\n see the GNU General Public License."
   "\n\n Initialized with (INITIALIZE-ACL2 'INCLUDE-BOOK *ACL2-PASS-2-FILES*)."
   "\n See the documentation topic note-3-3 for recent changes.\n"
   " Note: We have modified the prompt in some underlying Lisps to further\n"
   " distinguish it from the ACL2 prompt.\n\n NOTE!!  Proof trees are disabled"
   " in ACL2.  To enable them in emacs,\n look under the ACL2 source directory"
   " in interface/emacs/README.doc; \n and, to turn on proof trees, execute :"
   "START-PROOF-TREE in the ACL2 \n command loop.   Look in the ACL2"
   " documentation under PROOF-TREE.\n\nACL2 Version 3.3.  Level 1."
   "  Cbd \"/Users/sky/current/acl2/wrapper/\".\nDistributed books directory"
   " \"/Users/sky/acl2-sources/books/\".\nType :help for help.\n"
   "Type (good-bye) to quit completely out of ACL2.\n\n"))
(define preamble+prompt (string-append preamble prompt))

(define preamble/parsed
  (make-parse-state prompt-block (list (make-text-block preamble))))

;; The results of "(defthm x=x (equal x x) :rule-classes nil)\n",
;; without proof trees.
(define x=x
  (string-append
   "\nBut we reduce the conjecture to T, by primitive type reasoning.\n\n"
   "Q.E.D.\n\nSummary\nForm:  ( DEFTHM X=X ...)\nRules: "
   "((:FAKE-RUNE-FOR-TYPE-SET NIL))\nWarnings:  None\nTime:  0.00 seconds"
   " (prove: 0.00, print: 0.00, other: 0.00)\n X=X\n"))
(define x=x+prompt (string-append x=x prompt))

(define x=x/parsed
  (make-parse-state prompt-block (list (make-text-block x=x))))

;; The results of ":u\n", after the x=x theorem above.
(define undo "          0:x(EXIT-BOOT-STRAP-MODE)\n")
(define undo+prompt (string-append undo prompt))

(define undo/parsed
  (make-parse-state prompt-block (list (make-text-block undo))))

;; The results of ":start-proof-tree\n".
(define start-proof-tree
  (string-append
   "\nProof tree output is now enabled.  Note that :START-PROOF-TREE works\n"
   "by removing 'proof-tree from the inhibit-output-lst; see :DOC "
   "set-\ninhibit-output-lst.\n"))
(define start-proof-tree+prompt (string-append start-proof-tree prompt))

(define start-proof-tree/parsed
  (make-parse-state
   prompt-block (list (make-text-block start-proof-tree))))

;; The results of "(defthm x=x (equal x x) :rule-classes nil)\n",
;; with proof trees.

(define x=x/before
  "\n<< Starting proof tree logging >>\n")

(define x=x/tree
  "( DEFTHM X=X ...)\nQ.E.D.")

(define x=x/after
  (string-append
   "\nBut we reduce the conjecture to T, by primitive type reasoning.\n\n"
   "Q.E.D.\n\nSummary\nForm:  ( DEFTHM X=X ...)\nRules: "
   "((:FAKE-RUNE-FOR-TYPE-SET NIL))\nWarnings:  None\nTime:  0.00 seconds"
   " (prove: 0.00, print: 0.00, proof tree: 0.00, other: 0.00)\n X=X\n"))

(define x=x+trees
  (string-append x=x/before tree-prefix x=x/tree tree-suffix x=x/after))
(define x=x+trees+prompt (string-append x=x+trees prompt))

(define x=x+trees/parsed
  (make-parse-state
   prompt-block
   (list (make-text-block x=x/after)
         (make-tree-block x=x/tree)
         (make-text-block x=x/before))))

;; The results of "(set-guard-checking nil)".

(define guards-off
  (string-append
   "\nMasking guard violations but still checking guards except for"
   " self-\nrecursive calls.  To avoid guard checking entirely, "
   ":SET-GUARD-CHECKING\n:NONE.  See :DOC set-guard-checking.\n\n"))
(define guards-off+prompt-no-! (string-append guards-off prompt-no-!))

(define guards-off/parsed
  (make-parse-state prompt-no-!-block (list (make-text-block guards-off))))

;; The results of "(defthm bad (< x 0))\n"

(define incorrect/before "\n<< Starting proof tree logging >>")

(define incorrect/tree1
  "( DEFTHM BAD ...)\nc  0 Goal PUSH *1\n")

(define incorrect/middle
  (string-append
   "\nName the formula above *1.\n\nNo induction rackets are suggested by *1."
   "  Consequently, the proof\nattempt has failed.\n\nSummary\nForm:"
   "  ( DEFTHM BAD ...)\nRules: NIL\nWarnings:  None\nTime:  0.00 seconds"
   " (prove: 0.00, print: 0.00, proof tree: 0.00, other: 0.00)\n\n---\n"
   "The key checkpoint goal, below, may help you to debug this failure.\n"
   "See :DOC failure and see :DOC set-checkpoint-summary-limit.\n---\n\n*** "
   "Key checkpoint at the top level: ***\n\nGoal\n(< X 0)\n\n"
   "******** FAILED ********  See :DOC failure  ******** FAILED ********\n"))

(define incorrect/tree2
  (string-append
   "( DEFTHM BAD ...)\n******** FAILED ********  See :DOC failure  "
   "******** FAILED ********\nc  0 Goal PUSH *1\n"))

(define incorrect+trees
  (string-append incorrect/before
                 tree-prefix
                 incorrect/tree1
                 tree-suffix
                 incorrect/middle
                 tree-prefix
                 incorrect/tree2
                 tree-suffix))
(define incorrect+trees+prompt (string-append incorrect+trees prompt))

(define incorrect+trees/parsed
  (make-parse-state
   prompt-block
   (list (make-text-block "")
         (make-tree-block incorrect/tree2)
         (make-text-block incorrect/middle)
         (make-tree-block incorrect/tree1)
         (make-text-block incorrect/before))))

(define incorrect/incomplete/parsed
  (make-parse-state
   (make-partial-block "" (make-text-block ""))
   (list (make-tree-block incorrect/tree2)
         (make-text-block incorrect/middle)
         (make-tree-block incorrect/tree1)
         (make-text-block incorrect/before))))

;; result of sending "(+ 1 x)\n"
(define unbound-variable
  (string-append
   "\n\nACL2 Error in TOP-LEVEL:  Global variables, such as X, are not"
   " allowed.\nSee :DOC ASSIGN and :DOC @.\n\n"))
(define unbound-variable+prompt (string-append unbound-variable prompt))

(define unbound-variable/parsed
  (make-parse-state
   prompt-block (list (make-text-block unbound-variable))))

;; Result of sending the following three theorems:
#|
(defthm app/associative
  (equal (append (append x y) z)
         (append x (append y z))))

(defthm app/associative2
  (equal (append x (append y z))
         (append (append x y) z)))

(defthm app/associotive-len
  (= (len (append (append x y) z))
     (len (append x (append y z)))))
|#

(define app/associative
  (string-append
   "\nName the formula above *1.\n"
   "\nPerhaps we can prove *1 by induction.  Three induction rackets are\n"
   "suggested by this conjecture.  Subsumption reduces that number to two.\n"
   "However, one of these is flawed and so we are left with one viable\n"
   "candidate.  \n\nWe will induct according to a racket suggested by"
   " (BINARY-APPEND X Y).\nThis suggestion was produced using the :induction"
   " rule BINARY-APPEND.\nIf we let (:P X Y Z) denote *1 above then the"
   " induction racket we'll\nuse is\n(AND (IMPLIES (AND (NOT (ENDP X)) "
   "(:P (CDR X) Y Z))\n              (:P X Y Z))\n     (IMPLIES (ENDP X) "
   "(:P X Y Z))).\nThis induction is justified by the same argument used to"
   " admit BINARY-\nAPPEND.  When applied to the goal at hand the above"
   " induction racket\nproduces two nontautological subgoals.\n\n"
   "Subgoal *1/2\n(IMPLIES (AND (NOT (ENDP X))\n              "
   "(EQUAL (APPEND (APPEND (CDR X) Y) Z)\n                     "
   "(APPEND (CDR X) Y Z)))\n         (EQUAL (APPEND (APPEND X Y) Z)\n"
   "                (APPEND X Y Z))).\n\nBy the simple :definition ENDP we"
   " reduce the conjecture to\n\nSubgoal *1/2'\n(IMPLIES (AND (CONSP X)\n"
   "              (EQUAL (APPEND (APPEND (CDR X) Y) Z)\n                     "
   "(APPEND (CDR X) Y Z)))\n         (EQUAL (APPEND (APPEND X Y) Z)\n"
   "                (APPEND X Y Z))).\n\nBut simplification reduces this to T,"
   " using the :definition BINARY-\nAPPEND, primitive type reasoning and the"
   " :rewrite rules CAR-CONS and\nCDR-CONS.\n\nSubgoal *1/1\n"
   "(IMPLIES (ENDP X)\n         (EQUAL (APPEND (APPEND X Y) Z)\n"
   "                (APPEND X Y Z))).\n\nBy the simple :definition ENDP we"
   " reduce the conjecture to\n\nSubgoal *1/1'\n(IMPLIES (NOT (CONSP X))\n"
   "         (EQUAL (APPEND (APPEND X Y) Z)\n                (APPEND X Y Z)))."
   "\n\nBut simplification reduces this to T, using the :definition BINARY-\n"
   "APPEND and primitive type reasoning.\n\nThat completes the proof of *1.\n\n"
   "Q.E.D.\n\nSummary\nForm:  ( DEFTHM APP/ASSOCIATIVE ...)\nRules:"
   " ((:DEFINITION BINARY-APPEND)\n        (:DEFINITION ENDP)\n"
   "        (:DEFINITION NOT)\n        (:FAKE-RUNE-FOR-TYPE-SET NIL)\n"
   "        (:INDUCTION BINARY-APPEND)\n        (:REWRITE CAR-CONS)\n"
   "        (:REWRITE CDR-CONS))\nWarnings:  None\nTime:  0.01 seconds"
   " (prove: 0.00, print: 0.01, other: 0.00)\n APP/ASSOCIATIVE\n"))
(define app/associative+prompt (string-append app/associative prompt))

(define app/associative2
  (string-append
   "\nBut we reduce the conjecture to T, by the simple :rewrite rule"
   " APP/ASSOCIATIVE\\\n.\n\nQ.E.D.\n\nSummary\nForm:  "
   "( DEFTHM APP/ASSOCIATIVE2 ...)\nRules: ((:REWRITE APP/ASSOCIATIVE))\n"
   "Warnings:  None\nTime:  0.00 seconds "
   "(prove: 0.00, print: 0.00, other: 0.00)\n"
   " APP/ASSOCIATIVE2\n"))
(define app/associative2+prompt (string-append app/associative2 prompt))

(define app/associative-len
  (string-append
   "\n\n"
   "HARD ACL2 ERROR in PREPROCESS:  The call depth limit of 1000 has been\n"
   "exceeded in the ACL2 preprocessor (a sort of rewriter)."
   "  There is probably\na loop caused by some set of enabled simple rules."
   "  To see why the\nlimit was exceeded, execute\n  :brr t\nand next retry"
   " the proof with :hints\n  :do-not '(preprocess)\nand then follow the"
   " directions in the resulting error message.  See\n:DOC rewrite-stack-limit."
   "\n\n\n\nACL2 Error in TOP-LEVEL:  Evaluation aborted.  See :DOC wet"
   " for how\nyou might be able to get an error backtrace.\n\n"))
(define app/associative-len+prompt (string-append app/associative-len prompt))

(define app/associative-len/parsed
  (make-parse-state
   prompt-block
   (list (make-text-block app/associative-len))))



;; The result of sending app/associative from above, with proof trees:
(define app/associative/before
  (string-append
   "\n<< Starting proof tree logging >>\n"))
(define app/associative/tree1
  (string-append
   "( DEFTHM APP/ASSOCIATIVE ...)\nc  0 Goal PUSH *1\n\n"))
(define app/associative/body1
  (string-append
   "Name the formula above *1.\n\nPerhaps we can prove *1 by induction."
   "  Three induction rackets are\nsuggested by this conjecture.  Subsumption"
   " reduces that number to two.\nHowever, one of these is flawed and so we"
   " are left with one viable\ncandidate.  \n\nWe will induct according to a"
   " racket suggested by (BINARY-APPEND X Y).\nThis suggestion was produced"
   " using the :induction rule BINARY-APPEND.\nIf we let (:P X Y Z) denote *1"
   " above then the induction racket we'll\nuse is\n"
   "(AND (IMPLIES (AND (NOT (ENDP X)) (:P (CDR X) Y Z))\n"
   "              (:P X Y Z))\n     (IMPLIES (ENDP X) (:P X Y Z))).\n"
   "This induction is justified by the same argument used to admit BINARY-\n"
   "APPEND.  When applied to the goal at hand the above induction racket\n"
   "produces two nontautological subgoals.\n\nSubgoal *1/2\n"
   "(IMPLIES (AND (NOT (ENDP X))\n              "
   "(EQUAL (APPEND (APPEND (CDR X) Y) Z)\n                     "
   "(APPEND (CDR X) Y Z)))\n         (EQUAL (APPEND (APPEND X Y) Z)\n"
   "                (APPEND X Y Z))).\n"))
(define app/associative/tree2
  (string-append
   "( DEFTHM APP/ASSOCIATIVE ...)\nc  0 Goal PUSH *1\n"
   "++++++++++++++++++++++++++++++\nc  2 *1 INDUCT\n"
   "   1 |  Subgoal *1/2 preprocess\n     |  |  <1 subgoal>\n"
   "     |  <1 more subgoal>\n\n"))
(define app/associative/body2
  (string-append
   "By the simple :definition ENDP we reduce the conjecture to\n\n"
   "Subgoal *1/2'\n(IMPLIES (AND (CONSP X)\n              "
   "(EQUAL (APPEND (APPEND (CDR X) Y) Z)\n                     "
   "(APPEND (CDR X) Y Z)))\n         (EQUAL (APPEND (APPEND X Y) Z)\n"
   "                (APPEND X Y Z))).\n"))
(define app/associative/tree3
  (string-append
   "( DEFTHM APP/ASSOCIATIVE ...)\nc  0 Goal PUSH *1\n"
   "++++++++++++++++++++++++++++++\nc  2 *1 INDUCT\n"
   "     |  <1 more subgoal>\n\n"))
(define app/associative/body3
  (string-append
   "But simplification reduces this to T, using the :definition BINARY-\n"
   "APPEND, primitive type reasoning and the :rewrite rules CAR-CONS and\n"
   "CDR-CONS.\n\nSubgoal *1/1\n(IMPLIES (ENDP X)\n         "
   "(EQUAL (APPEND (APPEND X Y) Z)\n                (APPEND X Y Z))).\n"))
(define app/associative/tree4
  (string-append
   "( DEFTHM APP/ASSOCIATIVE ...)\nc  0 Goal PUSH *1\n"
   "++++++++++++++++++++++++++++++\nc  2 *1 INDUCT\n"
   "   1 |  Subgoal *1/1 preprocess\n     |  |  <1 subgoal>\n\n"))
(define app/associative/body4
  (string-append
   "By the simple :definition ENDP we reduce the conjecture to\n\n"
   "Subgoal *1/1'\n(IMPLIES (NOT (CONSP X))\n         "
   "(EQUAL (APPEND (APPEND X Y) Z)\n                (APPEND X Y Z))).\n"))
(define app/associative/tree5
  (string-append
   "( DEFTHM APP/ASSOCIATIVE ...)\nQ.E.D.\n"))
(define app/associative/conclusion
  (string-append
   "But simplification reduces this to T, using the :definition BINARY-\n"
   "APPEND and primitive type reasoning.\n\nThat completes the proof of *1.\n\n"
   "Q.E.D.\n\nSummary\nForm:  ( DEFTHM APP/ASSOCIATIVE ...)\n"
   "Rules: ((:DEFINITION BINARY-APPEND)\n        (:DEFINITION ENDP)\n"
   "        (:DEFINITION NOT)\n        (:FAKE-RUNE-FOR-TYPE-SET NIL)\n"
   "        (:INDUCTION BINARY-APPEND)\n        (:REWRITE CAR-CONS)\n"
   "        (:REWRITE CDR-CONS))\nWarnings:  None\nTime:  0.01 seconds"
   " (prove: 0.00, print: 0.00, proof tree: 0.00, other: 0.00)\n"
   " APP/ASSOCIATIVE\n"))

(define app/associative+trees
  (string-append app/associative/before 
                 tree-prefix app/associative/tree1 tree-suffix
                 app/associative/body1 
                 tree-prefix app/associative/tree2 tree-suffix
                 app/associative/body2 
                 tree-prefix app/associative/tree3 tree-suffix
                 app/associative/body3 
                 tree-prefix app/associative/tree4 tree-suffix
                 app/associative/body4 
                 tree-prefix app/associative/tree5 tree-suffix
                 app/associative/conclusion))
(define app/associative+trees+prompt
  (string-append app/associative+trees prompt))

(define app/associative+trees/parsed
  (make-parse-state
   prompt-block
   (list (make-text-block app/associative/conclusion)
         (make-tree-block app/associative/tree5)
         (make-text-block app/associative/body4)
         (make-tree-block app/associative/tree4)
         (make-text-block app/associative/body3)
         (make-tree-block app/associative/tree3)
         (make-text-block app/associative/body2)
         (make-tree-block app/associative/tree2)
         (make-text-block app/associative/body1)
         (make-tree-block app/associative/tree1)
         (make-text-block app/associative/before))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Proof Trees
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define one-subgoal-tree
  "( DEFTHM APP/ASSOCIATIVE ...)\nc  0 Goal PUSH *1\n\n")
(define one-subgoal-goals
  (list (list "Goal" 30 47)))

(define three-subgoal-tree
  (string-append
   "( DEFTHM APP/ASSOCIATIVE ...)\n"
   "c  0 Goal PUSH *1\n"
   "++++++++++++++++++++++++++++++\n"
   "c  2 *1 INDUCT\n"
   "   1 |  Subgoal *1/2 preprocess\n"
   "     |  |  <1 subgoal>\n"
   "     |  <1 more subgoal>\n\n"))
(define three-subgoal-goals
  (list (list "Goal" 30 47)
        (list "*1" 79 93)
        (list "Subgoal *1/2" 94 125)))

(define qed-tree
  "( DEFTHM APP/ASSOCIATIVE ...)\nQ.E.D.\n")

(define failed-tree1
  (string-append
   "( DEFTHM BAD ...)\n"
   "******** FAILED ********  See :DOC failure  ******** FAILED ********\n"
   "c  0 Goal PUSH *1\n"))
(define failed1-goals
  (list (list "Goal" 87 104)))

(define failed-tree2
  (string-append
   "( DEFTHM STEP-PLAYERS-PRESERVES-NON-DIAGONAL ...)\n"
   "******** FAILED ********  See :DOC failure  ******** FAILED ********\n"
   "   1 Goal preprocess\n"
   "c  2 |  Goal' ELIM\n"
   "   1 |  |  Subgoal 1 preprocess\n"
   "c  0 |  |  |  Subgoal 1' PUSH (reverting)\n"))
(define failed2-goals
  (list (list "Goal" 119 139)
        (list "Goal'" 140 158)
        (list "Subgoal 1" 159 190)
        (list "Subgoal 1'" 191 232)))
