#lang racket

(require "data-proof.rkt"
         "../drscheme/dracula-state.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sample Text
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define initial-text
  (string-append
   "Welcome to Clozure Common Lisp Version 1.2-r9226-RC1  (DarwinX8664)!\n"
   "\n"
   " ACL2 Version 3.3 built July 5, 2008  17:20:55.\n"
   " Copyright (C) 2007  University of Texas at Austin\n"
   " ACL2 comes with ABSOLUTELY NO WARRANTY.  This is free software and you\n"
   " are welcome to redistribute it under certain conditions.  For details,\n"
   " see the GNU General Public License.\n"
   "\n"
   " Initialized with (INITIALIZE-ACL2 'INCLUDE-BOOK *ACL2-PASS-2-FILES*).\n"
   " See the documentation topic note-3-3 for recent changes.\n"
   " Note: We have modified the prompt in some underlying Lisps to further\n"
   " distinguish it from the ACL2 prompt.\n"
   "\n"
   " NOTE!!  Proof trees are disabled in ACL2.  To enable them in emacs,\n"
   " look under the ACL2 source directory in interface/emacs/README.doc; \n"
   " and, to turn on proof trees, execute :START-PROOF-TREE in the ACL2 \n"
   " command loop.   Look in the ACL2 documentation under PROOF-TREE.\n"
   "\n"
   "ACL2 Version 3.3.  Level 1.  Cbd \"/Users/sky/\".\n"
   "Distributed books directory \"/Users/sky/acl2-sources/books/\".\n"
   "Type :help for help.\n"
   "Type (good-bye) to quit completely out of ACL2.\n"
   "\n"
   "ACL2 !>"))

(define three-text "3\nACL2 !>")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Sample States
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; proof-main+helper from data-proof.ss:

(define merged-state
  (dracula-state-populate initial-dracula-state proof-main+helper))

(define initial-with-helper
  (dracula-state-populate initial-dracula-state proof-helper))

(define unchanged-merge
  (dracula-state-populate initial-with-helper proof-helper))

(define replaced-merge
  (dracula-state-populate initial-with-helper proof-main))

(define initial-with-main
  (dracula-state-populate initial-dracula-state proof-main))

(define renamed-merge
  (dracula-state-populate initial-with-main proof-main/renamed))

(define reordered-merge
  (dracula-state-populate merged-state proof-main+helper/reordered))

#|

ACL2 connection not currently easily testable:

(define select-state
  (dracula-state-choose merged-state 1))

(define initial-prompt
  (dracula-state-update-acl2 select-state 1 initial-text))

(define start-a-term
  (dracula-state-advance initial-prompt 1))

(define admit-a-term
  (dracula-state-update-acl2 start-a-term 1 three-text))
|#
