#lang racket

(require (prefix-in mz: racket/base)
         "../lang/dracula.rkt"
         "../lang/check.rkt")

(provide (all-defined-out))

(begin-below

 (defconst *M31* 2147483647)
 (defconst *P1* 16807)

 (defun seedp (x) (mz:if (integer? x) t nil))

 (defun initial-seed () 1382728371)

 (defun next-seed (seed)
   (remainder (* *P1* seed) *M31*))

 (defun rand (max seed)
   (remainder seed max))

 )
