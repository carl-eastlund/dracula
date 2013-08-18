#lang racket

#|
Keep track of the current package name.  Useful for the ACL2 book -> module
compiler.  That effort has been suspended, though.
|#

(provide in-package)

(define-syntax (in-package stx)
  (syntax-case stx ()
    [(_ pkg) #'(begin)]))

