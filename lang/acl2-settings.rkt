#lang racket

#|
Extends drscheme:language:simple-settings with some ACL2 specific settings.
 (file "acl2-language-unit.rkt") imports these and uses them to implement
drscheme:language:module-based-language<%>.
|#

(require
  mzlib/struct
  "acl2-location-pref.rkt"
  "admit-before-run-pref.rkt")
(provide (struct-out acl2-settings)
  marshall-acl2-settings
  default-acl2-settings
  unmarshall-acl2-settings)

(define-struct acl2-settings (acl2-loc admit-before-run?))

(define acl2-settings->vector (make-->vector acl2-settings))

(define marshall-acl2-settings acl2-settings->vector)

(define default-acl2-settings 
  (make-acl2-settings 
    (path->string (get-acl2-location))
    (get-admit-before-run?)))

(define unmarshall-acl2-settings
  (lambda (v)
    (make-acl2-settings (vector-ref v 0) (vector-ref v 1))))
