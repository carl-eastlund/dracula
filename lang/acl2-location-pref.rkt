#lang racket
(require framework)
(provide (all-defined-out))

(define *acl2-loc-key* 'acl2-executable-location)
(preferences:set-default *acl2-loc-key* (build-path "~") path?)
(preferences:set-un/marshall
  *acl2-loc-key*
  path->string
  build-path)

(define (get-acl2-location) (preferences:get *acl2-loc-key*))
