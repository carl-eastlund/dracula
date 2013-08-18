#lang racket
(require framework)

(provide get-admit-before-run?
  set-admit-before-run?)

(define *admit-before-run*
  'admit-before-run-acl2-pref)

(preferences:set-default *admit-before-run* #f boolean?)

(define (get-admit-before-run?)
  (preferences:get *admit-before-run*))

(define (set-admit-before-run? value)
  (preferences:set *admit-before-run* value))

