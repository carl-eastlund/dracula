#lang racket

(require "constants.rkt")
(provide (all-defined-out))

(define (nil? x)
  (or (eq? x '()) (eq? x 'nil)))

(define (acl2:not x)
  (if (nil? x) t nil))

(define (not-nil x)
  (not (nil? x)))
