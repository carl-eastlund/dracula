#lang mischief

(provide
  module-name~>obligations
  module-name~>consequences
  dependency-queue
  identifier~>type)

(define module-name~>obligations (make-hash))
(define module-name~>consequences (make-hash))
(define dependency-queue (make-queue))
(define identifier~>type (make-free-id-table))
