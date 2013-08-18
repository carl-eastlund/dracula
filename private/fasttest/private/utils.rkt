#lang racket

(require
  "../../planet.rkt"
  (cce scheme))

(define-planet-package random schematics/random:1:0)

(provide cce random)
