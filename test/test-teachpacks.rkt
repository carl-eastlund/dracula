#lang racket

(require
 "../private/planet.rkt"
 (lib "unit.rkt")
 (prefix-in audio: "../teachpacks/audio.rkt")
 (prefix-in avl-rational-keys: "../teachpacks/avl-rational-keys.rkt")
 (prefix-in binary-io: "../teachpacks/binary-io-utilities.rkt")
 (prefix-in io: "../teachpacks/io-utilities.rkt")
 (prefix-in list: "../teachpacks/list-utilities.rkt")
 (prefix-in testing: "../teachpacks/testing.rkt")
 (prefix-in world: "../teachpacks/world.rkt")
 (prefix-in doublecheck: "../teachpacks/doublecheck.rkt"))
(require rackunit)

(provide test-teachpacks)

(define test-teachpacks
  (test-suite "Teachpacks"
    (test-case "audio")
    (test-case "avl-rational-keys")
    (test-case "binary-io-utilities")
    (test-case "io-utilities")
    (test-case "list-utilities")
    (test-case "testing")
    (test-case "world")
    (test-case "doublecheck")))
