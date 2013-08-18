#lang racket

(require "../private/planet.rkt"
         "test-language.rkt"
         "test-library.rkt"
         "test-teachpacks.rkt" ;; LINKS, does not TEST
         "test-modular.rkt"
         "test-parse.rkt"
         "test-proof.rkt"
         "test-regexp.rkt"
         "test-state.rkt"
         "test-private.rkt")
(require rackunit
         rackunit/gui)

(test/gui
 (test-suite "Dracula"
   (test-suite "Internal Tools"
     test-private
     regexp-test-suite)
   (test-suite "ACL2 Connection"
     parse-test-suite
     proof-test-suite
     state-test-suite)
   (test-suite "Language Implementation"
     test-teachpacks
     test-library
     test-language
     test-modular)))
