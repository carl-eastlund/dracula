#lang scheme

(require "checks.ss"
         "../gui.ss")

(provide gui-suite)

(define gui-suite
  (test-suite "gui.ss"))
