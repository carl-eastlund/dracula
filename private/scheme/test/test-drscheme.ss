#lang scheme

(require "checks.ss"
         "../drscheme.ss")

(provide drscheme-suite)

(define drscheme-suite
  (test-suite "drscheme.ss"))
