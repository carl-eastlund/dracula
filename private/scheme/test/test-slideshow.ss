#lang scheme

(require (except-in "checks.ss" before after)
         "../slideshow.ss")

(provide slideshow-suite)

(define slideshow-suite
  (test-suite "slideshow.ss"))
