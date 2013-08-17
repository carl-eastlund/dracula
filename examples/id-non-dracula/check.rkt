#lang racket
(require "fresh.rkt" "original.rkt" "identical.rkt" "different.rkt")
(begin-for-syntax
  (unless (free-identifier=? original identical)
    (error 'fresh "~v != ~v\n" original identical))
  (when (free-identifier=? original different)
    (error 'fresh "~v == ~v\n" original different)))
