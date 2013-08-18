#lang racket/base

(require racket/gui/base "../lang/dracula.rkt" "../lang/check.rkt")

(begin-below
 (defun play-wav (file async)
   (play-sound file (if async #t #f))))

(provide play-wav)
