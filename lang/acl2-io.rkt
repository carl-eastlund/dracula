#lang racket

#|
Provides a few of ACL2's file io primitives.
|#

(provide open-input-channel open-output-channel
  close-input-channel close-output-channel
  read-char$ princ$
  read-byte$ write-byte$)

(define (open-input-channel file-name io-type state)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (list '() state))])
    ;; Do I really want the port to leak out here?
    (list (open-input-file file-name) state)))

(define (open-output-channel file-name io-type state)
  (with-handlers ([exn:fail:filesystem? (lambda (e) (list '() state))])
    (list (open-output-file file-name #:exists 'replace) state)))

(define (close-input-channel channel state)
  (close-input-port channel)
  state)

(define (close-output-channel channel state)
  (close-output-port channel)
  state)

(define (read-char$ channel state)
  (let ([c (read-char channel)])
    (list (if (eof-object? c) '() c)
      state)))

(define (read-byte$ channel state)
  (let ([b (read-byte channel)])
    (list (if (eof-object? b)
            '()
            b)
      state)))

(define (write-byte$ byte channel state)
  (write-byte byte channel)
  state)

(define (princ$ obj channel state)
  (cond
    [(char? obj) (write-char obj channel)]
    [(string? obj) (write-string obj channel)]
    [(symbol? obj) (write obj channel)]
    [(number? obj) (write obj channel)]
    [else (error 'princ$
            "Expected character, string, symbol, or number as first argument.  Given ~s"
            obj)])
  state)
