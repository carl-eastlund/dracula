#lang racket/gui

(require mzlib/port)

(provide acl2-path->command-list)

(define (read-last-line)
  (strip-shell-command-start (current-input-port))
  (let loop ([prior-line ""]
             [line (read-line)])
    (if (eof-object? line) prior-line (loop line (read-line)))))

(define (windows-exe? acl2-path)
  (let-values ([(base name dir?) (split-path acl2-path)])
    (and (eq? (system-type 'os) 'windows)
      (not dir?)
      (path? name)
      (string=? (path->string name) "acl2.exe"))))

(define (number->string/-i-special n)
  (let ([ip (imag-part n)])
    (cond [(negative? ip) "-I"]
      [(positive? ip) "+I"]
      [else (number->string n)])))

(define (command-string? s)
  (and (not (string=? s "exec"))
    (not (string=? s "$*"))))

(define parse-command-tokens
  (match-lambda
    [(or
       (regexp
         #rx"^ *\"((?:[^\"]|\\\\\")*)\"(.*)$"
         (list whole token rest))
       (regexp
         #rx"^ *'((?:[^']|\\\\')*)'(.*)$"
         (list whole token rest))
       (regexp
         #rx"^ *([^ '\"]+)(.*)$"
         (list whole token rest)))
     (cons token (parse-command-tokens rest))]
    [(regexp #rx"^ *$") '()]
    [other (error 'parse-command-tokens
             "cannot parse ~s from ACL2 script" other)]))

(define (parse-cmd-string s)
  (filter command-string? (parse-command-tokens s)))

;; On Unix, read the command string from the shell script that starts ACL2.
;; We do this to get the right PID for later process control.
;; On Windows, acl2.exe isn't a shell script.
(define (acl2-path->command-list acl2-path)
  (if (windows-exe? acl2-path)
    (list (path->string acl2-path))
    (parse-cmd-string
      (with-input-from-file acl2-path read-last-line))))
