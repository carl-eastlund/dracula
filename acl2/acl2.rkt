#lang racket/gui

(require (except-in racket/system process)
         "parse.rkt"
         "rep.rkt"
         (prefix-in acl2- "../lang/acl2-reader.rkt"))

;; An ACL2 is (make-acl2 Process Interaction)
;; A Process is (make-process Subprocess Path OutputPort InputPort InputPort)
;; An Interaction is (make-interaction String String ParseState)
(define-struct acl2 (process [interaction #:mutable]) #:transparent)
(define-struct process (sub path in out err) #:transparent)
(define-struct interaction (prompt input output) #:prefab)

(define (open-acl2 #:dir dir-path-string #:exec exec-path-string)
  (let*-values ([(exec-path) (build-path exec-path-string)]
                [(dir-path) (build-path dir-path-string)]
                [(sub out in err)
                 (parameterize ([current-directory dir-path])
                   (subprocess #f #f #f exec-path))])
    (file-stream-buffer-mode out 'block)
    (file-stream-buffer-mode err 'block)
    (file-stream-buffer-mode in 'none)
    (make-acl2
     (make-process sub exec-path in out err)
     initial-preamble-interaction)))

(define (close-acl2 conn)
  (acl2-send conn '(quit)))

(define (kill-acl2 conn)
  (subprocess-kill (process-sub (acl2-process conn)) #t))

(define (interrupt-acl2 conn)
  (match (acl2-process conn)
    [(struct process [sub path _ _ _])
     (case (system-type 'os)
       [(unix macosx)
        (begin (subprocess-kill sub #f) #t)]
       [(windows)
        (let*-values ([(base name dir?) (split-path path)])
          (system* (build-path base "sendctrlc.exe")
                   (number->string (subprocess-pid sub))))])]))

(define (acl2-send conn sexp)
  (let* ([prev (acl2-interaction conn)]
         [proc (acl2-process conn)]
         [prompt (interaction-final-prompt prev)]
         [input (format "~s\n" sexp)])
    (set-acl2-interaction!
     conn
     (initial-send-interaction prompt input))
    (write-string input (process-in proc))
    (void)))

(define (read-string/eof port)
  (define b (make-bytes 1024))
  (define n (read-bytes-avail!* b port))
  (if (eof-object? n)
    eof
    (bytes->string/latin-1 b #f 0 n)))

(define (acl2-listen conn)
  (match (acl2-process conn)
    [(struct process [sub path in out err])
     (wrap-evt
      (apply choice-evt (filter-not port-closed? (list out err)))
      (lambda (port)
        (define (parse-all)
          (define str (read-string/eof port))
          (cond
            [(eof-object? str) (close-input-port port)]
            [(zero? (string-length str)) (void)]
            [else (match (acl2-interaction conn)
                    [(struct interaction [prompt input output])
                     (set-acl2-interaction!
                      conn
                      (make-interaction prompt input (parse str output)))
                     (yield)
                     (parse-all)])]))
        (parse-all)
        conn))]))

(define (acl2-wait conn)
  (wrap-evt (process-sub (acl2-process conn)) (lambda (_) conn)))

(define initial-preamble-interaction
  (make-interaction "" "" empty-parse-state))

(define (initial-send-interaction prompt input)
  (make-interaction prompt input empty-parse-state))

(define (interaction-final-prompt inter)
  (parse-prompt (interaction-output inter)))

(define (acl2-initial-prompt conn)
  (interaction-prompt (acl2-interaction conn)))

(define (acl2-input conn)
  (interaction-input (acl2-interaction conn)))

(define (acl2-output conn)
  (parse-normal-text (interaction-output (acl2-interaction conn))))

(define (acl2-final-prompt conn)
  (interaction-final-prompt (acl2-interaction conn)))

(define (acl2-proof-tree conn)
  (parse-last-proof-tree (interaction-output (acl2-interaction conn))))

(define (acl2-done? conn)
  (parse-finished? (interaction-output (acl2-interaction conn))))

(define (acl2-admitted? conn)
  (parse-success? (interaction-output (acl2-interaction conn))))

(define (acl2-value conn)
  (string->sexp
   (parse-normal-text
    (interaction-output (acl2-interaction conn)))))

(define (string->sexp str)
  (let* ([port (open-input-string str)])
    (with-handlers ([exn:fail? (lambda (exn) #f)])
      (let* ([one (acl2-read port)]
             [two (acl2-read port)])
        (and (not (eof-object? one))
             (eof-object? two)
             one)))))

(provide/contract
 [acl2? (-> any/c boolean?)]

 [open-acl2 (-> #:dir path-string? #:exec path-string? acl2?)]
 [close-acl2 (-> (and/c acl2? acl2-done?) void?)]
 [interrupt-acl2 (-> (and/c acl2? (not/c acl2-done?)) boolean?)]
 [kill-acl2 (-> acl2? void?)]

 [acl2-send (-> (and/c acl2? acl2-done?) sexp/c void?)]
 [acl2-listen (-> acl2? evt?)]
 [acl2-wait (-> acl2? evt?)]

 [acl2-initial-prompt (-> acl2? string?)]
 [acl2-input (-> acl2? string?)]
 [acl2-output (-> acl2? string?)]
 [acl2-final-prompt (-> acl2? string?)]
 [acl2-proof-tree (-> acl2? string?)]
 [acl2-done? (-> acl2? boolean?)]
 [acl2-admitted? (-> (and/c acl2? acl2-done?) boolean?)]
 [acl2-value (-> (and/c acl2? acl2-done?) (or/c sexp/c #f))])
