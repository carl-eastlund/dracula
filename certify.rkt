#!/usr/bin/env racket
#lang mischief

(module+ main

  (command-line

    #:once-any
    [{"-o" "--output"} file
     "Write certification script to <file>."
     (handler (file-handler file))]
    [{"-c" "--command"} command
     "Use certification script as input to <command>. (default: -c acl2)"
     (handler (command-handler command))]
    [{"-p" "--print"}
     "Print certification script."
     (handler (print-handler))]

    #:once-each
    [{"-v" "--verbose"}
     "Show certification output; for use with --command option."
     (verbose? #true)]
    [{"-f" "--force"}
     "Force recreation of proof obligations and certificates."
     (force? #true)]

    #:args files
    (apply (handler)
      (lambda {port} (apply certify-to-port port files))
      files)))

(require
  dracula/proof/dynamic
  dracula/expansion/paths
  syntax/modcode
  racket/date)

(provide certify)

(define (certify . files)
  (call-with-output-string
    (lambda {port}
      (apply certify-to-port port files))))

(define (certify-to-port port . input-files)

  ;; Get all dependencies:
  (define-values {mod-files mod~>deps}
    (apply proof-dependencies input-files))

  (define mod~>updated? (make-hash))

  (define (dependencies file)
    (dict-ref mod~>deps file
      (lambda ()
        (error '|raco certify|
          "no dependencies recorded for module '~a' in: ~v"
          file
          mod~>deps))))

  (define (dependencies-updated? file)
    (for/or {[dep (in-list (dependencies file))]}
      (dict-ref mod~>updated? dep #false)))

  ;; Update proof obligations where necessary:
  (define proof-files
    (if (force?)
      mod-files
      (for/filter {[mod-file (in-list mod-files)]}
        (define-values {zo-file zo-type}
          (get-module-path mod-file))
        (cond
          [(newer? zo-file (book-path mod-file) "Proof obligation")
           (dict-set! mod~>updated? mod-file #true)
           mod-file]
          [(dependencies-updated? mod-file)
           (dict-set! mod~>updated? mod-file #true)
           mod-file]
          [else
           #false]))))
  (apply write-program-obligations-to-file proof-files)

  ;; Compute certificates to update:
  (define cert-files
    (if (force?)
      mod-files
      (for/filter {[mod-file (in-list mod-files)]}
        (cond
          [(newer? (book-path mod-file) (certificate-path mod-file) "Certify")
           (dict-set! mod~>updated? mod-file #true)
           mod-file]
          [(dict-ref mod~>updated? mod-file #false)
           (error '|raco certify|
             "internal error: attempt to update proof obligations failed for ~a"
             mod-file)]
          [(dependencies-updated? mod-file)
           mod-file]
          [else
           #false]))))
  (for {[file (in-list cert-files)]}
    (define cert (certificate-path file))
    (when (file-exists? cert)
      (delete-file cert)))
  (parameterize {[current-output-port port]}
    (apply write-certification-script cert-files)))

(define (certified? file)
  (newer? (certificate-path file) file 'certified?))

(define (newer? file1 file2 [msg 'newer?])
  (cond
    [(not (file-exists? file1))
     #false]
    [(not (file-exists? file2))
     #true]
    [else
     (define time1 (file-or-directory-modify-seconds file1))
     (define time2 (file-or-directory-modify-seconds file2))
     (cond
       [(>= time1 time2)
        #true]
       [else #false])]))

(define (seconds->string secs)
  (date->string (seconds->date secs) #true))

(at-end
  (define handler
    (make-parameter (command-handler "acl2")))
  (define verbose?
    (make-parameter #false))
  (define force?
    (make-parameter #false)))

(define (file-handler file)
  (lambda {write-script . files}
    (call-with-output-file file #:exists 'truncate/replace
      write-script)))

(define (command-handler cmd)
  (lambda {write-script . files}

    (define command
      (find-executable-path cmd))

    (unless (executable? command)
      (error (current-command-symbol)
        "cannot execute command; no such executable: ~a"
        command))

    (define script-file
      (make-temporary-file "dracula-certify-script~a.lisp"))

    (call-with-output-file script-file
      #:exists 'truncate/replace
      (lambda {port}
        (write-script port)
        (flush-output port)))

    (eprintf "Proof obligations generated; running ACL2.\n")

    (define-values {sub sub-stdout sub-stdin sub-stderr}
      (subprocess
        (if (verbose?)
          (current-output-port)
          #false)
        #false
        (if (verbose?)
          (current-error-port)
          'stdout)
        command))

    (define output-handler
      (if sub-stdout
        (thread
          (lambda {}
            (for {[x (in-input-port-bytes sub-stdout)]}
              (void))))
        (thread void)))

    (fprintf sub-stdin "(LD ~s)\n" (path->string script-file))
    (flush-output sub-stdin)
    (close-output-port sub-stdin)

    (subprocess-wait sub)

    (thread-wait output-handler)

    (define failures (filter-not certified? files))

    (cond
      [(empty? failures)
       (eprintf "Certification was successful.\n")]
      [else
       (eprintf "Certification failed for:\n")
       (for {[file (in-list failures)]}
         (eprintf "  ~a (cannot find ~a)\n"
           file
           (certificate-path file)))
       (exit 1)])))

(define (time-of path)
  (date->string (seconds->date (file-or-directory-modify-seconds path))))

(define (print-handler)
  (lambda {write-script . files}
    (write-script (current-output-port))))

(define (current-command-symbol)
  (string->symbol
    (path->string
      (find-system-path 'run-file))))

(define (executable? path)
  (and (file-exists? path)
    (for/or {[perm (in-list (file-or-directory-permissions path))]}
      (eq? perm 'execute))))
