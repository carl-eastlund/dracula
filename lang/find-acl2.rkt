#lang racket/gui

#|
Locate the ACL2 binary via a GUI dialog or a cached path.  Attempts
to verify that the supplied executable is indeed ACL2 by looking at
the header text.
|#

(require
  framework
  "acl2-path-to-lisp-command.rkt"
  "acl2-location-pref.rkt")

(provide find-acl2)

(define find-acl2
  (lambda ([path (get-acl2-location)])
    (if (valid-path? path)
      path
      (let ([supplied-path
             (finder:get-file #f "Select ACL2 Executable" (byte-regexp #".*acl2.*")
               "The file must contain `acl2' in the name.")])
        (and (path? supplied-path)
          (if (valid-path? supplied-path)
            (begin (preferences:set *acl2-loc-key* supplied-path)
              supplied-path)
            (begin (message-box
                     "That is not ACL2"
                     (format
                       "The supplied executable ~s does not appear to be ACL2."
                       supplied-path)
                     #f
                     '(ok))
              #f)))))))

(define (valid-path? path)
  ;; acl2-path->command-list, in particular, may throw if user supplies
  ;; a strange `path'.
  (with-handlers ([exn:fail:read? (lambda (e) #f)])
    (let ([path (cond [(string? path) (string->path path)]
                  [(path? path) path]
                  [else #f])])
      (and (path? path)
        (file-exists? path)
        (let-values ([(proc->me me->proc pid err->me control)
                      (apply values 
                        (apply process* 
                          (acl2-path->command-list path)))])
          ;; Look for #"ACL2" in the output.
          (let* ([evt (regexp-match-evt #"ACL2" proc->me)]
                 [running? (eq? (control 'status) 'running)]
                 [is-acl2? (and running? (sync/timeout/enable-break 5 evt) #t)])
            (when is-acl2? ;; ACL2 is hard to kill; you must ask it politely
              (write-string ":q (good-bye)" me->proc)
              (newline me->proc)
              (flush-output me->proc))
            
            (control 'kill)
            (close-input-port proc->me)
            (close-input-port err->me)
            (close-output-port me->proc)
            is-acl2?))))))
