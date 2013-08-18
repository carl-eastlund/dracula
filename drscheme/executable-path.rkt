#lang racket

(require framework
         racket/gui/base)

(provide get-executable-path
         prompt-and-set-executable-path)

(define *acl2-loc-key* 'acl2-executable-location)

;;Make sure we have defaults and marshalling set for the preference:
(preferences:set-default *acl2-loc-key* (build-path "~") path?)
(preferences:set-un/marshall *acl2-loc-key*
                             path->string
                             build-path)

;; get-executable-path : -> Path
;; Returns the path stored in the preference, or prompts the user for one.
(define (get-executable-path)
  (let* ([path (preferences:get *acl2-loc-key*)])
    (if (looks-like-acl2? path)
        path
        (prompt-and-set-executable-path))))

;; Prompts the user for a path, and sets the preference to that.
(define (prompt-and-set-executable-path)
  (set-path-preference (prompt-for-path)))

;; set-path-preference : Path -> Path
;; Sets the preference in *acl2-loc-key* to the given path.
;; Returns the path unchanged, for convenience.
(define (set-path-preference path)
  (preferences:set *acl2-loc-key* path)
  path)

;; prompt-for-path : -> Path 
;; Displays a file selection box so the user can select an ACL2 executable.
(define (prompt-for-path)
  (let* ([path (or (finder:get-file #f "Please select an ACL2 Executable")
                   (preferences:get *acl2-loc-key*))])
    (unless (looks-like-acl2? path)
      (message-box "That is not ACL2"
                   "The path you provided does not seem to point to an ACL2 executable."))
    path))

;; valid-executable-path? Path -> Boolean
;; Checks if the given path points to an ACL2 executable.
(define (looks-like-acl2? path)
  (let* ([path (cond [(string? path) (string->path path)]
                     [else path])])
    (and (path? path)
         (file-exists? path))))