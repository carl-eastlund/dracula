#lang racket/gui

(require drscheme/tool
         framework
         "dracula-interfaces.rkt")

(provide dracula-drscheme-definitions^
         dracula-drscheme-definitions@)

(define-signature dracula-drscheme-definitions^
  (dracula-drscheme-definitions-mixin))

(define-unit dracula-drscheme-definitions@
  (import drscheme:tool^ dracula-interfaces^)
  (export dracula-drscheme-definitions^)

  (define dracula-drscheme-definitions-mixin
    (mixin
        (drscheme:unit:definitions-text<%>
         text:basic<%>
         (class->interface text%))
        (dracula-drscheme-definitions<%>)

      (inherit get-next-settings get-tab
               highlight-range unhighlight-range
               begin-edit-sequence end-edit-sequence)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  METHODS
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define/augment (on-close)
        (send (get-tab) shutdown-dracula-proof)
        (inner (void) on-close))

      (define/augment (after-set-next-settings settings)
        (queue-callback
         (lambda ()
           (send (get-tab) update-dracula-gui))
         #f)
        (inner (void) after-set-next-settings settings))

      (define/augment (after-insert start len)
        (queue-callback
         (lambda ()
           (send (get-tab) update-dracula-proof start))
         #f)
        (inner (void) after-insert start len))

      (define/augment (after-delete start len)
        (queue-callback
         (lambda ()
           (send (get-tab) update-dracula-proof start))
         #f)
        (inner (void) after-delete start len))
      
      (define/augment (can-delete? start len)
        (and (>= start locked-threshold)
             (inner #t can-delete? start len)))

      (define/augment (can-insert? start len)
        (and (>= start locked-threshold)
             (inner #t can-insert? start len)))

      (define/public (dracula-mode)
        (send
         (drscheme:language-configuration:language-settings-language
          (get-next-settings))
         dracula-mode))

      (define/public (lock-up-to-position pos)
        (set! locked-threshold pos))

      (define/public (highlight/save start end color)
        (cond
         [(< start end)
          (let* ([unhighlighter (highlight-range start end color)])
            (hash-set! saved-unhighlighters unhighlighter 'dummy))]
         ;; Really problematic to ever throw an error.
         ;; Fail gracefully.
         [else (log-warning
                (format "Dracula: internal error: ~a: start ~a >= end ~a"
                        'highlight/save
                        start
                        end))]))

      (define/public (unhighlight-saved)
        (begin-edit-sequence)
        (hash-for-each
         saved-unhighlighters
         (lambda (unhighlighter dummy)
           (unhighlighter)
           (hash-remove! saved-unhighlighters unhighlighter)))
        (end-edit-sequence))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  INITIALIZATION
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define locked-threshold 0)

      (define saved-unhighlighters (make-hasheq))

      (super-new))))