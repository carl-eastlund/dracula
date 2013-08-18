#lang racket/gui

(require drscheme/tool
         "dracula-interfaces.rkt"
         "../proof/proof.rkt"
         (prefix-in syntax: "../proof/syntax.rkt")
         "dracula-state.rkt"
         "dracula-proof-panel.rkt")

(provide dracula-drscheme-tab^
         dracula-drscheme-tab@)

(define green-color "PaleGreen")
(define red-color   "MistyRose")
(define blue-color  "LightBlue")

(define-signature dracula-drscheme-tab^
  (dracula-drscheme-tab-mixin))

(define-unit dracula-drscheme-tab@
  (import drscheme:tool^ dracula-interfaces^ dracula-proof-panel^)
  (export dracula-drscheme-tab^)

  (define drscheme-eventspace (current-eventspace))

  (define dracula-drscheme-tab-mixin
    (mixin
        (drscheme:unit:tab<%>)
        (dracula-drscheme-tab<%>)

      (inherit get-frame get-defs get-ints get-directory)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  METHODS
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define/augment (on-close)
        (shutdown-dracula-proof)
        (inner (void) on-close))

      (define/public (shutdown-dracula-proof)
        (send (get-proof-gui) shut-down-acl2-controller))

      (define/private (make-proof-gui)
        (define container (send (get-frame) get-dracula-gui-container))
        (define gui
          (new dracula-proof-panel%
               [parent container]
               [program-controller this]
               [style '(deleted)]))
        gui)

      (define/private (get-proof-gui)
        (unless dracula-proof-gui
          (set! dracula-proof-gui (make-proof-gui)))
        (unless (is-a? dracula-proof-gui dracula-proof-panel<%>)
          (error 'Dracula
                 "~a\n~a: expected a proof panel, got: ~e"
                 "internal error: GUI failed to initialize."
                 'get-proof-gui
                 dracula-proof-gui))
        dracula-proof-gui)

      (define/private (show-dracula-gui mode)
        (let* ([proof (get-proof-gui)]
               [parent (send proof get-parent)]
               [frame (get-frame)]
               [items (send frame get-dracula-menu-items)])
          (send parent change-children
                (lambda (children)
                  (append (filter-not
                           (lambda (child)
                             (is-a? child dracula-proof-panel<%>))
                           children)
                          (if (and mode show?) (list proof) null))))
          (send frame dracula-proof-shown show?)
          (for ([item (in-list items)])
            (send item enable mode))
          (send proof set-dracula-mode mode)))

      (define/public (toggle-show-proof)
        (set! show? (not show?))
        (update-dracula-gui))

      (define/public (get-proof-event)

        (if proof

            (handle-evt always-evt (lambda (evt) proof))

            (let* ([defs (get-defs)]
                   [channel (make-channel)])

              (thread
               (lambda ()

                 (let* ([dir (or (get-directory) (current-directory))])
                   ;; Expand and load from the current file's directory,
                   ;; or just the current directory if it's not saved.
                   [current-directory dir]
                   [current-load-relative-directory dir])

                 ;; Show compile errors and produce #f (no proof)
                 ;; without bubbling it up to the top.
                 (error-display-handler
                  (lambda (msg exn)
                    (parameterize ([current-eventspace drscheme-eventspace])
                      (queue-callback
                       (lambda ()
                         (send (get-ints) highlight-errors/exn exn))
                       #f))
                    (message-box "Error extracting proof:" msg #f '(ok stop))
                    (yield (channel-put-evt channel #f))))

                 ;; Compile the program and (if no errors) produce its proof.
                 (drscheme:eval:expand-program
                  (drscheme:language:make-text/pos
                   defs 0 (send defs last-position))
                  (send defs get-next-settings)
                  #t ;; compile-time-evals
                  void ;; init
                  void ;; kill
                  (lambda (term _)
                    (channel-put channel (syntax:get-proof term))))))

              (handle-evt channel (lambda (pf) (set! proof pf) pf)))))
      
      (define/private (get-proof)
        (yield (get-proof-event)))

      (define/public (get-cursor-location)
        (send (get-defs) get-start-position))

      (define/private (get-proof-start-position proof)
        0
        #|(if (proof-empty? proof)
            0
            (let* ([term (proof-nth proof 0)]
                   [stx  (term-syntax term)])
              (sub1 (syntax-position stx))))|#)
      
      (define/private (get-proof-term-end-position proof index)
        0
        #|(if (> index 0)
            (let* ([term (proof-nth proof (sub1 index))]
                   [stx  (term-syntax term)]
                   [pos  (sub1 (syntax-position stx))]
                   [span (syntax-span stx)])
              (+ pos span))
            (get-proof-start-position proof))|#)

      (define/public (update-proof-state state)

        (let* ([defs (get-defs)])

          (send defs begin-edit-sequence)

          (send defs unhighlight-saved)
          (send defs lock-up-to-position 0)

          (when (dracula-state-acl2-open? state)
            (let* ([black (dracula-state-first-admitted-position state)]
                   [green (dracula-state-last-admitted-position state)]
                   [red (dracula-state-last-attempted-position state)])
              (send defs lock-up-to-position green)
              (send defs highlight/save black green green-color)
              (unless (dracula-state-finished? state)
                (send defs highlight/save green red blue-color)
                (send defs lock-up-to-position red))
              (unless (dracula-state-edited? state)
                (send defs highlight/save green red red-color))))

          (send defs end-edit-sequence))

        #|(match status
          [(struct proof-status (proof green red))
           (let* ([green-start (get-proof-start-position proof)]
                  [green-end   (get-proof-term-end-position proof green)]
                  [red-end     (get-proof-term-end-position proof red)]
                  [lock-end    (if (> green 0) green-end 0)])
             ;; Begin updates
             (send (get-defs) begin-edit-sequence)
             ;; Lock the "green" portion of the proof.
             (send (get-defs) lock-up-to-position lock-end)
             ;; Remove old highlighting; highlight green & red portions.
             (send (get-defs) unhighlight-saved)
             (send (get-defs) highlight/save green-start green-end green-color)
             (send (get-defs) highlight/save green-end red-end red-color)
             ;; End updates
             (send (get-defs) end-edit-sequence))])|#)

      (define/public (update-dracula-proof start)
        (set! proof #f)
        (send (get-proof-gui) notify-proof-change start))

      (define/public (update-dracula-gui)
        (show-dracula-gui (send (get-defs) dracula-mode)))

      (define/public (save-output)
        (send (get-proof-gui) save-output))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  INITIALIZATION
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (super-new)

      (define show? #t)
      (define proof #f)
      (define dracula-proof-gui #f))))
