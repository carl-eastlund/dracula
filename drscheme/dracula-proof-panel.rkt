#lang racket/gui

(require drscheme/tool
         framework
         racket/runtime-path
         mrlib/switchable-button
         mrlib/bitmap-label
         racket/pretty
         "dracula-interfaces.rkt"
         "executable-path.rkt"
         "dracula-state.rkt"
         "../proof/proof.rkt"
         "../acl2/acl2.rkt"
         "../lang/acl2-module-v.rkt")

(provide dracula-proof-panel^
         dracula-proof-panel@)

;; Paths for the button images:
(define-runtime-path save-img "../images/save.png")
(define-runtime-path acl2-icon-img "../images/acl2-icon.png")
(define-runtime-path admit-one-img "../images/admit-one.png")
(define-runtime-path admit-all-img "../images/admit-all.png")
(define-runtime-path to-cursor-img "../images/to-cursor.png")
(define-runtime-path undo-one-img "../images/undo-one.png")
(define-runtime-path undo-all-img "../images/undo-all.png")
(define-runtime-path interrupt-img "../images/interrupt.png")
(define-runtime-path stop-img "../images/stop.png")
(define-runtime-path start-img "../images/start.png")
(define-runtime-path undock-img "../images/undock.png")
(define-runtime-path dock-img "../images/dock.png")

;; ======================================================================
;;
;;  UNIT
;;
;; ======================================================================

(define-signature dracula-proof-panel^
  (dracula-proof-panel%))

(define-unit dracula-proof-panel@
  (import drscheme:tool^ dracula-interfaces^)
  (export dracula-proof-panel^)

  (define drscheme-eventspace (current-eventspace))

  (define SEXP-ADD-TEACHPACK-DIR
    `(add-include-book-dir :teachpacks ,teachpack-path))

  ;; ======================================================================
  ;;
  ;;  PROOF PANEL CLASS
  ;;
  ;; ======================================================================

  (define dracula-proof-panel%
    (class* panel:vertical-dragable% (dracula-proof-panel<%>)

      ;; ==================================================
      ;;
      ;;  METHODS
      ;;
      ;; ==================================================

      (define/private (dracula-fail e [result (void)])
        (when (dracula-state-acl2-open? state)
          (set-state (dracula-state-error state)))
        (destroy-acl2)
        (message-box "Dracula: Internal Error" (exn-message e))
        result)

      ;; ==============================
      ;;  DrScheme Interaction
      ;; ==============================

      ;; shut-down-acl2-controller : -> Void
      ;; Terminate all ACL2 processes.
      (define/public (shut-down-acl2-controller)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (when (dracula-state-acl2-open? state)
            (set-state (dracula-state-stop-acl2 state)))
          (destroy-acl2)))

      (define/private (destroy-acl2)
        (when acl2
          (kill-acl2 acl2)
          (set! acl2 #f)))

      ;; save-output : -> Any
      ;; Save the output of the current ACL2 process.
      (define/public (save-output)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (if (dracula-state-acl2-open? state)
            (let* ([path (put-file #f #f #f "ACL2-Output" "txt")])
              (when path
                (let* ([file (open-output-file path
                                               #:mode 'text
                                               #:exists 'replace)])
                  (display (dracula-state-total-output state) file)
                  (close-output-port file))))
            (message-box "Dracula: Cannot Save"
                         "ACL2 is not currently open."))))
      
      ;; notify-proof-change : -> Void
      ;; Handle the event that the proof text has changed.
      (define/public (notify-proof-change position)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (when (and (dracula-state-acl2-open? state)
                     (< position (dracula-state-last-attempted-position state)))
            (set-state (dracula-state-edit state)))))

      ;; ==============================
      ;;  User Interaction
      ;; ==============================

      (define/public (start-acl2)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (set-state (dracula-state-pending state "updating proof"))
          (when (rebuild)
            (set-state (dracula-state-choose state 'Dracula))
            (run-acl2))))

      ;; run-acl2 : -> Void
      ;; Start an ACL2 process for the current proof.
      (define/private (run-acl2)

        ;; Start ACL2
        (set-state (dracula-state-pending state "starting ACL2"))
        (set! acl2 (open-acl2 #:dir (or (send program-controller get-directory)
                                        (current-directory))
                              #:exec (get-executable-path)))
        (set-state (dracula-state-start-acl2 state acl2))

        ;; Wait for a prompt.
        (set-state (dracula-state-pending state "waiting for ACL2 prompt"))
        (wait-for-acl2-prompt)

        ;; Send ACL2 our preamble; hide output.
        (set-state (dracula-state-pending state "initializing ACL2"))
        (for ([sexp (get-preamble)])
          (acl2-send acl2 sexp)
          (wait-for-acl2-prompt #:show #f))

        ;; Done.
        (set-state (dracula-state-done state)))

      ;; stop-acl2 : -> Void
      ;; Stop the ACL2 process for the current proof.
      (define/public (stop-acl2)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (if (dracula-state-active? state)
            (shut-down-acl2-controller)
            (begin
              (set-state (dracula-state-pending state "closing ACL2"))
              (close-acl2 acl2)
              (wait-for-acl2-termination)))))

      ;; update-name : Symbol -> Void
      ;; Selects a new proof obligation by name.
      (define/public (update-name name)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (set-state (dracula-state-choose state name))
          (run-acl2)))

      ;; admit-one : -> Void
      ;; Admit the next term.
      (define/public (admit-one)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (set-state (dracula-state-pending state "updating proof"))
          (when (rebuild)
            (set-state
             (dracula-state-pending state "admitting next unadmitted term"))
            (unless (or (dracula-state-admitted? state)
                        (dracula-state-start-of-proof? state))
              (set-state (dracula-state-rewind state)))
            (admit-to (+ (dracula-state-term-index state) 1)))
          (set-state (dracula-state-done state))))

      ;; undo-one : -> Void
      ;; Undo the last term admitted.
      (define/public (undo-one)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (set-state (dracula-state-pending state "updating proof"))
          (when (rebuild)
            (set-state
             (dracula-state-pending state "undoing last admitted term"))
            (unless (or (dracula-state-admitted? state)
                        (dracula-state-start-of-proof? state))
              (set-state (dracula-state-rewind state)))
            (undo-to (dracula-state-term-index state)))
          (set-state (dracula-state-done state))))

      ;; undo-all : -> Void
      ;; Undo all admitted terms.
      (define/public (undo-all)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (set-state (dracula-state-pending state "updating proof"))
          (when (rebuild)
            (set-state
             (dracula-state-pending state "undoing all admitted terms"))
            (unless (or (dracula-state-admitted? state)
                        (dracula-state-start-of-proof? state))
              (set-state (dracula-state-rewind state)))
            (undo-to 1))
          (set-state (dracula-state-done state))))

      ;; admit-all : -> Void
      ;; Attempt to admit all remaining terms.
      (define/public (admit-all)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (set-state (dracula-state-pending state "updating proof"))
          (when (rebuild)
            (set-state
             (dracula-state-pending state "admitting all unadmitted terms"))
            (unless (or (dracula-state-admitted? state)
                        (dracula-state-start-of-proof? state))
              (set-state (dracula-state-rewind state)))
            (admit-to (sub1 (dracula-state-proof-size state))))
          (set-state (dracula-state-done state))))

      ;; to-cursor : -> Void
      ;; Admit or rewind to the term with the cursor in it.
      (define/public (to-cursor)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])

          (set-state (dracula-state-pending state "updating proof"))
          (when (rebuild)

            (set-state
             (dracula-state-pending state "computing term index"))
            (unless (dracula-state-admitted? state)
              (set-state (dracula-state-rewind state)))

            (let ([cursor (send program-controller get-cursor-location)]
                  [last-admitted-position
                   (dracula-state-last-admitted-position state)])

              (if (> cursor last-admitted-position)

                (let ([pos (dracula-state-index-before-pos state cursor)])
                  (set-state
                   (dracula-state-pending state "admitting terms"))
                  (admit-to pos))

                (let ([pos (dracula-state-index-after-pos state cursor)])
                  (set-state
                   (dracula-state-pending state "undoing terms"))
                  (undo-to pos)))))

          (set-state (dracula-state-done state))))

      ;; ==============================
      ;;  Sub-control interaction
      ;; ==============================

      ;; get-rebuilt-state : -> (Or DraculaState #f)
      ;; Rebuild the proof; return it if successful, or #f if not.
      (define/public (get-rebuilt-state)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e #f))])
          (set-state (dracula-state-pending state "updating proof"))
          (let* ([success? (rebuild)])
            (set-state (dracula-state-done state))
            (and success? state))))

      ;; ==============================
      ;;  Admit/Undo loops
      ;; ==============================

      ;; admit-to : Nat -> Void
      ;; Admit until index n, or failure.
      ;; Precondition: state must be at an admitted term.
      (define/private (admit-to n)

        (unless (or (not (dracula-state-admitted? state))
                    (>= (dracula-state-term-index state) n)
                    (dracula-state-end-of-proof? state))

          (acl2-send acl2 (dracula-state-save-before-next-sexp state))
          (cond
           [(wait-for-acl2-prompt #:show #f #:expr #t) =>
            (lambda (saved)
              (acl2-send acl2 (dracula-state-next-sexp state))
              (set-state (dracula-state-advance state acl2 saved))
              (wait-for-acl2-prompt)
              (admit-to n))]
           [else (dracula-state-advance state acl2 #f)])))

      ;; undo-to : Nat -> Void
      ;; Undo back past index n.
      ;; Precondition: state must be at an admitted term.
      (define/private (undo-to n)

        (unless (or (< (dracula-state-term-index state) n)
                    (dracula-state-start-of-proof? state))

          (acl2-send acl2 (dracula-state-restore-saved-sexp state))
          (wait-for-acl2-prompt #:show #f)
          (set-state (dracula-state-rewind state))
          (undo-to n)))

      ;; ==============================
      ;;  Interrupting (disabled)
      ;; ==============================

      ;; interrupt : -> Void
      ;; Interrupt active processes, handing control back to the user.
      (define/public (interrupt)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          #|(begin
          (send handler interrupt (make-interrupt-callback))
          (set-state (dracula-state-interrupt state)))|#
          (when (and acl2 (not (acl2-done? acl2)))
            (unless (interrupt-acl2 acl2)
              (message-box
               "Interrupt ACL2: Error"
               (apply string-append
                      (add-between
                       (list "Could not interrupt the running ACL2 process."
                             "This usually occurs on Windows systems with"
                             "ACL2 installations that lack SENDCTRLC.EXE."
                             "Consider using the prebuilt binary installer.")
                       "\n")))))))

      ;; make-interrupt-callback : (-> (-> Void))
      ;; Callback for interrupting events.
      (define/private (make-interrupt-callback)
        (lambda ()
          (set-state (dracula-state-done state))))

      ;; ==============================
      ;;  ACL2 preamble
      ;; ==============================

      (define/private (get-preamble)
        (list ':start-proof-tree
              SEXP-ADD-TEACHPACK-DIR))

      ;; ==============================
      ;;  DrScheme communication
      ;; ==============================

      ;; set-dracula-mode : Mode -> Void
      ;; Set the Dracula mode ('acl2, 'modular-acl2, #f).
      (define/public (set-dracula-mode new-mode)
        (with-handlers ([exn:fail? (lambda (e) (dracula-fail e))])
          (unless (equal? new-mode mode)
            (shut-down-acl2-controller)
            (set! mode new-mode))
          (redraw)))

      ;; rebuild : -> Boolean
      ;; Updates the state for the current proof, reporting whether or not
      ;; there was an error.
      (define/private (rebuild)
        (cond
         [(yield (send program-controller get-proof-event)) =>
          (lambda (proof)
            (set-state (dracula-state-populate state proof))
            #t)]
         [else #f]))

      ;; ==============================
      ;;  ACL2 communication
      ;; ==============================

      ;; wait-for-acl2-prompt : [#:show Bool #:expr Bool] -> (Or Sexp Bool)
      ;; Listens to ACL2 output until it reaches a prompt.
      ;; Produces the result of a successful expression.
      ;; Produces #t for a successful non-expression.
      ;; Produces #f for any kind of failure.
      (define/private (wait-for-acl2-prompt #:show [show? #t] #:expr [expr? #f])

        (define acl2-time-delta 1)

        (let loop ()

          (yield (acl2-listen acl2))

          (when show?
            (set-state (dracula-state-update-acl2 state acl2)))

          (cond
           [(not (acl2-done? acl2)) (sleep/yield acl2-time-delta) (loop)]
           [(not (acl2-admitted? acl2)) #f]
           [expr? (acl2-value acl2)]
           [else #t])))

      (define/private (wait-for-acl2-termination)

        (define now (current-milliseconds))
        (define later (+ now 5000))

        (define (wait)
          (yield
           (choice-evt
            (handle-evt (acl2-wait acl2) finish)
            (handle-evt (alarm-evt later) give-up)
            (handle-evt (acl2-listen acl2) listen))))

        (define (finish evt)
          (set! acl2 #f)
          (set-state (dracula-state-stop-acl2 (dracula-state-done state))))

        (define (give-up evt)
          (shut-down-acl2-controller))

        (define (listen acl2)
          (set-state (dracula-state-update-acl2 state acl2))
          (wait))

        (wait))

      ;; ==============================
      ;;  State Manipulation
      ;; ==============================

      ;; set-state : DraculaState -> Void
      ;; Update the state and the GUI.
      (define/private (set-state new-state)
        (set! state new-state)
        (redraw))

      ;; redraw : -> Void
      ;; Update the GUI.
      (define/private (redraw)
        (send program-controller update-proof-state state)
        (send proof-controls redraw state mode)
        (send proof-display redraw state))

      ;; ==================================================
      ;;
      ;;  INITIALIZATION
      ;;
      ;; ==================================================

      (super-new)

      (init-field program-controller)

      (define state initial-dracula-state)
      (define acl2 #f)
      (define mode #f)

      (define proof-controls
        (new dracula-proof-controls%
             [parent this]
             [proof-controller this]))

      (define proof-display
        (new dracula-proof-display%
             [parent this]
             [proof-controller this]))

      #|(define handler (new dracula-event-handler%))|#

      (redraw)

      ))

  #|

  ;; ======================================================================
  ;;
  ;;  EVENT HANDLER CLASS
  ;;
  ;; ======================================================================

  (define dracula-event-handler%
    (class object%

      ;; ==================================================
      ;;
      ;;  Asynchronous methods (caller's thread)
      ;;
      ;; ==================================================

      ;; handle : (Event (-> Any)) -> Void
      ;; Handles the event asynchronously, queueing up the resulting
      ;; callback in the drscheme eventspace when it occurs.
      (define/public (handle event)
        (channel-put work-channel event))

      #|
      ;; interrupt : (-> Any) -> Void
      ;; Cancels any pending event, queueing up the given callback in the
      ;; drscheme eventspace if an event is canceled before it occurs.
      (define/public (interrupt callback)
        (channel-put interrupt-channel callback))
      |#

      ;; ==================================================
      ;;
      ;;  Synchronous methods (event thread)
      ;;
      ;; ==================================================

      ;; idle : -> never
      ;; Waits for an event to handle.
      (define/private (idle)
        (sync
         (handle-evt
          work-channel
          (lambda (event)
            (work event)))
         #|(handle-evt
          interrupt-channel
          (lambda (callback)
            (idle)))|#))

      ;; work : (Event Void) -> never
      ;; Handles or interrupts an event.
      (define/private (work event)
        (sync
         (handle-evt
          event
          (lambda (callback)
            (parameterize ([current-eventspace drscheme-eventspace])
              (queue-callback callback #f))
            (idle)))
         #|(handle-evt
          interrupt-channel
          (lambda (callback)
            (parameterize ([current-eventspace drscheme-eventspace])
              (queue-callback callback #f))
            (idle)))|#))

      ;; ==================================================
      ;;
      ;;  Initialization
      ;;
      ;; ==================================================

      (super-new)

      (define work-channel (make-channel))
      #|(define interrupt-channel (make-channel))|#

      (thread (lambda () (idle)))))

  |#

  ;; ======================================================================
  ;;
  ;;  PROOF CONTROLS CLASS
  ;;
  ;; ======================================================================

  (define dracula-proof-controls%
    (class vertical-panel%

      (inherit change-children)

      (define/public (redraw state mode)
        (send proof-tree redraw state)
        (send proof-choice redraw state mode)
        (send button-panel redraw state mode)
        (send status-message redraw state)
        (change-children
         (lambda (children)
           (case mode
             [(modular-acl2)
              (list proof-tree
                    proof-choice
                    button-panel
                    status-message)]
             [else
              (list proof-tree
                    button-panel
                    status-message)]))))

      (super-new)

      (init-field proof-controller)

      (define proof-tree
        (new dracula-proof-tree%
             [parent this]
             [proof-controller proof-controller]))

      (define proof-choice
        (new dracula-proof-choice%
             [parent this]
             [proof-controller proof-controller]))

      (define button-panel
        (new dracula-proof-buttons%
             [parent this]
             [proof-controller proof-controller]))

      (define status-message
        (new dracula-status-message%
             [parent this]
             [proof-controller proof-controller]))

      ))

  ;; ======================================================================
  ;;
  ;;  LOCKED TEXT FIELD CLASS / MIXIN
  ;;
  ;; ======================================================================

  (define (locked-text-field-mixin c%)
    (class c%

      (inherit get-editor)

      (define/override (set-value str)
        (send (get-editor) lock #f)
        (super set-value str)
        (send (get-editor) lock #t))

      (super-new)

      (init [undo-history 0])

      (send (get-editor) lock #t)
      (send (get-editor) set-max-undo-history undo-history)))

  (define locked-text-field%
    (locked-text-field-mixin text-field%))

  (define locked-combo-field%
    (locked-text-field-mixin combo-field%))

  ;; ======================================================================
  ;;
  ;;  PROOF TREE CLASS
  ;;
  ;; ======================================================================

  (define dracula-proof-tree%
    (class locked-text-field%

      (inherit set-value enable)

      (define/public (redraw state)
        (if (dracula-state-acl2-open? state)
            (begin (set-value (dracula-state-proof-tree state)) (enable #t))
            (begin (set-value "") (enable #f))))

      (super-new [label #f] [style '(multiple)])

      (init-field proof-controller)))

  ;; ======================================================================
  ;;
  ;;  PROOF CHOICE CLASS
  ;;
  ;; ======================================================================

  (define dracula-proof-choice%
    (class locked-combo-field%

      (inherit get-menu set-value)

      (define/public (redraw state mode)
        (log-debug "MACL2: redrawing")
        (if (dracula-state-proof-chosen? state)
            (set-value (symbol->string (dracula-state-current-name state)))
            (set-value "<Choose A Module>"))
        (enable (not (dracula-state-acl2-open? state)))
        (log-debug "MACL2: redrawn"))

      (define/override (enable ?)
        (log-debug (if ? "MACL2: enabled" "MACL2: disabled"))
        (super enable ?))

      (define/override (on-popup e)
        (log-debug "MACL2: popping up")
        (cond
         [(send proof-controller get-rebuilt-state) =>
          (lambda (state)
            (repopulate (get-menu) state)
            (super on-popup e))]
         [else (log-debug "MACL2: immediate popup")
               (super on-popup e)])
        (log-debug "MACL2: popped up"))

      (define/private (repopulate menu state)
        (log-debug "MACL2: repopulate")
        (for ([item (send menu get-items)])
          (send item delete))
        (log-debug "MACL2: depopulated")
        (for ([name (dracula-state-names state)])
          (log-debug (format "MACL2: repopulating '~a'" name))
          (new menu-item%
               [label (symbol->string name)]
               [parent menu]
               [callback
                (lambda (i e)
                  (log-debug (format "MACL2: selected '~a'" name))
                  (update name))]))
        (log-debug "MACL2: repopulated"))

      (define/private (update name)
        (send proof-controller update-name name))

      (super-new [label #f] [choices null] [stretchable-width #t])

      (init-field proof-controller)))

  #|
  (define dracula-proof-choice%
    (class choice%

      (inherit clear
               append
               enable
               set-string-selection
               get-string-selection)

      (define/public (redraw state)
        (let* ([names (dracula-state-names state)]
               [current (and (dracula-state-proof-chosen? state)
                             (dracula-state-current-name state))])
          (clear)
          (for ([name names]) (append (symbol->string name)))
          (when current (set-string-selection (symbol->string current)))
          (enable (and (cons? names) (cons? (rest names))))))

      (define/private (update)
        (send proof-controller update-name
              (string->symbol (get-string-selection))))

      (super-new [label #f]
                 [choices null]
                 [stretchable-width #t]
                 [callback (lambda _ (update))])

      (init-field proof-controller)))
  |#

  ;; ======================================================================
  ;;
  ;;  PROOF BUTTONS CLASS
  ;;
  ;; ======================================================================

  (define dracula-proof-buttons%
    (class vertical-panel%

      (define/public (redraw state mode)
        (let* ([chosen? (dracula-state-proof-chosen? state)]
               [open? (dracula-state-acl2-open? state)]
               [active? (dracula-state-active? state)]
               #|[interrupt? (dracula-state-interrupt? state)]|#
               #|[start? (and open? (dracula-state-start-of-proof? state))]|#
               #|[end? (and open? (dracula-state-end-of-proof? state))]|#
               [modular? (eq? mode 'modular-acl2)])

          (send go/interrupt/stop-panel
                choose
                (cond
                 #|[(and active? (not interrupt?)) interrupt-button]|#
                 [open? stop-button]
                 #|[modular? interrupt-button]|#
                 [else go-button]))

          (send go-button enable
            (and (not modular?) (not open?) #|(not active?)|#))
          (send interrupt-button enable
            #f #|(and active? (not interrupt?))|#)
          (send stop-button enable
            (and open? #|(or (not active?) interrupt?)|#))

          (send undo-all-button enable
            (and open? (not active?) #|(not start?)|#))
          (send undo-one-button enable
            (and open? (not active?) #|(not start?)|#))
          (send admit-all-button enable
            (and open? (not active?) #|(not end?)|#))
          (send admit-one-button enable
            (and open? (not active?) #|(not end?)|#))
          (send to-cursor-button enable
            (and open? (not active?) #|(not (and start? end?))|#))))

      (define/private (path->bitmap path)
        (make-object bitmap% (path->string path) 'png/mask))

      (super-new [stretchable-height #f] [stretchable-width #f])

      (define top-panel
        (new horizontal-panel% [parent this] [stretchable-width #f]))

      (define go/interrupt/stop-panel
        (new union-panel%
             [parent top-panel]
             [stretchable-width #f]
             [alignment '(right center)]))

      (define go-button
        (new dracula-button%
             [parent go/interrupt/stop-panel]
             [label "Start"]
             [bitmap (path->bitmap start-img)]
             [callback (lambda _ (send proof-controller start-acl2))]))

      (define interrupt-button
        (new dracula-button%
             [parent go/interrupt/stop-panel]
             [label "Break"]
             [bitmap (path->bitmap interrupt-img)]
             [callback (lambda _ (send proof-controller interrupt))]))

      (define stop-button
        (new dracula-button%
             [parent go/interrupt/stop-panel]
             [label "Stop"]
             [bitmap (path->bitmap stop-img)]
             [callback (lambda _ (send proof-controller stop-acl2))]))

      (define to-cursor-button
        (new dracula-button%
             [parent top-panel]
             [label "To Cursor"]
             [bitmap (path->bitmap to-cursor-img)]
             [callback (lambda _ (send proof-controller to-cursor))]))

      (define bottom-panel
        (new horizontal-panel% [parent this] [stretchable-width #f]))

      (define undo-all-button
        (new dracula-button%
             [parent bottom-panel]
             [label "Reset"]
             [bitmap (path->bitmap undo-all-img)]
             [callback (lambda _ (send proof-controller undo-all))]))

      (define undo-one-button
        (new dracula-button%
             [parent bottom-panel]
             [label "Undo"]
             [bitmap (path->bitmap undo-one-img)]
             [callback (lambda _ (send proof-controller undo-one))]))

      (define admit-one-button
        (new dracula-button%
             [parent bottom-panel]
             [label "Admit"]
             [bitmap (path->bitmap admit-one-img)]
             [callback (lambda _ (send proof-controller admit-one))]))

      (define admit-all-button
        (new dracula-button%
             [parent bottom-panel]
             [label "All"]
             [bitmap (path->bitmap admit-all-img)]
             [callback (lambda _ (send proof-controller admit-all))]))

      (init-field proof-controller)))

  (define union-panel%
    (class panel%

      (super-new)

      (inherit get-children get-alignment)

      (define/public (choose child)
        (for ([child* (get-children)])
          (send child* show (eq? child* child))))

      (define/override (container-size info)
        (match info
          [(list (list w h _ _) ...)
           (values (apply max 0 w)
                   (apply max 0 h))]))

      (define/override (place-children info w0 h0)
        (let*-values ([(ha va) (get-alignment)]
                      [(hp) (horiz->place ha)]
                      [(vp) (vert->place va)])
          (map (lambda (child) (place-child hp vp w0 h0 child)) info)))

      (define/private (place-child hp vp w0 h0 child)
        (match child
          [(list cw ch sw sh)
           (let*-values ([(x w) (place-dim hp w0 cw sw)]
                         [(y h) (place-dim vp h0 ch sh)])
             (list x y w h))]))

      (define/private (place-dim p maximum minimum stretch?)
        (match (list p stretch?)
          [(list _ #t) (values 0 maximum)]
          [(list 'min #f) (values 0 minimum)]
          [(list 'mid #f) (values (floor (/ (- maximum minimum) 2)) minimum)]
          [(list 'max #f) (values (- maximum minimum) minimum)]))

      (define/private horiz->place
        (match-lambda ['left 'min] ['center 'mid] ['right 'max]))

      (define/private vert->place
        (match-lambda ['top 'min] ['center 'mid] ['bottom 'max]))))

  (define dracula-button%
    (class switchable-button%
      (super-new)

      (inherit on-event)

      (define/override (on-superwindow-show shown?)
        (on-event (new mouse-event% [event-type 'leave]))
        (super on-superwindow-show shown?))))

  ;; ======================================================================
  ;;
  ;;  STATUS MESSAGE CLASS
  ;;
  ;; ======================================================================

  (define dracula-status-message%
    (class message%

      (inherit set-label enable)

      (define/public (redraw state)
        (set-label
         (cond
          [(dracula-state-active? state)
           (format "~a: ~a."
                   (cond
                    [(dracula-state-interrupt? state) "Interrupting"]
                    [(dracula-state-error? state) "ERROR"]
                    [else "Busy"])
                   (dracula-state-activity state))]
          [else ""])))

      (super-new [label ""] [stretchable-width #t])

      (init-field proof-controller)))

  ;; ======================================================================
  ;;
  ;;  PROOF DISPLAY CLASS
  ;;
  ;; ======================================================================

  (define dracula-proof-display%
    (class locked-text-field%

      (inherit get-value get-editor enable)

      (define/public (redraw state)
        (if (dracula-state-acl2-open? state)
            (begin (set-value
                    (string-append (dracula-state-initial-prompt state)
                                   (dracula-state-acl2-input state)
                                   (dracula-state-acl2-output state)
                                   (dracula-state-final-prompt state)))
                   (enable #t))
            (begin (set-value "")
                   (enable #f))))

      (define/override (set-value v)
        (if (string-prefix? value v)
          (let* ([suffix (substring v (string-length value))]
                 [ed (get-editor)])
            (send ed lock #f)
            (send ed insert suffix (send ed get-end-position))
            (send ed lock #t))
          (super set-value v))
        (set! value v))

      (define/private (string-prefix? a b)
        (and (<= (string-length a) (string-length b))
             (for/and ([cha (in-string a)]
                       [chb (in-string b)])
               (char=? cha chb))))

      (super-new [label #f] [style '(multiple)])

      (define value (get-value))

      (init-field proof-controller)))

  )
