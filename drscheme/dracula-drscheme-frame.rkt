#lang racket/gui

(require drscheme/tool
         framework
         "dracula-interfaces.rkt"
         "executable-path.rkt")

(provide dracula-drscheme-frame^
         dracula-drscheme-frame@)

(define-signature dracula-drscheme-frame^
  (dracula-drscheme-frame-mixin))

(define-unit dracula-drscheme-frame@
  (import drscheme:tool^ dracula-interfaces^)
  (export dracula-drscheme-frame^)

  (define panel:weighted-dragable<%>
    (interface [panel:dragable<%>]
      after-weight-change
      set-weights
      get-weights))

  (define panel:weighted-dragable-mixin
    (mixin [panel:dragable<%>] [panel:weighted-dragable<%>]
      (inherit after-percentage-change get-children)

      (define/override (after-new-child child)
        (after-weight-change))

      (define/override (set-percentages ps)
        (set-weights (normalize ps #:total (length ps))))

      (define/override (get-percentages)
        (normalize (get-weights-for-children)))

      (define/override (place-children i w h)
        (sync-weights)
        (super place-children i w h))

      (define/override (on-subwindow-event r e)
        (parameterize ([upload? #f])
          (super on-subwindow-event r e)))

      (define/public (after-weight-change)
        (sync-weights)
        (after-percentage-change))

      (define/public (set-weights wts)
        (record-weights wts)
        (sync-weights))

      (define/public (get-weights) weight-list)

      (define/private (record-weights wts)
        (set! weight-list wts))

      (define/private (sync-weights)
        (if (upload?)
          (let* ([ps (get-percentages)])
            (unless (null? ps)
              (super set-percentages ps)))
          (let* ([ps (super get-percentages)])
            (record-weights
             (normalize ps #:total (apply + (get-weights-for-children)))))))

      (define/private (get-weights-for-children)
        (let* ([children (get-children)])
          (for/list ([ch (in-list children)]
                     [wt (in-sequences weight-list (in-cycle (list 1)))])
            wt)))

      (define/private (normalize ns #:total [total 1])
        (if (null? ns)
          null
          (let* ([ratio (/ total (apply + ns))])
            (for/list ([n (in-list ns)])
              (* n ratio)))))

      (init [weights null])

      (define weight-list #f)
      (define upload? (make-parameter #t))

      (record-weights weights)
      (super-new)
      (sync-weights)))

  (define panel:horizontal-weighted%
    (panel:weighted-dragable-mixin panel:horizontal-dragable%))

  (define dracula-drscheme-frame-mixin
    (mixin (drscheme:unit:frame<%>) (dracula-drscheme-frame<%>)
      (inherit get-current-tab)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  METHODS
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define/override (get-definitions/interactions-panel-parent)
        (let* ([top (super get-definitions/interactions-panel-parent)]
               [mid (new panel:horizontal-weighted%
                      [parent top]
                      [weights (list 2)])]
               [bot (new vertical-panel% [parent mid])])
          (set! dracula-gui-container mid)
          (queue-callback
           (lambda ()
             (send (get-current-tab) update-dracula-gui))
           #f)
          bot))

      (define/augment (on-tab-change old-tab new-tab)
        (queue-callback
         (lambda ()
           (send new-tab update-dracula-gui))
         #f)
        (inner (void) on-tab-change old-tab new-tab))

      (define/public (get-dracula-gui-container)
        (unless (is-a? dracula-gui-container panel:dragable<%>)
          (error 'Dracula
                 "~a\n~a: expected a dragable panel, got: ~e"
                 "internal error: GUI failed to initialize."
                 'get-dracula-gui-container
                 dracula-gui-container))
        dracula-gui-container)

      (define/public (get-dracula-menu-items)
        (unless (is-a? language-menu labelled-menu-item<%>)
          (error 'Dracula
                 "~a\n~a: expected a labelled menu item, got: ~e"
                 "internal error: GUI failed to initialize."
                 'get-dracula-menu-items
                 language-menu))
        (list acl2-show/hide-menu-item
              acl2-save-menu-item
              acl2-path-menu-item))

      (define/public (dracula-proof-shown shown?)
        (unless (is-a? language-menu labelled-menu-item<%>)
          (error 'Dracula
                 "~a\n~a: expected a labelled menu item, got: ~e"
                 "internal error: GUI failed to initialize."
                 'dracula-proof-shown
                 acl2-show/hide-menu-item))
        (send acl2-show/hide-menu-item set-label
          (if shown? "Hide ACL2 Window" "Show ACL2 Window")))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  INITIALIZATION
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define dracula-gui-container #f)

      (super-new)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  MENU
      ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (inherit get-language-menu)

      (define language-menu (get-language-menu))

      (new separator-menu-item% [parent language-menu])

      (define acl2-show/hide-menu-item
        (new menu-item%
             [parent language-menu]
             [label "Show/Hide ACL2 Window"]
             [callback (lambda _ (send (get-current-tab) toggle-show-proof))]))

      (define acl2-save-menu-item
        (new menu-item%
             [parent language-menu]
             [label "Save ACL2 Output As..."]
             [callback (lambda _ (send (get-current-tab) save-output))]))

      (define acl2-path-menu-item
        (new menu-item%
             [label "Change ACL2 Executable Path..."]
             [parent language-menu]
             [callback (lambda _ (prompt-and-set-executable-path))]))

      )))
