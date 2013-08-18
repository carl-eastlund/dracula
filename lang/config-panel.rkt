#lang racket/gui
#|
Clicking "Show Details" in the choose language dialog will show a
config-panel as built below.
|#

(require
  "acl2-settings.rkt"
  "acl2-location-pref.rkt"
  "admit-before-run-pref.rkt"
  "find-acl2.rkt")

(provide attach-config-panel!)

;; Consume the parent panel and the super-class's settings-get/set
;; procedure, and (1) add a new panel for ACL2 options, and
;; (2) produce a new settings-get/set procedure.
(define (attach-config-panel! parent super-param)
  (let ([vp (new group-box-panel% 
              [parent parent]
              [label "ACL2 Options"]
              [alignment '(left center)])])
    (let* ([admit-before-run-cb
            (new check-box%
              [label "Admit Definitions Before Run"]
              [parent vp]
              [value (get-admit-before-run?)]
              [callback 
               (lambda (this e)
                 (set-admit-before-run? (send this get-value)))])]
           
           ;; The layout of this field and the button isn't stellar . . .
           [acl2-location-text
            (new text-field%
              [label "ACL2 Location"]
              [parent vp]
              [enabled #f] ;; can only change via the button below.
              [min-width 400]
              #;[init-value (path->string (get-acl2-location))])]
           [hp (new horizontal-pane%
                 [parent vp]
                 [alignment '(center center)])]
           [find-acl2-button
            (new button%
              [label "Change ACL2 Location"]
              [parent hp]
              [callback 
               (lambda (b e)
                 (cond [(find-acl2 #f)
                        => (lambda (new-path)
                             (send acl2-location-text
                               set-value (path->string new-path)))]
                   [else (void)]))])])
      
      ;; HACK -- there must be a better way to do this.
      ;; Remove some of the (inapplicable) `simple-settings' from view:
      #;
      (send (car (send parent get-children))
      ; the receiver here is the vertical panel that contains three
      ; group-box-panel%'s 
      ; (Input Syntax, Dynamic Properties, Output Syntax)
      change-children
      (lambda (children)
      (let ([input-options (car children)]
      [output-options (caddr children)])
      (remq input-options (remq output-options children)))))
      
      (case-lambda
        [()
         (cons (make-acl2-settings (send acl2-location-text get-value)
                 (send admit-before-run-cb get-value))
           (super-param))]
        [(settings)
         (super-param (cdr settings))
         (let ([a (car settings)])
           (send admit-before-run-cb set-value
             (acl2-settings-admit-before-run? a))
           (send acl2-location-text set-value
             (acl2-settings-acl2-loc a)))]))))


