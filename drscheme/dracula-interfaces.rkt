#lang racket/gui

(require drscheme/tool)

(provide dracula-interfaces^
         dracula-interfaces@)

(define-signature dracula-interfaces^
  (dracula-language-level<%>
   dracula-drscheme-frame<%>
   dracula-drscheme-tab<%>
   dracula-drscheme-definitions<%>
   dracula-proof-panel<%>))

(define-unit dracula-interfaces@
  (import drscheme:tool^)
  (export dracula-interfaces^)

  (define dracula-language-level<%>
    (interface (drscheme:language:language<%>)

      ;; dracula-mode : -> (Or 'acl2 'modular-acl2 #f)
      ;; Reports the Dracula mode for this language:
      ;; - 'acl2 means a single ACL2 proof/program
      ;; - 'modular-acl2 means a modular program with multiple proofs
      ;; - #f means not a Dracula language
      dracula-mode))

  (define dracula-drscheme-definitions<%>
    (interface (drscheme:unit:definitions-text<%>)

      ;; dracula-mode : -> Boolean
      ;; Reports the Dracula mode (based on language level, see above).
      dracula-mode

      ;; lock-up-to-position : Nat -> Void
      ;; Prevents editing from position 0 to the given n.
      lock-up-to-position

      ;; highlight/save : Nat Nat Color -> Void
      ;; Highlights a region and "saves" the information for unhighlight-saved.
      highlight/save

      ;; unhighlight-saved : -> Void
      ;; Unhighlights all "saved" regions from highlight/save.
      unhighlight-saved))

  (define dracula-proof-panel<%>
    (interface ()

      ;; set-dracula-mode : (Or 'modular-acl2 'acl2 #f) -> Void
      ;; Set the mode of Dracula interactions.
      set-dracula-mode

      ;; update-name : String -> Void
      ;; Select an ACL2 proof obligation by name.
      update-name

      ;; interrupt : -> Void
      ;; Interrupt an active computation.
      interrupt

      ;; start-acl2, stop-acl2 : -> Void
      ;; Start or stop an ACL2 session for a proof.
      start-acl2
      stop-acl2

      ;; undo-all, undo-one, to-cursor, admit-one, admit-all : -> Void
      ;; Move forward or backward in an ACL2 proof attempt.
      undo-all
      undo-one
      to-cursor
      admit-one
      admit-all

      ;; shut-down-acl2-controller : -> Void
      ;; Terminates an ACL2 process.
      shut-down-acl2-controller

      ;; save-output : -> Void
      ;; Saves the ACL2 output to a file.
      save-output

      ;; notify-proof-change : Nat -> Void
      ;; Lets the proof controller know its proof text is out of date.
      ;; The argument specifies how many terms are still unchanged.
      notify-proof-change))

  (define dracula-drscheme-tab<%>
    (interface (drscheme:unit:tab<%>)

      ;; toggle-show-proof : -> Void
      ;; Swaps whether the proof window should be shown or hidden
      toggle-show-proof

      ;; get-proof-event : -> (Event Proof)
      ;; Initiates recompilation, if necessary, and produces an event
      ;; whose result is the newly reconstructed proof.
      get-proof-event

      ;; get-cursor-location : -> Integer
      ;; Produces the user's current cursor location in the program.
      get-cursor-location

      ;; update-proof-state : State -> Void
      ;; Updates the highlighting in the definitions window to
      ;; reflect the new proof state
      update-proof-state

      ;; update-dracula-gui : -> Void
      ;; Notifies the tab that the language settings may have changed,
      ;; so the Dracula GUI may need to appear / disappear.
      update-dracula-gui

      ;; update-dracula-proof : -> Void
      ;; Notifies the tab that the proof text may have changed,
      ;; so the GUI will need to re-read it before its next action.
      update-dracula-proof

      ;; shutdown-dracula-proof : -> Void
      ;; Notifies the tab that it is about to close,
      ;; and must terminate the theorem prover.
      shutdown-dracula-proof))

  (define dracula-drscheme-frame<%>
    (interface (drscheme:unit:frame<%>)

      ;; dracula-proof-shown : Boolean -> Void
      ;; Report whether the Dracula proof window is shown to update the menu.
      dracula-proof-shown

      ;; get-dracula-menu-items : -> (Listof LabelledMenuItem)
      ;; Produce Dracula menu items.
      get-dracula-menu-items

      ;; get-dracula-gui-container : -> DragablePanel
      ;; Produces the parent GUI element of the Dracula proof GUI.
      get-dracula-gui-container)))
