#lang racket/gui

(require drscheme/tool
         "dracula-interfaces.rkt"
         "dracula-drscheme-frame.rkt"
         "dracula-drscheme-tab.rkt"
         "dracula-proof-panel.rkt"
         "dracula-drscheme-definitions.rkt"
         "dracula-language-level.rkt")

(provide tool@)

(define-unit install-tool@
  (import dracula-interfaces^
          dracula-drscheme-frame^
          dracula-drscheme-tab^
          dracula-drscheme-definitions^
          dracula-language-level^
          drscheme:tool^)
  (export drscheme:tool-exports^)

  (drscheme:get/extend:extend-unit-frame dracula-drscheme-frame-mixin)
  (drscheme:get/extend:extend-tab dracula-drscheme-tab-mixin)
  (drscheme:get/extend:extend-definitions-text dracula-drscheme-definitions-mixin)

  (define (phase1)
    (drscheme:language:extend-language-interface
     dracula-language-level<%>
     dracula-default-language-level-mixin))

  (define (phase2)

    ;; Dracula language level
    (drscheme:language-configuration:add-language
     (make-dracula-language-level))

    ;; Modular ACL2 language level
    (drscheme:language-configuration:add-language
     (make-dracula-modular-language-level))

    ;; ACL2 mode
    (drscheme:modes:add-mode
     dracula-mode-name
     dracula-mode-surrogate
     dracula-mode-repl-submit
     dracula-mode-matches-language)

    ))

(define-compound-unit/infer tool@
  (import drscheme:tool^)
  (export drscheme:tool-exports^)
  (link dracula-interfaces@
        dracula-drscheme-frame@
        dracula-drscheme-tab@
        dracula-proof-panel@
        dracula-drscheme-definitions@
        dracula-language-level@
        install-tool@))
