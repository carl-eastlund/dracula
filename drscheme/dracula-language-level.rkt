#lang racket/gui

(require
  drscheme/tool
  drscheme/tool-lib
  framework
  rackunit/gui
  racket/require
  (path-up "self/require.rkt")
  (path-up "self/module-path.rkt")
  (cce-in drscheme)
  (dracula-in lang/printer)
  "dracula-interfaces.rkt"
  (prefix-in acl2-
    (dracula-in lang/acl2-reader)))

(provide
  dracula-language-level^
  dracula-language-level@)

(define-signature dracula-language-level^
  (dracula-default-language-level-mixin
   dracula-mode-language-level-mixin
   dracula-mode-name
   dracula-mode-surrogate
   dracula-mode-repl-submit
   dracula-mode-matches-language
   make-dracula-language-level
   make-dracula-modular-language-level))

(define-namespace-anchor the-anchor)

(define-unit make-dracula-language-level@
  (import drscheme:tool^ language-level^ dracula-interfaces^)
  (export dracula-language-level^)

  (define acl2-capabilities
    (make-immutable-hasheq
     (list
      (cons 'drscheme:define-popup (cons "(def" "(defun ...)"))
      (cons 'drscheme:language-menu-title "Dracula")
      (cons 'drscheme:special:insert-fraction #f)
      (cons 'drscheme:special:insert-lambda #f)
      (cons 'drscheme:special:insert-image #f)
      (cons 'drscheme:special:insert-comment-box #f)
      (cons 'drscheme:special:insert-gui-tool #f)
      (cons 'drscheme:special:slideshow-menu-item #f)
      (cons 'drscheme:special:insert-text-box #f)
      (cons 'drscheme:special:xml-menus #f))))

  (define dracula-default-language-level-mixin
    (mixin () (dracula-language-level<%>)
      (super-new)
      (define/public (dracula-mode) #f)))

  (define (dracula-mode-language-level-mixin mode)
    (mixin (dracula-language-level<%>) ()
      (super-new)
      (define/override (dracula-mode) mode)))

  (define modular-reader-module
    (dracula-module-path #:version? #f 'modular/lang/reader))

  (define modular-metadata-lines 4)

  (define (modular-settings->metadata modname settings)
    (string-append
      ";; The first four lines of this file were added by Dracula.\n"
      ";; They tell DrScheme that this is a Dracula Modular ACL2 program.\n"
      ";; Leave these lines unchanged so that DrScheme can properly load this file.\n"
      (match module-path:modular-acl2
        [(? symbol? sym) (format "#lang ~s\n" sym)]
        [(list 'planet arg) (format "#lang planet ~s\n" arg)])))

  (define (modular-metadata->settings metadata default) default)

  (define default-settings
    (let* ([case-sensitive? #f]
           [printing-style 'write]
           [fraction-style 'mixed-fraction]
           [show-sharing? #f]
           [insert-newlines? #t]
           [annotations 'none])
      (drscheme:language:make-simple-settings case-sensitive?
                                              printing-style
                                              fraction-style
                                              show-sharing?
                                              insert-newlines?
                                              annotations)))

  (define (default-settings? settings) #t)

  (define (config-panel parent)
    (new message%
      [parent parent]
      [label "Default Settings."])
    (case-lambda
      [() default-settings]
      [(settings) (void)]))

  (define (setup-rackunit lang settings)
    (define ns (namespace-anchor->namespace the-anchor))
    (define mod
      ((current-module-name-resolver)
       'rackunit/private/gui/drracket-link
       #false
       #false
       #true))
    (lambda ()
      (namespace-attach-module ns mod)))

  (define (dracula-language-level name path number summary mode . other-mixins)
    (apply make-language-level
           name path
           #:number number
           #:hierarchy (list (cons "Dracula" 2000))
           #:summary summary
           #:url "http://www.ccs.neu.edu/home/cce/acl2"
           #:reader acl2-read-syntax
           (language-level-render-mixin acl2-convert #f)
           (language-level-capability-mixin acl2-capabilities)
           ;;language-level-no-executable-mixin
           ;;language-level-macro-stepper-mixin
           language-level-check-expect-mixin
           (language-level-dynamic-setup-mixin setup-rackunit)
           (language-level-settings-mixin default-settings
                                          default-settings?
                                          config-panel)
           (dracula-mode-language-level-mixin mode)
           other-mixins))

  (define (make-dracula-language-level)
    (dracula-language-level
      "ACL2"
      module-path:acl2
      1000
      "ACL2 simulation & theorem prover interface"
      'acl2))

  (define (make-dracula-modular-language-level)
    (dracula-language-level
      "Modular ACL2"
      module-path:modular-acl2
      2000
      "ACL2 + module system"
      'modular-acl2
      (language-level-metadata-mixin
        modular-reader-module
        modular-metadata-lines
        modular-metadata->settings
        modular-settings->metadata)))

  (define dracula-mode-name "ACL2 mode")

  (define dracula-mode-surrogate
    (new
     (class* scheme:text-mode% (mode:surrogate-text<%>)
       (super-new)

       (define/override (put-file text sup directory default-name)
         (parameterize ([finder:default-filters
                         (list (list "Lisp (.lisp)" "*.lisp")
                               (list "Any" "*.*"))]
                        [finder:default-extension "lisp"])
           (sup directory default-name))))))

  (define (dracula-mode-repl-submit text [start 0] [end #f])
    (parameterize ([current-input-port
                    (open-input-text-editor
                     text start (or end (send text last-position)))])
      (with-handlers ([exn:fail:read:eof? (lambda (e) #f)]
                      [exn:fail:read? (lambda (e) #t)])
      (let loop () (or (eof-object? (acl2-read)) (loop))))))

  (define (dracula-mode-matches-language names)
    (match names
      [(list _ ... (pregexp "\\b(Dracula|ACL2?|Lisp)\\b") _ ...) #t]
      [_ #f]))

  )

(define-compound-unit/infer dracula-language-level@
  (import drscheme:tool^ dracula-interfaces^)
  (export dracula-language-level^)
  (link language-level@ make-dracula-language-level@))
