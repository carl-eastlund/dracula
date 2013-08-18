#lang setup/infotab

;; Required for all packages
(define name "Dracula")

;; setup-plt:
(define compile-omit-paths
  (list "acl2/gui.rkt" "acl2/new-gui.rkt" "acl2/program-controller.rkt"))

;; tools:

(define tools (list (list "tool.rkt" "drscheme")))
(define tool-names (list "Dracula"))
(define tool-icons (list "images/acl2-icon.png"))

;; planet:

(define blurb '("Provides the Dracula language level for ACL2 emulation."))

(define release-notes
  '("8.22: Compatibility with Racket 5.2.0.4."))

(define categories '(devtools scientific))

(define homepage "http://www.ccs.neu.edu/home/cce/acl2/")

(define primary-file "lang/dracula.rkt")

(define required-core-version "4.2.5")

(define repositories '("4.x"))

(define scribblings
  '[("guide/guide.scrbl" [multi-page] [language -30])
    ("reference/reference.scrbl" [multi-page] [language -30])])
