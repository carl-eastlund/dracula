#lang setup/infotab

;; Package:

(define collection "dracula")
(define deps '["planet-schematics-random1"])

;; setup-plt:

(define name "Dracula")

(define compile-omit-paths
  (list
    "acl2/gui.rkt"
    "acl2/new-gui.rkt"
    "acl2/program-controller.rkt"))

;; tools:

(define tools (list (list "tool.rkt" "drscheme")))
(define tool-names (list "Dracula"))
(define tool-icons (list "images/acl2-icon.png"))

;; documentation:

(define scribblings
  '[("guide/guide.scrbl" [multi-page] [language -30])
    ("reference/reference.scrbl" [multi-page] [language -30])])
