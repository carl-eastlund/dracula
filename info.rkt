#lang setup/infotab

;; setup-plt:

(define name "Dracula")

;; Planet:
(define blurb (list "Dracula: ACL2 theorem prover tools in DrRacket"))
(define release-notes (list "8.25: updated modular acl2 file metadata"))
(define categories '(devtools scientific))
(define primary-file "main.rkt")
(define repositories '("4.x"))
(define required-core-version "5.92")
(define homepage "http://www.ccs.neu.edu/home/cce/acl2/")

;; Package:

(define collection "dracula")
(define deps '["planet-schematics-random1"])

;; tools:

(define tools (list (list "tool.rkt" "drscheme")))
(define tool-names (list "Dracula"))
(define tool-icons (list "images/acl2-icon.png"))

;; documentation:

(define scribblings
  '[("guide/guide.scrbl" [multi-page] [language -30])
    ("reference/reference.scrbl" [multi-page] [language -30])])
