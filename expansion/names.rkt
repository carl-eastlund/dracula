#lang mischief

(provide
  program-names
  defns-names
  defn-names)

(require
  dracula/expansion/grammar)

(define (program-names stxs)
  (defns-names stxs))

(define (defns-names stxs)
  (append-map defn-names stxs))

(define (defn-names stx)
  (syntax-parse stx
    [:not-defn '()]
    [:begin-for-syntax-defn '()]
    [:require-defn '()]
    [:provide-defn '()]
    [:syntax-defn '()]
    [:in-theory-defn '()]
    [:book-defn '()]
    [:primitive-defn (list (@ prim-name))]
    [:function-defn (list (@ name))]
    [:theorem-defn (list (@ name))]
    [:component-defn (list (@ name))]
    [:generic-defn (list (@ name))]
    [:instance-defn (list (@ name))]
    [:description-defn (cons (@ name) (@ field-name))]
    [:function-decl (list (@ name))]
    [:theorem-decl (list (@ name))]
    [:component-decl (list (@ name))]
    [:generic-decl (list (@ name))]))
