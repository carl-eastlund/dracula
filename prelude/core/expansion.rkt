#lang mischief

(provide
  (all-defined-out))

(require
  macro-debugger/emit
  dracula/model/unparse
  (for-template
    dracula/kernel
    dracula/prelude/core/keywords))

(define (symbol-update sym0 sym)
  (define str (symbol->string sym))
  (cond!
    [(symbol-interned? sym0) (string->symbol str)]
    [(symbol-unreadable? sym0) (string->unreadable-symbol str)]
    [else (string->uninterned-symbol str)]))

(define (format-special-id base
          #:source [source base]
          #:context [context base]
          fmt . args)
  (to-syntax #:source source #:context context
    (symbol-update
      (syntax-e base)
      (syntax-e (apply format-id base fmt args)))))

(define (intern-id id)
  (to-syntax #:stx id
    (intern-symbol (to-datum id))))

(define (intern-symbol sym)
  (cond
    [(symbol-interned? sym) sym]
    [else (string->symbol
            (symbol->string sym))]))

(define-syntax-class label-id
  (pattern name:id
    #:attr interned (intern-id (@ name))))

(define (derived stx [suffix ""])
  (to-syntax #:stx stx
    (string->unreadable-symbol
      (format "~a~a" (to-datum stx) suffix))))

(define (acl2-conventions id0)
  (define str0 (symbol->string (syntax-e id0)))
  (define str (string-upcase (regexp-replace* #px"[?]" str0 "P")))
  (define id (to-syntax #:stx id0 (string->symbol str)))
  id)

(define (deref-transformer base-stx label-stx)
  (define/syntax-parse base:expr base-stx)
  (define/syntax-parse label:label-id label-stx)
  (id-transformer
    (const #'(#%deref base 'label.interned))))

(struct meta-proxy
  [target]
  #:transparent
  #:property prop:set!-transformer
  (lambda (meta stx)
    (define/syntax-parse name (meta-proxy-target meta))
    (id-transform stx #'name)))

(struct meta-thunk meta-proxy []
  #:transparent
  #:property prop:set!-transformer
  (lambda (meta stx)
    (define/syntax-parse name (meta-proxy-target meta))
    (id-transform stx #'(name))))

(struct meta-component meta-proxy [desc] #:transparent)
(struct meta-generic meta-proxy [range] #:transparent)

(struct meta-description meta-proxy [members] #:transparent)
(struct meta-value-members [labels names body] #:transparent)
(struct meta-syntax-members [labels syns names expr assoc] #:transparent)

(define-syntax-class/specialize desc-id
  (static-binding meta-description? "a description name"))
(define-syntax-class/specialize comp-id
  (static-binding meta-component? "a component name"))
(define-syntax-class/specialize gen-id
  (static-binding meta-generic? "a generic name"))
(define-syntax-class/specialize val-id
  (static-binding meta-thunk? "a value name"))

(define-splicing-syntax-class desc-spec/base
  #:attributes {term
                value
                delta
                static
                [refined-label 1]
                [refined-defn 1]}
  #:literals {where}

  (pattern
    (~seq static:desc-id #:where ~!
        {[label:label-id #:= refined-defn:expr] ...})
    #:attr term #'(where static [label.interned #:= refined-defn] ...)
    #:attr [refined-label 1] (@ label.interned)
    #:attr value (@ static.value)
    #:attr delta (@ static.delta))

  (pattern
    (where ~! static:desc-id
      [label:label-id #:= refined-defn:expr] ...)
    #:attr term #'(where static [label.interned #:= refined-defn] ...)
    #:attr [refined-label 1] (@ label.interned)
    #:attr value (@ static.value)
    #:attr delta (@ static.delta))

  (pattern (~and term static:desc-id)
    #:attr [refined-label 1] '()
    #:attr [refined-defn 1] '()
    #:attr value (@ static.value)
    #:attr delta (@ static.delta)))

(define-splicing-syntax-class desc-spec
  #:attributes {term
                value
                delta
                static
                dynamic
                [refined-label 1]
                [refined-defn 1]

                [val-defn-label 2]
                [val-defn-name 2]
                [val-defn-expr 1]

                [stx-defn-label 2]
                [stx-defn-name 2]
                [stx-defn-expr 1]

                [val-decl-label 2]
                [val-decl-name 2]

                [stx-decl-label 2]
                [stx-decl-name 2]
                [stx-decl-expr 1]}
  (pattern :desc-spec/base

    #:do
    [(define members
       (refine-members (@ refined-label) (@ refined-defn)
         (intro-members (@ delta)
           (meta-description-members (@ value)))))
     (define dynamic-stx
       (make-refine-expr
         (meta-proxy-target (@ value))
         (selectors (@ refined-label) members)
         (@ refined-defn)))
     (define-values {value-members syntax-members}
       (partition meta-value-members? members))
     (define-values {value-defns value-decls}
       (partition meta-value-members-body value-members))
     (define-values {syntax-decls syntax-defns}
       (partition meta-syntax-members-assoc syntax-members))

     (define val-defn-labels (map meta-value-members-labels value-defns))
     (define val-decl-labels (map meta-value-members-labels value-decls))
     (define stx-defn-labels (map meta-syntax-members-labels syntax-defns))
     (define stx-decl-labels (map meta-syntax-members-labels syntax-decls))

     (define val-defn-names (map meta-value-members-names value-defns))
     (define val-decl-names (map meta-value-members-names value-decls))
     (define stx-defn-names (map meta-syntax-members-names syntax-defns))
     (define stx-decl-names (map meta-syntax-members-names syntax-decls))

     (define val-defn-exprs (map meta-value-members-body value-defns))
     (define stx-defn-exprs (map meta-syntax-members-expr syntax-defns))
     (define stx-decl-exprs (map meta-syntax-members-expr syntax-decls))

     (for {[labels (in-list val-defn-labels)]
           [names (in-list val-defn-names)]
           [expr (in-list val-defn-exprs)]}
       (unless (syntax? expr)
         (when (empty? labels)
           (error 'desc-spec "empty, bodyless definition"))
         (wrong-syntax (first labels)
           "bodyless definition: ~a ~a ~a"
           (stylish-print-as-string labels)
           (stylish-print-as-string names)
           (stylish-print-as-string expr))))]

    #:attr dynamic dynamic-stx

    #:attr [val-defn-label 2] val-defn-labels
    #:attr [val-decl-label 2] val-decl-labels
    #:attr [stx-defn-label 2] stx-defn-labels
    #:attr [stx-decl-label 2] stx-decl-labels

    #:attr [val-defn-name 2] val-defn-names
    #:attr [val-decl-name 2] val-decl-names
    #:attr [stx-defn-name 2] stx-defn-names
    #:attr [stx-decl-name 2] stx-decl-names

    #:attr [val-defn-expr 1] val-defn-exprs
    #:attr [stx-defn-expr 1] stx-defn-exprs
    #:attr [stx-decl-expr 1] stx-decl-exprs))

(define (selectors labels members)
  (define syn-table (synonym-table members))
  (for/list {[label (in-list labels)]}
    (define syn
      (dict-ref syn-table label
        (lambda ()
          (wrong-syntax label
            "cannot refine non-existent label"))))
    (intern-id syn)))

(define (intro-members delta members)
  (for/list {[mem (in-list members)]}
    (cond!
      [(meta-value-members? mem) (intro-value-member delta mem)]
      [(meta-syntax-members? mem) (intro-syntax-members delta mem)])))

(define (intro-value-member delta mem)
  (match mem
    [(meta-value-members labels names body)
     (meta-value-members
       (shadow* delta labels)
       (intro* names)
       (and body (intro body)))]))

(define (intro-syntax-members delta mem)
  (match mem
    [(meta-syntax-members labels syns names expr assoc)
     (meta-syntax-members
       (shadow* delta labels)
       (shadow* delta syns)
       (map intro names)
       (intro expr)
       (shadow delta assoc))]))

(define (intro stx)
  (syntax-local-introduce stx))

(define (intro* stxs)
  (map intro stxs))

(define (shadow delta stx)
  (and stx
    (syntax-local-introduce
      (syntax-local-get-shadower
        (syntax-local-introduce
          (delta stx))))))

(define (shadow* delta stxs)
  (for/list {[stx (in-list stxs)]}
    (shadow delta stx)))

(define (refine-members labels defns members)
  (define refined
    (refinement-table labels defns members))
  (for/list {[mem (in-list members)]}
    (refine-member refined mem)))

(define (refinement-table labels defns members)
  (define label~>synonym
    (synonym-table members))
  (define synonyms
    (for/list {[label (in-list labels)]}
      (define syn
        (dict-ref label~>synonym label
          (lambda ()
            (wrong-syntax label
              "no such label to refine"))))
      (unless syn
        (wrong-syntax label
          "cannot refine syntax-only label"))
      syn))
  (make-immutable-label-id-table
    (map cons synonyms defns)))

(define (synonym-table members)
  (make-immutable-label-id-table
    (for/append {[mem (in-list members)]}
      (member-synonyms mem))))

(define (member-synonyms mem)
  (cond!
    [(meta-value-members? mem) (value-member-synonyms mem)]
    [(meta-syntax-members? mem) (syntax-members-synonyms mem)]))

(define (value-member-synonyms mem)
  (define labels (meta-value-members-labels mem))
  (map cons labels labels))

(define (syntax-members-synonyms mem)
  (map cons
    (meta-syntax-members-labels mem)
    (meta-syntax-members-syns mem)))

(define (refine-member refined mem)
  (cond!
    [(meta-value-members? mem) (refine-value-member refined mem)]
    [(meta-syntax-members? mem) (refine-syntax-members refined mem)]))

(define (refine-value-member refined mem)
  (match mem
    [(meta-value-members labels names body)
     (define refinements (dict-ref* refined labels #false))
     (define new-body
       (cond!
         [(andmap false? refinements) body]
         [(andmap syntax? refinements)
          (when body
            (wrong-syntax (first refinements)
              "cannot refine label that is already defined"))
          ((fresh-mark) (make-values-expr refinements))]
         [else
          (wrong-syntax (first (filter syntax? refinements))
            "cannot refine grouped labels separately")]))
     (meta-value-members labels names new-body)]))

(define (refine-syntax-members refined mem)
  (match mem
    [(meta-syntax-members labels syns names expr assoc)
     (define refinement (and assoc (dict-ref refined assoc #false)))
     (define new-assoc
       (cond!
         [refinement
          (unless assoc
            (wrong-syntax refinement
              "cannot refine label that is already defined"))
          #false]
         [else assoc]))
     (meta-syntax-members labels syns names expr new-assoc)]))

(define (make-values-expr stxs)
  (match! stxs
    [(list stx) stx]
    [_ #`(values #,@stxs)]))

(define (dict-ref* dict keys default)
  (for/list {[key (in-list keys)]}
    (dict-ref dict key default)))

(define-syntax-class begin-defn
  #:no-delimit-cut
  #:literals {begin}
  (pattern (begin ~! body:expr ...)))

(define-syntax-class val-defn
  #:no-delimit-cut
  #:literals {define-values}
  (pattern (define-values ~! {name:id ...} body:expr)))

(define-syntax-class stx-defn
  #:no-delimit-cut
  #:literals {define-syntaxes}
  (pattern (define-syntaxes ~! {name:id ...} body:expr)))

(define-syntax-class val-decl
  #:no-delimit-cut
  #:literals {declare-values}
  (pattern (declare-values ~! {name:id ...} body:expr)))

(define-syntax-class stx-decl
  #:no-delimit-cut
  #:literals {declare-syntaxes}
  (pattern (declare-syntaxes ~! {name:id ...} assoc:id body:expr)))

(define-syntax-class syn-decl
  #:no-delimit-cut
  #:literals {declare-synonyms}
  (pattern (declare-synonyms ~! [stx-name:id val-name:id] ...)))

(define (declare->define stx)
  (syntax-parse stx
    [:val-defn stx]
    [:stx-defn stx]
    [:syn-decl #false]
    [:val-decl
     (to-syntax #:stx stx
       (list #'define-values (@ name) (@ body)))]
    [:stx-decl
     (to-syntax #:stx stx
       (list #'define-syntaxes (@ name) (@ body)))]))

(define (expand-description-body stxs)
  (append-map expand-description-term stxs))

(define (expand-description-term stx0)
  (define stx
    (expand-in-scope stx0
      #:stop-at
      (list
        #'begin
        #'define-values
        #'define-syntaxes
        #'declare-values
        #'declare-syntaxes
        #'declare-synonyms)))
  (syntax-parse stx
    [:begin-defn (expand-description-body (@ body))]
    [:val-defn #:fail-when #true "illegal definition in description" '()]
    [:val-decl (scope-bind-values! (@ name)) (list stx)]
    [:stx-defn (scope-bind-syntaxes/eval! (@ name) (@ body)) (list stx)]
    [:stx-decl (scope-bind-syntaxes/eval! (@ name) (@ body)) (list stx)]
    [:syn-decl (list stx)]))

(define-splicing-syntax-class measure
  #:attributes {[term 1]}
  (pattern (~seq #:measure ~! arg:expr)
    #:attr [term 1] (list #'(#%plain-lambda {} '#:measure arg)))
  (pattern (~seq)
    #:attr [term 1] (list #''#:no-measure)))

(define-splicing-syntax-class hints
  #:attributes {[term 1]}
  (pattern (~seq #:hints ~! [goal:goal ...])
    #:attr [term 1]
    (list #'(#%plain-lambda {} '#:goals goal.term ...)))
  (pattern (~seq)
    #:attr [term 1]
    (list #''#:no-goals)))

(define-syntax-class goal
  #:attributes {term}
  (pattern [name:str hint:hint ...]
    #:attr term #'(#%plain-lambda {} '#:goal name hint.term ...)))

(define-splicing-syntax-class hint
  #:attributes {term}
  (pattern (~seq #:by #:fiat)
    #:attr term #'(#%plain-lambda {} '#:by))
  (pattern (~seq #:by ~! inst:expr)
    #:attr term #'(#%plain-lambda {} '#:by inst))
  (pattern (~seq #:use ~! [inst:expr ...])
    #:attr term #'(#%plain-lambda {} '#:use inst ...))
  (pattern (~seq #:in-theory ~! theory:expr)
    #:attr term #'(#%plain-lambda {} '#:in-theory theory))
  (pattern (~seq #:induct ~! scheme:expr)
    #:attr term #'(#%plain-lambda {} '#:induct scheme)))

(define-splicing-syntax-class rule-classes
  #:attributes {[term 1]}
  (pattern (~seq #:rule-classes ~! [rule:rule-class ...])
    #:attr [term 1]
    (list #'(#%plain-lambda {} '#:rules rule.term ...)))
  (pattern (~seq)
    #:attr [term 1] (list #''#:no-rules)))

(define-splicing-syntax-class rule-class
  #:attributes {term}
  (pattern (~seq #:rewrite ~! [corollary:corollary-option])
    #:attr term #'(#%plain-lambda {} '#:rewrite corollary.term))
  (pattern (~seq #:forward-chaining ~! [corollary:corollary-option])
    #:attr term #'(#%plain-lambda {} '#:forward-chaining corollary.term))
  (pattern (~seq #:elim ~! [corollary:corollary-option])
    #:attr term #'(#%plain-lambda {} '#:elim corollary.term))
  (pattern (~seq #:type-prescription ~! [corollary:corollary-option])
    #:attr term #'(#%plain-lambda {} '#:type-prescription corollary.term))
  (pattern (~seq #:linear ~! [corollary:corollary-option])
    #:attr term #'(#%plain-lambda {} '#:linear corollary.term))
  (pattern
    (~seq #:definition ~!
      [(~or
         (~once corollary:corollary-option)
         (~once clique:clique-option))
       ...])
    #:attr term
    #'(#%plain-lambda {} '#:definition corollary.term clique.term))
  (pattern
    (~seq #:induction ~!
      [(~or
         (~once corollary:corollary-option)
         (~once pattern:pattern-option)
         (~once condition:condition-option)
         (~once scheme:scheme-option))
       ...])
    #:attr term
    #'(#%plain-lambda {} '#:induction
        corollary.term
        pattern.term
        condition.term
        scheme.term)))

(define-splicing-syntax-class clique-option
  #:attributes {term}
  (pattern (~seq #:controllers ~! [control:expr ...])
    #:attr term #'(#%plain-lambda {} '#:controllers control ...))
  (pattern (~seq #:clique ~! [fun:expr ...])
    #:attr term #'(#%plain-lambda {} '#:clique fun ...))
  (pattern (~seq) #:attr term #''#:no-clique))

(define-splicing-syntax-class corollary-option
  #:attributes {term}
  #:no-delimit-cut
  (pattern (~seq #:corollary ~! corollary:expr)
    #:attr term #'(#%plain-lambda {} '#:corollary corollary))
  (pattern (~seq) #:attr term #''#:no-corollary))

(define-splicing-syntax-class pattern-option
  #:attributes {term}
  #:no-delimit-cut
  (pattern (~seq #:pattern ~! pattern:expr)
    #:attr term #'(#%plain-lambda {} '#:pattern pattern)))

(define-splicing-syntax-class condition-option
  #:attributes {term}
  #:no-delimit-cut
  (pattern (~seq #:condition ~! condition:expr)
    #:attr term #'(#%plain-lambda {} '#:condition condition))
  (pattern (~seq) #:attr term #''#:no-condition))

(define-splicing-syntax-class scheme-option
  #:attributes {term}
  #:no-delimit-cut
  (pattern (~seq #:scheme ~! scheme:expr)
    #:attr term #'(#%plain-lambda {} '#:scheme scheme)))
