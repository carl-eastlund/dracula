#lang dracula/kernel

(provide
  description
  ~description)

(require
  (for-syntax
    mischief)
  dracula/prelude/core/keywords
  dracula/prelude/core/support)

(define-syntax (description stx)
  (syntax-parse stx
    [(_ name:id body:expr ...)
     #'(define/declare-description define-values name body ...)]))

(define-syntax (~description stx)
  (syntax-parse stx
    [(_ name:id body:expr ...)
     #'(define/declare-description declare-values name body ...)]))

(define-syntax (define/declare-description stx)
  (syntax-parse stx
    [(_ def:id name:id body:expr ...)
     (with-new-scope
       (define body-stxs
         (expand-description-body (@ body)))
       (syntax-parse body-stxs
         #:literals {define-values
                     define-syntaxes
                     declare-values
                     declare-syntaxes
                     declare-synonyms}
         [((~or
             vdecl:val-decl
             sdecl:stx-decl
             vdefn:val-defn
             sdefn:stx-defn
             syn:syn-decl)
           ...)

          #:do
          [(define (label id) (out-of-scope id))
           (define (labels ids) (map label ids))
           (define (labels* idss) (map labels idss))]

          #:attr desc-name (derived (@ name))

          #:attr [val-decl-label 2] (labels* (@ vdecl.name))
          #:attr [val-defn-label 2] (labels* (@ vdefn.name))
          #:attr [stx-decl-label 2] (labels* (@ sdecl.name))
          #:attr [stx-defn-label 2] (labels* (@ sdefn.name))
          #:attr [val-decl-interned 2] (map-map intern-id (@ val-decl-label))
          #:attr [val-defn-interned 2] (map-map intern-id (@ val-defn-label))

          #:do
          [(define synonym-table (make-label-id-table))
           (for {[stx-ids (in-list (@ syn.stx-name))]
                 [val-ids (in-list (@ syn.val-name))]}
             (for {[stx-id (in-list stx-ids)]
                   [val-id (in-list val-ids)]}
               (define stx-id* (out-of-scope stx-id))
               (define val-id* (out-of-scope val-id))
               (when (dict-has-key? synonym-table stx-id*)
                 (wrong-syntax stx-id
                   "duplicate synonym for label"))
               (dict-set! synonym-table stx-id* val-id*)))
           (define (synonym label)
             (dict-ref? synonym-table label
               #:success (lambda (syn) #`(quote-syntax #,syn))
               #:failure (lambda () #'#false)))]

          #:attr [stx-decl-syn 2] (map-map synonym (@ stx-decl-label))
          #:attr [stx-defn-syn 2] (map-map synonym (@ stx-defn-label))

          #:attr [spliced 1] (filter-map declare->define body-stxs)

          ($ (begin

               (def {desc-name}
                 (#%plain-lambda {} '#:type
                   (#%plain-lambda {} '#:instance
                     (#%plain-lambda {}
                       spliced ...
                       (values
                         (?@ 'val-decl-interned vdecl.name) ... ...
                         (?@ 'val-defn-interned vdefn.name) ... ...)))))

               (define-syntax name
                 (meta-description
                   #'desc-name
                   (list
                     (meta-value-members
                       (list #'val-decl-label ...)
                       (list #'vdecl.name ...)
                       #false)
                     ...
                     (meta-syntax-members
                       (list #'stx-decl-label ...)
                       (list stx-decl-syn ...)
                       (list #'sdecl.name ...)
                       #'sdecl.body
                       #'sdecl.assoc)
                     ...
                     (meta-value-members
                       (list #'val-defn-label ...)
                       (list #'vdefn.name ...)
                       #'vdefn.body)
                     ...
                     (meta-syntax-members
                       (list #'stx-defn-label ...)
                       (list stx-defn-syn ...)
                       (list #'sdefn.name ...)
                       #'sdefn.body
                       #false)
                     ...)))))]))]))
