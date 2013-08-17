#lang dracula/kernel

(provide
  implement-description
  bind-members
  define-syntaxes-for)

(require/provide
  (for-syntax
    dracula/prelude/core/expansion))

(require
  (for-syntax
    mischief)
  dracula/prelude/core/keywords
  dracula/prelude/core/imported)

(begin-for-syntax

  (define (expand-instance-body stxs)
    (for/append-lists {names body-stxs} {[stx (in-list stxs)]}
      (expand-instance-term stx)))

  (define (expand-instance-term stx0)
    (define stx
      (expand-in-scope stx0
        #:stop-at (list #'begin #'define-values #'define-syntaxes)))
    (syntax-parse stx
      #:literal-sets {kernel-literals}
      [(begin ~! body:expr ...) (expand-instance-body (@ body))]
      [(define-syntaxes ~! {name:id ...} body:expr)
       (scope-bind-syntaxes/eval! (@ name) (@ body))
       (values '() (list stx))]
      [(define-values ~! {name:id ...} body:expr)
       (scope-bind-values! (@ name))
       (values (@ name) (list stx))]
      [_ (values '() (list stx))])))

(define-syntax (implement-instance stx)
  (syntax-parse stx
    [(_ defn:expr ...)
     (define-values {names stxs}
       (with-new-scope
         (expand-instance-body (@ defn))))
     (define/syntax-parse [name ...] names)
     (define/syntax-parse [label ...] (map intern-id names))
     (define/syntax-parse [body ...] stxs)
     ($ (#%instance
          (#%plain-lambda {}
            body ...
            (values (?@ 'label name) ...))))]))

(define-syntax (implement-description stx)
  (syntax-parse stx
    [(_ :desc-spec defn:expr ...)
     ($ (#%seal dynamic
          (implement-instance
            (define-syntaxes {val-decl-name}
              (values (rename-transformer #'val-decl-label)))
            ...
            ...
            (define-syntaxes {stx-decl-name}
              (values (rename-transformer #'stx-decl-label)))
            ...
            ...
            (define-syntaxes {val-defn-label}
              (values (rename-transformer #'val-defn-name)))
            ...
            ...
            (define-syntaxes {stx-defn-label}
              (values (rename-transformer #'stx-defn-name)))
            ...
            ...
            (define-values {val-defn-name ...} val-defn-expr)
            ...
            defn
            ...
            (define-syntaxes {stx-defn-name ...} stx-defn-expr)
            ...)))]))

(define-shorthand (define-syntaxes-for {name:id ...} assoc:id body:expr)
  (define-syntaxes {name ...} body))

(begin-for-syntax

  (define-splicing-syntax-class (member-prefix base)
    #:attributes {prefix}
    (pattern (~seq #:prefix ~! prefix:id))
    (pattern (~seq #:dotted-prefix ~! dotted:id)
      #:attr prefix (format-id (@ dotted) "~a." (@ dotted)))
    (pattern (~seq #:dotted ~!)
      #:attr prefix (format-id base "~a." base))
    (pattern (~seq #:no-prefix ~!)
      #:attr prefix #'||))

  (define-splicing-syntax-class member-binding
    #:attributes {bind-derefs bind-syntaxes}
    (pattern (~seq #:declare ~!)
      #:attr bind-derefs #'declare-derefs
      #:attr bind-syntaxes #'declare-syntaxes)
    (pattern (~seq #:define ~!)
      #:attr bind-derefs #'define-derefs
      #:attr bind-syntaxes #'define-syntaxes-for)))

(define-shorthand (define-derefs base:expr [name:id label:label-id] ...)
  (begin (define-values {name} (#%deref base 'label.interned)) ...))

(define-shorthand (declare-derefs base:expr [name:id label:label-id] ...)
  (begin
    (declare-values {name}
      (#%plain-lambda {} '#:addr
        (#%deref base 'label.interned)))
    ...))

(define-syntax (bind-members stx)
  (syntax-parse stx
    [(_ name:comp-id
        (~or
          (~optional (~var || (member-prefix (@ name)))
            #:defaults {[prefix #'||]})
          (~optional :member-binding
            #:defaults {[bind-derefs #'define-derefs]
                        [bind-syntaxes #'define-syntaxes-for]}))
        ...)

     #:do {(define delta0 (@ name.delta))
           (define/syntax-parse [:desc-spec]
             (list (delta0 (meta-component-desc (@ name.value)))))
           (define/syntax-parse dyn
             (delta0 (meta-proxy-target (@ name.value))))
           (define (prefixed id)
             (format-special-id id #:source (@ name) #:context (@ name)
               "~a~a" (@ prefix) id))
           (define (pfx ids) (map prefixed ids))
           (define (pfx* idss) (map pfx idss))}

     #:attr [val-decl-dot 2] (pfx* (@ val-decl-label))
     #:attr [stx-decl-dot 2] (pfx* (@ stx-decl-label))
     #:attr [val-defn-dot 2] (pfx* (@ val-defn-label))
     #:attr [stx-defn-dot 2] (pfx* (@ stx-defn-label))

     #:do {(define marked (fresh-mark))
           (define (mark stxs) (map marked stxs))
           (define (mark* stxss) (map mark stxss))}

     #:attr [val-decl-name* 2] (mark* (@ val-decl-name))
     #:attr [stx-decl-name* 2] (mark* (@ stx-decl-name))
     #:attr [stx-decl-expr* 1] (mark (@ stx-decl-expr))
     #:attr [val-defn-name* 2] (mark* (@ val-defn-name))
     #:attr [stx-defn-name* 2] (mark* (@ stx-defn-name))
     #:attr [stx-defn-expr* 1] (mark (@ stx-defn-expr))

     #'(begin
         (bind-syntaxes
             {val-decl-name* ... ...
              stx-decl-name* ... ...
              val-defn-name* ... ...
              stx-defn-name* ... ...}
           dyn
           (values
             (rename-transformer #'val-decl-dot) ... ...
             (rename-transformer #'stx-decl-dot) ... ...
             (rename-transformer #'val-defn-dot) ... ...
             (rename-transformer #'stx-defn-dot) ... ...))
         (bind-derefs dyn
           [val-decl-dot val-decl-label] ... ...
           [val-defn-dot val-defn-label] ... ...)
         (begin
           (bind-syntaxes {stx-decl-dot ...} dyn stx-decl-expr*) ...
           (bind-syntaxes {stx-defn-dot ...} dyn stx-defn-expr*) ...))]))
