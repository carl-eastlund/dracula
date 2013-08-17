#lang dracula/kernel

(provide
  component
  ~component
  generic
  ~generic
  instance
  use
  ~use
  dot
  open
  ~open
  define-simple)

(require
  (for-syntax
    mischief)
  dracula/prelude/core/keywords
  dracula/prelude/core/support
  dracula/prelude/core/imported)

(define-shorthand (define-simple name:id term:expr)
  (define-values {name} term))

(define-shorthand (open name:comp-id ...)
  (begin (bind-members name) ...))
(define-shorthand (~open name:comp-id ...)
  (begin (bind-members name #:declare) ...))

(define-shorthand (use name:comp-id ...)
  (begin (bind-members name #:dotted) ...))
(define-shorthand (~use name:comp-id ...)
  (begin (bind-members name #:dotted #:declare) ...))

(define-syntax (dot stx)
  (syntax-parse stx
    [(_ name:comp-id label:id ...)
     (for/fold {[ref-stx (@ name)]} {[label-stx (in-list (@ label))]}
       (quasisyntax/loc stx
         (#%deref #,ref-stx '#,(intern-id label-stx))))]))

(define-syntax (component stx)
  (syntax-parse stx
    [(_ name:id #:> desc:desc-spec body:expr ...)
     (define/syntax-parse comp-name:id (derived (@ name)))
     #'(begin
         (define-values {comp-name}
           (implement-description desc.term body ...))
         (define-syntaxes {name}
           (meta-component #'comp-name #'desc.static))
         (declare-synonyms [name comp-name]))]))

(define-syntax (~component stx)
  (syntax-parse stx
    [(_ name:id #:> desc:desc-spec)
     (define/syntax-parse comp-name:id (derived (@ name)))
     #'(begin
         (declare-values {comp-name} desc.dynamic)
         (declare-syntaxes {name} comp-name
           (meta-component #'comp-name #'desc.static))
         (declare-synonyms [name comp-name]))]))

(define-syntax (generic stx)
  (syntax-parse stx
    [(_ (name:id formal:id #:> domain:desc-spec/base)
        #:> range:desc-spec/base
        body:expr ...)
     #'(generic (name [formal #:> domain.term]) #:> range.term body ...)]
    [(_ (name:id [formal:id #:> domain:desc-spec] ...)
        #:> range:desc-spec
        body:expr ...)
     (define/syntax-parse gen-name:id (derived (@ name)))
     (define/syntax-parse [gen-formal:id ...] (map derived (@ formal)))
     #'(begin
         (define-values {gen-name}
           (#%plain-lambda {gen-formal ...}
             (define-syntaxes {formal ...}
               (values (meta-component #'gen-formal #'domain.static) ...))
             (begin0 (implement-description range.term body ...)
               domain.dynamic
               ...)))
         (define-syntaxes {name}
           (meta-generic #'gen-name #'range.static))
         (declare-synonyms [name gen-name]))]))

(define-syntax (~generic stx)
  (syntax-parse stx
    [(_ (name:id formal:id #:> domain:desc-spec/base)
        #:> range:desc-spec/base)
     #'(~generic (name [formal #:> domain.term]) #:> range.term)]
    [(_ (name:id [formal:id #:> domain:desc-spec] ...)
        #:> range:desc-spec)
     (define/syntax-parse gen-name:id (derived (@ name)))
     (define/syntax-parse [gen-formal:id ...] (map derived (@ formal)))
     #'(begin
         (declare-values {gen-name}
           (#%plain-lambda {gen-formal ...}
             (define-syntaxes {formal ...}
               (values (meta-component #'gen-formal #'range.term) ...))
             (begin0 range.dynamic
               domain.dynamic
               ...)))
         (declare-syntaxes {name} gen-name
           (meta-component #'gen-name #'range.term))
         (declare-synonyms [name gen-name]))]))

(define-syntax (instance stx)
  (syntax-parse stx
    [(_ name:id (gen:gen-id arg:comp-id ...))
     (define/syntax-parse inst-name:id (derived (@ name)))
     (define/syntax-parse gen-dynamic (meta-proxy-target (@ gen.value)))
     (define/syntax-parse [arg-dynamic ...]
       (map meta-proxy-target (@ arg.value)))
     (define/syntax-parse desc
       ((@ gen.delta) (meta-generic-range (@ gen.value))))
     #'(begin
         (define-values {inst-name}
           (#%instantiate gen-dynamic arg-dynamic ...))
         (define-syntaxes {name}
           (meta-component #'inst-name #'desc))
         (declare-synonyms [name inst-name]))]))
