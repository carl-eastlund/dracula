#lang dracula/kernel

(provide
  define
  ~define
  theorem
  ~theorem
  enable
  disable
  primitive
  primitive-in
  validated-primitive-in
  validate
  assert
  in-theory
  include-book
  fold-shorthand)

(require/provide
  (for-syntax
    dracula/prelude/core/input-streams))

(require
  (for-syntax mischief)
  dracula/prelude/core/keywords
  dracula/prelude/core/imported
  dracula/prelude/core/support)

(define-syntax (assert stx)
  (syntax-parse stx
    [(_ e:expr)
     (define/syntax-parse source stx)
     #'(define-values {}
         (begin0 (values)
           (#%assert e (quote-syntax source))))]))

(define-syntax (include-book stx)
  (syntax-parse stx
    [(_ book:str #:dir #:system)
     #'(define-values {}
         (begin0 (values)
           (#%plain-lambda {} '#:include-book 'book
             (#%plain-lambda {} '#:dir '#:system))))]))

(define-syntax (in-theory stx)
  (syntax-parse stx
    [(_ theory:expr)
     #'(define-values {}
         (begin0 (values)
           (#%plain-lambda {} '#:in-theory theory)))]))

(define-syntax (enable stx)
  (syntax-parse stx
    [(_ rune:expr ...)
     #'(#%plain-lambda {} '#:enable rune ...)]))

(define-syntax (disable stx)
  (syntax-parse stx
    [(_ rune:expr ...)
     #'(#%plain-lambda {} '#:disable rune ...)]))

(define-syntax (define stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     (define/syntax-parse temp:id
       (derived (@ name)))
     #'(begin
         (define-values {temp}
           (#%plain-lambda {} '#:function '#:no-measure '#:no-goals body))
         (define-syntaxes {name}
           (meta-thunk #'temp))
         (declare-synonyms [name temp]))]
    [(_ (name:id formal:id ...) meas:measure body:expr)
     #'(define-values {name}
         (#%plain-lambda {formal ...}
           '#:function
           meas.term ...
           '#:no-goals
           body))]))

(define-syntax (~define stx)
  (syntax-parse stx
    [(_ name:id)
     (define/syntax-parse temp:id
       (derived (@ name)))
     #'(begin
         (declare-values {temp}
           (#%plain-lambda {} '#:stub 0))
         (declare-syntaxes {name} temp
           (meta-thunk #'temp))
         (declare-synonyms [name temp]))]
    [(_ (name:id formal:id ...))
     (define/syntax-parse arity:nat
       (length (@ formal)))
     #'(begin
         (declare-values {name}
           (#%plain-lambda {} '#:stub 'arity)))]
    [(_ (name:id formal:id ...) meas:measure body:expr)
     #'(declare-values {name}
         (#%plain-lambda {formal ...}
           '#:function
           meas.term ...
           body))]))

(define-syntax (theorem stx)
  (syntax-parse stx
    [(_ (name:id formal:id ...)
        (~optional
          (~and #:disable (~bind [disable #'(in-theory (disable name))]))
          #:defaults {[disable #'(begin)]})
        body:expr
        rules:rule-classes
        h:hints)
     #'(begin
         (define-values {name}
           (#%plain-lambda {formal ...}
             '#:theorem
             rules.term ...
             h.term ...
             body))
         disable)]))

(define-syntax (~theorem stx)
  (syntax-parse stx
    [(_ (name:id formal:id ...)
        body:expr
        rules:rule-classes)
     #'(declare-values {name}
         (#%plain-lambda {formal ...}
           '#:theorem
           rules.term ...
           body))]))

(define-syntax (primitive stx)
  (syntax-parse stx
    [(_ name:id arity:nat symbol:id package:id)
     #'(define-values {}
         (begin0 (#%plain-app values)
           (#%plain-lambda {} '#:primitive name 'arity 'package 'symbol)))]))

(define-require-syntax (primitive-in stx)
  (syntax-parse stx
    [(_ spec:expr
        (~optional (~seq #:package default-package:id)
          #:defaults {[default-package #'ACL2]})
        [external:id
         arity:nat
         (~optional (~seq #:as internal:id)
           #:defaults {[internal (@ external)]})
         (~optional (~seq #:symbol symbol:id)
           #:defaults {[symbol (acl2-conventions (@ internal))]})
         (~optional (~seq #:package package:id)
           #:defaults {[package (@ default-package)]})]
        ...)
     (syntax-local-lift-module-end-declaration
       #'(begin
           (primitive internal arity symbol package)
           ...))
     #'(only-in spec [external internal] ...)]))

(begin-for-syntax
  (define default-validation-count 1024))

(define-syntax (validate stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:count count:exact-positive-integer)
          #:defaults {[count (to-syntax default-validation-count)]})
        (fun:expr st:expr ...+))
     (define/syntax-parse original stx)
     #'(define-values {}
         (begin0 (values)
           (let-syntax
               {[the-macro
                 (lambda {stx}
                   (define inputs (stream-take (stream-zip st ...) 'count))
                   (define outputs (map (arg+ apply fun) inputs))
                   (define/syntax-parse the-inputs inputs)
                   (define/syntax-parse the-outputs outputs)
                   #'(#%validate fun 'the-inputs 'the-outputs
                       (quote-syntax original)))]}
             (the-macro))))]))

(define-require-syntax (validated-primitive-in stx)
  (syntax-parse stx
    [(_ spec:expr
        (~or
          (~optional (~seq #:package default-package:id)
            #:defaults {[default-package #'ACL2]})
          (~optional (~seq #:count default-count:exact-positive-integer)
            #:defaults {[default-count (to-syntax default-validation-count)]}))
        ...
        [(external:id input:expr ...)
         (~optional (~seq #:count count:exact-positive-integer)
           #:defaults {[count (@ default-count)]})
         (~optional (~seq #:as internal:id)
           #:defaults {[internal (@ external)]})
         (~optional (~seq #:symbol symbol:id)
           #:defaults {[symbol (acl2-conventions (@ internal))]})
         (~optional (~seq #:package package:id)
           #:defaults {[package (@ default-package)]})]
        ...)
     (define/syntax-parse [arity ...] (map length (@ input)))
     (syntax-local-lift-module-end-declaration
       #'(begin
           (require (for-syntax (only-in spec [external internal] ...)))
           (begin
             (primitive internal arity symbol package)
             (validate #:count count (internal input ...)))
           ...))
     #'(only-in spec [external internal] ...)]))

(define-syntax (fold-shorthand stx)
  (syntax-parse stx
    #:literals {[...dots ... #:phase (add1 (syntax-local-phase-level))]}
    [(_ (name:id arg:id ...dots last:id)
        template:expr
        (~optional identity:expr))
     (define/syntax-parse default
       (cond
         [(@ identity) #'(syntax identity)]
         [else #'(wrong-syntax stx
                   "expected 1 or more argument(s)")]))
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_) default]
           [(_ e:expr {... ...} e0:expr)
            (foldr
              (lambda (arg-stx last-stx)
                (define mark (fresh-mark))
                (define/syntax-parse last (mark last-stx))
                (define/syntax-parse arg (mark arg-stx))
                (mark #'template))
              (@ e0)
              (@ e))]))]))
