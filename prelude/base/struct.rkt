#lang dracula/kernel

(provide
  ~structs
  structs
  structs/internal
  ~structs/external
  is-struct?
  make-struct
  get-struct-field)

(require
  dracula/prelude/core
  dracula/prelude/base/primitive
  dracula/prelude/base/shorthand
  dracula/prelude/base/match
  (for-syntax
    mischief))

(begin-for-syntax

  (define-syntax-class type-id
    (pattern type:id
      #:attr pred (predicate-name (@ type))
      #:attr make (constructor-name (@ type))
      #:attr bool-thm (boolean-theorem-name (@ type))
      #:attr pred-thm (predicate-theorem-name (@ type))
      #:attr make-thm (constructor-theorem-name (@ type))
      #:attr get-thm (selector-theorem-name (@ type))
      #:attr not-thm (exclusion-theorem-name (@ type))
      #:attr size-thm (size-theorem-name (@ type))))

  (define-syntax-class (field-id ty)
    (pattern field:id
      #:attr get (selector-name ty (@ field))))

  (define (name fmt)
    (lambda (ty)
      (format-id ty #:source ty fmt ty)))

  (define predicate-name (name "~a?"))
  (define constructor-name (name "~a"))
  (define boolean-theorem-name (name "~a/boolean"))
  (define predicate-theorem-name (name "~a/predicate"))
  (define constructor-theorem-name (name "~a/elimination"))
  (define selector-theorem-name (name "~a/selectors"))
  (define exclusion-theorem-name (name "~a/exclusion"))
  (define size-theorem-name (name "~a/acl2-count"))

  (define (selector-name ty fd)
    (format-id ty #:source fd "~a-~a" ty fd)))

(define-syntax (is-struct? stx)
  (syntax-parse stx
    [(_ name:id arg:expr)
     (define/syntax-parse predicate:id
       (predicate-name (@ name)))
     #'(predicate arg)]))

(define-syntax (make-struct stx)
  (syntax-parse stx
    [(_ name:id arg:expr ...)
     (define/syntax-parse constructor:id
       (constructor-name (@ name)))
     #'(constructor arg ...)]))

(define-syntax (get-struct-field stx)
  (syntax-parse stx
    [(_ name:id field:id arg:expr)
     (define/syntax-parse selector:id
       (selector-name (@ name) (@ field)))
     #'(selector arg)]))

(define-syntax (~structs stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:desc desc:id)
          #:defaults {[desc #'Structs]})
        (~optional (~seq #:comp comp:id)
          #:defaults {[comp #'STRUCTS]})
        (type:id field:id ...) ...)

     #:fail-when (check-duplicate-identifier (@ type))
     "found a duplicate struct name"

     #'(begin
         (~description desc
           (~structs/external (type field ...) ...))
         (component comp #:> desc)
         (open comp))]))

(define-syntax (structs stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:desc desc:id)
          #:defaults {[desc #'Structs]})
        (~optional (~seq #:comp comp:id)
          #:defaults {[comp #'STRUCTS]})
        (type:id field:id ...) ...)

     #:fail-when (check-duplicate-identifier (@ type))
     "found a duplicate struct name"

     #'(begin
         (description desc
           (~structs/external (type field ...) ...))
         (component comp #:> desc
           (structs/internal (type field ...) ...))
         (open comp))]))

(define-syntax (~structs/external stx)
  (syntax-parse stx
    [(_ (type:id field:id ...) ...)

     #:fail-when (check-duplicate-identifier (@ type))
     "found a duplicate struct name"

     #'(begin
         (structs-declarations (type field ...) ...)
         (structs-contracts (type field ...) ...)
         (structs-macros (type field ...) ...))]))

(define-syntax (structs/internal stx)
  (syntax-parse stx
    [(_ (type:id field:id ...) ...)

     #:fail-when (check-duplicate-identifier (@ type))
     "found a duplicate struct name"

     #'(begin
         (structs-definitions (type field ...) ...)
         (structs-proofs (type field ...) ...))]))

(define-syntax (structs-macros stx)
  (syntax-parse stx
    [(_ (type field ...) ...)
     #'(begin
         (struct-macros type field ...)
         ...)]))

(define-syntax (struct-macros stx)
  (syntax-parse stx
    [(_ type:type-id (~var field (field-id (@ type))) ...)
     #'(define-match-type (type.pred (type.make field.get ...)))]))

(define-syntax (structs-declarations stx)
  (syntax-parse stx
    [(_ (type field ...) ...)
     #'(begin
         (struct-declarations type field ...)
         ...)]))

(define-syntax (struct-declarations stx)
  (syntax-parse stx
    [(_ type:type-id (~var field (field-id (@ type))) ...)
     #'(begin
         (~define (type.pred x))
         (~define (type.make field ...))
         (~define (field.get x))
         ...)]))

(define-syntax (structs-contracts stx)
  (syntax-parse stx
    [(_ (type field ...) ...)
     (define/syntax-parse {[other ...] ...}
       (for/list {[one (in-list (@ type))]}
         (for/list {[two (in-list (@ type))]
                    #:unless (bound-identifier=? one two)}
           two)))
     #'(begin
         (struct-contracts type {other ...} field ...)
         ...)]))

(define-syntax (structs-proofs stx)
  (syntax-parse stx
    [(_ (type field ...) ...)
     (define/syntax-parse {[other ...] ...}
       (for/list {[one (in-list (@ type))]}
         (for/list {[two (in-list (@ type))]
                    #:unless (bound-identifier=? one two)}
           two)))
     #'(begin
         (struct-proofs type {other ...} field ...)
         ...)]))

(define-syntax (struct-contracts stx)
  (syntax-parse stx
    [(_ type:id {other:id ...} field:id ...)
     #'(struct-theorems ~theorem type {other ...} field ...)]))

(define-syntax (struct-proofs stx)
  (syntax-parse stx
    [(_ type:id {other:id ...} field:id ...)
     #'(struct-theorems theorem type {other ...} field ...)]))

(define-syntax (struct-theorems stx)
  (syntax-parse stx
    [(_ thm:id type:id {other:id ...} field:id ...)
     #'(begin
         (struct-boolean-theorem thm type)
         (struct-predicate-theorem thm type field ...)
         (struct-constructor-theorem thm type field ...)
         (struct-selector-theorem thm type field ...)
         (struct-exclusion-theorem thm type {other ...} field ...)
         (struct-measure-theorem thm type field ...))]))

(define-syntax (struct-boolean-theorem stx)
  (syntax-parse stx
    [(_ thm:id type:type-id (~var field (field-id (@ type))) ...)
     #'(thm (type.bool-thm x)
         (boolean? (type.pred x))
         #:rule-classes {#:type-prescription []})]))

(define-syntax (struct-predicate-theorem stx)
  (syntax-parse stx
    [(_ thm:id type:type-id (~var field (field-id (@ type))) ...)
     #'(thm (type.pred-thm field ...)
         (type.pred (type.make field ...)))]))

(define-syntax (struct-constructor-theorem stx)
  (syntax-parse stx
    [(_ thm:id type:type-id (~var field (field-id (@ type))) ...)
     (define/syntax-parse rule
       (if (empty? (@ field)) '#:rewrite '#:elim))
     #'(thm (type.make-thm x)
         (implies (type.pred x)
           (equal? (type.make (field.get x) ...) x))
         #:rule-classes {rule []})]))

(define-syntax (struct-selector-theorem stx)
  (syntax-parse stx
    [(_ thm:id type:type-id (~var field (field-id (@ type))) ...)
     ($ (thm (type.get-thm field ...)
          (and (equal? (field.get (type.make field ...)) field) ...)
          #:rule-classes
            {(?@ #:rewrite
               [#:corollary (equal? (field.get (type.make field ...)) field)])
             ...}))]))

(define-syntax (struct-exclusion-theorem stx)
  (syntax-parse stx
    [(_ thm:id type:type-id {other:type-id ...}
        (~var field (field-id (@ type))) ...)
     ($ (thm (type.not-thm x)
          (implies (type.pred x)
            (and (not (other.pred x)) ...))
          #:rule-classes
            {(?@ #:rewrite
               [#:corollary (implies (type.pred x) (not (other.pred x)))])
             ...}))]))

(define-syntax (struct-measure-theorem stx)
  (syntax-parse stx
    [(_ thm:id type:type-id (~var field (field-id (@ type))) ...)
     (define/syntax-parse rule
       (if (empty? (@ field)) '#:rewrite '#:elim))
     ($ (thm (type.size-thm field ...)
          (and (< (size field) (size (type.make field ...))) ...)
          #:rule-classes
            {(?@ #:linear
               [#:corollary (< (size field) (size (type.make field ...)))])
             ...}))]))

(define-syntax (structs-definitions stx)
  (syntax-parse stx
    [(_ (type field ...) ...)
     #'(begin
         (struct-definitions type field ...)
         ...)]))

(define-syntax (struct-definitions stx)
  (syntax-parse stx
    [(_ type:type-id (~var field (field-id (@ type))) ...)

     #:attr [blank 1]
     (for/list {[fd (in-list (@ field))]} #'_)

     #:attr [pat 2]
     (for/list {[fd (in-list (@ field))]}
       (for/list {[other (in-list (@ field))]}
         (cond
           [(bound-identifier=? other fd) fd]
           [else #'_])))

     #'(begin
         (define (type.pred x)
           (match x
             [(list 'type blank ...) #true]
             [_ #false]))
         (define (type.make field ...)
           (list 'type field ...))
         (define (field.get x)
           (match x
             [(list 'type pat ...) field]))
         ...)]))
