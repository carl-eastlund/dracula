#lang mischief

(require/provide
  dracula/legacy/type/base
  dracula/legacy/type/value
  dracula/legacy/type/function
  dracula/legacy/type/theorem
  dracula/legacy/type/field
  dracula/legacy/type/description
  dracula/legacy/type/component
  dracula/legacy/type/generic)

(provide
  (contract-out
    [type->expr (-> type? syntax?)]
    [type->string (-> type? string?)]))

(define (type->string x)
  (cond!
    [(value-type? x) "a value"]
    [(function-type? x)
     (format "a function of arity ~a"
       (function-type-arity x))]
    [(theorem-type? x)
     (format "a theorem of arity ~a"
       (length (theorem-type-formals x)))]
    [(field-type? x) "a field accessor"]
    [(description-type? x) "a description"]
    [(component-type? x) "a component"]
    [(generic-type? x) "a generic"]))

(define (type->expr ty)

  (define/syntax-parse ref
    (ref->expr (type-ref ty)))

  (cond!

    [(value-type? ty) #'(value-type ref)]

    [(function-type? ty)
     (define/syntax-parse arity (sexp->expr (function-type-arity ty)))
     (define/syntax-parse prim (sexp->expr (function-type-primitive ty)))
     #'(function-type ref arity prim)]

    [(theorem-type? ty)
     (define/syntax-parse formals
       (list->expr (theorem-type-formals ty) id->expr))
     (define/syntax-parse body
       (syntax->expr (theorem-type-body ty)))
     (define/syntax-parse rule-classes
       (optional->expr (theorem-type-rule-classes ty)
         (arg+-right list->expr id->expr)))
     #'(theorem-type ref formals body rule-classes)]

    [(field-type? ty)
     (define/syntax-parse field (syntax->expr (field-type-field ty)))
     #'(field-type ref field)]

    [(description-type? ty)
     (define/syntax-parse fields
       (list->expr (description-type-fields ty) id->expr))
     (define/syntax-parse field~>role
       (id-table->expr (description-type-field~>role ty) sexp->expr))
     (define/syntax-parse field~>type
       (id-table->expr (description-type-field~>type ty) type->expr))
     (define/syntax-parse field~>name
       (id-table->expr (description-type-field~>name ty) id->expr))
     (define/syntax-parse decls
       (list->expr (description-type-defns ty) syntax->expr))
     #'(description-type
         ref
         fields
         field~>role
         field~>type
         field~>name
         decls)]

    [(component-type? ty)
     (define/syntax-parse desc
       (type->expr (component-type-description ty)))
     (define/syntax-parse field~>type
       (id-table->expr (component-type-field~>type ty) type->expr))
     #'(component-type ref desc field~>type)]

    [(generic-type? ty)
     (define/syntax-parse domain (type->expr (generic-type-domain ty)))
     (define/syntax-parse formal (syntax->expr (generic-type-formal ty)))
     (define/syntax-parse range (type->expr (generic-type-range ty)))
     #'(generic-type ref domain formal range)]))

(define (id-table->expr table value->expr)
  (define/syntax-parse {[key value] ...}
    (for/list {[(k v) (in-dict table)]}
      (list (id->expr k) (value->expr v))))
  #'(make-immutable-free-id-table
      (list (cons key value) ...)))

(define (optional->expr x/f value->expr)
  (cond!
    [x/f (value->expr x/f)]
    [else #'#false]))

(define (list->expr xs value->expr)
  (define/syntax-parse [value ...]
    (map value->expr xs))
  #'(list value ...))

(define (id->expr id)
  (syntax->expr id))

(define (ref->expr ref)
  (cond!
    [(syntax? ref) (syntax->expr ref)]
    [(false? ref) (sexp->expr ref)]))

(define (sexp->expr x)
  #`(quote #,x))

(define (syntax->expr stx)
  #`(quote-syntax #,stx))
