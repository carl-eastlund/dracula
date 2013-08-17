#lang mischief

(provide
  assign-unique-names)

(require
  dracula/legacy/type
  dracula/expansion/grammar
  dracula/legacy/registry
  dracula/legacy/ref
  dracula/proof/term)

(define (assign-unique-names x)
  (cond!
    [(list? x) (map assign-unique-names x)]
    [(pair? x)
     (cons (assign-unique-names (car x))
       (assign-unique-names (cdr x)))]
    [(syntax? x) (intern-ref x)]
    [else x]))

(define (intern-ref ref0)
  (define ref (canonicalize-ref ref0))
  (cond!
    [(syntax? ref) (entry-symbol (ref-entry ref))]
    [(false? ref) (impossible)]))

(define identifier~>entry
  (make-free-id-table))

(struct entry [type symbol fields] #:transparent)

(define (ref-entry stx)
  (syntax-parse stx

    [:var-expr
     (dict-ref! identifier~>entry (@ name)
       (lambda ()
         (define ty (dict-ref identifier~>type (@ name) #false))
         (build-entry ty (syntax-e (@ name)))))]

    [:deref-expr
     (define comp-entry (ref-entry (@ comp)))
     (define field-id
       (field-type-field
         (ref-type (@ field))))
     (define field~>entry (entry-fields comp-entry))
     (dict-ref! field~>entry field-id
       (lambda ()
         (define comp-ty (entry-type comp-entry))
         (define field~>type (component-type-field~>type comp-ty))
         (define ty (dict-ref field~>type field-id impossible))
         (build-entry ty (entry-symbol comp-entry) (syntax-e field-id))))]))

(define (build-entry ty . syms)
  (define fields (make-free-id-table))
  (define sym
    (cond!
      [(and (function-type? ty)
         (function-type-primitive ty))
       =>
       identity]
      [else
       (unique!-symbol
         (format-symbol "~a"
           (standard-symbol-name
             (string-upcase
               (string-join
                 (map symbol->string syms)
                 ".")))))]))
  (entry ty sym fields))

(define (unique!-symbol sym0)
  (define sym (unique-symbol sym0))
  (dict-set! symbol~>chosen? sym #true)
  sym)

(define (standard-symbol-name str)
  (list->string
    (for/list {[c (in-string str)]}
      (if (symbol-char? c) c #\_))))

(define (unique-symbol sym0)
  (check-unique-symbol sym0
    (lambda ()
      (unique-symbol/count sym0 2))))

(define (unique-symbol/count sym0 count)
  (check-unique-symbol (format-symbol "~a~a" sym0 count)
    (lambda ()
      (unique-symbol/count sym0 (add1 count)))))

(define (check-unique-symbol sym0 fail)
  (cond!
    [(dict-has-key? symbol~>chosen? sym0) (fail)]
    [else sym0]))

(define (symbol-upcase sym)
  (string->symbol
    (string-upcase
      (symbol->string sym))))

(define symbol~>chosen?
  (make-hasheq
    (make-alist dracula-package-imports #true)))
