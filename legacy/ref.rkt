#lang mischief

(provide
  (contract-out
    [ref-type (-> ref? (or/c type? #false))]
    [ref->phrase (-> ref? string?)]
    [ref->string (-> ref? string?)]
    [empty-ref-dict (-> (dict/c syntax? any/c))]
    [canonicalize-ref (-> ref? ref?)]
    [rename-ref (-> ref? renaming/c ref?)]
    [renaming/c contract?]))

(require
  dracula/legacy/type
  dracula/expansion/grammar
  dracula/legacy/registry)

(define (ref->phrase ref)
  (format "~a ~a"
    (ref->preposition ref)
    (ref->string ref)))

(define (ref->preposition ref)
  (cond!
    [(false? ref) "with"]
    [(syntax? ref) "defined as"]))

(define (ref->string ref)
  (cond!
    [(false? ref) "no name"]
    [(syntax? ref)
     (with-output-to-string
       (lambda ()
         (print-ref-syntax ref)))]))

(define (print-ref-syntax ref)
  (syntax-parse ref
    [:var-expr (printf "~s" (syntax-e (@ name)))]
    [:deref-expr
     (print-ref-syntax (@ comp))
     (printf ".~s"
       (syntax-e
         (field-type-field
           (ref-type (@ field)))))]))

(define (rename-ref ref renaming)
  (cond!
    [(syntax? ref) (rename-syntax ref renaming)]
    [(false? ref) ref]))

(define (rename-syntax stx renaming)

  (define (traverse stx)
    (syntax-parse stx
      [:var-expr
       (dict-ref? renaming (@ name)
         #:success canonicalize-ref
         #:failure (const (@ name)))]
      [:deref-expr
       (make-deref-expr #:source stx
         (traverse (@ comp))
         (traverse (@ field)))]))

  (traverse stx))

(define (canonicalize-ref stx)

  (define (traverse stx)
    (syntax-parse stx
      [:var-expr
       (dict-ref? identifier~>type (@ name)
         #:failure (lambda () (values stx #false))
         #:success (lambda (ty) (values (type-ref ty) ty)))]
      [:deref-expr
       (define-values {comp-ref comp-ty} (traverse (@ comp)))
       (define-values {field-ref field-ty} (traverse (@ field)))
       (cond
         [(and comp-ty field-ty)
          (define ty
            (field-of comp-ty
              (field-type-field
                field-ty)))
          (values (type-ref ty) ty)]
         [else
          (values
            (make-deref-expr #:source stx
              comp-ref
              field-ref)
            #true)])]))

  (define-values {ref ty}
    (traverse stx))

  ref)

(define (ref-type ref
          #:success [success identity]
          #:failure [failure (const #false)])
  (cond!
    [(syntax? ref) (ref-syntax-type ref #:success success #:failure failure)]
    [(false? ref) (failure)]))

(define (ref-syntax-type ref
          #:success [success identity]
          #:failure [failure (const #false)])
  (syntax-parse ref
    [:var-expr
     (dict-ref? identifier~>type (@ name)
       #:success success
       #:failure failure)]
    [:deref-expr
     (ref-syntax-type (@ comp)
       #:failure failure
       #:success
       (lambda (comp-ty)
         (ref-syntax-type (@ field)
           #:failure failure
           #:success
           (lambda (field-ty)
             (success
               (field-of comp-ty
                 (field-type-field field-ty)))))))]))

(define (field-of comp-ty field)
  (define field~>type (component-type-field~>type comp-ty))
  (define ty (dict-ref field~>type field impossible))
  ty)

(define (ref-dict-ref dict ref [default impossible])
  (define fail
    (cond!
      [(procedure? default) default]
      [else (const default)]))
  (ref-table-lookup (ref-dict-table dict) ref
    #:success ref-entry-value
    #:failure fail))

(define (ref-dict-set dict ref value)
  (ref-dict
    (ref-table-update (ref-dict-table dict) ref
      #:update
      (lambda (table id)
        (dict-set table id
          (ref-entry ref value))))))

(define (ref-dict-remove dict ref)
  (ref-dict
    (ref-table-update (ref-dict-table dict) ref
      #:update
      (lambda (table id)
        (dict-remove table id)))))

(define (ref-table-update id-table ref
                #:update f)
  (define (update ref f)
    (syntax-parse ref
      [:var-expr (f id-table (@ name))]
      [:deref-expr
       (update (@ comp)
         (lambda (comp-table id)
           (dict-update comp-table id
             (lambda (field-table)
               (f field-table
                  (field-type-field
                    (ref-type (@ field)))))
             (lambda ()
               (make-immutable-free-id-table)))))]))
  (update ref f))

(define (ref-table-lookup id-table ref
                #:success success
                #:failure failure)
  (define (lookup ref success)
    (syntax-parse ref
      [:var-expr
       (dict-ref? id-table (@ name)
         #:success success
         #:failure failure)]
      [:deref-expr
       (lookup (@ comp)
         (lambda (field-table)
           (define field-id
             (field-type-field
               (ref-type (@ field))))
           (dict-ref? field-table field-id
             #:success success
             #:failure failure)))]))
  (lookup ref success))

(define (ref-dict-count dict)
  (ref-dict-fold dict 0
    (lambda (k v n)
      (add1 n))))

(define (ref-dict-first dict)
  (non-empty
    (ref-dict-fold dict '()
      (lambda (k v alist)
        (list* (cons k v) alist)))))
(define (ref-dict-next dict iter) (non-empty (rest iter)))
(define (ref-dict-key dict iter) (car (first iter)))
(define (ref-dict-value dict iter) (cdr (first iter)))

(define (non-empty xs)
  (cond!
    [(empty? xs) #false]
    [(cons? xs) xs]))

(define (ref-dict-fold dict init f)
  (define (traverse id-table acc)
    (for/fold {[acc acc]} {[elem (in-dict-values id-table)]}
      (cond!
        [(ref-entry? elem)
         (define key (ref-entry-key elem))
         (define value (ref-entry-value elem))
         (f key value acc)]
        [else (traverse elem acc)])))
  (traverse (ref-dict-table dict) init))

(define ref-dict-prop
  (vector-immutable
    ref-dict-ref
    #false ;; set!
    ref-dict-set
    #false ;; remove!
    ref-dict-remove
    ref-dict-count ;; count
    ref-dict-first ;; iterate-first
    ref-dict-next ;; iterate-next
    ref-dict-key ;; iterate-key
    ref-dict-value)) ;; iterate-value

(struct ref-dict [table] #:transparent
  #:property prop:dict ref-dict-prop)

(struct ref-entry [key value])

(define (empty-ref-dict)
  (ref-dict (make-immutable-free-id-table)))

(define renaming/c
  (dict/c identifier? syntax?))
