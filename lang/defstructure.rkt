#lang racket

(require
  racket/require
  (path-up "self/require.rkt")
  "defun.rkt"
  (for-syntax
    (cce-in text)
    (cce-in syntax)))

(provide defstructure)

(define-syntax bool->CL
  (syntax-rules ()
    [(bool->CL x) (if x 't '())]))

(define-for-syntax (->string x)
  (cond [(string? x) x]
        [(symbol? x) (symbol->string x)]
        [(identifier? x) (->string (syntax-e x))]
        [else (error '->string "Given ~a" x)]))

(define-for-syntax sym+
  (case-lambda
    [(x) (string->symbol (->string x))]
    [(x y) (string->symbol (string-append (->string x) (->string y)))]
    [args (foldl sym+ "" (reverse args))]))

(define-for-syntax (make-ctor-name name) name) 

(define-for-syntax (make-weak-predicate-name name)
  (datum->syntax name (sym+ 'weak- name '-p)))

(define-for-syntax (make-predicate-name name)
  (datum->syntax name (sym+ name '-p)))

(define-for-syntax (field-spec->field-name fs)
  (syntax-case fs ()
    [fn (identifier? #'fn) #'fn]
    [(fn . other-stuff) (identifier? #'fn) #'fn]
    [_ (raise-syntax-error #f "Not a field-spec" fs)]))

(define-for-syntax (field-spec->field-kwd fs)
  (sym+ ': (field-spec->field-name fs)))

(define-for-syntax (opt-specs->assertions fopts)
  (foldl (lambda (opt assertions)
           (syntax-case* opt (:assert) text=?
             [(:assert assertion . other-stuff) (cons #'assertion assertions)]
             [_ assertions]))
         '()
         fopts))

(define-for-syntax (collect-field-assertions field-specs)
  (foldl (lambda (fs assertions) 
           (syntax-case fs ()
             [fname 
              (identifier? #'fname)
              assertions]
             [(fname . options) 
              (append (opt-specs->assertions (syntax->list #'options))
                      assertions)]))
         '() field-specs))

(define-for-syntax (make-selector-names name field-specs)
  (define (make-name field-spec)
    (datum->syntax name (sym+ name '- (field-spec->field-name field-spec))))
  (map make-name field-specs))

(define-for-syntax (make-predicate name field-specs opt-specs)
  (with-syntax ([weak-predicate-name (make-weak-predicate-name name)]
                [(selector ...) (make-selector-names name field-specs)]
                [(fname ...) (map field-spec->field-name field-specs)]
                [(fassertion ...) (collect-field-assertions field-specs)]
                [(opt-assertion ...) (opt-specs->assertions opt-specs)])
    #'(lambda (object)
        (if (null? (weak-predicate-name object))
            '()
            (let-values ([(fname ...) (values (selector object) ...)])
              (bool->CL
               (not (or (null? fassertion) ...
                        (null? opt-assertion) ...
                        ))))))))

(define-for-syntax (make-field-offsets field-specs)
  (build-list (length field-specs) add1))

(define-for-syntax (make-updater-name name)
  (datum->syntax name (sym+ 'update- name)))

;; keyword keyword/value-list -> (union ACL2-value #f)
;; Produce the value to which the given keyword maps.
;; If the keyword is not mapped, produce #f.
(define (extract-new-value kwd kwd/val-list)
  (cond [(memq kwd kwd/val-list) => cadr]
        [else #f]))

(define-for-syntax (expand-defstructure stx)
  (syntax-case stx (:options)
    [(ds name field-spec ... (:options opts ...))
     (with-syntax ([(formal ...) (fresh-ids* #'(field-spec ...))]
                   [ctor-name-id (make-ctor-name #'name)]
                   [weak-predicate-name-id (make-weak-predicate-name #'name)]
                   [predicate-name-id (make-predicate-name #'name)]
                   [predicate-fn (make-predicate #'name (syntax->list #'(field-spec ...))
                                                 (syntax->list #'(opts ...)))]
                   [(selector-name-id ...) 
                    (make-selector-names #'name (syntax->list 
                                                 #'(field-spec ...)))]
                   [updater-name-id
                    (make-updater-name #'name)]
                   [(field-kwd-id ...) (map field-spec->field-kwd
                                            (syntax->list #'(field-spec ...)))]
                   [(offset-num ...) (make-field-offsets 
                                      (syntax->list #'(field-spec ...)))])
       (with-syntax ([(internal-ctor internal-weak internal-pred internal-updater) 
                      (fresh-ids*
                        #'[ctor-name-id
                           weak-predicate-name-id
                           predicate-name-id
                           updater-name-id])]
                     [(internal-selector ...)
                      (fresh-ids* #'(selector-name-id ...))])
         #'(begin
             (define (internal-ctor formal ...)
               (list (quote name) formal ...))
             (define (internal-weak x)
               (bool->CL
                (and (list? x) 
                     (= (length x) (length (quote (name offset-num ...))))
                     (eq? (car x) (quote name)))))
             (define-values (internal-selector ...)
               (values (lambda (x) (list-ref x offset-num)) ...))
             (define internal-pred 
               (let-syntax ([weak-predicate-name-id
                             (syntax-rules ()
                               [(_ e) (internal-weak e)])]
                            [selector-name-id 
                             (syntax-rules ()
                               [(_ e) (internal-selector e)])]
                            ...)
                 predicate-fn))
             
             ;; FIX:  Need to do more static checking.  In ACL2, the
             ;; updater is a macro that expects a literal kwd/val-list
             ;; and it checks that the keywords are all really field
             ;; names.  
             (define internal-updater
               (let-syntax ([ctor-name-id 
                             (syntax-rules ()
                               [(_ formal ...) (internal-ctor formal ...)])]
                            [selector-name-id 
                             (syntax-rules ()
                               [(_ e) (internal-selector e)])]
                            ...)
                 (lambda (obj . kwd/val-list)
                   ;; kwd/val-list : alternating list of kwds and values
                   (ctor-name-id
                    (or (extract-new-value 'field-kwd-id kwd/val-list)
                        (selector-name-id obj))
                    ...))))
             (mutual-recursion
              (defun ctor-name-id (formal ...) (internal-ctor formal ...))
              (defun weak-predicate-name-id (x) (internal-weak x))
              (defun predicate-name-id (x) (internal-pred x))
              (defun selector-name-id (x) (internal-selector x)) ...)
             (define-syntax updater-name-id  
               ;; this one is multi-arg, so don't arity-check 
               ;; like the others:
               (lambda (stx)
                 (syntax-case stx ()
                   [(_ignore)
                    (raise-syntax-error
                        #f
                      "Structure updater needs a structure to update"
                      stx)]
                   [(_ignore obj kwd/val (... ...))
                    #'(internal-updater obj kwd/val (... ...))]
                   [_else 
                    (raise-syntax-error 
                        #f 
                      "Functions may be used only in operator position."
                      stx)]))))))]
    [(ds name field-spec ...)
     (expand-defstructure #'(ds name field-spec ... (:options)))]
    [_ (raise-syntax-error 
        #f
        (format "Expected a structure name followed by field names, but got ~a"
                (syntax->datum stx))
        stx)]))

(define-syntax (defstructure stx)
  (expand-defstructure stx))
