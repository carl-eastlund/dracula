#lang racket/base

(require

  mzlib/etc
  racket/require
  (path-up "self/require.rkt")
  (cce-in require-provide)
  "keywords.rkt"
  "dynamic-rep.rkt"
  "../lang/defun.rkt"
  "../lang/do-check.rkt"
  "../lang/theorems.rkt"

  (for-syntax
    racket/base
    racket/list
    racket/match
    racket/block
    syntax/parse
    syntax/boundmap
    racket/require-transform
    (cce-in syntax)
    (cce-in values)
    (cce-in text)
    (path-up "self/module-path.rkt")
    "static-rep.rkt"
    "syntax-meta.rkt"
    "../proof/proof.rkt"
    "../proof/syntax.rkt"))

(provide module-macro)

(define-for-syntax (reloc stx #:src src)
  (datum->syntax stx (syntax-e stx) src stx))

(define-for-syntax (reloc* stxs #:src src)
  (for/list {[stx (in-list stxs)]}
    (reloc stx #:src src)))

(define-for-syntax (matcher . ids)
  (let* ([table (make-free-identifier-mapping)])
    (for ([id ids])
      (free-identifier-mapping-put! table id #t))
    (lambda (stx)
      (syntax-case stx ()
        [(name . _)
         (identifier? #'name)
         (free-identifier-mapping-get table #'name (lambda () #f))]
        [_ #f]))))

(define-for-syntax (expand-module-export port id)
  (let* ([source (port/static-source port)]
         [concrete (export/static-concrete port)]
         [internals (port/static-sig-names/internal port)]
         [externals (port/static-sig-names/external port)]
         [arguments (port/static-sig-args port)])
    (with-syntax ([exp/dynamic id]
                  [(put ...) (reloc* #:src source externals)]
                  [(get ...) (reloc* #:src source internals)])
      (values
       concrete
       (syntax/loc source
         (define-values ()
           (begin
             (for ([sym (in-list '(put ...))]
                   [fun (in-list (unchecked-arity (list get ...)))])
               (interface/dynamic-put-function! exp/dynamic sym fun))
             (values))))
        #'(begin)))))

(define-for-syntax (expand-module-import port id)
  (let* ([source (port/static-source port)]
         [abstract (import/static-abstract port)]
         [internals (port/static-sig-names/internal port)]
         [externals (port/static-sig-names/external port)]
         [arguments (port/static-sig-args port)]
         [axioms (port/static-con-names/internal port)])
    (with-syntax ([imp/dynamic id]
                  [(get ...) (reloc* #:src source externals)]
                  [(put ...) (reloc* #:src source internals)]
                  [(tmp ...) (reloc* #:src source
                               (generate-temporaries externals))]
                  [(args ...) (map (lambda (arg) (reloc* #:src source arg))
                                arguments)]
                  [axs axioms])
      (values
       abstract
       (syntax/loc source
         (begin
           (define-values [tmp ...]
             (apply values
               (for/list ([sym (in-list '(get ...))])
                 (interface/dynamic-get-function imp/dynamic sym))))
           ;; combining multiple imported functions into one definition
           (mutual-recursion (defun put args (tmp . args)) ...)
           ;; combining multiple imported axioms/theorems into one definition
           (define-theorems "axiom" . axs)))
        #'(begin)))))

(define-for-syntax (expand-definition stx)
  (syntax-case* stx (include-book :dir :system :teachpacks) text=?
    [(include-book file :dir :teachpacks)
     (string-literal? #'file)
     (let*-values ([(module-path)
                    (dracula-teachpack-syntax #:stx stx
                      (string-append (syntax-e #'file) ".rkt"))]
                   [(imports sources) (expand-import module-path)]
                   [(names) (map import-local-id imports)])
       (with-syntax ([spec module-path]
                     [(name ...) names]
                     [(temp ...) (generate-temporaries names)])
         (values stx
                 (syntax/loc stx (rename-below [temp name] ...))
                 (syntax/loc stx (require (rename-in spec [name temp] ...))))))]
    [(include-book file :dir :system)
     (string-literal? #'file)
     (values stx (syntax/loc stx (begin)) (syntax/loc stx (begin)))]
    [(include-book file)
     (string-literal? #'file)
     (syntax-error
      stx
      "modules cannot include local books; convert the book to a module")]
    [(include-book . _)
     (syntax-error
      stx
      "expected a book name with :dir :system or :dir :teachpacks")]
    [_ (values stx stx (syntax/loc stx (begin)))]))

(define-for-syntax (expand-module-body imp exp body)
  (cond
    [(import/static? body) (expand-module-import body imp)]
    [(export/static? body) (expand-module-export body exp)]
    [else (expand-definition body)]))

(define-for-syntax (expand-dynamic stx)
  (syntax-case stx ()
    [(_ static-name)
     (let* ([import-name #'imp/dynamic]
            [export-name #'exp/dynamic]
            [mod (syntax->meta #:message "not a module" #'static-name)]
            [source (module/static-source mod)])
       (let*-values ([(defs runs reqs)
                      (for/lists [defs runs reqs]
                          ([stx (in-list (module/static-body mod))])
                        (expand-module-body import-name export-name stx))])
       (with-syntax ([(run ...) runs]
                     [(req ...) reqs]
                     [dynamic-name (refresh-identifier
                                    (module/static-dynamic mod))])
         (annotate-part
           (make-part
             (syntax-e #'static-name)
             (syntax->loc stx)
             (map syntax->term defs))
          (syntax/loc source
            (begin
              req ...
              (define dynamic-name
                (make-module/dynamic
                 (lambda (imp/dynamic)
                   (define exp/dynamic (empty-interface/dynamic))
                   (begin-below run ...)
                   (interface/dynamic-join
                     exp/dynamic
                     imp/dynamic))))))))))]))

(define-for-syntax (parse-port make-port/static rev-ports stx)
  (syntax-case stx ()
    [(_ ifc [ext int] ...)
     (let* ([ifc (syntax->meta #:message "not an interface" #'ifc)])
       (make-port/static
        stx
        ifc
        (map cons
             (syntax->list #'(ext ...))
             (syntax->list #'(int ...)))
        (map (lambda (inc)
               (or
                (findf (lambda (port)
                         (interface/static-external=?
                          inc (port/static-interface port)))
                       rev-ports)
                (syntax-error stx
                              "no im/export of ~a (required by ~a)"
                              (syntax-e (interface/static-name inc))
                              (syntax-e (interface/static-name ifc)))))
             (interface/static-includes ifc))))]))

(define-for-syntax (parse-body prior stxs)
  (match stxs
    [(list) (list)]
    [(list* stx stxs)
     (syntax-case stx (import export)
       [(import . _)
        (block
          (define port
            (parse-port
              make-import/static
              (filter import/static? prior)
              stx))
          (cons port
            (parse-body (cons port prior) stxs)))]
       [(export . _)
        (block
          (define port
            (parse-port
              make-export/static
              prior
              stx))
          (cons port
            (parse-body (cons port prior) stxs)))]
       [_ (cons stx (parse-body prior stxs))])]))

(define-for-syntax (expand-static stx)
  (syntax-case stx ()
    [(_ static-name original def ...)
     (syntax/loc #'original
       (define-syntax static-name
         (make-syntax-meta
           (make-module/static
             #'static-name
             #'dynamic-name
             #'original
             (parse-body null (list (quote-syntax def) ...)))
           (expand-keyword "cannot be used as an expression"))))]))

(define-for-syntax (expand-module stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ static-name . body)
       (parameterize ([current-syntax stx])
         (with-syntax ([original stx])
           (syntax/loc stx
             (begin
               (define-syntaxes (define-static) expand-static)
               (define-syntaxes (define-dynamic) expand-dynamic)
               (define-static static-name original . body)
               (define-dynamic static-name)))))])))

(define-syntax module-macro expand-module)
