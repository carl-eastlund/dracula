#lang racket/base

(require
  racket/require
  (path-up "self/require.rkt")
  (teachpack-in testing)
  (teachpack-in doublecheck)
  (dracula-in lang/defun)
  (dracula-in lang/theorems)
  "dynamic-rep.rkt"
  (for-syntax
    racket/base
    (cce-in syntax)
    "static-rep.rkt"
    "syntax-meta.rkt"))

(provide invoke-macro)

(define-for-syntax (expand-invoke stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ mod)
       (begin
         (unless (and (identifier? #'mod)
                      (syntax-meta? (syntax-local-value #'mod (lambda () #f)))
                      (module/static? (syntax->meta #'mod)))
           (syntax-error #'mod "expected the name of a module here"))
         (let* ([module/static (syntax->meta #:message "not a module" #'mod)]
                [module/dynamic (module/static-dynamic module/static)]
                [imports/static (module/static-imports module/static)]
                [exports/static (module/static-exports module/static)])
           (unless (null? imports/static)
             (syntax-error
              #'mod
              (format "unresolved imports:\n~s\nfrom exports:\n~s\n"
                      (map (lambda (port)
                             (map syntax-e (port/static-sig-names/external port)))
                           imports/static)
                      (map (lambda (port)
                             (map syntax-e (port/static-sig-names/external port)))
                           exports/static))))
           (let* ([fns (map port/static-sig-names/external exports/static)]
                  [args (map port/static-sig-args exports/static)]
                  [ths (map port/static-con-names/external exports/static)])
             (with-syntax ([dynamic module/dynamic]
                           [(fn ...) (map refresh-identifier (apply append fns))]
                           [(tmp ...) (generate-temporaries (apply append fns))]
                           [(args ...) (apply append args)]
                           [ths (map refresh-identifier (apply append ths))])
               (syntax/loc stx
                 (begin
                   (define impl
                     ((module/dynamic-implementation dynamic)
                      (empty-interface/dynamic)))
                   (define-values [tmp ...]
                     (apply values
                       (for/list ([f (in-list '(fn ...))])
                         (interface/dynamic-get-function impl f))))
                   (mutual-recursion (defun fn args (tmp . args)) ...)
                   (define-theorems "axiom" . ths)
                   (generate-report!)
                   (check-properties!)))))))])))

(define-syntax invoke-macro expand-invoke)
