#lang racket

(require (lib "include.rkt")
         "check.rkt"
         "../private/planet.rkt")

(require (for-syntax (lib "moddep.rkt" "syntax")
                     (prefix-in acl2- "acl2-reader.rkt")
                     "acl2-module-v.rkt"
                     racket/path
                     syntax/path-spec
                     (cce text)))

(provide include-book)

(define-for-syntax include-table (make-hash))

(define-syntax include-book
  (lambda (stx)

    (when (eq? (syntax-local-context) 'expression)
      (raise-syntax-error
          #f "not valid as an expression" stx))

    (syntax-case* stx (:dir :system :teachpacks) text=?

      [(_ name-stx :dir :teachpacks)

       (string? (syntax-e #'name-stx))

       (let* ([name (string-append (syntax-e #'name-stx) ".rkt")]
              [path (simple-form-path
                     (resolve-path-spec
                      (datum->syntax #f name)
                      #'name-stx
                      stx))])
         (if (hash-has-key? include-table path)
           (if (hash-ref include-table path)
             #'(begin)
             (raise-syntax-error #f
               (format "cannot include book ~s from inside itself" name)
               stx))
           (with-syntax ([teachpack-spec
                          (datum->syntax
                           stx
                           (make-teachpack-require-syntax name))])
             (quasisyntax/loc stx
               (begin
                 (begin-for-syntax (hash-set! include-table '#,path #f))
                 (require-below teachpack-spec)
                 (begin-for-syntax (hash-set! include-table '#,path #t)))))))]

      [(_ name-stx :dir :system)
       (quasisyntax/loc stx (begin))]

      [(_ name-stx)
       (let* ([name (string-append (syntax-e #'name-stx) ".lisp")]
              [path (simple-form-path
                     (resolve-path-spec
                      (datum->syntax #f name)
                      #'name-stx
                      stx))])
         (if (hash-has-key? include-table path)
           (if (hash-ref include-table path)
             #'(begin)
             (raise-syntax-error #f
               (format "cannot include book ~s from inside itself" name)
               stx))
           (quasisyntax/loc stx
             (begin
               (begin-for-syntax (hash-set! include-table '#,path #f))
               (include-at/relative-to/reader
                name-stx name-stx #,name acl2-read-syntax)
               (begin-for-syntax (hash-set! include-table '#,path #t))))))])))
