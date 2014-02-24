#lang racket

(require
  "do-check.rkt"
  racket/include
  racket/require
  (path-up "self/require.rkt")
  (for-syntax
    racket/match
    syntax/moddep
    (prefix-in acl2- "acl2-reader.rkt")
    (path-up "self/module-path.rkt")
    racket/path
    racket/syntax
    syntax/parse
    syntax/path-spec))

(provide include-book add-include-book-dir)

(begin-for-syntax

  (define include-table (make-hash))
  (define book-dir-table (make-hash))

  (define (start-include path) (hash-set! include-table path 'started))
  (define (finish-include path) (hash-set! include-table path 'finished))

  (define (path-for-include stx prefix-stx suffix)
    (define prefix (syntax-e prefix-stx))
    (define filename (string-append prefix suffix))
    (define path
      (simple-form-path
        (resolve-path-spec
          (datum->syntax #f filename)
          prefix-stx
          stx)))
    (match (hash-ref include-table path #false)
      [#false (list path filename)]
      ['finished '#:redundant]
      ['started
       (wrong-syntax stx
         "cannot include ~s from inside itself"
         filename)]))

  (define-syntax-class keyword-symbol
    #:attributes {}
    (pattern sym:id
      #:when (regexp-match?
               #px"^:[^:]" ;; colon followed by non-colon at start of symbol
               (symbol->string (syntax-e #'sym)))))

  (define-syntax-class custom-book-dir
    #:attributes {}
    (pattern key:keyword-symbol
      #:when (hash-has-key? book-dir-table (syntax-e #'key))))

  (define (register-book-dir! key path)
    (hash-set! book-dir-table key path)))

(define-syntax (include-book stx)

  (when (eq? (syntax-local-context) 'expression)
    (wrong-syntax stx "not valid as an expression"))

  (syntax-parse stx
    #:datum-literals
      {:dir
       :system
       :teachpacks
       :load-compiled-file
       :uncertified-okp
       :defaxioms-okp
       :skip-proofs-okp
       :ttags
       :doc}

    [(_ name:str :dir :teachpacks)
     (syntax-parse (path-for-include stx #'name ".rkt")
       [#:redundant #'(begin)]
       [{path filename}
        (define/syntax-parse spec
          (dracula-teachpack-syntax #:stx stx (syntax-e #'filename)))
        #'(begin
            (begin-for-syntax (start-include 'path))
            (require-below spec)
            (begin-for-syntax (finish-include 'path)))])]

    [(_ name:str)
     (syntax-parse (path-for-include stx #'name ".lisp")
       [#:redundant #'(begin)]
       [{path filename}
        #'(begin
            (begin-for-syntax (start-include 'path))
            (include-at/relative-to/reader name name filename acl2-read-syntax)
            (begin-for-syntax (finish-include 'path)))])]

    [(_ name:str
        (~or
          (~once
            (~or
              (~seq :dir :system)
              (~seq :dir _:custom-book-dir)))
          (~optional (~seq :load-compiled-file _))
          (~optional (~seq :uncertified-okp _))
          (~optional (~seq :defaxioms-okp _))
          (~optional (~seq :skip-proofs-okp _))
          (~optional (~seq :ttags _))
          (~optional (~seq :doc _:str)))
        ...)
     #'(begin)]))

(define-syntax (add-include-book-dir stx)
  (syntax-parse stx
    [(_ key:keyword-symbol path:str)
     #'(begin-for-syntax
         (register-book-dir! 'key 'path))]))
