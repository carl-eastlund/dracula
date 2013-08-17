#lang dracula/kernel

(provide
  mutual-recursion
  mutual-induction)

(require
  dracula/prelude/core
  dracula/prelude/base/primitive
  dracula/prelude/base/shorthand
  dracula/prelude/base/match
  dracula/prelude/base/struct)

(begin-for-syntax

  (struct meta-mutual [name tags formals]
    #:property prop:set!-transformer
    (lambda (meta stx)
      (id-transform stx
        (meta-mutual-name meta))))

  (define-syntax-class/specialize mutual-id
    (static-binding meta-mutual?
      "mutual recursion specification")))

(define-syntax (mutual-recursion stx)
  (syntax-parse stx
    #:literals {define}
    [(_ rec:id
        (define (name:id formal:id ...)
          #:measure measure:expr
          body:expr)
        ...+)
     (define/syntax-parse fun:id
       (format-id (@ rec) "~a-function" (@ rec)))
     (define/syntax-parse desc:id
       (format-id (@ rec) "~a-structs-desc" (@ rec)))
     (define/syntax-parse comp:id
       (format-id (@ rec) "~a-structs-comp" (@ rec)))
     (define/syntax-parse [tag:id ...]
       (for/list {[name-id (in-list (@ name))]}
         (format-id name-id "~a-tag" name-id)))
     #'(begin
         (define-syntax rec
           (meta-mutual #'fun
             (list #'tag ...)
             (list (list #'formal ...) ...)))
         (structs #:desc desc #:comp comp (tag formal ...) ...)
         (define (fun x)
           #:measure
           (let-syntax
               {[name
                 (syntax-parser
                   [(_ (~var formal expr) ...)
                    #'(fun (tag formal ...))])]
                ...}
             (match x
               [(tag formal ...) measure]
               ...
               [_ 0]))
           (let-syntax
               {[name
                 (syntax-parser
                   [(_ (~var formal expr) ...)
                    #'(fun (tag formal ...))])]
                ...}
             (match x
               [(tag formal ...) body]
               ...)))
         (define (name formal ...)
           (fun (tag formal ...)))
         ...)]))

(define-syntax (mutual-induction stx)
  (syntax-parse stx
    #:literals {theorem}
    [(_ ind:id rec:mutual-id
        (~and def
          (theorem (thm:id quant:id ...)
            body:expr))
        ...)
     (define/syntax-parse rec:id (meta-mutual-name (@ rec.value)))
     (define/syntax-parse [tag:id ...] (meta-mutual-tags (@ rec.value)))
     (define/syntax-parse [(formal:id ...) ...]
       (meta-mutual-formals (@ rec.value)))
     (unless (= (length (@ tag)) (length (@ thm)))
       (wrong-syntax stx
         "expected ~a theorems, but found ~a theorems"
         (length (@ tag))
         (length (@ thm))))
     (for {[formals (in-list (@ formal))]
           [quants (in-list (@ quant))]
           [stx* (in-list (@ def))]}
       (unless (= (length formals) (length quants))
         (wrong-syntax stx*
           "expected ~a arguments, but found ~a arguments"
           (length formals)
           (length quants))))
     #'(begin
         (theorem (ind x)
           (match/implies x
             [(tag quant ...) body]
             ...)
           #:rule-classes []
           #:hints {["Goal" #:induct (rec x)]})
         (theorem (thm quant ...) body
           #:hints {["Goal" #:use [(ind (tag quant ...))]]})
         ...)]))
