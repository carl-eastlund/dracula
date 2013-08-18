#|
ACL2 quote, quasiquote, unquote, unquote-splicing.

I lifted quasiquote from http://www.cs.indiana.edu/chezracket/syntax-case/.
|#
#lang racket

(require "constants.rkt" ;; for nil
         (for-syntax syntax/stx))

(provide (rename-out [acl2-datum #%datum]
                     [acl2-quote quote]
                     [acl2-quasiquote quasiquote])
         unquote
         unquote-splicing)

(define-for-syntax (nil->empty stx)
  (cond [(stx-pair? stx)
         (let ([car-stx (stx-car stx)]
               [cdr-stx (stx-cdr stx)])
           (let ([new-car (nil->empty (stx-car stx))]
                 [new-cdr (nil->empty (stx-cdr stx))])
             (if (and (eq? car-stx new-car) (eq? cdr-stx new-cdr))
                 stx
                 #`(#,new-car . #,new-cdr))))]
        [(and (identifier? stx)
              (free-identifier=? stx #'nil)) #'()]
        [else stx]))

(define-syntax (acl2-quote stx)
  (syntax-case stx ()
    [(_ body) #`(quote #,(nil->empty #'body))]))

(define-syntax (acl2-datum stx)
  (syntax-case stx ()
    [(_ . datum) (syntax/loc stx (acl2-quote datum))]))

;; DV: Jan 6, 2006
;; I lifted this code from Dybvig and Waddell's portable syntax-case
;; implementation at http://www.cs.indiana.edu/chezracket/syntax-case/
;; Version 7.0 (9/2/2005)
;; There are two small modifications in the definition of `quasilist*'
;; 1.  stx-car was originally just `car'
;; 2.  stx-cdr was originally just `cdr'
(define-syntax acl2-quasiquote
  (letrec
                                        ; these are here because syntax-case uses bound-identifier=?,
                                        ; and we want the more precise free-identifier=?
      ((isquote? (lambda (x)
                   (and (identifier? x)
                        (free-identifier=? x (syntax acl2-quote)))))
       (islist? (lambda (x)
                  (and (identifier? x)
                       (free-identifier=? x (syntax list)))))
       (iscons? (lambda (x)
                  (and (identifier? x)
                       (free-identifier=? x (syntax cons)))))
       (quote-nil? (lambda (x)
                     (syntax-case x ()
                       ((quote? ()) (isquote? (syntax quote?)))
                       (_ #f))))
       (stx-list (lambda args (quasisyntax ((unsyntax-splicing args)))))
       (stx-cons (lambda (a b) (with-syntax ([a a] [b b]) (syntax (a . b)))))
       (quasilist*
        (lambda (x y)
          (let f ((x x))
            (if (null? x)
                y
                ;; DV: Jan 6, 2006
                ;; The original source used car and cdr instead of
                ;; stx-car and stx-cdr here.
                (quasicons (stx-car x) (f (stx-cdr x)))))))
       (quasicons
        (lambda (x y)
          (with-syntax ((x x) (y y))
            (syntax-case (syntax y) ()
              ((quote? dy)
               (isquote? (syntax quote?))
               (syntax-case (syntax x) ()
                 ((quote? dx)
                  (isquote? (syntax quote?))
                  (syntax (acl2-quote (dx . dy))))
                 (_ (if (null? (syntax dy))
                        (syntax (list x))
                        (syntax (cons x y))))))
              ((listp . stuff)
               (islist? (syntax listp))
               (syntax (list x . stuff)))
              (else (syntax (cons x y)))))))
       (quasiappend
        (lambda (x y)
          (let ((ls (let f ((x x))
                      (if (stx-null? x)
                          (if (quote-nil? y)
                              '()
                              (stx-list y))
                          (if (quote-nil? (stx-car x))
                              (f (stx-cdr x))
                              (stx-cons (stx-car x) (f (stx-cdr x))))))))
            (cond
             ((stx-null? ls) (syntax (acl2-quote ())))
             ((stx-null? (stx-cdr ls)) (stx-car ls))
             (else (with-syntax ((ps ls))
                     (syntax (append . ps))))))))
       (vquasi
        (lambda (p lev)
          (syntax-case p ()
            ((p . q)
             (syntax-case #'p (unquote unquote-splicing)
               ((unquote p)
                (if (= lev 0)
                    (quasilist* (syntax (p)) (vquasi (syntax q) lev))
                    (quasicons (quasicons (syntax (acl2-quote unquote))
                                          (quasi (syntax (p)) (- lev 1)))
                               (vquasi (syntax q) lev))))
               ((unquote-splicing p)
                (if (= lev 0)
                    (quasiappend (syntax (p)) (vquasi (syntax q) lev))
                    (quasicons (quasicons (syntax (acl2-quote unquote-splicing))
                                          (quasi (syntax (p)) (- lev 1)))
                               (vquasi (syntax q) lev))))
               (p (quasicons (quasi (syntax p) lev) (vquasi (syntax q) lev)))))
            (() (syntax (acl2-quote ()))))))
       (quasi
        (lambda (p lev)
          (syntax-case p (unquote unquote-splicing acl2-quasiquote)
            ((unquote p)
             (if (= lev 0)
                 (syntax p)
                 (quasicons (syntax (acl2-quote unquote))
                            (quasi (syntax (p)) (- lev 1)))))
            (((unquote p) . q)
             (if (= lev 0)
                 (quasilist* (syntax (p)) (quasi (syntax q) lev))
                 (quasicons (quasicons (syntax (acl2-quote unquote))
                                       (quasi (syntax (p)) (- lev 1)))
                            (quasi (syntax q) lev))))
            (((unquote-splicing p) . q)
             (if (= lev 0)
                 (quasiappend (syntax (p)) (quasi (syntax q) lev))
                 (quasicons (quasicons (syntax (acl2-quote unquote-splicing))
                                       (quasi (syntax (p)) (- lev 1)))
                            (quasi (syntax q) lev))))
            ((acl2-quasiquote p)
             (quasicons (syntax (acl2-quote quasiquote))
                        (quasi (syntax (p)) (+ lev 1))))
            ((p . q)
             (quasicons (quasi (syntax p) lev) (quasi (syntax q) lev)))
            (p (syntax (acl2-quote p)))))))
    (lambda (x)
      (syntax-case x ()
        ((_ e) (quasi (syntax e) 0))))))
