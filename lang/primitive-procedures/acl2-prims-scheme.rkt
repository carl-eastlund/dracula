#|
Many library primitives are implemented in Scheme here for efficiency.
(http://www.cs.utexas.edu/users/moore/acl2/v3-0/PROGRAMMING.html)
|#
#lang racket

(require (prefix-in srfi: srfi/1)
         "../constants.rkt"
         "../nil-macros.rkt"
         "../equality.rkt"
         "../defun.rkt"
         "../check.rkt"
         (for-syntax syntax/parse))

(provide define/c)

;; Need to make these turn into macros that check for 1st-order usage.
(define-syntax (define/c stx)
  (syntax-parse stx
    #:literals [rename-out case-lambda]

    [(_ f:id c:expr e:expr)
     #'(begin
         (define f
           (let ()
             (with-contract |Dracula Program|
               ([contracted any/c])
               (define/contract f c e)
               (define contracted f))
             contracted))
         (define-syntax macro
           (lambda (stx*)
             (syntax-parse stx*
               [(_ . xs) #'(f . xs)]
               [_ (raise-syntax-error 'f
                    "functions must appear in operator position" stx*)])))
         (provide (rename-out [macro f])))]

    [(_ ((rename-out [internal:id external:id]) . xs) c:expr e:expr)
     #'(begin
         (begin-below (defun macro xs (to-export . xs)))
         (define (internal . xs) e)
         (define to-export
           (let ()
             (with-contract |Dracula Program|
               ([contracted any/c])
               (define/contract external c internal)
               (define contracted external))
             contracted))
         (provide (rename-out [macro external])))]

    [(_ (f:id . xs) c:expr e:expr)
     #'(begin
         (begin-below (defun macro xs (f . xs)))
         (define f
           (let ()
             (with-contract |Dracula Program|
               ([contracted any/c])
               (define/contract (f . xs) c e)
               (define contracted f))
             contracted))
         (provide (rename-out [macro f])))]))

(define-syntax bool->CL
  (syntax-rules ()
    [(_ x) (if x t nil)]))

(define real/rational? rational?)
(define (acl2-symbol? x) (or (symbol? x) (null? x)))
(define symbolp/c (flat-named-contract "symbolp" acl2-symbol?))

(define nil/c (flat-named-contract "nil" null?))

(define (acl2-number? x) (number? x))
(define acl2-number/c (flat-named-contract "acl2-numberp" acl2-number?))
(define (non-zero-acl2-number? x) (and (acl2-number? x) (not (zero? x))))
(define (non-zero-real/rational? x) (and (real/rational? x) (not (zero? x))))

(define (eqlable? x)
  (or (acl2-number? x) (acl2-symbol? x) (char? x)))
(define/c (eqlablep x) (any/c . -> . any) (bool->CL (eqlable? x)))
(define eqlablep/c (flat-named-contract "eqlablep" eqlable?))

(define/c (intern$ name package)
  (-> string? string? symbol?)
  (match package
    ["ACL2" (string->symbol name)]
    ["KEYWORD" (string->symbol (string-append ":" name))]
    [_ (error 'intern$ "only packages ACL2 and KEYWORD are supported")]))

(define/c (intern-in-package-of-symbol name other)
  (-> string? (or/c symbol? null?) symbol?)
  (match other
    [(app symbol->string (regexp #rx"^:"))
     (string->symbol (string-append ":" name))]
    [_ (string->symbol name)]))

(define-syntax (intern stx)
  (syntax-case stx ()
    [(_ name "ACL2") (syntax (intern$ name "ACL2"))]
    [(_ name "KEYWORD") (syntax (intern$ name "KEYWORD"))]
    [(_ name package)
     (raise-syntax-error #f
       "only packages ACL2 and KEYWORD are supported"
       stx #'package)]
    [_
     (raise-syntax-error #f
       "expected two arguments"
       stx)]))
(provide intern)

(define/c (eq x y) 
  (([x any/c] [y (if (acl2-symbol? x) any/c symbolp/c)]) () . ->d . any)
  (bool->CL (acl2-equal? x y)))
(define/c (eql x y) 
  (([x any/c] [y (if (eqlable? x) any/c eqlablep/c)]) () . ->d . any)
  (bool->CL (acl2-equal? x y)))
(define/c (equal x y) 
  (any/c any/c . -> . any)
  (bool->CL (acl2-equal? x y)))

(define-syntax (acl2:* stx)
  (syntax-case stx (acl2:*)
    [(_ . args) #'(* . args)]
    [acl2:* (raise-syntax-error #f "Functions are allowed only in operator position" stx)]))
(provide (rename-out [acl2:* *]))

(define-syntax (acl2:+ stx)
  (syntax-case stx (acl2:+)
    [(_ . args) #'(+ . args)]
    [acl2:+ (raise-syntax-error #f "Functions are allowed only in operator position" stx)]))
(provide (rename-out [acl2:+ +]))

(define acl2:-
  (case-lambda
    [(x) (- x)]
    [(x y) (- x y)]
    ;; arity error detected by macro into which this defn expands.
    ))
(define-syntax (macro- stx)
  (syntax-case stx ()
    [(_ x) #'(acl2:- x)]
    [(_ x y) #'(acl2:- x y)]
    [(_ . xs) (raise-syntax-error #f 
                (format "Expects only one or two arguments, but given ~a"
                  (length (syntax->list #'xs)))
                stx)]
    [_else (raise-syntax-error
               #f "Functions are allowed only in operator position" stx)]))
(provide (rename-out [macro- -]))

(provide not-nil)

;; SYMBOLS

(define/c (symbolp x) (any/c . -> . any)
  (bool->CL (or (symbol? x) (null? x))))

;; LISTS

(define-syntax (a:list stx)
  (syntax-case stx (a:list)
    [(_ . xs) #'(list . xs)]
    [a:list (raise-syntax-error #f
              "Functions can only be used in operator position" stx)]))
(define-syntax (a:list* stx)
  (syntax-case stx (a:list*)
    [(_ . xs) #'(list* . xs)]
    [a:list* (raise-syntax-error #f
               "Functions can only be used in operator position" stx)]))
(provide (rename-out [a:list list] [a:list* list*]))

(define/c ((rename-out [a:cons cons]) x y) [any/c any/c . -> . any] (cons x y))

(define/c make-list
  (case-> [natural-number/c . -> . any]
          [natural-number/c (symbols ':initial-element) any/c . -> . any])
  (case-lambda 
    [(size) (srfi:make-list size nil)]
    [(size :initial-element fill) (srfi:make-list size fill)]))

(define/c (endp x) ((or/c nil/c pair?) . -> . any) (eq x nil))
(define/c ((rename-out [a:null null]) x) (any/c . -> . any) (eq x nil))

(define/c (consp x) (any/c . -> . any) (bool->CL (pair? x)))

(define (true-list? x) (or (nil? x)
                           (and (pair? x) (true-list? (cdr x)))))
(define/c (true-listp x)
  (any/c . -> . any)
  (bool->CL (true-list? x)))

(define/c ((rename-out [a:car car]) x) [(or/c nil/c (cons/c any/c any/c)) . -> . any]
  (if (pair? x) (car x) '()))
(define/c ((rename-out [a:cdr cdr]) x) [(or/c nil/c (cons/c any/c any/c)) . -> . any]
  (if (pair? x) (cdr x) '()))

(define/c (len x) (any/c . -> . any)
  (let loop ([x x] [answer 0]) 
    (if (pair? x) (loop (cdr x) (add1 answer)) answer)))

(define-syntax (acl2:append stx)
  (syntax-case stx (acl2:append)
    [(_ . xs) #'(append . xs)]
    [acl2:append (raise-syntax-error #f "Functions are only allowed on operator position" stx)]))
(provide (rename-out [acl2:append append]))

(define/c concatenate ;; FIX: this contract is too weak; needs to be ->d
  (case-> [(symbols 'string 'list) . -> . any]
          [(symbols 'string 'list)
           #:rest (or/c (listof string?) (listof (listof any/c)))
           . -> .
           any])
  (case-lambda
    [(result-type) (if (eq? result-type 'list) '() "")]
    [(result-type . args)
     (apply (if (eq? result-type 'list) append string-append) args)]))

(define/c (revappend lst acc) [(listof any/c) any/c . -> . any]
  (if (null? lst)
    acc
    (revappend (cdr lst) (cons (car lst) acc))))

(define/c ((rename-out [acl2-reverse reverse]) lst-or-str) 
  [(or/c string? (listof any/c)) . -> . any]
  (if (string? lst-or-str)
    (list->string (revappend (string->list lst-or-str) '()))
    (revappend lst-or-str '())))

;; SET operations
(define/c (intersectp-eq x y)
  ((listof acl2-symbol?) (listof acl2-symbol?) . -> . any)
  (cond [(nil? x) nil]
        [(memq (car x) y) t]
        [else (intersectp-eq (cdr x) y)]))

(define/c (intersectp-equal x y)
  ((listof any/c) (listof any/c) . -> . any)
  (cond [(nil? x) nil]
        [(member (car x) y) t]
        [else (intersectp-equal (cdr x) y)]))

(define/c (lognot x)
  [integer? . -> . any]
  (bitwise-not x))

(define/c logand
  (case->
   (-> any/c any)
   (-> #:rest (listof integer?) any))
  (case-lambda
    [(x) x]
    [args (apply bitwise-and args)]))

(define/c logior
  (case->
   (-> any/c any)
   (-> #:rest (listof integer?) any))
  (case-lambda
    [(x) x]
    [args (apply bitwise-ior args)]))

(define/c (logorc1 i j)
  [integer? integer? . -> . any]
  (logior (lognot i) j))

(define/c (logorc2 i j)
  [integer? integer? . -> . any]
  (logior i (lognot j)))

(define (binary-logeqv x y) (logand (logorc1 x y) (logorc1 y x)))
(define/c logeqv
  (case->
   (-> any/c any)
   (-> #:rest (listof integer?) any))
  (case-lambda
    [() -1]
    [args (srfi:fold binary-logeqv (car args) (cdr args))]))

(define/c logxor
  (case->
   (-> any/c any)
   (-> #:rest (listof integer?) any))
  (case-lambda
    [(x) x]
    [args (apply bitwise-xor args)]))

(define/c ((rename-out [a:expt expt]) x y) [acl2-number? integer? . -> . any]
  (expt x y))

;; Character Predicates
(define/c (characterp x) [any/c . -> . any]
  (bool->CL (char? x)))
(define/c (alpha-char-p x) [char? . -> . any]
  (bool->CL (char-alphabetic? x)))
(define/c (char-equal c1 c2) [char? char? . -> . any]
  (bool->CL (char-ci=? c1 c2)))
(define/c (char< c1 c2) [char? char? . -> . any]
  (bool->CL (char<? c1 c2)))
(define/c (char<= c1 c2) [char? char? . -> . any]
  (bool->CL (char<=? c1 c2)))
(define/c (char>= c1 c2) [char? char? . -> . any]
  (bool->CL (char>=? c1 c2)))
(define/c (char> c1 c2) [char? char? . -> . any]
  (bool->CL (char>? c1 c2)))

(define/c (char-code c) [char? . -> . any] (char->integer c))
(define/c (code-char i) [integer? . -> . any] (integer->char i))

(define/c digit-char-p
  (case-> (char? . -> . any)
          (char? (integer-in 2 36) . -> . any))
  (case-lambda
    [(x) (or (string->number (string x)) nil)]
    [(x base) (or (string->number (string x) base) nil)]))

(define/c (make-character-list chars) [any/c . -> . any]
  (let loop ([chars chars]
             [result '()])
    (cond [(pair? chars) (loop (cdr chars) (cons (if (char? (car chars))
                                                   (car chars)
                                                   (code-char 0))
                                                 result))]
          [else (reverse result)])))

;; STRINGS
(define/c (stringp x) [any/c . -> . any] (bool->CL (string? x)))

(define *standard-chars*
  '(#\Newline #\Space #\! #\" #\# #\$
    #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\.
    #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
    #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B
    #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L
    #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V
    #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\`
    #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
    #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
    #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~))
(define/c (standard-char-p c) [char? . -> . any]
  (if (memq c *standard-chars*) t nil))
(define (standard-string? s)
  (and (string? s) (andmap (lambda (x) (memq x *standard-chars*)) (string->list s))))
(define/c (standard-char-listp x) [any/c . -> . any]
  (bool->CL (let loop ([x x])
              (if (pair? x)
                (and (memq (car x) *standard-chars*)
                     (loop (cdr x)))
                (null? x)))))
(define/c (string-equal s1 s2) ;; case insensitive!
  [standard-string? standard-string? . -> . any]
  (bool->CL (string-ci=? s1 s2)))
(define/c (string< s1 s2) [string? string? . -> . any]
  (let loop ([posn 0]
             [s1 (string->list s1)]
             [s2 (string->list s2)])
    (cond [(null? s1) (if (null? s2) nil posn)]
          [(null? s2) nil]
          [(char<? (car s1) (car s2)) posn]
          [(char=? (car s1) (car s2)) (loop (add1 posn) (cdr s1) (cdr s2))]
          [else nil])))
(define/c (string<= s1 s2) [string? string? . -> . any]
  (if (string=? s1 s2) (string-length s1) (string< s1 s2)))
(define/c (string>= s1 s2) [string? string? . -> . any]
  (if (string=? s1 s2) (string-length s1) (string> s1 s2)))
(define/c (string> s1 s2) [string? string? . -> . any]
  (string< s2 s1))

(define/c (nth n l) (natural-number/c (listof any/c) . -> . any)
  (if (>= n (length l)) nil (list-ref l n)))

(define/c (coerce x result-type)
  (([x (case result-type
         [(list) string?]
         [(string) (listof char?)]
         [else any/c])] ;; 2nd contract will fail in this case
    [result-type (symbols 'list 'string)])
   ()
   . ->d . any)
  (case result-type
    [(list) (string->list x)]
    [(string) (list->string x)]))

(define/c (explode-nonnegative-integer n radix rest)
  [natural-number/c (or/c (=/c 2) (=/c 8) (=/c 10) (=/c 16)) any/c . -> . any]
  (append (string->list (number->string n radix)) rest))

;; Numeric Predicates
(define/c (acl2-numberp x) [any/c . -> . any]
  (bool->CL (acl2-number? x)))
(define/c (natp x) [any/c . -> . any]
  (bool->CL (and (integer? x) (<= 0 x))))

(define (complex-rational? val)
  (and (complex? val)
       (let ([real (real-part val)]
             [imag (imag-part val)])
         (and (rational? real)
              (rational? imag)
              (not (zero? imag))))))
(define/c (complex-rationalp x) [any/c . -> . any] (bool->CL (complex-rational? x)))

(define/c (zp x) [natural-number/c . -> . any] (bool->CL (zero? x)))

(define/c (evenp x) [integer? . -> . any] (bool->CL (even? x)))
(define/c (oddp x) [integer? . -> . any] (bool->CL (odd? x)))

(define/c (minusp x) [real/rational? . -> . any] (bool->CL (negative? x)))
(define/c (plusp x) [real/rational? . -> . any] (bool->CL (positive? x)))
(define/c (posp x) [any/c . -> . any] (bool->CL (and (integer? x) (positive? x))))

(define/c (integerp x) (any/c . -> . any) (bool->CL (integer? x)))
(define/c (rationalp x) (any/c . -> . any) (bool->CL (rational? x)))
(define/c (real/rationalp x) (any/c . -> . any) (rationalp x))

(define/c (integer-range-p low high int) (integer? integer? any/c . -> . any)
  (bool->CL (and (integer? int)
                 (<= low int)
                 (< int high))))

(define/c ((rename-out [a:min min]) x y) [real/rational? real/rational? . -> . any]
  (min x y))
(define/c ((rename-out [a:max max]) x y) [real/rational? real/rational? . -> . any]
  (max x y))

(define/c (unary-/ x) (non-zero-acl2-number? . -> . any) (/ x))

(define-syntax (macro/ stx)
  (syntax-case stx ()
    [(_ x) #'(/ x)]
    [(_ num den) #'(/ num den)]
    [(_ . xs) (raise-syntax-error #f
                (format "Expected one or two arguments, but got ~a"
                  (length (syntax->list #'xs)))
                stx)]
    [_else (raise-syntax-error
               #f "Functions are allowed only in operator position" stx)]))
(provide (rename-out [macro/ /]))

(define/c (nonnegative-integer-quotient num den)
  (natural-number/c (and/c natural-number/c (>/c 0)) . -> . any)
  (quotient num den))

(define/c ((rename-out [acl2:= =]) x y) (acl2-number? acl2-number? . -> . any)
  (bool->CL (= x y)))

(define/c ((rename-out [acl2:< <]) x y) [real/rational? real/rational? . -> . any] (bool->CL (< x y)))
(define/c ((rename-out [acl2:<= <=]) x y) [real/rational? real/rational? . -> . any] (bool->CL (<= x y)))
(define/c ((rename-out [acl2:>= >=]) x y) [real/rational? real/rational? . -> . any] (bool->CL (>= x y)))
(define/c ((rename-out [acl2:> >]) x y) [real/rational? real/rational? . -> . any] (bool->CL (> x y)))

(define/c (fix x) [any/c . -> . any] (if (number? x) x 0))

(define/c ((rename-out [acl2:floor floor]) num den) [real/rational? non-zero-real/rational? . -> . any]
  (floor (/ num den)))

(define/c (mod i j) [real/rational? non-zero-real/rational? . -> . any]
  (- i (* (acl2:floor i j) j)))

(provide/contract
 (rename acl2:not not [any/c . -> . any]))

;; ALISTS
(define (alist? x)
  (or (null? x)
      (and (pair? x) (pair? (car x)) (alist? (cdr x)))))
(define/c (alistp x) [any/c . -> . any] (bool->CL (alist? x)))

;; ORDERINGS
(define (both pred? x y) (and (pred? x) (pred? y)))
(define (complex/lex<= c1 c2)
  (let ([x1 (fix c1)]
        [y1 (fix c2)])
    (or (< (real-part x1) (real-part y1))
        (and (= (real-part x1) (real-part y1))
             (< (imag-part x1) (imag-part y1))))))
(define (atom? x) (not (pair? x)))
(define/c (alphorder a b) [atom? atom? . -> . any]
                                        ;(guard (and (atom a) (atom b)))
  (cond [(both rational? a b) (acl2:<= a b)]
        [(both complex-rational? a b) (complex/lex<= a b)]
        [(both char? a b) (bool->CL (char<=? a b))]
        [(both string? a b) (bool->CL (string<=? a b))]
        [(both symbol? a b) (bool->CL (string-ci<=? (symbol->string a) (symbol->string b)))]
        [(both null? a b) t]
        ;; type mismatch convention: rational < complex < char < string < symbol
        [(rational? a) t]
        [(rational? b) nil]
        [(complex-rational? a) t]
        [(complex-rational? b) nil]
        [(char? a) t]
        [(char? b) nil]
        [(string? a) t]
        [(string? b) nil]
        [(symbol? a) t]
        [(null? a) t]
        [else nil]))


(define/c ((rename-out [a:second second]) x) [(or/c nil/c pair?) . -> . any] (a:cadr x))
(define/c ((rename-out [a:caar caar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:car x)))
(define/c ((rename-out [a:cadr cadr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cdr x)))
(define/c ((rename-out [a:cdar cdar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:car x)))
(define/c ((rename-out [a:cddr cddr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cdr x)))

(define/c ((rename-out [a:third third]) x) [(or/c nil/c pair?) . -> . any] (caddr x))
(define/c ((rename-out [a:caaar caaar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:caar x)))
(define/c ((rename-out [a:caadr caadr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cadr x)))
(define/c ((rename-out [a:cadar cadar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cdar x)))
(define/c ((rename-out [a:caddr caddr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cddr x)))
(define/c ((rename-out [a:cdaar cdaar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:caar x)))
(define/c ((rename-out [a:cdadr cdadr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cadr x)))
(define/c ((rename-out [a:cddar cddar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cdar x)))
(define/c ((rename-out [a:cdddr cdddr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cddr x)))

(define/c ((rename-out [a:fourth fourth]) x) [(or/c nil/c pair?) . -> . any] (cadddr x))
(define/c ((rename-out [a:caaaar caaaar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:caaar x)))
(define/c ((rename-out [a:caaadr caaadr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:caadr x)))
(define/c ((rename-out [a:caadar caadar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cadar x)))
(define/c ((rename-out [a:caaddr caaddr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:caddr x)))
(define/c ((rename-out [a:cadaar cadaar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cdaar x)))
(define/c ((rename-out [a:cadadr cadadr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cdadr x)))
(define/c ((rename-out [a:caddar caddar]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cddar x)))
(define/c ((rename-out [a:cadddr cadddr]) x) [(or/c nil/c pair?) . -> . any] (a:car (a:cdddr x)))

(define/c ((rename-out [a:cdaaar cdaaar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:caaar x)))
(define/c ((rename-out [a:cdaadr cdaadr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:caadr x)))
(define/c ((rename-out [a:cdadar cdadar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cadar x)))
(define/c ((rename-out [a:cdaddr cdaddr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:caddr x)))
(define/c ((rename-out [a:cddaar cddaar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cdaar x)))
(define/c ((rename-out [a:cddadr cddadr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cdadr x)))
(define/c ((rename-out [a:cdddar cdddar]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cddar x)))
(define/c ((rename-out [a:cddddr cddddr]) x) [(or/c nil/c pair?) . -> . any] (a:cdr (a:cdddr x)))
