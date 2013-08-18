#|
Defines many of the library procedures provided by ACL2:
http://www.cs.utexas.edu/users/moore/acl2/v3-0/PROGRAMMING.html

Other library functions are implemented in (file "acl2-prims-scheme.rkt").
|#
#lang racket

(require "../dracula-core.rkt"
         "../../private/planet.rkt"
         (prefix-in srfi: srfi/1)
         (prefix-in mz: racket)
         "acl2-prims-scheme.rkt")

(require (for-syntax (cce text)))

(provide (except-out (all-from-out "acl2-prims-scheme.rkt")
                     define/c
                     not-nil))

(define (false->nil x) (mz:if x x nil))

(define (racket->CL x) (mz:if x (mz:if (mz:eq? x #t) t x) nil))

(define (iterate n f x)
  (mz:if (mz:zero? n)
         x
         (iterate (mz:sub1 n) f (f x))))

(define (simple-procedure? f)
  (mz:and (mz:procedure? f)
          (mz:integer? (mz:procedure-arity f))))

(define (arity-matches? f g)
  (mz:= (mz:procedure-arity f)
        (mz:procedure-arity g)))

(define (format-variable-pair pair)
  (mz:format "\n~s = ~s" (mz:car pair) (mz:cdr pair)))

(define (format-variable-alist alist)
  (mz:apply mz:string-append (mz:map format-variable-pair alist)))

(define (guard-contract test sexp names)
  (mz:cond
   [(mz:not (simple-procedure? test))
    (mz:error 'guard-contract "not a fixed-arity procedure: ~s" test)]
   [(mz:not (mz:= (mz:procedure-arity test) (mz:length names)))
    (mz:error 'guard-contract "expected ~s to take ~s arguments; takes ~s"
              test (length names) (mz:procedure-arity test))]
   [else
    (make-proj-contract
     `(xargs :guard ,sexp)
     (lambda (pos neg src name)
       (lambda (f)
         (mz:cond
          [(mz:not (simple-procedure? f))
           (raise-contract-error f src pos name
                                 "not a fixed-arity procedure: ~s"
                                 f)]
          [(mz:not (arity-matches? f test))
           (raise-contract-error f src pos name
                                 "expected ~s to take ~s arguments; takes ~s"
                                 f
                                 (mz:procedure-arity test)
                                 (mz:procedure-arity f))]
          [else
           (lambda formals
             (if (mz:apply test formals)
               (mz:apply f formals)
               (raise-contract-error
                formals src neg name
                (format-variable-alist
                 (mz:map mz:cons names formals)))))])))
     (lambda (f) (mz:and (simple-procedure? f) (arity-matches? f test))))]))

(define-syntax (define/guard stx)
  (syntax-case* stx (guard else) text=?
    [(_ (name . formals) (guard test) body)
     (syntax/loc stx
       (define/c (name . formals)
         (guard-contract (lambda formals test) 'test 'formals)
         body))]
    [(_ (name . formals) body)
     (syntax/loc stx
       (define/c (name . formals) any/c body))]))

;; ;; PREDICATES ;;;;
;; Booleans
(define/guard (booleanp x) (or (eq x t) (eq x nil)))
(define/guard (implies p q) (or (not p) q))
(define/guard (iff p q)
  (if p (if q t nil) (if q nil t)))

;; ;; ORDINALS ;;;;
(define/guard (o-finp x) (atom x))
(define/guard (o-infp x) (not (o-finp x)))
(define/guard (o-first-coeff x)
  (guard (or (o-finp x) (consp (car x))))
  (if (o-finp x) x (cdar x)))
(define/guard (o-first-expt x)
  (guard (or (o-finp x) (consp (car x))))
  (if (o-finp x) 0 (caar x)))
(define/guard (o-rst x)
  (guard (consp x))
  (cdr x))
(define/guard (o-p x)
  (if (o-finp x)
    (natp x)
    (and (consp (car x))
         (o-p (o-first-expt x)) (not (eql 0 (o-first-expt x)))
         (posp (o-first-coeff x))
         (o-p (o-rst x))
         (o< (o-first-expt (o-rst x))
             (o-first-expt x)))))
(define (o<g x) (if (atom x)
                  (rationalp x)
                  (and (consp (car x))
                       (rationalp (o-first-coeff x))
                       (o<g (o-first-expt x))
                       (o<g (o-rst x)))))
(define/guard (o< x y)
  (guard (and (o<g x) (o<g y)))
  (cond [(o-finp x) (or (o-infp y) (< x y))]
        [(o-finp y) nil]
        [(not (equal (o-first-expt x)
                     (o-first-expt y)))
         (o< (o-first-expt x) (o-first-expt y))]
        [(not (= (o-first-coeff x) (o-first-coeff y)))
         (< (o-first-coeff x) (o-first-coeff y))]
        [t (o< (o-rst x) (o-rst y))]))
(define/guard (o<= x y) (not (o< y x)))
(define/guard (o> x y) (o< y x))
(define/guard (o>= x y) (not (o< x y)))

(define/guard (make-ord fe fco rst)
  (guard (and (posp fco) (o-p fe) (o-p rst)))
  (cons (cons fe fco) rst))

;; Numbers
;; predicates

(define/guard (complex/complex-rationalp x) (complex-rationalp x))

(define/guard (int= x y) ;; TEST ME
  (guard (and (integerp x) (integerp y)))
  (= x y))
(define/guard (/= x y)
  (guard (and (acl2-numberp x) (acl2-numberp y)))
  (not (= x y)))

(define/guard (zerop x)
  (guard (acl2-numberp x))
  (eql x 0))
(define/guard (zip x)
  (guard (integerp x))
  (if (integerp x)
    (= x 0)
    t))

(define/guard (zpf x)
  (guard (and (integerp x)
              (<= 0 x)
              (<= x (1- (expt 2 28)))))
  (zp x))

(define/guard (integer-listp x)
  (if (consp x)
    (and (integerp (car x))
         (integer-listp (cdr x)))
    (null x)))

(define/guard (rational-listp x)
  (if (consp x)
    (and (rationalp (car x))
         (rational-listp (cdr x)))
    (null x)))

;; Numbers, cont.
;;functions
(define/guard (abs r)
  (guard (real/rationalp r))
  (if (minusp r) (- r) r))

(define/guard (signum x)
  (guard (real/rationalp x))
  (cond [(zerop x) 0]
        [(minusp x) -1]
        [t 1]))

(define/guard (nfix x) (if (natp x) x 0))
(define/guard (ifix x) (if (integerp x) x 0))
(define/guard (rfix x) (if (rationalp x) x 0))
(define/guard (realfix x) (if (real/rationalp x) x 0))

(define/guard (ash i bits)
  (guard (and (integerp i) (integerp bits)))
  (mz:arithmetic-shift i bits))
(define/guard (integer-length i)
  (guard (integerp i))
  (cond [(zip i) 0]
        [(= i -1) 0]
        [t (1+ (integer-length (floor i 2)))]))

(define/guard (numerator x)
  (guard (rationalp x))
  (mz:numerator x))
(define/guard (denominator x)
  (guard (rationalp x))
  (mz:denominator x))

(define/guard (ceiling num den)
  (guard (and (real/rationalp num) (real/rationalp den)
              (not (equal den 0))))
  (mz:ceiling (/ num den)))

(define/guard (round i j)
  (guard (and (real/rationalp i)
              (real/rationalp j)
              (not (eql j 0))))
  ;; took from :pe round
  (let ((Q (* i (/ j))))
    (cond [(integerp Q) Q]
          [(>= Q 0)
           (let* ([FL (floor Q 1)]
                  [REMAINDER (- Q FL)])
             (cond [(> REMAINDER 1/2) (+ FL 1)]
                   [(< REMAINDER 1/2) FL]
                   [t (cond [(integerp (* FL (/ 2))) FL]
                            [t (+ FL 1)])]))]
          [t (let* ([CL (ceiling Q 1)]
                    [REMAINDER (- Q CL)])
               (cond [(< (- 1/2) REMAINDER) CL]
                     [(> (- 1/2) REMAINDER) (+ CL -1)]
                     (t (cond [(integerp (* CL (/ 2))) CL]
                              [t (+ CL -1)]))))])))
(define/guard (truncate i j)
  (guard (and (real/rationalp i) (real/rationalp j)
              (not (eql j 0))))
  (let* ((Q (* i (/ j)))
         (N (numerator Q))
         (D (denominator Q)))
    (cond ((= D 1) N)
          ((>= N 0)
           (nonnegative-integer-quotient N D))
          (t (- (nonnegative-integer-quotient (- N)
                                              D))))))

(define/guard (rem x y)
  (guard (and (real/rationalp x) (real/rationalp y)
              (not (eql y 0))))
  (- x (* (truncate x y) y)))
(define/guard (1+ x) (+ 1 x))
(define/guard (1- x) (- x 1))

(define/guard (unary-- x)
  (guard (acl2-numberp x))
  (mz:- x))

(define/guard (binary-+ x y) 
  (guard (and (acl2-numberp x) (acl2-numberp y)))
  (mz:+ x y))
(define/guard (binary-* x y) 
  (guard (and (acl2-numberp x) (acl2-numberp y)))
  (mz:* x y))
(define/guard (complex x y)
  (guard (and (rationalp x) (rationalp y)))
  (+ x (* y 0+1i)))

(define/guard (imagpart x)
  (guard (acl2-numberp x))
  (mz:imag-part x))
(define/guard (realpart x)
  (guard (acl2-numberp x))
  (mz:real-part x))

(define/guard (conjugate c)
  (guard (acl2-numberp c))
  (if (mz:complex? c)
    (let ([real (mz:real-part c)]
          [imag (mz:imag-part c)])
      (complex real (- imag)))
    c))

;; LOGICAL / BITWISE operations

(define/guard (logandc1 x y)
  (guard (and (integerp x) (integerp y)))
  (logand (lognot x) y))
(define/guard (logandc2 x y)
  (guard (and (integerp x) (integerp y)))
  (logand x (lognot y)))

(define/guard (logbitp bit i) ;; verify this.
  (guard (and (integerp i) (natp bit)))
  (= (logand (ash i (- bit)) 1) 1))

(define/guard (logcount x)
  (guard (integerp x))
  (cond [(zip x) 0]
        [(< x 0) (logcount (lognot x))]
        [(evenp x) (logcount (nonnegative-integer-quotient x 2))]
        [t (1+ (logcount (nonnegative-integer-quotient x 2)))]))



(define/guard (lognand x y)
  (guard (and (integerp x) (integerp y)))
  (lognot (logand x y)))

(define/guard (lognor i j)
  (guard (and (integerp i) (integerp j)))
  (lognot (logior i j)))

(define/guard (logtest x y)
  (guard (and (integerp x) (integerp y)))
  (not (zerop (logand x y))))




;; Symbols

(define/guard (keywordp x)
  (and (symbolp x)
       (eql (mz:string-ref (mz:symbol->string x) 0) #\:)))
(define/guard (symbol-name s)
  (guard (symbolp s))
  (mz:if (mz:eqv? s '()) 
         (symbol-name 'nil)
         (if (keywordp s)
           (substring (string-upcase (mz:symbol->string s) 1))
           (string-upcase (mz:symbol->string s)))))
(define/guard (symbol-package-name s) ;; FIX
  (guard (symbolp s))
  ;;(printf " >> WARNING: calling (symbol-package-name ~a)~n" s)
  (if (keywordp s)
    "KEYWORD"
    "COMMON-LISP"))

(define/guard (symbol-< x y) ;; FIX ME -- packages?
  (guard (and (symbolp x) (symbolp y)))
  ;;(printf " >> WARNING << -- fix symbol-< in acl2-prims.ss~n")
  (and (string< (symbol-name x) (symbol-name y)) t))

(define/guard (symbol-listp x)
  (if (consp x)
    (and (symbolp (car x))
         (symbol-listp (cdr x)))
    (null x)))
(define/guard (symbol-alistp x)
  (if (consp x)
    (and (consp (car x))
         (symbolp (car (car x)))
         (symbol-alistp (cdr x)))
    (null x)))

(define/guard (keyword-value-listp x)
  (if (consp x) 
    (and (consp (cdr x)) (keywordp (car x))
         (keyword-value-listp (cddr x)))
    (null x)))


;; Characters

(define/guard (character-listp x)
  (if (consp x)
    (and (characterp (car x))
         (character-listp (cdr x)))
    (null x)))
(define/guard (digit-to-char d)
  (guard (and (<= 0 d) (<= d 15)))
  (char-upcase (mz:string-ref (mz:number->string d 16) 0)))

(define/guard (upper-case-p c)
  (guard (standard-char-p c))
  (and (alpha-char-p c) (eql (char-upcase c) c)))
(define/guard (lower-case-p c)
  (guard (standard-char-p c))
  (and (alpha-char-p c) (eql (char-downcase c) c)))

(define/guard (char-upcase c)
  (guard (standard-char-p c))
  (mz:char-upcase c))
(define/guard (char-downcase c)
  (guard (standard-char-p c))
  (mz:char-downcase c))

;; Strings
(define/guard (string x)
  (guard (or (stringp x) (symbolp x) (characterp x)))
  (cond [(stringp x) x]
        [(symbolp x) (symbol-name x)]
        [(characterp x) (mz:string x)]))
(define/guard (char s n)
  (guard (and (stringp s)
              (integerp n)
              (>= n 0)
              (< n (length s))))
  (mz:string-ref s n))

(define/guard (string-listp x)
  (if (consp x)
    (and (stringp (car x)) (string-listp (cdr x)))
    (null x)))

(define (standard-stringp x)
  (and (stringp x)
       (standard-char-listp (coerce x 'list))))
(define/guard (standard-string-alistp x)
  (if (consp x)
    (and (consp (car x)) (standard-stringp (caar x))
         (standard-string-alistp (cdr x)))
    (null x)))

(define/guard (string-append s1 s2)
  (guard (and (stringp s1) (stringp s2)))
  (concatenate 'string s1 s2))

(define/guard (string-upcase s)
  (guard (and (stringp s) 
              (standard-char-listp (coerce s 'list))))
  (mz:string-upcase s))
(define/guard (string-downcase s)
  (guard (and (stringp s) 
              (standard-char-listp (coerce s 'list))))
  (mz:string-downcase s))

(define/guard (length x)
  (guard (or (stringp x) (true-listp x)))
  (cond [(stringp x) (mz:string-length x)]
        [t (mz:length x)]))

(define/guard (subseq seq start end)
  (guard (and (or (true-listp seq) (stringp seq))
              (integerp start)
              (<= 0 start)
              (or (null end)
                  (and (integerp end)
                       (<= end (length seq))))
              (<= start (or end (length seq)))))
  (cond [(stringp seq) 
         (if (null end)
           (mz:substring seq start)
           (mz:substring seq start end))]
        [(listp seq) (srfi:take (srfi:drop seq start)
                                (- (or end (length seq)) start))]))

(define (position/equiv equivp item seq)
  (mz:let loop ([seq (if (stringp seq) (coerce seq 'list) seq)]
                [posn 0])
          (cond [(null seq) nil]
                [(equivp item (car seq)) posn]
                [t (loop (cdr seq) (1+ posn))])))
(define/guard (position item seq)
  (guard (or (stringp seq)
             (and (true-listp seq)
                  (or (eqlablep item)
                      (eqlable-listp seq)))))
  (position/equiv (lambda (x y) (eql x y)) item seq))
(define/guard (position-eq item seq)
  (guard (and (true-listp seq)
              (or (symbolp item)
                  (symbol-listp seq))))
  (position/equiv (lambda (x y) (eq x y)) item seq))
(define/guard (position-equal item seq)
  (guard (or (stringp seq) (true-listp seq)))
  (position/equiv (lambda (x y) (equal x y)) item seq))

;; Lists

(define/guard (listp x) (or (null x) (consp x)))
(define/guard (proper-consp x)
  (and (true-listp x) (not (null x))))
(define/guard (improper-consp x)
  (if (consp x)
    (improper-consp/check-rest (cdr x))
    nil))
(define (improper-consp/check-rest x)
  (if (consp x)
    (improper-consp/check-rest (cdr x))
    (not (null x))))

(define/guard (eqlable-listp x)
  (if (consp x)
    (and (eqlablep (car x))
         (eqlable-listp (cdr x)))
    (null x)))



(define/guard (true-list-listp x)
  (if (consp x)
    (and (true-listp (car x))
         (true-list-listp (cdr x)))
    (null x)))

(define/guard (first x) (car x))

(define/guard (rest x) (cdr x))

(define/guard (fifth x) (car (iterate 4 (lambda (x) (cdr x)) x)))
(define/guard (sixth x) (car (iterate 5 (lambda (x) (cdr x)) x)))
(define/guard (seventh x) (car (iterate 6 (lambda (x) (cdr x)) x)))
(define/guard (eighth x) (car (iterate 7 (lambda (x) (cdr x)) x)))
(define/guard (ninth x) (car (iterate 8 (lambda (x) (cdr x)) x)))
(define/guard (tenth x) (car (iterate 9 (lambda (x) (cdr x)) x)))

(define/guard (nthcdr n l)
  (guard (and (natp n) (true-listp l)))
  (if (zp n) l (nthcdr (1- n) (cdr l))))
(define/guard (update-nth key val l)
  (guard (and (natp key) (true-listp l)))
  (if (zp key) 
    (cons val (cdr l))
    (cons (car l) (update-nth (1- key) val (cdr l)))))

(define/guard (take n l)
  (guard (and (natp n) (true-listp l)))
  (if (zp n)
    nil
    (cons (car l) (take (1- n) (cdr l)))))

(define/guard (binary-append x y)
  (guard (true-listp x))
  (mz:append x y))

(define (member/equiv equiv elt lst)
  (cond [(null lst) nil]
        [(equiv elt (car lst)) lst]
        [t (member/equiv equiv elt (cdr lst))]))
(define/guard (member elt lst)
  (guard (if (eqlablep elt) (true-listp lst) (eqlable-listp lst)))
  (member/equiv (lambda (x y) (eql x y))
                elt lst))
(define/guard (member-eq elt lst)
  (guard (if (symbolp elt) (true-listp lst) (symbol-listp lst)))
  (member/equiv (lambda (x y) (eq x y)) elt lst))
(define/guard (member-equal elt lst)
  (guard (true-listp lst))
  (member/equiv (lambda (x y) (equal x y)) elt lst))

(define (remove/equiv equiv x lst)
  (cond [(null lst) nil]
        [(equiv x (car lst)) (remove/equiv equiv x (cdr lst))]
        [t (cons (car lst) (remove/equiv equiv x (cdr lst)))]))

(define/guard (remove x lst)
  (guard (if (eqlablep x) (true-listp lst) (eqlable-listp lst)))
  (remove/equiv (lambda (a b) (eql a b)) x lst))

(define/guard (remove-eq x lst)
  (guard (if (symbolp x) (true-listp lst) (symbol-listp lst)))
  (remove/equiv (lambda (a b) (eq a b)) x lst))

(define/guard (remove-equal x lst)
  (guard (true-listp lst))
  (remove/equiv (lambda (a b) (equal a b)) x lst))

(define (remove-duplicates/equiv equiv lst)
  (cond [(endp lst) nil]
        [(member/equiv equiv (car lst) (cdr lst))
         (remove-duplicates/equiv equiv (cdr lst))]
        [t (cons (car lst) (remove-duplicates/equiv equiv (cdr lst)))]))
(define/guard (remove-duplicates lst)
  (guard (or (stringp lst) (eqlable-listp lst)))
  (if (stringp lst)
    (coerce (remove-duplicates-eql (coerce lst 'list)) 'string)
    (remove-duplicates-eql lst)))

(define/guard (remove-duplicates-eql lst)
  (guard (eqlable-listp lst))
  (remove-duplicates/equiv (lambda (x y) (eql x y)) lst))
(define/guard (remove-duplicates-equal lst)
  (guard (true-listp lst))
  (remove-duplicates/equiv (lambda (x y) (equal x y)) lst))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMOVE1 functions
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove1/equiv equiv x l)
  (cond [(endp l) nil]
        [(equiv x (car l)) (cdr l)]
        [t (cons (car l) (remove1/equiv equiv x (cdr l)))]))

(define/guard (remove1 x l)
  (guard (if (eqlablep x) (true-listp l) (eqlable-listp l)))
  (remove1/equiv (lambda (a b) (eql a b)) x l))

(define/guard (remove1-eq x l)
  (guard (if (symbolp x) (true-listp l) (symbol-listp l)))
  (remove1/equiv (lambda (a b) (eq a b)) x l))

(define/guard (remove1-equal x l)
  (guard (true-listp l))
  (remove1/equiv (lambda (a b) (equal a b)) x l))

(define/guard (no-duplicatesp lst)
  (guard (eqlable-listp lst))
  (cond [(endp lst) t]
        [(member (car lst) (cdr lst)) nil]
        [t (no-duplicatesp (cdr lst))]))
(define/guard (no-duplicatesp-equal lst)
  (guard (true-listp lst))
  (cond [(endp lst) t]
        [(member-equal (car lst) (cdr lst)) nil]
        [t (no-duplicatesp-equal (cdr lst))]))

(define/guard (pairlis$ xs ys)
  (guard (and (true-listp xs) (true-listp ys)))
  (if (null xs)
    nil
    (cons (cons (car xs) (car ys)) (pairlis$ (cdr xs) (cdr ys)))))

(define/guard (fix-true-list x)
  (if (consp x)
    (cons (car x) (fix-true-list (cdr x)))
    nil))

(define/guard (butlast l n)
  (guard (and (true-listp l) (natp n)))
  (if (<= (len l) n)
    nil
    (srfi:drop-right l n)))

(define/guard (last x)
  (guard (listp x))
  (if (null x) 
    x 
    (srfi:last-pair x)))




;; Lists / Alists
(define/guard (acons key value alst)
  (guard (alistp alst))
  (cons (cons key value) alst))

(define (assoc/equiv equivp key alist)
  (cond [(null alist) nil]
        [(equivp key (caar alist)) (car alist)]
        [t (assoc/equiv equivp key (cdr alist))]))
(define/guard (assoc key alist)
  (guard (and (alistp alist)
              (or (eqlablep key)
                  (eqlable-alistp alist))))
  (assoc/equiv (lambda (x y) (eql x y)) key alist))
(define/guard (assoc-eq key alist)
  (guard (and (alistp alist)
              (or (symbolp key)
                  (symbol-alistp alist))))
  (assoc/equiv (lambda (x y) (eq x y)) key alist))
(define/guard (assoc-equal key alist)
  (guard (alistp alist))
  (assoc/equiv (lambda (x y) (equal x y)) key alist))

(define/guard (assoc-keyword kw kvlist)
  (guard (keyword-value-listp kvlist))
  (cond [(null kvlist) nil]
        [(eq kw (car kvlist)) kvlist]
        [t (assoc-keyword kw (cddr kvlist))]))

(define/guard (assoc-string-equal key alist)
  (guard (and (stringp key) (alistp alist)))
  (assoc/equiv (lambda (s t) (string-equal s t))
               key alist))

(define (put-assoc/equiv equivp key value alist)
  (cond [(null alist) (list (cons key value))]
        [(equivp key (caar alist)) (cons (cons key value) (cdr alist))]
        [t (cons (car alist) (put-assoc/equiv equivp key value (cdr alist)))]))

(define/guard (put-assoc-eq key value alist)
  (guard (if (symbolp key) (alistp alist) (symbol-alistp alist)))
  (put-assoc/equiv (lambda (x y) (eq x y)) key value alist))
(define/guard (put-assoc-eql key value alist)
  (guard (if (eqlablep key) (alistp alist) (eqlable-alistp alist)))
  (put-assoc/equiv (lambda (x y) (eql x y)) key value alist))
(define/guard (put-assoc-equal key value alist)
  (guard (alistp alist))
  (put-assoc/equiv (lambda (x y) (equal x y)) key value alist))

(define (rassoc/equiv equiv key alist)
  (cond [(null alist) nil]
        [(equiv key (cdar alist)) (car alist)]
        [t (rassoc/equiv equiv key (cdr alist))]))
(define (r-eqlable-alistp x)
  (if (consp x)
    (and (consp (car x)) (eqlablep (cdar x))
         (r-eqlable-alistp (cdr x)))
    (null x)))
(define (r-symbol-alistp x)
  (if (consp x)
    (and (consp (car x)) (symbolp (cdar x))
         (r-symbol-alistp (cdr x)))
    (null x)))

(define/guard (rassoc key alist)
  (guard (if (eqlablep key) (alistp alist) (r-eqlable-alistp alist)))
  (rassoc/equiv (lambda (x y) (eql x y)) key alist))
(define/guard (rassoc-eq key alist)
  (guard (if (symbolp key) (alistp alist) (r-symbol-alistp alist)))
  (rassoc/equiv (lambda (x y) (eq x y)) key alist))
(define/guard (rassoc-equal key alist)
  (guard (alistp alist))
  (rassoc/equiv (lambda (x y) (equal x y)) key alist))

(define/guard (strip-cars lst)
  (guard (alistp lst))
  (mz:map (lambda (x) (car x)) lst))
(define/guard (strip-cdrs lst)
  (guard (alistp lst))
  (mz:map (lambda (x) (cdr x)) lst))

;; hack for a modicum compatibility with "data-structures/alist-theory"
(define/guard (domain lst)
  (guard (alistp lst))
  (strip-cars lst))
(define/guard (range lst)
  (guard (alistp lst))
  (strip-cdrs lst))

(define/guard (eqlable-alistp x)
  (if (consp x)
    (and (consp (car x))
         (eqlablep (car (car x)))
         (eqlable-alistp (cdr x)))
    (null x)))

;; Lists / Sets
(define/guard (add-to-set-eq elt set)
  (guard (if (symbolp elt)
           (true-listp set)
           (symbol-listp set)))
  (cond [(member-eq elt set) set]
        [t (cons elt set)]))
(define/guard (add-to-set-eql elt set)
  (guard (if (eqlablep elt) (true-listp set) (eqlable-listp set)))
  (cond [(member elt set) set]
        [t (cons elt set)]))
(define/guard (add-to-set-equal elt set)
  (guard (true-listp set))
  (cond [(member-equal elt set) set]
        [t (cons elt set)]))

(define/guard (set-difference-eq x y)
  (guard (and (true-listp x) (true-listp y)
              (or (symbol-listp x) (symbol-listp y))))
  (cond [(null x) nil]
        [(member-eq (car x) y) (set-difference-eq (cdr x) y)]
        [t (cons (car x) (set-difference-eq (cdr x) y))]))

(define/guard (set-difference-equal x y)
  (guard (and (true-listp x) (true-listp y)))
  (cond [(null x) nil]
        [(member-equal (car x) y) (set-difference-equal (cdr x) y)]
        [t (cons (car x) (set-difference-equal (cdr x) y))]))

(define/guard (subsetp x y)
  (guard (cond [(eqlable-listp y) (true-listp x)]
               [(eqlable-listp x) (true-listp y)]))
  (or (endp x)
      (and (member (car x) y) (subsetp (cdr x) y))))
(define/guard (subsetp-equal x y)
  (guard (and (true-listp x) (true-listp y)))
  (or (endp x) (and (member-equal (car x) y) (subsetp-equal (cdr x) y))))

(define/guard (union-eq x y)
  (guard (and (symbol-listp x) (true-listp y)))
  (cond [(endp x) y]
        [(member-eq (car x) y) (union-eq (cdr x) y)]
        [t (cons (car x) (union-eq (cdr x) y))]))

(define/guard (union-equal x y)
  (guard (and (true-listp x) (true-listp y)))
  (cond [(endp x) y]
        [(member-equal (car x) y) (union-equal (cdr x) y)]
        [t (cons (car x) (union-equal (cdr x) y))]))

;; Atoms and miscellany
(define/guard (atom x) (not (consp x)))
(define/guard (atom-listp x)
  (if (consp x)
    (and (atom (car x))
         (atom-listp (cdr x)))
    (null x)))

(define/guard (identity x) x)

;; ;; SUBSTITUTION
(define/guard (subst new old tree)
  (guard (eqlablep old))
  (cond [(eql old tree) new]
        [(atom tree) tree]
        [t (cons (subst new old (car tree))
                 (subst new old (cdr tree)))]))

(define/guard (substitute new old seq)
  (guard (or (and (stringp seq) (characterp new))
             (and (true-listp seq)
                  (or (eqlablep old)
                      (eqlable-listp seq)))))
  (if (stringp seq)
    (coerce (substitute-ac new old (coerce seq 'list) nil) 'string)
    (substitute-ac new old seq nil)))
(define (substitute-ac new old seq acc)
  (cond [(endp seq) (reverse acc)]
        [(eql old (car seq))
         (substitute-ac new old (cdr seq) (cons new acc))]
        [t (substitute-ac new old (cdr seq)
                          (cons (car seq) acc))]))

(define/guard (sublis alist tree)
  (guard (eqlable-alistp alist))
  (cond [(atom tree) (let ([pair (assoc tree alist)])
                       (if pair (cdr pair) tree))]
        [t (cons (sublis alist (car tree))
                 (sublis alist (cdr tree)))]))



;; ORDERINGS
(define (both ? x y) (and (? x) (? y))) ;; ? is an ACL2 predicate
(define (complex/lex<= c1 c2)
  (let ([x1 (fix c1)]
        [y1 (fix c2)])
    (or (< (realpart x1) (realpart y1))
        (and (= (realpart x1) (realpart y1))
             (< (imagpart x1) (imagpart y1))))))

(define/guard (lexorder x y)
  (cond [(atom x) (if (atom y) (alphorder x y) t)]
        [(atom y) nil]
        [(equal (car x) (car y)) (lexorder (cdr x) (cdr y))]
        [t (lexorder (car x) (car y))]))

(define/guard (acl2-count x)
  (cond [(consp x) (+ 1 (acl2-count (car x)) (acl2-count (cdr x)))]
        [(rationalp x) (if (integerp x) 
                         (ifix (abs x))
                         (+ (ifix (abs (numerator x)))
                            (denominator x)))]
        [(complex/complex-rationalp x) (+ 1 
                                          (acl2-count (realpart x))
                                          (acl2-count (imagpart x)))]
        [(stringp x) (length x)]
        [t 0]))
