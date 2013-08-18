#lang racket

(require "private/utils.ss")
(require (for-syntax (cce syntax))
         (cce function)
         (prefix-in raw- (combine-in (random random) srfi/27)))

(provide/contract
 [prob/c (case->
          (-> flat-contract?)
          (-> (one-of/c 0 1) flat-contract?)
          (-> (one-of/c 0) (one-of/c 1) flat-contract?))])

;; prob/c : [0 1] -> FlatContract
;; Accepts real numbers in (0,1), inclusive of 0 and/or 1 if supplied.
(define prob/c
  (match-lambda*
   [(list) (and/c (real-in 0 1) (>/c 0) (</c 1))]
   [(list 0) (and/c (real-in 0 1) (</c 1))]
   [(list 1) (and/c (real-in 0 1) (>/c 0))]
   [(list 0 1) (real-in 0 1)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SETUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (source) (current-pseudo-random-generator))

(define (schematics-random-integer k)
  ((raw-random-source-make-integers (source)) k))

(define (schematics-random-real)
  ((raw-random-source-make-reals (source))))

(define (schematics-random-binomial n p)
  ((raw-random-source-make-binomials (source)) n p))

(define (schematics-random-geometric p)
  ((raw-random-source-make-geometrics (source)) p))

(define (schematics-random-poisson r)
  ((raw-random-source-make-poissons (source)) r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BOOLEAN DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-boolean (->* [] [(prob/c 0 1)] boolean?)]
 [random-boolean/fair (-> boolean?)]
 [random-boolean/bernoulli (-> (prob/c 0 1) boolean?)])

;; random-boolean/bernoulli : Prob -> (Random Boolean)
(define (random-boolean/bernoulli p)
  (if (exact? p)
      (let* ([n (numerator p)]
             [d (denominator p)])
        (< (schematics-random-integer d) n))
      (< (schematics-random-real) p)))

;; random-boolean/fair : (Random Boolean)
(define (random-boolean/fair) (random-boolean/bernoulli 1/2))

;; random-boolean : [Prob] -> (Random Boolean)
(define (random-boolean [p 1/2])
  (random-boolean/bernoulli p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BOUNDED INTEGER DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-natural/binomial
  (->d ([n natural-number/c] [p (prob/c 0 1)]) () [_ (integer-in 0 n)])]
 [random-integer/uniform
  (->d ([lo exact-integer?] [hi (and/c exact-integer? (>=/c lo))]) ()
       [_ (integer-in lo hi)])])

;; random-natural/binomial : Nat Prob -> (Random Nat)
(define (random-natural/binomial n p)
  (inexact->exact (schematics-random-binomial n p)))

;; random-integer/uniform : Int Int -> (Random Int)
;; The second argument must not be less than the first.
(define (random-integer/uniform lo hi)
  (+ lo (schematics-random-integer (+ hi 1 (- lo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  UNBOUNDED INTEGER DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make these depend on their optional arguments once I learn how.
(provide/contract
 [random-natural/geometric
  (-> (prob/c) (one-of/c 0 1) natural-number/c)]
 [random-natural/pascal
  (-> exact-positive-integer? (prob/c) natural-number/c)]
 [random-natural/poisson
  (-> (and/c rational? positive?) natural-number/c)]
 [random-integer/skellam
  (->* [(and/c rational? positive?)] [(and/c rational? positive?)]
       exact-integer?)])

;; random-natural/geometric : Prob (Or 0 1) -> (Random Nat)
(define (random-natural/geometric p base)
  (+ base (inexact->exact (schematics-random-geometric p)) -1))

;; random-natural/pascal : Pos Prob -> (Random Nat)
(define (random-natural/pascal n p)
  (for/fold ([sum 0]) ([i (in-range 1 n)])
    (+ sum (random-natural/geometric p 0))))

;; random-natural/poisson : PosRat -> (Random Nat)
(define (random-natural/poisson rate)
  (inexact->exact (schematics-random-poisson rate)))

;; random-integer/skellam : PosRat [PosRat] -> (Random Int)
(define (random-integer/skellam pos-rate [neg-rate pos-rate])
  (- (random-natural/poisson pos-rate)
     (random-natural/poisson neg-rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BOUNDED REAL DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-real/uniform
  (->d ([lo real?] [hi (and/c real? (>=/c lo))]) () [_ (real-in lo hi)])])

;; random-real/uniform : Real Real>=lo -> Real in [lo,hi]
(define (random-real/uniform lo hi)
  (+ lo (* (- hi lo) (schematics-random-real))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM CHOICE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-choice (->* [any/c] [] #:rest (listof any/c) any/c)]
 [random-choice-weighted (-> (listof (cons/c (>/c 0) any/c)) any/c)])

(define (random-choice . args)
  (list-ref args (random-integer/uniform 0 (- (length args) 1))))

(define (random-choice-weighted alist)
  (let* ([weights (map inexact->exact (map car alist))]
         [values (map cdr alist)]
         [total (apply + weights)]
         [choice (random-real/uniform 0 1)])
    (let loop ([ws weights]
               [vs values]
               [cumulative 0])
      (if (null? ws)
          (error 'random-choice-weighted "no choices given")
          (let* ([accum (+ cumulative (/ (car ws) total))])
            (if (<= choice accum)
                (car vs)
                (loop (cdr ws) (cdr vs) accum)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM DISPATCH
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide random-case)

(define-for-syntax (expand-random-case/weighted-args stx)
  (syntax-case stx ()
    [() stx]
    [(expr #:weight wt . rest)
     (quasisyntax/loc stx
       ([expr wt] #,@(expand-random-case/weighted-args #'rest)))]
    [(expr . rest)
     (quasisyntax/loc stx
       ([expr 1] #,@(expand-random-case/weighted-args #'rest)))]
    [_
     (syntax-error
      stx
      "expected a sequence of expressions with optional #:weight keywords")]))

(define-syntax (random-case stx)
  (parameterize ([current-syntax stx])
    (syntax-case stx ()
      [(_ arg ...)
       (with-syntax ([([expr wt] ...)
                      (expand-random-case/weighted-args #'(arg ...))])
         (syntax/loc stx
           (call
            (random-choice-weighted
             (list (cons wt (lambda () expr)) ...)))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  DEFAULT NUMBER DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-natural (-> natural-number/c)]
 [random-integer (-> exact-integer?)]
 [random-rational (-> (and/c rational? exact?))]
 [random-exact (-> (and/c number? exact?))]
 [random-positive-real (-> (and/c inexact-real? (>/c 0)))]
 [random-real (-> inexact-real?)]
 [random-inexact (-> (and/c number? inexact?))]
 [random-number (-> number?)])

;; Primitive constructors

(define (random-nonnegative-integer)
  (random-natural/geometric 1/1000 0))

(define (random-positive-integer)
  (random-natural/geometric 1/1000 1))

(define (random-signed-integer)
  (random-case
   (random-nonnegative-integer)
   (- (random-nonnegative-integer))))

(define (random-nonnegative-ratio)
  (/ (random-nonnegative-integer) (random-positive-integer)))

(define (random-signed-ratio)
  (random-case
   (random-nonnegative-ratio)
   (- (random-nonnegative-ratio))))

(define (random-exact-complex)
  (make-rectangular (random-signed-ratio)
                    (random-signed-ratio)))

(define (random-unitary-real)
  (random-real/uniform 0 1))

(define (random-positive-real)
  (/ (random-unitary-real) (random-unitary-real)))

(define (random-signed-real)
  (random-case
   (random-positive-real)
   (- (random-positive-real))))

(define (random-inexact-complex)
  (make-rectangular (random-signed-real) (random-signed-real)))

;; Exported constructors

(define (random-natural)
  (random-nonnegative-integer))

(define (random-integer)
  (random-signed-integer))

(define (random-rational)
  (random-case
   (random-signed-integer)
   (random-signed-ratio)))

(define (random-exact)
  (random-case
   (random-signed-integer)
   (random-signed-ratio)
   (random-exact-complex)))

(define (random-real)
  (random-signed-real))

(define (random-inexact)
  (random-case
   (random-signed-real)
   (random-inexact-complex)))

(define (random-number)
  (random-case
   (random-signed-integer)
   (random-signed-ratio)
   (random-exact-complex)
   (random-signed-real)
   (random-inexact-complex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  LIST DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-list (->* [(-> any/c)] [#:len natural-number/c] list?)])

(define (random-list make-elem
                     #:len [len (random-natural/poisson 4)])
  (build-list len (thunk* (make-elem))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM TEXT DISTRIBUTIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-char (-> char?)]
 [random-string (->* [] [#:char (-> char?) #:len natural-number/c] string?)]
 [random-symbol (->* [] [#:string string?] symbol?)]
 [random-keyword (->* [] [#:string string?] keyword?)])

(define (random-char)
  (string-ref "abcdefghijklmnopqrstuvwxyz"
              (random-integer/uniform 0 25)))

(define (random-string #:char [make-char random-char]
                       #:len [len (random-natural/poisson 4)])
  (apply string (random-list make-char #:len len)))

(define (random-symbol #:string [string (random-string)])
  (string->symbol string))

(define (random-keyword #:string [string (random-string)])
  (string->keyword string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  RANDOM S-EXPRESSIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide/contract
 [random-atom (-> (not/c cons?))]
 [random-sexp (->* []
                   [#:atom (-> (not/c cons?))
                    #:improper boolean?
                    #:size exact-nonnegative-integer?]
                   any/c)])

(define (random-atom)
  (random-case
   empty
   (random-boolean)
   (random-symbol)
   (random-char)
   (random-number)
   (random-string)))

(define (random-sexp #:atom [make-atom random-atom]
                     #:improper [improper? #f]
                     #:size [size (random-natural/poisson 4)])
  (if improper?
      (random-improper-sexp? make-atom size)
      (random-proper-sexp? make-atom size)))

(define (random-improper-sexp? make-atom size)
  (if (= size 0)
      (make-atom)
      (let* ([left-size (random-integer/uniform 0 (- size 1))]
             [right-size (- size left-size 1)])
        (cons (random-improper-sexp? make-atom left-size)
              (random-improper-sexp? make-atom right-size)))))

(define (random-proper-sexp? make-atom size)
  (if (= size 0)
      (make-atom)
      (let* ([len (random-integer/uniform 1 size)]
             [sub (- size len)]
             [subsizes (random-split-count sub len)])
        (map (lambda (subsize)
               (random-proper-sexp? make-atom subsize))
             subsizes))))

(define (random-split-count count len)
  (let* ([vec (make-vector len 0)])
    (for ([i (in-range count)])
      (let* ([j (random-integer/uniform 0 (- len 1))])
        (vector-set! vec j (+ (vector-ref vec j) 1))))
    (vector->list vec)))
