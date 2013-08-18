#lang racket

(require "../random.rkt"
         "../rackunit.rkt"
         rackunit
         rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SECTION 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; absolute-value : Real -> Non-Negative-Real
;; Computes the absolute value of a number.
(define (absolute-value x)
  (if (< x 0) (- x) x))

;; area-of-circle : Positive -> Positive
;; Computes the area of a circle of a given radius.
(define (area-of-circle r)
  (* pi r r))

;; A Shape is one of:
;; - (make-circle Positive Symbol)
;; - (make-rectangle Positive Positive Symbol)
(define-struct circle (radius color))
(define-struct rectangle (width height color))

;; area : Shape -> Positive
;; Computes the area of a shape.
(define (area sh)
  (cond
   [(circle? sh) (area-of-circle (circle-radius sh))]
   [(rectangle? sh) (* (rectangle-width sh) (rectangle-height sh))]))

;; sum : (Listof Number) -> Number
;; Computes the total of the elements of a list.
(define (sum ns)
  (cond
   [(empty? ns) 0]
   [(cons? ns) (+ (car ns) (sum (cdr ns)))]))

;; A Binary Tree (BT) is one of:
;; - 'leaf
;; - (make-node Number BT BT)
(define-struct node (value left right))

;; bt-contains? : BT Number -> Boolean
;; Reports whether a BT contains a given number.
(define (bt-contains? bt n)
  (cond
   [(symbol? bt) #f]
   [(node? bt) (or (= (node-value bt) n)
                   (bt-contains? (node-left bt) n)
                   (bt-contains? (node-right bt) n))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  SECTION 2
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pay : Nat -> Nat
;; Calculates a pay rate of $50 + $8/hour,
;; given the number of hours worked in a month.
(define (pay hrs)
  (+ 50 (* 8 hrs)))

;; build-list-squares : Nat -> (Listof Nat)
;; Compute the squares of all the numbers from 1 to n.
(define (build-list-squares n)
  (if (<= n 0)
      empty
      (cons (sqr n) (build-list-squares (- n 1)))))

;; double : (Listof Number) -> (Listof Number)
;; Doubles each number in the list.
(define (double ns)
  (if (empty? ns)
      empty
      (cons (* 2 (first ns))
            (double (rest ns)))))

;; insert : Real (Sorted-Listof Real) -> (Sorted-Listof Real)
;; Inserts a number in its correct position in a sorted list.
(define (insert n ns)
  (cond
   [(empty? ns) (list n)]
   [(cons? ns) (if (<= n (first ns))
                   (cons n ns)
                   (cons (first ns) (insert n (rest ns))))]))

;; insert-sort : (Listof Real) -> (Listof Real)
;; Sorts a numeric list.
(define (insert-sort ns)
  (cond
   [(empty? ns) empty]
   [(cons? ns) (insert (first ns) (insert-sort (rest ns)))]))

;; A File is a String
;; A Dir is one of:
;; - empty
;; - (cons File Dir)
;; - (cons Dir Dir)

(define (count-files d)
  (cond
   [(empty? d) 0]
   [(string? (first d)) (+ 1 (count-files (rest d)))]
   [else (+ (count-files (first d)) (count-files (rest d)))]))

(define (dir->files d)
  (cond
   [(empty? d) empty]
   [(string? (first d)) (cons (first d) (dir->files (rest d)))]
   [else (append (dir->files (first d)) (dir->files (rest d)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TESTING DEFINITIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (random-shape)
  (random-case
   (make-circle (random-positive-real) 'red)
   (make-rectangle (random-positive-real) (random-positive-real) 'blue)))

(define (random-bt-of random-value)
  (random-case
   'leaf
   (make-node (random-value)
              (random-bt-of random-value)
              (random-bt-of random-value))
   #:weight 1/3))

(define (sorted? ns)
  (cond
   [(empty? ns) #t]
   [(and (cons? ns) (empty? (cdr ns))) #t]
   [(and (cons? ns) (cons? (cdr ns)))
    (and (<= (first ns) (first (rest ns)))
         (sorted? (rest ns)))]))

(define (random-directory max-depth)
  (random-case
   empty
   (cons (random-string) (random-directory (sub1 max-depth)))
   #:weight (- 1 (/ 1 max-depth))
   (cons (random-directory (sub1 max-depth))
         (random-directory (sub1 max-depth)))
   #:weight (- 1 (/ 1 max-depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  TESTS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests
 (test-suite "Tutorial"
   (test-suite "Success"
     (test-suite "Section 1"
       (test-suite "absolute-value"
         (test-random #:name "absolute value is real"
             {[x (random-real)]}
           (check-pred real? (absolute-value x)))
         (test-random #:name "absolute value is non-negative"
             {[x (random-real)]}
           (check <= 0 (absolute-value x))))
       (test-suite "area-of-circle"
         (test-random #:name "area of circle is real"
             {[r (random-positive-real)]}
           (check-pred real? (area-of-circle r)))
         (test-random #:name "area of circle is non-negative"
             {[r (random-positive-real)]}
           (check <= 0 (area-of-circle r))))
       (test-suite "area"
         (test-random #:name "area of shape is real"
             {[sh (random-shape)]}
           (check-pred real? (area sh)))
         (test-random #:name "area of shape is non-negative"
             {[sh (random-shape)]}
           (check <= 0 (area sh))))
       (test-suite "sum"
         (test-random #:name "sum is number"
             {[ns (random-list random-number)]}
           (check-pred number? (sum ns))))
       (test-suite "bt-contains?"
         (test-random #:name "bt-contains? yields boolean"
             {[n (random-integer/uniform 1 10)]
              [bt (random-bt-of
                    (lambda () (random-integer/uniform 1 10)))]}
           (check-pred boolean? (bt-contains? bt n)))))
     (test-suite "Section 2"
       (test-suite "pay"
         (test-random #:name "pay is integer"
             {[hrs (random-natural)]}
           (check-pred integer? (pay hrs))))
       (test-suite "build-list-squares"
         (test-random #:name "squares is list of integers"
             {[n (random-natural)]
              [sqrs (build-list-squares n)]}
           (for ([i sqrs]) (check-pred integer? i))))
       (test-suite "double"
         (test-random #:name "doubled integer is even"
             {[ns (random-list random-integer)]
              [ds (double ns)]}
           (for ([i ds]) (check-pred even? i))))
       (test-suite "insert"
         (test-random #:name "sort produces sorted list"
             {[n (random-integer)]
              [ns (sort (random-list random-integer) <)]}
           (check-pred sorted? ns)))
       (test-suite "insert-sort"
         (test-random #:name "insert-sort produces sorted list"
             {[ns (random-list random-integer)]}
           (check-pred sorted? (insert-sort ns))))
       (test-suite "files"
         (test-random #:name "count-files produces the right number"
             {[d (random-directory 4)]}
           (check = (count-files d) (length (dir->files d)))))))
   (test-suite "Failure"
     (test-suite "Section 1"
       (test-random #:name "absolute value is negative (!)"
           {[x (random-real)]}
         (check-pred negative? (absolute-value x)))
       (test-random #:name "area of circle is negative (!)"
           {[r (random-positive-real)]}
         (check < (area-of-circle r) 0))
       (test-random #:name "area of number (!) is number"
           {[sh (random-number)]}
         (check-pred number? (area sh)))
       (test-random #:name "binary tree contains arbitrary number (!)"
           {[n (random-real)]
            [bt (random-bt-of random-real)]}
         (check bt-contains? bt n)))
     (test-suite "Section 2"
       (test-random #:name "arbitrary integers are inexact (!)"
           {[ns (random-list random-integer)
             #:where (andmap inexact? ns)]}
         #t)
       (test-random #:name "arbitrary lists are sorted (!)"
           {[ns (random-list random-natural #:len 20)
             #:where (sorted? ns)]}
         #t)
       (test-random #:name "fail (!)"
           {[d (random-directory 4)]}
         (fail))))))
