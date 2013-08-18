#lang racket

(require "private/utils.ss"
         "private/random-stream.ss")
(require rackunit
         (for-syntax (cce text)))

(provide test-random)

(define-syntax (test-random stx)
  (syntax-case stx ()
    [(_ . rest) (syntax/loc stx (test-random/keywords () . rest))]))

(define-syntax (test-random/keywords stx)
  (syntax-case stx ()
    [(_ kvs key val . rest)
     (keyword-literal? #'key)
     (syntax/loc stx
       (test-random/keywords (key val . kvs) . rest))]
    [(_ (test-opt ...) ([lhs rhs opt ...] ...) body ...)
     (with-syntax ([([lhs* rhs* opt* ...] ...)
                    (reverse (syntax->list #'([lhs rhs opt ...] ...)))])
       (syntax/loc stx
         (make-random-test
          '(lhs* ...)
          (gen-random [lhs* rhs* opt* ...] ...)
          (lambda (lhs* ...)
            (with-check-info
             (['lhs lhs] ...)
             body ...))
          test-opt ...)))]))

(define-syntax (gen-random stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx (constant-random-stream null))]
    [(_ [lhs0 rhs0 #:where guard #:limit k] [lhs rhs opt ...] ...)
     (syntax/loc stx
       (make-cons-transducer
        (gen-random [lhs rhs opt ...] ...)
        (lambda (lhs ...) rhs0)
        #:where (lambda (lhs0 lhs ...) guard)
        #:limit k))]
    [(_ [lhs0 rhs0 #:limit k #:where guard] [lhs rhs opt ...] ...)
     (syntax/loc stx
       (gen-random [lhs0 rhs0 #:where guard #:limit k]
                   [lhs rhs opt ...] ...))]
    [(_ [lhs0 rhs0 #:where guard] [lhs rhs opt ...] ...)
     (syntax/loc stx
       (gen-random [lhs0 rhs0 #:where guard #:limit 1000]
                   [lhs rhs opt ...] ...))]
    [(_ [lhs0 rhs0 #:limit k] [lhs rhs opt ...] ...)
     (syntax/loc stx
       (gen-random [lhs0 rhs0 #:where #t #:limit k]
                   [lhs rhs opt ...] ...))]
    [(_ [lhs0 rhs0] [lhs rhs opt ...] ...)
     (syntax/loc stx
       (gen-random [lhs0 rhs0 #:where #t #:limit 1000]
                   [lhs rhs opt ...] ...))]))

(define (make-cons-transducer source f #:where pred #:limit limit)
  (random-stream-transducer
   source
   (lambda (input) (cons (apply f input) input))
   #:filter (lambda (output) (apply pred output))
   #:limit limit))

(define (make-random-test variables stream check
                          #:name [name "randomized test"]
                          #:repeat [repeat 100]
                          #:limit [limit 1000])
  (let* ([stream* (random-stream-transducer stream values #:limit limit)])
    (test-suite name
      (let/ec return
        (for {[i (in-range repeat)]}
          (when (random-stream-empty? stream*)
            (test-case (format "Trials #~s to #~s (unsatisfied preconditions)"
                         (add1 i) repeat)
              (fail (stream-empty-message i limit repeat variables stream)))
            (return (void)))
          (test-case (format "Trial #~s" (add1 i))
            (apply check (random-stream-next! stream*))))))))

(define (stream-empty-message i limit repeat vars stream)
  (with-output-to-string
    (lambda ()
      (printf "Out of ~a attempts to generate input data for" limit)
      (printf " ~a trials, only ~a satisfied the preconditions.\n" repeat i)
      (printf "\nFailed input data sets include:\n\n~a"
        (format-failures vars (random-stream-failures stream))))))

(define (format-failures reversed-names reversed-values)
  (let* ([names (reverse reversed-names)]
         [vals (take-up-to (map reverse reversed-values) 10)]
         [alists (map (lambda (val) (associate names val)) vals)]
         [strs (map format-failure alists)])
    (apply string-append (add-between strs "\n"))))

(define (take-up-to xs n)
  (for/list {[x (in-list xs)] [i (in-range n)]}
    x))

(define (format-failure alist)
  (apply string-append (map format-pair alist)))

(define (format-pair pair)
  (format "~s = ~s\n" (car pair) (cdr pair)))

(define (associate names vals)
  (if (or (null? names) (null? vals))
      null
      (cons (cons (car names) (car vals))
            (associate (cdr names) (cdr vals)))))
