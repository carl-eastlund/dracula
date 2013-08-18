#lang racket

(require "../random.rkt"
         "../rackunit.rkt"
         rackunit
         rackunit/text-ui)

(run-tests
 (test-suite "FastTest"
   (test-suite "default"
     (test-random
      ([x (random-integer)]
       [z (+ x (random-natural))]
       [y (random-integer/uniform x z)])
      (check-true (<= x y z))))
   (test-suite "options"
     (test-random
      #:name "xyz"
      #:repeat 10
      #:limit 9
      ([x (random-integer) #:where (< x 0)]
       [z (+ x (random-natural)) #:where (> z 0)]
       [y (random-integer/uniform x z) #:where (< x y z) #:limit 8])
      (check-true (<= x y z))))
   (test-suite "failure"
     (test-random #:name "backwards"
       ([x (random-integer)]
        [y (+ x (random-natural))])
       (check > x y)))))
