#lang racket

(require "utils.ss")
(require (cce queue)
         (cce function))

;; A (RandomStreamof T) is (make-random-stream promise)
;; where promise : (RandomStreamPromiseof T)
;; A (RandomStreamPromiseof T) is a
;;  (Promiseof (Or (Box (Listof Any)) (cons T (RandomStreamPromiseof T))))
(define-struct random-stream (promise) #:mutable)

(define (random-stream-empty? rs)
  (box? (force (random-stream-promise rs))))

(define (random-stream-next! rs)
  (let* ([pair (force (random-stream-promise rs))])
    (set-random-stream-promise! rs (cdr pair))
    (car pair)))

(define (constant-random-stream v)
  (make-random-stream (constant-random-stream-promise v)))

(define (constant-random-stream-promise v)
  (delay (cons v (constant-random-stream-promise v))))

(define (random-stream-transducer source transform
                                  #:filter [pred (const #t)]
                                  #:limit [limit 1000])
  (make-random-stream
   (delay (make-transducer source transform pred limit (make-queue)))))

(define (make-transducer source f pred limit q)
  (let next ([count 0] [failures null])
    (if (transducer-empty? count limit source q)
        (box (append failures (random-stream-failures source)))
        (let* ([input (transducer-get source q)]
               [output (f input)])
          (if (pred output)
              (cons output (delay (next (add1 count) failures)))
              (begin
                (enqueue! q input)
                (next (add1 count) (cons output failures))))))))

(define (transducer-empty? count limit source q)
  (or (>= count limit)
      (and (random-stream-empty? source)
           (queue-empty? q))))

(define (transducer-get source q)
  (cond
   [(not (random-stream-empty? source)) (random-stream-next! source)]
   [(not (queue-empty? q)) (dequeue! q)]
   [else (error 'transducer-get "nothing to get")]))

(define (random-stream-failures rs)
  (if (random-stream-empty? rs)
      (unbox (force (random-stream-promise rs)))
      null))

(define (queue->list q)
  (if (queue-empty? q)
      null
      (let* ([v (dequeue! q)])
        (cons v (queue->list q)))))

(provide/contract
 [random-stream? (-> any/c boolean?)]
 [random-stream-empty? (-> random-stream? boolean?)]
 [random-stream-next! (-> random-stream? any/c)]
 [random-stream-failures (-> random-stream? (listof any/c))]
 [constant-random-stream (-> any/c random-stream?)]
 [random-stream-transducer
  (->* [random-stream? (-> any/c any/c)]
       [#:filter (-> any/c boolean?)
        #:limit exact-positive-integer?]
       random-stream?)])
