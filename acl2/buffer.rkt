#lang racket

(define buffer (make-bytes 1024))

(define (read-available-string port)
  (let* ([bytes (read-available-bytes port)])
    (if (eof-object? bytes) bytes (bytes->string/utf-8 bytes))))

(define (read-available-bytes port)
  (read-available-bytes/offset port 0))

(define (read-available-bytes/offset port offset)
  (let* ([result (read-bytes-avail!* buffer port offset)])
    (if (eof-object? result)
        (if (zero? offset) result (subbytes buffer 0 offset))
        (let* ([new-offset (+ offset result)])
          (if (= new-offset (bytes-length buffer))
              (begin (set! buffer (bytes-append buffer buffer))
                     (read-available-bytes/offset port new-offset))
              (subbytes buffer 0 new-offset))))))

(provide/contract
 [read-available-bytes (-> input-port? (or/c bytes? eof-object?))]
 [read-available-string (-> input-port? (or/c string? eof-object?))])
