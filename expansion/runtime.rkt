#lang mischief

(provide
  #%instance
  #%seal
  #%instantiate
  #%deref
  #%assert
  #%validate)

(define (#%validate f in-lists outs [source #false])
  (for {[ins (in-list in-lists)]
        [out (in-list outs)]}
    (#%assert (equal? (apply f ins) out) source)))

(define (#%assert x [source #false])
  (unless x
    (error 'assert
      "~aAssertion failed.~a"
      (source-location->prefix source)
      (if source
        (format "\n  expression: ~s" (to-datum source))
        ""))))

(define (#%instance proc) (call-with-values proc hash))
(define (#%seal type term) term)
(define (#%instantiate gen . args) (apply gen args))
(define (#%deref comp field) (hash-ref comp field))
