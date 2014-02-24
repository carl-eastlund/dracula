#lang racket

(require
  racket/gui/dynamic
  racket/require
  (path-up "self/require.rkt")
  (cce-in scheme)
  (for-syntax (cce-in scheme))
  (prefix-in htdp: lang/private/teachprims)
  (prefix-in engine:
    (combine-in
      (lib "racket-tests.rkt" "test-engine")
      (lib "test-info.scm"    "test-engine"))))

(provide check-expect check-within check-error generate-report generate-report!)

(define-syntax (generate-report stx)
  (case (syntax-local-context)
    [(expression) (raise-syntax-error #f
                    "may not be used as an expression"
                    stx)])
  (syntax-case stx ()
    [(_) #'(begin)]
    [_ (raise-syntax-error #f
         "expected no arguments"
         stx)]))

(define (real-generate-report!)
  (match [engine:scheme-test-data]
    [(list rep event-space display)
     (let* ([engine (test-engine)])
       (send engine run)
       (when display
         (send engine refine-display-class display))
       (send engine setup-display rep event-space)
       (send engine summarize-results (current-output-port))
       (send engine reset-info))]))

(define-syntax-rule (generate-report!)
  (define-values []
    (begin
      (real-generate-report!)
      (values))))

(define (test-engine)
  (unless the-engine
    (set! the-engine (engine:builder)))
  the-engine)

(define the-engine #f)

(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_ actual expected)
     (with-syntax ([src (src->list stx)])
       (syntax/loc stx
         (define-values []
           (let ([engine (test-engine)]
                 [test (lambda () actual)])
             (send engine add-test
               (lambda ()
                 (check-expect-procedure test expected 'src engine)))
             (values)))))]
    [_ (raise-syntax-error #f
         "expected two arguments: expression to test and expected result"
         stx)]))

(define-syntax (check-within stx)
  (syntax-case stx ()
    [(_ actual expected delta)
     (with-syntax ([src (src->list stx)])
       (syntax/loc stx
         (define-values []
           (let ([engine (test-engine)]
                 [test (lambda () actual)])
             (send engine add-test
               (lambda ()
                 (check-within-procedure test expected delta 'src engine)))
             (values)))))]
    [_ (raise-syntax-error #f
         (string-append "expected three arguments: "
                        "expression to test, "
                        "expected result, and "
                        "acceptable difference")
         stx)]))

(define-syntax (check-error stx)
  (syntax-case stx ()
    [(_ actual message)
     (with-syntax ([src (src->list stx)])
       (syntax/loc stx
         (define-values []
           (let ([engine (test-engine)]
                 [test (lambda () actual)])
             (send engine add-test
               (lambda ()
                 (check-error-procedure test message 'src engine)))
             (values)))))]
    [_ (raise-syntax-error #f
         (string-append "expected two arguments: "
                        "expression to test and "
                        "expected error message")
         stx)]))

(define (check-expect-procedure thunk expected src engine)
  (notify engine)
  (let* ([result (with-handlers ([exn:fail? values]) (thunk))])
    (cond
     [(exn? result)
      (fail engine
            (engine:make-unexpected-error src
                                          (engine:test-format)
                                          expected
                                          (exn-message result)
                                          result)
            result)]
     [(htdp:beginner-equal? result expected) #t]
     [else
      (fail engine
            (engine:make-unequal src (engine:test-format) result expected)
            #f)])))

(define (check-within-procedure thunk expected delta src engine)
  (check rational? delta
         "check-within requires a rational number as the range; got ~a instead"
         delta)
  (notify engine)
  (let* ([result (with-handlers ([exn:fail? values]) (thunk))])
    (cond
     [(exn? result)
      (fail engine
            (engine:make-unexpected-error src
                                          (engine:test-format)
                                          expected
                                          (exn-message result)
                                          result)
            result)]
     [(htdp:beginner-equal~? result expected delta) #t]
     [else
      (fail engine
            (engine:make-outofrange src
                                    (engine:test-format)
                                    result
                                    expected
                                    delta)
            #f)])))

(define (check-error-procedure thunk message src engine)
  (check string? message
         "check-error requires a string as the expected message; got ~a instead"
         message)
  (notify engine)
  (let* ([result (with-handlers ([exn:fail? values]) (thunk))])
    (cond
     [(not (exn? result))
      (fail engine
            (engine:make-expected-error src
                                        (engine:test-format)
                                        message
                                        result)
            #f)]
     [(string=? (exn-message result) message) #t]
     [else
      (fail engine
            (engine:make-incorrect-error src
                                         (engine:test-format)
                                         message
                                         (exn-message result)
                                         result)
            result)])))

(define (check pred value fmt . args)
  (unless (pred value)
    (raise (make-exn:fail:contract (apply format fmt args)
                                   (current-continuation-marks)))))

(define (notify engine)
  (send (send engine get-info) add-check))

(define (fail engine failure exn)
  (send (send engine get-info) check-failed
    failure (engine:check-fail-src failure) exn)
  (and exn (raise exn)))
