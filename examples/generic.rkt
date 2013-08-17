#lang dracula/kernel

(define-values {ONE}
  (#%plain-lambda {} '#:type
    (#%plain-lambda {} '#:instance
      (#%plain-lambda {}
        (letrec-values {[{x} (#%plain-lambda {} '#:stub '0)]}
          (#%plain-app values 'x x))))))

(define-values {TWO}
  (#%plain-lambda {} '#:type
    (#%plain-lambda {} '#:instance
      (#%plain-lambda {}
        (letrec-values {[(One) ONE]}
          (#%plain-app values 'One One))))))

(define-values {EMPTY}
  (#%plain-lambda {} '#:type
    (#%plain-lambda {} '#:instance
      (#%plain-lambda {}
        (#%plain-app values)))))

(define-values {F}
  (#%plain-lambda {A B}
    (begin0
      (#%plain-app #%seal EMPTY
        (#%plain-app #%instance
          (#%plain-lambda {}
            (#%plain-app values))))
      ONE
      (#%plain-lambda {} '#:refine TWO '(One) A))))

(define-values {G}
  (#%plain-lambda {C D}
    (begin0
      (#%plain-app #%seal EMPTY
        (#%plain-app #%instance
          (#%plain-lambda {}
            (letrec-values {[(I) (#%plain-app #%instantiate F C D)]}
              (#%plain-app values 'I I)))))
      ONE
      (#%plain-lambda {} '#:refine TWO '(One) C))))
