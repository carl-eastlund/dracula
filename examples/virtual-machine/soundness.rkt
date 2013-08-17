#lang dracula

(require "descriptions.rkt")

(description STEP
  (~component Compiler #:> COMPILER)
  (~component Machine #:> VIRTUAL-MACHINE #:where {[Compiler #:= Compiler]})

  (~theorem (step-soundness s)
    (implies (and ((dot Machine state?) s)
               (not ((dot Machine error?) s))
               (not ((dot Machine finished?) s)))
      ((dot Machine state?) ((dot Machine step) s)))))

(generic (Step-Verification
           [C #:> COMPILER]
           [VM #:> VIRTUAL-MACHINE #:where {[Compiler #:= C]}])
  #:> STEP #:where {[Compiler #:= C] [Machine #:= VM]}

  (use C VM)
  (use VM.Code)

  (theorem (step-soundness s)
    (implies (and (VM.state? s)
               (not (VM.error? s))
               (not (VM.finished? s)))
      (VM.state? (VM.step s)))))

(generic (Verification
           [C #:> COMPILER]
           [VM #:> VIRTUAL-MACHINE #:where {[Compiler #:= C]}]
           [Step #:> STEP #:where {[Compiler #:= C] [Machine #:= VM]}])
  #:> VERIFICATION #:where {[Compiler #:= C] [Machine #:= VM]}

  (use C VM)

  (theorem (execution-soundness n s)
    (implies (VM.state? s)
      (VM.state? (VM.execute n s))))

  (theorem (initialization-soundness e)
    (implies (and (C.expr? e)
               (C.verified? e))
      (VM.state? (VM.initialize e))))

  (theorem (soundness e n)
    (implies (and (C.expr? e)
               (C.verified? e))
      (VM.state?
        (VM.execute n
          (VM.initialize e))))))
