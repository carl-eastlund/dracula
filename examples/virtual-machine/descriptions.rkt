#lang dracula

(provide
  (all-defined-out))

(description STACK

  (~define (empty))
  (~define (push s))
  (~define (pop s))
  (~define (alloc s xs))
  (~define (ref s n))
  (~define (set s n x))
  (~define (take s n))

  (~define (ref-all s ns)
    (match ns
      ['() '()]
      [(cons n ns)
       (cons (ref s n)
         (ref-all s ns))])))

(description COMPILER

  (~datatype bytecode
    [expr
     (loc
       [index natural?])
     (lam
       [arity natural?]
       [env list-of-natural?]
       [body expr?])
     (app
       [fun expr?]
       [args list-of-expr?])]
    [list-of-expr #:list-of expr?])

  (~define (verified? e)))

(description MACHINE-CODE

  (~component Compiler #:> COMPILER)

  (~datatype-definitions machine-value
    [value
     (uninit)
     (closure
       [arity natural?]
       [env frame?]
       [body (dot Compiler expr?)])]
    [frame #:list-of value?])

  (~datatype-definitions machine-stack
    [stack #:list-of frame?])

  (~datatype-definitions machine-instr
    [instr
     (pop)
     (push)
     (set)
     (swap)
     (call [arity natural?])
     (run [expr (dot Compiler expr?)])])

  (~datatype-definitions machine-instrs
    [list-of-instr #:list-of instr?])

  (~datatype-definitions machine-state
    [state
     (registers
       [value value?]
       [stack stack?]
       [code list-of-instr?])
     (error)]))

(description VIRTUAL-MACHINE

  (~component Compiler #:> COMPILER)
  (~component Stack #:> STACK)
  (~component Code #:> MACHINE-CODE
    #:where {[Compiler #:= Compiler]})

  (~define (finished? s)
    (and ((dot Code registers?) s)
      (empty? ((dot Code registers-code) s))))

  (~define (initialize e)
    ((dot Code registers)
      ((dot Code uninit))
      ((dot Stack push) ((dot Stack empty)))
      (list ((dot Code run) e))))

  (~define (execute n s)
    #:measure (fix-natural n)
    (cond
      [(natural-zero? n) s]
      [((dot Code error?) s) s]
      [(finished? s) s]
      [#:else (execute (sub1 n) (step s))]))

  (~define (step state)))

(description VERIFICATION
  (~component Compiler #:> COMPILER)
  (~component Machine #:> VIRTUAL-MACHINE #:where {[Compiler #:= Compiler]})
  (~theorem (soundness e n)
    (implies (and ((dot Compiler expr?) e)
               ((dot Compiler verified?) e))
      ((dot Machine Code state?)
        ((dot Machine execute) n
          ((dot Machine initialize) e))))))
