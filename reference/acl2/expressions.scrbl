#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{Expressions}

Dracula supports these built-in expression forms.

@defform[#:id #%app (<function-name> expr ...)]{
Call a function by writing a sequence starting with its name, followed by
expressions to compute its arguments.  See @secref["library"] for a list of the
functions provided by Dracula, or @secref["events"] to learn how to define your
own.
@examples[
#:eval the-evaluator
(identity 'VALUE)
(reverse "a man a plan a canal panama")
]
}

@defform[(case expr (x1 val1) ...)]{
Checks the value of @scheme[expr] against @scheme[x1] using @scheme[eql]. If it matches, @scheme[val1] is returned. Otherwise, it checks any the subsequent branches. The optional @scheme[otherwise] branch is evaluated whenever all the ones before it failed. If no branches match (and an @scheme[otherwise] branch is not given), the case statement returns @scheme[nil].
@examples[
#:eval the-evaluator
(case (+ 6 4)
      (11 10)
      (10 7)
      (otherwise 'error))
(case 6 (5 'match))
]
}

@defform[(case-match expr (pattern declaration body)...+)]{
Returns the value of the @scheme[body] that corresponds to the
first matching @scheme[pattern].
 
See ACL2's documentation for the pattern language used by case-match.
The @scheme[declaration] expressions are optional.
@examples[
#:eval the-evaluator
(defconst *list* (list 1 2 3 2 1))
(case-match *list*
  ((x x y z z)
   (list z y x))
  ((x y z y x)
   (list x y z)))    
]
}

@defform[(cond (x1 y1)...)]{
Checks the @scheme[x]s in the given order, and returns the corresponding @scheme[y] for the first @scheme[x] to not evaluate to @scheme[nil]. If no @scheme[x]s match, the cond statement returns @scheme[nil].
@examples[
#:eval the-evaluator
(cond 
 ((< -5 0) (* -1 -5))
 (t -5))
(cond (nil 'no) (t 'yes))
(cond (nil 'no))
]
}

@defform[(if p q r)]{
Returns @scheme[r] if @scheme[p] evaluates to nil; otherwise returns @scheme[q].
@examples[
#:eval the-evaluator
(if (< 0 3) 'yes 'no)
(if (< 3 0) 'yes 'no)
(if 5 'yes 'no)
(if 'blah 'yes 'no)
(if nil (+ "str" 'sym) 5)
]
}

@defform[(let ((var1 term1)... ) body)]{
Used for binding local variables. All @scheme[term]s are effectively evaluated in parallel; no term may refer to a variable that is bound in the same @scheme[let] expression.
@examples[
#:eval the-evaluator
(let ((x 4)) (+ x 6))
(let ((a 1) (b 2)) (+ a b))
(let ((a 1) (b (1+ a))) b)
]
}

@defform[(let* ((var1 term1)... ) body)]{
Used for binding local variables. All @scheme[term]s are evaluated sequentially, so they may refer to other variables defined in the same @scheme[let] expression
@examples[
#:eval the-evaluator
(let* ((x 4)) (+ x 6))
(let* ((a 1) (b 2)) (+ a b))
(let* ((a 1) (b (1+ a))) b)
]
}

@defform[(mv-let (var1 var2 var...)
                 (mv term1 term2 term...) 
                 body)]{
Binds each @scheme[var] to its corresponding @scheme[term] in the @scheme[mv]
expression, then evaluates the body.
@examples[
#:eval the-evaluator
(mv-let (x y z)
        (mv 1 2 3)
        (list x y z))
]
}
-