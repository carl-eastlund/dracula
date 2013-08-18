#lang scribble/doc
@(require scribble/manual
          scheme/runtime-path
          "../lang/acl2-module-v.ss"
          "../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@(define-runtime-path ChosenLanguage "images/chosen-language.png")
@(define-runtime-path fact-defn "images/fact-defn.png")
@(define-runtime-path fact-10 "images/fact-10.png")
@(define-runtime-path start-acl2 "images/start-acl2.png")
@(define-runtime-path acl2-started "images/acl2-started.png")
@(define-runtime-path fact-admitted "images/fact-admitted.png")
@(define-runtime-path fact-with-failed "images/fact-with-failed.png")
@(define-runtime-path admitted-theorem "images/admitted-theorem.png")

@title[#:tag "acl2"]{ACL2}

We will illustrate how to use our software via this short walkthrough. It
assumes that you have already installed the Dracula software. If you have
not, please refer to the @secref["install"] section.

If you have installed the software, start up DrScheme and read on!

@subsubsub*section{Choose the ACL2 Language Level}

The first step is to set your language level in DrScheme. Under the Language
menu, select @schemefont{Choose Language...}. A dialog will pop up. Under the
heading @bold{Dracula}, choose @schemefont{ACL2}.

@image[ChosenLanguage]

@subsubsub*section{Definitions Window}

The editing area in the DrScheme window is split into two sections. The top
is the Definitions Window, and that is where we edit programs. Type in
the definition of the factorial function:

@schemeblock[
  (defun fact (n)
    (if (zp n)
        1
        (* n (fact (1- n)))))
]

and then click @schemefont{Run}.

@image[fact-defn]

@subsubsub*section{Interactions Window}

You may now test your function interactively by typing expressions at the
prompt in the bottom half of the DrScheme Window. Try evaluating
@scheme[(fact 10)] at the prompt by typing in the expression followed
by a return. DrScheme will print out the answer @scheme[3628800].

Try making an error in the interactions window. For example, try to evaluate
@scheme[(fact -4)]. This is an error because the predicate @scheme[zp] only  
works on natural numbers, and you'll notice that DrScheme prints an error 
message to that effect. It has also highlighted @scheme[zp] in the 
definitions window to indicate which call received the invalid arguments. 

@image[fact-10]

@;{
@subsubsub*section{Help Desk}

ACL2's documentation is searchable via DrScheme's Help Desk. Highlight the
predicate @scheme[zp] in either the Definitions or Interactions window, and
then press F1. Follow the links to read the documentation on @scheme[zp] or
use the text field at the bottom of Help Desk to search for other
documentation.
;}

@subsubsub*section{Start ACL2}

The right-hand side of the window shows the ACL2 console. The top half shows
ACL2 proof trees, and the bottom half shows ACL2's output. The text here is
read-only; ACL2 interaction will happen via the buttons in DrScheme. Begin 
by clicking the @schemefont{Start} button. If this is your first time starting 
Dracula, you will be prompted to locate an ACL2 executable. Once you have
located it, ACL2 will start up and you will see its output in the console.

@image[start-acl2]

@;{@image[acl2-started]}


@subsubsub*section{Admitting a Term}

Click @schemefont{Admit}. This sends your definition of factorial to ACL2. 
ACL2 will attempt to prove that your function terminates on every input, 
and, if it can, your definition will be highlighted in green. If ACL2 rejects 
your definition, it will highlight it in red. If ACL2 rejects your code, edit
it and try again. If the next unhighlighted expression is ill-formed, DrScheme
will not send it to ACL2.

@;{@image[fact-admitted]}

@image[fact-with-failed]

In order to keep ACL2 and DrScheme in sync, code that is highlighted green 
cannot be edited. If you wish to edit a green expression, then click 
@schemefont{Undo} until the expression is unhighlighted. You may also click
@schemefont{Reset} to unhighlight all expressions in the definitions window.

The @schemefont{All} button will send all expressions in the definitions
window to ACL2. If one of them is rejected, DrScheme will highlight it red
and not attempt to admit the remaining expressions. 

The @schemefont{To Cursor} button will either attempt admit terms up to the
Definitions Window's cursor, or undo terms back to it, depending on where
the cursor is. By placing your cursor in the middle of a term, you can
alternate between admitting and undoing the term by clicking the
@schemefont{To Cursor} button repeatedly.

@subsubsub*section{Proving a Theorem}

Now you can prove that factorial always produces a positive number. Enter the following code after your definition of factorial:

@schemeblock[
  (defthm fact-produces-positive
    (posp (fact n)))
]

Click @schemefont{Admit Next} to send the theorem to ACL2. The theorem should
become highlighted in green. Now, look back at the proof tree window. You'll see
in the top it says @bold{Q.E.D.} just below the theorem's name.

@image[admitted-theorem]

When ACL2 is in the process of proving a conjecture, or if it fails to prove
one, a proof tree will be displayed in the proof tree window. For 
more information on using proof trees and checkpoints to debug failed proof
attempts, search for ``proof-tree'' in the ACL2 documentation. 

@;{
@subsubsub*section{Certifying a Book}

You can provide your programs and proofs to other users as a book. Each
book must start with the declaration @scheme[(in-package "ACL2")]. Add this
line above your definition of @scheme[fact]. Now click the
@schemefont{Save / Certify} button. ACL2 will report success: you have
certified this file as a book. It is guaranteed to contain only valid
programs and theorems.

Now use the book. Save your code and close your existing ACL2 session by
clicking @schemefont{Stop}. Open a new file and enter the following code:

@schemeblock[
  (include-book "fact")
  (equal (fact 3) 6)
]

The first line looks for @schemefont{fact.lisp} in the current directory and
makes all its definitions available for execution and theorem proving. Save
this code in the same directory as fact.lisp and run it. The test case should
produce @scheme[t]. If you start ACL2 for this file and click @schemefont{All},
ACL2 should accept this file.

If you include an uncertified book, ACL2 will issue a warning but proceed
anyway. Proofs based on uncertified books may contain errors, and programs
may run without terminating. Always certify books before you use or 
distribute them.
;}

@subsubsub*section{Stop ACL2}

To stop ACL2, click the @schemefont{Stop} button. This will terminate
the running ACL2 process for that window, and will clear the output and
proof tree windows. If you wish to save ACL2's output before stopping,
you may do so by clicking @schemefont{Save ACL2 Output As...} from the @schemefont{Dracula} menu.

@subsubsub*section{Explore the Sample Code}

Now that you have a handle on the basics of using ACL2 via DrScheme, see
@other-manual[(make-dracula-spec "reference/reference.scrbl")] for a full
description of the ACL2 language.  Also, see the
@link["http://www.ccs.neu.edu/home/cce/acl2/examples.html"]{Sample Code} at the
Dracula web page to explore more in-depth examples.
