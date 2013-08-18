#lang scribble/doc
@(require scribble/manual
          "display.ss"
          "../lang/acl2-module-v.ss"
          "../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in modular/main)))

@title[#:tag "modular"]{Modular ACL2}

Modular ACL2 lets you split up large programs into pieces that may be developed
and verified separately, then linked together and executed as trusted code.
This section of the guide assumes you are familiar with Dracula and ACL2; review
the @secref["install"] and @secref["acl2"] sections if you are not.

To run Modular ACL2, open the @onscreen{Language} menu, then the
@onscreen{Choose Language...} dialog.  Under the @onscreen{Dracula} heading,
select @onscreen{Modular ACL2}.

Modular ACL2 includes all of ACL2's built-in definitions.  You can execute ACL2
expressions in the interactions window, with the usual results:

@eval[
[]
[(+ 1 2)]
]

@section{Specify the Problem}

To write a program, you must first specify the problem you intend to
solve. Modular ACL2 expresses specifications as interfaces.  An interface may
contain signatures, which specify the name and arity of functions, and
contracts, which specify logical properties (usually about the same functions).

Consider the problem of multiplying a number by 3.  The specification of
this problem must describe a unary function whose output is three times its
input.  Here is an interface named @scheme[ITriple] that describes this
specification with a signature @scheme[triple] and a contract
@scheme[times-three]:

@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))
]

Enter this interface in the definitions window.  Now you have the specification
for a problem to solve with Modular ACL2.

@section{Divide and Conquer}

The next step in writing a program is providing a concrete implementation for
the specification.  In Modular ACL2, these implementations take the form of
modules.  To implement an interface, you must write a module that exports it.
Here is the beginning of a module that exports @scheme[ITriple]:

@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(module MTriple
  (code:comment #, @t{fill in implementation})
  ...
  (export ITriple))
]

This module must implement the specification described by @scheme[ITriple].  It
needs to define a unary function named @scheme[triple], and that function has to
satisfy the contract @scheme[times-three].  Here is a definition for
@scheme[triple] that reduces the problem to two subproblems: doubling and
addition.

@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(module MTriple
  (code:comment #, @t{solve subproblems})
  ...
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))
]

The @scheme[MTriple] module now only needs a solution for these subproblems.
Addition is easy; ACL2 provides the @scheme[+] primitive.  Doubling represents a
new problem, and requires a new specification.  We can write the specification
as a new interface @scheme[IDouble] and import it into @scheme[MTriple]:

@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(interface IDouble
  (sig double (x))
  (con times-two
    (= (double x) (* x 2))))

(module MTriple
  (import IDouble)
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))
]

@section{Repeat}

To complete the subproblem, we must write a module to implement @scheme[IDouble]
by the same process we wrote @scheme[MTriple] to implement @scheme[ITriple].  We
write the module @scheme[MDouble] which exports @scheme[IDouble] and defines
@scheme[double].  Doubling immediately reduces to the already solved problem of
addition, completing our second module @scheme[MDouble]:

@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(interface IDouble
  (sig double (x))
  (con times-two
    (= (double x) (* x 2))))

(module MTriple
  (import IDouble)
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))

(module MDouble
  (defun double (x)
    (+ x x))
  (export IDouble))
]

We now have a solution to our problem in two parts: we can double a number, and,
given a method for doubling a number, we can triple it.

@section{Verify}

Before trusting this code enough to run it, we should verify that it faithfully
implements its specification.  Dracula allows us to verify each module
separately using ACL2.  Open the drop-down box on the Dracula theorem proving
interface, initially labeled @onscreen{<Choose A Module>}.  When it presents the
list of modules, choose @scheme[MTriple].  You may now @onscreen{Admit} the
definitions in @scheme[MTriple] to verify them.

Modular ACL2 sends the definitions in the body of the module to ACL2.  Imported
interfaces are converted into an assumption of correctness.  The line
@scheme[(import IDouble)] in @scheme[MTriple] becomes:
@show[
(defstub double (x) t)
(defaxiom times-two
  (= (double x) (* x 2)))
]
The stub assumes the existence of a unary function named @scheme[double].  The
axiom expresses the assumption that @scheme[double] produces twice its input.

Definitions in the module are passed unchanged to ACL2; they may rely on
preceding definitions and imported assumptions.  The function @scheme[triple]
remains the same, and refers to the stub @scheme[double] from above:
@show[
(defun triple (x)
  (+ (double x) x))
]

Exported interfaces are converted to a conjecture of correctness that ACL2 must
verify, based on previous imports and definitions in the module.  Exported
signatures are not sent to ACL2 separately; rather, they must correspond to a
previous definition inside the module.  Contracts are translated to theorems
that must be proved about the module body.  The line @scheme[(export ITriple)]
in @scheme[MTriple] becomes:
@show[
(defthm times-three
  (= (triple x) (* x 3)))
]
Note that the signature for @scheme[triple] does not appear; it is subsumed by
the previous definition of the function @scheme[triple].  The conjecture
@scheme[times-three] becomes a theorem about @scheme[triple].

The full text sent to ACL2 for @scheme[MTriple] is:
@show[
(defstub double (x) t)
(defaxiom times-two
  (= (double x) (* x 2)))
(defun triple (x)
  (+ (double x) x))
(defthm times-three
  (= (triple x) (* x 3)))
]
This represents a proof that, given a method for doubling a number,
@scheme[triple] will faithfully triple that number.  ACL2 successfully admits
this proof.

Once @scheme[MTriple] is admitted, we must also verify @scheme[MDouble].  Note
that @scheme[MTriple] imports @scheme[IDouble], assuming a correct
implementation, while @scheme[MDouble] exports it, and must verify its
correctness.  Therefore, we must close ACL2 and start a new session when
switching modules so that the assumptions used in one module do not interfere in
the proof of another.

Close ACL2 with the @onscreen{Stop} button; then open the module drop-down list
again and select @scheme[MDouble].  Modular ACL2 converts @scheme[MDouble] and
its exported interface to plain ACL2 by the same process as above.  The full
text sent to ACL2 is:
@show[
(defun double (x)
  (+ x x))
(defthm times-two
  (= (double x) (* x 2)))
]

ACL2 admits these definitions, verifying that @scheme[MDouble] provides a
correct method for doubling a number.

@section{Link and Execute}

Now that both modules have been verified, we can safely link and run them.
Construct a new module combining the implementations of @scheme[MDouble] and
@scheme[MTriple]:
@show[
(link MDoubleTriple
  (MDouble MTriple))
]
This defines @scheme[MDoubleTriple] to be a module providing the exports of both
@scheme[MDouble] and @scheme[MTriple], using the implementation of each.
Furthermore, exports of @scheme[MDouble] are linked to corresponding imports of
@scheme[MTriple].  The resulting module has no imports, and exports both
@scheme[IDouble] and @scheme[ITriple].

Note that linking is applicative.  The original @scheme[MDouble] and
@scheme[MTriple] are unchanged, and may be linked again to other modules.

The module @scheme[MDoubleTriple] does not need to be verified.  It contains no
new proof obligations beyond those of @scheme[MDouble] and @scheme[MTriple],
which are already verified.  Verified modules in Modular ACL2 may be linked in
new contexts without being reverified.

Now that we have an implementation of @scheme[ITriple] that has no remaining
imports, we can invoke it to expose its exports and run it at the interactions
window.  The complete program, including the final invocation, is:
@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(interface IDouble
  (sig double (x))
  (con times-two
    (= (double x) (* x 2))))

(module MTriple
  (import IDouble)
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))

(module MDouble
  (defun double (x)
    (+ x x))
  (export IDouble))

(link MDoubleTriple
  (MDouble MTriple))

(invoke MDoubleTriple)
]
This allows us to triple numbers in the interactions window.
@eval[
[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(interface IDouble
  (sig double (x))
  (con times-two
    (= (double x) (* x 2))))

(module MTriple
  (import IDouble)
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))

(module MDouble
  (defun double (x)
    (+ x x))
  (export IDouble))

(link MDoubleTriple
  (MDouble MTriple))

(invoke MDoubleTriple)
]
[
(triple 2)
(triple (triple 2))
]
]

@section{Saving Files}

Now that we have a running file, we would like to save it to disk and use it
later.  Save the file as @filepath{example.lisp}.

@subsection{Metadata}

If you load @filepath{example.lisp} in another editor or language level, you will
see some extra lines at the top:
@show[
#, @litchar{;; The first four lines of this file were added by Dracula.}
#, @litchar{;; They tell DrScheme that this is a Dracula Modular ACL2 program.}
#, @litchar{;; Leave these lines unchanged so that DrScheme can properly load this file.}
#, @litchar{#reader(planet "reader.ss" ("cce" "dracula.plt") "modular" "lang")}
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(interface IDouble
  (sig double (x))
  (con times-two
    (= (double x) (* x 2))))

(module MTriple
  (import IDouble)
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))

(module MDouble
  (defun double (x)
    (+ x x))
  (export IDouble))

(link MDoubleTriple
  (MDouble MTriple))

(invoke MDoubleTriple)
]
These lines are hidden in the Modular ACL2 language level, but required so
DrScheme knows how to load the file.  Do not change them.

@subsection{Multiple Files}

It is often convenient to split large programs into multiple files to allow
separate or concurrent development of modules.  Modular ACL2 allows any
collection of interfaces and modules to be put in a separate file and loaded in
another.  A common organization is to put all the specifications in one file, so
that modules may refer to any or all of them; put each module in a separate
file, so they may be modified separately; and put the final linking and
invocation in yet another file.

Save the following as @filepath{interfaces.lisp}:
@show[
(interface ITriple
  (sig triple (x))
  (con times-three
    (= (triple x) (* x 3))))

(interface IDouble
  (sig double (x))
  (con times-two
    (= (double x) (* x 2))))
]

Then save @filepath{double.lisp}.  Note that we load @scheme[IDouble] from
@filepath{interfaces.lisp} with @scheme[require]:
@show[
(require "interfaces.lisp")

(module MDouble
  (defun double (x)
    (+ x x))
  (export IDouble))
]

Next, save @filepath{triple.lisp}:
@show[
(require "interfaces.lisp")

(module MTriple
  (import IDouble)
  (defun triple (x)
    (+ (double x) x))
  (export ITriple))
]

Finally, save @filepath{run.lisp}:
@show[
(require "double.lisp")
(require "triple.lisp")

(link MDoubleTriple
  (MDouble MTriple))

(invoke MDoubleTriple)
]

Now you have a working program to triple any number that is fully verified and
split up into multiple, reusable, separately editable components.  Note that
verification with ACL2 only examines the modules in a given file.  For instance,
you must open @filepath{double.lisp} to verify @scheme[MDouble] and
@filepath{triple.lisp} to verify @scheme[MTriple].

@section{Final Words}

Good luck using Modular ACL2!  For more details, see @secref[#:doc
(make-dracula-spec "reference/reference.scrbl") "modular"] in
@other-manual[(make-dracula-spec "reference/reference.scrbl")].
