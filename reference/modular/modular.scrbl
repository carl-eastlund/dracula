#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../evaluator.ss"
          "../../guide/display.ss"
          "../../lang/acl2-module-v.ss")
@(require (cce scribble))
@(require (for-syntax scheme/base)
          (for-label dracula/modular/main))

@title[#:tag "modular"]{Modular ACL2}

This section defines the Modular ACL2 language provided by Dracula.  For a
gentler introduction, see @secref["modular" #:doc (make-dracula-spec
"guide/guide.scrbl")] in @other-manual[(make-dracula-spec
"guide/guide.scrbl")].

@(declare-exporting [dracula/modular/main] #:use-sources [lang/acl])

@section{Interfaces}

@defform/subs[
#:literals [sig con include]
(interface name spec ...)
([spec (sig sig-name (arg-name ...))
       (sig sig-name (arg-name ...) (res-name ...))
       (con con-name expr)
       (include ifc-name)])
]{

Describes the external bindings of a module, introducing a name and
a set of specifications.  Specifications comprise the following three forms:

@defsubform*[[
(sig sig-name (arg-name ...))
(sig sig-name (arg-name ...) (res-name ...))
]]{

Describes the name, arity, and (optionally) multiple return values of a
function.  For example, the signature @scheme[(sig f (x y))] can be used to
describe:

@schemeblock[(defun f (x y) (cons y x))]

It can also describe a stub:

@schemeblock[(defstub f (x y) t)]

The names of the arguments is not significant; @scheme[(sig f (x y))] also
describes:

@schemeblock[(defun f (a b) (cons a b))]

The name of the function and number of arguments @italic{are} significant;
@scheme[(sig f (x y))] does not describe:

@schemeblock[(defstub g (x y z) t)]

Signatures may also describe functions/stubs with multiple return values.
The signature @scheme[(sig f (a b) (c d))] describes the function:

@schemeblock[(defun f (x y) (mv x y))]

and the signature:

@schemeblock[(defstub f (x y) (mv x y))]

}

@defsubform[(con con-name body-expr)]{

Describes a logical contract with the given name and body.  The body of a
contract may refer to ACL2 primitives, as well as signatures within the same
interface or any included interface.

For example, @scheme[(con f=identify (equal (f x) x))] describes the following
conjecture:

@schemeblock[(defthm f=identity (equal (f x) x))]

A contract may also describe an axiom:

@schemeblock[(defaxiom f=identity (equal (f x) x))]

The contract's name and body cannot be changed; @scheme[(con f=identify (equal
(f x) x))] does not describe:

@schemeblock[(defthm f-is-identity (equal x (f x)))]

}

@defsubform[(include ifc-name)]{

Imports one interface into another.  It may refer to any previously-defined
interface.  Contracts following the include may refer to signatures from the
included interface.

Include specifications also entail an obligation; if interface @scheme[C]
includes @scheme[B] and @scheme[B] includes @scheme[A], then @scheme[C] must
include @scheme[A] as well.  @emph{This obligation may be relaxed in the future,
so that including @scheme[B] will automatically include @scheme[A].}

}

For example, the interface @scheme[IAssociative] contains a signature describing
a binary function and a contract constraining it to be associative.  The
contract @scheme[mulop-associative] refers to the signature @scheme[mulop] as
well as the built-in ACL2 function @scheme[equal]:

@schemeblock[
(interface IAssociative
  (sig mulop (x y))
  (con mulop-associative
    (equal (mulop a (mulop b c))
           (mulop (mulop a b) c))))
]

The interface @scheme[ICommutative] includes @scheme[IAssociative] and
further constrains @scheme[mulop] to be commutative:

@schemeblock[
(interface ICommutative
  (include IAssociative)
  (con mulop-commutative
    (equal (mulop a b) (mulop b a))))
]

The interface @scheme[IDistributive] includes @scheme[ICommutative],
introduces a new function @scheme[addop], and states a contract that
@scheme[mulop] distributes over @scheme[addop].  Note that
@scheme[IDistributive] must also include @scheme[IAssociative] to satisfy the
obligation from @scheme[ICommutative]:

@schemeblock[
(interface IDistributive
  (include IAssociative)
  (include ICommutative)
  (sig addop (x y))
  (con addop/mulop-distributive
    (equal (mulop a (addop b c))
           (addop (mulop a b) (mulop a c)))))
]

The interface @scheme[IDistributeLists] extends @scheme[IDistributive] with
functions that perform @scheme[addop] and @scheme[mulop] on lists.  Its contract
@scheme[addall/mulall-distributive] refers to its own signatures, the imported
signatures @scheme[mulop] and @scheme[addop], and the built-in functions
@scheme[equal], @scheme[implies], and @scheme[proper-consp].

@schemeblock[
(interface IDistributeLists
  (include IAssociative)
  (include ICommutative)
  (include IDistributive)
  (sig addall (xs))
  (sig mulall (x ys))
  (con addall/mulall-distributive
    (implies (proper-consp bs)
             (equal (mulop a (addall bs))
                    (addall (mulall a bs))))))
]

}

@section{Modules}

@defform/subs[
#:literals [import export]
(module name body ...)
([body def
       expr
       (import ifc-name (ext-name int-name) ...)
       (export ifc-name (ext-name int-name) ...)])
]{

Defines a composable, verifiable program fragment, introducing a name and a set
of body forms.  Forms may be definitions, expressions, import declarations, or
export declarations.

@defsubform[(import ifc-name (ext-name int-name) ...)]{

Makes the bindings of any previously defined interface available inside a
module.  The module may use signatures as function names and rely on contracts
as axioms.  If an imported interface includes any other interfaces, the module
must import them as well.

An import declaration may optionally bind some or all of the interface's
external names to different internal names inside the module.  For instance, the
following import declaration binds @scheme[mulop] from @scheme[IAssociative] to
@scheme[times]:

@schemeblock[
(import IAssociative (mulop times))
]

}

@defsubform[(export ifc-name (ext-name int-name) ...)]{

Makes the implementation of a previously defined interface available to clients
of a module.  The module must provide a definition (internal, imported, or ACL2
built-in) for each signature in the interface; these definitions must satisfy
the interface's contracts.  If the interface includes other interfaces, the
module must either import or export them as well.

The export declaration may supply definitions whose internal names differ from
their external signaturse.  For instance, the following export declaration
provides the builtin ACL2 function @scheme[*] as @scheme[mulop] to implement
@scheme[IAssociative].

@schemeblock[
(export IAssociative (mulop *))
]

}

The module @scheme[MDistributive] provides an implementation of
@scheme[IDistributive]:

@schemeblock[
(module MDistributive
  (defun add (a b) (+ a b))
  (defun mul (a b) (* a b))
  (export IAssociative (addop add))
  (export ICommutative (addop add))
  (export IDistributive (addop add) (mulop mul)))
]

The module @scheme[MDistributeLists] provides an implementation of
@scheme[IDistributeLists] for any implementation of @scheme[IDistributive]:

@schemeblock[
(module MDistributeLists
  (import IAssociative (addop a))
  (import ICommutative (addop a))
  (import IDistributive (addop a) (mulop m))
  (defun add (xs)
    (if (endp (cdr xs))
        (car xs)
        (a (car xs) (add (cdr xs)))))
  (defun mul (x ys)
    (if (endp xs)
        nil
        (cons (m x (car ys)) (mul x (cdr ys)))))
  (export IDistributeLists (addall add) (mulall mul)))
]

}

@defform*[
[(link name (mod-name ...))
 (link name (import in-ifc ...) (export out-ifc ...) (mod-name ...))]
]{

Creates a composable program fragment by joining together multiple modules.
External names are resolved to link one module's imports to another's exports;
the export must come before the import to prevent cyclic dependencies.  Internal
names are kept separate; lexical scope is not violated.

The second form gives explicit imports and exports for the linked module.  The
imports list must at least include all interfaces imported by the combination of
each @racket[mod-name]; it may optionally include additional imports.
The exports list must only include interfaces exported by the combination of
each @racket[mod-name]; it may optionally exclude some of these exports.

@schemeblock[(link (MDist MDistributive MDistributeLists))]

The compound module @scheme[MDist] links together @scheme[MDistributive] with
@scheme[MDistributeLists].  The result is a module that exports both
@scheme[IDistributive] and @scheme[IDistributeLists].  It has no imports; the
import of @scheme[MDistributeLists] is resolved to the export of
@scheme[MDistributive].  Despite the duplicate definitions of @scheme[add] and
@scheme[mul] in the component modules, there are no name clashes in
@scheme[MDist]; @scheme[addop] and @scheme[mulop] are based on the functions
from @scheme[MDistributive], while @scheme[addall] and @scheme[mulall] are based
on the functions from @scheme[MDistributeLists].

The second version constructs a module with precisely the described imports and
exports.  This may be used to extend the set of imports and restrict the set of
exports.  It is not possible to exclude necessary imports, nor to include
unimplemented exports.

}

@defform[
(restrict name mod-name (import in-ifc ...) (export out-ifc ...))
]{

Defines the module @racket[name] with the same implementation as
@racket[mod-name], but restricting its exports to the listed interfaces
@racket[out-ifc ...] and expanding its imports to the listed interfaces
@racket[in-ifc ...].  This form is named @racket[restrict] because restricting
outputs is generally more useful than expanding inputs.  This form is equivalent
to the following @racket[link] definition:

@racketblock[(link name (import in-ifc ...) (export out-ifc ...) (mod-name))]

}

@section{Programs}

A complete Modular ACL2 program is a sequence of interfaces,
modules (including compound modules), module invocations,
and expressions.

@defform[
(invoke mod-name)
]{

Makes the exported bindings of a
fully-linked module available at the top level of a program.  The
invocation of @scheme[MDist] defines @scheme[addop], @scheme[mulop],
@scheme[addall], and @scheme[mulall].

@schemeblock[
(invoke MDist)
(mulop (addop 1 2) 3)
]

@defform[
(require path ...)
]{
Loads all the definitions from each Modular ACL2 file
specified by a @scheme[path] into the current file.
}

}
