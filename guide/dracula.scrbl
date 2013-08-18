#lang scribble/doc
@(require scribble/manual planet/util)

@(define maj (number->string (this-package-version-maj)))
@(define min (number->string (this-package-version-min)))

@title[#:tag "install"]{Getting Started}

This section provides instructions for installing, uninstalling, and upgrading
Dracula.

@subsubsub*section{Prerequisites}

For the latest release of Dracula, you need DrRacket version 5.0 or above, or
DrScheme version 4.0.1 or above. Download and install from
@link["http://www.racket-lang.org/"]{the Racket web site}.  Dracula also
requires ACL2 version 3.1+. Download and install from
@link["http://www.cs.utexas.edu/users/moore/acl2/"]{the ACL2 site at UT,
Austin}.

@subsubsub*section{Installing Dracula}
  
Dracula is available as a
@link["http://planet.plt-scheme.org/display.ss?package=dracula.plt&owner=cce"]{Planet
package}.  It can be installed using the @schemefont{planet} command line
utility. On Windows, this utility is in the directory where Racket is
installed; on Mac or Unix, it is in the @schemefont{bin} subdirectory. To
install Dracula using this utility, execute:

@exec{planet install cce dracula.plt @|maj| @|min|}

@subsubsub*section{Uninstalling Dracula}

To uninstall Dracula, run the following at the command line:

@exec{raco planet remove cce dracula.plt @|maj| @|min|}

For other versions of Dracula, change the major and minor version numbers.

@subsubsub*section{Upgrading Dracula}

To upgrade, remove the old Dracula as shown here. Then follow the installation
instructions above.

@subsubsub*section{Running Dracula programs as scripts}

To run a Dracula program, e.g. "@filepath{program.lisp}". from the command line,
add the following line to the top of the main file:

@schememod[(code:line planet cce/dracula)]

Then run:

@exec{racket program.lisp}
