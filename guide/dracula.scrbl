#lang scribble/doc
@(require
   scribble/manual
   racket/require
   (path-up "self/scribble.rkt"))

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
  
Dracula is available as a @link["http://pkg.racket-lang.org/#[dracula]"]{Racket
package}.  It can be installed using the @schemefont{raco} command line
utility. On Windows, this utility is in the directory where Racket is
installed; on Mac or Unix, it is in the @schemefont{bin} subdirectory. To
install Dracula using this utility, execute:

@exec{raco pkg install dracula}

@subsubsub*section{Uninstalling Dracula}

To uninstall Dracula, run the following at the command line:

@exec{raco pkg remove dracula}

@subsubsub*section{Upgrading Dracula}

To upgrade Dracula, run the following at the command line:

@exec{raco pkg update dracula}

@subsubsub*section{Running Dracula programs as scripts}

To run a Dracula program, e.g. "@filepath{program.lisp}" from the command line,
add the following line to the top of the main file:

@racketmod/dracula[main]

Then run:

@exec{racket program.lisp}
