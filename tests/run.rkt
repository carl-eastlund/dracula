#lang mischief

(module+ main
  (require racket/cmdline)
  (define use-gui? #false)
  (command-line
    #:once-any
    ["--gui"
     "Use the RackUnit GUI."
     (set! use-gui? #true)]
    [{"--no-gui" "--text"}
     "Use the RackUnit textual interface (default)."
     (set! use-gui? #false)]
    #:args {}
    (if use-gui?
      ((dynamic-require 'rackunit/gui 'test/gui) #:wait? #true dracula-tests)
      (exit ((dynamic-require 'rackunit/text-ui 'run-tests) dracula-tests)))))

(module+ test
  (require rackunit/text-ui)
  (run-tests dracula-tests))

(require
  rackunit
  dracula/tests/suite/atomic
  dracula/tests/suite/modular
  dracula/tests/suite/component
  dracula/tests/suite/macro
  dracula/tests/suite/surface)

(define dracula-tests
  (test-suite "Dracula"
    surface-tests
    #|macro-tests|#
    component-tests
    modular-tests
    atomic-tests))
