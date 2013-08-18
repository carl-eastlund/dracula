#lang scheme

(require "checks.ss"
         planet/util)

(provide planet-suite)

(define planet-suite
  (test-suite "planet.ss"
    (test-suite "this-package-version-symbol"
      (test-case "here"
        (check-equal? (this-package-version-symbol)
                      (string->symbol
                       (format "~a/~a:~a:~a"
                               (this-package-version-owner)
                               (regexp-replace "\\.plt$"
                                               (this-package-version-name)
                                               "")
                               (this-package-version-maj)
                               (this-package-version-min)))))
      (test-case "here/there"
        (check-equal? (this-package-version-symbol there)
                      (string->symbol
                       (format "~a/~a:~a:~a/there"
                               (this-package-version-owner)
                               (regexp-replace "\\.plt$"
                                               (this-package-version-name)
                                               "")
                               (this-package-version-maj)
                               (this-package-version-min))))))))
