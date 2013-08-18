#lang racket

(require "../private/planet.rkt"
         "../private/regexp.rkt")
(require rackunit)

(define-syntax (test-regexp stx)
  (syntax-case stx ()
    [(_ pattern string result)
     (syntax/loc stx
       (test-suite (format "(regexp-match ~s ~s) = ~s" 'pattern 'string 'result)
         (test-case "regexp"
           (check-equal? (regexp-match (regexp pattern) string) result))
         (test-case "pregexp"
           (check-equal? (regexp-match (pregexp pattern) string) result))))]))

(define regexp-test-suite
 (test-suite "Regular Expressions"
   (test-suite "regexp-sequence"
     (test-regexp (regexp-sequence) "a cat" (list ""))
     (test-regexp (regexp-sequence "cat") "a cat" (list "cat"))
     (test-regexp (regexp-sequence "hot" "dog") "a hotdog" (list "hotdog"))
     (test-regexp (regexp-sequence "cat" "dog") "a cat" #f)
     (test-regexp (regexp-sequence "cat" "dog") "a dog" #f)
     (test-regexp (regexp-sequence "a" "b|c") "c" #f))
   (test-suite "regexp-or"
     (test-regexp (regexp-or "cat") "a cat" (list "cat"))
     (test-regexp (regexp-or "cat" "dog") "a cat" (list "cat"))
     (test-regexp (regexp-or "cat" "dog") "a dog" (list "dog")))
   (test-suite "regexp-maybe"
     (test-regexp (regexp-maybe "cat") "a dog" (list ""))
     (test-regexp (regexp-maybe "cat") "catnap" (list "cat"))
     (test-regexp (regexp-maybe "hot" "dog") "hotdog!" (list "hotdog"))
     (test-regexp (regexp-maybe "hot" "dog") "a dog" (list "")))
   (test-suite "regexp-star"
     (test-regexp (regexp-star "a") "" (list ""))
     (test-regexp (regexp-star "a") "aaa" (list "aaa"))
     (test-regexp (regexp-star "ab") "abab" (list "abab"))
     (test-regexp (regexp-star "a" "b") "abab" (list "abab"))
     (test-regexp (regexp-star "a" "b") "aaaa" (list "")))
   (test-suite "regexp-plus"
     (test-regexp (regexp-plus "a") "" #f)
     (test-regexp (regexp-plus "a") "aaa" (list "aaa"))
     (test-regexp (regexp-plus "ab") "abab" (list "abab"))
     (test-regexp (regexp-plus "a" "b") "abab" (list "abab"))
     (test-regexp (regexp-plus "a" "b") "aaaa" #f))
   (test-suite "regexp-multi"
     (test-regexp (regexp-multi "^cat$") "ant\nbat\ncat\ndog" (list "cat")))
   (test-suite "regexp-save"
     (test-regexp (regexp-save "cat") "a cat" (list "cat" "cat")))))

(provide regexp-test-suite)
