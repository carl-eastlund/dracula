#lang mischief

(provide
  book-path
  book-path-without-extension
  certificate-path)

(require syntax/modcode)

(define (book-path-without-extension path)
  (path->string (path-remove-extension (book-path path))))

(define (book-path path)
  (metadata-path path ".lisp" certificate-subdir))

(define (certificate-path path)
  (metadata-path path ".cert" certificate-subdir))

(define certificate-subdir "compiled")

(define (metadata-path path extension . subs)
  (define-values {base name dir?} (split-path path))
  (path->string
    (apply get-metadata-path base
      (append subs (list (path-add-suffix name extension))))))

(define (path-remove-extension path)
  (path-replace-suffix path ""))
