#lang mischief

(provide
  writable-proof-obligations
  module-path->proof-obligation
  write-program-obligations-to-file
  write-certification-script
  all-modules-with-proofs
  proof-dependencies)

(require
  dracula/proof/term
  dracula/model
  dracula/expansion/paths)

(define (writable-proof-obligations)
  (for/hash
      {[{mod pf} (in-dict (all-proof-obligations))]
       #:when (path? mod)}
    (values mod pf)))

(define (module-path->proof-obligation mod-path)
  (define name
    (resolved-module-path-name
      (resolve-module-path mod-path
        #:load? #false)))
  (lookup-proof-obligation name))

(define (write-program-obligations-to-file file pf)
  (define book (book-path file))
  (make-directory* (path-only book))
  (with-output-to-file book #:exists 'replace
    (thunk
      (write-acl2 '(#s(:: ACL2 IN-PACKAGE) "DRACULA"))
      (for {[term (in-list pf)]}
        (printf "\n")
        (write-acl2 term)))))

(define (write-certification-script . mod-paths)
  (for-each write-acl2 portcullis-events)
  (for-each write-acl2 portcullis-non-events)
  (write-acl2 '(RESET-PREHISTORY))
  (for {[mod (in-list mod-paths)]}
    (write-acl2
      `(CERTIFY-BOOK ,(book-path-without-extension mod)
         ,(add1 (length portcullis-events))
         #s(:: ACL2 T)
         #:SKIP-PROOFS-OKP
         #s(:: ACL2 T)))
    (write-acl2 '(UBU! 0))))
