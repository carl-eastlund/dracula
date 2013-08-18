(module reader syntax/module-reader
  #:language acl2-module-v
  #:read acl2-read
  #:read-syntax acl2-read-syntax
  #:wrapper1 read-all/module-begin
  #:whole-body-readers? #t

  (require "acl2-module-v.rkt"
           racket/list racket/match
           (prefix-in acl2- "acl2-reader.rkt"))

  (define (read-all/module-begin reader)
    (list (cons '#%module-begin (read-all reader))))

  (define (read-all reader)
    (let* ([term (reader)])
      (if (eof-object? term) null (cons term (read-all reader))))))
