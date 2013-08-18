#lang racket

(provide
  file->string
  string-list->file)

(define (file->string fname state)
  (with-handlers ([exn:fail:filesystem? 
                   (lambda (e) 
                     (list '() 
                       (string-append "Error while opening file for input: " fname)
                       state))])
    (with-input-from-file fname
      (lambda ()
        (let ([str (make-string (file-size fname))])
          (read-string! str)
          (list str '() state))))))

(define (string-list->string strlist)
  (if (null? strlist) 
    ""
    (string-append (car strlist) "\n" (string-list->string (cdr strlist)))))

(define (string-list->file fname strlist state)
  (with-handlers ([exn:fail:filesystem?
                   (lambda (e) 
                     (list (string-append "Error while opening file for output: " fname) 
                       state))])
    (with-output-to-file fname
      (lambda ()
        (write-string (string-list->string strlist))
        (list '() state)))))

