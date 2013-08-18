#lang racket

(require "../lang/dracula.rkt" "../lang/check.rkt")

(provide (all-defined-out))

(begin-below

 ;; Reimplemented using MzScheme primitives
 ;; to make Ex3 run faster.

 (defconst *max-file-size* 4000000000)

 (defun binary-file->byte-list (fname state)
   (with-handlers ([exn:fail:filesystem? 
                    (lambda (e)
                      (list '()
                            (format "Error while opening file for input: ~a"
                                    fname)
                            state))])
     (let ([buffer (make-bytes (file-size fname))])
       (list (bytes->list
              (with-input-from-file fname
                (lambda () (read-bytes! buffer) buffer)))
             '()
             state))))

 (defun byte-list->binary-file (fname byte-list state)
   (with-handlers ([exn:fail:filesystem?
                    (lambda (e)
                      (list
                       (string-append
                        "Error while opening file for output: "
                        fname)
                       state))])
     (with-output-to-file fname
       (lambda () (write-bytes (list->bytes (map inexact->exact byte-list)))))
     (list '() state)))

 ;;===== Unit test framework ====================================================
 ;;==============================================================================
 #|  
 ;; Implementation note
 ;;   ACL2 has special display rules for multiple-values of
 ;;   multiplicity three when the last component is state.
 ;;   To avoid these rules, state is the first component
 ;;   in the multiple-value delivered by r-bmp
 (defun r-bmp (state)
 (mv-let (input-bytes error-open state)
 (binary-file->byte-list "C:/temp/imgIn.bmp" state)
 (if error-open
 (mv state error-open nil)
 (mv state
 (string-append "Success: read c:/imgIn.bmp"
 (coerce '(#\newline) 'STRING))
 input-bytes))))

 (defun rw-bmp (state)
 (mv-let (input-bytes error-open state)
 (binary-file->byte-list "C:/temp/imgIn.bmp" state)
 (if error-open
 (mv error-open state)
 (mv-let (error-close state)
 (byte-list->binary-file "C:/temp/imgOut.bmp"
 input-bytes
 state)
 (if error-close
 (mv error-close state)
 (mv nil state))))))

 (defun zap-bit (byte)
 (logand #b11111110 (char-code (code-char byte))))

 ;; Warning! To avoid stack overflow, issue the command
 ;;            :comp zap-bits
 ;;          after importing these functions
 ;; Implementation note
 ;;   The function zap-bits is tail recursive, and this
 ;;   is a crucial factor in applying it to long lists of bytes
 ;;   It would fail on lists longer than about 30,000 bytes
 ;;   if it were not tail recursive.
 (defun zap-bits (skip bytes zapped)
 (if (endp bytes)
 (reverse zapped)
 (if (zp skip)
 (zap-bits 0 (cdr bytes) (cons (zap-bit (car bytes)) zapped))
 (zap-bits (- skip 1) (cdr bytes) (cons (car bytes) zapped)))))

 ;; (zap-img bmp-in bmp-out state)
 ;; writes a copy of the BMP image file at
 ;; the path specified in the string bmp-in
 ;; to the path specified in the string bmp-out,
 ;; but with all low order bits in bytes beyond 
 ;; the 1000th byte set to zero
 ;; Warning! To avoid stack overflow, issue the command
 ;;            :comp zap-bits
 ;;          after importing these functions
 (defun zap-img (img-in img-out state)
 (mv-let (input-bytes error-open state)
 (binary-file->byte-list img-in state)
 (if error-open
 (mv error-open state)
 (mv-let (error-close state)
 (byte-list->binary-file img-out
 (zap-bits 1000 input-bytes nil)
 state)
 (if error-close
 (mv error-close state)
 (mv nil state))))))
 ;;==============================================================================
 |#

 )