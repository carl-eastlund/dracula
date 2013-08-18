;;===== Usage Note =============================================================
;; Place this file in the following directory
;;    C:/Program Files/ACL2-2.9.2/sources/books/SE/
;;
;; Use the following command to certify the module
;; (certify-book "C:/progra~1/ACL2-2.9.2/sources/books/SE/binary-io-utilities")
;;
;; Commands to get access to module functions
;;    Note: set-state-ok and include-book may be embedded in books
;;          ":comp" commands may be issued from session only, not from book
;;    (include-book "C:/progra~1/ACL2-2.9.2/sources/books/SE/binary-io-utilities")
;;    (set-state-ok t)
;;    :comp read-n-bytes ; Warning! Issue this cmd in session only, not in book
;;    :comp binary-write ; Warning! Issue this cmd in session only, not in book
;;
;; Limitations
;;   binary-file->byte-list is designed to read files up to 4GB,
;;   but may trigger problems on some systems, even on smaller files.
;;   Testing, so far, has succeeded on 2MB files, but failed on 25MB files.
;;==============================================================================

;;===== Environment setup ======================================================

  (in-package "ACL2")
  (set-compile-fns t)
  (set-state-ok t)
;;==============================================================================


(defun read-n-bytes (n bytes-already-in channel state)
   (if (or (not (integerp n)) (<= n 0))
       (mv bytes-already-in channel state)
       (mv-let (byte state) (read-byte$ channel state)
          (if (null byte)
              (mv bytes-already-in channel state)
              (read-n-bytes (- n 1)
                            (cons byte bytes-already-in) channel state)))))

(defconst *max-file-size* 4000000000) ;;limits input file to 4GB
(defun binary-file->byte-list (fname state)
   (mv-let (byte-list error state)
           (mv-let (channel state) (open-input-channel fname :byte state)
              (if (null channel)
                  (mv nil
                      (string-append "Error while opening file for input: "
                                     fname)
                      state)
                  (mv-let (byte-list chnl state)
                          (read-n-bytes *max-file-size* '() channel state)
                     (let ((state (close-input-channel chnl state)))
                       (mv byte-list nil state)))))
      (mv (reverse byte-list) error state)))

(defun binary-write (byte-list channel state)
  (if (atom byte-list)
      (mv channel state)
      (let ((state (write-byte$ (car byte-list) channel state)))
         (binary-write (cdr byte-list) channel state))))

(defun byte-list->binary-file (fname byte-list state)
  (mv-let (channel state)
          (open-output-channel fname :byte state)
     (if (null channel)
         (mv (string-append "Error while opening file for output: " fname)
             state)
         (mv-let (channel state)
                 (binary-write byte-list channel state)
            (let ((state (close-output-channel channel state)))
              (mv nil state))))))

;;===== Unit test framework ====================================================
;;==============================================================================

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

