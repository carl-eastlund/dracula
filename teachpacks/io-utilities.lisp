;;===== Module: io-utilities ===================================================
;;
;; Defining
;;   string-list->file        write file
;;   file->string             read file
;;   rat->str                 convert rational number to string
;;   str->rat                 convert string to rational number
;;   loc                      report LoC for specified file
;;
;; (string-list->file file-path list-of-strings state)
;;    Produces effect of writing strings to specified file, in sequence
;;    with newline-characters added at the end of each string
;;    Note: Directory separator in file-path string must be "/", not "\"
;;    Note: "Example functions" section shows how to use this function
;;          See: (wfoo state), (rwfoobar state)
;;    Note: Returns multiple value containing error status and state
;;
;; (file->string file-path state)
;;    delivers (mv string state) where string contains all characters in file
;;    Note: Directory separator in file-path string must be "/", not "\"
;;    Note: "Example functions" section shows how to use this function
;;          See: (rfoo state), (rwfoobar state)
;;    Note: Returns multiple value containing characters from file as
;;          string, error status, and state
;;    Warning! Virtual memory might be exhausted by reading really big files
;;
;; (rat->str r d)
;;     delivers string containing decimal representation of a rational
;;     number, r, with d decimal places digits following the decimal point
;;
;; (str->rat str)
;;    delivers rational number represented by str, a string of decimal digits,
;;    possibly containing a decimal point, and possibly with a leading +/- sign
;;
;; (loc file-path state)
;;    Reports number of lines of code in file specified by file-path (a string)
;;    Also reports state, which is annoying
;;==============================================================================

;;===== Usage Note =============================================================
;; Place this file in the following directory
;;    C:/Program Files/ACL2-2.9.2/sources/books/SE/
;;
;; Use the following command to certify the module
;;   (certify-book "C:/progra~1/ACL2-2.9.2/sources/books/SE/io-utilities")
;;
;; Warning! This book imports list-utilities
;;          Make sure a certified list-utilities book is in the books/SE folder
;;
;; Commands to get access to module functions
;;    Note: set-state-ok and include-book may be embedded in books
;;          :comp read-n-chars may be issued from session only, not from book
;;    (include-book "C:/progra~1/ACL2-2.9.2/sources/books/SE/io-utilities")
;;    (set-state-ok t)
;;    :comp read-n-chars       ; Warning! Issue these :comp commands in
;;    :comp write-all-strings  ;          ACL2 session only, not in book
;;==============================================================================

;;===== Environment setup ======================================================

  (in-package "ACL2")
  (set-compile-fns t)
  (include-book "list-utilities")
  (local (include-book "arithmetic-3/floor-mod/floor-mod" :dir :system))
  (set-state-ok t)
;;==============================================================================

;;===== Function: (dgt->chr dgt) ===============================================
;;
;;  Converts integer in 0-9 range to digit-character
;;
;;  Pre : (member-equal dgt '(0 1 2 3 4 5 6 7 8 9))
;;  Post: (member-equal (dgt->chr dgt) 
;;                      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
;;  Note: defaults to #\0 if dgt is not a one-digit, nonnegative integer
;;==============================================================================
(defun dgt->chr (dgt)
   (cond ((equal dgt 0) #\0)
         ((equal dgt 1) #\1)
         ((equal dgt 2) #\2)
         ((equal dgt 3) #\3)
         ((equal dgt 4) #\4)
         ((equal dgt 5) #\5)
         ((equal dgt 6) #\6)
         ((equal dgt 7) #\7)
         ((equal dgt 8) #\8)
         ((equal dgt 9) #\9)
         (t             #\0)))

;;====== Function: (dgts->chrs dgts) ===========================================
;;
;;  Converts every digit in a list to the corresponding character
;;
;;  Pre : (and (integer-listp dgts)
;;             (implies (and (integerp d) (member d dgts))
;;                      (member d '(0 1 2 3 4 5 6 7 8 9)))
;;  Post: (implies (and (characterp c) (member c (dgts->char dgts)))
;;                 (member c (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))              
;;==============================================================================
(defun dgts->chrs (dgts)
   (if (not (consp dgts))
       nil
       (cons (dgt->chr (car dgts))
             (dgts->chrs (cdr dgts)))))

;;===== Function: (chr->dgt dgt-chr) ===========================================
;;
;;  Converts a digit-character to the corresponding one-digit integer
;;
;;  Pre : (member-equal dgt-chr '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;;  Post: (member (char->dgt dgt-chr) '(0 1 2 3 4 5 6 7 8 9))
;;  Note: defaults to 0 if dgt-chr is not a digit character
;;==============================================================================
(defun chr->dgt (chr)
   (cond ((equal chr #\0) 0)
         ((equal chr #\1) 1)
         ((equal chr #\2) 2)
         ((equal chr #\3) 3)
         ((equal chr #\4) 4)
         ((equal chr #\5) 5)
         ((equal chr #\6) 6)
         ((equal chr #\7) 7)
         ((equal chr #\8) 8)
         ((equal chr #\9) 9)
         (t               0)))

;;====== Function: (chrs->dgts chrs) ===========================================
;;
;;  Converts every digit in a list to the corresponding character
;;
;;  Pre : (and (character-listp chrs)
;;             (implies (and (characterp c) (member c chrs))
;;                      (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;;  Post: (implies (and (integerp d) (member d (chrs->dgts chrs)))
;;                 (member d '(0 1 2 3 4 5 6 7 8 9)))              
;;==============================================================================
(defun chrs->dgts (chrs)
   (if (not (consp chrs))
       nil
       (cons (chr->dgt (car chrs))
             (chrs->dgts (cdr chrs)))))

;;===== Function: (horner s x coefficients) ====================================
;;
;;  Computes s*(x^n) + polynomial in x with given coefficients
;;    where n = (length coefficients)
;;  Note: Coefficient list ordering is high-order to low-order
;;        That is, (nth k coefficients) is coefficient of x^(n - k - 1)
;;        where n = (length coefficients) - 1, k = 0, 1, ... n-1
;;
;;  Pre : (and (numberp s) (numberp x) (number-listp coefficients))
;;  Post: (integerp (horner s x coefficients))
;;==============================================================================
(defun horner (accumulator x coefficients )
  (if (not (consp coefficients))
       accumulator
       (horner (+ (car coefficients) (* x  accumulator))
               x
               (cdr coefficients))))

;;===== Function: (dgts->int dgts) =============================================
;;
;;  Delivers integer whose decimal digits comprise dgts (high-order to low-order)
;;
;;  Pre : (implies (member-equal d dgts)
;;                 (member-equal d '(0 1 2 3 4 5 6 7 8 9)))
;;  Post: (and (integerp (dgts->int dgts))
;;             (>= (dgts->int dgts) 0))
;;==============================================================================
(defun dgts->int (dgts)
  (horner 0 10 dgts))

;;===== Function: (int->append-dgts n dgts) ====================================
;;
;;  Append dgts to digits of decimal representation of n
;;
;;  Pre:  (and (integerp n) (>= n 0)
;;             (implies (member-equal d dgts)
;;                      (member-equal d '(0 1 2 3 4 5 6 7 8 9))))
;;  Post: (equal (dgts->int (int->append-dgts n nil))
;;               n)
;;==============================================================================
(defun int->append-dgts (n dgts)
   (if (or (not (integerp n))
           (not (> n 0)))
       dgts
       (int->append-dgts (floor n 10) (cons (mod n 10) dgts))))

;;===== Function: (int->dgts n) ================================================
;;
;;  Deliver digits of decimal representation of n
;;
;;  Pre:  (and (integerp n) (>= n 0))
;;  Post: (equal (dgts->int (int->dgts n))
;;               n)
;;==============================================================================
(defun int->dgts (n)
  (int->append-dgts n nil))

;;===== Function: (str->int str) ===============================================
;;
;;  Returns integer represented by string
;;
;;  Pre:  (implies (member-equal chr (str->chrs str))
;;                 (member-equal chr '(#\0 #\1 #\2 ... #\9)))
;;  Post: (str->int str) is the integer specifed, in decimal notation, in str
;;==============================================================================
(defun str->int (str)
   (dgts->int (chrs->dgts (str->chrs str))))

;;===== Function: (int->str n) =================================================
;;
;;  Returns string containing decimal representation of n
;;
;;  Pre:  (and (integerp n)
;;             (>= n 0))
;;  Post: (equal (str->int (int->str n))
;;               n)
;;==============================================================================
(defun int->str (n)
   (chrs->str (dgts->chrs (int->dgts n))))

;;===== Function: (str->rat str) ===============================================
;;
;;  Returns number whose decimal representation is str
;;
;;  Pre:  (and (implies (member chr (str->chrs str))
;;                      (or (member chr '(#\0 #\1 #\2 ... #\9))
;;                          (equal chr #\+)
;;                          (equal chr #\-)
;;                          (equal chr #\.))
;;             (not (member #\+ (drop-past #\+ (str->chrs str))))
;;             (not (member #\- (drop-past #\+ (str->chrs str))))
;;             (not (member #\- (drop-past #\- (str->chrs str))))
;;             (not (member #\+ (drop-past #\- (str->chrs str)))))
;;  Post: (and (rationalp (str->rat str))
;;             (= (str->rat str)
;;                (str->rat (rat->str (str->rat str) (length str)))))
;;  Note: (equal (str->rat nil) 0)
;;  Note: Defaults to zero if str does not meet expectations
;;==============================================================================
(defun str->rat (str)
  (if (or (not (stringp str))
          (string-equal str ""))
      0
      (let* ((chrs (str->chrs str))
             (maybe-sign (car chrs))
             (sans-sign (if (member maybe-sign '(#\+ #\-))
                            (cdr chrs)
                            chrs))
             (parts (break-at #\. sans-sign))
             (int-part (car parts))
             (frc-part (cdr (cadr parts)))
             (int-dgts (chrs->dgts int-part))
             (frc-dgts (chrs->dgts frc-part))
             (r-magnitude  (+ (dgts->int int-dgts)
                              (/ (dgts->int frc-dgts)
                                 (expt 10 (length frc-dgts))))))
        (if (char-equal maybe-sign #\-)
            (- r-magnitude)
            r-magnitude))))

;;===== Function: (rat->str r d) ===========================================
;;
;;  Returns string containing decimal representation of r, to d decimal places
;;
;;  Pre:  (and (rationalp r)
;;             (integerp d)
;;             (>= d 0))
;;  Post: (<= (abs(- (str->rat (rat->str r d))
;;                    r))
;;            (/ 5 (expt 10 (+ d 1))))
;;  Note: Defaults to "0" if parameters fail to meet expectations
;;==============================================================================
(defun rat->str (r d)
  (if (or (not (rationalp r))
          (= r 0)
          (not (integerp d))
          (< d 0))
      "0"
  (let* ((r-shifted (round (* (abs r) (expt 10 d)) 1))
         (r-chrs (dgts->chrs (int->dgts r-shifted)))
         (n (length r-chrs))
         (num-dgts-before-decimal-pt (max 0 (- n d)))
         (minus-sign-if-needed (if (< r 0) '(#\-) nil)) 
         (parts (break-at-nth num-dgts-before-decimal-pt r-chrs))
         (int-part (car parts))
         (decimal-pt-if-needed (if (> d 0) '(#\.) nil))
         (frc-part (cadr parts)))
    (chrs->str (append minus-sign-if-needed
                       int-part
                       decimal-pt-if-needed
                       (pad-left d #\0 frc-part))))))

;;===== (read-n-chars n li channel state) ======================================
;;
;;  Inserts up to n characters from channel into li, leaving the new characters
;;  at the beginning of li in the reverse of the order in they were received in
;;
;;  Pre : (and (integerp n)
;;             (>= n 0)
;;             (character-listp li)
;;             (character-input-channelp channel))
;;  Post: Up to n characters have been received from channel
;;        Returns multiple value containing list of chars, channel, and state
;;==============================================================================
(defun read-n-chars (n li channel state)
   (if (or (not (integerp n)) (<= n 0))
       (mv li channel state)
       (mv-let (chr state) (read-char$ channel state)
          (if (null chr)
              (mv li channel state)
              (read-n-chars (- n 1) (cons chr li) channel state)))))

;;===== (write-all-strings strli channel state) ================================
;;
;;  Writes characters from each string in strli to channel
;;  (inserting a newline after the characters comprising each element of strli)
;;
;;  Pre : (and (string-listp strli)
;;             (character-input-channelp channel))
;;  Post: channel has received chars from strings in strli, plus newlines
;;        Returns multiple value containing channel and new state
;;==============================================================================
(defun write-all-strings (strli channel state)
   (if (endp strli)
       (mv channel state)
       (let ((state (princ$ (string-append (car strli)
                                           (coerce '(#\newline) 'STRING)) 
                            channel 
                            state)))
          (write-all-strings (cdr strli) channel state))))

;;===== (file->string file-path state) =========================================
;;
;;  Makes character sequence comprising file accessible as string
;;
;;  Pre : (character-listp file-path)
;;  Post: Characters in file become accessible as string portion
;;          of returned multiple value
;;        Returns multiple value:
;;          (1) string comprising first million characters from file
;;          (2) status (nil indicates success)
;;          (3) state
;;  Warning! System may run out of virtual memory when reading large files
;;  Warning! This function will not read files exceeding about 4GB
;;==============================================================================
(defun file->string (fname state)
   (mv-let (chli error state)
           (mv-let (channel state) (open-input-channel fname :character state)
              (if (null channel)
                  (mv nil
                      (string-append "Error while opening file for input: "
                                     fname)
                      state)
                  (mv-let (chlist chnl state)
                          (read-n-chars 4000000000 '() channel state)
                     (let ((state (close-input-channel chnl state)))
                       (mv chlist nil state)))))
      (mv (reverse (chrs->str chli)) error state)))

;;===== (string-list->file list-of-strings file-path state) ====================
;;
;;  Writes strings to specified file, in sequence, and with
;;  newline characters added at the end of each string
;;
;;  Pre:  (and (string-listp list-of-strings)
;;             (character-listp file-path))
;;  Post: Designated file contains characters from list-of-strings
;;          with newline characters inserted
;;        Returns multiple value:
;;          (1) status (nil indicates success)
;;          (2) state
;;==============================================================================
(defun string-list->file (fname strli state)
  (mv-let (channel state)
          (open-output-channel fname :character state)
     (if (null channel)
         (mv (string-append "Error while opening file for output: " fname)
             state)
         (mv-let (channel state)
                 (write-all-strings strli channel state)
            (let ((state (close-output-channel channel state)))
              (mv nil state))))))

;;===== (loc file-path state) ==================================================
;;
;;  Reports number of lines of code in file specified by file-path (a string)
;;  Also reports state, which is annoying
;;
;; Note: Lines consisting entirely of commentary or whitespace
;;       are not counted as lines of code
;; Warning! Unix-style path names required ("dir/nam", not "dir\nam")
;; WARNING! The input file must use Unix-style line-terminators.
;;          If you created the file using Notepad or Wordpad,
;;          you must convert it to Unix style, which you can do like this:
;;             dos2unix "some-program.lisp"
;;          Of course, you'll need to convet the file back to Windows style
;;          (unix2dos) when you want to edit it
;; Sample usage:
;; First:
;;   Copy file to be LoC-counted to a convenient place and convert to
;;   Unix-style line terminators, eg by using the following DOS commands
;;   copy "c:\Program Files\ACL2-2.8\Sources\Books\SE\io-utilities.lisp" c:\
;;   dos2unix c:\io-utilities.lisp
;; Then, in an ACL2 session:
;;   (include-book "C:/Program Files/ACL2-2.8/sources/books/SE/io-utilities")
;;   (set-state-ok t)
;;   :comp read-n-chars ; Warning! This cmd is for sessions only, not books
;;   (loc "c:/io-utilities.lisp" state)
;; Response in ACL2 session:
;;   (177 <state>)

(defun num-noncomments (lines count)
   (if (not (consp lines))
       count
       (let* ((whitespace '(#\Space #\Newline #\Tab))
              (leading-white-stripped (drop-set whitespace (car lines))))
         (if (and (consp leading-white-stripped)
                  (not (char-equal #\; (car leading-white-stripped))))
             (num-noncomments (cdr lines) (+ count 1))
             (num-noncomments (cdr lines) count)))))
(defun loc-from-file-as-string (str)
  (num-noncomments (packets #\Newline (str->chrs str)) 0))
(defun loc (file-path state)
  (mv-let (str error state) (file->string file-path state)
     (if error
         (mv error state)
         (mv (loc-from-file-as-string str) state))))

;;===== Example functions using file i/o =======================================
;;
;;  Warning!
;;    The variable "state"
;;      (1) cannot have any other name
;;      (2) can be used only in certain contexts
;;
;;  Note: You must issue the command
;;           (set-state-ok t)
;;        before invoking any function that refers to the variable state
;;
;;  Note: Directory separator in file-path string is "/", not "\"

;; (wfoo state)   ; This formula writes the file "C:/foo.wpd"
;;                  Well...actually...it's "C:\foo.wpd"
;;                  but ACL2 uses Unix conventions in path names

(defun wfoo (state)
  (mv-let (error state)
          (string-list->file "C:/foo.wpd"
                             (list "this is the first line"
                                   "this is the second line")
                             state)
     (if error
         (mv error state)
         (mv "Write C:/foo.wpd succeeded" state))))

;; (rfoo state)   ; This formula reads the file "C:/foo.wpd"
;;                  Well...actually...it's "C:\foo.wpd" 
;;                  but ACL2 uses Unix conventions in path names

(defun formula-for-rfoo-result (str)
  (string-append "input file as string follows: " str))

(defun rfoo (state)
  (mv-let (str error state) (file->string "C:/foo.wpd" state)
     (if error
         (mv error state)
         (mv (formula-for-rfoo-result str) state))))

;; (rwfoobar state)  ; This formula reads the file "C:/foo.wpd"
;;                     then writes the file "C:/bar.wpd"
;;                     Well...actually...it's "C:\foo.wpd" and "C:\bar.wpd"
;;                     but ACL2 uses Unix conventions in path names

(defun rwfoobar (state)
  (mv-let (input-as-string error-open state) (file->string "C:/foo.wpd" state)
     (if error-open
         (mv error-open state)
         (mv-let (error-close state)
                 (string-list->file "C:/bar.wpd"
                                    (list "c:/foo.wpd follows:"
                                          input-as-string)
                                    state)
            (if error-close
                (mv error-close state)
                (mv "Success: read c:/foo.wpd, wrote c:/bar.wpd" state))))))

;; (map-chrs->str list-list-chrs)  ; This formula applies chrs->str to
;;                                 ;   each element in a list in which each
;;                                 ;   element is a list of characters
;;                                 ; Thus, a list of lists of characters
;;                                 ;   becomes a list of strings

(defun map-chrs->str (list-list-chrs)
  (if (consp list-list-chrs)
      (cons (chrs->str (car list-list-chrs))
            (map-chrs->str (cdr list-list-chrs)))
      nil))

;; (your-computation str)  ; This formula converts a string to a list strings
;;                         ;   each of which is a substring of str filling in
;;                         ;   the gap between two newline-characters
;;                         ; In a real program, this function would do a
;;                         ;   more elaborate computation, to carry out
;;                         ;   whatever transformation you intended your
;;                         ;   program to do, and then it would represent the
;;                         ;   results of th comptutation as a list of strings

(defun your-computation (str)
  (map-chrs->str (packets #\Newline (str->chrs str))))
  
;; (ipo f-in f-out state)  ; This formula uses the function file->string reads
;;                         ;   the file f-in, producing a string containing all
;;                         ;   the characters in the file
;;                         ; Then it invokes the function "your-computation"
;;                         ;   to transform the string to a list of strings
;;                         ; Finally, it writes the list of strings delivered
;;                         ;   by your-computation to a file named f-out,
;;                         ;   one string per line
;;                         ; USING THIS INPUT/PROCESS/OUTPUT TEMPLATE
;;                             Copy the defun for ipo,
;;                         ;   change its name to something appropriate,
;;                         ;   define a new function to do the computation
;;                         ;   you are working on (note: your the input to
;;                         ;   your function will be a string consisting
;;                         ;   of all the characters from your input file,
;;                         ;   and the output must be a list of strings,
;;                         ;   one string for each line you want in your
;;                         ;   output file), and, finally,
;;                         ;   replace the invocation of "your-computation"
;;                         ;   by an invoction of the function you defined
;;                         ; Warning! The input file f-in must be a text-file
;;                         ;   with Unix-style line separators
;;                         ;   (You can convert DOS-style files to Unix-style
;;                         ;   with the dos2unix program.)
;;                         ; Warning! The strings f-in and f-out must be
;;                         ;   Unix-style file-path specs, with forward slashes
;;                         ;   not Windows-style back slashes
;;                         ;   For example, "C:/yourfolder/yourfile.wpd",
;;                         ;            not "C:\yourfolder\yourfile.wpd"
;;                         ; Warning! The third parameter of ipo cannot be
;;                         ;   any symbol other than the symbol whose name
;;                         ;   is "state"
;;                         ; Warning! Before invoking this function from an
;;                         ;   ACL2 session, you must issue these commands:
;;                         ;   (set-state-ok t)
;;                         ;   :comp read-n-chars
;;                         ;   :comp write-all-strings

(defun ipo (f-in f-out state)
  (mv-let (input-as-string error-open state) (file->string f-in state)
     (if error-open
         (mv error-open state)
         (mv-let (error-close state)
                 (string-list->file f-out
                                    (your-computation input-as-string)
                                    state)
            (if error-close
                (mv error-close state)
                (mv (string-append "Read "
                     (string-append f-in
                      (string-append ", compute, then write " f-out)))
                    state))))))
;;===== end of example functions ===============================================

