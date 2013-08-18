#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)
                     (this-package-in teachpacks/io-utilities)))

@title[(scheme "io-utilities")]

@(declare-exporting/this-package [teachpacks/io-utilities] [])

@specform[(include-book "io-utilities" :dir :teachpacks)]

Documentation under construction.

@verbatim{
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

;;===== Function: (dgt->chr dgt) ===============================================
;;
;;  Converts integer in 0-9 range to digit-character
;;
;;  Pre : (member-equal dgt '(0 1 2 3 4 5 6 7 8 9))
;;  Post: (member-equal (dgt->chr dgt) 
;;                      '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
;;  Note: defaults to #\0 if dgt is not a one-digit, nonnegative integer
;;==============================================================================

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

;;===== Function: (chr->dgt dgt-chr) ===========================================
;;
;;  Converts a digit-character to the corresponding one-digit integer
;;
;;  Pre : (member-equal dgt-chr '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
;;  Post: (member (char->dgt dgt-chr) '(0 1 2 3 4 5 6 7 8 9))
;;  Note: defaults to 0 if dgt-chr is not a digit character
;;==============================================================================

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

;;===== Function: (dgts->int dgts) =============================================
;;
;;  Delivers integer whose decimal digits comprise dgts (high-order to low-order)
;;
;;  Pre : (implies (member-equal d dgts)
;;                 (member-equal d '(0 1 2 3 4 5 6 7 8 9)))
;;  Post: (and (integerp (dgts->int dgts))
;;             (>= (dgts->int dgts) 0))
;;==============================================================================

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

;;===== Function: (int->dgts n) ================================================
;;
;;  Deliver digits of decimal representation of n
;;
;;  Pre:  (and (integerp n) (>= n 0))
;;  Post: (equal (dgts->int (int->dgts n))
;;               n)
;;==============================================================================

;;===== Function: (str->int str) ===============================================
;;
;;  Returns integer represented by string
;;
;;  Pre:  (implies (member-equal chr (str->chrs str))
;;                 (member-equal chr '(#\0 #\1 #\2 ... #\9)))
;;  Post: (str->int str) is the integer specifed, in decimal notation, in str
;;==============================================================================

;;===== Function: (int->str n) =================================================
;;
;;  Returns string containing decimal representation of n
;;
;;  Pre:  (and (integerp n)
;;             (>= n 0))
;;  Post: (equal (str->int (int->str n))
;;               n)
;;==============================================================================

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

}
