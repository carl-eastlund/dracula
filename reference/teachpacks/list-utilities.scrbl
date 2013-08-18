#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)
                     (this-package-in teachpacks/list-utilities)))

@title[(scheme "list-utilities")]

@(declare-exporting/this-package [teachpacks/list-utilities] [])

@specform[(include-book "list-utilities" :dir :teachpacks)]

Documentation under construction.

@verbatim{
;;===== Module: list-utilities =================================================
;;
;; Defining
;;   (break-at-nth n xs)
;;   (break-at-set delimiters xs)
;;   (break-at delimiter xs)
;;   (take-to-set delimiters xs)
;;   (take-to delimiter xs)
;;   (drop-set delimiters xs)
;;   (drop-past delimiter xs)
;;     = (drop-thru delimiter xs)
;;   (drop-past-n-delimiters n delimiter xs)
;;     = (drop-thru-n-delimiters n delimiter xs)
;;   (packets-set delimiters xs)
;;   (packets delimiter xs)
;;   (tokens delimiters xs)
;;   (replicate n x)
;;   (pad-left width pad xs)
;;   (chrs->str character-list)
;;   (str->chrs string)
;;   (words str)
;;==============================================================================

;;====== Function: (break-at-nth n xs) =========================================
;;
;;  Returns list with two elements
;;    (1) the first n elements of xs
;;    (2) xs without its first n elements
;;
;;  Pre : (and (integerp n)
;;             (>= n 0)
;;             (true-listp xs))
;;  Post: (and (= (min n (length xs))
;;                (length (car (break-at-nth n xs))))
;;             (equal (append (car  (break-at-nth n xs))
;;                            (cadr (break-at-nth n xs)))
;;                    xs))
;;==============================================================================

;;====== Function: (break-at-set delimiters xs) ================================
;;
;;  Returns list with two elements
;;    (1) the portion of xs that precedes its first element that is a
;;        member of the list delimiters
;;    (2) the portion of xs beginning with its first element that is a
;;        member of the list delimiters
;;
;;  Pre : (and (true-listp delimiters)
;;             (true-listp xs))
;;  Post: (and (equal (append (car (break-at-set ds xs))
;;                            (cadr (break-at-set ds xs)))
;;                    xs)
;;             (implies (consp (cadr (break-at-set ds xs)))
;;                      (member-equal (caadr (break-at-set ds xs)) ds)))
;;             (implies (member-equal d ds)
;;                      (not (member-equal d (car (break-at-set ds xs))))))
;;==============================================================================

;;====== Function: (break-at x xs) =============================================
;;
;;  Returns list with two elements
;;    (1) the portion of xs that precedes the first element equal to x
;;    (2) the portion of xs beginning with the first element equal to x
;;
;;  Pre : (true-listp xs)
;;  Post: (and (equal (append (car (break-at x xs)) (cadr (break-at x xs)))
;;                    xs)
;;             (implies (consp (cadr (break-at d xs)))
;;                      (member-equal (caadr (break-at-set d xs)) ds)))
;;             (implies (member-equal d ds)
;;                      (not (member-equal d (car (break-at d xs))))))
;;==============================================================================

;;====== Function: (take-to-set delimiters xs) =================================
;;
;;  Returns the part of xs that precedes its first element that is a member
;;  of the list delimiters
;;
;;  Pre : (and (true-listp delimiters)
;;             (true-listp xs))
;;  Post: (and (true-listp (take-to-set delimiters xs))
;;             (implies (member-equal d delimiters)
;;                      (not (member-equal d (take-to-set delimiters xs))))
;;             (implies (and (equal (append (take-to-set delimiters xs)
;;                                          ys)
;;                                  xs)
;;                           (consp ys))
;;                      (member-equal (car ys) delimiters)))
;;==============================================================================

;;====== Function: (take-to x xs) ==============================================
;;
;;  Returns the part of xs that precedes the first element equal to x
;;
;;  Pre : (true-listp xs)
;;  Post: (and (true-listp (take-to x xs))
;;             (not (member-equal x (take-to x xs)))
;;             (implies (member-equal x xs)
;;                      (equal (append (take-to x xs)
;;                                     (list x)
;;                                     (drop-past x xs))))
;;             (implies (not (member-equal x xs))
;;                      (equal (take-to x xs) xs)))
;;==============================================================================

;;===== Function: (drop-set delimiters xs) =====================================
;;
;;  Returns the part of xs that follows the maximal contiguous block of
;;  delimiters beginning at the first element of xs
;;
;;  Pre:  (true-listp xs)
;;  Post: (and (true-listp (drop-set delimiters xs))
;;             (implies (consp (drop-set delimiters xs))
;;                      (not (member (car (drop-set delimiters xs))
;;                                   delimiters)))
;;             (implies (and (equal (append ds (drop-set delimiters xs))
;;                                  xs)
;;                           (member d ds))
;;                      (member d delimiters)))
;;==============================================================================

;;===== Function: (drop-past delimiter xs) =====================================
;;
;;  Returns the part of xs that follows the first element equal to x
;;
;;  Pre:  (true-listp xs)
;;  Post: (and (true-listp (drop-past delimiter xs))
;;             (implies (member-equal x xs)
;;                      (equal (append (take-to delimiter xs)
;;                                     (list x)
;;                                     (drop-past delimiter xs))))
;;             (implies (not (member-equal delimiter xs))
;;                      (equal (drop-past delimiter xs) nil)))
;;==============================================================================

;;====== Function: (drop-past-n-delimiters n delimiter xs) =====================
;;
;;  Returns the part of xs that follows the first n occurances of x
;;
;;  Pre : (and (integerp n) (>= n 0) (true-listp xs))
;;  Post: (true-listp (drop-past-n-delimiters n d xs)) ; and some other stuff
;;==============================================================================

;;====== Function: (packets-set delimiters xs) ==================================
;;
;;  Parcels a list into a list of lists
;;  Contiguous sequences from a specified set of delimiters
;;  marks separations between packets
;;
;;  Pre : (and (true-listp delimiters) (true-listp xs))
;;  Post: (true-list-listp (packets-set ds xs))
;;        and a bunch of other stuff
;;==============================================================================


;;====== Function: (packets delimiter xs) ======================================
;;
;;  Parcels a list into a list of lists
;;  A specified delimiter marks separations between packets in the given list
;;
;;  Pre : (true-listp xs)
;;  Post: (and (true-list-listp (packets d xs))
;;             (implies (and (integerp n) (>= n 0))
;;                      (equal (nth n (packets d xs))
;;                             (take-to d (drop-past-n-delimiters n d xs)))))
;;==============================================================================

;;====== Function: (tokens delimiters xs) ======================================
;;
;;  Parcels a list into a list of tokens
;;  Tokens are the sublists residing between maximally contiguous
;;  sequences of delimiters
;;
;;  Pre : (and (true-listp delimiters) (true-listp xs))
;;  Post: (true-list-listp (tokens ds xs))
;;        and a bunch of other stuff
;;==============================================================================

;;===== Function: (rep-append n x xs) ==========================================
;;
;;  Appends xs to a list consisting of n copies of x
;;
;;  Pre : (and (integerp n)
;;             (>= n 0)
;;             (true-listp xs))
;;  Post: (and (implies (member-equal y (take n (rep-append n x)))
;;                      (equal y x))
;;             (equal (length (rep-append n x xs))
;;                    (+ (length xs) n))
;;             (equal (nthcdr n (rep-append n x xs))
;;                    xs))
;;==============================================================================

;;===== Function: (replicate n x) =================================================
;;
;;  Delivers a list consisting of n copies of x
;;
;;  Pre : (and (integerp n)
;;             (>= n 0))
;;  Post: (and (implies (member-equal y (replicate n x))
;;                      (equal y x))
;;             (equal (length (replicate n x)) n))
;;==============================================================================

;;===== Function: (pad-left w p xs) ============================================
;;
;;  Pads appends xs to copies of p to make the resulting list have w elements
;;  Note: Delivers xs, as is, if xs has w or more elements
;;
;;  Pre : (and (integerp w)
;;             (>= w 0)
;;             (listp xs))
;;  Post: (and (equal (length (pad-left w p xs))
;;                    (max w (length xs)))
;;             (implies (member-equal x (take (max 0 (- w (length xs))) xs))
;;                      (equal x p)))
;;==============================================================================

;;====== Function: (chrs->str chrs) ============================================
;;
;;  Converts list of characters to string
;;
;;  Pre : (character-listp chrs)
;;  Post: (stringp (chrs->str chrs))              
;;==============================================================================

;;====== Function: (str->chrs str) =============================================
;;
;;  Converts string to list of characters
;;
;;  Pre : (stringp str)
;;  Post: (character-listp (str->chrs str))
;;==============================================================================

;;====== Function: (words str) =================================================
;;
;;  Parcels a string into a list of words
;;  Words are the sublists residing between maximally contiguous
;;  spans of whitespace
;;
;;  Pre : (stringp str)
;;  Post: (string-listp (words str))
;;        and a bunch of other stuff
;;==============================================================================

}