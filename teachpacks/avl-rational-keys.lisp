;;=======  Module: AVL trees   ================================================
;; Defining
;;   (avl-retrieve tr key)
;;   (empty-tree)
;;   (avl-insert tr key datum)
;;   (avl-delete tr key)
;;   (avl-flatten tr)
;;=============================================================================
;; Module usage notes
;;
;; Place this file in the following directory
;;    C:/ACL2-3.1/acl2-sources/books/SE/
;;
;;  Use the following command to certify the module
;;  (certify-book "C:/ACL2-3.1/acl2-sources/books/SE/avl-rational-keys")
;;
;; Command to get access to module functions (any session or program)
;;  (include-book "SE/avl-rational-keys" :dir :system)
;;
;;=============================================================================
;; Function usage notes
;;
;; 1. (avl-retrieve tr key)
;;   assumes
;;     tr         has been constructed by one of the AVL-tree constructors
;;                (empty-tree, avl-insert, and avl-delete)
;;     new-key    is a rational number
;; 
;;   delivers
;;     either a two element list (k d)
;;       such that k equals key and
;;                 tr contains a subtree with k and d in its root
;;     or nil, in case key does not occur in tr
;;   
;; 2. (empty-tree)
;;   delivers an empty AVL-tree
;;
;; 3. (avl-insert tr key datum)
;;   assumes
;;      tr       has been constructed by the AVL-tree constructors
;;               (empty-tree, avl-insert, or avl-delete)
;;      key      is a rational number
;;   delivers an AVL tree with the following property
;;     (and (equal (avl-retrieve (avl-insert tr key datum) key)
;;                 (list key datum))
;;          (iff (avl-retrieve (avl-insert tr key datum) k)
;;               (or (avl-retrieve tr k)
;;                   (key= k key))))
;;
;; 4. (avl-delete tr key)
;;   assumes
;;      tr       has been constructed by the AVL-tree constructors
;;               (empty-tree, avl-insert, and avl-delete)
;;      key      is a rational number
;;   delivers an AVL tree with the following property
;;     (equal (avl-retrieve (avl-delete tr key) key)
;;            nil)
;;
;; 5. (avl-flatten tr)
;;   assumes
;;      tr       has been constructed by the AVL-tree constructors
;;               (empty-tree, avl-insert, and avl-delete)
;;   delivers a list of cons-pairs with the following properties
;;      (and (implies (occurs-in-tree? k tr)
;;                    (and (occurs-in-pairs? k (avl-flatten tr))
;;                         (meta-property-DF tr k)))
;;           (implies (not (occurs-in-tree? k tr))
;;                    (not (occurs-in-pairs? k (avl-flatten tr)))))
;;           (increasing-pairs? (avl-flatten tr)))
;;     where (meta-property-DF tr k) means that one of the elements, e, in
;;     the list (avl-flatten tr) satisfies (equal (car e) k)) and
;;     (cadr e) is the datum at the root of the subtree of tr where k occurs
;;
;;=============================================================================
;;Environment setup
  (in-package "ACL2")
;;=============================================================================

; Extractors (and empty-tree detector)
(defun empty-tree? (tr) (not (consp tr)))
(defun height (tr) (if (empty-tree? tr) 0 (car tr)))
(defun key (tr) (cadr tr))
(defun data (tr) (caddr tr))
(defun left (tr) (cadddr tr))
(defun right (tr) (car (cddddr tr)))
(defun keys (tr)
  (if (empty-tree? tr)
      nil
      (append (keys (left tr)) (list (key tr)) (keys (right tr)))))

; Constructors
(defun empty-tree ( ) nil)
(defun tree (k d lf rt)
		(list (+ 1 (max (height lf) (height rt))) k d lf rt))

; Contstraint detectors and key comparators
(defun key? (k) (rationalp k))	  ; to change representation of keys
(defun key< (j k) (< j k))	  ;     alter definitions of key? and key<
(defun key> (j k) (key< k j))
(defun key= (j k)		  ; note: definitions of
  (and (not (key< j k))           ;    key>, key=, and key-member	
       (not (key> j k))))	  ;        get in line automatically
(defun key-member (k ks)
  (and (consp ks)
       (or (key= k (car ks))
           (key-member k (cdr ks)))))
(defun data? (d)
  (if d t t))
(defun tree? (tr)
  (or (empty-tree? tr)
      (and (natp (height tr))		       ; height
           (= (height tr)                      ;   constraints
              (+ 1 (max (height (left tr))
                        (height (right tr)))))
           (key? (key tr))                     ; key constraint
           (data? (data tr))                   ; data constraint
           (tree? (left tr))                   ; subtree
           (tree? (right tr)))))               ;   constraints

; Key occurs in tree detector
(defun occurs-in-tree? (k tr)
  (and (key? k)
       (tree? tr)
       (key-member k (keys tr))))
(defun alternate-occurs-in-tree? (k tr)
  (and (key? k)
       (tree? tr)
       (not (empty-tree? tr))
       (or (key= k (key tr))
           (alternate-occurs-in-tree? k (left tr))
           (alternate-occurs-in-tree? k (right tr)))))

; all-key comparators
(defun all-keys< (k ks)
  (or (not (consp ks))
      (and (key< (car ks) k) (all-keys< k (cdr ks)))))

(defun all-keys> (k ks)
  (or (not (consp ks))
      (and (key> (car ks) k) (all-keys> k (cdr ks)))))

; definitions of ordered and balanced, and avl-tree detector
(defun ordered? (tr)
  (or (empty-tree? tr)
      (and (tree? tr)
           (all-keys< (key tr) (keys (left tr)))
           (all-keys> (key tr) (keys (right tr)))
           (ordered? (left tr))
           (ordered? (right tr)))))

(defun balanced? (tr)
  (and (tree? tr)
       (or (empty-tree? tr)
           (and (<= (abs (- (height (left tr)) (height (right tr)))) 1)
           (balanced? (left tr))
           (balanced? (right tr))))))

(defun avl-tree? (tr)
  (and (ordered? tr)
       (balanced? tr)))

; rotations
(defun easy-R (tr)
  (let* ((z (key tr)) (dz (data tr))
         (zL (left tr)) (zR (right tr))
         (x (key zL)) (dx (data zL))
         (xL (left zL)) (xR (right zL)))
     (tree x dx xL (tree z dz xR zR))))

(defun easy-L (tr)
  (let* ((z (key tr)) (dz (data tr))
         (zL (left tr)) (zR (right tr))
         (x (key zR)) (dx (data zR))
         (xL (left zR)) (xR (right zR)))
     (tree x dx (tree z dz zL xL) xR)))

(defun left-heavy? (tr)
  (and (tree? tr)
       (not (empty-tree? tr))
       (= (height (left tr)) (+ 2 (height (right tr))))))

(defun outside-left-heavy? (tr)
  (and (left-heavy? tr)
       (or (= (height (left (left tr)))
              (height (right (left tr))))
           (= (height (left (left tr)))
              (+ 1 (height (right (left tr))))))))

(defun right-rotatable? (tr)
  (and (ordered? tr)
       (not (empty-tree? tr))
       (balanced? (left tr))
       (balanced? (right tr))
       (not (empty-tree? (left tr)))))

(defun right-heavy? (tr)
  (and (tree? tr)
       (not (empty-tree? tr))
       (= (height (right tr)) (+ 2 (height (left tr))))))

(defun outside-right-heavy? (tr)
  (and (right-heavy? tr)
       (or (= (height (right (right tr))) (height (left (right tr))))
           (= (height (right (right tr))) (+ 1 (height (left (right tr))))))))

(defun left-rotatable? (tr)
  (and (tree? tr)
       (not (empty-tree? tr))
       (balanced? (left tr))
       (balanced? (right tr))
       (not (empty-tree? (right tr)))))

(defun hard-R (tr)
  (let* ((z (key tr))
         (dz (data tr))
         (zL (left tr))
         (zR (right tr)))
     (easy-R (tree z dz (easy-L zL) zR))))

(defun hard-L (tr)
  (let* ((z (key tr))
         (dz (data tr))
         (zL (left tr))
         (zR (right tr)))
     (easy-L (tree z dz zL (easy-R zR)))))

(defun inside-left-heavy? (tr)
  (and (left-heavy? tr)
       (= (height (right (left tr)))
          (+ 1 (height (left (left tr)))))))

(defun hard-R-rotatable? (tr)
  (and (right-rotatable? tr)
       (left-rotatable? (left tr))))

(defun inside-right-heavy? (tr)
  (and (right-heavy? tr)
       (= (height (left (right tr)))
          (+ 1 (height (right (right tr)))))))

(defun hard-L-rotatable? (tr)
  (and (left-rotatable? tr)
       (right-rotatable? (right tr))))

(defun rot-R (tr)
  (let ((zL (left tr)))
     (if (< (height (left zL)) (height (right zL)))
         (hard-R tr)
         (easy-R tr))))

(defun rot-L (tr)
  (let ((zR (right tr)))
     (if (< (height (right zR)) (height (left zR)))
         (hard-L tr)
         (easy-L tr))))

; insertion
(defun avl-insert (tr new-key new-datum)
  (if (empty-tree? tr)
      (tree new-key new-datum (empty-tree) (empty-tree))
      (if (key< new-key (key tr))
          (let* ((subL (avl-insert (left tr) new-key new-datum))
                 (subR (right tr))
                 (new-tr (tree (key tr) (data tr) subL subR)))
             (if (= (height subL) (+ (height subR) 2))
                 (rot-R new-tr)
                        new-tr))
          (if (key> new-key (key tr))
              (let* ((subL (left tr))
                     (subR (avl-insert (right tr) new-key new-datum))
                     (new-tr (tree (key tr) (data tr) subL subR)))
                 (if (= (height subR) (+ (height subL) 2))
                     (rot-L new-tr)
                     new-tr))
              (tree new-key new-datum (left tr) (right tr))))))

; delete root - easy case
(defun easy-delete (tr)
  (right tr))

; tree shrinking
(defun shrink (tr)
  (if (empty-tree? (right tr))
      (list (key tr) (data tr) (left tr))
      (let* ((key-data-tree (shrink (right tr)))
             (k (car key-data-tree))
             (d (cadr key-data-tree))
             (subL (left tr))
             (subR (caddr key-data-tree))
             (shrunken-tr (tree (key tr) (data tr) subL subR)))
         (if (= (height subL) (+ 2 (height subR)))
             (list k d (rot-R shrunken-tr))
             (list k d shrunken-tr)))))

(defun raise-sacrum (tr)
   (let* ((key-data-tree (shrink (left tr)))
          (k (car key-data-tree))
          (d (cadr key-data-tree))
          (subL (caddr key-data-tree))
          (subR (right tr))
          (new-tr (tree k d subL subR)))
     (if (= (height subR) (+ 2 (height subL)))
         (rot-L new-tr)
         new-tr)))

; delete root - hard case
(defun delete-root (tr)
  (if (empty-tree? (left tr))
      (easy-delete tr)
      (raise-sacrum tr)))

; deletion
(defun avl-delete (tr k)
  (if (empty-tree? tr)
      tr
      (if (key< k (key tr))           ; key occurs in left subtree
          (let* ((new-left (avl-delete (left tr) k))
                 (new-tr (tree (key tr) (data tr) new-left (right tr))))
             (if (= (height (right new-tr)) (+ 2 (height (left new-tr))))
                 (rot-L new-tr)
                 new-tr))
          (if (key> k (key tr))       ; key occurs in right subtree
              (let* ((new-right (avl-delete (right tr) k))
                     (new-tr (tree (key tr) (data tr) (left tr) new-right)))
                     (if (= (height (left new-tr)) (+ 2 (height (right new-tr))))
                         (rot-R new-tr)
                         new-tr))
                 (delete-root tr)))))  ; key occurs at root

; retrieval
(defun avl-retrieve (tr k)  ; delivers key/data pair with key = k
  (if (empty-tree? tr)      ; or nil if k does not occur in tr
      nil                                 ; signal k not present in tree
      (if (key< k (key tr))
          (avl-retrieve (left tr) k)      ; search left subtree
          (if (key> k (key tr))
              (avl-retrieve (right tr) k) ; search right subtree
              (cons k (data tr))))))      ; k is at root, deliver key/data pair

(defun avl-flatten (tr)  ; delivers all key/data cons-pairs
  (if (empty-tree? tr)   ; with keys in increasing order
      nil
      (append (avl-flatten (left tr))
              (list (cons (key tr) (data tr)))
              (avl-flatten (right tr)))))

(defun occurs-in-pairs? (k pairs)
  (and (consp pairs)
       (or (key= k (caar pairs))
           (occurs-in-pairs? k (cdr pairs)))))

(defun increasing-pairs? (pairs)
  (or (not (consp (cdr pairs)))
      (and (key< (caar pairs) (caadr pairs))
           (increasing-pairs? (cdr pairs)))))
