#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)
                     (this-package-in teachpacks/avl-rational-keys)))

@title[(scheme "avl-rational-keys")]

@(declare-exporting/this-package [teachpacks/avl-rational-keys] [])

@specform[(include-book "avl-rational-keys" :dir :teachpacks)]

Documentation under construction.

@verbatim{
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
}
