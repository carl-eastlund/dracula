#|
January 31 2006:  Decided to suspend formalization of images.
It's not on the critical path, and we can still reason about video games
via world-next and world-change.  The original code is toward the end
of this file.
|#

;; We'll just inform ACL2 that the image functions exist
;; but that we know nothing about them.

(in-package "ACL2")
(include-book "data-structures/structures" :dir :system)

;; Don't export bytep, but enable the prover to rewrite it away in
;; case it gets introduced by the theorems below.
(defun bytep (x) (integer-range-p 0 256 x)) ;; 0 <= x < 256
(defthm bytep-x=>0<=x<256
  (equal (bytep X) (integer-range-p 0 256 X)))
(in-theory (disable bytep))

(defstructure make-posn
   (x (:assert (integerp x)))
   (y (:assert (integerp y)))
   (:options (:keyword-constructor nil)
             (:keyword-updater nil)
             (:conc-name posn-)
             (:weak-predicate weak-posn?)
             (:predicate posn?)))

(encapsulate (((mode? *) => *)
              
              ((color? *) => *)
              ((image-color? *) => *)
              ((make-color * * *) => *)
              ((color-red *) => *)
              ((color-green *) => *)
              ((color-blue *) => *)
              
              ((image? *) => *)
              ((circle * * *) => *)
              ((rectangle * * * *) => *)
              ((triangle * * *) => *)
              ((star * * * * *) => *)
              ((line * * *) => *)
              ((add-line * * * * * *) => *)
              ((text * * *) => *)
              ((overlay * *) => *)
              ((overlay/xy * * * *) => *)
              
              ((image-width *) => *)
              ((image-height *) => *)
              
              ((pinhole-x *) => *)
              ((pinhole-y *) => *)
              ((move-pinhole * * *) => *)
              ((put-pinhole * * *) => *)
              
              ((image-inside? * *) => *)
              ((find-image * *) => *)
              
              ((image->color-list *) => *)
              ((color-list->image * * * * *) => *))
 (local (defun mode? (x) 
          (and (symbolp x)
               (or (eq x 'solid) (eq x 'outline)))))
 
 (local (defun color? (x) (eq (car x) 'color)))
 (local (defun image-color? (x) (or (symbolp x) (color? x))))
 (local (defun make-color (r g b) (list 'color r g b)))
 (local (defun color-red (c) (declare (ignore c)) 0))
 (local (defun color-green (c) (declare (ignore c)) 0))
 (local (defun color-blue (c) (declare (ignore c)) 0))

;; font-size? : Any -> Boolean
;; Checks if the input is a valid font size.
(defun font-size? (x)
  (and (integerp x)
       (>= x 1)
       (<= x 255)))
 

 ;; Image witnesses
 (local (defun image? (x) 
          (case-match x
	    (('circle r m c) (and (posp r) (mode? m) (image-color? c)))
	    (('rectangle w h m c)
	     (and (posp w) (posp h) (mode? m) (image-color? c)))
	    (('triangle lng m c)
	     (and (posp lng) (mode? m) (image-color? c)))
            (('star n o i m c)
             (and (integerp n) (>= n 2)
                  (posp o)
                  (posp i)
                  (mode? m)
                  (image-color? c)))
	    (('overlay im1 im2)
	     (and (image? im1) (image? im2)))
	    (('overlay/xy im1 x y im2)
	     (and (image? im1) (image? im2)
		  (integerp x) (integerp y)))
	    (('place-image im x y scene)
	     (and (image? im) (image? scene)
		  (integerp x) (integerp y)))
	    (('line x y c)
	     (and (integerp x) (integerp y) (image-color? c)))
	    (('add-line im x0 y0 x1 y1 c)
	     (and (image? im) (image-color? c) 
		  (integerp x0) (integerp y0)
		  (integerp x1) (integerp y1)))
	    (('text str sz c)
	     (and (stringp str) (posp sz) (image-color? c)))
	    )
	    
	    
	  ;(and (member-eq (car x) 
          ;                '(circle rectangle triangle overlay overlay/xy
          ;                         line add-line text color-list-image))
          ;     t)
	  ))
 (local (defun circle (radius mode color) 
          (list 'circle radius mode color)))
 (local (defun rectangle (width height mode color) 
          (list 'rectangle width height mode color)))
 (local (defun triangle (side-length mode color)
          (list 'triangle side-length mode color)))
 (local (defun star (n o i m c)
          (list 'star n o i m c)))
 (local (defun overlay (image-1 image-2) (list 'overlay image-1 image-2)))
 (local (defun overlay/xy (tgt-img x y src-img)
          (list 'overlay/xy tgt-img x y src-img)))
 (local (defun place-image (img x y scene)
          (list 'place-image img x y scene)))
 (local (defun line (x y color)
          (list 'line x y color)))
 (local (defun add-line (img x0 y0 x1 y1 color)
          (list 'add-line img x0 y0 x1 y1 color)))
 (local (defun text (str size color)
          (list 'text str size color)))
 
 (local (defun image-width (x) (declare (ignore x)) 0))
 (local (defun image-height (x) (declare (ignore x)) 0))
 
 (local (defun pinhole-x (i) (nfix i)))
 (local (defun pinhole-y (i) (nfix i)))
 
 (local (defun move-pinhole (i x y) (declare (ignore x y)) i))
 (local (defun put-pinhole (i x y) (declare (ignore x y)) i))
 
 (local (defun image-inside? (i1 i2) (equal i1 i2)))
 (local (defun find-image (i1 i2)
          (declare (ignore i1 i2))
          (make-posn 0 0)))
 
 (local (defun image->color-list (img)
          (list img)))
 (local (defun color-list->image (lo-color width height x y)
          (list 'color-list-image lo-color width height x y)))
 
 ;; Drawing mode spec
 (defthm mode?<=>outline-or-solid
   (iff (mode? x) (or (eq x 'solid) (eq x 'outline))))
 
 ;; Color specification
 (defthm color?@any->bool (booleanp (color? X)))
 (defthm image-color?@any->bool (booleanp (image-color? X)))
 (defthm image-color?-defn
     (iff (image-color? x) (or (symbolp x) (color? x))))
 (defthm color?=>image-color?
   (implies (color? C) (image-color? C)))
 (defthm make-color@byte*byte*byte->color?
   (implies (and (bytep r) (bytep g) (bytep b))
            (color? (make-color r g b))))
 (defthm color-red@color?->byte
   (implies (color? c) (bytep (color-red c))))
 (defthm color-green@color?->byte
   (implies (color? c) (bytep (color-green c))))
 (defthm color-blue@color?->byte
   (implies (color? c) (bytep (color-blue c))))
 
 ;; Image spec
 (defthm image?@any->bool (booleanp (image? X)))
 ;(defthm image?-circle=true
 ;  (implies (and (posp radius) (mode? mode) (image-color? color))
 ;           (image? (circle radius mode color))))
 (defthm image?-circle
     (iff (image? (circle radius mode color))
          (and (posp radius) (mode? mode) (image-color? color))))
 (defthm image?-rectangle=true
   (implies (and (posp width) (posp height) (mode? mode) (image-color? color))
            (image? (rectangle width height mode color))))
 (defthm image?-triangle=true
   (implies (and (posp size) (mode? mode) (image-color? color))
            (image? (triangle size mode color))))
 (defthm image?-star=true
   (implies (and (integerp n) (>= n 2)
                 (posp o) (posp i)
                 (mode? m) (image-color? c))
            (image? (star n o i m c))))
 (defthm image?-text=true
   (implies (and (stringp s) (font-size? sz) (image-color? c))
            (image? (text s sz c))))
 (defthm image?-overlay=true
   (implies (and (image? I) (image? J)) (image? (overlay I J))))
 (defthm image?-overlay/xy=true
   (implies (and (image? src) (image? tgt) (integerp x) (integerp y))
            (image? (overlay/xy tgt x y src))))
 
 
 (defthm image-width@image?->natp
   (implies (image? I) (natp (image-width I))))
 (defthm image-height@image?->natp
   (implies (image? I) (natp (image-height I))))
 
 (defthm pinhole-x@image?->natp
   (implies (image? I) (natp (pinhole-x I))))
 (defthm pinhole-y@image?->natp
   (implies (image? I) (natp (pinhole-y I))))
 
 (defthm move-pinhole@image?*integerp*integerp->image?
   (implies (and (image? I) (integerp X) (integerp Y))
            (image? (move-pinhole I X Y))))
 (defthm put-pinhole@imag?*natp*natp->image?
   (implies (and (image? I) (natp X) (natp Y))
            (image? (put-pinhole I X Y))))
 
 (defthm image-inside?@image?*image?->bool
   (implies (and (image? I) (image? J)) (booleanp (image-inside? I J))))
 
 (defthm find-image@image?*image?->posn?
   (implies (and (image? I) (image? J)) (posn? (find-image I J))))


(defthm empty-text-image-width
  (implies (and (font-size? size) (image-color? color))
           (= (image-width (text "" size color)) 0)))

(defthm append-right-text-image-width
  (implies (and (stringp a) (stringp b) (font-size? size) (image-color? color))
           (>= (image-width (text (string-append a b) size color))
               (image-width (text a size color)))))

(defthm append-left-text-image-width
  (implies (and (stringp a) (stringp b) (font-size? size) (image-color? color))
           (>= (image-width (text (string-append a b) size color))
               (image-width (text b size color)))))
 )
