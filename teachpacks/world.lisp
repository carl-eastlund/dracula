(in-package "ACL2")

(include-book "image")

(encapsulate 
 (((empty-scene * *) => *)
  ((place-image * * * *) => *))
 (local (defun empty-scene (x y) (rectangle x y 'solid 'white)))
 (local (defun place-image (img x y canvas-img)
          (overlay/xy canvas-img x y img)))
 (defthm image?-empty-scene=true
     (implies (and (posp x) (posp y))
              (image? (empty-scene x y))))

 ;; CCE: this was an iff in the other direction,
 ;; but image.lisp does not support that conclusion.
 (defthm image?-place-image=true
     (implies (and (image? img) (integerp x) (integerp y) (image? scene))
              (image? (place-image img x y scene))))

 )

(defun key-eventp (v) (or (characterp v) (symbolp v)))

(defun mouse-eventp (v)
  (consp (member v '(button-down button-up drag move enter leave))))

(defmacro big-bang (width height frequency world0)
  ;(declare (ignore width height frequency world0))
  `(defconst *big-bang* (list 'big-bang ',width ',height ',frequency ',world0)))

(defmacro on-redraw (x) 
  `(defconst *on-redraw* (quote ,x)))
(defmacro on-tick-event (x)
  `(defconst *on-tick-event* (quote ,x)))
(defmacro on-tick-event/state (x)
  `(defconst *on-tick-event/state* (quote ,x)))
(defmacro on-key-event (x) 
  `(defconst *on-key-event* (quote ,x)))
(defmacro on-key-event/state (x) 
  `(defconst *on-key-event/state* (quote ,x)))
(defmacro on-mouse-event (x)
  `(defconst *on-mouse-event* (quote ,x)))
(defmacro on-mouse-event/state (x)
  `(defconst *on-mouse-event/state* (quote ,x)))
(defmacro on-receive-event (x)
  `(defconst *on-receive-event* (quote ,x)))
(defmacro on-receive-event/state (x)
  `(defconst *on-receive-event/state* (quote ,x)))
(defmacro stop-when (x)
  `(defconst *stop-when* (quote ,x)))
(defmacro universe (initial process)
  `(defconst *universe* (list (quote ,initial) (quote ,process))))
(defmacro register (host name)
  `(defconst *register* (list (quote ,host) (quote ,name))))

(defconst *localhost* "127.0.0.1")

(include-book "data-structures/structures" :dir :system)
(defstructure packet world message)
