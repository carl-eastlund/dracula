#|
Bug:  this teachpack provides procedures that
      do not check for 1st-order usage.
|#
#lang racket/base

(require "../private/planet.rkt"
         (prefix-in mz: (combine-in racket/base
                                    htdp/world
                                    lang/posn
                                    (cce define)
                                    (cce function)))
         "../lang/dracula.rkt"
         "../lang/check.rkt"
         (for-syntax racket/base))

(mz:define-if-unbound mz:image?
  (mz:block
   (mz:local-require (only-in lang/htdp-advanced image?))
   (mz:eta* image? i)))

(provide (all-defined-out))

(begin-below

 ;; these need to be syntax that check for 1st order use.

 (defun empty-scene (w h) (mz:empty-scene w h))
 (defun place-image (i x y s) (mz:place-image i x y s))
 (defun add-line (i x y z w c) (mz:add-line i x y z w c))
 (defun make-color (r g b) (mz:make-color r g b))
 (defun color-red (c) (mz:color-red c))
 (defun color-green (c) (mz:color-green c))
 (defun color-blue (c) (mz:color-blue c))
 (defun rectangle (w h m c) (mz:nw:rectangle w h m c))
 (defun circle (r m c) (mz:circle r m c))
 (defun text (s f c) (mz:text s f c))
 (defun image-width (i) (mz:image-width i))
 (defun image-height (i) (mz:image-height i))
 (defun overlay (i j) (mz:overlay i j))
 (defun overlay/xy (i x y j) (mz:overlay/xy i x y j))
 (defun color-list->image (l w h x y) (mz:color-list->image l w h x y))
 (defun image->color-list (i) (mz:image->color-list i))
 (defun triangle (s m c) (mz:triangle s m c))
 (defun star (n o i m c) (mz:star n o i m c))
 (defun line (x y c) (mz:line x y c))
 (defun put-pinhole (i x y) (mz:put-pinhole i x y))
 (defun move-pinhole (i x y) (mz:move-pinhole i x y))
 (defun pinhole-x (i) (mz:pinhole-x i))
 (defun pinhole-y (i) (mz:pinhole-y i))

 (defun image-inside? (i j) (mz:if (mz:image-inside? i j) t nil))
 (defun bytep (v) (mz:if (mz:byte? v) t nil))
 (defun color? (v) (mz:if (mz:color? v) t nil))
 (defun image-color? (v) (mz:if (mz:image-color? v) t nil))
 (defun image? (v) (mz:if (mz:image? v) t nil))

 (defun mode? (v) (or (equal v 'solid) (equal v 'outline)))

 (defun make-posn (x y) (list 'make-posn x y))
 (defun posn-x (p) (cadr p))
 (defun posn-y (p) (caddr p))

 (defun weak-posn? (v)
   (and (true-listp v)
        (= (length v) 3)
        (eq (car v) 'make-posn)))

 (defun posn? (v)
   (and (weak-posn? v)
        (integerp (posn-x v))
        (integerp (posn-y v))))

 (defun find-image (a b)
   (let* ([p (mz:find-image a b)])
     (make-posn (mz:posn-x p) (mz:posn-y p))))

 (defun mouse-eventp (v)
   (and (member v '(button-down button-up drag move enter leave))
        t))

 (defun key-eventp (v)
   (or (symbolp v) (characterp v)))

 (defun font-size? (v)
   (and (integerp v) (<= 1 v) (<= v 255)))

 #|
 (defun packet (w m) (mz:make-package w m))
 (defun packet-p (x) (mz:package? x))
 ;;(defun packet-world (p) (mz:package-world p))
 ;;(defun packet-message (p) (mz:package-message p))

 (defconst *localhost* mz:LOCALHOST)
 |#

 (define-syntaxes ( on-tick-event
                    on-tick-event/state
                    on-key-event
                    on-key-event/state
                    on-redraw
                    stop-when
                    on-mouse-event
                    on-mouse-event/state
                    #|
                    on-receive-event
                    register
                    universe
                    |#
                    big-bang )
   (values
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-tick-event (lambda (w) (cb-name w)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-tick-event
                 (lambda (w)
                   (mv-let (w state) (cb-name w state) w)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-key-event (lambda (k w) (cb-name k w)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-key-event
                 (lambda (k w)
                   (mv-let (w state) (cb-name k w state) w)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-redraw (lambda (w) (cb-name w)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:stop-when
                (lambda (w)
                  (mz:if (member (cb-name w) '(nil ())) #f #t)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-mouse-event 
                (lambda (w x y evt) 
                  (cb-name w x y evt)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ cb-name)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:on-mouse-event 
                 (lambda (w x y evt)
                   (mv-let (w state) (cb-name w x y evt state) w)))
               (values))))]))
    (lambda (stx)
      (syntax-case stx ()
        [(_ width height freq w0)
         (quasisyntax/loc stx
           (define-values []
             (begin
               (mz:big-bang width height freq w0)
               (values))))]
        [_ (raise-syntax-error
            #f
            "big-bang is a procedure that expects 4 arguments"
            stx)]))))

 )