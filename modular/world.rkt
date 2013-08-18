#lang racket/base

(require (prefix-in tp: "../teachpacks/world.rkt")
         (for-syntax racket/base))

(define-syntax (eta stx)
  (syntax-case stx ()
    [(_ f . args) (syntax/loc stx (lambda args (f . args)))]))

(provide (except-out (all-defined-out) eta))

(define make-posn (eta tp:make-posn x y))
(define posn-x (eta tp:posn-x p))
(define posn-y (eta tp:posn-y p))
(define posn? (eta tp:posn? v))
(define weak-posn? (eta tp:weak-posn? v))


;; Type predicates:
(define image? (eta tp:image? v))
(define mode? (eta tp:mode? v))
(define image-color? (eta tp:image-color? v))
(define color? (eta tp:color? v))
(define font-size? (eta tp:font-size? v))
(define bytep (eta tp:bytep v))

;; Color structure:
(define make-color (eta tp:make-color r g b))
(define color-red (eta tp:color-red c))
(define color-green (eta tp:color-green c))
(define color-blue (eta tp:color-blue c))

;; Shape constructors:
(define rectangle (eta tp:rectangle w h m c))
(define circle (eta tp:circle r m c))
(define triangle (eta tp:triangle s m c))
(define star (eta tp:star n o i m c))

;; Other image constructors:
(define text (eta tp:text s f c))
(define line (eta tp:line x y c))
(define empty-scene (eta tp:empty-scene w h))
(define add-line (eta tp:add-line i x y z w c))
(define overlay (eta tp:overlay i j))
(define overlay/xy (eta tp:overlay/xy i x y j))
(define place-image (eta tp:place-image i x y s))

;; Image accessors / operators:
(define image-width (eta tp:image-width i))
(define image-height (eta tp:image-height i))
(define put-pinhole (eta tp:put-pinhole i x y))
(define move-pinhole (eta tp:move-pinhole i x y))
(define pinhole-x (eta tp:pinhole-x i))
(define pinhole-y (eta tp:pinhole-y i))
(define image-inside? (eta tp:image-inside? i j))
(define find-image (eta tp:find-image i j))
(define image->color-list (eta tp:image->color-list i))
(define color-list->image (eta tp:color-list->image i w h x y))

(define key-eventp (eta tp:key-eventp v))
(define mouse-eventp (eta tp:mouse-eventp v))

;; Almost eta expansion, but higher order constructs are definition macros.
(define (big-bang w h r x) (tp:big-bang w h r x) 't)
(define (on-tick-event t) (tp:on-tick-event t) 't)
(define (on-key-event k) (tp:on-key-event k) 't)
(define (on-mouse-event m) (tp:on-mouse-event m) 't)
(define (on-redraw r) (tp:on-redraw r) 't)
(define (stop-when d) (tp:stop-when d) 't)
