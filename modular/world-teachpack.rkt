#lang racket/base

(require "dynamic-rep.rkt"
         racket/runtime-path
         (for-syntax racket/base "static-rep.rkt"))

(provide (for-syntax posn-interface
                     image-interface
                     event-interface
                     world-interface
                     bigbang-interface)

         posn-module
         image-module
         event-module
         bigbang-module)

(define-runtime-path world "world.rkt")

(define (W sym)
  (dynamic-require world sym))

(define-for-syntax (sig name args)
  (make-sig/static name args #f))

(define-for-syntax posn-interface
  (make-interface/static
   #'iposn
   (list (sig #'make-posn (list #'x #'y))
         (sig #'posn-x (list #'p))
         (sig #'posn-y (list #'p))
         (sig #'posn? (list #'v))
         (sig #'weak-posn? (list #'v)))))

(define-for-syntax image-interface
  (make-interface/static
   #'iimage
   (list

    ;; Type predicates:
    (sig #'image? (list #'v))
    (sig #'mode? (list #'v))
    (sig #'image-color? (list #'v))
    (sig #'color? (list #'v))
    (sig #'font-size? (list #'v))
    (sig #'bytep (list #'v))

    ;; Color structure:
    (sig #'make-color (list #'r #'g #'b))
    (sig #'color-red (list #'c))
    (sig #'color-green (list #'c))
    (sig #'color-blue (list #'c))

    ;; Shape constructors:
    (sig #'rectangle (list #'w #'h #'m #'c))
    (sig #'circle (list #'r #'m #'c))
    (sig #'triangle (list #'s #'m #'c))
    (sig #'star (list #'n #'o #'i #'m #'c))

    ;; Other image constructors:
    (sig #'text (list #'s #'f #'c))
    (sig #'line (list #'x #'y #'c))
    (sig #'empty-scene (list #'w #'h))
    (sig #'add-line (list #'x1 #'y1 #'x2 #'y2 #'c))
    (sig #'overlay (list #'a #'b))
    (sig #'overlay/xy (list #'a #'x #'y #'b))
    (sig #'place-image (list #'i #'x #'y #'s))

    ;; Image accessors / operators:
    (sig #'image-width (list #'i))
    (sig #'image-height (list #'i))
    (sig #'put-pinhole (list #'i #'x #'y))
    (sig #'move-pinhole (list #'i #'x #'y))
    (sig #'pinhole-x (list #'i))
    (sig #'pinhole-y (list #'i))
    (sig #'image-inside? (list #'a #'b))
    (sig #'find-image (list #'a #'b))
    (sig #'image->color-list (list #'i))
    (sig #'color-list->image (list #'l #'w #'h #'x #'y))

    )))

(define-for-syntax event-interface
  (make-interface/static
   #'ievent
   (list (sig #'key-eventp (list #'v))
         (sig #'mouse-eventp (list #'v)))))

(define-for-syntax world-interface
  (make-interface/static
   #'iworld
   (list (sig #'handle-tick (list #'w))
         (sig #'handle-key (list #'w #'k))
         (sig #'handle-mouse (list #'w #'x #'y #'m))
         (sig #'render (list #'w))
         (sig #'done (list #'w))
         (sig #'tick-rate (list))
         (sig #'render-width (list))
         (sig #'render-height (list))
         (sig #'initial-world (list)))))

(define-for-syntax bigbang-interface
  (make-interface/static
   #'ibigbang
   (list (sig #'big-bang (list)))))

(define put-fun! interface/dynamic-put-function!)
(define get-fun interface/dynamic-get-function)

(define posn-module
  (make-module/dynamic
   (lambda (imports)
     (define exports (empty-interface/dynamic))
     (put-fun! exports 'make-posn (W 'make-posn))
     (put-fun! exports 'posn-x (W 'posn-x))
     (put-fun! exports 'posn-y (W 'posn-y))
     (put-fun! exports 'posn? (W 'posn?))
     (put-fun! exports 'weak-posn? (W 'weak-posn?))
     (interface/dynamic-join exports imports))))

(define image-module
  (make-module/dynamic
   (lambda (imports)
     (define exports (empty-interface/dynamic))

     ;; Type predicates:
     (put-fun! exports 'image? (W 'image?))
     (put-fun! exports 'mode? (W 'mode?))
     (put-fun! exports 'image-color? (W 'image-color?))
     (put-fun! exports 'color? (W 'color?))
     (put-fun! exports 'font-size? (W 'font-size?))
     (put-fun! exports 'bytep (W 'bytep))

     ;; Color structure:
     (put-fun! exports 'make-color (W 'make-color))
     (put-fun! exports 'color-red (W 'color-red))
     (put-fun! exports 'color-green (W 'color-green))
     (put-fun! exports 'color-blue (W 'color-blue))

     ;; Shape constructors:
     (put-fun! exports 'rectangle (W 'rectangle))
     (put-fun! exports 'circle (W 'circle))
     (put-fun! exports 'triangle (W 'triangle))
     (put-fun! exports 'star (W 'star))

     ;; Other image constructors:
     (put-fun! exports 'text (W 'text))
     (put-fun! exports 'line (W 'line))
     (put-fun! exports 'empty-scene (W 'empty-scene))
     (put-fun! exports 'add-line (W 'add-line))
     (put-fun! exports 'overlay (W 'overlay))
     (put-fun! exports 'overlay/xy (W 'overlay/xy))
     (put-fun! exports 'place-image (W 'place-image))

     ;; Image accessors / operators:
     (put-fun! exports 'image-width (W 'image-width))
     (put-fun! exports 'image-height (W 'image-height))
     (put-fun! exports 'put-pinhole (W 'put-pinhole))
     (put-fun! exports 'move-pinhole (W 'move-pinhole))
     (put-fun! exports 'pinhole-x (W 'pinhole-x))
     (put-fun! exports 'pinhole-y (W 'pinhole-y))
     (put-fun! exports 'image-inside? (W 'image-inside?))
     (put-fun! exports 'find-image (W 'find-image))
     (put-fun! exports 'image->color-list (W 'image->color-list))
     (put-fun! exports 'color-list->image (W 'color-list->image))

     (interface/dynamic-join exports imports))))

(define event-module
  (make-module/dynamic
   (lambda (imports)
     (define exports (empty-interface/dynamic))
     (put-fun! exports 'key-eventp (W 'key-eventp))
     (put-fun! exports 'mouse-eventp (W 'mouse-eventp))
     (interface/dynamic-join exports imports))))

(define bigbang-module
  (make-module/dynamic
   (lambda (imports)

     ;; Big-bang imports
     (define handle-tick (get-fun imports 'handle-tick))
     (define handle-key (get-fun imports 'handle-key))
     (define handle-mouse (get-fun imports 'handle-mouse))
     (define render (get-fun imports 'render))
     (define done (get-fun imports 'done))
     (define tick-rate (get-fun imports 'tick-rate))
     (define render-width (get-fun imports 'render-width))
     (define render-height (get-fun imports 'render-height))
     (define initial-world (get-fun imports 'initial-world))

     ;; World functions
     (define W:big-bang (W 'big-bang))
     (define W:on-tick-event (W 'on-tick-event))
     (define W:on-key-event (W 'on-key-event))
     (define W:on-mouse-event (W 'on-mouse-event))
     (define W:on-redraw (W 'on-redraw))
     (define W:stop-when (W 'stop-when))

     ;; Computed values
     (define width (render-width))
     (define height (render-height))
     (define rate (tick-rate))
     (define world0 (initial-world))

     ;; Exports
     (define exports (empty-interface/dynamic))
     (define (big-bang)
       (W:big-bang width height rate world0)
       (W:on-tick-event handle-tick)
       (W:on-key-event handle-key)
       (W:on-mouse-event handle-mouse)
       (W:on-redraw render)
       (W:stop-when done)
       't)
     (put-fun! exports 'big-bang big-bang)
     (interface/dynamic-join exports imports))))
