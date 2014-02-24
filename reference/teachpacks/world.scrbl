#lang scribble/doc
@(require
   scribble/manual
   scribble/eval
   racket/require
   (path-up "self/require.rkt")
   (path-up "self/scribble.rkt")
   (path-up "self/module-path.rkt")
   "../evaluator.rkt"
   (for-label
     (dracula-in main)
     (teachpack-in world)))

@title[#:style 'quiet (scheme "world")]

@(declare-exporting/dracula teachpacks/world)

@specform[(include-book "world" :dir :teachpacks)]

This teachpack provides a datatype for images and a framework for interactive
animations.  It is partially reflected in the ACL2 logic, but some of the
primitives cannot be fully reasoned about or executed due by ACL2 to their
imperative and platform-specific nature.

@section{Animations}

These forms produce an interactive animation that reacts to events.  The user
must define a World datatype, representing the state of an animation, and
functions to update the World in response to events such as keystrokes or mouse
buttons.  From this, the teachpack can generate an interactive animation.

Events are based on two extra datatypes.  A KeyEvent represents a keystroke, and
may be either a character or a symbol such as @scheme['left], @scheme['right],
@scheme['up], or @scheme['down].  A MouseEvent represents an action of the mouse
cursor, and may be @scheme['button-down], @scheme['button-up], @scheme['drag],
@scheme['move], @scheme['enter], or @scheme['leave].

@defproc[(big-bang [width posp] [height posp] [time rationalp] [world t]) t]{
Starts an animation, given a width and height for each frame, the time in
seconds between clock ticks, and the initial World value.
}

@defproc[(on-redraw [draw (code:line World -> image?)]) t]{
Registers a function to draw each frame of an animation.
}

@defproc[(stop-when [game-over (code:line World -> boolean?)]) t]{
Registers a function to report when the animation has ended.
}

@deftogether[(
@defproc[(on-tick-event [tick (code:line World -> World)]) t]
@defproc[(on-tick-event/state [tick (code:line World state -> (mv World state))]) t]
)]{
Registers a function to update the world each time the clock ticks.  The second
version allows i/o actions during the event handler based on ACL2's
@racket[state] variable.
}

@deftogether[(
@defproc[(on-key-event [press (code:line World KeyEvent -> World)]) t]
@defproc[(on-key-event/state [press (code:line World KeyEvent State -> (mv World State))]) t]
)]{
Registers a function to update the world each time the user presses or releases
a key.  The second
version allows i/o actions during the event handler based on ACL2's
@racket[state] variable.
}

@deftogether[(
@defproc[(on-mouse-event [click (code:line World integerp integerp MouseEvent ->
World)]) t]
@defproc[(on-mouse-event/state [click (code:line World integerp integerp MouseEvent State -> (mv World State))]) t]
)]{
Registers a function to update the world each time the user moves or clicks the
mouse.  The second
version allows i/o actions during the event handler based on ACL2's
@racket[state] variable.
}

@section{Scenes}

These primitives operate on images intended as complete animation frames.

@defproc[(empty-scene [width posp] [height posp]) image?]{
Produces a blank scene of the given dimensions from which to construct an
animation frame.
}

@defproc[(place-image [img image?] [x integerp] [y integerp] [scn image?])
         image?]{
Adds @scheme[img] to @scheme[scn] at the given coordinates.
}

@section{Images}

These primitives construct images as first-class values.

@deftogether[(
@defproc[(rectangle [w posp] [h posp] [m mode?] [c image-color?]) image?]
@defproc[(circle [r posp] [m mode?] [c image-color?]) image?]
@defproc[(triangle [s posp] [m mode?] [c image-color?]) image?]
@defproc[(line [x integerp] [y integerp] [c image-color?]) image?]
)]{
These functions construct images of various geometric shapes.
}

@defproc[(text [str stringp] [size posp] [color image-color?]) image?]{
Constructs an image of text.
}

@defproc[(add-line [i image?]
                   [x1 integerp] [y1 integerp]
                   [x2 integerp] [y2 integerp]
                   [c image-color?])
         image?]{
Adds a line from (@scheme[x1],@scheme[y1]) to (@scheme[x2],@scheme[y2]) of color
@scheme[c] to @scheme[i].
}

@deftogether[(
@defproc[(overlay [i image?] ...) image?]
@defproc[(overlay/xy [i image?] [x integerp] [y integerp] [j image?]) image?]
)]{
These functions place one image over another.  The first overlays a number of
images as they are; the second overlays one image at an offset on another.
}

@deftogether[(
@defproc[(image-width [i image?]) posp]
@defproc[(image-height [i image?]) posp]
)]{
These functions compute the dimensions of an image.
}

@deftogether[(
@defproc[(put-pinhole [i image?] [x integerp] [y integerp]) image?]
@defproc[(move-pinhole [i image?] [x integerp] [y integerp]) image?]
@defproc[(pinhole-x [i image?]) integerp]
@defproc[(pinhole-y [i image?]) integerp]
)]{
These functions deal with an image's pinhole, the point on which
@scheme[overlay] places other images and from which @scheme[place-image]
calculates offsets.
}

@defproc[(image? [v t]) booleanp]{
Recognizes images.
}

@section{Colors and Modes}

@defproc[(mode? [v t]) booleanp]{
Recognizes image drawing modes: @scheme['solid] or @scheme['outline].
}

@defproc[(image-color? [v t]) booleanp]{
Recognizes colors constructed with @scheme[make-color], or color names as
strings or symbols.  Color names include:

DarkRed, Red, LightPink, Pink, Brown, DarkOrange, Orange, Yellow, LightYellow,
Green, DarkGreen, LightGreen, Cyan, LightBlue, LightCyan, DarkBlue, Blue,
Purple, Magenta, DarkMagenta, Violet, White, LightGray, Gray, DarkGray, and
Black.

}

@deftogether[(
@defproc[(make-color [r bytep] [g bytep] [b bytep]) color?]
@defproc[(color-red [c color?]) bytep]
@defproc[(color-green [c color?]) bytep]
@defproc[(color-blue [c color?]) bytep]
@defproc[(color? [v t]) booleanp]
)]{
These functions (constructor, selectors, and predicate) define a datatype of
colors based on red, green, and blue values between 0 and 255.
}

@defproc[(bytep [v t]) booleanp]{
Recognizes integers between 0 and 255, inclusive.
}

@section{Posns}

@deftogether[(
@defproc[(make-posn [x integerp] [y integerp]) posn?]
@defproc[(posn-x [p posn?]) integerp]
@defproc[(posn-y [p posn?]) integerp]
@defproc[(posn? [v t]) booleanp]
@defproc[(weak-posn? [v t]) booleanp]
)]{
Posns are structures representing positions in two-dimensional space.  These
procedures correspond to the constructor, selectors, predicate, and weak
predicate for Posns.
}