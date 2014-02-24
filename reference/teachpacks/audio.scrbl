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
     (teachpack-in audio)))

@title[(scheme "audio")]

@(declare-exporting/dracula teachpacks/audio)

@specform[(include-book "audio" :dir :teachpacks)]

@defproc[(play-wav [file stringp] [async booleanp]) booleanp]{
Plays the .WAV file named by @scheme[file], returning whether or not the file
was playable.  If @scheme[async] is @scheme[nil], it does not return until the
file is complete, otherwise it returns immediately.
}
