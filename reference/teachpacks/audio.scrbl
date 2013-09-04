#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require (for-label dracula/lang/dracula
                     dracula/teachpacks/audio))

@title[(scheme "audio")]

@(declare-exporting dracula/teachpacks/audio)

@specform[(include-book "audio" :dir :teachpacks)]

@defproc[(play-wav [file stringp] [async booleanp]) booleanp]{
Plays the .WAV file named by @scheme[file], returning whether or not the file
was playable.  If @scheme[async] is @scheme[nil], it does not return until the
file is complete, otherwise it returns immediately.
}
