#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require "../../private/planet.ss")
@(require (cce scribble))
@(require (for-label (this-package-in lang/dracula)))

@title{IO}

@defthing[state state]

@defform[(open-input-channel ...)]
@defform[(open-output-channel ...)]

@defform[(read-byte$ ...)]
@defform[(read-char$ ...)]

@defform[(write-byte$ ...)]
