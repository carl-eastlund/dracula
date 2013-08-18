#lang scribble/doc
@(require scribble/manual scribble/eval "../evaluator.ss")
@(require (for-label dracula/lang/dracula))

@title{IO}

@defthing[state state]

@defform[(open-input-channel ...)]
@defform[(open-output-channel ...)]

@defform[(read-byte$ ...)]
@defform[(read-char$ ...)]

@defform[(write-byte$ ...)]
