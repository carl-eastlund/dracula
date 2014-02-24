#lang scribble/doc
@(require
   scribble/manual
   scribble/eval
   racket/require
   (path-up "self/require.rkt")
   "../evaluator.rkt"
   (for-label
     (dracula-in main)))

@title{IO}

@defthing[state state]

@defform[(open-input-channel ...)]
@defform[(open-output-channel ...)]

@defform[(read-byte$ ...)]
@defform[(read-char$ ...)]

@defform[(write-byte$ ...)]
