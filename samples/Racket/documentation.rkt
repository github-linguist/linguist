#lang scribble/manual
(require (for-label "sandwiches.rkt"))

@defproc[(make-sandwich [ingredients (listof ingredient?)])
         sandwich?]{
  Returns a sandwich given the right ingredients.
}
