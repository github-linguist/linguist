;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We can use Nodes and Links in the OpenCog
; Classic style, because these are also symbolic
; expressions, although Concept and List will not be
; turned into types and will remain just symbols
; in expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(EvaluationLink
  (PredicateNode "eats")
  (ListLink
     (ConceptNode "Sam")
     (ConceptNode "flies")))

(EvaluationLink
  (PredicateNode "eats")
  (ListLink
     (ConceptNode "Bill")
     (ConceptNode "pizza")))

(EvaluationLink
  (PredicateNode "eats")
  (ListLink
     (ConceptNode "Fritz")
     (ConceptNode "flies")))

!(assertEqualToResult
  (match &self
    (EvaluationLink
      (PredicateNode "eats")
       (ListLink
          $x
          (ConceptNode "flies")))
     $x)
  ((ConceptNode "Sam")
   (ConceptNode "Fritz")))
