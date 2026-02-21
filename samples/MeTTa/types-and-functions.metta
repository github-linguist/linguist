; Type definitions for a simple knowledge base
(: Entity Type)
(: Relation Type)
(: Triple (-> Entity Relation Entity Type))

; Define some entities and relations
(: human Entity)
(: mortal Entity)
(: socrates Entity)
(: is-a Relation)
(: implies Relation)

; Knowledge base facts
(Triple socrates is-a human)
(Triple human implies mortal)

; Inference rule: transitivity through implication
(= (infer (Triple $x is-a $class1))
   (let* (((Triple $x is-a $class0) (match &self (Triple $x is-a $class0) (Triple $x is-a $class0)))
          ((Triple $class0 implies $class1) (match &self (Triple $class0 implies $class1) (Triple $class0 implies $class1))))
     (Triple $x is-a $class1)))

; Query the knowledge base
!(match &self (Triple socrates is-a $what) $what)
!(infer (Triple socrates is-a $what))

; Higher-order function: map over a list
(: map-list (-> (-> $a $b) Expression $b))
(= (map-list $f ()) ())
(= (map-list $f ($head $tail))
   ((apply $f $head) (map-list $f $tail)))

; Arithmetic helper
(= (square $x) (* $x $x))

; Assert results
!(assertEqual (square 5) 25)
!(assertEqual (+ 2 3) 5)
