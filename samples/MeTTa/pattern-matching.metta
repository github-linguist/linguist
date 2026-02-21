; Pattern matching and non-deterministic evaluation in MeTTa
;
; MeTTa supports non-deterministic pattern matching through
; the `match` operation on atomspaces.

; Type declarations for a graph structure
(: Node Type)
(: Edge (-> Node Node Type))

(: a Node)
(: b Node)
(: c Node)
(: d Node)

; Define a directed graph
(Edge a b)
(Edge b c)
(Edge c d)
(Edge a c)

; Find all direct neighbors of a node
(= (neighbors $node)
   (match &self (Edge $node $neighbor) $neighbor))

; Find all paths of length 2
(= (path-2 $start $end)
   (let $mid (neighbors $start)
     (if (== (neighbors $mid) $end)
         ($start $mid $end)
         (empty))))

; Superpose to explore all neighbors non-deterministically
!(superpose ((neighbors a) (neighbors b) (neighbors c)))

; Collapse all results into a single expression
!(collapse (match &self (Edge a $x) $x))

; Case expression for classification
(= (classify $n)
   (case $n
     ((0 "zero")
      (1 "one")
      (2 "two")
      ($_ "other"))))

!(assertEqual (classify 0) "zero")
!(assertEqual (classify 1) "one")
!(assertEqual (classify 99) "other")

; Using get-type for runtime introspection
!(get-type 42)
!(get-type "hello")
!(get-type True)

; Import and module system
; (import! &self metta:stdlib)

; Bind a new space for isolated reasoning
!(bind! &kb (new-space))
!(add-atom &kb (Edge x y))
!(match &kb (Edge $a $b) ($a $b))
