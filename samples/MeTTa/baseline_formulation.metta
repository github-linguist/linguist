!(import! &self toy)
; !(import! &self royal92_simple)
; !(import! &self adameve_simple)
; !(import! &self lordOfTheRings_simple)
; !(import! &self simpsons_simple)

; UTILITY
(= (different $x $y) (if (== $x $y) (empty) $x))

; GENERATE EXTRA INDICES
!(match &self (parent $x $y) (add-atom &self (child $y $x)))

!(match &self (male $x) (add-atom &self (person $x)))
!(match &self (female $x) (add-atom &self (person $x)))

; GET ALL PARENTS
(= (get_parent $x) (match &self (parent $y $x) $y))
!(match &self (person $x) (Parent $x (get_parent $x)))

; GET ALL MOTHERS
(= (get_mother $x) (match &self (, (parent $y $x) (female $y)) $y))
!(match &self (person $x) (Mother $x (get_mother $x)))

; GET ALL SISTERS
(= (get_sister $x) (match &self (, (parent $y $x) (parent $y $z) (female $z)) (different $z $x)))
!(match &self (person $x) (Sister $x (get_sister $x)))

; GET ALL AUNTS
(= (get_aunt $x) (match &self (, (parent $p $x) (parent $gp $p) (parent $gp $q) (female $q)) (different $q $p)))
!(match &self (person $x) (Aunt $x (get_aunt $x)))

; GET ALL PREDECESSORS
(= (get_pred $x) (match &self (parent $p $x) $p))
(= (get_pred $x) (match &self (parent $p $x) (get_pred $p)))
!(match &self (person $x) (Pred $x (get_pred $x)))
