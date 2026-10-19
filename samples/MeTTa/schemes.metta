; For references see https://github.com/passy/awesome-recursion-schemes
; For my typed Scala implementation see https://github.com/Adam-Vandervorst/RecursionSchemes
!(transfer! "base.metta")

; folds
(= ((cata $F $alg) $a) ($alg ($F (cata $F $alg) (unFix $a))))

(= ((para_pair $F $alg) $a) (, ((para $F $alg) $a) $a))
(= ((para $F $alg) $a) ($alg ($F (para_pair $F $alg) (unFix $a))))

; unfolds
(= ((ana $F $coalg) $s) (Fix ($F (ana $F $coalg) ($coalg $s))))

(= ((futu_picking $F $coalg) (Pure $a)) ((futu $F $coalg) $a))
(= ((futu_picking $F $coalg) (Bind $f)) (Fix ($F (futu_picking $F $coalg) $f)))
(= ((futu $F $coalg) $s) (Fix ($F (futu_picking $F $coalg) ($coalg $s))))

; combinations
(= ((hylo $F $alg $coalg) $s) ($alg ($F (hylo $F $alg $coalg) ($coalg $s))))
