definition module GenHylo

import StdGeneric, GenMap

:: Fix f = In (f .(Fix f))
Out :: !u:(Fix v:a) -> v:(a w:(Fix v:a)), [u <= w]

hylo :: ((.f .b) -> .b) (.a -> (.f .a)) -> (.a -> .b) | gMap{|*->*|} f
cata :: (u:(f .a) -> .a) -> (Fix u:f) -> .a | gMap{|*->*|} f
ana :: (.a -> u:(f .a)) -> .a -> (Fix u:f) | gMap{|*->*|} f

