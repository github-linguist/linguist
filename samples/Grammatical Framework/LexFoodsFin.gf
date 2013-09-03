-- (c) 2009 Aarne Ranta under LGPL

instance LexFoodsFin of LexFoods = 
    open SyntaxFin, ParadigmsFin in {
  oper
    wine_N = mkN "viini" ;
    pizza_N = mkN "pizza" ;
    cheese_N = mkN "juusto" ;
    fish_N = mkN "kala" ;
    fresh_A = mkA "tuore" ;
    warm_A = mkA 
    (mkN "l‰mmin" "l‰mpim‰n" "l‰mmint‰" "l‰mpim‰n‰" "l‰mpim‰‰n" 
         "l‰mpimin‰" "l‰mpimi‰" "l‰mpimien" "l‰mpimiss‰" "l‰mpimiin"
	 ) 
    "l‰mpim‰mpi" "l‰mpimin" ;
    italian_A = mkA "italialainen" ;
    expensive_A = mkA "kallis" ;
    delicious_A = mkA "herkullinen" ;
    boring_A = mkA "tyls‰" ;
}
