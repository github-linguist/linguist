-- (c) 2009 Aarne Ranta under LGPL

instance LexFoodsFin of LexFoods =
    open SyntaxFin, ParadigmsFin in {
  flags coding=utf8;
  oper
    wine_N = mkN "viini" ;
    pizza_N = mkN "pizza" ;
    cheese_N = mkN "juusto" ;
    fish_N = mkN "kala" ;
    fresh_A = mkA "tuore" ;
    warm_A = mkA
    (mkN "lämmin" "lämpimän" "lämmintä" "lämpimänä" "lämpimään"
         "lämpiminä" "lämpimiä" "lämpimien" "lämpimissä" "lämpimiin"
	 )
    "lämpimämpi" "lämpimin" ;
    italian_A = mkA "italialainen" ;
    expensive_A = mkA "kallis" ;
    delicious_A = mkA "herkullinen" ;
    boring_A = mkA "tylsä" ;
}
