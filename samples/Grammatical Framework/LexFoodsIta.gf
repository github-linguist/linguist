-- (c) 2009 Aarne Ranta under LGPL

instance LexFoodsIta of LexFoods = 
    open SyntaxIta, ParadigmsIta in {
  oper
    wine_N = mkN "vino" ;
    pizza_N = mkN "pizza" ;
    cheese_N = mkN "formaggio" ;
    fish_N = mkN "pesce" ;
    fresh_A = mkA "fresco" ;
    warm_A = mkA "caldo" ;
    italian_A = mkA "italiano" ;
    expensive_A = mkA "caro" ;
    delicious_A = mkA "delizioso" ;
    boring_A = mkA "noioso" ;
}
