-- (c) 2009 Aarne Ranta under LGPL

instance LexFoodsSwe of LexFoods = 
    open SyntaxSwe, ParadigmsSwe in {
  oper
    wine_N = mkN "vin" "vinet" "viner" "vinerna" ;
    pizza_N = mkN "pizza" ;
    cheese_N = mkN "ost" ;
    fish_N = mkN "fisk" ;
    fresh_A = mkA "färsk" ;
    warm_A = mkA "varm" ;
    italian_A = mkA "italiensk" ;
    expensive_A = mkA "dyr" ;
    delicious_A = mkA "läcker" ;
    boring_A = mkA "tråkig" ;
}
