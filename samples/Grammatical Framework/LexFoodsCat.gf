-- (c) 2009 Jordi Saludes under LGPL

instance LexFoodsCat of LexFoods = 
    open SyntaxCat, ParadigmsCat, (M = MorphoCat) in {
  flags
	coding = utf8 ;
  oper
    wine_N = mkN "vi" "vins" M.Masc ;
    pizza_N = mkN "pizza" ;
    cheese_N = mkN "formatge" ;
    fish_N = mkN "peix" "peixos" M.Masc;
    fresh_A = mkA "fresc" "fresca" "frescos" "fresques" "frescament";
    warm_A = mkA "calent" ;
    italian_A = mkA "italià" "italiana" "italians" "italianes" "italianament" ;
    expensive_A = mkA "car" ;
    delicious_A = mkA "deliciós" "deliciosa" "deliciosos" "delicioses" "deliciosament";
    boring_A = mkA "aburrit" "aburrida" "aburrits" "aburrides" "aburridament" ;
}
