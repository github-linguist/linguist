-- (c) 2009 Aarne Ranta under LGPL

incomplete concrete FoodsI of Foods = 
    open Syntax, LexFoods in {
  lincat
    Comment = Utt ; 
    Item = NP ;
    Kind = CN ;
    Quality = AP ;
  lin
    Pred item quality = mkUtt (mkCl item quality) ;
    This kind = mkNP this_Det kind ;
    That kind = mkNP that_Det kind ;
    These kind = mkNP these_Det kind ;
    Those kind = mkNP those_Det kind ;
    Mod quality kind = mkCN quality kind ;
    Very quality = mkAP very_AdA quality ;

    Wine = mkCN wine_N ;
    Pizza = mkCN pizza_N ;
    Cheese = mkCN cheese_N ;
    Fish = mkCN fish_N ;
    Fresh = mkAP fresh_A ;
    Warm = mkAP warm_A ;
    Italian = mkAP italian_A ;
    Expensive = mkAP expensive_A ;
    Delicious = mkAP delicious_A ;
    Boring = mkAP boring_A ;
}
