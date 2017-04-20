--# -path=.:present

concrete FoodsSpa of Foods = open SyntaxSpa, StructuralSpa, ParadigmsSpa in {

	lincat
		Comment = Utt ;
		Item = NP ;
		Kind = CN ;
		Quality = AP ;

	lin
		Pred item quality = mkUtt (mkCl item quality) ;
		This kind = mkNP this_QuantSg kind ;
		That kind = mkNP that_QuantSg kind ;
		These kind = mkNP these_QuantPl kind ;
		Those kind = mkNP those_QuantPl kind ;
		Mod quality kind = mkCN quality kind ;
		Very quality = mkAP very_AdA quality ;
		Wine = mkCN (mkN "vino") ;
                Pizza = mkCN (mkN "pizza") ;
                Cheese = mkCN (mkN "queso") ;
                Fish = mkCN (mkN "pescado") ;
                Fresh = mkAP (mkA "fresco") ;
                Warm = mkAP (mkA "caliente") ;
                Italian = mkAP (mkA "italiano") ;
                Expensive = mkAP (mkA "caro") ;
                Delicious = mkAP (mkA "delicioso") ;
                Boring = mkAP (mkA "aburrido") ;

}

