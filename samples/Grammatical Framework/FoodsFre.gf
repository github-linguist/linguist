--# -path=.:../foods:present

concrete FoodsFre of Foods = open SyntaxFre, ParadigmsFre in {

	flags coding = utf8 ;
	
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
		
		Wine = mkCN (mkN "vin" masculine) ;
		Pizza = mkCN (mkN "pizza" feminine) ;
		Cheese = mkCN (mkN "fromage" masculine) ;
		Fish = mkCN (mkN "poisson" masculine) ;
		Fresh = mkAP (mkA "frais" "fraîche" "frais" "fraîchement") ;
		Warm = mkAP (mkA "chaud") ;
		Italian = mkAP (mkA "italien") ;
		Expensive = mkAP (mkA "cher") ;
		Delicious = mkAP (mkA "délicieux") ;
		Boring = mkAP (mkA "ennuyeux") ;
	}