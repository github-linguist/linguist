--# -path=.:alltenses

concrete FoodsTha of Foods = open SyntaxTha, LexiconTha,
  ParadigmsTha, (R=ResTha) in {

        flags coding = utf8 ;

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
		Wine = mkCN (mkN (R.thword "เหล้าอ" "งุ่น") "ขวด") ;
                Pizza = mkCN (mkN (R.thword "พิซ" "ซา") "ถาด") ;
                Cheese = mkCN (mkN (R.thword "เนย" "แข็ง") "ก้อน") ;
                Fish = mkCN fish_N ;
                Fresh = mkAP (mkA "สด") ;
                Warm = mkAP warm_A ;
                Italian = mkAP (mkA " อิตาลี") ;
                Expensive = mkAP (mkA "แพง") ;
                Delicious = mkAP (mkA "อร่อย") ;
                Boring = mkAP (mkA (R.thword "น่า" "เบิ่อ")) ;

}
