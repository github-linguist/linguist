concrete FoodsGle of Foods = open MutationsGle, CharactersGle in {
	param Gender = Masc|Fem ;
	param Number = Sg|Pl ;
	param Breadth = Broad|Slender|NoBreadth ;
	
	lincat Comment = Str;
	lin Pred item quality = "tá" ++ item ++ quality.s!Sg!Unmutated ;

	lincat Item = Str;
	lin
		This kind = (addArticleSg kind) ++ "seo" ;
		That kind = (addArticleSg kind) ++ "sin";
		These kind = (addArticlePl kind) ++ "seo" ;
		Those kind = (addArticlePl kind) ++ "sin" ;
	oper addArticleSg : {s : Number => Mutation => Str; g : Gender} -> Str =
					    \kind -> "an" ++ case kind.g of { Masc => kind.s!Sg!PrefixT; Fem => kind.s!Sg!Lenition1DNTLS } ;
	oper addArticlePl : {s : Number => Mutation => Str; g : Gender} -> Str =
					    \kind -> "na" ++ kind.s!Pl!PrefixH ;
	
	lincat Kind = {s : Number => Mutation => Str; g : Gender; pe : Breadth} ;
	lin
		Mod quality kind = {
			s = table{
					Sg => table{mutation => kind.s!Sg!mutation ++ case kind.g of {Masc => quality.s!Sg!Unmutated; Fem => quality.s!Sg!Lenition1} };
					Pl => table{mutation => kind.s!Pl!mutation ++ case kind.pe of {Slender => quality.s!Pl!Lenition1; _ => quality.s!Pl!Unmutated} }
			};
			g = kind.g;
			pe = kind.pe
		} ;
		Wine = makeNoun "fíon" "fíonta" Masc ;
		Cheese = makeNoun "cáis" "cáiseanna" Fem ;
		Fish = makeNoun "iasc" "éisc" Masc ;
		Pizza = makeNoun "píotsa" "píotsaí" Masc ;
	oper makeNoun : Str -> Str -> Gender -> {s : Number => Mutation => Str; g : Gender; pe : Breadth} =
		\sg,pl,g -> {
						s = table{Sg => (mutate sg); Pl => (mutate pl)};
						g = g;
						pe = case pl of {
							_ + v@(#broadVowel) + c@(#consonant*) + #consonant => Broad;
							_ + v@(#slenderVowel) + c@(#consonant*) + #consonant => Slender;
							_ => NoBreadth
						}
					} ;

	lincat Quality = {s : Number => Mutation => Str; sVery : Number => Str} ;
	lin
		Very quality = {s=table{number => table{_ => quality.sVery!number}}; sVery=quality.sVery } ;
		Fresh = makeAdjective "úr" "úra" ;
		Warm = makeAdjective "te" "te" ;
		Italian = makeAdjective "Iodálach" "Iodálacha" ;
		Expensive = makeAdjective "daor" "daora" ;
		Delicious = makeAdjective "blasta" "blasta" ;
		Boring = makeAdjective "leamh" "leamha" ;
	oper makeAdjective : Str -> Str -> {s : Number => Mutation => Str; sVery : Number => Str} =
		\sg,pl -> {
					s=table{Sg => (mutate sg); Pl => (mutate pl)};
					sVery=table{Sg => "an-"+(lenition1dntls sg); Pl => "an-"+(lenition1dntls pl)}
				  } ;
}