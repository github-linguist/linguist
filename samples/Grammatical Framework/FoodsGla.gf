concrete FoodsGla of Foods = open MutationsGla, CharactersGla, Prelude in {
	param Gender = Masc|Fem ;
	param Number = Sg|Pl ;
	param Breadth = Broad|Slender|NoBreadth ;
	param Beginning = Bcgmp|Other ;
	
	lincat Comment = Str;
	lin Pred item quality = "tha" ++ item ++ quality.s!Sg!Unmutated ;

	lincat Item = Str;
	lin
		This kind = (addArticleSg kind) ++ "seo" ;
		That kind = (addArticleSg kind) ++ "sin";
		These kind = (addArticlePl kind) ++ "seo" ;
		Those kind = (addArticlePl kind) ++ "sin" ;
	oper addArticleSg : {s : Number => Mutation => Str; g : Gender} -> Str =
						  \kind -> case kind.g of { Masc => "an" ++ kind.s!Sg!PrefixT; Fem => "a'" ++ kind.s!Sg!Lenition1DNTLS } ;
	oper addArticlePl : {s : Number => Mutation => Str; g : Gender} -> Str =
					    \kind -> "na" ++ kind.s!Pl!PrefixH ;
	
	oper Noun : Type = {s : Number => Mutation => Str; g : Gender; pe : Breadth; beginning: Beginning; };
	lincat Kind = Noun;
	lin
		Mod quality kind = {
			s = table{
					Sg => table{mutation => kind.s!Sg!mutation ++ case kind.g of {Masc => quality.s!Sg!Unmutated; Fem => quality.s!Sg!Lenition1} };
					Pl => table{mutation => kind.s!Pl!mutation ++ case kind.pe of {Slender => quality.s!Pl!Lenition1; _ => quality.s!Pl!Unmutated} }
			};
			g = kind.g;
			pe = kind.pe;
			beginning = kind.beginning
		} ;
		Wine = makeNoun "fìon" "fìontan" Masc ;
		Cheese = makeNoun "càise" "càisean" Masc ;
		Fish = makeNoun "iasg" "èisg" Masc ;
		Pizza = makeNoun "pizza" "pizzathan" Masc ;
	oper makeNoun : Str -> Str -> Gender -> Noun = \sg,pl,g -> {
		s = table{Sg => (mutate sg); Pl => (mutate pl)};
		g = g;
		pe = pe;
		beginning = Bcgmp
	}
	where {
		pe : Breadth = case pl of {
			_ + v@(#broadVowel) + c@(#consonant*) + #consonant => Broad;
			_ + v@(#slenderVowel) + c@(#consonant*) + #consonant => Slender;
			_ => NoBreadth
		}
	};

	oper Adjective : Type = {s : Number => Mutation => Str; sVery : Number => Str};
	lincat Quality = Adjective;
	lin
		Very quality = {s=table{number => table{_ => quality.sVery!number}}; sVery=quality.sVery } ;
		Fresh = makeAdjective "úr" "ùra" ;
		Warm = makeAdjective "blàth" "blàtha" ;
		Italian = makeAdjective "Eadailteach" "Eadailteach" ;
		Expensive = makeAdjective "daor" "daora" ;
		Delicious = makeAdjective "blasta" "blasta" ;
		Boring = makeAdjective "leamh" "leamha" ;
	oper makeAdjective : Str -> Str -> Adjective =
		\sg,pl -> {
					s=table{Sg => (mutate sg); Pl => (mutate pl)};
					sVery=table{Sg => "glè"++(lenition1dntls sg); Pl => "glè"++(lenition1dntls pl)}
				  } ;
}