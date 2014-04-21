-- (c) 2009 Femke Johansson under LGPL

concrete FoodsDut of Foods = {

	lincat
		Comment = {s : Str};
		Quality = {s : AForm => Str};
		Kind = { s : Number => Str};
		Item = {s : Str ; n : Number};
	
	lin
		Pred item quality = 
			{s = item.s ++ copula ! item.n ++ quality.s ! APred};
		This = det Sg "deze";
		These = det Pl "deze";
		That = det Sg "die";
		Those = det Pl "die";
		
		Mod quality kind =
			{s = \\n => quality.s ! AAttr ++ kind.s ! n};
			Wine = regNoun "wijn";
			Cheese = noun "kaas" "kazen";
			Fish = noun "vis" "vissen";
			Pizza = noun "pizza" "pizza's";
			
			Very a = {s = \\f => "erg" ++ a.s ! f};
			
			Fresh = regadj "vers";
			Warm = regadj "warm";
			Italian = regadj "Italiaans";
			Expensive = adj "duur" "dure";
			Delicious = regadj "lekker";
			Boring = regadj "saai";
		
		param
			Number = Sg | Pl;
			AForm = APred | AAttr;
		
		oper
			det : Number -> Str ->
				{s : Number => Str} -> {s : Str ; n: Number} =
				\n,det,noun -> {s = det ++ noun.s ! n ; n=n};
				
			noun : Str -> Str -> {s : Number => Str} = 
				\man,men -> {s = table {Sg => man; Pl => men}};
				
			regNoun : Str -> {s : Number => Str} =
				\wijn -> noun wijn (wijn + "en");
				
			regadj : Str -> {s : AForm => Str} =
				\koud -> adj koud (koud+"e");
			
			adj : Str -> Str -> {s : AForm => Str} =
				\duur, dure -> {s = table {APred => duur; AAttr => dure}};
				
			copula : Number => Str =
				table {Sg => "is" ; Pl => "zijn"};
}
