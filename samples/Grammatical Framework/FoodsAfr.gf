-- (c) 2009 Laurette Pretorius Sr & Jr and Ansu Berg under LGPL

concrete FoodsAfr of Foods = open Prelude, Predef in{
	lincat
		Comment = {s: Str} ;
		Kind = {s: Number => Str} ;
		Item = {s: Str ; n: Number} ;
		Quality = {s: AdjAP => Str} ;
		
	lin	
		Pred item quality = {s = item.s ++ "is" ++ (quality.s ! Predic)};
		This kind = {s = "hierdie" ++ (kind.s ! Sg); n = Sg};
		That kind = {s = "daardie" ++ (kind.s ! Sg); n = Sg};
		These kind = {s = "hierdie" ++ (kind.s ! Pl); n = Pl};
		Those kind = {s = "daardie" ++ (kind.s ! Pl); n = Pl};
		Mod quality kind = {s = table{n => (quality.s ! Attr) ++ (kind.s!n)}};
		
		Wine = declNoun_e "wyn";
		Cheese = declNoun_aa "kaas";
		Fish = declNoun_ss "vis";
		Pizza = declNoun_s "pizza";
		
		Very quality = veryAdj quality;
		
		Fresh = regAdj "vars";
		Warm = regAdj "warm";
		Italian = smartAdj_e "Italiaans";
		Expensive = regAdj "duur";
		Delicious = smartAdj_e "heerlik";
		Boring = smartAdj_e "vervelig";
		
	param
		AdjAP = Attr | Predic ;
		Number = Sg | Pl ;
	
	oper
		--Noun operations (wyn, kaas, vis, pizza)
		
		declNoun_aa: Str -> {s: Number => Str} = \x ->
		let v = tk 2 x
		in
		{s = table{Sg => x ; Pl => v + (last x) +"e"}}; 
		
		declNoun_e: Str -> {s: Number => Str} = \x -> {s = table{Sg => x ; Pl => x + "e"}} ;
		declNoun_s: Str -> {s: Number => Str} = \x -> {s = table{Sg => x ; Pl => x + "s"}} ;
		
		declNoun_ss: Str -> {s: Number => Str} = \x -> {s = table{Sg => x ; Pl => x + (last x) + "e"}} ;

		
		--Adjective operations
		
		mkAdj : Str -> Str -> {s: AdjAP => Str} = \x,y -> {s = table{Attr => x; Predic => y}};
		
		declAdj_e : Str -> {s : AdjAP=> Str} = \x -> mkAdj (x + "e") x;
		declAdj_g : Str -> {s : AdjAP=> Str} = \w ->
			let v = init w
			in mkAdj (v + "ë") w ;
		
		declAdj_oog : Str -> {s : AdjAP=> Str} = \w ->
			let v = init w
			in 
				let i = init v
				in mkAdj (i + "ë") w ;
					
		regAdj : Str -> {s : AdjAP=> Str} = \x -> mkAdj x x;
		
		veryAdj : {s: AdjAP => Str} -> {s : AdjAP=> Str} =  \x -> {s = table{a => "baie" ++ (x.s!a)}};
		
		
		smartAdj_e : Str -> {s : AdjAP=> Str} = \a -> case a of 
		{
			_ + "oog" 			   		=> declAdj_oog a ;
			_ + ("e" | "ie" | "o" | "oe") + "g" 	=> declAdj_g a ;
			_					   	=> declAdj_e a 
		};
}
