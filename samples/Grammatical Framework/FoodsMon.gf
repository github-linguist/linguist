--# -path=.:/GF/lib/src/prelude

-- (c) 2009 Nyamsuren Erdenebadrakh under LGPL

concrete FoodsMon of Foods = open Prelude in {
  flags coding=utf8;

  lincat
    Comment, Quality = SS ; 
	Kind = {s : Number => Str} ; 
    Item = {s : Str ; n : Number} ; 

  lin
	Pred item quality = ss (item.s ++ "бол" ++ quality.s) ;
	This  = det Sg "энэ" ;
	That  = det Sg "тэр" ;
	These = det Pl "эдгээр" ;
	Those = det Pl "тэдгээр" ;
	Mod quality kind = {s = \\n => quality.s ++ kind.s ! n} ;
	Wine = regNoun "дарс" ;
	Cheese = regNoun "бяслаг" ;
	Fish = regNoun "загас" ;
	Pizza = regNoun "пицца" ;
	Very = prefixSS "маш" ;
	Fresh = ss "шинэ" ;
	Warm = ss "халуун" ;
	Italian = ss "итали" ;
	Expensive = ss "үнэтэй" ;
	Delicious = ss "амттай" ;
	Boring = ss "амтгүй" ;

  param
    Number = Sg | Pl ;

  oper
	det : Number -> Str -> {s : Number => Str} -> {s : Str ; n : Number} = 
	\n,d,cn -> {
		s = d ++ cn.s ! n ;
		n = n
	} ;
  	
	regNoun : Str -> {s : Number => Str} = 
	\x -> {s = table {
		Sg => x ; 
		Pl => x + "нууд"}
		} ;
	}
	 
    
