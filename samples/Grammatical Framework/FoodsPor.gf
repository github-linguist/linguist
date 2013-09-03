-- (c) 2009 Rami Shashati under LGPL

concrete FoodsPor of Foods = open Prelude in {
	lincat
	  Comment = {s : Str} ;
	  Quality = {s : Gender => Number => Str} ;
	  Kind = {s : Number => Str ; g : Gender} ;
	  Item = {s : Str ; n : Number ; g : Gender } ;
	
	lin 
	  Pred item quality =
	    {s = item.s ++ copula ! item.n ++ quality.s ! item.g ! item.n } ;
	  This = det Sg (table {Masc => "este" ; Fem => "esta"}) ;
	  That = det Sg (table {Masc => "esse" ; Fem => "essa"}) ;
	  These = det Pl (table {Masc => "estes" ; Fem => "estas"}) ; 
	  Those = det Pl (table {Masc => "esses" ; Fem => "essas"}) ;
	 
	  Mod quality kind = { s = \\n => kind.s ! n ++ quality.s ! kind.g ! n ; g = kind.g } ;
	  
	  Wine = regNoun "vinho" Masc ;
	  Cheese = regNoun "queijo" Masc ;
	  Fish = regNoun "peixe" Masc ;
	  Pizza = regNoun "pizza" Fem ;
	  
      Very a = { s = \\g,n => "muito" ++ a.s ! g ! n } ;
      
      Fresh = mkAdjReg "fresco" ;
      Warm = mkAdjReg "quente" ;
      Italian = mkAdjReg "Italiano" ;
      Expensive = mkAdjReg "caro" ;
      Delicious = mkAdjReg "delicioso" ;
      Boring = mkAdjReg "chato" ;
	
	param
	  Number = Sg | Pl ;
	  Gender = Masc | Fem ;
	
	oper
	  QualityT : Type = {s : Gender => Number => Str} ;
	  
	  mkAdj : (_,_,_,_ : Str) -> QualityT = \bonito,bonita,bonitos,bonitas -> {
	  	s = table {
	  		Masc => table { Sg => bonito ; Pl => bonitos } ;
	  		Fem => table { Sg => bonita ; Pl => bonitas }
	    } ;
	  } ;
	  
	  -- regular pattern
	  adjSozinho : Str -> QualityT = \sozinho ->
        let sozinh = Predef.tk 1 sozinho
        in mkAdj sozinho (sozinh + "a") (sozinh + "os") (sozinh + "as") ;

      -- for gender-independent adjectives
      adjUtil : Str -> Str -> QualityT = \util,uteis ->
        mkAdj util util uteis uteis ;

      -- smart paradigm for adjcetives
      mkAdjReg : Str -> QualityT = \a -> case last a of {
      	"o" => adjSozinho a ;
        "e" => adjUtil a (a + "s")
      } ;
      
      ItemT : Type = {s : Str ; n : Number ; g : Gender } ;
      
      det : Number -> (Gender => Str) -> KindT -> ItemT =
        \num,det,noun -> {s = det ! noun.g ++ noun.s ! num ; n = num ; g = noun.g } ;
	    
	  KindT : Type = {s : Number => Str ; g : Gender} ;
	    
	  noun : Str -> Str -> Gender -> KindT =
	    \animal,animais,gen -> {s = table {Sg => animal ; Pl => animais} ; g = gen } ;
	    
	  regNoun : Str -> Gender -> KindT =
	    \carro,gen -> noun carro (carro + "s") gen ;
	  
	  copula : Number => Str = table {Sg => "é" ; Pl => "são"} ;
}
