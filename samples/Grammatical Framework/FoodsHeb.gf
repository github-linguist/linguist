--# -path=alltenses

--(c) 2009 Dana Dannells
-- Licensed under LGPL

concrete FoodsHeb of Foods = open Prelude in {
  
  flags coding=utf8 ;

    lincat
      Comment = SS ;
      Quality = {s: Number => Species => Gender =>  Str} ;  
      Kind = {s : Number => Species => Str ; g : Gender ; mod : Modified} ; 
      Item = {s : Str ; g : Gender ; n : Number ; sp : Species ; mod : Modified} ; 
  
 
    lin
      Pred item quality = ss (item.s ++ quality.s ! item.n ! Indef ! item.g ) ; 
      This  = det Sg Def "הזה" "הזאת"; 
      That  = det Sg Def "ההוא" "ההיא" ; 
      These = det Pl Def "האלה" "האלה" ; 
      Those = det Pl Def "ההם" "ההן" ; 
      Mod quality kind = {
	s = \\n,sp => kind.s ! n ! sp ++ quality.s ! n ! sp ! kind.g;
	g = kind.g ;
	mod = T
	} ;     
      Wine = regNoun "יין" "יינות" Masc ; 
      Cheese = regNoun "גבינה" "גבינות" Fem ;  
      Fish = regNoun "דג" "דגים" Masc ; 
      Pizza = regNoun "פיצה" "פיצות" Fem ; 
      Very qual = {s = \\g,n,sp => "מאוד" ++  qual.s ! g ! n ! sp} ;
      Fresh = regAdj "טרי" ; 
      Warm = regAdj "חם" ;
      Italian = regAdj2 "איטלקי" ;
      Expensive = regAdj "יקר" ; 
      Delicious = regAdj "טעים" ; 
      Boring = regAdj2 "משעמם"; 

    param 
      Number = Sg | Pl ;
      Gender = Masc | Fem ;
      Species = Def | Indef ;  
      Modified = T | F ;

    oper
	Noun : Type = {s : Number => Species => Str ; g : Gender ; mod : Modified } ;
  	Adj : Type = {s : Number => Species => Gender => Str} ;

      det : Number -> Species -> Str -> Str -> Noun -> 
	{s : Str ; g :Gender ; n : Number ; sp : Species ; mod : Modified} = 
        \n,sp,m,f,cn -> {
	  s = case cn.mod of { _ => cn.s ! n ! sp ++ case cn.g of {Masc => m ; Fem  => f} };
	  g = cn.g ; 
          n = n ;
	  sp = sp ;
	  mod = cn.mod
        } ;
      
	noun : (gvina,hagvina,gvinot,hagvinot : Str) ->  Gender -> Noun =  
      		\gvina,hagvina,gvinot,hagvinot,g -> {
        	s = table {
          		Sg  => table {
             			Indef => gvina ;
             			Def =>  hagvina 
            		} ;
          		Pl => table {
             			Indef => gvinot ;
             			Def => hagvinot  
            		} 
       	  	} ;
        	g = g ;
		mod = F 
      } ;

	regNoun : Str -> Str -> Gender -> Noun = 
	        \gvina,gvinot, g -> 
		noun gvina (defH gvina) gvinot (defH gvinot) g ; 

      defH : Str -> Str = \cn ->
	case cn of {_ => "ה" + cn};	

      replaceLastLetter : Str -> Str = \c ->
	 case c of {"ף" => "פ" ; "ם" => "מ" ; "ן" => "נ" ; "ץ" => "צ" ; "ך" => "כ"; _ => c} ;
	      
      adjective : (_,_,_,_ : Str) -> Adj = 
       \tov,tova,tovim,tovot -> {
        s = table {
          Sg => table { 
			Indef => table { Masc => tov ; Fem => tova } ; 
			Def => table { Masc => defH tov ; Fem => defH tova }  
            		} ; 
          Pl => table { 
			Indef => table {Masc => tovim ; Fem  => tovot } ; 
			Def => table { Masc => defH tovim ; Fem  => defH tovot }  
            		}
	}
      } ;
    
      regAdj : Str -> Adj = \tov ->
	case tov of { to + c@? =>
	adjective tov (to + replaceLastLetter (c) + "ה" ) (to + replaceLastLetter (c) +"ים" ) (to + replaceLastLetter (c) + "ות" )};	 
     
     regAdj2 : Str -> Adj = \italki ->
 	case italki of { italk+ c@? => 
    adjective italki (italk + replaceLastLetter (c)  +"ת" )  (italk + replaceLastLetter (c)+ "ים" ) (italk + replaceLastLetter (c)  + "ות" )};

}  -- FoodsHeb  
