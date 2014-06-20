{-
  File      : FoodsTur.gf
  Author    : Server Çimen
  Version   : 1.0
  Created on: August 26, 2009

  This file contains concrete grammar of Foods abstract grammar for Turkish Language.
  This grammar is to be used for Fridge demo and developed in the scope of GF Resource
  Grammar Summer School.

-}

concrete FoodsTur of Foods = open Predef in {
  flags
    coding=utf8 ;
  lincat
    Comment = {s : Str} ;
    Quality = {s : Str ; c : Case; softness : Softness; h : Harmony} ;
    Kind = {s : Case => Number => Str} ;
    Item = {s : Str; n : Number} ;
  lin
    This = det Sg "bu" ;
    That = det Sg "şu" ;
    These = det Pl "bu" ;
    Those = det Pl "şu" ;
    -- Reason for excluding plural form of copula: In Turkish if subject is not a human being,
    -- then singular form of copula is used regardless of the number of subject. Since all
    -- possible subjects are non human, copula do not need to have plural form.
    Pred item quality = {s = item.s ++ quality.s ++ "&+" ++ copula ! quality.softness ! quality.h} ;--! item.n} ;
    Mod quality kind = {s = case quality.c of {
                              Nom => \\t,n => quality.s ++ kind.s ! t ! n ;
                              Gen => \\t,n => quality.s ++ kind.s ! Gen ! n
                            }
                       } ;
    Wine = mkN "şarap" "şaraplar" "şarabı" "şarapları" ;
    Cheese = mkN "peynir" "peynirler" "peyniri" "peynirleri" ;
    Fish = mkN "balık" "balıklar" "balığı" "balıkları" ;
    Pizza = mkN "pizza" "pizzalar" "pizzası" "pizzaları" ;
    Very a = {s = "çok" ++ a.s ; c = a.c; softness = a.softness; h = a.h} ;
    Fresh = adj "taze" Nom;
    Warm = adj "ılık" Nom;
    Italian = adj "İtalyan" Gen ;
    Expensive = adj "pahalı" Nom;
    Delicious = adj "lezzetli" Nom;
    Boring = adj "sıkıcı" Nom;
  param
    Number = Sg | Pl ;
    Case = Nom | Gen ;
    Harmony = I_Har | Ih_Har | U_Har | Uh_Har ; --Ih = İ; Uh = Ü
    Softness = Soft | Hard ;
  oper
    det : Number -> Str -> {s : Case => Number => Str} -> {s : Str; n : Number} =
      \num,det,noun -> {s = det ++ noun.s ! Nom ! num; n = num} ;
    mkN = overload {
      mkN : Str -> Str -> {s : Case => Number => Str} = regNoun ;
      mkn : Str -> Str -> Str -> Str-> {s : Case => Number => Str} = noun ;
    } ;
    regNoun : Str -> Str -> {s : Case => Number => Str} =
      \peynir,peynirler -> noun peynir peynirler [] [] ;
    noun : Str -> Str -> Str -> Str-> {s : Case => Number => Str} = 
      \sarap,saraplar,sarabi,saraplari -> {
        s = table {
	      Nom =>  table {
			Sg => sarap ;
			Pl => saraplar
	              } ;
              Gen =>  table {
                        Sg => sarabi ;
			Pl => saraplari
	              }
              }
      };
  {-
    Since there is a bug in overloading, this overload is useless.

    mkA = overload {
      mkA : Str -> {s : Str; c : Case; softness : Softness; h : Harmony} = \base -> adj base Nom ;
      mkA : Str -> Case -> {s : Str; c : Case; softness : Softness; h : Harmony} = adj ;
    } ;
  -}
    adj : Str -> Case -> {s : Str; c : Case; softness : Softness; h : Harmony} =
      \italyan,ca -> {s = italyan ; c = ca; softness = (getSoftness italyan); h = (getHarmony italyan)} ;
    -- See the comment at lines 26 and 27 for excluded plural form of copula.
    copula : Softness => Harmony {-=> Number-} => Str =
      table {
        Soft => table {
                  I_Har => "dır" ;--table {
			   -- Sg => "dır" ;
			   -- Pl => "dırlar"
		  --} ;
		  Ih_Har => "dir" ;--table {
			    --Sg => "dir" ;
			    --Pl => "dirler"
		  --} ;
		  U_Har => "dur" ;--table {
			   -- Sg => "dur" ;
			   -- Pl => "durlar"
		  --} ;
		  Uh_Har => "dür" --table {
			    --Sg => "dür" ;
			    --Pl => "dürler"
		  --}
        } ;
        Hard => table {
                  I_Har => "tır" ;--table {
			    --Sg => "tır" ;
			    --Pl => "tırlar"
		  --} ;
		  Ih_Har => "tir" ;--table {
			    --Sg => "tir" ;
			    --Pl => "tirler"
		  --} ;
		  U_Har => "tur" ;--table {
			   -- Sg => "tur" ;
			   -- Pl => "turlar"
		  --} ;
		  Uh_Har => "tür"--table {
			    --Sg => "tür" ;
			    --Pl => "türler"
		  --}
        }
      } ;

    getHarmony : Str -> Harmony
      = \base -> case base of {
                 _+c@("ı"|"a"|"i"|"e"|"u"|"o"|"ü"|"ö")+
                 ("b"|"v"|"d"|"z"|"j"|"c"|"g"|"ğ"|"l"|"r"|"m"|"n"|"y"|"p"|"f"|"t"|"s"|"ş"|"ç"|"k"|"h")* =>
                    case c of {
                      ("ı"|"a") => I_Har ;
                      ("i"|"e") => Ih_Har ;
                      ("u"|"o") => U_Har  ;
                      ("ü"|"ö") => Uh_Har
                    }
        } ;
    getSoftness : Str -> Softness
      = \base -> case base of {
		   _+("f"|"s"|"t"|"k"|"ç"|"ş"|"h"|"p") => Hard ;
		   _ => Soft
		 } ;
}