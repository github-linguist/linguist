-- (c) 2011 Dinesh Simkhada under LGPL

concrete FoodsNep of Foods = {

  flags coding = utf8 ;

  lincat
    Comment, Quality = {s : Str} ; 
    Kind = {s : Number => Str} ; 
    Item = {s : Str ; n : Number} ; 
    
  lin
    Pred item quality = 
      {s = item.s ++ quality.s ++ copula ! item.n} ;
      
    This  = det Sg "यो" ;
    That  = det Sg "त्यो" ;
    These = det Pl "यी" ;
    Those = det Pl "ती" ;
    Mod quality kind = 
      {s = \\n => quality.s ++ kind.s ! n} ;
      
    Wine = regNoun "रक्सी" ;
    Cheese = regNoun "चिज" ;
    Fish = regNoun "माछा" ;
    Pizza = regNoun "पिज्जा" ;
    Very a = {s = "धेरै" ++ a.s} ;
    Fresh = adj "ताजा" ;
    Warm = adj "तातो" ;
    Italian = adj "इटालियन" ;
    Expensive = adj "महँगो" | adj "बहुमूल्य" ;
    Delicious = adj "स्वादिष्ट" | adj "मीठो" ;
    Boring = adjPl "नमिठो" ;
  
  param
    Number = Sg | Pl ;
    
  oper
    det : Number -> Str -> 
      {s : Number => Str} -> {s : Str ; n : Number} = 
        \n,det,noun -> {s = det ++ noun.s ! n ; n = n} ;
    
    noun : Str -> Str -> {s : Number => Str} = 
      \man,men -> {s = table {Sg => man ; Pl => men}} ;
    
    regNoun : Str -> {s : Number => Str} = 
      \car -> noun car (car + "हरु") ;
    
    adjPl : Str -> {s : Str} = \a -> case a of {
      bor + "ठो" => adj (bor + "ठा") ;
      _ => adj a
      } ;
      
    adj : Str -> {s : Str} = 
      \cold -> {s = cold} ;
    
    copula : Number => Str = 
      table {Sg => "छ" ; Pl => "छन्"} ;
}

