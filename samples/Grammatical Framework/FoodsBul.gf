-- (c) 2009 Krasimir Angelov under LGPL

concrete FoodsBul of Foods = {
  
  flags
    coding = utf8;

  param
    Gender = Masc | Fem | Neutr;
    Number = Sg | Pl;
    Agr    = ASg Gender | APl ;

  lincat
    Comment = Str ;
    Quality = {s : Agr => Str} ;
    Item = {s : Str; a : Agr} ;
    Kind = {s : Number => Str; g : Gender} ;

  lin
    Pred item qual = item.s ++ case item.a of {ASg _ => "е"; APl => "са"} ++ qual.s ! item.a ;
    
    This  kind = {s=case kind.g of {Masc=>"този"; Fem=>"тази";  Neutr=>"това" } ++ kind.s ! Sg; a=ASg kind.g} ;
    That  kind = {s=case kind.g of {Masc=>"онзи"; Fem=>"онази"; Neutr=>"онова"} ++ kind.s ! Sg; a=ASg kind.g} ;
    These kind = {s="тези"  ++ kind.s ! Pl; a=APl} ;
    Those kind = {s="онези" ++ kind.s ! Pl; a=APl} ;
    
    Mod qual kind = {s=\\n => qual.s ! (case n of {Sg => ASg kind.g; Pl => APl}) ++ kind.s ! n; g=kind.g} ;

    Wine   = {s = table {Sg => "вино";   Pl => "вина"};   g = Neutr};
    Cheese = {s = table {Sg => "сирене"; Pl => "сирена"}; g = Neutr};
    Fish   = {s = table {Sg => "риба";   Pl => "риби"};   g = Fem};
    Pizza  = {s = table {Sg => "пица";   Pl => "пици"};   g = Fem};

    Very qual = {s = \\g => "много" ++ qual.s ! g};

    Fresh     = {s = table {ASg Masc => "свеж";        ASg Fem => "свежа";       ASg Neutr => "свежо";       APl => "свежи"}};
    Warm      = {s = table {ASg Masc => "горещ";       ASg Fem => "гореща";      ASg Neutr => "горещо";      APl => "горещи"}};
    Italian   = {s = table {ASg Masc => "италиански";  ASg Fem => "италианска";  ASg Neutr => "италианско";  APl => "италиански"}}; 
    Expensive = {s = table {ASg Masc => "скъп";        ASg Fem => "скъпа";       ASg Neutr => "скъпо";       APl => "скъпи"}};
    Delicious = {s = table {ASg Masc => "превъзходен"; ASg Fem => "превъзходна"; ASg Neutr => "превъзходно"; APl => "превъзходни"}};
    Boring    = {s = table {ASg Masc => "еднообразен"; ASg Fem => "еднообразна"; ASg Neutr => "еднообразно"; APl => "еднообразни"}};
    
}
