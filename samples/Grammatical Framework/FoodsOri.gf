concrete FoodsOri of Foods = {

flags coding = utf8 ;

lincat
    Comment = Str;
    Item = Str;
    Kind = Str;
    Quality = Str;

lin
    Pred item quality = item ++ quality ++ "ଅଟେ";
    This kind = "ଏଇ" ++ kind;
    That kind = "ସେଇ" ++ kind;
    These kind = "ଏଇ" ++ kind ++ "ଗୁଡିକ" ;
    Those kind = "ସେଇ" ++ kind ++ "ଗୁଡିକ" ;
    Mod quality kind = quality ++ kind;
    Wine  = "ମଦ";
    Cheese  = "ଛେନା";
    Fish  = "ମାଛ";
    Pizza = "ପିଜଜ଼ା" ;
    Very quality = "ଅତି" ++ quality;
    Fresh  = "ତାଜା";
    Warm  = "ଗରମ";
    Italian  = "ଇଟାଲି";
    Expensive  = "ମୁଲ୍ୟବାନ୍";
    Delicious  = "ସ୍ଵାଦିସ୍ଟ ";
    Boring  = "ଅରୁଚିକର";

}
