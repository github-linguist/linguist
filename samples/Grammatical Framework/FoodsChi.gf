concrete FoodsChi of Foods = {
flags coding = utf8 ;
lincat
    Comment, Item = Str ;
    Kind = {s,c : Str} ; 
    Quality = {s,p : Str} ;
lin
    Pred item quality = item ++ "是" ++ quality.s ++ quality.p ;
    This kind = "这" ++ kind.c ++ kind.s ;
    That kind = "那" ++ kind.c ++ kind.s ;
    These kind = "这" ++ "些" ++ kind.s ;
    Those kind = "那" ++ "些" ++ kind.s ;
    Mod quality kind = {
      s = quality.s ++ quality.p ++ kind.s ;
      c = kind.c
      } ;
    Wine  = geKind "酒" ;
    Pizza = geKind "比 萨 饼" ;
    Cheese  = geKind "奶 酪" ;
    Fish  = geKind "鱼" ;
    Very quality = longQuality ("非 常" ++ quality.s) ;
    Fresh  = longQuality "新 鲜" ;
    Warm  = longQuality "温 热" ;
    Italian  = longQuality "意 大 利 式" ;
    Expensive  = longQuality "昂 贵" ;
    Delicious  = longQuality "美 味" ;
    Boring  = longQuality "难 吃" ;
oper
    mkKind : Str -> Str -> {s,c : Str} = \s,c ->
      {s = s ; c = c} ;
    geKind : Str -> {s,c : Str} = \s ->
      mkKind s "个" ;
    longQuality : Str -> {s,p : Str} = \s ->
      {s = s ; p = "的"} ;
}
