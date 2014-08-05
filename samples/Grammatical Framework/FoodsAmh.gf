concrete FoodsAmh of Foods ={
 flags coding = utf8;
 lincat
  Comment,Item,Kind,Quality = Str;
 lin
  Pred item quality = item ++ quality++ "ነው::" ;
  This kind = "ይህ" ++ kind;
  That kind = "ያ" ++ kind;
  Mod quality kind = quality ++ kind;
  Wine = "ወይን";
  Cheese = "አይብ";
  Fish = "ዓሳ";
  Very quality = "በጣም" ++ quality;
  Fresh = "አዲስ";
  Warm = "ትኩስ";
  Italian = "የጥልያን";
  Expensive = "ውድ";
  Delicious = "ጣፋጭ";
  Boring = "አስቀያሚ";
  
}   