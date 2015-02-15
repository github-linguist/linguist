c = {"April", "Tam O'Shanter","Emily"};
r = {"Bubbly:I'm > Tam and <= Emily" ,
StringReplace["Burns:\"When chapman billies leave the street ...\"", "\"" -> ""], "Short & shrift"};

ExportString[ XMLElement[  "CharacterRemarks", {},
{XMLElement["Character", {"name" -> c[[1]]}, {r[[1]]}],
 XMLElement["Character", {"name" -> c[[2]]}, {r[[2]]}],
 XMLElement["Character", {"name" -> c[[3]]}, {r[[3]]}]
}], "XML", "AttributeQuoting" -> "\""]
