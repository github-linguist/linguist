declare
  Pow5432 = {Pow 5 {Pow 4 {Pow 3 2}}}
  S = {Int.toString Pow5432}
  Len = {Length S}
in
  {System.showInfo
   {List.take S 20}#"..."#
   {List.drop S Len-20}#" ("#Len#" Digits)"}
