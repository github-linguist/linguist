years = {1900, 2100}; months = {1 ,3 ,5 ,7 ,8 ,10 ,12};
result = Select[Tuples[{Range@@years, months}], (DateString[# ~ Join ~ 1, "DayNameShort"] == "Fri")&];

Print[result // Length," months with 5 weekends" ];
Print["First months: ", DateString[#,{"MonthName"," ","Year"}]& /@ result[[1 ;; 5]]];
Print["Last months: " , DateString[#,{"MonthName"," ","Year"}]& /@ result[[-5 ;; All]]];
Print[# // Length, " years without 5 weekend months:\n", #] &@
 Complement[Range @@ years, Part[Transpose@result, 1]];
