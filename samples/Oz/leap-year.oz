declare
  fun {IsLeapYear Year}
     case Year mod 100 of 0 then
	Year mod 400 == 0
     else
	Year mod 4 == 0
     end
  end
in
  for Y in [1900 1996 1997 2000] do
     if {IsLeapYear Y} then
	{System.showInfo Y#" is a leap year."}
     else
	{System.showInfo Y#" is NOT a leap year."}
     end
  end
