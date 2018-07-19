IsLeapYear := function(n)
  return (n mod 4 = 0) and ((n mod 100 <> 0) or (n mod 400 = 0));
end;

# alternative using built-in function
IsLeapYear := function(n)
  return DaysInYear(n) = 366;
end;
