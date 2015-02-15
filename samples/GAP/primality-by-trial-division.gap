IsPrimeTrial := function(n)
   local k, m;
   if n < 5 then
      return (n = 2) or (n = 3);
   fi;
   if RemInt(n, 2) = 0 then
      return false;
   fi;
   m := RootInt(n);
   k := 3;
   while k <= m do
      if RemInt(n, k) = 0 then
         return false;
      fi;
      k := k + 2;
   od;
   return true;
end;

Filtered([1 .. 100], IsPrimeTrial);
# [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
