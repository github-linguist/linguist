Eratosthenes := function(n)
   local sieve, cur, mul;
   sieve := [1 .. n]*0;
   sieve[1] := 1;
   cur := 1;
   while cur <= n do
      if sieve[cur] = 0 then
         mul := cur*cur;
         while mul <= n do
            sieve[mul] := 1;
            mul := mul + cur;
         od;
      fi;
      cur := cur + 1;
   od;
   return Filtered([1 .. n], x -> sieve[x] = 0);
end;

Eratosthenes(100);
# [ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97 ]
