function Is_Perfect(N : Positive) return Boolean is
   Sum : Natural := 0;
begin
   for I in 1..N - 1 loop
      if N mod I = 0 then
         Sum := Sum + I;
      end if;
   end loop;
   return Sum = N;
end Is_Perfect;
