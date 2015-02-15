with Ada.Text_IO; use Ada.Text_IO;
procedure Mfact is

   function MultiFact (num : Natural; deg : Positive) return Natural is
      Result, N : Integer := num;
   begin
      if N = 0 then return 1; end if;
      loop
         N := N - deg; exit when N <= 0; Result := Result * N;
      end loop; return Result;
   end MultiFact;

begin
   for deg in 1..5 loop
      Put("Degree"& Integer'Image(deg) &":");
      for num in 1..10 loop Put(Integer'Image(MultiFact(num,deg))); end loop;
      New_line;
   end loop;
end Mfact;
