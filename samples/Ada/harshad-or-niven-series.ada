with Ada.Text_IO;

procedure Harshad is

   function Next(N: in out Positive) return Positive is

      function Sum_Of_Digits(N: Natural) return Natural is
	 ( if N = 0 then 0 else ((N mod 10) + Sum_Of_Digits(N / 10)) );
	
   begin
      while not (N mod Sum_Of_Digits(N) = 0) loop
	 N := N + 1;
      end loop;
      return N;
   end Next;

   Current: Positive := 1;

begin
   for I in 1 .. 20 loop
      Ada.Text_IO.Put(Integer'Image(Next(Current)));
      Current := Current + 1;
   end loop;
   Current := 1000 + 1;
   Ada.Text_IO.Put_Line(" ..." & Integer'Image(Next(Current)));
end Harshad;
