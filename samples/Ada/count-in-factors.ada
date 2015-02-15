with Ada.Command_Line;
with Ada.Text_IO;

procedure Count is
   type Number_List is array (Positive range <>) of Positive;

   function Decompose (N : Natural) return Number_List is
      Size : Natural := 0;
      M    : Natural := N;
      K    : Natural := 2;
   begin
      if N = 1 then
         return (1 => 1);
      end if;
      -- Estimation of the result length from above
      while M >= 2 loop
         M    := (M + 1) / 2;
         Size := Size + 1;
      end loop;
      M := N;
      -- Filling the result with prime numbers
      declare
         Result : Number_List (1 .. Size);
         Index  : Positive := 1;
      begin
         while N >= K loop -- Divisors loop
            while 0 = (M mod K) loop -- While divides
               Result (Index) := K;
               Index          := Index + 1;
               M              := M / K;
            end loop;
            K := K + 1;
         end loop;
         return Result (1 .. Index - 1);
      end;
   end Decompose;

   procedure Put (List : Number_List) is
   begin
      for Index in List'Range loop
         Ada.Text_IO.Put (Integer'Image (List (Index)));
         if Index /= List'Last then
            Ada.Text_IO.Put (" x");
         end if;
      end loop;
   end Put;

   N     : Natural := 1;
   Max_N : Natural := 15;
begin
   if Ada.Command_Line.Argument_Count = 1 then
      Max_N := Integer'Value (Ada.Command_Line.Argument (1));
   end if;
   loop
      Ada.Text_IO.Put (Integer'Image (N) & ": ");
      Put (Decompose (N));
      Ada.Text_IO.New_Line;
      N := N + 1;
      exit when N > Max_N;
   end loop;
end Count;
