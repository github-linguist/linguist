with Ada.Text_IO;                 use Ada.Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Numerics.Complex_Types;  use Ada.Numerics.Complex_Types;

procedure Roots_Of_Unity is
   Root : Complex;
begin
   for N in 2..10 loop
      Put_Line ("N =" & Integer'Image (N));
      for K in 0..N - 1 loop
         Root :=
             Compose_From_Polar
             (  Modulus  => 1.0,
                Argument => Float (K),
                Cycle    => Float (N)
             );
            -- Output
         Put ("   k =" & Integer'Image (K) & ", ");
         if Re (Root) < 0.0 then
            Put ("-");
         else
            Put ("+");
         end if;
         Put (abs Re (Root), Fore => 1, Exp => 0);
         if Im (Root) < 0.0 then
            Put ("-");
         else
            Put ("+");
         end if;
         Put (abs Im (Root), Fore => 1, Exp => 0);
         Put_Line ("i");
      end loop;
   end loop;
end Roots_Of_Unity;
