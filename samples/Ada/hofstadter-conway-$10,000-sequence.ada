-- Ada95 version
-- Allocation of arrays on the heap

with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Deallocation;

procedure Conway is

   package Real_io is new Float_IO (Float);

   Maxrange : constant := 2 ** 20;

   type Sequence is array (Positive range 1 .. Maxrange) of Positive;
   type Sequence_Ptr is access all Sequence;
   procedure Free is new Unchecked_Deallocation (Sequence, Sequence_Ptr);

   S : Sequence_Ptr := new Sequence;

   type Ratio_Array is array (Positive range 1 .. Maxrange) of Float;
   type Ratio_Ptr is access all Ratio_Array;
   procedure Free is new Unchecked_Deallocation (Ratio_Array, Ratio_Ptr);

   Ratio : Ratio_Ptr := new Ratio_Array;

   Mallows : Positive;
   M       : Natural := 0;
begin
   S (1) := 1;
   S (2) := 1;
   for K in 3 .. Maxrange loop
      S (K) := S (S (K - 1)) + S (K - S (K - 1));
   end loop;

   for k in 1 .. Maxrange loop
      Ratio (k) := Float (S (k)) / Float (k);
   end loop;

   for N in 1 .. 19 loop
      declare
         Max   : Float := 0.0;
         Where : Positive;
      begin
         for K in 2 ** N .. 2 ** (N + 1) loop
            if Max < Ratio (K) then
               Max   := Ratio (K);
               Where := K;
            end if;
         end loop;
         if (M = 0 and Max < 0.55) then
            M := N - 1;
         end if;
         Put
           ("Maximun of a(n)/n between 2^" &
            Integer'Image (N) &
            " and 2^" &
            Integer'Image (N + 1) &
            " was ");
         Real_io.Put (Max, Fore => 1, Aft => 8, Exp => 0);
         Put_Line (" at" & Integer'Image (Where));
      end;
   end loop;
   --  Calculate Mallows number
   for I in reverse 2 ** M .. 2 ** (M + 1) loop
      if (Ratio (I) > 0.55) then
         Mallows := I;
         exit;
      end if;
   end loop;
   Put_Line ("Mallows number" & Integer'Image (Mallows));
   Free (S);
   Free (Ratio);
end Conway;
