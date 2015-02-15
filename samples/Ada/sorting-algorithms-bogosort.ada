with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Test_Bogosort is
   generic
      type Ordered is private;
      type List is array (Positive range <>) of Ordered;
      with function "<" (L, R : Ordered) return Boolean is <>;
   procedure Bogosort (Data : in out List);

   procedure Bogosort (Data : in out List) is
      function Sorted return Boolean is
      begin
         for I in Data'First..Data'Last - 1 loop
            if not (Data (I) < Data (I + 1)) then
               return False;
            end if;
         end loop;
         return True;
      end Sorted;
      subtype Index is Integer range Data'Range;
      package Dices is new Ada.Numerics.Discrete_Random (Index);
      use Dices;
      Dice : Generator;
      procedure Shuffle is
         J    : Index;
         Temp : Ordered;
      begin
         for I in Data'Range loop
            J := Random (Dice);
            Temp := Data (I);
            Data (I) := Data (J);
            Data (J) := Temp;
         end loop;
      end Shuffle;
   begin
      while not Sorted loop
         Shuffle;
      end loop;
   end Bogosort;

   type List is array (Positive range <>) of Integer;
   procedure Integer_Bogosort is new Bogosort (Integer, List);
   Sequence : List := (7,6,3,9);
begin
   Integer_Bogosort (Sequence);
   for I in Sequence'Range loop
      Put (Integer'Image (Sequence (I)));
   end loop;
end Test_Bogosort;
