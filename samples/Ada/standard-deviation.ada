with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;

procedure Test_Deviation is
   type Sample is record
      N       : Natural := 0;
      Mean    : Float := 0.0;
      Squares : Float := 0.0;
   end record;
   procedure Add (Data : in out Sample; Point : Float) is
   begin
      Data.N       := Data.N + 1;
      Data.Mean    := Data.Mean    + Point;
      Data.Squares := Data.Squares + Point ** 2;
   end Add;
   function Deviation (Data : Sample) return Float is
   begin
      return Sqrt (Data.Squares / Float (Data.N) - (Data.Mean / Float (Data.N)) ** 2);
   end Deviation;

   Data : Sample;
   Test : array (1..8) of Float := (2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0);
begin
   for Item in Test'Range loop
      Add (Data, Test (Item));
   end loop;
   Put_Line ("Deviation" & Float'Image (Deviation (Data)));
end Test_Deviation;
