with Ada.Numerics.Discrete_Random, Ada.Text_IO;

procedure Naive_Random is

   type M_1000 is mod 1000;
   package Rand is new Ada.Numerics.Discrete_Random(M_1000);
   Gen: Rand.Generator;

   procedure Perform(Modulus: Positive; Expected, Margin: Natural;
                    Passed: out boolean) is
      Low:  Natural  := (100-Margin) * Expected/100;
      High: Natural  := (100+Margin) * Expected/100;
      Buckets: array(0 .. Modulus-1) of Natural := (others => 0);
      Index: Natural;
   begin
      for I in 1 .. Expected * Modulus loop
         Index := Integer(Rand.Random(Gen)) mod Modulus;
         Buckets(Index) := Buckets(Index) + 1;
      end loop;
      Passed := True;
      for I in Buckets'Range loop
         Ada.Text_IO.Put("Bucket" & Integer'Image(I+1) & ":" &
                           Integer'Image(Buckets(I)));
         if Buckets(I) < Low or else Buckets(I) > High then
            Ada.Text_IO.Put_Line(" (failed)");
            Passed := False;
         else
            Ada.Text_IO.Put_Line(" (passed)");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Perform;

   Number_Of_Buckets: Positive := Natural'Value(Ada.Text_IO.Get_Line);
   Expected_Per_Bucket: Natural := Natural'Value(Ada.Text_IO.Get_Line);
   Margin_In_Percent: Natural := Natural'Value(Ada.Text_IO.Get_Line);
   OK: Boolean;

begin
   Ada.Text_IO.Put_Line(  "Buckets:"    & Integer'Image(Number_Of_Buckets) &
                          ", Expected:" & Integer'Image(Expected_Per_Bucket) &
                          ", Margin:"   & Integer'Image(Margin_In_Percent));
   Rand.Reset(Gen);

   Perform(Modulus  => Number_Of_Buckets,
           Expected => Expected_Per_Bucket,
           Margin   => Margin_In_Percent,
           Passed   => OK);

   Ada.Text_IO.Put_Line("Test Passed? (" & Boolean'Image(OK) & ")");
end Naive_Random;
