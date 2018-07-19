with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Non_Continuous is
   type Sequence is array (Positive range <>) of Integer;
   procedure Put_NCS
             (  Tail : Sequence;                -- To generate subsequences of
                Head : Sequence := (1..0 => 1); -- Already generated
                Contiguous : Boolean := True    -- It is still continuous
             )  is
   begin
      if not Contiguous and then Head'Length > 1 then
         for I in Head'Range loop
            Put (Integer'Image (Head (I)));
         end loop;
         New_Line;
      end if;
      if Tail'Length /= 0 then
         declare
            New_Head : Sequence (Head'First..Head'Last + 1);
         begin
            New_Head (Head'Range) := Head;
            for I in Tail'Range loop
               New_Head (New_Head'Last) := Tail (I);
               Put_NCS
               (  Tail => Tail (I + 1..Tail'Last),
                  Head => New_Head,
                  Contiguous => Contiguous and then (I = Tail'First or else Head'Length = 0)
               );
            end loop;
         end;
      end if;
   end Put_NCS;
begin
   Put_NCS ((1,2,3));     New_Line;
   Put_NCS ((1,2,3,4));   New_Line;
   Put_NCS ((1,2,3,4,5)); New_Line;
end Test_Non_Continuous;
