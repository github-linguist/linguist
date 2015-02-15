with Ada.Text_Io; use Ada.Text_Io;

procedure Cocktail_Sort_Test is
   procedure Cocktail_Sort (Item : in out String) is
      procedure Swap(Left, Right : in out Character) is
         Temp : Character := Left;
      begin
         Left := Right;
         Right := Temp;
      end Swap;
      Swapped : Boolean := False;
   begin
      loop
         for I in 1..Item'Last - 1 loop
            if Item(I) > Item(I + 1) then
               Swap(Item(I), Item(I + 1));
               Swapped := True;
            end if;
         end loop;
         if not Swapped then
            for I in reverse 1..Item'Last - 1 loop
               if Item(I) > Item(I + 1) then
                  Swap(Item(I), Item(I + 1));
                  Swapped := True;
               end if;
            end loop;
         end if;
         exit when not Swapped;
         Swapped := False;
      end loop;
   end Cocktail_Sort;
   Data : String := "big fjords vex quick waltz nymph";
begin
   Put_Line(Data);
   Cocktail_Sort(Data);
   Put_Line(Data);
end Cocktail_Sort_Test;
