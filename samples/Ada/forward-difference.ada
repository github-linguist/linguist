with Ada.Text_Io;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.containers.Vectors;

procedure Forward_Difference is
   package Flt_Vect is new Ada.Containers.Vectors(Positive, Float);
   use Flt_Vect;
   procedure Print(Item : Vector) is
   begin
      if not Item.Is_Empty then
         Ada.Text_IO.Put('[');
         for I in 1..Item.Length loop
            Put(Item => Item.Element(Positive(I)), Fore => 1, Aft => 1, Exp => 0);
             if Positive(I) < Positive(Item.Length) then
               Ada.Text_Io.Put(", ");
            end if;
         end loop;
         Ada.Text_Io.Put_line("]");
      else
         Ada.Text_IO.Put_Line("Empty List");
      end if;

   end Print;

  function Diff(Item : Vector; Num_Passes : Natural) return Vector is
      A : Vector := Item;
      B : Vector := Empty_Vector;
   begin
      if not A.Is_Empty then
         for I in 1..Num_Passes loop
            for I in 1..Natural(A.Length) - 1 loop
                  B.Append(A.Element(I + 1) - A.Element(I));
            end loop;
            Move(Target => A, Source => B);
         end loop;
      end if;
      return A;
   end Diff;
   Values : array(1..10) of Float := (90.0, 47.0, 58.0, 29.0, 22.0, 32.0, 55.0, 5.0, 55.0, 73.0);
   A : Vector;
begin
   for I in Values'range loop
      A.Append(Values(I)); -- Fill the vector
   end loop;
   Print(Diff(A, 1));
   Print(Diff(A, 2));
   Print(Diff(A, 9));
   Print(Diff(A, 10));
   print(Diff(A, 0));
end Forward_Difference;
