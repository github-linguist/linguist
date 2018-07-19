with Ada.Text_IO; use Ada.Text_IO;
procedure Dinesman is
   subtype Floor is Positive range 1 .. 5;
   type People is (Baker, Cooper, Fletcher, Miller, Smith);
   type Floors is array (People'Range) of Floor;
   type PtFloors is access all Floors;

   function Constrained (f : PtFloors) return Boolean is begin
      if f (Baker) /= Floor'Last and
         f (Cooper) /= Floor'First and
         Floor'First < f (Fletcher) and f (Fletcher) < Floor'Last and
         f (Miller) > f (Cooper) and
         abs (f (Smith) - f (Fletcher)) /= 1 and
         abs (f (Fletcher) - f (Cooper)) /= 1
      then return True; end if;
      return False;
   end Constrained;

   procedure Solve (list : PtFloors; n : Natural) is
      procedure Swap (I : People; J : Natural) is
         temp : constant Floor := list (People'Val (J));
      begin list (People'Val (J)) := list (I); list (I) := temp;
      end Swap;
   begin
      if n = 1 then
         if Constrained (list) then
            for p in People'Range loop
               Put_Line (p'Img & " on floor " & list (p)'Img);
            end loop;
         end if;
         return;
      end if;
      for i in People'First .. People'Val (n - 1) loop
         Solve (list, n - 1);
         if n mod 2 = 1 then Swap (People'First, n - 1);
         else Swap (i, n - 1); end if;
      end loop;
   end Solve;

   thefloors : aliased Floors;
begin
   for person in People'Range loop
      thefloors (person) := People'Pos (person) + Floor'First;
   end loop;
   Solve (thefloors'Access, Floors'Length);
end Dinesman;
