with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Zig_Zag is

   type Matrix is array (Positive range <>, Positive range <>) of Natural;
   function Zig_Zag (Size : Positive) return Matrix is
      Data : Matrix (1..Size, 1..Size);
      I, J : Integer := 1;
   begin
      Data (1, 1) := 0;
      for Element in 1..Size**2 - 1 loop
         if (I + J) mod 2 = 0 then
            -- Even stripes
            if J < Size then
               J := J + 1;
            else
               I := I + 2;
            end if;
            if I > 1 then
               I := I - 1;
            end if;
         else
            -- Odd stripes
            if I < Size then
               I := I + 1;
            else
               J := J + 2;
            end if;
            if J > 1 then
               J := J - 1;
            end if;
         end if;
         Data (I, J) := Element;
      end loop;
      return Data;
   end Zig_Zag;

   procedure Put (Data : Matrix) is
   begin
      for I in Data'Range (1) loop
         for J in Data'Range (2) loop
            Put (Integer'Image (Data (I, J)));
         end loop;
         New_Line;
      end loop;
   end Put;

begin
   Put (Zig_Zag (5));
end Test_Zig_Zag;
