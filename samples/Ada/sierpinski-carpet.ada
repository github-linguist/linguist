with Ada.Text_Io; use Ada.Text_Io;

procedure Sierpinski_Carpet is
   subtype Index_Type is Integer range 1..81;
   type Pattern_Array is array(Index_Type range <>, Index_Type range <>) of Boolean;
   Pattern : Pattern_Array(1..81,1..81) := (Others =>(others => true));
   procedure Clear_Center(P : in out Pattern_Array; X1 : Index_Type; X2 : Index_Type;
         Y1 : Index_Type; Y2 : Index_Type) is
      Xfirst : Index_Type;
      Xlast  : Index_Type;
      Yfirst : Index_Type;
      Ylast  : Index_Type;
      Diff   : Integer;
   begin
      Xfirst :=(X2 - X1 + 1) / 3 + X1;
      Diff := Xfirst - X1;
      Xlast  := Xfirst + Diff;
      Yfirst := (Y2 - Y1) / 3 + Y1;
      YLast  := YFirst + Diff;

      for I in XFirst..XLast loop
         for J in YFirst..YLast loop
            P(I, J) := False;
         end loop;
      end loop;
   end Clear_Center;

   procedure Print(P : Pattern_Array) is
   begin
      for I in P'range(1) loop
         for J in P'range(2) loop
            if P(I,J) then
               Put('*');
            else
               Put(' ');
            end if;
         end loop;
         New_Line;
      end loop;
   end Print;

   procedure Divide_Square(P : in out Pattern_Array; Order : Positive) is
      Factor : Natural := 0;
      X1, X2 : Index_Type;
      Y1, Y2  : Index_Type;
      Division : Index_Type;
      Num_Sections : Index_Type;
   begin
      while Factor < Order loop
         Num_Sections := 3**Factor;
         Factor := Factor + 1;
         X1  := P'First;
         Division   := P'Last / Num_Sections;
         X2 := Division;
         Y1 := X1;
         Y2 := X2;
         loop
            loop
               Clear_Center(P, X1, X2, Y1, Y2);
               exit when X2 = P'Last;
               X1 := X2;
               X2 := X2 + Division;
            end loop;
            exit when Y2 = P'Last;
            Y1 := Y2;
            Y2 := Y2 + Division;
            X1 := P'First;
            X2 := Division;
         end loop;
      end loop;
   end Divide_Square;

begin
   Divide_Square(Pattern, 3);
   Print(Pattern);
end Sierpinski_Carpet;
