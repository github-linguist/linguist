with Ada.Text_IO;

procedure Box_The_Compass is

   type Degrees is digits 5 range 0.00 .. 359.99;
   type Index_Type is mod 32;

   function Long_Name(Short: String) return String is

      function Char_To_Name(Char: Character) return String is
      begin
         case Char is
            when 'N' | 'n' => return Char & "orth";
            when 'S' | 's' => return Char & "outh";
            when 'E' | 'e' => return Char & "ast";
            when 'W' | 'w' => return Char & "est";
            when 'b' => return " by ";
            when '-' => return "-";
            when others => raise Constraint_Error;
         end case;
      end Char_To_Name;

   begin
      if Short'Length = 0 or else Short(Short'First)=' ' then
         return "";
      else
         return Char_To_Name(Short(Short'First))
           & Long_Name(Short(Short'First+1 .. Short'Last));
      end if;
   end Long_Name;

   procedure Put_Line(Angle: Degrees) is

      function Index(D: Degrees) return Index_Type is
      begin
         return Index_Type(Integer(Degrees'Rounding(D/11.25)) mod 32);
      end Index;

      I: Integer := Integer(Index(Angle))+1;
      package DIO is new Ada.Text_IO.Float_IO(Degrees);
      Abbr: constant array(Index_Type) of String(1 .. 4)
        := ("N   ", "Nbe ", "N-ne", "Nebn", "Ne  ", "Nebe", "E-ne", "Ebn ",
            "E   ", "Ebs ", "E-se", "Sebe", "Se  ", "Sebs", "S-se", "Sbe ",
            "S   ", "Sbw ", "S-sw", "Swbs", "Sw  ", "Swbw", "W-sw", "Wbs ",
            "W   ", "Wbn ", "W-nw", "Nwbw", "Nw  ", "Nwbn", "N-nw", "Nbw ");

   begin
      DIO.Put(Angle, Fore => 3, Aft => 2, Exp => 0); -- format "zzx.xx"
      Ada.Text_IO.Put(" |");
      if I <= 9 then
         Ada.Text_IO.Put(" ");
      end if;
      Ada.Text_IO.Put_Line(" "  & Integer'Image(I) & " | "
                             & Long_Name(Abbr(Index(Angle))));
   end Put_Line;

   Difference: constant array(0..2) of Degrees'Base
     := (0=> 0.0, 1=> +5.62, 2=> - 5.62);

begin
   Ada.Text_IO.Put_Line(" angle | box | compass point");
   Ada.Text_IO.Put_Line(" ---------------------------------");
   for I in 0 .. 32 loop
      Put_Line(Degrees(Degrees'Base(I) * 11.25 + Difference(I mod 3)));
   end loop;
end Box_The_Compass;
