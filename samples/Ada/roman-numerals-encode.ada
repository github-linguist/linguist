with Ada.Text_IO;  use Ada.Text_IO;

procedure Roman_Numeral_Test is
   function To_Roman (Number : Positive) return String is
      subtype Digit is Integer range 0..9;
      function Roman (Figure : Digit; I, V, X : Character) return String is
      begin
         case Figure is
            when 0 => return "";
            when 1 => return "" & I;
            when 2 => return I & I;
            when 3 => return I & I & I;
            when 4 => return I & V;
            when 5 => return "" & V;
            when 6 => return V & I;
            when 7 => return V & I & I;
            when 8 => return V & I & I & I;
            when 9 => return I & X;
         end case;
      end Roman;
   begin
      pragma Assert (Number >= 1 and Number < 4000);
      return
         Roman (Number / 1000,       'M', ' ', ' ') &
         Roman (Number / 100 mod 10, 'C', 'D', 'M') &
         Roman (Number / 10 mod 10,  'X', 'L', 'C') &
         Roman (Number mod 10,       'I', 'V', 'X');
   end To_Roman;
begin
   Put_Line (To_Roman (1999));
   Put_Line (To_Roman (25));
   Put_Line (To_Roman (944));
end Roman_Numeral_Test;
