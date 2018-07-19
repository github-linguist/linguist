with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Kaprekar2 is
   use Ada.Strings.Fixed;

   To_Digit : constant String := "0123456789abcdefghijklmnopqrstuvwxyz";

   type Int is mod 2 ** 64;
   subtype Base_Number is Int range 2 .. 36;

   From_Digit : constant array (Character) of Int :=
     ('0'    => 0,
      '1'    => 1,
      '2'    => 2,
      '3'    => 3,
      '4'    => 4,
      '5'    => 5,
      '6'    => 6,
      '7'    => 7,
      '8'    => 8,
      '9'    => 9,
      'a'    => 10,
      'b'    => 11,
      'c'    => 12,
      'd'    => 13,
      'e'    => 14,
      'f'    => 15,
      'g'    => 16,
      'h'    => 17,
      'i'    => 18,
      'j'    => 19,
      'k'    => 20,
      'l'    => 21,
      'm'    => 22,
      'n'    => 23,
      'o'    => 24,
      'p'    => 25,
      'q'    => 26,
      'r'    => 27,
      's'    => 28,
      't'    => 29,
      'u'    => 30,
      'v'    => 31,
      'w'    => 32,
      'x'    => 33,
      'y'    => 34,
      'z'    => 35,
      others => 0);

   function To_String (Item : Int; Base : Base_Number := 10) return String is
      Value       : Int := Item;
      Digit_Index : Natural;
      Result      : String (1 .. 64);
      First       : Natural := Result'Last;
   begin
      while Value > 0 loop
         Digit_Index := Natural (Value mod Base);
         Result (First) := To_Digit (Digit_Index + 1);
         Value := Value / Base;
         First := First - 1;
      end loop;
      return Result (First + 1 .. Result'Last);
   end To_String;

   procedure Get (From : String; Item : out Int; Base : Base_Number := 10) is
   begin
      Item := 0;
      for I in From'Range loop
         Item := Item * Base;
         Item := Item + From_Digit (From (I));
      end loop;
   end Get;

   function Is_Kaprekar (N : Int; Base : Base_Number := 10) return Boolean is
      Square : Int;
   begin
      if N = 1 then
         return True;
      else
         Square := N ** 2;
         declare
            Image : String := To_String (Square, Base);
            A, B  : Int;
         begin
            for I in Image'First .. Image'Last - 1 loop
               exit when Count (Image (I + 1 .. Image'Last), "0")
                 = Image'Last - I;
               Get (From => Image (Image'First .. I),
                    Item => A,
                    Base => Base);
               Get (From => Image (I + 1 .. Image'Last),
                    Item => B,
                    Base => Base);
               if A + B = N then
                  return True;
               end if;
            end loop;
         end;
      end if;
      return False;
   end Is_Kaprekar;

   Count : Natural := 0;
begin
   for I in Int range 1 .. 10_000 loop
      if Is_Kaprekar (I) then
         Count := Count + 1;
         Ada.Text_IO.Put (To_String (I) & ",");
      end if;
   end loop;
   Ada.Text_IO.Put_Line (" Total:" & Integer'Image (Count));

   for I in Int range 10_001 .. 1_000_000 loop
      if Is_Kaprekar (I) then
         Count := Count + 1;
      end if;
   end loop;
   Ada.Text_IO.Put_Line ("Kaprekar Numbers below 1000000:" &
                         Integer'Image (Count));

   Count := 0;
   Ada.Text_IO.Put_Line ("Kaprekar Numbers below 1000000 in base 17:");
   for I in Int range 1 .. 17 ** 6 loop
      if Is_Kaprekar (I, 17) then
         Count := Count + 1;
         Ada.Text_IO.Put (To_String (I, 17) & ",");
      end if;
   end loop;
   Ada.Text_IO.Put_Line (" Total:" & Integer'Image (Count));
end Kaprekar2;
