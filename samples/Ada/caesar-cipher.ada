with Ada.Text_IO;

procedure Caesar is

   type M26 is mod 26;

   function To_M26(C: Character; Offset: Character) return M26 is
   begin
      return M26(Character'Pos(C)-Character'Pos(Offset));
   end To_M26;

   function To_Character(Value:   in  M26; Offset: Character)
                        return Character is
   begin
      return Character'Val(Integer(Value)+Character'Pos(Offset));
   end To_Character;

   function Encrypt (Plain: String; Key: M26) return String is
      Ciph: String(Plain'Range);

   begin
      for I in Plain'Range loop
         case Plain(I) is
            when 'A' .. 'Z' =>
               Ciph(I) := To_Character(To_M26(Plain(I), 'A')+Key, 'A');
            when 'a' .. 'z' =>
               Ciph(I) := To_Character(To_M26(Plain(I), 'a')+Key, 'a');
            when others =>
               Ciph(I) := Plain(I);
         end case;
      end loop;
      return Ciph;
   end Encrypt;

   Text:  String := Ada.Text_IO.Get_Line;
   Key: M26 := 3; -- Default key from "Commentarii de Bello Gallico"

begin -- Caesar main program

   Ada.Text_IO.Put_Line("Plaintext ------------>" & Text);
   Text := Encrypt(Text, Key);
   Ada.Text_IO.Put_Line("Ciphertext ----------->" & Text);
   Ada.Text_IO.Put_Line("Decrypted Ciphertext ->" & Encrypt(Text, -Key));

end Caesar;
