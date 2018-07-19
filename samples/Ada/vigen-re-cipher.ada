with Ada.Text_IO;

procedure Vignere_Cipher is

   subtype Letter is Character range 'A' .. 'Z';
   subtype Lowercase is Character range 'a' .. 'z';

   function "+"(X, Y: Letter) return Letter is
   begin
      return Character'Val( ( (Character'Pos(X)-Character'Pos('A'))
                                + (Character'Pos(Y)-Character'Pos('A')) ) mod 26
                          + Character'Pos('A'));
   end;

   function Normalize(S: String) return String is
      -- removes all characters except for uppercase and lowercase letters
      -- replaces lowercase by uppercase letters
      Offset: Integer := Character'Pos('A') - Character'Pos('a');
   begin
      if S="" then
         return "";
      elsif S(S'First) in Letter then
         return S(S'First) & Normalize(S(S'First+1 .. S'Last));
      elsif  S(S'First) in Lowercase then
         return (Character'Val(Character'Pos(S(S'First)) + Offset)
                   & Normalize(S(S'First+1 .. S'Last)));
      else
         return Normalize(S(S'First+1 .. S'Last));
      end if;
   end Normalize;

   function Encrypt(Key: String; Text: String) return String is
      Ciphertext: String(Text'Range);
   begin
      for I in Text'Range loop
         Ciphertext(I) := Text(I)
           + Key(Key'First + ((I-Text'First) mod Key'Length));
      end loop;
      return Ciphertext;
   end Encrypt;

   function Invert(Key: String) return String is
      Result: String(Key'Range);
   begin
      for I in Key'Range loop
         Result(I)
           := Character'Val( 26 - (Character'Pos(Key(I))-Character'Pos('A'))
                               + Character'Pos('A') );
      end loop;
      return Result;
   end Invert;

   use Ada.Text_IO;
   Input: String := Get_Line;
   Key:   String := Normalize(Get_Line);
   Ciph:  String := Encrypt(Key => Key, Text => Normalize(Input));

begin
   Put_Line("Input      =" & Input);
   Put_Line("Key        =" & Key);
   Put_Line("Ciphertext =" & Ciph);
   Put_Line("Decryption =" & Encrypt(Key => Invert(Key), Text => Ciph));
end Vignere_Cipher;
