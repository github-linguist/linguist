S : String := "12345";
S := Ada.Strings.Fixed.Trim(Source => Integer'Image(Integer'Value(S) + 1), Side => Ada.Strings.Both);
