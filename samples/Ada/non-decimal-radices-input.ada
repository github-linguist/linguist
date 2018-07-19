with Ada.Text_IO;
procedure Numbers is
   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   package Float_IO is new Ada.Text_IO.Float_IO (Float);
begin
   Int_IO.Put (Integer'Value ("16#ABCF123#"));
   Ada.Text_IO.New_Line;
   Int_IO.Put (Integer'Value ("8#7651#"));
   Ada.Text_IO.New_Line;
   Int_IO.Put (Integer'Value ("2#1010011010#"));
   Ada.Text_IO.New_Line;
   Float_IO.Put (Float'Value ("16#F.FF#E+2"));
   Ada.Text_IO.New_Line;
end Numbers;
