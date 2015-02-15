with Ada.Text_IO;
procedure CLS is
begin
   Ada.Text_IO.Put(ASCII.ESC & "[2J");
end CLS;
