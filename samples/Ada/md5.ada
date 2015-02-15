with Ada.Text_IO; use Ada.Text_IO;
with GNAT.MD5;

procedure MD5_Digest is
begin
   Put(GNAT.MD5.Digest("Foo bar baz"));
end MD5_Digest;
